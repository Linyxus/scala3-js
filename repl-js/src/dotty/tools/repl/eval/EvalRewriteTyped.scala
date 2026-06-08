package dotty.tools
package repl
package eval

import scala.collection.mutable

import dotc.ast.tpd
import dotc.ast.tpd.*
import dotc.core.Constants.Constant
import dotc.core.Contexts.*
import dotc.core.Decorators.*
import dotc.core.Flags
import dotc.core.NameKinds.DefaultGetterName
import dotc.core.Names.{Name, TermName, termName}
import dotc.core.Phases.Phase
import dotc.core.StdNames.nme
import dotc.core.Symbols.*
import dotc.core.Types.*
import dotc.cc.CheckCaptures
import dotc.report
import dotc.transform.MacroTransform
import dotc.util.SourceFile
import dotc.util.Spans.{NoSpan, Span}

import scala.runtime.eval.EvalContext

/** Post-PostTyper phase that fills the `bindings`, `expectedType`, and
 *  `enclosingSource` arguments of every `eval[T]` / `evalSafe[T]` / `evalLoop[R]`
 *  / `agent[T]` / `agentSafe[T]` call. This is *the* eval rewriter. (`evalLoop`
 *  has no body string of its own; the slots back its `EvalSession`'s
 *  `EvalContext` so the loop body can read the call site's surrounding source.)
 *
 *  Runs after PostTyper so the typed tree carries resolved symbols (eval is
 *  matched by `sym.owner == Eval.moduleClass`, never by name), captured locals
 *  survive to runtime, and class-member refs are shaped as `This(cls).select`.
 *
 *  This is the Scala.js foundation port: the var facade uses plain Scala
 *  `Function0`/`Function1` (there is no classloader boundary here), and the
 *  capture-checking integration (DiscardUses stamping, cc-aware type rendering)
 *  is deferred — `discardUses` is the identity for now.
 */
class EvalRewriteTyped(maybeConfig: Option[EvalCompilerConfig] = None) extends MacroTransform:

  override def phaseName: String = EvalRewriteTyped.name

  override def runsAfter: Set[String] =
    Set(dotc.transform.PostTyper.name)

  override protected def newTransformer(using Context): Transformer =
    new EvalRewriteTransformer

  private case class CapturedSym(
      sym: Symbol,
      sourceName: String,
      isVar: Boolean,
      isGiven: Boolean = false,
      isByName: Boolean = false,
      isDef: Boolean = false,
      selfThisCls: Option[ClassSymbol] = None,
      classMemberOf: Option[ClassSymbol] = None
  )

  private enum TopKind:
    case Unknown, Definition, Expression

  private enum EvalKind:
    case NotEval, PlainEval, PlainEvalSafe, EvalLike, EvalSafeLike, EvalLoop

    def isPlain: Boolean = this match
      case PlainEval | PlainEvalSafe => true
      case _ => false

    def isSafe: Boolean = this match
      case PlainEvalSafe | EvalSafeLike => true
      case _ => false

  private class EvalRewriteTransformer extends Transformer:

    private val frameStack = mutable.Stack.empty[List[CapturedSym]]

    private var topLevelStart: Int = -1
    private var topLevelEnd: Int = -1
    private var topLevelSource: SourceFile | Null = null
    private var topLevelKind: TopKind = TopKind.Unknown

    private def classifyTopLevel(tree: Tree): TopKind = tree match
      case _: DefDef | _: ValDef | _: TypeDef | _: Import | _: PackageDef =>
        TopKind.Definition
      case _ => TopKind.Expression

    private def withTopLevel[T](stat: Tree)(action: => T)(using Context): T =
      val savedStart = topLevelStart
      val savedEnd = topLevelEnd
      val savedSrc = topLevelSource
      val savedKind = topLevelKind
      val span = stat.span
      if span.exists then
        topLevelStart = span.start
        topLevelEnd = span.end
        topLevelSource = stat.source
        topLevelKind = classifyTopLevel(stat)
      try action
      finally
        topLevelStart = savedStart
        topLevelEnd = savedEnd
        topLevelSource = savedSrc
        topLevelKind = savedKind

    private def withScope[T](caps: List[CapturedSym])(action: => T): T =
      val pushed = caps.nonEmpty
      if pushed then frameStack.push(caps)
      try action
      finally if pushed then frameStack.pop()

    private def currentBindings: List[CapturedSym] =
      val seen = mutable.LinkedHashMap.empty[String, CapturedSym]
      for frame <- frameStack.toList.reverse; c <- frame do
        seen(c.sourceName) = c
      seen.values.toList

    override def transform(tree: Tree)(using Context): Tree =
      tree match
        case Block(stats, expr) =>
          val forwardDefs: List[CapturedSym] = stats.collect {
            case dd: DefDef
                if !dd.symbol.is(Flags.Synthetic)
                && !dd.name.isEmpty =>
              CapturedSym(dd.symbol, dd.name.toString, isVar = false, isDef = true)
          }
          val processed = mutable.ListBuffer.empty[Tree]
          var blockCaps: List[CapturedSym] = forwardDefs
          for stat <- stats do
            val newStat = withScope(blockCaps)(transform(stat))
            processed += newStat
            stat match
              case vd: ValDef
                  if !vd.symbol.is(Flags.Synthetic)
                  && !vd.name.isEmpty =>
                val isVar = vd.symbol.is(Flags.Mutable)
                val isGiven = vd.symbol.is(Flags.Given)
                blockCaps = blockCaps ++ List(
                  CapturedSym(vd.symbol, vd.name.toString, isVar, isGiven = isGiven)
                )
              case _ =>
          val newExpr = withScope(blockCaps)(transform(expr))
          cpy.Block(tree)(processed.toList, newExpr)

        case dd: DefDef =>
          val paramCaps: List[CapturedSym] = dd.paramss.flatMap { clause =>
            clause.collect {
              case vd: ValDef if !vd.name.isEmpty =>
                val byName = vd.symbol.info.isInstanceOf[ExprType]
                val isGiven = vd.symbol.is(Flags.Given)
                CapturedSym(vd.symbol, vd.name.toString, isVar = false, isGiven = isGiven, isByName = byName)
            }
          }
          val newRhs =
            inContext(ctx.withOwner(dd.symbol)) {
              withScope(paramCaps)(transform(dd.rhs))
            }
          cpy.DefDef(tree)(dd.name, dd.paramss, dd.tpt, newRhs)

        case impl: Template
            if ctx.owner.isClass
            && ctx.owner.is(Flags.Module)
            && ctx.owner.maybeOwner.is(Flags.Package) =>
          // The REPL session wrapper (`object rs$line$N`). Each member of its
          // Template body is a "top-level statement" w.r.t. the user's REPL line.
          val newStats = impl.body.mapConserve { stat =>
            withTopLevel(stat)(transform(stat))
          }
          cpy.Template(impl)(
            transformSub(impl.constr),
            transform(impl.parents)(using ctx.superCallContext),
            Nil,
            transformSelf(impl.self),
            newStats
          )

        case impl: Template
            if ctx.owner.isClass
            && !ctx.owner.is(Flags.Module)
            && !ctx.owner.is(Flags.Package) =>
          val classSym = ctx.owner.asClass
          val syntheticThises = List(
            CapturedSym(NoSymbol, "__this__", isVar = false, selfThisCls = Some(classSym)),
            CapturedSym(NoSymbol, s"__this__${classSym.name}", isVar = false, selfThisCls = Some(classSym))
          )
          val members = collectClassMembers(impl, classSym)
          withScope(syntheticThises ++ members)(super.transform(impl))

        case app: Apply =>
          val withChildren = super.transform(app).asInstanceOf[Apply]
          val kind = classifyCall(withChildren)
          if kind == EvalKind.NotEval then
            warnIfShadowingEvalName(withChildren)
            withChildren
          else if maybeConfig.nonEmpty && kind == EvalKind.EvalLoop then
            withChildren
          else if withChildren.args.length == 1 && kind.isPlain then
            expandOneArgToFourArg(withChildren, kind).getOrElse(withChildren)
          else
            fillEvalArgs(withChildren, kind)

        case _ =>
          super.transform(tree)

    private def collectClassMembers(impl: Template, classSym: ClassSymbol)(using Context): List[CapturedSym] =
      val out = mutable.ListBuffer.empty[CapturedSym]
      def addMember(vd: ValDef): Unit =
        if vd.symbol.exists && !vd.name.isEmpty && !vd.symbol.is(Flags.Synthetic) then
          out += CapturedSym(
            vd.symbol, vd.name.toString, isVar = false,
            classMemberOf = Some(classSym)
          )
      impl.constr.paramss.foreach { clause =>
        clause.foreach {
          case vd: ValDef
              if vd.symbol.is(Flags.ParamAccessor) || vd.symbol.is(Flags.Mutable) =>
            addMember(vd)
          case _ =>
        }
      }
      impl.body.foreach {
        case vd: ValDef => addMember(vd)
        case _ =>
      }
      out.toList

    private def classifyCall(app: Apply)(using Context): EvalKind =
      val sym = app.fun.symbol
      if !sym.exists then EvalKind.NotEval
      else
        val owner = sym.maybeOwner
        if owner.exists && owner == EvalRewriteTyped.evalModuleClass then
          sym.name.toString match
            case "eval" => EvalKind.PlainEval
            case "evalSafe" => EvalKind.PlainEvalSafe
            case "evalLoop" => EvalKind.EvalLoop
            case _ => EvalKind.NotEval
        else if sym.hasAnnotation(EvalRewriteTyped.evalLikeAnnotClass) then
          EvalKind.EvalLike
        else if sym.hasAnnotation(EvalRewriteTyped.evalSafeLikeAnnotClass) then
          EvalKind.EvalSafeLike
        else EvalKind.NotEval

    private def expandOneArgToFourArg(app: Apply, kind: EvalKind)(using Context): Option[Apply] =
      val Apply(fun, List(closureArg)) = app: @unchecked
      val oneArgSym = fun.symbol
      val typeArgs = fun match
        case TypeApply(_, ts) => ts
        case _ => Nil
      paramInfosOf(oneArgSym).headOption.flatMap { firstParamTpe =>
        findFourArgOverload(oneArgSym, firstParamTpe).map { fourArgSym =>
          val newFun =
            if typeArgs.nonEmpty then
              TypeApply(ref(fourArgSym), typeArgs).withSpan(fun.span)
            else
              ref(fourArgSym).withSpan(fun.span)
          val span = app.span
          val tArg = extractTypeArg(fun)
          val rendered = if tArg eq null then "" else EvalRewriteTyped.renderType(tArg)
          val encl = computeEnclosingSource(span)
          val wrappedEncl =
            if kind.isSafe && encl.contains(EvalContext.placeholder) then
              encl.replace(
                EvalContext.placeholder,
                s"_root_.scala.runtime.eval.Eval.handleCompileError(${EvalContext.placeholder})"
              )
            else encl
          val bindingsArg = buildBindingsArray(currentBindings, span)
          val expTpeArg = Literal(Constant(rendered)).withSpan(span)
          val enclArg = Literal(Constant(wrappedEncl)).withSpan(span)
          Apply(newFun, List(closureArg, bindingsArg, expTpeArg, enclArg)).withSpan(span)
        }
      }

    private def paramInfosOf(sym: Symbol)(using Context): List[Type] =
      sym.info match
        case pt: PolyType =>
          pt.resType match
            case mt: MethodType => mt.paramInfos
            case _ => Nil
        case mt: MethodType => mt.paramInfos
        case _ => Nil

    private def findFourArgOverload(sym: Symbol, firstParamTpe: Type)(using Context): Option[Symbol] =
      val owner = sym.maybeOwner
      if !owner.exists then None
      else
        owner.info.member(sym.name).alternatives.iterator.map(_.symbol).find { alt =>
          paramInfosOf(alt) match
            case head :: rest =>
              rest.length == 3 && (head =:= firstParamTpe)
            case _ => false
        }

    private def fillEvalArgs(app: Apply, kind: EvalKind)(using Context): Tree =
      val sym = app.fun.symbol
      val span = app.span

      if kind.isSafe then
        val resultTpe = sym.info.finalResultType
        if !isEvalResultType(resultTpe) then
          report.error(
            i"""${kind} call's method must return an `EvalResult[?]` — got `$resultTpe`.
               |For safe-flavor eval generators (annotated with `@evalSafeLike`), declare
               |the result type as `EvalResult[T]` so the rewriter can wrap the encl
               |source's marker in `Eval.handleCompileError(...)` correctly.""",
            app.srcPos
          )
          return app

      val clauseInfo = locateSyntheticClause(app, sym)
      if clauseInfo.isEmpty then return app
      val (clauseApp, bindIdx, expIdx, enclIdx) = clauseInfo.get
      val args = clauseApp.args

      def isFillable(t: Tree): Boolean = isDefaultArgFill(t)

      val bindFillable = isFillable(args(bindIdx))
      val expFillable = isFillable(args(expIdx))
      val enclFillable = isFillable(args(enclIdx))
      val allFillable = bindFillable && expFillable && enclFillable
      val noneFillable = !bindFillable && !expFillable && !enclFillable

      if !allFillable && !noneFillable then
        report.error(
          i"""eval-like call has a partial set of synthetic arguments — the rewriter
             |requires that `bindings`, `expectedType`, and `enclosingSource` are
             |either *all* default (filled in by the rewriter) or *all* explicitly
             |supplied (forwarded by a wrapping `@evalLike` function). Mixed states
             |would have silently overwritten one of your values.
             |
             |Slot states: bindings=${stateLabel(bindFillable)}, expectedType=${stateLabel(expFillable)}, enclosingSource=${stateLabel(enclFillable)}""",
          app.srcPos
        )
        return app

      if noneFillable then
        return app

      val argsBuf = args.toBuffer

      val tArg = extractTypeArg(app.fun)
      val renderedTpe = if tArg eq null then "" else EvalRewriteTyped.renderType(tArg)
      argsBuf(expIdx) = Literal(Constant(renderedTpe)).withSpan(argsBuf(expIdx).span)

      val encl = computeEnclosingSource(span)
      val wrappedEncl =
        if kind.isSafe && encl.contains(EvalContext.placeholder) then
          encl.replace(
            EvalContext.placeholder,
            s"_root_.scala.runtime.eval.Eval.handleCompileError(${EvalContext.placeholder})"
          )
        else encl
      argsBuf(enclIdx) = Literal(Constant(wrappedEncl)).withSpan(argsBuf(enclIdx).span)

      argsBuf(bindIdx) = buildBindingsArray(currentBindings, argsBuf(bindIdx).span)

      val newClauseApp = cpy.Apply(clauseApp)(clauseApp.fun, argsBuf.toList)
      if (newClauseApp eq clauseApp) || (clauseApp eq app) then
        if clauseApp eq app then newClauseApp else app
      else rethreadApply(app, clauseApp, newClauseApp)

    private def locateSyntheticClause(app: Apply, sym: Symbol)(using Context): Option[(Apply, Int, Int, Int)] =
      val clauses = paramClauseNames(sym.info)
      val totalTermClauses = clauses.length
      val clauseIdx = clauses.indexWhere(names =>
        names.contains(EvalRewriteTyped.BindingsParamName) &&
          names.contains(EvalRewriteTyped.ExpectedTypeParamName) &&
          names.contains(EvalRewriteTyped.EnclosingSourceParamName)
      )
      if clauseIdx < 0 then return None
      val descend = totalTermClauses - 1 - clauseIdx
      var cur: Tree = app
      var stepsLeft = descend
      while stepsLeft > 0 do
        cur match
          case a: Apply => cur = a.fun; stepsLeft -= 1
          case _ => return None
      cur match
        case a: Apply =>
          val names = clauses(clauseIdx)
          Some((
            a,
            names.indexOf(EvalRewriteTyped.BindingsParamName),
            names.indexOf(EvalRewriteTyped.ExpectedTypeParamName),
            names.indexOf(EvalRewriteTyped.EnclosingSourceParamName)
          ))
        case _ => None

    private def paramClauseNames(info: Type)(using Context): List[List[String]] = info match
      case pt: PolyType => paramClauseNames(pt.resType)
      case mt: MethodType => mt.paramNames.map(_.toString) :: paramClauseNames(mt.resType)
      case _ => Nil

    private def rethreadApply(app: Apply, original: Apply, replacement: Apply)(using Context): Tree =
      def loop(t: Tree): Tree =
        if t eq original then replacement
        else t match
          case a: Apply => cpy.Apply(a)(loop(a.fun), a.args)
          case _ => t
      loop(app)

    private def stateLabel(fillable: Boolean): String =
      if fillable then "default" else "supplied"

    private def isEvalResultType(tpe: Type)(using Context): Boolean =
      val cls = tpe.classSymbol
      cls.exists && cls.derivesFrom(EvalRewriteTyped.evalResultClass)

    private def isDefaultArgFill(t: Tree)(using Context): Boolean =
      def loop(t: Tree): Boolean = t match
        case Apply(fn, _) => loop(fn)
        case TypeApply(fn, _) => loop(fn)
        case _ =>
          val s = t.symbol
          s.exists && s.name.is(DefaultGetterName)
      loop(t)

    private def warnIfShadowingEvalName(app: Apply)(using Context): Unit =
      val sym = app.fun.symbol
      if sym == NoSymbol then return
      val name = sym.name.toString
      if name == "eval" || name == "evalSafe" then
        report.warning(
          i"`$name` here resolves to ${sym.owner}.${sym.name}, not `scala.runtime.eval.Eval.$name`; the eval rewriter is leaving this call alone. If you intended a custom eval generator, annotate the function with `@evalLike` (or `@evalSafeLike`).",
          app.srcPos
        )

    private def extractTypeArg(fun: Tree)(using Context): Type | Null = fun match
      case TypeApply(_, tArg :: _) => tArg.tpe
      case Apply(inner, _) => extractTypeArg(inner)
      case _ => null

    private def computeEnclosingSource(evalSpan: Span)(using Context): String =
      maybeConfig match
        case Some(cfg) if cfg.outerEnclosingSource.nonEmpty =>
          return composeChainedEncl(evalSpan, cfg.body, cfg.outerEnclosingSource)
        case _ =>

      val markerText = EvalBodyPlaceholder.Marker
      val sourceFile = topLevelSource
      if topLevelStart < 0 || !evalSpan.exists || sourceFile == null then return ""
      val src = sourceFile.content
      if topLevelEnd > src.length || topLevelStart >= topLevelEnd then return ""
      val relStart = evalSpan.start - topLevelStart
      val relEnd = evalSpan.end - topLevelStart
      val topLen = topLevelEnd - topLevelStart
      if relStart < 0 || relEnd > topLen || relStart > relEnd then return ""
      val topSrc = String.valueOf(src, topLevelStart, topLen)
      val withMarker =
        topSrc.substring(0, relStart) + markerText + topSrc.substring(relEnd)
      topLevelKind match
        case TopKind.Definition => withMarker
        case TopKind.Expression => s"val __unused__ : Any = { $withMarker }"
        case TopKind.Unknown => ""

    private def composeChainedEncl(innerSpan: Span, outerBody: String, outerEncl: String): String =
      if !innerSpan.exists then return ""
      val s = innerSpan.start
      val e = innerSpan.end
      if s < 0 || e > outerBody.length || s > e then return ""
      val outerBodyWithInnerMarker =
        outerBody.substring(0, s) + EvalBodyPlaceholder.Marker + outerBody.substring(e)
      outerEncl.replace(
        EvalBodyPlaceholder.Marker,
        EvalBodyPlaceholder.emit(outerBodyWithInnerMarker)
      )

    private def buildBindingsArray(caps: List[CapturedSym], span: Span)(using Context): Tree =
      val elemTpe: Type = EvalRewriteTyped.bindingClass.typeRef
      val elems: List[Tree] = caps.map(c => buildBind(c, span))
      JavaSeqLiteral(elems, TypeTree(elemTpe)).withSpan(span)

    private def buildBind(c: CapturedSym, span: Span)(using Context): Tree =
      if c.isVar then buildBindVar(c, span)
      else if c.isGiven then buildBindGiven(c, span)
      else if c.isByName then buildBindByName(c, span)
      else if c.isDef then buildBindDef(c, span)
      else
        val nameLit = Literal(Constant(c.sourceName)).withSpan(span)
        discardUses:
          ref(EvalRewriteTyped.bindSym)
            .appliedTo(nameLit, readRef(c, span))
            .withSpan(span)

    /** Suppress capture-set propagation from the synthesised `Eval.bind(name, v)`
     *  calls into the enclosing scope. Each captured local's value carries its
     *  own capture set; without this, the use recorded by `Eval.bind` flows into
     *  every enclosing function literal — including ones whose expected capture
     *  set forbids it (e.g. `String ->{any.rd} Int`), producing spurious cc
     *  errors at the outer compile. The binding only travels to the inner
     *  verification compile, which re-checks the body in its own lexical scope
     *  where the capability is legal.
     *
     *  Done by stamping the `Eval.bind*` Apply with [[CheckCaptures.DiscardUses]],
     *  which `CheckCaptures` rechecks `withDiscardedUses`. We use the attachment
     *  rather than emitting `caps.unsafe.unsafeDiscardUses(...)` because the
     *  latter is `@rejectSafe` and would fail `SafeRefs.checkSafe` when the live
     *  REPL session runs in safe mode. No runtime call is introduced — the marker
     *  rides on the `Eval.bind*` Apply we already emit, and is inert when cc is
     *  off (the capture-check phase never runs). */
    private def discardUses(bindApp: Tree)(using Context): Tree =
      bindApp.withAttachment(CheckCaptures.DiscardUses, ())

    private def readRef(c: CapturedSym, span: Span)(using Context): Tree =
      c.selfThisCls match
        case Some(cls) => This(cls).withSpan(span)
        case None =>
          c.classMemberOf match
            case Some(cls) => This(cls).select(c.sym).withSpan(span)
            case None => ref(c.sym).withSpan(span)

    private def buildBindGiven(c: CapturedSym, span: Span)(using Context): Tree =
      val nameLit = Literal(Constant(c.sourceName)).withSpan(span)
      discardUses:
        ref(EvalRewriteTyped.bindGivenSym)
          .appliedTo(nameLit, readRef(c, span))
          .withSpan(span)

    private def buildBindByName(c: CapturedSym, span: Span)(using Context): Tree =
      val nameLit = Literal(Constant(c.sourceName)).withSpan(span)
      val resultTpe = c.sym.info match
        case ExprType(rt) => rt
        case other => other
      val methTpe = MethodType(Nil, resultTpe)
      val fn = Lambda(methTpe, _ => readRef(c, span)).withSpan(span)
      discardUses:
        ref(EvalRewriteTyped.bindSym)
          .appliedTo(nameLit, fn)
          .withSpan(span)

    private def buildBindDef(c: CapturedSym, span: Span)(using Context): Tree =
      val nameLit = Literal(Constant(c.sourceName)).withSpan(span)
      val defSym = c.sym

      val (typeArgs: List[Type], methodLikeTpe: Type) = defSym.info match
        case poly: PolyType =>
          val anys: List[Type] = poly.paramRefs.map(_ => defn.AnyType)
          (anys, poly.instantiate(anys))
        case other =>
          (Nil, other)

      def flatten(t: Type): (List[List[Type]], List[List[TermName]], Type) = t match
        case mt: MethodType =>
          val (rest, namesRest, result) = flatten(mt.resType)
          (mt.paramInfos :: rest, mt.paramNames :: namesRest, result)
        case other =>
          (Nil, Nil, other)
      val (clauseInfos, clauseNames, _) = flatten(methodLikeTpe)

      def applyTypeArgs(t: Tree): Tree =
        if typeArgs.isEmpty then t else t.appliedToTypes(typeArgs)

      val etaTree: Tree =
        if clauseInfos.isEmpty then
          val methTpe = MethodType(Nil, defn.AnyType)
          Lambda(methTpe, _ => applyTypeArgs(ref(defSym))).withSpan(span)
        else
          val flatNames = clauseNames.flatten
          val flatInfos = clauseInfos.flatten
          val lambdaParamTpes: List[Type] = flatInfos.map {
            case et: ExprType => et
            case _ => defn.AnyType
          }
          val methTpe = MethodType(flatNames)(_ => lambdaParamTpes, _ => defn.AnyType)
          Lambda(methTpe, params =>
            val callArgs = params.lazyZip(flatInfos).map { (p, t) =>
              t match
                case _: ExprType => p
                case _ => p.cast(t)
            }
            def regroup(xs: List[Tree], sizes: List[Int]): List[List[Tree]] = sizes match
              case Nil => Nil
              case n :: rest =>
                val (head, tail) = xs.splitAt(n)
                head :: regroup(tail, rest)
            val grouped = regroup(callArgs, clauseInfos.map(_.length))
            applyTypeArgs(ref(defSym)).appliedToArgss(grouped)
          ).withSpan(span)
      discardUses:
        ref(EvalRewriteTyped.bindSym)
          .appliedTo(nameLit, etaTree)
          .withSpan(span)

    private def buildBindVar(c: CapturedSym, span: Span)(using Context): Tree =
      val varSym = c.sym
      val elemTpe = varSym.info.widen
      // SAM-typed `Supplier[T]` / `Consumer[T]` (not `Function0`/`Function1`):
      // these nominal SAMs have an empty capture set, so a captured var's
      // read/write effect cannot flow through the facade. Under safe mode that
      // makes capturing a var into a *pure* function in the body a compile error.
      val supplierTpe = EvalRewriteTyped.supplierClass.typeRef.appliedTo(elemTpe)
      val consumerTpe = EvalRewriteTyped.consumerClass.typeRef.appliedTo(elemTpe)

      val getMethTpe = MethodType(Nil, elemTpe)
      val getMeth = newAnonFun(ctx.owner, getMethTpe, coord = span)
      val getter =
        Closure(
          getMeth,
          _ => ref(varSym).withSpan(span).changeOwner(ctx.owner, getMeth),
          targetType = supplierTpe
        ).withSpan(span)

      val setMethTpe = MethodType(List(termName("v")))(_ => List(elemTpe), _ => defn.UnitType)
      val setMeth = newAnonFun(ctx.owner, setMethTpe, coord = span)
      val setter =
        Closure(
          setMeth,
          paramss =>
            Assign(ref(varSym), ref(paramss.head.head.symbol))
              .withSpan(span)
              .changeOwner(ctx.owner, setMeth),
          targetType = consumerTpe
        ).withSpan(span)

      val varRef = ref(EvalRewriteTyped.varRefSym)
        .appliedToType(elemTpe)
        .appliedTo(getter, setter)
        .withSpan(span)

      val nameLit = Literal(Constant(c.sourceName)).withSpan(span)
      discardUses:
        ref(EvalRewriteTyped.bindVarSym)
          .appliedTo(nameLit, varRef)
          .withSpan(span)

  end EvalRewriteTransformer

end EvalRewriteTyped

object EvalRewriteTyped:

  val name: String = "evalRewriteTyped"

  private val BindingsParamName: String = "bindings"
  private val ExpectedTypeParamName: String = "expectedType"
  private val EnclosingSourceParamName: String = "enclosingSource"

  private def evalModuleClass(using Context): Symbol =
    requiredModule("scala.runtime.eval.Eval").moduleClass

  private def evalLikeAnnotClass(using Context): ClassSymbol =
    requiredClass("scala.runtime.eval.evalLike")

  private def evalSafeLikeAnnotClass(using Context): ClassSymbol =
    requiredClass("scala.runtime.eval.evalSafeLike")

  private def evalResultClass(using Context): ClassSymbol =
    requiredClass("scala.runtime.eval.EvalResult")

  private def bindingClass(using Context): ClassSymbol =
    requiredClass("scala.runtime.eval.Eval.Binding")

  private def bindSym(using Context): Symbol =
    requiredModule("scala.runtime.eval.Eval").requiredMethod("bind")

  private def bindVarSym(using Context): Symbol =
    requiredModule("scala.runtime.eval.Eval").requiredMethod("bindVar")

  private def bindGivenSym(using Context): Symbol =
    requiredModule("scala.runtime.eval.Eval").requiredMethod("bindGiven")

  private def varRefSym(using Context): Symbol =
    requiredModule("scala.runtime.eval.Eval").requiredMethod("varRef")

  private def supplierClass(using Context): ClassSymbol =
    requiredClass("java.util.function.Supplier")

  private def consumerClass(using Context): ClassSymbol =
    requiredClass("java.util.function.Consumer")

  /** Render `tpe` as a Scala source string for the wrapper's
   *  `val __evalResult: <tpe> = ...` annotation. Returns "" when degenerate
   *  (`Nothing`/`Null`/error) or it mentions a symbol the wrapper can't resolve
   *  (an enclosing method's type param, or a locally-scoped class). The cc-aware
   *  capture-annotation handling is deferred (task #10). */
  private[repl] def renderType(tpe: Type)(using Context): String =
    if tpe == null || !tpe.exists || tpe.isError then return ""
    val widened = tpe.widen
    if !widened.exists || widened.isError then return ""
    if isUselessType(widened) then return ""
    val resolved = dealiasLocalAliases(widened)
    if mentionsLocallyScopedSymbol(resolved) then return ""
    val printCtx = ctx.fresh.setSetting(ctx.settings.color, "never")
    try
      resolved.show(using printCtx).replace(".this.", "#")
    catch case _: Throwable => ""

  private def isUselessType(tpe: Type)(using Context): Boolean =
    val sym = tpe.typeSymbol
    sym.exists && (sym == defn.NothingClass || sym == defn.NullClass)

  private def mentionsLocallyScopedSymbol(tpe: Type)(using Context): Boolean =
    tpe.existsPart { part =>
      val sym = part.typeSymbol
      sym.exists && {
        val isTypeParam = sym.is(Flags.TypeParam)
        val isTermOwned = sym.maybeOwner.exists && sym.maybeOwner.isTerm
        isTypeParam || isTermOwned
      }
    }

  private def dealiasLocalAliases(tpe: Type)(using Context): Type =
    val mapper = new TypeMap:
      def apply(tp: Type): Type = tp match
        case ref: TypeRef =>
          val sym = ref.symbol
          if sym.exists && sym.isAliasType && sym.maybeOwner.exists && sym.maybeOwner.isTerm then
            this(ref.dealias)
          else mapOver(tp)
        case _ => mapOver(tp)
    mapper(tpe)
