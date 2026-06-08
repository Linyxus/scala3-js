package dotty.tools
package repl
package eval

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.NameOps.*
import dotty.tools.dotc.core.DenotTransformers.DenotTransformer
import dotty.tools.dotc.core.Phases.*
import dotty.tools.dotc.report
import dotty.tools.dotc.transform.MacroTransform
import dotty.tools.dotc.util.SrcPos

import scala.collection.mutable

/** Post-typer phase that pulls the typed eval body out of its splice point and
 *  into the synthesised `__Expression.evaluate` method.
 *
 *  1. At the spliced `val __evalResult`, capture its rhs into [[EvalStore]] and
 *     replace it with `null.asInstanceOf[T]`.
 *  2. At `__Expression.evaluate`, run the captured body through
 *     [[ExtractTransformer]] to rewrite outer-scope references into `reflectEval`
 *     placeholders, and install the result as the rhs.
 *  3. The [[DenotTransformer]] hook re-parents body-local symbols from the val
 *     to `evaluate` so they survive the move.
 *
 *  Foundation port: the wrapper refl-helper retargeting (for private members) is
 *  omitted — those strategies route to runtime helpers that are stubbed pending
 *  task #10. Binding-based strategies (LocalValue/Assign, MethodCapture, This)
 *  are fully supported. */
private[eval] class ExtractEvalBody(config: EvalCompilerConfig, store: EvalStore)
  extends MacroTransform with DenotTransformer:

  override def phaseName: String = ExtractEvalBody.name
  override def changesMembers: Boolean = config.sessionLine

  /** Symbols of a session line's top-level definitions, lifted to public members
   *  of the line's `__EvalExpression` so the next line can `import __lineK.*`.
   *  Their re-owning, flag fix-up and (for `var`s) setter synthesis happen
   *  imperatively in [[ExtractEvalBodyTransformer.installSessionMembers]] — the
   *  one place that owns this transformation. */
  private val sessionMemberSymbols = mutable.Set.empty[Symbol]
  /** Symbols nested inside the line's trailing expression, re-owned to `evaluate`. */
  private val sessionEvalLocalSymbols = mutable.Set.empty[Symbol]
  /** Flags a lifted local definition must shed to become a normal public member. */
  private val sessionMemberMask: FlagSet = Private | PrivateLocal | Local | NonMember

  override def transformPhase(using Context): Phase = this.next

  override def transform(ref: SingleDenotation)(using Context): SingleDenotation =
    ref match
      case ref: SymDenotation if config.sessionLine && sessionEvalLocalSymbols.contains(ref.symbol) =>
        ref.copySymDenotation(owner = config.evaluateMethod)

      case ref: SymDenotation if isExpressionVal(ref.symbol.maybeOwner) =>
        ref.copySymDenotation(owner = config.evaluateMethod)
      case _ => ref

  override protected def newTransformer(using Context): Transformer =
    new ExtractEvalBodyTransformer

  private def isExpressionVal(sym: Symbol)(using Context): Boolean =
    sym.exists && sym.name == SpliceEvalBody.EvalResultName

  private def isWrapperSymbol(sym: Symbol)(using Context): Boolean =
    sym.exists && sym.name.toString.contains(ExtractEvalBody.WrapperMarker)

  private def holdsNoNestedClasses(wrapperClass: TypeDef)(using Context): Boolean =
    wrapperClass.rhs match
      case impl: Template =>
        !impl.body.exists {
          case td: TypeDef => td.isClassDef
          case _ => false
        }
      case _ => true

  private class ExtractEvalBodyTransformer extends Transformer:
    private var bodyTree: Tree | Null = null
    private var sessionMembers: List[Tree] = Nil
    private var sessionEvalBody: Tree | Null = null

    override def transform(tree: Tree)(using Context): Tree =
      tree match
        case PackageDef(pid, stats) =>
          val (exprClassDef, others) = stats.partition { stat =>
            stat.symbol.exists && stat.symbol == config.expressionClass
          }
          val transformedStats = (others ++ exprClassDef).map(transform)
          val wrapperClass = transformedStats.collectFirst {
            case stat: TypeDef if stat.isClassDef && isWrapperSymbol(stat.symbol) => stat
          }
          val canDropWrapper = wrapperClass.exists(holdsNoNestedClasses)
          val pruned =
            if !canDropWrapper then transformedStats
            else transformedStats.filter {
              case stat: TypeDef if isWrapperSymbol(stat.symbol) => false
              case stat: ValDef if isWrapperSymbol(stat.symbol) => false
              case _ => true
            }
          cpy.PackageDef(tree)(pid, pruned)

        case tree: ValDef if isExpressionVal(tree.symbol) =>
          bodyTree = tree.rhs
          store.store(tree.symbol)
          if config.sessionLine then
            val split = splitSessionBody(tree.rhs)
            sessionMembers = split.members
            sessionEvalBody = split.evalBody
            sessionMemberSymbols.clear()
            sessionEvalLocalSymbols.clear()
            sessionMemberSymbols ++= split.members.collect {
              case vd: ValDef => vd.symbol
              case dd: DefDef => dd.symbol
            }
            config.sessionValNames ++= split.members.collect {
              case vd: ValDef
                  if !vd.symbol.is(Given)
                  && vd.name.toString.nonEmpty =>
                vd.name.toString
            }
            collectSessionEvalLocalSymbols(split.evalBody)
          val defaultRhs = Literal(Constant(null)).cast(tree.tpt.tpe).withSpan(tree.rhs.span)
          cpy.ValDef(tree)(rhs = defaultRhs)

        case tree: DefDef if tree.symbol == config.evaluateMethod =>
          val captured = if config.sessionLine then sessionEvalBody else bodyTree
          if captured == null then tree
          else
            if config.sessionLine then installSessionEvalLocalDenots()
            val rhs = ExtractTransformer.transform(captured)
            cpy.DefDef(tree)(rhs = rhs)

        case tree: TypeDef if config.sessionLine && tree.symbol == config.expressionClass =>
          addSessionMembers(super.transform(tree).asInstanceOf[TypeDef])

        case _ => super.transform(tree)

    private case class SessionSplit(members: List[Tree], evalBody: Tree)

    private def splitSessionBody(tree: Tree)(using Context): SessionSplit =
      tree match
        case Block(stats, expr) =>
          val members = mutable.ListBuffer.empty[Tree]
          val evalStats = mutable.ListBuffer.empty[Tree]
          stats.foreach {
            case stat if isLiftableSessionMember(stat) => members += stat
            case stat: Import if config.sessionLine && isPriorLineImport(stat) =>
            case stat                                  => evalStats += stat
          }
          val evalBody =
            if evalStats.isEmpty then expr
            else cpy.Block(tree)(evalStats.toList, expr)
          SessionSplit(members.toList, evalBody)
        case _ =>
          SessionSplit(Nil, tree)

    private def isLiftableSessionMember(tree: Tree)(using Context): Boolean =
      tree match
        case vd: ValDef =>
          vd.symbol.exists && !vd.symbol.is(Synthetic) && !vd.name.isEmpty
        case dd: DefDef =>
          dd.symbol.exists && !dd.symbol.is(Synthetic) &&
            dd.name != nme.CONSTRUCTOR && !dd.name.isEmpty
        case _ => false

    private def isPriorLineImport(tree: Import)(using Context): Boolean =
      tree.expr match
        case Ident(name) => name.toString.startsWith("__line")
        case _ => false

    private def collectSessionEvalLocalSymbols(tree: Tree)(using Context): Unit =
      object LocalSymbolTraverser extends TreeTraverser:
        override def traverse(tree: Tree)(using Context): Unit =
          tree match
            case member: MemberDef
                if member.symbol.exists
                && !sessionMemberSymbols.contains(member.symbol) =>
              sessionEvalLocalSymbols += member.symbol
              traverseChildren(tree)
            case _ =>
              traverseChildren(tree)
      LocalSymbolTraverser.traverse(tree)

    /** Setters synthesised for lifted `var` members (see [[installSessionMembers]]).
     *  Held so [[addSessionMembers]] can emit their `def x_=(v) = ()` trees right
     *  after entering the symbols. */
    private var sessionSetterSyms: List[TermSymbol] = Nil

    /** Re-own each lifted definition to the `__EvalExpression` class as a normal
     *  public member, entering it into the class scope. For `var`s, also
     *  synthesise the setter that `Desugar` would have created for a class-level
     *  `var` (a local `var` never gets one) — without it the later `getters`
     *  phase tries to enter the setter itself, where `changesMembers` is off.
     *
     *  This is the single source of truth for the lift: re-owning, flag fix-up
     *  and setter creation all happen here (the `getters`/`memoize` phases then
     *  treat the members exactly like source-declared `val`/`var`/`def`s, so a
     *  lifted `val` keeps its compute-once-at-construction semantics). */
    private def installSessionMembers()(using Context): Unit =
      val setters = mutable.ListBuffer.empty[TermSymbol]
      sessionMembers.foreach {
        case member: MemberDef =>
          val sym = member.symbol
          sym.copySymDenotation(
            owner = config.expressionClass,
            initFlags = sym.flags &~ sessionMemberMask
          ).installAfter(ExtractEvalBody.this)
          sym.enteredAfter(ExtractEvalBody.this)
          if sym.is(Mutable) then
            val setter = makeSessionSetter(sym.asTerm)
            setter.enteredAfter(ExtractEvalBody.this)
            setters += setter
        case _ =>
      }
      sessionSetterSyms = setters.toList

    /** The public setter for a lifted `var`, mirroring `Desugar.valDef`. Its `()`
     *  body is filled in with the field write by the `memoize` phase. */
    private def makeSessionSetter(valSym: TermSymbol)(using Context): TermSymbol =
      newSymbol(
        config.expressionClass,
        valSym.name.setterName,
        Method | Accessor,
        MethodType(termName("x$1") :: Nil, valSym.info.widenExpr :: Nil, defn.UnitType)
      )

    private def installSessionEvalLocalDenots()(using Context): Unit =
      sessionEvalLocalSymbols.foreach { sym =>
        sym.copySymDenotation(owner = config.evaluateMethod)
          .installAfter(ExtractEvalBody.this)
      }

    private def addSessionMembers(tree: TypeDef)(using Context): TypeDef =
      if sessionMembers.isEmpty then tree
      else
        tree.rhs match
          case impl: Template =>
            installSessionMembers()
            val lifted = sessionMembers.map(ExtractTransformer.transform)
            val setterDefs = sessionSetterSyms.map(s => DefDef(s, unitLiteral).withSpan(tree.span))
            val body = impl.body.flatMap {
              case dd: DefDef if dd.symbol == config.evaluateMethod => (lifted ++ setterDefs) :+ dd
              case stat                                             => stat :: Nil
            }
            cpy.TypeDef(tree)(rhs = cpy.Template(impl)(body = body))
          case _ => tree
  end ExtractEvalBodyTransformer

  private object ExtractTransformer extends TreeMap:
    override def transform(tree: Tree)(using Context): Tree = tree match
      case _: ImportOrExport => tree

      // A same-line reference to a definition we are lifting to a class member:
      // qualify it as `this.<member>` (the body and the members were typed as
      // siblings in a block, so such references arrive as bare `Ident`s). Works
      // for `val`/`var` (field read) and parameterless `def`/`given` alike.
      case tree: Ident
          if config.sessionLine
          && sessionMemberSymbols.contains(tree.symbol) =>
        This(config.expressionClass).select(tree.symbol).withSpan(tree.span)

      case tree: This =>
        val cls = tree.symbol
        if cls == config.expressionClass then super.transform(tree)
        else if cls.is(ModuleClass) && isGloballyAccessible(cls) then
          super.transform(tree)
        else if cls.isClass && store.classOwners.contains(cls) then
          thisOrOuterValue(tree, cls.asClass)
        else
          report.error(
            s"eval: cannot reach outer `this` of class `${cls.name}` " +
              "from the eval call site (not in the captured owner chain).",
            tree.srcPos
          )
          super.transform(tree)

      case Assign(lhs: Ident, rhs)
          if isLocalVariable(lhs.symbol) && !isLocalToBody(lhs.symbol) =>
        setLocalValue(tree, lhs.symbol.asTerm, transform(rhs))

      case tree @ Assign(lhs, rhs) if isInaccessibleField(lhs) =>
        setField(tree, transformedQualifier(lhs), lhs.symbol.asTerm, transform(rhs))

      case tree: Ident =>
        val sym = tree.symbol
        if !sym.exists || isLocalToBody(sym) || isGloballyAccessible(sym) then
          super.transform(tree)
        else if isAccessibleViaStaticPrefix(tree) then
          super.transform(tree)
        else if isTermOwnedModule(sym) then
          super.transform(tree)
        else if isLocalVariable(sym) then
          getLocalValue(tree, sym.asTerm)
        else if isInaccessibleField(tree) then
          getField(tree, transformedQualifier(tree), sym.asTerm)
        else if isInaccessibleMethod(tree) then
          callMethod(tree, transformedQualifier(tree), sym.asTerm, Nil)
        else if isOuterMethodLocalDef(sym) then
          captureLocalMethod(tree, sym.asTerm, Nil)
        else
          report.error(
            s"eval: cannot reference outer symbol `${sym.name}` (owner: ${sym.owner}).",
            tree.srcPos
          )
          super.transform(tree)

      case tree: Select if isInaccessibleField(tree) =>
        getField(tree, transform(tree.qualifier), tree.symbol.asTerm)

      case tree: Select if isTermOwnedClassFieldAccess(tree) =>
        getField(tree, transform(tree.qualifier), tree.symbol.asTerm)

      case tree: Apply if isInaccessibleMethod(tree) =>
        val args = transformedMethodArgs(tree)
        callMethod(tree, transformedQualifier(tree), tree.symbol.asTerm, args)

      case tree: Apply if isTermOwnedClassMethodCall(tree) =>
        val args = transformedMethodArgs(tree)
        callMethod(tree, transformedQualifier(tree), tree.symbol.asTerm, args)

      case tree: TypeApply if isInaccessibleMethod(tree) =>
        val args = transformedMethodArgs(tree)
        callMethod(tree, transformedQualifier(tree), tree.symbol.asTerm, args)

      case tree: TypeApply if isTermOwnedClassMethodCall(tree) =>
        val args = transformedMethodArgs(tree)
        callMethod(tree, transformedQualifier(tree), tree.symbol.asTerm, args)

      case tree: Select if isInaccessibleMethod(tree) =>
        callMethod(tree, transform(tree.qualifier), tree.symbol.asTerm, Nil)

      case tree: Select if isTermOwnedClassMethodCall(tree) =>
        callMethod(tree, transform(tree.qualifier), tree.symbol.asTerm, Nil)

      case tree: Apply if isOuterMethodLocalDef(tree.fun.symbol) =>
        captureLocalMethod(tree, tree.fun.symbol.asTerm, transformedMethodArgs(tree))

      case tree: TypeApply if isOuterMethodLocalDef(tree.symbol) =>
        captureLocalMethod(tree, tree.symbol.asTerm, Nil)

      case _ => super.transform(tree)
    end transform

    private def getLocalValue(tree: Tree, sym: TermSymbol)(using Context): Tree =
      val castTo =
        if isTermOwnedClass(sym.info) then defn.ObjectType
        else tree.tpe.widen
      buildReflectEvalCast(tree, nullLiteral,
        ReflectEvalStrategy.LocalValue(sym, isByName(sym.info)), Nil, castTo)

    private def setLocalValue(tree: Tree, sym: TermSymbol, rhs: Tree)(using Context): Tree =
      reflectEvalPlaceholder(tree, nullLiteral,
        ReflectEvalStrategy.LocalValueAssign(sym), rhs :: Nil)

    private def getThisObject(tree: Tree, cls: ClassSymbol)(using Context): Tree =
      val castTo = if isTermOwnedSymbol(cls) then defn.ObjectType else cls.typeRef
      buildReflectEvalCast(tree, nullLiteral, ReflectEvalStrategy.This(cls), Nil, castTo)

    private def getOuter(tree: Tree, qualifier: Tree, outerCls: ClassSymbol)(using Context): Tree =
      buildReflectEvalCast(tree, qualifier, ReflectEvalStrategy.Outer(outerCls), Nil, outerCls.typeRef)

    private def buildReflectEvalCast(
        tree: Tree,
        qualifier: Tree,
        strategy: ReflectEvalStrategy,
        args: List[Tree],
        castTo: Type
    )(using Context): Tree =
      val evalArgs = List(
        qualifier,
        Literal(Constant(strategy.toString)),
        JavaSeqLiteral(args, TypeTree(defn.ObjectType))
      )
      cpy.Apply(tree)(
        Select(This(config.expressionClass), termName("reflectEval")),
        evalArgs
      ).withAttachment(ReflectEvalStrategy, strategy).cast(castTo)

    private def isAccessibleViaStaticPrefix(tree: Tree)(using Context): Boolean =
      val sym = tree.symbol
      if sym.exists && (sym.isPrivate || sym.is(Protected)) then false
      else tree.tpe match
        case tref: TermRef => isStaticPrefix(tref.prefix)
        case _ => false

    private def isStaticPrefix(prefix: Type)(using Context): Boolean = prefix match
      case tt: ThisType =>
        val cls = tt.cls
        cls.exists && cls.is(ModuleClass) && isGloballyAccessible(cls)
      case tref: TermRef =>
        val sym = tref.symbol
        sym.exists && sym.is(Module) && isStaticPrefix(tref.prefix)
      case _ => false

    private def isInaccessibleField(tree: Tree)(using Context): Boolean =
      val sym = tree.symbol
      sym.exists && sym.isField && sym.enclosingClass.isClass &&
        (sym.isPrivate || sym.is(Protected)) && !isLocalToBody(sym)

    private def isInaccessibleMethod(tree: Tree)(using Context): Boolean =
      val sym = tree.symbol
      sym.exists && sym.isRealMethod && sym.enclosingClass.isClass &&
        (sym.isPrivate || sym.is(Protected)) && !isLocalToBody(sym)

    private def isTermOwnedClassFieldAccess(tree: Tree)(using Context): Boolean =
      val sym = tree.symbol
      sym.exists && sym.isField && sym.enclosingClass.isClass &&
        isTermOwnedSymbol(sym.enclosingClass) &&
        !isLocalToBody(sym) &&
        !sym.isPrivate && !sym.is(Protected)

    private def isTermOwnedClassMethodCall(tree: Tree)(using Context): Boolean =
      val sym = tree.symbol
      sym.exists && sym.isRealMethod && !sym.isClassConstructor &&
        sym.enclosingClass.isClass &&
        isTermOwnedSymbol(sym.enclosingClass) &&
        !isLocalToBody(sym) &&
        !sym.isPrivate && !sym.is(Protected)

    private def isTermOwnedSymbol(cls: Symbol)(using Context): Boolean =
      cls.exists && cls.maybeOwner.exists && cls.maybeOwner.isTerm

    private def isTermOwnedClass(tpe: Type)(using Context): Boolean =
      val sym = tpe.widen.typeSymbol
      sym.exists && sym.isClass && isTermOwnedSymbol(sym)

    private def isTermOwnedModule(sym: Symbol)(using Context): Boolean =
      sym.exists && sym.is(Module) && isTermOwnedSymbol(sym)

    private def isOuterMethodLocalDef(sym: Symbol)(using Context): Boolean =
      sym.exists && sym.is(Method) && !sym.isClassConstructor &&
        // Owner is any term scope (method, or a `val`/block whose RHS is a
        // block) — the rewriter captured such defs as eta-expanded bindings
        // regardless. (The JVM port restricts to method owners; we broaden so a
        // `def` declared inside a `val r = { def h … }` block is reachable too.)
        sym.owner.exists && sym.owner.isTerm && !isLocalToBody(sym)

    private def getField(tree: Tree, qual: Tree, field: TermSymbol)(using Context): Tree =
      val resultTpe =
        if isTermOwnedClass(field.info) then defn.ObjectType else field.info.widen
      buildReflectEvalCast(tree, qual,
        ReflectEvalStrategy.Field(field, isByName = false, useReceiverClass(field)),
        Nil, resultTpe)

    private def setField(tree: Tree, qual: Tree, field: TermSymbol, rhs: Tree)(using Context): Tree =
      buildReflectEvalCast(tree, qual,
        ReflectEvalStrategy.FieldAssign(field, useReceiverClass(field)),
        rhs :: Nil, defn.UnitType)

    private def useReceiverClass(member: Symbol)(using Context): Boolean =
      val cls = member.enclosingClass
      cls.exists && cls.isClass && isTermOwnedSymbol(cls)

    private def callMethod(tree: Tree, qual: Tree, method: TermSymbol, args: List[Tree])(using Context): Tree =
      val rawResult = method.info.finalResultType.widen
      val resultTpe =
        if isTermOwnedClass(rawResult) then defn.ObjectType else rawResult
      buildReflectEvalCast(tree, qual,
        ReflectEvalStrategy.MethodCall(method, useReceiverClass(method)),
        args, resultTpe)

    private def captureLocalMethod(tree: Tree, method: TermSymbol, args: List[Tree])(using Context): Tree =
      buildReflectEvalCast(tree, nullLiteral,
        ReflectEvalStrategy.MethodCapture(method, method, isByName = false),
        args, tree.tpe.widen)

    private def transformedMethodArgs(tree: Tree)(using Context): List[Tree] = tree match
      case _: (Ident | Select) => Nil
      case Apply(fun, args) => transformedMethodArgs(fun) ++ args.map(transform)
      case TypeApply(fun, _) => transformedMethodArgs(fun)
      case _ => Nil

    private def transformedQualifier(tree: Tree)(using Context): Tree = tree match
      case Select(qual, _) => transform(qual)
      case Apply(fun, _) => transformedQualifier(fun)
      case TypeApply(fun, _) => transformedQualifier(fun)
      case Assign(lhs, _) => transformedQualifier(lhs)
      case Ident(_) =>
        val cls = tree.symbol.enclosingClass
        if cls.isClass && store.classOwners.contains(cls) then
          thisOrOuterValue(tree, cls.asClass)
        else
          report.error(
            s"eval: cannot synthesise qualifier for `${tree.symbol.name}` — " +
              s"its enclosing class `${cls.name}` is not in the captured owner chain.",
            tree.srcPos
          )
          nullLiteral
      case _ => nullLiteral

    private def thisOrOuterValue(tree: Tree, cls: ClassSymbol)(using Context): Tree =
      if cls.is(ModuleClass) && isGloballyAccessible(cls) then
        return ref(cls.sourceModule).withSpan(tree.span)
      val owners = store.classOwners
      val target = owners.indexOf(cls)
      if target < 0 then
        report.error(s"internal error: class `${cls.name}` not in classOwners", tree.srcPos)
        return getThisObject(tree, cls)
      val ths = getThisObject(tree, owners.head.asClass)
      owners.iterator.drop(1).take(target).foldLeft(ths) { (inner, outerSym) =>
        getOuter(tree, inner, outerSym)
      }

    private def reflectEvalPlaceholder(
        tree: Tree,
        qualifier: Tree,
        strategy: ReflectEvalStrategy,
        args: List[Tree]
    )(using Context): Tree =
      buildReflectEvalCast(tree, qualifier, strategy, args, tree.tpe.widen)

    private def isLocalVariable(sym: Symbol)(using Context): Boolean =
      sym.exists && !sym.is(Method) && sym.isLocalToBlock

    private def isByName(tpe: Type)(using Context): Boolean = tpe match
      case _: ExprType => true
      case ref: TermRef => isByName(ref.symbol.info)
      case _ => false

    private def isLocalToBody(sym: Symbol)(using Context): Boolean =
      val valSym = store.symbol
      if !sym.exists || valSym == null then false
      else sym.ownersIterator.exists(o =>
        o == valSym || o == config.evaluateMethod || o == config.expressionClass
      )

    private def isGloballyAccessible(sym: Symbol)(using Context): Boolean =
      if !sym.exists then false
      else if sym.isPrivate || sym.is(Protected) then false
      else if sym.is(Package) || sym.is(PackageClass) then true
      else
        val owner = sym.owner
        if !owner.exists then false
        else if owner.is(PackageClass) then true
        else if owner.is(ModuleClass) then isGloballyAccessible(owner)
        else false

  end ExtractTransformer

private[eval] object ExtractEvalBody:
  val name: String = "extractEvalBody"
  /** Substring shared by every wrapper module synthesised by the eval driver. */
  val WrapperMarker: String = "__EvalWrapper"
