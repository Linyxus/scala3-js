package dotty.tools
package repl
package eval

import dotty.tools.dotc.ast.untpd.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.report
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.dotc.util.SrcPos

/** Parser-stage phase that splices the eval body into the enclosing source at
 *  the marker position and appends a synthesised `__Expression` class to the
 *  same package.
 *
 *  The marker is a valid identifier ([[scala.runtime.eval.EvalContext.placeholder]]
 *  = `__evalBodyPlaceholder__`), so after parsing it appears as `Ident(<marker>)`
 *  exactly where the user's `eval(...)` call stood.
 *
 *  Input:
 *  {{{
 *  object EnclosingTest:
 *    def f(): Int = ({ __evalBodyPlaceholder__ })
 *  }}}
 *
 *  Output:
 *  {{{
 *  object EnclosingTest:
 *    def f(): Int = ({
 *      val __evalResult: Int = { <parsed body> }
 *      scala.runtime.eval.Eval.__noFold__()
 *      __evalResult
 *    })
 *
 *  class __Expression(bindings: Array[Eval.Binding]) extends EvalExpressionBase:
 *    def evaluate(): Any = ()
 *  }}}
 *
 *  This is the Scala.js foundation port: it omits the JVM port's class-method
 *  lift and private-member rewriting (deferred — they serve eval inside class
 *  methods). Each compilation should hit exactly one marker.
 */
private[eval] class SpliceEvalBody(config: EvalCompilerConfig) extends Phase:
  import SpliceEvalBody.*

  override def phaseName: String = SpliceEvalBody.name
  override def isCheckable: Boolean = false

  /** Sticky for the run: true once the marker has been found and replaced. */
  private var spliced = false

  /** Resets per `PackageDef` so `__Expression` is appended to exactly one. */
  private var expressionAppended = false

  protected def run(using Context): Unit =
    spliced = false
    expressionAppended = false
    val parsedBody = shapeEmbedReplLine(parseBody)
    val expressionClass = parseExpressionClass
    val chainDefs = chainClassDefsFor(parsedBody, ctx.compilationUnit.untpdTree)
    val splicer = new Splicer(parsedBody, expressionClass, chainDefs)
    ctx.compilationUnit.untpdTree = splicer.transform(ctx.compilationUnit.untpdTree)
    if !spliced && config.testMode then
      report.error(
        s"eval body marker `${config.marker}` not found in enclosing source",
        ctx.compilationUnit.untpdTree.srcPos
      )

  /** Shape an embedRepl session line. `EmbedReplSession.eval` appends the
   *  sentinel statement `_root_.scala.runtime.eval.EmbedRepl.endLine[T]()`
   *  (the `[T]` present when the call site's rendered type argument is) to the
   *  line's code; when the parsed body ends with it, rewrite the block to
   *  {{{
   *  <stats minus a trailing expression>
   *  val __embedReplLineValue__[: T] = <trailing expression | ()>
   *  throw new EmbedReplEnd(EmbedRepl.captureState(), __embedReplLineValue__)
   *  }}}
   *  so the line's value is its trailing expression (typed against `T` when
   *  given) or `()` for definition-only lines — REPL semantics, decided on the
   *  parsed tree rather than by string surgery.
   *
   *  The synthesised trees all carry the *sentinel's* span: the chained
   *  `enclosingSource` for the next line (computed by `EvalRewriteTyped` from
   *  the `captureState()` call's span in the body *source*) then replaces
   *  exactly the sentinel text, leaving the chain as `<code>\n<marker>` with
   *  the trailing expression as a typing-only discarded statement.
   *
   *  If the sentinel was swallowed by an unfinished construct in the code
   *  (e.g. a trailing `if c then`), it is not the block's result expression,
   *  no shaping happens, and the surviving `endLine` call fails loudly at
   *  runtime. */
  private def shapeEmbedReplLine(body: Tree)(using Context): Tree = body match
    case bk @ Block(stats0, expr0) =>
      val (stats, expr) = unswallowTrailingLambda(stats0, expr0)
      endLineTypeArg(expr) match
        case Some(targ) =>
          val span = expr.span
          val (front, trailing) =
            if stats.nonEmpty && stats.last.isTerm then (stats.init, Some(stats.last))
            else (stats, None)
          val vName = termName("__embedReplLineValue__")
          val vDef = ValDef(
            vName,
            targ.getOrElse(TypeTree()),
            trailing.getOrElse(Literal(Constant(())).withSpan(span))
          ).withSpan(span)
          val capture =
            Apply(selectFqn("scala.runtime.eval.EmbedRepl.captureState", span), Nil)
              .withSpan(span)
          val excTpt =
            Select(selectFqn("scala.runtime.eval", span), typeName("EmbedReplEnd"))
              .withSpan(span)
          val exc = Apply(
            Select(New(excTpt).withSpan(span), nme.CONSTRUCTOR).withSpan(span),
            List(capture, Ident(vName).withSpan(span))
          ).withSpan(span)
          cpy.Block(bk)(front :+ vDef, Throw(exc).withSpan(span))
        case None => body
    case _ => body

  /** Un-swallow the sentinel from a trailing lambda. A line ending with a bare
   *  lambda (`x => x * 2`) parses with the lambda's body extending to the end
   *  of the block, so the appended sentinel becomes the lambda body's result
   *  expression instead of the block's. Since the sentinel is ours and always
   *  appended *after* the user's code, finding it there is unambiguous: pull
   *  it out, restore the lambda (with the sentinel removed from its body) as
   *  the trailing statement, and let shaping proceed normally. A lambda whose
   *  whole body is the sentinel (code ended with `x =>`) is left alone — no
   *  user body to restore — and fails loudly via the surviving `endLine`. */
  private def unswallowTrailingLambda(stats: List[Tree], expr: Tree)(using Context): (List[Tree], Tree) =
    expr match
      case fn @ Function(params, fbody @ Block(fstats, fexpr))
          if endLineTypeArg(fexpr).isDefined && fstats.nonEmpty =>
        val restoredBody = fstats match
          case one :: Nil => one
          case more => cpy.Block(fbody)(more.init, more.last)
        (stats :+ cpy.Function(fn)(params, restoredBody), fexpr)
      case _ => (stats, expr)

  /** Match `[_root_.]scala.runtime.eval.EmbedRepl.endLine[T]()`; `Some(targ)`
   *  when it is the sentinel (with its optional explicit type-argument tree). */
  private def endLineTypeArg(tree: Tree): Option[Option[Tree]] =
    def path(t: Tree, acc: List[String]): List[String] = t match
      case Select(qual, name) => path(qual, name.toString :: acc)
      case Ident(name) => name.toString :: acc
      case _ => "<non-path>" :: acc
    def isEndLinePath(t: Tree): Boolean =
      path(t, Nil).takeRight(5) ==
        List("scala", "runtime", "eval", "EmbedRepl", "endLine")
    tree match
      case Apply(TypeApply(fn, targ :: Nil), Nil) if isEndLinePath(fn) => Some(Some(targ))
      case Apply(fn, Nil) if isEndLinePath(fn) => Some(None)
      case _ => None

  /** Class-like definitions (`class`/`case class`/`enum`/`trait`/`object`) in
   *  chain blocks enclosing the marker that the body references by simple
   *  name. They are *moved* out of the chain and re-elaborated inside the
   *  spliced body, where they are local to `__evalResult` and follow the
   *  proven same-line extraction path — a block-local class left in the kept
   *  chain wrapper cannot be referenced from `evaluate()` (lambdaLift cannot
   *  lift across the two top-level classes). The cost is per-line identity:
   *  each line that uses a chain class compiles (and, for an `object`,
   *  re-initialises) its own copy — the term-level session's documented
   *  semantics for type definitions.
   *
   *  Names the body defines itself are skipped (the body's own definition
   *  shadows the chain's); for a name defined by several chain levels the
   *  innermost wins. A chain definition that is hoisted away from a sibling
   *  that still references it leaves that sibling dangling — such mixed lines
   *  fail to type (a clear error where leaving the class in place would ICE
   *  in lambdaLift). */
  private def chainClassDefsFor(body: Tree, unitTree: Tree)(using Context): List[Tree] =
    val refNames = collectIdentNames(body)
    if refNames.isEmpty then return Nil
    val ownNames = topLevelDefNames(body)
    var spine: List[List[Tree]] = Nil // innermost-first snapshot at the marker
    val finder = new UntypedTreeTraverser:
      private var enclosing: List[List[Tree]] = Nil
      def traverse(tree: Tree)(using Context): Unit = tree match
        case id: Ident if id.name.toString == config.marker =>
          spine = enclosing
        case Block(stats, expr) =>
          enclosing = stats.filter(isClassLikeDef) :: enclosing
          stats.foreach(traverse)
          traverse(expr)
          enclosing = enclosing.tail
        case _ => traverseChildren(tree)
    finder.traverse(unitTree)
    val seen = scala.collection.mutable.Set.empty[Name]
    val chosenRev = spine.flatMap { levelDefs => // innermost level first
      levelDefs.reverse.filter { d =>
        val n = defName(d).toTermName
        refNames(n) && !ownNames(n) && seen.add(n)
      }
    }
    chosenRev.reverse // outermost-first, so inner re-definitions land later

  private def isClassLikeDef(tree: Tree): Boolean = tree match
    case td: TypeDef => td.isClassDef
    case _: ModuleDef => true
    case _ => false

  private def defName(tree: Tree): Name = tree match
    case td: TypeDef => td.name
    case md: ModuleDef => md.name
    case _ => nme.EMPTY

  /** Every `Ident` name occurring in `tree`, normalised to term names. */
  private def collectIdentNames(tree: Tree)(using Context): Set[Name] =
    val names = scala.collection.mutable.Set.empty[Name]
    val tr = new UntypedTreeTraverser:
      def traverse(t: Tree)(using Context): Unit = t match
        case id: Ident => names += id.name.toTermName
        case _ => traverseChildren(t)
    tr.traverse(tree)
    names.toSet

  /** Names the body's top-level statements define themselves. */
  private def topLevelDefNames(body: Tree): Set[Name] =
    body match
      case Block(stats, _) =>
        stats.collect {
          case td: TypeDef => td.name.toTermName
          case md: ModuleDef => md.name.toTermName
          case vd: ValDef if !vd.name.isEmpty => vd.name.toTermName
          case dd: DefDef if dd.name != nme.CONSTRUCTOR && !dd.name.isEmpty => dd.name.toTermName
        }.toSet
      case _ => Set.empty

  /** Strip `Apply(Ident(name), Nil)` → `Ident(name)` for any name in
   *  `parenless`, so the body can reference a parens-omitted sibling `def g = 42`
   *  as either `g` or `g()`. */
  private def stripEmptyApplyFor(body: Tree, parenless: Set[TermName])(using Context): Tree =
    val rewriter = new UntypedTreeMap:
      override def transform(tree: Tree)(using Context): Tree = tree match
        case Apply(id @ Ident(n), Nil) if parenless.contains(n.toTermName) =>
          Ident(n).withSpan(tree.span)
        case _ => super.transform(tree)
    rewriter.transform(body)

  private class Splicer(body: Tree, expressionClass: Seq[Tree], chainDefs: List[Tree]) extends UntypedTreeMap:
    private def isChainDef(t: Tree): Boolean = chainDefs.exists(_ eq t)

    override def transform(tree: Tree)(using Context): Tree =
      tree match
        case pkg: PackageDef =>
          val transformed = super.transform(pkg).asInstanceOf[PackageDef]
          if spliced && !expressionAppended then
            expressionAppended = true
            cpy.PackageDef(transformed)(
              transformed.pid,
              transformed.stats ++ expressionClass.map(_.withSpan(pkg.span))
            )
          else transformed

        // Block whose trailing expression is the marker: splice the body in and
        // hoist any sibling `given` declarations into the eval body so the body's
        // `summon[T]` stays co-located with its given after ExtractEvalBody
        // drains the block into evaluate().
        case bk @ Block(stats, expr: Ident) if expr.name.toString == config.marker =>
          val givens = stats.collect {
            case vd: ValDef if vd.mods.flags.is(Given) => vd
          }
          val parenslessDefNames = stats.collect {
            case dd: DefDef if dd.name != nme.CONSTRUCTOR && !dd.name.isEmpty && dd.paramss.isEmpty =>
              dd.name
          }.toSet
          val effectiveBody =
            if parenslessDefNames.isEmpty then body
            else stripEmptyApplyFor(body, parenslessDefNames)
          val hoistedStats: List[Tree] = chainDefs ++ givens
          val markerReplacement = mkExprBlock(effectiveBody, expr, hoistedStats)
          val keptStats = stats.filterNot(s => hoistedStats.exists(_ eq s))
          if keptStats.isEmpty then markerReplacement
          else cpy.Block(bk)(keptStats.map(transform), markerReplacement)

        // Marker found at expression position.
        case id: Ident if id.name.toString == config.marker =>
          mkExprBlock(body, id, chainDefs)

        // A chain block on the marker spine some of whose class-like definitions
        // were hoisted into the body: drop them here (their hoisted copies are
        // the definitions now).
        case bk @ Block(stats, expr) if stats.exists(isChainDef) =>
          cpy.Block(bk)(stats.filterNot(isChainDef).map(transform), transform(expr))

        case _ => super.transform(tree)

  /** Parse `config.body` as a block expression. Spans are relative to
   *  `config.body`. Inner eval calls have their `enclosingSource` filled by
   *  [[EvalRewriteTyped]]. */
  private def parseBody(using Context): Tree =
    val source = SourceFile.virtual("<eval-body>", config.body)
    val newCtx = ctx.fresh.setSource(source)
    val parser = Parsers.Parser(source)(using newCtx)
    parser.block()

  /** Parse the synthesised __Expression class declaration; returns the
   *  package's top-level stats. */
  private def parseExpressionClass(using Context): Seq[Tree] =
    val source = SourceFile.virtual("<eval-expression-class>", expressionClassSource)
    val newCtx = ctx.fresh.setSource(source)
    val parser = Parsers.Parser(source)(using newCtx)
    parser.parse().asInstanceOf[PackageDef].stats

  /** Source for the synthesised `__Expression` class — a thin subclass of
   *  [[scala.runtime.eval.EvalExpressionBase]] carrying only `evaluate()`, which
   *  [[ExtractEvalBody]] fills in with the typed user body. */
  private def expressionClassSource: String =
    s"""class ${config.outputClassName}(bindings: Array[scala.runtime.eval.Eval.Binding])
       |  extends scala.runtime.eval.EvalExpressionBase(bindings) {
       |  def evaluate(): Any = ()
      |}
      |""".stripMargin

  /** Build the splice block for the marker site:
   *  ```
   *  { val __evalResult = { <body> }; scala.runtime.eval.Eval.__noFold__(); __evalResult }
   *  ```
   *  The val carries the body's value for [[ExtractEvalBody]] to drain; the
   *  `__noFold__` effect prevents constant-folding of the surrounding expression;
   *  the block ends in `__evalResult` so the splice takes the body's type. */
  private def mkExprBlock(body: Tree, markerTree: Tree, hoistedStats: List[Tree] = Nil)(using Context): Tree =
    val span = markerTree.span
    if spliced then
      warnOrError(s"eval body marker `${config.marker}` appears more than once", markerTree.srcPos)
      Literal(Constant(())).withSpan(span)
    else
      spliced = true
      val effectiveBody: Tree =
        if hoistedStats.isEmpty then body
        else body match
          case Block(stats, expr) => cpy.Block(body)(hoistedStats ++ stats, expr)
          case _ => Block(hoistedStats, body).withSpan(body.span)
      val valTpt: Tree =
        if config.expectedType.isEmpty then TypeTree()
        else parseTypeFromString(config.expectedType, span)
      val valDef = ValDef(EvalResultName, valTpt, effectiveBody).withSpan(span)
      val effect = Apply(selectFqn("scala.runtime.eval.Eval.__noFold__", span), Nil).withSpan(span)
      val tail = Ident(EvalResultName).withSpan(span)
      Block(List(valDef, effect), tail).withSpan(span)

  /** Parse a type-source string into an untyped Tree via a synthetic
   *  `val __t__ : <typeStr> = ???`. Falls back to `TypeTree()` on parse failure. */
  private def parseTypeFromString(typeStr: String, span: Span)(using Context): Tree =
    val source = SourceFile.virtual("<eval-expected-type>", s"val __t__ : $typeStr = ???\n")
    val newCtx = ctx.fresh.setSource(source)
    val parser = Parsers.Parser(source)(using newCtx)
    val parsed =
      try parser.parse()
      catch case _: Throwable => null
    parsed match
      case pkg: PackageDef =>
        pkg.stats.headOption match
          case Some(vd: ValDef) => vd.tpt.withSpan(span)
          case _ => TypeTree().withSpan(span)
      case _ => TypeTree().withSpan(span)

  /** Build a dotted selection `a.b.c…` from a fully-qualified string. */
  private def selectFqn(fqn: String, span: Span)(using Context): Tree =
    val parts = fqn.split('.').toList
    parts.tail.foldLeft(Ident(termName(parts.head)).withSpan(span): Tree)(
      (acc, p) => Select(acc, termName(p)).withSpan(span))

  private def warnOrError(msg: String, srcPos: SrcPos)(using Context): Unit =
    if config.testMode then report.error(msg, srcPos)
    else report.warning(msg, srcPos)

private[eval] object SpliceEvalBody:
  val name: String = "spliceEvalBody"

  /** Name of the val we splice the body's value into; [[ExtractEvalBody]] drains
   *  its rhs into `__Expression.evaluate`. */
  val EvalResultName: TermName = termName("__evalResult")
