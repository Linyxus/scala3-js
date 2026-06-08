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
    val parsedBody = parseBody
    val expressionClass = parseExpressionClass
    val splicer = new Splicer(parsedBody, expressionClass)
    ctx.compilationUnit.untpdTree = splicer.transform(ctx.compilationUnit.untpdTree)
    if !spliced && config.testMode then
      report.error(
        s"eval body marker `${config.marker}` not found in enclosing source",
        ctx.compilationUnit.untpdTree.srcPos
      )

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

  private class Splicer(body: Tree, expressionClass: Seq[Tree]) extends UntypedTreeMap:
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
          val bodyTermNames = topLevelTermNames(effectiveBody)
          val sessionAliases =
            if !config.sessionLine then Nil
            else stats.collect {
              case vd: ValDef if isPriorLineAlias(vd) && !bodyTermNames(vd.name) => vd
            }
          val sessionLineValues =
            if !config.sessionLine then Nil
            else stats.collect {
              case vd: ValDef if isPriorLineValue(vd) => vd
            }
          val sessionImports =
            if !config.sessionLine then Nil
            else stats.collect {
              case imp: Import if isPriorLineImport(imp) => imp
            }
          val hoistedStats: List[Tree] =
            givens ++ sessionLineValues ++ sessionImports ++ sessionAliases
          val markerReplacement = mkExprBlock(effectiveBody, expr, hoistedStats)
          val keptStats = stats.filterNot(s => hoistedStats.exists(_ eq s))
          if keptStats.isEmpty then markerReplacement
          else cpy.Block(bk)(keptStats.map(transform), markerReplacement)

        // Marker found at expression position.
        case id: Ident if id.name.toString == config.marker =>
          mkExprBlock(body, id)

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

  private def topLevelTermNames(tree: Tree): Set[Name] =
    tree match
      case Block(stats, _) =>
        stats.collect {
          case vd: ValDef if !vd.name.isEmpty => vd.name
          case dd: DefDef if dd.name != nme.CONSTRUCTOR && !dd.name.isEmpty => dd.name
        }.toSet
      case _ => Set.empty

  private def isPriorLineAlias(tree: ValDef)(using Context): Boolean =
    tree.rhs match
      case Select(Ident(name), _) => name.toString.startsWith("__line")
      case _ => false

  private def isPriorLineValue(tree: ValDef): Boolean =
    tree.name.toString.startsWith("__line")

  private def isPriorLineImport(tree: Import): Boolean =
    tree.expr match
      case Ident(name) => name.toString.startsWith("__line")
      case _ => false

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
