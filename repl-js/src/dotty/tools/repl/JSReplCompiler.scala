package dotty.tools
package repl

import dotc.ast.untpd
import dotc.core.Contexts.*
import dotc.core.Decorators.*
import dotc.core.Constants.Constant
import dotc.core.Names.*
import dotc.core.NameKinds.ReplAssignName
import dotc.core.Phases.Phase
import dotc.core.StdNames.{nme, str}
import dotc.util.Spans.*

import scala.collection.mutable

/** A REPL compiler for the Scala.js backend.
 *
 *  It reuses `ReplCompiler` but swaps the wrapping phase for `JSReplPhase`,
 *  which bakes two JS-specific things into each `object rs$line$N` wrapper:
 *
 *   1. `println` statements rendering each fresh `resN` binding (the JVM REPL's
 *      reflective `Rendering` has no equivalent here), and
 *   2. a trivial `def replMain(): Unit = ()` trigger — the interpreter runs a
 *      `ModuleInitializer.mainMethod(wrapper, "replMain")` to force the object's
 *      lazy init, which runs the user code + render in the body.
 *
 *  Because each line is interpreted (`sjsir-interpreter`, no linker, no DCE),
 *  there is nothing to keep reachable: the previous `@JSExportTopLevel` root +
 *  keep-all-members machinery is gone.
 */
class JSReplCompiler extends ReplCompiler:
  override protected def frontendPhases: List[List[Phase]] = List(
    List(Parser()),
    List(JSReplPhase()),
    List(dotc.typer.TyperPhase(addRootImports = false)),
    List(dotc.transform.CheckUnused.PostTyper(), dotc.transform.CheckShadowing()),
    List(CollectTopLevelImports()),
    List(dotc.transform.PostTyper()),
    List(dotc.transform.UnrollDefinitions()),
  )

/** Mirrors the upstream `ReplPhase` wrapping, but appends the render statements +
 *  the `replMain` trigger to the generated `object rs$line$N`. */
class JSReplPhase extends Phase:
  import untpd.*

  def phaseName: String = "repl"

  protected def run(using Context): Unit =
    ctx.compilationUnit.untpdTree match
      case pkg @ PackageDef(_, stats) =>
        pkg.getAttachment(ReplCompiler.ReplState).foreach {
          case given State =>
            val defs = definitions(stats)
            val res  = wrapped(defs, Span(0, stats.last.span.end))
            res.putAttachment(ReplCompiler.ReplState, defs.state)
            ctx.compilationUnit.untpdTree = res
        }
      case _ =>

  private case class Definitions(stats: List[untpd.Tree], state: State)

  private def definitions(trees: List[untpd.Tree])(using Context, State): Definitions =
    import untpd.*
    val flattened = trees match
      case List(Block(stats, expr)) =>
        if expr eq EmptyTree then stats else stats :+ expr
      case _ => trees

    val state = summon[State]
    var valIdx = state.valIndex
    val defs = mutable.ListBuffer.empty[Tree]

    def maybeBumpValIdx(tree: Tree): Unit = tree match
      case apply: Apply   => for a <- apply.args  do maybeBumpValIdx(a)
      case tuple: Tuple   => for t <- tuple.trees do maybeBumpValIdx(t)
      case patDef: PatDef => for p <- patDef.pats do maybeBumpValIdx(p)
      case tree: NameTree => tree.name.show.stripPrefix(str.REPL_RES_PREFIX).toIntOption match
        case Some(n) if n >= valIdx => valIdx = n + 1
        case _                      =>
      case _ =>

    flattened.foreach {
      case expr @ Assign(id: Ident, _) =>
        val assignName = ReplAssignName(id.name.toTermName)
        val assign = ValDef(assignName, TypeTree(), id).withSpan(expr.span)
        defs += expr += assign
      case expr if expr.isTerm =>
        val resName = (str.REPL_RES_PREFIX + valIdx).toTermName
        valIdx += 1
        val vd = ValDef(resName, TypeTree(), expr).withSpan(expr.span)
        defs += vd
      case other =>
        maybeBumpValIdx(other)
        defs += other
    }

    Definitions(defs.toList, state.copy(objectIndex = state.objectIndex + 1, valIndex = valIdx))

  private def wrapped(defs: Definitions, span: Span)(using Context): untpd.PackageDef =
    import untpd.*

    val objectName = ctx.source.file.toString
    assert(objectName.startsWith(str.REPL_SESSION_LINE), s"unexpected wrapper name: $objectName")
    assert(objectName.endsWith(defs.state.objectIndex.toString))
    val objectTermName = objectName.toTermName
    ReplCompiler.objectNames.update(defs.state.objectIndex, objectTermName)

    val body = defs.stats ++ renderStmts(defs.stats) :+ replMain
    val tmpl = Template(emptyConstructor, Nil, Nil, EmptyValDef, body)
    val module = ModuleDef(objectTermName, tmpl).withSpan(span)
    PackageDef(Ident(nme.EMPTY_PACKAGE), List(module))

  // --- JS-specific injected members -----------------------------------------

  /** `def replMain(): Unit = ()` — invoked by the interpreter's module initializer
   *  to force the wrapper object's lazy init (running the user code + render). */
  private def replMain(using Context): untpd.Tree =
    import untpd.*
    DefDef(termName("replMain"), List(Nil), TypeTree(), Literal(Constant(())))

  /** `scala.Predef.println("resN = " + resN)` for each freshly-bound result val. */
  private def renderStmts(stats: List[untpd.Tree])(using Context): List[untpd.Tree] =
    import untpd.*
    stats.collect {
      case vd: ValDef if vd.name.toString.stripPrefix(str.REPL_RES_PREFIX).toIntOption.isDefined =>
        val name = vd.name
        val printlnRef = Select(Select(Ident(termName("scala")), termName("Predef")), termName("println"))
        // `"name = " + value` (toString). NB: `ScalaRunTime.stringOf` would render
        // REPL-style but isn't usable (it reflects via java.lang.Class/Package).
        val concat = Apply(Select(Literal(Constant(s"$name = ")), termName("+")), List(Ident(name)))
        Apply(printlnRef, List(concat))
    }
