package dotty.tools
package repl

import dotc.ast.untpd
import dotc.core.Contexts.*
import dotc.core.Decorators.*
import dotc.core.Constants.Constant
import dotc.core.Names.*
import dotc.core.NameOps.*
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
    // Fill the synthetic args (`bindings`/`expectedType`/`enclosingSource`) of
    // every `eval[T]` / `evalSafe[T]` call at this REPL line's call sites.
    List(new eval.EvalRewriteTyped(None)),
    List(dotc.transform.UnrollDefinitions()),
  )

  /** Drop `JUnitBootstrappers`: it forces `org.junit.Test` (absent from the REPL
   *  classpath), so `jsdefn.junit.TestAnnotClass` is `NoSymbol` and its `.asClass`
   *  throws a `ClassCastException` on Scala.js. The REPL never has JUnit tests.
   *
   *  Also insert [[WriteReplTasty]] right after `Pickler` so each line's `.tasty`
   *  lands in the session output dir. The JVM REPL gets this for free from
   *  `GenBCode` (which embeds TASTY in `.class`); on Scala.js `GenBCode` is
   *  stubbed, so without this a fresh-context compile (notably the dynamic
   *  `eval(...)` inner compile) couldn't resolve `rs$line$N` wrappers. */
  override protected def transformPhases: List[List[Phase]] =
    val base = super.transformPhases.map(_.filterNot(_.isInstanceOf[dotc.transform.sjs.JUnitBootstrappers]))
    // `transformPhases` runs right after dotty's `picklerPhases` (which includes
    // `Pickler`, populating `unit.pickled`), so prepending here writes `.tasty`
    // as early as possible.
    List(new WriteReplTasty) :: base

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

    // Auto-import the runtime `eval`/`evalSafe` entry points so the bare names
    // resolve in user code without an explicit import.
    val evalImport = Import(
      dotted("scala", "runtime", "eval", "Eval"),
      List(
        ImportSelector(Ident(termName("eval"))),
        ImportSelector(Ident(termName("evalSafe"))),
        ImportSelector(Ident(termName("evalLoop")))
      )
    ).withSpan(span)

    val body = evalImport :: (defs.stats ++ renderPushes(defs.stats) :+ replMain)
    val tmpl = Template(emptyConstructor, Nil, Nil, EmptyValDef, body)
    val module = ModuleDef(objectTermName, tmpl).withSpan(span)
    PackageDef(Ident(nme.EMPTY_PACKAGE), List(module))

  // --- JS-specific injected members -----------------------------------------

  /** `def replMain(): Unit = ()` — invoked by the interpreter's module initializer
   *  to force the wrapper object's lazy init (running the user code + render). */
  private def replMain(using Context): untpd.Tree =
    import untpd.*
    DefDef(termName("replMain"), List(Nil), TypeTree(), Literal(Constant(())))

  /** Crossing the 4th wall, JS-style.
   *
   *  For each freshly-bound value field, append a statement that renders its
   *  runtime value (via `scala.runtime.ReplRenderer.replStringOf`, which runs
   *  *inside* the interpreter where the value lives) and pushes the pair
   *  `[name, rendered]` onto the JS global array `__replRenders`. The driver
   *  resets that array before running the wrapper and reads it back after, then
   *  feeds the rendered strings into the same `renderVal` flow the JVM REPL uses
   *  — so the type header comes from the compiler and the value from the VM,
   *  exactly as on the JVM (where the header is `showUser` and the value is
   *  reflected + pprinted in the REPL classloader).
   *
   *  Lazy vals are deliberately skipped: the JVM REPL never forces them, and
   *  `replStringOf` would.
   */
  private def renderPushes(stats: List[untpd.Tree])(using Context): List[untpd.Tree] =
    import untpd.*
    stats.flatMap {
      case vd: ValDef if !vd.mods.is(dotc.core.Flags.Lazy) =>
        List(pushRender(vd.name.toString, Ident(vd.name)))
      case pd: PatDef if !pd.mods.is(dotc.core.Flags.Lazy) =>
        // Pattern bindings (`val Some(a) = …`, `val (a, b) = …`) only become
        // ValDefs after desugaring, so extract the bound names here and push for
        // each — referencing them by name (they resolve once desugared).
        pd.pats.flatMap(boundNames).distinct.map(n => pushRender(n, Ident(termName(n))))
      case _ => Nil
    }

  /** Names introduced by one left-hand side of a `val`/pattern definition. */
  private def boundNames(pat: untpd.Tree)(using Context): List[String] =
    import untpd.*
    pat match
      case id: Ident if id.name != nme.WILDCARD => List(id.name.toString) // bare lhs ident binds (any case)
      case other                                => patternVars(other)

  /** Variables bound *inside* a non-trivial pattern (lowercase, non-backquoted,
   *  non-wildcard idents and `Bind`s) — mirrors `Desugar.getVariables`. */
  private def patternVars(t: untpd.Tree)(using Context): List[String] =
    import untpd.*
    def isVar(id: Ident) = id.name.isVarPattern && !id.isBackquoted && id.name != nme.WILDCARD
    t match
      case Bind(nme.WILDCARD, body)         => patternVars(body)
      case Bind(name, body)                 => name.toString :: patternVars(body)
      case Typed(id: Ident, _) if isVar(id) => List(id.name.toString)
      case id: Ident if isVar(id)           => List(id.name.toString)
      case Apply(_, args)                   => args.flatMap(patternVars)
      case Typed(expr, _)                   => patternVars(expr)
      case NamedArg(_, arg)                 => patternVars(arg)
      case Tuple(trees)                     => trees.flatMap(patternVars)
      case Parens(p)                        => patternVars(p)
      case SeqLiteral(elems, _)             => elems.flatMap(patternVars)
      case Annotated(arg, _)                => patternVars(arg)
      case Block(Nil, expr)                 => patternVars(expr)
      case _                                => Nil

  /** Build a dotted selection `a.b.c…` rooted at an `Ident`. */
  private def dotted(parts: String*)(using Context): untpd.Tree =
    import untpd.*
    parts.tail.foldLeft(Ident(termName(parts.head)): Tree)((acc, p) => Select(acc, termName(p)))

  /** `scala.runtime.ReplRenderBridge.push(name, ReplRenderer.replStringOf(valueRef))`
   *
   *  The actual `js.Dynamic` push onto the global `__replRenders` array lives in
   *  `ReplRenderBridge` (its `library-js` override), not inline here. Both this
   *  call and `ReplRenderer.replStringOf` are in the `scala.runtime` package,
   *  which `SafeRefs` treats as assumed-safe, so the injected render survives a
   *  `language.experimental.safe` session — whereas an inline `js.Dynamic`
   *  selection would be rejected. See the matching global access in
   *  `InterpreterRunner.{resetBridge,readBridge}`. */
  private def pushRender(name: String, valueRef: untpd.Tree)(using Context): untpd.Tree =
    import untpd.*
    val rendered = Apply(dotted("scala", "runtime", "ReplRenderer", "replStringOf"), List(valueRef))
    Apply(dotted("scala", "runtime", "ReplRenderBridge", "push"), List(Literal(Constant(name)), rendered))

/** Writes each compiled REPL line's TASTY to the session output dir (one
 *  `<fqcn>.tasty` per top-level class), so a fresh-context compile (notably the
 *  dynamic `eval(...)` inner compile) can resolve `rs$line$N` wrappers from the
 *  classpath. The JVM REPL gets this from `GenBCode`; on Scala.js `GenBCode` is
 *  stubbed, so we pickle here.
 *
 *  We pickle the tree + attributes but deliberately SKIP the positions section:
 *  position pickling of the REPL's synthetic wrapper trees trips a `-1` in
 *  `PositionPickler` under the interpreter, and positions aren't needed for
 *  symbol resolution. Runs first in `transformPhases` (right after `Pickler`). */
class WriteReplTasty extends Phase:
  import dotc.core.tasty.{TastyPickler, TreePickler, Attributes, AttributePickler, ScratchData}
  import dotc.config.Feature
  def phaseName: String = "writeReplTasty"
  override def isCheckable: Boolean = false
  protected def run(using Context): Unit =
    val unit = ctx.compilationUnit
    val tree = unit.tpdTree
    if tree.isEmpty then return
    val outDir = ctx.settings.outputDir.value
    val attributes = Attributes(
      sourceFile = unit.source.path,
      scala2StandardLibrary = false,
      explicitNulls = ctx.settings.YexplicitNulls.value,
      captureChecked = Feature.ccEnabled,
      withPureFuns = Feature.pureFunsEnabled,
      isJava = false,
      isOutline = false
    )
    for cls <- unit.pickled.keys do
      val pickler = new TastyPickler(cls, isBestEffortTasty = false)
      val treePkl = new TreePickler(pickler, attributes)
      treePkl.pickle(tree :: Nil)
      val scratch = new ScratchData
      treePkl.compactify(scratch)
      AttributePickler.pickleAttributes(attributes, pickler, scratch.attributeBuffer)
      val bytes = pickler.assembleParts()
      val parts = cls.fullName.mangledString.split('.').toList
      var dir: dotty.tools.io.AbstractFile = outDir
      for p <- parts.init do dir = dir.subdirectoryNamed(p)
      val baseName = parts.last.stripSuffix("$")
      val f = dir.fileNamed(baseName + ".tasty")
      val os = f.output
      try os.write(bytes) finally os.close()
