package dotty.tools
package repl

import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.collection.mutable

import dotc.Driver
import dotc.ast.{tpd, untpd}
import dotc.ast.Trees.*
import dotc.core.Contexts.*
import dotc.core.Decorators.*
import dotc.core.Mode
import dotc.core.Denotations.Denotation
import dotc.core.Flags.*
import dotc.core.NameKinds.{SimpleNameKind, DefaultGetterName}
import dotc.core.NameOps.*
import dotc.core.Names.Name
import dotc.core.StdNames.*
import dotc.core.Symbols.{Symbol, defn}
import dotc.core.Phases.{unfusedPhases, typerPhase, checkCapturesPhase}
import dotc.config.{SJSPlatform, Platform, Feature}
import dotc.classpath.{AggregateClassPath, VirtualDirectoryClassPath}
import dotc.interfaces
import dotc.reporting.{ConsoleReporter, StoreReporter, Diagnostic}
import dotc.util.{SourceFile, SourcePosition}
import dotty.tools.io.{VirtualDirectory, AbstractFile}

/** The JS REPL driver: a persistent session evaluating each line incrementally.
 *
 *  This is the Scala.js analogue of the JVM `ReplDriver`. It reuses the very same
 *  compile + render pipeline (`ReplCompiler`, `renderDefinitions`, diagnostics via
 *  `ConsoleReporter`/`MessageRendering`) so the on-screen output is identical to
 *  the JVM REPL. The two platform-specific differences are:
 *
 *   - execution: each line's `.sjsir` is run by the `sjsir-interpreter` (a
 *     persistent class registry + heap) instead of a JVM class loader, and
 *   - value rendering: a binding's runtime value is rendered *inside* the
 *     interpreter and shipped back via a JS-global bridge (see [[InterpreterRunner]]
 *     and [[JSRendering]]), instead of JVM reflection + pprint.
 *
 *  Because the value only exists after the wrapper runs, the per-line order is
 *  compile → (await interpreter run, populating the bridge) → render → print,
 *  whereas the JVM renders lazily during reflection. The observable output order
 *  (user `println` first, then the rendered definitions) is the same.
 */
class JSReplDriver(
  cpDir: VirtualDirectory,
  sessionDir: VirtualDirectory,
  runner: InterpreterRunner,
  extraSettings: List[String] = Nil,
) extends Driver:

  override def sourcesRequired: Boolean = false

  /** Base REPL options. `-pagewidth 80` + `-color:never` match the JVM scripted
   *  test harness (`ReplTest.commonOptions`) so diagnostic formatting lines up.
   *  `extraSettings` carries per-script `//> using options …` directives. */
  private val baseSettings: Array[String] =
    Array("-scalajs", "-color:never", "-pagewidth", "80") ++ extraSettings

  override protected def initCtx: Context =
    val base = new ContextBase:
      override protected def newPlatform(using Context): Platform =
        new SJSPlatform:
          override def classPath(using Context): dotty.tools.io.ClassPath =
            AggregateClassPath(Seq(
              VirtualDirectoryClassPath(cpDir),
              VirtualDirectoryClassPath(sessionDir),
            ))
    base.initialCtx

  /** A fresh base context (new symbol table) for the initial session / `:reset`. */
  private def freshBaseCtx: Context =
    val ictx = initCtx.fresh.addMode(Mode.ReadPositions | Mode.Interactive)
    ictx.setSetting(ictx.settings.XcookComments, true)
    ictx.setSetting(ictx.settings.XreadComments, true)
    ictx

  /** Apply `extra` settings on top of `baseCtx`, keeping its symbol table. Used
   *  both to build the initial root context (fresh base) and to reconfigure it
   *  for `:settings` (current base, so previously-compiled wrappers stay valid). */
  private def setupOn(baseCtx: Context, extra: List[String]): Context =
    setup((baseSettings ++ extra).toArray, baseCtx) match
      case Some((files, ctx)) =>
        if files.nonEmpty then
          inContext(ctx)(outPrintln(i"Ignoring spurious arguments: ${files.mkString(", ")}"))
        val withOut = ctx.fresh.setSetting(ctx.settings.outputDir, sessionDir)
        withOut.base.initialize()(using withOut)
        withOut
      case None => baseCtx

  private def mkRootCtx(extra: List[String]): Context = setupOn(freshBaseCtx, extra)

  private var rootCtx: Context = mkRootCtx(Nil)
  private var compiler = new JSReplCompiler
  private val rendering = new JSRendering

  /** `.sjsir` paths already handed to the interpreter (so each line feeds only
   *  its own fresh classes). */
  private val fed = mutable.Set.empty[String]

  def initialState: State = State(0, 0, Map.empty, Set.empty, false, rootCtx)

  // --- output ----------------------------------------------------------------

  private def emit(s: String): Unit =
    js.Dynamic.global.process.stdout.write(s); ()
  private def outPrintln(s: String): Unit = emit(s + "\n")

  // --- per-line evaluation ---------------------------------------------------

  /** Evaluate one line of input, returning the next REPL state. */
  def evalLine(input: String, state: State): Future[State] =
    given State = state
    interpret(ParseResult.complete(input))

  private def interpret(res: ParseResult)(using state: State): Future[State] =
    res match
      case parsed: Parsed if parsed.source.content.mkString.startsWith("//>") =>
        outPrintln("Please use `:dep com.example::artifact:version` to add dependencies in the REPL")
        Future.successful(state)
      case parsed: Parsed if parsed.trees.nonEmpty =>
        propagateLanguageImports(parsed.trees)
        compile(parsed, state)
      case SyntaxErrors(_, errs, _) =>
        Future.successful(displayErrors(errs, state))
      case cmd: Command =>
        interpretCommand(cmd)
      case _ =>
        Future.successful(state)

  /** Compile `parsed`, run the wrapper, then render its definitions. */
  private def compile(parsed: Parsed, istate: State): Future[State] =
    def extractNewestWrapper(tree: untpd.Tree): Name = tree match
      case PackageDef(_, (obj: untpd.ModuleDef) :: Nil) => obj.name.moduleClassName
      case _ => nme.NO_NAME

    given State =
      val state0 = newRun(istate, parsed.reporter)
      state0.copy(context = state0.context.withSource(parsed.source))

    compiler.compile(parsed).fold(
      { case (errs, newState) => Future.successful(displayErrors(errs, newState)) },
      { case (unit, newState) =>
        val newestWrapper = extractNewestWrapper(unit.untpdTree)
        val newImports = extractTopLevelImports(newState.context)
        val allImports =
          if newImports.nonEmpty then newState.imports + (newState.objectIndex -> newImports)
          else newState.imports
        val newStateWithImports = newState.copy(
          imports = allImports,
          context = contextWithNewImports(newState.context, newImports),
        )
        val warnings = newState.context.reporter.removeBufferedMessages(using newState.context)

        runWrapper(newState.objectIndex).map { runError =>
          inContext(newState.context):
            val (updatedState, definitions) =
              if ctx.settings.XreplDisableDisplay.value then (newStateWithImports, Seq.empty)
              else renderDefinitions(unit.tpdTree, newestWrapper, runError)(using newStateWithImports)

            given Ordering[Diagnostic] =
              Ordering[(Int, Int, Int)].on(d => (d.pos.line, -d.level, d.pos.column))

            (if istate.quiet then warnings else definitions ++ warnings)
              .sorted
              .foreach(printDiagnostic)
            updatedState
        }
      }
    )

  /** Feed the line's freshly-compiled `.sjsir` to the interpreter and run it,
   *  populating the value bridge. Returns the failure (if init threw). */
  private def runWrapper(objectIndex: Int): Future[Option[Throwable]] =
    val className = str.REPL_SESSION_LINE + objectIndex // mirror class (no `$`)
    val fresh = collectSjsir(sessionDir).filterNot((path, _) => fed.contains(path))
    fed ++= fresh.keysIterator
    runner.resetBridge()
    runner.loadAndRun(fresh, className)
      .map { _ =>
        rendering.withRenders(runner.readBridge())
        None
      }
      .recover { case e: Throwable =>
        rendering.withRenders(runner.readBridge())
        Some(Option(e.getCause).getOrElse(e))
      }

  // --- rendering (ported from ReplDriver.renderDefinitions) ------------------

  private def renderDefinitions(tree: tpd.Tree, newestWrapper: Name, runError: Option[Throwable])(using state: State): (State, Seq[Diagnostic]) =
    given Context = state.context

    def resAndUnit(denot: Denotation)(using Context): Boolean =
      import scala.util.{Success, Try}
      val sym = denot.symbol
      val name = sym.name.show
      val hasValidNumber = Try(name.drop(3).toInt) match
        case Success(num) => num < state.valIndex
        case _ => false
      name.startsWith(str.REPL_RES_PREFIX) && hasValidNumber && sym.info == defn.UnitType

    def extractAndFormatMembers(symbol: Symbol)(using Context): (State, Seq[Diagnostic]) =
      if tree.symbol.info.exists then
        val info = symbol.info
        val defs =
          info.bounds.hi.finalResultType
            .membersBasedOnFlags(required = Method, excluded = Accessor | ParamAccessor | Synthetic | Private)
            .filterNot { denot =>
              defn.topClasses.contains(denot.symbol.owner) || denot.symbol.isConstructor
                || denot.symbol.name.is(DefaultGetterName)
                || denot.symbol.name.toString == "replMain" // injected interpreter trigger
            }
        val vals =
          info.fields
            .filterNot(_.symbol.isOneOf(ParamAccessor | Private | Synthetic | Artifact | Module))
            .filter(_.symbol.name.is(SimpleNameKind))
        val typeAliases =
          info.bounds.hi.typeMembers.filter(_.symbol.info.isTypeAlias)

        var failedInit = false
        val renderedVals =
          val buf = mutable.ListBuffer[Diagnostic]()
          for d <- vals do if !failedInit then rendering.renderVal(d) match
            case Right(Some(v)) => buf += v
            case Left(e) =>
              buf += rendering.renderError(e, d)
              failedInit = true
            case _ =>
          buf.toList

        if failedInit then
          (state.copy(invalidObjectIndexes = state.invalidObjectIndexes + state.objectIndex), renderedVals)
        else
          val formattedMembers =
            typeAliases.map(rendering.renderTypeAlias)
              ++ defs.map(rendering.renderMethod)
              ++ renderedVals
          (state.copy(valIndex = state.valIndex - vals.count(resAndUnit)), formattedMembers)
      else (state, Seq.empty)

    def isSyntheticCompanion(sym: Symbol) = sym.is(Module) && sym.is(Synthetic)

    def typeDefs(sym: Symbol)(using Context): Seq[Diagnostic] = sym.info.memberClasses
      .collect {
        case x if !isSyntheticCompanion(x.symbol) && !x.symbol.name.isReplWrapperName =>
          rendering.renderTypeDef(x)
      }

    val renderPhase =
      if Feature.ccEnabledSomewhere && checkCapturesPhase.exists then checkCapturesPhase
      else typerPhase.next
    atPhase(renderPhase) {
      tree.symbol.info.memberClasses
        .find(_.symbol.name == newestWrapper.moduleClassName)
        .map { wrapperModule =>
          val (newState, formattedMembers) = extractAndFormatMembers(wrapperModule.symbol)
          val formattedTypeDefs =
            if newState.invalidObjectIndexes.contains(state.objectIndex) then Seq.empty
            else typeDefs(wrapperModule.symbol)
          (newState, formattedTypeDefs ++ formattedMembers)
        }
        .getOrElse((state, Seq.empty))
    }

  // --- commands --------------------------------------------------------------

  private def interpretCommand(cmd: Command)(using state: State): Future[State] = cmd match
    case UnknownCommand(cmd) =>
      outPrintln(s"""Unknown command: "$cmd", run ":help" for a list of commands""")
      Future.successful(state)
    case AmbiguousCommand(cmd, matching) =>
      outPrintln(s""""$cmd" matches ${matching.mkString(", ")}. Try typing a few more characters. Run ":help" for a list of commands""")
      Future.successful(state)
    case Help =>
      outPrintln(Help.text)
      Future.successful(state)
    case Reset(arg) =>
      val tokens = splitArgs(arg)
      if tokens.nonEmpty then
        outPrintln(s"""|Resetting REPL state with the following settings:
                       |  ${tokens.mkString("\n  ")}
                       |""".stripMargin)
      else
        outPrintln("Resetting REPL state.")
      resetToInitial(tokens)
      runner.reset().flatMap { _ =>
        val initScript = rootCtx.settings.replInitScript.value(using rootCtx)
        if initScript.trim.nonEmpty then evalLine(initScript, initialState)
        else Future.successful(initialState)
      }
    case Imports =>
      for
        objectIndex <- state.validObjectIndexes
        imp <- state.imports.getOrElse(objectIndex, Nil)
      do outPrintln(imp.show(using state.context))
      Future.successful(state)
    case TypeOf(expr) =>
      expr match
        case "" => outPrintln(":type <expression>")
        case _ =>
          compiler.typeOf(expr)(using newRun(state)).fold(
            errs => displayErrors(errs, state),
            res => outPrintln(res)
          )
      Future.successful(state)
    case DocOf(expr) =>
      expr match
        case "" => outPrintln(":doc <expression>")
        case _ =>
          compiler.docOf(expr)(using newRun(state)).fold(
            errs => displayErrors(errs, state),
            res => outPrintln(res)
          )
      Future.successful(state)
    case Settings(arg) => arg match
      case "" =>
        given ctx: Context = state.context
        for s <- ctx.settings.userSetSettings(ctx.settingsState).sortBy(_.name) do
          outPrintln(s"${s.name} = ${if s.value == "" then "\"\"" else s.value}")
        Future.successful(state)
      case _ =>
        // Reconfigure on the *current* base (so prior wrappers stay valid) via
        // the full `setup` path — this applies multi-choice settings like
        // `-Wunused:all` exactly as `:reset` does.
        rootCtx = setupOn(rootCtx, splitArgs(arg))
        Future.successful(state.copy(context = rootCtx))
    case Silent =>
      Future.successful(state.copy(quiet = !state.quiet))
    case Quit =>
      Future.successful(state)
    case Require(_) =>
      outPrintln(":require is no longer supported, but has been replaced with :jar. Please use :jar")
      Future.successful(state)
    case JarCmd(path) =>
      outPrintln(s"""Cannot add "$path" to classpath.""")
      Future.successful(state)
    case KindOf(_) =>
      outPrintln("The :kind command is not currently supported.")
      Future.successful(state)
    case Sh(_) =>
      outPrintln("""The :sh command is deprecated. Use `import scala.sys.process._` and `"command".!` instead.""")
      Future.successful(state)
    case Load(_) =>
      outPrintln(":load is not supported in the JS REPL")
      Future.successful(state)
    case Dep(_) =>
      outPrintln(":dep is not supported in the JS REPL")
      Future.successful(state)

  /** Reset compiler/rendering/root context (the interpreter is reset separately). */
  private def resetToInitial(settings: List[String]): Unit =
    rootCtx = mkRootCtx(settings)
    compiler = new JSReplCompiler
    fed.clear()
    ReplCompiler.objectNames.clear()

  // --- helpers ---------------------------------------------------------------

  /** Whitespace-split a command argument, honouring single/double quotes.
   *  Replaces the compiler's `CommandLineParser.tokenize`, which calls
   *  `Character.isWhitespace(EOF = -1)` — that throws `ArrayIndexOutOfBounds`
   *  under Scala.js (the JVM returns false). */
  private def splitArgs(s: String): List[String] =
    val out = scala.collection.mutable.ListBuffer.empty[String]
    val sb = new StringBuilder
    var inTok = false
    var quote = ' '
    def flush(): Unit = if inTok then { out += sb.toString; sb.setLength(0); inTok = false }
    var i = 0
    while i < s.length do
      val c = s.charAt(i)
      if quote != ' ' then
        if c == quote then quote = ' ' else sb.append(c)
      else c match
        case '\'' | '"'           => quote = c; inTok = true
        case w if w.isWhitespace  => flush()
        case _                    => sb.append(c); inTok = true
      i += 1
    flush()
    out.toList

  private def newRun(state: State, reporter: StoreReporter = newStoreReporter): State =
    val run = compiler.newRun(rootCtx.fresh.setReporter(reporter), state)
    state.copy(context = run.runContext)

  private def extractTopLevelImports(ctx: Context): List[tpd.Import] =
    unfusedPhases(using ctx).collectFirst { case phase: CollectTopLevelImports => phase.imports }.get

  private def contextWithNewImports(ctx: Context, imports: List[tpd.Import]): Context =
    if imports.isEmpty then ctx
    else imports.foldLeft(ctx.fresh.setNewScope)((c, imp) => c.importContext(imp, imp.symbol(using c)))

  /** Enable global `language.*` imports for subsequent parses/compiles (i16250). */
  private def propagateLanguageImports(trees: List[untpd.Tree]): Unit =
    import dotc.core.NameKinds.QualifiedName
    for case untpd.Import(expr, selectors) <- trees do
      untpd.languageImport(expr) match
        case Some(prefix) =>
          for case untpd.ImportSelector(untpd.Ident(imported), untpd.EmptyTree, _) <- selectors do
            val qual = QualifiedName(prefix, imported.asTermName)
            if Feature.globalLanguageImports.contains(qual) then
              val summary = rootCtx.settings.processArguments(List(s"-language:${qual.toString}"), true, rootCtx.settingsState)
              rootCtx = rootCtx.fresh.setSettings(summary.sstate)
        case _ =>

  private def collectSjsir(dir: AbstractFile, prefix: String = ""): Map[String, Array[Byte]] =
    dir.iterator.flatMap { f =>
      val p = if prefix.isEmpty then f.name else s"$prefix/${f.name}"
      if f.isDirectory then collectSjsir(f, p)
      else if f.name.endsWith(".sjsir") then Iterator((p, f.toByteArray))
      else Iterator.empty
    }.toMap

  // --- diagnostics -----------------------------------------------------------

  private def displayErrors(errs: Seq[Diagnostic], state: State): State =
    errs.foreach(printDiagnostic(_)(using state))
    state

  /** Like ConsoleReporter, but without file paths, and writing to our stdout. */
  private object ReplConsoleReporter extends ConsoleReporter.AbstractConsoleReporter:
    override def posFileStr(pos: SourcePosition) = ""
    override def printMessage(msg: String): Unit = outPrintln(msg)
    override def echoMessage(msg: String): Unit = printMessage(msg)
    override def flush()(using Context): Unit = ()

  private def printDiagnostic(dia: Diagnostic)(using state: State): Unit = dia.level match
    case interfaces.Diagnostic.INFO => outPrintln(dia.msg.message)
    case _                          => ReplConsoleReporter.doReport(dia)(using state.context)
