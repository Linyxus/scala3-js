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
  output: String => Unit = s => { js.Dynamic.global.process.stdout.write(s); () },
) extends Driver:
  import JSReplDriver.EvalResult

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

  // --- dynamic eval ----------------------------------------------------------

  /** Latest post-compile state, so a runtime `eval(...)` callback compiles its
   *  body against the most up-to-date session context (and can import even the
   *  currently-executing line's earlier definitions). */
  private var currentState: State | Null = null

  /** Per-call-site cache of compiled eval `__Expression` classes (keyed on body
   *  + enclosing source + imports). On a hit we skip recompilation and just
   *  re-instantiate with the call's fresh bindings. Stores compile failures too,
   *  so a bad body doesn't recompile every call. */
  private val evalCache = mutable.Map.empty[String, Either[(Array[String], String), String]]

  private var evalIdCounter = 0
  private def nextEvalId(): String = { evalIdCounter += 1; "e" + evalIdCounter }

  // --- output ----------------------------------------------------------------

  private def emit(s: String): Unit = output(s)
  private def outPrintln(s: String): Unit = emit(s + "\n")

  // --- per-line evaluation ---------------------------------------------------

  /** Evaluate one line of input, returning the next REPL state. */
  def evalLine(input: String, state: State): Future[State] =
    evalLineResult(input, state).map(_.state)

  /** Evaluate one line of input, returning success/error metadata as well as
   *  the next REPL state. Intended for machine protocols such as JSONL stdio. */
  def evalLineResult(input: String, state: State): Future[EvalResult] =
    given State = state
    interpret(ParseResult.complete(input))

  private def interpret(res: ParseResult)(using state: State): Future[EvalResult] =
    res match
      case parsed: Parsed if parsed.source.content.mkString.startsWith("//>") =>
        outPrintln("Please use `:dep com.example::artifact:version` to add dependencies in the REPL")
        Future.successful(success(state))
      case parsed: Parsed if parsed.trees.nonEmpty =>
        propagateLanguageImports(parsed.trees)
        compile(parsed, state)
      case SyntaxErrors(_, errs, _) =>
        val next = displayErrors(errs, state)
        Future.successful(failure(next, diagnosticsMessage(errs)))
      case cmd: Command =>
        interpretCommand(cmd)
      case _ =>
        Future.successful(success(state))

  /** Compile `parsed`, run the wrapper, then render its definitions. */
  private def compile(parsed: Parsed, istate: State): Future[EvalResult] =
    def extractNewestWrapper(tree: untpd.Tree): Name = tree match
      case PackageDef(_, (obj: untpd.ModuleDef) :: Nil) => obj.name.moduleClassName
      case _ => nme.NO_NAME

    given State =
      val state0 = newRun(istate, parsed.reporter)
      state0.copy(context = state0.context.withSource(parsed.source))

    compiler.compile(parsed).fold(
      { case (errs, newState) =>
        val next = displayErrors(errs, newState)
        Future.successful(failure(next, diagnosticsMessage(errs)))
      },
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

        // Make the post-compile state visible to the `eval(...)` runtime callback
        // before user code runs, so an eval call from within this very line can
        // import this line's own wrapper.
        currentState = newStateWithImports

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
            runError match
              case Some(e) => failure(updatedState, throwableMessage(e))
              case None    => success(updatedState)
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

  private def interpretCommand(cmd: Command)(using state: State): Future[EvalResult] = cmd match
    case UnknownCommand(cmd) =>
      outPrintln(s"""Unknown command: "$cmd", run ":help" for a list of commands""")
      Future.successful(success(state))
    case AmbiguousCommand(cmd, matching) =>
      outPrintln(s""""$cmd" matches ${matching.mkString(", ")}. Try typing a few more characters. Run ":help" for a list of commands""")
      Future.successful(success(state))
    case Help =>
      outPrintln(Help.text)
      Future.successful(success(state))
    case Reset(arg) =>
      val tokens = splitArgs(arg)
      if tokens.nonEmpty then
        outPrintln(s"""|Resetting REPL state with the following settings:
                       |  ${tokens.mkString("\n  ")}
                       |""".stripMargin)
      else
        outPrintln("Resetting REPL state.")
      resetState(tokens).map(success)
    case Imports =>
      for
        objectIndex <- state.validObjectIndexes
        imp <- state.imports.getOrElse(objectIndex, Nil)
      do outPrintln(imp.show(using state.context))
      Future.successful(success(state))
    case TypeOf(expr) =>
      expr match
        case "" =>
          outPrintln(":type <expression>")
          Future.successful(success(state))
        case _ =>
          compiler.typeOf(expr)(using newRun(state)).fold(
            errs => Future.successful(failure(displayErrors(errs, state), diagnosticsMessage(errs))),
            res =>
              outPrintln(res)
              Future.successful(success(state))
          )
    case DocOf(expr) =>
      expr match
        case "" =>
          outPrintln(":doc <expression>")
          Future.successful(success(state))
        case _ =>
          compiler.docOf(expr)(using newRun(state)).fold(
            errs => Future.successful(failure(displayErrors(errs, state), diagnosticsMessage(errs))),
            res =>
              outPrintln(res)
              Future.successful(success(state))
          )
    case Settings(arg) => arg match
      case "" =>
        given ctx: Context = state.context
        for s <- ctx.settings.userSetSettings(ctx.settingsState).sortBy(_.name) do
          outPrintln(s"${s.name} = ${if s.value == "" then "\"\"" else s.value}")
        Future.successful(success(state))
      case _ =>
        // Reconfigure on the *current* base (so prior wrappers stay valid) via
        // the full `setup` path — this applies multi-choice settings like
        // `-Wunused:all` exactly as `:reset` does.
        rootCtx = setupOn(rootCtx, splitArgs(arg))
        Future.successful(success(state.copy(context = rootCtx)))
    case Silent =>
      Future.successful(success(state.copy(quiet = !state.quiet)))
    case Quit =>
      Future.successful(success(state))
    case Require(_) =>
      outPrintln(":require is no longer supported, but has been replaced with :jar. Please use :jar")
      Future.successful(success(state))
    case JarCmd(path) =>
      outPrintln(s"""Cannot add "$path" to classpath.""")
      Future.successful(success(state))
    case KindOf(_) =>
      outPrintln("The :kind command is not currently supported.")
      Future.successful(success(state))
    case Sh(_) =>
      outPrintln("""The :sh command is deprecated. Use `import scala.sys.process._` and `"command".!` instead.""")
      Future.successful(success(state))
    case Load(_) =>
      outPrintln(":load is not supported in the JS REPL")
      Future.successful(success(state))
    case Dep(_) =>
      outPrintln(":dep is not supported in the JS REPL")
      Future.successful(success(state))

  /** Reset compiler, rendered state, session output, and interpreter. */
  def resetState(settings: List[String] = Nil): Future[State] =
    resetToInitial(settings)
    sessionDir.clear()
    runner.reset().flatMap { _ =>
      val initScript = rootCtx.settings.replInitScript.value(using rootCtx)
      if initScript.trim.nonEmpty then evalLine(initScript, initialState)
      else Future.successful(initialState)
    }

  /** Reset compiler/rendering/root context (the interpreter is reset separately). */
  private def resetToInitial(settings: List[String]): Unit =
    rootCtx = mkRootCtx(settings)
    compiler = new JSReplCompiler
    fed.clear()
    evalCache.clear()
    evalIdCounter = 0
    currentState = null
    ReplCompiler.objectNames.clear()

  private def success(state: State): EvalResult =
    EvalResult(state, ok = true, error = None)

  private def failure(state: State, error: String): EvalResult =
    EvalResult(state, ok = false, error = Some(error))

  private def diagnosticsMessage(errs: Seq[Diagnostic]): String =
    errs.map(_.msg.message).mkString("\n")

  private def throwableMessage(e: Throwable): String =
    Option(e.getMessage).filter(_.nonEmpty).getOrElse(e.getClass.getName)

  // --- helpers ---------------------------------------------------------------

  /** Whitespace-split a command argument, honouring single/double quotes.
   *  Replaces the compiler's `CommandLineParser.tokenize`, which calls
   *  `Character.isWhitespace(EOF = -1)` — that throws `ArrayIndexOutOfBounds`
   *  under Scala.js (the JVM returns false). */
  private def splitArgs(s: String): List[String] =
    val out = scala.collection.mutable.ListBuffer.empty[String]
    val sb = new StringBuilder
    var inTok = false
    var quote = '\u0000'
    def flush(): Unit = if inTok then { out += sb.toString; sb.setLength(0); inTok = false }
    var i = 0
    while i < s.length do
      val c = s.charAt(i)
      if quote != '\u0000' then
        if c == quote then quote = '\u0000' else sb.append(c)
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

  // --- dynamic eval runtime --------------------------------------------------

  /** The JS-global bridge entry point installed by [[ReplSession]] as
   *  `globalThis.__replEval`. `bindings` and the success `value` are opaque
   *  interpreter values that round-trip through here untouched. */
  def evalDynamicJS(code: js.Any, bindings: js.Any, expectedType: js.Any, enclosingSource: js.Any): js.Any =
    evalDynamic(code.asInstanceOf[String], bindings,
                expectedType.asInstanceOf[String], enclosingSource.asInstanceOf[String]) match
      case Right(value) =>
        js.Dynamic.literal(ok = true, value = value.asInstanceOf[js.Any])
      case Left((errors, source)) =>
        js.Dynamic.literal(ok = false, errors = js.Array(errors*), source = source)

  private def evalDynamic(
      code: String, bindings: Any, expectedType: String, enclosingSource: String
  ): Either[(Array[String], String), Any] =
    val state = currentState
    if state == null then return Left((Array("eval: no active REPL state"), ""))

    val imports = buildEvalImports(state)
    val cacheable = enclosingSource.nonEmpty
    val importsKey = imports.mkString("\n")
    val key = if cacheable then code + " | " + enclosingSource + " | " + importsKey else null

    if key != null then
      evalCache.get(key) match
        case Some(Left(failure))   => return Left(failure)
        case Some(Right(className)) => return Right(runner.instantiateEval(className, bindings))
        case None                   => ()

    val uuid = nextEvalId()
    val outputClassName = str.REPL_SESSION_LINE + uuid + "$__EvalExpression"
    val wrapperName     = str.REPL_SESSION_LINE + uuid + "$__EvalWrapper"
    val evalImport = "import scala.runtime.eval.Eval.{eval, evalSafe}\nimport scala.runtime.eval.EmbedRepl.embedRepl\n"
    val importBlock = if imports.isEmpty then evalImport else evalImport + imports.mkString("", "\n", "\n")
    val wrappedSource = s"${importBlock}object $wrapperName {\n$enclosingSource\n}\n"

    val outDir = new VirtualDirectory("(eval-output)", None)
    val config = eval.EvalCompilerConfig(
      outputClassName = outputClassName,
      body = code,
      expectedType = expectedType,
      outerEnclosingSource = enclosingSource
    )
    evalCompile(wrappedSource, outDir, config) match
      case Left(errors) =>
        val failure = (errors.toArray, spliceBodyForDisplay(wrappedSource, code))
        val isTransient = errors.exists(_.startsWith("Internal compiler error:"))
        if key != null && !isTransient then evalCache.put(key, Left(failure))
        Left(failure)
      case Right(()) =>
        runner.registerEvalClasses(collectSjsir(outDir))
        val v = runner.instantiateEval(outputClassName, bindings)
        if key != null then evalCache.put(key, Right(outputClassName))
        Right(v)

  /** Compile one eval wrapper source through [[eval.EvalCompiler]], writing
   *  `.sjsir` into `outDir`. Reuses the session's `rootCtx` (same classpath +
   *  `-scalajs` + `Mode.Interactive`), so the body type-checks against the live
   *  session exactly as a REPL line would. */
  private def evalCompile(
      wrappedSource: String, outDir: VirtualDirectory, config: eval.EvalCompilerConfig
  ): Either[Seq[String], Unit] =
    val errors = mutable.ListBuffer.empty[String]
    val reporter = new eval.EvalReporter(errors += _)
    // The inner compile MUST run on a *fresh* `ContextBase` (new symbol table),
    // not the session's `rootCtx`: dotc denotations are run-scoped, so reusing
    // the session symbol table for this second run trips
    // "denotation ... invalid in run N". A fresh base built the same way as the
    // session (same `-scalajs` settings + `cpDir`+`sessionDir` classpath via
    // `initCtx`) resolves prior-line wrappers from their `.tasty` in sessionDir.
    setup(baseSettings, freshBaseCtx) match
      case None => Left(Seq("eval: failed to set up inner compiler"))
      case Some((_, ctx0)) =>
        val base = ctx0.fresh
          .setSetting(ctx0.settings.outputDir, outDir)
          .setReporter(reporter)
        base.base.initialize()(using base)
        try
          val compiler = new eval.EvalCompiler(config)
          val run = compiler.newRun(using base)
          // `compileSources` with an explicit virtual source, not
          // `compileFromStrings` (which mints a UUID-named source and pulls in
          // `java.security.SecureRandom`, absent under Scala.js).
          val src = SourceFile.virtual("<eval-" + config.outputClassName + ">", wrappedSource)
          run.compileSources(src :: Nil)
          if reporter.hasErrors then Left(errors.toList)
          else Right(())
        catch case e: Throwable =>
          Left(Seq(s"Internal compiler error: ${e.getClass.getName}: ${Option(e.getMessage).getOrElse("")}"))

  /** Imports the eval body needs to see the live session: `import rs$line$N.*`
   *  for each valid prior wrapper, plus the user-typed imports at each line. */
  private def buildEvalImports(state: State): List[String] =
    val printCtx = state.context.fresh.setSetting(state.context.settings.color, "never")
    state.validObjectIndexes.flatMap { i =>
      val wrapperImport = ReplCompiler.objectNames.get(i)
        .filter(hasSessionTasty)
        .map(n => s"import $n.{given, *}")
      val userImports = state.imports.getOrElse(i, Nil).map(_.show(using printCtx))
      wrapperImport ++ userImports
    }.toList
      // The eval/embedRepl auto-imports (injected into every wrapper by
      // JSReplPhase) are collected as top-level imports; drop them here since
      // `evalDynamic` always prepends them explicitly. Avoids N duplicate
      // import lines in the wrapper.
      .filterNot(i =>
        i.contains("scala.runtime.eval.Eval")
          || i.contains("scala.runtime.eval.EmbedRepl.embedRepl"))
      .distinct

  private def hasSessionTasty(name: Name): Boolean =
    val parts = name.mangledString.split('.').toSeq
    val fileParts = parts.updated(parts.length - 1, parts.last.stripSuffix("$") + ".tasty")
    sessionDir.lookupPath(fileParts, directory = false) != null

  /** Replace the first marker in the wrapper source with the body, so compile
   *  errors show the actual code rather than the placeholder. */
  private def spliceBodyForDisplay(wrappedSource: String, body: String): String =
    val marker = scala.runtime.eval.EvalContext.placeholder
    val idx = wrappedSource.indexOf(marker)
    if idx < 0 then wrappedSource
    else wrappedSource.substring(0, idx) + body + wrappedSource.substring(idx + marker.length)

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

object JSReplDriver:
  final case class EvalResult(state: State, ok: Boolean, error: Option[String])
