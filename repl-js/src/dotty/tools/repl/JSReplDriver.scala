package dotty.tools
package repl

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.collection.mutable

import dotc.Driver
import dotc.ast.tpd
import dotc.core.Contexts.*
import dotc.core.Mode
import dotc.core.StdNames.str
import dotc.config.{SJSPlatform, Platform}
import dotc.classpath.{AggregateClassPath, VirtualDirectoryClassPath}
import dotc.reporting.Diagnostic
import dotc.util.SourceFile
import dotty.tools.io.{VirtualDirectory, AbstractFile}

/** The JS REPL driver: a persistent session that evaluates each line incrementally.
 *
 *  Per line:
 *   1. wrap the input with `JSReplCompiler` (`object rs$line$N` + import chain +
 *      render statements + a `replMain` trigger),
 *   2. compile it with `-scalajs` into the in-memory `sessionDir` (also on the
 *      classpath, so later lines see earlier wrappers),
 *   3. hand the line's fresh `.sjsir` to the interpreter and run its `replMain`.
 *
 *  The interpreter keeps the live VM: prior classes + heap persist, the full
 *  library is loaded, nothing re-runs and any earlier binding stays referenceable.
 */
class JSReplDriver(
  cpDir: VirtualDirectory,
  sessionDir: VirtualDirectory,
  runner: InterpreterRunner,
) extends Driver:

  override def sourcesRequired: Boolean = false

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

  /** The persistent root context (classpath + `-scalajs` + outputDir = session). */
  val rootCtx: Context =
    val ictx = initCtx.fresh.addMode(Mode.ReadPositions | Mode.Interactive)
    val ctx = setup(Array("-scalajs", "-color:never"), ictx) match
      case Some((_, ctx)) => ctx
      case None           => ictx
    val withOut = ctx.fresh.setSetting(ctx.settings.outputDir, sessionDir)
    withOut.base.initialize()(using withOut)
    withOut

  private val compiler = new JSReplCompiler

  /** `.sjsir` paths already handed to the interpreter (so each line feeds only
   *  its own fresh classes). */
  private val fed = mutable.Set.empty[String]

  def initialState: State = State(0, 0, Map.empty, Set.empty, false, rootCtx)

  // --- per-line evaluation --------------------------------------------------

  /** Evaluate one line of input, returning the next REPL state. */
  def evalLine(input: String, state: State): Future[State] =
    val idx = state.objectIndex + 1
    val src = SourceFile.virtual(str.REPL_SESSION_LINE + idx, input)

    given State = state
    ParseResult(src) match
      case parsed: Parsed if parsed.trees.nonEmpty =>
        val runState = newRun(state, parsed.reporter)
        // The wrapping phase reads `ctx.source` to name `object rs$line$N`.
        val compileState = runState.copy(context = runState.context.withSource(parsed.source))
        compiler.compile(parsed)(using compileState) match
          case Right((unit, newState)) =>
            // Thread user imports forward so the next line's wrapper imports this one.
            val newImports = extractTopLevelImports(newState.context)
            val allImports =
              if newImports.nonEmpty then newState.imports + (newState.objectIndex -> newImports)
              else newState.imports
            val threaded = newState.copy(
              imports = allImports,
              context = contextWithNewImports(newState.context, newImports),
            )
            runLine(idx, threaded)
          case Left((diags, newState)) =>
            printDiagnostics(diags, newState.context)
            Future.successful(newState)
      case SyntaxErrors(_, errs, _) =>
        printDiagnostics(errs, state.context)
        Future.successful(state)
      case _ =>
        Future.successful(state)

  /** Feed the line's freshly-compiled `.sjsir` to the interpreter and run it.
   *  The module initializer calls the *static* `replMain` forwarder, which the
   *  backend emits in the object's mirror class `rs$line$N` (no `$`). */
  private def runLine(idx: Int, newState: State): Future[State] =
    val className = str.REPL_SESSION_LINE + idx        // rs$line$N (mirror class)
    val fresh = collectSjsir(sessionDir).filterNot((path, _) => fed.contains(path))
    fed ++= fresh.keysIterator
    runner.loadAndRun(fresh, className)
      .map(_ => newState)
      .recover { case e: Throwable =>
        val cause = Option(e.getCause).getOrElse(e)
        Console.err.println(s"[repl] runtime error: ${cause.getMessage}")
        newState
      }

  private def newRun(state: State, reporter: dotc.reporting.StoreReporter = newStoreReporter): State =
    val run = compiler.newRun(rootCtx.fresh.setReporter(reporter), state)
    state.copy(context = run.runContext)

  private def extractTopLevelImports(ctx: Context): List[tpd.Import] =
    dotc.core.Phases.unfusedPhases(using ctx)
      .collectFirst { case phase: CollectTopLevelImports => phase.imports }.get

  private def contextWithNewImports(ctx: Context, imports: List[tpd.Import]): Context =
    if imports.isEmpty then ctx
    else imports.foldLeft(ctx.fresh.setNewScope)((c, imp) => c.importContext(imp, imp.symbol(using c)))

  private def collectSjsir(dir: AbstractFile, prefix: String = ""): Map[String, Array[Byte]] =
    dir.iterator.flatMap { f =>
      val p = if prefix.isEmpty then f.name else s"$prefix/${f.name}"
      if f.isDirectory then collectSjsir(f, p)
      else if f.name.endsWith(".sjsir") then Iterator((p, f.toByteArray))
      else Iterator.empty
    }.toMap

  private def printDiagnostics(diags: List[Diagnostic], ctx: Context): Unit =
    given Context = ctx
    diags.foreach(d => System.err.println(d.msg.message))
