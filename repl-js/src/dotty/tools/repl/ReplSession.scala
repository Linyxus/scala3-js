package dotty.tools
package repl

import scala.concurrent.Future
import scala.collection.mutable.ListBuffer
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer

import dotty.tools.io.VirtualDirectory

/** A reusable, stateful Scala.js REPL session.
 *
 *  The human CLI owns its echo/prompt behavior directly. This session is the
 *  machine-facing API used by the JSONL worker: one eval updates one persistent
 *  REPL state, while REPL-rendered output and user stdout/stderr are kept in
 *  separate buffers. An ordered chunk stream is also retained for in-process
 *  front-ends that need to reproduce the human transcript exactly.
 */
final class ReplSession private (
  cpDir: VirtualDirectory,
  sessionDir: VirtualDirectory,
  runner: InterpreterRunner,
  extraCpDirs: List[VirtualDirectory],
):
  import ReplSession.*

  private var currentOutput: StringBuilder | Null = null
  private var currentChunks: ListBuffer[OutputChunk] | Null = null
  private val driver = new JSReplDriver(
    cpDir,
    sessionDir,
    runner,
    extraCpDirs = extraCpDirs,
    output = s =>
      val out = currentOutput
      if out != null then out.append(s)
      val chunks = currentChunks
      if chunks != null then chunks += OutputChunk.ReplOutput(s)
      ()
  )

  // Install the World-B → World-A bridge for dynamic `eval(...)`: user code
  // running inside the interpreter reaches the driver's compiler through this
  // JS global (see `scala.runtime.eval.EvalBridge`).
  js.Dynamic.global.globalThis.__replEval =
    ((code: js.Any, bindings: js.Any, expectedType: js.Any, enclosingSource: js.Any) =>
      driver.evalDynamicJS(code, bindings, expectedType, enclosingSource)
    ): js.Function4[js.Any, js.Any, js.Any, js.Any, js.Any]

  private var state: State = driver.initialState
  private var stateVersion: Int = 0

  def version: Int = stateVersion

  def eval(code: String): Future[EvalResponse] =
    val output = new StringBuilder
    val chunks = ListBuffer.empty[OutputChunk]
    currentOutput = output
    currentChunks = chunks
    captureProcessOutput(chunks)(driver.evalLineResult(code, state)).map {
      case (result, stdout, stderr) =>
        currentOutput = null
        currentChunks = null
        state = result.state
        if result.ok then stateVersion += 1
        EvalResponse(
          ok = result.ok,
          output = output.toString,
          stdout = stdout,
          stderr = stderr,
          error = result.error,
          stateVersion = stateVersion,
          chunks = chunks.toList,
        )
    }.recover {
      case e: Throwable =>
        currentOutput = null
        currentChunks = null
        EvalResponse(
          ok = false,
          output = output.toString,
          stdout = "",
          stderr = "",
          error = Some(throwableMessage(e)),
          stateVersion = stateVersion,
          chunks = chunks.toList,
        )
    }

  def reset(settings: List[String] = Nil): Future[Int] =
    val output = new StringBuilder
    val chunks = ListBuffer.empty[OutputChunk]
    currentOutput = output
    currentChunks = chunks
    captureProcessOutput(chunks)(driver.resetState(settings)).map {
      case (nextState, _, _) =>
        currentOutput = null
        currentChunks = null
        state = nextState
        stateVersion = 0
        stateVersion
    }.recover {
      case e: Throwable =>
        currentOutput = null
        currentChunks = null
        throw e
    }

  def shutdown(): Future[Unit] =
    Future.successful(())

  private def captureProcessOutput[A](chunks: ListBuffer[OutputChunk])(body: => Future[A]): Future[(A, String, String)] =
    val stdout = new StringBuilder
    val stderr = new StringBuilder

    val process = js.Dynamic.global.process
    val outObj = process.stdout
    val errObj = process.stderr
    val console = js.Dynamic.global.console

    val oldOutWrite = outObj.write
    val oldErrWrite = errObj.write
    val oldConsoleLog = console.log
    val oldConsoleError = console.error
    val oldConsoleWarn = console.warn

    def install(): Unit =
      outObj.updateDynamic("write")(((chunk: js.Any) =>
        val text = chunkToString(chunk)
        stdout.append(text)
        chunks += OutputChunk.Stdout(text)
        true
      ): js.Function1[js.Any, Boolean])
      errObj.updateDynamic("write")(((chunk: js.Any) =>
        val text = chunkToString(chunk)
        stderr.append(text)
        chunks += OutputChunk.Stderr(text)
        true
      ): js.Function1[js.Any, Boolean])
      console.updateDynamic("log")(((chunk: js.Any) =>
        val text = chunkToString(chunk) + "\n"
        stdout.append(text)
        chunks += OutputChunk.Stdout(text)
      ): js.Function1[js.Any, Unit])
      console.updateDynamic("error")(((chunk: js.Any) =>
        val text = chunkToString(chunk) + "\n"
        stderr.append(text)
        chunks += OutputChunk.Stderr(text)
      ): js.Function1[js.Any, Unit])
      console.updateDynamic("warn")(((chunk: js.Any) =>
        val text = chunkToString(chunk) + "\n"
        stderr.append(text)
        chunks += OutputChunk.Stderr(text)
      ): js.Function1[js.Any, Unit])

    def restore(): Unit =
      outObj.updateDynamic("write")(oldOutWrite)
      errObj.updateDynamic("write")(oldErrWrite)
      console.updateDynamic("log")(oldConsoleLog)
      console.updateDynamic("error")(oldConsoleError)
      console.updateDynamic("warn")(oldConsoleWarn)

    try
      install()
      body.map { value =>
        restore()
        (value, stdout.toString, stderr.toString)
      }.recover {
        case e: Throwable =>
          restore()
          throw e
      }
    catch
      case e: Throwable =>
        restore()
        Future.failed(e)

  private def chunkToString(chunk: js.Any): String =
    if chunk == null || js.isUndefined(chunk) then ""
    else if js.typeOf(chunk) == "string" then chunk.asInstanceOf[String]
    else
      try chunk.asInstanceOf[js.Dynamic].applyDynamic("toString")("utf8").asInstanceOf[String]
      catch case _: Throwable => chunk.toString

object ReplSession:
  sealed trait OutputChunk:
    def text: String

  object OutputChunk:
    final case class ReplOutput(text: String) extends OutputChunk
    final case class Stdout(text: String) extends OutputChunk
    final case class Stderr(text: String) extends OutputChunk

  final case class EvalResponse(
    ok: Boolean,
    output: String,
    stdout: String,
    stderr: String,
    error: Option[String],
    stateVersion: Int,
    chunks: List[OutputChunk] = Nil,
  )

  /** Create a session over the bundled classpath + interpreter libraries, with
   *  optional extra libraries (see [[ExtraLib]]) preloaded into all three sides.
   *  Shadowed extra-lib entries (first match wins) are reported on stderr. */
  def create(cpDir: VirtualDirectory, linkerLibs: ArrayBuffer, extraLibs: List[ExtraLib] = Nil): Future[ReplSession] =
    val sessionDir = new VirtualDirectory("(repl-session)", None)
    val runner = new InterpreterRunner
    for (libName, path) <- ExtraLib.shadowedPaths(cpDir, extraLibs) do
      Console.err.println(s"warning: $libName: classpath entry '$path' is shadowed by an earlier entry and ignored")
    registerJSModules(extraLibs)
    runner.loadLibrary(linkerLibs, extraLibs.map(_.sjsir)).map { _ =>
      new ReplSession(cpDir, sessionDir, runner, extraLibs.map(_.cpDir))
    }

  /** Publish the extra libraries' native companion JS modules, in classpath
   *  order, before any IR is loaded — a preloaded class's static initializer may
   *  already reach for a facade, and `loadIRFiles` runs those eagerly. */
  private def registerJSModules(extraLibs: List[ExtraLib]): Unit =
    for
      lib <- extraLibs
      (moduleName, code) <- lib.jsModules.toList.sortBy(_._1)
    do
      if !JSModuleRegistry.register(moduleName, code) then
        Console.err.println(s"warning: ${lib.name}: JS module '$moduleName' is already registered and ignored")

  private def throwableMessage(e: Throwable): String =
    Option(e.getMessage).filter(_.nonEmpty).getOrElse(e.getClass.getName)
