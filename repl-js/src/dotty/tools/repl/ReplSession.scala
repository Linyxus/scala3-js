package dotty.tools
package repl

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer

import dotty.tools.io.VirtualDirectory

/** A reusable, stateful Scala.js REPL session.
 *
 *  The human CLI owns its echo/prompt behavior directly. This session is the
 *  machine-facing API used by the JSONL worker: one eval updates one persistent
 *  REPL state, while REPL-rendered output and user stdout/stderr are kept in
 *  separate buffers.
 */
final class ReplSession private (
  cpDir: VirtualDirectory,
  sessionDir: VirtualDirectory,
  runner: InterpreterRunner,
):
  import ReplSession.*

  private var currentOutput: StringBuilder | Null = null
  private val driver = new JSReplDriver(
    cpDir,
    sessionDir,
    runner,
    output = s =>
      val out = currentOutput
      if out != null then out.append(s)
      ()
  )

  private var state: State = driver.initialState
  private var stateVersion: Int = 0

  def version: Int = stateVersion

  def eval(code: String): Future[EvalResponse] =
    val output = new StringBuilder
    currentOutput = output
    captureProcessOutput(driver.evalLineResult(code, state)).map {
      case (result, stdout, stderr) =>
        currentOutput = null
        state = result.state
        if result.ok then stateVersion += 1
        EvalResponse(
          ok = result.ok,
          output = output.toString,
          stdout = stdout,
          stderr = stderr,
          error = result.error,
          stateVersion = stateVersion,
        )
    }.recover {
      case e: Throwable =>
        currentOutput = null
        EvalResponse(
          ok = false,
          output = output.toString,
          stdout = "",
          stderr = "",
          error = Some(throwableMessage(e)),
          stateVersion = stateVersion,
        )
    }

  def reset(): Future[Int] =
    val output = new StringBuilder
    currentOutput = output
    captureProcessOutput(driver.resetState()).map {
      case (nextState, _, _) =>
        currentOutput = null
        state = nextState
        stateVersion = 0
        stateVersion
    }.recover {
      case e: Throwable =>
        currentOutput = null
        throw e
    }

  def shutdown(): Future[Unit] =
    Future.successful(())

  private def captureProcessOutput[A](body: => Future[A]): Future[(A, String, String)] =
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
        stdout.append(chunkToString(chunk))
        true
      ): js.Function1[js.Any, Boolean])
      errObj.updateDynamic("write")(((chunk: js.Any) =>
        stderr.append(chunkToString(chunk))
        true
      ): js.Function1[js.Any, Boolean])
      console.updateDynamic("log")(((chunk: js.Any) =>
        stdout.append(chunkToString(chunk))
        stdout.append("\n")
      ): js.Function1[js.Any, Unit])
      console.updateDynamic("error")(((chunk: js.Any) =>
        stderr.append(chunkToString(chunk))
        stderr.append("\n")
      ): js.Function1[js.Any, Unit])
      console.updateDynamic("warn")(((chunk: js.Any) =>
        stderr.append(chunkToString(chunk))
        stderr.append("\n")
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
  final case class EvalResponse(
    ok: Boolean,
    output: String,
    stdout: String,
    stderr: String,
    error: Option[String],
    stateVersion: Int,
  )

  def create(cpDir: VirtualDirectory, linkerLibs: ArrayBuffer): Future[ReplSession] =
    val sessionDir = new VirtualDirectory("(repl-session)", None)
    val runner = new InterpreterRunner
    runner.loadLibrary(linkerLibs).map { _ =>
      new ReplSession(cpDir, sessionDir, runner)
    }

  private def throwableMessage(e: Throwable): String =
    Option(e.getMessage).filter(_.nonEmpty).getOrElse(e.getClass.getName)
