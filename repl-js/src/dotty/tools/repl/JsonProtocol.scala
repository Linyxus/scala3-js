package dotty.tools
package repl

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js

/** The JSONL request/response protocol of the Scala.js REPL worker, factored out
 *  of [[JsonMain]] so the worker loop and the scripted test driver
 *  ([[EvalScriptedTests]]) exercise the exact same request → response path.
 *
 *  One request JSON object per line, one response JSON object per line. Pure:
 *  [[respond]] returns the response string rather than writing it, so callers
 *  decide what to do with it (write to stdout, or compare against a checkfile).
 */
object JsonProtocol:

  /** Handle one request `line` against `session`. Returns the response JSON
   *  string and whether the worker should stop (a `shutdown` request). */
  def respond(session: ReplSession, line: String): Future[(String, Boolean)] =
    parseJson(line) match
      case Left(error) => Future.successful((protocolError(error), false))
      case Right(req) =>
        stringField(req, "op") match
          case Some("eval") =>
            stringField(req, "code") match
              case Some(code) => session.eval(code).map(r => (evalResponse(r), false))
              case None       => Future.successful((protocolError("eval.code must be a string"), false))
          case Some("reset") =>
            session.reset().map(v => (resetResponse(v), false))
          case Some("shutdown") =>
            session.shutdown().map(_ => (shutdownResponse(session.version), true))
          case Some(_) =>
            Future.successful((protocolError("unknown op"), false))
          case None =>
            Future.successful((protocolError("op must be a string"), false))

  // --- response builders (single source of truth for the wire format) --------

  private def evalResponse(result: ReplSession.EvalResponse): String =
    val response = js.Dynamic.literal(
      op = "eval",
      ok = result.ok,
      output = result.output,
      stdout = result.stdout,
      stderr = result.stderr,
      stateVersion = result.stateVersion,
    )
    result.error.foreach(e => response.updateDynamic("error")(e))
    stringify(response)

  private def resetResponse(version: Int): String =
    stringify(js.Dynamic.literal(op = "reset", ok = true, stateVersion = version))

  private def shutdownResponse(version: Int): String =
    stringify(js.Dynamic.literal(op = "shutdown", ok = true, stateVersion = version))

  def protocolError(error: String): String =
    stringify(js.Dynamic.literal(op = "protocol", ok = false, error = error))

  // --- helpers ---------------------------------------------------------------

  private def stringify(obj: js.Dynamic): String = js.JSON.stringify(obj)

  private def parseJson(line: String): Either[String, js.Dynamic] =
    try Right(js.JSON.parse(line).asInstanceOf[js.Dynamic])
    catch case _: Throwable => Left("invalid JSON")

  private def stringField(obj: js.Dynamic, name: String): Option[String] =
    val value = obj.selectDynamic(name)
    if js.isUndefined(value) || value == null || js.typeOf(value) != "string" then None
    else Some(value.asInstanceOf[String])
