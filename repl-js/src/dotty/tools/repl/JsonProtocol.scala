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
  final case class Response(
    json: String,
    shouldStop: Boolean,
    eval: Option[ReplSession.EvalResponse] = None,
  )

  /** Version announced in the [[hello]] notice, bumped whenever the shape of the
   *  worker → parent traffic changes. 2 added the liveness notices below. */
  final val ProtocolVersion = 2

  /** Cadence of the [[tick]] liveness notice while a request is in flight. Both
   *  the announced `tickMs` and the worker's actual timer read this. */
  final val TickMs = 1000

  def evalRequest(code: String): String =
    stringify(js.Dynamic.literal(op = "eval", code = code))

  def resetRequest(settings: List[String] = Nil): String =
    val request = js.Dynamic.literal(op = "reset")
    if settings.nonEmpty then request.updateDynamic("settings")(js.Array(settings*))
    stringify(request)

  def shutdownRequest(): String =
    stringify(js.Dynamic.literal(op = "shutdown"))

  /** Handle one request `line` against `session`. Returns the response JSON
   *  string and whether the worker should stop (a `shutdown` request).
   *
   *  `onPhase` receives the eval pipeline's stage transitions as they happen;
   *  the worker turns them into [[progress]] notices. */
  def respond(session: ReplSession, line: String, onPhase: String => Unit = _ => ()): Future[(String, Boolean)] =
    respondDetailed(session, line, onPhase).map(r => (r.json, r.shouldStop))

  /** Like [[respond]], but keeps the typed eval result for in-process clients
   *  that need data not represented on the JSON wire, such as output ordering. */
  def respondDetailed(session: ReplSession, line: String, onPhase: String => Unit = _ => ()): Future[Response] =
    parseJson(line) match
      case Left(error) => Future.successful(Response(protocolError(error), false))
      case Right(req) =>
        stringField(req, "op") match
          case Some("eval") =>
            stringField(req, "code") match
              case Some(code) => session.eval(code, onPhase).map(r => Response(evalResponse(r), false, Some(r)))
              case None       => Future.successful(Response(protocolError("eval.code must be a string"), false))
          case Some("reset") =>
            stringArrayField(req, "settings") match
              case Some(settings) => session.reset(settings).map(v => Response(resetResponse(v), false))
              case None           => Future.successful(Response(protocolError("reset.settings must be an array of strings"), false))
          case Some("shutdown") =>
            session.shutdown().map(_ => Response(shutdownResponse(session.version), true))
          case Some(_) =>
            Future.successful(Response(protocolError("unknown op"), false))
          case None =>
            Future.successful(Response(protocolError("op must be a string"), false))

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

  // --- unsolicited worker → parent notices -----------------------------------
  //
  // Lines the worker writes outside the request → response exchange, so a parent
  // can tell "still working" from "hung" without waiting for the reply. Each is a
  // whole JSON object on its own line, framed exactly like a response, and each
  // is written the moment it happens (never buffered).

  /** The worker's first line, written before the (slow, asynchronous) session is
   *  created so the parent learns the protocol version and tick cadence at once. */
  def hello(pid: Int): String =
    stringify(js.Dynamic.literal(op = "hello", protocol = ProtocolVersion, tickMs = TickMs, pid = pid))

  /** Written the instant a request is dequeued, before it is acted on. */
  def received(): String =
    stringify(js.Dynamic.literal(op = "received"))

  /** Written every [[TickMs]] for as long as a request is in flight. `seq` counts
   *  monotonically over the worker's lifetime, not per request. */
  def tick(seq: Int): String =
    stringify(js.Dynamic.literal(op = "tick", seq = seq))

  /** Written at each stage transition of the eval pipeline. `phase` is an opaque
   *  lowercase token: it names whatever stages the pipeline actually has, so
   *  parents should display it rather than enumerate it. */
  def progress(phase: String): String =
    stringify(js.Dynamic.literal(op = "progress", phase = phase))

  // --- helpers ---------------------------------------------------------------

  private def stringify(obj: js.Dynamic): String = js.JSON.stringify(obj)

  private def parseJson(line: String): Either[String, js.Dynamic] =
    try Right(js.JSON.parse(line).asInstanceOf[js.Dynamic])
    catch case _: Throwable => Left("invalid JSON")

  private def stringField(obj: js.Dynamic, name: String): Option[String] =
    val value = obj.selectDynamic(name)
    if js.isUndefined(value) || value == null || js.typeOf(value) != "string" then None
    else Some(value.asInstanceOf[String])

  private def stringArrayField(obj: js.Dynamic, name: String): Option[List[String]] =
    val value = obj.selectDynamic(name)
    if js.isUndefined(value) || value == null then Some(Nil)
    else if !js.Array.isArray(value) then None
    else
      val arr = value.asInstanceOf[js.Array[js.Any]]
      val out = List.newBuilder[String]
      var i = 0
      while i < arr.length do
        val elem = arr(i)
        if elem == null || js.isUndefined(elem) || js.typeOf(elem) != "string" then return None
        out += elem.asInstanceOf[String]
        i += 1
      Some(out.result())
