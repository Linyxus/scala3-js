package dotty.tools
package repl

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js

/** In-process client for the JSON REPL protocol.
 *
 *  This is used by higher-level front-ends that want the worker semantics without
 *  spawning a second Node process: they still send JSON protocol requests through
 *  [[JsonProtocol]], then decode the JSON responses back into typed results.
 */
final class JsonReplClient(session: ReplSession):

  def eval(code: String): Future[ReplSession.EvalResponse] =
    request(JsonProtocol.evalRequest(code)).map { response =>
      response.eval.getOrElse(decodeEvalResponse(response.json))
    }

  def reset(settings: List[String] = Nil): Future[Int] =
    request(JsonProtocol.resetRequest(settings)).map { response =>
      val json = response.json
      val obj = parseObject(json)
      if stringField(obj, "op") != Some("reset") || !boolField(obj, "ok").contains(true) then
        throw new IllegalStateException(errorMessage(obj).getOrElse("invalid reset response"))
      intField(obj, "stateVersion").getOrElse(0)
    }

  def shutdown(): Future[Unit] =
    request(JsonProtocol.shutdownRequest()).map { response =>
      val json = response.json
      val obj = parseObject(json)
      if stringField(obj, "op") != Some("shutdown") || !boolField(obj, "ok").contains(true) then
        throw new IllegalStateException(errorMessage(obj).getOrElse("invalid shutdown response"))
      ()
    }

  private def request(json: String): Future[JsonProtocol.Response] =
    JsonProtocol.respondDetailed(session, json).map { response =>
      val obj = parseObject(response.json)
      if stringField(obj, "op").contains("protocol") then
        throw new IllegalStateException(errorMessage(obj).getOrElse("protocol error"))
      response
    }

  private def decodeEvalResponse(json: String): ReplSession.EvalResponse =
    val obj = parseObject(json)
    if stringField(obj, "op") != Some("eval") then
      throw new IllegalStateException(errorMessage(obj).getOrElse("invalid eval response"))
    ReplSession.EvalResponse(
      ok = boolField(obj, "ok").getOrElse(false),
      output = stringField(obj, "output").getOrElse(""),
      stdout = stringField(obj, "stdout").getOrElse(""),
      stderr = stringField(obj, "stderr").getOrElse(""),
      error = errorMessage(obj),
      stateVersion = intField(obj, "stateVersion").getOrElse(0),
    )

  private def parseObject(json: String): js.Dynamic =
    js.JSON.parse(json).asInstanceOf[js.Dynamic]

  private def stringField(obj: js.Dynamic, name: String): Option[String] =
    val value = obj.selectDynamic(name)
    if js.isUndefined(value) || value == null || js.typeOf(value) != "string" then None
    else Some(value.asInstanceOf[String])

  private def boolField(obj: js.Dynamic, name: String): Option[Boolean] =
    val value = obj.selectDynamic(name)
    if js.isUndefined(value) || value == null || js.typeOf(value) != "boolean" then None
    else Some(value.asInstanceOf[Boolean])

  private def intField(obj: js.Dynamic, name: String): Option[Int] =
    val value = obj.selectDynamic(name)
    if js.isUndefined(value) || value == null || js.typeOf(value) != "number" then None
    else Some(value.asInstanceOf[Double].toInt)

  private def errorMessage(obj: js.Dynamic): Option[String] =
    stringField(obj, "error")
