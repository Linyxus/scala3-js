package dotty.tools
package repl

import scala.collection.mutable
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js

/** JSONL stdio worker for the Scala.js REPL.
 *
 *  Protocol: one request JSON object per stdin line, one response JSON object
 *  per stdout line, processed strictly in input order (see [[JsonProtocol]]).
 */
object JsonMain:

  def main(args: Array[String]): Unit =
    if !ReplBootstrap.hasProcess then return
    ReplBootstrap.createSessionFromEnv() match
      case Some(sessionF) => sessionF.foreach(run)
      case None =>
        writeLine(JsonProtocol.protocolError("set DOTTY_CLASSPATH_BIN and DOTTY_LINKER_LIBS_BIN"))
        ReplBootstrap.setExitCode(1)

  private def run(session: ReplSession): Unit =
    val readline = js.Dynamic.global.require("readline")
    val process  = js.Dynamic.global.process
    val rl = readline.createInterface(js.Dynamic.literal(
      input = process.stdin,
      terminal = false,
    ))

    val pending = mutable.Queue.empty[String]
    var busy = false
    var closed = false

    def stop(): Unit =
      try rl.close() catch case _: Throwable => ()
      try process.exit(0) catch case _: Throwable => ()

    def pump(): Unit =
      if busy then ()
      else if pending.nonEmpty then
        val line = pending.dequeue()
        busy = true
        JsonProtocol.respond(session, line).map { (json, shouldStop) =>
          writeLine(json)
          busy = false
          if shouldStop then stop() else pump()
        }.recover { case e: Throwable =>
          writeLine(JsonProtocol.protocolError(throwableMessage(e)))
          busy = false
          pump()
        }
      else if closed then ()

    rl.on("line", ((line: String) =>
      pending.enqueue(line)
      pump()
    ): js.Function1[String, Unit])
    rl.on("close", (() =>
      closed = true
      pump()
    ): js.Function0[Unit])

  private def writeLine(json: String): Unit =
    js.Dynamic.global.process.stdout.write(json + "\n")
    ()

  private def throwableMessage(e: Throwable): String =
    Option(e.getMessage).filter(_.nonEmpty).getOrElse(e.getClass.getName)
