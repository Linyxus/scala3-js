package dotty.tools
package repl

import scala.collection.mutable
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.scalajs.js.typedarray.*

import dotty.tools.dotc.ClasspathBlob

/** JSONL stdio worker for the Scala.js REPL.
 *
 *  Protocol: one request JSON object per stdin line, one response JSON object
 *  per stdout line, processed strictly in input order. No request ids are used.
 */
object JsonMain:

  def main(args: Array[String]): Unit =
    if !hasProcess then return

    (env("DOTTY_CLASSPATH_BIN"), env("DOTTY_LINKER_LIBS_BIN")) match
      case (Some(cp), Some(lib)) =>
        val cpDir = ClasspathBlob.load(readArrayBuffer(cp))
        ReplSession.create(cpDir, readArrayBuffer(lib)).foreach(run)
      case _ =>
        writeProtocolError("set DOTTY_CLASSPATH_BIN and DOTTY_LINKER_LIBS_BIN")
        setExitCode(1)

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
        handleLine(session, line).map { shouldStop =>
          busy = false
          if shouldStop then stop()
          else pump()
        }.recover { case e: Throwable =>
          writeProtocolError(throwableMessage(e))
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

  private def handleLine(session: ReplSession, line: String): Future[Boolean] =
    parseJson(line) match
      case Left(error) =>
        writeProtocolError(error)
        Future.successful(false)
      case Right(req) =>
        stringField(req, "op") match
          case Some("eval") =>
            stringField(req, "code") match
              case Some(code) =>
                session.eval(code).map { result =>
                  val response = js.Dynamic.literal(
                    op = "eval",
                    ok = result.ok,
                    output = result.output,
                    stdout = result.stdout,
                    stderr = result.stderr,
                    stateVersion = result.stateVersion,
                  )
                  result.error.foreach(e => response.updateDynamic("error")(e))
                  writeJson(response)
                  false
                }
              case None =>
                writeProtocolError("eval.code must be a string")
                Future.successful(false)

          case Some("reset") =>
            session.reset().map { version =>
              writeJson(js.Dynamic.literal(
                op = "reset",
                ok = true,
                stateVersion = version,
              ))
              false
            }

          case Some("shutdown") =>
            session.shutdown().map { _ =>
              writeJson(js.Dynamic.literal(
                op = "shutdown",
                ok = true,
                stateVersion = session.version,
              ))
              true
            }

          case Some(_) =>
            writeProtocolError("unknown op")
            Future.successful(false)

          case None =>
            writeProtocolError("op must be a string")
            Future.successful(false)

  private def parseJson(line: String): Either[String, js.Dynamic] =
    try Right(js.JSON.parse(line).asInstanceOf[js.Dynamic])
    catch case _: Throwable => Left("invalid JSON")

  private def stringField(obj: js.Dynamic, name: String): Option[String] =
    val value = obj.selectDynamic(name)
    if js.isUndefined(value) || value == null || js.typeOf(value) != "string" then None
    else Some(value.asInstanceOf[String])

  private def writeProtocolError(error: String): Unit =
    writeJson(js.Dynamic.literal(
      op = "protocol",
      ok = false,
      error = error,
    ))

  private def writeJson(obj: js.Dynamic): Unit =
    js.Dynamic.global.process.stdout.write(js.JSON.stringify(obj) + "\n")
    ()

  private def hasProcess: Boolean =
    try { val _ = js.Dynamic.global.process.argv; true }
    catch { case _: Throwable => false }

  private def env(name: String): Option[String] =
    val v = js.Dynamic.global.process.env.selectDynamic(name)
    if js.isUndefined(v) || v == null then None else Some(v.asInstanceOf[String])

  private def readArrayBuffer(path: String): ArrayBuffer =
    val fs = js.Dynamic.global.require("fs")
    val u8 = fs.readFileSync(path).asInstanceOf[Uint8Array]
    u8.buffer.slice(u8.byteOffset, u8.byteOffset + u8.byteLength)

  private def setExitCode(code: Int): Unit =
    try js.Dynamic.global.process.exitCode = code
    catch case _: Throwable => ()

  private def throwableMessage(e: Throwable): String =
    Option(e.getMessage).filter(_.nonEmpty).getOrElse(e.getClass.getName)
