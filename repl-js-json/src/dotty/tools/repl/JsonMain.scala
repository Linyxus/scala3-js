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
 *
 *  Interleaved with the responses are the unsolicited liveness notices — `hello`
 *  at boot, then `received`, `progress` and `tick` while a request is in flight —
 *  so a parent can tell a slow worker from a hung one. They all go through
 *  [[rawWrite]], never through whatever `process.stdout.write` currently is.
 *
 *  Extra libraries (packed `.bin` archives, see the `packLibBin` sbt task) can
 *  be preloaded with `--classpath a.bin:b.bin` or `DOTTY_EXTRA_LIBS_BIN`.
 */
object JsonMain:

  /** `process.stdout.write`, bound to `process.stdout` and captured at startup
   *  before anything can be evaluated.
   *
   *  The protocol channel must not read `process.stdout.write` at call time:
   *  [[ReplSession.eval]] swaps it out for the duration of every eval to capture
   *  user output, and evaluated code is free to replace it outright. Either would
   *  otherwise divert — or silently swallow — a reply or a liveness notice. */
  private var rawWrite: js.Function1[String, Any] =
    ((s: String) => js.Dynamic.global.process.stdout.write(s)): js.Function1[String, Any]

  def main(args: Array[String]): Unit =
    if !ReplBootstrap.hasProcess then return
    captureRawWrite()
    // Before session creation, which is asynchronous and slow: the parent should
    // learn the worker's capabilities without waiting for it.
    writeLine(JsonProtocol.hello(pid))
    ReplBootstrap.extractClasspathArgs(ReplBootstrap.args) match
      case Left(err) =>
        writeLine(JsonProtocol.protocolError(err))
        ReplBootstrap.setExitCode(1)
      case Right((extraLibs, _)) =>
        ReplBootstrap.createSessionFromEnv(extraLibs) match
          case Some(sessionF) =>
            sessionF.foreach(run)
            sessionF.failed.foreach { e =>
              writeLine(JsonProtocol.protocolError(
                Option(e.getMessage).filter(_.nonEmpty).getOrElse(e.toString)))
              ReplBootstrap.setExitCode(1)
            }
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

    // Liveness ticks, armed only while a request is in flight. `seq` counts over
    // the worker's whole lifetime rather than restarting per request.
    var tickSeq = 0
    var tickTimer: js.Dynamic | Null = null

    def startTicks(): Unit =
      val timer = js.Dynamic.global.setInterval(
        (() =>
          tickSeq += 1
          writeLine(JsonProtocol.tick(tickSeq))
        ): js.Function0[Unit],
        JsonProtocol.TickMs)
      // An unref'd timer is never a reason for Node to stay alive, so a stray
      // armed tick can't keep an otherwise-idle worker from exiting.
      try timer.unref() catch case _: Throwable => ()
      tickTimer = timer

    def stopTicks(): Unit =
      val timer = tickTimer
      if timer != null then
        tickTimer = null
        js.Dynamic.global.clearInterval(timer)

    def stop(): Unit =
      try rl.close() catch case _: Throwable => ()
      try process.exit(0) catch case _: Throwable => ()

    def pump(): Unit =
      if busy then ()
      else if pending.nonEmpty then
        val line = pending.dequeue()
        busy = true
        writeLine(JsonProtocol.received())
        startTicks()
        JsonProtocol.respond(session, line, phase => writeLine(JsonProtocol.progress(phase))).map { (json, shouldStop) =>
          // Disarm first: no tick may slip out between here and the response.
          stopTicks()
          writeLine(json)
          busy = false
          if shouldStop then stop() else pump()
        }.recover { case e: Throwable =>
          stopTicks()
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

  /** Take the one reference to the real `process.stdout.write` that every
   *  protocol line will use for the rest of the run. Must happen before a
   *  session exists, hence before any eval can patch the property. */
  private def captureRawWrite(): Unit =
    try
      val stdout = js.Dynamic.global.process.stdout
      rawWrite = stdout.write.applyDynamic("bind")(stdout).asInstanceOf[js.Function1[String, Any]]
    catch case _: Throwable => ()

  private def pid: Int =
    try js.Dynamic.global.process.pid.asInstanceOf[Double].toInt
    catch case _: Throwable => 0

  private def writeLine(json: String): Unit =
    rawWrite(json + "\n")
    ()

  private def throwableMessage(e: Throwable): String =
    Option(e.getMessage).filter(_.nonEmpty).getOrElse(e.getClass.getName)
