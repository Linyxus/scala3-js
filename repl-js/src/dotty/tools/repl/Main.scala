package dotty.tools
package repl

import scala.scalajs.js
import scala.scalajs.js.typedarray.*
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import dotty.tools.dotc.ClasspathBlob
import dotty.tools.io.VirtualDirectory

/** JS/Node entry point for the Scala 3 REPL compiled to JavaScript.
 *
 *  Loads the bundled `classpath.bin` (for compiling lines) and `linker-libs.bin`
 *  (library `.sjsir` for linking lines) — their paths come from the
 *  `DOTTY_CLASSPATH_BIN` / `DOTTY_LINKER_LIBS_BIN` env vars set by the launcher —
 *  then evaluates each CLI argument as one REPL line (non-interactive proof
 *  stage; the interactive `readline` loop lands in a later milestone).
 */
object Main:

  def main(args: Array[String]): Unit =
    if !hasProcess then return

    val cpPath  = env("DOTTY_CLASSPATH_BIN")
    val libPath = env("DOTTY_LINKER_LIBS_BIN")
    (cpPath, libPath) match
      case (Some(cp), Some(lib)) =>
        import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
        val cpDir      = ClasspathBlob.load(readArrayBuffer(cp))         // for compiling lines
        val sessionDir = new VirtualDirectory("(repl-session)", None)
        val runner     = new InterpreterRunner
        val driver     = new JSReplDriver(cpDir, sessionDir, runner)

        // Load the full standard-library `.sjsir` into the interpreter once, then
        // start the loop (each line then loads only its own fresh classes).
        runner.loadLibrary(readArrayBuffer(lib)).foreach { _ =>
          val lines = readCliArgs(args).toList
          if lines.nonEmpty then runLines(driver, lines, driver.initialState)
          else interactive(driver)
        }
      case _ =>
        Console.err.println("error: set DOTTY_CLASSPATH_BIN and DOTTY_LINKER_LIBS_BIN (use bin/scala-repl-js)")
        setExitCode(1)

  /** Evaluate the lines sequentially: each waits for the previous to finish so
   *  state threads through and modules load in order. */
  private def runLines(driver: JSReplDriver, lines: List[String], state: State): Unit =
    lines match
      case Nil => ()
      case line :: rest =>
        println(s"scala> $line")
        driver.evalLine(line, state).foreach(next => runLines(driver, rest, next))

  /** Interactive read-eval-print loop over Node's `readline`.
   *
   *  Lines are queued as they arrive and processed one at a time: each line is
   *  fully evaluated (the eval is async — compile + interpret) before the next
   *  is pumped, so state threads through in order. A queue (rather than recursive
   *  `rl.question`) keeps it robust for piped stdin, which closes at EOF. */
  private def interactive(driver: JSReplDriver): Unit =
    val readline = js.Dynamic.global.require("readline")
    val process  = js.Dynamic.global.process
    val rl = readline.createInterface(js.Dynamic.literal(
      input = process.stdin, output = process.stdout, terminal = false))
    println("Welcome to Scala 3 on JavaScript (scala3-repl-sjs).")
    println("Evaluates each line incrementally on the JS compiler + .sjsir interpreter. Type :quit to exit.")

    val pending = scala.collection.mutable.Queue[String]()
    var state   = driver.initialState
    var busy    = false
    var ended   = false

    def quit(): Unit =
      try rl.close() catch case _: Throwable => ()
      process.exit(0)

    def prompt(): Unit = { js.Dynamic.global.process.stdout.write("scala> "); () }

    def pump(): Unit =
      if busy then ()
      else if pending.nonEmpty then
        pending.dequeue().trim match
          case ":quit" | ":q" => quit()
          case ""             => pump()
          case line =>
            busy = true
            driver.evalLine(line, state).foreach { next =>
              state = next; busy = false; prompt(); pump()
            }
      else if ended then quit()

    prompt()
    rl.on("line", ((l: String) => { pending.enqueue(l); pump() }): js.Function1[String, Unit])
    rl.on("close", (() => { ended = true; pump() }): js.Function0[Unit])

  // --- JS/Node helpers ------------------------------------------------------

  private def hasProcess: Boolean =
    try { val _ = js.Dynamic.global.process.argv; true }
    catch { case _: Throwable => false }

  private def readCliArgs(args: Array[String]): Array[String] =
    if args.nonEmpty then args
    else
      try
        val argv = js.Dynamic.global.process.argv.asInstanceOf[js.Array[String]]
        argv.jsSlice(2).toArray
      catch case _: Throwable => args

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
