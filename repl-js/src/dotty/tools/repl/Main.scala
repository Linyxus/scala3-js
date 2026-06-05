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
 *  (library `.sjsir` for the interpreter) — their paths come from the
 *  `DOTTY_CLASSPATH_BIN` / `DOTTY_LINKER_LIBS_BIN` env vars set by the launcher.
 *
 *  Three modes:
 *   - `--script <file>`: replay a scripted-test transcript (echo each `scala>`
 *     line, evaluate it, print the rendered output) — used by the test harness.
 *   - CLI args: evaluate each argument as one line.
 *   - no args: interactive `readline` loop.
 */
object Main:

  def main(args0: Array[String]): Unit =
    if !hasProcess then return

    // With `scalaJSUseMainModuleInitializer`, `main` is invoked with no args, so
    // read the real CLI args from `process.argv` (dropping `bun` + script path).
    val args =
      if args0.nonEmpty then args0
      else
        try js.Dynamic.global.process.argv.asInstanceOf[js.Array[String]].jsSlice(2).toArray
        catch case _: Throwable => args0

    val cpPath  = env("DOTTY_CLASSPATH_BIN")
    val libPath = env("DOTTY_LINKER_LIBS_BIN")
    (cpPath, libPath) match
      case (Some(cp), Some(lib)) =>
        val cpDir      = ClasspathBlob.load(readArrayBuffer(cp))
        val sessionDir = new VirtualDirectory("(repl-session)", None)
        val runner     = new InterpreterRunner

        runner.loadLibrary(readArrayBuffer(lib)).foreach { _ =>
          args.toList match
            case "--script" :: file :: _ =>
              runScript(cpDir, sessionDir, runner, file)
            case Nil =>
              val driver = new JSReplDriver(cpDir, sessionDir, runner)
              interactive(driver)
            case lines =>
              val driver = new JSReplDriver(cpDir, sessionDir, runner)
              runLines(driver, lines.map(l => s"$Prompt $l"), driver.initialState)
        }
      case _ =>
        Console.err.println("error: set DOTTY_CLASSPATH_BIN and DOTTY_LINKER_LIBS_BIN (use bin/scala-repl-js)")
        setExitCode(1)

  private val Prompt = "scala>"

  /** Replay a scripted-test file: reproduce the full transcript on stdout so the
   *  harness can diff it against the file. Mirrors `ReplTest.testScript`. */
  private def runScript(cpDir: VirtualDirectory, sessionDir: VirtualDirectory,
                        runner: InterpreterRunner, file: String): Unit =
    val all = readFileLines(file)
    // An optional leading `//> using options …` directive becomes compiler
    // settings (and is echoed as the transcript's first line, as on the JVM).
    val (optsLine, body) = all.headOption match
      case Some(h) if h.trim.startsWith("//>") || h.trim.startsWith("// scalac:") => (Some(h), all.tail)
      case _ => (None, all)
    val extra = optsLine.map(parseUsingOptions).getOrElse(Nil)
    val driver = new JSReplDriver(cpDir, sessionDir, runner, extra)

    optsLine.foreach(println)
    val inputs = body.filter(_.startsWith(Prompt))
    runLines(driver, inputs, driver.initialState)

  private def parseUsingOptions(line: String): List[String] =
    val t = line.trim
    val rest =
      if t.startsWith("//> using options") then t.stripPrefix("//> using options")
      else if t.startsWith("// scalac:") then t.stripPrefix("// scalac:")
      else ""
    rest.trim.split("\\s+").toList.filter(_.nonEmpty)

  /** Evaluate `scala> …` lines sequentially: echo each verbatim, then feed the
   *  text after the prompt to the driver (which prints the rendered output). */
  private def runLines(driver: JSReplDriver, lines: List[String], state: State): Unit =
    lines match
      case Nil => ()
      case line :: rest =>
        println(line)
        driver.evalLine(line.drop(Prompt.length), state).foreach(next => runLines(driver, rest, next))

  /** Interactive read-eval-print loop over Node's `readline`. */
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

  private def env(name: String): Option[String] =
    val v = js.Dynamic.global.process.env.selectDynamic(name)
    if js.isUndefined(v) || v == null then None else Some(v.asInstanceOf[String])

  private def readArrayBuffer(path: String): ArrayBuffer =
    val fs = js.Dynamic.global.require("fs")
    val u8 = fs.readFileSync(path).asInstanceOf[Uint8Array]
    u8.buffer.slice(u8.byteOffset, u8.byteOffset + u8.byteLength)

  private def readFileLines(path: String): List[String] =
    val fs = js.Dynamic.global.require("fs")
    val content = fs.readFileSync(path, "utf-8").asInstanceOf[String]
    val arr = content.split("\n", -1).toList
    if arr.nonEmpty && arr.last == "" then arr.init else arr

  private def setExitCode(code: Int): Unit =
    try js.Dynamic.global.process.exitCode = code
    catch case _: Throwable => ()
