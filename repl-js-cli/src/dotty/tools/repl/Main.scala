package dotty.tools
package repl

import scala.scalajs.js
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

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
    if !ReplBootstrap.hasProcess then return

    // With `scalaJSUseMainModuleInitializer`, `main` is invoked with no args, so
    // read the real CLI args from `process.argv` (dropping `node` + script path).
    val args =
      if args0.nonEmpty then args0
      else
        try js.Dynamic.global.process.argv.asInstanceOf[js.Array[String]].jsSlice(2).toArray
        catch case _: Throwable => args0

    ReplBootstrap.createSessionFromEnv() match
      case Some(sessionF) =>
        sessionF.foreach { session =>
          val client = new JsonReplClient(session)
          args.toList match
            case "--script" :: file :: _ => runScript(client, file)
            case Nil                     => interactive(client)
            case lines                   => runLines(client, lines.map(l => s"$Prompt $l"))
        }
        sessionF.failed.foreach(reportError)
      case _ =>
        Console.err.println("error: set DOTTY_CLASSPATH_BIN and DOTTY_LINKER_LIBS_BIN (use bin/scala-repl-js)")
        ReplBootstrap.setExitCode(1)

  private val Prompt = "scala>"

  /** Replay a scripted-test file: reproduce the full transcript on stdout (shared
   *  with the scripted-test driver via [[ScriptedRepl]]). */
  private def runScript(client: JsonReplClient, file: String): Unit =
    ScriptedRepl.reproduce(
      client,
      ReplBootstrap.readFileLines(file),
      s => { js.Dynamic.global.process.stdout.write(s); () },
      s => { js.Dynamic.global.process.stderr.write(s); () },
    ).failed.foreach(reportError)
    ()

  /** Evaluate `scala> …` lines sequentially: echo each verbatim, then feed the
   *  text after the prompt to the JSON REPL client. */
  private def runLines(client: JsonReplClient, lines: List[String]): Unit =
    lines match
      case Nil => ()
      case line :: rest =>
        println(line)
        val evalF = client.eval(line.drop(Prompt.length))
        evalF.foreach { result =>
          ScriptedRepl.emitResponse(
            result,
            s => { js.Dynamic.global.process.stdout.write(s); () },
            s => { js.Dynamic.global.process.stderr.write(s); () },
          )
          runLines(client, rest)
        }
        evalF.failed.foreach(reportError)

  /** Interactive read-eval-print loop over Node's `readline`. */
  private def interactive(client: JsonReplClient): Unit =
    val process  = js.Dynamic.global.process
    println("Welcome to Scala 3 on JavaScript (scala3-repl-sjs).")
    println("Evaluates each line incrementally on the JS compiler + .sjsir interpreter. Type :quit to exit.")

    val pending = scala.collection.mutable.Queue[String]()
    var busy    = false
    var ended   = false
    lazy val lineReader: ConsoleLineReader =
      ConsoleLineReader.create(
        process,
        s"$Prompt ",
        l => { pending.enqueue(l); pump() },
        () => { ended = true; pump() },
      )

    def quit(): Unit =
      try lineReader.close() catch case _: Throwable => ()
      process.exit(0)

    def prompt(): Unit = lineReader.prompt()

    def pump(): Unit =
      if busy then ()
      else if pending.nonEmpty then
        val line = pending.dequeue()
        line.trim match
          case ":quit" | ":q" => quit()
          case ""             => prompt(); pump()
          case _ =>
            busy = true
            val evalF = client.eval(line)
            evalF.foreach { result =>
              ScriptedRepl.emitResponse(
                result,
                s => { process.stdout.write(s); () },
                s => { process.stderr.write(s); () },
              )
              busy = false; prompt(); pump()
            }
            evalF.failed.foreach { e =>
              busy = false
              reportError(e)
              prompt()
              pump()
            }
      else if ended then quit()

    prompt()

  private def reportError(e: Throwable): Unit =
    Console.err.println(Option(e.getMessage).filter(_.nonEmpty).getOrElse(e.toString))
    ReplBootstrap.setExitCode(1)
