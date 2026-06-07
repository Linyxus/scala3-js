package dotty.tools
package repl

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import dotty.tools.io.VirtualDirectory

/** Replays a `scala>`-format REPL transcript, reproducing the full transcript
 *  (prompt echoes + rendered output) to a sink. Shared by the CLI `--script`
 *  mode ([[Main]]) and the scripted-test driver ([[ReplScriptedTests]]) so both
 *  produce the identical transcript. Mirrors the JVM `ReplTest.testScript`. */
object ScriptedRepl:

  val Prompt = "scala>"

  /** Reproduce the transcript for `fileLines` into `emit` (each call already
   *  includes its trailing newline). A leading `//> using options …` /
   *  `// scalac:` directive becomes compiler settings and is echoed first. */
  def reproduce(
      cpDir: VirtualDirectory, sessionDir: VirtualDirectory,
      runner: InterpreterRunner, fileLines: List[String], emit: String => Unit
  ): Future[Unit] =
    val (optsLine, body) = fileLines.headOption match
      case Some(h) if h.trim.startsWith("//>") || h.trim.startsWith("// scalac:") => (Some(h), fileLines.tail)
      case _ => (None, fileLines)
    val extra = optsLine.map(parseUsingOptions).getOrElse(Nil)
    val driver = new JSReplDriver(cpDir, sessionDir, runner, extra, output = emit)
    optsLine.foreach(o => emit(o + "\n"))
    val inputs = body.filter(_.startsWith(Prompt))
    runLines(driver, inputs, driver.initialState, emit)

  def parseUsingOptions(line: String): List[String] =
    val t = line.trim
    val rest =
      if t.startsWith("//> using options") then t.stripPrefix("//> using options")
      else if t.startsWith("// scalac:") then t.stripPrefix("// scalac:")
      else ""
    rest.trim.split("\\s+").toList.filter(_.nonEmpty)

  private def runLines(driver: JSReplDriver, lines: List[String], state: State, emit: String => Unit): Future[Unit] =
    lines match
      case Nil => Future.unit
      case line :: rest =>
        emit(line + "\n")
        driver.evalLine(line.drop(Prompt.length), state).flatMap(next => runLines(driver, rest, next, emit))
