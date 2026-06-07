package dotty.tools
package repl

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/** Replays a `scala>`-format REPL transcript through the JSON REPL client,
 *  reproducing the full transcript (prompt echoes + rendered output) to a sink.
 *  Shared by the CLI `--script` mode ([[Main]]) and the scripted-test driver
 *  ([[ReplScriptedTests]]) so both produce the identical transcript.
 */
object ScriptedRepl:

  val Prompt = "scala>"

  /** Reproduce the transcript for `fileLines` into `emit` (each call already
   *  includes its trailing newline when appropriate). A leading
   *  `//> using options ...` / `// scalac:` directive becomes reset settings and
   *  is echoed first. */
  def reproduce(
      client: JsonReplClient,
      fileLines: List[String],
      emit: String => Unit,
      emitErr: String => Unit = _ => (),
  ): Future[Unit] =
    val (optsLine, body) = fileLines.headOption match
      case Some(h) if h.trim.startsWith("//>") || h.trim.startsWith("// scalac:") => (Some(h), fileLines.tail)
      case _ => (None, fileLines)
    val extra = optsLine.map(parseUsingOptions).getOrElse(Nil)
    optsLine.foreach(o => emit(o + "\n"))
    val inputs = body.filter(_.startsWith(Prompt))
    client.reset(extra).flatMap(_ => runLines(client, inputs, emit, emitErr))

  def parseUsingOptions(line: String): List[String] =
    val t = line.trim
    val rest =
      if t.startsWith("//> using options") then t.stripPrefix("//> using options")
      else if t.startsWith("// scalac:") then t.stripPrefix("// scalac:")
      else ""
    rest.trim.split("\\s+").toList.filter(_.nonEmpty)

  private def runLines(
      client: JsonReplClient,
      lines: List[String],
      emit: String => Unit,
      emitErr: String => Unit,
  ): Future[Unit] =
    lines match
      case Nil => Future.unit
      case line :: rest =>
        emit(line + "\n")
        client.eval(line.drop(Prompt.length)).flatMap { result =>
          emitResponse(result, emit, emitErr)
          runLines(client, rest, emit, emitErr)
        }

  def emitResponse(result: ReplSession.EvalResponse, emit: String => Unit, emitErr: String => Unit): Unit =
    if result.chunks.nonEmpty then
      result.chunks.foreach {
        case ReplSession.OutputChunk.ReplOutput(text) => emit(text)
        case ReplSession.OutputChunk.Stdout(text)     => emit(text)
        case ReplSession.OutputChunk.Stderr(text)     => emitErr(text)
      }
    else
      emit(result.stdout)
      emitErr(result.stderr)
      emit(result.output)
    if !result.ok && result.output.isEmpty then
      result.error.foreach(error => emit(error + "\n"))
