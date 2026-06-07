package dotty.tools
package repl

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js

/** Scripted-test driver for the JSON REPL — the Scala.js analogue of dotty's
 *  `dotty.tools.repl.ScriptedTests`.
 *
 *  Each file in `EVAL_SCRIPTS_DIR` (default `repl-js/test-resources/eval`) is a
 *  JSONL transcript: lines beginning `>>> ` are requests; the line after each is
 *  the expected response (exact JSON). Comment (`#`) and blank lines are ignored
 *  on run and preserved/regenerated on `--update`. Every file runs against a
 *  freshly reset session, replayed in-process through [[JsonProtocol]] — the same
 *  path the worker uses — and each response is compared verbatim.
 *
 *  Run with `sbt scala3-repl-json-sjs/test`; regenerate the expected-response
 *  lines (checkfile model) with `sbt scala3-repl-json-sjs/updateEvalChecks`.
 *  A name-substring filter and `--update` may also be passed as plain args when
 *  invoking the linked main directly (`node …/main.js [filter] [--update]`).
 */
object EvalScriptedTests:

  private val RequestPrefix = ">>> "

  final case class Turn(comments: List[String], request: String, expected: String)

  def main(unused: Array[String]): Unit =
    if !ReplBootstrap.hasProcess then return
    val args = ReplBootstrap.args
    val update = args.contains("--update")
    val filter = args.find(a => !a.startsWith("--"))
    val dir = ReplBootstrap.env("EVAL_SCRIPTS_DIR").getOrElse("repl-js/test-resources/eval")

    ReplBootstrap.createSessionFromEnv() match
      case None =>
        println("error: set DOTTY_CLASSPATH_BIN and DOTTY_LINKER_LIBS_BIN")
        ReplBootstrap.setExitCode(1)
      case Some(sessionF) =>
        sessionF.flatMap(session => runAll(session, dir, update, filter)).onComplete {
          case scala.util.Success((passed, failed)) =>
            val verb = if update then "updated" else "passed"
            println(s"\n==== $passed $verb, $failed failed ====")
            if failed > 0 then ReplBootstrap.setExitCode(1)
          case scala.util.Failure(e) =>
            println(s"error: ${e}")
            ReplBootstrap.setExitCode(1)
        }

  private def runAll(session: ReplSession, dir: String, update: Boolean, filter: Option[String]): Future[(Int, Int)] =
    val files = listScripts(dir).filter(n => filter.forall(n.contains)).sorted
    if files.isEmpty then
      println(s"no scripts found in $dir" + filter.fold("")(f => s" matching '$f'"))
    seqFold(files, (0, 0)) { case ((ok, bad), name) =>
      runFile(session, dir + "/" + name, name, update).map(passed =>
        if passed then (ok + 1, bad) else (ok, bad + 1))
    }

  private def runFile(session: ReplSession, path: String, name: String, update: Boolean): Future[Boolean] =
    val turns = parse(readFile(path))
    session.reset().flatMap { _ =>
      seqFold(turns, List.empty[(Turn, String)]) { (acc, turn) =>
        JsonProtocol.respond(session, turn.request).map(r => acc :+ (turn, r._1))
      }.map { results =>
        if update then
          // Regenerate exact expectations; preserve hand-authored `~` regex
          // matchers (used for fuzzy diagnostics, mirroring the JVM suite).
          writeFile(path, render(results.map((t, actual) =>
            if isRegex(t.expected) then t else t.copy(expected = actual))))
          println(s"UPDATE $name (${results.size} turns)")
          true
        else
          val mismatches = results.filterNot((t, actual) => matches(t.expected, actual))
          if mismatches.isEmpty then
            println(s"PASS $name (${results.size} turns)")
            true
          else
            println(s"FAIL $name")
            mismatches.foreach { (t, actual) =>
              println(s"    request:  ${t.request}")
              println(s"    expected: ${t.expected}")
              println(s"    actual:   $actual")
            }
            false
      }
    }

  /** An expected line `~ <regex>` is a (Scala) regex the response must contain;
   *  anything else is matched verbatim. Regex matchers express the fuzzy
   *  diagnostics (cc / safe-mode) the way the JVM suite uses `assertContains`. */
  private def isRegex(expected: String): Boolean = expected.startsWith("~ ")
  private def matches(expected: String, actual: String): Boolean =
    if isRegex(expected) then
      try expected.stripPrefix("~ ").r.findFirstIn(actual).isDefined
      catch case _: Throwable => false // a malformed pattern is a test failure, not a crash
    else expected == actual

  // --- transcript format -----------------------------------------------------

  private def parse(content: String): List[Turn] =
    val turns = ListBuffer.empty[Turn]
    var comments = ListBuffer.empty[String]
    var pending: Option[(List[String], String)] = None
    def flush(expected: String): Unit =
      pending.foreach((cs, req) => turns += Turn(cs, req, expected))
      pending = None
    for raw <- content.split("\n", -1).toList do
      if raw.startsWith(RequestPrefix) then
        if pending.isDefined then flush("")
        pending = Some((comments.toList, raw.stripPrefix(RequestPrefix)))
        comments = ListBuffer.empty
      else if raw.startsWith("#") then
        comments += raw
      else if raw.trim.isEmpty then
        ()
      else if pending.isDefined then
        flush(raw)
    if pending.isDefined then flush("")
    turns.toList

  private def render(turns: List[Turn]): String =
    turns.map(t => (t.comments ++ List(RequestPrefix + t.request, t.expected)).mkString("\n"))
      .mkString("\n\n") + "\n"

  // --- node fs + async helpers -----------------------------------------------

  private def fs = js.Dynamic.global.require("fs")
  private def listScripts(dir: String): List[String] =
    try fs.readdirSync(dir).asInstanceOf[js.Array[String]].toList.filter(_.endsWith(".check"))
    catch case _: Throwable => Nil
  private def readFile(path: String): String =
    fs.readFileSync(path, "utf8").asInstanceOf[String]
  private def writeFile(path: String, content: String): Unit =
    fs.writeFileSync(path, content, "utf8")

  private def seqFold[A, B](xs: List[A], acc: B)(f: (B, A) => Future[B]): Future[B] =
    xs match
      case Nil     => Future.successful(acc)
      case h :: tl => f(acc, h).flatMap(acc2 => seqFold(tl, acc2)(f))
