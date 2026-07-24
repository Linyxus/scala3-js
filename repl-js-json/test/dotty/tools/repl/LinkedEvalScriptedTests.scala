package dotty.tools
package repl

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import dotty.tools.dotc.ClasspathBlob
import EvalScriptedTests.{Turn, parse, matches, listScripts, readFile, seqFold}

/** Stage C task 2: replay the SAME 69 eval transcripts (and ExtraLibsTests) that
 *  [[EvalScriptedTests]] runs through the interpreter, but through the linked image
 *  ([[dotty.tools.repl.linked.LinkedRunner]]) instead. Reached via
 *  `EvalScriptedTests.main --linked`; the sbt task is
 *  `scala3-repl-json-sjs/linkedEvalScriptedTests`.
 *
 *  Linked mode is an additive fast path with genuine restrictions (RECON3): dynamic
 *  `eval(...)` / `embedRepl(...)` cannot be served (no synchronous relink + import),
 *  structural/reflective selection breaches, and a few stack-frame strings render
 *  differently. So a transcript that exercises those diverges from the interpreter's
 *  checkfile. Each such transcript is listed — with a category + reason — in
 *  `linked-eval-excludes.txt` (path via `LINKED_EVAL_EXCLUDES`), so the task is GREEN
 *  with documented skips and any UNEXPECTED divergence (or a stale exclude that now
 *  passes) is reported. A file NOT in the list must replay byte-identically.
 *
 *  The transcript session is built with `linked = true` directly (no reliance on
 *  `DOTTY_REPL_LINKED`); ExtraLibsTests self-constructs its sessions from the env, so
 *  the sbt task sets `DOTTY_REPL_LINKED=1` for that leg — reported separately. */
object LinkedEvalScriptedTests:

  private final case class Tally(identical: Int, skipped: Int, unexpected: Int, stalePass: Int)

  def main(unused: Array[String]): Unit =
    if !ReplBootstrap.hasProcess then return
    val dir = ReplBootstrap.env("EVAL_SCRIPTS_DIR").getOrElse("repl-js/test-resources/eval")
    val excludes = parseExcludes(ReplBootstrap.env("LINKED_EVAL_EXCLUDES").flatMap(ReplBootstrap.readFileOpt))
    (ReplBootstrap.env("DOTTY_CLASSPATH_BIN"), ReplBootstrap.env("DOTTY_LINKER_LIBS_BIN")) match
      case (Some(cp), Some(lib)) =>
        val cpDir = ClasspathBlob.load(ReplBootstrap.readArrayBuffer(cp))
        val libBuf = ReplBootstrap.readArrayBuffer(lib)
        ReplSession.create(cpDir, libBuf, Nil, linked = true)
          .flatMap(session => runAll(session, dir, excludes))
          .flatMap { tally =>
            println("\n---- ExtraLibsTests (linked mode) ----")
            ExtraLibsTests.run().map(ef => (tally, ef))
          }
          .onComplete {
            case scala.util.Success((tally, (extraPass, extraFail))) =>
              report(tally, extraPass, extraFail)
            case scala.util.Failure(e) =>
              println(s"error: $e"); e.printStackTrace(); ReplBootstrap.setExitCode(1)
          }
      case _ =>
        println("error: set DOTTY_CLASSPATH_BIN and DOTTY_LINKER_LIBS_BIN")
        ReplBootstrap.setExitCode(1)

  private def report(t: Tally, extraPass: Int, extraFail: Int): Unit =
    println("\n==== linked eval transcripts ====")
    println(s"identical:  ${t.identical}")
    println(s"skipped:    ${t.skipped} (documented linked-mode divergences)")
    println(s"unexpected: ${t.unexpected} (not in excludes — MUST be 0)")
    println(s"stale-pass: ${t.stalePass} (excluded but now identical — prune excludes)")
    println(s"==== ExtraLibsTests (linked): $extraPass passed, $extraFail failed ====")
    // GREEN iff every non-excluded transcript replays identically AND ExtraLibsTests
    // is clean. A stale exclude (now-passing) is surfaced but not fatal.
    if t.unexpected > 0 || extraFail > 0 then ReplBootstrap.setExitCode(1)

  private def runAll(session: ReplSession, dir: String, excludes: Map[String, String]): Future[Tally] =
    val files = listScripts(dir).sorted
    seqFold(files, Tally(0, 0, 0, 0)) { (acc, name) =>
      runFile(session, dir + "/" + name, name).map { firstMismatch =>
        val excluded = excludes.contains(name)
        (firstMismatch, excluded) match
          case (None, false) =>
            println(s"PASS  $name")
            acc.copy(identical = acc.identical + 1)
          case (None, true) =>
            println(s"STALE $name — excluded but now identical: ${excludes(name)}")
            acc.copy(stalePass = acc.stalePass + 1)
          case (Some(_), true) =>
            println(s"SKIP  $name — ${excludes(name)}")
            acc.copy(skipped = acc.skipped + 1)
          case (Some((t, actual)), false) =>
            println(s"UNEXPECTED $name")
            println(s"    request:  ${abbrev(t.request)}")
            println(s"    expected: ${abbrev(t.expected)}")
            println(s"    actual:   ${abbrev(actual)}")
            acc.copy(unexpected = acc.unexpected + 1)
      }
    }

  /** Reset the linked session, replay the transcript, and return the FIRST turn whose
   *  response diverges from its checkfile expectation (None ⇒ replayed identically).
   *  Short-circuits at the first mismatch — enough to classify, and it keeps the
   *  dynamic-eval transcripts (which diverge on their first eval line) cheap. */
  private def runFile(session: ReplSession, path: String, name: String): Future[Option[(Turn, String)]] =
    val turns = parse(readFile(path))
    session.reset().flatMap(_ => replay(session, turns))

  private def replay(session: ReplSession, turns: List[Turn]): Future[Option[(Turn, String)]] =
    turns match
      case Nil => Future.successful(None)
      case turn :: rest =>
        JsonProtocol.respond(session, turn.request).flatMap { case (actual, _) =>
          if matches(turn.expected, actual) then replay(session, rest)
          else Future.successful(Some((turn, actual)))
        }

  /** Parse `name: reason` lines (`#`/blank ignored) into name → reason. */
  private def parseExcludes(content: Option[String]): Map[String, String] =
    content.getOrElse("").split("\n").iterator
      .map(_.trim)
      .filterNot(l => l.isEmpty || l.startsWith("#"))
      .flatMap { l =>
        l.split(":", 2) match
          case Array(n, r) if n.trim.nonEmpty => Some(n.trim -> r.trim)
          case Array(n) if n.trim.nonEmpty    => Some(n.trim -> "")
          case _                              => None
      }
      .toMap

  private def abbrev(s: String): String =
    val one = s.replace("\n", "\\n")
    if one.length <= 300 then one else one.take(300) + s"… (+${one.length - 300} chars)"
