package dotty.tools
package repl

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.util.{Success, Failure}

import dotty.tools.io.VirtualDirectory

/** Scripted-test driver for the human/CLI REPL — the Scala.js analogue of dotty's
 *  `dotty.tools.repl.ScriptedTests`. Each file in `REPL_SCRIPTS_DIR` (the shared
 *  `repl/test-resources/repl` transcripts) is replayed against a freshly-reset
 *  session via [[ScriptedRepl.reproduce]] (the same path `--script` uses) and the
 *  reproduced transcript is diffed against the file with `FileDiff` semantics
 *  (compare non-blank lines for exact equality).
 *
 *  Tests listed in `REPL_SCRIPTS_EXCLUDES` are platform-inherent skips (Scala.js
 *  runtime / interpreter divergences from the JVM); they're reported and skipped.
 *  Run via `sbt scala3-repl-cli-sjs/test`; pass a name substring as the first arg
 *  to run a subset. */
object ReplScriptedTests:

  def main(unused: Array[String]): Unit =
    if !ReplBootstrap.hasProcess then return
    val args = ReplBootstrap.args
    val filter = args.find(a => !a.startsWith("--"))
    val dir = ReplBootstrap.env("REPL_SCRIPTS_DIR").getOrElse("repl/test-resources/repl")
    val excludes = parseExcludes(ReplBootstrap.env("REPL_SCRIPTS_EXCLUDES").flatMap(ReplBootstrap.readFileOpt))

    ReplBootstrap.createRunnerFromEnv() match
      case None =>
        println("error: set DOTTY_CLASSPATH_BIN and DOTTY_LINKER_LIBS_BIN")
        ReplBootstrap.setExitCode(1)
      case Some((cpDir, runner, libLoaded)) =>
        libLoaded.flatMap(_ => runAll(cpDir, runner, dir, excludes, filter)).onComplete {
          case Success((passed, failed, skipped)) =>
            println(s"\n==== $passed passed, $failed failed, $skipped skipped (platform-inherent) ====")
            if failed > 0 then ReplBootstrap.setExitCode(1)
          case Failure(e) =>
            println(s"error: $e")
            ReplBootstrap.setExitCode(1)
        }

  private def runAll(cpDir: VirtualDirectory, runner: InterpreterRunner, dir: String,
                     excludes: Set[String], filter: Option[String]): Future[(Int, Int, Int)] =
    val files = ReplBootstrap.listFiles(dir).filter(n => filter.forall(n.contains)).sorted
    if files.isEmpty then println(s"no scripts found in $dir")
    seqFold(files, (0, 0, 0)) { case ((passed, failed, skipped), name) =>
      // A name filter overrides excludes, so a single excluded test can still be
      // run explicitly for debugging.
      if filter.isEmpty && excludes.contains(name) then
        println(s"SKIP $name")
        Future.successful((passed, failed, skipped + 1))
      else
        runFile(cpDir, runner, dir, name).map(ok =>
          if ok then (passed + 1, failed, skipped) else (passed, failed + 1, skipped))
    }

  private def runFile(cpDir: VirtualDirectory, runner: InterpreterRunner, dir: String, name: String): Future[Boolean] =
    // Fresh interpreter + session per transcript (the JVM runs each in isolation).
    runner.reset().flatMap { _ =>
      val sessionDir = new VirtualDirectory("(repl-session)", None)
      val fileLines = ReplBootstrap.readFileLines(dir + "/" + name)
      val buf = new StringBuilder
      // The reproduced transcript is the REPL's stdout: the driver's rendered
      // output (via the `emit` sink below) interleaved with the user code's own
      // stdout. Capture the latter (which goes to process.stdout / console.log)
      // into the same buffer, matching the bin harness's `>out 2>/dev/null`.
      withCapturedStdout(buf) {
        ScriptedRepl.reproduce(cpDir, sessionDir, runner, fileLines, s => { buf ++= s; () })
      }.map { _ =>
        val expected = nonBlank(fileLines)
        val actual = nonBlank(buf.toString.split("\n", -1).toList)
        if expected == actual then
          println(s"PASS $name (${expected.size} lines)")
          true
        else
          println(s"FAIL $name")
          printDiff(expected, actual)
          false
      }
    }

  /** Run `body` with the user code's own stdout redirected into `buf`.
   *
   *  A transcript's expected output interleaves the REPL's rendered output (which
   *  we already collect via the `emit` sink) with anything the *evaluated code*
   *  prints itself — that goes straight to `process.stdout` / `console.log` on the
   *  real process. We monkey-patch both to append to `buf` for the duration of the
   *  replay, then restore them (matching the bin harness's `>out 2>/dev/null`:
   *  stdout only, stderr discarded). */
  private def withCapturedStdout(buf: StringBuilder)(body: => Future[Unit]): Future[Unit] =
    val process = js.Dynamic.global.process
    val console = js.Dynamic.global.console
    val origWrite = process.stdout.write
    val origLog   = console.log
    val write: js.Function1[js.Any, Boolean] = (chunk: js.Any) => { buf ++= chunk.toString; true }
    val log: js.Function1[js.Any, Unit]      = (arg: js.Any)   => { buf ++= arg.toString; buf += '\n'; () }
    process.stdout.write = write
    console.log = log
    def restore(): Unit =
      process.stdout.write = origWrite
      console.log = origLog
    body.andThen { case _ => restore() }

  /** FileDiff semantics: keep only lines with a non-whitespace char. */
  private def nonBlank(lines: List[String]): List[String] =
    lines.filter(_.exists(c => !c.isWhitespace))

  private def printDiff(expected: List[String], actual: List[String]): Unit =
    val n = math.max(expected.length, actual.length)
    var i = 0
    while i < n do
      val e = if i < expected.length then Some(expected(i)) else None
      val a = if i < actual.length then Some(actual(i)) else None
      if e != a then
        e.foreach(l => println(s"    -$l"))
        a.foreach(l => println(s"    +$l"))
      i += 1

  /** Parse the excludes file: lines `name: reason`; `#`/blank ignored. */
  private def parseExcludes(content: Option[String]): Set[String] =
    content.getOrElse("").split("\n").iterator
      .map(_.trim)
      .filterNot(l => l.isEmpty || l.startsWith("#"))
      .flatMap(l => l.split(":", 2).headOption.map(_.trim))
      .filter(_.nonEmpty)
      .toSet

  private def seqFold[A, B](xs: List[A], acc: B)(f: (B, A) => Future[B]): Future[B] =
    xs match
      case Nil     => Future.successful(acc)
      case h :: tl => f(acc, h).flatMap(acc2 => seqFold(tl, acc2)(f))
