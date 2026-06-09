package dotty.tools
package repl

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.{Success, Failure}

/** Scripted-test driver for the human/CLI REPL — the Scala.js analogue of dotty's
 *  `dotty.tools.repl.ScriptedTests`. Each file in `REPL_SCRIPTS_DIR` (the shared
 *  `repl/test-resources/repl` transcripts) is replayed through the JSON REPL
 *  client via [[ScriptedRepl.reproduce]] (the same path `--script` uses) and the
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

    ReplBootstrap.createSessionFromEnv() match
      case None =>
        println("error: set DOTTY_CLASSPATH_BIN and DOTTY_LINKER_LIBS_BIN")
        ReplBootstrap.setExitCode(1)
      case Some(sessionF) =>
        sessionF.flatMap { session =>
          val client = new JsonReplClient(session)
          runAll(client, dir, excludes, filter).flatMap {
            case (passed, failed, skipped) =>
              runEvalErrorSmoke(client).map { ok =>
                val afterEvalSmoke =
                  if ok then (passed + 1, failed, skipped)
                  else (passed, failed + 1, skipped)
                val (passed1, failed1, skipped1) = afterEvalSmoke
                val afterLineEditorSmoke =
                  if runLineEditorSmoke() then (passed1 + 1, failed1, skipped1)
                  else (passed1, failed1 + 1, skipped1)
                val (passed2, failed2, skipped2) = afterLineEditorSmoke
                if runLineRenderingSmoke() then (passed2 + 1, failed2, skipped2)
                else (passed2, failed2 + 1, skipped2)
              }
          }
        }.onComplete {
          case Success((passed, failed, skipped)) =>
            println(s"\n==== $passed passed, $failed failed, $skipped skipped (platform-inherent) ====")
            if failed > 0 then ReplBootstrap.setExitCode(1)
          case Failure(e) =>
            println(s"error: $e")
            ReplBootstrap.setExitCode(1)
        }

  private def runAll(client: JsonReplClient, dir: String,
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
        runFile(client, dir, name).map(ok =>
          if ok then (passed + 1, failed, skipped) else (passed, failed + 1, skipped))
    }

  private def runFile(client: JsonReplClient, dir: String, name: String): Future[Boolean] =
    val fileLines = ReplBootstrap.readFileLines(dir + "/" + name)
    val buf = new StringBuilder
    ScriptedRepl.reproduce(client, fileLines, s => { buf ++= s; () }).map { _ =>
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

  private def runEvalErrorSmoke(client: JsonReplClient): Future[Boolean] =
    val buf = new StringBuilder
    ScriptedRepl.reproduce(client, List("""scala> eval("abc")"""), s => { buf ++= s; () }).map { _ =>
      val actual = buf.toString
      val ok = actual.contains("scala.runtime.eval.EvalCompileException") && actual.contains("Not found: abc")
      if ok then
        println("PASS eval-error-smoke")
        true
      else
        println("FAIL eval-error-smoke")
        println(actual)
        false
    }

  private def runLineEditorSmoke(): Boolean =
    def check(cond: Boolean, clue: String): Boolean =
      if cond then true
      else
        println(s"line-editor-smoke: $clue")
        false

    val editor = new ReplLineEditor(historyLimit = 10)

    editor.insert("val x =")
    editor.insertNewline()
    editor.insert("  1")
    val multilineOk = check(editor.accept() == "val x =\n  1", "inserted newline was not preserved")

    editor.insert("abc")
    editor.moveToLineStart()
    editor.insert("x")
    editor.moveToLineEnd()
    editor.insert("y")
    editor.moveToLineStart()
    editor.killToLineEnd()
    val ctrlOk = check(editor.buffer == "", "ctrl-style line editing produced the wrong buffer")

    editor.insert("first")
    editor.accept()
    editor.insert("second")
    editor.accept()
    editor.previousHistory()
    val hist1 = editor.buffer == "second"
    editor.previousHistory()
    val hist2 = editor.buffer == "first"
    editor.nextHistory()
    val hist3 = editor.buffer == "second"
    editor.nextHistory()
    val historyOk = check(hist1 && hist2 && hist3 && editor.buffer == "", "history navigation failed")

    val ok = multilineOk && ctrlOk && historyOk
    if ok then println("PASS line-editor-smoke")
    else println("FAIL line-editor-smoke")
    ok

  private def runLineRenderingSmoke(): Boolean =
    def check(cond: Boolean, clue: String): Boolean =
      if cond then true
      else
        println(s"line-rendering-smoke: $clue")
        false

    val rendered = TerminalLineRendering.render("scala> ", "val x =\n  1")
    val renderedOk = check(
      rendered == "scala> val x =\n   |  1",
      "continuation prompt was not rendered after newline",
    )

    val prefix = TerminalLineRendering.renderPrefix("scala> ", "val x =\n  1", "val x =\n".length)
    val prefixOk = check(
      prefix == "scala> val x =\n   |",
      "cursor prefix did not include continuation prompt",
    )

    val ok = renderedOk && prefixOk
    if ok then println("PASS line-rendering-smoke")
    else println("FAIL line-rendering-smoke")
    ok

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
