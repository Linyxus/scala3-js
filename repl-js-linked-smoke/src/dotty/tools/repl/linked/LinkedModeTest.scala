package dotty.tools
package repl
package linked

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer

import dotty.tools.dotc.ClasspathBlob
import dotty.tools.io.VirtualDirectory
import dotty.tools.repl.ReplSession.EvalResponse

/** Stage B functional proof: drives a REAL [[ReplSession]] in `DOTTY_REPL_LINKED`
 *  (linked-image) mode through the battery below, and — for the lines that must be
 *  observationally identical to the interpreter — drives the SAME inputs through an
 *  interpreter session in the same process and diffs the rendered output + captured
 *  stdout verbatim.
 *
 *  Battery (a REPL session, one continuous linked session for e–i to prove survival):
 *   a. `val x = 41`                              → `val x: Int = 41`   (compare vs interp)
 *   b. `x + 1`                                   → `val res0: Int = 42`(cross-line state; compare)
 *   c. `case class P(a: Int); P(3) match …`      → compile + match     (compare)
 *   d. `println("hi")`                           → stdout `hi`         (compare stdout)
 *   e. a runtime exception                       → error output       (record both, diff)
 *   f. a structural/reflective selection         → clean restriction error (linked-only)
 *   g. a normal line after the breach            → works (post-breach survival)
 *   h. read an earlier binding after the breach  → state intact
 *   i. a dynamic `eval(...)`                      → clean unsupported error (linked-only)
 *
 *  Run: `sbt scala3-repl-linked-smoke-sjs/linkedModeTest`.
 */
object LinkedModeTest:

  private final case class Check(label: String, ok: Boolean, detail: String)

  private val log = scala.collection.mutable.ListBuffer.empty[String]
  private def note(s: String): Unit = log += s

  def main(unused: Array[String]): Unit =
    if !ReplBootstrap.hasProcess then return
    (ReplBootstrap.env("DOTTY_CLASSPATH_BIN"), ReplBootstrap.env("DOTTY_LINKER_LIBS_BIN")) match
      case (Some(cp), Some(lib)) =>
        run(cp, lib).onComplete {
          case scala.util.Success(checks) =>
            println("\n---- transcript ----")
            log.foreach(println)
            println("\n==== linked-mode functional test ====")
            checks.foreach(c => println(s"[${if c.ok then "PASS" else "FAIL"}] ${c.label}: ${c.detail}"))
            val failed = checks.count(!_.ok)
            println(s"==== ${checks.count(_.ok)} passed, $failed failed ====")
            if failed > 0 then ReplBootstrap.setExitCode(1)
          case scala.util.Failure(e) =>
            println(s"error: $e"); e.printStackTrace(); ReplBootstrap.setExitCode(1)
        }
      case _ =>
        println("error: set DOTTY_CLASSPATH_BIN and DOTTY_LINKER_LIBS_BIN")
        ReplBootstrap.setExitCode(1)

  private def run(cpPath: String, libPath: String): Future[List[Check]] =
    val cpDir  = ClasspathBlob.load(ReplBootstrap.readArrayBuffer(cpPath))
    val libBuf = ReplBootstrap.readArrayBuffer(libPath)

    // Interpreter session first (untimed), then the linked session (boot = image build).
    ReplSession.create(cpDir, libBuf, Nil, linked = false).flatMap { interp =>
      val tBoot = System.nanoTime()
      ReplSession.create(cpDir, libBuf, Nil, linked = true).flatMap { linked =>
        val bootMs = (System.nanoTime() - tBoot).toDouble / 1e6
        note(f"[boot] linked image built in $bootMs%.0f ms")

        val checks = scala.collection.mutable.ListBuffer.empty[Check]
        var linkedTimes = List.empty[Double]

        /** Eval on the linked session, timed. */
        def linkedEval(code: String): Future[EvalResponse] =
          val t0 = System.nanoTime()
          linked.eval(code).map { r =>
            val ms = (System.nanoTime() - t0).toDouble / 1e6
            linkedTimes = ms :: linkedTimes
            note(f"[linked ${ms}%6.0f ms] >>> ${oneLine(code)}")
            recordResp("linked", r)
            r
          }

        def interpEval(code: String): Future[EvalResponse] =
          interp.eval(code).map { r => note(s"[interp       ] >>> ${oneLine(code)}"); recordResp("interp", r); r }

        /** Run `code` on both, diff output+stdout+ok, add a compare Check. */
        def compare(label: String, code: String): Future[Unit] =
          for i <- interpEval(code); l <- linkedEval(code) yield
            val same = i.output == l.output && i.stdout == l.stdout && i.ok == l.ok
            checks += Check(label, same,
              if same then s"identical (ok=${l.ok} out=${esc(l.output)} stdout=${esc(l.stdout)})"
              else s"DIVERGED\n      interp: ok=${i.ok} out=${esc(i.output)} stdout=${esc(i.stdout)} err=${i.error}\n      linked: ok=${l.ok} out=${esc(l.output)} stdout=${esc(l.stdout)} err=${l.error}")

        // ---- a–d: must be identical to the interpreter --------------------
        compare("a/val-binding",   "val x = 41")
          .flatMap(_ => compare("b/cross-line-state", "x + 1"))
          .flatMap(_ => compare("c/case-class-match", "case class P(a: Int); P(3) match { case P(n) => n * 2 }"))
          .flatMap(_ => compare("d/println-stdout",  "println(\"hi\")"))
          // ---- e: runtime exception — record both verbatim, compare ------
          .flatMap { _ =>
            for i <- interpEval("throw new RuntimeException(\"boom\")")
                l <- linkedEval("throw new RuntimeException(\"boom\")")
            yield
              val same = i.output == l.output && i.ok == l.ok
              checks += Check("e/runtime-exception", l.ok == false && l.error.exists(_.contains("boom")),
                s"linked ok=${l.ok} error=${l.error} out=${esc(l.output)}" +
                  (if same then " (identical to interp)"
                   else s" | interp ok=${i.ok} error=${i.error} out=${esc(i.output)}"))
          }
          // ---- f: structural/reflective selection — clean restriction ----
          // Explicit structural type so `sv.qq` is a genuine reflective selection
          // (an inferred `val sv = new {…}` would widen to Object and not compile).
          .flatMap { _ =>
            val structCode = "import scala.language.reflectiveCalls\nval sv: { def qq: Int } = new { def qq: Int = 42 }\nsv.qq"
            linkedEval(structCode).map { l =>
              checks += Check("f/structural-restriction",
                l.ok == false && l.error.exists(_.toLowerCase.contains("linked")),
                s"ok=${l.ok} error=${l.error}")
            }
          }
          // ---- g: post-breach survival (a normal, independent line) ------
          .flatMap { _ =>
            linkedEval("1 + 2").map { l =>
              checks += Check("g/post-breach-survival",
                l.ok && l.output.contains("= 3"),
                s"ok=${l.ok} out=${esc(l.output)} err=${l.error}")
            }
          }
          // ---- h: earlier state still readable after the breach ----------
          .flatMap { _ =>
            linkedEval("x + 100").map { l =>
              checks += Check("h/state-intact-after-breach",
                l.ok && l.output.contains("= 141"),
                s"ok=${l.ok} out=${esc(l.output)} err=${l.error}")
            }
          }
          // ---- i: dynamic eval(...) — clean unsupported error ------------
          .flatMap { _ =>
            linkedEval("eval[Int](\"1 + 1\")").map { l =>
              checks += Check("i/dynamic-eval-unsupported",
                l.ok == false && l.error.exists(_.toLowerCase.contains("not supported")),
                s"ok=${l.ok} error=${l.error}")
            }
          }
          .flatMap { _ =>
            val times = linkedTimes.reverse
            note(f"[timing] linked per-line ms: ${times.map(t => f"$t%.0f").mkString(", ")}")
            if times.nonEmpty then
              note(f"[timing] linked median ${median(times)}%.0f ms  min ${times.min}%.0f  max ${times.max}%.0f")
            // t1–t4: cross-line member reuse, on their own fresh session pair.
            reuseBattery(cpDir, libBuf).map(reuse => checks.toList ++ reuse)
          }
      }
    }

  /** t1–t4 — cross-line member reuse: a `def`/class/object member defined on one line
   *  and FIRST referenced two lines later. Stage B froze only the wrapper's ctor +
   *  `replMain`, so the later first-reference grew the wrapper's already-frozen module
   *  → spurious restriction. Stage C's per-line member forcing must make each reuse
   *  byte-identical to the interpreter.
   *
   *  A fresh interp+linked session pair (independent of the a–i battery, whose f/i
   *  breaches skew the linked session's `res` counter vs the interpreter's) so a
   *  byte-identical `compare` on the reference line is meaningful. */
  private def reuseBattery(cpDir: VirtualDirectory, libBuf: ArrayBuffer): Future[List[Check]] =
    ReplSession.create(cpDir, libBuf, Nil, linked = false).flatMap { interp =>
      ReplSession.create(cpDir, libBuf, Nil, linked = true).flatMap { linked =>
        val checks = scala.collection.mutable.ListBuffer.empty[Check]

        /** Drive `code` on both sessions (no assertion) — the setup lines. */
        def drive(code: String): Future[Unit] =
          for i <- interp.eval(code); l <- linked.eval(code) yield
            note(s"[reuse >>> ] ${oneLine(code)}")
            recordResp("interp", i); recordResp("linked", l)

        /** Drive `code` on both, assert the linked output is OK and byte-identical to
         *  the interpreter — the cross-line reference line. */
        def reuse(label: String, code: String): Future[Unit] =
          for i <- interp.eval(code); l <- linked.eval(code) yield
            note(s"[reuse ref ] ${oneLine(code)}")
            recordResp("interp", i); recordResp("linked", l)
            val same = i.output == l.output && i.stdout == l.stdout && i.ok == l.ok
            checks += Check(label, same && l.ok,
              if same && l.ok then s"identical (ok=${l.ok} out=${esc(l.output)})"
              else s"DIVERGED\n      interp: ok=${i.ok} out=${esc(i.output)} err=${i.error}\n      linked: ok=${l.ok} out=${esc(l.output)} err=${l.error}")

        // t1: def defined, gap, first reference two lines later.
        drive("def f(n: Int) = n + 1")
          .flatMap(_ => drive("val g1 = 0"))
          .flatMap(_ => reuse("t1/cross-line-def-reuse", "f(41)"))
          // t2: class member first touched two lines later.
          .flatMap(_ => drive("class C { def m = 7 }"))
          .flatMap(_ => drive("val g2 = 0"))
          .flatMap(_ => reuse("t2/class-member-reuse", "new C().m"))
          // t3: def with a function-typed parameter.
          .flatMap(_ => drive("def gg(h: Int => Int) = h(1)"))
          .flatMap(_ => drive("val g3 = 0"))
          .flatMap(_ => reuse("t3/function-typed-param-reuse", "gg(x => x + 1)"))
          // t4: object member first touched two lines later.
          .flatMap(_ => drive("object O4 { def k = 9 }"))
          .flatMap(_ => drive("val g4 = 0"))
          .flatMap(_ => reuse("t4/object-member-reuse", "O4.k"))
          .map(_ => checks.toList)
      }
    }

  // --- helpers ---------------------------------------------------------------

  private def recordResp(tag: String, r: EvalResponse): Unit =
    if r.output.nonEmpty then r.output.stripSuffix("\n").linesIterator.foreach(l => note(s"    [$tag out] $l"))
    if r.stdout.nonEmpty then r.stdout.stripSuffix("\n").linesIterator.foreach(l => note(s"    [$tag stdout] $l"))
    if !r.ok then note(s"    [$tag error] ${r.error.getOrElse("")}")

  private def oneLine(s: String): String = s.replace("\n", " ⏎ ")
  private def esc(s: String): String = "\"" + s.replace("\n", "\\n") + "\""

  private def median(xs: List[Double]): Double =
    val s = xs.sorted; val n = s.length
    if n == 0 then 0.0 else if n % 2 == 1 then s(n / 2) else (s(n / 2 - 1) + s(n / 2)) / 2
