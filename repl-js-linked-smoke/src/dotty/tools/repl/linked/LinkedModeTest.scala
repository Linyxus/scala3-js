package dotty.tools
package repl
package linked

import java.nio.ByteBuffer

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer

import org.scalajs.ir.Serializers

import dotty.tools.dotc.ClasspathBlob
import dotty.tools.io.VirtualDirectory
import dotty.tools.repl.ReplSession.EvalResponse
import dotty.tools.repl.linked.LinkedImage.PhaseTiming
import dotty.tools.repl.linked.LinkedRunner.Diagnostics.{BootTiming, LineTiming}

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
            if profile.nonEmpty then
              println("\n==== relink latency profile ====")
              profile.foreach(println)
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
        // The FIRST linked session in the process links on a fresh linker: a true
        // COLD link. Capture its phase breakdown now, before later warm sessions
        // (reuse battery, measurement run) overwrite the process-global diagnostic.
        coldBoot = LinkedRunner.Diagnostics.lastBoot
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
            reuseBattery(cpDir, libBuf).flatMap { reuse =>
              // Relink-latency profile — a dedicated measurement session (its output
              // is diagnostics only; it adds no pass/fail checks). Failures here must
              // not fail the functional test, so recover to a note.
              measurementRun(cpDir, libBuf)
                .recover { case e => prof(s"[measurement error] $e") }
                .map(_ => checks.toList ++ reuse)
            }
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

  // --- relink-latency profile (measurement, diagnostics only) ----------------

  /** COLD link phase breakdown of the first linked session's boot (captured in
   *  `run` before later warm sessions overwrite the process-global diagnostic). */
  private var coldBoot: Option[BootTiming] = None
  private val profile = scala.collection.mutable.ListBuffer.empty[String]
  private def prof(s: String): Unit = profile += s

  /** A dedicated measurement session: run N ordinary additive lines on a FRESH
   *  linked session (its per-line relinks are the warm-relink sample), collect each
   *  line's timing + the linker's phase breakdown, and build the cold + warm phase
   *  tables, the wrapper-code rows, and the accounted-vs-wall attribution. Appends
   *  to `profile` (printed by `main` after the pass/fail summary); adds no checks. */
  private def measurementRun(cpDir: VirtualDirectory, libBuf: ArrayBuffer): Future[Unit] =
    // Reference datapoint: the cost of deserializing the whole stdlib IR once. The
    // linker deserializes each reachable ClassDef ~twice per relink (once for infos,
    // once to assemble), none of it cached while the IR is `Version.Unversioned`, so
    // this single pass bounds that re-deserialization tax.
    val entries = ClasspathBlob.loadEntries(libBuf)
    val tDes = System.nanoTime()
    val defs = entries.map { case (_, b) => Serializers.deserialize(ByteBuffer.wrap(b)) }
    val stdlibDeserializeMs = (System.nanoTime() - tDes).toDouble / 1e6
    val stdlibClasses = defs.size

    ReplSession.create(cpDir, libBuf, Nil, linked = true).flatMap { linked =>
      val n = 10
      def loop(i: Int, acc: List[LineTiming]): Future[List[LineTiming]] =
        if i >= n then Future.successful(acc.reverse)
        else
          val before = LinkedRunner.Diagnostics.lastLine
          linked.eval(s"val v$i = $i").flatMap { r =>
            val after = LinkedRunner.Diagnostics.lastLine
            // Only take a fresh, additive per-line timing (guards against a stale
            // read if a line ever failed to record).
            val got = if r.ok && after.ne(before) then after.toList else Nil
            loop(i + 1, got ::: acc)
          }
      loop(0, Nil).map(warm => buildReport(stdlibClasses, stdlibDeserializeMs, warm))
    }

  private def buildReport(stdlibClasses: Int, stdlibDeserializeMs: Double,
      warm: List[LineTiming]): Unit =
    prof("config: ESModule / SmallestModules / optimizer off / batchMode false / ES2018")
    prof(f"stdlib ClassDefs: $stdlibClasses%,d   full-stdlib deserialize (1 pass, reference): $stdlibDeserializeMs%.0f ms")
    prof("")

    coldBoot match
      case Some(b) =>
        prof(f"--- COLD link (first boot, fresh linker): link() wall ${b.linkMs}%.0f ms ---")
        phaseTableSingle(b.linkMs, b.phaseTimings).foreach(prof)
      case None =>
        prof("--- COLD link: not captured ---")
    prof("")

    if warm.nonEmpty then
      prof(f"--- WARM per-line relink phases (${warm.size} additive lines): median (min–max) ms ---")
      phaseTableWarm(warm).foreach(prof)
      prof("")
      prof(f"--- WARM per-line wrapper-code breakdown (${warm.size} lines): median (min–max) ms ---")
      wrapperTable(warm).foreach(prof)
    else
      prof("--- WARM: no additive lines captured ---")

  // Order phases by start time and compute nesting depth by interval containment (a
  // phase is nested in another iff its [start, end] lies strictly inside). Returns
  // (title, depth) in pipeline order. Each phase title occurs once per link.
  private def classify(sample: List[PhaseTiming]): List[(String, Int)] =
    final case class Iv(title: String, start: Long, end: Long)
    val ivs = sample.map(p => Iv(p.title, p.endNanos - p.nanos, p.endNanos))
    ivs.sortBy(_.start).map { iv =>
      val depth = ivs.count(o =>
        o.title != iv.title && o.start <= iv.start && o.end >= iv.end &&
          (o.start < iv.start || o.end > iv.end))
      (iv.title, depth)
    }

  private def indent(depth: Int): String = "  " * depth
  private def pct(part: Double, whole: Double): String =
    if whole <= 0 then "   -" else f"${100 * part / whole}%3.0f%%"

  /** Phase table for a single link (cold boot). */
  private def phaseTableSingle(linkMs: Double, sample: List[PhaseTiming]): List[String] =
    if sample.isEmpty then List("(no phase timings captured)")
    else
      val order = classify(sample)
      val byTitle = sample.map(p => p.title -> p.ms).toMap
      val topSum = order.filter(_._2 == 0).map((t, _) => byTitle(t)).sum
      val header = f"${"phase"}%-42s ${"ms"}%7s  %%link"
      val rows = order.map { (title, depth) =>
        val label = indent(depth) + title
        f"$label%-42s ${byTitle(title)}%7.0f  ${pct(byTitle(title), linkMs)}"
      }
      val foot = List(
        f"${"sum(top-level phases)"}%-42s ${topSum}%7.0f  ${pct(topSum, linkMs)}",
        f"${"link residue (IR read + report + extract)"}%-42s ${linkMs - topSum}%7.0f  ${pct(linkMs - topSum, linkMs)}")
      header :: rows ++ foot

  /** Aggregated phase table over the warm lines: median (min–max) per phase. */
  private def phaseTableWarm(warm: List[LineTiming]): List[String] =
    val sample = warm.map(_.phaseTimings).maxBy(_.size) // a complete representative line
    if sample.isEmpty then List("(no phase timings captured)")
    else
      val order = classify(sample)
      val perTitle = order.map(_._1).map { title =>
        title -> warm.flatMap(_.phaseTimings.filter(_.title == title).map(_.ms))
      }.toMap
      val linkMed = median(warm.map(_.linkMs))
      val topTitles = order.filter(_._2 == 0).map(_._1).toSet
      val topSums = warm.map(lt => lt.phaseTimings.filter(p => topTitles(p.title)).map(_.ms).sum)
      val residues = warm.map(lt => lt.linkMs - lt.phaseTimings.filter(p => topTitles(p.title)).map(_.ms).sum)
      val header = f"${"phase"}%-42s ${"med"}%7s ${"min"}%7s ${"max"}%7s  %%link"
      val rows = order.map { (title, depth) =>
        val xs = perTitle(title)
        val label = indent(depth) + title
        f"$label%-42s ${median(xs)}%7.0f ${xs.min}%7.0f ${xs.max}%7.0f  ${pct(median(xs), linkMed)}"
      }
      val foot = List(
        agg("sum(top-level phases)", topSums, linkMed),
        agg("link() wall (phases + extract)", warm.map(_.linkMs), linkMed),
        agg("link residue (IR read + report + extract)", residues, linkMed))
      header :: rows ++ foot

  /** Wrapper-code breakdown over the warm lines: median (min–max) per stage, with
   *  the accounted-vs-wall attribution (residue must stay small). */
  private def wrapperTable(warm: List[LineTiming]): List[String] =
    val totalMed = median(warm.map(_.totalMs))
    val accounted = warm.map(lt =>
      lt.deserializeMs + lt.synthesizeMs + lt.linkMs + lt.oracleMs + lt.writebackMs + lt.importMs)
    val residues = warm.map(lt => lt.totalMs -
      (lt.deserializeMs + lt.synthesizeMs + lt.linkMs + lt.oracleMs + lt.writebackMs + lt.importMs))
    val header = f"${"stage"}%-42s ${"med"}%7s ${"min"}%7s ${"max"}%7s  %%total"
    List(
      header,
      agg("deserialize (line .sjsir)", warm.map(_.deserializeMs), totalMed),
      agg("synthesize (trigger + lineForce)", warm.map(_.synthesizeMs), totalMed),
      agg("link (relink + extract)", warm.map(_.linkMs), totalMed),
      agg("oracle (byte-diff)", warm.map(_.oracleMs), totalMed),
      agg("writeback (diff-only fs)", warm.map(_.writebackMs), totalMed),
      agg("import + run", warm.map(_.importMs), totalMed),
      agg("accounted (sum of stages)", accounted, totalMed),
      agg("total wall", warm.map(_.totalMs), totalMed),
      agg("residue (unaccounted)", residues, totalMed))

  /** A median (min–max) row against a whole for the %% column. */
  private def agg(label: String, xs: List[Double], whole: Double): String =
    f"$label%-42s ${median(xs)}%7.0f ${xs.min}%7.0f ${xs.max}%7.0f  ${pct(median(xs), whole)}"

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
