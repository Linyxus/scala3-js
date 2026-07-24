package dotty.tools
package repl
package linked

import java.nio.ByteBuffer

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer

import org.scalajs.ir.{ClassKind, Serializers, Version}
import org.scalajs.ir.Trees.{ClassDef, MethodDef, MemberNamespace}
import org.scalajs.ir.Types.Type
import org.scalajs.ir.Names.{ClassName, MethodName}
import org.scalajs.linker.interface.IRFile
import org.scalajs.linker.standard.{MemIRFileImpl, StandardLinkerBackend}

import dotty.tools.dotc.ClasspathBlob

/** A self-contained smoke test proving the [[TouchRoot]] + [[LinkedImage]] pipeline
 *  inside the fork's artifact world — the first measurement of the Scala.js linker
 *  hosting a member-complete touch-all image.
 *
 *  Runs under Node against the REAL packed `linker-libs.bin` (path in the env var
 *  `DOTTY_LINKER_LIBS_BIN`, exactly as the worker tests are launched):
 *
 *   a. build the touch-all image; report module count, total bytes, the largest
 *      (java.lang.Object) module, and the JS-hosted link wall time;
 *   b. relink the SAME input twice → assert zero changed and zero new files
 *      (determinism);
 *   c. synthesize a fake "entry" (a module top-level-exported under its own moduleID
 *      whose init mutates a global marker) → relink → assert zero changed
 *      previously-emitted modules and the entry's new module appears;
 *   d. boot the written image under `node` (import main.js → boot marker) and import
 *      the fake entry's module (its marker runs) — the warm-process mechanism;
 *   e. negative control: an entry referencing a member deliberately EXCLUDED from the
 *      touch root → relink → assert the oracle REPORTS changed shared modules.
 *
 *  Run: `sbt scala3-repl-linked-smoke-sjs/linkedImageSmoke`.
 */
object LinkedImageSmoke:

  private val BootGlobal = "__linkedImageBoot"
  private val BootValue = 1
  private val EntryAGlobal = "__smokeEntryA"
  private val EntryAValue = 4242

  private final case class Check(label: String, ok: Boolean, detail: String)

  def main(unused: Array[String]): Unit =
    if !ReplBootstrap.hasProcess then return
    ReplBootstrap.env("DOTTY_LINKER_LIBS_BIN") match
      case None =>
        println("error: set DOTTY_LINKER_LIBS_BIN")
        ReplBootstrap.setExitCode(1)
      case Some(libPath) =>
        run(libPath).onComplete {
          case scala.util.Success(checks) =>
            println("\n==== linked-image smoke ====")
            checks.foreach(c => println(s"[${if c.ok then "PASS" else "FAIL"}] ${c.label}: ${c.detail}"))
            val failed = checks.count(!_.ok)
            println(s"==== ${checks.count(_.ok)} passed, $failed failed ====")
            if failed > 0 then ReplBootstrap.setExitCode(1)
          case scala.util.Failure(e) =>
            println(s"error: $e")
            e.printStackTrace()
            ReplBootstrap.setExitCode(1)
        }

  private def run(libPath: String): Future[List[Check]] =
    val fs = js.Dynamic.global.require("fs")
    val os = js.Dynamic.global.require("os")
    val sessionDir = fs.mkdtempSync(os.tmpdir().asInstanceOf[String] + "/linked-smoke-").asInstanceOf[String]
    println(s"[smoke] session dir: $sessionDir")

    // Consume the fork's IR exactly as InterpreterRunner does: linker-libs.bin →
    // path/bytes; MemIRFileImpl for the linker, deserialized ClassDefs for the root.
    val entries = ClasspathBlob.loadEntries(ReplBootstrap.readArrayBuffer(libPath))
    val stdlibIR: Seq[IRFile] = entries.map { case (path, bytes) =>
      new MemIRFileImpl(path, Version.Unversioned, bytes): IRFile
    }
    val baseDefs: Seq[ClassDef] = entries.map { case (_, bytes) =>
      Serializers.deserialize(ByteBuffer.wrap(bytes))
    }
    val present: Set[ClassName] = baseDefs.iterator.map(_.name.name).toSet
    println(s"[smoke] loaded ${baseDefs.size} ClassDefs; ${Exclusions.default.size} exclusions")

    val coreSpec = StandardLinkerBackend(LinkedImage.config).coreSpec
    val tBuilt0 = System.nanoTime()
    val built = TouchRoot.build(baseDefs, present, coreSpec, Exclusions.default)
    val builtMs = (System.nanoTime() - tBuilt0).toDouble / 1e6
    val st = built.stats
    println(f"[smoke] touch root built in $builtMs%.0f ms: " +
      s"synthClasses=${st.synthClasses} exports=${st.exports} touchStmts=${st.touchStmts} " +
      s"touchedClasses=${st.touchedClasses} modules=${st.modulesLoaded} ctorNew=${st.ctorsNew} " +
      s"concreteMethods=${st.concreteMethods} staticMethods=${st.staticMethods} " +
      s"placeholders=${st.placeholders} placeholderSkipped=${st.placeholderSkipped} " +
      s"abstractTargets=${st.abstractTargets} argSkipped=${st.argSkipped} " +
      s"bad=${built.bad.size} badModules=${built.badModules.size} structBad=${built.structBad.size}")

    val (bootIR, bootInit) = LinkedImage.Synth.bootModule("boot.Main$", BootGlobal, BootValue)
    val baseIR = stdlibIR ++ built.files ++ bootIR

    // (a) build the image
    LinkedImage.buildImage(baseIR, Seq(bootInit), sessionDir).flatMap { case (image, r0) =>
      val largest = image.moduleNames
        .map(n => (n, image.content(n).map(_.length).getOrElse(0)))
        .sortBy(-_._2).take(5)
      val objMod = image.moduleNames.find(_.contains("java.lang.-Object"))
        .orElse(largest.headOption.map(_._1))
      val objBytes = objMod.flatMap(image.content).map(_.length).getOrElse(0)
      val checkA = Check("a/build-image",
        image.moduleCount > 100 && image.totalBytes > 1000000L && r0.linkMs > 0,
        f"modules=${image.moduleCount} totalBytes=${image.totalBytes}%,d (${image.totalBytes / 1048576.0}%.1f MB) " +
          f"objectModule=${objMod.getOrElse("?")}=${objBytes}%,d B coldLinkMs=${r0.linkMs}%.0f " +
          s"largest=${largest.map((n, b) => s"$n:$b").mkString(",")}")

      // (b) determinism: relink same input twice → 0 changed, 0 new
      image.relinkUnchanged().flatMap { rDet =>
        val checkB = Check("b/determinism",
          rDet.newModules.isEmpty && rDet.changedPreviouslyEmitted.isEmpty && rDet.removed.isEmpty,
          f"warmRelinkMs=${rDet.linkMs}%.0f new=${rDet.newModules.size} " +
            s"changed=${rDet.changedPreviouslyEmitted.size} removed=${rDet.removed.size}")

        // (c) fake entry: marker module, must be byte-additive
        val entryA = LinkedImage.Synth.markerModule("smoke.EntryA$", "entryA", "EntryA", EntryAGlobal, EntryAValue)
        image.addEntries(entryA).flatMap { rA =>
          val hasEntryA = rA.newModules.contains("entryA.js")
          val checkC = Check("c/additive-entry",
            rA.changedPreviouslyEmitted.isEmpty && hasEntryA,
            f"warmRelinkMs=${rA.linkMs}%.0f new=${rA.newModules.size} (entryA.js=$hasEntryA) " +
              s"changed=${rA.changedPreviouslyEmitted.size}")

          // (d) boot the warm process, then import the entry
          val bootOut = runBoot(sessionDir, "entryA.js")
          val bootVal = parseMarker(bootOut, "BOOT")
          val entryVal = parseMarker(bootOut, "ENTRYA")
          val checkD = Check("d/warm-boot",
            bootVal.contains(BootValue.toString) && entryVal.contains(EntryAValue.toString),
            s"boot=${bootVal.getOrElse("?")} entryA=${entryVal.getOrElse("?")}" +
              (if bootVal.isEmpty then s" raw=${bootOut.take(400)}" else ""))

          // (e) negative control: reference an excluded-but-linkable member
          findExcludedLinkableMethod(baseDefs, Exclusions.default, built.bad, built.structBad) match
            case None =>
              Future.successful(List(checkA, checkB, checkC, checkD,
                Check("e/oracle-breach", ok = false, "no excluded linkable method found to reference")))
            case Some((cn, mn, rt)) =>
              val entryB = LinkedImage.Synth.forceMethodModule(
                "smoke.EntryB$", "entryB", "breachB", cn, mn, rt)
              image.addEntries(entryB).map { rB =>
                val checkE = Check("e/oracle-breach",
                  rB.changedPreviouslyEmitted.nonEmpty,
                  s"referenced ${cn.nameString}.${mn.simpleName.nameString} → " +
                    s"changed=${rB.changedPreviouslyEmitted.size} new=${rB.newModules.size} " +
                    s"e.g. ${rB.changedPreviouslyEmitted.take(4).mkString(",")}")
                List(checkA, checkB, checkC, checkD, checkE)
              }
        }
      }
    }

  /** Boot main.js (runs the boot initializer) then import `entryModule` in one warm
   *  Node process; return its stdout. */
  private def runBoot(sessionDir: String, entryModule: String): String =
    val fs = js.Dynamic.global.require("fs")
    val cp = js.Dynamic.global.require("child_process")
    val script = sessionDir + "/__boot.mjs"
    fs.writeFileSync(script, BootMjs)
    try
      cp.execFileSync("node", js.Array(script, entryModule),
        js.Dynamic.literal(encoding = "utf8")).asInstanceOf[String]
    catch
      case e: Throwable =>
        val stdout = try e.asInstanceOf[js.Dynamic].stdout.asInstanceOf[String] catch case _: Throwable => ""
        s"BOOTERR=$e\n$stdout"

  private val BootMjs: String =
    """const dir = import.meta.dirname;
      |const f = (n) => "file://" + dir + "/" + n;
      |try {
      |  await import(f("main.js"));
      |  console.log("BOOT=" + globalThis.__linkedImageBoot);
      |  const target = process.argv[2];
      |  if (target) { await import(f(target)); }
      |  console.log("ENTRYA=" + globalThis.__smokeEntryA);
      |} catch (e) {
      |  console.log("BOOTERR=" + (e && e.stack ? e.stack : String(e)));
      |}
      |""".stripMargin

  private def parseMarker(out: String, key: String): Option[String] =
    out.linesIterator.collectFirst {
      case l if l.startsWith(s"$key=") => l.stripPrefix(s"$key=").trim
    }

  /** Find a zero-arg public method the touch root deliberately did NOT force
   *  (excluded) but which is linkable (`!bad`), on a class the root DID emit — so
   *  referencing it re-emits that class's already-written module (an oracle breach).*/
  private def findExcludedLinkableMethod(baseDefs: Seq[ClassDef], excl: Set[(String, String)],
      bad: Set[(ClassName, MethodName, Int)], structBad: Set[ClassName]): Option[(ClassName, MethodName, Type)] =
    val pubOrd = MemberNamespace.Public.ordinal
    val prefixExcls = excl.collect { case (p, "**") => p }.toList
    def classExcluded(s: String): Boolean =
      excl.contains((s, "*")) || prefixExcls.exists(s.startsWith)
    def classLike(cd: ClassDef): Boolean =
      cd.kind == ClassKind.Class || cd.kind == ClassKind.ModuleClass || cd.kind == ClassKind.Interface
    baseDefs.sortBy(_.name.name.nameString).iterator.flatMap { cd =>
      val cn = cd.name.name
      val cnStr = cn.nameString
      if !classLike(cd) || structBad.contains(cn) || classExcluded(cnStr) then Iterator.empty
      else cd.methods.iterator.filter { m =>
        m.flags.namespace == MemberNamespace.Public && m.body.isDefined && m.args.isEmpty &&
          TouchRoot.isMethodExcluded(excl, cnStr, m.name.name.simpleName.nameString) &&
          !bad.contains((cn, m.name.name, pubOrd))
      }.map(m => (cn, m.name.name, m.resultType))
    }.nextOption()
