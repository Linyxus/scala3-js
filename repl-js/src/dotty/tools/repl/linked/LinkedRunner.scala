package dotty.tools
package repl
package linked

import java.nio.ByteBuffer

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.scalajs.js.Thenable.Implicits.*

import org.scalajs.ir.{ClassKind, Serializers, Version}
import org.scalajs.ir.Trees.ClassDef
import org.scalajs.ir.Names.ClassName
import org.scalajs.linker.interface.{IRFile, Linker, ModuleInitializer}
import org.scalajs.linker.standard.{MemIRFileImpl, StandardLinkerBackend}

import dotty.tools.dotc.ClasspathBlob

/** The opt-in linked-image REPL runner (`DOTTY_REPL_LINKED=1`).
 *
 *  Instead of interpreting each line's `.sjsir`, it runs the line as a real
 *  Scala.js ES module linked into a warm, member-complete image ([[LinkedImage]] +
 *  [[TouchRoot]]) and imported into *this* worker process. Per line:
 *
 *   1. deserialize the line's fresh `.sjsir` and synthesize a trigger module
 *      (`LoadModule(rs$line$N$).replMain()`, see [[LinkedImage.Synth.lineTrigger]]);
 *   2. relink `image ++ prior-line-IR ++ this-line-IR ++ trigger` and consult the
 *      byte-diff oracle ([[LinkedImage.tryAddEntries]]);
 *   3. **additive** ⇒ the diff-only writeback already landed the new modules on
 *      disk; `import()` the trigger and call its `run()` export, which forces the
 *      wrapper's init in-process (user code + `pushRender`s fill
 *      `globalThis.__replRenders`, `println`s hit `process.stdout`), exactly as the
 *      interpreter does — so value rendering + stdout capture are unchanged;
 *   4. **breach** (`changedPreviouslyEmitted.nonEmpty`) ⇒ nothing was written or
 *      imported; [[LinkedImage.restoreBaseline]] resets the warm linker, and the
 *      line fails with a clear [[LinkedRunner.LinkedRestriction]]. The driver marks
 *      the line invalid so later lines never reference it. The session survives.
 *
 *  The interpreter is not used at all in linked mode; dynamic `eval(...)` (which
 *  needs a *synchronous* relink+import the linked world cannot provide) is refused
 *  fast by the driver (see [[ReplRunner.supportsDynamicEval]]).
 *
 *  The heavy base (deserialized stdlib IR + touch root + a warm linker) is cached
 *  process-globally and keyed by archive identity, so `:reset` — or a fresh session
 *  over the same archives — costs one warm relink into a new session dir, not a
 *  cold rebuild.
 */
final class LinkedRunner extends ReplRunner:
  import LinkedRunner.*

  def isLinked: Boolean = true

  private var image: LinkedImage | Null = null
  private var lineCounter: Int = 0

  // --- boot / library / reset -----------------------------------------------

  def loadLibrary(buffer: ArrayBuffer, extras: List[Map[String, Array[Byte]]] = Nil): Future[Unit] =
    val base = baseFor(buffer, extras)
    LinkedImage.buildInto(base.linker, base.baseIR, base.inits, freshSessionDir()).map { case (img, _) =>
      image = img
      lineCounter = 0
    }

  def reset(): Future[Unit] =
    val base = globalBase match
      case null => throw new IllegalStateException("linked runner :reset before loadLibrary")
      case b    => b
    val prev = image
    LinkedImage.buildInto(base.linker, base.baseIR, base.inits, freshSessionDir()).map { case (img, _) =>
      image = img
      lineCounter = 0
      prev match { case null => (); case p => bestEffortRemoveDir(p.sessionDir) }
    }

  // --- per-line -------------------------------------------------------------

  def loadAndRun(newSjsir: Map[String, Array[Byte]], wrapperClassName: String): Future[Unit] =
    image match
      case null => Future.failed(new IllegalStateException("linked image not built"))
      case img =>
        lineCounter += 1
        val freshDefs = newSjsir.valuesIterator
          .map(b => Serializers.deserialize(ByteBuffer.wrap(b))).toList
        val wrapperCN = resolveWrapper(freshDefs, wrapperClassName)
        val (triggerFiles, moduleId, _) = LinkedImage.Synth.lineTrigger(lineCounter, wrapperCN, freshDefs)
        val freshIR = toIRFiles(newSjsir) ++ triggerFiles
        // A relink can fail outright (not just breach) when this line's member
        // forcing reaches a target the image cannot link — treat that like a breach:
        // restore the committed baseline and surface a clean restriction, never a
        // corrupt warm linker or a dead session.
        val relink = img.tryAddEntries(freshIR).recoverWith {
          case r: LinkedRestriction => Future.failed(r)
          case e: Throwable => img.restoreBaseline().transformWith(_ =>
            Future.failed(new LinkedRestriction(linkFailureMessage(e))))
        }
        relink.flatMap { res =>
          if res.isAdditive then importAndRun(img.sessionDir, moduleId)
          else img.restoreBaseline().map(_ => throw new LinkedRestriction(restrictionMessage(res)))
        }

  // Dynamic eval is refused by the driver before it reaches here.
  def registerEvalClasses(newSjsir: Map[String, Array[Byte]]): Unit = throw unsupportedEval
  def instantiateEval(expressionClassName: String, bindings: Any): Any = throw unsupportedEval

  // --- helpers --------------------------------------------------------------

  private def toIRFiles(m: Map[String, Array[Byte]]): Seq[IRFile] =
    m.map { case (path, bytes) => new MemIRFileImpl(path, Version.Unversioned, bytes): IRFile }.toSeq

  /** Find the wrapper's IR module class among the line's fresh `.sjsir`, so the
   *  trigger's `LoadModule` names it exactly (the Scala.js module-class encoding of
   *  `object rs$line$N` — a trailing `$` — is resolved from the bytes, not guessed).
   *  `wrapperClassName` is the driver's `rs$line$N` (the interpreter's mirror name). */
  private def resolveWrapper(freshDefs: Seq[ClassDef], wrapperClassName: String): ClassName =
    val moduleClasses = freshDefs.collect { case cd if cd.kind == ClassKind.ModuleClass => cd.name.name }
    moduleClasses
      .find(cn => cn.nameString == wrapperClassName || cn.nameString == wrapperClassName + "$")
      .getOrElse(ClassName(wrapperClassName + "$"))

  /** Import `<moduleId>.js` from the session dir and call its `run()` export,
   *  running the wrapper's init in-process. A runtime exception thrown by the
   *  wrapper propagates out of `run()` and fails the future (the driver renders it
   *  via the same path as the interpreter). */
  private def importAndRun(sessionDir: String, moduleId: String): Future[Unit] =
    val url = "file://" + sessionDir + "/" + moduleId + ".js"
    dynImport(url).map { ns =>
      ns.asInstanceOf[js.Dynamic].run()
      ()
    }

object LinkedRunner:

  /** A line whose linked module would breach byte-additivity: it links a construct
   *  the warm image cannot serve without changing already-frozen modules. */
  final class LinkedRestriction(msg: String) extends RuntimeException(msg)

  private def restrictionMessage(res: LinkedImage.RelinkResult): String =
    val mods = res.changedPreviouslyEmitted
    val shown = mods.take(5).mkString(", ") +
      (if mods.size > 5 then s", … (+${mods.size - 5} more)" else "")
    "this line uses a construct unsupported in linked REPL mode — e.g. structural/reflective " +
      "selection, or a JVM-only stdlib member absent from the linked image.\n" +
      s"diagnostic: it would change ${mods.size} already-linked module(s): $shown"

  private def linkFailureMessage(e: Throwable): String =
    val detail = Option(e.getMessage).map(_.linesIterator.next()).getOrElse(e.getClass.getName.nn)
    "this line uses a construct unsupported in linked REPL mode: linking its members " +
      s"into the image failed ($detail)"

  private def unsupportedEval: Nothing =
    throw new IllegalStateException("dynamic eval(...) is not supported in linked REPL mode")

  private val BootGlobal = "__linkedReplBoot"

  /** REPL-runtime methods every generated wrapper calls, force-frozen into the image
   *  even when `computeBad` rejects them (see [[TouchRoot.forceMethodsModule]]).
   *  `ReplRenderer.replStringOf` renders each bound value; `ReplRenderBridge.push`
   *  ships it to the driver's bridge. (owner class nameString, method simpleName.) */
  private val ReplRuntimeRoots: Set[(String, String)] = Set(
    "scala.runtime.ReplRenderer$" -> "replStringOf",
    "scala.runtime.ReplRenderBridge$" -> "push",
  )

  /** The reusable, process-global base: the deserialized stdlib IR + synthesized
   *  touch root + boot module, plus a warm linker. Cached so `:reset` / a new
   *  session over the same archives skips the cold deserialize + touch-root synth. */
  private final class Base(
      val key: String, val linker: Linker,
      val baseIR: Seq[IRFile], val inits: Seq[ModuleInitializer])

  private var globalBase: Base | Null = null

  private def baseFor(buffer: ArrayBuffer, extras: List[Map[String, Array[Byte]]]): Base =
    val key = archiveKey(buffer, extras)
    globalBase match
      case b: Base if b.key == key => b
      case _ =>
        val nb = buildBase(key, buffer, extras)
        globalBase = nb
        nb

  private def archiveKey(buffer: ArrayBuffer, extras: List[Map[String, Array[Byte]]]): String =
    // Content- and order-sensitive. Two extra-lib sets that differ only in classpath
    // ORDER (or in the bytes of a same-named class) must NOT share a cached base, or
    // the second session reuses the first's image and resolves shadowing the wrong
    // way — the ExtraLibsTests shadow-order case, where [dup1,dup2] and [dup2,dup1]
    // collided on a size-only key. Hash each lib's sorted (path, length, content
    // hash), joined in classpath order.
    val extrasSig = extras.iterator.map { m =>
      m.toList.sortBy(_._1).iterator
        .map((p, b) => s"$p#${b.length}#${java.util.Arrays.hashCode(b)}").mkString(",")
    }.mkString("|")
    s"${buffer.byteLength}|$extrasSig"

  /** Keep the first entry for each `.sjsir` path (which is 1:1 with its class name),
   *  dropping later duplicates — first-on-classpath wins. */
  private def dedupFirstWins(entries: List[(String, Array[Byte])]): List[(String, Array[Byte])] =
    val seen = scala.collection.mutable.HashSet.empty[String]
    entries.filter { case (path, _) => seen.add(path) }

  private def buildBase(key: String, buffer: ArrayBuffer, extras: List[Map[String, Array[Byte]]]): Base =
    // Consume the fork's IR exactly as InterpreterRunner does: stdlib linker-libs +
    // each extra library's .sjsir. MemIRFileImpl for the linker, deserialized
    // ClassDefs for the touch root (over stdlib *and* extras, so extra-library
    // members are member-covered too — RECON3 Q7.5).
    //
    // First-on-classpath wins (RECON3 Q3; matches the interpreter's by-class-name
    // dedup on load order): dedup duplicate `.sjsir` paths keeping the FIRST, so a
    // shadowed extra-lib class never reaches the linker. Without this, two libs
    // defining the same class hand the linker two IR files for it, which it resolves
    // by its own rule (last wins), diverging from the interpreter's first-wins — the
    // ExtraLibsTests shadow-order case.
    val entries: List[(String, Array[Byte])] =
      dedupFirstWins(ClasspathBlob.loadEntries(buffer) ::: extras.flatMap(_.toList))
    val libIR: Seq[IRFile] =
      entries.map { case (path, bytes) => new MemIRFileImpl(path, Version.Unversioned, bytes): IRFile }
    val baseDefs: Seq[ClassDef] =
      entries.map { case (_, bytes) => Serializers.deserialize(ByteBuffer.wrap(bytes)) }
    val present: Set[ClassName] = baseDefs.iterator.map(_.name.name).toSet
    val coreSpec = StandardLinkerBackend(LinkedImage.config).coreSpec
    val built = TouchRoot.build(baseDefs, present, coreSpec, Exclusions.default)
    // The REPL's own generated wrappers always call these runtime hooks; force them
    // into the image even though computeBad (falsely) rejects some — otherwise every
    // rendered binding would breach on the shared ReplRenderer module.
    val preamble = TouchRoot.forceMethodsModule(baseDefs, ReplRuntimeRoots)
    val (bootIR, bootInit) = LinkedImage.Synth.bootModule("boot.Main$", BootGlobal, 1)
    new Base(key, LinkedImage.newLinker(), libIR ++ built.files ++ preamble ++ bootIR, Seq(bootInit))

  // --- Node fs / dynamic import ---------------------------------------------

  private def freshSessionDir(): String =
    val fs = js.Dynamic.global.require("fs")
    val os = js.Dynamic.global.require("os")
    fs.mkdtempSync(os.tmpdir().asInstanceOf[String] + "/linked-repl-").asInstanceOf[String]

  private def bestEffortRemoveDir(dir: String): Unit =
    try js.Dynamic.global.require("fs").rmSync(dir, js.Dynamic.literal(recursive = true, force = true))
    catch case _: Throwable => ()

  /** A runtime dynamic `import(url)` function. `import()` is not expressible as a
   *  plain call in compiled Scala.js for a runtime-computed specifier, so obtain
   *  one via indirect `eval` (available in the worker's Node context). An absolute
   *  `file://` URL is used so resolution is base-independent. */
  private val dynImport: js.Function1[String, js.Promise[js.Any]] =
    js.Dynamic.global.eval("(u => import(u))").asInstanceOf[js.Function1[String, js.Promise[js.Any]]]
