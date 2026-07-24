package dotty.tools
package repl
package linked

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.scalajs.js.typedarray.byteArray2Int8Array

import org.scalajs.ir.Trees.*
import org.scalajs.ir.Types.*
import org.scalajs.ir.Names.*
import org.scalajs.ir.{ClassKind, Position, Version}
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.WellKnownNames.{ObjectClass, NoArgConstructorName}

import org.scalajs.linker.StandardImpl
import org.scalajs.linker.MemOutputDirectory
import org.scalajs.linker.interface.{IRFile, ModuleInitializer, StandardConfig, ModuleKind, ModuleSplitStyle, ESVersion, Linker}
import org.scalajs.logging.{Level, Logger, ScalaConsoleLogger}

/** A warm, member-complete linked image and its incremental relinker.
 *
 *  The image is a full `SmallestModules` ES-module link of the stdlib IR + any
 *  extra-library IR + the synthesized touch root ([[TouchRoot]]) + a tiny boot
 *  module, done once into an in-memory [[MemOutputDirectory]]. That snapshot is the
 *  byte-diff oracle store: each relink ([[addEntries]]) compares every
 *  previously-emitted module against the last snapshot, so a change to any already-
 *  emitted module (`changedPreviouslyEmitted`) is a detectable additivity breach —
 *  the invariant the spike proved a perfect predictor of warm-process success.
 *
 *  New/changed modules are written back to a session directory on disk (Node fs):
 *  Node's ESM cache needs real files for `import()`, and the per-class modules import
 *  each other by relative path, so `data:` URLs are not an option. Only new and
 *  changed files are written (diff-only), so a byte-additive relink touches only the
 *  new entry's files. The full link is kept in RAM ([[content]]) regardless.
 *
 *  Shaped for stage B: [[LinkedImage.buildImage]] builds the image once and
 *  [[addEntries]] folds a compiled line's `.sjsir` into the warm image, surfacing the
 *  oracle result. Stage A only exercises this via the smoke test; the default REPL
 *  path (the interpreter) is untouched.
 */
final class LinkedImage private[linked] (
    linker: Linker,
    initializers: Seq[ModuleInitializer],
    baseIR: Seq[IRFile],
    val sessionDir: String,
    private var entriesIR: Seq[IRFile],
    private var snap: Map[String, Array[Byte]]):

  /** Every module currently in the image, sorted. */
  def moduleNames: List[String] = snap.keys.toList.sorted
  def moduleCount: Int = snap.size
  def content(name: String): Option[Array[Byte]] = snap.get(name)
  def totalBytes: Long = snap.valuesIterator.map(_.length.toLong).sum

  /** Relink `base ++ accumulated-entries ++ newEntryIR`, diff against the last
   *  snapshot, write only new/changed files back to [[sessionDir]], and return the
   *  oracle result. The entries accumulate: each call sees all prior entries too.
   *
   *  Unconditional: commits and writes back even on a breach. Stage B's per-line
   *  path uses [[tryAddEntries]] instead, which refuses to commit a breach. */
  def addEntries(newEntryIR: Seq[IRFile]): Future[LinkedImage.RelinkResult] =
    val nextEntries = entriesIR ++ newEntryIR
    LinkedImage.linkOnce(linker, baseIR ++ nextEntries, initializers).map { case (next, ms, timings) =>
      val res = LinkedImage.diffAndWriteback(sessionDir, snap, next, ms, timings)
      entriesIR = nextEntries
      snap = next
      res
    }

  /** The current committed entry IR (the wrapper + trigger IR of every line the
   *  image has accepted so far). */
  def committedEntries: Seq[IRFile] = entriesIR

  /** Relink `base ++ entries ++ newEntryIR` and diff against the last snapshot,
   *  but **commit only if the relink is additive** (no previously-emitted module
   *  changed or removed). On an additive relink the new/changed files are written
   *  back to [[sessionDir]] and the image advances; on a breach nothing is written
   *  and the image is left exactly at its prior committed baseline (the caller
   *  should [[restoreBaseline]] to reset the warm linker's incremental state, then
   *  surface the breach to the user). */
  def tryAddEntries(newEntryIR: Seq[IRFile]): Future[LinkedImage.RelinkResult] =
    val nextEntries = entriesIR ++ newEntryIR
    LinkedImage.linkOnce(linker, baseIR ++ nextEntries, initializers).map { case (next, ms, timings) =>
      val tOracle = System.nanoTime()
      val res = LinkedImage.diff(snap, next, ms, timings)
      val oracleMs = (System.nanoTime() - tOracle).toDouble / 1e6
      var writebackMs = 0.0
      if res.isAdditive then
        val tWrite = System.nanoTime()
        LinkedImage.writeback(sessionDir, snap, next)
        writebackMs = (System.nanoTime() - tWrite).toDouble / 1e6
        entriesIR = nextEntries
        snap = next
      res.copy(oracleMs = oracleMs, writebackMs = writebackMs)
    }

  /** Relink the current committed baseline (`base ++ entries`), resetting the warm
   *  linker's incremental state and re-asserting the on-disk image. Used after a
   *  [[tryAddEntries]] breach: determinism guarantees this reproduces the committed
   *  snapshot byte-for-byte (so a correct generator yields zero changed files). */
  def restoreBaseline(): Future[LinkedImage.RelinkResult] =
    LinkedImage.linkOnce(linker, baseIR ++ entriesIR, initializers).map { case (next, ms, timings) =>
      val res = LinkedImage.diff(snap, next, ms, timings)
      // Defensive: rewrite any drift (there should be none) and re-anchor snap.
      if (res.newModules.nonEmpty || res.changedPreviouslyEmitted.nonEmpty || res.removed.nonEmpty) then
        LinkedImage.writeback(sessionDir, snap, next)
        snap = next
      res
    }

  /** Relink the identical input again (determinism probe): a correct generator
   *  yields zero new and zero changed files. */
  def relinkUnchanged(): Future[LinkedImage.RelinkResult] = addEntries(Nil)

object LinkedImage:

  /** The oracle result of one (re)link. `changedPreviouslyEmitted` non-empty ⟺ an
   *  additivity breach; stage B surfaces that as a user error.
   *
   *  Carries the per-link timing breakdown (permanent relink-latency debugging
   *  infrastructure, purely observational): `linkMs` is the whole `link()` wall
   *  (link call + reading the modules out of the in-memory output dir); `oracleMs`
   *  the byte-diff pass; `writebackMs` the diff-only disk writeback; `phaseTimings`
   *  the linker's own per-phase breakdown (see [[PhaseTiming]] / [[CapturingLogger]]). */
  final case class RelinkResult(
      newModules: List[String],
      changedPreviouslyEmitted: List[String],
      removed: List[String],
      linkMs: Double,
      oracleMs: Double = 0.0,
      writebackMs: Double = 0.0,
      phaseTimings: List[PhaseTiming] = Nil):
    def isAdditive: Boolean = changedPreviouslyEmitted.isEmpty && removed.isEmpty

  /** One `logger.time(title, nanos)` event the linker reported during a link, plus
   *  the wall clock ([[System.nanoTime]]) it fired at. Because the linker's phases
   *  nest and each `time` fires when its block completes, `endNanos` lets the phase
   *  tree be reconstructed by interval containment: phase A is nested in phase B iff
   *  A's `[endNanos - nanos, endNanos]` interval lies inside B's. */
  final case class PhaseTiming(title: String, nanos: Long, endNanos: Long):
    def ms: Double = nanos.toDouble / 1e6

  /** A [[Logger]] that records every per-phase `time(title, nanos)` the linker
   *  reports while still forwarding real log messages to `delegate`. A fresh one is
   *  used per `link()` call ([[linkOnce]]) so its [[timings]] are that link's phase
   *  breakdown. Overriding `time` (a default `Logger` method that otherwise just
   *  `debug`s the duration) is the sanctioned way to observe the phases. */
  private final class CapturingLogger(delegate: Logger) extends Logger:
    private val buf = scala.collection.mutable.ListBuffer.empty[PhaseTiming]
    def log(level: Level, message: => String): Unit = delegate.log(level, message)
    def trace(t: => Throwable): Unit = delegate.trace(t)
    override def time(title: String, nanos: Long): Unit =
      buf += PhaseTiming(title, nanos, System.nanoTime())
    def timings: List[PhaseTiming] = buf.toList

  /** The pinned linker configuration: per-class ES modules, optimizer off, warm
   *  (non-batch) relinks, no source maps, ES2018 (pinned repo-wide). Mirrors
   *  `BrowserLinker` but for `ESModule` + `SmallestModules`. */
  val config: StandardConfig = StandardConfig()
    .withModuleKind(ModuleKind.ESModule)
    .withModuleSplitStyle(ModuleSplitStyle.SmallestModules)
    .withOptimizer(false)
    .withBatchMode(false)
    .withSourceMap(false)
    .withESFeatures(_.withESVersion(ESVersion.ES2018))

  private val baseLogger: Logger = new ScalaConsoleLogger(Level.Error)

  /** A fresh `StandardImpl.linker(config)` — a warm, incremental (non-batch)
   *  linker. Kept process-global by the linked runner so `:reset` reuses one warm
   *  linker (one ~2 s relink) instead of a full cold rebuild. */
  def newLinker(): Linker = StandardImpl.linker(config)

  /** Build the image once from `baseIR` (stdlib + extra + touch root + boot module)
   *  and `initializers` (the boot initializer), snapshotting into `sessionDir`,
   *  with a fresh linker. */
  def buildImage(baseIR: Seq[IRFile], initializers: Seq[ModuleInitializer],
      sessionDir: String): Future[(LinkedImage, RelinkResult)] =
    buildInto(newLinker(), baseIR, initializers, sessionDir)

  /** Like [[buildImage]] but links with a caller-supplied (possibly warm) `linker`,
   *  so `:reset` can reuse one process-global linker across sessions: relinking the
   *  base into a fresh `sessionDir` costs one warm relink, not a cold rebuild. The
   *  linker's incremental state is reset to the base by this link. */
  def buildInto(linker: Linker, baseIR: Seq[IRFile], initializers: Seq[ModuleInitializer],
      sessionDir: String): Future[(LinkedImage, RelinkResult)] =
    linkOnce(linker, baseIR, initializers).map { case (next, ms, timings) =>
      Fs.ensureDir(sessionDir)
      Fs.writePackageJson(sessionDir)
      next.foreach { case (name, bytes) => Fs.writeFileBytes(joinPath(sessionDir, name), bytes) }
      val img = new LinkedImage(linker, initializers, baseIR, sessionDir, Nil, next)
      (img, RelinkResult(next.keys.toList.sorted, Nil, Nil, ms, phaseTimings = timings))
    }

  private def linkOnce(linker: Linker, ir: Seq[IRFile],
      inits: Seq[ModuleInitializer]): Future[(Map[String, Array[Byte]], Double, List[PhaseTiming])] =
    val out = MemOutputDirectory()
    val cap = new CapturingLogger(baseLogger)
    val t0 = System.nanoTime()
    linker.link(ir, inits, out, cap).map { _ =>
      val ms = (System.nanoTime() - t0).toDouble / 1e6
      val b = Map.newBuilder[String, Array[Byte]]
      out.fileNames().foreach(n => b += (n -> out.content(n).get))
      (b.result(), ms, cap.timings)
    }

  private def diffAndWriteback(sessionDir: String, prev: Map[String, Array[Byte]],
      next: Map[String, Array[Byte]], ms: Double, timings: List[PhaseTiming]): RelinkResult =
    val t1 = System.nanoTime()
    val res = diff(prev, next, ms, timings)
    val oracleMs = (System.nanoTime() - t1).toDouble / 1e6
    val t2 = System.nanoTime()
    writeback(sessionDir, prev, next)
    val writebackMs = (System.nanoTime() - t2).toDouble / 1e6
    res.copy(oracleMs = oracleMs, writebackMs = writebackMs)

  /** Pure oracle: which modules are new / changed / removed relative to `prev`. */
  private def diff(prev: Map[String, Array[Byte]], next: Map[String, Array[Byte]],
      ms: Double, timings: List[PhaseTiming] = Nil): RelinkResult =
    val prevKeys = prev.keySet
    val nextKeys = next.keySet
    val newModules = (nextKeys diff prevKeys).toList.sorted
    val removed = (prevKeys diff nextKeys).toList.sorted
    val changed = (prevKeys intersect nextKeys).iterator
      .filter(k => !bytesEqual(prev(k), next(k))).toList.sorted
    RelinkResult(newModules, changed, removed, ms, phaseTimings = timings)

  /** Write only new/changed files and delete removed ones (diff-only writeback). */
  private def writeback(sessionDir: String, prev: Map[String, Array[Byte]],
      next: Map[String, Array[Byte]]): Unit =
    val prevKeys = prev.keySet
    val nextKeys = next.keySet
    val newModules = nextKeys diff prevKeys
    val removed = prevKeys diff nextKeys
    val changed = (prevKeys intersect nextKeys).filter(k => !bytesEqual(prev(k), next(k)))
    (newModules ++ changed).foreach(n => Fs.writeFileBytes(joinPath(sessionDir, n), next(n)))
    removed.foreach(n => Fs.deleteFile(joinPath(sessionDir, n)))

  private def bytesEqual(a: Array[Byte], b: Array[Byte]): Boolean =
    (a.length == b.length) && {
      var i = 0
      var eq = true
      while i < a.length && eq do
        if a(i) != b(i) then eq = false
        i += 1
      eq
    }

  private def joinPath(dir: String, name: String): String = s"$dir/$name"

  // --- Node fs writeback -----------------------------------------------------

  private object Fs:
    private def fs = js.Dynamic.global.require("fs")
    def ensureDir(dir: String): Unit =
      fs.mkdirSync(dir, js.Dynamic.literal(recursive = true))
    def writePackageJson(dir: String): Unit =
      fs.writeFileSync(joinPath(dir, "package.json"), """{"type":"module"}""")
    def writeFileBytes(path: String, bytes: Array[Byte]): Unit =
      fs.writeFileSync(path, byteArray2Int8Array(bytes))
    def deleteFile(path: String): Unit =
      try fs.unlinkSync(path)
      catch case _: Throwable => ()

  // --- synthesized IR scaffolding (boot module, marker/entry modules) --------
  //
  // Small IR-tree factories shared by the image's boot module and by the smoke
  // test's fake entries. Reachability of a top-level export is a linker root, so an
  // exported-but-never-called body links (forcing its references) without executing.

  object Synth:
    private given pos: Position = Position.NoPosition
    private val NoOrig = NoOriginalName
    private val EmptyHints = OptimizerHints.empty
    private val Unv = Version.Unversioned
    private val ctorFlags = MemberFlags.empty.withNamespace(MemberNamespace.Constructor)
    private val staticFlags = MemberFlags.empty.withNamespace(MemberNamespace.PublicStatic)
    private val jsStaticFlags = MemberFlags.empty.withNamespace(MemberNamespace.PublicStatic)

    /** Wrap a synthesized `ClassDef` as an in-memory IR file. */
    def directFile(cd: ClassDef): IRFile =
      val nameStr = cd.name.name.nameString
      val v = Version.fromBytes(nameStr.getBytes("UTF-8").nn)
      new TouchRoot.DirectIRFile(s"mem:$nameStr.sjsir", cd, v)

    private def thisTypeOf(cn: ClassName): ClassType = ClassType(cn, nullable = false, exact = false)

    private def superObjectCtor(thisType: ClassType): Tree =
      ApplyStatically(ApplyFlags.empty.withConstructor(true), This()(thisType),
        ObjectClass, MethodIdent(NoArgConstructorName), Nil)(VoidType)

    private def moduleClassDef(cn: ClassName, methods: List[MethodDef],
        tles: List[TopLevelExportDef]): ClassDef =
      ClassDef(ClassIdent(cn), NoOrig, ClassKind.ModuleClass, None,
        Some(ClassIdent(ObjectClass)), Nil, None, None,
        Nil, methods, None, Nil, Nil, tles)(EmptyHints)

    /** `globalThis[name] = value` — the observable marker used by boot / entries. */
    def setGlobalInt(name: String, value: Int): Tree =
      Assign(JSSelect(JSGlobalRef("globalThis"), StringLiteral(name)), IntLiteral(value))

    /** A boot ModuleClass with a public `main()` the module initializer runs, which
     *  sets `globalThis[globalName] = value`. Returns the IR + its initializer. */
    def bootModule(cnStr: String, globalName: String, value: Int): (Seq[IRFile], ModuleInitializer) =
      val cn = ClassName(cnStr)
      val thisType = thisTypeOf(cn)
      val ctor = MethodDef(ctorFlags, MethodIdent(NoArgConstructorName), NoOrig, Nil, VoidType,
        Some(Block(superObjectCtor(thisType), StoreModule())))(EmptyHints, Unv)
      // The linker's `mainMethod` initializer resolves to a *static* method on the
      // class (unlike `mainMethodWithArgs`, which loads the module and calls an
      // instance method); make `main` PublicStatic so it links + runs on boot.
      val mainName = MethodName("main", Nil, VoidRef)
      val mainM = MethodDef(staticFlags, MethodIdent(mainName), NoOrig, Nil, VoidType,
        Some(Block(setGlobalInt(globalName, value), Skip())))(EmptyHints, Unv)
      val cd = moduleClassDef(cn, List(ctor, mainM), Nil)
      (Seq(directFile(cd)), ModuleInitializer.mainMethod(cnStr, "main"))

    /** A ModuleClass exported top-level under `moduleId` whose constructor sets
     *  `globalThis[globalName] = value`; importing `<moduleId>.js` runs it. */
    def markerModule(cnStr: String, moduleId: String, exportName: String,
        globalName: String, value: Int): Seq[IRFile] =
      val cn = ClassName(cnStr)
      val thisType = thisTypeOf(cn)
      val ctor = MethodDef(ctorFlags, MethodIdent(NoArgConstructorName), NoOrig, Nil, VoidType,
        Some(Block(superObjectCtor(thisType), setGlobalInt(globalName, value), StoreModule())))(EmptyHints, Unv)
      val cd = moduleClassDef(cn, List(ctor), List(TopLevelModuleExportDef(moduleId, exportName)))
      Seq(directFile(cd))

    /** A ModuleClass hosting a single top-level-exported method (moduleID
     *  `moduleId`, export name `exportName`) whose body is `body`. The export is a
     *  linker root, so `body`'s references are forced into the link, but the method
     *  is never called (so `body` never executes). Used for the negative control. */
    def exportModule(cnStr: String, moduleId: String, exportName: String, body: Tree): Seq[IRFile] =
      val cn = ClassName(cnStr)
      val thisType = thisTypeOf(cn)
      val ctor = MethodDef(ctorFlags, MethodIdent(NoArgConstructorName), NoOrig, Nil, VoidType,
        Some(Block(superObjectCtor(thisType), StoreModule())))(EmptyHints, Unv)
      val jm = JSMethodDef(jsStaticFlags, StringLiteral(exportName), Nil, None,
        Block(body, Undefined()))(EmptyHints, Unv)
      val cd = moduleClassDef(cn, List(ctor), List(TopLevelMethodExportDef(moduleId, jm)))
      Seq(directFile(cd))

    /** An [[exportModule]] whose exported (never-called) body force-references the
     *  zero-arg method `targetMethod` on `targetClass` via a `Null()` receiver — the
     *  touch-root trick to make the linker emit that method's body without executing
     *  it. Used by the negative control to grow an already-emitted module. */
    def forceMethodModule(cnStr: String, moduleId: String, exportName: String,
        targetClass: ClassName, targetMethod: MethodName, resultType: Type): Seq[IRFile] =
      val body = ApplyStatically(ApplyFlags.empty, Null(), targetClass,
        MethodIdent(targetMethod), Nil)(resultType)
      exportModule(cnStr, moduleId, exportName, body)

    /** The per-line execution trigger ([[LinkedRunner]]).
     *
     *  A ModuleClass exported top-level under moduleID `linetrigger_<lineId>` with a
     *  single exported method `run()` whose body loads and runs the REPL wrapper
     *  `object rs$line$N` (module class `<wrapperClassName>$`): `LoadModule` forces
     *  the wrapper's module accessor + constructor (the user code + injected
     *  `pushRender`s), and the trailing `replMain()` is the same no-op trigger the
     *  interpreter invokes. Importing `linetrigger_<lineId>.js` defines the function;
     *  calling `run()` executes the line — in the worker process, so `__replRenders`
     *  and `process.stdout` fill exactly as under the interpreter.
     *
     *  A second, NEVER-called export `touch` force-references every public member of
     *  this line's fresh classes ([[TouchRoot.lineForce]]), so they freeze into their
     *  (new) modules at THIS link and a later first-reference (cross-line `def` reuse,
     *  `object` member access, `new C().m`) stays byte-additive. It lives in its own
     *  export — a linker root, so its body is linked — rather than the executed `run`
     *  body, so member forcing never re-runs user code (mirrors the image touch root).
     *
     *  Returns the trigger's IR file, its moduleID (⇒ the emitted `.js` name and
     *  `run` export the runner imports + calls), and the per-line force tally. */
    def lineTrigger(lineId: Int, wrapperCN: ClassName,
        freshDefs: Seq[ClassDef]): (Seq[IRFile], String, TouchRoot.LineForceStats) =
      val moduleId = s"linetrigger_$lineId"
      val cn = ClassName(s"linetrigger.Line$lineId$$")
      val thisType = thisTypeOf(cn)
      val replMain = MethodName("replMain", Nil, VoidRef)
      val ctor = MethodDef(ctorFlags, MethodIdent(NoArgConstructorName), NoOrig, Nil, VoidType,
        Some(Block(superObjectCtor(thisType), StoreModule())))(EmptyHints, Unv)
      val runBody = Block(
        Apply(ApplyFlags.empty, LoadModule(wrapperCN), MethodIdent(replMain), Nil)(VoidType),
        Undefined())
      val runJm = JSMethodDef(jsStaticFlags, StringLiteral("run"), Nil, None, runBody)(EmptyHints, Unv)
      val runExport = TopLevelMethodExportDef(moduleId, runJm)
      val (forceStmts, forceStats) = TouchRoot.lineForce(freshDefs)
      val exports =
        if forceStmts.isEmpty then List(runExport)
        else
          val touchJm = JSMethodDef(jsStaticFlags, StringLiteral("touch"), Nil, None,
            Block(forceStmts :+ Undefined()))(EmptyHints, Unv)
          List(runExport, TopLevelMethodExportDef(moduleId, touchJm))
      val cd = moduleClassDef(cn, List(ctor), exports)
      (Seq(directFile(cd)), moduleId, forceStats)
