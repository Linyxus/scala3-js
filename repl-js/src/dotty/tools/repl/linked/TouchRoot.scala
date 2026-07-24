package dotty.tools
package repl
package linked

import scala.concurrent.{Future, ExecutionContext}

import org.scalajs.ir.Trees.*
import org.scalajs.ir.Types.*
import org.scalajs.ir.Names.*
import org.scalajs.ir.{ClassKind, Position, Version, EntryPointsInfo}
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.WellKnownNames.{ObjectClass, NoArgConstructorName}

import org.scalajs.linker.interface.IRFile
import org.scalajs.linker.interface.unstable.IRFileImpl
import org.scalajs.linker.standard.CoreSpec

/** Synthesizes a never-executed "touch-all" IR root over the Scala.js stdlib IR,
 *  so a `SmallestModules` link freezes a *member-complete* image: one ES module per
 *  class, every linkable member reachable.
 *
 *  This is the JS-hosted port of the JVM spike's phase-2.5 `LinkDriver25.synthesize`
 *  + `computeBad` (`repl-link-spike/linker-driver/LinkDriver25.scala`). It consumes
 *  the fork's IR (the `ClassDef`s deserialized from `linker-libs.bin` and any extra
 *  libraries — the same bytes [[InterpreterRunner]] feeds the interpreter) and emits
 *  a small set of synthetic `ClassDef`s:
 *
 *   - a host `touchroot.Exports$` ModuleClass carrying per-package, chunked
 *     `TopLevelMethodExportDef`s (moduleID `"touchroot"`). Every touch statement is an
 *     ordinary IR edge — `LoadModule` (module accessors), `New` (constructors),
 *     `ApplyStatically`/`ApplyStatic` on a `Null()` receiver (forces the exact body,
 *     never virtual), `ClassOf`/`IsInstanceOf` (classData + instance tests). Because
 *     reachability flows through these edges attributed to the *referencing* unit
 *     (not `java.lang.Object`), SmallestModules splits per class instead of
 *     collapsing into one giant module.
 *   - a concrete placeholder subclass per abstract class, `New`-ed by the root, which
 *     forces the abstract class's constructor + prototype into its own frozen module
 *     (the spike's "ed" fix — a category `SymbolRequirement`s cannot cover).
 *
 *  The touch root is reachable only through top-level exports (linker roots) and is
 *  never imported at runtime, so it links but never executes. Determinism is
 *  non-negotiable: every iteration is sorted, nothing is timestamped, so relinking
 *  identical input reproduces identical bytes.
 */
object TouchRoot:

  private given pos: Position = Position.NoPosition

  /** An in-memory IR file backed directly by a synthesized `ClassDef` (no
   *  serialization round-trip). Mirrors the spike's `DirectIRFile`. */
  final class DirectIRFile(p: String, cd: ClassDef, v: Version) extends IRFileImpl(p, v):
    def entryPointsInfo(using ec: ExecutionContext): Future[EntryPointsInfo] =
      Future.successful(EntryPointsInfo.forClassDef(cd))
    def tree(using ec: ExecutionContext): Future[ClassDef] =
      Future.successful(cd)

  /** Accounting for the synthesized root (surfaced by the smoke test / stage B). */
  final case class IRStats(
      synthClasses: Int, exports: Int, touchStmts: Int,
      touchedClasses: Int, modulesLoaded: Int, ctorsNew: Int,
      concreteMethods: Int, staticMethods: Int,
      placeholders: Int, placeholderSkipped: Int, abstractTargets: Int,
      argSkipped: Int)

  /** The synthesized touch-root files plus the pre-filter sets that bounded them
   *  (the latter let callers, e.g. the smoke test, pick a deliberately-excluded but
   *  linkable member for a negative control). */
  final case class Built(
      files: Seq[IRFile], stats: IRStats,
      bad: Set[(ClassName, MethodName, Int)],
      badModules: Set[ClassName], structBad: Set[ClassName])

  /** Compute the pre-filter, synthesize the touch root, and wrap each synthesized
   *  `ClassDef` as an [[IRFile]] ready to hand to the linker. */
  def build(baseDefs: Seq[ClassDef], present: Set[ClassName], coreSpec: CoreSpec,
      exclusions: Set[(String, String)]): Built =
    val (bad, badModules, structBad) = computeBad(baseDefs, present, coreSpec)
    val (defs, stats) = synthesize(baseDefs, exclusions, bad, badModules, structBad,
      placeholders = true, traitPlaceholders = false, doInst = true)
    val files: Seq[IRFile] = defs.map { cd =>
      val nameStr = cd.name.name.nameString
      val v = Version.fromBytes(nameStr.getBytes("UTF-8").nn)
      new DirectIRFile(s"mem:$nameStr.sjsir", cd, v): IRFile
    }
    Built(files, stats, bad, badModules, structBad)

  /** The method-exclusion predicate [[synthesize]] uses, exposed so callers can
   *  find a member the touch root deliberately did NOT force. */
  def isMethodExcluded(excl: Set[(String, String)], ownerStr: String, simpleName: String): Boolean =
    val prefixes = excl.collect { case (o, m) if m.endsWith("~") => (o, m.dropRight(1)) }
    excl.contains((ownerStr, "*")) || excl.contains((ownerStr, simpleName)) ||
      excl.contains(("*", simpleName)) ||
      prefixes.exists { case (o, p) => (o == "*" || o == ownerStr) && simpleName.startsWith(p) }

  // ============================ IR-root synthesis ============================

  /** Build the synthesized touch-root `ClassDef`s (host + placeholders). */
  def synthesize(baseDefs: Seq[ClassDef],
      excl: Set[(String, String)],
      bad: Set[(ClassName, MethodName, Int)],
      badModules: Set[ClassName], structBad: Set[ClassName],
      placeholders: Boolean, traitPlaceholders: Boolean,
      doInst: Boolean = true,
      chunkSize: Int = 250): (Seq[ClassDef], IRStats) =

    val NoOrig = NoOriginalName
    val EmptyHints = OptimizerHints.empty
    val Unv = Version.Unversioned
    val byName: Map[ClassName, ClassDef] = baseDefs.map(cd => cd.name.name -> cd).toMap

    // ---- exclusion machinery (mirrors the spike's genRequirements) ----
    val prefixExcls = excl.collect { case (p, "**") => p }.toList
    val methodPrefixExcls =
      excl.collect { case (o, m) if m.endsWith("~") => (o, m.dropRight(1)) }.toList
    def classExcluded(s: String): Boolean =
      excl.contains((s, "*")) || prefixExcls.exists(s.startsWith)
    def methodExcluded(ownerStr: String, m: MethodDef): Boolean =
      val sn = m.name.name.simpleName.nameString
      excl.contains((ownerStr, "*")) || excl.contains((ownerStr, sn)) ||
        excl.contains(("*", sn)) ||
        methodPrefixExcls.exists { case (o, p) => (o == "*" || o == ownerStr) && sn.startsWith(p) }

    // ---- ancestry (self + super + interfaces), memoized ----
    val ancCache = scala.collection.mutable.HashMap.empty[ClassName, Set[ClassName]]
    def ancestors(cn: ClassName): Set[ClassName] =
      ancCache.getOrElseUpdate(cn,
        byName.get(cn) match
          case None => Set(cn)
          case Some(cd) =>
            (cd.superClass.map(_.name).toList ++ cd.interfaces.map(_.name))
              .foldLeft(Set(cn))((a, p) => if a.contains(p) then a else a ++ ancestors(p)))

    // A class is (Scala-)instantiable iff every Public abstract method declared
    // anywhere in its ancestry has a concrete provider somewhere in the ancestry.
    def unimplementedAbstract(cn: ClassName): Map[MethodName, MethodDef] =
      val abstractDecl = scala.collection.mutable.HashMap.empty[MethodName, MethodDef]
      val concrete = scala.collection.mutable.HashSet.empty[MethodName]
      for a <- ancestors(cn); adef <- byName.get(a).toList; m <- adef.methods
          if m.flags.namespace == MemberNamespace.Public do
        if m.body.isEmpty then abstractDecl.getOrElseUpdate(m.name.name, m)
        else concrete += m.name.name
      abstractDecl.filterNot { case (mn, _) => concrete.contains(mn) }.toMap
    def isInstantiable(cn: ClassName): Boolean = unimplementedAbstract(cn).isEmpty

    // ---- zero-value construction ----
    var argSkipped = 0
    // A well-typed, never-executed zero value of the given type, or None if the
    // type cannot be represented (typed closures, records, void, nothing).
    def zeroArg(tpe: Type): Option[Tree] = tpe match
      case _: ClosureType | _: RecordType | VoidType | NothingType | AnyNotNullType => None
      case ClassType(_, false, _) | ArrayType(_, false, _) => Some(AsInstanceOf(Null(), tpe))
      case _ => Some(zeroOf(tpe))
    // Args for a method/ctor: Some(list) or None if any param is unrepresentable.
    def argsFor(m: MethodDef): Option[List[Tree]] =
      val bs = m.args.map(p => zeroArg(p.ptpe))
      if bs.forall(_.isDefined) then Some(bs.map(_.get)) else { argSkipped += 1; None }
    // Stub body for an abstract method override on a placeholder.
    def stubBody(rt: Type): Option[Tree] =
      if rt == VoidType then Some(Skip()) else zeroArg(rt)

    // ---- per-package touch statements ----
    val perPkg = scala.collection.mutable.LinkedHashMap.empty[String, scala.collection.mutable.ArrayBuffer[Tree]]
    def pkgOf(cn: ClassName): String =
      val s = cn.nameString; val i = s.lastIndexOf('.'); if i < 0 then "_root_" else s.substring(0, i)
    def emit(pkg: String, t: Tree): Unit =
      perPkg.getOrElseUpdate(pkg, scala.collection.mutable.ArrayBuffer.empty[Tree]) += t

    var touchedClasses, modulesLoaded, ctorsNew, concreteMethods, staticMethods = 0
    var placeholderCount, placeholderSkipped, abstractTargets = 0
    val synthPlaceholders = scala.collection.mutable.ArrayBuffer.empty[ClassDef]
    var placeholderIdx = 0

    import ClassKind.*
    // deterministic order
    val sortedDefs = baseDefs.sortBy(_.name.name.nameString)

    for cd <- sortedDefs
        if !classExcluded(cd.name.name.nameString) && !structBad.contains(cd.name.name) do
      val cn = cd.name.name
      val k = cd.kind
      val ownerStr = cn.nameString
      val pkg = pkgOf(cn)
      val hijacked = k == HijackedClass
      def mkey(m: MethodDef) = (cn, m.name.name, m.flags.namespace.ordinal)

      val classLike = k == Class || k == ModuleClass || k == Interface
      if classLike && !hijacked then
        touchedClasses += 1
        emit(pkg, ClassOf(ClassRef(cn)))                                   // classData
        emit(pkg, IsInstanceOf(Null(), ClassType(cn, nullable = false, exact = false))) // instanceTests

      if k == ModuleClass && !badModules.contains(cn) then
        modulesLoaded += 1
        emit(pkg, LoadModule(cn))                                          // module accessor

      // concrete instantiable Class: New every usable public ctor
      if doInst && k == Class && isInstantiable(cn) then
        for m <- cd.methods.sortBy(_.name.name.nameString)
            if m.flags.namespace == MemberNamespace.Constructor && m.body.isDefined
              && !methodExcluded(ownerStr, m) && !bad.contains(mkey(m)) do
          argsFor(m).foreach { args => emit(pkg, New(cn, MethodIdent(m.name.name), args)); ctorsNew += 1 }

      // force concrete method bodies (skip hijacked, private, constructors)
      if k == Class || k == ModuleClass || k == Interface then
        for m <- cd.methods.sortBy(mm => (mm.flags.namespace.ordinal, mm.name.name.nameString))
            if m.body.isDefined && !methodExcluded(ownerStr, m) && !bad.contains(mkey(m)) do
          m.flags.namespace match
            case MemberNamespace.Public =>
              argsFor(m).foreach { args =>
                emit(pkg, ApplyStatically(ApplyFlags.empty, Null(), cn, MethodIdent(m.name.name), args)(m.resultType))
                concreteMethods += 1 }
            case MemberNamespace.PublicStatic =>
              argsFor(m).foreach { args =>
                emit(pkg, ApplyStatic(ApplyFlags.empty, cn, MethodIdent(m.name.name), args)(m.resultType))
                staticMethods += 1 }
            case _ => ()

      // ---- placeholder for abstract targets (close the "ed" gap) ----
      val isAbstractClass = k == Class && !isInstantiable(cn)
      val isTrait = k == Interface
      val wantPlaceholder = doInst &&
        ((placeholders && isAbstractClass) || (traitPlaceholders && isTrait))
      if wantPlaceholder && !hijacked then
        abstractTargets += 1
        // choose a super constructor (abstract classes) or Object (traits)
        val superClass = if isTrait then ObjectClass else cn
        val superCtorOpt: Option[(MethodName, List[Tree])] =
          if isTrait then Some((NoArgConstructorName, Nil))
          else
            byName.get(cn).toList.flatMap(_.methods)
              .filter(m => m.flags.namespace == MemberNamespace.Constructor && m.body.isDefined
                && !bad.contains((cn, m.name.name, m.flags.namespace.ordinal)))
              .sortBy(_.name.name.nameString)
              .flatMap(m => argsFor(m).map(a => (m.name.name, a)))
              .headOption
        // stub bodies for all unimplemented abstract public methods in ancestry
        val abstracts = unimplementedAbstract(cn).toList.sortBy(_._1.nameString)
        val stubOpts = abstracts.map { case (mn, m) => stubBody(m.resultType).map(b => (mn, m, b)) }
        superCtorOpt match
          case Some((scName, scArgs)) if stubOpts.forall(_.isDefined) =>
            val pName = ClassName(s"touchroot.P$placeholderIdx$$" +
              cn.nameString.replaceAll("[^A-Za-z0-9]", "_"))
            placeholderIdx += 1
            val thisType = ClassType(pName, nullable = false, exact = false)
            val ctorBody = Block(
              ApplyStatically(ApplyFlags.empty.withConstructor(true), This()(thisType),
                superClass, MethodIdent(scName), scArgs)(VoidType),
              Skip())
            val ctorDef = MethodDef(MemberFlags.empty.withNamespace(MemberNamespace.Constructor),
              MethodIdent(NoArgConstructorName), NoOrig, Nil, VoidType, Some(ctorBody))(EmptyHints, Unv)
            val stubDefs = stubOpts.map(_.get).map { case (mn, m, b) =>
              MethodDef(MemberFlags.empty.withNamespace(MemberNamespace.Public),
                MethodIdent(mn), NoOrig, m.args, m.resultType, Some(b))(EmptyHints, Unv)
            }
            val supers = if isTrait then (Some(ClassIdent(ObjectClass)), List(ClassIdent(cn)))
                         else (Some(ClassIdent(cn)), Nil)
            val pDef = ClassDef(ClassIdent(pName), NoOrig, ClassKind.Class, None,
              supers._1, supers._2, None, None,
              Nil, ctorDef :: stubDefs, None, Nil, Nil, Nil)(EmptyHints)
            synthPlaceholders += pDef
            emit(pkg, New(pName, MethodIdent(NoArgConstructorName), Nil))
            placeholderCount += 1
          case _ =>
            placeholderSkipped += 1

    // ---- assemble the host ModuleClass with chunked TopLevelMethodExportDefs ----
    val jsStaticFlags = MemberFlags.empty.withNamespace(MemberNamespace.PublicStatic)
    val hostName = ClassName("touchroot.Exports$")
    val hostType = ClassType(hostName, nullable = false, exact = false)
    val hostCtor = MethodDef(MemberFlags.empty.withNamespace(MemberNamespace.Constructor),
      MethodIdent(NoArgConstructorName), NoOrig, Nil, VoidType,
      Some(Block(
        ApplyStatically(ApplyFlags.empty.withConstructor(true), This()(hostType),
          ObjectClass, MethodIdent(NoArgConstructorName), Nil)(VoidType),
        StoreModule())))(EmptyHints, Unv)

    var exportCount = 0
    var totalStmts = 0
    val exportDefs = scala.collection.mutable.ArrayBuffer.empty[TopLevelExportDef]
    for (pkg, stmts) <- perPkg.toList.sortBy(_._1) do
      totalStmts += stmts.size
      val safe = pkg.replaceAll("[^A-Za-z0-9]", "_")
      stmts.grouped(chunkSize).zipWithIndex.foreach { case (chunk, i) =>
        val body = Block(chunk.toList :+ Undefined())
        val jm = JSMethodDef(jsStaticFlags, StringLiteral(s"t_${safe}_$i"), Nil, None, body)(EmptyHints, Unv)
        exportDefs += TopLevelMethodExportDef("touchroot", jm)
        exportCount += 1
      }

    val host = ClassDef(ClassIdent(hostName), NoOrig, ClassKind.ModuleClass, None,
      Some(ClassIdent(ObjectClass)), Nil, None, None,
      Nil, List(hostCtor), None, Nil, Nil, exportDefs.toList)(EmptyHints)

    val all = host +: synthPlaceholders.toSeq
    val stats = IRStats(all.size, exportCount, totalStmts, touchedClasses, modulesLoaded,
      ctorsNew, concreteMethods, staticMethods, placeholderCount, placeholderSkipped,
      abstractTargets, argSkipped)
    (all, stats)

  // ======================= reference-graph pre-filter =======================

  /** Compute, as a fixpoint over the per-method reference graph the linker's own
   *  `Infos.InfoGenerator` extracts, the set of members/modules that are unlinkable
   *  on Scala.js (transitively reach a class/member absent from the IR) plus the
   *  classes whose ancestry is structurally absent. Direct JS port of the spike. */
  def computeBad(defs: Seq[ClassDef], present: Set[ClassName],
      coreSpec: CoreSpec): (Set[(ClassName, MethodName, Int)], Set[ClassName], Set[ClassName]) =
    import org.scalajs.linker.analyzer.Infos
    import org.scalajs.linker.analyzer.Infos.{MethodReachable, MethodStaticallyReachable}
    import org.scalajs.linker.frontend.LinkTimeProperties
    val FlagModuleAccessed = Infos.ReachabilityInfoInClass.FlagModuleAccessed
    val FlagInstantiated = Infos.ReachabilityInfoInClass.FlagInstantiated
    val noArgCtor = NoArgConstructorName
    val ctorOrd = MemberNamespace.Constructor.ordinal
    val pubOrd = MemberNamespace.Public.ordinal
    val gen = new Infos.InfoGenerator(LinkTimeProperties.fromCoreSpec(coreSpec))
    type MK = (ClassName, MethodName, Int)
    val byNameAll = defs.map(cd => cd.name.name -> cd).toMap
    val ancCache = scala.collection.mutable.HashMap.empty[ClassName, Set[ClassName]]
    def ancestors(c: ClassName): Set[ClassName] =
      ancCache.getOrElseUpdate(c,
        byNameAll.get(c) match
          case None => Set(c)
          case Some(cd) =>
            (cd.superClass.map(_.name).toList ++ cd.interfaces.map(_.name))
              .foldLeft(Set(c))((a, p) => if a.contains(p) then a else a ++ ancestors(p)))
    val infoByKey = scala.collection.mutable.HashMap.empty[MK, Infos.MethodInfo]
    for cd <- defs; m <- cd.methods if m.body.isDefined do
      infoByKey((cd.name.name, m.name.name, m.flags.namespace.ordinal)) = gen.generateMethodInfo(m)
    def ctorKeys(c: ClassName): List[MK] =
      byNameAll.get(c).toList.flatMap(_.methods.collect {
        case m if m.flags.namespace == MemberNamespace.Constructor && m.body.isDefined =>
          (c, m.name.name, ctorOrd)
      })
    val declaredKeys = infoByKey.keySet
    val staticish = Set(ctorOrd, MemberNamespace.PublicStatic.ordinal,
      MemberNamespace.PrivateStatic.ordinal)
    val bad = scala.collection.mutable.HashSet.empty[MK]
    val structBad = defs.iterator
      .filter(cd => ancestors(cd.name.name).exists(a => !present.contains(a)))
      .map(_.name.name).toSet
    for cd <- defs if structBad.contains(cd.name.name); m <- cd.methods if m.body.isDefined do
      bad += ((cd.name.name, m.name.name, m.flags.namespace.ordinal))
    var changed = true
    while changed do
      changed = false
      for (k, info) <- infoByKey if !bad.contains(k) do
        var isBad = false
        val arr = info.byClass; var i = 0
        while i < arr.length && !isBad do
          val ric = arr(i); val c = ric.className
          if !present.contains(c) then isBad = true
          else
            val f = ric.flags
            if (f & FlagModuleAccessed) != 0 && bad.contains((c, noArgCtor, ctorOrd)) then isBad = true
            if !isBad && (f & FlagInstantiated) != 0 && ctorKeys(c).exists(bad.contains) then isBad = true
            // `memberInfos` is a nullable perf field; the fork's Scala.js library
            // signatures don't carry explicit-nulls info, so widen to compare.
            val mems: Array[Infos.MemberReachabilityInfo] | scala.Null = ric.memberInfos
            if !isBad && mems != null then
              var j = 0
              while j < mems.length && !isBad do
                mems(j) match
                  case m: MethodStaticallyReachable =>
                    val ord = m.namespace.ordinal
                    if bad.contains((c, m.methodName, ord)) then isBad = true
                    else if staticish.contains(ord) && !declaredKeys.contains((c, m.methodName, ord)) then
                      isBad = true
                  case m: MethodReachable =>
                    if bad.contains((c, m.methodName, pubOrd)) then isBad = true
                  case _ => ()
                j += 1
          i += 1
        if isBad then { bad += k; changed = true }
    val badModules = defs.collect {
      case cd if cd.kind == ClassKind.ModuleClass && bad.contains((cd.name.name, noArgCtor, ctorOrd)) =>
        cd.name.name
    }.toSet
    (bad.toSet, badModules, structBad)
