package org.scalajs.sjsirinterpreter.core

import scala.scalajs.js
import scala.collection.mutable

import org.scalajs.ir.{Names, Types, Position}
import org.scalajs.ir.Trees.ClassDef
import org.scalajs.sjsirinterpreter.core.values.Value

/** Synchronous eval driver, living *inside* the interpreter's package so it can
 *  reach the `private[core]` API (`executor`, `getClassInfo`, `ClassInfo`).
 *
 *  A runtime `eval(...)` executes while the interpreter stack is blocked, so its
 *  load+run must be fully synchronous; the public `Interpreter.loadIRFiles` is
 *  `Future`-based and schedules its registration on the microtask queue, which
 *  can't run while we're blocked. We therefore replicate `loadIRFiles`'
 *  synchronous registration core here: deserialize → build `ClassInfo` →
 *  register → run static initializers.
 *
 *  The class registry (`Interpreter.classInfos`) is plain-`private`, so even from
 *  this package it isn't reachable through the Scala API. We read it through its
 *  stable Scala.js field encoding (`<fqcn>__f_classInfos`). This is the one
 *  reach-in the design needs; a public synchronous-load API on the interpreter
 *  would replace it. Pinned to sjsir-interpreter 0.10.0.
 */
object EvalSupport:

  /** `MethodName` for the synthesised `def evaluate(): Any` (erased `()Object`). */
  private val EvaluateMethodName: Names.MethodName =
    Names.MethodName("evaluate", Nil, Types.ClassRef(Names.ClassName("java.lang.Object")))

  /** The interpreter's private class registry, reached by scanning the instance's
   *  own property keys for the Scala.js-mangled `…__f_classInfos` field (the exact
   *  name carries a linker-encoding prefix, so we match by suffix rather than
   *  hardcode it). */
  private def classInfosOf(interp: Interpreter): mutable.Map[Names.ClassName, ClassInfo] =
    val obj = interp.asInstanceOf[js.Object]
    val key = js.Object.keys(obj).find(_.endsWith("__f_classInfos")).getOrElse(
      throw new IllegalStateException("EvalSupport: could not locate Interpreter.classInfos field"))
    obj.asInstanceOf[js.Dynamic].selectDynamic(key)
      .asInstanceOf[mutable.Map[Names.ClassName, ClassInfo]]

  /** Register fresh `ClassDef`s into the interpreter (dedup by class name), then
   *  run their static initializers — mirroring the post-parse half of
   *  `Interpreter.loadIRFiles`. */
  def registerClassDefs(interp: Interpreter, classDefs: List[ClassDef]): Unit =
    given Position = Position.NoPosition
    val classInfos = classInfosOf(interp)
    val newInfos = mutable.ListBuffer.empty[ClassInfo]
    for cd <- classDefs do
      val cn = cd.className
      if !classInfos.contains(cn) then
        val ci = new ClassInfo(interp, cn, cd)
        classInfos.update(cn, ci)
        newInfos += ci
    if newInfos.nonEmpty then
      val sorted = newInfos.toList.sortBy(_.classNameString)
      interp.executor.runStaticInitializers(sorted)
      interp.executor.initializeTopLevelExports(sorted)

  /** Instantiate `expressionClassName(bindings)` and invoke `evaluate()`. */
  def instantiateAndRun(interp: Interpreter, expressionClassName: String, bindings: Any): Any =
    given Position = Position.NoPosition
    val ci = interp.getClassInfo(Names.ClassName(expressionClassName))
    val ctor = ci.lookupSingleConstructor()
    val instance = interp.executor.newInstanceWithConstructor(
      ctor, List(bindings.asInstanceOf[Value]))
    val evalM = ci.lookupPublicMethod(EvaluateMethodName)
    interp.executor.applyMethodDefGeneric(evalM, Some(instance), Nil)
