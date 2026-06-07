package scala.runtime.eval

/** Pre-compiled base class for the synthesised `__EvalExpression_…` classes.
 *
 *  The eval driver compiles a synthesised `__EvalExpression_<uuid>` for each call
 *  site; its `evaluate()` is filled by `ExtractEvalBody` with the user's body,
 *  and outer-scope references are lowered (by `ResolveEvalAccess`) into calls
 *  against the helpers defined here.
 *
 *  Unlike the JVM port, the interpreter shares one heap with the session, so the
 *  binding-based helpers (`getValue`/`getRaw`/`getThisObject`/var facades) need
 *  no reflection at all. The member-access helpers (`getField`/`setField`/
 *  `callMethod`/`getOuter`), which the JVM port implements with
 *  `java.lang.reflect`, are stubbed here pending an interpreter-native
 *  implementation — the foundation only lowers strategies that route through the
 *  bindings array, so these are never reached yet.
 */
abstract class EvalExpressionBase(bindings: Array[Eval.Binding]):

  /** The synthesised subclass implements this with the user's body. Left
   *  abstract so a subclass missing `evaluate()` is a compile error. */
  def evaluate(): Any

  /** The captured `this` of an enclosing class, stored as a `"__this__"`
   *  binding (or `null` when the call site has no enclosing instance). */
  protected final def getThisObject(): Object | Null =
    var i = 0
    while i < bindings.length do
      if bindings(i).name == "__this__" then
        return bindings(i).value.asInstanceOf[Object | Null]
      i = i + 1
    null

  /** Look up a binding by name, auto-unwrapping `Eval.VarRef` so the body sees a
   *  `T` rather than a live var facade. Var-assignment codegen uses `getRaw` to
   *  preserve the facade. */
  protected final def getValue(name: String): Any =
    val raw = getRaw(name)
    if raw.isInstanceOf[Eval.VarRef[?]] then
      raw.asInstanceOf[Eval.VarRef[Any]].get()
    else raw

  /** Return the binding's stored value as-is (no `VarRef` unwrap). */
  protected final def getRaw(name: String): Any =
    var i = 0
    while i < bindings.length do
      if bindings(i).name == name then return bindings(i).value
      i = i + 1
    throw new NoSuchElementException(name)

  // --- member-access helpers (interpreter-native impl pending) ---------------

  private def notYet(what: String): Nothing =
    throw new UnsupportedOperationException(
      s"eval: $what is not yet supported on the Scala.js REPL")

  protected final def getOuter(obj: Object | Null, outerTypeName: String): Object | Null =
    notYet("outer-class access ($outer walk)")

  protected final def getField(obj: Object | Null, className: String, fieldName: String): Any =
    notYet(s"private/protected field access ($fieldName)")

  protected final def setField(obj: Object | Null, className: String, fieldName: String, value: Object | Null): Unit =
    notYet(s"private/protected field write ($fieldName)")

  protected final def callMethod(
      obj: Object | Null,
      className: String,
      methodName: String,
      paramTypesNames: Array[String],
      returnTypeName: String,
      args: Array[Object | Null]
  ): Any =
    notYet(s"private/protected method call ($methodName)")

  /** Placeholder consumed by `ResolveEvalAccess`. Any surviving call means the
   *  lowering pipeline broke. */
  protected final def reflectEval(qualifier: Object | Null, strategyDesc: String, args: Array[Object | Null]): Any =
    throw new UnsupportedOperationException("reflectEval placeholder was not lowered")

end EvalExpressionBase
