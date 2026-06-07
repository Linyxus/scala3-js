package scala.runtime.eval

// Enables the `@caps.assumeSafe` annotation on `object Eval` below: that
// annotation is `@experimental`, but the cc-experimental exception lets it be
// used in any unit that imports `experimental.captureChecking`.
import scala.language.experimental.captureChecking

/** Runtime `eval` for the Scala.js dotty REPL.
 *
 *  `Eval.eval(code)` compiles and runs `code` at runtime against the live REPL
 *  session, returning the result typed as `T`. The argument can be any `String`
 *  computed at runtime (a literal, an interpolation, an LLM response, …).
 *
 *  This is the Scala.js port of the JVM dynamic-eval surface. The two platforms
 *  differ only in how the call reaches the compiler:
 *
 *   - On the JVM the running REPL installs an `Adapter` in an
 *     `InheritableThreadLocal`, and the public API uses JVM-intrinsic types to
 *     dodge classloader-boundary `LinkageError`s.
 *   - Here the whole session runs in ONE sjsir-interpreter heap (no classloaders,
 *     so no `LinkageError` risk), and the call reaches the driver through a
 *     JS-global bridge (`globalThis.__replEval`, see [[EvalBridge]]). We can
 *     therefore use ordinary Scala types (`scala.Function1`, `Either`, …) freely.
 *
 *  These types live in `library/src` (like `scala.runtime.ReplRenderer`) so they
 *  are bundled into both the compile classpath (`classpath.bin`) and the
 *  interpreter's loaded library (`linker-libs.bin`), making them referenceable
 *  from user code and from the synthesised `__EvalExpression` wrappers.
 *
 *  Tagged `@caps.assumeSafe` so safe-mode user code can call `eval`, `evalSafe`,
 *  `bind`, `varRef`, etc. The driver re-compiles the user's body string under
 *  the live session's flags, so a safe-mode session still applies safe-mode
 *  checks to the body itself: this annotation only exempts the eval driver's own
 *  surface API from safe-mode rejection, not the bodies it compiles.
 */
@caps.assumeSafe
object Eval:

  /** A captured binding.
   *
   *  @param name   source-level name at the call site.
   *  @param value  runtime value, or for `var` captures a [[VarRef]] facade
   *                closing over the outer var.
   *  @param isVar  `var` capture.
   *  @param isGiven  `given` capture (emitted into a `(using ...)` clause on the
   *                  synthesised wrapper so `summon[T]` resolves).
   */
  final class Binding(
      val name: String,
      val value: Any,
      val isVar: Boolean,
      val isGiven: Boolean = false
  ):
    override def toString: String =
      s"Binding($name, $value, isVar=$isVar, isGiven=$isGiven)"

  /** Live getter/setter facade over a captured `var`. The rewriter emits a
   *  `varRef(() => x, v => x = v)` at every `bindVar` site: the lambdas close
   *  over the outer var, so reads through `get()` always return the current
   *  value and writes through `set(v)` are immediately visible to every other
   *  piece of code that captured the same outer var.
   */
  trait VarRef[T]:
    def get(): T
    def set(v: T): Unit

  /** Helper used by the rewriter so the bind-site call `varRef(() => x, v => x = v)`
   *  stays terse. The parameter types are the JDK functional interfaces
   *  (`Supplier`/`Consumer`) rather than `scala.Function0`/`Function1` for a
   *  capture-checking reason: a `Supplier[T]` / `Consumer[T]` is a nominal SAM
   *  with an empty capture set, so the read/write effect of a captured `var`
   *  cannot flow through this facade. Under safe mode (which forbids the
   *  escape hatches) that makes capturing a mutable var into a *pure* function
   *  in the eval body a compile error — the intended behaviour — while a plain
   *  `() => T` would silently carry the effect. Scala 3 SAM conversion accepts a
   *  `() => x` literal where a `Supplier[T]` is expected and `v => x = v` where a
   *  `Consumer[T]` is expected. */
  def varRef[T](
      get: java.util.function.Supplier[T],
      set: java.util.function.Consumer[T]
  ): VarRef[T] =
    val getFn = get
    val setFn = set
    new VarRef[T]:
      def get(): T = getFn.get()
      def set(v: T): Unit = setFn.accept(v)

  /** Capture an immutable binding. */
  def bind(name: String, value: Any): Binding =
    new Binding(name, value, isVar = false)

  /** Capture a mutable (`var`) binding via a [[VarRef]] facade. */
  def bindVar(name: String, ref: VarRef[?]): Binding =
    new Binding(name, ref, isVar = true)

  /** Capture a `given` binding. */
  def bindGiven(name: String, value: Any): Binding =
    new Binding(name, value, isVar = false, isGiven = true)

  /** Synthetic verification-compile shim for `evalSafe[T]` calls. The verify
   *  compile re-elaborates `enclosingSource` with the body spliced into the
   *  marker; for `evalSafe[T]` the surrounding context expects `EvalResult[T]`,
   *  so the rewriter wraps the marker in `Eval.handleCompileError(...)`. Never
   *  invoked at runtime — it exists purely so the verify typechecker has
   *  something to call. */
  def handleCompileError[T](v: T): EvalResult[T] =
    EvalResult.success(v)

  /** Synthetic no-op inserted between the spliced `val __evalResult` and the
   *  block's tail so the optimiser can't constant-fold away the `val` site that
   *  `ExtractEvalBody` later drains. */
  def __noFold__(): Unit = ()

  /** Compile-failure descriptor produced by the wrapper compile and carried back
   *  through the bridge. Surfaced as the failure side of [[EvalResult]] (and as
   *  the data behind [[EvalCompileException]] for the throwing form). */
  final class CompileFailure(val errors: Array[String], val source: String):
    override def toString: String =
      s"CompileFailure(${errors.length} error(s))"

  // --- public entry points ---------------------------------------------------

  /** Compile and run `code` against the current REPL session, returning `T`.
   *
   *  The defaulted parameters are normally filled by the `EvalRewriteTyped`
   *  rewriter from the typed call-site scope:
   *
   *   - `bindings`: every term-level name in scope at the call site.
   *   - `expectedType`: the source rendering of the `[T]` argument.
   *   - `enclosingSource`: the enclosing statement's source with this call's
   *     span replaced by [[EvalContext.placeholder]].
   *
   *  Direct callers (outside the REPL rewriter) can leave them at their defaults.
   */
  def eval[T](
      code: String,
      bindings: Array[Binding] = Array.empty[Binding],
      expectedType: String = "",
      enclosingSource: String = ""
  ): T =
    evalImpl[T](code, bindings, expectedType, enclosingSource)

  /** Generator form: the closure receives the call-site context and returns the
   *  body string. Convenience 1-arg overload plus the full form the rewriter
   *  emits. (Defaults can't be shared across overloads, so the string form owns
   *  them.) */
  def eval[T](gen: EvalContext => String): T =
    eval[T](gen, Array.empty[Binding], "", "")

  def eval[T](
      gen: EvalContext => String,
      bindings: Array[Binding],
      expectedType: String,
      enclosingSource: String
  ): T =
    val ctx = new EvalContext(enclosingSource, bindings)
    evalImpl[T](gen(ctx), bindings, expectedType, enclosingSource)

  /** Non-throwing variant: returns [[EvalResult]] with the body's value on
   *  success or the [[CompileFailure]] on a compile-time failure of *this* call.
   *  Runtime exceptions thrown by the body itself still propagate. */
  def evalSafe[T](
      code: String,
      bindings: Array[Binding] = Array.empty[Binding],
      expectedType: String = "",
      enclosingSource: String = ""
  ): EvalResult[T] =
    evalSafeImpl[T](code, bindings, expectedType, enclosingSource)

  def evalSafe[T](gen: EvalContext => String): EvalResult[T] =
    evalSafe[T](gen, Array.empty[Binding], "", "")

  def evalSafe[T](
      gen: EvalContext => String,
      bindings: Array[Binding],
      expectedType: String,
      enclosingSource: String
  ): EvalResult[T] =
    val ctx = new EvalContext(enclosingSource, bindings)
    evalSafeImpl[T](gen(ctx), bindings, expectedType, enclosingSource)

  // --- implementation --------------------------------------------------------

  private def evalImpl[T](
      code: String,
      bindings: Array[Binding],
      expectedType: String,
      enclosingSource: String
  ): T =
    evalSafeImpl[T](code, bindings, expectedType, enclosingSource).get

  private def evalSafeImpl[T](
      code: String,
      bindings: Array[Binding],
      expectedType: String,
      enclosingSource: String
  ): EvalResult[T] =
    // We do NOT catch here. Body runtime exceptions — which include a *nested*
    // eval's compile-time failure surfacing as `EvalCompileException` — propagate
    // to the caller. Only *this* call's own compile error (a `Left` from the
    // bridge) becomes an `EvalResult.Failure`.
    EvalBridge.evalCode(code, bindings, expectedType, enclosingSource) match
      case Right(v) => EvalResult.success(v.asInstanceOf[T])
      case Left(f)  => EvalResult.failure(f)

end Eval
