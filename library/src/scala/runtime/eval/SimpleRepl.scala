package scala.runtime.eval

// Enables `@caps.assumeSafe` below (it is `@experimental`, exempted under the
// cc import) — same pattern as `object Eval`.
import scala.language.experimental.captureChecking

/** A minimal stateful REPL built purely on the one-shot [[Eval.eval]] chaining
 *  mechanism — no compiler or driver support beyond what `eval` already has.
 *
 *  {{{
 *  import scala.runtime.eval.SimpleRepl.*
 *
 *  val r = simpleRepl[Int] { s =>
 *    s.eval("val x = 10; ()")
 *    s.eval("val y = x + 5; ()")
 *    s.complete(s.eval[Int]("x + y"))   // 25
 *  }
 *  }}}
 *
 *  How it works: the session state is `(bindings, enclosingSource)` — the same
 *  pair the `@evalLike` rewriter fills for any eval call. Each `s.eval(code)`
 *  appends a tail to `code` and runs it as a one-shot eval against the current
 *  state:
 *
 *  {{{
 *  <code>
 *  match { case __v => throw new SimpleReplEnd(SimpleRepl.captureState(), __v) }
 *  }}}
 *
 *   - `match` cannot start a statement, so the line continues the code's
 *     trailing expression — and unlike `.match` (a selection, which would bind
 *     to the last operand: `x + y.match`), it consumes the whole infix
 *     expression. `__v` is the line's value — no parsing of `code` needed.
 *   - `captureState()` is itself `@evalLike`: the rewriter (running inside the
 *     eval compile) fills its `bindings` with every term in scope at that point
 *     — the line's own definitions included — and its `enclosingSource` with the
 *     composed chain, whose marker sits exactly there, inside the line's scope.
 *     The next line is therefore compiled lexically *nested* in this one:
 *     definitions accumulate, and shadowing is plain inner-scope shadowing.
 *   - The `throw` makes the spliced body type as `Nothing`, which conforms to
 *     whatever type the marker slot expects — so the first line works no matter
 *     what type the `simpleRepl` call site demanded, and every later slot (the
 *     `SimpleReplEnd` constructor argument) is uniform.
 *
 *  The line contract: **a line must end with a simple expression** (its value
 *  becomes the `eval` result). End definition-only lines with `; ()`, and bind
 *  a trailing `if`/`match` to a `val` first (the glue would consume only its
 *  last branch):
 *
 *   - `s.eval("val x = 10; ()")` — correct.
 *   - `s.eval("def f = 1")` / `s.eval("class C")` — fails loudly (the glue can't
 *     attach, or the tail is never reached and `eval` reports it).
 *   - `s.eval("val x = 10")` — the one *silent* trap: the glue merges into the
 *     initializer, so `x` is NOT accumulated and the rhs value is returned.
 *
 *  Being built on bindings + source re-elaboration, this is a *term-level*
 *  session: `val`/`var`/`def`/`given`/`import` accumulate fully (`var`s stay
 *  live through the `VarRef` facade). Classes defined in a line work within
 *  that line but do not accumulate reliably across lines — use the wrapper-based
 *  [[Eval.embedRepl]] for that. Locals of the `simpleRepl { s => ... }` lambda
 *  itself are not visible inside lines (the chain is rooted at the `simpleRepl`
 *  call site); locals of the *enclosing statement* are.
 */
object SimpleRepl:

  /** Open a session. The body must exit through `s.complete(value)`; the
   *  defaulted parameters are filled by the `@evalLike` rewriter from the call
   *  site and become the root of the session's chain.
   *
   *  The root marker is ascribed with the rendered `R` (`((<marker>): R)`):
   *  every spliced line types as `Nothing` (the tail throws), and without the
   *  ascription that `Nothing` would infect later uses of the call's result in
   *  the same statement during a line's verify compile (e.g.
   *  `val n = simpleRepl[Int] {…}; n + 1`). */
  @evalLike
  def simpleRepl[R](
      body: SimpleReplSession[R] => Nothing,
      bindings: Array[Eval.Binding] = Array.empty[Eval.Binding],
      expectedType: String = "",
      enclosingSource: String = ""
  ): R =
    val rootEnclosing =
      if expectedType.isEmpty then enclosingSource
      else
        enclosingSource.replace(
          EvalContext.placeholder,
          s"((${EvalContext.placeholder}): $expectedType)")
    val session = new SimpleReplSession[R](new SimpleReplState(bindings, rootEnclosing))
    try body(session)
    catch
      case e: SimpleReplComplete if e.owner eq session => e.value.asInstanceOf[R]

  /** Session-tail hook: re-packages the current scope as the next state. Only
   *  meaningful inside an eval compile, where the rewriter fills `bindings`
   *  with the terms in scope at the call and `enclosingSource` with the
   *  composed chain (marker at this call's span). `expectedType` is required
   *  by the `@evalLike` contract but unused. */
  @evalLike
  def captureState(
      bindings: Array[Eval.Binding] = Array.empty[Eval.Binding],
      expectedType: String = "",
      enclosingSource: String = ""
  ): SimpleReplState =
    new SimpleReplState(bindings, enclosingSource)

end SimpleRepl

/** Immutable session state: the captured bindings and the chain source whose
 *  marker is the next line's splice point. */
final class SimpleReplState(
    val bindings: Array[Eval.Binding],
    val enclosingSource: String
)

/** Control-flow carrier for a finished session line: the new state plus the
 *  line's trailing-expression value. Constructed by generated code, so the
 *  constructor must stay public. Stack trace suppressed — this fires on every
 *  line. */
final class SimpleReplEnd(val state: SimpleReplState, val value: Any)
    extends Throwable(null, null, false, false)

/** Control-flow carrier for [[SimpleReplSession.complete]]. `owner` identifies
 *  the session so nested `simpleRepl`s route completion to the right boundary.
 *  Stack trace suppressed. */
final class SimpleReplComplete(val owner: AnyRef, val value: Any)
    extends Throwable(null, null, false, false)

/** The handle passed to the `simpleRepl` body: a mutable cursor over an
 *  immutable [[SimpleReplState]]. */
@caps.assumeSafe
final class SimpleReplSession[R] private[eval] (initial: SimpleReplState):
  private var state: SimpleReplState = initial

  /** Run one session line against the current state and return its trailing
   *  expression's value (cast to `T` — unchecked, like `Eval.eval`). On
   *  success the session state advances to include the line's definitions; on
   *  a compile error (`EvalCompileException`) or an exception thrown by the
   *  line itself, the state is unchanged and the exception propagates.
   *
   *  `@evalLike` so the rewriter recognises the call (avoiding the "resolves
   *  to …, leaving this call alone" warning the `eval` name would otherwise
   *  trigger). The filled parameters are intentionally unused: a session line
   *  runs in the *session's* accumulated context, not this call site's. */
  @evalLike
  def eval[T](
      code: String,
      bindings: Array[Eval.Binding] = Array.empty[Eval.Binding],
      expectedType: String = "",
      enclosingSource: String = ""
  ): T =
    try
      Eval.eval[Any](
        code
          + "\nmatch { case __v => throw new _root_.scala.runtime.eval.SimpleReplEnd("
          + "_root_.scala.runtime.eval.SimpleRepl.captureState(), __v) }",
        state.bindings, "", state.enclosingSource)
      throw new IllegalStateException(
        "simpleRepl: line finished without reaching the session tail — "
          + "does it end with a definition? End the line with an expression, e.g. `; ()`.")
    catch
      case e: SimpleReplEnd =>
        state = e.state
        e.value.asInstanceOf[T]

  /** Exit the session: unwinds to the matching `simpleRepl` boundary, which
   *  returns `v`. The accumulated state is discarded. */
  def complete(v: R): Nothing = throw new SimpleReplComplete(this, v)

end SimpleReplSession
