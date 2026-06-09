package scala.runtime.eval

// Enables `@caps.assumeSafe` below (it is `@experimental`, exempted under the
// cc import) — same pattern as `object Eval`.
import scala.language.experimental.captureChecking

/** A minimal stateful REPL built purely on the one-shot [[Eval.eval]] chaining
 *  mechanism — no compiler or driver support beyond what `eval` already has.
 *
 *  {{{
 *  val r = simpleRepl[Int] { s =>
 *    s.eval("val x = 10")                 // statement line: definitions, no value
 *    s.eval("val y = x + 5")
 *    s.complete(s.eval[Int]("x + y"))     // value line: 25
 *  }
 *  }}}
 *
 *  How it works: the session carries `(bindings, enclosingSource)` — the same
 *  pair the `@evalLike` rewriter fills for any eval call. Each `s.eval(code)`
 *  appends one sentinel statement to `code` and runs it as a one-shot eval
 *  against the current session context:
 *
 *  {{{
 *  <code>
 *  SimpleRepl.endLine[T]()      // fully qualified; [T] when the call gave one
 *  }}}
 *
 *  A statement always parses, whatever shape `code` has. The eval compile's
 *  parser-stage phase (`SpliceEvalBody.shapeSimpleReplLine`) then rewrites the
 *  parsed block into the real tail — REPL line semantics decided on the tree,
 *  not by string surgery:
 *
 *  {{{
 *  <stats minus a trailing expression>
 *  val __simpleReplLineValue__[: T] = <trailing expression | ()>
 *  throw new SimpleReplEnd(SimpleRepl.captureState(), __simpleReplLineValue__)
 *  }}}
 *
 *  So a line is any mix of definitions and statements; its value is its
 *  trailing expression (typed against `T` when given — a definition-only line
 *  with a non-`Unit` `T` is a compile error) or `()`.
 *
 *  `captureState()` is `@evalLike`: the rewriter (running inside the eval
 *  compile) fills its `bindings` with every term in scope at that point — the
 *  line's own definitions included — and its `enclosingSource` with the
 *  composed chain, whose marker replaces the sentinel text, inside the line's
 *  scope. The next line is therefore compiled lexically *nested* in this one:
 *  definitions accumulate, and shadowing is plain inner-scope shadowing. The
 *  `throw` makes the spliced body type as `Nothing`, which conforms to
 *  whatever type the marker slot expects — so a line works no matter what
 *  type its splice slot demanded (`T` deliberately does not ride on
 *  `Eval.eval`'s `expectedType`, which would break that conformance).
 *
 *  Being built on bindings + source re-elaboration, this is a *term-level*
 *  session: `val`/`var`/`def`/`given`/`import` accumulate fully (`var`s stay
 *  live through the `VarRef` facade). Classes defined in a line work within
 *  that line but do not accumulate reliably across lines — use the
 *  wrapper-based [[Eval.embedRepl]] for that. Locals of the
 *  `simpleRepl { s => ... }` lambda itself are not visible inside lines (the
 *  chain is rooted at the `simpleRepl` call site); locals of the *enclosing
 *  statement* are. The one exception is the session handle itself: every
 *  call-site name bound to it is grafted into the chain on first use
 *  ([[SimpleReplSession.injectSelfNames]]), so lines can drive their own
 *  session — `s.eval("s.complete(x + 1)")` completes it from inside a line.
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
    val session = new SimpleReplSession[R](bindings, rootEnclosing, expectedType)
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

  /** Line-tail sentinel appended by [[SimpleReplSession.eval]] and replaced at
   *  parse time by `SpliceEvalBody`'s session-line shaping (the two must agree
   *  on this fully-qualified name). If it survives to runtime the line could
   *  not be shaped — its code ends in an unfinished construct that swallowed
   *  the appended sentinel. */
  def endLine[T](): Nothing =
    throw new IllegalStateException(
      "simpleRepl: the line tail was not shaped — "
        + "the line's code likely ends in an unfinished construct")

end SimpleRepl

/** Carrier for one captured session step: the bindings and the chain source
 *  whose marker is the next line's splice point. Constructed by
 *  [[SimpleRepl.captureState]] inside eval'd code. */
final class SimpleReplState(
    val bindings: Array[Eval.Binding],
    val enclosingSource: String
)

/** Control-flow carrier for a finished session line: the new state plus the
 *  line's value (`()` for statement lines). Constructed by generated code, so
 *  the constructor must stay public. Stack trace suppressed — this fires on
 *  every line. */
final class SimpleReplEnd(val state: SimpleReplState, val value: Any)
    extends Throwable(null, null, false, false)

/** Control-flow carrier for [[SimpleReplSession.complete]]. `owner` identifies
 *  the session so nested `simpleRepl`s route completion to the right boundary.
 *  Stack trace suppressed. */
final class SimpleReplComplete(val owner: AnyRef, val value: Any)
    extends Throwable(null, null, false, false)

/** The handle passed to the `simpleRepl` body: a mutable cursor over the
 *  session's eval context. */
@caps.assumeSafe
final class SimpleReplSession[R] private[eval] (
    initialBindings: Array[Eval.Binding],
    initialEnclosingSource: String,
    /** Source rendering of the session's result type `R`, as filled by the
     *  rewriter at the `simpleRepl` call site ("" when unknown). */
    val expectedType: String
):
  private var myBindings: Array[Eval.Binding] = initialBindings
  private var myEnclosingSource: String = initialEnclosingSource
  private val injectedSelfNames = scala.collection.mutable.HashSet.empty[String]

  /** The bindings currently in scope for the next line: the `simpleRepl` call
   *  site's captures plus everything session lines have defined so far. */
  def bindings: Array[Eval.Binding] = myBindings

  /** The session's current chain source; its marker is the splice point where
   *  the next line will be compiled. */
  def enclosingSource: String = myEnclosingSource

  /** Source rendering of this session's own type, for the chain-injected
   *  declarations of the handle ([[injectSelfNames]]). */
  private def selfTypeSource: String =
    val r = if expectedType.isEmpty then "?" else expectedType
    s"_root_.scala.runtime.eval.SimpleReplSession[$r]"

  /** Make this session handle visible inside lines, under every name the
   *  *call site* knows it by.
   *
   *  The chain is rooted at the `simpleRepl` call site, where the marker
   *  replaced the whole call — lambda included — so the body's handle name
   *  (e.g. the `ctx` of `simpleRepl { ctx => ... }`) is neither in the chain
   *  source nor in the root bindings. But `eval`'s own `@evalLike` fill
   *  captures the locals in scope at the `ctx.eval(...)` call, under their
   *  source names — including the handle itself. Each capture whose value
   *  *is* this session (by identity) is grafted into the chain as a
   *  typing-only declaration (the [[Eval.sessionPlaceholder]] pattern; the
   *  rhs never runs) backed by a real binding, so lines can use the handle:
   *  `ctx.complete(...)`, `ctx.eval(...)`, `ctx.enclosingSource`, … */
  private def injectSelfNames(callBindings: Array[Eval.Binding]): Unit =
    var i = 0
    while i < callBindings.length do
      val b = callBindings(i)
      val v = b.value match
        case ref: Eval.VarRef[?] => ref.get()
        case other => other
      if (v.asInstanceOf[AnyRef] eq this) && !injectedSelfNames.contains(b.name) then
        injectedSelfNames += b.name
        myEnclosingSource = myEnclosingSource.replace(
          EvalContext.placeholder,
          s"""{
             |val ${b.name}: $selfTypeSource = _root_.scala.runtime.eval.Eval.sessionPlaceholder[$selfTypeSource]
             |${EvalContext.placeholder}
             |}""".stripMargin)
        myBindings = myBindings :+ Eval.bind(b.name, this)
      i += 1

  /** Run one session line against the current session context: REPL
   *  semantics, any mix of definitions and statements. Returns the line's
   *  trailing expression's value — typed against `T` when a type argument is
   *  given, cast unchecked like `Eval.eval` — or `()` for a definition-only
   *  line (with a non-`Unit` `T` that mismatch is a compile error of the
   *  line). On success the session advances to include the line's
   *  definitions; on a compile error (`EvalCompileException`) or an exception
   *  thrown by the line itself, the session is unchanged and the exception
   *  propagates.
   *
   *  The appended `endLine[T]()` sentinel is a plain statement (so the line
   *  needs no particular shape to parse); `SpliceEvalBody` rewrites the parsed
   *  block into the real tail, naming the trailing expression. The rendered
   *  `T` rides as the sentinel's type argument rather than as `Eval.eval`'s
   *  `expectedType` so the splice block stays `Nothing`-typed — the chain's
   *  slot types must conform at every level.
   *
   *  `@evalLike` so the rewriter fills `expectedType` (the rendered `T`) and
   *  `bindings` (the call site's locals — used only to discover names for the
   *  session handle itself, see [[injectSelfNames]]). The filled
   *  `enclosingSource` is intentionally unused — a session line runs in the
   *  *session's* accumulated context, not this call site's. */
  @evalLike
  def eval[T](
      code: String,
      bindings: Array[Eval.Binding] = Array.empty[Eval.Binding],
      expectedType: String = "",
      enclosingSource: String = ""
  ): T =
    injectSelfNames(bindings)
    val targ = if expectedType.isEmpty then "" else s"[$expectedType]"
    try
      Eval.eval[Any](
        code + "\n_root_.scala.runtime.eval.SimpleRepl.endLine" + targ + "()",
        myBindings, "", myEnclosingSource)
      throw new IllegalStateException(
        "simpleRepl: line finished without reaching the session tail")
    catch
      case e: SimpleReplEnd =>
        myBindings = e.state.bindings
        myEnclosingSource = e.state.enclosingSource
        e.value.asInstanceOf[T]

  /** Exit the session: unwinds to the matching `simpleRepl` boundary, which
   *  returns `v`. The accumulated state is discarded. */
  def complete(v: R): Nothing = throw new SimpleReplComplete(this, v)

end SimpleReplSession
