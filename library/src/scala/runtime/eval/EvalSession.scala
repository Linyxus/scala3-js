package scala.runtime.eval

import scala.language.experimental.captureChecking
import scala.util.boundary

/** Stateful dynamic-eval session used by [[Eval.evalLoop]]. */
@caps.assumeSafe
final class EvalSession[R] private[eval] (
    label: boundary.Label[R],
    ctxBindings: Array[Eval.Binding],
    ctxEnclosingSource: String
):
  private var env: Array[Eval.Binding] = Array.empty[Eval.Binding]
  private var envClasses: List[String] = Nil
  private var envValNames: List[Array[String]] = Nil
  private var envImports: List[Array[String]] = Nil
  private val loopState = new LoopState

  /** Context for the loop as a whole: the `evalLoop(...)` call site's surrounding
   *  source and captured locals (filled by the rewriter), plus the live
   *  [[LoopState]] backing `attempts`/`history`/`lastError`. Each `s.eval` line
   *  separately captures its own call-site context. */
  val ctx: EvalContext = new EvalContext(ctxEnclosingSource, ctxBindings, loopState)

  def complete(v: R): Nothing =
    boundary.break(v)(using label)

  @evalLike
  def eval[T](
      code: String,
      bindings: Array[Eval.Binding] = Array.empty[Eval.Binding],
      expectedType: String = "",
      enclosingSource: String = ""
  ): T =
    evalSafeImpl[T](code, bindings, expectedType, enclosingSource).get

  @evalSafeLike
  def evalSafe[T](
      code: String,
      bindings: Array[Eval.Binding] = Array.empty[Eval.Binding],
      expectedType: String = "",
      enclosingSource: String = ""
  ): EvalResult[T] =
    evalSafeImpl[T](code, bindings, expectedType, enclosingSource)

  private def evalSafeImpl[T](
      code: String,
      bindings: Array[Eval.Binding],
      expectedType: String,
      enclosingSource: String
  ): EvalResult[T] =
    EvalBridge.evalSessionLine(
      code,
      env ++ bindings,
      expectedType,
      enclosingSource,
      env.map(_.name),
      envClasses.toArray,
      shadowingAliases(bindings.iterator.map(_.name).toSet),
      envImports.toArray
    ) match
      case Right((value, instance, className, valNames, imports)) =>
        loopState.recordSuccess(code, value)
        env = env :+ Eval.bind("__line" + (env.length + 1), instance)
        envClasses = envClasses :+ className
        envValNames = envValNames :+ valNames
        envImports = envImports :+ imports
        EvalResult.success(value.asInstanceOf[T])
      case Left(failure) =>
        loopState.recordFailure(code, failure.errors)
        EvalResult.failure(failure)

  /** Per prior line (parallel to `env`), the accumulated `val` names that line
   *  should re-export as an explicit `val name = __lineK.name` alias in the next
   *  line's import preamble.
   *
   *  Every prior line is brought in with `import __lineK.{given, *}`; when the
   *  same name is defined by more than one line those wildcard imports are
   *  ambiguous, and a same-named call-site local would otherwise out-rank them.
   *  An alias is a *definition*, which out-ranks every wildcard import, so
   *  emitting one at the line that defines a name *last* pins the reference to
   *  the most recent definition — the locked "later wins" shadowing rule. We only
   *  alias names that actually need it: those defined by two or more lines, or
   *  shadowing a `capturedName` (a call-site local captured for this call). */
  private def shadowingAliases(capturedNames: Set[String]): Array[Array[String]] =
    val lastDefiner = scala.collection.mutable.HashMap.empty[String, Int]
    val duplicated = scala.collection.mutable.HashSet.empty[String]
    for (names, idx) <- envValNames.iterator.zipWithIndex; name <- names do
      if lastDefiner.contains(name) then duplicated += name
      lastDefiner(name) = idx
    envValNames.iterator.zipWithIndex.map { (names, idx) =>
      names.filter(n => lastDefiner(n) == idx && (duplicated(n) || capturedNames(n)))
    }.toArray
