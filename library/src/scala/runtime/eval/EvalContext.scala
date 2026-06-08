package scala.runtime.eval

/** Information about the call site passed to the closure form of [[Eval.eval]].
 *
 *  The agent / LLM use case: a generator inspects `enclosingSource` (the source
 *  of the enclosing top-level statement with this eval call's location replaced
 *  by `placeholder`), decides what code to fill in, and returns it as a String.
 *
 *  @param enclosingSource source of the enclosing statement with the eval call's
 *                         span replaced by `placeholder` (empty when the rewriter
 *                         couldn't compute a slice).
 *  @param bindings        the bindings the rewriter captured at the call site.
 */
final class EvalContext(
    val enclosingSource: String,
    val bindings: Array[Eval.Binding],
    private[eval] val loopState: LoopState | Null = null
):
  /** The string the rewriter substituted into `enclosingSource` at the eval
   *  call's location. Splice generated code with
   *  `enclosingSource.replace(placeholder, generated)`. */
  def placeholder: String = EvalContext.placeholder

  /** Number of lines attempted in the enclosing [[Eval.evalLoop]], or `0` for
   *  ordinary one-shot eval contexts. */
  def attempts: Int =
    if loopState == null then 0 else loopState.attempts

  /** History of lines attempted in the enclosing [[Eval.evalLoop]], oldest first. */
  def history: List[EvalAttempt] =
    if loopState == null then Nil else loopState.history

  /** Most recent compile error reported by `EvalSession.evalSafe`, if any. */
  def lastError: Option[Array[String]] =
    if loopState == null then None else loopState.lastError

  override def toString: String =
    s"EvalContext(enclosingSource=${enclosingSource.length} chars, bindings=${bindings.length})"

object EvalContext:
  /** The marker the rewriter substitutes into `enclosingSource` at each eval
   *  call site. Chosen to stay a single Scala identifier, valid in expression
   *  position, with no collision risk against ordinary user names. */
  val placeholder: String = "__evalBodyPlaceholder__"
