package scala.runtime.eval

/** One attempted line in an [[Eval.embedRepl]] session.
 *
 *  `result` is `Right(renderedValue)` on success and `Left(errors)` on a
 *  compile-time failure of that line. Runtime exceptions propagate and are not
 *  recorded as completed attempts.
 */
final case class EvalAttempt(code: String, result: Either[Array[String], String])

private[eval] final class EmbedReplState:
  private var historyRev: List[EvalAttempt] = Nil
  private var lastError0: Option[Array[String]] = None

  def attempts: Int = historyRev.length
  def history: List[EvalAttempt] = historyRev.reverse
  def lastError: Option[Array[String]] = lastError0

  def recordSuccess(code: String, value: Any): Unit =
    lastError0 = None
    historyRev = EvalAttempt(code, Right(String.valueOf(value))) :: historyRev

  def recordFailure(code: String, errors: Array[String]): Unit =
    lastError0 = Some(errors)
    historyRev = EvalAttempt(code, Left(errors)) :: historyRev
