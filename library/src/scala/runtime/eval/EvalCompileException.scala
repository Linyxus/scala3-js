package scala.runtime.eval

/** Thrown by the throwing-form `eval[T]` when the inner compile of the
 *  synthesised wrapper fails (unknown identifier, type mismatch against the
 *  pinned `T`, parse error, …). Carries diagnostics and generated source as
 *  structured fields so callers can inspect them programmatically.
 *
 *  Body runtime exceptions (e.g. `eval("1 / 0")` raising `ArithmeticException`)
 *  propagate as the body's own exception, not as `EvalCompileException`.
 */
final class EvalCompileException(
    val errors: Array[String],
    val generatedSource: String
) extends RuntimeException(EvalCompileException.formatMessage(errors, generatedSource))

object EvalCompileException:
  private def formatMessage(errors: Array[String], generatedSource: String): String =
    val sb = new StringBuilder
    var i = 0
    while i < errors.length do
      if i > 0 then sb.append('\n')
      sb.append(errors(i))
      i += 1
    s"eval failed to compile:\n${sb.toString}\n\nGenerated source:\n$generatedSource"
