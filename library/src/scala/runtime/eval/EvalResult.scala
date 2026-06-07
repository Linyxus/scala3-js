package scala.runtime.eval

/** The result of a non-throwing `evalSafe` call: either a successful value of
 *  type `T`, or an [[Eval.CompileFailure]] describing the compile-time failure
 *  of *this call* (not of any nested eval inside the body — those propagate as
 *  exceptions from `get` like any other body exception). Designed for agent /
 *  LLM workflows that feed the error text back into a generator and retry.
 *
 *  ```
 *  Eval.evalSafe[Int](code) match
 *    case EvalResult.Success(v) => use(v)
 *    case EvalResult.Failure(f) => regenerate(f.errors)
 *  ```
 */
enum EvalResult[+T]:
  case Success(value: T)
  case Failure(failure: Eval.CompileFailure) extends EvalResult[Nothing]

  def isSuccess: Boolean = this match
    case _: Success[?] => true
    case _: Failure    => false

  def isFailure: Boolean = !isSuccess

  /** The body's return value on success, or throws an [[EvalCompileException]]
   *  built from the stored [[Eval.CompileFailure]] on failure (matches the
   *  throwing `eval[T]` form). */
  def get: T = this match
    case Success(v) => v
    case Failure(f) => throw new EvalCompileException(f.errors, f.source)

  /** The compile-time failure on a failed result, or `null` on success. */
  def error: Eval.CompileFailure | Null = this match
    case _: Success[?] => null
    case Failure(f)    => f

  /** The body's return value on success, or `default` on failure. */
  def getOrElse[U >: T](default: U): U = this match
    case Success(v) => v
    case _: Failure => default

  override def toString: String = this match
    case Success(v) => s"EvalResult.Success($v)"
    case Failure(f) => s"EvalResult.Failure(${f.errors.length} error(s))"

object EvalResult:
  def success[T](value: T): EvalResult[T] = Success(value)
  def failure[T](error: Eval.CompileFailure): EvalResult[T] = Failure(error)
