package scala.runtime.eval

import scala.scalajs.js

/** Scala.js override of [[EvalBridge]]: reaches the REPL driver (which owns the
 *  dotc-on-JS compiler) through the JS-global `globalThis.__replEval` function
 *  the session installs.
 *
 *  Protocol: `__replEval(code, bindings, expectedType, enclosingSource)` returns
 *  a JS object `{ ok: true, value }` on success or
 *  `{ ok: false, errors: string[], source }` on a compile failure of *this*
 *  call. The `bindings` array and the success `value` are interpreter values
 *  that round-trip through the driver opaquely (one shared heap). Body runtime
 *  exceptions propagate as thrown exceptions, never as `ok:false`.
 */
object EvalBridge:
  def evalCode(
      code: String,
      bindings: Array[Eval.Binding],
      expectedType: String,
      enclosingSource: String
  ): Either[Eval.CompileFailure, Any] =
    val fn = js.Dynamic.global.globalThis.__replEval
    if js.isUndefined(fn) then
      throw new IllegalStateException(
        "eval(...) requires an active Scala.js REPL session (no __replEval bridge installed)")
    val res = fn(
      code.asInstanceOf[js.Any],
      bindings.asInstanceOf[js.Any],
      expectedType.asInstanceOf[js.Any],
      enclosingSource.asInstanceOf[js.Any]
    ).asInstanceOf[js.Dynamic]
    if res.ok.asInstanceOf[Boolean] then
      Right(res.value.asInstanceOf[Any])
    else
      val errors = res.errors.asInstanceOf[js.Array[String]].toArray
      val source = res.source.asInstanceOf[String]
      Left(new Eval.CompileFailure(errors, source))
