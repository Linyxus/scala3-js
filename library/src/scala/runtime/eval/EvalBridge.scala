package scala.runtime.eval

/** Indirection between [[Eval]] (which runs inside the sjsir interpreter) and the
 *  REPL driver (native linked JS) that owns the compiler.
 *
 *  This `library/src` version is a stub: it only needs to *compile* for the JVM
 *  stdlib build (used as the eval compile classpath). The real implementation is
 *  the `library-js/src` override, which reaches the driver through the JS-global
 *  `globalThis.__replEval` bridge the session installs. `eval` is only ever run
 *  inside the Scala.js REPL, so this stub is never executed there.
 */
object EvalBridge:
  def evalCode(
      code: String,
      bindings: Array[Eval.Binding],
      expectedType: String,
      enclosingSource: String
  ): Either[Eval.CompileFailure, Any] =
    throw new IllegalStateException(
      "scala.runtime.eval.Eval requires an active Scala.js REPL session")
