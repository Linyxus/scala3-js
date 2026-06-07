package scala.runtime.eval

import scala.annotation.StaticAnnotation

/** Marks a user-defined function as an eval-like generator. `EvalRewriteTyped`
 *  rewrites calls to the annotated function the same way it rewrites
 *  `Eval.eval`: it fills three synthetic parameter slots — `bindings`,
 *  `expectedType`, `enclosingSource` — matched by name. Other parameters (the
 *  user's task / config) are left untouched.
 *
 *  The annotated function must declare parameters named exactly
 *  `bindings: Array[Eval.Binding]`, `expectedType: String`, and
 *  `enclosingSource: String`, each with a default. A call must have either all
 *  three default-filled (rewriter populates them) or all three explicitly
 *  supplied (rewriter leaves them alone); mixed states are rejected.
 */
final class evalLike extends StaticAnnotation

/** Like [[evalLike]] but for the non-throwing variant: the annotated function's
 *  result type is `EvalResult[T]`, and the rewriter wraps the verify-marker in
 *  `Eval.handleCompileError(...)`. */
final class evalSafeLike extends StaticAnnotation
