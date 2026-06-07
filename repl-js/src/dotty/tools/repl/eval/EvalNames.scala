package dotty.tools
package repl
package eval

import scala.runtime.eval.EvalContext

/** Marker text the eval pipeline splices into `enclosingSource` and later
 *  replaces with the (now known) eval body. `Marker` aliases
 *  [[scala.runtime.eval.EvalContext.placeholder]] so the slicing in
 *  [[EvalRewriteTyped]] and the wrapper-side helpers stay in sync. `emit` wraps
 *  the body in parens so the splice is valid in any expression position. */
private[repl] object EvalBodyPlaceholder:
  inline def Marker: String = EvalContext.placeholder
  def emit(body: String): String = s"({ $body })"

/** Method names the eval pipeline recognises as call sites it fills in.
 *  [[EvalRewriteTyped]] additionally restricts `eval` / `evalSafe` to symbols
 *  owned by the `Eval` module; `agent` / `agentSafe` are user-defined generators
 *  matched by name only. */
private[repl] object EvalNames:
  val EvalLike: Set[String] = Set("eval", "evalSafe", "agent", "agentSafe")
  val EvalOwned: Set[String] = Set("eval", "evalSafe")
  /** The non-throwing variants whose result is `EvalResult[T]`. */
  val EvalSafeLike: Set[String] = Set("evalSafe", "agentSafe")
