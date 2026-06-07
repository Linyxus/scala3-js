package dotty.tools
package repl
package eval

import dotc.Compiler
import dotc.cc.CheckCaptures
import dotc.core.Phases.Phase

/** Compiler subclass that drives the eval pipeline. On top of the standard
 *  frontend / transform chain it inserts:
 *
 *   - [[SpliceEvalBody]] (after parser): parse the body string, splice it into
 *     `enclosingSource` at the marker, append the synthesised `__Expression`.
 *   - [[EvalRewriteTyped]] (after PostTyper): fill nested eval calls' synthetic
 *     args (chained `enclosingSource`).
 *   - [[ExtractEvalBody]] (after `cc`, or after typer when cc is off): move the
 *     typed body into `__Expression.evaluate` and rewrite outer refs to
 *     `reflectEval` placeholders.
 *   - [[ResolveEvalAccess]] (post-erasure): lower each placeholder to a concrete
 *     accessor call.
 *
 *  This drives the same Scala.js backend (`GenSJSIR`) as the REPL line compiler,
 *  because the inner-compile context carries `-scalajs`. The per-invocation log
 *  phase is deferred (task #10). */
class EvalCompiler(config: EvalCompilerConfig) extends Compiler:

  override protected def frontendPhases: List[List[Phase]] =
    val parser :: others = super.frontendPhases: @unchecked
    parser :: List(SpliceEvalBody(config)) :: (others :+ List(new EvalRewriteTyped(Some(config))))

  override protected def transformPhases: List[List[Phase]] =
    val store = EvalStore()
    val transformPhases = super.transformPhases
    val ccIndex = transformPhases.indexWhere(_.exists(_.phaseName == CheckCaptures.name))
    val anchor = if ccIndex >= 0 then ccIndex else 0
    val (before, after) = transformPhases.splitAt(anchor + 1)
    val resolveGroup = List(ResolveEvalAccess(config, store))
    (before :+ List(ExtractEvalBody(config, store))) ++ (after :+ resolveGroup)
