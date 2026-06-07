package dotty.tools
package repl
package eval

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Contexts.*

/** Per-compile state shared between [[ExtractEvalBody]] and [[ResolveEvalAccess]].
 *  Populated by Extract once it finds the spliced `val __evalResult`:
 *   - `symbol`: the val symbol; owner-chain anchor for classifying body-local vs
 *     outer references.
 *   - `classOwners`: enclosing classes innermost-out; Resolve walks this to lower
 *     `This(C)` / `Outer(_, C)` placeholders. */
private[eval] class EvalStore:
  var symbol: TermSymbol | Null = null
  var classOwners: Seq[ClassSymbol] = Seq.empty

  def store(exprSym: Symbol)(using Context): Unit =
    symbol = exprSym.asTerm
    classOwners = exprSym.ownersIterator.collect { case cls: ClassSymbol => cls }.toSeq
