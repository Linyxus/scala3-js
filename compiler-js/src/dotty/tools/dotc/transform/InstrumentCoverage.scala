package dotty.tools.dotc
package transform

import core.Contexts.Context
import core.DenotTransformers.IdentityDenotTransformer
import core.Flags.*
import core.Constants.Constant
import core.Symbols.{Symbol, defn}
import core.Decorators.*
import util.Property
import ast.tpd.*

/** Stub InstrumentCoverage for Scala.js — coverage instrumentation not supported. */
class InstrumentCoverage extends MacroTransform with IdentityDenotTransformer:
  override def phaseName = InstrumentCoverage.name
  override def description = InstrumentCoverage.description
  override def isEnabled(using ctx: Context) = false
  override protected def newTransformer(using Context) = new Transformer:
    override def transform(tree: ast.tpd.Tree)(using Context) = tree

object InstrumentCoverage:
  val name: String = "instrumentCoverage"
  val description: String = "instrument code for coverage checking"

  /** Coverage probes are synthetic bookkeeping calls that should be transparent to
   *  later warning logic and should not steal source positions from the user tree
   *  they wrap.
   */
  def isCoverageProbe(tree: Tree)(using Context): Boolean = tree match
    case Apply(fun, Literal(Constant(_: Int)) :: Literal(Constant(_: String)) :: Nil) =>
      fun.symbol == defn.InvokedMethodRef.symbol
    case _ =>
      false

  /** Remove leading synthetic coverage wrappers to recover the user-written tree. */
  def stripLeadingCoverage(tree: Tree)(using Context): Tree = tree match
    case Typed(expr, _) =>
      stripLeadingCoverage(expr)
    case Inlined(_, Nil, expr) =>
      stripLeadingCoverage(expr)
    case Block(stats, expr) if stats.forall(isCoverageProbe) =>
      stripLeadingCoverage(expr)
    case _ =>
      tree

/** Stub of upstream `transform.LiftCoverage`, referenced by the shared `cc.SepCheck`.
 *  Coverage lifting never runs on Scala.js (InstrumentCoverage is disabled), so no
 *  tree ever carries the `CoverageLiftedTemp` attachment and this always returns false. */
object LiftCoverage:
  val CoverageLiftedTemp = Property.StickyKey[Unit]()

  def isCoverageLiftedTemp(sym: Symbol)(using Context): Boolean =
    sym.defTree.hasAttachment(CoverageLiftedTemp)
