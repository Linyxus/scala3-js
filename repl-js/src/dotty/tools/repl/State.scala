package dotty.tools
package repl

import dotc.ast.tpd
import dotc.core.Contexts.Context

/** The state of the REPL contains necessary bindings instead of having to have
 *  mutation
 *
 *  The compiler in the REPL needs to do some wrapping in order to compile
 *  valid code. This wrapping occurs when a single `MemberDef` that cannot be
 *  top-level needs to be compiled. In order to do this, we need some unique
 *  identifier for each of these wrappers. That identifier is `objectIndex`.
 *
 *  Free expressions such as `1 + 1` needs to have an assignment in order to be
 *  of use. These expressions are therefore given a identifier on the format
 *  `resX` where `X` starts at 0 and each new expression that needs an
 *  identifier is given the increment of the old identifier. This identifier is
 *  `valIndex`.
 *
 *  This is the same `State` the JVM REPL defines inside `ReplDriver.scala`;
 *  it lives in its own file here so the JS REPL can reuse `ReplCompiler` (which
 *  depends on `State`) without pulling in the JVM-only `ReplDriver`.
 *
 *  @param objectIndex the index of the next wrapper
 *  @param valIndex    the index of next value binding for free expressions
 *  @param imports     a map from object index to the list of user defined imports
 *  @param invalidObjectIndexes the set of object indexes that failed to initialize
 *  @param quiet       whether we print evaluation results
 *  @param context     the latest compiler context
 */
case class State(objectIndex: Int,
                 valIndex: Int,
                 imports: Map[Int, List[tpd.Import]],
                 invalidObjectIndexes: Set[Int],
                 quiet: Boolean,
                 context: Context):
  def validObjectIndexes = (1 to objectIndex).filterNot(invalidObjectIndexes.contains(_))
