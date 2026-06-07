package dotty.tools
package repl
package eval

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.reporting.AbstractReporter
import dotty.tools.dotc.reporting.Diagnostic

/** Reporter that forwards each compile error as a string to the eval driver's
 *  callback. Warnings are dropped — the interesting feedback for an eval call is
 *  whether it compiles. */
private[repl] class EvalReporter(reportError: String => Unit) extends AbstractReporter:
  override def doReport(dia: Diagnostic)(using Context): Unit =
    dia match
      case error: Diagnostic.Error =>
        val newPos = error.pos.source.positionInUltimateSource(error.pos)
        val errorWithNewPos = new Diagnostic.Error(error.msg, newPos)
        reportError(stripColor(messageAndPos(errorWithNewPos)))
      case _ => ()
