package dotty.tools
package repl
package eval

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Names.*

import scala.collection.mutable
import scala.runtime.eval.EvalContext

/** Configuration for one compile-and-run of an eval body through the eval
 *  pipeline. `outputClassName` must be unique per compile so different calls
 *  don't collide in the interpreter's class registry. */
private[repl] case class EvalCompilerConfig(
    packageName: String = "",
    outputClassName: String = "",
    body: String = "",
    marker: String = EvalContext.placeholder,
    errorReporter: String => Unit = (_: String) => (),
    testMode: Boolean = false,
    expectedType: String = "",
    /** Names (with isVar) of bindings already captured by the outer eval call;
     *  seeded into the nested-eval rewriter in [[SpliceEvalBody]]. */
    initialScope: Array[(String, Boolean)] = Array.empty,
    /** The outer eval's `enclosingSource` slice (with its own marker). When
     *  non-empty, [[SpliceEvalBody]] activates nested mode so each inner eval
     *  gets a composed `enclosingSource` chained off this one. */
    outerEnclosingSource: String = "",
    sessionLine: Boolean = false,
    sessionValNames: mutable.ListBuffer[String] = mutable.ListBuffer.empty[String],
    sessionImportStrings: mutable.ListBuffer[String] = mutable.ListBuffer.empty[String],
    evalLogDir: String = "",
    evalLogTimestamp: String = ""
):
  val expressionClassName: TypeName = typeName(outputClassName)

  def expressionClass(using Context): ClassSymbol =
    if packageName.isEmpty then requiredClass(outputClassName)
    else requiredClass(s"$packageName.$outputClassName")

  def evaluateMethod(using Context): Symbol =
    expressionClass.info.decl(termName("evaluate")).symbol
