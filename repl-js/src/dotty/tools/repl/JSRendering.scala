package dotty.tools
package repl

import dotc.*, core.*
import Contexts.*, Denotations.*, Flags.*, NameOps.*, StdNames.*, Symbols.*
import printing.ReplPrinter
import reporting.Diagnostic

/** The JS analogue of the JVM REPL's `Rendering`.
 *
 *  The JVM `Rendering` "crosses the 4th wall" with class loaders + reflection +
 *  pprint to fetch and pretty-print a binding's runtime value. Here the value
 *  is rendered *inside* the interpreter (by `scala.runtime.ReplRenderer`, called
 *  from the generated wrapper) and shipped back to the driver through a JS-global
 *  array; the driver hands us that `name -> rendered` map via [[withRenders]].
 *
 *  Everything else — the declaration text (`val x: Int`, `def f: Int`,
 *  `// defined class A`, …) — is pure compiler output produced by `ReplPrinter`,
 *  identical to the JVM REPL.
 */
private[repl] class JSRendering:
  import JSRendering.*

  /** `name -> rendered value` captured from the last wrapper run. */
  private var renders: Map[String, String] = Map.empty

  def withRenders(m: Map[String, String]): Unit = renders = m

  private def truncate(str: String, maxPrintCharacters: Int): String =
    val ncp = str.codePointCount(0, str.length)
    if ncp <= maxPrintCharacters then str
    else str.substring(0, str.offsetByCodePoints(0, maxPrintCharacters - 1))

  /** Look up a binding's rendered value, applying the same filtering the JVM
   *  REPL applies in `valueOf`: methods always show; a non-method whose type is
   *  `Unit` shows nothing (so `println(...)` doesn't print `val resN: Unit`). */
  private def valueOf(sym: Symbol)(using Context): Option[String] =
    renders.get(sym.name.show)
      .filter(_ => sym.is(Flags.Method) || sym.info != defn.UnitType)
      .map(stripReplPrefix)
      .map(truncate(_, ctx.settings.XreplPrintHeight.value.max(ctx.settings.pageWidth.value)))

  def renderTypeDef(d: Denotation)(using Context): Diagnostic =
    infoDiagnostic("// defined " ++ d.symbol.showUser, d)

  def renderTypeAlias(d: Denotation)(using Context): Diagnostic =
    infoDiagnostic("// defined alias " ++ d.symbol.showUser, d)

  /** Render method definition result */
  def renderMethod(d: Denotation)(using Context): Diagnostic =
    infoDiagnostic(d.symbol.showUser, d)

  /** Render value definition result. Returns `Left` if the wrapper failed to
   *  initialize for this binding (so the caller can mark the object invalid). */
  def renderVal(d: Denotation)(using Context): Either[Throwable, Option[Diagnostic]] =
    val dcl = d.symbol.showUser
    def msg(s: String) = infoDiagnostic(s, d)
    try
      Right(
        if d.symbol.is(Flags.Lazy) then Some(msg(dcl))
        else valueOf(d.symbol).map(value => msg(dcl + " = " + value))
      )
    catch case e: Throwable => Left(e)

  /** Render a runtime error raised while initializing the wrapper. */
  def renderError(thr: Throwable, d: Denotation)(using Context): Diagnostic =
    val msg = Option(thr.getMessage).getOrElse(thr.getClass.getName)
    infoDiagnostic(msg, d)

  private def infoDiagnostic(msg: String, d: Denotation)(using Context): Diagnostic =
    new Diagnostic.Info(msg, d.symbol.sourcePos)

object JSRendering:
  final val REPL_WRAPPER_NAME_PREFIX = str.REPL_SESSION_LINE

  extension (s: Symbol)
    def showUser(using Context): String =
      val printer = new ReplPrinter(ctx)
      val text = printer.dclText(s)
      text.mkString(ctx.settings.pageWidth.value)

  /** Strip a leading `rs$line$N` wrapper prefix from a rendered value (e.g. the
   *  default `toString` of a REPL-defined object/class). */
  private def stripReplPrefix(s: String): String =
    if s.startsWith(REPL_WRAPPER_NAME_PREFIX) then
      val prefixLen = REPL_WRAPPER_NAME_PREFIX.length
      val dropLen = prefixLen + s.drop(prefixLen).takeWhile(c => c.isDigit || c == '$').length
      s.substring(dropLen)
    else s
