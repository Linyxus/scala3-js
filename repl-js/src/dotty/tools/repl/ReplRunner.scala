package dotty.tools
package repl

import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer

/** The per-line execution seam of the JS REPL: the driver compiles a line to
 *  `.sjsir`, then hands it to a `ReplRunner` to run, which populates the value
 *  bridge the driver reads back.
 *
 *  Two implementations:
 *   - [[InterpreterRunner]] (default) runs each line on sjrd's `.sjsir`
 *     interpreter — a persistent class registry + heap, no linking.
 *   - [[dotty.tools.repl.linked.LinkedRunner]] (opt-in, `DOTTY_REPL_LINKED=1`)
 *     runs each line as a real Scala.js ES module linked into a warm,
 *     member-complete image, imported into this very process.
 *
 *  Both run the wrapper object's initializer *in this process*, so the
 *  compiler-injected `pushRender` statements fill the same `globalThis.__replRenders`
 *  array ([[resetBridge]]/[[readBridge]]) and user `println`s hit the same
 *  `process.stdout` — the value/stdout capture is delivery-agnostic (see
 *  `JSReplCompiler.pushRender`, `JSRendering`, `ReplSession.captureProcessOutput`).
 */
trait ReplRunner:

  /** `true` for the linked-image runner. Selects the linked-only driver
   *  behaviours: dynamic `eval(...)` is unsupported (see [[supportsDynamicEval]])
   *  and a line whose wrapper fails to run is marked invalid so later lines never
   *  re-import its (linked, ESM-cache-pinned) module. */
  def isLinked: Boolean

  /** Whether a runtime `eval(...)` call can be served. The linked image cannot
   *  relink + `import()` synchronously (both are async), so the interpreter-only
   *  dynamic-eval path is disabled in linked mode. */
  final def supportsDynamicEval: Boolean = !isLinked

  /** Load the bundled standard-library IR (from `linker-libs.bin`) plus each
   *  extra library's `.sjsir`, in order. The interpreter loads them into its live
   *  VM; the linked runner folds them into the warm image's touch root. */
  def loadLibrary(buffer: ArrayBuffer, extras: List[Map[String, Array[Byte]]] = Nil): Future[Unit]

  /** Discard/rebuild the live execution state for `:reset` (fresh interpreter, or
   *  fresh linked session dir + relinked baseline). */
  def reset(): Future[Unit]

  /** Load a line's freshly-compiled `.sjsir` and run its wrapper's initializer
   *  (`replMain`), populating the value bridge. The returned future fails with the
   *  wrapper's runtime exception if init throws (the driver renders it), or — in
   *  linked mode — with a [[dotty.tools.repl.linked.LinkedRunner.LinkedRestriction]]
   *  if the line links a construct the linked image cannot serve additively. */
  def loadAndRun(newSjsir: Map[String, Array[Byte]], wrapperClassName: String): Future[Unit]

  /** Register freshly-compiled dynamic-`eval(...)` `.sjsir` (interpreter only;
   *  the linked runner never reaches here — the driver checks
   *  [[supportsDynamicEval]] first). */
  def registerEvalClasses(newSjsir: Map[String, Array[Byte]]): Unit

  /** Instantiate + synchronously run a dynamic-`eval(...)` expression class
   *  (interpreter only). */
  def instantiateEval(expressionClassName: String, bindings: Any): Any

  /** Clear the value bridge before running a wrapper.
   *
   *  Addressed via `globalThis.__replRenders` (a property on the real global
   *  object), not a bare `__replRenders` global ref: under a strict-mode runtime
   *  (Node) assigning to an undeclared bare global throws `ReferenceError`. Must
   *  match the wrapper's access built in `JSReplCompiler.pushRender`. */
  final def resetBridge(): Unit =
    js.Dynamic.global.globalThis.__replRenders = new js.Array[Any]()

  /** Collect the `name -> rendered` pairs the wrapper pushed onto the bridge. */
  final def readBridge(): Map[String, String] =
    val arr = js.Dynamic.global.globalThis.__replRenders.asInstanceOf[js.Array[Any]]
    val b = Map.newBuilder[String, String]
    var i = 0
    while i + 1 < arr.length do
      b += (arr(i).asInstanceOf[String] -> arr(i + 1).asInstanceOf[String])
      i += 2
    b.result()
