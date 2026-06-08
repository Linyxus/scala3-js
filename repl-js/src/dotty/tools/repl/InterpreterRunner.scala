package dotty.tools
package repl

import scala.scalajs.js
import scala.scalajs.js.typedarray.*
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import org.scalajs.sjsirinterpreter.core.Interpreter
import org.scalajs.linker.interface.{Semantics, ModuleInitializer, IRFile}
import org.scalajs.linker.standard.MemIRFileImpl
import org.scalajs.ir.Version
import org.scalajs.ir.{Names, Types, Position, Serializers}
import org.scalajs.ir.Trees.ClassDef
import java.nio.ByteBuffer

/** Runs each REPL line on sjrd's `.sjsir` interpreter.
 *
 *  A single persistent `Interpreter` is the REPL's "live VM": its class registry
 *  and heap survive across calls. The full standard library is loaded once via
 *  `loadLibrary`; each line then `loadIRFiles`s only its freshly-compiled `.sjsir`
 *  (the interpreter dedups by class name, so prior classes/state are untouched and
 *  nothing re-runs) and triggers it with `runModuleInitializers`.
 *
 *  No linker, no DCE, no ES-module caching — every class is available, so a later
 *  line can reference any member of any earlier class or the library.
 *
 *  Crossing the 4th wall: the generated wrapper renders each binding's value
 *  (inside the interpreter, where the value lives) and pushes `[name, rendered]`
 *  pairs onto the JS-global array `__replRenders`. [[resetBridge]] clears it
 *  before a run and [[readBridge]] collects it after.
 */
class InterpreterRunner:
  private var interp = new Interpreter(Semantics.Defaults)
  private var libBuffer: ArrayBuffer = null.asInstanceOf[ArrayBuffer]

  private def toIRFiles(m: Map[String, Array[Byte]]): Seq[IRFile] =
    m.map { case (path, bytes) =>
      new MemIRFileImpl(path, Version.Unversioned, bytes): IRFile
    }.toSeq

  /** Load the bundled standard-library `.sjsir` (from `linker-libs.bin`) once. */
  def loadLibrary(buffer: ArrayBuffer): Future[Unit] =
    libBuffer = buffer
    interp.loadIRFiles(toIRFiles(InterpreterRunner.parseArchive(buffer)))

  /** Discard the live VM (a fresh interpreter + reloaded library) for `:reset`.
   *  Necessary because wrapper names restart at `rs$line$1`, which the old
   *  interpreter would dedup against the already-loaded class. */
  def reset(): Future[Unit] =
    interp = new Interpreter(Semantics.Defaults)
    interp.loadIRFiles(toIRFiles(InterpreterRunner.parseArchive(libBuffer)))

  /** Clear the value bridge before running a wrapper.
   *
   *  Addressed via `globalThis.__replRenders` (a property on the real global
   *  object), not a bare `__replRenders` global ref: under a strict-mode runtime
   *  (Node) assigning to an undeclared bare global throws `ReferenceError`; bun
   *  tolerated it, Node does not. Must match the wrapper's access built in
   *  `JSReplCompiler.pushRender`. */
  def resetBridge(): Unit =
    js.Dynamic.global.globalThis.__replRenders = new js.Array[Any]()

  /** Collect the `name -> rendered` pairs the wrapper pushed onto the bridge. */
  def readBridge(): Map[String, String] =
    val arr = js.Dynamic.global.globalThis.__replRenders.asInstanceOf[js.Array[Any]]
    val b = Map.newBuilder[String, String]
    var i = 0
    while i + 1 < arr.length do
      b += (arr(i).asInstanceOf[String] -> arr(i + 1).asInstanceOf[String])
      i += 2
    b.result()

  /** Load a line's freshly-compiled `.sjsir`, then run its wrapper's `replMain`
   *  (which forces the `object rs$line$N` init — running the user code + render). */
  def loadAndRun(newSjsir: Map[String, Array[Byte]], wrapperClassName: String): Future[Unit] =
    for
      _ <- interp.loadIRFiles(toIRFiles(newSjsir))
      _ <- interp.runModuleInitializers(
             List(ModuleInitializer.mainMethod(wrapperClassName, "replMain")))
    yield ()

  // --- synchronous eval support (dynamic `eval(...)`) -----------------------
  //
  // A runtime `eval(...)` call executes *inside* the interpreter while the
  // current line is mid-flight, so its compile→load→run cycle must be fully
  // synchronous (the `Future` API would schedule work on the microtask queue
  // that cannot run while the interpreter stack is blocked). The interpreter's
  // evaluation core is synchronous; [[org.scalajs.sjsirinterpreter.core.EvalSupport]]
  // (which lives in the interpreter's package to reach its `private[core]` API)
  // drives it directly.

  /** Synchronously register freshly-compiled eval `.sjsir` into the live
   *  interpreter. The interpreter dedups by class name, so re-registering an
   *  already-loaded class is a no-op. */
  def registerEvalClasses(newSjsir: Map[String, Array[Byte]]): Unit =
    val classDefs: List[ClassDef] =
      newSjsir.iterator.map { case (_, bytes) =>
        Serializers.deserialize(ByteBuffer.wrap(bytes))
      }.toList
    if classDefs.nonEmpty then
      org.scalajs.sjsirinterpreter.core.EvalSupport.registerClassDefs(interp, classDefs)

  /** Instantiate the synthesised `__EvalExpression` (passing the captured
   *  `bindings` array, an opaque interpreter value threaded back from the eval
   *  call site) and synchronously invoke its `evaluate()`, returning the body's
   *  value. The class must already be registered (via [[registerEvalClasses]]
   *  on a cache miss, or from a prior call on a cache hit). */
  def instantiateEval(expressionClassName: String, bindings: Any): Any =
    org.scalajs.sjsirinterpreter.core.EvalSupport.instantiateAndRun(
      interp, expressionClassName, bindings)

  /** Like [[instantiateEval]], but also returns the retained expression instance. */
  def instantiateEvalAndKeep(expressionClassName: String, bindings: Any): (Any, Any) =
    org.scalajs.sjsirinterpreter.core.EvalSupport.instantiateAndKeep(
      interp, expressionClassName, bindings)

object InterpreterRunner:

  /** Parse a packed `*.bin` archive (4-byte index length, JSON index, data) into
   *  path -> bytes. Same format as `ClasspathBlob`/`packLinkerLibs`. */
  private def parseArchive(buffer: ArrayBuffer): Map[String, Array[Byte]] =
    val view = new DataView(buffer)
    val indexLen = view.getUint32(0).toInt
    val indexBytes = new Uint8Array(buffer, 4, indexLen)
    val decoder = js.Dynamic.newInstance(js.Dynamic.global.TextDecoder)("utf-8")
    val indexJson = decoder.decode(indexBytes).asInstanceOf[String]
    val index = js.JSON.parse(indexJson).asInstanceOf[js.Dictionary[js.Array[Int]]]
    val dataOffset = 4 + indexLen
    index.map { case (path, arr) =>
      val fileOffset = arr(0)
      val fileSize = arr(1)
      val fileBytes = new Int8Array(buffer, dataOffset + fileOffset, fileSize)
      val byteArray = new Array[Byte](fileSize)
      var i = 0
      while i < fileSize do
        byteArray(i) = fileBytes(i)
        i += 1
      (path, byteArray)
    }.toMap
