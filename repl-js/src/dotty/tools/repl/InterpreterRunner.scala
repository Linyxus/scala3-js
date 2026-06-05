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

  /** Clear the value bridge before running a wrapper. */
  def resetBridge(): Unit =
    js.Dynamic.global.__replRenders = new js.Array[Any]()

  /** Collect the `name -> rendered` pairs the wrapper pushed onto the bridge. */
  def readBridge(): Map[String, String] =
    val arr = js.Dynamic.global.__replRenders.asInstanceOf[js.Array[Any]]
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
