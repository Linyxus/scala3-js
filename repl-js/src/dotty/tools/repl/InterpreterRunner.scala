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
 */
class InterpreterRunner:
  private val interp = new Interpreter(Semantics.Defaults)

  private def toIRFiles(m: Map[String, Array[Byte]]): Seq[IRFile] =
    m.map { case (path, bytes) =>
      new MemIRFileImpl(path, Version.Unversioned, bytes): IRFile
    }.toSeq

  /** Load the bundled standard-library `.sjsir` (from `linker-libs.bin`) once. */
  def loadLibrary(buffer: ArrayBuffer): Future[Unit] =
    interp.loadIRFiles(toIRFiles(InterpreterRunner.parseArchive(buffer)))

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
