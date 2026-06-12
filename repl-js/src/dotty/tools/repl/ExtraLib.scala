package dotty.tools
package repl

import scala.scalajs.js.typedarray.ArrayBuffer

import dotty.tools.dotc.ClasspathBlob
import dotty.tools.io.VirtualDirectory

/** An extra library preloaded into a REPL session, unpacked from a `.bin`
 *  archive (the `ClasspathBlob` format, produced by the `packLibBin` sbt task).
 *
 *  One archive carries both halves of a Scala.js library:
 *   - `.tasty` / `.class` entries become [[cpDir]], appended to the compiler's
 *     classpath so lines can typecheck against the library, and
 *   - `.sjsir` entries become [[sjsir]], loaded once into the interpreter so
 *     the library's classes are runnable.
 *
 *  Precedence is first-wins on both sides: the bundled standard library, then
 *  each extra library in the order given. The compiler sees that order through
 *  `AggregateClassPath` and the interpreter through its load order (it dedups
 *  classes by name, keeping the first definition).
 */
final class ExtraLib(
  val name: String,
  val cpDir: VirtualDirectory,
  val sjsir: Map[String, Array[Byte]],
)

object ExtraLib:

  /** Split a packed archive into the compile-time classpath entries and the
   *  runtime `.sjsir` entries. */
  def fromArchive(name: String, buffer: ArrayBuffer): ExtraLib =
    val entries = ClasspathBlob.loadEntries(buffer)
    val (ir, cp) = entries.partition((path, _) => path.endsWith(".sjsir"))
    new ExtraLib(name, ClasspathBlob.dirFromEntries(s"(extra-lib $name)", cp), ir.toMap)

  /** Classpath entries of `libs` that are shadowed by the same path in `base`
   *  (the bundled classpath) or in an earlier lib — those entries are ignored,
   *  since the first match on the classpath wins. Returns (lib name, path). */
  def shadowedPaths(base: VirtualDirectory, libs: List[ExtraLib]): List[(String, String)] =
    val seen = collection.mutable.Set.empty[String]
    libs.flatMap { lib =>
      ClasspathBlob.filesUnder(lib.cpDir).map(_._1).flatMap { path =>
        val shadowed = seen.contains(path) || ClasspathBlob.lookupPath(base, path).isDefined
        seen += path
        if shadowed then Some((lib.name, path)) else None
      }
    }
