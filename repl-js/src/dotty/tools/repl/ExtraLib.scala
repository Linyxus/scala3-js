package dotty.tools
package repl

import scala.scalajs.js
import scala.scalajs.js.typedarray.{ArrayBuffer, byteArray2Int8Array}

import dotty.tools.dotc.ClasspathBlob
import dotty.tools.io.VirtualDirectory

/** An extra library preloaded into a REPL session, unpacked from a `.bin`
 *  archive (the `ClasspathBlob` format, produced by the `packLibBin` sbt task).
 *
 *  One archive carries both halves of a Scala.js library:
 *   - `.tasty` / `.class` entries become [[cpDir]], appended to the compiler's
 *     classpath so lines can typecheck against the library,
 *   - `.sjsir` entries become [[sjsir]], loaded once into the interpreter so
 *     the library's classes are runnable, and
 *   - `js-modules/<name>.js` entries become [[jsModules]], native companion
 *     JavaScript published to interpreted code (see [[JSModuleRegistry]]).
 *
 *  Precedence is first-wins on all three: the bundled standard library, then
 *  each extra library in the order given. The compiler sees that order through
 *  `AggregateClassPath` and the interpreter through its load order (it dedups
 *  classes by name, keeping the first definition).
 */
final class ExtraLib(
  val name: String,
  val cpDir: VirtualDirectory,
  val sjsir: Map[String, Array[Byte]],
  val jsModules: Map[String, String] = Map.empty,
)

object ExtraLib:

  /** Archive prefix reserved for native companion JS modules. The rest of the
   *  namespace is left free for a future explicit index. */
  final val JSModulePrefix = "js-modules/"

  /** Split a packed archive into the compile-time classpath entries, the runtime
   *  `.sjsir` entries, and the native JS modules. A module is kept off the
   *  classpath, so it neither reaches the compiler nor shows up in
   *  [[shadowedPaths]]; anything else under [[JSModulePrefix]] is treated as an
   *  ordinary entry. */
  def fromArchive(name: String, buffer: ArrayBuffer): ExtraLib =
    val entries = ClasspathBlob.loadEntries(buffer)
    val (ir, rest) = entries.partition((path, _) => path.endsWith(".sjsir"))
    val (mods, cp) = rest.partition((path, _) => isJSModulePath(path))
    new ExtraLib(
      name,
      ClasspathBlob.dirFromEntries(s"(extra-lib $name)", cp),
      ir.toMap,
      mods.map((path, bytes) => (moduleNameOf(path), decodeUtf8(bytes))).toMap,
    )

  /** Whether `path` names a native JS module, i.e. `js-modules/<name>.js`. */
  def isJSModulePath(path: String): Boolean =
    path.startsWith(JSModulePrefix) && path.endsWith(".js")

  /** The module name an archive path publishes under. */
  def moduleNameOf(path: String): String =
    path.stripPrefix(JSModulePrefix).stripSuffix(".js")

  private def decodeUtf8(bytes: Array[Byte]): String =
    val decoder = js.Dynamic.newInstance(js.Dynamic.global.TextDecoder)("utf-8")
    decoder.decode(byteArray2Int8Array(bytes)).asInstanceOf[String]

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
