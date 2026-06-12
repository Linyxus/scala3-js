package dotty.tools.dotc

import scala.scalajs.js
import scala.scalajs.js.typedarray._

import dotty.tools.io.{AbstractFile, VirtualDirectory, VirtualFile}

/** Loads a classpath binary archive into a VirtualDirectory tree.
 *
 *  Archive format:
 *    [4 bytes: index length L, big-endian uint32]
 *    [L bytes: JSON index, UTF-8]
 *    [data bytes: concatenated file contents]
 *
 *  JSON index maps relative paths to [offset, size] pairs
 *  (offsets relative to data section start).
 */
object ClasspathBlob:

  def load(buffer: ArrayBuffer): VirtualDirectory =
    dirFromEntries("(classpath)", loadEntries(buffer))

  /** Parse an archive into (relative path, content) pairs, in index order. */
  def loadEntries(buffer: ArrayBuffer): List[(String, Array[Byte])] =
    val view = new DataView(buffer)

    // Read 4-byte big-endian index length
    val indexLen = view.getUint32(0).toInt

    // Decode JSON index
    val indexBytes = new Uint8Array(buffer, 4, indexLen)
    val decoder = js.Dynamic.newInstance(js.Dynamic.global.TextDecoder)("utf-8")
    val indexJson = decoder.decode(indexBytes).asInstanceOf[String]
    val index = js.JSON.parse(indexJson).asInstanceOf[js.Dictionary[js.Array[Int]]]

    val dataOffset = 4 + indexLen

    index.map { case (path, arr) =>
      val fileOffset = arr(0)
      val fileSize = arr(1)

      // Extract file bytes from the data section
      val fileBytes = new Int8Array(buffer, dataOffset + fileOffset, fileSize)
      val byteArray = new Array[Byte](fileSize)
      var i = 0
      while i < fileSize do
        byteArray(i) = fileBytes(i)
        i += 1
      (path, byteArray)
    }.toList

  /** Build a VirtualDirectory tree named `label` from (relative path, content)
   *  entries. */
  def dirFromEntries(label: String, entries: Iterable[(String, Array[Byte])]): VirtualDirectory =
    val root = new VirtualDirectory(label, None)

    entries.foreach { case (path, byteArray) =>
      // Create directory tree and file
      val parts = path.split('/')
      var dir: VirtualDirectory = root
      // Navigate/create intermediate directories
      var j = 0
      while j < parts.length - 1 do
        dir = dir.subdirectoryNamed(parts(j)).asInstanceOf[VirtualDirectory]
        j += 1

      // Create the file and write content
      val file = dir.fileNamed(parts.last)
      val out = file.output
      out.write(byteArray)
      out.close()
    }

    root

  /** All regular files under `dir` as (relative path, file), sorted by path. */
  def filesUnder(dir: VirtualDirectory): List[(String, AbstractFile)] =
    def walk(d: AbstractFile, prefix: String): List[(String, AbstractFile)] =
      d.iterator.toList.flatMap { f =>
        val path = if prefix.isEmpty then f.name else s"$prefix/${f.name}"
        if f.isDirectory then walk(f, path) else List((path, f))
      }
    walk(dir, "").sortBy(_._1)

  /** The inverse of [[dirFromEntries]]: (relative path, content) pairs, sorted
   *  by path. */
  def entriesOf(dir: VirtualDirectory): List[(String, Array[Byte])] =
    filesUnder(dir).map((path, f) => (path, f.toByteArray))

  /** Look up a relative `a/b/c`-style path in a directory tree. */
  def lookupPath(dir: VirtualDirectory, path: String): Option[AbstractFile] =
    val parts = path.split('/').toList.filter(_.nonEmpty)
    def go(d: AbstractFile, rest: List[String]): Option[AbstractFile] = rest match
      case Nil          => None
      case last :: Nil  => Option(d.lookupName(last, directory = false))
      case seg :: more  =>
        Option(d.lookupName(seg, directory = true)) match
          case Some(sub) => go(sub, more)
          case None      => None
    go(dir, parts)
