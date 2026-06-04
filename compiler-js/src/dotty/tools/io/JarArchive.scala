package dotty.tools.io

import scala.language.unsafeNulls

class JarArchive private (val jarPath: Path, root: Directory) extends PlainDirectory(root) {
  // `def` (not `val`) so the superclass `PlainDirectory` init does not access
  // these before they are assigned (avoids a -Wsafe-init warning); they just
  // forward to the already-initialized `jarPath` param.
  override def name: String = jarPath.name
  override def path: String = jarPath.path
  override def lastModified: Long = 0L
  def close(): Unit = ()
  override def exists: Boolean = false
  def allFileNames(): Iterator[String] = Iterator.empty
  override def toString: String = jarPath.toString
}

object JarArchive {
  def create(path: Path): JarArchive =
    throw new UnsupportedOperationException("JarArchive.create not supported on Scala.js")

  def open(path: Path, create: Boolean = false): JarArchive =
    throw new UnsupportedOperationException("JarArchive.open not supported on Scala.js")
}
