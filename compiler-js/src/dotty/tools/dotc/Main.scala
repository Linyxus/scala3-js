package dotty.tools
package dotc

import scala.scalajs.js
import scala.scalajs.js.typedarray._
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import dotty.tools.io.{VirtualDirectory, AbstractFile}
import dotty.tools.dotc.classpath.VirtualDirectoryClassPath
import dotty.tools.dotc.config.{JavaPlatform, SJSPlatform, Platform}
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.reporting.Reporter

/** JS/Node entry point for the Scala 3 compiler.
 *
 *  Supports two deployment shapes:
 *
 *   - **Packaged `scala3` binary** (`bun build --compile`): the classpath and
 *     linker libraries are embedded as assets. The bun wrapper (`cli.ts`)
 *     stashes their `$bunfs` paths on `globalThis` before this module runs.
 *   - **Dev `bun main.js`**: the same `classpath.bin` / `linker-libs.bin`
 *     archives are read from disk next to `main.js`, with a final fallback to
 *     the unpacked `lib/` class directories.
 *
 *  CLI surface:
 *   - `scala3 compile [opts] File.scala …`  — run the compiler
 *   - `scala3 run [--main Name] File.scala` — compile with `-scalajs`, link to
 *     JavaScript, and execute it in-process
 *   - any other args are passed straight to the compiler (e.g. `scala3 -version`)
 */
object Main extends Driver {

  override def main(args: Array[String]): Unit = {
    // In a browser, `process` does not exist — skip CLI initialization.
    if !hasProcess then return

    val cliArgs = readCliArgs(args)
    val cpDir = loadEmbeddedClasspath()

    cliArgs.toList match
      case "run" :: rest      => runMode(rest.toArray, cpDir)
      case "compile" :: rest  => compileMode(rest.toArray, cpDir)
      case other              => compileMode(other.toArray, cpDir)
  }

  // --- CLI argument handling ------------------------------------------------

  private def hasProcess: Boolean =
    try { val _ = js.Dynamic.global.process.argv; true }
    catch { case _: Throwable => false }

  /** When launched via the Scala.js main-module initializer, `args` is empty,
   *  so read the real arguments from `process.argv`. Both `bun main.js …` and a
   *  compiled bun binary (`./scala3 …`) have two argv prefixes
   *  (`[bun, <script>, ...args]`), so drop the first two.
   */
  private def readCliArgs(args: Array[String]): Array[String] =
    if args.nonEmpty then args
    else
      try
        val argv = js.Dynamic.global.process.argv.asInstanceOf[js.Array[String]]
        argv.jsSlice(2).toArray
      catch case _: Throwable => args

  // --- Embedded asset resolution -------------------------------------------

  private def globalString(name: String): Option[String] =
    // Go through the `globalThis` object: dynamic-name selection is disallowed
    // directly on the global scope, but fine on an explicit object reference.
    val v = js.Dynamic.global.globalThis.selectDynamic(name)
    if js.isUndefined(v) || v == null then None
    else Some(v.asInstanceOf[String])

  /** Resolve a packed archive: from the `$bunfs` global if embedded, otherwise
   *  from a sibling of `main.js` on disk (dev mode). Returns None if absent.
   */
  private def resolveArchive(globalName: String, fileName: String): Option[String] =
    globalString(globalName).orElse(siblingOfMainJs(fileName))

  private def siblingOfMainJs(fileName: String): Option[String] =
    try
      val path = js.Dynamic.global.require("path")
      val fs = js.Dynamic.global.require("fs")
      // main.js sits in scala3-compiler-{fast,full}opt/; archives are one level up.
      val scriptPath = js.Dynamic.global.process.argv.selectDynamic("1").asInstanceOf[String]
      val dirname = path.dirname(path.resolve(scriptPath)).asInstanceOf[String]
      val candidate = path.join(dirname, "..", fileName).asInstanceOf[String]
      if fs.existsSync(candidate).asInstanceOf[Boolean] then Some(candidate) else None
    catch case _: Throwable => None

  private def readArrayBuffer(path: String): ArrayBuffer =
    val fs = js.Dynamic.global.require("fs")
    val u8 = fs.readFileSync(path).asInstanceOf[Uint8Array]
    u8.buffer.slice(u8.byteOffset, u8.byteOffset + u8.byteLength)

  /** Load the packed classpath into an in-memory VirtualDirectory, if present. */
  private def loadEmbeddedClasspath(): Option[VirtualDirectory] =
    resolveArchive("__DOTTY_CLASSPATH_BIN__", "classpath.bin")
      .map(p => ClasspathBlob.load(readArrayBuffer(p)))

  // --- compile --------------------------------------------------------------

  private def compileMode(args: Array[String], cpDir: Option[VirtualDirectory]): Unit =
    val reporter = cpDir match
      case Some(dir) => new EmbeddedDriver(dir).process(args)
      case None      => process(injectBundledClasspath(args)) // legacy lib/ dirs
    if reporter.hasErrors then setExitCode(1)

  // --- run (compile -scalajs, link, execute) --------------------------------

  private def runMode(args: Array[String], cpDir: Option[VirtualDirectory]): Unit =
    cpDir match
      case None =>
        Console.err.println("error: `scala3 run` requires the packaged classpath (classpath.bin not found)")
        setExitCode(1)
      case Some(dir) =>
        resolveArchive("__DOTTY_LINKER_LIBS_BIN__", "linker-libs.bin") match
          case None =>
            Console.err.println("error: `scala3 run` requires linker-libs.bin (not found)")
            setExitCode(1)
          case Some(linkerPath) =>
            BrowserLinker.loadLibraries(readArrayBuffer(linkerPath))
            runLinked(args, dir)

  private def runLinked(args: Array[String], cpDir: VirtualDirectory): Unit =
    val (mainOpt, rest) = extractMainOption(args)
    val sourceFiles = rest.filter(_.endsWith(".scala"))
    if sourceFiles.isEmpty then
      Console.err.println("error: `scala3 run` needs at least one .scala source file")
      setExitCode(1)
      return

    val mainClass = mainOpt.getOrElse(detectMainClass(sourceFiles.head))

    // Compile with -scalajs into an in-memory output directory.
    val outputDir = new VirtualDirectory("(output)", None)
    val driver = new EmbeddedDriver(cpDir, Some(outputDir))
    val reporter = driver.process(rest ++ Array("-scalajs"))
    if reporter.hasErrors then
      setExitCode(1)
      return

    val userSjsir = collectSjsirFiles(outputDir)
    BrowserLinker.link(userSjsir, mainClass).toFuture.foreach { linkedJs =>
      try
        val fn = js.Dynamic.newInstance(js.Dynamic.global.Function)(linkedJs)
        fn.asInstanceOf[js.Function0[Any]]()
      catch
        case e: Throwable =>
          Console.err.println(s"error while running: ${e.getMessage}")
          setExitCode(1)
    }

  /** Pull `--main Name` out of the argument list. */
  private def extractMainOption(args: Array[String]): (Option[String], Array[String]) =
    val i = args.indexOf("--main")
    if i >= 0 && i + 1 < args.length then (Some(args(i + 1)), args.patch(i, Nil, 2))
    else (None, args)

  private val MainAnnotRe = raw"@main\s+def\s+([A-Za-z_][A-Za-z0-9_]*)".r

  /** Mirror demo's extractMainName: the `@main def <name>` becomes class
   *  `<name>`; default to the conventional `main`. */
  private def detectMainClass(sourceFile: String): String =
    try
      val src = js.Dynamic.global.require("fs").readFileSync(sourceFile, "utf-8").asInstanceOf[String]
      MainAnnotRe.findFirstMatchIn(src).map(_.group(1).nn).getOrElse("main")
    catch case _: Throwable => "main"

  private def collectSjsirFiles(dir: AbstractFile, prefix: String = ""): Map[String, Array[Byte]] =
    dir.iterator.flatMap { file =>
      val path = if prefix.isEmpty then file.name else s"$prefix/${file.name}"
      if file.isDirectory then collectSjsirFiles(file, path)
      else if file.name.endsWith(".sjsir") then Iterator((path, file.toByteArray))
      else Iterator.empty
    }.toMap

  private def setExitCode(code: Int): Unit =
    try js.Dynamic.global.process.exitCode = code
    catch case _: Throwable => ()

  // --- legacy dev fallback: classpath from unpacked lib/ directories --------

  /** Auto-detect bundled lib/ directory next to main.js and inject -classpath. */
  private def injectBundledClasspath(args: Array[String]): Array[String] = {
    try {
      val fs = js.Dynamic.global.require("fs")
      val path = js.Dynamic.global.require("path")
      val scriptPath = js.Dynamic.global.process.argv.selectDynamic("1").asInstanceOf[String]
      val dirname = path.dirname(path.resolve(scriptPath)).asInstanceOf[String]
      val libDir = path.join(dirname, "..", "lib").asInstanceOf[String]

      val jdkDir = path.join(libDir, "jdk").asInstanceOf[String]
      val scalaLibDir = path.join(libDir, "scala-lib").asInstanceOf[String]
      val sjsLibDir = path.join(libDir, "scalajs-lib").asInstanceOf[String]

      val allExist =
        fs.existsSync(jdkDir).asInstanceOf[Boolean] &&
        fs.existsSync(scalaLibDir).asInstanceOf[Boolean]

      if !allExist then return args

      val dirs = Seq(jdkDir, scalaLibDir) ++
        (if fs.existsSync(sjsLibDir).asInstanceOf[Boolean] then Seq(sjsLibDir) else Seq.empty)
      val bundledCp = dirs.mkString(":")

      val cpIdx = args.indexWhere(a => a == "-classpath" || a == "-cp")
      if cpIdx >= 0 && cpIdx + 1 < args.length then
        val userCp = args(cpIdx + 1)
        args.updated(cpIdx + 1, s"$bundledCp:$userCp")
      else
        Array("-classpath", bundledCp) ++ args
    } catch {
      case _: Throwable => args
    }
  }
}

/** A Driver whose classpath is served from an in-memory VirtualDirectory
 *  (loaded from `classpath.bin`), while source input and `-d` output stay on
 *  the real filesystem. When `outputDir` is given, compiler output is collected
 *  into that in-memory directory instead (used by `run` to gather `.sjsir`).
 */
private class EmbeddedDriver(
  cpDir: VirtualDirectory,
  outputDir: Option[VirtualDirectory] = None
) extends Driver:

  override protected def initCtx: Context =
    val base = new ContextBase:
      override protected def newPlatform(using Context): Platform =
        val cp = VirtualDirectoryClassPath(cpDir)
        if settings.scalajs.value then
          new SJSPlatform:
            override def classPath(using Context): dotty.tools.io.ClassPath = cp
        else
          new JavaPlatform:
            override def classPath(using Context): dotty.tools.io.ClassPath = cp
    base.initialCtx

  override def process(args: Array[String], rootCtx: Context): Reporter =
    setup(args, rootCtx) match
      case Some((files, compileCtx)) =>
        val ctx = outputDir match
          case Some(od) => compileCtx.fresh.setSetting(compileCtx.settings.outputDir, od)
          case None     => compileCtx
        doCompile(newCompiler(using ctx), files)(using ctx)
      case None =>
        rootCtx.reporter
