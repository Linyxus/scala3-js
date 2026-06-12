package dotty.tools
package repl

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.scalajs.js.typedarray.*

import dotty.tools.dotc.ClasspathBlob

/** Node-side bootstrap shared by the JSONL worker ([[JsonMain]]) and the scripted
 *  test driver ([[EvalScriptedTests]]): load the packed compiler archives from
 *  the `DOTTY_CLASSPATH_BIN` / `DOTTY_LINKER_LIBS_BIN` env vars and create a
 *  [[ReplSession]].
 *
 *  Extra libraries (packed `.bin` archives carrying `.tasty` + `.sjsir`, see
 *  [[ExtraLib]] and the `packLibBin` sbt task) can be preloaded via the
 *  `DOTTY_EXTRA_LIBS_BIN` env var (`:`-separated paths) and/or a
 *  `--classpath <paths>` CLI option (parsed by [[extractClasspathArgs]]). */
object ReplBootstrap:

  /** Create a session from the archive paths in the environment, or `None` if
   *  either required env var is missing. Extra libraries come from
   *  `DOTTY_EXTRA_LIBS_BIN` followed by `extraLibArgs` (`--classpath` values);
   *  a failure to load one fails the returned future, naming the archive. */
  def createSessionFromEnv(extraLibArgs: List[String] = Nil): Option[Future[ReplSession]] =
    (env("DOTTY_CLASSPATH_BIN"), env("DOTTY_LINKER_LIBS_BIN")) match
      case (Some(cp), Some(lib)) =>
        val cpDir = ClasspathBlob.load(readArrayBuffer(cp))
        val libBuffer = readArrayBuffer(lib)
        val extraPaths = envExtraLibPaths ++ extraLibArgs.flatMap(splitPathList)
        Some(Future(loadExtraLibs(extraPaths)).flatMap(extras =>
          ReplSession.create(cpDir, libBuffer, extras)))
      case _ => None

  /** Extra-library archive paths from `DOTTY_EXTRA_LIBS_BIN` (`:`-separated). */
  def envExtraLibPaths: List[String] =
    env("DOTTY_EXTRA_LIBS_BIN").map(splitPathList).getOrElse(Nil)

  /** Split a `:`-separated path list, dropping empty segments. */
  def splitPathList(s: String): List[String] =
    s.split(':').toList.map(_.trim).filter(_.nonEmpty)

  /** Extract every `--classpath <paths>` / `--classpath=<paths>` option from
   *  `args`, returning (archive paths in order, remaining args) — or an error
   *  message if a `--classpath` is missing its value. */
  def extractClasspathArgs(args: List[String]): Either[String, (List[String], List[String])] =
    def loop(rest: List[String], paths: List[String], kept: List[String]): Either[String, (List[String], List[String])] =
      rest match
        case "--classpath" :: value :: tail => loop(tail, paths ++ splitPathList(value), kept)
        case "--classpath" :: Nil           => Left("--classpath requires an argument (a `:`-separated list of packed .bin archives)")
        case arg :: tail if arg.startsWith("--classpath=") =>
          loop(tail, paths ++ splitPathList(arg.stripPrefix("--classpath=")), kept)
        case arg :: tail                    => loop(tail, paths, kept :+ arg)
        case Nil                            => Right((paths, kept))
    loop(args, Nil, Nil)

  /** Read and unpack each extra-library archive, attributing failures to the
   *  offending path. */
  def loadExtraLibs(paths: List[String]): List[ExtraLib] =
    paths.map { p =>
      try ExtraLib.fromArchive(baseName(p), readArrayBuffer(p))
      catch case e: Throwable =>
        val msg = Option(e.getMessage).filter(_.nonEmpty).getOrElse(e.toString)
        throw new RuntimeException(s"failed to load extra library archive '$p': $msg")
    }

  private def baseName(path: String): String =
    path.split('/').last

  /** The Node CLI args (`process.argv` minus `node` and the script path) — the
   *  Scala.js main-module-initializer doesn't forward them via `main(args)`. */
  def args: List[String] =
    try js.Dynamic.global.process.argv.asInstanceOf[js.Array[String]].toList.drop(2)
    catch case _: Throwable => Nil

  /** Load the classpath dir, the `DOTTY_EXTRA_LIBS_BIN` extra libraries, and a
   *  library-loaded `InterpreterRunner` from the env archives — for callers that
   *  drive the lower-level driver/runner directly. The extras' `.sjsir` is
   *  already loaded; their `cpDir`s still need to go on the driver's classpath
   *  (`JSReplDriver(…, extraCpDirs = extras.map(_.cpDir))`). */
  def createRunnerFromEnv(): Option[(dotty.tools.io.VirtualDirectory, List[ExtraLib], InterpreterRunner, Future[Unit])] =
    (env("DOTTY_CLASSPATH_BIN"), env("DOTTY_LINKER_LIBS_BIN")) match
      case (Some(cp), Some(lib)) =>
        val cpDir = ClasspathBlob.load(readArrayBuffer(cp))
        val runner = new InterpreterRunner
        val extras = loadExtraLibs(envExtraLibPaths)
        Some((cpDir, extras, runner, runner.loadLibrary(readArrayBuffer(lib), extras.map(_.sjsir))))
      case _ => None

  def readFileLines(path: String): List[String] =
    val content = js.Dynamic.global.require("fs").readFileSync(path, "utf-8").asInstanceOf[String]
    val arr = content.split("\n", -1).toList
    if arr.nonEmpty && arr.last == "" then arr.init else arr

  def readFileOpt(path: String): Option[String] =
    try Some(js.Dynamic.global.require("fs").readFileSync(path, "utf-8").asInstanceOf[String])
    catch case _: Throwable => None

  def listFiles(dir: String): List[String] =
    try js.Dynamic.global.require("fs").readdirSync(dir).asInstanceOf[js.Array[String]].toList
    catch case _: Throwable => Nil

  def hasProcess: Boolean =
    try { val _ = js.Dynamic.global.process.argv; true }
    catch { case _: Throwable => false }

  def env(name: String): Option[String] =
    val v = js.Dynamic.global.process.env.selectDynamic(name)
    if js.isUndefined(v) || v == null then None else Some(v.asInstanceOf[String])

  def setExitCode(code: Int): Unit =
    try js.Dynamic.global.process.exitCode = code
    catch case _: Throwable => ()

  def readArrayBuffer(path: String): ArrayBuffer =
    val fs = js.Dynamic.global.require("fs")
    val u8 = fs.readFileSync(path).asInstanceOf[Uint8Array]
    u8.buffer.slice(u8.byteOffset, u8.byteOffset + u8.byteLength)
