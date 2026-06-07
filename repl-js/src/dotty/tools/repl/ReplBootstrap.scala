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
 *  [[ReplSession]]. */
object ReplBootstrap:

  /** Create a session from the archive paths in the environment, or `None` if
   *  either env var is missing. */
  def createSessionFromEnv(): Option[Future[ReplSession]] =
    (env("DOTTY_CLASSPATH_BIN"), env("DOTTY_LINKER_LIBS_BIN")) match
      case (Some(cp), Some(lib)) =>
        val cpDir = ClasspathBlob.load(readArrayBuffer(cp))
        Some(ReplSession.create(cpDir, readArrayBuffer(lib)))
      case _ => None

  /** The Node CLI args (`process.argv` minus `node` and the script path) — the
   *  Scala.js main-module-initializer doesn't forward them via `main(args)`. */
  def args: List[String] =
    try js.Dynamic.global.process.argv.asInstanceOf[js.Array[String]].toList.drop(2)
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

  private def readArrayBuffer(path: String): ArrayBuffer =
    val fs = js.Dynamic.global.require("fs")
    val u8 = fs.readFileSync(path).asInstanceOf[Uint8Array]
    u8.buffer.slice(u8.byteOffset, u8.byteOffset + u8.byteLength)
