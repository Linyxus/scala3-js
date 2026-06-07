package scala.runtime

import scala.scalajs.js

/** Scala.js override of [[ReplRenderBridge]]: pushes the rendered `[name, value]`
 *  pair onto the JS-global `__replRenders` array the driver resets before each
 *  wrapper run and reads back after.
 *
 *  The array is addressed through `globalThis` (a property access on the real
 *  global object) rather than a bare `__replRenders` global: under a strict-mode
 *  runtime (Node) a bare assignment to an undeclared global throws
 *  `ReferenceError`, whereas a property set on `globalThis` is always allowed. */
object ReplRenderBridge:
  def push(name: String, rendered: String): Unit =
    js.Dynamic.global.globalThis.__replRenders
      .push(name.asInstanceOf[js.Any], rendered.asInstanceOf[js.Any])
    ()
