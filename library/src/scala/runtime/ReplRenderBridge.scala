package scala.runtime

/** Sink for the REPL's value-rendering bridge.
 *
 *  The JS REPL injects, into each compiled line's wrapper, a call that pushes
 *  `[name, rendered]` onto a JS-global array the driver reads back (see
 *  `JSReplCompiler.pushRender`). That push is a `js.Dynamic` operation, which is
 *  rejected under `language.experimental.safe`. Routing it through this object —
 *  in the `scala.runtime` package, which `SafeRefs` treats as assumed-safe —
 *  keeps the injected call inside the safe subset; the unsafe primitive lives in
 *  this object's body (compiled normally), exactly like [[scala.runtime.eval.EvalBridge]].
 *
 *  This `library/src` version is a stub: it only needs to *compile* for the JVM
 *  stdlib build used as the eval compile classpath. The real implementation is
 *  the `library-js/src` override, which performs the `js.Dynamic` push and runs
 *  inside the interpreter where the rendered values live. */
object ReplRenderBridge:
  def push(name: String, rendered: String): Unit = ()
