package dotty.tools
package repl

import scala.scalajs.js

/** The registry of native companion JS modules published to interpreted code.
 *
 *  An extra library ([[ExtraLib]]) may ship real, pre-linked JavaScript next to
 *  its `.tasty`/`.sjsir`, under the reserved archive prefix
 *  `js-modules/<name>.js`. Each such entry is executed once per worker as a
 *  CommonJS module and its exports published at `globalThis.__replJSModules.<name>`,
 *  which is what a `@js.native` facade reaches through its global fallback:
 *
 *  {{{
 *  @js.native
 *  @JSImport("aukGrepEngine", JSImport.Namespace,
 *            globalFallback = "__replJSModules.aukGrepEngine")
 *  object GrepEngine extends js.Object:
 *    def search(pattern: String, root: String): js.Array[String] = js.native
 *  }}}
 *
 *  The interpreter cannot honour a bare `Import` load spec — it throws "Imports
 *  are currently not supported" — but it resolves `ImportWithGlobalFallback`
 *  through the global branch, i.e. exactly the lookup above. The same facade
 *  therefore also works unchanged in a linked Scala.js build, where the real
 *  module import wins instead.
 *
 *  The registry is a property of `globalThis` rather than a bare global, for the
 *  reason spelled out in [[InterpreterRunner.resetBridge]] and because
 *  interpreted code reaches globals through `js.eval`, which runs in global scope
 *  and cannot see the worker's own lexical bindings.
 *
 *  It is deliberately process-wide: it outlives both individual sessions and
 *  `:reset` (which only rebuilds the interpreter), so a module's top-level code
 *  runs exactly once per worker. Registration is first-wins, mirroring the
 *  classpath and interpreter precedence documented on [[ExtraLib]].
 */
object JSModuleRegistry:

  /** The `globalThis` property under which modules are published. Must be a
   *  valid JS global reference, since a facade's `globalFallback` path starts
   *  with it. */
  final val RegistryName = "__replJSModules"

  /** Load `code` as a CommonJS module and publish its exports as `name`, unless
   *  `name` is already taken — in which case nothing runs and `false` is
   *  returned (first-wins; the caller reports the collision).
   *
   *  Throws if `name` is not a valid module name (see [[isValidModuleName]]), or
   *  if the module's top-level code throws. */
  def register(name: String, code: String): Boolean =
    if !isValidModuleName(name) then
      throw new IllegalArgumentException(
        s"invalid JS module name '$name': a native companion module must be named by a " +
        s"JS identifier ([A-Za-z_$$][A-Za-z0-9_$$]*), so that a facade can address it as " +
        s"`globalFallback = \"$RegistryName.<name>\"`")
    val reg = registry
    if js.special.in(name, reg) then false
    else
      // A fresh CommonJS scope per module: `module.exports` is what the module
      // ultimately publishes (it may replace the object wholesale), and the
      // `sourceURL` comment gives the generated function a real name in stack
      // traces. `require` is the host's own — Node's under both the fork's tests
      // and auk's bootstrap shim, which installs it on `globalThis`.
      val module = js.Dynamic.literal(exports = js.Dynamic.literal())
      val loader = js.Dynamic.newInstance(js.Dynamic.global.Function)(
        "exports", "module", "require",
        code + "\n//# sourceURL=" + name + ".js\n")
      loader.call(module.exports, module.exports, module, hostRequire)
      reg.updateDynamic(name)(module.exports)
      true

  /** Whether a module is published under `name`. */
  def isRegistered(name: String): Boolean =
    js.special.in(name, registry)

  /** The published module names, sorted. */
  def registered: List[String] =
    js.Object.keys(registry.asInstanceOf[js.Object]).toList.sorted

  /** Module names must be JS identifiers: a facade addresses its module as a
   *  dotted `globalFallback` path (`__replJSModules.<name>`), which the backend
   *  splits on `.` and which must remain valid JavaScript when the same facade
   *  is linked normally. Names are ASCII-only on purpose — an archive is a build
   *  artifact, not a place to be clever. Reserved words are allowed, since they
   *  are legal property names. */
  def isValidModuleName(name: String): Boolean =
    def isStart(c: Char): Boolean =
      (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || c == '$'
    def isPart(c: Char): Boolean = isStart(c) || (c >= '0' && c <= '9')
    name.nonEmpty && isStart(name.charAt(0)) && name.forall(isPart)

  /** The registry object, installed on first use.
   *
   *  It is a `Proxy` whose `get` trap names an unregistered module instead of
   *  yielding `undefined` — without it, a facade whose module was never shipped
   *  fails at the *next* property access with a bare "Cannot read properties of
   *  undefined", pointing nowhere near the actual mistake. Only string keys are
   *  trapped, so symbol probes (`util.inspect` and friends) behave normally. */
  private def registry: js.Dynamic =
    val globals = js.Dynamic.global.globalThis
    val existing = globals.selectDynamic(RegistryName)
    if !js.isUndefined(existing) && existing != null then existing
    else
      val store = js.Dynamic.literal()
      val handler = js.Dynamic.literal(
        get = ((target: js.Any, key: js.Any, receiver: js.Any) =>
          if js.typeOf(key) != "string" || js.special.in(key, target) then
            js.Dynamic.global.Reflect.get(target, key, receiver)
          else
            val have = js.Object.keys(target.asInstanceOf[js.Object]).toList.sorted
            val listed = if have.isEmpty then "none" else have.mkString(", ")
            throw js.JavaScriptException(
              new js.Error(s"no JS module '$key' registered (have: $listed)"))
        ): js.Function3[js.Any, js.Any, js.Any, js.Any]
      )
      val proxy = js.Dynamic.newInstance(js.Dynamic.global.Proxy)(store, handler)
      globals.updateDynamic(RegistryName)(proxy)
      proxy

  /** The host's `require`, or `undefined` where there is none — a module that
   *  never calls it still loads, and one that does gets a clear TypeError rather
   *  than a `ReferenceError` blamed on the registry. */
  private def hostRequire: js.Any =
    try js.Dynamic.global.require
    catch case _: Throwable => js.undefined
