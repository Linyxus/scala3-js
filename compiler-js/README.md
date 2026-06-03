# Scala 3 Compiler on JavaScript (compiler-js)

The `compiler-js/` subproject compiles the Scala 3 compiler itself to JavaScript
via Scala.js.

It currently supports two execution modes:

- **Node.js CLI** via `main.js` / `bin/scalac-js` for file-based compilation
- **Browser API** via the exported `DottyCompiler` object for in-memory compile
  and compile-and-link flows

The compiler frontend is functional: parsing, type-checking, error reporting,
and capture checking all work. The Scala.js backend (`-scalajs`) also works, so
`.sjsir` files can be emitted. The JVM backend (`GenBCode`) is stubbed out, so
a plain compile is useful for diagnostics and frontend validation but does not
emit `.class` files.

## Quick start

```bash
# Build the JS compiler and bundle library directories for the CLI:
sbt --client scala3-compiler-sjs/fastLinkJS
sbt --client scala3-compiler-sjs/bundleLibs

# Optional: build packed assets for the browser API:
sbt --client scala3-compiler-sjs/packClasspath
sbt --client scala3-compiler-sjs/packLinkerLibs

# Compile a Scala file (frontend only; no .class output):
mkdir -p /tmp/out
./bin/scalac-js -d /tmp/out Hello.scala

# Compile to Scala.js IR:
mkdir -p /tmp/out-sjs
./bin/scalac-js -scalajs -d /tmp/out-sjs Hello.scala
```

The `bin/scalac-js` script auto-builds with `fastLinkJS` + `bundleLibs` if
`main.js` is not found.

## How it works

### Source override mechanism

The compiler sources are shared with the main compiler. Files placed in
`compiler-js/src/` override files at the same relative path in the shared source
roots. The SBT build filter (in `project/Build.scala`) also excludes Java source
files and JVM-only packages (`backend/jvm/`, `scripting/`, `debug/`) from the
shared sources.

The current tree has about 110 source files in `compiler-js/src/`. Roughly 60
are direct overrides of shared compiler or tasty sources; the rest are
Scala.js-specific support files, entry points, and API stubs. Most overrides are
shims for JVM APIs not available in Scala.js (java.nio.file, java.util.zip,
java.lang.reflect, xsbti, etc.). A handful adapt compiler code to avoid
reflection or JVM-specific features.

### Bundled libraries (`bundleLibs`)

The JS compiler needs class files on its classpath just like the JVM compiler.
The `bundleLibs` SBT task extracts them to a `lib/` directory:

```
compiler-js/target/.../lib/
  jdk/            ← java.base classes (from jmod)
  scala-lib/      ← scala2 + scala3 library classes and .tasty files
  scalajs-lib/    ← scalajs-library classes (needed for -scalajs)
  sjsir/          ← library .sjsir files (used for browser linking)
```

Sources:
- **jdk/**: Extracted from `$JAVA_HOME/jmods/java.base.jmod` using `jmod extract`
  (jmod files have extra header bytes that make `IO.unzip` fail)
- **scala-lib/**: Copied from `scala-library-bootstrapped` class directory, which
  contains both the Scala 2 standard library and Scala 3 library extensions,
  compiled together with `.tasty` files
- **scalajs-lib/**: Extracted from the `scalajs-library` JAR resolved via the SBT
  update report
- **sjsir/**: Extracted from `scalajs-library`, `scalajs-javalib`, and the local
  `scala-library-sjs` class directory

The `lib/` directory is placed as a sibling of the `scala3-compiler-fastopt/`
directory (not inside it, because the Scala.js linker cleans its output
directory on re-link). Extraction is cached — delete `lib/` to force
re-extraction.

### Packed browser assets (`packClasspath`, `packLinkerLibs`)

The browser API uses packed archives instead of filesystem directories:

```
compiler-js/target/.../
  scala3-compiler-fastopt/
    main.js
    main.js.map
  lib/
    ...
  classpath.bin    ← packed .class/.tasty inputs for browser compilation
  linker-libs.bin  ← packed library .sjsir inputs for browser linking
```

- **classpath.bin** packs the files from `lib/jdk`, `lib/scala-lib`, and
  `lib/scalajs-lib`
- **linker-libs.bin** packs the `.sjsir` files from `lib/sjsir`

### Auto-classpath detection

`Main.scala` (the JS entry point) automatically detects the bundled `lib/`
directory and injects `-classpath` arguments:

1. Derives the script directory from `process.argv[1]` (Node.js)
2. Looks for `../lib/jdk`, `../lib/scala-lib`, `../lib/scalajs-lib` relative
   to `main.js`
3. If no `-classpath`/`-cp` is provided: prepends the bundled classpath
4. If `-classpath` is provided: merges bundled paths before user paths

Note: `js.Dynamic.global.__dirname` cannot be used because `__dirname` is a
module-scoped variable in Node.js, not a property of `global`.

## Architecture

### SBT project

Defined in `project/Build.scala` as `scala3-compiler-sjs`:

- Depends on `scala3-interfaces`, `tasty-core-bootstrapped`, `scala3-library-sjs`
- Uses `compiler-js/src` first, then shared compiler sources, bootstrapped-only
  sources, tasty-core sources, and generated `scalajs-ir` sources
- Compiled with the non-bootstrapped compiler
- `scalaJSUseMainModuleInitializer := true` with `Compile / mainClass := Some("dotty.tools.dotc.Main")`
- ES2018 target (needed for regex MULTILINE flag support)
- Provides the maintenance tasks `compile`, `fastLinkJS`, `bundleLibs`,
  `packClasspath`, and `packLinkerLibs`
- Fetches and repackages `scalajs-ir` sources (same as the bootstrapped compiler)

### Key overrides and additions

**MegaPhase.scala** — The only change is the `defines` method, which on JVM uses
`Class.getDeclaredMethods` (Java reflection) to check which mini-phases handle
which tree types. On Scala.js, this is replaced with `true` (conservatively call
all mini-phases for all tree types). This is a performance trade-off, not a
correctness issue. The rest of MegaPhase is the original, unmodified.

**GenBCode.scala** — Stubbed: `isRunnable` returns `false`. The JVM backend
cannot run in JavaScript.

**IO layer** — `java.io.File`, `java.nio.file.Files`, `java.io.FileOutputStream`
are reimplemented using Node.js `fs` and `path` modules. Jar/zip reading is
stubbed (only unpacked class directories work as classpath entries).

**Macros / Quotes** — `MacroAnnotations`, `Splicer`, `Interpreter`,
`MacroClassLoader` are stubbed. Compile-time macro evaluation is not supported.

**Plugins / SemanticDB / Profiler** — Disabled via stubs.

**sbt integration** — `ExtractAPI`, `ShowAPI`, `ThunkHolder`, and callback
interfaces are stubbed to remove the dependency on zinc/sbt internals.

**BrowserMain / BrowserLinker** — JS-specific entry points for in-memory browser
compilation and compile-and-link flows, backed by `classpath.bin` and
`linker-libs.bin`.

### Patterns and pitfalls

When adding new overrides:

- **Parameterless vs empty-paren**: Scala.js interface stubs define parameterless
  methods; if the compiler calls them with `()`, override the caller instead.
- **Constructor init order**: Use `def` (not `val`) for fields in classes like
  `PlainFile` where the superclass constructor accesses them before initialization.
- **Circular init**: Break cycles with `null.asInstanceOf` (see `NoSourcePosition`).
- **Scala.js built-in classes**: Cannot add methods to `Thread`, `System`, `Class`,
  `URI` — override the call sites instead.
- **Minimize overrides**: Always start from the original file. Only change what
  actually breaks. The MegaPhase incident (rewriting logic instead of fixing the
  one incompatible method) is the cautionary tale.

## Standalone executable (`scala3` binary)

The compiler can be packaged into a single self-contained native executable with
[bun](https://bun.sh)'s `bun build --compile`. The binary bundles the bun
runtime, the fully-optimized `main.js`, and the packed `classpath.bin` /
`linker-libs.bin` archives — no JVM, Node, or bun installation required to run
it.

```bash
# Build a binary for the host platform → dist/scala3
sbt --client scala3-compiler-sjs/buildBinary

# Build the full release matrix → dist/scala3-{darwin,linux}-{arm64,x64}[, .exe]
sbt --client scala3-compiler-sjs/buildBinaryAll
```

`bin/scala3` execs `dist/scala3`, building it on demand if missing.

### CLI surface

```bash
scala3 compile [opts] File.scala …   # run the compiler (frontend; -scalajs emits .sjsir)
scala3 run [--main Name] File.scala  # compile with -scalajs, link to JS, and run it
scala3 -version                      # any other args pass straight to the compiler
```

`run` auto-detects the entry point from `@main def <name>` (falling back to
`main`); pass `--main <Name>` to override.

### How it works

- **`buildBinary`** (host) and **`buildBinaryAll`** (cross-compiled matrix) live
  in `project/Build.scala`. Both depend on `fullLinkJS` + `packClasspath` +
  `packLinkerLibs` via the shared `prepareBinaryAssets` task, which also
  generates the bun entrypoint `cli.ts`.
- The classpath cannot be embedded as a loose class-file tree: bun stores
  embedded files under flat, content-hashed `$bunfs` names with no directory
  enumeration, but the compiler's `DirectoryClassPath` needs to list packages.
  So the binary reuses the **browser** machinery — the single `classpath.bin`
  archive is loaded by `ClasspathBlob.load` into an in-memory `VirtualDirectory`
  served as a `VirtualDirectoryClassPath` (see `EmbeddedDriver` in `Main.scala`).
- `cli.ts` embeds `main.js` / `classpath.bin` / `linker-libs.bin` with
  `import … with { type: "file" }`, stashes the classpath/linker `$bunfs` paths
  on `globalThis`, provides a `createRequire`-based `require` (the IO shims call
  `require("fs")`/`require("path")`), and runs `main.js` as a **classic script**
  via indirect `eval` — Scala.js NoModule output expects `this === globalThis`,
  so it must not be imported as a module.
- `Main.scala` resolves the archives from those globals when embedded, falling
  back to disk (next to `main.js`) for `bun main.js …` dev runs, with a final
  fallback to the unpacked `lib/` class directories for plain `compile`.

### Cross-compilation

`buildBinaryAll` targets darwin arm64/x64, linux x64/arm64, and windows x64. The
same `main.js` and archives are platform-independent; only the embedded bun
runtime differs per `--target`. Edit `binaryTargets` in `project/Build.scala` to
change the matrix. Notes: x64 may need a `-baseline` target for pre-2013 CPUs,
and macOS distribution requires `codesign` (with JIT entitlements, since `run`
uses `eval`).

## JavaScript API

The generated `main.js` exposes two usage modes: a Node.js CLI entry point and a
browser-facing API.

### Node.js (CLI)

When loaded in Node.js, `main.js` runs immediately as a CLI tool. It reads
arguments from `process.argv`, auto-detects bundled libraries, and invokes the
compiler. There is no API to call — it behaves like a standard command-line
compiler.

### Browser (`DottyCompiler`)

When loaded in a browser (via a `<script>` tag), `main.js` exports a global
`DottyCompiler` object with the following primary methods:

#### `DottyCompiler.loadClasspath(buffer: ArrayBuffer): void`

Loads the classpath archive into memory. Must be called before `compile()` or
`compileAndLink()`. The `buffer` should contain the contents of `classpath.bin`
(produced by the `packClasspath` SBT task).

#### `DottyCompiler.compile(source: string, args: string[]): Diagnostic[]`

Compiles a Scala source string and returns an array of diagnostic objects.

**Parameters:**
- `source` — Scala source code as a string
- `args` — additional compiler flags (e.g., `["-Xprint:typer", "-Ycc"]`)

**Returns** an array of objects, each with:

| Field      | Type     | Description                                      |
|------------|----------|--------------------------------------------------|
| `severity` | `string` | `"info"`, `"warning"`, or `"error"`              |
| `line`     | `number` | source line from dotc (`-1` if no position)      |
| `column`   | `number` | source column from dotc (`-1` if no position)    |
| `message`  | `string` | Rendered message with source context (ANSI-colored) |

#### `DottyCompiler.loadLinkerLibs(buffer: ArrayBuffer): void`

Loads the linker libraries archive into memory. Must be called before
`compileAndLink()`. The `buffer` should contain the contents of
`linker-libs.bin` (produced by the `packLinkerLibs` SBT task).

#### `DottyCompiler.compileAndLink(source: string, args: string[], mainClass: string): Promise<{ success, diagnostics, js }>`

Compiles a Scala source string with `-scalajs`, then links the emitted `.sjsir`
with the preloaded linker libraries and returns JavaScript source code.

- `success` is `false` if compilation failed
- `diagnostics` is the same diagnostic shape as `compile()`
- `js` is the linked JavaScript source when linking succeeds

#### Example

```javascript
const [cpResp, llResp] = await Promise.all([
  fetch("classpath.bin"),
  fetch("linker-libs.bin")
]);
DottyCompiler.loadClasspath(await cpResp.arrayBuffer());
DottyCompiler.loadLinkerLibs(await llResp.arrayBuffer());

const diagnostics = DottyCompiler.compile(
  "object Hello { val x: Int = true }",
  []
);
for (const d of diagnostics) {
  console.log(`[${d.severity}] line ${d.line}: ${d.message}`);
}

const linked = await DottyCompiler.compileAndLink(
  "object Main { def main(args: Array[String]): Unit = println(\"hi\") }",
  [],
  "Main"
);
if (linked.success) {
  console.log(linked.js);
}
```

There is also a manual browser harness in `compiler-js/browser-test/index.html`
that loads `main.js`, `classpath.bin`, and `linker-libs.bin`.

## Limitations

- **No `.class` output**: The JVM backend is stubbed. A plain compile can report
  diagnostics, but it does not produce JVM bytecode.
- **No jar/zip classpath**: Only unpacked class directories work. Implementing
  in-browser zip reading (e.g., via JSZip) would also enable jar classpath support.
- **No macro evaluation**: Compile-time splice/macro execution is not supported.
- **No dynamic plugin loading**: Compiler plugins are stubbed out on Scala.js.
- **No incremental compilation**: sbt integration is stubbed.
- **Browser API is in-memory only**: File-based IO depends on Node.js `fs`/`path`.
  The browser path uses virtual files plus `classpath.bin` / `linker-libs.bin`.
- **Limited verification harness**: The subproject has a manual browser page in
  `compiler-js/browser-test/index.html`; it does not currently have a dedicated
  compilation test suite under `tests/`.

## File inventory

The current tree has about 110 source files in `compiler-js/src/`.

- Roughly 60 are path-for-path overrides of shared compiler or tasty sources
- Roughly 50 are JS-specific support files, entry points, or API stubs

Common categories include:

- Java / IO compatibility shims (`java/nio/file/`, `java/io/`, `java/util/zip/`)
- Compiler infrastructure overrides (`MegaPhase.scala`, `Splicer.scala`,
  `Plugin.scala`, `Driver.scala`)
- Classpath and archive handling (`ClassPathFactory.scala`, `ZipArchive.scala`,
  `ClasspathBlob.scala`)
- Browser entry points (`BrowserMain.scala`, `BrowserLinker.scala`)
- sbt / xsbti / SemanticDB / profiler stubs
