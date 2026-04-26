# Scala 3 Playground (`demo/`)

An in-browser, light-themed playground for the Scala 3 compiler. The compiler
itself runs in the browser (it's compiled to JavaScript via Scala.js), so
compilation and Scala.js linking happen entirely on the client — no server.

## Prerequisites

You need the compiler artifacts produced by SBT. From the repository root:

```bash
sbt 'project scala3-compiler-sjs' \
    'fastLinkJS; bundleLibs; packClasspath; packLinkerLibs'
```

This produces `main.js`, `classpath.bin`, and `linker-libs.bin` under
`compiler-js/target/scala3-compiler-sjs/<scalaVersion>-nonbootstrapped/`.
A small script in this project picks them up automatically.

## Develop

```bash
cd demo
bun install
bun run dev
```

Open <http://localhost:5173>. The page downloads the compiler bundle
(~51 MB) and the classpath / linker-libs archives on first load — three
progress bars show the status. Once ready, you can compile and run Scala 3
code directly in the page.

## Build

```bash
bun run build
```

Produces `dist/`, which contains the static site plus copies of the compiler
artifacts. Serve it with any static file server.

## Layout

- `src/main.ts` — entry point.
- `src/compiler/` — loader (XHR + blob-URL script install), typed wrapper for
  the global `DottyCompiler` API, and a console-capturing runner.
- `src/ui/` — `Editor`, `Toolbar`, `OutputPanel`, `Loader`, `Canvas`, `App`.
- `src/util/` — ANSI-to-HTML conversion, HTML escape, `@main` extraction.
- `src/examples/` — Scala source files imported via Vite's `?raw` suffix.
- `src/styles/` — light palette tokens, app layout, CodeMirror light theme.
- `scripts/link-artifacts.mjs` — finds the latest sbt build and symlinks
  `main.js`, `classpath.bin`, `linker-libs.bin` into `public/`.

## Why the artifacts are symlinked

Scala version bumps change the `…-nonbootstrapped` directory name. The link
script picks the most recent matching directory, so it survives version
changes without code edits. Symlinks (rather than copies) make sbt rebuilds
appear in Vite immediately and avoid duplicating the 51 MB bundle.
