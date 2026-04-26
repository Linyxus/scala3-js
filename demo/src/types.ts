// Shared types for the playground.
//
// `DottyCompiler` is exported by the Scala-compiled-to-JS bundle
// (compiler-js/src/dotty/tools/dotc/BrowserMain.scala). It registers itself on
// `window` via `@JSExportTopLevel("DottyCompiler")`. Method signatures and
// return shapes mirror that file 1:1.

export type Severity = 'info' | 'warning' | 'error';

export interface Diagnostic {
  severity: Severity;
  /** 1-based source line, or -1 if no position */
  line: number;
  /** 1-based source column, or -1 if no position */
  column: number;
  /** Pre-rendered message including ANSI SGR codes and source context */
  message: string;
}

export interface CompileAndLinkResult {
  success: boolean;
  diagnostics: Diagnostic[];
  /** Linked JavaScript source as a string. `undefined` when `success === false`. */
  js?: string;
}

export interface DottyCompiler {
  loadClasspath(buf: ArrayBuffer): void;
  compile(source: string, args: string[]): Diagnostic[];
  loadLinkerLibs(buf: ArrayBuffer): void;
  compileAndLink(source: string, args: string[], mainClass: string): Promise<CompileAndLinkResult>;
}

declare global {
  interface Window {
    DottyCompiler?: DottyCompiler;
  }
}
