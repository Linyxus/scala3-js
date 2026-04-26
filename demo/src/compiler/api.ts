// Typed wrapper over the global `window.DottyCompiler` registered by main.js.
// All methods throw if main.js hasn't been loaded yet.

import type { Diagnostic, CompileAndLinkResult, DottyCompiler } from '../types';

function compiler(): DottyCompiler {
  const dc = window.DottyCompiler;
  if (!dc) throw new Error('DottyCompiler is not loaded yet — call init() first');
  return dc;
}

export function loadClasspath(buf: ArrayBuffer): void {
  compiler().loadClasspath(buf);
}

export function loadLinkerLibs(buf: ArrayBuffer): void {
  compiler().loadLinkerLibs(buf);
}

export function compile(source: string, args: string[] = []): Diagnostic[] {
  return compiler().compile(source, args);
}

export function compileAndLink(
  source: string,
  mainClass: string,
  args: string[] = [],
): Promise<CompileAndLinkResult> {
  return compiler().compileAndLink(source, args, mainClass);
}
