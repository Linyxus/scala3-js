// Extract the function name annotated with `@main def <name>` so we know
// which class to pass to DottyCompiler.compileAndLink. Falls back to
// "main" — also the conventional default object name. Mirrors the regex
// in compiler-js/browser-test/index.html:545-547.

const MAIN_RE = /@main\s+def\s+([A-Za-z_][A-Za-z0-9_]*)/;

export function extractMainName(source: string): string {
  const m = source.match(MAIN_RE);
  return m ? m[1] : 'main';
}
