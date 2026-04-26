// Execute linked JS produced by DottyCompiler.compileAndLink and capture
// `console.log` / `warn` / `error` output (which is where Scala.js routes
// `println`). The captured logs and any thrown exception are returned for
// the caller to render. The runner runs in the page realm (matches the
// existing browser-test behavior) so user code can reach `appCanvas` and
// other DOM globals.

export interface RunResult {
  logs: string[];
  error: { message: string; stack: string } | null;
}

export function runWithCapture(jsSource: string): RunResult {
  const logs: string[] = [];
  const origLog = console.log;
  const origWarn = console.warn;
  const origError = console.error;
  console.log = (...args: unknown[]) => {
    logs.push(args.map(String).join(' '));
    origLog(...args);
  };
  console.warn = (...args: unknown[]) => {
    logs.push('[warn] ' + args.map(String).join(' '));
    origWarn(...args);
  };
  console.error = (...args: unknown[]) => {
    logs.push('[error] ' + args.map(String).join(' '));
    origError(...args);
  };
  try {
    // eslint-disable-next-line @typescript-eslint/no-implied-eval
    new Function(jsSource)();
    return { logs, error: null };
  } catch (e) {
    const err = e as Error;
    return {
      logs,
      error: { message: err.message ?? String(err), stack: err.stack ?? '' },
    };
  } finally {
    console.log = origLog;
    console.warn = origWarn;
    console.error = origError;
  }
}
