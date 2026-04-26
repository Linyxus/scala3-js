// Asset loading for the in-browser Scala 3 compiler.
//
// Two responsibilities:
//   1. fetchWithProgress: XHR-based ArrayBuffer download with progress events
//      (more cross-browser-friendly than fetch + ReadableStream).
//   2. installCompilerScript: load main.js as a classic <script> via a blob
//      URL. This is required because main.js is Scala.js NoModule output —
//      it uses top-level `let`, which doesn't escape `eval` scope, so we
//      cannot use `eval()` or dynamic `import()`. The blob-URL <script>
//      pattern lets `@JSExportTopLevel("DottyCompiler")` bind onto `window`.
//
// Source-map and source-URL hints are appended so devtools shows a readable
// filename instead of "blob:…".

export interface ProgressEvent {
  loaded: number;
  total: number | null;
  fraction: number | null;
}

export type ProgressHandler = (e: ProgressEvent) => void;

export function fetchWithProgress(url: string, onProgress?: ProgressHandler): Promise<ArrayBuffer> {
  return new Promise((resolve, reject) => {
    const xhr = new XMLHttpRequest();
    xhr.open('GET', url, true);
    xhr.responseType = 'arraybuffer';
    xhr.onprogress = (e) => {
      if (!onProgress) return;
      if (e.lengthComputable) {
        onProgress({ loaded: e.loaded, total: e.total, fraction: e.loaded / e.total });
      } else {
        onProgress({ loaded: e.loaded, total: null, fraction: null });
      }
    };
    xhr.onload = () => {
      if (xhr.status >= 200 && xhr.status < 300) {
        onProgress?.({ loaded: xhr.response.byteLength, total: xhr.response.byteLength, fraction: 1 });
        resolve(xhr.response as ArrayBuffer);
      } else {
        reject(new Error(`HTTP ${xhr.status} for ${url}`));
      }
    };
    xhr.onerror = () => reject(new Error(`Network error fetching ${url}`));
    xhr.send();
  });
}

/**
 * Install main.js as a classic script via a blob URL.
 *
 * Why two scripts: main.js declares `let DottyCompiler` at the top level.
 * Top-level `let` in a classic script lives in the Script scope, NOT on
 * `globalThis`/`window`. Inline classic scripts on the same page can see
 * it (which is how compiler-js/browser-test/index.html works), but ES
 * modules cannot. So after main.js loads, we inject a second classic
 * script that copies the Script-scope binding onto `window.DottyCompiler`
 * — the bridge both scripts share.
 */
export function installCompilerScript(buffer: ArrayBuffer): Promise<void> {
  return new Promise((resolve, reject) => {
    const blob = new Blob([buffer], { type: 'application/javascript' });
    const main = document.createElement('script');
    main.src = URL.createObjectURL(blob);
    main.onload = () => {
      URL.revokeObjectURL(main.src);
      // Now bridge: a second classic script can see main.js's let-binding.
      const bridge = document.createElement('script');
      bridge.textContent =
        'try { window.DottyCompiler = DottyCompiler; } catch (e) { window.__dottyBridgeError = e; }';
      bridge.onload = null; // inline scripts don't fire onload
      document.head.appendChild(bridge);
      // Inline script execution is synchronous — check the result.
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      const w = window as unknown as { DottyCompiler?: unknown; __dottyBridgeError?: Error };
      if (w.__dottyBridgeError) {
        reject(w.__dottyBridgeError);
      } else if (!w.DottyCompiler) {
        reject(new Error('main.js loaded but DottyCompiler was not exposed'));
      } else {
        resolve();
      }
    };
    main.onerror = () => {
      URL.revokeObjectURL(main.src);
      reject(new Error('Failed to execute main.js'));
    };
    document.head.appendChild(main);
  });
}
