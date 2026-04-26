// Top-level controller. Owns the state machine:
//   loading -> ready (idle | compiling) -> error
// Wires Toolbar -> Editor -> Compiler API -> OutputPanel.

import { fetchWithProgress, installCompilerScript } from '../compiler/loader';
import * as compilerApi from '../compiler/api';
import { runWithCapture } from '../compiler/runner';
import { extractMainName } from '../util/extractMain';

import { createEditor, type EditorHandle } from './Editor';
import { createToolbar, type ToolbarHandle } from './Toolbar';
import { createOutputPanel, type OutputPanelHandle } from './OutputPanel';
import { createLoader } from './Loader';
import { defaultExample, examples } from '../examples';

type Status = 'loading' | 'ready' | 'compiling' | 'error';
type Mode = 'compile' | 'run';

export function mountApp(root: HTMLElement): void {
  root.innerHTML = '';

  const header = buildHeader();
  root.appendChild(header.root);

  const main = document.createElement('main');
  main.className = 'app-main';
  root.appendChild(main);

  const loader = createLoader(main);

  loadCompiler({
    onJs: (l, t) => loader.rowJs.setProgress(l, t),
    onCp: (l, t) => loader.rowCp.setProgress(l, t),
    onLl: (l, t) => loader.rowLl.setProgress(l, t),
  })
    .then(() => {
      loader.remove();
      header.setStatus('ready');
      buildPlayground(main, header);
    })
    .catch((err) => {
      header.setStatus('error');
      loader.setError(`${err?.message ?? err}`);
    });
}

interface HeaderHandle {
  root: HTMLElement;
  setStatus(s: Status): void;
}

function buildHeader(): HeaderHandle {
  const header = document.createElement('header');
  header.className = 'app-header';

  const brand = document.createElement('div');
  brand.className = 'app-header__brand';

  const mark = document.createElement('div');
  mark.className = 'app-header__brand-mark';
  mark.textContent = 'S3';

  const title = document.createElement('div');
  title.className = 'app-header__title';
  title.innerHTML = `<span>Scala 3 Playground</span><small>Compiler running entirely in your browser</small>`;
  brand.append(mark, title);

  const right = document.createElement('div');
  right.className = 'app-header__right';

  const pill = document.createElement('div');
  pill.className = 'status-pill';
  pill.dataset.state = 'loading';
  pill.setAttribute('aria-live', 'polite');
  pill.innerHTML = `<span class="status-pill__dot"></span><span class="status-pill__label">Loading…</span>`;

  const link = document.createElement('a');
  link.className = 'app-header__github';
  link.target = '_blank';
  link.rel = 'noopener';
  link.href = 'https://github.com/scala/scala3';
  link.innerHTML = 'GitHub ↗';

  right.append(pill, link);
  header.append(brand, right);

  const labelEl = pill.querySelector('.status-pill__label') as HTMLSpanElement;

  return {
    root: header,
    setStatus(s: Status) {
      pill.dataset.state = s;
      labelEl.textContent = ({
        loading: 'Loading…',
        ready: 'Ready',
        compiling: 'Compiling…',
        error: 'Error',
      } as const)[s];
    },
  };
}

interface CompilerLoadHooks {
  onJs(loaded: number, total: number | null): void;
  onCp(loaded: number, total: number | null): void;
  onLl(loaded: number, total: number | null): void;
}

async function loadCompiler(hooks: CompilerLoadHooks): Promise<void> {
  const [jsBuf, cpBuf, llBuf] = await Promise.all([
    fetchWithProgress('main.js', (e) => hooks.onJs(e.loaded, e.total)),
    fetchWithProgress('classpath.bin', (e) => hooks.onCp(e.loaded, e.total)),
    fetchWithProgress('linker-libs.bin', (e) => hooks.onLl(e.loaded, e.total)),
  ]);

  await installCompilerScript(jsBuf);
  compilerApi.loadClasspath(cpBuf);
  compilerApi.loadLinkerLibs(llBuf);
}

function buildPlayground(main: HTMLElement, header: HeaderHandle): void {
  let toolbar: ToolbarHandle | null = null;
  let editor: EditorHandle | null = null;
  let output: OutputPanelHandle | null = null;

  toolbar = createToolbar({
    parent: main,
    onSelectExample(ex) {
      editor?.setDoc(ex.source);
      editor?.setDiagnostics([]);
    },
    onAction(mode) {
      runAction(mode);
    },
  });

  // Two-column workspace: editor (left) + output (right)
  const workspace = document.createElement('div');
  workspace.className = 'workspace';
  main.appendChild(workspace);

  // Editor pane
  const editorPane = document.createElement('section');
  editorPane.className = 'editor-pane';
  const editorHeading = document.createElement('div');
  editorHeading.className = 'editor-pane__heading';
  editorHeading.innerHTML = `<span class="editor-pane__file">Main.scala</span>`;
  const editorMount = document.createElement('div');
  editorMount.className = 'editor-pane__cm';
  editorPane.append(editorHeading, editorMount);
  workspace.appendChild(editorPane);

  const initialEx = examples.find((e) => e.id === defaultExample) ?? examples[0];
  toolbar.setSelected(initialEx.id);

  // Output pane container
  const outputContainer = document.createElement('section');
  workspace.appendChild(outputContainer);

  output = createOutputPanel(outputContainer);
  editor = createEditor({
    parent: editorMount,
    initialDoc: initialEx.source,
    onChange: () => {
      if (!toolbar) return;
      const currentId = toolbar.selectedExampleId();
      if (currentId === 'custom') return;
      const currentEx = examples.find((e) => e.id === currentId);
      if (currentEx && editor && editor.getDoc() !== currentEx.source) {
        toolbar.setSelected('custom');
      }
    },
    onCompile: () => runAction('compile'),
    onRun: () => runAction('run'),
  });

  toolbar.setEnabled(true);

  function runAction(mode: Mode): void {
    if (!editor || !output || !toolbar) return;
    const source = editor.getDoc();
    toolbar.setBusy(true);
    header.setStatus('compiling');

    // Defer so the Compiling… pill paints before the synchronous compile.
    setTimeout(async () => {
      try {
        if (mode === 'compile') {
          handleCompileResult(compilerApi.compile(source));
        } else {
          const mainClass = extractMainName(source);
          handleRunResult(await compilerApi.compileAndLink(source, mainClass));
        }
        header.setStatus('ready');
      } catch (e) {
        const err = e as Error;
        output?.setOutput({
          logs: [],
          runtimeError: { message: err.message, stack: err.stack ?? '' },
        });
        output?.focus('output');
        header.setStatus('error');
      } finally {
        toolbar?.setBusy(false);
      }
    }, 10);
  }

  function handleCompileResult(diags: ReturnType<typeof compilerApi.compile>): void {
    if (!output || !editor) return;
    output.setDiagnostics(diags);
    editor.setDiagnostics(diags);
    output.setGeneratedJs(null);
    if (diags.length > 0) {
      output.focus('diagnostics');
    } else {
      output.setOutput({
        logs: [],
        placeholder: 'Compiled cleanly. Press ⌘↵ to run.',
      });
      output.focus('output');
    }
  }

  function handleRunResult(result: Awaited<ReturnType<typeof compilerApi.compileAndLink>>): void {
    if (!output || !editor) return;
    output.setDiagnostics(result.diagnostics);
    editor.setDiagnostics(result.diagnostics);

    if (!result.success || !result.js) {
      output.focus('diagnostics');
      output.setGeneratedJs(null);
      return;
    }

    output.setGeneratedJs(result.js);
    output.resetCanvas();
    const runResult = runWithCapture(result.js);

    // If user code did not show the canvas, hide the host so the panel
    // doesn't end with an empty box.
    const canvas = document.getElementById('appCanvas') as HTMLCanvasElement | null;
    if (!canvas || canvas.style.display !== 'block') output.hideCanvas();

    output.setOutput({
      logs: runResult.logs,
      runtimeError: runResult.error,
    });
    output.focus('output');
  }

  // Global error boundary — surface unexpected failures into the Output tab.
  window.addEventListener('error', (e) => {
    output?.setOutput({
      logs: [],
      runtimeError: { message: e.message, stack: (e.error as Error | undefined)?.stack ?? '' },
    });
    output?.focus('output');
    header.setStatus('error');
  });
  window.addEventListener('unhandledrejection', (e) => {
    const reason = e.reason as Error | undefined;
    output?.setOutput({
      logs: [],
      runtimeError: { message: String(reason?.message ?? e.reason), stack: reason?.stack ?? '' },
    });
    output?.focus('output');
    header.setStatus('error');
  });
}
