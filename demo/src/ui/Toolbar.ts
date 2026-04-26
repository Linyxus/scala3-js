import { examples, defaultExample, type Example } from '../examples';

export type RunMode = 'compile' | 'run';
export type LoaderKind = 'js' | 'cp' | 'll';

export interface ToolbarOptions {
  parent: HTMLElement;
  onSelectExample: (ex: Example) => void;
  onAction: (mode: RunMode) => void;
}

export interface ToolbarHandle {
  setEnabled(enabled: boolean): void;
  setBusy(busy: boolean): void;
  selectedExampleId(): string;
  setSelected(id: string): void;
  /** Update aggregate compiler-download progress while in loading state. */
  setLoadingProgress(kind: LoaderKind, loaded: number, total: number | null): void;
  /** Switch the right-side action slot from progress bar to the Compile/Run buttons. */
  setReady(): void;
  /** Replace the action slot with an error message. */
  setError(msg: string): void;
}

export function createToolbar(opts: ToolbarOptions): ToolbarHandle {
  const root = document.createElement('div');
  root.className = 'toolbar';

  // Example picker
  const pickerGroup = document.createElement('div');
  pickerGroup.className = 'toolbar__group';
  const label = document.createElement('label');
  label.htmlFor = 'example-select';
  label.textContent = 'Example';
  const select = document.createElement('select');
  select.id = 'example-select';
  for (const ex of examples) {
    const opt = document.createElement('option');
    opt.value = ex.id;
    opt.textContent = ex.label;
    if (ex.id === defaultExample) opt.selected = true;
    select.appendChild(opt);
  }
  const customOpt = document.createElement('option');
  customOpt.value = 'custom';
  customOpt.textContent = 'Custom';
  select.appendChild(customOpt);

  select.addEventListener('change', () => {
    const id = select.value;
    const ex = examples.find((e) => e.id === id);
    if (ex) opts.onSelectExample(ex);
  });

  pickerGroup.append(label, select);

  // Spacer
  const spacer = document.createElement('div');
  spacer.className = 'toolbar__spacer';

  // Action slot — starts in loading mode, swaps to buttons via setReady().
  const actionSlot = document.createElement('div');
  actionSlot.className = 'toolbar__group toolbar__actions';

  // Loading widget: text + bar + percent.
  const loadingWidget = document.createElement('div');
  loadingWidget.className = 'toolbar-loader';
  const loadingLabel = document.createElement('span');
  loadingLabel.className = 'toolbar-loader__label';
  loadingLabel.textContent = 'Loading compiler…';
  const loadingBar = document.createElement('div');
  loadingBar.className = 'toolbar-loader__bar';
  loadingBar.setAttribute('role', 'progressbar');
  loadingBar.setAttribute('aria-valuemin', '0');
  loadingBar.setAttribute('aria-valuemax', '100');
  const loadingFill = document.createElement('div');
  loadingFill.className = 'toolbar-loader__fill';
  loadingBar.appendChild(loadingFill);
  const loadingPct = document.createElement('span');
  loadingPct.className = 'toolbar-loader__pct';
  loadingPct.textContent = '0%';
  loadingWidget.append(loadingLabel, loadingBar, loadingPct);
  actionSlot.appendChild(loadingWidget);

  // Buttons (built but not mounted; setReady() swaps them in).
  const compileBtn = document.createElement('button');
  compileBtn.type = 'button';
  compileBtn.className = 'btn';
  compileBtn.disabled = true;
  compileBtn.textContent = 'Compile';

  const runBtn = document.createElement('button');
  runBtn.type = 'button';
  runBtn.className = 'btn btn--primary';
  runBtn.disabled = true;
  runBtn.textContent = 'Run';

  compileBtn.addEventListener('click', () => opts.onAction('compile'));
  runBtn.addEventListener('click', () => opts.onAction('run'));

  root.append(pickerGroup, spacer, actionSlot);
  opts.parent.appendChild(root);

  // Aggregate progress state across the three downloads.
  const progress: Record<LoaderKind, { loaded: number; total: number | null }> = {
    js: { loaded: 0, total: null },
    cp: { loaded: 0, total: null },
    ll: { loaded: 0, total: null },
  };

  function refreshProgress(): void {
    let loaded = 0;
    let total = 0;
    let known = true;
    for (const k of Object.keys(progress) as LoaderKind[]) {
      loaded += progress[k].loaded;
      if (progress[k].total == null) known = false;
      else total += progress[k].total!;
    }
    if (known && total > 0) {
      const pct = Math.max(0, Math.min(100, Math.round((loaded / total) * 100)));
      loadingFill.style.width = pct + '%';
      loadingPct.textContent = `${pct}%`;
      loadingBar.setAttribute('aria-valuenow', String(pct));
    } else {
      // Indeterminate-ish: pulse fill, show MB transferred so far.
      loadingFill.style.width = '40%';
      loadingPct.textContent = `${(loaded / (1024 * 1024)).toFixed(1)} MB`;
    }
  }

  return {
    setEnabled(enabled: boolean): void {
      select.disabled = !enabled;
      compileBtn.disabled = !enabled;
      runBtn.disabled = !enabled;
    },
    setBusy(busy: boolean): void {
      compileBtn.disabled = busy;
      runBtn.disabled = busy;
    },
    selectedExampleId(): string {
      return select.value;
    },
    setSelected(id: string): void {
      select.value = id;
    },
    setLoadingProgress(kind, loaded, total): void {
      progress[kind] = { loaded, total };
      refreshProgress();
    },
    setReady(): void {
      actionSlot.replaceChildren(compileBtn, runBtn);
      compileBtn.disabled = false;
      runBtn.disabled = false;
    },
    setError(msg: string): void {
      const errBox = document.createElement('span');
      errBox.className = 'toolbar-loader__error';
      errBox.textContent = msg;
      actionSlot.replaceChildren(errBox);
    },
  };
}
