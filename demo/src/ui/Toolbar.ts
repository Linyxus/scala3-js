import { examples, defaultExample, type Example } from '../examples';

export type RunMode = 'compile' | 'run';

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
  select.disabled = true;
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

  // Action buttons
  const actionGroup = document.createElement('div');
  actionGroup.className = 'toolbar__group';

  const compileBtn = document.createElement('button');
  compileBtn.type = 'button';
  compileBtn.className = 'btn';
  compileBtn.disabled = true;
  compileBtn.innerHTML = `Compile <span class="btn__kbd">⌘⇧↵</span>`;

  const runBtn = document.createElement('button');
  runBtn.type = 'button';
  runBtn.className = 'btn btn--primary';
  runBtn.disabled = true;
  runBtn.innerHTML = `Run <span class="btn__kbd">⌘↵</span>`;

  compileBtn.addEventListener('click', () => opts.onAction('compile'));
  runBtn.addEventListener('click', () => opts.onAction('run'));

  actionGroup.append(compileBtn, runBtn);
  root.append(pickerGroup, spacer, actionGroup);
  opts.parent.appendChild(root);

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
  };
}
