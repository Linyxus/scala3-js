// Tabbed output panel: Output / Diagnostics / Generated JS.
// Exposes simple setters; auto-switches tabs when the App calls focusFor().

import { ansiToHtml } from '../util/ansi';
import { escapeHtml } from '../util/escapeHtml';
import type { Diagnostic } from '../types';

export type TabId = 'output' | 'diagnostics' | 'js';

export interface OutputPanelHandle {
  /** Render runtime output (logs, runtime errors, "no output" message). */
  setOutput(opts: { logs: string[]; runtimeError?: { message: string; stack: string } | null; placeholder?: string }): void;
  setDiagnostics(diags: Diagnostic[]): void;
  setGeneratedJs(js: string | null): void;
  /** Make a particular tab active. */
  focus(tab: TabId): void;
  /** Reveal the canvas inside the Output tab and reset its size. */
  resetCanvas(): void;
  /** Hide the canvas if it isn't being used. */
  hideCanvas(): void;
}

interface PanelDef {
  id: TabId;
  label: string;
}

const PANEL_DEFS: PanelDef[] = [
  { id: 'output', label: 'Output' },
  { id: 'diagnostics', label: 'Diagnostics' },
  { id: 'js', label: 'Generated JS' },
];

export function createOutputPanel(parent: HTMLElement): OutputPanelHandle {
  const root = document.createElement('div');
  root.className = 'output-pane';

  // Tab bar
  const tabs = document.createElement('div');
  tabs.className = 'tabs';
  tabs.setAttribute('role', 'tablist');

  const panels: Record<TabId, HTMLElement> = {} as Record<TabId, HTMLElement>;
  const tabButtons: Record<TabId, HTMLButtonElement> = {} as Record<TabId, HTMLButtonElement>;
  const badges: Partial<Record<TabId, HTMLElement>> = {};

  for (const def of PANEL_DEFS) {
    const btn = document.createElement('button');
    btn.type = 'button';
    btn.className = 'tabs__tab';
    btn.setAttribute('role', 'tab');
    btn.textContent = def.label;

    if (def.id === 'diagnostics') {
      const badge = document.createElement('span');
      badge.className = 'tabs__badge';
      badge.style.display = 'none';
      btn.appendChild(badge);
      badges.diagnostics = badge;
    }

    btn.addEventListener('click', () => focus(def.id));
    tabs.appendChild(btn);
    tabButtons[def.id] = btn;
  }

  root.appendChild(tabs);

  // Output panel (with canvas inside)
  const outputPanel = document.createElement('div');
  outputPanel.className = 'tabs__panel';
  outputPanel.setAttribute('role', 'tabpanel');
  panels.output = outputPanel;

  const outputBody = document.createElement('pre');
  outputBody.className = 'tabs__output';
  outputPanel.appendChild(outputBody);

  const canvasHost = document.createElement('div');
  canvasHost.className = 'canvas-host';
  const canvas = document.createElement('canvas');
  canvas.id = 'appCanvas';
  canvas.width = 600;
  canvas.height = 400;
  canvasHost.appendChild(canvas);
  outputPanel.appendChild(canvasHost);

  // Diagnostics panel
  const diagPanel = document.createElement('div');
  diagPanel.className = 'tabs__panel';
  diagPanel.setAttribute('role', 'tabpanel');
  panels.diagnostics = diagPanel;

  // JS panel
  const jsPanel = document.createElement('div');
  jsPanel.className = 'tabs__panel';
  jsPanel.setAttribute('role', 'tabpanel');
  panels.js = jsPanel;

  root.append(outputPanel, diagPanel, jsPanel);
  parent.appendChild(root);

  let currentTab: TabId = 'output';
  function focus(tab: TabId): void {
    currentTab = tab;
    for (const def of PANEL_DEFS) {
      const isActive = def.id === tab;
      panels[def.id].dataset.active = String(isActive);
      tabButtons[def.id].setAttribute('aria-selected', String(isActive));
    }
  }
  focus('output');

  // initial placeholders
  outputBody.innerHTML = '<span class="tabs__placeholder">Run your program to see its output here.</span>';
  diagPanel.innerHTML = '<span class="tabs__placeholder">Compile your program to see diagnostics here.</span>';
  jsPanel.innerHTML = '<span class="tabs__placeholder">Linked JavaScript appears here after a successful run.</span>';

  return {
    setOutput({ logs, runtimeError, placeholder }) {
      let html = '';
      if (logs.length > 0) {
        html += `<span class="tabs__divider">Program output</span>\n`;
        html += logs.map(escapeHtml).join('\n');
      }
      if (runtimeError) {
        if (logs.length > 0) html += '\n';
        html += `<span class="ansi-red ansi-bold">Runtime error:</span> `;
        html += `<span class="ansi-red">${escapeHtml(runtimeError.message)}</span>`;
        if (runtimeError.stack) {
          html += `\n<span class="ansi-bright-white">${escapeHtml(runtimeError.stack)}</span>`;
        }
      }
      if (!logs.length && !runtimeError) {
        if (placeholder) {
          html = `<span class="ansi-green">${escapeHtml(placeholder)}</span>`;
        } else {
          html = `<span class="tabs__placeholder">Program executed successfully (no output).</span>`;
        }
      }
      outputBody.innerHTML = html;
    },
    setDiagnostics(diags) {
      // Update badge
      const badge = badges.diagnostics!;
      const errorCount = diags.filter((d) => d.severity === 'error').length;
      if (diags.length === 0) {
        badge.style.display = 'none';
        diagPanel.innerHTML = `<span class="tabs__placeholder">No diagnostics — clean compile.</span>`;
        return;
      }
      badge.style.display = '';
      badge.textContent = String(diags.length);
      badge.className = errorCount > 0 ? 'tabs__badge tabs__badge--err' : 'tabs__badge';

      const html = diags
        .map((d) => `<div class="tabs__output">${ansiToHtml(d.message)}</div>`)
        .join('');
      diagPanel.innerHTML = html;
    },
    setGeneratedJs(js) {
      if (!js) {
        jsPanel.innerHTML = `<span class="tabs__placeholder">No JavaScript yet — run a program first.</span>`;
        return;
      }
      const pre = document.createElement('pre');
      pre.className = 'tabs__output';
      pre.textContent = js;
      jsPanel.replaceChildren(pre);
    },
    focus,
    resetCanvas() {
      const ctx = canvas.getContext('2d');
      ctx?.clearRect(0, 0, canvas.width, canvas.height);
      canvas.width = 600;
      canvas.height = 400;
      canvas.style.display = '';
      canvasHost.dataset.visible = 'true';
    },
    hideCanvas() {
      canvasHost.dataset.visible = 'false';
    },
  };
  // currentTab kept for debugging if needed
  void currentTab;
}
