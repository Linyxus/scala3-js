// CodeMirror 6 editor with a light theme, Scala highlighting, and lint
// markers driven by compiler diagnostics.

import { EditorView, keymap, lineNumbers, highlightActiveLine, highlightActiveLineGutter } from '@codemirror/view';
import { EditorState, type Extension } from '@codemirror/state';
import { defaultKeymap, history, historyKeymap, indentWithTab } from '@codemirror/commands';
import { bracketMatching, indentOnInput, syntaxHighlighting, defaultHighlightStyle, foldGutter, foldKeymap } from '@codemirror/language';
import { setDiagnostics, type Diagnostic as CmDiagnostic, lintGutter } from '@codemirror/lint';
import { StreamLanguage } from '@codemirror/language';
import { scala } from '@codemirror/legacy-modes/mode/clike';

import type { Diagnostic } from '../types';
import { stripAnsi } from '../util/ansi';

const lightTheme = EditorView.theme(
  {
    '&': {
      color: 'var(--text)',
      backgroundColor: 'var(--surface)',
      height: '100%',
    },
    '.cm-content': {
      caretColor: 'var(--accent)',
      padding: '12px 0',
    },
    '.cm-cursor, .cm-dropCursor': { borderLeftColor: 'var(--accent)' },
    '&.cm-focused .cm-selectionBackground, ::selection': {
      backgroundColor: 'var(--accent-soft)',
    },
    '.cm-gutters': {
      backgroundColor: 'var(--surface)',
      color: 'var(--text-soft)',
      border: 'none',
      borderRight: '1px solid var(--border)',
    },
    '.cm-activeLineGutter, .cm-activeLine': {
      backgroundColor: 'rgb(79 70 229 / 4%)',
    },
    '.cm-lineNumbers .cm-gutterElement': { padding: '0 14px 0 6px' },
    '.cm-foldGutter .cm-gutterElement': { color: 'var(--text-soft)' },
    '.cm-scroller': { fontFamily: 'var(--font-mono)', lineHeight: '1.6' },
    '.cm-tooltip': {
      backgroundColor: 'var(--surface)',
      borderColor: 'var(--border)',
      color: 'var(--text)',
    },
    '.cm-matchingBracket, .cm-nonmatchingBracket': {
      backgroundColor: 'var(--accent-soft)',
      outline: '1px solid var(--accent)',
    },
  },
  { dark: false },
);

export interface EditorOptions {
  parent: HTMLElement;
  initialDoc: string;
  onChange?: (doc: string) => void;
  onCompile?: () => void;
  onRun?: () => void;
}

export interface EditorHandle {
  view: EditorView;
  getDoc(): string;
  setDoc(text: string): void;
  setDiagnostics(diags: Diagnostic[]): void;
}

export function createEditor(opts: EditorOptions): EditorHandle {
  const userKeymap = keymap.of([
    {
      key: 'Mod-Enter',
      preventDefault: true,
      run: () => {
        opts.onRun?.();
        return true;
      },
    },
    {
      key: 'Mod-Shift-Enter',
      preventDefault: true,
      run: () => {
        opts.onCompile?.();
        return true;
      },
    },
  ]);

  const extensions: Extension[] = [
    lineNumbers(),
    foldGutter(),
    history(),
    indentOnInput(),
    bracketMatching(),
    highlightActiveLine(),
    highlightActiveLineGutter(),
    syntaxHighlighting(defaultHighlightStyle, { fallback: true }),
    StreamLanguage.define(scala),
    lintGutter(),
    keymap.of([...defaultKeymap, ...historyKeymap, ...foldKeymap, indentWithTab]),
    userKeymap,
    lightTheme,
    EditorView.updateListener.of((update) => {
      if (update.docChanged && opts.onChange) {
        opts.onChange(update.state.doc.toString());
      }
    }),
  ];

  const view = new EditorView({
    parent: opts.parent,
    state: EditorState.create({ doc: opts.initialDoc, extensions }),
  });

  const handle: EditorHandle = {
    view,
    getDoc: () => view.state.doc.toString(),
    setDoc: (text: string) => {
      view.dispatch({
        changes: { from: 0, to: view.state.doc.length, insert: text },
      });
    },
    setDiagnostics: (diags: Diagnostic[]) => {
      const doc = view.state.doc;
      const cmDiags: CmDiagnostic[] = [];
      for (const d of diags) {
        if (d.line < 1 || d.line > doc.lines) continue;
        const lineInfo = doc.line(d.line);
        const col = Math.max(0, (d.column ?? 1) - 1);
        const from = Math.min(lineInfo.from + col, lineInfo.to);
        cmDiags.push({
          from,
          to: lineInfo.to,
          severity: d.severity === 'info' ? 'info' : (d.severity as 'warning' | 'error'),
          message: stripAnsi(d.message),
        });
      }
      view.dispatch(setDiagnostics(view.state, cmDiags));
    },
  };

  return handle;
}
