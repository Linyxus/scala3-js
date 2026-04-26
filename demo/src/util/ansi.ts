// Convert a string with ANSI SGR escape codes into HTML using `.ansi-*`
// classes (defined in styles/app.css). Faithful port of the function at
// compiler-js/browser-test/index.html:383-411 — the only change is TS types.
//
// Code 0 (or an empty parameter) closes the open span without opening a new
// one. Code 1 contributes `.ansi-bold`. Color codes 30-37 / 90-97 map to
// foreground / bright-foreground classes. Unknown codes are ignored.

const COLOR_MAP: Record<string, string> = {
  '30': 'ansi-black',
  '31': 'ansi-red',
  '32': 'ansi-green',
  '33': 'ansi-yellow',
  '34': 'ansi-blue',
  '35': 'ansi-magenta',
  '36': 'ansi-cyan',
  '37': 'ansi-white',
  '90': 'ansi-bright-red', // intentionally same as 91 — see browser-test:387
  '91': 'ansi-bright-red',
  '92': 'ansi-bright-green',
  '93': 'ansi-bright-yellow',
  '94': 'ansi-bright-blue',
  '95': 'ansi-bright-magenta',
  '96': 'ansi-bright-cyan',
  '97': 'ansi-bright-white',
};

const SGR_RE = /\x1b\[([0-9;]*)m/;

export function ansiToHtml(text: string): string {
  let result = '';
  let open = false;
  const parts = text.split(SGR_RE);
  for (let i = 0; i < parts.length; i++) {
    if (i % 2 === 0) {
      result += parts[i].replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
    } else {
      const raw = parts[i];
      const codes = raw.split(';');
      if (open) {
        result += '</span>';
        open = false;
      }
      if (raw === '0' || raw === '') continue;
      const classes: string[] = [];
      for (const c of codes) {
        if (c === '1') classes.push('ansi-bold');
        else if (COLOR_MAP[c]) classes.push(COLOR_MAP[c]);
      }
      if (classes.length > 0) {
        result += `<span class="${classes.join(' ')}">`;
        open = true;
      }
    }
  }
  if (open) result += '</span>';
  return result;
}

/** Strip ANSI SGR escapes for plain-text contexts (e.g. CodeMirror lint markers). */
export function stripAnsi(text: string): string {
  return text.replace(/\x1b\[[0-9;]*m/g, '');
}
