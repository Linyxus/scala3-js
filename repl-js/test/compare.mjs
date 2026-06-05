// Compare a JS-REPL scripted-test transcript against its expected check file,
// replicating dotty's ReplTest/FileDiff semantics:
//   - keep only "non-blank" lines (lines containing a non-whitespace char),
//   - compare the remaining lines for exact equality (trailing spaces kept).
//
// Usage: node compare.mjs <expectedFile> <actualFile>
// Exit 0 on match, 1 on mismatch (printing a unified-ish diff).

import { readFileSync } from 'fs';

const [, , expFile, actFile] = process.argv;

const nonBlank = (s) =>
  s.split('\n').filter((l) => /\S/.test(l));

const exp = nonBlank(readFileSync(expFile, 'utf8'));
const act = nonBlank(readFileSync(actFile, 'utf8'));

let ok = exp.length === act.length && exp.every((l, i) => l === act[i]);

if (ok) process.exit(0);

// Minimal line-by-line diff.
const n = Math.max(exp.length, act.length);
console.log('  --- expected (check file) vs +++ actual (js repl) ---');
for (let i = 0; i < n; i++) {
  const e = exp[i];
  const a = act[i];
  if (e === a) {
    console.log('   ' + (e ?? ''));
  } else {
    if (e !== undefined) console.log('  -' + e);
    if (a !== undefined) console.log('  +' + a);
  }
}
process.exit(1);
