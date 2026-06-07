#!/usr/bin/env node
// Comprehensive tests for dynamic eval(...) in the JS REPL.
//
// Ported/adapted from the JVM DynamicEvalTests, restricted to the foundation's
// capabilities (binding-based capture, in-context recompile, nested eval,
// evalSafe, caching). Deferred categories (private class-member access, capture
// checking, safe-mode) are noted at the bottom and covered by a separate task.
import { spawn } from 'node:child_process';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const here = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(here, '..', '..');
const worker = path.join(root, 'bin', 'scala-repl-json');

const proc = spawn('bash', [worker], { cwd: root, stdio: ['pipe', 'pipe', 'pipe'] });
let stdout = '', stderr = '';
const responses = [], waiters = [];
proc.stdout.setEncoding('utf8');
proc.stderr.setEncoding('utf8');
proc.stdout.on('data', (chunk) => {
  stdout += chunk;
  let idx;
  while ((idx = stdout.indexOf('\n')) >= 0) {
    const line = stdout.slice(0, idx); stdout = stdout.slice(idx + 1);
    if (line.length === 0) continue;
    const r = JSON.parse(line);
    const w = waiters.shift();
    if (w) w(r); else responses.push(r);
  }
});
proc.stderr.on('data', (c) => { stderr += c; });
function nextResponse() {
  if (responses.length > 0) return Promise.resolve(responses.shift());
  return new Promise((resolve, reject) => {
    const t = setTimeout(() => reject(new Error(`timeout; stderr:\n${stderr}`)), 120_000);
    waiters.push((r) => { clearTimeout(t); resolve(r); });
  });
}
async function request(value) {
  proc.stdin.write(`${JSON.stringify(value)}\n`);
  return await nextResponse();
}
const evalReq = (code) => request({ op: 'eval', code });
const reset = () => request({ op: 'reset' });

let pass = 0, fail = 0;
const fails = [];

/** Run `lines` (all but last are setup), assert the LAST response matches. */
async function test(name, lines, re, { expectError = false } = {}) {
  await reset();
  let res;
  for (const code of lines) res = await evalReq(code);
  const hay = `${res.output}\n${res.error ?? ''}\n${res.stdout ?? ''}`;
  const okFlag = expectError ? res.ok === false : true;
  if (re.test(hay) && okFlag) { pass++; }
  else {
    fail++;
    fails.push(`${name}\n    expected ${re}${expectError ? ' (ok:false)' : ''}\n    got ok=${res.ok} output=${JSON.stringify(res.output)} error=${JSON.stringify(res.error)}`);
  }
}

try {
  // === 1. Return types via the polymorphic T ===
  await test('returnsInt', ['val r: Int = eval("1 + 2")'], /val r: Int = 3/);
  await test('returnsLong', ['val r: Long = eval("1000000000000L")'], /val r: Long = 1000000000000L/);
  await test('returnsDouble', ['val r: Double = eval("math.sqrt(16.0)")'], /val r: Double = 4(\.0)?/);
  await test('returnsBoolean', ['val r: Boolean = eval("1 < 2 && 3 != 4")'], /val r: Boolean = true/);
  await test('returnsChar', ['val r: Char = eval("\'a\'.toUpper")'], /val r: Char = 'A'/);
  await test('returnsString', ['val r: String = eval("\\"ab\\" + \\"cd\\"")'], /val r: String = "abcd"/);
  await test('returnsList', ['val r: List[Int] = eval("List(1,2,3).map(_ * 2)")'], /List\(2, 4, 6\)/);
  await test('returnsTuple', ['val r: (Int, String) = eval("(1, \\"x\\")")'], /\(1, "?x"?\)|\(1,x\)/);
  await test('returnsOption', ['val r: Option[Int] = eval("Some(5)")'], /Some\(5\)/);
  await test('returnsUnit', ['val r: Unit = eval("()")'], /r: Unit/);

  // === 2. Explicit [T] type argument ===
  await test('typeArg', ['val r = eval[Int]("21 * 2")'], /Int = 42/);

  // === 3. Runtime-computed body string ===
  await test('bodyFromVal', ['val code = "10 + 5"', 'val r: Int = eval(code)'], /Int = 15/);
  await test('bodyFromInterp', ['val n = 6', 'val r: Int = eval(s"$n * 7")'], /Int = 42/);

  // === 4. Capturing previous REPL lines ===
  await test('captureVal', ['val n = 10', 'val r: Int = eval("n * 4")'], /Int = 40/);
  await test('captureDefByName', ['def f(j: Int) = j + 1', 'val rs = List("f").map(fn => eval[Int](s"$fn(1)"))'], /List\(2\)/);
  await test('captureGivenSummon', ['given Int = 7', 'val r: Int = eval("summon[Int] * 6")'], /Int = 42/);
  await test('captureVarRead', ['var n = 10', 'val r: Int = eval("n + 5")'], /Int = 15/);

  // === 5. Capturing locals / lambda params ===
  await test('lambdaParam', ['val r = List(1,2,3).map(z => eval[Int]("z * z"))'], /List\(1, 4, 9\)/);
  await test('nestedLambda', ['val xss = List(List(0,1), List(2))', 'val i = 2',
    'val r = xss.flatMap(xs => xs.map(x => eval[Int]("(x + xs.length) * i")))'], /List\(4, 6, 6\)/);
  await test('blockLocalVal', ['val r = { val a = 8; eval[Int]("a + 1") }'], /Int = 9/);
  await test('methodParam', ['def g(p: Int): Int = eval[Int]("p * 10")', 'val r = g(4)'], /Int = 40/);

  // === 6. var write-through via the VarRef facade ===
  await test('varWriteThrough', ['var n = 10', 'eval[Unit]("n = n + 5")', 'val r = n'], /Int = 15/);

  // === 7. capture a block-local def (eta-expansion) ===
  await test('blockLocalDef', ['val r = { def h(x: Int) = x * 3; eval[Int]("h(4)") }'], /Int = 12/);

  // === 8. Nested eval ===
  await test('nestedEval', ['val r = List(1,2,3).map(x => eval[Int](s"List($x, ${x*2}).map(y => eval[Int](\\"y + 1\\")).sum"))'], /List\(5, 8, 11\)/);

  // === 9. Type errors are real compiler diagnostics ===
  await test('typeMismatchPin', ['val r = eval[Int]("\\"abc\\"")'], /Found:.*String[\s\S]*Required:.*Int/, { expectError: true });
  await test('unknownIdent', ['val r = eval[Int]("nope123")'], /[Nn]ot found.*nope123/, { expectError: true });
  await test('parseError', ['val r = eval[Int]("1 +")'], /error|expected/i, { expectError: true });

  // === 10. evalSafe — non-throwing ===
  await test('evalSafeSuccess', ['val r = evalSafe[Int]("3 + 4").getOrElse(-1)'], /Int = 7/);
  await test('evalSafeFailure', ['val r = evalSafe[Int]("not code").isFailure'], /Boolean = true/);
  await test('evalSafeErrors', ['val r = evalSafe[Int]("1 +") match { case scala.runtime.eval.EvalResult.Failure(f) => f.errors.length > 0; case _ => false }'], /true/);

  // === 11. Body runtime exception propagates (not a compile failure) ===
  await test('bodyThrows', ['val r = eval[Int]("1 / 0")'], /ArithmeticException|by zero/, { expectError: true });

  // === 12. Caching: same call site in a loop recompiles once, varies value ===
  await test('cacheLoop', ['val r = (1 to 4).toList.map(k => eval[Int]("k * k"))'], /List\(1, 4, 9, 16\)/);

  // === 13. Generator/closure form: eval { ctx => code } ===
  await test('closureForm', ['val r = eval[Int] { ctx => "100 + 1" }'], /Int = 101/);
  await test('closureCtxEncl', ['val n = 5', 'val r = eval[Int] { ctx => "n + 1" }'], /Int = 6/);

  console.log(`\n${pass}/${pass + fail} eval tests passed`);
  if (fail > 0) { console.log('\nFAILURES:\n' + fails.map(f => '  - ' + f).join('\n')); process.exitCode = 1; }
  await request({ op: 'shutdown' });
  proc.stdin.end();
} catch (err) {
  proc.kill();
  console.error(err);
  console.error('STDERR:\n' + stderr.slice(-2000));
  process.exitCode = 1;
}
