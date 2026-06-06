#!/usr/bin/env node
import assert from 'node:assert/strict';
import { spawn } from 'node:child_process';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const here = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(here, '..', '..');
const worker = path.join(root, 'bin', 'scala-repl-json');

const proc = spawn('bash', [worker], {
  cwd: root,
  stdio: ['pipe', 'pipe', 'pipe'],
});

let stdout = '';
let stderr = '';
const responses = [];
const waiters = [];

proc.stdout.setEncoding('utf8');
proc.stderr.setEncoding('utf8');

proc.stdout.on('data', (chunk) => {
  stdout += chunk;
  let idx;
  while ((idx = stdout.indexOf('\n')) >= 0) {
    const line = stdout.slice(0, idx);
    stdout = stdout.slice(idx + 1);
    if (line.length === 0) continue;
    const response = JSON.parse(line);
    const waiter = waiters.shift();
    if (waiter) waiter(response);
    else responses.push(response);
  }
});

proc.stderr.on('data', (chunk) => {
  stderr += chunk;
});

function nextResponse() {
  if (responses.length > 0) return Promise.resolve(responses.shift());
  return new Promise((resolve, reject) => {
    const timeout = setTimeout(() => {
      reject(new Error(`timed out waiting for worker response; stderr:\n${stderr}`));
    }, 120_000);
    waiters.push((response) => {
      clearTimeout(timeout);
      resolve(response);
    });
  });
}

async function request(value) {
  proc.stdin.write(typeof value === 'string' ? `${value}\n` : `${JSON.stringify(value)}\n`);
  return await nextResponse();
}

try {
  let res = await request({ op: 'eval', code: 'val x = 40' });
  assert.equal(res.op, 'eval');
  assert.equal(res.ok, true);
  assert.match(res.output, /val x: Int = 40/);
  assert.equal(res.stdout, '');
  assert.equal(res.stderr, '');
  assert.equal(res.stateVersion, 1);

  res = await request({ op: 'eval', code: 'x + 2' });
  assert.equal(res.ok, true);
  assert.match(res.output, /42/);
  assert.equal(res.stateVersion, 2);

  res = await request({ op: 'reset' });
  assert.deepEqual(res, { op: 'reset', ok: true, stateVersion: 0 });

  res = await request({ op: 'eval', code: 'x' });
  assert.equal(res.ok, false);
  assert.equal(res.stateVersion, 0);
  assert.match(res.output, /Not found|not found|x/);

  res = await request({ op: 'eval', code: 'throw new RuntimeException("boom")' });
  assert.equal(res.ok, false);
  assert.match(`${res.error}\n${res.output}`, /boom|RuntimeException/);

  res = await request({
    op: 'eval',
    code: 'Console.println("out"); Console.err.println("err"); 7',
  });
  assert.equal(res.ok, true);
  assert.match(res.output, /7/);
  assert.match(res.stdout, /out/);
  assert.match(res.stderr, /err/);

  res = await request('{not json');
  assert.deepEqual(res, { op: 'protocol', ok: false, error: 'invalid JSON' });

  res = await request({ op: 'bogus' });
  assert.deepEqual(res, { op: 'protocol', ok: false, error: 'unknown op' });

  res = await request({ op: 'shutdown' });
  assert.equal(res.op, 'shutdown');
  assert.equal(res.ok, true);

  proc.stdin.end();
  await new Promise((resolve, reject) => {
    proc.on('exit', (code) => {
      if (code === 0) resolve();
      else reject(new Error(`worker exited with ${code}; stderr:\n${stderr}`));
    });
  });

  console.log('PASS json-worker');
} catch (err) {
  proc.kill();
  throw err;
}
