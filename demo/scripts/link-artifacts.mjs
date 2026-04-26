#!/usr/bin/env node
// Find the latest *-nonbootstrapped/ directory under
// compiler-js/target/scala3-compiler-sjs/ and symlink the four compiler
// artifacts into demo/public/. Runs as a predev / prebuild hook.

import fs from 'node:fs';
import path from 'node:path';
import url from 'node:url';

const here = path.dirname(url.fileURLToPath(import.meta.url));
const demoRoot = path.resolve(here, '..');
const repoRoot = path.resolve(demoRoot, '..');
const targetRoot = path.join(repoRoot, 'compiler-js', 'target', 'scala3-compiler-sjs');
const publicDir = path.join(demoRoot, 'public');

function die(msg) {
  console.error(`[link-artifacts] ${msg}`);
  process.exit(1);
}

if (!fs.existsSync(targetRoot)) {
  die(
    `compiler-js target dir not found at ${targetRoot}\n` +
    `  Run from repo root: sbt 'project scala3-compiler-sjs' 'fastLinkJS; bundleLibs; packClasspath; packLinkerLibs'`,
  );
}

const candidates = fs
  .readdirSync(targetRoot, { withFileTypes: true })
  .filter((d) => d.isDirectory() && d.name.endsWith('-nonbootstrapped'))
  .map((d) => {
    const dir = path.join(targetRoot, d.name);
    return { name: d.name, dir, mtime: fs.statSync(dir).mtimeMs };
  })
  .sort((a, b) => b.mtime - a.mtime);

if (candidates.length === 0) {
  die(
    `No *-nonbootstrapped/ directory under ${targetRoot}\n` +
    `  Run: sbt 'project scala3-compiler-sjs' fastLinkJS`,
  );
}

const base = candidates[0];
console.log(`[link-artifacts] using ${base.name}`);

const sources = {
  'main.js': path.join(base.dir, 'scala3-compiler-fastopt', 'main.js'),
  'main.js.map': path.join(base.dir, 'scala3-compiler-fastopt', 'main.js.map'),
  'classpath.bin': path.join(base.dir, 'classpath.bin'),
  'linker-libs.bin': path.join(base.dir, 'linker-libs.bin'),
};

const required = ['main.js', 'classpath.bin', 'linker-libs.bin'];
const missing = required.filter((name) => !fs.existsSync(sources[name]));

if (missing.length > 0) {
  die(
    `missing artifacts: ${missing.join(', ')}\n` +
    `  Run: sbt 'project scala3-compiler-sjs' 'fastLinkJS; bundleLibs; packClasspath; packLinkerLibs'`,
  );
}

fs.mkdirSync(publicDir, { recursive: true });

for (const [name, src] of Object.entries(sources)) {
  const dest = path.join(publicDir, name);
  if (!fs.existsSync(src)) {
    // main.js.map is optional
    continue;
  }
  // Replace any existing symlink/file to avoid stale targets.
  try {
    const stat = fs.lstatSync(dest);
    if (stat.isSymbolicLink() || stat.isFile()) fs.unlinkSync(dest);
  } catch {
    // no-op: dest doesn't exist
  }
  try {
    fs.symlinkSync(src, dest, process.platform === 'win32' ? 'junction' : 'file');
  } catch (err) {
    // Fallback to copy if symlinks aren't permitted.
    if (err && err.code === 'EPERM') {
      fs.copyFileSync(src, dest);
    } else {
      throw err;
    }
  }
  const sizeMb = (fs.statSync(src).size / (1024 * 1024)).toFixed(1);
  console.log(`[link-artifacts]   ${name.padEnd(16)} -> ${path.relative(repoRoot, src)}  (${sizeMb} MB)`);
}

console.log('[link-artifacts] done');
