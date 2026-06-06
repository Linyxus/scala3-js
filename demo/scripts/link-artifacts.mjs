#!/usr/bin/env node
// Find the latest *-nonbootstrapped/ directory under
// compiler-js-browser/target/scala3-compiler-browser-sjs/ and
// compiler-js-cli/target/scala3-compiler-cli-sjs/, then symlink the compiler
// artifacts into demo/public/. Runs as a predev / prebuild hook.

import fs from 'node:fs';
import path from 'node:path';
import url from 'node:url';

const here = path.dirname(url.fileURLToPath(import.meta.url));
const demoRoot = path.resolve(here, '..');
const repoRoot = path.resolve(demoRoot, '..');
const browserTargetRoot = path.join(repoRoot, 'compiler-js-browser', 'target', 'scala3-compiler-browser-sjs');
const cliTargetRoot = path.join(repoRoot, 'compiler-js-cli', 'target', 'scala3-compiler-cli-sjs');
const publicDir = path.join(demoRoot, 'public');

function die(msg) {
  console.error(`[link-artifacts] ${msg}`);
  process.exit(1);
}

if (!fs.existsSync(browserTargetRoot)) {
  die(
    `compiler browser target dir not found at ${browserTargetRoot}\n` +
    `  Run from repo root: sbt --client 'scala3-compiler-browser-sjs/fastLinkJS' 'scala3-compiler-cli-sjs/packClasspath' 'scala3-compiler-cli-sjs/packLinkerLibs'`,
  );
}

if (!fs.existsSync(cliTargetRoot)) {
  die(
    `compiler CLI target dir not found at ${cliTargetRoot}\n` +
    `  Run from repo root: sbt --client 'scala3-compiler-browser-sjs/fastLinkJS' 'scala3-compiler-cli-sjs/packClasspath' 'scala3-compiler-cli-sjs/packLinkerLibs'`,
  );
}

function latestBase(targetRoot, label) {
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
      `  Run: sbt --client '${label}/fastLinkJS'`,
    );
  }

  return candidates[0];
}

const browserBase = latestBase(browserTargetRoot, 'scala3-compiler-browser-sjs');
const cliBase = latestBase(cliTargetRoot, 'scala3-compiler-cli-sjs');
console.log(`[link-artifacts] using browser ${browserBase.name}, assets ${cliBase.name}`);

const sources = {
  'main.js': path.join(browserBase.dir, 'scala3-compiler-browser-fastopt', 'main.js'),
  'main.js.map': path.join(browserBase.dir, 'scala3-compiler-browser-fastopt', 'main.js.map'),
  'classpath.bin': path.join(cliBase.dir, 'classpath.bin'),
  'linker-libs.bin': path.join(cliBase.dir, 'linker-libs.bin'),
};

const required = ['main.js', 'classpath.bin', 'linker-libs.bin'];
const missing = required.filter((name) => !fs.existsSync(sources[name]));

if (missing.length > 0) {
  die(
    `missing artifacts: ${missing.join(', ')}\n` +
    `  Run: sbt --client 'scala3-compiler-browser-sjs/fastLinkJS' 'scala3-compiler-cli-sjs/packClasspath' 'scala3-compiler-cli-sjs/packLinkerLibs'`,
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
