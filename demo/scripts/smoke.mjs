#!/usr/bin/env bun
// End-to-end smoke test for the Scala 3 playground.
//
// Drives the page via Playwright Chromium:
//   1. waits for the status pill to read "Ready"
//   2. for each example, picks it from the dropdown, presses Run, waits
//      for either output or a runtime error
//   3. captures a screenshot per example into demo/screenshots/
//   4. types a deliberate type error and presses Compile, verifies the
//      Diagnostics tab gets focused with at least one error diagnostic
//   5. reports any console errors / failed network requests
//
// Run from repo root or demo/ — uses absolute paths.
//
// Requires the dev server to be running at http://localhost:5173.

import { chromium } from 'playwright';
import fs from 'node:fs';
import path from 'node:path';
import url from 'node:url';

const here = path.dirname(url.fileURLToPath(import.meta.url));
const demoRoot = path.resolve(here, '..');
const screenshotsDir = path.join(demoRoot, 'screenshots');
fs.mkdirSync(screenshotsDir, { recursive: true });

const BASE_URL = process.env.PLAYGROUND_URL ?? 'http://localhost:5173';
const READY_TIMEOUT_MS = 120_000;
const COMPILE_TIMEOUT_MS = 60_000;

const EXAMPLES = ['hello', 'canvas', 'spirograph', 'gameoflife'];

const consoleErrors = [];
const networkFailures = [];

function header(msg) {
  console.log(`\n=== ${msg} ===`);
}

async function waitForStatus(page, want, timeoutMs = READY_TIMEOUT_MS) {
  await page.waitForFunction(
    (w) => {
      const pill = document.querySelector('.status-pill');
      return pill && pill.dataset.state === w;
    },
    want,
    { timeout: timeoutMs },
  );
}

// Wait until the primary Run button is no longer disabled.
async function waitIdle(page, timeoutMs = COMPILE_TIMEOUT_MS) {
  await page.waitForFunction(
    () => {
      const btn = document.querySelector('button.btn--primary');
      return btn && !btn.hasAttribute('disabled');
    },
    null,
    { timeout: timeoutMs },
  );
}

async function main() {
  const browser = await chromium.launch();
  const context = await browser.newContext({ viewport: { width: 1280, height: 900 } });
  const page = await context.newPage();

  page.on('console', (msg) => {
    if (msg.type() === 'error') {
      consoleErrors.push(`[console.error] ${msg.text()}`);
    }
  });
  page.on('pageerror', (err) => {
    consoleErrors.push(`[pageerror] ${err.message}`);
  });
  page.on('requestfailed', (req) => {
    const failure = req.failure();
    networkFailures.push(`${req.url()} — ${failure?.errorText ?? 'unknown'}`);
  });

  header(`Loading ${BASE_URL}`);
  await page.goto(BASE_URL, { waitUntil: 'domcontentloaded' });

  console.log('Waiting for compiler bundle to load (up to 120 s)…');
  await waitForStatus(page, 'ready');
  console.log('Ready ✓');

  await page.screenshot({ path: path.join(screenshotsDir, '00-ready.png'), fullPage: true });

  // --- Run each built-in example ---
  for (const id of EXAMPLES) {
    header(`Run example: ${id}`);
    await page.selectOption('#example-select', id);
    // give the editor a tick to flush
    await page.waitForTimeout(150);
    // Click the primary Run button (kbd is ⌘↵)
    await page.click('button.btn--primary');
    await waitIdle(page);

    // Output tab should be auto-focused now
    const outputText = await page.textContent('.tabs__panel[data-active="true"] .tabs__output');
    console.log('  output preview:', (outputText ?? '').replace(/\s+/g, ' ').slice(0, 120));

    await page.screenshot({
      path: path.join(screenshotsDir, `01-${id}.png`),
      fullPage: true,
    });
  }

  // --- Compile a deliberate type error ---
  header('Compile a deliberate type error');
  const badSource = `object Bad:\n  val x: Int = "not an int"\n`;
  await page.selectOption('#example-select', 'hello'); // reset
  await page.waitForTimeout(100);
  // Replace the editor doc by Cmd-A + delete + type
  await page.click('.cm-content');
  await page.keyboard.press('Meta+A');
  await page.keyboard.press('Backspace');
  await page.keyboard.type(badSource, { delay: 0 });
  // Compile (no run)
  await page.click('button.btn:not(.btn--primary)');
  await waitIdle(page);

  // Diagnostics tab should be focused with a badge
  const diagPanel = await page.$('.tabs__panel[data-active="true"]');
  const diagText = (await diagPanel?.textContent()) ?? '';
  if (!/Type Mismatch|Required: Int|E007/.test(diagText)) {
    consoleErrors.push(`Diagnostics didn't render expected error text. Got: ${diagText.slice(0, 200)}`);
  } else {
    console.log('  ✓ Diagnostic rendered:', diagText.match(/E007[^\n]*/)?.[0] ?? '(present)');
  }

  await page.screenshot({ path: path.join(screenshotsDir, '02-error.png'), fullPage: true });

  // --- Report ---
  header('Summary');
  console.log(`Console errors: ${consoleErrors.length}`);
  for (const e of consoleErrors) console.log('  -', e);
  console.log(`Network failures: ${networkFailures.length}`);
  for (const e of networkFailures) console.log('  -', e);
  console.log(`Screenshots: ${screenshotsDir}`);
  for (const f of fs.readdirSync(screenshotsDir).sort()) {
    console.log('  -', f);
  }

  await browser.close();

  if (consoleErrors.length > 0 || networkFailures.length > 0) {
    process.exit(1);
  }
}

main().catch((e) => {
  console.error('FATAL:', e);
  process.exit(2);
});
