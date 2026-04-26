import { defineConfig } from 'vite';
import path from 'node:path';
import fs from 'node:fs';

const repoRoot = path.resolve(__dirname, '..');
const compilerJsTarget = path.join(repoRoot, 'compiler-js', 'target');

export default defineConfig({
  server: {
    fs: {
      allow: [path.resolve(__dirname), compilerJsTarget],
    },
  },
  build: {
    target: 'es2022',
    sourcemap: true,
    assetsInlineLimit: 0,
  },
  assetsInclude: ['**/*.bin'],
  resolve: {
    preserveSymlinks: false,
  },
  optimizeDeps: {
    include: ['codemirror', '@codemirror/view', '@codemirror/state', '@codemirror/lint', '@codemirror/language', '@codemirror/legacy-modes/mode/clike', '@codemirror/commands'],
  },
  // Suppress noisy warnings if the compiler artifacts aren't yet linked.
  define: {
    __COMPILER_JS_TARGET__: JSON.stringify(fs.existsSync(compilerJsTarget)),
  },
});
