// Initial loader card with three progress bars (main.js / classpath.bin /
// linker-libs.bin). Returns handles for updating each bar individually.

export interface LoaderRowHandle {
  setProgress(loaded: number, total: number | null): void;
  setError(message: string): void;
}

export interface LoaderHandle {
  root: HTMLElement;
  rowJs: LoaderRowHandle;
  rowCp: LoaderRowHandle;
  rowLl: LoaderRowHandle;
  remove(): void;
  setError(msg: string): void;
}

interface RowSpec {
  label: string;
  approxBytes: number;
}

function fmtMb(bytes: number): string {
  return (bytes / (1024 * 1024)).toFixed(1) + ' MB';
}

function makeRow(spec: RowSpec): { el: HTMLElement; handle: LoaderRowHandle } {
  const row = document.createElement('div');
  row.className = 'loader-row';

  const label = document.createElement('div');
  label.className = 'loader-row__label';
  label.textContent = spec.label;

  const bar = document.createElement('div');
  bar.className = 'loader-row__bar';
  bar.setAttribute('role', 'progressbar');
  bar.setAttribute('aria-valuemin', '0');
  bar.setAttribute('aria-valuemax', '100');

  const fill = document.createElement('div');
  fill.className = 'loader-row__fill';
  bar.appendChild(fill);

  const pct = document.createElement('div');
  pct.className = 'loader-row__pct';
  pct.textContent = '0%';

  row.append(label, bar, pct);

  return {
    el: row,
    handle: {
      setProgress(loaded, total) {
        if (total != null && total > 0) {
          const ratio = Math.max(0, Math.min(1, loaded / total));
          const pctValue = Math.round(ratio * 100);
          fill.style.width = pctValue + '%';
          pct.textContent = `${fmtMb(loaded)} / ${fmtMb(total)}`;
          bar.setAttribute('aria-valuenow', String(pctValue));
        } else {
          // Indeterminate-ish: show fixed half-width with raw byte count
          fill.style.width = '60%';
          pct.textContent = fmtMb(loaded);
        }
      },
      setError(message: string) {
        fill.style.background = 'var(--err)';
        pct.textContent = message;
      },
    },
  };
}

export function createLoader(parent: HTMLElement): LoaderHandle {
  const root = document.createElement('div');
  root.className = 'loader-card';
  root.setAttribute('aria-live', 'polite');

  const title = document.createElement('h2');
  title.className = 'loader-card__title';
  title.textContent = 'Loading the Scala 3 compiler';

  const subtitle = document.createElement('p');
  subtitle.className = 'loader-card__subtitle';
  subtitle.textContent = 'Downloading the compiler bundle, classpath, and linker libraries.';

  const bars = document.createElement('div');
  bars.className = 'loader-card__bars';

  const js = makeRow({ label: 'main.js', approxBytes: 51 * 1024 * 1024 });
  const cp = makeRow({ label: 'classpath.bin', approxBytes: 32 * 1024 * 1024 });
  const ll = makeRow({ label: 'linker-libs.bin', approxBytes: 16 * 1024 * 1024 });
  bars.append(js.el, cp.el, ll.el);

  const errorBox = document.createElement('div');
  errorBox.className = 'loader-card__error';

  root.append(title, subtitle, bars, errorBox);
  parent.appendChild(root);

  return {
    root,
    rowJs: js.handle,
    rowCp: cp.handle,
    rowLl: ll.handle,
    remove(): void {
      root.remove();
    },
    setError(msg: string): void {
      errorBox.textContent = msg;
      errorBox.dataset.visible = 'true';
    },
  };
}
