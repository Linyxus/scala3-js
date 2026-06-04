# Keeping `compiler-js/src` overrides in sync with upstream

The Scala.js compiler is built by compiling the **shared** `compiler/src` (and
`compiler/src-bootstrapped`, `tasty/src`) tree with `-scalajs`, while files under
`compiler-js/src/` **replace** their same-path upstream counterparts (see the
source-selection logic in `project/Build.scala`). This lets the JS build swap in
JS-compatible versions of files that use JVM-only APIs.

## The drift problem

Because an override lives at a *different path* than the file it replaces, git
never 3-way-merges it on a rebase. Upstream evolves `compiler/src/X.scala`; the
override `compiler-js/src/X.scala` silently freezes. You only discover the
divergence when the JS build fails to compile — one error at a time — even though
much of it is mechanical (upstream added a method, renamed a helper, removed a
branch). Worse, some drift *compiles fine* but quietly diverges in behaviour.

## The fix: `bin/sync-overrides`

`bin/sync-overrides` restores the 3-way merge for every **tracked** override
(one whose path also exists upstream):

```
ours   = the current override            compiler-js/src/X
base   = upstream X at the last sync      sha in compiler-js/.upstream-base
theirs = upstream X at the target commit  --onto (default: scala3/main)
result = git merge-file(ours, base, theirs)
```

Upstream changes in regions the override didn't touch merge cleanly (there
`ours == base`, so the merge takes `theirs`). You only get a conflict where a JS
tweak overlaps an upstream change — exactly where judgment is needed — and each
conflict is a `diff3` hunk showing both sides with full context.

Files with **no** upstream counterpart (the `java.*` JDK shims, `BrowserMain`,
`interfaces.scala`, `xsbti/*`, …) carry no dotty drift and are skipped.

### Rebase workflow

```bash
./bin/sync-overrides --report-only        # 1. preview: clean vs conflict vs orphan
./bin/sync-overrides                       # 2. apply 3-way merges in place
#                                            3. resolve any <<<<<<< markers, compile
git rebase scala3/main                     # 4. advance the pristine compiler/src tree
git rev-parse scala3/main > compiler-js/.upstream-base   # 5. record the new sync point
```

`--report-only` writes nothing. The tool exits non-zero if there are conflicts or
orphans, so it can gate CI.

### What it flags

- **conflict** — JS tweak overlaps an upstream change; resolve the `diff3` markers.
- **orphaned upstream** — upstream *deleted/renamed* the file (e.g. this is how
  `ThreadPoolFactory.scala` would have surfaced); the override may be dead.
- **new-upstream / no-base** — upstream *added* a file you also override; review.
- **stub (skipped)** — no upstream counterpart; nothing to merge.

### Known gap

`dotty/tools/dotc/interfaces/interfaces.scala` re-declares several upstream
**Java** interfaces in one Scala file, so it has no 1:1 upstream path and is
treated as a stub. It *is* drift-prone (it caused the `SourceFile.content()`
mismatch) — watch it by hand, or add an explicit mapping later.

`compiler-js/.upstream-base` records the upstream commit the overrides currently
match. Keep it accurate: an imprecise base over-reports conflicts.
