#!/usr/bin/env bash
#
# scripts/release.sh [--skip-builds] <releaseName>
#
# Builds all standalone `scala3` binaries (sbt buildBinaryAll) and publishes
# them as a GitHub release tagged <releaseName>, with each binary attached as a
# release asset.
#
#   --skip-builds   Skip buildBinaryAll and publish the binaries already in
#                   dist/ (e.g. when iterating on the release itself).
#
# Requires: bun and an authenticated GitHub CLI (`gh auth login`); sbt too
# unless --skip-builds is given.

set -euo pipefail

SKIP_BUILDS=0
RELEASE_NAME=""
for arg in "$@"; do
  case "$arg" in
    --skip-builds) SKIP_BUILDS=1 ;;
    -*) echo "error: unknown flag '$arg'" >&2; exit 1 ;;
    *)
      if [ -n "$RELEASE_NAME" ]; then
        echo "error: unexpected extra argument '$arg'" >&2; exit 1
      fi
      RELEASE_NAME="$arg"
      ;;
  esac
done

if [ -z "$RELEASE_NAME" ]; then
  echo "Usage: scripts/release.sh [--skip-builds] <releaseName>" >&2
  exit 1
fi

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DIST="$ROOT/dist"

cd "$ROOT"

# --- Preflight -------------------------------------------------------------
REQUIRED_TOOLS=(bun gh)
[ "$SKIP_BUILDS" -eq 0 ] && REQUIRED_TOOLS+=(sbt)
for tool in "${REQUIRED_TOOLS[@]}"; do
  command -v "$tool" >/dev/null 2>&1 || { echo "error: '$tool' not found on PATH" >&2; exit 1; }
done
gh auth status >/dev/null 2>&1 || { echo "error: gh is not authenticated (run: gh auth login)" >&2; exit 1; }

# Refuse to clobber an existing release/tag.
if gh release view "$RELEASE_NAME" >/dev/null 2>&1; then
  echo "error: a GitHub release named '$RELEASE_NAME' already exists" >&2
  exit 1
fi

COMMIT="$(git rev-parse HEAD)"
if [ -n "$(git status --porcelain)" ]; then
  echo "warning: working tree has uncommitted changes; the release will tag committed HEAD ($COMMIT)" >&2
fi

# --- Build ------------------------------------------------------------------
if [ "$SKIP_BUILDS" -eq 1 ]; then
  echo "==> Skipping build (--skip-builds); using existing binaries in dist/"
else
  echo "==> Building all binaries (sbt scala3-compiler-sjs/buildBinaryAll)..."
  sbt 'project scala3-compiler-sjs' buildBinaryAll
fi

# Expected matrix outputs — keep in sync with `binaryTargets` in project/Build.scala.
ASSETS=(
  "$DIST/scala3-darwin-arm64"
  "$DIST/scala3-darwin-x64"
  "$DIST/scala3-linux-x64"
  "$DIST/scala3-linux-arm64"
  "$DIST/scala3-windows-x64.exe"
)

for asset in "${ASSETS[@]}"; do
  [ -f "$asset" ] || { echo "error: expected binary not found: $asset" >&2; exit 1; }
done

# --- Publish ----------------------------------------------------------------
echo "==> Creating GitHub release '$RELEASE_NAME' (tagging $COMMIT)..."
gh release create "$RELEASE_NAME" "${ASSETS[@]}" \
  --title "$RELEASE_NAME" \
  --target "$COMMIT" \
  --generate-notes

echo "==> Published release '$RELEASE_NAME' with ${#ASSETS[@]} binaries:"
for asset in "${ASSETS[@]}"; do
  echo "      $(basename "$asset") ($(du -h "$asset" | cut -f1))"
done
