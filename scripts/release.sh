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
ENTITLEMENTS="$ROOT/scripts/macos-entitlements.plist"

cd "$ROOT"

# Notarize a signed macOS binary (online ticket — a bare executable can't be
# stapled, so Gatekeeper verifies against Apple's servers).
notarize_macos_binary() {
  local bin="$1"
  local zip="${bin}.notarize.zip"
  local auth=()
  if [ -n "${MACOS_NOTARY_PROFILE:-}" ]; then
    auth=(--keychain-profile "$MACOS_NOTARY_PROFILE")
  elif [ -n "${MACOS_NOTARY_APPLE_ID:-}" ] && [ -n "${MACOS_NOTARY_PASSWORD:-}" ] && [ -n "${MACOS_NOTARY_TEAM_ID:-}" ]; then
    auth=(--apple-id "$MACOS_NOTARY_APPLE_ID" --password "$MACOS_NOTARY_PASSWORD" --team-id "$MACOS_NOTARY_TEAM_ID")
  else
    echo "error: MACOS_SIGN_IDENTITY is set but no notary credentials found." >&2
    echo "       Set MACOS_NOTARY_PROFILE, or MACOS_NOTARY_APPLE_ID + MACOS_NOTARY_PASSWORD + MACOS_NOTARY_TEAM_ID." >&2
    exit 1
  fi
  echo "    notarizing $(basename "$bin")..."
  /usr/bin/ditto -c -k --keepParent "$bin" "$zip"
  xcrun notarytool submit "$zip" "${auth[@]}" --wait
  rm -f "$zip"
}

# Code-sign one macOS binary: Developer ID + notarization when an identity is
# configured, otherwise a valid ad-hoc signature (runnable from the terminal,
# but Finder/Gatekeeper will still warn on download).
sign_macos_binary() {
  local bin="$1"
  if [ -n "${MACOS_SIGN_IDENTITY:-}" ]; then
    echo "    Developer ID signing $(basename "$bin") ($MACOS_SIGN_IDENTITY)"
    codesign --force --timestamp --options runtime \
      --entitlements "$ENTITLEMENTS" --sign "$MACOS_SIGN_IDENTITY" "$bin"
    notarize_macos_binary "$bin"
  else
    echo "    ad-hoc signing $(basename "$bin")"
    codesign --force --sign - "$bin"
  fi
  codesign --verify --verbose "$bin"
}

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
  echo "==> Building all binaries (sbt scala3-compiler-cli-sjs/buildBinaryAll)..."
  sbt --client 'scala3-compiler-cli-sjs/buildBinaryAll'
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

# --- Sign macOS binaries ----------------------------------------------------
MACOS_BINARIES=(
  "$DIST/scala3-darwin-arm64"
  "$DIST/scala3-darwin-x64"
)
if [ "$(uname -s)" = "Darwin" ] && command -v codesign >/dev/null 2>&1; then
  echo "==> Code-signing macOS binaries..."
  for bin in "${MACOS_BINARIES[@]}"; do
    sign_macos_binary "$bin"
  done
  if [ -z "${MACOS_SIGN_IDENTITY:-}" ]; then
    echo "    note: ad-hoc signed (no MACOS_SIGN_IDENTITY). Binaries run from the"
    echo "          terminal; users who hit a Finder/Gatekeeper prompt can run:"
    echo "            xattr -d com.apple.quarantine ./scala3-darwin-*"
  fi
else
  echo "warning: not on macOS (or codesign missing) — macOS binaries will be UNSIGNED" >&2
fi

# --- Publish ----------------------------------------------------------------
# The release tag points at HEAD, so that commit must exist on the remote.
if ! git branch -r --contains HEAD 2>/dev/null | grep -q .; then
  BRANCH="$(git rev-parse --abbrev-ref HEAD)"
  if [ "$BRANCH" = "HEAD" ]; then
    echo "error: detached HEAD — check out a branch (and push it) before releasing" >&2
    exit 1
  fi
  echo "==> HEAD ($COMMIT) is not on origin yet; pushing '$BRANCH'..."
  git push origin "$BRANCH"
fi

echo "==> Creating GitHub release '$RELEASE_NAME' (tagging $COMMIT)..."
gh release create "$RELEASE_NAME" "${ASSETS[@]}" \
  --title "$RELEASE_NAME" \
  --target "$COMMIT" \
  --generate-notes

echo "==> Published release '$RELEASE_NAME' with ${#ASSETS[@]} binaries:"
for asset in "${ASSETS[@]}"; do
  echo "      $(basename "$asset") ($(du -h "$asset" | cut -f1))"
done
