#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
VERSION_FILE="$REPO_ROOT/VERSION"
CHECK_MODE="${1:-}"

if [ ! -f "$VERSION_FILE" ]; then
  echo "Missing VERSION file: $VERSION_FILE" >&2
  exit 1
fi

VER="$(tr -d '\n' < "$VERSION_FILE")"
if ! echo "$VER" | grep -qE '^[0-9]+\.[0-9]+\.[0-9]+$'; then
  echo "Invalid version in VERSION file: '$VER' (expected semver X.Y.Z)" >&2
  exit 1
fi

# Each entry: file  pattern  replacement  description
declare -a TARGETS=(
  "coq/theories/Clawq/Cli.v|clawq [0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*-dev|clawq ${VER}-dev|Cli.v version"
  "coq/theories/Clawq/McpFraming.v|\"\"version\"\":\"\"[0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*\"\"|\"\"version\"\":\"\"${VER}\"\"|McpFraming.v version"
  "src/main_wasm.ml|[0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*-wasm|${VER}-wasm|main_wasm.ml version"
  "docs/package.json|\"version\": \"[0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*\"|\"version\": \"${VER}\"|docs/package.json version"
  "docs/src/components/Sidebar.astro|v[0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*|v${VER}|Sidebar.astro version"
  "scripts/wasm_templates/IDENTITY.md|Version: [0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*|Version: ${VER}|IDENTITY.md version"

)

ERRORS=0

for entry in "${TARGETS[@]}"; do
  IFS='|' read -r file pattern replacement desc <<< "$entry"
  filepath="$REPO_ROOT/$file"

  if [ ! -f "$filepath" ]; then
    echo "WARNING: $file not found, skipping ($desc)" >&2
    continue
  fi

  if [ "$CHECK_MODE" = "--check" ]; then
    # In check mode, verify the file already has the correct version
    if ! grep -q "$replacement" "$filepath"; then
      echo "OUT OF DATE: $file ($desc) — expected '$replacement'" >&2
      ERRORS=$((ERRORS + 1))
    fi
  else
    sed -i "s|${pattern}|${replacement}|g" "$filepath"
  fi
done

if [ "$CHECK_MODE" = "--check" ]; then
  if [ "$ERRORS" -gt 0 ]; then
    echo "$ERRORS file(s) out of date. Run 'make sync-version' to fix." >&2
    exit 1
  else
    echo "All files in sync with VERSION $VER."
    exit 0
  fi
else
  echo "Synced version $VER to all targets."
fi
