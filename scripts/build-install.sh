#!/usr/bin/env bash
set -euo pipefail

CLAWQ_BIN="./_build_opt_speed/default/src/main.exe"
NPM_BIN="./npm-pkg/bin/clawq"
NPM_DEBUG="./npm-pkg/bin/clawq.debug"

echo "==> Building release-speed (-O2)..."
mkdir -p npm-pkg/bin
make build-opt-speed

echo "==> Copying binary..."
cp "$CLAWQ_BIN" "$NPM_BIN"

echo "==> Extracting debug symbols..."
objcopy --only-keep-debug "$NPM_BIN" "$NPM_DEBUG"
chmod -x "$NPM_DEBUG"

echo "==> Stripping binary..."
strip "$NPM_BIN"

echo "==> Adding debug link..."
objcopy --add-gnu-debuglink="$NPM_DEBUG" "$NPM_BIN"

echo ""
echo "Results:"
ls -lh "$NPM_BIN" "$NPM_DEBUG"
