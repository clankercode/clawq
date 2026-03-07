#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
UI_DIST="$REPO_ROOT/ui/dist"
OUTPUT="$REPO_ROOT/src/chat_ui_assets.ml"
CHECK_MODE="${1:-}"

if [ ! -f "$UI_DIST/index.html" ] || [ ! -f "$UI_DIST/chat.css" ] || [ ! -f "$UI_DIST/chat.js" ]; then
  echo "Missing UI build output in $UI_DIST. Run 'make ui' first." >&2
  exit 1
fi

TMP_OUTPUT="$(mktemp)"

python3 - "$UI_DIST" >"$TMP_OUTPUT" <<'PY'
import hashlib
import pathlib
import sys

dist = pathlib.Path(sys.argv[1])
files = [
    ("index_html", "index.html"),
    ("chat_css", "chat.css"),
    ("chat_js", "chat.js"),
]

digest = hashlib.sha256()
for _, file_name in files:
    data = (dist / file_name).read_bytes()
    digest.update(file_name.encode("utf-8"))
    digest.update(b"\n")
    digest.update(data)
    digest.update(b"\n")

version = f"sha256:{digest.hexdigest()}"

def quoted(text: str) -> str:
    tag = "ui"
    while f"|{tag}}}" in text:
        tag += "x"
    return "{" + tag + "|" + text + "|" + tag + "}"

print("(* Auto-generated chat UI assets - embedded UI bundle *)")
print()
print("let ui_version =")
print(f"  {quoted(version)}")
print()
for index, (binding, file_name) in enumerate(files):
    text = (dist / file_name).read_text(encoding="utf-8")
    print(f"let {binding} =")
    print(f"  {quoted(text)}")
    if index != len(files) - 1:
        print()
PY

if [ "$CHECK_MODE" = "--check" ]; then
  if ! diff -u "$OUTPUT" "$TMP_OUTPUT"; then
    rm -f "$TMP_OUTPUT"
    echo "chat_ui_assets.ml is out of date. Run 'make ui'." >&2
    exit 1
  fi
  rm -f "$TMP_OUTPUT"
  echo "chat_ui_assets.ml is up to date."
  exit 0
fi

mv "$TMP_OUTPUT" "$OUTPUT"
echo "Generated $OUTPUT from $UI_DIST"
