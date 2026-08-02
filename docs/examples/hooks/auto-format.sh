#!/bin/sh
# PostToolUse hook: format a file written by a Write or Edit tool.
# Configure this only for tools whose input carries file_path or path.

set -eu

payload=$(mktemp "${TMPDIR:-/tmp}/clawq-hook.XXXXXX")
trap 'rm -f "$payload"' EXIT HUP INT TERM
cat >"$payload"

# This intentionally handles the ordinary one-line JSON string fields emitted
# by Clawq. It avoids a jq/python dependency; paths containing escaped quotes
# are skipped rather than formatted incorrectly.
file=$(sed -n 's/.*"file_path"[[:space:]]*:[[:space:]]*"\([^"\\]*\)".*/\1/p' "$payload" | head -n 1)
[ -n "$file" ] || file=$(sed -n 's/.*"path"[[:space:]]*:[[:space:]]*"\([^"\\]*\)".*/\1/p' "$payload" | head -n 1)
[ -n "$file" ] || exit 0
[ -f "$file" ] || exit 0

case "$file" in
  *.ml|*.mli)
    command -v ocamlformat >/dev/null 2>&1 && ocamlformat -i "$file" || :
    ;;
  *.go)
    command -v gofmt >/dev/null 2>&1 && gofmt -w "$file" || :
    ;;
  *.rs)
    command -v rustfmt >/dev/null 2>&1 && rustfmt "$file" || :
    ;;
  *.js|*.jsx|*.ts|*.tsx|*.json|*.css|*.md|*.yaml|*.yml)
    command -v prettier >/dev/null 2>&1 && prettier --write "$file" >/dev/null 2>&1 || :
    ;;
  *.py)
    command -v ruff >/dev/null 2>&1 && ruff format "$file" >/dev/null 2>&1 || :
    ;;
esac

exit 0
