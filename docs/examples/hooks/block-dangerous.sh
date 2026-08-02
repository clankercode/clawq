#!/bin/sh
# PreToolUse hook: reject a deliberately narrow set of destructive shell forms.

set -eu

payload=$(mktemp "${TMPDIR:-/tmp}/clawq-hook.XXXXXX")
trap 'rm -f "$payload"' EXIT HUP INT TERM
cat >"$payload"

# The hook is intentionally conservative: it looks for unambiguous destructive
# forms rather than attempting to parse every shell dialect.
if grep -Eiq 'rm[[:space:]]+-[[:alnum:]]*r[[:alnum:]]*f[[:alnum:]]*[[:space:]]+(/|~)|mkfs(\.|[[:space:]])|dd[[:space:]].*of=/dev/|:[[:space:]]*\(\)[[:space:]]*\{|(^|[;&|[:space:]])(shutdown|reboot|poweroff)([;&|[:space:]]|$)|git[[:space:]]+reset[[:space:]]+--hard|git[[:space:]]+clean[[:space:]]+-[[:alnum:]]*f' "$payload"; then
  printf '%s\n' 'Blocked by lifecycle hook: destructive command requires explicit manual review.' >&2
  exit 2
fi

exit 0
