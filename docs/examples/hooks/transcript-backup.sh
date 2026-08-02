#!/bin/sh
# PreCompact hook: preserve the received event payload before compaction.

set -eu

project=${CLAWQ_PROJECT_DIR:-${CLAUDE_PROJECT_DIR:-$(pwd)}}
backup_dir="$project/.clawq/hook-transcripts"
mkdir -p "$backup_dir"

timestamp=$(date -u +%Y%m%dT%H%M%SZ)
target="$backup_dir/$timestamp-$$.json"
temp="$target.tmp"
trap 'rm -f "$temp"' EXIT HUP INT TERM
cat >"$temp"
mv "$temp" "$target"
