#!/bin/sh
# Stop hook: block completion until the local task is marked complete.

set -eu

project=${CLAWQ_PROJECT_DIR:-${CLAUDE_PROJECT_DIR:-$(pwd)}}
task_file="$project/.clawq/current-task"

if [ ! -s "$task_file" ]; then
  printf '%s\n' "Blocked by lifecycle hook: record task completion in $task_file before stopping." >&2
  exit 2
fi

if ! grep -Eiq '^[[:space:]]*(status[[:space:]]*=[[:space:]]*)?(done|complete|completed)[[:space:]]*$' "$task_file"; then
  printf '%s\n' "Blocked by lifecycle hook: mark $task_file as done before stopping." >&2
  exit 2
fi

exit 0
