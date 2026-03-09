#!/usr/bin/env bash
# clean_stale_dune_locks.sh — remove stale Dune .lock files from build dirs.
#
# Dune holds an flock on _build/.lock while running. If the file exists but
# no process holds the lock, it is stale and safe to remove.
#
# Usage: ./scripts/clean_stale_dune_locks.sh [DIR ...]
# With no arguments, checks all known local build dirs.

set -euo pipefail

DEFAULT_DIRS=("_build" "_build_opt_speed" "_build_opt_size" "_build_opt_min")

dirs=("${@:-}")
if [ ${#dirs[@]} -eq 0 ] || [ -z "${dirs[0]}" ]; then
  dirs=("${DEFAULT_DIRS[@]}")
fi

for dir in "${dirs[@]}"; do
  lockfile="$dir/.lock"
  [ -e "$lockfile" ] || continue

  # Try a non-blocking exclusive flock. If we get it, no process holds the
  # lock, so it is stale. We release immediately (the subshell exits).
  if (flock --nonblock --exclusive 9 || exit 1) 9<"$lockfile" 2>/dev/null; then
    echo "Removing stale lock: $lockfile" >&2
    rm -f "$lockfile"
  else
    echo "Lock is held by an active process: $lockfile" >&2
    exit 1
  fi
done
