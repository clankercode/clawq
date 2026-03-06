#!/usr/bin/env bash
# Scan Coq .v files for Theorem/Lemma counts, validate against formal_verification.yml.
# Usage: bash scripts/update_fv_data.sh
set -euo pipefail

COQ_DIR="coq/theories/Clawq"
YML_FILE="docs/src/data/formal_verification.yml"

echo "=== Formal Verification Data Validation ==="
echo ""

# --- Scan actual theorem/lemma counts ---
echo "--- Actual Theorem/Lemma Counts (from .v files) ---"
declare -A actual_counts
total=0
for f in "$COQ_DIR"/*.v; do
  base=$(basename "$f" .v)
  count=$(grep -cE '^\s*(Theorem|Lemma)\b' "$f" 2>/dev/null || true)
  count=${count:-0}
  if [ "$count" -gt 0 ]; then
    printf "  %-20s %3d\n" "$base" "$count"
    actual_counts["$base"]=$count
    total=$((total + count))
  fi
done
echo ""
echo "Total: $total theorems/lemmas across ${#actual_counts[@]} files"
echo ""

# --- Validate YAML counts ---
if [ ! -f "$YML_FILE" ]; then
  echo "ERROR: $YML_FILE not found"
  exit 1
fi

echo "--- Validating $YML_FILE ---"
errors=0

# Parse YAML: extract phase, coq_file(s), theorems, status for verified phases
# We validate that verified phases with a single coq_file match the grep count.
# F5 and F1 share ConfigProofs.v, F6 spans two files — handle specially.
current_phase=""
current_status=""
current_theorems=""
current_coq_file=""
in_coq_files=false
coq_files_list=""

validate_entry() {
  local phase="$1" status="$2" theorems="$3" coq_file="$4" coq_files="$5"

  if [ "$status" != "verified" ]; then
    # Planned phases should have 0 theorems
    if [ "$theorems" != "0" ]; then
      echo "  WARN: $phase has status=$status but theorems=$theorems (expected 0)"
    fi
    return
  fi

  # For multi-file phases (F6), sum the counts
  if [ -n "$coq_files" ]; then
    expected=0
    for cf in $coq_files; do
      base=$(basename "$cf" .v)
      file_count=${actual_counts["$base"]:-0}
      expected=$((expected + file_count))
    done
    if [ "$theorems" -ne "$expected" ]; then
      echo "  MISMATCH: $phase claims $theorems theorems, actual total is $expected (from: $coq_files)"
      errors=$((errors + 1))
    else
      echo "  OK: $phase = $theorems theorems"
    fi
    return
  fi

  # Single file phase
  if [ -n "$coq_file" ]; then
    base=$(basename "$coq_file" .v)
    expected=${actual_counts["$base"]:-0}
    if [ "$theorems" -ne "$expected" ]; then
      echo "  MISMATCH: $phase claims $theorems theorems in $coq_file, actual is $expected"
      errors=$((errors + 1))
    else
      echo "  OK: $phase = $theorems theorems"
    fi
  fi
}

# Simple line-by-line YAML parser
while IFS= read -r line; do
  # New entry
  if [[ "$line" =~ ^-\ phase:\ *\"([^\"]+)\" ]]; then
    # Validate previous entry
    if [ -n "$current_phase" ]; then
      validate_entry "$current_phase" "$current_status" "$current_theorems" "$current_coq_file" "$coq_files_list"
    fi
    current_phase="${BASH_REMATCH[1]}"
    current_status=""
    current_theorems=""
    current_coq_file=""
    in_coq_files=false
    coq_files_list=""
  elif [[ "$line" =~ ^\ +status:\ *\"([^\"]+)\" ]]; then
    current_status="${BASH_REMATCH[1]}"
  elif [[ "$line" =~ ^\ +theorems:\ *([0-9]+) ]]; then
    current_theorems="${BASH_REMATCH[1]}"
  elif [[ "$line" =~ ^\ +coq_file:\ *\"([^\"]+)\" ]]; then
    current_coq_file="${BASH_REMATCH[1]}"
  elif [[ "$line" =~ ^\ +coq_files: ]]; then
    in_coq_files=true
    coq_files_list=""
  elif $in_coq_files && [[ "$line" =~ ^\ +- ]]; then
    # Extract file path from list item
    if [[ "$line" =~ \"([^\"]+)\" ]]; then
      coq_files_list="$coq_files_list ${BASH_REMATCH[1]}"
    fi
  else
    if $in_coq_files && [[ ! "$line" =~ ^\ +- ]]; then
      in_coq_files=false
    fi
  fi
done < "$YML_FILE"

# Validate last entry
if [ -n "$current_phase" ]; then
  validate_entry "$current_phase" "$current_status" "$current_theorems" "$current_coq_file" "$coq_files_list"
fi

echo ""
if [ "$errors" -gt 0 ]; then
  echo "FAILED: $errors mismatched theorem count(s). Update $YML_FILE to match actual .v files."
  exit 1
else
  echo "PASSED: All theorem counts match."
fi
