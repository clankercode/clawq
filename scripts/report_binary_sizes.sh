#!/usr/bin/env bash
set -euo pipefail

check=0
output="dist/binary-size-report.tsv"
thresholds="ci/binary-size-thresholds.tsv"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --check)
      check=1
      shift
      ;;
    --output)
      output="$2"
      shift 2
      ;;
    --thresholds)
      thresholds="$2"
      shift 2
      ;;
    *)
      echo "Unknown argument: $1" >&2
      exit 1
      ;;
  esac
done

if ! command -v size >/dev/null 2>&1; then
  echo "Missing required tool: size" >&2
  exit 1
fi

mkdir -p "$(dirname "$output")"

get_exact_section_size() {
  local exe="$1"
  local section="$2"
  size -A -d "$exe" | awk -v section="$section" '$1 == section { sum += $2 } END { print sum + 0 }'
}

get_debug_section_size() {
  local exe="$1"
  size -A -d "$exe" | awk '/^\.debug/ { sum += $2 } END { print sum + 0 }'
}

threshold_line() {
  local profile="$1"
  awk -v profile="$profile" 'NF >= 5 && $1 == profile { print $0 }' "$thresholds"
}

write_header() {
  printf 'profile\tpath\ttotal\ttext\trodata\tdebug\n' >"$output"
}

report_profile() {
  local profile="$1"
  local exe="$2"
  local total text rodata debug
  total=$(stat -c%s "$exe")
  text=$(get_exact_section_size "$exe" ".text")
  rodata=$(get_exact_section_size "$exe" ".rodata")
  debug=$(get_debug_section_size "$exe")

  printf '%s\t%s\t%s\t%s\t%s\t%s\n' \
    "$profile" "$exe" "$total" "$text" "$rodata" "$debug" >>"$output"
  printf '%s total=%s text=%s rodata=%s debug=%s\n' \
    "$profile" "$total" "$text" "$rodata" "$debug"

  if [[ "$check" -eq 1 ]]; then
    local line max_total max_text max_rodata max_debug
    line=$(threshold_line "$profile")
    if [[ -z "$line" ]]; then
      echo "Missing thresholds for profile '$profile' in $thresholds" >&2
      exit 1
    fi

    read -r _ max_total max_text max_rodata max_debug <<<"$line"

    if (( total > max_total )); then
      echo "$profile total size regression: $total > $max_total" >&2
      exit 1
    fi
    if (( text > max_text )); then
      echo "$profile .text regression: $text > $max_text" >&2
      exit 1
    fi
    if (( rodata > max_rodata )); then
      echo "$profile .rodata regression: $rodata > $max_rodata" >&2
      exit 1
    fi
    if (( debug > max_debug )); then
      echo "$profile .debug regression: $debug > $max_debug" >&2
      exit 1
    fi
  fi
}

write_header
report_profile speed "_build_opt_speed/default/src/main.exe"
report_profile size "_build_opt_size/default/src/main.exe"

printf 'Wrote %s\n' "$output"
