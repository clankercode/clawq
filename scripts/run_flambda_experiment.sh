#!/usr/bin/env bash
set -euo pipefail

baseline_switch="clawq-5.1"
flambda_switch="clawq-5.1-flambda"
compiler="ocaml-variants.5.1.1+options"
runs=5

while [[ $# -gt 0 ]]; do
  case "$1" in
    --baseline-switch)
      baseline_switch="$2"
      shift 2
      ;;
    --flambda-switch)
      flambda_switch="$2"
      shift 2
      ;;
    --compiler)
      compiler="$2"
      shift 2
      ;;
    --runs)
      runs="$2"
      shift 2
      ;;
    *)
      echo "Unknown argument: $1" >&2
      exit 1
      ;;
  esac
done

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
dist_dir="$repo_root/dist/flambda-experiment"
report_tsv="$dist_dir/results.tsv"
report_md="$dist_dir/report.md"

mkdir -p "$dist_dir"

switch_exists() {
  opam switch list --short | grep -Fxq "$1"
}

ensure_switch_ready() {
  local switch="$1"
  local with_flambda="$2"

  if ! switch_exists "$switch"; then
    if [[ "$with_flambda" == "1" ]]; then
      opam switch create "$switch" "$compiler" ocaml-option-flambda -y
    else
      echo "Missing required baseline switch: $switch" >&2
      exit 1
    fi
  fi

  opam install --switch="$switch" -y . --deps-only
}

switch_flambda_value() {
  opam exec --switch="$1" -- ocamlopt -config \
    | awk -F: '/^flambda:/ { gsub(/^[ 	]+/, "", $2); print $2 }'
}

median_time() {
  local switch="$1"
  local command="$2"
  RUNS="$runs" MEASURE_CMD="$command" \
    opam exec --switch="$switch" -- /usr/bin/env python3 - <<'PY'
import os
import statistics
import subprocess
import time

runs = int(os.environ["RUNS"])
command = os.environ["MEASURE_CMD"]
values = []
for _ in range(runs):
    start = time.perf_counter()
    subprocess.run(
        command,
        shell=True,
        executable="/bin/bash",
        check=True,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    values.append(time.perf_counter() - start)

print(f"{statistics.median(values):.6f}")
PY
}

capture_profile() {
  local switch="$1"
  local label="$2"
  local switch_slug
  local speed_unstripped speed_stripped size_unstripped size_stripped
  local speed_help size_help status_time flambda
  switch_slug="${switch//[^A-Za-z0-9._-]/_}"

  rm -f "$dist_dir/${switch_slug}-speed-unstripped.exe" \
    "$dist_dir/${switch_slug}-speed-stripped.exe" \
    "$dist_dir/${switch_slug}-size-unstripped.exe" \
    "$dist_dir/${switch_slug}-size-stripped.exe"

  make -C "$repo_root" SHELL_SWITCH="$switch" build-opt-speed
  speed_unstripped=$(stat -c%s "$repo_root/_build_opt_speed/default/src/main.exe")
  cp -f "$repo_root/_build_opt_speed/default/src/main.exe" \
    "$dist_dir/${switch_slug}-speed-unstripped.exe"

  make -C "$repo_root" SHELL_SWITCH="$switch" build-opt-speed-stripped
  speed_stripped=$(stat -c%s "$repo_root/dist/clawq-speed")
  cp -f "$repo_root/dist/clawq-speed" "$dist_dir/${switch_slug}-speed-stripped.exe"

  make -C "$repo_root" SHELL_SWITCH="$switch" build-opt-size
  size_unstripped=$(stat -c%s "$repo_root/_build_opt_size/default/src/main.exe")
  cp -f "$repo_root/_build_opt_size/default/src/main.exe" \
    "$dist_dir/${switch_slug}-size-unstripped.exe"

  make -C "$repo_root" SHELL_SWITCH="$switch" build-opt-size-stripped
  size_stripped=$(stat -c%s "$repo_root/dist/clawq-size")
  cp -f "$repo_root/dist/clawq-size" "$dist_dir/${switch_slug}-size-stripped.exe"

  speed_help=$(median_time "$switch" \
    "$dist_dir/${switch_slug}-speed-unstripped.exe --help")
  size_help=$(median_time "$switch" \
    "$dist_dir/${switch_slug}-size-unstripped.exe --help")
  status_time=$(median_time "$switch" \
    "$dist_dir/${switch_slug}-speed-unstripped.exe status")
  flambda=$(switch_flambda_value "$switch")

  printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
    "$label" "$switch" "$flambda" "$speed_unstripped" "$speed_stripped" \
    "$size_unstripped" "$size_stripped" "$speed_help" "$status_time" \
    >>"$report_tsv"
  printf '%s\t%s\n' "$label-size-help-median_s" "$size_help" >>"$report_tsv.extra"
}

write_report() {
  python3 - "$report_tsv" "$report_tsv.extra" "$report_md" <<'PY'
import csv
import sys

tsv_path, extra_path, md_path = sys.argv[1:4]
rows = []
with open(tsv_path, newline='', encoding='utf-8') as fh:
    reader = csv.DictReader(fh, delimiter='\t')
    rows = list(reader)

extra = {}
with open(extra_path, 'r', encoding='utf-8') as fh:
    for line in fh:
        if not line.strip():
            continue
        key, value = line.rstrip('\n').split('\t', 1)
        extra[key] = value

baseline, flambda = rows

def pct(new, old):
    old = float(old)
    new = float(new)
    if old == 0:
        return 'n/a'
    return f"{((new - old) / old) * 100:+.2f}%"

lines = []
lines.append('# Flambda Experiment')
lines.append('')
lines.append('| profile | switch | flambda | speed unstripped | speed stripped | size unstripped | size stripped | speed help median (s) | speed status median (s) |')
lines.append('|---|---|---:|---:|---:|---:|---:|---:|---:|')
for row in rows:
    lines.append(
        f"| {row['profile']} | {row['switch']} | {row['flambda']} | {row['speed_unstripped_bytes']} | {row['speed_stripped_bytes']} | {row['size_unstripped_bytes']} | {row['size_stripped_bytes']} | {row['speed_help_median_s']} | {row['status_median_s']} |"
    )
lines.append('')
lines.append('## Delta vs baseline')
lines.append('')
lines.append('| metric | flambda delta |')
lines.append('|---|---:|')
for metric, key in [
    ('speed unstripped bytes', 'speed_unstripped_bytes'),
    ('speed stripped bytes', 'speed_stripped_bytes'),
    ('size unstripped bytes', 'size_unstripped_bytes'),
    ('size stripped bytes', 'size_stripped_bytes'),
    ('speed help median', 'speed_help_median_s'),
    ('size help median', None),
    ('speed status median', 'status_median_s'),
]:
    if key is None:
        base_v = extra['baseline-size-help-median_s']
        flambda_v = extra['flambda-size-help-median_s']
    else:
        base_v = baseline[key]
        flambda_v = flambda[key]
    lines.append(f"| {metric} | {pct(flambda_v, base_v)} |")
lines.append('')
lines.append('Startup timing uses `main.exe help` medians. Runtime timing uses `main.exe status` medians as a CLI/runtime proxy on the speed build.')

with open(md_path, 'w', encoding='utf-8') as fh:
    fh.write('\n'.join(lines) + '\n')
PY
}

printf 'profile\tswitch\tflambda\tspeed_unstripped_bytes\tspeed_stripped_bytes\tsize_unstripped_bytes\tsize_stripped_bytes\tspeed_help_median_s\tstatus_median_s\n' >"$report_tsv"
: >"$report_tsv.extra"

ensure_switch_ready "$baseline_switch" 0
ensure_switch_ready "$flambda_switch" 1

capture_profile "$baseline_switch" baseline
capture_profile "$flambda_switch" flambda
write_report

printf 'Wrote %s and %s\n' "$report_tsv" "$report_md"
