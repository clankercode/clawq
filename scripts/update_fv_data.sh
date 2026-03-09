#!/usr/bin/env bash
# Scan Coq .v files, update formal_verification.yml theorem counts for ALL
# phases (verified and planned), validate verified phases, then update the
# docs page with current stats.
# Usage: bash scripts/update_fv_data.sh
set -euo pipefail

COQ_DIR="coq/theories/Clawq"
YML_FILE="docs/src/data/formal_verification.yml"
JSON_FILE="docs/src/data/fv-stats.json"

echo "=== Formal Verification Data Update ==="
echo ""

# --- Scan actual theorem/lemma counts from .v files ---
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

if [ ! -f "$YML_FILE" ]; then
  echo "ERROR: $YML_FILE not found"
  exit 1
fi

# --- Update YAML theorem counts for ALL phases ---
# Groups YAML into per-phase blocks, looks up actual count from coq_file(s),
# and rewrites the theorems: field in place.
echo "--- Updating $YML_FILE ---"
python3 - "$YML_FILE" "$COQ_DIR" 2>&1 <<'PYEOF'
import sys, re, glob, os, subprocess

yml_path = sys.argv[1]
coq_dir  = sys.argv[2]

# Count theorems per .v file
actual = {}
for fpath in sorted(glob.glob(f"{coq_dir}/*.v")):
    base = os.path.basename(fpath)[:-2]
    r = subprocess.run(['grep', '-cE', r'^\s*(Theorem|Lemma)\b', fpath],
                       capture_output=True, text=True)
    actual[base] = int(r.stdout.strip()) if r.returncode in (0, 1) else 0

def file_count(path):
    return actual.get(os.path.basename(path)[:-2], 0)

with open(yml_path) as f:
    lines = f.readlines()

# Group lines into per-phase blocks (each starts with "- phase:")
blocks, cur = [], []
for line in lines:
    if re.match(r'^- phase:', line) and cur:
        blocks.append(cur)
        cur = []
    cur.append(line)
if cur:
    blocks.append(cur)

out = []
for block in blocks:
    # Extract coq_file / coq_files from block (they appear after theorems:)
    coq_file, coq_files, in_cfiles = None, [], False
    for line in block:
        m = re.match(r'^\s+coq_file:\s*"([^"]+)"', line)
        if m:
            coq_file = m.group(1)
        if re.match(r'^\s+coq_files:', line):
            in_cfiles = True
            continue
        if in_cfiles:
            m2 = re.match(r'\s+- "([^"]+)"', line)
            if m2:
                coq_files.append(m2.group(1))
            elif re.match(r'^\s+\w', line):
                in_cfiles = False

    new_count = (sum(file_count(f) for f in coq_files) if coq_files
                 else file_count(coq_file) if coq_file
                 else None)

    for line in block:
        m = re.match(r'^(\s+theorems:\s*)(\d+)(.*)', line)
        if m and new_count is not None:
            old = int(m.group(2))
            if old != new_count:
                pm = re.search(r'"([^"]+)"', block[0])
                pid = pm.group(1) if pm else '?'
                print(f"  {pid}: {old} -> {new_count}")
            out.append(f"{m.group(1)}{new_count}{m.group(3)}\n")
        else:
            out.append(line)

with open(yml_path, 'w') as f:
    f.writelines(out)
print("  YAML theorem counts updated.")
PYEOF
echo ""

# --- Validate verified phases ---
echo "--- Validating verified phases ---"
errors=0
current_phase="" current_status="" current_theorems=""
current_coq_file="" in_coq_files=false coq_files_list=""

validate_entry() {
  local phase="$1" status="$2" theorems="$3" coq_file="$4" coq_files="$5"
  [ "$status" = "verified" ] || return 0  # only check verified phases

  local expected=0
  if [ -n "$coq_files" ]; then
    for cf in $coq_files; do
      base=$(basename "$cf" .v)
      expected=$((expected + ${actual_counts["$base"]:-0}))
    done
  elif [ -n "$coq_file" ]; then
    base=$(basename "$coq_file" .v)
    expected=${actual_counts["$base"]:-0}
  else
    return 0
  fi

  if [ "$theorems" -ne "$expected" ]; then
    echo "  ERROR: $phase claims $theorems theorems, actual is $expected"
    errors=$((errors + 1))
  else
    echo "  OK: $phase = $theorems theorems"
  fi
}

while IFS= read -r line; do
  if [[ "$line" =~ ^-\ phase:\ *\"([^\"]+)\" ]]; then
    [ -n "$current_phase" ] && validate_entry "$current_phase" "$current_status" \
      "$current_theorems" "$current_coq_file" "$coq_files_list"
    current_phase="${BASH_REMATCH[1]}"
    current_status="" current_theorems="" current_coq_file=""
    in_coq_files=false coq_files_list=""
  elif [[ "$line" =~ ^\ +status:\ *\"([^\"]+)\" ]];   then current_status="${BASH_REMATCH[1]}"
  elif [[ "$line" =~ ^\ +theorems:\ *([0-9]+) ]];      then current_theorems="${BASH_REMATCH[1]}"
  elif [[ "$line" =~ ^\ +coq_file:\ *\"([^\"]+)\" ]];  then current_coq_file="${BASH_REMATCH[1]}"
  elif [[ "$line" =~ ^\ +coq_files: ]]; then in_coq_files=true coq_files_list=""
  elif $in_coq_files && [[ "$line" =~ ^\ +- ]]; then
    [[ "$line" =~ \"([^\"]+)\" ]] && coq_files_list="$coq_files_list ${BASH_REMATCH[1]}"
  else
    $in_coq_files && [[ ! "$line" =~ ^\ +- ]] && in_coq_files=false
  fi
done < "$YML_FILE"
[ -n "$current_phase" ] && validate_entry "$current_phase" "$current_status" \
  "$current_theorems" "$current_coq_file" "$coq_files_list"

echo ""
if [ "$errors" -gt 0 ]; then
  echo "FAILED: $errors validation error(s) in verified phases."
  exit 1
else
  echo "PASSED: All verified phase counts match."
fi

# --- Compute stats ---
echo ""
echo "--- Computing derived FV stats ---"

# Verified unique total: sum actual_counts for unique .v files backing verified phases.
# Files shared across phases (ConfigProofs.v for F1+F5) counted once.
declare -A _v_seen
_v_total=0
_cur_status="" _cur_coq_file="" _cur_coq_files_list="" _in_cfiles=false

flush_verified() {
  [[ "$_cur_status" != "verified" ]] && return
  if [[ -n "$_cur_coq_files_list" ]]; then
    for cf in $_cur_coq_files_list; do
      b=$(basename "$cf" .v)
      [[ -z "${_v_seen[$b]+_}" ]] && { _v_seen[$b]=1; _v_total=$((_v_total + ${actual_counts[$b]:-0})); }
    done
  elif [[ -n "$_cur_coq_file" ]]; then
    b=$(basename "$_cur_coq_file" .v)
    [[ -z "${_v_seen[$b]+_}" ]] && { _v_seen[$b]=1; _v_total=$((_v_total + ${actual_counts[$b]:-0})); }
  fi
  return 0
}

while IFS= read -r line; do
  if [[ "$line" =~ ^-\ phase: ]]; then
    flush_verified
    _cur_status="" _cur_coq_file="" _cur_coq_files_list="" _in_cfiles=false
  elif [[ "$line" =~ ^\ +status:\ *\"([^\"]+)\" ]];  then _cur_status="${BASH_REMATCH[1]}"
  elif [[ "$line" =~ ^\ +coq_file:\ *\"([^\"]+)\" ]]; then _cur_coq_file="${BASH_REMATCH[1]}"
  elif [[ "$line" =~ ^\ +coq_files: ]]; then _in_cfiles=true _cur_coq_files_list=""
  elif $_in_cfiles && [[ "$line" =~ ^\ +- ]]; then
    [[ "$line" =~ \"([^\"]+)\" ]] && _cur_coq_files_list="$_cur_coq_files_list ${BASH_REMATCH[1]}"
  else
    $_in_cfiles && [[ ! "$line" =~ ^\ +- ]] && _in_cfiles=false
  fi
done < "$YML_FILE"
flush_verified

coq_lines=$(wc -l "$COQ_DIR"/*.v | awk '/total/{print $1}')
extracted_count=$(grep -c 'extracted: true' "$YML_FILE" || true)
today=$(date +%Y-%m-%d)
remaining_count=$(( total - _v_total ))
percent=$(awk "BEGIN { printf \"%.1f\", ${_v_total} * 100 / ${total} }")

command_count=$(python3 - <<'PYEOF'
from pathlib import Path

text = Path('src/main.ml').read_text()
start = text.index('let cmds =')
end = text.index('in\n  exit', start)
block = text[start:end]
count = 0
for line in block.splitlines():
    s = line.strip().rstrip(';')
    if s.endswith('_cmd') and not s.startswith('let cmds') and s not in ('[', ']'):
        count += 1
print(count)
PYEOF
)

test_count=$(python3 - <<'PYEOF'
import pathlib
import re

count = 0
for path in pathlib.Path('test').glob('*.ml'):
    count += len(re.findall(r'Alcotest\.test_case\b', path.read_text()))
print(count)
PYEOF
)

echo "  total theorems (all files):       $total"
echo "  verified (unique files, YAML):    $_v_total"
echo "  remaining (written, not verified): $remaining_count"
echo "  modules extracted:                $extracted_count"
echo "  coq lines:                        $coq_lines"
echo "  progress bar:                     ${percent}%"
echo "  top-level commands:               $command_count"
echo "  alcotest cases:                   $test_count"
echo "  today:                            $today"
echo ""

if [ ! -f "$JSON_FILE" ]; then
  echo "Creating $JSON_FILE"
  mkdir -p "$(dirname "$JSON_FILE")"
  printf '{}\n' > "$JSON_FILE"
fi

python3 - "$YML_FILE" "$JSON_FILE" "$today" "$total" "$_v_total" "$remaining_count" "$extracted_count" "$coq_lines" "$percent" "$command_count" "$test_count" <<'PYEOF'
import json
import sys

import yaml

yml_path, json_path, today, repository_total, verified_unique_total, remaining_total, extracted_modules, coq_lines, verified_unique_percent, command_count, test_count = sys.argv[1:12]

with open(yml_path) as f:
    phases = yaml.safe_load(f)

public_phases = [phase for phase in phases if phase['phase'] != 'F0']

# Deduplicate public_phase_total by coq_file to avoid double-counting
# (e.g. F1 and F5 both reference ConfigProofs.v)
_seen_files = set()
public_phase_total = 0
for phase in public_phases:
    files = phase.get('coq_files', [phase['coq_file']] if 'coq_file' in phase else [])
    has_new_file = any(f not in _seen_files for f in files)
    _seen_files.update(files)
    if has_new_file:
        public_phase_total += phase['theorems']
# Also deduplicate verified/in_progress/planned by coq_file
def _dedup_sum(phases, status_filter):
    seen = set()
    total = 0
    for phase in phases:
        if phase['status'] != status_filter:
            continue
        files = phase.get('coq_files', [phase['coq_file']] if 'coq_file' in phase else [])
        if any(f not in seen for f in files):
            seen.update(files)
            total += phase['theorems']
    return total

public_verified_total = _dedup_sum(public_phases, 'verified')
public_in_progress_total = _dedup_sum(public_phases, 'in_progress')
public_planned_total = _dedup_sum(public_phases, 'planned')
verified_domain_count = sum(1 for phase in public_phases if phase['status'] == 'verified')

stats = {
    'generated_on': today,
    'coq_version': '8.19',
    'repository_total': int(repository_total),
    'public_phase_total': public_phase_total,
    'verified_unique_total': int(verified_unique_total),
    'verified_unique_percent': float(verified_unique_percent),
    'public_verified_total': public_verified_total,
    'public_in_progress_total': public_in_progress_total,
    'public_planned_total': public_planned_total,
    'in_progress_total': int(remaining_total),
    'extracted_modules': int(extracted_modules),
    'coq_lines': int(coq_lines),
    'command_count': int(command_count),
    'test_count': int(test_count),
    'verified_domain_count': verified_domain_count,
}

with open(json_path, 'w') as f:
    json.dump(stats, f, indent=2)
    f.write('\n')
PYEOF

echo "DONE: $JSON_FILE updated."
echo "  repository_total=${total}, public_phase_total=$(python3 -c "import json; print(json.load(open('$JSON_FILE'))['public_phase_total'])")"
echo "  verified=${_v_total}, remaining=${remaining_count}, extracted=${extracted_count}, lines=${coq_lines}, date=${today}"

if [ ! -f "docs/src/content/docs/formal-verification.mdx" ]; then
  echo "ERROR: docs/src/content/docs/formal-verification.mdx not found."
  exit 1
fi

# --- Patch hardcoded numbers in .mdx from YAML ---
echo ""
echo "--- Patching .mdx hardcoded counts from YAML ---"
MDX_FILE="docs/src/content/docs/formal-verification.mdx"
python3 - "$YML_FILE" "$MDX_FILE" <<'PYEOF'
import re, sys, yaml

yml_path, mdx_path = sys.argv[1], sys.argv[2]

with open(yml_path) as f:
    phases = yaml.safe_load(f)

# Build phase metadata map. F0 uses "F0", F1 uses "F1", etc.
phase_data = {}
for p in phases:
    pid = p['phase']
    phase_data[pid] = {
        'theorems': p['theorems'],
        'status': p['status'],
        'module': p.get('module', ''),
        'props': ', '.join(p.get('key_properties', [])),
        'extracted': bool(p.get('extracted', False)),
    }

status_meta = {
    'verified': {
        'phase_class': 'phase-verified',
        'ledger_class': 'ledger-verified',
        'pill_class': 'phase-pill-verified',
        'pill_html': '&#10003; Verified',
        'scroll_label': 'verified',
    },
    'in_progress': {
        'phase_class': 'phase-progress',
        'ledger_class': 'ledger-progress',
        'pill_class': 'phase-pill-progress',
        'pill_html': 'In Progress',
        'scroll_label': 'in progress',
    },
    'planned': {
        'phase_class': 'phase-planned',
        'ledger_class': 'ledger-planned',
        'pill_class': 'phase-pill-planned',
        'pill_html': 'Planned',
        'scroll_label': 'planned',
    },
}

extracted_badge = (
    '<span class="ledger-extracted"><svg width="11" height="11" viewBox="0 0 24 24" '
    'fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" '
    'stroke-linejoin="round" aria-hidden="true"><path d="M12 3v13M7 11l5 5 5-5"/>'
    '<path d="M5 20h14"/></svg>extracted</span>'
)

with open(mdx_path) as f:
    content = f.read()

changes = 0

def patch_phase_card_block(block):
    global changes
    m = re.search(r'<span class="phase-id">(F\d+)</span>', block)
    if not m:
        return block
    pid = m.group(1)
    if pid not in phase_data:
        return block
    meta = phase_data[pid]
    sm = status_meta[meta['status']]
    old = block
    block = re.sub(
        r'(<div class=")phase-card phase-[^"]+(">)',
        lambda m: f'{m.group(1)}phase-card {sm["phase_class"]}{m.group(2)}',
        block,
        count=1,
    )
    block = re.sub(
        r'<span class="phase-pill [^"]+">.*?</span>',
        f'<span class="phase-pill {sm["pill_class"]}">{sm["pill_html"]}</span>',
        block,
        count=1,
        flags=re.S,
    )
    if block != old:
        changes += 1
        print(f"  {pid} phase-card status -> {meta['status']}")
    return block

# 1. Patch ledger rows: find pattern <span class="ledger-id">F#</span> ...
#    then update module, properties, count, status, and extracted badge.
# Strategy: split into row blocks, patch each.
def patch_ledger_block(block):
    global changes
    m = re.search(r'<span class="ledger-id">(F\d+)</span>', block)
    if not m:
        return block
    pid = m.group(1)
    if pid not in phase_data:
        return block
    meta = phase_data[pid]
    sm = status_meta[meta['status']]
    n = meta['theorems']
    old = block
    block = re.sub(
        r'(<div class=")ledger-row ledger-[^"]+(">)',
        lambda m: f'{m.group(1)}ledger-row {sm["ledger_class"]}{m.group(2)}',
        block,
        count=1,
    )
    block = re.sub(
        r'(<code class="ledger-module">).*?(</code>)',
        lambda m: f'{m.group(1)}{meta["module"]}{m.group(2)}',
        block,
        count=1,
        flags=re.S,
    )
    block = re.sub(
        r'(<span class="ledger-props">).*?(</span>)',
        lambda m: f'{m.group(1)}{meta["props"]}{m.group(2)}',
        block,
        count=1,
        flags=re.S,
    )
    block = re.sub(
        r'(<span class="ledger-n">)\d+(</span>)',
        lambda m: f'{m.group(1)}{n}{m.group(2)}',
        block,
        count=1,
    )
    block = re.sub(
        r'<span class="phase-pill [^"]+">.*?</span>',
        f'<span class="phase-pill {sm["pill_class"]}">{sm["pill_html"]}</span>',
        block,
        count=1,
        flags=re.S,
    )
    block = re.sub(r'\s*<span class="ledger-extracted">.*?</span>', '', block, flags=re.S)
    if meta['extracted']:
        block = re.sub(
            r'(</div>\s*</div>\s*)$',
            f'      {extracted_badge}\n    </div>\n  </div>\n',
            block,
            count=1,
        )
    if block != old:
        changes += 1
        print(f"  {pid} ledger -> synced")
    return block

# 1a. Patch phase cards in the status grid.
parts = re.split(r'(<div class="phase-card[^"]*">)', content)
patched = []
i = 0
while i < len(parts):
    if re.match(r'<div class="phase-card', parts[i]):
        block = parts[i]
        if i + 1 < len(parts):
            block += parts[i + 1]
            i += 2
        else:
            i += 1
        patched.append(patch_phase_card_block(block))
    else:
        patched.append(parts[i])
        i += 1
content = ''.join(patched)

# 1b. Split on ledger-row divs, patch each
parts = re.split(r'(<div class="ledger-row[^"]*">)', content)
patched = []
i = 0
while i < len(parts):
    if re.match(r'<div class="ledger-row', parts[i]):
        # Collect the opening tag + content until next ledger-row or end
        block = parts[i]
        if i + 1 < len(parts):
            block += parts[i + 1]
            i += 2
        else:
            i += 1
        patched.append(patch_ledger_block(block))
    else:
        patched.append(parts[i])
        i += 1
content = ''.join(patched)

# 2. Patch scroll-count: <span class="scroll-count">N/N verified</span>
# These appear after <span class="scroll-title">F#: ...</span>
# Use a context-aware regex: find F# in scroll-title, then patch next scroll-count
def patch_scroll_counts(content):
    global changes
    # Find all scroll-item blocks
    items = list(re.finditer(
        r'<span class="scroll-title">F(\d+):[^<]*</span>',
        content
    ))
    for m in reversed(items):  # reverse to preserve offsets
        pid = f'F{m.group(1)}'
        if pid not in phase_data:
            continue
        n = phase_data[pid]['theorems']
        status = phase_data[pid]['status']
        # Find next scroll-count after this match
        rest = content[m.end():]
        sc = re.search(r'(<span class="scroll-count">)\d+/\d+ \w+(</span>)', rest)
        if sc:
            label = status_meta[status]['scroll_label']
            new_val = f'{sc.group(1)}{n}/{n} {label}{sc.group(2)}'
            old_val = sc.group(0)
            if old_val != new_val:
                pos = m.end() + sc.start()
                content = content[:pos] + new_val + content[pos + len(old_val):]
                changes += 1
                print(f"  {pid} scroll-count -> {n}/{n} {label}")
    return content

content = patch_scroll_counts(content)

with open(mdx_path, 'w') as f:
    f.write(content)

if changes == 0:
    print("  All .mdx counts already match YAML.")
else:
    print(f"  Patched {changes} count(s) in .mdx.")
PYEOF
echo ""
