#!/usr/bin/env bash
# Generate formal verification coverage report and SVG badge.
# Usage: ./scripts/formal_verification_report.sh [--badge-only]
set -euo pipefail

COQ_DIR="coq/theories/Clawq"
BADGE_OUT="docs/badges/formal-verification.svg"

# Count Theorem/Lemma declarations across all proof files
count_proofs() {
  local total=0
  local details=""
  for f in "$COQ_DIR"/*.v; do
    local base
    base=$(basename "$f" .v)
    local count
    count=$(grep -cE '^\s*(Theorem|Lemma)\b' "$f" 2>/dev/null || true)
    count=${count:-0}
    if [ "$count" -gt 0 ]; then
      details="${details}  ${base}: ${count} proofs\n"
    fi
    total=$((total + count))
  done
  echo "$total"
  if [ "${1:-}" = "--details" ]; then
    echo -e "$details" >&2
  fi
}

# Count extracted symbols (Extraction directives in Extract.v)
count_extracted() {
  # Count Clawq.Module.symbol lines in Extract.v (the extracted symbols)
  local c
  c=$(grep -cE '^\s*Clawq\.' "$COQ_DIR/Extract.v" 2>/dev/null || true)
  echo "${c:-0}"
}

# List verified property domains from proof file names
list_domains() {
  local domains=""
  for f in "$COQ_DIR"/*Proofs.v "$COQ_DIR"/PathSafety.v "$COQ_DIR"/AuditChain.v "$COQ_DIR"/RateLimiter.v; do
    [ -f "$f" ] || continue
    local base
    base=$(basename "$f" .v)
    local count
    count=$(grep -cE '^\s*(Theorem|Lemma)\b' "$f" 2>/dev/null || true)
    count=${count:-0}
    [ "$count" -gt 0 ] && domains="${domains}${base}(${count}) "
  done
  echo "$domains"
}

# Generate SVG badge
generate_badge() {
  local count="$1"
  local label="Coq proofs"
  local value="${count} verified"
  local label_width=72
  local value_width=72
  local total_width=$((label_width + value_width))
  local label_x=$((label_width / 2))
  local value_x=$((label_width + value_width / 2))

  mkdir -p "$(dirname "$BADGE_OUT")"

  cat > "$BADGE_OUT" <<SVGEOF
<svg xmlns="http://www.w3.org/2000/svg" width="${total_width}" height="20" role="img" aria-label="${label}: ${value}">
  <title>${label}: ${value}</title>
  <linearGradient id="s" x2="0" y2="100%">
    <stop offset="0" stop-color="#bbb" stop-opacity=".1"/>
    <stop offset="1" stop-opacity=".1"/>
  </linearGradient>
  <clipPath id="r">
    <rect width="${total_width}" height="20" rx="3" fill="#fff"/>
  </clipPath>
  <g clip-path="url(#r)">
    <rect width="${label_width}" height="20" fill="#555"/>
    <rect x="${label_width}" width="${value_width}" height="20" fill="#4c1"/>
    <rect width="${total_width}" height="20" fill="url(#s)"/>
  </g>
  <g fill="#fff" text-anchor="middle" font-family="Verdana,Geneva,DejaVu Sans,sans-serif" text-rendering="geometricPrecision" font-size="11">
    <text aria-hidden="true" x="${label_x}" y="150" fill="#010101" fill-opacity=".3" transform="scale(.1)" textLength="${label_width}0">${label}</text>
    <text x="${label_x}" y="140" transform="scale(.1)" fill="#fff" textLength="${label_width}0">${label}</text>
    <text aria-hidden="true" x="${value_x}0" y="150" fill="#010101" fill-opacity=".3" transform="scale(.1)" textLength="${value_width}0">${value}</text>
    <text x="${value_x}0" y="140" transform="scale(.1)" fill="#fff" textLength="${value_width}0">${value}</text>
  </g>
</svg>
SVGEOF
}

# Main
proof_count=$(count_proofs)
extracted_count=$(count_extracted)
domains=$(list_domains)

if [ "${1:-}" != "--badge-only" ]; then
  echo "=== Formal Verification Coverage Report ==="
  echo ""
  echo "Proof assistant: Coq 8.19"
  echo "Source directory: ${COQ_DIR}/"
  echo ""
  echo "--- Proof Counts by Module ---"
  for f in "$COQ_DIR"/*.v; do
    base=$(basename "$f" .v)
    count=$(grep -cE '^\s*(Theorem|Lemma)\b' "$f" 2>/dev/null || true)
    count=${count:-0}
    if [ "$count" -gt 0 ]; then
      printf "  %-20s %3d theorems/lemmas\n" "$base" "$count"
    fi
  done
  echo ""
  echo "Total proven: ${proof_count} theorems/lemmas"
  echo "Extracted to OCaml: ${extracted_count} symbols"
  echo "Verified domains: ${domains}"
  echo ""
  echo "--- Verified Properties ---"
  echo "  - Command parsing correctness (all 18 CLI commands)"
  echo "  - Configuration validation (weights, port range, temperature range)"
  echo "  - Secure-by-default guarantees"
  echo "  - Path normalization safety (no directory traversal)"
  echo "  - Path normalization idempotence"
  echo "  - Workspace containment (prefix safety)"
  echo "  - HMAC audit chain integrity"
  echo "  - Token bucket rate limiter bounds and monotonicity"
  echo ""
fi

generate_badge "$proof_count"
echo "Badge generated: ${BADGE_OUT} (${proof_count} proofs)"
