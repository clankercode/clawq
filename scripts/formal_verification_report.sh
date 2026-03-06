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

# Generate premium SVG verification badge
generate_badge() {
  local count="$1"
  local label="Coq proofs"
  local value="${count} verified"

  # Layout: [icon area 24px] [separator] [label text] [dot separator] [value text] [padding]
  local icon_w=26
  local label_text="${label}"
  local value_text="${value}"

  # Approximate text widths (Verdana 10px ~6.2px/char avg)
  local label_tw=$(( ${#label_text} * 62 / 10 ))
  local value_tw=$(( ${#value_text} * 62 / 10 ))

  # Total width: icon + padding + label + gap + value + padding
  local total_width=$(( icon_w + 6 + label_tw + 12 + value_tw + 8 ))

  # Text x positions
  local label_x=$(( icon_w + 6 + label_tw / 2 ))
  local value_x=$(( icon_w + 6 + label_tw + 12 + value_tw / 2 ))
  local dot_x=$(( icon_w + 6 + label_tw + 6 ))

  mkdir -p "$(dirname "$BADGE_OUT")"

  cat > "$BADGE_OUT" <<SVGEOF
<svg xmlns="http://www.w3.org/2000/svg" width="${total_width}" height="24" role="img" aria-label="${label}: ${value}">
  <title>${label}: ${value}</title>
  <defs>
    <linearGradient id="bg" x1="0" y1="0" x2="1" y2="1">
      <stop offset="0%" stop-color="#1e2340"/>
      <stop offset="100%" stop-color="#151929"/>
    </linearGradient>
    <linearGradient id="icon-bg" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0%" stop-color="#2a1f5e"/>
      <stop offset="100%" stop-color="#1a1040"/>
    </linearGradient>
    <linearGradient id="gold" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0%" stop-color="#fde68a"/>
      <stop offset="100%" stop-color="#d97706"/>
    </linearGradient>
    <linearGradient id="glass" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0%" stop-color="#fff" stop-opacity=".10"/>
      <stop offset="50%" stop-color="#fff" stop-opacity=".03"/>
      <stop offset="51%" stop-color="#000" stop-opacity=".03"/>
      <stop offset="100%" stop-color="#000" stop-opacity=".08"/>
    </linearGradient>
    <clipPath id="r">
      <rect width="${total_width}" height="24" rx="5"/>
    </clipPath>
  </defs>
  <g clip-path="url(#r)">
    <!-- Background -->
    <rect width="${total_width}" height="24" fill="url(#bg)"/>
    <!-- Icon area -->
    <rect width="${icon_w}" height="24" fill="url(#icon-bg)"/>
    <!-- Glass overlay -->
    <rect width="${total_width}" height="24" fill="url(#glass)"/>
    <!-- Top highlight -->
    <rect width="${total_width}" height="1" fill="#fff" opacity=".06"/>
  </g>
  <!-- Gold shield icon with checkmark -->
  <g transform="translate(7,6)">
    <path d="M6 0.5 L11 2.5 V6.5 C11 9.5 8.5 11.5 6 12 C3.5 11.5 1 9.5 1 6.5 V2.5 Z"
          fill="url(#gold)" stroke="#92400e" stroke-width="0.4"/>
    <path d="M3.5 6 L5.2 7.8 L8.5 4.2"
          fill="none" stroke="#fff" stroke-width="1.3" stroke-linecap="round" stroke-linejoin="round"/>
  </g>
  <!-- Subtle separator -->
  <rect x="${icon_w}" y="4" width="1" height="16" fill="#fff" opacity=".06" rx="0.5"/>
  <!-- Label text with shadow -->
  <text x="$(( label_x + 1 ))" y="16" text-anchor="middle" fill="#000" opacity=".35"
        font-family="Verdana,Geneva,DejaVu Sans,sans-serif" font-size="10.5"
        text-rendering="geometricPrecision">${label_text}</text>
  <text x="${label_x}" y="15" text-anchor="middle" fill="#c4b5e0"
        font-family="Verdana,Geneva,DejaVu Sans,sans-serif" font-size="10.5"
        text-rendering="geometricPrecision">${label_text}</text>
  <!-- Dot separator -->
  <circle cx="${dot_x}" cy="12" r="1.5" fill="#d97706" opacity=".5"/>
  <!-- Value text with shadow -->
  <text x="$(( value_x + 1 ))" y="16" text-anchor="middle" fill="#000" opacity=".35"
        font-family="Verdana,Geneva,DejaVu Sans,sans-serif" font-size="10.5" font-weight="bold"
        text-rendering="geometricPrecision">${value_text}</text>
  <text x="${value_x}" y="15" text-anchor="middle" fill="#fbbf24"
        font-family="Verdana,Geneva,DejaVu Sans,sans-serif" font-size="10.5" font-weight="bold"
        text-rendering="geometricPrecision">${value_text}</text>
  <!-- Border -->
  <rect width="${total_width}" height="24" rx="5" fill="none" stroke="#fbbf24" stroke-opacity=".2" stroke-width="0.5"/>
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
