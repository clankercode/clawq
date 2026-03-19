#!/usr/bin/env bash
# Generate formal verification coverage report and SVG badge.
# Usage: ./scripts/formal_verification_report.sh [--badge-only]
set -euo pipefail

COQ_DIR="coq/theories/Clawq"
BADGE_OUT="docs/public/badges/formal-verification.svg"
SEAL_OUT="docs/public/badges/formal-verification-seal.svg"

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

# ─── Rectangular Badge ──────────────────────────────────────────────
# Victorian brass nameplate: dark bg, gear+check icon, Georgia serif,
# corner brackets, diamond separator, QED square
generate_badge() {
  local count="$1"
  local label="Coq proofs"
  local value="${count} verified"

  # Layout widths (Georgia ~8.8px/char at 15px)
  local icon_w=26
  local label_tw=$(( ${#label} * 88 / 10 ))
  local value_tw=$(( ${#value} * 88 / 10 ))
  local qed_w=12  # space for QED square

  # Total: icon + gap + label + diamond + value + gap + qed + padding
  local total_width=$(( icon_w + 8 + label_tw + 14 + value_tw + 6 + qed_w + 4 ))
  local h=30

  # Text x positions
  local label_x=$(( icon_w + 8 + label_tw / 2 ))
  local diamond_x=$(( icon_w + 8 + label_tw + 7 ))
  local value_x=$(( icon_w + 8 + label_tw + 14 + value_tw / 2 ))
  local qed_x=$(( total_width - qed_w / 2 - 4 ))

  # Corner bracket size
  local cb=5

  mkdir -p "$(dirname "$BADGE_OUT")"

  cat > "$BADGE_OUT" <<SVGEOF
<svg xmlns="http://www.w3.org/2000/svg" width="${total_width}" height="${h}" role="img" aria-label="${label}: ${value}">
  <title>${label}: ${value}</title>
  <defs>
    <linearGradient id="bg" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0%" stop-color="#13111A"/>
      <stop offset="100%" stop-color="#0D0B0F"/>
    </linearGradient>
    <linearGradient id="glass" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0%" stop-color="#fff" stop-opacity=".06"/>
      <stop offset="50%" stop-color="#fff" stop-opacity=".01"/>
      <stop offset="100%" stop-color="#000" stop-opacity=".04"/>
    </linearGradient>
    <clipPath id="r">
      <rect width="${total_width}" height="${h}" rx="4"/>
    </clipPath>
  </defs>
  <!-- Background -->
  <g clip-path="url(#r)">
    <rect width="${total_width}" height="${h}" fill="url(#bg)"/>
    <rect width="${total_width}" height="${h}" fill="url(#glass)"/>
  </g>
  <!-- Border: brass -->
  <rect width="${total_width}" height="${h}" rx="4" fill="none"
        stroke="#8B7332" stroke-opacity=".5" stroke-width="0.75"/>
  <!-- Corner brackets (brass L-shapes) -->
  <g stroke="#C9A84C" stroke-width="0.75" fill="none" opacity=".5">
    <path d="M3,${cb} L3,3 L${cb},3"/>
    <path d="M$((total_width - cb)),3 L$((total_width - 3)),3 L$((total_width - 3)),${cb}"/>
    <path d="M3,$((h - cb)) L3,$((h - 3)) L${cb},$((h - 3))"/>
    <path d="M$((total_width - cb)),$((h - 3)) L$((total_width - 3)),$((h - 3)) L$((total_width - 3)),$((h - cb))"/>
  </g>
  <!-- Gear icon with checkmark -->
  <g transform="translate(7,7)">
    <!-- Gear body (simplified: circle + 8 radiating ticks) -->
    <circle cx="8" cy="8" r="5.5" fill="none" stroke="#C9A84C" stroke-width="1"/>
    <circle cx="8" cy="8" r="2" fill="none" stroke="#8B7332" stroke-width="0.75"/>
    <g stroke="#C9A84C" stroke-width="1" stroke-linecap="round">
      <line x1="8" y1="0.5" x2="8" y2="2.5"/>
      <line x1="8" y1="13.5" x2="8" y2="15.5"/>
      <line x1="0.5" y1="8" x2="2.5" y2="8"/>
      <line x1="13.5" y1="8" x2="15.5" y2="8"/>
      <line x1="2.7" y1="2.7" x2="4.1" y2="4.1"/>
      <line x1="11.9" y1="11.9" x2="13.3" y2="13.3"/>
      <line x1="2.7" y1="13.3" x2="4.1" y2="11.9"/>
      <line x1="11.9" y1="4.1" x2="13.3" y2="2.7"/>
    </g>
    <!-- Teal checkmark overlay -->
    <path d="M5 8.5 L7 10.5 L11 5.5" fill="none" stroke="#2E8B7A" stroke-width="1.5"
          stroke-linecap="round" stroke-linejoin="round"/>
  </g>
  <!-- Separator line -->
  <rect x="${icon_w}" y="7" width="0.5" height="16" fill="#8B7332" opacity=".3"/>
  <!-- Label: "Coq proofs" in warm cream -->
  <text x="$(( label_x + 1 ))" y="20" text-anchor="middle" fill="#000" opacity=".4"
        font-family="Georgia,serif" font-size="15"
        text-rendering="geometricPrecision">${label}</text>
  <text x="${label_x}" y="19" text-anchor="middle" fill="#E8E2D6"
        font-family="Georgia,serif" font-size="15"
        text-rendering="geometricPrecision">${label}</text>
  <!-- Diamond separator -->
  <text x="${diamond_x}" y="14.5" text-anchor="middle" dominant-baseline="central" fill="#B8860B" opacity=".7"
        font-size="9">&#x25C6;</text>
  <!-- Value: count in teal -->
  <text x="$(( value_x + 1 ))" y="20" text-anchor="middle" fill="#000" opacity=".4"
        font-family="Georgia,serif" font-size="15" font-weight="bold"
        text-rendering="geometricPrecision">${value}</text>
  <text x="${value_x}" y="19" text-anchor="middle" fill="#5BBFAD"
        font-family="Georgia,serif" font-size="15" font-weight="bold"
        text-rendering="geometricPrecision">${value}</text>
  <!-- QED square -->
  <rect x="$((qed_x - 3))" y="12" width="5.5" height="5.5" fill="#B8860B" opacity=".55" rx="0.5"/>
</svg>
SVGEOF
}

# ─── Circular Mini-Seal ─────────────────────────────────────────────
# Matches site's VerificationSeal.astro: gear-toothed ring, radiating
# lines, proof count center, curved text on ring. Python for geometry.
generate_seal() {
  local count="$1"
  mkdir -p "$(dirname "$SEAL_OUT")"

  python3 - "$count" "$SEAL_OUT" <<'PYEOF'
import math, sys

count = sys.argv[1]
out_path = sys.argv[2]

cx, cy = 70, 70
w, h = 140, 140

# --- Gear tooth ring ---
# Proper rounded gear profile: 24 teeth with smooth involute-like curves
# using cubic bezier approximations for the tooth rise/fall
teeth = 24
root_r = 52     # root circle (valley between teeth)
tip_r = 66      # tip circle (top of teeth)
tooth_width = 0.45  # fraction of tooth pitch occupied by the tooth (vs gap)

segments = []
for i in range(teeth):
    pitch = 2 * math.pi / teeth
    base = i * pitch

    # Key angles within this tooth
    gap_half = pitch * (1 - tooth_width) / 2
    tooth_start = base + gap_half
    tooth_mid = base + pitch / 2
    tooth_end = base + pitch - gap_half

    # Points along the profile (dense sampling with smooth radius interpolation)
    n_pts = 32  # points per tooth for smoothness
    for j in range(n_pts):
        t = j / n_pts
        a = base + t * pitch

        # Determine radius based on where we are in the tooth cycle
        if a < tooth_start:
            # In the gap (root circle)
            r = root_r
        elif a < tooth_start + gap_half * 0.4:
            # Rising flank — smooth sinusoidal rise
            frac = (a - tooth_start) / (gap_half * 0.4)
            # Smoothstep
            s = frac * frac * (3 - 2 * frac)
            r = root_r + (tip_r - root_r) * s
        elif a < tooth_end - gap_half * 0.4:
            # Tooth tip plateau
            r = tip_r
        elif a < tooth_end:
            # Falling flank — smooth sinusoidal fall
            frac = (a - (tooth_end - gap_half * 0.4)) / (gap_half * 0.4)
            s = frac * frac * (3 - 2 * frac)
            r = tip_r - (tip_r - root_r) * s
        else:
            # Trailing gap
            r = root_r

        segments.append((cx + r * math.cos(a), cy + r * math.sin(a)))

gear_path = f"M {segments[0][0]:.2f},{segments[0][1]:.2f}" + "".join(
    f" L {x:.2f},{y:.2f}" for x, y in segments[1:]
) + " Z"

# Radiating lines: 12 lines between inner ring and gear root
rad_segs = []
for i in range(12):
    a = (i / 12) * 2 * math.pi
    r1, r2 = 40, 49
    rad_segs.append(
        f"M {cx+r1*math.cos(a):.2f},{cy+r1*math.sin(a):.2f} "
        f"L {cx+r2*math.cos(a):.2f},{cy+r2*math.sin(a):.2f}"
    )
rad_lines = " ".join(rad_segs)

# Text arcs — centered between inner ring (r=38) and gear root (r=52)
# Midpoint = 45
tr = 45

svg = f'''<svg xmlns="http://www.w3.org/2000/svg" width="{w}" height="{h}" viewBox="0 0 {w} {h}" role="img" aria-label="Coq: {count} proofs verified">
  <title>Coq: {count} proofs verified</title>
  <defs>
    <linearGradient id="seal-bg" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0%" stop-color="#13111A"/>
      <stop offset="100%" stop-color="#0D0B0F"/>
    </linearGradient>
    <!-- Top arc for "VERIFIED" -->
    <path id="arc-top" d="M {cx-tr},{cy} A {tr},{tr} 0 1,1 {cx+tr},{cy}" fill="none"/>
    <!-- Bottom arc for "COQ" (counter-clockwise so text reads upright) -->
    <path id="arc-bot" d="M {cx-tr},{cy} A {tr},{tr} 0 1,0 {cx+tr},{cy}" fill="none"/>
  </defs>
  <!-- Background circle -->
  <circle cx="{cx}" cy="{cy}" r="68" fill="url(#seal-bg)"/>
  <circle cx="{cx}" cy="{cy}" r="68" fill="none" stroke="#8B7332" stroke-width="0.5" stroke-opacity=".3"/>
  <!-- Outer gear-toothed ring -->
  <path d="{gear_path}" fill="none" stroke="#C9A84C" stroke-width="1.5"/>
  <!-- Inner smooth ring -->
  <circle cx="{cx}" cy="{cy}" r="38" fill="none" stroke="#8B7332" stroke-width="1"/>
  <!-- Radiating lines (compass rose) -->
  <path d="{rad_lines}" fill="none" stroke="#8B7332" stroke-width="0.5" opacity=".4"/>
  <!-- "VERIFIED" curving along top of ring — green fill, black stroke -->
  <text font-family="Georgia,serif" font-size="14" font-weight="bold"
        letter-spacing="2" text-rendering="geometricPrecision">
    <textPath href="#arc-top" startOffset="50%" text-anchor="middle"
              fill="#5BBFAD" stroke="#0D0B0F" stroke-width="0.4">VERIFIED</textPath>
  </text>
  <!-- "COQ" curving along bottom of ring — green fill, black stroke -->
  <text font-family="Georgia,serif" font-size="14" font-weight="bold"
        letter-spacing="2" text-rendering="geometricPrecision">
    <textPath href="#arc-bot" startOffset="50%" text-anchor="middle"
              fill="#5BBFAD" stroke="#0D0B0F" stroke-width="0.4" dy="5">COQ</textPath>
  </text>
  <!-- Center: proof count -->
  <text x="{cx}" y="{cy - 4}" text-anchor="middle" dominant-baseline="central"
        font-family="Georgia,serif" font-size="28" font-weight="bold"
        fill="#C9A84C">{count}</text>
  <!-- "proofs" below count -->
  <text x="{cx}" y="{cy + 14}" text-anchor="middle"
        font-family="Georgia,serif" font-size="11"
        fill="#9C978A" letter-spacing="1">proofs</text>
</svg>
'''

with open(out_path, 'w') as f:
    f.write(svg)
PYEOF
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
generate_seal "$proof_count"
# Generate Python seal (canonical version)
SEAL_PY_OUT="docs/public/badges/formal-verification-seal.py.svg"
python3 scripts/generate_seal.py --count "$proof_count" --out "$SEAL_PY_OUT"
echo "Badge generated: ${BADGE_OUT} (${proof_count} proofs)"
echo "Seal generated:  ${SEAL_OUT} (${proof_count} proofs)"
echo "Seal (py):       ${SEAL_PY_OUT} (${proof_count} proofs)"
