#!/usr/bin/env python3
"""Generate the formal verification seal SVG.

Usage:
    python3 scripts/generate_seal.py [--count N] [--out PATH]

Defaults to counting proofs from coq/ and writing to
docs/badges/formal-verification-seal.py.svg (adjacent to the bash-generated one).
"""
import argparse
import math
import os
import re
import subprocess
import sys
from pathlib import Path


# ── Geometry helpers ────────────────────────────────────────────────

def gear_ring(cx, cy, teeth, root_r, tip_r, tooth_width=0.45, pts_per_tooth=32):
    """Generate a gear ring with smooth sinusoidal teeth.

    Uses a cosine wave to create naturally rounded teeth and valleys
    with equal width. No flat sections — everything is curved like
    a real involute gear profile.

    tooth_width: fraction of pitch that is "above the midline" (tip side).
                 0.5 = symmetric teeth and valleys (equal width).
    """
    points = []
    pitch = 2 * math.pi / teeth
    mid_r = (root_r + tip_r) / 2
    amp = (tip_r - root_r) / 2

    for i in range(teeth):
        base = i * pitch
        for j in range(pts_per_tooth):
            frac = j / pts_per_tooth  # 0..1 within this tooth's pitch
            a = base + frac * pitch

            # Cosine wave: 0.0 = valley center, 0.5 = tip center
            # Offset so tooth center is at frac=0.5
            r = mid_r + amp * math.cos(2 * math.pi * (frac - 0.5))

            points.append((cx + r * math.cos(a), cy + r * math.sin(a)))

    d = f"M {points[0][0]:.2f},{points[0][1]:.2f}"
    d += "".join(f" L {x:.2f},{y:.2f}" for x, y in points[1:])
    d += " Z"
    return d


def radiating_lines(cx, cy, r_inner, r_outer, count):
    """Generate compass-rose radiating line segments."""
    segs = []
    for i in range(count):
        a = (i / count) * 2 * math.pi
        x1, y1 = cx + r_inner * math.cos(a), cy + r_inner * math.sin(a)
        x2, y2 = cx + r_outer * math.cos(a), cy + r_outer * math.sin(a)
        segs.append(f"M {x1:.2f},{y1:.2f} L {x2:.2f},{y2:.2f}")
    return " ".join(segs)


# ── Proof counting ──────────────────────────────────────────────────

def count_proofs(coq_dir="coq/theories/Clawq"):
    """Count Theorem/Lemma declarations in .v files."""
    total = 0
    coq_path = Path(coq_dir)
    if not coq_path.exists():
        return 0
    for f in sorted(coq_path.glob("*.v")):
        text = f.read_text()
        total += len(re.findall(r"^\s*(Theorem|Lemma)\b", text, re.MULTILINE))
    return total


# ── SVG generation ──────────────────────────────────────────────────

def generate_seal_svg(proof_count, **overrides):
    """Return the complete SVG string for the verification seal.

    All layout values can be overridden via keyword args (None = use default).
    """

    def ov(key, default):
        """Return override if not None, else default."""
        v = overrides.get(key)
        return v if v is not None else default

    # ── Layout ──────────────────────────────────────────────────────
    cx, cy = 70, 70           # center point
    w, h = 140, 140           # SVG dimensions
    bg_r = 68                 # background circle radius

    # ── Gear ────────────────────────────────────────────────────────
    gear_teeth = ov("gear_teeth", 18)
    gear_root_r = ov("gear_root_r", 52)       # valley between teeth
    gear_tip_r = ov("gear_tip_r", 66)         # tip of teeth
    gear_tooth_width = ov("gear_tooth_width", 0.45)  # fraction of pitch occupied by tooth
    gear_pts_per_tooth = ov("gear_pts_per_tooth", 48)  # smoothness
    gear_stroke_w = 1.5

    # ── Inner ring ──────────────────────────────────────────────────
    inner_ring_r = 38
    inner_ring_stroke_w = 1

    # ── Radiating lines ─────────────────────────────────────────────
    rad_count = 12
    rad_r_inner = 40
    rad_r_outer = 49          # should stay inside text arc

    # ── Ring text: "VERIFIED" (top) ─────────────────────────────────
    top_text = "VERIFIED"
    top_arc_r = ov("top_arc_r", 40.5)        # arc radius for top text path
    top_font_size = ov("top_font_size", 14)
    top_letter_spacing = ov("top_letter_spacing", 2)
    top_dy = ov("top_dy", 0)                  # perpendicular offset (negative=outward, positive=inward)

    # ── Ring text: "COQ" (bottom) ───────────────────────────────────
    bot_text = "COQ"
    bot_arc_r = ov("bot_arc_r", 49)           # arc radius for bottom text path
    bot_font_size = ov("bot_font_size", 14)
    bot_letter_spacing = ov("bot_letter_spacing", 3)
    bot_dy = ov("bot_dy", 5)                  # perpendicular offset (positive=inward toward center)
    bot_start_offset = ov("bot_start_offset", 51.34)  # % along arc (50=centered)

    # ── Ring text style (shared) ────────────────────────────────────
    ring_text_fill = "#5BBFAD"
    ring_text_stroke = "#0D0B0F"
    ring_text_stroke_w = 0.4

    # ── Center text ─────────────────────────────────────────────────
    count_font_size = ov("count_font_size", 38)
    count_y_offset = ov("count_y_offset", -4)  # relative to cy
    label_text = "proofs"
    label_font_size = ov("label_font_size", 20)
    label_y_offset = ov("label_y_offset", 15)  # relative to cy
    label_stroke_w = ov("label_stroke_w", 2.0)
    label_font_weight = ov("label_font_weight", 600)

    # ── Colors ──────────────────────────────────────────────────────
    bg_dark = "#0D0B0F"
    bg_light = "#13111A"
    brass = "#C9A84C"
    brass_dark = "#8B7332"
    teal_light = "#5BBFAD"
    text_muted = "#9C978A"
    gold = "#C9A84C"

    # ── Generate geometry ───────────────────────────────────────────
    gear_path = gear_ring(cx, cy, gear_teeth, gear_root_r, gear_tip_r,
                          gear_tooth_width, gear_pts_per_tooth)
    rad_lines = radiating_lines(cx, cy, rad_r_inner, rad_r_outer, rad_count)

    # Build dy attributes (omit if 0)
    top_dy_attr = f' dy="{top_dy}"' if top_dy else ""
    bot_dy_attr = f' dy="{bot_dy}"' if bot_dy else ""

    return f"""\
<svg xmlns="http://www.w3.org/2000/svg" width="{w}" height="{h}" viewBox="0 0 {w} {h}" role="img" aria-label="Coq: {proof_count} proofs verified">
  <title>Coq: {proof_count} proofs verified</title>
  <defs>
    <linearGradient id="seal-bg" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0%" stop-color="{bg_light}"/>
      <stop offset="100%" stop-color="{bg_dark}"/>
    </linearGradient>
    <!-- Top arc for "{top_text}" -->
    <path id="arc-top" d="M {cx - top_arc_r},{cy} A {top_arc_r},{top_arc_r} 0 1,1 {cx + top_arc_r},{cy}" fill="none"/>
    <!-- Bottom arc for "{bot_text}" (counter-clockwise so text reads upright) -->
    <path id="arc-bot" d="M {cx - bot_arc_r},{cy} A {bot_arc_r},{bot_arc_r} 0 1,0 {cx + bot_arc_r},{cy}" fill="none"/>
  </defs>
  <!-- Background circle -->
  <circle cx="{cx}" cy="{cy}" r="{bg_r}" fill="url(#seal-bg)"/>
  <circle cx="{cx}" cy="{cy}" r="{bg_r}" fill="none" stroke="{brass_dark}" stroke-width="0.5" stroke-opacity=".3"/>
  <!-- Outer gear-toothed ring -->
  <path d="{gear_path}" fill="none" stroke="{brass}" stroke-width="{gear_stroke_w}"/>
  <!-- Inner smooth ring -->
  <circle cx="{cx}" cy="{cy}" r="{inner_ring_r}" fill="none" stroke="{brass_dark}" stroke-width="{inner_ring_stroke_w}"/>
  <!-- Radiating lines (compass rose) -->
  <path d="{rad_lines}" fill="none" stroke="{brass_dark}" stroke-width="0.5" opacity=".4"/>
  <!-- "{top_text}" curving along top of ring -->
  <text font-family="Georgia,serif" font-size="{top_font_size}" font-weight="bold"
        letter-spacing="{top_letter_spacing}" text-rendering="geometricPrecision">
    <textPath href="#arc-top" startOffset="50%" text-anchor="middle"
              fill="{ring_text_fill}" stroke="{ring_text_stroke}" stroke-width="{ring_text_stroke_w}"{top_dy_attr}>{top_text}</textPath>
  </text>
  <!-- "{bot_text}" curving along bottom of ring -->
  <text font-family="Georgia,serif" font-size="{bot_font_size}" font-weight="bold"
        letter-spacing="{bot_letter_spacing}" text-rendering="geometricPrecision">
    <textPath href="#arc-bot" startOffset="{bot_start_offset}%" text-anchor="middle"
              fill="{ring_text_fill}" stroke="{ring_text_stroke}" stroke-width="{ring_text_stroke_w}"{bot_dy_attr}>{bot_text}</textPath>
  </text>
  <!-- Center: proof count -->
  <text x="{cx}" y="{cy + count_y_offset}" text-anchor="middle" dominant-baseline="central"
        font-family="'Courier New',monospace" font-size="{count_font_size}" font-weight="bold"
        fill="{gold}">{proof_count}</text>
  <!-- "proofs" below count -->
  <text x="{cx}" y="{cy + label_y_offset}" text-anchor="middle"
        font-family="Georgia,serif" font-size="{label_font_size}" font-weight="{label_font_weight}"
        fill="{text_muted}" stroke="{bg_dark}" stroke-width="{label_stroke_w}"
        paint-order="stroke" letter-spacing="1">{label_text}</text>
</svg>
"""


# ── Main ────────────────────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser(description="Generate formal verification seal SVG")
    parser.add_argument("--count", type=int, default=None,
                        help="Proof count (default: auto-detect from coq/)")
    parser.add_argument("--out", type=str, default="docs/badges/formal-verification-seal.py.svg",
                        help="Output path")
    # Text positioning overrides
    parser.add_argument("--top-arc-r", type=float, default=None,
                        help="Arc radius for VERIFIED text (default: 45)")
    parser.add_argument("--bot-arc-r", type=float, default=None,
                        help="Arc radius for COQ text (default: 45)")
    parser.add_argument("--top-dy", type=float, default=None,
                        help="VERIFIED perpendicular offset (default: 0)")
    parser.add_argument("--bot-dy", type=float, default=None,
                        help="COQ perpendicular offset (default: 5)")
    parser.add_argument("--top-font-size", type=float, default=None,
                        help="VERIFIED font size (default: 14)")
    parser.add_argument("--bot-font-size", type=float, default=None,
                        help="COQ font size (default: 14)")
    # Gear overrides
    parser.add_argument("--gear-teeth", type=int, default=None)
    parser.add_argument("--gear-root-r", type=float, default=None)
    parser.add_argument("--gear-tip-r", type=float, default=None)
    parser.add_argument("--gear-tooth-width", type=float, default=None)
    parser.add_argument("--gear-pts", type=int, default=None,
                        help="Points per tooth for smoothness (default: 32)")
    parser.add_argument("--top-letter-spacing", type=float, default=None)
    parser.add_argument("--bot-letter-spacing", type=float, default=None)
    parser.add_argument("--count-font-size", type=float, default=None)
    parser.add_argument("--count-y-offset", type=float, default=None)
    parser.add_argument("--label-font-size", type=float, default=None)
    parser.add_argument("--label-y-offset", type=float, default=None)
    parser.add_argument("--label-stroke-width", type=float, default=None)
    parser.add_argument("--bot-start-offset", type=float, default=None,
                        help="Bottom text position along arc in %% (default: 50 = centered)")
    parser.add_argument("--label-font-weight", type=int, default=None,
                        help="Font weight for 'proofs' label (default: 600, semi-bold)")
    parser.add_argument("--ring-font-size", type=float, default=None,
                        help="Font size for ring text (VERIFIED/COQ, default: 14)")
    args = parser.parse_args()

    proof_count = args.count if args.count is not None else count_proofs()
    if proof_count == 0:
        print("Warning: 0 proofs found, using 0", file=sys.stderr)

    svg = generate_seal_svg(
        proof_count,
        top_arc_r=args.top_arc_r,
        bot_arc_r=args.bot_arc_r,
        top_dy=args.top_dy,
        bot_dy=args.bot_dy,
        top_font_size=args.top_font_size if args.top_font_size is not None else args.ring_font_size,
        bot_font_size=args.bot_font_size if args.bot_font_size is not None else args.ring_font_size,
        gear_teeth=args.gear_teeth,
        gear_root_r=args.gear_root_r,
        gear_tip_r=args.gear_tip_r,
        gear_tooth_width=args.gear_tooth_width,
        gear_pts_per_tooth=args.gear_pts,
        top_letter_spacing=args.top_letter_spacing,
        bot_letter_spacing=args.bot_letter_spacing,
        count_font_size=args.count_font_size,
        count_y_offset=args.count_y_offset,
        label_font_size=args.label_font_size,
        label_y_offset=args.label_y_offset,
        label_stroke_w=args.label_stroke_width,
        bot_start_offset=args.bot_start_offset,
        label_font_weight=args.label_font_weight,
    )

    out = Path(args.out)
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text(svg)
    print(f"Seal generated: {out} ({proof_count} proofs)")


if __name__ == "__main__":
    main()
