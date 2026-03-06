# Clawq Documentation Site — Design Specification

## "The Formal Verification"

**Concept:** A Victorian academic treatise on machine-checked correctness. The site reads like a formal mathematical publication from an alternate timeline where Babbage completed his Analytical Engine. The word "formal" appears everywhere — in the math, in the manners, in the dress code of a steampunk lobster gentleman.

**Domain:** clawq.org

**Framework:** Astro (static site, GitHub Pages deployment from `docs/`)

---

## 1. Color Palette

### Dark Mode (Default) — "The Clockwork Study"

| Role | Hex | Description |
|------|-----|-------------|
| Background primary | `#0D0B0F` | Near-black with faint violet warmth |
| Background secondary | `#13111A` | Sidebar, card surfaces |
| Background tertiary | `#1A1822` | Code blocks, table rows, elevated surfaces |
| Background hover | `#22202E` | Interactive hover states |
| Brass primary | `#C9A84C` | Main accent — links, active nav, icons |
| Brass light | `#E2C97E` | Hover state, highlighted elements |
| Brass dark | `#8B7332` | Borders, secondary accents, muted brass |
| Brass glow | `#C9A84C1A` | 10% opacity brass for subtle background washes |
| Gold bright | `#F0D878` | Verification badges, completed status (sparingly) |
| Coq teal | `#2E8B7A` | Coq-related UI, extraction status, verified theorems |
| Coq teal light | `#5BBFAD` | Teal on dark backgrounds |
| Text primary | `#E8E2D6` | Warm cream body text |
| Text secondary | `#9C978A` | Captions, metadata, sidebar inactive |
| Text tertiary | `#6B6660` | Timestamps, line numbers, lowest emphasis |
| Error red | `#C44B3F` | Errors, warnings |
| Info blue | `#4A6B8A` | Informational callouts |
| QED gold | `#B8860B` | QED markers, theorem labels |

### Light Mode — "The Patent Office"

| Role | Hex | Description |
|------|-----|-------------|
| Background primary | `#FAF6F0` | Warm parchment cream |
| Background secondary | `#F3EDE4` | Sidebar, card surfaces |
| Background tertiary | `#EDEADF` | Code blocks, table rows |
| Brass primary | `#7A6321` | Darkened brass for legibility on light bg |
| Brass dark | `#5C4A18` | Borders on light backgrounds |
| Coq teal | `#2E8B7A` | Same teal works on both modes |
| Text primary | `#2A2520` | Dark sepia ink |
| Text secondary | `#6B5E52` | Faded ink |
| Text tertiary | `#9C8E80` | Annotations, captions |
| QED gold | `#B8860B` | Same gold on both modes |
| Border | `#D6CDBF` | Subtle warm rule lines |
| Border emphasis | `#B8AFA0` | Table rules, stronger dividers |

---

## 2. Typography

### Font Stack

| Role | Font | Fallback | Weights | Usage |
|------|------|----------|---------|-------|
| Headings | Playfair Display | Georgia, serif | 400, 700, 900 | H1-H3, hero title, drop caps |
| Section labels | EB Garamond | Georgia, serif | 500, small-caps | Sidebar group headers, table headers, section markers |
| Body | Source Serif 4 | Charter, Georgia, serif | 400, 500, 600 | Prose, descriptions |
| UI / labels | Inter | system-ui, sans-serif | 400, 500, 600 | Breadcrumbs, version badges, search bar, nav items |
| Code | JetBrains Mono | Consolas, monospace | 400, 500 | Code blocks, inline code, proof references |
| Logo wordmark | Uncial Antiqua | fantasy | 400 | "Clawq" wordmark only (landing page, sidebar top) |

### Type Scale (fluid, clamp-based)

```css
--fs-xs:   clamp(0.72rem, 0.68rem + 0.2vw, 0.8rem);    /* captions */
--fs-sm:   clamp(0.83rem, 0.78rem + 0.25vw, 0.9rem);   /* small text */
--fs-base: clamp(0.95rem, 0.9rem + 0.25vw, 1.05rem);   /* body */
--fs-md:   clamp(1.1rem, 1rem + 0.5vw, 1.25rem);       /* h4 */
--fs-lg:   clamp(1.3rem, 1.1rem + 1vw, 1.6rem);        /* h3 */
--fs-xl:   clamp(1.6rem, 1.3rem + 1.5vw, 2.1rem);      /* h2 */
--fs-2xl:  clamp(2rem, 1.5rem + 2.5vw, 2.8rem);        /* h1 */
--fs-hero: clamp(2.5rem, 1.8rem + 3.5vw, 4rem);        /* landing hero */
```

### Line Heights

- Body: `1.72` (generous, academic reading)
- Headings: `1.2`
- Code: `1.55`
- Paragraph spacing: `1.25em` margin-bottom

### Heading Decoration

- H1: Playfair Display 900. Bottom border: `2px solid var(--brass-primary)` with a small gear icon (16px SVG) centered on it via pseudo-element.
- H2: Playfair Display 700. Thin `1px` top rule in `var(--border)` with `2rem` padding-top above the rule. Creates clear section breaks.
- Section labels (sidebar groups): EB Garamond small-caps, `--fs-xs`, `letter-spacing: 0.12em`, uppercase.

### Drop Caps

First paragraph of each documentation page gets a drop cap:

```css
.doc-content > p:first-of-type::first-letter {
  font-family: "Playfair Display", serif;
  font-weight: 900;
  font-size: 3.5em;
  float: left;
  line-height: 0.8;
  margin-right: 8px;
  margin-top: 4px;
  color: var(--brass-primary);
}
```

---

## 3. Layout

### Grid Structure

```
Max width: 1400px, centered

+------------+------------------------------+-----------+
| Sidebar    | Content Area                 | TOC Rail  |
| 260px      | max-width: 720px prose       | 200px     |
| fixed      | centered in remaining space  | sticky    |
+------------+------------------------------+-----------+

TOC rail visible only at >1280px viewport width.
```

### Header (Top Bar)

- Height: `48px`, fixed position
- Background: `var(--bg-primary)` with `1px` bottom border in `var(--brass-dark)` at 30% opacity
- Left: Clawq wordmark (Uncial Antiqua, `1.125rem`) + small gear icon (20px, brass)
- Center: Breadcrumb trail (Inter 400, `--text-secondary`)
- Right: Search input (pill-shaped), theme toggle (sun/moon), GitHub icon link
- No shadow — the brass rule line is the only separation

### Sidebar — "The Mathematical Index"

- Background: `var(--bg-secondary)`
- Right border: `1px solid var(--brass-dark)` at 25% opacity
- **Gear-rack edge:** Along the right border, a subtle repeating SVG pattern of tiny gear teeth every `8px`, rendered in `var(--brass-dark)` at 30% opacity, `4px` wide. The "engineering precision" motif.

Navigation structured like a formal mathematical text:

```
CLAWQ                         (Uncial Antiqua wordmark)

PREFACE
  Overview
  Quick Start

I. FOUNDATIONS
  1.1  Architecture
  1.2  Coq Core
  1.3  Extraction Pipeline

II. FORMAL PROPERTIES
  2.1  Verification Status
  2.2  Proof Roadmap

III. RUNTIME
  3.1  Configuration
  3.2  CLI Reference
  3.3  Channels

IV. OPERATIONS
  4.1  Development Guide

APPENDICES
  A. Config Schema
```

- Section numbers in Inter 500, `--text-tertiary`
- Group labels in EB Garamond small-caps, `--text-secondary`
- Active item: left `2px solid var(--brass-primary)`, text in `--brass-light`, subtle brass background wash
- Hover: text shifts to `--brass-primary`
- Section dividers: thin `1px` rule with tiny gear icon (10px) at left end
- Bottom: version badge pill (`v0.x.y`, Inter 500, `--text-tertiary`)

### Footer

- Centered, minimal
- Ornamental rule: `--- diamond ---` pattern
- "Clawq -- The Formal AI Assistant"
- "OCaml 5.1 . Coq 8.19 . MIT License"
- Final gold QED square: `var(--qed-gold)`

### Content Area

- Max prose width: `720px`, centered in available space
- Padding: `3rem` top, `2.5rem` sides
- Paragraph max-width: `68ch`
- Subtle blueprint grid background: very faint engineering-paper lines at 3-5% opacity in brass

---

## 4. Visual Elements

### Blueprint Grid Background

Content area carries a very subtle grid:

```css
background-image:
  linear-gradient(var(--brass-primary-05) 1px, transparent 1px),
  linear-gradient(90deg, var(--brass-primary-05) 1px, transparent 1px);
background-size: 40px 40px;
```

Where `--brass-primary-05` is brass at ~5% opacity. Gives a faint engineering-paper feel without interfering with readability.

### Animated Background Gears

Two decorative gears, `position: fixed`, behind all content at `z-index: 0`:

1. **Large gear** (280px) — bottom-right, partially off-screen. Clockwise, `120s` per revolution. Stroke-only SVG, `--brass-primary` at `opacity: 0.03` (dark) / `0.05` (light).
2. **Small gear** (140px) — meshed with large gear, counter-clockwise, `60s`. Same styling.

Both: `pointer-events: none`, `will-change: transform` for GPU acceleration.

**Reduced motion:** `@media (prefers-reduced-motion: reduce)` stops animation, shows gears static.

**Mobile:** Hidden below `768px`.

### Ornamental Horizontal Rules

Instead of plain `<hr>`, centered decorative rule:

```
--- [diamond] ---
```

Where `[diamond]` is in `--qed-gold`, dashes in `--border`. Total width: `120px`, centered.

### QED Markers

Gold filled square at the end of completed/verified sections:

```css
.section-complete::after {
  content: "\25A0";  /* filled square */
  display: block;
  text-align: right;
  color: var(--qed-gold);
  font-size: 0.7rem;
  margin-top: 1.5rem;
  opacity: 0.7;
}
```

### Corner Brackets

Key content cards (hero cards, FV phase cards, callout boxes) receive L-shaped brass lines at each corner:

```css
.bracketed::before {
  content: '';
  position: absolute;
  top: 8px; left: 8px;
  width: 16px; height: 16px;
  border-top: 1px solid var(--brass-primary);
  border-left: 1px solid var(--brass-primary);
}
/* Repeat for all four corners */
```

### Theorem / Definition / Lemma Callout Boxes

Styled like a mathematical textbook:

```
+-- Theorem 3.1 (Memory Isolation) -------------------------+
|                                                            |
|  For all sessions s1, s2 where s1 != s2, the memory       |
|  regions M(s1) and M(s2) are disjoint.                    |
|                                                            |
|  Status: Machine-checked (Coq)                             |
+------------------------------------------------------------+
```

- Left border: `3px solid var(--coq-teal)` for theorems, `var(--brass-primary)` for definitions
- Background: teal-tinted at ~5% opacity for theorems
- Label: EB Garamond small-caps, `--fs-sm`
- Status badge: small pill with `--coq-teal` background, white text

### Verified Callout (Unique)

A special callout type for statements backed by Coq proofs:
- Coq teal left border
- Small Coq rooster or gear+check icon
- Used to mark formally verified claims in documentation

---

## 5. Code Blocks

### Syntax Theme — "Watchmaker's Loupe"

| Token | Dark | Light |
|-------|------|-------|
| Background | `#141320` | `#EDEADF` |
| Default text | `#D4CEBC` | `#2A2520` |
| Comments | `#5C584E` italic | `#8A8275` italic |
| Strings | `#C9A84C` (brass) | `#7A6321` |
| Numbers | `#E2C97E` (light brass) | `#5C4A18` |
| Keywords | `#8FA4B8` (blued steel) | `#3D5A73` |
| Functions | `#D4A76A` (warm amber) | `#8B5E2F` |
| Types | `#7DAA6E` (verdigris) | `#4A6B3A` |
| Operators | `#9C978A` | `#6B5E52` |
| Coq Theorem/Proof/Qed | `#C9A84C` bold | `#7A6321` bold |

### Code Block Chrome

- Top bar: `--bg-tertiary`, with language label (Inter 500, `--fs-xs`, `--text-tertiary`, uppercase) left-aligned, copy button (clipboard icon, `--text-tertiary`, hover `--brass-primary`) right-aligned
- Left border: `2px solid var(--brass-dark)` at 30% opacity
- Border-radius: `6px`
- Max height before scroll: `500px`
- Scrollbar: thin, brass-tinted thumb

### Coq Code Blocks (Special Treatment)

- Background tinted toward teal: dark `#1A2826` / light `#EAF0ED`
- Left border: `3px solid var(--coq-teal)`
- Signals "this is formally verified code"

### Inline Code

- Background: `var(--bg-tertiary)`
- Border: `1px solid var(--border)`
- Border-radius: `3px`
- Padding: `2px 6px`
- Font: JetBrains Mono, `0.85em`

---

## 6. Tables

### Standard Tables (Academic "Booktabs" Style)

Horizontal rules only, no vertical borders:

- Top rule: `2px solid var(--text-primary)`
- Header bottom rule: `1px solid var(--text-primary)`
- Bottom rule: `2px solid var(--text-primary)`
- No row borders by default (clean academic look)
- Header text: EB Garamond small-caps, `--fs-xs`, `letter-spacing: 0.06em`
- Body text: Source Serif 4, `--fs-sm`

### FV Status Table (Enhanced)

The formal verification table uses the academic base with additional features:

Status icons:
- Verified: `var(--coq-teal)` checkmark
- In Progress: `var(--brass-primary)` spinning gear (CSS animation)
- Planned: `var(--text-tertiary)` open circle
- Specified: `var(--qed-gold)` diamond

Verified rows get a `2px` left border in `--coq-teal`.

---

## 7. Formal Verification Page — The Showcase

This is the crown jewel of the site.

### Page Header

Title: "II. FORMAL PROPERTIES" section label + "Verification Status" in Playfair Display 900.

Subtitle: "A machine-checked account of Clawq's proven properties." in Source Serif 4 italic, `--text-secondary`.

Metadata line: "Last extraction: 2026-03-06 . Coq 8.19 . 69 theorems" in `--fs-xs`, `--text-tertiary`.

### Verification Seal

Centered below header. A `140px` circular SVG:

- Outer ring: gear-toothed edge (24 teeth), `--brass-primary` stroke
- Inner ring: smooth circle, `--brass-dark` stroke
- Center: "FV" monogram in Playfair Display 900, `--brass-primary`
- Radiating lines between rings (compass rose / clock chapter ring style)
- Below seal: "Coq-Verified Core" in EB Garamond small-caps, `--text-secondary`
- Subtle rotation on outer gear: `60s` per revolution (respects reduced-motion)

### Mechanical Counter — Theorem Count

Row of flip-counter digits (odometer/split-flap display):

```
+---+---+---+
| 0 | 6 | 9 |  THEOREMS PROVEN
+---+---+---+
```

- Each digit: `48px x 64px` card, `--bg-tertiary`, `1px solid var(--brass-dark)` border
- Digit: JetBrains Mono 700, `2rem`, `--text-primary`
- Horizontal bisect line (split-flap effect)
- On page load: digits animate from 0 to actual count with flip animation, `150ms` stagger
- Adjacent counters for: EXTRACTED, COQ LOC

### Verification Scorecard

Progress summary:

```
+---------------------------------------------------+
|                                                     |
|              69 / ~120                              |
|          PROPERTIES VERIFIED                        |
|                                                     |
|    [=============================............] 58%  |
|                                                     |
|    [teal] 69 verified  [gold] 0 in progress         |
|    [gray] ~51 planned                               |
|                                                     |
+---------------------------------------------------+

(corner brackets on this card)
```

- Progress bar: `6px` height, rounded
  - Verified: `--coq-teal`
  - In progress: `--brass-primary`
  - Planned: `--bg-secondary`

### Phase Status — Progress Rings

Grid of circular SVG progress rings, one per phase (F1-F12):

- Ring diameter: `64px`
- Track: `--bg-tertiary`, `2px` stroke
- Fill arc: `--brass-primary` (in progress) or `--coq-teal` (complete), `3px` stroke
- Percentage centered inside in Inter 600
- 100% rings get subtle brass glow
- Below each ring: phase label (e.g., "F1: Config") in `--fs-xs`
- Status label: "VERIFIED" / "IN PROGRESS" / "PLANNED"

### Module Breakdown (Collapsible)

Each Coq module expands to show individual theorems:

```
=== Module: ConfigProofs.v ===========================
Status: 15/15 verified                              [QED]

  Theorem 1 (valid_port_bounds).
    For all p, valid_port p = true -> 1 <= p <= 65535.
    Proof. -- verified, ConfigProofs.v line 42        [check]

  Theorem 2 (default_config_valid).
    validate_config default_config = true.
    Proof. -- verified, ConfigProofs.v line 58        [check]

  ...
                                                     [QED]
```

- Module header: Playfair Display semibold, with completion fraction
- Each theorem: indented, formal statement in Source Serif 4 italic
- Proof reference: JetBrains Mono, `--text-tertiary`, linked to Coq source
- Checkmark: `--coq-teal`
- QED gold square at section end

### Trust Boundary Diagram

CSS-rendered diagram:

```
+--- Machine-Checked (Coq) -------------------------+
|                                                     |
|   Coq Core  ->  Extraction  ->  clawq_core.ml      |
|                                                     |
+--- Trust Boundary (dashed, red) ------------------+
|                                                     |
|   Runtime (OCaml)  .  I/O  .  Network               |
|                                                     |
+-----------------------------------------------------+
```

- Top zone: teal-tinted background
- Bottom zone: neutral
- Boundary: dashed line, `--error-red`

---

## 8. Landing Page

### Hero Section

Full viewport height, centered content.

**Background:** `var(--bg-primary)` with blueprint grid at slightly higher opacity.

**Layout:**

```
              +-----------------------------+
              |                             |
              |     [Cover Art Image]       |
              |     clawq-cover.webp        |
              |     max-width: 480px        |
              |     brass picture frame     |
              |     border treatment        |
              |                             |
              +-----------------------------+

                      C L A W Q
                The Formal AI Assistant

     A formally verified personal AI assistant runtime --
     Coq-proven core properties extracted to OCaml,
     with impeccable manners and machine-checked correctness.

              [ Get Started ]  [ Verified Properties ]

                      --- [diamond] ---

        69 theorems verified . 18 commands . 226 tests
```

**Cover art treatment:**
- Max-width `480px`, centered
- `3px solid var(--brass-dark)` outer border + `1px solid var(--brass-primary)` inner border offset `4px`
- Subtle warm glow: `box-shadow: 0 0 40px var(--brass-glow)`

**Title:** "CLAWQ" in Playfair Display 900, `--fs-hero`, letter-spacing `0.2em`

**CTA buttons:**
- Primary: `--brass-primary` bg, `--bg-primary` text, Inter 600
- Secondary: transparent, `1px solid var(--brass-dark)`, `--brass-primary` text

**Stats line:** Numbers in JetBrains Mono 500 for monospaced feel, separated by brass middots

### Below the Fold — Feature Cards

Three cards with corner brackets:

| Card | Icon | Title | Description |
|------|------|-------|-------------|
| 1 | Gear+check SVG | Formally Verified | Core properties proven in Coq with 69 theorems |
| 2 | Arrow/extract SVG | Extracted to OCaml | Direct Coq extraction pipeline to native code |
| 3 | Shield SVG | Secure Runtime | Landlock sandbox, rate limiting, encrypted secrets |

Cards animate in on scroll intersection (`translateY(20px)` + `opacity: 0` to final position).

---

## 9. Dark/Light Mode

### Implementation

- CSS custom properties throughout, toggled by `[data-theme="dark"]` / `[data-theme="light"]` on `<html>`
- Default: dark
- Respects `prefers-color-scheme` on first visit
- Manual toggle stored in `localStorage`
- Inline `<script>` in `<head>` sets theme before first paint (no flash)
- Transition: `background-color 200ms ease, color 200ms ease` on key elements

### Light Mode Adjustments

- Blueprint grid opacity increases slightly (`0.06`)
- Background gears increase to `opacity: 0.05`
- Drop caps: brass darkens to `--brass-primary` (light variant)
- Code blocks: light warm background, dark text (full palette swap)
- Cover art: slight `saturate(0.9)` to keep muted

---

## 10. Mobile Responsiveness

### Breakpoints

| Name | Width | Changes |
|------|-------|---------|
| Desktop | >= 1280px | Full layout: sidebar + content + TOC rail |
| Standard | 1024-1279px | Sidebar + content, no TOC rail |
| Tablet | 768-1023px | Sidebar collapses to overlay |
| Mobile | < 768px | Single column, hamburger nav |

### Mobile (< 768px)

- **Header:** Hamburger menu (brass-colored lines) replaces sidebar. Logo centered. Theme toggle + search right.
- **Sidebar:** Full-height overlay from left, `--bg-secondary` at 0.97 opacity, backdrop blur. Same styling, larger touch targets (48px min height).
- **Content:** Full width, `16px` horizontal padding
- **Code blocks:** Full bleed (negative margins), horizontal scroll, font drops to `13px`
- **Tables:** Horizontally scrollable wrapper with brass fade gradient indicating scroll
- **Background gears:** Hidden (`display: none`)
- **Blueprint grid:** Remains but at minimal opacity
- **FV page:** Stat cards stack vertically, progress rings shrink to `48px`, counter digits to `36px x 48px`, filter pills scroll horizontally
- **Drop caps:** Reduced to `2.5x` size
- **All touch targets:** 44px minimum

---

## 11. Page Structure

| Page | Route | Content Source |
|------|-------|---------------|
| Landing | `/` | New (hero + features + stats) |
| Quick Start | `/quickstart` | Adapted from `old-docs/QUICKSTART.md` |
| Architecture | `/architecture` | Adapted from `old-docs/ARCHITECTURE.md` |
| CLI Reference | `/cli-reference` | From README CLI section |
| Configuration | `/configuration` | Config wizard + JSON schema + set/get/show |
| Channels | `/channels` | Channel overview (15+ channels) |
| Formal Verification | `/formal-verification` | New (renders from `_data/formal_verification.yml`) |
| Development | `/development` | Build, test, extract, contribute |

### Excluded from Site

- `old-docs/PLAN_P2_*.md` — internal planning docs
- `old-docs/FORMALIZATION_PLAN.md` — referenced/linked but not a site page (internal detail)
- `old-docs/MINIMIZATION_STRATS.md` — internal
- `old-docs/OPTIMIZATION_MAINTENANCE.md` — internal

---

## 12. Data-Driven FV Table

### Source: `docs/src/data/formal_verification.yml`

Single source of truth for verification status. Updated via `make update-fv`.

### Schema (per entry)

```yaml
- phase: F1
  module: ConfigProofs.v
  title: Configuration Validation
  status: verified        # verified | in_progress | planned | specified
  theorems: 15
  extracted: false
  security_roi: high
  difficulty: medium
  coq_file: coq/theories/Clawq/ConfigProofs.v
  key_properties:
    - Weight sum invariants
    - Port/temperature range validation
    - Secure-by-default security config
```

### Update Script

`make update-fv` runs a script that:
1. Counts `Theorem`/`Lemma` declarations per `.v` file
2. Checks extraction status from `Extract.v`
3. Outputs/updates the YAML data file

---

## 13. Astro Project Structure

```
docs/
  astro.config.mjs
  package.json
  tsconfig.json
  CNAME                          (clawq.org)
  public/
    clawq-cover.webp
    badges/formal-verification.svg
    CNAME
  src/
    layouts/
      DocsLayout.astro           Main three-column layout
      LandingLayout.astro        Landing page layout
    components/
      Header.astro
      Sidebar.astro
      TOCRail.astro
      Footer.astro
      CodeBlock.astro            Custom code block wrapper
      Callout.astro              Note/tip/warning/verified variants
      TheoremBox.astro           Theorem/definition/lemma callouts
      VerificationSeal.astro     Gear-toothed FV seal SVG
      MechanicalCounter.astro    Flip-counter digits (client island)
      ProgressRing.astro         SVG progress ring
      VerificationTable.astro    FV status table
      CornerBrackets.astro       Brass corner bracket wrapper
      GearBackground.astro       Animated background gears
      ThemeToggle.astro          Dark/light toggle (client island)
      StatCard.astro
    styles/
      global.css                 Custom properties, base styles
      typography.css             Font imports, headings, drop caps
      code-theme.css             "Watchmaker's Loupe" syntax theme
    content/
      docs/
        index.mdx                Landing page
        quickstart.mdx
        architecture.mdx
        cli-reference.mdx
        configuration.mdx
        channels.mdx
        formal-verification.mdx
        development.mdx
    data/
      formal_verification.yml    FV status data
    assets/
      gear-sprite.svg            Shared gear icon sprite
```

---

## 14. Prev/Next Navigation

Bottom of each page:

```
<- 1.2 Coq Core            2.1 Verification Status ->
```

Playfair Display, `--brass-primary`, with top ornamental rule separator.

---

## 15. Search

- Pagefind (Astro-compatible static search) or Astro's built-in search
- Search input in header: pill-shaped, `--bg-tertiary`, `--brass-dark` border
- Results overlay: `--bg-secondary`, brass-accented result items

---

## 16. Performance Notes

- Fonts: loaded via `@fontsource` npm packages (not CDN). `font-display: swap`.
- Gear SVGs: inlined in shared `<symbol>` sprite, referenced via `<use>`.
- Blueprint grid: CSS-only (no image).
- Noise/paper texture: not used (clean CSS backgrounds only).
- View transitions: native CSS View Transitions API via Astro.
- Islands: only `ThemeToggle`, `MechanicalCounter`, FV table filter need client JS.
- All static content is zero-JS.

---

## Design Principles Summary

1. **"Formal" is the pun and the brand.** Mathematical formalism meets Victorian formality. Every design decision should reinforce one or both meanings.
2. **Readability first.** The Victorian aesthetic enhances but never hinders reading technical documentation.
3. **Brass is the accent, not the base.** Used for emphasis, interaction, and verification status. Never dominant.
4. **Gears are controlled.** Exactly 2 background gears + sidebar edge + section dividers + FV seal. No gear clutter.
5. **The FV page is the showcase.** It should impress visitors and demonstrate the project's unique value proposition.
6. **Academic structure.** Navigation reads like a mathematical text. Theorems are presented formally. QED squares close verified sections.
7. **Dark by default.** The clockwork study is the primary experience. Light mode is an equally polished alternative.
