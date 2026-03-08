# Gear Editor Plan

## Goal

Build a new debug authoring page for manual landing-page gear layout, starting with external spur gears and arbitrary closed loops, while designing the underlying saved format as a reusable generic clock-scene schema that can later grow to support planetary gears, ring gears, racks, shafts, carriers, and other clock mechanisms.

This editor is intended to become the manual composition tool for the homepage hero gear backdrop. It must let us place gears by hand, connect them into mesh graphs that may contain loops, validate the resulting mechanism, and export a layout that the landing page can load directly.

## Scope Decisions Already Made

### Confirmed product decisions

- The saved format should be reusable.
- We should design a generic clock-scene schema now rather than a gear-only schema.
- Milestone 1 should be limited to external spur gears and loops.
- Planetary gears are deferred to Milestone 3.

### M1 feature expectations

- Scroll to zoom
- Drag and drop gears
- Optional placement lock to grid
- Select two gears and attempt to auto-generate a third bridge gear
- If bridge generation fails, show a descriptive error message
- Support looped gear graphs, not just trees

## Why The Existing Main Solver Is Not The Right Base

The current main gear solver in `src/lib/gears/solver.ts` is built around a tree-shaped placement model:

- `GearSpec` supports at most one `placement.with` parent
- placement resolution walks unresolved child nodes until parent placements become known
- cycles would remain unresolved and eventually throw
- the existing helper `createMeshedPairScene()` is explicitly pair-oriented

This makes it unsuitable as the core authoring model for a manual editor whose defining requirement is closed-loop support.

## Why The Backdrop Graph Pipeline Is The Right Mechanical Base

The backdrop generation and debug code already works with arbitrary gear graphs:

- `DraftGear[]` + `DraftMeshEdge[]` represent a free mesh graph instead of a parent-only tree
- `buildSolvedDebugGears()` already propagates phase through arbitrary adjacency
- `buildTrueMeshEdges()` already filters to actual geometric meshes for display
- `evaluatePlacement()` already performs useful geometric legality checks
- backdrop generators already reason about multi-neighbor gears and phase consistency in looped graphs

This means the mechanical and rendering foundation for M1 already exists, even though the editor UI does not.

## Core Product Vision

We should think of the new page as a manual clock-layout authoring tool, not just a one-off gear debug page.

That means:

- the saved file format should represent an editable scene, not only a rendered hero subset
- the editor should preserve future-compatible structure even when only gears are currently supported
- the homepage hero backdrop should become a derived export target from the scene
- unsupported future parts should not force a schema rewrite later

## High-Level Architecture

There should be three distinct layers.

### 1. Generic scene model

This is the long-lived saved format. It should describe clock parts, relations, viewport/editor metadata, and future extensibility.

### 2. M1 gear mechanics layer

This is the implementation subset used for the first editor. It only supports external spur gears and mesh relations, but it operates on top of the generic scene model.

### 3. Export adapter layer

This derives a landing-page-compatible gear backdrop payload from the generic scene, validating that the current scene uses only the supported hero subset.

## Proposed Saved Format: `ClockScene`

The saved format should be a versioned JSON document, designed for stability and future migration.

### Top-level shape

```ts
type ClockScene = {
  version: number;
  name: string;
  description?: string;
  units?: "px";
  viewport: SceneViewport;
  heroFrame?: HeroFrame;
  parts: ClockPart[];
  relations: ClockRelation[];
  editorState?: EditorState;
  exportHints?: ExportHints;
};
```

### Viewport

```ts
type SceneViewport = {
  width: number;
  height: number;
  origin?: { x: number; y: number };
};
```

This should default to the same hero-space assumptions already used by the gear backdrop tooling where practical, but remain explicit in the saved scene.

### Hero frame

```ts
type HeroFrame = {
  x: number;
  y: number;
  width: number;
  height: number;
  padding?: number;
};
```

This defines the region intended for homepage export and preview.

### Editor state

```ts
type EditorState = {
  zoom?: number;
  pan?: { x: number; y: number };
  snapToGrid?: boolean;
  gridSize?: number;
  showGrid?: boolean;
  showPitchCircles?: boolean;
  showMeshLines?: boolean;
  showPhaseMarkers?: boolean;
};
```

This is editor-only convenience data. It should never be required by the export pipeline.

### Export hints

```ts
type ExportHints = {
  preferredRootId?: string;
  preserveManualPhase?: boolean;
  paletteHint?: string;
};
```

This allows hero export defaults without contaminating the scene's core mechanical meaning.

## Proposed Generic Part Model

### Base part

```ts
type BasePart = {
  id: string;
  kind: ClockPartKind;
  label?: string;
  position: { x: number; y: number };
  rotationDeg?: number;
  locked?: boolean;
  hidden?: boolean;
  tags?: string[];
  render?: PartRenderHints;
};
```

### M1-supported part: gear

```ts
type GearPart = BasePart & {
  kind: "gear";
  gearType?: "external-spur";
  teeth: number;
  circularPitch?: number;
  pitchRadius?: number;
  phaseTurn?: number;
  style?: {
    holeRadiusRatio?: number;
    innerRingRadiusRatio?: number;
  };
};
```

### Reserved future parts

These should be represented in the schema now, even if the editor cannot create or simulate them yet:

- `ring-gear`
- `rack`
- `shaft`
- `carrier`
- `anchor`
- `decorative-shape`

They do not need full M1 behavior, but the union should reserve them so future expansion does not feel bolted on.

## Proposed Generic Relation Model

### Base relation

```ts
type BaseRelation = {
  id: string;
  kind: ClockRelationKind;
  aId: string;
  bId: string;
  locked?: boolean;
};
```

### M1-supported relation: mesh

```ts
type MeshRelation = BaseRelation & {
  kind: "mesh";
  meshType?: "external";
};
```

### Reserved future relations

- `coaxial`
- `mounted-on-carrier`
- `fixed-phase`
- `distance-lock`
- `alignment`

These relation kinds matter for planetary and other clock mechanisms later.

## Compatibility Strategy

The editor should use `ClockScene` as the source of truth.

The existing homepage hero backdrop pipeline should remain gear-draft based for now.

Therefore we need an adapter:

- `ClockScene` -> M1 solved gear graph subset -> backdrop-compatible export payload

Eventually we should also support the reverse direction:

- existing hero/manual backdrop payload -> `ClockScene`

This will let old assets be loaded into the editor.

## M1 Mechanical Subset

M1 should enforce the following constraints:

- only `gear` parts are mechanically active
- only `mesh` relations are mechanically active
- all meshes are external spur gear meshes
- all gears lie in a single plane
- loops are allowed
- disconnected components are allowed in the editor, but export may warn or restrict based on intended use
- no ring gears
- no carriers
- no coaxial semantics beyond purely visual overlap rejection

## M1 User Experience Goals

The page should feel like a proper workbench, not a raw debug dump.

### Layout

- large central workspace for direct manipulation
- left or right inspector sidebar for selected part or relation
- top toolbar for creation, editing mode, and export/import actions
- bottom or inline diagnostics strip for layout validity, selected counts, and export warnings

### Primary interactions

- click to select gear
- shift-click to multi-select
- drag selected gear to move
- drag workspace to pan
- wheel to zoom
- keyboard delete to remove selection
- optional snap-to-grid
- connect or disconnect mesh relations from a mode or button action

### Visual overlays

- gear outlines
- pitch circles
- optional root/outer guides
- mesh lines for actual relations
- invalid overlaps highlighted in red
- selected gear emphasis
- ghost previews when dragging or attempting bridge insertion

## Editor Page Proposal

Create a new page, likely:

- `src/pages/debug-manual-gears.astro`

This page should reuse the tone and structure of the existing debug pages while becoming the main authoring surface for manual mechanical layout.

It should not attempt to render all homepage algorithms or generators. It is a dedicated editing tool.

## Proposed New Modules

### Scene schema and conversion

- `src/lib/gears/editor/scene_types.ts`
- `src/lib/gears/editor/scene_validation.ts`
- `src/lib/gears/editor/scene_serialization.ts`
- `src/lib/gears/editor/scene_compat.ts`

### M1 mechanical helpers

- `src/lib/gears/editor/gear_scene_projection.ts`
- `src/lib/gears/editor/gear_scene_solver.ts`
- `src/lib/gears/editor/gear_scene_diagnostics.ts`
- `src/lib/gears/editor/bridge_solver.ts`

### Export adapters

- `src/lib/gears/editor/export_hero_backdrop.ts`
- `src/lib/gears/editor/import_backdrop_draft.ts`

### Client-side editor state

- `src/scripts/debug-manual-gears.ts`

We should keep interaction-heavy code out of the Astro page file as much as possible.

## Reuse From Existing Code

### Mechanical and rendering helpers to reuse

- `buildGearPath()` from `src/lib/gears/path.ts`
- `getTunedRadii()` from `src/lib/gears/path.ts`
- `sampleGearOutlinePoints()` from `src/lib/gears/path.ts`
- `buildSolvedDebugGears()` from `src/lib/gears/backdrop/debug_spec.ts`
- `buildTrueMeshEdges()` from `src/lib/gears/backdrop/debug_spec.ts`
- `evaluatePlacement()` from `src/lib/gears/backdrop/shared.ts`
- `solveNeighborPhaseTurn()` from `src/lib/gears/backdrop/shared.ts`

### UI references to reuse conceptually

- workbench structure from `src/pages/debug-gears.astro`
- large network inspection approach from `src/components/SpecBackdropDebugView.astro`
- hero-compatible rendering feel from `src/components/HeroGearBackdrop.astro`

## Core Internal Flow For M1

### 1. Load scene

The editor loads a `ClockScene` document. This may come from:

- built-in starter preset
- imported JSON
- future landing-page manual layout asset

### 2. Project scene into M1 gear graph

The editor extracts only supported parts and relations:

- gear parts -> draft gears
- mesh relations -> draft mesh edges

Unsupported parts should be preserved in the in-memory scene but ignored by M1 mechanics with visible warnings.

### 3. Validate geometry and solve phase

The projected graph is validated for:

- legal mesh distances
- non-mesh clearance
- parity consistency
- loop phase consistency
- duplicate or malformed relations

### 4. Render workspace

The solved result is rendered with gear outlines and overlays.

### 5. User edits scene

All user interactions update `ClockScene`, not a temporary gear-only shadow format.

### 6. Export hero subset

When exporting to landing-page format, the editor derives a backdrop-compatible payload from the scene and reports any unsupported or invalid content.

## Detailed M1 Feature Plan

### A. Workspace controls

#### Scroll to zoom

- wheel changes zoom centered around cursor position
- clamp zoom to sensible bounds
- retain pan/zoom in editor state
- add quick reset zoom action

This is essential for usability and should not be deferred.

#### Pan

- drag empty canvas background to pan
- optional middle mouse drag support if convenient

#### Grid snap

- toggle snap to grid
- configurable grid size
- visible grid overlay toggle

Important note: grid snap should assist placement but never replace mechanical snap. If the user is placing a gear to mesh with another, the mechanical relationship should be the higher-value hint.

### B. Selection and editing

#### Selection model

- single selection for simple editing
- two-gear multi-selection should enable bridge insertion tools
- future extensibility for larger multi-select batches

#### Drag and drop

- drag selected gear directly in workspace
- show live validity feedback while moving
- if snap-to-grid is active, snap candidate center before final placement validation
- support holding a modifier to temporarily bypass snap

#### Add gear

- toolbar button creates a new gear at viewport center or click position
- default teeth count should use current library conventions, probably in the low-to-mid range

#### Delete gear

- removes gear and all attached relations
- should warn in diagnostics but not require confirmation for debug usage

### C. Relation editing

#### Create mesh relation

- either from a connect mode or selection-based action
- relation should only be committed when geometrically valid
- otherwise the editor should explain why not

#### Remove mesh relation

- selectable mesh line or inspector action

### D. Loop support

Closed loops are a hard requirement.

The solver for editor validation should:

- allow arbitrary graph topology
- assign or propagate phase turns through adjacency
- verify that every edge constraint is satisfied
- report the exact conflicting loop when possible

This should borrow heavily from the phase consistency logic already present in backdrop code.

### E. Auto-bridge between two gears

This is one of the most distinctive M1 tools.

#### Expected behavior

- user selects exactly two gears
- editor attempts to insert one new external spur gear between them
- if successful, it creates the gear and two mesh relations
- if not, it explains why in plain English

#### Search strategy

- iterate candidate tooth counts in an allowed range
- derive candidate pitch radius for each
- test whether a center exists that meshes with both selected gears
- reject any candidate that collides with other gears or violates contact-angle compatibility
- reject any candidate that causes phase inconsistency
- choose best candidate using a simple score such as minimal collision risk, reasonable size, and visual fit

#### Failure categories

- no candidate tooth count matches required geometry
- candidate collides with existing gears
- candidate violates contact angle alignment rules
- candidate creates unsatisfied loop phase

Error messages should be specific and actionable.

### F. Diagnostics

The editor should maintain a structured diagnostics panel instead of just a single validity boolean.

#### Diagnostic categories

- overlap errors
- invalid mesh distance
- relation references missing part
- duplicate relation
- parity conflict
- phase inconsistency
- unsupported part ignored for hero export
- disconnected graph warnings
- export clipping or out-of-frame warnings

#### Diagnostic UX

- show severity: error, warning, info
- clicking a diagnostic should highlight involved gears or relations
- the status strip should summarize counts

## Export Plan

### Export targets for M1

#### 1. Native scene JSON

This is the authoritative saved format.

#### 2. Hero backdrop gear draft JSON

Derived subset that can be loaded by the landing-page hero backdrop path.

### M1 export requirements

- all mechanically active parts in export must be external spur gears
- all active relations in export must be external meshes
- all exported gears must have resolved center, teeth, pitch radius, parity, and phase information as required by the existing gear-draft path

### Export warnings

- unsupported part kinds present in scene
- unsupported relation kinds present in scene
- hidden or locked elements excluded from export if such policy is chosen
- geometry outside hero frame

### Recommended future import/export symmetry

We should eventually support:

- `ClockScene` save/load
- hero draft import into `ClockScene`
- hero draft export from `ClockScene`

This will prevent editor-isolated assets.

## Homepage Integration Strategy

The landing page currently uses `HeroGearBackdrop.astro` with generated algorithm drafts.

The manual authoring path should not disturb the current algorithm system immediately.

Recommended path:

1. Add a way to load a manual draft payload into the existing hero rendering stack.
2. Keep the current algorithm flow untouched for procedural layouts.
3. Introduce a manual/preset source mode when ready.

That keeps the new editor useful without forcing an immediate rewrite of homepage backdrop selection.

## Proposed M1 Deliverables

### Deliverable 1: scene schema package

- generic `ClockScene` types
- validation helpers
- serialization helpers
- sample scene fixture

### Deliverable 2: gear projection and diagnostics

- project `ClockScene` into gear graph subset
- validate graph
- compute solved debug representation
- produce structured diagnostics

### Deliverable 3: editor page and client state

- new manual gears debug page
- zoom/pan/select/drag
- grid snap
- inspector
- diagnostics strip

### Deliverable 4: relation tools

- connect/disconnect mesh relations
- visual relation overlays
- selection-based editing

### Deliverable 5: bridge insertion

- two-gear selection bridge search
- success insertion path
- descriptive failure messages

### Deliverable 6: import/export

- native `ClockScene` JSON save/load
- hero backdrop draft export
- future-ready import hooks

## Recommended Task Breakdown

### Phase A: schema and adapter groundwork

1. Define `ClockScene` types and versioning strategy.
2. Define M1 supported kind/relation subset rules.
3. Implement scene validation.
4. Implement conversion between scene and gear graph subset.
5. Implement hero export adapter.

### Phase B: mechanics and diagnostics

1. Implement graph projection from scene to draft gears and edges.
2. Reuse existing phase propagation and true mesh helpers.
3. Add structured diagnostics builder.
4. Add helper APIs for move validation and edge editing.
5. Add bridge solver helper.

### Phase C: page shell and interaction layer

1. Create `debug-manual-gears.astro` shell.
2. Add toolbar, workspace, inspector, diagnostics.
3. Add client script for zoom, pan, selection, drag.
4. Add live rerender loop.
5. Add relation editing tools.

### Phase D: persistence and export

1. Add native scene JSON import/export.
2. Add hero draft export.
3. Add copy/download UX.
4. Add built-in starter scenes.

### Phase E: polish

1. Improve error messages.
2. Improve snapping visuals.
3. Add keyboard shortcuts.
4. Add hero preview frame overlay.

## Concrete M1 UX Suggestions

### Toolbar actions

- Add Gear
- Delete
- Connect
- Disconnect
- Bridge Selected
- Toggle Grid Snap
- Toggle Grid
- Reset View
- Import Scene
- Export Scene
- Export Hero Draft

### Inspector sections

- Selected item summary
- Gear properties: teeth, pitch radius, phase, lock state
- Relation properties if a mesh edge is selected
- Validation messages for current selection

### Status strip

- total gears
- total mesh relations
- components count
- errors count
- warnings count
- exportable yes/no

## Sample Validation Rules For M1

### Structural

- part ids must be unique
- relation ids must be unique
- relation endpoints must exist
- duplicate mesh relations must be rejected
- self-relations must be rejected

### Mechanical

- external mesh gears must have opposite parity
- relation center distances must match pitch radii sum within tolerance
- non-related gears must satisfy clearance thresholds
- contact angles for multiple neighbors must remain compatible with tooth spacing
- all loop phase equations must be satisfied

### Export

- only supported part kinds included
- only supported relation kinds included
- hero frame clipping warnings emitted

## Testing Strategy

### Unit tests

- schema validation
- scene-to-gear projection
- export adapter correctness
- loop phase consistency checks
- bridge solver success and failure cases

### Interaction/manual verification

- drag a gear and observe diagnostics update
- create a valid loop and confirm no phase error
- create an impossible loop and confirm descriptive error
- select two gears and bridge successfully
- select two impossible gears and confirm descriptive bridge failure
- zoom and pan behavior at min/max scales
- grid snap toggle behavior
- export and reload round trip for native scene JSON

### Visual verification

- exported hero draft renders consistently in the homepage preview path
- editor overlays remain legible on large and small layouts

## Risks And Mitigations

### Risk: interaction script becomes too monolithic

Mitigation:

- keep geometry and validation logic in library modules
- keep the page script focused on state wiring and DOM interaction

### Risk: saved format drifts into gear-only assumptions

Mitigation:

- define generic part and relation unions now
- keep M1 restrictions in validation and export rules, not in the schema shape itself

### Risk: bridge solver becomes overly complex too early

Mitigation:

- scope it to one inserted external spur gear only
- no multi-gear bridge chains in M1

### Risk: planetary expansion becomes awkward later

Mitigation:

- reserve `ring-gear`, `carrier`, and `coaxial` concepts in the saved format now
- avoid encoding every mechanic as plain mesh edges

### Risk: homepage integration causes performance regressions

Mitigation:

- keep the editor page isolated from homepage generation
- keep manual layout loading as a targeted source mode
- avoid the dev-only anti-pattern of rendering every algorithm on `/`

## Milestone Plan

### Milestone 1: external spur gear editor with loops

- generic scene schema defined
- gear-only M1 subset implemented
- manual workspace page built
- drag/drop gears
- zoom/pan
- grid snap
- closed-loop validation
- bridge insertion
- native scene save/load
- hero draft export

### Milestone 2: authoring quality improvements

- improved snapping and previews
- better diagnostics navigation
- more robust import/export polish
- hero preview workflow
- starter templates and presets

### Milestone 3: planetary gear support

- ring gears
- carriers
- coaxial constraints
- planetary-specific solving rules
- planetary-aware export warnings or partial export policies

### Milestone 4: broader clock-mechanism support

- racks
- shafts
- decorative/mechanical secondary parts
- additional constraint kinds as needed

## Recommended Immediate Next Steps

1. Define the `ClockScene` schema and versioned type module.
2. Implement scene validation and M1 subset projection.
3. Implement hero draft export adapter.
4. Build the new debug page shell and client state.
5. Wire existing gear rendering/math helpers into the editor.
6. Add bridge solver and diagnostics.

## Final Recommendation

The right long-term move is to treat this as a clock-scene editor with a gear-focused first milestone, not a one-off gear debugger. The existing backdrop graph code already gives us the crucial loop-capable mechanics that the pair/tree solver lacks. If we keep `ClockScene` as the saved source of truth and make the homepage backdrop a derived export target, we get an immediately useful manual layout workflow without sacrificing future support for planetary gears and richer clock structures.
