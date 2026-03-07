import { buildGearPath, type GearProfileTuning } from "../path.ts";
import type { RotationDirection, SolvedGear } from "../model.ts";
import type { DraftGear, DraftMeshEdge } from "./types.ts";
import { HERO_GEAR_CIRCULAR_PITCH } from "./shared.ts";
import { solveNeighborPhaseTurn } from "./shared.ts";

const ROOT_PERIOD_SEC = 132;
const ROOT_DIRECTION: RotationDirection = "cw";
const TRUE_MESH_RENDER_EPSILON = 0.06;

export const DEBUG_TOOTH_TUNING: GearProfileTuning = {
  valleyWidth: 0.085,
  tipWidth: 0.02,
  toothLength: 1.08,
  roundness: 1.05,
};

function normalizeTurn(value: number): number {
  return ((value % 1) + 1) % 1;
}

function draftPhaseTurn(gear: DraftGear): number | undefined {
  const value = (gear as Record<string, unknown>)["phaseTurn"];
  return typeof value === "number" ? value : undefined;
}

export function buildSolvedDebugGears(draft: DraftGear[], edges: DraftMeshEdge[]): SolvedGear[] {
  const byId = new Map(draft.map((gear) => [gear.id, gear]));
  const root = draft[0];
  if (!root) return [];

  const rootOmega = ((2 * Math.PI) / ROOT_PERIOD_SEC) * (ROOT_DIRECTION === "cw" ? 1 : -1);
  const gearModule = HERO_GEAR_CIRCULAR_PITCH / Math.PI;

  const solved = draft.map((gear) => {
    const sign = gear.parity === root.parity ? 1 : -1;
    const angularVelocity = rootOmega * sign * (root.teeth / gear.teeth);
    const periodSec = (2 * Math.PI) / Math.abs(angularVelocity);
    const toothAmplitude = gearModule * 0.82;
    const outerRadius = gear.pitchRadius + toothAmplitude;
    const rootRadius = Math.max(gear.pitchRadius - toothAmplitude, gear.pitchRadius * 0.7);
    const rotationDirection: RotationDirection = angularVelocity >= 0 ? "cw" : "ccw";

    return {
      id: gear.id,
      center: gear.center,
      teeth: gear.teeth,
      module: gearModule,
      circularPitch: HERO_GEAR_CIRCULAR_PITCH,
      pitchRadius: gear.pitchRadius,
      outerRadius,
      rootRadius,
      holeRadius: Math.max(gear.pitchRadius * 0.16, gearModule * 1.8),
      innerRingRadius: gear.pitchRadius * 0.48,
      angularVelocity,
      rotationDirection,
      periodSec,
      phaseDeg: 0,
    } satisfies SolvedGear;
  });

  const adjacency = new Map<string, Array<{ other: string; solve: (currentTurn: number) => number }>>();
  for (const edge of edges) {
    const a = byId.get(edge.a);
    const b = byId.get(edge.b);
    if (!a || !b) continue;
    const alpha = Math.atan2(b.center.y - a.center.y, b.center.x - a.center.x);
    if (!adjacency.has(a.id)) adjacency.set(a.id, []);
    if (!adjacency.has(b.id)) adjacency.set(b.id, []);
    adjacency.get(a.id)?.push({
      other: b.id,
      solve: (currentTurn) =>
        solveNeighborPhaseTurn({
          currentTeeth: a.teeth,
          neighborTeeth: b.teeth,
          currentTurn,
          contactAngleCurrentToNeighbor: alpha,
        }),
    });
    adjacency.get(b.id)?.push({
      other: a.id,
      solve: (currentTurn) =>
        solveNeighborPhaseTurn({
          currentTeeth: b.teeth,
          neighborTeeth: a.teeth,
          currentTurn,
          contactAngleCurrentToNeighbor: alpha + Math.PI,
        }),
    });
  }

  const assignedTurns = new Map<string, number>();
  for (const gear of draft) {
    const phase = draftPhaseTurn(gear);
    if (typeof phase === "number") assignedTurns.set(gear.id, normalizeTurn(phase));
  }

  for (const gear of draft) {
    if (assignedTurns.has(gear.id)) continue;
    assignedTurns.set(gear.id, draftPhaseTurn(gear) ?? 0);
    const queue = [gear.id];
    while (queue.length > 0) {
      const current = queue.shift();
      if (!current) continue;
      const currentTurn = assignedTurns.get(current) ?? 0;
      for (const neighbor of adjacency.get(current) ?? []) {
        if (assignedTurns.has(neighbor.other)) continue;
        assignedTurns.set(neighbor.other, neighbor.solve(currentTurn));
        queue.push(neighbor.other);
      }
    }
  }

  for (const gear of solved) {
    gear.phaseDeg = (assignedTurns.get(gear.id) ?? 0) * 360;
  }

  return solved;
}

export function buildTrueMeshEdges(draft: DraftGear[], edges: DraftMeshEdge[]) {
  const byId = new Map(draft.map((gear) => [gear.id, gear]));
  return edges
    .map((edge) => {
      const a = byId.get(edge.a);
      const b = byId.get(edge.b);
      if (!a || !b) return null;
      const centerDistance = Math.hypot(a.center.x - b.center.x, a.center.y - b.center.y);
      const expected = a.pitchRadius + b.pitchRadius;
      if (Math.abs(centerDistance - expected) > TRUE_MESH_RENDER_EPSILON) return null;
      return { aId: a.id, bId: b.id, x1: a.center.x, y1: a.center.y, x2: b.center.x, y2: b.center.y };
    })
    .filter((edge): edge is { aId: string; bId: string; x1: number; y1: number; x2: number; y2: number } => Boolean(edge));
}

export function backdropBounds(gears: Array<{ center: { x: number; y: number }; outerRadius: number }>, padding = 48) {
  const minX = Math.min(...gears.map((gear) => gear.center.x - gear.outerRadius)) - padding;
  const maxX = Math.max(...gears.map((gear) => gear.center.x + gear.outerRadius)) + padding;
  const minY = Math.min(...gears.map((gear) => gear.center.y - gear.outerRadius)) - padding;
  const maxY = Math.max(...gears.map((gear) => gear.center.y + gear.outerRadius)) + padding;
  return {
    minX,
    minY,
    maxX,
    maxY,
    width: maxX - minX,
    height: maxY - minY,
  };
}

export function componentStats(gears: DraftGear[], edges: DraftMeshEdge[]) {
  const adjacency = new Map<string, string[]>();
  for (const gear of gears) adjacency.set(gear.id, []);
  for (const edge of edges) {
    adjacency.get(edge.a)?.push(edge.b);
    adjacency.get(edge.b)?.push(edge.a);
  }

  const visited = new Set<string>();
  const sizes: number[] = [];
  for (const gear of gears) {
    if (visited.has(gear.id)) continue;
    let size = 0;
    const queue = [gear.id];
    visited.add(gear.id);
    while (queue.length > 0) {
      const current = queue.shift();
      if (!current) continue;
      size += 1;
      for (const neighbor of adjacency.get(current) ?? []) {
        if (visited.has(neighbor)) continue;
        visited.add(neighbor);
        queue.push(neighbor);
      }
    }
    sizes.push(size);
  }

  sizes.sort((a, b) => b - a);
  return {
    componentCount: sizes.length,
    largestComponent: sizes[0] ?? 0,
    sizes,
  };
}

export function renderedDebugGears(draft: DraftGear[], edges: DraftMeshEdge[]) {
  const solved = buildSolvedDebugGears(draft, edges);
  return solved.map((gear) => ({ gear, path: buildGearPath(gear, DEBUG_TOOTH_TUNING) }));
}
