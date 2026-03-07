import { pitchRadiusFromTeeth } from "../solver.ts";
import type { BackdropGeneratorFn, DraftGear, DraftMeshEdge, Point } from "./types.ts";
import {
  HERO_GEAR_CIRCULAR_PITCH,
  evaluatePlacement,
  getTwoParentMeshedIntersections,
  outerRadiusFromTeeth,
  registerMeshContacts,
  solveNeighborPhaseTurn,
} from "./shared.ts";

type Patch = {
  id: string;
  teeth: number;
  center: Point;
  angle: number;
  radius: number;
  parity: 0 | 1;
};

type NodeKind = "a" | "b";

type BridgePlan = {
  gear: DraftGear;
  neighbors: DraftGear[];
  score: number;
};

const PATCH_TEETH = [12, 15, 18, 21, 24] as const;
const BRIDGE_TEETH = [12, 15, 18, 21, 24, 27] as const;
const SQRT3_OVER_2 = Math.sqrt(3) / 2;

function rotate(point: Point, angle: number): Point {
  const cos = Math.cos(angle);
  const sin = Math.sin(angle);
  return {
    x: point.x * cos - point.y * sin,
    y: point.x * sin + point.y * cos,
  };
}

function hexRadius(i: number, j: number): number {
  return Math.max(Math.abs(i), Math.abs(j), Math.abs(i + j));
}

function basisForPatch(patch: Patch) {
  const spacing = pitchRadiusFromTeeth(patch.teeth, HERO_GEAR_CIRCULAR_PITCH) * 2;
  return {
    b1: rotate({ x: spacing * 1.5, y: spacing * SQRT3_OVER_2 }, patch.angle),
    b2: rotate({ x: spacing * 1.5, y: -spacing * SQRT3_OVER_2 }, patch.angle),
    d: rotate({ x: spacing, y: 0 }, patch.angle),
  };
}

function edgeKey(a: string, b: string): string {
  return a < b ? `${a}|${b}` : `${b}|${a}`;
}

function nodeKey(patchId: string, kind: NodeKind, i: number, j: number): string {
  return `${patchId}:${kind}:${i}:${j}`;
}

function nodeCenter(patch: Patch, kind: NodeKind, i: number, j: number): Point {
  const { b1, b2, d } = basisForPatch(patch);
  const base = {
    x: patch.center.x + i * b1.x + j * b2.x,
    y: patch.center.y + i * b1.y + j * b2.y,
  };
  return kind === "a" ? base : { x: base.x + d.x, y: base.y + d.y };
}

function nodeNeighbors(kind: NodeKind, i: number, j: number): Array<{ kind: NodeKind; i: number; j: number }> {
  if (kind === "a") {
    return [
      { kind: "b", i, j },
      { kind: "b", i: i - 1, j },
      { kind: "b", i, j: j - 1 },
    ];
  }

  return [
    { kind: "a", i, j },
    { kind: "a", i: i + 1, j },
    { kind: "a", i, j: j + 1 },
  ];
}

function patchMask(patch: Patch, kind: NodeKind, i: number, j: number): boolean {
  const radius = hexRadius(i, j);
  if (radius > patch.radius) return false;
  if (radius <= patch.radius - 1) return true;

  const waveA = Math.sin(i * 0.81 + patch.center.x * 0.0027 + patch.angle * 2.7);
  const waveB = Math.cos(j * 1.03 - patch.center.y * 0.0032);
  const waveC = Math.sin((i + j) * 0.67 + (kind === "a" ? 0.35 : -0.55));
  return waveA + waveB + waveC > -0.95;
}

export const generateHexWebBackdrop: BackdropGeneratorFn = ({ seed, targetCount = 116 }) => {
  const random = (() => {
    let state = (seed ^ 0x8f42d) >>> 0;
    return () => {
      state += 0x6d2b79f5;
      let t = state;
      t = Math.imul(t ^ (t >>> 15), t | 1);
      t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
      return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
    };
  })();

  const gears: DraftGear[] = [];
  const edges: DraftMeshEdge[] = [];
  const edgeKeys = new Set<string>();
  const contactAnglesByGearId = new Map<string, number[]>();
  const patchByGearId = new Map<string, string>();

  function registerPlacement(gear: DraftGear, neighbors: DraftGear[], patchId: string): void {
    gears.push(gear);
    patchByGearId.set(gear.id, patchId);
    registerMeshContacts({ gear, neighbors, contactAnglesByGearId, edges, edgeKeys });
  }

  const patches: Patch[] = [];
  let anchorX = -210;
  let anchorY = 138;
  let anchorAngle = -0.18 + (random() - 0.5) * 0.1;
  for (let index = 0; index < 8; index += 1) {
    anchorX += 140 + random() * 68;
    anchorY += (random() - 0.5) * 58;
    anchorY = Math.max(102, Math.min(206, anchorY));
    anchorAngle += (random() - 0.5) * 0.22;
    anchorAngle = Math.max(-0.42, Math.min(0.42, anchorAngle));

    patches.push({
      id: `hex-anchor-${index}`,
      teeth: PATCH_TEETH[index % PATCH_TEETH.length],
      center: { x: anchorX, y: anchorY },
      angle: anchorAngle,
      radius: 3 + Math.floor(random() * 3),
      parity: (index % 2) as 0 | 1,
    });

    if (index > 0 && index < 7) {
      patches.push({
        id: `hex-side-${index}`,
        teeth: PATCH_TEETH[(index + 2) % PATCH_TEETH.length],
        center: {
          x: anchorX + (random() - 0.5) * 92,
          y: anchorY + (random() - 0.5) * 56,
        },
        angle: anchorAngle + (random() - 0.5) * 0.32,
        radius: 2 + Math.floor(random() * 3),
        parity: ((index + 1) % 2) as 0 | 1,
      });
    }
  }

  for (const patch of patches) {
    const plannedNodes: Array<{ kind: NodeKind; i: number; j: number; center: Point }> = [];
    for (let i = -patch.radius; i <= patch.radius; i += 1) {
      for (let j = -patch.radius; j <= patch.radius; j += 1) {
        for (const kind of ["a", "b"] as const) {
          if (!patchMask(patch, kind, i, j)) continue;
          plannedNodes.push({ kind, i, j, center: nodeCenter(patch, kind, i, j) });
        }
      }
    }

    plannedNodes.sort((left, right) => {
      const leftBias = hexRadius(left.i, left.j) * 10 + (left.kind === "a" ? 0 : 1);
      const rightBias = hexRadius(right.i, right.j) * 10 + (right.kind === "a" ? 0 : 1);
      if (leftBias !== rightBias) return leftBias - rightBias;
      return left.center.x - right.center.x;
    });

    const placed = new Map<string, DraftGear>();
    for (const node of plannedNodes) {
      if (gears.length >= targetCount) break;
      const localNeighbors = nodeNeighbors(node.kind, node.i, node.j)
        .map((neighbor) => placed.get(nodeKey(patch.id, neighbor.kind, neighbor.i, neighbor.j)))
        .filter((gear): gear is DraftGear => Boolean(gear));
      if (placed.size > 0 && localNeighbors.length === 0) continue;

      const pitchRadius = pitchRadiusFromTeeth(patch.teeth, HERO_GEAR_CIRCULAR_PITCH);
      const candidate: DraftGear = {
        id: `hero-g${gears.length}`,
        teeth: patch.teeth,
        pitchRadius,
        outerRadius: outerRadiusFromTeeth(patch.teeth),
        center: node.center,
        phaseTurn:
          localNeighbors[0] == null
            ? 0
            : solveNeighborPhaseTurn({
                currentTeeth: localNeighbors[0].teeth,
                neighborTeeth: patch.teeth,
                currentTurn: localNeighbors[0].phaseTurn ?? 0,
                contactAngleCurrentToNeighbor: Math.atan2(
                  node.center.y - localNeighbors[0].center.y,
                  node.center.x - localNeighbors[0].center.x
                ),
              }),
        parity: ((patch.parity + (node.kind === "a" ? 0 : 1)) % 2) as 0 | 1,
        parentId: localNeighbors[0]?.id,
        appearIndex: gears.length,
      };

      const verdict = evaluatePlacement(candidate, gears, contactAnglesByGearId, localNeighbors[0]?.id, true);
      if (!verdict.ok) continue;
      if (gears.length > 0 && verdict.neighbors.length === 0) continue;
      registerPlacement(candidate, verdict.neighbors, patch.id);
      placed.set(nodeKey(patch.id, node.kind, node.i, node.j), candidate);
    }
  }

  function degreeMap(): Map<string, number> {
    const degrees = new Map(gears.map((gear) => [gear.id, 0]));
    for (const edge of edges) {
      degrees.set(edge.a, (degrees.get(edge.a) ?? 0) + 1);
      degrees.set(edge.b, (degrees.get(edge.b) ?? 0) + 1);
    }
    return degrees;
  }

  function components(): DraftGear[][] {
    const byId = new Map(gears.map((gear) => [gear.id, gear]));
    const adjacency = new Map(gears.map((gear) => [gear.id, [] as string[]]));
    for (const edge of edges) {
      adjacency.get(edge.a)?.push(edge.b);
      adjacency.get(edge.b)?.push(edge.a);
    }

    const seen = new Set<string>();
    const result: DraftGear[][] = [];
    for (const gear of gears) {
      if (seen.has(gear.id)) continue;
      const component: DraftGear[] = [];
      const stack = [gear.id];
      seen.add(gear.id);
      while (stack.length > 0) {
        const current = stack.pop();
        if (!current) continue;
        const currentGear = byId.get(current);
        if (currentGear) component.push(currentGear);
        for (const next of adjacency.get(current) ?? []) {
          if (seen.has(next)) continue;
          seen.add(next);
          stack.push(next);
        }
      }
      result.push(component);
    }
    return result;
  }

  function bestBridge(candidatesA: DraftGear[], candidatesB: DraftGear[], mode: "connect" | "loop"): BridgePlan | null {
    const degrees = degreeMap();
    let best: BridgePlan | null = null;

    for (const a of candidatesA) {
      for (const b of candidatesB) {
        if (a.id === b.id || a.parity !== b.parity) continue;
        if (edgeKeys.has(edgeKey(a.id, b.id))) continue;

        const span = Math.hypot(a.center.x - b.center.x, a.center.y - b.center.y);
        if (span < 60 || span > 460) continue;

        for (const teeth of BRIDGE_TEETH) {
          const pitchRadius = pitchRadiusFromTeeth(teeth, HERO_GEAR_CIRCULAR_PITCH);
          for (const option of getTwoParentMeshedIntersections({ parentA: a, parentB: b, teeth })) {
            const candidate: DraftGear = {
              id: `hero-g${gears.length}`,
              teeth,
              pitchRadius,
              outerRadius: outerRadiusFromTeeth(teeth),
              center: option.center,
              phaseTurn: solveNeighborPhaseTurn({
                currentTeeth: a.teeth,
                neighborTeeth: teeth,
                currentTurn: a.phaseTurn ?? 0,
                contactAngleCurrentToNeighbor: option.contactAngleFromA,
              }),
              parity: (a.parity === 0 ? 1 : 0) as 0 | 1,
              parentId: a.id,
              appearIndex: gears.length,
            };

            const verdict = evaluatePlacement(candidate, gears, contactAnglesByGearId, undefined, true);
            if (!verdict.ok || verdict.neighbors.length < 2) continue;
            if (!verdict.neighbors.some((neighbor) => neighbor.id === a.id)) continue;
            if (!verdict.neighbors.some((neighbor) => neighbor.id === b.id)) continue;

            const extraNeighbors = Math.max(0, verdict.neighbors.length - 2);
            const leafBonus = ((degrees.get(a.id) ?? 0) <= 1 ? 40 : 0) + ((degrees.get(b.id) ?? 0) <= 1 ? 40 : 0);
            const varietyBonus = Math.min(50, Math.abs(a.teeth - b.teeth) * 4 + Math.abs(teeth - a.teeth) * 2);
            const modeBonus = mode === "connect" ? 380 : 180;
            const crossPatchBonus = patchByGearId.get(a.id) !== patchByGearId.get(b.id) ? 130 : 0;
            const score =
              modeBonus +
              verdict.neighbors.length * 120 +
              extraNeighbors * 180 +
              leafBonus +
              varietyBonus -
              span * 0.08 +
              crossPatchBonus +
              random() * 4;

            if (!best || score > best.score) {
              best = { gear: candidate, neighbors: verdict.neighbors, score };
            }
          }
        }
      }
    }

    return best;
  }

  let connectPass = 0;
  while (connectPass < 36) {
    connectPass += 1;
    const groups = components();
    if (groups.length <= 1) break;

    let best: BridgePlan | null = null;
    for (let i = 0; i < groups.length; i += 1) {
      for (let j = i + 1; j < groups.length; j += 1) {
        const candidate = bestBridge(groups[i], groups[j], "connect");
        if (candidate && (!best || candidate.score > best.score)) best = candidate;
      }
    }

    if (!best) break;
    registerPlacement(best.gear, best.neighbors, `connect-${connectPass}`);
  }

  let loopPass = 0;
  while (gears.length < targetCount && loopPass < 56) {
    loopPass += 1;
    const degrees = degreeMap();
    const sorted = gears
      .slice()
      .sort((left, right) => {
        const leftScore = (degrees.get(left.id) ?? 0) + left.center.y * 0.002;
        const rightScore = (degrees.get(right.id) ?? 0) + right.center.y * 0.002;
        return leftScore - rightScore;
      })
      .slice(0, Math.min(40, gears.length));

    const best = bestBridge(sorted, gears, "loop");
    if (!best) break;
    registerPlacement(best.gear, best.neighbors, `loop-${loopPass}`);
  }

  let densifyPass = 0;
  while (gears.length < targetCount && densifyPass < 32) {
    densifyPass += 1;
    const degrees = degreeMap();
    const sparse = gears.filter((gear) => (degrees.get(gear.id) ?? 0) <= 2);
    if (sparse.length < 2) break;

    const best = bestBridge(sparse, gears, "loop");
    if (!best) break;
    registerPlacement(best.gear, best.neighbors, `dense-${densifyPass}`);
  }

  return { gears, edges };
};
