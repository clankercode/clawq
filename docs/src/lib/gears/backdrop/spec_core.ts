import { pitchRadiusFromTeeth } from "../solver.ts";
import type { BackdropGeneratorOptions, BackdropGeneratorResult, DraftGear, DraftMeshEdge, Point } from "./types.ts";
import { debugFastSpecBackdropAttempts, generateFastSpecBackdrop } from "./spec_fast.ts";
import {
  HERO_GEAR_CIRCULAR_PITCH,
  createGenerationContext,
  dist,
  getTwoParentMeshedIntersections,
  getLegalContactSlots,
  isContactAngleCompatible,
  isPhaseTurnConsistentWithNeighbors,
  meshEdgeKey,
  normalizeTurn,
  outerRadiusFromTeeth,
  registerMeshContacts,
  solveNeighborPhaseTurn,
} from "./shared.ts";

type SpecAlgorithmMode = "topology-first" | "constraint-solver";

type SpecBuildProfile = {
  mode: SpecAlgorithmMode;
  targetCount: number;
  maxAttempts: number;
  componentCountMin: number;
  componentCountMax: number;
  componentSizeMin: number;
  componentSizeMax: number;
  placementAngleSamples: number;
  placementAttempts: number;
  bridgeAttempts: number;
  bridgeTeethSamples: number;
  multiStarts: number;
  solverIterations: number;
  solverJitter: number;
  compactnessPull: number;
  overlapPush: number;
  rowCount: number;
  rootTeethMin: number;
  rootTeethMax: number;
  preferSmallNeighbors: number;
  preferLargeNeighbors: number;
  preferredRatioMin: number;
  preferredRatioMax: number;
  branchSpreadWeight: number;
  compactClusterWeight: number;
  bridgeSpanPreference: number;
  bridgeLoopPreference: number;
};

type NodePlan = {
  key: number;
  parentKey?: number;
  parity: 0 | 1;
  teeth: number;
  depth: number;
};

type BuiltComponent = {
  id: string;
  gears: DraftGear[];
  edges: DraftMeshEdge[];
  score: number;
};

type Bounds = {
  minX: number;
  minY: number;
  maxX: number;
  maxY: number;
};

const TAU = Math.PI * 2;
const HERO_X_MIN = -180;
const HERO_X_MAX = 1780;
const HERO_Y_MIN = -40;
const HERO_Y_MAX = 430;
const HERO_GEAR_MODULE = HERO_GEAR_CIRCULAR_PITCH / Math.PI;
const SPEC_MAX_TEETH = 75;
const MAX_DEGREE = 3;
const MESH_MATCH_EPSILON = 0.08;
const BRIDGE_SPAN_MIN = 150;
const BRIDGE_SPAN_MAX = 460;

export const SPEC_MIN_TEETH = 17;
export const SPEC_MIN_CLEARANCE = HERO_GEAR_MODULE * 0.3;

function collisionRadius(gear: Pick<DraftGear, "outerRadius">): number {
  return gear.outerRadius + SPEC_MIN_CLEARANCE;
}

function buildDraftGear(options: {
  id: string;
  teeth: number;
  center: Point;
  parity: 0 | 1;
  parentId?: string;
  appearIndex?: number;
  phaseTurn?: number;
}): DraftGear {
  const { id, teeth, center, parity, parentId, appearIndex = 0, phaseTurn } = options;
  return {
    id,
    teeth,
    pitchRadius: pitchRadiusFromTeeth(teeth, HERO_GEAR_CIRCULAR_PITCH),
    outerRadius: outerRadiusFromTeeth(teeth),
    center,
    phaseTurn,
    parity,
    parentId,
    appearIndex,
  };
}

function rotatePoint(point: Point, angleRad: number): Point {
  const cos = Math.cos(angleRad);
  const sin = Math.sin(angleRad);
  return {
    x: point.x * cos - point.y * sin,
    y: point.x * sin + point.y * cos,
  };
}

function translatePoint(point: Point, offset: Point): Point {
  return { x: point.x + offset.x, y: point.y + offset.y };
}

function boundsOfGears(gears: DraftGear[]): Bounds {
  let minX = Infinity;
  let minY = Infinity;
  let maxX = -Infinity;
  let maxY = -Infinity;

  for (const gear of gears) {
    minX = Math.min(minX, gear.center.x - collisionRadius(gear));
    minY = Math.min(minY, gear.center.y - collisionRadius(gear));
    maxX = Math.max(maxX, gear.center.x + collisionRadius(gear));
    maxY = Math.max(maxY, gear.center.y + collisionRadius(gear));
  }

  return { minX, minY, maxX, maxY };
}

function boundsWidth(bounds: Bounds): number {
  return bounds.maxX - bounds.minX;
}

function boundsHeight(bounds: Bounds): number {
  return bounds.maxY - bounds.minY;
}

function translateComponent(component: BuiltComponent, offset: Point): BuiltComponent {
  return {
    ...component,
    gears: component.gears.map((gear) => ({
      ...gear,
      center: translatePoint(gear.center, offset),
    })),
  };
}

function rotateComponent(component: BuiltComponent, angleRad: number): BuiltComponent {
  return {
    ...component,
    gears: component.gears.map((gear) => ({
      ...gear,
      center: rotatePoint(gear.center, angleRad),
    })),
  };
}

function clamp(value: number, min: number, max: number): number {
  return Math.min(max, Math.max(min, value));
}

function randInt(random: () => number, min: number, max: number): number {
  return Math.floor(random() * (max - min + 1)) + min;
}

function sampleTeeth(random: () => number): number {
  const roll = random();
  if (roll < 0.5) return randInt(random, SPEC_MIN_TEETH, 30);
  if (roll < 0.8) return randInt(random, 31, 55);
  return randInt(random, 56, SPEC_MAX_TEETH);
}

function chooseNeighborTeeth(random: () => number, parentTeeth: number, population: NodePlan[], cfg: SpecBuildProfile): number {
  let best = clamp(parentTeeth + (random() < 0.5 ? -8 : 8), SPEC_MIN_TEETH, SPEC_MAX_TEETH);
  let bestScore = -Infinity;

  for (let attempt = 0; attempt < 36; attempt += 1) {
    const candidate = sampleTeeth(random);
    const ratio = candidate / parentTeeth;
    if (ratio < cfg.preferredRatioMin || ratio > cfg.preferredRatioMax) continue;

    const diff = Math.abs(candidate - parentTeeth);
    const duplicatePenalty = population.some((node) => node.teeth === candidate) ? 0.45 : 0;
    const ratioCenter = (cfg.preferredRatioMin + cfg.preferredRatioMax) * 0.5;
    const score =
      Math.min(diff, 28) * 0.15 +
      (diff >= 6 ? 1.6 : -0.6) +
      Math.abs(ratio - ratioCenter) * 1.2 -
      duplicatePenalty +
      (candidate <= 30 ? cfg.preferSmallNeighbors : 0) +
      (candidate >= 34 ? cfg.preferLargeNeighbors : 0) +
      random() * 0.45;

    if (score > bestScore) {
      best = candidate;
      bestScore = score;
    }
  }

  return best;
}

function weightedPick<T>(random: () => number, items: T[], weightOf: (item: T) => number): T {
  const weights = items.map((item) => Math.max(0.001, weightOf(item)));
  const total = weights.reduce((sum, weight) => sum + weight, 0);
  let cursor = random() * total;

  for (let index = 0; index < items.length; index += 1) {
    cursor -= weights[index];
    if (cursor <= 0) return items[index];
  }

  return items[items.length - 1];
}

function shuffleInPlace<T>(random: () => number, values: T[]): T[] {
  for (let index = values.length - 1; index > 0; index -= 1) {
    const swapIndex = Math.floor(random() * (index + 1));
    [values[index], values[swapIndex]] = [values[swapIndex], values[index]];
  }
  return values;
}

function sampleComponentBudgets(random: () => number, cfg: SpecBuildProfile): number[] {
  const budgets: number[] = [];
  const minComponents = Math.max(cfg.componentCountMin, Math.ceil(cfg.targetCount / cfg.componentSizeMax));
  const maxComponents = Math.min(cfg.componentCountMax, Math.floor(cfg.targetCount / cfg.componentSizeMin));
  const desiredComponents = clamp(
    Math.round(cfg.targetCount / ((cfg.componentSizeMin + cfg.componentSizeMax) * 0.5)) + randInt(random, -1, 1),
    minComponents,
    Math.max(minComponents, maxComponents),
  );
  let remaining = cfg.targetCount;

  for (let index = 0; index < desiredComponents; index += 1) {
    const componentsLeft = desiredComponents - index - 1;
    const minThis = Math.max(cfg.componentSizeMin, remaining - componentsLeft * cfg.componentSizeMax);
    const minForRest = componentsLeft * cfg.componentSizeMin;
    const maxThis = Math.min(cfg.componentSizeMax, remaining - minForRest);
    if (maxThis < minThis) break;

    const favoredMin = index === 0 ? Math.max(minThis, Math.floor(maxThis * (cfg.mode === "constraint-solver" ? 0.8 : 0.68))) : minThis;
    const favoredMax = index === 0 ? maxThis : Math.max(minThis, Math.floor((minThis + maxThis) * 0.78));
    const size =
      index === desiredComponents - 1
        ? remaining
        : randInt(random, favoredMin, Math.max(favoredMin, favoredMax));
    budgets.push(size);
    remaining -= size;
  }

  if (budgets.length === 0) {
    budgets.push(cfg.componentSizeMin);
  }

  return budgets;
}

function buildNodePlan(random: () => number, nodeCount: number, cfg: SpecBuildProfile): NodePlan[] {
  const root: NodePlan = {
    key: 0,
    parity: 0,
    teeth: randInt(random, cfg.rootTeethMin, cfg.rootTeethMax),
    depth: 0,
  };
  const nodes = [root];
  const degrees = new Map<number, number>([[0, 0]]);

  const pattern = random();
  const preferredParents: number[] = [];
  if (cfg.mode === "topology-first" && pattern < 0.45) {
    for (let key = 1; key < nodeCount; key += 1) preferredParents.push(key - 1);
  } else if (cfg.mode === "constraint-solver" && pattern < 0.55) {
    for (let key = 1; key < nodeCount; key += 1) {
      if (key <= 2) preferredParents.push(0);
      else if (key % 3 === 0) preferredParents.push(Math.max(0, key - 2));
      else preferredParents.push(Math.max(0, key - 1));
    }
  } else if (pattern < 0.66) {
    for (let key = 1; key < nodeCount; key += 1) {
      if (key <= 2) preferredParents.push(0);
      else preferredParents.push(key % 2 === 0 ? 1 : 2);
    }
  } else {
    for (let key = 1; key < nodeCount; key += 1) {
      if (key <= 3) preferredParents.push(0);
      else preferredParents.push(1 + ((key - 4) % Math.min(3, key - 1)));
    }
  }

  for (let key = 1; key < nodeCount; key += 1) {
    const preferredParent = preferredParents[key - 1] ?? 0;
    const preferredNode = nodes.find((node) => node.key === preferredParent);
    const candidates = nodes.filter((node) => (degrees.get(node.key) ?? 0) < MAX_DEGREE);
    const parent = preferredNode && (degrees.get(preferredNode.key) ?? 0) < MAX_DEGREE
      ? preferredNode
      : weightedPick(random, candidates, (node) => {
          const degree = degrees.get(node.key) ?? 0;
          const degreeBias = degree === 0 ? 2.4 : degree === 1 ? 1.55 : 0.85;
          const depthBias = node.depth <= 1 ? 1.35 : node.depth <= 3 ? 1.05 : 0.92;
          return degreeBias * depthBias;
        });
    const child: NodePlan = {
      key,
      parentKey: parent.key,
      parity: parent.parity === 0 ? 1 : 0,
      teeth: chooseNeighborTeeth(random, parent.teeth, nodes, cfg),
      depth: parent.depth + 1,
    };

    nodes.push(child);
    degrees.set(parent.key, (degrees.get(parent.key) ?? 0) + 1);
    degrees.set(child.key, 1);
  }

  return nodes;
}

function adjacencyOf(edges: DraftMeshEdge[]): Map<string, Set<string>> {
  const adjacency = new Map<string, Set<string>>();
  for (const edge of edges) {
    if (!adjacency.has(edge.a)) adjacency.set(edge.a, new Set());
    if (!adjacency.has(edge.b)) adjacency.set(edge.b, new Set());
    adjacency.get(edge.a)?.add(edge.b);
    adjacency.get(edge.b)?.add(edge.a);
  }
  return adjacency;
}

function addMeshEdge(edges: DraftMeshEdge[], edgeKeys: Set<string>, a: string, b: string): void {
  const key = meshEdgeKey(a, b);
  if (edgeKeys.has(key)) return;
  edgeKeys.add(key);
  edges.push({ a, b });
}

function provisionalNeighborAngles(candidate: DraftGear, neighbors: DraftGear[]): number[] {
  return neighbors.map((neighbor) => Math.atan2(neighbor.center.y - candidate.center.y, neighbor.center.x - candidate.center.x));
}

function collectMeshNeighbors(options: {
  candidate: DraftGear;
  gears: DraftGear[];
  contactAnglesByGearId: Map<string, number[]>;
  allowedMeshIds?: Set<string>;
}): { ok: boolean; neighbors: DraftGear[] } {
  const { candidate, gears, contactAnglesByGearId, allowedMeshIds } = options;
  const neighbors: DraftGear[] = [];

  for (const other of gears) {
    const d = dist(candidate.center, other.center);
    const expected = candidate.pitchRadius + other.pitchRadius;
    const residual = Math.abs(d - expected);

    if (residual <= MESH_MATCH_EPSILON) {
      if (candidate.parity === other.parity) return { ok: false, neighbors: [] };
      if (allowedMeshIds && !allowedMeshIds.has(other.id)) return { ok: false, neighbors: [] };

      const candidateContact = Math.atan2(other.center.y - candidate.center.y, other.center.x - candidate.center.x);
      if (!isContactAngleCompatible(candidate, provisionalNeighborAngles(candidate, neighbors), candidateContact)) {
        return { ok: false, neighbors: [] };
      }

      const otherContact = Math.atan2(candidate.center.y - other.center.y, candidate.center.x - other.center.x);
      if (!isContactAngleCompatible(other, contactAnglesByGearId.get(other.id) ?? [], otherContact)) {
        return { ok: false, neighbors: [] };
      }

      neighbors.push(other);
      continue;
    }

    if (d < expected - MESH_MATCH_EPSILON) return { ok: false, neighbors: [] };
    if (d < collisionRadius(candidate) + collisionRadius(other)) return { ok: false, neighbors: [] };
  }

  if (allowedMeshIds) {
    for (const id of allowedMeshIds) {
      if (!neighbors.some((neighbor) => neighbor.id === id)) return { ok: false, neighbors: [] };
    }
  }

  if (!isPhaseTurnConsistentWithNeighbors(candidate, neighbors)) {
    return { ok: false, neighbors: [] };
  }

  return { ok: true, neighbors };
}

function degreeMap(edges: DraftMeshEdge[]): Map<string, number> {
  const degrees = new Map<string, number>();
  for (const edge of edges) {
    degrees.set(edge.a, (degrees.get(edge.a) ?? 0) + 1);
    degrees.set(edge.b, (degrees.get(edge.b) ?? 0) + 1);
  }
  return degrees;
}

function candidateAngleScore(options: {
  candidate: DraftGear;
  angleRad: number;
  center: Point;
  allGears: DraftGear[];
  componentGears: DraftGear[];
  existingParentAngles: number[];
  cfg: SpecBuildProfile;
  random: () => number;
}): number {
  const { candidate, angleRad, center, allGears, componentGears, existingParentAngles, cfg, random } = options;
  const radialDistance = Math.hypot(center.x, center.y);
  const minGap = existingParentAngles.length === 0
    ? Math.PI
    : Math.min(
        ...existingParentAngles.map((angle) => {
          const delta = Math.atan2(Math.sin(angleRad - angle), Math.cos(angleRad - angle));
      return Math.abs(delta);
        }),
      );
  const antiGridPenalty = Math.abs(Math.cos(angleRad * 2));
  const componentBounds = boundsOfGears([...componentGears, candidate]);
  const width = boundsWidth(componentBounds);
  const height = boundsHeight(componentBounds);
  const spreadBonus = Math.abs(center.x) * 0.015 + Math.abs(center.y) * 0.007;
  const overlapPressure = allGears.reduce((sum, gear) => sum + 1 / Math.max(32, dist(center, gear.center)), 0);

  return (
    minGap * 12 -
    radialDistance * cfg.compactClusterWeight -
    antiGridPenalty * 3.5 -
    overlapPressure * 22 +
    spreadBonus * cfg.branchSpreadWeight -
    Math.abs(width - height) * 0.006 +
    random() * 0.6
  );
}

function tryConstructiveComponent(options: {
  componentId: number;
  nodes: NodePlan[];
  allGears: DraftGear[];
  cfg: SpecBuildProfile;
  random: () => number;
}): BuiltComponent | null {
  const { componentId, nodes, allGears, cfg, random } = options;
  const gears: DraftGear[] = [];
  const edges: DraftMeshEdge[] = [];
  const edgeKeys = new Set<string>();
  const contactAnglesByGearId = new Map<string, number[]>();
  const gearByKey = new Map<number, DraftGear>();

  const rootNode = nodes[0];
  const root = buildDraftGear({
    id: `spec-c${componentId}-g${rootNode.key}`,
    teeth: rootNode.teeth,
    center: { x: 0, y: 0 },
    parity: 0,
    phaseTurn: normalizeTurn(random() * 0.5),
  });
  gears.push(root);
  gearByKey.set(rootNode.key, root);
  contactAnglesByGearId.set(root.id, []);

  const orderedNodes = nodes.slice(1).sort((left, right) => {
    if (left.depth !== right.depth) return left.depth - right.depth;
    return random() - 0.5;
  });

  for (const node of orderedNodes) {
    const parent = gearByKey.get(node.parentKey ?? -1);
    if (!parent) return null;

    const parentAngles = (contactAnglesByGearId.get(parent.id) ?? []).slice();
    const parentParentAngle = parent.parentId
      ? (() => {
          const parentParent = gears.find((gear) => gear.id === parent.parentId);
          return parentParent ? Math.atan2(parent.center.y - parentParent.center.y, parent.center.x - parentParent.center.x) : undefined;
        })()
      : undefined;
    const preferredAngles = parentParentAngle == null
      ? [random() * TAU, (random() - 0.5) * 0.6]
      : [parentParentAngle + Math.PI, parentParentAngle + Math.PI * 0.7, parentParentAngle - Math.PI * 0.7];
    const legalSlots = getLegalContactSlots({
      gear: parent,
      slotCount: parent.teeth,
      contactAnglesByGearId,
    })
      .map((slot) => ({
        angleRad: (slot * TAU) / parent.teeth,
        desirability: Math.min(
          ...preferredAngles.map((preferred) => Math.abs(Math.atan2(Math.sin(((slot * TAU) / parent.teeth) - preferred), Math.cos(((slot * TAU) / parent.teeth) - preferred)))),
        ),
      }))
      .sort((left, right) => left.desirability - right.desirability || random() - 0.5)
      .slice(0, Math.max(cfg.placementAngleSamples, 10));

    let best: { gear: DraftGear; neighbors: DraftGear[]; score: number } | null = null;

    for (const { angleRad } of legalSlots) {
      const center = {
        x: parent.center.x + Math.cos(angleRad) * (parent.pitchRadius + pitchRadiusFromTeeth(node.teeth, HERO_GEAR_CIRCULAR_PITCH)),
        y: parent.center.y + Math.sin(angleRad) * (parent.pitchRadius + pitchRadiusFromTeeth(node.teeth, HERO_GEAR_CIRCULAR_PITCH)),
      };
      const candidate = buildDraftGear({
        id: `spec-c${componentId}-g${node.key}`,
        teeth: node.teeth,
        center,
        parity: node.parity,
        parentId: parent.id,
        phaseTurn: solveNeighborPhaseTurn({
          currentTeeth: parent.teeth,
          neighborTeeth: node.teeth,
          currentTurn: parent.phaseTurn ?? 0,
          contactAngleCurrentToNeighbor: angleRad,
        }),
      });
      const verdict = collectMeshNeighbors({
        candidate,
        gears: [...allGears, ...gears],
        contactAnglesByGearId,
        allowedMeshIds: new Set([parent.id]),
      });
      if (!verdict.ok) continue;

      const score = candidateAngleScore({
        candidate,
        angleRad,
        center,
        allGears,
        componentGears: gears,
        existingParentAngles: parentAngles,
        cfg,
        random,
      });

      if (!best || score > best.score) best = { gear: candidate, neighbors: verdict.neighbors, score };
    }

    if (!best) return null;

    gears.push(best.gear);
    gearByKey.set(node.key, best.gear);
    registerMeshContacts({ gear: best.gear, neighbors: best.neighbors, contactAnglesByGearId, edges, edgeKeys });
  }

  return {
    id: `spec-c${componentId}`,
    gears,
    edges,
    score: 0,
  };
}

function sampleBridgeTeeth(random: () => number, limit: number): number[] {
  const candidates = new Set<number>();
  while (candidates.size < limit) {
    candidates.add(sampleTeeth(random));
  }
  return [...candidates];
}

function addBridgeGears(options: {
  component: BuiltComponent;
  allGears: DraftGear[];
  cfg: SpecBuildProfile;
  random: () => number;
}): BuiltComponent {
  const { component, allGears, cfg, random } = options;
  const gears = component.gears.slice();
  const edges = component.edges.slice();
  const edgeKeys = new Set(edges.map((edge) => meshEdgeKey(edge.a, edge.b)));
  const contactAnglesByGearId = new Map<string, number[]>();
  for (const gear of gears) contactAnglesByGearId.set(gear.id, []);
  for (const edge of edges) {
    const a = gears.find((gear) => gear.id === edge.a);
    const b = gears.find((gear) => gear.id === edge.b);
    if (a && b) registerMeshContacts({ gear: a, neighbors: [b], contactAnglesByGearId });
  }

  const tries = cfg.bridgeAttempts;
  for (let attempt = 0; attempt < tries; attempt += 1) {
    const pairs: Array<{ a: DraftGear; b: DraftGear; span: number }> = [];
    for (let i = 0; i < gears.length; i += 1) {
      for (let j = i + 1; j < gears.length; j += 1) {
        const a = gears[i];
        const b = gears[j];
        if (a.parity !== b.parity) continue;
        if (edgeKeys.has(meshEdgeKey(a.id, b.id))) continue;
        const span = dist(a.center, b.center);
        if (span < BRIDGE_SPAN_MIN || span > BRIDGE_SPAN_MAX) continue;
        pairs.push({ a, b, span });
      }
    }
    if (pairs.length === 0) break;

    shuffleInPlace(random, pairs);
    let best: { gear: DraftGear; neighbors: DraftGear[]; score: number } | null = null;

    for (const pair of pairs.slice(0, 42)) {
      for (const teeth of sampleBridgeTeeth(random, cfg.bridgeTeethSamples)) {
        for (const option of getTwoParentMeshedIntersections({ parentA: pair.a, parentB: pair.b, teeth })) {
          const candidate = buildDraftGear({
            id: `${component.id}-bridge-${gears.length}`,
            teeth,
            center: option.center,
            parity: pair.a.parity === 0 ? 1 : 0,
            parentId: pair.a.id,
            phaseTurn: solveNeighborPhaseTurn({
              currentTeeth: pair.a.teeth,
              neighborTeeth: teeth,
              currentTurn: pair.a.phaseTurn ?? 0,
              contactAngleCurrentToNeighbor: option.contactAngleFromA,
            }),
          });

          const verdict = collectMeshNeighbors({
            candidate,
            gears: [...allGears, ...gears],
            contactAnglesByGearId,
            allowedMeshIds: new Set([pair.a.id, pair.b.id]),
          });
          if (!verdict.ok) continue;

          const score =
            verdict.neighbors.length * (60 + cfg.bridgeLoopPreference * 18) +
            Math.min(pair.span, 320) * cfg.bridgeSpanPreference +
            Math.abs(pair.a.teeth - pair.b.teeth) * 1.2 +
            (teeth <= 30 ? 8 : 0) +
            random() * 2;
          if (!best || score > best.score) best = { gear: candidate, neighbors: verdict.neighbors, score };
        }
      }
    }

    if (!best) break;
    gears.push(best.gear);
    registerMeshContacts({ gear: best.gear, neighbors: best.neighbors, contactAnglesByGearId, edges, edgeKeys });
  }

  return { ...component, gears, edges };
}

function solverEnergy(component: BuiltComponent, cfg: SpecBuildProfile): number {
  const adjacency = adjacencyOf(component.edges);
  let energy = 0;

  for (const edge of component.edges) {
    const a = component.gears.find((gear) => gear.id === edge.a);
    const b = component.gears.find((gear) => gear.id === edge.b);
    if (!a || !b) continue;
    const desired = a.pitchRadius + b.pitchRadius;
    const residual = dist(a.center, b.center) - desired;
    energy += residual * residual * 220;
  }

  for (let i = 0; i < component.gears.length; i += 1) {
    for (let j = i + 1; j < component.gears.length; j += 1) {
      const a = component.gears[i];
      const b = component.gears[j];
      if (adjacency.get(a.id)?.has(b.id)) continue;
      const minDistance = collisionRadius(a) + collisionRadius(b);
      const actual = dist(a.center, b.center);
      if (actual < minDistance) {
        const penetration = minDistance - actual;
        energy += penetration * penetration * 160;
      }
    }
  }

  const bounds = boundsOfGears(component.gears);
  const centroid = component.gears.reduce((sum, gear) => ({ x: sum.x + gear.center.x, y: sum.y + gear.center.y }), { x: 0, y: 0 });
  centroid.x /= Math.max(1, component.gears.length);
  centroid.y /= Math.max(1, component.gears.length);
  energy += component.gears.reduce((sum, gear) => sum + dist(gear.center, centroid) * cfg.compactClusterWeight * 6.5, 0);
  energy += boundsWidth(bounds) * boundsHeight(bounds) * 0.0025;
  return energy;
}

function solveComponentLayout(options: {
  component: BuiltComponent;
  cfg: SpecBuildProfile;
  random: () => number;
}): BuiltComponent {
  const { component, cfg, random } = options;
  const rootId = component.gears[0]?.id;
  if (!rootId) return component;

  const adjacency = adjacencyOf(component.edges);
  let best = component;
  let bestEnergy = solverEnergy(component, cfg);

  for (let start = 0; start < cfg.multiStarts; start += 1) {
    const gears = component.gears.map((gear, index) => {
      if (gear.id === rootId) return { ...gear, center: { x: 0, y: 0 } };
      const jitter = start === 0 ? 0 : gear.pitchRadius * cfg.solverJitter;
      return {
        ...gear,
        center: {
          x: gear.center.x + (random() - 0.5) * jitter,
          y: gear.center.y + (random() - 0.5) * jitter + (index % 2 === 0 ? 1 : -1) * random() * jitter * 0.2,
        },
      };
    });
    const trial: BuiltComponent = { ...component, gears };

    for (let iter = 0; iter < cfg.solverIterations; iter += 1) {
      const byId = new Map(trial.gears.map((gear) => [gear.id, gear]));
      const edgeOrder = component.edges
        .map((edge) => {
          const a = byId.get(edge.a)!;
          const b = byId.get(edge.b)!;
          return {
            edge,
            residual: Math.abs(dist(a.center, b.center) - (a.pitchRadius + b.pitchRadius)),
          };
        })
        .sort((left, right) => right.residual - left.residual);

      for (const { edge } of edgeOrder) {
        const a = byId.get(edge.a);
        const b = byId.get(edge.b);
        if (!a || !b) continue;
        let dx = b.center.x - a.center.x;
        let dy = b.center.y - a.center.y;
        let length = Math.hypot(dx, dy);
        const desired = a.pitchRadius + b.pitchRadius;
        if (length < 1e-6) {
          dx = 1;
          dy = 0;
          length = 1;
        }
        const offset = ((length - desired) * 0.5) / length;
        if (a.id === rootId) {
          b.center = { x: b.center.x - dx * offset * 2, y: b.center.y - dy * offset * 2 };
        } else if (b.id === rootId) {
          a.center = { x: a.center.x + dx * offset * 2, y: a.center.y + dy * offset * 2 };
        } else {
          a.center = { x: a.center.x + dx * offset, y: a.center.y + dy * offset };
          b.center = { x: b.center.x - dx * offset, y: b.center.y - dy * offset };
        }
      }

      for (let i = 0; i < trial.gears.length; i += 1) {
        for (let j = i + 1; j < trial.gears.length; j += 1) {
          const a = trial.gears[i];
          const b = trial.gears[j];
          if (adjacency.get(a.id)?.has(b.id)) continue;

          let dx = b.center.x - a.center.x;
          let dy = b.center.y - a.center.y;
          let length = Math.hypot(dx, dy);
          const minimum = collisionRadius(a) + collisionRadius(b);
          if (length >= minimum) continue;
          if (length < 1e-6) {
            dx = 1;
            dy = 0;
            length = 1;
          }
          const push = ((minimum - length) * 0.5 * cfg.overlapPush) / length;
          if (a.id === rootId) {
            b.center = { x: b.center.x + dx * push * 2, y: b.center.y + dy * push * 2 };
          } else if (b.id === rootId) {
            a.center = { x: a.center.x - dx * push * 2, y: a.center.y - dy * push * 2 };
          } else {
            a.center = { x: a.center.x - dx * push, y: a.center.y - dy * push };
            b.center = { x: b.center.x + dx * push, y: b.center.y + dy * push };
          }
        }
      }

      const centroid = trial.gears
        .filter((gear) => gear.id !== rootId)
        .reduce((sum, gear) => ({ x: sum.x + gear.center.x, y: sum.y + gear.center.y }), { x: 0, y: 0 });
      const freeCount = Math.max(1, trial.gears.length - 1);
      centroid.x /= freeCount;
      centroid.y /= freeCount;

      for (const gear of trial.gears) {
        if (gear.id === rootId) {
          gear.center = { x: 0, y: 0 };
          continue;
        }
        gear.center = {
          x: gear.center.x - centroid.x * cfg.compactnessPull,
          y: gear.center.y - centroid.y * cfg.compactnessPull,
        };
      }
    }

    const rotated = chooseBestRotation(trial, cfg, random);
    const energy = solverEnergy(rotated, cfg);
    if (energy < bestEnergy) {
      best = rotated;
      bestEnergy = energy;
    }
  }

  return { ...best, score: bestEnergy };
}

function edgeAlignmentPenalty(component: BuiltComponent): number {
  let penalty = 0;
  for (const edge of component.edges) {
    const a = component.gears.find((gear) => gear.id === edge.a);
    const b = component.gears.find((gear) => gear.id === edge.b);
    if (!a || !b) continue;
    const theta = Math.atan2(b.center.y - a.center.y, b.center.x - a.center.x);
    penalty += Math.cos(theta * 2) ** 2;
  }
  return penalty;
}

function angularIrregularity(component: BuiltComponent): number {
  const adjacency = adjacencyOf(component.edges);
  let score = 0;

  for (const gear of component.gears) {
    const neighbors = [...(adjacency.get(gear.id) ?? [])]
      .map((id) => component.gears.find((candidate) => candidate.id === id))
      .filter((candidate): candidate is DraftGear => Boolean(candidate));
    if (neighbors.length < 2) continue;

    const angles = neighbors
      .map((neighbor) => Math.atan2(neighbor.center.y - gear.center.y, neighbor.center.x - gear.center.x))
      .sort((left, right) => left - right);
    const gaps = angles.map((angle, index) => {
      const next = angles[(index + 1) % angles.length] ?? angles[0] + TAU;
      return (index === angles.length - 1 ? next + TAU - angle : next - angle);
    });
    const average = gaps.reduce((sum, gap) => sum + gap, 0) / gaps.length;
    const variance = gaps.reduce((sum, gap) => sum + (gap - average) * (gap - average), 0) / gaps.length;
    score += Math.sqrt(variance);
  }

  return score;
}

function chooseBestRotation(component: BuiltComponent, cfg: SpecBuildProfile, random: () => number): BuiltComponent {
  let best = component;
  let bestScore = -Infinity;
  const availableHeight = HERO_Y_MAX - HERO_Y_MIN + 30;

  for (let sample = 0; sample < Math.max(8, cfg.multiStarts); sample += 1) {
    const angle = (sample / Math.max(8, cfg.multiStarts)) * TAU + (random() - 0.5) * 0.1;
    const rotated = rotateComponent(component, angle);
    const bounds = boundsOfGears(rotated.gears);
    const overflowPenalty = Math.max(0, boundsHeight(bounds) - availableHeight);
    const score =
      angularIrregularity(rotated) * 45 -
      edgeAlignmentPenalty(rotated) * 12 -
      boundsWidth(bounds) * boundsHeight(bounds) * 0.002 +
      boundsWidth(bounds) * (cfg.mode === "topology-first" ? 0.035 : -0.008) -
      boundsHeight(bounds) * (cfg.mode === "topology-first" ? 0.045 : 0.11) -
      overflowPenalty * 10 +
      random() * 0.1;
    if (score > bestScore) {
      best = rotated;
      bestScore = score;
    }
  }

  return best;
}

function validateAndAssignPhaseTurns(gears: DraftGear[], edges: DraftMeshEdge[]): boolean {
  const byId = new Map(gears.map((gear) => [gear.id, gear]));
  const adjacency = new Map<string, Array<{ other: string; angle: number }>>();

  for (const edge of edges) {
    const a = byId.get(edge.a);
    const b = byId.get(edge.b);
    if (!a || !b) return false;
    const desired = a.pitchRadius + b.pitchRadius;
    if (Math.abs(dist(a.center, b.center) - desired) > 0.12) return false;
    if (!adjacency.has(a.id)) adjacency.set(a.id, []);
    if (!adjacency.has(b.id)) adjacency.set(b.id, []);
    const angle = Math.atan2(b.center.y - a.center.y, b.center.x - a.center.x);
    adjacency.get(a.id)?.push({ other: b.id, angle });
    adjacency.get(b.id)?.push({ other: a.id, angle: angle + Math.PI });
  }

  const edgeKeys = new Set(edges.map((edge) => meshEdgeKey(edge.a, edge.b)));
  for (let i = 0; i < gears.length; i += 1) {
    for (let j = i + 1; j < gears.length; j += 1) {
      const a = gears[i];
      const b = gears[j];
      if (edgeKeys.has(meshEdgeKey(a.id, b.id))) continue;
      if (dist(a.center, b.center) < collisionRadius(a) + collisionRadius(b) - 0.03) return false;
    }
  }

  const assignedTurns = new Map<string, number>();
  for (const gear of gears) {
    if (assignedTurns.has(gear.id)) continue;
    assignedTurns.set(gear.id, normalizeTurn(gear.phaseTurn ?? 0));
    const queue = [gear.id];

    while (queue.length > 0) {
      const currentId = queue.shift();
      if (!currentId) continue;
      const currentTurn = assignedTurns.get(currentId) ?? 0;
      const currentGear = byId.get(currentId);
      if (!currentGear) return false;

      for (const neighbor of adjacency.get(currentId) ?? []) {
        const neighborGear = byId.get(neighbor.other);
        if (!neighborGear) return false;
        const required = solveNeighborPhaseTurn({
          currentTeeth: currentGear.teeth,
          neighborTeeth: neighborGear.teeth,
          currentTurn,
          contactAngleCurrentToNeighbor: neighbor.angle,
        });
        const existing = assignedTurns.get(neighbor.other);
        if (existing == null) {
          assignedTurns.set(neighbor.other, required);
          queue.push(neighbor.other);
          continue;
        }
      }
    }
  }

  for (const edge of edges) {
    const a = byId.get(edge.a);
    const b = byId.get(edge.b);
    if (!a || !b) return false;
    const aTurn = assignedTurns.get(a.id);
    const bTurn = assignedTurns.get(b.id);
    if (aTurn == null || bTurn == null) return false;
    const alpha = Math.atan2(b.center.y - a.center.y, b.center.x - a.center.x);
    const alphaA = alpha / TAU;
    const alphaB = (alpha + Math.PI) / TAU;
    const lhs = a.teeth * (alphaA - aTurn) + b.teeth * (alphaB - bTurn);
    const delta = Math.abs(normalizeTurn(lhs - 0.5));
    const wrapped = Math.min(delta, 1 - delta);
    if (wrapped > 1e-5) return false;
  }

  for (const gear of gears) {
    gear.phaseTurn = assignedTurns.get(gear.id) ?? 0;
  }

  return true;
}

function scoreFinalLayout(gears: DraftGear[], edges: DraftMeshEdge[], cfg: SpecBuildProfile): number {
  const bounds = boundsOfGears(gears);
  const xSpan = bounds.maxX - bounds.minX;
  const ySpan = bounds.maxY - bounds.minY;
  const sizeMean = gears.reduce((sum, gear) => sum + Math.log(gear.teeth), 0) / Math.max(1, gears.length);
  const sizeVariance = gears.reduce((sum, gear) => sum + (Math.log(gear.teeth) - sizeMean) ** 2, 0) / Math.max(1, gears.length);
  const component: BuiltComponent = { id: "scored", gears, edges, score: 0 };
  const degrees = degreeMap(edges);
  const multiNeighborCount = gears.filter((gear) => (degrees.get(gear.id) ?? 0) >= 2).length;

  return (
    gears.length * 95 +
    sizeVariance * 420 +
    angularIrregularity(component) * (cfg.mode === "topology-first" ? 220 : 110) -
    edgeAlignmentPenalty(component) * 40 -
    xSpan * (cfg.mode === "topology-first" ? 0.035 : 0.1) -
    ySpan * (cfg.mode === "topology-first" ? 0.11 : 0.22) -
    multiNeighborCount * (cfg.mode === "constraint-solver" ? 90 : 28) +
    (boundsWidth(bounds) * boundsHeight(bounds)) * 0.0007
  );
}

function packComponents(options: {
  components: BuiltComponent[];
  cfg: SpecBuildProfile;
  random: () => number;
}): BuiltComponent[] {
  const { components, cfg, random } = options;
  const rowTargets = Array.from({ length: cfg.rowCount }, (_, index) => {
    const t = cfg.rowCount === 1 ? 0.5 : index / (cfg.rowCount - 1);
    return 60 + t * 320;
  });
  const cursors = rowTargets.map(() => HERO_X_MIN - 40);
  const packed: BuiltComponent[] = [];

  const ordered = components
    .slice()
    .sort((left, right) => boundsWidth(boundsOfGears(right.gears)) - boundsWidth(boundsOfGears(left.gears)));

  for (const component of ordered) {
    let best: { row: number; packed: BuiltComponent; score: number } | null = null;

    for (let row = 0; row < rowTargets.length; row += 1) {
      const bounds = boundsOfGears(component.gears);
      const x = cursors[row] - bounds.minX + (packed.length === 0 ? -random() * 40 : random() * 20);
      const yCenter = (bounds.minY + bounds.maxY) * 0.5;
      const y = rowTargets[row] - yCenter + (random() - 0.5) * 26;
      const translated = translateComponent(component, { x, y });
      const translatedBounds = boundsOfGears(translated.gears);
      if (translatedBounds.maxY > HERO_Y_MAX + 24 || translatedBounds.minY < HERO_Y_MIN - 24) continue;

      let overlapsPacked = false;
      for (const packedComponent of packed) {
        for (const translatedGear of translated.gears) {
          for (const packedGear of packedComponent.gears) {
            const minimum = collisionRadius(translatedGear) + collisionRadius(packedGear) - 0.03;
            if (dist(translatedGear.center, packedGear.center) < minimum) {
              overlapsPacked = true;
              break;
            }
          }
          if (overlapsPacked) break;
        }
        if (overlapsPacked) break;
      }
      if (overlapsPacked) continue;

      const score =
        translatedBounds.minX * -0.02 -
        Math.abs((translatedBounds.minY + translatedBounds.maxY) * 0.5 - rowTargets[row]) * 0.3 +
        Math.max(0, translatedBounds.maxX - (HERO_X_MAX + 40)) * -0.4 +
        scoreFinalLayout(translated.gears, translated.edges, cfg) * 0.01;
      if (!best || score > best.score) best = { row, packed: translated, score };
    }

    if (!best) continue;
    packed.push(best.packed);
    const packedBounds = boundsOfGears(best.packed.gears);
    cursors[best.row] = packedBounds.maxX + 44;
  }

  return packed;
}

function finalizeBackdrop(components: BuiltComponent[]): BackdropGeneratorResult | null {
  const flattenedGears = components.flatMap((component) => component.gears);
  const flattenedEdges = components.flatMap((component) => component.edges);
  if (flattenedGears.length === 0) return null;

  const originalById = new Map(flattenedGears.map((gear) => [gear.id, gear]));
  const orderedGears = flattenedGears.slice().sort((left, right) => {
    if (left.center.x !== right.center.x) return left.center.x - right.center.x;
    return left.center.y - right.center.y;
  });
  const idMap = new Map<string, string>();
  const gears = orderedGears.map((gear, index) => {
    const nextId = `hero-g${index}`;
    idMap.set(gear.id, nextId);
    return {
      ...gear,
      id: nextId,
      appearIndex: index,
    };
  });
  for (const gear of gears) {
    const originalId = [...idMap.entries()].find(([, nextId]) => nextId === gear.id)?.[0];
    const originalParent = originalId ? originalById.get(originalId)?.parentId : undefined;
    gear.parentId = originalParent ? idMap.get(originalParent) : undefined;
  }

  const edges = flattenedEdges.map((edge) => ({
    a: idMap.get(edge.a) ?? edge.a,
    b: idMap.get(edge.b) ?? edge.b,
  }));

  if (!validateAndAssignPhaseTurns(gears, edges)) return null;
  return { gears, edges };
}

function buildProfile(mode: SpecAlgorithmMode, targetCount: number): SpecBuildProfile {
  if (mode === "constraint-solver") {
    return {
      mode,
      targetCount,
      maxAttempts: 32,
      componentCountMin: 8,
      componentCountMax: 10,
      componentSizeMin: 5,
      componentSizeMax: 7,
      placementAngleSamples: 24,
      placementAttempts: 28,
      bridgeAttempts: 5,
      bridgeTeethSamples: 10,
      multiStarts: 10,
      solverIterations: 150,
      solverJitter: 0.58,
      compactnessPull: 0.08,
      overlapPush: 1.06,
      rowCount: 3,
      rootTeethMin: 28,
      rootTeethMax: 44,
      preferSmallNeighbors: 0.55,
      preferLargeNeighbors: 0.35,
      preferredRatioMin: 0.35,
      preferredRatioMax: 2.3,
      branchSpreadWeight: 0.7,
      compactClusterWeight: 0.014,
      bridgeSpanPreference: 0.04,
      bridgeLoopPreference: 1.5,
    };
  }

  return {
    mode,
    targetCount,
    maxAttempts: 28,
    componentCountMin: 8,
    componentCountMax: 10,
    componentSizeMin: 5,
    componentSizeMax: 7,
    placementAngleSamples: 24,
    placementAttempts: 20,
    bridgeAttempts: 4,
    bridgeTeethSamples: 8,
    multiStarts: 5,
    solverIterations: 80,
    solverJitter: 0.18,
    compactnessPull: 0.04,
    overlapPush: 1.0,
    rowCount: 3,
    rootTeethMin: 38,
    rootTeethMax: 58,
    preferSmallNeighbors: 1.15,
    preferLargeNeighbors: -0.15,
    preferredRatioMin: 0.32,
    preferredRatioMax: 2.4,
    branchSpreadWeight: 1.35,
    compactClusterWeight: 0.01,
    bridgeSpanPreference: 0.12,
    bridgeLoopPreference: 0.45,
  };
}

function buildComponents(random: () => number, cfg: SpecBuildProfile): BuiltComponent[] {
  const budgets = sampleComponentBudgets(random, cfg);
  const components: BuiltComponent[] = [];

  for (let componentIndex = 0; componentIndex < budgets.length; componentIndex += 1) {
    const nodeCount = budgets[componentIndex];
    let built: BuiltComponent | null = null;
    let bestScore = -Infinity;

    for (let attempt = 0; attempt < cfg.placementAttempts; attempt += 1) {
      const nodes = buildNodePlan(random, nodeCount, cfg);
      const trial = tryConstructiveComponent({
        componentId: componentIndex,
        nodes,
        allGears: [],
        cfg,
        random,
      });
      if (!trial) continue;
      const withBridges = addBridgeGears({ component: trial, allGears: [], cfg, random });
      const solved = solveComponentLayout({ component: withBridges, cfg, random });
      const bounds = boundsOfGears(solved.gears);
      const attemptScore =
        solved.gears.length * 70 -
        Math.max(0, boundsHeight(bounds) - (HERO_Y_MAX - HERO_Y_MIN + 30)) * 4 -
        boundsWidth(bounds) * 0.04 +
        angularIrregularity(solved) * 20 -
        edgeAlignmentPenalty(solved) * 6 +
        (cfg.mode === "constraint-solver"
          ? (degreeMap(solved.edges).size > 0
              ? solved.gears.filter((gear) => (degreeMap(solved.edges).get(gear.id) ?? 0) >= 2).length * 24
              : 0)
          : 0);
      if (!built || attemptScore > bestScore) {
        built = solved;
        bestScore = attemptScore;
      }
    }

    if (!built) continue;
    components.push(built);
  }

  return components;
}

export function generateSpecBackdrop(
  options: BackdropGeneratorOptions & { mode: SpecAlgorithmMode },
): BackdropGeneratorResult {
  return generateFastSpecBackdrop(options);
}

export function debugSpecBackdropAttempts(
  options: BackdropGeneratorOptions & { mode: SpecAlgorithmMode; attempts?: number },
) {
  return debugFastSpecBackdropAttempts(options);
}
