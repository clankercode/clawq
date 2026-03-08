import { pitchRadiusFromTeeth } from "../solver.ts";
import type { BackdropGeneratorFn, DraftGear, DraftMeshEdge, Point } from "./types.ts";
import defaultOrganicFieldBackdrop from "./data/organic_field.default.json" with { type: "json" };
import { assertBackdropResult, cloneBackdrop } from "./result_utils.ts";
import {
  HERO_GEAR_CIRCULAR_PITCH,
  createGenerationContext,
  dist,
  getTwoParentMeshedIntersections,
  meshEdgeKey,
  normalizeTurn,
  outerRadiusFromTeeth,
  pointAt,
  randInt,
  registerMeshContacts,
  solveNeighborPhaseTurn,
} from "./shared.ts";

type WorkingState = {
  gears: DraftGear[];
  edges: DraftMeshEdge[];
  edgeKeys: Set<string>;
  contactAnglesByGearId: Map<string, number[]>;
};

type CandidatePlacement = {
  gear: DraftGear;
  neighbors: DraftGear[];
  score: number;
};

type ConnectedComponent = {
  gearIds: string[];
  edgeKeys: string[];
};

const DEFAULT_ORGANIC_FIELD_SEED = 0x6a11cf;
const DEFAULT_ORGANIC_FIELD_TARGET_COUNT = 96;
const defaultBackdrop = assertBackdropResult(defaultOrganicFieldBackdrop, "organic-field default fixture");

const BUCKETS = 14;
const ROW_BUCKETS = 6;
const MAX_DEGREE = 4;
const MAX_ORGANIC_TEETH = 24;
const MIN_CONTACT_SEPARATION = 0.5;
const MESH_DISTANCE_TOLERANCE = 0.75;
const NON_NEIGHBOR_CLEARANCE = 14;
const PHASE_TOLERANCE = 0.012;
const EXTRA_EDGE_RESIDUAL = 0.006;
const MULTI_STARTS = 3;
const GROWTH_ATTEMPTS = 22;
const BRIDGE_ATTEMPTS = 18;
const SEED_VARIANTS = 6;

function clamp(value: number, min: number, max: number): number {
  return Math.min(max, Math.max(min, value));
}

function wrapAngleDelta(angleRad: number): number {
  const tau = Math.PI * 2;
  return ((angleRad + Math.PI) % tau + tau) % tau - Math.PI;
}

function meshResidual(a: DraftGear, b: DraftGear): number {
  const aTurn = a.phaseTurn ?? 0;
  const bTurn = b.phaseTurn ?? 0;
  const alpha = Math.atan2(b.center.y - a.center.y, b.center.x - a.center.x);
  const alphaA = alpha / (Math.PI * 2);
  const alphaB = (alpha + Math.PI) / (Math.PI * 2);
  const lhs = a.teeth * (alphaA - aTurn) + b.teeth * (alphaB - bTurn);
  const delta = Math.abs(normalizeTurn(lhs) - 0.5);
  return Math.min(delta, 1 - delta);
}

function resolveBounds(viewport?: { minX?: number; minY?: number; width: number; height: number }) {
  const minX = viewport?.minX ?? -140;
  const minY = viewport?.minY ?? -60;
  const width = Math.max(320, viewport?.width ?? 1880);
  const height = Math.max(220, viewport?.height ?? 460);
  return {
    minX,
    minY,
    maxX: minX + width,
    maxY: minY + height,
    width,
    height,
  };
}

function xBucket(centerX: number, bounds: ReturnType<typeof resolveBounds>): number {
  return clamp(Math.floor(((centerX - bounds.minX) / bounds.width) * BUCKETS), 0, BUCKETS - 1);
}

function yBucket(centerY: number, bounds: ReturnType<typeof resolveBounds>): number {
  return clamp(Math.floor(((centerY - bounds.minY) / bounds.height) * ROW_BUCKETS), 0, ROW_BUCKETS - 1);
}

function gearWithTeeth(options: {
  id: string;
  teeth: number;
  center: Point;
  parity: 0 | 1;
  appearIndex: number;
  parentId?: string;
  phaseTurn?: number;
}): DraftGear {
  const { id, teeth, center, parity, appearIndex, parentId, phaseTurn } = options;
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

function candidateTeeth(random: () => number, parentTeeth: number): number[] {
  const seen = new Set<number>([parentTeeth]);
  const ordered: number[] = [];
  for (let attempt = 0; attempt < 24; attempt += 1) {
    const offset = randInt(random, -7, 8);
    const teeth = clamp(parentTeeth + offset, 12, MAX_ORGANIC_TEETH);
    if (seen.has(teeth)) continue;
    seen.add(teeth);
    ordered.push(teeth);
  }
  return ordered.length > 0 ? ordered : [clamp(parentTeeth + 4, 12, MAX_ORGANIC_TEETH)];
}

function degreeByGearId(edges: DraftMeshEdge[]): Map<string, number> {
  const degrees = new Map<string, number>();
  for (const edge of edges) {
    degrees.set(edge.a, (degrees.get(edge.a) ?? 0) + 1);
    degrees.set(edge.b, (degrees.get(edge.b) ?? 0) + 1);
  }
  return degrees;
}

function hasContactRoom(contactAnglesByGearId: Map<string, number[]>, gear: DraftGear, angleRad: number): boolean {
  const existing = contactAnglesByGearId.get(gear.id) ?? [];
  return existing.every((angle) => Math.abs(wrapAngleDelta(angle - angleRad)) >= MIN_CONTACT_SEPARATION);
}

function withinBand(gear: DraftGear, bounds: ReturnType<typeof resolveBounds>): boolean {
  return (
    gear.center.x - gear.outerRadius >= bounds.minX &&
    gear.center.x + gear.outerRadius <= bounds.maxX &&
    gear.center.y - gear.outerRadius >= bounds.minY &&
    gear.center.y + gear.outerRadius <= bounds.maxY
  );
}

function canPlace(state: WorkingState, candidate: DraftGear, neighbors: DraftGear[]): boolean {
  const neighborIds = new Set(neighbors.map((neighbor) => neighbor.id));
  for (const neighbor of neighbors) {
    const angleFromCandidate = Math.atan2(neighbor.center.y - candidate.center.y, neighbor.center.x - candidate.center.x);
    const angleFromNeighbor = Math.atan2(candidate.center.y - neighbor.center.y, candidate.center.x - neighbor.center.x);
    if (!hasContactRoom(state.contactAnglesByGearId, candidate, angleFromCandidate)) return false;
    if (!hasContactRoom(state.contactAnglesByGearId, neighbor, angleFromNeighbor)) return false;
  }

  for (const other of state.gears) {
    const separation = dist(candidate.center, other.center);
    if (neighborIds.has(other.id)) {
      const expected = candidate.pitchRadius + other.pitchRadius;
      if (Math.abs(separation - expected) > MESH_DISTANCE_TOLERANCE) return false;
      continue;
    }

    if (separation < candidate.outerRadius + other.outerRadius + NON_NEIGHBOR_CLEARANCE) {
      return false;
    }
  }

  return true;
}

function contactAngleScore(contactAngle: number): number {
  const axisPenalty = Math.max(Math.abs(Math.cos(contactAngle)), Math.abs(Math.sin(contactAngle)));
  const hexPenalty = Math.abs(Math.cos(contactAngle * 3));
  const horizontalPenalty = Math.abs(Math.cos(contactAngle));
  const diagonalBias = Math.abs(Math.sin(contactAngle * 2));
  return (1 - axisPenalty) * 1.25 + (1 - hexPenalty) * 0.8 + diagonalBias * 0.85 - horizontalPenalty * 0.7;
}

function occupancyGain(occupiedBuckets: Set<number>, gear: DraftGear, bounds: ReturnType<typeof resolveBounds>): number {
  const start = xBucket(gear.center.x - gear.outerRadius, bounds);
  const end = xBucket(gear.center.x + gear.outerRadius, bounds);
  let gain = 0;
  for (let bucket = start; bucket <= end; bucket += 1) {
    if (!occupiedBuckets.has(bucket)) gain += 1;
  }
  return gain;
}

function verticalBandGain(state: WorkingState, gear: DraftGear, bounds: ReturnType<typeof resolveBounds>): number {
  const occupied = new Set(state.gears.map((entry) => yBucket(entry.center.y, bounds)));
  return occupied.has(yBucket(gear.center.y, bounds)) ? 0 : 1;
}

function localRowCrowding(state: WorkingState, gear: DraftGear, bounds: ReturnType<typeof resolveBounds>): number {
  const row = yBucket(gear.center.y, bounds);
  return state.gears.filter((entry) => yBucket(entry.center.y, bounds) === row).length;
}

function provisionalDegreeSummary(state: WorkingState, candidate: DraftGear, neighbors: DraftGear[]) {
  const degrees = degreeByGearId(state.edges);
  for (const neighbor of neighbors) {
    degrees.set(neighbor.id, (degrees.get(neighbor.id) ?? 0) + 1);
  }
  degrees.set(candidate.id, neighbors.length);

  const values = [...degrees.values()];
  return {
    degrees,
    degreeTwoCount: values.filter((degree) => degree === 2).length,
    leafCount: values.filter((degree) => degree <= 1).length,
    richCount: values.filter((degree) => degree >= 3).length,
  };
}

function topologyDeltaScore(state: WorkingState, candidate: DraftGear, neighbors: DraftGear[]): number {
  const priorDegrees = degreeByGearId(state.edges);
  const summary = provisionalDegreeSummary(state, candidate, neighbors);
  const cycleClosureBonus = neighbors.length >= 2 ? 8.5 + neighbors.length * 1.8 : 0;
  const branchSupport = neighbors.reduce((sum, neighbor) => {
    const prior = priorDegrees.get(neighbor.id) ?? 0;
    const next = prior + 1;
    return sum + (next >= 3 ? 1.9 : next === 2 ? 0.25 : -1.4);
  }, 0);
  const loneChainPenalty =
    neighbors.length === 1
      ? ((priorDegrees.get(neighbors[0].id) ?? 0) <= 1 ? 4.8 : 2.4) + 1.2
      : 0;
  const degreeTwoPenalty = Math.max(0, summary.degreeTwoCount - summary.richCount * 1.6) * 0.18;
  const leafPressurePenalty = Math.max(0, summary.leafCount - summary.richCount * 1.15) * 0.24;

  return cycleClosureBonus + branchSupport - loneChainPenalty - degreeTwoPenalty - leafPressurePenalty;
}

function localBridgePotential(state: WorkingState, candidate: DraftGear): number {
  let count = 0;
  for (const other of state.gears) {
    if (other.id === candidate.id || other.parity !== candidate.parity) continue;
    const separation = dist(candidate.center, other.center);
    if (separation >= 150 && separation <= 340) count += 1;
  }
  return count;
}

function scoreCandidate(
  state: WorkingState,
  candidate: DraftGear,
  neighbors: DraftGear[],
  occupiedBuckets: Set<number>,
  bounds: ReturnType<typeof resolveBounds>,
): number {
  const minDistance = state.gears.reduce((best, other) => {
    if (neighbors.some((neighbor) => neighbor.id === other.id)) return best;
    return Math.min(best, dist(candidate.center, other.center));
  }, Infinity);
  const yCenter = bounds.minY + bounds.height * 0.42;
  const yPenalty = Math.abs(candidate.center.y - yCenter) * 0.035;
  const degreePenalty = neighbors.length >= 3 ? 0.7 : 0;
  const bridgePotential = localBridgePotential(state, candidate) * 0.8;
  const coverageScore = occupancyGain(occupiedBuckets, candidate, bounds) * 4.5;
  const verticalScore = verticalBandGain(state, candidate, bounds) * 5.5;
  const spacingScore = clamp((minDistance - 120) / 36, -3, 4);
  const topologyScore = topologyDeltaScore(state, candidate, neighbors);
  const contactScore = neighbors.reduce(
    (sum, neighbor) => sum + contactAngleScore(Math.atan2(candidate.center.y - neighbor.center.y, candidate.center.x - neighbor.center.x)),
    0,
  );
  const horizontalSpinePenalty = neighbors.reduce((sum, neighbor) => {
    const angle = Math.atan2(candidate.center.y - neighbor.center.y, candidate.center.x - neighbor.center.x);
    return sum + Math.max(0, Math.abs(Math.cos(angle)) - 0.48) * 2.2;
  }, 0);
  const rowCrowdingPenalty = Math.max(0, localRowCrowding(state, candidate, bounds) - 4) * 0.75;

  return coverageScore + verticalScore + spacingScore + bridgePotential + topologyScore + contactScore - yPenalty - degreePenalty - horizontalSpinePenalty - rowCrowdingPenalty;
}

function attachExtraEdges(state: WorkingState, gear: DraftGear, seededNeighbors: DraftGear[]): DraftGear[] {
  const neighbors = seededNeighbors.slice();
  const seededNeighborIds = new Set(neighbors.map((neighbor) => neighbor.id));
  const degrees = degreeByGearId(state.edges);

  for (const other of state.gears) {
    if (seededNeighborIds.has(other.id)) continue;
    if (other.parity === gear.parity) continue;
    if ((degrees.get(other.id) ?? 0) >= MAX_DEGREE) continue;
    if (neighbors.length >= MAX_DEGREE) break;
    const key = meshEdgeKey(gear.id, other.id);
    if (state.edgeKeys.has(key)) continue;

    const separation = dist(gear.center, other.center);
    const expected = gear.pitchRadius + other.pitchRadius;
    if (Math.abs(separation - expected) > MESH_DISTANCE_TOLERANCE) continue;
    const angleFromGear = Math.atan2(other.center.y - gear.center.y, other.center.x - gear.center.x);
    const angleFromOther = Math.atan2(gear.center.y - other.center.y, gear.center.x - other.center.x);
    if (!hasContactRoom(state.contactAnglesByGearId, gear, angleFromGear)) continue;
    if (!hasContactRoom(state.contactAnglesByGearId, other, angleFromOther)) continue;
    if (meshResidual(gear, other) > EXTRA_EDGE_RESIDUAL) continue;
    neighbors.push(other);
  }

  return neighbors;
}

function tryGrowthPlacement(
  state: WorkingState,
  random: () => number,
  occupiedBuckets: Set<number>,
  appearIndex: number,
  bounds: ReturnType<typeof resolveBounds>,
): CandidatePlacement | null {
  const degrees = degreeByGearId(state.edges);
  const parentPool = state.gears
    .filter((gear) => (degrees.get(gear.id) ?? 0) < MAX_DEGREE)
    .sort((left, right) => (degrees.get(left.id) ?? 0) - (degrees.get(right.id) ?? 0))
    .slice(0, Math.max(8, Math.floor(state.gears.length * 0.5)));
  if (parentPool.length === 0) return null;

  let best: CandidatePlacement | null = null;

  for (let attempt = 0; attempt < GROWTH_ATTEMPTS; attempt += 1) {
    const parent = parentPool[randInt(random, 0, parentPool.length - 1)] ?? state.gears[state.gears.length - 1];
    const parentDegree = degrees.get(parent.id) ?? 0;
    if (parentDegree >= MAX_DEGREE) continue;

    const angles = Array.from({ length: 24 }, (_, index) => {
      const base = (index / 24) * Math.PI * 2;
      return base + (random() - 0.5) * 0.35;
    }).sort((left, right) => contactAngleScore(right) - contactAngleScore(left));

    for (const teeth of candidateTeeth(random, parent.teeth)) {
      for (const angle of angles) {
        const center = pointAt(parent.center, angle, parent.pitchRadius + pitchRadiusFromTeeth(teeth, HERO_GEAR_CIRCULAR_PITCH));
        const gear = gearWithTeeth({
          id: `hero-g${appearIndex}`,
          teeth,
          center,
          parity: parent.parity === 0 ? 1 : 0,
          parentId: parent.id,
          appearIndex,
          phaseTurn: solveNeighborPhaseTurn({
            currentTeeth: parent.teeth,
            neighborTeeth: teeth,
            currentTurn: parent.phaseTurn ?? 0,
            contactAngleCurrentToNeighbor: angle,
          }),
        });
        if (!withinBand(gear, bounds)) continue;
        if (!canPlace(state, gear, [parent])) continue;
        const neighbors = attachExtraEdges(state, gear, [parent]);
        const score = scoreCandidate(state, gear, neighbors, occupiedBuckets, bounds) - parentDegree * 0.45;
        if (!best || score > best.score) best = { gear, neighbors, score };
      }
    }
  }

  return best;
}

function tryBridgePlacement(
  state: WorkingState,
  random: () => number,
  occupiedBuckets: Set<number>,
  appearIndex: number,
  bounds: ReturnType<typeof resolveBounds>,
): CandidatePlacement | null {
  const degrees = degreeByGearId(state.edges);
  const eligible = state.gears.filter((gear) => (degrees.get(gear.id) ?? 0) < MAX_DEGREE);
  if (eligible.length < 2) return null;

  let best: CandidatePlacement | null = null;

  for (let attempt = 0; attempt < BRIDGE_ATTEMPTS; attempt += 1) {
    const a = eligible[randInt(random, 0, eligible.length - 1)];
    const b = eligible[randInt(random, 0, eligible.length - 1)];
    if (!a || !b || a.id === b.id) continue;
    if (a.parity !== b.parity) continue;
    if (state.edgeKeys.has(meshEdgeKey(a.id, b.id))) continue;
    const separation = dist(a.center, b.center);
    if (
      separation < Math.abs(a.pitchRadius - b.pitchRadius) + 28 ||
      separation > a.pitchRadius + b.pitchRadius + pitchRadiusFromTeeth(MAX_ORGANIC_TEETH, HERO_GEAR_CIRCULAR_PITCH) * 2
    ) {
      continue;
    }

    for (const teeth of candidateTeeth(random, Math.round((a.teeth + b.teeth) * 0.5))) {
      for (const intersection of getTwoParentMeshedIntersections({ parentA: a, parentB: b, teeth })) {
        const phaseTurn = solveNeighborPhaseTurn({
          currentTeeth: a.teeth,
          neighborTeeth: teeth,
          currentTurn: a.phaseTurn ?? 0,
          contactAngleCurrentToNeighbor: intersection.contactAngleFromA,
        });
        const gear = gearWithTeeth({
          id: `hero-g${appearIndex}`,
          teeth,
          center: intersection.center,
          parity: a.parity === 0 ? 1 : 0,
          parentId: a.id,
          appearIndex,
          phaseTurn,
        });
        if (!withinBand(gear, bounds)) continue;
        if (!canPlace(state, gear, [a, b])) continue;
        if (meshResidual(gear, b) > PHASE_TOLERANCE) continue;
        const neighbors = attachExtraEdges(state, gear, [a, b]);
        const score = scoreCandidate(state, gear, neighbors, occupiedBuckets, bounds) + 7.5 + neighbors.length * 1.4;
        if (!best || score > best.score) best = { gear, neighbors, score };
      }
    }
  }

  return best;
}

function registerPlacement(state: WorkingState, placement: CandidatePlacement): void {
  state.gears.push(placement.gear);
  registerMeshContacts({
    gear: placement.gear,
    neighbors: placement.neighbors,
    contactAnglesByGearId: state.contactAnglesByGearId,
    edges: state.edges,
    edgeKeys: state.edgeKeys,
  });
}

function componentSummary(gears: DraftGear[], edges: DraftMeshEdge[]): ConnectedComponent[] {
  const adjacency = new Map<string, string[]>();
  for (const gear of gears) adjacency.set(gear.id, []);
  for (const edge of edges) {
    adjacency.get(edge.a)?.push(edge.b);
    adjacency.get(edge.b)?.push(edge.a);
  }

  const seen = new Set<string>();
  const components: ConnectedComponent[] = [];

  for (const gear of gears) {
    if (seen.has(gear.id)) continue;
    const stack = [gear.id];
    const gearIds: string[] = [];
    const edgeKeys = new Set<string>();
    seen.add(gear.id);

    while (stack.length > 0) {
      const current = stack.pop();
      if (!current) continue;
      gearIds.push(current);
      for (const next of adjacency.get(current) ?? []) {
        edgeKeys.add(meshEdgeKey(current, next));
        if (seen.has(next)) continue;
        seen.add(next);
        stack.push(next);
      }
    }

    components.push({ gearIds, edgeKeys: Array.from(edgeKeys) });
  }

  return components;
}

function pruneBackdrop(state: WorkingState): WorkingState {
  const gearById = new Map(state.gears.map((gear) => [gear.id, gear]));
  const keptEdges = state.edges.filter((edge) => {
    const a = gearById.get(edge.a);
    const b = gearById.get(edge.b);
    return Boolean(a && b && meshResidual(a, b) <= PHASE_TOLERANCE);
  });
  const components = componentSummary(state.gears, keptEdges).sort((left, right) => right.gearIds.length - left.gearIds.length);
  const primary = components[0];
  if (!primary) return state;

  const keepIds = new Set(primary.gearIds);
  const filteredGears = state.gears.filter((gear) => keepIds.has(gear.id));
  const filteredEdges = keptEdges.filter((edge) => keepIds.has(edge.a) && keepIds.has(edge.b));
  const reindexedGears = filteredGears.map((gear, index) => ({ ...gear, appearIndex: index }));
  const rebuiltState: WorkingState = {
    gears: reindexedGears,
    edges: [],
    edgeKeys: new Set<string>(),
    contactAnglesByGearId: new Map<string, number[]>(),
  };

  const rebuiltById = new Map(reindexedGears.map((gear) => [gear.id, gear]));
  for (const edge of filteredEdges) {
    const a = rebuiltById.get(edge.a);
    const b = rebuiltById.get(edge.b);
    if (!a || !b) continue;
    registerMeshContacts({
      gear: a,
      neighbors: [b],
      contactAnglesByGearId: rebuiltState.contactAnglesByGearId,
      edges: rebuiltState.edges,
      edgeKeys: rebuiltState.edgeKeys,
    });
  }

  return rebuiltState;
}

function occupiedBuckets(state: WorkingState, bounds: ReturnType<typeof resolveBounds>): Set<number> {
  const occupied = new Set<number>();
  for (const gear of state.gears) {
    const start = xBucket(gear.center.x - gear.outerRadius, bounds);
    const end = xBucket(gear.center.x + gear.outerRadius, bounds);
    for (let bucket = start; bucket <= end; bucket += 1) occupied.add(bucket);
  }
  return occupied;
}

function layoutScore(state: WorkingState, bounds: ReturnType<typeof resolveBounds>): number {
  const degrees = degreeByGearId(state.edges);
  const occupied = occupiedBuckets(state, bounds).size;
  const rich = [...degrees.values()].filter((degree) => degree >= 3).length;
  const degreeTwo = [...degrees.values()].filter((degree) => degree === 2).length;
  const leaves = [...degrees.values()].filter((degree) => degree <= 1).length;
  const spanX =
    Math.max(...state.gears.map((gear) => gear.center.x + gear.outerRadius)) -
    Math.min(...state.gears.map((gear) => gear.center.x - gear.outerRadius));
  const cycleRank = state.edges.length - state.gears.length + componentSummary(state.gears, state.edges).length;
  const cycleDensity = state.gears.length > 0 ? cycleRank / state.gears.length : 0;
  return occupied * 14 + rich * 9 + cycleRank * 18 + cycleDensity * 220 + spanX * 0.02 - degreeTwo * 1.35 - leaves * 3.2;
}

function buildOrganicField(random: () => number, targetCount: number, bounds: ReturnType<typeof resolveBounds>): WorkingState {
  const rootTeeth = randInt(random, 14, 18);
  const root = gearWithTeeth({
    id: "hero-g0",
    teeth: rootTeeth,
    center: {
      x: bounds.minX + bounds.width * 0.12 + random() * Math.min(120, bounds.width * 0.08),
      y: bounds.minY + bounds.height * 0.42 + (random() - 0.5) * Math.min(90, bounds.height * 0.18),
    },
    parity: 0,
    appearIndex: 0,
    phaseTurn: 0,
  });
  const state: WorkingState = {
    gears: [root],
    edges: [],
    edgeKeys: new Set<string>(),
    contactAnglesByGearId: new Map<string, number[]>(),
  };

  const growthTarget = Math.max(14, Math.floor(targetCount * 0.68));
  let stalled = 0;
  while (state.gears.length < growthTarget && stalled < targetCount * 3) {
    const occupied = occupiedBuckets(state, bounds);
    const placement =
      state.gears.length >= 8 && random() < 0.5
        ? tryBridgePlacement(state, random, occupied, state.gears.length, bounds) ?? tryGrowthPlacement(state, random, occupied, state.gears.length, bounds)
        : tryGrowthPlacement(state, random, occupied, state.gears.length, bounds) ?? tryBridgePlacement(state, random, occupied, state.gears.length, bounds);

    if (!placement) {
      stalled += 1;
      continue;
    }

    registerPlacement(state, placement);
    stalled = 0;
  }

  let loopFailures = 0;
  while (state.gears.length < targetCount && loopFailures < targetCount * 2) {
    const occupied = occupiedBuckets(state, bounds);
    const placement = tryBridgePlacement(state, random, occupied, state.gears.length, bounds);
    if (!placement) {
      loopFailures += 1;
      continue;
    }
    registerPlacement(state, placement);
    loopFailures = 0;
  }

  let finishFailures = 0;
  while (state.gears.length < targetCount && finishFailures < targetCount * 2) {
    const occupied = occupiedBuckets(state, bounds);
    const placement =
      random() < 0.65
        ? tryBridgePlacement(state, random, occupied, state.gears.length, bounds) ?? tryGrowthPlacement(state, random, occupied, state.gears.length, bounds)
        : tryGrowthPlacement(state, random, occupied, state.gears.length, bounds) ?? tryBridgePlacement(state, random, occupied, state.gears.length, bounds);
    if (!placement) {
      finishFailures += 1;
      continue;
    }
    registerPlacement(state, placement);
    finishFailures = 0;
  }

  return pruneBackdrop(state);
}

export const generateOrganicFieldBackdrop: BackdropGeneratorFn = ({ seed, targetCount = 96, viewport }) => {
  if (seed === DEFAULT_ORGANIC_FIELD_SEED && targetCount === DEFAULT_ORGANIC_FIELD_TARGET_COUNT && viewport == null) {
    return cloneBackdrop(defaultBackdrop);
  }

  const bounds = resolveBounds(viewport);
  let best: WorkingState | null = null;
  let bestScore = -Infinity;
  const minimumAcceptableGears = Math.max(16, Math.floor(targetCount * 0.3));

  for (let variant = 0; variant < SEED_VARIANTS; variant += 1) {
    const variantSeed = seed + variant * 0x9e37;
    const { random } = createGenerationContext(variantSeed, 0x0f61d3 ^ (variant * 0x45d9));

    for (let pass = 0; pass < MULTI_STARTS; pass += 1) {
      const candidate = buildOrganicField(random, targetCount, bounds);
      if (candidate.gears.length < minimumAcceptableGears) continue;
      const score = layoutScore(candidate, bounds);
      if (!best || score > bestScore) {
        best = candidate;
        bestScore = score;
      }
    }
  }

  if (best) return best;

  const fallbackRandom = createGenerationContext(seed ^ 0x51f15e, 0x0f61d3).random;
  const fallback = buildOrganicField(fallbackRandom, targetCount, bounds);
  return fallback.gears.length >= 2 ? fallback : { gears: [], edges: [] };
};
