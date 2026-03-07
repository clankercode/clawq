import { pitchRadiusFromTeeth } from "../solver.ts";
import {
  HERO_GEAR_CIRCULAR_PITCH,
  MIN_TEETH,
  VIEWBOX,
  createGenerationContext,
  dist,
  evaluatePlacement,
  getLegalContactSlots,
  getTwoParentMeshedIntersections,
  isContactAngleCompatible,
  meshEdgeKey,
  normalizeTurn,
  outerRadiusFromTeeth,
  pointAt,
  randInt,
  registerMeshContacts,
  solveNeighborPhaseTurn,
  type ContactAngleMap,
} from "./shared.ts";
import type { BackdropGeneratorOptions, BackdropGeneratorResult, DraftGear, DraftMeshEdge, Point } from "./types.ts";

type SpecMode = "topology-first" | "constraint-solver";

type CoverageSample = Point & {
  key: string;
  weight: number;
};

type Placement = {
  gear: DraftGear;
  neighbors: DraftGear[];
  score: number;
};

type PairPlan = {
  a: DraftGear;
  b: DraftGear;
  score: number;
};

type LoopChainCursor = {
  prevEven: DraftGear;
  carryOdd: DraftGear;
  direction: -1 | 1;
};

type ModeConfig = {
  seedSalt: number;
  rootY: number;
  targetBottomY: number;
  maxAttemptsFactor: number;
  bridgeBias: number;
  futureBridgeWeight: number;
  expansionCoverageWeight: number;
  leafPenalty: number;
  denseNeighborWeight: number;
};

const MAX_TEETH = 28;
const MAX_PITCH_RADIUS = pitchRadiusFromTeeth(MAX_TEETH, HERO_GEAR_CIRCULAR_PITCH);
const COVERAGE_COLS = 17;
const COVERAGE_ROWS = 8;
const COVERAGE_MARGIN_X = 220;
const TOP_Y = -170;

const MODE_CONFIG: Record<SpecMode, ModeConfig> = {
  "topology-first": {
    seedSalt: 0x5f42a,
    rootY: 212,
    targetBottomY: 328,
    maxAttemptsFactor: 22,
    bridgeBias: 420,
    futureBridgeWeight: 3.4,
    expansionCoverageWeight: 8,
    leafPenalty: 34,
    denseNeighborWeight: 140,
  },
  "constraint-solver": {
    seedSalt: 0x91bc3,
    rootY: 228,
    targetBottomY: 346,
    maxAttemptsFactor: 30,
    bridgeBias: 560,
    futureBridgeWeight: 3.2,
    expansionCoverageWeight: 8,
    leafPenalty: 30,
    denseNeighborWeight: 220,
  },
};

function angleDelta(a: number, b: number): number {
  return Math.atan2(Math.sin(a - b), Math.cos(a - b));
}

function bottomContourY(x: number, mode: SpecMode, phase: number): number {
  const base = MODE_CONFIG[mode].targetBottomY;
  return base + Math.sin(x / 185 + phase) * 22 + Math.sin(x / 79 - phase * 0.65) * 12;
}

function buildCoverageSamples(mode: SpecMode, phase: number): CoverageSample[] {
  const xMin = -COVERAGE_MARGIN_X;
  const xMax = VIEWBOX.width + COVERAGE_MARGIN_X;
  const yMin = TOP_Y;
  const yMax = MODE_CONFIG[mode].targetBottomY + 18;
  const xStep = (xMax - xMin) / (COVERAGE_COLS - 1);
  const yStep = (yMax - yMin) / (COVERAGE_ROWS - 1);
  const samples: CoverageSample[] = [];

  for (let row = 0; row < COVERAGE_ROWS; row += 1) {
    for (let col = 0; col < COVERAGE_COLS; col += 1) {
      const x = xMin + col * xStep;
      const y = yMin + row * yStep;
      const contourY = bottomContourY(x, mode, phase);
      if (y > contourY) continue;

      const edgeBias = Math.abs(col - (COVERAGE_COLS - 1) * 0.5) / ((COVERAGE_COLS - 1) * 0.5);
      const topBias = 1 - row / (COVERAGE_ROWS - 1);
      samples.push({
        x,
        y,
        key: `${col}:${row}`,
        weight: 1 + edgeBias * 0.9 + topBias * 0.95,
      });
    }
  }

  return samples;
}

function gearTouchesSample(gear: DraftGear, sample: CoverageSample): boolean {
  return dist(gear.center, sample) <= gear.outerRadius + 40;
}

function rootAngles(random: () => number): number[] {
  const stepTurns = [0.1, 0.18, 0.12, 0.2, 0.14, 0.15, 0.11];
  let turn = random();
  const angles: number[] = [];
  for (const step of stepTurns) {
    angles.push(turn * Math.PI * 2);
    turn += step + (random() - 0.5) * 0.02;
  }

  return angles.sort((left, right) => {
    const leftBias = Math.abs(Math.cos(left)) * 1.7 + Math.max(0, -Math.sin(left));
    const rightBias = Math.abs(Math.cos(right)) * 1.7 + Math.max(0, -Math.sin(right));
    return rightBias - leftBias;
  });
}

function parentAnglesFrom(parent: DraftGear, contactAnglesByGearId: ContactAngleMap, random: () => number): number[] {
  const contacts = contactAnglesByGearId.get(parent.id) ?? [];
  if (contacts.length === 0) return rootAngles(random);

  const slots = getLegalContactSlots({
    gear: parent,
    slotCount: parent.teeth,
    angleOffsetRad: contacts[0],
    contactAnglesByGearId,
  });
  const toothAngle = (Math.PI * 2) / parent.teeth;
  return slots
    .map((slot) => {
      const base = contacts[0] + slot * toothAngle;
      const clearance = contacts.reduce((best, contact) => Math.min(best, Math.abs(angleDelta(base, contact))), Math.PI);
      const outwardBias = Math.abs(base % (Math.PI * 2) - Math.PI) * 0.3;
      return { angle: base, score: clearance * 20 + outwardBias + random() * 4 };
    })
    .sort((a, b) => b.score - a.score)
    .slice(0, Math.min(14, slots.length))
    .flatMap(({ angle }) => {
      const offsets = [0, -toothAngle * 0.08, toothAngle * 0.08, -toothAngle * 0.16, toothAngle * 0.16, -toothAngle * 0.28, toothAngle * 0.28];
      return offsets.map((offset) => angle + offset);
    });
}

function buildCandidateTeeth(random: () => number, parentTeeth: number, mode: SpecMode): number[] {
  const preferred = mode === "constraint-solver" ? 15 : 15;
  return Array.from({ length: MAX_TEETH - MIN_TEETH + 1 }, (_, index) => index + MIN_TEETH)
    .map((teeth) => {
      const contrast = Math.abs(teeth - parentTeeth) * 0.035;
      const preferredBias = Math.abs(teeth - preferred) * 0.04;
      const largePenalty = teeth >= 25 ? 0.28 : 0;
      return {
        teeth,
        score: contrast + preferredBias + largePenalty + random() * 0.45,
      };
    })
    .sort((a, b) => a.score - b.score)
    .slice(0, 11)
    .map((entry) => entry.teeth);
}

function bridgeTeethOrder(plan: PairPlan, random: () => number): number[] {
  const centerTeeth = Math.round((plan.a.teeth + plan.b.teeth) * 0.5);
  return Array.from({ length: MAX_TEETH - MIN_TEETH + 1 }, (_, index) => index + MIN_TEETH)
    .map((teeth) => ({
      teeth,
      score: Math.abs(teeth - centerTeeth) * 0.28 + (teeth >= 24 ? 0.18 : 0) + random() * 0.9,
    }))
    .sort((a, b) => a.score - b.score)
    .slice(0, 12)
    .map((entry) => entry.teeth);
}

function pitchDistanceRange(a: DraftGear, b: DraftGear): { minReach: number; maxReach: number } {
  return {
    minReach: Math.abs(a.pitchRadius - b.pitchRadius),
    maxReach: a.pitchRadius + b.pitchRadius + MAX_PITCH_RADIUS * 2,
  };
}

function countSharedNeighbors(a: Iterable<string>, b: Set<string> | undefined): number {
  if (!b) return 0;
  let count = 0;
  for (const id of a) {
    if (b.has(id)) count += 1;
  }
  return count;
}

function cloneContactAngleMap(source: ContactAngleMap): ContactAngleMap {
  const cloned: ContactAngleMap = new Map();
  for (const [gearId, angles] of source) cloned.set(gearId, angles.slice());
  return cloned;
}

function provisionalContactAngles(candidate: DraftGear, neighbors: DraftGear[]): number[] {
  return neighbors.map((neighbor) => Math.atan2(neighbor.center.y - candidate.center.y, neighbor.center.x - candidate.center.x));
}

function scoreNearMeshMassage(state: SpecGenerationState, candidate: DraftGear, neighbors: DraftGear[]): number {
  const candidateContacts = provisionalContactAngles(candidate, neighbors);
  let score = 0;

  for (const other of state.gears) {
    if (neighbors.some((neighbor) => neighbor.id === other.id)) continue;
    if (candidate.parity === other.parity) continue;

    const expected = candidate.pitchRadius + other.pitchRadius;
    const residual = Math.abs(dist(candidate.center, other.center) - expected);
    if (residual > 22) continue;

    const contactFromCandidate = Math.atan2(other.center.y - candidate.center.y, other.center.x - candidate.center.x);
    if (!isContactAngleCompatible(candidate, candidateContacts, contactFromCandidate)) continue;

    const contactFromOther = Math.atan2(candidate.center.y - other.center.y, candidate.center.x - other.center.x);
    if (!isContactAngleCompatible(other, state.contactAnglesByGearId.get(other.id) ?? [], contactFromOther)) continue;

    const leafBonus = (state.degreeByGearId.get(other.id) ?? 0) <= 1 ? 18 : 0;
    score += (22 - residual) * 2.8 + leafBonus;
  }

  return score;
}

function forwardProgress(direction: -1 | 1, from: Point, to: Point): number {
  return direction * (to.x - from.x);
}

function cycleRankOf(state: SpecGenerationState): number {
  if (state.gears.length === 0) return 0;
  return state.edges.length - state.gears.length + 1;
}

class SpecGenerationState {
  readonly mode: SpecMode;
  readonly random: () => number;
  readonly targetCount: number;
  readonly gears: DraftGear[] = [];
  readonly edges: DraftMeshEdge[] = [];
  readonly edgeKeys = new Set<string>();
  readonly contactAnglesByGearId: ContactAngleMap = new Map();
  readonly degreeByGearId = new Map<string, number>();
  readonly neighborIdsByGearId = new Map<string, Set<string>>();
  readonly coverageSamples: CoverageSample[];
  readonly coveredSampleKeys = new Set<string>();
  readonly contourPhase: number;

  constructor(mode: SpecMode, random: () => number, targetCount: number) {
    this.mode = mode;
    this.random = random;
    this.targetCount = targetCount;
    this.contourPhase = random() * Math.PI * 2;
    this.coverageSamples = buildCoverageSamples(mode, this.contourPhase);
  }

  coverageProgress(): number {
    return this.coverageSamples.length === 0 ? 1 : this.coveredSampleKeys.size / this.coverageSamples.length;
  }

  addCoverageForGear(gear: DraftGear): void {
    for (const sample of this.coverageSamples) {
      if (gearTouchesSample(gear, sample)) this.coveredSampleKeys.add(sample.key);
    }
  }

  scoreCoverageGain(candidate: DraftGear): number {
    let gain = 0;
    for (const sample of this.coverageSamples) {
      if (this.coveredSampleKeys.has(sample.key)) continue;
      if (gearTouchesSample(candidate, sample)) gain += sample.weight;
    }
    return gain;
  }

  registerPlacement(gear: DraftGear, neighbors: DraftGear[]): void {
    this.gears.push(gear);
    this.degreeByGearId.set(gear.id, neighbors.length);
    this.neighborIdsByGearId.set(gear.id, new Set(neighbors.map((neighbor) => neighbor.id)));
    for (const neighbor of neighbors) {
      this.degreeByGearId.set(neighbor.id, (this.degreeByGearId.get(neighbor.id) ?? 0) + 1);
      if (!this.neighborIdsByGearId.has(neighbor.id)) this.neighborIdsByGearId.set(neighbor.id, new Set());
      this.neighborIdsByGearId.get(neighbor.id)!.add(gear.id);
    }
    registerMeshContacts({
      gear,
      neighbors,
      contactAnglesByGearId: this.contactAnglesByGearId,
      edges: this.edges,
      edgeKeys: this.edgeKeys,
    });
    this.addCoverageForGear(gear);
  }

  bounds() {
    return {
      minX: Math.min(...this.gears.map((gear) => gear.center.x - gear.outerRadius)),
      maxX: Math.max(...this.gears.map((gear) => gear.center.x + gear.outerRadius)),
      minY: Math.min(...this.gears.map((gear) => gear.center.y - gear.outerRadius)),
      maxY: Math.max(...this.gears.map((gear) => gear.center.y + gear.outerRadius)),
    };
  }

  scoreExpansionNeed(candidate: DraftGear): number {
    if (this.gears.length === 0) return 0;
    const bounds = this.bounds();
    let score = 0;
    if (candidate.center.x - candidate.outerRadius < bounds.minX + 100) score += 16;
    if (candidate.center.x + candidate.outerRadius > bounds.maxX - 100) score += 16;
    if (candidate.center.y - candidate.outerRadius < bounds.minY + 90) score += 9;
    if (candidate.center.y + candidate.outerRadius > bounds.maxY - 80) score += 4;
    return score;
  }

  futureBridgePotential(candidate: DraftGear, neighbors: DraftGear[]): number {
    let score = 0;
    for (const other of this.gears) {
      if (other.parity !== candidate.parity) continue;
      if (neighbors.some((neighbor) => neighbor.id === other.id)) continue;
      if (this.edgeKeys.has(meshEdgeKey(candidate.id, other.id))) continue;

      const distance = dist(candidate.center, other.center);
      const { minReach, maxReach } = pitchDistanceRange(candidate, other);
      if (distance < minReach - 0.2 || distance > maxReach + 0.2) continue;

      const shared = countSharedNeighbors(neighbors.map((neighbor) => neighbor.id), this.neighborIdsByGearId.get(other.id));
      const degree = this.degreeByGearId.get(other.id) ?? 0;
      const leafBonus = degree <= 1 ? 62 : degree === 2 ? 26 : 0;
      score += Math.max(0, 340 - distance) * 0.46 + shared * 92 + leafBonus;
    }

    return score;
  }

  candidateFromParent(parent: DraftGear, angle: number, teeth: number, appearIndex = this.gears.length): DraftGear {
    const pitchRadius = pitchRadiusFromTeeth(teeth, HERO_GEAR_CIRCULAR_PITCH);
    return {
      id: `hero-g${appearIndex}`,
      teeth,
      pitchRadius,
      outerRadius: outerRadiusFromTeeth(teeth),
      center: pointAt(parent.center, angle, parent.pitchRadius + pitchRadius),
      phaseTurn: solveNeighborPhaseTurn({
        currentTeeth: parent.teeth,
        neighborTeeth: teeth,
        currentTurn: parent.phaseTurn ?? 0,
        contactAngleCurrentToNeighbor: angle,
      }),
      parity: parent.parity === 0 ? 1 : 0,
      parentId: parent.id,
      appearIndex,
    };
  }

  parentAngles(parent: DraftGear): number[] {
    return parentAnglesFrom(parent, this.contactAnglesByGearId, this.random);
  }

  buildPairPlans(limit: number): PairPlan[] {
    const plans: PairPlan[] = [];

    for (let i = 0; i < this.gears.length; i += 1) {
      const a = this.gears[i];
      for (let j = i + 1; j < this.gears.length; j += 1) {
        const b = this.gears[j];
        if (a.parity !== b.parity) continue;
        if (this.edgeKeys.has(meshEdgeKey(a.id, b.id))) continue;

        const distance = dist(a.center, b.center);
        const { minReach, maxReach } = pitchDistanceRange(a, b);
        if (distance < minReach - 0.2 || distance > maxReach + 0.2) continue;

        const midpoint = { x: (a.center.x + b.center.x) * 0.5, y: (a.center.y + b.center.y) * 0.5 };
        const shared = countSharedNeighbors(this.neighborIdsByGearId.get(a.id) ?? [], this.neighborIdsByGearId.get(b.id));
        const degreeA = this.degreeByGearId.get(a.id) ?? 0;
        const degreeB = this.degreeByGearId.get(b.id) ?? 0;
        const leafBonus = (degreeA <= 1 ? 110 : degreeA === 2 ? 32 : 0) + (degreeB <= 1 ? 110 : degreeB === 2 ? 32 : 0);
        const midpointBias = Math.max(0, 300 - Math.abs(midpoint.y - 220)) * 0.08;
        const contourBias = Math.max(0, 90 - Math.abs(midpoint.y - bottomContourY(midpoint.x, this.mode, this.contourPhase))) * 0.22;
        plans.push({
          a,
          b,
          score: shared * 220 + leafBonus + midpointBias + contourBias + Math.max(0, 340 - distance) * 0.28 + this.random() * 4,
        });
      }
    }

    plans.sort((a, b) => b.score - a.score);
    return plans.slice(0, limit);
  }
}

function registerStandaloneGear(state: SpecGenerationState, gear: DraftGear): void {
  state.gears.push(gear);
  state.degreeByGearId.set(gear.id, 0);
  state.neighborIdsByGearId.set(gear.id, new Set());
  state.contactAnglesByGearId.set(gear.id, []);
  state.addCoverageForGear(gear);
}

function bridgeCandidate(
  state: SpecGenerationState,
  plan: PairPlan,
  extraGears: DraftGear[] = [],
  contactAnglesByGearId = state.contactAnglesByGearId,
): Placement | null {
  const config = MODE_CONFIG[state.mode];
  let best: Placement | null = null;

  for (const teeth of bridgeTeethOrder(plan, state.random)) {
    for (const option of getTwoParentMeshedIntersections({ parentA: plan.a, parentB: plan.b, teeth })) {
      const pitchRadius = pitchRadiusFromTeeth(teeth, HERO_GEAR_CIRCULAR_PITCH);
      const candidate: DraftGear = {
        id: `hero-g${state.gears.length + extraGears.length}`,
        teeth,
        pitchRadius,
        outerRadius: outerRadiusFromTeeth(teeth),
        center: option.center,
        phaseTurn: solveNeighborPhaseTurn({
          currentTeeth: plan.a.teeth,
          neighborTeeth: teeth,
          currentTurn: plan.a.phaseTurn ?? 0,
          contactAngleCurrentToNeighbor: option.contactAngleFromA,
        }),
        parity: plan.a.parity === 0 ? 1 : 0,
        parentId: plan.a.id,
        appearIndex: state.gears.length + extraGears.length,
      };

      const verdict = evaluatePlacement(candidate, [...state.gears, ...extraGears], contactAnglesByGearId, undefined, false);
      if (!verdict.ok || verdict.neighbors.length < 2) continue;
      if (!verdict.neighbors.some((neighbor) => neighbor.id === plan.a.id)) continue;
      if (!verdict.neighbors.some((neighbor) => neighbor.id === plan.b.id)) continue;

      const extraNeighbors = Math.max(0, verdict.neighbors.length - 2);
      const coverageGain = state.scoreCoverageGain(candidate);
      const futureBridge = state.futureBridgePotential(candidate, verdict.neighbors) * 0.45;
      const score =
        plan.score +
        config.bridgeBias +
        verdict.neighbors.length * 120 +
        extraNeighbors * 160 +
        coverageGain * 6 +
        futureBridge +
        scoreNearMeshMassage(state, candidate, verdict.neighbors) * 0.45 +
        state.random() * 4;

      if (!best || score > best.score) best = { gear: candidate, neighbors: verdict.neighbors, score };
      if (verdict.neighbors.length >= 4) return { gear: candidate, neighbors: verdict.neighbors, score };
    }
  }

  return best;
}

function tryAnchorLoopSegment(
  state: SpecGenerationState,
  prevEven: DraftGear,
  direction: -1 | 1,
  segmentIndex: number,
): { anchor: DraftGear; bridges: [Placement, Placement]; score: number } | null {
  const config = MODE_CONFIG[state.mode];
  const anchorTeethList = buildCandidateTeeth(state.random, prevEven.teeth, state.mode).slice(0, 9);
  const waveSign = segmentIndex % 2 === 0 ? -1 : 1;
  let best: { anchor: DraftGear; bridges: [Placement, Placement]; score: number } | null = null;

  for (const anchorTeeth of anchorTeethList) {
    const anchorPitchRadius = pitchRadiusFromTeeth(anchorTeeth, HERO_GEAR_CIRCULAR_PITCH);
    for (const bridgeTeeth of buildCandidateTeeth(state.random, anchorTeeth, state.mode).slice(0, 8)) {
      const dxOptions = [132, 154, 176, 198, 220];
      const dyOptions = [waveSign * 54, waveSign * 82, waveSign * 112, waveSign * 68 + (state.random() - 0.5) * 18];

      for (const dxBase of dxOptions) {
        for (const dyBase of dyOptions) {
          const anchor: DraftGear = {
            id: `hero-g${state.gears.length}`,
            teeth: anchorTeeth,
            pitchRadius: anchorPitchRadius,
            outerRadius: outerRadiusFromTeeth(anchorTeeth),
            center: {
              x: prevEven.center.x + direction * (dxBase + state.random() * 24),
              y: prevEven.center.y + dyBase + (state.random() - 0.5) * 22,
            },
            phaseTurn: prevEven.phaseTurn ?? 0,
            parity: prevEven.parity,
            parentId: prevEven.id,
            appearIndex: state.gears.length,
          };

          const anchorVerdict = evaluatePlacement(anchor, state.gears, state.contactAnglesByGearId, undefined, false);
          if (!anchorVerdict.ok || anchorVerdict.neighbors.length > 0) continue;

          const options = getTwoParentMeshedIntersections({ parentA: prevEven, parentB: anchor, teeth: bridgeTeeth });
          if (options.length < 2) continue;

          for (const order of [options, [options[1], options[0]]]) {
            const tempContacts = cloneContactAngleMap(state.contactAnglesByGearId);
            tempContacts.set(anchor.id, []);

            const firstBridge: DraftGear = {
              id: `hero-g${state.gears.length + 1}`,
              teeth: bridgeTeeth,
              pitchRadius: pitchRadiusFromTeeth(bridgeTeeth, HERO_GEAR_CIRCULAR_PITCH),
              outerRadius: outerRadiusFromTeeth(bridgeTeeth),
              center: order[0].center,
              phaseTurn: solveNeighborPhaseTurn({
                currentTeeth: prevEven.teeth,
                neighborTeeth: bridgeTeeth,
                currentTurn: prevEven.phaseTurn ?? 0,
                contactAngleCurrentToNeighbor: order[0].contactAngleFromA,
              }),
              parity: prevEven.parity === 0 ? 1 : 0,
              parentId: prevEven.id,
              appearIndex: state.gears.length + 1,
            };

            anchor.phaseTurn = solveNeighborPhaseTurn({
              currentTeeth: firstBridge.teeth,
              neighborTeeth: anchor.teeth,
              currentTurn: firstBridge.phaseTurn ?? 0,
              contactAngleCurrentToNeighbor: Math.atan2(
                anchor.center.y - firstBridge.center.y,
                anchor.center.x - firstBridge.center.x,
              ),
            });

            const firstVerdict = evaluatePlacement(firstBridge, [...state.gears, anchor], tempContacts, undefined, false);
            if (!firstVerdict.ok) continue;
            if (!firstVerdict.neighbors.some((neighbor) => neighbor.id === prevEven.id)) continue;
            if (!firstVerdict.neighbors.some((neighbor) => neighbor.id === anchor.id)) continue;

            registerMeshContacts({ gear: firstBridge, neighbors: firstVerdict.neighbors, contactAnglesByGearId: tempContacts });

            const secondBridge: DraftGear = {
              id: `hero-g${state.gears.length + 2}`,
              teeth: bridgeTeeth,
              pitchRadius: firstBridge.pitchRadius,
              outerRadius: firstBridge.outerRadius,
              center: order[1].center,
              phaseTurn: solveNeighborPhaseTurn({
                currentTeeth: prevEven.teeth,
                neighborTeeth: bridgeTeeth,
                currentTurn: prevEven.phaseTurn ?? 0,
                contactAngleCurrentToNeighbor: order[1].contactAngleFromA,
              }),
              parity: firstBridge.parity,
              parentId: prevEven.id,
              appearIndex: state.gears.length + 2,
            };

            const secondVerdict = evaluatePlacement(secondBridge, [...state.gears, anchor, firstBridge], tempContacts, undefined, false);
            if (!secondVerdict.ok) continue;
            if (!secondVerdict.neighbors.some((neighbor) => neighbor.id === prevEven.id)) continue;
            if (!secondVerdict.neighbors.some((neighbor) => neighbor.id === anchor.id)) continue;

            const progress = forwardProgress(direction, prevEven.center, anchor.center);
            const midpointY = (prevEven.center.y + anchor.center.y) * 0.5;
            const sideA = Math.sign(firstBridge.center.y - midpointY);
            const sideB = Math.sign(secondBridge.center.y - midpointY);
            const score =
              progress * 2.1 +
              Math.abs(firstBridge.center.y - secondBridge.center.y) * 0.4 +
              (sideA !== 0 && sideB !== 0 && sideA !== sideB ? 90 : -60) -
              Math.abs(anchor.center.y - config.rootY) * 0.12 +
              state.scoreCoverageGain(anchor) * 4 +
              state.random() * 4;

            if (!best || score > best.score) {
              best = {
                anchor: { ...anchor },
                bridges: [
                  { gear: firstBridge, neighbors: firstVerdict.neighbors, score },
                  { gear: secondBridge, neighbors: secondVerdict.neighbors, score },
                ],
                score,
              };
            }
          }
        }
      }
    }
  }

  return best;
}

function tryDirectionalExpansion(
  state: SpecGenerationState,
  parent: DraftGear,
  direction: -1 | 1,
  targetAngle: number,
  appearIndex = state.gears.length,
): Placement | null {
  const teethList = buildCandidateTeeth(state.random, parent.teeth, state.mode).slice(0, state.mode === "constraint-solver" ? 9 : 7);
  let best: Placement | null = null;

  for (const angle of state.parentAngles(parent)) {
    const angleBias = Math.abs(angleDelta(angle, targetAngle));
    if (angleBias > 1.6) continue;

    for (const teeth of teethList) {
      const candidate = state.candidateFromParent(parent, angle, teeth, appearIndex);
      const verdict = evaluatePlacement(candidate, state.gears, state.contactAnglesByGearId, parent.id, false);
      if (!verdict.ok || verdict.neighbors.length === 0) continue;

      const score =
        forwardProgress(direction, parent.center, candidate.center) * 0.9 -
        angleBias * 70 -
        Math.abs(candidate.center.y - MODE_CONFIG[state.mode].rootY) * 0.08 +
        state.scoreCoverageGain(candidate) * 5 +
        state.futureBridgePotential(candidate, verdict.neighbors) * 1.15 +
        scoreNearMeshMassage(state, candidate, verdict.neighbors) * 0.6 +
        verdict.neighbors.length * 24 +
        state.random() * 3;

      if (!best || score > best.score) best = { gear: candidate, neighbors: verdict.neighbors, score };
    }
  }

  return best;
}

function tryBiasedChildPlacement(options: {
  state: SpecGenerationState;
  parent: DraftGear;
  targetAngle: number;
  extraGears?: DraftGear[];
  contactAnglesByGearId?: ContactAngleMap;
  appearIndex: number;
}): Placement | null {
  const { state, parent, targetAngle, extraGears = [], contactAnglesByGearId = state.contactAnglesByGearId, appearIndex } = options;
  const teethList = buildCandidateTeeth(state.random, parent.teeth, state.mode).slice(0, 7);
  let best: Placement | null = null;

  for (const angle of parentAnglesFrom(parent, contactAnglesByGearId, state.random)) {
    const angleBias = Math.abs(angleDelta(angle, targetAngle));
    if (angleBias > 1.25) continue;

    for (const teeth of teethList) {
      const candidate = state.candidateFromParent(parent, angle, teeth, appearIndex);
      const verdict = evaluatePlacement(candidate, [...state.gears, ...extraGears], contactAnglesByGearId, parent.id, false);
      if (!verdict.ok || verdict.neighbors.length === 0) continue;

      const score =
        -angleBias * 80 +
        state.futureBridgePotential(candidate, verdict.neighbors) * 0.9 +
        scoreNearMeshMassage(state, candidate, verdict.neighbors) * 0.7 +
        verdict.neighbors.length * 24 +
        state.random() * 2;

      if (!best || score > best.score) best = { gear: candidate, neighbors: verdict.neighbors, score };
    }
  }

  return best;
}

function tryTwinLoopStep(
  state: SpecGenerationState,
  anchor: DraftGear,
  direction: -1 | 1,
  segmentIndex: number,
): { children: [Placement, Placement]; nextEven: Placement; score: number } | null {
  const base = direction > 0 ? 0 : Math.PI;
  const wave = segmentIndex % 2 === 0 ? 0.76 : 0.56;
  const first = tryBiasedChildPlacement({
    state,
    parent: anchor,
    targetAngle: base - wave,
    appearIndex: state.gears.length,
  });
  if (!first) return null;

  const tempContacts = cloneContactAngleMap(state.contactAnglesByGearId);
  registerMeshContacts({ gear: first.gear, neighbors: first.neighbors, contactAnglesByGearId: tempContacts });

  let best: { children: [Placement, Placement]; nextEven: Placement; score: number } | null = null;
  const targetAngle = base + wave;
  const secondTeethList = buildCandidateTeeth(state.random, anchor.teeth, state.mode).slice(0, 7);

  for (const angle of parentAnglesFrom(anchor, tempContacts, state.random)) {
    const angleBias = Math.abs(angleDelta(angle, targetAngle));
    if (angleBias > 1.25) continue;

    for (const teeth of secondTeethList) {
      const secondGear = state.candidateFromParent(anchor, angle, teeth, state.gears.length + 1);
      const secondVerdict = evaluatePlacement(secondGear, [...state.gears, first.gear], tempContacts, anchor.id, false);
      if (!secondVerdict.ok || secondVerdict.neighbors.length === 0) continue;

      const secondContacts = cloneContactAngleMap(tempContacts);
      registerMeshContacts({ gear: secondGear, neighbors: secondVerdict.neighbors, contactAnglesByGearId: secondContacts });

      const plan: PairPlan = {
        a: first.gear,
        b: secondGear,
        score: 320 + forwardProgress(direction, anchor.center, { x: (first.gear.center.x + secondGear.center.x) * 0.5, y: anchor.center.y }) * 0.4,
      };
      const nextEven = bridgeCandidate(state, plan, [first.gear, secondGear], secondContacts);
      if (!nextEven) continue;

      const score =
        nextEven.neighbors.length * 180 +
        forwardProgress(direction, anchor.center, nextEven.gear.center) * 1.4 +
        scoreNearMeshMassage(state, nextEven.gear, nextEven.neighbors) * 0.6 -
        angleBias * 40 +
        state.random() * 2;

      if (!best || score > best.score) {
        best = {
          children: [first, { gear: secondGear, neighbors: secondVerdict.neighbors, score }],
          nextEven: { ...nextEven, score },
          score,
        };
      }
    }
  }

  return best;
}

function tryLoopChainStep(state: SpecGenerationState, cursor: LoopChainCursor):
  | { nextEven: Placement; bridgeOdd: Placement; nextCursor: LoopChainCursor; score: number }
  | null {
  const { prevEven, carryOdd, direction } = cursor;
  const verticalSign = Math.sign(carryOdd.center.y - prevEven.center.y) || direction;
  const targetAngle = (direction > 0 ? 0 : Math.PI) - verticalSign * 0.72;
  let best: { nextEven: Placement; bridgeOdd: Placement; nextCursor: LoopChainCursor; score: number } | null = null;

  for (const angle of state.parentAngles(carryOdd)) {
    const angleBias = Math.abs(angleDelta(angle, targetAngle));
    if (angleBias > 1.25) continue;

    for (const teeth of buildCandidateTeeth(state.random, carryOdd.teeth, state.mode).slice(0, 8)) {
      const evenCandidate = state.candidateFromParent(carryOdd, angle, teeth, state.gears.length);
      const evenVerdict = evaluatePlacement(evenCandidate, state.gears, state.contactAnglesByGearId, carryOdd.id, false);
      if (!evenVerdict.ok || evenVerdict.neighbors.length === 0) continue;
      if (evenCandidate.parity !== prevEven.parity) continue;

      const progress = forwardProgress(direction, prevEven.center, evenCandidate.center);
      if (progress < 36) continue;

      const bridgeContacts = cloneContactAngleMap(state.contactAnglesByGearId);
      registerMeshContacts({ gear: evenCandidate, neighbors: evenVerdict.neighbors, contactAnglesByGearId: bridgeContacts });

      const bridgePlan: PairPlan = {
        a: prevEven,
        b: evenCandidate,
        score: 220 + progress * 0.6 - angleBias * 45,
      };
      const bridgePlacement = bridgeCandidate(state, bridgePlan, [evenCandidate], bridgeContacts);
      if (!bridgePlacement) continue;

      const midpointY = (prevEven.center.y + evenCandidate.center.y) * 0.5;
      const carrySide = Math.sign(carryOdd.center.y - midpointY);
      const bridgeSide = Math.sign(bridgePlacement.gear.center.y - midpointY);
      const sideBonus = carrySide === 0 || bridgeSide === 0 ? 0 : carrySide !== bridgeSide ? 70 : -25;
      const score =
        progress * 1.6 +
        bridgePlacement.neighbors.length * 120 +
        evenVerdict.neighbors.length * 30 +
        sideBonus -
        angleBias * 70 -
        Math.abs(evenCandidate.center.y - MODE_CONFIG[state.mode].rootY) * 0.08 +
        scoreNearMeshMassage(state, evenCandidate, evenVerdict.neighbors) * 0.5 +
        state.random() * 3;

      const nextCursor: LoopChainCursor = {
        prevEven: evenCandidate,
        carryOdd: bridgePlacement.gear,
        direction,
      };

      if (!best || score > best.score) {
        best = {
          nextEven: { gear: evenCandidate, neighbors: evenVerdict.neighbors, score },
          bridgeOdd: bridgePlacement,
          nextCursor,
          score,
        };
      }
    }
  }

  return best;
}

function seedTopologyChain(state: SpecGenerationState): void {
  const config = MODE_CONFIG[state.mode];
  const rootTeeth = 16 + randInt(state.random, 0, 3);
  const root: DraftGear = {
    id: "hero-g0",
    teeth: rootTeeth,
    pitchRadius: pitchRadiusFromTeeth(rootTeeth, HERO_GEAR_CIRCULAR_PITCH),
    outerRadius: outerRadiusFromTeeth(rootTeeth),
    center: {
      x: VIEWBOX.width * 0.42 + (state.random() - 0.5) * 84,
      y: config.rootY + (state.random() - 0.5) * 18,
    },
    phaseTurn: 0,
    parity: 0,
    appearIndex: 0,
  };

  registerStandaloneGear(state, root);

  const branches: Array<{ direction: -1 | 1; prevEven: DraftGear }> = [
    { direction: 1, prevEven: root },
    { direction: -1, prevEven: root },
  ];

  let segmentIndex = 0;
  while (state.gears.length < Math.min(state.targetCount, 40) && segmentIndex < 16) {
    segmentIndex += 1;
    let progressed = false;

    for (const branch of branches) {
      const loop = tryTwinLoopStep(state, branch.prevEven, branch.direction, segmentIndex);
      if (!loop) continue;
      state.registerPlacement(loop.children[0].gear, loop.children[0].neighbors);
      state.registerPlacement(loop.children[1].gear, loop.children[1].neighbors);
      state.registerPlacement(loop.nextEven.gear, loop.nextEven.neighbors);
      branch.prevEven = loop.nextEven.gear;
      progressed = true;
      if (state.gears.length >= Math.min(state.targetCount, 40)) break;
    }

    if (!progressed) {
      for (const branch of branches) {
        const direction = branch.direction;
        const targetAngle = direction > 0 ? (segmentIndex % 2 === 0 ? -0.52 : 0.62) : Math.PI + (segmentIndex % 2 === 0 ? 0.52 : -0.62);
        const carry = tryDirectionalExpansion(state, branch.prevEven, direction, targetAngle, state.gears.length);
        if (!carry) continue;
        state.registerPlacement(carry.gear, carry.neighbors);
        progressed = true;
      }
    }

    if (!progressed) break;
  }
}

function scoreShape(state: SpecGenerationState, candidate: DraftGear, neighbors: DraftGear[], parent: DraftGear): number {
  const sizeContrast = Math.abs(candidate.teeth - parent.teeth) * 1.3;
  const centerBias = -Math.abs(candidate.center.x - VIEWBOX.width * 0.5) * 0.0032;
  const heightBias = -Math.abs(candidate.center.y - 220) * 0.008;
  const contourGap = Math.abs(candidate.center.y + candidate.outerRadius - bottomContourY(candidate.center.x, state.mode, state.contourPhase));
  const contourBias = contourGap < 84 ? (84 - contourGap) * 0.2 : -Math.min(18, (contourGap - 84) * 0.08);
  const neighborBonus = neighbors.length >= 2 ? neighbors.length * 26 : 0;
  return sizeContrast + centerBias + heightBias + contourBias + neighborBonus;
}

function tryBestExpansion(state: SpecGenerationState, parent: DraftGear): Placement | null {
  const config = MODE_CONFIG[state.mode];
  const teethList = buildCandidateTeeth(state.random, parent.teeth, state.mode);
  const progress = state.coverageProgress();
  const bounds = state.gears.length > 0 ? state.bounds() : null;
  const xSpan = bounds ? bounds.maxX - bounds.minX : 0;
      const minNeighbors =
    state.mode === "constraint-solver"
      ? state.gears.length >= Math.max(54, Math.floor(state.targetCount * 0.75)) && cycleRankOf(state) >= Math.max(10, Math.floor(state.gears.length / 5))
        ? 2
        : 1
      : progress > 0.62 && xSpan > 1450
        ? 2
        : 1;
  let best: Placement | null = null;

  for (const angle of state.parentAngles(parent)) {
    for (const teeth of teethList) {
      const candidate = state.candidateFromParent(parent, angle, teeth);
      const verdict = evaluatePlacement(candidate, state.gears, state.contactAnglesByGearId, parent.id, false);
      if (!verdict.ok || verdict.neighbors.length < minNeighbors) continue;

      const coverageGain = state.scoreCoverageGain(candidate);
      const futureBridge = state.futureBridgePotential(candidate, verdict.neighbors) * config.futureBridgeWeight;
      const nearMeshMassage = scoreNearMeshMassage(state, candidate, verdict.neighbors);
      const loopBias = verdict.neighbors.length >= 2 ? config.denseNeighborWeight + verdict.neighbors.length * 60 : progress > 0.72 ? -24 : 0;
      const parentDegree = state.degreeByGearId.get(parent.id) ?? 0;
      const leafPenalty = verdict.neighbors.length < 2 && progress > 0.38 ? config.leafPenalty : 0;
      const score =
        coverageGain * config.expansionCoverageWeight +
        futureBridge +
        nearMeshMassage +
        loopBias +
        state.scoreExpansionNeed(candidate) +
        scoreShape(state, candidate, verdict.neighbors, parent) -
        parentDegree * 6 -
        leafPenalty -
        (progress > 0.82 && verdict.neighbors.length < 2 ? 60 : 0) +
        state.random() * 5;

      if (!best || score > best.score) best = { gear: candidate, neighbors: verdict.neighbors, score };
      if (verdict.neighbors.length >= 3 && futureBridge + nearMeshMassage > 56) {
        return { gear: candidate, neighbors: verdict.neighbors, score };
      }
    }
  }

  return best;
}

function tryBestBridge(state: SpecGenerationState, plan: PairPlan): Placement | null {
  return bridgeCandidate(state, plan);
}

function closureSweep(state: SpecGenerationState, limit: number, passes: number): void {
  let remaining = passes;
  while (remaining > 0 && state.gears.length < state.targetCount) {
    remaining -= 1;
    let best: Placement | null = null;
    for (const plan of state.buildPairPlans(limit)) {
      const placement = tryBestBridge(state, plan);
      if (!placement) continue;
      if (!best || placement.score > best.score) best = placement;
      if (placement.neighbors.length >= 4) break;
    }
    if (!best) break;
    state.registerPlacement(best.gear, best.neighbors);
  }
}

function parentPool(state: SpecGenerationState): DraftGear[] {
  const progress = state.coverageProgress();
  return state.gears
    .map((gear) => {
      const degree = state.degreeByGearId.get(gear.id) ?? 0;
      const edgeBias = state.scoreExpansionNeed(gear);
      const leafBias = degree <= 1 ? 28 : degree === 2 ? 12 : degree === 3 ? 3 : -8;
      const closureBias = state.mode === "constraint-solver" ? (degree <= 2 ? 24 : 0) : 0;
      const futureBias = progress < 0.55 ? edgeBias + leafBias * 0.5 : leafBias * 1.5 + closureBias;
      return { gear, score: futureBias - degree * (state.mode === "constraint-solver" ? 5 : 7) + state.random() * 4 };
    })
    .sort((a, b) => b.score - a.score)
    .slice(0, Math.min(22, state.gears.length))
    .map((entry) => entry.gear);
}

function seedMode(state: SpecGenerationState): void {
  seedTopologyChain(state);

  if (state.mode !== "constraint-solver") return;

  let warmup = 0;
  while (state.gears.length < Math.min(state.targetCount, 22) && warmup < 24) {
    warmup += 1;
    let best: Placement | null = null;

    for (const plan of state.buildPairPlans(28)) {
      const placement = tryBestBridge(state, plan);
      if (!placement) continue;
      if (!best || placement.score > best.score) best = placement;
      if (placement.neighbors.length >= 4) break;
    }

    if (!best) {
      for (const parent of parentPool(state)) {
        const placement = tryBestExpansion(state, parent);
        if (!placement) continue;
        if (!best || placement.score > best.score) best = placement;
      }
    }

    if (!best) break;
    state.registerPlacement(best.gear, best.neighbors);
  }
}

function assignPhaseTurns(gears: DraftGear[], edges: DraftMeshEdge[]): boolean {
  const byId = new Map(gears.map((gear) => [gear.id, gear]));
  const adjacency = new Map<string, Array<{ other: string; angle: number }>>();

  for (const edge of edges) {
    const a = byId.get(edge.a);
    const b = byId.get(edge.b);
    if (!a || !b) return false;
    if (!adjacency.has(a.id)) adjacency.set(a.id, []);
    if (!adjacency.has(b.id)) adjacency.set(b.id, []);
    const angle = Math.atan2(b.center.y - a.center.y, b.center.x - a.center.x);
    adjacency.get(a.id)!.push({ other: b.id, angle });
    adjacency.get(b.id)!.push({ other: a.id, angle: angle + Math.PI });
  }

  const assigned = new Map<string, number>();
  for (const gear of gears) {
    if (assigned.has(gear.id)) continue;
    assigned.set(gear.id, normalizeTurn(gear.phaseTurn ?? 0));
    const queue = [gear.id];

    while (queue.length > 0) {
      const currentId = queue.shift();
      if (!currentId) continue;
      const current = byId.get(currentId);
      const currentTurn = assigned.get(currentId);
      if (!current || currentTurn == null) return false;

      for (const neighbor of adjacency.get(currentId) ?? []) {
        const neighborGear = byId.get(neighbor.other);
        if (!neighborGear) return false;
        const required = solveNeighborPhaseTurn({
          currentTeeth: current.teeth,
          neighborTeeth: neighborGear.teeth,
          currentTurn,
          contactAngleCurrentToNeighbor: neighbor.angle,
        });
        const existing = assigned.get(neighbor.other);
        if (existing == null) {
          assigned.set(neighbor.other, required);
          queue.push(neighbor.other);
          continue;
        }
        const delta = Math.abs(normalizeTurn(existing - required));
        const wrapped = Math.min(delta, 1 - delta);
        if (wrapped > 0.03) return false;
      }
    }
  }

  for (const gear of gears) gear.phaseTurn = assigned.get(gear.id) ?? 0;
  return true;
}

function normalizeHorizontalPlacement(gears: DraftGear[]): void {
  if (gears.length === 0) return;
  const minX = Math.min(...gears.map((gear) => gear.center.x - gear.outerRadius));
  const maxX = Math.max(...gears.map((gear) => gear.center.x + gear.outerRadius));
  let dx = 0;
  if (maxX < 1220) dx = 1220 - maxX;
  if (minX + dx > 80) dx += 80 - (minX + dx);
  if (Math.abs(dx) < 1e-6) return;
  for (const gear of gears) gear.center.x += dx;
}

function generateMode(options: BackdropGeneratorOptions & { mode: SpecMode }): BackdropGeneratorResult {
  const targetCount = options.targetCount ?? 72;
  const config = MODE_CONFIG[options.mode];
  const { random } = createGenerationContext(options.seed, config.seedSalt);
  const state = new SpecGenerationState(options.mode, random, targetCount);

  seedMode(state);

  let attempts = 0;
  const maxAttempts = Math.max(1200, targetCount * config.maxAttemptsFactor);
  while (state.gears.length < targetCount && attempts < maxAttempts) {
    attempts += 1;

    let bestBridge: Placement | null = null;
    const pairPlans = state.buildPairPlans(options.mode === "constraint-solver" ? 120 : 96);
    for (const plan of pairPlans) {
      const placement = tryBestBridge(state, plan);
      if (!placement) continue;
      if (!bestBridge || placement.score > bestBridge.score) bestBridge = placement;
      if (placement.neighbors.length >= 4) break;
    }

    let bestExpansion: Placement | null = null;
    for (const parent of parentPool(state)) {
      const placement = tryBestExpansion(state, parent);
      if (!placement) continue;
      if (!bestExpansion || placement.score > bestExpansion.score) bestExpansion = placement;
      if (placement.neighbors.length >= 3) break;
    }

    const desiredCycleRank =
      options.mode === "constraint-solver"
        ? Math.max(3, Math.floor(state.gears.length / 4))
        : Math.max(2, Math.floor(state.gears.length / 6));
    const needsClosure = cycleRankOf(state) < desiredCycleRank;
    const nearTarget = state.gears.length >= targetCount - (options.mode === "constraint-solver" ? 18 : 14);

    const chooseBridge =
      bestBridge != null &&
      (bestExpansion == null ||
        (options.mode === "constraint-solver" && (needsClosure || state.coverageProgress() > 0.56)) ||
        nearTarget ||
        needsClosure ||
        state.coverageProgress() > 0.28 ||
        bestBridge.score >= bestExpansion.score - (options.mode === "topology-first" ? 28 : 8));

    const chosen = chooseBridge ? bestBridge : bestExpansion ?? bestBridge;
    if (!chosen) continue;
    state.registerPlacement(chosen.gear, chosen.neighbors);

    if (options.mode === "constraint-solver" || needsClosure) {
      let closureBursts = 0;
      while (state.gears.length < targetCount && closureBursts < (options.mode === "constraint-solver" ? 2 : 2)) {
        closureBursts += 1;
        let closure: Placement | null = null;
        for (const plan of state.buildPairPlans(options.mode === "constraint-solver" ? 90 : 72)) {
          const placement = tryBestBridge(state, plan);
          if (!placement) continue;
          if (!closure || placement.score > closure.score) closure = placement;
          if (placement.neighbors.length >= 4) break;
        }
        if (!closure || (!needsClosure && bestExpansion && closure.score < bestExpansion.score + 26)) break;
        state.registerPlacement(closure.gear, closure.neighbors);
      }
    }
  }

  let densifyPass = 0;
  while (state.gears.length < targetCount && densifyPass < (options.mode === "constraint-solver" ? 56 : 38)) {
    densifyPass += 1;
    let best: Placement | null = null;
    for (const plan of state.buildPairPlans(options.mode === "constraint-solver" ? 140 : 120)) {
      const placement = tryBestBridge(state, plan);
      if (!placement) continue;
      if (!best || placement.score > best.score) best = placement;
      if (placement.neighbors.length >= 4) break;
    }
    if (best) {
      state.registerPlacement(best.gear, best.neighbors);
      continue;
    }

    if (state.gears.length >= targetCount - (options.mode === "constraint-solver" ? 14 : 10)) break;

    let expansion: Placement | null = null;
    for (const parent of parentPool(state)) {
      const placement = tryBestExpansion(state, parent);
      if (!placement) continue;
      if (!expansion || placement.score > expansion.score) expansion = placement;
    }
    if (!expansion) break;
    state.registerPlacement(expansion.gear, expansion.neighbors);
  }

  closureSweep(state, options.mode === "constraint-solver" ? 180 : 140, options.mode === "constraint-solver" ? 8 : 6);

  assignPhaseTurns(state.gears, state.edges);
  normalizeHorizontalPlacement(state.gears);

  return { gears: state.gears, edges: state.edges };
}

export function generateFastSpecBackdrop(
  options: BackdropGeneratorOptions & { mode: SpecMode },
): BackdropGeneratorResult {
  return generateMode(options);
}

export function debugFastSpecBackdropAttempts(
  options: BackdropGeneratorOptions & { mode: SpecMode; attempts?: number },
) {
  const attempts = options.attempts ?? 4;
  const diagnostics: Array<{ attempt: number; gears: number; edges: number }> = [];
  for (let attempt = 0; attempt < attempts; attempt += 1) {
    const result = generateFastSpecBackdrop({
      ...options,
      seed: options.seed + attempt * 97,
    });
    diagnostics.push({
      attempt,
      gears: result.gears.length,
      edges: result.edges.length,
    });
  }
  return diagnostics;
}
