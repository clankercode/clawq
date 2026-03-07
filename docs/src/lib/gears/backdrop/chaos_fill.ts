import { pitchRadiusFromTeeth } from "../solver.ts";
import type { BackdropGeneratorFn, DraftGear, DraftMeshEdge, Point } from "./types.ts";
import {
  HERO_GEAR_CIRCULAR_PITCH,
  MIN_TEETH,
  VIEWBOX,
  Y_MAX,
  Y_MIN,
  createGenerationContext,
  dist,
  evaluatePlacement,
  getLegalContactSlots,
  getTwoParentMeshedIntersections,
  outerRadiusFromTeeth,
  pickCandidateTeeth,
  pointAt,
  randInt,
  registerMeshContacts,
  solveNeighborPhaseTurn,
} from "./shared.ts";

const MAX_TEETH = 28;
const MAX_BRIDGE_PITCH_RADIUS = pitchRadiusFromTeeth(MAX_TEETH, HERO_GEAR_CIRCULAR_PITCH);
const COVERAGE_COLS = 18;
const COVERAGE_ROWS = 10;
const COVERAGE_MARGIN_X = 220;
const TARGET_TOP_Y = -170;
const TARGET_BOTTOM_Y = 336;
const ROOT_CENTER = { x: VIEWBOX.width * 0.5, y: 214 };

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

type CoverageSample = Point & {
  key: string;
  weight: number;
};

function edgeKey(a: string, b: string): string {
  return a < b ? `${a}|${b}` : `${b}|${a}`;
}

function angleDelta(a: number, b: number): number {
  return Math.atan2(Math.sin(a - b), Math.cos(a - b));
}

function shuffled<T>(items: T[], random: () => number): T[] {
  const next = items.slice();
  for (let i = next.length - 1; i > 0; i -= 1) {
    const j = Math.floor(random() * (i + 1));
    [next[i], next[j]] = [next[j], next[i]];
  }
  return next;
}

function bottomContourY(x: number, phase: number): number {
  return TARGET_BOTTOM_Y + Math.sin(x / 170 + phase) * 24 + Math.sin(x / 73 - phase * 0.7) * 11;
}

function buildCoverageSamples(phase: number): CoverageSample[] {
  const xMin = -COVERAGE_MARGIN_X;
  const xMax = VIEWBOX.width + COVERAGE_MARGIN_X;
  const yMin = TARGET_TOP_Y;
  const yMax = TARGET_BOTTOM_Y + 16;
  const xStep = (xMax - xMin) / (COVERAGE_COLS - 1);
  const yStep = (yMax - yMin) / (COVERAGE_ROWS - 1);
  const samples: CoverageSample[] = [];

  for (let row = 0; row < COVERAGE_ROWS; row += 1) {
    for (let col = 0; col < COVERAGE_COLS; col += 1) {
      const x = xMin + col * xStep;
      const y = yMin + row * yStep;
      const contourY = bottomContourY(x, phase);
      if (y > contourY) continue;
      const edgeX = Math.abs(col - (COVERAGE_COLS - 1) * 0.5) / ((COVERAGE_COLS - 1) * 0.5);
      const topBias = 1 - row / (COVERAGE_ROWS - 1);
      const contourBand = 1 - Math.min(1, Math.abs(contourY - y) / 88);
      samples.push({
        x,
        y,
        key: `${col}:${row}`,
        weight: 1 + edgeX * 0.9 + topBias * 0.95 + contourBand * 0.4,
      });
    }
  }

  return samples;
}

function gearTouchesSample(gear: DraftGear, sample: CoverageSample): boolean {
  return dist(gear.center, sample) <= gear.outerRadius + 42;
}

function buildCandidateTeethOrder(random: () => number, parentTeeth: number): number[] {
  const teeth = Array.from({ length: MAX_TEETH - MIN_TEETH + 1 }, (_, index) => index + MIN_TEETH);
  const bucketBias = (teethValue: number) => {
    if (teethValue <= 14) return -0.55;
    if (teethValue <= 18) return -0.18;
    if (teethValue <= 22) return 0.04;
    return 0.28;
  };

  return teeth
    .map((teethValue) => {
      const contrastBias = -Math.min(8, Math.abs(teethValue - parentTeeth)) * 0.03;
      return {
        teeth: teethValue,
        score: bucketBias(teethValue) + contrastBias + random() * 0.7,
      };
    })
    .sort((a, b) => a.score - b.score)
    .slice(0, 9)
    .map((item) => item.teeth);
}

function scoreCoverageGain(
  candidate: DraftGear,
  coverageSamples: CoverageSample[],
  coveredSampleKeys: Set<string>
): number {
  let gain = 0;
  for (const sample of coverageSamples) {
    if (coveredSampleKeys.has(sample.key)) continue;
    if (gearTouchesSample(candidate, sample)) gain += sample.weight;
  }
  return gain;
}

function scoreExpansionNeed(candidate: DraftGear, gears: DraftGear[]): number {
  const xMin = Math.min(...gears.map((gear) => gear.center.x - gear.outerRadius));
  const xMax = Math.max(...gears.map((gear) => gear.center.x + gear.outerRadius));
  const yMin = Math.min(...gears.map((gear) => gear.center.y - gear.outerRadius));
  const yMax = Math.max(...gears.map((gear) => gear.center.y + gear.outerRadius));

  let score = 0;
  if (candidate.center.x - candidate.outerRadius < xMin + 120) score += 18;
  if (candidate.center.x + candidate.outerRadius > xMax - 120) score += 18;
  if (candidate.center.y - candidate.outerRadius < yMin + 90) score += 11;
  if (candidate.center.y + candidate.outerRadius > yMax - 70) score += 5;
  return score;
}

function scoreShape(candidate: DraftGear, neighbors: DraftGear[], parent: DraftGear | undefined, contourPhase: number): number {
  const neighborMeanTeeth =
    neighbors.reduce((sum, neighbor) => sum + neighbor.teeth, 0) / Math.max(1, neighbors.length);
  const sizeContrast = Math.abs(candidate.teeth - neighborMeanTeeth);
  const centerBias = -Math.abs(candidate.center.x - VIEWBOX.width * 0.5) * 0.0035;
  const heightBias = -Math.abs(candidate.center.y - 195) * 0.008;
  const parentContrast = parent ? Math.abs(candidate.teeth - parent.teeth) * 1.4 : 0;
  const contourGap = Math.abs(candidate.center.y + candidate.outerRadius - bottomContourY(candidate.center.x, contourPhase));
  const contourBias = contourGap < 84 ? (84 - contourGap) * 0.2 : -Math.min(14, (contourGap - 84) * 0.08);
  return sizeContrast * 2.8 + parentContrast + centerBias + heightBias + contourBias;
}

function sharedNeighborCount(candidateNeighbors: DraftGear[], otherNeighborIds: Set<string> | undefined): number {
  if (!otherNeighborIds) return 0;
  let count = 0;
  for (const neighbor of candidateNeighbors) {
    if (otherNeighborIds.has(neighbor.id)) count += 1;
  }
  return count;
}

function addCoverageForGear(
  gear: DraftGear,
  coverageSamples: CoverageSample[],
  coveredSampleKeys: Set<string>
): void {
  for (const sample of coverageSamples) {
    if (gearTouchesSample(gear, sample)) coveredSampleKeys.add(sample.key);
  }
}

function seedAngles(random: () => number): number[] {
  const base = [0.12, 0.41, 0.78, 1.2, 1.55, 1.86].map((turn) => turn * Math.PI);
  return shuffled(
    base.map((angle) => angle + (random() - 0.5) * 0.24),
    random
  );
}

export const generateChaosFillBackdrop: BackdropGeneratorFn = ({ seed, targetCount = 108 }) => {
  const { random } = createGenerationContext(seed, 0x6c38f);
  const contourPhase = random() * Math.PI * 2;
  const gears: DraftGear[] = [];
  const edges: DraftMeshEdge[] = [];
  const edgeKeys = new Set<string>();
  const coverageSamples = buildCoverageSamples(contourPhase);
  const coveredSampleKeys = new Set<string>();
  const contactAnglesByGearId = new Map<string, number[]>();
  const degreeByGearId = new Map<string, number>();
  const neighborIdsByGearId = new Map<string, Set<string>>();

  function registerPlacement(gear: DraftGear, neighbors: DraftGear[]): void {
    gears.push(gear);
    contactAnglesByGearId.set(gear.id, contactAnglesByGearId.get(gear.id) ?? []);
    degreeByGearId.set(gear.id, neighbors.length);
    neighborIdsByGearId.set(gear.id, new Set(neighbors.map((neighbor) => neighbor.id)));
    for (const neighbor of neighbors) {
      degreeByGearId.set(neighbor.id, (degreeByGearId.get(neighbor.id) ?? 0) + 1);
      if (!neighborIdsByGearId.has(neighbor.id)) neighborIdsByGearId.set(neighbor.id, new Set());
      neighborIdsByGearId.get(neighbor.id)!.add(gear.id);
    }
    registerMeshContacts({ gear, neighbors, contactAnglesByGearId, edges, edgeKeys });
    addCoverageForGear(gear, coverageSamples, coveredSampleKeys);
  }

  function candidateFromParent(parent: DraftGear, angle: number, teeth: number): DraftGear {
    const pitchRadius = pitchRadiusFromTeeth(teeth, HERO_GEAR_CIRCULAR_PITCH);
    return {
      id: `hero-g${gears.length}`,
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
      appearIndex: gears.length,
    };
  }

  function parentAngles(parent: DraftGear): number[] {
    const contacts = contactAnglesByGearId.get(parent.id) ?? [];
    if (contacts.length === 0) {
      return shuffled(
        Array.from({ length: Math.max(12, Math.min(18, parent.teeth)) }, (_, index) => {
          return (index / Math.max(12, Math.min(18, parent.teeth))) * Math.PI * 2 + (random() - 0.5) * 0.26;
        }),
        random
      );
    }

    const slots = getLegalContactSlots({
      gear: parent,
      slotCount: parent.teeth,
      angleOffsetRad: contacts[0],
      contactAnglesByGearId,
    });
    const pitchAngle = (Math.PI * 2) / parent.teeth;
    const scored = slots.map((slot) => {
      const angle = contacts[0] + slot * pitchAngle;
      const gapToNearestContact = contacts.reduce((best, contact) => {
        const delta = Math.abs(angleDelta(angle, contact));
        return Math.min(best, delta);
      }, Math.PI);
      const radialBias = Math.abs(angleDelta(angle, Math.atan2(parent.center.y - ROOT_CENTER.y, parent.center.x - ROOT_CENTER.x)));
      return {
        angle,
        score: gapToNearestContact * 28 + radialBias * 4 + random() * 5,
      };
    });

    scored.sort((a, b) => b.score - a.score);
    return scored.map((item) => item.angle);
  }

  function scoreSinglePlacement(candidate: DraftGear, neighbors: DraftGear[], parent: DraftGear): number {
    const coverageProgress = coverageSamples.length === 0 ? 1 : coveredSampleKeys.size / coverageSamples.length;
    const coverageGain = scoreCoverageGain(candidate, coverageSamples, coveredSampleKeys);
    const coverageWeight = coverageProgress < 0.44 ? 15 : coverageProgress < 0.68 ? 10 : coverageProgress < 0.82 ? 6.5 : 3.5;
    const loopBias =
      neighbors.length >= 2
        ? 150 + neighbors.length * 70 + Math.max(0, neighbors.length - 2) * 110 + coverageProgress * 120
        : coverageProgress > 0.8
          ? -38
          : 0;
    const parentDegree = degreeByGearId.get(parent.id) ?? 0;
    const degreeBias = -parentDegree * 5 + (parentDegree <= 1 ? 20 : 0);
    const futureBridgeBias = gears.reduce((score, other) => {
      if (other.id === parent.id || other.parity !== candidate.parity) return score;
      const distance = dist(candidate.center, other.center);
      const minReach = Math.abs(candidate.pitchRadius - other.pitchRadius);
      const maxReach = candidate.pitchRadius + other.pitchRadius + MAX_BRIDGE_PITCH_RADIUS * 2;
      if (distance < minReach - 0.2 || distance > maxReach + 0.2) return score;

      const shared = sharedNeighborCount(neighbors, neighborIdsByGearId.get(other.id));
      const proximity = Math.max(0, 300 - distance) * 0.3;
      return score + proximity + shared * 42;
    }, 0);
    const contourOvershoot = candidate.center.y + candidate.outerRadius - bottomContourY(candidate.center.x, contourPhase);
    const boundaryPenalty = contourOvershoot > 26 ? contourOvershoot * 1.5 : 0;
    const lateLeafPenalty = coverageProgress > 0.86 && neighbors.length < 2 ? 70 : 0;
    return (
      coverageGain * coverageWeight +
      loopBias +
      futureBridgeBias +
      scoreExpansionNeed(candidate, gears) +
      scoreShape(candidate, neighbors, parent, contourPhase) +
      degreeBias +
      random() * 8 -
      boundaryPenalty -
      lateLeafPenalty
    );
  }

  function trySinglePlacement(parent: DraftGear, maxAngles = 8): Placement | null {
    const angleList = parentAngles(parent).slice(0, maxAngles);
    const teethList = buildCandidateTeethOrder(random, parent.teeth);
    let best: Placement | null = null;

    for (const angle of angleList) {
      for (const teeth of teethList) {
        const candidate = candidateFromParent(parent, angle, teeth);
        const verdict = evaluatePlacement(candidate, gears, contactAnglesByGearId, parent.id, true);
        if (!verdict.ok || verdict.neighbors.length === 0) continue;

        const score = scoreSinglePlacement(candidate, verdict.neighbors, parent);
        if (!best || score > best.score) best = { gear: candidate, neighbors: verdict.neighbors, score };
        if (verdict.neighbors.length >= 3 && scoreCoverageGain(candidate, coverageSamples, coveredSampleKeys) > 1.2) {
          return { gear: candidate, neighbors: verdict.neighbors, score };
        }
      }
    }

    return best;
  }

  function commonNeighborCount(a: DraftGear, b: DraftGear): number {
    const neighborsA = neighborIdsByGearId.get(a.id);
    const neighborsB = neighborIdsByGearId.get(b.id);
    if (!neighborsA || !neighborsB) return 0;

    let count = 0;
    for (const id of neighborsA) {
      if (neighborsB.has(id)) count += 1;
    }
    return count;
  }

  function buildPairPlans(limit = 44): PairPlan[] {
    const planMap = new Map<string, PairPlan>();

    for (const a of gears) {
      const nearby = gears
        .filter((b) => b.id !== a.id && b.parity === a.parity && !edgeKeys.has(edgeKey(a.id, b.id)))
        .map((b) => ({ gear: b, distanceBetween: dist(a.center, b.center) }))
        .filter(({ gear: b, distanceBetween }) => {
          const minReach = Math.abs(a.pitchRadius - b.pitchRadius);
          const maxReach = a.pitchRadius + b.pitchRadius + MAX_BRIDGE_PITCH_RADIUS * 2;
          return distanceBetween >= minReach - 0.2 && distanceBetween <= maxReach + 0.2;
        })
        .sort((left, right) => left.distanceBetween - right.distanceBetween)
        .slice(0, 10);

      for (const { gear: b, distanceBetween } of nearby) {
        const key = edgeKey(a.id, b.id);
        if (planMap.has(key)) continue;

        const midpoint = { x: (a.center.x + b.center.x) * 0.5, y: (a.center.y + b.center.y) * 0.5 };
        const midpointCoverage = scoreCoverageGain(
          {
            id: "score",
            teeth: 16,
            pitchRadius: 1,
            outerRadius: Math.min(distanceBetween * 0.35, 80),
            center: midpoint,
            parity: 0,
            appearIndex: 0,
          },
          coverageSamples,
          coveredSampleKeys
        );
        const closureBias = commonNeighborCount(a, b) * 95;
        const degreeA = degreeByGearId.get(a.id) ?? 0;
        const degreeB = degreeByGearId.get(b.id) ?? 0;
        const degreeBias = -(degreeA + degreeB) * 3.5;
        const leafBias = (degreeA <= 1 ? 95 : 0) + (degreeB <= 1 ? 95 : 0);
        const sizeBias = Math.abs(a.teeth - b.teeth) * 2.6;
        const spanBias = Math.max(0, 300 - distanceBetween) * 0.18;
        planMap.set(key, {
          a,
          b,
          score:
            midpointCoverage * 8 +
            closureBias +
            degreeBias +
            leafBias +
            sizeBias +
            spanBias +
            Math.max(0, 90 - Math.abs(midpoint.y - bottomContourY(midpoint.x, contourPhase))) * 0.18 +
            random() * 8,
        });
      }
    }

    return [...planMap.values()].sort((a, b) => b.score - a.score).slice(0, limit);
  }

function bridgeTeethOrder(plan: PairPlan): number[] {
  const centerTeeth = Math.round((plan.a.teeth + plan.b.teeth) * 0.5);
  return Array.from({ length: MAX_TEETH - MIN_TEETH + 1 }, (_, index) => index + MIN_TEETH)
    .map((teeth) => ({
      teeth,
      score: Math.abs(teeth - centerTeeth) * 0.28 + (teeth >= 23 ? 0.15 : 0) + random() * 1.15,
    }))
    .sort((a, b) => a.score - b.score)
    .slice(0, 12)
    .map((item) => item.teeth);
}

  function tryBridgePlacement(plan: PairPlan): Placement | null {
    let best: Placement | null = null;

    for (const teeth of bridgeTeethOrder(plan)) {
      const pitchRadius = pitchRadiusFromTeeth(teeth, HERO_GEAR_CIRCULAR_PITCH);
      const intersections = getTwoParentMeshedIntersections({ parentA: plan.a, parentB: plan.b, teeth });
      if (intersections.length === 0) continue;

      for (const option of intersections) {
        const candidate: DraftGear = {
          id: `hero-g${gears.length}`,
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
          appearIndex: gears.length,
        };

        const verdict = evaluatePlacement(candidate, gears, contactAnglesByGearId, undefined, true);
        if (!verdict.ok || verdict.neighbors.length < 2) continue;
        if (!verdict.neighbors.some((neighbor) => neighbor.id === plan.a.id)) continue;
        if (!verdict.neighbors.some((neighbor) => neighbor.id === plan.b.id)) continue;

        const coverageGain = scoreCoverageGain(candidate, coverageSamples, coveredSampleKeys);
        const thirdNeighborBias = Math.max(0, verdict.neighbors.length - 2) * 120;
        const score =
          plan.score +
          coverageGain * 9 +
          verdict.neighbors.length * 120 +
          scoreShape(candidate, verdict.neighbors, undefined, contourPhase) +
          scoreExpansionNeed(candidate, gears) +
          thirdNeighborBias +
          random() * 6;
        if (!best || score > best.score) best = { gear: candidate, neighbors: verdict.neighbors, score };
        if (verdict.neighbors.length >= 3 && coverageGain > 0.5) return best;
      }
    }

    return best;
  }

  const rootTeeth = 18 + randInt(random, 0, 4);
  const root: DraftGear = {
    id: "hero-g0",
    teeth: rootTeeth,
    pitchRadius: pitchRadiusFromTeeth(rootTeeth, HERO_GEAR_CIRCULAR_PITCH),
    outerRadius: outerRadiusFromTeeth(rootTeeth),
    center: ROOT_CENTER,
    phaseTurn: 0,
    parity: 0,
    appearIndex: 0,
  };
  gears.push(root);
  contactAnglesByGearId.set(root.id, []);
  degreeByGearId.set(root.id, 0);
  neighborIdsByGearId.set(root.id, new Set());
  addCoverageForGear(root, coverageSamples, coveredSampleKeys);

  for (const angle of seedAngles(random)) {
    if (gears.length >= Math.min(targetCount, 8)) break;
    const rootPlacement = trySinglePlacement(root, 16);
    if (!rootPlacement) break;
    if (Math.abs(angleDelta(Math.atan2(rootPlacement.gear.center.y - root.center.y, rootPlacement.gear.center.x - root.center.x), angle)) > Math.PI / 3) {
      const seedTeeth = buildCandidateTeethOrder(random, root.teeth)[0] ?? pickCandidateTeeth(random);
      const seededCandidate = candidateFromParent(root, angle, seedTeeth);
      const seededVerdict = evaluatePlacement(seededCandidate, gears, contactAnglesByGearId, root.id, true);
      if (seededVerdict.ok && seededVerdict.neighbors.length > 0) {
        registerPlacement(seededCandidate, seededVerdict.neighbors);
        continue;
      }
    }
    registerPlacement(rootPlacement.gear, rootPlacement.neighbors);
  }

  let attempts = 0;
  const maxAttempts = Math.max(900, targetCount * 18);
  while (gears.length < targetCount && attempts < maxAttempts) {
    attempts += 1;

    let bestPlacement: Placement | null = null;
    const pairPlans = buildPairPlans();
    const bridgeBudget = gears.length < targetCount * 0.45 ? 12 : 20;
    for (const plan of pairPlans.slice(0, bridgeBudget)) {
      const placement = tryBridgePlacement(plan);
      if (!placement) continue;
      if (!bestPlacement || placement.score > bestPlacement.score) bestPlacement = placement;
      if (placement.neighbors.length >= 4) break;
    }

    const parentPool = gears
      .map((gear) => {
          const degree = degreeByGearId.get(gear.id) ?? 0;
          const coverageBias = scoreExpansionNeed(gear, gears);
          const sizeBias = gear.teeth <= 16 ? 6 : gear.teeth >= 23 ? 3 : 0;
          return {
            gear,
            score: coverageBias - degree * 8 + sizeBias + random() * 5,
          };
       })
      .sort((a, b) => b.score - a.score)
      .map((entry) => entry.gear)
      .slice(0, Math.min(20, gears.length));

    for (const parent of parentPool) {
      const placement = trySinglePlacement(parent, gears.length < 16 ? 12 : 8);
      if (!placement) continue;
      if (!bestPlacement || placement.score > bestPlacement.score) bestPlacement = placement;
      if (placement.neighbors.length >= 3 && scoreCoverageGain(placement.gear, coverageSamples, coveredSampleKeys) > 0.8) {
        break;
      }
    }

    if (!bestPlacement && gears.length < Math.max(12, targetCount * 0.3)) {
      const fallbackParent = gears[randInt(random, 0, gears.length - 1)];
      const fallbackAngle = random() * Math.PI * 2;
      const fallbackTeeth = pickCandidateTeeth(random);
      const fallbackCandidate = candidateFromParent(fallbackParent, fallbackAngle, fallbackTeeth);
      const fallbackVerdict = evaluatePlacement(fallbackCandidate, gears, contactAnglesByGearId, fallbackParent.id, true);
      if (fallbackVerdict.ok && fallbackVerdict.neighbors.length > 0) {
        bestPlacement = {
          gear: fallbackCandidate,
          neighbors: fallbackVerdict.neighbors,
          score: scoreSinglePlacement(fallbackCandidate, fallbackVerdict.neighbors, fallbackParent),
        };
      }
    }

    if (!bestPlacement) continue;
    registerPlacement(bestPlacement.gear, bestPlacement.neighbors);
  }

  let densifyPass = 0;
  while (gears.length < targetCount && densifyPass < 24) {
    densifyPass += 1;
    let bestPlacement: Placement | null = null;
    for (const plan of buildPairPlans(56).slice(0, 24)) {
      const placement = tryBridgePlacement(plan);
      if (!placement) continue;
      if (!bestPlacement || placement.score > bestPlacement.score) bestPlacement = placement;
      if (placement.neighbors.length >= 4) break;
    }

    if (!bestPlacement) break;
    registerPlacement(bestPlacement.gear, bestPlacement.neighbors);
  }

  return { gears, edges };
};
