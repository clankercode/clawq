import { pitchRadiusFromTeeth } from "../solver.ts";
import type { BackdropGeneratorFn, DraftGear, DraftMeshEdge, Point } from "./types.ts";
import {
  HERO_GEAR_CIRCULAR_PITCH,
  MESH_EPSILON,
  MIN_TEETH,
  VIEWBOX,
  createGenerationContext,
  dist,
  evaluatePlacement,
  normalizeTurn,
  outerRadiusFromTeeth,
  pickCandidateTeeth,
  pointAt,
  randInt,
  solveNeighborPhaseTurn,
} from "./shared.ts";

const MAX_TEETH = 28;
const PAIR_PHASE_TOLERANCE = 0.035;

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

function edgeKey(a: string, b: string): string {
  return a < b ? `${a}|${b}` : `${b}|${a}`;
}

function circularResidual(a: number, b: number): number {
  const delta = normalizeTurn(a - b);
  return Math.min(delta, 1 - delta);
}

function circleIntersections(a: Point, b: Point, radiusA: number, radiusB: number): Point[] {
  const d = dist(a, b);
  if (d <= 1e-6) return [];
  if (d > radiusA + radiusB + MESH_EPSILON) return [];
  if (d < Math.abs(radiusA - radiusB) - MESH_EPSILON) return [];

  const along = (radiusA * radiusA - radiusB * radiusB + d * d) / (2 * d);
  const heightSquared = radiusA * radiusA - along * along;
  if (heightSquared < -MESH_EPSILON) return [];

  const height = Math.sqrt(Math.max(0, heightSquared));
  const ux = (b.x - a.x) / d;
  const uy = (b.y - a.y) / d;
  const base = { x: a.x + ux * along, y: a.y + uy * along };
  const px = -uy * height;
  const py = ux * height;

  if (height <= 1e-6) return [base];
  return [
    { x: base.x + px, y: base.y + py },
    { x: base.x - px, y: base.y - py },
  ];
}

function scoreCoverage(center: Point, missingLeft: boolean, missingRight: boolean): number {
  if (missingLeft && missingRight) return -Math.abs(center.x - VIEWBOX.width * 0.5) * 0.02;
  if (missingLeft) return (VIEWBOX.width * 0.5 - center.x) * 0.08;
  if (missingRight) return (center.x - VIEWBOX.width * 0.5) * 0.08;
  return -Math.abs(center.y - 260) * 0.03;
}

function neighborPhase(gear: DraftGear, candidateTeeth: number, center: Point): number {
  return solveNeighborPhaseTurn({
    currentTeeth: gear.teeth,
    neighborTeeth: candidateTeeth,
    currentTurn: gear.phaseTurn ?? 0,
    contactAngleCurrentToNeighbor: Math.atan2(center.y - gear.center.y, center.x - gear.center.x),
  });
}

function buildRootAngles(random: () => number): number[] {
  const stepTurns = [0.12, 0.19, 0.11, 0.21, 0.15, 0.22];
  let turn = random();
  const angles: number[] = [];

  for (const step of stepTurns) {
    angles.push(turn * Math.PI * 2);
    turn += step + (random() - 0.5) * 0.024;
  }

  return angles.sort((left, right) => {
    const leftBias = Math.abs(Math.cos(left)) * 1.8 + Math.max(0, -Math.sin(left));
    const rightBias = Math.abs(Math.cos(right)) * 1.8 + Math.max(0, -Math.sin(right));
    return rightBias - leftBias;
  });
}

export const generateCellFillBackdrop: BackdropGeneratorFn = ({ seed, targetCount = 84 }) => {
  const { random } = createGenerationContext(seed, 0x43f11c);
  const gears: DraftGear[] = [];
  const edges: DraftMeshEdge[] = [];
  const edgeKeys = new Set<string>();
  const contactAnglesByGearId = new Map<string, number[]>();
  const childCountByGearId = new Map<string, number>();
  const neighborIdsByGearId = new Map<string, Set<string>>();

  function addEdge(a: string, b: string): void {
    const key = edgeKey(a, b);
    if (edgeKeys.has(key)) return;
    edgeKeys.add(key);
    edges.push({ a, b });
  }

  function registerPlacement(gear: DraftGear, neighbors: DraftGear[]): void {
    gears.push(gear);
    childCountByGearId.set(gear.id, 0);
    if (!contactAnglesByGearId.has(gear.id)) contactAnglesByGearId.set(gear.id, []);
    if (!neighborIdsByGearId.has(gear.id)) neighborIdsByGearId.set(gear.id, new Set());
    const gearContacts = contactAnglesByGearId.get(gear.id)!;
    const gearNeighborIds = neighborIdsByGearId.get(gear.id)!;

    for (const neighbor of neighbors) {
      const angleFromGear = Math.atan2(neighbor.center.y - gear.center.y, neighbor.center.x - gear.center.x);
      gearContacts.push(angleFromGear);
      gearNeighborIds.add(neighbor.id);

      if (!contactAnglesByGearId.has(neighbor.id)) contactAnglesByGearId.set(neighbor.id, []);
      if (!neighborIdsByGearId.has(neighbor.id)) neighborIdsByGearId.set(neighbor.id, new Set());
      contactAnglesByGearId
        .get(neighbor.id)!
        .push(Math.atan2(gear.center.y - neighbor.center.y, gear.center.x - neighbor.center.x));
      neighborIdsByGearId.get(neighbor.id)!.add(gear.id);

      childCountByGearId.set(neighbor.id, (childCountByGearId.get(neighbor.id) ?? 0) + 1);
      addEdge(gear.id, neighbor.id);
    }
  }

  function missingLeftCoverage(): boolean {
    return !gears.some((gear) => gear.center.x + gear.outerRadius < -70);
  }

  function missingRightCoverage(): boolean {
    return !gears.some((gear) => gear.center.x - gear.outerRadius > VIEWBOX.width + 70);
  }

  function compatiblePhaseForNeighbors(candidate: DraftGear, neighbors: DraftGear[]): DraftGear | null {
    if (neighbors.length === 0) return null;

    let phaseTurn: number | null = null;
    let parentId = candidate.parentId;
    for (const neighbor of neighbors) {
      const nextPhase = neighborPhase(neighbor, candidate.teeth, candidate.center);
      if (phaseTurn === null) {
        phaseTurn = nextPhase;
        parentId = neighbor.id;
        continue;
      }
      if (circularResidual(nextPhase, phaseTurn) > PAIR_PHASE_TOLERANCE) {
        return null;
      }
    }

    return {
      ...candidate,
      phaseTurn: phaseTurn ?? 0,
      parentId,
    };
  }

  function candidateTeethByPitch(minPitchRadius = 0): number[] {
    const candidates: { teeth: number; score: number }[] = [];
    for (let teeth = MIN_TEETH; teeth <= MAX_TEETH; teeth += 1) {
      const pitchRadius = pitchRadiusFromTeeth(teeth, HERO_GEAR_CIRCULAR_PITCH);
      if (pitchRadius + MESH_EPSILON < minPitchRadius) continue;
      const smallGearBias = teeth <= 16 ? -0.4 : teeth <= 20 ? 0 : 0.35;
      candidates.push({
        teeth,
        score: Math.abs(pitchRadius - minPitchRadius) + teeth * 0.015 + smallGearBias,
      });
    }

    if (candidates.length === 0) return [];
    candidates.sort((a, b) => a.score - b.score);
    return candidates.map((item) => item.teeth);
  }

  function emptySlotAngles(parent: DraftGear): number[] {
    const contacts = contactAnglesByGearId.get(parent.id) ?? [];
    const toothAngle = (2 * Math.PI) / parent.teeth;

    if (contacts.length === 0) {
      return ROOT_ANGLES.slice().sort(
        (a, b) => Math.abs(a) - Math.abs(b) + (random() - 0.5) * 0.01
      );
    }

    const base = contacts[0];
    const angles: number[] = [];
    for (let slot = 0; slot < parent.teeth; slot += 1) {
      const angle = base + slot * toothAngle;
      const occupied = contacts.some(
        (contact) => Math.abs(Math.atan2(Math.sin(angle - contact), Math.cos(angle - contact))) < toothAngle * 0.18
      );
      if (!occupied) angles.push(angle);
    }

    return angles;
  }

  function trySingleParentPlacement(parent: DraftGear, angles: number[], minNeighbors: number): Placement | null {
    const missingLeft = missingLeftCoverage();
    const missingRight = missingRightCoverage();
    const teethList = candidateTeethByPitch(0);

    for (const angle of angles) {
      for (const candidateTeeth of teethList) {
        const candidatePitch = pitchRadiusFromTeeth(candidateTeeth, HERO_GEAR_CIRCULAR_PITCH);
        const center = pointAt(parent.center, angle, parent.pitchRadius + candidatePitch);
        const draft: DraftGear = {
          id: `hero-g${gears.length}`,
          teeth: candidateTeeth,
          pitchRadius: candidatePitch,
          outerRadius: outerRadiusFromTeeth(candidateTeeth),
          center,
          parity: parent.parity === 0 ? 1 : 0,
          parentId: parent.id,
          appearIndex: gears.length,
        };

        const verdict = evaluatePlacement(draft, gears, contactAnglesByGearId, parent.id);
        if (!verdict.ok || verdict.neighbors.length < minNeighbors) continue;

        const withPhase = compatiblePhaseForNeighbors(draft, verdict.neighbors);
        if (!withPhase) continue;

        return {
          gear: withPhase,
          neighbors: verdict.neighbors,
          score:
            verdict.neighbors.length * 80 +
            scoreCoverage(center, missingLeft, missingRight) -
            (childCountByGearId.get(parent.id) ?? 0) * 6,
        };
      }
    }

    return null;
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

  function buildPairPlans(): PairPlan[] {
    const missingLeft = missingLeftCoverage();
    const missingRight = missingRightCoverage();
    const plans: PairPlan[] = [];

    for (let i = 0; i < gears.length; i += 1) {
      for (let j = i + 1; j < gears.length; j += 1) {
        const a = gears[i];
        const b = gears[j];
        if (a.parity !== b.parity) continue;
        if (edgeKeys.has(edgeKey(a.id, b.id))) continue;

        const distance = dist(a.center, b.center);
        const minPitchRadius = Math.max(0, (distance - a.pitchRadius - b.pitchRadius) * 0.5);
        const maxPitchRadius = pitchRadiusFromTeeth(MAX_TEETH, HERO_GEAR_CIRCULAR_PITCH);
        if (minPitchRadius > maxPitchRadius + MESH_EPSILON) continue;
        if (distance < Math.abs(a.pitchRadius - b.pitchRadius) - MESH_EPSILON) continue;

        const midpoint = { x: (a.center.x + b.center.x) * 0.5, y: (a.center.y + b.center.y) * 0.5 };
        const closureBias = commonNeighborCount(a, b) * 140;
        const degreeBias = -((childCountByGearId.get(a.id) ?? 0) + (childCountByGearId.get(b.id) ?? 0)) * 4;
        plans.push({
          a,
          b,
          score: closureBias + degreeBias + scoreCoverage(midpoint, missingLeft, missingRight),
        });
      }
    }

    plans.sort((a, b) => b.score - a.score);
    return plans.slice(0, 20);
  }

  function tryPairPlacement(plan: PairPlan): Placement | null {
    const missingLeft = missingLeftCoverage();
    const missingRight = missingRightCoverage();
    const distance = dist(plan.a.center, plan.b.center);
    const minPitchRadius = Math.max(0, (distance - plan.a.pitchRadius - plan.b.pitchRadius) * 0.5);
    const teethList = candidateTeethByPitch(minPitchRadius);

    for (const candidateTeeth of teethList) {
      const candidatePitch = pitchRadiusFromTeeth(candidateTeeth, HERO_GEAR_CIRCULAR_PITCH);
      const intersections = circleIntersections(
        plan.a.center,
        plan.b.center,
        plan.a.pitchRadius + candidatePitch,
        plan.b.pitchRadius + candidatePitch
      );

      const ordered = intersections
        .map((center) => ({
          center,
          score:
            scoreCoverage(center, missingLeft, missingRight) +
            commonNeighborCount(plan.a, plan.b) * 20 -
            Math.abs(center.y - 250) * 0.02,
        }))
        .sort((a, b) => b.score - a.score);

      for (const option of ordered) {
        const draft: DraftGear = {
          id: `hero-g${gears.length}`,
          teeth: candidateTeeth,
          pitchRadius: candidatePitch,
          outerRadius: outerRadiusFromTeeth(candidateTeeth),
          center: option.center,
          parity: plan.a.parity === 0 ? 1 : 0,
          parentId: plan.a.id,
          appearIndex: gears.length,
        };

        const verdict = evaluatePlacement(draft, gears, contactAnglesByGearId);
        if (!verdict.ok || verdict.neighbors.length < 2) continue;
        if (!verdict.neighbors.some((neighbor) => neighbor.id === plan.a.id)) continue;
        if (!verdict.neighbors.some((neighbor) => neighbor.id === plan.b.id)) continue;

        const withPhase = compatiblePhaseForNeighbors(draft, verdict.neighbors);
        if (!withPhase) continue;

        return {
          gear: withPhase,
          neighbors: verdict.neighbors,
          score: verdict.neighbors.length * 120 + option.score + plan.score,
        };
      }
    }

    return null;
  }

  const rootTeeth = 17 + randInt(random, 0, 3);
  const rootAngles = buildRootAngles(random);
  const root: DraftGear = {
    id: "hero-g0",
    teeth: rootTeeth,
    pitchRadius: pitchRadiusFromTeeth(rootTeeth, HERO_GEAR_CIRCULAR_PITCH),
    outerRadius: outerRadiusFromTeeth(rootTeeth),
    center: {
      x: VIEWBOX.width * 0.5 + (random() - 0.5) * 86,
      y: 274 + (random() - 0.5) * 30,
    },
    phaseTurn: 0,
    parity: 0,
    appearIndex: 0,
  };
  gears.push(root);
  childCountByGearId.set(root.id, 0);
  contactAnglesByGearId.set(root.id, []);
  neighborIdsByGearId.set(root.id, new Set());

  for (const angle of rootAngles) {
    if (gears.length >= Math.min(targetCount, 7)) break;
    const placement = trySingleParentPlacement(root, [angle], 1);
    if (placement) registerPlacement(placement.gear, placement.neighbors);
  }

  let attempts = 0;
  const maxAttempts = Math.max(16000, targetCount * 320);
  while (gears.length < targetCount && attempts < maxAttempts) {
    attempts += 1;

    let bestPlacement: Placement | null = null;
    const pairPlans = buildPairPlans();
    for (const plan of pairPlans) {
      const placement = tryPairPlacement(plan);
      if (!placement) continue;
      if (!bestPlacement || placement.score > bestPlacement.score) {
        bestPlacement = placement;
      }
      if (placement.neighbors.length >= 3) break;
    }

    if (!bestPlacement || random() < 0.25) {
      const parents = gears
        .slice()
        .sort((a, b) => {
          const childDelta = (childCountByGearId.get(a.id) ?? 0) - (childCountByGearId.get(b.id) ?? 0);
          if (childDelta !== 0) return childDelta;
          const aBias = scoreCoverage(a.center, missingLeftCoverage(), missingRightCoverage());
          const bBias = scoreCoverage(b.center, missingLeftCoverage(), missingRightCoverage());
          return bBias - aBias;
        })
        .slice(0, Math.min(20, gears.length));

      for (const parent of parents) {
        const placement = trySingleParentPlacement(parent, emptySlotAngles(parent), 1);
        if (!placement) continue;
        if (!bestPlacement || placement.score > bestPlacement.score) {
          bestPlacement = placement;
        }
        if (placement.neighbors.length >= 2) break;
      }
    }

    if (!bestPlacement) continue;
    registerPlacement(bestPlacement.gear, bestPlacement.neighbors);
  }

  if (gears.length < Math.min(targetCount, 10)) {
    const extraParent = gears[randInt(random, 0, gears.length - 1)];
    const fallbackTeeth = pickCandidateTeeth(random);
    const fallbackPitch = pitchRadiusFromTeeth(fallbackTeeth, HERO_GEAR_CIRCULAR_PITCH);
    const fallbackAngle = random() * Math.PI * 2;
    const fallbackDraft: DraftGear = {
      id: `hero-g${gears.length}`,
      teeth: fallbackTeeth,
      pitchRadius: fallbackPitch,
      outerRadius: outerRadiusFromTeeth(fallbackTeeth),
      center: pointAt(extraParent.center, fallbackAngle, extraParent.pitchRadius + fallbackPitch),
      parity: extraParent.parity === 0 ? 1 : 0,
      parentId: extraParent.id,
      appearIndex: gears.length,
    };
    const verdict = evaluatePlacement(fallbackDraft, gears, contactAnglesByGearId, extraParent.id);
    const withPhase = verdict.ok ? compatiblePhaseForNeighbors(fallbackDraft, verdict.neighbors) : null;
    if (withPhase) registerPlacement(withPhase, verdict.neighbors);
  }

  return { gears, edges };
};
