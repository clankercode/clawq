import test from "node:test";
import assert from "node:assert/strict";

import { createMeshedPairScene, solveGearScene } from "../src/lib/gears/solver.ts";
import { gearRadiusAtAngle, sampleGearOutlinePoints } from "../src/lib/gears/path.ts";
import {
  DEFAULT_GEAR_TUNING_RANGES,
  randomGearProfileTuning,
  solveGearProfileTuning,
} from "../src/lib/gears/tuning.ts";
import { generateHeroBackdropDraft } from "../src/lib/gears/backdrop_generation.ts";
import { BACKDROP_ALGORITHMS, DEFAULT_HERO_GEAR_ALGORITHM } from "../src/lib/gears/backdrop_algorithms.ts";
import { HERO_GEAR_CIRCULAR_PITCH, MESH_PHASE_OFFSET_TURNS } from "../src/lib/gears/backdrop/shared.ts";

const FAST_BACKDROP_ALGORITHMS = BACKDROP_ALGORITHMS.filter((algorithm) => !algorithm.startsWith("chaos-"));
const CHAOS_BACKDROP_ALGORITHMS = BACKDROP_ALGORITHMS.filter((algorithm) => algorithm.startsWith("chaos-"));

function segmentsIntersect(a1, a2, b1, b2) {
  const orient = (p, q, r) =>
    (q.x - p.x) * (r.y - p.y) - (q.y - p.y) * (r.x - p.x);

  const o1 = orient(a1, a2, b1);
  const o2 = orient(a1, a2, b2);
  const o3 = orient(b1, b2, a1);
  const o4 = orient(b1, b2, a2);

  return Math.sign(o1) !== Math.sign(o2) && Math.sign(o3) !== Math.sign(o4);
}

function transformPoint(point, center, phaseDeg) {
  const phase = (phaseDeg * Math.PI) / 180;
  const cosPhase = Math.cos(phase);
  const sinPhase = Math.sin(phase);

  return {
    x: center.x + point.x * cosPhase - point.y * sinPhase,
    y: center.y + point.x * sinPhase + point.y * cosPhase,
  };
}

function normalizeTurn(value) {
  return ((value % 1) + 1) % 1;
}

function turnDistance(a, b) {
  const delta = Math.abs(normalizeTurn(a) - normalizeTurn(b));
  return Math.min(delta, 1 - delta);
}

function meshConstraintResidual(a, b, alpha, aTurn, bTurn) {
  const alphaA = alpha / (Math.PI * 2);
  const alphaB = (alpha + Math.PI) / (Math.PI * 2);
  const lhs = a.teeth * (alphaA - aTurn) + b.teeth * (alphaB - bTurn);
  return turnDistance(lhs, MESH_PHASE_OFFSET_TURNS);
}

function hasPhaseAlignedMesh(gears, edges, tolerance = 0.01) {
  if (gears.length === 0) return true;

  const byId = new Map(gears.map((gear) => [gear.id, gear]));
  const adjacency = new Map();

  for (const edge of edges) {
    const a = byId.get(edge.a);
    const b = byId.get(edge.b);
    if (!a || !b) return false;

    const alpha = Math.atan2(b.center.y - a.center.y, b.center.x - a.center.x);
    if (!adjacency.has(a.id)) adjacency.set(a.id, []);
    if (!adjacency.has(b.id)) adjacency.set(b.id, []);
    adjacency.get(a.id).push({
      other: b.id,
      solve: (currentTurn) =>
        normalizeTurn(
          (alpha + Math.PI) / (Math.PI * 2) -
            (MESH_PHASE_OFFSET_TURNS - a.teeth * (alpha / (Math.PI * 2) - currentTurn)) / b.teeth
        ),
      alpha,
    });
    adjacency.get(b.id).push({
      other: a.id,
      solve: (currentTurn) =>
        normalizeTurn(
          alpha / (Math.PI * 2) -
            (MESH_PHASE_OFFSET_TURNS - b.teeth * ((alpha + Math.PI) / (Math.PI * 2) - currentTurn)) / a.teeth
        ),
      alpha: alpha + Math.PI,
    });
  }

  const assigned = new Map();

  for (const gear of gears) {
    if (assigned.has(gear.id)) continue;
    assigned.set(gear.id, 0);
    const queue = [gear.id];

    while (queue.length > 0) {
      const current = queue.shift();
      const currentPhase = assigned.get(current);
      const neighbors = adjacency.get(current) ?? [];
      const currentGear = byId.get(current);
      if (!currentGear) return false;

      for (const neighbor of neighbors) {
        const neighborGear = byId.get(neighbor.other);
        if (!neighborGear) return false;
        const required = neighbor.solve(currentPhase);
        if (!assigned.has(neighbor.other)) {
          assigned.set(neighbor.other, required);
          queue.push(neighbor.other);
          continue;
        }

        const existing = assigned.get(neighbor.other);
        const residual = meshConstraintResidual(currentGear, neighborGear, neighbor.alpha, currentPhase, existing);
        if (residual > tolerance) {
          return false;
        }
      }
    }
  }

  return true;
}

function pointInPolygon(point, polygon) {
  let inside = false;

  for (let i = 0, j = polygon.length - 1; i < polygon.length; j = i, i += 1) {
    const xi = polygon[i].x;
    const yi = polygon[i].y;
    const xj = polygon[j].x;
    const yj = polygon[j].y;
    const intersects =
      yi > point.y !== yj > point.y &&
      point.x < ((xj - xi) * (point.y - yi)) / (yj - yi) + xi;

    if (intersects) {
      inside = !inside;
    }
  }

  return inside;
}

const OVERLAY_TUNING = {
  valleyWidth: 0.085,
  tipWidth: 0.02,
  toothLength: 1.08,
  roundness: 1.05,
};

function draftAsSolvedForRadiusSampling(gear) {
  const module = HERO_GEAR_CIRCULAR_PITCH / Math.PI;
  const amplitude = module * 0.82;

  return {
    id: gear.id,
    center: gear.center,
    teeth: gear.teeth,
    module,
    circularPitch: HERO_GEAR_CIRCULAR_PITCH,
    pitchRadius: gear.pitchRadius,
    outerRadius: gear.pitchRadius + amplitude,
    rootRadius: Math.max(gear.pitchRadius - amplitude, gear.pitchRadius * 0.7),
    holeRadius: 1,
    innerRingRadius: 1,
    angularVelocity: 0,
    rotationDirection: "cw",
    periodSec: 1,
    phaseDeg: 0,
  };
}

function maxCenterlineEdgeOverlap(result) {
  const byId = new Map(result.gears.map((gear) => [gear.id, gear]));
  let maxOverlap = -Infinity;

  for (const edge of result.edges) {
    const a = byId.get(edge.a);
    const b = byId.get(edge.b);
    if (!a || !b) continue;

    const aTurn = a.phaseTurn ?? 0;
    const bTurn = b.phaseTurn ?? 0;
    const aSolved = draftAsSolvedForRadiusSampling(a);
    const bSolved = draftAsSolvedForRadiusSampling(b);
    const alpha = Math.atan2(b.center.y - a.center.y, b.center.x - a.center.x);
    const aLocal = alpha - aTurn * Math.PI * 2;
    const bLocal = alpha + Math.PI - bTurn * Math.PI * 2;
    const aRadius = gearRadiusAtAngle(aSolved, aLocal, OVERLAY_TUNING);
    const bRadius = gearRadiusAtAngle(bSolved, bLocal, OVERLAY_TUNING);
    const centerDistance = Math.hypot(a.center.x - b.center.x, a.center.y - b.center.y);
    const overlap = aRadius + bRadius - centerDistance;
    if (overlap > maxOverlap) maxOverlap = overlap;
  }

  return maxOverlap;
}

function explicitPhaseResiduals(result) {
  const byId = new Map(result.gears.map((gear) => [gear.id, gear]));
  return result.edges.map((edge) => {
    const a = byId.get(edge.a);
    const b = byId.get(edge.b);
    assert.ok(a && b, `Missing gear(s) for edge ${edge.a} -> ${edge.b}`);
    assert.notEqual(a.phaseTurn, undefined, `${a.id} is missing explicit phaseTurn`);
    assert.notEqual(b.phaseTurn, undefined, `${b.id} is missing explicit phaseTurn`);

    const alpha = Math.atan2(b.center.y - a.center.y, b.center.x - a.center.x);
    return {
      edge,
      residual: meshConstraintResidual(a, b, alpha, a.phaseTurn, b.phaseTurn),
    };
  });
}

function neighborCounts(result) {
  const counts = new Map(result.gears.map((gear) => [gear.id, 0]));
  for (const edge of result.edges) {
    counts.set(edge.a, (counts.get(edge.a) ?? 0) + 1);
    counts.set(edge.b, (counts.get(edge.b) ?? 0) + 1);
  }
  return counts;
}

function occupancyBuckets(result, bucketCount = 14) {
  const xMin = -180;
  const xMax = 1780;
  const width = (xMax - xMin) / bucketCount;
  const occupied = new Set();

  for (const gear of result.gears) {
    const start = Math.max(0, Math.floor((gear.center.x - gear.outerRadius - xMin) / width));
    const end = Math.min(bucketCount - 1, Math.floor((gear.center.x + gear.outerRadius - xMin) / width));
    for (let index = start; index <= end; index += 1) occupied.add(index);
  }

  return occupied.size;
}

function downsample(points, stride = 4) {
  const sampled = [];
  for (let i = 0; i < points.length; i += stride) sampled.push(points[i]);
  return sampled;
}

function outlineIntersectionCount(aOutline, bOutline) {
  let intersections = 0;

  for (let i = 0; i < aOutline.length; i += 1) {
    const a1 = aOutline[i];
    const a2 = aOutline[(i + 1) % aOutline.length];

    for (let j = 0; j < bOutline.length; j += 1) {
      const b1 = bOutline[j];
      const b2 = bOutline[(j + 1) % bOutline.length];
      if (segmentsIntersect(a1, a2, b1, b2)) intersections += 1;
    }
  }

  return intersections;
}

function transformedDraftOutline(gear) {
  const solved = draftAsSolvedForRadiusSampling(gear);
  const points = downsample(sampleGearOutlinePoints(solved, OVERLAY_TUNING), 6);
  return points.map((point) => transformPoint(point, gear.center, (gear.phaseTurn ?? 0) * 360));
}

test("gear outline stays between root and outer radii without self-intersections", () => {
  const scene = solveGearScene(
    createMeshedPairScene({
      circularPitch: 34,
      minTeeth: 12,
      driverId: "gear-large",
      driverPeriodSec: 120,
      driverDirection: "cw",
      first: {
        id: "gear-large",
        center: { x: 280, y: 252 },
        targetPitchRadius: 130,
      },
      second: {
        id: "gear-small",
        idealCenter: { x: 136, y: 116 },
        targetPitchRadius: 65,
      },
    })
  );

  for (const gear of scene.gears) {
    const points = sampleGearOutlinePoints(gear);

    assert.ok(points.length > gear.teeth * 10, `Expected dense outline for ${gear.id}`);

    for (const point of points) {
      const radius = Math.hypot(point.x, point.y);
      assert.ok(radius >= gear.rootRadius - 0.001, `${gear.id} dipped below root radius`);
      assert.ok(radius <= gear.outerRadius + 0.001, `${gear.id} exceeded outer radius`);
    }

    for (let i = 0; i < points.length; i += 1) {
      const a1 = points[i];
      const a2 = points[(i + 1) % points.length];

      for (let j = i + 2; j < points.length; j += 1) {
        if (i === 0 && j === points.length - 1) {
          continue;
        }

        const b1 = points[j];
        const b2 = points[(j + 1) % points.length];

        if (segmentsIntersect(a1, a2, b1, b2)) {
          assert.fail(`${gear.id} outline self-intersects between segments ${i} and ${j}`);
        }
      }
    }
  }
});

test("gear outline covers every tooth sector around the full 360 degrees", () => {
  const scene = solveGearScene(
    createMeshedPairScene({
      circularPitch: 34,
      minTeeth: 12,
      driverId: "gear-large",
      driverPeriodSec: 120,
      driverDirection: "cw",
      first: {
        id: "gear-large",
        center: { x: 280, y: 252 },
        targetPitchRadius: 130,
      },
      second: {
        id: "gear-small",
        idealCenter: { x: 136, y: 116 },
        targetPitchRadius: 65,
      },
    })
  );

  for (const gear of scene.gears) {
    const points = sampleGearOutlinePoints(gear);
    const activeSectors = new Set();
    const threshold = gear.rootRadius + (gear.outerRadius - gear.rootRadius) * 0.2;

    for (const point of points) {
      const angle = Math.atan2(point.y, point.x);
      const normalized = (angle + Math.PI) / (Math.PI * 2);
      const sector = Math.min(gear.teeth - 1, Math.floor(normalized * gear.teeth));
      const radius = Math.hypot(point.x, point.y);

      if (radius > threshold) {
        activeSectors.add(sector);
      }
    }

    assert.equal(activeSectors.size, gear.teeth, `${gear.id} does not render all tooth sectors around 360 degrees`);
  }
});

test("meshed pair avoids heavy geometric overlap", () => {
  const scene = solveGearScene(
    createMeshedPairScene({
      circularPitch: 34,
      minTeeth: 12,
      driverId: "gear-large",
      driverPeriodSec: 120,
      driverDirection: "cw",
      first: {
        id: "gear-large",
        center: { x: 280, y: 252 },
        targetPitchRadius: 130,
      },
      second: {
        id: "gear-small",
        idealCenter: { x: 136, y: 116 },
        targetPitchRadius: 65,
      },
    })
  );

  const [large, small] = scene.gears;
  const largeOutline = sampleGearOutlinePoints(large).map((point) => transformPoint(point, large.center, large.phaseDeg));
  const smallOutline = sampleGearOutlinePoints(small).map((point) => transformPoint(point, small.center, small.phaseDeg));

  let intersections = 0;

  for (let i = 0; i < largeOutline.length; i += 1) {
    const a1 = largeOutline[i];
    const a2 = largeOutline[(i + 1) % largeOutline.length];

    for (let j = 0; j < smallOutline.length; j += 1) {
      const b1 = smallOutline[j];
      const b2 = smallOutline[(j + 1) % smallOutline.length];

      if (segmentsIntersect(a1, a2, b1, b2)) {
        intersections += 1;
      }
    }
  }

  assert.equal(pointInPolygon(largeOutline[0], smallOutline), false, "Large gear outline falls inside small gear polygon");
  assert.equal(pointInPolygon(smallOutline[0], largeOutline), false, "Small gear outline falls inside large gear polygon");
  assert.ok(intersections <= 12, `Gear outlines overlap too heavily (${intersections} intersections)`);
});

test("random tuning generator respects configured ranges", () => {
  const tuning = randomGearProfileTuning(() => 0.5);

  assert.ok(tuning.valleyWidth >= DEFAULT_GEAR_TUNING_RANGES.valleyWidth.min);
  assert.ok(tuning.valleyWidth <= DEFAULT_GEAR_TUNING_RANGES.valleyWidth.max);
  assert.ok(tuning.tipWidth >= DEFAULT_GEAR_TUNING_RANGES.tipWidth.min);
  assert.ok(tuning.tipWidth <= DEFAULT_GEAR_TUNING_RANGES.tipWidth.max);
  assert.ok(tuning.toothLength >= DEFAULT_GEAR_TUNING_RANGES.toothLength.min);
  assert.ok(tuning.toothLength <= DEFAULT_GEAR_TUNING_RANGES.toothLength.max);
  assert.ok(tuning.roundness >= DEFAULT_GEAR_TUNING_RANGES.roundness.min);
  assert.ok(tuning.roundness <= DEFAULT_GEAR_TUNING_RANGES.roundness.max);
});

test("tuning solver finds a low-overlap candidate deterministically", () => {
  const scene = solveGearScene(
    createMeshedPairScene({
      circularPitch: 34,
      minTeeth: 12,
      driverId: "gear-large",
      driverPeriodSec: 120,
      driverDirection: "cw",
      first: {
        id: "gear-large",
        center: { x: 280, y: 252 },
        targetPitchRadius: 130,
      },
      second: {
        id: "gear-small",
        idealCenter: { x: 136, y: 116 },
        targetPitchRadius: 65,
      },
    })
  );

  const result = solveGearProfileTuning(scene, {
    seed: 12345,
    samples: 120,
    intersectionTolerance: 2,
  });

  assert.ok(result.top.length > 0, "Expected at least one candidate");
  assert.ok(result.best.intersections <= 2, `Expected low-overlap tuning, got ${result.best.intersections} intersections`);
  assert.equal(result.best.containsOtherOutline, false, "Best tuning should avoid polygon containment overlap");
});

test("hero backdrop generators repeatedly produce dense populations", () => {
  const algorithms = FAST_BACKDROP_ALGORITHMS;

  for (const algorithm of algorithms) {
    for (let seed = 0; seed < 20; seed += 1) {
      const result = generateHeroBackdropDraft({ algorithm, seed: 0x6a11cf + seed });
      assert.ok(
        result.gears.length > 20,
        `${algorithm} generator produced only ${result.gears.length} gears for seed ${seed}`
      );
    }
  }
});

test("hero backdrop generators produce >=20 gears with phase-alignable meshes", () => {
  const algorithms = FAST_BACKDROP_ALGORITHMS;

  for (const algorithm of algorithms) {
    let candidate = null;

    for (let seed = 0; seed < 40; seed += 1) {
      const result = generateHeroBackdropDraft({ algorithm, seed: 0x6a11cf + seed });
      if (result.gears.length >= 20) {
        candidate = result;
        break;
      }
    }

    assert.ok(candidate, `${algorithm} could not produce a layout with at least 20 gears`);
    assert.ok(
      hasPhaseAlignedMesh(candidate.gears, candidate.edges),
      `${algorithm} produced a >=20-gear layout that cannot be phase-aligned for meshing`
    );
  }
});

test("hero backdrop generators store explicit phase turns that satisfy every mesh edge", () => {
  for (const algorithm of FAST_BACKDROP_ALGORITHMS) {
    let candidate = null;

    for (let seed = 0; seed < 40; seed += 1) {
      const result = generateHeroBackdropDraft({ algorithm, seed: 0x6a11cf + seed });
      if (result.gears.length >= 20) {
        candidate = result;
        break;
      }
    }

    assert.ok(candidate, `${algorithm} could not produce a layout with at least 20 gears`);

    for (const { edge, residual } of explicitPhaseResiduals(candidate)) {
      assert.ok(residual <= 1e-6, `${algorithm} explicit phase residual too high for ${edge.a}<->${edge.b}: ${residual}`);
    }
  }
});

test("loop-capable backdrop generators create strongly interconnected meshes", () => {
  for (const algorithm of ["weave", "cell-fill", "ring-web", "chaos-bridged", "chaos-cluster"]) {
    const result = generateHeroBackdropDraft({ algorithm, seed: 0x6a11cf, targetCount: 60 });
    const counts = [...neighborCounts(result).values()];
    const multiNeighborGears = counts.filter((count) => count >= 2).length;

    assert.ok(result.gears.length >= 20, `${algorithm} should produce at least 20 gears for the reference seed`);
    assert.ok(
      result.edges.length >= Math.floor(result.gears.length * 0.95),
      `${algorithm} should contain substantial loop structure, got ${result.edges.length} edges for ${result.gears.length} gears`
    );
    assert.ok(
      multiNeighborGears >= Math.floor(result.gears.length * 0.6),
      `${algorithm} should give most gears 2+ neighbors, got ${multiNeighborGears}/${result.gears.length}`
    );
  }
});

test("chaos-cluster fills the hero width while staying in the upper hero band", () => {
  const result = generateHeroBackdropDraft({ algorithm: "chaos-cluster", seed: 0x6a11cf, targetCount: 116 });
  const minLeft = Math.min(...result.gears.map((gear) => gear.center.x - gear.outerRadius));
  const maxRight = Math.max(...result.gears.map((gear) => gear.center.x + gear.outerRadius));
  const minTop = Math.min(...result.gears.map((gear) => gear.center.y - gear.outerRadius));
  const maxBottom = Math.max(...result.gears.map((gear) => gear.center.y + gear.outerRadius));

  assert.ok(result.gears.length >= 40, `chaos-cluster should produce a large field, got ${result.gears.length} gears`);
  assert.ok(occupancyBuckets(result) >= 12, "chaos-fill should cover almost the full hero width");
  assert.ok(minLeft <= -80, `chaos-cluster should push off the left edge, got ${minLeft}`);
  assert.ok(maxRight >= 1680, `chaos-cluster should push off the right edge, got ${maxRight}`);
  assert.ok(minTop <= -40, `chaos-cluster should push off the top edge, got ${minTop}`);
  assert.ok(maxBottom <= 430, `chaos-cluster should keep the bottom edge near the header band, got ${maxBottom}`);
});

test("chaos variants keep explicit phases and generate viable dense fields", () => {
  for (const algorithm of CHAOS_BACKDROP_ALGORITHMS) {
    const result = generateHeroBackdropDraft({ algorithm, seed: 0x6a11cf, targetCount: 72 });
    const counts = [...neighborCounts(result).values()];
    const multiNeighborGears = counts.filter((count) => count >= 2).length;

    assert.ok(result.gears.length >= 24, `${algorithm} should produce at least 24 gears for the reference seed`);
    assert.ok(result.edges.length >= Math.floor(result.gears.length * 0.85), `${algorithm} should stay reasonably interconnected`);
    assert.ok(multiNeighborGears >= Math.floor(result.gears.length * 0.5), `${algorithm} should avoid collapsing into mostly leaves`);

    for (const { edge, residual } of explicitPhaseResiduals(result)) {
      assert.ok(residual <= 1e-6, `${algorithm} explicit phase residual too high for ${edge.a}<->${edge.b}: ${residual}`);
    }
  }
});

test("hex-web generates a non-empty backdrop", () => {
  const result = generateHeroBackdropDraft({ algorithm: "hex-web", seed: 0x6a11cf, targetCount: 116 });

  assert.ok(result.gears.length > 0, "hex-web should generate at least one gear");
  assert.ok(result.edges.length > 0, "hex-web should generate at least one mesh edge");
});

test("default hero backdrop stays in the upper hero band", () => {
  const result = generateHeroBackdropDraft({ seed: 0x6a11cf, targetCount: 116 });
  const maxBottom = Math.max(...result.gears.map((gear) => gear.center.y + gear.outerRadius));

  assert.equal(DEFAULT_HERO_GEAR_ALGORITHM, "chaos-cluster");
  assert.ok(result.gears.length >= 40, `default backdrop should produce a large field, got ${result.gears.length} gears`);
  assert.ok(maxBottom <= 430, `default backdrop should keep the bottom edge near the header band, got ${maxBottom}`);
});

test("row-debug generated outlines avoid heavy edge intersections", () => {
  const result = generateHeroBackdropDraft({ algorithm: "row-debug", seed: 0x6a11cf, targetCount: 40 });
  const byId = new Map(result.gears.map((gear) => [gear.id, gear]));

  for (const edge of result.edges.slice(0, 10)) {
    const a = byId.get(edge.a);
    const b = byId.get(edge.b);
    assert.ok(a && b, `Missing gear(s) for edge ${edge.a} -> ${edge.b}`);

    const aOutline = transformedDraftOutline(a);
    const bOutline = transformedDraftOutline(b);
    const intersections = outlineIntersectionCount(aOutline, bOutline);

    assert.ok(intersections <= 14, `row-debug outlines overlap too heavily for ${edge.a}<->${edge.b} (${intersections} intersections)`);
  }
});

test("sine-debug generated outlines avoid heavy edge intersections", () => {
  const result = generateHeroBackdropDraft({ algorithm: "sine-debug", seed: 0x6a11cf, targetCount: 40 });
  const byId = new Map(result.gears.map((gear) => [gear.id, gear]));

  for (const edge of result.edges.slice(0, 12)) {
    const a = byId.get(edge.a);
    const b = byId.get(edge.b);
    assert.ok(a && b, `Missing gear(s) for edge ${edge.a} -> ${edge.b}`);

    const aOutline = transformedDraftOutline(a);
    const bOutline = transformedDraftOutline(b);
    const intersections = outlineIntersectionCount(aOutline, bOutline);

    assert.ok(intersections <= 14, `sine-debug outlines overlap too heavily for ${edge.a}<->${edge.b} (${intersections} intersections)`);
  }
});

test("row-debug assigns explicit phase turns that satisfy every mesh edge", () => {
  const result = generateHeroBackdropDraft({ algorithm: "row-debug", seed: 0x6a11cf, targetCount: 40 });
  assert.ok(result.gears.length >= 20, `Expected dense row-debug layout, got ${result.gears.length} gears`);

  for (const { edge, residual } of explicitPhaseResiduals(result)) {
    assert.ok(residual <= 1e-6, `row-debug mesh residual too high for ${edge.a}<->${edge.b}: ${residual}`);
  }
});

test("row-debug keeps sampled edge overlap below collision threshold", () => {
  const result = generateHeroBackdropDraft({ algorithm: "row-debug", seed: 0x6a11cf, targetCount: 40 });
  const worst = maxCenterlineEdgeOverlap(result);
  assert.ok(worst <= 0.25, `row-debug worst centerline edge overlap too high: ${worst}`);
});
