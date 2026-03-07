import type { BackdropGeneratorOptions, BackdropGeneratorResult, DraftGear, DraftMeshEdge } from "./types.ts";
import { generateSpecBackdrop } from "./spec_core.ts";

type Mode = "topology-first" | "constraint-solver";

const HERO_X_MIN = -180;
const HERO_X_MAX = 1780;
const HERO_Y_MIN = -40;
const HERO_Y_MAX = 430;
const WINDOW_WIDTH = HERO_X_MAX - HERO_X_MIN + 260;

function boundsOfGears(gears: DraftGear[]) {
  return {
    minX: Math.min(...gears.map((gear) => gear.center.x - gear.outerRadius)),
    maxX: Math.max(...gears.map((gear) => gear.center.x + gear.outerRadius)),
    minY: Math.min(...gears.map((gear) => gear.center.y - gear.outerRadius)),
    maxY: Math.max(...gears.map((gear) => gear.center.y + gear.outerRadius)),
  };
}

function graphStats(gears: DraftGear[], edges: DraftMeshEdge[]) {
  const adjacency = new Map<string, string[]>();
  const degreeById = new Map<string, number>();
  for (const gear of gears) {
    adjacency.set(gear.id, []);
    degreeById.set(gear.id, 0);
  }
  for (const edge of edges) {
    adjacency.get(edge.a)?.push(edge.b);
    adjacency.get(edge.b)?.push(edge.a);
    degreeById.set(edge.a, (degreeById.get(edge.a) ?? 0) + 1);
    degreeById.set(edge.b, (degreeById.get(edge.b) ?? 0) + 1);
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
  const largestComponent = sizes[0] ?? 0;
  const cycleSurplus = Math.max(0, edges.length - gears.length + sizes.length);
  const leafCount = gears.filter((gear) => (degreeById.get(gear.id) ?? 0) <= 1).length;
  const multiNeighborCount = gears.filter((gear) => (degreeById.get(gear.id) ?? 0) >= 2).length;
  const richNeighborCount = gears.filter((gear) => (degreeById.get(gear.id) ?? 0) >= 3).length;
  return {
    componentCount: sizes.length,
    largestComponent,
    outsideLargest: gears.length - largestComponent,
    cycleSurplus,
    leafCount,
    multiNeighborCount,
    richNeighborCount,
  };
}

function remapBackdrop(result: BackdropGeneratorResult): BackdropGeneratorResult {
  const idMap = new Map<string, string>();
  const gears = result.gears
    .slice()
    .sort((left, right) => (left.center.x !== right.center.x ? left.center.x - right.center.x : left.center.y - right.center.y))
    .map((gear, index) => {
      const id = `hero-g${index}`;
      idMap.set(gear.id, id);
      return {
        ...gear,
        id,
        appearIndex: index,
      };
    });

  const parentLookup = new Map(result.gears.map((gear) => [gear.id, gear.parentId]));
  for (const gear of gears) {
    const originalId = [...idMap.entries()].find(([, nextId]) => nextId === gear.id)?.[0];
    const parentId = originalId ? parentLookup.get(originalId) : undefined;
    gear.parentId = parentId ? idMap.get(parentId) : undefined;
  }

  const edges = result.edges
    .map((edge) => ({ a: idMap.get(edge.a), b: idMap.get(edge.b) }))
    .filter((edge): edge is DraftMeshEdge => Boolean(edge.a && edge.b));

  return { gears, edges };
}

function trimBackdropToHero(result: BackdropGeneratorResult, mode: Mode): BackdropGeneratorResult {
  if (result.gears.length === 0) return result;

  let bestIds = new Set(result.gears.map((gear) => gear.id));
  let bestScore = -Infinity;

  for (const anchor of result.gears) {
    const startX = anchor.center.x - anchor.outerRadius - 40;
    const endX = startX + WINDOW_WIDTH;
    const kept = result.gears.filter(
      (gear) => gear.center.x + gear.outerRadius >= startX && gear.center.x - gear.outerRadius <= endX,
    );
    const ids = new Set(kept.map((gear) => gear.id));
    const keptEdges = result.edges.filter((edge) => ids.has(edge.a) && ids.has(edge.b));
    const edgeCount = keptEdges.length;
    const graph = graphStats(kept, keptEdges);
    const score =
      kept.length * 12 +
      edgeCount * 7 +
      graph.largestComponent * (mode === "constraint-solver" ? 34 : 20) -
      Math.max(0, graph.componentCount - 1) * (mode === "constraint-solver" ? 42 : 18) -
      graph.outsideLargest * (mode === "constraint-solver" ? 14 : 4) +
      graph.cycleSurplus * (mode === "constraint-solver" ? 78 : 44) +
      graph.richNeighborCount * (mode === "constraint-solver" ? 24 : 15) +
      graph.multiNeighborCount * (mode === "constraint-solver" ? 14 : 4) -
      graph.leafCount * (mode === "constraint-solver" ? 10 : 7);
    if (score > bestScore) {
      bestIds = ids;
      bestScore = score;
    }
  }

  const gears = result.gears.filter((gear) => bestIds.has(gear.id));
  const edges = result.edges.filter((edge) => bestIds.has(edge.a) && bestIds.has(edge.b));
  if (gears.length === 0) return { gears: [], edges: [] };

  const bounds = boundsOfGears(gears);
  const offsetX = HERO_X_MIN - bounds.minX + 24;
  const desiredCenterY = mode === "constraint-solver" ? 150 : 126;
  const currentCenterY = (bounds.minY + bounds.maxY) * 0.5;
  const offsetY = desiredCenterY - currentCenterY;

  return remapBackdrop({
    gears: gears.map((gear) => ({
      ...gear,
      center: {
        x: gear.center.x + offsetX,
        y: gear.center.y + offsetY,
      },
    })),
    edges,
  });
}

function candidateScore(result: BackdropGeneratorResult, mode: Mode): number {
  if (result.gears.length === 0) return -Infinity;
  const bounds = boundsOfGears(result.gears);
  const edgeCount = result.edges.length;
  const graph = graphStats(result.gears, result.edges);
  return (
    result.gears.length * 120 +
    edgeCount * 60 +
    graph.largestComponent * (mode === "constraint-solver" ? 170 : 90) -
    Math.max(0, graph.componentCount - 1) * (mode === "constraint-solver" ? 170 : 65) -
    graph.outsideLargest * (mode === "constraint-solver" ? 32 : 10) +
    graph.cycleSurplus * (mode === "constraint-solver" ? 260 : 160) +
    graph.multiNeighborCount * (mode === "constraint-solver" ? 120 : 24) +
    graph.richNeighborCount * (mode === "constraint-solver" ? 160 : 110) -
    graph.leafCount * (mode === "constraint-solver" ? 65 : 28) -
    Math.max(0, bounds.maxX - (HERO_X_MAX + 120)) * 0.08 -
    Math.max(0, HERO_X_MIN - bounds.minX) * 0.08 -
    Math.max(0, bounds.maxY - (HERO_Y_MAX + 30)) * (mode === "constraint-solver" ? 1.1 : 0.45) -
    Math.max(0, HERO_Y_MIN - bounds.minY) * 0.6
  );
}

export function generatePresentedSpecBackdrop(
  options: BackdropGeneratorOptions & { mode: Mode; targetCounts: number[]; retrySeeds: number },
): BackdropGeneratorResult {
  let best: BackdropGeneratorResult = { gears: [], edges: [] };
  let bestScore = -Infinity;

  for (let retry = 0; retry < options.retrySeeds; retry += 1) {
      const seed = options.seed + retry * 97_409;
    for (const targetCount of options.targetCounts) {
      const raw = generateSpecBackdrop({ mode: options.mode, seed, targetCount });
      const trimmed = trimBackdropToHero(raw, options.mode);
      const score = candidateScore(trimmed, options.mode);
      if (score > bestScore) {
        best = trimmed;
        bestScore = score;
      }
    }
  }

  return best;
}
