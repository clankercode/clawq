import type { BackdropGeneratorResult, DraftGear } from "./types.ts";

export function cloneBackdrop(result: BackdropGeneratorResult): BackdropGeneratorResult {
  return {
    gears: result.gears.map((gear) => ({
      ...gear,
      center: { ...gear.center },
    })),
    edges: result.edges.map((edge) => ({ ...edge })),
  };
}

export function isNonEmptyBackdrop(result: BackdropGeneratorResult): boolean {
  return result.gears.length > 0 && result.edges.length > 0;
}

function isFiniteNumber(value: unknown): value is number {
  return typeof value === "number" && Number.isFinite(value);
}

function isValidGear(gear: DraftGear): boolean {
  return (
    typeof gear.id === "string" &&
    gear.id.length > 0 &&
    Number.isInteger(gear.teeth) &&
    gear.teeth > 0 &&
    isFiniteNumber(gear.pitchRadius) &&
    gear.pitchRadius > 0 &&
    isFiniteNumber(gear.outerRadius) &&
    gear.outerRadius > 0 &&
    isFiniteNumber(gear.center?.x) &&
    isFiniteNumber(gear.center?.y) &&
    (gear.phaseTurn == null || isFiniteNumber(gear.phaseTurn)) &&
    (gear.parity === 0 || gear.parity === 1) &&
    Number.isInteger(gear.appearIndex)
  );
}

export function assertBackdropResult(result: BackdropGeneratorResult, label: string): BackdropGeneratorResult {
  if (!result || !Array.isArray(result.gears) || !Array.isArray(result.edges)) {
    throw new Error(`${label} returned an invalid backdrop payload.`);
  }

  const gearIds = new Set<string>();
  for (const gear of result.gears) {
    if (!isValidGear(gear)) {
      throw new Error(`${label} returned an invalid gear entry.`);
    }
    if (gearIds.has(gear.id)) {
      throw new Error(`${label} returned duplicate gear id '${gear.id}'.`);
    }
    gearIds.add(gear.id);
  }

  for (const gear of result.gears) {
    if (gear.parentId && !gearIds.has(gear.parentId)) {
      throw new Error(`${label} returned gear '${gear.id}' with missing parent '${gear.parentId}'.`);
    }
  }

  const edgeKeys = new Set<string>();
  for (const edge of result.edges) {
    if (typeof edge.a !== "string" || typeof edge.b !== "string" || edge.a.length === 0 || edge.b.length === 0) {
      throw new Error(`${label} returned an invalid edge entry.`);
    }
    if (edge.a === edge.b) {
      throw new Error(`${label} returned a self-edge for '${edge.a}'.`);
    }
    if (!gearIds.has(edge.a) || !gearIds.has(edge.b)) {
      throw new Error(`${label} returned edge '${edge.a}<->${edge.b}' with missing endpoint.`);
    }
    const key = edge.a < edge.b ? `${edge.a}:${edge.b}` : `${edge.b}:${edge.a}`;
    if (edgeKeys.has(key)) {
      throw new Error(`${label} returned duplicate edge '${key}'.`);
    }
    edgeKeys.add(key);
  }

  return result;
}
