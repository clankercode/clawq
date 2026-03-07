import { DEFAULT_HERO_GEAR_ALGORITHM, type BackdropAlgorithm } from "./backdrop_algorithms.ts";
import { BACKDROP_GENERATORS } from "./backdrop/registry.ts";
import { assertBackdropResult, cloneBackdrop, isNonEmptyBackdrop } from "./backdrop/result_utils.ts";
import type { BackdropGeneratorResult } from "./backdrop/types.ts";

export { HERO_GEAR_CIRCULAR_PITCH } from "./backdrop/shared.ts";
export type {
  BackdropGeneratorFn,
  BackdropGeneratorOptions,
  BackdropGeneratorResult,
  DraftGear,
  DraftMeshEdge,
  Point,
} from "./backdrop/types.ts";

const generatedBackdropCache = new Map<string, BackdropGeneratorResult>();
const MAX_BACKDROP_CACHE_ENTRIES = 32;

function cacheBackdrop(key: string, result: BackdropGeneratorResult): void {
  if (!isNonEmptyBackdrop(result)) return;
  if (generatedBackdropCache.size >= MAX_BACKDROP_CACHE_ENTRIES) {
    const oldestKey = generatedBackdropCache.keys().next().value;
    if (oldestKey) generatedBackdropCache.delete(oldestKey);
  }
  generatedBackdropCache.set(key, cloneBackdrop(result));
}

export function generateHeroBackdropDraft(options: {
  algorithm?: BackdropAlgorithm;
  seed?: number;
  targetCount?: number;
} = {}) {
  const algorithm = options.algorithm ?? DEFAULT_HERO_GEAR_ALGORITHM;
  const seed = options.seed ?? 0x6a11cf;
  const targetCount =
    options.targetCount ??
    (algorithm === "row-debug" || algorithm === "sine-debug"
      ? 999
      : algorithm === "chaos-fill" || algorithm === "chaos-cluster" || algorithm === "hex-web"
        ? 116
        : algorithm === "topology-first"
          ? 72
        : algorithm === "constraint-solver"
            ? 72
            : 90);
  const cacheKey = `${algorithm}|${seed}|${targetCount}`;
  const cached = generatedBackdropCache.get(cacheKey);
  if (cached) return cloneBackdrop(cached);

  const generated = assertBackdropResult(
    BACKDROP_GENERATORS[algorithm]({ seed, targetCount }),
    `${algorithm} backdrop generator`,
  );
  cacheBackdrop(cacheKey, generated);
  return cloneBackdrop(generated);
}
