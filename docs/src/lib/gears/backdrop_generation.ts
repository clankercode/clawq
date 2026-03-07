import { DEFAULT_HERO_GEAR_ALGORITHM, type BackdropAlgorithm } from "./backdrop_algorithms.ts";
import { BACKDROP_GENERATORS } from "./backdrop/registry.ts";

export { HERO_GEAR_CIRCULAR_PITCH } from "./backdrop/shared.ts";
export type {
  BackdropGeneratorFn,
  BackdropGeneratorOptions,
  BackdropGeneratorResult,
  DraftGear,
  DraftMeshEdge,
  Point,
} from "./backdrop/types.ts";

export function generateHeroBackdropDraft(options: {
  algorithm?: BackdropAlgorithm;
  seed?: number;
  targetCount?: number;
} = {}) {
  const algorithm = options.algorithm ?? DEFAULT_HERO_GEAR_ALGORITHM;
  const seed = options.seed ?? 0x6a11cf;
  const targetCount =
    options.targetCount ??
    (algorithm === "row-debug" || algorithm === "sine-debug" ? 999 : algorithm === "chaos-fill" || algorithm === "chaos-cluster" || algorithm === "hex-web" ? 116 : 90);
  return BACKDROP_GENERATORS[algorithm]({ seed, targetCount });
}
