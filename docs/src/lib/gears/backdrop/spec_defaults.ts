export type SpecBackdropMode = "topology-first" | "constraint-solver";

export const DEFAULT_SPEC_BACKDROP_SEED = 0x6a11cf;
export const DEFAULT_SPEC_BACKDROP_TARGET_COUNT = 72;

export function specPresentationOptions(mode: SpecBackdropMode, targetCount: number) {
  if (mode === "constraint-solver") {
    return {
      mode,
      targetCounts: [targetCount],
      retrySeeds: 1,
    };
  }

  return {
    mode,
    targetCounts: [Math.max(52, Math.floor(targetCount * 0.78)), targetCount],
    retrySeeds: 3,
  };
}
