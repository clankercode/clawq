export const BACKDROP_ALGORITHMS = [
  "branch",
  "chaos-bridged",
  "chaos-cavity",
  "chaos-cluster",
  "cell-fill",
  "chaos-fill",
  "hex-web",
  "lattice",
  "radial",
  "ring-web",
  "row-debug",
  "sine-debug",
  "weave",
] as const;

export const DEFAULT_HERO_GEAR_ALGORITHM = "chaos-cluster" as const;

export type BackdropAlgorithm = (typeof BACKDROP_ALGORITHMS)[number];
