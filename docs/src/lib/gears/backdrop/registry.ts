import type { BackdropAlgorithm } from "../backdrop_algorithms.ts";
import type { BackdropGeneratorFn } from "./types.ts";
import { generateBranchBackdrop } from "./branch.ts";
import { generateChaosBridgedBackdrop } from "./chaos_bridged.ts";
import { generateChaosCavityBackdrop } from "./chaos_cavity.ts";
import { generateChaosClusterBackdrop } from "./chaos_cluster.ts";
import { generateCellFillBackdrop } from "./cell_fill.ts";
import { generateConstraintSolverBackdrop } from "./constraint_solver.ts";
import { generateChaosFillBackdrop } from "./chaos_fill.ts";
import { generateHexWebBackdrop } from "./hex_web.ts";
import { generateLatticeBackdrop } from "./lattice.ts";
import { generateRadialBackdrop } from "./radial.ts";
import { generateRingWebBackdrop } from "./ring_web.ts";
import { generateRowDebugBackdrop } from "./row_debug.ts";
import { generateSineDebugBackdrop } from "./sine_debug.ts";
import { generateTopologyFirstBackdrop } from "./topology_first.ts";
import { generateWeaveBackdrop } from "./weave.ts";

export const BACKDROP_GENERATORS: Record<BackdropAlgorithm, BackdropGeneratorFn> = {
  branch: generateBranchBackdrop,
  "chaos-bridged": generateChaosBridgedBackdrop,
  "chaos-cavity": generateChaosCavityBackdrop,
  "chaos-cluster": generateChaosClusterBackdrop,
  "cell-fill": generateCellFillBackdrop,
  "chaos-fill": generateChaosFillBackdrop,
  "constraint-solver": generateConstraintSolverBackdrop,
  "hex-web": generateHexWebBackdrop,
  lattice: generateLatticeBackdrop,
  radial: generateRadialBackdrop,
  "ring-web": generateRingWebBackdrop,
  "row-debug": generateRowDebugBackdrop,
  "sine-debug": generateSineDebugBackdrop,
  "topology-first": generateTopologyFirstBackdrop,
  weave: generateWeaveBackdrop,
};
