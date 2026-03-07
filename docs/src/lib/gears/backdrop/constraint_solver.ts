import type { BackdropGeneratorFn } from "./types.ts";
import { generatePresentedSpecBackdrop } from "./spec_present.ts";
import defaultConstraintSolverBackdrop from "./data/constraint_solver.default.json" with { type: "json" };
import { assertBackdropResult, cloneBackdrop } from "./result_utils.ts";
import {
  DEFAULT_SPEC_BACKDROP_SEED,
  DEFAULT_SPEC_BACKDROP_TARGET_COUNT,
  specPresentationOptions,
} from "./spec_defaults.ts";

const defaultBackdrop = assertBackdropResult(defaultConstraintSolverBackdrop, "constraint-solver default fixture");

export const generateConstraintSolverBackdrop: BackdropGeneratorFn = ({ seed, targetCount = 72 }) =>
  seed === DEFAULT_SPEC_BACKDROP_SEED && targetCount === DEFAULT_SPEC_BACKDROP_TARGET_COUNT
    ? cloneBackdrop(defaultBackdrop)
    : assertBackdropResult(
        generatePresentedSpecBackdrop({
          seed,
          targetCount,
          ...specPresentationOptions("constraint-solver", targetCount),
        }),
        "constraint-solver generated backdrop",
      );
