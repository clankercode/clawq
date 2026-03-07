import type { BackdropGeneratorFn } from "./types.ts";
import { generatePresentedSpecBackdrop } from "./spec_present.ts";
import defaultTopologyFirstBackdrop from "./data/topology_first.default.json" with { type: "json" };
import { assertBackdropResult, cloneBackdrop } from "./result_utils.ts";
import {
  DEFAULT_SPEC_BACKDROP_SEED,
  DEFAULT_SPEC_BACKDROP_TARGET_COUNT,
  specPresentationOptions,
} from "./spec_defaults.ts";

const defaultBackdrop = assertBackdropResult(defaultTopologyFirstBackdrop, "topology-first default fixture");

export const generateTopologyFirstBackdrop: BackdropGeneratorFn = ({ seed, targetCount = 72 }) =>
  seed === DEFAULT_SPEC_BACKDROP_SEED && targetCount === DEFAULT_SPEC_BACKDROP_TARGET_COUNT
    ? cloneBackdrop(defaultBackdrop)
    : assertBackdropResult(
        generatePresentedSpecBackdrop({
          seed,
          targetCount,
          ...specPresentationOptions("topology-first", targetCount),
        }),
        "topology-first generated backdrop",
      );
