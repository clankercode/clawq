import { writeFile } from "node:fs/promises";
import { resolve } from "node:path";

import { generatePresentedSpecBackdrop } from "../src/lib/gears/backdrop/spec_present.ts";
import { generateSpecBackdrop } from "../src/lib/gears/backdrop/spec_core.ts";
import {
  DEFAULT_SPEC_BACKDROP_SEED,
  DEFAULT_SPEC_BACKDROP_TARGET_COUNT,
  specPresentationOptions,
} from "../src/lib/gears/backdrop/spec_defaults.ts";
import { assertBackdropResult } from "../src/lib/gears/backdrop/result_utils.ts";

const outputs = [
  {
    mode: "topology-first",
    outputPath: resolve("src/lib/gears/backdrop/data/topology_first.default.json"),
    rawOutputPath: resolve("src/lib/gears/backdrop/data/topology_first.raw.json"),
  },
  {
    mode: "constraint-solver",
    outputPath: resolve("src/lib/gears/backdrop/data/constraint_solver.default.json"),
    rawOutputPath: resolve("src/lib/gears/backdrop/data/constraint_solver.raw.json"),
  },
];

for (const { mode, outputPath, rawOutputPath } of outputs) {
  const rawBackdrop = assertBackdropResult(
    generateSpecBackdrop({
      mode,
      seed: DEFAULT_SPEC_BACKDROP_SEED,
      targetCount: DEFAULT_SPEC_BACKDROP_TARGET_COUNT,
    }),
    `${mode} regenerated raw backdrop`,
  );
  const backdrop = assertBackdropResult(
    generatePresentedSpecBackdrop({
      seed: DEFAULT_SPEC_BACKDROP_SEED,
      targetCount: DEFAULT_SPEC_BACKDROP_TARGET_COUNT,
      ...specPresentationOptions(mode, DEFAULT_SPEC_BACKDROP_TARGET_COUNT),
    }),
    `${mode} regenerated backdrop`,
  );

  await writeFile(rawOutputPath, `${JSON.stringify(rawBackdrop, null, 2)}\n`, "utf8");
  await writeFile(outputPath, `${JSON.stringify(backdrop, null, 2)}\n`, "utf8");
  console.log(`${mode}: raw ${rawBackdrop.gears.length} gears, ${rawBackdrop.edges.length} edges -> ${rawOutputPath}`);
  console.log(`${mode}: ${backdrop.gears.length} gears, ${backdrop.edges.length} edges -> ${outputPath}`);
}
