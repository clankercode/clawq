import { mkdir, writeFile } from "node:fs/promises";
import { resolve } from "node:path";
import { readFile } from "node:fs/promises";

import { DEFAULT_SPEC_BACKDROP_SEED, DEFAULT_SPEC_BACKDROP_TARGET_COUNT, specPresentationOptions } from "../src/lib/gears/backdrop/spec_defaults.ts";
import { generateSpecBackdrop } from "../src/lib/gears/backdrop/spec_core.ts";
import { generatePresentedSpecBackdrop } from "../src/lib/gears/backdrop/spec_present.ts";
import { renderBackdropSvg, summarizeBackdrop } from "../src/lib/gears/backdrop/svg_render.ts";

const modes = ["topology-first", "constraint-solver"];

function parseArgs() {
  const args = new Map();
  for (const arg of process.argv.slice(2)) {
    const [key, value] = arg.split("=");
    if (key.startsWith("--")) args.set(key.slice(2), value ?? "1");
  }
  return args;
}

async function readBackdropJson(filePath) {
  return JSON.parse(await readFile(filePath, "utf8"));
}

async function main() {
  const args = parseArgs();
  const seed = Number(args.get("seed") ?? DEFAULT_SPEC_BACKDROP_SEED);
  const targetCount = Number(args.get("targetCount") ?? DEFAULT_SPEC_BACKDROP_TARGET_COUNT);
  const outputDir = resolve(args.get("out") ?? "./.artifacts/spec-backdrops");
  const inputArg = args.get("input");

  await mkdir(outputDir, { recursive: true });

  const manifest = [];
  if (inputArg) {
    for (const inputPath of inputArg.split(",").map((value) => resolve(value.trim())).filter(Boolean)) {
      const draft = await readBackdropJson(inputPath);
      const baseName = inputPath.split("/").at(-1)?.replace(/\.json$/u, "") ?? "layout";
      const outputPath = resolve(outputDir, `${baseName}.svg`);
      const svg = renderBackdropSvg({
        title: baseName,
        subtitle: `rendered from ${inputPath}`,
        draft,
      });
      await writeFile(outputPath, `${svg}\n`, "utf8");
      manifest.push({ source: inputPath, outputPath, ...summarizeBackdrop(draft) });
    }
  } else {
    for (const mode of modes) {
      const raw = generateSpecBackdrop({ mode, seed, targetCount });
      const presented = generatePresentedSpecBackdrop({ mode, seed, targetCount, ...specPresentationOptions(mode, targetCount) });
      const rawSvg = renderBackdropSvg({
        title: `${mode} raw`,
        subtitle: `seed ${seed} | target ${targetCount} | direct generateSpecBackdrop() output`,
        draft: raw,
      });
      const presentedSvg = renderBackdropSvg({
        title: `${mode} presented`,
        subtitle: `seed ${seed} | target ${targetCount} | hero presentation output`,
        draft: presented,
      });

      const rawPath = resolve(outputDir, `${mode}.raw.svg`);
      const presentedPath = resolve(outputDir, `${mode}.presented.svg`);
      await writeFile(rawPath, `${rawSvg}\n`, "utf8");
      await writeFile(presentedPath, `${presentedSvg}\n`, "utf8");

      manifest.push({
        mode,
        seed,
        targetCount,
        raw: { path: rawPath, ...summarizeBackdrop(raw) },
        presented: { path: presentedPath, ...summarizeBackdrop(presented) },
      });
    }
  }

  const manifestPath = resolve(outputDir, "manifest.json");
  await writeFile(manifestPath, `${JSON.stringify(manifest, null, 2)}\n`, "utf8");

  for (const entry of manifest) {
    if (entry.mode) {
      console.log(`${entry.mode}: raw ${entry.raw.gears}/${entry.raw.edges} -> ${entry.raw.path}`);
      console.log(`${entry.mode}: presented ${entry.presented.gears}/${entry.presented.edges} -> ${entry.presented.path}`);
    } else {
      console.log(`${entry.source}: ${entry.gears} gears / ${entry.edges} edges -> ${entry.outputPath}`);
    }
  }
  console.log(`manifest -> ${manifestPath}`);
}

await main();
