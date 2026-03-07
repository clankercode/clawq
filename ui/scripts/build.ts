import { copyFileSync, existsSync, mkdirSync, readdirSync, rmSync, watch } from "node:fs";
import { join, resolve } from "node:path";

const rootDir = resolve(import.meta.dir, "..");
const distDir = join(rootDir, "dist");
const watchMode = Bun.argv.includes("--watch");

function ensureCleanDist() {
  rmSync(distDir, { force: true, recursive: true });
  mkdirSync(distDir, { recursive: true });
}

async function buildOnce() {
  ensureCleanDist();
  const result = await Bun.build({
    entrypoints: [join(rootDir, "src/main.ts")],
    format: "iife",
    target: "browser",
    outdir: distDir,
    minify: !watchMode,
    sourcemap: watchMode ? "external" : "none",
  });
  if (!result.success) {
    throw new Error(result.logs.map((log) => log.message).join("\n"));
  }
  const builtJs = join(distDir, "main.js");
  const chatJs = join(distDir, "chat.js");
  if (existsSync(builtJs)) {
    rmSync(chatJs, { force: true });
    await Bun.write(chatJs, Bun.file(builtJs));
    rmSync(builtJs, { force: true });
  }
  copyFileSync(join(rootDir, "index.html"), join(distDir, "index.html"));
  copyFileSync(join(rootDir, "styles/chat.css"), join(distDir, "chat.css"));
  console.log(`[ui] built ${distDir}`);
}

function collectWatchRoots(path: string): string[] {
  const roots: string[] = [path];
  for (const entry of readdirSync(path, { withFileTypes: true })) {
    if (!entry.isDirectory()) {
      continue;
    }
    if (entry.name === "dist" || entry.name === "node_modules") {
      continue;
    }
    roots.push(...collectWatchRoots(join(path, entry.name)));
  }
  return roots;
}

function watchTree(path: string) {
  watch(path, (_eventType, filename) => {
    if (!filename) {
      return;
    }
    void buildOnce().catch((error) => {
      console.error("[ui] build failed");
      console.error(error instanceof Error ? error.message : String(error));
    });
  });
}

await buildOnce();

if (watchMode) {
  console.log("[ui] watching for changes...");
  for (const root of collectWatchRoots(rootDir)) {
    watchTree(root);
  }
  await new Promise(() => undefined);
}
