// Render the social preview card to a 1200x630 PNG using a headless browser.
//
//   npm run render:og
//
// Source:  scripts/og-image.html   (edit this to change the card)
// Output:  public/og-image.png      (committed; referenced by SeoMeta.astro)
//
// Requires Playwright's chromium:  npx playwright install chromium
import { chromium } from "playwright";
import { fileURLToPath, pathToFileURL } from "node:url";
import { dirname, resolve } from "node:path";

const here = dirname(fileURLToPath(import.meta.url));
const htmlPath = resolve(here, "og-image.html");
const outPath = resolve(here, "..", "public", "og-image.png");

const WIDTH = 1200;
const HEIGHT = 630;

const browser = await chromium.launch();
try {
  const page = await browser.newPage({
    viewport: { width: WIDTH, height: HEIGHT },
    deviceScaleFactor: 1,
  });
  await page.goto(pathToFileURL(htmlPath).href, { waitUntil: "networkidle" });
  // Ensure web fonts are fully loaded before capturing.
  await page.evaluate(() => document.fonts.ready);
  await page.screenshot({
    path: outPath,
    clip: { x: 0, y: 0, width: WIDTH, height: HEIGHT },
  });
  console.log(`Wrote ${outPath} (${WIDTH}x${HEIGHT})`);
} finally {
  await browser.close();
}
