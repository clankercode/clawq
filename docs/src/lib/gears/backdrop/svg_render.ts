import { buildGearPath } from "../path.ts";
import type { BackdropGeneratorResult } from "./types.ts";
import {
  backdropBounds,
  buildSolvedDebugGears,
  buildTrueMeshEdges,
  componentStats,
  DEBUG_TOOTH_TUNING,
} from "./debug_spec.ts";

const palette = [
  { line: "rgba(235, 207, 132, 0.74)", fill: "rgba(201, 159, 66, 0.10)" },
  { line: "rgba(144, 208, 223, 0.72)", fill: "rgba(55, 116, 143, 0.10)" },
  { line: "rgba(198, 174, 203, 0.68)", fill: "rgba(125, 91, 154, 0.10)" },
];

function escapeXml(value: string | number): string {
  return String(value)
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;");
}

export function summarizeBackdrop(result: BackdropGeneratorResult) {
  const stats = componentStats(result.gears, result.edges);
  return {
    gears: result.gears.length,
    edges: result.edges.length,
    componentCount: stats.componentCount,
    largestComponent: stats.largestComponent,
    componentSizes: stats.sizes,
  };
}

export function renderBackdropSvg(options: {
  title: string;
  subtitle: string;
  draft: BackdropGeneratorResult;
}): string {
  const { title, subtitle, draft } = options;
  const solved = buildSolvedDebugGears(draft.gears, draft.edges);
  const edges = buildTrueMeshEdges(draft.gears, draft.edges);
  const summary = summarizeBackdrop(draft);
  const bounds = solved.length > 0 ? backdropBounds(solved, 72) : { minX: 0, minY: 0, width: 1200, height: 700 };
  const headerHeight = 112;
  const width = Math.max(1200, Math.ceil(bounds.width));
  const height = Math.max(680, Math.ceil(bounds.height + headerHeight));
  const viewMinY = bounds.minY - headerHeight;

  const renderedGears = solved
    .slice()
    .sort((left, right) => left.center.y - right.center.y)
    .map((gear, index) => ({
      gear,
      path: buildGearPath(gear, DEBUG_TOOTH_TUNING),
      colors: palette[index % palette.length],
    }));

  const metrics = [
    `gears ${summary.gears}`,
    `edges ${summary.edges}`,
    `components ${summary.componentCount}`,
    `largest ${summary.largestComponent}`,
  ].join("  |  ");

  return `<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" viewBox="${bounds.minX} ${viewMinY} ${width} ${height}" role="img" aria-label="${escapeXml(title)}">
  <defs>
    <linearGradient id="bg-gradient" x1="0" y1="0" x2="1" y2="1">
      <stop offset="0%" stop-color="#070811" />
      <stop offset="55%" stop-color="#09071a" />
      <stop offset="100%" stop-color="#04050d" />
    </linearGradient>
    <pattern id="dot-grid" width="32" height="32" patternUnits="userSpaceOnUse">
      <circle cx="1.5" cy="1.5" r="0.8" fill="rgba(232,186,84,0.12)" />
    </pattern>
  </defs>
  <rect x="${bounds.minX}" y="${viewMinY}" width="${width}" height="${height}" fill="url(#bg-gradient)" rx="28" />
  <rect x="${bounds.minX}" y="${viewMinY}" width="${width}" height="${height}" fill="url(#dot-grid)" rx="28" />
  <text x="${bounds.minX + 28}" y="${viewMinY + 42}" fill="#d9b657" font-size="18" font-family="Georgia, serif" letter-spacing="1.6">${escapeXml(title)}</text>
  <text x="${bounds.minX + 28}" y="${viewMinY + 70}" fill="#d0c8b8" font-size="13" font-family="Georgia, serif">${escapeXml(subtitle)}</text>
  <text x="${bounds.minX + width - 28}" y="${viewMinY + 42}" text-anchor="end" fill="#938970" font-size="12" font-family="monospace">${escapeXml(metrics)}</text>
  <g>
    ${edges
      .map(
        (edge) =>
          `<line x1="${edge.x1}" y1="${edge.y1}" x2="${edge.x2}" y2="${edge.y2}" stroke="rgba(133,197,255,0.55)" stroke-width="1" stroke-dasharray="5 5" vector-effect="non-scaling-stroke" />`,
      )
      .join("\n    ")}
  </g>
  <g>
    ${renderedGears
      .map(({ gear, path, colors }) => `<g transform="translate(${gear.center.x} ${gear.center.y}) rotate(${gear.phaseDeg})">
      <path d="${path}" fill="${colors.fill}" stroke="${colors.line}" stroke-width="1.7" stroke-linejoin="round" vector-effect="non-scaling-stroke" />
      <circle r="${gear.pitchRadius}" fill="none" stroke="rgba(121,215,222,0.28)" stroke-width="0.8" vector-effect="non-scaling-stroke" />
      <circle r="${gear.holeRadius}" fill="none" stroke="rgba(235,207,132,0.36)" stroke-width="0.8" vector-effect="non-scaling-stroke" />
    </g>`)
      .join("\n    ")}
  </g>
</svg>`;
}
