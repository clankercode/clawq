type CanvasPayload = {
  viewbox: {
    minX: number;
    minY: number;
    width: number;
    height: number;
    scale: number;
    panYRatio: number;
  };
  gears: Array<{
    id: string;
    center: { x: number; y: number };
    path: string;
    line: string;
    fill: string;
    stroke: number;
    insetStroke: number;
    pitchRadius: number;
    innerRingRadius: number;
    holeRadius: number;
    phaseDeg: number;
    periodSec: number;
    rotationDirection: "cw" | "ccw";
  }>;
};

type CanvasState = {
  canvas: HTMLCanvasElement;
  ctx: CanvasRenderingContext2D;
  payload: CanvasPayload;
  pathById: Map<string, Path2D>;
  resizeObserver: ResizeObserver;
  rafId: number;
};

function parsePayload(canvas: HTMLCanvasElement): CanvasPayload | null {
  const payloadId = canvas.dataset.canvasPayloadId;
  if (!payloadId) return null;
  const script = document.getElementById(payloadId);
  if (!(script instanceof HTMLScriptElement)) return null;
  try {
    return JSON.parse(script.textContent ?? "") as CanvasPayload;
  } catch (error) {
    console.error("[hero-gear-canvas] failed to parse payload", error);
    return null;
  }
}

function computeViewport(state: CanvasState) {
  const backdrop = state.canvas.closest(".hero-gear-backdrop");
  if (!(backdrop instanceof HTMLDivElement)) return null;

  const cssWidth = backdrop.clientWidth;
  const cssHeight = backdrop.clientHeight;
  if (!(cssWidth > 0) || !(cssHeight > 0)) return null;

  const dpr = window.devicePixelRatio || 1;
  const pixelWidth = Math.max(1, Math.round(cssWidth * dpr));
  const pixelHeight = Math.max(1, Math.round(cssHeight * dpr));
  if (state.canvas.width !== pixelWidth || state.canvas.height !== pixelHeight) {
    state.canvas.width = pixelWidth;
    state.canvas.height = pixelHeight;
  }

  const { minX, minY, width, height, scale, panYRatio } = state.payload.viewbox;
  const scaledHeight = height * scale;
  const scaledWidth = width * scale;
  const scaledOriginX = minX - (scaledWidth - width) * 0.5;
  const scaledOriginY = minY - (scaledHeight - height) * 0.5 - scaledHeight * panYRatio;
  const worldWidth = (cssWidth / cssHeight) * scaledHeight;
  const worldOriginX = scaledOriginX + (scaledWidth - worldWidth) * 0.5;

  return {
    cssWidth,
    cssHeight,
    dpr,
    worldOriginX,
    worldOriginY: scaledOriginY,
    worldWidth,
    worldHeight: scaledHeight,
  };
}

function draw(state: CanvasState, now: number): void {
  const viewport = computeViewport(state);
  if (!viewport) {
    state.rafId = requestAnimationFrame((time) => draw(state, time));
    return;
  }

  const { ctx } = state;
  ctx.setTransform(1, 0, 0, 1, 0, 0);
  ctx.clearRect(0, 0, state.canvas.width, state.canvas.height);

  const scaleX = (viewport.cssWidth * viewport.dpr) / viewport.worldWidth;
  const scaleY = (viewport.cssHeight * viewport.dpr) / viewport.worldHeight;
  ctx.setTransform(scaleX, 0, 0, scaleY, -viewport.worldOriginX * scaleX, -viewport.worldOriginY * scaleY);

  for (const gear of state.payload.gears) {
    const path = state.pathById.get(gear.id);
    if (!path) continue;

    const direction = gear.rotationDirection === "cw" ? 1 : -1;
    const angle = (gear.phaseDeg * Math.PI) / 180 + direction * ((now / 1000) * (2 * Math.PI / gear.periodSec));

    ctx.save();
    ctx.translate(gear.center.x, gear.center.y);
    ctx.rotate(angle);

    ctx.fillStyle = gear.fill;
    ctx.fill(path);

    ctx.lineWidth = gear.stroke;
    ctx.strokeStyle = gear.line;
    ctx.stroke(path);

    ctx.lineWidth = gear.insetStroke;
    ctx.strokeStyle = gear.line;
    ctx.beginPath();
    ctx.arc(0, 0, gear.innerRingRadius, 0, Math.PI * 2);
    ctx.stroke();

    ctx.beginPath();
    ctx.arc(0, 0, gear.holeRadius, 0, Math.PI * 2);
    ctx.stroke();

    ctx.restore();
  }

  state.rafId = requestAnimationFrame((time) => draw(state, time));
}

function attachCanvas(canvas: HTMLCanvasElement): void {
  const ctx = canvas.getContext("2d");
  const payload = parsePayload(canvas);
  if (!ctx || !payload) return;

  const state: CanvasState = {
    canvas,
    ctx,
    payload,
    pathById: new Map(payload.gears.map((gear) => [gear.id, new Path2D(gear.path)])),
    resizeObserver: new ResizeObserver(() => {
      computeViewport(state);
    }),
    rafId: 0,
  };

  const backdrop = canvas.closest(".hero-gear-backdrop");
  if (backdrop instanceof HTMLDivElement) {
    state.resizeObserver.observe(backdrop);
  }

  state.rafId = requestAnimationFrame((time) => draw(state, time));
}

for (const node of document.querySelectorAll<HTMLCanvasElement>(".hero-gear-canvas[data-canvas-payload-id]")) {
  attachCanvas(node);
}
