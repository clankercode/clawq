type StyleState = {
  color: string | null;
  bold: boolean;
};

const ANSI_PATTERN = /\u001b\[([0-9;]*)m/g;

function escapeHtml(text: string): string {
  return text
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;");
}

function colorForCode(code: number): string | null {
  const palette = new Map<number, string>([
    [30, "#1f2328"],
    [31, "#c64537"],
    [32, "#2f7d4b"],
    [33, "#8e6110"],
    [34, "#3058a6"],
    [35, "#7d4689"],
    [36, "#1e6f72"],
    [37, "#d8d2c4"],
    [90, "#6b7280"],
    [91, "#ef6a5b"],
    [92, "#58b26d"],
    [93, "#d7a045"],
    [94, "#6f94ff"],
    [95, "#bb74e3"],
    [96, "#46b7bc"],
    [97, "#fff8ea"],
  ]);
  return palette.get(code) ?? null;
}

function toStyleAttr(state: StyleState): string {
  const rules: string[] = [];
  if (state.color) {
    rules.push(`color:${state.color}`);
  }
  if (state.bold) {
    rules.push("font-weight:700");
  }
  return rules.join(";");
}

function applyCode(state: StyleState, code: number) {
  if (code === 0) {
    state.color = null;
    state.bold = false;
    return;
  }
  if (code === 1) {
    state.bold = true;
    return;
  }
  if (code === 22) {
    state.bold = false;
    return;
  }
  if (code === 39) {
    state.color = null;
    return;
  }
  const color = colorForCode(code);
  if (color) {
    state.color = color;
  }
}

export function ansiToHtml(text: string): string {
  const state: StyleState = { color: null, bold: false };
  let cursor = 0;
  let html = "";
  let match: RegExpExecArray | null = null;

  while ((match = ANSI_PATTERN.exec(text)) !== null) {
    const plain = text.slice(cursor, match.index);
    if (plain) {
      const escaped = escapeHtml(plain);
      const styleAttr = toStyleAttr(state);
      html += styleAttr ? `<span style="${styleAttr}">${escaped}</span>` : escaped;
    }
    const codes = match[1] === "" ? [0] : match[1].split(";").map((value) => Number.parseInt(value, 10));
    for (const code of codes) {
      applyCode(state, Number.isNaN(code) ? 0 : code);
    }
    cursor = match.index + match[0].length;
  }

  const tail = text.slice(cursor);
  if (tail) {
    const escaped = escapeHtml(tail);
    const styleAttr = toStyleAttr(state);
    html += styleAttr ? `<span style="${styleAttr}">${escaped}</span>` : escaped;
  }

  return html.replaceAll("\n", "<br>");
}

export function escapePlainText(text: string): string {
  return escapeHtml(text);
}
