import type { ChatLog, NormalizedMessage, NormalizedToolCall } from "./types";
import { ansiToHtml } from "./ansi";

declare const marked: { parse: (md: string) => string };
declare const DOMPurify: { sanitize: (html: string) => string };

function renderMarkdown(text: string): string {
  try {
    const raw = marked.parse(text);
    return DOMPurify.sanitize(raw);
  } catch {
    return escapeHtml(text);
  }
}

function escapeHtml(text: string): string {
  return text
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;");
}

function makeCollapsible(summary: HTMLElement, body: HTMLElement, container: HTMLElement, startOpen = false) {
  if (!startOpen) {
    body.style.display = "none";
  }
  summary.style.cursor = "pointer";
  const chevron = document.createElement("span");
  chevron.className = "cv-chevron";
  chevron.textContent = startOpen ? "\u25BC" : "\u25B6";
  summary.prepend(chevron);
  summary.addEventListener("click", () => {
    const isOpen = body.style.display !== "none";
    body.style.display = isOpen ? "none" : "block";
    chevron.textContent = isOpen ? "\u25B6" : "\u25BC";
    container.classList.toggle("cv-open", !isOpen);
  });
}

export function renderTranscript(log: ChatLog): HTMLElement {
  const container = document.createElement("div");
  container.className = "cv-transcript";

  // System prompt
  if (log.systemPrompt) {
    const sysMsg: NormalizedMessage = {
      index: -1,
      role: "system",
      content: log.systemPrompt,
    };
    container.appendChild(renderSystemMessage(sysMsg));
  }

  // Build tool result lookup: toolCallId -> NormalizedMessage
  const toolResultMap = new Map<string, NormalizedMessage>();
  for (const msg of log.messages) {
    if (msg.role === "tool" && msg.toolCallId) {
      toolResultMap.set(msg.toolCallId, msg);
    }
  }

  // Build set of tool call IDs that have a matching assistant tool call
  const pairedToolCallIds = new Set<string>();
  for (const msg of log.messages) {
    if (msg.role === "assistant" && msg.toolCalls) {
      for (const tc of msg.toolCalls) {
        pairedToolCallIds.add(tc.id);
      }
    }
  }

  for (const msg of log.messages) {
    switch (msg.role) {
      case "system":
        container.appendChild(renderSystemMessage(msg));
        break;
      case "user":
        container.appendChild(renderUserMessage(msg));
        break;
      case "assistant":
        container.appendChild(renderAssistantMessage(msg, toolResultMap));
        break;
      case "tool":
        // Tool results paired with an assistant tool call are shown inline in the tool card;
        // render standalone only if orphaned (no matching assistant tool call)
        if (!msg.toolCallId || !pairedToolCallIds.has(msg.toolCallId)) {
          container.appendChild(renderToolResultStandalone(msg));
        }
        break;
      case "event":
        container.appendChild(renderEventMessage(msg));
        break;
    }
  }

  return container;
}

function renderSystemMessage(msg: NormalizedMessage): HTMLElement {
  const el = document.createElement("div");
  el.className = "cv-message cv-message--system";

  const header = document.createElement("div");
  header.className = "cv-message__header";
  header.innerHTML = `<span class="cv-role-label">System</span>`;

  const body = document.createElement("div");
  body.className = "cv-message__body";
  body.innerHTML = renderMarkdown(msg.content);

  makeCollapsible(header, body, el, msg.content.length < 200);

  el.append(header, body);
  return el;
}

function renderUserMessage(msg: NormalizedMessage): HTMLElement {
  const el = document.createElement("div");
  el.className = "cv-message cv-message--user";

  const header = document.createElement("div");
  header.className = "cv-message__header";
  const label = `<span class="cv-role-label">User</span>`;
  const ts = msg.createdAt ? `<span class="cv-timestamp">${formatTimestamp(msg.createdAt)}</span>` : "";
  header.innerHTML = label + ts;

  const body = document.createElement("div");
  body.className = "cv-message__body";
  body.innerHTML = renderMarkdown(msg.content);

  el.append(header, body);
  return el;
}

function renderAssistantMessage(msg: NormalizedMessage, toolResultMap: Map<string, NormalizedMessage>): HTMLElement {
  const el = document.createElement("div");
  el.className = "cv-message cv-message--assistant";

  const header = document.createElement("div");
  header.className = "cv-message__header";
  const label = `<span class="cv-role-label">Assistant</span>`;
  const ts = msg.createdAt ? `<span class="cv-timestamp">${formatTimestamp(msg.createdAt)}</span>` : "";
  header.innerHTML = label + ts;
  el.appendChild(header);

  // Thinking block
  if (msg.thinking) {
    el.appendChild(renderThinkingBlock(msg.thinking));
  }

  // Content
  if (msg.content.trim()) {
    const body = document.createElement("div");
    body.className = "cv-message__body";
    body.innerHTML = renderMarkdown(msg.content);
    el.appendChild(body);
  }

  // Tool calls
  if (msg.toolCalls && msg.toolCalls.length > 0) {
    const stack = document.createElement("div");
    stack.className = "cv-tool-stack";
    for (const tc of msg.toolCalls) {
      const result = toolResultMap.get(tc.id);
      stack.appendChild(renderToolCallCard(tc, result));
    }
    el.appendChild(stack);
  }

  return el;
}

function renderThinkingBlock(text: string): HTMLElement {
  const el = document.createElement("div");
  el.className = "cv-thinking";

  const header = document.createElement("div");
  header.className = "cv-thinking__header";
  header.innerHTML = `<span>Thinking trace</span>`;

  const body = document.createElement("div");
  body.className = "cv-thinking__body";
  body.textContent = text;

  makeCollapsible(header, body, el, false);

  el.append(header, body);
  return el;
}

function renderToolCallCard(tc: NormalizedToolCall, result?: NormalizedMessage): HTMLElement {
  const el = document.createElement("div");
  el.className = "cv-tool-card";

  const header = document.createElement("div");
  header.className = "cv-tool-card__header";

  const nameEl = document.createElement("span");
  nameEl.className = "cv-tool-card__name";
  nameEl.textContent = tc.name;

  const statusEl = document.createElement("span");
  statusEl.className = "cv-tool-card__status";
  if (result) {
    const isError = result.content.toLowerCase().startsWith("error");
    statusEl.textContent = isError ? "ERROR" : "DONE";
    statusEl.classList.add(isError ? "cv-tool-card__status--error" : "cv-tool-card__status--done");
  }

  header.append(nameEl, statusEl);

  const body = document.createElement("div");
  body.className = "cv-tool-card__body";

  // Arguments
  if (tc.arguments) {
    const argsEl = document.createElement("div");
    argsEl.className = "cv-tool-card__args";
    try {
      const parsed = JSON.parse(tc.arguments);
      argsEl.textContent = JSON.stringify(parsed, null, 2);
    } catch {
      argsEl.textContent = tc.arguments;
    }
    body.appendChild(argsEl);
  }

  // Result
  if (result) {
    const resultEl = document.createElement("div");
    resultEl.className = "cv-tool-card__result";
    if (result.toolName) {
      const toolLabel = document.createElement("div");
      toolLabel.className = "cv-tool-card__result-label";
      toolLabel.textContent = result.toolName;
      resultEl.appendChild(toolLabel);
    }
    const resultContent = document.createElement("div");
    resultContent.className = "cv-tool-card__result-content";
    resultContent.innerHTML = ansiToHtml(result.content);
    resultEl.appendChild(resultContent);
    body.appendChild(resultEl);
  }

  makeCollapsible(header, body, el, false);

  el.append(header, body);
  return el;
}

function renderToolResultStandalone(msg: NormalizedMessage): HTMLElement {
  const el = document.createElement("div");
  el.className = "cv-message cv-message--tool";

  const header = document.createElement("div");
  header.className = "cv-message__header";
  header.innerHTML = `<span class="cv-role-label">Tool${msg.toolName ? `: ${escapeHtml(msg.toolName)}` : ""}</span>`;

  const body = document.createElement("div");
  body.className = "cv-message__body cv-tool-output";
  body.innerHTML = ansiToHtml(msg.content);

  el.append(header, body);
  return el;
}

function renderEventMessage(msg: NormalizedMessage): HTMLElement {
  const el = document.createElement("div");
  el.className = "cv-message cv-message--event";

  const body = document.createElement("div");
  body.className = "cv-message__body";
  body.textContent = msg.content;

  el.appendChild(body);
  return el;
}

function formatTimestamp(ts: string): string {
  try {
    const d = new Date(ts);
    return d.toLocaleString();
  } catch {
    return ts;
  }
}
