import { AssistantTurnView, UserTurnView } from "./messages";
import { installSlashPopover, type SlashCommand } from "./slash";
import { readSSE, type ServerEvent } from "./stream";
import { installVersionBanner } from "./version";

const SESSION_KEY = "clawq_ui_session_id";
const TOKEN_KEY = "clawq_ui_token";

const transcript = document.querySelector<HTMLDivElement>("#transcript");
const composerForm = document.querySelector<HTMLFormElement>("#composer-form");
const composerInput = document.querySelector<HTMLTextAreaElement>("#composer-input");
const sendButton = document.querySelector<HTMLButtonElement>("#send-btn");
const stopButton = document.querySelector<HTMLButtonElement>("#stop-btn");
const statusPill = document.querySelector<HTMLDivElement>("#status-pill");
const statusText = document.querySelector<HTMLSpanElement>("#status-text");
const slashPopover = document.querySelector<HTMLDivElement>("#slash-popover");
const pairingModal = document.querySelector<HTMLDivElement>("#pair-modal");
const pairingCode = document.querySelector<HTMLInputElement>("#pairing-code");
const pairingSubmit = document.querySelector<HTMLButtonElement>("#pairing-submit");
const pairingError = document.querySelector<HTMLParagraphElement>("#pairing-error");
const newSessionButton = document.querySelector<HTMLButtonElement>("#new-session-btn");
const themeToggle = document.querySelector<HTMLButtonElement>("#theme-toggle");
const versionBanner = document.querySelector<HTMLElement>("#version-banner");
const versionDismiss = document.querySelector<HTMLButtonElement>("#version-dismiss");
const versionReload = document.querySelector<HTMLButtonElement>("#version-reload");

if (
  !transcript ||
  !composerForm ||
  !composerInput ||
  !sendButton ||
  !stopButton ||
  !statusPill ||
  !statusText ||
  !slashPopover ||
  !pairingModal ||
  !pairingCode ||
  !pairingSubmit ||
  !pairingError ||
  !newSessionButton ||
  !versionBanner ||
  !versionDismiss ||
  !versionReload
) {
  throw new Error("UI boot failed: missing required DOM nodes");
}

let abortController: AbortController | null = null;
let retryAfterPair: (() => void) | null = null;

function getSessionId(): string {
  const existing = localStorage.getItem(SESSION_KEY);
  if (existing) {
    return existing;
  }
  const next = `web-${Math.random().toString(36).slice(2)}${Date.now().toString(36)}`;
  localStorage.setItem(SESSION_KEY, next);
  return next;
}

function resetSession() {
  localStorage.removeItem(SESSION_KEY);
  transcript.innerHTML = "";
  composerInput.focus();
}

function getToken(): string {
  return localStorage.getItem(TOKEN_KEY) ?? "";
}

function setToken(token: string) {
  localStorage.setItem(TOKEN_KEY, token);
}

function setStatus(state: "idle" | "streaming" | "thinking" | "error", label: string) {
  statusPill.className = `status-pill status-pill--${state}`;
  statusText.textContent = label;
}

function scrollToBottom() {
  transcript.scrollTop = transcript.scrollHeight;
}

function appendTurn(turn: { element: HTMLElement }) {
  transcript.append(turn.element);
  scrollToBottom();
}

function insertCommand(command: string) {
  composerInput.value = command;
  composerInput.focus();
  composerInput.setSelectionRange(command.length, command.length);
}

async function fetchCommands(): Promise<SlashCommand[]> {
  const response = await fetch("/commands", { cache: "force-cache" });
  if (!response.ok) {
    return [];
  }
  return (await response.json()) as SlashCommand[];
}

function showPairModal(onSuccess: () => void) {
  retryAfterPair = onSuccess;
  pairingModal.hidden = false;
  pairingCode.value = "";
  pairingError.textContent = "";
  pairingCode.focus();
}

function hidePairModal() {
  pairingModal.hidden = true;
  pairingError.textContent = "";
}

function updateTurn(turn: AssistantTurnView, event: ServerEvent) {
  switch (event.type) {
    case "delta":
      turn.appendText(event.content);
      setStatus("streaming", "streaming reply");
      break;
    case "thinking_delta":
      turn.appendThinking(event.content);
      setStatus("thinking", "thinking");
      break;
    case "tool_start":
      turn.startTool(event.id, event.name, event.arguments);
      setStatus("streaming", `running ${event.name}`);
      break;
    case "tool_output_delta":
      turn.appendToolOutput(event.id, event.chunk);
      break;
    case "tool_result":
      turn.finishTool(event.id, event.name, event.result, event.is_error);
      break;
    case "tool_call_delta":
      break;
    case "error":
      turn.appendText(`\n\n[stream error] ${event.message}`);
      setStatus("error", "stream error");
      break;
    case "done":
      break;
    default:
      break;
  }
  scrollToBottom();
}

async function sendMessage(message: string) {
  const trimmed = message.trim();
  if (!trimmed) {
    return;
  }

  appendTurn(new UserTurnView(trimmed));
  const assistantTurn = new AssistantTurnView();
  appendTurn(assistantTurn);

  abortController = new AbortController();
  setStatus("thinking", "contacting daemon");
  sendButton.disabled = true;
  stopButton.hidden = false;

  try {
    const headers: Record<string, string> = { "Content-Type": "application/json" };
    const token = getToken();
    if (token) {
      headers.Authorization = `Bearer ${token}`;
    }

    const response = await fetch("/chat/stream", {
      method: "POST",
      headers,
      body: JSON.stringify({ message: trimmed, session_id: getSessionId() }),
      signal: abortController.signal,
    });

    if (response.status === 401 || response.status === 403) {
      showPairModal(() => {
        void sendMessage(trimmed);
      });
      assistantTurn.appendText("Pairing required before chat can continue.");
      setStatus("idle", "pairing required");
      return;
    }

    if (!response.ok) {
      const errorText = await response.text();
      assistantTurn.appendText(`Error: ${response.status} ${errorText}`);
      setStatus("error", "request failed");
      return;
    }

    for await (const event of readSSE(response)) {
      updateTurn(assistantTurn, event);
      if (event.type === "done") {
        break;
      }
    }

    await assistantTurn.finalize();
    setStatus("idle", "ready");
  } catch (error) {
    if (error instanceof DOMException && error.name === "AbortError") {
      setStatus("idle", "stopped");
      return;
    }
    assistantTurn.appendText(`Error: ${error instanceof Error ? error.message : String(error)}`);
    setStatus("error", "network error");
  } finally {
    abortController = null;
    sendButton.disabled = false;
    stopButton.hidden = true;
    scrollToBottom();
  }
}

installSlashPopover({
  input: composerInput,
  popover: slashPopover,
  fetchCommands,
});

installVersionBanner({
  banner: versionBanner,
  dismissButton: versionDismiss,
  reloadButton: versionReload,
});

composerForm.addEventListener("submit", (event) => {
  event.preventDefault();
  const message = composerInput.value;
  if (!message.trim()) {
    return;
  }
  composerInput.value = "";
  void sendMessage(message);
});

composerInput.addEventListener("keydown", (event) => {
  if (event.key === "Enter" && !event.shiftKey) {
    event.preventDefault();
    composerForm.requestSubmit();
  }
});

stopButton.addEventListener("click", () => {
  abortController?.abort();
});

pairingSubmit.addEventListener("click", async () => {
  const code = pairingCode.value.trim();
  if (!/^\d{6}$/.test(code)) {
    pairingError.textContent = "Enter a 6-digit code.";
    return;
  }
  try {
    const response = await fetch("/pair", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ code }),
    });
    const payload = (await response.json()) as { error?: string; token?: string };
    if (!response.ok || !payload.token) {
      pairingError.textContent = payload.error ?? "Pairing failed.";
      return;
    }
    setToken(payload.token);
    hidePairModal();
    retryAfterPair?.();
    retryAfterPair = null;
    setStatus("idle", "paired");
  } catch (error) {
    pairingError.textContent = error instanceof Error ? error.message : String(error);
  }
});

pairingCode.addEventListener("keydown", (event) => {
  if (event.key === "Enter") {
    event.preventDefault();
    pairingSubmit.click();
  }
});

newSessionButton.addEventListener("click", () => {
  resetSession();
  setStatus("idle", "new session");
});

themeToggle?.addEventListener("click", () => {
  const current = document.documentElement.getAttribute("data-theme");
  const next = current === "light" ? "dark" : "light";
  document.documentElement.setAttribute("data-theme", next);
  localStorage.setItem("clawq_theme", next);
});

for (const chip of document.querySelectorAll<HTMLButtonElement>(".command-chip")) {
  chip.addEventListener("click", () => {
    insertCommand(chip.dataset.command ?? "");
  });
}

setStatus("idle", "ready");
composerInput.focus();
