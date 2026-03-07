import { escapePlainText } from "./ansi";
import { ThinkingBlockView } from "./thinking";
import { ToolPanelView } from "./tool-panel";

let mermaidLoader: Promise<void> | null = null;

function escapeUserText(text: string): string {
  return escapePlainText(text).replaceAll("\n", "<br>");
}

function configureMarkdown() {
  if (!window.marked || !window.hljs) {
    return;
  }
  window.marked.setOptions({
    breaks: true,
    gfm: true,
    highlight(code: string, language: string) {
      if (language && window.hljs.getLanguage(language)) {
        return window.hljs.highlight(code, { language }).value;
      }
      return window.hljs.highlightAuto(code).value;
    },
  });
}

async function ensureMermaid() {
  if (window.mermaid) {
    return;
  }
  if (!mermaidLoader) {
    mermaidLoader = new Promise<void>((resolve, reject) => {
      const existing = document.querySelector<HTMLScriptElement>('script[data-mermaid-loader="true"]');
      if (existing) {
        existing.addEventListener("load", () => resolve(), { once: true });
        existing.addEventListener("error", () => reject(new Error("Failed to load mermaid")), { once: true });
        return;
      }
      const script = document.createElement("script");
      script.src = "https://cdn.jsdelivr.net/npm/mermaid@11.12.3/dist/mermaid.min.js";
      script.integrity = "sha384-jFhLSLFn4m565eRAS0CDMWubMqOtfZWWbE8kqgGdU+VHbJ3B2G/4X8u+0BM8MtdU";
      script.crossOrigin = "anonymous";
      script.defer = true;
      script.dataset.mermaidLoader = "true";
      script.addEventListener("load", () => resolve(), { once: true });
      script.addEventListener("error", () => reject(new Error("Failed to load mermaid")), { once: true });
      document.head.append(script);
    }).then(() => {
      window.mermaid?.initialize({ startOnLoad: false, theme: "neutral" });
    });
  }
  await mermaidLoader;
}

async function renderMermaidBlocks(container: HTMLElement) {
  const blocks = Array.from(container.querySelectorAll("pre code.language-mermaid"));
  if (blocks.length === 0) {
    return;
  }
  try {
    await ensureMermaid();
    await window.mermaid?.run({ nodes: blocks.map((block) => block.parentElement).filter(Boolean) });
  } catch {
    for (const block of blocks) {
      const note = document.createElement("p");
      note.className = "render-note";
      note.textContent = "Mermaid diagram failed to render.";
      block.parentElement?.after(note);
    }
  }
}

export class UserTurnView {
  readonly element: HTMLElement;

  constructor(text: string) {
    const root = document.createElement("article");
    root.className = "turn turn--user";
    root.innerHTML = `
      <div class="turn__eyebrow">you</div>
      <div class="turn__body turn__body--user">${escapeUserText(text)}</div>
    `;
    this.element = root;
  }
}

export class AssistantTurnView {
  readonly element: HTMLElement;
  private readonly textBody: HTMLDivElement;
  private readonly toolStack: HTMLDivElement;
  private readonly thinking: ThinkingBlockView;
  private readonly toolPanels = new Map<string, ToolPanelView>();
  private rawText = "";

  constructor() {
    const root = document.createElement("article");
    root.className = "turn turn--assistant";

    const eyebrow = document.createElement("div");
    eyebrow.className = "turn__eyebrow";
    eyebrow.textContent = "assistant";

    this.thinking = new ThinkingBlockView();

    this.toolStack = document.createElement("div");
    this.toolStack.className = "tool-stack";

    this.textBody = document.createElement("div");
    this.textBody.className = "turn__body turn__body--assistant";
    this.textBody.innerHTML = "<p class=\"turn__placeholder\">Waiting for stream...</p>";

    root.append(eyebrow, this.thinking.element, this.toolStack, this.textBody);
    this.element = root;
  }

  appendText(chunk: string) {
    this.rawText += chunk;
    this.textBody.textContent = this.rawText;
  }

  appendThinking(chunk: string) {
    this.thinking.append(chunk);
  }

  startTool(id: string, name: string, argumentsJson: string) {
    if (this.toolPanels.has(id)) {
      return;
    }
    const panel = new ToolPanelView(id, name, argumentsJson);
    this.toolPanels.set(id, panel);
    this.toolStack.append(panel.element);
  }

  appendToolOutput(id: string, chunk: string) {
    const panel = this.toolPanels.get(id);
    if (panel) {
      panel.appendOutput(chunk);
    }
  }

  finishTool(id: string, name: string, result: string, isError: boolean) {
    if (!this.toolPanels.has(id)) {
      this.startTool(id, name, "{}");
    }
    this.toolPanels.get(id)?.finish(result, isError);
  }

  async finalize() {
    this.thinking.finalize();
    configureMarkdown();
    if (!this.rawText.trim()) {
      this.textBody.innerHTML = "<p class=\"turn__placeholder\">No assistant text for this turn.</p>";
      return;
    }
    if (!window.marked || !window.DOMPurify) {
      this.textBody.innerHTML = escapeUserText(this.rawText);
      return;
    }
    const rendered = window.marked.parse(this.rawText);
    this.textBody.innerHTML = window.DOMPurify.sanitize(rendered);
    await renderMermaidBlocks(this.textBody);
  }
}
