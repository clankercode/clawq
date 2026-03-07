import { ansiToHtml, escapePlainText } from "./ansi";

export class ToolPanelView {
  readonly id: string;
  private readonly root: HTMLDivElement;
  private readonly status: HTMLSpanElement;
  private readonly output: HTMLDivElement;
  private readonly result: HTMLDivElement;
  private rawOutput = "";

  constructor(id: string, name: string, argumentsJson: string) {
    this.id = id;
    this.root = document.createElement("div");
    this.root.className = "tool-card tool-panel";

    const header = document.createElement("button");
    header.type = "button";
    header.className = "tool-card__header";
    header.innerHTML = `
      <span>
        <span class="tool-card__name">${escapePlainText(name)}</span>
        <span class="tool-card__args">${escapePlainText(argumentsJson || "{}")}</span>
      </span>
    `;

    this.status = document.createElement("span");
    this.status.className = "tool-card__status tool-card__status--running";
    this.status.textContent = "running";
    header.append(this.status);

    const body = document.createElement("div");
    body.className = "tool-card__body";

    this.output = document.createElement("div");
    this.output.className = "tool-card__output";

    this.result = document.createElement("div");
    this.result.className = "tool-card__result";

    header.addEventListener("click", () => {
      this.root.classList.toggle("tool-card--open");
    });

    body.append(this.output, this.result);
    this.root.append(header, body);
  }

  get element(): HTMLDivElement {
    return this.root;
  }

  appendOutput(chunk: string) {
    this.rawOutput += chunk;
    this.root.classList.add("tool-card--open");
    this.output.innerHTML = ansiToHtml(this.rawOutput);
  }

  finish(result: string, isError: boolean) {
    this.status.className = `tool-card__status ${isError ? "tool-card__status--error" : "tool-card__status--done"}`;
    this.status.textContent = isError ? "error" : "done";
    this.result.textContent = result;
    this.root.classList.add("tool-card--open");
  }
}
