export class ThinkingBlockView {
  private readonly root: HTMLDivElement;
  private readonly summary: HTMLButtonElement;
  private readonly body: HTMLDivElement;
  private content = "";

  constructor() {
    this.root = document.createElement("div");
    this.root.className = "thinking-block";
    this.root.hidden = true;

    this.summary = document.createElement("button");
    this.summary.type = "button";
    this.summary.className = "thinking-block__summary";
    this.summary.innerHTML = "<span>Thinking trace</span><span class=\"thinking-block__chevron\">+</span>";

    this.body = document.createElement("div");
    this.body.className = "thinking-block__body";

    this.summary.addEventListener("click", () => {
      this.root.classList.toggle("thinking-block--open");
    });

    this.root.append(this.summary, this.body);
  }

  get element(): HTMLDivElement {
    return this.root;
  }

  append(chunk: string) {
    this.content += chunk;
    this.root.hidden = false;
    this.body.textContent = this.content;
  }

  finalize() {
    if (this.content.trim()) {
      this.root.classList.add("thinking-block--ready");
    }
  }
}
