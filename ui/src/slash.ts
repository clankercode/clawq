export type SlashCommand = {
  name: string;
  description: string;
};

type Options = {
  input: HTMLTextAreaElement;
  popover: HTMLElement;
  fetchCommands: () => Promise<SlashCommand[]>;
  fetchConfigKeys?: (prefix: string) => Promise<string[]>;
};

function currentQuery(value: string, caret: number): { start: number; query: string } | null {
  const prefix = value.slice(0, caret);
  const match = prefix.match(/(?:^|\s)\/([a-z0-9_-]*)$/i);
  if (!match) {
    return null;
  }
  return { start: caret - match[1].length - 1, query: match[1].toLowerCase() };
}

function currentConfigArgQuery(value: string, caret: number): { start: number; prefix: string } | null {
  const before = value.slice(0, caret);
  const match = before.match(/^\/config\s+(?:get|set)\s+(\S*)$/i);
  if (!match) {
    return null;
  }
  return { start: caret - match[1].length, prefix: match[1].toLowerCase() };
}

export function installSlashPopover(options: Options) {
  const { input, popover, fetchCommands, fetchConfigKeys } = options;
  let commands: SlashCommand[] | null = null;
  let selectedIndex = 0;
  let activeQuery: { start: number; query: string } | null = null;
  let activeConfigQuery: { start: number; prefix: string } | null = null;

  async function ensureCommands() {
    if (!commands) {
      commands = await fetchCommands();
    }
    return commands;
  }

  function close() {
    popover.hidden = true;
    popover.innerHTML = "";
    activeQuery = null;
    activeConfigQuery = null;
    selectedIndex = 0;
  }

  function applyConfigKey(key: string) {
    if (!activeConfigQuery) {
      return;
    }
    const caret = input.selectionStart ?? input.value.length;
    input.value = `${input.value.slice(0, activeConfigQuery.start)}${key} ${input.value.slice(caret)}`;
    const nextCaret = activeConfigQuery.start + key.length + 1;
    input.setSelectionRange(nextCaret, nextCaret);
    input.focus();
    close();
  }

  function applyCommand(command: SlashCommand) {
    if (!activeQuery) {
      return;
    }
    const caret = input.selectionStart ?? input.value.length;
    input.value = `${input.value.slice(0, activeQuery.start)}/${command.name} ${input.value.slice(caret)}`;
    const nextCaret = activeQuery.start + command.name.length + 2;
    input.setSelectionRange(nextCaret, nextCaret);
    input.focus();
    close();
  }

  async function render() {
    const caret = input.selectionStart ?? input.value.length;

    if (fetchConfigKeys) {
      activeConfigQuery = currentConfigArgQuery(input.value, caret);
      if (activeConfigQuery) {
        activeQuery = null;
        const keys = await fetchConfigKeys(activeConfigQuery.prefix);
        if (keys.length === 0) {
          close();
          return;
        }
        const shown = keys.slice(0, 8);
        selectedIndex = Math.min(selectedIndex, shown.length - 1);
        popover.innerHTML = "";
        shown.forEach((key, index) => {
          const item = document.createElement("button");
          item.type = "button";
          item.className = `slash-popover__item ${index === selectedIndex ? "slash-popover__item--selected" : ""}`;
          item.innerHTML = `<strong>${key}</strong>`;
          item.addEventListener("mousedown", (event) => {
            event.preventDefault();
            applyConfigKey(key);
          });
          popover.append(item);
        });
        popover.hidden = false;
        return;
      }
    }

    activeQuery = currentQuery(input.value, caret);
    if (!activeQuery) {
      close();
      return;
    }
    const available = (await ensureCommands()).filter((command) => command.name.toLowerCase().startsWith(activeQuery!.query)).slice(0, 8);
    if (available.length === 0) {
      close();
      return;
    }
    selectedIndex = Math.min(selectedIndex, available.length - 1);
    popover.innerHTML = "";
    available.forEach((command, index) => {
      const item = document.createElement("button");
      item.type = "button";
      item.className = `slash-popover__item ${index === selectedIndex ? "slash-popover__item--selected" : ""}`;
      item.innerHTML = `<strong>/${command.name}</strong><span>${command.description}</span>`;
      item.addEventListener("mousedown", (event) => {
        event.preventDefault();
        applyCommand(command);
      });
      popover.append(item);
    });
    popover.hidden = false;
  }

  input.addEventListener("input", () => {
    void render();
  });

  input.addEventListener("click", () => {
    void render();
  });

  input.addEventListener("keydown", async (event) => {
    if (popover.hidden) {
      return;
    }
    const items = Array.from(popover.querySelectorAll<HTMLButtonElement>(".slash-popover__item"));
    if (items.length === 0) {
      return;
    }
    if (event.key === "ArrowDown") {
      event.preventDefault();
      selectedIndex = (selectedIndex + 1) % items.length;
      await render();
      return;
    }
    if (event.key === "ArrowUp") {
      event.preventDefault();
      selectedIndex = (selectedIndex - 1 + items.length) % items.length;
      await render();
      return;
    }
    if (event.key === "Escape") {
      event.preventDefault();
      close();
      return;
    }
    if (event.key === "Tab" || event.key === "Enter") {
      const item = items[selectedIndex];
      if (!item) {
        return;
      }
      event.preventDefault();
      item.dispatchEvent(new MouseEvent("mousedown", { bubbles: true }));
    }
  });

  document.addEventListener("click", (event) => {
    if (event.target instanceof Node && (popover.contains(event.target) || input.contains(event.target))) {
      return;
    }
    close();
  });

  return { close };
}
