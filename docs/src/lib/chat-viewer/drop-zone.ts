export interface DropZoneCallbacks {
  onFile: (data: unknown, fileName: string) => void;
  onError: (message: string) => void;
}

export function initDropZone(dropZoneEl: HTMLElement, callbacks: DropZoneCallbacks) {
  const fileInput = document.createElement("input");
  fileInput.type = "file";
  fileInput.accept = ".json,.jsonl";
  fileInput.style.display = "none";
  dropZoneEl.appendChild(fileInput);

  // Click to browse
  dropZoneEl.addEventListener("click", (e) => {
    if ((e.target as HTMLElement).tagName !== "INPUT") {
      fileInput.click();
    }
  });

  fileInput.addEventListener("change", () => {
    if (fileInput.files && fileInput.files.length > 0) {
      handleFile(fileInput.files[0], callbacks);
      fileInput.value = "";
    }
  });

  // Drag and drop on the drop zone
  dropZoneEl.addEventListener("dragover", (e) => {
    e.preventDefault();
    dropZoneEl.classList.add("cv-drop-zone--active");
  });

  dropZoneEl.addEventListener("dragleave", () => {
    dropZoneEl.classList.remove("cv-drop-zone--active");
  });

  dropZoneEl.addEventListener("drop", (e) => {
    e.preventDefault();
    dropZoneEl.classList.remove("cv-drop-zone--active");
    if (e.dataTransfer?.files && e.dataTransfer.files.length > 0) {
      handleFile(e.dataTransfer.files[0], callbacks);
    }
  });

  // Full-page drag overlay
  let dragCounter = 0;
  const overlay = document.createElement("div");
  overlay.className = "cv-drag-overlay";
  overlay.innerHTML = `<div class="cv-drag-overlay__inner">Drop .json or .jsonl file here</div>`;
  document.body.appendChild(overlay);

  document.addEventListener("dragenter", (e) => {
    e.preventDefault();
    dragCounter++;
    if (dragCounter === 1) {
      overlay.classList.add("cv-drag-overlay--visible");
    }
  });

  document.addEventListener("dragleave", (e) => {
    e.preventDefault();
    dragCounter--;
    if (dragCounter <= 0) {
      dragCounter = 0;
      overlay.classList.remove("cv-drag-overlay--visible");
    }
  });

  document.addEventListener("drop", (e) => {
    e.preventDefault();
    dragCounter = 0;
    overlay.classList.remove("cv-drag-overlay--visible");
  });

  overlay.addEventListener("dragover", (e) => e.preventDefault());
  overlay.addEventListener("drop", (e) => {
    e.preventDefault();
    dragCounter = 0;
    overlay.classList.remove("cv-drag-overlay--visible");
    if (e.dataTransfer?.files && e.dataTransfer.files.length > 0) {
      handleFile(e.dataTransfer.files[0], callbacks);
    }
  });
}

function parseJsonOrJsonl(text: string): unknown {
  // Try standard JSON first
  try {
    return JSON.parse(text);
  } catch {
    // Try JSONL: parse each non-empty line as a JSON object, collect into array
    const lines = text.split("\n").filter((line) => line.trim().length > 0);
    if (lines.length === 0) throw new Error("Empty file.");
    const items = lines.map((line, i) => {
      try {
        return JSON.parse(line);
      } catch {
        throw new Error(`Invalid JSON on line ${i + 1}.`);
      }
    });
    return items;
  }
}

function handleFile(file: File, callbacks: DropZoneCallbacks) {
  if (!file.name.endsWith(".json") && !file.name.endsWith(".jsonl")) {
    callbacks.onError("Please drop a .json or .jsonl file.");
    return;
  }

  const reader = new FileReader();
  reader.onload = () => {
    try {
      const data = parseJsonOrJsonl(reader.result as string);
      callbacks.onFile(data, file.name);
    } catch (e) {
      callbacks.onError(e instanceof Error ? e.message : "Could not parse the file.");
    }
  };
  reader.onerror = () => {
    callbacks.onError("Failed to read file.");
  };
  reader.readAsText(file);
}
