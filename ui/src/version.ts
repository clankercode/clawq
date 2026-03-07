type VersionOptions = {
  banner: HTMLElement;
  dismissButton: HTMLButtonElement;
  reloadButton: HTMLButtonElement;
};

const DISMISS_KEY = "clawq_ui_version_banner_dismissed";

async function fetchVersion(): Promise<string | null> {
  try {
    const response = await fetch("/ui-version", { cache: "no-store" });
    if (!response.ok) {
      return null;
    }
    const payload = (await response.json()) as { version?: string };
    return payload.version ?? null;
  } catch {
    return null;
  }
}

export function installVersionBanner(options: VersionOptions) {
  const localVersion = document.querySelector<HTMLMetaElement>('meta[name="ui-version"]')?.content ?? "";
  const { banner, dismissButton, reloadButton } = options;

  function show() {
    if (sessionStorage.getItem(DISMISS_KEY) === localVersion) {
      return;
    }
    banner.hidden = false;
  }

  async function check() {
    const remoteVersion = await fetchVersion();
    if (remoteVersion && localVersion && remoteVersion !== localVersion) {
      show();
    }
  }

  dismissButton.addEventListener("click", () => {
    sessionStorage.setItem(DISMISS_KEY, localVersion);
    banner.hidden = true;
  });

  reloadButton.addEventListener("click", () => {
    window.location.reload();
  });

  document.addEventListener("visibilitychange", () => {
    if (!document.hidden) {
      void check();
    }
  });

  window.setInterval(() => {
    void check();
  }, 5 * 60 * 1000);
}
