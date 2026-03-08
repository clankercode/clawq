(* Auto-generated chat UI assets - embedded UI bundle *)

let ui_version =
  {ui|sha256:d9966bbeeab9a8bb980b3382159fa91764989f8e56942c487bc8d216f54b8b5f|ui}

let index_html =
  {ui|<!DOCTYPE html>
<html lang="en" data-theme="dark">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <meta name="ui-version" content="dev">
  <title>clawq web chat</title>
  <script>
    (function(){var t=localStorage.getItem('clawq_theme');if(!t){t=window.matchMedia&&window.matchMedia('(prefers-color-scheme:light)').matches?'light':'dark'}document.documentElement.setAttribute('data-theme',t)})();
  </script>
  <link rel="preconnect" href="https://fonts.googleapis.com">
  <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
  <link href="https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;500&family=Playfair+Display:wght@700;900&family=Source+Serif+4:wght@400;500;600&display=swap" rel="stylesheet">
  <link rel="stylesheet" href="/chat.css">
  <script defer src="https://cdn.jsdelivr.net/npm/dompurify@3.2.6/dist/purify.min.js" integrity="sha384-JEyTNhjM6R1ElGoJns4U2Ln4ofPcqzSsynQkmEc/KGy6336qAZl70tDLufbkla+3" crossorigin="anonymous"></script>
  <script defer src="https://cdn.jsdelivr.net/npm/marked@15.0.12/marked.min.js" integrity="sha384-948ahk4ZmxYVYOc+rxN1H2gM1EJ2Duhp7uHtZ4WSLkV4Vtx5MUqnV+l7u9B+jFv+" crossorigin="anonymous"></script>
  <script defer src="https://cdn.jsdelivr.net/npm/@highlightjs/cdn-assets@11.11.1/highlight.min.js" integrity="sha384-RH2xi4eIQ/gjtbs9fUXM68sLSi99C7ZWBRX1vDrVv6GQXRibxXLbwO2NGZB74MbU" crossorigin="anonymous"></script>
</head>
<body>
  <div class="app-shell">
    <header class="masthead">
      <div>
        <p class="eyebrow">stream console</p>
        <h1>clawq</h1>
        <p class="subtitle">A formally verified assistant runtime with live reasoning, tool traces, and slash control.</p>
      </div>
      <div class="masthead__status">
        <div class="status-pill status-pill--idle" id="status-pill">
          <span class="status-pill__dot"></span>
          <span id="status-text">ready</span>
        </div>
        <button class="theme-toggle" id="theme-toggle" type="button" title="Toggle light/dark theme">
          <svg class="theme-toggle__icon" viewBox="0 0 24 24">
            <circle cx="12" cy="12" r="5"/>
            <line x1="12" y1="1" x2="12" y2="3"/>
            <line x1="12" y1="21" x2="12" y2="23"/>
            <line x1="4.22" y1="4.22" x2="5.64" y2="5.64"/>
            <line x1="18.36" y1="18.36" x2="19.78" y2="19.78"/>
            <line x1="1" y1="12" x2="3" y2="12"/>
            <line x1="21" y1="12" x2="23" y2="12"/>
            <line x1="4.22" y1="19.78" x2="5.64" y2="18.36"/>
            <line x1="18.36" y1="5.64" x2="19.78" y2="4.22"/>
          </svg>
        </button>
        <button class="ghost-button" id="new-session-btn" type="button">New session</button>
      </div>
    </header>

    <div class="version-banner" id="version-banner" hidden>
      <span>clawq updated in the background.</span>
      <div class="version-banner__actions">
        <button class="ghost-button" id="version-dismiss" type="button">Dismiss</button>
        <button id="version-reload" type="button">Reload</button>
      </div>
    </div>

    <main class="workspace">
      <aside class="sidebar">
        <section class="sidebar-card">
          <p class="sidebar-card__label">Field notes</p>
          <ul class="sidebar-list">
            <li>Streaming text stays plain until the turn completes, then upgrades to markdown.</li>
            <li>Thinking deltas collapse into a dedicated panel so reasoning does not muddy the final answer.</li>
            <li>Tool cards keep raw output, including ANSI color, while the final result remains searchable.</li>
          </ul>
        </section>
        <section class="sidebar-card">
          <p class="sidebar-card__label">Quick starts</p>
          <div class="command-grid">
            <button class="command-chip" data-command="/help ">/help</button>
            <button class="command-chip" data-command="/new ">/new</button>
            <button class="command-chip" data-command="/status ">/status</button>
          </div>
          <p class="sidebar-card__hint">Type <code>/</code> for command suggestions. Prefix a message with <code>!</code> to interrupt the current turn.</p>
        </section>
      </aside>

      <section class="transcript-panel">
        <div class="transcript" id="transcript" aria-live="polite"></div>

        <div class="composer-shell">
          <div class="slash-popover" id="slash-popover" hidden></div>
          <form id="composer-form" class="composer">
            <label class="composer__label" for="composer-input">Message</label>
            <textarea id="composer-input" rows="4" placeholder="Ask clawq to reason, inspect files, or run a tool..." autocomplete="off"></textarea>
            <div class="composer__actions">
              <p class="composer__hint">Shift+Enter for a newline. Prefix with <code>!</code> to interrupt the current turn.</p>
              <div class="composer__buttons">
                <button class="ghost-button" id="stop-btn" type="button" hidden>Stop</button>
                <button id="send-btn" type="submit">Send</button>
              </div>
            </div>
          </form>
        </div>
      </section>
    </main>
  </div>

  <div class="pairing-modal" id="pair-modal" hidden>
    <div class="pairing-dialog" role="dialog" aria-modal="true" aria-labelledby="pairing-title">
      <p class="eyebrow">secure entry</p>
      <h2 id="pairing-title">Pair this browser</h2>
      <p class="pairing-copy">Enter the 6-digit code shown by <code>clawq otp-show</code>. Assets stay public; chat requests stay gated.</p>
      <input id="pairing-code" type="text" inputmode="numeric" maxlength="6" placeholder="000000" pattern="[0-9]{6}">
      <p class="pairing-error" id="pairing-error"></p>
      <button id="pairing-submit" type="button">Pair and continue</button>
    </div>
  </div>

  <div class="gear-backdrop" aria-hidden="true">
    <svg class="gear gear--large" viewBox="0 0 280 280"><path d="M140 20 l8-16 h-16 l8 16 M140 260 l8 16 h-16 l8-16 M20 140 l-16-8 v16 l16-8 M260 140 l16 8 v-16 l-16 8 M52 52 l-14-8 -6 14 14-6 M228 52 l14-8 6 14-14-6 M52 228 l-14 8-6-14 14 6 M228 228 l14 8 6-14-14 6" stroke-linecap="round"/><circle cx="140" cy="140" r="100"/><circle cx="140" cy="140" r="70"/><circle cx="140" cy="140" r="35"/></svg>
    <svg class="gear gear--small" viewBox="0 0 140 140"><path d="M70 10 l5-8 h-10 l5 8 M70 130 l5 8 h-10 l5-8 M10 70 l-8-5 v10 l8-5 M130 70 l8 5 v-10 l-8 5 M28 28 l-7-5-3 7 7-2 M112 28 l7-5 3 7-7-2 M28 112 l-7 5-3-7 7 2 M112 112 l7 5 3-7-7 2" stroke-linecap="round"/><circle cx="70" cy="70" r="48"/><circle cx="70" cy="70" r="32"/><circle cx="70" cy="70" r="16"/></svg>
  </div>

  <script defer src="/chat.js"></script>
</body>
</html>
|ui}

let chat_css =
  {ui|/* ===================================================================
   Clawq Web Chat — "The Clockwork Study"
   Dark-first Victorian academic / steampunk aesthetic
   =================================================================== */

/* --- Design tokens (dark default) --------------------------------- */
:root {
  /* Backgrounds */
  --bg-primary: #0D0B0F;
  --bg-secondary: #13111A;
  --bg-tertiary: #1A1822;
  --bg-hover: #22202E;

  /* Brass accent */
  --brass: #C9A84C;
  --brass-light: #E2C97E;
  --brass-dark: #8B7332;
  --brass-glow: rgba(201, 168, 76, 0.10);

  /* Coq teal */
  --teal: #2E8B7A;
  --teal-light: #5BBFAD;

  /* Text */
  --text-primary: #E8E2D6;
  --text-secondary: #9C978A;
  --text-tertiary: #6B6660;

  /* Semantic */
  --qed-gold: #B8860B;
  --error-red: #C44B3F;
  --info-blue: #4A6B8A;

  /* Surfaces */
  --line: rgba(201, 168, 76, 0.12);
  --console: #0F0D14;
  --console-line: rgba(201, 168, 76, 0.10);

  /* Radii */
  --radius-lg: 16px;
  --radius-md: 12px;
  --radius-sm: 8px;

  /* Shadows */
  --shadow: 0 8px 32px rgba(0, 0, 0, 0.45);
  --shadow-sm: 0 2px 8px rgba(0, 0, 0, 0.3);

  color-scheme: dark;
}

/* --- Light mode overrides ----------------------------------------- */
[data-theme="light"] {
  --bg-primary: #FAF6F0;
  --bg-secondary: #F3EDE4;
  --bg-tertiary: #EDEADF;
  --bg-hover: #E5DDD2;

  --brass: #7A6321;
  --brass-light: #9B7E2E;
  --brass-dark: #5C4A18;
  --brass-glow: rgba(122, 99, 33, 0.08);

  --teal: #2E8B7A;
  --teal-light: #3A9E8B;

  --text-primary: #2A2520;
  --text-secondary: #6B5E52;
  --text-tertiary: #9C8E80;

  --line: rgba(122, 99, 33, 0.14);
  --console: #1A1822;
  --console-line: rgba(201, 168, 76, 0.10);

  --shadow: 0 8px 32px rgba(90, 70, 30, 0.10);
  --shadow-sm: 0 2px 8px rgba(90, 70, 30, 0.06);

  color-scheme: light;
}

/* --- Reset & base ------------------------------------------------- */
* {
  box-sizing: border-box;
}

html,
body {
  min-height: 100%;
}

body {
  margin: 0;
  color: var(--text-primary);
  font-family: "Source Serif 4", "Charter", Georgia, serif;
  background: var(--bg-primary);
  transition: background-color 200ms ease, color 200ms ease;
}

/* Blueprint grid */
body::before {
  content: "";
  position: fixed;
  inset: 0;
  pointer-events: none;
  z-index: 0;
  opacity: 0.5;
  background-image:
    linear-gradient(rgba(201, 168, 76, 0.05) 1px, transparent 1px),
    linear-gradient(90deg, rgba(201, 168, 76, 0.05) 1px, transparent 1px);
  background-size: 40px 40px;
  mask-image: linear-gradient(to bottom, rgba(0, 0, 0, 0.6), transparent 88%);
}

[data-theme="light"] body::before {
  opacity: 0.7;
}

button,
textarea,
input {
  font: inherit;
}

/* --- Global button base ------------------------------------------- */
button {
  border: none;
  border-radius: 6px;
  background: linear-gradient(135deg, var(--brass) 0%, var(--brass-light) 100%);
  color: var(--bg-primary);
  cursor: pointer;
  padding: 0.75rem 1.2rem;
  font-weight: 600;
  transition: transform 120ms ease, box-shadow 120ms ease, opacity 120ms ease,
              background-color 200ms ease, border-color 200ms ease;
  box-shadow: var(--shadow-sm);
}

button:hover {
  transform: translateY(-1px);
  box-shadow: 0 4px 16px rgba(201, 168, 76, 0.25);
}

button:disabled {
  opacity: 0.45;
  cursor: not-allowed;
  transform: none;
}

.ghost-button {
  background: var(--bg-secondary);
  color: var(--text-primary);
  border: 1px solid var(--line);
  box-shadow: none;
}

.ghost-button:hover {
  border-color: var(--brass-dark);
  background: var(--bg-hover);
  box-shadow: none;
}

/* --- Monospace elements ------------------------------------------- */
code,
pre,
.tool-card__name,
.tool-card__args,
.slash-popover__item strong {
  font-family: "JetBrains Mono", Consolas, "SFMono-Regular", monospace;
}

/* --- App shell ---------------------------------------------------- */
.app-shell {
  position: relative;
  z-index: 1;
  width: min(1320px, calc(100vw - 2rem));
  margin: 0 auto;
  padding: 1.5rem 0 2rem;
}

/* --- Masthead ----------------------------------------------------- */
.masthead {
  display: flex;
  justify-content: space-between;
  gap: 1rem;
  align-items: flex-start;
  margin-bottom: 1rem;
}

.eyebrow,
.turn__eyebrow,
.sidebar-card__label,
.composer__label {
  margin: 0 0 0.4rem;
  text-transform: uppercase;
  letter-spacing: 0.18em;
  font-size: 0.72rem;
  color: var(--brass-dark);
  font-family: "Source Serif 4", Georgia, serif;
  font-weight: 500;
}

.masthead h1,
.pairing-dialog h2 {
  margin: 0;
  font-family: "Playfair Display", Georgia, serif;
  font-size: clamp(2.4rem, 6vw, 4.2rem);
  font-weight: 900;
  line-height: 0.94;
  color: var(--text-primary);
}

.subtitle {
  margin: 0.7rem 0 0;
  max-width: 42rem;
  color: var(--text-secondary);
  font-size: 1rem;
  line-height: 1.6;
}

.masthead__status {
  display: flex;
  gap: 0.7rem;
  align-items: center;
}

/* --- Theme toggle ------------------------------------------------- */
.theme-toggle {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 2.4rem;
  height: 2.4rem;
  padding: 0;
  border-radius: 50%;
  background: var(--bg-secondary);
  border: 1px solid var(--line);
  color: var(--brass);
  cursor: pointer;
  box-shadow: none;
  transition: background-color 200ms ease, border-color 200ms ease, color 200ms ease;
}

.theme-toggle:hover {
  background: var(--bg-hover);
  border-color: var(--brass-dark);
  transform: none;
  box-shadow: none;
}

.theme-toggle__icon {
  width: 1.1rem;
  height: 1.1rem;
  fill: none;
  stroke: currentColor;
  stroke-width: 2;
  stroke-linecap: round;
  stroke-linejoin: round;
}

/* --- Status pill -------------------------------------------------- */
.status-pill {
  display: inline-flex;
  align-items: center;
  gap: 0.55rem;
  padding: 0.55rem 0.9rem;
  border-radius: 999px;
  background: var(--bg-secondary);
  border: 1px solid var(--line);
  color: var(--text-secondary);
  font-size: 0.85rem;
  transition: background-color 200ms ease, border-color 200ms ease;
}

.status-pill__dot {
  width: 0.55rem;
  height: 0.55rem;
  border-radius: 999px;
  background: var(--text-tertiary);
  transition: background-color 200ms ease;
}

.status-pill--idle .status-pill__dot {
  background: var(--teal);
}

.status-pill--streaming .status-pill__dot {
  background: var(--brass);
}

.status-pill--thinking .status-pill__dot {
  background: var(--brass-light);
  animation: pulse 1.1s infinite ease-in-out;
}

.status-pill--error .status-pill__dot {
  background: var(--error-red);
}

/* --- Version banner ----------------------------------------------- */
.version-banner {
  display: flex;
  justify-content: space-between;
  align-items: center;
  gap: 1rem;
  padding: 0.8rem 1rem;
  margin-bottom: 1rem;
  border-radius: var(--radius-sm);
  background: var(--bg-tertiary);
  border-left: 3px solid var(--brass);
  transition: background-color 200ms ease, border-color 200ms ease;
}

.version-banner__actions {
  display: flex;
  gap: 0.6rem;
}

/* --- Workspace layout --------------------------------------------- */
.workspace {
  display: grid;
  grid-template-columns: 300px minmax(0, 1fr);
  gap: 1rem;
}

.sidebar,
.transcript-panel {
  min-height: 72vh;
}

.sidebar {
  display: grid;
  gap: 1rem;
  align-content: start;
}

/* --- Sidebar cards (with corner brackets) ------------------------- */
.sidebar-card {
  position: relative;
  padding: 1.2rem;
  border-radius: var(--radius-md);
  background: var(--bg-secondary);
  border: 1px solid var(--line);
  transition: background-color 200ms ease, border-color 200ms ease;
}

/* Corner brackets */
.sidebar-card::before,
.sidebar-card::after {
  content: "";
  position: absolute;
  width: 14px;
  height: 14px;
  pointer-events: none;
}

.sidebar-card::before {
  top: 6px;
  left: 6px;
  border-top: 1px solid var(--brass-dark);
  border-left: 1px solid var(--brass-dark);
}

.sidebar-card::after {
  bottom: 6px;
  right: 6px;
  border-bottom: 1px solid var(--brass-dark);
  border-right: 1px solid var(--brass-dark);
}

.sidebar-list {
  margin: 0;
  padding-left: 1.1rem;
  color: var(--text-secondary);
  line-height: 1.65;
  font-size: 0.92rem;
}

.command-grid {
  display: flex;
  flex-wrap: wrap;
  gap: 0.5rem;
}

.command-chip {
  padding: 0.5rem 0.8rem;
  font-size: 0.85rem;
  background: var(--bg-tertiary);
  color: var(--text-primary);
  border: 1px solid var(--line);
  border-radius: 6px;
  font-family: "JetBrains Mono", Consolas, monospace;
  box-shadow: none;
}

.command-chip:hover {
  border-color: var(--brass);
  color: var(--brass);
  transform: none;
  box-shadow: none;
}

.sidebar-card__hint {
  margin: 0.8rem 0 0;
  color: var(--text-tertiary);
  font-size: 0.88rem;
}

.sidebar-card__hint code {
  background: var(--bg-tertiary);
  padding: 0.15em 0.4em;
  border-radius: 3px;
  font-size: 0.88em;
  border: 1px solid var(--line);
}

/* --- Transcript panel --------------------------------------------- */
.transcript-panel {
  display: grid;
  grid-template-rows: minmax(0, 1fr) auto;
  overflow: hidden;
  border-radius: var(--radius-lg);
  background: var(--bg-secondary);
  border: 1px solid var(--line);
  transition: background-color 200ms ease, border-color 200ms ease;
}

.transcript {
  min-height: 0;
  overflow: auto;
  padding: 1.2rem;
  display: grid;
  gap: 1rem;
}

/* Scrollbar styling */
.transcript::-webkit-scrollbar {
  width: 6px;
}

.transcript::-webkit-scrollbar-track {
  background: transparent;
}

.transcript::-webkit-scrollbar-thumb {
  background: var(--brass-dark);
  border-radius: 3px;
}

.transcript::-webkit-scrollbar-thumb:hover {
  background: var(--brass);
}

/* --- Turn layout -------------------------------------------------- */
.turn {
  display: grid;
  gap: 0.55rem;
}

.turn--user {
  justify-items: end;
}

.turn__body {
  max-width: min(92%, 56rem);
  padding: 1rem 1.15rem;
  border-radius: var(--radius-md);
  line-height: 1.68;
  transition: background-color 200ms ease, border-color 200ms ease;
}

/* User turn: teal-tinted */
.turn__body--user {
  background: linear-gradient(135deg, rgba(46, 139, 122, 0.12), rgba(46, 139, 122, 0.22));
  border-top-right-radius: 4px;
  border: 1px solid rgba(46, 139, 122, 0.18);
}

/* Assistant turn: dark surface + brass left border */
.turn__body--assistant {
  background: var(--bg-tertiary);
  border-left: 2px solid var(--brass);
  border-radius: var(--radius-md);
  border-top-left-radius: 2px;
}

.turn__body--assistant :is(p, ul, ol, pre, blockquote, h1, h2, h3, h4) {
  margin-top: 0;
}

.turn__body--assistant :is(p, ul, ol, blockquote, pre) + :is(p, ul, ol, blockquote, pre, h1, h2, h3, h4) {
  margin-top: 1rem;
}

/* Code blocks inside assistant */
.turn__body--assistant pre {
  overflow: auto;
  padding: 1rem;
  border-radius: 8px;
  background: var(--console);
  border: 1px solid var(--console-line);
}

.turn__body--assistant code {
  font-size: 0.9em;
}

/* Inline code in assistant */
.turn__body--assistant :not(pre) > code {
  background: rgba(201, 168, 76, 0.08);
  padding: 0.15em 0.4em;
  border-radius: 3px;
  border: 1px solid var(--line);
  font-size: 0.88em;
}

.turn__body--assistant a {
  color: var(--brass);
  text-decoration-color: var(--brass-dark);
}

.turn__body--assistant a:hover {
  color: var(--brass-light);
}

.turn__body--assistant blockquote {
  border-left: 3px solid var(--teal);
  margin-left: 0;
  padding: 0.5rem 1rem;
  color: var(--text-secondary);
  background: rgba(46, 139, 122, 0.05);
  border-radius: 0 6px 6px 0;
}

.turn__placeholder,
.render-note {
  color: var(--text-tertiary);
  margin: 0;
}

/* --- QED marker --------------------------------------------------- */
.qed-marker {
  text-align: right;
  color: var(--qed-gold);
  font-size: 0.65rem;
  opacity: 0.7;
  margin-top: 0.75rem;
  line-height: 1;
}

/* --- Tool stack --------------------------------------------------- */
.tool-stack {
  display: grid;
  gap: 0.7rem;
}

.tool-card,
.tool-panel {
  border-radius: 10px;
  background: linear-gradient(180deg, rgba(15, 13, 20, 0.97), rgba(10, 8, 16, 0.99));
  color: var(--text-primary);
  overflow: hidden;
  border: 1px solid var(--console-line);
}

.tool-card__header {
  width: 100%;
  display: flex;
  justify-content: space-between;
  align-items: flex-start;
  gap: 1rem;
  padding: 0.85rem 1rem;
  border-radius: 0;
  background: transparent;
  box-shadow: none;
  color: inherit;
  text-align: left;
}

.tool-card__header:hover {
  transform: none;
  box-shadow: none;
}

.tool-card__name {
  display: block;
  font-size: 0.88rem;
  color: var(--brass);
}

.tool-card__args {
  display: block;
  margin-top: 0.35rem;
  color: var(--text-tertiary);
  font-size: 0.78rem;
  word-break: break-all;
}

.tool-card__status {
  white-space: nowrap;
  font-size: 0.75rem;
  text-transform: uppercase;
  letter-spacing: 0.12em;
  font-family: "JetBrains Mono", Consolas, monospace;
}

.tool-card__status--running {
  color: var(--brass-light);
}

.tool-card__status--done {
  color: var(--teal-light);
}

.tool-card__status--error {
  color: var(--error-red);
}

.tool-card__body {
  display: none;
  border-top: 1px solid var(--console-line);
}

.tool-card--open .tool-card__body,
.tool-panel.tool-card--open .tool-card__body {
  display: block;
}

.tool-card__output,
.tool-card__result {
  padding: 0.85rem 1rem;
  white-space: pre-wrap;
  word-break: break-word;
}

.tool-card__output {
  font-family: "JetBrains Mono", Consolas, monospace;
  font-size: 0.84rem;
  line-height: 1.5;
  color: var(--text-secondary);
}

.tool-card__result {
  border-top: 1px solid var(--console-line);
  color: var(--text-primary);
}

/* --- Thinking blocks ---------------------------------------------- */
.thinking-block {
  max-width: min(92%, 56rem);
  border-radius: 10px;
  overflow: hidden;
  background: rgba(46, 139, 122, 0.05);
  border-left: 3px solid var(--teal);
  transition: background-color 200ms ease;
}

.thinking-block__summary {
  width: 100%;
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 0.7rem 1rem;
  border-radius: 0;
  background: transparent;
  box-shadow: none;
  color: var(--teal-light);
  font-size: 0.88rem;
}

.thinking-block__summary:hover {
  transform: none;
  box-shadow: none;
  color: var(--teal-light);
}

.thinking-block__body {
  display: none;
  padding: 0 1rem 1rem;
  color: var(--text-tertiary);
  white-space: pre-wrap;
  font-size: 0.9rem;
  line-height: 1.55;
}

.thinking-block--open .thinking-block__body,
.thinking-block--ready .thinking-block__body {
  display: block;
}

.thinking-block__chevron {
  font-size: 1rem;
  color: var(--teal);
}

/* --- Composer ----------------------------------------------------- */
.composer-shell {
  position: relative;
  padding: 1rem;
}

.composer {
  padding: 1rem;
  border-radius: var(--radius-md);
  background: var(--bg-secondary);
  border: 1px solid var(--line);
  transition: background-color 200ms ease, border-color 200ms ease;
}

.composer textarea {
  width: 100%;
  min-height: 6.5rem;
  resize: vertical;
  padding: 0.9rem 1rem;
  border-radius: 8px;
  border: 1px solid var(--line);
  background: var(--bg-primary);
  color: var(--text-primary);
  line-height: 1.6;
  font-size: 0.95rem;
  transition: border-color 200ms ease, background-color 200ms ease;
}

.composer textarea:focus,
.pairing-dialog input:focus {
  outline: none;
  border-color: var(--brass);
  box-shadow: 0 0 0 3px var(--brass-glow);
}

.composer textarea::placeholder {
  color: var(--text-tertiary);
}

.composer__actions {
  display: flex;
  justify-content: space-between;
  gap: 1rem;
  margin-top: 0.75rem;
  align-items: center;
}

.composer__hint {
  margin: 0;
  color: var(--text-tertiary);
  font-size: 0.82rem;
}

.composer__buttons {
  display: flex;
  gap: 0.6rem;
}

/* --- Slash popover ------------------------------------------------ */
.slash-popover {
  position: absolute;
  left: 1rem;
  right: 1rem;
  bottom: calc(100% + 0.4rem);
  padding: 0.4rem;
  border-radius: 10px;
  background: var(--bg-tertiary);
  box-shadow: var(--shadow);
  border: 1px solid var(--line);
  display: grid;
  gap: 0.2rem;
}

.slash-popover__item {
  width: 100%;
  display: flex;
  justify-content: space-between;
  gap: 1rem;
  padding: 0.65rem 0.8rem;
  border-radius: 6px;
  background: transparent;
  color: var(--text-primary);
  box-shadow: none;
  font-size: 0.9rem;
}

.slash-popover__item:hover {
  transform: none;
  box-shadow: none;
}

.slash-popover__item span {
  color: var(--text-tertiary);
}

.slash-popover__item--selected,
.slash-popover__item:hover {
  background: var(--brass-glow);
  border-color: transparent;
}

.slash-popover__item--selected strong,
.slash-popover__item:hover strong {
  color: var(--brass);
}

/* --- Pairing modal ------------------------------------------------ */
.pairing-modal {
  position: fixed;
  inset: 0;
  z-index: 100;
  display: grid;
  place-items: center;
  background: rgba(0, 0, 0, 0.6);
  backdrop-filter: blur(4px);
  padding: 1rem;
}

.pairing-modal[hidden] {
  display: none;
}

.pairing-dialog {
  position: relative;
  width: min(30rem, 100%);
  padding: 1.5rem;
  border-radius: var(--radius-lg);
  background: var(--bg-secondary);
  border: 1px solid var(--brass-dark);
  box-shadow: var(--shadow);
}

/* Corner brackets on dialog */
.pairing-dialog::before,
.pairing-dialog::after {
  content: "";
  position: absolute;
  width: 18px;
  height: 18px;
  pointer-events: none;
}

.pairing-dialog::before {
  top: 8px;
  left: 8px;
  border-top: 1px solid var(--brass);
  border-left: 1px solid var(--brass);
}

.pairing-dialog::after {
  bottom: 8px;
  right: 8px;
  border-bottom: 1px solid var(--brass);
  border-right: 1px solid var(--brass);
}

.pairing-copy,
.pairing-error {
  color: var(--text-secondary);
}

.pairing-error {
  color: var(--error-red);
}

.pairing-dialog input {
  width: 100%;
  margin: 1rem 0 0.75rem;
  padding: 0.9rem;
  border-radius: 8px;
  border: 1px solid var(--line);
  background: var(--bg-primary);
  color: var(--text-primary);
  text-align: center;
  letter-spacing: 0.36em;
  font-size: 1.2rem;
  font-family: "JetBrains Mono", Consolas, monospace;
}

/* --- Gear backdrop ------------------------------------------------ */
.gear-backdrop {
  position: fixed;
  bottom: -60px;
  right: -60px;
  z-index: 0;
  pointer-events: none;
  opacity: 0.03;
}

[data-theme="light"] .gear-backdrop {
  opacity: 0.05;
}

.gear {
  position: absolute;
  fill: none;
  stroke: var(--brass);
  stroke-width: 1.5;
}

.gear--large {
  width: 280px;
  height: 280px;
  bottom: 0;
  right: 0;
  animation: spin-cw 120s linear infinite;
}

.gear--small {
  width: 140px;
  height: 140px;
  bottom: 180px;
  right: 200px;
  animation: spin-ccw 60s linear infinite;
}

@keyframes spin-cw {
  to { transform: rotate(360deg); }
}

@keyframes spin-ccw {
  to { transform: rotate(-360deg); }
}

@media (prefers-reduced-motion: reduce) {
  .gear--large,
  .gear--small {
    animation: none;
  }
}

@media (max-width: 767px) {
  .gear-backdrop {
    display: none;
  }
}

/* --- Keyframes ---------------------------------------------------- */
@keyframes pulse {
  0%,
  100% {
    transform: scale(1);
    opacity: 1;
  }
  50% {
    transform: scale(0.78);
    opacity: 0.55;
  }
}

/* --- highlight.js dark override ----------------------------------- */
.hljs {
  background: var(--console) !important;
  color: var(--text-primary) !important;
}

.hljs-keyword { color: #8FA4B8; }
.hljs-string { color: var(--brass); }
.hljs-number { color: var(--brass-light); }
.hljs-comment { color: var(--text-tertiary); font-style: italic; }
.hljs-function { color: #D4A76A; }
.hljs-type,
.hljs-title.class_ { color: #7DAA6E; }
.hljs-built_in { color: var(--teal-light); }
.hljs-attr { color: var(--brass); }
.hljs-literal { color: var(--brass-light); }
.hljs-meta { color: var(--text-secondary); }
.hljs-selector-class,
.hljs-selector-id { color: var(--teal-light); }

/* --- Responsive: tablet ------------------------------------------- */
@media (max-width: 960px) {
  .workspace {
    grid-template-columns: 1fr;
  }

  .sidebar {
    order: 2;
  }
}

/* --- Responsive: mobile ------------------------------------------ */
@media (max-width: 720px) {
  .app-shell {
    width: min(100vw - 1rem, 100%);
    padding-top: 0.75rem;
  }

  .masthead,
  .composer__actions,
  .version-banner {
    flex-direction: column;
    align-items: stretch;
  }

  .masthead__status {
    justify-content: flex-start;
  }

  .composer__buttons,
  .version-banner__actions {
    justify-content: stretch;
  }

  .turn__body,
  .thinking-block {
    max-width: 100%;
  }
}
|ui}

let chat_js =
  {ui|(()=>{var et=/\u001b\[([0-9;]*)m/g;function S(t){return t.replaceAll("&","&amp;").replaceAll("<","&lt;").replaceAll(">","&gt;").replaceAll('"',"&quot;")}function nt(t){return new Map([[30,"#1f2328"],[31,"#c64537"],[32,"#2f7d4b"],[33,"#8e6110"],[34,"#3058a6"],[35,"#7d4689"],[36,"#1e6f72"],[37,"#d8d2c4"],[90,"#6b7280"],[91,"#ef6a5b"],[92,"#58b26d"],[93,"#d7a045"],[94,"#6f94ff"],[95,"#bb74e3"],[96,"#46b7bc"],[97,"#fff8ea"]]).get(t)??null}function P(t){let e=[];if(t.color)e.push(`color:${t.color}`);if(t.bold)e.push("font-weight:700");return e.join(";")}function rt(t,e){if(e===0){t.color=null,t.bold=!1;return}if(e===1){t.bold=!0;return}if(e===22){t.bold=!1;return}if(e===39){t.color=null;return}let n=nt(e);if(n)t.color=n}function O(t){let e={color:null,bold:!1},n=0,r="",i=null;while((i=et.exec(t))!==null){let s=t.slice(n,i.index);if(s){let l=S(s),h=P(e);r+=h?`<span style="${h}">${l}</span>`:l}let d=i[1]===""?[0]:i[1].split(";").map((l)=>Number.parseInt(l,10));for(let l of d)rt(e,Number.isNaN(l)?0:l);n=i.index+i[0].length}let o=t.slice(n);if(o){let s=S(o),d=P(e);r+=d?`<span style="${d}">${s}</span>`:s}return r.replaceAll(`
`,"<br>")}function y(t){return S(t)}class k{root;summary;body;content="";constructor(){this.root=document.createElement("div"),this.root.className="thinking-block",this.root.hidden=!0,this.summary=document.createElement("button"),this.summary.type="button",this.summary.className="thinking-block__summary",this.summary.innerHTML='<span>Thinking trace</span><span class="thinking-block__chevron">+</span>',this.body=document.createElement("div"),this.body.className="thinking-block__body",this.summary.addEventListener("click",()=>{this.root.classList.toggle("thinking-block--open")}),this.root.append(this.summary,this.body)}get element(){return this.root}append(t){this.content+=t,this.root.hidden=!1,this.body.textContent=this.content}finalize(){if(this.content.trim())this.root.classList.add("thinking-block--ready")}}class L{id;root;status;output;result;rawOutput="";constructor(t,e,n){this.id=t,this.root=document.createElement("div"),this.root.className="tool-card tool-panel";let r=document.createElement("button");r.type="button",r.className="tool-card__header",r.innerHTML=`
      <span>
        <span class="tool-card__name">${y(e)}</span>
        <span class="tool-card__args">${y(n||"{}")}</span>
      </span>
    `,this.status=document.createElement("span"),this.status.className="tool-card__status tool-card__status--running",this.status.textContent="running",r.append(this.status);let i=document.createElement("div");i.className="tool-card__body",this.output=document.createElement("div"),this.output.className="tool-card__output",this.result=document.createElement("div"),this.result.className="tool-card__result",r.addEventListener("click",()=>{this.root.classList.toggle("tool-card--open")}),i.append(this.output,this.result),this.root.append(r,i)}get element(){return this.root}appendOutput(t){this.rawOutput+=t,this.root.classList.add("tool-card--open"),this.output.innerHTML=O(this.rawOutput)}finish(t,e){this.status.className=`tool-card__status ${e?"tool-card__status--error":"tool-card__status--done"}`,this.status.textContent=e?"error":"done",this.result.textContent=t,this.root.classList.add("tool-card--open")}}var _=null;function $(t){return y(t).replaceAll(`
`,"<br>")}function ot(){if(!window.marked||!window.hljs)return;window.marked.setOptions({breaks:!0,gfm:!0,highlight(t,e){if(e&&window.hljs.getLanguage(e))return window.hljs.highlight(t,{language:e}).value;return window.hljs.highlightAuto(t).value}})}async function st(){if(window.mermaid)return;if(!_)_=new Promise((t,e)=>{let n=document.querySelector('script[data-mermaid-loader="true"]');if(n){n.addEventListener("load",()=>t(),{once:!0}),n.addEventListener("error",()=>e(Error("Failed to load mermaid")),{once:!0});return}let r=document.createElement("script");r.src="https://cdn.jsdelivr.net/npm/mermaid@11.12.3/dist/mermaid.min.js",r.integrity="sha384-jFhLSLFn4m565eRAS0CDMWubMqOtfZWWbE8kqgGdU+VHbJ3B2G/4X8u+0BM8MtdU",r.crossOrigin="anonymous",r.defer=!0,r.dataset.mermaidLoader="true",r.addEventListener("load",()=>t(),{once:!0}),r.addEventListener("error",()=>e(Error("Failed to load mermaid")),{once:!0}),document.head.append(r)}).then(()=>{let t=document.documentElement.getAttribute("data-theme")==="light"?"neutral":"dark";window.mermaid?.initialize({startOnLoad:!1,theme:t})});await _}async function it(t){let e=Array.from(t.querySelectorAll("pre code.language-mermaid"));if(e.length===0)return;try{await st(),await window.mermaid?.run({nodes:e.map((n)=>n.parentElement).filter(Boolean)})}catch{for(let n of e){let r=document.createElement("p");r.className="render-note",r.textContent="Mermaid diagram failed to render.",n.parentElement?.after(r)}}}class M{element;constructor(t){let e=document.createElement("article");e.className="turn turn--user",e.innerHTML=`
      <div class="turn__eyebrow">you</div>
      <div class="turn__body turn__body--user">${$(t)}</div>
    `,this.element=e}}class x{element;textBody;toolStack;thinking;toolPanels=new Map;rawText="";constructor(){let t=document.createElement("article");t.className="turn turn--assistant";let e=document.createElement("div");e.className="turn__eyebrow",e.textContent="assistant",this.thinking=new k,this.toolStack=document.createElement("div"),this.toolStack.className="tool-stack",this.textBody=document.createElement("div"),this.textBody.className="turn__body turn__body--assistant",this.textBody.innerHTML='<p class="turn__placeholder">Waiting for stream...</p>',t.append(e,this.thinking.element,this.toolStack,this.textBody),this.element=t}appendText(t){this.rawText+=t,this.textBody.textContent=this.rawText}appendThinking(t){this.thinking.append(t)}startTool(t,e,n){if(this.toolPanels.has(t))return;let r=new L(t,e,n);this.toolPanels.set(t,r),this.toolStack.append(r.element)}appendToolOutput(t,e){let n=this.toolPanels.get(t);if(n)n.appendOutput(e)}finishTool(t,e,n,r){if(!this.toolPanels.has(t))this.startTool(t,e,"{}");this.toolPanels.get(t)?.finish(n,r)}async finalize(){if(this.thinking.finalize(),ot(),!this.rawText.trim()){this.textBody.innerHTML='<p class="turn__placeholder">No assistant text for this turn.</p>';return}if(!window.marked||!window.DOMPurify){this.textBody.innerHTML=$(this.rawText);return}let t=window.marked.parse(this.rawText);this.textBody.innerHTML=window.DOMPurify.sanitize(t);let e=document.createElement("div");e.className="qed-marker",e.textContent="◼",this.textBody.append(e),await it(this.textBody)}}function at(t,e){let r=t.slice(0,e).match(/(?:^|\s)\/([a-z0-9_-]*)$/i);if(!r)return null;return{start:e-r[1].length-1,query:r[1].toLowerCase()}}function I(t){let{input:e,popover:n,fetchCommands:r}=t,i=null,o=0,s=null;async function d(){if(!i)i=await r();return i}function l(){n.hidden=!0,n.innerHTML="",s=null,o=0}function h(a){if(!s)return;let u=e.selectionStart??e.value.length;e.value=`${e.value.slice(0,s.start)}/${a.name} ${e.value.slice(u)}`;let m=s.start+a.name.length+2;e.setSelectionRange(m,m),e.focus(),l()}async function w(){let a=e.selectionStart??e.value.length;if(s=at(e.value,a),!s){l();return}let u=(await d()).filter((m)=>m.name.toLowerCase().startsWith(s.query)).slice(0,8);if(u.length===0){l();return}o=Math.min(o,u.length-1),n.innerHTML="",u.forEach((m,Z)=>{let f=document.createElement("button");f.type="button",f.className=`slash-popover__item ${Z===o?"slash-popover__item--selected":""}`,f.innerHTML=`<strong>/${m.name}</strong><span>${m.description}</span>`,f.addEventListener("mousedown",(tt)=>{tt.preventDefault(),h(m)}),n.append(f)}),n.hidden=!1}return e.addEventListener("input",()=>{w()}),e.addEventListener("click",()=>{w()}),e.addEventListener("keydown",async(a)=>{if(n.hidden)return;let u=Array.from(n.querySelectorAll(".slash-popover__item"));if(u.length===0)return;if(a.key==="ArrowDown"){a.preventDefault(),o=(o+1)%u.length,await w();return}if(a.key==="ArrowUp"){a.preventDefault(),o=(o-1+u.length)%u.length,await w();return}if(a.key==="Escape"){a.preventDefault(),l();return}if(a.key==="Tab"||a.key==="Enter"){let m=u[o];if(!m)return;a.preventDefault(),m.dispatchEvent(new MouseEvent("mousedown",{bubbles:!0}))}}),document.addEventListener("click",(a)=>{if(a.target instanceof Node&&(n.contains(a.target)||e.contains(a.target)))return;l()}),{close:l}}function V(t){let e=t.split(`
`).filter((r)=>r.startsWith("data:")).map((r)=>r.slice(5).trimStart());if(e.length===0)return[];let n=e.join(`
`).trim();if(!n||n==="[DONE]")return[{type:"done"}];try{return[JSON.parse(n)]}catch{return[{type:"error",message:`Malformed stream event: ${n}`}]}}async function*j(t){if(!t.body){yield{type:"error",message:"Response body missing"};return}let e=t.body.getReader(),n=new TextDecoder,r="";while(!0){let{done:o,value:s}=await e.read();if(o)break;r+=n.decode(s,{stream:!0});let d=r.split(`

`);r=d.pop()??"";for(let l of d)for(let h of V(l))yield h}let i=r.trim();if(i)for(let o of V(i))yield o}async function lt(){try{let t=await fetch("/ui-version",{cache:"no-store"});if(!t.ok)return null;return(await t.json()).version??null}catch{return null}}function R(t){let e=document.querySelector('meta[name="ui-version"]')?.content??"",{banner:n,dismissButton:r,reloadButton:i}=t;function o(){if(sessionStorage.getItem("clawq_ui_version_banner_dismissed")===e)return;n.hidden=!1}async function s(){let d=await lt();if(d&&e&&d!==e)o()}r.addEventListener("click",()=>{sessionStorage.setItem("clawq_ui_version_banner_dismissed",e),n.hidden=!0}),i.addEventListener("click",()=>{window.location.reload()}),document.addEventListener("visibilitychange",()=>{if(!document.hidden)s()}),window.setInterval(()=>{s()},300000)}var H="clawq_ui_session_id",F="clawq_ui_token",E=document.querySelector("#transcript"),q=document.querySelector("#composer-form"),p=document.querySelector("#composer-input"),B=document.querySelector("#send-btn"),b=document.querySelector("#stop-btn"),K=document.querySelector("#status-pill"),U=document.querySelector("#status-text"),W=document.querySelector("#slash-popover"),A=document.querySelector("#pair-modal"),v=document.querySelector("#pairing-code"),D=document.querySelector("#pairing-submit"),g=document.querySelector("#pairing-error"),Y=document.querySelector("#new-session-btn"),ct=document.querySelector("#theme-toggle"),J=document.querySelector("#version-banner"),G=document.querySelector("#version-dismiss"),Q=document.querySelector("#version-reload");if(!E||!q||!p||!B||!b||!K||!U||!W||!A||!v||!D||!g||!Y||!J||!G||!Q)throw Error("UI boot failed: missing required DOM nodes");var T=null,C=null;function dt(){let t=localStorage.getItem(H);if(t)return t;let e=`web-${Math.random().toString(36).slice(2)}${Date.now().toString(36)}`;return localStorage.setItem(H,e),e}function ut(){localStorage.removeItem(H),E.innerHTML="",p.focus()}function mt(){return localStorage.getItem(F)??""}function pt(t){localStorage.setItem(F,t)}function c(t,e){K.className=`status-pill status-pill--${t}`,U.textContent=e}function N(){E.scrollTop=E.scrollHeight}function z(t){E.append(t.element),N()}function ht(t){p.value=t,p.focus(),p.setSelectionRange(t.length,t.length)}async function gt(){let t=await fetch("/commands",{cache:"force-cache"});if(!t.ok)return[];return await t.json()}function ft(t){C=t,A.hidden=!1,v.value="",g.textContent="",v.focus()}function yt(){A.hidden=!0,g.textContent=""}function Et(t,e){switch(e.type){case"delta":t.appendText(e.content),c("streaming","streaming reply");break;case"thinking_delta":t.appendThinking(e.content),c("thinking","thinking");break;case"tool_start":t.startTool(e.id,e.name,e.arguments),c("streaming",`running ${e.name}`);break;case"tool_output_delta":t.appendToolOutput(e.id,e.chunk);break;case"tool_result":t.finishTool(e.id,e.name,e.result,e.is_error);break;case"tool_call_delta":break;case"error":t.appendText(`

[stream error] ${e.message}`),c("error","stream error");break;case"done":break;default:break}N()}async function X(t){let e=t.trim();if(!e)return;z(new M(e));let n=new x;z(n),T=new AbortController,c("thinking","contacting daemon"),B.disabled=!0,b.hidden=!1;try{let r={"Content-Type":"application/json"},i=mt();if(i)r.Authorization=`Bearer ${i}`;let o=await fetch("/chat/stream",{method:"POST",headers:r,body:JSON.stringify({message:e,session_id:dt()}),signal:T.signal});if(o.status===401||o.status===403){ft(()=>{X(e)}),n.appendText("Pairing required before chat can continue."),c("idle","pairing required");return}if(!o.ok){let s=await o.text();n.appendText(`Error: ${o.status} ${s}`),c("error","request failed");return}for await(let s of j(o))if(Et(n,s),s.type==="done")break;await n.finalize(),c("idle","ready")}catch(r){if(r instanceof DOMException&&r.name==="AbortError"){c("idle","stopped");return}n.appendText(`Error: ${r instanceof Error?r.message:String(r)}`),c("error","network error")}finally{T=null,B.disabled=!1,b.hidden=!0,N()}}I({input:p,popover:W,fetchCommands:gt});R({banner:J,dismissButton:G,reloadButton:Q});q.addEventListener("submit",(t)=>{t.preventDefault();let e=p.value;if(!e.trim())return;p.value="",X(e)});p.addEventListener("keydown",(t)=>{if(t.key==="Enter"&&!t.shiftKey)t.preventDefault(),q.requestSubmit()});b.addEventListener("click",()=>{T?.abort()});D.addEventListener("click",async()=>{let t=v.value.trim();if(!/^\d{6}$/.test(t)){g.textContent="Enter a 6-digit code.";return}try{let e=await fetch("/pair",{method:"POST",headers:{"Content-Type":"application/json"},body:JSON.stringify({code:t})}),n=await e.json();if(!e.ok||!n.token){g.textContent=n.error??"Pairing failed.";return}pt(n.token),yt(),C?.(),C=null,c("idle","paired")}catch(e){g.textContent=e instanceof Error?e.message:String(e)}});v.addEventListener("keydown",(t)=>{if(t.key==="Enter")t.preventDefault(),D.click()});Y.addEventListener("click",()=>{ut(),c("idle","new session")});ct?.addEventListener("click",()=>{let e=document.documentElement.getAttribute("data-theme")==="light"?"dark":"light";document.documentElement.setAttribute("data-theme",e),localStorage.setItem("clawq_theme",e)});for(let t of document.querySelectorAll(".command-chip"))t.addEventListener("click",()=>{ht(t.dataset.command??"")});c("idle","ready");p.focus();})();
|ui}
