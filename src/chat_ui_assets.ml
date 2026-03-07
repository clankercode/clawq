(* Auto-generated chat UI assets - embedded UI bundle *)

let ui_version =
  {ui|sha256:28f3ec6733cea15a3ea8edd346f389b40d90487c2c5f5ecfab1b059fd38f8534|ui}

let index_html =
  {ui|<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <meta name="ui-version" content="dev">
  <title>clawq web chat</title>
  <link rel="stylesheet" href="/chat.css">
  <script defer src="https://cdn.jsdelivr.net/npm/dompurify@3.2.6/dist/purify.min.js" integrity="sha384-JEyTNhjM6R1ElGoJns4U2Ln4ofPcqzSsynQkmEc/KGy6336qAZl70tDLufbkla+3" crossorigin="anonymous"></script>
  <script defer src="https://cdn.jsdelivr.net/npm/marked@15.0.12/marked.min.js" integrity="sha384-948ahk4ZmxYVYOc+rxN1H2gM1EJ2Duhp7uHtZ4WSLkV4Vtx5MUqnV+l7u9B+jFv+" crossorigin="anonymous"></script>
  <script defer src="https://cdn.jsdelivr.net/npm/highlight.js@11.11.1/lib/common.min.js" integrity="sha384-PaYbudF4+JA0o1XwzEg2SdOGwBFfJFwiZ0hFm3lEVQjMSBqHMsGAKTEc0k6Lh6ig" crossorigin="anonymous"></script>
</head>
<body>
  <div class="app-shell">
    <header class="masthead">
      <div>
        <p class="eyebrow">stream console</p>
        <h1>clawq</h1>
        <p class="subtitle">A paper-notebook chat surface with live reasoning, tool traces, and slash control.</p>
      </div>
      <div class="masthead__status">
        <div class="status-pill status-pill--idle" id="status-pill">
          <span class="status-pill__dot"></span>
          <span id="status-text">ready</span>
        </div>
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
          <p class="sidebar-card__hint">Type <code>/</code> in the composer for live command suggestions.</p>
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
              <p class="composer__hint">Shift+Enter for a newline. Tool output streams inline.</p>
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

  <script defer src="/chat.js"></script>
</body>
</html>
|ui}

let chat_css =
  {ui|:root {
  --paper: #efe3cf;
  --paper-strong: #e3d0b2;
  --ink: #1f2630;
  --ink-soft: #47515d;
  --accent: #a54b2a;
  --accent-soft: #c9895e;
  --sage: #426154;
  --line: rgba(31, 38, 48, 0.14);
  --glass: rgba(255, 250, 242, 0.72);
  --console: #18212c;
  --console-line: rgba(246, 233, 210, 0.12);
  --shadow: 0 24px 60px rgba(65, 45, 19, 0.14);
  --radius-lg: 28px;
  --radius-md: 20px;
  --radius-sm: 14px;
  color-scheme: light;
}

* {
  box-sizing: border-box;
}

html,
body {
  min-height: 100%;
}

body {
  margin: 0;
  color: var(--ink);
  font-family: "Iowan Old Style", "Palatino Linotype", "Book Antiqua", Palatino, serif;
  background:
    radial-gradient(circle at top left, rgba(255, 247, 230, 0.92), transparent 36%),
    radial-gradient(circle at right 10% top 12%, rgba(197, 133, 88, 0.16), transparent 24%),
    linear-gradient(140deg, #f6ebd9 0%, #f0dec4 52%, #efe5d5 100%);
}

body::before {
  content: "";
  position: fixed;
  inset: 0;
  pointer-events: none;
  opacity: 0.4;
  background-image:
    linear-gradient(rgba(126, 94, 43, 0.04) 1px, transparent 1px),
    linear-gradient(90deg, rgba(126, 94, 43, 0.04) 1px, transparent 1px);
  background-size: 28px 28px;
  mask-image: linear-gradient(to bottom, rgba(0, 0, 0, 0.7), transparent 92%);
}

button,
textarea,
input {
  font: inherit;
}

button {
  border: none;
  border-radius: 999px;
  background: linear-gradient(135deg, var(--accent) 0%, #c36d45 100%);
  color: #fff8ef;
  cursor: pointer;
  padding: 0.85rem 1.3rem;
  transition: transform 140ms ease, box-shadow 140ms ease, opacity 140ms ease;
  box-shadow: 0 10px 18px rgba(165, 75, 42, 0.18);
}

button:hover {
  transform: translateY(-1px);
}

button:disabled {
  opacity: 0.55;
  cursor: not-allowed;
  transform: none;
}

.ghost-button {
  background: rgba(255, 251, 245, 0.7);
  color: var(--ink);
  box-shadow: inset 0 0 0 1px rgba(31, 38, 48, 0.12);
}

code,
pre,
.tool-card__name,
.tool-card__args,
.slash-popover__item strong {
  font-family: "IBM Plex Mono", "SFMono-Regular", Consolas, monospace;
}

.app-shell {
  width: min(1320px, calc(100vw - 2rem));
  margin: 0 auto;
  padding: 1.5rem 0 2rem;
}

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
  color: var(--ink-soft);
}

.masthead h1,
.pairing-dialog h2 {
  margin: 0;
  font-size: clamp(2.8rem, 7vw, 5rem);
  line-height: 0.94;
  font-weight: 600;
}

.subtitle {
  margin: 0.85rem 0 0;
  max-width: 42rem;
  color: var(--ink-soft);
  font-size: 1.05rem;
  line-height: 1.55;
}

.masthead__status {
  display: grid;
  gap: 0.8rem;
  justify-items: end;
}

.status-pill {
  display: inline-flex;
  align-items: center;
  gap: 0.65rem;
  padding: 0.7rem 1rem;
  border-radius: 999px;
  background: rgba(255, 250, 243, 0.76);
  box-shadow: inset 0 0 0 1px rgba(31, 38, 48, 0.08);
  color: var(--ink-soft);
}

.status-pill__dot {
  width: 0.68rem;
  height: 0.68rem;
  border-radius: 999px;
  background: #7d8c9d;
}

.status-pill--idle .status-pill__dot {
  background: var(--sage);
}

.status-pill--streaming .status-pill__dot {
  background: var(--accent);
}

.status-pill--thinking .status-pill__dot {
  background: #c48b25;
  animation: pulse 1.1s infinite ease-in-out;
}

.status-pill--error .status-pill__dot {
  background: #b3433b;
}

.version-banner {
  display: flex;
  justify-content: space-between;
  align-items: center;
  gap: 1rem;
  padding: 0.9rem 1rem;
  margin-bottom: 1rem;
  border-radius: var(--radius-sm);
  background: rgba(250, 240, 222, 0.92);
  box-shadow: inset 0 0 0 1px rgba(165, 75, 42, 0.16);
}

.version-banner__actions {
  display: flex;
  gap: 0.7rem;
}

.workspace {
  display: grid;
  grid-template-columns: 320px minmax(0, 1fr);
  gap: 1rem;
}

.sidebar,
.transcript-panel {
  min-height: 72vh;
}

.sidebar {
  display: grid;
  gap: 1rem;
}

.sidebar-card,
.transcript-panel,
.composer,
.pairing-dialog {
  border-radius: var(--radius-lg);
  background: var(--glass);
  backdrop-filter: blur(10px);
  box-shadow: var(--shadow);
  border: 1px solid rgba(255, 255, 255, 0.44);
}

.sidebar-card {
  padding: 1.25rem;
}

.sidebar-list {
  margin: 0;
  padding-left: 1.1rem;
  color: var(--ink-soft);
  line-height: 1.65;
}

.command-grid {
  display: flex;
  flex-wrap: wrap;
  gap: 0.6rem;
}

.command-chip {
  padding: 0.65rem 0.95rem;
  background: rgba(255, 251, 245, 0.8);
  color: var(--ink);
  box-shadow: inset 0 0 0 1px rgba(31, 38, 48, 0.08);
}

.sidebar-card__hint {
  margin: 0.9rem 0 0;
  color: var(--ink-soft);
}

.transcript-panel {
  display: grid;
  grid-template-rows: minmax(0, 1fr) auto;
  overflow: hidden;
}

.transcript {
  min-height: 0;
  overflow: auto;
  padding: 1.2rem;
  display: grid;
  gap: 1rem;
}

.turn {
  display: grid;
  gap: 0.65rem;
}

.turn--user {
  justify-items: end;
}

.turn__body {
  max-width: min(92%, 56rem);
  padding: 1rem 1.15rem;
  border-radius: var(--radius-md);
  line-height: 1.68;
}

.turn__body--user {
  background: linear-gradient(135deg, rgba(66, 97, 84, 0.12), rgba(66, 97, 84, 0.22));
  border-top-right-radius: 0.45rem;
}

.turn__body--assistant {
  background: rgba(255, 253, 248, 0.86);
  box-shadow: inset 0 0 0 1px rgba(31, 38, 48, 0.08);
}

.turn__body--assistant :is(p, ul, ol, pre, blockquote, h1, h2, h3, h4) {
  margin-top: 0;
}

.turn__body--assistant :is(p, ul, ol, blockquote, pre) + :is(p, ul, ol, blockquote, pre, h1, h2, h3, h4) {
  margin-top: 1rem;
}

.turn__body--assistant pre {
  overflow: auto;
  padding: 1rem;
  border-radius: 1rem;
  background: #fff8ef;
  box-shadow: inset 0 0 0 1px rgba(31, 38, 48, 0.08);
}

.turn__body--assistant code {
  font-size: 0.95em;
}

.turn__placeholder,
.render-note {
  color: var(--ink-soft);
  margin: 0;
}

.tool-stack {
  display: grid;
  gap: 0.8rem;
}

.tool-card,
.tool-panel {
  border-radius: 1.2rem;
  background: linear-gradient(180deg, rgba(24, 33, 44, 0.96), rgba(19, 28, 39, 0.98));
  color: #f6e9d2;
  overflow: hidden;
  border: 1px solid var(--console-line);
}

.tool-card__header {
  width: 100%;
  display: flex;
  justify-content: space-between;
  align-items: flex-start;
  gap: 1rem;
  padding: 1rem;
  border-radius: 0;
  background: transparent;
  box-shadow: none;
  color: inherit;
  text-align: left;
}

.tool-card__name {
  display: block;
  font-size: 0.92rem;
}

.tool-card__args {
  display: block;
  margin-top: 0.4rem;
  color: rgba(246, 233, 210, 0.7);
  font-size: 0.8rem;
  word-break: break-all;
}

.tool-card__status {
  white-space: nowrap;
  font-size: 0.78rem;
  text-transform: uppercase;
  letter-spacing: 0.12em;
}

.tool-card__status--running {
  color: #f5bd54;
}

.tool-card__status--done {
  color: #7fd8a5;
}

.tool-card__status--error {
  color: #ff9386;
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
  padding: 1rem;
  white-space: pre-wrap;
  word-break: break-word;
}

.tool-card__output {
  font-family: "IBM Plex Mono", "SFMono-Regular", Consolas, monospace;
  font-size: 0.88rem;
  line-height: 1.5;
}

.tool-card__result {
  border-top: 1px solid var(--console-line);
  color: rgba(246, 233, 210, 0.84);
}

.thinking-block {
  max-width: min(92%, 56rem);
  border-radius: 1rem;
  overflow: hidden;
  background: rgba(252, 246, 235, 0.8);
  box-shadow: inset 0 0 0 1px rgba(31, 38, 48, 0.08);
}

.thinking-block__summary {
  width: 100%;
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 0.8rem 1rem;
  border-radius: 0;
  background: transparent;
  box-shadow: none;
  color: var(--ink-soft);
}

.thinking-block__body {
  display: none;
  padding: 0 1rem 1rem;
  color: var(--ink-soft);
  white-space: pre-wrap;
}

.thinking-block--open .thinking-block__body,
.thinking-block--ready .thinking-block__body {
  display: block;
}

.thinking-block__chevron {
  font-size: 1.15rem;
}

.composer-shell {
  position: relative;
  padding: 1rem;
}

.composer {
  padding: 1rem;
}

.composer textarea {
  width: 100%;
  min-height: 7rem;
  resize: vertical;
  padding: 1rem 1.1rem;
  border-radius: 1.25rem;
  border: 1px solid rgba(31, 38, 48, 0.12);
  background: rgba(255, 252, 247, 0.92);
  color: var(--ink);
  line-height: 1.6;
}

.composer textarea:focus,
.pairing-dialog input:focus {
  outline: 2px solid rgba(165, 75, 42, 0.3);
  outline-offset: 2px;
}

.composer__actions {
  display: flex;
  justify-content: space-between;
  gap: 1rem;
  margin-top: 0.85rem;
  align-items: center;
}

.composer__hint {
  margin: 0;
  color: var(--ink-soft);
}

.composer__buttons {
  display: flex;
  gap: 0.7rem;
}

.slash-popover {
  position: absolute;
  left: 1rem;
  right: 1rem;
  bottom: calc(100% + 0.4rem);
  padding: 0.45rem;
  border-radius: 1.15rem;
  background: rgba(255, 250, 243, 0.97);
  box-shadow: var(--shadow);
  border: 1px solid rgba(31, 38, 48, 0.08);
  display: grid;
  gap: 0.25rem;
}

.slash-popover__item {
  width: 100%;
  display: flex;
  justify-content: space-between;
  gap: 1rem;
  padding: 0.75rem 0.9rem;
  border-radius: 0.95rem;
  background: transparent;
  color: var(--ink);
  box-shadow: none;
}

.slash-popover__item span {
  color: var(--ink-soft);
}

.slash-popover__item--selected,
.slash-popover__item:hover {
  background: rgba(165, 75, 42, 0.08);
}

.pairing-modal {
  position: fixed;
  inset: 0;
  display: grid;
  place-items: center;
  background: rgba(31, 38, 48, 0.38);
  padding: 1rem;
}

.pairing-dialog {
  width: min(32rem, 100%);
  padding: 1.6rem;
}

.pairing-copy,
.pairing-error {
  color: var(--ink-soft);
}

.pairing-dialog input {
  width: 100%;
  margin: 1rem 0 0.75rem;
  padding: 1rem;
  border-radius: 1rem;
  border: 1px solid rgba(31, 38, 48, 0.12);
  background: rgba(255, 252, 247, 0.94);
  text-align: center;
  letter-spacing: 0.36em;
  font-size: 1.25rem;
}

@keyframes pulse {
  0%,
  100% {
    transform: scale(1);
    opacity: 1;
  }
  50% {
    transform: scale(0.82);
    opacity: 0.6;
  }
}

@media (max-width: 960px) {
  .workspace {
    grid-template-columns: 1fr;
  }

  .sidebar {
    order: 2;
  }
}

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

  .masthead__status,
  .composer__buttons,
  .version-banner__actions {
    justify-items: stretch;
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
`,"<br>")}function ot(){if(!window.marked||!window.hljs)return;window.marked.setOptions({breaks:!0,gfm:!0,highlight(t,e){if(e&&window.hljs.getLanguage(e))return window.hljs.highlight(t,{language:e}).value;return window.hljs.highlightAuto(t).value}})}async function st(){if(window.mermaid)return;if(!_)_=new Promise((t,e)=>{let n=document.querySelector('script[data-mermaid-loader="true"]');if(n){n.addEventListener("load",()=>t(),{once:!0}),n.addEventListener("error",()=>e(Error("Failed to load mermaid")),{once:!0});return}let r=document.createElement("script");r.src="https://cdn.jsdelivr.net/npm/mermaid@11.12.3/dist/mermaid.min.js",r.integrity="sha384-jFhLSLFn4m565eRAS0CDMWubMqOtfZWWbE8kqgGdU+VHbJ3B2G/4X8u+0BM8MtdU",r.crossOrigin="anonymous",r.defer=!0,r.dataset.mermaidLoader="true",r.addEventListener("load",()=>t(),{once:!0}),r.addEventListener("error",()=>e(Error("Failed to load mermaid")),{once:!0}),document.head.append(r)}).then(()=>{window.mermaid?.initialize({startOnLoad:!1,theme:"neutral"})});await _}async function it(t){let e=Array.from(t.querySelectorAll("pre code.language-mermaid"));if(e.length===0)return;try{await st(),await window.mermaid?.run({nodes:e.map((n)=>n.parentElement).filter(Boolean)})}catch{for(let n of e){let r=document.createElement("p");r.className="render-note",r.textContent="Mermaid diagram failed to render.",n.parentElement?.after(r)}}}class M{element;constructor(t){let e=document.createElement("article");e.className="turn turn--user",e.innerHTML=`
      <div class="turn__eyebrow">you</div>
      <div class="turn__body turn__body--user">${$(t)}</div>
    `,this.element=e}}class x{element;textBody;toolStack;thinking;toolPanels=new Map;rawText="";constructor(){let t=document.createElement("article");t.className="turn turn--assistant";let e=document.createElement("div");e.className="turn__eyebrow",e.textContent="assistant",this.thinking=new k,this.toolStack=document.createElement("div"),this.toolStack.className="tool-stack",this.textBody=document.createElement("div"),this.textBody.className="turn__body turn__body--assistant",this.textBody.innerHTML='<p class="turn__placeholder">Waiting for stream...</p>',t.append(e,this.thinking.element,this.toolStack,this.textBody),this.element=t}appendText(t){this.rawText+=t,this.textBody.textContent=this.rawText}appendThinking(t){this.thinking.append(t)}startTool(t,e,n){if(this.toolPanels.has(t))return;let r=new L(t,e,n);this.toolPanels.set(t,r),this.toolStack.append(r.element)}appendToolOutput(t,e){let n=this.toolPanels.get(t);if(n)n.appendOutput(e)}finishTool(t,e,n,r){if(!this.toolPanels.has(t))this.startTool(t,e,"{}");this.toolPanels.get(t)?.finish(n,r)}async finalize(){if(this.thinking.finalize(),ot(),!this.rawText.trim()){this.textBody.innerHTML='<p class="turn__placeholder">No assistant text for this turn.</p>';return}if(!window.marked||!window.DOMPurify){this.textBody.innerHTML=$(this.rawText);return}let t=window.marked.parse(this.rawText);this.textBody.innerHTML=window.DOMPurify.sanitize(t),await it(this.textBody)}}function at(t,e){let r=t.slice(0,e).match(/(?:^|\s)\/([a-z0-9_-]*)$/i);if(!r)return null;return{start:e-r[1].length-1,query:r[1].toLowerCase()}}function I(t){let{input:e,popover:n,fetchCommands:r}=t,i=null,o=0,s=null;async function d(){if(!i)i=await r();return i}function l(){n.hidden=!0,n.innerHTML="",s=null,o=0}function h(a){if(!s)return;let u=e.selectionStart??e.value.length;e.value=`${e.value.slice(0,s.start)}/${a.name} ${e.value.slice(u)}`;let m=s.start+a.name.length+2;e.setSelectionRange(m,m),e.focus(),l()}async function w(){let a=e.selectionStart??e.value.length;if(s=at(e.value,a),!s){l();return}let u=(await d()).filter((m)=>m.name.toLowerCase().startsWith(s.query)).slice(0,8);if(u.length===0){l();return}o=Math.min(o,u.length-1),n.innerHTML="",u.forEach((m,Z)=>{let g=document.createElement("button");g.type="button",g.className=`slash-popover__item ${Z===o?"slash-popover__item--selected":""}`,g.innerHTML=`<strong>/${m.name}</strong><span>${m.description}</span>`,g.addEventListener("mousedown",(tt)=>{tt.preventDefault(),h(m)}),n.append(g)}),n.hidden=!1}return e.addEventListener("input",()=>{w()}),e.addEventListener("click",()=>{w()}),e.addEventListener("keydown",async(a)=>{if(n.hidden)return;let u=Array.from(n.querySelectorAll(".slash-popover__item"));if(u.length===0)return;if(a.key==="ArrowDown"){a.preventDefault(),o=(o+1)%u.length,await w();return}if(a.key==="ArrowUp"){a.preventDefault(),o=(o-1+u.length)%u.length,await w();return}if(a.key==="Escape"){a.preventDefault(),l();return}if(a.key==="Tab"||a.key==="Enter"){let m=u[o];if(!m)return;a.preventDefault(),m.dispatchEvent(new MouseEvent("mousedown",{bubbles:!0}))}}),document.addEventListener("click",(a)=>{if(a.target instanceof Node&&(n.contains(a.target)||e.contains(a.target)))return;l()}),{close:l}}function V(t){let e=t.split(`
`).filter((r)=>r.startsWith("data:")).map((r)=>r.slice(5).trimStart());if(e.length===0)return[];let n=e.join(`
`).trim();if(!n||n==="[DONE]")return[{type:"done"}];try{return[JSON.parse(n)]}catch{return[{type:"error",message:`Malformed stream event: ${n}`}]}}async function*j(t){if(!t.body){yield{type:"error",message:"Response body missing"};return}let e=t.body.getReader(),n=new TextDecoder,r="";while(!0){let{done:o,value:s}=await e.read();if(o)break;r+=n.decode(s,{stream:!0});let d=r.split(`

`);r=d.pop()??"";for(let l of d)for(let h of V(l))yield h}let i=r.trim();if(i)for(let o of V(i))yield o}async function lt(){try{let t=await fetch("/ui-version",{cache:"no-store"});if(!t.ok)return null;return(await t.json()).version??null}catch{return null}}function R(t){let e=document.querySelector('meta[name="ui-version"]')?.content??"",{banner:n,dismissButton:r,reloadButton:i}=t;function o(){if(sessionStorage.getItem("clawq_ui_version_banner_dismissed")===e)return;n.hidden=!1}async function s(){let d=await lt();if(d&&e&&d!==e)o()}r.addEventListener("click",()=>{sessionStorage.setItem("clawq_ui_version_banner_dismissed",e),n.hidden=!0}),i.addEventListener("click",()=>{window.location.reload()}),document.addEventListener("visibilitychange",()=>{if(!document.hidden)s()}),window.setInterval(()=>{s()},300000)}var H="clawq_ui_session_id",F="clawq_ui_token",E=document.querySelector("#transcript"),q=document.querySelector("#composer-form"),p=document.querySelector("#composer-input"),B=document.querySelector("#send-btn"),b=document.querySelector("#stop-btn"),K=document.querySelector("#status-pill"),U=document.querySelector("#status-text"),W=document.querySelector("#slash-popover"),D=document.querySelector("#pair-modal"),v=document.querySelector("#pairing-code"),A=document.querySelector("#pairing-submit"),f=document.querySelector("#pairing-error"),Y=document.querySelector("#new-session-btn"),J=document.querySelector("#version-banner"),G=document.querySelector("#version-dismiss"),Q=document.querySelector("#version-reload");if(!E||!q||!p||!B||!b||!K||!U||!W||!D||!v||!A||!f||!Y||!J||!G||!Q)throw Error("UI boot failed: missing required DOM nodes");var T=null,C=null;function ct(){let t=localStorage.getItem(H);if(t)return t;let e=`web-${Math.random().toString(36).slice(2)}${Date.now().toString(36)}`;return localStorage.setItem(H,e),e}function dt(){localStorage.removeItem(H),E.innerHTML="",p.focus()}function ut(){return localStorage.getItem(F)??""}function mt(t){localStorage.setItem(F,t)}function c(t,e){K.className=`status-pill status-pill--${t}`,U.textContent=e}function N(){E.scrollTop=E.scrollHeight}function z(t){E.append(t.element),N()}function pt(t){p.value=t,p.focus(),p.setSelectionRange(t.length,t.length)}async function ht(){let t=await fetch("/commands",{cache:"force-cache"});if(!t.ok)return[];return await t.json()}function ft(t){C=t,D.hidden=!1,v.value="",f.textContent="",v.focus()}function gt(){D.hidden=!0,f.textContent=""}function yt(t,e){switch(e.type){case"delta":t.appendText(e.content),c("streaming","streaming reply");break;case"thinking_delta":t.appendThinking(e.content),c("thinking","thinking");break;case"tool_start":t.startTool(e.id,e.name,e.arguments),c("streaming",`running ${e.name}`);break;case"tool_output_delta":t.appendToolOutput(e.id,e.chunk);break;case"tool_result":t.finishTool(e.id,e.name,e.result,e.is_error);break;case"tool_call_delta":break;case"error":t.appendText(`

[stream error] ${e.message}`),c("error","stream error");break;case"done":break;default:break}N()}async function X(t){let e=t.trim();if(!e)return;z(new M(e));let n=new x;z(n),T=new AbortController,c("thinking","contacting daemon"),B.disabled=!0,b.hidden=!1;try{let r={"Content-Type":"application/json"},i=ut();if(i)r.Authorization=`Bearer ${i}`;let o=await fetch("/chat/stream",{method:"POST",headers:r,body:JSON.stringify({message:e,session_id:ct()}),signal:T.signal});if(o.status===401||o.status===403){ft(()=>{X(e)}),n.appendText("Pairing required before chat can continue."),c("idle","pairing required");return}if(!o.ok){let s=await o.text();n.appendText(`Error: ${o.status} ${s}`),c("error","request failed");return}for await(let s of j(o))if(yt(n,s),s.type==="done")break;await n.finalize(),c("idle","ready")}catch(r){if(r instanceof DOMException&&r.name==="AbortError"){c("idle","stopped");return}n.appendText(`Error: ${r instanceof Error?r.message:String(r)}`),c("error","network error")}finally{T=null,B.disabled=!1,b.hidden=!0,N()}}I({input:p,popover:W,fetchCommands:ht});R({banner:J,dismissButton:G,reloadButton:Q});q.addEventListener("submit",(t)=>{t.preventDefault();let e=p.value;if(!e.trim())return;p.value="",X(e)});p.addEventListener("keydown",(t)=>{if(t.key==="Enter"&&!t.shiftKey)t.preventDefault(),q.requestSubmit()});b.addEventListener("click",()=>{T?.abort()});A.addEventListener("click",async()=>{let t=v.value.trim();if(!/^\d{6}$/.test(t)){f.textContent="Enter a 6-digit code.";return}try{let e=await fetch("/pair",{method:"POST",headers:{"Content-Type":"application/json"},body:JSON.stringify({code:t})}),n=await e.json();if(!e.ok||!n.token){f.textContent=n.error??"Pairing failed.";return}mt(n.token),gt(),C?.(),C=null,c("idle","paired")}catch(e){f.textContent=e instanceof Error?e.message:String(e)}});v.addEventListener("keydown",(t)=>{if(t.key==="Enter")t.preventDefault(),A.click()});Y.addEventListener("click",()=>{dt(),c("idle","new session")});for(let t of document.querySelectorAll(".command-chip"))t.addEventListener("click",()=>{pt(t.dataset.command??"")});c("idle","ready");p.focus();})();
|ui}
