# Clawq — The Formal AI Assistant

<p align="center">
  <img src="docs/clawq-cover.webp" alt="Clawq, The Formal AI Assistant — Mastering Polite Automation and Protocol" width="100%" />
</p>

<p align="center">
  <img src="docs/public/badges/formal-verification.svg" alt="Formal Verification" />
</p>

A *formally* verified personal AI assistant runtime — Coq-proven core properties extracted to OCaml, with impeccable manners and machine-checked correctness. Multi-channel support (CLI, Telegram, Discord, Slack), HTTP gateway, streaming web chat UI, cron scheduling, audit logging, and MCP server.

## Quick Help

```bash
# First-time setup — interactive wizard
clawq onboard

# Or configure piece by piece with an API key provider
clawq config set providers.openrouter.api_key "sk-..."
clawq config set channels.telegram.accounts.main.bot_token "123:ABC..."
clawq config show                  # review config (secrets redacted)

# Or use a ChatGPT/Codex subscription provider
clawq auth codex-login openai-codex

# Start the daemon
clawq agent

# Open the web UI
xdg-open http://127.0.0.1:13451/

# If gateway pairing is enabled (default), show the current browser pairing code
clawq otp-show

# Common operations
clawq status                       # runtime status
clawq doctor                       # config health check
clawq models                       # list configured providers
clawq channel                      # list active channels
clawq config show security         # inspect a specific config section
```

See the [Quick Start guide](https://clawq.org/quickstart) for a full walkthrough.

## Quick Start

### Prerequisites

- **opam** (OCaml package manager)
- **libsqlite3-dev** (or equivalent for your distro)

### Bootstrap and Build

```bash
# 1. Create opam switch "clawq-5.1" and install all dependencies
make bootstrap

# 2. Build
make build

# 3. Run tests
make test
```

### Getting Started with Telegram

The fastest way to get a running clawq instance is via Telegram:

1. Create a bot with [@BotFather](https://t.me/BotFather) and get your bot token.
2. Either get an API key from an OpenAI-compatible LLM provider, or plan to use a ChatGPT/Codex subscription with the built-in `openai-codex` provider.
3. Run the interactive setup wizard:

```bash
./clawq onboard
```

This launches a full interactive TUI wizard (when run in a terminal) to configure your provider, model, security, channels, gateway, and memory settings. Pipe input or redirect to a non-TTY and it falls back to generating a starter template instead.

4. Edit `~/.clawq/config.json` with your tokens if needed, or use `clawq config set`:

```json
{
  "providers": {
    "openai": {
      "api_key": "sk-...",
      "default_model": "gpt-4o"
    }
  },
  "channels": {
    "telegram": {
      "accounts": {
        "main": {
          "bot_token": "123456:ABC-...",
          "allow_from": ["*"]
        }
      }
    }
  }
}
```

For ChatGPT/Codex subscription auth instead of an API key, add a provider like this and then run `clawq auth codex-login openai-codex`:

```json
{
  "providers": {
    "openai-codex": {
      "kind": "openai-codex",
      "base_url": "https://chatgpt.com/backend-api/codex",
      "default_model": "openai-codex:gpt-5.3-codex"
    }
  }
}
```

`clawq auth codex-login openai-codex` opens a browser and waits for the OAuth callback on `http://localhost:1455/auth/callback`. If the browser cannot reach the local callback, paste the full redirect URL back into the terminal when prompted.

5. Start the daemon:

```bash
./clawq agent
```

Your bot is now live on Telegram. Send it a message to verify.

### Using the Web UI

Once the daemon is running, open `http://127.0.0.1:13451/` in a browser.

- The root gateway route serves the embedded chat UI.
- Assistant replies stream live over SSE.
- Thinking and tool output render in separate panels during a turn.
- Slash commands are available with autocomplete from `/commands`.
- When `gateway.require_pairing` is enabled (the default), the UI prompts for the live 6-digit browser pairing code from `clawq otp-show` before sending chat requests.

For live UI development, run `make ui-dev`, create `~/.clawq/ui/DEV`, and point the daemon at the same `~/.clawq/ui/` directory. In normal mode, clawq extracts versioned embedded assets there automatically.

## CLI Commands

```
clawq active           Show active 5-hour window usage (cost, tokens, quota)
clawq agent            Start the daemon (agent loop, gateway, all configured channels)
clawq audit            View and manage the security audit log
clawq auth             Show provider auth status, run Codex OAuth login, or encrypt secrets
clawq background       Inspect and control background coding worktree tasks
clawq benchmark        Measure tool invocation latency
clawq capabilities     List all active runtime capabilities
clawq channel          List configured channels
clawq completions      Generate shell tab-completion scripts (bash, zsh, fish)
clawq config           Manage configuration (wizard, get/set/show)
clawq costs            Show cumulative LLM costs and token usage
clawq cron             Manage cron jobs for scheduled agent messages
clawq debug            Debug utilities (html-preview, http)
clawq delegate         Queue a high-level background coding handoff
clawq doctor           Check configuration for common issues
clawq manifest         Generate connector command manifests (Teams, Telegram)
clawq mcp              Start the MCP server (Model Context Protocol)
clawq memory           Show memory backend configuration
clawq migrate          Run database migrations
clawq models           List configured LLM providers and their default models
clawq onboard          Interactive setup wizard (or template when not in a TTY)
clawq otp-show         Show the current browser pairing code and Telegram TOTP codes
clawq phase2           Show Phase 2 feature status
clawq plan             Manage multi-stage planning pipelines
clawq provider         Inspect LLM provider configuration and quota state
clawq reset-agent      Wipe all session history, cron jobs, and workspace files
clawq reset-workspace  Reset workspace files without clearing sessions
clawq runtime          Manage native and Docker runtimes
clawq service          Manage the clawq system service (start/stop/restart)
clawq session          Manage agent sessions (list, show, inject, pending, keepalive, heartbeat, epochs, compact)
clawq setup            Interactive setup wizards for individual integrations
clawq skills           Manage agent skills (shell-script tool extensions)
clawq status           Show runtime configuration and daemon status
clawq transcribe       Transcribe an audio file using the configured STT provider
clawq tunnel           Manage a public tunnel to the local gateway (start/stop/status/apply/restart)
clawq update           Request a live daemon update and graceful restart
clawq usage            Show provider quota/usage status
clawq version          Print version and build info
clawq watcher          Manage the error correction watcher
clawq workspace        Print the current workspace directory or manage backups (backup/versions/restore/delete)
```

### Config subcommands

```
clawq config wizard              Full interactive TUI wizard (provider, model, security,
                                 channels, gateway, memory)
clawq config set KEY VALUE       Set a config value by dot-path
                                    e.g. clawq config set security.tools_enabled true
clawq config get KEY             Read a config value by dot-path
                                    e.g. clawq config get providers.openai.default_model
clawq config show [SECTION]      Display current config with secrets redacted
                                     e.g. clawq config show channels
```

### Auth subcommands

```
clawq auth                         Show redacted auth status for configured providers
clawq auth encrypt                 Encrypt plaintext secrets in config
clawq auth codex-login [PROVIDER]  Start ChatGPT/Codex OAuth login flow
clawq auth codex-status [PROVIDER] Show saved Codex OAuth status
clawq auth codex-logout [PROVIDER] Remove saved Codex OAuth credentials
```

Run `clawq COMMAND --help` for per-command usage.

### Background task examples

```bash
# High-level handoff for a background coding run
clawq delegate "implement the feature described in TODO.md"

# Explicit runner/repo/model overrides when needed
clawq delegate --runner codex --model gpt-5.4 --repo /path/to/repo "implement the feature described in TODO.md"

# Low-level queue command for the same flow
clawq background add codex --model gpt-5.4 /path/to/repo "implement the feature described in TODO.md"

# Queue a Claude task on a specific branch
clawq background add claude /path/to/repo --branch clawq-experiment "fix failing tests and summarize the changes"

# Inspect progress
clawq background list
clawq background show 3
clawq background wait 3
clawq background logs 3
clawq background resume 3
clawq background message 3 "please fix the tests before wrapping up"

# Cancel a queued or running task
clawq background cancel 3
```

`clawq delegate` is the friendly entry point for handing off repository work asynchronously. The `background` command family is the lower-level surface for listing tasks, waiting for completion, reading captured logs, resuming a task, sending follow-up chat messages into an existing task, and cancelling work.

The full daemon picks up queued background tasks automatically. Each task gets its own git worktree under `~/.clawq/background-worktrees/` and a log file under `~/.clawq/background-logs/` for later inspection.

Once a worktree-backed task has started, `clawq background resume <id>` uses the runner's native continue/resume support, and `clawq background message <id> ...` durably queues a new user-style chat message that is replayed into the resumed task conversation in FIFO order.

## Make Targets

| Target | Description |
|--------|-------------|
| `make bootstrap` | Create opam switch and install all dependencies |
| `make build` | Build the project |
| `make build-minimal` | Build minimal binary (`clawq-min`, core-only) |
| `make build-opt` | Optimized build (`OPT=speed` or `OPT=size`) |
| `make build-opt-speed` | Optimized build with `-O3` |
| `make build-opt-size` | Optimized build with `-O2 -compact` |
| `make build-opt-speed-stripped` | Stripped optimized speed build |
| `make build-opt-size-stripped` | Stripped optimized size build |
| `make build-opt-minimal` | Optimized minimal binary |
| `make test` | Run quick tests (skips Slow-tagged integration tests) |
| `make test-all` | Run all tests including Slow-tagged integration tests |
| `make test-nocontainer` | Run tests without Docker container |
| `make test-run ARGS="..."` | Run specific test suite/cases (see CLAUDE.md for syntax) |
| `make fmt` | Format code with ocamlformat |
| `make fmt-check` | Check formatting |
| `make extract` | Regenerate OCaml from Coq theories |
| `make extract-check` | Check for extraction drift |
| `make coq-verify` | Check Coq proofs compile |
| `make coq-check` | Verify Coq source consistency |
| `make ui` | Build the web UI and regenerate embedded assets |
| `make ui-dev` | Run the Bun watcher for web UI development |
| `make ui-check` | Verify embedded web UI assets are current |
| `make run` | Print CLI help |
| `make phase2` | Show Phase 2 feature status |
| `make clean` | Clean build artifacts |
| `make docker-build` | Build Docker image |
| `make docker-run` | Run daemon in Docker |
| `make docker-test-image` | Build Docker test isolation image |
| `make dependency-audit` | Audit opam dependencies |
| `make fv-all` | Refresh formal verification stats, data, and badges |
| `make binary-size-report` | Report binary sizes for all build profiles |
| `make binary-size-check` | Check binary sizes against thresholds |
| `make verify-report` | Generate formal verification report and badge |
| `make release` | Build release artifacts |

## Run Daemon in Docker

```bash
# Build image
make docker-build

# Run daemon (foreground)
make docker-run

# Health check
curl http://127.0.0.1:13451/health
```

Direct Docker command:

```bash
docker run -it --rm -p 13451:13451 \
  -e CLAWQ_MASTER_KEY="your-passphrase" \
  clawq:latest agent
```

To persist config/state across restarts:

```bash
docker run -it --rm -p 13451:13451 \
  -v "$HOME/.clawq:/root/.clawq" \
  -e CLAWQ_MASTER_KEY="your-passphrase" \
  clawq:latest agent
```

## Extraction Workflow

```bash
# Regenerate src/extracted/ from Coq theories (requires Coq)
make extract

# Check whether extracted code has drifted from Coq sources
make extract-check
```

## Formal Verification

Core properties are machine-checked in Coq and extracted to OCaml via `coq/theories/Clawq/Extract.v`.

Proof counts, badges, and other FV figures are generated by `make fv-all` from the Coq sources plus `docs/src/data/formal_verification.yml`.

For the current breakdown, see:
- the generated stats in `docs/src/data/fv-stats.json`
- the generated badges in `docs/public/badges/`
- the published verification page at `https://clawq.org/formal-verification/`

```bash
# Refresh proof stats, docs data, and badges
make fv-all
```

## Notes
- The generated extraction file path is `src/extracted/clawq_core.ml`.

When delegation quality matters, prefer `gpt-5.4` for Codex background work. Use `gpt-5.3-codex` when you specifically want the Codex-tuned path or need to mirror Codex OAuth defaults more closely.
