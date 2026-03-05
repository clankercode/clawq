# clawq Architecture

## Build Pipeline

```
coq/theories/Clawq/*.v    Coq source theories
        |
        v
  [coqc + Extraction]     scripts/extract.sh
        |
        v
src/extracted/clawq_core.{ml,mli}  Generated OCaml (tracked in git)
        |
        v
  clawq_extracted library      src/extracted/dune
        |
        v
  clawq_runtime library        src/dune (command_bridge, phase2)
        |
        v
    clawq executable            src/dune (main.ml + cmdliner)
```

## Module Map

### Coq Theories (`coq/theories/Clawq/`)

| File | Role |
|------|------|
| `Interfaces.v` | 7 record-based interface definitions (Provider, Channel, Tool, Memory, RuntimeAdapter, Tunnel, Security) |
| `Config.v` | Configuration records (GatewayConfig, MemoryConfig, SecurityConfig, ClawqConfig) with defaults and validation |
| `Cli.v` | Command ADT, `parse_command`, `dispatch`, and `usage` string |
| `Extract.v` | Extraction directives: type mappings (ExtrOcamlBasic, ExtrOcamlNativeString, ExtrOcamlNatInt) and function list |

### Extracted OCaml (`src/extracted/`)

| File | Role |
|------|------|
| `clawq_core.ml` | Auto-generated OCaml from Coq extraction. Contains command parsing, dispatch, config validation. Tracked in git so the project builds without Coq installed. |
| `clawq_core.mli` | Auto-generated interface file from Coq extraction. Tracked alongside the `.ml`. |

### Runtime (`src/`)

| File | Role |
|------|------|
| `main.ml` | Entry point; uses Cmdliner for CLI argument parsing |
| `command_bridge.ml` | Bridges CLI arguments to extracted Coq dispatch; handles runtime-only commands (e.g., `phase2`) |
| `runtime_config.ml` | Runtime configuration: loading, validation, and defaults for all subsystems |
| `config_loader.ml` | File-based config loading: reads JSON config, merges with env vars |
| `agent.ml` | Agent loop: prompt assembly, provider calls, tool dispatch, conversation management |
| `session.ml` | Session lifecycle: create, resume, persist conversation state |
| `provider.ml` | LLM provider abstraction: OpenAI-compatible HTTP calls, streaming, model selection |
| `memory.ml` | Key-value memory backend: SQLite-backed store/recall/forget with namespace support |
| `vector.ml` | Local vector index: embedding storage in SQLite, cosine similarity, OpenAI-compatible embeddings API client, hybrid FTS+vector merge strategy |
| `tool.ml` | Tool type definitions and invocation framework with risk-level enforcement |
| `tool_registry.ml` | Dynamic tool registration: register, lookup, list tools by name or category |
| `tools_builtin.ml` | Built-in tool implementations: file I/O, shell exec, web fetch, search |
| `mcp_server.ml` | MCP server: exposes registered tools over JSON-RPC; configurable tool filtering |
| `skills.ml` | Skill loader: discovers and loads skill definitions from the filesystem |
| `http_server.ml` | HTTP server: Cohttp-lwt endpoint for web channel and health checks |
| `http_client.ml` | HTTP client: shared Cohttp-lwt client for provider and API calls |
| `telegram.ml` | Telegram channel: bot API integration for send/receive via long polling |
| `daemon.ml` | Daemonisation: fork, PID file, signal handling for background operation |
| `service.ml` | Service orchestrator: starts/stops subsystems (server, tunnel, scheduler) |
| `scheduler.ml` | Scheduled tasks: cron-like recurring job execution |
| `audit.ml` | Audit logging: append-only log of tool invocations and security events |
| `secret_store.ml` | Secret encryption at rest: AES-256-GCM via mirage-crypto, PBKDF2 key derivation from CLAWQ_MASTER_KEY env var, `$ENC:` prefix format |
| `migrate.ml` | Database migrations: versioned schema upgrades for SQLite stores |
| `resilience.ml` | Reliability policies: with_timeout, with_retry (exponential backoff), with_fallback, with_timeout_retry |
| `runtime_native.ml` | Native runtime adapter: wraps daemon/service for start/stop/status/health |
| `runtime_docker.ml` | Docker runtime adapter: manages clawq in Docker containers via docker CLI |
| `tunnel_cloudflare.ml` | Cloudflare tunnel: manages cloudflared process, extracts `*.trycloudflare.com` URL |
| `stt.ml` | Speech-to-text: audio transcription via Whisper-compatible API |
| `phase2.ml` | Lists features deferred to Phase 2 |

## Dune Libraries

| Library | Modules | Dependencies |
|---------|---------|-------------|
| `clawq_extracted` | `clawq_core` | (none) — unwrapped, `-w -39` for extraction artifacts |
| `clawq_runtime` | `command_bridge`, `runtime_config`, `config_loader`, `agent`, `session`, `provider`, `memory`, `vector`, `tool`, `tool_registry`, `tools_builtin`, `mcp_server`, `skills`, `http_server`, `http_client`, `telegram`, `daemon`, `service`, `scheduler`, `audit`, `secret_store`, `migrate`, `resilience`, `runtime_native`, `runtime_docker`, `tunnel_cloudflare`, `stt`, `phase2` | `yojson`, `sqlite3`, `lwt`, `cohttp-lwt-unix`, `conduit-lwt-unix`, `tls-lwt`, `logs`, `fmt`, `mirage-crypto`, `mirage-crypto-rng`, `mirage-crypto-rng.unix`, `kdf.pbkdf`, `digestif.c`, `base64`, `clawq_extracted` — unwrapped |
| `clawq` (executable) | `main` | `clawq_runtime`, `cmdliner` |

Both libraries use `(wrapped false)` so modules are accessible directly (e.g., `Clawq_core.dispatch` rather than `Clawq_extracted.Clawq_core.dispatch`). The extracted library also suppresses warning 39 (`-w -39`) since Coq extraction sometimes emits unnecessary `rec` flags.

## Interface Inventory

From `Interfaces.v`, these 7 records define the contract surface for future implementations:

| Interface | Fields | Purpose |
|-----------|--------|---------|
| `Provider` | name, complete, health | LLM provider abstraction |
| `Channel` | name, start, stop, send | Communication channel (web, telegram, etc.) |
| `Tool` | name, invoke, risk_level | Agent tool with risk classification |
| `Memory` | store, recall, forget | Key-value memory backend |
| `RuntimeAdapter` | name, start, stop | Runtime lifecycle management |
| `Tunnel` | name, start, status | Network tunnel (e.g., Cloudflare) |
| `Security` | workspace_only, audit_enabled, encrypt_secrets | Security policy flags |

## Dependency Direction

```
Interfaces.v  (no deps)
     |
     v
  Config.v    (depends on String, List, Bool)
     |
     v
   Cli.v      (depends on String, List)
     |
     v
 Extract.v    (depends on Cli, Config, ExtrOcaml*)
```

OCaml side:
```
clawq_extracted  -->  clawq_runtime  -->  clawq (executable)
   (no deps)        (yojson, sqlite3,       (cmdliner)
                     lwt, cohttp-lwt-unix,
                     conduit-lwt-unix,
                     tls-lwt, logs, fmt,
                     mirage-crypto,
                     mirage-crypto-rng,
                     kdf.pbkdf,
                     digestif.c, base64)
```

## Build Commands

| Command | What It Does |
|---------|-------------|
| `make bootstrap` | Create opam switch, install all dependencies |
| `make build` | `dune build` |
| `make extract` | Run Coq extraction via `scripts/extract.sh` |
| `make extract-check` | Verify extracted code matches what extraction produces |
| `make test` | `dune runtest` |
| `make run` | `dune exec clawq -- help` |
| `make phase2` | `dune exec clawq -- phase2` |
| `make fmt` | `dune fmt` |
| `make fmt-check` | Check formatting without modifying files |
| `make clean` | `dune clean` |
| `make release` | `dune build --release` |
| `make docker-build` | Build Docker image |
| `make docker-run` | Run clawq in Docker container |

## New in Phase C-E

- **Vector search** — Hybrid FTS + embeddings with configurable weights. Embeddings are stored in SQLite alongside FTS indices; cosine similarity is computed locally and results are merged via a weighted scoring strategy.
- **Secret encryption** — AES-256-GCM encryption at rest using mirage-crypto. Keys are derived via PBKDF2 from the `CLAWQ_MASTER_KEY` environment variable. Encrypted values use a `$ENC:` prefix for transparent detection.
- **Runtime adapters** — Two adapters implementing the `RuntimeAdapter` interface: `runtime_native` (wraps local daemon/service lifecycle) and `runtime_docker` (manages clawq inside Docker containers via the docker CLI).
- **Tunnel** — Cloudflare tunnel support via `cloudflared`. Automatically extracts the assigned `*.trycloudflare.com` URL from process output.
- **Resilience** — Reliability policies for LLM calls: `with_timeout`, `with_retry` (exponential backoff with jitter), `with_fallback`, and the combined `with_timeout_retry`.
- **MCP** — The MCP server now exposes all registered tools. Tool exposure is configurable via `mcp.enabled` and `mcp.exposed_tools` in the config.
- **Packaging** — Dockerfile added for containerized deployment (`make docker-build` / `make docker-run`).
