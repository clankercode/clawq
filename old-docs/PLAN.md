# Coq Port of nullclaw: Implementation Plan

## Summary
- Build a Coq-first port of nullclaw with practical execution via Coq extraction to OCaml.
- Start with a runnable, compatible core (CLI/runtime architecture, memory, tools, security), then expand to deferred areas.
- Keep high compatibility with nullclaw/OpenClaw config/workspace structures.

## Decisions Captured
- Core CLI (`onboard`, `agent`, config loading/validation): MVP
- Gateway API (`/health`, `/pair`, `/webhook`): Phase 2
- Multi-channel framework: MVP
- Initial channels beyond CLI: `web` + `telegram`
- Memory: full hybrid direction; MVP storage = SQLite FTS + local vector index
- Tools: safe core tools in MVP (file read/write/edit, controlled shell, HTTP)
- Security: strong baseline in MVP (workspace scoping, secret encryption, audit trail)
- Scheduler/cron: MVP
- Skills + MCP: MVP
- Subagent/delegation orchestration: Phase 2
- Runtime targets: native + docker + wasm path in MVP
- Voice: MVP
- Hardware peripherals: Phase 2
- Service lifecycle commands: MVP
- OpenClaw migration command: MVP
- Self-update: Phase 2
- Provider scope: OpenAI-compatible abstraction first (MVP)
- Tunnel scope: MVP interface + Cloudflare provider
- Compatibility: high config/workspace compatibility in MVP
- Toolchain: `opam + dune + Coq 8.19`
- Execution path: Coq extraction to OCaml + dune executable
- Dependency policy: max leverage
- Product emphasis: practical executable first

## Phases

### Phase 0: Repository Bootstrap
1. Initialize Coq project (`opam`, `dune`, Coq 8.19).
2. Set up extraction pipeline (Coq -> OCaml -> executable).
3. Install and pin dependencies.
4. Add CI for build, extraction, tests.

### Phase 1: Core Architecture and Compatibility
1. Implement config schema/parser compatible with nullclaw/OpenClaw structure.
2. Implement command routing and MVP CLI skeleton.
3. Define core interfaces for provider/channel/tool/memory/runtime/tunnel/security.
4. Add workspace bootstrap/migration scaffolding.

### Phase 2: MVP Runtime Features
1. Agent loop with session state and tool dispatch.
2. Safe core tools and risk controls.
3. Hybrid memory path (SQLite FTS + local vector index + merge).
4. Security baseline (secrets, workspace policy, audit).
5. Channel framework + `web` and `telegram` adapters.
6. Skills and MCP integration.
7. Cron scheduling and run history.
8. Service lifecycle and OpenClaw migration.
9. Voice path.

### Phase 3: Runtime and Tunnel
1. Native runtime path.
2. Docker runtime adapter.
3. WASM runtime path.
4. Tunnel interface + Cloudflare implementation.

### Phase 4: Hardening and Release
1. Reliability behavior (timeouts/retries/degradation).
2. Performance and memory baseline checks.
3. Packaging and release docs.
4. Prepare and prioritize Phase 2 backlog execution.

## Planned Public Interfaces
1. `Config.t` + validation for nullclaw-like keys and sections.
2. `Provider` interface (`complete`, `stream?`, `health`, `capabilities`).
3. `Channel` interface (`start`, `stop`, `receive`, `send`, `health`).
4. `Tool` interface (`name`, `schema`, `invoke`, risk metadata).
5. `Memory` interface (`store`, `recall`, `search`, `forget`, `stats`).
6. `RuntimeAdapter` interface (native/docker/wasm execution contract).
7. `Tunnel` interface (`start`, `stop`, `status`, endpoint metadata).
8. `Security` interface (secret store, audit sink, policy checks).

## Test Scenarios
1. Config compatibility fixtures and validation tests.
2. CLI integration tests for MVP command groups.
3. Memory correctness tests (store/recall/search ranking/merge).
4. Tool security tests (path traversal, allowlists, audit emission).
5. Channel adapter contract tests (`web`, `telegram`).
6. Cron scheduling and run-history tests.
7. Migration tests (dry-run and apply).
8. Extraction build test (Coq -> OCaml executable).
9. Runtime adapter tests (native/docker/wasm startup + health).
10. Tunnel lifecycle tests (Cloudflare adapter).

## Deferred to Phase 2
1. Gateway API and pairing-token flow (`/health`, `/pair`, `/webhook`).
2. Subagent/delegation orchestration manager.
3. Hardware peripheral integrations.
4. Self-update command surface.
5. Tunnel providers beyond Cloudflare.
6. Channel adapters beyond `web` and `telegram`.
7. Additional native providers beyond OpenAI-compatible baseline.
