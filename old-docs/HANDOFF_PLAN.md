# Handoff Plan: Coq Port of nullclaw (`clawq`)

## 1) Goal and Success Criteria
- Build a Coq-first port of `nullclaw` with practical runtime execution via Coq extraction to OCaml.
- Keep high compatibility with nullclaw/OpenClaw config/workspace conventions.
- Deliver an MVP with:
  - Core CLI command skeleton and dispatch architecture.
  - Channel framework with `web` and `telegram` as first adapters.
  - Hybrid memory direction with SQLite FTS + local vector index.
  - Safe core tools (file R/W/edit, controlled shell, HTTP).
  - Security baseline (workspace scoping, secret encryption, audit trail).
  - Skills + MCP integration path.
  - Scheduler (cron/once + run history).
  - Service lifecycle and OpenClaw migration commands.
  - Voice path.
  - Runtime paths for native + docker + wasm.
  - Tunnel interface with Cloudflare provider.

## 2) Locked Product Decisions
- MVP:
  - Core CLI: yes.
  - Channel framework: yes.
  - Initial channel adapters: web + telegram.
  - Memory: full hybrid direction, first store target SQLite FTS + local vector index.
  - Tools: safe core tools only.
  - Security: strong baseline.
  - Cron scheduler: yes.
  - Skills + MCP: yes.
  - Runtime targets: native + docker + wasm.
  - Voice: yes.
  - Service + migration: yes.
  - Provider strategy: OpenAI-compatible abstraction first.
  - Tunnel: interface + Cloudflare in MVP.
  - Compatibility: high compatibility with nullclaw/OpenClaw schema/workspace.
- Toolchain and execution:
  - Coq 8.19 + dune + opam.
  - Coq extraction to OCaml + dune executable.
  - Dependency policy: max leverage.
  - Practical executable first.

## 3) Explicitly Deferred to Phase 2
1. Gateway API and pairing-token flow (`/health`, `/pair`, `/webhook`).
2. Subagent/delegation orchestration manager.
3. Hardware peripheral integrations.
4. Self-update command surface.
5. Additional tunnel providers beyond Cloudflare.
6. Additional channel adapters beyond web/telegram.
7. Additional native providers beyond OpenAI-compatible baseline.

## 4) Current Repository Status
### 4.1 Files added/updated
- Added:
  - `PLAN.md` (agreed product plan and deferred list).
  - `HANDOFF_PLAN.md` (this file).
  - `dune-project`
  - `clawq.opam`
  - `coq/theories/dune`
  - `coq/theories/Clawq/Interfaces.v`
  - `coq/theories/Clawq/Config.v`
  - `coq/theories/Clawq/Cli.v`
  - `coq/theories/Clawq/Extract.v`
  - `src/dune`
  - `src/main.ml`
  - `src/command_bridge.ml`
  - `src/phase2.ml`
  - `src/extracted/dune`
  - `src/extracted/clawq_core.ml` (placeholder to be overwritten by extraction)
  - `scripts/bootstrap_coq.sh`
  - `scripts/extract.sh`
- Updated:
  - `.gitignore` (build artifacts and Coq outputs).

### 4.2 Environment facts discovered
- `opam`, `coqc`, `dune`, `ocaml` are currently missing on the machine.
- Repo root is lightweight; `nullclaw/` exists as vendored reference code and is ignored by `.gitignore`.

## 5) Immediate Next Steps (Execution Order)
1. Mark scripts executable:
   - `chmod +x scripts/bootstrap_coq.sh scripts/extract.sh`
2. Install system prerequisites (requires elevated/network permissions):
   - Preferred on Arch-like systems:
     - `sudo pacman -S --needed --noconfirm opam ocaml dune coq sqlite`
   - If distro differs, use equivalent package manager and ensure:
     - `opam`, `coqc`, `dune`, `ocaml` are on `PATH`.
3. Bootstrap opam switch and packages:
   - `./scripts/bootstrap_coq.sh`
4. Verify toolchain:
   - `opam --version`
   - `ocaml -version`
   - `coqc --version`
   - `dune --version`
5. Build baseline:
   - `eval "$(opam env --switch=clawq-5.1)"`
   - `dune build`
6. Run extraction and rebuild:
   - `./scripts/extract.sh`
   - `dune build`
7. Smoke test CLI:
   - `dune exec clawq -- help`
   - `dune exec clawq -- phase2`
   - `dune exec clawq -- version`

## 6) Implementation Backlog (Decision-Complete)
### Phase A: Solidify foundation
1. Replace placeholder extracted module flow with reliable generated artifact workflow:
   - Decide and enforce one path:
     - Generated file tracked in VCS, or
     - Generated in build dir and vendored via dune rule.
2. Add module map and architecture docs for:
   - provider/channel/tool/memory/runtime/tunnel/security interfaces.
3. Add CI workflow:
   - Build, extraction check, unit tests, formatting checks.

### Phase B: Core CLI and config compatibility
1. Implement parser/validator for nullclaw-compatible config schema:
   - `models.providers`
   - `agents.defaults.model.primary`
   - `channels.*.accounts`
   - `memory`
   - `security`
   - `runtime`
   - `tunnel`
2. Implement command groups with stable CLI contract:
   - `onboard`, `agent`, `status`, `doctor`, `cron`, `channel`, `skills`,
     `migrate`, `service`, `models`, `memory`, `workspace`, `capabilities`, `auth`.
3. Add compatibility fixtures from `nullclaw/config.example.json`.

### Phase C: MVP runtime capabilities
1. Agent loop + session handling + tool dispatch.
2. Safe core tools:
   - file read/write/edit with workspace confinement
   - shell allowlist execution
   - HTTP request/search abstraction
3. Memory:
   - SQLite primary storage
   - FTS retrieval
   - local vector index and merge strategy
4. Security baseline:
   - secret encryption
   - workspace-only policy checks
   - audit event sink and retention hooks
5. Channels:
   - framework contracts
   - `web` and `telegram` adapters
6. Skills and MCP hooks.
7. Cron scheduler with run history persistence.
8. Service command handling + OpenClaw migration path.
9. Voice path integration.

### Phase D: Runtime + tunnel
1. Native runtime adapter.
2. Docker runtime adapter.
3. WASM runtime path.
4. Tunnel interface + Cloudflare implementation.

### Phase E: Hardening and release
1. Reliability policies (timeouts/retries/fallback/degrade behavior).
2. Perf and memory baseline tests.
3. Packaging and operator docs.
4. Phase 2 deferred backlog prep.

## 7) Test Plan
1. Unit tests:
   - command parsing/dispatch
   - config validation
   - memory scoring and merge behavior
2. Integration tests:
   - command invocation and expected output shape
   - migration dry-run/apply
   - cron schedule and run history
3. Security tests:
   - path traversal rejection
   - command allowlist enforcement
   - audit log emission
4. Adapter tests:
   - channel contract compliance for web/telegram
   - runtime adapter startup/health
   - tunnel Cloudflare lifecycle
5. Extraction tests:
   - extraction succeeds
   - extracted module compiles with runtime

## 8) Risks and Mitigations
- Risk: Coq extraction artifacts drift from runtime assumptions.
  - Mitigation: CI check that re-runs extraction and verifies no diff.
- Risk: dependency availability/version skew in opam ecosystem.
  - Mitigation: pin switch/package versions and document lock strategy.
- Risk: strict compatibility increases initial complexity.
  - Mitigation: stage by schema sections with fixtures per section.
- Risk: broad MVP scope.
  - Mitigation: enforce milestone slices with acceptance gates before next slice.

## 9) Acceptance Gates
1. Gate 1 (Bootstrap):
   - Fresh machine can run bootstrap and `dune build`.
2. Gate 2 (CLI/config):
   - All MVP commands parse and return structured status.
   - Config compatibility fixtures pass.
3. Gate 3 (runtime core):
   - Agent loop + safe tools + memory + security baseline functional.
4. Gate 4 (connectivity):
   - web/telegram channels functional in local smoke tests.
   - skills/MCP hooks exercised.
5. Gate 5 (ops/runtime):
   - cron/service/migration + native/docker/wasm + tunnel(Cloudflare) path validated.

## 10) Notes for Next Agent
- Treat `PLAN.md` as product scope source of truth and keep `Deferred to Phase 2` synchronized.
- Preserve non-destructive workflow; do not modify `nullclaw/` reference contents.
- Before significant edits, run quick local checks (`dune build`) once toolchain is installed.
