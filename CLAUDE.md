# Clawq -- an AI assistant for work and life

Project name is "Clawq" (note casing)

Agent instructions for this repository. Keep changes minimal, verifiable, and aligned with existing OCaml style.

Subdirectory-specific guidelines exist in `docs/CLAUDE.md`, `src/CLAUDE.md`, and `test/CLAUDE.md`
(loaded automatically when working in those directories).

## Project Snapshot

- Language/toolchain: OCaml 5.1 (`opam` + `dune`), with Coq extraction artifacts in `src/extracted/`.
- Main binary: `clawq` (`src/main.ml`); minimal binary: `clawq-min` (`src/main_min.ml`).
- Runtime split: `clawq_runtime_core` (core CLI) / `clawq_runtime_integrations` (network/server).

## Environment and Setup

- Default shell in `Makefile` runs through: `opam exec --switch=clawq-5.1 -- /usr/bin/env bash`.
- Install deps (local): `opam install . --deps-only --with-test`.
- CI also installs system package: `libsqlite3-dev`.
- If commands fail due to env mismatch, prefix with `opam exec --switch=clawq-5.1 -- <command>`.
- Keep client/network timeouts strictly higher than any connector long-poll timeout, or normal long-poll waits can surface as `Lwt_unix.Timeout`.

## Build, Test, Lint Commands

- Do not run `dune` commands in parallel — Dune locks `_build`; concurrent runs hang or fail on `_build/.lock`.
- Never launch more than one `dune` command at a time in this repo.
- Stale locks are cleaned automatically by `scripts/clean_stale_dune_locks.sh`. If a lock error persists, the lock is actively held — wait for or stop the owning process.

- Primary build: `make build`
- Run CLI help: `make run`
- Run phase2 command: `make phase2`
- Build embedded web UI assets: `make ui` (live iteration: `make ui-dev` + create `~/.clawq/ui/DEV`)
- Verify generated UI assets: `make ui-check`

- Run quick tests: `make test` (skips `Slow`-tagged integration tests)
- Run all tests: `make test-all` (builds `main.exe` first)
- Run runner integration tests: `make test-run ARGS="test runner_integration"`
  - Requires runner binaries in PATH (codex, claude, kimi, gemini, opencode, cursor-agent)
  - Tests skip gracefully if a runner binary is not available
  - Fresh invocation tests (Tier 2) also need runner auth configured; skip on auth errors

- Format: `make fmt` / `make fmt-check`

- Coq: `make extract`, `make extract-check`, `make coq-verify`, `make coq-check`

## Running a Single Test (Important)

- List all: `make test-run ARGS="list"`
- By suite regex: `make test-run ARGS="test <SUITE_REGEX>"`
- By case index: `make test-run ARGS="test command_bridge 20"`
- Multiple indices/ranges: `make test-run ARGS="test scheduler 0,3,8-10"`
- The test binary subcommand is `test` (not a direct suite name). Regex applies to suite names; case selection is numeric indexes.

## Optimization Commands

- Optimized builds: `make build-opt-speed`, `make build-opt-size`, `make build-opt-speed-stripped`, `make build-opt-size-stripped`
- Minimal builds: `make build-minimal`, `make build-opt-minimal`
- Build profiles in `dune-workspace`: `release-speed` (`-O3`), `release-size` (`-O2 -compact`)
- Output contract: optimization targets must end with one line: `<path/to/exe> <size_kb> KB`

## File Size Guidelines

- Ideal: under 1000 lines. Hard limit: 2000 lines.
- Split into focused sub-modules by concern (e.g., `foo_util.ml`, `foo_core.ml`, `foo.ml`).
- Re-export via `include Sub_module` (preferred) or explicit `let f = Sub_module.f` aliases.

## Code Style Guidelines

- Formatter: `ocamlformat` v0.28.1, profile `default`. Never fight formatter output.
- Prefer minimal `open`; use fully qualified modules where reasonable.
- Command handlers: `cmd_<name>` pattern. Values/functions: `snake_case`.
- Test names: concise behavior phrases (Alcotest case names).
- Prefer explicit records/algebraic types over ad-hoc tuples for complex values.
- Use `option`/`result` for expected failures; reserve exceptions for I/O boundaries.
- Return user-facing error strings in command bridge paths. Keep errors actionable.
- Preserve existing behavior unless task explicitly requests semantic changes.
- Comments only for non-obvious invariants/protocol details.

## Testing Expectations for Code Changes

- Minimum after non-trivial OCaml edits: `make test`.
- After formatting-sensitive edits: `make fmt-check`.
- After extraction-related edits: `make extract-check`.
- After runtime/library reshaping: full tests + at least one optimized build.

## Runtime Split Rules (Do Not Regress)

1. Keep optional integrations out of `clawq_runtime_core` unless strictly required.
2. New network/server features belong in `clawq_runtime_integrations`.
3. Integration-only commands: return "disabled in minimal build" in `src/command_bridge_min.ml`.
4. Evaluate new dependencies for core vs integration placement before linking.

## Safety and Change Boundaries

- Do not commit generated drift accidentally (especially extraction outputs) without intent.
- Do not delete or revert unrelated user changes in a dirty working tree.
- Prefer additive, targeted edits over broad refactors unless requested.
- Keep Makefile target behavior stable when extending command surface.
- Keep app state in sync with config state: when config is updated at runtime (daemon file watcher, SIGHUP, `config set`), any in-memory state derived from config must be refreshed. Do not rely on restart for config changes to take effect.

## Proactive Completion

- Do not stop at the narrowest interpretation if adjacent behavior is clearly required.
- Follow real production paths end-to-end rather than adding debug-only shortcuts.
- If a task uncovers an obvious missing piece, fix it in the same change when safe and local.
- Prefer fully completed behavior plus tests over partial scaffolding.
- In handoff, call out deliberate gaps; do not silently leave known functional mismatches.

## Model Format Convention

- Canonical format: `provider:model` (colon), e.g. `openai:gpt-5.4`. Default: `"openai-codex:gpt-5.4"`.
- Legacy `provider/model` (slash) and bare `model` are accepted but deprecated; warnings shown at config load/status/set.
- `models set-default` auto-normalizes to canonical. See `src/pmodel.ml` for parsing API.

## Quick File Map

- CLI entrypoints: `src/main.ml`, `src/main_min.ml`
- Command routing: `src/command_bridge.ml`, `src/command_bridge_min.ml`
- Process spawning: `src/process_group.ml` (fork+setsid+execve, signal group lifecycle)
- Runner framework: `src/runner_framework.ml` (session ID strategies, per-runner command generation)
- Web UI: `src/ui_server.ml`, `src/chat_ui_assets.ml`, `ui/`, `scripts/gen_chat_ui_assets.sh`
- Structured pipelines: `src/structured_pipeline.ml` (types, parsing, DB, builtins), `src/structured_pipeline_schema.ml` (JSON Schema validator), `src/structured_pipeline_run.ml` (execution engine)
- Build config: `dune-project`, `dune-workspace`, `src/dune`, `test/dune`
- Tests: `test/test_main.ml` and `test/test_*.ml`

## Agent Templates

- Templates: `src/agent_template.ml` (types, parsing, discovery), `src/agent_template_builtins.ml` (builtin registry), and generated `src/agent_template_builtins_*.ml` groups from `docs/builtin-agent-prompts/`
- CLI: `clawq agents <list|show|create|edit|delete|bind|unbind|bindings|setup|path>`
- When adding new built-in tools, review `docs/builtin-agent-prompts/*.md` and update each agent's `allowed_tools` / `disallowed_tools` lists as appropriate, then run `make gen-agents`.

## Misc Notes

- When implementing or updating features, have a background subagent check `docs/*` to see if anything requires updating. Claude code agents: Use haiku for the model when creating the agent task.
- **Dune memory usage:** A dependency cycle in the module graph (e.g. `task_tree_ops → slash_commands → ... → task_tree_ops`) causes dune/ocamldep to loop and consume 20-30+ GB of RAM until OOM. If dune uses >2GB, suspect a cycle — check with `dune build 2>&1 | grep "Dependency cycle"`. The Makefile sets `OCAMLRUNPARAM=o=120,O=120` to cap OCaml GC memory, but cycles bypass this. Fix by breaking the cycle (e.g. pass `~title:string` instead of a high-level type).

## Recommended Agent Workflow

1. Read relevant modules and adjacent tests first.
2. Implement the smallest change that preserves real runtime semantics.
3. Use the todo tool to track current tasks when it helps maintain progress.
4. Run focused test(s), then `make test`.
5. Run formatting checks if OCaml files changed.
6. Before handoff, check for obvious follow-on fixes/docs/tests.
7. Summarize behavior changes and verification commands in final handoff.

## Codebase

File limit: soft limit at 1000 LoC, hard limit at 2k LoC. Proactively take refactoring opportunities to avoid large files.
