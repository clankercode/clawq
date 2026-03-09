# CLAUDE.md

Agent instructions for this repository. Keep changes minimal, verifiable, and aligned with existing OCaml style.

## Project Snapshot

- Language/toolchain: OCaml 5.1 (`opam` + `dune`), with Coq extraction artifacts in `src/extracted/`.
- Main binary: `clawq` (`src/main.ml`).
- Minimal binary: `clawq-min` (`src/main_min.ml`) for core-only builds.
- Runtime split:
  - `clawq_runtime_core` (core CLI/runtime behavior).
  - `clawq_runtime_integrations` (network/server integrations).

## Instruction Sources

- Cursor rules: none found (`.cursor/rules/` missing, `.cursorrules` missing).
- Copilot rules: none found (`.github/copilot-instructions.md` missing).
- Therefore this file is the canonical agent guidance in-repo.

## Environment and Setup

- Default shell in `Makefile` runs through: `opam exec --switch=clawq-5.1 -- /usr/bin/env bash`.
- Install deps (local): `opam install . --deps-only --with-test`.
- CI also installs system package: `libsqlite3-dev`.
- If commands fail due to env mismatch, prefix with:
  - `opam exec --switch=clawq-5.1 -- <command>`

## Build, Test, Lint Commands

- Do not run `dune` commands in parallel in this repo. Dune locks `_build`, and concurrent `dune`/`opam exec -- dune ...` commands can hang or fail on `_build/.lock`.
- Stale locks are cleaned automatically by `scripts/clean_stale_dune_locks.sh` (called from the `check_dune_lock` macro before each build/test target). The script uses `flock --nonblock` to detect whether a lock is genuinely held; only unheld locks are removed.
- If a command fails with a Dune lock error, the lock is actively held by another process. Wait for it to finish or find and stop the owning process.

- Primary build: `make build`
- Direct dune build: `dune build`
- Run CLI help: `make run`
- Run phase2 command: `make phase2`
- Build embedded web UI assets: `make ui`
- Run UI asset watcher for dev mode: `make ui-dev`
- Verify generated UI assets are current: `make ui-check`

- Run all tests: `make test`
- Direct test run: `dune runtest`

- Format code: `make fmt`
- Format check: `make fmt-check`
- CI-equivalent format gate: `dune fmt && git diff --exit-code`

- Coq extraction refresh: `make extract`
- Coq extraction drift check: `make extract-check`
- Verify Coq proofs only: `make coq-verify`
- Full Coq check (proofs + drift): `make coq-check`

## Running a Single Test (Important)

- List all test suites/cases:
  - `opam exec --switch=clawq-5.1 -- dune exec test/test_main.exe -- list`
- Run subset by suite regex:
  - `opam exec --switch=clawq-5.1 -- dune exec test/test_main.exe -- test <SUITE_REGEX>`
- Run one exact case index in a suite:
  - `opam exec --switch=clawq-5.1 -- dune exec test/test_main.exe -- test command_bridge 20`
- Run multiple indices/ranges:
  - `... -- test scheduler 0,3,8-10`

Notes:
- The test binary subcommand is `test` (not a direct suite name).
- Regex applies to suite names; case selection is numeric indexes from `list` output.

## Optimization and Binary Size Commands

- Optimized full build (choose mode): `make build-opt OPT=speed|size`
- Explicit optimized full builds:
  - `make build-opt-speed`
  - `make build-opt-size`
- Stripped optimized full builds:
  - `make build-opt-speed-stripped`
  - `make build-opt-size-stripped`
- Minimal builds:
  - `make build-minimal`
  - `make build-opt-minimal`

Required output contract:
- Optimization targets must end with one line: `<relative/path/to/exe> <size_kb> KB`
- Example: `_build_opt_size/default/src/main.exe 19434 KB`

## Build Profiles

- Defined in `dune-workspace`:
  - `release-speed`: `-O3`
  - `release-size`: `-O2 -compact`

## Code Style Guidelines

Formatting:
- Use `ocamlformat` (`.ocamlformat`: version `0.28.1`, profile `default`).
- Never hand-format to fight formatter output.
- Keep lines and layout formatter-friendly.

Imports and module usage:
- Prefer minimal `open`; use fully qualified modules where reasonable.
- `open` at top is acceptable for narrow, obvious cases (for example `open Cmdliner`).
- Avoid broad `open` chains that hide symbol origin.

Naming:
- Modules: `PascalCase` filenames (`runtime_config.ml` module `Runtime_config`).
- Values/functions: `snake_case`.
- Test names: concise behavior phrases (as seen in Alcotest case names).
- Use clear command handler names: `cmd_<name>` pattern is established.

Types and data modeling:
- Prefer explicit records and algebraic types over ad-hoc tuples for complex values.
- Add type annotations where they improve readability in callbacks/pattern matches.
- Use `option`/`result` for expected failure paths; reserve exceptions for exceptional boundaries.

Error handling:
- Return user-facing error strings in command bridge paths.
- Use `try ... with` around I/O and external boundaries; avoid exception-driven core flow.
- Keep error messages actionable and specific.

Control flow:
- Prefer small helper functions for command-specific behavior.
- Keep match branches direct and readable.
- Preserve existing behavior unless task explicitly requests semantic changes.

Comments:
- Add comments only for non-obvious invariants/protocol details.
- Do not add explanatory noise for straightforward code.

## Testing Expectations for Code Changes

- Minimum after non-trivial OCaml edits: `make test`.
- After formatting-sensitive edits: `make fmt-check`.
- After extraction-related edits: `make extract-check`.
- After runtime/library reshaping: run full tests and at least one optimized build command.

## Runtime Split Rules (Do Not Regress)

1. Keep optional integrations out of `clawq_runtime_core` unless strictly required.
2. New network/server features belong in `clawq_runtime_integrations`.
3. Integration-only commands must not be exposed in minimal build as active behavior.
4. In `src/command_bridge_min.ml`, return clear "disabled in minimal build" messages.
5. Evaluate new dependencies for core vs integration placement before linking.

## Safety and Change Boundaries

- Do not commit generated drift accidentally (especially extraction outputs) without intent.
- Do not delete or revert unrelated user changes in a dirty working tree.
- Prefer additive, targeted edits over broad refactors unless requested.
- Keep Makefile target behavior stable when extending command surface.

## Proactive Completion

- Do not stop at the narrowest possible interpretation if adjacent behavior is clearly required for the feature to be genuinely usable.
- When a requested command or feature should mirror an existing runtime path, follow the real production path end-to-end rather than adding a debug-only shortcut that bypasses important semantics.
- If a task uncovers an obvious missing piece, fix it in the same change when it is safe, local, and verifiable.
- Prefer fully completed behavior plus tests over partial scaffolding, even when the user asked in shorthand.
- In handoff, call out any deliberate gaps that remain; do not silently leave known functional mismatches.

## Quick File Map

- Build/test orchestration: `Makefile`
- UI source and Bun pipeline: `ui/`, `scripts/gen_chat_ui_assets.sh`
- Dune project config: `dune-project`, `dune-workspace`, `src/dune`, `test/dune`
- CLI entrypoints: `src/main.ml`, `src/main_min.ml`
- Process spawning: `src/process_group.ml` (fork+setsid+execve, signal group lifecycle)
- Command routing: `src/command_bridge.ml`, `src/command_bridge_min.ml`
- Web UI serving/assets: `src/ui_server.ml`, `src/chat_ui_assets.ml`
- Tests: `test/test_main.ml` and `test/test_*.ml`

## Web UI Dev Mode

- Build embedded assets with `make ui`.
- For live UI iteration, run `make ui-dev` and create `~/.clawq/ui/DEV`.
- With DEV mode enabled, clawq serves files from `~/.clawq/ui/` without overwriting them on startup.

## Recommended Agent Workflow

1. Read relevant modules and adjacent tests first.
2. Implement the smallest change that still preserves the real runtime semantics users would expect.
3. Use the todo tool to keep track of current tasks when it helps maintain progress.
4. Run focused test(s), then `make test`.
5. Run formatting checks if OCaml files changed.
6. Before handoff, quickly check for obvious follow-on fixes/docs/tests needed to make the task feel complete.
7. Summarize behavior changes and verification commands in final handoff.

## Formal Verification Docs Maintenance

Data pipeline: `coq/theories/Clawq/*.v` → `docs/src/data/formal_verification.yml` → `docs/src/data/fv-stats.json` → `docs/src/content/docs/formal-verification.mdx`.

**Automated by `make update-fv`** (runs `scripts/update_fv_data.sh`):
- Theorem/lemma counts in YAML (grepped from `.v` files)
- All derived stats in JSON (totals, percentages, verified/in-progress/planned counts)
- Validation that verified-phase YAML counts match actual `.v` file counts
- Hardcoded counts in `.mdx` (ledger-n values, scroll-count N/N labels)

**Full pipeline including Coq proof check**: `make fv-all` (runs `coq-check` + `update-fv` + `verify-report`).

**Manual steps still required when adding/completing a phase**:
- Update `status` field in `docs/src/data/formal_verification.yml` (e.g. `in_progress` → `verified`)
- Update `extracted` field if extraction status changed
- Add new phase entries to `formal_verification.yml` for new Coq modules
- Add/update phase card, ledger row, and module breakdown accordion in `formal-verification.mdx` (structure and prose — counts are patched automatically)

**When to run `make update-fv`**:
- After adding, removing, or modifying any Theorem/Lemma in a `.v` file
- After changing phase status in `formal_verification.yml`
- Before committing FV-related changes

## llms.txt Maintenance

Two files in `docs/public/`:
- `llms.txt` — spec-compliant index (follows llmstxt.org: H1, blockquote, H2 link-list sections only). Served at `clawq.org/llms.txt`.
- `llms-full.txt` — full self-knowledge reference. Served at `clawq.org/llms-full.txt`. This is the detailed document clawq uses to understand itself: every CLI command, config field with defaults, all tools, channels, endpoints, setup guides.

**When to update `docs/public/llms-full.txt`:**
- Adding, removing, or renaming a CLI command or subcommand (`src/main.ml`, `src/command_bridge.ml`)
- Adding or changing config fields or defaults (`src/runtime_config.ml`, `src/config_loader.ml`)
- Adding, removing, or renaming a built-in tool (`src/tools_builtin.ml`)
- Changing the shell allowlist or security mechanisms (`src/tools_builtin.ml`)
- Adding or changing HTTP gateway endpoints (`src/http_server.ml`)
- Adding or changing a channel implementation (`src/telegram.ml`, `src/discord.ml`, `src/slack.ml`, `src/slack_socket.ml`, etc.)
- Changing tunnel provider support
- Changing any user-facing behavior documented in the file

**When to update `docs/public/llms.txt`:**
- Adding new doc pages (add to the appropriate H2 link-list section)
- Changing the project summary

**How to update:**
- Keep llms-full.txt factual, concise, and oriented toward clawq operating on itself — not a marketing overview.
- Verify defaults against `Runtime_config.default` in `src/runtime_config.ml`.
- Verify tool names and counts against `src/tools_builtin.ml` registrations.
- Verify command names against `src/main.ml` command list.
- Keep llms.txt spec-compliant: no headings in body, H2 sections are link lists only.

## Research Source: nullclaw

- See `nullclaw/` in this repo's root.
