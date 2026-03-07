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

## Quick File Map

- Build/test orchestration: `Makefile`
- UI source and Bun pipeline: `ui/`, `scripts/gen_chat_ui_assets.sh`
- Dune project config: `dune-project`, `dune-workspace`, `src/dune`, `test/dune`
- CLI entrypoints: `src/main.ml`, `src/main_min.ml`
- Command routing: `src/command_bridge.ml`, `src/command_bridge_min.ml`
- Web UI serving/assets: `src/ui_server.ml`, `src/chat_ui_assets.ml`
- Tests: `test/test_main.ml` and `test/test_*.ml`

## Web UI Dev Mode

- Build embedded assets with `make ui`.
- For live UI iteration, run `make ui-dev` and create `~/.clawq/ui/DEV`.
- With DEV mode enabled, clawq serves files from `~/.clawq/ui/` without overwriting them on startup.

## Recommended Agent Workflow

1. Read relevant modules and adjacent tests first.
2. Implement smallest viable change.
3. Run focused test(s), then `make test`.
4. Run formatting checks if OCaml files changed.
5. Summarize behavior changes and verification commands in final handoff.

## Research Source: nullclaw

- See `nullclaw/` in this repo's root.
