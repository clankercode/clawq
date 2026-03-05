# P2-09: WASM Runtime

## Context

nullclaw supports a WASM/WASI build target that runs in wasmtime or browsers. clawq has WASM mentioned in PLAN.md as a target but it is not implemented. The primary motivation is enabling sandboxed execution in cloud environments and edge deployments where native binaries can't run.

OCaml has two WASM paths:
1. **`wasm_of_ocaml`** (Tarides, recently released) — compiles OCaml bytecode to WASM + WASI; supports most of the stdlib
2. **`js_of_ocaml`** — compiles to JavaScript (not WASM, but runs in browsers/Node)

The most practical approach for clawq is `wasm_of_ocaml` targeting WASI, which is the OCaml equivalent of nullclaw's `main_wasi.zig`.

## Design

### Scope of WASM Build

The WASM build must be **minimal** — no networking, no daemon, no channels. It supports:
- `help`, `version`, `status`, `onboard` — informational commands
- `agent` — interactive CLI chat (stdin/stdout, single-turn, no history persistence)
- `memory` — read/list core memories from workspace file
- `identity` — show identity docs (AGENTS.md, SOUL.md)

No: HTTP server, Telegram, Discord, daemon, SQLite (file I/O only), TLS.

### Build Target

New dune executable target `main_wasm` (analogous to `main_min`) in `src/dune`:

```lisp
(executable
 (name main_wasm)
 (libraries clawq_runtime_core_wasm cmdliner)
 (modes wasm))
```

Requires `wasm_of_ocaml` installed in the opam switch.

### New Library: `clawq_runtime_core_wasm`

A further-stripped version of `clawq_runtime_core` that excludes:
- Modules using `Unix` beyond basic file I/O (no sockets, no landlock)
- `Lwt` (WASI is synchronous in most runtimes)
- `Sqlite3` — replace with file-based JSON memory
- `mirage-crypto-rng` (WASI PRNG is different)

Alternatively, implement a thin WASM shim (`src/main_wasm.ml`) that directly implements the minimal command set without pulling in the full runtime.

### Simpler Alternative: WASM Shim

Given the complexity of porting the full runtime to WASM, the simpler approach matches nullclaw's `main_wasi.zig` strategy: a **standalone WASM entrypoint** that reimplements only what's needed:

```ocaml
(* src/main_wasm.ml *)
let () =
  match Sys.argv with
  | [| _; "help" |] | [| _ |] -> print_endline help_text
  | [| _; "version" |] -> print_endline "clawq 0.1.0-wasm"
  | [| _; "status" |] -> print_endline (status_text ())
  | [| _; "agent" |] -> run_agent_loop ()
  | [| _; "memory"; "list" |] -> list_memories ()
  | [| _; "identity" |] -> show_identity ()
  | _ -> Printf.eprintf "Unknown command\n"; exit 1
```

Memory storage for WASM: read/write `MEMORY.md` (Markdown) in the workspace root directory (file-based, no SQLite). WASI file access is limited to the preopened directory.

LLM provider for WASM: HTTP calls via WASI sockets (available in wasmtime 14+ with `--allow-ip-name-lookup` and network permission). Use minimal HTTP implementation (no cohttp, just raw WASI socket calls or `curl` subprocess).

### Build System Changes

1. Add `wasm_of_ocaml` as an optional build dependency in `clawq.opam` (`{with-wasm}` optional flag or separate opam package).
2. Add `build-wasm` Makefile target:
   ```makefile
   build-wasm:
   	opam exec --switch=clawq-5.1 -- dune build src/main_wasm.bc.wasm
   ```
3. Output path: `_build/default/src/main_wasm.bc.wasm`

### Runtime Requirements

Users running the WASM binary need wasmtime:
```bash
wasmtime --dir . ./clawq.wasm agent
```

Or via browser with a WASI polyfill (advanced, out of scope for initial implementation).

### Identity Templates (matching nullclaw)

On `onboard` in WASM mode, write default template files to workspace:
- `IDENTITY.md` — bot name, role, personality
- `USER.md` — user preferences
- `MEMORY.md` — durable memory notes (key-value in Markdown table)
- `HEARTBEAT.md` — periodic review checklist

These mirror nullclaw's WASM default templates.

## Files to Create/Modify

- **Create**: `src/main_wasm.ml` — minimal WASM entrypoint (standalone, no library deps)
- **Modify**: `src/dune` — add `main_wasm` executable target with `wasm_of_ocaml` backend
- **Modify**: `Makefile` — add `build-wasm`, `run-wasm` targets
- **Modify**: `clawq.opam` — add `wasm_of_ocaml` as optional dependency
- **Modify**: `docs/QUICKSTART.md` — document WASM build + wasmtime usage

## Test Strategy

1. **Build test**: `dune build src/main_wasm.bc.wasm` succeeds without error
2. **Help command**: `wasmtime --dir . ./clawq.wasm help` prints help text (CI skipped if wasmtime absent)
3. **Version command**: `wasmtime ./clawq.wasm version` prints version string
4. **Memory file read**: pre-populate `MEMORY.md` → `wasmtime --dir . ./clawq.wasm memory list` returns entries
5. **Onboard**: `wasmtime --dir . ./clawq.wasm onboard` creates template files in workspace

Add `make test-wasm` target (skipped if wasmtime not in PATH).

## Dependencies

- `wasm_of_ocaml` (optional opam package, not required for default build)
- `wasmtime` (external tool, for running/testing WASM output)
- No changes to existing build for users who don't need WASM

## Risks

- `wasm_of_ocaml` is relatively new; some stdlib functions may not be supported yet.
- WASI networking support in wasmtime requires explicit `--allow-ip-name-lookup` and socket permissions.
- If `wasm_of_ocaml` proves too incomplete, fallback is `js_of_ocaml` (produces JS, not WASM, but runs in Node/browser).
