# P2-08: Sandbox Hardening — Firejail & Bubblewrap

## Context

clawq has Landlock OS-level sandboxing (`landlock.ml`) and workspace path checking (Coq-verified + `Unix.realpath`). nullclaw adds two more sandbox backends: **Firejail** and **Bubblewrap** (bwrap), with auto-detection and a fallback chain.

This plan adds Firejail and Bubblewrap support as alternative/complementary sandbox backends for tool execution, particularly `execute_shell` in `tools_builtin.ml`. It does not attempt to port nullclaw's full policy engine (`policy.zig` — autonomy levels, approval gates) — that is a separate concern.

## Design

### Sandbox Interface

Introduce `src/sandbox.ml` — a vtable-like dispatch module:

```ocaml
type backend =
  | Landlock    (* in-process via landlock.ml — already implemented *)
  | Firejail    (* wraps command with firejail *)
  | Bubblewrap  (* wraps command with bwrap *)
  | None        (* no wrapping, path checks only *)

type t = {
  backend : backend;
  workspace : string;
}

(* Wrap argv with sandbox prefix appropriate for backend *)
val wrap_command : t -> string array -> string array

(* Check if a backend is available on this system *)
val is_available : backend -> bool

(* Detect best available backend given config preference *)
val detect : preferred:string -> workspace:string -> t
```

### Auto-Detection

`Sandbox.detect`:
1. If `preferred = "landlock"` and running Linux 5.13+: use Landlock (already applied at process level by `landlock.ml`; no per-command wrapping needed).
2. If `preferred = "firejail"` or auto: check `which firejail` → use if available.
3. If `preferred = "bubblewrap"` or auto: check `which bwrap` → use if available.
4. Fallback: `None` (path checks still apply).

Detection: `Unix.access "/usr/bin/firejail" [Unix.X_OK]` (or search PATH).

### Firejail Wrapper

nullclaw's implementation:
```
firejail --private={workspace} --net=none --quiet --noprofile <original_argv>
```

In `tools_builtin.ml` `execute_shell`, wrap the command argv before spawning:
```ocaml
let argv = Sandbox.wrap_command sandbox [| shell_cmd; ... |] in
Lwt_process.exec (argv.(0), argv) ...
```

Options used:
- `--private={workspace}` — private home directory inside workspace
- `--net=none` — disable network access for shell tools
- `--quiet` — suppress firejail banner
- `--noprofile` — don't load default profiles that might conflict

### Bubblewrap Wrapper

nullclaw's implementation:
```
bwrap --ro-bind /usr /usr --dev /dev --proc /proc
      --bind /tmp /tmp --bind {workspace} /workspace
      --unshare-all --die-with-parent
      <original_argv>
```

Mounts:
- `--ro-bind /usr /usr` — read-only /usr
- `--dev /dev` — device access
- `--proc /proc` — proc filesystem
- `--bind /tmp /tmp` — temp files
- `--bind {workspace} /workspace` — workspace read-write
- `--unshare-all` — new user/pid/net/ipc/uts namespaces
- `--die-with-parent` — child dies if parent dies

### Config Changes

Add to `Runtime_config.security_config`:
```ocaml
sandbox_backend : string;   (* "auto" | "landlock" | "firejail" | "bubblewrap" | "none" *)
```

Default: `"auto"` (try Landlock → Firejail → Bubblewrap → None).

Note: Landlock is applied at process level in `daemon.ml` via `Landlock.activate`. Firejail/Bubblewrap wrap individual tool invocations. These are complementary.

### Integration with `tools_builtin.ml`

In `execute_shell`:
```ocaml
(* Current: spawn shell command directly *)
(* New: wrap with sandbox if configured *)
let cmd_argv = Sandbox.wrap_command sandbox [| "/bin/sh"; "-c"; command |] in
Lwt_process.exec (cmd_argv.(0), cmd_argv) ...
```

The sandbox is initialized in `daemon.ml` and passed through `Session` → `Agent` → tool invocations. Alternatively, store in a global ref initialized at startup (simpler for CLI mode).

### Approval Gates (Future — Out of Scope Here)

nullclaw's `policy.zig` has autonomy levels (none/basic/full) and per-tool approval gates. This is a separate, complex feature. This plan only adds the sandbox wrapping layer. Approval gates can be a P2-09-or-later plan.

## Files to Create/Modify

- **Create**: `src/sandbox.ml` — backend type, wrap_command, is_available, detect
- **Modify**: `src/tools_builtin.ml` — wrap `execute_shell` argv through `Sandbox.wrap_command`
- **Modify**: `src/runtime_config.ml` — add `sandbox_backend` to `security_config`
- **Modify**: `src/config_loader.ml` — parse `security.sandbox_backend`
- **Modify**: `src/daemon.ml` — call `Sandbox.detect` on startup, pass to session/tools
- **Modify**: `src/dune` — add `sandbox` to `clawq_runtime_core`

## Test Strategy

1. **`is_available`**: on Linux, check for firejail/bwrap presence (use `try Unix.access ... with _ -> false`)
2. **`wrap_command` firejail**: `[| "ls"; "-la" |]` → `[| "firejail"; "--private=/tmp/ws"; "--net=none"; "--quiet"; "--noprofile"; "ls"; "-la" |]`
3. **`wrap_command` bubblewrap**: verify bwrap prefix args contain `--unshare-all` and `--bind {workspace} /workspace`
4. **`wrap_command` none**: returns argv unchanged
5. **`detect`** with `preferred="none"` → always returns None backend
6. **`detect`** auto-detection order: verify priority (Landlock → Firejail → Bubblewrap → None)
7. **Execute shell with firejail**: integration test (skipped if firejail not installed)

Run: `make test`. Add suite `sandbox` to `test/test_main.ml`.

## Dependencies

- No new opam packages.
- External tools (optional, user-installed): `firejail`, `bwrap`.
- Existing: `Lwt_process` for command wrapping.
