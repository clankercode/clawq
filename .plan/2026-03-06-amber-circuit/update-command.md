# /update Command Design

## Overview

`/update` triggers a self-update cycle: pull latest code, rebuild, signal restart. It is available
as both a chat command (user types `/update` in any channel) and as an agent tool (the agent can
call it autonomously, e.g. after patching source files).

**Binary download mode is deferred to B018.** This design covers git+compile only.

## Behavior

```
User: /update

Clawq: Starting update...
       Running: git pull
       [output of git pull, or "Already up to date." or error]
       Running: make build
       [build progress indication]
       Build complete. Restarting...
       [daemon restarts via SIGUSR1, drain proceeds, user sees drain warnings]
```

If git pull fails (e.g., conflicts, no remote, network error): **continue to build anyway.**
The user may have local changes — the point is to rebuild and restart whatever is currently in
the working tree.

If make build fails: **report the error and do NOT restart.** A failed build should not replace
the running daemon.

## Implementation: src/update_tool.ml

```ocaml
(** Find the repository root from the executable path.
    Walks up from Sys.executable_name looking for a dune-project file. *)
val find_repo_root : unit -> string option

(** Run the update sequence.
    Returns a stream of progress strings via the callback. *)
val run_update :
  send_progress:(string -> unit Lwt.t) ->
  unit Lwt.t
```

Internals:

1. `find_repo_root ()` — walk up from `Sys.executable_name` looking for `dune-project` or
   `.git`. Return `None` if not found (update impossible, report error).

2. Run `git -C repo_root pull 2>&1` via `Lwt_process.pread`. Non-fatal on non-zero exit.
   Send output to user.

3. Run `make -C repo_root build 2>&1` via `Lwt_process.pread_lines` for streaming output.
   On non-zero exit: send error message and return (do NOT restart).

4. Send "Build complete. Sending restart signal..." to user.

5. `Unix.kill (Unix.getpid ()) Sys.sigusr1` — trigger SIGUSR1 on ourselves. The existing
   SIGUSR1 handler in daemon.ml takes over from here (drain, execv, etc.).

## Agent Tool Registration

In `src/tool_registry.ml`, register a new tool:

```
name:        "update_clawq"
description: "Update clawq to the latest version by pulling from git and rebuilding.
              Use this after modifying source code to reload the changes.
              Reports progress and triggers a graceful restart."
parameters:  {} (no parameters)
```

The tool implementation calls `Update_tool.run_update ~send_progress:(reply session_key)`.

## Chat Command: /update

Channel message handlers already dispatch tool-like commands (or will via a command parser).
Add `/update` to the command dispatch table as a shorthand that invokes `update_clawq` tool.

Suggested location: wherever `/help` or similar commands are handled. If no such dispatcher
exists yet, add a pre-agent check in session.ml or the channel handler:

```ocaml
if String.trim text = "/update" then
  Update_tool.run_update ~send_progress:(reply session_key)
else
  (* normal agent dispatch *)
```

## Error Cases

| Situation | Behavior |
|---|---|
| `find_repo_root` returns None | "Cannot find repository root, update not available." |
| git pull exits non-zero | Log warning, report to user, continue to build |
| make build exits non-zero | "Build failed: [output]. Restart aborted." |
| SIGUSR1 triggers drain which times out | Drain warning messages sent, force restart anyway |
| Update called while already draining | "Restart already in progress, please wait." |

## Notes

- `Lwt_process.pread` collects all output before returning. For `make build`, which can take
  30+ seconds, prefer `pread_lines` with a callback to stream output progressively to the user.
- The `send_progress` callback should rate-limit to avoid flooding the chat (e.g., buffer lines,
  flush every 2s).
- The agent should not call `update_clawq` in a loop. Add a guard: if `draining = true`, the
  tool returns an error immediately.
