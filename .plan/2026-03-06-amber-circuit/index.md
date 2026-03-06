# I003: Self-Restarting Daemon

**Plan directory:** `.plan/2026-03-06-amber-circuit/`
**Date:** 2026-03-06
**Estimate:** ~17h (see backlog-tasks.md for breakdown)
**Backlog idea:** `.backlog/ideas/I003-support-self-restarting-this-s.todo`

## Goal

Enable the daemon to restart itself in-place after a code update, loading the new binary without
losing conversation history. The canonical dev loop becomes:

```
edit code -> make build -> make restart   # (or agent calls /update autonomously)
```

The daemon remains the same PID throughout (via `execve`). All active session histories are
persisted to SQLite before restart and reconstructed on startup. Sessions where the agent owed a
response are automatically resumed.

## Design Decisions

| Decision | Choice | Rationale |
|---|---|---|
| Restart mechanism | SIGUSR1 + `Unix.execv` | SIGHUP taken for config-reload; execv preserves PID |
| Fork avoidance post-execv | `CLAWQ_DAEMON_NOFORK=1` env var | Clean, no new flags needed in CLI arg parsing |
| State to persist | Message history + turn state + Discord RESUME | History = continuity; turn = resume agent; Discord RESUME = no missed messages |
| Drain timeout | 60s with user warnings at 5/10/15/30/45/60s | LLM calls can be slow; user should know what's happening |
| Agent turn resume | Immediately re-invoke on startup | Better UX; guard against duplicates via `response_sent_at` |
| Binary path on execv | `Sys.executable_name` | Same path, new binary after `make build` |
| `/update` command | git pull + make build + signal-restart | Dev-focused; binary download deferred to B018 |

## Architecture Overview

### Restart Flow

```
SIGUSR1 received
  |
  v
daemon.ml: restart_wakeup triggered
  |
  v
Drain phase (up to 60s):
  - draining := true  (channels stop accepting new messages)
  - warnings sent to active sessions at 5/10/15/30/45/60s via channel send fns
  - poll in_flight_count every 100ms
  |
  v
Flush phase:
  - session_state table updated for all active sessions (turn, last_active)
  - any unsaved messages flushed to messages table
  - MCP clients disconnected
  - telemetry flushed
  |
  v
Unix.putenv "CLAWQ_DAEMON_NOFORK" "1"
Unix.execv Sys.executable_name [| Sys.executable_name; "service"; "start" |]
  |
  v (new binary, same PID, same FDs)
service.ml: cmd_start detects CLAWQ_DAEMON_NOFORK=1
  - skip fork
  - skip PID write (PID unchanged)
  - call Lwt_main.run (Daemon.run ~config) directly
  |
  v
daemon.ml: Daemon.run starts
  - load sessions from DB (messages table, session_state table)
  - for each session where turn='agent' AND response_sent_at IS NULL
      AND last_active > now - 1h: re-invoke agent LLM
  - load Discord RESUME state from DB
  - start all channels
```

### Session Persistence Flow

```
User message arrives
  |
  v
session.ml: get_or_create_session session_key
  - if DB has messages for session_key: reconstruct agent.history
  - upsert session_state: (session_key, turn='user', channel, channel_id)
  |
  v
agent.ml: add_message role content
  - prepend to in-memory history
  - Memory.save_message db session_key role content tool_calls  <-- new
  - update session_state.turn and session_state.last_active    <-- new
  |
  v
agent.ml: run_turn (LLM call)
  - session_state.turn = 'agent' BEFORE calling LLM            <-- new
  |
  v
response generated
  - session_state.response_sent_at = now  AFTER channel send   <-- new
  - session_state.turn = 'user'                                 <-- new
```

## File Change Map

### New files
- `src/session_persistence.ml` — session_state CRUD + history load/save helpers
- `src/restart.ml` — drain logic, warning dispatch, execv orchestration
- `src/update_tool.ml` — /update git+build+signal implementation
- `test/test_session_persistence.ml` — unit tests for persistence layer
- `test/test_restart.ml` — unit tests for restart flow

### Modified files
- `src/memory.ml` — schema migration to v2: add `session_state`, `discord_resume_state` tables
- `src/agent.ml` — call `session_persistence` on message add; track turn state
- `src/session.ml` — load history from DB on session init
- `src/daemon.ml` — SIGUSR1 handler, drain loop, restart logic, on-startup resume
- `src/service.ml` — CLAWQ_DAEMON_NOFORK check in cmd_start
- `src/discord.ml` — save/load resume state from `discord_resume_state` table
- `src/command_bridge.ml` — add `service signal-restart` subcommand
- `src/tool_registry.ml` — register `update_clawq` tool
- `Makefile` — add `restart` target

## Supporting Documents

- [session-persistence.md](session-persistence.md) — DB schema, history reconstruction
- [restart-mechanism.md](restart-mechanism.md) — SIGUSR1, execv, drain, NOFORK
- [discord-resume.md](discord-resume.md) — Discord RESUME persistence design
- [update-command.md](update-command.md) — /update tool design
- [backlog-tasks.md](backlog-tasks.md) — task list with estimates and dependencies

## Risks and Notes

1. **Are messages already written to DB?** `memory.ml` has a `messages` table and `session.ml`
   has `db: Sqlite3.db option`, but it's unclear if messages are persisted during normal
   conversation or only for semantic search. **Must verify before T-SR2.** If already written,
   T-SR2 collapses to just wiring the read-back path.

2. **execv + Lwt:** `Unix.execv` must be called from outside the Lwt scheduler (after
   `Lwt_main.run` returns or from a point where the scheduler is stopped). The drain loop
   must fully cancel all pending Lwt promises before calling execv.

3. **session_state.channel routing for warnings:** Warning messages during drain require
   knowing which send function to use per session. The daemon already has references to
   all channel modules. We'll use a `(string -> unit Lwt.t) Hashtbl.t` (session_key → send_fn)
   populated when channels start, updated when sessions are created.

4. **Race: response sent vs. restart mid-flight:** The guard is `response_sent_at IS NULL` in
   session_state. The channel send function must update this field AFTER successfully sending.
   If send fails, field stays NULL and restart will retry — acceptable since the response was
   never delivered anyway.

5. **`/update` on dirty working tree:** git pull may fail or noop. That's fine — if there are
   local changes, the build still runs against whatever is in the tree. The command should
   report git output clearly and not fail if pull returns non-zero.
