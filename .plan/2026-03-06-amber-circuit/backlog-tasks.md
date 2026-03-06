# I003 Backlog Tasks

All tasks below should be created with `backlog add` under a new epic/milestone.

## Critical Finding (Pre-Implementation Verified)

**Messages are ALREADY persisted and loaded** in `session.ml`:
- `Memory.load_history` called on `get_or_create` (line 20) — history reconstructed from DB
- `Memory.store_message` called after every turn (lines 98-100, 155-157) — all messages written

This means **T-SR2 and T-SR3 are already implemented**. The `messages` table is live and used.
The session persistence for restart is mostly already working — restart just needs to know
_which sessions were active_ and _whose turn it was_.

## Revised Task List

---

### T-SR1 — DB Schema Migration: session_state table
**File:** `src/memory.ml`
**Estimate:** 0.5h
**Depends on:** nothing

Bump schema_version 1 → 2. Add migration that creates:

```sql
CREATE TABLE IF NOT EXISTS session_state (
  session_key      TEXT PRIMARY KEY,
  turn             TEXT NOT NULL DEFAULT 'user',  -- 'user' | 'agent'
  channel          TEXT,
  channel_id       TEXT,
  response_sent_at TEXT,
  last_active      TEXT NOT NULL DEFAULT (datetime('now'))
);
```

Note: `discord_resume_state` table also added here (see T-SR9).

Add test: migration runs cleanly on fresh DB and on existing v1 DB.

---

### T-SR2 — Verify message persistence completeness
**File:** `src/session.ml`, `src/memory.ml`
**Estimate:** 0.5h (was 2h — already implemented!)
**Depends on:** nothing

Messages already persisted via `Memory.store_message`. Verify:
1. `tool_calls_json` field is correctly serialized (check `Memory.store_message` signature)
2. `tool_call_id` and `tool_name` fields populated for tool messages
3. The `load_history` → `store_message` round-trip preserves tool_calls correctly

No code changes likely needed. Just read and confirm; add a test case if the round-trip is
not already covered.

---

### T-SR3 — Verify history reconstruction on restart
**File:** `src/session.ml` lines 11-39
**Estimate:** 0.25h (was 1h — already implemented!)
**Depends on:** nothing

History already loaded in `get_or_create` from `Memory.load_history`. This means after
restart, when a new session is created for an existing `session_key`, the history is
automatically restored from DB. No code changes needed.

Confirm: the `get_or_create` path works correctly with CLAWQ_DAEMON_NOFORK (i.e., the
`db` handle is properly opened before session operations). Document this in index.md.

---

### T-SR4 — Turn state tracking in session_state
**Files:** `src/session.ml`, `src/session_persistence.ml` (new, or inline in session.ml)
**Estimate:** 1h
**Depends on:** T-SR1

After T-SR1 creates `session_state` table, write to it at key transition points:

1. **Before LLM call** (in `session.ml:turn` before `Agent.turn`):
   ```ocaml
   upsert_session_state db ~session_key:key ~turn:"agent"
     ~channel ~channel_id ()
   ```

2. **After response sent** (in each channel handler, after the channel send succeeds):
   ```ocaml
   mark_response_sent db ~session_key:key ()
   (* sets response_sent_at = datetime('now'), turn = 'user' *)
   ```

The `channel` and `channel_id` must be threaded into `session.ml:turn` as optional
parameters (already has `?channel_name ?channel_type` — add `?channel_id` for routing).

Helper functions (inline in session.ml or new session_persistence.ml):
- `upsert_session_state`
- `mark_response_sent`
- `load_pending_agent_sessions`

---

### T-SR5 — On-startup resume of agent-turn sessions
**File:** `src/daemon.ml`
**Estimate:** 1.5h
**Depends on:** T-SR4

At top of `Daemon.run`, after DB open, before channel starts:

```ocaml
let pending = load_pending_agent_sessions ~db:(Some db) ~max_age_seconds:3600 in
(* For each: reconstruct session via get_or_create (history auto-loaded),
   invoke agent.turn, route response via appropriate channel send fn *)
```

The tricky part: routing the resumed response back requires knowing the channel send function.
The daemon has references to all channel modules. Use a dispatch table keyed on `channel`:

```ocaml
let resume_send = match channel with
  | "telegram" -> fun text -> Telegram.send_message ~bot_token ~chat_id:channel_id ~text
  | "discord"  -> fun text -> Discord.send_message ~token ~channel_id ~text
  | "slack"    -> fun text -> Slack.send_message ~token ~channel_id ~text
  | _          -> fun _ -> Lwt.return ()
in
```

Config values (tokens) come from `config` which is available in `Daemon.run`.

---

### T-SR6 — SIGUSR1 handler + drain + execv
**File:** `src/daemon.ml` (+ potentially `src/restart.ml` if logic is large)
**Estimate:** 2.5h
**Depends on:** T-SR4, T-SR5

**Signal registration** (alongside SIGINT/SIGTERM):
```ocaml
let restart_waiter, restart_wakeup = Lwt.wait () in
let _ = Lwt_unix.on_signal Sys.sigusr1 (fun _ ->
    Logs.info (fun m -> m "SIGUSR1: initiating graceful restart");
    Lwt.wakeup_later restart_wakeup ()) in
```

Wire `restart_waiter` into the main `Lwt.pick` so it can interrupt the gateway loop.

**Drain sequence:**
1. `draining := true` — global ref; channel handlers check before dispatching new messages
2. Warning loop (concurrent Lwt task): send warnings at 5/10/15/30/45/60s to all sessions
   in `channel_notifiers` hashtable (session_key → send fn, registered by channel handlers)
3. Wait loop: poll `!in_flight_count = 0` every 100ms, up to 600 ticks (60s)
4. Flush: flush session_state for all active sessions, MCP disconnect, telemetry, audit log
5. `Unix.putenv "CLAWQ_DAEMON_NOFORK" "1"`
6. `Unix.execv Sys.executable_name [| Sys.executable_name; "service"; "start" |]`

`in_flight_count`: atomic int ref in daemon.ml (or session.ml), incremented before `Agent.turn`,
decremented after (wrap in try/finally to ensure decrement on exception too).

`channel_notifiers`: `(string, string -> unit Lwt.t) Hashtbl.t` in daemon.ml, populated by
channel message handlers when they start processing a message.

---

### T-SR7 — CLAWQ_DAEMON_NOFORK in service.ml
**File:** `src/service.ml`
**Estimate:** 0.5h
**Depends on:** T-SR6

At top of `cmd_start`:
```ocaml
if Sys.getenv_opt "CLAWQ_DAEMON_NOFORK" = Some "1" then begin
  Unix.putenv "CLAWQ_DAEMON_NOFORK" "";  (* clear so children don't inherit *)
  Logs.info (fun m -> m "Daemon restarting in-place (NOFORK)");
  (try Lwt_main.run (Daemon.run ~config) with _ -> ());
  ""
end else
  ... (* existing fork/daemonize logic unchanged *)
```

The NOFORK path skips: fork, setsid, FD redirects, PID write. The existing PID file
remains valid (same PID after execv).

---

### T-SR8 — Discord RESUME persistence
**Files:** `src/discord.ml`, `src/memory.ml` (migration), `src/dune`
**Estimate:** 1.5h
**Depends on:** T-SR1

New `discord_resume_state` table (added in T-SR1 migration):
```sql
CREATE TABLE IF NOT EXISTS discord_resume_state (
  id                 INTEGER PRIMARY KEY CHECK (id = 1),
  session_id         TEXT NOT NULL,
  seq                INTEGER NOT NULL,
  resume_gateway_url TEXT NOT NULL,
  updated_at         TEXT NOT NULL DEFAULT (datetime('now'))
);
```

In `discord.ml`:
- On startup: query `discord_resume_state` and pre-populate `resume_session_id/seq/url` refs
- After disconnect: `INSERT OR REPLACE INTO discord_resume_state (id,...) VALUES (1,...)`
- On fatal close code (4004, 4010-4014): `DELETE FROM discord_resume_state WHERE id=1`

Thread `db: Sqlite3.db option` through `Discord.start` signature (currently only has config).

See `discord-resume.md` for full design.

---

### T-SR9 — clawq service signal-restart command
**Files:** `src/service.ml`, `src/command_bridge.ml`
**Estimate:** 0.5h
**Depends on:** T-SR6

In `service.ml`:
```ocaml
let cmd_signal_restart () =
  match read_pid () with
  | None -> "Daemon is not running (no PID file)"
  | Some pid ->
    Unix.kill pid Sys.sigusr1;
    "Restart signal sent to PID " ^ string_of_int pid
```

In `command_bridge.ml`, add `"signal-restart"` case to service dispatch.
Error behavior: return error string if not running (do NOT auto-start — Q4 answer).

---

### T-SR10 — make restart target
**File:** `Makefile`
**Estimate:** 0.25h
**Depends on:** T-SR9

```makefile
CLAWQ_BIN ?= ./_build/default/src/main.exe

.PHONY: restart build-restart

restart: ## Gracefully restart running daemon (in-place, same PID)
	$(CLAWQ_BIN) service signal-restart

build-restart: build restart ## Build then gracefully restart daemon
```

---

### T-SR11 — /update agent tool + chat command
**Files:** `src/update_tool.ml`, `src/tool_registry.ml`, channel handlers, `src/dune`
**Estimate:** 1.5h
**Depends on:** T-SR6, T-SR9

New `update_tool.ml`:
1. `find_repo_root ()` — walk up from `Sys.executable_name` for `dune-project` / `.git`
2. `git -C root pull` via `Lwt_process.pread_lines` — non-fatal on non-zero exit, stream output
3. `make -C root build` via `Lwt_process.pread_lines` — fatal on non-zero (do NOT restart)
4. `Unix.kill (Unix.getpid ()) Sys.sigusr1` — triggers drain+restart

Register `update_clawq` tool in `tool_registry.ml`.
Add `/update` pre-agent dispatch in channel handlers (check before routing to agent).

Guard: if `!draining = true`, return "Restart already in progress" immediately.

See `update-command.md` for full design and error cases.

---

### T-SR12 — Unit tests
**Files:** `test/test_session_persistence.ml`, `test/test_restart.ml`, `test/dune`
**Estimate:** 2h
**Depends on:** T-SR1, T-SR4, T-SR7, T-SR8

Test suites:
- Schema migration v1 → v2 (fresh DB, existing v1 DB)
- `session_state` upsert + `mark_response_sent` + `load_pending_agent_sessions`
- Tool call round-trip: `store_message` with tool_calls → `load_history` preserves calls
- CLAWQ_DAEMON_NOFORK detection in service.ml (mock env, no actual fork)
- Discord RESUME save → load round-trip

---

### T-SR13 — Integration test (start → message → restart → verify)
**File:** Shell script or `test/test_integration_restart.sh`
**Estimate:** 1h
**Depends on:** all above

Script:
1. Start daemon (`clawq service start` or `clawq agent`)
2. Send a test message via HTTP gateway (`curl /chat`)
3. Verify message appears in messages table (`sqlite3 ~/.clawq/memory.db`)
4. Trigger `clawq service signal-restart`
5. Poll `clawq status` / health endpoint until daemon responds
6. Verify session history still in DB with correct session_key
7. (Optional) Send another message and verify history continues from previous

---

## Dependency Graph

```
T-SR1 (schema migration)
  |
  +-- T-SR2 (verify persistence — already done, just audit)
  |
  +-- T-SR3 (verify reconstruction — already done, just audit)
  |
  +-- T-SR4 (turn state tracking)
  |     |
  |     +-- T-SR5 (on-startup resume)
  |     |
  |     +-- T-SR6 (SIGUSR1 + drain + execv)
  |           |
  |           +-- T-SR7 (CLAWQ_DAEMON_NOFORK)
  |           |
  |           +-- T-SR9 (signal-restart CLI)
  |           |     |
  |           |     +-- T-SR10 (make restart)
  |           |     |
  |           |     +-- T-SR11 (/update tool) [also needs T-SR6]
  |           |
  |           +-- T-SR5 (startup resume, also needs T-SR4)
  |
  +-- T-SR8 (Discord RESUME) [parallel to T-SR4..T-SR7]
  |
T-SR12 (unit tests) [needs T-SR1, T-SR4, T-SR7, T-SR8]
T-SR13 (integration test) [needs all]
```

**T-SR8 (Discord RESUME) can be done in parallel with T-SR4 through T-SR7.**

## Revised Estimate

| Task | Description | Estimate |
|---|---|---|
| T-SR1 | DB schema migration | 0.5h |
| T-SR2 | Verify message persistence (already done) | 0.5h |
| T-SR3 | Verify history reconstruction (already done) | 0.25h |
| T-SR4 | Turn state tracking | 1h |
| T-SR5 | On-startup resume of agent sessions | 1.5h |
| T-SR6 | SIGUSR1 + drain + execv | 2.5h |
| T-SR7 | CLAWQ_DAEMON_NOFORK | 0.5h |
| T-SR8 | Discord RESUME persistence | 1.5h |
| T-SR9 | signal-restart CLI | 0.5h |
| T-SR10 | make restart target | 0.25h |
| T-SR11 | /update tool | 1.5h |
| T-SR12 | Unit tests | 2h |
| T-SR13 | Integration test | 1h |
| **Total** | | **~13.5h** |

Down from 16.25h to ~13.5h thanks to the pre-existing persistence layer.
The original 10h estimate was still optimistic; recommend updating I003 to 14h.

## Note on Existing `clawq service restart`

The existing `cmd_restart` (stop + sleep 1s + start) remains unchanged as a "hard restart."
It is equivalent to running the new binary with `service restart` after `make build`.
The new `signal-restart` is the preferred dev-loop path: in-place, same PID, history survives.
