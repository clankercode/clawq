# I019: Error Correction Watcher

## Context

clawq has stuck detection (`session_observer.ml` + `stuck_detector.ml`) and postmortem agents, but no proactive periodic scanner that catches errors from daemon logs, background task failures, and session tool errors. I019 adds:

1. A **daemon-side scanner** (Lwt fiber) that periodically scans daemon.log and the DB for errors
2. A **separate OS process** (`clawq ec-run`) spawned via `Process_group` that is truly independent of the daemon runtime — survives deadlocks
3. A **full multi-model diagnosis + voting pipeline** for complex errors (deadlocks, etc.)
4. Toggleable on/off. Default on for dev builds (`-dev` in version), off for release.

The EC process operates independently: its own DB connection, its own Lwt event loop, its own provider calls. If the daemon deadlocks, the already-spawned EC process continues running.

---

## Architecture

```
Daemon                           EC Process (separate OS process, long-running)
┌─────────────────┐              ┌──────────────────────────────┐
│ error_watcher.ml│              │ ec_process.ml                │
│                 │  fork+exec   │                              │
│ manage EC       ├─────────────>│ Own scan loop (every 30s):   │
│  lifecycle      │              │  1. Scan daemon.log          │
│ PID tracking    │              │  2. Scan session DB          │
│ graceful        │  SIGUSR2     │  3. Scan bg task errors      │
│  handoff on     ├─────────────>│  4. Correlate logs+sessions  │
│  restart        │              │  5. Dedup + classify         │
└─────────────────┘              │  6. Multi-model diagnosis    │
                                 │  7. Solution voting          │
                                 │  8. Synthesize plan          │
                                 │  9. Spawn fix task           │
                                 │ 10. Write EC report          │
                                 └──────────────────────────────┘
```

---

## New Files

### 1. `src/error_watcher.ml` (~350 lines, `clawq_runtime_core`)

Daemon-side EC process lifecycle manager. The daemon does NOT scan errors itself — it only manages the EC process.

**Types:**
- `error_source = DaemonLog | SessionError | BackgroundTaskLog`
- `error_entry = { source; timestamp; session_key option; message; context; severity }`
- `state` — mutable record: `ec_pid option`, `ec_start_time`, `old_ec_pid option` (during handoff)

**EC process lifecycle:**
- `start_ec_process ~clawq_dir ~config ()` — spawns `clawq ec-run --daemon-mode` via `Process_group.start_to_file`, writes PID file
- `stop_ec_process ~state ()` — sends SIGTERM, waits up to 30s
- `graceful_handoff ~state ~clawq_dir ~config ()` — starts new EC process, sends SIGUSR2 to old, waits up to 90s, then SIGTERM old if still alive
- `check_ec_health ~state ()` — verify EC process is alive (PID check), restart if crashed

**Shared types and helpers** (used by both daemon and EC process):
- `strip_ansi s` — remove ANSI escape sequences
- `parse_log_line s` — parse `[HH:MM:SS.mmm] LEVEL [session_key] message` format
- `error_entry` JSON serialization/deserialization
- `pid_file_path`, `lock_file_path` constants

**Daemon integration loop:**
- `run_lifecycle_loop ~state ~config ~clawq_dir ()` — check EC health every 30s, restart if crashed

### 2. `src/ec_process.ml` (~700 lines, `clawq_runtime_integrations`)

Long-running EC process — runs as a separate OS process via `clawq ec-run --daemon-mode`. Has its own DB connection, config, Lwt event loop.

**Entry point:**
- `run_daemon_mode ()` — opens own DB connection, loads config, installs signal handlers, runs scan+analyze loop

**Signal handling:**
- `SIGUSR2` → set `pausing := true`; if idle, sleep 60s then exit; if mid-analysis, finish then sleep 60s then exit
- `SIGTERM` → set `shutting_down := true`; finish current work (up to 30s), write final report, exit
- `SIGINT` → immediate clean exit

**PID/lock file management:**
- On startup: write PID to `~/.clawq/ec_process.pid` (atomic: write `.pid.new`, rename)
- Acquire `~/.clawq/ec_process.lock` with `flock(LOCK_EX | LOCK_NB)` for DB writes
- On exit: remove PID file, release lock

**Scan loop (every `scan_interval_s`, default 30s):**
1. Scan daemon.log from tracked byte offset (parse `[HH:MM:SS.mmm] LEVEL message`, strip ANSI, handle rotation)
2. Scan session DB for tool errors (`role='tool' AND content LIKE 'Error:%'`, exclude `__error_correction__%` and `__postmortem_%`)
3. Scan background tasks for failures (`status='failed'` since last scan)
4. Correlate: match daemon.log entries with session messages by timestamp + session_key
5. Deduplicate by normalized first-line within cooldown window
6. Classify: transient (connection refused, timeout, 429, ECONNRESET) vs actionable
7. If actionable errors found → run multi-model pipeline

**Log+session correlation:**
- `format_correlated_context errors` — interleaves daemon.log lines and session messages chronologically:
  ```
  === Error Cluster: 2026-03-12T10:15:30 - 10:15:35 ===
  [10:15:30.123] ERROR [telegram:123:456] Provider timeout
  [10:15:31.000] DB: session telegram:123:456 tool_result: "Error: connection timed out"
  [10:15:32.500] ERROR [telegram:123:456] Retry failed
  [10:15:35.000] DB: session telegram:123:456 tool_result: "Error: connection refused"
  ```

**Multi-model diagnosis pipeline:**

Phase 1 — **Diagnosis** (parallel):
- Query primary models (`anthropic:claude-opus-4-6`, `openai-codex:gpt-5.4`) in parallel with error context
- Each returns structured diagnosis: classification, root cause, affected components
- If primary models fail, fall back to `zai_coding:glm-5` and `kimi_coding:kimi-for-code`
- For fallback models: query both in parallel, non-fatal failures (continue with available results), combine their diagnoses into one merged analysis
- Error handling: if all models fail, write a minimal report and exit scan cycle (non-fatal, try again next cycle)

Phase 2 — **Solution proposals** (parallel):
- Each model proposes ranked solutions with sub-components
- Solutions have property tags for voting (e.g., approaches A/B for component X, approaches C/D for component Y)

Phase 3 — **Voting**:
- Present all proposed solutions to all available models
- Each model ranks the complete solution combinations (e.g., `[A+D, A+C, B+D, B+C]`)
- Tally votes across models, determine winning combination
- Handle ties by preferring the simpler/more conservative solution

Phase 4 — **Planning**:
- Top 2 models each plan the winning solution in detail
- Synthesize a best-of-both plan: merge complementary details, resolve conflicts by preferring the more thorough approach

Phase 5 — **Implementation** (if `auto_fix_enabled`):
- Enqueue a background fix task via `Background_task.enqueue` with:
  - `use_worktree = true`, `automerge = false` (safety: manual merge for EC fixes)
  - Branch: `ec/fix-{error_hash}-{timestamp}`
  - Commit tag requirement in prompt: `[INTERNAL_EC]`
  - The synthesized plan as the agent prompt

**Deadlock-specific handling:**
- When errors suggest deadlock (mutex timeout, `Lwt_mutex`, process hang, stale PID):
  - Always use powerful thinking models (not fallbacks)
  - Include instructions to investigate: check file/socket locks, examine thread states, analyze mutex ordering
  - Instruct the coding agent to verify the fix addresses the root cause (not just symptoms)

**Report writing:**
- Writes markdown to `~/.clawq/ec_reports/{timestamp}-{error_hash}.md` (PID in filename during handoff)
- Includes: error context, correlated logs, all model diagnoses, voting results, winning plan, implementation status

**Helper:** `query_model ~config ~model ~system_prompt ~user_prompt` — wraps `Provider.complete` with explicit model override.

### 3. `test/test_error_watcher.ml` (~400 lines)

Unit tests (no LLM calls):
- ANSI stripping (various escape sequences)
- Log line parsing (timestamps, levels, session IDs, edge cases)
- Error classification (transient vs actionable patterns)
- Deduplication (within/outside cooldown)
- EC prompt construction (well-structured output)
- Dev build detection (`"0.1.0-dev"` → true, `"1.0.0"` → false)
- Config defaults match build profile
- Log rotation offset reset (file shrunk)
- Ignore pattern matching
- Session key exclusion (EC/postmortem sessions filtered)
- Correlated context formatting (daemon.log + session DB merged)
- Error entry JSON serialization/deserialization
- Full scan cycle with temp DB + mock daemon.log

---

## Files to Modify

### 4. `src/runtime_config.ml` (~+30 lines)

Add after `observer_config` (line ~395):
```ocaml
type error_watcher_config = {
  ec_enabled : bool;
  scan_interval_s : int;           (* default 30 *)
  primary_models : Pmodel.t list;  (* default [anthropic:claude-opus-4-6; openai-codex:gpt-5.4] *)
  fallback_models : Pmodel.t list; (* default [zai_coding:glm-5; kimi_coding:kimi-for-code] *)
  cooldown_s : int;                (* default 300 *)
  max_errors_per_batch : int;      (* default 10 *)
  ignore_patterns : string list;   (* regex patterns to suppress *)
  auto_fix_enabled : bool;         (* default false *)
  ec_commit_tag : string;          (* default "[INTERNAL_EC]" *)
}
```

Default: `ec_enabled = Build_info.version` contains `-dev`. Add `error_watcher : error_watcher_config` to `type t` after `log`.

### 5. `src/config_loader.ml` (~+50 lines)

Add `error_watcher` JSON section parsing following `observer` pattern (lines 1340-1389). Key: `"error_watcher"`. Parse `primary_models` and `fallback_models` as string lists → `Pmodel.t list`.

### 6. `src/config_set.ml` (~+12 lines)

Add `error_watcher` to `config_schema` with leaf fields.

### 7. `src/daemon.ml` (~+35 lines)

Add EC process lifecycle management after background task poll loop (~line 1144):
```
if config.error_watcher.ec_enabled then begin
  let ec_state = Error_watcher.create_state () in
  (* Start EC process on daemon startup *)
  Lwt.async (fun () -> Lwt.catch (fun () ->
    let* () = Error_watcher.graceful_handoff ~state:ec_state ~clawq_dir ~config () in
    (* Health check loop: restart EC if it crashes *)
    let rec loop () =
      let cur = Session.get_config session_manager in
      if cur.error_watcher.ec_enabled then begin
        let* () = Error_watcher.check_ec_health ~state:ec_state ~clawq_dir ~config () in
        let* () = Lwt_unix.sleep 30.0 in
        loop ()
      end else begin
        (* Disabled at runtime: stop EC process *)
        let* () = Error_watcher.stop_ec_process ~state:ec_state () in
        let* () = Lwt_unix.sleep 60.0 in
        loop ()
      end
    in loop ()) (fun exn -> log error));
  log "Error correction watcher started"
end
```

Also register EC process cleanup in daemon shutdown handler (send SIGTERM to EC process on daemon exit).

### 8. `src/command_bridge.ml` (~+40 lines)

Add `"watcher" :: rest -> cmd_watcher rest` to `handle`.

Commands:
- `watcher` / `watcher status` — show config, EC process status
- `watcher enable` / `watcher disable` — toggle via config_set
- `watcher reports` — list EC reports from `~/.clawq/ec_reports/`
- `watcher report ID` — show specific report content

### 9. `src/command_bridge_min.ml` (~+1 line)

Add `"watcher" :: _ -> "Error watcher commands are disabled in minimal build."`.

### 10. `src/main.ml` (~+20 lines)

Add `watcher_cmd` and `ec_run_cmd` (internal subcommand) to cmdliner:
- `clawq watcher [status|enable|disable|reports|report ID]` — user-facing
- `clawq ec-run --daemon-mode` — internal, long-running process spawned by daemon; runs own scan loop, handles SIGUSR2/SIGTERM

### 11. `src/dune` (~+2 lines)

Add `error_watcher` to `clawq_runtime_core`, `ec_process` to `clawq_runtime_integrations`.

### 12. `test/test_main.ml` + `test/dune`

Add `("error_watcher", Test_error_watcher.suite)` and module reference.

---

## Multi-Model Pipeline Detail

```
Primary Models (parallel)     Fallback Models (parallel, non-fatal)
┌──────────────────────┐     ┌──────────────────────┐
│ claude-opus-4-6      │     │ zai_coding:glm-5     │
│ openai-codex:gpt-5.4 │     │ kimi_coding:kimi-for-│
└──────┬───────────────┘     │   code               │
       │                     └──────┬───────────────┘
       │ if primary fails           │ best-effort
       │◄───────────────────────────┘
       ▼
  Phase 1: DIAGNOSIS (each model returns structured analysis)
       │
       ▼
  Phase 2: SOLUTION PROPOSALS (each model proposes ranked solutions)
       │
       ▼
  Phase 3: VOTING (all models rank complete solution combos)
       │    e.g. [A+D, A+C, B+D, B+C] → tally → winner
       ▼
  Phase 4: PLANNING (top 2 models plan winner → synthesize)
       │
       ▼
  Phase 5: IMPLEMENTATION (if auto_fix_enabled → Background_task.enqueue)
```

For fallback models (zai/kimi):
- Query both in parallel
- If one fails, use the other's result alone
- If both succeed, merge diagnoses (combine unique findings)
- Merge prompt: "Given these two analyses, synthesize a combined diagnosis..."

---

## EC Process Lifecycle

The EC process is a long-running companion to the daemon, not a one-shot task. It runs its own scan loop independently.

**Startup:**
- Daemon checks for existing EC process PID file (`~/.clawq/ec_process.pid`)
- If PID file exists and process is alive: initiate graceful handoff (see below)
- Spawn new EC process via `Process_group.start_to_file` (`clawq ec-run --daemon-mode`)
- New process writes its PID to `~/.clawq/ec_process.pid`
- EC process runs its own scan+analyze loop (not relying on daemon to feed it errors)

**Graceful handoff (daemon restart / config reload):**
1. Daemon spawns new EC process
2. New EC process starts up, writes new PID file (atomically: write to `.pid.new`, rename)
3. Daemon sends `SIGUSR2` to old EC process
4. Old EC process receives SIGUSR2:
   - If idle: enters 60-second pause, then exits cleanly
   - If mid-analysis: finishes current work, then enters pause + exit
5. Daemon waits up to 90s for old process to exit, then SIGTERM if still alive

**Multiple EC process safeguards:**
- EC processes use a **lock file** (`~/.clawq/ec_process.lock`) with `flock(LOCK_EX | LOCK_NB)` around DB writes and report writes
- During handoff overlap, both processes can read freely but only the lock holder writes
- Error context files in `~/.clawq/ec_tmp/` are namespaced by PID to avoid conflicts
- EC report filenames include PID to prevent overwrites
- Background fix task enqueuing is idempotent: task branch name includes error hash, `Background_task.enqueue` checks for existing task with same branch before creating

**Shutdown:**
- `SIGTERM` → EC process finishes current work (up to 30s grace), writes final report, exits
- `SIGINT` → immediate clean exit (no in-flight work)
- `SIGUSR2` → pause mode (as described above)

**PID file management:**
- Written on EC process startup
- Removed on clean exit
- Stale PID files detected by checking if PID is alive

## Safety & Error Handling

- **Self-referential loop prevention**: Session scanner excludes `__error_correction__%` and `__postmortem_%` keys
- **Cooldown**: Per-error-pattern cooldown (default 5min) prevents re-triggering
- **Concurrent EC guard**: Lock file ensures only one EC process writes at a time; multiple can coexist during handoff
- **auto_fix_enabled defaults false**: EC only reports by default; must explicitly enable code fixes
- **Non-fatal model failures**: If all models fail in a phase, write partial report and exit cleanly
- **EC commits**: Always in worktrees, never on master, tagged `[INTERNAL_EC]`, automerge disabled
- **Deadlock resilience**: EC is a separate OS process (fork+setsid+execve) — daemon deadlock doesn't affect it

---

## Verification

1. `make build` — compiles cleanly
2. `make fmt-check` — formatting passes
3. `make test` — all existing + new tests pass
4. Manual: start daemon → check logs for "Error correction watcher started"
5. Manual: `clawq watcher status` shows config
6. Manual: `clawq watcher enable` / `clawq watcher disable` toggles
7. Manual: inject error, verify EC process spawned and report written to `~/.clawq/ec_reports/`

---

## Implementation Order

1. Config types + defaults (`runtime_config.ml`) + parsing (`config_loader.ml`) + schema (`config_set.ml`)
2. Core scanner (`error_watcher.ml`) — log parsing, DB queries, dedup, classification, correlation
3. EC process (`ec_process.ml`) — multi-model pipeline, report writing, fix spawning
4. CLI subcommands (`main.ml`) — `watcher` and `ec-run`
5. Command bridge (`command_bridge.ml`, `command_bridge_min.ml`)
6. Daemon integration (`daemon.ml`)
7. Build wiring (`src/dune`, `test/dune`)
8. Tests (`test/test_error_watcher.ml`, `test/test_main.ml`)
9. `make build && make fmt-check && make test`
10. Check `docs/*` for updates needed (haiku subagent per CLAUDE.md)
11. `bl done I019`
12. Run `review-and-fix` skill
