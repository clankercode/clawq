# Restart Mechanism Design

## Signal: SIGUSR1

SIGHUP is already taken for config-reload (daemon.ml:325). SIGUSR1 is the chosen signal for
in-place restart.

Signal registration in daemon.ml alongside existing handlers:

```ocaml
let restart_wakener, restart_wakeup = Lwt.wait () in
let _ =
  Lwt_unix.on_signal Sys.sigusr1 (fun _ ->
    Logs.info (fun m -> m "SIGUSR1 received, initiating graceful restart");
    Lwt.wakeup_later restart_wakeup ())
in
```

The `restart_wakeup` is wired into the main `Lwt.pick` alongside `shutdown_waiter` — but
restart takes a different code path (drain + execv) rather than clean exit.

## Drain Logic

New module `src/restart.ml` (or inline in daemon.ml if small enough):

```
drain_and_restart ~db ~session_manager ~channel_notifiers ~config ()
```

### Drain phases

**Phase 1: Stop ingestion** (immediate)
- Set `draining := true` (global ref checked by all channel message handlers)
- Each channel's message handler: if `!draining` then discard/queue message and return
- This prevents new agent calls from starting

**Phase 2: Warning loop** (concurrent with drain wait)

For each active session in `channel_notifiers` hashtable:
```
t=0s:  "Restarting soon, finishing current requests..."
t=5s:  "Still restarting, please wait (5s)..."
t=10s: "Still restarting (10s)..."
t=15s: "Still restarting (15s)..."
t=30s: "Still restarting (30s)..."
t=45s: "Almost there (45s)..."
t=60s: "Restart timeout reached, forcing restart now."
```

`channel_notifiers`: `(string, string -> unit Lwt.t) Hashtbl.t` mapping session_key to a
send-back function. This table is maintained by the daemon:
- Populated when a new message arrives for a session (each channel module passes its send fn)
- The send fn is specific to the channel/chat_id

**Phase 3: Wait for drain** (up to 60s)

```ocaml
let rec wait_drain n =
  if !in_flight_count = 0 then Lwt.return ()
  else if n = 0 then (
    Logs.warn (fun m -> m "Drain timeout, forcing restart with %d requests in flight" !in_flight_count);
    Lwt.return ()
  ) else (
    let%lwt () = Lwt_unix.sleep 0.1 in
    wait_drain (n - 1)
  )
in
let%lwt () = wait_drain 600 in  (* 60s / 0.1s = 600 ticks *)
```

`in_flight_count`: atomic int ref incremented before LLM call, decremented after response sent.

**Phase 4: Flush state**

```ocaml
(* Flush all in-memory session state to DB *)
Session_manager.flush_all_to_db session_manager;
(* MCP clients disconnect (same as SIGTERM path) *)
List.iter Mcp_client.disconnect !mcp_clients;
(* Telemetry flush *)
Telemetry.flush ();
(* Audit log entry *)
Audit.log db (DaemonEvent { action = "daemon restart"; details = "execv" });
```

**Phase 5: execv**

```ocaml
Unix.putenv "CLAWQ_DAEMON_NOFORK" "1";
(* execv replaces current process image, same PID, same FDs *)
Unix.execv Sys.executable_name [| Sys.executable_name; "service"; "start" |]
(* Never returns *)
```

Note: `Sys.executable_name` is the path to the currently-running binary. After `make build`,
this path contains the new binary. execv loads it.

## CLAWQ_DAEMON_NOFORK in service.ml

Modify `cmd_start` in `src/service.ml`:

```ocaml
let cmd_start ~config =
  (* Check if we're being exec'd back by a self-restarting daemon *)
  if Sys.getenv_opt "CLAWQ_DAEMON_NOFORK" = Some "1" then begin
    (* Already a daemon: stdin=/dev/null, stdout/stderr=logfile, PID file valid *)
    (* Unset the env var so child processes don't inherit it *)
    Unix.putenv "CLAWQ_DAEMON_NOFORK" "";
    Logs.info (fun m -> m "Daemon restarting in-place (NOFORK mode)");
    (try Lwt_main.run (Daemon.run ~config) with _ -> ());
    ""  (* cmd_start return type is string *)
  end else begin
    (* Normal path: check not already running, fork, daemonize *)
    ...existing logic...
  end
```

The NOFORK path:
- Does NOT write a new PID file (PID is unchanged after execv)
- Does NOT redirect FDs (already done by original fork)
- Does NOT call `setsid` (already in a new session)
- Calls `Lwt_main.run (Daemon.run ~config)` directly

## On-Startup Session Resume in daemon.ml

At the top of `Daemon.run`, after opening the DB and before starting channels:

```ocaml
let%lwt () =
  let pending = Session_persistence.load_pending_agent_sessions
    ~db:(Some db) ~max_age_seconds:3600 in
  if pending <> [] then
    Logs.info (fun m -> m "Resuming %d pending agent sessions" (List.length pending));
  Lwt_list.iter_p
    (fun (session_key, channel_opt, channel_id_opt) ->
      match channel_opt, channel_id_opt with
      | Some channel, Some channel_id ->
        let%lwt () = resume_agent_session
          ~session_manager ~session_key ~channel ~channel_id ~config in
        Lwt.return ()
      | _ ->
        Logs.warn (fun m -> m "Cannot resume session %s: missing channel info" session_key);
        (* Mark as abandoned to avoid infinite retry *)
        Session_persistence.mark_response_sent ~db:(Some db) ~session_key ();
        Lwt.return ())
    pending
in
```

`resume_agent_session` reconstructs the agent (history already in DB), invokes the LLM, and
routes the response back via the appropriate channel send function.

## clawq service signal-restart CLI command

In `command_bridge.ml`, extend the `service` subcommand dispatch:

```
"signal-restart" -> Service.cmd_signal_restart ()
```

In `service.ml`:
```ocaml
let cmd_signal_restart () =
  match read_pid () with
  | None -> "Daemon is not running"
  | Some pid ->
    Unix.kill pid Sys.sigusr1;
    "Restart signal sent to daemon (PID " ^ string_of_int pid ^ ")"
```

## Makefile target

```makefile
restart: ## Restart the running daemon (sends signal, loads new binary)
	$(CLAWQ_BIN) service signal-restart

build-restart: build restart ## Build and restart daemon
```

Where `CLAWQ_BIN` defaults to `./_build/default/src/main.exe` (or wherever the built binary is).

## Interaction with Existing `service restart` command

The existing `clawq service restart` = `cmd_stop` + `sleep 1.0` + `cmd_start` is NOT changed.
It remains available as a "hard restart" (kills the process, starts a new one, different PID).

The new `signal-restart` is the "soft restart" (in-place execv, same PID, state preserved).

`make restart` uses `signal-restart` by default (preferred in normal dev workflow).

## Drain Visibility: channel_notifiers Registry

The daemon needs to send warning messages to active users during drain. Architecture:

```ocaml
(* In daemon.ml *)
let channel_notifiers : (string, string -> unit Lwt.t) Hashtbl.t =
  Hashtbl.create 16
```

Each channel module, when it dispatches a message to a session, calls:
```ocaml
Hashtbl.replace channel_notifiers session_key (fun msg ->
  (* e.g., for Telegram: *)
  Telegram.send_message ~bot_token ~chat_id ~text:msg)
```

This can be passed into channel start functions or threaded through session.ml.
The simplest approach: add an optional `on_session_active` callback to channel start signatures.

Note: B499 extended this pattern beyond drain warnings. Background task completion turns (`inject_background_task_completion` in `daemon_util.ml`) now also register a channel notifier via `with_registered_notifier` so that tool call visibility (ToolStart/ToolResult) is sent to channels during automated bg-task follow-up turns. Additionally, `drain_queued_messages_loop` now preserves messages when no notifier is registered (re-queues instead of dropping), ensuring at-least-once delivery.
