include Session_types

let autonomous_stay_idle_message = "STAY_IDLE"

(* STAY_IDLE remains a valid hidden control response for runtime logic, but do
   not mention or spell it out in visible autonomous check-in/keepalive prompt
   text. Advertising the token makes the agent over-index on idling and tempts
   future prompt edits to reintroduce the same behavioral bug. *)
let autonomous_continuation_prompt =
  "Autonomous session check-in: continue working if more remains."

let keepalive_nudge_prompt =
  "[Automated Keepalive Check-In]\n\
   Continue working on your tasks if any remain."

let default_autonomous_continuation_delay = 90.0

let create_live_activity_state () =
  let changed, wake_changed = Lwt.wait () in
  { active_scopes = 0; generation = 0; changed; wake_changed }

let live_activity_state mgr ~key =
  match Hashtbl.find_opt mgr.live_activity key with
  | Some state -> state
  | None ->
      let state = create_live_activity_state () in
      Hashtbl.replace mgr.live_activity key state;
      state

let snapshot_live_activity state =
  { active = state.active_scopes > 0; generation = state.generation }

let advance_live_activity state =
  let prev_wake = state.wake_changed in
  let changed, wake_changed = Lwt.wait () in
  state.generation <- state.generation + 1;
  state.changed <- changed;
  state.wake_changed <- wake_changed;
  Lwt.wakeup_later prev_wake ()

let continuation_state mgr ~key =
  match Hashtbl.find_opt mgr.continuation_checks key with
  | Some state -> state
  | None ->
      let state = { cancel = None; disarmed = false } in
      Hashtbl.replace mgr.continuation_checks key state;
      state

let clear_pending_continuation state =
  match state.cancel with
  | Some cancel ->
      Lwt.wakeup_later cancel ();
      state.cancel <- None
  | None -> ()

let with_continuation_state mgr ~key f =
  Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
    ~label:"sessions_lock" mgr.sessions_lock (fun () ->
      f (continuation_state mgr ~key))

let cancel_autonomous_continuation mgr ~key =
  with_continuation_state mgr ~key (fun state ->
      clear_pending_continuation state;
      Lwt.return_unit)

let mark_autonomous_activity_started mgr ~key =
  with_continuation_state mgr ~key (fun state ->
      state.disarmed <- false;
      clear_pending_continuation state;
      Lwt.return_unit)

let current_live_activity mgr ~key =
  Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
    ~label:"sessions_lock" mgr.sessions_lock (fun () ->
      let state = live_activity_state mgr ~key in
      Lwt.return (snapshot_live_activity state))

let rec wait_for_live_activity_change mgr ~key ~after_generation =
  let open Lwt.Syntax in
  let* snapshot, changed =
    Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
      ~label:"sessions_lock" mgr.sessions_lock (fun () ->
        let state = live_activity_state mgr ~key in
        Lwt.return (snapshot_live_activity state, state.changed))
  in
  if snapshot.generation <> after_generation then Lwt.return snapshot
  else
    let* () = changed in
    wait_for_live_activity_change mgr ~key ~after_generation

let start_live_activity mgr ~key =
  Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
    ~label:"sessions_lock" mgr.sessions_lock (fun () ->
      let state = live_activity_state mgr ~key in
      let was_inactive = state.active_scopes = 0 in
      state.active_scopes <- state.active_scopes + 1;
      if was_inactive then advance_live_activity state;
      Lwt.return_unit)

let stop_live_activity mgr ~key =
  Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
    ~label:"sessions_lock" mgr.sessions_lock (fun () ->
      let state = live_activity_state mgr ~key in
      if state.active_scopes > 0 then begin
        state.active_scopes <- state.active_scopes - 1;
        if state.active_scopes = 0 then advance_live_activity state
      end;
      Lwt.return_unit)

let with_live_activity mgr ~key f =
  let open Lwt.Syntax in
  let* () = start_live_activity mgr ~key in
  Lwt.finalize f (fun () -> stop_live_activity mgr ~key)
