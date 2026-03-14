(* Generic typing indicator support for all connectors.
   Extracted from telegram_api.ml to enable reuse across Teams, Discord, etc.

   The core pattern: a per-session background loop that watches
   Session.live_activity and calls a connector-specific send_action
   whenever the session is active, refreshing at a configurable interval.
   When the session goes idle, the loop waits; after idle_timeout it exits. *)

type typing_watcher = { refresh : unit -> unit }

let typing_watchers : (string, typing_watcher) Hashtbl.t = Hashtbl.create 64

let rec typing_loop_live_activity ~current_activity ~wait_for_change
    ~wait_for_refresh ~send_action ~interval ~idle_timeout () =
  let open Lwt.Syntax in
  let rec wait_until_active snapshot =
    if snapshot.Session_core.active then keep_active snapshot
    else
      let* next =
        Lwt.pick
          [
            (let* snapshot =
               wait_for_change
                 ~after_generation:snapshot.Session_core.generation
             in
             Lwt.return (`Changed snapshot));
            (let* () = Lwt_unix.sleep idle_timeout in
             Lwt.return `Idle_timeout);
          ]
      in
      match next with
      | `Changed snapshot -> wait_until_active snapshot
      | `Idle_timeout -> Lwt.return_unit
  and keep_active snapshot =
    if not snapshot.Session_core.active then wait_until_active snapshot
    else
      let* () =
        Lwt.catch (fun () -> send_action ()) (fun _exn -> Lwt.return_unit)
      in
      let* next =
        Lwt.pick
          [
            (let* snapshot =
               wait_for_change
                 ~after_generation:snapshot.Session_core.generation
             in
             Lwt.return (`Changed snapshot));
            (let* () =
               Lwt.pick [ Lwt_unix.sleep interval; wait_for_refresh () ]
             in
             let* snapshot = current_activity () in
             Lwt.return (`Tick snapshot));
          ]
      in
      match next with
      | `Changed snapshot -> keep_active snapshot
      | `Tick snapshot -> keep_active snapshot
  in
  let* snapshot = current_activity () in
  wait_until_active snapshot

let ensure_session_typing_watcher ~(session_mgr : Session.t) ~key ~send_action
    ~interval ~idle_timeout =
  match Hashtbl.find_opt typing_watchers key with
  | Some watcher -> watcher
  | None ->
      let refresh_trigger = Lwt_condition.create () in
      let watcher =
        { refresh = (fun () -> Lwt_condition.broadcast refresh_trigger ()) }
      in
      Hashtbl.replace typing_watchers key watcher;
      Lwt.async (fun () ->
          Lwt.finalize
            (fun () ->
              typing_loop_live_activity
                ~current_activity:(fun () ->
                  Session.current_live_activity session_mgr ~key)
                ~wait_for_change:(fun ~after_generation ->
                  Session.wait_for_live_activity_change session_mgr ~key
                    ~after_generation)
                ~wait_for_refresh:(fun () -> Lwt_condition.wait refresh_trigger)
                ~send_action ~interval ~idle_timeout ())
            (fun () ->
              Hashtbl.remove typing_watchers key;
              Lwt.return_unit));
      watcher
