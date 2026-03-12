let rec schedule_autonomous_continuation ?delay ?(around_turn = fun f -> f ())
    ?(on_response = fun _response -> Lwt.return_unit) mgr ~key =
  let delay =
    match delay with
    | Some d -> d
    | None ->
        mgr.Session_core.config.agent_defaults.autonomous_continuation_delay
  in
  let open Lwt.Syntax in
  if not mgr.config.agent_defaults.autonomous_continuation_enabled then
    Lwt.return_unit
  else
    let* should_schedule, cancel_waiter =
      Session_core.with_continuation_state mgr ~key (fun state ->
          if state.Session_core.disarmed then Lwt.return (false, None)
          else begin
            Session_core.clear_pending_continuation state;
            let cancel_waiter, cancel = Lwt.wait () in
            state.cancel <- Some cancel;
            Lwt.return (true, Some cancel_waiter)
          end)
    in
    match (should_schedule, cancel_waiter) with
    | false, _ | _, None -> Lwt.return_unit
    | true, Some cancel_waiter ->
        let* cancelled =
          Lwt.pick
            [
              (let* () = Lwt_unix.sleep delay in
               Lwt.return_false);
              (let* () = cancel_waiter in
               Lwt.return_true);
            ]
        in
        if cancelled then Lwt.return_unit
        else
          let compaction_suggestion =
            Session_core.compaction_suggestion_for_prompt mgr ~key
          in
          let prompt_with_suggestion =
            Session_core.autonomous_continuation_prompt ^ compaction_suggestion
          in
          let* () =
            if mgr.config.agent_defaults.send_continuation_checkin then
              let notify_opt =
                match Session_core.find_silent_channel_notifier mgr ~key with
                | Some _ as s -> s
                | None -> Session_core.find_registered_notifier mgr ~key
              in
              match notify_opt with
              | Some notify ->
                  let labeled =
                    "[automatic continuation check-in]\n"
                    ^ prompt_with_suggestion
                  in
                  Lwt.catch
                    (fun () -> notify labeled)
                    (fun _ -> Lwt.return_unit)
              | None -> Lwt.return_unit
            else Lwt.return_unit
          in
          let run_continuation_turn () =
            Lwt.catch
              (fun () ->
                around_turn (fun () ->
                    Session_turn.turn mgr ~key ~message:prompt_with_suggestion
                      ()))
              (fun exn ->
                Logs.warn (fun m ->
                    m "Autonomous continuation prompt failed for %s: %s" key
                      (Printexc.to_string exn));
                Lwt.return "")
          in
          let* response =
            if mgr.config.agent_defaults.send_continuation_checkin then
              run_continuation_turn ()
            else
              Session_core.with_suppressed_channel_output mgr ~key
                run_continuation_turn
          in
          let trimmed = String.trim response in
          if trimmed = Session_core.queued_message_response then Lwt.return_unit
          else if trimmed = Session_core.autonomous_stay_idle_message then
            Session_core.with_continuation_state mgr ~key (fun state ->
                state.disarmed <- true;
                state.cancel <- None;
                Lwt.return_unit)
          else begin
            let* () =
              Lwt.catch
                (fun () -> on_response trimmed)
                (fun exn ->
                  Logs.warn (fun m ->
                      m "Autonomous continuation on_response failed for %s: %s"
                        key (Printexc.to_string exn));
                  Lwt.return_unit)
            in
            let* () = Session_core.cancel_autonomous_continuation mgr ~key in
            schedule_autonomous_continuation ~delay ~around_turn ~on_response
              mgr ~key
          end

let process_autonomous_turn_result ?delay ?(around_turn = fun f -> f ())
    ?(on_response = fun _response -> Lwt.return_unit) mgr ~key ~response =
  let delay =
    match delay with
    | Some d -> d
    | None ->
        mgr.Session_core.config.agent_defaults.autonomous_continuation_delay
  in
  let trimmed = String.trim response in
  if trimmed = "" || trimmed = "HEARTBEAT_OK" then Lwt.return_unit
  else if trimmed = Session_core.autonomous_stay_idle_message then
    Session_core.with_continuation_state mgr ~key (fun state ->
        state.disarmed <- true;
        Session_core.clear_pending_continuation state;
        Lwt.return_unit)
  else
    schedule_autonomous_continuation ~delay ~around_turn ~on_response mgr ~key
