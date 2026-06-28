(* Telegram long-polling loop and update dispatch *)

let dispatch_update ~bot_token ~(account : Runtime_config.telegram_account)
    ~(session_mgr : Session.t) ?run_update_command ?chat_limiter update =
  Lwt.catch
    (fun () ->
      Telegram.handle_update ~bot_token ~account ~session_mgr
        ?run_update_command ?chat_limiter update)
    (fun exn ->
      Logs.err (fun m ->
          m "Telegram: handle_update error for update_id=%d: %s"
            update.update_id (Printexc.to_string exn));
      Lwt.return_unit)

let flush_pending_text_update ~key ~bot_token
    ~(account : Runtime_config.telegram_account) ~(session_mgr : Session.t)
    ?run_update_command ?chat_limiter () =
  match Hashtbl.find_opt Telegram.pending_text_updates key with
  | None -> Lwt.return_unit
  | Some pending ->
      Hashtbl.remove Telegram.pending_text_updates key;
      dispatch_update ~bot_token ~account ~session_mgr ?run_update_command
        ?chat_limiter pending.update

let schedule_pending_text_flush ~key ~bot_token
    ~(account : Runtime_config.telegram_account) ~(session_mgr : Session.t)
    ?run_update_command ?chat_limiter generation =
  Lwt.async (fun () ->
      let open Lwt.Syntax in
      let* () = Lwt_unix.sleep !Telegram.text_coalesce_window_seconds in
      match Hashtbl.find_opt Telegram.pending_text_updates key with
      | Some pending
        when pending.generation = generation
             && Unix.gettimeofday () -. pending.last_seen_at
                >= !Telegram.text_coalesce_window_seconds ->
          Hashtbl.remove Telegram.pending_text_updates key;
          dispatch_update ~bot_token ~account ~session_mgr ?run_update_command
            ?chat_limiter pending.update
      | _ -> Lwt.return_unit)

let buffer_or_dispatch_update ~bot_token
    ~(account : Runtime_config.telegram_account) ~(session_mgr : Session.t)
    ?run_update_command ?chat_limiter update =
  let open Lwt.Syntax in
  let now = Unix.gettimeofday () in
  let key = Telegram.text_coalesce_key ~bot_token update in
  if
    (not (Telegram.is_text_coalescing_candidate update))
    || !Telegram.text_coalesce_window_seconds <= 0.0
  then begin
    let* () =
      flush_pending_text_update ~key ~bot_token ~account ~session_mgr
        ?run_update_command ?chat_limiter ()
    in
    dispatch_update ~bot_token ~account ~session_mgr ?run_update_command
      ?chat_limiter update
  end
  else
    match Hashtbl.find_opt Telegram.pending_text_updates key with
    | Some pending when Telegram.can_coalesce_text_updates ~now pending update
      ->
        pending.update <- Telegram.merge_text_updates pending update;
        pending.last_seen_at <- now;
        pending.generation <- pending.generation + 1;
        schedule_pending_text_flush ~key ~bot_token ~account ~session_mgr
          ?run_update_command ?chat_limiter pending.generation;
        Lwt.return_unit
    | Some _ ->
        let* () =
          flush_pending_text_update ~key ~bot_token ~account ~session_mgr
            ?run_update_command ?chat_limiter ()
        in
        let pending = Telegram.{ update; last_seen_at = now; generation = 0 } in
        Hashtbl.replace Telegram.pending_text_updates key pending;
        schedule_pending_text_flush ~key ~bot_token ~account ~session_mgr
          ?run_update_command ?chat_limiter pending.generation;
        Lwt.return_unit
    | None ->
        let pending = Telegram.{ update; last_seen_at = now; generation = 0 } in
        Hashtbl.replace Telegram.pending_text_updates key pending;
        schedule_pending_text_flush ~key ~bot_token ~account ~session_mgr
          ?run_update_command ?chat_limiter pending.generation;
        Lwt.return_unit

let poll_account ~bot_token ~(account : Runtime_config.telegram_account) ~name
    ~(session_mgr : Session.t) ?run_update_command ?chat_limiter ~stop () =
  let open Lwt.Syntax in
  Logs.info (fun m -> m "Starting Telegram polling for account '%s'" name);
  let* () =
    Lwt.catch
      (fun () -> Telegram.set_my_commands ~bot_token)
      (fun exn ->
        Logs.warn (fun m ->
            m "Telegram: setMyCommands failed for '%s': %s" name
              (Printexc.to_string exn));
        Lwt.return_unit)
  in
  let offset = ref 0 in
  let poll_count = ref 0 in
  let conflict_backoff = ref 5.0 in
  let rec poll () =
    if not (Lwt.is_sleeping stop) then begin
      Logs.info (fun m ->
          m "Telegram: poller for '%s' stopping (stop signalled)" name);
      Lwt.return_unit
    end
    else begin
      incr poll_count;
      if !poll_count <= 3 then
        Logs.info (fun m ->
            m "Telegram poll #%d for account '%s'" !poll_count name)
      else if !poll_count = 4 then
        Logs.info (fun m ->
            m "Telegram polling stable, suppressing routine poll logs for '%s'"
              name);
      let poll_start = Unix.gettimeofday () in
      let* poll_result_opt =
        Lwt.pick
          [
            (let* r =
               Lwt.catch
                 (fun () ->
                   Telegram.get_updates ~bot_token ~offset:!offset ~timeout:30)
                 (fun exn ->
                   let msg = Printexc.to_string exn in
                   let contains_substr s sub =
                     let ls = String.length s in
                     let lb = String.length sub in
                     if lb = 0 then true
                     else if lb > ls then false
                     else
                       let limit = ls - lb in
                       let rec loop i =
                         if i > limit then false
                         else if String.sub s i lb = sub then true
                         else loop (i + 1)
                       in
                       loop 0
                   in
                   let is_cancel =
                     exn = Lwt.Canceled || contains_substr msg "Canceled"
                   in
                   (* B636: shutdown / mid-poll cancellation is normal;
                      log at INFO so real errors are not drowned out. *)
                   if is_cancel then
                     Logs.info (fun m ->
                         m
                           "Telegram poll cancelled for '%s' (shutdown or \
                            stop-signal): %s"
                           name msg)
                   else
                     Logs.err (fun m ->
                         m "Telegram poll error for '%s': %s" name msg);
                   let* () =
                     if is_cancel then Lwt.return_unit else Lwt_unix.sleep 5.0
                   in
                   Lwt.return (Telegram.Updates (0, [])))
             in
             Lwt.return (Some r));
            (let* () = Lwt.protected stop in
             Lwt.return None);
          ]
      in
      match poll_result_opt with
      | None ->
          Logs.info (fun m ->
              m "Telegram: poller for '%s' stopping (stop signalled mid-poll)"
                name);
          Lwt.return_unit
      | Some poll_result ->
          let* max_uid, updates =
            match poll_result with
            | Telegram.Updates (max_uid, updates) ->
                conflict_backoff := 5.0;
                Lwt.return (max_uid, updates)
            | Telegram.Poll_error Telegram.Conflict_webhook ->
                Logs.warn (fun m ->
                    m
                      "Telegram: clearing webhook for '%s' before resuming \
                       long-polling"
                      name);
                let* () = Telegram.delete_webhook ~bot_token in
                let* () = Lwt_unix.sleep 2.0 in
                Lwt.return (0, [])
            | Telegram.Poll_error Telegram.Conflict_duplicate_poller ->
                Logs.warn (fun m ->
                    m
                      "Telegram: another poller is active for '%s', backing \
                       off %.0fs"
                      name !conflict_backoff);
                let* () = Lwt_unix.sleep !conflict_backoff in
                conflict_backoff := Float.min (!conflict_backoff *. 2.0) 60.0;
                Lwt.return (0, [])
            | Telegram.Poll_error (Telegram.Other_error _) ->
                let* () = Lwt_unix.sleep 5.0 in
                Lwt.return (0, [])
          in
          if max_uid + 1 > !offset then offset := max_uid + 1;
          let update_count = List.length updates in
          let* () =
            Lwt_list.iter_s
              (fun (update : Telegram.update) ->
                offset := update.update_id + 1;
                if update.message_id > 0 then begin
                  let cur =
                    Option.value ~default:0
                      (Hashtbl.find_opt Telegram.latest_chat_msg_id
                         update.chat_id)
                  in
                  if update.message_id > cur then
                    Hashtbl.replace Telegram.latest_chat_msg_id update.chat_id
                      update.message_id
                end;
                if Telegram.should_process_update update then
                  buffer_or_dispatch_update ~bot_token ~account ~session_mgr
                    ?run_update_command ?chat_limiter update
                else begin
                  Logs.info (fun m ->
                      m
                        "Telegram: ignoring duplicate update update_id=%d \
                         chat_id=%s"
                        update.update_id update.chat_id);
                  Lwt.return_unit
                end)
              updates
          in
          (if !poll_count <= 3 || !poll_count mod 100 = 0 then
             let poll_elapsed_ms =
               (Unix.gettimeofday () -. poll_start) *. 1000.0
             in
             Logs.info (fun m ->
                 m
                   "Telegram poll #%d for '%s': %.0fms elapsed, %d update(s) \
                    received"
                   !poll_count name poll_elapsed_ms update_count));
          let* () =
            let rec drain_callbacks () =
              if Queue.is_empty Telegram.pending_callbacks then Lwt.return_unit
              else
                let cb = Queue.pop Telegram.pending_callbacks in
                if cb.cb_bot_token <> bot_token then begin
                  (* Re-queue callbacks for other accounts *)
                  Queue.push cb Telegram.pending_callbacks;
                  Lwt.return_unit
                end
                else
                  let* () =
                    Lwt.catch
                      (fun () ->
                        match cb.data with
                        | data
                          when String.starts_with
                                 ~prefix:Telegram.details_callback_prefix data
                          ->
                            let text =
                              match
                                Telegram.take_tool_result_details
                                  ~chat_id:cb.cb_chat_id ~user_id:cb.cb_user_id
                                  data
                              with
                              | Some details when String.trim details <> "" ->
                                  details
                              | _ -> "No details available."
                            in
                            let* () =
                              Telegram.answer_callback_query ~bot_token
                                ~callback_query_id:cb.callback_query_id ()
                            in
                            Telegram.send_message ~disable_notification:true
                              ~bot_token ~chat_id:cb.cb_chat_id ~text ()
                        | data -> (
                            (* Check question callbacks first *)
                            let question_key_opt =
                              match
                                Hashtbl.find_opt Telegram.callback_routing data
                              with
                              | Some (sk, _, _) -> Some sk
                              | None ->
                                  (* For question callbacks, derive session
                                     key from chat_id *)
                                  let tg_key =
                                    Printf.sprintf "telegram:%s" cb.cb_chat_id
                                  in
                                  if
                                    Session.has_pending_question session_mgr
                                      ~key:tg_key
                                  then Some tg_key
                                  else None
                            in
                            let resolved_question =
                              match question_key_opt with
                              | Some sk ->
                                  Session.resolve_question_callback session_mgr
                                    ~key:sk ~callback_id:data
                              | None -> false
                            in
                            if resolved_question then begin
                              Logs.debug (fun m ->
                                  m
                                    "Telegram: question callback resolved for \
                                     %s"
                                    data);
                              (* Clean up stale callback_routing entry *)
                              Hashtbl.remove Telegram.callback_routing data;
                              Telegram.answer_callback_query ~bot_token
                                ~callback_query_id:cb.callback_query_id
                                ~text:"Selected" ()
                            end
                            else
                              match
                                Hashtbl.find_opt Telegram.callback_routing data
                              with
                              | Some (session_key, label, _created) ->
                                  Hashtbl.remove Telegram.callback_routing data;
                                  let* () =
                                    Telegram.answer_callback_query ~bot_token
                                      ~callback_query_id:cb.callback_query_id
                                      ~text:
                                        (Printf.sprintf "Selected: %s" label)
                                      ()
                                  in
                                  Lwt.async (fun () ->
                                      Lwt.catch
                                        (fun () ->
                                          let message =
                                            Printf.sprintf "[Button: %s]" label
                                          in
                                          let* response =
                                            Session.turn session_mgr
                                              ~key:session_key ~message
                                              ~channel:"telegram"
                                              ~channel_id:cb.cb_chat_id
                                              ~snapshot_work_type:
                                                Access_snapshot.Room_turn ()
                                          in
                                          if
                                            not
                                              (Session.should_suppress_response
                                                 response)
                                          then
                                            Telegram.send_chunked
                                              ~disable_notification:false
                                              ~parse_mode:"MarkdownV2"
                                              ~bot_token ~chat_id:cb.cb_chat_id
                                              ~text:
                                                (Telegram_format
                                                 .markdown_to_mdv2 response)
                                              ()
                                          else Lwt.return_unit)
                                        (fun exn ->
                                          Logs.err (fun m ->
                                              m
                                                "Telegram: button callback \
                                                 routing error: %s"
                                                (Printexc.to_string exn));
                                          Lwt.return_unit));
                                  Lwt.return_unit
                              | None ->
                                  Telegram.answer_callback_query ~bot_token
                                    ~callback_query_id:cb.callback_query_id
                                    ~text:"Unknown action" ()))
                      (fun exn ->
                        Logs.err (fun m ->
                            m "Telegram: callback handling error: %s"
                              (Printexc.to_string exn));
                        Lwt.return_unit)
                  in
                  drain_callbacks ()
            in
            drain_callbacks ()
          in
          (* Drain poll answers *)
          let* () =
            let rec drain_poll_answers () =
              if Queue.is_empty Telegram.pending_poll_answers then
                Lwt.return_unit
              else
                let pa = Queue.pop Telegram.pending_poll_answers in
                let* () =
                  match
                    Hashtbl.find_opt Telegram.poll_routing pa.pa_poll_id
                  with
                  | Some
                      ( session_key,
                        chat_id,
                        poll_bot_token,
                        options,
                        _created_at ) ->
                      let selected =
                        List.filter_map
                          (fun idx ->
                            if idx >= 0 && idx < List.length options then
                              Some (List.nth options idx)
                            else None)
                          pa.pa_option_ids
                      in
                      if selected = [] then Lwt.return_unit
                      else begin
                        (* Check if this is a question poll answer *)
                        let answer_text = String.concat ", " selected in
                        if
                          Session.has_pending_question session_mgr
                            ~key:session_key
                        then begin
                          (* Resolve the pending question with poll answer *)
                          Logs.debug (fun m ->
                              m
                                "Telegram: resolving question via poll answer: \
                                 %s"
                                answer_text);
                          (match
                             Hashtbl.find_opt session_mgr.pending_questions
                               session_key
                           with
                          | Some resolver ->
                              Hashtbl.remove session_mgr.pending_questions
                                session_key;
                              Lwt.wakeup_later resolver answer_text
                          | None -> ());
                          Lwt.return_unit
                        end
                        else begin
                          Lwt.async (fun () ->
                              Lwt.catch
                                (fun () ->
                                  let message =
                                    Printf.sprintf "[Poll vote: %s]" answer_text
                                  in
                                  let* response =
                                    Session.turn session_mgr ~key:session_key
                                      ~message ~channel:"telegram"
                                      ~channel_id:chat_id
                                      ~snapshot_work_type:
                                        Access_snapshot.Room_turn ()
                                  in
                                  if
                                    not
                                      (Session.should_suppress_response response)
                                  then
                                    Telegram.send_chunked
                                      ~disable_notification:false
                                      ~bot_token:poll_bot_token ~chat_id
                                      ~text:response ()
                                  else Lwt.return_unit)
                                (fun exn ->
                                  Logs.err (fun m ->
                                      m
                                        "Telegram: poll answer routing error: \
                                         %s"
                                        (Printexc.to_string exn));
                                  Lwt.return_unit));
                          Lwt.return_unit
                        end
                      end
                  | None ->
                      Logs.debug (fun m ->
                          m
                            "Telegram: ignoring poll_answer for unknown \
                             poll_id=%s"
                            pa.pa_poll_id);
                      Lwt.return_unit
                in
                drain_poll_answers ()
            in
            drain_poll_answers ()
          in
          (* Periodic cleanup of stale routing entries *)
          if !poll_count mod 100 = 0 then Telegram.cleanup_stale_routing ();
          poll ()
    end
  in
  poll ()

let start_polling ~(config : Runtime_config.t) ~(session_manager : Session.t)
    ?run_update_command ?chat_limiter ~stop () =
  match config.channels.telegram with
  | None ->
      Logs.info (fun m -> m "No Telegram config found, skipping polling");
      Lwt.return_unit
  | Some tg_config -> (
      Telegram.text_coalesce_window_seconds :=
        float_of_int tg_config.text_coalesce_ms /. 1000.0;
      match tg_config.accounts with
      | [] ->
          Logs.info (fun m -> m "No Telegram accounts configured");
          Lwt.return_unit
      | accounts -> (
          let poll_loops =
            List.filter_map
              (fun (name, (account : Runtime_config.telegram_account)) ->
                if account.bot_token = "" then (
                  Logs.info (fun m ->
                      m "Telegram account '%s' has empty bot_token, skipping"
                        name);
                  None)
                else
                  Some
                    (poll_account ~bot_token:account.bot_token ~account ~name
                       ~session_mgr:session_manager ?run_update_command
                       ?chat_limiter ~stop ()))
              accounts
          in
          match poll_loops with
          | [] ->
              Logs.info (fun m -> m "No Telegram accounts with valid bot_token");
              Lwt.return_unit
          | loops -> Lwt.join loops))
