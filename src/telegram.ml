include Telegram_api

let handle_update ~bot_token ~(account : Runtime_config.telegram_account)
    ~(session_mgr : Session.t) ?run_update_command ?chat_limiter
    (update : update) =
  let open Lwt.Syntax in
  (* Check /pair command first (before auth checks) *)
  let trimmed = String.trim update.text in
  let is_pair_cmd =
    String.length trimmed > 6
    && String.lowercase_ascii (String.sub trimmed 0 6) = "/pair "
  in
  if is_pair_cmd then
    let code = String.trim (String.sub trimmed 6 (String.length trimmed - 6)) in
    handle_pair_command ~bot_token ~account ~chat_id:update.chat_id ~code
  else if
    (not (is_allowed ~account ~chat_id:update.chat_id))
    && requires_totp_auth ~account ~chat_id:update.chat_id
  then (
    Logs.warn (fun m ->
        m "Telegram: unauthenticated chat_id=%s, requesting pairing"
          update.chat_id);
    send_message ~bot_token ~chat_id:update.chat_id
      ~text:
        "Please pair first: type `/pair <6-digit-code>`.\n\
         Get the code from `clawq otp-show` command."
      ())
  else if
    (not (is_allowed ~account ~chat_id:update.chat_id))
    && not (is_totp_paired ~chat_id:update.chat_id ~now:(Unix.gettimeofday ()))
  then (
    Logs.warn (fun m ->
        m "Telegram: ignoring message from unauthorized chat_id=%s"
          update.chat_id);
    Lwt.return_unit)
  else
    let* rate_ok =
      match chat_limiter with
      | Some lim -> Rate_limiter.check_and_consume lim ~key:update.chat_id
      | None -> Lwt.return true
    in
    if not rate_ok then begin
      let now = Unix.gettimeofday () in
      let should_warn =
        match Hashtbl.find_opt _rate_limit_warnings update.chat_id with
        | Some last -> now -. last >= 60.0
        | None -> true
      in
      if should_warn then begin
        Hashtbl.replace _rate_limit_warnings update.chat_id now;
        let* () =
          send_message ~bot_token ~chat_id:update.chat_id
            ~text:
              "Please slow down, I can only process a limited number of \
               messages per minute."
            ()
        in
        Lwt.return_unit
      end
      else Lwt.return_unit
    end
    else
      let key = "telegram:" ^ update.chat_id ^ ":" ^ update.user_id in
      let typing_watcher =
        ensure_session_typing_watcher ~session_mgr ~key ~bot_token
          ~chat_id:update.chat_id
      in
      let refresh_typing () = typing_watcher.refresh () in
      (* Register a persistent channel notifier so autonomous continuation
         responses can reach the Telegram chat *)
      let send_to_chat ?(disable_notification = false) text =
        let open Lwt.Syntax in
        let* () =
          send_chunked ~disable_notification ~parse_mode:"MarkdownV2" ~bot_token
            ~chat_id:update.chat_id
            ~text:(Telegram_format.markdown_to_mdv2 text)
            ()
        in
        refresh_typing ();
        Lwt.return_unit
      in
      if Option.is_none (Session.find_registered_notifier session_mgr ~key) then begin
        Session.register_channel_notifier session_mgr ~key send_to_chat;
        Session.register_silent_channel_notifier session_mgr ~key
          (send_to_chat ~disable_notification:true);
        Session.register_status_message_factory session_mgr ~key (fun () ->
            Status_message.create
              ~notifier:
                (make_status_notifier ~bot_token ~chat_id:update.chat_id)
              ~parse_mode:"HTML" ())
      end;
      (* Register rich notifier for inline keyboards and polls *)
      if Option.is_none (Session.find_rich_notifier session_mgr ~key) then
        Session.register_rich_notifier session_mgr ~key (fun msg ->
            let open Lwt.Syntax in
            match msg with
            | Rich_message.Text text ->
                let* () =
                  send_chunked ~parse_mode:"MarkdownV2" ~bot_token
                    ~chat_id:update.chat_id
                    ~text:(Telegram_format.markdown_to_mdv2 text)
                    ()
                in
                refresh_typing ();
                Lwt.return Rich_message.{ message_id = "0"; callback_ids = [] }
            | Rich_message.TextWithButtons { text; button_rows } ->
                let now = Unix.gettimeofday () in
                let buttons =
                  List.concat_map
                    (fun row ->
                      List.map
                        (fun (btn : Rich_message.button) ->
                          (btn.label, btn.callback_id))
                        row)
                    button_rows
                in
                let callback_ids =
                  List.map
                    (fun (label, cb_id) ->
                      Hashtbl.replace callback_routing cb_id (key, label, now);
                      cb_id)
                    buttons
                in
                let* msg_id =
                  send_message_with_keyboard ~disable_notification:false
                    ~bot_token ~chat_id:update.chat_id ~text ~buttons ()
                in
                refresh_typing ();
                Lwt.return Rich_message.{ message_id = msg_id; callback_ids }
            | Rich_message.Poll { question; options; allows_multiple } ->
                let* msg_id, poll_id =
                  send_poll_api ~disable_notification:false ~bot_token
                    ~chat_id:update.chat_id ~question ~options ~allows_multiple
                    ()
                in
                Hashtbl.replace poll_routing poll_id
                  (key, update.chat_id, bot_token, options, Unix.gettimeofday ());
                refresh_typing ();
                Lwt.return
                  Rich_message.{ message_id = msg_id; callback_ids = [] });
      let image_content_parts = ref [] in
      let* user_text =
        match update.voice_file_id with
        | Some file_id ->
            Lwt.catch
              (fun () ->
                let get_file_uri =
                  Printf.sprintf "%s%s/getFile?file_id=%s" api_base bot_token
                    file_id
                in
                let* _status, file_body =
                  Http_client.get ~uri:get_file_uri ~headers:[]
                in
                let file_json = Yojson.Safe.from_string file_body in
                let file_path =
                  Yojson.Safe.Util.(
                    file_json |> member "result" |> member "file_path"
                    |> to_string)
                in
                let download_uri =
                  Printf.sprintf "https://api.telegram.org/file/bot%s/%s"
                    bot_token file_path
                in
                let* _status, audio_data =
                  Http_client.get ~uri:download_uri ~headers:[]
                in
                let filename = Filename.basename file_path in
                let content_type = Stt.content_type_of_ext filename in
                let config = Session.get_config session_mgr in
                let* result =
                  Stt.transcribe ~config ~audio_data ~filename ~content_type ()
                in
                Lwt.return ("[Voice]: " ^ result.text))
              (fun exn ->
                Logs.err (fun m ->
                    m "Voice transcription failed: %s" (Printexc.to_string exn));
                Lwt.return "")
        | None -> (
            (* Determine image file_id from photo, sticker, or image document *)
            let image_file_id =
              match update.photo_file_id with
              | Some fid -> Some fid
              | None -> (
                  match update.sticker_file_id with
                  | Some fid -> Some fid
                  | None -> (
                      match
                        (update.document_file_id, update.document_mime_type)
                      with
                      | Some fid, Some mt
                        when String.length mt >= 6
                             && String.sub mt 0 6 = "image/" ->
                          Some fid
                      | _ -> None))
            in
            match image_file_id with
            | Some file_id ->
                Lwt.catch
                  (fun () ->
                    let* image_data =
                      download_telegram_file ~bot_token ~file_id
                    in
                    let media_type = detect_mime_type image_data in
                    let b64 = Base64.encode_exn image_data in
                    let text =
                      match update.caption with
                      | Some c -> c
                      | None -> "[Image]"
                    in
                    image_content_parts :=
                      [ Provider.Image_base64 { data = b64; media_type } ];
                    Lwt.return text)
                  (fun exn ->
                    Logs.err (fun m ->
                        m "Image download failed: %s" (Printexc.to_string exn));
                    let cap =
                      match update.caption with
                      | Some c -> " — " ^ c
                      | None -> ""
                    in
                    if update.photo_file_id <> None then
                      Lwt.return ("[Photo received" ^ cap ^ "]")
                    else if update.sticker_file_id <> None then
                      Lwt.return ("[Sticker received" ^ cap ^ "]")
                    else Lwt.return ("[Image document received" ^ cap ^ "]"))
            | None -> (
                match update.document_file_id with
                | Some _ ->
                    let name =
                      match update.document_name with
                      | Some n -> ": " ^ n
                      | None -> ""
                    in
                    let cap =
                      match update.caption with
                      | Some c -> " — " ^ c
                      | None -> ""
                    in
                    Lwt.return ("[Document" ^ name ^ cap ^ "]")
                | None -> Lwt.return update.text))
      in
      if user_text = "" then Lwt.return_unit
      else if Update_tool.is_update_command user_text then (
        let send_first text =
          send_message_with_id ~disable_notification:true ~bot_token
            ~chat_id:update.chat_id ~text ()
        in
        let edit msg_id text =
          edit_message ~bot_token ~chat_id:update.chat_id ~message_id:msg_id
            ~text ()
        in
        let send_progress, _get_final =
          Update_tool.make_progress_sender ~send_first ~edit
            ~mode:Update_tool.Auto ()
        in
        let run_update_command =
          match run_update_command with
          | Some run_update_command -> run_update_command
          | None ->
              fun ?(mode = Update_tool.Auto)
                ?prepare_restart
                ~send_progress
                ()
              ->
                Update_tool.run_update ?prepare_restart ~mode
                  ~is_draining:(fun () -> Session.is_draining session_mgr)
                  ~send_progress ()
        in
        (* Eagerly acknowledge this update before starting the build.
           Without this, if a concurrent /update is rejected by claim_update and
           exec-restart then races with the normal poll-advance cycle, the rejected
           message can be re-delivered to the new daemon, triggering a redundant
           build.  Ignore failures — the prepare_restart path below is the safety
           valve that will abort the restart if the final ack fails. *)
        let* _ =
          Lwt.catch
            (fun () ->
              acknowledge_update ~bot_token ~update_id:update.update_id)
            (fun _ -> Lwt.return (Ok ()))
        in
        Logs.info (fun m ->
            m "Telegram: /update command from chat_id=%s, initiating update"
              update.chat_id);
        let* _response =
          run_update_command
            ~prepare_restart:(fun () ->
              Restart_notify.write ~channel:"telegram"
                ~channel_id:update.chat_id;
              acknowledge_update ~bot_token ~update_id:update.update_id)
            ~send_progress ()
        in
        Lwt.return_unit)
      else
        match Slash_commands.handle user_text with
        | Reply text -> send_message ~bot_token ~chat_id:update.chat_id ~text ()
        | Reset ->
            let* active_bg_tasks = Session.reset session_mgr ~key in
            send_message ~bot_token ~chat_id:update.chat_id
              ~text:(Slash_commands.reset_message ~active_bg_tasks ())
              ()
        | Compact -> (
            let notifier =
              make_status_notifier ~bot_token ~chat_id:update.chat_id
            in
            let* compact_result =
              Session.compact session_mgr ~key ~notifier ()
            in
            match compact_result with
            | Ok _ ->
                (* Progress/result message handled by session.compact via notifier *)
                Lwt.return_unit
            | Error err ->
                send_message ~bot_token ~chat_id:update.chat_id
                  ~text:(Printf.sprintf "Compaction failed: %s" err)
                  ())
        | RuntimeCtx ->
            let* text = Session.runtime_context_block session_mgr ~key in
            send_message ~bot_token ~chat_id:update.chat_id ~text ()
        | Thinking Slash_commands.ShowThinking ->
            let current =
              (Session.get_config session_mgr).agent_defaults.reasoning_effort
            in
            send_message ~bot_token ~chat_id:update.chat_id
              ~text:(current_thinking_message current)
              ()
        | Thinking (Slash_commands.SetThinking level) ->
            send_message ~bot_token ~chat_id:update.chat_id
              ~text:
                (set_thinking_level ~session_mgr ~chat_id:update.chat_id
                   ~user_id:update.user_id level)
              ()
        | ShowThinking action ->
            let cfg = Session.get_config session_mgr in
            let current = cfg.agent_defaults.show_thinking in
            let text =
              match action with
              | Slash_commands.ShowThinkingStatus ->
                  Printf.sprintf "Show thinking: %s"
                    (if current then "on" else "off")
              | Slash_commands.ToggleShowThinking -> (
                  let new_val = not current in
                  match Config_set.set_show_thinking new_val with
                  | Ok () ->
                      let agent_defaults =
                        { cfg.agent_defaults with show_thinking = new_val }
                      in
                      Session.update_config ~source:"telegram" session_mgr
                        { cfg with agent_defaults };
                      Logs.info (fun m ->
                          m
                            "Telegram show_thinking toggled chat_id=%s \
                             user_id=%s from=%b to=%b"
                            update.chat_id update.user_id current new_val);
                      Printf.sprintf "Show thinking: %s"
                        (if new_val then "on" else "off")
                  | Error err -> "Failed to update show_thinking: " ^ err)
            in
            send_message ~bot_token ~chat_id:update.chat_id ~text ()
        | Delegate prompt ->
            let* () =
              send_message ~bot_token ~chat_id:update.chat_id
                ~text:"Delegating to a temporary session..." ()
            in
            let tg_prompt = telegram_delegate_prompt ~user_prompt:prompt in
            Session.delegate_turn session_mgr ~prompt:tg_prompt
              ~send_reply:(fun text ->
                send_chunked_html_with_fallback ~disable_notification:false
                  ~bot_token ~chat_id:update.chat_id ~text ());
            Lwt.return_unit
        | Tools ->
            let text =
              match Session.get_tool_registry session_mgr with
              | Some reg ->
                  let tools, skills = Tool_registry.partition_skills reg in
                  Slash_commands.format_tools_telegram tools skills
              | None -> "Tools are not enabled."
            in
            send_message ~bot_token ~chat_id:update.chat_id ~text
              ~parse_mode:"HTML" ()
        | Tasks ->
            let text =
              match Session.get_db session_mgr with
              | Some db ->
                  Task_tree.init_schema db;
                  Task_tree.render_tree_with_legend ~db ~session_key:key
              | None -> "Tasks are not available (no database)."
            in
            send_message ~bot_token ~chat_id:update.chat_id ~text ()
        | Model action -> (
            let open Slash_commands in
            match action with
            | ModelShow ->
                let current =
                  Session.get_session_effective_model session_mgr ~key
                in
                let prefs = Model_preferences.load () in
                let usage_ranked =
                  List.filter_map
                    (fun (m, c) ->
                      if List.mem m prefs.favorites then None else Some (m, c))
                    prefs.usage_counts
                in
                let text =
                  format_model_show_telegram ~current ~favorites:prefs.favorites
                    ~usage_ranked
                in
                send_message ~bot_token ~chat_id:update.chat_id ~text
                  ~parse_mode:"HTML" ()
            | ModelSet name -> (
                let provider, model_id, fmt = Models_catalog.split_name name in
                match fmt with
                | Models_catalog.Canonical | Models_catalog.Legacy ->
                    let hint =
                      match fmt with
                      | Models_catalog.Legacy ->
                          Printf.sprintf
                            "\nHint: use %s:%s format instead of %s/%s."
                            provider model_id provider model_id
                      | _ -> ""
                    in
                    let cfg = Session.get_config session_mgr in
                    let provider_in_config =
                      List.mem_assoc provider cfg.providers
                    in
                    let warn =
                      if not provider_in_config then
                        Printf.sprintf
                          "\n\
                           Warning: provider '%s' not found in config. Add it \
                           to your config.json to use this model."
                          provider
                      else ""
                    in
                    Session.set_session_model session_mgr ~key ~model:name;
                    send_message ~bot_token ~chat_id:update.chat_id
                      ~text:
                        (Printf.sprintf
                           "Model set to: %s (provider: %s)%s%s\n\
                            Persisted for this session across restarts. Use \
                            /model set-default to change the global default."
                           model_id provider hint warn)
                      ()
                | Models_catalog.Plain -> (
                    let model_info = Models_catalog.find_by_full_name name in
                    match model_info with
                    | None ->
                        let text =
                          Printf.sprintf
                            "Warning: '%s' not found in model catalog. Setting \
                             anyway.\n\
                             Persisted for this session across restarts. Use \
                             /model set-default to change the global default."
                            name
                        in
                        Session.set_session_model session_mgr ~key ~model:name;
                        send_message ~bot_token ~chat_id:update.chat_id ~text ()
                    | Some m ->
                        Session.set_session_model session_mgr ~key ~model:name;
                        let display =
                          if m.Models_catalog.provider <> "" then
                            Printf.sprintf
                              "Model set to: %s (provider: %s)\n\
                               Persisted for this session across restarts. Use \
                               /model set-default to change the global \
                               default."
                              m.Models_catalog.id m.Models_catalog.provider
                          else
                            Printf.sprintf
                              "Model set to: %s\n\
                               Persisted for this session across restarts. Use \
                               /model set-default to change the global \
                               default."
                              name
                        in
                        send_message ~bot_token ~chat_id:update.chat_id
                          ~text:display ()))
            | ModelSetDefault name -> (
                let provider, model_id, fmt = Models_catalog.split_name name in
                let hint =
                  match fmt with
                  | Models_catalog.Legacy ->
                      Printf.sprintf "\nHint: use %s:%s format instead."
                        provider model_id
                  | _ -> ""
                in
                let result =
                  Config_set.set_json_value "agent_defaults.primary_model"
                    (`String name)
                in
                match result with
                | Error e ->
                    send_message ~bot_token ~chat_id:update.chat_id
                      ~text:(Printf.sprintf "Error writing config: %s" e)
                      ()
                | Ok () ->
                    let msg =
                      match fmt with
                      | Models_catalog.Canonical | Models_catalog.Legacy ->
                          Printf.sprintf
                            "Default model set to: %s (provider: %s)%s\n\
                             Applies to new sessions."
                            model_id provider hint
                      | Models_catalog.Plain ->
                          Printf.sprintf
                            "Default model set to: %s\nApplies to new sessions."
                            name
                    in
                    send_message ~bot_token ~chat_id:update.chat_id ~text:msg ()
                )
            | ModelFav name ->
                let prefs = Model_preferences.toggle_favorite name in
                let status =
                  if List.mem name prefs.favorites then "added to"
                  else "removed from"
                in
                send_message ~bot_token ~chat_id:update.chat_id
                  ~text:(Printf.sprintf "%s %s favorites" name status)
                  ()
            | ModelUnfav name ->
                let _ = Model_preferences.remove_favorite name in
                send_message ~bot_token ~chat_id:update.chat_id
                  ~text:(Printf.sprintf "Removed from favorites: %s" name)
                  ()
            | ModelList provider ->
                let db_extras =
                  match Session.get_db session_mgr with
                  | None -> []
                  | Some db ->
                      Model_discovery.get_db_only_models ~db
                        ~provider_filter:provider
                in
                let models =
                  Models_catalog.to_plain_list ~provider_filter:provider
                    ~db_extras ()
                  |> String.split_on_char '\n'
                  |> List.filter (fun s -> s <> "")
                in
                let text = format_model_list_telegram ~models ~provider in
                send_message ~bot_token ~chat_id:update.chat_id ~text
                  ~parse_mode:"HTML" ()
            | ModelUsage ->
                let cfg = Session.get_config session_mgr in
                Provider_quota.set_cache_ttl cfg.quota_cache_ttl_s;
                let results = Provider_quota.get_all_cached () in
                let lines =
                  List.map
                    (fun (name, pq) ->
                      let summary = Provider_quota.to_summary_string pq in
                      let threshold =
                        match List.assoc_opt name cfg.providers with
                        | Some pc ->
                            Option.value ~default:0.85 pc.quota_threshold
                        | None -> 0.85
                      in
                      let label = Provider_quota.status_label ~threshold pq in
                      summary ^ "  " ^ label)
                    results
                in
                let text =
                  if lines = [] then "No providers configured."
                  else
                    "<b>Provider Quota/Usage</b>\n\n" ^ String.concat "\n" lines
                in
                send_message ~bot_token ~chat_id:update.chat_id ~text
                  ~parse_mode:"HTML" ())
        | ForkAnd prompt ->
            let* () =
              send_message ~bot_token ~chat_id:update.chat_id
                ~text:"Forking session..." ()
            in
            let tg_prompt = telegram_delegate_prompt ~user_prompt:prompt in
            Session.fork_and_run session_mgr ~parent_key:key ~prompt:tg_prompt
              ~send_reply:(fun text ->
                send_chunked_html_with_fallback ~disable_notification:false
                  ~bot_token ~chat_id:update.chat_id ~text ());
            Lwt.return_unit
        | NotACommand -> (
            let msg = user_text in
            (* Early busy-session fast path: if the session is already busy,
               enqueue immediately without the reaction HTTP call or UI setup.
               This avoids ~1s+ of latency from the setMessageReaction API call
               and other setup that is unnecessary for queued messages. *)
            let normalized_msg =
              if String.length msg > 0 && msg.[0] = '!' then
                let raw = String.sub msg 1 (String.length msg - 1) in
                if String.trim raw = "" then "[interrupted]" else raw
              else msg
            in
            let* () =
              if String.length msg > 0 && msg.[0] = '!' then
                Session.set_interrupt_if_present session_mgr ~key normalized_msg
              else Lwt.return_unit
            in
            let* early_queued =
              Session.enqueue_message_if_busy session_mgr ~key
                ({
                   message = normalized_msg;
                   content_parts = !image_content_parts;
                   attachments = [];
                   channel_name = Some "telegram";
                   channel_type = Some "dm";
                   sender_id = None;
                   sender_name = None;
                   channel = Some "telegram";
                   channel_id = Some update.chat_id;
                   message_id = Some (string_of_int update.message_id);
                   inbound_queue_id = None;
                 }
                  : Session.queued_message)
            in
            if early_queued then Lwt.return_unit
            else
              let agent_defaults =
                (Session.get_config session_mgr).agent_defaults
              in
              let use_consolidated =
                agent_defaults.show_tool_calls
                && agent_defaults.tool_status_mode = "consolidated"
              in
              let current_turn_has_tools = ref false in
              let current_turn_tool_details = ref [] in
              let tool_reaction_set = ref false in
              let peers =
                Reaction_tracker.get_or_create_peers reactions ~key
                  ~initial:update.message_id
              in
              Reaction_tracker.add_peer reactions ~key
                ~message_id:update.message_id;
              let set_reaction emoji =
                Reaction_tracker.set_reaction_all reactions ~peers_ref:peers
                  ~set_one:(fun mid e ->
                    Lwt.catch
                      (fun () ->
                        set_message_reaction ~bot_token ~chat_id:update.chat_id
                          ~message_id:mid ~emoji:e ())
                      (fun _exn -> Lwt.return_unit))
                  ~emoji
              in
              let set_reaction_on mid emoji =
                Lwt.catch
                  (fun () ->
                    set_message_reaction ~bot_token ~chat_id:update.chat_id
                      ~message_id:mid ~emoji ())
                  (fun _exn -> Lwt.return_unit)
              in
              let thinking_buf = Buffer.create 256 in
              let drain_progress_msg_id = ref None in
              let status_msg =
                if use_consolidated then
                  let status_notifier =
                    make_status_notifier ~bot_token ~chat_id:update.chat_id
                  in
                  Some
                    (Status_message.create ~notifier:status_notifier
                       ~parse_mode:"HTML" ())
                else None
              in
              (match status_msg with
              | Some sm ->
                  Session.register_interrupt_finalizer session_mgr ~key
                    (fun () ->
                      let open Lwt.Syntax in
                      let* () = Status_message.finalize sm in
                      let* mid =
                        Lwt.catch
                          (fun () ->
                            send_message_with_id ~disable_notification:true
                              ~bot_token ~chat_id:update.chat_id
                              ~text:
                                "\xe2\x8f\xb3 Processing new \
                                 message\xe2\x80\xa6"
                              ())
                          (fun _ -> Lwt.return "0")
                      in
                      if mid <> "0" && mid <> "" then
                        drain_progress_msg_id := Some mid;
                      Lwt.return_unit)
              | None -> ());
              let visibility = Stream_visibility.create () in
              let tool_start_times : (string, float * string option) Hashtbl.t =
                Hashtbl.create 8
              in
              let send_expandable ~name ~result ~is_error =
                if is_error then
                  let formatted = Telegram_format.format_error_trace result in
                  send_chunked ~disable_notification:true
                    ~parse_mode:"MarkdownV2" ~bot_token ~chat_id:update.chat_id
                    ~text:formatted ()
                else
                  match
                    Telegram_format.format_sensitive_result ~name result
                  with
                  | Some formatted ->
                      send_chunked ~disable_notification:true
                        ~parse_mode:"MarkdownV2" ~bot_token
                        ~chat_id:update.chat_id ~text:formatted ()
                  | None -> (
                      match
                        Telegram_format.format_verbose_result ~name result
                      with
                      | Some formatted ->
                          send_chunked ~disable_notification:true
                            ~parse_mode:"MarkdownV2" ~bot_token
                            ~chat_id:update.chat_id ~text:formatted ()
                      | None -> Lwt.return_unit)
              in
              let on_chunk chunk =
                match status_msg with
                | Some sm -> (
                    match chunk with
                    | Provider.ToolStart { id; name; arguments } ->
                        let* () =
                          if not !tool_reaction_set then begin
                            tool_reaction_set := true;
                            set_reaction reaction_emoji_tools
                          end
                          else Lwt.return_unit
                        in
                        let summary =
                          Stream_visibility.summarize_tool_arguments ~name
                            arguments
                        in
                        let* () =
                          Status_message.tool_start sm ~id ~name ~summary
                        in
                        let action = chat_action_for_tool name in
                        let* () =
                          Lwt.catch
                            (fun () ->
                              send_chat_action ~bot_token
                                ~chat_id:update.chat_id ~action)
                            (fun _exn -> Lwt.return_unit)
                        in
                        refresh_typing ();
                        Lwt.return_unit
                    | Provider.ToolResult { id; name; result; is_error } ->
                        let open Lwt.Syntax in
                        let* () =
                          Status_message.tool_result sm ~id ~name ~result
                            ~is_error
                        in
                        refresh_typing ();
                        current_turn_tool_details :=
                          format_tool_result_detail ~name ~result
                          :: !current_turn_tool_details;
                        current_turn_has_tools := true;
                        (* Only send inline messages for errors; non-error
                         output is available via "Show Details" button *)
                        if is_error then (
                          let info = Status_message.get_tool_info sm ~id in
                          let emoji =
                            Option.fold ~none:"\xE2\x9C\x97"
                              ~some:(fun (e : Status_message.tool_entry) ->
                                e.emoji)
                              info
                          in
                          let summary =
                            Option.bind info
                              (fun (e : Status_message.tool_entry) -> e.summary)
                          in
                          let duration_secs =
                            Option.bind info
                              (fun (e : Status_message.tool_entry) ->
                                Option.map
                                  (fun fin -> fin -. e.started_at)
                                  e.finished_at)
                          in
                          let formatted =
                            Telegram_format.format_error_standalone ~emoji ~name
                              ~summary ~duration_secs ~result
                          in
                          let* () =
                            send_chunked ~disable_notification:true
                              ~parse_mode:"MarkdownV2" ~bot_token
                              ~chat_id:update.chat_id ~text:formatted ()
                          in
                          refresh_typing ();
                          Lwt.return_unit)
                        else Lwt.return_unit
                    | Provider.ThinkingDelta text ->
                        if agent_defaults.show_thinking then
                          Buffer.add_string thinking_buf text;
                        Lwt.return_unit
                    | Provider.Delta _ | Provider.ToolCallDelta _
                    | Provider.ToolOutputDelta _ | Provider.Done ->
                        Lwt.return_unit)
                | None -> (
                    let open Lwt.Syntax in
                    let* () =
                      if not !tool_reaction_set then
                        match chunk with
                        | Provider.ToolStart _ ->
                            tool_reaction_set := true;
                            set_reaction reaction_emoji_tools
                        | _ -> Lwt.return_unit
                      else Lwt.return_unit
                    in
                    let* () =
                      match chunk with
                      | Provider.ToolStart { id; name; arguments } ->
                          let summary =
                            Stream_visibility.summarize_tool_arguments ~name
                              arguments
                          in
                          Hashtbl.replace tool_start_times id
                            (Unix.gettimeofday (), summary);
                          let action = chat_action_for_tool name in
                          let* () =
                            Lwt.catch
                              (fun () ->
                                send_chat_action ~bot_token
                                  ~chat_id:update.chat_id ~action)
                              (fun _exn -> Lwt.return_unit)
                          in
                          refresh_typing ();
                          Lwt.return_unit
                      | _ -> Lwt.return_unit
                    in
                    let settings : Stream_visibility.settings =
                      {
                        show_thinking = agent_defaults.show_thinking;
                        show_tool_calls = agent_defaults.show_tool_calls;
                        notify_tool_starts = true;
                        notify_tool_successes = true;
                      }
                    in
                    let* () =
                      Stream_visibility.on_chunk visibility ~settings
                        ~notify:(fun text ->
                          let text = Telegram_format.markdown_to_mdv2 text in
                          let open Lwt.Syntax in
                          let* () =
                            send_chunked ~disable_notification:true
                              ~parse_mode:"MarkdownV2" ~bot_token
                              ~chat_id:update.chat_id ~text ()
                          in
                          refresh_typing ();
                          Lwt.return_unit)
                        chunk
                    in
                    match chunk with
                    | Provider.ToolResult { id; name; result; is_error; _ } ->
                        let* () =
                          if is_error then
                            let emoji = Stream_visibility.tool_emoji name in
                            let duration_secs, summary =
                              match Hashtbl.find_opt tool_start_times id with
                              | Some (t0, s) ->
                                  (Some (Unix.gettimeofday () -. t0), s)
                              | None -> (None, None)
                            in
                            let formatted =
                              Telegram_format.format_error_standalone ~emoji
                                ~name ~summary ~duration_secs ~result
                            in
                            send_chunked ~disable_notification:true
                              ~parse_mode:"MarkdownV2" ~bot_token
                              ~chat_id:update.chat_id ~text:formatted ()
                          else send_expandable ~name ~result ~is_error
                        in
                        refresh_typing ();
                        Lwt.return_unit
                    | _ -> Lwt.return_unit)
              in
              (* See reaction_emoji_* constants and valid_reaction_emojis *)
              Lwt.async (fun () ->
                  Lwt.catch
                    (fun () -> set_reaction reaction_emoji_received)
                    (fun _exn -> Lwt.return_unit));
              let on_drain_progress : Session.drain_progress =
                {
                  before_turn =
                    (fun queued_msg_id ->
                      let* () =
                        match queued_msg_id with
                        | Some mid -> (
                            match int_of_string_opt mid with
                            | Some mid_int ->
                                set_reaction_on mid_int reaction_emoji_received
                            | None -> Lwt.return_unit)
                        | None -> Lwt.return_unit
                      in
                      let* () =
                        match !drain_progress_msg_id with
                        | Some mid ->
                            Lwt.catch
                              (fun () ->
                                delete_message ~bot_token
                                  ~chat_id:update.chat_id ~message_id:mid ())
                              (fun _exn -> Lwt.return_unit)
                        | None -> Lwt.return_unit
                      in
                      let* mid =
                        send_message_with_id ~disable_notification:true
                          ~bot_token ~chat_id:update.chat_id
                          ~text:
                            "\xe2\x8f\xb3 Processing queued message\xe2\x80\xa6"
                          ()
                      in
                      drain_progress_msg_id := Some mid;
                      refresh_typing ();
                      Lwt.return_unit);
                  after_turn =
                    (fun queued_msg_id ->
                      match queued_msg_id with
                      | Some mid -> (
                          match int_of_string_opt mid with
                          | Some mid_int ->
                              set_reaction_on mid_int reaction_emoji_done
                          | None -> Lwt.return_unit)
                      | None -> Lwt.return_unit);
                  after_all =
                    (fun () ->
                      match !drain_progress_msg_id with
                      | Some mid ->
                          drain_progress_msg_id := None;
                          let open Lwt.Syntax in
                          let* () =
                            Lwt.catch
                              (fun () ->
                                delete_message ~bot_token
                                  ~chat_id:update.chat_id ~message_id:mid ())
                              (fun _exn -> Lwt.return_unit)
                          in
                          refresh_typing ();
                          Lwt.return_unit
                      | None -> Lwt.return_unit);
                }
              in
              let response_sent = ref false in
              let* result =
                Session.with_registered_notifier session_mgr ~key
                  ~notify:(fun text ->
                    let open Lwt.Syntax in
                    let* () =
                      send_chunked ~disable_notification:true
                        ~parse_mode:"MarkdownV2" ~bot_token
                        ~chat_id:update.chat_id
                        ~text:(Telegram_format.markdown_to_mdv2 text)
                        ()
                    in
                    refresh_typing ();
                    Lwt.return_unit)
                  (fun () ->
                    Lwt.finalize
                      (fun () ->
                        Lwt.catch
                          (fun () ->
                            let before_drain response =
                              if Session.is_queued_message_response response
                              then Lwt.return_unit
                              else
                                let open Lwt.Syntax in
                                let* () =
                                  match status_msg with
                                  | Some sm -> Status_message.finalize sm
                                  | None -> Lwt.return_unit
                                in
                                let* () =
                                  if
                                    status_msg <> None
                                    && !current_turn_has_tools
                                  then (
                                    let details_text =
                                      List.rev !current_turn_tool_details
                                      |> String.concat "\n---\n"
                                    in
                                    let details_callback =
                                      register_tool_result_details
                                        ~chat_id:update.chat_id
                                        ~user_id:update.user_id details_text
                                    in
                                    let* _msg_id =
                                      send_message_with_keyboard
                                        ~disable_notification:true ~bot_token
                                        ~chat_id:update.chat_id
                                        ~text:
                                          "\xF0\x9F\x93\x8B Tool output \
                                           available"
                                        ~buttons:
                                          [ ("Show Details", details_callback) ]
                                        ()
                                    in
                                    refresh_typing ();
                                    Lwt.return_unit)
                                  else Lwt.return_unit
                                in
                                let thinking =
                                  match status_msg with
                                  | Some _ -> Buffer.contents thinking_buf
                                  | None ->
                                      Stream_visibility.thinking_text visibility
                                in
                                let* () =
                                  if thinking <> "" then (
                                    let* () =
                                      send_chunked ~parse_mode:"MarkdownV2"
                                        ~bot_token ~chat_id:update.chat_id
                                        ~text:
                                          (Telegram_format.format_thinking
                                             thinking)
                                        ()
                                    in
                                    refresh_typing ();
                                    Lwt.return_unit)
                                  else Lwt.return_unit
                                in
                                let* () =
                                  let* () =
                                    send_chunked ~disable_notification:false
                                      ~parse_mode:"MarkdownV2" ~bot_token
                                      ~chat_id:update.chat_id
                                      ~text:
                                        (Telegram_format.markdown_to_mdv2
                                           response)
                                      ()
                                  in
                                  refresh_typing ();
                                  Lwt.return_unit
                                in
                                let* () = set_reaction reaction_emoji_done in
                                if
                                  not
                                    (Session.take_response_deferred session_mgr
                                       ~key)
                                then Session.mark_response_sent session_mgr ~key;
                                response_sent := true;
                                Lwt.return_unit
                            in
                            let turn_p =
                              Session.turn_stream session_mgr ~key ~message:msg
                                ~content_parts:!image_content_parts
                                ~channel_name:"telegram" ~channel_type:"dm"
                                ~channel:"telegram" ~channel_id:update.chat_id
                                ~message_id:(string_of_int update.message_id)
                                ~on_drain_progress ~before_drain ~on_chunk ()
                            in
                            let* response = turn_p in
                            Lwt.return (Ok response))
                          (fun exn ->
                            Lwt.return (Error (Printexc.to_string exn))))
                      (fun () ->
                        Session.unregister_interrupt_finalizer session_mgr ~key;
                        Lwt.return_unit))
              in
              match result with
              | Ok response ->
                  if Session.is_queued_message_response response then
                    Lwt.return_unit
                  else if !response_sent then (
                    let* () =
                      Reaction_tracker.cleanup_with_remove reactions ~key
                        ~remove:(fun mid _emoji ->
                          clear_message_reaction ~bot_token
                            ~chat_id:update.chat_id ~message_id:mid ())
                    in
                    Lwt.async (fun () ->
                        Session.process_autonomous_turn_result
                          ~on_response:send_to_chat session_mgr ~key ~response);
                    Lwt.return_unit)
                  else
                    let* () =
                      match status_msg with
                      | Some sm -> Status_message.finalize sm
                      | None -> Lwt.return_unit
                    in
                    let* () =
                      if status_msg <> None && !current_turn_has_tools then (
                        let details_text =
                          List.rev !current_turn_tool_details
                          |> String.concat "\n---\n"
                        in
                        let details_callback =
                          register_tool_result_details ~chat_id:update.chat_id
                            ~user_id:update.user_id details_text
                        in
                        let* _msg_id =
                          send_message_with_keyboard ~disable_notification:true
                            ~bot_token ~chat_id:update.chat_id
                            ~text:"\xF0\x9F\x93\x8B Tool output available"
                            ~buttons:[ ("Show Details", details_callback) ]
                            ()
                        in
                        refresh_typing ();
                        Lwt.return_unit)
                      else Lwt.return_unit
                    in
                    let thinking =
                      match status_msg with
                      | Some _ -> Buffer.contents thinking_buf
                      | None -> Stream_visibility.thinking_text visibility
                    in
                    let* () =
                      if thinking <> "" then (
                        let* () =
                          send_chunked ~parse_mode:"MarkdownV2" ~bot_token
                            ~chat_id:update.chat_id
                            ~text:(Telegram_format.format_thinking thinking)
                            ()
                        in
                        refresh_typing ();
                        Lwt.return_unit)
                      else Lwt.return_unit
                    in
                    let* () =
                      send_chunked ~disable_notification:false
                        ~parse_mode:"MarkdownV2" ~bot_token
                        ~chat_id:update.chat_id
                        ~text:(Telegram_format.markdown_to_mdv2 response)
                        ()
                    in
                    let* () = set_reaction reaction_emoji_done in
                    let* () =
                      Reaction_tracker.cleanup_with_remove reactions ~key
                        ~remove:(fun mid _emoji ->
                          clear_message_reaction ~bot_token
                            ~chat_id:update.chat_id ~message_id:mid ())
                    in
                    if not (Session.take_response_deferred session_mgr ~key)
                    then Session.mark_response_sent session_mgr ~key;
                    Lwt.async (fun () ->
                        Session.process_autonomous_turn_result
                          ~on_response:send_to_chat session_mgr ~key ~response);
                    Lwt.return_unit
              | Error err ->
                  Logs.err (fun m ->
                      m "Agent error for chat_id=%s: %s" update.chat_id err);
                  let* () =
                    match status_msg with
                    | Some sm -> Status_message.finalize sm
                    | None -> Lwt.return_unit
                  in
                  let* () =
                    send_message ~disable_notification:false ~bot_token
                      ~chat_id:update.chat_id
                      ~text:
                        (Printf.sprintf
                           "Sorry, an error occurred processing your message: \
                            %s"
                           err)
                      ()
                  in
                  let* () = set_reaction reaction_emoji_error in
                  let* () =
                    Reaction_tracker.cleanup_with_remove reactions ~key
                      ~remove:(fun mid _emoji ->
                        clear_message_reaction ~bot_token
                          ~chat_id:update.chat_id ~message_id:mid ())
                  in
                  if not (Session.take_response_deferred session_mgr ~key) then
                    Session.mark_response_sent session_mgr ~key;
                  Lwt.return_unit)

let dispatch_update ~bot_token ~(account : Runtime_config.telegram_account)
    ~(session_mgr : Session.t) ?run_update_command ?chat_limiter update =
  Lwt.async (fun () ->
      Lwt.catch
        (fun () ->
          handle_update ~bot_token ~account ~session_mgr ?run_update_command
            ?chat_limiter update)
        (fun exn ->
          Logs.err (fun m ->
              m "Telegram: handle_update error for update_id=%d: %s"
                update.update_id (Printexc.to_string exn));
          Lwt.return_unit))

let flush_pending_text_update ~key ~bot_token
    ~(account : Runtime_config.telegram_account) ~(session_mgr : Session.t)
    ?run_update_command ?chat_limiter () =
  match Hashtbl.find_opt pending_text_updates key with
  | None -> ()
  | Some pending ->
      Hashtbl.remove pending_text_updates key;
      dispatch_update ~bot_token ~account ~session_mgr ?run_update_command
        ?chat_limiter pending.update

let schedule_pending_text_flush ~key ~bot_token
    ~(account : Runtime_config.telegram_account) ~(session_mgr : Session.t)
    ?run_update_command ?chat_limiter generation =
  Lwt.async (fun () ->
      let open Lwt.Syntax in
      let* () = Lwt_unix.sleep !text_coalesce_window_seconds in
      match Hashtbl.find_opt pending_text_updates key with
      | Some pending
        when pending.generation = generation
             && Unix.gettimeofday () -. pending.last_seen_at
                >= !text_coalesce_window_seconds ->
          Hashtbl.remove pending_text_updates key;
          Lwt.catch
            (fun () ->
              handle_update ~bot_token ~account ~session_mgr ?run_update_command
                ?chat_limiter pending.update)
            (fun exn ->
              Logs.err (fun m ->
                  m "Telegram: handle_update error for update_id=%d: %s"
                    pending.update.update_id (Printexc.to_string exn));
              Lwt.return_unit)
      | _ -> Lwt.return_unit)

let buffer_or_dispatch_update ~bot_token
    ~(account : Runtime_config.telegram_account) ~(session_mgr : Session.t)
    ?run_update_command ?chat_limiter update =
  let now = Unix.gettimeofday () in
  let key = text_coalesce_key ~bot_token update in
  if
    (not (is_text_coalescing_candidate update))
    || !text_coalesce_window_seconds <= 0.0
  then begin
    flush_pending_text_update ~key ~bot_token ~account ~session_mgr
      ?run_update_command ?chat_limiter ();
    dispatch_update ~bot_token ~account ~session_mgr ?run_update_command
      ?chat_limiter update
  end
  else
    match Hashtbl.find_opt pending_text_updates key with
    | Some pending when can_coalesce_text_updates ~now pending update ->
        pending.update <- merge_text_updates pending update;
        pending.last_seen_at <- now;
        pending.generation <- pending.generation + 1;
        schedule_pending_text_flush ~key ~bot_token ~account ~session_mgr
          ?run_update_command ?chat_limiter pending.generation
    | Some _ ->
        flush_pending_text_update ~key ~bot_token ~account ~session_mgr
          ?run_update_command ?chat_limiter ();
        let pending = { update; last_seen_at = now; generation = 0 } in
        Hashtbl.replace pending_text_updates key pending;
        schedule_pending_text_flush ~key ~bot_token ~account ~session_mgr
          ?run_update_command ?chat_limiter pending.generation
    | None ->
        let pending = { update; last_seen_at = now; generation = 0 } in
        Hashtbl.replace pending_text_updates key pending;
        schedule_pending_text_flush ~key ~bot_token ~account ~session_mgr
          ?run_update_command ?chat_limiter pending.generation

let poll_account ~bot_token ~(account : Runtime_config.telegram_account) ~name
    ~(session_mgr : Session.t) ?run_update_command ?chat_limiter () =
  let open Lwt.Syntax in
  Logs.info (fun m -> m "Starting Telegram polling for account '%s'" name);
  let* () =
    Lwt.catch
      (fun () -> set_my_commands ~bot_token)
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
    incr poll_count;
    if !poll_count <= 3 then
      Logs.info (fun m ->
          m "Telegram poll #%d for account '%s'" !poll_count name)
    else if !poll_count = 4 then
      Logs.info (fun m ->
          m "Telegram polling stable, suppressing routine poll logs for '%s'"
            name);
    let poll_start = Unix.gettimeofday () in
    let* poll_result =
      Lwt.catch
        (fun () -> get_updates ~bot_token ~offset:!offset ~timeout:30)
        (fun exn ->
          Logs.err (fun m ->
              m "Telegram poll error for '%s': %s" name (Printexc.to_string exn));
          let* () = Lwt_unix.sleep 5.0 in
          Lwt.return (Updates (0, [])))
    in
    let* max_uid, updates =
      match poll_result with
      | Updates (max_uid, updates) ->
          conflict_backoff := 5.0;
          Lwt.return (max_uid, updates)
      | Poll_error Conflict_webhook ->
          Logs.warn (fun m ->
              m
                "Telegram: clearing webhook for '%s' before resuming \
                 long-polling"
                name);
          let* () = delete_webhook ~bot_token in
          let* () = Lwt_unix.sleep 2.0 in
          Lwt.return (0, [])
      | Poll_error Conflict_duplicate_poller ->
          Logs.warn (fun m ->
              m "Telegram: another poller is active for '%s', backing off %.0fs"
                name !conflict_backoff);
          let* () = Lwt_unix.sleep !conflict_backoff in
          conflict_backoff := Float.min (!conflict_backoff *. 2.0) 60.0;
          Lwt.return (0, [])
      | Poll_error (Other_error _) ->
          let* () = Lwt_unix.sleep 5.0 in
          Lwt.return (0, [])
    in
    if max_uid + 1 > !offset then offset := max_uid + 1;
    let update_count = List.length updates in
    List.iter
      (fun update ->
        offset := update.update_id + 1;
        if update.message_id > 0 then begin
          let cur =
            Option.value ~default:0
              (Hashtbl.find_opt latest_chat_msg_id update.chat_id)
          in
          if update.message_id > cur then
            Hashtbl.replace latest_chat_msg_id update.chat_id update.message_id
        end;
        if should_process_update update then
          buffer_or_dispatch_update ~bot_token ~account ~session_mgr
            ?run_update_command ?chat_limiter update
        else
          Logs.info (fun m ->
              m "Telegram: ignoring duplicate update update_id=%d chat_id=%s"
                update.update_id update.chat_id))
      updates;
    (if !poll_count <= 3 || !poll_count mod 100 = 0 then
       let poll_elapsed_ms = (Unix.gettimeofday () -. poll_start) *. 1000.0 in
       Logs.info (fun m ->
           m "Telegram poll #%d for '%s': %.0fms elapsed, %d update(s) received"
             !poll_count name poll_elapsed_ms update_count));
    let* () =
      let rec drain_callbacks () =
        if Queue.is_empty pending_callbacks then Lwt.return_unit
        else
          let cb = Queue.pop pending_callbacks in
          if cb.cb_bot_token <> bot_token then begin
            (* Re-queue callbacks for other accounts *)
            Queue.push cb pending_callbacks;
            Lwt.return_unit
          end
          else
            let* () =
              Lwt.catch
                (fun () ->
                  match cb.data with
                  | data
                    when String.starts_with ~prefix:details_callback_prefix data
                    ->
                      let text =
                        match
                          take_tool_result_details ~chat_id:cb.cb_chat_id
                            ~user_id:cb.cb_user_id data
                        with
                        | Some details when String.trim details <> "" -> details
                        | _ -> "No details available."
                      in
                      let* () =
                        answer_callback_query ~bot_token
                          ~callback_query_id:cb.callback_query_id ()
                      in
                      send_message ~disable_notification:true ~bot_token
                        ~chat_id:cb.cb_chat_id ~text ()
                  | data -> (
                      match Hashtbl.find_opt callback_routing data with
                      | Some (session_key, label, _created) ->
                          Hashtbl.remove callback_routing data;
                          let* () =
                            answer_callback_query ~bot_token
                              ~callback_query_id:cb.callback_query_id
                              ~text:(Printf.sprintf "Selected: %s" label)
                              ()
                          in
                          Lwt.async (fun () ->
                              Lwt.catch
                                (fun () ->
                                  let message =
                                    Printf.sprintf "[Button: %s]" label
                                  in
                                  let* response =
                                    Session.turn session_mgr ~key:session_key
                                      ~message ~channel:"telegram"
                                      ~channel_id:cb.cb_chat_id ()
                                  in
                                  if
                                    not
                                      (Session.is_queued_message_response
                                         response)
                                  then
                                    send_chunked ~disable_notification:false
                                      ~parse_mode:"MarkdownV2" ~bot_token
                                      ~chat_id:cb.cb_chat_id
                                      ~text:
                                        (Telegram_format.markdown_to_mdv2
                                           response)
                                      ()
                                  else Lwt.return_unit)
                                (fun exn ->
                                  Logs.err (fun m ->
                                      m
                                        "Telegram: button callback routing \
                                         error: %s"
                                        (Printexc.to_string exn));
                                  Lwt.return_unit));
                          Lwt.return_unit
                      | None ->
                          answer_callback_query ~bot_token
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
        if Queue.is_empty pending_poll_answers then Lwt.return_unit
        else
          let pa = Queue.pop pending_poll_answers in
          let* () =
            match Hashtbl.find_opt poll_routing pa.pa_poll_id with
            | Some (session_key, chat_id, poll_bot_token, options, _created_at)
              ->
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
                  Lwt.async (fun () ->
                      Lwt.catch
                        (fun () ->
                          let message =
                            Printf.sprintf "[Poll vote: %s]"
                              (String.concat ", " selected)
                          in
                          let* response =
                            Session.turn session_mgr ~key:session_key ~message
                              ~channel:"telegram" ~channel_id:chat_id ()
                          in
                          if not (Session.is_queued_message_response response)
                          then
                            send_chunked ~disable_notification:false
                              ~bot_token:poll_bot_token ~chat_id ~text:response
                              ()
                          else Lwt.return_unit)
                        (fun exn ->
                          Logs.err (fun m ->
                              m "Telegram: poll answer routing error: %s"
                                (Printexc.to_string exn));
                          Lwt.return_unit));
                  Lwt.return_unit
                end
            | None ->
                Logs.debug (fun m ->
                    m "Telegram: ignoring poll_answer for unknown poll_id=%s"
                      pa.pa_poll_id);
                Lwt.return_unit
          in
          drain_poll_answers ()
      in
      drain_poll_answers ()
    in
    (* Periodic cleanup of stale routing entries *)
    if !poll_count mod 100 = 0 then cleanup_stale_routing ();
    poll ()
  in
  poll ()

let start_polling ~(config : Runtime_config.t) ~(session_manager : Session.t)
    ?run_update_command ?chat_limiter () =
  match config.channels.telegram with
  | None ->
      Logs.info (fun m -> m "No Telegram config found, skipping polling");
      Lwt.return_unit
  | Some tg_config -> (
      text_coalesce_window_seconds :=
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
                       ?chat_limiter ()))
              accounts
          in
          match poll_loops with
          | [] ->
              Logs.info (fun m -> m "No Telegram accounts with valid bot_token");
              Lwt.return_unit
          | loops -> Lwt.join loops))
