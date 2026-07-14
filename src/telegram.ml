include Telegram_api

let should_salute_queued_interrupt ~inbound_text ~queued =
  queued && Connector_status.is_interrupt_ack_message inbound_text

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
      let key =
        "telegram:"
        ^ Session.sanitize_session_key update.chat_id
        ^ ":"
        ^ Session.sanitize_session_key update.user_id
      in
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
              ~parse_mode:"HTML" ());
        Session.register_connector_capabilities session_mgr ~key
          Connector_capabilities.telegram
      end;
      Telegram_rich_notifier.register ~session_mgr ~key ~bot_token
        ~chat_id:update.chat_id ~refresh_typing ~send_text:(fun text ->
          send_to_chat text);
      let* inbound =
        Telegram_attachments.resolve_user_text ~bot_token ~update ~session_mgr
          ~key ()
      in
      let user_text = inbound.user_text in
      let image_content_parts = inbound.image_content_parts in
      let doc_attachments = inbound.doc_attachments in
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
              (match Session.get_session_model_override session_mgr ~key with
              | Some model ->
                  Restart_notify.write_session ~channel:"telegram"
                    ~channel_id:update.chat_id ~session_key:key ~model
              | None ->
                  Restart_notify.write_session_key ~channel:"telegram"
                    ~channel_id:update.chat_id ~session_key:key);
              acknowledge_update ~bot_token ~update_id:update.update_id)
            ~send_progress ()
        in
        Lwt.return_unit)
      else
        let skill_names =
          List.map
            (fun (s : Skills.skill_md_meta) -> s.md_name)
            (Skills.available_skills ())
        in
        let cmd_result = Slash_commands.handle ~skill_names user_text in
        let* cmd_result, user_text, skill_injections, _loaded_skill_name =
          match cmd_result with
          | Slash_commands.SkillInvoke (name, args) -> (
              if
                args = ""
                && Session.skill_loaded_in_context session_mgr ~key name
              then Lwt.return (Slash_commands.NotACommand, user_text, [], None)
              else
                let* result = Skills.expand_slash_skill ~name ~args () in
                match result with
                | Ok r ->
                    Lwt.return
                      ( Slash_commands.NotACommand,
                        user_text,
                        [ r.skill_injection ],
                        Some name )
                | Error msg ->
                    Lwt.return (Slash_commands.Reply msg, user_text, [], None))
          | other -> Lwt.return (other, user_text, [], None)
        in
        let is_admin =
          match Session.get_db session_mgr with
          | Some db ->
              Admin.is_admin ~db ~channel:"telegram" ~sender_id:update.user_id
          | None -> false
        in
        let user_group = if is_admin then "admin" else "guest" in
        let cmd_result = Slash_commands.gate_admin ~is_admin cmd_result in
        let env : Connector_dispatch.dispatch_env =
          {
            connector = Format_adapter.Telegram_html;
            connector_name = "telegram";
            log_name = "Telegram";
            thinking_channel_field = "chat_id";
            thinking_user_field = "user_id";
            show_thinking_channel_field = "chat_id";
            show_thinking_user_field = "user_id";
            session_mgr;
            key;
            channel_id = update.chat_id;
            channel_name = Some "telegram";
            channel_type = Some "dm";
            sender_name = None;
            message_id = Some (string_of_int update.message_id);
            user_id = update.user_id;
            is_admin;
            send_plain =
              (fun text ->
                send_message ~bot_token ~chat_id:update.chat_id ~text ());
            send_formatted =
              (fun text ->
                send_chunked_html_with_fallback ~bot_token
                  ~chat_id:update.chat_id ~text ());
          }
        in
        match cmd_result with
        | AdminRequired _ -> assert false
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
        | Delegate (agent_name, prompt) ->
            let* () =
              send_message ~bot_token ~chat_id:update.chat_id
                ~text:"Delegating to a temporary session..." ()
            in
            let tg_prompt = telegram_delegate_prompt ~user_prompt:prompt in
            let send_agent_reply text =
              send_chunked_html_with_fallback ~disable_notification:false
                ~bot_token ~chat_id:update.chat_id ~text ()
            in
            Session.delegate_turn session_mgr ~parent_key:key
              ~debug_notify:send_agent_reply ?agent_name ~prompt:tg_prompt
              ~send_reply:send_agent_reply ();
            Lwt.return_unit
        | AgentInvoke (agent_name, prompt) ->
            let* () =
              send_message ~bot_token ~chat_id:update.chat_id
                ~text:(Printf.sprintf "Invoking agent '%s'..." agent_name)
                ()
            in
            let send_agent_reply text =
              send_chunked_html_with_fallback ~disable_notification:false
                ~bot_token ~chat_id:update.chat_id ~text ()
            in
            Session.agent_invoke_turn session_mgr ~agent_name ~prompt
              ~parent_key:key ~debug_notify:send_agent_reply
              ~send_reply:send_agent_reply ();
            Lwt.return_unit
        | Rig action -> (
            match action with
            | RigList ->
                let text = Rig.list_text () in
                send_chunked_html_with_fallback ~bot_token
                  ~chat_id:update.chat_id ~text ()
            | RigInstall name | RigAdjust name | RigRemove name -> (
                let act =
                  match action with
                  | RigInstall _ -> `Install
                  | RigAdjust _ -> `Adjust
                  | _ -> `Remove
                in
                let act_str =
                  match act with
                  | `Install -> "install"
                  | `Adjust -> "adjust"
                  | `Remove -> "remove"
                in
                match Rig.prompt_for ~name ~action:act with
                | Error msg ->
                    send_message ~bot_token ~chat_id:update.chat_id ~text:msg ()
                | Ok prompt ->
                    let* () =
                      send_message ~bot_token ~chat_id:update.chat_id
                        ~text:
                          (Printf.sprintf "Running rig %s for '%s'..." act_str
                             name)
                        ()
                    in
                    let send_agent_reply text =
                      send_chunked_html_with_fallback
                        ~disable_notification:false ~bot_token
                        ~chat_id:update.chat_id ~text ()
                    in
                    Session.delegate_turn session_mgr ~prompt ~parent_key:key
                      ~debug_notify:send_agent_reply
                      ~send_reply:send_agent_reply ();
                    (match act with
                    | `Install -> (
                        match Rig.find_rig name with
                        | Some rig ->
                            Rig.mark_installed ~name ~version:rig.version
                        | None -> ())
                    | `Remove -> Rig.mark_removed ~name
                    | `Adjust -> ());
                    Lwt.return_unit))
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
                  format_model_show ~connector:Format_adapter.Telegram_html
                    ~current ~favorites:prefs.favorites ~usage_ranked
                in
                send_message ~bot_token ~chat_id:update.chat_id ~text
                  ~parse_mode:"HTML" ()
            | ModelSet _ | ModelSetForce _ | ModelSetDefault _ ->
                let* text =
                  Slash_commands_model.handle_model_set_action
                    ~config_source:"telegram" ~session_manager:session_mgr ~key
                    action
                in
                send_message ~bot_token ~chat_id:update.chat_id ~text ()
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
            | ModelList (provider, availability) ->
                let db_extras =
                  match Session.get_db session_mgr with
                  | None -> []
                  | Some db ->
                      Model_discovery.get_db_only_model_infos ~db
                        ~provider_filter:provider ~availability ()
                in
                let models =
                  Models_catalog.to_plain_list ~provider_filter:provider
                    ~availability ~db_extras ()
                  |> String.split_on_char '\n'
                  |> List.filter (fun s -> s <> "")
                in
                let text =
                  format_model_list ~connector:Format_adapter.Telegram_html
                    ~models ~provider
                in
                send_message ~bot_token ~chat_id:update.chat_id ~text
                  ~parse_mode:"HTML" ()
            | ModelUsage ->
                let cfg = Session.get_config session_mgr in
                Provider_quota.set_cache_ttl cfg.quota_cache_ttl_s;
                let results =
                  Provider_quota.get_all_cached ()
                  |> List.map (fun (_name, pq) -> pq)
                in
                let text =
                  Slash_commands.format_model_usage
                    ~connector:Format_adapter.Telegram_html ~config:cfg results
                in
                send_chunked_html_with_fallback ~bot_token
                  ~chat_id:update.chat_id ~text ())
        | ForkAnd (agent_name, prompt) ->
            let* () =
              send_message ~bot_token ~chat_id:update.chat_id
                ~text:"Forking session..." ()
            in
            let tg_prompt = telegram_delegate_prompt ~user_prompt:prompt in
            let send_agent_reply text =
              send_chunked_html_with_fallback ~disable_notification:false
                ~bot_token ~chat_id:update.chat_id ~text ()
            in
            Session.fork_and_run session_mgr ~parent_key:key ?agent_name
              ~debug_notify:send_agent_reply ~prompt:tg_prompt
              ~send_reply:send_agent_reply ();
            Lwt.return_unit
        | Debate prompt -> (
            match Session.get_db session_mgr with
            | Some db ->
                let config = Session.get_config session_mgr in
                let send_agent_reply text =
                  send_chunked_html_with_fallback ~disable_notification:false
                    ~bot_token ~chat_id:update.chat_id ~text ()
                in
                let on_llm_call_debug =
                  Session.debug_callback_for session_mgr ~key
                    (Some send_agent_reply)
                in
                let* text =
                  Debate.run_for_prompt ?on_llm_call_debug ~config ~db ~prompt
                    ()
                in
                send_chunked_html_with_fallback ~bot_token
                  ~chat_id:update.chat_id ~text ()
            | None ->
                send_message ~bot_token ~chat_id:update.chat_id
                  ~text:"Debate requires a database." ())
        | BashRun cmd ->
            let config = Session.get_config session_mgr in
            let* result =
              Slash_commands_bash.run_bash_command ~config ~session_key:key cmd
            in
            let text = Slash_commands_bash.format_result cmd result in
            if String.length text > 4000 then
              let timestamp =
                Int64.to_int (Int64.of_float (Unix.gettimeofday ()))
              in
              let filename = Printf.sprintf "bash_output_%d.txt" timestamp in
              let* doc_result =
                send_document ~bot_token ~chat_id:update.chat_id ~filename
                  ~content:text ()
              in
              match doc_result with
              | Ok _ -> Lwt.return_unit
              | Error err ->
                  let truncated =
                    String.sub text 0 3900 ^ "\n...\n[truncated, send failed: "
                    ^ err ^ "]"
                  in
                  send_message ~bot_token ~chat_id:update.chat_id
                    ~text:truncated ()
            else send_message ~bot_token ~chat_id:update.chat_id ~text ()
        | DebugDumpChat -> (
            let content = Session.dump_json session_mgr ~key in
            let timestamp =
              Int64.to_int (Int64.of_float (Unix.gettimeofday ()))
            in
            let safe_key =
              String.map
                (fun c ->
                  match c with
                  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' -> c
                  | _ -> '_')
                key
            in
            let filename =
              Printf.sprintf "session_%s_%d.json" safe_key timestamp
            in
            let* result =
              send_document ~bot_token ~chat_id:update.chat_id ~filename
                ~content ()
            in
            match result with
            | Ok _ -> Lwt.return_unit
            | Error err ->
                send_message ~bot_token ~chat_id:update.chat_id
                  ~text:
                    (Printf.sprintf
                       "Failed to send debug dump: %s\n\nDump length: %d bytes"
                       err (String.length content))
                  ())
        | InjectConnectorHistory _ ->
            send_message ~bot_token ~chat_id:update.chat_id
              ~text:
                "Connector history is not applicable for this channel — \
                 Telegram delivers all messages to the bot, so the agent \
                 already sees all channel activity. Use \
                 /inject_connector_history in Teams or Discord group chats."
              ()
        | SkillInvoke _ -> Lwt.return_unit (* unreachable: preprocessed above *)
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
              Session.enqueue_message_if_busy session_mgr ~key ~raw_message:msg
                ({
                   message = normalized_msg;
                   content_parts = image_content_parts;
                   attachments = [];
                   channel_name = Some "telegram";
                   channel_type = Some "dm";
                   sender_id = Some update.user_id;
                   sender_name = None;
                   user_group = Some user_group;
                   channel = Some "telegram";
                   channel_id = Some update.chat_id;
                   message_id = Some (string_of_int update.message_id);
                   inbound_queue_id = None;
                   bang = false;
                   deferred_followup = false;
                   snapshot_work_type = Some Access_snapshot.Room_turn;
                   has_external_users = false;
                 }
                  : Session.queued_message)
            in
            if early_queued then
              if
                should_salute_queued_interrupt ~inbound_text:msg
                  ~queued:early_queued
              then
                Lwt.catch
                  (fun () ->
                    set_message_reaction ~bot_token ~chat_id:update.chat_id
                      ~message_id:update.message_id
                      ~emoji:reaction_emoji_interrupt_ack ())
                  (fun _exn -> Lwt.return_unit)
              else Lwt.return_unit
            else
              let cfg = Session.get_config session_mgr in
              let agent_defaults = cfg.agent_defaults in
              let low_volume =
                Runtime_config.room_low_volume cfg ~session_key:key
              in
              let use_consolidated =
                Status_update.shows_tool_status ~agent_defaults ~low_volume ()
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
              let drain_progress_msg_id = ref None in
              let notifier_factory =
                if use_consolidated then
                  Some
                    (fun () ->
                      let status_notifier =
                        make_status_notifier ~bot_token ~chat_id:update.chat_id
                      in
                      Status_message.create ~notifier:status_notifier
                        ~parse_mode:"HTML" ())
                else None
              in
              let strategy =
                Status_update.select_strategy ~agent_defaults
                  ~capabilities:(Some Connector_capabilities.telegram)
                  ~low_volume ()
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
              let handler =
                Status_update.make_handler ~strategy ~notifier_factory
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
                  ~agent_defaults ~low_volume ~parse_mode:"HTML"
                  ~on_tool_event:(fun event ->
                    let open Lwt.Syntax in
                    match event with
                    | Status_update.Tool_started _ ->
                        if low_volume then Lwt.return_unit
                        else if not !tool_reaction_set then begin
                          tool_reaction_set := true;
                          set_reaction reaction_emoji_tools
                        end
                        else Lwt.return_unit
                    | Tool_completed
                        {
                          id;
                          name;
                          result;
                          is_error;
                          summary = _;
                          duration_secs = _;
                        } ->
                        current_turn_tool_details :=
                          format_tool_result_detail ~name ~result
                          :: !current_turn_tool_details;
                        current_turn_has_tools := true;
                        if low_volume then Lwt.return_unit
                        else if (not use_consolidated) && not is_error then
                          send_expandable ~name ~result ~is_error
                        else Lwt.return_unit)
                  ~on_error_detail:(fun (detail : Status_update.error_detail) ->
                    let formatted =
                      Telegram_format.format_error_standalone
                        ~emoji:detail.emoji ~name:detail.name
                        ~summary:detail.summary
                        ~duration_secs:detail.duration_secs
                        ~result:detail.result
                    in
                    let open Lwt.Syntax in
                    let* () =
                      send_chunked ~disable_notification:true
                        ~parse_mode:"MarkdownV2" ~bot_token
                        ~chat_id:update.chat_id ~text:formatted ()
                    in
                    refresh_typing ();
                    Lwt.return_unit)
                  ()
              in
              (match notifier_factory with
              | Some _ ->
                  Session.register_interrupt_finalizer session_mgr ~key
                    (fun () ->
                      let open Lwt.Syntax in
                      let* () = handler.finalize () in
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
              let on_chunk = handler.on_chunk in
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
                              if Session.should_suppress_response response then
                                Lwt.return_unit
                              else
                                let open Lwt.Syntax in
                                let* () = handler.finalize () in
                                let* () =
                                  if use_consolidated && !current_turn_has_tools
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
                                let thinking = handler.get_thinking () in
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
                                ~content_parts:image_content_parts
                                ~attachments:doc_attachments ~skill_injections
                                ~channel_name:"telegram" ~channel_type:"dm"
                                ~sender_id:update.user_id ~user_group
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
                  if Session.should_suppress_response response then
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
                    let* () = handler.finalize () in
                    let* () =
                      if use_consolidated && !current_turn_has_tools then (
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
                    let thinking = handler.get_thinking () in
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
                  let* () = handler.finalize () in
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
        | ( RegisterAsAdminOtc _ | Reply _ | FormattedReply _ | Help | Menu _
          | Reset | RuntimeCtx | Context | Uptime | Status | Thinking _
          | ShowThinking _ | Heartbeat _ | Debug _ | AgentMenu _ | ModelMenu _
          | ThinkingMenu | ConfigMenu _ | SkillsMenu _ | CostsMenu | BgMenu
          | Tools | Tasks | TasksFull | Costs _ | Session _ | Usage _ | Active
          | Bg _ | WorkflowRun _ | Cron _ | Bl _ | HeldItems _ | Memories _
          | RoomsMemory _ | ExplainAccess | WhatCanDo | Repo _ | Followup _ ) as
          r ->
            Connector_dispatch.dispatch env r

(* Poll loop, dispatch, and start_polling are in Telegram_poll *)
