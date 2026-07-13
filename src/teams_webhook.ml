include Teams_api

(* Main webhook handler — called from http_server.ml with raw body.
   Responds asynchronously; caller should return 202 immediately. *)
let handle_webhook ~(config : Runtime_config.teams_config)
    ~(session_manager : Session.t) ?(send_reply_fn = send_reply)
    ?(send_adaptive_card_fn = send_adaptive_card) ?event_limiter ?turn_fn
    ~auth_header body_str =
  let open Lwt.Syntax in
  let send_reply = send_reply_fn in
  let send_adaptive_card = send_adaptive_card_fn in
  let session_turn = match turn_fn with Some f -> f | None -> Session.turn in
  (* Verify JWT claims *)
  let* auth_result = verify_auth ~config auth_header in
  match auth_result with
  | Error reason ->
      Logs.warn (fun m -> m "Teams: auth failed: %s" reason);
      Lwt.return_unit
  | Ok () -> (
      match Teams_activity_parser.parse_activity body_str with
      | None -> Lwt.return_unit
      | Some
          {
            activity_id;
            service_url;
            conversation_id;
            reply_to_id;
            user_id;
            user_name;
            team_id;
            text = raw_text;
            is_group;
            is_external;
            tenant_id;
            mentioned_ids;
            attachments = parsed_attachments;
          } -> (
          let db = Session.get_db session_manager in
          if dedup_seen_persistent ~db ~conversation_id ~activity_id then
            Lwt.return_unit
          else
            let text = strip_at_mentions raw_text in
            if text = "" && parsed_attachments = [] then Lwt.return_unit
            else
              (* In group chats, only process if bot was mentioned or
                 addressed *)
              let bot_mentioned =
                if is_group then
                  let bot_id_prefix = "28:" ^ config.app_id in
                  List.exists
                    (fun mid -> mid = config.app_id || mid = bot_id_prefix)
                    mentioned_ids
                else false
              in
              if
                not
                  (Group_chat_filter.should_respond ~is_group ~bot_mentioned
                     ~is_reply_to_bot:false ~bot_name:"clawq" text)
              then begin
                Logs.debug (fun m ->
                    m
                      "Teams: ignoring unaddressed group message conv=%s \
                       user=%s"
                      conversation_id user_id);
                let eff_tid = if team_id = "" then "personal" else team_id in
                (if room_has_profile_binding ~session_manager ~conversation_id
                 then
                   record_scoped_room_history_if_bound ~session_manager
                     ~team_id:eff_tid ~conversation_id ~user_id ~user_name ~text
                 else
                   let cfg = Session.get_config session_manager in
                   if
                     Connector_capabilities.should_capture_history
                       ~enabled:cfg.connector_history.enabled
                       Connector_capabilities.teams
                   then begin
                     let hist_key =
                       resolve_session_key ~session_manager ~team_id:eff_tid
                         ~conversation_id ~reply_to_id ()
                     in
                     let db =
                       if cfg.connector_history.persist_to_db then
                         Session.get_db session_manager
                       else None
                     in
                     Connector_history.record ?db
                       ~persist:cfg.connector_history.persist_to_db
                       ~key:hist_key ~channel_type:"teams"
                       ~max:cfg.connector_history.max_messages
                       ~sender_name:user_name ~sender_id:user_id ~text ()
                   end);
                Lwt.return_unit
              end
              else
                let effective_team_id =
                  if team_id = "" then "personal" else team_id
                in
                Logs.info (fun m ->
                    m "Teams: message from user=%s (id=%s) team=%s conv=%s"
                      (if user_name <> "" then user_name else user_id)
                      user_id effective_team_id conversation_id);
                if is_external || tenant_id <> None then
                  Logs.info (fun m ->
                      m
                        "Teams: external room detected conv=%s is_external=%b \
                         tenant_id=%s"
                        conversation_id is_external
                        (Option.value tenant_id ~default:""));
                if not (is_team_allowed ~config ~team_id:effective_team_id) then (
                  Logs.warn (fun m ->
                      m "Teams: ignoring message from unauthorized team=%s"
                        effective_team_id);
                  Lwt.return_unit)
                else if not (is_user_allowed ~config ~user_id) then (
                  Logs.warn (fun m ->
                      m
                        "Teams: ignoring message from unauthorized user=%s \
                         (id=%s)"
                        (if user_name <> "" then user_name else user_id)
                        user_id);
                  Lwt.return_unit)
                else
                  let effective_service_url =
                    if service_url = "" then config.service_url else service_url
                  in
                  let key =
                    resolve_session_key ~session_manager
                      ~team_id:effective_team_id ~conversation_id ~reply_to_id
                      ()
                  in
                  let sender_name =
                    if user_name = "" then None else Some user_name
                  in
                  (* @mention the sender in group chats so they get a
                   notification. Only on final responses and ask_user_question
                   prompts — not on intermediate streaming updates (notify). *)
                  let mention =
                    if
                      is_group && user_name <> ""
                      && config.mention_mode <> "none"
                    then Some { mention_id = user_id; mention_name = user_name }
                    else None
                  in
                  let send_text text =
                    let open Lwt.Syntax in
                    let* _id =
                      send_reply ~alert:true ~config
                        ~service_url:effective_service_url ~conversation_id
                        ~reply_to_id:activity_id ~text ?mention ()
                    in
                    Lwt.return_unit
                  in
                  let limiter_key = conversation_id ^ ":" ^ user_id in
                  let* rate_decision =
                    check_incoming_rate_limit ?event_limiter ~limiter_key ()
                  in
                  match rate_decision with
                  | Rate_limited { should_warn } ->
                      if should_warn then
                        send_text incoming_rate_limited_message
                      else Lwt.return_unit
                  | Allowed -> (
                      (* Ensure a typing indicator watcher is running for this
                       session. The watcher tracks Session live_activity and
                       sends typing activities while the session is active. *)
                      let typing_watcher =
                        Typing_indicator.ensure_session_typing_watcher
                          ~session_mgr:session_manager ~key
                          ~send_action:(fun () ->
                            send_typing_activity ~config
                              ~service_url:effective_service_url
                              ~conversation_id)
                          ~interval:3.0 ~idle_timeout:300.0
                      in
                      let refresh_typing () = typing_watcher.refresh () in
                      let skill_names =
                        List.map
                          (fun (s : Skills.skill_md_meta) -> s.md_name)
                          (Skills.available_skills ())
                      in
                      let slash_text = normalize_clawq_slash_text text in
                      let* ( cmd_result,
                             text,
                             skill_injections,
                             _loaded_skill_name ) =
                        match Slash_commands.handle ~skill_names slash_text with
                        | Slash_commands.SkillInvoke (name, args) -> (
                            if
                              args = ""
                              && Session.skill_loaded_in_context session_manager
                                   ~key name
                            then
                              Lwt.return
                                (Slash_commands.NotACommand, text, [], None)
                            else
                              let* result =
                                Skills.expand_slash_skill ~name ~args ()
                              in
                              match result with
                              | Ok r ->
                                  Lwt.return
                                    ( Slash_commands.NotACommand,
                                      text,
                                      [ r.skill_injection ],
                                      Some name )
                              | Error err_msg ->
                                  Lwt.return
                                    ( Slash_commands.Reply err_msg,
                                      text,
                                      [],
                                      None ))
                        | Slash_commands.InjectConnectorHistory count -> (
                            let cfg = Session.get_config session_manager in
                            let hist_key =
                              resolve_session_key ~session_manager
                                ~team_id:effective_team_id ~conversation_id
                                ~reply_to_id ()
                            in
                            let db =
                              if cfg.connector_history.persist_to_db then
                                Session.get_db session_manager
                              else None
                            in
                            match
                              Connector_history.get_formatted_for_key ?db
                                ~key:hist_key ~count ()
                            with
                            | Some (context, n) ->
                                let* _id =
                                  send_reply ~alert:false ~config
                                    ~service_url:effective_service_url
                                    ~conversation_id ~reply_to_id:activity_id
                                    ~text:
                                      (Printf.sprintf
                                         "Last %d chat msgs loaded into context"
                                         n)
                                    ()
                                in
                                Lwt.return
                                  ( Slash_commands.NotACommand,
                                    Printf.sprintf
                                      "[Loaded %d messages from channel \
                                       history]"
                                      n,
                                    [ context ],
                                    None )
                            | None ->
                                Lwt.return
                                  ( Slash_commands.Reply
                                      "No connector history available. Ensure \
                                       connector_history.enabled is true in \
                                       config. Buffer captures unaddressed \
                                       group messages received since daemon \
                                       started (or from DB if persist_to_db is \
                                       on).",
                                    text,
                                    [],
                                    None ))
                        | other -> Lwt.return (other, text, [], None)
                      in
                      let is_admin =
                        match Session.get_db session_manager with
                        | Some db ->
                            Admin.is_admin ~db ~channel:"teams"
                              ~sender_id:user_id
                        | None -> false
                      in
                      let user_group = if is_admin then "admin" else "guest" in
                      let cmd_result =
                        Slash_commands.gate_admin ~is_admin cmd_result
                      in
                      (match cmd_result with
                      | InjectConnectorHistory _ -> ()
                      | _ ->
                          record_scoped_room_history_if_bound ~session_manager
                            ~team_id:effective_team_id ~conversation_id ~user_id
                            ~user_name ~text);
                      match cmd_result with
                      | RegisterAsAdminOtc None ->
                          let _code =
                            Admin.generate_otc ~channel:"teams"
                              ~sender_id:user_id
                          in
                          send_text
                            "Admin registration initiated. Check the daemon \
                             console/logs for your one-time code, then run: \
                             /register_as_admin_otc CODE"
                      | RegisterAsAdminOtc (Some code) -> (
                          match Session.get_db session_manager with
                          | Some db -> (
                              match
                                Admin.verify_otc ~db ~channel:"teams"
                                  ~sender_id:user_id ~code
                              with
                              | Ok () ->
                                  send_text "Successfully registered as admin."
                              | Error err_msg -> send_text err_msg)
                          | None -> send_text "Database not available.")
                      | AdminRequired _ -> assert false
                      | InjectConnectorHistory _ ->
                          Lwt.return_unit (* unreachable: preprocessed above *)
                      | SkillInvoke _ ->
                          Lwt.return_unit (* unreachable: preprocessed above *)
                      | Followup action ->
                          let followup_channel_id =
                            encode_channel_id ~service_url:effective_service_url
                              ~conversation_id
                          in
                          Connector_dispatch.dispatch_followup
                            ~session_mgr:session_manager ~key
                            ~connector_name:"teams"
                            ~channel_id:followup_channel_id
                            ~channel_name:"teams"
                            ~channel_type:(if is_group then "group" else "dm")
                            ?sender_name ~message_id:activity_id ~user_id
                            ~is_admin ~send_reply:send_text action
                      | NotACommand -> (
                          (* Register status message factory and capabilities *)
                          if
                            Option.is_none
                              (Session.find_connector_capabilities
                                 session_manager ~key)
                          then
                            Session.register_connector_capabilities
                              session_manager ~key Connector_capabilities.teams;
                          Session.register_status_message_factory
                            session_manager ~key (fun () ->
                              let notifier =
                                make_status_notifier ~config
                                  ~service_url:effective_service_url
                                  ~conversation_id ~reply_to_id:activity_id
                              in
                              Status_message.create ~notifier
                                ~parse_mode:"Teams" ());
                          (* Register alerting notifier for ask_user_question *)
                          Session.register_alert_channel_notifier
                            session_manager ~key (fun reply_text ->
                              let* _id =
                                send_reply ~alert:true ~config
                                  ~service_url:effective_service_url
                                  ~conversation_id ~reply_to_id:activity_id
                                  ~text:reply_text ?mention ()
                              in
                              refresh_typing ();
                              Lwt.return_unit);
                          Teams_rich_notifier.register ~session_manager ~key
                            ~config ~service_url:effective_service_url
                            ~conversation_id ~reply_to_id:activity_id
                            ~send_reply ~send_adaptive_card;
                          let* result =
                            Session.with_registered_notifier session_manager
                              ~key
                              ~notify:(fun reply_text ->
                                (* No mention on intermediate updates — mention only
                             on the final response to avoid repeated tagging. *)
                                let* _id =
                                  send_reply ~alert:false ~config
                                    ~service_url:effective_service_url
                                    ~conversation_id ~reply_to_id:activity_id
                                    ~text:reply_text ()
                                in
                                refresh_typing ();
                                Lwt.return_unit)
                              (fun () ->
                                Lwt.catch
                                  (fun () ->
                                    let* content_parts, att_list, message =
                                      Teams_attachments.resolve
                                        ~teams_config:config ~session_manager
                                        ~key ~service_url:effective_service_url
                                        ~conversation_id
                                        ~reply_to_id:activity_id ~text
                                        parsed_attachments
                                    in
                                    (* Auto-inject bounded room context for
                                       profile-bound rooms with connector
                                       history enabled. *)
                                    let skill_injections =
                                      let ctx_key =
                                        resolve_session_key ~session_manager
                                          ~team_id:effective_team_id
                                          ~conversation_id ()
                                      in
                                      match
                                        Teams_context_capture
                                        .capture_room_context ~session_manager
                                          ~has_binding:
                                            (room_has_profile_binding
                                               ~session_manager)
                                          ~session_key:ctx_key ~conversation_id
                                      with
                                      | Some ctx -> ctx :: skill_injections
                                      | None -> skill_injections
                                    in
                                    let* response =
                                      let message_id =
                                        match String.trim reply_to_id with
                                        | "" ->
                                            let activity_id =
                                              String.trim activity_id
                                            in
                                            if activity_id = "" then None
                                            else Some activity_id
                                        | reply_to_id -> Some reply_to_id
                                      in
                                      session_turn session_manager ~key ~message
                                        ~content_parts ~attachments:att_list
                                        ~skill_injections ~channel_name:"teams"
                                        ~channel_type:
                                          (if is_group then "group" else "dm")
                                        ~user_group ~channel:"teams"
                                        ~channel_id:
                                          (encode_channel_id
                                             ~service_url:effective_service_url
                                             ~conversation_id)
                                        ~sender_id:user_id ?sender_name
                                        ?message_id
                                        ~has_external_users:is_external ()
                                    in
                                    Lwt.return (Ok response))
                                  (fun exn ->
                                    Lwt.return
                                      (Error (user_facing_error_of_exn exn))))
                          in
                          match result with
                          | Ok response ->
                              if Session.should_suppress_response response then
                                Lwt.return_unit
                              else
                                let* _id =
                                  send_reply ~alert:true ~config
                                    ~service_url:effective_service_url
                                    ~conversation_id ~reply_to_id:activity_id
                                    ~text:response ?mention ()
                                in
                                Lwt.return_unit
                          | Error err ->
                              Logs.err (fun m ->
                                  m
                                    "Teams: agent error for conv=%s user=%s \
                                     (id=%s): %s"
                                    conversation_id
                                    (if user_name <> "" then user_name
                                     else user_id)
                                    user_id err);
                              send_text (agent_error_message err))
                      | Reply text -> send_text text
                      | FormattedReply fn ->
                          let text = fn Format_adapter.Teams in
                          send_text text
                      | Help ->
                          let show_test = is_admin in
                          let text =
                            Slash_commands.format_help
                              ~connector:Format_adapter.Teams ~show_test
                              ~is_admin ()
                          in
                          send_text text
                      | Menu page ->
                          let full_config =
                            Session.get_config session_manager
                          in
                          let card_json =
                            Slash_commands_manifest.menu_adaptive_card_json
                              ~page ~is_admin ~config:full_config
                              ~session_key:key ()
                          in
                          let* _id =
                            send_adaptive_card ~config
                              ~service_url:effective_service_url
                              ~conversation_id ~reply_to_id:activity_id
                              ~card:card_json ()
                          in
                          Lwt.return_unit
                      | Reset ->
                          let* active_bg_tasks =
                            Session.reset session_manager ~key
                          in
                          send_text
                            (Slash_commands_fmt.format_reset
                               ~connector:Format_adapter.Teams ~active_bg_tasks)
                      | Compact -> (
                          let notifier =
                            make_status_notifier ~config
                              ~service_url:effective_service_url
                              ~conversation_id ~reply_to_id:activity_id
                          in
                          let* compact_result =
                            Session.compact session_manager ~key ~notifier ()
                          in
                          match compact_result with
                          | Ok _ -> Lwt.return_unit
                          | Error err ->
                              send_text
                                (Printf.sprintf "Compaction failed: %s" err))
                      | RuntimeCtx ->
                          let* text =
                            Session.runtime_context_block session_manager ~key
                          in
                          send_text text
                      | Context ->
                          send_text
                            (Slash_commands_context.format
                               ~connector:Format_adapter.Teams
                               ~session_mgr:session_manager ~session_key:key)
                      | Uptime ->
                          let raw =
                            Daemon_status.daemon_uptime_reply
                              ~pid:(Daemon_status.read_current_daemon_pid ())
                          in
                          send_text
                            (Slash_commands_fmt.format_uptime
                               ~connector:Format_adapter.Teams raw)
                      | Status ->
                          let text =
                            Slash_commands.format_status
                              ~connector:Format_adapter.Teams
                              ~db:(Session.get_db session_manager)
                              ~session_count:
                                (Session.session_count session_manager)
                              ~active_count:
                                (Session.active_session_count session_manager)
                              ()
                          in
                          send_text text
                      | Thinking Slash_commands.ShowThinking ->
                          let current =
                            (Session.get_config session_manager).agent_defaults
                              .reasoning_effort
                          in
                          send_text
                            (Slash_commands_fmt.format_thinking_status
                               ~connector:Format_adapter.Teams current)
                      | Thinking (Slash_commands.SetThinking level) ->
                          let connector = Format_adapter.Teams in
                          let cfg = Session.get_config session_manager in
                          let previous = cfg.agent_defaults.reasoning_effort in
                          let text =
                            match Config_set.set_reasoning_effort level with
                            | Ok () ->
                                Session.update_config ~source:"teams"
                                  session_manager
                                  {
                                    cfg with
                                    agent_defaults =
                                      {
                                        cfg.agent_defaults with
                                        reasoning_effort = level;
                                      };
                                  };
                                Slash_commands_fmt.format_thinking_set
                                  ~connector ~previous level
                            | Error err ->
                                "Failed to set thinking level: " ^ err
                          in
                          send_text text
                      | ShowThinking action ->
                          let connector = Format_adapter.Teams in
                          let cfg = Session.get_config session_manager in
                          let current = cfg.agent_defaults.show_thinking in
                          let text =
                            match action with
                            | Slash_commands.ShowThinkingStatus ->
                                Slash_commands_fmt.format_show_thinking_status
                                  ~connector current
                            | Slash_commands.ToggleShowThinking -> (
                                let new_val = not current in
                                match Config_set.set_show_thinking new_val with
                                | Ok () ->
                                    Session.update_config ~source:"teams"
                                      session_manager
                                      {
                                        cfg with
                                        agent_defaults =
                                          {
                                            cfg.agent_defaults with
                                            show_thinking = new_val;
                                          };
                                      };
                                    Slash_commands_fmt
                                    .format_show_thinking_toggle ~connector
                                      new_val
                                | Error err ->
                                    "Failed to update show_thinking: " ^ err)
                          in
                          send_text text
                      | Heartbeat action ->
                          let connector = Format_adapter.Teams in
                          let text =
                            match action with
                            | Slash_commands.HeartbeatStatus ->
                                Slash_commands_fmt.format_heartbeat_status
                                  ~connector
                                  (Session.session_heartbeat_status_text
                                     session_manager ~key)
                            | Slash_commands.SetHeartbeat enabled -> (
                                match
                                  Session.set_session_heartbeat session_manager
                                    ~key ~enabled
                                with
                                | Ok () ->
                                    Slash_commands_fmt.format_heartbeat_set
                                      ~connector enabled key
                                | Error err -> err)
                          in
                          send_text text
                      | Debug action ->
                          let connector = Format_adapter.Teams in
                          let text =
                            match action with
                            | Slash_commands.DebugStatus ->
                                Slash_commands_fmt.format_debug_status
                                  ~connector
                                  (Session.session_debug_status_text
                                     session_manager ~key)
                            | Slash_commands.SetDebug enabled -> (
                                match
                                  Session.set_session_debug session_manager ~key
                                    ~enabled
                                with
                                | Ok () ->
                                    Slash_commands_fmt.format_debug_set
                                      ~connector enabled key
                                | Error err -> err)
                          in
                          send_text text
                      | Delegate (agent_name, prompt) ->
                          let* () =
                            send_text "Delegating to a temporary session..."
                          in
                          Session.delegate_turn session_manager ~parent_key:key
                            ~debug_notify:send_text ?agent_name ~prompt
                            ~send_reply:send_text ();
                          Lwt.return_unit
                      | AgentInvoke (agent_name, prompt) ->
                          let* () =
                            send_text
                              (Printf.sprintf "Invoking agent '%s'..."
                                 agent_name)
                          in
                          Session.agent_invoke_turn session_manager ~agent_name
                            ~parent_key:key ~debug_notify:send_text ~prompt
                            ~send_reply:send_text ();
                          Lwt.return_unit
                      | ( AgentMenu _ | ModelMenu _ | ThinkingMenu
                        | ConfigMenu _ | SkillsMenu _ | CostsMenu | BgMenu ) as
                        cmd ->
                          Teams_command_cards.handle ~session_manager ~key
                            ~is_admin ~config ~service_url:effective_service_url
                            ~conversation_id ~reply_to_id:activity_id
                            ~send_adaptive_card cmd
                      | ForkAnd (agent_name, prompt) ->
                          let* () = send_text "Forking session..." in
                          Session.fork_and_run session_manager ~parent_key:key
                            ~debug_notify:send_text ?agent_name ~prompt
                            ~send_reply:send_text ();
                          Lwt.return_unit
                      | Debate prompt -> (
                          match Session.get_db session_manager with
                          | Some db ->
                              let config = Session.get_config session_manager in
                              let on_llm_call_debug =
                                Session.debug_callback_for session_manager ~key
                                  (Some send_text)
                              in
                              let* text =
                                Debate.run_for_prompt ?on_llm_call_debug ~config
                                  ~db ~prompt ()
                              in
                              send_text text
                          | None -> send_text "Debate requires a database.")
                      | BashRun cmd ->
                          Teams_command_text.send_bash_run ~session_manager ~key
                            ~send_text cmd
                      | DebugDumpChat ->
                          Teams_debug_dump.handle ~session_manager ~key ~config
                            ~service_url:effective_service_url ~conversation_id
                            ~reply_to_id:activity_id ~team_id ~is_group
                            ~user_group ~send_text
                      | Tools ->
                          Teams_command_text.send_tools ~session_manager
                            ~is_admin ~send_text
                      | Tasks ->
                          Teams_command_text.send_tasks ~session_manager ~key
                            ~full:false ~send_text
                      | TasksFull ->
                          Teams_command_text.send_tasks ~session_manager ~key
                            ~full:true ~send_text
                      | Costs action ->
                          let text =
                            match Session.get_db session_manager with
                            | Some db ->
                                Slash_commands.format_costs
                                  ~connector:Format_adapter.Teams ~db action
                            | None -> "Costs are not available (no database)."
                          in
                          send_text text
                      | Session action ->
                          let text =
                            match Session.get_db session_manager with
                            | Some db ->
                                Slash_commands_sessions.format_session
                                  ~connector:Format_adapter.Teams ~db action
                            | None -> "Sessions not available (no database)."
                          in
                          send_text text
                      | Usage action ->
                          let text =
                            match Session.get_db session_manager with
                            | Some db ->
                                Slash_commands.format_usage
                                  ~connector:Format_adapter.Teams ~db action
                            | None -> "Usage is not available (no database)."
                          in
                          send_text text
                      | Active ->
                          let text =
                            match Session.get_db session_manager with
                            | Some db ->
                                let cfg = Session.get_config session_manager in
                                Slash_commands.format_active
                                  ~connector:Format_adapter.Teams ~db
                                  ~config:cfg ()
                            | None ->
                                "Active usage is not available (no database)."
                          in
                          send_text text
                      | Bg action -> (
                          match Session.get_db session_manager with
                          | None ->
                              send_text
                                "Background tasks are not available (no \
                                 database)."
                          | Some db -> (
                              match action with
                              | BgCancel id ->
                                  let text =
                                    match
                                      Background_task.cancel_with_signal
                                        ~send_signal:Unix.kill
                                        ~terminate_group:(fun
                                            ?grace_seconds:_
                                            ?wait_seconds:_
                                            pid
                                          ->
                                          Lwt.async (fun () ->
                                              Process_group.terminate pid))
                                        ~db ~id ()
                                    with
                                    | Ok msg -> msg
                                    | Error msg -> msg
                                  in
                                  send_text text
                              | _ ->
                                  let* text =
                                    Slash_commands.format_bg
                                      ~connector:Format_adapter.Teams ~db action
                                  in
                                  send_text text))
                      | WorkflowRun action ->
                          let* text =
                            match Session.get_db session_manager with
                            | Some db ->
                                let config =
                                  Session.get_config session_manager
                                in
                                Slash_commands.format_workflow
                                  ~connector:Format_adapter.Teams ~db ~config
                                  ~room_id:key ~requester_id:user_id action
                            | None ->
                                Lwt.return
                                  "Workflow runs are not available (no \
                                   database)."
                          in
                          send_text text
                      | Cron action ->
                          let text =
                            match Session.get_db session_manager with
                            | Some db ->
                                Slash_commands.format_cron
                                  ~connector:Format_adapter.Teams ~db
                                  ~session_key:key
                                  ~is_admin:(user_group = "admin") action
                            | None -> "Cron is not available (no database)."
                          in
                          send_text text
                      | Bl action ->
                          let text =
                            Slash_commands.format_bl
                              ~connector:Format_adapter.Teams action
                          in
                          send_text text
                      | HeldItems action ->
                          let text =
                            match Session.get_db session_manager with
                            | Some db ->
                                Slash_commands.format_held_items
                                  ~connector:Format_adapter.Teams ~db action
                            | None ->
                                "Held items are not available (no database)."
                          in
                          send_text text
                      | Memories action ->
                          let text =
                            match Session.get_db session_manager with
                            | Some db ->
                                Slash_commands.format_memories
                                  ~connector:Format_adapter.Teams ~db action
                            | None ->
                                "Memories are not available (no database)."
                          in
                          send_text text
                      | RoomsMemory action ->
                          let text =
                            match Session.get_db session_manager with
                            | Some db ->
                                let cfg = Session.get_config session_manager in
                                Slash_commands.format_room_memories
                                  ~connector:Format_adapter.Teams ~db ~cfg
                                  ~channel_id:conversation_id ~is_admin action
                            | None -> "Room memory commands require a database."
                          in
                          send_text text
                      | ExplainAccess ->
                          let cfg = Session.get_config session_manager in
                          let access_key =
                            Connector_dispatch.room_access_key cfg key
                          in
                          let explanation =
                            Access_explanation.create ~config:cfg
                              ~session_key:access_key ()
                          in
                          let text = Access_explanation.to_text explanation in
                          send_text
                            (Format_adapter.code_block Format_adapter.Teams text)
                      | WhatCanDo as cmd ->
                          Teams_command_cards.handle ~session_manager ~key
                            ~is_admin ~config ~service_url:effective_service_url
                            ~conversation_id ~reply_to_id:activity_id
                            ~send_adaptive_card cmd
                      | Rig action -> (
                          match action with
                          | RigList ->
                              let text = Rig.list_text () in
                              send_text text
                          | RigInstall name | RigAdjust name | RigRemove name
                            -> (
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
                              | Error msg -> send_text msg
                              | Ok prompt ->
                                  let* () =
                                    send_text
                                      (Printf.sprintf
                                         "Running rig %s for '%s'..." act_str
                                         name)
                                  in
                                  Session.delegate_turn session_manager ~prompt
                                    ~parent_key:key ~debug_notify:send_text
                                    ~send_reply:send_text ();
                                  (match act with
                                  | `Install -> (
                                      match Rig.find_rig name with
                                      | Some rig ->
                                          Rig.mark_installed ~name
                                            ~version:rig.version
                                      | None -> ())
                                  | `Remove -> Rig.mark_removed ~name
                                  | `Adjust -> ());
                                  Lwt.return_unit))
                      | Repo action -> (
                          match Session.get_db session_manager with
                          | Some db ->
                              Slash_commands_repo.handle_repo_action ~db
                                ~session_key:key ~connector:Format_adapter.Teams
                                ~send_reply:send_text
                                ~set_cwd:(fun cwd ->
                                  Session.set_effective_cwd session_manager ~key
                                    ~cwd)
                                action
                          | None ->
                              send_text
                                "Repository management is not available (no \
                                 database).")
                      | Model action -> (
                          let open Slash_commands in
                          match action with
                          | ModelShow ->
                              let current =
                                Session.get_session_effective_model
                                  session_manager ~key
                              in
                              let prefs = Model_preferences.load () in
                              let usage_ranked =
                                List.filter_map
                                  (fun (m, c) ->
                                    if List.mem m prefs.favorites then None
                                    else Some (m, c))
                                  prefs.usage_counts
                              in
                              let text =
                                format_model_show
                                  ~connector:Format_adapter.Teams ~current
                                  ~favorites:prefs.favorites ~usage_ranked
                              in
                              send_text text
                          | ModelSet _ | ModelSetForce _ | ModelSetDefault _ ->
                              let* text =
                                Slash_commands_model.handle_model_set_action
                                  ~config_source:"teams" ~session_manager ~key
                                  action
                              in
                              send_text text
                          | ModelFav name ->
                              let prefs =
                                Model_preferences.toggle_favorite name
                              in
                              let status =
                                if List.mem name prefs.favorites then "added to"
                                else "removed from"
                              in
                              send_text
                                (Printf.sprintf "%s %s favorites" name status)
                          | ModelUnfav name ->
                              let _ = Model_preferences.remove_favorite name in
                              send_text
                                (Printf.sprintf "Removed from favorites: %s"
                                   name)
                          | ModelList (provider, availability) ->
                              let db_extras =
                                match Session.get_db session_manager with
                                | None -> []
                                | Some db ->
                                    Model_discovery.get_db_only_model_infos ~db
                                      ~provider_filter:provider ~availability ()
                              in
                              let models =
                                Models_catalog.to_plain_list
                                  ~provider_filter:provider ~availability
                                  ~db_extras ()
                                |> String.split_on_char '\n'
                                |> List.filter (fun s -> s <> "")
                              in
                              let text =
                                format_model_list
                                  ~connector:Format_adapter.Teams ~models
                                  ~provider
                              in
                              send_text text
                          | ModelUsage ->
                              let cfg = Session.get_config session_manager in
                              Provider_quota.set_cache_ttl cfg.quota_cache_ttl_s;
                              let results =
                                Provider_quota.get_all_cached ()
                                |> List.map (fun (_name, pq) -> pq)
                              in
                              let text =
                                Slash_commands.format_model_usage
                                  ~connector:Format_adapter.Teams ~config:cfg
                                  results
                              in
                              send_text text))))

(* Channel start — webhook-only, no polling loop needed *)
let start ~(config : Runtime_config.t) ~(_session_manager : Session.t) =
  match config.channels.teams with
  | None ->
      Logs.info (fun m -> m "Teams: no config found, skipping");
      Lwt.return_unit
  | Some tc ->
      Logs.info (fun m ->
          m "Teams: webhook channel ready at %s (app_id: %s...)" tc.webhook_path
            (String.sub tc.app_id 0 (min 8 (String.length tc.app_id))));
      Lwt.return_unit
