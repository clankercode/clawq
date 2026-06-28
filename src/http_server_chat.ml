include Http_server_0_util

let reply_json text =
  let resp_json =
    `Assoc [ ("response", `String text) ] |> Yojson.Safe.to_string
  in
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~headers:json_headers
    ~body:resp_json ()

let raw_reply_json = reply_json
let raw_sse_reply = sse_reply

let starts_with ~prefix text =
  let prefix_len = String.length prefix in
  String.length text >= prefix_len && String.sub text 0 prefix_len = prefix

let contains_case_insensitive ~needle text =
  let needle = String.lowercase_ascii needle in
  let text = String.lowercase_ascii text in
  let needle_len = String.length needle in
  let text_len = String.length text in
  let rec loop i =
    i + needle_len <= text_len
    && (String.sub text i needle_len = needle || loop (i + 1))
  in
  needle <> "" && loop 0

let slash_command_error_text text =
  let text = String.trim text in
  starts_with ~prefix:"Error:" text
  || starts_with ~prefix:"Unknown " text
  || starts_with ~prefix:"Unhandled command" text
  || contains_case_insensitive ~needle:"not found" text
  || contains_case_insensitive ~needle:"marked unavailable" text
  || contains_case_insensitive ~needle:"marked deprecated" text
  || contains_case_insensitive ~needle:"validation failed" text
  || contains_case_insensitive ~needle:"ambiguous model" text

let persist_slash_command_error session_manager ~key ~message ~response =
  match Session.get_db session_manager with
  | Some db
    when starts_with ~prefix:"/" (String.trim message)
         && slash_command_error_text response ->
      Memory.store_message ~db ~session_key:key
        (Provider.make_message ~role:"user" ~content:message);
      Memory.store_message ~db ~session_key:key
        (Provider.make_message ~role:"assistant" ~content:response);
      Lwt.return_unit
  | _ -> Lwt.return_unit

let handle_model_action ~session_manager ~key ~emit action =
  let open Lwt.Syntax in
  let open Slash_commands in
  match action with
  | ModelShow ->
      let current = Session.get_session_effective_model session_manager ~key in
      let prefs = Model_preferences.load () in
      let usage_ranked =
        List.filter_map
          (fun (m, c) ->
            if List.mem m prefs.favorites then None else Some (m, c))
          prefs.usage_counts
      in
      emit
        (Slash_commands.format_model_show ~connector:Format_adapter.Plain
           ~current ~favorites:prefs.favorites ~usage_ranked)
  | ModelSet _ | ModelSetForce _ | ModelSetDefault _ ->
      let* text =
        Slash_commands_model.handle_model_set_action
          ~config_source:"gateway_api" ~session_manager ~key action
      in
      emit text
  | ModelFav name ->
      let prefs = Model_preferences.toggle_favorite name in
      let status =
        if List.mem name prefs.favorites then "added to" else "removed from"
      in
      emit (Printf.sprintf "%s %s favorites" name status)
  | ModelUnfav name ->
      let _ = Model_preferences.remove_favorite name in
      emit (Printf.sprintf "Removed from favorites: %s" name)
  | ModelList (provider, availability) ->
      emit
        (Http_server_models.model_list_text ~session_manager ~provider
           ~availability)
  | ModelUsage ->
      let cfg = Session.get_config session_manager in
      Provider_quota.set_cache_ttl cfg.quota_cache_ttl_s;
      let* results =
        Lwt_list.map_s
          (fun (name, pc) ->
            Provider_quota.fetch_for_provider ~config:pc ~name ())
          cfg.providers
      in
      emit
        (Slash_commands.format_model_usage ~connector:Format_adapter.Plain
           ~config:cfg results)

(* Slash-command results that resolve to a single text reply, shared between the
   streaming and non-streaming chat endpoints. [emit] renders the text for the
   active endpoint (JSON body vs SSE stream). Returns [None] for results the
   caller must handle path-specifically. BashRun/DebugDumpChat always render as a
   JSON body in both endpoints, so they bypass [emit]. *)
let dispatch_common ~session_manager ~key ~emit cmd_result =
  let open Lwt.Syntax in
  match cmd_result with
  | Slash_commands.RegisterAsAdminOtc _ ->
      Some
        (emit
           "Admin registration is only available via channel connectors \
            (Telegram, Discord, Slack, etc.).")
  | Slash_commands.FormattedReply fn -> Some (emit (fn Format_adapter.Plain))
  | Slash_commands.Help | Slash_commands.Menu _ ->
      let show_test = true in
      Some
        (emit
           (Slash_commands.format_help ~connector:Format_adapter.Plain
              ~show_test ~is_admin:true ()))
  | Slash_commands.RuntimeCtx ->
      Some
        (let* response = Session.runtime_context_block session_manager ~key in
         emit response)
  | Slash_commands.Context ->
      Some
        (emit
           (Slash_commands_context.format ~connector:Format_adapter.Plain
              ~session_mgr:session_manager ~session_key:key))
  | Slash_commands.Uptime ->
      Some
        (emit
           (Daemon_status.daemon_uptime_reply
              ~pid:(Daemon_status.read_current_daemon_pid ())))
  | Slash_commands.Status ->
      Some
        (emit
           (Slash_commands.format_status ~connector:Format_adapter.Plain
              ~db:(Session.get_db session_manager)
              ~session_count:(Session.session_count session_manager)
              ~active_count:(Session.active_session_count session_manager)
              ()))
  | Slash_commands.Costs action ->
      Some
        (emit
           (match Session.get_db session_manager with
           | Some db ->
               Slash_commands.format_costs ~connector:Format_adapter.Plain ~db
                 action
           | None -> "Costs are not available (no database)."))
  | Slash_commands.Session action ->
      Some
        (emit
           (match Session.get_db session_manager with
           | Some db ->
               Slash_commands_sessions.format_session
                 ~connector:Format_adapter.Plain ~db action
           | None -> "Sessions not available (no database)."))
  | Slash_commands.Usage action ->
      Some
        (emit
           (match Session.get_db session_manager with
           | Some db ->
               Slash_commands.format_usage ~connector:Format_adapter.Plain ~db
                 action
           | None -> "Usage is not available (no database)."))
  | Slash_commands.Active ->
      Some
        (emit
           (match Session.get_db session_manager with
           | Some db ->
               let config = Session.get_config session_manager in
               Slash_commands.format_active ~connector:Format_adapter.Plain ~db
                 ~config ()
           | None -> "Active usage is not available (no database)."))
  | Slash_commands.Bg action ->
      Some
        (let* text =
           match Session.get_db session_manager with
           | Some db ->
               Slash_commands.format_bg ~connector:Format_adapter.Plain ~db
                 action
           | None ->
               Lwt.return "Background tasks are not available (no database)."
         in
         emit text)
  | Slash_commands.Cron action ->
      Some
        (emit
           (match Session.get_db session_manager with
           | Some db ->
               Slash_commands.format_cron ~connector:Format_adapter.Plain ~db
                 ~session_key:key action
           | None -> "Cron is not available (no database)."))
  | Slash_commands.Bl action ->
      Some
        (emit (Slash_commands.format_bl ~connector:Format_adapter.Plain action))
  | Slash_commands.HeldItems action ->
      Some
        (emit
           (match Session.get_db session_manager with
           | Some db ->
               Slash_commands.format_held_items ~connector:Format_adapter.Plain
                 ~db action
           | None -> "Held items are not available (no database)."))
  | Slash_commands.Memories action ->
      Some
        (emit
           (match Session.get_db session_manager with
           | Some db ->
               Slash_commands.format_memories ~connector:Format_adapter.Plain
                 ~db action
           | None -> "Memories are not available (no database)."))
  | Slash_commands.Rig action -> Some (emit (Rig.format_slash_action action))
  | Slash_commands.Repo action ->
      Some
        (let* text =
           match Session.get_db session_manager with
           | Some db ->
               let buf = Buffer.create 256 in
               let* () =
                 Slash_commands_repo.handle_repo_action ~db ~session_key:key
                   ~connector:Format_adapter.Plain
                   ~send_reply:(fun text ->
                     if Buffer.length buf > 0 then Buffer.add_char buf '\n';
                     Buffer.add_string buf text;
                     Lwt.return_unit)
                   ~set_cwd:(fun cwd ->
                     Session.set_effective_cwd session_manager ~key ~cwd)
                   action
               in
               Lwt.return (Buffer.contents buf)
           | None ->
               Lwt.return
                 "Repository management is not available (no database)."
         in
         emit text)
  | Slash_commands.Tools ->
      let show_test = true in
      Some
        (emit
           (match Session.get_tool_registry session_manager with
           | Some reg ->
               let tools, _ = Tool_registry.partition_skills reg in
               let tools = Skills.filter_visible_tools ~show_test tools in
               let skills =
                 Skills.filter_visible_tools ~show_test
                   (Skills.available_skills_as_tools ())
               in
               Slash_commands.format_tools ~connector:Format_adapter.Plain tools
                 skills
                 (Agent_template.available_templates ())
           | None -> "Tools are not enabled."))
  | Slash_commands.Model action ->
      Some (handle_model_action ~session_manager ~key ~emit action)
  | Slash_commands.Heartbeat _ ->
      Some
        (emit
           "Heartbeat routing is only available for Telegram, Slack, Discord, \
            and Teams sessions.")
  | Slash_commands.Debug action ->
      Some
        (emit
           (match action with
           | Slash_commands.DebugStatus ->
               Slash_commands_fmt.format_debug_status
                 ~connector:Format_adapter.Plain
                 (Session.session_debug_status_text session_manager ~key)
           | Slash_commands.SetDebug enabled -> (
               match
                 Session.set_session_debug session_manager ~key ~enabled
               with
               | Ok () ->
                   Slash_commands_fmt.format_debug_set
                     ~connector:Format_adapter.Plain enabled key
               | Error err -> err)))
  | Slash_commands.Followup action ->
      Some
        (let* response =
           Connector_dispatch.dispatch_followup_collect
             ~session_mgr:session_manager ~key ~connector_name:"web"
             ~channel_id:key ~user_id:"web" ~is_admin:true action
         in
         emit response)
  | Slash_commands.AgentMenu page ->
      Some
        (emit
           (Slash_commands_fmt.format_agent_menu ~connector:Format_adapter.Plain
              ~page))
  | Slash_commands.ModelMenu page ->
      Some
        (emit
           (Slash_commands_fmt.format_model_menu ~connector:Format_adapter.Plain
              ~page))
  | Slash_commands.ThinkingMenu ->
      Some
        (emit
           (Slash_commands_fmt.format_thinking_menu
              ~connector:Format_adapter.Plain))
  | Slash_commands.ConfigMenu page ->
      Some
        (emit
           (Slash_commands_fmt.format_config_menu
              ~connector:Format_adapter.Plain ~page))
  | Slash_commands.SkillsMenu page ->
      let show_test = true in
      Some
        (emit
           (Slash_commands_fmt.format_skills_menu
              ~connector:Format_adapter.Plain ~page ~show_test ()))
  | Slash_commands.CostsMenu ->
      Some
        (emit
           (Slash_commands_fmt.format_costs_menu ~connector:Format_adapter.Plain))
  | Slash_commands.BgMenu ->
      Some
        (emit
           (Slash_commands_fmt.format_bg_menu ~connector:Format_adapter.Plain))
  | Slash_commands.InjectConnectorHistory _ ->
      Some
        (emit
           "Connector history is not applicable for the gateway channel \
            \xe2\x80\x94 use /inject_connector_history in Teams or Discord \
            group chats.")
  | Slash_commands.BashRun cmd ->
      Some
        (let config = Session.get_config session_manager in
         let* result =
           Slash_commands_bash.run_bash_command ~config ~session_key:key cmd
         in
         reply_json (Slash_commands_bash.format_result cmd result))
  | Slash_commands.DebugDumpChat ->
      Some (reply_json (Session.dump_json session_manager ~key))
  | _ -> None

let handle_chat ~session_manager ~require_pairing ~auth_token ?ip_limiter
    ?session_limiter ?pairing req body =
  let open Lwt.Syntax in
  let* ip_ok =
    match ip_limiter with
    | Some lim -> Rate_limiter.check_and_consume lim ~key:(client_ip req)
    | None -> Lwt.return true
  in
  if not ip_ok then
    let* _ = Cohttp_lwt.Body.drain_body body in
    rate_limit_response ()
  else if require_pairing && not (pairing_auth_ok ~auth_token ?pairing req) then
    let* _ = Cohttp_lwt.Body.drain_body body in
    Cohttp_lwt_unix.Server.respond_string ~status:`Forbidden
      ~headers:json_headers
      ~body:
        {|{"error":"pairing required; use a valid paired token to access this endpoint"}|}
      ()
  else if not (auth_ok ~auth_token ?pairing req) then
    let* _ = Cohttp_lwt.Body.drain_body body in
    Cohttp_lwt_unix.Server.respond_string ~status:`Unauthorized
      ~headers:json_headers ~body:{|{"error":"unauthorized"}|} ()
  else
    let* body_str = Cohttp_lwt.Body.to_string body in
    let json =
      try Ok (Yojson.Safe.from_string body_str)
      with exn -> Error (Printexc.to_string exn)
    in
    match json with
    | Error msg ->
        Cohttp_lwt_unix.Server.respond_string ~status:`Bad_request
          ~headers:json_headers
          ~body:
            (Yojson.Safe.to_string
               (`Assoc [ ("error", `String ("invalid JSON: " ^ msg)) ]))
          ()
    | Ok json -> (
        let open Yojson.Safe.Util in
        let session_id =
          try json |> member "session_id" |> to_string with _ -> ""
        in
        let session_id = Session.sanitize_session_key session_id in
        let message =
          try json |> member "message" |> to_string with _ -> ""
        in
        if session_id = "" then bad_request "session_id is required"
        else if message = "" then bad_request "message is required"
        else
          let* sess_ok =
            match session_limiter with
            | Some lim -> Rate_limiter.check_and_consume lim ~key:session_id
            | None -> Lwt.return true
          in
          if not sess_ok then rate_limit_response ()
          else
            let key = "web:" ^ session_id in
            let reply_json text =
              let* () =
                persist_slash_command_error session_manager ~key ~message
                  ~response:text
              in
              raw_reply_json text
            in
            let skill_names =
              List.map
                (fun (s : Skills.skill_md_meta) -> s.md_name)
                (Skills.available_skills ())
            in
            let* cmd_result, message, skill_injections, _loaded_skill_name =
              match Slash_commands.handle ~skill_names message with
              | Slash_commands.SkillInvoke (name, args) -> (
                  if
                    args = ""
                    && Session.skill_loaded_in_context session_manager ~key name
                  then Lwt.return (Slash_commands.NotACommand, message, [], None)
                  else
                    let* result = Skills.expand_slash_skill ~name ~args () in
                    match result with
                    | Ok r ->
                        Lwt.return
                          ( Slash_commands.NotACommand,
                            message,
                            [ r.skill_injection ],
                            Some name )
                    | Error err_msg ->
                        Lwt.return
                          (Slash_commands.Reply err_msg, message, [], None))
              | other -> Lwt.return (other, message, [], None)
            in
            let cmd_result =
              Slash_commands.gate_admin ~is_admin:true cmd_result
            in
            match cmd_result with
            | Slash_commands.AdminRequired _ -> assert false
            | Slash_commands.AgentInvoke (agent_name, prompt) ->
                let waiter, resolver = Lwt.wait () in
                Session.agent_invoke_turn session_manager ~agent_name
                  ~parent_key:key ~prompt
                  ~send_reply:(fun text ->
                    Lwt.wakeup_later resolver text;
                    Lwt.return_unit)
                  ();
                let* response = waiter in
                reply_json response
            | Slash_commands.Debate prompt -> (
                match Session.get_db session_manager with
                | Some db ->
                    let config = Session.get_config session_manager in
                    let debug_notify, debug_fields =
                      make_json_debug_capture ()
                    in
                    let on_llm_call_debug =
                      Session.debug_callback_for session_manager ~key
                        (Some debug_notify)
                    in
                    let* text =
                      Debate.run_for_prompt ?on_llm_call_debug ~config ~db
                        ~prompt ()
                    in
                    let resp_json =
                      `Assoc ([ ("response", `String text) ] @ debug_fields ())
                      |> Yojson.Safe.to_string
                    in
                    Cohttp_lwt_unix.Server.respond_string ~status:`OK
                      ~headers:json_headers ~body:resp_json ()
                | None ->
                    let resp_json =
                      `Assoc
                        [ ("response", `String "Debate requires a database.") ]
                      |> Yojson.Safe.to_string
                    in
                    Cohttp_lwt_unix.Server.respond_string ~status:`OK
                      ~headers:json_headers ~body:resp_json ())
            | other -> (
                match
                  dispatch_common ~session_manager ~key ~emit:reply_json other
                with
                | Some r -> r
                | None -> (
                    let loaded_notifications = ref [] in
                    let debug_notify, debug_fields =
                      make_json_debug_capture ()
                    in
                    let* result =
                      Lwt.catch
                        (fun () ->
                          let* response =
                            Session.with_registered_notifier session_manager
                              ~key
                              ~notify:(fun text ->
                                let* () = debug_notify text in
                                if not (has_prefix ~prefix:"debug:" text) then
                                  loaded_notifications :=
                                    !loaded_notifications @ [ text ];
                                Lwt.return_unit)
                              (fun () ->
                                Session.turn session_manager ~key ~message
                                  ~skill_injections ~user_group:"admin"
                                  ~snapshot_work_type:Access_snapshot.Room_turn
                                  ())
                          in
                          Lwt.return (Ok response))
                        (fun exn -> Lwt.return (Error (Printexc.to_string exn)))
                    in
                    match result with
                    | Ok response ->
                        if
                          not
                            (Session.take_response_deferred session_manager ~key)
                        then Session.mark_response_sent session_manager ~key;
                        let response =
                          match !loaded_notifications with
                          | [] -> response
                          | notes ->
                              Printf.sprintf "%s\n\n%s"
                                (String.concat "\n" notes) response
                        in
                        let resp_json =
                          `Assoc
                            ([ ("response", `String response) ]
                            @ debug_fields ())
                          |> Yojson.Safe.to_string
                        in
                        Cohttp_lwt_unix.Server.respond_string ~status:`OK
                          ~headers:json_headers ~body:resp_json ()
                    | Error err ->
                        if
                          not
                            (Session.take_response_deferred session_manager ~key)
                        then Session.mark_response_sent session_manager ~key;
                        Cohttp_lwt_unix.Server.respond_string
                          ~status:`Internal_server_error ~headers:json_headers
                          ~body:
                            (Yojson.Safe.to_string
                               (`Assoc [ ("error", `String err) ]))
                          ())))

let handle_chat_stream ~session_manager ~require_pairing ~auth_token ?ip_limiter
    ?session_limiter ?pairing req body =
  let open Lwt.Syntax in
  let* ip_ok =
    match ip_limiter with
    | Some lim -> Rate_limiter.check_and_consume lim ~key:(client_ip req)
    | None -> Lwt.return true
  in
  if not ip_ok then
    let* _ = Cohttp_lwt.Body.drain_body body in
    rate_limit_response ()
  else if require_pairing && not (pairing_auth_ok ~auth_token ?pairing req) then
    let* _ = Cohttp_lwt.Body.drain_body body in
    Cohttp_lwt_unix.Server.respond_string ~status:`Forbidden
      ~headers:json_headers
      ~body:
        {|{"error":"pairing required; use a valid paired token to access this endpoint"}|}
      ()
  else if not (auth_ok ~auth_token ?pairing req) then
    let* _ = Cohttp_lwt.Body.drain_body body in
    Cohttp_lwt_unix.Server.respond_string ~status:`Unauthorized
      ~headers:json_headers ~body:{|{"error":"unauthorized"}|} ()
  else
    let* body_str = Cohttp_lwt.Body.to_string body in
    let json =
      try Ok (Yojson.Safe.from_string body_str)
      with exn -> Error (Printexc.to_string exn)
    in
    match json with
    | Error msg ->
        Cohttp_lwt_unix.Server.respond_string ~status:`Bad_request
          ~headers:json_headers
          ~body:
            (Yojson.Safe.to_string
               (`Assoc [ ("error", `String ("invalid JSON: " ^ msg)) ]))
          ()
    | Ok json -> (
        let open Yojson.Safe.Util in
        let session_id =
          try json |> member "session_id" |> to_string with _ -> ""
        in
        let session_id = Session.sanitize_session_key session_id in
        let message =
          try json |> member "message" |> to_string with _ -> ""
        in
        if session_id = "" then
          Cohttp_lwt_unix.Server.respond_string ~status:`Bad_request
            ~headers:json_headers ~body:{|{"error":"session_id is required"}|}
            ()
        else if message = "" then
          Cohttp_lwt_unix.Server.respond_string ~status:`Bad_request
            ~headers:json_headers ~body:{|{"error":"message is required"}|} ()
        else
          let* sess_ok =
            match session_limiter with
            | Some lim -> Rate_limiter.check_and_consume lim ~key:session_id
            | None -> Lwt.return true
          in
          if not sess_ok then rate_limit_response ()
          else
            let key = "web:" ^ session_id in
            let sse_reply text =
              let* () =
                persist_slash_command_error session_manager ~key ~message
                  ~response:text
              in
              raw_sse_reply text
            in
            let skill_names =
              List.map
                (fun (s : Skills.skill_md_meta) -> s.md_name)
                (Skills.available_skills ())
            in
            let* cmd_result, message, skill_injections, _loaded_skill_name =
              match Slash_commands.handle ~skill_names message with
              | Slash_commands.SkillInvoke (name, args) -> (
                  if
                    args = ""
                    && Session.skill_loaded_in_context session_manager ~key name
                  then Lwt.return (Slash_commands.NotACommand, message, [], None)
                  else
                    let* result = Skills.expand_slash_skill ~name ~args () in
                    match result with
                    | Ok r ->
                        Lwt.return
                          ( Slash_commands.NotACommand,
                            message,
                            [ r.skill_injection ],
                            Some name )
                    | Error err_msg ->
                        Lwt.return
                          (Slash_commands.Reply err_msg, message, [], None))
              | other -> Lwt.return (other, message, [], None)
            in
            let cmd_result =
              Slash_commands.gate_admin ~is_admin:true cmd_result
            in
            match cmd_result with
            | Slash_commands.AdminRequired _ -> assert false
            | Slash_commands.Reply text -> sse_reply text
            | Slash_commands.Reset ->
                let key = "web:" ^ session_id in
                let* active_bg_tasks = Session.reset session_manager ~key in
                sse_reply (Slash_commands.reset_message ~active_bg_tasks ())
            | Slash_commands.Compact ->
                let key = "web:" ^ session_id in
                let stream, push = Lwt_stream.create () in
                let push_sse text =
                  let data =
                    Yojson.Safe.to_string
                      (json_of_stream_event (Provider.Delta text))
                  in
                  push (Some (Printf.sprintf "data: %s\n\n" data))
                in
                let notifier : Status_message.notifier =
                  {
                    send =
                      (fun ?parse_mode:_ text ->
                        push_sse text;
                        Lwt.return "web-compact");
                    edit =
                      (fun _id ?parse_mode:_ text ->
                        push_sse text;
                        Lwt.return_none);
                    delete = (fun _id -> Lwt.return_unit);
                  }
                in
                Lwt.async (fun () ->
                    Lwt.catch
                      (fun () ->
                        let* compact_result =
                          Session.compact session_manager ~key ~notifier ()
                        in
                        let text =
                          match compact_result with
                          | Ok true -> "\xe2\x9c\x85 Session history compacted."
                          | Ok false ->
                              "Nothing to compact \xe2\x80\x94 session history \
                               is already short enough."
                          | Error err ->
                              Printf.sprintf "Compaction failed: %s" err
                        in
                        push_sse text;
                        push (Some "data: [DONE]\n\n");
                        push None;
                        Lwt.return_unit)
                      (fun exn ->
                        push_sse
                          (Printf.sprintf "Compaction failed: %s"
                             (Printexc.to_string exn));
                        push (Some "data: [DONE]\n\n");
                        push None;
                        Lwt.return_unit));
                let headers =
                  Cohttp.Header.of_list
                    [
                      ("Content-Type", "text/event-stream");
                      ("Cache-Control", "no-cache");
                      ("Connection", "keep-alive");
                    ]
                in
                Cohttp_lwt_unix.Server.respond ~status:`OK ~headers
                  ~body:(Cohttp_lwt.Body.of_stream stream)
                  ()
            | Slash_commands.Thinking Slash_commands.ShowThinking ->
                let current =
                  (Session.get_config session_manager).agent_defaults
                    .reasoning_effort
                in
                sse_reply
                  (Slash_commands_fmt.format_thinking_status
                     ~connector:Format_adapter.Plain current)
            | Slash_commands.Thinking (Slash_commands.SetThinking level) ->
                let cfg = Session.get_config session_manager in
                let previous = cfg.agent_defaults.reasoning_effort in
                let text =
                  match Config_set.set_reasoning_effort level with
                  | Ok () ->
                      let agent_defaults =
                        { cfg.agent_defaults with reasoning_effort = level }
                      in
                      Session.update_config ~source:"gateway_api"
                        session_manager
                        { cfg with agent_defaults };
                      Slash_commands_fmt.format_thinking_set
                        ~connector:Format_adapter.Plain ~previous level
                  | Error err -> err
                in
                sse_reply text
            | Slash_commands.ShowThinking action ->
                let connector = Format_adapter.Plain in
                let cfg = Session.get_config session_manager in
                let current = cfg.agent_defaults.show_thinking in
                let text =
                  match action with
                  | Slash_commands.ShowThinkingStatus ->
                      Slash_commands_fmt.format_show_thinking_status ~connector
                        current
                  | Slash_commands.ToggleShowThinking -> (
                      let new_val = not current in
                      match Config_set.set_show_thinking new_val with
                      | Ok () ->
                          let agent_defaults =
                            { cfg.agent_defaults with show_thinking = new_val }
                          in
                          Session.update_config ~source:"gateway_api"
                            session_manager
                            { cfg with agent_defaults };
                          Slash_commands_fmt.format_show_thinking_toggle
                            ~connector new_val
                      | Error err -> "Failed to update show_thinking: " ^ err)
                in
                sse_reply text
            | Slash_commands.Tasks ->
                let key = "web:" ^ session_id in
                let text =
                  match Session.get_db session_manager with
                  | Some db ->
                      Task_tree.init_schema db;
                      Task_tree.render_emoji_tree ~db ~session_key:key ()
                  | None -> "Tasks are not available (no database)."
                in
                sse_reply text
            | Slash_commands.TasksFull ->
                let key = "web:" ^ session_id in
                let text =
                  match Session.get_db session_manager with
                  | Some db ->
                      Task_tree.init_schema db;
                      Task_tree.render_tree_with_legend ~db ~session_key:key
                  | None -> "Tasks are not available (no database)."
                in
                sse_reply text
            | Slash_commands.Delegate (agent_name, prompt) ->
                let key = "web:" ^ session_id in
                let stream, push = Lwt_stream.create () in
                let push_sse text =
                  let data =
                    Yojson.Safe.to_string
                      (json_of_stream_event (Provider.Delta text))
                  in
                  push (Some (Printf.sprintf "data: %s\n\n" data))
                in
                let push_sse_lwt text =
                  push_sse text;
                  Lwt.return_unit
                in
                push_sse "Delegating...";
                Session.delegate_turn session_manager ?agent_name ~prompt
                  ~parent_key:key ~debug_notify:push_sse_lwt
                  ~send_reply:(fun text ->
                    push_sse text;
                    push (Some "data: [DONE]\n\n");
                    push None;
                    Lwt.return_unit)
                  ();
                let headers =
                  Cohttp.Header.of_list
                    [
                      ("Content-Type", "text/event-stream");
                      ("Cache-Control", "no-cache");
                      ("Connection", "keep-alive");
                    ]
                in
                Cohttp_lwt_unix.Server.respond ~status:`OK ~headers
                  ~body:(Cohttp_lwt.Body.of_stream stream)
                  ()
            | Slash_commands.AgentInvoke (agent_name, prompt) ->
                let key = "web:" ^ session_id in
                let stream, push = Lwt_stream.create () in
                let push_sse text =
                  let data =
                    Yojson.Safe.to_string
                      (json_of_stream_event (Provider.Delta text))
                  in
                  push (Some (Printf.sprintf "data: %s\n\n" data))
                in
                let push_sse_lwt text =
                  push_sse text;
                  Lwt.return_unit
                in
                push_sse (Printf.sprintf "Invoking agent '%s'..." agent_name);
                Session.agent_invoke_turn session_manager ~agent_name
                  ~parent_key:key ~debug_notify:push_sse_lwt ~prompt
                  ~send_reply:(fun text ->
                    push_sse text;
                    push (Some "data: [DONE]\n\n");
                    push None;
                    Lwt.return_unit)
                  ();
                let headers =
                  Cohttp.Header.of_list
                    [
                      ("Content-Type", "text/event-stream");
                      ("Cache-Control", "no-cache");
                      ("Connection", "keep-alive");
                    ]
                in
                Cohttp_lwt_unix.Server.respond ~status:`OK ~headers
                  ~body:(Cohttp_lwt.Body.of_stream stream)
                  ()
            | Slash_commands.AgentMenu page ->
                let text =
                  Slash_commands_fmt.format_agent_menu
                    ~connector:Format_adapter.Plain ~page
                in
                let stream, push = Lwt_stream.create () in
                let data =
                  Yojson.Safe.to_string
                    (json_of_stream_event (Provider.Delta text))
                in
                push (Some (Printf.sprintf "data: %s\n\n" data));
                push (Some "data: [DONE]\n\n");
                push None;
                let headers =
                  Cohttp.Header.of_list
                    [
                      ("Content-Type", "text/event-stream");
                      ("Cache-Control", "no-cache");
                      ("Connection", "keep-alive");
                    ]
                in
                Cohttp_lwt_unix.Server.respond ~status:`OK ~headers
                  ~body:(Cohttp_lwt.Body.of_stream stream)
                  ()
            | Slash_commands.ForkAnd (agent_name, prompt) ->
                let key = "web:" ^ session_id in
                let stream, push = Lwt_stream.create () in
                let push_sse text =
                  let data =
                    Yojson.Safe.to_string
                      (json_of_stream_event (Provider.Delta text))
                  in
                  push (Some (Printf.sprintf "data: %s\n\n" data))
                in
                let push_sse_lwt text =
                  push_sse text;
                  Lwt.return_unit
                in
                push_sse "Forking session...";
                Session.fork_and_run session_manager ~parent_key:key
                  ~debug_notify:push_sse_lwt ?agent_name ~prompt
                  ~send_reply:(fun text ->
                    push_sse text;
                    push (Some "data: [DONE]\n\n");
                    push None;
                    Lwt.return_unit)
                  ();
                let headers =
                  Cohttp.Header.of_list
                    [
                      ("Content-Type", "text/event-stream");
                      ("Cache-Control", "no-cache");
                      ("Connection", "keep-alive");
                    ]
                in
                Cohttp_lwt_unix.Server.respond ~status:`OK ~headers
                  ~body:(Cohttp_lwt.Body.of_stream stream)
                  ()
            | Slash_commands.Debate prompt -> (
                let key = "web:" ^ session_id in
                match Session.get_db session_manager with
                | Some db ->
                    let stream, push = Lwt_stream.create () in
                    let push_sse text =
                      let data =
                        Yojson.Safe.to_string
                          (json_of_stream_event (Provider.Delta text))
                      in
                      push (Some (Printf.sprintf "data: %s\n\n" data))
                    in
                    let push_sse_lwt text =
                      push_sse text;
                      Lwt.return_unit
                    in
                    let on_llm_call_debug =
                      Session.debug_callback_for session_manager ~key
                        (Some push_sse_lwt)
                    in
                    let config = Session.get_config session_manager in
                    let* text =
                      Debate.run_for_prompt ?on_llm_call_debug ~config ~db
                        ~prompt ()
                    in
                    push_sse text;
                    push (Some "data: [DONE]\n\n");
                    push None;
                    let headers =
                      Cohttp.Header.of_list
                        [
                          ("Content-Type", "text/event-stream");
                          ("Cache-Control", "no-cache");
                          ("Connection", "keep-alive");
                        ]
                    in
                    Cohttp_lwt_unix.Server.respond ~status:`OK ~headers
                      ~body:(Cohttp_lwt.Body.of_stream stream)
                      ()
                | None -> sse_reply "Debate requires a database.")
            | Slash_commands.Model action ->
                handle_model_action ~session_manager ~key ~emit:sse_reply action
            | Slash_commands.SkillInvoke _ ->
                sse_reply "Error: unexpected SkillInvoke"
            | Slash_commands.NotACommand ->
                let key = "web:" ^ session_id in
                let stream, push = Lwt_stream.create () in
                Lwt.async (fun () ->
                    Session.with_registered_notifier session_manager ~key
                      ~notify:(fun text ->
                        let data =
                          Yojson.Safe.to_string
                            (json_of_stream_event (Provider.Delta text))
                        in
                        push (Some (Printf.sprintf "data: %s\n\n" data));
                        Lwt.return_unit)
                      (fun () ->
                        Lwt.catch
                          (fun () ->
                            let* _response =
                              Session.turn_stream session_manager ~key ~message
                                ~skill_injections ~user_group:"admin"
                                ~on_chunk:(fun chunk ->
                                  let data =
                                    Yojson.Safe.to_string
                                      (json_of_stream_event chunk)
                                  in
                                  push
                                    (Some (Printf.sprintf "data: %s\n\n" data));
                                  Lwt.return_unit)
                                ()
                            in
                            if
                              not
                                (Session.take_response_deferred session_manager
                                   ~key)
                            then Session.mark_response_sent session_manager ~key;
                            push (Some "data: [DONE]\n\n");
                            push None;
                            Lwt.return_unit)
                          (fun exn ->
                            let err = Printexc.to_string exn in
                            if
                              not
                                (Session.take_response_deferred session_manager
                                   ~key)
                            then Session.mark_response_sent session_manager ~key;
                            push
                              (Some
                                 (Printf.sprintf
                                    "data: {\"type\":\"error\",\"message\":%s}\n\n"
                                    (Yojson.Safe.to_string (`String err))));
                            push (Some "data: [DONE]\n\n");
                            push None;
                            Lwt.return_unit)));
                let headers =
                  Cohttp.Header.of_list
                    [
                      ("Content-Type", "text/event-stream");
                      ("Cache-Control", "no-cache");
                      ("Connection", "keep-alive");
                    ]
                in
                Cohttp_lwt_unix.Server.respond ~status:`OK ~headers
                  ~body:(Cohttp_lwt.Body.of_stream stream)
                  ()
            | other -> (
                match
                  dispatch_common ~session_manager ~key ~emit:sse_reply other
                with
                | Some r -> r
                | None -> sse_reply "Unhandled command."))
