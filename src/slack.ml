type event =
  | UrlVerification of string
  | Message of {
      channel_id : string;
      user_id : string;
      text : string;
      bot_id : string option;
      ts : string;
    }
  | Other

let current_thinking_message current =
  Printf.sprintf "Current thinking level: %s"
    (Slash_commands.thinking_level_to_string current)

let is_allowed_allowlist ~kind ~id allowlist =
  let coq_allowed = Clawq_core.is_allowed0 id allowlist in
  let ocaml_allowed =
    match allowlist with [ "*" ] -> true | ids -> List.mem id ids
  in
  if coq_allowed <> ocaml_allowed then
    Logs.warn (fun m ->
        m "Slack allowlist drift for %s=%s: Coq=%b OCaml=%b" kind id coq_allowed
          ocaml_allowed);
  coq_allowed

let set_thinking_level ~(session_manager : Session.t) ~channel_id ~user_id level
    =
  let cfg = Session.get_config session_manager in
  let previous = cfg.agent_defaults.reasoning_effort in
  match Config_set.set_reasoning_effort level with
  | Ok () ->
      let agent_defaults =
        { cfg.agent_defaults with reasoning_effort = level }
      in
      Session.update_config ~source:"slack" session_manager
        { cfg with agent_defaults };
      Logs.info (fun m ->
          m "Slack thinking level updated channel=%s user=%s from=%s to=%s"
            channel_id user_id
            (Slash_commands.thinking_level_to_string previous)
            (Slash_commands.thinking_level_to_string level));
      Printf.sprintf "Thinking level changed from %s to %s."
        (Slash_commands.thinking_level_to_string previous)
        (Slash_commands.thinking_level_to_string level)
  | Error err ->
      Logs.err (fun m ->
          m "Slack thinking level update failed channel=%s user=%s: %s"
            channel_id user_id err);
      "Failed to update thinking level: " ^ err

let is_allowed ~(config : Runtime_config.slack_config) ~channel_id ~user_id =
  let ch_ok =
    is_allowed_allowlist ~kind:"channel" ~id:channel_id config.allow_channels
  in
  let usr_ok =
    is_allowed_allowlist ~kind:"user" ~id:user_id config.allow_users
  in
  ch_ok && usr_ok

let verify_signature ~signing_secret ~timestamp ~body ~signature =
  let now = Unix.gettimeofday () in
  let ts = try float_of_string timestamp with _ -> 0.0 in
  if Float.abs (now -. ts) > 300.0 then false
  else
    let basestring = "v0:" ^ timestamp ^ ":" ^ body in
    let expected =
      "v0="
      ^ Digestif.SHA256.(hmac_string ~key:signing_secret basestring |> to_hex)
    in
    Eqaf.equal expected signature

let send_message ~bot_token ~channel_id ~text =
  let open Lwt.Syntax in
  let uri = "https://slack.com/api/chat.postMessage" in
  let headers = [ ("Authorization", "Bearer " ^ bot_token) ] in
  let body =
    `Assoc [ ("channel", `String channel_id); ("text", `String text) ]
    |> Yojson.Safe.to_string
  in
  let* _status, _body = Http_client.post_json ~uri ~headers ~body in
  Lwt.return_unit

let send_message_with_id ~bot_token ~channel_id ~text =
  let open Lwt.Syntax in
  let uri = "https://slack.com/api/chat.postMessage" in
  let headers = [ ("Authorization", "Bearer " ^ bot_token) ] in
  let body =
    `Assoc [ ("channel", `String channel_id); ("text", `String text) ]
    |> Yojson.Safe.to_string
  in
  let* _status, resp_body = Http_client.post_json ~uri ~headers ~body in
  let ts =
    try
      let json = Yojson.Safe.from_string resp_body in
      json |> Yojson.Safe.Util.member "ts" |> Yojson.Safe.Util.to_string
    with _ -> "0"
  in
  Lwt.return ts

let edit_message ~bot_token ~channel_id ~ts ~text =
  let open Lwt.Syntax in
  let uri = "https://slack.com/api/chat.update" in
  let headers = [ ("Authorization", "Bearer " ^ bot_token) ] in
  let body =
    `Assoc
      [
        ("channel", `String channel_id);
        ("ts", `String ts);
        ("text", `String text);
      ]
    |> Yojson.Safe.to_string
  in
  let* _status, _body = Http_client.post_json ~uri ~headers ~body in
  Lwt.return_unit

let delete_message ~bot_token ~channel_id ~ts =
  let open Lwt.Syntax in
  let uri = "https://slack.com/api/chat.delete" in
  let headers = [ ("Authorization", "Bearer " ^ bot_token) ] in
  let body =
    `Assoc [ ("channel", `String channel_id); ("ts", `String ts) ]
    |> Yojson.Safe.to_string
  in
  let* _status, _body = Http_client.post_json ~uri ~headers ~body in
  Lwt.return_unit

let make_status_notifier ~bot_token ~channel_id : Status_message.notifier =
  {
    send =
      (fun ?parse_mode:_ text ->
        send_message_with_id ~bot_token ~channel_id ~text);
    edit =
      (fun ts ?parse_mode:_ text ->
        let open Lwt.Syntax in
        let* () = edit_message ~bot_token ~channel_id ~ts ~text in
        Lwt.return None);
    delete = (fun ts -> delete_message ~bot_token ~channel_id ~ts);
  }

let add_reaction ~bot_token ~channel_id ~timestamp ~emoji_name =
  let open Lwt.Syntax in
  let uri = "https://slack.com/api/reactions.add" in
  let headers = [ ("Authorization", "Bearer " ^ bot_token) ] in
  let body =
    `Assoc
      [
        ("channel", `String channel_id);
        ("timestamp", `String timestamp);
        ("name", `String emoji_name);
      ]
    |> Yojson.Safe.to_string
  in
  let* _status, _body = Http_client.post_json ~uri ~headers ~body in
  Lwt.return_unit

let remove_reaction ~bot_token ~channel_id ~timestamp ~emoji_name =
  let open Lwt.Syntax in
  let uri = "https://slack.com/api/reactions.remove" in
  let headers = [ ("Authorization", "Bearer " ^ bot_token) ] in
  let body =
    `Assoc
      [
        ("channel", `String channel_id);
        ("timestamp", `String timestamp);
        ("name", `String emoji_name);
      ]
    |> Yojson.Safe.to_string
  in
  let* _status, _body = Http_client.post_json ~uri ~headers ~body in
  Lwt.return_unit

let _rate_limit_warnings : (string, float) Hashtbl.t = Hashtbl.create 16

(* Tracks message timestamps whose reactions should be kept in sync per session key *)
let reactions : string Reaction_tracker.t = Reaction_tracker.create ()

let parse_event body =
  try
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    let typ = json |> member "type" |> to_string in
    match typ with
    | "url_verification" ->
        let challenge = json |> member "challenge" |> to_string in
        Some (UrlVerification challenge)
    | "event_callback" -> (
        let evt = json |> member "event" in
        let evt_type = evt |> member "type" |> to_string in
        match evt_type with
        | "message" ->
            let channel_id = evt |> member "channel" |> to_string in
            let user_id =
              try evt |> member "user" |> to_string with _ -> ""
            in
            let text = try evt |> member "text" |> to_string with _ -> "" in
            let bot_id =
              try Some (evt |> member "bot_id" |> to_string) with _ -> None
            in
            let ts = try evt |> member "ts" |> to_string with _ -> "" in
            Some (Message { channel_id; user_id; text; bot_id; ts })
        | _ -> Some Other)
    | _ -> Some Other
  with _ -> None

let handle_event ~(config : Runtime_config.slack_config)
    ~(session_manager : Session.t) ?(send_message_fn = send_message)
    ?run_update_command ?event_limiter body =
  let open Lwt.Syntax in
  match parse_event body with
  | Some (UrlVerification challenge) ->
      let resp =
        `Assoc [ ("challenge", `String challenge) ] |> Yojson.Safe.to_string
      in
      Lwt.return resp
  | Some (Message { bot_id = Some _; _ }) -> Lwt.return "ok"
  | Some (Message { channel_id; user_id; text; bot_id = None; ts }) ->
      if not (is_allowed ~config ~channel_id ~user_id) then begin
        Logs.warn (fun m ->
            m "Slack: ignoring message from unauthorized channel=%s user=%s"
              channel_id user_id);
        Lwt.return "ok"
      end
      else if user_id = "" then begin
        Logs.debug (fun m -> m "Slack: dropping message with empty user_id");
        Lwt.return "ok"
      end
      else begin
        let limiter_key = channel_id ^ ":" ^ user_id in
        let* rate_ok =
          match event_limiter with
          | Some lim -> Rate_limiter.check_and_consume lim ~key:limiter_key
          | None -> Lwt.return true
        in
        if not rate_ok then begin
          let now = Unix.gettimeofday () in
          let should_warn =
            match Hashtbl.find_opt _rate_limit_warnings limiter_key with
            | Some last -> now -. last >= 60.0
            | None -> true
          in
          if should_warn then begin
            Hashtbl.replace _rate_limit_warnings limiter_key now;
            let* () =
              send_message_fn ~bot_token:config.bot_token ~channel_id
                ~text:
                  "Please slow down, I can only process a limited number of \
                   messages per minute."
            in
            Lwt.return "ok"
          end
          else Lwt.return "ok"
        end
        else
          let key = "slack:" ^ channel_id ^ ":" ^ user_id in
          (* Register a persistent channel notifier so autonomous continuation
             responses can reach the Slack channel *)
          let send_to_channel_persistent text =
            send_message_fn ~bot_token:config.bot_token ~channel_id ~text
          in
          if
            Option.is_none
              (Session.find_registered_notifier session_manager ~key)
          then begin
            Session.register_channel_notifier session_manager ~key
              send_to_channel_persistent;
            Session.register_status_message_factory session_manager ~key
              (fun () ->
                let notifier =
                  make_status_notifier ~bot_token:config.bot_token ~channel_id
                in
                Status_message.create ~notifier
                  ~parse_mode:Connector_status.Slack.status_parse_mode ())
          end;
          if Update_tool.is_update_command text then begin
            let notify text =
              send_message_fn ~bot_token:config.bot_token ~channel_id ~text
            in
            let send_first text =
              send_message_with_id ~bot_token:config.bot_token ~channel_id ~text
            in
            let edit_msg ts text =
              edit_message ~bot_token:config.bot_token ~channel_id ~ts ~text
            in
            let progress_send, _get_final =
              Update_tool.make_progress_sender ~send_first ~edit:edit_msg
                ~mode:Update_tool.Auto ()
            in
            let run_update_command, send_progress =
              match run_update_command with
              | Some run_update_command -> (run_update_command, notify)
              | None ->
                  ( (fun ?(mode = Update_tool.Auto)
                      ?prepare_restart:_
                      ~send_progress
                      ()
                    ->
                      Update_tool.run_update ~mode
                        ~is_draining:(fun () ->
                          Session.is_draining session_manager)
                        ~send_progress ()),
                    progress_send )
            in
            Session.register_channel_notifier session_manager ~key notify;
            Logs.info (fun m ->
                m
                  "Slack: /update command from channel=%s user=%s, initiating \
                   update"
                  channel_id user_id);
            Lwt.async (fun () ->
                Lwt.finalize
                  (fun () ->
                    Lwt.catch
                      (fun () ->
                        let* _response =
                          run_update_command
                            ~prepare_restart:(fun () ->
                              Restart_notify.write ~channel:"slack" ~channel_id;
                              Lwt.return (Ok ()))
                            ~send_progress ()
                        in
                        Lwt.return_unit)
                      (fun exn ->
                        notify
                          (Printf.sprintf
                             "Sorry, an error occurred processing your \
                              message: %s"
                             (Printexc.to_string exn))))
                  (fun () ->
                    Session.unregister_channel_notifier session_manager ~key;
                    Lwt.return_unit));
            Lwt.return "ok"
          end
          else
            match Slash_commands.handle text with
            | Reply reply_text ->
                let* () =
                  send_message_fn ~bot_token:config.bot_token ~channel_id
                    ~text:reply_text
                in
                Lwt.return "ok"
            | Reset ->
                let* () = Session.reset session_manager ~key in
                let* () =
                  send_message_fn ~bot_token:config.bot_token ~channel_id
                    ~text:Slash_commands.reset_message
                in
                Lwt.return "ok"
            | Compact ->
                let notifier =
                  make_status_notifier ~bot_token:config.bot_token ~channel_id
                in
                let* compact_result =
                  Session.compact session_manager ~key ~notifier ()
                in
                let* () =
                  match compact_result with
                  | Ok _ ->
                      (* Progress/result message handled by session.compact via notifier *)
                      Lwt.return_unit
                  | Error err ->
                      send_message_fn ~bot_token:config.bot_token ~channel_id
                        ~text:(Printf.sprintf "Compaction failed: %s" err)
                in
                Lwt.return "ok"
            | RuntimeCtx ->
                let* text =
                  Session.runtime_context_block session_manager ~key
                in
                let* () =
                  send_message_fn ~bot_token:config.bot_token ~channel_id ~text
                in
                Lwt.return "ok"
            | Thinking Slash_commands.ShowThinking ->
                let current =
                  (Session.get_config session_manager).agent_defaults
                    .reasoning_effort
                in
                let* () =
                  send_message_fn ~bot_token:config.bot_token ~channel_id
                    ~text:(current_thinking_message current)
                in
                Lwt.return "ok"
            | Thinking (Slash_commands.SetThinking level) ->
                let* () =
                  send_message_fn ~bot_token:config.bot_token ~channel_id
                    ~text:
                      (set_thinking_level ~session_manager ~channel_id ~user_id
                         level)
                in
                Lwt.return "ok"
            | ShowThinking action ->
                let cfg = Session.get_config session_manager in
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
                          Session.update_config ~source:"slack" session_manager
                            { cfg with agent_defaults };
                          Logs.info (fun m ->
                              m
                                "Slack show_thinking toggled channel_id=%s \
                                 user_id=%s from=%b to=%b"
                                channel_id user_id current new_val);
                          Printf.sprintf "Show thinking: %s"
                            (if new_val then "on" else "off")
                      | Error err -> "Failed to update show_thinking: " ^ err)
                in
                let* () =
                  send_message_fn ~bot_token:config.bot_token ~channel_id ~text
                in
                Lwt.return "ok"
            | Delegate prompt ->
                Lwt.async (fun () ->
                    send_message_fn ~bot_token:config.bot_token ~channel_id
                      ~text:"Delegating to a temporary session...");
                Session.delegate_turn session_manager ~prompt
                  ~send_reply:(fun text ->
                    send_message_fn ~bot_token:config.bot_token ~channel_id
                      ~text);
                Lwt.return "ok"
            | Tools ->
                let text =
                  match Session.get_tool_registry session_manager with
                  | Some reg ->
                      let tools, skills = Tool_registry.partition_skills reg in
                      Slash_commands.format_tools_plain tools skills
                  | None -> "Tools are not enabled."
                in
                let* () =
                  send_message_fn ~bot_token:config.bot_token ~channel_id ~text
                in
                Lwt.return "ok"
            | Tasks ->
                let text =
                  match Session.get_db session_manager with
                  | Some db ->
                      Task_tree.init_schema db;
                      Task_tree.render_tree_with_legend ~db ~session_key:key
                  | None -> "Tasks are not available (no database)."
                in
                let* () =
                  send_message_fn ~bot_token:config.bot_token ~channel_id ~text
                in
                Lwt.return "ok"
            | Model action -> (
                let open Slash_commands in
                match action with
                | ModelShow ->
                    let current =
                      Session.get_session_effective_model session_manager ~key
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
                      format_model_show_plain ~current
                        ~favorites:prefs.favorites ~usage_ranked
                    in
                    let* () =
                      send_message_fn ~bot_token:config.bot_token ~channel_id
                        ~text
                    in
                    Lwt.return "ok"
                | ModelSet name -> (
                    let provider, model_id, fmt =
                      Models_catalog.split_name name
                    in
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
                        let cfg = Session.get_config session_manager in
                        let provider_in_config =
                          List.mem_assoc provider cfg.providers
                        in
                        let warn =
                          if not provider_in_config then
                            Printf.sprintf
                              "\n\
                               Warning: provider '%s' not found in config. Add \
                               it to your config.json to use this model."
                              provider
                          else ""
                        in
                        Session.set_session_model session_manager ~key
                          ~model:name;
                        let _ = Model_preferences.increment_usage name in
                        let* () =
                          send_message_fn ~bot_token:config.bot_token
                            ~channel_id
                            ~text:
                              (Printf.sprintf
                                 "Model set to: %s (provider: %s)%s%s\n\
                                  Persisted for this session across restarts. \
                                  Use /model set-default to change the global \
                                  default."
                                 model_id provider hint warn)
                        in
                        Lwt.return "ok"
                    | Models_catalog.Plain -> (
                        let model_info =
                          Models_catalog.find_by_full_name name
                        in
                        match model_info with
                        | None ->
                            let text =
                              Printf.sprintf
                                "Warning: '%s' not found in model catalog. \
                                 Setting anyway.\n\
                                 Persisted for this session across restarts. \
                                 Use /model set-default to change the global \
                                 default."
                                name
                            in
                            Session.set_session_model session_manager ~key
                              ~model:name;
                            let _ = Model_preferences.increment_usage name in
                            let* () =
                              send_message_fn ~bot_token:config.bot_token
                                ~channel_id ~text
                            in
                            Lwt.return "ok"
                        | Some m ->
                            Session.set_session_model session_manager ~key
                              ~model:name;
                            let _ = Model_preferences.increment_usage name in
                            let display =
                              if m.Models_catalog.provider <> "" then
                                Printf.sprintf
                                  "Model set to: %s (provider: %s)\n\
                                   Persisted for this session across restarts. \
                                   Use /model set-default to change the global \
                                   default."
                                  m.Models_catalog.id m.Models_catalog.provider
                              else
                                Printf.sprintf
                                  "Model set to: %s\n\
                                   Persisted for this session across restarts. \
                                   Use /model set-default to change the global \
                                   default."
                                  name
                            in
                            let* () =
                              send_message_fn ~bot_token:config.bot_token
                                ~channel_id ~text:display
                            in
                            Lwt.return "ok"))
                | ModelSetDefault name -> (
                    let provider, model_id, fmt =
                      Models_catalog.split_name name
                    in
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
                        let* () =
                          send_message_fn ~bot_token:config.bot_token
                            ~channel_id
                            ~text:(Printf.sprintf "Error writing config: %s" e)
                        in
                        Lwt.return "ok"
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
                                "Default model set to: %s\n\
                                 Applies to new sessions."
                                name
                        in
                        let* () =
                          send_message_fn ~bot_token:config.bot_token
                            ~channel_id ~text:msg
                        in
                        Lwt.return "ok")
                | ModelFav name ->
                    let prefs = Model_preferences.toggle_favorite name in
                    let status =
                      if List.mem name prefs.favorites then "added to"
                      else "removed from"
                    in
                    let* () =
                      send_message_fn ~bot_token:config.bot_token ~channel_id
                        ~text:(Printf.sprintf "%s %s favorites" name status)
                    in
                    Lwt.return "ok"
                | ModelUnfav name ->
                    let _ = Model_preferences.remove_favorite name in
                    let* () =
                      send_message_fn ~bot_token:config.bot_token ~channel_id
                        ~text:(Printf.sprintf "Removed from favorites: %s" name)
                    in
                    Lwt.return "ok"
                | ModelList provider ->
                    let db_extras =
                      match Session.get_db session_manager with
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
                    let text = format_model_list_plain ~models ~provider in
                    let* () =
                      send_message_fn ~bot_token:config.bot_token ~channel_id
                        ~text
                    in
                    Lwt.return "ok"
                | ModelUsage ->
                    let cfg = Session.get_config session_manager in
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
                          let label =
                            Provider_quota.status_label ~threshold pq
                          in
                          summary ^ "  " ^ label)
                        results
                    in
                    let text =
                      if lines = [] then "No providers configured."
                      else
                        "*Provider Quota/Usage*\n\n" ^ String.concat "\n" lines
                    in
                    let* () =
                      send_message_fn ~bot_token:config.bot_token ~channel_id
                        ~text
                    in
                    Lwt.return "ok")
            | ForkAnd prompt ->
                Lwt.async (fun () ->
                    send_message_fn ~bot_token:config.bot_token ~channel_id
                      ~text:"Forking session...");
                Session.fork_and_run session_manager ~parent_key:key ~prompt
                  ~send_reply:(fun text ->
                    send_message_fn ~bot_token:config.bot_token ~channel_id
                      ~text);
                Lwt.return "ok"
            | NotACommand -> (
                let agent_defaults =
                  (Session.get_config session_manager).agent_defaults
                in
                let use_consolidated =
                  agent_defaults.show_tool_calls
                  && agent_defaults.tool_status_mode = "consolidated"
                in
                let tool_reaction_set = ref false in
                let peers =
                  Reaction_tracker.get_or_create_peers reactions ~key
                    ~initial:ts
                in
                Reaction_tracker.add_peer reactions ~key ~message_id:ts;
                let set_reaction_on_single timestamp emoji_name =
                  Reaction_tracker.set_reaction_on_single reactions
                    ~message_id:timestamp
                    ~remove_previous:(fun timestamp prev ->
                      remove_reaction ~bot_token:config.bot_token ~channel_id
                        ~timestamp ~emoji_name:prev)
                    ~add:(fun timestamp emoji_name ->
                      add_reaction ~bot_token:config.bot_token ~channel_id
                        ~timestamp ~emoji_name)
                    ~emoji:emoji_name
                in
                let set_reaction emoji_name =
                  Reaction_tracker.set_reaction_all reactions ~peers_ref:peers
                    ~set_one:(fun timestamp emoji ->
                      set_reaction_on_single timestamp emoji)
                    ~emoji:emoji_name
                in
                let thinking_buf = Buffer.create 256 in
                let status_msg =
                  if use_consolidated then
                    let status_notifier =
                      make_status_notifier ~bot_token:config.bot_token
                        ~channel_id
                    in
                    Some
                      (Status_message.create ~notifier:status_notifier
                         ~parse_mode:Connector_status.Slack.status_parse_mode ())
                  else None
                in
                let visibility = Stream_visibility.create () in
                let on_chunk chunk =
                  (match chunk with
                  | Provider.ToolStart _ ->
                      if not !tool_reaction_set then begin
                        tool_reaction_set := true;
                        Lwt.async (fun () ->
                            set_reaction
                              (Connector_status.Slack.phase_emoji Processing))
                      end
                  | _ -> ());
                  match status_msg with
                  | Some sm -> (
                      match chunk with
                      | Provider.ToolStart { id; name; arguments } ->
                          let summary =
                            Stream_visibility.summarize_tool_arguments ~name
                              arguments
                          in
                          Status_message.tool_start sm ~id ~name ~summary
                      | Provider.ToolResult { id; name; result; is_error } ->
                          Status_message.tool_result sm ~id ~name ~result
                            ~is_error
                      | Provider.ThinkingDelta text ->
                          if agent_defaults.show_thinking then
                            Buffer.add_string thinking_buf text;
                          Lwt.return_unit
                      | Provider.Delta _ | Provider.ToolCallDelta _
                      | Provider.ToolOutputDelta _ | Provider.Done ->
                          Lwt.return_unit)
                  | None ->
                      let settings : Stream_visibility.settings =
                        {
                          show_thinking = agent_defaults.show_thinking;
                          show_tool_calls = agent_defaults.show_tool_calls;
                          notify_tool_starts = false;
                          notify_tool_successes = true;
                        }
                      in
                      Stream_visibility.on_chunk visibility ~settings
                        ~notify:(fun text ->
                          send_message_fn ~bot_token:config.bot_token
                            ~channel_id ~text)
                        chunk
                in
                let* () =
                  set_reaction (Connector_status.Slack.phase_emoji Received)
                in
                let drain_progress_msg_ts = ref None in
                let on_drain_progress : Session.drain_progress =
                  {
                    before_turn =
                      (fun queued_msg_id ->
                        let* () =
                          match queued_msg_id with
                          | Some msg_ts ->
                              set_reaction_on_single msg_ts
                                (Connector_status.Slack.phase_emoji Received)
                          | None -> Lwt.return_unit
                        in
                        let* () =
                          match !drain_progress_msg_ts with
                          | Some prev_ts ->
                              Lwt.catch
                                (fun () ->
                                  delete_message ~bot_token:config.bot_token
                                    ~channel_id ~ts:prev_ts)
                                (fun _exn -> Lwt.return_unit)
                          | None -> Lwt.return_unit
                        in
                        let* new_ts =
                          send_message_with_id ~bot_token:config.bot_token
                            ~channel_id
                            ~text:
                              "\xe2\x8f\xb3 Processing queued \
                               message\xe2\x80\xa6"
                        in
                        drain_progress_msg_ts := Some new_ts;
                        Lwt.return_unit);
                    after_turn =
                      (fun queued_msg_id ->
                        match queued_msg_id with
                        | Some msg_ts ->
                            set_reaction_on_single msg_ts
                              (Connector_status.Slack.phase_emoji Completed)
                        | None -> Lwt.return_unit);
                    after_all =
                      (fun () ->
                        match !drain_progress_msg_ts with
                        | Some prev_ts ->
                            drain_progress_msg_ts := None;
                            Lwt.catch
                              (fun () ->
                                delete_message ~bot_token:config.bot_token
                                  ~channel_id ~ts:prev_ts)
                              (fun _exn -> Lwt.return_unit)
                        | None -> Lwt.return_unit);
                  }
                in
                let response_sent = ref false in
                let before_drain response =
                  if Session.is_queued_message_response response then
                    Lwt.return_unit
                  else
                    let open Lwt.Syntax in
                    let* () =
                      match status_msg with
                      | Some sm -> Status_message.finalize sm
                      | None -> Lwt.return_unit
                    in
                    let thinking =
                      match status_msg with
                      | Some _ -> Buffer.contents thinking_buf
                      | None -> Stream_visibility.thinking_text visibility
                    in
                    let* () =
                      if thinking <> "" then
                        send_message_fn ~bot_token:config.bot_token ~channel_id
                          ~text:("_" ^ thinking ^ "_")
                      else Lwt.return_unit
                    in
                    let* () =
                      send_message_fn ~bot_token:config.bot_token ~channel_id
                        ~text:response
                    in
                    let* () =
                      set_reaction
                        (Connector_status.Slack.phase_emoji Completed)
                    in
                    if not (Session.take_response_deferred session_manager ~key)
                    then Session.mark_response_sent session_manager ~key;
                    response_sent := true;
                    Lwt.return_unit
                in
                let* result =
                  Session.with_registered_notifier session_manager ~key
                    ~notify:(fun text ->
                      send_message_fn ~bot_token:config.bot_token ~channel_id
                        ~text)
                    (fun () ->
                      Lwt.catch
                        (fun () ->
                          let* response =
                            Session.turn_stream session_manager ~key
                              ~message:text ~channel_name:channel_id
                              ~channel_type:"group" ~sender_id:user_id
                              ~channel:"slack" ~channel_id ~message_id:ts
                              ~on_drain_progress ~before_drain ~on_chunk ()
                          in
                          Lwt.return (Ok response))
                        (fun exn -> Lwt.return (Error (Printexc.to_string exn))))
                in
                match result with
                | Ok response ->
                    if Session.is_queued_message_response response then
                      Lwt.return "ok"
                    else if !response_sent then begin
                      ignore (Reaction_tracker.cleanup reactions ~key);
                      let send_to_channel text =
                        send_message_fn ~bot_token:config.bot_token ~channel_id
                          ~text
                      in
                      Lwt.async (fun () ->
                          Session.process_autonomous_turn_result
                            ~on_response:send_to_channel session_manager ~key
                            ~response);
                      Lwt.return "ok"
                    end
                    else
                      let* () =
                        match status_msg with
                        | Some sm -> Status_message.finalize sm
                        | None -> Lwt.return_unit
                      in
                      let thinking =
                        match status_msg with
                        | Some _ -> Buffer.contents thinking_buf
                        | None -> Stream_visibility.thinking_text visibility
                      in
                      let* () =
                        if thinking <> "" then
                          send_message_fn ~bot_token:config.bot_token
                            ~channel_id
                            ~text:("_" ^ thinking ^ "_")
                        else Lwt.return_unit
                      in
                      let* () =
                        send_message_fn ~bot_token:config.bot_token ~channel_id
                          ~text:response
                      in
                      let* () =
                        set_reaction
                          (Connector_status.Slack.phase_emoji Completed)
                      in
                      ignore (Reaction_tracker.cleanup reactions ~key);
                      if
                        not
                          (Session.take_response_deferred session_manager ~key)
                      then Session.mark_response_sent session_manager ~key;
                      let send_to_channel text =
                        send_message_fn ~bot_token:config.bot_token ~channel_id
                          ~text
                      in
                      Lwt.async (fun () ->
                          Session.process_autonomous_turn_result
                            ~on_response:send_to_channel session_manager ~key
                            ~response);
                      Lwt.return "ok"
                | Error err ->
                    Logs.err (fun m ->
                        m "Slack agent error for channel=%s user=%s: %s"
                          channel_id user_id err);
                    let* () =
                      match status_msg with
                      | Some sm -> Status_message.finalize sm
                      | None -> Lwt.return_unit
                    in
                    let* () =
                      send_message_fn ~bot_token:config.bot_token ~channel_id
                        ~text:
                          (Printf.sprintf
                             "Sorry, an error occurred processing your \
                              message: %s"
                             err)
                    in
                    let* () =
                      set_reaction (Connector_status.Slack.phase_emoji Failed)
                    in
                    ignore (Reaction_tracker.cleanup reactions ~key);
                    if not (Session.take_response_deferred session_manager ~key)
                    then Session.mark_response_sent session_manager ~key;
                    Lwt.return "ok")
      end
  | Some Other | None -> Lwt.return "ok"
