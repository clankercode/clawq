type slack_file = {
  url_private_download : string;
  file_name : string;
  mimetype : string;
  file_size : int;
}

type event =
  | UrlVerification of string
  | Message of {
      channel_id : string;
      user_id : string;
      text : string;
      bot_id : string option;
      ts : string;
      thread_ts : string option;
      files : slack_file list;
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

(** Resolve the session key for a Slack channel+user pair. If the channel has an
    active room profile binding (shared room session), returns "slack:CHANNEL".
    Otherwise returns the per-user key "slack:CHANNEL:USER". *)
let room_has_profile_binding ~(session_manager : Session.t) ~channel_id =
  match Session.get_db session_manager with
  | Some db -> (
      match Memory.get_room_profile_binding ~db ~room_id:channel_id with
      | Some _ -> true
      | None -> false)
  | None -> false

let resolve_session_key ~(session_manager : Session.t) ~channel_id ~user_id =
  let has_profile = room_has_profile_binding ~session_manager ~channel_id in
  if has_profile then "slack:" ^ Session.sanitize_session_key channel_id
  else
    "slack:"
    ^ Session.sanitize_session_key channel_id
    ^ ":"
    ^ Session.sanitize_session_key user_id

let timestamp_of_slack_ts ts =
  match float_of_string_opt ts with Some t -> Some t | None -> None

let record_scoped_room_history_if_bound ~(session_manager : Session.t)
    ~channel_id ~user_id ~text ~ts =
  let cfg = Session.get_config session_manager in
  if
    String.trim text <> ""
    && room_has_profile_binding ~session_manager ~channel_id
    && Connector_capabilities.should_capture_history
         ~enabled:cfg.connector_history.enabled Connector_capabilities.slack
  then
    let key = "slack:" ^ Session.sanitize_session_key channel_id in
    let db =
      if cfg.connector_history.persist_to_db then Session.get_db session_manager
      else None
    in
    let timestamp = timestamp_of_slack_ts ts in
    Connector_history.record ?db ?timestamp
      ~persist:cfg.connector_history.persist_to_db ~key ~room_id:channel_id
      ~connector_type:"slack" ~channel_type:"slack"
      ~max:cfg.connector_history.max_messages ~sender_name:user_id
      ~sender_id:user_id ~text ()

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

let build_post_body ~channel_id ~text ~(thread_ts : string option) =
  let thread_field =
    match thread_ts with Some ts -> [ ("thread_ts", `String ts) ] | None -> []
  in
  `Assoc
    (("channel", `String channel_id) :: ("text", `String text) :: thread_field)
  |> Yojson.Safe.to_string

let send_message ~bot_token ~channel_id ~text =
  let open Lwt.Syntax in
  let uri = "https://slack.com/api/chat.postMessage" in
  let headers = [ ("Authorization", "Bearer " ^ bot_token) ] in
  let body = build_post_body ~channel_id ~text ~thread_ts:None in
  let* _status, _body = Http_client.post_json ~uri ~headers ~body in
  Lwt.return_unit

let send_message_with_id ~bot_token ~channel_id ~text =
  let open Lwt.Syntax in
  let uri = "https://slack.com/api/chat.postMessage" in
  let headers = [ ("Authorization", "Bearer " ^ bot_token) ] in
  let body = build_post_body ~channel_id ~text ~thread_ts:None in
  let* _status, resp_body = Http_client.post_json ~uri ~headers ~body in
  let ts =
    try
      let json = Yojson.Safe.from_string resp_body in
      json |> Yojson.Safe.Util.member "ts" |> Yojson.Safe.Util.to_string
    with _ -> "0"
  in
  Lwt.return ts

(** Send a message, optionally replying in a Slack thread. When [thread_ts] is
    provided the message is posted as a reply in that thread; otherwise it is an
    ordinary channel message. *)
let send_message_reply ~bot_token ~channel_id ~text ?thread_ts () =
  let open Lwt.Syntax in
  let uri = "https://slack.com/api/chat.postMessage" in
  let headers = [ ("Authorization", "Bearer " ^ bot_token) ] in
  let body = build_post_body ~channel_id ~text ~thread_ts in
  let* _status, _body = Http_client.post_json ~uri ~headers ~body in
  Lwt.return_unit

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
            let thread_ts =
              try Some (evt |> member "thread_ts" |> to_string) with _ -> None
            in
            let files =
              try
                evt |> member "files" |> to_list
                |> List.filter_map (fun f ->
                    try
                      let url_private_download =
                        f |> member "url_private_download" |> to_string
                      in
                      let file_name =
                        try f |> member "name" |> to_string with _ -> "file"
                      in
                      let mimetype =
                        try f |> member "mimetype" |> to_string
                        with _ -> "application/octet-stream"
                      in
                      let file_size =
                        try f |> member "size" |> to_int with _ -> 0
                      in
                      Some
                        { url_private_download; file_name; mimetype; file_size }
                    with _ -> None)
              with _ -> []
            in
            Some
              (Message
                 { channel_id; user_id; text; bot_id; ts; thread_ts; files })
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
  | Some
      (Message
         { channel_id; user_id; text; bot_id = None; ts; thread_ts; files }) ->
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
          let key = resolve_session_key ~session_manager ~channel_id ~user_id in
          (* Reply helper: sends into the thread when thread_ts is present,
             otherwise uses the caller-provided send_message_fn *)
          let reply ~text =
            match thread_ts with
            | Some ts ->
                send_message_reply ~bot_token:config.bot_token ~channel_id ~text
                  ~thread_ts:ts ()
            | None ->
                send_message_fn ~bot_token:config.bot_token ~channel_id ~text
          in
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
                  ~parse_mode:Connector_status.Slack.status_parse_mode ());
            Session.register_connector_capabilities session_manager ~key
              Connector_capabilities.slack
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
                              (match
                                 Session.get_session_model_override
                                   session_manager ~key
                               with
                              | Some model ->
                                  Restart_notify.write_session ~channel:"slack"
                                    ~channel_id ~session_key:key ~model
                              | None ->
                                  Restart_notify.write_session_key
                                    ~channel:"slack" ~channel_id
                                    ~session_key:key);
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
            let skill_names =
              List.map
                (fun (s : Skills.skill_md_meta) -> s.md_name)
                (Skills.available_skills ())
            in
            let* cmd_result, text, skill_injections, _loaded_skill_name =
              match Slash_commands.handle ~skill_names text with
              | Slash_commands.SkillInvoke (name, args) -> (
                  if
                    args = ""
                    && Session.skill_loaded_in_context session_manager ~key name
                  then Lwt.return (Slash_commands.NotACommand, text, [], None)
                  else
                    let* result = Skills.expand_slash_skill ~name ~args () in
                    match result with
                    | Ok r ->
                        Lwt.return
                          ( Slash_commands.NotACommand,
                            text,
                            [ r.skill_injection ],
                            Some name )
                    | Error err_msg ->
                        Lwt.return (Slash_commands.Reply err_msg, text, [], None)
                  )
              | other -> Lwt.return (other, text, [], None)
            in
            let is_admin =
              match Session.get_db session_manager with
              | Some db ->
                  Admin.is_admin ~db ~channel:"slack" ~sender_id:user_id
              | None -> false
            in
            let user_group = if is_admin then "admin" else "guest" in
            let cmd_result = Slash_commands.gate_admin ~is_admin cmd_result in
            (match cmd_result with
            | InjectConnectorHistory _ -> ()
            | _ ->
                record_scoped_room_history_if_bound ~session_manager ~channel_id
                  ~user_id ~text ~ts);
            let send_reply text =
              send_message_fn ~bot_token:config.bot_token ~channel_id ~text
            in
            let env : Connector_dispatch.dispatch_env =
              {
                connector = Format_adapter.Slack;
                connector_name = "slack";
                log_name = "Slack";
                thinking_channel_field = "channel";
                thinking_user_field = "user";
                show_thinking_channel_field = "channel_id";
                show_thinking_user_field = "user_id";
                session_mgr = session_manager;
                key;
                channel_id;
                channel_name = Some channel_id;
                channel_type = Some "group";
                sender_name = None;
                message_id = Some ts;
                user_id;
                is_admin;
                send_plain = send_reply;
                send_formatted = send_reply;
              }
            in
            (* Create task-tree record for async commands from profiled rooms *)
            (if Room_request_classifier.classify cmd_result = AsyncCommand then
               match Session.get_db session_manager with
               | Some db -> (
                   Task_tree.init_schema db;
                   match
                     Memory.get_room_profile_binding ~db ~room_id:channel_id
                   with
                   | Some binding ->
                       let origin =
                         Room_origin.make ~connector:"slack" ~room_id:channel_id
                           ~requester_id:user_id ~profile_id:binding.profile_id
                           ()
                       in
                       let title =
                         Room_request_classifier.title_of_async_cmd cmd_result
                         |> Option.value ~default:""
                       in
                       ignore
                         (Task_tree_ops.create_async_cmd_task ~db
                            ~session_key:key ~title ~origin ?thread_id:thread_ts
                            ~requester:user_id ~profile_id:binding.profile_id ())
                   | None -> ())
               | None -> ());
            match cmd_result with
            | AdminRequired _ -> assert false
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
            | Delegate (agent_name, prompt) ->
                Lwt.async (fun () ->
                    send_message_fn ~bot_token:config.bot_token ~channel_id
                      ~text:"Delegating to a temporary session...");
                let send_agent_reply text =
                  send_message_fn ~bot_token:config.bot_token ~channel_id ~text
                in
                Session.delegate_turn session_manager ?agent_name ~prompt
                  ~parent_key:key ~debug_notify:send_agent_reply
                  ~send_reply:send_agent_reply ();
                Lwt.return "ok"
            | AgentInvoke (agent_name, prompt) ->
                Lwt.async (fun () ->
                    send_message_fn ~bot_token:config.bot_token ~channel_id
                      ~text:(Printf.sprintf "Invoking agent '%s'..." agent_name));
                let send_agent_reply text =
                  send_message_fn ~bot_token:config.bot_token ~channel_id ~text
                in
                Session.agent_invoke_turn session_manager ~agent_name ~prompt
                  ~parent_key:key ~debug_notify:send_agent_reply
                  ~send_reply:send_agent_reply ();
                Lwt.return "ok"
            | Rig action -> (
                match action with
                | RigList ->
                    let text = Rig.list_text () in
                    let* () =
                      send_message_fn ~bot_token:config.bot_token ~channel_id
                        ~text
                    in
                    Lwt.return "ok"
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
                        let* () =
                          send_message_fn ~bot_token:config.bot_token
                            ~channel_id ~text:msg
                        in
                        Lwt.return "ok"
                    | Ok prompt ->
                        Lwt.async (fun () ->
                            send_message_fn ~bot_token:config.bot_token
                              ~channel_id
                              ~text:
                                (Printf.sprintf "Running rig %s for '%s'..."
                                   act_str name));
                        let send_agent_reply text =
                          send_message_fn ~bot_token:config.bot_token
                            ~channel_id ~text
                        in
                        Session.delegate_turn session_manager ~prompt
                          ~parent_key:key ~debug_notify:send_agent_reply
                          ~send_reply:send_agent_reply ();
                        (match act with
                        | `Install -> (
                            match Rig.find_rig name with
                            | Some rig ->
                                Rig.mark_installed ~name ~version:rig.version
                            | None -> ())
                        | `Remove -> Rig.mark_removed ~name
                        | `Adjust -> ());
                        Lwt.return "ok"))
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
                      format_model_show ~connector:Format_adapter.Slack ~current
                        ~favorites:prefs.favorites ~usage_ranked
                    in
                    let* () =
                      send_message_fn ~bot_token:config.bot_token ~channel_id
                        ~text
                    in
                    Lwt.return "ok"
                | ModelSet _ | ModelSetForce _ | ModelSetDefault _ ->
                    let* text =
                      Slash_commands_model.handle_model_set_action
                        ~config_source:"slack" ~session_manager ~key action
                    in
                    let* () =
                      send_message_fn ~bot_token:config.bot_token ~channel_id
                        ~text
                    in
                    Lwt.return "ok"
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
                | ModelList (provider, availability) ->
                    let db_extras =
                      match Session.get_db session_manager with
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
                      format_model_list ~connector:Format_adapter.Slack ~models
                        ~provider
                    in
                    let* () =
                      send_message_fn ~bot_token:config.bot_token ~channel_id
                        ~text
                    in
                    Lwt.return "ok"
                | ModelUsage ->
                    let cfg = Session.get_config session_manager in
                    Provider_quota.set_cache_ttl cfg.quota_cache_ttl_s;
                    let results =
                      Provider_quota.get_all_cached ()
                      |> List.map (fun (_name, pq) -> pq)
                    in
                    let text =
                      Slash_commands.format_model_usage
                        ~connector:Format_adapter.Slack ~config:cfg results
                    in
                    let* () =
                      send_message_fn ~bot_token:config.bot_token ~channel_id
                        ~text
                    in
                    Lwt.return "ok")
            | ForkAnd (agent_name, prompt) ->
                Lwt.async (fun () ->
                    send_message_fn ~bot_token:config.bot_token ~channel_id
                      ~text:"Forking session...");
                let send_agent_reply text =
                  send_message_fn ~bot_token:config.bot_token ~channel_id ~text
                in
                Session.fork_and_run session_manager ~parent_key:key ?agent_name
                  ~debug_notify:send_agent_reply ~prompt
                  ~send_reply:send_agent_reply ();
                Lwt.return "ok"
            | Debate prompt -> (
                match Session.get_db session_manager with
                | Some db ->
                    let cfg = Session.get_config session_manager in
                    let send_agent_reply text =
                      send_message_fn ~bot_token:config.bot_token ~channel_id
                        ~text
                    in
                    let on_llm_call_debug =
                      Session.debug_callback_for session_manager ~key
                        (Some send_agent_reply)
                    in
                    let* text =
                      Debate.run_for_prompt ?on_llm_call_debug ~config:cfg ~db
                        ~prompt ()
                    in
                    let* () =
                      send_message_fn ~bot_token:config.bot_token ~channel_id
                        ~text
                    in
                    Lwt.return "ok"
                | None ->
                    let* () =
                      send_message_fn ~bot_token:config.bot_token ~channel_id
                        ~text:"Debate requires a database."
                    in
                    Lwt.return "ok")
            | BashRun cmd ->
                let cfg = Session.get_config session_manager in
                let* result =
                  Slash_commands_bash.run_bash_command ~config:cfg
                    ~session_key:key cmd
                in
                let full_text = Slash_commands_bash.format_result cmd result in
                let max_len = 3000 in
                let text =
                  if String.length full_text <= max_len then full_text
                  else String.sub full_text 0 max_len ^ "\n...[truncated]"
                in
                let* () =
                  send_message_fn ~bot_token:config.bot_token ~channel_id ~text
                in
                Lwt.return "ok"
            | DebugDumpChat ->
                let content = Session.dump_json session_manager ~key in
                let max_len = 1800 in
                let text =
                  if String.length content <= max_len then content
                  else
                    "Session dump (truncated — full dump not yet supported for \
                     this connector):\n"
                    ^ String.sub content 0 max_len
                    ^ "\n..."
                in
                let* () =
                  send_message_fn ~bot_token:config.bot_token ~channel_id ~text
                in
                Lwt.return "ok"
            | InjectConnectorHistory _ ->
                let* () =
                  send_message_fn ~bot_token:config.bot_token ~channel_id
                    ~text:
                      "Connector history is not applicable for this channel — \
                       Slack delivers all messages to the bot, so the agent \
                       already sees all channel activity. Use \
                       /inject_connector_history in Teams or Discord group \
                       chats."
                in
                Lwt.return "ok"
            | SkillInvoke _ ->
                Lwt.return "ok" (* unreachable: preprocessed above *)
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
                let notifier_factory =
                  if use_consolidated then
                    Some
                      (fun () ->
                        let status_notifier =
                          make_status_notifier ~bot_token:config.bot_token
                            ~channel_id
                        in
                        Status_message.create ~notifier:status_notifier
                          ~parse_mode:Connector_status.Slack.status_parse_mode
                          ())
                  else None
                in
                let strategy =
                  Status_update.select_strategy ~agent_defaults
                    ~capabilities:(Some Connector_capabilities.slack)
                in
                let handler =
                  Status_update.make_handler ~strategy ~notifier_factory
                    ~notify:(fun text -> reply ~text)
                    ~agent_defaults
                    ~parse_mode:Connector_status.Slack.status_parse_mode ()
                in
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
                  handler.on_chunk chunk
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
                  if Session.should_suppress_response response then
                    Lwt.return_unit
                  else
                    let open Lwt.Syntax in
                    let* () = handler.finalize () in
                    let thinking = handler.get_thinking () in
                    let* () =
                      if thinking <> "" then reply ~text:("_" ^ thinking ^ "_")
                      else Lwt.return_unit
                    in
                    let* () = reply ~text:response in
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
                    ~notify:(fun text -> reply ~text)
                    (fun () ->
                      Lwt.catch
                        (fun () ->
                          let full_config =
                            Session.get_config session_manager
                          in
                          (* Partition audio files for transcription *)
                          let audio_files, non_audio_files =
                            List.partition
                              (fun (f : slack_file) ->
                                Voice_transcription.is_audio_mime f.mimetype)
                              files
                          in
                          let headers =
                            [ ("Authorization", "Bearer " ^ config.bot_token) ]
                          in
                          let* transcription_prefix =
                            if
                              audio_files <> []
                              && full_config.security
                                   .attachment_downloads_enabled
                            then
                              let* texts =
                                Lwt_list.map_s
                                  (fun (f : slack_file) ->
                                    match
                                      Voice_transcription.validate
                                        ~config:full_config
                                        ~filename:f.file_name
                                        ~mime_type:(Some f.mimetype)
                                        ~size:(Some f.file_size)
                                        ~duration_seconds:None
                                    with
                                    | Error reason ->
                                        Logs.info (fun m ->
                                            m "Slack voice skipped %s: %s"
                                              f.file_name
                                              (Voice_transcription
                                               .skip_reason_to_string reason));
                                        Lwt.return ""
                                    | Ok () ->
                                        Lwt.catch
                                          (fun () ->
                                            let* _status, audio_data =
                                              Http_client.get
                                                ~uri:f.url_private_download
                                                ~headers
                                            in
                                            let notifier =
                                              make_status_notifier
                                                ~bot_token:config.bot_token
                                                ~channel_id
                                            in
                                            Voice_transcription
                                            .transcribe_with_progress
                                              ~config:full_config ~notifier
                                              ~audio_data ~filename:f.file_name
                                              ())
                                          (fun exn ->
                                            Logs.err (fun m ->
                                                m
                                                  "Slack voice transcription \
                                                   failed %s: %s"
                                                  f.file_name
                                                  (Printexc.to_string exn));
                                            Lwt.return ""))
                                  audio_files
                              in
                              Lwt.return
                                (String.concat ""
                                   (List.filter (fun s -> s <> "") texts))
                            else Lwt.return ""
                          in
                          let effective_text =
                            if transcription_prefix <> "" then
                              transcription_prefix ^ "\n" ^ text
                            else text
                          in
                          let* content_parts, att_list, message =
                            if
                              non_audio_files <> []
                              && full_config.security
                                   .attachment_downloads_enabled
                            then
                              let workspace =
                                Runtime_config.effective_workspace full_config
                              in
                              let metas =
                                List.map
                                  (fun (f : slack_file) ->
                                    Attachment_download.
                                      {
                                        url = f.url_private_download;
                                        filename = f.file_name;
                                        mime_type = Some f.mimetype;
                                        size = Some f.file_size;
                                      })
                                  non_audio_files
                              in
                              Attachment_download.process_attachments metas
                                ~headers ~workspace
                                ~db:(Session.get_db session_manager)
                                ~session_key:key ~source:"slack"
                                ~content_parts:[] ~attachments:[]
                                ~message:effective_text
                            else
                              let placeholder =
                                if non_audio_files <> [] then
                                  let names =
                                    List.map
                                      (fun (f : slack_file) ->
                                        Printf.sprintf
                                          "\n\
                                           [Attachment: %s (download disabled)]"
                                          f.file_name)
                                      non_audio_files
                                  in
                                  effective_text ^ String.concat "" names
                                else effective_text
                              in
                              Lwt.return ([], [], placeholder)
                          in
                          let* response =
                            Session.turn_stream session_manager ~key ~message
                              ~content_parts ~attachments:att_list
                              ~skill_injections ~channel_name:channel_id
                              ~channel_type:"group" ~sender_id:user_id
                              ~user_group ~channel:"slack" ~channel_id
                              ~message_id:ts ~on_drain_progress ~before_drain
                              ~on_chunk ()
                          in
                          Lwt.return (Ok response))
                        (fun exn -> Lwt.return (Error (Printexc.to_string exn))))
                in
                match result with
                | Ok response ->
                    if Session.should_suppress_response response then
                      Lwt.return "ok"
                    else if !response_sent then (
                      let* () =
                        Reaction_tracker.cleanup_with_remove reactions ~key
                          ~remove:(fun timestamp emoji_name ->
                            remove_reaction ~bot_token:config.bot_token
                              ~channel_id ~timestamp ~emoji_name)
                      in
                      Lwt.async (fun () ->
                          Session.process_autonomous_turn_result
                            ~on_response:(fun text -> reply ~text)
                            session_manager ~key ~response);
                      Lwt.return "ok")
                    else
                      let* () = handler.finalize () in
                      let thinking = handler.get_thinking () in
                      let* () =
                        if thinking <> "" then reply ~text:("_" ^ thinking ^ "_")
                        else Lwt.return_unit
                      in
                      let* () = reply ~text:response in
                      let* () =
                        set_reaction
                          (Connector_status.Slack.phase_emoji Completed)
                      in
                      let* () =
                        Reaction_tracker.cleanup_with_remove reactions ~key
                          ~remove:(fun timestamp emoji_name ->
                            remove_reaction ~bot_token:config.bot_token
                              ~channel_id ~timestamp ~emoji_name)
                      in
                      if
                        not
                          (Session.take_response_deferred session_manager ~key)
                      then Session.mark_response_sent session_manager ~key;
                      Lwt.async (fun () ->
                          Session.process_autonomous_turn_result
                            ~on_response:(fun text -> reply ~text)
                            session_manager ~key ~response);
                      Lwt.return "ok"
                | Error err ->
                    Logs.err (fun m ->
                        m "Slack agent error for channel=%s user=%s: %s"
                          channel_id user_id err);
                    let* () = handler.finalize () in
                    let* () =
                      reply
                        ~text:
                          (Printf.sprintf
                             "Sorry, an error occurred processing your \
                              message: %s"
                             err)
                    in
                    let* () =
                      set_reaction (Connector_status.Slack.phase_emoji Failed)
                    in
                    let* () =
                      Reaction_tracker.cleanup_with_remove reactions ~key
                        ~remove:(fun timestamp emoji_name ->
                          remove_reaction ~bot_token:config.bot_token
                            ~channel_id ~timestamp ~emoji_name)
                    in
                    if not (Session.take_response_deferred session_manager ~key)
                    then Session.mark_response_sent session_manager ~key;
                    Lwt.return "ok")
            | ( RegisterAsAdminOtc _ | Reply _ | FormattedReply _ | Help
              | Menu _ | Reset | RuntimeCtx | Context | Uptime | Status
              | Thinking _ | ShowThinking _ | Heartbeat _ | Debug _
              | AgentMenu _ | ModelMenu _ | ThinkingMenu | ConfigMenu _
              | SkillsMenu _ | CostsMenu | BgMenu | Tools | Tasks | TasksFull
              | Costs _ | Session _ | Usage _ | Active | Bg _ | Cron _ | Bl _
              | HeldItems _ | Memories _ | Repo _ | Followup _ ) as r ->
                let* () = Connector_dispatch.dispatch env r in
                Lwt.return "ok"
      end
  | Some Other | None -> Lwt.return "ok"
