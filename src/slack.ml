type event =
  | UrlVerification of string
  | Message of {
      channel_id : string;
      user_id : string;
      text : string;
      bot_id : string option;
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
      Session.update_config session_manager { cfg with agent_defaults };
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

let _rate_limit_warnings : (string, float) Hashtbl.t = Hashtbl.create 16

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
            Some (Message { channel_id; user_id; text; bot_id })
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
  | Some (Message { channel_id; user_id; text; bot_id = None }) ->
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
          if Update_tool.is_update_command text then begin
            let notify text =
              send_message_fn ~bot_token:config.bot_token ~channel_id ~text
            in
            let run_update_command =
              match run_update_command with
              | Some run_update_command -> run_update_command
              | None ->
                  fun ?prepare_restart:_ ~send_progress () ->
                    Update_tool.run_update
                      ~is_draining:(fun () ->
                        Session.is_draining session_manager)
                      ~send_progress ()
            in
            Session.register_channel_notifier session_manager ~key notify;
            Lwt.async (fun () ->
                Lwt.finalize
                  (fun () ->
                    Lwt.catch
                      (fun () ->
                        let* response =
                          run_update_command
                            ~prepare_restart:(fun () ->
                              Restart_notify.write ~channel:"slack" ~channel_id;
                              Lwt.return (Ok ()))
                            ~send_progress:notify ()
                        in
                        notify response)
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
            | NotACommand -> (
                let* result =
                  Session.with_registered_notifier session_manager ~key
                    ~notify:(fun text ->
                      send_message_fn ~bot_token:config.bot_token ~channel_id
                        ~text)
                    (fun () ->
                      Lwt.catch
                        (fun () ->
                          let* response =
                            Session.turn session_manager ~key ~message:text
                              ~channel_name:channel_id ~channel_type:"group"
                              ~sender_id:user_id ~channel:"slack" ~channel_id ()
                          in
                          Lwt.return (Ok response))
                        (fun exn -> Lwt.return (Error (Printexc.to_string exn))))
                in
                match result with
                | Ok response ->
                    if Session.is_queued_message_response response then
                      Lwt.return "ok"
                    else
                      let* () =
                        send_message_fn ~bot_token:config.bot_token ~channel_id
                          ~text:response
                      in
                      if
                        not
                          (Session.take_response_deferred session_manager ~key)
                      then Session.mark_response_sent session_manager ~key;
                      Lwt.return "ok"
                | Error err ->
                    Logs.err (fun m ->
                        m "Slack agent error for channel=%s user=%s: %s"
                          channel_id user_id err);
                    let* () =
                      send_message_fn ~bot_token:config.bot_token ~channel_id
                        ~text:
                          (Printf.sprintf
                             "Sorry, an error occurred processing your \
                              message: %s"
                             err)
                    in
                    if not (Session.take_response_deferred session_manager ~key)
                    then Session.mark_response_sent session_manager ~key;
                    Lwt.return "ok")
      end
  | Some Other | None -> Lwt.return "ok"
