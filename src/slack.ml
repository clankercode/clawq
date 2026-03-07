type event =
  | UrlVerification of string
  | Message of {
      channel_id : string;
      user_id : string;
      text : string;
      bot_id : string option;
    }
  | Other

let is_allowed ~(config : Runtime_config.slack_config) ~channel_id ~user_id =
  let ch_ok =
    match config.allow_channels with
    | [ "*" ] -> true
    | ids -> List.mem channel_id ids
  in
  let usr_ok =
    match config.allow_users with
    | [ "*" ] -> true
    | ids -> List.mem user_id ids
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
    ~(session_manager : Session.t) ?event_limiter body =
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
              send_message ~bot_token:config.bot_token ~channel_id
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
          match Slash_commands.handle text with
          | Reply reply_text ->
              let* () =
                send_message ~bot_token:config.bot_token ~channel_id
                  ~text:reply_text
              in
              Lwt.return "ok"
          | Reset ->
              let* () = Session.reset session_manager ~key in
              let* () =
                send_message ~bot_token:config.bot_token ~channel_id
                  ~text:Slash_commands.reset_message
              in
              Lwt.return "ok"
          | NotACommand -> (
              let* result =
                Session.with_registered_notifier session_manager ~key
                  ~notify:(fun text ->
                    send_message ~bot_token:config.bot_token ~channel_id ~text)
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
                  let* () =
                    send_message ~bot_token:config.bot_token ~channel_id
                      ~text:response
                  in
                  Session.mark_response_sent session_manager ~key;
                  Lwt.return "ok"
              | Error err ->
                  Logs.err (fun m ->
                      m "Slack agent error for channel=%s user=%s: %s"
                        channel_id user_id err);
                  let* () =
                    send_message ~bot_token:config.bot_token ~channel_id
                      ~text:"Sorry, an error occurred processing your message."
                  in
                  Session.mark_response_sent session_manager ~key;
                  Lwt.return "ok")
      end
  | Some Other | None -> Lwt.return "ok"
