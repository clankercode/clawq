(* LINE channel via Messaging API webhooks *)

let api_base = "https://api.line.me/v2/bot/message"

let is_allowed ~(config : Runtime_config.line_config) ~user_id =
  Channel_util.is_allowed ~allowlist:config.allow_from user_id

(* Verify X-Line-Signature: HMAC-SHA256 of body, base64-encoded *)
let verify_signature ~channel_secret ~body ~signature =
  let computed =
    Digestif.SHA256.hmac_string ~key:channel_secret body
    |> Digestif.SHA256.to_raw_string |> Base64.encode_exn
  in
  Eqaf.equal computed signature

let send_reply ~(config : Runtime_config.line_config) ~reply_token ~text =
  let open Lwt.Syntax in
  let uri = api_base ^ "/reply" in
  let headers =
    [ ("Authorization", "Bearer " ^ config.channel_access_token) ]
  in
  let body =
    `Assoc
      [
        ("replyToken", `String reply_token);
        ( "messages",
          `List [ `Assoc [ ("type", `String "text"); ("text", `String text) ] ]
        );
      ]
    |> Yojson.Safe.to_string
  in
  let* _status, _body = Http_client.post_json ~uri ~headers ~body in
  Lwt.return_unit

let send_push ~(config : Runtime_config.line_config) ~user_id ~text =
  let open Lwt.Syntax in
  let uri = api_base ^ "/push" in
  let headers =
    [ ("Authorization", "Bearer " ^ config.channel_access_token) ]
  in
  let body =
    `Assoc
      [
        ("to", `String user_id);
        ( "messages",
          `List [ `Assoc [ ("type", `String "text"); ("text", `String text) ] ]
        );
      ]
    |> Yojson.Safe.to_string
  in
  let* _status, _body = Http_client.post_json ~uri ~headers ~body in
  Lwt.return_unit

(* Parse LINE webhook events, return (user_id, reply_token, text) list *)
let parse_events body_str =
  try
    let json = Yojson.Safe.from_string body_str in
    let open Yojson.Safe.Util in
    let events = json |> member "events" |> to_list in
    List.filter_map
      (fun ev ->
        try
          let typ = ev |> member "type" |> to_string in
          if typ <> "message" then None
          else
            let source = ev |> member "source" in
            let user_id =
              try source |> member "userId" |> to_string with _ -> ""
            in
            let message = ev |> member "message" in
            let msg_type =
              try message |> member "type" |> to_string with _ -> ""
            in
            if msg_type <> "text" then None
            else
              let text =
                try message |> member "text" |> to_string with _ -> ""
              in
              let reply_token =
                try ev |> member "replyToken" |> to_string with _ -> ""
              in
              if user_id = "" || text = "" then None
              else Some (user_id, reply_token, text)
        with _ -> None)
      events
  with _ -> []

let handle_webhook ~(config : Runtime_config.line_config)
    ~(session_mgr : Session.t) ~signature body_str =
  let open Lwt.Syntax in
  if
    not
      (verify_signature ~channel_secret:config.channel_secret ~body:body_str
         ~signature)
  then begin
    Logs.warn (fun m -> m "LINE: invalid signature, rejecting webhook");
    Lwt.return false
  end
  else begin
    let events = parse_events body_str in
    let* () =
      Lwt_list.iter_s
        (fun (user_id, reply_token, text) ->
          if not (is_allowed ~config ~user_id) then begin
            Logs.warn (fun m ->
                m "LINE: ignoring message from unauthorized userId=%s" user_id);
            Lwt.return_unit
          end
          else
            let key = "line:" ^ user_id in
            let notify text =
              if reply_token <> "" then send_reply ~config ~reply_token ~text
              else send_push ~config ~user_id ~text
            in
            Session.register_connector_capabilities session_mgr ~key
              Connector_capabilities.line;
            let* result =
              Session.with_registered_notifier session_mgr ~key ~notify
                (fun () ->
                  Lwt.catch
                    (fun () ->
                      let* response =
                        Session.turn session_mgr ~key ~message:text
                          ~channel_name:"line" ~channel_type:"dm" ()
                      in
                      Lwt.return (Ok response))
                    (fun exn -> Lwt.return (Error (Printexc.to_string exn))))
            in
            match result with
            | Ok response ->
                if Session.should_suppress_response response then
                  Lwt.return_unit
                else if reply_token <> "" then
                  send_reply ~config ~reply_token ~text:response
                else send_push ~config ~user_id ~text:response
            | Error err ->
                Logs.err (fun m ->
                    m "LINE: agent error for userId=%s: %s" user_id err);
                Lwt.return_unit)
        events
    in
    Lwt.return true
  end
