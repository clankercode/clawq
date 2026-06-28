(* DingTalk channel via Stream Mode WebSocket or webhook *)

let stream_register_url =
  "https://api.dingtalk.com/v1.0/gateway/connections/open"

let open_api_base = "https://api.dingtalk.com/v1.0"

let is_allowed ~(config : Runtime_config.dingtalk_config) ~sender_id =
  Channel_util.is_allowed ~allowlist:config.allow_from sender_id

(* Compute HMAC-SHA256 signature of timestamp with app_secret *)
let compute_auth_sig ~app_secret ~timestamp =
  let string_to_sign = timestamp ^ "\n" ^ app_secret in
  Digestif.SHA256.hmac_string ~key:app_secret string_to_sign
  |> Digestif.SHA256.to_raw_string |> Base64.encode_exn

let get_access_token ~(config : Runtime_config.dingtalk_config) =
  let open Lwt.Syntax in
  let uri = open_api_base ^ "/oauth2/accessToken" in
  let body =
    `Assoc
      [
        ("appKey", `String config.app_key);
        ("appSecret", `String config.app_secret);
      ]
    |> Yojson.Safe.to_string
  in
  let* status, resp_body = Http_client.post_json ~uri ~headers:[] ~body in
  if status >= 200 && status < 300 then (
    try
      let json = Yojson.Safe.from_string resp_body in
      let open Yojson.Safe.Util in
      let token = json |> member "accessToken" |> to_string in
      Lwt.return (Some token)
    with exn ->
      Logs.err (fun m ->
          m "DingTalk: failed to parse token: %s" (Printexc.to_string exn));
      Lwt.return None)
  else begin
    Logs.warn (fun m -> m "DingTalk: token fetch failed (HTTP %d)" status);
    Lwt.return None
  end

let send_message ~(config : Runtime_config.dingtalk_config)
    ~open_conversation_id ~text =
  let open Lwt.Syntax in
  match config.webhook_url with
  | Some wh_url ->
      let body =
        `Assoc
          [
            ("msgtype", `String "text");
            ("text", `Assoc [ ("content", `String text) ]);
          ]
        |> Yojson.Safe.to_string
      in
      let* _status, _body =
        Http_client.post_json ~uri:wh_url ~headers:[] ~body
      in
      Lwt.return_unit
  | None -> (
      let* token_opt = get_access_token ~config in
      match token_opt with
      | None ->
          Logs.err (fun m -> m "DingTalk: cannot send message, no access token");
          Lwt.return_unit
      | Some token ->
          let uri = open_api_base ^ "/robot/messages/sendByConversation" in
          let headers = [ ("x-acs-dingtalk-access-token", token) ] in
          let body =
            `Assoc
              [
                ("openConversationId", `String open_conversation_id);
                ("robotCode", `String config.app_key);
                ( "msgParam",
                  `String
                    (Yojson.Safe.to_string
                       (`Assoc [ ("content", `String text) ])) );
                ("msgKey", `String "sampleText");
              ]
            |> Yojson.Safe.to_string
          in
          let* _status, _body = Http_client.post_json ~uri ~headers ~body in
          Lwt.return_unit)

let parse_stream_message ~event_type data =
  try
    if event_type <> "im.message.receive_v1" then None
    else
      let open Yojson.Safe.Util in
      let conversation_id =
        try data |> member "conversationId" |> to_string with _ -> ""
      in
      let sender_id =
        try data |> member "senderId" |> to_string with _ -> ""
      in
      let content =
        try data |> member "text" |> member "content" |> to_string
        with _ -> ""
      in
      (* conversationType "1" = private/1:1, "2" = group *)
      let conversation_type =
        try data |> member "conversationType" |> to_string with _ -> "2"
      in
      if conversation_id = "" || sender_id = "" || content = "" then None
      else Some (conversation_id, sender_id, content, conversation_type)
  with _ -> None

let start ~(config : Runtime_config.t) ~(session_manager : Session.t) =
  match config.channels.dingtalk with
  | None ->
      Logs.info (fun m -> m "No DingTalk config found, skipping");
      Lwt.return_unit
  | Some dt_config ->
      if dt_config.app_key = "" || dt_config.app_secret = "" then begin
        Logs.info (fun m ->
            m "DingTalk: app_key or app_secret is empty, skipping");
        Lwt.return_unit
      end
      else begin
        Logs.info (fun m -> m "DingTalk channel starting (stream mode)");
        let open Lwt.Syntax in
        let backoff = Channel_util.Backoff.create () in
        let rec connect_loop () =
          (* Step 1: Register via HTTP POST to get the WSS endpoint and ticket *)
          let reg_body =
            `Assoc
              [
                ("clientId", `String dt_config.app_key);
                ("clientSecret", `String dt_config.app_secret);
                ("ua", `String "clawq/1.0");
                ( "subscriptions",
                  `List
                    [
                      `Assoc
                        [ ("type", `String "EVENT"); ("topic", `String "*") ];
                    ] );
              ]
            |> Yojson.Safe.to_string
          in
          let result =
            Lwt.catch
              (fun () ->
                let* status, reg_resp =
                  Http_client.post_json ~uri:stream_register_url ~headers:[]
                    ~body:reg_body
                in
                if status < 200 || status >= 300 then
                  Lwt.fail_with
                    (Printf.sprintf
                       "DingTalk: stream registration failed (HTTP %d)" status)
                else
                  let reg_json = Yojson.Safe.from_string reg_resp in
                  let open Yojson.Safe.Util in
                  let endpoint = reg_json |> member "endpoint" |> to_string in
                  let ticket = reg_json |> member "ticket" |> to_string in
                  let ws_url = endpoint ^ "?ticket=" ^ Uri.pct_encode ticket in
                  let* ws = Ws_client.connect_wss ~uri:ws_url () in
                  Channel_util.Backoff.reset backoff;
                  Ws_client.on_message ws (fun msg ->
                      Lwt.catch
                        (fun () ->
                          let json = Yojson.Safe.from_string msg in
                          let open Yojson.Safe.Util in
                          (* Send ack for all messages *)
                          let msg_id =
                            try json |> member "messageId" |> to_string
                            with _ -> ""
                          in
                          let ack =
                            `Assoc
                              [
                                ("code", `Int 0);
                                ( "headers",
                                  `Assoc
                                    [
                                      ("contentType", `String "application/json");
                                      ("messageId", `String msg_id);
                                    ] );
                                ("message", `String "SUCCESS");
                                ("data", `String "{}");
                              ]
                            |> Yojson.Safe.to_string
                          in
                          let* () = Ws_client.send_text ws ack in
                          let event_type =
                            try
                              json |> member "headers" |> member "eventType"
                              |> to_string
                            with _ -> ""
                          in
                          let data =
                            try
                              let s = json |> member "data" |> to_string in
                              Yojson.Safe.from_string s
                            with _ -> json |> member "data"
                          in
                          match parse_stream_message ~event_type data with
                          | None -> Lwt.return_unit
                          | Some
                              ( conversation_id,
                                sender_id,
                                content,
                                conversation_type ) -> (
                              if not (is_allowed ~config:dt_config ~sender_id)
                              then (
                                Logs.warn (fun m ->
                                    m
                                      "DingTalk: ignoring message from \
                                       unauthorized sender=%s"
                                      sender_id);
                                Lwt.return_unit)
                              else
                                let channel_type =
                                  if conversation_type = "1" then "dm"
                                  else "group"
                                in
                                let key =
                                  "dingtalk:" ^ conversation_id ^ ":"
                                  ^ sender_id
                                in
                                Session.register_connector_capabilities
                                  session_manager ~key
                                  Connector_capabilities.dingtalk;
                                let* result =
                                  Session.with_registered_notifier
                                    session_manager ~key
                                    ~notify:(fun text ->
                                      send_message ~config:dt_config
                                        ~open_conversation_id:conversation_id
                                        ~text)
                                    (fun () ->
                                      Lwt.catch
                                        (fun () ->
                                          let* response =
                                            Session.turn session_manager ~key
                                              ~message:content
                                              ~channel_name:"dingtalk"
                                              ~channel_type ~sender_id
                                              ~snapshot_work_type:
                                                Access_snapshot.Room_turn ()
                                          in
                                          Lwt.return (Ok response))
                                        (fun exn ->
                                          Lwt.return
                                            (Error (Printexc.to_string exn))))
                                in
                                match result with
                                | Ok response
                                  when Session.should_suppress_response response
                                  ->
                                    Lwt.return_unit
                                | Ok response ->
                                    send_message ~config:dt_config
                                      ~open_conversation_id:conversation_id
                                      ~text:response
                                | Error err ->
                                    Logs.err (fun m ->
                                        m
                                          "DingTalk: agent error for \
                                           conversation=%s sender=%s: %s"
                                          conversation_id sender_id err);
                                    Lwt.return_unit))
                        (fun exn ->
                          Logs.err (fun m ->
                              m "DingTalk: message handler error: %s"
                                (Printexc.to_string exn));
                          Lwt.return_unit));
                  let* () = Ws_client.closed ws in
                  Lwt.return (Ok ()))
              (fun exn -> Lwt.return (Error (Printexc.to_string exn)))
          in
          let* outcome = result in
          (match outcome with
          | Error err ->
              Logs.err (fun m -> m "DingTalk: connection error: %s" err);
              Channel_util.Backoff.increase backoff
          | Ok () -> Logs.info (fun m -> m "DingTalk: connection closed"));
          let delay = Channel_util.Backoff.current backoff in
          Logs.info (fun m -> m "DingTalk: reconnecting in %.0fs" delay);
          let* () = Lwt_unix.sleep delay in
          connect_loop ()
        in
        connect_loop ()
      end
