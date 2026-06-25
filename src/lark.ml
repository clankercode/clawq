(* Lark/Feishu channel *)

let feishu_base = "https://open.feishu.cn/open-apis"
let lark_base = "https://open.larksuite.com/open-apis"
let dedup = Channel_util.Lru_dedup.create 500

let dedup_seen id =
  if id = "" then false else Channel_util.Lru_dedup.check_and_mark dedup id

(* Strip @_user_N mention placeholders from text *)
let strip_mention_placeholders text =
  let buf = Buffer.create (String.length text) in
  let len = String.length text in
  let i = ref 0 in
  while !i < len do
    if
      !i + 6 < len
      && text.[!i] = '@'
      && text.[!i + 1] = '_'
      && text.[!i + 2] = 'u'
      && text.[!i + 3] = 's'
      && text.[!i + 4] = 'e'
      && text.[!i + 5] = 'r'
      && text.[!i + 6] = '_'
    then begin
      (* skip @_user_ then digits *)
      i := !i + 7;
      while !i < len && text.[!i] >= '0' && text.[!i] <= '9' do
        i := !i + 1
      done;
      (* skip one trailing space if present *)
      if !i < len && text.[!i] = ' ' then i := !i + 1
    end
    else begin
      Buffer.add_char buf text.[!i];
      i := !i + 1
    end
  done;
  Buffer.contents buf

let api_base (endpoint : string) =
  if endpoint = "lark" then lark_base else feishu_base

(* Tenant access token cache *)
let token_cache : (string * float) option ref = ref None
(* F4: global mutable state — safe under OCaml 5.1 cooperative Lwt (single
   domain). If multi-domain parallelism is introduced, wrap in Atomic.t or
   protect with a mutex. *)

let get_tenant_access_token ~(config : Runtime_config.lark_config) =
  let open Lwt.Syntax in
  let now = Unix.gettimeofday () in
  match !token_cache with
  | Some (token, expiry) when now < expiry -> Lwt.return (Some token)
  | _ ->
      let base = api_base config.endpoint in
      let uri = base ^ "/auth/v3/tenant_access_token/internal" in
      let body =
        `Assoc
          [
            ("app_id", `String config.app_id);
            ("app_secret", `String config.app_secret);
          ]
        |> Yojson.Safe.to_string
      in
      let* status, resp_body = Http_client.post_json ~uri ~headers:[] ~body in
      if status >= 200 && status < 300 then (
        try
          let json = Yojson.Safe.from_string resp_body in
          let open Yojson.Safe.Util in
          let token = json |> member "tenant_access_token" |> to_string in
          let expire = try json |> member "expire" |> to_int with _ -> 7200 in
          (* Cache with 60s safety margin *)
          let expiry = now +. float_of_int expire -. 60.0 in
          token_cache := Some (token, expiry);
          Lwt.return (Some token)
        with exn ->
          Logs.err (fun m ->
              m "Lark: failed to parse token response: %s"
                (Printexc.to_string exn));
          Lwt.return None)
      else begin
        Logs.warn (fun m -> m "Lark: token fetch failed (HTTP %d)" status);
        Lwt.return None
      end

let is_allowed ~(config : Runtime_config.lark_config) ~user_id =
  Channel_util.is_allowed ~allowlist:config.allow_users user_id

(* Verify Lark webhook signature: HMAC-SHA256 of timestamp + nonce + body *)
let verify_lark_signature ~verification_token ~timestamp ~nonce ~body ~signature
    =
  (* Reject stale timestamps (>300s) to prevent replay attacks *)
  let ts_ok =
    match float_of_string_opt timestamp with
    | Some ts ->
        let age = abs_float (Unix.gettimeofday () -. ts) in
        age < 300.0
    | None -> false
  in
  if not ts_ok then false
  else
    let payload = timestamp ^ nonce ^ body in
    let computed =
      Digestif.SHA256.hmac_string ~key:verification_token payload
      |> Digestif.SHA256.to_hex
    in
    Eqaf.equal computed signature

let send_message ~(config : Runtime_config.lark_config) ~chat_id ~text =
  let open Lwt.Syntax in
  let* token_opt = get_tenant_access_token ~config in
  match token_opt with
  | None ->
      Logs.err (fun m -> m "Lark: cannot send message, no token");
      Lwt.return_unit
  | Some token ->
      let base = api_base config.endpoint in
      let uri = base ^ "/im/v1/messages?receive_id_type=chat_id" in
      let headers = [ ("Authorization", "Bearer " ^ token) ] in
      let body =
        `Assoc
          [
            ("receive_id", `String chat_id);
            ("msg_type", `String "text");
            ( "content",
              `String
                (Yojson.Safe.to_string (`Assoc [ ("text", `String text) ])) );
          ]
        |> Yojson.Safe.to_string
      in
      let* _status, _body = Http_client.post_json ~uri ~headers ~body in
      Lwt.return_unit

let parse_message_event json =
  try
    let open Yojson.Safe.Util in
    let event = json |> member "event" in
    let message = event |> member "message" in
    let sender = event |> member "sender" in
    let chat_id = message |> member "chat_id" |> to_string in
    let chat_type =
      try message |> member "chat_type" |> to_string with _ -> "p2p"
    in
    let user_id =
      try sender |> member "sender_id" |> member "open_id" |> to_string
      with _ -> ""
    in
    let raw_text =
      try
        let content_str = message |> member "content" |> to_string in
        let content = Yojson.Safe.from_string content_str in
        content |> member "text" |> to_string
      with _ -> ""
    in
    let text = strip_mention_placeholders raw_text in
    let event_id =
      try json |> member "header" |> member "event_id" |> to_string
      with _ -> ""
    in
    (* For group chats, only process if bot was @-mentioned *)
    let mentions_present =
      if chat_type = "group" then
        try
          let mentions = message |> member "mentions" |> to_list in
          List.length mentions > 0
        with _ -> false
      else true
    in
    if text = "" || chat_id = "" || not mentions_present then None
    else Some (event_id, chat_id, user_id, chat_type, text)
  with _ -> None

let handle_webhook_body ~(config : Runtime_config.lark_config)
    ~(session_mgr : Session.t) body_str =
  let open Lwt.Syntax in
  (* H3: wrap entire body in Lwt.catch to catch both synchronous exceptions
     (JSON parse) and async Lwt promise rejections (send_message, Session.turn). *)
  Lwt.catch
    (fun () ->
      try
        let json = Yojson.Safe.from_string body_str in
        let open Yojson.Safe.Util in
        (* Challenge verification (URL verification) *)
        let challenge =
          try Some (json |> member "challenge" |> to_string) with _ -> None
        in
        match challenge with
        | Some ch ->
            let resp =
              `Assoc [ ("challenge", `String ch) ] |> Yojson.Safe.to_string
            in
            Lwt.return (`Challenge resp)
        | None -> (
            match parse_message_event json with
            | None -> Lwt.return (`Ok {|{"code":0}|})
            | Some (event_id, chat_id, user_id, chat_type, text) -> (
                if dedup_seen event_id then Lwt.return (`Ok {|{"code":0}|})
                else if not (is_allowed ~config ~user_id) then (
                  Logs.warn (fun m ->
                      m "Lark: ignoring message from unauthorized user=%s"
                        user_id);
                  Lwt.return (`Ok {|{"code":0}|}))
                else
                  let channel_type =
                    if chat_type = "p2p" then "dm" else "group"
                  in
                  let key = "lark:" ^ chat_id ^ ":" ^ user_id in
                  Session.register_connector_capabilities session_mgr ~key
                    Connector_capabilities.lark;
                  let* result =
                    Session.with_registered_notifier session_mgr ~key
                      ~notify:(fun text -> send_message ~config ~chat_id ~text)
                      (fun () ->
                        Lwt.catch
                          (fun () ->
                            let* response =
                              Session.turn session_mgr ~key ~message:text
                                ~channel_name:"lark" ~channel_type
                                ~sender_id:user_id ()
                            in
                            Lwt.return (Ok response))
                          (fun exn ->
                            Lwt.return (Error (Printexc.to_string exn))))
                  in
                  match result with
                  | Ok response ->
                      if Session.should_suppress_response response then
                        Lwt.return (`Ok {|{"code":0}|})
                      else
                        let* () =
                          send_message ~config ~chat_id ~text:response
                        in
                        Lwt.return (`Ok {|{"code":0}|})
                  | Error err ->
                      Logs.err (fun m ->
                          m "Lark: agent error for chat=%s user=%s: %s" chat_id
                            user_id err);
                      Lwt.return (`Error err)))
      with exn ->
        (* H3: catch synchronous exceptions (JSON parse, etc.) *)
        Logs.warn (fun m ->
            m "Lark handle_webhook_body exception: %s" (Printexc.to_string exn));
        Lwt.return (`Error (Printexc.to_string exn)))
    (fun exn ->
      (* H3: catch async Lwt promise rejections *)
      Logs.warn (fun m ->
          m "Lark handle_webhook_body async error: %s" (Printexc.to_string exn));
      Lwt.return (`Error (Printexc.to_string exn)))

let start ~(config : Runtime_config.t) ~(session_manager : Session.t) =
  match config.channels.lark with
  | None ->
      Logs.info (fun m -> m "No Lark config found, skipping");
      Lwt.return_unit
  | Some lark_config ->
      if not lark_config.enabled then begin
        Logs.info (fun m -> m "Lark channel disabled in config, skipping");
        Lwt.return_unit
      end
      else if lark_config.mode = "webhook" then begin
        Logs.info (fun m ->
            m "Lark channel configured in webhook mode (endpoint=%s)"
              lark_config.endpoint);
        Lwt.return_unit
      end
      else begin
        (* WebSocket mode *)
        Logs.info (fun m ->
            m "Lark channel: starting websocket mode (endpoint=%s)"
              lark_config.endpoint);
        let rec connect_loop () =
          let open Lwt.Syntax in
          let* token_opt = get_tenant_access_token ~config:lark_config in
          match token_opt with
          | None ->
              Logs.err (fun m ->
                  m "Lark WS: cannot get access token, retry in 30s");
              let* () = Lwt_unix.sleep 30.0 in
              connect_loop ()
          | Some _token ->
              let ws_host =
                if lark_config.endpoint = "lark" then "open.larksuite.com"
                else "open.feishu.cn"
              in
              let ws_path = "/event/ws?app_id=" ^ lark_config.app_id in
              let uri = Printf.sprintf "wss://%s%s" ws_host ws_path in
              let* () =
                Lwt.catch
                  (fun () ->
                    let* ws = Ws_client.connect_wss ~uri () in
                    Ws_client.on_message ws (fun msg ->
                        let open Lwt.Syntax in
                        try
                          let json = Yojson.Safe.from_string msg in
                          let open Yojson.Safe.Util in
                          let log_id =
                            try
                              json |> member "header" |> member "logId"
                              |> to_string
                            with _ -> ""
                          in
                          (* H4: process message first, then ACK only on
                             success. If processing fails, skip ACK so the
                             server can retry the message. *)
                          let* result =
                            handle_webhook_body ~config:lark_config
                              ~session_mgr:session_manager msg
                          in
                          match result with
                          | `Ok _ | `Challenge _ ->
                              let ack =
                                Yojson.Safe.to_string
                                  (`Assoc
                                     [
                                       ("code", `Int 0);
                                       ("logId", `String log_id);
                                     ])
                              in
                              let* () = Ws_client.send_text ws ack in
                              Lwt.return_unit
                          | `Error _ ->
                              (* H4: skip ACK to allow server retry *)
                              Lwt.return_unit
                        with exn ->
                          Logs.warn (fun m ->
                              m "Lark WS message error: %s"
                                (Printexc.to_string exn));
                          Lwt.return_unit);
                    Ws_client.closed ws)
                  (fun exn ->
                    Logs.warn (fun m ->
                        m "Lark WS: disconnected: %s, reconnecting in 5s"
                          (Printexc.to_string exn));
                    Lwt_unix.sleep 5.0)
              in
              connect_loop ()
        in
        connect_loop ()
      end
