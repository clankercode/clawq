(* Mattermost channel via WebSocket API *)

let strip_trailing_slash s =
  if String.length s > 0 && s.[String.length s - 1] = '/' then
    String.sub s 0 (String.length s - 1)
  else s

let is_allowed ~(config : Runtime_config.mattermost_config) ~user_id =
  Channel_util.is_allowed ~allowlist:config.allow_users user_id

let is_allowed_channel ~(config : Runtime_config.mattermost_config) ~channel_id
    =
  match config.channel_ids with [] -> true | ids -> List.mem channel_id ids

let send_message ~(config : Runtime_config.mattermost_config) ~channel_id ~text
    =
  let open Lwt.Syntax in
  let base = strip_trailing_slash config.url in
  let uri = Printf.sprintf "%s/api/v4/posts" base in
  let headers = [ ("Authorization", "Bearer " ^ config.access_token) ] in
  let body =
    `Assoc [ ("channel_id", `String channel_id); ("message", `String text) ]
    |> Yojson.Safe.to_string
  in
  let* status, resp_body = Http_client.post_json ~uri ~headers ~body in
  if status >= 300 then
    Logs.err (fun m ->
        m "Mattermost: send_message failed status=%d body=%s" status resp_body);
  Lwt.return_unit

let fetch_self_user_id ~(config : Runtime_config.mattermost_config) =
  let open Lwt.Syntax in
  let base = strip_trailing_slash config.url in
  let uri = Printf.sprintf "%s/api/v4/users/me" base in
  let headers = [ ("Authorization", "Bearer " ^ config.access_token) ] in
  let* status, body = Http_client.get ~uri ~headers in
  if status = 200 then
    try
      let json = Yojson.Safe.from_string body in
      let id = Yojson.Safe.Util.(json |> member "id" |> to_string) in
      Lwt.return (Some id)
    with _ -> Lwt.return None
  else begin
    Logs.warn (fun m ->
        m "Mattermost: failed to fetch self user_id (status=%d)" status);
    Lwt.return None
  end

(* Map a Mattermost channel type code to clawq's channel_type convention.
   Mattermost uses "D"=direct, "G"=group DM, "O"=open, "P"=private. Only direct
   messages are 1:1 conversations; everything else is treated as group chat so
   the agent applies group-chat conduct (respond only when addressed). *)
let clawq_channel_type_of_mm = function "D" -> "dm" | _ -> "group"

let parse_posted_event data =
  try
    let open Yojson.Safe.Util in
    let post_str = data |> member "post" |> to_string in
    let post = Yojson.Safe.from_string post_str in
    let channel_id = post |> member "channel_id" |> to_string in
    let user_id = post |> member "user_id" |> to_string in
    let message = post |> member "message" |> to_string in
    (* channel_type lives on the broadcast data, sibling of "post". Default to
       group ("") when absent so unknown payloads keep group-chat conduct. *)
    let channel_type =
      try data |> member "channel_type" |> to_string with _ -> ""
    in
    Some (channel_id, user_id, message, channel_type)
  with _ -> None

let start ~(config : Runtime_config.t) ~(session_manager : Session.t) =
  match config.channels.mattermost with
  | None ->
      Logs.info (fun m -> m "No Mattermost config found, skipping");
      Lwt.return_unit
  | Some mm_config ->
      if mm_config.url = "" || mm_config.access_token = "" then begin
        Logs.info (fun m ->
            m "Mattermost: url or access_token is empty, skipping");
        Lwt.return_unit
      end
      else begin
        Logs.info (fun m ->
            m "Mattermost channel starting (url=%s)" mm_config.url);
        let open Lwt.Syntax in
        (* Fetch bot's own user_id to avoid self-message loops *)
        let* self_user_id = fetch_self_user_id ~config:mm_config in
        (match self_user_id with
        | Some id -> Logs.info (fun m -> m "Mattermost: bot user_id=%s" id)
        | None ->
            Logs.warn (fun m ->
                m
                  "Mattermost: could not determine bot user_id, self-message \
                   filtering disabled"));
        (* Convert http(s):// URL to ws(s):// for WebSocket *)
        let ws_url =
          let base = strip_trailing_slash mm_config.url in
          if String.length base >= 8 && String.sub base 0 8 = "https://" then
            "wss://"
            ^ String.sub base 8 (String.length base - 8)
            ^ "/api/v4/websocket"
          else if String.length base >= 7 && String.sub base 0 7 = "http://"
          then
            "ws://"
            ^ String.sub base 7 (String.length base - 7)
            ^ "/api/v4/websocket"
          else base ^ "/api/v4/websocket"
        in
        let backoff = Channel_util.Backoff.create () in
        let rec connect_loop () =
          let result =
            Lwt.catch
              (fun () ->
                let* ws = Ws_client.connect_wss ~uri:ws_url () in
                Channel_util.Backoff.reset backoff;
                (* Send auth challenge *)
                let auth_msg =
                  `Assoc
                    [
                      ("action", `String "authentication_challenge");
                      ("seq", `Int 1);
                      ( "data",
                        `Assoc [ ("token", `String mm_config.access_token) ] );
                    ]
                  |> Yojson.Safe.to_string
                in
                let* () = Ws_client.send_text ws auth_msg in
                Ws_client.on_message ws (fun msg ->
                    Lwt.catch
                      (fun () ->
                        let json = Yojson.Safe.from_string msg in
                        let open Yojson.Safe.Util in
                        let event =
                          try json |> member "event" |> to_string with _ -> ""
                        in
                        if event = "posted" then
                          let data = json |> member "data" in
                          match parse_posted_event data with
                          | None -> Lwt.return_unit
                          | Some (channel_id, user_id, message, mm_channel_type)
                            -> (
                              if self_user_id = Some user_id then
                                Lwt.return_unit
                              else if
                                not (is_allowed ~config:mm_config ~user_id)
                              then (
                                Logs.warn (fun m ->
                                    m
                                      "Mattermost: ignoring message from \
                                       unauthorized user=%s"
                                      user_id);
                                Lwt.return_unit)
                              else if
                                not
                                  (is_allowed_channel ~config:mm_config
                                     ~channel_id)
                              then Lwt.return_unit
                              else if message = "" then Lwt.return_unit
                              else
                                let key =
                                  "mattermost:" ^ channel_id ^ ":" ^ user_id
                                in
                                Session.register_connector_capabilities
                                  session_manager ~key
                                  Connector_capabilities.mattermost;
                                let* result =
                                  Session.with_registered_notifier
                                    session_manager ~key
                                    ~notify:(fun text ->
                                      send_message ~config:mm_config ~channel_id
                                        ~text)
                                    (fun () ->
                                      Lwt.catch
                                        (fun () ->
                                          let* response =
                                            Session.turn session_manager ~key
                                              ~message
                                              ~channel_name:"mattermost"
                                              ~channel_type:
                                                (clawq_channel_type_of_mm
                                                   mm_channel_type)
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
                                    send_message ~config:mm_config ~channel_id
                                      ~text:response
                                | Error err ->
                                    Logs.err (fun m ->
                                        m
                                          "Mattermost: agent error for \
                                           channel=%s user=%s: %s"
                                          channel_id user_id err);
                                    Lwt.return_unit)
                        else Lwt.return_unit)
                      (fun exn ->
                        Logs.err (fun m ->
                            m "Mattermost: message handler error: %s"
                              (Printexc.to_string exn));
                        Lwt.return_unit));
                let* () = Ws_client.closed ws in
                Lwt.return (Ok ()))
              (fun exn -> Lwt.return (Error (Printexc.to_string exn)))
          in
          let* outcome = result in
          (match outcome with
          | Error err ->
              Logs.err (fun m -> m "Mattermost: connection error: %s" err);
              Channel_util.Backoff.increase backoff
          | Ok () -> Logs.info (fun m -> m "Mattermost: connection closed"));
          let delay = Channel_util.Backoff.current backoff in
          Logs.info (fun m -> m "Mattermost: reconnecting in %.0fs" delay);
          let* () = Lwt_unix.sleep delay in
          connect_loop ()
        in
        connect_loop ()
      end
