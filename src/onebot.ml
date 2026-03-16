(* OneBot v11 channel (QQ and compatible bots) *)

let is_allowed_user ~(config : Runtime_config.onebot_config) ~user_id =
  Channel_util.is_allowed ~allowlist:config.allow_from user_id

let is_allowed_group ~(config : Runtime_config.onebot_config) ~group_id =
  Channel_util.is_allowed ~allowlist:config.allow_groups group_id

(* Split text into chunks of at most max_bytes bytes, on UTF-8 boundaries *)
let split_utf8 ~max_bytes text =
  let len = String.length text in
  if len <= max_bytes then [ text ]
  else
    let chunks = ref [] in
    let start = ref 0 in
    let chunk_start = ref 0 in
    while !start < len do
      (* Advance one UTF-8 character *)
      let byte = Char.code text.[!start] in
      let char_len =
        if byte land 0x80 = 0 then 1
        else if byte land 0xE0 = 0xC0 then 2
        else if byte land 0xF0 = 0xE0 then 3
        else if byte land 0xF8 = 0xF0 then 4
        else 1
      in
      let next = !start + char_len in
      if next - !chunk_start > max_bytes then begin
        if !start > !chunk_start then begin
          chunks :=
            String.sub text !chunk_start (!start - !chunk_start) :: !chunks;
          chunk_start := !start
        end
        else begin
          (* Single character exceeds limit; emit it alone *)
          chunks := String.sub text !start char_len :: !chunks;
          chunk_start := next
        end
      end;
      start := next
    done;
    if !chunk_start < len then
      chunks := String.sub text !chunk_start (len - !chunk_start) :: !chunks;
    List.rev !chunks

let send_private_msg ~(config : Runtime_config.onebot_config) ~user_id ~text =
  let open Lwt.Syntax in
  let uri = config.http_url ^ "/send_private_msg" in
  let headers =
    match config.access_token with
    | Some tok -> [ ("Authorization", "Bearer " ^ tok) ]
    | None -> []
  in
  let chunks = split_utf8 ~max_bytes:4500 text in
  Lwt_list.iter_s
    (fun chunk ->
      let body =
        `Assoc
          [
            ( "user_id",
              match int_of_string_opt user_id with
              | Some i -> `Int i
              | None -> `String user_id );
            ("message", `String chunk);
          ]
        |> Yojson.Safe.to_string
      in
      let* _status, _body = Http_client.post_json ~uri ~headers ~body in
      Lwt.return_unit)
    chunks

let send_group_msg ~(config : Runtime_config.onebot_config) ~group_id ~text =
  let open Lwt.Syntax in
  let uri = config.http_url ^ "/send_group_msg" in
  let headers =
    match config.access_token with
    | Some tok -> [ ("Authorization", "Bearer " ^ tok) ]
    | None -> []
  in
  let chunks = split_utf8 ~max_bytes:4500 text in
  Lwt_list.iter_s
    (fun chunk ->
      let body =
        `Assoc
          [
            ( "group_id",
              match int_of_string_opt group_id with
              | Some i -> `Int i
              | None -> `String group_id );
            ("message", `String chunk);
          ]
        |> Yojson.Safe.to_string
      in
      let* _status, _body = Http_client.post_json ~uri ~headers ~body in
      Lwt.return_unit)
    chunks

(* Extract text content from message field (string or array format) *)
let extract_text json =
  let open Yojson.Safe.Util in
  (* Try string format first *)
  try Some (json |> member "message" |> to_string)
  with _ -> (
    (* Try array format: [{type: "text", data: {text: "..."}}] *)
    try
      let segments = json |> member "message" |> to_list in
      let texts =
        List.filter_map
          (fun seg ->
            try
              let typ = seg |> member "type" |> to_string in
              if typ = "text" then
                Some (seg |> member "data" |> member "text" |> to_string)
              else None
            with _ -> None)
          segments
      in
      if texts = [] then None else Some (String.concat "" texts)
    with _ -> None)

let parse_message_event json =
  try
    let open Yojson.Safe.Util in
    let post_type =
      try json |> member "post_type" |> to_string with _ -> ""
    in
    if post_type <> "message" then None
    else
      let message_type =
        try json |> member "message_type" |> to_string with _ -> ""
      in
      let user_id =
        try json |> member "user_id" |> to_int |> string_of_int with _ -> ""
      in
      let text = match extract_text json with Some t -> t | None -> "" in
      if user_id = "" || text = "" then None
      else
        let group_id =
          try Some (json |> member "group_id" |> to_int |> string_of_int)
          with _ -> None
        in
        Some (message_type, user_id, group_id, text)
  with _ -> None

let start ~(config : Runtime_config.t) ~(session_manager : Session.t) =
  match config.channels.onebot with
  | None ->
      Logs.info (fun m -> m "No OneBot config found, skipping");
      Lwt.return_unit
  | Some ob_config ->
      if ob_config.ws_url = "" then begin
        Logs.info (fun m -> m "OneBot: ws_url is empty, skipping");
        Lwt.return_unit
      end
      else begin
        Logs.info (fun m ->
            m "OneBot channel starting (ws_url=%s)" ob_config.ws_url);
        let open Lwt.Syntax in
        let backoff = Channel_util.Backoff.create () in
        let rec connect_loop () =
          let result =
            Lwt.catch
              (fun () ->
                let is_tls =
                  let scheme = Uri.scheme (Uri.of_string ob_config.ws_url) in
                  scheme = Some "wss" || scheme = Some "https"
                in
                let* ws =
                  if is_tls then Ws_client.connect_wss ~uri:ob_config.ws_url ()
                  else Ws_client.connect_ws ~uri:ob_config.ws_url ()
                in
                Channel_util.Backoff.reset backoff;
                (* Send auth if token provided *)
                let* () =
                  match ob_config.access_token with
                  | Some tok ->
                      let auth_msg =
                        `Assoc
                          [
                            ("action", `String "meta::connect");
                            ("params", `Assoc [ ("access_token", `String tok) ]);
                          ]
                        |> Yojson.Safe.to_string
                      in
                      Ws_client.send_text ws auth_msg
                  | None -> Lwt.return_unit
                in
                Ws_client.on_message ws (fun msg ->
                    Lwt.catch
                      (fun () ->
                        let json = Yojson.Safe.from_string msg in
                        match parse_message_event json with
                        | None -> Lwt.return_unit
                        | Some (message_type, user_id, group_id, text) -> (
                            if not (is_allowed_user ~config:ob_config ~user_id)
                            then (
                              Logs.warn (fun m ->
                                  m
                                    "OneBot: ignoring message from \
                                     unauthorized user_id=%s"
                                    user_id);
                              Lwt.return_unit)
                            else
                              let group_ok =
                                match group_id with
                                | Some gid ->
                                    is_allowed_group ~config:ob_config
                                      ~group_id:gid
                                | None -> true
                              in
                              if not group_ok then Lwt.return_unit
                              else
                                let channel_type, key =
                                  match group_id with
                                  | Some gid ->
                                      ( "group",
                                        "onebot:group:" ^ gid ^ ":" ^ user_id )
                                  | None -> ("dm", "onebot:private:" ^ user_id)
                                in
                                let channel_name =
                                  match group_id with
                                  | Some gid -> gid
                                  | None -> user_id
                                in
                                let notify text =
                                  match (message_type, group_id) with
                                  | "group", Some gid ->
                                      send_group_msg ~config:ob_config
                                        ~group_id:gid ~text
                                  | _ ->
                                      send_private_msg ~config:ob_config
                                        ~user_id ~text
                                in
                                let* result =
                                  Session.with_registered_notifier
                                    session_manager ~key ~notify (fun () ->
                                      Lwt.catch
                                        (fun () ->
                                          let* response =
                                            Session.turn session_manager ~key
                                              ~message:text ~channel_name
                                              ~channel_type ~sender_id:user_id
                                              ()
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
                                | Ok response -> (
                                    match (message_type, group_id) with
                                    | "group", Some gid ->
                                        send_group_msg ~config:ob_config
                                          ~group_id:gid ~text:response
                                    | _ ->
                                        send_private_msg ~config:ob_config
                                          ~user_id ~text:response)
                                | Error err ->
                                    Logs.err (fun m ->
                                        m "OneBot: agent error for user=%s: %s"
                                          user_id err);
                                    Lwt.return_unit))
                      (fun exn ->
                        Logs.err (fun m ->
                            m "OneBot: message handler error: %s"
                              (Printexc.to_string exn));
                        Lwt.return_unit));
                let* () = Ws_client.closed ws in
                Lwt.return (Ok ()))
              (fun exn -> Lwt.return (Error (Printexc.to_string exn)))
          in
          let* outcome = result in
          (match outcome with
          | Error err ->
              Logs.err (fun m -> m "OneBot: connection error: %s" err);
              Channel_util.Backoff.increase backoff
          | Ok () -> Logs.info (fun m -> m "OneBot: connection closed"));
          let delay = Channel_util.Backoff.current backoff in
          Logs.info (fun m -> m "OneBot: reconnecting in %.0fs" delay);
          let* () = Lwt_unix.sleep delay in
          connect_loop ()
        in
        connect_loop ()
      end
