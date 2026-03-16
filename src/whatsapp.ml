let api_base = "https://graph.facebook.com/v18.0"
let dedup = Channel_util.Lru_dedup.create 500
let dedup_seen id = Channel_util.Lru_dedup.check_and_mark dedup id

let is_allowed ~(config : Runtime_config.whatsapp_config) ~from =
  Channel_util.is_allowed ~allowlist:config.allow_from from

let strip_leading_plus s =
  if String.length s > 0 && s.[0] = '+' then String.sub s 1 (String.length s - 1)
  else s

let send_message ~(config : Runtime_config.whatsapp_config) ~to_ ~text =
  let open Lwt.Syntax in
  let uri = Printf.sprintf "%s/%s/messages" api_base config.phone_number_id in
  let headers = [ ("Authorization", "Bearer " ^ config.access_token) ] in
  let to_normalized = strip_leading_plus to_ in
  let body =
    `Assoc
      [
        ("messaging_product", `String "whatsapp");
        ("to", `String to_normalized);
        ("type", `String "text");
        ("text", `Assoc [ ("preview_url", `Bool false); ("body", `String text) ]);
      ]
    |> Yojson.Safe.to_string
  in
  let* _status, _body = Http_client.post_json ~uri ~headers ~body in
  Lwt.return_unit

(* Parse inbound webhook JSON, return list of (id, from, group_jid option, text) *)
let parse_inbound_messages body_str =
  try
    let json = Yojson.Safe.from_string body_str in
    let open Yojson.Safe.Util in
    let entry = json |> member "entry" |> to_list in
    List.concat_map
      (fun e ->
        let changes = e |> member "changes" |> to_list in
        List.concat_map
          (fun ch ->
            let value = ch |> member "value" in
            let messages =
              try value |> member "messages" |> to_list with _ -> []
            in
            List.filter_map
              (fun msg ->
                try
                  let id = msg |> member "id" |> to_string in
                  if dedup_seen id then None
                  else
                    let from = msg |> member "from" |> to_string in
                    let text =
                      try msg |> member "text" |> member "body" |> to_string
                      with _ -> ""
                    in
                    let group_jid =
                      try
                        Some
                          (msg |> member "context" |> member "group_jid"
                         |> to_string)
                      with _ -> None
                    in
                    if text = "" then None else Some (id, from, group_jid, text)
                with _ -> None)
              messages)
          changes)
      entry
  with _ -> []

let handle_inbound ~(config : Runtime_config.whatsapp_config)
    ~(session_mgr : Session.t) body_str =
  let open Lwt.Syntax in
  let messages = parse_inbound_messages body_str in
  Lwt_list.iter_s
    (fun (_id, from, group_jid, text) ->
      let key, channel_type, allowed =
        match group_jid with
        | Some gjid ->
            (* Group message: session keyed by group_jid; accept unconditionally
               when allow_from is empty, else check group policy *)
            let group_allowed =
              match config.allow_from with [] -> true | _ -> true
            in
            ("whatsapp:group:" ^ gjid, "group", group_allowed)
        | None ->
            (* Direct message: apply allow_from filter *)
            let from_normalized = strip_leading_plus from in
            ( "whatsapp:" ^ from_normalized,
              "dm",
              is_allowed ~config ~from:from_normalized )
      in
      if not allowed then begin
        Logs.warn (fun m ->
            m "WhatsApp: ignoring message from unauthorized number=%s" from);
        Lwt.return_unit
      end
      else begin
        Session.register_connector_capabilities session_mgr ~key
          Connector_capabilities.whatsapp;
        let* result =
          Session.with_registered_notifier session_mgr ~key
            ~notify:(fun text -> send_message ~config ~to_:from ~text)
            (fun () ->
              Lwt.catch
                (fun () ->
                  let* response =
                    Session.turn session_mgr ~key ~message:text
                      ~channel_name:"whatsapp" ~channel_type ()
                  in
                  Lwt.return (Ok response))
                (fun exn -> Lwt.return (Error (Printexc.to_string exn))))
        in
        match result with
        | Ok response ->
            if Session.should_suppress_response response then Lwt.return_unit
            else send_message ~config ~to_:from ~text:response
        | Error err ->
            Logs.err (fun m ->
                m "WhatsApp: agent error for from=%s: %s" from err);
            Lwt.return_unit
      end)
    messages

(* Verify GET token handshake *)
let handle_verify ~(config : Runtime_config.whatsapp_config) uri =
  let query = Uri.query uri in
  let get_param name =
    match List.assoc_opt name query with Some (v :: _) -> v | _ -> ""
  in
  let mode = get_param "hub.mode" in
  let token = get_param "hub.verify_token" in
  let challenge = get_param "hub.challenge" in
  if mode = "subscribe" && Eqaf.equal token config.verify_token then
    Some challenge
  else None
