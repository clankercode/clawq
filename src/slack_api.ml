(* Shared Slack API, parsing, and session helpers. *)

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

let should_salute_queued_interrupt ~inbound_text ~response =
  Connector_status.is_interrupt_ack_message inbound_text
  && Session.is_queued_message_response response

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

(** {1 Private channel policy}

    Defense-in-depth: under the default [Deny] policy, Slack private channels
    are refused even if they appear in [allow_channels]. To use a private
    channel the operator must explicitly list it in [allow_private_channels]. *)

(** Simple TTL cache for channel privacy lookups. Entries expire after
    [cache_ttl_s] seconds to avoid stale data if a channel's privacy changes. *)

let _channel_privacy_cache : (string * (bool * float)) list ref = ref []
let _channel_privacy_cache_ttl_s = 300.0

let _channel_privacy_cache_lookup channel_id =
  match List.assoc_opt channel_id !_channel_privacy_cache with
  | Some (is_private, ts) ->
      let now = Unix.gettimeofday () in
      if now -. ts < _channel_privacy_cache_ttl_s then Some is_private
      else begin
        _channel_privacy_cache :=
          List.filter (fun (k, _) -> k <> channel_id) !_channel_privacy_cache;
        None
      end
  | None -> None

let _channel_privacy_cache_store channel_id is_private =
  let now = Unix.gettimeofday () in
  _channel_privacy_cache :=
    (channel_id, (is_private, now))
    :: List.filter (fun (k, _) -> k <> channel_id) !_channel_privacy_cache

(** Fetch channel metadata from Slack [conversations.info] and return
    [Some is_private] on success, [None] on API failure. *)
let fetch_channel_is_private ~bot_token ~channel_id =
  let open Lwt.Syntax in
  match _channel_privacy_cache_lookup channel_id with
  | Some cached -> Lwt.return (Some cached)
  | None ->
      let uri =
        Printf.sprintf "https://slack.com/api/conversations.info?channel=%s"
          channel_id
      in
      let headers = [ ("Authorization", "Bearer " ^ bot_token) ] in
      Lwt.catch
        (fun () ->
          let* _status, body = Http_client.get ~uri ~headers in
          try
            let json = Yojson.Safe.from_string body in
            let open Yojson.Safe.Util in
            let ok = try json |> member "ok" |> to_bool with _ -> false in
            if ok then (
              let channel = json |> member "channel" in
              let is_private =
                try channel |> member "is_private" |> to_bool with _ -> false
              in
              _channel_privacy_cache_store channel_id is_private;
              Lwt.return (Some is_private))
            else begin
              Logs.warn (fun m ->
                  m "Slack conversations.info failed for %s: %s" channel_id
                    (try json |> member "error" |> to_string
                     with _ -> "unknown"));
              Lwt.return None
            end
          with exn ->
            Logs.warn (fun m ->
                m "Slack conversations.info parse error for %s: %s" channel_id
                  (Printexc.to_string exn));
            Lwt.return None)
        (fun exn ->
          Logs.warn (fun m ->
              m "Slack conversations.info HTTP error for %s: %s" channel_id
                (Printexc.to_string exn));
          Lwt.return None)

(** Check the private-channel policy for a channel. Returns [true] if the
    channel is allowed under the policy, [false] if it should be refused.
    [is_private_opt = None] means the API call failed — under [Deny] we
    conservatively allow (the allow-list gate already passed), since we cannot
    determine privacy status. *)
let check_private_channel_policy ~(config : Runtime_config.slack_config)
    ~channel_id ~is_private_opt =
  match config.private_channel_policy with
  | Runtime_config.Pc_allow_if_listed -> true
  | Runtime_config.Pc_deny -> (
      match is_private_opt with
      | None ->
          (* API failed — cannot determine privacy. Fail closed: deny unless
             channel is explicitly in allow_private_channels. *)
          is_allowed_allowlist ~kind:"private_channel" ~id:channel_id
            config.allow_private_channels
      | Some false -> (* Public channel — always allowed *) true
      | Some true ->
          (* Private channel — only allowed if explicitly listed *)
          is_allowed_allowlist ~kind:"private_channel" ~id:channel_id
            config.allow_private_channels)

(** Validate that [allow_channels] does not contain private channels under the
    [Deny] policy. Returns a list of warning messages for any private channels
    found. *)
let validate_private_channels_in_allowlist ~bot_token ~allow_channels
    ~private_channel_policy ~allow_private_channels =
  let open Lwt.Syntax in
  match private_channel_policy with
  | Runtime_config.Pc_allow_if_listed -> Lwt.return []
  | Runtime_config.Pc_deny ->
      let* warnings =
        Lwt_list.filter_map_s
          (fun channel_id ->
            if channel_id = "*" then Lwt.return None
            else
              let* is_private_opt =
                fetch_channel_is_private ~bot_token ~channel_id
              in
              match is_private_opt with
              | Some true ->
                  let in_explicit =
                    is_allowed_allowlist ~kind:"private_channel" ~id:channel_id
                      allow_private_channels
                  in
                  if not in_explicit then
                    Lwt.return
                      (Some
                         (Printf.sprintf
                            "Slack channel %s is private and listed \
                             in                              allow_channels \
                             but not in \
                             allow_private_channels.                              \
                             Under the default 'deny' policy this channel \
                             will                              be refused. Add \
                             it to allow_private_channels \
                             to                              opt in."
                            channel_id))
                  else Lwt.return None
              | _ -> Lwt.return None)
          allow_channels
      in
      Lwt.return warnings

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
