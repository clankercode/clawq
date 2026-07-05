let api_base = "https://discord.com/api/v10"

let should_salute_queued_interrupt ~inbound_text ~response =
  Connector_status.is_interrupt_ack_message inbound_text
  && Session.is_queued_message_response response

let current_thinking_message current =
  Printf.sprintf "Current thinking level: %s"
    (Slash_commands.thinking_level_to_string current)

let is_allowed_allowlist ~kind ~id allowlist =
  let coq_allowed = Clawq_core.is_allowed0 id allowlist in
  let ocaml_allowed = Channel_util.is_allowed ~allowlist id in
  if coq_allowed <> ocaml_allowed then
    Logs.warn (fun m ->
        m "Discord allowlist drift for %s=%s: Coq=%b OCaml=%b" kind id
          coq_allowed ocaml_allowed);
  coq_allowed

type discord_attachment = {
  att_id : string;
  att_filename : string;
  att_url : string;
  att_content_type : string option;
  att_size : int;
}

type message = {
  id : string;
  channel_id : string;
  guild_id : string option;
  author_id : string;
  author_bot : bool;
  content : string;
  mention_ids : string list;
  attachments : discord_attachment list;
}

type resume_state = {
  session_id : string;
  seq : int;
  resume_gateway_url : string;
}

let bot_user_id_ref : string option ref = ref None

let is_allowed ~(config : Runtime_config.discord_config) ~guild_id ~user_id =
  let guild_ok =
    let guild_id = match guild_id with Some g -> g | None -> "" in
    is_allowed_allowlist ~kind:"guild" ~id:guild_id config.allow_guilds
  in
  let user_ok =
    is_allowed_allowlist ~kind:"user" ~id:user_id config.allow_users
  in
  guild_ok && user_ok

let session_key ~channel_id ~author_id =
  Printf.sprintf "discord:%s:%s"
    (Session.sanitize_session_key channel_id)
    (Session.sanitize_session_key author_id)

let room_has_profile_binding ~(session_mgr : Session.t) ~channel_id =
  match Session.get_db session_mgr with
  | Some db -> (
      match Memory.get_room_profile_binding ~db ~room_id:channel_id with
      | Some _ -> true
      | None -> false)
  | None -> false

let scoped_room_history_key ~channel_id =
  "discord:" ^ Session.sanitize_session_key channel_id

let history_key_for_channel ~(session_mgr : Session.t) ~channel_id =
  if room_has_profile_binding ~session_mgr ~channel_id then
    scoped_room_history_key ~channel_id
  else Printf.sprintf "discord-hist:%s" channel_id

let record_scoped_room_history_if_bound ~(session_mgr : Session.t) ~channel_id
    ~author_id ~content =
  let cfg = Session.get_config session_mgr in
  if
    String.trim content <> ""
    && room_has_profile_binding ~session_mgr ~channel_id
    && Connector_capabilities.should_capture_history
         ~enabled:cfg.connector_history.enabled Connector_capabilities.discord
  then
    let key = scoped_room_history_key ~channel_id in
    let db =
      if cfg.connector_history.persist_to_db then Session.get_db session_mgr
      else None
    in
    Connector_history.record ?db ~persist:cfg.connector_history.persist_to_db
      ~key ~room_id:channel_id ~connector_type:"discord" ~channel_type:"discord"
      ~max:cfg.connector_history.max_messages ~sender_name:author_id
      ~sender_id:author_id ~text:content ()

let chunk_text ?(max_len = 2000) text =
  Channel_util.chunk_text ~prefer_newline_break:false ~max_len text

(* Tracks message IDs whose reactions should be kept in sync per session key *)
let reactions : string Reaction_tracker.t = Reaction_tracker.create ()

(* --- Discord REST rate limit tracking --- *)

type route_bucket = { mutable remaining : int; mutable reset_at : float }

let route_buckets : (string, route_bucket) Hashtbl.t = Hashtbl.create 32

(* Per-route mutexes so that rate-limit waits and 429 backoffs on one route
   serialize that route's bucket consumption without blocking other routes.
   A single global mutex would let a rate-limited channel stall all channels.
   Lookup/insert is atomic under Lwt's cooperative scheduling (no yield). *)
let route_mutexes : (string, Lwt_mutex.t) Hashtbl.t = Hashtbl.create 32

let route_mutex_for route =
  match Hashtbl.find_opt route_mutexes route with
  | Some m -> m
  | None ->
      let m = Lwt_mutex.create () in
      Hashtbl.replace route_mutexes route m;
      m

let global_rate_limit : float ref = ref 0.0
let _rate_limit_warnings : (string, float) Hashtbl.t = Hashtbl.create 32

let wait_for_rate_limit ~route =
  let open Lwt.Syntax in
  let now = Unix.gettimeofday () in
  let* () =
    if !global_rate_limit > now then Lwt_unix.sleep (!global_rate_limit -. now)
    else Lwt.return_unit
  in
  match Hashtbl.find_opt route_buckets route with
  | Some bucket when bucket.remaining <= 0 && bucket.reset_at > now ->
      Lwt_unix.sleep (bucket.reset_at -. now)
  | _ -> Lwt.return_unit

let update_rate_limit ~route ~headers =
  let remaining =
    try
      Cohttp.Header.get headers "x-ratelimit-remaining"
      |> Option.map int_of_string
    with _ -> None
  in
  let reset_at =
    try
      Cohttp.Header.get headers "x-ratelimit-reset"
      |> Option.map float_of_string
    with _ -> None
  in
  let is_global =
    try
      Cohttp.Header.get headers "x-ratelimit-global"
      |> Option.map (fun s -> String.lowercase_ascii s = "true")
      |> Option.value ~default:false
    with _ -> false
  in
  if is_global then begin
    let retry_after =
      try
        Cohttp.Header.get headers "retry-after"
        |> Option.map float_of_string |> Option.value ~default:1.0
      with _ -> 1.0
    in
    global_rate_limit := Unix.gettimeofday () +. retry_after
  end;
  match (remaining, reset_at) with
  | Some r, Some ra ->
      let bucket =
        match Hashtbl.find_opt route_buckets route with
        | Some b -> b
        | None ->
            let b = { remaining = r; reset_at = ra } in
            Hashtbl.replace route_buckets route b;
            b
      in
      bucket.remaining <- r;
      bucket.reset_at <- ra
  | _ -> ()

let discord_rest_call ~route ~f =
  let open Lwt.Syntax in
  let max_retries = 3 in
  let rec attempt n =
    let* () = wait_for_rate_limit ~route in
    let* status, headers, body = f () in
    update_rate_limit ~route ~headers;
    if status = 429 && n < max_retries then begin
      let retry_after =
        try
          Cohttp.Header.get headers "retry-after"
          |> Option.map float_of_string |> Option.value ~default:1.0
        with _ -> 1.0
      in
      Logs.warn (fun m ->
          m "Discord: rate limited on %s, retrying after %.1fs (attempt %d/%d)"
            route retry_after (n + 1) max_retries);
      let* () = Lwt_unix.sleep retry_after in
      attempt (n + 1)
    end
    else Lwt.return (status, body)
  in
  Lwt_util.with_lock_timeout ~label:"discord_rest" (route_mutex_for route)
    (fun () -> attempt 0)

let send_message_with_id ?(suppress_notifications = false) ~bot_token
    ~channel_id ~text () =
  let open Lwt.Syntax in
  let route = "POST /channels/" ^ channel_id ^ "/messages" in
  let uri = Printf.sprintf "%s/channels/%s/messages" api_base channel_id in
  let headers = [ ("Authorization", "Bot " ^ bot_token) ] in
  let fields =
    [ ("content", `String text) ]
    @ if suppress_notifications then [ ("flags", `Int 4096) ] else []
  in
  let body = `Assoc fields |> Yojson.Safe.to_string in
  let* _status, resp_body =
    discord_rest_call ~route ~f:(fun () ->
        Http_client.post_json_with_headers ~uri ~headers ~body)
  in
  let msg_id =
    try
      let json = Yojson.Safe.from_string resp_body in
      json |> Yojson.Safe.Util.member "id" |> Yojson.Safe.Util.to_string
    with _ -> "0"
  in
  Lwt.return msg_id

let edit_message ~bot_token ~channel_id ~message_id ~text =
  let open Lwt.Syntax in
  let route = "PATCH /channels/" ^ channel_id ^ "/messages/" ^ message_id in
  let uri =
    Printf.sprintf "%s/channels/%s/messages/%s" api_base channel_id message_id
  in
  let headers = [ ("Authorization", "Bot " ^ bot_token) ] in
  let body = `Assoc [ ("content", `String text) ] |> Yojson.Safe.to_string in
  let* _status, _body =
    discord_rest_call ~route ~f:(fun () ->
        let* status, body = Http_client.patch_json ~uri ~headers ~body in
        let empty_headers = Cohttp.Header.init () in
        Lwt.return (status, empty_headers, body))
  in
  Lwt.return_unit

let delete_message ~bot_token ~channel_id ~message_id =
  let open Lwt.Syntax in
  let route = "DELETE /channels/" ^ channel_id ^ "/messages/" ^ message_id in
  let uri =
    Printf.sprintf "%s/channels/%s/messages/%s" api_base channel_id message_id
  in
  let headers = [ ("Authorization", "Bot " ^ bot_token) ] in
  let* _status, _body =
    discord_rest_call ~route ~f:(fun () ->
        let* status, body = Http_client.delete ~uri ~headers ~body:"" in
        let empty_headers = Cohttp.Header.init () in
        Lwt.return (status, empty_headers, body))
  in
  Lwt.return_unit

(* Send a typing indicator to a Discord channel.
   POST /channels/{channel_id}/typing — indicator lasts ~10 seconds. *)
let trigger_typing ~bot_token ~channel_id =
  let open Lwt.Syntax in
  let route = "POST /channels/" ^ channel_id ^ "/typing" in
  let uri = Printf.sprintf "%s/channels/%s/typing" api_base channel_id in
  let headers = [ ("Authorization", "Bot " ^ bot_token) ] in
  let* _status, _body =
    discord_rest_call ~route ~f:(fun () ->
        Http_client.post_json_with_headers ~uri ~headers ~body:"")
  in
  Lwt.return_unit

let make_status_notifier ~bot_token ~channel_id : Status_message.notifier =
  {
    send =
      (fun ?parse_mode:_ text ->
        send_message_with_id ~suppress_notifications:true ~bot_token ~channel_id
          ~text ());
    edit =
      (fun msg_id ?parse_mode:_ text ->
        let open Lwt.Syntax in
        let* () =
          edit_message ~bot_token ~channel_id ~message_id:msg_id ~text
        in
        Lwt.return None);
    delete =
      (fun msg_id -> delete_message ~bot_token ~channel_id ~message_id:msg_id);
  }

let add_reaction ~bot_token ~channel_id ~message_id ~emoji =
  let open Lwt.Syntax in
  let encoded_emoji = Uri.pct_encode emoji in
  let route =
    "PUT /channels/" ^ channel_id ^ "/messages/" ^ message_id ^ "/reactions"
  in
  let uri =
    Printf.sprintf "%s/channels/%s/messages/%s/reactions/%s/@me" api_base
      channel_id message_id encoded_emoji
  in
  let headers = [ ("Authorization", "Bot " ^ bot_token) ] in
  let* _status, _body =
    discord_rest_call ~route ~f:(fun () -> Http_client.put_empty ~uri ~headers)
  in
  Lwt.return_unit

let delete_own_reaction ~bot_token ~channel_id ~message_id ~emoji =
  let open Lwt.Syntax in
  let encoded_emoji = Uri.pct_encode emoji in
  let route =
    "DELETE /channels/" ^ channel_id ^ "/messages/" ^ message_id ^ "/reactions"
  in
  let uri =
    Printf.sprintf "%s/channels/%s/messages/%s/reactions/%s/@me" api_base
      channel_id message_id encoded_emoji
  in
  let headers = [ ("Authorization", "Bot " ^ bot_token) ] in
  let* _status, _body =
    discord_rest_call ~route ~f:(fun () ->
        let* status, body = Http_client.delete ~uri ~headers ~body:"" in
        let empty_headers = Cohttp.Header.init () in
        Lwt.return (status, empty_headers, body))
  in
  Lwt.return_unit

let send_message ~bot_token ~channel_id ~text =
  let open Lwt.Syntax in
  let route = "POST /channels/" ^ channel_id ^ "/messages" in
  let uri = Printf.sprintf "%s/channels/%s/messages" api_base channel_id in
  let headers = [ ("Authorization", "Bot " ^ bot_token) ] in
  let chunks = chunk_text text in
  let* () =
    Lwt_list.iter_s
      (fun chunk ->
        let body =
          `Assoc [ ("content", `String chunk) ] |> Yojson.Safe.to_string
        in
        let* _status, _body =
          discord_rest_call ~route ~f:(fun () ->
              Http_client.post_json_with_headers ~uri ~headers ~body)
        in
        Lwt.return_unit)
      chunks
  in
  Lwt.return_unit

let save_resume_state ~db ~session_id ~seq ~resume_gateway_url =
  match db with
  | None -> ()
  | Some db ->
      let sql =
        "INSERT OR REPLACE INTO discord_resume_state (id, session_id, seq, \
         resume_gateway_url, updated_at) VALUES (1, ?, ?, ?, datetime('now'))"
      in
      let stmt = Sqlite3.prepare db sql in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_id));
          ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int seq)));
          ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT resume_gateway_url));
          match Sqlite3.step stmt with
          | Sqlite3.Rc.DONE -> ()
          | rc ->
              Logs.warn (fun m ->
                  m "Discord: failed to persist resume state: %s"
                    (Sqlite3.Rc.to_string rc)))

let load_resume_state ~db =
  match db with
  | None -> None
  | Some db ->
      let stmt =
        Sqlite3.prepare db
          "SELECT session_id, seq, resume_gateway_url FROM \
           discord_resume_state WHERE id = 1"
      in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          match Sqlite3.step stmt with
          | Sqlite3.Rc.ROW -> (
              match
                ( Sqlite3.column stmt 0,
                  Sqlite3.column stmt 1,
                  Sqlite3.column stmt 2 )
              with
              | ( Sqlite3.Data.TEXT session_id,
                  Sqlite3.Data.INT seq,
                  Sqlite3.Data.TEXT resume_gateway_url ) ->
                  Some
                    { session_id; seq = Int64.to_int seq; resume_gateway_url }
              | _ -> None)
          | _ -> None)

let clear_resume_state ~db =
  match db with
  | None -> ()
  | Some db ->
      let stmt =
        Sqlite3.prepare db "DELETE FROM discord_resume_state WHERE id = 1"
      in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          match Sqlite3.step stmt with
          | Sqlite3.Rc.DONE -> ()
          | rc ->
              Logs.warn (fun m ->
                  m "Discord: failed to clear resume state: %s"
                    (Sqlite3.Rc.to_string rc)))

let persist_resume_refs ~db ~resume_session_id ~resume_seq ~resume_url =
  match (!resume_session_id, !resume_seq, !resume_url) with
  | Some session_id, Some seq, Some resume_gateway_url ->
      save_resume_state ~db ~session_id ~seq ~resume_gateway_url
  | _ -> ()

let clear_resume_refs ~db ~resume_session_id ~resume_seq ~resume_url =
  resume_session_id := None;
  resume_seq := None;
  resume_url := None;
  clear_resume_state ~db

let make_resume_refs ~db =
  match load_resume_state ~db with
  | Some state ->
      ( ref (Some state.session_id),
        ref (Some state.seq),
        ref (Some state.resume_gateway_url) )
  | None -> (ref None, ref None, ref None)

let parse_attachments_json d =
  let open Yojson.Safe.Util in
  try
    d |> member "attachments" |> to_list
    |> List.filter_map (fun att ->
        try
          let att_id = att |> member "id" |> to_string in
          let att_filename = att |> member "filename" |> to_string in
          let att_url = att |> member "url" |> to_string in
          let att_content_type =
            try Some (att |> member "content_type" |> to_string)
            with _ -> None
          in
          let att_size = try att |> member "size" |> to_int with _ -> 0 in
          Some { att_id; att_filename; att_url; att_content_type; att_size }
        with _ -> None)
  with _ -> []

let parse_message_create json =
  let open Yojson.Safe.Util in
  try
    let t = json |> member "t" |> to_string in
    if t <> "MESSAGE_CREATE" then None
    else
      let d = json |> member "d" in
      let channel_id = d |> member "channel_id" |> to_string in
      let guild_id =
        try Some (d |> member "guild_id" |> to_string) with _ -> None
      in
      let author = d |> member "author" in
      let author_id = author |> member "id" |> to_string in
      let author_bot =
        try author |> member "bot" |> to_bool with _ -> false
      in
      let id = d |> member "id" |> to_string in
      let content = d |> member "content" |> to_string in
      let mention_ids =
        try
          d |> member "mentions" |> to_list
          |> List.filter_map (fun m ->
              try Some (m |> member "id" |> to_string) with _ -> None)
        with _ -> []
      in
      let attachments = parse_attachments_json d in
      Some
        {
          id;
          channel_id;
          guild_id;
          author_id;
          author_bot;
          content;
          mention_ids;
          attachments;
        }
  with _ -> None

let parse_dispatch_message d =
  let open Yojson.Safe.Util in
  try
    let id = d |> member "id" |> to_string in
    let channel_id = d |> member "channel_id" |> to_string in
    let guild_id =
      try Some (d |> member "guild_id" |> to_string) with _ -> None
    in
    let author = d |> member "author" in
    let author_id = author |> member "id" |> to_string in
    let author_bot = try author |> member "bot" |> to_bool with _ -> false in
    let content = d |> member "content" |> to_string in
    let mention_ids =
      try
        d |> member "mentions" |> to_list
        |> List.filter_map (fun m ->
            try Some (m |> member "id" |> to_string) with _ -> None)
      with _ -> []
    in
    let attachments = parse_attachments_json d in
    Some
      {
        id;
        channel_id;
        guild_id;
        author_id;
        author_bot;
        content;
        mention_ids;
        attachments;
      }
  with _ -> None

let is_bot_message json =
  let open Yojson.Safe.Util in
  try
    let d = json |> member "d" in
    let author = d |> member "author" in
    author |> member "bot" |> to_bool
  with _ -> false
