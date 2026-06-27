let name = "discord"
let api_base = "https://discord.com/api/v10"

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

let handle_message ~(discord_config : Runtime_config.discord_config)
    ~(session_mgr : Session.t) ?(send_message_fn = send_message)
    ?message_limiter (msg : message) =
  let open Lwt.Syntax in
  if msg.author_bot then Lwt.return_unit
  else if
    not
      (is_allowed ~config:discord_config ~guild_id:msg.guild_id
         ~user_id:msg.author_id)
  then begin
    Logs.warn (fun m ->
        m "Discord: ignoring message from unauthorized user=%s guild=%s"
          msg.author_id
          (Option.value msg.guild_id ~default:"(DM)"));
    Lwt.return_unit
  end
  else if msg.content = "" then Lwt.return_unit
  else
    (* In guild channels, only process if bot was mentioned or addressed *)
    let is_guild = Option.is_some msg.guild_id in
    let bot_mentioned =
      if is_guild then
        match !bot_user_id_ref with
        | Some bid -> List.mem bid msg.mention_ids
        | None -> false
      else false
    in
    if
      not
        (Group_chat_filter.should_respond ~is_group:is_guild ~bot_mentioned
           ~is_reply_to_bot:false ~bot_name:"clawq" msg.content)
    then begin
      Logs.debug (fun m -> m "Discord: ignoring unaddressed guild message");
      let cfg = Session.get_config session_mgr in
      if
        Connector_capabilities.should_capture_history
          ~enabled:cfg.connector_history.enabled Connector_capabilities.discord
      then begin
        let hist_key = Printf.sprintf "discord-hist:%s" msg.channel_id in
        let db =
          if cfg.connector_history.persist_to_db then Session.get_db session_mgr
          else None
        in
        Connector_history.record ?db
          ~persist:cfg.connector_history.persist_to_db ~key:hist_key
          ~channel_type:"discord" ~max:cfg.connector_history.max_messages
          ~sender_name:msg.author_id ~sender_id:msg.author_id ~text:msg.content
          ()
      end;
      Lwt.return_unit
    end
    else
      let limiter_key = msg.channel_id ^ ":" ^ msg.author_id in
      let* rate_ok =
        match message_limiter with
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
          send_message_fn ~bot_token:discord_config.bot_token
            ~channel_id:msg.channel_id
            ~text:
              "Please slow down, I can only process a limited number of \
               messages per minute."
        end
        else Lwt.return_unit
      end
      else
        let key =
          session_key ~channel_id:msg.channel_id ~author_id:msg.author_id
        in
        (* Ensure a typing indicator watcher is running for this session.
         Discord typing indicator lasts ~10s; we refresh every 8s. *)
        let _typing_watcher =
          Typing_indicator.ensure_session_typing_watcher ~session_mgr ~key
            ~send_action:(fun () ->
              trigger_typing ~bot_token:discord_config.bot_token
                ~channel_id:msg.channel_id)
            ~interval:8.0 ~idle_timeout:300.0
        in
        let skill_names =
          List.map
            (fun (s : Skills.skill_md_meta) -> s.md_name)
            (Skills.available_skills ())
        in
        let* cmd_result, msg, skill_injections, _loaded_skill_name =
          match Slash_commands.handle ~skill_names msg.content with
          | Slash_commands.SkillInvoke (name, args) -> (
              if
                args = ""
                && Session.skill_loaded_in_context session_mgr ~key name
              then Lwt.return (Slash_commands.NotACommand, msg, [], None)
              else
                let* result = Skills.expand_slash_skill ~name ~args () in
                match result with
                | Ok r ->
                    Lwt.return
                      ( Slash_commands.NotACommand,
                        msg,
                        [ r.skill_injection ],
                        Some name )
                | Error err_msg ->
                    Lwt.return (Slash_commands.Reply err_msg, msg, [], None))
          | Slash_commands.InjectConnectorHistory count ->
              let cfg = Session.get_config session_mgr in
              let hist_key = Printf.sprintf "discord-hist:%s" msg.channel_id in
              let db =
                if cfg.connector_history.persist_to_db then
                  Session.get_db session_mgr
                else None
              in
              let entries = Connector_history.get ?db ~key:hist_key ~count () in
              if entries = [] then
                Lwt.return
                  ( Slash_commands.Reply
                      "No connector history available. Ensure \
                       connector_history.enabled is true in config. Buffer \
                       captures unaddressed group messages received since \
                       daemon started (or from DB if persist_to_db is on).",
                    msg,
                    [],
                    None )
              else begin
                let context = Connector_history.format_for_context entries in
                let n = List.length entries in
                let* () =
                  send_message_fn ~bot_token:discord_config.bot_token
                    ~channel_id:msg.channel_id
                    ~text:
                      (Printf.sprintf "Last %d chat msgs loaded into context" n)
                in
                let new_msg =
                  {
                    msg with
                    content =
                      Printf.sprintf "[Loaded %d messages from channel history]"
                        n;
                  }
                in
                Lwt.return
                  (Slash_commands.NotACommand, new_msg, [ context ], None)
              end
          | other -> Lwt.return (other, msg, [], None)
        in
        let is_admin =
          match Session.get_db session_mgr with
          | Some db ->
              Admin.is_admin ~db ~channel:"discord" ~sender_id:msg.author_id
          | None -> false
        in
        let user_group = if is_admin then "admin" else "guest" in
        let cmd_result = Slash_commands.gate_admin ~is_admin cmd_result in
        let send_reply text =
          send_message_fn ~bot_token:discord_config.bot_token
            ~channel_id:msg.channel_id ~text
        in
        let env : Connector_dispatch.dispatch_env =
          {
            connector = Format_adapter.Discord;
            connector_name = "discord";
            log_name = "Discord";
            thinking_channel_field = "channel";
            thinking_user_field = "user";
            show_thinking_channel_field = "channel_id";
            show_thinking_user_field = "author_id";
            session_mgr;
            key;
            channel_id = msg.channel_id;
            user_id = msg.author_id;
            is_admin;
            send_plain = send_reply;
            send_formatted = send_reply;
          }
        in
        match cmd_result with
        | AdminRequired _ -> assert false
        | InjectConnectorHistory _ ->
            Lwt.return_unit (* unreachable: preprocessed above *)
        | Compact -> (
            let notifier =
              make_status_notifier ~bot_token:discord_config.bot_token
                ~channel_id:msg.channel_id
            in
            let* compact_result =
              Session.compact session_mgr ~key ~notifier ()
            in
            match compact_result with
            | Ok _ ->
                (* Progress/result message handled by session.compact via notifier *)
                Lwt.return_unit
            | Error err ->
                send_message ~bot_token:discord_config.bot_token
                  ~channel_id:msg.channel_id
                  ~text:(Printf.sprintf "Compaction failed: %s" err))
        | Delegate (agent_name, prompt) ->
            let* () =
              send_message_fn ~bot_token:discord_config.bot_token
                ~channel_id:msg.channel_id
                ~text:"Delegating to a temporary session..."
            in
            let send_agent_reply text =
              send_message_fn ~bot_token:discord_config.bot_token
                ~channel_id:msg.channel_id ~text
            in
            Session.delegate_turn session_mgr ?agent_name ~prompt
              ~parent_key:key ~debug_notify:send_agent_reply
              ~send_reply:send_agent_reply ();
            Lwt.return_unit
        | AgentInvoke (agent_name, prompt) ->
            let* () =
              send_message_fn ~bot_token:discord_config.bot_token
                ~channel_id:msg.channel_id
                ~text:(Printf.sprintf "Invoking agent '%s'..." agent_name)
            in
            let send_agent_reply text =
              send_message_fn ~bot_token:discord_config.bot_token
                ~channel_id:msg.channel_id ~text
            in
            Session.agent_invoke_turn session_mgr ~agent_name ~prompt
              ~parent_key:key ~debug_notify:send_agent_reply
              ~send_reply:send_agent_reply ();
            Lwt.return_unit
        | Rig action -> (
            match action with
            | RigList ->
                let text = Rig.list_text () in
                send_message_fn ~bot_token:discord_config.bot_token
                  ~channel_id:msg.channel_id ~text
            | RigInstall name | RigAdjust name | RigRemove name -> (
                let act =
                  match action with
                  | RigInstall _ -> `Install
                  | RigAdjust _ -> `Adjust
                  | _ -> `Remove
                in
                let act_str =
                  match act with
                  | `Install -> "install"
                  | `Adjust -> "adjust"
                  | `Remove -> "remove"
                in
                match Rig.prompt_for ~name ~action:act with
                | Error err_msg ->
                    send_message_fn ~bot_token:discord_config.bot_token
                      ~channel_id:msg.channel_id ~text:err_msg
                | Ok prompt ->
                    let* () =
                      send_message_fn ~bot_token:discord_config.bot_token
                        ~channel_id:msg.channel_id
                        ~text:
                          (Printf.sprintf "Running rig %s for '%s'..." act_str
                             name)
                    in
                    let send_agent_reply text =
                      send_message_fn ~bot_token:discord_config.bot_token
                        ~channel_id:msg.channel_id ~text
                    in
                    Session.delegate_turn session_mgr ~prompt ~parent_key:key
                      ~debug_notify:send_agent_reply
                      ~send_reply:send_agent_reply ();
                    (match act with
                    | `Install -> (
                        match Rig.find_rig name with
                        | Some rig ->
                            Rig.mark_installed ~name ~version:rig.version
                        | None -> ())
                    | `Remove -> Rig.mark_removed ~name
                    | `Adjust -> ());
                    Lwt.return_unit))
        | Model action -> (
            let open Slash_commands in
            match action with
            | ModelShow ->
                let current =
                  Session.get_session_effective_model session_mgr ~key
                in
                let prefs = Model_preferences.load () in
                let usage_ranked =
                  List.filter_map
                    (fun (m, c) ->
                      if List.mem m prefs.favorites then None else Some (m, c))
                    prefs.usage_counts
                in
                let text =
                  format_model_show ~connector:Format_adapter.Discord ~current
                    ~favorites:prefs.favorites ~usage_ranked
                in
                send_message_fn ~bot_token:discord_config.bot_token
                  ~channel_id:msg.channel_id ~text
            | ModelSet name | ModelSetForce name -> (
                let force =
                  match action with ModelSetForce _ -> true | _ -> false
                in
                let cfg = Session.get_config session_mgr in
                let configured_providers = List.map fst cfg.providers in
                let validation_error =
                  if force then None
                  else
                    Models_catalog.validate_model_name ~configured_providers
                      name
                in
                match validation_error with
                | Some err ->
                    send_message_fn ~bot_token:discord_config.bot_token
                      ~channel_id:msg.channel_id ~text:err
                | None -> (
                    match
                      Model_discovery.validate_cached_model_allowed_opt
                        (Session.get_db session_mgr)
                        name
                    with
                    | Some err ->
                        send_message_fn ~bot_token:discord_config.bot_token
                          ~channel_id:msg.channel_id ~text:err
                    | None ->
                        let provider, model_id, fmt =
                          Models_catalog.split_name name
                        in
                        let hint =
                          match fmt with
                          | Models_catalog.Legacy ->
                              Printf.sprintf
                                "\nHint: use %s:%s format instead of %s/%s."
                                provider model_id provider model_id
                          | _ -> ""
                        in
                        let warn =
                          match fmt with
                          | Models_catalog.Canonical | Models_catalog.Legacy ->
                              let provider_in_config =
                                List.mem_assoc provider cfg.providers
                              in
                              if not provider_in_config then
                                Printf.sprintf
                                  "\n\
                                   Warning: provider '%s' not found in config. \
                                   Add it to your config.json to use this \
                                   model."
                                  provider
                              else ""
                          | Models_catalog.Plain -> ""
                        in
                        Session.set_session_model session_mgr ~key ~model:name;
                        let model_info =
                          Models_catalog.find_by_full_name name
                        in
                        let display =
                          match (fmt, model_info) with
                          | ( (Models_catalog.Canonical | Models_catalog.Legacy),
                              _ ) ->
                              Printf.sprintf
                                "Model set to: %s (provider: %s)%s%s\n\
                                 Persisted for this session across restarts. \
                                 Use /model set-default to change the global \
                                 default."
                                model_id provider hint warn
                          | Models_catalog.Plain, None ->
                              Printf.sprintf
                                "Warning: '%s' not found in model catalog. \
                                 Setting anyway.\n\
                                 Persisted for this session across restarts. \
                                 Use /model set-default to change the global \
                                 default."
                                name
                          | Models_catalog.Plain, Some m ->
                              if m.Models_catalog.provider <> "" then
                                Printf.sprintf
                                  "Model set to: %s (provider: %s)\n\
                                   Persisted for this session across restarts. \
                                   Use /model set-default to change the global \
                                   default."
                                  m.Models_catalog.id m.Models_catalog.provider
                              else
                                Printf.sprintf
                                  "Model set to: %s\n\
                                   Persisted for this session across restarts. \
                                   Use /model set-default to change the global \
                                   default."
                                  name
                        in
                        send_message_fn ~bot_token:discord_config.bot_token
                          ~channel_id:msg.channel_id ~text:display))
            | ModelSetDefault name -> (
                let provider, model_id, fmt = Models_catalog.split_name name in
                let hint =
                  match fmt with
                  | Models_catalog.Legacy ->
                      Printf.sprintf "\nHint: use %s:%s format instead."
                        provider model_id
                  | _ -> ""
                in
                match
                  Model_discovery.validate_cached_model_allowed_opt
                    (Session.get_db session_mgr)
                    name
                with
                | Some err ->
                    send_message_fn ~bot_token:discord_config.bot_token
                      ~channel_id:msg.channel_id ~text:err
                | None -> (
                    let result =
                      Config_set.set_json_value "agent_defaults.primary_model"
                        (`String name)
                    in
                    match result with
                    | Error e ->
                        send_message_fn ~bot_token:discord_config.bot_token
                          ~channel_id:msg.channel_id
                          ~text:(Printf.sprintf "Error writing config: %s" e)
                    | Ok () ->
                        let reply_text =
                          match fmt with
                          | Models_catalog.Canonical | Models_catalog.Legacy ->
                              Printf.sprintf
                                "Default model set to: %s (provider: %s)%s\n\
                                 Applies to new sessions."
                                model_id provider hint
                          | Models_catalog.Plain ->
                              Printf.sprintf
                                "Default model set to: %s\n\
                                 Applies to new sessions."
                                name
                        in
                        send_message_fn ~bot_token:discord_config.bot_token
                          ~channel_id:msg.channel_id ~text:reply_text))
            | ModelFav name ->
                let prefs = Model_preferences.toggle_favorite name in
                let status =
                  if List.mem name prefs.favorites then "added to"
                  else "removed from"
                in
                send_message_fn ~bot_token:discord_config.bot_token
                  ~channel_id:msg.channel_id
                  ~text:(Printf.sprintf "%s %s favorites" name status)
            | ModelUnfav name ->
                let _ = Model_preferences.remove_favorite name in
                send_message_fn ~bot_token:discord_config.bot_token
                  ~channel_id:msg.channel_id
                  ~text:(Printf.sprintf "Removed from favorites: %s" name)
            | ModelList (provider, availability) ->
                let db_extras =
                  match Session.get_db session_mgr with
                  | None -> []
                  | Some db ->
                      Model_discovery.get_db_only_model_infos ~db
                        ~provider_filter:provider ~availability ()
                in
                let models =
                  Models_catalog.to_plain_list ~provider_filter:provider
                    ~availability ~db_extras ()
                  |> String.split_on_char '\n'
                  |> List.filter (fun s -> s <> "")
                in
                let text =
                  format_model_list ~connector:Format_adapter.Discord ~models
                    ~provider
                in
                send_message_fn ~bot_token:discord_config.bot_token
                  ~channel_id:msg.channel_id ~text
            | ModelUsage ->
                let cfg = Session.get_config session_mgr in
                Provider_quota.set_cache_ttl cfg.quota_cache_ttl_s;
                let results =
                  Provider_quota.get_all_cached ()
                  |> List.map (fun (_name, pq) -> pq)
                in
                let text =
                  Slash_commands.format_model_usage
                    ~connector:Format_adapter.Discord ~config:cfg results
                in
                send_message_fn ~bot_token:discord_config.bot_token
                  ~channel_id:msg.channel_id ~text)
        | ForkAnd (agent_name, prompt) ->
            let* () =
              send_message_fn ~bot_token:discord_config.bot_token
                ~channel_id:msg.channel_id ~text:"Forking session..."
            in
            let send_agent_reply text =
              send_message_fn ~bot_token:discord_config.bot_token
                ~channel_id:msg.channel_id ~text
            in
            Session.fork_and_run session_mgr ~parent_key:key ?agent_name ~prompt
              ~debug_notify:send_agent_reply ~send_reply:send_agent_reply ();
            Lwt.return_unit
        | Debate prompt -> (
            match Session.get_db session_mgr with
            | Some db ->
                let config = Session.get_config session_mgr in
                let send_agent_reply text =
                  send_message_fn ~bot_token:discord_config.bot_token
                    ~channel_id:msg.channel_id ~text
                in
                let on_llm_call_debug =
                  Session.debug_callback_for session_mgr ~key
                    (Some send_agent_reply)
                in
                let* text =
                  Debate.run_for_prompt ?on_llm_call_debug ~config ~db ~prompt
                    ()
                in
                send_message_fn ~bot_token:discord_config.bot_token
                  ~channel_id:msg.channel_id ~text
            | None ->
                send_message_fn ~bot_token:discord_config.bot_token
                  ~channel_id:msg.channel_id ~text:"Debate requires a database."
            )
        | BashRun cmd ->
            let config = Session.get_config session_mgr in
            let* result =
              Slash_commands_bash.run_bash_command ~config ~session_key:key cmd
            in
            let full_text = Slash_commands_bash.format_result cmd result in
            let max_len = 1800 in
            let text =
              if String.length full_text <= max_len then full_text
              else String.sub full_text 0 max_len ^ "\n...[truncated]"
            in
            send_message_fn ~bot_token:discord_config.bot_token
              ~channel_id:msg.channel_id ~text
        | DebugDumpChat ->
            let content = Session.dump_json session_mgr ~key in
            let max_len = 1800 in
            let text =
              if String.length content <= max_len then content
              else
                "Session dump (truncated — full dump not yet supported for \
                 this connector):\n"
                ^ String.sub content 0 max_len
                ^ "\n..."
            in
            send_message_fn ~bot_token:discord_config.bot_token
              ~channel_id:msg.channel_id ~text
        | SkillInvoke _ -> Lwt.return_unit (* unreachable: preprocessed above *)
        | NotACommand when Update_tool.is_update_command msg.content -> (
            let send_first text =
              send_message_with_id ~suppress_notifications:true
                ~bot_token:discord_config.bot_token ~channel_id:msg.channel_id
                ~text ()
            in
            let edit_msg msg_id text =
              edit_message ~bot_token:discord_config.bot_token
                ~channel_id:msg.channel_id ~message_id:msg_id ~text
            in
            let send_progress, _get_final =
              Update_tool.make_progress_sender ~send_first ~edit:edit_msg
                ~mode:Update_tool.Auto ()
            in
            let notify text =
              send_message_fn ~bot_token:discord_config.bot_token
                ~channel_id:msg.channel_id ~text
            in
            let* result =
              Session.with_registered_notifier session_mgr ~key ~notify
                (fun () ->
                  Lwt.catch
                    (fun () ->
                      let* _response =
                        Update_tool.run_update
                          ~prepare_restart:(fun () ->
                            Restart_notify.write ~channel:"discord"
                              ~channel_id:msg.channel_id;
                            Lwt.return (Ok ()))
                          ~is_draining:(fun () ->
                            Session.is_draining session_mgr)
                          ~send_progress ()
                      in
                      Lwt.return (Ok ()))
                    (fun exn -> Lwt.return (Error (Printexc.to_string exn))))
            in
            match result with
            | Ok () -> Lwt.return_unit
            | Error err ->
                send_message_fn ~bot_token:discord_config.bot_token
                  ~channel_id:msg.channel_id
                  ~text:
                    (Printf.sprintf
                       "Sorry, an error occurred processing your message: %s"
                       err))
        | NotACommand -> (
            let discord_channel_type =
              match msg.guild_id with Some _ -> "group" | None -> "dm"
            in
            let agent_defaults =
              (Session.get_config session_mgr).agent_defaults
            in
            let use_consolidated =
              agent_defaults.show_tool_calls
              && agent_defaults.tool_status_mode = "consolidated"
            in
            let tool_reaction_set = ref false in
            let peers =
              Reaction_tracker.get_or_create_peers reactions ~key
                ~initial:msg.id
            in
            Reaction_tracker.add_peer reactions ~key ~message_id:msg.id;
            let set_reaction_on_single mid emoji =
              Reaction_tracker.set_reaction_on_single reactions ~message_id:mid
                ~remove_previous:(fun mid prev ->
                  delete_own_reaction ~bot_token:discord_config.bot_token
                    ~channel_id:msg.channel_id ~message_id:mid ~emoji:prev)
                ~add:(fun mid emoji ->
                  add_reaction ~bot_token:discord_config.bot_token
                    ~channel_id:msg.channel_id ~message_id:mid ~emoji)
                ~emoji
            in
            let set_reaction emoji =
              Reaction_tracker.set_reaction_all reactions ~peers_ref:peers
                ~set_one:(fun mid emoji -> set_reaction_on_single mid emoji)
                ~emoji
            in
            let notifier_factory =
              if use_consolidated then
                Some
                  (fun () ->
                    let status_notifier =
                      make_status_notifier ~bot_token:discord_config.bot_token
                        ~channel_id:msg.channel_id
                    in
                    Status_message.create ~notifier:status_notifier
                      ~parse_mode:"Markdown" ())
              else None
            in
            let strategy =
              Status_update.select_strategy ~agent_defaults
                ~capabilities:(Some Connector_capabilities.discord)
            in
            let handler =
              Status_update.make_handler ~strategy ~notifier_factory
                ~notify:(fun text ->
                  send_message_fn ~bot_token:discord_config.bot_token
                    ~channel_id:msg.channel_id ~text)
                ~agent_defaults ~parse_mode:"Markdown"
            in
            let on_chunk chunk =
              (match chunk with
              | Provider.ToolStart _ ->
                  if not !tool_reaction_set then begin
                    tool_reaction_set := true;
                    Lwt.async (fun () ->
                        set_reaction
                          (Connector_status.Discord.phase_emoji Processing))
                  end
              | _ -> ());
              handler.on_chunk chunk
            in
            let* () =
              set_reaction (Connector_status.Discord.phase_emoji Received)
            in
            let drain_progress_msg_id = ref None in
            let on_drain_progress : Session.drain_progress =
              {
                before_turn =
                  (fun queued_msg_id ->
                    let* () =
                      match queued_msg_id with
                      | Some mid ->
                          set_reaction_on_single mid
                            (Connector_status.Discord.phase_emoji Received)
                      | None -> Lwt.return_unit
                    in
                    let* () =
                      match !drain_progress_msg_id with
                      | Some mid ->
                          Lwt.catch
                            (fun () ->
                              delete_message ~bot_token:discord_config.bot_token
                                ~channel_id:msg.channel_id ~message_id:mid)
                            (fun _exn -> Lwt.return_unit)
                      | None -> Lwt.return_unit
                    in
                    let* mid =
                      send_message_with_id ~suppress_notifications:true
                        ~bot_token:discord_config.bot_token
                        ~channel_id:msg.channel_id
                        ~text:
                          "\xe2\x8f\xb3 Processing queued message\xe2\x80\xa6"
                        ()
                    in
                    drain_progress_msg_id := Some mid;
                    Lwt.return_unit);
                after_turn =
                  (fun queued_msg_id ->
                    match queued_msg_id with
                    | Some mid ->
                        set_reaction_on_single mid
                          (Connector_status.Discord.phase_emoji Completed)
                    | None -> Lwt.return_unit);
                after_all =
                  (fun () ->
                    match !drain_progress_msg_id with
                    | Some mid ->
                        drain_progress_msg_id := None;
                        Lwt.catch
                          (fun () ->
                            delete_message ~bot_token:discord_config.bot_token
                              ~channel_id:msg.channel_id ~message_id:mid)
                          (fun _exn -> Lwt.return_unit)
                    | None -> Lwt.return_unit);
              }
            in
            let response_sent = ref false in
            let before_drain response =
              if Session.should_suppress_response response then Lwt.return_unit
              else
                let open Lwt.Syntax in
                let* () = handler.finalize () in
                let thinking = handler.get_thinking () in
                let* () =
                  if thinking <> "" then
                    send_message_fn ~bot_token:discord_config.bot_token
                      ~channel_id:msg.channel_id
                      ~text:("_" ^ thinking ^ "_")
                  else Lwt.return_unit
                in
                let* () =
                  send_message_fn ~bot_token:discord_config.bot_token
                    ~channel_id:msg.channel_id ~text:response
                in
                let* () =
                  set_reaction (Connector_status.Discord.phase_emoji Completed)
                in
                if not (Session.take_response_deferred session_mgr ~key) then
                  Session.mark_response_sent session_mgr ~key;
                response_sent := true;
                Lwt.return_unit
            in
            let* result =
              Session.with_registered_notifier session_mgr ~key
                ~notify:(fun text ->
                  send_message_fn ~bot_token:discord_config.bot_token
                    ~channel_id:msg.channel_id ~text)
                (fun () ->
                  Lwt.catch
                    (fun () ->
                      let config = Session.get_config session_mgr in
                      (* Partition audio attachments for transcription *)
                      let is_audio_att (a : discord_attachment) =
                        match a.att_content_type with
                        | Some ct -> Voice_transcription.is_audio_mime ct
                        | None ->
                            Voice_transcription.is_audio_filename a.att_filename
                      in
                      let audio_atts, non_audio_atts =
                        List.partition is_audio_att msg.attachments
                      in
                      let* transcription_prefix =
                        if
                          audio_atts <> []
                          && config.security.attachment_downloads_enabled
                        then
                          let* texts =
                            Lwt_list.map_s
                              (fun (a : discord_attachment) ->
                                match
                                  Voice_transcription.validate ~config
                                    ~filename:a.att_filename
                                    ~mime_type:a.att_content_type
                                    ~size:(Some a.att_size)
                                    ~duration_seconds:None
                                with
                                | Error reason ->
                                    Logs.info (fun m ->
                                        m "Discord voice skipped %s: %s"
                                          a.att_filename
                                          (Voice_transcription
                                           .skip_reason_to_string reason));
                                    Lwt.return ""
                                | Ok () ->
                                    Lwt.catch
                                      (fun () ->
                                        let* _status, audio_data =
                                          Http_client.get ~uri:a.att_url
                                            ~headers:[]
                                        in
                                        let notifier =
                                          make_status_notifier
                                            ~bot_token:discord_config.bot_token
                                            ~channel_id:msg.channel_id
                                        in
                                        Voice_transcription
                                        .transcribe_with_progress ~config
                                          ~notifier ~audio_data
                                          ~filename:a.att_filename ())
                                      (fun exn ->
                                        Logs.err (fun m ->
                                            m
                                              "Discord voice transcription \
                                               failed %s: %s"
                                              a.att_filename
                                              (Printexc.to_string exn));
                                        Lwt.return ""))
                              audio_atts
                          in
                          Lwt.return
                            (String.concat ""
                               (List.filter (fun s -> s <> "") texts))
                        else Lwt.return ""
                      in
                      let effective_content =
                        if transcription_prefix <> "" then
                          transcription_prefix ^ "\n" ^ msg.content
                        else msg.content
                      in
                      let* content_parts, att_list, message =
                        if
                          non_audio_atts <> []
                          && config.security.attachment_downloads_enabled
                        then
                          let workspace =
                            Runtime_config.effective_workspace config
                          in
                          let metas =
                            List.map
                              (fun (a : discord_attachment) ->
                                Attachment_download.
                                  {
                                    url = a.att_url;
                                    filename = a.att_filename;
                                    mime_type = a.att_content_type;
                                    size = Some a.att_size;
                                  })
                              non_audio_atts
                          in
                          Attachment_download.process_attachments metas
                            ~headers:[] ~workspace
                            ~db:(Session.get_db session_mgr)
                            ~session_key:key ~source:"discord" ~content_parts:[]
                            ~attachments:[] ~message:effective_content
                        else
                          let placeholder =
                            if non_audio_atts <> [] then
                              let names =
                                List.map
                                  (fun (a : discord_attachment) ->
                                    Printf.sprintf
                                      "\n[Attachment: %s (download disabled)]"
                                      a.att_filename)
                                  non_audio_atts
                              in
                              effective_content ^ String.concat "" names
                            else effective_content
                          in
                          Lwt.return ([], [], placeholder)
                      in
                      let* response =
                        Session.turn_stream session_mgr ~key ~message
                          ~content_parts ~attachments:att_list ~skill_injections
                          ~channel_name:msg.channel_id
                          ~channel_type:discord_channel_type
                          ~sender_id:msg.author_id ~user_group
                          ~channel:"discord" ~channel_id:msg.channel_id
                          ~message_id:msg.id ~on_drain_progress ~before_drain
                          ~on_chunk ()
                      in
                      Lwt.return (Ok response))
                    (fun exn -> Lwt.return (Error (Printexc.to_string exn))))
            in
            match result with
            | Ok response ->
                if Session.should_suppress_response response then
                  Lwt.return_unit
                else if !response_sent then (
                  let* () =
                    Reaction_tracker.cleanup_with_remove reactions ~key
                      ~remove:(fun mid emoji ->
                        delete_own_reaction ~bot_token:discord_config.bot_token
                          ~channel_id:msg.channel_id ~message_id:mid ~emoji)
                  in
                  let send_to_channel text =
                    send_message_fn ~bot_token:discord_config.bot_token
                      ~channel_id:msg.channel_id ~text
                  in
                  if
                    Option.is_none
                      (Session.find_registered_notifier session_mgr ~key)
                  then begin
                    Session.register_channel_notifier session_mgr ~key
                      send_to_channel;
                    Session.register_status_message_factory session_mgr ~key
                      (fun () ->
                        let notifier =
                          make_status_notifier
                            ~bot_token:discord_config.bot_token
                            ~channel_id:msg.channel_id
                        in
                        Status_message.create ~notifier ~parse_mode:"Markdown"
                          ());
                    Session.register_connector_capabilities session_mgr ~key
                      Connector_capabilities.discord
                  end;
                  Lwt.async (fun () ->
                      Session.process_autonomous_turn_result
                        ~on_response:send_to_channel session_mgr ~key ~response);
                  Lwt.return_unit)
                else
                  let* () = handler.finalize () in
                  let thinking = handler.get_thinking () in
                  let* () =
                    if thinking <> "" then
                      send_message_fn ~bot_token:discord_config.bot_token
                        ~channel_id:msg.channel_id
                        ~text:("_" ^ thinking ^ "_")
                    else Lwt.return_unit
                  in
                  let* () =
                    send_message_fn ~bot_token:discord_config.bot_token
                      ~channel_id:msg.channel_id ~text:response
                  in
                  let* () =
                    set_reaction
                      (Connector_status.Discord.phase_emoji Completed)
                  in
                  let* () =
                    Reaction_tracker.cleanup_with_remove reactions ~key
                      ~remove:(fun mid emoji ->
                        delete_own_reaction ~bot_token:discord_config.bot_token
                          ~channel_id:msg.channel_id ~message_id:mid ~emoji)
                  in
                  if not (Session.take_response_deferred session_mgr ~key) then
                    Session.mark_response_sent session_mgr ~key;
                  let send_to_channel text =
                    send_message_fn ~bot_token:discord_config.bot_token
                      ~channel_id:msg.channel_id ~text
                  in
                  if
                    Option.is_none
                      (Session.find_registered_notifier session_mgr ~key)
                  then begin
                    Session.register_channel_notifier session_mgr ~key
                      send_to_channel;
                    Session.register_status_message_factory session_mgr ~key
                      (fun () ->
                        let notifier =
                          make_status_notifier
                            ~bot_token:discord_config.bot_token
                            ~channel_id:msg.channel_id
                        in
                        Status_message.create ~notifier ~parse_mode:"Markdown"
                          ());
                    Session.register_connector_capabilities session_mgr ~key
                      Connector_capabilities.discord
                  end;
                  Lwt.async (fun () ->
                      Session.process_autonomous_turn_result
                        ~on_response:send_to_channel session_mgr ~key ~response);
                  Lwt.return_unit
            | Error err ->
                Logs.err (fun m ->
                    m "Discord agent error for channel=%s user=%s: %s"
                      msg.channel_id msg.author_id err);
                let* () = handler.finalize () in
                let* () =
                  send_message_fn ~bot_token:discord_config.bot_token
                    ~channel_id:msg.channel_id
                    ~text:
                      (Printf.sprintf
                         "Sorry, an error occurred processing your message: %s"
                         err)
                in
                let* () =
                  set_reaction (Connector_status.Discord.phase_emoji Failed)
                in
                let* () =
                  Reaction_tracker.cleanup_with_remove reactions ~key
                    ~remove:(fun mid emoji ->
                      delete_own_reaction ~bot_token:discord_config.bot_token
                        ~channel_id:msg.channel_id ~message_id:mid ~emoji)
                in
                if not (Session.take_response_deferred session_mgr ~key) then
                  Session.mark_response_sent session_mgr ~key;
                Lwt.return_unit)
        | ( RegisterAsAdminOtc _ | Reply _ | FormattedReply _ | Help | Menu _
          | Reset | RuntimeCtx | Uptime | Status | Thinking _ | ShowThinking _
          | Heartbeat _ | Debug _ | AgentMenu _ | ModelMenu _ | ThinkingMenu
          | ConfigMenu _ | SkillsMenu _ | CostsMenu | BgMenu | Tools | Tasks
          | TasksFull | Costs _ | Session _ | Usage _ | Active | Bg _ | Cron _
          | Bl _ | HeldItems _ | Memories _ | Repo _ ) as r ->
            Connector_dispatch.dispatch env r

(* Close code classification for reconnect behavior *)
let is_fatal_close_code code =
  match code with 4004 | 4010 | 4011 | 4012 | 4013 | 4014 -> true | _ -> false

let should_clear_resume_state code =
  match code with
  | 4004 | 4007 | 4009 | 4010 | 4011 | 4012 | 4013 | 4014 -> true
  | _ -> false

let start ~config ~session_manager ~db ~(message_limiter : Rate_limiter.t) =
  match config.Runtime_config.channels.discord with
  | None ->
      Logs.info (fun m -> m "No Discord config found, skipping");
      Lwt.return_unit
  | Some discord_config ->
      if discord_config.bot_token = "" then begin
        Logs.info (fun m -> m "Discord bot_token is empty, skipping");
        Lwt.return_unit
      end
      else begin
        Logs.info (fun m ->
            m "Discord channel starting (WebSocket gateway mode)");
        let open Lwt.Syntax in
        let resume_session_id, resume_seq, resume_url = make_resume_refs ~db in
        (match !resume_seq with
        | Some seq ->
            Logs.info (fun m ->
                m "Discord: loaded persisted resume state (seq=%d)" seq)
        | None -> ());
        let backoff = Channel_util.Backoff.create () in
        let rec connect_loop () =
          let close_p, close_u = Lwt.wait () in
          let on_dispatch event_name d =
            if event_name = "READY" then begin
              let open Yojson.Safe.Util in
              (bot_user_id_ref :=
                 try Some (d |> member "user" |> member "id" |> to_string)
                 with _ -> None);
              Lwt.return_unit
            end
            else if event_name = "MESSAGE_CREATE" then (
              match parse_dispatch_message d with
              | Some msg ->
                  handle_message ~discord_config ~session_mgr:session_manager
                    ~message_limiter msg
              | None ->
                  Logs.debug (fun m ->
                      m "Discord: dropping malformed MESSAGE_CREATE dispatch");
                  Lwt.return_unit)
            else Lwt.return_unit
          in
          let on_close code =
            if Lwt.is_sleeping close_p then Lwt.wakeup_later close_u code;
            Lwt.return_unit
          in
          let result =
            Lwt.catch
              (fun () ->
                let* gw =
                  Discord_gateway.connect ~bot_token:discord_config.bot_token
                    ~intents:discord_config.intents
                    ?resume_session_id:!resume_session_id
                    ?resume_seq:!resume_seq ?resume_url:!resume_url ~on_dispatch
                    ~on_close ()
                in
                Channel_util.Backoff.reset backoff;
                let* code = close_p in
                resume_session_id := Discord_gateway.session_id gw;
                resume_seq := Discord_gateway.last_seq gw;
                resume_url := Discord_gateway.resume_url gw;
                persist_resume_refs ~db ~resume_session_id ~resume_seq
                  ~resume_url;
                Lwt.return (Ok code))
              (fun exn -> Lwt.return (Error (Printexc.to_string exn)))
          in
          let* outcome = result in
          match outcome with
          | Error err ->
              Logs.err (fun m -> m "Discord: gateway connection error: %s" err);
              Logs.info (fun m ->
                  m "Discord: reconnecting in %.0fs"
                    (Channel_util.Backoff.current backoff));
              let* () = Channel_util.Backoff.sleep_and_increase backoff in
              connect_loop ()
          | Ok code ->
              let code_int = match code with Some c -> c | None -> 0 in
              if should_clear_resume_state code_int then
                clear_resume_refs ~db ~resume_session_id ~resume_seq ~resume_url;
              if is_fatal_close_code code_int then begin
                Logs.err (fun m ->
                    m "Discord: fatal close code %d, not reconnecting" code_int);
                Lwt.return_unit
              end
              else begin
                Logs.info (fun m ->
                    m "Discord: connection closed (code=%d), reconnecting"
                      code_int);
                let* () =
                  if !resume_session_id <> None then begin
                    Channel_util.Backoff.reset backoff;
                    Lwt_unix.sleep 1.0
                  end
                  else Channel_util.Backoff.sleep_and_increase backoff
                in
                connect_loop ()
              end
        in
        connect_loop ()
      end
