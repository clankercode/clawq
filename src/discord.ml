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

let set_thinking_level ~(session_mgr : Session.t) ~channel_id ~author_id level =
  let cfg = Session.get_config session_mgr in
  let previous = cfg.agent_defaults.reasoning_effort in
  match Config_set.set_reasoning_effort level with
  | Ok () ->
      let agent_defaults =
        { cfg.agent_defaults with reasoning_effort = level }
      in
      Session.update_config ~source:"discord" session_mgr
        { cfg with agent_defaults };
      Logs.info (fun m ->
          m "Discord thinking level updated channel=%s user=%s from=%s to=%s"
            channel_id author_id
            (Slash_commands.thinking_level_to_string previous)
            (Slash_commands.thinking_level_to_string level));
      Printf.sprintf "Thinking level changed from %s to %s."
        (Slash_commands.thinking_level_to_string previous)
        (Slash_commands.thinking_level_to_string level)
  | Error err ->
      Logs.err (fun m ->
          m "Discord thinking level update failed channel=%s user=%s: %s"
            channel_id author_id err);
      "Failed to update thinking level: " ^ err

type message = {
  id : string;
  channel_id : string;
  guild_id : string option;
  author_id : string;
  author_bot : bool;
  content : string;
  mention_ids : string list;
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
let route_mutex = Lwt_mutex.create ()
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
  Lwt_mutex.with_lock route_mutex (fun () ->
      let* () = wait_for_rate_limit ~route in
      let* status, headers, body = f () in
      update_rate_limit ~route ~headers;
      if status = 429 then begin
        let retry_after =
          try
            Cohttp.Header.get headers "retry-after"
            |> Option.map float_of_string |> Option.value ~default:1.0
          with _ -> 1.0
        in
        Logs.warn (fun m ->
            m "Discord: rate limited on %s, retrying after %.1fs" route
              retry_after);
        let* () = Lwt_unix.sleep retry_after in
        let* () = wait_for_rate_limit ~route in
        let* status2, headers2, body2 = f () in
        update_rate_limit ~route ~headers:headers2;
        Lwt.return (status2, body2)
      end
      else Lwt.return (status, body))

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
      Some
        {
          id;
          channel_id;
          guild_id;
          author_id;
          author_bot;
          content;
          mention_ids;
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
    Some
      { id; channel_id; guild_id; author_id; author_bot; content; mention_ids }
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
        let* cmd_result, msg, skill_injections, loaded_skill_name =
          match Slash_commands.handle ~skill_names msg.content with
          | Slash_commands.SkillInvoke (name, args) -> (
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
          | other -> Lwt.return (other, msg, [], None)
        in
        let* () =
          match loaded_skill_name with
          | Some name ->
              send_message_fn ~bot_token:discord_config.bot_token
                ~channel_id:msg.channel_id
                ~text:(Printf.sprintf "Loaded skill: %s" name)
          | None -> Lwt.return_unit
        in
        match cmd_result with
        | Reply text ->
            send_message_fn ~bot_token:discord_config.bot_token
              ~channel_id:msg.channel_id ~text
        | FormattedReply fn ->
            let text = fn Format_adapter.Discord in
            send_message_fn ~bot_token:discord_config.bot_token
              ~channel_id:msg.channel_id ~text
        | Help | Menu _ ->
            let text =
              Slash_commands.format_help ~connector:Format_adapter.Discord
            in
            send_message_fn ~bot_token:discord_config.bot_token
              ~channel_id:msg.channel_id ~text
        | Reset ->
            let* active_bg_tasks = Session.reset session_mgr ~key in
            let text =
              Slash_commands_fmt.format_reset ~connector:Format_adapter.Discord
                ~active_bg_tasks
            in
            send_message_fn ~bot_token:discord_config.bot_token
              ~channel_id:msg.channel_id ~text
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
        | RuntimeCtx ->
            let* text = Session.runtime_context_block session_mgr ~key in
            send_message_fn ~bot_token:discord_config.bot_token
              ~channel_id:msg.channel_id ~text
        | Uptime ->
            let raw =
              Daemon_status.daemon_uptime_reply
                ~pid:(Daemon_status.read_current_daemon_pid ())
            in
            let text =
              Slash_commands_fmt.format_uptime ~connector:Format_adapter.Discord
                raw
            in
            send_message_fn ~bot_token:discord_config.bot_token
              ~channel_id:msg.channel_id ~text
        | Status ->
            let text =
              Slash_commands.format_status ~connector:Format_adapter.Discord
                ~db:(Session.get_db session_mgr)
                ~session_count:(Session.session_count session_mgr)
                ~active_count:(Session.active_session_count session_mgr)
                ()
            in
            send_message_fn ~bot_token:discord_config.bot_token
              ~channel_id:msg.channel_id ~text
        | Thinking Slash_commands.ShowThinking ->
            let current =
              (Session.get_config session_mgr).agent_defaults.reasoning_effort
            in
            let text =
              Slash_commands_fmt.format_thinking_status
                ~connector:Format_adapter.Discord current
            in
            send_message_fn ~bot_token:discord_config.bot_token
              ~channel_id:msg.channel_id ~text
        | Thinking (Slash_commands.SetThinking level) ->
            let text =
              set_thinking_level ~session_mgr ~channel_id:msg.channel_id
                ~author_id:msg.author_id level
            in
            send_message_fn ~bot_token:discord_config.bot_token
              ~channel_id:msg.channel_id ~text
        | ShowThinking action ->
            let connector = Format_adapter.Discord in
            let cfg = Session.get_config session_mgr in
            let current = cfg.agent_defaults.show_thinking in
            let text =
              match action with
              | Slash_commands.ShowThinkingStatus ->
                  Slash_commands_fmt.format_show_thinking_status ~connector
                    current
              | Slash_commands.ToggleShowThinking -> (
                  let new_val = not current in
                  match Config_set.set_show_thinking new_val with
                  | Ok () ->
                      let agent_defaults =
                        { cfg.agent_defaults with show_thinking = new_val }
                      in
                      Session.update_config ~source:"discord" session_mgr
                        { cfg with agent_defaults };
                      Logs.info (fun m ->
                          m
                            "Discord show_thinking toggled channel_id=%s \
                             author_id=%s from=%b to=%b"
                            msg.channel_id msg.author_id current new_val);
                      Slash_commands_fmt.format_show_thinking_toggle ~connector
                        new_val
                  | Error err -> "Failed to update show_thinking: " ^ err)
            in
            send_message_fn ~bot_token:discord_config.bot_token
              ~channel_id:msg.channel_id ~text
        | Heartbeat action ->
            let connector = Format_adapter.Discord in
            let text =
              match action with
              | Slash_commands.HeartbeatStatus ->
                  Slash_commands_fmt.format_heartbeat_status ~connector
                    (Session.session_heartbeat_status_text session_mgr ~key)
              | Slash_commands.SetHeartbeat enabled -> (
                  match
                    Session.set_session_heartbeat session_mgr ~key ~enabled
                  with
                  | Ok () ->
                      Slash_commands_fmt.format_heartbeat_set ~connector enabled
                        key
                  | Error err -> err)
            in
            send_message_fn ~bot_token:discord_config.bot_token
              ~channel_id:msg.channel_id ~text
        | Delegate (agent_name, prompt) ->
            let* () =
              send_message_fn ~bot_token:discord_config.bot_token
                ~channel_id:msg.channel_id
                ~text:"Delegating to a temporary session..."
            in
            Session.delegate_turn session_mgr ?agent_name ~prompt
              ~send_reply:(fun text ->
                send_message_fn ~bot_token:discord_config.bot_token
                  ~channel_id:msg.channel_id ~text)
              ();
            Lwt.return_unit
        | AgentInvoke (agent_name, prompt) ->
            let* () =
              send_message_fn ~bot_token:discord_config.bot_token
                ~channel_id:msg.channel_id
                ~text:(Printf.sprintf "Invoking agent '%s'..." agent_name)
            in
            Session.agent_invoke_turn session_mgr ~agent_name ~prompt
              ~send_reply:(fun text ->
                send_message_fn ~bot_token:discord_config.bot_token
                  ~channel_id:msg.channel_id ~text);
            Lwt.return_unit
        | AgentMenu page ->
            let text =
              Slash_commands_fmt.format_agent_menu
                ~connector:Format_adapter.Discord ~page
            in
            let* () =
              send_message_fn ~bot_token:discord_config.bot_token
                ~channel_id:msg.channel_id ~text
            in
            Lwt.return_unit
        | ModelMenu page ->
            let text =
              Slash_commands_fmt.format_model_menu
                ~connector:Format_adapter.Discord ~page
            in
            send_message_fn ~bot_token:discord_config.bot_token
              ~channel_id:msg.channel_id ~text
        | ThinkingMenu ->
            let text =
              Slash_commands_fmt.format_thinking_menu
                ~connector:Format_adapter.Discord
            in
            send_message_fn ~bot_token:discord_config.bot_token
              ~channel_id:msg.channel_id ~text
        | ConfigMenu page ->
            let text =
              Slash_commands_fmt.format_config_menu
                ~connector:Format_adapter.Discord ~page
            in
            send_message_fn ~bot_token:discord_config.bot_token
              ~channel_id:msg.channel_id ~text
        | SkillsMenu page ->
            let text =
              Slash_commands_fmt.format_skills_menu
                ~connector:Format_adapter.Discord ~page
            in
            send_message_fn ~bot_token:discord_config.bot_token
              ~channel_id:msg.channel_id ~text
        | CostsMenu ->
            let text =
              Slash_commands_fmt.format_costs_menu
                ~connector:Format_adapter.Discord
            in
            send_message_fn ~bot_token:discord_config.bot_token
              ~channel_id:msg.channel_id ~text
        | BgMenu ->
            let text =
              Slash_commands_fmt.format_bg_menu
                ~connector:Format_adapter.Discord
            in
            send_message_fn ~bot_token:discord_config.bot_token
              ~channel_id:msg.channel_id ~text
        | Tools ->
            let text =
              match Session.get_tool_registry session_mgr with
              | Some reg ->
                  let tools, _ = Tool_registry.partition_skills reg in
                  let skills = Skills.available_skills_as_tools () in
                  Slash_commands.format_tools ~connector:Format_adapter.Discord
                    tools skills
              | None -> "Tools are not enabled."
            in
            send_message_fn ~bot_token:discord_config.bot_token
              ~channel_id:msg.channel_id ~text
        | Tasks ->
            let raw =
              match Session.get_db session_mgr with
              | Some db ->
                  Task_tree.init_schema db;
                  Task_tree.render_emoji_tree ~db ~session_key:key ()
              | None -> "Tasks are not available (no database)."
            in
            let text =
              Slash_commands_fmt.format_tasks ~connector:Format_adapter.Discord
                raw
            in
            send_message_fn ~bot_token:discord_config.bot_token
              ~channel_id:msg.channel_id ~text
        | TasksFull ->
            let raw =
              match Session.get_db session_mgr with
              | Some db ->
                  Task_tree.init_schema db;
                  Task_tree.render_tree_with_legend ~db ~session_key:key
              | None -> "Tasks are not available (no database)."
            in
            let text =
              Slash_commands_fmt.format_tasks ~connector:Format_adapter.Discord
                raw
            in
            send_message_fn ~bot_token:discord_config.bot_token
              ~channel_id:msg.channel_id ~text
        | Costs action ->
            let text =
              match Session.get_db session_mgr with
              | Some db ->
                  Slash_commands.format_costs ~connector:Format_adapter.Discord
                    ~db action
              | None -> "Costs are not available (no database)."
            in
            send_message_fn ~bot_token:discord_config.bot_token
              ~channel_id:msg.channel_id ~text
        | Usage action ->
            let text =
              match Session.get_db session_mgr with
              | Some db ->
                  Slash_commands.format_usage ~connector:Format_adapter.Discord
                    ~db action
              | None -> "Usage is not available (no database)."
            in
            send_message_fn ~bot_token:discord_config.bot_token
              ~channel_id:msg.channel_id ~text
        | Active ->
            let text =
              match Session.get_db session_mgr with
              | Some db ->
                  let config = Session.get_config session_mgr in
                  Slash_commands.format_active ~connector:Format_adapter.Discord
                    ~db ~config ()
              | None -> "Active usage is not available (no database)."
            in
            send_message_fn ~bot_token:discord_config.bot_token
              ~channel_id:msg.channel_id ~text
        | Bg action ->
            let text =
              match Session.get_db session_mgr with
              | Some db ->
                  Slash_commands.format_bg ~connector:Format_adapter.Discord ~db
                    action
              | None -> "Background tasks are not available (no database)."
            in
            send_message_fn ~bot_token:discord_config.bot_token
              ~channel_id:msg.channel_id ~text
        | Cron action ->
            let text =
              match Session.get_db session_mgr with
              | Some db ->
                  Slash_commands.format_cron ~connector:Format_adapter.Discord
                    ~db ~session_key:key action
              | None -> "Cron is not available (no database)."
            in
            send_message_fn ~bot_token:discord_config.bot_token
              ~channel_id:msg.channel_id ~text
        | Bl action ->
            let text =
              Slash_commands.format_bl ~connector:Format_adapter.Discord action
            in
            send_message_fn ~bot_token:discord_config.bot_token
              ~channel_id:msg.channel_id ~text
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
            | ModelSet name -> (
                let provider, model_id, fmt = Models_catalog.split_name name in
                match fmt with
                | Models_catalog.Canonical | Models_catalog.Legacy ->
                    let hint =
                      match fmt with
                      | Models_catalog.Legacy ->
                          Printf.sprintf
                            "\nHint: use %s:%s format instead of %s/%s."
                            provider model_id provider model_id
                      | _ -> ""
                    in
                    let cfg = Session.get_config session_mgr in
                    let provider_in_config =
                      List.mem_assoc provider cfg.providers
                    in
                    let warn =
                      if not provider_in_config then
                        Printf.sprintf
                          "\n\
                           Warning: provider '%s' not found in config. Add it \
                           to your config.json to use this model."
                          provider
                      else ""
                    in
                    Session.set_session_model session_mgr ~key ~model:name;
                    send_message_fn ~bot_token:discord_config.bot_token
                      ~channel_id:msg.channel_id
                      ~text:
                        (Printf.sprintf
                           "Model set to: %s (provider: %s)%s%s\n\
                            Persisted for this session across restarts. Use \
                            /model set-default to change the global default."
                           model_id provider hint warn)
                | Models_catalog.Plain -> (
                    let model_info = Models_catalog.find_by_full_name name in
                    match model_info with
                    | None ->
                        let text =
                          Printf.sprintf
                            "Warning: '%s' not found in model catalog. Setting \
                             anyway.\n\
                             Persisted for this session across restarts. Use \
                             /model set-default to change the global default."
                            name
                        in
                        Session.set_session_model session_mgr ~key ~model:name;
                        send_message_fn ~bot_token:discord_config.bot_token
                          ~channel_id:msg.channel_id ~text
                    | Some m ->
                        Session.set_session_model session_mgr ~key ~model:name;
                        let display =
                          if m.Models_catalog.provider <> "" then
                            Printf.sprintf
                              "Model set to: %s (provider: %s)\n\
                               Persisted for this session across restarts. Use \
                               /model set-default to change the global \
                               default."
                              m.Models_catalog.id m.Models_catalog.provider
                          else
                            Printf.sprintf
                              "Model set to: %s\n\
                               Persisted for this session across restarts. Use \
                               /model set-default to change the global \
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
                            "Default model set to: %s\nApplies to new sessions."
                            name
                    in
                    send_message_fn ~bot_token:discord_config.bot_token
                      ~channel_id:msg.channel_id ~text:reply_text)
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
            | ModelList provider ->
                let db_extras =
                  match Session.get_db session_mgr with
                  | None -> []
                  | Some db ->
                      Model_discovery.get_db_only_models ~db
                        ~provider_filter:provider
                in
                let models =
                  Models_catalog.to_plain_list ~provider_filter:provider
                    ~db_extras ()
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
            Session.fork_and_run session_mgr ~parent_key:key ?agent_name ~prompt
              ~send_reply:(fun text ->
                send_message_fn ~bot_token:discord_config.bot_token
                  ~channel_id:msg.channel_id ~text)
              ();
            Lwt.return_unit
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
            let available_agents =
              List.map
                (fun (t : Agent_template.t) -> t.name)
                (Agent_template.available_templates ())
            in
            match
              Group_chat_filter.parse_agent_mention ~available_agents
                msg.content
            with
            | Some (agent_name, prompt) when prompt <> "" ->
                let* () =
                  send_message_fn ~bot_token:discord_config.bot_token
                    ~channel_id:msg.channel_id
                    ~text:(Printf.sprintf "Invoking agent '%s'..." agent_name)
                in
                Session.agent_invoke_turn session_mgr ~agent_name ~prompt
                  ~send_reply:(fun text ->
                    send_message_fn ~bot_token:discord_config.bot_token
                      ~channel_id:msg.channel_id ~text);
                Lwt.return_unit
            | Some (agent_name, _) ->
                send_message_fn ~bot_token:discord_config.bot_token
                  ~channel_id:msg.channel_id
                  ~text:
                    (Printf.sprintf
                       "Usage: @%s <prompt> — provide a prompt for the agent."
                       agent_name)
            | None -> (
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
                  Reaction_tracker.set_reaction_on_single reactions
                    ~message_id:mid
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
                          make_status_notifier
                            ~bot_token:discord_config.bot_token
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
                                  delete_message
                                    ~bot_token:discord_config.bot_token
                                    ~channel_id:msg.channel_id ~message_id:mid)
                                (fun _exn -> Lwt.return_unit)
                          | None -> Lwt.return_unit
                        in
                        let* mid =
                          send_message_with_id ~suppress_notifications:true
                            ~bot_token:discord_config.bot_token
                            ~channel_id:msg.channel_id
                            ~text:
                              "\xe2\x8f\xb3 Processing queued \
                               message\xe2\x80\xa6"
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
                                delete_message
                                  ~bot_token:discord_config.bot_token
                                  ~channel_id:msg.channel_id ~message_id:mid)
                              (fun _exn -> Lwt.return_unit)
                        | None -> Lwt.return_unit);
                  }
                in
                let response_sent = ref false in
                let before_drain response =
                  if Session.should_suppress_response response then
                    Lwt.return_unit
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
                      set_reaction
                        (Connector_status.Discord.phase_emoji Completed)
                    in
                    if not (Session.take_response_deferred session_mgr ~key)
                    then Session.mark_response_sent session_mgr ~key;
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
                          let* response =
                            Session.turn_stream session_mgr ~key
                              ~message:msg.content ~skill_injections
                              ~channel_name:msg.channel_id
                              ~channel_type:discord_channel_type
                              ~sender_id:msg.author_id ~channel:"discord"
                              ~channel_id:msg.channel_id ~message_id:msg.id
                              ~on_drain_progress ~before_drain ~on_chunk ()
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
                            delete_own_reaction
                              ~bot_token:discord_config.bot_token
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
                            Status_message.create ~notifier
                              ~parse_mode:"Markdown" ());
                        Session.register_connector_capabilities session_mgr ~key
                          Connector_capabilities.discord
                      end;
                      Lwt.async (fun () ->
                          Session.process_autonomous_turn_result
                            ~on_response:send_to_channel session_mgr ~key
                            ~response);
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
                            delete_own_reaction
                              ~bot_token:discord_config.bot_token
                              ~channel_id:msg.channel_id ~message_id:mid ~emoji)
                      in
                      if not (Session.take_response_deferred session_mgr ~key)
                      then Session.mark_response_sent session_mgr ~key;
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
                            Status_message.create ~notifier
                              ~parse_mode:"Markdown" ());
                        Session.register_connector_capabilities session_mgr ~key
                          Connector_capabilities.discord
                      end;
                      Lwt.async (fun () ->
                          Session.process_autonomous_turn_result
                            ~on_response:send_to_channel session_mgr ~key
                            ~response);
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
                             "Sorry, an error occurred processing your \
                              message: %s"
                             err)
                    in
                    let* () =
                      set_reaction (Connector_status.Discord.phase_emoji Failed)
                    in
                    let* () =
                      Reaction_tracker.cleanup_with_remove reactions ~key
                        ~remove:(fun mid emoji ->
                          delete_own_reaction
                            ~bot_token:discord_config.bot_token
                            ~channel_id:msg.channel_id ~message_id:mid ~emoji)
                    in
                    if not (Session.take_response_deferred session_mgr ~key)
                    then Session.mark_response_sent session_mgr ~key;
                    Lwt.return_unit))

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
