let write_state ~pairing_code ~(config : Runtime_config.t) ~components =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  let state_dir = Filename.concat home ".clawq" in
  let state_path = Filename.concat state_dir "daemon_state.json" in
  (try if not (Sys.file_exists state_dir) then Sys.mkdir state_dir 0o755
   with _ -> ());
  let fields =
    [
      ( "components",
        `Assoc
          (List.map (fun (name, status) -> (name, `String status)) components)
      );
      ("gateway_port", `Int config.gateway.port);
      ("gateway_host", `String config.gateway.host);
      ("telegram_enabled", `Bool (config.channels.telegram <> None));
      ("discord_enabled", `Bool (config.channels.discord <> None));
      ("slack_enabled", `Bool (config.channels.slack <> None));
      ("github_enabled", `Bool (config.channels.github <> None));
      ("pid", `Int (Unix.getpid ()));
    ]
  in
  let fields =
    match pairing_code with
    | Some code -> ("pairing_code", `String code) :: fields
    | None -> fields
  in
  let json = `Assoc fields in
  try
    let oc = open_out state_path in
    output_string oc (Yojson.Safe.pretty_to_string json);
    close_out oc
  with exn ->
    Logs.warn (fun m ->
        m "Failed to write daemon state: %s" (Printexc.to_string exn))

type resume_senders = {
  send_telegram :
    bot_token:string -> chat_id:string -> text:string -> unit Lwt.t;
  send_discord :
    bot_token:string -> channel_id:string -> text:string -> unit Lwt.t;
  send_slack :
    bot_token:string -> channel_id:string -> text:string -> unit Lwt.t;
}

let default_resume_senders =
  {
    send_telegram =
      (fun ~bot_token ~chat_id ~text ->
        Telegram.send_message ~bot_token ~chat_id ~text ());
    send_discord = Discord.send_message;
    send_slack = Slack.send_message;
  }

type exit_intent = Shutdown | Restart

let initial_drain_warning = "Restarting soon, finishing current requests..."

let drain_warning_schedule =
  [
    (5.0, "Still restarting, please wait (5s)...");
    (10.0, "Still restarting (10s)...");
    (15.0, "Still restarting (15s)...");
    (30.0, "Still restarting (30s)...");
    (45.0, "Almost there (45s)...");
    (60.0, "Restart timeout reached, forcing restart now.");
  ]

let send_drain_warnings ?(schedule = drain_warning_schedule)
    ~(session_manager : Session.t) ~stop () =
  let rec loop last_t = function
    | [] -> Lwt.return_unit
    | (t, message) :: rest ->
        let open Lwt.Syntax in
        let* () =
          if t > last_t then Lwt_unix.sleep (t -. last_t) else Lwt.return_unit
        in
        if !stop then Lwt.return_unit
        else begin
          let* () = Session.notify_channel_sessions session_manager message in
          loop t rest
        end
  in
  loop 0.0 schedule

let wait_for_drain ?(attempts = 600) ?(sleep_seconds = 0.1)
    ~(session_manager : Session.t) () =
  let rec loop attempts_remaining =
    let open Lwt.Syntax in
    if Session.current_in_flight session_manager = 0 then Lwt.return false
    else if attempts_remaining <= 0 then begin
      Logs.warn (fun m ->
          m "Drain timeout, forcing restart with %d requests in flight"
            (Session.current_in_flight session_manager));
      Lwt.return true
    end
    else begin
      let* () = Lwt_unix.sleep sleep_seconds in
      loop (attempts_remaining - 1)
    end
  in
  loop attempts

let dispatch_resumed_message ?(senders = default_resume_senders)
    ~(config : Runtime_config.t) ~channel ~channel_id ~text () =
  let open Lwt.Syntax in
  match channel with
  | "telegram" -> (
      match config.channels.telegram with
      | Some { accounts = (_, account) :: _ } ->
          let* () =
            senders.send_telegram ~bot_token:account.bot_token
              ~chat_id:channel_id ~text
          in
          Lwt.return (Ok ())
      | Some { accounts = [] } | None ->
          Lwt.return (Error "telegram channel is not configured"))
  | "discord" -> (
      match config.channels.discord with
      | Some discord ->
          let* () =
            senders.send_discord ~bot_token:discord.bot_token ~channel_id ~text
          in
          Lwt.return (Ok ())
      | None -> Lwt.return (Error "discord channel is not configured"))
  | "slack" -> (
      match config.channels.slack with
      | Some slack ->
          let* () =
            senders.send_slack ~bot_token:slack.bot_token ~channel_id ~text
          in
          Lwt.return (Ok ())
      | None -> Lwt.return (Error "slack channel is not configured"))
  | _ -> Lwt.return (Error (Printf.sprintf "unsupported channel %s" channel))

let default_resume_turn ~(session_manager : Session.t) ~session_key agent
    interrupt =
  let open Lwt.Syntax in
  let* compacted = Agent.compact_history_if_needed agent in
  if compacted then
    Session.persist_compacted_history session_manager ~key:session_key agent;
  let runtime_context =
    Prompt_builder.build_runtime_context ~config:session_manager.config
      ~details:
        (Session.runtime_context_details session_manager ~agent ~key:session_key
           ~compacted_before_turn:compacted)
      ()
  in
  Agent.turn agent ~user_message:"" ?db:session_manager.db ~session_key
    ~interrupt_check:(fun () -> !interrupt)
    ?runtime_context ~history_prepared:true ()

let resume_agent_session ?(senders = default_resume_senders) ?run_turn
    ~(session_manager : Session.t) ~(config : Runtime_config.t) ~session_key
    ~channel ~channel_id () =
  let run_turn =
    match run_turn with
    | Some f -> f
    | None -> default_resume_turn ~session_manager ~session_key
  in
  let open Lwt.Syntax in
  Session.with_session_lock session_manager ~key:session_key
    (fun agent interrupt ->
      let history_before = List.length agent.Agent.history in
      let* response =
        Session.with_in_flight session_manager (fun () ->
            run_turn agent interrupt)
      in
      Session.persist_new_messages session_manager ~key:session_key
        ~history_before agent;
      let* dispatch_result =
        dispatch_resumed_message ~senders ~config ~channel ~channel_id
          ~text:response ()
      in
      match dispatch_result with
      | Ok () ->
          Session.mark_response_sent session_manager ~key:session_key;
          Lwt.return_unit
      | Error msg ->
          Logs.warn (fun m ->
              m "Failed to deliver resumed session %s via %s:%s: %s" session_key
                channel channel_id msg);
          Lwt.return_unit)

let resume_pending_agent_sessions ?(senders = default_resume_senders)
    ?resume_one ~(session_manager : Session.t) ~(config : Runtime_config.t) () =
  let resume_one =
    match resume_one with
    | Some f -> f
    | None ->
        fun ~session_key ~channel ~channel_id ->
          resume_agent_session ~senders ~session_manager ~config ~session_key
            ~channel ~channel_id ()
  in
  let pending =
    Session.load_pending_agent_sessions session_manager ~max_age_seconds:3600
  in
  let open Lwt.Syntax in
  if pending <> [] then
    Logs.info (fun m ->
        m "Resuming %d pending agent sessions" (List.length pending));
  Lwt_list.iter_s
    (fun (session_key, channel_opt, channel_id_opt) ->
      match (channel_opt, channel_id_opt) with
      | Some channel, Some channel_id ->
          Lwt.catch
            (fun () -> resume_one ~session_key ~channel ~channel_id)
            (fun exn ->
              Logs.err (fun m ->
                  m "Failed to resume session %s: %s" session_key
                    (Printexc.to_string exn));
              Lwt.return_unit)
      | _ ->
          Logs.warn (fun m ->
              m "Cannot resume session %s: missing channel info" session_key);
          Session.mark_response_sent session_manager ~key:session_key;
          Lwt.return_unit)
    pending

let local_date_key t =
  let tm = Unix.localtime t in
  (tm.Unix.tm_year, tm.Unix.tm_yday)

let pp_date_banner ppf t =
  let tm = Unix.localtime t in
  Fmt.pf ppf "=== %04d-%02d-%02d ===" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday

let maybe_emit_date_banner ppf last_date_ref t =
  let date_key = local_date_key t in
  if !last_date_ref <> Some date_key then begin
    pp_date_banner ppf t;
    Format.pp_print_newline ppf ();
    last_date_ref := Some date_key
  end

let run ~(config : Runtime_config.t) =
  let open Lwt.Syntax in
  let pp_header_with_ts ppf t h =
    let tm = Unix.localtime t in
    let ms = int_of_float ((t -. floor t) *. 1000.0) in
    Fmt.pf ppf "[%02d:%02d:%02d.%03d] %a" tm.Unix.tm_hour tm.Unix.tm_min
      tm.Unix.tm_sec ms Logs_fmt.pp_header h
  in
  let is_loopback_host host =
    let h = String.lowercase_ascii (String.trim host) in
    h = "127.0.0.1" || h = "localhost" || h = "::1"
  in
  if
    (not (is_loopback_host config.gateway.host))
    && config.gateway.auth_token = None
  then
    failwith
      (Printf.sprintf
         "Refusing to bind gateway.host=%S without gateway.auth_token.\n\
          To keep the gateway loopback-only, set gateway.host to 127.0.0.1 (or \
          localhost / ::1).\n\
          Example: clawq config set gateway.host 127.0.0.1\n\
          To allow non-loopback binding, set gateway.auth_token in \
          ~/.clawq/config.json or run: clawq config set gateway.auth_token \
          YOUR_TOKEN"
         config.gateway.host);
  if (not config.gateway.require_pairing) && config.gateway.auth_token = None
  then
    Logs.warn (fun m ->
        m
          "Gateway running without require_pairing or auth_token; suitable \
           only for local development on loopback");
  (* All log output is rendered to a side buffer first so we can:
     1. Drop cohttp/client.ml HTTP-method request lines (those log the full
        request URI at Info level via the default "application" source).
     2. Scrub Telegram bot tokens from any message before it reaches stderr
        (the Telegram API embeds the token in the URL path, e.g.
        /bot<TOKEN>/sendChatAction).
     Both branches always call k () so the return type stays polymorphic.
     Named cohttp sources only log at Debug, already silenced by the global
     Info level, but we set their level explicitly below for safety. *)
  let check_buf = Buffer.create 128 in
  let check_ppf = Format.formatter_of_buffer check_buf in
  let last_log_date = ref None in
  let starts_with_http_method s =
    let n = String.length s in
    (n >= 4 && String.sub s 0 4 = "GET ")
    || (n >= 5 && String.sub s 0 5 = "POST ")
    || (n >= 4 && String.sub s 0 4 = "PUT ")
    || (n >= 7 && String.sub s 0 7 = "DELETE ")
    || (n >= 5 && String.sub s 0 5 = "HEAD ")
    || (n >= 6 && String.sub s 0 6 = "PATCH ")
  in
  (* Replace /bot<TOKEN>/ with /bot<REDACTED>/ in Telegram API URI paths. *)
  let scrub_telegram_tokens s =
    let marker = "/bot" in
    let mlen = 4 in
    let slen = String.length s in
    let buf = Buffer.create slen in
    let i = ref 0 in
    while !i < slen do
      if !i + mlen <= slen && String.sub s !i mlen = marker then begin
        let j = ref (!i + mlen) in
        while !j < slen && s.[!j] <> '/' do
          incr j
        done;
        if !j < slen then begin
          Buffer.add_string buf "/bot<REDACTED>";
          i := !j
        end
        else begin
          Buffer.add_char buf s.[!i];
          incr i
        end
      end
      else begin
        Buffer.add_char buf s.[!i];
        incr i
      end
    done;
    Buffer.contents buf
  in
  let report src level ~over k msgf =
    (* Render to check buffer to peek at and sanitize message content. *)
    msgf (fun ?header ?tags:_ fmt ->
        Format.pp_print_flush check_ppf ();
        Buffer.clear check_buf;
        Format.kfprintf
          (fun ppf ->
            Format.pp_print_flush ppf ();
            let s = Buffer.contents check_buf in
            if
              Logs.Src.name src = "application"
              && level = Logs.Info && starts_with_http_method s
            then (
              over ();
              k ())
            else begin
              let s = scrub_telegram_tokens s in
              let dst = Format.err_formatter in
              let t = Unix.gettimeofday () in
              maybe_emit_date_banner dst last_log_date t;
              pp_header_with_ts dst t (level, header);
              Format.pp_print_string dst s;
              Format.pp_print_newline dst ();
              over ();
              k ()
            end)
          check_ppf fmt)
  in
  let reporter = { Logs.report } in
  Logs.set_reporter reporter;
  Logs.set_level (Some Logs.Info);
  List.iter
    (fun src ->
      let name = Logs.Src.name src in
      if String.length name >= 6 && String.sub name 0 6 = "cohttp" then
        Logs.Src.set_level src (Some Logs.Warning))
    (Logs.Src.list ());
  Logs.info (fun m ->
      m "clawq %s starting (pid=%d)" Build_info.version_string (Unix.getpid ()));
  let workspace = Runtime_config.effective_workspace config in
  Workspace_scaffold.ensure_dir workspace;
  let active_provider, _, active_model = Provider.select_provider ~config in
  Logs.info (fun m ->
      m "Provider: %s | Model: %s | Temp: %.2f" active_provider active_model
        config.default_temperature);
  Logs.info (fun m -> m "Workspace: %s" workspace);
  Logs.info (fun m ->
      m
        "Channels: cli=%b telegram=%b discord=%b slack=%b github=%b signal=%b \
         matrix=%b irc=%b email=%b nostr=%b dingtalk=%b onebot=%b lark=%b"
        config.channels.cli
        (config.channels.telegram <> None)
        (config.channels.discord <> None)
        (config.channels.slack <> None)
        (config.channels.github <> None)
        (config.channels.signal <> None)
        (config.channels.matrix <> None)
        (config.channels.irc <> None)
        (config.channels.email <> None)
        (config.channels.nostr <> None)
        (config.channels.dingtalk <> None)
        (config.channels.onebot <> None)
        (match config.channels.lark with
        | Some lk -> lk.enabled
        | None -> false));
  let sandbox =
    let backend = Sandbox.backend_of_policy config.security.sandbox_backend in
    {
      Sandbox.backend;
      workspace;
      extra_allowed_paths =
        config.security.extra_allowed_paths
        |> List.map Runtime_config.expand_home;
      isolate_filesystem = config.security.workspace_only;
    }
  in
  Logs.info (fun m ->
      m "Sandbox backend: %s"
        (Sandbox.backend_to_string sandbox.Sandbox.backend));
  let tool_registry =
    if config.security.tools_enabled then begin
      let registry = Tool_registry.create () in
      Tools_builtin.register_all ~config ~sandbox registry;
      let skills =
        Skills.load_all ~workspace_only:config.security.workspace_only
          ~allowed_commands:Tools_builtin.default_shell_allowlist ()
      in
      List.iter
        (fun s ->
          Tool_registry.register registry s;
          Logs.info (fun m -> m "Loaded skill: %s" s.Tool.name))
        skills;
      Tool_registry.register registry
        (Skills.skill_create_tool ~workspace_only:config.security.workspace_only
           ~allowed_commands:Tools_builtin.default_shell_allowlist registry);
      Tool_registry.register registry (Skills.skill_list_tool ());
      Logs.info (fun m ->
          m "Tools enabled, registered built-in tools + %d skills"
            (List.length skills));
      Some registry
    end
    else begin
      Logs.info (fun m ->
          m "Tools disabled (set security.tools_enabled to enable)");
      None
    end
  in
  (* Connect MCP stdio clients and register their tools *)
  let mcp_clients = ref [] in
  (match (tool_registry, config.mcp.enabled) with
  | Some registry, true -> (
      let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
      let servers_path =
        Filename.concat (Filename.concat home ".clawq") "mcp_servers.json"
      in
      if Sys.file_exists servers_path then
        try
          let json = Yojson.Safe.from_file servers_path in
          let open Yojson.Safe.Util in
          let servers = try json |> to_list with _ -> [] in
          List.iter
            (fun s ->
              try
                let name = s |> member "name" |> to_string in
                let command = s |> member "command" |> to_string in
                let args =
                  try s |> member "args" |> to_list |> List.map to_string
                  with _ -> []
                in
                let env =
                  try
                    s |> member "env" |> to_assoc
                    |> List.map (fun (k, v) -> (k, to_string v))
                  with _ -> []
                in
                let cfg = { Mcp_client.name; command; args; env } in
                let client =
                  Lwt_main.run
                    (Lwt.catch
                       (fun () ->
                         let open Lwt.Syntax in
                         let* c = Mcp_client.connect cfg in
                         Lwt.return (Some c))
                       (fun exn ->
                         Logs.warn (fun m ->
                             m "MCP client '%s' failed to connect: %s" name
                               (Printexc.to_string exn));
                         Lwt.return_none))
                in
                match client with
                | None -> ()
                | Some c ->
                    mcp_clients := c :: !mcp_clients;
                    List.iter
                      (fun t ->
                        Tool_registry.register registry t;
                        Logs.info (fun m ->
                            m "MCP tool registered: %s (from %s)" t.Tool.name
                              name))
                      (Mcp_client.discovered_tools c)
              with exn ->
                Logs.warn (fun m ->
                    m "MCP server config parse error: %s"
                      (Printexc.to_string exn)))
            servers
        with exn ->
          Logs.warn (fun m ->
              m "Failed to load MCP servers config: %s" (Printexc.to_string exn))
      )
  | _ -> ());
  let db =
    let db_path =
      if config.memory.db_path <> "" then config.memory.db_path
      else
        let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
        Filename.concat (Filename.concat home ".clawq") "memory.db"
    in
    try
      let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
      let clawq_dir = Filename.concat home ".clawq" in
      (try if not (Sys.file_exists clawq_dir) then Sys.mkdir clawq_dir 0o755
       with _ -> ());
      let db =
        Memory.init ~db_path ~search_enabled:config.memory.search_enabled ()
      in
      Vector.init_schema db;
      if config.security.audit_enabled then begin
        Audit.init_schema db;
        Logs.info (fun m -> m "Audit trail enabled")
      end;
      Logs.info (fun m ->
          m "SQLite memory initialized at %s (vector index enabled)" db_path);
      Some db
    with exn ->
      Logs.warn (fun m ->
          m "Failed to initialize SQLite memory: %s" (Printexc.to_string exn));
      None
  in
  (* Register memory tools into existing registry now that db is available *)
  (match (tool_registry, db) with
  | Some registry, Some db ->
      Tool_registry.register registry (Tools_builtin.memory_store ~db);
      Tool_registry.register registry (Tools_builtin.memory_recall ~db);
      Tool_registry.register registry (Tools_builtin.memory_forget ~db);
      Tool_registry.register registry (Tools_builtin.memory_list ~db)
  | _ -> ());
  (* Auto-hydrate core memories from snapshot if db is empty *)
  (match db with
  | Some db ->
      let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
      let snapshot_path =
        Filename.concat (Filename.concat home ".clawq") "memory_snapshot.json"
      in
      if Sys.file_exists snapshot_path then begin
        let count = Memory.count_core ~db in
        if count = 0 then begin
          Logs.info (fun m ->
              m "Auto-hydrating core memories from %s" snapshot_path);
          try Memory.import_snapshot ~db ~path:snapshot_path
          with exn ->
            Logs.warn (fun m ->
                m "Failed to import memory snapshot: %s"
                  (Printexc.to_string exn))
        end
      end
  | None -> ());
  let signing_key =
    match db with
    | Some _db
      when config.security.audit_enabled
           && config.security.audit_signing_enabled -> (
        match Audit.get_signing_key () with
        | Ok k ->
            Logs.info (fun m -> m "Audit signing enabled");
            Some k
        | Error msg ->
            Logs.warn (fun m -> m "Audit signing key unavailable: %s" msg);
            None)
    | _ -> None
  in
  let rl = config.security.rate_limit in
  let ip_limiter =
    Rate_limiter.create ~rate_per_minute:rl.gateway_per_ip_rpm
      ~burst_multiplier:rl.burst_multiplier
  in
  let session_limiter =
    Rate_limiter.create ~rate_per_minute:rl.gateway_per_session_rpm
      ~burst_multiplier:rl.burst_multiplier
  in
  let chat_limiter =
    Rate_limiter.create ~rate_per_minute:rl.telegram_per_chat_rpm
      ~burst_multiplier:rl.burst_multiplier
  in
  let discord_message_limiter =
    Rate_limiter.create ~rate_per_minute:rl.telegram_per_chat_rpm
      ~burst_multiplier:rl.burst_multiplier
  in
  let slack_event_limiter =
    Rate_limiter.create ~rate_per_minute:rl.telegram_per_chat_rpm
      ~burst_multiplier:rl.burst_multiplier
  in
  let landlock_enabled = config.security.landlock_enabled in
  if landlock_enabled then begin
    Logs.info (fun m -> m "Landlock sandbox requested, activating...");
    Landlock.sandbox_workspace ~config
  end;
  let telemetry =
    match config.telemetry with
    | Some tc when tc.enabled && tc.endpoint <> "" ->
        let t =
          Telemetry.create ~endpoint:tc.endpoint ~service_name:tc.service_name
        in
        Logs.info (fun m ->
            m "OpenTelemetry enabled: endpoint=%s service=%s" tc.endpoint
              tc.service_name);
        Some t
    | _ -> None
  in
  let session_manager =
    Session.create ~config ?tool_registry ~sandbox ~landlock_enabled ?db ()
  in
  (match tool_registry with
  | Some registry ->
      Tool_registry.register registry
        (Tools_builtin.send_message
           ~send_fn:
             (Some
                (fun ~text ->
                  Session.notify_channel_sessions session_manager text)))
  | None -> ());
  let update_lock = Lwt_mutex.create () in
  let update_in_progress = ref false in
  let claim_update () =
    Lwt_mutex.with_lock update_lock (fun () ->
        if !update_in_progress || Session.is_draining session_manager then
          Lwt.return false
        else begin
          update_in_progress := true;
          Lwt.return true
        end)
  in
  let finish_update () =
    Lwt_mutex.with_lock update_lock (fun () ->
        update_in_progress := false;
        Lwt.return_unit)
  in
  let run_update ?prepare_restart ~send_progress () =
    Update_tool.run_update ?prepare_restart ~claim_update ~finish_update
      ~is_draining:(fun () -> Session.is_draining session_manager)
      ~send_progress ()
  in
  (match tool_registry with
  | Some registry ->
      Tool_registry.register registry
        (Update_tool.tool ~claim_update ~finish_update
           ~is_draining:(fun () -> Session.is_draining session_manager)
           ())
  | None -> ());
  Session.set_special_command_handler session_manager
    (fun ~key ~message ~send_progress ->
      if not (Update_tool.is_update_command message) then Lwt.return_none
      else
        let send_progress =
          match send_progress with
          | Some f -> f
          | None -> fun _ -> Lwt.return_unit
        in
        let prepare_restart () =
          (match Restart_notify.parse_channel_from_key key with
          | Some (channel, channel_id) ->
              Restart_notify.write ~channel ~channel_id
          | None -> ());
          Lwt.return (Ok ())
        in
        let open Lwt.Syntax in
        let* response = run_update ~prepare_restart ~send_progress () in
        Lwt.return_some response);
  let* () = resume_pending_agent_sessions ~session_manager ~config () in
  let* () =
    Lwt.catch
      (fun () ->
        match Restart_notify.read () with
        | Some (channel, channel_id) ->
            Restart_notify.remove ();
            let text =
              Printf.sprintf "clawq updated and restarted successfully (%s)."
                Build_info.version_string
            in
            let open Lwt.Syntax in
            let* result =
              dispatch_resumed_message ~config ~channel ~channel_id ~text ()
            in
            (match result with
            | Ok () ->
                Logs.info (fun m ->
                    m "Sent post-update notification to %s:%s" channel
                      channel_id)
            | Error err ->
                Logs.warn (fun m ->
                    m "Failed to send post-update notification: %s" err));
            Lwt.return_unit
        | None -> Lwt.return_unit)
      (fun exn ->
        Logs.warn (fun m ->
            m "Error checking restart notification marker: %s"
              (Printexc.to_string exn));
        Lwt.return_unit)
  in
  let tunnel_url_ref = ref None in
  let[@warning "-26"] tunnel_supervisor =
    if config.tunnel.enabled then begin
      let initial_url, supervisor =
        Cf_tunnel.start ~config:config.tunnel ~on_url:(fun url ->
            tunnel_url_ref := Some url;
            Logs.info (fun m -> m "Tunnel URL: %s" url);
            match config.channels.github with
            | Some _ ->
                Logs.info (fun m ->
                    m "GitHub webhooks ready at: %s/github/webhook/..." url)
            | None -> ())
      in
      tunnel_url_ref := initial_url;
      supervisor
    end
    else Lwt.return_unit
  in
  if config.channels.github <> None && not config.tunnel.enabled then
    Logs.warn (fun m ->
        m
          "GitHub channel configured but tunnel is disabled; webhooks may not \
           be reachable");
  let github_api_limiter =
    Rate_limiter.create ~rate_per_minute:60 ~burst_multiplier:1.0
  in
  let web_channel_handler =
    match config.web_channel with
    | Some wc_cfg when wc_cfg.enabled ->
        let wc = Web_channel.create ~config:wc_cfg ~session_manager in
        Logs.info (fun m ->
            m "WebChannel enabled at prefix: %s" wc_cfg.path_prefix);
        Some wc
    | _ -> None
  in
  let[@warning "-26"] pairing =
    if config.gateway.require_pairing then begin
      let p =
        Pairing.create ~max_attempts:config.gateway.max_pair_attempts
          ~lockout_seconds:(float_of_int config.gateway.pair_lockout_seconds)
          ()
      in
      let s = Pairing.status p in
      Logs.info (fun m ->
          m "OTP pairing code: %s (share with trusted clients)" s.code);
      Some p
    end
    else None
  in
  let ui_server = Ui_server.init () in
  let write_runtime_state ~components =
    let pairing_code =
      match pairing with Some p -> Some (Pairing.status p).code | None -> None
    in
    write_state ~pairing_code ~config ~components
  in
  Logs.info (fun m ->
      m "Web UI assets ready at %s (version=%s dev_mode=%b)" ui_server.ui_dir
        (Ui_server.version ui_server)
        ui_server.dev_mode);
  write_runtime_state
    ~components:
      [
        ("gateway", "starting");
        ("telegram", "starting");
        ("discord", "starting");
        ("slack", "starting");
        ("signal", "starting");
        ("matrix", "starting");
        ("irc", "starting");
        ("email", "starting");
        ("nostr", "starting");
        ("dingtalk", "starting");
        ("onebot", "starting");
        ("lark", "starting");
      ];
  let gateway_stop, stop_gateway = Lwt.wait () in
  let gateway =
    Lwt.catch
      (fun () ->
        Http_server.start ~port:config.gateway.port ~host:config.gateway.host
          ~require_pairing:config.gateway.require_pairing
          ~auth_token:config.gateway.auth_token ~session_manager
          ?slack_config:config.channels.slack
          ?github_config:config.channels.github ~github_api_limiter ~ip_limiter
          ~session_limiter ~slack_event_limiter ?web_channel:web_channel_handler
          ~slack_run_update_command:run_update
          ?whatsapp_config:config.channels.whatsapp
          ?line_config:config.channels.line
          ?lark_config:
            (match config.channels.lark with
            | Some lc when lc.enabled && lc.mode = "webhook" -> Some lc
            | _ -> None)
          ?pairing ~ui_server ~stop:gateway_stop ())
      (fun exn ->
        Logs.err (fun m ->
            m "Gateway server error: %s" (Printexc.to_string exn));
        Lwt.return_unit)
  in
  let telegram =
    Lwt.catch
      (fun () ->
        Telegram.start_polling ~config ~session_manager
          ~run_update_command:run_update ~chat_limiter ())
      (fun exn ->
        Logs.err (fun m ->
            m "Telegram polling error: %s" (Printexc.to_string exn));
        Lwt.return_unit)
  in
  let shutdown_waiter, shutdown_resolver = Lwt.wait () in
  let restart_waiter, restart_resolver = Lwt.wait () in
  let shutting_down = ref false in
  let restarting = ref false in
  let do_shutdown _ =
    if not !shutting_down then begin
      shutting_down := true;
      Logs.info (fun m -> m "Received shutdown signal, stopping...");
      write_runtime_state
        ~components:[ ("gateway", "stopping"); ("telegram", "stopping") ];
      Lwt.wakeup_later shutdown_resolver ()
    end
  in
  let do_restart _ =
    if not !restarting then begin
      restarting := true;
      Logs.info (fun m -> m "SIGUSR1 received, initiating graceful restart");
      write_runtime_state
        ~components:[ ("gateway", "restarting"); ("telegram", "restarting") ];
      Lwt.wakeup_later restart_resolver ()
    end
  in
  let _ = Lwt_unix.on_signal Sys.sigint do_shutdown in
  let _ = Lwt_unix.on_signal Sys.sigterm do_shutdown in
  let _ = Lwt_unix.on_signal Sys.sigusr1 do_restart in
  let _ =
    Lwt_unix.on_signal Sys.sighup (fun _ ->
        Logs.info (fun m -> m "SIGHUP received, reloading config...");
        try
          let new_config = Config_loader.load () in
          Session.update_config session_manager new_config;
          Logs.info (fun m -> m "Config reloaded successfully")
        with exn ->
          Logs.err (fun m ->
              m "Config reload failed: %s" (Printexc.to_string exn)))
  in
  let slack_socket_enabled =
    match config.channels.slack with
    | Some sc when sc.socket_mode && sc.app_token <> "" -> true
    | _ -> false
  in
  write_runtime_state
    ~components:
      ([
         ("gateway", "running");
         ("telegram", "running");
         ("discord", "running");
         ("slack", "running");
         ("cron", "running");
         ("signal", "running");
         ("matrix", "running");
         ("irc", "running");
         ("email", "running");
         ("nostr", "running");
         ("dingtalk", "running");
         ("onebot", "running");
         ("lark", "running");
       ]
      @ if slack_socket_enabled then [ ("slack_socket", "running") ] else []);
  (match db with
  | Some db when config.security.audit_enabled ->
      Audit.log ~db ?signing_key
        (DaemonEvent
           {
             action = "start";
             details =
               Printf.sprintf "pid=%d gateway=%s:%d" (Unix.getpid ())
                 config.gateway.host config.gateway.port;
           })
  | _ -> ());
  Logs.info (fun m ->
      m "Daemon ready. Gateway on %s:%d" config.gateway.host config.gateway.port);
  Lwt.async (fun () -> telegram);
  Lwt.async (fun () ->
      Lwt.catch
        (fun () ->
          Discord.start ~config ~session_manager ~db
            ~message_limiter:discord_message_limiter)
        (fun exn ->
          Logs.err (fun m ->
              m "Discord channel error: %s" (Printexc.to_string exn));
          Lwt.return_unit));
  (match config.channels.slack with
  | Some sc when sc.socket_mode && sc.app_token <> "" ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () ->
              Slack_socket.start ~config ~session_manager
                ~event_limiter:slack_event_limiter)
            (fun exn ->
              Logs.err (fun m ->
                  m "Slack Socket Mode error: %s" (Printexc.to_string exn));
              Lwt.return_unit))
  | _ -> ());
  Lwt.async (fun () ->
      Lwt.catch
        (fun () -> Mattermost.start ~config ~session_manager)
        (fun exn ->
          Logs.err (fun m ->
              m "Mattermost channel error: %s" (Printexc.to_string exn));
          Lwt.return_unit));
  Lwt.async (fun () ->
      Lwt.catch
        (fun () -> tunnel_supervisor)
        (fun exn ->
          Logs.err (fun m ->
              m "Tunnel supervisor error: %s" (Printexc.to_string exn));
          Lwt.return_unit));
  Lwt.async (fun () ->
      Lwt.catch
        (fun () -> Imessage.start ~config ~session_manager)
        (fun exn ->
          Logs.err (fun m ->
              m "iMessage channel error: %s" (Printexc.to_string exn));
          Lwt.return_unit));
  (match config.channels.signal with
  | Some _ ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () -> Signal.start ~config ~session_manager)
            (fun exn ->
              Logs.err (fun m ->
                  m "Signal channel error: %s" (Printexc.to_string exn));
              Lwt.return_unit))
  | None -> ());
  (match config.channels.matrix with
  | Some _ ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () -> Matrix.start ~config ~session_manager)
            (fun exn ->
              Logs.err (fun m ->
                  m "Matrix channel error: %s" (Printexc.to_string exn));
              Lwt.return_unit))
  | None -> ());
  (match config.channels.irc with
  | Some _ ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () -> Irc.start ~config ~session_manager)
            (fun exn ->
              Logs.err (fun m ->
                  m "IRC channel error: %s" (Printexc.to_string exn));
              Lwt.return_unit))
  | None -> ());
  (match config.channels.email with
  | Some _ ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () -> Email_channel.start ~config ~session_manager)
            (fun exn ->
              Logs.err (fun m ->
                  m "Email channel error: %s" (Printexc.to_string exn));
              Lwt.return_unit))
  | None -> ());
  (match config.channels.nostr with
  | Some _ ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () -> Nostr.start ~config ~session_manager)
            (fun exn ->
              Logs.err (fun m ->
                  m "Nostr channel error: %s" (Printexc.to_string exn));
              Lwt.return_unit))
  | None -> ());
  (match config.channels.dingtalk with
  | Some _ ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () -> Dingtalk.start ~config ~session_manager)
            (fun exn ->
              Logs.err (fun m ->
                  m "DingTalk channel error: %s" (Printexc.to_string exn));
              Lwt.return_unit))
  | None -> ());
  (match config.channels.onebot with
  | Some _ ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () -> Onebot.start ~config ~session_manager)
            (fun exn ->
              Logs.err (fun m ->
                  m "OneBot channel error: %s" (Printexc.to_string exn));
              Lwt.return_unit))
  | None -> ());
  (match config.channels.lark with
  | Some lk when lk.enabled ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () -> Lark.start ~config ~session_manager)
            (fun exn ->
              Logs.err (fun m ->
                  m "Lark channel error: %s" (Printexc.to_string exn));
              Lwt.return_unit))
  | Some _ | None -> ());
  (match db with
  | Some db ->
      Scheduler.init_schema db;
      Lwt.async (fun () ->
          Lwt.catch
            (fun () ->
              let last_memory_cleanup = ref (Unix.gettimeofday ()) in
              let last_retention_run = ref 0.0 in
              let rec loop () =
                let open Lwt.Syntax in
                let* () = Lwt_unix.sleep 60.0 in
                let* () = Scheduler.tick ~db ~session_mgr:session_manager in
                let now = Unix.gettimeofday () in
                if now -. !last_memory_cleanup >= 3600.0 then begin
                  last_memory_cleanup := now;
                  let mem = config.memory in
                  Logs.info (fun m -> m "Running periodic memory cleanup");
                  Memory.cleanup_all ~db
                    ~max_messages:mem.max_messages_per_session
                    ~max_age_days:mem.max_message_age_days
                end;
                if
                  config.security.audit_enabled
                  && now -. !last_retention_run >= 3600.0
                then begin
                  last_retention_run := now;
                  ignore (Audit.retention_tick ~db ~config)
                end;
                let* () =
                  Rate_limiter.cleanup_expired ip_limiter
                    ~max_idle_seconds:300.0
                in
                let* () =
                  Rate_limiter.cleanup_expired session_limiter
                    ~max_idle_seconds:300.0
                in
                let* () =
                  Rate_limiter.cleanup_expired chat_limiter
                    ~max_idle_seconds:300.0
                in
                let* () =
                  Rate_limiter.cleanup_expired discord_message_limiter
                    ~max_idle_seconds:300.0
                in
                let* () =
                  Rate_limiter.cleanup_expired slack_event_limiter
                    ~max_idle_seconds:300.0
                in
                let* () =
                  match telemetry with
                  | Some t -> Telemetry.maybe_flush t
                  | None -> Lwt.return_unit
                in
                loop ()
              in
              loop ())
            (fun exn ->
              Logs.err (fun m ->
                  m "Cron scheduler error: %s" (Printexc.to_string exn));
              Lwt.return_unit));
      Logs.info (fun m -> m "Cron scheduler started")
  | None -> Logs.info (fun m -> m "Cron scheduler disabled (no database)"));
  if config.heartbeat.heartbeat_enabled then begin
    let hb = config.heartbeat in
    Logs.info (fun m ->
        m "Heartbeat enabled: interval=%ds quiet=%d:00-%d:00"
          hb.heartbeat_interval_seconds hb.heartbeat_quiet_start
          hb.heartbeat_quiet_end);
    Lwt.async (fun () ->
        Lwt.catch
          (fun () ->
            let rec hb_loop () =
              let open Lwt.Syntax in
              let* () =
                Lwt_unix.sleep (float_of_int hb.heartbeat_interval_seconds)
              in
              let tm = Unix.localtime (Unix.gettimeofday ()) in
              let hour = tm.Unix.tm_hour in
              let in_quiet =
                if hb.heartbeat_quiet_start > hb.heartbeat_quiet_end then
                  hour >= hb.heartbeat_quiet_start
                  || hour < hb.heartbeat_quiet_end
                else
                  hour >= hb.heartbeat_quiet_start
                  && hour < hb.heartbeat_quiet_end
              in
              if in_quiet then begin
                Logs.debug (fun m -> m "Heartbeat: quiet hours, skipping");
                hb_loop ()
              end
              else
                let hb_path = Filename.concat workspace "HEARTBEAT.md" in
                if Sys.file_exists hb_path then begin
                  let content =
                    try
                      let ic = open_in hb_path in
                      let n = in_channel_length ic in
                      let buf = Bytes.create n in
                      really_input ic buf 0 n;
                      close_in ic;
                      String.trim (Bytes.to_string buf)
                    with _ -> ""
                  in
                  if content = "" then hb_loop ()
                  else begin
                    if Session.is_draining session_manager then begin
                      Logs.info (fun m ->
                          m "Heartbeat: daemon draining, skipping turn");
                      hb_loop ()
                    end
                    else begin
                      let key = "__main__" in
                      let* result =
                        Lwt.catch
                          (fun () ->
                            Session.try_session_lock session_manager ~key
                              (fun agent _interrupt ->
                                Logs.info (fun m ->
                                    m
                                      "Heartbeat: processing HEARTBEAT.md (%d \
                                       chars) on main session"
                                      (String.length content));
                                let* compacted =
                                  Agent.prepare_turn_history agent
                                    ~user_message:content ?db ()
                                in
                                let runtime_context =
                                  Prompt_builder.build_runtime_context ~config
                                    ~details:
                                      (Session.runtime_context_details
                                         session_manager ~agent ~key
                                         ~compacted_before_turn:compacted)
                                    ()
                                in
                                Agent.turn agent ~user_message:content ?db
                                  ~session_key:key ?runtime_context
                                  ~history_prepared:true ()))
                          (fun exn ->
                            Logs.err (fun m ->
                                m "Heartbeat error: %s" (Printexc.to_string exn));
                            Lwt.return_none)
                      in
                      (match result with
                      | None ->
                          Logs.info (fun m ->
                              m
                                "Heartbeat: main session busy, skipping this \
                                 tick")
                      | Some response ->
                          let trimmed = String.trim response in
                          if trimmed = "HEARTBEAT_OK" then
                            Logs.info (fun m ->
                                m
                                  "Heartbeat: agent replied HEARTBEAT_OK, no \
                                   outbound")
                          else begin
                            Logs.info (fun m ->
                                m "Heartbeat: agent response (%d chars)"
                                  (String.length trimmed));
                            match config.notify with
                            | Some nc ->
                                Logs.info (fun m ->
                                    m "Heartbeat: would notify via %s -> %s"
                                      nc.notify_channel nc.notify_target)
                            | None ->
                                Logs.warn (fun m ->
                                    m
                                      "Heartbeat: agent wants to send a \
                                       message but no notify target configured")
                          end);
                      hb_loop ()
                    end
                  end
                end
                else hb_loop ()
            in
            hb_loop ())
          (fun exn ->
            Logs.err (fun m ->
                m "Heartbeat loop error: %s" (Printexc.to_string exn));
            Lwt.return_unit))
  end;
  let* picked_intent =
    Lwt.pick
      [
        (let open Lwt.Syntax in
         let* () = shutdown_waiter in
         Lwt.return Shutdown);
        (let open Lwt.Syntax in
         let* () = restart_waiter in
         Lwt.return Restart);
        (let open Lwt.Syntax in
         let* () = gateway in
         Lwt.return Shutdown);
      ]
  in
  let* final_intent =
    match picked_intent with
    | Shutdown ->
        Lwt.wakeup_later stop_gateway ();
        let* () = gateway in
        Lwt.return Shutdown
    | Restart ->
        let* () = Session.start_draining session_manager in
        Lwt.wakeup_later stop_gateway ();
        let* () = gateway in
        let* () =
          Session.notify_channel_sessions session_manager initial_drain_warning
        in
        let stop_warnings = ref false in
        let warnings_p =
          Lwt.catch
            (fun () ->
              send_drain_warnings ~session_manager ~stop:stop_warnings ())
            (fun exn ->
              Logs.warn (fun m ->
                  m "Drain warning loop failed: %s" (Printexc.to_string exn));
              Lwt.return_unit)
        in
        Lwt.async (fun () -> warnings_p);
        let* timed_out = wait_for_drain ~session_manager () in
        if not timed_out then stop_warnings := true;
        let* () = if timed_out then warnings_p else Lwt.return_unit in
        Lwt.return Restart
  in
  write_runtime_state
    ~components:
      [
        ("gateway", "stopped");
        ("telegram", "stopped");
        ("discord", "stopped");
        ("slack", if final_intent = Restart then "restarting" else "stopped");
      ];
  (match db with
  | Some db when config.security.audit_enabled ->
      Audit.log ~db ?signing_key
        (DaemonEvent
           {
             action = (if final_intent = Restart then "restart" else "stop");
             details =
               (if final_intent = Restart then "daemon restart requested"
                else "clean shutdown");
           })
  | _ -> ());
  (* Flush telemetry on shutdown *)
  let* () =
    match telemetry with Some t -> Telemetry.flush t | None -> Lwt.return_unit
  in
  (* Disconnect MCP clients *)
  let* () =
    Lwt_list.iter_s
      (fun c ->
        Lwt.catch (fun () -> Mcp_client.disconnect c) (fun _ -> Lwt.return_unit))
      !mcp_clients
  in
  (* PID file cleanup is handled by service.ml after Daemon.run returns *)
  Logs.info (fun m ->
      m "clawq daemon %s"
        (if final_intent = Restart then "ready to restart" else "stopped"));
  Lwt.return final_intent
