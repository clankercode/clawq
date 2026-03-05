let write_state ~(config : Runtime_config.t) ~components =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  let state_dir = Filename.concat home ".clawq" in
  let state_path = Filename.concat state_dir "daemon_state.json" in
  (try if not (Sys.file_exists state_dir) then Sys.mkdir state_dir 0o755
   with _ -> ());
  let json =
    `Assoc
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
        ("pid", `Int (Unix.getpid ()));
      ]
  in
  try
    let oc = open_out state_path in
    output_string oc (Yojson.Safe.pretty_to_string json);
    close_out oc
  with exn ->
    Logs.warn (fun m ->
        m "Failed to write daemon state: %s" (Printexc.to_string exn))

let run ~(config : Runtime_config.t) =
  let open Lwt.Syntax in
  let is_loopback_host host =
    let h = String.lowercase_ascii (String.trim host) in
    h = "127.0.0.1" || h = "localhost" || h = "::1"
  in
  if
    (not (is_loopback_host config.gateway.host))
    && config.gateway.auth_token = None
  then failwith "Refusing non-loopback gateway bind without gateway.auth_token";
  if (not config.gateway.require_pairing) && config.gateway.auth_token = None
  then
    Logs.warn (fun m ->
        m
          "Gateway running without require_pairing or auth_token; suitable \
           only for local development on loopback");
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  List.iter
    (fun src ->
      let name = Logs.Src.name src in
      if String.length name >= 6 && String.sub name 0 6 = "cohttp" then
        Logs.Src.set_level src (Some Logs.Warning))
    (Logs.Src.list ());
  Logs.info (fun m -> m "clawq daemon starting (pid=%d)" (Unix.getpid ()));
  let workspace = Runtime_config.effective_workspace config in
  Workspace_scaffold.ensure_dir workspace;
  let active_provider =
    let with_key =
      List.filter
        (fun (_, p) -> Runtime_config.is_key_set p.Runtime_config.api_key)
        config.providers
    in
    let preferred =
      match config.default_provider with
      | Some name -> (
          match List.find_opt (fun (n, _) -> n = name) config.providers with
          | Some (n, p) when Runtime_config.is_key_set p.api_key -> Some n
          | _ -> None)
      | None -> None
    in
    match preferred with
    | Some n -> n
    | None -> ( match with_key with (n, _) :: _ -> n | [] -> "(none)")
  in
  Logs.info (fun m ->
      m "Provider: %s | Model: %s | Temp: %.2f" active_provider
        (Runtime_config.effective_primary_model config.agent_defaults)
        config.default_temperature);
  Logs.info (fun m -> m "Workspace: %s" workspace);
  Logs.info (fun m ->
      m "Channels: cli=%b telegram=%b discord=%b slack=%b" config.channels.cli
        (config.channels.telegram <> None)
        (config.channels.discord <> None)
        (config.channels.slack <> None));
  let tool_registry =
    if config.security.tools_enabled then begin
      let registry = Tool_registry.create () in
      Tools_builtin.register_all ~config registry;
      let skills =
        Skills.load_all ~workspace_only:config.security.workspace_only
          ~allowed_commands:Tools_builtin.default_shell_allowlist ()
      in
      List.iter
        (fun s ->
          Tool_registry.register registry s;
          Logs.info (fun m -> m "Loaded skill: %s" s.Tool.name))
        skills;
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
  if config.security.landlock_enabled then begin
    Logs.info (fun m -> m "Landlock sandbox requested, activating...");
    Landlock.sandbox_workspace ~config
  end;
  let session_manager = Session.create ~config ?tool_registry ?db () in
  write_state ~config
    ~components:[ ("gateway", "starting"); ("telegram", "starting") ];
  let gateway =
    Lwt.catch
      (fun () ->
        Http_server.start ~port:config.gateway.port ~host:config.gateway.host
          ~require_pairing:config.gateway.require_pairing
          ~auth_token:config.gateway.auth_token ~session_manager
          ?slack_config:config.channels.slack ~ip_limiter ~session_limiter ())
      (fun exn ->
        Logs.err (fun m ->
            m "Gateway server error: %s" (Printexc.to_string exn));
        Lwt.return_unit)
  in
  let telegram =
    Lwt.catch
      (fun () ->
        Telegram.start_polling ~config ~session_manager ~chat_limiter ())
      (fun exn ->
        Logs.err (fun m ->
            m "Telegram polling error: %s" (Printexc.to_string exn));
        Lwt.return_unit)
  in
  let shutdown_waiter, shutdown_resolver = Lwt.wait () in
  let shutting_down = ref false in
  let do_shutdown _ =
    if not !shutting_down then begin
      shutting_down := true;
      Logs.info (fun m -> m "Received shutdown signal, stopping...");
      write_state ~config
        ~components:[ ("gateway", "stopping"); ("telegram", "stopping") ];
      Lwt.wakeup_later shutdown_resolver ()
    end
  in
  let _ = Lwt_unix.on_signal Sys.sigint do_shutdown in
  let _ = Lwt_unix.on_signal Sys.sigterm do_shutdown in
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
  write_state ~config
    ~components:
      ([ ("gateway", "running"); ("telegram", "running"); ("cron", "running") ]
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
        (fun () -> Discord.start ~config ~session_manager)
        (fun exn ->
          Logs.err (fun m ->
              m "Discord channel error: %s" (Printexc.to_string exn));
          Lwt.return_unit));
  (match config.channels.slack with
  | Some sc when sc.socket_mode && sc.app_token <> "" ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () -> Slack_socket.start ~config ~session_manager)
            (fun exn ->
              Logs.err (fun m ->
                  m "Slack Socket Mode error: %s" (Printexc.to_string exn));
              Lwt.return_unit))
  | _ -> ());
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
                loop ()
              in
              loop ())
            (fun exn ->
              Logs.err (fun m ->
                  m "Cron scheduler error: %s" (Printexc.to_string exn));
              Lwt.return_unit));
      Logs.info (fun m -> m "Cron scheduler started")
  | None -> Logs.info (fun m -> m "Cron scheduler disabled (no database)"));
  let* () = Lwt.pick [ shutdown_waiter; gateway ] in
  write_state ~config
    ~components:[ ("gateway", "stopped"); ("telegram", "stopped") ];
  (match db with
  | Some db when config.security.audit_enabled ->
      Audit.log ~db ?signing_key
        (DaemonEvent { action = "stop"; details = "clean shutdown" })
  | _ -> ());
  (* PID file cleanup is handled by service.ml after Daemon.run returns *)
  Logs.info (fun m -> m "clawq daemon stopped");
  Lwt.return_unit
