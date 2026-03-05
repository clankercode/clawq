let write_state ~(config : Runtime_config.t) ~components =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  let state_dir = Filename.concat home ".clawq" in
  let state_path = Filename.concat state_dir "daemon_state.json" in
  (try
     if not (Sys.file_exists state_dir) then Sys.mkdir state_dir 0o755
   with _ -> ());
  let json =
    `Assoc
      [
        ( "components",
          `Assoc
            (List.map
               (fun (name, status) ->
                 (name, `String status))
               components) );
        ("gateway_port", `Int config.gateway.port);
        ("gateway_host", `String config.gateway.host);
        ( "telegram_enabled",
          `Bool (config.channels.telegram <> None) );
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
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  Logs.info (fun m -> m "clawq daemon starting (pid=%d)" (Unix.getpid ()));
  let tool_registry =
    if config.security.tools_enabled then begin
      let registry = Tool_registry.create () in
      Tools_builtin.register_all ~config registry;
      Logs.info (fun m -> m "Tools enabled, registered built-in tools");
      Some registry
    end else begin
      Logs.info (fun m -> m "Tools disabled (set security.tools_enabled to enable)");
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
      (try
         if not (Sys.file_exists clawq_dir) then Sys.mkdir clawq_dir 0o755
       with _ -> ());
      let db = Memory.init ~db_path in
      Logs.info (fun m -> m "SQLite memory initialized at %s" db_path);
      Some db
    with exn ->
      Logs.warn (fun m ->
          m "Failed to initialize SQLite memory: %s" (Printexc.to_string exn));
      None
  in
  let session_manager = Session.create ~config ?tool_registry ?db () in
  write_state ~config
    ~components:[ ("gateway", "starting"); ("telegram", "starting") ];
  let gateway =
    Lwt.catch
      (fun () ->
        Http_server.start ~port:config.gateway.port ~host:config.gateway.host
          ~session_manager)
      (fun exn ->
        Logs.err (fun m ->
            m "Gateway server error: %s" (Printexc.to_string exn));
        Lwt.return_unit)
  in
  let telegram =
    Lwt.catch
      (fun () ->
        Telegram.start_polling ~config ~session_manager)
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
  write_state ~config
    ~components:[ ("gateway", "running"); ("telegram", "running") ];
  Logs.info (fun m ->
      m "Daemon ready. Gateway on %s:%d" config.gateway.host
        config.gateway.port);
  Lwt.async (fun () -> telegram);
  let* () = Lwt.pick [ shutdown_waiter; gateway ] in
  write_state ~config
    ~components:[ ("gateway", "stopped"); ("telegram", "stopped") ];
  Logs.info (fun m -> m "clawq daemon stopped");
  Lwt.return_unit
