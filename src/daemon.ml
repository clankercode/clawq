include Daemon_util

let current_max_concurrent_native_agents =
  Daemon_runtime_loops.current_max_concurrent_native_agents

let apply_runtime_config_reload =
  Daemon_config_reload.apply_runtime_config_reload

let run ~(config : Runtime_config.t) =
  Daemon_logging.install ();
  let current_config = ref config in
  let open Lwt.Syntax in
  if (not config.gateway.require_pairing) && config.gateway.auth_token = None
  then
    Logs.warn (fun m ->
        m
          "Gateway running without require_pairing or auth_token; suitable \
           only for local development on loopback");
  if
    (not (Daemon_logging.is_loopback_host config.gateway.host))
    && config.gateway.auth_token = None
  then
    failwith
      (Printf.sprintf
         "Refusing to bind gateway.host=%S without gateway.auth_token.\n\
          To keep the gateway loopback-only, set gateway.host to 127.0.0.1 (or \
          localhost / ::1).\n\
          Example: clawq config set gateway.host 127.0.0.1\n\
          To allow non-loopback binding, set gateway.auth_token in %s or run: \
          clawq config set gateway.auth_token YOUR_TOKEN"
         (Dot_dir.config_path ()) config.gateway.host);
  Logs.info (fun m ->
      m "clawq %s starting (pid=%d build=%s)" Build_info.version_string
        (Unix.getpid ()) Build_info.version_string);
  let workspace = Runtime_config.effective_workspace config in
  Workspace_scaffold.ensure_dir workspace;
  let active_provider, _, active_model = Provider.select_provider ~config () in
  Logs.info (fun m ->
      m "Provider: %s | Model: %s | Temp: %.2f" active_provider active_model
        config.default_temperature);
  Logs.info (fun m -> m "Workspace: %s" workspace);
  Logs.info (fun m ->
      m
        "Channels: cli=%b telegram=%b discord=%b slack=%b github=%b signal=%b \
         matrix=%b irc=%b email=%b nostr=%b dingtalk=%b onebot=%b lark=%b \
         teams=%b mattermost=%b imessage=%b whatsapp=%b line=%b"
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
        | None -> false)
        (config.channels.teams <> None)
        (config.channels.mattermost <> None)
        (config.channels.imessage <> None)
        (config.channels.whatsapp <> None)
        (config.channels.line <> None));
  let sandbox = ref (make_sandbox config) in
  Logs.info (fun m ->
      m "Sandbox backend: %s"
        (Sandbox.backend_to_string !sandbox.Sandbox.backend));
  let db = Daemon_startup.init_database ~config in
  Daemon_startup.reconcile_room_profiles_at_startup ~db ~config;
  let tool_registry =
    Daemon_startup.init_tool_registry ~config ~current_config ~sandbox:!sandbox
      ~db
  in
  (* Connect configured MCP clients and register their tools *)
  let mcp_clients = ref [] in
  let* () = run_mcp_setup_stage ~tool_registry ~config ~mcp_clients () in
  (* Auto-hydrate core memories from snapshot if db is empty *)
  (match db with
  | Some db ->
      let snapshot_path = Dot_dir.sub "memory_snapshot.json" in
      if Sys.file_exists snapshot_path then begin
        let count = Memory.count_core ~db in
        if count = 0 then begin
          Logs.info (fun m ->
              m "Auto-hydrating core memories from %s" snapshot_path);
          try
            let imported = Memory.import_snapshot ~db ~path:snapshot_path in
            Logs.info (fun m ->
                m "Auto-hydrated %d core memories from snapshot" imported)
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
  let teams_event_limiter =
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
    Session.create ~config:!current_config ?tool_registry ~sandbox:!sandbox
      ~landlock_enabled ?db ()
  in
  (match tool_registry with
  | Some registry ->
      refresh_runtime_bound_tools ~config:!current_config ~session_manager
        ~sandbox:!sandbox registry
  | None -> ());
  let rich_send_fn =
    Some
      (fun ~session_key content ->
        match Session.find_rich_notifier session_manager ~key:session_key with
        | Some notifier -> notifier content
        | None -> (
            match
              Session.find_registered_notifier session_manager ~key:session_key
            with
            | Some text_notify ->
                let open Lwt.Syntax in
                let text = Rich_message.to_fallback_text content in
                let* () = text_notify text in
                Lwt.return Rich_message.{ message_id = "0"; callback_ids = [] }
            | None -> (
                (* Fallback: direct channel dispatch when notifier not yet
                   registered (e.g., post-restart before first inbound message) *)
                match Restart_notify.parse_channel_from_key session_key with
                | Some (channel, channel_id) -> (
                    let text = Rich_message.to_fallback_text content in
                    let config = !current_config in
                    let open Lwt.Syntax in
                    let* result =
                      dispatch_resumed_message ~config ~channel ~channel_id
                        ~text ()
                    in
                    match result with
                    | Ok () ->
                        Logs.info (fun m ->
                            m
                              "rich_send_fn: delivered via direct %s dispatch \
                               (no notifier registered) session=%s"
                              channel session_key);
                        Lwt.return
                          Rich_message.{ message_id = "0"; callback_ids = [] }
                    | Error err ->
                        Lwt.fail_with
                          (Printf.sprintf
                             "No notifier registered for session %s and direct \
                              %s dispatch failed: %s"
                             session_key channel err))
                | None ->
                    Lwt.fail_with
                      (Printf.sprintf
                         "No notifier registered for session %s and cannot \
                          parse channel from key"
                         session_key))))
  in
  let channel_send_fn =
    Some (fun ~text -> Session.notify_channel_sessions session_manager text)
  in
  let store_file =
    Some
      (fun ~content ~content_type ~filename ->
        Temp_downloads.download_url
          (Temp_downloads.add ~content ~content_type ~filename ~ttl_s:3600.0))
  in
  let send_file_runtime =
    { send_fn = channel_send_fn; rich_send_fn; store_file }
  in
  (match tool_registry with
  | Some registry ->
      Tool_registry.register registry
        (Tools_builtin.send_message ~rich_send_fn ~send_fn:channel_send_fn);
      Tool_registry.register registry
        (Tools_builtin.send_poll ~rich_send_fn ~send_fn:channel_send_fn);
      Tool_registry.register registry
        (Tools_builtin.send_file
           ~workspace:(Runtime_config.effective_workspace !current_config)
           ~workspace_only:config.security.workspace_only
           ~extra_allowed_paths:config.security.extra_allowed_paths
           ~rich_send_fn ~send_fn:channel_send_fn ~store_file);
      Tool_registry.register registry
        (Tools_builtin.compact_history ~compact_fn:(fun ~session_key ->
             match Hashtbl.find_opt session_manager.sessions session_key with
             | None -> Lwt.return "Error: session not found"
             | Some (agent, _mutex, _interrupt) -> (
                 let open Lwt.Syntax in
                 let on_llm_call_debug =
                   Session.debug_callback_for session_manager ~key:session_key
                     (Session.find_registered_notifier session_manager
                        ~key:session_key)
                 in
                 Agent.refresh_profiled_room_flag agent ?db:session_manager.db
                   ~session_key ();
                 let* info =
                   Agent.force_compact_history agent ?db:session_manager.db
                     ?on_llm_call_debug ()
                 in
                 match info with
                 | Some info ->
                     (* Set the flag so session.ml persists the full compacted
                        state (including the compact_history tool result that
                        hasn't been appended yet) at turn completion.  Do NOT
                        call persist_compacted_history here — that would write
                        history before the tool result is in agent.history,
                        leaving an orphaned tool call in the DB if the
                        subsequent LLM call fails. *)
                     agent.Agent.compacted_mid_turn <- true;
                     Lwt.return
                       (Printf.sprintf
                          "Compacted: %dk -> %dk tokens (context window: %dk)"
                          (info.pre_tokens / 1000) (info.post_tokens / 1000)
                          (info.context_window / 1000))
                 | None ->
                     Lwt.return
                       "Nothing to compact (history too short or already \
                        compacted)")))
  | None -> ());
  let runner_tokens =
    if config.mcp.runner_relay_enabled then Some (Runner_relay.create_tokens ())
    else None
  in
  let ask_fn_ref = ref None in
  (match tool_registry with
  | Some registry ->
      ask_fn_ref :=
        Some
          (Daemon_questions.register_tool ~config:!current_config
             ~session_manager ~db registry)
  | None -> ());
  (* Re-register task_tree tools with current workspace and optional channel notifications. *)
  (match (tool_registry, db) with
  | Some registry, Some db ->
      let notify =
        if !current_config.agent_defaults.task_tree_notifications then
          Some
            (Daemon_task_tree_helpers.task_tree_notify_for_session
               session_manager)
        else None
      in
      Daemon_task_tree_helpers.refresh_task_tree_tools_with_current_workspace
        ~current_config ~db ?notify registry
  | _ -> ());
  let update_lock = Lwt_mutex.create () in
  let update_in_progress = ref false in
  let claim_update () =
    Lwt_util.with_lock_timeout ~label:"update_claim"
      ~fatal_timeout:Lwt_util.short_fatal_timeout update_lock (fun () ->
        if !update_in_progress || Session.is_draining session_manager then
          Lwt.return false
        else begin
          update_in_progress := true;
          Lwt.return true
        end)
  in
  let finish_update () =
    Lwt_util.with_lock_timeout ~label:"update_finish"
      ~fatal_timeout:Lwt_util.short_fatal_timeout update_lock (fun () ->
        update_in_progress := false;
        Lwt.return_unit)
  in
  let run_update ?(mode = Update_tool.Auto) ?prepare_restart ~send_progress
      ~interrupt_check () =
    Update_tool.run_update ~mode ?prepare_restart ~claim_update ~finish_update
      ~is_draining:(fun () -> Session.is_draining session_manager)
      ~send_progress ~interrupt_check ()
  in
  let run_update_command ?(mode = Update_tool.Auto) ?prepare_restart
      ~send_progress () =
    run_update ~mode ?prepare_restart ~send_progress ~interrupt_check:None ()
  in
  (match tool_registry with
  | Some registry ->
      Tool_registry.register registry
        (Update_tool.tool ~claim_update ~finish_update
           ~session_model_override:(fun key ->
             Session.get_session_model_override session_manager ~key)
           ~is_draining:(fun () -> Session.is_draining session_manager)
           ())
  | None -> ());
  Session.set_special_command_handler session_manager
    (fun ~key ~message ~send_progress ~interrupt_check ->
      if not (Update_tool.is_update_command message) then Lwt.return_none
      else
        let send_progress =
          match send_progress with
          | Some f -> f
          | None -> fun _ -> Lwt.return_unit
        in
        let prepare_restart () =
          (match Restart_notify.parse_channel_from_key key with
          | Some (channel, channel_id) -> (
              match Session.get_session_model_override session_manager ~key with
              | Some model ->
                  Restart_notify.write_session ~channel ~channel_id
                    ~session_key:key ~model
              | None ->
                  Restart_notify.write_session_key ~channel ~channel_id
                    ~session_key:key)
          | None -> ());
          Lwt.return (Ok ())
        in
        let open Lwt.Syntax in
        let* response =
          run_update ~prepare_restart ~send_progress ~interrupt_check ()
        in
        Lwt.return_some response);
  let* () =
    Lwt.catch
      (fun () ->
        let marker =
          match Sys.getenv_opt Restart_notify.env_key with
          | Some raw when String.trim raw <> "" -> (
              match Restart_notify.marker_from_json_string raw with
              | Some marker -> Some marker
              | None -> Restart_notify.read_marker ())
          | _ -> Restart_notify.read_marker ()
        in
        match marker with
        | Some marker ->
            Restart_notify.remove ();
            let open Lwt.Syntax in
            let* () =
              match (marker.session_key, marker.model) with
              | Some key, Some model ->
                  Session.set_session_model_with_compact session_manager ~key
                    ~model
                  |> Lwt.map (fun _ -> ())
              | _, _ -> Lwt.return_unit
            in
            let text =
              Printf.sprintf "clawq updated and restarted successfully (%s)."
                Build_info.version_string
            in
            let open Lwt.Syntax in
            let* result =
              dispatch_resumed_message ~config ~channel:marker.channel
                ~channel_id:marker.channel_id ~text ()
            in
            (match result with
            | Ok () ->
                Logs.info (fun m ->
                    m "Sent post-update notification to %s:%s" marker.channel
                      marker.channel_id)
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
  let tunnel_manager = Tunnel_manager.create () in
  let update_daemon_state_tunnel_ref = ref (fun () -> ()) in
  let tunnel_on_url url_opt =
    tunnel_url_ref := url_opt;
    Temp_downloads.public_base_url := url_opt;
    !update_daemon_state_tunnel_ref ();
    match url_opt with
    | Some url -> (
        Logs.info (fun m -> m "Tunnel URL: %s" url);
        let cur = !current_config in
        match cur.channels.github with
        | Some _ ->
            Logs.info (fun m ->
                m "GitHub webhooks ready at: %s/github/webhook/..." url)
        | None -> ())
    | None -> Logs.info (fun m -> m "Tunnel stopped")
  in
  (* Pre-set public base URL from static tunnel config if available *)
  if config.tunnel.enabled && String.trim config.tunnel.url <> "" then
    Temp_downloads.public_base_url := Some config.tunnel.url;
  (Prompt_builder.tunnel_status_line_fn :=
     fun () ->
       let cur = !current_config in
       if not cur.tunnel.enabled then "not configured"
       else
         match !tunnel_url_ref with
         | Some url -> url
         | None -> (
             match tunnel_manager.Tunnel_manager.state with
             | Tunnel_manager.Active _ -> "up (unknown url)"
             | Tunnel_manager.Idle -> "down"));
  Lwt.async (fun () ->
      Lwt.catch
        (fun () ->
          Tunnel_manager.apply_config tunnel_manager ~config:config.tunnel
            ~port:config.gateway.port ~on_url:tunnel_on_url)
        (fun exn ->
          Logs.err (fun m ->
              m "Initial tunnel apply error: %s" (Printexc.to_string exn));
          Lwt.return_unit));
  Tunnel_manager.set_daemon_hooks
    ~status:(fun () ->
      Yojson.Safe.pretty_to_string (Tunnel_manager.status_json tunnel_manager))
    ~apply:(fun () ->
      let open Lwt.Syntax in
      let cur = !current_config in
      let* () =
        Tunnel_manager.apply_config tunnel_manager ~config:cur.tunnel
          ~port:cur.gateway.port ~on_url:tunnel_on_url
      in
      Lwt.return
        (Yojson.Safe.pretty_to_string
           (Tunnel_manager.status_json tunnel_manager)))
    ~restart:(fun () ->
      let open Lwt.Syntax in
      let cur = !current_config in
      let* () =
        Tunnel_manager.restart tunnel_manager ~config:cur.tunnel
          ~port:cur.gateway.port ~on_url:tunnel_on_url
      in
      Lwt.return
        (Yojson.Safe.pretty_to_string
           (Tunnel_manager.status_json tunnel_manager)));
  if config.channels.github <> None && not config.tunnel.enabled then
    Logs.warn (fun m ->
        m
          "GitHub channel configured but tunnel is disabled; webhooks may not \
           be reachable");
  (* Initialize GitHub App token cache if App auth is configured *)
  (match config.channels.github with
  | Some gc -> Github_app_token.init_from_config gc
  | None -> ());
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
    let tunnel_json = Some (Tunnel_manager.status_json tunnel_manager) in
    write_state ~pairing_code ~tunnel_json ~config ~components
  in
  (update_daemon_state_tunnel_ref :=
     fun () ->
       let state_path = Filename.concat (Dot_dir.path ()) "daemon_state.json" in
       try
         let json = Yojson.Safe.from_file state_path in
         let tunnel_json = Tunnel_manager.status_json tunnel_manager in
         let updated =
           match json with
           | `Assoc fields ->
               `Assoc
                 (("tunnel", tunnel_json)
                 :: List.filter (fun (k, _) -> k <> "tunnel") fields)
           | other -> other
         in
         let oc = open_out state_path in
         Fun.protect
           ~finally:(fun () -> close_out oc)
           (fun () ->
             output_string oc (Yojson.Safe.pretty_to_string updated);
             output_char oc '\n')
       with _ -> ());
  Logs.info (fun m ->
      m "Web UI assets ready at %s (version=%s dev_mode=%b)" ui_server.ui_dir
        (Ui_server.version ui_server)
        ui_server.dev_mode);
  let discord_creds_ok =
    match config.channels.discord with
    | Some d -> Runtime_config.discord_has_valid_credentials d
    | None -> false
  in
  let slack_creds_ok =
    match config.channels.slack with
    | Some s -> Runtime_config.slack_has_valid_credentials s
    | None -> false
  in
  (* B735: validate private channel policy at startup *)
  (match config.channels.slack with
  | Some s when slack_creds_ok ->
      Lwt.async (fun () ->
          let open Lwt.Syntax in
          Lwt.catch
            (fun () ->
              let* warnings =
                Slack.validate_private_channels_in_allowlist
                  ~bot_token:s.bot_token ~allow_channels:s.allow_channels
                  ~private_channel_policy:s.private_channel_policy
                  ~allow_private_channels:s.allow_private_channels
              in
              List.iter (fun w -> Logs.warn (fun m -> m "%s" w)) warnings;
              Lwt.return_unit)
            (fun exn ->
              Logs.warn (fun m ->
                  m "Slack private channel validation failed: %s"
                    (Printexc.to_string exn));
              Lwt.return_unit))
  | _ -> ());
  write_runtime_state
    ~components:
      [
        ("gateway", "starting");
        ("telegram", "starting");
        ("discord", if discord_creds_ok then "starting" else "disabled");
        ("slack", if slack_creds_ok then "starting" else "disabled");
        ("signal", "starting");
        ("matrix", "starting");
        ("irc", "starting");
        ("email", "starting");
        ("nostr", "starting");
        ("dingtalk", "starting");
        ("onebot", "starting");
        ("lark", "starting");
        ("teams", "starting");
        ("mattermost", "starting");
        ("imessage", "starting");
        ("whatsapp", "starting");
        ("line", "starting");
      ];
  let gateway_stop, stop_gateway = Lwt.wait () in
  let gateway =
    Lwt.catch
      (fun () ->
        Http_server.start ~port:config.gateway.port ~host:config.gateway.host
          ~require_pairing:config.gateway.require_pairing
          ~auth_token:config.gateway.auth_token ~session_manager
          ~daemon_run_update_command:(fun ~mode ~send_progress () ->
            run_update_command ~mode ~send_progress ())
          ?slack_config:config.channels.slack
          ?github_config:config.channels.github ~config ~github_api_limiter
          ~ip_limiter ~session_limiter ~slack_event_limiter ~teams_event_limiter
          ?web_channel:web_channel_handler
          ~slack_run_update_command:run_update_command
          ?whatsapp_config:config.channels.whatsapp
          ?line_config:config.channels.line
          ?lark_config:
            (match config.channels.lark with
            | Some lc when lc.enabled && lc.mode = "webhook" -> Some lc
            | _ -> None)
          ?teams_config:config.channels.teams ?pairing ?runner_tokens
          ?ask_fn:!ask_fn_ref ~ui_server ~stop:gateway_stop ())
      (fun exn ->
        Logs.err (fun m ->
            m "Gateway server error: %s" (Printexc.to_string exn));
        Lwt.return_unit)
  in
  let telegram_stop_waiter, telegram_stop_resolver = Lwt.wait () in
  let telegram =
    Lwt.catch
      (fun () ->
        Telegram_poll.start_polling ~config ~session_manager ~run_update_command
          ~chat_limiter ~stop:telegram_stop_waiter ())
      (fun exn ->
        Logs.err (fun m ->
            m "Telegram polling error: %s" (Printexc.to_string exn));
        Lwt.return_unit)
  in
  let ec_state = Error_watcher.create_state () in
  let shutdown_waiter, shutdown_resolver = Lwt.wait () in
  let restart_waiter, restart_resolver = Lwt.wait () in
  let shutting_down = ref false in
  let restarting = ref false in
  let deadlock_restart = ref false in
  (Lwt_util.on_fatal_timeout :=
     fun label ->
       Logs.err (fun m -> m "Mutex deadlock on [%s], triggering restart" label);
       if not !restarting then begin
         deadlock_restart := true;
         restarting := true;
         Lwt.wakeup_later restart_resolver ()
       end);
  let last_restart_signal_at =
    ref
      (match Sys.getenv_opt restart_signal_ts_env with
      | Some raw -> float_of_string_opt (String.trim raw)
      | None -> None)
  in
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
      let now = Unix.gettimeofday () in
      match !last_restart_signal_at with
      | Some last -> (
          match restart_signal_duplicate_delta ~now ~last_signal_at:last with
          | Some delta ->
              Logs.warn (fun m ->
                  m
                    "Ignoring duplicate SIGUSR1 restart signal %.3fs after \
                     previous restart handoff"
                    delta)
          | None ->
              last_restart_signal_at := Some now;
              Unix.putenv restart_signal_ts_env (Printf.sprintf "%.6f" now);
              restarting := true;
              Logs.info (fun m ->
                  m "SIGUSR1 received, initiating graceful restart");
              write_runtime_state
                ~components:
                  [ ("gateway", "restarting"); ("telegram", "restarting") ];
              Lwt.wakeup_later restart_resolver ())
      | None ->
          last_restart_signal_at := Some now;
          Unix.putenv restart_signal_ts_env (Printf.sprintf "%.6f" now);
          restarting := true;
          Logs.info (fun m -> m "SIGUSR1 received, initiating graceful restart");
          write_runtime_state
            ~components:
              [ ("gateway", "restarting"); ("telegram", "restarting") ];
          Lwt.wakeup_later restart_resolver ()
    end
  in
  let _ = Lwt_unix.on_signal Sys.sigint do_shutdown in
  let _ = Lwt_unix.on_signal Sys.sigterm do_shutdown in
  let _ = Lwt_unix.on_signal Sys.sigusr1 do_restart in
  let _ =
    Lwt_unix.on_signal Sys.sighup (fun _ ->
        Logs.info (fun m -> m "SIGHUP received, reloading config...");
        match Config_loader.load_result () with
        | Error msg ->
            Logs.err (fun m ->
                m "Config reload failed: %s, preserving current config" msg)
        | Ok new_config -> (
            match
              apply_runtime_config_reload ~source:"config_reload"
                ~current_config ~session_manager ~sandbox ~db ~tool_registry
                ~send_file_runtime ~new_config ()
            with
            | Error msg -> Logs.err (fun m -> m "Config reload failed: %s" msg)
            | Ok () ->
                Lwt.async (fun () ->
                    Lwt.catch
                      (fun () ->
                        Tunnel_manager.apply_config tunnel_manager
                          ~config:new_config.tunnel
                          ~port:new_config.gateway.port ~on_url:tunnel_on_url)
                      (fun exn ->
                        Logs.err (fun m ->
                            m "Tunnel reconfiguration error: %s"
                              (Printexc.to_string exn));
                        Lwt.return_unit));
                (* Handle EC process enable/disable on config reload *)
                apply_ec_watcher_toggle ~new_config ~ec_state;
                Logs.info (fun m -> m "Config reloaded successfully")))
  in
  let slack_socket_enabled =
    match config.channels.slack with
    | Some sc
      when sc.socket_mode && Runtime_config.is_credential_valid sc.app_token ->
        true
    | _ -> false
  in
  write_runtime_state
    ~components:
      ([
         ("gateway", "running");
         ("telegram", "running");
         ("discord", if discord_creds_ok then "running" else "disabled");
         ("slack", if slack_creds_ok then "running" else "disabled");
         ("cron", "running");
         ("signal", "running");
         ("matrix", "running");
         ("irc", "running");
         ("email", "running");
         ("nostr", "running");
         ("dingtalk", "running");
         ("onebot", "running");
         ("lark", "running");
         ("teams", "running");
         ("mattermost", "running");
         ("imessage", "running");
         ("whatsapp", "running");
         ("line", "running");
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
  Daemon_channels.start_non_telegram_channels ~config ~session_manager ~db
    ~discord_message_limiter ~slack_event_limiter;
  let* _resume_summary =
    run_pending_session_resume_stage ~session_manager ~config ()
  in
  let* _replay_summary = run_durable_replay_stage ~session_manager ~config () in
  (* Replay any pending questions that survived a restart *)
  (match !ask_fn_ref with
  | Some ask_fn -> replay_pending_questions ~session_manager ~ask_fn ()
  | None -> ());
  (* Keepalive periodic loop: nudges idle keepalive-enabled sessions every 15m *)
  (match db with
  | Some db ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () -> run_keepalive_loop ~db ~session_manager)
            (fun exn ->
              Logs.err (fun m ->
                  m "Keepalive loop error: %s" (Printexc.to_string exn));
              Lwt.return_unit))
  | None -> ());
  Daemon_background_loops.start_model_discovery_refresh ~db ~config;
  (* B668: web_search backend health probe at startup. Logs a warning if the
     configured provider is broken so cron operators see it immediately
     instead of after a postmortem.
     B672: also emit a backend inventory line so operators can see at a
     glance which search fallbacks the agent has access to. *)
  Daemon_background_loops.log_search_inventory ~config;
  Daemon_background_loops.start_web_search_health_check ~config;
  (* Error Correction watcher process *)
  if config.error_watcher.enabled then begin
    Logs.info (fun m -> m "Starting Error Correction watcher process");
    (try Error_watcher.start_ec_process ec_state
     with exn ->
       Logs.err (fun m ->
           m "Failed to start EC process: %s" (Printexc.to_string exn)));
    Lwt.async (fun () ->
        Lwt.catch
          (fun () ->
            Error_watcher.run_health_check_loop ~shutdown:shutdown_waiter
              ec_state)
          (fun exn ->
            Logs.err (fun m ->
                m "EC health check loop error: %s" (Printexc.to_string exn));
            Lwt.return_unit))
  end;
  Daemon_config_watch.start ~current_config ~session_manager ~sandbox ~db
    ~tool_registry ~send_file_runtime ~tunnel_manager ~tunnel_on_url ~ec_state
    ();
  Daemon_background_loops.start_quota_refresh ~config ~current_config;
  Daemon_runtime_loops.start ~db ~config ~current_config ~session_manager
    ~ip_limiter ~session_limiter ~chat_limiter ~discord_message_limiter
    ~slack_event_limiter ~teams_event_limiter ~telemetry ~runner_tokens;
  Daemon_heartbeat.start ~config ~workspace ~session_manager;
  Daemon_background_loops.start_subagent_status_loop ~db;
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
        if Lwt.is_sleeping telegram_stop_waiter then
          Lwt.wakeup_later telegram_stop_resolver ();
        let* () = gateway in
        let* () = telegram in
        Lwt.return Shutdown
    | Restart ->
        (* Stop Telegram poller early so the in-flight long-poll request is
           cancelled before we exec the new process. This prevents the new
           process from getting a 409 Conflict from the Telegram API. *)
        if Lwt.is_sleeping telegram_stop_waiter then
          Lwt.wakeup_later telegram_stop_resolver ();
        let* () =
          Session.interrupt_resumable_channel_sessions session_manager
        in
        let* () = Session.start_draining session_manager in
        let* () =
          Session.notify_channel_sessions session_manager initial_drain_warning
        in
        let stop_warnings = ref false in
        let warnings_p =
          Lwt.catch
            (fun () -> send_drain_warnings ~stop:stop_warnings ())
            (fun exn ->
              Logs.warn (fun m ->
                  m "Drain warning loop failed: %s" (Printexc.to_string exn));
              Lwt.return_unit)
        in
        Lwt.async (fun () -> warnings_p);
        let drain_attempts = if !deadlock_restart then 150 else 600 in
        let* timed_out =
          wait_for_drain ~attempts:drain_attempts ~session_manager ()
        in
        stop_warnings := true;
        let* () = if timed_out then warnings_p else Lwt.return_unit in
        Logs.info (fun m -> m "Draining complete, stopping gateway for restart");
        Lwt.wakeup_later stop_gateway ();
        let* () = gateway in
        let* () = telegram in
        Logs.info (fun m ->
            m "Gateway and Telegram stopped; proceeding with restart exec");
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
  (* Stop EC process *)
  let* () =
    if ec_state.pid <> None then begin
      Logs.info (fun m -> m "Stopping EC process");
      if final_intent = Restart then Error_watcher.kill_ec_process ec_state
      else Error_watcher.stop_ec_process ec_state
    end
    else Lwt.return_unit
  in
  (* Stop tunnel manager *)
  let* () = Tunnel_manager.stop tunnel_manager in
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
  (* Close all browser sessions *)
  let* () = Cdp_client.close_all () in
  (* PID file cleanup is handled by service.ml after Daemon.run returns *)
  Logs.info (fun m ->
      m "clawq daemon %s"
        (if final_intent = Restart then "ready to restart" else "stopped"));
  (Lwt_util.on_fatal_timeout := fun _ -> ());
  Lwt.return final_intent
