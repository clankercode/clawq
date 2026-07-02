include Daemon_util

(* Task tree helpers extracted to Daemon_task_tree_helpers *)
let current_max_concurrent_native_agents (current_config : Runtime_config.t ref)
    =
  !current_config.agent_defaults.max_concurrent_native_agents

let refresh_active_template_tool_registries session_manager =
  match Session.get_tool_registry session_manager with
  | None -> ()
  | Some base_registry ->
      Hashtbl.iter
        (fun _ (agent, _, _) ->
          match (agent.Agent.agent_template, agent.Agent.tool_registry) with
          | Some tmpl, Some registry ->
              let refreshed =
                Agent_template.filter_tool_registry base_registry tmpl
              in
              Tool_registry.restore registry (Tool_registry.snapshot refreshed)
          | _ -> ())
        session_manager.sessions

let apply_runtime_config_reload
    ?(reconcile_room_profiles =
      fun ~db ~config -> ignore (Memory.reconcile_room_profiles ~db ~config))
    ?send_file_runtime ?(after_publish = fun () -> ()) ~source ~current_config
    ~session_manager ~sandbox ~db ~tool_registry ~new_config () =
  let old_config = !current_config in
  let old_sandbox = !sandbox in
  let old_registry = Option.map Tool_registry.snapshot tool_registry in
  try
    (* Reconcile room profile config into DB BEFORE publishing new_config so
       that on failure the old config and its derived policies remain active. *)
    (match db with
    | Some db -> reconcile_room_profiles ~db ~config:new_config
    | None -> ());
    sandbox := make_sandbox new_config;
    current_config := new_config;
    (* Re-initialize GitHub App token cache on config reload *)
    (match new_config.channels.github with
    | Some gc -> Github_app_token.init_from_config gc
    | None -> Github_app_token.invalidate_all ());
    Session.set_sandbox session_manager !sandbox;
    Session.update_config ~source session_manager new_config;
    Http_debug.sync_config new_config.log;
    (let old_sc = old_config.summarizer in
     let new_sc = new_config.summarizer in
     if old_sc <> new_sc then
       Logs.info (fun m ->
           m
             "Summarizer config updated [%s]: enabled=%b→%b, model=%s→%s, \
              threshold=%d→%d"
             source old_sc.enabled new_sc.enabled
             (Pmodel.to_string old_sc.model)
             (Pmodel.to_string new_sc.model)
             old_sc.threshold_chars new_sc.threshold_chars));
    (match tool_registry with
    | Some registry -> (
        refresh_runtime_bound_tools ?send_file_runtime ~config:new_config
          ~session_manager ~sandbox:!sandbox registry;
        match db with
        | Some db ->
            let notify =
              if new_config.agent_defaults.task_tree_notifications then
                Some
                  (Daemon_task_tree_helpers.task_tree_notify_for_session
                     session_manager)
              else None
            in
            Daemon_task_tree_helpers
            .refresh_task_tree_tools_with_current_workspace ~current_config ~db
              ?notify registry
        | None -> ())
    | None -> ());
    refresh_active_template_tool_registries session_manager;
    (* B735: validate private channel policy on config reload *)
    (match new_config.channels.slack with
    | Some s when Runtime_config.slack_has_valid_credentials s ->
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
              (fun _exn -> Lwt.return_unit))
    | _ -> ());
    after_publish ();
    Ok ()
  with exn ->
    (* Rollback to old config and sandbox on failure to preserve
       last valid policy *)
    Logs.warn (fun m ->
        m "Config reload failed [%s], rolling back to previous config: %s"
          source (Printexc.to_string exn));
    (match (tool_registry, old_registry) with
    | Some registry, Some snapshot -> Tool_registry.restore registry snapshot
    | _ -> ());
    sandbox := old_sandbox;
    current_config := old_config;
    (* Restore GitHub App token to match rolled-back config *)
    (match old_config.channels.github with
    | Some gc -> Github_app_token.init_from_config gc
    | None -> Github_app_token.invalidate_all ());
    Session.set_sandbox session_manager old_sandbox;
    Session.update_config ~source session_manager old_config;
    refresh_active_template_tool_registries session_manager;
    Http_debug.sync_config old_config.log;
    Error (Printexc.to_string exn)

let run ~(config : Runtime_config.t) =
  (Lwt.async_exception_hook :=
     fun exn ->
       let bt = Printexc.get_backtrace () in
       let bt_msg = if bt = "" then " (no backtrace)" else "\n" ^ bt in
       Logs.err (fun m ->
           m "Uncaught async exception: %s%s" (Printexc.to_string exn) bt_msg);
       Format.pp_print_flush Format.err_formatter ());
  let current_config = ref config in
  let open Lwt.Syntax in
  let is_loopback_host host =
    let h = String.lowercase_ascii (String.trim host) in
    h = "127.0.0.1" || h = "localhost" || h = "::1"
  in
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
              let mc = msg_color level in
              if mc <> "" then begin
                Format.pp_print_string dst mc;
                Format.pp_print_string dst s;
                Format.pp_print_string dst ansi_reset
              end
              else Format.pp_print_string dst s;
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
  if (not config.gateway.require_pairing) && config.gateway.auth_token = None
  then
    Logs.warn (fun m ->
        m
          "Gateway running without require_pairing or auth_token; suitable \
           only for local development on loopback");
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
  let db =
    let db_path =
      if config.memory.db_path <> "" then config.memory.db_path
      else Dot_dir.db_path ()
    in
    try
      let clawq_dir = Dot_dir.path () in
      (try if not (Sys.file_exists clawq_dir) then Sys.mkdir clawq_dir 0o755
       with _ -> ());
      let db =
        Memory.init ~db_path ~search_enabled:config.memory.search_enabled ()
      in
      Vector.init_schema db;
      Provider_quota.set_db db;
      if config.security.audit_enabled then begin
        Audit.init_schema db;
        Logs.info (fun m -> m "Audit trail enabled")
      end;
      Access_snapshot.init_schema db;
      Room_session_record.init_schema db;
      Logs.info (fun m ->
          m "SQLite memory initialized at %s (vector index enabled)" db_path);
      Some db
    with exn ->
      Logs.warn (fun m ->
          m "Failed to initialize SQLite memory: %s" (Printexc.to_string exn));
      None
  in
  (* Reconcile room profile config into DB at startup *)
  (match db with
  | Some db -> (
      try ignore (Memory.reconcile_room_profiles ~db ~config)
      with exn ->
        Logs.warn (fun m ->
            m "Room profile reconciliation failed at startup: %s"
              (Printexc.to_string exn)))
  | None -> ());
  let tool_registry =
    if config.security.tools_enabled then begin
      let registry = Tool_registry.create () in
      Tools_builtin.register_all ~config:!current_config ~sandbox:!sandbox ~db
        registry;
      let skills =
        Skills.load_all ~workspace_only:config.security.workspace_only
          ~allowed_commands:Tools_builtin.default_shell_allowlist ()
      in
      List.iter
        (fun s ->
          Tool_registry.register_skill registry s;
          Logs.info (fun m -> m "Loaded skill: %s" s.Tool.name))
        skills;
      Tool_registry.register registry (Skills.skill_create_tool ());
      let workspace = Runtime_config.effective_workspace config in
      Tool_registry.register registry
        (Skills.skill_list_tool ~workspace_dir:workspace ());
      let skill_cache = Skills.init_cache ~workspace_dir:workspace () in
      ignore (Agent_template.init_cache ~workspace_dir:workspace ());
      Tool_registry.register registry
        (Skills.use_skill_tool ~workspace_only:config.security.workspace_only ());
      Lwt.async (fun () -> Skills.skill_watcher_loop skill_cache);
      Session_turn.expand_skill_refs_fn := Skills.expand_skill_refs;
      (Agent.find_skill_for_reload_fn :=
         fun name ->
           match Skills.find_skill_md name with
           | Some s -> Some (s.meta.md_description, s.instructions)
           | None -> None);
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
  (* Register ask_user_question with ask_fn closure *)
  let runner_tokens =
    if config.mcp.runner_relay_enabled then Some (Runner_relay.create_tokens ())
    else None
  in
  let ask_fn_ref = ref None in
  (match tool_registry with
  | Some registry ->
      let notes_enabled = !current_config.interactive.enable_question_notes in
      let ask_fn ~session_key ~questions =
        let open Lwt.Syntax in
        let notify =
          match
            Session.find_alert_channel_notifier session_manager ~key:session_key
          with
          | Some n -> n
          | None -> (
              match
                Session.find_registered_notifier session_manager
                  ~key:session_key
              with
              | Some n -> n
              | None ->
                  fun _text ->
                    Lwt.fail_with
                      (Printf.sprintf "No channel notifier for session %s"
                         session_key))
        in
        (* B594/B595: only show "Add notes?" prompt when:
           1. qtype supports notes (not Text/File_upload — those already are
              free-form text, asking for notes is redundant),
           2. qtype isn't a binary/scale that almost never has notes
              (Confirm yes/no, Rating 1-5),
           3. the question explicitly opted in via notes:true in the schema.
           This stops the daemon from asking "Add notes?" after every single
           select question by default. *)
        let qtype_supports_notes = function
          | Tools_builtin.Text _ | Tools_builtin.File_upload _
          | Tools_builtin.Confirm | Tools_builtin.Rating _ ->
              false
          | _ -> true
        in
        let notes_eligible qi =
          qtype_supports_notes qi.Tools_builtin.qtype && qi.request_notes
        in
        let caps =
          Session.find_connector_capabilities session_manager ~key:session_key
        in
        let rich_notify =
          Session.find_rich_notifier session_manager ~key:session_key
        in
        let has_rich = Option.is_some rich_notify in
        let connector =
          match caps with
          | Some c -> c.Connector_capabilities.connector
          | None -> Format_adapter.Plain
        in
        let total = List.length questions in
        let cleanup_db () =
          (match db with
          | Some db -> (
              try Memory.pending_question_delete ~db ~session_key
              with exn ->
                Logs.warn (fun m ->
                    m "[%s] Failed to clean pending question from DB: %s"
                      session_key (Printexc.to_string exn)))
          | None -> ());
          Lwt.return_unit
        in
        Lwt.finalize
          (fun () ->
            let* results =
              Lwt_list.mapi_s
                (fun i qi ->
                  (match db with
                  | Some db -> (
                      try
                        Memory.pending_question_upsert ~db ~session_key
                          ~questions_json:
                            (Tools_builtin.question_items_to_json questions)
                          ~question_index:i
                      with exn ->
                        Logs.warn (fun m ->
                            m "[%s] Failed to persist pending question: %s"
                              session_key (Printexc.to_string exn)))
                  | None -> ());
                  let strategy =
                    Question_presenter.select_strategy ~capabilities:caps
                      ~has_rich_notifier:has_rich qi.Tools_builtin.qtype
                  in
                  let rendered =
                    Question_presenter.render_question ~strategy ~connector
                      ~session_key ~index:i ~total qi
                  in
                  let callback_ids = ref [] in
                  let* () =
                    match rendered with
                    | Question_presenter.RichMessage msg -> (
                        match rich_notify with
                        | Some rn ->
                            Logs.info (fun m ->
                                m "[%s] Sending rich question %d/%d" session_key
                                  (i + 1) total);
                            let cbs =
                              Question_presenter.extract_callback_answers msg
                            in
                            Session.register_question_callbacks session_manager
                              ~key:session_key ~callbacks:cbs;
                            callback_ids := List.map (fun (id, _) -> id) cbs;
                            let* _result = rn msg in
                            Lwt.return_unit
                        | None ->
                            Logs.info (fun m ->
                                m
                                  "[%s] Rich notifier unavailable, falling \
                                   back to text for question %d/%d"
                                  session_key (i + 1) total);
                            notify (Rich_message.to_fallback_text msg))
                    | Question_presenter.TextMessage text ->
                        Logs.info (fun m ->
                            m "[%s] Sending text question %d/%d" session_key
                              (i + 1) total);
                        notify text
                  in
                  let promise, _resolver =
                    Session.register_pending_question session_manager
                      ~key:session_key
                  in
                  let* raw = promise in
                  Session.clear_question_callbacks session_manager
                    ~key:session_key ~callback_ids:!callback_ids;
                  if raw = Session.question_cancelled_sentinel then
                    Lwt.fail (Failure "Question cancelled by user interrupt")
                  else
                    let* notes =
                      if notes_enabled && notes_eligible qi then begin
                        let* () = notify "Add notes? (reply or 'skip')" in
                        let notes_promise, _resolver =
                          Session.register_pending_question session_manager
                            ~key:session_key
                        in
                        let* notes_raw = notes_promise in
                        if
                          notes_raw = Session.question_cancelled_sentinel
                          || String.lowercase_ascii (String.trim notes_raw)
                             = "skip"
                        then Lwt.return_none
                        else Lwt.return_some notes_raw
                      end
                      else Lwt.return_none
                    in
                    Lwt.return
                      Tools_builtin.
                        { question = qi.question; answer = raw; notes })
                questions
            in
            Lwt.return results)
          cleanup_db
      in
      ask_fn_ref := Some ask_fn;
      Tool_registry.register registry
        (Tools_builtin.ask_user_question ~ask_fn:(Some ask_fn))
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
  (match config.channels.teams with
  | Some _ ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () -> Teams.start ~config ~_session_manager:session_manager)
            (fun exn ->
              Logs.err (fun m ->
                  m "Teams channel error: %s" (Printexc.to_string exn));
              Lwt.return_unit))
  | None -> ());
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
  (* Background model discovery refresh at startup *)
  (match db with
  | Some db ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () -> Model_discovery.maybe_refresh ~db ~config ())
            (fun exn ->
              Logs.warn (fun m ->
                  m "Model discovery startup refresh failed: %s"
                    (Printexc.to_string exn));
              Lwt.return_unit))
  | None -> ());
  (* B668: web_search backend health probe at startup. Logs a warning if the
     configured provider is broken so cron operators see it immediately
     instead of after a postmortem.
     B672: also emit a backend inventory line so operators can see at a
     glance which search fallbacks the agent has access to. *)
  let search_inventory =
    let backends = ref [] in
    (match config.web_search with
    | Some ws ->
        backends :=
          Printf.sprintf "web_search[%s]+ddg-fallback" ws.search_provider
          :: !backends
    | None -> ());
    (match config.zai_mcp with
    | Some cfg when cfg.websearch_enabled ->
        backends := "web_search_prime[zai_mcp]" :: !backends
    | _ -> ());
    (match config.zai_mcp with
    | Some cfg when cfg.webfetch_enabled ->
        backends := "web_fetch_prime[zai_mcp]" :: !backends
    | _ -> ());
    backends := "web_fetch" :: "http_get" :: !backends;
    List.rev !backends
  in
  Logs.info (fun m ->
      m "search backends registered: %s"
        (if search_inventory = [] then "(none)"
         else String.concat ", " search_inventory));
  if config.web_search <> None then
    Lwt.async (fun () ->
        Lwt.catch
          (fun () ->
            let open Lwt.Syntax in
            let* result = Tools_builtin_net.web_search_health_check ~config in
            (match result with
            | Ok msg -> Logs.info (fun m -> m "web_search health check: %s" msg)
            | Error reason ->
                Logs.warn (fun m ->
                    m "web_search health check FAILED: %s" reason));
            Lwt.return_unit)
          (fun exn ->
            Logs.warn (fun m ->
                m "web_search health check exception: %s"
                  (Printexc.to_string exn));
            Lwt.return_unit));
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
  (* Config file watcher: stat every 10s, reload on mtime change *)
  let last_config_mtime = ref 0.0 in
  let config_watch_path = Config_loader.default_path () in
  (try
     let st = Unix.stat config_watch_path in
     last_config_mtime := st.Unix.st_mtime
   with _ -> ());
  Lwt.async (fun () ->
      Lwt.catch
        (fun () ->
          let rec config_watch_loop () =
            let open Lwt.Syntax in
            let* () = Lwt_unix.sleep 10.0 in
            (try
               let st = Unix.stat config_watch_path in
               if st.Unix.st_mtime > !last_config_mtime then begin
                 match Config_loader.load_result () with
                 | Error msg ->
                     (* Do not advance [last_config_mtime]; retry next cycle. *)
                     Logs.err (fun m ->
                         m
                           "Config auto-reload failed: %s, preserving current \
                            config"
                           msg)
                 | Ok new_config -> (
                     match
                       apply_runtime_config_reload ~source:"config_file_watch"
                         ~current_config ~session_manager ~sandbox ~db
                         ~tool_registry ~send_file_runtime ~new_config ()
                     with
                     | Error msg ->
                         (* Do not advance [last_config_mtime]; retry next cycle. *)
                         Logs.err (fun m ->
                             m "Config auto-reload failed: %s" msg)
                     | Ok () ->
                         Lwt.async (fun () ->
                             Lwt.catch
                               (fun () ->
                                 Tunnel_manager.apply_config tunnel_manager
                                   ~config:new_config.tunnel
                                   ~port:new_config.gateway.port
                                   ~on_url:tunnel_on_url)
                               (fun exn ->
                                 Logs.err (fun m ->
                                     m
                                       "Tunnel reconfiguration error (file \
                                        watch): %s"
                                       (Printexc.to_string exn));
                                 Lwt.return_unit));
                         (* Handle EC process enable/disable on auto-reload *)
                         apply_ec_watcher_toggle ~new_config ~ec_state;
                         Logs.info (fun m ->
                             m "Config auto-reloaded (file changed)");
                         last_config_mtime := st.Unix.st_mtime)
               end
             with exn ->
               Logs.debug (fun m ->
                   m "Config watch stat failed: %s" (Printexc.to_string exn)));
            config_watch_loop ()
          in
          config_watch_loop ())
        (fun exn ->
          Logs.err (fun m ->
              m "Config watch loop error: %s" (Printexc.to_string exn));
          Lwt.return_unit));
  (* Background quota refresh: runs once at startup and every quota_cache_ttl_s
     seconds thereafter.  Only starts if at least one provider has
     quota_check_enabled = true. *)
  let any_quota_enabled =
    List.exists
      (fun (_, (pc : Runtime_config.provider_config)) -> pc.quota_check_enabled)
      config.providers
  in
  if any_quota_enabled then
    Lwt.async (fun () ->
        Lwt.catch
          (fun () ->
            let rec quota_refresh_loop () =
              let open Lwt.Syntax in
              let current = !current_config in
              Provider_quota.set_cache_ttl current.quota_cache_ttl_s;
              let* results = Provider_quota.refresh_all ~config:current () in
              let summaries =
                List.map Provider_quota.to_summary_string results
              in
              Logs.info (fun m ->
                  m "Quota refresh: %s" (String.concat " | " summaries));
              let* () =
                Lwt_unix.sleep (float_of_int current.quota_cache_ttl_s)
              in
              quota_refresh_loop ()
            in
            quota_refresh_loop ())
          (fun exn ->
            Logs.err (fun m ->
                m "Quota refresh loop error: %s" (Printexc.to_string exn));
            Lwt.return_unit));
  (match db with
  | Some db ->
      Scheduler.init_schema db;
      Background_task.init_schema db;
      Ambient_daemon.init_schema db;
      let recovered =
        Background_task.reap_dead_running_tasks ~db
          ~on_task_finished:
            (notify_background_task_finished ~session_manager ~config ~db)
      in
      if recovered > 0 then
        Logs.warn (fun m ->
            m
              "Recovered %d orphaned background task(s) from previous daemon \
               run"
              recovered);
      (* B736: Re-enqueue stale Local tasks that were in-progress when the
         daemon shut down. Runs after reap so only genuinely orphaned Local
         tasks are considered; external-runner readopt runs next. *)
      let re_enqueued =
        Background_task.reenqueue_stale_local_tasks ~db
          ~on_task_finished:
            (notify_background_task_finished ~session_manager ~config ~db)
      in
      if re_enqueued > 0 then
        Logs.info (fun m ->
            m "Re-enqueued %d Local background task(s) after daemon restart"
              re_enqueued);
      let readopted =
        Background_task.readopt_running_tasks ~db
          ~on_task_finished:
            (notify_background_task_finished ~session_manager ~config ~db)
      in
      if readopted > 0 then
        Logs.info (fun m ->
            m "Re-adopted %d running background task(s) from previous daemon"
              readopted);
      Lwt.async (fun () ->
          Lwt.catch
            (fun () ->
              let last_memory_cleanup = ref (Unix.gettimeofday ()) in
              let last_retention_run = ref 0.0 in
              let rec loop () =
                let open Lwt.Syntax in
                let tick_config = Session.get_config session_manager in
                let deliver ~channel ~channel_id ~text =
                  Daemon_util.dispatch_resumed_message ~config:tick_config
                    ~channel ~channel_id ~text ()
                in
                let* () =
                  Scheduler.tick ~db ~session_mgr:session_manager ~deliver ()
                in
                let now = Unix.gettimeofday () in
                let cur_config = Session.get_config session_manager in
                let* () = Ambient_daemon.tick ~db ~config:cur_config () in
                if now -. !last_memory_cleanup >= 3600.0 then begin
                  last_memory_cleanup := now;
                  let mem = cur_config.memory in
                  Logs.info (fun m -> m "Running periodic memory cleanup");
                  Memory.cleanup_all ~db
                    ~max_messages:mem.max_messages_per_session
                    ~max_age_days:mem.max_message_age_days;
                  Task_tree.maybe_purge_deleted_tasks ~db ~config:cur_config;
                  if cur_config.connector_history.enabled then
                    Memory.cleanup_connector_history ~db
                      ~max_age_days:cur_config.connector_history.max_age_days
                      ~max_messages:cur_config.connector_history.max_messages;
                  Memory.cleanup_teams_dedup ~db ~max_age_days:30;
                  let purged =
                    Summary_store.purge_older_than ~db
                      ~max_age_days:cur_config.summarizer.max_age_days
                  in
                  if purged > 0 then
                    Logs.info (fun m -> m "Purged %d expired summaries" purged)
                end;
                if
                  cur_config.security.audit_enabled
                  && now -. !last_retention_run >= 3600.0
                then begin
                  last_retention_run := now;
                  ignore (Audit.retention_tick ~db ~config:cur_config)
                end;
                (let log_path = Dot_dir.sub "daemon.log" in
                 if Log_rotation.maybe_rotate ~log_path ~config:cur_config.log
                 then Logs.info (fun m -> m "Rotated daemon.log"));
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
                  Rate_limiter.cleanup_expired teams_event_limiter
                    ~max_idle_seconds:300.0
                in
                let* () =
                  match telemetry with
                  | Some t -> Telemetry.maybe_flush t
                  | None -> Lwt.return_unit
                in
                Temp_downloads.cleanup ();
                Teams.cleanup_pending_consents ();
                (match runner_tokens with
                | Some rt -> Runner_relay.cleanup_expired rt
                | None -> ());
                let* () = Lwt_unix.sleep 60.0 in
                loop ()
              in
              loop ())
            (fun exn ->
              Logs.err (fun m ->
                  m "Cron scheduler error: %s" (Printexc.to_string exn));
              Lwt.return_unit));
      (* Periodic repo fetch loop: auto-fetches managed repos every 15 min *)
      Lwt.async (fun () ->
          Lwt.catch
            (fun () ->
              let rec repo_fetch_loop () =
                let open Lwt.Syntax in
                let* () = Lwt_unix.sleep 900.0 in
                let managed = Repo_manager.list_managed_repos ~db in
                let* () =
                  Lwt_list.iter_s
                    (fun (info : Repo_manager.repo_info) ->
                      if Sys.file_exists info.local_path then begin
                        let* result =
                          Repo_manager.fetch_repo ~path:info.local_path
                        in
                        Repo_manager.update_fetch_status ~db
                          ~session_key:info.session_key
                          ?error:
                            (match result with
                            | Error e -> Some e
                            | Ok () -> None)
                          ();
                        Lwt.return_unit
                      end
                      else Lwt.return_unit)
                    managed
                in
                repo_fetch_loop ()
              in
              repo_fetch_loop ())
            (fun exn ->
              Logs.err (fun m ->
                  m "Repo fetch loop error: %s" (Printexc.to_string exn));
              Lwt.return_unit));
      Lwt.async (fun () ->
          Lwt.catch
            (fun () ->
              let rec loop () =
                let open Lwt.Syntax in
                let queued =
                  List.filter
                    (fun (t : Background_task.task) ->
                      t.status = Background_task.Queued)
                    (Background_task.list_tasks ~db)
                in
                if queued <> [] then
                  Logs.info (fun m ->
                      m "Background task poll: %d queued task(s) pending"
                        (List.length queued));
                ignore
                  (Background_task.reap_dead_running_tasks ~db
                     ~on_task_finished:
                       (notify_background_task_finished ~session_manager ~config
                          ~db));
                ignore
                  (Background_task.readopt_running_tasks ~db
                     ~on_task_finished:
                       (notify_background_task_finished ~session_manager ~config
                          ~db));
                let () =
                  let current = !current_config in
                  let augment_env =
                    match runner_tokens with
                    | None -> None
                    | Some tokens ->
                        Some
                          (fun ~session_key ~task_id env ->
                            let token =
                              Runner_relay.generate_token tokens ~session_key
                                ~task_id ()
                            in
                            let port = current.gateway.port in
                            Array.append env
                              [|
                                "CLAWQ_RUNNER_TOKEN=" ^ token;
                                Printf.sprintf
                                  "CLAWQ_MCP_URL=http://127.0.0.1:%d/mcp" port;
                                Printf.sprintf
                                  "CLAWQ_RUNNER_ASK_URL=http://127.0.0.1:%d/runner/ask"
                                  port;
                              |])
                  in
                  Background_task.start_queued_with_local_runner ?augment_env
                    ?max_local_running_tasks:
                      (current_max_concurrent_native_agents current_config)
                    ~run_turn:(fun
                        ~key
                        ~message
                        ?model
                        ?agent_name
                        ?cwd
                        ?context_snapshot
                        ~interrupt_check
                        ~on_history_update
                        ()
                      ->
                      Daemon_util.run_local_background_turn ~session_manager
                        ~key ~message ?model ?agent_name ?cwd ?context_snapshot
                        ~interrupt_check ~on_history_update ())
                    ~db
                    ~on_task_finished:
                      (notify_background_task_finished ~session_manager ~config
                         ~db)
                    ~on_task_started:
                      (notify_background_task_started ~session_manager ~config
                         ~db)
                    ()
                in
                let* () =
                  Lwt.choose
                    [
                      Lwt_condition.wait Background_task_db.enqueue_condition;
                      Lwt_unix.sleep 5.0;
                    ]
                in
                loop ()
              in
              loop ())
            (fun exn ->
              Logs.err (fun m ->
                  m "Background task loop error: %s" (Printexc.to_string exn));
              Lwt.return_unit));
      Logs.info (fun m -> m "Cron scheduler started")
  | None -> Logs.info (fun m -> m "Cron scheduler disabled (no database)"));
  let hb = config.heartbeat in
  Logs.info (fun m ->
      m "Heartbeat loop started: enabled=%b interval=%ds quiet=%d:00-%d:00"
        hb.enabled hb.interval_seconds hb.quiet_start hb.quiet_end);
  Lwt.async (fun () ->
      Lwt.catch
        (fun () ->
          let rec hb_loop () =
            let open Lwt.Syntax in
            let cur_hb = (Session.get_config session_manager).heartbeat in
            let* () = Lwt_unix.sleep (float_of_int cur_hb.interval_seconds) in
            let cur_hb = (Session.get_config session_manager).heartbeat in
            if not cur_hb.enabled then begin
              Logs.debug (fun m -> m "Heartbeat: disabled, skipping tick");
              hb_loop ()
            end
            else
              let tm = Unix.localtime (Unix.gettimeofday ()) in
              let hour = tm.Unix.tm_hour in
              let in_quiet =
                if cur_hb.quiet_start > cur_hb.quiet_end then
                  hour >= cur_hb.quiet_start || hour < cur_hb.quiet_end
                else hour >= cur_hb.quiet_start && hour < cur_hb.quiet_end
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
                      Fun.protect
                        ~finally:(fun () -> close_in_noerr ic)
                        (fun () ->
                          let n = in_channel_length ic in
                          let buf = Bytes.create n in
                          really_input ic buf 0 n;
                          String.trim (Bytes.to_string buf))
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
                      let keys =
                        Session.list_heartbeat_session_keys session_manager
                      in
                      let* () =
                        if keys = [] then begin
                          Logs.debug (fun m ->
                              m "Heartbeat: no opted-in sessions, skipping tick");
                          Lwt.return_unit
                        end
                        else
                          Lwt_list.iter_s
                            (fun key ->
                              let* result =
                                Lwt.catch
                                  (fun () ->
                                    Logs.info (fun m ->
                                        m
                                          "Heartbeat: processing HEARTBEAT.md \
                                           (%d chars) on %s"
                                          (String.length content) key);
                                    let* result =
                                      Session.with_suppressed_channel_output
                                        session_manager ~key (fun () ->
                                          Session.try_turn session_manager ~key
                                            ~message:content ())
                                    in
                                    match result with
                                    | Some response -> Lwt.return_some response
                                    | None ->
                                        Logs.info (fun m ->
                                            m
                                              "Heartbeat: session %s busy, \
                                               skipping this tick"
                                              key);
                                        Lwt.return_none)
                                  (fun exn ->
                                    Logs.err (fun m ->
                                        m "Heartbeat error for %s: %s" key
                                          (Printexc.to_string exn));
                                    Lwt.return_none)
                              in
                              match result with
                              | None -> Lwt.return_unit
                              | Some response ->
                                  handle_heartbeat_response ~session_manager
                                    ~key ~response ())
                            keys
                      in
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
          Lwt.return_unit));
  (* B719: Periodic status emitter for running subagents *)
  (match db with
  | Some db ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () -> Subagent_tool.run_subagent_status_loop ~db ())
            (fun exn ->
              Logs.err (fun m ->
                  m "Subagent status loop error: %s" (Printexc.to_string exn));
              Lwt.return_unit));
      Logs.info (fun m -> m "Subagent status loop started")
  | None -> Logs.info (fun m -> m "Subagent status loop disabled (no database)"));
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
