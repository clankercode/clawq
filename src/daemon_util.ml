let write_state ~pairing_code ~(tunnel_json : Yojson.Safe.t option)
    ~(config : Runtime_config.t) ~components =
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
      ("teams_enabled", `Bool (config.channels.teams <> None));
      ("pid", `Int (Unix.getpid ()));
    ]
  in
  let fields =
    match pairing_code with
    | Some code -> ("pairing_code", `String code) :: fields
    | None -> fields
  in
  let fields =
    match tunnel_json with
    | Some tj -> ("tunnel", tj) :: fields
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
        Telegram.send_message ~disable_notification:true ~bot_token ~chat_id
          ~text ());
    send_discord = Discord.send_message;
    send_slack = Slack.send_message;
  }

type boot_resume_summary = {
  pending_count : int;
  resumed_count : int;
  missing_channel_count : int;
  failed_count : int;
}

type boot_replay_summary = {
  reclaimed_stale_count : int;
  reclaimed_failed_count : int;
  session_count : int;
  total_rows : int;
  replayed_count : int;
  failed_count : int;
}

let boot_stage_start_message stage = Printf.sprintf "Boot: %s start" stage

let boot_stage_done_message ?detail ~elapsed_s stage =
  match detail with
  | Some detail when String.trim detail <> "" ->
      Printf.sprintf "Boot: %s done elapsed=%.3fs %s" stage elapsed_s detail
  | Some _ | None ->
      Printf.sprintf "Boot: %s done elapsed=%.3fs" stage elapsed_s

let log_boot_stage_start stage =
  Logs.info (fun m -> m "%s" (boot_stage_start_message stage))

let log_boot_stage_done ?detail ~started_at stage =
  let elapsed_s = max 0.0 (Unix.gettimeofday () -. started_at) in
  Logs.info (fun m -> m "%s" (boot_stage_done_message ?detail ~elapsed_s stage))

let boot_stage_error_detail exn =
  Printf.sprintf "status=error error=%s" (Printexc.to_string exn)

let with_boot_stage_logging ?(now = Unix.gettimeofday)
    ?(log_message = fun msg -> Logs.info (fun m -> m "%s" msg))
    ?detail_of_result stage f =
  let open Lwt.Syntax in
  let started_at = now () in
  log_message (boot_stage_start_message stage);
  Lwt.catch
    (fun () ->
      let* result = f () in
      let detail =
        Option.map (fun detail_fn -> detail_fn result) detail_of_result
      in
      let elapsed_s = max 0.0 (now () -. started_at) in
      log_message (boot_stage_done_message ?detail ~elapsed_s stage);
      Lwt.return result)
    (fun exn ->
      let elapsed_s = max 0.0 (now () -. started_at) in
      log_message
        (boot_stage_done_message
           ~detail:(boot_stage_error_detail exn)
           ~elapsed_s stage);
      Lwt.fail exn)

let boot_resume_summary_message (summary : boot_resume_summary) =
  Printf.sprintf "pending=%d resumed=%d missing_channel=%d failed=%d"
    summary.pending_count summary.resumed_count summary.missing_channel_count
    summary.failed_count

let boot_replay_summary_message (summary : boot_replay_summary) =
  Printf.sprintf
    "sessions=%d rows=%d reclaimed_stale=%d reclaimed_failed=%d replayed=%d \
     failed=%d"
    summary.session_count summary.total_rows summary.reclaimed_stale_count
    summary.reclaimed_failed_count summary.replayed_count summary.failed_count

let mcp_servers_path () =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  Filename.concat (Filename.concat home ".clawq") "mcp_servers.json"

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

let keepalive_check_interval_s = 900.0
let keepalive_idle_threshold_s = 900.0
let restart_signal_ts_env = "CLAWQ_LAST_RESTART_SIGNAL_TS"
let restart_signal_duplicate_window_seconds = 5.0

let restart_signal_duplicate_delta ~now ~last_signal_at =
  let delta = now -. last_signal_at in
  if delta >= 0.0 && delta < restart_signal_duplicate_window_seconds then
    Some delta
  else None

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

let resumed_dispatch_target ~session_key ~channel ~channel_id =
  if session_key = "__main__" then
    Printf.sprintf "session=%s route=main-via-%s:%s" session_key channel
      channel_id
  else Printf.sprintf "session=%s route=%s:%s" session_key channel channel_id

let notify_resumed_session ?(senders = default_resume_senders)
    ~(session_manager : Session.t) ~(config : Runtime_config.t) ~session_key
    ~channel ~channel_id text =
  let open Lwt.Syntax in
  match Session.find_registered_notifier session_manager ~key:session_key with
  | Some notify ->
      Lwt.catch
        (fun () -> notify text)
        (fun exn ->
          Logs.warn (fun m ->
              m "Resumed session notifier failed for %s: %s" session_key
                (Printexc.to_string exn));
          Lwt.return_unit)
  | None ->
      Lwt.catch
        (fun () ->
          let* result =
            dispatch_resumed_message ~senders ~config ~channel ~channel_id ~text
              ()
          in
          match result with
          | Ok () -> Lwt.return_unit
          | Error err ->
              Logs.warn (fun m ->
                  m "Failed to send resumed session notice for %s via %s:%s: %s"
                    session_key channel channel_id err);
              Lwt.return_unit)
        (fun exn ->
          Logs.warn (fun m ->
              m "Resumed session notice dispatch failed for %s: %s" session_key
                (Printexc.to_string exn));
          Lwt.return_unit)

let post_dispatch_resumed_session_response
    ?(continuation_delay = Session.default_autonomous_continuation_delay)
    ?(senders = default_resume_senders) ~(session_manager : Session.t)
    ~(config : Runtime_config.t) ~session_key ~channel ~channel_id ~response ()
    =
  let trimmed = String.trim response in
  let target = resumed_dispatch_target ~session_key ~channel ~channel_id in
  Logs.info (fun m ->
      m "Resume continuation evaluation starting %s response_len=%d" target
        (String.length trimmed));
  if trimmed = "" || trimmed = "HEARTBEAT_OK" then begin
    Logs.info (fun m ->
        m
          "Resume continuation stayed idle %s reason=no-follow-up-response \
           after_restart_resume=true"
          target);
    Lwt.return_unit
  end
  else if trimmed = Session.autonomous_stay_idle_message then begin
    let open Lwt.Syntax in
    let* () =
      Session.process_autonomous_turn_result ~delay:continuation_delay
        session_manager ~key:session_key ~response:trimmed
    in
    Logs.info (fun m ->
        m
          "Resume continuation disarmed %s reason=agent-requested-idle \
           after_restart_resume=true"
          target);
    Lwt.return_unit
  end
  else begin
    let on_response follow_up =
      let open Lwt.Syntax in
      Logs.info (fun m ->
          m "Resume continuation sending follow-up %s follow_up_len=%d" target
            (String.length (String.trim follow_up)));
      let* () =
        if session_key = "__main__" then Lwt.return_unit
        else
          notify_resumed_session ~senders ~session_manager ~config ~session_key
            ~channel ~channel_id follow_up
      in
      Logs.info (fun m -> m "Resume continuation follow-up sent %s" target);
      Lwt.return_unit
    in
    Lwt.async (fun () ->
        Lwt.catch
          (fun () ->
            Session.process_autonomous_turn_result ~delay:continuation_delay
              ~on_response session_manager ~key:session_key ~response:trimmed)
          (fun exn ->
            Logs.err (fun m ->
                m "Resume continuation failed %s: %s" target
                  (Printexc.to_string exn));
            Lwt.return_unit));
    Logs.info (fun m -> m "Resume continuation armed %s" target);
    Lwt.return_unit
  end

let handle_heartbeat_response
    ?(continuation_delay = Session.default_autonomous_continuation_delay)
    ~(session_manager : Session.t) ~key ~response () =
  let open Lwt.Syntax in
  let trimmed = String.trim response in
  let* () =
    if
      trimmed = "" || trimmed = "HEARTBEAT_OK"
      || trimmed = Session.autonomous_stay_idle_message
    then
      Session.process_autonomous_turn_result ~delay:continuation_delay
        session_manager ~key ~response:trimmed
    else begin
      Lwt.async (fun () ->
          Lwt.catch
            (fun () ->
              Session.process_autonomous_turn_result ~delay:continuation_delay
                session_manager ~key ~response:trimmed)
            (fun exn ->
              Logs.err (fun m ->
                  m "Heartbeat continuation error: %s" (Printexc.to_string exn));
              Lwt.return_unit));
      Lwt.return_unit
    end
  in
  if trimmed = "HEARTBEAT_OK" then
    Logs.info (fun m -> m "Heartbeat: agent replied HEARTBEAT_OK, no outbound")
  else begin
    Logs.info (fun m ->
        m "Heartbeat: agent response (%d chars)" (String.length trimmed));
    match (Session.get_config session_manager).notify with
    | Some nc ->
        Logs.info (fun m ->
            m "Heartbeat: would notify via %s -> %s" nc.notify_channel
              nc.notify_target)
    | None ->
        Logs.warn (fun m ->
            m
              "Heartbeat: agent wants to send a message but no notify target \
               configured")
  end;
  Lwt.return_unit

let refresh_runtime_bound_tools ~(config : Runtime_config.t)
    ~(session_manager : Session.t) ~sandbox registry =
  let refresh_optional name ~configured make_tool =
    if configured then Tool_registry.replace registry (make_tool ())
    else Tool_registry.remove registry name
  in
  refresh_optional "web_search" ~configured:(config.web_search <> None)
    (fun () -> Tools_builtin.web_search ~config);
  refresh_optional "transcribe" ~configured:(config.stt <> None) (fun () ->
      Tools_builtin.transcribe ~config);
  let workspace = Runtime_config.effective_workspace config in
  Tool_registry.replace registry
    (Tools_builtin.shell_exec_with_hooks ~workspace
       ~workspace_only:config.security.workspace_only
       ~allowed_commands:Tools_builtin.default_shell_allowlist
       ~extra_allowed_paths:config.security.extra_allowed_paths ~sandbox
       ~session_mgr:session_manager ());
  Tool_registry.replace registry
    (Tools_builtin.doc_write ~workspace
       ~workspace_files:config.prompt.workspace_files);
  Logs.info (fun m -> m "Refreshed runtime-bound tools")

let make_sandbox (config : Runtime_config.t) =
  let backend = Sandbox.backend_of_policy config.security.sandbox_backend in
  let workspace = Runtime_config.effective_workspace config in
  {
    Sandbox.backend;
    workspace;
    extra_allowed_paths =
      config.security.extra_allowed_paths |> List.map Runtime_config.expand_home;
    isolate_filesystem = config.security.workspace_only;
  }

let background_task_wakeup_message task =
  Background_task.terse_finished_message task

let inject_background_task_completion
    ?(continuation_delay = Session.default_autonomous_continuation_delay)
    ?(senders = default_resume_senders) ~(session_manager : Session.t)
    ~(config : Runtime_config.t) ~session_key ?channel ?channel_id task =
  let message = background_task_wakeup_message task in
  Lwt.catch
    (fun () ->
      let open Lwt.Syntax in
      let* response =
        Session.turn session_manager ~key:session_key ~message ?channel
          ?channel_id ()
      in
      if Session.is_queued_message_response response then begin
        Logs.info (fun m ->
            m
              "Background task completion injected into busy session %s; \
               queued for later processing"
              session_key);
        Lwt.return_unit
      end
      else begin
        Session.mark_response_sent session_manager ~key:session_key;
        Logs.info (fun m ->
            m "Background task completion injected into session %s" session_key);
        match (channel, channel_id) with
        | Some ch, Some cid ->
            let* () =
              notify_resumed_session ~senders ~session_manager ~config
                ~session_key ~channel:ch ~channel_id:cid response
            in
            post_dispatch_resumed_session_response ~continuation_delay ~senders
              ~session_manager ~config ~session_key ~channel:ch ~channel_id:cid
              ~response ()
        | _ -> Lwt.return_unit
      end)
    (fun exn ->
      Logs.warn (fun m ->
          m "Background task completion session injection failed for %s: %s"
            session_key (Printexc.to_string exn));
      Lwt.return_unit)

let notify_background_task_finished ?continuation_delay
    ?(senders = default_resume_senders) ~(session_manager : Session.t) ~config
    task =
  let text = Background_task.status_message task in
  let open Lwt.Syntax in
  let* () =
    match task.Background_task.session_key with
    | Some key -> (
        match Session.find_registered_notifier session_manager ~key with
        | Some notify ->
            Lwt.catch
              (fun () -> notify text)
              (fun exn ->
                Logs.warn (fun m ->
                    m "Background task notifier failed: %s"
                      (Printexc.to_string exn));
                Lwt.return_unit)
        | None -> (
            match (task.channel, task.channel_id) with
            | Some channel, Some channel_id -> (
                let* result =
                  dispatch_resumed_message ~senders ~config ~channel ~channel_id
                    ~text ()
                in
                match result with
                | Ok () -> Lwt.return_unit
                | Error err ->
                    Logs.warn (fun m ->
                        m "Background task completion dispatch failed: %s" err);
                    Lwt.return_unit)
            | _ -> Lwt.return_unit))
    | None -> (
        match (task.channel, task.channel_id) with
        | Some channel, Some channel_id -> (
            let* result =
              dispatch_resumed_message ~config ~channel ~channel_id ~text ()
            in
            match result with
            | Ok () -> Lwt.return_unit
            | Error err ->
                Logs.warn (fun m ->
                    m "Background task completion dispatch failed: %s" err);
                Lwt.return_unit)
        | _ -> Lwt.return_unit)
  in
  match task.Background_task.session_key with
  | Some session_key ->
      inject_background_task_completion ?continuation_delay ~senders
        ~session_manager ~config ~session_key ?channel:task.channel
        ?channel_id:task.channel_id task
  | None -> Lwt.return_unit

let notify_background_task_started ~(session_manager : Session.t)
    ~config:(_config : Runtime_config.t) task =
  let message = Background_task.terse_started_message task in
  match task.Background_task.session_key with
  | Some key ->
      let open Lwt.Syntax in
      let* _queued =
        Session.enqueue_message_if_busy session_manager ~key
          {
            Session.message;
            content_parts = [];
            attachments = [];
            channel_name = None;
            channel_type = None;
            sender_id = None;
            sender_name = None;
            channel = task.channel;
            channel_id = task.channel_id;
            message_id = None;
            inbound_queue_id = None;
          }
      in
      Lwt.return_unit
  | None -> Lwt.return_unit

let resume_turn_prompt =
  "Automatic restart-resume: the daemon restarted while autonomous work was "
  ^ "actively in progress in this session. Resume the interrupted work now "
  ^ "— review the conversation history to identify where you left off, pick "
  ^ "up the highest-priority unfinished task, and continue executing from "
  ^ "that point. Use the available tools as needed and deliver the next "
  ^ "meaningful progress update without waiting for a new user message. "
(* Agent says STAY_IDLE too much so let's pretend it doesn't exist and see what happens *)
(*^ "(If, after checking the full conversation state, you have confirmed that "
  ^ "there is absolutely no way to continue, reply exactly STAY_IDLE.)"*)

let default_resume_turn ~(session_manager : Session.t) ~notify ~session_key
    agent interrupt =
  let open Lwt.Syntax in
  let* compaction_info =
    Agent.compact_history_if_needed agent ?db:session_manager.db ()
  in
  let compacted = Option.is_some compaction_info in
  let* () = Session.notify_compaction_if_needed ~notify compaction_info in
  if compacted then
    Session.persist_compacted_history session_manager ~key:session_key agent;
  Logs.info (fun m ->
      m "Firing automatic restart-resume prompt for session=%s prompt_len=%d"
        session_key
        (String.length resume_turn_prompt));
  agent.Agent.history <-
    Provider.make_message ~role:"system" ~content:resume_turn_prompt
    :: agent.Agent.history;
  let runtime_context =
    Prompt_builder.build_runtime_context ~config:session_manager.config
      ~details:
        (Session.runtime_context_details session_manager ~agent ~key:session_key
           ~compacted_before_turn:compacted)
      ()
  in
  Agent.turn agent ~user_message:resume_turn_prompt ?db:session_manager.db
    ~session_key
    ~interrupt_check:(fun () -> !interrupt)
    ?runtime_context ~history_prepared:true ()

let resume_agent_session ?(senders = default_resume_senders) ?run_turn
    ?(after_dispatch = fun ~response:_ -> Lwt.return_unit)
    ~(session_manager : Session.t) ~(config : Runtime_config.t) ~session_key
    ~channel ~channel_id () =
  let notify text =
    notify_resumed_session ~senders ~session_manager ~config ~session_key
      ~channel ~channel_id text
  in
  let run_turn =
    match run_turn with
    | Some f -> f
    | None -> default_resume_turn ~session_manager ~notify ~session_key
  in
  let open Lwt.Syntax in
  Logs.info (fun m ->
      m "Automatic restart-resume: beginning resume sequence for %s"
        (resumed_dispatch_target ~session_key ~channel ~channel_id));
  let* () =
    notify_resumed_session ~senders ~session_manager ~config ~session_key
      ~channel ~channel_id
      ("[automatic restart-resume]\n" ^ resume_turn_prompt)
  in
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
      let* () =
        match dispatch_result with
        | Ok () ->
            Logs.info (fun m ->
                m "Resumed session response dispatched %s response_len=%d"
                  (resumed_dispatch_target ~session_key ~channel ~channel_id)
                  (String.length (String.trim response)));
            let* () = after_dispatch ~response in
            Session.clear_response_deferred session_manager ~key:session_key;
            Session.mark_response_sent session_manager ~key:session_key;
            Lwt.return_unit
        | Error msg ->
            Logs.warn (fun m ->
                m "Failed to deliver resumed session %s via %s:%s: %s"
                  session_key channel channel_id msg);
            Lwt.return_unit
      in
      (* Drain any messages that were queued while the resume turn held the lock *)
      interrupt := None;
      Session.drain_queued_messages session_manager ~key:session_key agent
        interrupt ())

let setup_mcp_clients ?(connect_client = Mcp_client.connect) ~registry
    ~mcp_clients () =
  let servers_path = mcp_servers_path () in
  if not (Sys.file_exists servers_path) then Lwt.return_unit
  else
    let servers = Mcp_client.load_server_configs servers_path in
    let open Lwt.Syntax in
    Lwt_list.iter_s
      (fun cfg ->
        Lwt.catch
          (fun () ->
            let* client = connect_client cfg in
            mcp_clients := client :: !mcp_clients;
            List.iter
              (fun t ->
                Tool_registry.register registry t;
                Logs.info (fun m ->
                    m "MCP tool registered: %s (from %s)" t.Tool.name
                      cfg.Mcp_client.name))
              (Mcp_client.discovered_tools client);
            Lwt.return_unit)
          (fun exn ->
            Logs.warn (fun m ->
                m "MCP client '%s' failed to connect: %s" cfg.Mcp_client.name
                  (Printexc.to_string exn));
            Lwt.return_unit))
      servers

let run_mcp_setup_stage ?(now = Unix.gettimeofday)
    ?(log_message = fun msg -> Logs.info (fun m -> m "%s" msg))
    ?(connect_client = Mcp_client.connect) ~tool_registry ~config ~mcp_clients
    () =
  with_boot_stage_logging ~now ~log_message "mcp-setup" (fun () ->
      match (tool_registry, config.Runtime_config.mcp.enabled) with
      | Some registry, true ->
          Lwt.catch
            (fun () ->
              setup_mcp_clients ~connect_client ~registry ~mcp_clients ())
            (fun exn ->
              Logs.warn (fun m ->
                  m "Failed to load MCP servers config: %s"
                    (Printexc.to_string exn));
              Lwt.return_unit)
      | _ -> Lwt.return_unit)

let resume_pending_agent_sessions ?(senders = default_resume_senders)
    ?resume_one ~(session_manager : Session.t) ~(config : Runtime_config.t) () =
  let resume_one =
    match resume_one with
    | Some f -> f
    | None ->
        fun ~session_key ~channel ~channel_id ->
          let after_dispatch ~response =
            post_dispatch_resumed_session_response ~senders ~session_manager
              ~config ~session_key ~channel ~channel_id ~response ()
          in
          resume_agent_session ~senders ~after_dispatch ~session_manager ~config
            ~session_key ~channel ~channel_id ()
  in
  let pending =
    Session.load_pending_agent_sessions session_manager ~max_age_seconds:3600
  in
  let open Lwt.Syntax in
  let summary =
    {
      pending_count = List.length pending;
      resumed_count = 0;
      missing_channel_count = 0;
      failed_count = 0;
    }
    |> ref
  in
  if pending <> [] then
    Logs.info (fun m ->
        m "Resuming %d pending agent sessions" (List.length pending));
  let* () =
    Lwt_list.iter_s
      (fun (session_key, channel_opt, channel_id_opt) ->
        match (channel_opt, channel_id_opt) with
        | Some channel, Some channel_id ->
            Lwt.catch
              (fun () ->
                let* () = resume_one ~session_key ~channel ~channel_id in
                summary :=
                  { !summary with resumed_count = !summary.resumed_count + 1 };
                Lwt.return_unit)
              (fun exn ->
                summary :=
                  { !summary with failed_count = !summary.failed_count + 1 };
                Logs.err (fun m ->
                    m "Failed to resume session %s: %s" session_key
                      (Printexc.to_string exn));
                Lwt.return_unit)
        | _ ->
            summary :=
              {
                !summary with
                missing_channel_count = !summary.missing_channel_count + 1;
              };
            Logs.warn (fun m ->
                m "Cannot resume session %s: missing channel info" session_key);
            Session.mark_response_sent session_manager ~key:session_key;
            Lwt.return_unit)
      pending
  in
  Logs.info (fun m ->
      m "Boot: pending-session resume summary %s"
        (boot_resume_summary_message !summary));
  Lwt.return !summary

(* ANSI color codes for log output — respects NO_COLOR convention *)
let no_color =
  match Sys.getenv_opt "NO_COLOR" with Some _ -> true | None -> false

let c s = if no_color then "" else s
let ansi_reset = c "\027[0m"
let ansi_bold = c "\027[1m"
let ansi_dim = c "\027[2m"
let ansi_fg_red = c "\027[91m"
let ansi_fg_yellow = c "\027[93m"
let ansi_fg_green = c "\027[32m"
let ansi_fg_cyan = c "\027[36m"
let ansi_fg_gray = c "\027[90m"

let level_tag = function
  | Logs.App -> "APP  "
  | Logs.Error -> "ERROR"
  | Logs.Warning -> "WARN "
  | Logs.Info -> "INFO "
  | Logs.Debug -> "DEBUG"

let level_color = function
  | Logs.App -> ansi_bold ^ ansi_fg_cyan
  | Logs.Error -> ansi_bold ^ ansi_fg_red
  | Logs.Warning -> ansi_fg_yellow
  | Logs.Info -> ansi_fg_green
  | Logs.Debug -> ansi_dim

let msg_color = function
  | Logs.Error -> ansi_fg_red
  | Logs.Warning -> ansi_fg_yellow
  | _ -> ""

let local_date_key t =
  let tm = Unix.localtime t in
  (tm.Unix.tm_year, tm.Unix.tm_yday)

let pp_date_banner ppf t =
  let tm = Unix.localtime t in
  Fmt.pf ppf "%s%s=== %04d-%02d-%02d ===%s" ansi_bold ansi_fg_cyan
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday ansi_reset

let maybe_emit_date_banner ppf last_date_ref t =
  let date_key = local_date_key t in
  if !last_date_ref <> Some date_key then begin
    pp_date_banner ppf t;
    Format.pp_print_newline ppf ();
    last_date_ref := Some date_key
  end

let pp_header_with_ts ppf t (level, _header) =
  let tm = Unix.localtime t in
  let ms = int_of_float ((t -. floor t) *. 1000.0) in
  let lc = level_color level in
  let lt = level_tag level in
  Fmt.pf ppf "%s[%02d:%02d:%02d.%03d]%s %s%s%s " ansi_fg_gray tm.Unix.tm_hour
    tm.Unix.tm_min tm.Unix.tm_sec ms ansi_reset lc lt ansi_reset

let run_keepalive_loop ~db ~(session_manager : Session.t) =
  let open Lwt.Syntax in
  let rec loop () =
    let* () = Lwt_unix.sleep keepalive_check_interval_s in
    let keys = Memory.list_keepalive_session_keys ~db in
    let* () =
      Lwt_list.iter_s
        (fun session_key ->
          let* snapshot =
            Session.current_live_activity session_manager ~key:session_key
          in
          if snapshot.Session.active then begin
            Logs.debug (fun m ->
                m "Keepalive: skipping %s (currently active)" session_key);
            Lwt.return_unit
          end
          else begin
            let infos =
              Memory.list_session_infos ~db ~prefix:session_key
                ~activity:Memory.Any ()
            in
            let is_idle =
              match
                List.find_opt
                  (fun (r : Memory.session_info) -> r.session_key = session_key)
                  infos
              with
              | None -> true
              | Some r -> (
                  match r.last_active with
                  | None -> true
                  | Some ts ->
                      let epoch = Background_task.parse_sqlite_datetime ts in
                      epoch = 0.0
                      || Unix.gettimeofday () -. epoch
                         >= keepalive_idle_threshold_s)
            in
            if not is_idle then begin
              Logs.debug (fun m ->
                  m "Keepalive: skipping %s (recently active)" session_key);
              Lwt.return_unit
            end
            else begin
              Logs.info (fun m ->
                  m "Keepalive: nudging idle session %s" session_key);
              Lwt.async (fun () ->
                  Lwt.catch
                    (fun () ->
                      Session.with_suppressed_channel_output session_manager
                        ~key:session_key (fun () ->
                          let* _response =
                            Session.turn session_manager ~key:session_key
                              ~message:Session.keepalive_nudge_prompt ()
                          in
                          Lwt.return_unit))
                    (fun exn ->
                      Logs.err (fun m ->
                          m "Keepalive turn error for %s: %s" session_key
                            (Printexc.to_string exn));
                      Lwt.return_unit));
              Lwt.return_unit
            end
          end)
        keys
    in
    loop ()
  in
  loop ()

let replay_durable_inbound_queue
    ?(replay_turn :
       (Session.t -> key:string -> message:string -> unit -> string Lwt.t)
       option) ~(session_manager : Session.t) ~(config : Runtime_config.t) () =
  ignore config;
  match session_manager.Session.db with
  | None ->
      let summary =
        {
          reclaimed_stale_count = 0;
          reclaimed_failed_count = 0;
          session_count = 0;
          total_rows = 0;
          replayed_count = 0;
          failed_count = 0;
        }
      in
      Logs.info (fun m ->
          m "Boot: durable inbound replay summary %s"
            (boot_replay_summary_message summary));
      Lwt.return summary
  | Some db ->
      let open Lwt.Syntax in
      let turn_fn =
        match replay_turn with
        | Some f -> f
        | None -> fun mgr ~key ~message () -> Session.turn mgr ~key ~message ()
      in
      let reclaimed = Memory.queue_reclaim_stale ~db ~older_than_seconds:3600 in
      if reclaimed > 0 then
        Logs.info (fun m ->
            m "Boot: reclaimed %d stale inbound queue claims" reclaimed);
      let reclaimed_failed = Memory.queue_reclaim_failed ~db in
      if reclaimed_failed > 0 then
        Logs.info (fun m ->
            m "Boot: reclaimed %d failed inbound queue rows for retry"
              reclaimed_failed);
      let pending_sessions = Memory.queue_list_pending_sessions ~db in
      let total = Memory.queue_count_all ~db in
      let summary =
        {
          reclaimed_stale_count = reclaimed;
          reclaimed_failed_count = reclaimed_failed;
          session_count = List.length pending_sessions;
          total_rows = total;
          replayed_count = 0;
          failed_count = 0;
        }
        |> ref
      in
      if pending_sessions = [] then begin
        Logs.info (fun m -> m "Boot: no durable inbound queue rows to replay");
        Logs.info (fun m ->
            m "Boot: durable inbound replay summary %s"
              (boot_replay_summary_message !summary));
        Lwt.return !summary
      end
      else begin
        Logs.info (fun m ->
            m "Boot: replaying %d durable inbound queue rows across %d sessions"
              total
              (List.length pending_sessions));
        let* () =
          Lwt_list.iter_s
            (fun session_key ->
              let rec drain_session () =
                match Memory.queue_claim ~db ~session_key with
                | Memory.Claim_empty -> Lwt.return_unit
                | Memory.Claim_ok row ->
                    Logs.info (fun m ->
                        m
                          "Replay: claimed queue_id=%d session=%s source=%s \
                           attempt=%d"
                          row.queue_id session_key row.source row.attempt_count);
                    let message, is_bang =
                      try
                        let json = Yojson.Safe.from_string row.payload_json in
                        let open Yojson.Safe.Util in
                        let msg =
                          json |> member "message" |> to_string_option
                          |> Option.value ~default:""
                        in
                        let bang =
                          json |> member "bang" |> to_bool_option
                          |> Option.value ~default:false
                        in
                        (msg, bang)
                      with _ -> (row.payload_json, false)
                    in
                    if String.trim message = "" then begin
                      Logs.warn (fun m ->
                          m
                            "Replay: skipping queue_id=%d session=%s \
                             reason=empty-message"
                            row.queue_id session_key);
                      Memory.queue_record_failure ~db ~queue_id:row.queue_id
                        ~error:"empty message";
                      summary :=
                        {
                          !summary with
                          failed_count = !summary.failed_count + 1;
                        };
                      drain_session ()
                    end
                    else
                      let replay_message =
                        if
                          is_bang
                          && String.length message > 0
                          && message.[0] <> '!'
                        then "!" ^ message
                        else message
                      in
                      Lwt.catch
                        (fun () ->
                          Logs.info (fun m ->
                              m
                                "Replay: processing queue_id=%d session=%s \
                                 bang=%b msg_len=%d"
                                row.queue_id session_key is_bang
                                (String.length message));
                          let* _response =
                            turn_fn session_manager ~key:session_key
                              ~message:replay_message ()
                          in
                          let deleted =
                            Memory.queue_delete ~db ~queue_id:row.queue_id
                          in
                          if deleted then
                            Logs.info (fun m ->
                                m
                                  "Replay: success queue_id=%d session=%s \
                                   deleted=true"
                                  row.queue_id session_key)
                          else
                            Logs.warn (fun m ->
                                m
                                  "Replay: success queue_id=%d session=%s \
                                   deleted=false (already removed)"
                                  row.queue_id session_key);
                          summary :=
                            {
                              !summary with
                              replayed_count = !summary.replayed_count + 1;
                            };
                          drain_session ())
                        (fun exn ->
                          let error = Printexc.to_string exn in
                          Logs.err (fun m ->
                              m "Replay: failed queue_id=%d session=%s error=%s"
                                row.queue_id session_key error);
                          Memory.queue_record_failure ~db ~queue_id:row.queue_id
                            ~error;
                          summary :=
                            {
                              !summary with
                              failed_count = !summary.failed_count + 1;
                            };
                          drain_session ())
              in
              drain_session ())
            pending_sessions
        in
        Logs.info (fun m ->
            m "Boot: durable inbound replay complete replayed=%d failed=%d"
              !summary.replayed_count !summary.failed_count);
        Logs.info (fun m ->
            m "Boot: durable inbound replay summary %s"
              (boot_replay_summary_message !summary));
        Lwt.return !summary
      end

let resume_sessions_after_channels ?(senders = default_resume_senders)
    ?resume_one ~(session_manager : Session.t) ~(config : Runtime_config.t) () =
  Logs.info (fun m ->
      m
        "Boot: resuming pending routed sessions after channel listeners \
         spawned; outbound resume delivery uses direct Telegram/Discord/Slack \
         send APIs and does not wait for polling or gateway readiness");
  resume_pending_agent_sessions ~senders ?resume_one ~session_manager ~config ()

let run_pending_session_resume_stage ?(now = Unix.gettimeofday)
    ?(log_message = fun msg -> Logs.info (fun m -> m "%s" msg))
    ?(senders = default_resume_senders) ?resume_one ~session_manager ~config ()
    =
  with_boot_stage_logging ~now ~log_message
    ~detail_of_result:boot_resume_summary_message "pending-session-resume"
    (fun () ->
      resume_sessions_after_channels ~senders ?resume_one ~session_manager
        ~config ())

let run_durable_replay_stage ?(now = Unix.gettimeofday)
    ?(log_message = fun msg -> Logs.info (fun m -> m "%s" msg)) ?replay_turn
    ~session_manager ~config () =
  with_boot_stage_logging ~now ~log_message
    ~detail_of_result:boot_replay_summary_message "durable-replay" (fun () ->
      replay_durable_inbound_queue ?replay_turn ~session_manager ~config ())
