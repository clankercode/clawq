(** Database-backed daemon runtime loops. *)

let current_max_concurrent_native_agents (current_config : Runtime_config.t ref)
    =
  !current_config.agent_defaults.max_concurrent_native_agents

let notify_finished ~session_manager ~config ~db =
  Daemon_util.notify_background_task_finished ~session_manager ~config ~db

let notify_started ~session_manager ~config ~db =
  Daemon_util.notify_background_task_started ~session_manager ~config ~db

let recover_background_tasks ~db ~(config : Runtime_config.t)
    ~(session_manager : Session.t) =
  let recovered =
    Background_task.reap_dead_running_tasks ~db
      ~on_task_finished:(notify_finished ~session_manager ~config ~db)
  in
  if recovered > 0 then
    Logs.warn (fun m ->
        m "Recovered %d orphaned background task(s) from previous daemon run"
          recovered);
  let re_enqueued =
    Background_task.reenqueue_stale_local_tasks ~db
      ~on_task_finished:(notify_finished ~session_manager ~config ~db)
  in
  if re_enqueued > 0 then
    Logs.info (fun m ->
        m "Re-enqueued %d Local background task(s) after daemon restart"
          re_enqueued);
  let readopted =
    Background_task.readopt_running_tasks ~db
      ~on_task_finished:(notify_finished ~session_manager ~config ~db)
  in
  if readopted > 0 then
    Logs.info (fun m ->
        m "Re-adopted %d running background task(s) from previous daemon"
          readopted)

let start_scheduler_loop ~db ~(session_manager : Session.t) ~ip_limiter
    ~session_limiter ~chat_limiter ~discord_message_limiter ~slack_event_limiter
    ~teams_event_limiter ~telemetry ~runner_tokens =
  Lwt.async (fun () ->
      Lwt.catch
        (fun () ->
          let last_memory_cleanup = ref (Unix.gettimeofday ()) in
          let last_retention_run = ref 0.0 in
          let rec loop () =
            let open Lwt.Syntax in
            let tick_config = Session.get_config session_manager in
            let deliver ~channel ~channel_id ~text =
              Daemon_util.dispatch_resumed_message ~config:tick_config ~channel
                ~channel_id ~text ()
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
              Memory.cleanup_all ~db ~max_messages:mem.max_messages_per_session
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
             if Log_rotation.maybe_rotate ~log_path ~config:cur_config.log then
               Logs.info (fun m -> m "Rotated daemon.log"));
            let* () =
              Rate_limiter.cleanup_expired ip_limiter ~max_idle_seconds:300.0
            in
            let* () =
              Rate_limiter.cleanup_expired session_limiter
                ~max_idle_seconds:300.0
            in
            let* () =
              Rate_limiter.cleanup_expired chat_limiter ~max_idle_seconds:300.0
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
          Lwt.return_unit))

let start_repo_fetch_loop ~db =
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
                        (match result with Error e -> Some e | Ok () -> None)
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
          Lwt.return_unit))

let start_background_task_loop ~db ~(config : Runtime_config.t)
    ~(current_config : Runtime_config.t ref) ~(session_manager : Session.t)
    ~runner_tokens =
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
                   (notify_finished ~session_manager ~config ~db));
            ignore
              (Background_task.readopt_running_tasks ~db
                 ~on_task_finished:
                   (notify_finished ~session_manager ~config ~db));
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
                  Daemon_util.run_local_background_turn ~session_manager ~key
                    ~message ?model ?agent_name ?cwd ?context_snapshot
                    ~interrupt_check ~on_history_update ())
                ~db
                ~on_task_finished:(notify_finished ~session_manager ~config ~db)
                ~on_task_started:(notify_started ~session_manager ~config ~db)
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
          Lwt.return_unit))

let start ~db ~(config : Runtime_config.t)
    ~(current_config : Runtime_config.t ref) ~(session_manager : Session.t)
    ~ip_limiter ~session_limiter ~chat_limiter ~discord_message_limiter
    ~slack_event_limiter ~teams_event_limiter ~telemetry ~runner_tokens =
  match db with
  | Some db ->
      Scheduler.init_schema db;
      Background_task.init_schema db;
      Ambient_daemon.init_schema db;
      recover_background_tasks ~db ~config ~session_manager;
      start_scheduler_loop ~db ~session_manager ~ip_limiter ~session_limiter
        ~chat_limiter ~discord_message_limiter ~slack_event_limiter
        ~teams_event_limiter ~telemetry ~runner_tokens;
      start_repo_fetch_loop ~db;
      start_background_task_loop ~db ~config ~current_config ~session_manager
        ~runner_tokens;
      Logs.info (fun m -> m "Cron scheduler started")
  | None -> Logs.info (fun m -> m "Cron scheduler disabled (no database)")
