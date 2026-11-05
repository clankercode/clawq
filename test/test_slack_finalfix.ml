open Lwt.Syntax
open Test_slack_fix1_fixtures

let revoke base =
  ignore (Admin.remove_admin ~db:base.db ~channel:"slack" ~sender_id:"U123")

let classified base handler =
  admin base;
  ignore
    (Test_slack_fix1_recovery.configure_classification base handler
       ~kind:"shared" ~override:true)

let clean manager clock =
  live_runs manager = []
  && groups manager = []
  && not
       (List.exists
          (fun (t : Test_slack_socket.timer) -> Lwt.is_sleeping t.promise)
          clock.Test_slack_socket.timers)

let start_owner base kind =
  match kind with
  | "continuation" ->
      Session.process_autonomous_turn_result ~delay:0.0 base.manager ~key
        ~response:"ordinary authorized response"
  | "heartbeat" ->
      ignore (Session.set_session_heartbeat base.manager ~key ~enabled:true);
      Daemon_heartbeat.process_heartbeat_for_session
        ~session_manager:base.manager ~content:Session.keepalive_nudge_prompt
        key
  | "completion" ->
      Daemon_util.inject_background_task_completion
        ~session_manager:base.manager ~config:base.manager.config
        ~session_key:key (origin_task ())
  | "keepalive" ->
      Memory.set_session_keepalive ~db:base.db ~session_key:key ~enabled:true;
      let first = ref true in
      let sleep _ =
        if !first then begin
          first := false;
          Lwt.pause ()
        end
        else fst (Lwt.task ())
      in
      Daemon_util.with_keepalive_timing ~sleep
        ~now:(fun () -> Unix.gettimeofday () +. 3600.)
        (fun () ->
          Daemon_util.run_keepalive_loop ~db:base.db
            ~session_manager:base.manager)
  | _ -> assert false

let stop_owner base kind work =
  let* () = Session.cancel_autonomous_continuation base.manager ~key in
  if kind = "keepalive" then Lwt.cancel work;
  ignore_error (fun () -> work)

let test_synthetic kind mode () =
  with_recording (fun base calls _ handler _ provider run ->
      classified base handler;
      let* _ = run (message "seed authorized native history") in
      let cfg = base.manager.config in
      let clock = Test_slack_socket.make_clock () in
      Slack_thread_runs.with_clock
        ~now:(fun () -> clock.time)
        ~sleep:(Test_slack_socket.sleep clock)
        (fun () ->
          let invoked = ref 0 and snapshot = ref None and huddle = ref None in
          let pending, release = Lwt.task () in
          (provider :=
             fun ~model ~messages:_ ->
               incr invoked;
               let agent, _, _ = Hashtbl.find base.manager.sessions key in
               snapshot := agent.Agent.access_snapshot;
               huddle :=
                 Some
                   (Slack_huddle_context.resolve ~tool_name:"slack_huddles"
                      {
                        Tool.default_context with
                        session_key = Some key;
                        snapshot_id = agent.Agent.access_snapshot_id;
                      });
               if mode = "allowed" then pending
               else Lwt.return (text model Session.autonomous_stay_idle_message));
          let* () =
            match mode with
            | "revoked" ->
                revoke base;
                Lwt.return_unit
            | "disabled" ->
                reload_slack base (fun sc -> { sc with bot_token = "" });
                Lwt.return_unit
            | "terminal" -> Slack_thread_runs.shutdown base.manager
            | _ -> Lwt.return_unit
          in
          let work = start_owner base kind in
          let* () = Test_slack_socket.settle 20 in
          let owned = live_runs base.manager in
          let active =
            List.filter
              (fun (r : Slack_thread_runs.run) -> Slack_thread_runs.owns r)
              owned
          in
          let* stopped =
            match active with
            | [ r ] ->
                let* _ =
                  Slack.handle_event
                    ~config:(Option.get base.manager.config.channels.slack)
                    ~session_manager:base.manager
                    (Yojson.Safe.to_string
                       (interactive ~user:"U123" ~reference:r.reference
                          ~message_ts:(Option.get r.control) ()))
                in
                Lwt.return (interrupted base.manager)
            | _ -> Lwt.return_false
          in
          if Lwt.is_sleeping pending then
            Lwt.wakeup_later release
              (text "gpt-test" Session.autonomous_stay_idle_message);
          let* () = Test_slack_socket.settle 20 in
          let* () = stop_owner base kind work in
          let released = clean base.manager clock in
          let* () = Slack_thread_runs.shutdown base.manager in
          if mode = "allowed" then begin
            Alcotest.(check int)
              "actual synthetic owner enters provider once" 1 !invoked;
            check_bool "actual synthetic owner has an owned native generation"
              true
              (List.length active = 1);
            check_bool "owned synthetic public Stop interrupts task" true
              stopped;
            check_bool "synthetic run has no huddle request authority" true
              (match !huddle with Some (Error _) -> true | _ -> false);
            check_bool
              "synthetic execution snapshot keeps full shared classification"
              true
              (match !snapshot with
              | Some s ->
                  s.room_id = Some "C123"
                  && Room_policy.room_scope_to_string s.room_classification
                     = "shared"
                  && String.starts_with ~prefix:"admin_override:"
                       s.room_policy_decision
              | _ -> false);
            check_bool "native stream and control were visibly retired" true
              (List.exists (fun (m, _) -> m = "chat.stopStream") !calls
              && List.exists
                   (fun (m, p) -> m = "chat.update" && get "blocks" p = arr [])
                   !calls)
          end
          else begin
            Alcotest.(check int)
              "refused actual synthetic owner never enters task provider/tools"
              0 !invoked;
            if mode = "revoked" then
              check_bool "DB-only revoke keeps config identity" true
                (base.manager.config == cfg)
          end;
          check_bool "synthetic lifecycle cleanup joined" true released;
          check_bool "synthetic execution invents no human queue row" true
            (Memory.queue_list ~db:base.db ~session_key:key = []);
          Lwt.return_unit))

let test_prepared ?(interruption = "") mode denied () =
  let armed = ref false and renamed = ref false and notified = ref false in
  let manager = ref None in
  let waiting, release_notification = Lwt.task () in
  let delay method_ params =
    if !armed && method_ = "agents.sessions.rename" then begin
      renamed := true;
      let mgr = Option.get !manager in
      let agent, _, _ = Hashtbl.find mgr.Session.sessions key in
      (* Simulate an external workspace edit; the real Session notification
         awaits mocked Slack delivery after the synchronous prompt hook. *)
      agent.Agent.observed_active_workspace_files <-
        [ ("AGENTS.md", Some "previous") ];
      Lwt.return_unit
    end
    else if
      !armed
      && method_ = "chat.postMessage"
      && contains
           (Slack_huddles.field "text" params)
           "workspace context refreshed"
    then begin
      notified := true;
      waiting
    end
    else Lwt.return_unit
  in
  with_recording ~delay (fun base calls _ handler _ provider run ->
      classified base handler;
      if mode = "visibility" then begin
        let cfg = base.manager.config in
        Session.update_config base.manager
          {
            cfg with
            agent_defaults = { cfg.agent_defaults with show_tool_calls = true };
          }
      end;
      manager := Some base.manager;
      let* _ = run (message "seed native thread") in
      let held, release_owner = Lwt.task () in
      (provider := fun ~model:_ ~messages:_ -> held);
      let owner =
        if mode = "fresh" then None
        else
          Some
            (Session.turn_synthetic base.manager ~key ~message:"foreign owner"
               ())
      in
      let* () = Test_slack_socket.settle 5 in
      let* () =
        if mode = "fresh" then Lwt.return_unit
        else
          let* _ =
            run ~id:"queued-final"
              (message ~thread:"100.000001" ~ts:"100.000030"
                 "original queued human work")
          in
          Lwt.return_unit
      in
      let rows = Memory.queue_list ~db:base.db ~session_key:key in
      let* () =
        if mode = "boot" then begin
          Option.iter Lwt.cancel owner;
          ignore_error (fun () -> Lwt.map (fun _ -> ()) (Option.get owner))
        end
        else Lwt.return_unit
      in
      let mgr =
        if mode = "boot" then
          Session.create ~config:base.manager.config
            ?tool_registry:base.manager.tool_registry ~db:base.db ()
        else base.manager
      in
      manager := Some mgr;
      let cfg = mgr.config in
      let invoked = ref 0 and snapshot = ref None in
      (provider :=
         fun ~model ~messages:_ ->
           incr invoked;
           let agent, _, _ = Hashtbl.find mgr.sessions key in
           snapshot := agent.Agent.access_snapshot;
           Lwt.return (text model Session.autonomous_stay_idle_message));
      let clock = Test_slack_socket.make_clock () in
      Slack_thread_runs.with_clock
        ~now:(fun () -> clock.time)
        ~sleep:(Test_slack_socket.sleep clock)
        (fun () ->
          armed := true;
          let work =
            if mode = "fresh" then
              Lwt.map
                (fun _ -> ())
                (run ~id:"fresh-final"
                   (message ~thread:"100.000001" ~ts:"100.000031"
                      "fresh human work"))
            else if mode = "boot" then
              Lwt.map
                (fun _ -> ())
                (Daemon_inbound_replay.replay_durable_inbound_queue
                   ~session_manager:mgr ~config:cfg ())
            else begin
              release_text release_owner "gpt-test";
              Lwt.map (fun _ -> ()) (Option.get owner)
            end
          in
          let* () = Test_slack_socket.settle 20 in
          let held_after_rename =
            !renamed && !notified && Lwt.is_sleeping work
          in
          if denied then revoke base;
          let* () =
            match interruption with
            | "stop" ->
                let r =
                  List.find
                    (fun (r : Slack_thread_runs.run) ->
                      Slack_thread_runs.owns r)
                    (live_runs mgr)
                in
                Lwt.map
                  (fun _ -> ())
                  (Slack.handle_event
                     ~config:(Option.get mgr.config.channels.slack)
                     ~session_manager:mgr
                     (Yojson.Safe.to_string
                        (interactive ~user:"U123" ~reference:r.reference
                           ~message_ts:(Option.get r.control) ())))
            | "shutdown" | "restart" ->
                if interruption = "restart" then begin
                  let _, _, interrupt = Hashtbl.find mgr.sessions key in
                  interrupt := Some Agent.restart_interrupt_token
                end;
                Slack_thread_runs.shutdown mgr
            | _ -> Lwt.return_unit
          in
          let preserved_interrupt =
            if interruption = "restart" then
              let _, _, interrupt = Hashtbl.find mgr.sessions key in
              !interrupt = Some Agent.restart_interrupt_token
            else true
          in
          Lwt.wakeup_later release_notification ();
          let* () = work in
          let retained = Memory.queue_list ~db:base.db ~session_key:key in
          let released = clean mgr clock in
          let* () = Slack_thread_runs.shutdown mgr in
          let* () = Slack_thread_runs.shutdown base.manager in
          check_bool
            "real post-rename Session notification held before task entry" true
            held_after_rename;
          check_bool
            "configuration identity unchanged during DB-only role change" true
            (mgr.config == cfg);
          Alcotest.(check int)
            "post-preparation task entries"
            (if denied || interruption <> "" then 0 else 1)
            !invoked;
          check_bool "native cleanup after final verification" true released;
          check_bool "terminal shutdown preserves existing restart interrupt"
            true preserved_interrupt;
          if
            (denied || interruption = "shutdown" || interruption = "restart")
            && mode <> "fresh"
          then
            check_bool "original durable row and denial reason retained" true
              (match (rows, retained) with
              | [ before ], [ after ] ->
                  before.queue_id = after.queue_id
                  && before.payload_json = after.payload_json
                  && Option.fold ~none:false
                       ~some:(fun s ->
                         contains s
                           (if denied then "fix3 shared role" else "shut down"))
                       after.last_error
              | _ -> false)
          else
            check_bool "no invented or leftover durable row" true (retained = []);
          if (not denied) && interruption = "" then
            check_bool
              "final execution snapshot preserves verified shared scope" true
              (match !snapshot with
              | Some s ->
                  Room_policy.room_scope_to_string s.room_classification
                  = "shared"
                  && String.starts_with ~prefix:"admin_override:"
                       s.room_policy_decision
              | _ -> false);
          if denied && mode = "fresh" then
            check_bool "fresh refusal reports current policy" true
              (List.exists
                 (fun (m, p) ->
                   let text = Slack_huddles.field "text" p in
                   m = "chat.postMessage"
                   && contains text "fix3 shared role"
                   && not (contains text "Request_retained"))
                 !calls);
          Lwt.return_unit))

let test_busy_completion ~boot ~denied () =
  with_recording (fun base _ _ handler _ provider run ->
      classified base handler;
      ignore
        (Sqlite3.exec base.db
           "INSERT INTO channel_admins(channel,sender_id) VALUES \
            ('slack','U456')");
      let* _ = run (message "seed original U123 thread owner") in
      let pending, release = Lwt.task () in
      (provider := fun ~model:_ ~messages:_ -> pending);
      (* A different human owns the lock when U123's task completes. *)
      let owner =
        run ~id:"other-human"
          (message ~user:"U456" ~thread:"100.000001" ~ts:"100.000040"
             "U456 independent work")
      in
      let* () = Test_slack_socket.settle 5 in
      let* () =
        Daemon_util.inject_background_task_completion
          ~session_manager:base.manager ~config:base.manager.config
          ~session_key:key (origin_task ())
      in
      let* _ =
        run ~id:"queued-human"
          (message ~user:"U456" ~thread:"100.000001" ~ts:"100.000041"
             "U456 next independent work")
      in
      let rows = Memory.queue_list ~db:base.db ~session_key:key in
      let separated =
        match rows with
        | [ a; b ] ->
            let a = parse a.payload_json and b = parse b.payload_json in
            get "execution_origin" a = str "synthetic"
            && get "synthetic_owner" a = str "U123"
            && get "message_id" a = `Null
            && get "sender_id" a = `Null
            && get "message_id" b = str "100.000041"
            && get "sender_id" b = str "U456"
        | _ -> false
      in
      let mgr =
        if boot then
          Session.create ~config:base.manager.config
            ?tool_registry:base.manager.tool_registry ~db:base.db ()
        else base.manager
      in
      let invoked = ref 0 and huddle = ref [] in
      (provider :=
         fun ~model ~messages:_ ->
           incr invoked;
           let agent, _, _ = Hashtbl.find mgr.sessions key in
           huddle :=
             Slack_huddle_context.resolve ~tool_name:"slack_huddles"
               {
                 Tool.default_context with
                 session_key = Some key;
                 snapshot_id = agent.Agent.access_snapshot_id;
               }
             :: !huddle;
           Lwt.return (text model Session.autonomous_stay_idle_message));
      if denied then revoke base;
      let* () =
        if boot then begin
          Lwt.cancel owner;
          let* () = ignore_error (fun () -> Lwt.map (fun _ -> ()) owner) in
          Lwt.map
            (fun _ -> ())
            (Daemon_inbound_replay.replay_durable_inbound_queue
               ~session_manager:mgr ~config:mgr.config ())
        end
        else begin
          Lwt.wakeup_later release
            (text "gpt-test" Session.autonomous_stay_idle_message);
          Lwt.map (fun _ -> ()) owner
        end
      in
      let retained = Memory.queue_list ~db:base.db ~session_key:key in
      let* () = Session.cancel_autonomous_continuation mgr ~key in
      let* () = Slack_thread_runs.shutdown mgr in
      let* () = Slack_thread_runs.shutdown base.manager in
      check_bool
        "completion persisted separately with original task human and no Slack \
         message"
        true separated;
      Alcotest.(check int)
        "completion cannot borrow current or queued human's authority"
        (if denied then 0 else 2)
        !invoked;
      if denied then begin
        check_bool
          "refused completion and later human request retained unchanged" true
          (match (rows, retained) with
          | [ a; b ], [ x; y ] ->
              a.queue_id = x.queue_id && b.queue_id = y.queue_id
              && a.payload_json = x.payload_json
              && b.payload_json = y.payload_json
              && Option.fold ~none:false
                   ~some:(fun s -> contains s "fix3 shared role")
                   x.last_error
          | _ -> false)
      end
      else begin
        check_bool "allowed completion and human request both processed" true
          (retained = []);
        check_bool "completion did not acquire huddle authority" true
          (match List.rev !huddle with Error _ :: _ -> true | _ -> false)
      end;
      check_bool "busy native resources released" true
        (live_runs mgr = [] && groups mgr = []);
      Lwt.return_unit)

let test_unmarked kind () =
  with_recording (fun base _ _ handler _ provider run ->
      classified base handler;
      let* _ =
        run (message "seed authorized owner, not arbitrary cron authority")
      in
      let invoked = ref 0 in
      (provider :=
         fun ~model ~messages:_ ->
           incr invoked;
           Lwt.return (text model Session.autonomous_stay_idle_message));
      let error = ref None in
      let* () =
        Lwt.catch
          (fun () ->
            match kind with
            | "turn" ->
                Lwt.map
                  (fun _ -> ())
                  (Session.turn base.manager ~key
                     ~message:"unmarked native input" ())
            | "stream" ->
                Lwt.map
                  (fun _ -> ())
                  (Session.turn_stream base.manager ~key
                     ~message:"unmarked stream"
                     ~on_chunk:(fun _ -> Lwt.return_unit)
                     ())
            | "try" ->
                Lwt.map
                  (fun _ -> ())
                  (Session.try_turn base.manager ~key ~message:"unmarked try" ())
            | "cron" ->
                Scheduler.init_schema base.db;
                ignore
                  (Scheduler.add_job ~db:base.db ~name:"unmarked-native"
                     ~session_key:key ~message:"unmarked cron identity"
                     ~schedule:"every 1s" ());
                let* () =
                  Scheduler.tick ~db:base.db ~session_mgr:base.manager ()
                in
                Test_slack_socket.settle 20
            | _ -> assert false)
          (fun e ->
            error := Some (Printexc.to_string e);
            Lwt.return_unit)
      in
      let runs =
        if kind = "cron" then
          Scheduler.get_history ~db:base.db ~name:"unmarked-native" ~limit:1
        else []
      in
      let* () = Session.cancel_autonomous_continuation base.manager ~key in
      let* () = Slack_thread_runs.shutdown base.manager in
      Alcotest.(check int)
        "unmarked native work never borrows last human authority" 0 !invoked;
      check_bool "missing provenance has an actionable refusal" true
        (if kind = "cron" then
           match runs with
           | [ r ] ->
               r.status = "error"
               && Option.fold ~none:false
                    ~some:(fun s -> contains s "provenance")
                    r.result_preview
           | _ -> false
         else
           Option.fold ~none:false
             ~some:(fun s -> contains s "provenance")
             !error);
      check_bool "refusal invents no human request" true
        (Memory.queue_list ~db:base.db ~session_key:key = []);
      Lwt.return_unit)

let test_heartbeat_busy () =
  with_recording (fun base _ _ handler _ provider run ->
      classified base handler;
      let pending, release = Lwt.task () in
      let invoked = ref 0 in
      (provider :=
         fun ~model:_ ~messages:_ ->
           incr invoked;
           pending);
      let owner = run (message "active native task") in
      let* () = Test_slack_socket.settle 5 in
      ignore (Session.set_session_heartbeat base.manager ~key ~enabled:true);
      let before = live_runs base.manager in
      let* () =
        Daemon_heartbeat.process_heartbeat_for_session
          ~session_manager:base.manager ~content:"heartbeat work while busy" key
      in
      let skipped =
        queued_count base.manager = 0
        && (match (before, live_runs base.manager) with
          | [ a ], [ b ] -> a == b
          | _ -> false)
        && not (interrupted base.manager)
      in
      Lwt.wakeup_later release
        (text "gpt-test" Session.autonomous_stay_idle_message);
      let* _ = owner in
      let* () = Slack_thread_runs.shutdown base.manager in
      Alcotest.(check int) "busy heartbeat starts no task provider" 1 !invoked;
      check_bool "busy heartbeat skips without queue/interrupt/new generation"
        true skipped;
      Lwt.return_unit)

let test_unmarked_busy () =
  with_recording (fun base _ _ handler _ provider run ->
      classified base handler;
      let pending, release = Lwt.task () in
      (provider := fun ~model:_ ~messages:_ -> pending);
      let owner = run (message "active authorized human") in
      let* () = Test_slack_socket.settle 5 in
      let question, _ = Session.register_pending_question base.manager ~key in
      let* () =
        ignore_error (fun () ->
            Lwt.map
              (fun _ -> ())
              (Session.turn base.manager ~key ~message:"!unmarked cron steering"
                 ()))
      in
      let untouched =
        Lwt.is_sleeping question
        && (not (interrupted base.manager))
        && queued_count base.manager = 0
      in
      Session.cancel_pending_question base.manager ~key;
      Lwt.wakeup_later release
        (text "gpt-test" Session.autonomous_stay_idle_message);
      let* _ = owner in
      let* () = Slack_thread_runs.shutdown base.manager in
      check_bool
        "unmarked busy input cannot answer questions, interrupt, or queue" true
        untouched;
      Lwt.return_unit)

let test_draining_retention synthetic phase () =
  let armed = ref false and notified = ref false and blocked = ref false in
  let waiting, release_preparation = Lwt.task () in
  let current_manager = ref None in
  let delay method_ params =
    if !armed && method_ = "agents.sessions.rename" then begin
      let mgr = Option.get !current_manager in
      let agent, _, _ = Hashtbl.find mgr.Session.sessions key in
      agent.Agent.observed_active_workspace_files <-
        [ ("AGENTS.md", Some "previous") ];
      if phase = "before-preparation" then begin
        blocked := true;
        waiting
      end
      else Lwt.return_unit
    end
    else if
      !armed
      && method_ = "chat.postMessage"
      && contains
           (Slack_huddles.field "text" params)
           "workspace context refreshed"
    then begin
      notified := true;
      if phase <> "verification" then begin
        blocked := true;
        waiting
      end
      else Lwt.return_unit
    end
    else if !armed && !notified && method_ = "users.info" then begin
      blocked := true;
      waiting
    end
    else Lwt.return_unit
  in
  with_recording ~delay (fun base calls _ handler _ provider run ->
      classified base handler;
      ignore
        (Sqlite3.exec base.db
           "INSERT INTO channel_admins(channel,sender_id) VALUES \
            ('slack','U456')");
      current_manager := Some base.manager;
      let clock = Test_slack_socket.make_clock () in
      Slack_thread_runs.with_clock
        ~now:(fun () -> clock.time)
        ~sleep:(Test_slack_socket.sleep clock)
        (fun () ->
          let* _ = run (message "seed original U123 owner") in
          let pending, release_owner = Lwt.task () in
          (provider := fun ~model:_ ~messages:_ -> pending);
          let owner =
            run ~id:"restart-other-human"
              (message ~user:"U456" ~thread:"100.000001" ~ts:"100.000050"
                 "U456 independent work")
          in
          let* () = Test_slack_socket.settle 5 in
          let* () =
            if synthetic then
              Daemon_util.inject_background_task_completion
                ~session_manager:base.manager ~config:base.manager.config
                ~session_key:key (origin_task ())
            else
              Lwt.map
                (fun _ -> ())
                (run ~id:"restart-queued-human"
                   (message ~thread:"100.000001" ~ts:"100.000051"
                      "U123 original queued human work"))
          in
          let rows = Memory.queue_list ~db:base.db ~session_key:key in
          let queued = Hashtbl.find base.manager.queued_messages key in
          let invoked = ref 0 and entries = ref [] and snapshots = ref [] in
          (provider :=
             fun ~model ~messages:_ ->
               incr invoked;
               let mgr = Option.get !current_manager in
               entries :=
                 !entries
                 @ List.map
                     (fun (r : Slack_thread_runs.run) -> r.owner)
                     (List.filter Slack_thread_runs.owns (live_runs mgr));
               let agent, _, _ = Hashtbl.find mgr.sessions key in
               snapshots := agent.Agent.access_snapshot :: !snapshots;
               Lwt.return (text model "original queued request completed"));
          armed := true;
          release_text release_owner "gpt-test";
          let* () = Test_slack_socket.settle 20 in
          let held = !blocked && Lwt.is_sleeping owner in
          (* The daemon sets Session draining before closing native admission. *)
          let* () = Session.start_draining base.manager in
          let* () =
            if phase = "production-restart" then
              Slack_thread_runs.shutdown base.manager
            else Lwt.return_unit
          in
          Lwt.wakeup_later release_preparation ();
          let* _ = owner in
          armed := false;
          let before_boot_entries = !entries in
          let before_boot_invoked = !invoked in
          let retained = Memory.queue_list ~db:base.db ~session_key:key in
          let memory_retained =
            Hashtbl.find_opt base.manager.queued_messages key = Some queued
          in
          let _, _, interrupt = Hashtbl.find base.manager.sessions key in
          let preserved_interrupt =
            !interrupt = Some Agent.restart_interrupt_token
          in
          let pending_before_boot =
            Session.load_pending_agent_sessions base.manager
              ~max_age_seconds:3600
          in
          let released = clean base.manager clock in
          let* () = Session.cancel_autonomous_continuation base.manager ~key in
          let* () = Slack_thread_runs.shutdown base.manager in
          let mgr =
            Session.create ~config:base.manager.config
              ?tool_registry:base.manager.tool_registry ~db:base.db ()
          in
          current_manager := Some mgr;
          (* Boot resumes pending sessions before it replays durable requests.
             Keep U456 authorized so an erroneous last-human resume is visible. *)
          revoke base;
          let* resumed =
            Daemon_resume.resume_sessions_after_channels ~session_manager:mgr
              ~config:mgr.config ()
          in
          let* denied =
            Daemon_inbound_replay.replay_durable_inbound_queue
              ~session_manager:mgr ~config:mgr.config ()
          in
          let denied_rows = Memory.queue_list ~db:base.db ~session_key:key in
          let before_allow_entries = !entries in
          let before_allow_invoked = !invoked in
          admin base;
          let* replayed =
            Daemon_inbound_replay.replay_durable_inbound_queue
              ~session_manager:mgr ~config:mgr.config ()
          in
          let* () = Session.cancel_autonomous_continuation mgr ~key in
          let fresh_released = clean mgr clock in
          let* () = Slack_thread_runs.shutdown mgr in
          let next =
            Session.create ~config:mgr.config ?tool_registry:mgr.tool_registry
              ~db:base.db ()
          in
          current_manager := Some next;
          let* next_resume =
            Daemon_resume.resume_sessions_after_channels ~session_manager:next
              ~config:next.config ()
          in
          let* next_replay =
            Daemon_inbound_replay.replay_durable_inbound_queue
              ~session_manager:next ~config:next.config ()
          in
          let* () = Session.cancel_autonomous_continuation next ~key in
          let next_released = clean next clock in
          let* () = Slack_thread_runs.shutdown next in
          check_bool "actual foreign drain held before task entry" true held;
          check_bool "draining prevents task entry even before native shutdown"
            true
            (before_boot_invoked = 0 && before_boot_entries = []);
          check_bool
            "original durable row, payload and draining reason retained" true
            (match (rows, retained) with
            | [ before ], [ after ] ->
                before.queue_id = after.queue_id
                && before.payload_json = after.payload_json
                && after.state = Memory.Failed
                && Option.fold ~none:false
                     ~some:(fun s -> contains s "Session draining")
                     after.last_error
            | _ -> false);
          check_bool "original queued owner and request retained in memory" true
            memory_retained;
          check_bool "restart interrupt survives preparation and cleanup" true
            preserved_interrupt;
          check_bool "native resources and in-flight work joined" true
            (released && Session.current_in_flight base.manager = 0);
          if phase <> "before-preparation" then
            check_bool "retained work remains pending, never falsely answered"
              true
              (List.exists (fun (k, _, _) -> k = key) pending_before_boot);
          Alcotest.(check (list string))
            "boot cannot execute retained data using last-human authority" []
            before_allow_entries;
          Alcotest.(check int)
            "boot defers competing automatic resume" 0 resumed.pending_count;
          check_bool "fresh authorization refuses revoked original owner" true
            (denied.failed_count = 1 && before_allow_invoked = 0
           && before_allow_entries = []
            &&
            match (rows, denied_rows) with
            | [ before ], [ after ] ->
                before.queue_id = after.queue_id
                && before.payload_json = after.payload_json
                && Option.fold ~none:false
                     ~some:(fun s -> contains s "fix3 shared role")
                     after.last_error
            | _ -> false);
          Alcotest.(check int)
            "task entries across draining, replay and subsequent boot" 1
            !invoked;
          check_bool "new manager replays exactly once under original U123" true
            (replayed.replayed_count = 1
            && replayed.failed_count = 0 && !entries = [ "U123" ]
            && Memory.queue_list ~db:base.db ~session_key:key = []);
          check_bool "completed replay cannot resume again on the next boot"
            true
            (next_resume.pending_count = 0
            && next_replay.total_rows = 0 && next_released);
          check_bool
            "replay has current shared authorization and joined cleanup" true
            (fresh_released
            &&
            match !snapshots with
            | [ Some s ] ->
                Room_policy.room_scope_to_string s.room_classification
                = "shared"
                && String.starts_with ~prefix:"admin_override:"
                     s.room_policy_decision
            | _ -> false);
          check_bool "draining never delivered a successful restart response"
            true
            (not
               (List.exists
                  (fun (method_, p) ->
                    method_ = "chat.postMessage"
                    && Slack_huddles.field "text" p = Session.draining_message)
                  !calls));
          Lwt.return_unit))

let suite =
  List.concat_map
    (fun kind ->
      List.map
        (fun mode -> (kind ^ " " ^ mode, `Quick, test_synthetic kind mode))
        [ "revoked"; "disabled"; "terminal"; "allowed" ])
    [ "continuation"; "keepalive"; "completion"; "heartbeat" ]
  @ List.concat_map
      (fun mode ->
        List.map
          (fun denied ->
            ( (mode ^ if denied then " late revoke" else " late allow"),
              `Quick,
              test_prepared mode denied ))
          [ true; false ])
      [ "fresh"; "foreign-drain"; "boot"; "visibility" ]
  @ List.concat_map
      (fun boot ->
        List.map
          (fun denied ->
            ( ((if boot then "boot" else "busy")
              ^ " completion "
              ^ if denied then "original owner revoked" else "allowed"),
              `Quick,
              test_busy_completion ~boot ~denied ))
          [ true; false ])
      [ false; true ]
  @ List.map
      (fun kind -> ("unmarked " ^ kind, `Quick, test_unmarked kind))
      [ "turn"; "stream"; "try"; "cron" ]
  @ [
      ("heartbeat busy skip", `Quick, test_heartbeat_busy);
      ("unmarked busy isolation", `Quick, test_unmarked_busy);
    ]
  @ List.concat_map
      (fun mode ->
        List.map
          (fun interruption ->
            ( mode ^ " late " ^ interruption,
              `Quick,
              test_prepared ~interruption mode false ))
          [ "stop"; "shutdown"; "restart" ])
      [ "fresh"; "foreign-drain" ]
  @ List.concat_map
      (fun synthetic ->
        List.map
          (fun phase ->
            ( (if synthetic then "completion" else "human") ^ " " ^ phase,
              `Quick,
              test_draining_retention synthetic phase ))
          [
            "production-restart";
            "preparation";
            "verification";
            "before-preparation";
          ])
      [ true; false ]
