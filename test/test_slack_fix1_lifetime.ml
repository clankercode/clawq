open Lwt.Syntax
open Test_slack_fix1_fixtures

let test_lifetime ~shutdown () =
  with_recording (fun base calls _ _ _ provider run ->
      let clock = Test_slack_socket.make_clock () in
      let gate, release = Lwt.task () in
      (provider := fun ~model:_ ~messages:_ -> gate);
      Slack_thread_runs.with_clock
        ~now:(fun () -> clock.time)
        ~sleep:(Test_slack_socket.sleep clock)
        (fun () ->
          let work = run (message "long running work") in
          let* () = Test_slack_socket.settle 4 in
          let pending () =
            List.filter
              (fun (t : Test_slack_socket.timer) -> Lwt.is_sleeping t.promise)
              clock.timers
          in
          let started_timer = pending () <> [] in
          let start_time = clock.time in
          let* () =
            if shutdown then Slack_thread_runs.shutdown base.manager
            else if started_timer then
              let rec advance n =
                if n = 0 then Lwt.return_unit
                else begin
                  Test_slack_socket.fire clock 540.;
                  let* () = Test_slack_socket.settle 4 in
                  advance (n - 1)
                end
              in
              advance 7
            else Lwt.return_unit
          in
          let released_on_shutdown =
            live_runs base.manager = [] && groups base.manager = []
          in
          let streams =
            List.filter (fun (m, _) -> m = "chat.startStream") !calls
          in
          let processing =
            List.filter
              (fun (m, p) ->
                m = "agents.sessions.setStatus"
                && get "status" p = str "processing")
              !calls
          in
          release_text release "gpt-test";
          let* _ = work in
          check_bool "live generation has an owned maintenance timer" true
            started_timer;
          if shutdown then
            check_bool "shutdown releases run, group and caretaker" true
              released_on_shutdown
          else begin
            check_bool "fake clock crosses one hour" true
              (clock.time -. start_time >= 3600.);
            check_bool "owned marker rotates before each ten minute limit" true
              (List.length streams >= 8);
            check_bool "processing is refreshed beyond one hour" true
              (List.length processing >= 8)
          end;
          check_bool "no timer survives terminal/shutdown cleanup" true
            (pending () = []);
          let before = List.length !calls in
          let* () = Test_slack_socket.settle 5 in
          Alcotest.(check int)
            "no maintenance write after terminal cleanup" before
            (List.length !calls);
          Lwt.return_unit))

let test_delayed_rotation () =
  let gate, release_http = Lwt.wait () in
  let processing = ref 0 in
  let delay method_ params =
    if
      method_ = "agents.sessions.setStatus"
      && get "status" params = str "processing"
    then begin
      incr processing;
      if !processing = 2 then gate else Lwt.return_unit
    end
    else Lwt.return_unit
  in
  with_recording ~delay (fun base calls _ _ _ provider run ->
      let clock = Test_slack_socket.make_clock () in
      let first, release_first = Lwt.task () in
      let second, release_second = Lwt.task () in
      let count = ref 0 in
      (provider :=
         fun ~model:_ ~messages:_ ->
           incr count;
           if !count = 1 then first else second);
      Slack_thread_runs.with_clock
        ~now:(fun () -> clock.time)
        ~sleep:(Test_slack_socket.sleep clock)
        (fun () ->
          let work = run (message "old long work") in
          let* () = Test_slack_socket.settle 4 in
          let old_run = List.hd (live_runs base.manager) in
          let old_ts = Option.get old_run.stream in
          Test_slack_socket.fire clock 540.;
          let* () = Test_slack_socket.settle 4 in
          release_text release_first "gpt-test";
          let* () = Test_slack_socket.settle 4 in
          let next =
            run ~id:"after-delayed-rotation"
              (message ~thread:"100.000001" ~ts:"100.000006" "successor work")
          in
          let* () = Test_slack_socket.settle 5 in
          let waiting = !count = 1 && Lwt.is_sleeping work in
          Lwt.wakeup_later release_http ();
          let* () = Test_slack_socket.settle 8 in
          let* _ =
            run ~id:"stale-native"
              (obj
                 [
                   ("type", str "agent_session_stopped");
                   ("user", str "U123");
                   ("channel", str "C123");
                   ("thread_ts", str "100.000001");
                   ("streaming_message_ts", arr [ str old_ts ]);
                 ])
          in
          let stale_safe = not (interrupted base.manager) in
          let states =
            List.filter_map
              (fun (m, p) ->
                if m = "agents.sessions.setStatus" then Some (get "status" p)
                else None)
              !calls
          in
          let stream_count =
            List.length
              (List.filter (fun (m, _) -> m = "chat.startStream") !calls)
          in
          release_text release_second "gpt-test";
          let* _ = work in
          let* _ = next in
          check_bool "successor waits joined delayed old UI reply" true waiting;
          Alcotest.(check int)
            "finished rotation never opens abandoned marker" 2 stream_count;
          check_bool "old terminal precedes successor processing" true
            (states
            = [
                str "processing";
                str "processing";
                str "active";
                str "processing";
              ]);
          check_bool "stale old native stream cannot interrupt successor" true
            stale_safe;
          check_bool "joined runs release timers and groups" true
            (live_runs base.manager = []
            && groups base.manager = []
            && not
                 (List.exists
                    (fun (t : Test_slack_socket.timer) ->
                      Lwt.is_sleeping t.promise)
                    clock.timers));
          Lwt.return_unit))

let test_shutdown_finishing () =
  let gate, release_http = Lwt.wait () in
  let n = ref 0 in
  let delay method_ params =
    if
      method_ = "agents.sessions.setStatus"
      && get "status" params = str "processing"
    then begin
      incr n;
      if !n = 2 then gate else Lwt.return_unit
    end
    else Lwt.return_unit
  in
  with_recording ~delay (fun base _ _ _ _ provider run ->
      let clock = Test_slack_socket.make_clock () in
      let pending, release_provider = Lwt.task () in
      (provider := fun ~model:_ ~messages:_ -> pending);
      Slack_thread_runs.with_clock
        ~now:(fun () -> clock.time)
        ~sleep:(Test_slack_socket.sleep clock)
        (fun () ->
          let work = run (message "finishing during rotation") in
          let* () = Test_slack_socket.settle 4 in
          Test_slack_socket.fire clock 540.;
          let* () = Test_slack_socket.settle 4 in
          release_text release_provider "gpt-test";
          let* () = Test_slack_socket.settle 4 in
          let stopping = Slack_thread_runs.shutdown base.manager in
          let joined = Lwt.is_sleeping stopping in
          Lwt.wakeup_later release_http ();
          let* _ = work in
          let* () = stopping in
          check_bool "shutdown joins cleanup already removed from live lookup"
            true joined;
          check_bool "all finishing groups released before shutdown returns"
            true
            (groups base.manager = []);
          Lwt.return_unit))

let test_preparation_deadline () =
  let gate, release_http = Lwt.wait () in
  let delay method_ _ =
    if method_ = "agents.sessions.rename" then gate else Lwt.return_unit
  in
  with_recording ~delay (fun _base calls _ _ _ provider run ->
      let clock = Test_slack_socket.make_clock () in
      let pending, release_provider = Lwt.task () in
      (provider := fun ~model:_ ~messages:_ -> pending);
      Slack_thread_runs.with_clock
        ~now:(fun () -> clock.time)
        ~sleep:(Test_slack_socket.sleep clock)
        (fun () ->
          let started = clock.time in
          let work = run (message "slow UI preparation") in
          let* () = Test_slack_socket.settle 4 in
          clock.time <- clock.time +. 120.;
          Lwt.wakeup_later release_http ();
          let* () = Test_slack_socket.settle 4 in
          if
            List.exists
              (fun (t : Test_slack_socket.timer) ->
                t.delay = 420. && Lwt.is_sleeping t.promise)
              clock.timers
          then Test_slack_socket.fire clock 420.
          else clock.time <- clock.time +. 420.;
          let* () = Test_slack_socket.settle 4 in
          let count =
            List.length
              (List.filter (fun (m, _) -> m = "chat.startStream") !calls)
          in
          release_text release_provider "gpt-test";
          let* _ = work in
          check_bool "clock reaches nine minutes from original stream" true
            (clock.time -. started = 540.);
          Alcotest.(check int)
            "rotation deadline includes delayed UI preparation" 2 count;
          Lwt.return_unit))

let test_shutdown_preparing ~restart () =
  let ui, release_ui = Lwt.wait () in
  let armed = ref false and renamed = ref false in
  let delay m _ =
    if !armed && m = "agents.sessions.rename" then begin
      renamed := true;
      ui
    end
    else Lwt.return_unit
  in
  with_recording ~delay (fun base _ _ _ _ provider run ->
      let clock = Test_slack_socket.make_clock () in
      Slack_thread_runs.with_clock
        ~now:(fun () -> clock.time)
        ~sleep:(Test_slack_socket.sleep clock)
        (fun () ->
          let held, release_owner = Lwt.task () in
          (provider :=
             fun ~model ~messages:_ ->
               let* content = held in
               Lwt.return (text model content));
          let owner =
            Session.turn base.manager ~key ~message:"background owner" ()
          in
          let* () = Test_slack_socket.settle 4 in
          let* _ =
            run ~id:"queued-preparation"
              (message ~thread:"100.000001" ~ts:"100.000010"
                 "queued before shutdown")
          in
          let r =
            Option.get (Slack_thread_runs.find base.manager key "100.000010")
          in
          let row = List.hd (Memory.queue_list ~db:base.db ~session_key:key) in
          let invoked = ref 0 in
          (provider :=
             fun ~model ~messages:_ ->
               incr invoked;
               Lwt.return (text model "must not start"));
          armed := true;
          Lwt.wakeup_later release_owner "owner done";
          let* () = Test_slack_socket.settle 6 in
          let preparation_held = !renamed && Lwt.is_sleeping owner in
          (if restart then
             let _, _, interrupt = Hashtbl.find base.manager.sessions key in
             interrupt := Some Agent.restart_interrupt_token);
          let stopping = Slack_thread_runs.shutdown base.manager in
          let joined = Lwt.is_sleeping stopping in
          Lwt.wakeup_later release_ui ();
          let* _ = owner in
          let* () = stopping in
          let saved_interrupt =
            Session.interrupt_check_if_present base.manager ~key ()
          in
          let retained = Memory.queue_list ~db:base.db ~session_key:key in
          let resources =
            live_runs base.manager = []
            && groups base.manager = []
            && not
                 (List.exists
                    (fun (t : Test_slack_socket.timer) ->
                      Lwt.is_sleeping t.promise)
                    clock.timers)
          in
          let before_successor = !invoked in
          armed := false;
          let fresh =
            Session.create ~config:base.manager.config
              ?tool_registry:base.manager.tool_registry ~db:base.db ()
          in
          let retried = ref 0 in
          (provider :=
             fun ~model ~messages:_ ->
               incr retried;
               Lwt.return (text model "retried retained request"));
          let* replay =
            Daemon_inbound_replay.replay_durable_inbound_queue
              ~session_manager:fresh ~config:fresh.config ()
          in
          let next, release_next = Lwt.task () in
          (provider := fun ~model:_ ~messages:_ -> next);
          let successor =
            Slack.handle_event
              ~config:(Option.get fresh.config.channels.slack)
              ~session_manager:fresh
              (Yojson.Safe.to_string
                 (callback
                    ~id:(Temp_downloads.generate_token ())
                    (message ~thread:"100.000001" ~ts:"100.000011" "successor")))
          in
          let* () = Test_slack_socket.settle 5 in
          let* _ =
            Slack.handle_event
              ~config:(Option.get fresh.config.channels.slack)
              ~session_manager:fresh
              (Yojson.Safe.to_string
                 (interactive ~reference:r.reference
                    ~message_ts:(Option.get r.control) ()))
          in
          let stale_safe =
            Session.interrupt_check_if_present fresh ~key () = None
          in
          release_text release_next "gpt-test";
          let* _ = successor in
          check_bool "actual queued preparation held at native rename" true
            preparation_held;
          check_bool "shutdown joins preparing UI cleanup" true joined;
          Alcotest.(check int)
            "terminal preparation never enters provider" 0 before_successor;
          Alcotest.(check (option string))
            "shutdown/restart interrupt preserved"
            (Some
               (if restart then Agent.restart_interrupt_token
                else Agent.stop_interrupt_token))
            saved_interrupt;
          check_bool "shutdown retains original row with terminal reason" true
            (match retained with
            | [ x ] ->
                x.queue_id = row.queue_id
                && Option.fold ~none:false
                     ~some:(fun e -> contains e "finished")
                     x.last_error
            | _ -> false);
          check_bool "shutdown joins run/group/maintenance release" true
            resources;
          Alcotest.(check int)
            "new manager replays retained request" 1 replay.replayed_count;
          Alcotest.(check int)
            "retained request executes once after restart" 1 !retried;
          check_bool "stale prepared control cannot stop later generation" true
            stale_safe;
          check_bool
            "successor and retry release all resources and durable rows" true
            (live_runs base.manager = []
            && live_runs fresh = []
            && groups base.manager = []
            && groups fresh = []
            && Memory.queue_list ~db:base.db ~session_key:key = []);
          Lwt.return_unit))

let test_shutdown_queued_finishing () =
  let ui, release_ui = Lwt.wait () in
  let terminal = ref false in
  let delay m p =
    if m = "chat.update" && get "blocks" p = arr [] then begin
      terminal := true;
      ui
    end
    else Lwt.return_unit
  in
  with_recording ~delay (fun base _ _ _ _ provider run ->
      let clock = Test_slack_socket.make_clock () in
      Slack_thread_runs.with_clock
        ~now:(fun () -> clock.time)
        ~sleep:(Test_slack_socket.sleep clock)
        (fun () ->
          let held, release_owner = Lwt.task () in
          let invoked = ref 0 in
          (provider :=
             fun ~model ~messages:_ ->
               incr invoked;
               let* content = held in
               Lwt.return (text model content));
          let owner =
            Session.turn base.manager ~key ~message:"background owns lock" ()
          in
          let* () = Test_slack_socket.settle 4 in
          let* _ =
            run ~id:"never-active"
              (message ~thread:"100.000001" ~ts:"100.000012" "queued task B")
          in
          let r =
            Option.get (Slack_thread_runs.find base.manager key "100.000012")
          in
          let never_active = (not r.active) && Lwt.is_sleeping owner in
          let cancelling =
            Slack.handle_event
              ~config:(Option.get base.manager.config.channels.slack)
              ~session_manager:base.manager
              (Yojson.Safe.to_string
                 (interactive ~reference:r.reference
                    ~message_ts:(Option.get r.control) ()))
          in
          let* () = Test_slack_socket.settle 4 in
          let removed_while_finishing =
            !terminal && r.finished
            && live_runs base.manager = []
            && groups base.manager <> []
          in
          let stopping = Slack_thread_runs.shutdown base.manager in
          let joined = Lwt.is_sleeping stopping in
          Lwt.wakeup_later release_ui ();
          let* _ = cancelling in
          let* () = stopping in
          Lwt.wakeup_later release_owner "background done";
          let* _ = owner in
          check_bool "queued control never had active native owner" true
            never_active;
          check_bool "delayed terminal cleanup removed from live lookup" true
            removed_while_finishing;
          check_bool "shutdown waits never-active terminal control reply" true
            joined;
          Alcotest.(check int) "cancelled queued work never executes" 1 !invoked;
          check_bool "all queued cancellation cleanup joined" true
            (live_runs base.manager = []
            && groups base.manager = []
            && Memory.queue_list ~db:base.db ~session_key:key = []
            && not
                 (List.exists
                    (fun (t : Test_slack_socket.timer) ->
                      Lwt.is_sleeping t.promise)
                    clock.timers));
          Lwt.return_unit))

let test_shutdown_before_create method_ () =
  let pending, release_ui = Lwt.wait () in
  let waiting = ref false in
  let delay m _ =
    if m = method_ then begin
      waiting := true;
      pending
    end
    else Lwt.return_unit
  in
  with_recording ~delay (fun base calls _ _ _ provider run ->
      let invoked = ref 0 in
      (provider :=
         fun ~model ~messages:_ ->
           incr invoked;
           Lwt.return (text model "must never start"));
      let delivery = run (message "in flight before native create") in
      let* () = Test_slack_socket.settle 4 in
      let before_create =
        !waiting && Lwt.is_sleeping delivery
        && live_runs base.manager = []
        && groups base.manager = []
      in
      let* () = Slack_thread_runs.shutdown base.manager in
      let at_shutdown = List.length !calls in
      Lwt.wakeup_later release_ui ();
      let* _ = delivery in
      let* _ =
        run ~id:"delivery-after-shutdown"
          (message ~ts:"101.000001" "new delivery after shutdown")
      in
      check_bool "real ingress held before run exists" true before_create;
      Alcotest.(check int)
        "shutdown fence prevents late provider entry" 0 !invoked;
      check_bool "no post-shutdown task UI writes" true
        (not
           (List.exists
              (fun (m, _) ->
                List.mem m
                  [
                    "chat.postMessage";
                    "chat.startStream";
                    "agents.sessions.setStatus";
                    "agents.sessions.rename";
                  ])
              (List.filteri (fun i _ -> i >= at_shutdown) !calls)));
      check_bool "no run group or queue allocated after shutdown snapshot" true
        (live_runs base.manager = []
        && groups base.manager = []
        && Memory.queue_list ~db:base.db ~session_key:key = []);
      Lwt.return_unit)

let test_shutdown_during_control_creation () =
  let pending, release_ui = Lwt.wait () in
  let posted = ref false in
  let delay m p =
    if m = "chat.postMessage" && get "blocks" p <> `Null then begin
      posted := true;
      pending
    end
    else Lwt.return_unit
  in
  with_recording ~delay (fun base calls _ _ _ provider run ->
      let invoked = ref 0 in
      (provider :=
         fun ~model ~messages:_ ->
           incr invoked;
           Lwt.return (text model "must not start"));
      let delivery = run (message "control still posting") in
      let* () = Test_slack_socket.settle 4 in
      let r = List.hd (live_runs base.manager) in
      let stopping = Slack_thread_runs.shutdown base.manager in
      let joined = Lwt.is_sleeping stopping in
      Lwt.wakeup_later release_ui ();
      let* _ = delivery in
      let* () = stopping in
      check_bool "real control post held after admission" true !posted;
      check_bool "shutdown joins admitted control creation" true joined;
      Alcotest.(check int)
        "admitted terminal run cannot enter provider" 0 !invoked;
      check_bool "late control reply cleaned before shutdown completion" true
        (List.exists
           (fun (m, p) ->
             m = "chat.update"
             && get "ts" p = str (Option.get r.control)
             && get "blocks" p = arr [])
           !calls);
      check_bool "admitted UI resources released" true
        (groups base.manager = [] && live_runs base.manager = []);
      Lwt.return_unit)

let test_reconnect_reload_same_manager () =
  with_recording (fun base _ credentials _ _ provider _ ->
      reload_slack base (fun sc ->
          { sc with socket_mode = true; app_token = "xapp-fixture" });
      let config = base.manager.config in
      let first = Test_slack_socket.make_socket ()
      and second = Test_slack_socket.make_socket ()
      and clock = Test_slack_socket.make_clock () in
      let connections = ref 0 and invoked = ref 0 in
      (provider :=
         fun ~model ~messages:_ ->
           incr invoked;
           Lwt.return (text model "socket complete"));
      let loop =
        Slack_socket.start_with
          ~connect:(fun () ->
            incr connections;
            if !connections = 1 then Lwt.return first.transport
            else if !connections = 2 then Lwt.return second.transport
            else fst (Lwt.task ()))
          ~on_health:(fun _ -> ())
          ~sleep:(Test_slack_socket.sleep clock)
          ~config ~session_manager:base.manager
          ~event_limiter:
            (Rate_limiter.create ~rate_per_minute:100 ~burst_multiplier:1.0)
          ()
      in
      let deliver socket ts =
        socket.Test_slack_socket.emit
          (Yojson.Safe.to_string
             (obj
                [
                  ("envelope_id", str ts);
                  ("type", str "events_api");
                  ( "payload",
                    callback
                      ~id:(Temp_downloads.generate_token ())
                      (message ~ts "same manager socket work") );
                ]))
      in
      Lwt.finalize
        (fun () ->
          let* () = first.emit {|{"type":"hello"}|} in
          let* () = deliver first "100.000001" in
          let* () = Test_slack_socket.settle 10 in
          let initial = !invoked in
          reload_slack base (fun sc ->
              { sc with bot_token = "xoxb-rotated-fix2" });
          let* () =
            first.emit {|{"type":"disconnect","reason":"refresh_requested"}|}
          in
          let* () = Test_slack_socket.settle 4 in
          Test_slack_socket.fire clock 1.;
          let* () = Test_slack_socket.settle 4 in
          let* () = second.emit {|{"type":"hello"}|} in
          let* () = deliver second "101.000001" in
          let* () = Test_slack_socket.settle 10 in
          Alcotest.(check int) "first real Socket task executed" 1 initial;
          Alcotest.(check int)
            "real connector reconnects within same lifetime" 2 !connections;
          Alcotest.(check int)
            "reconnect and config reload preserve accepting manager" 2 !invoked;
          check_bool "post-reconnect native writes use current token" true
            (List.exists
               (fun (m, token) ->
                 m = "agents.sessions.rename"
                 && token = Some "Bearer xoxb-rotated-fix2")
               !credentials);
          Lwt.return_unit)
        (fun () ->
          Lwt.cancel loop;
          ignore_error (fun () -> loop)))

let suite =
  [
    ( "same manager Socket reconnect and credential reload",
      `Quick,
      test_reconnect_reload_same_manager );
    ( "shutdown joins admitted control creation",
      `Quick,
      test_shutdown_during_control_creation );
    ( "shutdown fences authorization before native create",
      `Quick,
      test_shutdown_before_create "auth.test" );
    ( "shutdown fences history before native create",
      `Quick,
      test_shutdown_before_create "conversations.replies" );
    ( "shutdown during queued preparation retains request",
      `Quick,
      test_shutdown_preparing ~restart:false );
    ( "restart during queued preparation preserves interrupt",
      `Quick,
      test_shutdown_preparing ~restart:true );
    ( "shutdown joins never-active queued control cleanup",
      `Quick,
      test_shutdown_queued_finishing );
    ( "rotate stream and refresh processing beyond hour",
      `Quick,
      test_lifetime ~shutdown:false );
    ( "connector shutdown cancels joined maintenance",
      `Quick,
      test_lifetime ~shutdown:true );
    ( "delayed rotation joined before successor generation",
      `Quick,
      test_delayed_rotation );
    ( "shutdown joins already finishing native UI",
      `Quick,
      test_shutdown_finishing );
    ( "stream deadline includes preparation latency",
      `Quick,
      test_preparation_deadline );
  ]
