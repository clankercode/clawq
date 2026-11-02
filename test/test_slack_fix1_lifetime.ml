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

let suite =
  [
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
