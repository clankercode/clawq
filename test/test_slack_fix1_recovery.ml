open Lwt.Syntax
open Test_slack_fix1_fixtures

let test_boot_replay mode () =
  with_recording (fun base _ _ _ _ provider run ->
      let pending, release = Lwt.task () in
      (provider := fun ~model:_ ~messages:_ -> pending);
      let work = run (message "first") in
      let* () = Test_slack_socket.settle 3 in
      let* _ =
        run ~id:"queued"
          (message ~user:"U456" ~thread:"100.000001" ~ts:"100.000003"
             "durable request by U456")
      in
      let rows = Memory.queue_list ~db:base.db ~session_key:key in
      Alcotest.(check int)
        "real ingress persisted one followup" 1 (List.length rows);
      Lwt.cancel work;
      let* () = ignore_error (fun () -> Lwt.map (fun _ -> ()) work) in
      if Lwt.is_sleeping pending then release_text release "gpt-test";
      let old = base.manager.config in
      let sc = Option.get old.channels.slack in
      let sc =
        match mode with
        | "disabled" -> { sc with bot_token = "" }
        | "revoked" -> { sc with allow_users = [ "U123" ] }
        | "valid" -> { sc with allow_users = [ "U456" ] }
        | _ -> sc
      in
      let cfg = { old with channels = { old.channels with slack = Some sc } } in
      if mode = "missing" then
        ignore
          (Memory.queue_update_payload ~db:base.db
             ~queue_id:(List.hd rows).queue_id
             ~payload_json:
               {|{"message":"no original human","deferred_followup":true}|});
      let fresh =
        Session.create ~config:cfg ?tool_registry:base.manager.tool_registry
          ~db:base.db ()
      in
      let invoked = ref 0 in
      (provider :=
         fun ~model ~messages:_ ->
           incr invoked;
           Lwt.return (text model "replayed"));
      let* summary =
        Daemon_inbound_replay.replay_durable_inbound_queue
          ~session_manager:fresh ~config:old ()
      in
      if mode = "valid" then begin
        Alcotest.(check int)
          "verified original human executes despite different last owner" 1
          !invoked;
        Alcotest.(check int)
          "valid row counted replayed" 1 summary.replayed_count;
        check_bool "valid replay retired and native generation released" true
          (Memory.queue_list ~db:base.db ~session_key:key = []
          && live_runs fresh = []
          && groups fresh = []);
        Lwt_list.iter_s Slack_thread_runs.finish (live_runs base.manager)
      end
      else begin
        Alcotest.(check int)
          "unauthorized replay never reaches provider" 0 !invoked;
        Alcotest.(check int)
          "held row not counted replayed" 0 summary.replayed_count;
        let retained = Memory.queue_list ~db:base.db ~session_key:key in
        check_bool "refused row retained with truthful reason" true
          (List.length retained = 1 && (List.hd retained).last_error <> None);
        Lwt_list.iter_s Slack_thread_runs.finish (live_runs base.manager)
      end)

let test_foreign_drain ~resume ~revoke () =
  with_recording (fun base calls _ _ _ provider run ->
      let* _ = run (message "seed original thread") in
      let held, release_held = Lwt.task () in
      let next, release_next = Lwt.task () in
      let provider_calls = ref 0 in
      (provider :=
         fun ~model ~messages:_ ->
           incr provider_calls;
           if (not resume) && !provider_calls = 1 then
             let* content = held in
             Lwt.return (text model content)
           else next);
      let owner =
        if resume then
          Daemon_resume.resume_agent_session ~session_manager:base.manager
            ~config:base.manager.config ~session_key:key ~channel:"slack"
            ~channel_id:"C123"
            ~run_turn:(fun _ _ -> held)
            ()
        else
          let* _ =
            Session.turn base.manager ~key ~message:"background owner" ()
          in
          Lwt.return_unit
      in
      (* Ordinary buffered Session.turn is the actual autonomous/background drain owner. *)
      let* () = Test_slack_socket.settle 4 in
      let* _ =
        run ~id:"queued-foreign"
          (message ~user:"U456" ~thread:"100.000001" ~ts:"100.000007"
             "followup while other owner holds lock")
      in
      if revoke then
        reload_slack base (fun sc -> { sc with allow_users = [ "U123" ] });
      Lwt.wakeup_later release_held "owner complete";
      let* () = Test_slack_socket.settle 8 in
      if revoke then begin
        Alcotest.(check int)
          "revoked queued human never reaches provider" 0 !provider_calls;
        let* () = owner in
        check_bool "denied queued request remains durable" true
          (Memory.queue_list ~db:base.db ~session_key:key <> []);
        Lwt_list.iter_s Slack_thread_runs.finish (live_runs base.manager)
      end
      else begin
        let retained =
          live_runs base.manager
          |> List.find_opt (fun (r : Slack_thread_runs.run) ->
              r.message = "100.000007")
        in
        let r =
          match retained with
          | Some r -> r
          | None ->
              Alcotest.fail "queued run disappeared before provider finished"
        in
        let active = r.active in
        let* _ =
          Slack.handle_event
            ~config:(Option.get base.manager.config.channels.slack)
            ~session_manager:base.manager
            (Yojson.Safe.to_string
               (interactive ~user:"U456" ~reference:r.reference
                  ~message_ts:(Option.get r.control) ()))
        in
        let stopped = interrupted base.manager in
        if Lwt.is_sleeping next then release_text release_next "gpt-test";
        let* () = owner in
        check_bool "foreign drainer activates queued run" true active;
        check_bool "matching control interrupts actual foreign-drained turn"
          true stopped;
        check_bool "run/control ownership released on finish" true
          (live_runs base.manager = [] && groups base.manager = []);
        check_bool "matching public control visibly removed" true
          (List.exists
             (fun (m, p) ->
               m = "chat.update"
               && get "ts" p = str (Option.get r.control)
               && get "blocks" p = arr [])
             !calls);
        check_bool "executed row retired" true
          (Memory.queue_list ~db:base.db ~session_key:key = []);
        check_bool "terminal native active sent" true
          (List.exists
             (fun (m, p) ->
               m = "agents.sessions.setStatus" && get "status" p = str "active")
             !calls);
        Lwt.return_unit
      end)

let suite =
  [
    ("boot replay current Slack disable", `Quick, test_boot_replay "disabled");
    ("boot replay original requester revoke", `Quick, test_boot_replay "revoked");
    ( "boot replay missing provenance retained",
      `Quick,
      test_boot_replay "missing" );
    ("valid boot replay uses original human", `Quick, test_boot_replay "valid");
    ( "resume queued request fresh authorization",
      `Quick,
      test_foreign_drain ~resume:true ~revoke:true );
    ( "resume drainer owns queued native lifecycle",
      `Quick,
      test_foreign_drain ~resume:true ~revoke:false );
    ( "background buffered owner queued lifecycle",
      `Quick,
      test_foreign_drain ~resume:false ~revoke:false );
  ]
