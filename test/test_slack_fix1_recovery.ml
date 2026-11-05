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
            Session.turn_synthetic base.manager ~key ~message:"background owner"
              ()
          in
          Lwt.return_unit
      in
      (* The explicit buffered automatic owner drains subsequent human requests. *)
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

let test_admin_policy ~shared ~boot ~resume ~already_revoked () =
  let waiting, release_ui = Lwt.wait () in
  let armed = ref false and renamed = ref false in
  let delay method_ _ =
    if !armed && method_ = "agents.sessions.rename" then begin
      renamed := true;
      waiting
    end
    else Lwt.return_unit
  in
  with_recording ~delay (fun base _ _ handler _ provider run ->
      let clock = Test_slack_socket.make_clock () in
      Slack_thread_runs.with_clock
        ~now:(fun () -> clock.time)
        ~sleep:(Test_slack_socket.sleep clock)
        (fun () ->
          admin base;
          let* _ = run (message "seed native thread") in
          let held, release_owner = Lwt.task () in
          (provider :=
             fun ~model ~messages:_ ->
               let* content = held in
               Lwt.return (text model content));
          let owner =
            if resume then
              Daemon_resume.resume_agent_session ~session_manager:base.manager
                ~config:base.manager.config ~session_key:key ~channel:"slack"
                ~channel_id:"C123"
                ~run_turn:(fun _ _ -> held)
                ()
            else
              let* _ =
                Session.turn_synthetic base.manager ~key
                  ~message:"background owner" ()
              in
              Lwt.return_unit
          in
          let* () = Test_slack_socket.settle 4 in
          let* _ =
            run ~id:"admin-followup"
              (message ~thread:"100.000001" ~ts:"100.000009"
                 "admin-only queued work")
          in
          let row = List.hd (Memory.queue_list ~db:base.db ~session_key:key) in
          check_bool "durable row actually carries saved admin role" true
            (contains row.payload_json "admin");
          if boot then Lwt.cancel owner;
          let* () =
            if boot then ignore_error (fun () -> owner) else Lwt.return_unit
          in
          let cfg =
            {
              base.manager.config with
              external_room_policy =
                {
                  default_action =
                    Runtime_config_types.Policy_deny
                      ("fix2 admin revocation", true);
                  per_connector = [];
                };
            }
          in
          let manager =
            if boot then
              Session.create ~config:cfg
                ?tool_registry:base.manager.tool_registry ~db:base.db ()
            else begin
              base.cfg := cfg;
              Session.update_config base.manager cfg;
              base.manager
            end
          in
          let old = !handler in
          (handler :=
             fun m p ->
               let j = old m p in
               if shared && m = "conversations.info" then
                 update j
                   [
                     ( "channel",
                       update (get "channel" j)
                         [
                           ("is_shared", `Bool true);
                           ("is_ext_shared", `Bool false);
                         ] );
                   ]
               else if (not shared) && m = "users.info" then
                 update j
                   [
                     ( "user",
                       update (get "user" j) [ ("is_restricted", `Bool true) ]
                     );
                   ]
               else j);
          let invoked = ref 0 in
          (provider :=
             fun ~model ~messages:_ ->
               incr invoked;
               Lwt.return (text model "should not execute"));
          let revoke () =
            ignore
              (Admin.remove_admin ~db:base.db ~channel:"slack" ~sender_id:"U123")
          in
          if already_revoked then revoke ();
          armed := true;
          let replayed = ref 0 in
          let draining =
            if boot then (
              let* summary =
                Daemon_inbound_replay.replay_durable_inbound_queue
                  ~session_manager:manager ~config:cfg ()
              in
              replayed := summary.replayed_count;
              Lwt.return_unit)
            else begin
              Lwt.wakeup_later release_owner "background finished";
              owner
            end
          in
          let* () = Test_slack_socket.settle 8 in
          let preparation_held = !renamed && Lwt.is_sleeping draining in
          if not already_revoked then revoke ();
          let config_unchanged = manager.config == cfg in
          Lwt.wakeup_later release_ui ();
          let* () = draining in
          let retained = Memory.queue_list ~db:base.db ~session_key:key in
          let clean =
            live_runs manager = []
            && groups manager = []
            && not
                 (List.exists
                    (fun (t : Test_slack_socket.timer) ->
                      Lwt.is_sleeping t.promise)
                    clock.timers)
          in
          let* () = Slack_thread_runs.shutdown base.manager in
          if not already_revoked then
            check_bool "actual native rename awaited before DB-only revocation"
              true preparation_held;
          check_bool "DB role changed without replacing config" true
            config_unchanged;
          Alcotest.(check int)
            "fresh policy denial blocks provider and tools" 0 !invoked;
          Alcotest.(check int)
            "refused request never counted replayed" 0 !replayed;
          check_bool "original durable row retained with actual policy denial"
            true
            (match retained with
            | [ r ] ->
                r.queue_id = row.queue_id
                && Option.fold ~none:false
                     ~some:(fun e -> contains e "fix2 admin revocation")
                     r.last_error
            | _ -> false);
          check_bool "refused queued lifecycle fully released" true clean;
          Lwt.return_unit))

let configure_classification base handler ~kind ~override =
  let channel = if kind = "dm" then "D123" else "C123" in
  reload_slack base (fun sc ->
      {
        sc with
        allow_channels = [ channel ];
        private_channel_policy = Pc_allow_if_listed;
      });
  let cfg =
    {
      base.manager.config with
      external_room_policy =
        {
          default_action =
            (if override then
               Runtime_config_types.Policy_deny ("fix3 shared role", true)
             else if kind = "shared" then Policy_allow
             else Policy_deny ("ordinary room control", false));
          per_connector = [];
        };
    }
  in
  base.cfg := cfg;
  Session.update_config base.manager cfg;
  let old = !handler in
  (handler :=
     fun m p ->
       if m = "conversations.info" then
         ok
           [
             ( "channel",
               if kind = "dm" then
                 obj
                   [
                     ("id", str channel);
                     ("is_im", `Bool true);
                     ("user", str "U123");
                   ]
               else
                 obj
                   [
                     ("id", str channel);
                     ("is_member", `Bool true);
                     ("is_private", `Bool false);
                     ("is_shared", `Bool (kind = "shared"));
                     ("is_ext_shared", `Bool false);
                   ] );
           ]
       else old m p);
  channel

let test_fresh_shared_revoke () =
  let waiting, release_ui = Lwt.task () in
  let renamed = ref false in
  let delay m _ =
    if m = "agents.sessions.rename" then begin
      renamed := true;
      waiting
    end
    else Lwt.return_unit
  in
  with_recording ~delay (fun base calls _ handler _ provider run ->
      admin base;
      ignore
        (configure_classification base handler ~kind:"shared" ~override:true);
      let cfg = base.manager.config in
      let invoked = ref 0 in
      (provider :=
         fun ~model ~messages:_ ->
           incr invoked;
           Lwt.return (text model "must not execute"));
      let clock = Test_slack_socket.make_clock () in
      Slack_thread_runs.with_clock
        ~now:(fun () -> clock.time)
        ~sleep:(Test_slack_socket.sleep clock)
        (fun () ->
          let work = run (message "fresh shared request") in
          let* () = Test_slack_socket.settle 5 in
          let held = !renamed && Lwt.is_sleeping work in
          ignore
            (Admin.remove_admin ~db:base.db ~channel:"slack" ~sender_id:"U123");
          let unchanged = base.manager.config == cfg in
          Lwt.wakeup_later release_ui ();
          let* _ = work in
          check_bool "fresh real native rename held before DB revoke" true held;
          check_bool "fresh role change leaves config identity unchanged" true
            unchanged;
          Alcotest.(check int)
            "fresh shared guest never enters provider or tools" 0 !invoked;
          check_bool "fresh refusal reports configured policy reason" true
            (List.exists
               (fun (m, p) ->
                 let text = Slack_huddles.field "text" p in
                 m = "chat.postMessage"
                 && contains text "fix3 shared role"
                 && not (contains text "Request_retained"))
               !calls);
          check_bool "fresh refusal allocates no artificial durable request"
            true
            (Memory.queue_list ~db:base.db ~session_key:key = []);
          check_bool "fresh refused run releases controls group and maintenance"
            true
            (live_runs base.manager = []
            && groups base.manager = []
            && not
                 (List.exists
                    (fun (t : Test_slack_socket.timer) ->
                      Lwt.is_sleeping t.promise)
                    clock.timers));
          Lwt.return_unit))

let test_classified_snapshot ~kind ~override ~queued () =
  with_recording (fun base _ _ handler _ provider run ->
      if override then admin base;
      let channel = configure_classification base handler ~kind ~override in
      let native_key = "slack:" ^ channel ^ ":thread:T123:A123:100.000001" in
      let* () =
        if queued then
          let* _ =
            run ~id:"seed-classified-owner"
              (message ~channel "seed verified owner")
          in
          Lwt.return_unit
        else Lwt.return_unit
      in
      let held, release_owner = Lwt.task () in
      (provider := fun ~model:_ ~messages:_ -> held);
      let owner =
        if queued then
          Some
            (Session.turn_synthetic base.manager ~key:native_key
               ~message:"ordinary background owner" ())
        else None
      in
      let* () = Test_slack_socket.settle 4 in
      let invoked = ref 0 in
      (provider :=
         fun ~model ~messages:_ ->
           incr invoked;
           Lwt.return (text model "classified response"));
      let* _ =
        run ~id:"classified-request"
          (if queued then
             message ~channel ~thread:"100.000001" ~ts:"100.000020"
               "queued classified work"
           else message ~channel "fresh classified work")
      in
      let* () =
        match owner with
        | None ->
            Lwt.cancel held;
            Lwt.return_unit
        | Some task ->
            release_text release_owner "gpt-test";
            let* _ = task in
            Lwt.return_unit
      in
      Alcotest.(check int) "classified native request executes once" 1 !invoked;
      let agent, _, _ = Hashtbl.find base.manager.sessions native_key in
      let snap =
        Option.get
          (Access_snapshot.get_by_id ~db:base.db
             (Option.get agent.Agent.access_snapshot_id))
      in
      Alcotest.(check (option string))
        "snapshot retains actual Slack room" (Some channel) snap.room_id;
      Alcotest.(check string)
        "persisted verified scope is truthful" kind
        (Room_policy.room_scope_to_string snap.room_classification);
      check_bool "persisted decision matches current classified policy" true
        (if override then
           String.starts_with ~prefix:"admin_override:"
             snap.room_policy_decision
         else snap.room_policy_decision = "allow");
      check_bool "successful classified turn retires row and run resources" true
        (Memory.queue_list ~db:base.db ~session_key:native_key = []
        && live_runs base.manager = []
        && groups base.manager = []);
      Lwt.return_unit)

let test_default_native_resume ~revoke ~override () =
  let waiting, release_ui = Lwt.task () in
  let armed = ref false and renamed = ref false in
  let delay m _ =
    if !armed && m = "agents.sessions.rename" then begin
      renamed := true;
      waiting
    end
    else Lwt.return_unit
  in
  with_recording ~delay (fun base calls _ handler _ provider run ->
      let* _ = run (message "seed persisted native owner and history") in
      admin base;
      ignore (configure_classification base handler ~kind:"shared" ~override);
      let cfg = base.manager.config in
      let invoked = ref 0 and snapshot = ref None in
      (provider :=
         fun ~model ~messages:_ ->
           incr invoked;
           let agent, _, _ = Hashtbl.find base.manager.sessions key in
           snapshot :=
             Option.bind agent.Agent.access_snapshot_id (fun id ->
                 Access_snapshot.get_by_id ~db:base.db id);
           Lwt.return (text model "automatic resume complete"));
      let clock = Test_slack_socket.make_clock () in
      Slack_thread_runs.with_clock
        ~now:(fun () -> clock.time)
        ~sleep:(Test_slack_socket.sleep clock)
        (fun () ->
          armed := true;
          (* Exercise default_resume_turn -> Agent.turn, without a run_turn stub. *)
          let work =
            Lwt.catch
              (fun () ->
                let* () =
                  Daemon_resume.resume_agent_session
                    ~session_manager:base.manager ~config:cfg ~session_key:key
                    ~channel:"slack" ~channel_id:"C123" ()
                in
                Lwt.return_none)
              (fun e -> Lwt.return_some (Printexc.to_string e))
          in
          let* () = Test_slack_socket.settle 6 in
          let held = !renamed && Lwt.is_sleeping work in
          if revoke then
            ignore
              (Admin.remove_admin ~db:base.db ~channel:"slack" ~sender_id:"U123");
          let unchanged = base.manager.config == cfg in
          Lwt.wakeup_later release_ui ();
          let* error = work in
          check_bool "real default resume held native rename" true held;
          check_bool "resume leaves config identity unchanged" true unchanged;
          Alcotest.(check int)
            "default resume provider entries"
            (if revoke then 0 else 1)
            !invoked;
          if revoke then
            check_bool "resume refuses with actual shared policy reason" true
              (Option.fold ~none:false
                 ~some:(fun e -> contains e "fix3 shared role")
                 error)
          else begin
            Alcotest.(check (option string))
              "default resume succeeds" None error;
            let snap = Option.get !snapshot in
            Alcotest.(check (option string))
              "resume snapshot actual room" (Some "C123") snap.room_id;
            Alcotest.(check string)
              "resume provider sees fresh shared scope" "shared"
              (Room_policy.room_scope_to_string snap.room_classification);
            check_bool "resume snapshot decision uses current verified role"
              true
              (if override then
                 String.starts_with ~prefix:"admin_override:"
                   snap.room_policy_decision
               else snap.room_policy_decision = "allow")
          end;
          check_bool "default resume cleanup removes matching controls" true
            (List.exists
               (fun (m, p) -> m = "chat.update" && get "blocks" p = arr [])
               !calls);
          check_bool "default resume releases run group and maintenance" true
            (live_runs base.manager = []
            && groups base.manager = []
            && not
                 (List.exists
                    (fun (t : Test_slack_socket.timer) ->
                      Lwt.is_sleeping t.promise)
                    clock.timers));
          check_bool "resume creates no artificial durable human request" true
            (Memory.queue_list ~db:base.db ~session_key:key = []);
          Lwt.return_unit))

let suite =
  [
    ( "default resume shared DB revoke during rename",
      `Quick,
      test_default_native_resume ~revoke:true ~override:true );
    ( "default resume shared allow snapshot",
      `Quick,
      test_default_native_resume ~revoke:false ~override:false );
    ( "default resume shared admin override snapshot",
      `Quick,
      test_default_native_resume ~revoke:false ~override:true );
    ( "shared boot DB admin revoke during rename",
      `Quick,
      test_admin_policy ~shared:true ~boot:true ~resume:false
        ~already_revoked:false );
    ( "shared resume DB admin revoke during rename",
      `Quick,
      test_admin_policy ~shared:true ~boot:false ~resume:true
        ~already_revoked:false );
    ( "shared background DB admin revoke during rename",
      `Quick,
      test_admin_policy ~shared:true ~boot:false ~resume:false
        ~already_revoked:false );
    ( "fresh shared DB admin revoke during rename",
      `Quick,
      test_fresh_shared_revoke );
    ( "fresh shared allow snapshot",
      `Quick,
      test_classified_snapshot ~kind:"shared" ~override:false ~queued:false );
    ( "queued shared allow snapshot",
      `Quick,
      test_classified_snapshot ~kind:"shared" ~override:false ~queued:true );
    ( "fresh shared admin override snapshot",
      `Quick,
      test_classified_snapshot ~kind:"shared" ~override:true ~queued:false );
    ( "queued shared admin override snapshot",
      `Quick,
      test_classified_snapshot ~kind:"shared" ~override:true ~queued:true );
    ( "fresh DM snapshot control",
      `Quick,
      test_classified_snapshot ~kind:"dm" ~override:false ~queued:false );
    ( "queued DM snapshot control",
      `Quick,
      test_classified_snapshot ~kind:"dm" ~override:false ~queued:true );
    ( "fresh group snapshot control",
      `Quick,
      test_classified_snapshot ~kind:"group" ~override:false ~queued:false );
    ( "queued group snapshot control",
      `Quick,
      test_classified_snapshot ~kind:"group" ~override:false ~queued:true );
    ( "boot DB admin revoke during rename",
      `Quick,
      test_admin_policy ~shared:false ~boot:true ~resume:false
        ~already_revoked:false );
    ( "resume drain DB admin revoke during rename",
      `Quick,
      test_admin_policy ~shared:false ~boot:false ~resume:true
        ~already_revoked:false );
    ( "background drain DB admin revoke during rename",
      `Quick,
      test_admin_policy ~shared:false ~boot:false ~resume:false
        ~already_revoked:false );
    ( "boot saved admin is currently guest",
      `Quick,
      test_admin_policy ~shared:false ~boot:true ~resume:false
        ~already_revoked:true );
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
