open Lwt.Syntax
open Test_slack_fix1_fixtures

let test_exact_snapshot () =
  with_recording (fun base _ _ _ _ provider run ->
      let invoked = ref 0 and step = ref 0 in
      Tool_registry.register
        (Option.get base.manager.tool_registry)
        {
          Tool.name = "blocked_tool";
          description = "must not run";
          parameters_schema =
            obj [ ("type", str "object"); ("properties", obj []) ];
          invoke =
            (fun ?context:_ _ ->
              incr invoked;
              Lwt.return "ran forbidden tool");
          invoke_stream = None;
          risk_level = Tool.Low;
          deferred = false;
        };
      let parsed =
        Config_loader.parse_config ~resolve_secrets:false
          (obj
             [
               ( "room_profiles",
                 arr
                   [
                     obj
                       [
                         ("id", str "parent"); ("model", str "openai:gpt-test");
                       ];
                     obj
                       [
                         ("id", str "exact");
                         ("model", str "openai:gpt-test");
                         ("denied_tools", arr [ str "blocked_tool" ]);
                       ];
                   ] );
               ( "room_profile_bindings",
                 arr
                   [
                     obj
                       [
                         ("profile_id", str "parent");
                         ("room", str "C123");
                         ("active", `Bool true);
                       ];
                     obj
                       [
                         ("profile_id", str "exact");
                         ("room", str key);
                         ("active", `Bool true);
                       ];
                   ] );
             ])
      in
      Session.update_config base.manager
        {
          base.manager.config with
          room_profiles = parsed.room_profiles;
          room_profile_bindings = parsed.room_profile_bindings;
        };
      (provider :=
         fun ~model ~messages:_ ->
           incr step;
           Lwt.return
             (if !step = 1 then
                Provider.ToolCalls
                  {
                    calls =
                      [
                        {
                          Provider.id = "denied";
                          function_name = "blocked_tool";
                          arguments = "{}";
                        };
                      ];
                    model;
                    usage = None;
                    thinking = None;
                    provider_response_items_json = None;
                  }
              else text model "done"));
      let* _ = run (message "attempt exact denied tool") in
      Alcotest.(check int)
        "exact-thread deny enforced at actual invocation" 0 !invoked;
      let agent, _, _ = Hashtbl.find base.manager.sessions key in
      let snapshot_id = Option.get agent.Agent.access_snapshot_id in
      let snap =
        Option.get (Access_snapshot.get_by_id ~db:base.db snapshot_id)
      in
      Alcotest.(check (option string))
        "actual room is unchanged" (Some "C123") snap.room_id;
      Alcotest.(check (option string))
        "exact profile frozen" (Some "exact") snap.profile_id;
      check_bool "exact denial frozen" true
        (List.mem "blocked_tool" snap.denied_tools);
      Lwt.return_unit)

let test_exact_egress () =
  with_recording (fun base calls _ _ seen _ run ->
      deny_egress base key;
      let* _ = run (message "must not read") in
      check_bool "exact-thread egress denies before any transport" true
        (!calls = [] && !seen = []);
      Lwt.return_unit)

let test_revoked_fallback ~channel () =
  with_recording (fun base calls _ _ _ _ run ->
      let* _ = run (message "register real guarded notifier") in
      let old = base.manager.config in
      reload_slack base (fun sc ->
          if channel then { sc with allow_channels = [ "C999" ] }
          else { sc with bot_token = "" });
      calls := [];
      let* () =
        Daemon_resume.notify_resumed_session ~session_manager:base.manager
          ~config:old ~session_key:key ~channel:"slack" ~channel_id:"C123"
          "forbidden fallback"
      in
      check_bool "live policy refusal cannot fall back through captured config"
        true
        (writes calls = []);
      Lwt.return_unit)

let test_fallback_egress () =
  with_recording (fun base calls _ _ _ _ run ->
      let* _ = run (message "initial") in
      let old = base.manager.config in
      Session.unregister_channel_notifier base.manager ~key;
      deny_egress base key;
      calls := [];
      let* () =
        Daemon_resume.notify_resumed_session ~session_manager:base.manager
          ~config:old ~session_key:key ~channel:"slack" ~channel_id:"C123"
          "egress denied"
      in
      check_bool "persisted fallback obeys exact current egress" true
        (writes calls = []);
      Lwt.return_unit)

let test_status ~rotation () =
  with_recording (fun base calls credentials _ _ _ run ->
      let* _ = run (message "register retained status factory") in
      let status =
        (Hashtbl.find base.manager.status_message_factories key) ()
      in
      let* () =
        Status_message.tool_start status ~id:"t" ~name:"bash"
          ~summary:(Some "first")
      in
      reload_slack base (fun sc ->
          {
            sc with
            bot_token = (if rotation then "xoxb-rotated-fixture" else "");
          });
      calls := [];
      credentials := [];
      status.last_edit <- 0.;
      let* () =
        ignore_error (fun () ->
            Status_message.tool_result status ~id:"t" ~name:"bash"
              ~result:"secret later output" ~is_error:false)
      in
      let* () = ignore_error (fun () -> Status_message.finalize status) in
      if rotation then
        check_bool "retained callback never reuses old token" true
          (List.for_all
             (fun (_, token) -> token = Some "Bearer xoxb-rotated-fixture")
             !credentials)
      else
        check_bool "retained edits/finalization stop after disable" true
          (writes calls = []);
      Lwt.return_unit)

let test_factory_egress () =
  with_recording (fun base calls _ _ _ _ run ->
      let* _ = run (message "register factory") in
      let status =
        (Hashtbl.find base.manager.status_message_factories key) ()
      in
      deny_egress base key;
      calls := [];
      let* () =
        ignore_error (fun () ->
            Status_message.update_thinking status
              "long enough model progress to force initial send")
      in
      let* () = ignore_error (fun () -> Status_message.finalize status) in
      check_bool "factory restores exact route outside ingress context" true
        (writes calls = []);
      Lwt.return_unit)

let test_origin_progress mode () =
  with_recording (fun base calls credentials _ _ _ run ->
      let* _ = run (message "register completion notifier") in
      let old = base.manager.config in
      let task =
        {
          (origin_task ()) with
          id =
            (if mode = "rotation" then 8801
             else if mode = "egress" then 8802
             else 8803);
        }
      in
      (match mode with
      | "rotation" ->
          reload_slack base (fun sc ->
              { sc with bot_token = "xoxb-rotated-fixture" })
      | "egress" -> deny_egress base key
      | _ -> reload_slack base (fun sc -> { sc with bot_token = "" }));
      Session.set_special_command_handler base.manager
        (fun ~key:_ ~message:_ ~send_progress:_ ~interrupt_check:_ ->
          Lwt.return_some "HEARTBEAT_OK");
      calls := [];
      credentials := [];
      let* () =
        Daemon_util.notify_background_task_finished
          ~session_manager:base.manager ~config:old task
      in
      let* () = Session.cancel_autonomous_continuation base.manager ~key in
      if mode = "rotation" then
        check_bool "origin progress always selects current token" true
          (List.for_all
             (fun (_, t) -> t = Some "Bearer xoxb-rotated-fixture")
             !credentials)
      else
        check_bool
          "origin progress and completion cannot bypass revocation/egress" true
          (writes calls = []);
      Lwt.return_unit)

let suite =
  [
    ("exact thread snapshot blocks actual tool", `Quick, test_exact_snapshot);
    ("exact thread egress before transport", `Quick, test_exact_egress);
    ( "retained config cannot undo bot disable",
      `Quick,
      test_revoked_fallback ~channel:false );
    ( "retained config cannot undo channel revoke",
      `Quick,
      test_revoked_fallback ~channel:true );
    ("no notifier fallback current egress", `Quick, test_fallback_egress);
    ("retained status disable", `Quick, test_status ~rotation:false);
    ("retained status token rotation", `Quick, test_status ~rotation:true);
    ("factory outside ingress exact egress", `Quick, test_factory_egress);
    ( "origin progress completion current disable",
      `Quick,
      test_origin_progress "disabled" );
    ( "origin progress completion exact egress",
      `Quick,
      test_origin_progress "egress" );
    ( "origin progress completion token rotation",
      `Quick,
      test_origin_progress "rotation" );
  ]
