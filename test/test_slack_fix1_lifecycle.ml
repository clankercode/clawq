open Lwt.Syntax
open Test_slack_fix1_fixtures

let test_dm () =
  with_recording (fun base calls _ handler seen _ run ->
      reload_slack base (fun sc ->
          {
            sc with
            private_channel_policy = Pc_allow_if_listed;
            allow_channels = [ "D123" ];
          });
      let old = !handler in
      (handler :=
         fun m p ->
           if m = "conversations.info" then
             ok
               [
                 ( "channel",
                   obj
                     [
                       ("id", str "D123");
                       ("created", `Int 947353180);
                       ("is_im", `Bool true);
                       ("is_org_shared", `Bool false);
                       ("user", str "U123");
                       ("is_user_deleted", `Bool false);
                       ("priority", `Float 0.);
                       ("is_open", `Bool true);
                     ] );
               ]
           else old m p);
      let* _ = run (message ~channel:"D123" "DM root") in
      let* _ =
        run ~id:"dm-follow"
          (message ~channel:"D123" ~thread:"100.000001" ~ts:"100.000003"
             "DM followup")
      in
      Alcotest.(check int)
        "documented DM root and followup execute" 2 (List.length !seen);
      let dm_key = "slack:D123:thread:T123:A123:100.000001" in
      check_bool "DM followup keeps stable root" true
        (Hashtbl.mem base.manager.sessions dm_key);
      check_bool "DM response stays in root" true
        (List.exists
           (fun (m, p) ->
             m = "chat.postMessage"
             && get "channel" p = str "D123"
             && get "thread_ts" p = str "100.000001")
           !calls);
      let* _ =
        run ~id:"wrong-human"
          (message ~channel:"D123" ~user:"U456" ~thread:"100.000001"
             ~ts:"100.000004" "other human")
      in
      Alcotest.(check int) "DM metadata binds exact human" 2 (List.length !seen);
      Lwt.return_unit)

let test_question () =
  with_recording (fun base _ _ _ _ provider run ->
      let registry = Option.get base.manager.tool_registry in
      let _ask_fn =
        Daemon_questions.register_tool ~config:base.manager.config
          ~session_manager:base.manager ~db:(Some base.db) registry
      in
      let count = ref 0 and answered = ref false in
      (provider :=
         fun ~model ~messages ->
           incr count;
           if !count = 1 then
             Lwt.return
               (Provider.ToolCalls
                  {
                    calls =
                      [
                        {
                          Provider.id = "question";
                          function_name = "ask_user_question";
                          arguments =
                            {|{"questions":[{"question":"Which colour?","type":"text"}]}|};
                        };
                      ];
                    model;
                    usage = None;
                    thinking = None;
                    provider_response_items_json = None;
                  })
           else begin
             answered :=
               List.exists
                 (fun (m : Provider.message) ->
                   m.role = "tool" && contains m.content "blue"
                   && not (contains m.content "Slack thread context"))
                 messages;
             Lwt.return (text model "answered")
           end);
      let work = run (message "ask me") in
      let* () = Test_slack_socket.settle 5 in
      check_bool "real tool registered pending resolver" true
        (Hashtbl.mem base.manager.pending_questions key);
      let* _ =
        run ~id:"answer" (message ~thread:"100.000001" ~ts:"100.000003" "blue")
      in
      let consumed = not (Hashtbl.mem base.manager.pending_questions key) in
      if not consumed then Session.cancel_pending_question base.manager ~key;
      let* _ = work in
      check_bool "original answer resolves pending question" true consumed;
      check_bool "answer has no prefixed history/control wrapper" true !answered;
      Alcotest.(check int) "only tool continuation reaches provider" 2 !count;
      Alcotest.(check int)
        "answer not deferred as another prompt" 0
        (queued_count base.manager);
      Lwt.return_unit)

let test_control ~bang () =
  with_recording (fun base _ _ _ _ provider run ->
      admin base;
      let gate, release = Lwt.task () in
      (provider := fun ~model:_ ~messages:_ -> gate);
      let work = run (message "held work") in
      let* () = Test_slack_socket.settle 3 in
      let control = if bang then "!change direction" else "stop" in
      let* _ =
        run ~id:"control"
          (message ~thread:"100.000001" ~ts:"100.000003" control)
      in
      let interrupt = Session.interrupt_check_if_present base.manager ~key () in
      let queued = queued_count base.manager in
      ignore (Session.take_all_queued_messages base.manager ~key);
      release_text release "gpt-test";
      let* _ = work in
      check_bool "raw control interrupts actual held turn" true
        (interrupt
        = Some (if bang then "change direction" else Agent.stop_interrupt_token)
        );
      Alcotest.(check int) "control never becomes deferred prompt" 0 queued;
      Lwt.return_unit)

let suite =
  [
    ("documented DM metadata root followup and unrelated human", `Quick, test_dm);
    ("real pending question answer before deferral", `Quick, test_question);
    ("raw bang before model context prefix", `Quick, test_control ~bang:true);
    ("raw admin stop before deferred queue", `Quick, test_control ~bang:false);
  ]
