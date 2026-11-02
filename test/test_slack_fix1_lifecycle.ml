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
      let dm_handler = !handler in
      let* () =
        Lwt_list.iter_s
          (fun deleted ->
            (handler :=
               fun m p ->
                 let j = dm_handler m p in
                 if m = "conversations.info" then
                   update j
                     [
                       ( "channel",
                         update (get "channel" j)
                           [ ("is_user_deleted", deleted) ] );
                     ]
                 else j);
            let* _ =
              run
                ~id:("deleted-" ^ Yojson.Safe.to_string deleted)
                (message ~channel:"D123" ~thread:"100.000001"
                   ~ts:
                     (if deleted = `Bool true then "100.000005"
                      else "100.000006")
                   "deleted or malformed peer")
            in
            Alcotest.(check int)
              "explicit deletion or malformed metadata denied" 2
              (List.length !seen);
            Lwt.return_unit)
          [ `Bool true; str "false" ]
      in
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
  with_recording (fun base calls _ _ _ provider run ->
      admin base;
      let gate, release = Lwt.task () in
      let inputs = ref [] in
      (provider :=
         fun ~model ~messages ->
           inputs := !inputs @ [ messages ];
           if List.length !inputs = 1 then gate
           else Lwt.return (text model "new direction complete"));
      let work = run (message "held work") in
      let* () = Test_slack_socket.settle 3 in
      let control = if bang then "!change direction" else "stop" in
      let* _ =
        run ~id:"control"
          (message ~thread:"100.000001" ~ts:"100.000003" control)
      in
      let interrupt = Session.interrupt_check_if_present base.manager ~key () in
      let queued = queued_count base.manager in
      let rows = Memory.queue_list ~db:base.db ~session_key:key in
      let provenance =
        match rows with
        | [ row ] ->
            contains row.payload_json "U123"
            && contains row.payload_json "100.000003"
            && contains row.payload_json "change direction"
        | _ -> false
      in
      let followup_run = Slack_thread_runs.find base.manager key "100.000003" in
      release_text release "gpt-test";
      let* _ = work in
      check_bool "raw control interrupts actual held turn" true
        (interrupt
        = Some (if bang then "change direction" else Agent.stop_interrupt_token)
        );
      Alcotest.(check int)
        "bang retained once, bare stop consumed"
        (if bang then 1 else 0)
        queued;
      Alcotest.(check int)
        "bang instruction executes once after interruption"
        (if bang then 2 else 1)
        (List.length !inputs);
      if bang then begin
        check_bool "followup has original durable requester/message" true
          provenance;
        let last = List.hd (List.rev (List.nth !inputs 1)) in
        check_bool "model receives normalized instruction after context" true
          (contains last.Provider.content "change direction"
          && not (contains last.content "!change direction"));
        Alcotest.(check int)
          "followup has its own native activation" 2
          (List.length
             (List.filter (fun (m, _) -> m = "agents.sessions.rename") !calls));
        let states =
          List.filter_map
            (fun (m, p) ->
              if m = "agents.sessions.setStatus" then Some (get "status" p)
              else None)
            !calls
        in
        check_bool "old terminal precedes fresh processing and terminal" true
          (states
          = [ str "processing"; str "active"; str "processing"; str "active" ]);
        let r = Option.get followup_run in
        check_bool "followup owned its own activation and terminal control" true
          (r.finished
          && List.exists
               (fun (m, p) ->
                 m = "chat.update"
                 && get "ts" p = str (Option.get r.control)
                 && get "blocks" p = arr [])
               !calls)
      end;
      check_bool "durable retirement and all native resources released" true
        (Memory.queue_list ~db:base.db ~session_key:key = []
        && live_runs base.manager = []
        && groups base.manager = []);
      Lwt.return_unit)

let test_bang_revoked_during_cancel () =
  let ui, release_ui = Lwt.wait () in
  let armed = ref false and authorizations = ref 0 in
  let delay m _ =
    if !armed && m = "auth.test" then begin
      incr authorizations;
      if !authorizations = 2 then ui else Lwt.return_unit
    end
    else Lwt.return_unit
  in
  with_recording ~delay (fun base _ _ _ _ provider run ->
      let held, release_provider = Lwt.task () in
      (provider := fun ~model:_ ~messages:_ -> held);
      let work = run (message "held native owner") in
      let* () = Test_slack_socket.settle 4 in
      armed := true;
      let bang =
        run ~id:"revoked-bang"
          (message ~thread:"100.000001" ~ts:"100.000013" "!revoked change")
      in
      let* () = Test_slack_socket.settle 4 in
      let held_authorization = !authorizations = 2 && Lwt.is_sleeping bang in
      reload_slack base (fun sc -> { sc with allow_users = [ "U456" ] });
      Lwt.wakeup_later release_ui ();
      let* _ = bang in
      let interrupted =
        Session.interrupt_check_if_present base.manager ~key ()
      in
      release_text release_provider "gpt-test";
      let* _ = work in
      let* () = Slack_thread_runs.shutdown base.manager in
      check_bool "bang cancel awaits fresh authorization" true
        held_authorization;
      Alcotest.(check (option string))
        "refused cancellation cannot write normalized interrupt" None
        interrupted;
      Lwt.return_unit)

let suite =
  [
    ( "bang revocation during cancellation authorization",
      `Quick,
      test_bang_revoked_during_cancel );
    ("documented DM metadata root followup and unrelated human", `Quick, test_dm);
    ("real pending question answer before deferral", `Quick, test_question);
    ("raw bang before model context prefix", `Quick, test_control ~bang:true);
    ("raw admin stop before deferred queue", `Quick, test_control ~bang:false);
  ]
