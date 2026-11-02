open Lwt.Syntax
open Test_slack_fix1_fixtures

let test_preparation_reload ~queued () =
  let gate, release = Lwt.wait () in
  let block = ref false in
  let delay m _ =
    if !block && m = "agents.sessions.rename" then gate else Lwt.return_unit
  in
  with_recording ~delay (fun base _ _ _ _ provider run ->
      let first, release_first = Lwt.task () in
      let invoked = ref 0 in
      (provider :=
         fun ~model ~messages:_ ->
           incr invoked;
           if queued && !invoked = 1 then first
           else Lwt.return (text model "ran"));
      let work = run (message "first work") in
      let* () = Test_slack_socket.settle 4 in
      let* () =
        if queued then begin
          let* _ =
            run ~id:"q-prep"
              (message ~thread:"100.000001" ~ts:"100.000008" "queued work")
          in
          Lwt.return_unit
        end
        else Lwt.return_unit
      in
      (* For the initial path, block before starting it, below. *)
      let* _ = if queued then Lwt.return "ok" else work in
      invoked := 0;
      block := true;
      let target =
        if queued then begin
          release_text release_first "gpt-test";
          work
        end
        else
          run ~id:"initial-prep"
            (message ~thread:"100.000001" ~ts:"100.000009" "new work")
      in
      let* () = Test_slack_socket.settle 6 in
      reload_slack base (fun sc -> { sc with allow_users = [ "U456" ] });
      Lwt.wakeup_later release ();
      let* _ = target in
      Alcotest.(check int)
        "reload during UI preparation prevents model start" 0 !invoked;
      if queued then
        check_bool "queued request retained truthfully on preparation reload"
          true
          (Memory.queue_list ~db:base.db ~session_key:key <> []);
      check_bool "refused preparation releases native ownership" true
        (live_runs base.manager = [] && groups base.manager = []);
      Lwt.return_unit)

let test_mixed_append () =
  with_recording (fun base _ _ _ _ provider run ->
      let first, release = Lwt.task () in
      let invoked = ref 0 and leaked = ref false in
      (provider :=
         fun ~model ~messages ->
           incr invoked;
           if !invoked = 1 then first
           else begin
             leaked :=
               List.exists
                 (fun (m : Provider.message) ->
                   contains m.content "revoked guest addition")
                 messages;
             Lwt.return (text model "drained")
           end);
      let work = run (message "held owner") in
      let* () = Test_slack_socket.settle 3 in
      let* _ =
        run ~id:"admin-q"
          (message ~thread:"100.000001" ~ts:"100.000003"
             "allowed original followup")
      in
      let* _ =
        run ~id:"guest-append"
          (message ~user:"U456" ~thread:"100.000001" ~ts:"100.000004"
             "/followup-append revoked guest addition")
      in
      reload_slack base (fun sc -> { sc with allow_users = [ "U123" ] });
      release_text release "gpt-test";
      let* _ = work in
      check_bool "different human append never inherits earlier requester" false
        !leaked;
      let rows = Memory.queue_list ~db:base.db ~session_key:key in
      check_bool "revoked appended request retained with own provenance" true
        (List.exists
           (fun (r : Memory.queue_row) ->
             contains r.payload_json "U456" && r.last_error <> None)
           rows);
      Lwt_list.iter_s Slack_thread_runs.finish (live_runs base.manager))

let child_key =
  Room_session.child_thread_key ~connector:"slack" ~profile_id:"room-agent"
    ~room_id:"C123" ~thread_id:"100.000001" ~source_message_id:"100.000002" ()

let test_child_progress () =
  with_recording (fun base calls credentials _ _ _ _ ->
      let _, origin_json, thread_id, _ =
        Background_task_context.origin_fields_from_context ~db:base.db
          ~context:{ Tool.default_context with session_key = Some child_key }
          ()
      in
      let task =
        {
          (origin_task ()) with
          id = 8891;
          session_key = Some child_key;
          origin_json;
          thread_id;
        }
      in
      Room_progress.clear_progress_msg_id ~task_id:task.id;
      let old = base.manager.config in
      let* _ =
        Daemon_util.deliver_room_progress ~config:old
          ~session_manager:base.manager task
      in
      let delivered =
        List.exists
          (fun (m, p) ->
            m = "chat.postMessage"
            && get "channel" p = str "C123"
            && get "thread_ts" p = str "100.000001")
          !calls
      in
      reload_slack base (fun sc ->
          { sc with bot_token = "xoxb-rotated-fixture" });
      credentials := [];
      let* _ =
        Daemon_util.deliver_room_progress ~config:old
          ~session_manager:base.manager task
      in
      let rotated =
        !credentials <> []
        && List.for_all
             (fun (_, t) -> t = Some "Bearer xoxb-rotated-fixture")
             !credentials
      in
      deny_egress base "slack:C123";
      calls := [];
      let* _ =
        Daemon_util.deliver_room_progress ~config:old
          ~session_manager:base.manager task
      in
      Room_progress.clear_progress_msg_id ~task_id:task.id;
      check_bool "typed child-origin progress preserves room/root" true
        delivered;
      check_bool "child retained progress selects current token" true rotated;
      check_bool "child room egress remains enforced" true (writes calls = []);
      Lwt.return_unit)

let test_progress_mismatch () =
  with_recording (fun base calls _ _ _ _ _ ->
      let sc = Option.get base.manager.config.channels.slack in
      let cb =
        Connector_room_progress.slack_callbacks ~session_manager:base.manager
          ~session_key:child_key sc
      in
      let* () =
        ignore_error (fun () ->
            let* _ =
              cb.send ~room_id:"C999" ~thread_id:"100.000001"
                ~text:"mismatch room" ()
            in
            Lwt.return_unit)
      in
      let* () =
        ignore_error (fun () ->
            let* _ =
              cb.send ~room_id:"C123" ~thread_id:"100.000099"
                ~text:"mismatch root" ()
            in
            Lwt.return_unit)
      in
      let arbitrary =
        Connector_room_progress.slack_callbacks ~session_manager:base.manager
          ~session_key:"slack:C123:U123" sc
      in
      let* () =
        ignore_error (fun () ->
            let* _ =
              arbitrary.send ~room_id:"C123" ~thread_id:"100.000001"
                ~text:"unbound root" ()
            in
            Lwt.return_unit)
      in
      check_bool "child/missing locator mismatch never reaches transport" true
        (writes calls = []);
      Lwt.return_unit)

let test_held_reload ~rotation () =
  with_recording (fun base calls credentials _ _ provider run ->
      let held, release = Lwt.task () in
      (provider := fun ~model:_ ~messages:_ -> held);
      let work = run (message "held before config reload") in
      let* () = Test_slack_socket.settle 4 in
      reload_slack base (fun sc ->
          {
            sc with
            bot_token = (if rotation then "xoxb-rotated-fixture" else "");
          });
      calls := [];
      credentials := [];
      release_text release "gpt-test";
      let* _ =
        Lwt.catch
          (fun () -> work)
          (function
            | Slack_thread_api.Delivery_denied _ ->
                Lwt.return "delivery refused"
            | exn -> Lwt.fail exn)
      in
      if rotation then
        check_bool
          "all terminal transport uses current credential including reactions"
          true
          (List.for_all
             (fun (_, t) -> t = Some "Bearer xoxb-rotated-fixture")
             !credentials)
      else
        check_bool "no terminal transport after disable including reactions"
          true (!calls = []);
      check_bool "reload completion releases lifecycle resources" true
        (live_runs base.manager = [] && groups base.manager = []);
      Lwt.return_unit)

let suite =
  [
    ( "initial preparation config change fails closed",
      `Quick,
      test_preparation_reload ~queued:false );
    ( "queued preparation config change retains request",
      `Quick,
      test_preparation_reload ~queued:true );
    ( "mixed author append retains original requester isolation",
      `Quick,
      test_mixed_append );
    ( "typed legacy child thread progress current config and egress",
      `Quick,
      test_child_progress );
    ( "child and arbitrary progress mismatch refused",
      `Quick,
      test_progress_mismatch );
    ( "held completion refuses every write after disable",
      `Quick,
      test_held_reload ~rotation:false );
    ( "held completion rotates all terminal credentials",
      `Quick,
      test_held_reload ~rotation:true );
  ]
