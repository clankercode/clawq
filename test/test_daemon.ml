let query_single_text_option db sql =
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.TEXT s -> Some s
          | _ -> None)
      | _ -> None)

let test_dispatch_resumed_message_routes_telegram () =
  let called = ref None in
  let senders =
    {
      Daemon.default_resume_senders with
      send_telegram =
        (fun ~bot_token ~chat_id ~text ->
          called := Some (bot_token, chat_id, text);
          Lwt.return_unit);
    }
  in
  let telegram_account =
    { Runtime_config.bot_token = "tg-token"; allow_from = []; totp = None }
  in
  let config =
    {
      Runtime_config.default with
      channels =
        {
          Runtime_config.default.channels with
          telegram = Some { accounts = [ ("main", telegram_account) ] };
        };
    }
  in
  let result =
    Lwt_main.run
      (Daemon.dispatch_resumed_message ~senders ~config ~channel:"telegram"
         ~channel_id:"42" ~text:"hello" ())
  in
  Alcotest.(check (result unit string)) "dispatch ok" (Ok ()) result;
  Alcotest.(check (option (triple string string string)))
    "telegram sender called"
    (Some ("tg-token", "42", "hello"))
    !called

let test_resume_pending_agent_sessions_marks_missing_channel_info () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let session_manager = Session.create ~config ~db () in
  Session.record_agent_turn session_manager ~key:"resume:missing" ();
  let resumed = ref [] in
  Lwt_main.run
    (Daemon.resume_pending_agent_sessions ~session_manager ~config
       ~resume_one:(fun ~session_key ~channel ~channel_id ->
         resumed := (session_key, channel, channel_id) :: !resumed;
         Lwt.return_unit)
       ());
  Alcotest.(check int) "resume callback not called" 0 (List.length !resumed);
  Alcotest.(check (option string))
    "pending state cleared" (Some "user")
    (query_single_text_option db
       "SELECT turn FROM session_state WHERE session_key = 'resume:missing'")

let test_resume_agent_session_persists_response_and_marks_sent () =
  let db = Memory.init ~db_path:":memory:" () in
  let slack_config =
    {
      Runtime_config.bot_token = "xoxb-test";
      signing_secret = "secret";
      events_path = "/slack/events";
      allow_channels = [];
      allow_users = [];
      app_token = "";
      socket_mode = false;
    }
  in
  let config =
    {
      Runtime_config.default with
      channels =
        { Runtime_config.default.channels with slack = Some slack_config };
    }
  in
  let session_manager = Session.create ~config ~db () in
  Memory.store_message ~db ~session_key:"slack:c1:u1"
    (Provider.make_message ~role:"user" ~content:"hello");
  Session.record_agent_turn session_manager ~key:"slack:c1:u1" ~channel:"slack"
    ~channel_id:"c1" ();
  let dispatched = ref None in
  let senders =
    {
      Daemon.default_resume_senders with
      send_slack =
        (fun ~bot_token ~channel_id ~text ->
          dispatched := Some (bot_token, channel_id, text);
          Lwt.return_unit);
    }
  in
  let run_turn agent _interrupt =
    Alcotest.(check int) "history restored" 1 (List.length agent.Agent.history);
    agent.Agent.history <-
      Provider.make_message ~role:"assistant" ~content:"resumed"
      :: agent.Agent.history;
    Lwt.return "resumed"
  in
  Lwt_main.run
    (Daemon.resume_agent_session ~senders ~run_turn ~session_manager ~config
       ~session_key:"slack:c1:u1" ~channel:"slack" ~channel_id:"c1" ());
  let history = Memory.load_history ~db ~session_key:"slack:c1:u1" in
  Alcotest.(check int) "assistant response persisted" 2 (List.length history);
  Alcotest.(check string)
    "latest persisted role" "assistant" (List.nth history 1).Provider.role;
  Alcotest.(check (option (triple string string string)))
    "slack sender called"
    (Some ("xoxb-test", "c1", "resumed"))
    !dispatched;
  Alcotest.(check int)
    "pending session cleared" 0
    (List.length
       (Session.load_pending_agent_sessions session_manager
          ~max_age_seconds:3600))

let test_wait_for_drain_returns_when_in_flight_reaches_zero () =
  let config = Runtime_config.default in
  let session_manager = Session.create ~config () in
  session_manager.Session.in_flight_count := 1;
  Lwt.async (fun () ->
      let open Lwt.Syntax in
      let* () = Lwt_unix.sleep 0.05 in
      session_manager.Session.in_flight_count := 0;
      Lwt.return_unit);
  let timed_out = Lwt_main.run (Daemon.wait_for_drain ~session_manager ()) in
  Alcotest.(check bool) "drain completes before timeout" false timed_out;
  Alcotest.(check int)
    "in-flight drained" 0
    (Session.current_in_flight session_manager)

let test_wait_for_drain_reports_timeout () =
  let config = Runtime_config.default in
  let session_manager = Session.create ~config () in
  session_manager.Session.in_flight_count := 1;
  let timed_out =
    Lwt_main.run
      (Daemon.wait_for_drain ~attempts:1 ~sleep_seconds:0.0 ~session_manager ())
  in
  Alcotest.(check bool) "timeout reported" true timed_out

let test_send_drain_warnings_sends_scheduled_messages () =
  let config = Runtime_config.default in
  let session_manager = Session.create ~config () in
  let received = ref [] in
  Lwt_main.run
    (Session.with_registered_notifier session_manager ~key:"telegram:1:u"
       ~notify:(fun text ->
         received := text :: !received;
         Lwt.return_unit)
       (fun () ->
         Daemon.send_drain_warnings
           ~schedule:[ (0.0, "five"); (0.0, "ten") ]
           ~session_manager ~stop:(ref false) ()));
  Alcotest.(check (list string))
    "warnings delivered in order" [ "five"; "ten" ] (List.rev !received)

let suite =
  [
    Alcotest.test_case "dispatch resumed message routes telegram" `Quick
      test_dispatch_resumed_message_routes_telegram;
    Alcotest.test_case
      "resume pending sessions marks missing channel info as sent" `Quick
      test_resume_pending_agent_sessions_marks_missing_channel_info;
    Alcotest.test_case "resume agent session persists response and marks sent"
      `Quick test_resume_agent_session_persists_response_and_marks_sent;
    Alcotest.test_case "wait for drain returns when in-flight reaches zero"
      `Quick test_wait_for_drain_returns_when_in_flight_reaches_zero;
    Alcotest.test_case "wait for drain reports timeout" `Quick
      test_wait_for_drain_reports_timeout;
    Alcotest.test_case "send drain warnings sends scheduled messages" `Quick
      test_send_drain_warnings_sends_scheduled_messages;
  ]
