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

let local_time ~year ~month ~day ~hour ~minute ~second =
  fst
    (Unix.mktime
       {
         Unix.tm_sec = second;
         tm_min = minute;
         tm_hour = hour;
         tm_mday = day;
         tm_mon = month - 1;
         tm_year = year - 1900;
         tm_wday = 0;
         tm_yday = 0;
         tm_isdst = false;
       })

let render_header_at t =
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  Daemon.pp_header_with_ts ppf t (Logs.Info, None);
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let render_date_banners times =
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  let last_date = ref None in
  List.iter (Daemon.maybe_emit_date_banner ppf last_date) times;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

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

let test_restart_notify_write_read_roundtrip () =
  Restart_notify.write ~channel:"telegram" ~channel_id:"12345";
  let result = Restart_notify.read () in
  Restart_notify.remove ();
  Alcotest.(check (option (pair string string)))
    "roundtrip"
    (Some ("telegram", "12345"))
    result

let test_restart_notify_expired_marker () =
  let path = Restart_notify.path () in
  let json =
    `Assoc
      [
        ("channel", `String "discord");
        ("channel_id", `String "chan1");
        ("timestamp", `Float (Unix.gettimeofday () -. 600.0));
      ]
  in
  let oc = open_out path in
  output_string oc (Yojson.Safe.to_string json);
  close_out oc;
  let result = Restart_notify.read () in
  Alcotest.(check (option (pair string string))) "expired" None result;
  Alcotest.(check bool) "file cleaned up" false (Sys.file_exists path)

let test_restart_notify_missing_marker () =
  Restart_notify.remove ();
  let result = Restart_notify.read () in
  Alcotest.(check (option (pair string string))) "missing" None result

let test_parse_channel_from_key () =
  Alcotest.(check (option (pair string string)))
    "telegram key"
    (Some ("telegram", "123"))
    (Restart_notify.parse_channel_from_key "telegram:123:456");
  Alcotest.(check (option (pair string string)))
    "discord key"
    (Some ("discord", "chan"))
    (Restart_notify.parse_channel_from_key "discord:chan:user");
  Alcotest.(check (option (pair string string)))
    "slack key"
    (Some ("slack", "C01"))
    (Restart_notify.parse_channel_from_key "slack:C01:U01");
  Alcotest.(check (option (pair string string)))
    "main key" None
    (Restart_notify.parse_channel_from_key "__main__")

let test_pp_header_with_ts_includes_full_date () =
  let output =
    render_header_at
      (local_time ~year:2026 ~month:3 ~day:8 ~hour:10 ~minute:11 ~second:12)
  in
  let has_prefix =
    try
      ignore
        (Str.search_forward
           (Str.regexp_string "[2026-03-08 10:11:12.") output 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool) "header includes full date" true has_prefix

let test_maybe_emit_date_banner_logs_first_entry_date () =
  let output =
    render_date_banners
      [ local_time ~year:2026 ~month:3 ~day:8 ~hour:10 ~minute:0 ~second:0 ]
  in
  Alcotest.(check string) "first date banner" "=== 2026-03-08 ===\n" output

let test_maybe_emit_date_banner_logs_when_day_advances () =
  let output =
    render_date_banners
      [
        local_time ~year:2026 ~month:3 ~day:8 ~hour:10 ~minute:0 ~second:0;
        local_time ~year:2026 ~month:3 ~day:8 ~hour:23 ~minute:59 ~second:59;
        local_time ~year:2026 ~month:3 ~day:9 ~hour:0 ~minute:0 ~second:0;
      ]
  in
  Alcotest.(check string)
    "date rollover banners" "=== 2026-03-08 ===\n=== 2026-03-09 ===\n" output

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
    Alcotest.test_case "restart notify write/read roundtrip" `Quick
      test_restart_notify_write_read_roundtrip;
    Alcotest.test_case "restart notify expired marker" `Quick
      test_restart_notify_expired_marker;
    Alcotest.test_case "restart notify missing marker" `Quick
      test_restart_notify_missing_marker;
    Alcotest.test_case "parse channel from key" `Quick
      test_parse_channel_from_key;
    Alcotest.test_case "pp_header_with_ts includes full date" `Quick
      test_pp_header_with_ts_includes_full_date;
    Alcotest.test_case "date banner logs first entry date" `Quick
      test_maybe_emit_date_banner_logs_first_entry_date;
    Alcotest.test_case "date banner logs on day rollover" `Quick
      test_maybe_emit_date_banner_logs_when_day_advances;
  ]
