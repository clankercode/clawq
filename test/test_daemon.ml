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

let free_port () =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close sock)
    (fun () ->
      Unix.setsockopt sock Unix.SO_REUSEADDR true;
      Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      match Unix.getsockname sock with
      | Unix.ADDR_INET (_, port) -> port
      | Unix.ADDR_UNIX _ -> Alcotest.fail "expected inet socket")

let make_fake_provider_config base_url : Runtime_config.provider_config =
  {
    api_key = "test-key";
    kind = None;
    base_url = Some base_url;
    default_model = Some "fake-model";
    project_id = None;
    location = None;
    service_account_json = None;
    thinking_budget_tokens = None;
    oai_thinking_style = "none";
    codex_oauth = None;
  }

let with_fake_chat_provider f =
  let port = free_port () in
  let callback _conn req body =
    let open Lwt.Syntax in
    let* body_text = Cohttp_lwt.Body.to_string body in
    let json = Yojson.Safe.from_string body_text in
    let open Yojson.Safe.Util in
    let messages = json |> member "messages" |> to_list in
    let user_messages =
      messages
      |> List.filter_map (fun msg ->
          try
            if msg |> member "role" |> to_string = "user" then
              Some (msg |> member "content" |> to_string)
            else None
          with _ -> None)
    in
    let latest = match List.rev user_messages with x :: _ -> x | [] -> "" in
    let response_body =
      Yojson.Safe.to_string
        (`Assoc
           [
             ("id", `String "cmpl_fake");
             ("object", `String "chat.completion");
             ("model", `String "fake-model");
             ( "choices",
               `List
                 [
                   `Assoc
                     [
                       ("index", `Int 0);
                       ( "message",
                         `Assoc
                           [
                             ("role", `String "assistant");
                             ("content", `String ("reply:" ^ latest));
                           ] );
                       ("finish_reason", `String "stop");
                     ];
                 ] );
           ])
    in
    Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:response_body ()
  in
  let server =
    Cohttp_lwt_unix.Server.create
      ~mode:(`TCP (`Port port))
      (Cohttp_lwt_unix.Server.make ~callback ())
  in
  let stop, stopper = Lwt.wait () in
  Lwt.async (fun () -> Lwt.pick [ server; stop ]);
  Fun.protect
    ~finally:(fun () -> Lwt.wakeup_later stopper ())
    (fun () ->
      let config =
        {
          Runtime_config.default with
          default_provider = Some "fake";
          providers =
            [
              ( "fake",
                make_fake_provider_config
                  (Printf.sprintf "http://127.0.0.1:%d" port) );
            ];
          prompt =
            { Runtime_config.default.prompt with dynamic_enabled = false };
          security =
            { Runtime_config.default.security with tools_enabled = false };
          agent_defaults =
            {
              Runtime_config.default.agent_defaults with
              primary_model = "fake-model";
              show_thinking = false;
              show_tool_calls = false;
            };
        }
      in
      f config)

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

let test_resume_agent_session_sends_compaction_notice () =
  with_fake_chat_provider (fun base_config ->
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
          base_config with
          channels = { base_config.channels with slack = Some slack_config };
          memory = { base_config.memory with max_messages_per_session = 21 };
          model_context_limits = [ ("fake-model", 1000) ];
        }
      in
      let session_manager = Session.create ~config ~db () in
      for i = 1 to 21 do
        Memory.store_message ~db ~session_key:"slack:c1:u1"
          (Provider.make_message ~role:"user"
             ~content:
               (Printf.sprintf "%s seed message %02d" (String.make 200 'x') i))
      done;
      Session.record_agent_turn session_manager ~key:"slack:c1:u1"
        ~channel:"slack" ~channel_id:"c1" ();
      let dispatched = ref [] in
      let senders =
        {
          Daemon.default_resume_senders with
          send_slack =
            (fun ~bot_token:_ ~channel_id:_ ~text ->
              dispatched := text :: !dispatched;
              Lwt.return_unit);
        }
      in
      Lwt_main.run
        (Daemon.resume_agent_session ~senders ~session_manager ~config
           ~session_key:"slack:c1:u1" ~channel:"slack" ~channel_id:"c1" ());
      let sent = List.rev !dispatched in
      Alcotest.(check int)
        "compaction notice plus response" 2 (List.length sent);
      Alcotest.(check string)
        "first message is compaction notice" Session.compaction_notice
        (List.nth sent 0);
      Alcotest.(check bool)
        "second message is response" true
        (String.starts_with ~prefix:"reply:" (List.nth sent 1)))

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
           (Str.regexp_string "[2026-03-08 10:11:12.")
           output 0);
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

let test_resume_pending_main_session_arms_autonomous_continuation () =
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
  Session.record_agent_turn session_manager ~key:"__main__" ~channel:"slack"
    ~channel_id:"c1" ();
  let resumed = ref [] in
  let resume_one ~session_key ~channel ~channel_id =
    Daemon.resume_agent_session ~session_manager ~config ~session_key ~channel
      ~channel_id
      ~senders:
        {
          Daemon.default_resume_senders with
          send_slack =
            (fun ~bot_token:_ ~channel_id:_ ~text ->
              resumed := text :: !resumed;
              Lwt.return_unit);
        }
      ~run_turn:(fun agent _interrupt ->
        agent.Agent.history <-
          Provider.make_message ~role:"assistant" ~content:"continue_work"
          :: agent.Agent.history;
        Lwt.return "continue_work")
      ~after_dispatch:(fun ~response:_ ->
        let state =
          Session.continuation_state session_manager ~key:session_key
        in
        let _waiter, wakener = Lwt.wait () in
        state.Session.cancel <- Some wakener;
        Lwt.return_unit)
      ()
  in
  Lwt_main.run
    (Daemon.resume_pending_agent_sessions ~session_manager ~config ~resume_one
       ());
  let state =
    Hashtbl.find session_manager.Session.continuation_checks "__main__"
  in
  Alcotest.(check bool) "main session not disarmed" false state.disarmed;
  Alcotest.(check bool) "continuation armed" true (Option.is_some state.cancel)

let make_test_task ?(id = 9) ?(session_key = Some "telegram:42:user")
    ?(channel = Some "telegram") ?(channel_id = Some "42") () :
    Background_task.task =
  {
    id;
    runner = Background_task.Codex;
    model = Some "gpt-5.4";
    repo_path = "/repo";
    prompt = "investigate";
    branch = Printf.sprintf "clawq-bg-%d" id;
    worktree_path = Some (Printf.sprintf "/tmp/task-%d" id);
    log_path = Some (Printf.sprintf "/tmp/task-%d.log" id);
    status = Background_task.Succeeded;
    session_key;
    channel;
    channel_id;
    pid = None;
    result_preview = Some "ok";
    created_at = "2026-03-09 00:00:00";
    started_at = Some "2026-03-09 00:00:01";
    finished_at = Some "2026-03-09 00:00:02";
  }

let test_notify_background_task_finished_dispatches_and_injects_wakeup () =
  let db = Memory.init ~db_path:":memory:" () in
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
  let session_manager = Session.create ~config ~db () in
  let dispatched = ref None in
  let injected = ref [] in
  let senders =
    {
      Daemon.default_resume_senders with
      send_telegram =
        (fun ~bot_token ~chat_id ~text ->
          dispatched := Some (bot_token, chat_id, text);
          Lwt.return_unit);
    }
  in
  Session.set_special_command_handler session_manager
    (fun ~key ~message ~send_progress:_ ~interrupt_check:_ ->
      if key = "telegram:42:user" then begin
        injected := message :: !injected;
        Lwt.return_some "woke up"
      end
      else Lwt.return_none);
  let task = make_test_task () in
  Lwt_main.run
    (Daemon.notify_background_task_finished ~senders ~session_manager ~config
       task);
  Alcotest.(check (option (triple string string string)))
    "telegram sender called"
    (Some ("tg-token", "42", Background_task.status_message task))
    !dispatched;
  match List.rev !injected with
  | [ message ] ->
      Alcotest.(check bool)
        "automatic label present" true
        (String.starts_with
           ~prefix:"[automatic background-task completion notice]" message);
      Alcotest.(check bool)
        "mentions automatic wake-up" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "wakes in the same session")
                message 0);
           true
         with Not_found -> false);
      Alcotest.(check int)
        "one automatic wake-up injection observed" 1 (List.length !injected)
  | msgs ->
      Alcotest.failf "expected exactly one injected wake-up message, got %d"
        (List.length msgs)

let test_notify_background_task_finished_queues_wakeup_when_session_busy () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let session_manager = Session.create ~config ~db () in
  Session.register_channel_notifier session_manager ~key:"telegram:42:user"
    (fun _ -> Lwt.return_unit);
  let blocker, release = Lwt.wait () in
  let seen = ref [] in
  Session.set_special_command_handler session_manager
    (fun ~key ~message ~send_progress:_ ~interrupt_check:_ ->
      if key = "telegram:42:user" && message = "hold" then blocker
      else if key = "telegram:42:user" then begin
        seen := message :: !seen;
        Lwt.return_some "processed"
      end
      else Lwt.return_none);
  Lwt.async (fun () ->
      let open Lwt.Syntax in
      let* _ =
        Session.turn session_manager ~key:"telegram:42:user" ~message:"hold" ()
      in
      Lwt.return_unit);
  Unix.sleepf 0.05;
  let task = make_test_task ~id:12 () in
  Lwt_main.run
    (Daemon.notify_background_task_finished ~session_manager ~config task);
  let wake_messages_before =
    List.filter
      (fun message ->
        String.starts_with
          ~prefix:"[automatic background-task completion notice]" message)
      !seen
  in
  Alcotest.(check int)
    "exactly one automatic wake-up injection observed" 1
    (List.length wake_messages_before);
  Lwt.wakeup_later release (Some "released");
  Unix.sleepf 0.05;
  let wake_messages =
    List.filter
      (fun message ->
        String.starts_with
          ~prefix:"[automatic background-task completion notice]" message)
      !seen
  in
  match List.rev wake_messages with
  | [ message ] ->
      Alcotest.(check bool)
        "queued automatic label present" true
        (String.starts_with
           ~prefix:"[automatic background-task completion notice]" message)
  | msgs ->
      Alcotest.failf "expected one queued wake-up message, got %d"
        (List.length msgs)

let suite =
  [
    Alcotest.test_case "dispatch resumed message routes telegram" `Quick
      test_dispatch_resumed_message_routes_telegram;
    Alcotest.test_case
      "resume pending sessions marks missing channel info as sent" `Quick
      test_resume_pending_agent_sessions_marks_missing_channel_info;
    Alcotest.test_case "resume agent session persists response and marks sent"
      `Quick test_resume_agent_session_persists_response_and_marks_sent;
    Alcotest.test_case "background completion dispatches and injects wake-up"
      `Quick test_notify_background_task_finished_dispatches_and_injects_wakeup;
    Alcotest.test_case "background completion queues wake-up when session busy"
      `Quick
      test_notify_background_task_finished_queues_wakeup_when_session_busy;
    Alcotest.test_case "resume agent session sends compaction notice" `Quick
      test_resume_agent_session_sends_compaction_notice;
    Alcotest.test_case
      "resume pending main session arms autonomous continuation" `Quick
      test_resume_pending_main_session_arms_autonomous_continuation;
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
