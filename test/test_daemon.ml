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

let string_contains haystack needle =
  let hay_len = String.length haystack and needle_len = String.length needle in
  let rec loop i =
    if i + needle_len > hay_len then false
    else if String.sub haystack i needle_len = needle then true
    else loop (i + 1)
  in
  needle_len = 0 || loop 0

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

let strip_ansi s = Str.global_replace (Str.regexp "\027\\[[0-9;]*m") "" s

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
          telegram =
            Some
              {
                accounts = [ ("main", telegram_account) ];
                text_coalesce_ms = 150;
              };
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

let test_dispatch_resumed_message_routes_discord () =
  let called = ref None in
  let senders =
    {
      Daemon.default_resume_senders with
      send_discord =
        (fun ~bot_token ~channel_id ~text ->
          called := Some (bot_token, channel_id, text);
          Lwt.return_unit);
    }
  in
  let config =
    {
      Runtime_config.default with
      channels =
        {
          Runtime_config.default.channels with
          discord =
            Some
              {
                bot_token = "discord-token";
                allow_guilds = [];
                allow_users = [];
                intents = 0;
              };
        };
    }
  in
  let result =
    Lwt_main.run
      (Daemon.dispatch_resumed_message ~senders ~config ~channel:"discord"
         ~channel_id:"chan-42" ~text:"hello" ())
  in
  Alcotest.(check (result unit string)) "dispatch ok" (Ok ()) result;
  Alcotest.(check (option (triple string string string)))
    "discord sender called"
    (Some ("discord-token", "chan-42", "hello"))
    !called

let test_dispatch_resumed_message_routes_slack () =
  let called = ref None in
  let senders =
    {
      Daemon.default_resume_senders with
      send_slack =
        (fun ~bot_token ~channel_id ~text ->
          called := Some (bot_token, channel_id, text);
          Lwt.return_unit);
    }
  in
  let config =
    {
      Runtime_config.default with
      channels =
        {
          Runtime_config.default.channels with
          slack =
            Some
              {
                bot_token = "slack-token";
                signing_secret = "secret";
                events_path = "/slack/events";
                allow_channels = [];
                allow_users = [];
                socket_mode = false;
                app_token = "";
              };
        };
    }
  in
  let result =
    Lwt_main.run
      (Daemon.dispatch_resumed_message ~senders ~config ~channel:"slack"
         ~channel_id:"C42" ~text:"hello" ())
  in
  Alcotest.(check (result unit string)) "dispatch ok" (Ok ()) result;
  Alcotest.(check (option (triple string string string)))
    "slack sender called"
    (Some ("slack-token", "C42", "hello"))
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

let test_default_resume_turn_uses_explicit_resume_prompt () =
  with_fake_chat_provider (fun base_config ->
      Alcotest.(check bool)
        "resume prompt is distinct from generic continuation prompt" true
        (Daemon.resume_turn_prompt <> Session.autonomous_continuation_prompt);
      Alcotest.(check bool)
        "resume prompt says to continue now" true
        (string_contains Daemon.resume_turn_prompt
           "This is the chance to continue that interrupted work now.");
      Alcotest.(check bool)
        "resume prompt names highest-priority unfinished task" true
        (string_contains Daemon.resume_turn_prompt
           "Resume the highest-priority unfinished task");
      Alcotest.(check bool)
        "resume prompt says not to wait for a user" true
        (string_contains Daemon.resume_turn_prompt
           "without waiting for a new user message");
      Alcotest.(check bool)
        "resume prompt keeps stay-idle escape hatch narrow" true
        (string_contains Daemon.resume_turn_prompt
           "Reply exactly STAY_IDLE only if");
      let db = Memory.init ~db_path:":memory:" () in
      let telegram_account =
        { Runtime_config.bot_token = "tg-token"; allow_from = []; totp = None }
      in
      let config =
        {
          base_config with
          channels =
            {
              base_config.channels with
              telegram =
                Some
                  {
                    accounts = [ ("main", telegram_account) ];
                    text_coalesce_ms = 150;
                  };
            };
        }
      in
      let session_manager = Session.create ~config ~db () in
      Session.record_agent_turn session_manager ~key:"telegram:42:user"
        ~channel:"telegram" ~channel_id:"42" ();
      let dispatched = ref [] in
      Lwt_main.run
        (Daemon.resume_agent_session ~session_manager ~config
           ~senders:
             {
               Daemon.default_resume_senders with
               send_telegram =
                 (fun ~bot_token:_ ~chat_id ~text ->
                   dispatched := (chat_id, text) :: !dispatched;
                   Lwt.return_unit);
             }
           ~session_key:"telegram:42:user" ~channel:"telegram" ~channel_id:"42"
           ());
      Alcotest.(check int)
        "injection label plus response dispatched" 2 (List.length !dispatched);
      let sent = List.rev !dispatched in
      let _ci1, label = List.hd sent in
      Alcotest.(check bool)
        "first dispatch is injection label" true
        (String.starts_with ~prefix:"[automatic restart-resume]" label);
      let _ci2, text = List.nth sent 1 in
      Alcotest.(check bool)
        "resume response is not empty" true
        (String.trim text <> "");
      let history = Memory.load_history ~db ~session_key:"telegram:42:user" in
      let resume_prompt_present =
        List.exists
          (fun (msg : Provider.message) ->
            (msg.role = "system" || msg.role = "user")
            && msg.content = Daemon.resume_turn_prompt)
          history
      in
      Alcotest.(check bool)
        "resume prompt persisted into history" true resume_prompt_present)

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
        "injection label plus compaction notice plus response" 3
        (List.length sent);
      Alcotest.(check bool)
        "first message is injection label" true
        (String.starts_with ~prefix:"[automatic restart-resume]"
           (List.nth sent 0));
      Alcotest.(check string)
        "second message is compaction notice" Session.compaction_notice
        (List.nth sent 1);
      Alcotest.(check bool)
        "third message is response" true
        (String.starts_with ~prefix:"reply:" (List.nth sent 2)))

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

let test_pp_header_with_ts_includes_time () =
  let output =
    strip_ansi
      (render_header_at
         (local_time ~year:2026 ~month:3 ~day:8 ~hour:10 ~minute:11 ~second:12))
  in
  let has_time =
    try
      ignore (Str.search_forward (Str.regexp_string "[10:11:12.") output 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool) "header includes time" true has_time

let test_pp_header_colorized () =
  let output =
    render_header_at
      (local_time ~year:2026 ~month:3 ~day:8 ~hour:10 ~minute:11 ~second:12)
  in
  let has_ansi =
    try
      ignore (Str.search_forward (Str.regexp "\027\\[") output 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool) "header contains ANSI codes" true has_ansi;
  let stripped = strip_ansi output in
  let has_level =
    try
      ignore (Str.search_forward (Str.regexp_string "INFO") stripped 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool) "header contains level tag" true has_level

let test_maybe_emit_date_banner_logs_first_entry_date () =
  let output =
    strip_ansi
      (render_date_banners
         [ local_time ~year:2026 ~month:3 ~day:8 ~hour:10 ~minute:0 ~second:0 ])
  in
  Alcotest.(check string) "first date banner" "=== 2026-03-08 ===\n" output

let test_maybe_emit_date_banner_logs_when_day_advances () =
  let output =
    strip_ansi
      (render_date_banners
         [
           local_time ~year:2026 ~month:3 ~day:8 ~hour:10 ~minute:0 ~second:0;
           local_time ~year:2026 ~month:3 ~day:8 ~hour:23 ~minute:59 ~second:59;
           local_time ~year:2026 ~month:3 ~day:9 ~hour:0 ~minute:0 ~second:0;
         ])
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

let test_post_dispatch_resumed_routed_session_arms_and_sends_follow_up () =
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
          telegram =
            Some
              {
                accounts = [ ("main", telegram_account) ];
                text_coalesce_ms = 150;
              };
        };
    }
  in
  let session_manager = Session.create ~config ~db () in
  let prompts = ref 0 in
  let sent = ref [] in
  Session.set_special_command_handler session_manager
    (fun ~key ~message ~send_progress:_ ~interrupt_check:_ ->
      if
        key = "telegram:42:user"
        && String.starts_with ~prefix:Session.autonomous_continuation_prompt
             message
      then begin
        incr prompts;
        if !prompts = 1 then Lwt.return_some "follow-up"
        else Lwt.return_some Session.autonomous_stay_idle_message
      end
      else Lwt.return_none);
  let senders =
    {
      Daemon.default_resume_senders with
      send_telegram =
        (fun ~bot_token:_ ~chat_id ~text ->
          sent := (chat_id, text) :: !sent;
          Lwt.return_unit);
    }
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       Daemon.post_dispatch_resumed_session_response ~continuation_delay:0.02
         ~senders ~session_manager ~config ~session_key:"telegram:42:user"
         ~channel:"telegram" ~channel_id:"42" ~response:"continue_work" ()
     in
     let* () = Lwt_unix.sleep 0.08 in
     Lwt.return_unit);
  let state =
    Hashtbl.find session_manager.Session.continuation_checks "telegram:42:user"
  in
  Alcotest.(check int) "continuation prompt ran twice" 2 !prompts;
  Alcotest.(check (list (pair string string)))
    "follow-up delivered to resumed telegram session"
    [ ("42", "follow-up") ]
    (List.rev !sent);
  Alcotest.(check bool)
    "routed continuation disarmed after stay idle" true state.disarmed

let test_post_dispatch_resumed_routed_session_disarms_on_stay_idle () =
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
          telegram =
            Some
              {
                accounts = [ ("main", telegram_account) ];
                text_coalesce_ms = 150;
              };
        };
    }
  in
  let session_manager = Session.create ~config ~db () in
  let sent = ref [] in
  let senders =
    {
      Daemon.default_resume_senders with
      send_telegram =
        (fun ~bot_token:_ ~chat_id ~text ->
          sent := (chat_id, text) :: !sent;
          Lwt.return_unit);
    }
  in
  Lwt_main.run
    (Daemon.post_dispatch_resumed_session_response ~continuation_delay:0.02
       ~senders ~session_manager ~config ~session_key:"telegram:42:user"
       ~channel:"telegram" ~channel_id:"42"
       ~response:Session.autonomous_stay_idle_message ());
  let state =
    Hashtbl.find session_manager.Session.continuation_checks "telegram:42:user"
  in
  Alcotest.(check bool)
    "stay idle disarms routed continuation" true state.disarmed;
  Alcotest.(check (list (pair string string))) "no follow-up sent" [] !sent

let test_handle_heartbeat_response_keeps_idle_heartbeat_idle () =
  let db = Memory.init ~db_path:":memory:" () in
  let session_manager = Session.create ~config:Runtime_config.default ~db () in
  let key = "__main__" in
  Lwt_main.run
    (Daemon.handle_heartbeat_response ~session_manager ~key
       ~response:" HEARTBEAT_OK " ());
  let state = Session.continuation_state session_manager ~key in
  Alcotest.(check bool)
    "heartbeat ok leaves continuation disarmed flag alone" false state.disarmed;
  Alcotest.(check bool)
    "heartbeat ok does not arm continuation" false
    (Option.is_some state.cancel)

let test_handle_heartbeat_response_disarms_stay_idle () =
  let db = Memory.init ~db_path:":memory:" () in
  let session_manager = Session.create ~config:Runtime_config.default ~db () in
  let key = "__main__" in
  let state = Session.continuation_state session_manager ~key in
  let _waiter, wakener = Lwt.wait () in
  state.Session.cancel <- Some wakener;
  Lwt_main.run
    (Daemon.handle_heartbeat_response ~session_manager ~key
       ~response:" STAY_IDLE " ());
  Alcotest.(check bool) "stay idle disarms continuation" true state.disarmed;
  Alcotest.(check bool)
    "stay idle clears pending continuation" false
    (Option.is_some state.cancel)

let test_handle_heartbeat_response_arms_follow_up_for_non_idle_reply () =
  let db = Memory.init ~db_path:":memory:" () in
  let session_manager = Session.create ~config:Runtime_config.default ~db () in
  let key = "__main__" in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       Daemon.handle_heartbeat_response ~session_manager ~key
         ~response:"continue_work" ()
     in
     Lwt.pause ());
  let state = Session.continuation_state session_manager ~key in
  Alcotest.(check bool)
    "non-idle heartbeat arms continuation" true
    (Option.is_some state.cancel);
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Session.cancel_autonomous_continuation session_manager ~key in
     Lwt.pause ())

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
          telegram =
            Some
              {
                accounts = [ ("main", telegram_account) ];
                text_coalesce_ms = 150;
              };
        };
    }
  in
  let session_manager = Session.create ~config ~db () in
  let dispatched = ref [] in
  let injected = ref [] in
  let senders =
    {
      Daemon.default_resume_senders with
      send_telegram =
        (fun ~bot_token ~chat_id ~text ->
          dispatched := (bot_token, chat_id, text) :: !dispatched;
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
    (let open Lwt.Syntax in
     let* () =
       Daemon.notify_background_task_finished ~continuation_delay:100.0 ~senders
         ~session_manager ~config task
     in
     let* () = Lwt.pause () in
     Session.cancel_autonomous_continuation session_manager
       ~key:"telegram:42:user");
  let dispatched_rev = List.rev !dispatched in
  Alcotest.(check int)
    "two dispatches: status + agent response" 2
    (List.length dispatched_rev);
  (match dispatched_rev with
  | [ (bt1, ci1, t1); (bt2, ci2, t2) ] ->
      Alcotest.(check string) "first bot_token" "tg-token" bt1;
      Alcotest.(check string) "first chat_id" "42" ci1;
      Alcotest.(check string)
        "first dispatch is status message"
        (Background_task.status_message task)
        t1;
      Alcotest.(check string) "second bot_token" "tg-token" bt2;
      Alcotest.(check string) "second chat_id" "42" ci2;
      Alcotest.(check string) "second dispatch is agent response" "woke up" t2
  | _ -> Alcotest.failf "expected exactly two dispatches");
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
         with Not_found -> false)
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

let test_background_task_wakeup_arms_autonomous_continuation () =
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
  let senders =
    {
      Daemon.default_resume_senders with
      send_telegram = (fun ~bot_token:_ ~chat_id:_ ~text:_ -> Lwt.return_unit);
    }
  in
  Session.set_special_command_handler session_manager
    (fun ~key ~message:_ ~send_progress:_ ~interrupt_check:_ ->
      if key = "telegram:42:user" then Lwt.return_some "still working"
      else Lwt.return_none);
  let task = make_test_task () in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       Daemon.notify_background_task_finished ~continuation_delay:100.0 ~senders
         ~session_manager ~config task
     in
     Lwt.pause ());
  let state =
    Session.continuation_state session_manager ~key:"telegram:42:user"
  in
  Alcotest.(check bool)
    "continuation armed after wakeup" true
    (Option.is_some state.cancel);
  Lwt_main.run
    (Session.cancel_autonomous_continuation session_manager
       ~key:"telegram:42:user")

let test_background_task_wakeup_stay_idle_disarms () =
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
  let senders =
    {
      Daemon.default_resume_senders with
      send_telegram = (fun ~bot_token:_ ~chat_id:_ ~text:_ -> Lwt.return_unit);
    }
  in
  Session.set_special_command_handler session_manager
    (fun ~key ~message:_ ~send_progress:_ ~interrupt_check:_ ->
      if key = "telegram:42:user" then
        Lwt.return_some Session.autonomous_stay_idle_message
      else Lwt.return_none);
  let task = make_test_task () in
  Lwt_main.run
    (Daemon.notify_background_task_finished ~continuation_delay:100.0 ~senders
       ~session_manager ~config task);
  let state =
    Session.continuation_state session_manager ~key:"telegram:42:user"
  in
  Alcotest.(check bool)
    "continuation disarmed after STAY_IDLE" true state.disarmed

let test_resume_agent_session_sends_visible_injection_prompt () =
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
  let sent = ref [] in
  let senders =
    {
      Daemon.default_resume_senders with
      send_telegram =
        (fun ~bot_token:_ ~chat_id:_ ~text ->
          sent := text :: !sent;
          Lwt.return_unit);
    }
  in
  Lwt_main.run
    (Daemon.resume_agent_session ~senders ~session_manager ~config
       ~session_key:"telegram:42:user" ~channel:"telegram" ~channel_id:"42"
       ~run_turn:(fun agent _interrupt ->
         agent.Agent.history <-
           Provider.make_message ~role:"assistant" ~content:"ok"
           :: agent.Agent.history;
         Lwt.return "ok")
       ());
  let sent_rev = List.rev !sent in
  Alcotest.(check bool)
    "at least two messages sent" true
    (List.length sent_rev >= 2);
  let first = List.hd sent_rev in
  Alcotest.(check bool)
    "first message is labeled injection" true
    (String.starts_with ~prefix:"[automatic restart-resume]" first);
  Alcotest.(check bool)
    "injection contains resume prompt" true
    (try
       ignore
         (Str.search_forward
            (Str.regexp_string "Automatic resume after daemon restart")
            first 0);
       true
     with Not_found -> false);
  let second = List.nth sent_rev 1 in
  Alcotest.(check string) "second message is response" "ok" second

let suite =
  [
    Alcotest.test_case "dispatch resumed message routes telegram" `Quick
      test_dispatch_resumed_message_routes_telegram;
    Alcotest.test_case "dispatch resumed message routes discord" `Quick
      test_dispatch_resumed_message_routes_discord;
    Alcotest.test_case "dispatch resumed message routes slack" `Quick
      test_dispatch_resumed_message_routes_slack;
    Alcotest.test_case
      "resume pending sessions marks missing channel info as sent" `Quick
      test_resume_pending_agent_sessions_marks_missing_channel_info;
    Alcotest.test_case "default resume turn uses explicit automatic prompt"
      `Quick test_default_resume_turn_uses_explicit_resume_prompt;
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
    Alcotest.test_case
      "post-dispatch resumed routed session sends continuation follow-up" `Quick
      test_post_dispatch_resumed_routed_session_arms_and_sends_follow_up;
    Alcotest.test_case
      "post-dispatch resumed routed session stays idle on STAY_IDLE" `Quick
      test_post_dispatch_resumed_routed_session_disarms_on_stay_idle;
    Alcotest.test_case "heartbeat ok stays idle without continuation" `Quick
      test_handle_heartbeat_response_keeps_idle_heartbeat_idle;
    Alcotest.test_case "heartbeat STAY_IDLE disarms continuation" `Quick
      test_handle_heartbeat_response_disarms_stay_idle;
    Alcotest.test_case "heartbeat work reply arms continuation" `Quick
      test_handle_heartbeat_response_arms_follow_up_for_non_idle_reply;
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
    Alcotest.test_case "pp_header_with_ts includes time" `Quick
      test_pp_header_with_ts_includes_time;
    Alcotest.test_case "pp_header colorized" `Quick test_pp_header_colorized;
    Alcotest.test_case "date banner logs first entry date" `Quick
      test_maybe_emit_date_banner_logs_first_entry_date;
    Alcotest.test_case "date banner logs on day rollover" `Quick
      test_maybe_emit_date_banner_logs_when_day_advances;
    Alcotest.test_case "background task wakeup arms autonomous continuation"
      `Quick test_background_task_wakeup_arms_autonomous_continuation;
    Alcotest.test_case "background task wakeup STAY_IDLE disarms" `Quick
      test_background_task_wakeup_stay_idle_disarms;
    Alcotest.test_case "resume agent session sends visible injection prompt"
      `Quick test_resume_agent_session_sends_visible_injection_prompt;
  ]
