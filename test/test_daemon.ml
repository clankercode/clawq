let make_fake_provider_config base_url : Runtime_config.provider_config =
  {
    Runtime_config.default_provider_config with
    api_key = "test-key";
    base_url = Some base_url;
    default_model = Some "fake-model";
  }

let with_fake_chat_provider ?on_request f =
  let port = Test_helpers.free_port () in
  let callback _conn req body =
    let open Lwt.Syntax in
    let* body_text = Cohttp_lwt.Body.to_string body in
    let json = Yojson.Safe.from_string body_text in
    (match on_request with Some cb -> cb json | None -> ());
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
              subagent_default_model = Some "fake-native:subagent-default";
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

let write_file path body =
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc body)

let with_temp_git_repo f =
  let dir = Filename.temp_file "clawq-daemon-test-repo" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let cmd =
    Printf.sprintf
      "git -C %s init -q && git -C %s config user.name Test && git -C %s \
       config user.email t@t && git -C %s commit --allow-empty -m init -q"
      (Filename.quote dir) (Filename.quote dir) (Filename.quote dir)
      (Filename.quote dir)
  in
  if Sys.command cmd <> 0 then Alcotest.fail "failed to initialize git repo";
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir))))
    (fun () -> f dir)

let test_boot_stage_message_helpers () =
  Alcotest.(check string)
    "start message" "Boot: mcp-setup start"
    (Daemon.boot_stage_start_message "mcp-setup");
  Alcotest.(check string)
    "done message includes elapsed and detail"
    "Boot: durable-replay done elapsed=0.125s replayed=2 failed=1"
    (Daemon.boot_stage_done_message ~detail:"replayed=2 failed=1"
       ~elapsed_s:0.125 "durable-replay")

let make_now times =
  let remaining = ref times in
  fun () ->
    match !remaining with
    | value :: rest ->
        remaining := rest;
        value
    | [] -> failwith "test now exhausted"

let test_with_boot_stage_logging_success () =
  let messages = ref [] in
  let now = make_now [ 100.0; 100.125 ] in
  let result =
    Lwt_main.run
      (Daemon.with_boot_stage_logging ~now
         ~log_message:(fun msg -> messages := !messages @ [ msg ])
         ~detail_of_result:(fun count -> Printf.sprintf "count=%d" count)
         "sample-stage"
         (fun () -> Lwt.return 3))
  in
  Alcotest.(check int) "result propagated" 3 result;
  Alcotest.(check (list string))
    "messages emitted"
    [
      "Boot: sample-stage start";
      "Boot: sample-stage done elapsed=0.125s count=3";
    ]
    !messages

let test_with_boot_stage_logging_error () =
  let messages = ref [] in
  let now = make_now [ 200.0; 200.050 ] in
  let result =
    Lwt.catch
      (fun () ->
        let open Lwt.Syntax in
        let* () =
          Daemon.with_boot_stage_logging ~now
            ~log_message:(fun msg -> messages := !messages @ [ msg ])
            "sample-stage"
            (fun () -> Lwt.fail (Failure "boom"))
        in
        Lwt.return "ok")
      (fun exn -> Lwt.return (Printexc.to_string exn))
    |> Lwt_main.run
  in
  Alcotest.(check string) "error reraised" "Failure(\"boom\")" result;
  Alcotest.(check (list string))
    "messages emitted"
    [
      "Boot: sample-stage start";
      "Boot: sample-stage done elapsed=0.050s status=error \
       error=Failure(\"boom\")";
    ]
    !messages

let test_setup_mcp_clients_loads_configs_and_registers_tools () =
  Test_helpers.with_temp_home (fun home ->
      let clawq_dir = Filename.concat home ".clawq" in
      Unix.mkdir clawq_dir 0o755;
      write_file
        (Filename.concat clawq_dir "mcp_servers.json")
        {|
[
  {
    "name": "local",
    "command": "uvx",
    "args": ["mcp-server", "--stdio"],
    "env": {"TOKEN": "abc"}
  },
  {
    "name": "remote",
    "url": "https://mcp.example.test/rpc",
    "headers": {"Authorization": "Bearer token"}
  }
]
|};
      let registry = Tool_registry.create () in
      let mcp_clients = ref [] in
      let connected = ref [] in
      let connect_client ?startup_timeout_s:_ ?http_post:_ cfg =
        connected := cfg :: !connected;
        let tool_name = cfg.Mcp_client.name ^ "_tool" in
        let tool =
          {
            Tool.name = tool_name;
            description = "fake MCP tool";
            parameters_schema = `Assoc [ ("type", `String "object") ];
            invoke = (fun ?context:_ _args -> Lwt.return tool_name);
            invoke_stream = None;
            risk_level = Tool.Low;
            deferred = false;
          }
        in
        let client =
          {
            Mcp_client.config = cfg;
            transport =
              Mcp_client.Http
                {
                  url = "https://unused.example.test/rpc";
                  headers = [];
                  post =
                    (fun ~url:_ ~headers:_ ~body:_ ->
                      Lwt.return (200, "{}", "application/json"));
                };
            next_id = 1;
            discovered = [ tool ];
          }
        in
        Lwt.return client
      in
      Lwt_main.run
        (Daemon.setup_mcp_clients ~connect_client ~registry ~mcp_clients ());
      let connected = List.rev !connected in
      Alcotest.(check int) "two configs loaded" 2 (List.length connected);
      let local_cfg = List.nth connected 0 in
      let remote_cfg = List.nth connected 1 in
      Alcotest.(check string) "local config name" "local" local_cfg.name;
      Alcotest.(check (list string))
        "local args"
        [ "mcp-server"; "--stdio" ]
        local_cfg.args;
      Alcotest.(check (list (pair string string)))
        "local env"
        [ ("TOKEN", "abc") ]
        local_cfg.env;
      Alcotest.(check string) "remote config name" "remote" remote_cfg.name;
      Alcotest.(check string)
        "remote url stored in command" "https://mcp.example.test/rpc"
        remote_cfg.command;
      Alcotest.(check (list (pair string string)))
        "remote headers"
        [ ("Authorization", "Bearer token") ]
        remote_cfg.env;
      Alcotest.(check (list string))
        "registered MCP tools"
        [ "local_tool"; "remote_tool" ]
        (Tool_registry.list registry |> List.map (fun t -> t.Tool.name));
      Alcotest.(check int) "clients tracked" 2 (List.length !mcp_clients))

let test_boot_startup_stage_sequence_without_full_daemon () =
  Test_helpers.with_temp_home (fun home ->
      let clawq_dir = Filename.concat home ".clawq" in
      Unix.mkdir clawq_dir 0o755;
      write_file
        (Filename.concat clawq_dir "mcp_servers.json")
        {|
[
  {
    "name": "remote",
    "url": "https://mcp.example.test/rpc",
    "headers": {"Authorization": "Bearer token"}
  }
]
|};
      let db = Memory.init ~db_path:":memory:" () in
      let config = Runtime_config.default in
      let session_manager = Session.create ~config ~db () in
      Session.record_agent_turn session_manager ~key:"resume:ok"
        ~channel:"slack" ~channel_id:"C1" ();
      ignore
        (Memory.queue_enqueue ~db ~session_key:"slack:C2:user" ~source:"cli"
           ~payload_json:
             (Yojson.Safe.to_string
                (`Assoc [ ("message", `String "queued"); ("bang", `Bool false) ])));
      let registry = Tool_registry.create () in
      let mcp_clients = ref [] in
      let connected = ref [] in
      let resumed = ref [] in
      let replayed = ref [] in
      let messages = ref [] in
      let now = make_now [ 10.0; 10.100; 20.0; 20.125; 30.0; 30.250 ] in
      let log_message msg = messages := !messages @ [ msg ] in
      let connect_client ?startup_timeout_s:_ ?http_post:_ cfg =
        connected := cfg :: !connected;
        let tool =
          {
            Tool.name = "remote_tool";
            description = "fake MCP tool";
            parameters_schema = `Assoc [ ("type", `String "object") ];
            invoke = (fun ?context:_ _args -> Lwt.return "remote_tool");
            invoke_stream = None;
            risk_level = Tool.Low;
            deferred = false;
          }
        in
        let client =
          {
            Mcp_client.config = cfg;
            transport =
              Mcp_client.Http
                {
                  url = "https://unused.example.test/rpc";
                  headers = [];
                  post =
                    (fun ~url:_ ~headers:_ ~body:_ ->
                      Lwt.return (200, "{}", "application/json"));
                };
            next_id = 1;
            discovered = [ tool ];
          }
        in
        Lwt.return client
      in
      let resume_one ~session_key ~channel ~channel_id =
        resumed := (session_key, channel, channel_id) :: !resumed;
        Lwt.return_unit
      in
      let replay_turn _mgr ~key ~message ?cwd:_ () =
        replayed := (key, message) :: !replayed;
        Lwt.return "ok"
      in
      let open Lwt.Syntax in
      Lwt_main.run
        (let* () =
           Daemon.run_mcp_setup_stage ~now ~log_message ~connect_client
             ~tool_registry:(Some registry) ~config ~mcp_clients ()
         in
         let* _resume_summary =
           Daemon.run_pending_session_resume_stage ~now ~log_message ~resume_one
             ~session_manager ~config ()
         in
         let* _replay_summary =
           Daemon.run_durable_replay_stage ~now ~log_message ~replay_turn
             ~session_manager ~config ()
         in
         Lwt.return_unit);
      Alcotest.(check int) "one mcp config connected" 1 (List.length !connected);
      Alcotest.(check (list string))
        "mcp tool registered" [ "remote_tool" ]
        (Tool_registry.list registry |> List.map (fun t -> t.Tool.name));
      Alcotest.(check int) "one mcp client tracked" 1 (List.length !mcp_clients);
      Alcotest.(check (list (triple string string string)))
        "pending session resumed"
        [ ("resume:ok", "slack", "C1") ]
        !resumed;
      Alcotest.(check (list (pair string string)))
        "durable queue replayed"
        [ ("slack:C2:user", "queued") ]
        !replayed;
      Alcotest.(check int)
        "durable queue drained" 0
        (Memory.queue_count ~db ~session_key:"slack:C2:user");
      Alcotest.(check (list string))
        "boot stage logs"
        [
          "Boot: mcp-setup start";
          "Boot: mcp-setup done elapsed=0.100s";
          "Boot: pending-session-resume start";
          "Boot: pending-session-resume done elapsed=0.125s pending=1 \
           resumed=1 missing_channel=0 failed=0";
          "Boot: durable-replay start";
          "Boot: durable-replay done elapsed=0.250s sessions=1 rows=1 \
           reclaimed_stale=0 reclaimed_failed=0 replayed=1 failed=0";
        ]
        !messages)

let test_boot_startup_stage_sequence_continues_after_mcp_failure () =
  Test_helpers.with_temp_home (fun home ->
      let clawq_dir = Filename.concat home ".clawq" in
      Unix.mkdir clawq_dir 0o755;
      write_file
        (Filename.concat clawq_dir "mcp_servers.json")
        {|
[
  {
    "name": "remote",
    "url": "https://mcp.example.test/rpc"
  }
]
|};
      let db = Memory.init ~db_path:":memory:" () in
      let config = Runtime_config.default in
      let session_manager = Session.create ~config ~db () in
      Session.record_agent_turn session_manager ~key:"resume:ok"
        ~channel:"slack" ~channel_id:"C1" ();
      ignore
        (Memory.queue_enqueue ~db ~session_key:"slack:C2:user" ~source:"cli"
           ~payload_json:
             (Yojson.Safe.to_string
                (`Assoc [ ("message", `String "queued"); ("bang", `Bool false) ])));
      let registry = Tool_registry.create () in
      let mcp_clients = ref [] in
      let resumed = ref [] in
      let replayed = ref [] in
      let messages = ref [] in
      let now = make_now [ 10.0; 10.100; 20.0; 20.125; 30.0; 30.250 ] in
      let log_message msg = messages := !messages @ [ msg ] in
      let connect_client ?startup_timeout_s:_ ?http_post:_ _cfg =
        Lwt.fail_with "boom"
      in
      let resume_one ~session_key ~channel ~channel_id =
        resumed := (session_key, channel, channel_id) :: !resumed;
        Lwt.return_unit
      in
      let replay_turn _mgr ~key ~message ?cwd:_ () =
        replayed := (key, message) :: !replayed;
        Lwt.return "ok"
      in
      let open Lwt.Syntax in
      Lwt_main.run
        (let* () =
           Daemon.run_mcp_setup_stage ~now ~log_message ~connect_client
             ~tool_registry:(Some registry) ~config ~mcp_clients ()
         in
         let* _resume_summary =
           Daemon.run_pending_session_resume_stage ~now ~log_message ~resume_one
             ~session_manager ~config ()
         in
         let* _replay_summary =
           Daemon.run_durable_replay_stage ~now ~log_message ~replay_turn
             ~session_manager ~config ()
         in
         Lwt.return_unit);
      Alcotest.(check int) "no mcp clients tracked" 0 (List.length !mcp_clients);
      Alcotest.(check (list string))
        "no mcp tools registered" []
        (Tool_registry.list registry |> List.map (fun t -> t.Tool.name));
      Alcotest.(check (list (triple string string string)))
        "pending session still resumed"
        [ ("resume:ok", "slack", "C1") ]
        !resumed;
      Alcotest.(check (list (pair string string)))
        "durable queue still replayed"
        [ ("slack:C2:user", "queued") ]
        !replayed;
      Alcotest.(check (list string))
        "boot stages still complete"
        [
          "Boot: mcp-setup start";
          "Boot: mcp-setup done elapsed=0.100s";
          "Boot: pending-session-resume start";
          "Boot: pending-session-resume done elapsed=0.125s pending=1 \
           resumed=1 missing_channel=0 failed=0";
          "Boot: durable-replay start";
          "Boot: durable-replay done elapsed=0.250s sessions=1 rows=1 \
           reclaimed_stale=0 reclaimed_failed=0 replayed=1 failed=0";
        ]
        !messages)

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
                default_model = None;
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
                default_model = None;
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
                default_model = None;
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

let test_dispatch_resumed_message_routes_github () =
  let config = Runtime_config.default in
  let result =
    Lwt_main.run
      (Daemon.dispatch_resumed_message ~config ~channel:"github"
         ~channel_id:"acme/backend" ~text:"done" ())
  in
  Alcotest.(check (result unit string)) "github dispatch ok" (Ok ()) result

(* B666: when Teams.send_message returns "" (the failure signal —
   missing/invalid service_url, OAuth token missing, HTTP error), the
   dispatcher must surface an Error so the cron scheduler doesn't log
   "delivery succeeded" after a Teams ERROR. *)
let test_dispatch_resumed_message_teams_empty_activity_is_error () =
  let senders =
    {
      Daemon.default_resume_senders with
      send_teams = (fun ~config:_ ~channel_id:_ ~text:_ -> Lwt.return "");
    }
  in
  let teams_cfg =
    {
      Runtime_config.app_id = "app";
      app_secret = "secret";
      tenant_id = "tenant";
      service_url = "https://example.test/";
      webhook_path = "/teams/webhook";
      allow_teams = [];
      allow_users = [];
      mention_mode = "first";
      default_model = None;
      file_consent_cards = false;
    }
  in
  let config =
    {
      Runtime_config.default with
      channels = { Runtime_config.default.channels with teams = Some teams_cfg };
    }
  in
  let result =
    Lwt_main.run
      (Daemon.dispatch_resumed_message ~senders ~config ~channel:"teams"
         ~channel_id:"|conv-1" ~text:"hi" ())
  in
  match result with
  | Ok () ->
      Alcotest.fail "expected Error when teams send returned empty activity_id"
  | Error msg ->
      Alcotest.(check bool)
        "error mentions teams send" true
        (try
           ignore (Str.search_forward (Str.regexp_string "teams send") msg 0);
           true
         with Not_found -> false)

let test_dispatch_resumed_message_teams_non_empty_activity_is_ok () =
  let senders =
    {
      Daemon.default_resume_senders with
      send_teams =
        (fun ~config:_ ~channel_id:_ ~text:_ -> Lwt.return "activity-123");
    }
  in
  let teams_cfg =
    {
      Runtime_config.app_id = "app";
      app_secret = "secret";
      tenant_id = "tenant";
      service_url = "https://example.test/";
      webhook_path = "/teams/webhook";
      allow_teams = [];
      allow_users = [];
      mention_mode = "first";
      default_model = None;
      file_consent_cards = false;
    }
  in
  let config =
    {
      Runtime_config.default with
      channels = { Runtime_config.default.channels with teams = Some teams_cfg };
    }
  in
  let result =
    Lwt_main.run
      (Daemon.dispatch_resumed_message ~senders ~config ~channel:"teams"
         ~channel_id:"|conv-1" ~text:"hi" ())
  in
  Alcotest.(check (result unit string)) "teams dispatch ok" (Ok ()) result

let test_resume_pending_agent_sessions_marks_missing_channel_info () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let session_manager = Session.create ~config ~db () in
  Session.record_agent_turn session_manager ~key:"resume:missing" ();
  let resumed = ref [] in
  let summary =
    Lwt_main.run
      (Daemon.resume_pending_agent_sessions ~session_manager ~config
         ~resume_one:(fun ~session_key ~channel ~channel_id ->
           resumed := (session_key, channel, channel_id) :: !resumed;
           Lwt.return_unit)
         ())
  in
  Alcotest.(check int) "resume callback not called" 0 (List.length !resumed);
  Alcotest.(check int) "summary pending count" 1 summary.pending_count;
  Alcotest.(check int)
    "summary missing channel count" 1 summary.missing_channel_count;
  Alcotest.(check int) "summary resumed count" 0 summary.resumed_count;
  Alcotest.(check int) "summary failed count" 0 summary.failed_count;
  Alcotest.(check (option string))
    "pending state cleared" (Some "user")
    (Test_helpers.query_single_text_option db
       "SELECT turn FROM session_state WHERE session_key = 'resume:missing'")

let test_resume_pending_agent_sessions_summary_counts () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let session_manager = Session.create ~config ~db () in
  Session.record_agent_turn session_manager ~key:"resume:ok" ~channel:"slack"
    ~channel_id:"c1" ();
  Session.record_agent_turn session_manager ~key:"resume:missing" ();
  Session.record_agent_turn session_manager ~key:"resume:fail" ~channel:"slack"
    ~channel_id:"c2" ();
  let resumed = ref [] in
  let summary =
    Lwt_main.run
      (Daemon.resume_pending_agent_sessions ~session_manager ~config
         ~resume_one:(fun ~session_key ~channel ~channel_id ->
           resumed := (session_key, channel, channel_id) :: !resumed;
           if session_key = "resume:fail" then Lwt.fail_with "resume failed"
           else Lwt.return_unit)
         ())
  in
  Alcotest.(check int) "summary pending count" 3 summary.pending_count;
  Alcotest.(check int) "summary resumed count" 1 summary.resumed_count;
  Alcotest.(check int)
    "summary missing channel count" 1 summary.missing_channel_count;
  Alcotest.(check int) "summary failed count" 1 summary.failed_count;
  Alcotest.(check int)
    "resume callback ran for routed rows" 2 (List.length !resumed)

let test_default_resume_turn_uses_explicit_resume_prompt () =
  with_fake_chat_provider (fun base_config ->
      Alcotest.(check bool)
        "resume prompt is distinct from generic continuation prompt" true
        (Daemon.resume_turn_prompt <> Session.autonomous_continuation_prompt);
      Alcotest.(check bool)
        "resume prompt commands immediate resumption" true
        (Test_helpers.string_contains Daemon.resume_turn_prompt
           "Resume the interrupted work now");
      Alcotest.(check bool)
        "resume prompt names highest-priority unfinished task" true
        (Test_helpers.string_contains Daemon.resume_turn_prompt
           "highest-priority unfinished task");
      Alcotest.(check bool)
        "resume prompt says not to wait for a user" true
        (Test_helpers.string_contains Daemon.resume_turn_prompt
           "do not wait for a follow-up message");
      Alcotest.(check bool)
        "stay-idle not mentioned in resume prompt" false
        (Test_helpers.string_contains Daemon.resume_turn_prompt "STAY_IDLE");
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
                    default_model = None;
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
            msg.role = "user" && msg.content = Daemon.resume_turn_prompt)
          history
      in
      Alcotest.(check bool)
        "resume prompt persisted into history as a user message" true
        resume_prompt_present)

(* Regression for the zai_coding/glm HTTP 400 code 1214 ("messages parameter
   is illegal") seen during automatic restart-resume of a workflow_run session.
   The resume turn injected resume_turn_prompt as a trailing `system` message
   into an otherwise-empty history, so the OpenAI-compatible payload sent to
   z.ai contained no `user` message and ended with a `system` message — which
   z.ai rejects. This test captures the actual messages payload the provider
   receives during a resume turn and asserts it is OpenAI-compat valid:
   at least one `user` message, and no trailing `system`/`developer` message. *)
let test_resume_turn_payload_is_openai_compat_valid () =
  let port = Test_helpers.free_port () in
  let captured : (string * string) list option ref = ref None in
  let callback _conn _req body =
    let open Lwt.Syntax in
    let* body_text = Cohttp_lwt.Body.to_string body in
    let json = Yojson.Safe.from_string body_text in
    let open Yojson.Safe.Util in
    let messages = json |> member "messages" |> to_list in
    let role_content =
      List.map
        (fun m ->
          let role = try m |> member "role" |> to_string with _ -> "" in
          let content = try m |> member "content" |> to_string with _ -> "" in
          (role, content))
        messages
    in
    captured := Some role_content;
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
                             ("content", `String "resumed-ok");
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
      let db = Memory.init ~db_path:":memory:" () in
      let telegram_account =
        { Runtime_config.bot_token = "tg-token"; allow_from = []; totp = None }
      in
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
          channels =
            {
              Runtime_config.default.channels with
              telegram =
                Some
                  {
                    accounts = [ ("main", telegram_account) ];
                    text_coalesce_ms = 150;
                    default_model = None;
                  };
            };
        }
      in
      let session_manager = Session.create ~config ~db () in
      Session.record_agent_turn session_manager ~key:"telegram:42:user"
        ~channel:"telegram" ~channel_id:"42" ();
      Lwt_main.run
        (Daemon.resume_agent_session ~session_manager ~config
           ~senders:
             {
               Daemon.default_resume_senders with
               send_telegram =
                 (fun ~bot_token:_ ~chat_id:_ ~text:_ -> Lwt.return_unit);
             }
           ~session_key:"telegram:42:user" ~channel:"telegram" ~channel_id:"42"
           ());
      let payload =
        match !captured with
        | Some p -> p
        | None -> Alcotest.fail "provider never received a resume-turn payload"
      in
      let roles = List.map fst payload in
      Alcotest.(check bool)
        "resume payload contains at least one user message" true
        (List.exists (fun r -> r = "user") roles);
      let last_role = match List.rev roles with r :: _ -> r | [] -> "" in
      Alcotest.(check bool)
        "resume payload does not end with a system/developer message" true
        (last_role <> "system" && last_role <> "developer");
      Alcotest.(check bool)
        "resume prompt delivered as a user message" true
        (List.exists
           (fun (role, content) ->
             role = "user"
             && Test_helpers.string_contains content Daemon.resume_turn_prompt)
           payload);
      let history = Memory.load_history ~db ~session_key:"telegram:42:user" in
      let durable_resume_prompts =
        List.filter
          (fun (m : Provider.message) ->
            m.role = "user" && m.content = Daemon.resume_turn_prompt)
          history
      in
      Alcotest.(check int)
        "resume prompt stored exactly once in durable history" 1
        (List.length durable_resume_prompts))

let test_resume_agent_session_sends_debug_summary () =
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
          default_model = None;
        }
      in
      let config =
        {
          base_config with
          channels = { base_config.channels with slack = Some slack_config };
        }
      in
      let session_manager = Session.create ~config ~db () in
      let key = "slack:c1:u1" in
      Session.record_agent_turn session_manager ~key ~channel:"slack"
        ~channel_id:"c1" ();
      (match Session.set_session_debug session_manager ~key ~enabled:true with
      | Ok () -> ()
      | Error msg -> Alcotest.fail msg);
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
           ~session_key:key ~channel:"slack" ~channel_id:"c1" ());
      let sent = List.rev !dispatched in
      Alcotest.(check int)
        "injection label plus debug summary plus response" 3 (List.length sent);
      Alcotest.(check bool)
        "first message is injection label" true
        (String.starts_with ~prefix:"[automatic restart-resume]"
           (List.nth sent 0));
      Alcotest.(check bool)
        "second message is debug summary" true
        (Test_helpers.string_contains (List.nth sent 1)
           "debug: llm provider=fake");
      Alcotest.(check bool)
        "debug summary includes model" true
        (Test_helpers.string_contains (List.nth sent 1) "model=fake-model");
      Alcotest.(check bool)
        "third message is response" true
        (String.starts_with ~prefix:"reply:" (List.nth sent 2)))

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
      default_model = None;
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
          default_model = None;
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
      Alcotest.(check bool)
        "second message is compaction notice" true
        (String.starts_with ~prefix:"\xF0\x9F\x97\x9C" (List.nth sent 1));
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

let test_start_draining_interrupts_all_sessions () =
  let config = Runtime_config.default in
  let session_manager = Session.create ~config () in
  (* Manually insert sessions: a channel session and a non-channel session *)
  let make_session () =
    let agent = Agent.create ~config () in
    let mutex = Lwt_mutex.create () in
    let interrupt = ref None in
    (agent, mutex, interrupt)
  in
  let main_triple = make_session () in
  let chan_triple = make_session () in
  Hashtbl.replace session_manager.Session.sessions "__main__" main_triple;
  Hashtbl.replace session_manager.Session.sessions "telegram:123:u" chan_triple;
  Lwt_main.run (Session.start_draining session_manager);
  let _, _, main_interrupt = main_triple in
  let _, _, chan_interrupt = chan_triple in
  Alcotest.(check bool)
    "__main__ session interrupted" true
    (!main_interrupt = Some Agent.restart_interrupt_token);
  Alcotest.(check bool)
    "channel session interrupted" true
    (!chan_interrupt = Some Agent.restart_interrupt_token);
  Alcotest.(check bool)
    "draining flag set" true
    (Session.is_draining session_manager)

let test_send_drain_warnings_does_not_notify_channel () =
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
           ~stop:(ref false) ()));
  Alcotest.(check (list string)) "warnings not sent to channel" [] !received

let test_restart_signal_duplicate_delta_recent () =
  let now = 100.0 in
  let expected = Daemon.restart_signal_duplicate_window_seconds /. 2.0 in
  let actual =
    Daemon.restart_signal_duplicate_delta ~now ~last_signal_at:(now -. expected)
  in
  Alcotest.(check (option (float 1e-6)))
    "recent SIGUSR1 is treated as duplicate" (Some expected) actual

let test_restart_signal_duplicate_delta_outside_window () =
  let now = 100.0 in
  let actual =
    Daemon.restart_signal_duplicate_delta ~now
      ~last_signal_at:
        (now -. (Daemon.restart_signal_duplicate_window_seconds +. 0.1))
  in
  Alcotest.(check (option (float 1e-6)))
    "older SIGUSR1 is not treated as duplicate" None actual

let test_restart_signal_duplicate_delta_negative_delta () =
  let now = 100.0 in
  let actual =
    Daemon.restart_signal_duplicate_delta ~now ~last_signal_at:(now +. 1.0)
  in
  Alcotest.(check (option (float 1e-6)))
    "future timestamp is ignored" None actual

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
    (Some ("telegram", "123:456"))
    (Restart_notify.parse_channel_from_key "telegram:123:456");
  Alcotest.(check (option (pair string string)))
    "discord key"
    (Some ("discord", "chan:user"))
    (Restart_notify.parse_channel_from_key "discord:chan:user");
  Alcotest.(check (option (pair string string)))
    "slack key"
    (Some ("slack", "C01:U01"))
    (Restart_notify.parse_channel_from_key "slack:C01:U01");
  Alcotest.(check (option (pair string string)))
    "teams key"
    (Some ("teams", "|19:3ed169b9886a4a1faadc1dc20687cc66@thread.v2"))
    (Restart_notify.parse_channel_from_key
       "teams:personal:19:3ed169b9886a4a1faadc1dc20687cc66@thread.v2");
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
  let expects_ansi = Option.is_none (Sys.getenv_opt "NO_COLOR") in
  Alcotest.(check bool) "header ANSI follows NO_COLOR" expects_ansi has_ansi;
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
      default_model = None;
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
  ignore
    (Lwt_main.run
       (Daemon.resume_pending_agent_sessions ~session_manager ~config
          ~resume_one ()));
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
                default_model = None;
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
                default_model = None;
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

let test_handle_heartbeat_response_sends_initial_reply_to_session () =
  let db = Memory.init ~db_path:":memory:" () in
  let session_manager = Session.create ~config:Runtime_config.default ~db () in
  let key = "telegram:42:user" in
  let sent = ref [] in
  Session.register_channel_notifier session_manager ~key (fun text ->
      sent := text :: !sent;
      Lwt.return_unit);
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       Daemon.handle_heartbeat_response ~session_manager ~key
         ~response:"follow up on this" ()
     in
     Lwt.pause ());
  Alcotest.(check (list string))
    "heartbeat reply sent to session" [ "follow up on this" ] !sent;
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
    automerge = false;
    use_worktree = true;
    merge_status = None;
    retry_count = 0;
    parent_task_id = None;
    replaced_by = None;
    runner_session_id = None;
    acp = false;
    agent_name = None;
    notification_status = None;
    notification_error = None;
    notification_attempts = 0;
    follow_up_prompt = None;
  }

let test_local_background_turn_template_persists_history_and_model () =
  let seen_models = ref [] in
  let seen_user_counts = ref [] in
  let old_native_complete = !Provider.native_complete in
  Fun.protect
    ~finally:(fun () -> Provider.native_complete := old_native_complete)
    (fun () ->
      Provider.register_native_complete Provider.Cohere
        (fun
          ~config:_ ~provider:_ ~model ~messages ?tools:_ ?session_key:_ () ->
          let user_count =
            messages
            |> List.filter (fun (msg : Provider.message) -> msg.role = "user")
            |> List.length
          in
          seen_models := model :: !seen_models;
          seen_user_counts := user_count :: !seen_user_counts;
          let latest =
            messages
            |> List.filter_map (fun (msg : Provider.message) ->
                if msg.role = "user" then Some msg.content else None)
            |> List.rev
            |> function
            | latest :: _ -> latest
            | [] -> ""
          in
          Lwt.return
            (Provider.Text
               {
                 content = "reply:" ^ latest;
                 usage = None;
                 model;
                 provider_response_items_json = None;
                 thinking = None;
               }));
      let config =
        {
          Runtime_config.default with
          default_provider = Some "fake-native";
          providers =
            [
              ( "fake-native",
                {
                  Runtime_config.default_provider_config with
                  api_key = "test-key";
                  kind = Some "cohere";
                  default_model = Some "fake-model";
                } );
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
      let db = Memory.init ~db_path:":memory:" () in
      let session_manager = Session.create ~config ~db () in
      let key = "__bg_task:77" in
      let model = "xiaomi-token-plan-sgp:mimo-v2.5-pro" in
      let noop_history _msgs = Lwt.return_unit in
      let no_interrupt () = None in
      Lwt_main.run
        (let open Lwt.Syntax in
         let* first =
           Daemon.run_local_background_turn ~session_manager ~key
             ~message:"first local template turn" ~model ~agent_name:"coder"
             ~interrupt_check:no_interrupt ~on_history_update:noop_history ()
         in
         Alcotest.(check bool)
           "first fake response" true
           (Test_helpers.string_contains first "first local template turn");
         let* second =
           Daemon.run_local_background_turn ~session_manager ~key
             ~message:"second local template turn" ~model ~agent_name:"coder"
             ~interrupt_check:no_interrupt ~on_history_update:noop_history ()
         in
         Alcotest.(check bool)
           "second fake response" true
           (Test_helpers.string_contains second "second local template turn");
         Lwt.return_unit);
      Alcotest.(check (list string))
        "explicit model sent for both turns" [ model; model ]
        (List.rev !seen_models);
      Alcotest.(check (list int))
        "second turn includes prior user history" [ 1; 2 ]
        (List.rev !seen_user_counts);
      let history = Memory.load_history ~db ~session_key:key in
      Alcotest.(check int) "four messages persisted" 4 (List.length history);
      Alcotest.(check bool)
        "stable transcript sees second reply" true
        (Test_helpers.string_contains
           (Background_task_transcript.render ~db ~id:77 ~regex:"second" ())
           "second local template turn"))

let test_local_background_turn_missing_template_fails () =
  Test_helpers.with_temp_home (fun _home ->
      let db = Memory.init ~db_path:":memory:" () in
      let session_manager =
        Session.create ~config:Runtime_config.default ~db ()
      in
      let noop_history _msgs = Lwt.return_unit in
      let no_interrupt () = None in
      let result =
        Lwt_main.run
          (Lwt.catch
             (fun () ->
               let open Lwt.Syntax in
               let* _ =
                 Daemon.run_local_background_turn ~session_manager
                   ~key:"__bg_task:404" ~message:"hello"
                   ~agent_name:"definitely-missing-template"
                   ~interrupt_check:no_interrupt ~on_history_update:noop_history
                   ()
               in
               Lwt.return "unexpected success")
             (fun exn -> Lwt.return (Printexc.to_string exn)))
      in
      Alcotest.(check bool)
        "missing template fails local turn" true
        (Test_helpers.string_contains result
           "agent template 'definitely-missing-template' not found"))

let test_local_background_turn_template_model_precedence () =
  Test_helpers.with_temp_home (fun home ->
      ignore (Agent_template.init_cache ());
      let agents_dir =
        Filename.concat (Filename.concat home ".clawq") "agents"
      in
      let ensure_dir dir =
        if not (Sys.file_exists dir) then Unix.mkdir dir 0o755
      in
      ensure_dir (Filename.concat home ".clawq");
      ensure_dir agents_dir;
      let template_path = Filename.concat agents_dir "modelled-local.md" in
      let oc = open_out template_path in
      Fun.protect
        ~finally:(fun () -> close_out_noerr oc)
        (fun () ->
          output_string oc
            "---\n\
             name: modelled-local\n\
             description: Model precedence regression\n\
             role: coder\n\
             model: fake-native:template-choice\n\
             ---\n\
             You are a modelled local test agent.\n");
      let seen_models = ref [] in
      let old_native_complete = !Provider.native_complete in
      Fun.protect
        ~finally:(fun () -> Provider.native_complete := old_native_complete)
        (fun () ->
          Provider.register_native_complete Provider.Cohere
            (fun
              ~config:_
              ~provider:_
              ~model
              ~messages:_
              ?tools:_
              ?session_key:_
              ()
            ->
              seen_models := model :: !seen_models;
              Lwt.return
                (Provider.Text
                   {
                     content = "template model response";
                     usage = None;
                     model;
                     provider_response_items_json = None;
                     thinking = None;
                   }));
          let config =
            {
              Runtime_config.default with
              default_provider = Some "fake-native";
              providers =
                [
                  ( "fake-native",
                    {
                      Runtime_config.default_provider_config with
                      api_key = "test-key";
                      kind = Some "cohere";
                      default_model = Some "fake-native:global-default";
                    } );
                ];
              prompt =
                { Runtime_config.default.prompt with dynamic_enabled = false };
              security =
                { Runtime_config.default.security with tools_enabled = false };
              agent_defaults =
                {
                  Runtime_config.default.agent_defaults with
                  primary_model = "fake-native:global-default";
                  subagent_default_model = Some "fake-native:subagent-default";
                  show_thinking = false;
                  show_tool_calls = false;
                };
            }
          in
          let db = Memory.init ~db_path:":memory:" () in
          let session_manager = Session.create ~config ~db () in
          ignore
            (Lwt_main.run
               (Daemon.run_local_background_turn ~session_manager
                  ~key:"__bg_task:78" ~message:"template model turn"
                  ~agent_name:"modelled-local"
                  ~interrupt_check:(fun () -> None)
                  ~on_history_update:(fun _ -> Lwt.return_unit)
                  ()));
          Alcotest.(check (list string))
            "template model beats subagent default and global"
            [ "template-choice" ] (List.rev !seen_models)))

let test_local_background_turn_no_template_subagent_default_model () =
  let seen_models = ref [] in
  let old_native_complete = !Provider.native_complete in
  Fun.protect
    ~finally:(fun () -> Provider.native_complete := old_native_complete)
    (fun () ->
      Provider.register_native_complete Provider.Cohere
        (fun
          ~config:_ ~provider:_ ~model ~messages:_ ?tools:_ ?session_key:_ () ->
          seen_models := model :: !seen_models;
          Lwt.return
            (Provider.Text
               {
                 content = "subagent default model response";
                 usage = None;
                 model;
                 provider_response_items_json = None;
                 thinking = None;
               }));
      let config =
        {
          Runtime_config.default with
          default_provider = Some "fake-native";
          providers =
            [
              ( "fake-native",
                {
                  Runtime_config.default_provider_config with
                  api_key = "test-key";
                  kind = Some "cohere";
                  default_model = Some "fake-native:global-default";
                } );
            ];
          prompt =
            { Runtime_config.default.prompt with dynamic_enabled = false };
          security =
            { Runtime_config.default.security with tools_enabled = false };
          agent_defaults =
            {
              Runtime_config.default.agent_defaults with
              primary_model = "fake-native:global-default";
              subagent_default_model = Some "fake-native:subagent-default";
              show_thinking = false;
              show_tool_calls = false;
            };
        }
      in
      let db = Memory.init ~db_path:":memory:" () in
      let session_manager = Session.create ~config ~db () in
      ignore
        (Lwt_main.run
           (Daemon.run_local_background_turn ~session_manager
              ~key:"__bg_task:79" ~message:"plain local turn"
              ~interrupt_check:(fun () -> None)
              ~on_history_update:(fun _ -> Lwt.return_unit)
              ()));
      Alcotest.(check (list string))
        "subagent default beats global when no explicit/template model"
        [ "subagent-default" ] (List.rev !seen_models))

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
                default_model = None;
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
      Alcotest.(check bool)
        "first dispatch is channel notification" true
        (Test_helpers.string_contains t1
           "Background task #9 finished: SUCCEEDED");
      Alcotest.(check string) "second bot_token" "tg-token" bt2;
      Alcotest.(check string) "second chat_id" "42" ci2;
      Alcotest.(check string) "second dispatch is agent response" "woke up" t2
  | _ -> Alcotest.failf "expected exactly two dispatches");
  match List.rev !injected with
  | [ message ] ->
      Alcotest.(check bool)
        "terse finished message" true
        (String.starts_with ~prefix:"[bg #" message);
      Alcotest.(check bool)
        "mentions succeeded or failed" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp "succeeded\\|failed\\|cancelled")
                message 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "includes bounded result preview" true
        (Test_helpers.string_contains message "Result preview: ok");
      Alcotest.(check bool)
        "points to subagent transcript command" true
        (Test_helpers.string_contains message "subagents transcript 9")
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
      (fun message -> String.starts_with ~prefix:"[bg #" message)
      !seen
  in
  Alcotest.(check int)
    "exactly one automatic wake-up injection observed" 1
    (List.length wake_messages_before);
  Lwt.wakeup_later release (Some "released");
  Unix.sleepf 0.05;
  let wake_messages =
    List.filter
      (fun message -> String.starts_with ~prefix:"[bg #" message)
      !seen
  in
  match List.rev wake_messages with
  | [ message ] ->
      Alcotest.(check bool)
        "queued terse label present" true
        (String.starts_with ~prefix:"[bg #" message);
      Alcotest.(check bool)
        "queued wake has transcript pointer" true
        (Test_helpers.string_contains message "subagents transcript 12")
  | msgs ->
      Alcotest.failf "expected one queued wake-up message, got %d"
        (List.length msgs)

let test_notify_background_task_finished_dirty_worktree_dispatches_finalize_hint
    () =
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
                default_model = None;
              };
        };
    }
  in
  let session_manager = Session.create ~config ~db () in
  let dispatched = ref [] in
  let senders =
    {
      Daemon.default_resume_senders with
      send_telegram =
        (fun ~bot_token:_ ~chat_id:_ ~text ->
          dispatched := text :: !dispatched;
          Lwt.return_unit);
    }
  in
  Session.set_special_command_handler session_manager
    (fun ~key ~message:_ ~send_progress:_ ~interrupt_check:_ ->
      if key = "telegram:42:user" then Lwt.return_some "ok" else Lwt.return_none);
  let task =
    {
      (make_test_task ()) with
      Background_task.status = Background_task.DirtyWorktree;
      worktree_path = Some "/some/path";
      result_preview = Some "Task left uncommitted changes";
    }
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       Daemon.notify_background_task_finished ~continuation_delay:100.0 ~senders
         ~session_manager ~config task
     in
     let* () = Lwt.pause () in
     Session.cancel_autonomous_continuation session_manager
       ~key:"telegram:42:user");
  let status_text =
    match List.rev !dispatched with
    | t :: _ -> t
    | [] -> Alcotest.fail "expected at least one dispatch"
  in
  Alcotest.(check bool)
    "dispatched text contains result_preview" true
    (try
       ignore
         (Str.search_forward
            (Str.regexp_string "Task left uncommitted changes")
            status_text 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "dispatched text contains finalize hint" true
    (try
       ignore
         (Str.search_forward
            (Str.regexp_string "background finalize")
            status_text 0);
       true
     with Not_found -> false)

let test_inject_bg_task_completion_registers_notifier () =
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
                default_model = None;
              };
        };
    }
  in
  let session_manager = Session.create ~config ~db () in
  let notifier_present_during_turn = ref false in
  let senders =
    {
      Daemon.default_resume_senders with
      send_telegram = (fun ~bot_token:_ ~chat_id:_ ~text:_ -> Lwt.return_unit);
    }
  in
  Session.set_special_command_handler session_manager
    (fun ~key ~message:_ ~send_progress:_ ~interrupt_check:_ ->
      if key = "telegram:42:user" then begin
        notifier_present_during_turn :=
          Option.is_some (Session.find_registered_notifier session_manager ~key);
        Lwt.return_some "handled"
      end
      else Lwt.return_none);
  let task = make_test_task () in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       Daemon.inject_background_task_completion ~continuation_delay:100.0
         ~senders ~session_manager ~config ~session_key:"telegram:42:user"
         ~channel:"telegram" ~channel_id:"42" task
     in
     Session.cancel_autonomous_continuation session_manager
       ~key:"telegram:42:user");
  Alcotest.(check bool)
    "notifier registered during bg task turn" true
    !notifier_present_during_turn

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
          telegram =
            Some
              {
                accounts = [ ("main", telegram_account) ];
                text_coalesce_ms = 150;
                default_model = None;
              };
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
          telegram =
            Some
              {
                accounts = [ ("main", telegram_account) ];
                text_coalesce_ms = 150;
                default_model = None;
              };
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
  with_fake_chat_provider (fun base_config ->
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
                    default_model = None;
                  };
            };
        }
      in
      let session_manager = Session.create ~config ~db () in
      Memory.store_message ~db ~session_key:"telegram:42:user"
        (Provider.make_message ~role:"user"
           ~content:"please continue after restart");
      Session.record_agent_turn session_manager ~key:"telegram:42:user"
        ~channel:"telegram" ~channel_id:"42" ();
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
           ());
      let sent_rev = List.rev !sent in
      Alcotest.(check bool)
        "at least two messages sent" true
        (List.length sent_rev >= 2);
      let first = List.hd sent_rev in
      Alcotest.(check bool)
        "first message is labeled injection" true
        (String.starts_with ~prefix:"[automatic restart-resume]" first);
      Alcotest.(check string)
        "injection is user-facing notice" Daemon.resume_user_notice first;
      let second = List.nth sent_rev 1 in
      Alcotest.(check bool)
        "second message reflects meaningful continuation" true
        (String.length (String.trim second) > 0 && second <> "ok");
      let history = Memory.load_history ~db ~session_key:"telegram:42:user" in
      Alcotest.(check bool)
        "history includes injected resume prompt as a user message" true
        (List.exists
           (fun (m : Provider.message) ->
             m.role = "user" && m.content = Daemon.resume_turn_prompt)
           history);
      Alcotest.(check bool)
        "history retains prior user message" true
        (List.exists
           (fun (m : Provider.message) ->
             m.role = "user" && m.content = "please continue after restart")
           history);
      Alcotest.(check bool)
        "assistant reply persisted" true
        (List.exists
           (fun (m : Provider.message) -> m.role = "assistant")
           history))

let test_rich_send_fn_direct_dispatch_fallback () =
  (* Simulate the rich_send_fn fallback: when no notifier is registered,
     dispatch_resumed_message is called via parse_channel_from_key *)
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
                accounts = [ ("default", telegram_account) ];
                text_coalesce_ms = 150;
                default_model = None;
              };
        };
    }
  in
  let session_key = "telegram:42:42" in
  let content = Rich_message.Text "hello from fallback" in
  (* Replicate the fallback logic from rich_send_fn *)
  let result =
    Lwt_main.run
      (match Restart_notify.parse_channel_from_key session_key with
      | Some (channel, channel_id) -> (
          let text = Rich_message.to_fallback_text content in
          let open Lwt.Syntax in
          let* result =
            Daemon.dispatch_resumed_message ~senders ~config ~channel
              ~channel_id ~text ()
          in
          match result with
          | Ok () ->
              Lwt.return
                (Ok Rich_message.{ message_id = "0"; callback_ids = [] })
          | Error err -> Lwt.return (Error err))
      | None -> Lwt.return (Error "cannot parse channel from key"))
  in
  (match result with
  | Ok sr ->
      Alcotest.(check string) "message_id" "0" sr.Rich_message.message_id;
      Alcotest.(check (list string))
        "callback_ids" [] sr.Rich_message.callback_ids
  | Error err -> Alcotest.fail ("unexpected error: " ^ err));
  match !called with
  | Some (bot_token, chat_id, text) ->
      Alcotest.(check string) "bot_token" "tg-token" bot_token;
      Alcotest.(check string) "chat_id" "42:42" chat_id;
      Alcotest.(check string) "text" "hello from fallback" text
  | None -> Alcotest.fail "telegram sender was not called"

let test_rich_send_fn_fallback_unparseable_key () =
  let session_key = "unparseable" in
  match Restart_notify.parse_channel_from_key session_key with
  | Some _ -> Alcotest.fail "should not parse unparseable key"
  | None ->
      (* Confirms the fallback would raise "cannot parse channel from key" *)
      Alcotest.(check pass) "unparseable key returns None" () ()

let test_replay_durable_inbound_drains_and_deletes () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let session_manager = Session.create ~config ~db () in
  let key = "telegram:1:user" in
  ignore
    (Memory.queue_enqueue ~db ~session_key:key ~source:"cli"
       ~payload_json:
         (Yojson.Safe.to_string
            (`Assoc [ ("message", `String "hello"); ("bang", `Bool false) ])));
  Alcotest.(check int)
    "1 pending before replay" 1
    (Memory.queue_count ~db ~session_key:key);
  let replayed = ref [] in
  let replay_turn _mgr ~key ~message ?cwd:_ () =
    replayed := (key, message) :: !replayed;
    Lwt.return "ok"
  in
  let summary =
    Lwt_main.run
      (Daemon.replay_durable_inbound_queue ~replay_turn ~session_manager ~config
         ())
  in
  Alcotest.(check int)
    "0 pending after replay" 0
    (Memory.queue_count ~db ~session_key:key);
  Alcotest.(check int) "summary session count" 1 summary.session_count;
  Alcotest.(check int) "summary total rows" 1 summary.total_rows;
  Alcotest.(check int) "summary replayed count" 1 summary.replayed_count;
  Alcotest.(check int) "summary failed count" 0 summary.failed_count;
  Alcotest.(check int) "1 message replayed" 1 (List.length !replayed);
  let rkey, rmsg = List.hd !replayed in
  Alcotest.(check string) "correct key" key rkey;
  Alcotest.(check string) "correct message" "hello" rmsg

let test_replay_durable_inbound_summary_counts () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let session_manager = Session.create ~config ~db () in
  ignore
    (Memory.queue_enqueue ~db ~session_key:"telegram:10:user" ~source:"cli"
       ~payload_json:
         (Yojson.Safe.to_string
            (`Assoc [ ("message", `String "ok"); ("bang", `Bool false) ])));
  ignore
    (Memory.queue_enqueue ~db ~session_key:"telegram:10:user" ~source:"cli"
       ~payload_json:
         (Yojson.Safe.to_string
            (`Assoc [ ("message", `String ""); ("bang", `Bool false) ])));
  ignore
    (Memory.queue_enqueue ~db ~session_key:"telegram:11:user" ~source:"cli"
       ~payload_json:
         (Yojson.Safe.to_string
            (`Assoc [ ("message", `String "boom"); ("bang", `Bool false) ])));
  let summary =
    Lwt_main.run
      (Daemon.replay_durable_inbound_queue
         ~replay_turn:(fun _mgr ~key:_ ~message ?cwd:_ () ->
           if message = "boom" then Lwt.fail_with "boom" else Lwt.return "ok")
         ~session_manager ~config ())
  in
  Alcotest.(check int) "summary session count" 2 summary.session_count;
  Alcotest.(check int) "summary total rows" 3 summary.total_rows;
  Alcotest.(check int) "summary replayed count" 1 summary.replayed_count;
  Alcotest.(check int) "summary failed count" 2 summary.failed_count;
  Alcotest.(check int)
    "summary reclaimed stale count" 0 summary.reclaimed_stale_count;
  Alcotest.(check int)
    "summary reclaimed failed count" 0 summary.reclaimed_failed_count

let test_replay_durable_inbound_fifo_ordering () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let session_manager = Session.create ~config ~db () in
  let key = "telegram:2:user" in
  let enq msg =
    ignore
      (Memory.queue_enqueue ~db ~session_key:key ~source:"cli"
         ~payload_json:
           (Yojson.Safe.to_string
              (`Assoc [ ("message", `String msg); ("bang", `Bool false) ])))
  in
  enq "first";
  enq "second";
  enq "third";
  let replayed = ref [] in
  let replay_turn _mgr ~key:_ ~message ?cwd:_ () =
    replayed := message :: !replayed;
    Lwt.return "ok"
  in
  ignore
    (Lwt_main.run
       (Daemon.replay_durable_inbound_queue ~replay_turn ~session_manager
          ~config ()));
  let ordered = List.rev !replayed in
  Alcotest.(check (list string))
    "FIFO order"
    [ "first"; "second"; "third" ]
    ordered

let test_replay_records_failure_on_error () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let session_manager = Session.create ~config ~db () in
  let key = "telegram:3:user" in
  ignore
    (Memory.queue_enqueue ~db ~session_key:key ~source:"cli"
       ~payload_json:
         (Yojson.Safe.to_string
            (`Assoc [ ("message", `String "fail me"); ("bang", `Bool false) ])));
  let replay_turn _mgr ~key:_ ~message:_ ?cwd:_ () =
    Lwt.fail_with "test error"
  in
  ignore
    (Lwt_main.run
       (Daemon.replay_durable_inbound_queue ~replay_turn ~session_manager
          ~config ()));
  let rows = Memory.queue_list ~db ~session_key:key in
  Alcotest.(check int) "row still present" 1 (List.length rows);
  let row = List.hd rows in
  Alcotest.(check int) "attempt_count incremented" 1 row.attempt_count;
  Alcotest.(check (option string))
    "last_error set" (Some "Failure(\"test error\")") row.last_error

let test_replay_skips_empty_message () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let session_manager = Session.create ~config ~db () in
  let key = "telegram:4:user" in
  ignore
    (Memory.queue_enqueue ~db ~session_key:key ~source:"cli"
       ~payload_json:
         (Yojson.Safe.to_string
            (`Assoc [ ("message", `String ""); ("bang", `Bool false) ])));
  let replayed = ref 0 in
  let replay_turn _mgr ~key:_ ~message:_ ?cwd:_ () =
    incr replayed;
    Lwt.return "ok"
  in
  ignore
    (Lwt_main.run
       (Daemon.replay_durable_inbound_queue ~replay_turn ~session_manager
          ~config ()));
  Alcotest.(check int) "turn not called for empty" 0 !replayed;
  let rows = Memory.queue_list ~db ~session_key:key in
  Alcotest.(check int) "row still present (failed)" 1 (List.length rows);
  Alcotest.(check (option string))
    "error is empty message" (Some "empty message") (List.hd rows).last_error

let test_replay_preserves_bang_prefix () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let session_manager = Session.create ~config ~db () in
  let key = "telegram:5:user" in
  ignore
    (Memory.queue_enqueue ~db ~session_key:key ~source:"cli"
       ~payload_json:
         (Yojson.Safe.to_string
            (`Assoc [ ("message", `String "urgent"); ("bang", `Bool true) ])));
  let replayed = ref [] in
  let replay_turn _mgr ~key:_ ~message ?cwd:_ () =
    replayed := message :: !replayed;
    Lwt.return "ok"
  in
  ignore
    (Lwt_main.run
       (Daemon.replay_durable_inbound_queue ~replay_turn ~session_manager
          ~config ()));
  Alcotest.(check (list string)) "bang prefix added" [ "!urgent" ] !replayed

let test_session_reset_clears_pending_queue () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let session_manager = Session.create ~config ~db () in
  let key = "telegram:6:user" in
  ignore
    (Memory.queue_enqueue ~db ~session_key:key ~source:"cli"
       ~payload_json:
         (Yojson.Safe.to_string
            (`Assoc [ ("message", `String "queued"); ("bang", `Bool false) ])));
  Alcotest.(check int)
    "1 pending before reset" 1
    (Memory.queue_count ~db ~session_key:key);
  ignore (Lwt_main.run (Session.reset session_manager ~key));
  Alcotest.(check int)
    "0 pending after reset" 0
    (Memory.queue_count ~db ~session_key:key)

let test_refresh_runtime_bound_tools_replaces_shell_exec_on_reload () =
  let registry = Tool_registry.create () in
  let config1 = Runtime_config.default in
  let sandbox1 =
    Sandbox.create ~backend:Sandbox.None
      ~workspace:(Runtime_config.effective_workspace config1)
      ~extra_allowed_paths:config1.security.extra_allowed_paths
      ~workspace_only:config1.security.workspace_only ()
  in
  let session_manager = Session.create ~config:config1 ~sandbox:sandbox1 () in
  Daemon.refresh_runtime_bound_tools ~config:config1 ~session_manager
    ~sandbox:sandbox1 registry;
  let shell1 = Option.get (Tool_registry.find registry "shell_exec") in
  let config2 =
    {
      config1 with
      workspace =
        Filename.concat (Runtime_config.effective_workspace config1) "alt";
      security = { config1.security with workspace_only = true };
    }
  in
  let sandbox2 =
    Sandbox.create ~backend:Sandbox.None
      ~workspace:(Runtime_config.effective_workspace config2)
      ~extra_allowed_paths:config2.security.extra_allowed_paths
      ~workspace_only:config2.security.workspace_only ()
  in
  Session.set_sandbox session_manager sandbox2;
  Session.update_config ~source:"test_reload" session_manager config2;
  Daemon.refresh_runtime_bound_tools ~config:config2 ~session_manager
    ~sandbox:sandbox2 registry;
  let shell2 = Option.get (Tool_registry.find registry "shell_exec") in
  Alcotest.(check bool) "shell_exec replaced on reload" true (shell1 != shell2);
  Alcotest.(check bool)
    "shell_exec description reflects reloaded workspace policy" true
    (Test_helpers.string_contains shell2.Tool.description "Workspace policy")

let test_task_tree_tool_with_current_workspace_autostarts_without_cwd () =
  with_temp_git_repo (fun repo_path ->
      let db = Memory.init ~db_path:":memory:" () in
      Task_tree.init_schema db;
      Background_task.init_schema db;
      let current_config =
        ref { Runtime_config.default with workspace = repo_path }
      in
      let notify _session_key =
        Some (Format_adapter.Plain, fun _text -> Lwt.return_unit)
      in
      let tool =
        Daemon.task_tree_tool_with_current_workspace ~current_config ~db ~notify
          ()
      in
      let context =
        {
          Tool.session_key = Some "s1";
          send_progress = None;
          interrupt_check = None;
          inject_system_messages = None;
          effective_cwd = None;
          request_cwd_change = None;
        }
      in
      let result =
        Lwt_main.run
          (tool.Tool.invoke ~context
             (`Assoc
                [
                  ( "operations",
                    `List
                      [
                        `Assoc
                          [
                            ("op", `String "add");
                            ("id", `String "impl");
                            ("title", `String "Implement");
                            ("agent_prompt", `String "Build it");
                            ("autostart", `Bool true);
                          ];
                      ] );
                ]))
      in
      Alcotest.(check bool)
        "autostart reported queued task" true
        (Test_helpers.string_contains result "Queued task agent");
      match Background_task.list_tasks ~db with
      | [ task ] ->
          Alcotest.(check string)
            "queued task uses configured workspace" repo_path task.repo_path
      | tasks ->
          Alcotest.failf "expected one background task, got %d"
            (List.length tasks))

let test_refresh_task_tree_tools_replaces_start_agent_workspace_on_reload () =
  with_temp_git_repo (fun repo1 ->
      with_temp_git_repo (fun repo2 ->
          let registry = Tool_registry.create () in
          let db = Memory.init ~db_path:":memory:" () in
          Task_tree.init_schema db;
          Background_task.init_schema db;
          let current_config =
            ref { Runtime_config.default with workspace = repo1 }
          in
          let session_manager = Session.create ~config:!current_config ~db () in
          Daemon.refresh_task_tree_tools_with_current_workspace ~current_config
            ~db registry;
          ignore
            (Task_tree.insert_task ~db ~session_key:"default" ~id:"impl"
               ~parent_id:None ~title:"Implement" ~status:Task_tree.Pending
               ~note:None ~depends_on:[] ~agent_model:None ~agent_type:None
               ~agent_prompt:(Some "Build it") ~agent_details:None
               ~autostart:false);
          current_config := { !current_config with workspace = repo2 };
          Session.update_config ~source:"test_reload" session_manager
            !current_config;
          Daemon.refresh_task_tree_tools_with_current_workspace ~current_config
            ~db registry;
          let tool =
            Option.get (Tool_registry.find registry "task_start_agent")
          in
          let result =
            Lwt_main.run
              (tool.Tool.invoke
                 (`Assoc
                    [ ("id", `String "impl"); ("use_worktree", `Bool false) ]))
          in
          Alcotest.(check bool)
            "start_agent reported queued task" true
            (Test_helpers.string_contains result "Queued task agent");
          match Background_task.list_tasks ~db with
          | [ task ] ->
              Alcotest.(check string)
                "queued task uses reloaded workspace" repo2 task.repo_path
          | tasks ->
              Alcotest.failf "expected one background task, got %d"
                (List.length tasks)))

let test_current_max_concurrent_native_agents_reads_reloaded_config () =
  let config cap =
    {
      Runtime_config.default with
      agent_defaults =
        {
          Runtime_config.default.agent_defaults with
          max_concurrent_native_agents = cap;
        };
    }
  in
  let current_config = ref (config (Some 1)) in
  Alcotest.(check (option int))
    "initial cap" (Some 1)
    (Daemon.current_max_concurrent_native_agents current_config);
  current_config := config (Some 3);
  Alcotest.(check (option int))
    "reloaded cap" (Some 3)
    (Daemon.current_max_concurrent_native_agents current_config)

let insert_test_task db =
  let dir = Filename.temp_file "clawq-notify-test" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let code =
    Sys.command
      (Printf.sprintf
         "git -C %s init -q && git -C %s config user.name Test && git -C %s \
          config user.email t@t && git -C %s commit --allow-empty -m init -q"
         (Filename.quote dir) (Filename.quote dir) (Filename.quote dir)
         (Filename.quote dir))
  in
  if code <> 0 then Alcotest.failf "git setup failed (exit %d)" code;
  let id =
    match
      Background_task.enqueue ~db ~runner:Background_task.Codex ~repo_path:dir
        ~prompt:"test" ~session_key:"telegram:42:user" ~channel:"telegram"
        ~channel_id:"42" ()
    with
    | Ok id -> id
    | Error e -> Alcotest.failf "enqueue: %s" e
  in
  ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir)));
  id

let test_notify_records_delivered_on_success () =
  let db = Memory.init ~db_path:":memory:" () in
  Background_task.init_schema db;
  Scheduler.init_schema db;
  let task_id = insert_test_task db in
  let config = Runtime_config.default in
  let session_manager = Session.create ~config ~db () in
  Session.register_channel_notifier session_manager ~key:"telegram:42:user"
    (fun _text -> Lwt.return_unit);
  let task =
    { (make_test_task ~id:task_id ()) with Background_task.status = Succeeded }
  in
  Lwt_main.run
    (Daemon.notify_background_task_finished ~continuation_delay:100.0
       ~session_manager ~config ~db task);
  let t =
    match Background_task.get_task ~db ~id:task_id with
    | Some t -> t
    | None -> Alcotest.fail "task not found"
  in
  Alcotest.(check (option string))
    "notification status" (Some "delivered") t.notification_status;
  Alcotest.(check (option string)) "no error" None t.notification_error;
  Alcotest.(check int) "attempts is 1" 1 t.notification_attempts

let test_notify_records_failed_on_sender_error () =
  let db = Memory.init ~db_path:":memory:" () in
  Background_task.init_schema db;
  Scheduler.init_schema db;
  let task_id = insert_test_task db in
  let config = Runtime_config.default in
  let session_manager = Session.create ~config ~db () in
  Session.register_channel_notifier session_manager ~key:"telegram:42:user"
    (fun _text -> Lwt.fail_with "send failed");
  let task =
    { (make_test_task ~id:task_id ()) with Background_task.status = Succeeded }
  in
  Lwt_main.run
    (Daemon.notify_background_task_finished ~continuation_delay:100.0
       ~session_manager ~config ~db task);
  let t =
    match Background_task.get_task ~db ~id:task_id with
    | Some t -> t
    | None -> Alcotest.fail "task not found"
  in
  Alcotest.(check (option string))
    "notification status" (Some "failed") t.notification_status;
  Alcotest.(check bool)
    "error contains reason" true
    (match t.notification_error with
    | Some e -> Test_helpers.string_contains e "send failed"
    | None -> false)

let test_notify_records_skipped_when_no_channel () =
  let db = Memory.init ~db_path:":memory:" () in
  Background_task.init_schema db;
  Scheduler.init_schema db;
  let task_id = insert_test_task db in
  let config = Runtime_config.default in
  let session_manager = Session.create ~config ~db () in
  let task =
    {
      (make_test_task ~id:task_id ~session_key:None ~channel:None
         ~channel_id:None ())
      with
      Background_task.status = Succeeded;
    }
  in
  Lwt_main.run
    (Daemon.notify_background_task_finished ~session_manager ~config ~db task);
  let t =
    match Background_task.get_task ~db ~id:task_id with
    | Some t -> t
    | None -> Alcotest.fail "task not found"
  in
  Alcotest.(check (option string))
    "notification status" (Some "skipped") t.notification_status

let test_notify_discord_dispatches_channel_notification () =
  let db = Memory.init ~db_path:":memory:" () in
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
                default_model = None;
              };
        };
    }
  in
  let session_manager = Session.create ~config ~db () in
  let dispatched = ref [] in
  let senders =
    {
      Daemon.default_resume_senders with
      send_discord =
        (fun ~bot_token:_ ~channel_id:_ ~text ->
          dispatched := text :: !dispatched;
          Lwt.return_unit);
    }
  in
  Session.set_special_command_handler session_manager
    (fun ~key ~message:_ ~send_progress:_ ~interrupt_check:_ ->
      if key = "discord:42:user" then Lwt.return_some "ok" else Lwt.return_none);
  let task =
    make_test_task ~id:9 ~session_key:(Some "discord:42:user")
      ~channel:(Some "discord") ~channel_id:(Some "42") ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       Daemon.notify_background_task_finished ~continuation_delay:100.0 ~senders
         ~session_manager ~config task
     in
     let* () = Lwt.pause () in
     Session.cancel_autonomous_continuation session_manager
       ~key:"discord:42:user");
  match List.rev !dispatched with
  | t :: _ ->
      Alcotest.(check bool)
        "discord dispatch has channel notification format" true
        (Test_helpers.string_contains t "Background task #9 finished: SUCCEEDED")
  | [] -> Alcotest.fail "expected at least one dispatch"

(* B673: restart-resume history sanitization. Stuck/watchdog/circuit-breaker
   noise messages from a prior session epoch should be dropped before the
   resume turn so they don't bias the LLM. *)
let test_b673_sanitize_history_drops_noise_messages () =
  let stuck =
    Provider.make_message ~role:"user"
      ~content:
        "[Observer] Stuck pattern detected: SameErrorString repeated 3 times"
  in
  let watchdog =
    Provider.make_message ~role:"assistant"
      ~content:
        "[Watchdog] Pausing this session after 2 consecutive stuck detections"
  in
  let circuit_breaker =
    Provider.make_message ~role:"assistant"
      ~content:
        "Aborted turn after 3 consecutive identical parameter-validation \
         failures on tool 'web_search' (missing: query)."
  in
  let normal_user = Provider.make_message ~role:"user" ~content:"hello" in
  let normal_assistant =
    Provider.make_message ~role:"assistant" ~content:"hi there"
  in
  let history =
    [ normal_user; stuck; normal_assistant; watchdog; circuit_breaker ]
  in
  let cleaned, dropped = Daemon.sanitize_history_for_resume history in
  Alcotest.(check int) "dropped 3 noise messages" 3 dropped;
  let contents = List.map (fun (m : Provider.message) -> m.content) cleaned in
  Alcotest.(check (list string))
    "only normal messages remain" [ "hello"; "hi there" ] contents

(* B673: sanitizer is conservative — does NOT drop normal user/assistant
   messages even if they incidentally start with "[" or otherwise look
   noisy. *)
let test_b673_sanitize_history_preserves_normal_messages () =
  let bracketed_normal =
    Provider.make_message ~role:"user"
      ~content:"[Question] Why is the sky blue?"
  in
  let asst_normal =
    Provider.make_message ~role:"assistant" ~content:"It's Rayleigh scattering."
  in
  let history = [ bracketed_normal; asst_normal ] in
  let cleaned, dropped = Daemon.sanitize_history_for_resume history in
  Alcotest.(check int) "no noise dropped" 0 dropped;
  Alcotest.(check int) "both messages preserved" 2 (List.length cleaned)

let suite =
  [
    Alcotest.test_case "B673: sanitize history drops noise messages" `Quick
      test_b673_sanitize_history_drops_noise_messages;
    Alcotest.test_case "B673: sanitize history preserves normal messages" `Quick
      test_b673_sanitize_history_preserves_normal_messages;
    Alcotest.test_case "boot stage message helpers" `Quick
      test_boot_stage_message_helpers;
    Alcotest.test_case "with boot stage logging success" `Quick
      test_with_boot_stage_logging_success;
    Alcotest.test_case "with boot stage logging error" `Quick
      test_with_boot_stage_logging_error;
    Alcotest.test_case "boot startup stage sequence without full daemon" `Quick
      test_boot_startup_stage_sequence_without_full_daemon;
    Alcotest.test_case "boot startup stage sequence continues after mcp failure"
      `Quick test_boot_startup_stage_sequence_continues_after_mcp_failure;
    Alcotest.test_case "setup MCP clients loads configs and registers tools"
      `Quick test_setup_mcp_clients_loads_configs_and_registers_tools;
    Alcotest.test_case "dispatch resumed message routes telegram" `Quick
      test_dispatch_resumed_message_routes_telegram;
    Alcotest.test_case "dispatch resumed message routes discord" `Quick
      test_dispatch_resumed_message_routes_discord;
    Alcotest.test_case "dispatch resumed message routes slack" `Quick
      test_dispatch_resumed_message_routes_slack;
    Alcotest.test_case "dispatch resumed message routes github (no-op)" `Quick
      test_dispatch_resumed_message_routes_github;
    Alcotest.test_case
      "B666: dispatch resumed message teams empty activity_id surfaces Error"
      `Quick test_dispatch_resumed_message_teams_empty_activity_is_error;
    Alcotest.test_case
      "B666: dispatch resumed message teams non-empty activity_id is Ok" `Quick
      test_dispatch_resumed_message_teams_non_empty_activity_is_ok;
    Alcotest.test_case
      "resume pending sessions marks missing channel info as sent" `Quick
      test_resume_pending_agent_sessions_marks_missing_channel_info;
    Alcotest.test_case "resume pending sessions summary counts" `Quick
      test_resume_pending_agent_sessions_summary_counts;
    Alcotest.test_case "default resume turn uses explicit automatic prompt"
      `Quick test_default_resume_turn_uses_explicit_resume_prompt;
    Alcotest.test_case "resume turn payload is openai-compat valid (z.ai 1214)"
      `Quick test_resume_turn_payload_is_openai_compat_valid;
    Alcotest.test_case "resume agent session sends debug summary" `Quick
      test_resume_agent_session_sends_debug_summary;
    Alcotest.test_case "resume agent session persists response and marks sent"
      `Quick test_resume_agent_session_persists_response_and_marks_sent;
    Alcotest.test_case
      "local background template persists history and explicit model" `Quick
      test_local_background_turn_template_persists_history_and_model;
    Alcotest.test_case "local background missing template fails" `Quick
      test_local_background_turn_missing_template_fails;
    Alcotest.test_case "local background template model precedence" `Quick
      test_local_background_turn_template_model_precedence;
    Alcotest.test_case "local background no-template subagent default model"
      `Quick test_local_background_turn_no_template_subagent_default_model;
    Alcotest.test_case "background completion dispatches and injects wake-up"
      `Quick test_notify_background_task_finished_dispatches_and_injects_wakeup;
    Alcotest.test_case "background completion queues wake-up when session busy"
      `Quick
      test_notify_background_task_finished_queues_wakeup_when_session_busy;
    Alcotest.test_case
      "background dirty-worktree completion dispatches finalize hint" `Quick
      test_notify_background_task_finished_dirty_worktree_dispatches_finalize_hint;
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
    Alcotest.test_case "heartbeat work reply sends initial session message"
      `Quick test_handle_heartbeat_response_sends_initial_reply_to_session;
    Alcotest.test_case "wait for drain returns when in-flight reaches zero"
      `Quick test_wait_for_drain_returns_when_in_flight_reaches_zero;
    Alcotest.test_case "wait for drain reports timeout" `Quick
      test_wait_for_drain_reports_timeout;
    Alcotest.test_case "start draining interrupts all sessions" `Quick
      test_start_draining_interrupts_all_sessions;
    Alcotest.test_case "send drain warnings sends scheduled messages" `Quick
      test_send_drain_warnings_does_not_notify_channel;
    Alcotest.test_case "restart signal duplicate delta detects recent signal"
      `Quick test_restart_signal_duplicate_delta_recent;
    Alcotest.test_case "restart signal duplicate delta ignores older signal"
      `Quick test_restart_signal_duplicate_delta_outside_window;
    Alcotest.test_case "restart signal duplicate delta ignores future timestamp"
      `Quick test_restart_signal_duplicate_delta_negative_delta;
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
    Alcotest.test_case "inject bg task completion registers notifier" `Quick
      test_inject_bg_task_completion_registers_notifier;
    Alcotest.test_case "background task wakeup arms autonomous continuation"
      `Quick test_background_task_wakeup_arms_autonomous_continuation;
    Alcotest.test_case "background task wakeup STAY_IDLE disarms" `Quick
      test_background_task_wakeup_stay_idle_disarms;
    Alcotest.test_case "resume agent session sends visible injection prompt"
      `Quick test_resume_agent_session_sends_visible_injection_prompt;
    Alcotest.test_case
      "rich_send_fn direct dispatch fallback for telegram session" `Quick
      test_rich_send_fn_direct_dispatch_fallback;
    Alcotest.test_case "rich_send_fn fallback fails for unparseable session key"
      `Quick test_rich_send_fn_fallback_unparseable_key;
    Alcotest.test_case "replay durable inbound drains and deletes" `Quick
      test_replay_durable_inbound_drains_and_deletes;
    Alcotest.test_case "replay durable inbound summary counts" `Quick
      test_replay_durable_inbound_summary_counts;
    Alcotest.test_case "replay durable inbound FIFO ordering" `Quick
      test_replay_durable_inbound_fifo_ordering;
    Alcotest.test_case "replay records failure on error" `Quick
      test_replay_records_failure_on_error;
    Alcotest.test_case "replay skips empty message" `Quick
      test_replay_skips_empty_message;
    Alcotest.test_case "replay preserves bang prefix" `Quick
      test_replay_preserves_bang_prefix;
    Alcotest.test_case "session reset clears pending queue" `Quick
      test_session_reset_clears_pending_queue;
    Alcotest.test_case "refresh runtime-bound tools replaces shell_exec" `Quick
      test_refresh_runtime_bound_tools_replaces_shell_exec_on_reload;
    Alcotest.test_case
      "task tree notification tool autostarts with current workspace" `Quick
      test_task_tree_tool_with_current_workspace_autostarts_without_cwd;
    Alcotest.test_case "task start agent refreshes workspace on reload" `Quick
      test_refresh_task_tree_tools_replaces_start_agent_workspace_on_reload;
    Alcotest.test_case "native agent cap reads reloaded config" `Quick
      test_current_max_concurrent_native_agents_reads_reloaded_config;
    Alcotest.test_case "notify records delivered on success" `Quick
      test_notify_records_delivered_on_success;
    Alcotest.test_case "notify records failed on sender error" `Quick
      test_notify_records_failed_on_sender_error;
    Alcotest.test_case "notify records skipped when no channel" `Quick
      test_notify_records_skipped_when_no_channel;
    Alcotest.test_case "notify discord dispatches channel notification" `Quick
      test_notify_discord_dispatches_channel_notification;
  ]
