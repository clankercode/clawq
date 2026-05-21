let string_contains haystack needle =
  let hay_len = String.length haystack and needle_len = String.length needle in
  let rec loop i =
    if i + needle_len > hay_len then false
    else if String.sub haystack i needle_len = needle then true
    else loop (i + 1)
  in
  needle_len = 0 || loop 0

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let make_fake_provider_config base_url : Runtime_config.provider_config =
  {
    Runtime_config.default_provider_config with
    api_key = "test-key";
    base_url = Some base_url;
    default_model = Some "fake-model";
  }

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

let with_fake_chat_provider ?response_for_user f =
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
    let response_text =
      match response_for_user with
      | Some reply -> reply latest
      | None -> "reply:" ^ latest
    in
    match (Cohttp.Request.meth req, Uri.path (Cohttp.Request.uri req)) with
    | `POST, "/chat/completions" ->
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
                                 ("content", `String response_text);
                               ] );
                           ("finish_reason", `String "stop");
                         ];
                     ] );
                 ( "usage",
                   `Assoc
                     [
                       ("prompt_tokens", `Int 1); ("completion_tokens", `Int 1);
                     ] );
               ])
        in
        Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:response_body ()
    | _ -> Cohttp_lwt_unix.Server.respond_string ~status:`Not_found ~body:"" ()
  in
  let stop, stopper = Lwt.wait () in
  let server =
    Cohttp_lwt_unix.Server.create
      ~mode:(`TCP (`Port port))
      (Cohttp_lwt_unix.Server.make ~callback ())
  in
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

let sample_stats session_key =
  {
    Session_observer.session_key;
    turn_count = 4;
    total_tool_calls = 2;
    error_count = 1;
    session_age_s = 12.0;
  }

let test_check_stuck_writes_durable_log () =
  Test_helpers.with_temp_home (fun _home ->
      with_fake_chat_provider
        ~response_for_user:(fun _ -> "STUCK:repeating failed tool call")
        (fun config ->
          let history =
            [
              Provider.make_message ~role:"assistant" ~content:"Still trying";
              Provider.make_message ~role:"user" ~content:"Please fix it";
            ]
          in
          let verdict =
            Lwt_main.run
              (Session_observer.check_stuck ~config ~history
                 ~stats:(sample_stats "session-log-1")
                 ())
          in
          (match verdict with
          | Session_observer.Stuck { reason; confidence = `High } ->
              Alcotest.(check string)
                "reason" "repeating failed tool call" reason
          | _ -> Alcotest.fail "expected high-confidence stuck verdict");
          let log_path = Dot_dir.sub "observer.log" in
          Alcotest.(check bool)
            "observer log created" true (Sys.file_exists log_path);
          let log_text = read_file log_path in
          Alcotest.(check bool)
            "writes stuck event" true
            (string_contains log_text "\"event\":\"stuck_check\"");
          Alcotest.(check bool)
            "writes session key" true
            (string_contains log_text "\"session_key\":\"session-log-1\"");
          Alcotest.(check bool)
            "writes reason" true
            (string_contains log_text
               "\"reason\":\"repeating failed tool call\"")))

let test_check_stuck_logs_failures () =
  Test_helpers.with_temp_home (fun _home ->
      let config =
        {
          Runtime_config.default with
          default_provider = Some "fake";
          providers =
            [ ("fake", make_fake_provider_config "http://127.0.0.1:9") ];
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
      let history =
        [ Provider.make_message ~role:"user" ~content:"Check me" ]
      in
      let verdict =
        Lwt_main.run
          (Session_observer.check_stuck ~config ~history
             ~stats:(sample_stats "session-log-failure")
             ())
      in
      (match verdict with
      | Session_observer.Error _ -> ()
      | _ -> Alcotest.fail "expected observer error verdict");
      let log_text = read_file (Dot_dir.sub "observer.log") in
      Alcotest.(check bool)
        "writes error event" true
        (string_contains log_text "\"event\":\"stuck_check_error\"");
      Alcotest.(check bool)
        "writes failed session key" true
        (string_contains log_text "\"session_key\":\"session-log-failure\""))

let test_check_thinking_excerpt_logs_looping_verdict () =
  Test_helpers.with_temp_home (fun _home ->
      with_fake_chat_provider
        ~response_for_user:(fun _ -> "LOOPING:repeating the same plan")
        (fun config ->
          let verdict =
            Lwt_main.run
              (Session_observer.check_thinking_excerpt ~config
                 ~excerpt:"Need to do X. Need to do X. Need to do X." ())
          in
          (match verdict with
          | `Looping reason ->
              Alcotest.(check string)
                "loop reason" "repeating the same plan" reason
          | `Sane -> Alcotest.fail "expected looping verdict");
          let log_text = read_file (Dot_dir.sub "observer.log") in
          Alcotest.(check bool)
            "writes thinking event" true
            (string_contains log_text "\"event\":\"thinking_check\"");
          Alcotest.(check bool)
            "writes looping verdict" true
            (string_contains log_text "\"verdict\":\"looping\"");
          Alcotest.(check bool)
            "writes loop reason" true
            (string_contains log_text "\"reason\":\"repeating the same plan\"")))

let test_observer_log_path () =
  Test_helpers.with_temp_home (fun home ->
      let path = Session_observer.observer_log_path () in
      let expected =
        Filename.concat (Filename.concat home ".clawq") "observer.log"
      in
      Alcotest.(check string) "log path" expected path)

let test_append_observer_log_writes_json_line () =
  Test_helpers.with_temp_home (fun _home ->
      Session_observer.append_observer_log
        [ ("event", `String "test_event"); ("value", `Int 42) ];
      let log_path = Session_observer.observer_log_path () in
      Alcotest.(check bool) "log file created" true (Sys.file_exists log_path);
      let content = read_file log_path in
      let json = Yojson.Safe.from_string (String.trim content) in
      let open Yojson.Safe.Util in
      let ts = json |> member "ts" |> to_string in
      Alcotest.(check bool) "has ts field" true (String.length ts > 0);
      Alcotest.(check string)
        "event field" "test_event"
        (json |> member "event" |> to_string);
      Alcotest.(check int) "value field" 42 (json |> member "value" |> to_int))

let test_append_observer_log_appends_multiple () =
  Test_helpers.with_temp_home (fun _home ->
      Session_observer.append_observer_log [ ("n", `Int 1) ];
      Session_observer.append_observer_log [ ("n", `Int 2) ];
      let content = read_file (Session_observer.observer_log_path ()) in
      let lines =
        String.split_on_char '\n' content
        |> List.filter (fun s -> String.trim s <> "")
      in
      Alcotest.(check int) "two lines" 2 (List.length lines);
      let open Yojson.Safe.Util in
      let j1 = Yojson.Safe.from_string (List.nth lines 0) in
      let j2 = Yojson.Safe.from_string (List.nth lines 1) in
      Alcotest.(check int) "first n" 1 (j1 |> member "n" |> to_int);
      Alcotest.(check int) "second n" 2 (j2 |> member "n" |> to_int))

let test_log_stuck_check_ok () =
  Test_helpers.with_temp_home (fun _home ->
      Session_observer.log_stuck_check ~session_key:"s1" ~round:1
        ~message_count:5 ~raw_response:"OK" ~parsed:`Ok;
      let content = read_file (Session_observer.observer_log_path ()) in
      let json = Yojson.Safe.from_string (String.trim content) in
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "event" "stuck_check"
        (json |> member "event" |> to_string);
      Alcotest.(check string)
        "session_key" "s1"
        (json |> member "session_key" |> to_string);
      Alcotest.(check int) "round" 1 (json |> member "round" |> to_int);
      Alcotest.(check int)
        "message_count" 5
        (json |> member "message_count" |> to_int);
      Alcotest.(check string)
        "verdict" "ok"
        (json |> member "verdict" |> to_string))

let test_log_stuck_check_stuck () =
  Test_helpers.with_temp_home (fun _home ->
      Session_observer.log_stuck_check ~session_key:"s2" ~round:2
        ~message_count:10 ~raw_response:"STUCK:looping on file_write"
        ~parsed:(`Stuck "looping on file_write");
      let content = read_file (Session_observer.observer_log_path ()) in
      let json = Yojson.Safe.from_string (String.trim content) in
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "verdict" "stuck"
        (json |> member "verdict" |> to_string);
      Alcotest.(check string)
        "reason" "looping on file_write"
        (json |> member "reason" |> to_string))

let test_log_stuck_check_error () =
  Test_helpers.with_temp_home (fun _home ->
      Session_observer.log_stuck_check_error ~session_key:"s3" ~message_count:7
        ~error:"connection refused";
      let content = read_file (Session_observer.observer_log_path ()) in
      let json = Yojson.Safe.from_string (String.trim content) in
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "event" "stuck_check_error"
        (json |> member "event" |> to_string);
      Alcotest.(check string)
        "error" "connection refused"
        (json |> member "error" |> to_string))

let test_log_thinking_check_sane () =
  Test_helpers.with_temp_home (fun _home ->
      Session_observer.log_thinking_check ~excerpt:"reasoning text"
        ~raw_response:"SANE" ~parsed:`Sane;
      let content = read_file (Session_observer.observer_log_path ()) in
      let json = Yojson.Safe.from_string (String.trim content) in
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "event" "thinking_check"
        (json |> member "event" |> to_string);
      Alcotest.(check string)
        "verdict" "sane"
        (json |> member "verdict" |> to_string);
      Alcotest.(check int)
        "excerpt_chars" 14
        (json |> member "excerpt_chars" |> to_int))

let test_log_thinking_check_error () =
  Test_helpers.with_temp_home (fun _home ->
      Session_observer.log_thinking_check_error ~excerpt:"some text"
        ~error:"timeout";
      let content = read_file (Session_observer.observer_log_path ()) in
      let json = Yojson.Safe.from_string (String.trim content) in
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "event" "thinking_check_error"
        (json |> member "event" |> to_string);
      Alcotest.(check string)
        "error" "timeout"
        (json |> member "error" |> to_string))

let test_check_stuck_filters_event_messages () =
  Test_helpers.with_temp_home (fun _home ->
      with_fake_chat_provider
        ~response_for_user:(fun _ -> "OK")
        (fun config ->
          let history =
            [
              Provider.make_message ~role:"user" ~content:"Hello";
              Provider.make_message ~role:"event" ~content:"config reloaded";
              Provider.make_message ~role:"assistant" ~content:"Hi there";
              Provider.make_message ~role:"event"
                ~content:"project docs refreshed";
              Provider.make_message ~role:"user" ~content:"Do the task";
            ]
          in
          let verdict =
            Lwt_main.run
              (Session_observer.check_stuck ~config ~history
                 ~stats:(sample_stats "session-event-filter")
                 ())
          in
          match verdict with
          | Session_observer.Ok -> ()
          | Session_observer.Stuck { reason; _ } ->
              Alcotest.fail ("expected ok, got stuck: " ^ reason)
          | Session_observer.Error msg ->
              Alcotest.fail ("expected ok, got error: " ^ msg)))

(* B667: when the last assistant message in history is a complete sentence
   ending with terminal punctuation and has no pending tool_calls, check_stuck
   must short-circuit to Ok without calling the LLM observer. This prevents
   false-positive 'invalid_response_format' verdicts on legitimate short
   responses like "Nothing notable." which were triggering postmortem chains.
   We assert the bypass by setting the fake provider to STUCK:should-not-fire
   — if the LLM is consulted at all, the verdict would be Stuck, not Ok. *)
let test_check_stuck_bypasses_llm_for_complete_response () =
  Test_helpers.with_temp_home (fun _home ->
      with_fake_chat_provider
        ~response_for_user:(fun _ -> "STUCK:should-not-fire")
        (fun config ->
          let history =
            [
              Provider.make_message ~role:"assistant"
                ~content:"Nothing notable.";
              Provider.make_message ~role:"user"
                ~content:"Anything new in the world?";
            ]
          in
          let verdict =
            Lwt_main.run
              (Session_observer.check_stuck ~config ~history
                 ~stats:(sample_stats "session-b667-bypass")
                 ())
          in
          match verdict with
          | Session_observer.Ok -> ()
          | Session_observer.Stuck { reason; _ } ->
              Alcotest.fail
                (Printf.sprintf
                   "B667: expected bypass to return Ok, got Stuck:%s — LLM \
                    observer was incorrectly consulted"
                   reason)
          | Session_observer.Error msg ->
              Alcotest.fail ("expected ok, got error: " ^ msg)))

(* B667 follow-up: terminal punctuation may be wrapped in trailing markdown
   noise (e.g. a final sentence ending ".](url))" — period then markdown
   link closure). Bypass must scan past trailing `)`, `]`, `*`, etc. *)
let test_check_stuck_bypasses_llm_for_markdown_trailing_close () =
  Test_helpers.with_temp_home (fun _home ->
      with_fake_chat_provider
        ~response_for_user:(fun _ -> "STUCK:should-not-fire")
        (fun config ->
          let history =
            [
              Provider.make_message ~role:"assistant"
                ~content:
                  "Two notable items. **Bitcoin reserve bill.** Lawmakers \
                   filed ARMA. ([CryptoSlate](https://example.test/article))";
              Provider.make_message ~role:"user" ~content:"News check";
            ]
          in
          let verdict =
            Lwt_main.run
              (Session_observer.check_stuck ~config ~history
                 ~stats:(sample_stats "session-b667-markdown")
                 ())
          in
          match verdict with
          | Session_observer.Ok -> ()
          | Session_observer.Stuck { reason; _ } ->
              Alcotest.fail
                (Printf.sprintf
                   "B667: expected bypass for markdown-trailing-close \
                    response, got Stuck:%s"
                   reason)
          | Session_observer.Error msg ->
              Alcotest.fail ("expected ok, got error: " ^ msg)))

let test_check_stuck_no_bypass_for_incomplete_response () =
  Test_helpers.with_temp_home (fun _home ->
      with_fake_chat_provider
        ~response_for_user:(fun _ -> "STUCK:looping-on-tool-calls")
        (fun config ->
          let history =
            [
              (* Note: no terminal punctuation — bypass should not fire,
                 LLM observer should be consulted. *)
              Provider.make_message ~role:"assistant" ~content:"working on it";
              Provider.make_message ~role:"user" ~content:"Status";
            ]
          in
          let verdict =
            Lwt_main.run
              (Session_observer.check_stuck ~config ~history
                 ~stats:(sample_stats "session-b667-no-bypass")
                 ())
          in
          match verdict with
          | Session_observer.Stuck { reason; _ } ->
              Alcotest.(check string)
                "LLM observer was consulted" "looping-on-tool-calls" reason
          | Session_observer.Ok ->
              Alcotest.fail
                "B667: bypass fired on incomplete response (no terminal \
                 punctuation) — LLM observer should have run"
          | Session_observer.Error msg ->
              Alcotest.fail ("expected stuck, got error: " ^ msg)))

let test_parse_verdict () =
  Alcotest.(check string)
    "OK" "ok"
    (match Session_observer.parse_verdict "OK" with
    | `Ok -> "ok"
    | _ -> "other");
  Alcotest.(check string)
    "NEED_MORE" "need_more"
    (match Session_observer.parse_verdict "NEED_MORE" with
    | `Need_more -> "need_more"
    | _ -> "other");
  Alcotest.(check string)
    "STUCK:reason" "looping"
    (match Session_observer.parse_verdict "STUCK:looping" with
    | `Stuck r -> r
    | _ -> "other");
  Alcotest.(check string)
    "STUCK with whitespace" "trimmed"
    (match Session_observer.parse_verdict "  STUCK:  trimmed  " with
    | `Stuck r -> r
    | _ -> "other");
  Alcotest.(check string)
    "unknown defaults to ok" "ok"
    (match Session_observer.parse_verdict "MAYBE" with
    | `Ok -> "ok"
    | _ -> "other");
  (* B-postobserve: when the LLM emits a duplicated STUCK: prefix or trails
     after the first reason, parse_verdict should clean it to one line. The
     duplicated "...searchesSTUCK:..." form showed up in daemon.log. *)
  Alcotest.(check string)
    "duplicated STUCK: prefix collapses to first reason"
    "repeating static response without performing web searches"
    (match
       Session_observer.parse_verdict
         "STUCK:repeating static response without performing web \
          searchesSTUCK:repeating static response without performing web \
          searches"
     with
    | `Stuck r -> r
    | _ -> "other");
  Alcotest.(check string)
    "newline-terminated reason keeps only the first line" "looping"
    (match
       Session_observer.parse_verdict
         "STUCK:looping\nadditional trailing prose to ignore"
     with
    | `Stuck r -> r
    | _ -> "other")

let suite =
  [
    Alcotest.test_case "check_stuck writes durable observer log" `Quick
      test_check_stuck_writes_durable_log;
    Alcotest.test_case "check_stuck logs provider failures" `Quick
      test_check_stuck_logs_failures;
    Alcotest.test_case "thinking excerpt logs looping verdict" `Quick
      test_check_thinking_excerpt_logs_looping_verdict;
    Alcotest.test_case "observer_log_path" `Quick test_observer_log_path;
    Alcotest.test_case "append_observer_log writes JSON line" `Quick
      test_append_observer_log_writes_json_line;
    Alcotest.test_case "append_observer_log appends multiple" `Quick
      test_append_observer_log_appends_multiple;
    Alcotest.test_case "log_stuck_check ok verdict" `Quick
      test_log_stuck_check_ok;
    Alcotest.test_case "log_stuck_check stuck verdict" `Quick
      test_log_stuck_check_stuck;
    Alcotest.test_case "log_stuck_check_error" `Quick test_log_stuck_check_error;
    Alcotest.test_case "log_thinking_check sane" `Quick
      test_log_thinking_check_sane;
    Alcotest.test_case "log_thinking_check_error" `Quick
      test_log_thinking_check_error;
    Alcotest.test_case "parse_verdict" `Quick test_parse_verdict;
    Alcotest.test_case "check_stuck filters event messages" `Quick
      test_check_stuck_filters_event_messages;
    Alcotest.test_case
      "B667: check_stuck bypasses LLM for complete terminating response" `Quick
      test_check_stuck_bypasses_llm_for_complete_response;
    Alcotest.test_case
      "B667: check_stuck bypasses LLM when sentence ends in trailing markdown"
      `Quick test_check_stuck_bypasses_llm_for_markdown_trailing_close;
    Alcotest.test_case
      "B667: check_stuck still consults LLM for incomplete response" `Quick
      test_check_stuck_no_bypass_for_incomplete_response;
  ]
