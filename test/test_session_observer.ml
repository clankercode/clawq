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
                "reason"
                "repeating failed tool call" reason
          | _ -> Alcotest.fail "expected high-confidence stuck verdict");
          let log_path = Dot_dir.sub "observer.log" in
          Alcotest.(check bool) "observer log created" true
            (Sys.file_exists log_path);
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
      let history = [ Provider.make_message ~role:"user" ~content:"Check me" ] in
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
                 ~excerpt:"Need to do X. Need to do X. Need to do X."
                 ())
          in
          (match verdict with
          | `Looping reason ->
              Alcotest.(check string)
                "loop reason"
                "repeating the same plan" reason
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
            (string_contains log_text
               "\"reason\":\"repeating the same plan\"")))

let suite =
  [
    Alcotest.test_case "check_stuck writes durable observer log" `Quick
      test_check_stuck_writes_durable_log;
    Alcotest.test_case "check_stuck logs provider failures" `Quick
      test_check_stuck_logs_failures;
    Alcotest.test_case "thinking excerpt logs looping verdict" `Quick
      test_check_thinking_excerpt_logs_looping_verdict;
  ]
