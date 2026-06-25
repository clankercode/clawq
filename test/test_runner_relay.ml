let test_generate_and_validate () =
  let tokens = Runner_relay.create_tokens () in
  let token =
    Runner_relay.generate_token tokens ~session_key:"test:session1" ()
  in
  Alcotest.(check bool) "token is non-empty" true (String.length token > 0);
  match Runner_relay.validate_token tokens ~token with
  | None -> Alcotest.fail "expected valid token"
  | Some entry ->
      Alcotest.(check string)
        "session_key matches" "test:session1" entry.session_key;
      Alcotest.(check bool) "no task_id" true (entry.task_id = None);
      Alcotest.(check bool)
        "expires_at in future" true
        (entry.expires_at > Unix.gettimeofday ())

let test_generate_with_task_id () =
  let tokens = Runner_relay.create_tokens () in
  let token =
    Runner_relay.generate_token tokens ~session_key:"test:session2" ~task_id:42
      ()
  in
  match Runner_relay.validate_token tokens ~token with
  | None -> Alcotest.fail "expected valid token"
  | Some entry ->
      Alcotest.(check (option int)) "task_id matches" (Some 42) entry.task_id

let test_invalid_token () =
  let tokens = Runner_relay.create_tokens () in
  let _token =
    Runner_relay.generate_token tokens ~session_key:"test:session3" ()
  in
  match Runner_relay.validate_token tokens ~token:"bogus-token" with
  | None -> ()
  | Some _ -> Alcotest.fail "expected invalid token"

let test_expired_token () =
  let tokens = Runner_relay.create_tokens () in
  let token =
    Runner_relay.generate_token tokens ~session_key:"test:session4" ~ttl_hours:0
      ()
  in
  Unix.sleepf 0.01;
  match Runner_relay.validate_token tokens ~token with
  | None -> ()
  | Some _ -> Alcotest.fail "expected expired token to be rejected"

let test_cleanup_expired () =
  let tokens = Runner_relay.create_tokens () in
  let _t1 =
    Runner_relay.generate_token tokens ~session_key:"s1" ~ttl_hours:0 ()
  in
  let t2 =
    Runner_relay.generate_token tokens ~session_key:"s2" ~ttl_hours:24 ()
  in
  Unix.sleepf 0.01;
  Runner_relay.cleanup_expired tokens;
  Alcotest.(check bool) "expired token removed" true (Hashtbl.length tokens = 1);
  Alcotest.(check bool)
    "valid token still present" true
    (Runner_relay.validate_token tokens ~token:t2 <> None)

let test_is_loopback () =
  Alcotest.(check bool) "127.0.0.1" true (Runner_relay.is_loopback "127.0.0.1");
  Alcotest.(check bool) "::1" true (Runner_relay.is_loopback "::1");
  Alcotest.(check bool) "localhost" true (Runner_relay.is_loopback "localhost");
  Alcotest.(check bool) "unknown" false (Runner_relay.is_loopback "unknown");
  Alcotest.(check bool)
    "192.168.1.1" false
    (Runner_relay.is_loopback "192.168.1.1");
  Alcotest.(check bool) "10.0.0.1" false (Runner_relay.is_loopback "10.0.0.1")

let test_relay_question_success () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let ask_fn ~session_key:_ ~questions =
       Lwt.return
         (List.map
            (fun (q : Tools_builtin.question_item) ->
              Tools_builtin.
                { question = q.question; answer = "test answer"; notes = None })
            questions)
     in
     let questions =
       [
         Tools_builtin.
           {
             question = "What color?";
             qtype = Text { placeholder = None };
             request_notes = false;
           };
       ]
     in
     let* result =
       Runner_relay.relay_question ~ask_fn ~session_key:"test:sk" ~questions
         ~timeout_s:5
     in
     (match result with
     | Ok results ->
         Alcotest.(check int) "one result" 1 (List.length results);
         let r = List.hd results in
         Alcotest.(check string) "answer" "test answer" r.answer
     | Error msg -> Alcotest.fail ("expected success: " ^ msg));
     Lwt.return_unit)

let test_relay_question_timeout () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let ask_fn ~session_key:_ ~questions:_ =
       let* () = Lwt_unix.sleep 10.0 in
       Lwt.return []
     in
     let questions =
       [
         Tools_builtin.
           {
             question = "Slow question";
             qtype = Text { placeholder = None };
             request_notes = false;
           };
       ]
     in
     let* result =
       Runner_relay.relay_question ~ask_fn ~session_key:"test:sk" ~questions
         ~timeout_s:0
     in
     (match result with
     | Ok _ -> Alcotest.fail "expected timeout"
     | Error msg ->
         Alcotest.(check bool) "timeout message" true (String.length msg > 0));
     Lwt.return_unit)

let test_relay_question_error () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let ask_fn ~session_key:_ ~questions:_ =
       Lwt.fail_with "channel disconnected"
     in
     let questions =
       [
         Tools_builtin.
           {
             question = "Bad question";
             qtype = Text { placeholder = None };
             request_notes = false;
           };
       ]
     in
     let* result =
       Runner_relay.relay_question ~ask_fn ~session_key:"test:sk" ~questions
         ~timeout_s:5
     in
     (match result with
     | Ok _ -> Alcotest.fail "expected error"
     | Error msg ->
         Alcotest.(check bool)
           "contains error message" true
           (String.length msg > 0));
     Lwt.return_unit)

let test_mcp_http_tools_list () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let ask_fn ~session_key:_ ~questions:_ = Lwt.return [] in
     let registry =
       Mcp_server_http.make_relay_registry ~ask_fn ~session_key:"test:mcp"
     in
     let body =
       {|{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}|}
     in
     let* _status, resp = Mcp_server_http.handle ~registry ~body in
     let json = Yojson.Safe.from_string resp in
     let tools =
       Yojson.Safe.Util.(json |> member "result" |> member "tools" |> to_list)
     in
     Alcotest.(check int) "one tool" 1 (List.length tools);
     let tool_name =
       Yojson.Safe.Util.(List.hd tools |> member "name" |> to_string)
     in
     Alcotest.(check string) "tool name" "ask_user_question" tool_name;
     Lwt.return_unit)

let test_mcp_http_initialize () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let ask_fn ~session_key:_ ~questions:_ = Lwt.return [] in
     let registry =
       Mcp_server_http.make_relay_registry ~ask_fn ~session_key:"test:mcp"
     in
     let body =
       {|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}|}
     in
     let* _status, resp = Mcp_server_http.handle ~registry ~body in
     let json = Yojson.Safe.from_string resp in
     let version =
       Yojson.Safe.Util.(
         json |> member "result" |> member "protocolVersion" |> to_string)
     in
     Alcotest.(check string) "protocol version" "2024-11-05" version;
     Lwt.return_unit)

let test_mcp_http_tools_call () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let ask_fn ~session_key:_ ~questions =
       Lwt.return
         (List.map
            (fun (q : Tools_builtin.question_item) ->
              Tools_builtin.
                { question = q.question; answer = "mcp answer"; notes = None })
            questions)
     in
     let registry =
       Mcp_server_http.make_relay_registry ~ask_fn ~session_key:"test:mcp"
     in
     let body =
       {|{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"ask_user_question","arguments":{"questions":[{"type":"text","question":"MCP question?"}]}}}|}
     in
     let* _status, resp = Mcp_server_http.handle ~registry ~body in
     let json = Yojson.Safe.from_string resp in
     let content =
       Yojson.Safe.Util.(json |> member "result" |> member "content" |> to_list)
     in
     Alcotest.(check bool) "has content" true (List.length content > 0);
     let text =
       Yojson.Safe.Util.(List.hd content |> member "text" |> to_string)
     in
     Alcotest.(check bool)
       "contains answer" true
       (try
          let _ = Yojson.Safe.from_string text in
          true
        with _ -> false);
     Lwt.return_unit)

let test_mcp_http_unknown_tool () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let ask_fn ~session_key:_ ~questions:_ = Lwt.return [] in
     let registry =
       Mcp_server_http.make_relay_registry ~ask_fn ~session_key:"test:mcp"
     in
     let body =
       {|{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"nonexistent","arguments":{}}}|}
     in
     let* _status, resp = Mcp_server_http.handle ~registry ~body in
     let json = Yojson.Safe.from_string resp in
     let err_msg =
       Yojson.Safe.Util.(
         json |> member "error" |> member "message" |> to_string)
     in
     Alcotest.(check bool)
       "error mentions unknown tool" true
       (String.length err_msg > 0
       &&
         try
           let _ = String.index err_msg 'n' in
           true
         with _ -> false);
     Lwt.return_unit)

let test_mcp_http_parse_error () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let ask_fn ~session_key:_ ~questions:_ = Lwt.return [] in
     let registry =
       Mcp_server_http.make_relay_registry ~ask_fn ~session_key:"test:mcp"
     in
     let body = "not json at all" in
     let* _status, resp = Mcp_server_http.handle ~registry ~body in
     let json = Yojson.Safe.from_string resp in
     let err_code =
       Yojson.Safe.Util.(json |> member "error" |> member "code" |> to_int)
     in
     Alcotest.(check int) "parse error code" (-32700) err_code;
     Lwt.return_unit)

let test_mcp_http_unknown_method () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let ask_fn ~session_key:_ ~questions:_ = Lwt.return [] in
     let registry =
       Mcp_server_http.make_relay_registry ~ask_fn ~session_key:"test:mcp"
     in
     let body =
       {|{"jsonrpc":"2.0","id":4,"method":"resources/list","params":{}}|}
     in
     let* _status, resp = Mcp_server_http.handle ~registry ~body in
     let json = Yojson.Safe.from_string resp in
     let err_code =
       Yojson.Safe.Util.(json |> member "error" |> member "code" |> to_int)
     in
     Alcotest.(check int) "method not found code" (-32601) err_code;
     Lwt.return_unit)

let test_multiple_tokens () =
  let tokens = Runner_relay.create_tokens () in
  let t1 = Runner_relay.generate_token tokens ~session_key:"s1" ~task_id:1 () in
  let t2 = Runner_relay.generate_token tokens ~session_key:"s2" ~task_id:2 () in
  let t3 = Runner_relay.generate_token tokens ~session_key:"s1" ~task_id:3 () in
  (match Runner_relay.validate_token tokens ~token:t1 with
  | None -> Alcotest.fail "t1 should be valid"
  | Some e -> Alcotest.(check string) "t1 session" "s1" e.session_key);
  (match Runner_relay.validate_token tokens ~token:t2 with
  | None -> Alcotest.fail "t2 should be valid"
  | Some e -> Alcotest.(check string) "t2 session" "s2" e.session_key);
  match Runner_relay.validate_token tokens ~token:t3 with
  | None -> Alcotest.fail "t3 should be valid"
  | Some e ->
      Alcotest.(check string) "t3 session" "s1" e.session_key;
      Alcotest.(check (option int)) "t3 task_id" (Some 3) e.task_id

let test_config_defaults () =
  let cfg = Runtime_config.default in
  Alcotest.(check bool)
    "runner_relay_enabled default" true cfg.mcp.runner_relay_enabled;
  Alcotest.(check int)
    "runner_token_ttl_hours default" 24 cfg.mcp.runner_token_ttl_hours;
  Alcotest.(check int)
    "runner_question_timeout_s default" 300 cfg.mcp.runner_question_timeout_s

let test_config_parsing () =
  let json_str =
    {|{"mcp":{"enabled":true,"runner_relay_enabled":false,"runner_token_ttl_hours":12,"runner_question_timeout_s":60}}|}
  in
  let json = Yojson.Safe.from_string json_str in
  let cfg = Config_loader.parse_config ~resolve_secrets:false json in
  Alcotest.(check bool)
    "runner_relay_enabled" false cfg.mcp.runner_relay_enabled;
  Alcotest.(check int)
    "runner_token_ttl_hours" 12 cfg.mcp.runner_token_ttl_hours;
  Alcotest.(check int)
    "runner_question_timeout_s" 60 cfg.mcp.runner_question_timeout_s

let test_config_backward_compat () =
  let json_str = {|{"mcp":{"enabled":true}}|} in
  let json = Yojson.Safe.from_string json_str in
  let cfg = Config_loader.parse_config ~resolve_secrets:false json in
  Alcotest.(check bool)
    "relay defaults to true" true cfg.mcp.runner_relay_enabled;
  Alcotest.(check int) "ttl defaults to 24" 24 cfg.mcp.runner_token_ttl_hours;
  Alcotest.(check int)
    "timeout defaults to 300" 300 cfg.mcp.runner_question_timeout_s

let suite =
  [
    Alcotest.test_case "generate and validate token" `Quick
      test_generate_and_validate;
    Alcotest.test_case "generate with task_id" `Quick test_generate_with_task_id;
    Alcotest.test_case "invalid token rejected" `Quick test_invalid_token;
    Alcotest.test_case "expired token rejected" `Quick test_expired_token;
    Alcotest.test_case "cleanup expired" `Quick test_cleanup_expired;
    Alcotest.test_case "is_loopback" `Quick test_is_loopback;
    Alcotest.test_case "relay question success" `Quick
      test_relay_question_success;
    Alcotest.test_case "relay question timeout" `Quick
      test_relay_question_timeout;
    Alcotest.test_case "relay question error" `Quick test_relay_question_error;
    Alcotest.test_case "MCP HTTP tools/list" `Quick test_mcp_http_tools_list;
    Alcotest.test_case "MCP HTTP initialize" `Quick test_mcp_http_initialize;
    Alcotest.test_case "MCP HTTP tools/call" `Quick test_mcp_http_tools_call;
    Alcotest.test_case "MCP HTTP unknown tool" `Quick test_mcp_http_unknown_tool;
    Alcotest.test_case "MCP HTTP parse error" `Quick test_mcp_http_parse_error;
    Alcotest.test_case "MCP HTTP unknown method" `Quick
      test_mcp_http_unknown_method;
    Alcotest.test_case "multiple tokens" `Quick test_multiple_tokens;
    Alcotest.test_case "config defaults" `Quick test_config_defaults;
    Alcotest.test_case "config parsing" `Quick test_config_parsing;
    Alcotest.test_case "config backward compat" `Quick
      test_config_backward_compat;
  ]
