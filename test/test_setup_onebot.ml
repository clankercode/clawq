(* test_setup_onebot.ml — Unit tests for Setup_onebot pure functions *)

let validate_ws_url_ws () =
  Alcotest.(check (result string string))
    "ws valid" (Ok "ws://localhost:8080")
    (Setup_onebot.validate_ws_url "ws://localhost:8080")

let validate_ws_url_wss () =
  Alcotest.(check (result string string))
    "wss valid" (Ok "wss://example.com/ws")
    (Setup_onebot.validate_ws_url "wss://example.com/ws")

let validate_ws_url_empty () =
  match Setup_onebot.validate_ws_url "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty ws URL"

let validate_ws_url_http () =
  match Setup_onebot.validate_ws_url "http://localhost:8080" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for http URL (must be ws/wss)"

let validate_ws_url_no_scheme () =
  match Setup_onebot.validate_ws_url "localhost:8080" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for URL without scheme"

let validate_http_url_http () =
  Alcotest.(check (result string string))
    "http valid" (Ok "http://localhost:5700")
    (Setup_onebot.validate_http_url "http://localhost:5700")

let validate_http_url_https () =
  Alcotest.(check (result string string))
    "https valid" (Ok "https://example.com/api")
    (Setup_onebot.validate_http_url "https://example.com/api")

let validate_http_url_empty () =
  match Setup_onebot.validate_http_url "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty http URL"

let validate_http_url_ws () =
  match Setup_onebot.validate_http_url "ws://localhost:8080" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for ws URL (must be http/https)"

let build_json_roundtrip () =
  let json =
    Setup_onebot.build_onebot_json ~ws_url:"ws://localhost:8080"
      ~http_url:"http://localhost:5700" ~access_token:"" ~allow_from:[ "*" ]
      ~allow_groups:[ "*" ]
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.onebot with
  | Some o ->
      Alcotest.(check string) "ws_url" "ws://localhost:8080" o.ws_url;
      Alcotest.(check string) "http_url" "http://localhost:5700" o.http_url;
      Alcotest.(check (option string)) "access_token None" None o.access_token;
      Alcotest.(check (list string)) "allow_from" [ "*" ] o.allow_from;
      Alcotest.(check (list string)) "allow_groups" [ "*" ] o.allow_groups
  | None -> Alcotest.fail "expected onebot config"

let build_json_with_token () =
  let json =
    Setup_onebot.build_onebot_json ~ws_url:"wss://example.com/ws"
      ~http_url:"https://example.com/api" ~access_token:"my_secret_token"
      ~allow_from:[ "12345678" ] ~allow_groups:[ "987654321" ]
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.onebot with
  | Some o ->
      Alcotest.(check (option string))
        "access_token" (Some "my_secret_token") o.access_token;
      Alcotest.(check (list string)) "allow_from" [ "12345678" ] o.allow_from;
      Alcotest.(check (list string))
        "allow_groups" [ "987654321" ] o.allow_groups
  | None -> Alcotest.fail "expected onebot config"

let instructions_content () =
  let s = Setup_onebot.post_setup_instructions in
  let contains sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has docs URL" true
    (contains "https://clawq.org/channels/#onebot");
  Alcotest.(check bool) "has go-cqhttp mention" true (contains "go-cqhttp");
  Alcotest.(check bool) "has daemon start" true (contains "clawq daemon start")

let suite =
  [
    Alcotest.test_case "validate_ws_url ws" `Quick validate_ws_url_ws;
    Alcotest.test_case "validate_ws_url wss" `Quick validate_ws_url_wss;
    Alcotest.test_case "validate_ws_url empty" `Quick validate_ws_url_empty;
    Alcotest.test_case "validate_ws_url http" `Quick validate_ws_url_http;
    Alcotest.test_case "validate_ws_url no_scheme" `Quick
      validate_ws_url_no_scheme;
    Alcotest.test_case "validate_http_url http" `Quick validate_http_url_http;
    Alcotest.test_case "validate_http_url https" `Quick validate_http_url_https;
    Alcotest.test_case "validate_http_url empty" `Quick validate_http_url_empty;
    Alcotest.test_case "validate_http_url ws" `Quick validate_http_url_ws;
    Alcotest.test_case "build_json roundtrip" `Quick build_json_roundtrip;
    Alcotest.test_case "build_json with_token" `Quick build_json_with_token;
    Alcotest.test_case "post_setup_instructions content" `Quick
      instructions_content;
  ]
