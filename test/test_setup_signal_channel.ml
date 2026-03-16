(* test_setup_signal_channel.ml — Unit tests for Setup_signal_channel pure functions *)

let validate_url_valid_https () =
  Alcotest.(check (result string string))
    "https ok" (Ok "https://localhost:8080")
    (Setup_signal_channel.validate_url "https://localhost:8080")

let validate_url_valid_http () =
  Alcotest.(check (result string string))
    "http ok" (Ok "http://localhost:8080")
    (Setup_signal_channel.validate_url "http://localhost:8080")

let validate_url_empty () =
  match Setup_signal_channel.validate_url "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty URL"

let validate_url_no_scheme () =
  match Setup_signal_channel.validate_url "localhost:8080" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for URL without scheme"

let validate_max_chunk_bytes_valid () =
  Alcotest.(check (result string string))
    "valid" (Ok "50000")
    (Setup_signal_channel.validate_max_chunk_bytes "50000")

let validate_max_chunk_bytes_zero () =
  match Setup_signal_channel.validate_max_chunk_bytes "0" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for zero"

let validate_max_chunk_bytes_negative () =
  match Setup_signal_channel.validate_max_chunk_bytes "-100" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for negative"

let validate_max_chunk_bytes_non_integer () =
  match Setup_signal_channel.validate_max_chunk_bytes "abc" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-integer"

let build_json_roundtrip () =
  let json =
    Setup_signal_channel.build_signal_json ~base_url:"http://localhost:8080"
      ~account:"+15551234567" ~api_mode:"json-rpc" ~allow_from:[ "*" ]
      ~max_chunk_bytes:50000
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.signal with
  | Some sg ->
      Alcotest.(check string) "base_url" "http://localhost:8080" sg.base_url;
      Alcotest.(check string) "account" "+15551234567" sg.account;
      Alcotest.(check string) "api_mode" "json-rpc" sg.api_mode;
      Alcotest.(check (list string)) "allow_from" [ "*" ] sg.allow_from;
      Alcotest.(check int) "max_chunk_bytes" 50000 sg.max_chunk_bytes
  | None -> Alcotest.fail "expected signal config"

let build_json_rest_mode () =
  let json =
    Setup_signal_channel.build_signal_json ~base_url:"http://localhost:8080"
      ~account:"+15551234567" ~api_mode:"rest" ~allow_from:[ "*" ]
      ~max_chunk_bytes:10000
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.signal with
  | Some sg ->
      Alcotest.(check string) "api_mode" "rest" sg.api_mode;
      Alcotest.(check int) "max_chunk_bytes" 10000 sg.max_chunk_bytes
  | None -> Alcotest.fail "expected signal config"

let build_json_restricted_users () =
  let json =
    Setup_signal_channel.build_signal_json ~base_url:"http://localhost:8080"
      ~account:"+15551234567" ~api_mode:"json-rpc"
      ~allow_from:[ "+15559876543"; "+15550001111" ]
      ~max_chunk_bytes:50000
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.signal with
  | Some sg ->
      Alcotest.(check (list string))
        "allow_from"
        [ "+15559876543"; "+15550001111" ]
        sg.allow_from
  | None -> Alcotest.fail "expected signal config"

let post_instructions_content () =
  let s = Setup_signal_channel.post_setup_instructions in
  let contains sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has docs url" true
    (contains "https://clawq.org/channels/#signal");
  Alcotest.(check bool) "has signal-cli mention" true (contains "signal-cli")

let suite =
  [
    Alcotest.test_case "validate_url https" `Quick validate_url_valid_https;
    Alcotest.test_case "validate_url http" `Quick validate_url_valid_http;
    Alcotest.test_case "validate_url empty" `Quick validate_url_empty;
    Alcotest.test_case "validate_url no scheme" `Quick validate_url_no_scheme;
    Alcotest.test_case "validate_max_chunk_bytes valid" `Quick
      validate_max_chunk_bytes_valid;
    Alcotest.test_case "validate_max_chunk_bytes zero" `Quick
      validate_max_chunk_bytes_zero;
    Alcotest.test_case "validate_max_chunk_bytes negative" `Quick
      validate_max_chunk_bytes_negative;
    Alcotest.test_case "validate_max_chunk_bytes non-integer" `Quick
      validate_max_chunk_bytes_non_integer;
    Alcotest.test_case "build_json roundtrip" `Quick build_json_roundtrip;
    Alcotest.test_case "build_json rest mode" `Quick build_json_rest_mode;
    Alcotest.test_case "build_json restricted users" `Quick
      build_json_restricted_users;
    Alcotest.test_case "post_instructions content" `Quick
      post_instructions_content;
  ]
