(* test_setup_imessage.ml — Unit tests for Setup_imessage pure functions *)

let validate_poll_interval_valid () =
  Alcotest.(check (result string string))
    "valid float" (Ok "5.0")
    (Setup_imessage.validate_poll_interval "5.0")

let validate_poll_interval_integer () =
  Alcotest.(check (result string string))
    "integer ok" (Ok "10")
    (Setup_imessage.validate_poll_interval "10")

let validate_poll_interval_zero () =
  match Setup_imessage.validate_poll_interval "0" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for zero"

let validate_poll_interval_negative () =
  match Setup_imessage.validate_poll_interval "-1.0" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for negative"

let validate_poll_interval_non_number () =
  match Setup_imessage.validate_poll_interval "abc" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-number"

let build_json_roundtrip () =
  let json =
    Setup_imessage.build_imessage_json ~poll_interval_s:5.0 ~allow_from:[ "*" ]
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.imessage with
  | Some im ->
      Alcotest.(check (float 0.001)) "poll_interval_s" 5.0 im.poll_interval_s;
      Alcotest.(check (list string)) "allow_from" [ "*" ] im.allow_from
  | None -> Alcotest.fail "expected imessage config"

let build_json_custom_values () =
  let json =
    Setup_imessage.build_imessage_json ~poll_interval_s:2.5
      ~allow_from:[ "+15551234567"; "+15559876543" ]
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.imessage with
  | Some im ->
      Alcotest.(check (float 0.001)) "poll_interval_s" 2.5 im.poll_interval_s;
      Alcotest.(check (list string))
        "allow_from"
        [ "+15551234567"; "+15559876543" ]
        im.allow_from
  | None -> Alcotest.fail "expected imessage config"

let post_instructions_content () =
  let s = Setup_imessage.post_setup_instructions in
  let contains sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has docs url" true
    (contains "https://clawq.org/channels/#imessage");
  Alcotest.(check bool) "has poll mention" true (contains "poll")

let suite =
  [
    Alcotest.test_case "validate_poll_interval valid" `Quick
      validate_poll_interval_valid;
    Alcotest.test_case "validate_poll_interval integer" `Quick
      validate_poll_interval_integer;
    Alcotest.test_case "validate_poll_interval zero" `Quick
      validate_poll_interval_zero;
    Alcotest.test_case "validate_poll_interval negative" `Quick
      validate_poll_interval_negative;
    Alcotest.test_case "validate_poll_interval non-number" `Quick
      validate_poll_interval_non_number;
    Alcotest.test_case "build_json roundtrip" `Quick build_json_roundtrip;
    Alcotest.test_case "build_json custom values" `Quick
      build_json_custom_values;
    Alcotest.test_case "post_instructions content" `Quick
      post_instructions_content;
  ]
