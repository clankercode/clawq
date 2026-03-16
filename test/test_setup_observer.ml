(* test_setup_observer.ml — Unit tests for Setup_observer pure functions *)

let validate_model_valid () =
  Alcotest.(check (result string string))
    "valid model" (Ok "groq:openai/gpt-oss-120b")
    (Setup_observer.validate_model "groq:openai/gpt-oss-120b")

let validate_model_empty () =
  match Setup_observer.validate_model "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty model"

let validate_model_no_provider () =
  match Setup_observer.validate_model "gpt-4" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for bare model name"

let validate_positive_int_valid () =
  Alcotest.(check (result string string))
    "valid int" (Ok "5")
    (Setup_observer.validate_positive_int "5")

let validate_positive_int_zero () =
  match Setup_observer.validate_positive_int "0" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for zero"

let validate_positive_int_negative () =
  match Setup_observer.validate_positive_int "-3" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for negative"

let validate_positive_int_non_int () =
  match Setup_observer.validate_positive_int "abc" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-integer"

let build_json_roundtrip () =
  let json =
    Setup_observer.build_observer_json ~enabled:true
      ~model:"groq:openai/gpt-oss-120b" ~check_every_n_messages:5
      ~round1_window:8 ~round2_window:30 ~thinking_token_threshold:5000
      ~consecutive_errors_threshold:3 ~repeat_call_threshold:2
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  let obs = config.observer in
  Alcotest.(check bool) "enabled" true obs.enabled;
  Alcotest.(check string)
    "model" "groq:openai/gpt-oss-120b"
    (Pmodel.to_string obs.model);
  Alcotest.(check int) "check_every_n_messages" 5 obs.check_every_n_messages;
  Alcotest.(check int) "round1_window" 8 obs.round1_window;
  Alcotest.(check int) "round2_window" 30 obs.round2_window;
  Alcotest.(check int)
    "thinking_token_threshold" 5000 obs.thinking_token_threshold;
  Alcotest.(check int)
    "consecutive_errors_threshold" 3 obs.consecutive_errors_threshold;
  Alcotest.(check int) "repeat_call_threshold" 2 obs.repeat_call_threshold

let build_json_disabled () =
  let json =
    Setup_observer.build_observer_json ~enabled:false ~model:"openai:gpt-4o"
      ~check_every_n_messages:10 ~round1_window:5 ~round2_window:20
      ~thinking_token_threshold:1000 ~consecutive_errors_threshold:5
      ~repeat_call_threshold:3
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  let obs = config.observer in
  Alcotest.(check bool) "disabled" false obs.enabled;
  Alcotest.(check string) "model" "openai:gpt-4o" (Pmodel.to_string obs.model);
  Alcotest.(check int) "check_every_n_messages" 10 obs.check_every_n_messages

let post_instructions_content () =
  let s = Setup_observer.post_setup_instructions in
  let contains sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has docs url" true
    (contains "https://clawq.org/observer/");
  Alcotest.(check bool) "mentions model" true (contains "model")

let suite =
  [
    Alcotest.test_case "validate_model valid" `Quick validate_model_valid;
    Alcotest.test_case "validate_model empty" `Quick validate_model_empty;
    Alcotest.test_case "validate_model no provider" `Quick
      validate_model_no_provider;
    Alcotest.test_case "validate_positive_int valid" `Quick
      validate_positive_int_valid;
    Alcotest.test_case "validate_positive_int zero" `Quick
      validate_positive_int_zero;
    Alcotest.test_case "validate_positive_int negative" `Quick
      validate_positive_int_negative;
    Alcotest.test_case "validate_positive_int non-int" `Quick
      validate_positive_int_non_int;
    Alcotest.test_case "build_json roundtrip" `Quick build_json_roundtrip;
    Alcotest.test_case "build_json disabled" `Quick build_json_disabled;
    Alcotest.test_case "post_instructions content" `Quick
      post_instructions_content;
  ]
