(* test_setup_summarizer.ml — Unit tests for Setup_summarizer pure functions *)

(* ── validate_model ──────────────────────────────────────────────── *)

let validate_model_valid () =
  match Setup_summarizer.validate_model "groq:openai/gpt-oss-120b" with
  | Ok m ->
      Alcotest.(check string)
        "raw" "groq:openai/gpt-oss-120b" (Pmodel.to_string m)
  | Error e -> Alcotest.fail (Printf.sprintf "expected Ok, got Error %S" e)

let validate_model_no_colon () =
  match Setup_summarizer.validate_model "just-a-model" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for missing colon"

let validate_model_empty () =
  match Setup_summarizer.validate_model "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty string"

let validate_model_empty_provider () =
  match Setup_summarizer.validate_model ":model" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty provider"

let validate_model_empty_model () =
  match Setup_summarizer.validate_model "provider:" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty model"

(* ── validate_positive_int ───────────────────────────────────────── *)

let validate_positive_int_valid () =
  Alcotest.(check (result int string))
    "valid" (Ok 42)
    (Setup_summarizer.validate_positive_int "42")

let validate_positive_int_zero () =
  match Setup_summarizer.validate_positive_int "0" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for zero"

let validate_positive_int_negative () =
  match Setup_summarizer.validate_positive_int "-5" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for negative"

let validate_positive_int_non_numeric () =
  match Setup_summarizer.validate_positive_int "abc" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-numeric"

(* ── validate_tool_name ──────────────────────────────────────────── *)

let validate_tool_name_valid () =
  Alcotest.(check (result string string))
    "valid" (Ok "shell_exec")
    (Setup_summarizer.validate_tool_name "shell_exec")

let validate_tool_name_empty () =
  match Setup_summarizer.validate_tool_name "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty"

let validate_tool_name_spaces () =
  match Setup_summarizer.validate_tool_name "bad name" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for spaces"

(* ── validate_threshold_chars ────────────────────────────────────── *)

let validate_threshold_chars_normal () =
  match Setup_summarizer.validate_threshold_chars "1500" with
  | Ok (1500, None) -> ()
  | Ok (_, Some w) -> Alcotest.fail (Printf.sprintf "unexpected warning: %s" w)
  | Ok (n, _) -> Alcotest.fail (Printf.sprintf "unexpected value: %d" n)
  | Error e -> Alcotest.fail (Printf.sprintf "unexpected error: %s" e)

let validate_threshold_chars_low_warning () =
  match Setup_summarizer.validate_threshold_chars "100" with
  | Ok (100, Some _) -> ()
  | Ok (_, None) -> Alcotest.fail "expected warning for low value"
  | _ -> Alcotest.fail "unexpected result"

let validate_threshold_chars_high_warning () =
  match Setup_summarizer.validate_threshold_chars "99999" with
  | Ok (99999, Some _) -> ()
  | Ok (_, None) -> Alcotest.fail "expected warning for high value"
  | _ -> Alcotest.fail "unexpected result"

(* ── validate_p1/p2 cross-validation ─────────────────────────────── *)

let validate_p1_must_exceed_p2 () =
  match Setup_summarizer.validate_p1_max_chars ~p2_max:12000 "5000" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error: p1 <= p2"

let validate_p2_must_be_less_than_p1 () =
  match Setup_summarizer.validate_p2_max_chars ~p1_max:200000 "300000" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error: p2 >= p1"

(* ── build_json roundtrip ────────────────────────────────────────── *)

let build_json_roundtrip () =
  let sc : Runtime_config.summarizer_config =
    {
      summarizer_enabled = true;
      summarizer_model = Pmodel.parse_exn "anthropic:claude-sonnet-4-6";
      escalation_model = Some (Pmodel.parse_exn "anthropic:claude-opus-4-6");
      threshold_chars = 2000;
      p1_max_chars = 150_000;
      p2_max_chars = 10_000;
      context_window_messages = 6;
      excluded_tools = [ "tool_search"; "unsummarize" ];
      max_age_days = 14;
      envelope_template = Some "## Summary\n{summary}";
    }
  in
  let json = Setup_summarizer.build_summarizer_json ~sc in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  Alcotest.(check bool) "enabled" true config.summarizer.summarizer_enabled;
  Alcotest.(check string)
    "model" "anthropic:claude-sonnet-4-6"
    (Pmodel.to_string config.summarizer.summarizer_model);
  (match config.summarizer.escalation_model with
  | Some m ->
      Alcotest.(check string)
        "escalation" "anthropic:claude-opus-4-6" (Pmodel.to_string m)
  | None -> Alcotest.fail "expected escalation_model");
  Alcotest.(check int) "threshold" 2000 config.summarizer.threshold_chars;
  Alcotest.(check int) "p1_max" 150_000 config.summarizer.p1_max_chars;
  Alcotest.(check int) "p2_max" 10_000 config.summarizer.p2_max_chars;
  Alcotest.(check int) "ctx_win" 6 config.summarizer.context_window_messages;
  Alcotest.(check (list string))
    "excluded"
    [ "tool_search"; "unsummarize" ]
    config.summarizer.excluded_tools;
  Alcotest.(check int) "max_age" 14 config.summarizer.max_age_days;
  match config.summarizer.envelope_template with
  | Some t -> Alcotest.(check string) "envelope" "## Summary\n{summary}" t
  | None -> Alcotest.fail "expected envelope_template"

let build_json_defaults () =
  let sc = Runtime_config.default_summarizer_config in
  let json = Setup_summarizer.build_summarizer_json ~sc in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  Alcotest.(check bool) "enabled" true config.summarizer.summarizer_enabled;
  Alcotest.(check int) "threshold" 1500 config.summarizer.threshold_chars;
  Alcotest.(check int) "p1_max" 200_000 config.summarizer.p1_max_chars;
  Alcotest.(check int) "p2_max" 12_000 config.summarizer.p2_max_chars

let build_json_merge_existing () =
  let existing =
    Yojson.Safe.from_string
      {|{"channels":{"cli":true},"default_temperature":0.7}|}
  in
  let sc : Runtime_config.summarizer_config =
    {
      Runtime_config.default_summarizer_config with
      summarizer_enabled = false;
      threshold_chars = 3000;
    }
  in
  let overlay = Setup_summarizer.build_summarizer_json ~sc in
  let result = Setup_common.deep_merge_json existing overlay in
  let config = Config_loader.parse_config ~resolve_secrets:false result in
  Alcotest.(check bool) "enabled" false config.summarizer.summarizer_enabled;
  Alcotest.(check int) "threshold" 3000 config.summarizer.threshold_chars

let build_json_escalation_none () =
  let sc =
    { Runtime_config.default_summarizer_config with escalation_model = None }
  in
  let json = Setup_summarizer.build_summarizer_json ~sc in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  Alcotest.(check bool)
    "escalation is none" true
    (config.summarizer.escalation_model = None)

let build_json_escalation_some () =
  let sc =
    {
      Runtime_config.default_summarizer_config with
      escalation_model = Some (Pmodel.parse_exn "openai:gpt-4o");
    }
  in
  let json = Setup_summarizer.build_summarizer_json ~sc in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.summarizer.escalation_model with
  | Some m ->
      Alcotest.(check string) "escalation" "openai:gpt-4o" (Pmodel.to_string m)
  | None -> Alcotest.fail "expected Some escalation_model"

(* ── post_setup_instructions ─────────────────────────────────────── *)

let post_instructions_content () =
  let sc : Runtime_config.summarizer_config =
    {
      Runtime_config.default_summarizer_config with
      summarizer_enabled = true;
      summarizer_model = Pmodel.parse_exn "groq:llama-3.3-70b";
      threshold_chars = 2500;
    }
  in
  let s = Setup_summarizer.post_setup_instructions ~sc in
  let contains sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool) "has enabled" true (contains "yes");
  Alcotest.(check bool) "has model" true (contains "groq:llama-3.3-70b");
  Alcotest.(check bool) "has unsummarize" true (contains "unsummarize");
  Alcotest.(check bool) "has threshold" true (contains "2500")

(* ── Suite ───────────────────────────────────────────────────────── *)

let suite =
  [
    Alcotest.test_case "validate_model valid" `Quick validate_model_valid;
    Alcotest.test_case "validate_model no colon" `Quick validate_model_no_colon;
    Alcotest.test_case "validate_model empty" `Quick validate_model_empty;
    Alcotest.test_case "validate_model empty provider" `Quick
      validate_model_empty_provider;
    Alcotest.test_case "validate_model empty model" `Quick
      validate_model_empty_model;
    Alcotest.test_case "validate_positive_int valid" `Quick
      validate_positive_int_valid;
    Alcotest.test_case "validate_positive_int zero" `Quick
      validate_positive_int_zero;
    Alcotest.test_case "validate_positive_int negative" `Quick
      validate_positive_int_negative;
    Alcotest.test_case "validate_positive_int non-numeric" `Quick
      validate_positive_int_non_numeric;
    Alcotest.test_case "validate_tool_name valid" `Quick
      validate_tool_name_valid;
    Alcotest.test_case "validate_tool_name empty" `Quick
      validate_tool_name_empty;
    Alcotest.test_case "validate_tool_name spaces" `Quick
      validate_tool_name_spaces;
    Alcotest.test_case "validate_threshold_chars normal" `Quick
      validate_threshold_chars_normal;
    Alcotest.test_case "validate_threshold_chars low warning" `Quick
      validate_threshold_chars_low_warning;
    Alcotest.test_case "validate_threshold_chars high warning" `Quick
      validate_threshold_chars_high_warning;
    Alcotest.test_case "validate_p1 must exceed p2" `Quick
      validate_p1_must_exceed_p2;
    Alcotest.test_case "validate_p2 must be less than p1" `Quick
      validate_p2_must_be_less_than_p1;
    Alcotest.test_case "build_json roundtrip" `Quick build_json_roundtrip;
    Alcotest.test_case "build_json defaults" `Quick build_json_defaults;
    Alcotest.test_case "build_json merge existing" `Quick
      build_json_merge_existing;
    Alcotest.test_case "build_json escalation none" `Quick
      build_json_escalation_none;
    Alcotest.test_case "build_json escalation some" `Quick
      build_json_escalation_some;
    Alcotest.test_case "post_instructions content" `Quick
      post_instructions_content;
  ]
