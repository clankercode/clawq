let test_strip_date_suffix () =
  Alcotest.(check string)
    "strip date suffix" "claude-opus-4-6"
    (Provider.normalize_model_name "claude-opus-4-6-20250301");
  Alcotest.(check string)
    "no suffix" "claude-opus-4-6"
    (Provider.normalize_model_name "claude-opus-4-6");
  Alcotest.(check string)
    "case insensitive" "gpt-4o"
    (Provider.normalize_model_name "GPT-4o")

let test_find_provider_for_model () =
  let providers =
    [
      ( "anthropic",
        {
          Runtime_config.api_key = "sk-abc";
          base_url = Some "https://api.anthropic.com/v1";
          default_model = None;
        } );
      ( "openai",
        {
          Runtime_config.api_key = "sk-xyz";
          base_url = None;
          default_model = None;
        } );
    ]
  in
  let result =
    Provider.find_provider_for_model ~providers
      ~model_name:"anthropic/claude-opus-4-6"
  in
  (match result with
  | Some (name, _) ->
      Alcotest.(check string) "matched anthropic" "anthropic" name
  | None -> Alcotest.fail "expected match for anthropic prefix");
  let result2 =
    Provider.find_provider_for_model ~providers ~model_name:"openai/gpt-4o"
  in
  (match result2 with
  | Some (name, _) -> Alcotest.(check string) "matched openai" "openai" name
  | None -> Alcotest.fail "expected match for openai prefix");
  let result3 =
    Provider.find_provider_for_model ~providers ~model_name:"unknown/some-model"
  in
  Alcotest.(check bool) "no match" true (result3 = None)

let test_find_provider_no_key () =
  let providers =
    [
      ( "anthropic",
        { Runtime_config.api_key = ""; base_url = None; default_model = None }
      );
    ]
  in
  let result =
    Provider.find_provider_for_model ~providers
      ~model_name:"anthropic/claude-opus-4-6"
  in
  Alcotest.(check bool) "no match without key" true (result = None)

let test_find_provider_date_suffix () =
  let providers =
    [
      ( "anthropic",
        {
          Runtime_config.api_key = "sk-abc";
          base_url = None;
          default_model = None;
        } );
    ]
  in
  let result =
    Provider.find_provider_for_model ~providers
      ~model_name:"anthropic/claude-opus-4-6-20250301"
  in
  match result with
  | Some (name, _) ->
      Alcotest.(check string) "matched with date suffix" "anthropic" name
  | None -> Alcotest.fail "expected match with date suffix"

let test_context_window_known () =
  let check name expected =
    Alcotest.(check (option int))
      name (Some expected)
      (Runtime_config.context_window_for_model name)
  in
  check "claude-opus-4-6" 200000;
  check "gpt-4o" 128000;
  check "gpt-4o-mini" 128000;
  check "llama-3.3-70b" 128000;
  check "gemini-1.5-pro" 2097152

let test_context_window_with_prefix () =
  Alcotest.(check (option int))
    "with provider prefix" (Some 200000)
    (Runtime_config.context_window_for_model "anthropic/claude-opus-4-6")

let test_context_window_with_date () =
  Alcotest.(check (option int))
    "with date suffix" (Some 128000)
    (Runtime_config.context_window_for_model "gpt-4o-20250101")

let test_context_window_unknown () =
  Alcotest.(check (option int))
    "unknown model" None
    (Runtime_config.context_window_for_model "some-custom-model")

let suite =
  [
    Alcotest.test_case "strip date suffix + normalize" `Quick
      test_strip_date_suffix;
    Alcotest.test_case "find provider for model" `Quick
      test_find_provider_for_model;
    Alcotest.test_case "find provider no key" `Quick test_find_provider_no_key;
    Alcotest.test_case "find provider date suffix" `Quick
      test_find_provider_date_suffix;
    Alcotest.test_case "context window known models" `Quick
      test_context_window_known;
    Alcotest.test_case "context window with prefix" `Quick
      test_context_window_with_prefix;
    Alcotest.test_case "context window with date" `Quick
      test_context_window_with_date;
    Alcotest.test_case "context window unknown" `Quick
      test_context_window_unknown;
  ]
