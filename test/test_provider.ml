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
          project_id = None;
          location = None;
          service_account_json = None;
        } );
      ( "openai",
        {
          Runtime_config.api_key = "sk-xyz";
          base_url = None;
          default_model = None;
          project_id = None;
          location = None;
          service_account_json = None;
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
        {
          Runtime_config.api_key = "";
          base_url = None;
          default_model = None;
          project_id = None;
          location = None;
          service_account_json = None;
        } );
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
          project_id = None;
          location = None;
          service_account_json = None;
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

let make_provider ?(base_url = None) api_key =
  {
    Runtime_config.api_key;
    base_url;
    default_model = None;
    project_id = None;
    location = None;
    service_account_json = None;
  }

let test_detect_kind_anthropic () =
  let cfg = make_provider "sk-ant-abc123" in
  Alcotest.(check bool)
    "anthropic key detected" true
    (Provider.detect_kind cfg = Provider.Anthropic)

let test_detect_kind_gemini () =
  let cfg = make_provider "AIzaSyABC123" in
  Alcotest.(check bool)
    "gemini key detected" true
    (Provider.detect_kind cfg = Provider.Gemini)

let test_detect_kind_ollama_localhost () =
  let cfg = make_provider ~base_url:(Some "http://localhost:11434") "" in
  Alcotest.(check bool)
    "ollama localhost detected" true
    (Provider.detect_kind cfg = Provider.Ollama)

let test_detect_kind_ollama_url () =
  let cfg =
    make_provider ~base_url:(Some "http://my-ollama-server.local/v1") "anykey"
  in
  Alcotest.(check bool)
    "ollama url detected" true
    (Provider.detect_kind cfg = Provider.Ollama)

let test_detect_kind_vertex () =
  let cfg =
    make_provider
      ~base_url:(Some "https://us-central1-aiplatform.googleapis.com/v1")
      "anykey"
  in
  Alcotest.(check bool)
    "vertex url detected" true
    (Provider.detect_kind cfg = Provider.Vertex)

let test_detect_kind_openai_compat_default () =
  let cfg = make_provider "sk-openai-abc" in
  Alcotest.(check bool)
    "openai compat default" true
    (Provider.detect_kind cfg = Provider.OpenAICompat)

let test_detect_kind_openai_compat_openrouter () =
  let cfg =
    make_provider ~base_url:(Some "https://openrouter.ai/api/v1") "sk-or-abc"
  in
  Alcotest.(check bool)
    "openrouter is openai compat" true
    (Provider.detect_kind cfg = Provider.OpenAICompat)

let test_detect_kind_anthropic_short_key () =
  (* key shorter than 7 chars should NOT match sk-ant- prefix *)
  let cfg = make_provider "sk-ant" in
  Alcotest.(check bool)
    "short sk-ant key not anthropic" true
    (Provider.detect_kind cfg <> Provider.Anthropic)

let test_detect_kind_gemini_short_key () =
  (* key shorter than 6 chars should NOT match AIzaSy prefix *)
  let cfg = make_provider "AIzaS" in
  Alcotest.(check bool)
    "short AIzaS key not gemini" true
    (Provider.detect_kind cfg <> Provider.Gemini)

let test_normalize_empty () =
  Alcotest.(check string) "empty string" "" (Provider.normalize_model_name "")

let test_normalize_already_lower () =
  Alcotest.(check string)
    "already lowercase" "gpt-4o"
    (Provider.normalize_model_name "gpt-4o")

let test_normalize_mixed_case_date () =
  (* uppercase with date suffix: strip date, then lowercase *)
  Alcotest.(check string)
    "uppercase with date suffix" "claude-opus-4-6"
    (Provider.normalize_model_name "Claude-Opus-4-6-20250301")

let test_context_window_claude3 () =
  Alcotest.(check (option int))
    "claude-3.5-sonnet" (Some 200000)
    (Runtime_config.context_window_for_model "claude-3.5-sonnet")

let test_context_window_deepseek () =
  Alcotest.(check (option int))
    "deepseek-r1" (Some 128000)
    (Runtime_config.context_window_for_model "deepseek-r1")

let test_find_provider_first_wins () =
  let providers =
    [
      ( "anthropic",
        {
          Runtime_config.api_key = "sk-abc";
          base_url = None;
          default_model = None;
          project_id = None;
          location = None;
          service_account_json = None;
        } );
      ( "anthropic2",
        {
          Runtime_config.api_key = "sk-xyz";
          base_url = None;
          default_model = None;
          project_id = None;
          location = None;
          service_account_json = None;
        } );
    ]
  in
  let result =
    Provider.find_provider_for_model ~providers
      ~model_name:"anthropic/claude-opus-4-6"
  in
  match result with
  | Some (name, _) ->
      Alcotest.(check string) "first provider wins" "anthropic" name
  | None -> Alcotest.fail "expected first provider match"

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
    Alcotest.test_case "detect kind anthropic" `Quick test_detect_kind_anthropic;
    Alcotest.test_case "detect kind gemini" `Quick test_detect_kind_gemini;
    Alcotest.test_case "detect kind ollama localhost" `Quick
      test_detect_kind_ollama_localhost;
    Alcotest.test_case "detect kind ollama url" `Quick
      test_detect_kind_ollama_url;
    Alcotest.test_case "detect kind vertex" `Quick test_detect_kind_vertex;
    Alcotest.test_case "detect kind openai compat default" `Quick
      test_detect_kind_openai_compat_default;
    Alcotest.test_case "detect kind openrouter openai compat" `Quick
      test_detect_kind_openai_compat_openrouter;
    Alcotest.test_case "detect kind short sk-ant not anthropic" `Quick
      test_detect_kind_anthropic_short_key;
    Alcotest.test_case "detect kind short AIzaS not gemini" `Quick
      test_detect_kind_gemini_short_key;
    Alcotest.test_case "normalize empty string" `Quick test_normalize_empty;
    Alcotest.test_case "normalize already lowercase" `Quick
      test_normalize_already_lower;
    Alcotest.test_case "normalize mixed case with date" `Quick
      test_normalize_mixed_case_date;
    Alcotest.test_case "context window claude3 sonnet" `Quick
      test_context_window_claude3;
    Alcotest.test_case "context window deepseek" `Quick
      test_context_window_deepseek;
    Alcotest.test_case "find provider first wins" `Quick
      test_find_provider_first_wins;
  ]
