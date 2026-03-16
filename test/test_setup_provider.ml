(* test_setup_provider.ml — Unit tests for Setup_provider pure functions *)

let validate_name_valid () =
  Alcotest.(check (result string string))
    "valid" (Ok "openai-codex")
    (Setup_provider.validate_provider_name "openai-codex")

let validate_name_valid_underscores () =
  Alcotest.(check (result string string))
    "underscores ok" (Ok "my_provider_2")
    (Setup_provider.validate_provider_name "my_provider_2")

let validate_name_empty () =
  match Setup_provider.validate_provider_name "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty name"

let validate_name_whitespace_only () =
  match Setup_provider.validate_provider_name "   " with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for whitespace-only name"

let validate_name_uppercase () =
  match Setup_provider.validate_provider_name "OpenAI" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for uppercase name"

let validate_name_spaces () =
  match Setup_provider.validate_provider_name "open ai" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for spaces in name"

let validate_name_special_chars () =
  match Setup_provider.validate_provider_name "open@ai" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for special chars"

let validate_name_trimmed () =
  (* leading/trailing spaces should be trimmed and then checked *)
  match Setup_provider.validate_provider_name "  openai  " with
  | Ok "openai" -> ()
  | Ok s -> Alcotest.failf "expected 'openai' but got '%s'" s
  | Error _ -> Alcotest.fail "expected success after trim"

let validate_api_key_valid () =
  Alcotest.(check (result string string))
    "valid" (Ok "sk-abc123")
    (Setup_provider.validate_api_key "sk-abc123")

let validate_api_key_empty () =
  match Setup_provider.validate_api_key "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty key"

let validate_api_key_whitespace_only () =
  match Setup_provider.validate_api_key "   " with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for whitespace-only key"

let validate_api_key_trimmed () =
  match Setup_provider.validate_api_key "  mykey  " with
  | Ok "mykey" -> ()
  | Ok s -> Alcotest.failf "expected 'mykey' but got '%s'" s
  | Error _ -> Alcotest.fail "expected success"

let validate_base_url_empty () =
  Alcotest.(check (result string string))
    "empty ok" (Ok "")
    (Setup_provider.validate_base_url "")

let validate_base_url_https () =
  Alcotest.(check (result string string))
    "https ok" (Ok "https://api.example.com")
    (Setup_provider.validate_base_url "https://api.example.com")

let validate_base_url_http () =
  Alcotest.(check (result string string))
    "http ok" (Ok "http://localhost:11434")
    (Setup_provider.validate_base_url "http://localhost:11434")

let validate_base_url_invalid () =
  match Setup_provider.validate_base_url "ftp://example.com" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for ftp:// url"

let validate_base_url_no_scheme () =
  match Setup_provider.validate_base_url "example.com" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for url without scheme"

let build_json_basic () =
  let json =
    Setup_provider.build_provider_json ~name:"openai-codex" ~api_key:"sk-abc"
      ~base_url:"" ~default_model:""
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match List.assoc_opt "openai-codex" config.providers with
  | Some pc -> Alcotest.(check string) "api_key" "sk-abc" pc.api_key
  | None -> Alcotest.fail "expected openai-codex provider"

let build_json_with_base_url () =
  let json =
    Setup_provider.build_provider_json ~name:"ollama" ~api_key:""
      ~base_url:"http://localhost:11434" ~default_model:"llama3"
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match List.assoc_opt "ollama" config.providers with
  | Some pc ->
      Alcotest.(check (option string))
        "base_url" (Some "http://localhost:11434") pc.base_url;
      Alcotest.(check (option string))
        "default_model" (Some "llama3") pc.default_model
  | None -> Alcotest.fail "expected ollama provider"

let build_json_no_base_url_empty_model () =
  let json =
    Setup_provider.build_provider_json ~name:"anthropic" ~api_key:"sk-ant-xyz"
      ~base_url:"" ~default_model:""
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match List.assoc_opt "anthropic" config.providers with
  | Some pc ->
      Alcotest.(check (option string)) "base_url none" None pc.base_url;
      Alcotest.(check (option string))
        "default_model none" None pc.default_model
  | None -> Alcotest.fail "expected anthropic provider"

let suite =
  [
    Alcotest.test_case "validate_name valid" `Quick validate_name_valid;
    Alcotest.test_case "validate_name underscores" `Quick
      validate_name_valid_underscores;
    Alcotest.test_case "validate_name empty" `Quick validate_name_empty;
    Alcotest.test_case "validate_name whitespace" `Quick
      validate_name_whitespace_only;
    Alcotest.test_case "validate_name uppercase" `Quick validate_name_uppercase;
    Alcotest.test_case "validate_name spaces" `Quick validate_name_spaces;
    Alcotest.test_case "validate_name special chars" `Quick
      validate_name_special_chars;
    Alcotest.test_case "validate_name trimmed" `Quick validate_name_trimmed;
    Alcotest.test_case "validate_api_key valid" `Quick validate_api_key_valid;
    Alcotest.test_case "validate_api_key empty" `Quick validate_api_key_empty;
    Alcotest.test_case "validate_api_key whitespace" `Quick
      validate_api_key_whitespace_only;
    Alcotest.test_case "validate_api_key trimmed" `Quick
      validate_api_key_trimmed;
    Alcotest.test_case "validate_base_url empty" `Quick validate_base_url_empty;
    Alcotest.test_case "validate_base_url https" `Quick validate_base_url_https;
    Alcotest.test_case "validate_base_url http" `Quick validate_base_url_http;
    Alcotest.test_case "validate_base_url invalid" `Quick
      validate_base_url_invalid;
    Alcotest.test_case "validate_base_url no scheme" `Quick
      validate_base_url_no_scheme;
    Alcotest.test_case "build_json basic" `Quick build_json_basic;
    Alcotest.test_case "build_json with base_url" `Quick
      build_json_with_base_url;
    Alcotest.test_case "build_json no base_url empty model" `Quick
      build_json_no_base_url_empty_model;
  ]
