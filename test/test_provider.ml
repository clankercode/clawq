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
          Runtime_config.default_provider_config with
          api_key = "sk-abc";
          base_url = Some "https://api.anthropic.com/v1";
        } );
      ( "openai",
        { Runtime_config.default_provider_config with api_key = "sk-xyz" } );
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
      ("anthropic", { Runtime_config.default_provider_config with api_key = "" });
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
        { Runtime_config.default_provider_config with api_key = "sk-abc" } );
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
  check "gpt-5.4" 272000;
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

let test_context_window_codex_prefix () =
  Alcotest.(check (option int))
    "codex provider prefix" (Some 272000)
    (Runtime_config.context_window_for_model "openai-codex/gpt-5.4")

let test_context_window_uses_configured_override () =
  Alcotest.(check (option int))
    "configured override" (Some 384000)
    (Runtime_config.context_window_for_model
       ~configured_limits:[ ("openai-codex/gpt-5.4", 384000) ]
       "gpt-5.4")

let test_context_window_unknown () =
  Alcotest.(check (option int))
    "unknown model" None
    (Runtime_config.context_window_for_model "some-custom-model")

let make_provider ?(base_url = None) api_key =
  { Runtime_config.default_provider_config with api_key; base_url }

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

let test_detect_kind_openai_codex_explicit () =
  let cfg = { (make_provider "") with kind = Some "openai-codex" } in
  Alcotest.(check bool)
    "explicit codex kind" true
    (Provider.detect_kind cfg = Provider.OpenAICodex)

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

let test_detect_kind_cohere_url () =
  let cfg =
    make_provider ~base_url:(Some "https://api.cohere.com/v2") "anykey"
  in
  Alcotest.(check bool)
    "cohere url detected" true
    (Provider.detect_kind cfg = Provider.Cohere)

let test_detect_kind_cohere_by_name () =
  let cfg = make_provider "some-cohere-key" in
  Alcotest.(check bool)
    "cohere by name" true
    (Provider.detect_kind ~name:"cohere" cfg = Provider.Cohere)

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
        { Runtime_config.default_provider_config with api_key = "sk-abc" } );
      ( "anthropic2",
        { Runtime_config.default_provider_config with api_key = "sk-xyz" } );
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

let test_find_provider_with_codex_oauth_auth () =
  let providers =
    [
      ( "openai-codex",
        {
          Runtime_config.default_provider_config with
          kind = Some "openai-codex";
          base_url = Some "https://chatgpt.com/backend-api/codex";
          default_model = Some "openai-codex/gpt-5-codex";
          codex_oauth =
            Some
              {
                Runtime_config.access_token = "access-token";
                refresh_token = "refresh-token";
                expires_at_ms = 1730000000000;
                account_id = None;
                email = None;
              };
        } );
    ]
  in
  match
    Provider.find_provider_for_model ~providers
      ~model_name:"openai-codex/gpt-5-codex"
  with
  | Some (name, _) ->
      Alcotest.(check string) "matched codex provider" "openai-codex" name
  | None -> Alcotest.fail "expected match for codex oauth provider"

let test_find_provider_codex_associated_models () =
  let providers =
    [
      ( "openai-codex",
        {
          Runtime_config.default_provider_config with
          kind = Some "openai-codex";
          base_url = Some "https://chatgpt.com/backend-api/codex";
          default_model = Some "openai-codex/gpt-5.3-codex";
          codex_oauth =
            Some
              {
                Runtime_config.access_token = "access-token";
                refresh_token = "refresh-token";
                expires_at_ms = 1730000000000;
                account_id = None;
                email = None;
              };
        } );
    ]
  in
  let check msg model =
    match Provider.find_provider_for_model ~providers ~model_name:model with
    | Some (name, _) -> Alcotest.(check string) msg "openai-codex" name
    | None -> Alcotest.fail (Printf.sprintf "expected match for %s" model)
  in
  check "gpt-5.2 routes to codex" "gpt-5.2";
  check "gpt-5.3-codex routes to codex" "gpt-5.3-codex";
  check "gpt-5.3-codex-spark routes to codex" "gpt-5.3-codex-spark";
  check "gpt-5.4 routes to codex" "gpt-5.4";
  check "gpt-5 routes to codex" "gpt-5";
  check "gpt-5.1 routes to codex" "gpt-5.1";
  check "gpt-5.1-codex-max routes to codex" "gpt-5.1-codex-max";
  check "gpt-5.2-codex routes to codex" "gpt-5.2-codex";
  check "gpt-5-codex-mini routes to codex" "gpt-5-codex-mini";
  check "gpt-5.4-pro routes to codex" "gpt-5.4-pro";
  check "gpt-5-mini routes to codex" "gpt-5-mini"

let test_find_provider_ignores_empty_codex_oauth_tokens () =
  let providers =
    [
      ( "openai-codex",
        {
          Runtime_config.default_provider_config with
          kind = Some "openai-codex";
          base_url = Some "https://chatgpt.com/backend-api/codex";
          default_model = Some "openai-codex/gpt-5-codex";
          codex_oauth =
            Some
              {
                Runtime_config.access_token = "";
                refresh_token = "";
                expires_at_ms = 0;
                account_id = None;
                email = None;
              };
        } );
    ]
  in
  match
    Provider.find_provider_for_model ~providers
      ~model_name:"openai-codex/gpt-5-codex"
  with
  | Some _ -> Alcotest.fail "did not expect match for empty codex oauth creds"
  | None -> ()

let test_message_to_json_text_only () =
  let msg = Provider.make_message ~role:"user" ~content:"hello" in
  let json = Provider.message_to_json msg in
  let content = Yojson.Safe.Util.(json |> member "content") in
  match content with
  | `String s -> Alcotest.(check string) "text content" "hello" s
  | _ -> Alcotest.fail "expected string content for text-only message"

let test_message_to_json_with_image () =
  let msg =
    Provider.make_message_with_parts ~role:"user" ~content:"describe this"
      ~content_parts:
        [
          Provider.Text "describe this";
          Provider.Image_base64 { data = "abc123"; media_type = "image/jpeg" };
        ]
  in
  let json = Provider.message_to_json msg in
  let content = Yojson.Safe.Util.(json |> member "content") in
  match content with
  | `List parts ->
      Alcotest.(check int) "two parts" 2 (List.length parts);
      let first = List.nth parts 0 in
      let typ = Yojson.Safe.Util.(first |> member "type" |> to_string) in
      Alcotest.(check string) "first part type" "text" typ;
      let second = List.nth parts 1 in
      let typ2 = Yojson.Safe.Util.(second |> member "type" |> to_string) in
      Alcotest.(check string) "second part type" "image_url" typ2;
      let url =
        Yojson.Safe.Util.(
          second |> member "image_url" |> member "url" |> to_string)
      in
      Alcotest.(check string) "data url" "data:image/jpeg;base64,abc123" url
  | _ -> Alcotest.fail "expected list content for multimodal message"

let test_detect_mime_type_jpeg () =
  let data = "\xFF\xD8\xFF\xE0rest" in
  Alcotest.(check string) "jpeg" "image/jpeg" (Telegram.detect_mime_type data)

let test_detect_mime_type_png () =
  let data = "\x89PNG\r\n\x1a\n" in
  Alcotest.(check string) "png" "image/png" (Telegram.detect_mime_type data)

let test_detect_mime_type_webp () =
  let data = "RIFF\x00\x00\x00\x00WEBP" in
  Alcotest.(check string) "webp" "image/webp" (Telegram.detect_mime_type data)

let test_detect_mime_type_gif () =
  let data = "GIF89a" in
  Alcotest.(check string) "gif" "image/gif" (Telegram.detect_mime_type data)

let test_detect_mime_type_fallback () =
  let data = "unknown" in
  Alcotest.(check string)
    "fallback" "image/jpeg"
    (Telegram.detect_mime_type data)

let test_anthropic_content_parts () =
  let msgs =
    [
      Provider.make_message_with_parts ~role:"user" ~content:"look at this"
        ~content_parts:
          [
            Provider.Text "look at this";
            Provider.Image_base64 { data = "imgdata"; media_type = "image/png" };
          ];
    ]
  in
  let json_list = Provider_anthropic.messages_to_anthropic_json msgs in
  match json_list with
  | [ msg_json ] -> (
      let content = Yojson.Safe.Util.(msg_json |> member "content") in
      match content with
      | `List parts ->
          Alcotest.(check int) "two parts" 2 (List.length parts);
          let img = List.nth parts 1 in
          let typ = Yojson.Safe.Util.(img |> member "type" |> to_string) in
          Alcotest.(check string) "image type" "image" typ;
          let src_type =
            Yojson.Safe.Util.(
              img |> member "source" |> member "type" |> to_string)
          in
          Alcotest.(check string) "source type" "base64" src_type
      | _ -> Alcotest.fail "expected list content")
  | _ -> Alcotest.fail "expected single message"

let test_select_provider_bare_model_overrides_default () =
  let config =
    {
      Runtime_config.default with
      providers =
        [
          ( "openai-codex",
            {
              Runtime_config.default_provider_config with
              kind = Some "openai-codex";
              base_url = Some "https://chatgpt.com/backend-api/codex";
              default_model = Some "openai-codex/gpt-5-codex";
              codex_oauth =
                Some
                  {
                    Runtime_config.access_token = "access-token";
                    refresh_token = "refresh-token";
                    expires_at_ms = 1730000000000;
                    account_id = None;
                    email = None;
                  };
            } );
        ];
      agent_defaults =
        { Runtime_config.default.agent_defaults with primary_model = "gpt-5.4" };
    }
  in
  let provider_name, _provider, model = Provider.select_provider ~config () in
  Alcotest.(check string) "routes to codex" "openai-codex" provider_name;
  Alcotest.(check string) "uses user model not default" "gpt-5.4" model

let test_select_provider_quota_fallback_respects_user_model () =
  let config =
    {
      Runtime_config.default with
      providers =
        [
          ( "constrained-provider",
            {
              Runtime_config.default_provider_config with
              api_key = "sk-constrained";
              quota_threshold = Some 0.8;
            } );
          ( "alternative-provider",
            {
              Runtime_config.default_provider_config with
              api_key = "sk-alternative";
              default_model = Some "alternative/default-model";
            } );
        ];
      agent_defaults =
        { Runtime_config.default.agent_defaults with primary_model = "gpt-5.4" };
    }
  in
  let quota_states =
    [
      ( "constrained-provider",
        {
          Provider_quota.provider_name = "constrained-provider";
          state =
            Provider_quota.Known
              {
                session =
                  Some
                    {
                      used_pct = 90.0;
                      resets_at = None;
                      window_duration_s = None;
                    };
                weekly = None;
                monthly = None;
              };
          fetched_at = Unix.gettimeofday ();
        } );
    ]
  in
  let provider_name, _provider, model =
    Provider.select_provider ~config ~quota_states ()
  in
  Alcotest.(check string)
    "routes to alternative" "alternative-provider" provider_name;
  Alcotest.(check string) "uses user model not alt default" "gpt-5.4" model

let test_sanitize_utf8_valid_ascii () =
  Alcotest.(check string)
    "ascii unchanged" "hello world"
    (Provider.sanitize_utf8 "hello world")

let test_sanitize_utf8_valid_multibyte () =
  let s = "\xC3\xA9\xE2\x80\x99\xF0\x9F\x98\x80" in
  Alcotest.(check string)
    "valid multibyte unchanged" s (Provider.sanitize_utf8 s)

let test_sanitize_utf8_lone_continuation () =
  let s = "abc\x9Cdef" in
  let expected = "abc\xEF\xBF\xBDdef" in
  Alcotest.(check string)
    "lone 0x9C replaced" expected (Provider.sanitize_utf8 s)

let test_sanitize_utf8_truncated_sequence () =
  let s = "abc\xC3" in
  let expected = "abc\xEF\xBF\xBD" in
  Alcotest.(check string)
    "truncated 2-byte replaced" expected (Provider.sanitize_utf8 s)

let test_sanitize_utf8_overlong () =
  let s = "\xE0\x80\xAF" in
  (* Each byte replaced individually: 0xE0 rejected as overlong start,
     then 0x80 and 0xAF are lone continuations *)
  let r = "\xEF\xBF\xBD" in
  let expected = r ^ r ^ r in
  Alcotest.(check string)
    "overlong 3-byte rejected" expected (Provider.sanitize_utf8 s)

let test_sanitize_utf8_surrogate () =
  let s = "\xED\xA0\x80" in
  let r = "\xEF\xBF\xBD" in
  let expected = r ^ r ^ r in
  Alcotest.(check string)
    "surrogate half rejected" expected (Provider.sanitize_utf8 s)

let test_sanitize_utf8_empty () =
  Alcotest.(check string) "empty unchanged" "" (Provider.sanitize_utf8 "")

let test_sanitize_utf8_message_to_json () =
  let m = Provider.make_message ~role:"user" ~content:"hello\x9Cworld" in
  let json = Provider.message_to_json m in
  let content = Yojson.Safe.Util.(json |> member "content" |> to_string) in
  Alcotest.(check string)
    "content sanitized in json" "hello\xEF\xBF\xBDworld" content

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
    Alcotest.test_case "context window codex prefix" `Quick
      test_context_window_codex_prefix;
    Alcotest.test_case "context window uses configured override" `Quick
      test_context_window_uses_configured_override;
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
    Alcotest.test_case "detect kind openai codex explicit" `Quick
      test_detect_kind_openai_codex_explicit;
    Alcotest.test_case "detect kind short sk-ant not anthropic" `Quick
      test_detect_kind_anthropic_short_key;
    Alcotest.test_case "detect kind short AIzaS not gemini" `Quick
      test_detect_kind_gemini_short_key;
    Alcotest.test_case "detect kind cohere url" `Quick
      test_detect_kind_cohere_url;
    Alcotest.test_case "detect kind cohere by name" `Quick
      test_detect_kind_cohere_by_name;
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
    Alcotest.test_case "find provider with codex oauth auth" `Quick
      test_find_provider_with_codex_oauth_auth;
    Alcotest.test_case "find provider codex associated models" `Quick
      test_find_provider_codex_associated_models;
    Alcotest.test_case "find provider ignores empty codex oauth tokens" `Quick
      test_find_provider_ignores_empty_codex_oauth_tokens;
    Alcotest.test_case "message_to_json text only" `Quick
      test_message_to_json_text_only;
    Alcotest.test_case "message_to_json with image" `Quick
      test_message_to_json_with_image;
    Alcotest.test_case "detect MIME jpeg" `Quick test_detect_mime_type_jpeg;
    Alcotest.test_case "detect MIME png" `Quick test_detect_mime_type_png;
    Alcotest.test_case "detect MIME webp" `Quick test_detect_mime_type_webp;
    Alcotest.test_case "detect MIME gif" `Quick test_detect_mime_type_gif;
    Alcotest.test_case "detect MIME fallback" `Quick
      test_detect_mime_type_fallback;
    Alcotest.test_case "anthropic content parts" `Quick
      test_anthropic_content_parts;
    Alcotest.test_case "select_provider bare model overrides default" `Quick
      test_select_provider_bare_model_overrides_default;
    Alcotest.test_case "select_provider quota fallback respects user model"
      `Quick test_select_provider_quota_fallback_respects_user_model;
    Alcotest.test_case "sanitize_utf8 valid ascii" `Quick
      test_sanitize_utf8_valid_ascii;
    Alcotest.test_case "sanitize_utf8 valid multibyte" `Quick
      test_sanitize_utf8_valid_multibyte;
    Alcotest.test_case "sanitize_utf8 lone continuation byte" `Quick
      test_sanitize_utf8_lone_continuation;
    Alcotest.test_case "sanitize_utf8 truncated sequence" `Quick
      test_sanitize_utf8_truncated_sequence;
    Alcotest.test_case "sanitize_utf8 overlong encoding" `Quick
      test_sanitize_utf8_overlong;
    Alcotest.test_case "sanitize_utf8 surrogate half" `Quick
      test_sanitize_utf8_surrogate;
    Alcotest.test_case "sanitize_utf8 empty string" `Quick
      test_sanitize_utf8_empty;
    Alcotest.test_case "sanitize_utf8 in message_to_json" `Quick
      test_sanitize_utf8_message_to_json;
  ]
