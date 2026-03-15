let dest_of_uri_tests () =
  let check desc uri expected =
    let actual = Http_debug.dest_of_uri uri in
    Alcotest.(check string) desc expected actual
  in
  check "openai api" "https://api.openai.com/v1/chat" "openai";
  check "openai subdomain" "https://foo.openai.com/bar" "openai";
  check "anthropic" "https://api.anthropic.com/v1/messages" "anthropic";
  check "telegram" "https://api.telegram.org/bot123/send" "telegram";
  check "discord" "https://discord.com/api/v10/channels" "discord";
  check "discord subdomain" "https://gateway.discord.com/ws" "discord";
  check "slack" "https://slack.com/api/chat.postMessage" "slack";
  check "slack subdomain" "https://api.slack.com/events" "slack";
  check "teams" "https://graph.microsoft.com/v1.0/teams" "teams";
  check "groq" "https://api.groq.com/v1/completions" "groq";
  check "google" "https://generativelanguage.googleapis.com/v1" "google";
  check "localhost" "http://localhost:8080/health" "localhost";
  check "127.0.0.1" "http://127.0.0.1:3000/api" "localhost";
  check "unknown host" "https://example.com/api" "example.com";
  check "no host" "/relative/path" "unknown"

let header_redaction_tests () =
  let headers =
    [
      ("Authorization", "Bearer sk-abc123secret");
      ("Content-Type", "application/json");
      ("X-Api-Key", "key-mysecret");
      ("User-Agent", "clawq/0.1");
      ("Cookie", "session=s3cret_val");
    ]
  in
  let redacted = Http_debug.redact_headers headers in
  (* Authorization should be redacted *)
  let auth_val = List.assoc "Authorization" redacted in
  Alcotest.(check bool)
    "auth redacted" true
    (auth_val <> "Bearer sk-abc123secret");
  Alcotest.(check bool)
    "auth uses redact_token" true
    (String.length auth_val > 0 && String.contains auth_val '.');
  (* Content-Type should be preserved *)
  let ct = List.assoc "Content-Type" redacted in
  Alcotest.(check string) "content-type preserved" "application/json" ct;
  (* X-Api-Key should be redacted *)
  let api_key = List.assoc "X-Api-Key" redacted in
  Alcotest.(check bool) "api-key redacted" true (api_key <> "key-mysecret");
  (* User-Agent preserved *)
  let ua = List.assoc "User-Agent" redacted in
  Alcotest.(check string) "user-agent preserved" "clawq/0.1" ua

let har_format_tests () =
  Test_helpers.with_temp_home (fun _dir ->
      Http_debug.enabled_ref := true;
      Http_debug.log_roundtrip ~method_:"GET"
        ~uri:"https://api.openai.com/v1/models" ~label:"test"
        ~req_headers:[ ("Authorization", "Bearer sk-test") ]
        ~req_body:"" ~status:200
        ~resp_headers:[ ("Content-Type", "application/json") ]
        ~resp_body:{|{"data":[]}|} ~started:(Unix.gettimeofday ());
      (* Find the HAR file *)
      let debug_dir = Http_debug.debug_dir () in
      let today = Http_debug.today_dir () in
      Alcotest.(check bool) "debug dir exists" true (Sys.file_exists debug_dir);
      Alcotest.(check bool) "today dir exists" true (Sys.file_exists today);
      let files = Sys.readdir today in
      let har_files =
        Array.to_list files
        |> List.filter (fun f -> Filename.check_suffix f ".har")
      in
      Alcotest.(check bool) "at least one HAR" true (List.length har_files > 0);
      (* Parse the HAR file *)
      let har_path = Filename.concat today (List.hd har_files) in
      let ic = open_in har_path in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      let json = Yojson.Safe.from_string content in
      let open Yojson.Safe.Util in
      let log = json |> member "log" in
      let version = log |> member "version" |> to_string in
      Alcotest.(check string) "HAR version" "1.2" version;
      let creator_name =
        log |> member "creator" |> member "name" |> to_string
      in
      Alcotest.(check string) "creator" "clawq" creator_name;
      let entries = log |> member "entries" |> to_list in
      Alcotest.(check int) "one entry" 1 (List.length entries);
      let entry = List.hd entries in
      let req = entry |> member "request" in
      let method_ = req |> member "method" |> to_string in
      Alcotest.(check string) "method" "GET" method_;
      let resp = entry |> member "response" in
      let status = resp |> member "status" |> to_int in
      Alcotest.(check int) "status" 200 status;
      (* Verify auth header is redacted in HAR *)
      let req_headers = req |> member "headers" |> to_list in
      let auth_header =
        List.find
          (fun h -> h |> member "name" |> to_string = "Authorization")
          req_headers
      in
      let auth_value = auth_header |> member "value" |> to_string in
      Alcotest.(check bool)
        "auth redacted in HAR" true
        (auth_value <> "Bearer sk-test");
      Http_debug.enabled_ref := false)

let full_body_preserved_tests () =
  Test_helpers.with_temp_home (fun _dir ->
      Http_debug.enabled_ref := true;
      let large_body = String.make 100_000 'x' in
      Http_debug.log_roundtrip ~method_:"POST"
        ~uri:"https://api.openai.com/v1/chat" ~label:"test"
        ~req_headers:[ ("Content-Type", "application/json") ]
        ~req_body:large_body ~status:200
        ~resp_headers:[ ("Content-Type", "application/json") ]
        ~resp_body:large_body ~started:(Unix.gettimeofday ());
      let today = Http_debug.today_dir () in
      let files = Sys.readdir today in
      let har_files =
        Array.to_list files
        |> List.filter (fun f -> Filename.check_suffix f ".har")
      in
      let har_path = Filename.concat today (List.hd har_files) in
      let ic = open_in har_path in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      (* Body should be fully preserved *)
      Alcotest.(check bool)
        "full body preserved" true
        (String.length content > 200_000);
      Http_debug.enabled_ref := false)

let sidecar_meta_tests () =
  Test_helpers.with_temp_home (fun _dir ->
      Http_debug.enabled_ref := true;
      Http_debug.log_roundtrip ~method_:"GET"
        ~uri:"https://api.telegram.org/bot/getMe" ~label:"telegram_test"
        ~req_headers:[] ~req_body:"" ~status:200 ~resp_headers:[]
        ~resp_body:"{}" ~started:(Unix.gettimeofday ());
      let today = Http_debug.today_dir () in
      let files = Sys.readdir today in
      let meta_files =
        Array.to_list files
        |> List.filter (fun f -> Filename.check_suffix f ".meta.json")
      in
      Alcotest.(check bool) "meta file exists" true (List.length meta_files > 0);
      let meta_path = Filename.concat today (List.hd meta_files) in
      let ic = open_in meta_path in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      let json = Yojson.Safe.from_string content in
      let open Yojson.Safe.Util in
      let label = json |> member "label" |> to_string in
      Alcotest.(check string) "label in meta" "telegram_test" label;
      Http_debug.enabled_ref := false)

let disabled_no_files_tests () =
  Test_helpers.with_temp_home (fun _dir ->
      Http_debug.enabled_ref := false;
      Http_debug.log_roundtrip ~method_:"GET"
        ~uri:"https://api.openai.com/v1/models" ~label:"test" ~req_headers:[]
        ~req_body:"" ~status:200 ~resp_headers:[] ~resp_body:"{}"
        ~started:(Unix.gettimeofday ());
      let debug_dir = Http_debug.debug_dir () in
      Alcotest.(check bool)
        "no debug dir when disabled" false
        (Sys.file_exists debug_dir))

let sync_config_tests () =
  Test_helpers.with_temp_home (fun _dir ->
      let old_env = Sys.getenv_opt "CLAWQ_DEBUG_HTTP" in
      (try Unix.putenv "CLAWQ_DEBUG_HTTP" "" with _ -> ());
      Http_debug.enabled_ref := false;
      Http_debug.sync_config
        { Runtime_config.max_size_mb = 10; max_files = 5; debug_http = true };
      Alcotest.(check bool) "sync_config enables" true (Http_debug.enabled ());
      Http_debug.sync_config
        { Runtime_config.max_size_mb = 10; max_files = 5; debug_http = false };
      Alcotest.(check bool) "sync_config disables" false (Http_debug.enabled ());
      match old_env with
      | Some v -> Unix.putenv "CLAWQ_DEBUG_HTTP" v
      | None -> ( try Unix.putenv "CLAWQ_DEBUG_HTTP" "" with _ -> ()))

let env_var_override_tests () =
  Test_helpers.with_temp_home (fun _dir ->
      let old_env = Sys.getenv_opt "CLAWQ_DEBUG_HTTP" in
      Unix.putenv "CLAWQ_DEBUG_HTTP" "1";
      Http_debug.sync_config
        { Runtime_config.max_size_mb = 10; max_files = 5; debug_http = false };
      Alcotest.(check bool)
        "env var overrides config" true (Http_debug.enabled ());
      match old_env with
      | Some v -> Unix.putenv "CLAWQ_DEBUG_HTTP" v
      | None -> Unix.putenv "CLAWQ_DEBUG_HTTP" "")

let date_based_dir_tests () =
  Test_helpers.with_temp_home (fun _dir ->
      Http_debug.enabled_ref := true;
      Http_debug.log_roundtrip ~method_:"GET" ~uri:"https://example.com/test"
        ~label:"test" ~req_headers:[] ~req_body:"" ~status:200 ~resp_headers:[]
        ~resp_body:"{}" ~started:(Unix.gettimeofday ());
      let today = Http_debug.today_str () in
      let today_dir = Http_debug.today_dir () in
      Alcotest.(check bool) "date-formatted dir" true (String.length today = 10);
      Alcotest.(check bool) "today dir exists" true (Sys.file_exists today_dir);
      (* dir name should match YYYY-MM-DD pattern *)
      Alcotest.(check bool)
        "date format" true
        (today.[4] = '-' && today.[7] = '-');
      Http_debug.enabled_ref := false)

let status_info_tests () =
  Test_helpers.with_temp_home (fun _dir ->
      Http_debug.enabled_ref := false;
      let info = Http_debug.status_info () in
      Alcotest.(check bool)
        "status contains enabled" true
        (String_util.contains info "enabled");
      Alcotest.(check bool)
        "status contains log dir" true
        (String_util.contains info "log dir"))

let clear_logs_tests () =
  Test_helpers.with_temp_home (fun _dir ->
      Http_debug.enabled_ref := true;
      Http_debug.log_roundtrip ~method_:"GET" ~uri:"https://example.com/test"
        ~label:"test" ~req_headers:[] ~req_body:"" ~status:200 ~resp_headers:[]
        ~resp_body:"{}" ~started:(Unix.gettimeofday ());
      let debug_dir = Http_debug.debug_dir () in
      Alcotest.(check bool)
        "dir exists before clear" true
        (Sys.file_exists debug_dir);
      let result = Http_debug.clear_logs () in
      Alcotest.(check bool)
        "clear returns message" true
        (String_util.contains result "cleared");
      (* Dir should still exist but be empty *)
      let entries = Sys.readdir debug_dir in
      Alcotest.(check int) "dir empty after clear" 0 (Array.length entries);
      Http_debug.enabled_ref := false)

let tail_logs_tests () =
  Test_helpers.with_temp_home (fun _dir ->
      Http_debug.enabled_ref := true;
      Http_debug.log_roundtrip ~method_:"GET"
        ~uri:"https://api.openai.com/v1/models" ~label:"test" ~req_headers:[]
        ~req_body:"" ~status:200 ~resp_headers:[] ~resp_body:"{}"
        ~started:(Unix.gettimeofday ());
      let result = Http_debug.tail_logs 5 in
      Alcotest.(check bool) "tail shows entries" true (String.length result > 0);
      Alcotest.(check bool)
        "tail contains method" true
        (String_util.contains result "GET");
      Alcotest.(check bool)
        "tail contains status" true
        (String_util.contains result "200");
      Http_debug.enabled_ref := false)

let suite =
  [
    Alcotest.test_case "dest_of_uri mapping" `Quick dest_of_uri_tests;
    Alcotest.test_case "header redaction" `Quick header_redaction_tests;
    Alcotest.test_case "HAR format and structure" `Quick har_format_tests;
    Alcotest.test_case "full bodies preserved" `Quick full_body_preserved_tests;
    Alcotest.test_case "sidecar metadata" `Quick sidecar_meta_tests;
    Alcotest.test_case "disabled produces no files" `Quick
      disabled_no_files_tests;
    Alcotest.test_case "sync_config toggles" `Quick sync_config_tests;
    Alcotest.test_case "env var override" `Quick env_var_override_tests;
    Alcotest.test_case "date-based directories" `Quick date_based_dir_tests;
    Alcotest.test_case "status info" `Quick status_info_tests;
    Alcotest.test_case "clear logs" `Quick clear_logs_tests;
    Alcotest.test_case "tail logs" `Quick tail_logs_tests;
  ]
