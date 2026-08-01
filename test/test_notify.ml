let telegram_account token : Runtime_config.telegram_account =
  { bot_token = token; allow_from = [ "*" ]; totp = None }

let telegram_config accounts : Runtime_config.telegram_config =
  { accounts; text_coalesce_ms = 150; default_model = None }

let teams_config : Runtime_config.teams_config =
  {
    app_id = "app-id-123";
    app_secret = "app-secret-123";
    tenant_id = "tenant-id-123";
    webhook_path = "/webhooks/teams";
    service_url = "https://smba.trafficmanager.net/emea/";
    allow_teams = [ "*" ];
    allow_users = [ "*" ];
    default_model = None;
    mention_mode = "entity";
    file_consent_cards = true;
  }

let config ?telegram ?teams () =
  let channels = { Runtime_config.default.channels with telegram; teams } in
  { Runtime_config.default with channels }

let request ?account ?parse_mode channel target message : Cli_notify.request =
  { channel; target; account; parse_mode; message }

let check_error_contains expected = function
  | Ok () -> Alcotest.failf "expected error containing %S, got success" expected
  | Error message ->
      Alcotest.(check bool)
        "actionable error" true
        (Test_helpers.string_contains message expected)

let test_channel_parsing () =
  Alcotest.(check bool)
    "Telegram case-insensitive" true
    (Cli_notify.channel_of_string "TeLeGrAm" = Ok Cli_notify.Telegram);
  match Cli_notify.channel_of_string "discord" with
  | Ok _ -> Alcotest.fail "unsupported connector accepted"
  | Error message ->
      Alcotest.(check bool)
        "supported choices included" true
        (Test_helpers.string_contains message "telegram"
        && Test_helpers.string_contains message "teams")

let test_telegram_main_account_and_parse_mode () =
  let seen = ref None in
  let send_telegram ~bot_token ~chat_id ~text ~parse_mode =
    seen := Some (bot_token, chat_id, text, parse_mode);
    Lwt.return "42"
  in
  let cfg =
    config
      ~telegram:
        (telegram_config
           [
             ("other", telegram_account "other-token");
             ("main", telegram_account "main-token");
           ])
      ()
  in
  let result =
    Lwt_main.run
      (Cli_notify.deliver ~send_telegram ~config:cfg
         (request ~parse_mode:"html" Cli_notify.Telegram "12345"
            "  hello world  "))
  in
  Alcotest.(check (result unit string)) "delivery succeeds" (Ok ()) result;
  match !seen with
  | None -> Alcotest.fail "Telegram sender was not called"
  | Some (bot_token, chat_id, text, parse_mode) ->
      Alcotest.(check string) "configured token" "main-token" bot_token;
      Alcotest.(check string) "target chat" "12345" chat_id;
      Alcotest.(check string) "trimmed message" "hello world" text;
      Alcotest.(check (option string))
        "normalized parse mode" (Some "HTML") parse_mode

let test_telegram_named_account () =
  let seen_token = ref "" in
  let send_telegram ~bot_token ~chat_id:_ ~text:_ ~parse_mode:_ =
    seen_token := bot_token;
    Lwt.return "7"
  in
  let cfg =
    config
      ~telegram:
        (telegram_config
           [
             ("work", telegram_account "work-token");
             ("home", telegram_account "home-token");
           ])
      ()
  in
  let result =
    Lwt_main.run
      (Cli_notify.deliver ~send_telegram ~config:cfg
         (request ~account:"home" Cli_notify.Telegram "99" "ping"))
  in
  Alcotest.(check (result unit string)) "delivery succeeds" (Ok ()) result;
  Alcotest.(check string) "selected account token" "home-token" !seen_token

let test_telegram_requires_account_when_ambiguous () =
  let cfg =
    config
      ~telegram:
        (telegram_config
           [
             ("work", telegram_account "work-token");
             ("home", telegram_account "home-token");
           ])
      ()
  in
  Lwt_main.run
    (Cli_notify.deliver ~config:cfg (request Cli_notify.Telegram "99" "ping"))
  |> check_error_contains "Specify --account"

let test_telegram_send_failure_is_error () =
  let send_telegram ~bot_token:_ ~chat_id:_ ~text:_ ~parse_mode:_ =
    Lwt.return "0"
  in
  let cfg =
    config
      ~telegram:(telegram_config [ ("main", telegram_account "bot-token") ])
      ()
  in
  Lwt_main.run
    (Cli_notify.deliver ~send_telegram ~config:cfg
       (request Cli_notify.Telegram "99" "ping"))
  |> check_error_contains "no message ID"

let test_teams_delivery_reuses_config () =
  let seen = ref None in
  let send_teams ~config ~channel_id ~text =
    seen := Some (config.Runtime_config.app_id, channel_id, text);
    Lwt.return (Ok "activity-id")
  in
  let cfg = config ~teams:teams_config () in
  let result =
    Lwt_main.run
      (Cli_notify.deliver ~send_teams ~config:cfg
         (request ~parse_mode:"markdown" Cli_notify.Teams "conversation"
            "release complete"))
  in
  Alcotest.(check (result unit string)) "delivery succeeds" (Ok ()) result;
  Alcotest.(check (option (triple string string string)))
    "existing Teams sender receives configured credentials and target"
    (Some
       ( "app-id-123",
         "https://smba.trafficmanager.net/emea/|conversation",
         "release complete" ))
    !seen

let test_teams_service_url_override_is_rejected () =
  let sender_called = ref false in
  let send_teams ~config:_ ~channel_id:_ ~text:_ =
    sender_called := true;
    Lwt.return (Ok "activity-id")
  in
  let cfg = config ~teams:teams_config () in
  Lwt_main.run
    (Cli_notify.deliver ~send_teams ~config:cfg
       (request Cli_notify.Teams "https://attacker.example/|conversation" "ping"))
  |> check_error_contains "conversation ID only";
  Alcotest.(check bool) "sender not called" false !sender_called

let test_teams_encoded_target_is_rejected () =
  let cfg = config ~teams:teams_config () in
  Lwt_main.run
    (Cli_notify.deliver ~config:cfg
       (request Cli_notify.Teams "arbitrary-service|conversation" "ping"))
  |> check_error_contains "channels.teams.service_url"

let test_teams_url_without_separator_is_rejected () =
  let sender_called = ref false in
  let send_teams ~config:_ ~channel_id:_ ~text:_ =
    sender_called := true;
    Lwt.return (Ok "activity-id")
  in
  let cfg = config ~teams:teams_config () in
  Lwt_main.run
    (Cli_notify.deliver ~send_teams ~config:cfg
       (request Cli_notify.Teams "https://attacker.example/conversation" "ping"))
  |> check_error_contains "conversation ID only";
  Alcotest.(check bool) "sender not called" false !sender_called

let test_teams_checked_delivery_reports_partial_failure () =
  let requests = ref 0 in
  let fetch_token_fn ~config:_ = Lwt.return (Some "oauth-token-secret") in
  let post_json_fn ~conversation_id:_ ~uri:_ ~headers:_ ~body:_ =
    incr requests;
    if !requests = 1 then Lwt.return (500, "response-body-secret")
    else Lwt.return (201, {|{"id":"later-activity-id"}|})
  in
  let text = String.make (Teams_api.max_message_chars + 1) 'x' in
  let result =
    Lwt_main.run
      (Teams_api.send_message_checked ~fetch_token_fn ~post_json_fn
         ~config:teams_config
         ~channel_id:(teams_config.service_url ^ "|conversation")
         ~text ())
  in
  Alcotest.(check int) "all chunks attempted" 2 !requests;
  match result with
  | Ok activity_id ->
      Alcotest.failf "partial delivery reported success with ID %S" activity_id
  | Error message ->
      Alcotest.(check bool)
        "early failed chunk identified" true
        (Test_helpers.string_contains message "chunk 1 of 2"
        && Test_helpers.string_contains message "HTTP 500");
      Alcotest.(check bool)
        "response body is not exposed" false
        (Test_helpers.string_contains message "response-body-secret");
      Alcotest.(check bool)
        "OAuth token is not exposed" false
        (Test_helpers.string_contains message "oauth-token-secret")

let test_teams_rejects_non_markdown_parse_mode () =
  let cfg = config ~teams:teams_config () in
  Lwt_main.run
    (Cli_notify.deliver ~config:cfg
       (request ~parse_mode:"HTML" Cli_notify.Teams "conversation" "ping"))
  |> check_error_contains "Markdown only"

let test_missing_connector_is_actionable () =
  Lwt_main.run
    (Cli_notify.deliver ~config:(config ())
       (request Cli_notify.Telegram "99" "ping"))
  |> check_error_contains "Telegram is not configured"

let suite =
  [
    Alcotest.test_case "channel parsing" `Quick test_channel_parsing;
    Alcotest.test_case "Telegram main account and parse mode" `Quick
      test_telegram_main_account_and_parse_mode;
    Alcotest.test_case "Telegram named account" `Quick
      test_telegram_named_account;
    Alcotest.test_case "Telegram ambiguous account" `Quick
      test_telegram_requires_account_when_ambiguous;
    Alcotest.test_case "Telegram send failure" `Quick
      test_telegram_send_failure_is_error;
    Alcotest.test_case "Teams delivery reuses config" `Quick
      test_teams_delivery_reuses_config;
    Alcotest.test_case "Teams service URL override rejected" `Quick
      test_teams_service_url_override_is_rejected;
    Alcotest.test_case "Teams encoded target rejected" `Quick
      test_teams_encoded_target_is_rejected;
    Alcotest.test_case "Teams URL target rejected" `Quick
      test_teams_url_without_separator_is_rejected;
    Alcotest.test_case "Teams partial delivery fails" `Quick
      test_teams_checked_delivery_reports_partial_failure;
    Alcotest.test_case "Teams parse mode validation" `Quick
      test_teams_rejects_non_markdown_parse_mode;
    Alcotest.test_case "missing connector" `Quick
      test_missing_connector_is_actionable;
  ]
