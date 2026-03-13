(* Tests for channel format parsing and formatting *)

(* Helper to check string contains substring *)
let contains_str haystack needle =
  let hlen = String.length haystack and nlen = String.length needle in
  if nlen = 0 then true
  else if nlen > hlen then false
  else
    let found = ref false in
    for i = 0 to hlen - nlen do
      if String.sub haystack i nlen = needle then found := true
    done;
    !found

(* ===== Slack tests ===== *)

let test_slack_parse_url_verification () =
  let body =
    {|{"type": "url_verification", "challenge": "abc123", "token": "tok"}|}
  in
  match Slack.parse_event body with
  | Some (Slack.UrlVerification challenge) ->
      Alcotest.(check string) "challenge" "abc123" challenge
  | Some _ -> Alcotest.fail "expected UrlVerification"
  | None -> Alcotest.fail "expected Some"

let test_slack_parse_message_event () =
  let body =
    {|{
      "type": "event_callback",
      "event": {
        "type": "message",
        "channel": "C123",
        "user": "U456",
        "text": "hello there"
      }
    }|}
  in
  match Slack.parse_event body with
  | Some (Slack.Message { channel_id; user_id; text; bot_id }) ->
      Alcotest.(check string) "channel_id" "C123" channel_id;
      Alcotest.(check string) "user_id" "U456" user_id;
      Alcotest.(check string) "text" "hello there" text;
      Alcotest.(check bool) "no bot_id" true (bot_id = None)
  | Some _ -> Alcotest.fail "expected Message"
  | None -> Alcotest.fail "expected Some"

let test_slack_parse_bot_message () =
  let body =
    {|{
      "type": "event_callback",
      "event": {
        "type": "message",
        "channel": "C123",
        "user": "U456",
        "text": "bot says hi",
        "bot_id": "B789"
      }
    }|}
  in
  match Slack.parse_event body with
  | Some (Slack.Message { bot_id = Some bid; _ }) ->
      Alcotest.(check string) "bot_id" "B789" bid
  | _ -> Alcotest.fail "expected Message with bot_id"

let test_slack_parse_invalid_json () =
  match Slack.parse_event "{ bad json" with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for invalid JSON"

let test_slack_parse_other_event () =
  let body =
    {|{"type": "event_callback", "event": {"type": "reaction_added"}}|}
  in
  match Slack.parse_event body with
  | Some Slack.Other -> ()
  | _ -> Alcotest.fail "expected Other"

let test_slack_parse_unknown_type () =
  let body = {|{"type": "unknown_type"}|} in
  match Slack.parse_event body with
  | Some Slack.Other -> ()
  | _ -> Alcotest.fail "expected Other for unknown type"

let test_slack_is_allowed_wildcard_channel () =
  let cfg : Runtime_config.slack_config =
    {
      bot_token = "xoxb-test";
      signing_secret = "secret";
      events_path = "/slack/events";
      allow_channels = [ "*" ];
      allow_users = [ "*" ];
      app_token = "";
      socket_mode = false;
    }
  in
  Alcotest.(check bool)
    "wildcard allows any channel" true
    (Slack.is_allowed ~config:cfg ~channel_id:"C123" ~user_id:"U456")

let test_slack_is_allowed_specific_channel_match () =
  let cfg : Runtime_config.slack_config =
    {
      bot_token = "xoxb-test";
      signing_secret = "secret";
      events_path = "/slack/events";
      allow_channels = [ "C123"; "C456" ];
      allow_users = [ "*" ];
      app_token = "";
      socket_mode = false;
    }
  in
  Alcotest.(check bool)
    "specific channel match" true
    (Slack.is_allowed ~config:cfg ~channel_id:"C123" ~user_id:"U789")

let test_slack_is_allowed_specific_channel_no_match () =
  let cfg : Runtime_config.slack_config =
    {
      bot_token = "xoxb-test";
      signing_secret = "secret";
      events_path = "/slack/events";
      allow_channels = [ "C123" ];
      allow_users = [ "*" ];
      app_token = "";
      socket_mode = false;
    }
  in
  Alcotest.(check bool)
    "channel not in list" false
    (Slack.is_allowed ~config:cfg ~channel_id:"COTHER" ~user_id:"U789")

let test_slack_is_allowed_user_filter () =
  let cfg : Runtime_config.slack_config =
    {
      bot_token = "xoxb-test";
      signing_secret = "secret";
      events_path = "/slack/events";
      allow_channels = [ "*" ];
      allow_users = [ "U123" ];
      app_token = "";
      socket_mode = false;
    }
  in
  Alcotest.(check bool)
    "allowed user" true
    (Slack.is_allowed ~config:cfg ~channel_id:"CANY" ~user_id:"U123");
  Alcotest.(check bool)
    "denied user" false
    (Slack.is_allowed ~config:cfg ~channel_id:"CANY" ~user_id:"UOTHER")

let test_slack_verify_signature_bad_timestamp () =
  (* Timestamp far in the past *)
  let ts = string_of_float (Unix.gettimeofday () -. 400.0) in
  let result =
    Slack.verify_signature ~signing_secret:"secret" ~timestamp:ts ~body:"test"
      ~signature:"v0=abc"
  in
  Alcotest.(check bool) "old timestamp rejected" false result

let test_slack_verify_signature_bad_sig () =
  let ts = string_of_float (Unix.gettimeofday ()) in
  let result =
    Slack.verify_signature ~signing_secret:"secret" ~timestamp:ts ~body:"test"
      ~signature:"v0=bad_signature"
  in
  Alcotest.(check bool) "wrong sig rejected" false result

let test_slack_parse_message_missing_text () =
  let body =
    {|{
      "type": "event_callback",
      "event": {
        "type": "message",
        "channel": "C123",
        "user": "U456"
      }
    }|}
  in
  (* Missing text should still parse, defaulting to empty string *)
  match Slack.parse_event body with
  | Some (Slack.Message { text; _ }) ->
      Alcotest.(check string) "empty text default" "" text
  | _ -> Alcotest.fail "expected Message even without text"

let test_slack_parse_missing_user () =
  let body =
    {|{
      "type": "event_callback",
      "event": {
        "type": "message",
        "channel": "C123",
        "text": "hi"
      }
    }|}
  in
  (* Missing user should default to empty *)
  match Slack.parse_event body with
  | Some (Slack.Message { user_id; _ }) ->
      Alcotest.(check string) "empty user default" "" user_id
  | _ -> Alcotest.fail "expected Message"

(* ===== Discord tests ===== *)

let test_discord_chunk_text_short () =
  let chunks = Discord.chunk_text "hello" in
  Alcotest.(check int) "1 chunk for short text" 1 (List.length chunks);
  Alcotest.(check string) "text unchanged" "hello" (List.hd chunks)

let test_discord_chunk_text_exact_limit () =
  let text = String.make 2000 'a' in
  let chunks = Discord.chunk_text text in
  Alcotest.(check int) "exactly 1 chunk at limit" 1 (List.length chunks)

let test_discord_chunk_text_over_limit () =
  let text = String.make 4001 'x' in
  let chunks = Discord.chunk_text text in
  Alcotest.(check bool) "multiple chunks" true (List.length chunks >= 2)

let test_discord_chunk_text_custom_max_len () =
  let text = String.make 100 'z' in
  let chunks = Discord.chunk_text ~max_len:30 text in
  Alcotest.(check bool) "chunks with custom max" true (List.length chunks >= 3)

let test_discord_is_allowed_wildcard () =
  let cfg : Runtime_config.discord_config =
    {
      bot_token = "bot-token";
      allow_guilds = [ "*" ];
      allow_users = [ "*" ];
      intents = 0;
    }
  in
  Alcotest.(check bool)
    "wildcard allows all" true
    (Discord.is_allowed ~config:cfg ~guild_id:(Some "G123") ~user_id:"U456")

let test_discord_is_allowed_specific_guild () =
  let cfg : Runtime_config.discord_config =
    {
      bot_token = "bot-token";
      allow_guilds = [ "G123" ];
      allow_users = [ "*" ];
      intents = 0;
    }
  in
  Alcotest.(check bool)
    "matching guild" true
    (Discord.is_allowed ~config:cfg ~guild_id:(Some "G123") ~user_id:"U456");
  Alcotest.(check bool)
    "non-matching guild" false
    (Discord.is_allowed ~config:cfg ~guild_id:(Some "GOTHER") ~user_id:"U456")

let test_discord_is_allowed_no_guild () =
  let cfg : Runtime_config.discord_config =
    {
      bot_token = "bot-token";
      allow_guilds = [ "G123" ];
      allow_users = [ "*" ];
      intents = 0;
    }
  in
  (* None guild_id with non-wildcard list should fail *)
  Alcotest.(check bool)
    "None guild rejected" false
    (Discord.is_allowed ~config:cfg ~guild_id:None ~user_id:"U456")

let test_discord_is_allowed_user_filter () =
  let cfg : Runtime_config.discord_config =
    {
      bot_token = "bot-token";
      allow_guilds = [ "*" ];
      allow_users = [ "U123" ];
      intents = 0;
    }
  in
  Alcotest.(check bool)
    "allowed user" true
    (Discord.is_allowed ~config:cfg ~guild_id:(Some "G1") ~user_id:"U123");
  Alcotest.(check bool)
    "denied user" false
    (Discord.is_allowed ~config:cfg ~guild_id:(Some "G1") ~user_id:"UOTHER")

let test_discord_session_key_format () =
  let key = Discord.session_key ~channel_id:"C1" ~author_id:"A2" in
  Alcotest.(check bool) "has discord prefix" true (contains_str key "discord:");
  Alcotest.(check bool) "contains channel_id" true (contains_str key "C1");
  Alcotest.(check bool) "contains author_id" true (contains_str key "A2")

let test_discord_chunk_preserves_content () =
  let original = "Hello from Discord! " ^ String.make 1980 'x' ^ " end" in
  let chunks = Discord.chunk_text original in
  let reconstructed = String.concat "" chunks in
  Alcotest.(check string) "content preserved" original reconstructed

let test_discord_chunk_empty_text () =
  let chunks = Discord.chunk_text "" in
  Alcotest.(check int) "1 chunk for empty" 1 (List.length chunks);
  Alcotest.(check string) "empty string chunk" "" (List.hd chunks)

let test_discord_is_allowed_wildcard_guild_with_no_guild_id () =
  let cfg : Runtime_config.discord_config =
    {
      bot_token = "bot-token";
      allow_guilds = [ "*" ];
      allow_users = [ "*" ];
      intents = 0;
    }
  in
  (* Wildcard should allow even None guild *)
  Alcotest.(check bool)
    "wildcard allows None guild" true
    (Discord.is_allowed ~config:cfg ~guild_id:None ~user_id:"U456")

(* ===== Telegram tests ===== *)

let test_telegram_redact_short_token () =
  (* Short token - all stars *)
  let r = Telegram.redact_token "abc" in
  Alcotest.(check string) "short token redacted" "***" r;
  let r8 = Telegram.redact_token "12345678" in
  Alcotest.(check string) "8 char redacted" "********" r8

let test_telegram_redact_long_token () =
  let r = Telegram.redact_token "1234567890abcdef" in
  Alcotest.(check bool)
    "long token partially redacted" true (contains_str r "...")

let test_telegram_redact_preserves_first_four () =
  let r = Telegram.redact_token "1234567890abcdef" in
  Alcotest.(check bool)
    "starts with first 4 chars" true
    (String.length r > 4 && String.sub r 0 4 = "1234")

let test_telegram_chunk_text_short () =
  (* Telegram doesn't export chunk_text directly but
     we can test that message parsing handles basic structure *)
  (* Instead, test that the api_base is set correctly *)
  Alcotest.(check bool)
    "api_base contains telegram.org" true
    (contains_str !Telegram.api_base "telegram.org")

let test_telegram_parse_update_structure () =
  (* Test the JSON structure Telegram expects *)
  let json_str =
    {|{"update_id": 42, "message": {"chat": {"id": 123}, "from": {"id": 456}, "text": "hello"}}|}
  in
  let json = Yojson.Safe.from_string json_str in
  let open Yojson.Safe.Util in
  let update_id = json |> member "update_id" |> to_int in
  Alcotest.(check int) "update_id" 42 update_id;
  let chat_id =
    json |> member "message" |> member "chat" |> member "id" |> to_int
  in
  Alcotest.(check int) "chat_id" 123 chat_id

let test_telegram_parse_voice_message () =
  let json_str =
    {|{"update_id": 1, "message": {"chat": {"id": 123}, "from": {"id": 456}, "voice": {"file_id": "voice123"}}}|}
  in
  let json = Yojson.Safe.from_string json_str in
  let open Yojson.Safe.Util in
  let file_id =
    json |> member "message" |> member "voice" |> member "file_id" |> to_string
  in
  Alcotest.(check string) "voice file_id" "voice123" file_id

let test_telegram_parse_photo_message () =
  let json_str =
    {|{"update_id": 1, "message": {"chat": {"id": 123}, "from": {"id": 456}, "photo": [{"file_id": "small"}, {"file_id": "large"}]}}|}
  in
  let json = Yojson.Safe.from_string json_str in
  let open Yojson.Safe.Util in
  let photos =
    json |> member "message" |> member "photo" |> to_list
    |> List.map (fun p -> p |> member "file_id" |> to_string)
  in
  Alcotest.(check int) "2 photos" 2 (List.length photos);
  Alcotest.(check string) "last photo" "large" (List.nth photos 1)

let test_telegram_allow_from_wildcard () =
  let acct : Runtime_config.telegram_account =
    { bot_token = "tok"; allow_from = [ "*" ]; totp = None }
  in
  Alcotest.(check bool)
    "wildcard allow_from" true
    (Telegram.is_allowed ~account:acct ~chat_id:"anyone")

let test_telegram_allow_from_specific () =
  let acct : Runtime_config.telegram_account =
    { bot_token = "tok"; allow_from = [ "123"; "456" ]; totp = None }
  in
  Alcotest.(check bool)
    "user 123 allowed" true
    (Telegram.is_allowed ~account:acct ~chat_id:"123");
  Alcotest.(check bool)
    "user 789 not allowed" false
    (Telegram.is_allowed ~account:acct ~chat_id:"789")

let test_telegram_parse_document_message () =
  let json_str =
    {|{"update_id": 1, "message": {"chat": {"id": 123}, "from": {"id": 456}, "document": {"file_id": "doc123", "file_name": "test.pdf"}}}|}
  in
  let json = Yojson.Safe.from_string json_str in
  let open Yojson.Safe.Util in
  let file_id =
    json |> member "message" |> member "document" |> member "file_id"
    |> to_string
  in
  Alcotest.(check string) "document file_id" "doc123" file_id

let test_telegram_empty_text_default () =
  let json_str =
    {|{"update_id": 1, "message": {"chat": {"id": 123}, "from": {"id": 456}}}|}
  in
  let json = Yojson.Safe.from_string json_str in
  let open Yojson.Safe.Util in
  let text =
    try json |> member "message" |> member "text" |> to_string with _ -> ""
  in
  Alcotest.(check string) "empty text" "" text

let test_telegram_redact_eight_char_token () =
  let r = Telegram.redact_token "12345678" in
  Alcotest.(check string) "8 char redacted" "********" r

(* ===== Slack additional tests ===== *)

let test_slack_verify_valid_signature () =
  (* Compute a valid signature and verify it *)
  let secret = "test-signing-secret" in
  let ts = string_of_float (Unix.gettimeofday ()) in
  let body = "v=hello&body=test" in
  let basestring = "v0:" ^ ts ^ ":" ^ body in
  let expected =
    "v0=" ^ Digestif.SHA256.(hmac_string ~key:secret basestring |> to_hex)
  in
  let result =
    Slack.verify_signature ~signing_secret:secret ~timestamp:ts ~body
      ~signature:expected
  in
  Alcotest.(check bool) "valid signature accepted" true result

let test_slack_verify_signature_small_future_skew () =
  let secret = "test-signing-secret" in
  let ts = string_of_float (Unix.gettimeofday () +. 120.0) in
  let body = "future=small" in
  let basestring = "v0:" ^ ts ^ ":" ^ body in
  let expected =
    "v0=" ^ Digestif.SHA256.(hmac_string ~key:secret basestring |> to_hex)
  in
  let result =
    Slack.verify_signature ~signing_secret:secret ~timestamp:ts ~body
      ~signature:expected
  in
  Alcotest.(check bool) "small future skew accepted" true result

let test_slack_verify_signature_far_future_skew () =
  let secret = "test-signing-secret" in
  let ts = string_of_float (Unix.gettimeofday () +. 600.0) in
  let body = "future=far" in
  let basestring = "v0:" ^ ts ^ ":" ^ body in
  let expected =
    "v0=" ^ Digestif.SHA256.(hmac_string ~key:secret basestring |> to_hex)
  in
  let result =
    Slack.verify_signature ~signing_secret:secret ~timestamp:ts ~body
      ~signature:expected
  in
  Alcotest.(check bool) "far future skew rejected" false result

let test_slack_parse_empty_body () =
  match Slack.parse_event "" with
  | None -> ()
  | Some _ -> Alcotest.fail "empty body should parse to None"

let test_slack_parse_empty_json () =
  match Slack.parse_event "{}" with
  | None -> () (* missing required fields -> exception -> None *)
  | Some Slack.Other -> ()
  | Some _ -> Alcotest.fail "empty json should parse to None or Other"

let test_slack_is_allowed_both_wildcard () =
  let cfg : Runtime_config.slack_config =
    {
      bot_token = "xoxb";
      signing_secret = "";
      events_path = "/e";
      allow_channels = [ "*" ];
      allow_users = [ "*" ];
      app_token = "";
      socket_mode = false;
    }
  in
  Alcotest.(check bool)
    "both wildcard" true
    (Slack.is_allowed ~config:cfg ~channel_id:"CANY" ~user_id:"UANY")

let test_slack_is_allowed_neither_match () =
  let cfg : Runtime_config.slack_config =
    {
      bot_token = "xoxb";
      signing_secret = "";
      events_path = "/e";
      allow_channels = [ "CSPECIFIC" ];
      allow_users = [ "USPECIFIC" ];
      app_token = "";
      socket_mode = false;
    }
  in
  Alcotest.(check bool)
    "neither matches" false
    (Slack.is_allowed ~config:cfg ~channel_id:"COTHER" ~user_id:"UOTHER")

(* ===== Additional Discord tests ===== *)

let test_discord_chunk_single_exact_2000 () =
  let text = String.make 2000 'y' in
  let chunks = Discord.chunk_text text in
  Alcotest.(check int) "1 chunk for 2000 chars" 1 (List.length chunks)

let test_discord_chunk_2001_chars () =
  let text = String.make 2001 'y' in
  let chunks = Discord.chunk_text text in
  Alcotest.(check int) "2 chunks for 2001 chars" 2 (List.length chunks)

let test_discord_session_key_deterministic () =
  let k1 = Discord.session_key ~channel_id:"C1" ~author_id:"A1" in
  let k2 = Discord.session_key ~channel_id:"C1" ~author_id:"A1" in
  Alcotest.(check string) "deterministic key" k1 k2

let test_discord_session_key_unique_per_user () =
  let k1 = Discord.session_key ~channel_id:"C1" ~author_id:"A1" in
  let k2 = Discord.session_key ~channel_id:"C1" ~author_id:"A2" in
  Alcotest.(check bool) "unique per user" true (k1 <> k2)

let test_discord_session_key_unique_per_channel () =
  let k1 = Discord.session_key ~channel_id:"C1" ~author_id:"A1" in
  let k2 = Discord.session_key ~channel_id:"C2" ~author_id:"A1" in
  Alcotest.(check bool) "unique per channel" true (k1 <> k2)

(* ===== More telegram tests ===== *)

let test_telegram_parse_missing_from () =
  (* from field missing - should be handled gracefully *)
  let json_str =
    {|{"update_id": 1, "message": {"chat": {"id": 123}, "text": "hi"}}|}
  in
  let json = Yojson.Safe.from_string json_str in
  let open Yojson.Safe.Util in
  let chat_id =
    json |> member "message" |> member "chat" |> member "id" |> to_int
  in
  Alcotest.(check int) "chat_id extracted" 123 chat_id

let test_telegram_api_base_format () =
  Alcotest.(check bool)
    "api_base ends with /bot" true
    (let base = !Telegram.api_base in
     let len = String.length base in
     len > 4 && String.sub base (len - 4) 4 = "/bot")

let test_telegram_pairing_active_before_expiry () =
  let chat_id = "chat_pair_active" in
  Hashtbl.replace Telegram._paired_sessions chat_id (Unix.gettimeofday () +. 5.0);
  let paired = Telegram.is_totp_paired ~chat_id ~now:(Unix.gettimeofday ()) in
  Alcotest.(check bool) "paired session active before expiry" true paired;
  Hashtbl.remove Telegram._paired_sessions chat_id

let test_telegram_pairing_inactive_after_expiry () =
  let chat_id = "chat_pair_expired" in
  Hashtbl.replace Telegram._paired_sessions chat_id (Unix.gettimeofday () -. 1.0);
  let paired = Telegram.is_totp_paired ~chat_id ~now:(Unix.gettimeofday ()) in
  Alcotest.(check bool) "paired session rejected after expiry" false paired;
  Hashtbl.remove Telegram._paired_sessions chat_id

let test_telegram_cleanup_expired_pairings () =
  let expired_id = "chat_pair_cleanup_expired" in
  let active_id = "chat_pair_cleanup_active" in
  Hashtbl.replace Telegram._paired_sessions expired_id
    (Unix.gettimeofday () -. 10.0);
  Hashtbl.replace Telegram._paired_sessions active_id
    (Unix.gettimeofday () +. 10.0);
  Telegram.cleanup_expired_sessions ();
  Alcotest.(check bool)
    "expired pairing removed" true
    (Hashtbl.find_opt Telegram._paired_sessions expired_id = None);
  Alcotest.(check bool)
    "active pairing retained" true
    (Hashtbl.find_opt Telegram._paired_sessions active_id <> None);
  Hashtbl.remove Telegram._paired_sessions active_id

let test_telegram_multiple_accounts_config () =
  let cfg : Runtime_config.telegram_config =
    {
      accounts =
        [
          ("main", { bot_token = "tok1"; allow_from = [ "*" ]; totp = None });
          ( "secondary",
            { bot_token = "tok2"; allow_from = [ "123" ]; totp = None } );
        ];
      text_coalesce_ms = 150;
    }
  in
  Alcotest.(check int) "2 accounts" 2 (List.length cfg.accounts)

let test_slack_parse_complex_event () =
  let body =
    {|{
      "type": "event_callback",
      "event": {
        "type": "message",
        "channel": "CREVIEW",
        "user": "UREVIEW",
        "text": "Please review this PR",
        "ts": "1234567890.123456"
      }
    }|}
  in
  match Slack.parse_event body with
  | Some (Slack.Message { text; _ }) ->
      Alcotest.(check bool) "text contains PR" true (contains_str text "PR")
  | _ -> Alcotest.fail "expected Message"

let test_discord_name_constant () =
  Alcotest.(check string) "discord name" "discord" Discord.name

let suite =
  [
    (* Slack tests *)
    Alcotest.test_case "slack parse url_verification" `Quick
      test_slack_parse_url_verification;
    Alcotest.test_case "slack parse message event" `Quick
      test_slack_parse_message_event;
    Alcotest.test_case "slack parse bot message" `Quick
      test_slack_parse_bot_message;
    Alcotest.test_case "slack parse invalid json" `Quick
      test_slack_parse_invalid_json;
    Alcotest.test_case "slack parse other event" `Quick
      test_slack_parse_other_event;
    Alcotest.test_case "slack parse unknown type" `Quick
      test_slack_parse_unknown_type;
    Alcotest.test_case "slack is_allowed wildcard channel" `Quick
      test_slack_is_allowed_wildcard_channel;
    Alcotest.test_case "slack is_allowed specific channel match" `Quick
      test_slack_is_allowed_specific_channel_match;
    Alcotest.test_case "slack is_allowed channel no match" `Quick
      test_slack_is_allowed_specific_channel_no_match;
    Alcotest.test_case "slack is_allowed user filter" `Quick
      test_slack_is_allowed_user_filter;
    Alcotest.test_case "slack verify signature bad timestamp" `Quick
      test_slack_verify_signature_bad_timestamp;
    Alcotest.test_case "slack verify signature bad sig" `Quick
      test_slack_verify_signature_bad_sig;
    Alcotest.test_case "slack parse message missing text" `Quick
      test_slack_parse_message_missing_text;
    Alcotest.test_case "slack parse missing user" `Quick
      test_slack_parse_missing_user;
    Alcotest.test_case "slack verify valid signature" `Quick
      test_slack_verify_valid_signature;
    Alcotest.test_case "slack verify signature small future skew" `Quick
      test_slack_verify_signature_small_future_skew;
    Alcotest.test_case "slack verify signature far future skew" `Quick
      test_slack_verify_signature_far_future_skew;
    Alcotest.test_case "slack parse empty body" `Quick
      test_slack_parse_empty_body;
    Alcotest.test_case "slack parse empty json" `Quick
      test_slack_parse_empty_json;
    Alcotest.test_case "slack is_allowed both wildcard" `Quick
      test_slack_is_allowed_both_wildcard;
    Alcotest.test_case "slack is_allowed neither match" `Quick
      test_slack_is_allowed_neither_match;
    Alcotest.test_case "slack parse complex event" `Quick
      test_slack_parse_complex_event;
    (* Discord tests *)
    Alcotest.test_case "discord chunk short text" `Quick
      test_discord_chunk_text_short;
    Alcotest.test_case "discord chunk exact limit" `Quick
      test_discord_chunk_text_exact_limit;
    Alcotest.test_case "discord chunk over limit" `Quick
      test_discord_chunk_text_over_limit;
    Alcotest.test_case "discord chunk custom max_len" `Quick
      test_discord_chunk_text_custom_max_len;
    Alcotest.test_case "discord is_allowed wildcard" `Quick
      test_discord_is_allowed_wildcard;
    Alcotest.test_case "discord is_allowed specific guild" `Quick
      test_discord_is_allowed_specific_guild;
    Alcotest.test_case "discord is_allowed no guild" `Quick
      test_discord_is_allowed_no_guild;
    Alcotest.test_case "discord is_allowed user filter" `Quick
      test_discord_is_allowed_user_filter;
    Alcotest.test_case "discord session_key format" `Quick
      test_discord_session_key_format;
    Alcotest.test_case "discord chunk preserves content" `Quick
      test_discord_chunk_preserves_content;
    Alcotest.test_case "discord chunk empty text" `Quick
      test_discord_chunk_empty_text;
    Alcotest.test_case "discord is_allowed wildcard guild with None" `Quick
      test_discord_is_allowed_wildcard_guild_with_no_guild_id;
    Alcotest.test_case "discord chunk single 2000" `Quick
      test_discord_chunk_single_exact_2000;
    Alcotest.test_case "discord chunk 2001 chars" `Quick
      test_discord_chunk_2001_chars;
    Alcotest.test_case "discord session key deterministic" `Quick
      test_discord_session_key_deterministic;
    Alcotest.test_case "discord session key unique per user" `Quick
      test_discord_session_key_unique_per_user;
    Alcotest.test_case "discord session key unique per channel" `Quick
      test_discord_session_key_unique_per_channel;
    Alcotest.test_case "discord name constant" `Quick test_discord_name_constant;
    (* Telegram tests *)
    Alcotest.test_case "telegram redact short token" `Quick
      test_telegram_redact_short_token;
    Alcotest.test_case "telegram redact long token" `Quick
      test_telegram_redact_long_token;
    Alcotest.test_case "telegram redact preserves first four" `Quick
      test_telegram_redact_preserves_first_four;
    Alcotest.test_case "telegram api_base contains telegram.org" `Quick
      test_telegram_chunk_text_short;
    Alcotest.test_case "telegram parse update structure" `Quick
      test_telegram_parse_update_structure;
    Alcotest.test_case "telegram parse voice message" `Quick
      test_telegram_parse_voice_message;
    Alcotest.test_case "telegram parse photo message" `Quick
      test_telegram_parse_photo_message;
    Alcotest.test_case "telegram allow_from wildcard" `Quick
      test_telegram_allow_from_wildcard;
    Alcotest.test_case "telegram allow_from specific" `Quick
      test_telegram_allow_from_specific;
    Alcotest.test_case "telegram parse document message" `Quick
      test_telegram_parse_document_message;
    Alcotest.test_case "telegram empty text default" `Quick
      test_telegram_empty_text_default;
    Alcotest.test_case "telegram redact 8 char token" `Quick
      test_telegram_redact_eight_char_token;
    Alcotest.test_case "telegram parse missing from" `Quick
      test_telegram_parse_missing_from;
    Alcotest.test_case "telegram api_base format" `Quick
      test_telegram_api_base_format;
    Alcotest.test_case "telegram pairing active before expiry" `Quick
      test_telegram_pairing_active_before_expiry;
    Alcotest.test_case "telegram pairing inactive after expiry" `Quick
      test_telegram_pairing_inactive_after_expiry;
    Alcotest.test_case "telegram cleanup expired pairings" `Quick
      test_telegram_cleanup_expired_pairings;
    Alcotest.test_case "telegram multiple accounts config" `Quick
      test_telegram_multiple_accounts_config;
  ]
