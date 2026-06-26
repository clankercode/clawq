(* test_setup_discord.ml — Unit tests for Setup_discord pure functions *)

let validate_token_valid () =
  Alcotest.(check (result string string))
    "valid" (Ok "my_bot_token")
    (Setup_discord.validate_bot_token "my_bot_token")

let validate_token_spaces () =
  Alcotest.(check (result string string))
    "spaces trimmed" (Ok "my_bot_token")
    (Setup_discord.validate_bot_token "  my_bot_token  ")

let validate_token_empty () =
  match Setup_discord.validate_bot_token "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty token"

let validate_token_whitespace_only () =
  match Setup_discord.validate_bot_token "   " with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for whitespace-only token"

let default_intents_value () =
  Alcotest.(check int) "default intents" 33281 Setup_discord.default_intents

let default_intents_has_guilds () =
  (* GUILDS = 1 *)
  Alcotest.(check bool)
    "has GUILDS" true
    (Setup_discord.default_intents land 1 <> 0)

let default_intents_has_guild_messages () =
  (* GUILD_MESSAGES = 512 *)
  Alcotest.(check bool)
    "has GUILD_MESSAGES" true
    (Setup_discord.default_intents land 512 <> 0)

let default_intents_has_message_content () =
  (* MESSAGE_CONTENT = 32768 *)
  Alcotest.(check bool)
    "has MESSAGE_CONTENT" true
    (Setup_discord.default_intents land 32768 <> 0)

let build_json_roundtrip () =
  let json =
    Setup_discord.build_discord_json ~bot_token:"test_token"
      ~allow_guilds:[ "*" ] ~allow_users:[ "*" ] ~intents:33281
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.discord with
  | Some d ->
      Alcotest.(check string) "bot_token" "test_token" d.bot_token;
      Alcotest.(check (list string)) "allow_guilds" [ "*" ] d.allow_guilds;
      Alcotest.(check (list string)) "allow_users" [ "*" ] d.allow_users;
      Alcotest.(check int) "intents" 33281 d.intents
  | None -> Alcotest.fail "expected discord config"

let build_json_specific_guilds () =
  let json =
    Setup_discord.build_discord_json ~bot_token:"tok"
      ~allow_guilds:[ "123456789"; "987654321" ]
      ~allow_users:[ "111222333" ] ~intents:513
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.discord with
  | Some d ->
      Alcotest.(check (list string))
        "allow_guilds"
        [ "123456789"; "987654321" ]
        d.allow_guilds;
      Alcotest.(check (list string)) "allow_users" [ "111222333" ] d.allow_users;
      Alcotest.(check int) "intents" 513 d.intents
  | None -> Alcotest.fail "expected discord config"

let build_json_custom_intents () =
  let json =
    Setup_discord.build_discord_json ~bot_token:"tok" ~allow_guilds:[ "*" ]
      ~allow_users:[ "*" ] ~intents:4609
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.discord with
  | Some d -> Alcotest.(check int) "intents" 4609 d.intents
  | None -> Alcotest.fail "expected discord config"

let deep_merge_preserves_existing () =
  let existing =
    Yojson.Safe.from_string
      {|{"channels":{"cli":true,"telegram":{"accounts":{"default":{"bot_token":"tok"}}}},"default_temperature":0.7}|}
  in
  let overlay =
    Setup_discord.build_discord_json ~bot_token:"discord_tok"
      ~allow_guilds:[ "*" ] ~allow_users:[ "*" ] ~intents:33281
  in
  let result = Setup_common.deep_merge_json existing overlay in
  let config = Config_loader.parse_config ~resolve_secrets:false result in
  (* Discord should be present *)
  (match config.channels.discord with
  | Some d -> Alcotest.(check string) "bot_token" "discord_tok" d.bot_token
  | None -> Alcotest.fail "expected discord config after merge");
  (* Telegram should be preserved *)
  match config.channels.telegram with
  | Some _ -> ()
  | None -> Alcotest.fail "telegram should be preserved after merge"

let instructions_content () =
  let s = Setup_discord.post_setup_instructions in
  Alcotest.(check bool)
    "has developer portal" true
    (Test_helpers.string_contains s "https://discord.com/developers/applications");
  Alcotest.(check bool)
    "has invite URL pattern" true
    (Test_helpers.string_contains s "discord.com/oauth2/authorize");
  Alcotest.(check bool)
    "has MESSAGE CONTENT INTENT" true
    (Test_helpers.string_contains s "MESSAGE CONTENT INTENT");
  Alcotest.(check bool) "has bot scope" true (Test_helpers.string_contains s "bot")

let intent_names_default () =
  let names = Setup_discord.intent_names 33281 in
  Alcotest.(check bool) "has GUILDS" true (List.mem "GUILDS" names);
  Alcotest.(check bool)
    "has GUILD_MESSAGES" true
    (List.mem "GUILD_MESSAGES" names);
  Alcotest.(check bool)
    "has MESSAGE_CONTENT" true
    (List.mem "MESSAGE_CONTENT" names);
  Alcotest.(check bool)
    "no GUILD_MEMBERS" false
    (List.mem "GUILD_MEMBERS" names)

let intent_names_empty () =
  let names = Setup_discord.intent_names 0 in
  Alcotest.(check (list string)) "empty" [] names

let suite =
  [
    Alcotest.test_case "validate_bot_token valid" `Quick validate_token_valid;
    Alcotest.test_case "validate_bot_token spaces" `Quick validate_token_spaces;
    Alcotest.test_case "validate_bot_token empty" `Quick validate_token_empty;
    Alcotest.test_case "validate_bot_token whitespace" `Quick
      validate_token_whitespace_only;
    Alcotest.test_case "default_intents value" `Quick default_intents_value;
    Alcotest.test_case "default_intents has GUILDS" `Quick
      default_intents_has_guilds;
    Alcotest.test_case "default_intents has GUILD_MESSAGES" `Quick
      default_intents_has_guild_messages;
    Alcotest.test_case "default_intents has MESSAGE_CONTENT" `Quick
      default_intents_has_message_content;
    Alcotest.test_case "build_json roundtrip" `Quick build_json_roundtrip;
    Alcotest.test_case "build_json specific guilds" `Quick
      build_json_specific_guilds;
    Alcotest.test_case "build_json custom intents" `Quick
      build_json_custom_intents;
    Alcotest.test_case "deep merge preserves existing" `Quick
      deep_merge_preserves_existing;
    Alcotest.test_case "post_setup_instructions content" `Quick
      instructions_content;
    Alcotest.test_case "intent_names default" `Quick intent_names_default;
    Alcotest.test_case "intent_names empty" `Quick intent_names_empty;
  ]
