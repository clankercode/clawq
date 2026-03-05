let make_config ?(allow_guilds = ["*"]) ?(allow_users = ["*"])
    ?(bot_token = "test-token") ?(intents = 513) () : Runtime_config.discord_config =
  { bot_token; allow_guilds; allow_users; intents }

let test_is_allowed_wildcard () =
  let config = make_config () in
  Alcotest.(check bool) "wildcard allows any"
    true (Discord.is_allowed ~config ~guild_id:(Some "123") ~user_id:"456")

let test_is_allowed_specific_guild () =
  let config = make_config ~allow_guilds:["guild1"] () in
  Alcotest.(check bool) "matching guild"
    true (Discord.is_allowed ~config ~guild_id:(Some "guild1") ~user_id:"any");
  Alcotest.(check bool) "non-matching guild"
    false (Discord.is_allowed ~config ~guild_id:(Some "other") ~user_id:"any")

let test_is_allowed_specific_user () =
  let config = make_config ~allow_users:["user1"] () in
  Alcotest.(check bool) "matching user"
    true (Discord.is_allowed ~config ~guild_id:(Some "any") ~user_id:"user1");
  Alcotest.(check bool) "non-matching user"
    false (Discord.is_allowed ~config ~guild_id:(Some "any") ~user_id:"other")

let test_is_allowed_combined () =
  let config = make_config ~allow_guilds:["g1"] ~allow_users:["u1"] () in
  Alcotest.(check bool) "both match"
    true (Discord.is_allowed ~config ~guild_id:(Some "g1") ~user_id:"u1");
  Alcotest.(check bool) "guild mismatch"
    false (Discord.is_allowed ~config ~guild_id:(Some "g2") ~user_id:"u1");
  Alcotest.(check bool) "user mismatch"
    false (Discord.is_allowed ~config ~guild_id:(Some "g1") ~user_id:"u2");
  Alcotest.(check bool) "both mismatch"
    false (Discord.is_allowed ~config ~guild_id:(Some "g2") ~user_id:"u2")

let test_send_message_chunking () =
  let short = "hello" in
  let chunks_short = Discord.chunk_text short in
  Alcotest.(check int) "short message = 1 chunk" 1 (List.length chunks_short);
  Alcotest.(check string) "short chunk content" "hello" (List.hd chunks_short);
  let long = String.make 4500 'x' in
  let chunks_long = Discord.chunk_text long in
  Alcotest.(check int) "4500 chars = 3 chunks" 3 (List.length chunks_long);
  Alcotest.(check int) "first chunk len" 2000 (String.length (List.nth chunks_long 0));
  Alcotest.(check int) "second chunk len" 2000 (String.length (List.nth chunks_long 1));
  Alcotest.(check int) "third chunk len" 500 (String.length (List.nth chunks_long 2));
  let exact = String.make 2000 'y' in
  let chunks_exact = Discord.chunk_text exact in
  Alcotest.(check int) "exactly 2000 = 1 chunk" 1 (List.length chunks_exact)

let test_parse_message_create () =
  let json = Yojson.Safe.from_string {|{
    "t": "MESSAGE_CREATE",
    "d": {
      "id": "123",
      "channel_id": "456",
      "guild_id": "789",
      "author": {"id": "111", "username": "user", "bot": false},
      "content": "hello"
    }
  }|} in
  match Discord.parse_message_create json with
  | None -> Alcotest.fail "expected Some message"
  | Some msg ->
    Alcotest.(check string) "channel_id" "456" msg.channel_id;
    Alcotest.(check (option string)) "guild_id" (Some "789") msg.guild_id;
    Alcotest.(check string) "author_id" "111" msg.author_id;
    Alcotest.(check bool) "author_bot" false msg.author_bot;
    Alcotest.(check string) "content" "hello" msg.content

let test_session_key_format () =
  let key = Discord.session_key ~channel_id:"ch1" ~author_id:"au1" in
  Alcotest.(check string) "session key format" "discord:ch1:au1" key

let test_bot_message_ignored () =
  let bot_json = Yojson.Safe.from_string {|{
    "t": "MESSAGE_CREATE",
    "d": {
      "id": "1",
      "channel_id": "c",
      "author": {"id": "b", "bot": true},
      "content": "bot says hi"
    }
  }|} in
  Alcotest.(check bool) "bot message detected" true (Discord.is_bot_message bot_json);
  let user_json = Yojson.Safe.from_string {|{
    "t": "MESSAGE_CREATE",
    "d": {
      "id": "2",
      "channel_id": "c",
      "author": {"id": "u", "bot": false},
      "content": "user says hi"
    }
  }|} in
  Alcotest.(check bool) "user message not bot" false (Discord.is_bot_message user_json);
  (* Also verify parse_message_create detects bot *)
  match Discord.parse_message_create bot_json with
  | None -> Alcotest.fail "should parse bot message"
  | Some msg -> Alcotest.(check bool) "parsed bot flag" true msg.author_bot

let suite : unit Alcotest.test_case list =
  [
    Alcotest.test_case "is_allowed wildcard" `Quick test_is_allowed_wildcard;
    Alcotest.test_case "is_allowed specific guild" `Quick test_is_allowed_specific_guild;
    Alcotest.test_case "is_allowed specific user" `Quick test_is_allowed_specific_user;
    Alcotest.test_case "is_allowed combined" `Quick test_is_allowed_combined;
    Alcotest.test_case "send_message chunking" `Quick test_send_message_chunking;
    Alcotest.test_case "parse_message_create" `Quick test_parse_message_create;
    Alcotest.test_case "session_key format" `Quick test_session_key_format;
    Alcotest.test_case "bot_message ignored" `Quick test_bot_message_ignored;
  ]
