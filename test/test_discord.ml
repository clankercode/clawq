let make_config ?(allow_guilds = [ "*" ]) ?(allow_users = [ "*" ])
    ?(bot_token = "test-token") ?(intents = 513) () :
    Runtime_config.discord_config =
  { bot_token; allow_guilds; allow_users; intents }

let test_is_allowed_wildcard () =
  let config = make_config () in
  Alcotest.(check bool)
    "wildcard allows any" true
    (Discord.is_allowed ~config ~guild_id:(Some "123") ~user_id:"456")

let test_is_allowed_specific_guild () =
  let config = make_config ~allow_guilds:[ "guild1" ] () in
  Alcotest.(check bool)
    "matching guild" true
    (Discord.is_allowed ~config ~guild_id:(Some "guild1") ~user_id:"any");
  Alcotest.(check bool)
    "non-matching guild" false
    (Discord.is_allowed ~config ~guild_id:(Some "other") ~user_id:"any")

let test_is_allowed_specific_user () =
  let config = make_config ~allow_users:[ "user1" ] () in
  Alcotest.(check bool)
    "matching user" true
    (Discord.is_allowed ~config ~guild_id:(Some "any") ~user_id:"user1");
  Alcotest.(check bool)
    "non-matching user" false
    (Discord.is_allowed ~config ~guild_id:(Some "any") ~user_id:"other")

let test_is_allowed_combined () =
  let config = make_config ~allow_guilds:[ "g1" ] ~allow_users:[ "u1" ] () in
  Alcotest.(check bool)
    "both match" true
    (Discord.is_allowed ~config ~guild_id:(Some "g1") ~user_id:"u1");
  Alcotest.(check bool)
    "guild mismatch" false
    (Discord.is_allowed ~config ~guild_id:(Some "g2") ~user_id:"u1");
  Alcotest.(check bool)
    "user mismatch" false
    (Discord.is_allowed ~config ~guild_id:(Some "g1") ~user_id:"u2");
  Alcotest.(check bool)
    "both mismatch" false
    (Discord.is_allowed ~config ~guild_id:(Some "g2") ~user_id:"u2")

let test_send_message_chunking () =
  let short = "hello" in
  let chunks_short = Discord.chunk_text short in
  Alcotest.(check int) "short message = 1 chunk" 1 (List.length chunks_short);
  Alcotest.(check string) "short chunk content" "hello" (List.hd chunks_short);
  let long = String.make 4500 'x' in
  let chunks_long = Discord.chunk_text long in
  Alcotest.(check int) "4500 chars = 3 chunks" 3 (List.length chunks_long);
  Alcotest.(check int)
    "first chunk len" 2000
    (String.length (List.nth chunks_long 0));
  Alcotest.(check int)
    "second chunk len" 2000
    (String.length (List.nth chunks_long 1));
  Alcotest.(check int)
    "third chunk len" 500
    (String.length (List.nth chunks_long 2));
  let exact = String.make 2000 'y' in
  let chunks_exact = Discord.chunk_text exact in
  Alcotest.(check int) "exactly 2000 = 1 chunk" 1 (List.length chunks_exact)

let test_parse_message_create () =
  let json =
    Yojson.Safe.from_string
      {|{
    "t": "MESSAGE_CREATE",
    "d": {
      "id": "123",
      "channel_id": "456",
      "guild_id": "789",
      "author": {"id": "111", "username": "user", "bot": false},
      "content": "hello"
    }
  }|}
  in
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
  let bot_json =
    Yojson.Safe.from_string
      {|{
    "t": "MESSAGE_CREATE",
    "d": {
      "id": "1",
      "channel_id": "c",
      "author": {"id": "b", "bot": true},
      "content": "bot says hi"
    }
  }|}
  in
  Alcotest.(check bool)
    "bot message detected" true
    (Discord.is_bot_message bot_json);
  let user_json =
    Yojson.Safe.from_string
      {|{
    "t": "MESSAGE_CREATE",
    "d": {
      "id": "2",
      "channel_id": "c",
      "author": {"id": "u", "bot": false},
      "content": "user says hi"
    }
  }|}
  in
  Alcotest.(check bool)
    "user message not bot" false
    (Discord.is_bot_message user_json);
  (* Also verify parse_message_create detects bot *)
  match Discord.parse_message_create bot_json with
  | None -> Alcotest.fail "should parse bot message"
  | Some msg -> Alcotest.(check bool) "parsed bot flag" true msg.author_bot

let test_update_rate_limit_headers () =
  let route = "test-route-1" in
  Hashtbl.clear Discord.route_buckets;
  let headers =
    Cohttp.Header.of_list
      [ ("x-ratelimit-remaining", "4"); ("x-ratelimit-reset", "1700000010.0") ]
  in
  Discord.update_rate_limit ~route ~headers;
  match Hashtbl.find_opt Discord.route_buckets route with
  | None -> Alcotest.fail "expected bucket to be created"
  | Some bucket ->
      Alcotest.(check int) "remaining" 4 bucket.remaining;
      Alcotest.(check (float 0.1)) "reset_at" 1700000010.0 bucket.reset_at

let test_update_rate_limit_global () =
  let route = "test-route-2" in
  Hashtbl.clear Discord.route_buckets;
  Discord.global_rate_limit := 0.0;
  let headers =
    Cohttp.Header.of_list
      [ ("x-ratelimit-global", "true"); ("retry-after", "2.5") ]
  in
  Discord.update_rate_limit ~route ~headers;
  Alcotest.(check bool)
    "global rate limit set" true
    (!Discord.global_rate_limit > Unix.gettimeofday ());
  Discord.global_rate_limit := 0.0

let test_wait_for_rate_limit_no_bucket () =
  Hashtbl.clear Discord.route_buckets;
  Discord.global_rate_limit := 0.0;
  let t0 = Unix.gettimeofday () in
  Lwt_main.run (Discord.wait_for_rate_limit ~route:"unknown-route");
  let elapsed = Unix.gettimeofday () -. t0 in
  Alcotest.(check bool) "no delay for unknown route" true (elapsed < 0.1)

let test_is_fatal_close_code () =
  Alcotest.(check bool) "4004 fatal" true (Discord.is_fatal_close_code 4004);
  Alcotest.(check bool) "4010 fatal" true (Discord.is_fatal_close_code 4010);
  Alcotest.(check bool) "4011 fatal" true (Discord.is_fatal_close_code 4011);
  Alcotest.(check bool) "4012 fatal" true (Discord.is_fatal_close_code 4012);
  Alcotest.(check bool) "4013 fatal" true (Discord.is_fatal_close_code 4013);
  Alcotest.(check bool) "4014 fatal" true (Discord.is_fatal_close_code 4014);
  Alcotest.(check bool)
    "4000 not fatal" false
    (Discord.is_fatal_close_code 4000);
  Alcotest.(check bool)
    "4001 not fatal" false
    (Discord.is_fatal_close_code 4001);
  Alcotest.(check bool)
    "1000 not fatal" false
    (Discord.is_fatal_close_code 1000)

let test_is_allowed_dm_non_wildcard_guild () =
  let config = make_config ~allow_guilds:[ "guild1" ] () in
  Alcotest.(check bool)
    "DM with non-wildcard guild list" false
    (Discord.is_allowed ~config ~guild_id:None ~user_id:"any")

let test_parse_message_create_wrong_type () =
  let json =
    Yojson.Safe.from_string
      {|{
    "t": "GUILD_CREATE",
    "d": {
      "channel_id": "456",
      "author": {"id": "111"},
      "content": "hello"
    }
  }|}
  in
  Alcotest.(check bool)
    "non-MESSAGE_CREATE returns None" true
    (Discord.parse_message_create json = None)

let test_parse_dispatch_message_malformed () =
  let d = Yojson.Safe.from_string {|{"not_valid": true}|} in
  Alcotest.(check bool)
    "malformed dispatch returns None" true
    (Discord.parse_dispatch_message d = None)

let test_parse_dispatch_message () =
  let d =
    Yojson.Safe.from_string
      {|{"id":"msg1","channel_id":"ch1","guild_id":"g1","author":{"id":"u1","bot":false},"content":"test"}|}
  in
  match Discord.parse_dispatch_message d with
  | None -> Alcotest.fail "expected Some message"
  | Some msg ->
      Alcotest.(check string) "channel_id" "ch1" msg.channel_id;
      Alcotest.(check (option string)) "guild_id" (Some "g1") msg.guild_id;
      Alcotest.(check string) "author_id" "u1" msg.author_id;
      Alcotest.(check bool) "author_bot" false msg.author_bot;
      Alcotest.(check string) "content" "test" msg.content

let test_parse_dispatch_message_dm () =
  let d =
    Yojson.Safe.from_string
      {|{"id":"msg2","channel_id":"dm1","author":{"id":"u2"},"content":"dm test"}|}
  in
  match Discord.parse_dispatch_message d with
  | None -> Alcotest.fail "expected Some message"
  | Some msg ->
      Alcotest.(check string) "channel_id" "dm1" msg.channel_id;
      Alcotest.(check (option string)) "guild_id" None msg.guild_id;
      Alcotest.(check string) "author_id" "u2" msg.author_id;
      Alcotest.(check bool) "author_bot" false msg.author_bot;
      Alcotest.(check string) "content" "dm test" msg.content

let test_parse_message_create_with_mentions () =
  let json =
    Yojson.Safe.from_string
      {|{
    "t": "MESSAGE_CREATE",
    "d": {
      "id": "123",
      "channel_id": "456",
      "guild_id": "789",
      "author": {"id": "111", "username": "user", "bot": false},
      "content": "<@bot-id> hello",
      "mentions": [{"id": "bot-id", "username": "clawq"}]
    }
  }|}
  in
  match Discord.parse_message_create json with
  | None -> Alcotest.fail "expected Some message"
  | Some msg ->
      Alcotest.(check int) "mention_ids count" 1 (List.length msg.mention_ids);
      Alcotest.(check string) "mention_id" "bot-id" (List.hd msg.mention_ids)

let test_parse_dispatch_message_with_mentions () =
  let d =
    Yojson.Safe.from_string
      {|{"id":"msg3","channel_id":"ch1","guild_id":"g1","author":{"id":"u1","bot":false},"content":"test","mentions":[{"id":"bot1"},{"id":"bot2"}]}|}
  in
  match Discord.parse_dispatch_message d with
  | None -> Alcotest.fail "expected Some message"
  | Some msg ->
      Alcotest.(check int) "mention_ids count" 2 (List.length msg.mention_ids);
      Alcotest.(check bool)
        "bot1 in mentions" true
        (List.mem "bot1" msg.mention_ids);
      Alcotest.(check bool)
        "bot2 in mentions" true
        (List.mem "bot2" msg.mention_ids)

let test_parse_message_create_empty_mentions () =
  let json =
    Yojson.Safe.from_string
      {|{
    "t": "MESSAGE_CREATE",
    "d": {
      "id": "123",
      "channel_id": "456",
      "author": {"id": "111", "bot": false},
      "content": "hello",
      "mentions": []
    }
  }|}
  in
  match Discord.parse_message_create json with
  | None -> Alcotest.fail "expected Some message"
  | Some msg ->
      Alcotest.(check int) "empty mentions" 0 (List.length msg.mention_ids)

let test_parse_message_create_with_attachments () =
  let json =
    Yojson.Safe.from_string
      {|{
    "t": "MESSAGE_CREATE",
    "d": {
      "id": "m1",
      "channel_id": "c1",
      "guild_id": "g1",
      "author": {"id": "u1", "bot": false},
      "content": "check this out",
      "attachments": [
        {
          "id": "att1",
          "filename": "report.pdf",
          "url": "https://cdn.discord.com/att/report.pdf",
          "content_type": "application/pdf",
          "size": 12345
        }
      ]
    }
  }|}
  in
  match Discord.parse_message_create json with
  | None -> Alcotest.fail "expected Some"
  | Some msg ->
      Alcotest.(check int) "one attachment" 1 (List.length msg.attachments);
      let a = List.hd msg.attachments in
      Alcotest.(check string) "att_filename" "report.pdf" a.att_filename;
      Alcotest.(check string)
        "att_url" "https://cdn.discord.com/att/report.pdf" a.att_url;
      Alcotest.(check (option string))
        "att_content_type" (Some "application/pdf") a.att_content_type;
      Alcotest.(check int) "att_size" 12345 a.att_size

let test_parse_dispatch_message_with_attachments () =
  let d =
    Yojson.Safe.from_string
      {|{
    "id": "m2",
    "channel_id": "c2",
    "guild_id": "g2",
    "author": {"id": "u2", "bot": false},
    "content": "image",
    "attachments": [
      {
        "id": "att2",
        "filename": "photo.jpg",
        "url": "https://cdn.discord.com/att/photo.jpg",
        "content_type": "image/jpeg",
        "size": 54321
      }
    ]
  }|}
  in
  match Discord.parse_dispatch_message d with
  | None -> Alcotest.fail "expected Some"
  | Some msg ->
      Alcotest.(check int) "one attachment" 1 (List.length msg.attachments);
      let a = List.hd msg.attachments in
      Alcotest.(check string) "att_filename" "photo.jpg" a.att_filename;
      Alcotest.(check int) "att_size" 54321 a.att_size

let test_parse_message_create_no_attachments () =
  let json =
    Yojson.Safe.from_string
      {|{
    "t": "MESSAGE_CREATE",
    "d": {
      "id": "m3",
      "channel_id": "c3",
      "author": {"id": "u3", "bot": false},
      "content": "no files"
    }
  }|}
  in
  match Discord.parse_message_create json with
  | None -> Alcotest.fail "expected Some"
  | Some msg ->
      Alcotest.(check int) "no attachments" 0 (List.length msg.attachments)

let suite : unit Alcotest.test_case list =
  [
    Alcotest.test_case "is_allowed wildcard" `Quick test_is_allowed_wildcard;
    Alcotest.test_case "is_allowed specific guild" `Quick
      test_is_allowed_specific_guild;
    Alcotest.test_case "is_allowed specific user" `Quick
      test_is_allowed_specific_user;
    Alcotest.test_case "is_allowed combined" `Quick test_is_allowed_combined;
    Alcotest.test_case "send_message chunking" `Quick test_send_message_chunking;
    Alcotest.test_case "parse_message_create" `Quick test_parse_message_create;
    Alcotest.test_case "session_key format" `Quick test_session_key_format;
    Alcotest.test_case "bot_message ignored" `Quick test_bot_message_ignored;
    Alcotest.test_case "rate limit header update" `Quick
      test_update_rate_limit_headers;
    Alcotest.test_case "rate limit global" `Quick test_update_rate_limit_global;
    Alcotest.test_case "wait no bucket" `Quick
      test_wait_for_rate_limit_no_bucket;
    Alcotest.test_case "fatal close codes" `Quick test_is_fatal_close_code;
    Alcotest.test_case "is_allowed DM non-wildcard guild" `Quick
      test_is_allowed_dm_non_wildcard_guild;
    Alcotest.test_case "parse_message_create wrong type" `Quick
      test_parse_message_create_wrong_type;
    Alcotest.test_case "parse dispatch message malformed" `Quick
      test_parse_dispatch_message_malformed;
    Alcotest.test_case "parse dispatch message" `Quick
      test_parse_dispatch_message;
    Alcotest.test_case "parse dispatch message dm" `Quick
      test_parse_dispatch_message_dm;
    Alcotest.test_case "parse_message_create with mentions" `Quick
      test_parse_message_create_with_mentions;
    Alcotest.test_case "parse_dispatch_message with mentions" `Quick
      test_parse_dispatch_message_with_mentions;
    Alcotest.test_case "parse_message_create empty mentions" `Quick
      test_parse_message_create_empty_mentions;
    Alcotest.test_case "parse_message_create with attachments" `Quick
      test_parse_message_create_with_attachments;
    Alcotest.test_case "parse_dispatch_message with attachments" `Quick
      test_parse_dispatch_message_with_attachments;
    Alcotest.test_case "parse_message_create no attachments" `Quick
      test_parse_message_create_no_attachments;
  ]
