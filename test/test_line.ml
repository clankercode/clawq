(* Tests for LINE channel module *)

let mk_line_cfg ?(allow_from = [ "*" ]) () : Runtime_config.line_config =
  {
    channel_access_token = "tok";
    channel_secret = "secret";
    allow_from;
    default_model = None;
  }

(* --- is_allowed tests --- *)

let test_is_allowed_wildcard () =
  let config = mk_line_cfg () in
  Alcotest.(check bool)
    "wildcard" true
    (Line_channel.is_allowed ~config ~user_id:"any")

let test_is_allowed_match () =
  let config = mk_line_cfg ~allow_from:[ "user1" ] () in
  Alcotest.(check bool)
    "match" true
    (Line_channel.is_allowed ~config ~user_id:"user1")

let test_is_allowed_no_match () =
  let config = mk_line_cfg ~allow_from:[ "user1" ] () in
  Alcotest.(check bool)
    "no match" false
    (Line_channel.is_allowed ~config ~user_id:"user9")

(* --- verify_signature tests --- *)

let test_verify_valid_signature () =
  let channel_secret = "test-secret" in
  let body = "test body" in
  let computed =
    Digestif.SHA256.hmac_string ~key:channel_secret body
    |> Digestif.SHA256.to_raw_string |> Base64.encode_exn
  in
  Alcotest.(check bool)
    "valid sig" true
    (Line_channel.verify_signature ~channel_secret ~body ~signature:computed)

let test_verify_invalid_signature () =
  Alcotest.(check bool)
    "invalid sig" false
    (Line_channel.verify_signature ~channel_secret:"secret" ~body:"test"
       ~signature:"wrong")

(* --- parse_events tests --- *)

let test_parse_events_valid () =
  let body =
    {|{"events":[
    {"type":"message","source":{"userId":"u1"},"replyToken":"rt1","message":{"type":"text","text":"hello"}}
  ]}|}
  in
  let events = Line_channel.parse_events body in
  Alcotest.(check int) "1 event" 1 (List.length events);
  let user_id, reply_token, text = List.hd events in
  Alcotest.(check string) "user_id" "u1" user_id;
  Alcotest.(check string) "reply_token" "rt1" reply_token;
  Alcotest.(check string) "text" "hello" text

let test_parse_events_empty () =
  let events = Line_channel.parse_events {|{"events":[]}|} in
  Alcotest.(check int) "no events" 0 (List.length events)

let test_parse_events_non_text () =
  let body =
    {|{"events":[
    {"type":"message","source":{"userId":"u1"},"replyToken":"rt1","message":{"type":"image","text":""}}
  ]}|}
  in
  let events = Line_channel.parse_events body in
  Alcotest.(check int) "skip non-text" 0 (List.length events)

let test_parse_events_non_message () =
  let body = {|{"events":[{"type":"follow","source":{"userId":"u1"}}]}|} in
  let events = Line_channel.parse_events body in
  Alcotest.(check int) "skip non-message" 0 (List.length events)

let test_parse_events_invalid () =
  let events = Line_channel.parse_events "bad json" in
  Alcotest.(check int) "no events for invalid" 0 (List.length events)

let test_parse_events_missing_user () =
  let body =
    {|{"events":[
    {"type":"message","source":{},"replyToken":"rt1","message":{"type":"text","text":"hello"}}
  ]}|}
  in
  let events = Line_channel.parse_events body in
  Alcotest.(check int) "skip missing user" 0 (List.length events)

let test_parse_events_empty_text () =
  let body =
    {|{"events":[
    {"type":"message","source":{"userId":"u1"},"replyToken":"rt1","message":{"type":"text","text":""}}
  ]}|}
  in
  let events = Line_channel.parse_events body in
  Alcotest.(check int) "skip empty text" 0 (List.length events)

let test_parse_events_multiple () =
  let body =
    {|{"events":[
    {"type":"message","source":{"userId":"u1"},"replyToken":"rt1","message":{"type":"text","text":"msg1"}},
    {"type":"message","source":{"userId":"u2"},"replyToken":"rt2","message":{"type":"text","text":"msg2"}}
  ]}|}
  in
  let events = Line_channel.parse_events body in
  Alcotest.(check int) "2 events" 2 (List.length events)

let suite =
  [
    Alcotest.test_case "is_allowed wildcard" `Quick test_is_allowed_wildcard;
    Alcotest.test_case "is_allowed match" `Quick test_is_allowed_match;
    Alcotest.test_case "is_allowed no match" `Quick test_is_allowed_no_match;
    Alcotest.test_case "verify valid sig" `Quick test_verify_valid_signature;
    Alcotest.test_case "verify invalid sig" `Quick test_verify_invalid_signature;
    Alcotest.test_case "parse events valid" `Quick test_parse_events_valid;
    Alcotest.test_case "parse events empty" `Quick test_parse_events_empty;
    Alcotest.test_case "parse events non-text" `Quick test_parse_events_non_text;
    Alcotest.test_case "parse events non-message" `Quick
      test_parse_events_non_message;
    Alcotest.test_case "parse events invalid" `Quick test_parse_events_invalid;
    Alcotest.test_case "parse events missing user" `Quick
      test_parse_events_missing_user;
    Alcotest.test_case "parse events empty text" `Quick
      test_parse_events_empty_text;
    Alcotest.test_case "parse events multiple" `Quick test_parse_events_multiple;
  ]
