(* Tests for Lark/Feishu channel module *)

let mk_lark_cfg ?(allow_users = [ "*" ]) () : Runtime_config.lark_config =
  {
    app_id = "cli_test";
    app_secret = "secret";
    verification_token = "vtok";
    endpoint = "feishu";
    mode = "webhook";
    allow_users;
  }

(* --- is_allowed tests --- *)

let test_is_allowed_wildcard () =
  let config = mk_lark_cfg () in
  Alcotest.(check bool) "wildcard" true (Lark.is_allowed ~config ~user_id:"any")

let test_is_allowed_match () =
  let config = mk_lark_cfg ~allow_users:[ "user1" ] () in
  Alcotest.(check bool) "match" true (Lark.is_allowed ~config ~user_id:"user1")

let test_is_allowed_no_match () =
  let config = mk_lark_cfg ~allow_users:[ "user1" ] () in
  Alcotest.(check bool)
    "no match" false
    (Lark.is_allowed ~config ~user_id:"user9")

(* --- api_base tests --- *)

let test_api_base_feishu () =
  let base = Lark.api_base "feishu" in
  Alcotest.(check bool) "feishu base" true (String.length base > 0);
  Alcotest.(check bool)
    "contains feishu" true
    (let len = String.length base in
     len > 5
     &&
       try
         ignore (String.index base 'f');
         true
       with Not_found -> false)

let test_api_base_lark () =
  let base = Lark.api_base "lark" in
  Alcotest.(check bool) "lark base" true (String.length base > 0)

let test_api_base_default () =
  let base = Lark.api_base "other" in
  Alcotest.(check bool) "default is feishu" true (base = Lark.api_base "feishu")

(* --- verify_lark_signature tests --- *)

let test_verify_sig_valid () =
  let vtok = "test-verification-token" in
  let timestamp = "1234567890" in
  let nonce = "nonce123" in
  let body = "{\"test\":true}" in
  let payload = timestamp ^ nonce ^ body in
  let signature =
    Digestif.SHA256.hmac_string ~key:vtok payload |> Digestif.SHA256.to_hex
  in
  Alcotest.(check bool)
    "valid sig" true
    (Lark.verify_lark_signature ~verification_token:vtok ~timestamp ~nonce ~body
       ~signature)

let test_verify_sig_invalid () =
  Alcotest.(check bool)
    "invalid sig" false
    (Lark.verify_lark_signature ~verification_token:"tok" ~timestamp:"ts"
       ~nonce:"n" ~body:"b" ~signature:"wrong")

(* --- parse_message_event tests --- *)

let test_parse_message_valid () =
  let json =
    Yojson.Safe.from_string
      {|{"header":{"event_id":"evt1"},"event":{"message":{"chat_id":"chat1","content":"{\"text\":\"hello\"}"},"sender":{"sender_id":{"open_id":"user1"}}}}|}
  in
  match Lark.parse_message_event json with
  | Some (event_id, chat_id, user_id, _chat_type, text) ->
      Alcotest.(check string) "event_id" "evt1" event_id;
      Alcotest.(check string) "chat_id" "chat1" chat_id;
      Alcotest.(check string) "user_id" "user1" user_id;
      Alcotest.(check string) "text" "hello" text
  | None -> Alcotest.fail "expected Some"

let test_parse_message_empty_text () =
  let json =
    Yojson.Safe.from_string
      {|{"header":{"event_id":"evt1"},"event":{"message":{"chat_id":"chat1","content":"{\"text\":\"\"}"},"sender":{"sender_id":{"open_id":"user1"}}}}|}
  in
  match Lark.parse_message_event json with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for empty text"

let test_parse_message_invalid () =
  match Lark.parse_message_event `Null with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None"

let suite =
  [
    Alcotest.test_case "is_allowed wildcard" `Quick test_is_allowed_wildcard;
    Alcotest.test_case "is_allowed match" `Quick test_is_allowed_match;
    Alcotest.test_case "is_allowed no match" `Quick test_is_allowed_no_match;
    Alcotest.test_case "api_base feishu" `Quick test_api_base_feishu;
    Alcotest.test_case "api_base lark" `Quick test_api_base_lark;
    Alcotest.test_case "api_base default" `Quick test_api_base_default;
    Alcotest.test_case "verify sig valid" `Quick test_verify_sig_valid;
    Alcotest.test_case "verify sig invalid" `Quick test_verify_sig_invalid;
    Alcotest.test_case "parse message valid" `Quick test_parse_message_valid;
    Alcotest.test_case "parse message empty text" `Quick
      test_parse_message_empty_text;
    Alcotest.test_case "parse message invalid" `Quick test_parse_message_invalid;
  ]
