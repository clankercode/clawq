(* Tests for OneBot channel module *)

let mk_ob_cfg ?(allow_from = [ "*" ]) ?(allow_groups = [ "*" ]) () :
    Runtime_config.onebot_config =
  {
    ws_url = "ws://localhost:6700";
    http_url = "http://localhost:5700";
    access_token = None;
    allow_from;
    allow_groups;
    default_model = None;
  }

(* --- is_allowed_user tests --- *)

let test_user_allowed_wildcard () =
  let config = mk_ob_cfg () in
  Alcotest.(check bool)
    "wildcard" true
    (Onebot.is_allowed_user ~config ~user_id:"any")

let test_user_allowed_match () =
  let config = mk_ob_cfg ~allow_from:[ "123" ] () in
  Alcotest.(check bool)
    "match" true
    (Onebot.is_allowed_user ~config ~user_id:"123")

let test_user_allowed_no_match () =
  let config = mk_ob_cfg ~allow_from:[ "123" ] () in
  Alcotest.(check bool)
    "no match" false
    (Onebot.is_allowed_user ~config ~user_id:"999")

(* --- is_allowed_group tests --- *)

let test_group_allowed_wildcard () =
  let config = mk_ob_cfg () in
  Alcotest.(check bool)
    "wildcard" true
    (Onebot.is_allowed_group ~config ~group_id:"any")

let test_group_allowed_match () =
  let config = mk_ob_cfg ~allow_groups:[ "group1" ] () in
  Alcotest.(check bool)
    "match" true
    (Onebot.is_allowed_group ~config ~group_id:"group1")

let test_group_allowed_no_match () =
  let config = mk_ob_cfg ~allow_groups:[ "group1" ] () in
  Alcotest.(check bool)
    "no match" false
    (Onebot.is_allowed_group ~config ~group_id:"group9")

(* --- extract_text tests --- *)

let test_extract_text_string () =
  let json = Yojson.Safe.from_string {|{"message":"hello world"}|} in
  match Onebot.extract_text json with
  | Some text -> Alcotest.(check string) "string format" "hello world" text
  | None -> Alcotest.fail "expected Some"

let test_extract_text_array () =
  let json =
    Yojson.Safe.from_string
      {|{"message":[{"type":"text","data":{"text":"hello "}},{"type":"text","data":{"text":"world"}}]}|}
  in
  match Onebot.extract_text json with
  | Some text -> Alcotest.(check string) "array format" "hello world" text
  | None -> Alcotest.fail "expected Some"

let test_extract_text_empty () =
  let json = Yojson.Safe.from_string {|{}|} in
  match Onebot.extract_text json with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None"

let test_extract_text_mixed_types () =
  let json =
    Yojson.Safe.from_string
      {|{"message":[{"type":"text","data":{"text":"hello"}},{"type":"image","data":{"url":"img.jpg"}}]}|}
  in
  match Onebot.extract_text json with
  | Some text -> Alcotest.(check string) "text only" "hello" text
  | None -> Alcotest.fail "expected Some"

(* --- parse_message_event tests --- *)

let test_parse_msg_private () =
  let json =
    Yojson.Safe.from_string
      {|{"post_type":"message","message_type":"private","user_id":123,"message":"hello"}|}
  in
  match Onebot.parse_message_event json with
  | Some (msg_type, user_id, group_id, text) ->
      Alcotest.(check string) "type" "private" msg_type;
      Alcotest.(check string) "user_id" "123" user_id;
      Alcotest.(check (option string)) "no group" None group_id;
      Alcotest.(check string) "text" "hello" text
  | None -> Alcotest.fail "expected Some"

let test_parse_msg_group () =
  let json =
    Yojson.Safe.from_string
      {|{"post_type":"message","message_type":"group","user_id":123,"group_id":456,"message":"hello"}|}
  in
  match Onebot.parse_message_event json with
  | Some (msg_type, _uid, group_id, _text) ->
      Alcotest.(check string) "type" "group" msg_type;
      Alcotest.(check (option string)) "group_id" (Some "456") group_id
  | None -> Alcotest.fail "expected Some"

let test_parse_msg_not_message () =
  let json = Yojson.Safe.from_string {|{"post_type":"notice"}|} in
  match Onebot.parse_message_event json with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None"

let test_parse_msg_empty_text () =
  let json =
    Yojson.Safe.from_string
      {|{"post_type":"message","message_type":"private","user_id":123}|}
  in
  match Onebot.parse_message_event json with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for empty text"

let suite =
  [
    Alcotest.test_case "user allowed wildcard" `Quick test_user_allowed_wildcard;
    Alcotest.test_case "user allowed match" `Quick test_user_allowed_match;
    Alcotest.test_case "user allowed no match" `Quick test_user_allowed_no_match;
    Alcotest.test_case "group allowed wildcard" `Quick
      test_group_allowed_wildcard;
    Alcotest.test_case "group allowed match" `Quick test_group_allowed_match;
    Alcotest.test_case "group allowed no match" `Quick
      test_group_allowed_no_match;
    Alcotest.test_case "extract text string" `Quick test_extract_text_string;
    Alcotest.test_case "extract text array" `Quick test_extract_text_array;
    Alcotest.test_case "extract text empty" `Quick test_extract_text_empty;
    Alcotest.test_case "extract text mixed types" `Quick
      test_extract_text_mixed_types;
    Alcotest.test_case "parse msg private" `Quick test_parse_msg_private;
    Alcotest.test_case "parse msg group" `Quick test_parse_msg_group;
    Alcotest.test_case "parse msg not message" `Quick test_parse_msg_not_message;
    Alcotest.test_case "parse msg empty text" `Quick test_parse_msg_empty_text;
  ]
