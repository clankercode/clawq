(* Tests for DingTalk channel module *)

let mk_dt_cfg ?(allow_from = [ "*" ]) () : Runtime_config.dingtalk_config =
  {
    app_key = "appkey1";
    app_secret = "appsecret1";
    agent_id = "agent1";
    allow_from;
    webhook_url = None;
    default_model = None;
  }

(* --- is_allowed tests --- *)

let test_is_allowed_wildcard () =
  let config = mk_dt_cfg () in
  Alcotest.(check bool)
    "wildcard" true
    (Dingtalk.is_allowed ~config ~sender_id:"any")

let test_is_allowed_match () =
  let config = mk_dt_cfg ~allow_from:[ "sender1" ] () in
  Alcotest.(check bool)
    "match" true
    (Dingtalk.is_allowed ~config ~sender_id:"sender1")

let test_is_allowed_no_match () =
  let config = mk_dt_cfg ~allow_from:[ "sender1" ] () in
  Alcotest.(check bool)
    "no match" false
    (Dingtalk.is_allowed ~config ~sender_id:"sender9")

(* --- compute_auth_sig tests --- *)

let test_auth_sig_non_empty () =
  let sig_ = Dingtalk.compute_auth_sig ~app_secret:"secret" ~timestamp:"1234" in
  Alcotest.(check bool) "non-empty" true (String.length sig_ > 0)

let test_auth_sig_deterministic () =
  let s1 = Dingtalk.compute_auth_sig ~app_secret:"secret" ~timestamp:"1234" in
  let s2 = Dingtalk.compute_auth_sig ~app_secret:"secret" ~timestamp:"1234" in
  Alcotest.(check string) "deterministic" s1 s2

let test_auth_sig_different_inputs () =
  let s1 = Dingtalk.compute_auth_sig ~app_secret:"secret1" ~timestamp:"1234" in
  let s2 = Dingtalk.compute_auth_sig ~app_secret:"secret2" ~timestamp:"1234" in
  Alcotest.(check bool) "different secrets -> different sigs" true (s1 <> s2)

let test_auth_sig_is_base64 () =
  let sig_ = Dingtalk.compute_auth_sig ~app_secret:"secret" ~timestamp:"1234" in
  (* Should be valid base64 *)
  match Base64.decode sig_ with
  | Ok _ -> ()
  | Error _ -> Alcotest.fail "expected valid base64"

(* --- parse_stream_message tests --- *)

let test_parse_stream_valid () =
  let data =
    Yojson.Safe.from_string
      {|{"conversationId":"conv1","senderId":"sender1","text":{"content":"hello"}}|}
  in
  match
    Dingtalk.parse_stream_message ~event_type:"im.message.receive_v1" data
  with
  | Some (conv_id, sender_id, content, _conversation_type) ->
      Alcotest.(check string) "conv_id" "conv1" conv_id;
      Alcotest.(check string) "sender_id" "sender1" sender_id;
      Alcotest.(check string) "content" "hello" content
  | None -> Alcotest.fail "expected Some"

let test_parse_stream_wrong_type () =
  let data = Yojson.Safe.from_string {|{}|} in
  match Dingtalk.parse_stream_message ~event_type:"other_event" data with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for wrong type"

let test_parse_stream_empty_content () =
  let data =
    Yojson.Safe.from_string
      {|{"conversationId":"conv1","senderId":"sender1","text":{"content":""}}|}
  in
  match
    Dingtalk.parse_stream_message ~event_type:"im.message.receive_v1" data
  with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for empty content"

let test_parse_stream_invalid () =
  match
    Dingtalk.parse_stream_message ~event_type:"im.message.receive_v1" `Null
  with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None"

let suite =
  [
    Alcotest.test_case "is_allowed wildcard" `Quick test_is_allowed_wildcard;
    Alcotest.test_case "is_allowed match" `Quick test_is_allowed_match;
    Alcotest.test_case "is_allowed no match" `Quick test_is_allowed_no_match;
    Alcotest.test_case "auth sig non-empty" `Quick test_auth_sig_non_empty;
    Alcotest.test_case "auth sig deterministic" `Quick
      test_auth_sig_deterministic;
    Alcotest.test_case "auth sig different inputs" `Quick
      test_auth_sig_different_inputs;
    Alcotest.test_case "auth sig is base64" `Quick test_auth_sig_is_base64;
    Alcotest.test_case "parse stream valid" `Quick test_parse_stream_valid;
    Alcotest.test_case "parse stream wrong type" `Quick
      test_parse_stream_wrong_type;
    Alcotest.test_case "parse stream empty content" `Quick
      test_parse_stream_empty_content;
    Alcotest.test_case "parse stream invalid" `Quick test_parse_stream_invalid;
  ]
