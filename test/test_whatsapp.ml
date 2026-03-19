(* Tests for WhatsApp channel module *)

let mk_wa_cfg ?(allow_from = [ "*" ]) () : Runtime_config.whatsapp_config =
  {
    phone_number_id = "12345";
    access_token = "tok";
    verify_token = "vtok";
    allow_from;
    default_model = None;
  }

(* --- is_allowed tests --- *)

let test_is_allowed_wildcard () =
  let config = mk_wa_cfg () in
  Alcotest.(check bool)
    "wildcard allows all" true
    (Whatsapp.is_allowed ~config ~from:"+1234")

let test_is_allowed_specific_match () =
  let config = mk_wa_cfg ~allow_from:[ "+1234"; "+5678" ] () in
  Alcotest.(check bool) "match" true (Whatsapp.is_allowed ~config ~from:"+1234")

let test_is_allowed_no_match () =
  let config = mk_wa_cfg ~allow_from:[ "+1234" ] () in
  Alcotest.(check bool)
    "no match" false
    (Whatsapp.is_allowed ~config ~from:"+9999")

(* --- parse_inbound_messages tests --- *)

let test_parse_inbound_valid () =
  let body =
    {|{"entry":[{"changes":[{"value":{"messages":[
    {"id":"msg1","from":"+1234","text":{"body":"hello"}}
  ]}}]}]}|}
  in
  let msgs = Whatsapp.parse_inbound_messages body in
  Alcotest.(check int) "1 message" 1 (List.length msgs);
  let _id, from, _group_jid, text = List.hd msgs in
  Alcotest.(check string) "from" "+1234" from;
  Alcotest.(check string) "text" "hello" text

let test_parse_inbound_empty_entry () =
  let msgs = Whatsapp.parse_inbound_messages {|{"entry":[]}|} in
  Alcotest.(check int) "no messages" 0 (List.length msgs)

let test_parse_inbound_invalid () =
  let msgs = Whatsapp.parse_inbound_messages "bad json" in
  Alcotest.(check int) "no messages for invalid" 0 (List.length msgs)

let test_parse_inbound_no_text () =
  let body =
    {|{"entry":[{"changes":[{"value":{"messages":[
    {"id":"msg1","from":"+1234","text":{"body":""}}
  ]}}]}]}|}
  in
  let msgs = Whatsapp.parse_inbound_messages body in
  Alcotest.(check int) "skip empty text" 0 (List.length msgs)

let test_parse_inbound_no_messages () =
  let body = {|{"entry":[{"changes":[{"value":{}}]}]}|} in
  let msgs = Whatsapp.parse_inbound_messages body in
  Alcotest.(check int) "no messages key" 0 (List.length msgs)

(* --- handle_verify tests --- *)

let test_verify_valid () =
  let config = mk_wa_cfg () in
  let uri =
    Uri.of_string
      "/?hub.mode=subscribe&hub.verify_token=vtok&hub.challenge=test123"
  in
  match Whatsapp.handle_verify ~config uri with
  | Some challenge -> Alcotest.(check string) "challenge" "test123" challenge
  | None -> Alcotest.fail "expected Some"

let test_verify_wrong_token () =
  let config = mk_wa_cfg () in
  let uri =
    Uri.of_string
      "/?hub.mode=subscribe&hub.verify_token=wrong&hub.challenge=test"
  in
  match Whatsapp.handle_verify ~config uri with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for wrong token"

let test_verify_wrong_mode () =
  let config = mk_wa_cfg () in
  let uri =
    Uri.of_string
      "/?hub.mode=unsubscribe&hub.verify_token=vtok&hub.challenge=test"
  in
  match Whatsapp.handle_verify ~config uri with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for wrong mode"

let test_verify_missing_params () =
  let config = mk_wa_cfg () in
  let uri = Uri.of_string "/" in
  match Whatsapp.handle_verify ~config uri with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for missing params"

(* --- dedup_seen tests --- *)

let test_dedup_first_time () =
  let id = "wa-dedup-" ^ string_of_float (Unix.gettimeofday ()) in
  Alcotest.(check bool) "first time not seen" false (Whatsapp.dedup_seen id)

let test_dedup_second_time () =
  let id = "wa-dedup2-" ^ string_of_float (Unix.gettimeofday ()) in
  ignore (Whatsapp.dedup_seen id);
  Alcotest.(check bool) "second time seen" true (Whatsapp.dedup_seen id)

let suite =
  [
    Alcotest.test_case "is_allowed wildcard" `Quick test_is_allowed_wildcard;
    Alcotest.test_case "is_allowed match" `Quick test_is_allowed_specific_match;
    Alcotest.test_case "is_allowed no match" `Quick test_is_allowed_no_match;
    Alcotest.test_case "parse inbound valid" `Quick test_parse_inbound_valid;
    Alcotest.test_case "parse inbound empty entry" `Quick
      test_parse_inbound_empty_entry;
    Alcotest.test_case "parse inbound invalid" `Quick test_parse_inbound_invalid;
    Alcotest.test_case "parse inbound no text" `Quick test_parse_inbound_no_text;
    Alcotest.test_case "parse inbound no messages" `Quick
      test_parse_inbound_no_messages;
    Alcotest.test_case "verify valid" `Quick test_verify_valid;
    Alcotest.test_case "verify wrong token" `Quick test_verify_wrong_token;
    Alcotest.test_case "verify wrong mode" `Quick test_verify_wrong_mode;
    Alcotest.test_case "verify missing params" `Quick test_verify_missing_params;
    Alcotest.test_case "dedup first time" `Quick test_dedup_first_time;
    Alcotest.test_case "dedup second time" `Quick test_dedup_second_time;
  ]
