let check_origin =
  let pp fmt (o : Room_origin.t) =
    Format.fprintf fmt
      "{connector=%s; workspace=%s; room=%s; req_id=%s; req_name=%s; \
       src_msg=%s; thread=%s; svc_url=%s; profile=%s}"
      (match o.connector with Some s -> s | None -> "-")
      (match o.workspace_id with Some s -> s | None -> "-")
      (match o.room_id with Some s -> s | None -> "-")
      (match o.requester_id with Some s -> s | None -> "-")
      (match o.requester_name with Some s -> s | None -> "-")
      (match o.source_message_id with Some s -> s | None -> "-")
      (match o.thread_id with Some s -> s | None -> "-")
      (match o.service_url with Some s -> s | None -> "-")
      (match o.profile_id with Some n -> string_of_int n | None -> "-")
  in
  Alcotest.testable pp ( = )

let contains_sub s sub =
  let sl = String.length s in
  let subl = String.length sub in
  let rec go i =
    if i + subl > sl then false
    else if String.sub s i subl = sub then true
    else go (i + 1)
  in
  subl = 0 || go 0

let test_make_with_all_fields () =
  let o =
    Room_origin.make ~connector:"slack" ~workspace_id:"T01" ~room_id:"C02"
      ~requester_id:"U03" ~requester_name:"Alice" ~source_message_id:"M04"
      ~thread_id:"T05" ~service_url:"https://slack.example.com" ~profile_id:42
      ()
  in
  Alcotest.(check (option string)) "connector" (Some "slack") o.connector;
  Alcotest.(check (option string)) "workspace_id" (Some "T01") o.workspace_id;
  Alcotest.(check (option string)) "room_id" (Some "C02") o.room_id;
  Alcotest.(check (option string)) "requester_id" (Some "U03") o.requester_id;
  Alcotest.(check (option string)) "requester_name" (Some "Alice")
    o.requester_name;
  Alcotest.(check (option string)) "source_message_id" (Some "M04")
    o.source_message_id;
  Alcotest.(check (option string)) "thread_id" (Some "T05") o.thread_id;
  Alcotest.(check (option string)) "service_url"
    (Some "https://slack.example.com") o.service_url;
  Alcotest.(check (option int)) "profile_id" (Some 42) o.profile_id

let test_make_partial () =
  let o = Room_origin.make ~connector:"discord" ~room_id:"chan-1" () in
  Alcotest.(check (option string)) "connector" (Some "discord") o.connector;
  Alcotest.(check (option string)) "workspace_id" None o.workspace_id;
  Alcotest.(check (option string)) "room_id" (Some "chan-1") o.room_id

let test_empty () =
  let o = Room_origin.empty in
  Alcotest.check check_origin "empty" Room_origin.empty o;
  Alcotest.(check bool) "is_empty" true (Room_origin.is_empty o)

let test_is_empty_false () =
  let o = Room_origin.make ~connector:"slack" () in
  Alcotest.(check bool) "not empty" false (Room_origin.is_empty o)

let test_to_json_roundtrip () =
  let o =
    Room_origin.make ~connector:"teams" ~workspace_id:"team-9"
      ~room_id:"conv-abc" ~requester_id:"user-1" ~requester_name:"Bob"
      ~source_message_id:"msg-7" ~thread_id:"thr-3"
      ~service_url:"https://teams.microsoft.com" ~profile_id:7 ()
  in
  let json_str = Room_origin.to_json_string o in
  match Room_origin.of_json_string json_str with
  | Error msg -> Alcotest.failf "roundtrip parse failed: %s" msg
  | Ok parsed ->
      Alcotest.(check (option string)) "connector" o.connector parsed.connector;
      Alcotest.(check (option string)) "workspace_id" o.workspace_id
        parsed.workspace_id;
      Alcotest.(check (option string)) "room_id" o.room_id parsed.room_id;
      Alcotest.(check (option string)) "requester_id" o.requester_id
        parsed.requester_id;
      Alcotest.(check (option string)) "requester_name" o.requester_name
        parsed.requester_name;
      Alcotest.(check (option string)) "source_message_id"
        o.source_message_id parsed.source_message_id;
      Alcotest.(check (option string)) "thread_id" o.thread_id parsed.thread_id;
      Alcotest.(check (option string)) "service_url" o.service_url
        parsed.service_url;
      Alcotest.(check (option int)) "profile_id" o.profile_id parsed.profile_id

let test_compact_json_omits_none () =
  let o = Room_origin.make ~connector:"telegram" ~room_id:"123" () in
  let json = Room_origin.to_compact_json o in
  let str = Yojson.Safe.to_string json in
  Alcotest.(check bool) "no workspace_id" false
    (contains_sub str "workspace_id");
  Alcotest.(check bool) "no requester_id" false
    (contains_sub str "requester_id");
  Alcotest.(check bool) "has connector" true (contains_sub str "connector")

let test_compact_json_includes_set () =
  let o =
    Room_origin.make ~connector:"slack" ~workspace_id:"T01" ~room_id:"C02"
      ~profile_id:9 ()
  in
  let str = Room_origin.to_compact_json_string o in
  Alcotest.(check bool) "has workspace_id" true (contains_sub str "T01");
  Alcotest.(check bool) "has profile_id" true (contains_sub str "9")

let test_of_json_null () =
  match Room_origin.of_json `Null with
  | Error msg -> Alcotest.failf "null parse failed: %s" msg
  | Ok o -> Alcotest.(check bool) "null is empty" true (Room_origin.is_empty o)

let test_of_json_string_error () =
  match Room_origin.of_json (`String "bad") with
  | Error _ -> () (* expected *)
  | Ok _ -> Alcotest.fail "expected error for non-object JSON"

let test_of_json_string_roundtrip () =
  let o = Room_origin.make ~connector:"discord" ~requester_id:"U99" () in
  let s = Room_origin.to_json_string o in
  match Room_origin.of_json_string s with
  | Error msg -> Alcotest.failf "string roundtrip failed: %s" msg
  | Ok parsed ->
      Alcotest.(check (option string)) "connector" o.connector parsed.connector;
      Alcotest.(check (option string)) "requester_id" o.requester_id
        parsed.requester_id

let test_of_json_string_opt_error () =
  Alcotest.check
    (Alcotest.option check_origin)
    "invalid string returns None" None
    (Room_origin.of_json_string_opt "not-json")

let test_of_json_string_opt_ok () =
  let o = Room_origin.make ~connector:"web" () in
  let s = Room_origin.to_json_string o in
  match Room_origin.of_json_string_opt s with
  | None -> Alcotest.fail "expected Some for valid JSON"
  | Some parsed ->
      Alcotest.(check (option string)) "connector" o.connector parsed.connector

let test_from_room_session () =
  let session : Room_session.session =
    { channel = Slack; kind = Room; channel_id = "C01"; sender_id = "U01" }
  in
  let o =
    Room_origin.from_room_session ~workspace_id:"T99" ~thread_id:"thr-1"
      ~service_url:"https://slack.example.com" ~profile_id:5 session
  in
  Alcotest.(check (option string)) "connector" (Some "slack") o.connector;
  Alcotest.(check (option string)) "workspace_id" (Some "T99") o.workspace_id;
  Alcotest.(check (option string)) "room_id" (Some "C01") o.room_id;
  Alcotest.(check (option string)) "requester_id" (Some "U01") o.requester_id;
  Alcotest.(check (option string)) "thread_id" (Some "thr-1") o.thread_id;
  Alcotest.(check (option string)) "service_url"
    (Some "https://slack.example.com") o.service_url;
  Alcotest.(check (option int)) "profile_id" (Some 5) o.profile_id

let test_from_room_session_minimal () =
  let session : Room_session.session =
    { channel = Discord; kind = Room; channel_id = "ch-1"; sender_id = "u-2" }
  in
  let o = Room_origin.from_room_session session in
  Alcotest.(check (option string)) "connector" (Some "discord") o.connector;
  Alcotest.(check (option string)) "workspace_id" None o.workspace_id;
  Alcotest.(check (option string)) "room_id" (Some "ch-1") o.room_id;
  Alcotest.(check (option string)) "requester_id" (Some "u-2") o.requester_id

let test_display_summary () =
  let o =
    Room_origin.make ~connector:"slack" ~room_id:"C01" ~requester_name:"Alice"
      ()
  in
  let expected = "Slack room=C01 requester=Alice" in
  Alcotest.(check string) "display summary" expected
    (Room_origin.display_summary o)

let test_display_summary_none () =
  let expected = "CLI room=- requester=-" in
  Alcotest.(check string) "display summary empty" expected
    (Room_origin.display_summary Room_origin.empty)

let test_display_summary_requester_id_fallback () =
  let o =
    Room_origin.make ~connector:"telegram" ~room_id:"123" ~requester_id:"U01"
      ()
  in
  Alcotest.(check string) "requester id fallback"
    "Telegram room=123 requester=U01"
    (Room_origin.display_summary o)

let suite =
  [
    Alcotest.test_case "make with all fields" `Quick test_make_with_all_fields;
    Alcotest.test_case "make partial" `Quick test_make_partial;
    Alcotest.test_case "empty and is_empty" `Quick test_empty;
    Alcotest.test_case "is_empty false" `Quick test_is_empty_false;
    Alcotest.test_case "to_json roundtrip" `Quick test_to_json_roundtrip;
    Alcotest.test_case "compact json omits none" `Quick
      test_compact_json_omits_none;
    Alcotest.test_case "compact json includes set" `Quick
      test_compact_json_includes_set;
    Alcotest.test_case "of_json null" `Quick test_of_json_null;
    Alcotest.test_case "of_json string error" `Quick test_of_json_string_error;
    Alcotest.test_case "of_json string roundtrip" `Quick
      test_of_json_string_roundtrip;
    Alcotest.test_case "of_json_string_opt error" `Quick
      test_of_json_string_opt_error;
    Alcotest.test_case "of_json_string_opt ok" `Quick
      test_of_json_string_opt_ok;
    Alcotest.test_case "from_room_session" `Quick test_from_room_session;
    Alcotest.test_case "from_room_session minimal" `Quick
      test_from_room_session_minimal;
    Alcotest.test_case "display summary" `Quick test_display_summary;
    Alcotest.test_case "display summary none" `Quick test_display_summary_none;
    Alcotest.test_case "display summary requester_id fallback" `Quick
      test_display_summary_requester_id_fallback;
  ]
