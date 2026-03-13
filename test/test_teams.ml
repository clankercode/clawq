let activity_json ~activity_type ~text ~activity_id ~service_url ~user_id
    ~user_name ~conversation_id ~team_id ?(is_group = false) () =
  let team_data =
    if team_id = "" then `Null
    else `Assoc [ ("team", `Assoc [ ("id", `String team_id) ]) ]
  in
  `Assoc
    [
      ("type", `String activity_type);
      ("id", `String activity_id);
      ("serviceUrl", `String service_url);
      ("text", `String text);
      ("from", `Assoc [ ("id", `String user_id); ("name", `String user_name) ]);
      ( "conversation",
        `Assoc [ ("id", `String conversation_id); ("isGroup", `Bool is_group) ]
      );
      ("channelData", team_data);
    ]
  |> Yojson.Safe.to_string

let test_parse_activity_returns_record () =
  let body =
    activity_json ~activity_type:"message" ~text:"hello" ~activity_id:"act-1"
      ~service_url:"https://svc" ~user_id:"u1" ~user_name:"Alice"
      ~conversation_id:"conv-1" ~team_id:"t1" ()
  in
  match Teams.parse_activity body with
  | None -> Alcotest.fail "expected Some"
  | Some a ->
      Alcotest.(check string) "activity_id" "act-1" a.activity_id;
      Alcotest.(check string) "service_url" "https://svc" a.service_url;
      Alcotest.(check string) "conversation_id" "conv-1" a.conversation_id;
      Alcotest.(check string) "user_id" "u1" a.user_id;
      Alcotest.(check string) "user_name" "Alice" a.user_name;
      Alcotest.(check string) "team_id" "t1" a.team_id;
      Alcotest.(check string) "text" "hello" a.text;
      Alcotest.(check bool) "is_group" false a.is_group

let test_parse_activity_non_message () =
  let body =
    activity_json ~activity_type:"typing" ~text:"hello" ~activity_id:"act-2"
      ~service_url:"https://svc" ~user_id:"u1" ~user_name:"Alice"
      ~conversation_id:"conv-1" ~team_id:"" ()
  in
  Alcotest.(check bool)
    "typing returns None" true
    (Teams.parse_activity body = None)

let test_parse_activity_empty_text () =
  let body =
    activity_json ~activity_type:"message" ~text:"" ~activity_id:"act-3"
      ~service_url:"https://svc" ~user_id:"u1" ~user_name:"Alice"
      ~conversation_id:"conv-1" ~team_id:"" ()
  in
  Alcotest.(check bool)
    "empty text returns None" true
    (Teams.parse_activity body = None)

let test_parse_activity_missing_from_name () =
  let body =
    `Assoc
      [
        ("type", `String "message");
        ("id", `String "act-4");
        ("serviceUrl", `String "https://svc");
        ("text", `String "hi");
        ("from", `Assoc [ ("id", `String "u1") ]);
        ("conversation", `Assoc [ ("id", `String "conv-1") ]);
        ("channelData", `Null);
      ]
    |> Yojson.Safe.to_string
  in
  match Teams.parse_activity body with
  | None -> Alcotest.fail "expected Some"
  | Some a ->
      Alcotest.(check string) "user_name defaults to empty" "" a.user_name

let test_session_key () =
  let key = Teams.session_key ~team_id:"t1" ~conversation_id:"conv-1" in
  Alcotest.(check string) "session key format" "teams:t1:conv-1" key

let test_session_key_personal () =
  let key = Teams.session_key ~team_id:"personal" ~conversation_id:"conv-abc" in
  Alcotest.(check string) "personal session key" "teams:personal:conv-abc" key

let test_strip_at_mentions () =
  let result = Teams.strip_at_mentions "<at>Bot</at> hello world" in
  Alcotest.(check string) "stripped mention" "hello world" result

let test_strip_at_mentions_multiple () =
  let result = Teams.strip_at_mentions "<at>Bot</at> hi <at>Other</at> there" in
  Alcotest.(check string) "stripped multiple" "hi  there" result

let test_strip_at_mentions_no_tags () =
  let result = Teams.strip_at_mentions "plain text" in
  Alcotest.(check string) "no tags unchanged" "plain text" result

let test_split_message_single () =
  let chunks = Teams.split_message "short message" in
  Alcotest.(check int) "one chunk" 1 (List.length chunks);
  Alcotest.(check string) "content" "short message" (List.hd chunks)

let test_parse_activity_group_chat () =
  let body =
    activity_json ~activity_type:"message" ~text:"hi" ~activity_id:"act-g"
      ~service_url:"https://svc" ~user_id:"u1" ~user_name:"Alice"
      ~conversation_id:"conv-g" ~team_id:"t1" ~is_group:true ()
  in
  match Teams.parse_activity body with
  | None -> Alcotest.fail "expected Some"
  | Some a ->
      Alcotest.(check bool) "is_group true" true a.is_group;
      Alcotest.(check string) "text" "hi" a.text

let test_build_reply_body_no_mention () =
  let body = Teams.build_reply_body ~alert:false ~text:"hello" ~mention:None in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "type" "message" (json |> member "type" |> to_string);
  Alcotest.(check string) "text" "hello" (json |> member "text" |> to_string);
  let alert =
    json |> member "channelData" |> member "notification" |> member "alert"
    |> to_bool
  in
  Alcotest.(check bool) "alert false" false alert;
  let entities = try json |> member "entities" |> to_list with _ -> [] in
  Alcotest.(check int) "no entities" 0 (List.length entities)

let test_build_reply_body_with_mention () =
  let mention =
    Some Teams.{ mention_id = "user-123"; mention_name = "Alice" }
  in
  let body = Teams.build_reply_body ~alert:true ~text:"hello" ~mention in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "text with mention" "<at>Alice</at> hello"
    (json |> member "text" |> to_string);
  (* alert=true includes channelData.notification.alert: true to force a toast *)
  let alert_val =
    json |> member "channelData" |> member "notification" |> member "alert"
    |> to_bool
  in
  Alcotest.(check bool) "channelData alert true" true alert_val;
  let entities = json |> member "entities" |> to_list in
  Alcotest.(check int) "one entity" 1 (List.length entities);
  let entity = List.hd entities in
  Alcotest.(check string)
    "entity type" "mention"
    (entity |> member "type" |> to_string);
  Alcotest.(check string)
    "mentioned id" "user-123"
    (entity |> member "mentioned" |> member "id" |> to_string);
  Alcotest.(check string)
    "mentioned name" "Alice"
    (entity |> member "mentioned" |> member "name" |> to_string);
  Alcotest.(check string)
    "entity text" "<at>Alice</at>"
    (entity |> member "text" |> to_string)

let test_split_message_multi () =
  let long = String.make (Teams.max_message_chars + 100) 'x' in
  let chunks = Teams.split_message long in
  Alcotest.(check bool) "multiple chunks" true (List.length chunks > 1)

let suite =
  [
    Alcotest.test_case "parse_activity returns record" `Quick
      test_parse_activity_returns_record;
    Alcotest.test_case "parse_activity non-message" `Quick
      test_parse_activity_non_message;
    Alcotest.test_case "parse_activity empty text" `Quick
      test_parse_activity_empty_text;
    Alcotest.test_case "parse_activity missing from.name" `Quick
      test_parse_activity_missing_from_name;
    Alcotest.test_case "session_key format" `Quick test_session_key;
    Alcotest.test_case "session_key personal" `Quick test_session_key_personal;
    Alcotest.test_case "strip_at_mentions" `Quick test_strip_at_mentions;
    Alcotest.test_case "strip_at_mentions multiple" `Quick
      test_strip_at_mentions_multiple;
    Alcotest.test_case "strip_at_mentions no tags" `Quick
      test_strip_at_mentions_no_tags;
    Alcotest.test_case "parse_activity group chat" `Quick
      test_parse_activity_group_chat;
    Alcotest.test_case "build_reply_body no mention" `Quick
      test_build_reply_body_no_mention;
    Alcotest.test_case "build_reply_body with mention" `Quick
      test_build_reply_body_with_mention;
    Alcotest.test_case "split_message single" `Quick test_split_message_single;
    Alcotest.test_case "split_message multi" `Quick test_split_message_multi;
  ]
