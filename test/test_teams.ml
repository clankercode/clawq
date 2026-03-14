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
  let body =
    Teams.build_reply_body ~alert:false ~text:"hello" ~mention:None
      ~mention_mode:"entity"
  in
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
  let body =
    Teams.build_reply_body ~alert:true ~text:"hello" ~mention
      ~mention_mode:"entity"
  in
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

let test_build_reply_body_text_mention () =
  let mention =
    Some Teams.{ mention_id = "user-123"; mention_name = "Alice" }
  in
  let body =
    Teams.build_reply_body ~alert:false ~text:"hello" ~mention
      ~mention_mode:"text"
  in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "text with fake at" "@Alice hello"
    (json |> member "text" |> to_string);
  let entities = try json |> member "entities" |> to_list with _ -> [] in
  Alcotest.(check int) "no entities for text mode" 0 (List.length entities)

let test_build_reply_body_none_mode () =
  (* mention_mode "none" is handled by passing mention:None from handle_webhook,
     so build_reply_body with mention:None should produce no prefix regardless
     of mode *)
  let body =
    Teams.build_reply_body ~alert:false ~text:"hello" ~mention:None
      ~mention_mode:"none"
  in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "no prefix" "hello"
    (json |> member "text" |> to_string)

let test_split_message_multi () =
  let long = String.make (Teams.max_message_chars + 100) 'x' in
  let chunks = Teams.split_message long in
  Alcotest.(check bool) "multiple chunks" true (List.length chunks > 1)

let test_encode_decode_channel_id () =
  let service_url = "https://smba.trafficmanager.net/amer/" in
  let conversation_id = "19:3ed169b9@thread.v2" in
  let encoded = Teams.encode_channel_id ~service_url ~conversation_id in
  let decoded_svc, decoded_conv = Teams.decode_channel_id encoded in
  Alcotest.(check string) "service_url roundtrip" service_url decoded_svc;
  Alcotest.(check string)
    "conversation_id roundtrip" conversation_id decoded_conv

let test_decode_channel_id_with_pipe_in_conversation_id () =
  (* Pipe delimiter: service_url is up to the first |; rest is conversation_id *)
  let service_url = "https://smba.trafficmanager.net/amer/" in
  let conversation_id = "19:abc|def@thread.v2" in
  let encoded = Teams.encode_channel_id ~service_url ~conversation_id in
  let decoded_svc, decoded_conv = Teams.decode_channel_id encoded in
  Alcotest.(check string)
    "service_url with pipe in conv" service_url decoded_svc;
  Alcotest.(check string)
    "conversation_id preserved" conversation_id decoded_conv

let test_encode_channel_id_format () =
  let encoded =
    Teams.encode_channel_id ~service_url:"https://svc" ~conversation_id:"conv-1"
  in
  Alcotest.(check string) "pipe-delimited" "https://svc|conv-1" encoded

let test_slash_command_recognized_after_mention_strip () =
  let stripped = Teams.strip_at_mentions "<at>Bot</at> /help" in
  Alcotest.(check string) "stripped to /help" "/help" stripped;
  match Slash_commands.handle stripped with
  | Slash_commands.Help ->
      let text = Slash_commands.format_help ~connector:Format_adapter.Teams in
      Alcotest.(check bool)
        "help stays multiline" true
        (String.contains text '\n')
  | _ -> Alcotest.fail "expected Help from /help"

let test_help_reply_body_uses_markdown_table () =
  let help_text = Slash_commands.format_help ~connector:Format_adapter.Teams in
  let body =
    Teams.build_reply_body ~alert:false ~text:help_text ~mention:None
      ~mention_mode:"entity"
  in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let text = json |> member "text" |> to_string in
  Alcotest.(check bool)
    "help body stays multiline" true
    (String.contains text '\n');
  Alcotest.(check bool)
    "help body contains markdown table" true
    (try
       ignore
         (Str.search_forward
            (Str.regexp_string "| Command | Description |")
            text 0);
       true
     with Not_found -> false)

let test_slash_new_after_mention_strip () =
  let stripped = Teams.strip_at_mentions "<at>Bot</at> /new" in
  Alcotest.(check string) "stripped to /new" "/new" stripped;
  match Slash_commands.handle stripped with
  | Slash_commands.Reset -> ()
  | _ -> Alcotest.fail "expected Reset from /new"

let test_build_reply_body_includes_text_format () =
  let body =
    Teams.build_reply_body ~alert:false ~text:"hello" ~mention:None
      ~mention_mode:"entity"
  in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "textFormat is markdown" "markdown"
    (json |> member "textFormat" |> to_string)

let test_build_reply_body_normalizes_tables () =
  let llm_text =
    "Here are results\n| Name | Score |\n| Alice | 95 |\n| Bob | 87 |\nDone."
  in
  let body =
    Teams.build_reply_body ~alert:false ~text:llm_text ~mention:None
      ~mention_mode:"entity"
  in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let text = json |> member "text" |> to_string in
  Alcotest.(check bool)
    "separator row inserted" true
    (try
       ignore (Str.search_forward (Str.regexp_string "| --- | --- |") text 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "blank line before table" true
    (try
       ignore (Str.search_forward (Str.regexp_string "results\n\n|") text 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "blank line after table" true
    (try
       ignore (Str.search_forward (Str.regexp_string "|\n\nDone") text 0);
       true
     with Not_found -> false)

let test_not_slash_after_mention_strip () =
  let stripped = Teams.strip_at_mentions "<at>Bot</at> hello world" in
  match Slash_commands.handle stripped with
  | Slash_commands.NotACommand -> ()
  | _ -> Alcotest.fail "expected NotACommand for normal message"

let test_build_attachment_upload_body () =
  let body =
    Teams.build_attachment_upload_body ~filename:"test.json"
      ~content_type:"application/json" ~content:"hello world"
  in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "type" "application/json"
    (json |> member "type" |> to_string);
  Alcotest.(check string) "name" "test.json" (json |> member "name" |> to_string);
  let b64 = json |> member "originalBase64" |> to_string in
  Alcotest.(check string)
    "base64 roundtrip" "hello world" (Base64.decode_exn b64)

let test_build_message_with_attachment () =
  let body =
    Teams.build_message_with_attachment ~filename:"dump.json"
      ~content_type:"application/json"
      ~content_url:"https://svc/v3/attachments/att-1/views/original"
  in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "type" "message" (json |> member "type" |> to_string);
  Alcotest.(check string) "text" "dump.json" (json |> member "text" |> to_string);
  let attachments = json |> member "attachments" |> to_list in
  Alcotest.(check int) "one attachment" 1 (List.length attachments);
  let att = List.hd attachments in
  Alcotest.(check string)
    "contentType" "application/json"
    (att |> member "contentType" |> to_string);
  Alcotest.(check string)
    "contentUrl" "https://svc/v3/attachments/att-1/views/original"
    (att |> member "contentUrl" |> to_string);
  Alcotest.(check string) "name" "dump.json" (att |> member "name" |> to_string)

let test_debug_dump_filename_sanitization () =
  let safe_key =
    String.map
      (fun c ->
        match c with
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' -> c
        | _ -> '_')
      "teams:personal:19:abc@thread.v2"
  in
  Alcotest.(check string)
    "sanitized key" "teams_personal_19_abc_thread_v2" safe_key;
  let filename = Printf.sprintf "session_%s_%d.json" safe_key 1710500000 in
  Alcotest.(check bool)
    "filename has no special chars" true
    (String.to_seq filename
    |> Seq.for_all (fun c ->
        match c with
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '.' -> true
        | _ -> false))

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
    Alcotest.test_case "build_reply_body text mention" `Quick
      test_build_reply_body_text_mention;
    Alcotest.test_case "build_reply_body none mode" `Quick
      test_build_reply_body_none_mode;
    Alcotest.test_case "split_message single" `Quick test_split_message_single;
    Alcotest.test_case "split_message multi" `Quick test_split_message_multi;
    Alcotest.test_case "encode_decode channel_id roundtrip" `Quick
      test_encode_decode_channel_id;
    Alcotest.test_case "decode channel_id with pipe in conversation_id" `Quick
      test_decode_channel_id_with_pipe_in_conversation_id;
    Alcotest.test_case "encode_channel_id format" `Quick
      test_encode_channel_id_format;
    Alcotest.test_case "slash /help recognized after mention strip" `Quick
      test_slash_command_recognized_after_mention_strip;
    Alcotest.test_case "help reply body uses markdown table" `Quick
      test_help_reply_body_uses_markdown_table;
    Alcotest.test_case "slash /new recognized after mention strip" `Quick
      test_slash_new_after_mention_strip;
    Alcotest.test_case "build_reply_body includes textFormat" `Quick
      test_build_reply_body_includes_text_format;
    Alcotest.test_case "normal message not a slash command" `Quick
      test_not_slash_after_mention_strip;
    Alcotest.test_case "build_reply_body normalizes tables" `Quick
      test_build_reply_body_normalizes_tables;
    Alcotest.test_case "build_attachment_upload_body" `Quick
      test_build_attachment_upload_body;
    Alcotest.test_case "build_message_with_attachment" `Quick
      test_build_message_with_attachment;
    Alcotest.test_case "debug dump filename sanitization" `Quick
      test_debug_dump_filename_sanitization;
  ]
