let mention_entity ~id ~name =
  `Assoc
    [
      ("type", `String "mention");
      ("mentioned", `Assoc [ ("id", `String id); ("name", `String name) ]);
      ("text", `String (Printf.sprintf "<at>%s</at>" name));
    ]

let activity_json ~activity_type ~text ~activity_id ~service_url ~user_id
    ~user_name ~conversation_id ~team_id ?(is_group = false) ?(entities = [])
    ?(attachments = []) () =
  let team_data =
    if team_id = "" then `Null
    else `Assoc [ ("team", `Assoc [ ("id", `String team_id) ]) ]
  in
  let fields =
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
  in
  let fields =
    if entities = [] then fields else fields @ [ ("entities", `List entities) ]
  in
  let fields =
    if attachments = [] then fields
    else fields @ [ ("attachments", `List attachments) ]
  in
  `Assoc fields |> Yojson.Safe.to_string

let test_teams_config () : Runtime_config.teams_config =
  {
    app_id = "test-app";
    app_secret = "test-secret";
    tenant_id = "test-tenant";
    webhook_path = "/teams/webhook";
    service_url = "https://smba.trafficmanager.net/amer";
    allow_teams = [ "*" ];
    allow_users = [ "*" ];
    mention_mode = "entity";
    file_consent_cards = true;
  }

let base64url_encode s =
  Base64.encode_string s |> String.to_seq |> List.of_seq
  |> List.filter (fun c -> c <> '=')
  |> List.map (function '+' -> '-' | '/' -> '_' | c -> c)
  |> List.to_seq |> String.of_seq

let bearer_for_config (config : Runtime_config.teams_config) =
  let now = int_of_float (Unix.gettimeofday ()) in
  let header = {|{"alg":"none","typ":"JWT"}|} |> base64url_encode in
  let payload =
    Printf.sprintf
      {|{"aud":"%s","iss":"https://api.botframework.com","exp":%d,"nbf":%d}|}
      config.app_id (now + 3600) (now - 60)
    |> base64url_encode
  in
  "Bearer " ^ header ^ "." ^ payload ^ ".sig"

let check_ok_invoke_response ~msg body_str =
  let json = Yojson.Safe.from_string body_str in
  let open Yojson.Safe.Util in
  Alcotest.(check int) (msg ^ " status") 200 (json |> member "status" |> to_int);
  Alcotest.(check bool)
    (msg ^ " body is object") true
    (match json |> member "body" with `Assoc [] -> true | _ -> false)

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

let test_select_file_upload_delivery_personal_chat () =
  Alcotest.(check bool)
    "personal 1:1 uses consent card" true
    (match
       Teams.select_file_upload_delivery ~file_consent_cards:true ~team_id:""
         ~is_group:false
     with
    | Teams.File_consent_card -> true
    | Teams.Temp_download_url -> false)

let test_select_file_upload_delivery_group_chat () =
  Alcotest.(check bool)
    "group chat falls back" true
    (match
       Teams.select_file_upload_delivery ~file_consent_cards:true ~team_id:""
         ~is_group:true
     with
    | Teams.Temp_download_url -> true
    | Teams.File_consent_card -> false)

let test_select_file_upload_delivery_team_channel () =
  Alcotest.(check bool)
    "team channel falls back" true
    (match
       Teams.select_file_upload_delivery ~file_consent_cards:true ~team_id:"t1"
         ~is_group:true
     with
    | Teams.Temp_download_url -> true
    | Teams.File_consent_card -> false)

let test_select_file_upload_delivery_disabled () =
  Alcotest.(check bool)
    "disabled → temp download" true
    (Teams.select_file_upload_delivery ~file_consent_cards:false ~team_id:""
       ~is_group:false
    = Teams.Temp_download_url)

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

let test_temp_downloads_add_and_get () =
  let token =
    Temp_downloads.add ~content:"test content" ~content_type:"text/plain"
      ~filename:"test.txt" ~ttl_s:60.0
  in
  Alcotest.(check bool) "token is non-empty" true (String.length token > 0);
  match Temp_downloads.get token with
  | None -> Alcotest.fail "expected Some entry"
  | Some entry ->
      Alcotest.(check string) "content" "test content" entry.content;
      Alcotest.(check string) "content_type" "text/plain" entry.content_type;
      Alcotest.(check string) "filename" "test.txt" entry.filename

let test_temp_downloads_expired () =
  let token =
    Temp_downloads.add ~content:"old" ~content_type:"text/plain"
      ~filename:"old.txt" ~ttl_s:0.0
  in
  Unix.sleepf 0.01;
  Alcotest.(check bool)
    "expired entry is None" true
    (Temp_downloads.get token = None)

let test_temp_downloads_missing () =
  Alcotest.(check bool)
    "missing token is None" true
    (Temp_downloads.get "nonexistent-token" = None)

let test_temp_downloads_url_with_base () =
  let old_base = !Temp_downloads.public_base_url in
  Temp_downloads.public_base_url := Some "https://example.com";
  let url = Temp_downloads.download_url "abc123" in
  Temp_downloads.public_base_url := old_base;
  Alcotest.(check (option string))
    "url with base" (Some "https://example.com/downloads/abc123") url

let test_temp_downloads_url_trailing_slash () =
  let old_base = !Temp_downloads.public_base_url in
  Temp_downloads.public_base_url := Some "https://example.com/";
  let url = Temp_downloads.download_url "abc123" in
  Temp_downloads.public_base_url := old_base;
  Alcotest.(check (option string))
    "url strips trailing slash" (Some "https://example.com/downloads/abc123")
    url

let test_temp_downloads_url_no_base () =
  let old_base = !Temp_downloads.public_base_url in
  Temp_downloads.public_base_url := None;
  let url = Temp_downloads.download_url "abc123" in
  Temp_downloads.public_base_url := old_base;
  Alcotest.(check (option string)) "no base returns None" None url

let test_temp_downloads_cleanup () =
  let _live =
    Temp_downloads.add ~content:"live" ~content_type:"text/plain"
      ~filename:"live.txt" ~ttl_s:60.0
  in
  let expired =
    Temp_downloads.add ~content:"expired" ~content_type:"text/plain"
      ~filename:"expired.txt" ~ttl_s:0.0
  in
  Unix.sleepf 0.01;
  Temp_downloads.cleanup ();
  Alcotest.(check bool)
    "expired removed by cleanup" true
    (Temp_downloads.get expired = None)

let test_build_file_consent_card () =
  let body =
    Teams.build_file_consent_card ~filename:"dump.json"
      ~description:"Session dump" ~size_bytes:12345 ~consent_id:"abc123"
  in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "type" "message" (json |> member "type" |> to_string);
  let attachments = json |> member "attachments" |> to_list in
  Alcotest.(check int) "one attachment" 1 (List.length attachments);
  let att = List.hd attachments in
  Alcotest.(check string)
    "contentType" "application/vnd.microsoft.teams.card.file.consent"
    (att |> member "contentType" |> to_string);
  Alcotest.(check string) "name" "dump.json" (att |> member "name" |> to_string);
  let content = att |> member "content" in
  Alcotest.(check string)
    "description" "Session dump"
    (content |> member "description" |> to_string);
  Alcotest.(check int)
    "sizeInBytes" 12345
    (content |> member "sizeInBytes" |> to_int);
  let accept_ctx = content |> member "acceptContext" in
  Alcotest.(check string)
    "acceptContext consentId" "abc123"
    (accept_ctx |> member "consentId" |> to_string);
  let decline_ctx = content |> member "declineContext" in
  Alcotest.(check string)
    "declineContext consentId" "abc123"
    (decline_ctx |> member "consentId" |> to_string)

let test_build_file_info_card () =
  let body =
    Teams.build_file_info_card ~filename:"dump.json"
      ~content_url:"https://onedrive/file" ~unique_id:"uid-1" ~file_type:"json"
  in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "type" "message" (json |> member "type" |> to_string);
  let attachments = json |> member "attachments" |> to_list in
  let att = List.hd attachments in
  Alcotest.(check string)
    "contentType" "application/vnd.microsoft.teams.card.file.info"
    (att |> member "contentType" |> to_string);
  Alcotest.(check string)
    "contentUrl" "https://onedrive/file"
    (att |> member "contentUrl" |> to_string);
  let content = att |> member "content" in
  Alcotest.(check string)
    "uniqueId" "uid-1"
    (content |> member "uniqueId" |> to_string);
  Alcotest.(check string)
    "fileType" "json"
    (content |> member "fileType" |> to_string)

let test_pending_consent_store_and_get () =
  let consent_id =
    Teams.store_pending_consent ~content:"test data" ~filename:"test.json"
      ~content_type:"application/json" ~ttl_s:60.0
  in
  Alcotest.(check bool) "id non-empty" true (String.length consent_id > 0);
  match Teams.get_pending_consent consent_id with
  | None -> Alcotest.fail "expected Some"
  | Some entry ->
      Alcotest.(check string) "content" "test data" entry.content;
      Alcotest.(check string) "filename" "test.json" entry.filename

let test_pending_consent_expired () =
  let consent_id =
    Teams.store_pending_consent ~content:"old" ~filename:"old.json"
      ~content_type:"application/json" ~ttl_s:0.0
  in
  Unix.sleepf 0.01;
  Alcotest.(check bool)
    "expired is None" true
    (Teams.get_pending_consent consent_id = None)

let test_pending_consent_consumed_on_get () =
  let consent_id =
    Teams.store_pending_consent ~content:"once" ~filename:"once.json"
      ~content_type:"application/json" ~ttl_s:60.0
  in
  ignore (Teams.get_pending_consent consent_id);
  Alcotest.(check bool)
    "second get is None" true
    (Teams.get_pending_consent consent_id = None)

let test_pending_consent_cleanup () =
  let _live =
    Teams.store_pending_consent ~content:"live" ~filename:"live.json"
      ~content_type:"application/json" ~ttl_s:60.0
  in
  let expired =
    Teams.store_pending_consent ~content:"expired" ~filename:"exp.json"
      ~content_type:"application/json" ~ttl_s:0.0
  in
  Unix.sleepf 0.01;
  Teams.cleanup_pending_consents ();
  Alcotest.(check bool)
    "expired removed by cleanup" true
    (Teams.get_pending_consent expired = None)

let test_file_consent_invoke_returns_immediately () =
  (* Store a pending consent so handle_file_consent_invoke finds it *)
  let consent_id =
    Teams.store_pending_consent ~content:"file data" ~filename:"test.json"
      ~content_type:"application/json" ~ttl_s:60.0
  in
  let config = test_teams_config () in
  let invoke_json =
    `Assoc
      [
        ("type", `String "invoke");
        ("name", `String "fileConsent/invoke");
        ("serviceUrl", `String "https://smba.trafficmanager.net/amer");
        ("conversation", `Assoc [ ("id", `String "conv-test") ]);
        ( "value",
          `Assoc
            [
              ("action", `String "accept");
              ("context", `Assoc [ ("consentId", `String consent_id) ]);
              ( "uploadInfo",
                `Assoc
                  [
                    (* Use a fake URL — the background upload will fail,
                       but the invoke response should return immediately *)
                    ("uploadUrl", `String "https://fake-onedrive/upload");
                    ("contentUrl", `String "https://fake-onedrive/file");
                    ("uniqueId", `String "uid-1");
                    ("fileType", `String "json");
                  ] );
            ] );
      ]
  in
  let response =
    Lwt_main.run (Teams.handle_file_consent_invoke ~config invoke_json)
  in
  Alcotest.(check int) "invoke status code" 200 response.status_code;
  check_ok_invoke_response ~msg:"invoke response is immediate 200"
    (Teams.invoke_response_body response)

let test_file_consent_invoke_expired_returns_200 () =
  (* With no pending consent, should still return 200 immediately *)
  let config = test_teams_config () in
  let invoke_json =
    `Assoc
      [
        ("type", `String "invoke");
        ("name", `String "fileConsent/invoke");
        ("serviceUrl", `String "https://smba.trafficmanager.net/amer");
        ("conversation", `Assoc [ ("id", `String "conv-test") ]);
        ( "value",
          `Assoc
            [
              ("action", `String "accept");
              ( "context",
                `Assoc [ ("consentId", `String "nonexistent-consent-id") ] );
            ] );
      ]
  in
  let response =
    Lwt_main.run (Teams.handle_file_consent_invoke ~config invoke_json)
  in
  Alcotest.(check int) "expired consent status code" 200 response.status_code;
  check_ok_invoke_response ~msg:"expired consent returns 200"
    (Teams.invoke_response_body response)

let test_file_consent_invoke_decline () =
  let consent_id =
    Teams.store_pending_consent ~content:"data" ~filename:"d.json"
      ~content_type:"application/json" ~ttl_s:60.0
  in
  let config = test_teams_config () in
  let invoke_json =
    `Assoc
      [
        ("type", `String "invoke");
        ("name", `String "fileConsent/invoke");
        ("serviceUrl", `String "https://svc");
        ("conversation", `Assoc [ ("id", `String "conv-1") ]);
        ( "value",
          `Assoc
            [
              ("action", `String "decline");
              ("context", `Assoc [ ("consentId", `String consent_id) ]);
            ] );
      ]
  in
  let response =
    Lwt_main.run (Teams.handle_file_consent_invoke ~config invoke_json)
  in
  Alcotest.(check int) "decline status code" 200 response.status_code;
  check_ok_invoke_response ~msg:"decline returns 200"
    (Teams.invoke_response_body response);
  (* Pending consent should be cleaned up *)
  Alcotest.(check bool)
    "consent consumed on decline" true
    (Teams.get_pending_consent consent_id = None)

let test_handle_invoke_file_consent_decline () =
  let consent_id =
    Teams.store_pending_consent ~content:"data" ~filename:"d.json"
      ~content_type:"application/json" ~ttl_s:60.0
  in
  let config = test_teams_config () in
  let body =
    `Assoc
      [
        ("type", `String "invoke");
        ("name", `String "fileConsent/invoke");
        ("serviceUrl", `String "https://svc");
        ("conversation", `Assoc [ ("id", `String "conv-1") ]);
        ( "value",
          `Assoc
            [
              ("action", `String "decline");
              ("context", `Assoc [ ("consentId", `String consent_id) ]);
            ] );
      ]
    |> Yojson.Safe.to_string
  in
  let status_code, body_str =
    Lwt_main.run
      (Teams.handle_invoke ~config ~auth_header:(bearer_for_config config) body)
  in
  Alcotest.(check int) "handle_invoke decline status code" 200 status_code;
  check_ok_invoke_response ~msg:"handle_invoke decline body" body_str

let test_handle_invoke_unknown_name_returns_ok () =
  let config = test_teams_config () in
  let body =
    `Assoc [ ("type", `String "invoke"); ("name", `String "unknown/invoke") ]
    |> Yojson.Safe.to_string
  in
  let status_code, body_str =
    Lwt_main.run
      (Teams.handle_invoke ~config ~auth_header:(bearer_for_config config) body)
  in
  Alcotest.(check int) "handle_invoke unknown status code" 200 status_code;
  check_ok_invoke_response ~msg:"handle_invoke unknown body" body_str

let test_is_retryable_status () =
  Alcotest.(check bool) "429 retryable" true (Teams.is_retryable_status 429);
  Alcotest.(check bool) "412 retryable" true (Teams.is_retryable_status 412);
  Alcotest.(check bool) "502 retryable" true (Teams.is_retryable_status 502);
  Alcotest.(check bool) "504 retryable" true (Teams.is_retryable_status 504);
  Alcotest.(check bool)
    "200 not retryable" false
    (Teams.is_retryable_status 200);
  Alcotest.(check bool)
    "400 not retryable" false
    (Teams.is_retryable_status 400);
  Alcotest.(check bool)
    "500 not retryable" false
    (Teams.is_retryable_status 500);
  Alcotest.(check bool)
    "401 not retryable" false
    (Teams.is_retryable_status 401);
  Alcotest.(check bool)
    "503 not retryable" false
    (Teams.is_retryable_status 503)

let test_throttle_enforces_spacing () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let conv =
       "test-throttle-conv-" ^ string_of_float (Unix.gettimeofday ())
     in
     let t0 = Unix.gettimeofday () in
     let* () = Teams.throttle_for_conversation ~conversation_id:conv in
     let t1 = Unix.gettimeofday () in
     let* () = Teams.throttle_for_conversation ~conversation_id:conv in
     let t2 = Unix.gettimeofday () in
     (* First call should be near-instant *)
     Alcotest.(check bool) "first call fast" true (t1 -. t0 < 0.5);
     (* Second call should wait ~1s *)
     Alcotest.(check bool) "second call waited >=0.9s" true (t2 -. t1 >= 0.9);
     Lwt.return_unit)

let test_throttle_different_conversations () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let ts = string_of_float (Unix.gettimeofday ()) in
     let conv_a = "test-throttle-a-" ^ ts in
     let conv_b = "test-throttle-b-" ^ ts in
     let t0 = Unix.gettimeofday () in
     let* () = Teams.throttle_for_conversation ~conversation_id:conv_a in
     let* () = Teams.throttle_for_conversation ~conversation_id:conv_b in
     let t1 = Unix.gettimeofday () in
     (* Different conversations should not block each other *)
     Alcotest.(check bool) "different convs fast" true (t1 -. t0 < 0.5);
     Lwt.return_unit)

let test_parse_activity_with_entities () =
  let entities =
    [
      mention_entity ~id:"28:test-app" ~name:"clawq";
      mention_entity ~id:"user-2" ~name:"Alice";
    ]
  in
  let body =
    activity_json ~activity_type:"message" ~text:"<at>clawq</at> hello"
      ~activity_id:"act-e" ~service_url:"https://svc" ~user_id:"u1"
      ~user_name:"Bob" ~conversation_id:"conv-e" ~team_id:"t1" ~is_group:true
      ~entities ()
  in
  match Teams.parse_activity body with
  | None -> Alcotest.fail "expected Some"
  | Some a ->
      Alcotest.(check int) "mentioned_ids count" 2 (List.length a.mentioned_ids);
      Alcotest.(check bool)
        "bot id in mentioned_ids" true
        (List.mem "28:test-app" a.mentioned_ids);
      Alcotest.(check bool)
        "user id in mentioned_ids" true
        (List.mem "user-2" a.mentioned_ids)

let test_parse_activity_no_entities () =
  let body =
    activity_json ~activity_type:"message" ~text:"hello" ~activity_id:"act-ne"
      ~service_url:"https://svc" ~user_id:"u1" ~user_name:"Alice"
      ~conversation_id:"conv-ne" ~team_id:"t1" ()
  in
  match Teams.parse_activity body with
  | None -> Alcotest.fail "expected Some"
  | Some a ->
      Alcotest.(check int)
        "no entities = empty mentioned_ids" 0
        (List.length a.mentioned_ids)

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

let test_parse_activity_with_attachments () =
  let att =
    `Assoc
      [
        ("contentType", `String "application/pdf");
        ("contentUrl", `String "https://example.com/doc.pdf");
        ("name", `String "report.pdf");
      ]
  in
  let body =
    activity_json ~activity_type:"message" ~text:"See attached"
      ~activity_id:"a1" ~service_url:"https://svc" ~user_id:"u1"
      ~user_name:"User" ~conversation_id:"c1" ~team_id:"t1" ~attachments:[ att ]
      ()
  in
  match Teams.parse_activity body with
  | Some act ->
      Alcotest.(check int) "one attachment" 1 (List.length act.attachments);
      let a = List.hd act.attachments in
      Alcotest.(check string) "name" "report.pdf" a.name;
      Alcotest.(check string) "content_type" "application/pdf" a.content_type;
      Alcotest.(check string)
        "content_url" "https://example.com/doc.pdf" a.content_url
  | None -> Alcotest.fail "expected Some"

let test_parse_activity_card_filtered () =
  let card =
    `Assoc
      [
        ("contentType", `String "application/vnd.microsoft.card.adaptive");
        ("content", `Assoc []);
      ]
  in
  let real =
    `Assoc
      [
        ("contentType", `String "text/plain");
        ("contentUrl", `String "https://example.com/file.txt");
        ("name", `String "file.txt");
      ]
  in
  let body =
    activity_json ~activity_type:"message" ~text:"hello" ~activity_id:"a2"
      ~service_url:"https://svc" ~user_id:"u1" ~user_name:"User"
      ~conversation_id:"c1" ~team_id:"t1" ~attachments:[ card; real ] ()
  in
  match Teams.parse_activity body with
  | Some act ->
      Alcotest.(check int) "card filtered out" 1 (List.length act.attachments);
      Alcotest.(check string)
        "kept name" "file.txt" (List.hd act.attachments).name
  | None -> Alcotest.fail "expected Some"

let test_parse_activity_attachment_only () =
  let att =
    `Assoc
      [
        ("contentType", `String "image/png");
        ("contentUrl", `String "https://example.com/img.png");
        ("name", `String "screenshot.png");
      ]
  in
  let body =
    activity_json ~activity_type:"message" ~text:"" ~activity_id:"a3"
      ~service_url:"https://svc" ~user_id:"u1" ~user_name:"User"
      ~conversation_id:"c1" ~team_id:"t1" ~attachments:[ att ] ()
  in
  match Teams.parse_activity body with
  | Some act ->
      Alcotest.(check string) "text empty" "" act.text;
      Alcotest.(check int) "has attachment" 1 (List.length act.attachments)
  | None -> Alcotest.fail "attachment-only should return Some"

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
    Alcotest.test_case "parse_activity with entities" `Quick
      test_parse_activity_with_entities;
    Alcotest.test_case "parse_activity no entities" `Quick
      test_parse_activity_no_entities;
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
    Alcotest.test_case "select file upload delivery personal chat" `Quick
      test_select_file_upload_delivery_personal_chat;
    Alcotest.test_case "select file upload delivery group chat" `Quick
      test_select_file_upload_delivery_group_chat;
    Alcotest.test_case "select file upload delivery team channel" `Quick
      test_select_file_upload_delivery_team_channel;
    Alcotest.test_case "select file upload delivery disabled" `Quick
      test_select_file_upload_delivery_disabled;
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
    Alcotest.test_case "is_retryable_status" `Quick test_is_retryable_status;
    Alcotest.test_case "throttle enforces spacing" `Slow
      test_throttle_enforces_spacing;
    Alcotest.test_case "throttle different conversations" `Quick
      test_throttle_different_conversations;
    Alcotest.test_case "debug dump filename sanitization" `Quick
      test_debug_dump_filename_sanitization;
    Alcotest.test_case "temp_downloads add and get" `Quick
      test_temp_downloads_add_and_get;
    Alcotest.test_case "temp_downloads expired entry" `Quick
      test_temp_downloads_expired;
    Alcotest.test_case "temp_downloads missing token" `Quick
      test_temp_downloads_missing;
    Alcotest.test_case "temp_downloads url with base" `Quick
      test_temp_downloads_url_with_base;
    Alcotest.test_case "temp_downloads url trailing slash" `Quick
      test_temp_downloads_url_trailing_slash;
    Alcotest.test_case "temp_downloads url no base" `Quick
      test_temp_downloads_url_no_base;
    Alcotest.test_case "temp_downloads cleanup" `Quick
      test_temp_downloads_cleanup;
    Alcotest.test_case "file consent card JSON" `Quick
      test_build_file_consent_card;
    Alcotest.test_case "file info card JSON" `Quick test_build_file_info_card;
    Alcotest.test_case "pending consent store and get" `Quick
      test_pending_consent_store_and_get;
    Alcotest.test_case "pending consent expired" `Quick
      test_pending_consent_expired;
    Alcotest.test_case "pending consent consumed on get" `Quick
      test_pending_consent_consumed_on_get;
    Alcotest.test_case "pending consent cleanup" `Quick
      test_pending_consent_cleanup;
    Alcotest.test_case "file consent invoke returns immediately" `Quick
      test_file_consent_invoke_returns_immediately;
    Alcotest.test_case "file consent invoke expired returns 200" `Quick
      test_file_consent_invoke_expired_returns_200;
    Alcotest.test_case "file consent invoke decline" `Quick
      test_file_consent_invoke_decline;
    Alcotest.test_case "handle_invoke file consent decline" `Quick
      test_handle_invoke_file_consent_decline;
    Alcotest.test_case "handle_invoke unknown name returns ok" `Quick
      test_handle_invoke_unknown_name_returns_ok;
    Alcotest.test_case "parse_activity with attachments" `Quick
      test_parse_activity_with_attachments;
    Alcotest.test_case "parse_activity card filtered" `Quick
      test_parse_activity_card_filtered;
    Alcotest.test_case "parse_activity attachment only" `Quick
      test_parse_activity_attachment_only;
  ]
