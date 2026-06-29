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
    default_model = None;
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

let run_teams_webhook ?send_reply_fn ?send_adaptive_card_fn ?event_limiter
    ?turn_fn ?(activity_id = "act-webhook") ?(text = "hello") () =
  let config = test_teams_config () in
  let session_manager = Session.create ~config:Runtime_config.default () in
  let body =
    activity_json ~activity_type:"message" ~text ~activity_id
      ~service_url:"https://svc" ~user_id:"u1" ~user_name:"Alice"
      ~conversation_id:"conv-1" ~team_id:"team-1" ()
  in
  Lwt_main.run
    (Teams.handle_webhook ~config ~session_manager ?send_reply_fn
       ?send_adaptive_card_fn ?event_limiter ?turn_fn
       ~auth_header:(bearer_for_config config) body)

let capture_reply sent ?alert:_ ~config:_ ~service_url:_ ~conversation_id:_
    ~reply_to_id:_ ~text ?mention:_ () =
  sent := text :: !sent;
  Lwt.return "reply-id"

let capture_adaptive_card cards ~config:_ ~service_url:_ ~conversation_id:_
    ~reply_to_id:_ ~card () =
  cards := card :: !cards;
  Lwt.return "test-activity-id"

let successful_turn count _mgr ~key:_ ~message:_ ?content_parts:_ ?attachments:_
    ?skill_injections:_ ?channel_name:_ ?channel_type:_ ?sender_id:_
    ?sender_name:_ ?user_group:_ ?channel:_ ?channel_id:_ ?message_id:_ ?cwd:_
    ?deferred_if_busy:_ ?before_drain:_ ?snapshot_work_type:_ () =
  incr count;
  Lwt.return "agent ok"

let failing_turn _mgr ~key:_ ~message:_ ?content_parts:_ ?attachments:_
    ?skill_injections:_ ?channel_name:_ ?channel_type:_ ?sender_id:_
    ?sender_name:_ ?user_group:_ ?channel:_ ?channel_id:_ ?message_id:_ ?cwd:_
    ?deferred_if_busy:_ ?before_drain:_ ?snapshot_work_type:_ () =
  Lwt.fail_with "boom"

let test_parse_activity_returns_record () =
  let body =
    activity_json ~activity_type:"message" ~text:"hello" ~activity_id:"act-1"
      ~service_url:"https://svc" ~user_id:"u1" ~user_name:"Alice"
      ~conversation_id:"conv-1" ~team_id:"t1" ()
  in
  match Teams_activity_parser.parse_activity body with
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
    (Teams_activity_parser.parse_activity body = None)

let test_parse_activity_empty_text () =
  let body =
    activity_json ~activity_type:"message" ~text:"" ~activity_id:"act-3"
      ~service_url:"https://svc" ~user_id:"u1" ~user_name:"Alice"
      ~conversation_id:"conv-1" ~team_id:"" ()
  in
  Alcotest.(check bool)
    "empty text returns None" true
    (Teams_activity_parser.parse_activity body = None)

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
  match Teams_activity_parser.parse_activity body with
  | None -> Alcotest.fail "expected Some"
  | Some a ->
      Alcotest.(check string) "user_name defaults to empty" "" a.user_name

let test_session_key () =
  let key = Teams.session_key ~team_id:"t1" ~conversation_id:"conv-1" in
  Alcotest.(check string) "session key format" "teams:t1:conv-1" key

let test_session_key_personal () =
  let key = Teams.session_key ~team_id:"personal" ~conversation_id:"conv-abc" in
  Alcotest.(check string) "personal session key" "teams:personal:conv-abc" key

let test_resolve_session_key_uses_room_binding () =
  let db = Memory.init ~db_path:":memory:" () in
  let profile_id = Memory.insert_room_profile ~db ~name:"teams-room" in
  Memory.upsert_room_profile_binding ~db ~room_id:"conv-bound" ~profile_id;
  let mgr = Session.create ~config:Runtime_config.default ~db () in
  let key =
    Teams.resolve_session_key ~session_manager:mgr ~team_id:"team-1"
      ~conversation_id:"conv-bound" ()
  in
  Alcotest.(check string) "bound room key" "teams:conv-bound" key

let test_resolve_session_key_falls_back_to_team_conversation () =
  let db = Memory.init ~db_path:":memory:" () in
  let mgr = Session.create ~config:Runtime_config.default ~db () in
  let key =
    Teams.resolve_session_key ~session_manager:mgr ~team_id:"team-1"
      ~conversation_id:"conv-free" ()
  in
  Alcotest.(check string) "unbound key" "teams:team-1:conv-free" key

let test_consent_room_context_uses_room_binding () =
  let db = Memory.init ~db_path:":memory:" () in
  let profile_id = Memory.insert_room_profile ~db ~name:"incident-room" in
  Memory.upsert_room_profile_binding ~db ~room_id:"conv-bound" ~profile_id;
  let mgr = Session.create ~config:Runtime_config.default ~db () in
  match
    Teams.consent_room_context ~session_manager:mgr
      ~conversation_id:"conv-bound"
  with
  | None -> Alcotest.fail "expected room consent context"
  | Some ctx ->
      Alcotest.(check string) "room id" "conv-bound" ctx.room_id;
      Alcotest.(check string) "session key" "teams:conv-bound" ctx.session_key;
      Alcotest.(check string) "profile name" "incident-room" ctx.profile_name

let runtime_config_with_connector_history ?(enabled = true) () =
  {
    Runtime_config.default with
    connector_history =
      {
        Runtime_config.default.connector_history with
        enabled;
        persist_to_db = true;
      };
  }

let bind_room_profile ~db ~room_id =
  let profile_id =
    Memory.insert_room_profile ~db ~name:("profile-" ^ room_id)
  in
  Memory.upsert_room_profile_binding ~db ~room_id ~profile_id

let handle_teams_room_message ?(enabled = true) ~db ~conversation_id ~text () =
  Hashtbl.reset Connector_history.buffers;
  let config = test_teams_config () in
  let session_manager =
    Session.create
      ~config:(runtime_config_with_connector_history ~enabled ())
      ~db ()
  in
  let sent = ref [] in
  let body =
    activity_json ~activity_type:"message" ~text ~activity_id:"act-room"
      ~service_url:"https://svc" ~user_id:"u-room" ~user_name:"Room User"
      ~conversation_id ~team_id:"team-room" ~is_group:true ()
  in
  Lwt_main.run
    (Teams.handle_webhook ~config ~session_manager
       ~send_reply_fn:(capture_reply sent)
       ~auth_header:(bearer_for_config config) body)

let test_room_history_capture_for_bound_room () =
  let db = Memory.init ~db_path:":memory:" () in
  bind_room_profile ~db ~room_id:"conv-history";
  handle_teams_room_message ~db ~conversation_id:"conv-history" ~text:"/help" ();
  match
    Connector_history.query ~db ~room_id:"conv-history" ~connector_type:"teams"
      ()
  with
  | [ entry ] ->
      Alcotest.(check string) "room_id" "conv-history" entry.room_id;
      Alcotest.(check string) "sender_id" "u-room" entry.sender_id;
      Alcotest.(check string) "sender_name" "Room User" entry.sender_name;
      Alcotest.(check string) "text" "/help" entry.text
  | entries ->
      Alcotest.failf "expected one Teams scoped history entry, got %d"
        (List.length entries)

let test_room_history_privacy_guard_requires_binding () =
  let db = Memory.init ~db_path:":memory:" () in
  handle_teams_room_message ~db ~conversation_id:"conv-unbound" ~text:"/help" ();
  let entries =
    Connector_history.query ~db ~room_id:"conv-unbound" ~connector_type:"teams"
      ()
  in
  Alcotest.(check int)
    "unbound Teams room history entries" 0 (List.length entries)

let test_room_history_respects_capabilities_gate () =
  let db = Memory.init ~db_path:":memory:" () in
  bind_room_profile ~db ~room_id:"conv-disabled";
  handle_teams_room_message ~enabled:false ~db ~conversation_id:"conv-disabled"
    ~text:"/help" ();
  let entries =
    Connector_history.query ~db ~room_id:"conv-disabled" ~connector_type:"teams"
      ()
  in
  Alcotest.(check int)
    "disabled Teams room history entries" 0 (List.length entries)

let test_normalize_clawq_slash_known_subcommand () =
  Alcotest.(check string)
    "known subcommand" "/status"
    (Teams.normalize_clawq_slash_text "/clawq status")

let test_normalize_clawq_slash_unknown_subcommand_shows_help () =
  let normalized = Teams.normalize_clawq_slash_text "/clawq frobnicate" in
  Alcotest.(check string) "unknown becomes help" "/help" normalized;
  match Slash_commands.handle normalized with
  | Slash_commands.Help -> ()
  | _ -> Alcotest.fail "expected Help"

let test_normalize_clawq_slash_admin_subcommand_still_gated () =
  let normalized = Teams.normalize_clawq_slash_text "/clawq config show" in
  Alcotest.(check string) "admin subcommand" "/config show" normalized;
  match
    Slash_commands.handle normalized
    |> Slash_commands.gate_admin ~is_admin:false
  with
  | Slash_commands.Reply msg ->
      Alcotest.(check bool)
        "requires admin" true
        (Test_helpers.string_contains msg "requires admin")
  | _ -> Alcotest.fail "expected admin-required reply"

let test_teams_rate_limit_message_matches_slack_text () =
  Alcotest.(check string)
    "rate limit message"
    "Please slow down, I can only process a limited number of messages per \
     minute."
    Teams.incoming_rate_limited_message

let test_teams_rate_limit_enforced_like_slack () =
  Hashtbl.clear Teams.rate_limit_warnings;
  let limiter = Rate_limiter.create ~rate_per_minute:1 ~burst_multiplier:1.0 in
  let sent = ref [] in
  let turn_count = ref 0 in
  let send_reply_fn = capture_reply sent in
  let turn_fn = successful_turn turn_count in
  run_teams_webhook ~send_reply_fn ~event_limiter:limiter ~turn_fn
    ~activity_id:"act-rate-1" ~text:"hello" ();
  run_teams_webhook ~send_reply_fn ~event_limiter:limiter ~turn_fn
    ~activity_id:"act-rate-2" ~text:"hello again" ();
  Alcotest.(check int) "only first turn reaches agent" 1 !turn_count;
  Alcotest.(check bool)
    "rate-limit warning sent" true
    (List.exists (fun msg -> msg = Teams.incoming_rate_limited_message) !sent)

let test_teams_menu_card_renders_imback_actions () =
  let card =
    Slash_commands_manifest.menu_adaptive_card_json ~is_admin:false ()
  in
  let s = Yojson.Safe.to_string card in
  Alcotest.(check bool)
    "adaptive card attachment" true
    (Test_helpers.string_contains s "application/vnd.microsoft.card.adaptive");
  Alcotest.(check bool)
    "Teams imBack actions" true
    (Test_helpers.string_contains s "\"imBack\"")

let test_teams_menu_command_sends_adaptive_card () =
  let sent = ref [] in
  let cards = ref [] in
  run_teams_webhook ~send_reply_fn:(capture_reply sent)
    ~send_adaptive_card_fn:(capture_adaptive_card cards)
    ~activity_id:"act-menu" ~text:"/clawq menu" ();
  Alcotest.(check int) "one adaptive card" 1 (List.length !cards);
  Alcotest.(check (list string)) "no text reply" [] !sent;
  let card_text = Yojson.Safe.to_string (List.hd !cards) in
  Alcotest.(check bool)
    "Teams card content type" true
    (Test_helpers.string_contains card_text
       "application/vnd.microsoft.card.adaptive");
  Alcotest.(check bool)
    "Teams card uses imBack" true
    (Test_helpers.string_contains card_text "\"imBack\"")

let test_teams_menu_filters_room_profile_tools () =
  let config =
    {
      Runtime_config.default with
      room_profiles =
        [
          {
            id = "incident";
            display_name = None;
            model = "";
            system_prompt = "";
            max_tool_iterations = 10;
            status = "active";
            allowed_tools = [ "background_task_list" ];
            denied_tools = [];
            access_bundle_ids = [];
            ambient_enabled = false;
            ambient_quiet_start = 23;
            ambient_quiet_end = 8;
            ambient_rate_limit_rph = 0;
          };
        ];
      room_profile_bindings =
        [ { profile_id = "incident"; room = "conv-bound"; active = true } ];
    }
  in
  let card =
    Slash_commands_manifest.menu_adaptive_card_json ~page:2 ~is_admin:true
      ~config ~session_key:"teams:conv-bound" ()
  in
  let card_text = Yojson.Safe.to_string card in
  Alcotest.(check bool)
    "bg remains visible because list tool is allowed" true
    (Test_helpers.string_contains card_text "/bg");
  Alcotest.(check bool)
    "delegate hidden because delegate tool is not allowed" false
    (Test_helpers.string_contains card_text "/delegate")

let test_teams_bg_card_filters_room_profile_tools () =
  let config =
    {
      Runtime_config.default with
      room_profiles =
        [
          {
            id = "incident";
            display_name = None;
            model = "";
            system_prompt = "";
            max_tool_iterations = 10;
            status = "active";
            allowed_tools = [ "background_task_list" ];
            denied_tools = [];
            access_bundle_ids = [];
            ambient_enabled = false;
            ambient_quiet_start = 23;
            ambient_quiet_end = 8;
            ambient_rate_limit_rph = 0;
          };
        ];
      room_profile_bindings =
        [ { profile_id = "incident"; room = "conv-bound"; active = true } ];
    }
  in
  let card =
    Slash_commands_manifest.bg_menu_adaptive_card_json ~config
      ~session_key:"teams:conv-bound"
      ~cancellable:[ (7, "codex") ]
      ()
  in
  let card_text = Yojson.Safe.to_string card in
  Alcotest.(check bool)
    "list button visible" true
    (Test_helpers.string_contains card_text "List Tasks");
  Alcotest.(check bool)
    "create button hidden" false
    (Test_helpers.string_contains card_text "Create Task");
  Alcotest.(check bool)
    "cancel button hidden" false
    (Test_helpers.string_contains card_text "Cancel #7")

let test_teams_agent_error_replies_like_slack () =
  let sent = ref [] in
  run_teams_webhook ~send_reply_fn:(capture_reply sent) ~turn_fn:failing_turn
    ~activity_id:"act-error" ~text:"hello" ();
  Alcotest.(check bool)
    "error reply sent" true
    (List.exists
       (fun msg ->
         Test_helpers.string_contains msg
           "Sorry, an error occurred processing your message: boom")
       !sent)

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
  match Teams_activity_parser.parse_activity body with
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

(* A long run with no whitespace forces a mid-word break. No character may be
   dropped: rejoining the chunks must reproduce the original exactly. *)
let test_split_message_no_whitespace_lossless () =
  let long = String.make (Teams.max_message_chars + 100) 'x' in
  let chunks = Teams.split_message long in
  Alcotest.(check bool) "multiple chunks" true (List.length chunks > 1);
  Alcotest.(check string)
    "no characters lost on forced break" long (String.concat "" chunks)

(* Mixed content: a long unbroken run followed by whitespace then a tail. The
   forced mid-word break in the run must not drop a character; the trailing
   whitespace lands inside the final chunk and is not consumed as a separator,
   so rejoining the chunks reproduces the original exactly. *)
let test_split_message_mixed_lossless () =
  let run = String.make (Teams.max_message_chars + 50) 'a' in
  let text = run ^ " tail" in
  let chunks = Teams.split_message text in
  Alcotest.(check bool) "multiple chunks" true (List.length chunks > 1);
  Alcotest.(check string)
    "forced break loses no characters" text (String.concat "" chunks)

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
      let text =
        Slash_commands.format_help ~connector:Format_adapter.Teams
          ~is_admin:true ()
      in
      Alcotest.(check bool)
        "help stays multiline" true
        (String.contains text '\n')
  | _ -> Alcotest.fail "expected Help from /help"

let test_help_reply_body_uses_markdown_table () =
  let help_text =
    Slash_commands.format_help ~connector:Format_adapter.Teams ~is_admin:true ()
  in
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
    Teams_file_upload.build_attachment_upload_body ~filename:"test.json"
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
    Teams_file_upload.build_message_with_attachment ~filename:"dump.json"
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
      ~description:"Session dump" ~size_bytes:12345 ~consent_id:"abc123" ()
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

let test_build_file_consent_card_includes_room_profile_context () =
  let room_context : Teams.consent_room_context =
    {
      room_id = "conv-room";
      session_key = "teams:conv-room";
      profile_name = "support-room";
    }
  in
  let body =
    Teams.build_file_consent_card ~filename:"dump.json"
      ~description:"Session dump" ~size_bytes:12345 ~consent_id:"abc123"
      ~room_context ()
  in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let content =
    json |> member "attachments" |> to_list |> List.hd |> member "content"
  in
  Alcotest.(check string)
    "description names room profile" "Session dump\nRoom profile: support-room"
    (content |> member "description" |> to_string);
  List.iter
    (fun field ->
      let ctx = content |> member field in
      Alcotest.(check string)
        (field ^ " roomId") "conv-room"
        (ctx |> member "roomId" |> to_string);
      Alcotest.(check string)
        (field ^ " sessionKey") "teams:conv-room"
        (ctx |> member "sessionKey" |> to_string);
      Alcotest.(check string)
        (field ^ " roomProfileName")
        "support-room"
        (ctx |> member "roomProfileName" |> to_string))
    [ "acceptContext"; "declineContext" ]

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
      ~content_type:"application/json" ~ttl_s:60.0 ()
  in
  Alcotest.(check bool) "id non-empty" true (String.length consent_id > 0);
  match Teams.get_pending_consent consent_id with
  | None -> Alcotest.fail "expected Some"
  | Some entry ->
      Alcotest.(check string) "content" "test data" entry.content;
      Alcotest.(check string) "filename" "test.json" entry.filename

let test_pending_consent_preserves_room_context () =
  let room_context : Teams.consent_room_context =
    {
      room_id = "conv-bound";
      session_key = "teams:conv-bound";
      profile_name = "incident-room";
    }
  in
  let consent_id =
    Teams.store_pending_consent ~content:"test data" ~filename:"test.json"
      ~content_type:"application/json" ~ttl_s:60.0 ~room_context ()
  in
  match Teams.get_pending_consent consent_id with
  | None -> Alcotest.fail "expected Some"
  | Some entry -> (
      match entry.room_context with
      | None -> Alcotest.fail "expected room context"
      | Some ctx ->
          Alcotest.(check string) "room id" "conv-bound" ctx.room_id;
          Alcotest.(check string)
            "session key" "teams:conv-bound" ctx.session_key;
          Alcotest.(check string)
            "profile name" "incident-room" ctx.profile_name)

let test_pending_consent_expired () =
  let consent_id =
    Teams.store_pending_consent ~content:"old" ~filename:"old.json"
      ~content_type:"application/json" ~ttl_s:0.0 ()
  in
  Unix.sleepf 0.01;
  Alcotest.(check bool)
    "expired is None" true
    (Teams.get_pending_consent consent_id = None)

let test_pending_consent_consumed_on_get () =
  let consent_id =
    Teams.store_pending_consent ~content:"once" ~filename:"once.json"
      ~content_type:"application/json" ~ttl_s:60.0 ()
  in
  ignore (Teams.get_pending_consent consent_id);
  Alcotest.(check bool)
    "second get is None" true
    (Teams.get_pending_consent consent_id = None)

let test_pending_consent_cleanup () =
  let _live =
    Teams.store_pending_consent ~content:"live" ~filename:"live.json"
      ~content_type:"application/json" ~ttl_s:60.0 ()
  in
  let expired =
    Teams.store_pending_consent ~content:"expired" ~filename:"exp.json"
      ~content_type:"application/json" ~ttl_s:0.0 ()
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
      ~content_type:"application/json" ~ttl_s:60.0 ()
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

let test_file_consent_context_roundtrips_from_invoke () =
  let context =
    `Assoc
      [
        ("consentId", `String "abc123");
        ("roomId", `String "conv-bound");
        ("sessionKey", `String "teams:conv-bound");
        ("roomProfileName", `String "incident-room");
      ]
  in
  match Teams.consent_room_context_of_json context with
  | None -> Alcotest.fail "expected room context"
  | Some ctx ->
      Alcotest.(check string) "room id" "conv-bound" ctx.room_id;
      Alcotest.(check string) "session key" "teams:conv-bound" ctx.session_key;
      Alcotest.(check string) "profile name" "incident-room" ctx.profile_name

let test_file_consent_invoke_decline () =
  let consent_id =
    Teams.store_pending_consent ~content:"data" ~filename:"d.json"
      ~content_type:"application/json" ~ttl_s:60.0 ()
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
      ~content_type:"application/json" ~ttl_s:60.0 ()
  in
  let config = test_teams_config () in
  let session_manager = Session.create ~config:Runtime_config.default () in
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
      (Teams.handle_invoke ~config ~session_manager
         ~auth_header:(bearer_for_config config) body)
  in
  Alcotest.(check int) "handle_invoke decline status code" 200 status_code;
  check_ok_invoke_response ~msg:"handle_invoke decline body" body_str

let test_handle_invoke_unknown_name_returns_ok () =
  let config = test_teams_config () in
  let session_manager = Session.create ~config:Runtime_config.default () in
  let body =
    `Assoc [ ("type", `String "invoke"); ("name", `String "unknown/invoke") ]
    |> Yojson.Safe.to_string
  in
  let status_code, body_str =
    Lwt_main.run
      (Teams.handle_invoke ~config ~session_manager
         ~auth_header:(bearer_for_config config) body)
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
  match Teams_activity_parser.parse_activity body with
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
  match Teams_activity_parser.parse_activity body with
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
  match Teams_activity_parser.parse_activity body with
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
  match Teams_activity_parser.parse_activity body with
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
  match Teams_activity_parser.parse_activity body with
  | Some act ->
      Alcotest.(check string) "text empty" "" act.text;
      Alcotest.(check int) "has attachment" 1 (List.length act.attachments)
  | None -> Alcotest.fail "attachment-only should return Some"

(* --- P11.M3.E3.T001: Reply targeting tests --- *)

(* build_reply_uri with non-empty reply_to_id targets the specific activity
   (threaded reply via Bot Framework). *)
let test_build_reply_uri_with_reply_to_id () =
  let uri =
    Teams.build_reply_uri ~service_url:"https://smba.trafficmanager.net/amer"
      ~conversation_id:"19:abc@thread.v2" ~reply_to_id:"act-42"
  in
  Alcotest.(check string)
    "targets specific activity"
    "https://smba.trafficmanager.net/amer/v3/conversations/19:abc@thread.v2/activities/act-42"
    uri

(* build_reply_uri with empty reply_to_id posts a new activity to the
   conversation (no threading). *)
let test_build_reply_uri_empty_reply_to_id () =
  let uri =
    Teams.build_reply_uri ~service_url:"https://smba.trafficmanager.net/amer"
      ~conversation_id:"19:abc@thread.v2" ~reply_to_id:""
  in
  Alcotest.(check string)
    "targets conversation"
    "https://smba.trafficmanager.net/amer/v3/conversations/19:abc@thread.v2/activities"
    uri

(* Personal 1:1 chat: conversation_id is a simple opaque string,
   reply_to_id targets the specific activity. *)
let test_build_reply_uri_personal_chat () =
  let uri =
    Teams.build_reply_uri ~service_url:"https://smba.trafficmanager.net/amer"
      ~conversation_id:"a:1personal-conversation-id"
      ~reply_to_id:"act-personal-1"
  in
  (* conversation_id contains colon which gets percent-encoded *)
  Alcotest.(check bool)
    "personal chat reply contains activities path" true
    (String.length uri > 0
    && String.sub uri 0
         (String.length "https://smba.trafficmanager.net/amer/v3/conversations/")
       = "https://smba.trafficmanager.net/amer/v3/conversations/");
  Alcotest.(check bool)
    "personal chat reply ends with activity id" true
    (let suffix = "/activities/act-personal-1" in
     let ulen = String.length uri in
     let slen = String.length suffix in
     ulen >= slen && String.sub uri (ulen - slen) slen = suffix)

(* Group chat: conversation_id is a group opaque string,
   reply_to_id targets the specific activity. *)
let test_build_reply_uri_group_chat () =
  let uri =
    Teams.build_reply_uri ~service_url:"https://smba.trafficmanager.net/amer"
      ~conversation_id:"19:meeting-id@thread.v2" ~reply_to_id:"act-group-1"
  in
  Alcotest.(check bool)
    "group chat reply contains activities path" true
    (String.length uri > 0
    && String.sub uri 0
         (String.length "https://smba.trafficmanager.net/amer/v3/conversations/")
       = "https://smba.trafficmanager.net/amer/v3/conversations/");
  Alcotest.(check bool)
    "group chat reply ends with activity id" true
    (let suffix = "/activities/act-group-1" in
     let ulen = String.length uri in
     let slen = String.length suffix in
     ulen >= slen && String.sub uri (ulen - slen) slen = suffix)

(* Channel thread conversation: @thread.v2 suffix means Thread kind.
   reply_to_id still targets the specific activity via the same URL scheme. *)
let test_build_reply_uri_channel_thread () =
  let uri =
    Teams.build_reply_uri ~service_url:"https://smba.trafficmanager.net/amer"
      ~conversation_id:"19:channel-thread-id@thread.v2"
      ~reply_to_id:"act-thread-1"
  in
  Alcotest.(check bool)
    "channel thread reply contains activities path" true
    (String.length uri > 0
    && String.sub uri 0
         (String.length "https://smba.trafficmanager.net/amer/v3/conversations/")
       = "https://smba.trafficmanager.net/amer/v3/conversations/");
  Alcotest.(check bool)
    "channel thread reply ends with activity id" true
    (let suffix = "/activities/act-thread-1" in
     let ulen = String.length uri in
     let slen = String.length suffix in
     ulen >= slen && String.sub uri (ulen - slen) slen = suffix)

(* parse_activity extracts activity_id correctly so callers can use it
   as reply_to_id for threaded replies. *)
let test_parse_activity_extracts_activity_id_for_reply () =
  let body =
    activity_json ~activity_type:"message" ~text:"please reply"
      ~activity_id:"msg-target-99" ~service_url:"https://svc" ~user_id:"u1"
      ~user_name:"Alice" ~conversation_id:"conv-r" ~team_id:"t1" ()
  in
  match Teams_activity_parser.parse_activity body with
  | None -> Alcotest.fail "expected Some"
  | Some a ->
      Alcotest.(check string)
        "activity_id usable as reply_to_id" "msg-target-99" a.activity_id

(* parse_activity with @thread.v2 conversation_id — the extracted
   conversation_id preserves the thread suffix for key construction. *)
let test_parse_activity_thread_conversation_id () =
  let body =
    activity_json ~activity_type:"message" ~text:"threaded" ~activity_id:"act-t"
      ~service_url:"https://svc" ~user_id:"u1" ~user_name:"Bob"
      ~conversation_id:"19:abc@thread.v2" ~team_id:"t1" ()
  in
  match Teams_activity_parser.parse_activity body with
  | None -> Alcotest.fail "expected Some"
  | Some a ->
      Alcotest.(check bool)
        "@thread.v2 suffix preserved" true
        (Room_session.is_thread_conversation_id a.conversation_id);
      Alcotest.(check string)
        "session key reflects thread" "teams:t1:19:abc@thread.v2"
        (Teams.session_key ~team_id:a.team_id ~conversation_id:a.conversation_id)

(* Room_session.detect_teams_kind correctly classifies room types:
   - Thread: conversation_id ends with @thread.v2
   - Personal: team_id = "personal"
   - Room: everything else *)
let test_detect_teams_kind_thread () =
  Alcotest.(check string)
    "@thread.v2 -> Thread" "thread"
    (Room_session.kind_to_string
       (Room_session.detect_teams_kind "t1" "19:abc@thread.v2"))

let test_detect_teams_kind_personal () =
  Alcotest.(check string)
    "personal team_id -> Personal" "personal"
    (Room_session.kind_to_string
       (Room_session.detect_teams_kind "personal" "a:1conv-id"))

let test_detect_teams_kind_room () =
  Alcotest.(check string)
    "other -> Room" "room"
    (Room_session.kind_to_string
       (Room_session.detect_teams_kind "t1" "19:channel-msg"))

(* Reply URL scheme is consistent across all room types — the Bot Framework
   uses the same /activities/{id} path regardless of room kind. *)
let test_reply_url_scheme_consistent_across_room_types () =
  let svc = "https://smba.trafficmanager.net/amer" in
  let test_cases =
    [
      ("personal", "a:1personal-conv", "act-p");
      ("group", "19:meeting@thread.v2", "act-g");
      ("channel", "19:channel-msg@thread.v2", "act-c");
      ("thread", "19:abc@thread.v2", "act-t");
    ]
  in
  List.iter
    (fun (label, conv_id, reply_id) ->
      let uri =
        Teams.build_reply_uri ~service_url:svc ~conversation_id:conv_id
          ~reply_to_id:reply_id
      in
      (* All room types use the same /activities/{id} path scheme *)
      Alcotest.(check bool)
        (label ^ " reply URI has activities path")
        true
        (let prefix = svc ^ "/v3/conversations/" in
         let plen = String.length prefix in
         String.length uri > plen && String.sub uri 0 plen = prefix);
      Alcotest.(check bool)
        (label ^ " reply URI ends with reply_id")
        true
        (let suffix = "/activities/" ^ reply_id in
         let ulen = String.length uri in
         let slen = String.length suffix in
         ulen >= slen && String.sub uri (ulen - slen) slen = suffix))
    test_cases

(* B464: empty text guard — both send_reply and edit_activity must short-circuit
   without calling fetch_token / hitting HTTP. We assert this by using an
   intentionally invalid service_url; if the guard didn't fire, the next branch
   would log an error about service_url scheme. With the guard, we get only the
   "refusing to send empty reply" warning and an empty activity_id back. *)
let test_send_reply_empty_text_short_circuits () =
  let cfg = test_teams_config () in
  let result =
    Lwt_main.run
      (Teams.send_reply ~config:cfg
         ~service_url:"https://smba.trafficmanager.net/au/test/"
         ~conversation_id:"19:test@thread.v2" ~reply_to_id:"" ~text:"" ())
  in
  Alcotest.(check string) "empty text returns empty activity_id" "" result

let test_send_reply_whitespace_only_short_circuits () =
  let cfg = test_teams_config () in
  let result =
    Lwt_main.run
      (Teams.send_reply ~config:cfg
         ~service_url:"https://smba.trafficmanager.net/au/test/"
         ~conversation_id:"19:test@thread.v2" ~reply_to_id:"" ~text:"   \n\t  "
         ())
  in
  Alcotest.(check string) "whitespace-only returns empty activity_id" "" result

let test_edit_activity_empty_text_short_circuits () =
  let cfg = test_teams_config () in
  (* P15.M2.E1.T003: edit_activity now raises on empty text. *)
  let raised =
    try
      Lwt_main.run
        (Teams.edit_activity ~config:cfg
           ~service_url:"https://smba.trafficmanager.net/au/test/"
           ~conversation_id:"19:test@thread.v2" ~activity_id:"act-1" ~text:"" ());
      false
    with Failure _ -> true
  in
  Alcotest.(check bool) "empty text raises" true raised

(* P15.M2.E1.T001: Thread-aware session key tests *)
let test_thread_session_key_with_reply_to_id () =
  let key =
    Teams.thread_session_key ~team_id:"team-1" ~conversation_id:"conv-1"
      ~reply_to_id:"msg-123"
  in
  Alcotest.(check string) "thread key" "teams:team-1:conv-1:thread:msg-123" key

let test_thread_session_key_without_reply_to_id () =
  let key =
    Teams.thread_session_key ~team_id:"team-1" ~conversation_id:"conv-1"
      ~reply_to_id:""
  in
  Alcotest.(check string) "non-thread key" "teams:team-1:conv-1" key

let test_resolve_session_key_with_thread () =
  let db = Memory.init ~db_path:":memory:" () in
  let mgr = Session.create ~config:Runtime_config.default ~db () in
  let key =
    Teams.resolve_session_key ~session_manager:mgr ~team_id:"team-1"
      ~conversation_id:"conv-1" ~reply_to_id:"msg-456" ()
  in
  Alcotest.(check string) "thread key" "teams:team-1:conv-1:thread:msg-456" key

let test_resolve_session_key_without_thread () =
  let db = Memory.init ~db_path:":memory:" () in
  let mgr = Session.create ~config:Runtime_config.default ~db () in
  let key =
    Teams.resolve_session_key ~session_manager:mgr ~team_id:"team-1"
      ~conversation_id:"conv-1" ()
  in
  Alcotest.(check string) "non-thread key" "teams:team-1:conv-1" key

let test_resolve_session_key_thread_with_room_binding () =
  let db = Memory.init ~db_path:":memory:" () in
  let profile_id = Memory.insert_room_profile ~db ~name:"teams-room" in
  Memory.upsert_room_profile_binding ~db ~room_id:"conv-bound" ~profile_id;
  let mgr = Session.create ~config:Runtime_config.default ~db () in
  let key =
    Teams.resolve_session_key ~session_manager:mgr ~team_id:"team-1"
      ~conversation_id:"conv-bound" ~reply_to_id:"msg-789" ()
  in
  (* Room binding takes precedence over thread key *)
  Alcotest.(check string) "bound room key" "teams:conv-bound" key

(* P15.M2.E1.T001: Persistent deduplication tests *)
let test_teams_dedup_persistent_check_and_mark () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.init_teams_dedup_schema db;
  (* First call should return false (not seen) *)
  let seen1 =
    Memory.teams_dedup_check_and_mark ~db ~conversation_id:"conv-1"
      ~activity_id:"act-1"
  in
  Alcotest.(check bool) "first call not seen" false seen1;
  (* Second call should return true (already seen) *)
  let seen2 =
    Memory.teams_dedup_check_and_mark ~db ~conversation_id:"conv-1"
      ~activity_id:"act-1"
  in
  Alcotest.(check bool) "second call seen" true seen2;
  (* Different activity should return false *)
  let seen3 =
    Memory.teams_dedup_check_and_mark ~db ~conversation_id:"conv-1"
      ~activity_id:"act-2"
  in
  Alcotest.(check bool) "different activity not seen" false seen3

let test_teams_dedup_persistent_empty_id () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.init_teams_dedup_schema db;
  (* Empty activity_id should always return false *)
  let seen1 =
    Memory.teams_dedup_check_and_mark ~db ~conversation_id:"conv-1"
      ~activity_id:""
  in
  Alcotest.(check bool) "empty id not seen" false seen1;
  let seen2 =
    Memory.teams_dedup_check_and_mark ~db ~conversation_id:"conv-1"
      ~activity_id:""
  in
  Alcotest.(check bool) "empty id still not seen" false seen2

let test_teams_dedup_persistent_cleanup () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.init_teams_dedup_schema db;
  (* Insert an old entry directly *)
  let stmt =
    Sqlite3.prepare db
      "INSERT INTO teams_dedup (conversation_id, activity_id, processed_at) \
       VALUES (?, ?, datetime('now', '-2 days'))"
  in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT "conv-old"));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT "act-old"));
  ignore (Sqlite3.step stmt);
  ignore (Sqlite3.finalize stmt);
  (* Cleanup with 1 day should remove it *)
  Memory.cleanup_teams_dedup ~db ~max_age_days:1;
  (* Should not be seen anymore *)
  let seen =
    Memory.teams_dedup_check_and_mark ~db ~conversation_id:"conv-old"
      ~activity_id:"act-old"
  in
  Alcotest.(check bool) "after cleanup not seen" false seen

let test_teams_dedup_persistent_survives_restart () =
  let db_path = Filename.temp_file "teams_dedup" ".db" in
  let db = Memory.init ~db_path () in
  Memory.init_teams_dedup_schema db;
  (* Mark an activity *)
  let _ =
    Memory.teams_dedup_check_and_mark ~db ~conversation_id:"conv-persist"
      ~activity_id:"act-persist"
  in
  (* Close and reopen database *)
  ignore (Sqlite3.db_close db);
  let db2 = Memory.init ~db_path () in
  (* Should still be seen *)
  let seen =
    Memory.teams_dedup_check_and_mark ~db:db2 ~conversation_id:"conv-persist"
      ~activity_id:"act-persist"
  in
  Alcotest.(check bool) "survives restart" true seen;
  ignore (Sqlite3.db_close db2);
  Sys.remove db_path

let test_teams_dedup_persistent_fallback_to_lru () =
  (* When no database, should fall back to in-memory LRU *)
  let seen1 =
    Teams.dedup_seen_persistent ~db:None ~conversation_id:"conv-lru"
      ~activity_id:"act-lru"
  in
  Alcotest.(check bool) "first call not seen" false seen1;
  let seen2 =
    Teams.dedup_seen_persistent ~db:None ~conversation_id:"conv-lru"
      ~activity_id:"act-lru"
  in
  Alcotest.(check bool) "second call seen" true seen2

let test_teams_dedup_different_conversations () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.init_teams_dedup_schema db;
  (* Mark an activity in conversation 1 *)
  let seen1 =
    Memory.teams_dedup_check_and_mark ~db ~conversation_id:"conv-1"
      ~activity_id:"act-same"
  in
  Alcotest.(check bool) "conv1 first call not seen" false seen1;
  (* Same activity_id in different conversation should NOT be deduplicated *)
  let seen2 =
    Memory.teams_dedup_check_and_mark ~db ~conversation_id:"conv-2"
      ~activity_id:"act-same"
  in
  Alcotest.(check bool) "conv2 same activity not seen" false seen2

(* P15.M2.E1.T002: Bounded context capture tests *)

let test_context_capture_returns_context_for_bound_room () =
  Hashtbl.reset Connector_history.buffers;
  let db = Memory.init ~db_path:":memory:" () in
  let config =
    {
      Runtime_config.default with
      connector_history =
        {
          Runtime_config.default.connector_history with
          enabled = true;
          persist_to_db = true;
        };
    }
  in
  let session_manager = Session.create ~config ~db () in
  Connector_history.record ~db ~persist:true ~key:"teams:conv-ctx"
    ~room_id:"conv-ctx" ~connector_type:"teams" ~channel_type:"teams" ~max:50
    ~sender_name:"Alice" ~sender_id:"a1" ~text:"hello" ();
  Connector_history.record ~db ~persist:true ~key:"teams:conv-ctx"
    ~room_id:"conv-ctx" ~connector_type:"teams" ~channel_type:"teams" ~max:50
    ~sender_name:"Bob" ~sender_id:"b1" ~text:"world" ();
  match
    Teams_context_capture.capture_room_context ~session_manager
      ~has_binding:(fun ~conversation_id:_ -> true)
      ~session_key:"teams:conv-ctx" ~conversation_id:"conv-ctx"
  with
  | Some ctx -> (
      Alcotest.(check bool) "context non-empty" true (String.length ctx > 0);
      match Str.search_forward (Str.regexp_string "Room context") ctx 0 with
      | _ -> ()
      | exception Not_found -> Alcotest.fail "missing Room context header")
  | None -> Alcotest.fail "expected Some context for bound room"

let test_context_capture_none_for_unbound_room () =
  Hashtbl.reset Connector_history.buffers;
  let db = Memory.init ~db_path:":memory:" () in
  let session_manager = Session.create ~config:Runtime_config.default ~db () in
  Connector_history.record ~db ~persist:true ~key:"teams:conv-no"
    ~room_id:"conv-no" ~connector_type:"teams" ~channel_type:"teams" ~max:50
    ~sender_name:"Alice" ~sender_id:"a1" ~text:"hello" ();
  let result =
    Teams_context_capture.capture_room_context ~session_manager
      ~has_binding:(fun ~conversation_id:_ -> false)
      ~session_key:"teams:conv-no" ~conversation_id:"conv-no"
  in
  Alcotest.(check bool) "unbound returns None" true (result = None)

let test_context_capture_none_when_disabled () =
  Hashtbl.reset Connector_history.buffers;
  let db = Memory.init ~db_path:":memory:" () in
  let session_manager = Session.create ~config:Runtime_config.default ~db () in
  Connector_history.record ~db ~persist:true ~key:"teams:conv-dis"
    ~room_id:"conv-dis" ~connector_type:"teams" ~channel_type:"teams" ~max:50
    ~sender_name:"Alice" ~sender_id:"a1" ~text:"hello" ();
  let result =
    Teams_context_capture.capture_room_context ~session_manager
      ~has_binding:(fun ~conversation_id:_ -> true)
      ~session_key:"teams:conv-dis" ~conversation_id:"conv-dis"
  in
  Alcotest.(check bool) "disabled returns None" true (result = None)

let test_context_capture_none_when_empty () =
  Hashtbl.reset Connector_history.buffers;
  let db = Memory.init ~db_path:":memory:" () in
  let config =
    {
      Runtime_config.default with
      connector_history =
        {
          Runtime_config.default.connector_history with
          enabled = true;
          persist_to_db = true;
        };
    }
  in
  let session_manager = Session.create ~config ~db () in
  let result =
    Teams_context_capture.capture_room_context ~session_manager
      ~has_binding:(fun ~conversation_id:_ -> true)
      ~session_key:"teams:conv-empty" ~conversation_id:"conv-empty"
  in
  Alcotest.(check bool) "empty returns None" true (result = None)

let test_context_capture_bounded_by_max () =
  Hashtbl.reset Connector_history.buffers;
  let db = Memory.init ~db_path:":memory:" () in
  let config =
    {
      Runtime_config.default with
      connector_history =
        {
          Runtime_config.default.connector_history with
          enabled = true;
          persist_to_db = true;
          max_messages = 5;
        };
    }
  in
  let session_manager = Session.create ~config ~db () in
  for i = 1 to 50 do
    Connector_history.record ~db ~persist:true ~key:"teams:conv-bnd"
      ~room_id:"conv-bnd" ~connector_type:"teams" ~channel_type:"teams" ~max:50
      ~sender_name:(Printf.sprintf "User%d" i)
      ~sender_id:(Printf.sprintf "u%d" i)
      ~text:(Printf.sprintf "message %d" i)
      ()
  done;
  match
    Teams_context_capture.capture_room_context ~session_manager
      ~has_binding:(fun ~conversation_id:_ -> true)
      ~session_key:"teams:conv-bnd" ~conversation_id:"conv-bnd"
  with
  | Some ctx -> (
      (* Header format: "[Room context: N recent messages...]" *)
      match
        Str.search_forward (Str.regexp "Room context: \\([0-9]+\\)") ctx 0
      with
      | _ ->
          let n_str = Str.matched_group 1 ctx in
          let n = int_of_string n_str in
          Alcotest.(check int) "bounded to max_messages=5" 5 n
      | exception Not_found ->
          Alcotest.fail "missing Room context count in header")
  | None -> Alcotest.fail "expected Some for room with 50 entries"

let test_get_formatted_for_key_roundtrip () =
  Hashtbl.reset Connector_history.buffers;
  let db = Memory.init ~db_path:":memory:" () in
  Memory.init_connector_history_schema db;
  Connector_history.record ~db ~persist:true ~key:"k1" ~room_id:"r1"
    ~connector_type:"teams" ~channel_type:"teams" ~max:50 ~sender_name:"A"
    ~sender_id:"a" ~text:"msg1" ();
  Connector_history.record ~db ~persist:true ~key:"k1" ~room_id:"r1"
    ~connector_type:"teams" ~channel_type:"teams" ~max:50 ~sender_name:"B"
    ~sender_id:"b" ~text:"msg2" ();
  match Connector_history.get_formatted_for_key ~db ~key:"k1" ~count:10 () with
  | Some (ctx, n) -> (
      Alcotest.(check int) "count" 2 n;
      match Str.search_forward (Str.regexp_string "msg1") ctx 0 with
      | _ -> ()
      | exception Not_found -> Alcotest.fail "missing msg1 in context")
  | None -> Alcotest.fail "expected Some for non-empty key"

let test_get_formatted_for_key_none_for_empty () =
  Hashtbl.reset Connector_history.buffers;
  let db = Memory.init ~db_path:":memory:" () in
  Memory.init_connector_history_schema db;
  let result =
    Connector_history.get_formatted_for_key ~db ~key:"no-key" ~count:10 ()
  in
  Alcotest.(check bool) "empty returns None" true (result = None)

let suite =
  [
    Alcotest.test_case "B464: send_reply empty text short-circuits" `Quick
      test_send_reply_empty_text_short_circuits;
    Alcotest.test_case "thread_session_key with reply_to_id" `Quick
      test_thread_session_key_with_reply_to_id;
    Alcotest.test_case "thread_session_key without reply_to_id" `Quick
      test_thread_session_key_without_reply_to_id;
    Alcotest.test_case "resolve_session_key with thread" `Quick
      test_resolve_session_key_with_thread;
    Alcotest.test_case "resolve_session_key without thread" `Quick
      test_resolve_session_key_without_thread;
    Alcotest.test_case "resolve_session_key thread with room binding" `Quick
      test_resolve_session_key_thread_with_room_binding;
    Alcotest.test_case "teams_dedup persistent check_and_mark" `Quick
      test_teams_dedup_persistent_check_and_mark;
    Alcotest.test_case "teams_dedup persistent empty id" `Quick
      test_teams_dedup_persistent_empty_id;
    Alcotest.test_case "teams_dedup persistent cleanup" `Quick
      test_teams_dedup_persistent_cleanup;
    Alcotest.test_case "teams_dedup persistent survives restart" `Quick
      test_teams_dedup_persistent_survives_restart;
    Alcotest.test_case "teams_dedup persistent fallback to LRU" `Quick
      test_teams_dedup_persistent_fallback_to_lru;
    Alcotest.test_case "teams_dedup different conversations" `Quick
      test_teams_dedup_different_conversations;
    Alcotest.test_case "B464: send_reply whitespace short-circuits" `Quick
      test_send_reply_whitespace_only_short_circuits;
    Alcotest.test_case "B464: edit_activity empty text short-circuits" `Quick
      test_edit_activity_empty_text_short_circuits;
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
    Alcotest.test_case "resolve_session_key uses room binding" `Quick
      test_resolve_session_key_uses_room_binding;
    Alcotest.test_case "resolve_session_key falls back to team conversation"
      `Quick test_resolve_session_key_falls_back_to_team_conversation;
    Alcotest.test_case "consent room context uses room binding" `Quick
      test_consent_room_context_uses_room_binding;
    Alcotest.test_case "room history captures bound room messages" `Quick
      test_room_history_capture_for_bound_room;
    Alcotest.test_case "room history requires room binding" `Quick
      test_room_history_privacy_guard_requires_binding;
    Alcotest.test_case "room history respects capabilities gate" `Quick
      test_room_history_respects_capabilities_gate;
    Alcotest.test_case "normalize /clawq known subcommand" `Quick
      test_normalize_clawq_slash_known_subcommand;
    Alcotest.test_case "normalize /clawq unknown subcommand" `Quick
      test_normalize_clawq_slash_unknown_subcommand_shows_help;
    Alcotest.test_case "normalize /clawq admin subcommand remains gated" `Quick
      test_normalize_clawq_slash_admin_subcommand_still_gated;
    Alcotest.test_case "rate limit message matches Slack" `Quick
      test_teams_rate_limit_message_matches_slack_text;
    Alcotest.test_case "rate limit enforced like Slack" `Quick
      test_teams_rate_limit_enforced_like_slack;
    Alcotest.test_case "menu card renders imBack actions" `Quick
      test_teams_menu_card_renders_imback_actions;
    Alcotest.test_case "menu command sends adaptive card" `Quick
      test_teams_menu_command_sends_adaptive_card;
    Alcotest.test_case "menu filters room profile tools" `Quick
      test_teams_menu_filters_room_profile_tools;
    Alcotest.test_case "bg card filters room profile tools" `Quick
      test_teams_bg_card_filters_room_profile_tools;
    Alcotest.test_case "agent error replies like Slack" `Quick
      test_teams_agent_error_replies_like_slack;
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
    Alcotest.test_case "split_message no-whitespace lossless" `Quick
      test_split_message_no_whitespace_lossless;
    Alcotest.test_case "split_message mixed lossless" `Quick
      test_split_message_mixed_lossless;
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
    Alcotest.test_case "file consent card room context" `Quick
      test_build_file_consent_card_includes_room_profile_context;
    Alcotest.test_case "file info card JSON" `Quick test_build_file_info_card;
    Alcotest.test_case "pending consent store and get" `Quick
      test_pending_consent_store_and_get;
    Alcotest.test_case "pending consent room context" `Quick
      test_pending_consent_preserves_room_context;
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
    Alcotest.test_case "file consent invoke room context" `Quick
      test_file_consent_context_roundtrips_from_invoke;
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
    (* P11.M3.E3.T001: Reply targeting tests *)
    Alcotest.test_case "reply_uri with reply_to_id" `Quick
      test_build_reply_uri_with_reply_to_id;
    Alcotest.test_case "reply_uri empty reply_to_id" `Quick
      test_build_reply_uri_empty_reply_to_id;
    Alcotest.test_case "reply_uri personal chat" `Quick
      test_build_reply_uri_personal_chat;
    Alcotest.test_case "reply_uri group chat" `Quick
      test_build_reply_uri_group_chat;
    Alcotest.test_case "reply_uri channel thread" `Quick
      test_build_reply_uri_channel_thread;
    Alcotest.test_case "parse_activity extracts activity_id for reply" `Quick
      test_parse_activity_extracts_activity_id_for_reply;
    Alcotest.test_case "parse_activity thread conversation_id" `Quick
      test_parse_activity_thread_conversation_id;
    Alcotest.test_case "detect_teams_kind thread" `Quick
      test_detect_teams_kind_thread;
    Alcotest.test_case "detect_teams_kind personal" `Quick
      test_detect_teams_kind_personal;
    Alcotest.test_case "detect_teams_kind room" `Quick
      test_detect_teams_kind_room;
    Alcotest.test_case "reply URI scheme consistent across room types" `Quick
      test_reply_url_scheme_consistent_across_room_types;
    (* P15.M2.E1.T002: Bounded context capture *)
    Alcotest.test_case "context capture for bound room" `Quick
      test_context_capture_returns_context_for_bound_room;
    Alcotest.test_case "context capture none for unbound" `Quick
      test_context_capture_none_for_unbound_room;
    Alcotest.test_case "context capture none when disabled" `Quick
      test_context_capture_none_when_disabled;
    Alcotest.test_case "context capture none when empty" `Quick
      test_context_capture_none_when_empty;
    Alcotest.test_case "context capture bounded by max" `Quick
      test_context_capture_bounded_by_max;
    Alcotest.test_case "get_formatted_for_key roundtrip" `Quick
      test_get_formatted_for_key_roundtrip;
    Alcotest.test_case "get_formatted_for_key empty" `Quick
      test_get_formatted_for_key_none_for_empty;
  ]
