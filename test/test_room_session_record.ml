let parse json = Config_loader.parse_config (Yojson.Safe.from_string json)

let with_db f =
  Test_helpers.with_memory_store
    ~init_schema:[ Room_session_record.init_schema ]
    f

let basic_config_json =
  {|{
    "workspace": "/tmp/test-rsr",
    "access_bundles": [
      {"id": "base", "allowed_tools": ["file_read", "shell_exec"], "denied_tools": ["deploy"]}
    ],
    "access_scopes": [
      {"id": "default", "level": "default", "access_bundle_ids": ["base"]}
    ]
  }|}

let profile_config_json =
  {|{
    "workspace": "/tmp/test-rsr",
    "access_bundles": [
      {"id": "base", "allowed_tools": ["file_read"]}
    ],
    "access_scopes": [
      {"id": "default", "level": "default", "access_bundle_ids": ["base"]}
    ],
    "room_profiles": [
      {"id": "vip", "display_name": "VIP Agent", "model": "openai:gpt-5.4", "system_prompt": "Be helpful", "allowed_tools": ["vip_tool"], "denied_tools": ["danger"]}
    ],
    "room_profile_bindings": [
      {"profile_id": "vip", "room": "C100", "active": true}
    ]
  }|}

let test_create_basic () =
  let cfg = parse basic_config_json in
  let snap_id = "snap_test_001" in
  let record =
    Room_session_record.create ~config:cfg ~access_snapshot_id:snap_id
      ~session_key:"slack:C123:U456" ()
  in
  Alcotest.(check bool) "id non-empty" true (String.length record.id > 0);
  Alcotest.(check bool)
    "created_at non-empty" true
    (String.length record.created_at > 0);
  Alcotest.(check (option string))
    "session_key" (Some "slack:C123:U456") record.session_key;
  Alcotest.(check (option string)) "room_id None" None record.room_id;
  Alcotest.(check string) "access_snapshot_id" snap_id record.access_snapshot_id;
  Alcotest.(check bool)
    "config_hash non-empty" true
    (String.length record.config_hash > 0);
  Alcotest.(check bool)
    "agent_config None without profile" true
    (record.agent_config = None);
  Alcotest.(check bool)
    "connector_context empty" true
    (record.connector_context = Room_session_record.empty_connector_context);
  Alcotest.(check bool) "delivery None" true (record.delivery = None);
  Alcotest.(check bool) "transcript_url None" true (record.transcript_url = None);
  Alcotest.(check bool) "session_url None" true (record.session_url = None)

let test_create_with_room_id () =
  let cfg = parse basic_config_json in
  let record =
    Room_session_record.create ~config:cfg ~access_snapshot_id:"snap_002"
      ~session_key:"slack:C123:U456" ~room_id:"C123" ()
  in
  Alcotest.(check (option string)) "room_id" (Some "C123") record.room_id

let test_create_with_profile () =
  let cfg = parse profile_config_json in
  let record =
    Room_session_record.create ~config:cfg ~access_snapshot_id:"snap_003"
      ~session_key:"slack:C100" ~room_id:"C100" ()
  in
  match record.agent_config with
  | None -> Alcotest.fail "expected agent_config for room C100"
  | Some ac ->
      Alcotest.(check string) "profile_id" "vip" ac.profile_id;
      Alcotest.(check (option string))
        "display_name" (Some "VIP Agent") ac.display_name;
      Alcotest.(check string) "model" "openai:gpt-5.4" ac.model;
      Alcotest.(check bool)
        "system_prompt_digest non-empty" true
        (String.length ac.system_prompt_digest > 0);
      Alcotest.(check string) "status" "active" ac.status;
      Alcotest.(check (list string))
        "allowed_tools" [ "vip_tool" ] ac.allowed_tools;
      Alcotest.(check (list string)) "denied_tools" [ "danger" ] ac.denied_tools;
      Alcotest.(check bool) "ambient_enabled" false ac.ambient_enabled

let test_create_with_origin () =
  let cfg = parse basic_config_json in
  let origin =
    Room_origin.make ~connector:"slack" ~workspace_id:"T1" ~room_id:"C50"
      ~requester_id:"U10" ~requester_name:"Alice" ~source_message_id:"1234.5678"
      ~thread_id:"1234.5678.000001" ~service_url:"https://slack.com" ()
  in
  let record =
    Room_session_record.create ~config:cfg ~access_snapshot_id:"snap_004"
      ~origin ()
  in
  let cc = record.connector_context in
  Alcotest.(check (option string)) "connector" (Some "slack") cc.connector;
  Alcotest.(check (option string)) "workspace_id" (Some "T1") cc.workspace_id;
  Alcotest.(check (option string)) "room_id" (Some "C50") cc.room_id;
  Alcotest.(check (option string)) "requester_id" (Some "U10") cc.requester_id;
  Alcotest.(check (option string))
    "requester_name" (Some "Alice") cc.requester_name;
  Alcotest.(check (option string))
    "source_message_id" (Some "1234.5678") cc.source_message_id;
  Alcotest.(check (option string))
    "thread_id" (Some "1234.5678.000001") cc.thread_id;
  Alcotest.(check (option string))
    "service_url" (Some "https://slack.com") cc.service_url

let test_create_with_delivery () =
  let cfg = parse basic_config_json in
  let delivery : Room_session_record.delivery_snapshot =
    {
      state = "confirmed";
      last_update = "2026-06-29T10:00:00Z";
      message_id = Some "msg_123";
      error_detail = None;
    }
  in
  let record =
    Room_session_record.create ~config:cfg ~access_snapshot_id:"snap_005"
      ~delivery ~session_key:"slack:C1" ()
  in
  match record.delivery with
  | None -> Alcotest.fail "expected delivery"
  | Some d ->
      Alcotest.(check string) "state" "confirmed" d.state;
      Alcotest.(check string) "last_update" "2026-06-29T10:00:00Z" d.last_update;
      Alcotest.(check (option string))
        "message_id" (Some "msg_123") d.message_id;
      Alcotest.(check bool) "error_detail None" true (d.error_detail = None)

let test_create_with_transcript_and_session_urls () =
  let cfg = parse basic_config_json in
  let record =
    Room_session_record.create ~config:cfg ~access_snapshot_id:"snap_006"
      ~transcript_url:"https://example.com/transcript"
      ~session_url:"https://example.com/session" ~session_key:"slack:C1" ()
  in
  Alcotest.(check (option string))
    "transcript_url" (Some "https://example.com/transcript")
    record.transcript_url;
  Alcotest.(check (option string))
    "session_url" (Some "https://example.com/session") record.session_url

let test_create_derives_room_from_origin () =
  let cfg = parse basic_config_json in
  let origin = Room_origin.make ~connector:"discord" ~room_id:"D999" () in
  let record =
    Room_session_record.create ~config:cfg ~access_snapshot_id:"snap_007"
      ~origin ()
  in
  Alcotest.(check (option string))
    "room_id from origin" (Some "D999") record.room_id

let test_persist_and_get () =
  with_db (fun db ->
      let cfg = parse basic_config_json in
      let record =
        Room_session_record.create ~config:cfg ~access_snapshot_id:"snap_010"
          ~session_key:"slack:C123" ~room_id:"C123" ()
      in
      Room_session_record.persist ~db record;
      let found = Room_session_record.get ~db ~id:record.id () in
      match found with
      | None -> Alcotest.fail "record not found"
      | Some f ->
          Alcotest.(check string) "id" record.id f.id;
          Alcotest.(check string) "created_at" record.created_at f.created_at;
          Alcotest.(check (option string))
            "session_key" record.session_key f.session_key;
          Alcotest.(check (option string)) "room_id" record.room_id f.room_id;
          Alcotest.(check string) "config_hash" record.config_hash f.config_hash;
          Alcotest.(check string)
            "access_snapshot_id" record.access_snapshot_id f.access_snapshot_id)

let test_persist_with_agent_config () =
  with_db (fun db ->
      let cfg = parse profile_config_json in
      let record =
        Room_session_record.create ~config:cfg ~access_snapshot_id:"snap_011"
          ~session_key:"slack:C100" ~room_id:"C100" ()
      in
      Room_session_record.persist ~db record;
      match Room_session_record.get ~db ~id:record.id () with
      | None -> Alcotest.fail "record not found"
      | Some f -> (
          match f.agent_config with
          | None -> Alcotest.fail "agent_config lost after roundtrip"
          | Some ac ->
              Alcotest.(check string) "profile_id" "vip" ac.profile_id;
              Alcotest.(check string) "model" "openai:gpt-5.4" ac.model;
              Alcotest.(check (list string))
                "allowed_tools" [ "vip_tool" ] ac.allowed_tools))

let test_persist_with_connector_context () =
  with_db (fun db ->
      let cfg = parse basic_config_json in
      let origin =
        Room_origin.make ~connector:"teams" ~workspace_id:"W1" ~room_id:"C200"
          ~requester_id:"U5" ~requester_name:"Bob" ~thread_id:"t1" ()
      in
      let record =
        Room_session_record.create ~config:cfg ~access_snapshot_id:"snap_012"
          ~origin ()
      in
      Room_session_record.persist ~db record;
      match Room_session_record.get ~db ~id:record.id () with
      | None -> Alcotest.fail "record not found"
      | Some f ->
          let cc = f.connector_context in
          Alcotest.(check (option string))
            "connector" (Some "teams") cc.connector;
          Alcotest.(check (option string)) "room_id" (Some "C200") cc.room_id;
          Alcotest.(check (option string))
            "requester_name" (Some "Bob") cc.requester_name;
          Alcotest.(check (option string)) "thread_id" (Some "t1") cc.thread_id)

let test_persist_with_delivery () =
  with_db (fun db ->
      let cfg = parse basic_config_json in
      let delivery : Room_session_record.delivery_snapshot =
        {
          state = "failed:network_error";
          last_update = "2026-06-29T12:00:00Z";
          message_id = Some "msg_456";
          error_detail = Some "Connection timed out";
        }
      in
      let record =
        Room_session_record.create ~config:cfg ~access_snapshot_id:"snap_013"
          ~delivery ~session_key:"slack:C1" ()
      in
      Room_session_record.persist ~db record;
      match Room_session_record.get ~db ~id:record.id () with
      | None -> Alcotest.fail "record not found"
      | Some f -> (
          match f.delivery with
          | None -> Alcotest.fail "delivery lost after roundtrip"
          | Some d ->
              Alcotest.(check string) "state" "failed:network_error" d.state;
              Alcotest.(check (option string))
                "message_id" (Some "msg_456") d.message_id;
              Alcotest.(check (option string))
                "error_detail" (Some "Connection timed out") d.error_detail))

let test_persist_with_links () =
  with_db (fun db ->
      let cfg = parse basic_config_json in
      let record =
        Room_session_record.create ~config:cfg ~access_snapshot_id:"snap_014"
          ~transcript_url:"https://tr.example.com/1"
          ~session_url:"https://s.example.com/1" ~session_key:"slack:C1" ()
      in
      Room_session_record.persist ~db record;
      match Room_session_record.get ~db ~id:record.id () with
      | None -> Alcotest.fail "record not found"
      | Some f ->
          Alcotest.(check (option string))
            "transcript_url" (Some "https://tr.example.com/1") f.transcript_url;
          Alcotest.(check (option string))
            "session_url" (Some "https://s.example.com/1") f.session_url)

let test_get_nonexistent () =
  with_db (fun db ->
      let result = Room_session_record.get ~db ~id:"nonexistent" () in
      Alcotest.(check bool) "None for missing" true (result = None))

let test_query_all () =
  with_db (fun db ->
      let cfg = parse basic_config_json in
      for i = 1 to 5 do
        let record =
          Room_session_record.create ~config:cfg
            ~access_snapshot_id:(Printf.sprintf "snap_q_%d" i)
            ~session_key:(Printf.sprintf "slack:C%d" i)
            ()
        in
        Room_session_record.persist ~db record
      done;
      let all = Room_session_record.query ~db () in
      Alcotest.(check int) "5 records" 5 (List.length all))

let test_query_by_room_id () =
  with_db (fun db ->
      let cfg = parse basic_config_json in
      let r1 =
        Room_session_record.create ~config:cfg ~access_snapshot_id:"s1"
          ~room_id:"C100" ()
      in
      let r2 =
        Room_session_record.create ~config:cfg ~access_snapshot_id:"s2"
          ~room_id:"C200" ()
      in
      let r3 =
        Room_session_record.create ~config:cfg ~access_snapshot_id:"s3"
          ~room_id:"C100" ()
      in
      Room_session_record.persist ~db r1;
      Room_session_record.persist ~db r2;
      Room_session_record.persist ~db r3;
      let c100 = Room_session_record.query ~db ~room_id:"C100" () in
      Alcotest.(check int) "2 C100 records" 2 (List.length c100);
      let c200 = Room_session_record.query ~db ~room_id:"C200" () in
      Alcotest.(check int) "1 C200 record" 1 (List.length c200))

let test_query_by_session_key () =
  with_db (fun db ->
      let cfg = parse basic_config_json in
      let r1 =
        Room_session_record.create ~config:cfg ~access_snapshot_id:"s1"
          ~session_key:"slack:C1" ()
      in
      let r2 =
        Room_session_record.create ~config:cfg ~access_snapshot_id:"s2"
          ~session_key:"discord:C2" ()
      in
      Room_session_record.persist ~db r1;
      Room_session_record.persist ~db r2;
      let slack = Room_session_record.query ~db ~session_key:"slack:C1" () in
      Alcotest.(check int) "1 slack" 1 (List.length slack);
      let discord =
        Room_session_record.query ~db ~session_key:"discord:C2" ()
      in
      Alcotest.(check int) "1 discord" 1 (List.length discord))

let test_query_by_config_hash () =
  with_db (fun db ->
      let cfg1 = parse {|{ "workspace": "/tmp/test1" }|} in
      let cfg2 = parse {|{ "workspace": "/tmp/test2" }|} in
      let r1 =
        Room_session_record.create ~config:cfg1 ~access_snapshot_id:"s1"
          ~session_key:"slack:C1" ()
      in
      let r2 =
        Room_session_record.create ~config:cfg2 ~access_snapshot_id:"s2"
          ~session_key:"slack:C2" ()
      in
      Room_session_record.persist ~db r1;
      Room_session_record.persist ~db r2;
      let h1 = Room_session_record.query ~db ~config_hash:r1.config_hash () in
      Alcotest.(check int) "1 with hash1" 1 (List.length h1))

let test_query_by_access_snapshot_id () =
  with_db (fun db ->
      let cfg = parse basic_config_json in
      let r1 =
        Room_session_record.create ~config:cfg ~access_snapshot_id:"snap_A"
          ~session_key:"slack:C1" ()
      in
      let r2 =
        Room_session_record.create ~config:cfg ~access_snapshot_id:"snap_B"
          ~session_key:"slack:C2" ()
      in
      Room_session_record.persist ~db r1;
      Room_session_record.persist ~db r2;
      let snap_a =
        Room_session_record.query ~db ~access_snapshot_id:"snap_A" ()
      in
      Alcotest.(check int) "1 snap_A" 1 (List.length snap_a))

let test_query_limit () =
  with_db (fun db ->
      let cfg = parse basic_config_json in
      for i = 1 to 10 do
        let record =
          Room_session_record.create ~config:cfg
            ~access_snapshot_id:(Printf.sprintf "snap_l_%d" i)
            ~session_key:(Printf.sprintf "slack:C%d" i)
            ()
        in
        Room_session_record.persist ~db record
      done;
      let five = Room_session_record.query ~db ~limit:5 () in
      Alcotest.(check int) "limit 5" 5 (List.length five);
      let twenty = Room_session_record.query ~db ~limit:20 () in
      Alcotest.(check int) "limit 20 returns 10" 10 (List.length twenty))

let test_query_empty () =
  with_db (fun db ->
      let all = Room_session_record.query ~db () in
      Alcotest.(check int) "empty" 0 (List.length all))

let test_get_latest_for_room () =
  with_db (fun db ->
      let cfg = parse basic_config_json in
      let r1 =
        Room_session_record.create ~config:cfg ~access_snapshot_id:"s1"
          ~room_id:"C500" ()
      in
      (* Small delay to ensure different timestamps *)
      Unix.sleepf 0.01;
      let r2 =
        Room_session_record.create ~config:cfg ~access_snapshot_id:"s2"
          ~room_id:"C500" ()
      in
      Room_session_record.persist ~db r1;
      Room_session_record.persist ~db r2;
      let latest =
        Room_session_record.get_latest_for_room ~db ~room_id:"C500" ()
      in
      match latest with
      | None -> Alcotest.fail "expected latest record"
      | Some l -> Alcotest.(check string) "latest id" r2.id l.id)

let test_get_latest_for_room_empty () =
  with_db (fun db ->
      let result =
        Room_session_record.get_latest_for_room ~db ~room_id:"no_room" ()
      in
      Alcotest.(check bool) "None for empty room" true (result = None))

let test_to_json () =
  let cfg = parse profile_config_json in
  let delivery : Room_session_record.delivery_snapshot =
    {
      state = "sent";
      last_update = "2026-06-29T10:00:00Z";
      message_id = Some "m1";
      error_detail = None;
    }
  in
  let origin =
    Room_origin.make ~connector:"slack" ~room_id:"C100" ~requester_id:"U1"
      ~requester_name:"Alice" ()
  in
  let record =
    Room_session_record.create ~config:cfg ~access_snapshot_id:"snap_json"
      ~origin ~delivery ~transcript_url:"https://tr" ~session_url:"https://s"
      ~session_key:"slack:C100" ~room_id:"C100" ()
  in
  let json = Room_session_record.to_json record in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "json id" record.id (json |> member "id" |> to_string);
  Alcotest.(check string)
    "json session_key" "slack:C100"
    (json |> member "session_key" |> to_string);
  Alcotest.(check string)
    "json room_id" "C100"
    (json |> member "room_id" |> to_string);
  Alcotest.(check string)
    "json access_snapshot_id" "snap_json"
    (json |> member "access_snapshot_id" |> to_string);
  Alcotest.(check bool)
    "json has agent_config" true
    (json |> member "agent_config" <> `Null);
  let ac = json |> member "agent_config" in
  Alcotest.(check string)
    "json agent profile_id" "vip"
    (ac |> member "profile_id" |> to_string);
  Alcotest.(check bool)
    "json has connector_context" true
    (json |> member "connector_context" <> `Null);
  let cc = json |> member "connector_context" in
  Alcotest.(check string)
    "json connector" "slack"
    (cc |> member "connector" |> to_string);
  Alcotest.(check bool)
    "json has delivery" true
    (json |> member "delivery" <> `Null);
  let d = json |> member "delivery" in
  Alcotest.(check string)
    "json delivery state" "sent"
    (d |> member "state" |> to_string);
  Alcotest.(check string)
    "json transcript_url" "https://tr"
    (json |> member "transcript_url" |> to_string);
  Alcotest.(check string)
    "json session_url" "https://s"
    (json |> member "session_url" |> to_string)

let test_to_json_minimal () =
  let cfg = parse basic_config_json in
  let record =
    Room_session_record.create ~config:cfg ~access_snapshot_id:"snap_min" ()
  in
  let json = Room_session_record.to_json record in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "json id" record.id (json |> member "id" |> to_string);
  Alcotest.(check bool)
    "no session_key" true
    (json |> member "session_key" = `Null);
  Alcotest.(check bool) "no room_id" true (json |> member "room_id" = `Null);
  Alcotest.(check bool)
    "no agent_config" true
    (json |> member "agent_config" = `Null);
  Alcotest.(check bool) "no delivery" true (json |> member "delivery" = `Null);
  Alcotest.(check bool)
    "no transcript_url" true
    (json |> member "transcript_url" = `Null);
  Alcotest.(check bool)
    "no session_url" true
    (json |> member "session_url" = `Null)

let test_assemble_and_persist () =
  with_db (fun db ->
      let cfg = parse profile_config_json in
      (* First create an access snapshot *)
      Access_snapshot.init_schema db;
      let snap =
        Access_snapshot.create ~config:cfg ~work_type:Room_turn
          ~session_key:"slack:C100" ~room_id:"C100" ()
      in
      Access_snapshot.persist ~db snap;
      let record =
        Room_session_record.assemble_and_persist ~db ~config:cfg
          ~access_snapshot_id:snap.id ~session_key:"slack:C100" ~room_id:"C100"
          ()
      in
      Alcotest.(check bool) "id non-empty" true (String.length record.id > 0);
      Alcotest.(check string) "snapshot id" snap.id record.access_snapshot_id;
      let found = Room_session_record.get ~db ~id:record.id () in
      Alcotest.(check bool) "persisted" true (Option.is_some found);
      match found with
      | None -> Alcotest.fail "not found"
      | Some f -> (
          Alcotest.(check (option string)) "room_id" (Some "C100") f.room_id;
          match f.agent_config with
          | None -> Alcotest.fail "agent_config missing"
          | Some ac -> Alcotest.(check string) "profile_id" "vip" ac.profile_id))

let test_assemble_with_full_origin () =
  with_db (fun db ->
      let cfg = parse profile_config_json in
      Access_snapshot.init_schema db;
      let snap =
        Access_snapshot.create ~config:cfg ~work_type:Room_turn
          ~session_key:"slack:C100" ~room_id:"C100" ()
      in
      Access_snapshot.persist ~db snap;
      let origin =
        Room_origin.make ~connector:"slack" ~workspace_id:"T1" ~room_id:"C100"
          ~requester_id:"U1" ~requester_name:"Alice"
          ~source_message_id:"1234.5678" ~thread_id:"1234.5678.000001"
          ~service_url:"https://hooks.slack.com" ()
      in
      let delivery : Room_session_record.delivery_snapshot =
        {
          state = "confirmed";
          last_update = "2026-06-29T10:00:00Z";
          message_id = Some "ts_1234";
          error_detail = None;
        }
      in
      let record =
        Room_session_record.assemble_and_persist ~db ~config:cfg
          ~access_snapshot_id:snap.id ~origin ~delivery
          ~transcript_url:"https://example.com/tr"
          ~session_url:"https://example.com/s" ~session_key:"slack:C100:U1"
          ~room_id:"C100" ()
      in
      let found = Room_session_record.get ~db ~id:record.id () in
      match found with
      | None -> Alcotest.fail "not found"
      | Some f -> (
          Alcotest.(check (option string))
            "session_key" (Some "slack:C100:U1") f.session_key;
          Alcotest.(check (option string)) "room_id" (Some "C100") f.room_id;
          let cc = f.connector_context in
          Alcotest.(check (option string))
            "connector" (Some "slack") cc.connector;
          Alcotest.(check (option string))
            "requester_name" (Some "Alice") cc.requester_name;
          Alcotest.(check (option string))
            "thread_id" (Some "1234.5678.000001") cc.thread_id;
          (match f.delivery with
          | None -> Alcotest.fail "delivery missing"
          | Some d ->
              Alcotest.(check string) "state" "confirmed" d.state;
              Alcotest.(check (option string))
                "message_id" (Some "ts_1234") d.message_id);
          Alcotest.(check (option string))
            "transcript_url" (Some "https://example.com/tr") f.transcript_url;
          Alcotest.(check (option string))
            "session_url" (Some "https://example.com/s") f.session_url;
          match f.agent_config with
          | None -> Alcotest.fail "agent_config missing"
          | Some ac -> Alcotest.(check string) "profile_id" "vip" ac.profile_id))

let test_immutable_after_persist () =
  with_db (fun db ->
      let cfg = parse basic_config_json in
      let record =
        Room_session_record.create ~config:cfg ~access_snapshot_id:"snap_imm"
          ~session_key:"slack:C1" ~room_id:"C1" ()
      in
      Room_session_record.persist ~db record;
      let found = Room_session_record.get ~db ~id:record.id () in
      match found with
      | None -> Alcotest.fail "not found"
      | Some f ->
          Alcotest.(check string) "id immutable" record.id f.id;
          Alcotest.(check string)
            "created_at immutable" record.created_at f.created_at;
          Alcotest.(check string)
            "config_hash immutable" record.config_hash f.config_hash;
          Alcotest.(check string)
            "access_snapshot_id immutable" record.access_snapshot_id
            f.access_snapshot_id)

let test_delete_before () =
  with_db (fun db ->
      let cfg = parse basic_config_json in
      let record =
        Room_session_record.create ~config:cfg ~access_snapshot_id:"snap_del"
          ~session_key:"slack:C1" ()
      in
      Room_session_record.persist ~db record;
      let deleted =
        Room_session_record.delete_before ~db
          ~before_timestamp:"2099-01-01T00:00:00Z" ()
      in
      Alcotest.(check bool) "deleted some" true (deleted > 0);
      let found = Room_session_record.get ~db ~id:record.id () in
      Alcotest.(check bool) "deleted" true (found = None))

let test_init_schema_idempotent () =
  let db = Memory.init ~db_path:":memory:" () in
  Room_session_record.init_schema db;
  Room_session_record.init_schema db;
  let cfg = parse basic_config_json in
  let record =
    Room_session_record.create ~config:cfg ~access_snapshot_id:"snap_idem"
      ~session_key:"slack:C1" ()
  in
  Room_session_record.persist ~db record;
  let found = Room_session_record.get ~db ~id:record.id () in
  Alcotest.(check bool) "works after double init" true (Option.is_some found);
  ignore (Sqlite3.db_close db)

let test_system_prompt_digest_deterministic () =
  let digest1 =
    Room_session_record.system_prompt_digest "Be helpful and concise"
  in
  let digest2 =
    Room_session_record.system_prompt_digest "Be helpful and concise"
  in
  Alcotest.(check string) "deterministic" digest1 digest2;
  Alcotest.(check bool) "hex length" true (String.length digest1 = 64);
  let digest3 = Room_session_record.system_prompt_digest "Different prompt" in
  Alcotest.(check bool) "different for different input" true (digest1 <> digest3)

let test_config_hash_matches_access_snapshot () =
  let cfg = parse basic_config_json in
  let snap_hash = Access_snapshot.config_hash cfg in
  let record =
    Room_session_record.create ~config:cfg ~access_snapshot_id:"snap_hash" ()
  in
  Alcotest.(check string) "config_hash matches" snap_hash record.config_hash

let test_connector_context_roundtrip_empty () =
  with_db (fun db ->
      let cfg = parse basic_config_json in
      let record =
        Room_session_record.create ~config:cfg ~access_snapshot_id:"snap_empty"
          ()
      in
      Room_session_record.persist ~db record;
      match Room_session_record.get ~db ~id:record.id () with
      | None -> Alcotest.fail "not found"
      | Some f ->
          Alcotest.(check bool)
            "empty connector_context preserved" true
            (f.connector_context = Room_session_record.empty_connector_context))

let test_query_ordered_by_created_at_desc () =
  with_db (fun db ->
      let cfg = parse basic_config_json in
      let r1 =
        Room_session_record.create ~config:cfg ~access_snapshot_id:"s1"
          ~room_id:"C1" ()
      in
      Unix.sleepf 0.01;
      let r2 =
        Room_session_record.create ~config:cfg ~access_snapshot_id:"s2"
          ~room_id:"C1" ()
      in
      Room_session_record.persist ~db r1;
      Room_session_record.persist ~db r2;
      let all = Room_session_record.query ~db ~room_id:"C1" () in
      match all with
      | [ first; second ] ->
          Alcotest.(check string) "newest first" r2.id first.id;
          Alcotest.(check string) "oldest second" r1.id second.id
      | _ -> Alcotest.fail "expected 2 records")

let suite =
  [
    Alcotest.test_case "create basic" `Quick test_create_basic;
    Alcotest.test_case "create with room_id" `Quick test_create_with_room_id;
    Alcotest.test_case "create with profile" `Quick test_create_with_profile;
    Alcotest.test_case "create with origin" `Quick test_create_with_origin;
    Alcotest.test_case "create with delivery" `Quick test_create_with_delivery;
    Alcotest.test_case "create with transcript and session urls" `Quick
      test_create_with_transcript_and_session_urls;
    Alcotest.test_case "create derives room from origin" `Quick
      test_create_derives_room_from_origin;
    Alcotest.test_case "persist and get" `Quick test_persist_and_get;
    Alcotest.test_case "persist with agent_config" `Quick
      test_persist_with_agent_config;
    Alcotest.test_case "persist with connector_context" `Quick
      test_persist_with_connector_context;
    Alcotest.test_case "persist with delivery" `Quick test_persist_with_delivery;
    Alcotest.test_case "persist with links" `Quick test_persist_with_links;
    Alcotest.test_case "get nonexistent" `Quick test_get_nonexistent;
    Alcotest.test_case "query all" `Quick test_query_all;
    Alcotest.test_case "query by room_id" `Quick test_query_by_room_id;
    Alcotest.test_case "query by session_key" `Quick test_query_by_session_key;
    Alcotest.test_case "query by config_hash" `Quick test_query_by_config_hash;
    Alcotest.test_case "query by access_snapshot_id" `Quick
      test_query_by_access_snapshot_id;
    Alcotest.test_case "query limit" `Quick test_query_limit;
    Alcotest.test_case "query empty" `Quick test_query_empty;
    Alcotest.test_case "get latest for room" `Quick test_get_latest_for_room;
    Alcotest.test_case "get latest for room empty" `Quick
      test_get_latest_for_room_empty;
    Alcotest.test_case "to_json" `Quick test_to_json;
    Alcotest.test_case "to_json minimal" `Quick test_to_json_minimal;
    Alcotest.test_case "assemble and persist" `Quick test_assemble_and_persist;
    Alcotest.test_case "assemble with full origin" `Quick
      test_assemble_with_full_origin;
    Alcotest.test_case "immutable after persist" `Quick
      test_immutable_after_persist;
    Alcotest.test_case "delete before" `Quick test_delete_before;
    Alcotest.test_case "init_schema idempotent" `Quick
      test_init_schema_idempotent;
    Alcotest.test_case "system_prompt_digest deterministic" `Quick
      test_system_prompt_digest_deterministic;
    Alcotest.test_case "config_hash matches access_snapshot" `Quick
      test_config_hash_matches_access_snapshot;
    Alcotest.test_case "connector_context roundtrip empty" `Quick
      test_connector_context_roundtrip_empty;
    Alcotest.test_case "query ordered by created_at desc" `Quick
      test_query_ordered_by_created_at_desc;
  ]
