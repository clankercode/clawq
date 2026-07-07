let with_db f = Test_helpers.with_memory_store f
let make_empty_config () = Runtime_config.default

let test_scope_snapshot_from_config () =
  with_db (fun db ->
      let cfg = make_empty_config () in
      let room_id = "telegram:room-123" in
      let exp = Room_audit_export.generate ~cfg ~db ~room_id () in
      Alcotest.(check string) "room_id" room_id exp.room_id;
      Alcotest.(check string)
        "scope" "unknown"
        (Room_policy.room_scope_to_string exp.scope_snapshot.scope);
      Alcotest.(check bool) "no binding" false exp.scope_snapshot.binding_active;
      Alcotest.(check int) "no events" 0 exp.total_count)

let test_categorize_memory_events () =
  with_db (fun db ->
      let cfg = make_empty_config () in
      let room_id = "room-mem" in
      let emit ~room_id:rid ~event_type ~actor ~metadata =
        ignore
          (Room_activity_ledger.append ~db ~room_id:rid ~event_type
             ~timestamp:(Room_activity_ledger.timestamp_now ())
             ~actor ~metadata)
      in
      emit ~room_id ~event_type:"memory_saved" ~actor:"user"
        ~metadata:(`Assoc [ ("memory_id", `Int 1) ]);
      emit ~room_id ~event_type:"scope_granted" ~actor:"admin"
        ~metadata:(`Assoc []);
      emit ~room_id ~event_type:"team_grant_added" ~actor:"admin"
        ~metadata:(`Assoc []);
      let exp = Room_audit_export.generate ~cfg ~db ~room_id () in
      Alcotest.(check int) "3 events" 3 exp.total_count;
      let mem_count =
        Option.value ~default:0 (List.assoc_opt "memory" exp.category_counts)
      in
      Alcotest.(check int) "all memory" 3 mem_count)

let test_categorize_github_events () =
  with_db (fun db ->
      let cfg = make_empty_config () in
      let room_id = "room-gh" in
      ignore
        (Room_activity_ledger.record_github_update_delivered ~db ~room_id
           ~delivery_id:"d1" ~repo:"org/repo" ~pr_number:42
           ~event_type:"pull_request" ~payload_summary:"opened" ());
      ignore
        (Room_activity_ledger.record_github_update_skipped ~db ~room_id
           ~delivery_id:"d2" ~repo:"org/repo" ~pr_number:43
           ~event_type:"pull_request" ~reason:"no subscription"
           ~payload_summary:"closed" ());
      ignore
        (Room_activity_ledger.record_github_update_denied ~db ~room_id
           ~delivery_id:"d3" ~repo:"org/repo" ~pr_number:44
           ~event_type:"pull_request" ~deny_reason:"quiet hours"
           ~payload_summary:"merged" ());
      let exp = Room_audit_export.generate ~cfg ~db ~room_id () in
      Alcotest.(check int) "3 events" 3 exp.total_count;
      let gh_count =
        Option.value ~default:0 (List.assoc_opt "github" exp.category_counts)
      in
      Alcotest.(check int) "all github" 3 gh_count)

let test_categorize_delivery_events () =
  with_db (fun db ->
      let cfg = make_empty_config () in
      let room_id = "room-dlv" in
      let _event =
        Room_activity_ledger.record_delivery_attempt ~db ~room_id
          ~connector:"teams" ~task_id:1 ()
      in
      let _event =
        Room_activity_ledger.record_delivery_success ~db ~room_id
          ~connector:"teams" ~task_id:1 ~message_id:"msg-123" ~thread_id:"t-1"
          ()
      in
      let exp = Room_audit_export.generate ~cfg ~db ~room_id () in
      Alcotest.(check int) "2 events" 2 exp.total_count;
      let dlv_count =
        Option.value ~default:0 (List.assoc_opt "delivery" exp.category_counts)
      in
      Alcotest.(check int) "all delivery" 2 dlv_count)

let test_categorize_mixed_events () =
  with_db (fun db ->
      let cfg = make_empty_config () in
      let room_id = "room-mixed" in
      (* Memory event *)
      ignore
        (Room_activity_ledger.append ~db ~room_id ~event_type:"memory_saved"
           ~timestamp:(Room_activity_ledger.timestamp_now ())
           ~actor:"user" ~metadata:(`Assoc []));
      (* GitHub event *)
      ignore
        (Room_activity_ledger.record_github_update_delivered ~db ~room_id
           ~delivery_id:"d1" ~repo:"org/repo" ~pr_number:1
           ~event_type:"pull_request" ~payload_summary:"opened" ());
      (* Delivery event *)
      ignore
        (Room_activity_ledger.record_delivery_failure ~db ~room_id
           ~connector:"slack" ~task_id:5 ~error:"timeout" ());
      (* Admin event *)
      ignore
        (Room_activity_ledger.append ~db ~room_id ~event_type:"admin_denied"
           ~timestamp:(Room_activity_ledger.timestamp_now ())
           ~actor:"cli" ~metadata:(`Assoc []));
      let exp = Room_audit_export.generate ~cfg ~db ~room_id () in
      Alcotest.(check int) "4 events" 4 exp.total_count;
      Alcotest.(check bool)
        "has memory" true
        (List.mem_assoc "memory" exp.category_counts);
      Alcotest.(check bool)
        "has github" true
        (List.mem_assoc "github" exp.category_counts);
      Alcotest.(check bool)
        "has delivery" true
        (List.mem_assoc "delivery" exp.category_counts);
      Alcotest.(check bool)
        "has setup" true
        (List.mem_assoc "setup" exp.category_counts))

let test_events_only_from_specified_room () =
  with_db (fun db ->
      let cfg = make_empty_config () in
      ignore
        (Room_activity_ledger.append ~db ~room_id:"room-A"
           ~event_type:"memory_saved"
           ~timestamp:(Room_activity_ledger.timestamp_now ())
           ~actor:"user" ~metadata:(`Assoc []));
      ignore
        (Room_activity_ledger.append ~db ~room_id:"room-B"
           ~event_type:"delivery_failure"
           ~timestamp:(Room_activity_ledger.timestamp_now ())
           ~actor:"connector" ~metadata:(`Assoc []));
      let exp_a = Room_audit_export.generate ~cfg ~db ~room_id:"room-A" () in
      Alcotest.(check int) "room-A events" 1 exp_a.total_count;
      let exp_b = Room_audit_export.generate ~cfg ~db ~room_id:"room-B" () in
      Alcotest.(check int) "room-B events" 1 exp_b.total_count)

let test_json_export_roundtrip () =
  with_db (fun db ->
      let cfg = make_empty_config () in
      let room_id = "room-json" in
      ignore
        (Room_activity_ledger.append ~db ~room_id ~event_type:"memory_saved"
           ~timestamp:(Room_activity_ledger.timestamp_now ())
           ~actor:"user"
           ~metadata:(`Assoc [ ("key", `String "value") ]));
      let exp = Room_audit_export.generate ~cfg ~db ~room_id () in
      let json_str = Room_audit_export.export_to_json_string exp in
      let json = Yojson.Safe.from_string json_str in
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "room_id in json" room_id
        (json |> member "room_id" |> to_string);
      Alcotest.(check int)
        "total_count" 1
        (json |> member "total_count" |> to_int);
      let events = json |> member "events" |> to_list in
      Alcotest.(check int) "1 event in json" 1 (List.length events);
      let first = List.hd events in
      Alcotest.(check string)
        "category" "memory"
        (first |> member "category" |> to_string);
      Alcotest.(check bool)
        "has metadata_redacted" true
        (first |> member "metadata_redacted" <> `Null))

let test_jsonl_export_format () =
  with_db (fun db ->
      let cfg = make_empty_config () in
      let room_id = "room-jsonl" in
      ignore
        (Room_activity_ledger.append ~db ~room_id ~event_type:"memory_saved"
           ~timestamp:(Room_activity_ledger.timestamp_now ())
           ~actor:"user" ~metadata:(`Assoc []));
      ignore
        (Room_activity_ledger.append ~db ~room_id ~event_type:"delivery_success"
           ~timestamp:(Room_activity_ledger.timestamp_now ())
           ~actor:"slack" ~metadata:(`Assoc []));
      let exp = Room_audit_export.generate ~cfg ~db ~room_id () in
      let jsonl = Room_audit_export.export_to_jsonl exp in
      let lines = String.split_on_char '\n' jsonl in
      Alcotest.(check int) "3 lines (header + 2 events)" 3 (List.length lines);
      (* Verify each line is valid JSON *)
      List.iter
        (fun line ->
          let _json = Yojson.Safe.from_string line in
          ())
        lines;
      (* Verify header has scope snapshot *)
      let header_json = Yojson.Safe.from_string (List.hd lines) in
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "header type" "header"
        (header_json |> member "type" |> to_string);
      Alcotest.(check bool)
        "has scope_snapshot" true
        (header_json |> member "scope_snapshot" <> `Null))

let test_text_format_contains_sections () =
  with_db (fun db ->
      let cfg = make_empty_config () in
      let room_id = "room-text" in
      ignore
        (Room_activity_ledger.append ~db ~room_id ~event_type:"memory_saved"
           ~timestamp:(Room_activity_ledger.timestamp_now ())
           ~actor:"user" ~metadata:(`Assoc []));
      let exp = Room_audit_export.generate ~cfg ~db ~room_id () in
      let text = Room_audit_export.format_text exp in
      Alcotest.(check bool)
        "has room id" true
        (let re = Str.regexp_string room_id in
         try
           ignore (Str.search_forward re text 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "has scope snapshot" true
        (let re = Str.regexp_string "Scope Snapshot" in
         try
           ignore (Str.search_forward re text 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "has event summary" true
        (let re = Str.regexp_string "Event Summary" in
         try
           ignore (Str.search_forward re text 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "has events section" true
        (let re = Str.regexp_string "Events" in
         try
           ignore (Str.search_forward re text 0);
           true
         with Not_found -> false))

let test_text_format_empty_room () =
  with_db (fun db ->
      let cfg = make_empty_config () in
      let room_id = "room-empty" in
      let exp = Room_audit_export.generate ~cfg ~db ~room_id () in
      let text = Room_audit_export.format_text exp in
      Alcotest.(check bool)
        "shows no events message" true
        (let re = Str.regexp_string "No governance events" in
         try
           ignore (Str.search_forward re text 0);
           true
         with Not_found -> false))

let test_redact_metadata_preserves_structure () =
  let metadata =
    `Assoc
      [
        ("memory_id", `Int 42);
        ("reference", `String "some-long-reference-string-here");
        ("task_id", `Int 7);
        ("nested", `Assoc [ ("ref", `String "another-ref") ]);
      ]
  in
  let redacted = Room_audit_export.redact_metadata metadata in
  let open Yojson.Safe.Util in
  Alcotest.(check int)
    "memory_id preserved" 42
    (redacted |> member "memory_id" |> to_int);
  Alcotest.(check int)
    "task_id preserved" 7
    (redacted |> member "task_id" |> to_int);
  let ref_val = redacted |> member "reference" |> to_string in
  Alcotest.(check bool) "reference redacted" true (String.contains ref_val '*');
  Alcotest.(check bool) "reference not empty" true (String.length ref_val > 0)

let test_summary_line () =
  with_db (fun db ->
      let cfg = make_empty_config () in
      let room_id = "room-summary" in
      ignore
        (Room_activity_ledger.append ~db ~room_id ~event_type:"memory_saved"
           ~timestamp:(Room_activity_ledger.timestamp_now ())
           ~actor:"user" ~metadata:(`Assoc []));
      let exp = Room_audit_export.generate ~cfg ~db ~room_id () in
      let summary = Room_audit_export.summary_line ~export:exp in
      Alcotest.(check bool)
        "has Audit prefix" true
        (let re = Str.regexp_string "Audit:" in
         try
           ignore (Str.search_forward re summary 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "has event count" true
        (let re = Str.regexp_string "1 events" in
         try
           ignore (Str.search_forward re summary 0);
           true
         with Not_found -> false))

let test_scope_snapshot_dm () =
  with_db (fun db ->
      let cfg = make_empty_config () in
      (* DM-style room ID: connector:@user *)
      let room_id = "slack:@U12345" in
      let exp = Room_audit_export.generate ~cfg ~db ~room_id () in
      Alcotest.(check string)
        "scope" "dm"
        (Room_policy.room_scope_to_string exp.scope_snapshot.scope))

let test_scope_snapshot_unknown () =
  with_db (fun db ->
      let cfg = make_empty_config () in
      let room_id = "unknown-room" in
      let exp = Room_audit_export.generate ~cfg ~db ~room_id () in
      Alcotest.(check string)
        "scope" "unknown"
        (Room_policy.room_scope_to_string exp.scope_snapshot.scope))

let test_categorize_teams_lifecycle_events () =
  with_db (fun db ->
      let cfg = make_empty_config () in
      let room_id = "room-teams-lifecycle" in
      let ts = Room_activity_ledger.timestamp_now () in
      List.iter
        (fun et ->
          ignore
            (Room_activity_ledger.append ~db ~room_id ~event_type:et
               ~timestamp:ts ~actor:"teams" ~metadata:(`Assoc [])))
        [
          "teams_delivery_scheduled";
          "teams_delivery_generated";
          "teams_delivery_attempted";
          "teams_delivery_transport_accepted";
          "teams_delivery_message_id_recorded";
          "teams_delivery_edit_failed";
          "teams_delivery_fallback_sent";
          "teams_delivery_failed";
          "teams_delivery_user_visible_unconfirmed";
        ];
      let exp = Room_audit_export.generate ~cfg ~db ~room_id () in
      Alcotest.(check int) "9 events" 9 exp.total_count;
      let dlv_count =
        Option.value ~default:0 (List.assoc_opt "delivery" exp.category_counts)
      in
      Alcotest.(check int) "all delivery" 9 dlv_count)

let test_categorize_ambient_delivery_events () =
  with_db (fun db ->
      let cfg = make_empty_config () in
      let room_id = "room-ambient" in
      let ts = Room_activity_ledger.timestamp_now () in
      ignore
        (Room_activity_ledger.append ~db ~room_id ~event_type:"ambient_delivery"
           ~timestamp:ts ~actor:"ambient_watcher" ~metadata:(`Assoc []));
      ignore
        (Room_activity_ledger.append ~db ~room_id
           ~event_type:"ambient_delivery_failed" ~timestamp:ts
           ~actor:"ambient_watcher" ~metadata:(`Assoc []));
      let exp = Room_audit_export.generate ~cfg ~db ~room_id () in
      Alcotest.(check int) "2 events" 2 exp.total_count;
      let dlv_count =
        Option.value ~default:0 (List.assoc_opt "delivery" exp.category_counts)
      in
      Alcotest.(check int) "both delivery" 2 dlv_count)

let test_redact_metadata_redacts_id_fields () =
  let metadata =
    `Assoc
      [
        ("memory_id", `Int 42);
        ("snapshot_id", `String "snap-abcdef123456");
        ("delivery_id", `String "del-abcdef123456");
        ("message_id", `String "msg-abcdef123456");
        ("scope_key", `String "room:my-scope-key");
        ("activity_id", `String "act-abcdef123456");
        ("tracking_id", `String "track-abcdef1234");
        ("access_snapshot_id", `String "asnp-abcdef1234");
        ("item_id", `String "item-abcdef1234");
        ("source_message_id", `String "smid-abcdef1234");
        ("connector", `String "teams");
      ]
  in
  let redacted = Room_audit_export.redact_metadata metadata in
  let open Yojson.Safe.Util in
  (* Non-redacted fields preserved exactly *)
  Alcotest.(check int)
    "memory_id preserved" 42
    (redacted |> member "memory_id" |> to_int);
  Alcotest.(check string)
    "connector preserved" "teams"
    (redacted |> member "connector" |> to_string);
  (* Redacted fields contain asterisks *)
  List.iter
    (fun field ->
      let val_str = redacted |> member field |> to_string in
      Alcotest.(check bool)
        (field ^ " contains asterisk")
        true
        (String.contains val_str '*'))
    [
      "snapshot_id";
      "delivery_id";
      "message_id";
      "scope_key";
      "activity_id";
      "tracking_id";
      "access_snapshot_id";
      "item_id";
      "source_message_id";
    ]

let suite =
  [
    Alcotest.test_case "scope snapshot from config" `Quick
      test_scope_snapshot_from_config;
    Alcotest.test_case "categorize memory events" `Quick
      test_categorize_memory_events;
    Alcotest.test_case "categorize github events" `Quick
      test_categorize_github_events;
    Alcotest.test_case "categorize delivery events" `Quick
      test_categorize_delivery_events;
    Alcotest.test_case "categorize mixed events" `Quick
      test_categorize_mixed_events;
    Alcotest.test_case "events only from specified room" `Quick
      test_events_only_from_specified_room;
    Alcotest.test_case "json export roundtrip" `Quick test_json_export_roundtrip;
    Alcotest.test_case "jsonl export format" `Quick test_jsonl_export_format;
    Alcotest.test_case "text format contains sections" `Quick
      test_text_format_contains_sections;
    Alcotest.test_case "text format empty room" `Quick
      test_text_format_empty_room;
    Alcotest.test_case "redact metadata preserves structure" `Quick
      test_redact_metadata_preserves_structure;
    Alcotest.test_case "summary line" `Quick test_summary_line;
    Alcotest.test_case "scope snapshot dm" `Quick test_scope_snapshot_dm;
    Alcotest.test_case "scope snapshot unknown" `Quick
      test_scope_snapshot_unknown;
    Alcotest.test_case "categorize teams lifecycle events" `Quick
      test_categorize_teams_lifecycle_events;
    Alcotest.test_case "categorize ambient delivery events" `Quick
      test_categorize_ambient_delivery_events;
    Alcotest.test_case "redact metadata redacts id fields" `Quick
      test_redact_metadata_redacts_id_fields;
  ]
