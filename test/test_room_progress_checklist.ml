let with_db f = Test_helpers.with_memory_store f

let test_append_creates_planned_item () =
  with_db (fun db ->
      let item =
        Room_progress_checklist.append ~db ~task_id:1
          ~title:"Implement auth module" ()
      in
      Alcotest.(check int) "task_id" 1 item.task_id;
      Alcotest.(check string) "title" "Implement auth module" item.title;
      Alcotest.(check string)
        "state" "planned"
        (Room_progress_checklist.string_of_item_state item.state);
      Alcotest.(check string)
        "delivery" "pending"
        (Room_progress_checklist.string_of_delivery_state item.delivery_state);
      Alcotest.(check bool) "id positive" true (item.id > 0);
      Alcotest.(check bool)
        "last_update not empty" true
        (String.length item.last_update > 0))

let test_append_with_links () =
  with_db (fun db ->
      let item =
        Room_progress_checklist.append ~db ~task_id:2 ~title:"Run tests"
          ~transcript_url:"https://example.com/transcript"
          ~session_url:"https://example.com/session"
          ~session_record_id:"rsr_123_000001" ()
      in
      Alcotest.(check (option string))
        "transcript_url" (Some "https://example.com/transcript")
        item.transcript_url;
      Alcotest.(check (option string))
        "session_url" (Some "https://example.com/session") item.session_url;
      Alcotest.(check (option string))
        "session_record_id" (Some "rsr_123_000001") item.session_record_id)

let test_append_without_links () =
  with_db (fun db ->
      let item =
        Room_progress_checklist.append ~db ~task_id:3 ~title:"Deploy" ()
      in
      Alcotest.(check (option string))
        "transcript_url None" None item.transcript_url;
      Alcotest.(check (option string)) "session_url None" None item.session_url;
      Alcotest.(check (option string))
        "session_record_id None" None item.session_record_id)

let test_update_state_transitions () =
  with_db (fun db ->
      let item =
        Room_progress_checklist.append ~db ~task_id:4 ~title:"Write code" ()
      in
      Alcotest.(check string)
        "initial state" "planned"
        (Room_progress_checklist.string_of_item_state item.state);
      let updated =
        Room_progress_checklist.update_state ~db ~id:item.id
          ~state:Room_progress_checklist.Current ()
      in
      (match updated with
      | None -> Alcotest.fail "expected Some item after update"
      | Some u ->
          Alcotest.(check string)
            "current" "current"
            (Room_progress_checklist.string_of_item_state u.state));
      let blocked =
        Room_progress_checklist.update_state ~db ~id:item.id
          ~state:Room_progress_checklist.Blocked ()
      in
      (match blocked with
      | None -> Alcotest.fail "expected Some item after blocked"
      | Some b ->
          Alcotest.(check string)
            "blocked" "blocked"
            (Room_progress_checklist.string_of_item_state b.state));
      let done_item =
        Room_progress_checklist.update_state ~db ~id:item.id
          ~state:Room_progress_checklist.Done ()
      in
      (match done_item with
      | None -> Alcotest.fail "expected Some item after done"
      | Some d ->
          Alcotest.(check string)
            "done" "done"
            (Room_progress_checklist.string_of_item_state d.state));
      let final =
        Room_progress_checklist.update_state ~db ~id:item.id
          ~state:Room_progress_checklist.Final ()
      in
      match final with
      | None -> Alcotest.fail "expected Some item after final"
      | Some f ->
          Alcotest.(check string)
            "final" "final"
            (Room_progress_checklist.string_of_item_state f.state))

let test_update_state_nonexistent () =
  with_db (fun db ->
      let result =
        Room_progress_checklist.update_state ~db ~id:9999
          ~state:Room_progress_checklist.Current ()
      in
      Alcotest.(check bool) "None for missing" true (result = None))

let test_set_links () =
  with_db (fun db ->
      let item =
        Room_progress_checklist.append ~db ~task_id:5 ~title:"Review PR" ()
      in
      Alcotest.(check (option string))
        "no transcript initially" None item.transcript_url;
      Alcotest.(check (option string))
        "no session record initially" None item.session_record_id;
      let updated =
        Room_progress_checklist.set_links ~db ~id:item.id
          ~transcript_url:"https://example.com/tr" ~session_url:"https://s"
          ~session_record_id:"rsr_456_000001" ()
      in
      match updated with
      | None -> Alcotest.fail "expected Some after set_links"
      | Some u ->
          Alcotest.(check (option string))
            "transcript set" (Some "https://example.com/tr") u.transcript_url;
          Alcotest.(check (option string))
            "session set" (Some "https://s") u.session_url;
          Alcotest.(check (option string))
            "session record set" (Some "rsr_456_000001") u.session_record_id)

let test_set_links_partial () =
  with_db (fun db ->
      let item =
        Room_progress_checklist.append ~db ~task_id:6 ~title:"Debug" ()
      in
      let _ =
        Room_progress_checklist.set_links ~db ~id:item.id
          ~transcript_url:"https://t" ()
      in
      let fetched = Room_progress_checklist.get ~db ~id:item.id () in
      match fetched with
      | None -> Alcotest.fail "expected item"
      | Some u ->
          Alcotest.(check (option string))
            "transcript preserved" (Some "https://t") u.transcript_url;
          Alcotest.(check (option string))
            "session still None" None u.session_url;
          Alcotest.(check (option string))
            "session record still None" None u.session_record_id)

let test_set_delivery_state () =
  with_db (fun db ->
      let item =
        Room_progress_checklist.append ~db ~task_id:7 ~title:"Notify room" ()
      in
      Alcotest.(check string)
        "initial delivery" "pending"
        (Room_progress_checklist.string_of_delivery_state item.delivery_state);
      let sent =
        Room_progress_checklist.set_delivery_state ~db ~id:item.id
          ~delivery_state:Room_progress_checklist.Delivery_sent ()
      in
      match sent with
      | None -> Alcotest.fail "expected Some after set_delivery_state"
      | Some u -> (
          Alcotest.(check string)
            "sent" "sent"
            (Room_progress_checklist.string_of_delivery_state u.delivery_state);
          let confirmed =
            Room_progress_checklist.set_delivery_state ~db ~id:item.id
              ~delivery_state:Room_progress_checklist.Delivery_confirmed ()
          in
          match confirmed with
          | None -> Alcotest.fail "expected Some after confirmed"
          | Some c ->
              Alcotest.(check string)
                "confirmed" "confirmed"
                (Room_progress_checklist.string_of_delivery_state
                   c.delivery_state)))

let test_set_delivery_state_failed () =
  with_db (fun db ->
      let item =
        Room_progress_checklist.append ~db ~task_id:8 ~title:"Send update" ()
      in
      let failed =
        Room_progress_checklist.set_delivery_state ~db ~id:item.id
          ~delivery_state:
            (Room_progress_checklist.Delivery_failed "network error") ()
      in
      match failed with
      | None -> Alcotest.fail "expected Some after failed"
      | Some u ->
          let ds =
            Room_progress_checklist.string_of_delivery_state u.delivery_state
          in
          Alcotest.(check bool)
            "starts with failed" true
            (String.length ds > 6 && String.sub ds 0 6 = "failed"))

let test_get_by_id () =
  with_db (fun db ->
      let item =
        Room_progress_checklist.append ~db ~task_id:9 ~title:"Fetch me" ()
      in
      let fetched = Room_progress_checklist.get ~db ~id:item.id () in
      match fetched with
      | None -> Alcotest.fail "expected Some item"
      | Some f ->
          Alcotest.(check int) "same id" item.id f.id;
          Alcotest.(check string) "same title" "Fetch me" f.title)

let test_get_nonexistent () =
  with_db (fun db ->
      let result = Room_progress_checklist.get ~db ~id:9999 () in
      Alcotest.(check bool) "None for missing" true (result = None))

let test_query_by_task_ordering () =
  with_db (fun db ->
      let _ =
        Room_progress_checklist.append ~db ~task_id:10 ~title:"Step 1" ()
      in
      let _ =
        Room_progress_checklist.append ~db ~task_id:10 ~title:"Step 2" ()
      in
      let _ =
        Room_progress_checklist.append ~db ~task_id:10 ~title:"Step 3" ()
      in
      let items = Room_progress_checklist.query_by_task ~db ~task_id:10 () in
      Alcotest.(check int) "three items" 3 (List.length items);
      let titles = List.map (fun i -> i.Room_progress_checklist.title) items in
      Alcotest.(check (list string))
        "ordered by id"
        [ "Step 1"; "Step 2"; "Step 3" ]
        titles)

let test_query_by_task_isolation () =
  with_db (fun db ->
      let _ =
        Room_progress_checklist.append ~db ~task_id:11 ~title:"Task 11 item" ()
      in
      let _ =
        Room_progress_checklist.append ~db ~task_id:12 ~title:"Task 12 item" ()
      in
      let items_11 = Room_progress_checklist.query_by_task ~db ~task_id:11 () in
      let items_12 = Room_progress_checklist.query_by_task ~db ~task_id:12 () in
      Alcotest.(check int) "task 11 has 1" 1 (List.length items_11);
      Alcotest.(check int) "task 12 has 1" 1 (List.length items_12);
      Alcotest.(check string)
        "task 11 title" "Task 11 item"
        (List.hd items_11).Room_progress_checklist.title;
      Alcotest.(check string)
        "task 12 title" "Task 12 item"
        (List.hd items_12).Room_progress_checklist.title)

let test_query_empty_task () =
  with_db (fun db ->
      let items = Room_progress_checklist.query_by_task ~db ~task_id:999 () in
      Alcotest.(check int) "empty" 0 (List.length items))

let test_query_pending_delivery () =
  with_db (fun db ->
      let _i1 =
        Room_progress_checklist.append ~db ~task_id:13 ~title:"Pending" ()
      in
      let i2 =
        Room_progress_checklist.append ~db ~task_id:13 ~title:"Sent" ()
      in
      let i3 =
        Room_progress_checklist.append ~db ~task_id:13 ~title:"Confirmed" ()
      in
      let _ =
        Room_progress_checklist.set_delivery_state ~db ~id:i2.id
          ~delivery_state:Room_progress_checklist.Delivery_sent ()
      in
      let _ =
        Room_progress_checklist.set_delivery_state ~db ~id:i2.id
          ~delivery_state:Room_progress_checklist.Delivery_confirmed ()
      in
      let _ =
        Room_progress_checklist.set_delivery_state ~db ~id:i3.id
          ~delivery_state:Room_progress_checklist.Delivery_confirmed ()
      in
      let pending =
        Room_progress_checklist.query_pending_delivery ~db ~task_id:13 ()
      in
      Alcotest.(check int) "pending count" 1 (List.length pending);
      Alcotest.(check string)
        "pending title" "Pending"
        (List.hd pending).Room_progress_checklist.title)

let test_update_state_resets_delivery () =
  with_db (fun db ->
      let item =
        Room_progress_checklist.append ~db ~task_id:50 ~title:"Reset test" ()
      in
      let _ =
        Room_progress_checklist.set_delivery_state ~db ~id:item.id
          ~delivery_state:Room_progress_checklist.Delivery_confirmed ()
      in
      let updated =
        Room_progress_checklist.update_state ~db ~id:item.id
          ~state:Room_progress_checklist.Current ()
      in
      match updated with
      | None -> Alcotest.fail "expected Some after update_state"
      | Some u ->
          Alcotest.(check string)
            "delivery reset to pending" "pending"
            (Room_progress_checklist.string_of_delivery_state u.delivery_state))

let test_query_pending_delivery_includes_failed () =
  with_db (fun db ->
      let i1 =
        Room_progress_checklist.append ~db ~task_id:51 ~title:"Failed item" ()
      in
      let _ =
        Room_progress_checklist.set_delivery_state ~db ~id:i1.id
          ~delivery_state:
            (Room_progress_checklist.Delivery_failed "network error") ()
      in
      let pending =
        Room_progress_checklist.query_pending_delivery ~db ~task_id:51 ()
      in
      Alcotest.(check int) "failed included" 1 (List.length pending);
      Alcotest.(check string)
        "failed title" "Failed item"
        (List.hd pending).Room_progress_checklist.title)

let test_set_delivery_state_updates_timestamp () =
  with_db (fun db ->
      let item =
        Room_progress_checklist.append ~db ~task_id:52 ~title:"Timestamp test"
          ()
      in
      let original_ts = item.last_update in
      let updated =
        Room_progress_checklist.set_delivery_state ~db ~id:item.id
          ~delivery_state:Room_progress_checklist.Delivery_sent ()
      in
      match updated with
      | None -> Alcotest.fail "expected Some after set_delivery_state"
      | Some u ->
          Alcotest.(check bool)
            "timestamp updated" true
            (u.last_update >= original_ts))

let test_set_links_resets_delivery () =
  with_db (fun db ->
      let item =
        Room_progress_checklist.append ~db ~task_id:53 ~title:"Links reset test"
          ()
      in
      let _ =
        Room_progress_checklist.set_delivery_state ~db ~id:item.id
          ~delivery_state:Room_progress_checklist.Delivery_confirmed ()
      in
      let updated =
        Room_progress_checklist.set_links ~db ~id:item.id
          ~transcript_url:"https://new-tr" ()
      in
      match updated with
      | None -> Alcotest.fail "expected Some after set_links"
      | Some u ->
          Alcotest.(check string)
            "delivery reset" "pending"
            (Room_progress_checklist.string_of_delivery_state u.delivery_state);
          Alcotest.(check (option string))
            "transcript set" (Some "https://new-tr") u.transcript_url;
          let pending =
            Room_progress_checklist.query_pending_delivery ~db ~task_id:53 ()
          in
          Alcotest.(check int) "pending count" 1 (List.length pending))

let test_render_single_item () =
  let item =
    {
      Room_progress_checklist.id = 1;
      task_id = 1;
      title = "Implement auth";
      state = Room_progress_checklist.Done;
      transcript_url = Some "https://example.com/tr";
      session_url = None;
      session_record_id = None;
      last_update = "2026-06-29T10:00:00Z";
      delivery_state = Room_progress_checklist.Delivery_confirmed;
    }
  in
  let rendered = Room_progress_checklist.render_item item in
  Alcotest.(check bool)
    "has icon" true
    (Test_helpers.string_contains rendered "[x]");
  Alcotest.(check bool)
    "has title" true
    (Test_helpers.string_contains rendered "Implement auth");
  Alcotest.(check bool)
    "has transcript" true
    (Test_helpers.string_contains rendered "https://example.com/tr");
  Alcotest.(check bool)
    "no session" false
    (Test_helpers.string_contains rendered "session:")

let test_render_item_with_session () =
  let item =
    {
      Room_progress_checklist.id = 2;
      task_id = 1;
      title = "Review code";
      state = Room_progress_checklist.Current;
      transcript_url = None;
      session_url = Some "https://example.com/s";
      session_record_id = None;
      last_update = "2026-06-29T10:00:00Z";
      delivery_state = Room_progress_checklist.Delivery_pending;
    }
  in
  let rendered = Room_progress_checklist.render_item item in
  Alcotest.(check bool)
    "has current icon" true
    (Test_helpers.string_contains rendered "[~]");
  Alcotest.(check bool)
    "has session" true
    (Test_helpers.string_contains rendered "session:");
  Alcotest.(check bool)
    "has url" true
    (Test_helpers.string_contains rendered "https://example.com/s")

let test_render_item_with_session_record () =
  let item =
    {
      Room_progress_checklist.id = 3;
      task_id = 1;
      title = "Build feature";
      state = Room_progress_checklist.Done;
      transcript_url = Some "https://example.com/tr";
      session_url = None;
      session_record_id = Some "rsr_789_000001";
      last_update = "2026-06-29T10:00:00Z";
      delivery_state = Room_progress_checklist.Delivery_confirmed;
    }
  in
  let rendered = Room_progress_checklist.render_item item in
  Alcotest.(check bool)
    "has record" true
    (Test_helpers.string_contains rendered "record:");
  Alcotest.(check bool)
    "has record id" true
    (Test_helpers.string_contains rendered "rsr_789_000001");
  Alcotest.(check bool)
    "has transcript" true
    (Test_helpers.string_contains rendered "https://example.com/tr")

let test_render_list () =
  with_db (fun db ->
      let _ =
        Room_progress_checklist.append ~db ~task_id:20 ~title:"Step one" ()
      in
      let _ =
        Room_progress_checklist.append ~db ~task_id:20 ~title:"Step two" ()
      in
      let items = Room_progress_checklist.query_by_task ~db ~task_id:20 () in
      let rendered = Room_progress_checklist.render items in
      Alcotest.(check bool)
        "has step one" true
        (Test_helpers.string_contains rendered "Step one");
      Alcotest.(check bool)
        "has step two" true
        (Test_helpers.string_contains rendered "Step two"))

let test_render_empty () =
  let rendered = Room_progress_checklist.render [] in
  Alcotest.(check string) "empty placeholder" "(no checklist items)" rendered

let test_render_summary () =
  with_db (fun db ->
      let i1 = Room_progress_checklist.append ~db ~task_id:30 ~title:"A" () in
      let _ = Room_progress_checklist.append ~db ~task_id:30 ~title:"B" () in
      let i3 = Room_progress_checklist.append ~db ~task_id:30 ~title:"C" () in
      let _ =
        Room_progress_checklist.update_state ~db ~id:i1.id
          ~state:Room_progress_checklist.Done ()
      in
      let _ =
        Room_progress_checklist.update_state ~db ~id:i3.id
          ~state:Room_progress_checklist.Current ()
      in
      let items = Room_progress_checklist.query_by_task ~db ~task_id:30 () in
      let summary = Room_progress_checklist.render_summary items in
      Alcotest.(check bool)
        "has done count" true
        (Test_helpers.string_contains summary "1 done");
      Alcotest.(check bool)
        "has current count" true
        (Test_helpers.string_contains summary "1 current");
      Alcotest.(check bool)
        "has planned count" true
        (Test_helpers.string_contains summary "1 planned"))

let test_render_summary_empty () =
  Alcotest.(check string)
    "empty summary" "no items"
    (Room_progress_checklist.render_summary [])

let test_delete_by_task () =
  with_db (fun db ->
      let _ = Room_progress_checklist.append ~db ~task_id:40 ~title:"X" () in
      let _ = Room_progress_checklist.append ~db ~task_id:40 ~title:"Y" () in
      let _ = Room_progress_checklist.append ~db ~task_id:41 ~title:"Z" () in
      let deleted = Room_progress_checklist.delete_by_task ~db ~task_id:40 () in
      Alcotest.(check int) "deleted 2" 2 deleted;
      let remaining_40 =
        Room_progress_checklist.query_by_task ~db ~task_id:40 ()
      in
      let remaining_41 =
        Room_progress_checklist.query_by_task ~db ~task_id:41 ()
      in
      Alcotest.(check int) "task 40 empty" 0 (List.length remaining_40);
      Alcotest.(check int) "task 41 intact" 1 (List.length remaining_41))

let test_delete_before () =
  with_db (fun db ->
      let _ = Room_progress_checklist.append ~db ~task_id:50 ~title:"Old" () in
      let deleted =
        Room_progress_checklist.delete_before ~db
          ~before_timestamp:"2099-01-01T00:00:00Z" ()
      in
      Alcotest.(check bool) "deleted some" true (deleted > 0);
      let remaining =
        Room_progress_checklist.query_by_task ~db ~task_id:50 ()
      in
      Alcotest.(check int) "all deleted" 0 (List.length remaining))

let test_json_roundtrip () =
  with_db (fun db ->
      let _ =
        Room_progress_checklist.append ~db ~task_id:60 ~title:"JSON test"
          ~transcript_url:"https://t" ~session_url:"https://s"
          ~session_record_id:"rsr_999_000001" ()
      in
      let items = Room_progress_checklist.query_by_task ~db ~task_id:60 () in
      let json_str = Room_progress_checklist.json_string_of_items items in
      let parsed =
        try Yojson.Safe.from_string json_str
        with Yojson.Json_error msg ->
          Alcotest.failf "JSON parse error: %s" msg
      in
      match parsed with
      | `List [ obj ] ->
          let title =
            Yojson.Safe.Util.member "title" obj |> Yojson.Safe.Util.to_string
          in
          Alcotest.(check string) "json title" "JSON test" title;
          let state =
            Yojson.Safe.Util.member "state" obj |> Yojson.Safe.Util.to_string
          in
          Alcotest.(check string) "json state" "planned" state;
          let tr =
            Yojson.Safe.Util.member "transcript_url" obj
            |> Yojson.Safe.Util.to_string
          in
          Alcotest.(check string) "json transcript" "https://t" tr;
          let sr =
            Yojson.Safe.Util.member "session_record_id" obj
            |> Yojson.Safe.Util.to_string
          in
          Alcotest.(check string) "json session record" "rsr_999_000001" sr
      | _ -> Alcotest.fail "expected single-element JSON array")

let test_state_string_roundtrip () =
  let open Room_progress_checklist in
  let states = [ Planned; Current; Blocked; Done; Final ] in
  List.iter
    (fun state ->
      let s = string_of_item_state state in
      let roundtripped = item_state_of_string s in
      match roundtripped with
      | Some rt ->
          Alcotest.(check string)
            (Printf.sprintf "roundtrip %s" s)
            (string_of_item_state state)
            (string_of_item_state rt)
      | None -> Alcotest.failf "item_state_of_string returned None for %s" s)
    states

let test_delivery_state_string_roundtrip () =
  let open Room_progress_checklist in
  let states =
    [ Delivery_pending; Delivery_sent; Delivery_confirmed; Delivery_failed "x" ]
  in
  List.iter
    (fun ds ->
      let s = string_of_delivery_state ds in
      let roundtripped = delivery_state_of_string s in
      match roundtripped with
      | Some rt ->
          Alcotest.(check string)
            (Printf.sprintf "roundtrip %s" s)
            (string_of_delivery_state ds)
            (string_of_delivery_state rt)
      | None -> Alcotest.failf "delivery_state_of_string returned None for %s" s)
    states

let test_item_state_of_string_invalid () =
  Alcotest.(check bool)
    "invalid state" true
    (Room_progress_checklist.item_state_of_string "not_a_state" = None)

let test_delivery_state_of_string_invalid () =
  Alcotest.(check bool)
    "invalid delivery" true
    (Room_progress_checklist.delivery_state_of_string "nonsense" = None)

let test_migration_adds_schema () =
  Test_helpers.with_temp_dir (fun dir ->
      let db_path = Filename.concat dir "memory.db" in
      let db = Sqlite3.db_open db_path in
      Memory.exec_exn db
        "CREATE TABLE schema_version (version INTEGER NOT NULL)";
      Memory.exec_exn db "INSERT INTO schema_version (version) VALUES (37)";
      ignore (Sqlite3.db_close db);
      let migrated = Memory.init ~db_path () in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close migrated))
        (fun () ->
          Alcotest.(check int)
            "schema version current" Memory.schema_version
            (Test_helpers.query_single_int migrated
               "SELECT version FROM schema_version");
          let item =
            Room_progress_checklist.append ~db:migrated ~task_id:1
              ~title:"migration test" ()
          in
          Alcotest.(check string) "title" "migration test" item.title))

let test_migration_adds_session_record_id () =
  Test_helpers.with_temp_dir (fun dir ->
      let db_path = Filename.concat dir "memory.db" in
      let db = Sqlite3.db_open db_path in
      Memory.exec_exn db
        "CREATE TABLE schema_version (version INTEGER NOT NULL)";
      Memory.exec_exn db "INSERT INTO schema_version (version) VALUES (37)";
      (* Create old table without session_record_id *)
      Memory.exec_exn db
        "CREATE TABLE room_progress_checklist (id INTEGER PRIMARY KEY \
         AUTOINCREMENT,task_id INTEGER NOT NULL,title TEXT NOT NULL,state TEXT \
         NOT NULL DEFAULT 'planned',transcript_url TEXT,session_url \
         TEXT,last_update TEXT NOT NULL DEFAULT \
         (datetime('now')),delivery_state TEXT NOT NULL DEFAULT 'pending')";
      (* Insert an item without session_record_id *)
      Memory.exec_exn db
        "INSERT INTO room_progress_checklist (task_id, title) VALUES (1, 'old \
         item')";
      ignore (Sqlite3.db_close db);
      (* Re-open with migration *)
      let migrated = Memory.init ~db_path () in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close migrated))
        (fun () ->
          (* Old item should still be queryable *)
          let items =
            Room_progress_checklist.query_by_task ~db:migrated ~task_id:1 ()
          in
          Alcotest.(check int) "old item exists" 1 (List.length items);
          let old_item = List.hd items in
          Alcotest.(check string) "old title" "old item" old_item.title;
          Alcotest.(check (option string))
            "old item has no session_record_id" None old_item.session_record_id;
          (* New items should support session_record_id *)
          let new_item =
            Room_progress_checklist.append ~db:migrated ~task_id:2
              ~title:"new item" ~session_record_id:"rsr_new_001" ()
          in
          Alcotest.(check (option string))
            "new item has session_record_id" (Some "rsr_new_001")
            new_item.session_record_id))

let test_state_icon () =
  let open Room_progress_checklist in
  Alcotest.(check string) "planned icon" "[ ]" (state_icon Planned);
  Alcotest.(check string) "current icon" "[~]" (state_icon Current);
  Alcotest.(check string) "blocked icon" "[!]" (state_icon Blocked);
  Alcotest.(check string) "done icon" "[x]" (state_icon Done);
  Alcotest.(check string) "final icon" "[*]" (state_icon Final)

let test_is_terminal_item_state () =
  let open Room_progress_checklist in
  Alcotest.(check bool)
    "planned not terminal" false
    (is_terminal_item_state Planned);
  Alcotest.(check bool)
    "current not terminal" false
    (is_terminal_item_state Current);
  Alcotest.(check bool)
    "blocked not terminal" false
    (is_terminal_item_state Blocked);
  Alcotest.(check bool) "done not terminal" false (is_terminal_item_state Done);
  Alcotest.(check bool) "final is terminal" true (is_terminal_item_state Final)

let test_full_lifecycle () =
  with_db (fun db ->
      (* Create a checklist item *)
      let item =
        Room_progress_checklist.append ~db ~task_id:100
          ~title:"Implement feature" ~transcript_url:"https://tr"
          ~session_record_id:"rsr_lifecycle_001" ()
      in
      Alcotest.(check string)
        "initial" "planned"
        (Room_progress_checklist.string_of_item_state item.state);
      Alcotest.(check (option string))
        "session record preserved" (Some "rsr_lifecycle_001")
        item.session_record_id;
      (* Move to current *)
      let current =
        Room_progress_checklist.update_state ~db ~id:item.id
          ~state:Room_progress_checklist.Current ()
      in
      (match current with
      | None -> Alcotest.fail "expected current"
      | Some c ->
          Alcotest.(check string)
            "current" "current"
            (Room_progress_checklist.string_of_item_state c.state));
      (* Mark delivery sent *)
      let _ =
        Room_progress_checklist.set_delivery_state ~db ~id:item.id
          ~delivery_state:Room_progress_checklist.Delivery_sent ()
      in
      (* Block on dependency *)
      let blocked =
        Room_progress_checklist.update_state ~db ~id:item.id
          ~state:Room_progress_checklist.Blocked ()
      in
      (match blocked with
      | None -> Alcotest.fail "expected blocked"
      | Some b ->
          Alcotest.(check string)
            "blocked" "blocked"
            (Room_progress_checklist.string_of_item_state b.state));
      (* Resume and complete *)
      let _ =
        Room_progress_checklist.update_state ~db ~id:item.id
          ~state:Room_progress_checklist.Current ()
      in
      let done_item =
        Room_progress_checklist.update_state ~db ~id:item.id
          ~state:Room_progress_checklist.Done ()
      in
      (match done_item with
      | None -> Alcotest.fail "expected done"
      | Some d ->
          Alcotest.(check string)
            "done" "done"
            (Room_progress_checklist.string_of_item_state d.state));
      (* Finalize *)
      let final =
        Room_progress_checklist.update_state ~db ~id:item.id
          ~state:Room_progress_checklist.Final ()
      in
      match final with
      | None -> Alcotest.fail "expected final"
      | Some f ->
          Alcotest.(check string)
            "final" "final"
            (Room_progress_checklist.string_of_item_state f.state);
          Alcotest.(check (option string))
            "transcript preserved" (Some "https://tr") f.transcript_url;
          Alcotest.(check (option string))
            "session record preserved" (Some "rsr_lifecycle_001")
            f.session_record_id;
          (* Verify delivery state reset to pending after state transition *)
          Alcotest.(check string)
            "delivery pending" "pending"
            (Room_progress_checklist.string_of_delivery_state f.delivery_state);
          (* Verify renderable *)
          let items =
            Room_progress_checklist.query_by_task ~db ~task_id:100 ()
          in
          let rendered = Room_progress_checklist.render items in
          Alcotest.(check bool)
            "rendered has final icon" true
            (Test_helpers.string_contains rendered "[*]"))

let suite =
  [
    Alcotest.test_case "append creates planned item" `Quick
      test_append_creates_planned_item;
    Alcotest.test_case "append with links" `Quick test_append_with_links;
    Alcotest.test_case "append without links" `Quick test_append_without_links;
    Alcotest.test_case "update state transitions" `Quick
      test_update_state_transitions;
    Alcotest.test_case "update state nonexistent" `Quick
      test_update_state_nonexistent;
    Alcotest.test_case "set links" `Quick test_set_links;
    Alcotest.test_case "set links partial" `Quick test_set_links_partial;
    Alcotest.test_case "set delivery state" `Quick test_set_delivery_state;
    Alcotest.test_case "set delivery state failed" `Quick
      test_set_delivery_state_failed;
    Alcotest.test_case "get by id" `Quick test_get_by_id;
    Alcotest.test_case "get nonexistent" `Quick test_get_nonexistent;
    Alcotest.test_case "query by task ordering" `Quick
      test_query_by_task_ordering;
    Alcotest.test_case "query by task isolation" `Quick
      test_query_by_task_isolation;
    Alcotest.test_case "query empty task" `Quick test_query_empty_task;
    Alcotest.test_case "query pending delivery" `Quick
      test_query_pending_delivery;
    Alcotest.test_case "update state resets delivery" `Quick
      test_update_state_resets_delivery;
    Alcotest.test_case "query pending delivery includes failed" `Quick
      test_query_pending_delivery_includes_failed;
    Alcotest.test_case "set delivery state updates timestamp" `Quick
      test_set_delivery_state_updates_timestamp;
    Alcotest.test_case "set links resets delivery" `Quick
      test_set_links_resets_delivery;
    Alcotest.test_case "render single item" `Quick test_render_single_item;
    Alcotest.test_case "render item with session" `Quick
      test_render_item_with_session;
    Alcotest.test_case "render item with session record" `Quick
      test_render_item_with_session_record;
    Alcotest.test_case "render list" `Quick test_render_list;
    Alcotest.test_case "render empty" `Quick test_render_empty;
    Alcotest.test_case "render summary" `Quick test_render_summary;
    Alcotest.test_case "render summary empty" `Quick test_render_summary_empty;
    Alcotest.test_case "delete by task" `Quick test_delete_by_task;
    Alcotest.test_case "delete before" `Quick test_delete_before;
    Alcotest.test_case "json roundtrip" `Quick test_json_roundtrip;
    Alcotest.test_case "state string roundtrip" `Quick
      test_state_string_roundtrip;
    Alcotest.test_case "delivery state string roundtrip" `Quick
      test_delivery_state_string_roundtrip;
    Alcotest.test_case "item state of string invalid" `Quick
      test_item_state_of_string_invalid;
    Alcotest.test_case "delivery state of string invalid" `Quick
      test_delivery_state_of_string_invalid;
    Alcotest.test_case "migration adds schema" `Quick test_migration_adds_schema;
    Alcotest.test_case "migration adds session_record_id" `Quick
      test_migration_adds_session_record_id;
    Alcotest.test_case "state icon" `Quick test_state_icon;
    Alcotest.test_case "is terminal item state" `Quick
      test_is_terminal_item_state;
    Alcotest.test_case "full lifecycle" `Quick test_full_lifecycle;
  ]
