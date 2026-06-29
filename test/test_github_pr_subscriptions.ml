let with_db f =
  let db = Memory.init ~db_path:":memory:" () in
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let test_add_subscription () =
  with_db (fun db ->
      let sub =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:42 ~profile_id:1 ()
      in
      Alcotest.(check string) "room_id" "room-1" sub.room_id;
      Alcotest.(check string) "repo" "owner/repo" sub.repo;
      Alcotest.(check int) "pr_number" 42 sub.pr_number;
      Alcotest.(check int) "profile_id" 1 sub.profile_id;
      Alcotest.(check bool)
        "on_open default" true sub.notification_preferences.on_open;
      Alcotest.(check bool)
        "on_close default" true sub.notification_preferences.on_close;
      Alcotest.(check bool)
        "on_comment default" true sub.notification_preferences.on_comment;
      Alcotest.(check bool)
        "on_review default" true sub.notification_preferences.on_review;
      Alcotest.(check bool)
        "on_status default" true sub.notification_preferences.on_status;
      Alcotest.(check bool)
        "on_merge default" true sub.notification_preferences.on_merge)

let test_add_with_custom_preferences () =
  with_db (fun db ->
      let prefs =
        {
          Github_pr_subscriptions.on_open = true;
          on_close = false;
          on_comment = true;
          on_review = false;
          on_status = true;
          on_merge = false;
        }
      in
      let sub =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:42 ~profile_id:1 ~notification_preferences:prefs ()
      in
      Alcotest.(check bool) "on_open" true sub.notification_preferences.on_open;
      Alcotest.(check bool)
        "on_close" false sub.notification_preferences.on_close;
      Alcotest.(check bool)
        "on_comment" true sub.notification_preferences.on_comment;
      Alcotest.(check bool)
        "on_review" false sub.notification_preferences.on_review;
      Alcotest.(check bool)
        "on_status" true sub.notification_preferences.on_status;
      Alcotest.(check bool)
        "on_merge" false sub.notification_preferences.on_merge)

let test_add_upserts_existing () =
  with_db (fun db ->
      let _sub1 =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:42 ~profile_id:1 ()
      in
      let sub2 =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:42 ~profile_id:2 ()
      in
      Alcotest.(check int) "profile_id updated" 2 sub2.profile_id;
      let all = Github_pr_subscriptions.find_by_room ~db ~room_id:"room-1" in
      Alcotest.(check int) "still one subscription" 1 (List.length all))

let test_find () =
  with_db (fun db ->
      let _sub =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:42 ~profile_id:1 ()
      in
      let found =
        Github_pr_subscriptions.find ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:42
      in
      match found with
      | Some sub -> Alcotest.(check int) "pr_number" 42 sub.pr_number
      | None -> Alcotest.fail "expected to find subscription")

let test_find_not_found () =
  with_db (fun db ->
      let found =
        Github_pr_subscriptions.find ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:99
      in
      match found with
      | Some _ -> Alcotest.fail "expected None"
      | None -> Alcotest.(check bool) "not found" true true)

let test_remove () =
  with_db (fun db ->
      let _sub =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:42 ~profile_id:1 ()
      in
      let removed =
        Github_pr_subscriptions.remove ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:42
      in
      Alcotest.(check bool) "removed" true removed;
      let found =
        Github_pr_subscriptions.find ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:42
      in
      match found with
      | Some _ -> Alcotest.fail "subscription should be removed"
      | None -> Alcotest.(check bool) "not found after remove" true true)

let test_remove_not_found () =
  with_db (fun db ->
      let removed =
        Github_pr_subscriptions.remove ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:99
      in
      Alcotest.(check bool) "not removed" false removed)

let test_update_preferences () =
  with_db (fun db ->
      let _sub =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:42 ~profile_id:1 ()
      in
      let new_prefs =
        {
          Github_pr_subscriptions.on_open = false;
          on_close = false;
          on_comment = false;
          on_review = false;
          on_status = false;
          on_merge = true;
        }
      in
      let updated =
        Github_pr_subscriptions.update_preferences ~db ~room_id:"room-1"
          ~repo:"owner/repo" ~pr_number:42 ~preferences:new_prefs
      in
      Alcotest.(check bool)
        "on_open" false updated.notification_preferences.on_open;
      Alcotest.(check bool)
        "on_close" false updated.notification_preferences.on_close;
      Alcotest.(check bool)
        "on_comment" false updated.notification_preferences.on_comment;
      Alcotest.(check bool)
        "on_review" false updated.notification_preferences.on_review;
      Alcotest.(check bool)
        "on_status" false updated.notification_preferences.on_status;
      Alcotest.(check bool)
        "on_merge" true updated.notification_preferences.on_merge)

let test_update_preferences_not_found () =
  with_db (fun db ->
      let new_prefs =
        Github_pr_subscriptions.default_notification_preferences
      in
      match
        Github_pr_subscriptions.update_preferences ~db ~room_id:"room-1"
          ~repo:"owner/repo" ~pr_number:99 ~preferences:new_prefs
      with
      | _ -> Alcotest.fail "expected Not_found"
      | exception Not_found ->
          Alcotest.(check bool) "raised Not_found" true true)

let test_find_by_room () =
  with_db (fun db ->
      let _sub1 =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:1 ~profile_id:1 ()
      in
      let _sub2 =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:2 ~profile_id:1 ()
      in
      let _sub3 =
        Github_pr_subscriptions.add ~db ~room_id:"room-2" ~repo:"owner/repo"
          ~pr_number:1 ~profile_id:2 ()
      in
      let room1_subs =
        Github_pr_subscriptions.find_by_room ~db ~room_id:"room-1"
      in
      Alcotest.(check int) "room-1 count" 2 (List.length room1_subs);
      let room2_subs =
        Github_pr_subscriptions.find_by_room ~db ~room_id:"room-2"
      in
      Alcotest.(check int) "room-2 count" 1 (List.length room2_subs);
      let room3_subs =
        Github_pr_subscriptions.find_by_room ~db ~room_id:"room-3"
      in
      Alcotest.(check int) "room-3 count" 0 (List.length room3_subs))

let test_find_by_repo () =
  with_db (fun db ->
      let _sub1 =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo1"
          ~pr_number:1 ~profile_id:1 ()
      in
      let _sub2 =
        Github_pr_subscriptions.add ~db ~room_id:"room-2" ~repo:"owner/repo1"
          ~pr_number:2 ~profile_id:2 ()
      in
      let _sub3 =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo2"
          ~pr_number:1 ~profile_id:1 ()
      in
      let repo1_subs =
        Github_pr_subscriptions.find_by_repo ~db ~repo:"owner/repo1"
      in
      Alcotest.(check int) "repo1 count" 2 (List.length repo1_subs);
      let repo2_subs =
        Github_pr_subscriptions.find_by_repo ~db ~repo:"owner/repo2"
      in
      Alcotest.(check int) "repo2 count" 1 (List.length repo2_subs))

let test_find_by_repo_pr () =
  with_db (fun db ->
      let _sub1 =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:42 ~profile_id:1 ()
      in
      let _sub2 =
        Github_pr_subscriptions.add ~db ~room_id:"room-2" ~repo:"owner/repo"
          ~pr_number:42 ~profile_id:2 ()
      in
      let _sub3 =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:99 ~profile_id:1 ()
      in
      let pr42_subs =
        Github_pr_subscriptions.find_by_repo_pr ~db ~repo:"owner/repo"
          ~pr_number:42
      in
      Alcotest.(check int) "pr42 count" 2 (List.length pr42_subs);
      let pr99_subs =
        Github_pr_subscriptions.find_by_repo_pr ~db ~repo:"owner/repo"
          ~pr_number:99
      in
      Alcotest.(check int) "pr99 count" 1 (List.length pr99_subs))

let test_delete_by_room () =
  with_db (fun db ->
      let _sub1 =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:1 ~profile_id:1 ()
      in
      let _sub2 =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:2 ~profile_id:1 ()
      in
      let _sub3 =
        Github_pr_subscriptions.add ~db ~room_id:"room-2" ~repo:"owner/repo"
          ~pr_number:1 ~profile_id:2 ()
      in
      let deleted =
        Github_pr_subscriptions.delete_by_room ~db ~room_id:"room-1"
      in
      Alcotest.(check int) "deleted count" 2 deleted;
      let room1_subs =
        Github_pr_subscriptions.find_by_room ~db ~room_id:"room-1"
      in
      Alcotest.(check int) "room-1 empty" 0 (List.length room1_subs);
      let room2_subs =
        Github_pr_subscriptions.find_by_room ~db ~room_id:"room-2"
      in
      Alcotest.(check int) "room-2 intact" 1 (List.length room2_subs))

let test_delete_by_repo () =
  with_db (fun db ->
      let _sub1 =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo1"
          ~pr_number:1 ~profile_id:1 ()
      in
      let _sub2 =
        Github_pr_subscriptions.add ~db ~room_id:"room-2" ~repo:"owner/repo1"
          ~pr_number:2 ~profile_id:2 ()
      in
      let _sub3 =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo2"
          ~pr_number:1 ~profile_id:1 ()
      in
      let deleted =
        Github_pr_subscriptions.delete_by_repo ~db ~repo:"owner/repo1"
      in
      Alcotest.(check int) "deleted count" 2 deleted;
      let repo1_subs =
        Github_pr_subscriptions.find_by_repo ~db ~repo:"owner/repo1"
      in
      Alcotest.(check int) "repo1 empty" 0 (List.length repo1_subs);
      let repo2_subs =
        Github_pr_subscriptions.find_by_repo ~db ~repo:"owner/repo2"
      in
      Alcotest.(check int) "repo2 intact" 1 (List.length repo2_subs))

let test_count () =
  with_db (fun db ->
      let _sub1 =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:1 ~profile_id:1 ()
      in
      let _sub2 =
        Github_pr_subscriptions.add ~db ~room_id:"room-2" ~repo:"owner/repo"
          ~pr_number:2 ~profile_id:2 ()
      in
      let count = Github_pr_subscriptions.count ~db () in
      Alcotest.(check int) "count" 2 count)

let test_should_notify () =
  let open Github_pr_subscriptions in
  let sub =
    {
      id = 1;
      room_id = "room-1";
      repo = "owner/repo";
      pr_number = 42;
      profile_id = 1;
      enabled = true;
      notification_preferences =
        {
          on_open = true;
          on_close = false;
          on_comment = true;
          on_review = false;
          on_status = true;
          on_merge = false;
        };
      created_at = "";
      updated_at = "";
    }
  in
  Alcotest.(check bool)
    "opened" true
    (should_notify ~subscription:sub ~event_type:"opened");
  Alcotest.(check bool)
    "reopened" true
    (should_notify ~subscription:sub ~event_type:"reopened");
  Alcotest.(check bool)
    "closed" false
    (should_notify ~subscription:sub ~event_type:"closed");
  Alcotest.(check bool)
    "comment" true
    (should_notify ~subscription:sub ~event_type:"comment");
  Alcotest.(check bool)
    "review" false
    (should_notify ~subscription:sub ~event_type:"review");
  Alcotest.(check bool)
    "status" true
    (should_notify ~subscription:sub ~event_type:"status");
  Alcotest.(check bool)
    "merged" false
    (should_notify ~subscription:sub ~event_type:"merged");
  Alcotest.(check bool)
    "unknown event" true
    (should_notify ~subscription:sub ~event_type:"labeled")

let test_json_roundtrip () =
  with_db (fun db ->
      let prefs =
        {
          Github_pr_subscriptions.on_open = false;
          on_close = true;
          on_comment = false;
          on_review = true;
          on_status = false;
          on_merge = true;
        }
      in
      let sub =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:42 ~profile_id:1 ~notification_preferences:prefs ()
      in
      let json_str = Github_pr_subscriptions.subscription_to_string sub in
      let parsed =
        try Yojson.Safe.from_string json_str with Yojson.Json_error _ -> `Null
      in
      match parsed with
      | `Assoc fields ->
          Alcotest.(check bool)
            "has room_id" true
            (List.mem_assoc "room_id" fields);
          Alcotest.(check bool) "has repo" true (List.mem_assoc "repo" fields);
          Alcotest.(check bool)
            "has pr_number" true
            (List.mem_assoc "pr_number" fields);
          Alcotest.(check bool)
            "has notification_preferences" true
            (List.mem_assoc "notification_preferences" fields)
      | _ -> Alcotest.fail "expected JSON object")

let test_migration_adds_schema () =
  Test_helpers.with_temp_dir (fun dir ->
      let db_path = Filename.concat dir "memory.db" in
      let db = Sqlite3.db_open db_path in
      Memory.exec_exn db
        "CREATE TABLE schema_version (version INTEGER NOT NULL)";
      Memory.exec_exn db "INSERT INTO schema_version (version) VALUES (40)";
      ignore (Sqlite3.db_close db);
      let migrated = Memory.init ~db_path () in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close migrated))
        (fun () ->
          Alcotest.(check int)
            "schema version current" Memory.schema_version
            (Test_helpers.query_single_int migrated
               "SELECT version FROM schema_version");
          let sub =
            Github_pr_subscriptions.add ~db:migrated ~room_id:"room-1"
              ~repo:"owner/repo" ~pr_number:1 ~profile_id:1 ()
          in
          Alcotest.(check int) "pr_number" 1 sub.pr_number))

let suite =
  [
    Alcotest.test_case "add subscription" `Quick test_add_subscription;
    Alcotest.test_case "add with custom preferences" `Quick
      test_add_with_custom_preferences;
    Alcotest.test_case "add upserts existing" `Quick test_add_upserts_existing;
    Alcotest.test_case "find" `Quick test_find;
    Alcotest.test_case "find not found" `Quick test_find_not_found;
    Alcotest.test_case "remove" `Quick test_remove;
    Alcotest.test_case "remove not found" `Quick test_remove_not_found;
    Alcotest.test_case "update preferences" `Quick test_update_preferences;
    Alcotest.test_case "update preferences not found" `Quick
      test_update_preferences_not_found;
    Alcotest.test_case "find by room" `Quick test_find_by_room;
    Alcotest.test_case "find by repo" `Quick test_find_by_repo;
    Alcotest.test_case "find by repo pr" `Quick test_find_by_repo_pr;
    Alcotest.test_case "delete by room" `Quick test_delete_by_room;
    Alcotest.test_case "delete by repo" `Quick test_delete_by_repo;
    Alcotest.test_case "count" `Quick test_count;
    Alcotest.test_case "should notify" `Quick test_should_notify;
    Alcotest.test_case "json roundtrip" `Quick test_json_roundtrip;
    Alcotest.test_case "migration adds schema" `Quick test_migration_adds_schema;
  ]
