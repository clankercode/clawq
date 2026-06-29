let with_db f =
  let db = Memory.init ~db_path:":memory:" () in
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let test_insert_and_find_by_github () =
  with_db (fun db ->
      let inserted =
        Room_github_backlinks.insert ~db ~repo:"owner/repo" ~pr_number:42
          ~github_item_type:Room_github_backlinks.Pr_comment
          ~github_item_id:"comment-123"
          ~github_url:"https://github.com/owner/repo/pull/42#issuecomment-123"
          ~room_id:"room-1" ~room_item_type:Room_github_backlinks.Message
          ~room_item_id:"msg-456"
          ~direction:Room_github_backlinks.Github_to_room
          ~relationship:Room_github_backlinks.Subscription_delivery ()
      in
      Alcotest.(check bool) "inserted" true inserted;
      let found =
        Room_github_backlinks.find_by_github ~db ~repo:"owner/repo"
          ~github_item_type:Room_github_backlinks.Pr_comment
          ~github_item_id:"comment-123" ()
      in
      Alcotest.(check int) "found one" 1 (List.length found);
      let bl = List.hd found in
      Alcotest.(check string) "repo" "owner/repo" bl.repo;
      Alcotest.(check (option int)) "pr_number" (Some 42) bl.pr_number;
      Alcotest.(check string) "room_id" "room-1" bl.room_id;
      Alcotest.(check string)
        "github_item_type" "pr_comment"
        (Room_github_backlinks.github_item_type_to_string bl.github_item_type);
      Alcotest.(check string)
        "direction" "github_to_room"
        (Room_github_backlinks.direction_to_string bl.direction);
      Alcotest.(check string)
        "relationship" "subscription_delivery"
        (Room_github_backlinks.relationship_to_string bl.relationship))

let test_insert_is_idempotent () =
  with_db (fun db ->
      let first =
        Room_github_backlinks.insert ~db ~repo:"owner/repo" ~pr_number:1
          ~github_item_type:Room_github_backlinks.Pr_comment
          ~github_item_id:"comment-1" ~room_id:"room-1"
          ~room_item_type:Room_github_backlinks.Message ~room_item_id:"msg-1"
          ~direction:Room_github_backlinks.Github_to_room
          ~relationship:Room_github_backlinks.Subscription_delivery ()
      in
      let second =
        Room_github_backlinks.insert ~db ~repo:"owner/repo" ~pr_number:1
          ~github_item_type:Room_github_backlinks.Pr_comment
          ~github_item_id:"comment-1" ~room_id:"room-1"
          ~room_item_type:Room_github_backlinks.Message ~room_item_id:"msg-1"
          ~direction:Room_github_backlinks.Github_to_room
          ~relationship:Room_github_backlinks.Subscription_delivery ()
      in
      Alcotest.(check bool) "first inserted" true first;
      Alcotest.(check bool) "second not inserted (duplicate)" false second;
      let found =
        Room_github_backlinks.find_by_github ~db ~repo:"owner/repo"
          ~github_item_type:Room_github_backlinks.Pr_comment ()
      in
      Alcotest.(check int) "only one record" 1 (List.length found))

let test_find_by_room () =
  with_db (fun db ->
      ignore
        (Room_github_backlinks.insert ~db ~repo:"owner/repo" ~pr_number:1
           ~github_item_type:Room_github_backlinks.Pr_comment ~room_id:"room-1"
           ~room_item_type:Room_github_backlinks.Message ~room_item_id:"msg-1"
           ~direction:Room_github_backlinks.Github_to_room
           ~relationship:Room_github_backlinks.Subscription_delivery ());
      ignore
        (Room_github_backlinks.insert ~db ~repo:"owner/repo" ~pr_number:2
           ~github_item_type:Room_github_backlinks.Pr_comment ~room_id:"room-1"
           ~room_item_type:Room_github_backlinks.Background_task
           ~room_item_id:"task-1"
           ~direction:Room_github_backlinks.Room_to_github
           ~relationship:Room_github_backlinks.Triggered_run ());
      ignore
        (Room_github_backlinks.insert ~db ~repo:"other/repo" ~pr_number:3
           ~github_item_type:Room_github_backlinks.Pr_comment ~room_id:"room-2"
           ~room_item_type:Room_github_backlinks.Message
           ~direction:Room_github_backlinks.Github_to_room
           ~relationship:Room_github_backlinks.Subscription_delivery ());
      (* All room-1 backlinks *)
      let room1_all =
        Room_github_backlinks.find_by_room ~db ~room_id:"room-1" ()
      in
      Alcotest.(check int) "room-1 all" 2 (List.length room1_all);
      (* Filter by room_item_type *)
      let room1_tasks =
        Room_github_backlinks.find_by_room ~db ~room_id:"room-1"
          ~room_item_type:Room_github_backlinks.Background_task ()
      in
      Alcotest.(check int) "room-1 tasks" 1 (List.length room1_tasks);
      (* Filter by room_item_id *)
      let room1_msg =
        Room_github_backlinks.find_by_room ~db ~room_id:"room-1"
          ~room_item_id:"msg-1" ()
      in
      Alcotest.(check int) "room-1 msg-1" 1 (List.length room1_msg))

let test_find_by_repo_pr () =
  with_db (fun db ->
      ignore
        (Room_github_backlinks.insert ~db ~repo:"owner/repo" ~pr_number:42
           ~github_item_type:Room_github_backlinks.Pr_comment
           ~github_item_id:"comment-1" ~room_id:"room-1"
           ~room_item_type:Room_github_backlinks.Message
           ~direction:Room_github_backlinks.Github_to_room
           ~relationship:Room_github_backlinks.Subscription_delivery ());
      ignore
        (Room_github_backlinks.insert ~db ~repo:"owner/repo" ~pr_number:42
           ~github_item_type:Room_github_backlinks.Check_run
           ~github_item_id:"check-1" ~room_id:"room-2"
           ~room_item_type:Room_github_backlinks.Message
           ~direction:Room_github_backlinks.Github_to_room
           ~relationship:Room_github_backlinks.Ci_notification ());
      ignore
        (Room_github_backlinks.insert ~db ~repo:"owner/repo" ~pr_number:99
           ~github_item_type:Room_github_backlinks.Pr_comment
           ~github_item_id:"comment-2" ~room_id:"room-1"
           ~room_item_type:Room_github_backlinks.Message
           ~direction:Room_github_backlinks.Github_to_room
           ~relationship:Room_github_backlinks.Subscription_delivery ());
      let pr42 =
        Room_github_backlinks.find_by_repo_pr ~db ~repo:"owner/repo"
          ~pr_number:42 ()
      in
      Alcotest.(check int) "PR #42 backlinks" 2 (List.length pr42);
      let pr99 =
        Room_github_backlinks.find_by_repo_pr ~db ~repo:"owner/repo"
          ~pr_number:99 ()
      in
      Alcotest.(check int) "PR #99 backlinks" 1 (List.length pr99))

let test_count_by_room () =
  with_db (fun db ->
      for i = 1 to 5 do
        ignore
          (Room_github_backlinks.insert ~db ~repo:"owner/repo" ~pr_number:i
             ~github_item_type:Room_github_backlinks.Pr_comment
             ~room_id:"room-1" ~room_item_type:Room_github_backlinks.Message
             ~room_item_id:(Printf.sprintf "msg-%d" i)
             ~direction:Room_github_backlinks.Github_to_room
             ~relationship:Room_github_backlinks.Subscription_delivery ())
      done;
      ignore
        (Room_github_backlinks.insert ~db ~repo:"owner/repo" ~pr_number:1
           ~github_item_type:Room_github_backlinks.Pr_comment ~room_id:"room-2"
           ~room_item_type:Room_github_backlinks.Message
           ~direction:Room_github_backlinks.Github_to_room
           ~relationship:Room_github_backlinks.Subscription_delivery ());
      Alcotest.(check int)
        "room-1 count" 5
        (Room_github_backlinks.count_by_room ~db ~room_id:"room-1" ());
      Alcotest.(check int)
        "room-2 count" 1
        (Room_github_backlinks.count_by_room ~db ~room_id:"room-2" ());
      Alcotest.(check int)
        "room-3 count" 0
        (Room_github_backlinks.count_by_room ~db ~room_id:"room-3" ()))

let test_delete_before () =
  with_db (fun db ->
      (* Insert with explicit timestamps *)
      let insert_with_ts ts id =
        let sql =
          "INSERT INTO room_github_backlinks (repo, github_item_type, \
           github_item_id, room_id, room_item_type, direction, relationship, \
           created_at) VALUES ('owner/repo', 'pr_comment', '" ^ id
          ^ "', 'room-1', 'message', 'github_to_room', \
             'subscription_delivery', ?)"
        in
        let stmt = Sqlite3.prepare db sql in
        Fun.protect
          ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
          (fun () ->
            ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT ts));
            ignore (Sqlite3.step stmt))
      in
      insert_with_ts "2026-06-20T10:00:00Z" "del-1";
      insert_with_ts "2026-06-25T10:00:00Z" "del-2";
      insert_with_ts "2026-06-29T10:00:00Z" "del-3";
      let deleted =
        Room_github_backlinks.delete_before ~db
          ~before_timestamp:"2026-06-25T10:00:00Z" ()
      in
      Alcotest.(check int) "deleted count" 1 deleted;
      let remaining =
        Room_github_backlinks.find_by_room ~db ~room_id:"room-1" ()
      in
      Alcotest.(check int) "remaining count" 2 (List.length remaining))

let test_json_serialization () =
  with_db (fun db ->
      ignore
        (Room_github_backlinks.insert ~db ~repo:"owner/repo" ~pr_number:42
           ~commit_sha:"abc1234"
           ~github_item_type:Room_github_backlinks.Pr_comment
           ~github_item_id:"comment-1"
           ~github_url:"https://github.com/owner/repo/pull/42" ~room_id:"room-1"
           ~thread_id:"thread-1" ~room_item_type:Room_github_backlinks.Message
           ~room_item_id:"msg-1" ~direction:Room_github_backlinks.Github_to_room
           ~relationship:Room_github_backlinks.Subscription_delivery
           ~snapshot_id:"snap-1" ());
      let found = Room_github_backlinks.find_by_room ~db ~room_id:"room-1" () in
      let bl = List.hd found in
      let json = Room_github_backlinks.backlink_to_json bl in
      let json_str = Yojson.Safe.to_string json in
      Alcotest.(check bool)
        "json has repo" true
        (String.contains json_str 'o'
        && String.contains json_str 'w'
        && String.contains json_str 'n');
      (* Verify round-trip of enum types *)
      Alcotest.(check string)
        "github_item_type roundtrip" "pr_comment"
        (Room_github_backlinks.github_item_type_of_string
           (Room_github_backlinks.github_item_type_to_string
              Room_github_backlinks.Pr_comment)
        |> Room_github_backlinks.github_item_type_to_string);
      Alcotest.(check string)
        "room_item_type roundtrip" "background_task"
        (Room_github_backlinks.room_item_type_of_string
           (Room_github_backlinks.room_item_type_to_string
              Room_github_backlinks.Background_task)
        |> Room_github_backlinks.room_item_type_to_string);
      Alcotest.(check string)
        "direction roundtrip" "room_to_github"
        (Room_github_backlinks.direction_of_string
           (Room_github_backlinks.direction_to_string
              Room_github_backlinks.Room_to_github)
        |> Room_github_backlinks.direction_to_string);
      Alcotest.(check string)
        "relationship roundtrip" "triggered_run"
        (Room_github_backlinks.relationship_of_string
           (Room_github_backlinks.relationship_to_string
              Room_github_backlinks.Triggered_run)
        |> Room_github_backlinks.relationship_to_string))

let test_convenience_subscription_delivery () =
  with_db (fun db ->
      Room_github_backlinks.record_subscription_delivery ~db ~repo:"owner/repo"
        ~pr_number:10 ~room_id:"room-1" ~event_type:"pull_request"
        ~github_url:"https://github.com/owner/repo/pull/10" ();
      let found =
        Room_github_backlinks.find_by_github ~db ~repo:"owner/repo"
          ~github_item_type:Room_github_backlinks.Pr_comment ()
      in
      Alcotest.(check int) "one delivery" 1 (List.length found);
      let bl = List.hd found in
      Alcotest.(check string)
        "relationship" "subscription_delivery"
        (Room_github_backlinks.relationship_to_string bl.relationship);
      Alcotest.(check string)
        "direction" "github_to_room"
        (Room_github_backlinks.direction_to_string bl.direction))

let test_convenience_ci_notification () =
  with_db (fun db ->
      Room_github_backlinks.record_ci_notification ~db ~repo:"owner/repo"
        ~pr_number:5 ~github_item_type:Room_github_backlinks.Check_run
        ~room_id:"room-1" ();
      let found = Room_github_backlinks.find_by_room ~db ~room_id:"room-1" () in
      Alcotest.(check int) "one ci notification" 1 (List.length found);
      let bl = List.hd found in
      Alcotest.(check string)
        "relationship" "ci_notification"
        (Room_github_backlinks.relationship_to_string bl.relationship))

let test_convenience_triggered_run () =
  with_db (fun db ->
      Room_github_backlinks.record_triggered_run ~db ~repo:"owner/repo"
        ~pr_number:42 ~github_item_type:Room_github_backlinks.Pr_comment
        ~room_id:"room-1" ~room_item_type:Room_github_backlinks.Review_run
        ~room_item_id:"run-1" ();
      let found = Room_github_backlinks.find_by_room ~db ~room_id:"room-1" () in
      Alcotest.(check int) "one triggered run" 1 (List.length found);
      let bl = List.hd found in
      Alcotest.(check string)
        "relationship" "triggered_run"
        (Room_github_backlinks.relationship_to_string bl.relationship);
      Alcotest.(check string)
        "direction" "room_to_github"
        (Room_github_backlinks.direction_to_string bl.direction);
      Alcotest.(check (option string))
        "room_item_id" (Some "run-1") bl.room_item_id)

let suite =
  [
    Alcotest.test_case "insert and find by github" `Quick
      test_insert_and_find_by_github;
    Alcotest.test_case "insert is idempotent" `Quick test_insert_is_idempotent;
    Alcotest.test_case "find by room" `Quick test_find_by_room;
    Alcotest.test_case "find by repo pr" `Quick test_find_by_repo_pr;
    Alcotest.test_case "count by room" `Quick test_count_by_room;
    Alcotest.test_case "delete before" `Quick test_delete_before;
    Alcotest.test_case "json serialization" `Quick test_json_serialization;
    Alcotest.test_case "convenience subscription delivery" `Quick
      test_convenience_subscription_delivery;
    Alcotest.test_case "convenience ci notification" `Quick
      test_convenience_ci_notification;
    Alcotest.test_case "convenience triggered run" `Quick
      test_convenience_triggered_run;
  ]
