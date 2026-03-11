let fresh_db () =
  let db = Sqlite3.db_open ":memory:" in
  Task_tree.init_schema db;
  db

let test_init_schema_idempotent () =
  let db = fresh_db () in
  Task_tree.init_schema db;
  Task_tree.init_schema db;
  let count = Task_tree.count_tasks ~db ~session_key:"s1" in
  Alcotest.(check int) "empty after init" 0 count

let test_add_root_task () =
  let db = fresh_db () in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "add"); ("title", `String "Root task") ] ]
  in
  (match result with
  | Ok output ->
      Alcotest.(check bool)
        "contains Added" true
        (try
           ignore (Str.search_forward (Str.regexp_string "Added") output 0);
           true
         with Not_found -> false)
  | Error e -> Alcotest.fail ("Expected Ok, got Error: " ^ e));
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  Alcotest.(check int) "one task" 1 (List.length tasks);
  Alcotest.(check string) "id is 1" "1" (List.hd tasks).id;
  Alcotest.(check string) "title" "Root task" (List.hd tasks).title

let test_add_custom_string_id () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add");
            ("id", `String "auth");
            ("title", `String "Implement auth");
          ];
      ]
  in
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  Alcotest.(check int) "one task" 1 (List.length tasks);
  Alcotest.(check string) "custom id" "auth" (List.hd tasks).id

let test_add_depth_auto_nesting () =
  let db = fresh_db () in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Parent"); ("depth", `Int 0);
          ];
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Child"); ("depth", `Int 1);
          ];
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Grandchild");
            ("depth", `Int 2);
          ];
      ]
  in
  (match result with Ok _ -> () | Error e -> Alcotest.fail e);
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  Alcotest.(check int) "three tasks" 3 (List.length tasks);
  let child = List.find (fun t -> t.Task_tree.title = "Child") tasks in
  Alcotest.(check (option string)) "child parent" (Some "1") child.parent_id;
  let gc = List.find (fun t -> t.Task_tree.title = "Grandchild") tasks in
  Alcotest.(check (option string)) "grandchild parent" (Some "2") gc.parent_id

let test_add_explicit_parent () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add");
            ("id", `String "root");
            ("title", `String "Root");
          ];
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Child");
            ("parent", `String "root");
          ];
      ]
  in
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  let child = List.find (fun t -> t.Task_tree.title = "Child") tasks in
  Alcotest.(check (option string))
    "explicit parent" (Some "root") child.parent_id

let test_depth_jump_validation () =
  let db = fresh_db () in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Root"); ("depth", `Int 0);
          ];
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Bad jump");
            ("depth", `Int 3);
          ];
      ]
  in
  match result with
  | Error msg ->
      Alcotest.(check bool)
        "mentions skips" true
        (try
           ignore (Str.search_forward (Str.regexp_string "skips") msg 0);
           true
         with Not_found -> false)
  | Ok _ -> Alcotest.fail "Expected error for depth jump"

let test_update_status_lifecycle () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "add"); ("title", `String "Task") ] ]
  in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "1");
            ("status", `String "in_progress");
          ];
      ]
  in
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  Alcotest.(check string)
    "in_progress" "in_progress"
    (Task_tree.string_of_status (List.hd tasks).status);
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "1");
            ("status", `String "done");
          ];
      ]
  in
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  Alcotest.(check string)
    "done" "done"
    (Task_tree.string_of_status (List.hd tasks).status)

let test_update_done_with_incomplete_children () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Parent"); ("depth", `Int 0);
          ];
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Child"); ("depth", `Int 1);
          ];
      ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "1");
            ("status", `String "done");
          ];
      ]
  in
  match result with
  | Error msg ->
      Alcotest.(check bool)
        "mentions incomplete" true
        (try
           ignore (Str.search_forward (Str.regexp_string "incomplete") msg 0);
           true
         with Not_found -> false)
  | Ok _ -> Alcotest.fail "Expected error for incomplete children"

let test_update_done_after_completing_children () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Parent"); ("depth", `Int 0);
          ];
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Child"); ("depth", `Int 1);
          ];
      ]
  in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "2");
            ("status", `String "done");
          ];
      ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "1");
            ("status", `String "done");
          ];
      ]
  in
  match result with
  | Ok _ -> ()
  | Error e -> Alcotest.fail ("Expected Ok, got Error: " ^ e)

let test_update_done_with_cancelled_child () =
  (* B292: Parent should be markable done when all children are done OR cancelled *)
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Parent"); ("depth", `Int 0);
          ];
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Cancelled child");
            ("depth", `Int 1);
          ];
      ]
  in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "2");
            ("status", `String "cancelled");
          ];
      ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "1");
            ("status", `String "done");
          ];
      ]
  in
  match result with
  | Ok _ -> ()
  | Error e ->
      Alcotest.fail ("Expected Ok with cancelled child, got Error: " ^ e)

let test_update_done_with_mixed_children () =
  (* B292: Parent with both done and cancelled children should be markable done *)
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Parent"); ("depth", `Int 0);
          ];
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Done child");
            ("depth", `Int 1);
          ];
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Cancelled child");
            ("depth", `Int 1);
          ];
      ]
  in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "2");
            ("status", `String "done");
          ];
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "3");
            ("status", `String "cancelled");
          ];
      ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "1");
            ("status", `String "done");
          ];
      ]
  in
  match result with
  | Ok _ -> ()
  | Error e -> Alcotest.fail ("Expected Ok with mixed children, got Error: " ^ e)

let test_in_progress_propagation () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Root"); ("depth", `Int 0);
          ];
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Child"); ("depth", `Int 1);
          ];
      ]
  in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "2");
            ("status", `String "in_progress");
          ];
      ]
  in
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  let root = List.find (fun t -> t.Task_tree.id = "1") tasks in
  Alcotest.(check string)
    "root promoted to in_progress" "in_progress"
    (Task_tree.string_of_status root.status)

let test_remove_task_and_subtree () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Root"); ("depth", `Int 0);
          ];
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Child"); ("depth", `Int 1);
          ];
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Other"); ("depth", `Int 0);
          ];
      ]
  in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "remove"); ("id", `String "1") ] ]
  in
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  Alcotest.(check int) "only Other remains" 1 (List.length tasks);
  Alcotest.(check string) "remaining is Other" "Other" (List.hd tasks).title

let test_remove_blocked_by_in_progress () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Root");
            ("status", `String "in_progress");
          ];
      ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "remove"); ("id", `String "1") ] ]
  in
  match result with
  | Error msg ->
      Alcotest.(check bool)
        "mentions in_progress" true
        (try
           ignore (Str.search_forward (Str.regexp_string "in_progress") msg 0);
           true
         with Not_found -> false)
  | Ok _ -> Alcotest.fail "Expected error for in_progress removal"

let test_clear_only_done_cancelled () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Done");
            ("status", `String "done");
          ];
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Cancelled");
            ("status", `String "cancelled");
          ];
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Pending");
            ("status", `String "pending");
          ];
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Error");
            ("status", `String "error");
          ];
      ]
  in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "clear") ] ]
  in
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  Alcotest.(check int) "pending + error remain" 2 (List.length tasks);
  let titles = List.map (fun t -> t.Task_tree.title) tasks in
  Alcotest.(check bool) "has Pending" true (List.mem "Pending" titles);
  Alcotest.(check bool) "has Error" true (List.mem "Error" titles)

let test_archive_completed_subtree () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Done root");
            ("status", `String "done");
          ];
        `Assoc [ ("op", `String "add"); ("title", `String "Still pending") ];
      ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "archive"); ("id", `String "1") ] ]
  in
  (match result with Ok _ -> () | Error e -> Alcotest.fail e);
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  Alcotest.(check int) "only pending remains" 1 (List.length tasks);
  Alcotest.(check string)
    "remaining is pending" "Still pending" (List.hd tasks).title;
  let archived =
    Memory.query_single_int db
      "SELECT COUNT(*) FROM task_tree_archive WHERE session_key = 's1'"
  in
  Alcotest.(check int) "one archived" 1 archived

let test_archive_resets_auto_id () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Task");
            ("status", `String "done");
          ];
      ]
  in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "archive") ] ]
  in
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  Alcotest.(check int) "empty after archive" 0 (List.length tasks);
  (* Soft-delete preserves the row; next_auto_id still sees the max id=1,
     so the next auto-id is 2 (avoids collision on potential restore). *)
  let next_id = Task_tree.next_auto_id ~db ~session_key:"s1" in
  Alcotest.(check string) "next id after soft-delete" "2" next_id

let test_archive_blocked_by_incomplete () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Root"); ("depth", `Int 0);
          ];
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Child"); ("depth", `Int 1);
          ];
      ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "archive"); ("id", `String "1") ] ]
  in
  match result with
  | Error msg ->
      Alcotest.(check bool)
        "mentions non-terminal" true
        (try
           ignore (Str.search_forward (Str.regexp_string "non-terminal") msg 0);
           true
         with Not_found -> false)
  | Ok _ -> Alcotest.fail "Expected error for incomplete archive"

let test_session_isolation () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "add"); ("title", `String "Session 1") ] ]
  in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s2"
      [ `Assoc [ ("op", `String "add"); ("title", `String "Session 2") ] ]
  in
  let s1_tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  let s2_tasks = Task_tree.load_tasks ~db ~session_key:"s2" () in
  Alcotest.(check int) "s1 has one" 1 (List.length s1_tasks);
  Alcotest.(check int) "s2 has one" 1 (List.length s2_tasks);
  Alcotest.(check string) "s1 title" "Session 1" (List.hd s1_tasks).title;
  Alcotest.(check string) "s2 title" "Session 2" (List.hd s2_tasks).title

let test_render_empty_tree () =
  let db = fresh_db () in
  let output = Task_tree.render_tree ~db ~session_key:"s1" in
  Alcotest.(check bool)
    "contains encouragement" true
    (try
       ignore
         (Str.search_forward (Str.regexp_string "No tasks tracked") output 0);
       true
     with Not_found -> false)

let test_render_populated_tree () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "First");
            ("status", `String "in_progress");
            ("depth", `Int 0);
          ];
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Sub");
            ("status", `String "done");
            ("depth", `Int 1);
          ];
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Second"); ("depth", `Int 0);
          ];
      ]
  in
  let output = Task_tree.render_tree ~db ~session_key:"s1" in
  Alcotest.(check bool)
    "contains [>]" true
    (try
       ignore (Str.search_forward (Str.regexp_string "[>]") output 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains [x]" true
    (try
       ignore (Str.search_forward (Str.regexp_string "[x]") output 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains hierarchy" true
    (try
       ignore (Str.search_forward (Str.regexp_string "1.1") output 0);
       true
     with Not_found -> false)

let test_batch_operations () =
  let db = fresh_db () in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc [ ("op", `String "add"); ("title", `String "A") ];
        `Assoc [ ("op", `String "add"); ("title", `String "B") ];
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "1");
            ("status", `String "done");
          ];
      ]
  in
  (match result with Ok _ -> () | Error e -> Alcotest.fail e);
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  Alcotest.(check int) "two tasks" 2 (List.length tasks);
  let t1 = List.find (fun t -> t.Task_tree.id = "1") tasks in
  Alcotest.(check string)
    "t1 done" "done"
    (Task_tree.string_of_status t1.status)

let test_tool_invoke_round_trip () =
  let db = fresh_db () in
  let tool_t = Task_tree.tool ~db () in
  let args =
    `Assoc
      [
        ( "operations",
          `List
            [ `Assoc [ ("op", `String "add"); ("title", `String "Test task") ] ]
        );
      ]
  in
  let ctx =
    {
      Tool.session_key = Some "s1";
      send_progress = None;
      interrupt_check = None;
    }
  in
  let result = Lwt_main.run (tool_t.invoke ~context:ctx args) in
  Alcotest.(check bool)
    "contains Added" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Added") result 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains compact summary" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Tasks: 1 total") result 0);
       true
     with Not_found -> false)

let test_id_collision_rejected () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add");
            ("id", `String "myid");
            ("title", `String "First");
          ];
      ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add");
            ("id", `String "myid");
            ("title", `String "Duplicate");
          ];
      ]
  in
  match result with
  | Error msg ->
      Alcotest.(check bool)
        "mentions already exists" true
        (try
           ignore
             (Str.search_forward (Str.regexp_string "already exists") msg 0);
           true
         with Not_found -> false)
  | Ok _ -> Alcotest.fail "Expected error for duplicate ID"

let test_auto_id_skips_agent_chosen () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc [ ("op", `String "add"); ("title", `String "Auto 1") ];
        `Assoc [ ("op", `String "add"); ("title", `String "Auto 2") ];
      ]
  in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add");
            ("id", `String "3");
            ("title", `String "Manual 3");
          ];
      ]
  in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "add"); ("title", `String "Auto next") ] ]
  in
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  let last = List.find (fun t -> t.Task_tree.title = "Auto next") tasks in
  Alcotest.(check string) "auto-id skipped to 4" "4" last.id

let test_reopen_task () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Task");
            ("status", `String "done");
          ];
      ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "1");
            ("status", `String "pending");
          ];
      ]
  in
  (match result with Ok _ -> () | Error e -> Alcotest.fail e);
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  Alcotest.(check string)
    "reopened to pending" "pending"
    (Task_tree.string_of_status (List.hd tasks).status)

let test_in_progress_adds_can_exceed_old_live_cap () =
  let db = fresh_db () in
  for i = 1 to 50 do
    match
      Task_tree.process_operations ~db ~session_key:"s1"
        [
          `Assoc
            [
              ("op", `String "add");
              ("title", `String (Printf.sprintf "Task %d" i));
              ("status", `String "in_progress");
            ];
        ]
    with
    | Ok _ -> ()
    | Error e -> Alcotest.failf "Setup failed at task %d: %s" i e
  done;
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Task 51");
            ("status", `String "in_progress");
          ];
      ]
  in
  let output =
    match result with
    | Ok output -> output
    | Error e -> Alcotest.fail ("Expected success past old live cap: " ^ e)
  in
  Alcotest.(check bool)
    "warns once threshold exceeded" true
    (try
       ignore (Str.search_forward (Str.regexp_string "WARNING") output 0);
       true
     with Not_found -> false);
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  Alcotest.(check int)
    "51 live in_progress tasks allowed" 51 (List.length tasks)

let test_max_depth_guardrail () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [ ("op", `String "add"); ("title", `String "L0"); ("depth", `Int 0) ];
        `Assoc
          [ ("op", `String "add"); ("title", `String "L1"); ("depth", `Int 1) ];
        `Assoc
          [ ("op", `String "add"); ("title", `String "L2"); ("depth", `Int 2) ];
        `Assoc
          [ ("op", `String "add"); ("title", `String "L3"); ("depth", `Int 3) ];
        `Assoc
          [ ("op", `String "add"); ("title", `String "L4"); ("depth", `Int 4) ];
      ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "L5");
            ("parent", `String "5");
          ];
      ]
  in
  match result with
  | Error msg ->
      Alcotest.(check bool)
        "mentions max" true
        (try
           ignore (Str.search_forward (Str.regexp_string "Max nesting") msg 0);
           true
         with Not_found -> false)
  | Ok _ -> Alcotest.fail "Expected error for exceeding max depth"

let test_max_concurrent_in_progress () =
  let db = fresh_db () in
  (* Add 4 in_progress tasks — should succeed without error *)
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "T1");
            ("status", `String "in_progress");
          ];
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "T2");
            ("status", `String "in_progress");
          ];
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "T3");
            ("status", `String "in_progress");
          ];
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "T4");
            ("status", `String "in_progress");
          ];
      ]
  in
  (match result with
  | Ok output ->
      Alcotest.(check bool)
        "no warning below threshold" true
        (not
           (try
              ignore (Str.search_forward (Str.regexp_string "WARNING") output 0);
              true
            with Not_found -> false))
  | Error e -> Alcotest.fail ("Expected success for 4 in_progress: " ^ e));
  (* Add a 5th — should succeed with a warning *)
  let result2 =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "T5");
            ("status", `String "in_progress");
          ];
      ]
  in
  match result2 with
  | Ok output ->
      Alcotest.(check bool)
        "warns at threshold" true
        (try
           ignore (Str.search_forward (Str.regexp_string "WARNING") output 0);
           true
         with Not_found -> false)
  | Error e -> Alcotest.fail ("Expected success with warning for 5th: " ^ e)

let test_batch_transaction_rollback () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Existing");
            ("depth", `Int 0);
          ];
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Child"); ("depth", `Int 1);
          ];
      ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc [ ("op", `String "add"); ("title", `String "New") ];
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "1");
            ("status", `String "done");
          ];
      ]
  in
  (* op 2 should fail because child #2 is still pending *)
  (match result with
  | Error msg ->
      Alcotest.(check bool)
        "mentions batch failed" true
        (try
           ignore (Str.search_forward (Str.regexp_string "Batch failed") msg 0);
           true
         with Not_found -> false)
  | Ok _ -> Alcotest.fail "Expected batch error");
  (* Verify rollback: New task should NOT exist *)
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  Alcotest.(check int) "still 2 tasks (rollback)" 2 (List.length tasks)

let test_render_with_legend () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "add"); ("title", `String "Task") ] ]
  in
  let output = Task_tree.render_tree_with_legend ~db ~session_key:"s1" in
  Alcotest.(check bool)
    "contains Legend" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Legend") output 0);
       true
     with Not_found -> false)

let test_note_only_update () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "add"); ("title", `String "Task") ] ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "1");
            ("note", `String "my note");
          ];
      ]
  in
  (match result with Ok _ -> () | Error e -> Alcotest.fail e);
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  let t = List.hd tasks in
  Alcotest.(check (option string)) "note persisted" (Some "my note") t.note;
  Alcotest.(check string)
    "status unchanged" "pending"
    (Task_tree.string_of_status t.status)

let test_comma_separated_id_update () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc [ ("op", `String "add"); ("title", `String "A") ];
        `Assoc [ ("op", `String "add"); ("title", `String "B") ];
        `Assoc [ ("op", `String "add"); ("title", `String "C") ];
      ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "1,2,3");
            ("status", `String "done");
          ];
      ]
  in
  (match result with Ok _ -> () | Error e -> Alcotest.fail e);
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  List.iter
    (fun t ->
      Alcotest.(check string)
        (Printf.sprintf "#%s is done" t.Task_tree.id)
        "done"
        (Task_tree.string_of_status t.status))
    tasks

let test_archive_all_completed_roots () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Root A");
            ("status", `String "done");
          ];
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Root B");
            ("status", `String "done");
          ];
      ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "archive") ] ]
  in
  (match result with Ok _ -> () | Error e -> Alcotest.fail e);
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  Alcotest.(check int) "no active tasks" 0 (List.length tasks);
  let archived =
    Memory.query_single_int db
      "SELECT COUNT(*) FROM task_tree_archive WHERE session_key = 's1'"
  in
  Alcotest.(check int) "both archived" 2 archived

let test_reorder_move_to_first () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc [ ("op", `String "add"); ("title", `String "A") ];
        `Assoc [ ("op", `String "add"); ("title", `String "B") ];
        `Assoc [ ("op", `String "add"); ("title", `String "C") ];
      ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "reorder");
            ("id", `String "3");
            ("position", `String "first");
          ];
      ]
  in
  (match result with Ok _ -> () | Error e -> Alcotest.fail e);
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  let titles = List.map (fun t -> t.Task_tree.title) tasks in
  Alcotest.(check (list string)) "C is first" [ "C"; "A"; "B" ] titles

let test_reorder_move_to_last () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc [ ("op", `String "add"); ("title", `String "A") ];
        `Assoc [ ("op", `String "add"); ("title", `String "B") ];
        `Assoc [ ("op", `String "add"); ("title", `String "C") ];
      ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "reorder");
            ("id", `String "1");
            ("position", `String "last");
          ];
      ]
  in
  (match result with Ok _ -> () | Error e -> Alcotest.fail e);
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  let titles = List.map (fun t -> t.Task_tree.title) tasks in
  Alcotest.(check (list string)) "A is last" [ "B"; "C"; "A" ] titles

let test_reorder_after_sibling () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc [ ("op", `String "add"); ("title", `String "A") ];
        `Assoc [ ("op", `String "add"); ("title", `String "B") ];
        `Assoc [ ("op", `String "add"); ("title", `String "C") ];
      ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "reorder");
            ("id", `String "1");
            ("position", `String "after:2");
          ];
      ]
  in
  (match result with Ok _ -> () | Error e -> Alcotest.fail e);
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  let titles = List.map (fun t -> t.Task_tree.title) tasks in
  Alcotest.(check (list string)) "A after B" [ "B"; "A"; "C" ] titles

let test_reorder_before_sibling () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc [ ("op", `String "add"); ("title", `String "A") ];
        `Assoc [ ("op", `String "add"); ("title", `String "B") ];
        `Assoc [ ("op", `String "add"); ("title", `String "C") ];
      ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "reorder");
            ("id", `String "3");
            ("position", `String "before:1");
          ];
      ]
  in
  (match result with Ok _ -> () | Error e -> Alcotest.fail e);
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  let titles = List.map (fun t -> t.Task_tree.title) tasks in
  Alcotest.(check (list string)) "C before A" [ "C"; "A"; "B" ] titles

let test_reorder_not_found () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "add"); ("title", `String "A") ] ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "reorder");
            ("id", `String "99");
            ("position", `String "first");
          ];
      ]
  in
  match result with
  | Error msg ->
      Alcotest.(check bool)
        "mentions not found" true
        (try
           ignore (Str.search_forward (Str.regexp_string "not found") msg 0);
           true
         with Not_found -> false)
  | Ok _ -> Alcotest.fail "Expected error for missing task"

let test_reorder_no_siblings () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "add"); ("title", `String "Only") ] ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "reorder");
            ("id", `String "1");
            ("position", `String "first");
          ];
      ]
  in
  match result with
  | Error msg ->
      Alcotest.(check bool)
        "mentions no siblings" true
        (try
           ignore (Str.search_forward (Str.regexp_string "No siblings") msg 0);
           true
         with Not_found -> false)
  | Ok _ -> Alcotest.fail "Expected error for no siblings"

let test_reorder_non_sibling_target () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Root1"); ("depth", `Int 0);
          ];
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Child"); ("depth", `Int 1);
          ];
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Root2"); ("depth", `Int 0);
          ];
      ]
  in
  (* Try to reorder Root1 after Child (which is a child, not a sibling) *)
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "reorder");
            ("id", `String "1");
            ("position", `String "after:2");
          ];
      ]
  in
  match result with
  | Error msg ->
      Alcotest.(check bool)
        "mentions not found among siblings" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "not found among siblings")
                msg 0);
           true
         with Not_found -> false)
  | Ok _ -> Alcotest.fail "Expected error for non-sibling reference"

let test_reorder_invalid_position () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc [ ("op", `String "add"); ("title", `String "A") ];
        `Assoc [ ("op", `String "add"); ("title", `String "B") ];
      ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "reorder");
            ("id", `String "1");
            ("position", `String "middle");
          ];
      ]
  in
  match result with
  | Error msg ->
      Alcotest.(check bool)
        "mentions invalid position" true
        (try
           ignore
             (Str.search_forward (Str.regexp_string "Invalid position") msg 0);
           true
         with Not_found -> false)
  | Ok _ -> Alcotest.fail "Expected error for invalid position"

let test_reorder_preserves_children () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Root1"); ("depth", `Int 0);
          ];
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Child of Root1");
            ("depth", `Int 1);
          ];
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Root2"); ("depth", `Int 0);
          ];
      ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "reorder");
            ("id", `String "1");
            ("position", `String "last");
          ];
      ]
  in
  (match result with Ok _ -> () | Error e -> Alcotest.fail e);
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  let child = List.find (fun t -> t.Task_tree.title = "Child of Root1") tasks in
  Alcotest.(check (option string))
    "child still parented to Root1" (Some "1") child.parent_id;
  (* Verify render order: Root2 first, then Root1 with its child *)
  let tree = Task_tree.render_tree ~db ~session_key:"s1" in
  let root2_pos =
    try Str.search_forward (Str.regexp_string "Root2") tree 0
    with Not_found -> max_int
  in
  let root1_pos =
    try Str.search_forward (Str.regexp_string "Root1") tree 0
    with Not_found -> max_int
  in
  Alcotest.(check bool) "Root2 before Root1" true (root2_pos < root1_pos)

let test_add_empty_parent_normalized_to_root () =
  let db = fresh_db () in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Root via empty parent");
            ("parent", `String "");
          ];
      ]
  in
  (match result with Ok _ -> () | Error e -> Alcotest.fail e);
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  Alcotest.(check int) "one task" 1 (List.length tasks);
  Alcotest.(check (option string))
    "root (no parent)" None (List.hd tasks).parent_id

let test_add_whitespace_parent_normalized () =
  let db = fresh_db () in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Root via whitespace parent");
            ("parent", `String "  ");
          ];
      ]
  in
  (match result with Ok _ -> () | Error e -> Alcotest.fail e);
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  Alcotest.(check int) "one task" 1 (List.length tasks);
  Alcotest.(check (option string))
    "root (no parent)" None (List.hd tasks).parent_id

let test_add_depth_overrides_invalid_parent () =
  let db = fresh_db () in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Root despite bad parent");
            ("parent", `String "nonexistent");
            ("depth", `Int 0);
          ];
      ]
  in
  (match result with Ok _ -> () | Error e -> Alcotest.fail e);
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  Alcotest.(check int) "one task" 1 (List.length tasks);
  Alcotest.(check (option string))
    "root (depth 0 wins)" None (List.hd tasks).parent_id

let test_add_batch_b237_scenario () =
  let db = fresh_db () in
  (* Reproduces the exact B237 bug scenario: depth carries intent, parent is noise *)
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add");
            ("id", `String "plan");
            ("title", `String "Plan");
            ("parent", `String "");
            ("depth", `Int 0);
          ];
        `Assoc
          [
            ("op", `String "add");
            ("id", `String "step1");
            ("title", `String "Step 1");
            ("parent", `String "missing");
            ("depth", `Int 1);
          ];
        `Assoc
          [
            ("op", `String "add");
            ("id", `String "step1a");
            ("title", `String "Step 1a");
            ("parent", `String "");
            ("depth", `Int 2);
          ];
        `Assoc
          [
            ("op", `String "add");
            ("id", `String "step2");
            ("title", `String "Step 2");
            ("parent", `String "missing");
            ("depth", `Int 1);
          ];
        `Assoc
          [
            ("op", `String "add");
            ("id", `String "step3");
            ("title", `String "Step 3");
            ("depth", `Int 1);
          ];
      ]
  in
  (match result with Ok _ -> () | Error e -> Alcotest.fail e);
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  Alcotest.(check int) "five tasks" 5 (List.length tasks);
  let plan = List.find (fun t -> t.Task_tree.id = "plan") tasks in
  let step1 = List.find (fun t -> t.Task_tree.id = "step1") tasks in
  let step1a = List.find (fun t -> t.Task_tree.id = "step1a") tasks in
  let step2 = List.find (fun t -> t.Task_tree.id = "step2") tasks in
  let step3 = List.find (fun t -> t.Task_tree.id = "step3") tasks in
  Alcotest.(check (option string)) "plan is root" None plan.parent_id;
  Alcotest.(check (option string))
    "step1 under plan" (Some "plan") step1.parent_id;
  Alcotest.(check (option string))
    "step1a under step1" (Some "step1") step1a.parent_id;
  Alcotest.(check (option string))
    "step2 under plan" (Some "plan") step2.parent_id;
  Alcotest.(check (option string))
    "step3 under plan" (Some "plan") step3.parent_id

let test_add_depth_and_valid_parent_uses_depth () =
  let db = fresh_db () in
  (* First create a root task *)
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add");
            ("id", `String "root");
            ("title", `String "Root");
          ];
      ]
  in
  (* Now add with both parent (valid) and depth 0 — depth should win *)
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "New root despite valid parent");
            ("parent", `String "root");
            ("depth", `Int 0);
          ];
      ]
  in
  (match result with Ok _ -> () | Error e -> Alcotest.fail e);
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  let new_task = List.find (fun t -> t.Task_tree.id <> "root") tasks in
  Alcotest.(check (option string))
    "depth 0 wins over valid parent" None new_task.parent_id

let test_error_msg_parent_not_found_has_suggestion () =
  let db = fresh_db () in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Orphan");
            ("parent", `String "nope");
          ];
      ]
  in
  match result with
  | Ok _ -> Alcotest.fail "Expected error for missing parent"
  | Error e ->
      Alcotest.(check bool)
        "error mentions suggestion" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "Omit both for a root task")
                e 0);
           true
         with Not_found -> false)

let test_error_msg_unknown_op_lists_valid () =
  let db = fresh_db () in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "foo") ] ]
  in
  match result with
  | Ok _ -> Alcotest.fail "Expected error for unknown op"
  | Error e ->
      Alcotest.(check bool)
        "error lists valid operations" true
        (try
           ignore
             (Str.search_forward (Str.regexp_string "Valid operations:") e 0);
           true
         with Not_found -> false)

let contains s sub =
  try
    ignore (Str.search_forward (Str.regexp_string sub) s 0);
    true
  with Not_found -> false

let test_format_notification_add () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "add"); ("title", `String "Setup DB") ] ]
  in
  let ops =
    [ `Assoc [ ("op", `String "add"); ("title", `String "Setup DB") ] ]
  in
  let result =
    Task_tree.format_notification ~connector:Format_adapter.Plain ~db
      ~session_key:"s1" ops
  in
  match result with
  | None -> Alcotest.fail "Expected Some notification"
  | Some text ->
      Alcotest.(check bool) "contains title" true (contains text "Setup DB");
      Alcotest.(check bool) "contains pending" true (contains text "[pending]");
      Alcotest.(check bool)
        "contains header" true
        (contains text "Task tree updated")

let test_format_notification_update () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "add"); ("title", `String "Work item") ] ]
  in
  let ops =
    [
      `Assoc
        [
          ("op", `String "update");
          ("id", `String "1");
          ("status", `String "done");
        ];
    ]
  in
  let result =
    Task_tree.format_notification ~connector:Format_adapter.Discord ~db
      ~session_key:"s1" ops
  in
  match result with
  | None -> Alcotest.fail "Expected Some notification"
  | Some text ->
      Alcotest.(check bool) "contains #1" true (contains text "`#1`");
      Alcotest.(check bool) "contains done" true (contains text "`done`")

let test_format_notification_focus () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc [ ("op", `String "add"); ("title", `String "Single task") ];
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "1");
            ("status", `String "in_progress");
          ];
      ]
  in
  let ops =
    [
      `Assoc
        [
          ("op", `String "update");
          ("id", `String "1");
          ("status", `String "in_progress");
        ];
    ]
  in
  let result =
    Task_tree.format_notification ~connector:Format_adapter.Plain ~db
      ~session_key:"s1" ops
  in
  match result with
  | None -> Alcotest.fail "Expected Some notification"
  | Some text ->
      Alcotest.(check bool) "contains Focus" true (contains text "Focus:");
      Alcotest.(check bool)
        "contains task title" true
        (contains text "Single task")

let test_format_notification_multiple_active () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add");
            ("id", `String "a");
            ("title", `String "Task A");
            ("status", `String "in_progress");
          ];
        `Assoc
          [
            ("op", `String "add");
            ("id", `String "b");
            ("title", `String "Task B");
            ("status", `String "in_progress");
          ];
      ]
  in
  let ops =
    [
      `Assoc
        [
          ("op", `String "update");
          ("id", `String "a");
          ("status", `String "in_progress");
        ];
    ]
  in
  let result =
    Task_tree.format_notification ~connector:Format_adapter.Plain ~db
      ~session_key:"s1" ops
  in
  match result with
  | None -> Alcotest.fail "Expected Some notification"
  | Some text ->
      Alcotest.(check bool) "contains Focus" true (contains text "Focus:");
      Alcotest.(check bool) "contains #a" true (contains text "#a");
      Alcotest.(check bool) "contains #b" true (contains text "#b")

let test_format_notification_reorder_only () =
  let db = fresh_db () in
  let ops = [ `Assoc [ ("op", `String "reorder") ] ] in
  let result =
    Task_tree.format_notification ~connector:Format_adapter.Plain ~db
      ~session_key:"s1" ops
  in
  Alcotest.(check bool) "reorder-only returns None" true (result = None)

let test_format_notification_discord_formatting () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "add"); ("title", `String "My task") ] ]
  in
  let ops =
    [ `Assoc [ ("op", `String "add"); ("title", `String "My task") ] ]
  in
  let result =
    Task_tree.format_notification ~connector:Format_adapter.Discord ~db
      ~session_key:"s1" ops
  in
  match result with
  | None -> Alcotest.fail "Expected Some notification"
  | Some text ->
      Alcotest.(check bool)
        "Discord bold header" true
        (contains text "**Task tree updated**");
      Alcotest.(check bool)
        "Discord bold title" true
        (contains text "**My task**")

let test_tool_notify_called_on_success () =
  let db = fresh_db () in
  let notified = ref None in
  let notify session_key =
    let send text =
      notified := Some (session_key, text);
      Lwt.return_unit
    in
    Some (Format_adapter.Plain, send)
  in
  let tool_t = Task_tree.tool ~db ~notify () in
  let args =
    `Assoc
      [
        ( "operations",
          `List
            [ `Assoc [ ("op", `String "add"); ("title", `String "Notify me") ] ]
        );
      ]
  in
  let ctx =
    {
      Tool.session_key = Some "test:1";
      send_progress = None;
      interrupt_check = None;
    }
  in
  let result = Lwt_main.run (tool_t.invoke ~context:ctx args) in
  Alcotest.(check bool) "tool succeeded" true (contains result "Added");
  match !notified with
  | None -> Alcotest.fail "Expected notification to be sent"
  | Some (sk, text) ->
      Alcotest.(check string) "session key" "test:1" sk;
      Alcotest.(check bool)
        "notification contains title" true
        (contains text "Notify me")

let test_tool_notify_not_called_on_error () =
  let db = fresh_db () in
  let notified = ref false in
  let notify _session_key =
    let send _text =
      notified := true;
      Lwt.return_unit
    in
    Some (Format_adapter.Plain, send)
  in
  let tool_t = Task_tree.tool ~db ~notify () in
  let args =
    `Assoc
      [
        ( "operations",
          `List [ `Assoc [ ("op", `String "update"); ("id", `String "nope") ] ]
        );
      ]
  in
  let ctx =
    {
      Tool.session_key = Some "test:1";
      send_progress = None;
      interrupt_check = None;
    }
  in
  let result = Lwt_main.run (tool_t.invoke ~context:ctx args) in
  Alcotest.(check bool) "tool errored" true (contains result "Error");
  Alcotest.(check bool) "notify not called" false !notified

let test_tool_no_notify_when_none () =
  let db = fresh_db () in
  let tool_t = Task_tree.tool ~db () in
  let args =
    `Assoc
      [
        ( "operations",
          `List
            [ `Assoc [ ("op", `String "add"); ("title", `String "No notify") ] ]
        );
      ]
  in
  let ctx =
    {
      Tool.session_key = Some "test:1";
      send_progress = None;
      interrupt_check = None;
    }
  in
  let result = Lwt_main.run (tool_t.invoke ~context:ctx args) in
  Alcotest.(check bool)
    "tool works without notify" true (contains result "Added")

let fresh_templates_dir () =
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "clawq_test_templates_%d_%f" (Unix.getpid ())
         (Unix.gettimeofday ()))
  in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error _ -> ());
  Task_tree.set_templates_dir dir;
  dir

let cleanup_templates_dir dir =
  (try
     Array.iter (fun f -> Sys.remove (Filename.concat dir f)) (Sys.readdir dir)
   with _ -> ());
  (try Unix.rmdir dir with _ -> ());
  Task_tree.set_templates_dir dir

let test_seed_inline_basic () =
  let db = fresh_db () in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "seed");
            ( "tasks",
              `List
                [
                  `Assoc [ ("title", `String "Root"); ("depth", `Int 0) ];
                  `Assoc [ ("title", `String "Child A"); ("depth", `Int 1) ];
                  `Assoc [ ("title", `String "Child B"); ("depth", `Int 1) ];
                ] );
          ];
      ]
  in
  (match result with Ok _ -> () | Error e -> Alcotest.fail e);
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  Alcotest.(check int) "three tasks" 3 (List.length tasks);
  let root = List.find (fun t -> t.Task_tree.title = "Root") tasks in
  let child_a = List.find (fun t -> t.Task_tree.title = "Child A") tasks in
  let child_b = List.find (fun t -> t.Task_tree.title = "Child B") tasks in
  Alcotest.(check (option string)) "root is root" None root.parent_id;
  Alcotest.(check (option string))
    "child_a under root" (Some root.id) child_a.parent_id;
  Alcotest.(check (option string))
    "child_b under root" (Some root.id) child_b.parent_id

let test_seed_with_vars () =
  let db = fresh_db () in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "seed");
            ( "vars",
              `Assoc [ ("pr", `String "PR #42"); ("repo", `String "clawq") ] );
            ( "tasks",
              `List
                [
                  `Assoc
                    [
                      ("title", `String "Review {{pr}} in {{repo}}");
                      ("depth", `Int 0);
                      ("note", `String "Check {{repo}} CI");
                    ];
                  `Assoc
                    [ ("title", `String "Read {{pr}} diff"); ("depth", `Int 1) ];
                ] );
          ];
      ]
  in
  (match result with Ok _ -> () | Error e -> Alcotest.fail e);
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  let root = List.find (fun t -> t.Task_tree.id = "1") tasks in
  Alcotest.(check string)
    "title substituted" "Review PR #42 in clawq" root.title;
  Alcotest.(check (option string))
    "note substituted" (Some "Check clawq CI") root.note;
  let child = List.find (fun t -> t.Task_tree.id = "2") tasks in
  Alcotest.(check string)
    "child title substituted" "Read PR #42 diff" child.title

let test_seed_unresolved_vars () =
  let db = fresh_db () in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "seed");
            ("vars", `Assoc [ ("known", `String "yes") ]);
            ( "tasks",
              `List
                [
                  `Assoc
                    [
                      ("title", `String "{{known}} and {{unknown}}");
                      ("depth", `Int 0);
                    ];
                ] );
          ];
      ]
  in
  (match result with Ok _ -> () | Error e -> Alcotest.fail e);
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  Alcotest.(check string)
    "unresolved left as-is" "yes and {{unknown}}" (List.hd tasks).title

let test_seed_exceeds_batch_limit () =
  let db = fresh_db () in
  let many_tasks =
    List.init 51 (fun i ->
        `Assoc
          [ ("title", `String (Printf.sprintf "Task %d" i)); ("depth", `Int 0) ])
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "seed"); ("tasks", `List many_tasks) ] ]
  in
  match result with
  | Error msg ->
      Alcotest.(check bool)
        "mentions max" true
        (contains msg "Too many operations")
  | Ok _ -> Alcotest.fail "Expected error for exceeding batch limit"

let test_seed_mixed_with_other_ops () =
  let db = fresh_db () in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "seed");
            ( "tasks",
              `List
                [
                  `Assoc [ ("title", `String "Seeded task"); ("depth", `Int 0) ];
                ] );
          ];
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "1");
            ("status", `String "in_progress");
          ];
      ]
  in
  (match result with Ok _ -> () | Error e -> Alcotest.fail e);
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  Alcotest.(check int) "one task" 1 (List.length tasks);
  Alcotest.(check string)
    "task is in_progress" "in_progress"
    (Task_tree.string_of_status (List.hd tasks).status)

let test_seed_empty_tasks () =
  let db = fresh_db () in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "seed"); ("tasks", `List []) ] ]
  in
  match result with
  | Error msg ->
      Alcotest.(check bool)
        "mentions at least one" true
        (contains msg "at least one")
  | Ok _ -> Alcotest.fail "Expected error for empty tasks"

let test_seed_neither_template_nor_tasks () =
  let db = fresh_db () in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "seed") ] ]
  in
  match result with
  | Error msg ->
      Alcotest.(check bool)
        "mentions provide" true
        (contains msg "Provide exactly one")
  | Ok _ -> Alcotest.fail "Expected error for missing template/tasks"

let test_seed_both_template_and_tasks () =
  let db = fresh_db () in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "seed");
            ("template", `String "foo");
            ( "tasks",
              `List [ `Assoc [ ("title", `String "T"); ("depth", `Int 0) ] ] );
          ];
      ]
  in
  match result with
  | Error msg ->
      Alcotest.(check bool) "mentions not both" true (contains msg "not both")
  | Ok _ -> Alcotest.fail "Expected error for both template and tasks"

let test_save_template_and_seed_round_trip () =
  let db = fresh_db () in
  let tdir = fresh_templates_dir () in
  let save_result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "save_template");
            ("name", `String "review-loop");
            ("description", `String "PR review workflow");
            ( "tasks",
              `List
                [
                  `Assoc
                    [ ("title", `String "Review {{pr}}"); ("depth", `Int 0) ];
                  `Assoc [ ("title", `String "Read diff"); ("depth", `Int 1) ];
                  `Assoc
                    [
                      ("title", `String "Post review");
                      ("depth", `Int 1);
                      ("note", `String "for {{pr}}");
                    ];
                ] );
          ];
      ]
  in
  (match save_result with
  | Ok output ->
      Alcotest.(check bool)
        "contains Saved" true
        (contains output "Saved template")
  | Error e -> Alcotest.fail ("save_template failed: " ^ e));
  let seed_result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "seed");
            ("template", `String "review-loop");
            ("vars", `Assoc [ ("pr", `String "PR #99") ]);
          ];
      ]
  in
  (match seed_result with
  | Ok _ -> ()
  | Error e -> Alcotest.fail ("seed failed: " ^ e));
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  Alcotest.(check int) "three tasks seeded" 3 (List.length tasks);
  let root = List.find (fun t -> t.Task_tree.title = "Review PR #99") tasks in
  Alcotest.(check (option string)) "root is root" None root.parent_id;
  let post = List.find (fun t -> t.Task_tree.title = "Post review") tasks in
  Alcotest.(check (option string))
    "note substituted" (Some "for PR #99") post.note;
  cleanup_templates_dir tdir

let test_save_template_invalid_name () =
  let db = fresh_db () in
  let _tdir = fresh_templates_dir () in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "save_template");
            ("name", `String "bad name!");
            ( "tasks",
              `List [ `Assoc [ ("title", `String "T"); ("depth", `Int 0) ] ] );
          ];
      ]
  in
  (match result with
  | Error msg ->
      Alcotest.(check bool)
        "mentions alphanumeric" true
        (contains msg "alphanumeric")
  | Ok _ -> Alcotest.fail "Expected error for invalid template name");
  cleanup_templates_dir _tdir

let test_list_templates () =
  let db = fresh_db () in
  let tdir = fresh_templates_dir () in
  ignore
    (Task_tree.process_operations ~db ~session_key:"s1"
       [
         `Assoc
           [
             ("op", `String "save_template");
             ("name", `String "alpha");
             ("description", `String "Alpha template");
             ( "tasks",
               `List [ `Assoc [ ("title", `String "T"); ("depth", `Int 0) ] ] );
           ];
       ]);
  ignore
    (Task_tree.process_operations ~db ~session_key:"s1"
       [
         `Assoc
           [
             ("op", `String "save_template");
             ("name", `String "beta");
             ( "tasks",
               `List [ `Assoc [ ("title", `String "T"); ("depth", `Int 0) ] ] );
           ];
       ]);
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "list_templates") ] ]
  in
  (match result with
  | Ok output ->
      Alcotest.(check bool) "contains alpha" true (contains output "alpha");
      Alcotest.(check bool) "contains beta" true (contains output "beta");
      Alcotest.(check bool)
        "contains description" true
        (contains output "Alpha template")
  | Error e -> Alcotest.fail ("list_templates failed: " ^ e));
  cleanup_templates_dir tdir

let test_delete_template () =
  let db = fresh_db () in
  let tdir = fresh_templates_dir () in
  ignore
    (Task_tree.process_operations ~db ~session_key:"s1"
       [
         `Assoc
           [
             ("op", `String "save_template");
             ("name", `String "to-delete");
             ( "tasks",
               `List [ `Assoc [ ("title", `String "T"); ("depth", `Int 0) ] ] );
           ];
       ]);
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [ ("op", `String "delete_template"); ("name", `String "to-delete") ];
      ]
  in
  (match result with
  | Ok output ->
      Alcotest.(check bool)
        "contains Deleted" true
        (contains output "Deleted template")
  | Error e -> Alcotest.fail ("delete_template failed: " ^ e));
  let list_result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "list_templates") ] ]
  in
  (match list_result with
  | Ok output ->
      Alcotest.(check bool)
        "template gone" true
        (contains output "No saved templates")
  | Error e -> Alcotest.fail ("list_templates after delete failed: " ^ e));
  cleanup_templates_dir tdir

let test_seed_missing_template () =
  let db = fresh_db () in
  let _tdir = fresh_templates_dir () in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "seed"); ("template", `String "nonexistent") ] ]
  in
  (match result with
  | Error msg ->
      Alcotest.(check bool) "mentions not found" true (contains msg "not found");
      Alcotest.(check bool)
        "mentions list_templates" true
        (contains msg "list_templates")
  | Ok _ -> Alcotest.fail "Expected error for missing template");
  cleanup_templates_dir _tdir

let test_find_active_session_key_preferred () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"__main__"
      [ `Assoc [ ("op", `String "add"); ("title", `String "Main task") ] ]
  in
  let result = Task_tree.find_active_session_key ~db ~preferred:"__main__" in
  Alcotest.(check (option string)) "returns preferred" (Some "__main__") result

let test_find_active_session_key_fallback () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"telegram:123:456"
      [ `Assoc [ ("op", `String "add"); ("title", `String "Chat task") ] ]
  in
  let result = Task_tree.find_active_session_key ~db ~preferred:"__main__" in
  Alcotest.(check (option string))
    "falls back to session with tasks" (Some "telegram:123:456") result

let test_find_active_session_key_empty () =
  let db = fresh_db () in
  let result = Task_tree.find_active_session_key ~db ~preferred:"__main__" in
  Alcotest.(check (option string)) "returns None when empty" None result

let test_render_compact_empty () =
  let db = fresh_db () in
  let result = Task_tree.render_compact ~db ~session_key:"s1" in
  Alcotest.(check bool)
    "contains no tasks message" true
    (String.length result > 0
    &&
      try
        ignore
          (Str.search_forward (Str.regexp_string "No tasks tracked") result 0);
        true
      with Not_found -> false)

let test_render_compact_summary_counts () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc [ ("op", `String "add"); ("title", `String "T1") ];
        `Assoc [ ("op", `String "add"); ("title", `String "T2") ];
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "1");
            ("status", `String "in_progress");
          ];
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "2");
            ("status", `String "done");
          ];
      ]
  in
  let result = Task_tree.render_compact ~db ~session_key:"s1" in
  Alcotest.(check bool)
    "has total count" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Tasks: 2 total") result 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "has active count" true
    (try
       ignore (Str.search_forward (Str.regexp_string "1 active") result 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "has done count" true
    (try
       ignore (Str.search_forward (Str.regexp_string "1 done") result 0);
       true
     with Not_found -> false)

let test_render_compact_shows_active_and_error () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc [ ("op", `String "add"); ("title", `String "Active task") ];
        `Assoc [ ("op", `String "add"); ("title", `String "Blocked task") ];
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "1");
            ("status", `String "in_progress");
          ];
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "2");
            ("status", `String "error");
            ("note", `String "upstream issue");
          ];
      ]
  in
  let result = Task_tree.render_compact ~db ~session_key:"s1" in
  Alcotest.(check bool)
    "shows active section" true
    (try
       ignore (Str.search_forward (Str.regexp_string "[>] #1") result 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "shows active title" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Active task") result 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "shows blocked section" true
    (try
       ignore (Str.search_forward (Str.regexp_string "[!] #2") result 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "shows error note" true
    (try
       ignore (Str.search_forward (Str.regexp_string "upstream issue") result 0);
       true
     with Not_found -> false)

let test_render_compact_hides_done_cancelled () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc [ ("op", `String "add"); ("title", `String "Done task") ];
        `Assoc [ ("op", `String "add"); ("title", `String "Cancelled task") ];
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "1");
            ("status", `String "done");
          ];
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "2");
            ("status", `String "cancelled");
          ];
      ]
  in
  let result = Task_tree.render_compact ~db ~session_key:"s1" in
  Alcotest.(check bool)
    "no Done task title listed" true
    (not
       (try
          ignore
            (Str.search_forward (Str.regexp_string "#1 — Done task") result 0);
          true
        with Not_found -> false));
  Alcotest.(check bool)
    "no Cancelled task title listed" true
    (not
       (try
          ignore
            (Str.search_forward
               (Str.regexp_string "#2 — Cancelled task")
               result 0);
          true
        with Not_found -> false));
  Alcotest.(check bool)
    "archive nudge present" true
    (try
       ignore
         (Str.search_forward (Str.regexp_string "2 done — archive") result 0);
       true
     with Not_found -> false)

let test_render_compact_next_pending_actionable () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Parent"); ("depth", `Int 0);
          ];
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Child"); ("depth", `Int 1);
          ];
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Root2"); ("depth", `Int 0);
          ];
      ]
  in
  let result = Task_tree.render_compact ~db ~session_key:"s1" in
  (* Parent and Root2 are root-actionable pending; Child has pending parent *)
  Alcotest.(check bool)
    "shows Parent in Next" true
    (try
       ignore (Str.search_forward (Str.regexp_string "#1 — Parent") result 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "shows Root2 in Next" true
    (try
       ignore (Str.search_forward (Str.regexp_string "#3 — Root2") result 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "hides Child (has pending parent)" true
    (not
       (try
          ignore (Str.search_forward (Str.regexp_string "#2 — Child") result 0);
          true
        with Not_found -> false))

let test_render_compact_limits_pending () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc [ ("op", `String "add"); ("title", `String "R1") ];
        `Assoc [ ("op", `String "add"); ("title", `String "R2") ];
        `Assoc [ ("op", `String "add"); ("title", `String "R3") ];
        `Assoc [ ("op", `String "add"); ("title", `String "R4") ];
        `Assoc [ ("op", `String "add"); ("title", `String "R5") ];
      ]
  in
  let result = Task_tree.render_compact ~db ~session_key:"s1" in
  Alcotest.(check bool)
    "overflow indicator present" true
    (try
       ignore (Str.search_forward (Str.regexp_string "(+2 more)") result 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "R4 not shown" true
    (not
       (try
          ignore (Str.search_forward (Str.regexp_string "#4 — R4") result 0);
          true
        with Not_found -> false))

let test_render_compact_archive_nudge () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc [ ("op", `String "add"); ("title", `String "T1") ];
        `Assoc [ ("op", `String "add"); ("title", `String "T2") ];
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "1");
            ("status", `String "done");
          ];
      ]
  in
  let result = Task_tree.render_compact ~db ~session_key:"s1" in
  Alcotest.(check bool)
    "archive nudge" true
    (try
       ignore
         (Str.search_forward
            (Str.regexp_string "1 done — archive to save tokens")
            result 0);
       true
     with Not_found -> false)

let test_render_focus_empty () =
  let db = fresh_db () in
  let result = Task_tree.render_focus ~db ~session_key:"s1" in
  Alcotest.(check bool)
    "contains no tasks message" true
    (try
       ignore
         (Str.search_forward (Str.regexp_string "No tasks tracked") result 0);
       true
     with Not_found -> false)

let test_render_focus_active_with_path () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Root"); ("depth", `Int 0);
          ];
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Child"); ("depth", `Int 1);
          ];
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "2");
            ("status", `String "in_progress");
          ];
      ]
  in
  let result = Task_tree.render_focus ~db ~session_key:"s1" in
  Alcotest.(check bool)
    "shows path line" true
    (try
       ignore (Str.search_forward (Str.regexp_string "path:") result 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "path contains Root" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Root") result 0);
       true
     with Not_found -> false)

let test_render_focus_root_active_no_path () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc [ ("op", `String "add"); ("title", `String "Root task") ];
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "1");
            ("status", `String "in_progress");
          ];
      ]
  in
  let result = Task_tree.render_focus ~db ~session_key:"s1" in
  Alcotest.(check bool)
    "root active has no path line" true
    (not
       (try
          ignore (Str.search_forward (Str.regexp_string "path:") result 0);
          true
        with Not_found -> false));
  Alcotest.(check bool)
    "root active shown in Active section" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Root task") result 0);
       true
     with Not_found -> false)

let test_render_focus_blocked_with_note () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc [ ("op", `String "add"); ("title", `String "Stuck task") ];
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "1");
            ("status", `String "error");
            ("note", `String "waiting on API");
          ];
      ]
  in
  let result = Task_tree.render_focus ~db ~session_key:"s1" in
  Alcotest.(check bool)
    "Blocked section present" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Blocked:") result 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "note shown in blocked" true
    (try
       ignore (Str.search_forward (Str.regexp_string "waiting on API") result 0);
       true
     with Not_found -> false)

let test_render_focus_no_active_shows_next () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc [ ("op", `String "add"); ("title", `String "Pending A") ];
        `Assoc [ ("op", `String "add"); ("title", `String "Pending B") ];
      ]
  in
  let result = Task_tree.render_focus ~db ~session_key:"s1" in
  Alcotest.(check bool)
    "Next section shown" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Next:") result 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "Pending A in Next" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Pending A") result 0);
       true
     with Not_found -> false)

let test_render_focus_prefers_children_of_active () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Parent task");
            ("depth", `Int 0);
          ];
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Child task");
            ("depth", `Int 1);
          ];
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Other root");
            ("depth", `Int 0);
          ];
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "1");
            ("status", `String "in_progress");
          ];
      ]
  in
  let result = Task_tree.render_focus ~db ~session_key:"s1" in
  let child_pos =
    try Str.search_forward (Str.regexp_string "Child task") result 0
    with Not_found -> max_int
  in
  let other_pos =
    try Str.search_forward (Str.regexp_string "Other root") result 0
    with Not_found -> max_int
  in
  (* Child task (child of active) should appear before Other root *)
  Alcotest.(check bool)
    "child of active appears before other pending root" true
    (child_pos < other_pos)

let test_process_operations_compact_output () =
  let db = fresh_db () in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "add"); ("title", `String "Test task") ] ]
  in
  match result with
  | Ok output ->
      Alcotest.(check bool)
        "no legend line" true
        (not
           (try
              ignore (Str.search_forward (Str.regexp_string "Legend:") output 0);
              true
            with Not_found -> false));
      Alcotest.(check bool)
        "has compact summary" true
        (try
           ignore
             (Str.search_forward (Str.regexp_string "Tasks: 1 total") output 0);
           true
         with Not_found -> false)
  | Error e -> Alcotest.fail ("Expected Ok, got Error: " ^ e)

(* ── B293: bulk ops, soft delete, restore ── *)

let test_recursive_update_cancelled () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Root"); ("depth", `Int 0);
          ];
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Child A");
            ("depth", `Int 1);
          ];
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Child B");
            ("depth", `Int 1);
          ];
      ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "1");
            ("status", `String "cancelled");
            ("recursive", `Bool true);
          ];
      ]
  in
  (match result with
  | Ok _ -> ()
  | Error e -> Alcotest.fail ("Expected Ok, got Error: " ^ e));
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  Alcotest.(check int) "all 3 tasks still visible" 3 (List.length tasks);
  List.iter
    (fun t ->
      Alcotest.(check string)
        (Printf.sprintf "#%s is cancelled" t.Task_tree.id)
        "cancelled"
        (Task_tree.string_of_status t.status))
    tasks

let test_recursive_update_done () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Root"); ("depth", `Int 0);
          ];
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Child"); ("depth", `Int 1);
          ];
      ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "1");
            ("status", `String "done");
            ("recursive", `Bool true);
          ];
      ]
  in
  (match result with
  | Ok output ->
      Alcotest.(check bool)
        "contains recursively" true
        (try
           ignore
             (Str.search_forward (Str.regexp_string "recursively") output 0);
           true
         with Not_found -> false)
  | Error e -> Alcotest.fail ("Expected Ok, got Error: " ^ e));
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  List.iter
    (fun t ->
      Alcotest.(check string)
        (Printf.sprintf "#%s is done" t.Task_tree.id)
        "done"
        (Task_tree.string_of_status t.status))
    tasks

let test_recursive_update_invalid_status () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "add"); ("title", `String "Root") ] ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "1");
            ("status", `String "pending");
            ("recursive", `Bool true);
          ];
      ]
  in
  match result with
  | Error _ -> ()
  | Ok _ ->
      Alcotest.fail "Expected Error for recursive=true with status=pending"

let test_recursive_update_missing_status () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "add"); ("title", `String "Root") ] ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "update");
            ("id", `String "1");
            ("recursive", `Bool true);
          ];
      ]
  in
  match result with
  | Error msg ->
      Alcotest.(check bool)
        "mentions status required" true
        (try
           ignore (Str.search_forward (Str.regexp_string "status") msg 0);
           true
         with Not_found -> false)
  | Ok _ ->
      Alcotest.fail "Expected Error for recursive=true without status field"

let test_remove_soft_deletes () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "add"); ("title", `String "Task") ] ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "remove"); ("id", `String "1") ] ]
  in
  (match result with
  | Ok output ->
      Alcotest.(check bool)
        "contains restore hint" true
        (try
           ignore
             (Str.search_forward (Str.regexp_string "Restore with") output 0);
           true
         with Not_found -> false)
  | Error e -> Alcotest.fail ("Expected Ok, got Error: " ^ e));
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  Alcotest.(check int) "no active tasks" 0 (List.length tasks);
  let all_tasks =
    Task_tree.load_tasks ~include_deleted:true ~db ~session_key:"s1" ()
  in
  Alcotest.(check int) "one deleted task" 1 (List.length all_tasks);
  Alcotest.(check bool)
    "deleted_at is set" true
    ((List.hd all_tasks).Task_tree.deleted_at <> None)

let test_remove_recursive_bypasses_in_progress () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Root"); ("depth", `Int 0);
          ];
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Child");
            ("depth", `Int 1);
            ("status", `String "in_progress");
          ];
      ]
  in
  let r1 =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "remove"); ("id", `String "1") ] ]
  in
  (match r1 with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "Expected error for remove with in_progress subtree");
  let r2 =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "remove");
            ("id", `String "1");
            ("recursive", `Bool true);
          ];
      ]
  in
  (match r2 with
  | Ok _ -> ()
  | Error e -> Alcotest.fail ("Expected Ok with recursive=true, got: " ^ e));
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  Alcotest.(check int) "all removed" 0 (List.length tasks)

let test_clear_soft_deletes () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Done task");
            ("status", `String "done");
          ];
        `Assoc [ ("op", `String "add"); ("title", `String "Pending task") ];
      ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "clear") ] ]
  in
  (match result with
  | Ok output ->
      Alcotest.(check bool)
        "mentions soft-deleted" true
        (try
           ignore
             (Str.search_forward (Str.regexp_string "Soft-deleted") output 0);
           true
         with Not_found -> false)
  | Error e -> Alcotest.fail ("Expected Ok, got Error: " ^ e));
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  Alcotest.(check int) "only pending task remains active" 1 (List.length tasks);
  Alcotest.(check string)
    "remaining task is pending" "Pending task" (List.hd tasks).title

let test_restore_task () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Root"); ("depth", `Int 0);
          ];
        `Assoc
          [
            ("op", `String "add"); ("title", `String "Child"); ("depth", `Int 1);
          ];
      ]
  in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "remove");
            ("id", `String "1");
            ("recursive", `Bool true);
          ];
      ]
  in
  let active_before = Task_tree.load_tasks ~db ~session_key:"s1" () in
  Alcotest.(check int)
    "no active tasks after remove" 0
    (List.length active_before);
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "restore"); ("id", `String "1") ] ]
  in
  (match result with
  | Ok output ->
      Alcotest.(check bool)
        "contains Restored" true
        (try
           ignore (Str.search_forward (Str.regexp_string "Restored") output 0);
           true
         with Not_found -> false)
  | Error e -> Alcotest.fail ("Expected Ok, got Error: " ^ e));
  let tasks = Task_tree.load_tasks ~db ~session_key:"s1" () in
  Alcotest.(check int) "both tasks restored" 2 (List.length tasks);
  List.iter
    (fun t ->
      Alcotest.(check bool)
        (Printf.sprintf "#%s deleted_at is None" t.Task_tree.id)
        true
        (t.Task_tree.deleted_at = None))
    tasks

let test_restore_not_deleted () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "add"); ("title", `String "Task") ] ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "restore"); ("id", `String "1") ] ]
  in
  match result with
  | Error _ -> ()
  | Ok _ ->
      Alcotest.fail "Expected Error when restoring an active (non-deleted) task"

let test_restore_not_found () =
  let db = fresh_db () in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "restore"); ("id", `String "999") ] ]
  in
  match result with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "Expected Error when restoring nonexistent task"

let test_list_include_deleted () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc [ ("op", `String "add"); ("title", `String "Active task") ];
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Deleted task");
            ("status", `String "done");
          ];
      ]
  in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "remove"); ("id", `String "2") ] ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "list"); ("include_deleted", `Bool true) ] ]
  in
  match result with
  | Ok output ->
      Alcotest.(check bool)
        "lists active task" true
        (try
           ignore
             (Str.search_forward (Str.regexp_string "Active task") output 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "lists deleted task" true
        (try
           ignore
             (Str.search_forward (Str.regexp_string "Deleted task") output 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "marks deleted" true
        (try
           ignore (Str.search_forward (Str.regexp_string "[deleted]") output 0);
           true
         with Not_found -> false)
  | Error e -> Alcotest.fail ("Expected Ok, got Error: " ^ e)

let test_list_excludes_deleted_by_default () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc [ ("op", `String "add"); ("title", `String "Active") ];
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Deleted");
            ("status", `String "done");
          ];
      ]
  in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "remove"); ("id", `String "2") ] ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "list") ] ]
  in
  match result with
  | Ok output ->
      Alcotest.(check bool)
        "shows active" true
        (try
           ignore (Str.search_forward (Str.regexp_string "Active") output 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "hides deleted" false
        (try
           ignore (Str.search_forward (Str.regexp_string "Deleted") output 0);
           true
         with Not_found -> false)
  | Error e -> Alcotest.fail ("Expected Ok, got Error: " ^ e)

let test_list_op_explicit () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "add"); ("title", `String "My task") ] ]
  in
  let result =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "list") ] ]
  in
  match result with
  | Ok output ->
      Alcotest.(check bool)
        "list shows task" true
        (try
           ignore (Str.search_forward (Str.regexp_string "My task") output 0);
           true
         with Not_found -> false)
  | Error e -> Alcotest.fail ("Expected Ok, got Error: " ^ e)

let test_default_list_empty_ops () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc [ ("op", `String "add"); ("title", `String "Default list task") ];
      ]
  in
  let result = Task_tree.process_operations ~db ~session_key:"s1" [] in
  match result with
  | Ok output ->
      Alcotest.(check bool)
        "empty ops defaults to list" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "Default list task")
                output 0);
           true
         with Not_found -> false)
  | Error e -> Alcotest.fail ("Expected Ok, got Error: " ^ e)

let test_default_list_empty_db () =
  let db = fresh_db () in
  let result = Task_tree.process_operations ~db ~session_key:"s1" [] in
  match result with
  | Ok output ->
      Alcotest.(check bool)
        "empty ops on empty db returns ok" true
        (String.length output > 0)
  | Error e -> Alcotest.fail ("Expected Ok on empty list, got Error: " ^ e)

let test_purge_disabled_by_default () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Task");
            ("status", `String "done");
          ];
      ]
  in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [ `Assoc [ ("op", `String "remove"); ("id", `String "1") ] ]
  in
  Task_tree.maybe_purge_deleted_tasks ~db ~config:Runtime_config.default;
  let all =
    Task_tree.load_tasks ~include_deleted:true ~db ~session_key:"s1" ()
  in
  Alcotest.(check int)
    "soft-deleted row survives when purge disabled" 1 (List.length all)

let test_purge_removes_old_rows () =
  let db = fresh_db () in
  let _ =
    Task_tree.process_operations ~db ~session_key:"s1"
      [
        `Assoc
          [
            ("op", `String "add");
            ("title", `String "Task");
            ("status", `String "done");
          ];
      ]
  in
  Memory.exec_exn db
    "UPDATE task_tree SET deleted_at = datetime('now', '-2 days') WHERE id = \
     '1'";
  let config =
    {
      Runtime_config.default with
      memory =
        { Runtime_config.default.memory with task_tree_purge_after_days = 1 };
    }
  in
  Task_tree.maybe_purge_deleted_tasks ~db ~config;
  let all =
    Task_tree.load_tasks ~include_deleted:true ~db ~session_key:"s1" ()
  in
  Alcotest.(check int) "old soft-deleted row hard-purged" 0 (List.length all)

let suite =
  [
    Alcotest.test_case "init_schema idempotent" `Quick
      test_init_schema_idempotent;
    Alcotest.test_case "add root task" `Quick test_add_root_task;
    Alcotest.test_case "add custom string ID" `Quick test_add_custom_string_id;
    Alcotest.test_case "add depth auto-nesting" `Quick
      test_add_depth_auto_nesting;
    Alcotest.test_case "add explicit parent" `Quick test_add_explicit_parent;
    Alcotest.test_case "depth jump validation" `Quick test_depth_jump_validation;
    Alcotest.test_case "update status lifecycle" `Quick
      test_update_status_lifecycle;
    Alcotest.test_case "update done with incomplete children" `Quick
      test_update_done_with_incomplete_children;
    Alcotest.test_case "update done after completing children" `Quick
      test_update_done_after_completing_children;
    Alcotest.test_case "update done with cancelled child" `Quick
      test_update_done_with_cancelled_child;
    Alcotest.test_case "update done with mixed done/cancelled children" `Quick
      test_update_done_with_mixed_children;
    Alcotest.test_case "in_progress propagation" `Quick
      test_in_progress_propagation;
    Alcotest.test_case "remove task and subtree" `Quick
      test_remove_task_and_subtree;
    Alcotest.test_case "remove blocked by in_progress" `Quick
      test_remove_blocked_by_in_progress;
    Alcotest.test_case "clear only done/cancelled" `Quick
      test_clear_only_done_cancelled;
    Alcotest.test_case "archive completed subtree" `Quick
      test_archive_completed_subtree;
    Alcotest.test_case "archive resets auto-ID" `Quick
      test_archive_resets_auto_id;
    Alcotest.test_case "archive blocked by incomplete" `Quick
      test_archive_blocked_by_incomplete;
    Alcotest.test_case "session isolation" `Quick test_session_isolation;
    Alcotest.test_case "render empty tree" `Quick test_render_empty_tree;
    Alcotest.test_case "render populated tree" `Quick test_render_populated_tree;
    Alcotest.test_case "batch operations" `Quick test_batch_operations;
    Alcotest.test_case "tool invoke round-trip" `Quick
      test_tool_invoke_round_trip;
    Alcotest.test_case "ID collision rejected" `Quick test_id_collision_rejected;
    Alcotest.test_case "auto-ID skips agent-chosen" `Quick
      test_auto_id_skips_agent_chosen;
    Alcotest.test_case "re-open task" `Quick test_reopen_task;
    Alcotest.test_case "in_progress adds can exceed old live cap" `Quick
      test_in_progress_adds_can_exceed_old_live_cap;
    Alcotest.test_case "max depth guardrail" `Quick test_max_depth_guardrail;
    Alcotest.test_case "max concurrent in_progress" `Quick
      test_max_concurrent_in_progress;
    Alcotest.test_case "batch transaction rollback" `Quick
      test_batch_transaction_rollback;
    Alcotest.test_case "render with legend" `Quick test_render_with_legend;
    Alcotest.test_case "note-only update" `Quick test_note_only_update;
    Alcotest.test_case "comma-separated ID update" `Quick
      test_comma_separated_id_update;
    Alcotest.test_case "archive all completed roots" `Quick
      test_archive_all_completed_roots;
    Alcotest.test_case "reorder move to first" `Quick test_reorder_move_to_first;
    Alcotest.test_case "reorder move to last" `Quick test_reorder_move_to_last;
    Alcotest.test_case "reorder after sibling" `Quick test_reorder_after_sibling;
    Alcotest.test_case "reorder before sibling" `Quick
      test_reorder_before_sibling;
    Alcotest.test_case "reorder not found" `Quick test_reorder_not_found;
    Alcotest.test_case "reorder no siblings" `Quick test_reorder_no_siblings;
    Alcotest.test_case "reorder non-sibling target" `Quick
      test_reorder_non_sibling_target;
    Alcotest.test_case "reorder invalid position" `Quick
      test_reorder_invalid_position;
    Alcotest.test_case "reorder preserves children" `Quick
      test_reorder_preserves_children;
    Alcotest.test_case "empty parent normalized to root" `Quick
      test_add_empty_parent_normalized_to_root;
    Alcotest.test_case "whitespace parent normalized" `Quick
      test_add_whitespace_parent_normalized;
    Alcotest.test_case "depth overrides invalid parent" `Quick
      test_add_depth_overrides_invalid_parent;
    Alcotest.test_case "batch B237 scenario" `Quick test_add_batch_b237_scenario;
    Alcotest.test_case "depth and valid parent uses depth" `Quick
      test_add_depth_and_valid_parent_uses_depth;
    Alcotest.test_case "error msg parent not found has suggestion" `Quick
      test_error_msg_parent_not_found_has_suggestion;
    Alcotest.test_case "error msg unknown op lists valid" `Quick
      test_error_msg_unknown_op_lists_valid;
    Alcotest.test_case "format notification add" `Quick
      test_format_notification_add;
    Alcotest.test_case "format notification update" `Quick
      test_format_notification_update;
    Alcotest.test_case "format notification focus" `Quick
      test_format_notification_focus;
    Alcotest.test_case "format notification multiple active" `Quick
      test_format_notification_multiple_active;
    Alcotest.test_case "format notification reorder only" `Quick
      test_format_notification_reorder_only;
    Alcotest.test_case "format notification Discord formatting" `Quick
      test_format_notification_discord_formatting;
    Alcotest.test_case "tool notify called on success" `Quick
      test_tool_notify_called_on_success;
    Alcotest.test_case "tool notify not called on error" `Quick
      test_tool_notify_not_called_on_error;
    Alcotest.test_case "tool no notify when None" `Quick
      test_tool_no_notify_when_none;
    Alcotest.test_case "seed inline basic" `Quick test_seed_inline_basic;
    Alcotest.test_case "seed with vars" `Quick test_seed_with_vars;
    Alcotest.test_case "seed unresolved vars" `Quick test_seed_unresolved_vars;
    Alcotest.test_case "seed exceeds batch limit" `Quick
      test_seed_exceeds_batch_limit;
    Alcotest.test_case "seed mixed with other ops" `Quick
      test_seed_mixed_with_other_ops;
    Alcotest.test_case "seed empty tasks" `Quick test_seed_empty_tasks;
    Alcotest.test_case "seed neither template nor tasks" `Quick
      test_seed_neither_template_nor_tasks;
    Alcotest.test_case "seed both template and tasks" `Quick
      test_seed_both_template_and_tasks;
    Alcotest.test_case "save_template + seed round-trip" `Quick
      test_save_template_and_seed_round_trip;
    Alcotest.test_case "save_template invalid name" `Quick
      test_save_template_invalid_name;
    Alcotest.test_case "list_templates" `Quick test_list_templates;
    Alcotest.test_case "delete_template" `Quick test_delete_template;
    Alcotest.test_case "seed missing template" `Quick test_seed_missing_template;
    Alcotest.test_case "find_active_session_key prefers preferred" `Quick
      test_find_active_session_key_preferred;
    Alcotest.test_case "find_active_session_key falls back" `Quick
      test_find_active_session_key_fallback;
    Alcotest.test_case "find_active_session_key empty db" `Quick
      test_find_active_session_key_empty;
    Alcotest.test_case "render_compact empty" `Quick test_render_compact_empty;
    Alcotest.test_case "render_compact summary counts" `Quick
      test_render_compact_summary_counts;
    Alcotest.test_case "render_compact active and error" `Quick
      test_render_compact_shows_active_and_error;
    Alcotest.test_case "render_compact hides done/cancelled" `Quick
      test_render_compact_hides_done_cancelled;
    Alcotest.test_case "render_compact next pending actionable" `Quick
      test_render_compact_next_pending_actionable;
    Alcotest.test_case "render_compact limits pending" `Quick
      test_render_compact_limits_pending;
    Alcotest.test_case "render_compact archive nudge" `Quick
      test_render_compact_archive_nudge;
    Alcotest.test_case "render_focus empty" `Quick test_render_focus_empty;
    Alcotest.test_case "render_focus active with path" `Quick
      test_render_focus_active_with_path;
    Alcotest.test_case "render_focus root active no path" `Quick
      test_render_focus_root_active_no_path;
    Alcotest.test_case "render_focus blocked with note" `Quick
      test_render_focus_blocked_with_note;
    Alcotest.test_case "render_focus no active shows next" `Quick
      test_render_focus_no_active_shows_next;
    Alcotest.test_case "render_focus prefers children of active" `Quick
      test_render_focus_prefers_children_of_active;
    Alcotest.test_case "process_operations compact output" `Quick
      test_process_operations_compact_output;
    (* B293: bulk ops, soft delete, restore *)
    Alcotest.test_case "recursive update cancelled" `Quick
      test_recursive_update_cancelled;
    Alcotest.test_case "recursive update done" `Quick test_recursive_update_done;
    Alcotest.test_case "recursive update invalid status" `Quick
      test_recursive_update_invalid_status;
    Alcotest.test_case "recursive update missing status" `Quick
      test_recursive_update_missing_status;
    Alcotest.test_case "remove soft-deletes" `Quick test_remove_soft_deletes;
    Alcotest.test_case "remove recursive bypasses in_progress" `Quick
      test_remove_recursive_bypasses_in_progress;
    Alcotest.test_case "clear soft-deletes" `Quick test_clear_soft_deletes;
    Alcotest.test_case "restore task" `Quick test_restore_task;
    Alcotest.test_case "restore not deleted" `Quick test_restore_not_deleted;
    Alcotest.test_case "restore not found" `Quick test_restore_not_found;
    Alcotest.test_case "list include_deleted" `Quick test_list_include_deleted;
    Alcotest.test_case "list excludes deleted by default" `Quick
      test_list_excludes_deleted_by_default;
    Alcotest.test_case "list op explicit" `Quick test_list_op_explicit;
    Alcotest.test_case "default list empty ops" `Quick
      test_default_list_empty_ops;
    Alcotest.test_case "default list empty db" `Quick test_default_list_empty_db;
    Alcotest.test_case "purge disabled by default" `Quick
      test_purge_disabled_by_default;
    Alcotest.test_case "purge removes old rows" `Quick
      test_purge_removes_old_rows;
  ]
