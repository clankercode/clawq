(** Tests for [Room_stale_query] — the stale room task/thread query engine. *)

open Room_stale_query

let with_db f =
  Test_helpers.with_memory_store
    ~init_schema:[ Background_task.init_schema; Task_tree_core.init_schema ]
    f

let with_temp_git_repo f =
  let tmp = Filename.temp_file "clawq-test" ".dir" in
  Sys.remove tmp;
  Unix.mkdir tmp 0o755;
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote tmp))))
    (fun () ->
      ignore
        (Sys.command (Printf.sprintf "git -C %s init -q" (Filename.quote tmp)));
      f tmp)

(** Helper: build a Room_origin JSON string with given fields. *)
let make_origin_json ?room_id ?thread_id ?requester () =
  let open Yojson.Safe in
  let fields =
    [
      ("connector", Some (`String "slack"));
      ("room_id", Option.map (fun s -> `String s) room_id);
      ("thread_id", Option.map (fun s -> `String s) thread_id);
      ("requester_name", Option.map (fun s -> `String s) requester);
    ]
    |> List.filter_map (fun (k, v) -> Option.map (fun v -> (k, v)) v)
  in
  Yojson.Safe.to_string (`Assoc fields)

(** Helper: enqueue a background task with origin fields. *)
let enqueue_bg_task ~db ?(runner = Background_task.Codex) ~repo_path ~prompt
    ?origin_json ?thread_id ?requester () =
  match
    Background_task.enqueue ~db ~runner ~repo_path ~prompt ?origin_json
      ?thread_id ?requester ()
  with
  | Ok id -> id
  | Error msg -> Alcotest.fail ("enqueue failed: " ^ msg)

(** Helper: add a task_tree task via process_operations with origin data. *)
let add_tree_task ~db ~session_key ~id ~title ?origin_json ?thread_id ?requester
    ?(status = "pending") () =
  let ops =
    `Assoc
      ([
         ("op", `String "add");
         ("id", `String id);
         ("title", `String title);
         ("status", `String status);
       ]
      @ (match origin_json with
        | Some oj -> [ ("origin_json", `String oj) ]
        | None -> [])
      @ (match thread_id with
        | Some tid -> [ ("thread_id", `String tid) ]
        | None -> [])
      @
      match requester with
      | Some r -> [ ("requester", `String r) ]
      | None -> [])
  in
  match Task_tree.process_operations ~db ~session_key [ ops ] with
  | Ok _ -> ()
  | Error e -> Alcotest.fail ("add_tree_task failed: " ^ e)

(** Helper: update task_tree status via process_operations. *)
let update_tree_status ~db ~session_key ~id ~status =
  let ops =
    `Assoc
      [
        ("op", `String "update"); ("id", `String id); ("status", `String status);
      ]
  in
  match Task_tree.process_operations ~db ~session_key [ ops ] with
  | Ok _ -> ()
  | Error e -> Alcotest.fail ("update status failed: " ^ e)

(** Helper: set the created_at of a background task to a specific datetime. *)
let set_bg_created_at ~db ~id ~datetime =
  let sql = "UPDATE background_tasks SET created_at = ? WHERE id = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT datetime));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int id)));
      ignore (Sqlite3.step stmt))

(** Helper: set created_at of a task_tree task. *)
let set_tree_created_at ~db ~session_key ~id ~datetime =
  let sql =
    "UPDATE task_tree SET created_at = ? WHERE session_key = ? AND id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT datetime));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT session_key));
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT id));
      ignore (Sqlite3.step stmt))

(** Helper: set updated_at of a task_tree task. *)
let set_tree_updated_at ~db ~session_key ~id ~datetime =
  let sql =
    "UPDATE task_tree SET updated_at = ? WHERE session_key = ? AND id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT datetime));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT session_key));
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT id));
      ignore (Sqlite3.step stmt))

(** Helper: set the started_at of a background task. *)
let set_bg_started_at ~db ~id ~datetime =
  let sql = "UPDATE background_tasks SET started_at = ? WHERE id = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT datetime));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int id)));
      ignore (Sqlite3.step stmt))

(** Helper: set background task status manually. *)
let set_bg_status ~db ~id ~status =
  let sql = "UPDATE background_tasks SET status = ? WHERE id = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT status));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int id)));
      ignore (Sqlite3.step stmt))

(* --- Tests --- *)

(** Empty DB returns no stale items. *)
let test_empty_db () =
  with_db (fun db ->
      let items = find_stale ~db ~now:1000.0 ~stale_after_s:60.0 () in
      Alcotest.(check int) "empty" 0 (List.length items))

(** Fresh (not yet stale) background task is not returned. *)
let test_bg_task_not_stale () =
  with_temp_git_repo (fun repo_path ->
      with_db (fun db ->
          let origin = make_origin_json ~room_id:"C001" ~requester:"Alice" () in
          let _id =
            enqueue_bg_task ~db ~repo_path ~prompt:"do stuff"
              ~origin_json:origin ()
          in
          (* created_at is datetime('now'), stale_after is 1 hour.
             With now=far_future, it should not be stale because we set
             created_at to near now. *)
          let now = Unix.gettimeofday () in
          let items =
            find_stale ~db ~now ~stale_after_s:3600.0 ~room_id:"C001" ()
          in
          Alcotest.(check int) "fresh bg task" 0 (List.length items)))

(** Old queued background task IS returned as stale. *)
let test_bg_task_stale_queued () =
  with_temp_git_repo (fun repo_path ->
      with_db (fun db ->
          let origin = make_origin_json ~room_id:"C001" ~requester:"Alice" () in
          let id =
            enqueue_bg_task ~db ~repo_path ~prompt:"old task"
              ~origin_json:origin ()
          in
          (* Set created_at to 2 hours ago *)
          let two_hours_ago =
            Room_stale_query.unix_to_sqlite_datetime
              (Unix.gettimeofday () -. 7200.0)
          in
          set_bg_created_at ~db ~id ~datetime:two_hours_ago;
          let now = Unix.gettimeofday () in
          let items =
            find_stale ~db ~now ~stale_after_s:1800.0 ~room_id:"C001" ()
          in
          Alcotest.(check int) "one stale" 1 (List.length items);
          let item = List.hd items in
          Alcotest.(check string)
            "source" "background_task"
            (source_to_string item.source);
          Alcotest.(check string) "status" "queued" item.status;
          Alcotest.(check (option string)) "room_id" (Some "C001") item.room_id))

(** Old running background task IS returned as stale. *)
let test_bg_task_stale_running () =
  with_temp_git_repo (fun repo_path ->
      with_db (fun db ->
          let origin = make_origin_json ~room_id:"C002" () in
          let id =
            enqueue_bg_task ~db ~repo_path ~prompt:"running task"
              ~origin_json:origin ()
          in
          set_bg_status ~db ~id ~status:"running";
          let one_hour_ago =
            Room_stale_query.unix_to_sqlite_datetime
              (Unix.gettimeofday () -. 3600.0)
          in
          set_bg_started_at ~db ~id ~datetime:one_hour_ago;
          let now = Unix.gettimeofday () in
          let items =
            find_stale ~db ~now ~stale_after_s:1800.0 ~room_id:"C002" ()
          in
          Alcotest.(check int) "one stale running" 1 (List.length items);
          let item = List.hd items in
          Alcotest.(check string) "status" "running" item.status;
          Alcotest.(check bool) "age > 1800" true (item.age_seconds >= 1800.0)))

(** Terminal background tasks (succeeded, failed) are never returned. *)
let test_bg_task_terminal_excluded () =
  with_temp_git_repo (fun repo_path ->
      with_db (fun db ->
          let origin = make_origin_json ~room_id:"C003" () in
          let id_succ =
            enqueue_bg_task ~db ~repo_path ~prompt:"done task"
              ~origin_json:origin ()
          in
          set_bg_status ~db ~id:id_succ ~status:"succeeded";
          set_bg_created_at ~db ~id:id_succ
            ~datetime:
              (Room_stale_query.unix_to_sqlite_datetime
                 (Unix.gettimeofday () -. 86400.0));
          let id_fail =
            enqueue_bg_task ~db ~repo_path ~prompt:"failed task"
              ~origin_json:origin ()
          in
          set_bg_status ~db ~id:id_fail ~status:"failed";
          set_bg_created_at ~db ~id:id_fail
            ~datetime:
              (Room_stale_query.unix_to_sqlite_datetime
                 (Unix.gettimeofday () -. 86400.0));
          let now = Unix.gettimeofday () in
          let items =
            find_stale ~db ~now ~stale_after_s:60.0 ~room_id:"C003" ()
          in
          Alcotest.(check int) "terminal excluded" 0 (List.length items)))

(** Task tree pending task becomes stale. *)
let test_tree_task_stale_pending () =
  with_db (fun db ->
      let origin = make_origin_json ~room_id:"C010" ~requester:"Bob" () in
      add_tree_task ~db ~session_key:"s1" ~id:"t1" ~title:"Design"
        ~origin_json:origin ~status:"pending" ();
      let one_hour_ago =
        Room_stale_query.unix_to_sqlite_datetime (Unix.gettimeofday () -. 3600.0)
      in
      set_tree_created_at ~db ~session_key:"s1" ~id:"t1" ~datetime:one_hour_ago;
      set_tree_updated_at ~db ~session_key:"s1" ~id:"t1" ~datetime:one_hour_ago;
      let now = Unix.gettimeofday () in
      let items =
        find_stale ~db ~now ~stale_after_s:1800.0 ~room_id:"C010" ()
      in
      Alcotest.(check int) "one stale tree" 1 (List.length items);
      let item = List.hd items in
      Alcotest.(check string)
        "source" "task_tree"
        (source_to_string item.source);
      Alcotest.(check string) "status" "pending" item.status;
      Alcotest.(check string) "id" "t1" item.id)

(** Task tree in_progress task becomes stale. *)
let test_tree_task_stale_in_progress () =
  with_db (fun db ->
      let origin = make_origin_json ~room_id:"C011" () in
      add_tree_task ~db ~session_key:"s1" ~id:"t2" ~title:"Implement"
        ~origin_json:origin ~status:"pending" ();
      update_tree_status ~db ~session_key:"s1" ~id:"t2" ~status:"in_progress";
      let two_hours_ago =
        Room_stale_query.unix_to_sqlite_datetime (Unix.gettimeofday () -. 7200.0)
      in
      set_tree_updated_at ~db ~session_key:"s1" ~id:"t2" ~datetime:two_hours_ago;
      let now = Unix.gettimeofday () in
      let items =
        find_stale ~db ~now ~stale_after_s:1800.0 ~room_id:"C011" ()
      in
      Alcotest.(check int) "one stale in_progress" 1 (List.length items);
      let item = List.hd items in
      Alcotest.(check string) "status" "in_progress" item.status)

(** Done/cancelled task_tree tasks are excluded. *)
let test_tree_task_terminal_excluded () =
  with_db (fun db ->
      let origin = make_origin_json ~room_id:"C012" () in
      add_tree_task ~db ~session_key:"s1" ~id:"t3" ~title:"Done task"
        ~origin_json:origin ~status:"done" ();
      set_tree_updated_at ~db ~session_key:"s1" ~id:"t3"
        ~datetime:
          (Room_stale_query.unix_to_sqlite_datetime
             (Unix.gettimeofday () -. 86400.0));
      let now = Unix.gettimeofday () in
      let items = find_stale ~db ~now ~stale_after_s:60.0 ~room_id:"C012" () in
      Alcotest.(check int) "terminal excluded" 0 (List.length items))

(** Room_id scoping: only items matching the room_id are returned. *)
let test_room_id_scoping () =
  with_temp_git_repo (fun repo_path ->
      with_db (fun db ->
          let origin_a = make_origin_json ~room_id:"room-A" () in
          let origin_b = make_origin_json ~room_id:"room-B" () in
          let id_a =
            enqueue_bg_task ~db ~repo_path ~prompt:"task in A"
              ~origin_json:origin_a ()
          in
          let _id_b =
            enqueue_bg_task ~db ~repo_path ~prompt:"task in B"
              ~origin_json:origin_b ()
          in
          let old =
            Room_stale_query.unix_to_sqlite_datetime
              (Unix.gettimeofday () -. 7200.0)
          in
          set_bg_created_at ~db ~id:id_a ~datetime:old;
          let now = Unix.gettimeofday () in
          let items_a =
            find_stale ~db ~now ~stale_after_s:60.0 ~room_id:"room-A" ()
          in
          Alcotest.(check int) "room-A only" 1 (List.length items_a);
          let items_b =
            find_stale ~db ~now ~stale_after_s:60.0 ~room_id:"room-B" ()
          in
          Alcotest.(check int) "room-B fresh" 0 (List.length items_b)))

(** Thread_id scoping: only items matching the thread are returned. *)
let test_thread_id_scoping () =
  with_temp_git_repo (fun repo_path ->
      with_db (fun db ->
          let origin_1 =
            make_origin_json ~room_id:"C020" ~thread_id:"thread-1" ()
          in
          let origin_2 =
            make_origin_json ~room_id:"C020" ~thread_id:"thread-2" ()
          in
          let id_1 =
            enqueue_bg_task ~db ~repo_path ~prompt:"in thread 1"
              ~origin_json:origin_1 ()
          in
          let _id_2 =
            enqueue_bg_task ~db ~repo_path ~prompt:"in thread 2"
              ~origin_json:origin_2 ()
          in
          let old =
            Room_stale_query.unix_to_sqlite_datetime
              (Unix.gettimeofday () -. 7200.0)
          in
          set_bg_created_at ~db ~id:id_1 ~datetime:old;
          let now = Unix.gettimeofday () in
          let items =
            find_stale ~db ~now ~stale_after_s:60.0 ~room_id:"C020"
              ~thread_id:"thread-1" ()
          in
          Alcotest.(check int) "thread-1 only" 1 (List.length items);
          let item = List.hd items in
          Alcotest.(check (option string))
            "thread_id" (Some "thread-1") item.thread_id))

(** Combined query: both bg tasks and tree tasks in same result. *)
let test_combined_query () =
  with_temp_git_repo (fun repo_path ->
      with_db (fun db ->
          let origin = make_origin_json ~room_id:"C030" () in
          let bg_id =
            enqueue_bg_task ~db ~repo_path ~prompt:"bg stale"
              ~origin_json:origin ()
          in
          let old =
            Room_stale_query.unix_to_sqlite_datetime
              (Unix.gettimeofday () -. 7200.0)
          in
          set_bg_created_at ~db ~id:bg_id ~datetime:old;
          add_tree_task ~db ~session_key:"s1" ~id:"tree1" ~title:"tree stale"
            ~origin_json:origin ~status:"pending" ();
          set_tree_created_at ~db ~session_key:"s1" ~id:"tree1" ~datetime:old;
          set_tree_updated_at ~db ~session_key:"s1" ~id:"tree1" ~datetime:old;
          let now = Unix.gettimeofday () in
          let items =
            find_stale ~db ~now ~stale_after_s:60.0 ~room_id:"C030" ()
          in
          Alcotest.(check int) "two stale items" 2 (List.length items);
          let sources =
            List.map (fun i -> source_to_string i.source) items
            |> List.sort String.compare
          in
          Alcotest.(check (list string))
            "both sources"
            [ "background_task"; "task_tree" ]
            sources))

(** No room_id filter returns all stale items across rooms. *)
let test_no_room_filter () =
  with_temp_git_repo (fun repo_path ->
      with_db (fun db ->
          let origin_a = make_origin_json ~room_id:"room-X" () in
          let origin_b = make_origin_json ~room_id:"room-Y" () in
          let id_a =
            enqueue_bg_task ~db ~repo_path ~prompt:"in X" ~origin_json:origin_a
              ()
          in
          let id_b =
            enqueue_bg_task ~db ~repo_path ~prompt:"in Y" ~origin_json:origin_b
              ()
          in
          let old =
            Room_stale_query.unix_to_sqlite_datetime
              (Unix.gettimeofday () -. 7200.0)
          in
          set_bg_created_at ~db ~id:id_a ~datetime:old;
          set_bg_created_at ~db ~id:id_b ~datetime:old;
          let now = Unix.gettimeofday () in
          (* No room_id filter *)
          let items = find_stale ~db ~now ~stale_after_s:60.0 () in
          Alcotest.(check int) "all rooms" 2 (List.length items)))

(** Items without origin_json (no room_id) are excluded when room_id filter is
    provided. *)
let test_no_origin_excluded_with_room_filter () =
  with_temp_git_repo (fun repo_path ->
      with_db (fun db ->
          (* Task without origin_json *)
          let id = enqueue_bg_task ~db ~repo_path ~prompt:"no origin" () in
          let old =
            Room_stale_query.unix_to_sqlite_datetime
              (Unix.gettimeofday () -. 7200.0)
          in
          set_bg_created_at ~db ~id ~datetime:old;
          let now = Unix.gettimeofday () in
          let items =
            find_stale ~db ~now ~stale_after_s:60.0 ~room_id:"any-room" ()
          in
          Alcotest.(check int) "no origin excluded" 0 (List.length items)))

(** Items without origin_json are included when NO room_id filter. *)
let test_no_origin_included_without_room_filter () =
  with_temp_git_repo (fun repo_path ->
      with_db (fun db ->
          let id = enqueue_bg_task ~db ~repo_path ~prompt:"no origin" () in
          let old =
            Room_stale_query.unix_to_sqlite_datetime
              (Unix.gettimeofday () -. 7200.0)
          in
          set_bg_created_at ~db ~id ~datetime:old;
          let now = Unix.gettimeofday () in
          let items = find_stale ~db ~now ~stale_after_s:60.0 () in
          Alcotest.(check int) "no filter includes" 1 (List.length items)))

(** Age_seconds is positive and approximately correct. *)
let test_age_seconds_reasonable () =
  with_temp_git_repo (fun repo_path ->
      with_db (fun db ->
          let origin = make_origin_json ~room_id:"C040" () in
          let id =
            enqueue_bg_task ~db ~repo_path ~prompt:"age check"
              ~origin_json:origin ()
          in
          let created_ts = Unix.gettimeofday () -. 5000.0 in
          set_bg_created_at ~db ~id
            ~datetime:(Room_stale_query.unix_to_sqlite_datetime created_ts);
          let now = created_ts +. 5000.0 in
          let items =
            find_stale ~db ~now ~stale_after_s:1.0 ~room_id:"C040" ()
          in
          Alcotest.(check int) "one item" 1 (List.length items);
          let item = List.hd items in
          Alcotest.(check bool)
            "age ~5000s" true
            (item.age_seconds >= 4999.0 && item.age_seconds <= 5001.0)))

(** Determinism: same inputs produce same output. *)
let test_deterministic () =
  with_temp_git_repo (fun repo_path ->
      with_db (fun db ->
          let origin = make_origin_json ~room_id:"C050" () in
          let id =
            enqueue_bg_task ~db ~repo_path ~prompt:"det check"
              ~origin_json:origin ()
          in
          let old =
            Room_stale_query.unix_to_sqlite_datetime
              (Unix.gettimeofday () -. 7200.0)
          in
          set_bg_created_at ~db ~id ~datetime:old;
          let now = 99999.0 in
          let run1 =
            find_stale ~db ~now ~stale_after_s:60.0 ~room_id:"C050" ()
          in
          let run2 =
            find_stale ~db ~now ~stale_after_s:60.0 ~room_id:"C050" ()
          in
          Alcotest.(check int)
            "same length" (List.length run1) (List.length run2);
          List.iter2
            (fun a b ->
              Alcotest.(check string) "same id" a.id b.id;
              Alcotest.(check string)
                "same source"
                (source_to_string a.source)
                (source_to_string b.source);
              Alcotest.(check (float 0.001))
                "same age" a.age_seconds b.age_seconds)
            run1 run2))

(** Stale threshold boundary: item exactly at threshold IS stale. *)
let test_threshold_boundary_at () =
  with_temp_git_repo (fun repo_path ->
      with_db (fun db ->
          let origin = make_origin_json ~room_id:"C060" () in
          let id =
            enqueue_bg_task ~db ~repo_path ~prompt:"boundary"
              ~origin_json:origin ()
          in
          (* Set created_at to exactly 100s before now *)
          let now = 10000.0 in
          set_bg_created_at ~db ~id
            ~datetime:(Room_stale_query.unix_to_sqlite_datetime (now -. 100.0));
          let items =
            find_stale ~db ~now ~stale_after_s:100.0 ~room_id:"C060" ()
          in
          Alcotest.(check int) "at threshold" 1 (List.length items)))

(** Stale threshold boundary: item 1s before threshold is NOT stale. *)
let test_threshold_boundary_below () =
  with_temp_git_repo (fun repo_path ->
      with_db (fun db ->
          let origin = make_origin_json ~room_id:"C061" () in
          let id =
            enqueue_bg_task ~db ~repo_path ~prompt:"below boundary"
              ~origin_json:origin ()
          in
          let now = 10000.0 in
          set_bg_created_at ~db ~id
            ~datetime:(Room_stale_query.unix_to_sqlite_datetime (now -. 99.0));
          let items =
            find_stale ~db ~now ~stale_after_s:100.0 ~room_id:"C061" ()
          in
          Alcotest.(check int) "below threshold" 0 (List.length items)))

let suite =
  [
    Alcotest.test_case "empty db returns nothing" `Quick test_empty_db;
    Alcotest.test_case "fresh bg task not stale" `Quick test_bg_task_not_stale;
    Alcotest.test_case "old queued bg task is stale" `Quick
      test_bg_task_stale_queued;
    Alcotest.test_case "old running bg task is stale" `Quick
      test_bg_task_stale_running;
    Alcotest.test_case "terminal bg tasks excluded" `Quick
      test_bg_task_terminal_excluded;
    Alcotest.test_case "stale pending tree task" `Quick
      test_tree_task_stale_pending;
    Alcotest.test_case "stale in_progress tree task" `Quick
      test_tree_task_stale_in_progress;
    Alcotest.test_case "terminal tree tasks excluded" `Quick
      test_tree_task_terminal_excluded;
    Alcotest.test_case "room_id scoping" `Quick test_room_id_scoping;
    Alcotest.test_case "thread_id scoping" `Quick test_thread_id_scoping;
    Alcotest.test_case "combined bg + tree query" `Quick test_combined_query;
    Alcotest.test_case "no room filter returns all" `Quick test_no_room_filter;
    Alcotest.test_case "no origin excluded with room filter" `Quick
      test_no_origin_excluded_with_room_filter;
    Alcotest.test_case "no origin included without filter" `Quick
      test_no_origin_included_without_room_filter;
    Alcotest.test_case "age_seconds reasonable" `Quick
      test_age_seconds_reasonable;
    Alcotest.test_case "deterministic output" `Quick test_deterministic;
    Alcotest.test_case "at threshold is stale" `Quick test_threshold_boundary_at;
    Alcotest.test_case "below threshold not stale" `Quick
      test_threshold_boundary_below;
  ]
