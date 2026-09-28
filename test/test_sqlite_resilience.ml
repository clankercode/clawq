(* B824: SQLite locking resilience — WAL journal mode, busy_timeout coverage,
   and BUSY retry-with-backoff on schema-init and hot append paths. *)

let pragma_text db sql =
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.TEXT s -> Some s
          | _ -> None)
      | _ -> None)

let pragma_int db sql =
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.INT n -> Int64.to_int n
          | Sqlite3.Data.TEXT s -> int_of_string s
          | _ -> 0)
      | _ -> 0)

let mode_of db =
  match pragma_text db "PRAGMA journal_mode" with Some m -> m | None -> ""

let test_memory_init_enables_wal_and_busy_timeout () =
  Test_helpers.with_temp_dir (fun dir ->
      let db_path = Filename.concat dir "memory.db" in
      let db = Memory.init ~db_path () in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close db))
        (fun () ->
          Alcotest.(check string) "journal mode is wal" "wal" (mode_of db);
          Alcotest.(check int)
            "busy timeout set by central init" 5000
            (pragma_int db "PRAGMA busy_timeout");
          (* WAL is a persistent property of the file, so a later plain
             connection inherits it instead of falling back to rollback
             journal mode. *)
          let later = Sqlite3.db_open db_path in
          Fun.protect
            ~finally:(fun () -> ignore (Sqlite3.db_close later))
            (fun () ->
              Alcotest.(check string) "wal inherited" "wal" (mode_of later))))

let test_open_db_sets_busy_timeout () =
  Test_helpers.with_temp_dir (fun dir ->
      let db = Sql_util.open_db (Filename.concat dir "plain.db") in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close db))
        (fun () ->
          Alcotest.(check int)
            "shared busy timeout" 5000
            (pragma_int db "PRAGMA busy_timeout"));
      let tuned = Sql_util.open_db ~ms:250 (Filename.concat dir "tuned.db") in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close tuned))
        (fun () ->
          Alcotest.(check int)
            "explicit override" 250
            (pragma_int tuned "PRAGMA busy_timeout")))

(* --- retry-with-backoff ------------------------------------------------- *)

let test_is_busy_error_classifies_lock_failures () =
  let busy =
    [
      Failure "SQLite error: BUSY (sql: CREATE TABLE IF NOT EXISTS t)";
      Failure "room_activity_ledger append failed: BUSY";
      Failure "SQLite error: LOCKED (sql: DELETE FROM t)";
      Sqlite3.Error "database is locked";
    ]
  in
  List.iteri
    (fun i exn ->
      Alcotest.(check bool)
        (Printf.sprintf "busy case %d" i)
        true
        (Sql_util.is_busy_error exn))
    busy;
  Alcotest.(check bool)
    "schema error is not busy" false
    (Sql_util.is_busy_error (Failure "SQLite error: no such table: x"));
  Alcotest.(check bool)
    "unrelated exception" false
    (Sql_util.is_busy_error Not_found)

let test_retry_busy_backs_off_then_succeeds () =
  let calls = ref 0 and sleeps = ref [] in
  let result =
    Sql_util.retry_busy ~attempts:5 ~base_delay_s:0.01
      ~sleep:(fun d -> sleeps := d :: !sleeps)
      (fun () ->
        incr calls;
        if !calls < 3 then
          failwith "SQLite error: BUSY (sql: CREATE TABLE IF NOT EXISTS t)";
        "ok")
  in
  Alcotest.(check string) "result" "ok" result;
  Alcotest.(check int) "attempts used" 3 !calls;
  Alcotest.(check (list (float 1e-9)))
    "exponential backoff" [ 0.01; 0.02 ] (List.rev !sleeps)

let test_retry_busy_caps_delay () =
  let sleeps = ref [] in
  (try
     ignore
       (Sql_util.retry_busy ~attempts:6 ~base_delay_s:0.2 ~max_delay_s:0.25
          ~sleep:(fun d -> sleeps := d :: !sleeps)
          (fun () -> failwith "BUSY"))
   with Failure _ -> ());
  Alcotest.(check (list (float 1e-9)))
    "delay capped at max_delay_s"
    [ 0.2; 0.25; 0.25; 0.25; 0.25 ]
    (List.rev !sleeps)

let test_retry_busy_gives_up_after_attempts () =
  let calls = ref 0 in
  let message =
    match
      Sql_util.retry_busy ~attempts:3
        ~sleep:(fun _ -> ())
        (fun () ->
          incr calls;
          failwith "github_action_reconcile schema error: BUSY (sql: x)")
    with
    | result -> Alcotest.failf "expected Failure, got %s" result
    | exception Failure msg -> msg
  in
  Alcotest.(check int) "attempts executed" 3 !calls;
  Alcotest.(check bool)
    "last error surfaces" true
    (Test_helpers.string_contains message "BUSY")

let test_retry_busy_propagates_non_busy_errors () =
  let calls = ref 0 and raised = ref false in
  (try
     ignore
       (Sql_util.retry_busy ~attempts:5
          ~sleep:(fun _ -> ())
          (fun () ->
            incr calls;
            failwith "SQLite error: no such table: x"))
   with Failure msg ->
     raised := true;
     Alcotest.(check string)
       "message preserved" "SQLite error: no such table: x" msg);
  Alcotest.(check bool) "raised" true !raised;
  Alcotest.(check int) "not retried" 1 !calls

(* --- real cross-connection contention ------------------------------------

   A forked child holds a write transaction on the database file and commits
   shortly after, which is the smallest deterministic stand-in for the daemon +
   CLI + EC process collisions in the ticket. The parent's connection runs with
   a deliberately short busy timeout so the transient BUSY is really hit and the
   retry path has to absorb it. *)

let with_contending_writer ~db_path ~hold_s f =
  let held_read, held_write = Unix.pipe () in
  let pid = Unix.fork () in
  if pid = 0 then begin
    Unix.close held_read;
    (try
       let db = Sqlite3.db_open db_path in
       ignore (Sqlite3.busy_timeout db 200);
       ignore (Sqlite3.exec db "BEGIN IMMEDIATE");
       ignore (Sqlite3.exec db "INSERT INTO contention_probe (id) VALUES (1)");
       ignore (Unix.write held_write (Bytes.of_string "held\n") 0 5);
       Unix.sleepf hold_s;
       ignore (Sqlite3.exec db "COMMIT");
       ignore (Sqlite3.db_close db)
     with _ -> ());
    Unix._exit 0
  end
  else begin
    Unix.close held_write;
    let buf = Bytes.create 5 in
    let rec drain n =
      if n < 5 then
        match Unix.read held_read buf n (5 - n) with
        | 0 -> () (* child died before locking; run anyway *)
        | read -> drain (n + read)
    in
    drain 0;
    Unix.close held_read;
    Fun.protect
      ~finally:(fun () -> ignore (Unix.waitpid [] pid))
      (fun () -> f ())
  end

(* Seed a WAL database and close every connection before forking: no open
   handle may cross the fork. *)
let seed_wal_db dir =
  let db_path = Filename.concat dir "contention.db" in
  let db = Memory.init ~db_path () in
  Sql_util.exec_exn db
    "CREATE TABLE IF NOT EXISTS contention_probe (id INTEGER PRIMARY KEY)";
  ignore (Sqlite3.db_close db);
  db_path

let test_ledger_schema_and_append_survive_contention () =
  Test_helpers.with_temp_dir (fun dir ->
      let db_path = seed_wal_db dir in
      with_contending_writer ~db_path ~hold_s:0.12 (fun () ->
          let db = Sql_util.open_db ~ms:25 db_path in
          Fun.protect
            ~finally:(fun () -> ignore (Sqlite3.db_close db))
            (fun () ->
              Room_activity_ledger.init_schema db;
              let event =
                Room_activity_ledger.append ~db ~room_id:"room-1"
                  ~event_type:"delivery_attempt"
                  ~timestamp:"2026-09-28T00:00:00Z" ~actor:"teams"
                  ~metadata:(`Assoc [])
              in
              Alcotest.(check string)
                "append returned stored event" "room-1" event.room_id;
              Alcotest.(check int)
                "ledger row visible" 1
                (List.length
                   (Room_activity_ledger.query ~db ~room_id:"room-1" ()));
              Alcotest.(check int)
                "contending writer committed" 1
                (pragma_int db "SELECT COUNT(*) FROM contention_probe"))))

let test_schema_init_paths_survive_contention () =
  Test_helpers.with_temp_dir (fun dir ->
      let db_path = seed_wal_db dir in
      with_contending_writer ~db_path ~hold_s:0.12 (fun () ->
          let db = Sql_util.open_db ~ms:25 db_path in
          Fun.protect
            ~finally:(fun () -> ignore (Sqlite3.db_close db))
            (fun () ->
              Github_action_reconcile.ensure_schema db;
              Ec_diagnosis.init_ec_reports_schema db;
              Background_task_db.init_schema db;
              (* Modules that never open their own connection (they receive the
                 shared handle) must still write through it under contention. *)
              (match
                 Background_task_db.queue_message ~db ~task_id:1 ~message:"ping"
               with
              | Ok id ->
                  Alcotest.(check bool) "task message queued" true (id > 0)
              | Error err -> Alcotest.failf "queue_message failed: %s" err);
              Agent_provider_ledger.record_provider_error_event ~db
                ~session_key:"slack:room-2:user-1" ~provider:"openai"
                ~model:"gpt" (Failure "boom");
              Github_notify_batch.enqueue ~db ~room_id:"room-1"
                ~body:"pr updated";
              Alcotest.(check int)
                "notify queue row stored" 1
                (Github_notify_batch.count ~db);
              Alcotest.(check int)
                "provider ledger row stored" 1
                (List.length
                   (Room_activity_ledger.query ~db ~room_id:"room-2"
                      ~event_type:"provider_error" ()));
              Alcotest.(check int)
                "contending writer committed" 1
                (pragma_int db "SELECT COUNT(*) FROM contention_probe"))))

(* --- repeated identical BUSY log lines ---------------------------------- *)

(* --- WAL switch is best effort ------------------------------------------- *)

let test_enable_wal_accepts_memory_answer_without_retrying () =
  (* In-memory databases cannot leave "memory" mode; sqlite's answer is final,
     so no retry (and no sleep) may be spent on it. *)
  let db = Sqlite3.db_open ":memory:" in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      let sleeps = ref 0 in
      Alcotest.(check string)
        "memory db keeps memory mode" "memory"
        (Sql_util.enable_wal ~sleep:(fun _ -> incr sleeps) db);
      Alcotest.(check int) "no retries on a final answer" 0 !sleeps)

let test_enable_wal_degrades_when_the_file_is_locked () =
  Test_helpers.with_temp_dir (fun dir ->
      let db_path = Filename.concat dir "locked.db" in
      let a = Sqlite3.db_open db_path in
      ignore (Sqlite3.busy_timeout a 100);
      ignore (Sqlite3.exec a "CREATE TABLE t (id INTEGER)");
      (* Open read transaction on A: a rollback-journal file cannot be
         converted while another connection is reading it. *)
      ignore (Sqlite3.exec a "BEGIN");
      ignore (Sqlite3.exec a "SELECT COUNT(*) FROM t");
      let b = Sql_util.open_db db_path in
      (* Short timeout so each BUSY probe returns immediately instead of
         blocking for the production 5s inside a unit test. *)
      Sql_util.set_busy_timeout ~ms:20 b;
      Fun.protect
        ~finally:(fun () ->
          ignore (Sqlite3.db_close b);
          ignore (Sqlite3.exec a "ROLLBACK");
          ignore (Sqlite3.db_close a))
        (fun () ->
          let sleeps = ref 0 in
          Alcotest.(check string)
            "stays in rollback mode while a reader holds the file" "delete"
            (Sql_util.enable_wal ~sleep:(fun _ -> incr sleeps) b);
          Alcotest.(check int) "retried the locked probe" 2 !sleeps;
          ignore (Sqlite3.exec a "ROLLBACK");
          Alcotest.(check string)
            "converts once the reader is gone" "wal"
            (Sql_util.enable_wal ~sleep:(fun _ -> incr sleeps) b)))

let test_log_dedup_emits_once_per_window () =
  let t = Log_dedup.create ~window_s:60. () in
  Alcotest.(check (option string))
    "first line emits" (Some "boom: BUSY")
    (Log_dedup.emit t ~now:1000. ~key:"busy" "boom: BUSY");
  Alcotest.(check (option string))
    "duplicate suppressed" None
    (Log_dedup.emit t ~now:1001. ~key:"busy" "boom: BUSY (other delivery)");
  Alcotest.(check (option string))
    "still inside window" None
    (Log_dedup.emit t ~now:1059. ~key:"busy" "boom: BUSY (third delivery)");
  Alcotest.(check (option string))
    "after window reports suppressed count"
    (Some "boom: BUSY (suppressed 2 in 60s)")
    (Log_dedup.emit t ~now:1061. ~key:"busy" "boom: BUSY");
  (* distinct keys are independent: a new error class is not swallowed *)
  Alcotest.(check (option string))
    "other key emits" (Some "other: LOCKED")
    (Log_dedup.emit t ~now:1001. ~key:"locked" "other: LOCKED")

(* --- notify flush loop survives database errors -------------------------- *)

let test_notify_flush_tick_survives_db_error () =
  (* No github_notify_queue table: the tick's first database call raises, and
     the tick must return normally instead of killing the async flush loop
     (the uncaught BUSY observed in daemon.log at 10:09). *)
  let mgr = Session.create ~config:Runtime_config.default () in
  let db = Sqlite3.db_open ":memory:" in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      let last_sent = Hashtbl.create 4 in
      Lwt_main.run
        (Github_notify_flush.flush_tick ~db ~session_manager:mgr
           ~config:Runtime_config.default ~last_sent ());
      Alcotest.(check bool) "tick returned normally" true true)

let suite =
  [
    Alcotest.test_case "memory init enables wal and busy timeout" `Quick
      test_memory_init_enables_wal_and_busy_timeout;
    Alcotest.test_case "open_db applies shared busy timeout" `Quick
      test_open_db_sets_busy_timeout;
    Alcotest.test_case "is_busy_error classifies lock failures" `Quick
      test_is_busy_error_classifies_lock_failures;
    Alcotest.test_case "retry_busy backs off then succeeds" `Quick
      test_retry_busy_backs_off_then_succeeds;
    Alcotest.test_case "retry_busy caps delay" `Quick test_retry_busy_caps_delay;
    Alcotest.test_case "retry_busy gives up after attempts" `Quick
      test_retry_busy_gives_up_after_attempts;
    Alcotest.test_case "retry_busy propagates non busy errors" `Quick
      test_retry_busy_propagates_non_busy_errors;
    Alcotest.test_case "ledger schema and append survive contention" `Quick
      test_ledger_schema_and_append_survive_contention;
    Alcotest.test_case "schema init and shared writes survive contention" `Quick
      test_schema_init_paths_survive_contention;
    Alcotest.test_case "enable_wal accepts memory answer without retrying"
      `Quick test_enable_wal_accepts_memory_answer_without_retrying;
    Alcotest.test_case "enable_wal degrades when the file is locked" `Quick
      test_enable_wal_degrades_when_the_file_is_locked;
    Alcotest.test_case "log dedup emits once per window" `Quick
      test_log_dedup_emits_once_per_window;
    Alcotest.test_case "notify flush tick survives db error" `Quick
      test_notify_flush_tick_survives_db_error;
  ]
