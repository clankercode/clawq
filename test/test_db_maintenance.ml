(* test_db_maintenance.ml — B830 retention sweeps + nightly VACUUM *)

let now = 1_767_225_600.0 (* 2026-01-01T00:00:00Z, injected clock *)
let day = 86_400.0
let days_ago d = now -. (float_of_int d *. day)
let iso t = Time_util.iso8601_utc ~t ()
let iso_micros t = Time_util.iso8601_utc_micros ~t ()
let sql_dt t = Time_util.sql_datetime_utc ~t ()

let exec db sql params =
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      List.iteri
        (fun i p -> ignore (Sqlite3.bind stmt (i + 1) p : Sqlite3.Rc.t))
        params;
      match Sqlite3.step stmt with
      (* PRAGMA statements step ROW before DONE; both mean success here. *)
      | Sqlite3.Rc.DONE | Sqlite3.Rc.ROW -> ()
      | rc ->
          Alcotest.failf "statement failed: %s (%s)" sql
            (Sqlite3.Rc.to_string rc))

let count_db db table =
  let stmt =
    Sqlite3.prepare db (Printf.sprintf "SELECT COUNT(*) FROM %s" table)
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.INT n -> Int64.to_int n
          | _ -> 0)
      | _ -> 0)

let retention_config ?(journal = 7) ?(deliveries = 7) ?(stats = 30)
    ?(egress = 7) ?(correlations = 7) ?(outbox = 7) ?(ledger = 30)
    ?(vacuum_enabled = true) ?(vacuum_min_mb = 200) () : Runtime_config.t =
  {
    Runtime_config.default with
    db_retention =
      {
        github_room_event_journal_days = journal;
        github_app_webhook_deliveries_days = deliveries;
        request_stats_days = stats;
        egress_audit_days = egress;
        github_action_correlations_days = correlations;
        github_delivery_outbox_days = outbox;
        room_activity_ledger_days = ledger;
        vacuum_enabled;
        vacuum_min_mb;
      };
  }

let disabled_config =
  retention_config ~journal:0 ~deliveries:0 ~stats:0 ~egress:0 ~correlations:0
    ~outbox:0 ~ledger:0 ~vacuum_enabled:false ()

(* ── per-table purge fixtures ──────────────────────────────────────────── *)

let insert_journal_row db id created_at =
  Github_room_event_journal.ensure_schema db;
  exec db
    {|INSERT INTO github_room_event_journal
      (id, room_id, delivery_id, item_key, envelope_json, route_id, created_at)
      VALUES (?, 'room-1', NULL, 'item', '{}', NULL, ?)|}
    [ Sqlite3.Data.TEXT id; Sqlite3.Data.TEXT created_at ]

let insert_delivery_row db delivery_id received_at =
  Github_app_webhook_ingress.ensure_schema db;
  exec db
    {|INSERT INTO github_app_webhook_deliveries
      (delivery_id, received_at, event, status)
      VALUES (?, ?, 'push', 'accepted')|}
    [ Sqlite3.Data.TEXT delivery_id; Sqlite3.Data.TEXT received_at ]

let insert_stats_row db requested_at =
  exec db
    {|INSERT INTO request_stats
      (session_key, provider, model, prompt_tokens, completion_tokens,
       requested_at)
      VALUES ('s1', 'p', 'm', 1, 1, ?)|}
    [ Sqlite3.Data.TEXT requested_at ]

let insert_egress_row db id timestamp =
  exec db
    {|INSERT INTO egress_audit
      (id, timestamp, decision, host_redacted, matched_rule_index)
      VALUES (?, ?, 'allowed', 'a**.com', 0)|}
    [ Sqlite3.Data.INT (Int64.of_int id); Sqlite3.Data.TEXT timestamp ]

let insert_correlation_row db id status created_at closed_at =
  Github_action_reconcile.ensure_schema db;
  exec db
    {|INSERT INTO github_action_correlations
      (id, room_id, action, actor_mode, status, created_at, closed_at)
      VALUES (?, 'room-1', 'comment', 'user', ?, ?, ?)|}
    [
      Sqlite3.Data.TEXT id;
      Sqlite3.Data.TEXT status;
      Sqlite3.Data.TEXT created_at;
      (match closed_at with
      | Some ts -> Sqlite3.Data.TEXT ts
      | None -> Sqlite3.Data.NULL);
    ]

let insert_outbox_row db id status created_at =
  Github_delivery_outbox.ensure_schema db;
  exec db
    {|INSERT INTO github_delivery_outbox
      (id, room_id, item_key, intent_json, status, next_attempt_at, created_at)
      VALUES (?, 'room-1', 'item', '{}', ?, ?, ?)|}
    [
      Sqlite3.Data.TEXT id;
      Sqlite3.Data.TEXT status;
      Sqlite3.Data.TEXT created_at;
      Sqlite3.Data.TEXT created_at;
    ]

let insert_ledger_row db timestamp =
  Room_activity_ledger.init_schema db;
  ignore
    (Room_activity_ledger.append ~db ~room_id:"room-1" ~event_type:"note"
       ~timestamp ~actor:"agent" ~metadata:`Null)

(* ── per-table purge tests ─────────────────────────────────────────────── *)

let test_journal_purge_keeps_newer_rows () =
  Test_helpers.with_memory_store (fun db ->
      insert_journal_row db "old" (iso (days_ago 8));
      insert_journal_row db "new" (iso (days_ago 6));
      let deleted =
        Test_helpers.assert_ok
          (Github_room_event_journal.purge_older_than ~db ~days:7 ~now ())
      in
      Alcotest.(check int) "old rows deleted" 1 deleted;
      Alcotest.(check int)
        "newer rows kept" 1
        (count_db db "github_room_event_journal");
      let kept =
        Test_helpers.assert_ok
          (Github_room_event_journal.list_for_room ~db ~room_id:"room-1" ())
      in
      Alcotest.(check string)
        "kept row is the newer one" "new" (List.hd kept).id)

let test_journal_purge_disabled_is_noop () =
  Test_helpers.with_memory_store (fun db ->
      insert_journal_row db "old" (iso (days_ago 400));
      let deleted =
        Test_helpers.assert_ok
          (Github_room_event_journal.purge_older_than ~db ~days:0 ~now ())
      in
      Alcotest.(check int) "days=0 deletes nothing" 0 deleted;
      Alcotest.(check int)
        "row still present" 1
        (count_db db "github_room_event_journal"))

let test_deliveries_purge_keeps_newer_rows () =
  Test_helpers.with_memory_store (fun db ->
      insert_delivery_row db "d-old" (iso (days_ago 8));
      insert_delivery_row db "d-new" (iso (days_ago 6));
      let deleted =
        Test_helpers.assert_ok
          (Github_app_webhook_ingress.purge_older_than ~db ~days:7 ~now ())
      in
      Alcotest.(check int) "old deliveries deleted" 1 deleted;
      Alcotest.(check int)
        "newer deliveries kept" 1
        (count_db db "github_app_webhook_deliveries");
      Alcotest.(check bool)
        "newer delivery still deduped" true
        (Github_app_webhook_ingress.was_seen ~db ~delivery_id:"d-new");
      Alcotest.(check bool)
        "old delivery no longer deduped" false
        (Github_app_webhook_ingress.was_seen ~db ~delivery_id:"d-old"))

let test_request_stats_purge_keeps_newer_rows () =
  Test_helpers.with_memory_store (fun db ->
      (* requested_at uses SQL datetime format, not ISO-8601 with T/Z. *)
      insert_stats_row db (sql_dt (days_ago 31));
      insert_stats_row db (sql_dt (days_ago 29));
      let deleted =
        Test_helpers.assert_ok
          (Request_stats.purge_older_than ~db ~days:30 ~now ())
      in
      Alcotest.(check int) "old stats deleted" 1 deleted;
      Alcotest.(check int) "newer stats kept" 1 (count_db db "request_stats"))

let test_egress_audit_purge_keeps_newer_rows () =
  Test_helpers.with_memory_store (fun db ->
      Egress_audit.init_schema db;
      insert_egress_row db 1 (iso_micros (days_ago 8));
      insert_egress_row db 2 (iso_micros (days_ago 6));
      Alcotest.(check int)
        "old events deleted" 1
        (Egress_audit.purge_older_than ~db ~days:7 ~now ());
      Alcotest.(check int) "newer events kept" 1 (count_db db "egress_audit");
      Alcotest.(check int)
        "days=0 is a no-op" 0
        (Egress_audit.purge_older_than ~db ~days:0 ~now ());
      Alcotest.(check int)
        "still one row after disabled tick" 1
        (count_db db "egress_audit"))

let test_correlations_purge_closed_only () =
  Test_helpers.with_memory_store (fun db ->
      insert_correlation_row db "closed-old" "closed"
        (iso (days_ago 40))
        (Some (iso (days_ago 8)));
      insert_correlation_row db "closed-new" "closed"
        (iso (days_ago 8))
        (Some (iso (days_ago 6)));
      insert_correlation_row db "open-old" "open" (iso (days_ago 40)) None;
      let deleted =
        Test_helpers.assert_ok
          (Github_action_reconcile.purge_closed_older_than ~db ~days:7 ~now ())
      in
      Alcotest.(check int) "only closed+old deleted" 1 deleted;
      Alcotest.(check int)
        "closed-new and open-old kept" 2
        (count_db db "github_action_correlations"))

let test_outbox_purge_terminal_only () =
  Test_helpers.with_memory_store (fun db ->
      insert_outbox_row db "dead-old" "dead_letter" (iso (days_ago 8));
      insert_outbox_row db "sup-old" "superseded" (iso (days_ago 8));
      insert_outbox_row db "dead-new" "dead_letter" (iso (days_ago 6));
      insert_outbox_row db "pending-old" "pending" (iso (days_ago 40));
      insert_outbox_row db "ok-old" "succeeded" (iso (days_ago 40));
      let deleted =
        Test_helpers.assert_ok
          (Github_delivery_outbox.purge_terminal_older_than ~db ~days:7 ~now ())
      in
      Alcotest.(check int) "terminal+old deleted" 2 deleted;
      Alcotest.(check int)
        "open and succeeded kept" 3
        (count_db db "github_delivery_outbox"))

let test_ledger_purge_keeps_newer_rows () =
  Test_helpers.with_memory_store (fun db ->
      Room_activity_ledger.init_schema db;
      insert_ledger_row db (iso_micros (days_ago 31));
      insert_ledger_row db (iso_micros (days_ago 29));
      Alcotest.(check int)
        "old ledger events deleted" 1
        (Room_activity_ledger.purge_older_than ~db ~days:30 ~now ());
      Alcotest.(check int)
        "newer ledger events kept" 1
        (count_db db "room_activity_ledger"))

(* ── retention_tick orchestration ──────────────────────────────────────── *)

let seed_all_tables db =
  insert_journal_row db "j-old" (iso (days_ago 8));
  insert_delivery_row db "d-old" (iso (days_ago 8));
  insert_stats_row db (sql_dt (days_ago 31));
  Egress_audit.init_schema db;
  insert_egress_row db 1 (iso_micros (days_ago 8));
  insert_correlation_row db "c-old" "closed"
    (iso (days_ago 40))
    (Some (iso (days_ago 8)));
  insert_outbox_row db "o-old" "dead_letter" (iso (days_ago 8));
  insert_ledger_row db (iso_micros (days_ago 31))

let tables =
  [
    "github_room_event_journal";
    "github_app_webhook_deliveries";
    "request_stats";
    "egress_audit";
    "github_action_correlations";
    "github_delivery_outbox";
    "room_activity_ledger";
  ]

let test_retention_tick_purges_every_table () =
  Test_helpers.with_memory_store (fun db ->
      seed_all_tables db;
      Db_maintenance.retention_tick ~db ~config:(retention_config ()) ~now ();
      List.iter
        (fun table ->
          Alcotest.(check int)
            (table ^ " emptied of expired rows")
            0 (count_db db table))
        tables)

let test_retention_tick_disabled_config_purges_nothing () =
  Test_helpers.with_memory_store (fun db ->
      seed_all_tables db;
      Db_maintenance.retention_tick ~db ~config:disabled_config ~now ();
      List.iter
        (fun table ->
          Alcotest.(check int)
            (table ^ " untouched when days=0")
            1 (count_db db table))
        tables)

let test_retention_tick_respects_per_table_windows () =
  Test_helpers.with_memory_store (fun db ->
      (* Everything seeded is 8/31 days old; a 9/32-day config keeps it all. *)
      seed_all_tables db;
      let config =
        retention_config ~journal:9 ~deliveries:9 ~stats:32 ~egress:9
          ~correlations:9 ~outbox:9 ~ledger:32 ()
      in
      Db_maintenance.retention_tick ~db ~config ~now ();
      List.iter
        (fun table ->
          Alcotest.(check int)
            (table ^ " kept inside wider window")
            1 (count_db db table))
        tables)

(* ── VACUUM gating ─────────────────────────────────────────────────────── *)

let check_vacuum label expected actual =
  let outcome_repr = function
    | Db_maintenance.Vacuum_disabled -> "disabled"
    | Vacuum_skipped_no_file -> "no_file"
    | Vacuum_skipped_small _ -> "small"
    | Vacuum_skipped_window _ -> "window"
    | Vacuum_skipped_recent -> "recent"
    | Vacuum_failed e -> "failed: " ^ e
    | Vacuumed _ -> "vacuumed"
  in
  Alcotest.(check string) label expected (outcome_repr actual)

let hour_utc h _ = { Unix.(gmtime 0.0) with tm_hour = h }

let with_file_db f =
  Test_helpers.with_temp_dir (fun dir ->
      let db_path = Filename.concat dir "memory.db" in
      let db = Memory.init ~db_path () in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close db))
        (fun () -> f db db_path))

let noon_utc _ = { Unix.(gmtime 0.0) with tm_hour = 12 }

(* Size threshold gating against the real (small) temp db file. *)
let test_maybe_vacuum_size_threshold () =
  with_file_db (fun db _ ->
      let state = Db_maintenance.create_vacuum_state () in
      check_vacuum "file under threshold skipped" "small"
        (Db_maintenance.maybe_vacuum ~db
           ~config:(retention_config ~vacuum_min_mb:1024 ())
           ~state ~now ~window:(0, 24) ());
      Alcotest.(check bool) "no run recorded" true (state.last_vacuum_at = 0.0);
      check_vacuum "file at/over threshold vacuums" "vacuumed"
        (Db_maintenance.maybe_vacuum ~db
           ~config:(retention_config ~vacuum_min_mb:0 ())
           ~state ~now ~window:(0, 24) ()))

let test_maybe_vacuum_window_and_interval_gates () =
  with_file_db (fun db _ ->
      let window = (2, 5) in
      let at_hour ~hour ~state =
        Db_maintenance.maybe_vacuum ~db
          ~config:(retention_config ~vacuum_min_mb:0 ())
          ~state ~now ~window ~localtime:(hour_utc hour) ()
      in
      let fresh () = Db_maintenance.create_vacuum_state () in
      check_vacuum "hour 2 (lower bound) included" "vacuumed"
        (at_hour ~hour:2 ~state:(fresh ()));
      check_vacuum "hour 4 inside window" "vacuumed"
        (at_hour ~hour:4 ~state:(fresh ()));
      check_vacuum "hour 5 (upper bound) excluded" "window"
        (at_hour ~hour:5 ~state:(fresh ()));
      check_vacuum "midday outside window" "window"
        (at_hour ~hour:12 ~state:(fresh ()));
      (* Windows may wrap past midnight, e.g. 22:00-05:00. *)
      let wrapped = (22, 5) in
      let wrapped_at ~hour ~state =
        Db_maintenance.maybe_vacuum ~db
          ~config:(retention_config ~vacuum_min_mb:0 ())
          ~state ~now ~window:wrapped ~localtime:(hour_utc hour) ()
      in
      check_vacuum "wrapped window late hour" "vacuumed"
        (wrapped_at ~hour:23 ~state:(fresh ()));
      check_vacuum "wrapped window midday hour" "window"
        (wrapped_at ~hour:12 ~state:(fresh ()));
      (* At most one run per 24h, measured from the last successful run. *)
      let state =
        Db_maintenance.create_vacuum_state ~last_vacuum_at:(now -. 3600.0) ()
      in
      check_vacuum "run 1h ago skips" "recent" (at_hour ~hour:3 ~state);
      let state_due =
        Db_maintenance.create_vacuum_state ~last_vacuum_at:(now -. 86_400.0) ()
      in
      check_vacuum "run exactly 24h ago is due" "vacuumed"
        (at_hour ~hour:3 ~state:state_due))

let test_maybe_vacuum_runs_and_gates () =
  with_file_db (fun db db_path ->
      let state = Db_maintenance.create_vacuum_state () in
      let cfg ?(vacuum_min_mb = 0) ?(vacuum_enabled = true) () =
        retention_config ~vacuum_enabled ~vacuum_min_mb ()
      in
      let call ?(now_v = now) ?(config = cfg ()) ?(window = (0, 24)) ?localtime
          () =
        Db_maintenance.maybe_vacuum ~db ~config ~state ~now:now_v ~window
          ?localtime ()
      in
      check_vacuum "disabled by config" "disabled"
        (call ~config:(cfg ~vacuum_enabled:false ()) ());
      check_vacuum "below size threshold" "small"
        (call ~config:(cfg ~vacuum_min_mb:1024 ()) ());
      check_vacuum "outside window" "window"
        (call ~window:(2, 5) ~localtime:noon_utc ());
      check_vacuum "first run vacuums" "vacuumed" (call ());
      Alcotest.(check bool)
        "state records the run" true
        (state.last_vacuum_at = now);
      check_vacuum "second run within 24h skipped" "recent"
        (call ~now_v:(now +. 3600.0) ());
      check_vacuum "run after 24h vacuums again" "vacuumed"
        (call ~now_v:(now +. 86_400.0 +. 60.0) ());
      Alcotest.(check bool)
        "state updated to second run" true
        (state.last_vacuum_at = now +. 86_400.0 +. 60.0);
      (* The db stays usable after a real VACUUM. *)
      Memory.store_message ~db ~session_key:"s1"
        (Provider.make_message ~role:"user" ~content:"post-vacuum");
      Alcotest.(check int)
        "db writable after vacuum" 1
        (List.length (Memory.load_history ~db ~session_key:"s1"));
      Alcotest.(check bool)
        "db file still exists" true (Sys.file_exists db_path))

let test_maybe_vacuum_skips_in_memory_db () =
  Test_helpers.with_memory_store (fun db ->
      let state = Db_maintenance.create_vacuum_state () in
      check_vacuum "in-memory db has no file to vacuum" "no_file"
        (Db_maintenance.maybe_vacuum ~db
           ~config:(retention_config ~vacuum_min_mb:0 ())
           ~state ~now ~window:(0, 24) ());
      Alcotest.(check bool) "no run recorded" true (state.last_vacuum_at = 0.0))

let test_maybe_vacuum_shrinks_after_purge () =
  with_file_db (fun db db_path ->
      (* Retention frees a large blob; VACUUM must return the pages to the OS
         so the file shrinks. *)
      let big = String.make 400_000 'x' in
      for _ = 1 to 8 do
        Memory.store_message ~db ~session_key:"s1"
          (Provider.make_message ~role:"user" ~content:big)
      done;
      exec db "DELETE FROM messages WHERE session_key = 's1'" [];
      (* Push the writes out of the -wal file so `before` reflects them. *)
      exec db "PRAGMA wal_checkpoint(TRUNCATE)" [];
      let before = (Unix.stat db_path).Unix.st_size in
      let state = Db_maintenance.create_vacuum_state () in
      check_vacuum "vacuum runs" "vacuumed"
        (Db_maintenance.maybe_vacuum ~db
           ~config:(retention_config ~vacuum_min_mb:0 ())
           ~state ~now ~window:(0, 24) ());
      let after = (Unix.stat db_path).Unix.st_size in
      Alcotest.(check bool)
        (Printf.sprintf "file shrank after vacuum (%d -> %d)" before after)
        true (after < before))

let suite =
  [
    ( "journal purge keeps newer rows",
      `Quick,
      test_journal_purge_keeps_newer_rows );
    ( "journal purge disabled is noop",
      `Quick,
      test_journal_purge_disabled_is_noop );
    ( "deliveries purge keeps newer rows",
      `Quick,
      test_deliveries_purge_keeps_newer_rows );
    ( "request_stats purge keeps newer rows",
      `Quick,
      test_request_stats_purge_keeps_newer_rows );
    ( "egress_audit purge keeps newer rows",
      `Quick,
      test_egress_audit_purge_keeps_newer_rows );
    ( "correlations purge closed only",
      `Quick,
      test_correlations_purge_closed_only );
    ("outbox purge terminal only", `Quick, test_outbox_purge_terminal_only);
    ("ledger purge keeps newer rows", `Quick, test_ledger_purge_keeps_newer_rows);
    ( "retention tick purges every table",
      `Quick,
      test_retention_tick_purges_every_table );
    ( "retention tick disabled config purges nothing",
      `Quick,
      test_retention_tick_disabled_config_purges_nothing );
    ( "retention tick respects per-table windows",
      `Quick,
      test_retention_tick_respects_per_table_windows );
    ("maybe_vacuum size threshold", `Quick, test_maybe_vacuum_size_threshold);
    ( "maybe_vacuum window and interval gates",
      `Quick,
      test_maybe_vacuum_window_and_interval_gates );
    ("maybe_vacuum runs and gates", `Quick, test_maybe_vacuum_runs_and_gates);
    ( "maybe_vacuum skips in-memory db",
      `Quick,
      test_maybe_vacuum_skips_in_memory_db );
    ( "maybe_vacuum shrinks db after purge",
      `Quick,
      test_maybe_vacuum_shrinks_after_purge );
  ]
