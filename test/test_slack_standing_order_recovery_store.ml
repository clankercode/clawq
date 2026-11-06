module S = Test_slack_standing_order_support
module Sql = Slack_standing_order_sql

let test_status_is_read_only () =
  let db = Sqlite3.db_open ":memory:" in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      let count () = Sql.int_query db "SELECT count(*) FROM sqlite_master" [] in
      let before = count () in
      ignore
        (Slack_standing_order_cli.run ~db ~config:Runtime_config.default
           ~now:S.now (Slack_standing_order_cli.Show "so_absent"));
      Alcotest.(check int)
        "status cannot initialize feature schema" before (count ()))

let suite = [ S.case "status never bootstraps" test_status_is_read_only ]

module R = Slack_standing_order_recovery_store
module Rollout = Slack_standing_order_rollout_store
module Store = Slack_standing_order_store
module T = Slack_standing_order_types

let ok = function Ok v -> v | Error _ -> Alcotest.fail "unexpected error"
let check = Alcotest.(check bool)

let identity : Rollout.identity =
  {
    team_id = "T123";
    app_id = "A123";
    enterprise_id = None;
    own_user_id = "UBOT";
    own_bot_id = "BOWN";
  }

let uuid db =
  let _, id, _, _ = Rollout.read_anchor db in
  id

let new_order ?(channel = "C123") ?(target = S.claude) db id =
  let o =
    Store.insert_pending db ~verified_identity:identity ~id ~team_id:"T123"
      ~api_app_id:"A123" ~channel_id:channel ~grantor_user_id:"U123"
      ~action:(S.relay_action ~target ())
      ~management_thread_ts:"100.000001" ~now:S.now ()
    |> ok
  in
  Sql.in_tx db ~on_error:Fun.id (fun () ->
      Store.activate_in_tx db ~order_id:o.id ~active_since:"1700000000.000100"
        ~confirmed_by:"U123" ~bot_user_id:"UBOT" ~bot_id:"BOWN" ~now:S.now)
  |> ok

let fresh () =
  let db = Sqlite3.db_open ":memory:" in
  Store.ensure_schema db;
  db

let expect_ready db id =
  check "fresh ready" true
    (R.rollout_readiness db ~order_id:id = R.Ready R.Fresh_no_legacy_work)

let close db =
  R.close_gap db ~team_id:"T123" ~app_id:"A123" ~channel_id:"C123"
    ~cause:"test_disconnect" ~now:S.now
  |> ok

let scan db (o : T.order) =
  let gap = close db in
  check "connected does not open" true (R.mark_connected db ~token:gap);
  let owner =
    R.claim db ~order_id:o.id ~owner_id:("owner_" ^ o.id) ~now:S.now
    |> Option.get
  in
  check "scan started" true
    (R.begin_scan db ~order:o ~owner ~mode:R.Gap_recovery ~gap
       ~oldest:"1700000000.000100" ~latest:"1700000100.000000" ~now:S.now);
  let c : R.coverage =
    {
      oldest = "1700000000.000100";
      latest = "1700000100.000000";
      history_cursor = "";
      pause_cursor = "";
      last_success = None;
    }
  in
  (gap, owner, c)

let finish db (o : T.order) gap owner (c : R.coverage) =
  check "pause typed completion" true
    (R.complete_pause db ~order_id:o.id ~owner ~now:S.now);
  check "history typed completion" true
    (R.complete_history db ~order_id:o.id ~owner ~now:S.now);
  check "finished" true (R.finish_scan db ~order_id:o.id ~owner ~now:S.now);
  check "verified" true
    (R.verify_gap db ~token:gap ~owner ~through:c.latest ~now:S.now)

let fire db (o : T.order) root room start =
  R.record_root db ~order:o ~root_ts:root ~room_id:room ~date_start:start
    ~fire_id:None;
  Store.insert_fire db ~order:o ~root_ts:root ~room_id:room ~date_start:start
    ~now:S.now ()
  |> ok |> Option.get

let test_production_setup () =
  let open Lwt.Syntax in
  Test_slack_standing_order_fixtures.with_orders (fun env ->
      let* o, _ = Test_slack_standing_order_cards.confirmed env in
      expect_ready env.db o.id;
      check "normal activation announced" true
        (Store.activation_announced env.db ~order_id:o.id);
      Alcotest.(check int)
        "no importer receipt" 0
        (Sql.int_query env.db
           "SELECT count(*) FROM slack_standing_order_inventory_manifests" []);
      let* out =
        Test_slack_standing_order_fixtures.invoke env
          "slack_standing_order_show"
          (`Assoc [ ("order", `String "SO-1") ])
      in
      check "show uses same fresh decision" true
        (Test_helpers.string_contains out "fresh_no_legacy_work");
      Lwt.return_unit)

let test_gap_fences () =
  let db = fresh () in
  let o = new_order db "so_fresh" in
  expect_ready db o.id;
  let gap, owner, c = scan db o in
  check "finish cannot stand in for sweep" false
    (R.finish_scan db ~order_id:o.id ~owner ~now:S.now);
  check "connect alone cannot verify" false
    (R.verify_gap db ~token:gap ~owner ~through:c.latest ~now:S.now);
  check "page commit" true
    (R.save_page db ~order_id:o.id ~owner ~coverage:c
       ~candidates:[ ("1700000001.000001", "R123", "1700000001") ]
       ~now:S.now);
  finish db o gap owner c;
  let held = fire db o "1700000001.000001" "R123" "1700000001" in
  check "recovery still held without notice" false
    (R.relay_allowed db ~fire_id:held.id ~now:S.now);
  let live = fire db o "1700000002.000001" "RNEW" "1700000002" in
  check "covered live candidate admitted" true
    (R.relay_allowed db ~fire_id:live.id ~now:S.now);
  let other =
    new_order ~target:{ S.claude with app_id = "AOTHER" } db "so_other"
  in
  let other_fire = fire db other "1700000003.000001" "ROTHER" "1700000003" in
  check "verified order cannot open another order on the same channel" false
    (R.relay_allowed db ~fire_id:other_fire.id ~now:S.now);
  let new_gap = close db in
  check "synchronous close" false
    (R.relay_allowed db ~fire_id:live.id ~now:S.now);
  check "old hello rejected" false (R.mark_connected db ~token:gap);
  check "new hello alone closed" true (R.mark_connected db ~token:new_gap);
  check "old sweep rejected" false
    (R.verify_gap db ~token:gap ~owner ~through:c.latest ~now:S.now);
  check "stale page rejected" false
    (R.save_page db ~order_id:o.id ~owner ~coverage:c ~candidates:[] ~now:S.now);
  ignore (Sqlite3.db_close db)

let test_atomic_page () =
  let db = fresh () in
  let o = new_order db "so_atomic" in
  let _, owner, c = scan db o in
  let bad = { c with R.history_cursor = "advance" } in
  check "partial page rolls back" false
    (R.save_page db ~order_id:o.id ~owner ~coverage:bad
       ~candidates:
         [ ("1700000001.1", "R1", "1700000001"); ("bad", "R2", "1700000002") ]
       ~now:S.now);
  Alcotest.(check int)
    "no first candidate from rolled back page" 0
    (Sql.int_query db
       "SELECT count(*) FROM slack_standing_order_recovery_candidates" []);
  Alcotest.(check string)
    "cursor unchanged" ""
    (Option.get (R.load_coverage db ~order_id:o.id)).history_cursor;
  check "expired owner" false
    (R.save_page db ~order_id:o.id ~owner ~coverage:c ~candidates:[]
       ~now:(S.now +. 121.));
  check "expired renew" false
    (R.renew db ~order_id:o.id ~owner ~now:(S.now +. 121.));
  ignore (Sqlite3.db_close db)

let test_corruption () =
  List.iter
    (fun (name, corrupt) ->
      let db = fresh () in
      let o = new_order db "so_corrupt" in
      corrupt db;
      check
        (name ^ " denies readiness")
        true
        (match R.rollout_readiness db ~order_id:o.id with
        | R.Denied _ -> true
        | _ -> false);
      let before = Sql.int_query db "SELECT count(*) FROM sqlite_master" [] in
      check
        (name ^ " is not repaired")
        true
        (try
           Store.ensure_schema db;
           false
         with _ -> true);
      Alcotest.(check int)
        "no schema repair" before
        (Sql.int_query db "SELECT count(*) FROM sqlite_master" []);
      ignore (Sqlite3.db_close db))
    [
      ( "missing state",
        fun db ->
          Sql.exec db "DELETE FROM slack_standing_order_rollout_state" [] );
      ( "corrupt evidence",
        fun db ->
          Sql.exec db
            "DROP TRIGGER slack_standing_order_rollout_evidence_no_update" [];
          Sql.exec db
            "UPDATE slack_standing_order_rollout_evidence SET digest='bad' \
             WHERE seq=1"
            [] );
      ( "malformed installed transcript index",
        fun db ->
          Sql.exec db "DROP INDEX slack_standing_order_fires_transcript" [];
          Sql.exec db
            "CREATE INDEX slack_standing_order_fires_transcript ON \
             slack_standing_order_fires(transcript_file_id) WHERE \
             transcript_file_id IS NOT NULL"
            [] );
      ( "missing schema component",
        fun db -> Sql.exec db "DROP TABLE slack_standing_order_thread_work" []
      );
    ];
  let db = fresh () in
  let o = new_order db "so_activation" in
  Sql.exec db "DELETE FROM slack_standing_order_activation_bindings" [];
  check "missing activation is denied" true
    (R.rollout_readiness db ~order_id:o.id = R.Denied R.Activation_unbound);
  ignore (Sqlite3.db_close db)

let with_two_handles fn =
  let path = Filename.temp_file "clawq-rollout-" ".db" in
  let a = Sqlite3.db_open path and b = Sqlite3.db_open path in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sqlite3.db_close a);
      ignore (Sqlite3.db_close b);
      Sys.remove path)
    (fun () -> fn a b)

let test_two_handles () =
  with_two_handles (fun a b ->
      Store.ensure_schema a;
      let original = uuid a in
      Store.ensure_schema b;
      Alcotest.(check string) "one installation" original (uuid b);
      let concurrent_reads = ref 0 in
      Sqlite3.create_fun0 a "observe_uncommitted_binding" (fun () ->
          incr concurrent_reads;
          check "second handle cannot see uncommitted first binding" true
            (R.rollout_readiness b ~order_id:"so_race"
            = R.Denied R.Unbound_installation);
          Sqlite3.Data.NULL);
      Sql.exec a
        "CREATE TRIGGER observe_binding BEFORE INSERT ON slack_standing_orders \
         BEGIN SELECT observe_uncommitted_binding(); END"
        [];
      let o = new_order a "so_race" in
      Sql.exec a "DROP TRIGGER observe_binding" [];
      Alcotest.(check int)
        "concurrent reader observed insertion boundary" 1 !concurrent_reads;
      check "second identity cannot rebind committed installation" true
        (Result.is_error
           (Store.insert_pending b
              ~verified_identity:{ identity with own_bot_id = "BOTHER" }
              ~id:"so_wrong_identity" ~team_id:"T123" ~api_app_id:"A123"
              ~channel_id:"COTHER" ~grantor_user_id:"U123"
              ~action:(S.relay_action ()) ~management_thread_ts:"100.000001"
              ~now:S.now ()));
      expect_ready b o.id;
      let gap, owner, c = scan a o in
      check "second handle cannot take owner" true
        (R.claim b ~order_id:o.id ~owner_id:"second" ~now:S.now = None);
      let newer =
        R.claim b ~order_id:o.id ~owner_id:"second" ~now:(S.now +. 121.)
        |> Option.get
      in
      check "takeover increments generation" true
        (newer.generation > owner.generation);
      check "old holder cannot finish" false
        (R.verify_gap a ~token:gap ~owner ~through:c.latest ~now:(S.now +. 121.));
      check "old holder cannot page" false
        (R.save_page a ~order_id:o.id ~owner ~coverage:c ~candidates:[]
           ~now:(S.now +. 121.));
      Alcotest.(check string) "takeover preserves identity" original (uuid b))

let test_identity_epoch () =
  let db = fresh () in
  let o = new_order db "so_identity" in
  let original = uuid db in
  check "wrong identity detected" true
    (Rollout.revalidate_identity db { identity with own_bot_id = "BOTHER" }
    = Ok false);
  check "identity denied" true
    (R.rollout_readiness db ~order_id:o.id = R.Denied R.Identity_mismatch);
  ignore (Rollout.revalidate_identity db identity);
  expect_ready db o.id;
  ignore
    (Store.deactivate db ~order_id:o.id (Store.Pause "U123") ~now:S.now |> ok);
  let paused = Store.find db o.id |> Option.get in
  let resumed =
    Store.resume db ~order_id:o.id ~expected_epoch:paused.epoch
      ~active_since:"1700000200.000001" ~bot_user_id:"UBOT" ~bot_id:"BOWN"
      ~now:S.now
    |> ok
  in
  expect_ready db resumed.id;
  Alcotest.(check int)
    "both activation boundaries retained" 2
    (Sql.int_query db
       "SELECT count(*) FROM slack_standing_order_activation_bindings WHERE \
        order_id=?"
       [ Sql.text o.id ]);
  Alcotest.(check string) "resume never mints installation" original (uuid db);
  ignore (Sqlite3.db_close db)

let test_bootstrap_rollback () =
  let db = Sqlite3.db_open ":memory:" in
  check "initialization interrupted" true
    (try
       Rollout.bootstrap db ~install:(fun () ->
           Slack_standing_order_store_base.ensure_schema_tables db;
           failwith "injected crash");
       false
     with _ -> true);
  Alcotest.(check int)
    "DDL and proof rolled back together" 0
    (Sql.int_query db "SELECT count(*) FROM sqlite_master" []);
  Store.ensure_schema db;
  let o = new_order db "so_retry" in
  expect_ready db o.id;
  ignore (Sqlite3.db_close db)

let suite =
  suite
  @ [
      S.case "ordinary audited create and confirmation need no inventory"
        test_production_setup;
      S.case "typed coverage gap and order fences" test_gap_fences;
      S.case "atomic candidate cursor rollback and expiry" test_atomic_page;
      S.case "corrupt proof never repaired" test_corruption;
      S.case "two handles share evidence and owner CAS" test_two_handles;
      S.case "identity and resume activation boundaries" test_identity_epoch;
      S.case "atomic bootstrap rollback and retry" test_bootstrap_rollback;
    ]

let test_unused_and_ambiguous_origins () =
  let db = Sqlite3.db_open ":memory:" in
  Slack_standing_order_store_base.ensure_schema_tables db;
  Access_snapshot.init_schema db;
  Setup_plan_apply.init_schema db;
  Sql.exec db "CREATE TABLE slack_huddles(observer_metadata TEXT)" [];
  Sql.exec db "INSERT INTO slack_huddles VALUES('no order authority')" [];
  let unrelated =
    Test_setup_plan.make_clean_plan ~now:S.now ~id:"unrelated" ()
  in
  ignore (Setup_plan_apply.store_plan ~db unrelated |> Result.get_ok);
  Store.ensure_schema db;
  let o = new_order db "so_unused" in
  expect_ready db o.id;
  ignore (Sqlite3.db_close db);
  List.iter
    (fun setup ->
      let db = Sqlite3.db_open ":memory:" in
      setup db;
      let malformed = not (Rollout.base_shape db) in
      let initialized =
        try
          Store.ensure_schema db;
          true
        with _ -> false
      in
      if malformed then (
        check "malformed schema initialization denied" false initialized;
        check "malformed schema has no anchored proof" false
          (Rollout.table_exists db "slack_standing_order_feature_version"))
      else (
        check "ambiguous/used origin held" true
          (R.rollout_readiness db ~order_id:"missing"
          = R.Denied R.Inventory_pending);
        Store.ensure_schema db;
        check "restart never auto-freshens" true
          (R.rollout_readiness db ~order_id:"missing"
          = R.Denied R.Inventory_pending));
      ignore (Sqlite3.db_close db))
    [
      (fun db ->
        Slack_standing_order_store_base.ensure_schema_tables db;
        Sql.exec db
          "INSERT INTO slack_standing_order_dispatches(fire_id,attempt_no) \
           VALUES('orphan',0)"
          []);
      (fun db ->
        Sql.exec db "CREATE TABLE slack_standing_order_cards(id TEXT)" []);
      (fun db ->
        Slack_standing_order_store_base.ensure_schema_tables db;
        Sql.exec db
          "CREATE TABLE replacement AS SELECT * FROM slack_standing_order_fires"
          [];
        Sql.exec db "DROP TABLE slack_standing_order_fires" [];
        Sql.exec db
          "ALTER TABLE replacement RENAME TO slack_standing_order_fires" []);
      (fun db ->
        Slack_standing_order_store_base.ensure_schema_tables db;
        Setup_plan_apply.init_schema db;
        Sql.exec db
          "INSERT INTO \
           setup_plans(id,digest,principal_id,base_revision,plan_json,created_at,expires_at,updated_at) \
           VALUES('broken','d','u','r','{broken','a','b','a')"
          []);
      (fun db ->
        Slack_standing_order_store_base.ensure_schema_tables db;
        Access_snapshot.init_schema db;
        Sql.exec db
          "INSERT INTO \
           access_snapshots(id,config_hash,work_type,standing_order_id) \
           VALUES('orphan','hash','standing_order','missing')"
          []);
    ]

let test_notice_boundary () =
  let db = fresh () in
  let o = new_order db "so_notice" in
  let gap, owner, c = scan db o in
  check "held page" true
    (R.save_page db ~order_id:o.id ~owner ~coverage:c
       ~candidates:[ ("1700000001.1", "RHOLD", "1700000001") ]
       ~now:S.now);
  finish db o gap owner c;
  let f = fire db o "1700000001.1" "RHOLD" "1700000001" in
  let card =
    Store.insert_card db ~kind:T.Pause_button ~order:o ~created_by:"U123"
      ~now:S.now ()
  in
  check "batch creation" true
    (R.create_batch db ~order:o ~owner ~batch_id:"batch_one" ~card_id:card.id
       ~roots:[ ("1700000001.1", "1700000001") ]
       ~now:S.now);
  check "notice reservation" true
    (R.reserve_notice db ~batch_id:"batch_one" ~owner ~now:S.now);
  check "no second notice reservation" false
    (R.reserve_notice db ~batch_id:"batch_one" ~owner ~now:S.now);
  check "short hold cannot bind" false
    (R.bind_notice db ~batch_id:"batch_one" ~owner ~ts:"1700000100.1"
       ~release_at:(S.now +. 599.) ~now:S.now);
  check "confirmed exact card bound" true
    (R.bind_notice db ~batch_id:"batch_one" ~owner ~ts:"1700000100.1"
       ~release_at:(S.now +. 600.) ~now:S.now);
  List.iter
    (fun t ->
      check "renew during hold" true
        (R.renew db ~order_id:o.id ~owner ~now:(S.now +. t)))
    [ 100.; 200.; 300.; 400.; 500.; 599. ];
  check "599 held" false
    (R.release_batch db ~batch_id:"batch_one" ~owner ~now:(S.now +. 599.));
  check "600 released" true
    (R.release_batch db ~batch_id:"batch_one" ~owner ~now:(S.now +. 600.));
  check "confirmed hold admission" true
    (R.relay_allowed db ~fire_id:f.id ~now:(S.now +. 600.));
  ignore (close db);
  check "new gap revokes released batch" false
    (R.relay_allowed db ~fire_id:f.id ~now:(S.now +. 600.));
  ignore (Sqlite3.db_close db)

let test_readonly_p3_status () =
  let source = S.fresh_db () in
  let o = S.seed source () in
  let db = Sqlite3.db_open ":memory:" in
  Slack_standing_order_store_base.ensure_schema_tables db;
  ignore (Slack_standing_order_store_base.insert_order db o);
  let before = Sql.int_query db "SELECT count(*) FROM sqlite_master" [] in
  ignore
    (Slack_standing_order_cli.run ~db ~config:Runtime_config.default ~now:S.now
       (Slack_standing_order_cli.Show o.id));
  Alcotest.(check int)
    "P3 show cannot bootstrap" before
    (Sql.int_query db "SELECT count(*) FROM sqlite_master" []);
  check "P3 show remains missing proof" true
    (R.rollout_readiness db ~order_id:o.id = R.Denied R.Missing_proof);
  ignore (Sqlite3.db_close source);
  ignore (Sqlite3.db_close db)

let suite =
  suite
  @ [
      S.case "unused existing surface and ambiguous orphan origins"
        test_unused_and_ambiguous_origins;
      S.case "bound notice hold and generation revocation" test_notice_boundary;
      S.case "P3 show never initializes or migrates" test_readonly_p3_status;
    ]

let test_first_binding_rollback () =
  let db = fresh () in
  let original = uuid db in
  Sql.exec db
    "CREATE TRIGGER fail_pending BEFORE INSERT ON slack_standing_orders BEGIN \
     SELECT RAISE(ABORT,'injected first pending failure'); END"
    [];
  check "binding failure rolls back" true
    (Result.is_error
       (Store.insert_pending db ~verified_identity:identity ~id:"so_rollback"
          ~team_id:"T123" ~api_app_id:"A123" ~channel_id:"C123"
          ~grantor_user_id:"U123" ~action:(S.relay_action ())
          ~management_thread_ts:"100.000001" ~now:S.now ()));
  Alcotest.(check int)
    "no partial pending binding" 0
    (Sql.int_query db "SELECT count(*) FROM slack_standing_order_order_bindings"
       []);
  Alcotest.(check int)
    "no partial binding evidence" 1
    (Sql.int_query db
       "SELECT count(*) FROM slack_standing_order_rollout_evidence" []);
  check "still unbound unused" true
    (R.rollout_readiness db ~order_id:"so_rollback"
    = R.Denied R.Unbound_installation);
  Sql.exec db "DROP TRIGGER fail_pending" [];
  let o = new_order db "so_retry" in
  expect_ready db o.id;
  Alcotest.(check string) "binding retry uses same UUID" original (uuid db);
  ignore (Sqlite3.db_close db)

let test_cursor_rescan () =
  let db = fresh () in
  let o = new_order db "so_cursor" in
  let _, owner, c = scan db o in
  let next = { c with R.history_cursor = "loop" } in
  check "first complete page persisted" true
    (R.save_page db ~order_id:o.id ~owner ~coverage:next
       ~candidates:[ ("1700000001.1", "R1", "1700000001") ]
       ~now:S.now);
  check "repeated cursor starts bounded rescan" false
    (R.save_page db ~order_id:o.id ~owner ~coverage:next ~candidates:[]
       ~now:S.now);
  let saved = R.load_coverage db ~order_id:o.id |> Option.get in
  Alcotest.(check string) "rescan keeps oldest" c.oldest saved.oldest;
  Alcotest.(check string) "rescan keeps latest" c.latest saved.latest;
  Alcotest.(check string) "rescan resets cursor" "" saved.history_cursor;
  Alcotest.(check int)
    "dedup candidates survive rescan" 1
    (Sql.int_query db
       "SELECT count(*) FROM slack_standing_order_recovery_candidates" []);
  check "incomplete rescan cannot finish" false
    (R.finish_scan db ~order_id:o.id ~owner ~now:S.now);
  ignore (Sqlite3.db_close db)

let test_startup_orphan_live_root () =
  let db = fresh () in
  let o = new_order db "so_restart" in
  let gap, owner, c = scan db o in
  finish db o gap owner c;
  R.record_root db ~order:o ~root_ts:"1700000002.000001" ~room_id:"RNEW"
    ~date_start:"1700000002" ~fire_id:None;
  R.recover_startup db ~now:S.now;
  R.recover_startup db ~now:S.now;
  let gap, owner, c = scan db o in
  finish db o gap owner c;
  let f = fire db o "1700000002.000001" "RNEW" "1700000002" in
  check "pre-crash root cannot bypass recovery notice after coverage" false
    (R.relay_allowed db ~fire_id:f.id ~now:S.now);
  ignore (Sqlite3.db_close db)

let suite =
  suite
  @ [
      S.case "first Plan binding rollback preserves initial proof"
        test_first_binding_rollback;
      S.case "repeated cursor rescans fixed bounds without skipping"
        test_cursor_rescan;
      S.case "startup holds durable roots without dispatched fire"
        test_startup_orphan_live_root;
    ]

let test_malformed_indexes () =
  List.iter
    (fun (name, replacement) ->
      let db = Sqlite3.db_open ":memory:" in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close db))
        (fun () ->
          Slack_standing_order_store_base.ensure_schema_tables db;
          Sql.exec db ("DROP INDEX " ^ name) [];
          Sql.exec db replacement [];
          check "isolated malformed index is not recognized" false
            (Rollout.base_shape db);
          check "malformed resulting schema cannot be anchored" true
            (try
               Store.ensure_schema db;
               false
             with _ -> true);
          check "no version anchor after rollback" false
            (Rollout.table_exists db "slack_standing_order_feature_version");
          check "malformed schema never ready" true
            (match R.rollout_readiness db ~order_id:"absent" with
            | R.Denied _ -> true
            | _ -> false)))
    [
      ( "slack_standing_order_fires_transcript",
        "CREATE INDEX slack_standing_order_fires_transcript ON \
         slack_standing_order_fires(transcript_file_id) WHERE \
         transcript_file_id IS NOT NULL" );
      ( "slack_standing_order_fires_transcript",
        "CREATE UNIQUE INDEX slack_standing_order_fires_transcript ON \
         slack_standing_order_fires(transcript_file_id) WHERE \
         transcript_file_id IS NOT NULL AND order_id='only_one'" );
      ( "slack_standing_orders_one_live",
        "CREATE INDEX slack_standing_orders_one_live ON \
         slack_standing_orders(team_id,channel_id,trigger_kind,target_app_id) \
         WHERE state IN ('pending_confirmation','active','paused')" );
      ( "slack_standing_orders_one_live",
        "CREATE UNIQUE INDEX slack_standing_orders_one_live ON \
         slack_standing_orders(team_id,channel_id,trigger_kind,target_app_id) \
         WHERE state='active'" );
    ];
  let db = fresh () in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      let a = new_order db "so_unique_a" in
      let b =
        new_order ~target:{ S.claude with app_id = "AOTHER" } db "so_unique_b"
      in
      let f1 = fire db a "1700000001.1" "R1" "1700000001" in
      let f2 = fire db b "1700000002.1" "R2" "1700000002" in
      Sql.exec db
        "UPDATE slack_standing_order_fires SET transcript_file_id='FSAME' \
         WHERE id=?"
        [ Sql.text f1.id ];
      check "global transcript cannot be reused by another order" true
        (try
           Sql.exec db
             "UPDATE slack_standing_order_fires SET transcript_file_id='FSAME' \
              WHERE id=?"
             [ Sql.text f2.id ];
           false
         with _ -> true);
      check "one live order invariant" true
        (try
           ignore (new_order db "so_duplicate_target");
           false
         with _ -> true))

let test_close_failure_shared_handles () =
  let path = Filename.temp_file "clawq-shared-denial-" ".db" in
  let a = Sqlite3.db_open path and b = Sqlite3.db_open path in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sqlite3.db_close a);
      ignore (Sqlite3.db_close b);
      Sys.remove path)
    (fun () ->
      Store.ensure_schema a;
      let o = new_order a "so_shared" in
      let gap, owner, c = scan a o in
      check "recovery candidate saved" true
        (R.save_page a ~order_id:o.id ~owner ~coverage:c
           ~candidates:[ ("1700000001.1", "RHOLD", "1700000001") ]
           ~now:S.now);
      finish a o gap owner c;
      let live = fire a o "1700000002.1" "RLIVE" "1700000002" in
      let card =
        Store.insert_card a ~kind:T.Pause_button ~order:o ~created_by:"U123"
          ~now:S.now ()
      in
      check "batch created" true
        (R.create_batch a ~order:o ~owner ~batch_id:"shared_batch"
           ~card_id:card.id
           ~roots:[ ("1700000001.1", "1700000001") ]
           ~now:S.now);
      check "notice reserved" true
        (R.reserve_notice a ~batch_id:"shared_batch" ~owner ~now:S.now);
      check "notice confirmed" true
        (R.bind_notice a ~batch_id:"shared_batch" ~owner ~ts:"1700000100.1"
           ~release_at:(S.now +. 600.) ~now:S.now);
      List.iter
        (fun t ->
          check "renew hold" true
            (R.renew a ~order_id:o.id ~owner ~now:(S.now +. t)))
        [ 100.; 200.; 300.; 400.; 500.; 599. ];
      List.iter
        (fun db ->
          check "admitted before failure" true
            (R.relay_allowed db ~fire_id:live.id ~now:(S.now +. 600.)))
        [ a; b ];
      Sql.exec a
        "CREATE TRIGGER fail_shared_close BEFORE UPDATE ON \
         slack_standing_order_recovery_gaps BEGIN SELECT RAISE(ABORT,'injected \
         close failure'); END"
        [];
      check "durable close failed" true
        (Result.is_error
           (R.close_gap a ~team_id:"T123" ~app_id:"A123" ~channel_id:"C123"
              ~cause:"disconnect" ~now:(S.now +. 600.)));
      List.iter
        (fun db ->
          check "all local handles deny relay" false
            (R.relay_allowed db ~fire_id:live.id ~now:(S.now +. 600.));
          check "all local handles deny verification" false
            (R.verify_gap db ~token:gap ~owner ~through:c.latest
               ~now:(S.now +. 600.));
          check "all local handles deny release" false
            (R.release_batch db ~batch_id:"shared_batch" ~owner
               ~now:(S.now +. 600.));
          check "old hello cannot clear latch" false
            (R.mark_connected db ~token:gap))
        [ a; b ];
      let unrelated_path = Filename.temp_file "clawq-unrelated-denial-" ".db" in
      List.iter
        (fun path ->
          let unrelated = Sqlite3.db_open path in
          Fun.protect
            ~finally:(fun () -> ignore (Sqlite3.db_close unrelated))
            (fun () ->
              let backup =
                Sqlite3.Backup.init ~dst:unrelated ~dst_name:"main" ~src:a
                  ~src_name:"main"
              in
              ignore (Sqlite3.Backup.step backup (-1));
              ignore (Sqlite3.Backup.finish backup);
              check
                "unrelated DB with identical installation UUID stays admitted"
                true
                (R.relay_allowed unrelated ~fire_id:live.id ~now:(S.now +. 600.))))
        [ ":memory:"; unrelated_path ];
      Sys.remove unrelated_path;
      check "unrelated channel has no local denial" false
        (R.latched b { gap with R.channel_id = "COTHER" });
      Sql.exec a "DROP TRIGGER fail_shared_close" [];
      let next =
        R.close_gap b ~team_id:"T123" ~app_id:"A123" ~channel_id:"C123"
          ~cause:"retry" ~now:(S.now +. 600.)
        |> ok
      in
      check "successful close supersedes generation" true
        (next.gap_generation > gap.gap_generation);
      check "retry clears shared latch" true (R.mark_connected a ~token:next);
      List.iter
        (fun db ->
          check "old generation remains denied after durable retry" false
            (R.relay_allowed db ~fire_id:live.id ~now:(S.now +. 600.)))
        [ a; b ];
      check "retry scan under new durable generation" true
        (R.begin_scan b ~order:o ~owner ~mode:R.Gap_recovery ~gap:next
           ~oldest:c.oldest ~latest:c.latest ~now:(S.now +. 600.));
      check "retry pause coverage" true
        (R.complete_pause b ~order_id:o.id ~owner ~now:(S.now +. 600.));
      check "retry history coverage" true
        (R.complete_history b ~order_id:o.id ~owner ~now:(S.now +. 600.));
      check "retry scan complete" true
        (R.finish_scan b ~order_id:o.id ~owner ~now:(S.now +. 600.));
      check "retry verified" true
        (R.verify_gap b ~token:next ~owner ~through:c.latest ~now:(S.now +. 600.));
      List.iter
        (fun db ->
          check "new verified generation admits all local handles" true
            (R.relay_allowed db ~fire_id:live.id ~now:(S.now +. 600.)))
        [ a; b ])

let suite =
  suite
  @ [
      S.case
        "narrow malformed index definitions cannot establish schema authority"
        test_malformed_indexes;
      S.case "failed close denies every local handle until durable retry"
        test_close_failure_shared_handles;
    ]

let test_missing_constraints () =
  let replace sql before after =
    let re = Str.regexp_string before in
    Str.global_replace re after sql
  in
  List.iter
    (fun (before, after) ->
      let db = Sqlite3.db_open ":memory:" in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close db))
        (fun () ->
          List.iter
            (fun sql -> Sql.exec db (replace sql before after) [])
            Slack_standing_order_store_base.schema;
          Sql.exec db
            (replace Slack_standing_order_store_base.dispatch_schema before
               after)
            [];
          check "isolated missing check or nullability denied" false
            (Rollout.base_shape db);
          check "no malformed resulting schema anchor" true
            (try
               Store.ensure_schema db;
               false
             with _ -> true);
          check "no inventory can certify malformed schema" true
            (Result.is_error
               (Slack_standing_order_receipt_import.apply db []
                  ~manifest_digest:(String.make 64 '0')));
          check "malformed stays denied" true
            (match R.rollout_readiness db ~order_id:"absent" with
            | R.Denied _ -> true
            | _ -> false)))
    [
      ("CHECK (reposts BETWEEN 0 AND 1)", "");
      ( "CHECK (reposts BETWEEN 0 AND 1)",
        "/* CHECK (reposts BETWEEN 0 AND 1) */" );
      ("CHECK (display_no >= 1)", "");
      ("CHECK (date_start <> '')", "");
      ("CHECK (attempt_no IN (0,1))", "");
      ("target_app_id TEXT NOT NULL", "target_app_id TEXT");
      ("uploaded_file_ids TEXT NOT NULL", "uploaded_file_ids TEXT");
    ]

let suite =
  suite
  @ [
      S.case "required checks and nullability cannot be certified by inventory"
        test_missing_constraints;
    ]
