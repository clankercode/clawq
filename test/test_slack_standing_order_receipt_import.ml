module S = Test_slack_standing_order_support
module I = Slack_standing_order_receipt_import
module R = Slack_standing_order_rollout_store
module Store = Slack_standing_order_store
module Sql = Slack_standing_order_sql

let test_preview_uninitialized () =
  let db = Sqlite3.db_open ":memory:" in
  let before = Sql.int_query db "SELECT count(*) FROM sqlite_master" [] in
  Alcotest.(check bool)
    "missing schema denied" true
    (Result.is_error (I.preview db []));
  Alcotest.(check int)
    "preview did not create schema" before
    (Sql.int_query db "SELECT count(*) FROM sqlite_master" []);
  ignore (Sqlite3.db_close db)

let suite =
  [
    S.case "preview is read only on uninitialized DB" test_preview_uninitialized;
  ]

module T = Slack_standing_order_types

let check = Alcotest.(check bool)
let ok = function Ok v -> v | Error e -> Alcotest.fail (I.error_name e)

let identity : R.identity =
  {
    team_id = "T123";
    app_id = "A123";
    enterprise_id = None;
    own_user_id = "UBOT";
    own_bot_id = "BOWN";
  }

let legacy () =
  (* An actual pre-feature schema and raw order, BEFORE the real initializer. *)
  let source = S.fresh_db () in
  let order = S.seed source () in
  let db = Sqlite3.db_open ":memory:" in
  Slack_standing_order_store_base.ensure_schema_tables db;
  check "raw P3 order inserted" true
    (Slack_standing_order_store_base.insert_order db order = Sqlite3.Rc.DONE);
  Store.ensure_schema db;
  ignore (Sqlite3.db_close source);
  let _, installation, _, _ = R.read_anchor db in
  (db, order, installation)

let receipt (o : T.order) installation =
  let r : I.external_receipt =
    {
      schema_version = 1;
      installation;
      identity;
      order_id = o.id;
      channel_id = o.channel_id;
      root_ts = "1700000001.000001";
      room_id = "RMANUAL";
      date_start = "1700000001";
      relay_ts = "1700000030.000001";
      own_file_ids = [ "FNOTES"; "FTRANSCRIPT" ];
      source_canvas_id = None;
      source_transcript_id = None;
      evidence_digest = "";
      reviewer = "reviewer-test";
    }
  in
  { r with evidence_digest = I.receipt_digest r }

let inventory db (o : T.order) installation receipts =
  let orders : I.activation_scope list =
    [
      {
        order_id = o.id;
        epoch = o.epoch;
        active_since = o.active_since;
        activation_evidence = R.digest "reviewed-original-click";
      };
    ]
  in
  let entries =
    List.map (fun (r : I.external_receipt) -> r.evidence_digest) receipts
    |> List.sort String.compare
  in
  ({
     schema_version = 1;
     installation;
     identity;
     bootstrap_scope_digest =
       R.digest (Yojson.Safe.to_string (R.bootstrap_inventory db));
     reviewer = "reviewer-test";
     scope_review = "Reviewed all raw P3 work and all input entries";
     orders;
     entries;
     dispositions = List.map (fun d -> I.Delivered d) entries;
   }
    : I.inventory)

let validated db m receipts =
  (I.validate_inventory db m |> ok)
  :: List.map (fun r -> I.validate db ~receipt:r |> ok) receipts

let count db table = Sql.int_query db ("SELECT count(*) FROM " ^ table) []

let test_manual_import () =
  let db, o, id = legacy () in
  check "P3 initially pending" true
    (R.rollout_readiness db ~order_id:o.id = R.Denied R.Inventory_pending);
  let r = receipt o id in
  let m = inventory db o id [ r ] in
  let digest = I.inventory_digest m in
  let inputs = validated db m [ r ] in
  ignore (I.preview db inputs |> ok);
  Alcotest.(check int)
    "preview writes no delivery" 0
    (count db "slack_standing_order_external_deliveries");
  ignore (I.apply db inputs ~manifest_digest:digest |> ok);
  check "inventory verified" true
    (R.rollout_readiness db ~order_id:o.id = R.Ready R.Inventory_verified);
  check "delivered instance suppressed" true
    (R.suppressed db ~order_id:o.id ~root_ts:r.root_ts ~room_id:r.room_id
       ~date_start:r.date_start);
  Alcotest.(check int)
    "no fabricated fire" 0
    (count db "slack_standing_order_fires");
  Alcotest.(check int)
    "no fabricated attempt/debit" 0
    (count db "slack_standing_order_dispatches");
  ignore (I.apply db inputs ~manifest_digest:digest |> ok);
  Alcotest.(check int)
    "repeat import one tombstone" 1
    (count db "slack_standing_order_suppression");
  ignore (I.verify_rollout db ~manifest_digest:digest |> ok);
  let pause =
    Store.deactivate db ~order_id:o.id (Store.Pause "U123") ~now:S.now
    |> Result.get_ok
  in
  ignore
    (Store.resume db ~order_id:o.id ~expected_epoch:pause.after.epoch
       ~active_since:"1700000200.000001" ~bot_user_id:"UBOT" ~bot_id:"BOWN"
       ~now:S.now
    |> Result.get_ok);
  check "inventory readiness survives ordinary resume" true
    (R.rollout_readiness db ~order_id:o.id = R.Ready R.Inventory_verified);
  check "suppression survives resume epoch" true
    (R.suppressed db ~order_id:o.id ~root_ts:r.root_ts ~room_id:r.room_id
       ~date_start:r.date_start);
  ignore (R.revalidate_identity db { identity with own_bot_id = "BNEW" });
  check "suppression survives identity change" true
    (R.suppressed db ~order_id:o.id ~root_ts:r.root_ts ~room_id:r.room_id
       ~date_start:r.date_start);
  check "tombstone cannot be deleted" true
    (try
       Sql.exec db "DELETE FROM slack_standing_order_suppression" [];
       false
     with _ -> true);
  ignore (Sqlite3.db_close db)

let test_partial_and_rollback () =
  let db, o, id = legacy () in
  let r = receipt o id in
  let m = inventory db o id [ r ] in
  let inputs = validated db m [ r ] in
  let digest = I.inventory_digest m in
  check "partial inventory denied" true
    (Result.is_error (I.apply db [ List.hd inputs ] ~manifest_digest:digest));
  check "partial stays pending" true
    (R.rollout_readiness db ~order_id:o.id = R.Denied R.Inventory_pending);
  Sql.exec db
    "CREATE TRIGGER fail_manifest BEFORE INSERT ON \
     slack_standing_order_inventory_manifests BEGIN SELECT \
     RAISE(ABORT,'injected crash before completion'); END"
    [];
  check "completion crash rejected" true
    (Result.is_error (I.apply db inputs ~manifest_digest:digest));
  Alcotest.(check int)
    "suppression rolled back" 0
    (count db "slack_standing_order_suppression");
  Alcotest.(check int)
    "external metadata rolled back" 0
    (count db "slack_standing_order_external_deliveries");
  check "completion crash still pending" true
    (R.rollout_readiness db ~order_id:o.id = R.Denied R.Inventory_pending);
  Sql.exec db "DROP TRIGGER fail_manifest" [];
  ignore (I.apply db inputs ~manifest_digest:digest |> ok);
  ignore (Sqlite3.db_close db)

let test_validation () =
  let db, o, id = legacy () in
  let r = receipt o id in
  List.iter
    (fun bad ->
      let bad = { bad with I.evidence_digest = I.receipt_digest bad } in
      check "validly hashed conflicting identity is rejected" true
        (I.validate db ~receipt:bad = Error I.Invalid_identity))
    [
      { r with channel_id = "COTHER" };
      { r with identity = { identity with own_user_id = "UOTHER" } };
    ];
  List.iter
    (fun (name, r) ->
      check name true (Result.is_error (I.validate db ~receipt:r)))
    [
      ("wrong installation", { r with installation = "other" });
      ("wrong channel", { r with channel_id = "COTHER" });
      ("wrong root", { r with root_ts = "not-ts" });
      ("missing date", { r with date_start = "" });
      ("bad own file", { r with own_file_ids = [ "UWRONG" ] });
      ("duplicate files", { r with own_file_ids = [ "FONE"; "FONE" ] });
      ( "wrong own identity",
        { r with identity = { identity with own_user_id = "UOTHER" } } );
      ("bad evidence", { r with evidence_digest = String.make 64 '0' });
    ];
  let m = inventory db o id [] in
  check "empty inventory needs explicit applicable scope" true
    (Result.is_error (I.validate_inventory db { m with orders = [] }));
  let reviewed = validated db m [] in
  ignore (I.apply db reviewed ~manifest_digest:(I.inventory_digest m) |> ok);
  check "reviewed complete empty inventory accepted" true
    (R.rollout_readiness db ~order_id:o.id = R.Ready R.Inventory_verified);
  ignore (Sqlite3.db_close db)

let test_byte_identical_preview_and_reopen () =
  let path = Filename.temp_file "clawq-receipts-" ".db" in
  let source, o, _ = legacy () in
  let db = Sqlite3.db_open path in
  (* Copy the finite fixture through SQLite's backup API, not a live DB. *)
  let backup =
    Sqlite3.Backup.init ~dst:db ~dst_name:"main" ~src:source ~src_name:"main"
  in
  ignore (Sqlite3.Backup.step backup (-1));
  ignore (Sqlite3.Backup.finish backup);
  ignore (Sqlite3.db_close source);
  let _, id, _, _ = R.read_anchor db in
  let r = receipt o id in
  let m = inventory db o id [ r ] in
  let read_file () =
    let ic = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in ic)
      (fun () -> really_input_string ic (in_channel_length ic))
  in
  let before = read_file () in
  let inputs = validated db m [ r ] in
  ignore (I.preview db inputs |> ok);
  check "preview byte-identical DB" true (before = read_file ());
  ignore (I.apply db inputs ~manifest_digest:(I.inventory_digest m) |> ok);
  ignore (Sqlite3.db_close db);
  let a = Sqlite3.db_open path and b = Sqlite3.db_open path in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sqlite3.db_close a);
      ignore (Sqlite3.db_close b);
      Sys.remove path)
    (fun () ->
      ignore (I.verify_rollout a ~manifest_digest:(I.inventory_digest m) |> ok);
      ignore (I.apply b inputs ~manifest_digest:(I.inventory_digest m) |> ok);
      Alcotest.(check int)
        "two handles after lost tool receipt" 1
        (count a "slack_standing_order_suppression");
      check "reopened decision" true
        (R.rollout_readiness b ~order_id:o.id = R.Ready R.Inventory_verified))

let test_automatic_conflict () =
  let db, o, id = legacy () in
  let r = receipt o id in
  let fire =
    S.fire db o ~room:r.room_id ~state:T.Posted ~dispatched_at:S.now
      ~relay_ts:r.relay_ts ()
  in
  Sql.exec db
    "UPDATE slack_standing_order_fires SET \
     root_ts=?,date_start=?,uploaded_file_ids=? WHERE id=?"
    Sql.
      [
        text r.root_ts;
        text r.date_start;
        text (Yojson.Safe.to_string (I.strings r.own_file_ids));
        text fire.id;
      ];
  (* Exact existing attempt identity is required; no nonce is invented. *)
  Sql.exec db
    "UPDATE slack_standing_order_dispatches SET uploaded_file_ids=? WHERE \
     fire_id=? AND attempt_no=0"
    Sql.
      [ text (Yojson.Safe.to_string (I.strings r.own_file_ids)); text fire.id ];
  Sql.exec db
    "INSERT INTO \
     slack_standing_order_dispatches(fire_id,attempt_no,uploaded_file_ids,relay_ts,dispatched_at) \
     VALUES(?,1,'[\"FSECOND\"]','1700000050.000001',?)"
    [ Sql.text fire.id; Sqlite3.Data.FLOAT S.now ];
  Sql.exec db
    "UPDATE slack_standing_order_fires SET \
     reposts=1,current_attempt=1,uploaded_file_ids='[\"FSECOND\"]',relay_ts='1700000050.000001' \
     WHERE id=?"
    [ Sql.text fire.id ];
  (* The other legitimate retained attempt is a different delivery of the same
     instance; only a collision or incompatible identity is a conflict. *)
  ignore (I.validate db ~receipt:r |> ok);
  let conflicting = { r with relay_ts = "1700000031.000001" } in
  let conflicting =
    { conflicting with evidence_digest = I.receipt_digest conflicting }
  in
  check "conflicting automatic evidence refused" true
    (I.validate db ~receipt:conflicting = Error I.Conflicting_delivery);
  check "conflict leaves pending" true
    (R.rollout_readiness db ~order_id:o.id = R.Denied R.Inventory_pending);
  let m = inventory db o id [ r ] in
  let inputs = validated db m [ r ] in
  check "preview links retained attempt" true
    (List.exists
       (function I.Link_automatic (_, id, 0) -> id = fire.id | _ -> false)
       (I.preview db inputs |> ok));
  let before = count db "slack_standing_order_dispatches" in
  ignore (I.apply db inputs ~manifest_digest:(I.inventory_digest m) |> ok);
  Alcotest.(check int)
    "linked delivery creates no attempt" before
    (count db "slack_standing_order_dispatches");
  Sql.exec db
    "UPDATE slack_standing_order_dispatches SET relay_ts='1700000999.000001' \
     WHERE fire_id=?"
    [ Sql.text fire.id ];
  check "stored automatic link must still match retained evidence" true
    (I.verify_rollout db ~manifest_digest:(I.inventory_digest m)
    = Error I.Invalid_proof);
  check "readiness denies a stale automatic link" true
    (R.rollout_readiness db ~order_id:o.id = R.Denied R.Invalid_proof);
  ignore (Sqlite3.db_close db)

let suite =
  suite
  @ [
      S.case "manual no-nonce delivery suppresses without executor debit"
        test_manual_import;
      S.case "partial inventory and completion failure roll back"
        test_partial_and_rollback;
      S.case "identity formats evidence and reviewed empty scope"
        test_validation;
      S.case "byte-identical preview reopen and two handles"
        test_byte_identical_preview_and_reopen;
      S.case "automatic identity conflict remains closed"
        test_automatic_conflict;
    ]

let test_used_origin_is_monotonic () =
  List.iter
    (fun state ->
      let source = S.fresh_db () in
      let o = S.seed source ~state () in
      let db = Sqlite3.db_open ":memory:" in
      Slack_standing_order_store_base.ensure_schema_tables db;
      ignore
        (Slack_standing_order_store_base.insert_order db
           { o with T.action = S.relay_action () });
      Store.ensure_schema db;
      check "every P3 state requires inventory" true
        (R.rollout_readiness db ~order_id:o.id = R.Denied R.Inventory_pending);
      Sql.exec db "DELETE FROM slack_standing_orders" [];
      Store.ensure_schema db;
      check "later empty rows never auto-freshen" true
        (R.rollout_readiness db ~order_id:o.id = R.Denied R.Inventory_pending);
      ignore (Sqlite3.db_close source);
      ignore (Sqlite3.db_close db))
    T.[ Pending_confirmation; Active; Paused; Suspended; Cancelled ]

let test_stored_receipt_corruption () =
  let db, o, id = legacy () in
  let r = receipt o id in
  let m = inventory db o id [ r ] in
  let digest = I.inventory_digest m in
  ignore (I.apply db (validated db m [ r ]) ~manifest_digest:digest |> ok);
  let trigger =
    Sql.query db
      "SELECT sql FROM sqlite_master WHERE \
       name='slack_standing_order_external_deliveries_no_update'"
      [] (fun s -> Sql_util.text_column s 0)
    |> List.hd
  in
  Sql.exec db "DROP TRIGGER slack_standing_order_external_deliveries_no_update"
    [];
  let corrupt = { r with own_file_ids = [ "FCORRUPT" ] } in
  Sql.exec db "UPDATE slack_standing_order_external_deliveries SET metadata=?"
    [ Sql.text (Yojson.Safe.to_string (I.receipt_json corrupt)) ];
  Sql.exec db trigger [];
  check "stored payload corruption denies verified inventory" true
    (I.verify_rollout db ~manifest_digest:digest = Error I.Invalid_proof);
  check "readiness rejects same corrupted payload" true
    (R.rollout_readiness db ~order_id:o.id = R.Denied R.Invalid_proof);
  check "corruption cannot remove suppression" true
    (R.suppressed db ~order_id:o.id ~root_ts:r.root_ts ~room_id:r.room_id
       ~date_start:r.date_start);
  ignore (Sqlite3.db_close db)

let suite =
  suite
  @ [
      S.case "all P3 states and later empty counts retain legacy origin"
        test_used_origin_is_monotonic;
      S.case "stored receipt payload digest is rechecked"
        test_stored_receipt_corruption;
    ]

let test_cross_instance_conflicts () =
  List.iter
    (fun (cross_order, same_relay) ->
      let db, a, installation = legacy () in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close db))
        (fun () ->
          let b =
            if cross_order then
              S.seed db ~target:{ S.claude with app_id = "AOTHER" } ()
            else a
          in
          let original = receipt a installation in
          let fire =
            S.fire db a ~room:original.room_id ~date_start:original.date_start
              ~state:T.Posted ~dispatched_at:S.now ~relay_ts:original.relay_ts
              ()
          in
          Sql.exec db
            "UPDATE slack_standing_order_fires SET \
             root_ts=?,uploaded_file_ids=? WHERE id=?"
            Sql.
              [
                text original.root_ts;
                text (Yojson.Safe.to_string (I.strings original.own_file_ids));
                text fire.id;
              ];
          Sql.exec db
            "UPDATE slack_standing_order_dispatches SET uploaded_file_ids=? \
             WHERE fire_id=?"
            Sql.
              [
                text (Yojson.Safe.to_string (I.strings original.own_file_ids));
                text fire.id;
              ];
          let proposed =
            {
              original with
              I.order_id = b.id;
              root_ts = "1700000002.000001";
              room_id = "ROTHER";
              date_start = "1700000002";
              relay_ts =
                (if same_relay then original.relay_ts else "1700000040.000001");
              own_file_ids =
                (if same_relay then [ "FDIFFERENT" ] else [ "FNOTES"; "FNEW" ]);
            }
          in
          let proposed =
            { proposed with I.evidence_digest = I.receipt_digest proposed }
          in
          check "cross-instance automatic conflict rejected" true
            (I.validate db ~receipt:proposed = Error I.Conflicting_delivery);
          let m = inventory db a installation [ proposed ] in
          let m =
            if cross_order then
              {
                m with
                I.orders =
                  m.orders
                  @ [
                      {
                        I.order_id = b.id;
                        epoch = b.epoch;
                        active_since = b.active_since;
                        activation_evidence = R.digest "reviewed-click";
                      };
                    ];
              }
            else m
          in
          (* Even an envelope validated before another handle records delivery must be rechecked in apply. *)
          let inputs = [ I.Complete_inventory m; I.Delivery proposed ] in
          check "atomic apply rejects conflict" true
            (I.apply db inputs ~manifest_digest:(I.inventory_digest m)
            = Error I.Conflicting_delivery);
          check "conflict stays pending" true
            (R.rollout_readiness db ~order_id:b.id
            = R.Denied R.Inventory_pending);
          Alcotest.(check int)
            "no partial suppression" 0
            (count db "slack_standing_order_suppression")))
    [ (false, true); (true, true); (false, false); (true, false) ]

let suite =
  suite
  @ [
      S.case "cross-root and cross-order relay or file overlap denies import"
        test_cross_instance_conflicts;
    ]

let add_automatic db (o : T.order) (r : I.external_receipt) ~root ~files =
  let f =
    S.fire db o ~room:"RCONFLICT" ~date_start:"1700000009" ~state:T.Posted
      ~dispatched_at:S.now ~relay_ts:r.relay_ts ()
  in
  Sql.exec db
    "UPDATE slack_standing_order_fires SET root_ts=?,uploaded_file_ids=? WHERE \
     id=?"
    Sql.[ text root; text (Yojson.Safe.to_string (I.strings files)); text f.id ];
  Sql.exec db
    "UPDATE slack_standing_order_dispatches SET uploaded_file_ids=? WHERE \
     fire_id=?"
    Sql.[ text (Yojson.Safe.to_string (I.strings files)); text f.id ];
  f

let test_conflict_race_and_reopen () =
  List.iter
    (fun late ->
      let source, o, installation = legacy () in
      let path = Filename.temp_file "clawq-import-conflicts-" ".db" in
      let a = Sqlite3.db_open path and b = Sqlite3.db_open path in
      Fun.protect
        ~finally:(fun () ->
          ignore (Sqlite3.db_close a);
          ignore (Sqlite3.db_close b);
          ignore (Sqlite3.db_close source);
          Sys.remove path)
        (fun () ->
          let backup =
            Sqlite3.Backup.init ~dst:a ~dst_name:"main" ~src:source
              ~src_name:"main"
          in
          ignore (Sqlite3.Backup.step backup (-1));
          ignore (Sqlite3.Backup.finish backup);
          let r = receipt o installation in
          let m = inventory a o installation [ r ] in
          let inputs = validated a m [ r ] and digest = I.inventory_digest m in
          if late then ignore (I.apply a inputs ~manifest_digest:digest |> ok);
          ignore
            (add_automatic b o r ~root:"1700000009.000001" ~files:[ "FNOTES" ]);
          if late then
            let reopened = Sqlite3.db_open path in
            Fun.protect
              ~finally:(fun () -> ignore (Sqlite3.db_close reopened))
              (fun () ->
                check "reopened verifier rejects late automatic conflict" true
                  (I.verify_rollout reopened ~manifest_digest:digest
                  = Error I.Invalid_proof);
                check "readiness rechecks same inventory conflict" true
                  (R.rollout_readiness reopened ~order_id:o.id
                  = R.Denied R.Invalid_proof);
                check "late conflict never removes suppression" true
                  (R.suppressed reopened ~order_id:o.id ~root_ts:r.root_ts
                     ~room_id:r.room_id ~date_start:r.date_start))
          else (
            check
              "apply rechecks a validated envelope after competing handle \
               writes"
              true
              (I.apply a inputs ~manifest_digest:digest
              = Error I.Conflicting_delivery);
            check "atomic race leaves pending" true
              (R.rollout_readiness a ~order_id:o.id
              = R.Denied R.Inventory_pending);
            Alcotest.(check int)
              "no partial delivery on race" 0
              (count a "slack_standing_order_external_deliveries"))))
    [ false; true ]

let test_malformed_automatic_inventory () =
  let db, o, installation = legacy () in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      let r = receipt o installation in
      let f =
        add_automatic db o r ~root:"1700000009.000001" ~files:[ "FNOTES" ]
      in
      Sql.exec db
        "UPDATE slack_standing_order_dispatches SET \
         uploaded_file_ids='malformed json' WHERE fire_id=?"
        [ Sql.text f.id ];
      check "raw malformed delivery cannot be skipped" true
        (I.validate db ~receipt:r = Error I.Conflicting_delivery);
      Sql.exec db
        "UPDATE slack_standing_order_dispatches SET \
         uploaded_file_ids='[\"FOTHER\"]' WHERE fire_id=?"
        [ Sql.text f.id ];
      Sql.exec db
        "UPDATE slack_standing_orders SET action_json='undecodable' WHERE id=?"
        [ Sql.text o.id ];
      check "undecodable order cannot hide its retained relay" true
        (I.validate db ~receipt:r = Error I.Conflicting_delivery))

let suite =
  suite
  @ [
      S.case
        "atomic import race and reopened verification inspect retained \
         deliveries"
        test_conflict_race_and_reopen;
      S.case "raw malformed or undecodable automatic inventory is never skipped"
        test_malformed_automatic_inventory;
    ]
