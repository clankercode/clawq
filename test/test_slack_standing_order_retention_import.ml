open Test_slack_standing_order_retention_support
module Import = Slack_standing_order_receipt_import
module Import_fixture = Test_slack_standing_order_receipt_import

let linked_fixture ?(second = true) ?(proof = true) ?(canvas = "FSOURCE") db o
    installation =
  Access_snapshot.init_schema db;
  Admin.init_schema db;
  Egress_audit.init_schema db;
  S.at (S.now +. 1000.);
  let r = Import_fixture.receipt o installation in
  let r =
    {
      r with
      Import.relay_ts = "1700000060.000001";
      source_canvas_id = (if proof then Some canvas else None);
      source_transcript_id = (if proof then Some "FTRANS" else None);
    }
  in
  let r = { r with Import.evidence_digest = Import.receipt_digest r } in
  let fire =
    S.fire db o ~room:r.room_id ~date_start:r.date_start ~state:T.Picked_up
      ~dispatched_at:(S.now +. 60.) ~relay_ts:r.relay_ts ()
  in
  Sql.exec db "UPDATE slack_standing_order_fires SET root_ts=? WHERE id=?"
    Sql.[ text r.root_ts; text fire.id ];
  Store.save_attempt db fire.id
    {
      (Store.empty_attempt 0) with
      uploaded_file_ids = r.own_file_ids;
      dispatched_at = Some (S.now +. 60.);
      relay_ts = Some r.relay_ts;
      pickup_started_at = Some (S.now +. 60.000001);
    };
  if second then (
    Store.save_attempt db fire.id
      {
        (Store.empty_attempt 1) with
        uploaded_file_ids = [ "FSECOND" ];
        dispatched_at = Some (S.now +. 90.);
        relay_ts = Some "1700000090.000001";
        pickup_started_at = Some (S.now +. 90.000001);
      };
    Sql.exec db
      "UPDATE slack_standing_order_fires SET \
       reposts=1,current_attempt=1,relay_ts='1700000090.000001',uploaded_file_ids='[\"FSECOND\"]' \
       WHERE id=?"
      [ Sql.text fire.id ]);
  let manifest = Import_fixture.inventory db o installation [ r ] in
  let inputs = Import_fixture.validated db manifest [ r ] in
  check "validated preview links exact retained attempt" true
    (List.exists
       (function
         | Import.Link_automatic (_, id, 0) -> id = fire.id | _ -> false)
       (Import_fixture.ok (Import.preview db inputs)));
  ignore
    (Import_fixture.ok
       (Import.apply db inputs
          ~manifest_digest:(Import.inventory_digest manifest)));
  let o =
    (Result.get_ok
       (Store.deactivate db ~order_id:o.id (Store.Pause "U123")
          ~now:(Store.now ())))
      .after
  in
  (o, fire, r)

let test_linked_two_attempts () =
  let db, o, installation = Import_fixture.legacy () in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      let o, fire, r = linked_fixture db o installation in
      check "legacy dedicated source columns are NULL" true
        (Sql.int_query db
           "SELECT count(*) FROM slack_standing_order_fires WHERE id=? AND \
            source_canvas_id IS NULL AND source_transcript_id IS NULL"
           [ Sql.text fire.id ]
        = 1);
      let retained () =
        Sql.query db
          ("SELECT " ^ Store.attempt_columns
         ^ " FROM slack_standing_order_dispatches WHERE fire_id=? ORDER BY \
            attempt_no")
          [ Sql.text fire.id ]
          Store.decode_attempt
      in
      let before = retained () in
      let m = mock () in
      let manager = Session.create ~config:(S.config ()) ~db () in
      run db o m manager;
      Printf.printf "LINKED_FIX deleted=%s notes=%d complete=%b\n%!"
        (String.concat "," (deleted m))
        (count m "chat.postMessage")
        (complete db fire.id);
      check "validated receipt files were actually deleted" true
        (List.for_all (fun id -> List.mem id (deleted m)) r.own_file_ids);
      check "linked source proof covers second retained attempt" true
        (List.mem "FSECOND" (deleted m));
      eq "both actual relay threads receive fixed notes" 2
        (count m "chat.postMessage");
      check "all linked obligations complete" true (complete db fire.id);
      check "both retained attempt identities are unchanged" true
        (retained () = before);
      let noted =
        !(m.S.calls)
        |> List.filter_map (fun (_, method_, p) ->
            if method_ = "chat.postMessage" then Some (S.param p "thread_ts")
            else None)
      in
      Alcotest.(check (list string))
        "only both actual relay threads are noted"
        [ "1700000060.000001"; "1700000090.000001" ]
        (List.sort String.compare noted);
      Alcotest.(check (list string))
        "only the three own files are deleted"
        [ "FNOTES"; "FSECOND"; "FTRANSCRIPT" ]
        (List.sort String.compare (deleted m)))

let suite =
  [
    S.case "linked NULL-source import cleans both dispatched attempts"
      test_linked_two_attempts;
  ]

let with_linked ?second ?proof ?canvas work =
  let db, o, installation = Import_fixture.legacy () in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      let o, fire, r =
        linked_fixture ?second ?proof ?canvas db o installation
      in
      let m = mock () in
      let manager = Session.create ~config:(S.config ()) ~db () in
      work db o fire r m manager)

let assert_idempotent db o (fire : T.fire) m manager ~files ~notes =
  check "complete after all proven obligations" true (complete db fire.T.id);
  eq "exact own-file delete count" files (count m "files.delete");
  eq "exact fixed-note count" notes (count m "chat.postMessage");
  let before = List.length !(m.S.calls) in
  run db o m manager;
  S.at (Store.now () +. 300.);
  run db o m manager;
  eq "completed job repeated passes send nothing" before
    (List.length !(m.S.calls))

let test_linked_one () =
  with_linked ~second:false (fun db o fire _ m manager ->
      run db o m manager;
      assert_idempotent db o fire m manager ~files:2 ~notes:1;
      eq "one retained attempt not fabricated" 1
        (Sql.int_query db
           "SELECT count(*) FROM slack_standing_order_dispatches WHERE \
            fire_id=?"
           [ Sql.text fire.id ]))

let test_dedicated () =
  with_linked ~proof:false (fun db o fire _ m manager ->
      Sql.exec db
        "UPDATE slack_standing_order_fires SET \
         source_canvas_id='FSOURCE',source_transcript_id='FTRANS' WHERE id=?"
        [ Sql.text fire.id ];
      run db o m manager;
      check "dedicated proof independently covers second attempt" true
        (List.mem "FSECOND" (deleted m));
      assert_idempotent db o fire m manager ~files:3 ~notes:2)

let test_incomplete () =
  with_linked ~proof:false (fun db o fire _ m manager ->
      run db o m manager;
      no_writes m;
      check "no cleanup job without source association" true
        (Cleanup.find db fire.id = None);
      eq "no HTTP for incomplete proof" 0 (List.length !(m.S.calls)))

let test_source_overlap canvas () =
  with_linked ~canvas (fun db o fire _ m manager ->
      run db o m manager;
      no_writes m;
      check "neither attempt file can be treated as source" true
        (Cleanup.find db fire.id = None))

let test_conflicting_dedicated () =
  with_linked (fun db o fire _ m manager ->
      Sql.exec db
        "UPDATE slack_standing_order_fires SET source_canvas_id='FVERIFIED' \
         WHERE id=?"
        [ Sql.text fire.id ];
      S.override m "files.info" (fun p ->
          check "receipt does not replace conflicting dedicated source" true
            (S.param p "file" = "FVERIFIED");
          (200, S.ok [ ("file", obj [ ("id", str "FVERIFIED") ]) ]));
      run db o m manager;
      no_writes m;
      check "dedicated job remains watching" true
        ((Option.get (Cleanup.find db fire.id)).canvas = "FVERIFIED"))

let retain_second db (fire : T.fire) ~files ~relay =
  Store.save_attempt db fire.id
    {
      (Store.empty_attempt 1) with
      uploaded_file_ids = files;
      dispatched_at = Some (S.now +. 90.);
      relay_ts = relay;
    };
  Sql.exec db
    "UPDATE slack_standing_order_fires SET uploaded_file_ids=?,relay_ts=? \
     WHERE id=?"
    Sql.
      [
        text (Cleanup.json_ids files); Sqlite3.Data.opt_text relay; text fire.id;
      ]

let with_linked_disk work =
  let seed, o, installation = Import_fixture.legacy () in
  let o, fire, r = linked_fixture seed o installation in
  let path = Filename.temp_file "clawq-retention-import" ".db" in
  Sql.exec seed "VACUUM INTO ?" [ Sql.text path ];
  ignore (Sqlite3.db_close seed);
  let db = ref (Sqlite3.db_open path) in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sqlite3.db_close !db);
      Sys.remove path)
    (fun () ->
      let reopen () =
        ignore (Sqlite3.db_close !db);
        db := Sqlite3.db_open path
      in
      let manager () = Session.create ~config:(S.config ()) ~db:!db () in
      work path db reopen manager o fire r)

let test_completed_extension late_files () =
  with_linked_disk (fun path db reopen manager o fire _ ->
      (* The second dispatch is retained before cleanup. Its completion identity
       arrives later, as it does after an ambiguous complete/adoption. *)
      retain_second !db fire
        ~files:(if late_files then [] else [ "FSECOND" ])
        ~relay:None;
      let m = mock () in
      run !db o m (manager ());
      eq "only actual first thread can be noted" 1 (count m "chat.postMessage");
      check "existing job has completed known obligations" true
        (complete !db fire.id);
      let original_notes = Cleanup.threads !db fire.id in
      reopen ();
      S.at (S.now +. (31. *. 86400.));
      retain_second !db fire ~files:[ "FSECOND" ]
        ~relay:(Some "1700000090.000001");
      Cleanup.refresh !db o ~now:(Store.now ());
      check "missing-source job reopens beyond monitoring TTL" false
        (complete !db fire.id);
      check "first confirmed note preserved" true
        (List.for_all
           (fun row -> List.mem row (Cleanup.threads !db fire.id))
           original_notes);
      let owner =
        Option.get (Cleanup.claim !db ~id:fire.id ~now:(Store.now ()))
      in
      let other = Sqlite3.db_open path in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close other))
        (fun () ->
          Cleanup.refresh other o ~now:(Store.now ());
          check "refresh preserves independent owner CAS" true
            (Cleanup.owns !db ~job:fire.id ~owner ~now:(Store.now ()));
          let before = List.length !(m.S.calls) in
          run other o m (Session.create ~config:(S.config ()) ~db:other ());
          eq "another handle cannot send under current owner" before
            (List.length !(m.S.calls)));
      Cleanup.release !db ~job:fire.id ~owner ~now:(Store.now ());
      run !db o m (manager ());
      assert_idempotent !db o fire m (manager ()) ~files:3 ~notes:2;
      eq "no source repoll after missing proof" 1 (count m "files.info");
      reopen ();
      assert_idempotent !db o fire m (manager ()) ~files:3 ~notes:2)

let test_incomplete_extension () =
  with_linked_disk (fun _ db reopen manager o fire _ ->
      retain_second !db fire ~files:[ "FSECOND" ] ~relay:None;
      let m = mock () in
      S.override m "files.delete" (fun p ->
          if S.param p "file" = "FSECOND" then (503, obj []) else (200, S.ok []));
      run !db o m (manager ());
      eq "receipt files cleaned before retry" 3 (count m "files.delete");
      eq "no note before all files removed" 0 (count m "chat.postMessage");
      check "partial job stays incomplete" false (complete !db fire.id);
      reopen ();
      S.at (S.now +. (31. *. 86400.));
      retain_second !db fire ~files:[ "FSECOND" ]
        ~relay:(Some "1700000090.000001");
      S.override m "files.delete" (fun p ->
          check "only failed second file is retried" true
            (S.param p "file" = "FSECOND");
          (200, S.ok []));
      run !db o m (manager ());
      assert_idempotent !db o fire m (manager ()) ~files:4 ~notes:2)

let test_scope_identity_refusals () =
  let db, o, installation = Import_fixture.legacy () in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      let r = Import_fixture.receipt o installation in
      List.iter
        (fun r ->
          let r = { r with Import.evidence_digest = Import.receipt_digest r } in
          check
            "source receipt cannot change installation scope or own identity"
            true
            (Result.is_error (Import.validate db ~receipt:r)))
        [
          { r with installation = "other"; source_canvas_id = Some "FSOURCE" };
          { r with channel_id = "COTHER"; source_canvas_id = Some "FSOURCE" };
          {
            r with
            identity = { r.identity with own_bot_id = "BOTHER" };
            source_canvas_id = Some "FSOURCE";
          };
        ];
      eq "rejected receipts leave no delivery" 0
        (Import_fixture.count db "slack_standing_order_external_deliveries"))

let suite =
  suite
  @ [
      S.case "linked one-attempt cleanup stays idempotent" test_linked_one;
      S.case "dedicated automatic proof covers both without receipt sources"
        test_dedicated;
      S.case "linked incomplete source proof refuses cleanup" test_incomplete;
      S.case "receipt source cannot be its own file"
        (test_source_overlap "FNOTES");
      S.case "receipt source cannot be other retained attempt file"
        (test_source_overlap "FSECOND");
      S.case "conflicting dedicated source remains conservative"
        test_conflicting_dedicated;
      S.case "complete job extends late second identity across disk reopen"
        (fun () -> test_completed_extension true);
      S.case "complete job adds late second thread without duplicate notes"
        (fun () -> test_completed_extension false);
      S.case "incomplete job extends late thread beyond TTL across disk reopen"
        test_incomplete_extension;
      S.case "source receipts retain scope and own identity validation"
        test_scope_identity_refusals;
    ]

let test_linked_window days () =
  with_linked (fun db o fire _ m manager ->
      S.at (S.now +. (days *. 86400.));
      run db o m manager;
      if days > 30. then (
        no_writes m;
        check "old relay creates no monitoring job" true
          (Cleanup.find db fire.id = None);
        eq "no source HTTP outside monitoring window" 0 (count m "files.info"))
      else assert_idempotent db o fire m manager ~files:3 ~notes:2)

let test_linked_watch_expiry () =
  with_linked (fun db o fire _ m manager ->
      S.override m "files.info" (fun p ->
          (200, S.ok [ ("file", obj [ ("id", str (S.param p "file")) ]) ]));
      run db o m manager;
      eq "both present proven sources polled" 2 (count m "files.info");
      S.at (S.now +. (31. *. 86400.));
      run db o m manager;
      no_writes m;
      eq "expired linked watch is not repolled" 2 (count m "files.info");
      check "present-source job expires rather than reopens" true
        ((Option.get (Cleanup.find db fire.id)).status = "expired"))

let test_old_receipt_recent_second () =
  with_linked (fun db o fire _ m manager ->
      Store.save_attempt db fire.id
        {
          (Store.empty_attempt 1) with
          uploaded_file_ids = [ "FSECOND" ];
          dispatched_at = Some (S.now +. (2. *. 86400.));
          relay_ts = Some "1700172800.000001";
        };
      Sql.exec db
        "UPDATE slack_standing_order_fires SET relay_ts='1700172800.000001' \
         WHERE id=?"
        [ Sql.text fire.id ];
      S.at (S.now +. (31. *. 86400.));
      run db o m manager;
      check
        "recent second relay selects exact linked fire despite older receipt"
        true
        (List.mem "FSECOND" (deleted m));
      assert_idempotent db o fire m manager ~files:3 ~notes:2)

let suite =
  suite
  @ [
      S.case "linked day29 relay selection" (test_linked_window 29.);
      S.case "linked day31 relay exclusion" (test_linked_window 31.);
      S.case "linked present-source monitoring expires" test_linked_watch_expiry;
      S.case "recent second attempt selects fire with older receipt"
        test_old_receipt_recent_second;
    ]
