open Test_slack_standing_order_retention_support
open Lwt.Syntax
module Clock = Test_slack_standing_order_reconcile_support

let page = Clock.page

let with_disk work =
  let path = Filename.temp_file "clawq-retention" ".db" in
  let seed = S.fresh_db () in
  let o, f = fixture seed in
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
      work path db reopen manager o f)

let test_partial_crash () =
  with_disk (fun _ db reopen manager o f ->
      add_attempt !db f;
      let m = mock () and removed = ref [] and crash = ref true in
      let base = m.http in
      let http ~verb ~token ~method_ ~params =
        if method_ = "files.delete" then (
          let id = Yojson.Safe.Util.to_string (List.assoc "file" params) in
          check "write-ahead per-file attempt" true
            (List.assoc id (Cleanup.files !db f.id) = "attempted");
          if !crash && id = "FOWN1" then (
            removed := id :: !removed;
            crash := false;
            Lwt.fail Lwt.Canceled)
          else if List.mem id !removed then
            Lwt.return
              ( 200,
                Yojson.Safe.to_string
                  (obj [ ("ok", `Bool false); ("error", str "file_not_found") ])
              )
          else (
            removed := id :: !removed;
            base ~verb ~token ~method_ ~params))
        else base ~verb ~token ~method_ ~params
      in
      let cancelled =
        try
          Lwt_main.run
            (Retention.pass ~ctx:(context ()) ~db:!db ~manager:(manager ())
               ~http o);
          false
        with Lwt.Canceled -> true
      in
      check "actual crash after Slack side effect" true cancelled;
      Alcotest.(check string)
        "first file result committed" "removed"
        (List.assoc "FOWN0" (Cleanup.files !db f.id));
      Alcotest.(check string)
        "ambiguous second file durable" "attempted"
        (List.assoc "FOWN1" (Cleanup.files !db f.id));
      reopen ();
      (* Loss crosses the observation TTL; source proof and cleanup remain durable. *)
      S.at (S.now +. (31. *. 86400.));
      Slack_huddle_ledger.ensure_schema !db;
      ignore (Slack_huddle_ledger.prune ~db:!db ~now:(Store.now ()));
      Lwt_main.run
        (Retention.pass ~ctx:(context ()) ~db:!db ~manager:(manager ()) ~http o);
      check "outstanding cleanup closes after reopen/day31" true
        (complete !db f.id);
      eq "one side effect per own file" 3
        (List.length (List.sort_uniq String.compare !removed));
      eq "never repeat confirmed first delete" 1
        (List.length (List.filter (( = ) "FOWN0") (deleted m)));
      eq "one fixed note per relay after recovery" 2
        (count m "chat.postMessage"))

let own_note job thread ts =
  obj
    ([
       ("ts", str ts);
       ("thread_ts", str thread);
       ("user", str "UBOT");
       ("bot_id", str "BOWN");
     ]
    @ Scope.note_params ~channel:"C123" ~job ~thread)

let test_note_crash ~spoof ~incomplete () =
  with_disk (fun _ db reopen manager o f ->
      let m = mock () and posted = ref [] in
      let base = m.http in
      let crash = ref true in
      let http ~verb ~token ~method_ ~params =
        if method_ = "chat.postMessage" && !crash then (
          crash := false;
          check "note attempted before network" true
            (List.assoc "1699999940.000001" (Cleanup.threads !db f.id)
            = "attempted");
          posted := [ own_note f.id "1699999940.000001" "1699999999.000001" ];
          Lwt.fail Lwt.Canceled)
        else if method_ = "conversations.replies" then
          let cursor =
            Option.value (List.assoc_opt "cursor" params) ~default:(str "")
          in
          let notes =
            if spoof then List.map (set "bot_id" (str "BOTHER")) !posted
            else !posted
          in
          let json =
            if incomplete then set "has_more" (`Bool true) (page notes)
            else if cursor = str "" then
              page notes
              |> set "has_more" (`Bool true)
              |> set "response_metadata"
                   (obj [ ("next_cursor", str "next-note-page") ])
            else page []
          in
          base ~verb ~token ~method_ ~params
          |> Lwt.map (fun _ -> (200, Yojson.Safe.to_string json))
        else base ~verb ~token ~method_ ~params
      in
      S.override m "conversations.replies" (fun _ -> (200, page []));
      (try
         Lwt_main.run
           (Retention.pass ~ctx:(context ()) ~db:!db ~manager:(manager ()) ~http
              o)
       with Lwt.Canceled -> ());
      eq "server owns exactly one note despite client loss" 1
        (List.length !posted);
      reopen ();
      S.at (S.now +. 300.);
      Lwt_main.run
        (Retention.pass ~ctx:(context ()) ~db:!db ~manager:(manager ()) ~http o);
      check "only complete own identity adoption confirms"
        ((not spoof) && not incomplete)
        (complete !db f.id);
      eq "no blind repost after ambiguous outcome" 0
        (count m "chat.postMessage");
      eq "original actual note side effects" 1 (List.length !posted);
      S.at (S.now +. 600.);
      Lwt_main.run
        (Retention.pass ~ctx:(context ()) ~db:!db ~manager:(manager ()) ~http o);
      eq "still never blindly reposts" 1 (List.length !posted))

let test_note_ambiguity () =
  with_fixture (fun db o f m mgr ->
      S.override m "chat.postMessage" (fun _ -> (503, obj []));
      run db o m mgr;
      eq "initial ambiguous attempt" 1 (count m "chat.postMessage");
      S.override m "conversations.replies" (fun _ -> (200, page []));
      S.at (S.now +. 300.);
      run db o m mgr;
      check "absent note stays unknown" false (complete db f.id);
      let status =
        Cleanup.status_json db ~order_id:o.id
        |> Yojson.Safe.Util.to_list |> List.hd
        |> Yojson.Safe.Util.member "notes"
        |> Yojson.Safe.Util.to_list
      in
      check "unknown cleanup note inspectable through status" true
        (List.exists
           (fun note ->
             Yojson.Safe.Util.member "status" note = `String "unknown")
           status);
      S.override m "conversations.replies" (fun _ ->
          ( 200,
            page
              [
                own_note f.id "1699999940.000001" "1699999998.000001";
                own_note f.id "1699999940.000001" "1699999999.000001";
              ] ));
      S.at (S.now +. 600.);
      run db o m mgr;
      check "two exact matches remain ambiguous" false (complete db f.id);
      eq "503 never causes automatic second note" 1 (count m "chat.postMessage"))

let test_wait_revocation change () =
  with_fixture (fun db o f m mgr ->
      let calls = ref 0 in
      let ctx =
        {
          (context ()) with
          Ctx.limit =
            (fun ~channel_id:_ ->
              incr calls;
              if !calls = 6 then
                Session.update_config mgr (change (Session.get_config mgr));
              Lwt.return_unit);
        }
      in
      run ~ctx db o m mgr;
      no_writes m;
      check "viable source deletion evidence existed before wait" true
        (Option.get (Cleanup.find db f.id)).missing;
      eq "actual deletion dispatch never reached" 0 (count m "files.delete");
      Session.update_config mgr (S.config ());
      S.at (S.now +. 300.);
      run db o m mgr;
      check "same fixture succeeds when permitted" true (complete db f.id))

let test_two_owners () =
  with_disk (fun path db _ manager o f ->
      let second = Sqlite3.db_open path in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close second))
        (fun () ->
          let m = mock () and calls = ref 0 and successor = ref None in
          let ctx =
            {
              (context ()) with
              Ctx.limit =
                (fun ~channel_id:_ ->
                  incr calls;
                  if !calls = 6 then (
                    check "other handle cannot claim live owner" true
                      (Cleanup.claim second ~id:f.id ~now:S.now = None);
                    S.at (S.now +. 301.);
                    successor :=
                      Cleanup.claim second ~id:f.id ~now:(Store.now ());
                    check "expired lease transferred independently" true
                      (!successor <> None));
                  Lwt.return_unit);
            }
          in
          run ~ctx !db o m (manager ());
          no_writes m;
          let owner = Option.get !successor in
          check "new owner still owns after stale finalizer" true
            (Cleanup.owns second ~job:f.id ~owner ~now:(Store.now ()));
          Cleanup.release second ~job:f.id ~owner ~now:(Store.now ());
          run !db o m (manager ());
          check "new owner completes cleanly" true (complete !db f.id)))

let test_real_limiter_wait () =
  with_fixture (fun db o f m mgr ->
      let clock : Clock.clock = { now = S.now; timers = [] } in
      (Store.clock := fun () -> clock.now);
      let limiter =
        Slack_read_limiter.create_with
          ~now:(fun () -> clock.now)
          ~wait:(Clock.wait clock)
      in
      let key = SO.method_key ~installation:"T123:A123" "files.delete" in
      Slack_read_limiter.note_retry_after limiter ~key 150.;
      let ctx =
        {
          (context ()) with
          Ctx.limiter = Some limiter;
          now = (fun () -> clock.now);
          wait = Clock.wait clock;
        }
      in
      Lwt_main.run
        (let pending = Retention.pass ~ctx ~db ~manager:mgr ~http:m.http o in
         let* () = Clock.advance clock (S.now +. 125.) in
         eq "delete still behind Retry-After" 0 (count m "files.delete");
         check "owner renewed through real limiter wait" true
           (Sql.int_query db
              "SELECT count(*) FROM slack_order_cleanup_jobs WHERE id=? AND \
               until>?"
              Sql.[ text f.id; Sqlite3.Data.FLOAT clock.now ]
           = 1);
         let* () = Clock.drain clock pending in
         check "eventual deletion succeeds" true (complete db f.id);
         let posts =
           !(m.calls)
           |> List.filter (fun (_, method_, _) -> method_ = "chat.postMessage")
         in
         eq "one paced cleanup note" 1 (List.length posts);
         Lwt.return_unit))

let test_sources_verified_fetch () =
  let module E = Test_slack_standing_order_exec_support in
  E.with_env (fun e ->
      E.enable_transcript e;
      E.seen e;
      E.ready e ();
      E.resolve e;
      let f = E.one_fire e in
      let rows () =
        Sql.query e.db
          "SELECT source_canvas_id,source_transcript_id FROM \
           slack_standing_order_fires WHERE id=?"
          [ Sql.text f.id ]
          (fun s ->
            (Sql_util.opt_text_column s 0, Sql_util.opt_text_column s 1))
      in
      check "hints do not prove source IDs" true (rows () = [ (None, None) ]);
      S.at (S.now +. 100.);
      check "real verified fetch succeeds" true (E.fetch e f.id <> None);
      check "verified canvas and transcript durable" true
        (rows () = [ (Some "FCANVAS", Some "FTRANS") ]);
      ignore
        (Slack_huddle_ledger.prune ~db:e.db ~now:(S.now +. (31. *. 86400.)));
      check "source proof independent of observation TTL" true
        (rows () = [ (Some "FCANVAS", Some "FTRANS") ]))

let test_external complete_proof () =
  let module I = Slack_standing_order_receipt_import in
  let module F = Test_slack_standing_order_receipt_import in
  let db, o, id = F.legacy () in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      Access_snapshot.init_schema db;
      Admin.init_schema db;
      Egress_audit.init_schema db;
      let r = F.receipt o id in
      let r =
        {
          r with
          I.relay_ts = "1699999940.000001";
          source_canvas_id = (if complete_proof then Some "FSOURCE" else None);
          source_transcript_id =
            (if complete_proof then Some "FTRANS" else None);
        }
      in
      let r = { r with I.evidence_digest = I.receipt_digest r } in
      let manifest = F.inventory db o id [ r ] in
      ignore
        (F.ok
           (I.apply db
              (F.validated db manifest [ r ])
              ~manifest_digest:(I.inventory_digest manifest)));
      let m = mock () in
      let manager = Session.create ~config:(S.config ()) ~db () in
      run db o m manager;
      eq "external source proof gates cleanup"
        (if complete_proof then 2 else 0)
        (count m "files.delete");
      eq "import never fabricates executor attempts" 0
        (Sql.int_query db "SELECT count(*) FROM slack_standing_order_dispatches"
           []);
      check "suppression is retained" true
        (Slack_standing_order_rollout_store.suppressed db ~order_id:o.id
           ~root_ts:r.root_ts ~room_id:r.room_id ~date_start:r.date_start))

let suite =
  [
    S.case "partial two-attempt deletion crash disk reopen day31"
      test_partial_crash;
    S.case "lost note receipt own-bot adoption across disk reopen"
      (test_note_crash ~spoof:false ~incomplete:false);
    S.case "spoofed cleanup marker is not adoption"
      (test_note_crash ~spoof:true ~incomplete:false);
    S.case "incomplete cleanup note scan stays held"
      (test_note_crash ~spoof:false ~incomplete:true);
    S.case "503 absent duplicate note stays unknown no repost"
      test_note_ambiguity;
    S.case "capability revoked during delete wait"
      (test_wait_revocation (fun cfg ->
           S.profile cfg ~denied:[ "slack_huddle_content" ] ()));
    S.case "egress revoked during delete wait"
      (test_wait_revocation (fun cfg ->
           { cfg with egress = (S.config ~strict_rules:[] ()).egress }));
    S.case "token rotated during delete wait"
      (test_wait_revocation (fun cfg ->
           S.with_slack cfg (fun sc -> { sc with bot_token = "xoxb-rotated" })));
    S.case "two real handles expired owner cannot delete or note"
      test_two_owners;
    S.case "shared Retry-After renews cleanup owner" test_real_limiter_wait;
    S.case "verified fetch persists source proof beyond ledger TTL"
      test_sources_verified_fetch;
    S.case "external delivery complete own/source proof cleanup"
      (test_external true);
    S.case "external delivery absent source proof zero cleanup"
      (test_external false);
  ]

let test_cancelled_start () =
  Clock.with_env (fun e ->
      Clock.with_transport e (fun () ->
          let o = e.order in
          let f =
            S.fire e.f.db o ~state:T.Picked_up ~room:"RCANCELLED"
              ~date_start:"1700000001" ~dispatched_at:(S.now +. 20.)
              ~relay_ts:"1700000020.000001" ()
          in
          Store.save_attempt e.f.db f.id
            {
              (Store.empty_attempt 0) with
              uploaded_file_ids = [ "FOWN0" ];
              dispatched_at = Some (S.now +. 20.);
              relay_ts = f.relay_ts;
            };
          Sql.exec e.f.db
            "UPDATE slack_standing_order_fires SET source_canvas_id='FSOURCE' \
             WHERE id=?"
            [ Sql.text f.id ];
          ignore
            (Result.get_ok
               (Store.deactivate e.f.db ~order_id:o.id (Store.Cancel "U123")
                  ~now:e.clock.now));
          let original = !(e.answer) in
          (e.answer :=
             fun method_ params ->
               match method_ with
               | "files.info" ->
                   Lwt.return
                     ( 200,
                       obj
                         [
                           ("ok", `Bool false); ("error", str "file_not_found");
                         ] )
               | "files.delete" -> Lwt.return (200, S.ok [])
               | _ -> original method_ params);
          Reconcile.start ~db:e.f.db ~manager:e.f.base.manager;
          let* () = Clock.advance e.clock (e.clock.now +. 25.) in
          eq "startup services cancelled order" 1 (Clock.count e "files.delete");
          eq "startup note remains a thread note" 1
            (Clock.count e "chat.postMessage");
          check "cancelled startup completed cleanup" true
            (complete e.f.db f.id);
          Lwt.return_unit))

let test_shared_wait_revocation change () =
  with_fixture (fun db o f m mgr ->
      let clock : Clock.clock = { now = S.now; timers = [] } in
      (Store.clock := fun () -> clock.now);
      let limiter =
        Slack_read_limiter.create_with
          ~now:(fun () -> clock.now)
          ~wait:(Clock.wait clock)
      in
      Slack_read_limiter.note_retry_after limiter
        ~key:(SO.method_key ~installation:"T123:A123" "files.delete")
        150.;
      let ctx =
        {
          (context ()) with
          Ctx.limiter = Some limiter;
          now = (fun () -> clock.now);
          wait = Clock.wait clock;
        }
      in
      Lwt_main.run
        (let pending = Retention.pass ~ctx ~db ~manager:mgr ~http:m.http o in
         let* () = Clock.advance clock (S.now +. 125.) in
         eq "real delete limiter reached" 0 (count m "files.delete");
         check "source missing already durable" true
           (Option.get (Cleanup.find db f.id)).missing;
         Session.update_config mgr (change (Session.get_config mgr));
         let* () = Clock.drain clock pending in
         no_writes m;
         Session.update_config mgr (S.config ());
         let* () = Clock.advance clock (clock.now +. 300.) in
         let retry = Retention.pass ~ctx ~db ~manager:mgr ~http:m.http o in
         let* () = Clock.drain clock retry in
         check "same real limiter positive control completes" true
           (complete db f.id);
         Lwt.return_unit))

let test_note_pages () =
  with_fixture (fun db o f m mgr ->
      S.override m "chat.postMessage" (fun _ -> (503, obj []));
      run db o m mgr;
      let cursors = ref [] in
      S.override m "conversations.replies" (fun params ->
          let cursor = S.param params "cursor" in
          cursors := !cursors @ [ cursor ];
          let n = if cursor = "" then 0 else int_of_string cursor in
          let messages =
            if n = 24 then
              [ own_note f.id "1699999940.000001" "1699999999.000001" ]
            else []
          in
          ( 200,
            if n = 24 then page messages
            else
              page messages
              |> set "has_more" (`Bool true)
              |> set "response_metadata"
                   (obj [ ("next_cursor", str (string_of_int (n + 1))) ]) ));
      for n = 1 to 5 do
        S.at (S.now +. float_of_int (n * 300));
        run db o m mgr
      done;
      check "full bounded multi-slice scan adopted" true (complete db f.id);
      eq "durable page cursor avoids rereads" 25 (List.length !cursors);
      eq "every cursor visited once" 25
        (List.length (List.sort_uniq String.compare !cursors));
      eq "no new note across scans" 1 (count m "chat.postMessage"))

let test_source_transport () =
  with_fixture (fun db o _ m mgr ->
      let http ~verb ~token ~method_ ~params =
        if method_ = "files.info" then Lwt.fail (Failure "synthetic transport")
        else m.http ~verb ~token ~method_ ~params
      in
      Lwt_main.run (Retention.pass ~ctx:(context ()) ~db ~manager:mgr ~http o);
      no_writes m)

let suite =
  suite
  @ [
      S.case "actual reconciler startup includes cancelled deliveries"
        test_cancelled_start;
      S.case "real shared limiter capability revocation and retry"
        (test_shared_wait_revocation (fun cfg ->
             S.profile cfg ~denied:[ "slack_huddle_content" ] ()));
      S.case "real shared limiter egress revocation and retry"
        (test_shared_wait_revocation (fun cfg ->
             { cfg with egress = (S.config ~strict_rules:[] ()).egress }));
      S.case "durable cleanup note cursor across bounded slices" test_note_pages;
      S.case "source transport failure never deletes" test_source_transport;
    ]

let test_late_relay_identity () =
  with_fixture (fun db o f m mgr ->
      let a =
        {
          (Store.empty_attempt 0) with
          uploaded_file_ids = [ "FOWN0" ];
          dispatched_at = Some (S.now -. 60.);
        }
      in
      Store.save_attempt db f.id a;
      run db o m mgr;
      eq "dispatched own files cleaned" 1 (count m "files.delete");
      eq "unknown relay cannot be given a guessed note thread" 0
        (count m "chat.postMessage");
      check "outstanding note retained until actual identity arrives" false
        (complete db f.id);
      S.at (S.now +. (31. *. 86400.));
      Store.save_attempt db f.id { a with relay_ts = Some "1699999940.000001" };
      run db o m mgr;
      eq "late confirmed relay gets its fixed note after TTL" 1
        (count m "chat.postMessage");
      check "late identity closes retained cleanup" true (complete db f.id))

let suite =
  suite
  @ [
      S.case "late dispatched relay identity retained beyond TTL"
        test_late_relay_identity;
    ]

let test_normalized_note () =
  with_fixture (fun db o f _ _ ->
      Cleanup.refresh db o ~now:S.now;
      let job = Option.get (Cleanup.find db f.id) in
      let original = own_note f.id "1699999940.000001" "1699999999.000001" in
      check "unchanged exact own-note control" true
        (Scope.matches ~job ~channel:o.channel_id ~thread:"1699999940.000001"
           original);
      let blocks =
        Yojson.Safe.Util.member "blocks" original |> Yojson.Safe.Util.to_list
      in
      let block = List.hd blocks in
      let text = Yojson.Safe.Util.member "text" block in
      let normalized = block |> set "text" (set "emoji" (`Bool true) text) in
      let normalized =
        match normalized with
        | `Assoc fields -> `Assoc (List.rev fields)
        | _ -> assert false
      in
      let returned = set "blocks" (`List [ normalized ]) original in
      check "Slack-normalized own marker/text still adopts" true
        (Scope.matches ~job ~channel:o.channel_id ~thread:"1699999940.000001"
           returned);
      check "normalization never bypasses own-bot identity" false
        (Scope.matches ~job ~channel:o.channel_id ~thread:"1699999940.000001"
           (set "bot_id" (str "BOTHER") returned)))

let suite =
  suite
  @ [
      S.case "cleanup note normalization preserves own marker adoption"
        test_normalized_note;
    ]
