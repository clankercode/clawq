module S = Test_slack_standing_order_support
module T = Slack_standing_order_types
module Store = Slack_standing_order_store

let ok = function Ok v -> v | Error e -> Alcotest.fail e
let some = function Some v -> v | None -> Alcotest.fail "expected Some"
let check = Alcotest.(check bool)
let integer = Alcotest.(check int)
let state = Alcotest.check S.fire_state_t
let runtime db id = some (Store.runtime db ~fire_id:id)
let claim db id owner now = some (ok (Store.claim db ~fire_id:id ~owner ~now))

let announce db (o : T.order) =
  let c =
    Store.insert_card db ~kind:T.Pause_button ~order:o ~created_by:"U123"
      ~now:S.now ()
  in
  Store.set_card_message_ts db ~card_id:c.id "1700000000.000200"

let seed db =
  let o = S.seed db () in
  announce db o;
  let f = S.fire db o ~room:"R1" () in
  (o, f)

let change db lease (before : Store.fire_runtime) st now =
  let after = { before with fire = { before.fire with state = st } } in
  Store.cas db ~lease ~before ~after ~now

let prepare db id lease now =
  let r = runtime db id in
  check "fetch" true (ok (change db lease r T.Fetching now));
  check "upload" true (ok (change db lease (runtime db id) T.Uploading now))

let reserve db id lease now repost =
  Store.reserve_dispatch db ~fire_id:id ~lease ~repost ~now

let with_handles f =
  let path = Filename.temp_file "huddle-fire-store" ".db" in
  let a = Sqlite3.db_open path and b = Sqlite3.db_open path in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sqlite3.db_close b);
      ignore (Sqlite3.db_close a);
      Sys.remove path)
    (fun () ->
      Store.ensure_schema a;
      Store.ensure_schema b;
      f a b)

let test_channel_claim () =
  with_handles (fun a b ->
      let o, f = seed a in
      let other = S.fire a o ~room:"R2" () in
      let channel = S.seed a ~channel:"C456" () in
      let third = S.fire a channel ~room:"R3" () in
      let lease = claim a f.id "worker-a" S.now in
      Alcotest.(check (float 0.)) "120 s lease" (S.now +. 120.) lease.until;
      check "second handle blocked" true
        (ok (Store.claim b ~fire_id:other.id ~owner:"worker-b" ~now:S.now)
        = None);
      ignore (claim b third.id "worker-b" S.now : Store.lease);
      ignore (claim b other.id "worker-b" (S.now +. 120.) : Store.lease))

let test_expired () =
  with_handles (fun a b ->
      let _, f = seed a in
      let lease = claim a f.id "old" S.now in
      let before = runtime a f.id in
      check "expired relay transition" false
        (ok (change a lease before T.Fetching lease.until));
      check "expired read-only update" false
        (ok
           (Store.cas a ~lease ~before
              ~after:{ before with next_attempt_at = Some lease.until }
              ~now:lease.until));
      let successor = claim b f.id "new" lease.until in
      integer "epoch advances" (lease.lease_epoch + 1) successor.lease_epoch;
      check "old renewal" true
        (ok (Store.renew a ~fire_id:f.id ~lease ~now:lease.until) = None);
      Store.release a ~fire_id:f.id ~lease ~now:lease.until;
      check "old release preserves successor" true
        ((runtime b f.id).claim = Some successor))

let test_migration () =
  let db = Sqlite3.db_open ":memory:" in
  List.iter (Sql_util.exec_exn db) Slack_standing_order_store_base.schema;
  Sql_util.exec_exn db
    "INSERT INTO slack_standing_order_fires \
     (id,order_id,room_id,date_start,state,epoch,uploaded_file_ids,relay_ts,dispatched_at,reposts,created_at,updated_at) \
     VALUES \
     ('legacy','so_old','R1','1700000000','posted',1,'[\"FOLD\"]','1700000010.000001',1700000005,1,'old','old')";
  Store.ensure_schema db;
  Store.ensure_schema db;
  integer "initialization imports legacy attempts" 2
    (Store.int_query db "SELECT COUNT(*) FROM slack_standing_order_dispatches"
       []);
  let first = runtime db "legacy" in
  let second = runtime db "legacy" in
  check "repeat migration preserves runtime" true (first = second);
  integer "both historical debits" 2
    (Store.dispatches_since db ~order_id:"so_old" ~since:0.);
  integer "two retained attempts" 2 (List.length first.attempts);
  let a = List.hd first.attempts in
  Alcotest.(check (list string)) "old ids" [ "FOLD" ] a.uploaded_file_ids;
  Alcotest.(check (option (float 0.000001)))
    "relay timestamp starts pickup" (Some 1700000010.000001) a.pickup_started_at

let test_failed_cap () =
  let db = S.fresh_db () in
  let _, f = seed db in
  let lease = claim db f.id "worker" S.now in
  prepare db f.id lease S.now;
  Sqlite3.create_fun0 db "count" (fun () -> failwith "fixture cap read failure");
  check "strict failure" true
    (Result.is_error (reserve db f.id lease S.now false));
  let r = runtime db f.id in
  state "no posting" T.Uploading r.fire.state;
  check "no debit" true
    (List.for_all
       (fun (a : Store.relay_attempt) -> a.dispatched_at = None)
       r.attempts)

let test_repost_day () =
  let db = S.fresh_db () in
  let o, f = seed db in
  let lease = claim db f.id "worker" S.now in
  prepare db f.id lease S.now;
  check "first reservation" true (ok (reserve db f.id lease S.now false));
  check "unknown" true
    (ok (change db lease (runtime db f.id) T.Posting_unknown S.now));
  let later = S.now +. 86401. in
  let lease = claim db f.id "worker" later in
  check "repost" true (ok (reserve db f.id lease later true));
  integer "actual day" 1
    (Store.dispatches_since db ~order_id:o.id ~since:(later -. 86400.));
  integer "all time" 2 (Store.dispatches_since db ~order_id:o.id ~since:0.);
  check "no third attempt" false (ok (reserve db f.id lease later true))

let record_relay db id lease ts now =
  let before = runtime db id in
  let attempts =
    List.map
      (fun (a : Store.relay_attempt) ->
        if a.attempt_no = before.current_attempt then
          { a with relay_ts = Some ts }
        else a)
      before.attempts
  in
  let after =
    {
      before with
      attempts;
      fire = { before.fire with state = T.Posted; relay_ts = Some ts };
    }
  in
  Store.cas db ~lease ~before ~after ~now

let test_pause_complete () =
  let db = S.fresh_db () in
  let o, f = seed db in
  let lease = claim db f.id "worker" S.now in
  prepare db f.id lease S.now;
  check "reserved" true (ok (reserve db f.id lease S.now false));
  let before_pause = runtime db f.id in
  let receipt =
    ok (Store.deactivate db ~order_id:o.id (Store.Pause "U456") ~now:S.now)
  in
  Alcotest.(check (option int)) "disclosed dispatch" (Some 1) receipt.in_flight;
  (* The returning complete still has the snapshot from before the pause. *)
  let ts = "1700000001.000001" in
  let attempts =
    List.map
      (fun (a : Store.relay_attempt) -> { a with relay_ts = Some ts })
      before_pause.attempts
  in
  let after =
    {
      before_pause with
      attempts;
      fire = { before_pause.fire with state = T.Posted; relay_ts = Some ts };
    }
  in
  check "complete recorded" true
    (ok (Store.cas db ~lease ~before:before_pause ~after ~now:S.now));
  let r = runtime db f.id in
  state "posted" T.Posted r.fire.state;
  check "sticky pause fence" true r.fire.repost_forbidden;
  check "no repost" false (ok (reserve db f.id lease S.now true))

let test_pause_before () =
  let db = S.fresh_db () in
  let o, f = seed db in
  let lease = claim db f.id "worker" S.now in
  prepare db f.id lease S.now;
  ignore
    (ok (Store.deactivate db ~order_id:o.id (Store.Pause "U456") ~now:S.now)
      : Store.state_change);
  check "no dispatch after pause" false (ok (reserve db f.id lease S.now false));
  integer "no cap debit" 0 (Store.dispatches_since db ~order_id:o.id ~since:0.)

let test_transcript_unique () =
  let db = S.fresh_db () in
  let o, f = seed db in
  ignore
    (ok
       (Store.insert_fire db ~order:o ~room_id:"R2" ~date_start:"1700000001"
          ~transcript_file_id:"FTRANSCRIPT" ~now:S.now ()));
  let lease = claim db f.id "worker" S.now in
  let before = runtime db f.id in
  check "uniqueness refuses" true
    (Result.is_error
       (Store.cas db ~lease ~before
          ~after:{ before with transcript_id = Some "FTRANSCRIPT" }
          ~now:S.now));
  check "row unchanged" true (runtime db f.id = before)

let test_suspend_epoch () =
  let db = S.fresh_db () in
  let o, f = seed db in
  ignore
    (ok (Store.deactivate db ~order_id:o.id (Store.Pause "U456") ~now:S.now)
      : Store.state_change);
  let paused = some (Store.find db o.id) in
  let resumed =
    match
      Store.resume db ~order_id:o.id ~expected_epoch:paused.epoch
        ~active_since:"1700000001.000001" ~bot_user_id:"UBOT" ~bot_id:"BOWN"
        ~now:S.now
    with
    | Ok o -> o
    | Error _ -> Alcotest.fail "resume failed"
  in
  check "old refusal ignored" false
    (ok
       (Store.suspend_if_epoch db ~order_id:o.id ~epoch:o.epoch
          ~reason:"missing_scope" ~now:S.now));
  check "resumed order unchanged" true (Store.find db o.id = Some resumed);
  let r = runtime db f.id in
  check "current epoch suspends" true
    (ok
       (Store.suspend_if_epoch db ~order_id:o.id ~epoch:resumed.epoch
          ~reason:"missing_scope" ~now:S.now));
  check "same refusal idempotent" false
    (ok
       (Store.suspend_if_epoch db ~order_id:o.id ~epoch:resumed.epoch
          ~reason:"missing_scope" ~now:S.now));
  state "already skipped fire unchanged" r.fire.state
    (runtime db f.id).fire.state

let test_unannounced () =
  let db = S.fresh_db () in
  let o = S.seed db () in
  let f = S.fire db o ~state:T.Uploading ~room:"R1" () in
  let lease = claim db f.id "worker" S.now in
  check "active alone is insufficient" false
    (ok (reserve db f.id lease S.now false));
  integer "no debit" 0 (Store.dispatches_since db ~order_id:o.id ~since:0.)

let test_renew_release_due () =
  let db = S.fresh_db () in
  let _, f = seed db in
  integer "due initially" 1 (List.length (Store.due db ~now:S.now));
  let lease = claim db f.id "worker" S.now in
  integer "live claim not due" 0 (List.length (Store.due db ~now:S.now));
  let renewed =
    some (ok (Store.renew db ~fire_id:f.id ~lease ~now:(S.now +. 30.)))
  in
  Alcotest.(check (float 0.))
    "renew for 120 seconds" (S.now +. 150.) renewed.until;
  integer "same fencing epoch" lease.lease_epoch renewed.lease_epoch;
  let before = runtime db f.id in
  check "schedule" true
    (ok
       (Store.cas db ~lease:renewed ~before
          ~after:{ before with next_attempt_at = Some (S.now +. 60.) }
          ~now:(S.now +. 30.)));
  Store.release db ~fire_id:f.id ~lease:renewed ~now:(S.now +. 30.);
  integer "future time not due" 0
    (List.length (Store.due db ~now:(S.now +. 59.)));
  integer "due on boundary" 1 (List.length (Store.due db ~now:(S.now +. 60.)))

let test_retained_attempts () =
  let db = S.fresh_db () in
  let _, f = seed db in
  let lease = claim db f.id "worker" S.now in
  prepare db f.id lease S.now;
  let before = runtime db f.id in
  let a =
    {
      (Store.empty_attempt 0) with
      uploaded_file_ids = [ "FFIRST" ];
      upload_started_at = Some S.now;
    }
  in
  check "prepare first" true
    (ok
       (Store.cas db ~lease ~before
          ~after:{ before with attempts = [ a ] }
          ~now:S.now));
  integer "prepared is not a debit" 0
    (Store.dispatches_since db ~order_id:before.fire.order_id ~since:0.);
  check "reserve first" true (ok (reserve db f.id lease S.now false));
  check "first shared" true
    (ok (record_relay db f.id lease "1700000001.000001" S.now));
  let before = runtime db f.id in
  let second =
    {
      (Store.empty_attempt 1) with
      uploaded_file_ids = [ "FSECOND" ];
      upload_started_at = Some S.now;
    }
  in
  check "prepare second" true
    (ok
       (Store.cas db ~lease ~before
          ~after:{ before with attempts = before.attempts @ [ second ] }
          ~now:S.now));
  check "reserve second" true (ok (reserve db f.id lease S.now true));
  check "second shared" true
    (ok (record_relay db f.id lease "1700000100.000001" S.now));
  let r = runtime db f.id in
  integer "both retained" 2 (List.length r.attempts);
  Alcotest.(check (list (list string)))
    "ids retained"
    [ [ "FFIRST" ]; [ "FSECOND" ] ]
    (List.map (fun (a : Store.relay_attempt) -> a.uploaded_file_ids) r.attempts);
  Alcotest.(check (option string))
    "current ts" (Some "1700000100.000001") r.fire.relay_ts;
  Alcotest.(check (list (option (float 0.000001))))
    "independent relay clocks"
    [ Some 1700000001.000001; Some 1700000100.000001 ]
    (List.map (fun (a : Store.relay_attempt) -> a.pickup_started_at) r.attempts);
  let projection =
    Store.query db
      "SELECT uploaded_file_ids FROM slack_standing_order_fires WHERE id=?"
      [ Store.text f.id ]
      (fun s -> Sql_util.text_column s 0)
  in
  Alcotest.(check (list string))
    "compatibility projection" [ "[\"FSECOND\"]" ] projection

let test_cap_atomic () =
  with_handles (fun a b ->
      let o, f = seed a in
      Store.exec a
        "UPDATE slack_standing_orders SET max_fires_per_day=1 WHERE id=?"
        [ Store.text o.id ];
      let next = S.fire a o ~room:"R2" () in
      let lease = claim a f.id "one" S.now in
      prepare a f.id lease S.now;
      check "first gets last slot" true (ok (reserve a f.id lease S.now false));
      check "duplicate reservation" false
        (ok (reserve a f.id lease S.now false));
      Store.release a ~fire_id:f.id ~lease ~now:S.now;
      let lease = claim b next.id "two" S.now in
      prepare b next.id lease S.now;
      check "fresh cap across handles" false
        (ok (reserve b next.id lease S.now false));
      integer "one debit" 1 (Store.dispatches_since a ~order_id:o.id ~since:0.))

let test_invalid_cas () =
  let db = S.fresh_db () in
  let _, f = seed db in
  let lease = claim db f.id "worker" S.now in
  let before = runtime db f.id in
  check "no direct posting" true
    (Result.is_error (change db lease before T.Posting S.now));
  check "scope immutable" true
    (Result.is_error
       (Store.cas db ~lease ~before
          ~after:{ before with fire = { before.fire with order_id = "other" } }
          ~now:S.now));
  check "unchanged" true (runtime db f.id = before)

let test_paused_adoption () =
  let db = S.fresh_db () in
  let o, f = seed db in
  let lease = claim db f.id "worker" S.now in
  prepare db f.id lease S.now;
  check "first" true (ok (reserve db f.id lease S.now false));
  check "unknown" true
    (ok (change db lease (runtime db f.id) T.Posting_unknown S.now));
  check "second" true (ok (reserve db f.id lease S.now true));
  ignore
    (ok (Store.deactivate db ~order_id:o.id (Store.Pause "U456") ~now:S.now)
      : Store.state_change);
  let before = runtime db f.id in
  let ts = "1700000010.000001" in
  let attempts =
    List.map
      (fun (a : Store.relay_attempt) ->
        if a.attempt_no = 1 then
          { a with uploaded_file_ids = [ "FADOPTED" ]; relay_ts = Some ts }
        else a)
      before.attempts
  in
  let after =
    {
      before with
      attempts;
      fire = { before.fire with state = T.Posted; relay_ts = Some ts };
    }
  in
  check "read-only adoption after pause" true
    (ok (Store.cas db ~lease ~before ~after ~now:S.now));
  integer "keeps prior debit" 2
    (Store.dispatches_since db ~order_id:o.id ~since:0.)

let test_reservation_rollback () =
  let db = S.fresh_db () in
  let o, f = seed db in
  let lease = claim db f.id "worker" S.now in
  prepare db f.id lease S.now;
  Store.exec db
    "CREATE TRIGGER fail_reserve BEFORE UPDATE OF dispatched_at ON \
     slack_standing_order_fires BEGIN SELECT RAISE(ABORT,'fixture reservation \
     write failure'); END"
    [];
  check "failed update" true
    (Result.is_error (reserve db f.id lease S.now false));
  integer "debit rolled back" 0
    (Store.dispatches_since db ~order_id:o.id ~since:0.);
  state "fire rolled back" T.Uploading (runtime db f.id).fire.state

let terminal_repost db capped =
  let o, f = seed db in
  let lease = claim db f.id "worker" S.now in
  prepare db f.id lease S.now;
  let before = runtime db f.id in
  let first =
    { (Store.empty_attempt 0) with uploaded_file_ids = [ "FFIRST" ] }
  in
  check "prepare original" true
    (ok
       (Store.cas db ~lease ~before
          ~after:{ before with attempts = [ first ] }
          ~now:S.now));
  check "reserve original" true (ok (reserve db f.id lease S.now false));
  check "record original" true
    (ok (record_relay db f.id lease "1700000001.000001" S.now));
  let notice_at = S.now +. 10. in
  let before = runtime db f.id in
  let first =
    {
      (List.hd before.attempts) with
      first_reply_ts = Some "1700000010.000001";
      rate_limited_at = Some notice_at;
    }
  in
  let second =
    { (Store.empty_attempt 1) with uploaded_file_ids = [ "FSECOND" ] }
  in
  check "record rate notice and prepare re-post" true
    (ok
       (Store.cas db ~lease ~before
          ~after:
            {
              before with
              attempts = [ first; second ];
              next_attempt_at = Some (notice_at +. 300.);
            }
          ~now:notice_at));
  let now = notice_at +. 300. in
  let lease = claim db f.id "worker" now in
  if capped then
    Store.exec db
      "UPDATE slack_standing_orders SET max_fires_per_day=1 WHERE id=?"
      [ Store.text o.id ];
  check "re-post reservation" (not capped) (ok (reserve db f.id lease now true));
  check "terminal re-post outcome" true
    (ok
       (change db lease (runtime db f.id)
          (if capped then T.Skipped_cap else T.Failed)
          now));
  (o, f, lease, now)

let observe_original db id lease st now =
  let before = runtime db id in
  let attempts =
    List.map
      (fun (a : Store.relay_attempt) ->
        if a.attempt_no <> 0 then a
        else
          match st with
          | T.Acknowledged ->
              { a with reaction_seen = true; acknowledged_at = Some now }
          | T.Picked_up ->
              { a with acknowledged_at = Some now; picked_up_at = Some now }
          | T.Not_delivered -> { a with not_delivered = true }
          | _ -> Alcotest.fail "unexpected observation state")
      before.attempts
  in
  Store.cas db ~lease ~before
    ~after:{ before with attempts; fire = { before.fire with state = st } }
    ~now

let check_dispatch_unchanged db (before : Store.fire_runtime) =
  let after = runtime db before.fire.id in
  let identities (r : Store.fire_runtime) =
    List.map
      (fun (a : Store.relay_attempt) ->
        ( a.attempt_no,
          a.uploaded_file_ids,
          a.relay_ts,
          a.dispatched_at,
          a.pickup_started_at,
          a.first_reply_ts,
          a.rate_limited_at ))
      r.attempts
  in
  check "retained identities, debits and clocks unchanged" true
    (identities before = identities after);
  check "fire identity and accounting unchanged" true
    ({ after.fire with state = before.fire.state } = before.fire);
  integer "current attempt unchanged" before.current_attempt
    after.current_attempt;
  check "source identity unchanged" true
    ((before.root_ts, before.canvas_id, before.transcript_id, before.nonce)
    = (after.root_ts, after.canvas_id, after.transcript_id, after.nonce))

let test_terminal_repost_observation capped st () =
  let db = S.fresh_db () in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      let o, f, lease, now = terminal_repost db capped in
      let before = runtime db f.id in
      integer "expected re-post count"
        (if capped then 0 else 1)
        before.fire.reposts;
      let debits = if capped then 1 else 2 in
      integer "expected dispatch debits" debits
        (Store.dispatches_since db ~order_id:o.id ~since:0.);
      check "retained original observation" true
        (ok (observe_original db f.id lease st (now +. 1.)));
      let after = runtime db f.id in
      state "observation advances fire" st after.fire.state;
      let original = List.hd after.attempts in
      (match st with
      | T.Acknowledged -> check "reaction recorded" true original.reaction_seen
      | T.Picked_up ->
          check "pickup recorded" true (original.picked_up_at <> None)
      | T.Not_delivered ->
          check "non-delivery recorded" true original.not_delivered
      | _ -> assert false);
      let second = List.nth after.attempts 1 in
      check "original never acknowledges second attempt" true
        (second.acknowledged_at = None
        && second.picked_up_at = None && (not second.reaction_seen)
        && not second.not_delivered);
      check_dispatch_unchanged db before;
      integer "observation costs no dispatch" debits
        (Store.dispatches_since db ~order_id:o.id ~since:0.);
      if st <> T.Not_delivered then
        check "late disabled notice wins" true
          (ok (observe_original db f.id lease T.Not_delivered (now +. 2.)));
      state "non-delivery dominates" T.Not_delivered
        (runtime db f.id).fire.state;
      let terminal = runtime db f.id in
      List.iter
        (fun target ->
          check "cannot regress non-delivery" true
            (Result.is_error (change db lease terminal target (now +. 3.))))
        [ T.Acknowledged; T.Picked_up ];
      check_dispatch_unchanged db before)

let test_terminal_tracking_requires_dispatch () =
  List.iter
    (fun terminal ->
      let db = S.fresh_db () in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close db))
        (fun () ->
          let _, f = seed db in
          let lease = claim db f.id "worker" S.now in
          check "undispatched terminal" true
            (ok (change db lease (runtime db f.id) terminal S.now));
          let before = runtime db f.id in
          List.iter
            (fun target ->
              let a =
                {
                  (Store.empty_attempt 0) with
                  reaction_seen = true;
                  acknowledged_at = Some S.now;
                  picked_up_at = Some S.now;
                  not_delivered = target = T.Not_delivered;
                }
              in
              check "undispatched terminal stays closed" true
                (Result.is_error
                   (Store.cas db ~lease ~before
                      ~after:
                        {
                          before with
                          attempts = [ a ];
                          fire = { before.fire with state = target };
                        }
                      ~now:S.now));
              check "refusal preserves runtime" true (runtime db f.id = before))
            [ T.Acknowledged; T.Picked_up; T.Not_delivered ]))
    [ T.Skipped_cap; T.Failed ]

let test_terminal_tracking_requires_evidence () =
  List.iter
    (fun capped ->
      let db = S.fresh_db () in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close db))
        (fun () ->
          let _, f, lease, now = terminal_repost db capped in
          let before = runtime db f.id in
          List.iter
            (fun target ->
              check "state alone cannot reopen terminal" true
                (Result.is_error (change db lease before target now));
              if capped then begin
                let attempts =
                  List.map
                    (fun (a : Store.relay_attempt) ->
                      if a.attempt_no = 0 then a
                      else
                        {
                          a with
                          reaction_seen = true;
                          acknowledged_at = Some now;
                          picked_up_at = Some now;
                          not_delivered = target = T.Not_delivered;
                        })
                    before.attempts
                in
                check "prepared re-post is not dispatched evidence" true
                  (Result.is_error
                     (Store.cas db ~lease ~before
                        ~after:
                          {
                            before with
                            attempts;
                            fire = { before.fire with state = target };
                          }
                        ~now))
              end;
              check "refusal preserves runtime" true (runtime db f.id = before))
            [ T.Acknowledged; T.Picked_up; T.Not_delivered ];
          let attempts =
            List.map
              (fun (a : Store.relay_attempt) ->
                if a.attempt_no = 0 then { a with not_delivered = true } else a)
              before.attempts
          in
          check "retain non-delivery evidence" true
            (ok
               (Store.cas db ~lease ~before ~after:{ before with attempts } ~now));
          List.iter
            (fun target ->
              check "retained non-delivery prevents positive tracking" true
                (Result.is_error (observe_original db f.id lease target now)))
            [ T.Acknowledged; T.Picked_up ]))
    [ true; false ]

let test_second_adoption_identity () =
  with_handles (fun db _ ->
      let o, f = seed db in
      let lease = claim db f.id "worker" S.now in
      prepare db f.id lease S.now;
      let before = runtime db f.id in
      let first =
        { (Store.empty_attempt 0) with uploaded_file_ids = [ "FORIGINAL" ] }
      in
      check "first identity" true
        (ok
           (Store.cas db ~lease ~before
              ~after:{ before with attempts = [ first ] }
              ~now:S.now));
      check "reserve original" true (ok (reserve db f.id lease S.now false));
      check "record original" true
        (ok (record_relay db f.id lease "1700000001.000001" S.now));
      check "reserve one repost" true (ok (reserve db f.id lease S.now true));
      Store.exec db
        "UPDATE slack_standing_orders SET max_fires_per_day=2 WHERE id=?"
        [ Store.text o.id ];
      ignore
        (ok
           (Store.deactivate db ~order_id:o.id (Store.Pause "U456") ~now:S.now));
      let before = runtime db f.id in
      let second =
        {
          (some (Store.attempt before 1)) with
          uploaded_file_ids = [ "FSECOND" ];
          relay_ts = Some "1700000100.000001";
        }
      in
      let after =
        {
          before with
          attempts = [ some (Store.attempt before 0); second ];
          fire =
            { before.fire with state = T.Posted; relay_ts = second.relay_ts };
        }
      in
      check "paused capped adoption" true
        (ok (Store.cas db ~lease ~before ~after ~now:S.now));
      let saved = runtime db f.id in
      integer "two retained attempts" 2 (List.length saved.attempts);
      integer "one repost debit" 1 saved.fire.reposts;
      integer "existing debits only" 2
        (Store.dispatches_since db ~order_id:o.id ~since:0.);
      check "first identity unchanged" true
        ((some (Store.attempt saved 0)).uploaded_file_ids = [ "FORIGINAL" ]);
      Alcotest.(check (option (float 0.000001)))
        "new pickup origin" (Some 1700000100.000001)
        (some (Store.attempt saved 1)).pickup_started_at;
      check "cannot debit adopted result again" false
        (ok (reserve db f.id lease S.now true)))

let test_complete_evidence () =
  with_handles (fun db other ->
      let _, f = seed db in
      let lease = claim db f.id "worker" S.now in
      prepare db f.id lease S.now;
      let before = runtime db f.id in
      let rejected =
        { (Store.empty_attempt 0) with complete_rejected_before_share = true }
      in
      check "undispatched rejection refused" true
        (Result.is_error
           (Store.cas db ~lease ~before
              ~after:{ before with attempts = [ rejected ] }
              ~now:S.now));
      check "original reserved" true (ok (reserve db f.id lease S.now false));
      let before = runtime db f.id in
      let rejected =
        {
          (some (Store.attempt before 0)) with
          complete_rejected_before_share = true;
        }
      in
      check "retain rejection" true
        (ok
           (Store.cas db ~lease ~before
              ~after:
                {
                  before with
                  attempts = [ rejected ];
                  fire =
                    {
                      before.fire with
                      state = T.Posting_unknown;
                      reason = Some "complete_rejected_before_share";
                    };
                }
              ~now:S.now));
      let before = runtime db f.id in
      check "retry reason can change" true
        (ok
           (Store.cas db ~lease ~before
              ~after:
                {
                  before with
                  fire =
                    { before.fire with reason = Some "indeterminate:auth.test" };
                }
              ~now:S.now));
      Store.ensure_schema other;
      let before = runtime other f.id in
      check "durable across handles and schema initialization" true
        (some (Store.attempt before 0)).complete_rejected_before_share;
      let cleared = { rejected with complete_rejected_before_share = false } in
      check "rejection cannot be erased" true
        (Result.is_error
           (Store.cas other ~lease ~before
              ~after:{ before with attempts = [ cleared ] }
              ~now:S.now));
      check "reserve second" true (ok (reserve db f.id lease S.now true));
      let saved = runtime other f.id in
      check "original rejection retained independently" true
        (some (Store.attempt saved 0)).complete_rejected_before_share;
      check "second attempt has independent outcome" false
        (some (Store.attempt saved 1)).complete_rejected_before_share)

let test_complete_evidence_migration () =
  with_handles (fun db other ->
      let _, f = seed db in
      let lease = claim db f.id "worker" S.now in
      prepare db f.id lease S.now;
      check "reserve original" true (ok (reserve db f.id lease S.now false));
      let before = runtime db f.id in
      check "legacy rejection reason" true
        (ok
           (Store.cas db ~lease ~before
              ~after:
                {
                  before with
                  fire =
                    {
                      before.fire with
                      state = T.Posting_unknown;
                      reason = Some "complete_rejected_before_share";
                    };
                }
              ~now:S.now));
      Store.exec db
        "ALTER TABLE slack_standing_order_dispatches DROP COLUMN \
         complete_rejected_before_share"
        [];
      Store.exec db
        "CREATE TRIGGER fail_backfill BEFORE UPDATE ON \
         slack_standing_order_dispatches BEGIN SELECT RAISE(FAIL,'injected \
         backfill failure'); END"
        [];
      check "first backfill fails" true
        (try
           Store.ensure_schema db;
           false
         with exn when Store.is_db_error exn -> true);
      check "column installation committed before backfill" true
        (List.mem "complete_rejected_before_share"
           (Store.query other
              "PRAGMA table_info(slack_standing_order_dispatches)" [] (fun s ->
                Sql_util.text_column s 1)));
      integer "failed backfill leaves evidence unset" 0
        (Store.int_query other
           "SELECT complete_rejected_before_share FROM \
            slack_standing_order_dispatches WHERE fire_id=?"
           [ Store.text f.id ]);
      Store.exec db "DROP TRIGGER fail_backfill" [];
      Store.ensure_schema db;
      Store.ensure_schema other;
      check "retry restores legacy rejection idempotently" true
        (some (Store.attempt (runtime other f.id) 0))
          .complete_rejected_before_share;
      (* A pre-attempt-table fire must import the same evidence, too. *)
      Store.exec db
        "DELETE FROM slack_standing_order_dispatches WHERE fire_id=?"
        [ Store.text f.id ];
      Store.ensure_schema db;
      check "legacy fire import retains rejection" true
        (some (Store.attempt (runtime db f.id) 0))
          .complete_rejected_before_share)

let suite =
  [
    S.case "complete rejection is durable immutable per attempt evidence"
      test_complete_evidence;
    S.case "complete rejection migration preserves existing evidence"
      test_complete_evidence_migration;
    S.case
      "adopted second attempt retains identities and exactly one repost debit"
      test_second_adoption_identity;
    S.case "one leased fire per channel across handles" test_channel_claim;
    S.case "expired lease holder cannot write state" test_expired;
    S.case "runtime migration twice preserves rows" test_migration;
    S.case "failed cap read never reserves" test_failed_cap;
    S.case "repost counts on its actual day" test_repost_day;
    S.case "pause racing complete records dispatched fire" test_pause_complete;
    S.case "pause before reserve gives no dispatch" test_pause_before;
    S.case "transcript id already used refuses runtime update"
      test_transcript_unique;
    S.case "old hard refusal cannot suspend resumed epoch" test_suspend_epoch;
    S.case "unannounced activation cannot reserve" test_unannounced;
    S.case "renew release and due times" test_renew_release_due;
    S.case "retained attempts preserve identities and independent pickup clocks"
      test_retained_attempts;
    S.case "last cap slot reserved once across handles" test_cap_atomic;
    S.case "CAS rejects undeclared transitions and scope changes"
      test_invalid_cas;
    S.case "paused adoption retains existing debit without active fence"
      test_paused_adoption;
    S.case "reservation write failure rolls back debit"
      test_reservation_rollback;
    S.case "cap-refused re-post retains original reaction"
      (test_terminal_repost_observation true T.Acknowledged);
    S.case "cap-refused re-post retains original ordinary reply"
      (test_terminal_repost_observation true T.Picked_up);
    S.case "cap-refused re-post retains original disabled or spend-limit notice"
      (test_terminal_repost_observation true T.Not_delivered);
    S.case "failed re-post retains original reaction"
      (test_terminal_repost_observation false T.Acknowledged);
    S.case "failed re-post retains original ordinary reply"
      (test_terminal_repost_observation false T.Picked_up);
    S.case "failed re-post retains original disabled or spend-limit notice"
      (test_terminal_repost_observation false T.Not_delivered);
    S.case "undispatched terminal fires stay closed"
      test_terminal_tracking_requires_dispatch;
    S.case "terminal tracking transitions require retained attempt evidence"
      test_terminal_tracking_requires_evidence;
  ]
