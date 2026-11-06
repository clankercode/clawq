module S = Test_slack_standing_order_support
module Q = Slack_standing_order_schedule
module Store = Slack_standing_order_store
module T = Slack_standing_order_types
module G = Slack_standing_order_grant
module Sql = Slack_standing_order_sql

let check = Alcotest.(check bool)

let with_db f =
  let db = S.fresh_db () in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      let o = S.seed db () in
      f db o)

let root = "1700000001.000100"
let register db o metadata = Q.register ~db ~key:(Q.key o root) ~metadata

let claim db o owner now =
  Q.claim_resolution ~db ~key:(Q.key o root) ~owner_id:owner ~now

let finish db lease outcome now = Q.finish_resolution ~db ~lease ~outcome ~now
let due db o now = Q.resolution_due ~db ~key:(Q.key o root) ~now

let test_fallback () =
  with_db (fun db o ->
      register db o "live";
      let l = Option.get (claim db o "one" S.now) in
      check "one owner" true (claim db o "two" S.now = None);
      check "successful inspection" true (finish db l Q.Live S.now);
      check "not due at 299" false (due db o (S.now +. 299.));
      register db o "live";
      check "duplicate metadata is quiet" false (due db o (S.now +. 299.));
      check "due at 300" true (due db o (S.now +. 300.));
      register db o "ended";
      check "end dirties fallback now" true (due db o S.now))

let test_dirty_race () =
  with_db (fun db o ->
      register db o "live";
      let l = Option.get (claim db o "one" S.now) in
      register db o "ready";
      check "old inspection releases owner" true (finish db l Q.Settled S.now);
      check "new revision survives settled old result" true (due db o S.now);
      let newer = Option.get (claim db o "two" S.now) in
      check "old owner cannot finish successor" false (finish db l Q.Live S.now);
      check "new owner settles" true (finish db newer Q.Settled S.now);
      Q.dirty_channel ~db ~channel:o.channel_id;
      check "settled roots leave resolution queue" false
        (due db o (S.now +. 9999.)))

let test_backoff () =
  with_db (fun db o ->
      register db o "live";
      let at = ref S.now in
      List.iteri
        (fun i delay ->
          let l = Option.get (claim db o "retry" !at) in
          check "failure persists" true
            (Q.fail_resolution ~db ~lease:l
               ~failure:(G.failure (G.Indeterminate "transport"))
               ~now:!at);
          register db o (string_of_int i);
          Q.dirty_channel ~db ~channel:o.channel_id;
          check "dirty cannot erase transport backoff" false
            (due db o (!at +. delay -. 1.));
          at := !at +. delay;
          check "retry due" true (due db o !at))
        [ 120.; 300.; 600.; 1200.; 1200. ])

let test_expiry () =
  with_db (fun db o ->
      register db o "live";
      let l = Option.get (claim db o "same-owner" S.now) in
      let now = S.now +. 120. in
      let next = Option.get (claim db o "same-owner" now) in
      check "expiry token fences same owner" false (finish db l Q.Live now);
      check "successor completes" true (finish db next Q.Live now))

let hint db ?(app = "A123") ?(canvas = "FCANVAS") ?(ready = "1700000060.000100")
    () =
  Q.note_ready ~db ~team:"T123" ~app ~channel:"C123" ~thread_ts:root
    ~canvas_file_id:canvas ~ready_ts:ready ~now:S.now

let fetching db o ?(reason = "notes_not_ready") () =
  let f =
    S.fire db o ~state:T.Fetching ~room:"R123" ~date_start:"1700000001" ()
  in
  Sql.exec db
    "UPDATE slack_standing_order_fires SET \
     root_ts=?,reason=?,next_attempt_at=? WHERE id=?"
    Sqlite3.Data.[ TEXT root; TEXT reason; FLOAT (S.now +. 300.); TEXT f.id ];
  f

let test_take () =
  with_db (fun db o ->
      let f = fetching db o () in
      hint db ~app:"AOTHER" ();
      check "installation isolation" true (Q.ready_fires ~db ~now:S.now = []);
      hint db ();
      hint db ();
      Alcotest.(check int)
        "deduplicated hint" 1
        (Sql.int_query db "SELECT COUNT(*) FROM slack_order_ready_hints" []);
      let lease =
        match Store.claim db ~fire_id:f.id ~owner:"worker" ~now:S.now with
        | Ok (Some l) -> l
        | _ -> Alcotest.fail "claim"
      in
      check "wrong owner" false
        (Q.take_ready ~db ~fire_id:f.id
           ~lease:{ lease with owner = "other" }
           ~now:S.now);
      check "consumer CAS makes due" true
        (Q.take_ready ~db ~fire_id:f.id ~lease ~now:S.now);
      let r = Option.get (Store.runtime db ~fire_id:f.id) in
      check "retry guard open" true (r.next_attempt_at = Some S.now);
      check "no source association manufactured" true (r.canvas_id = None);
      check "no grant epoch or claim changes" true
        (r.fire.epoch = o.epoch && r.claim = Some lease);
      check "same hint once" false
        (Q.take_ready ~db ~fire_id:f.id ~lease ~now:S.now))

let test_refusals () =
  List.iter
    (fun reason ->
      with_db (fun db o ->
          ignore (fetching db o ~reason ());
          hint db ();
          check reason true (Q.ready_fires ~db ~now:S.now = [])))
    [ "p4_recovery_held"; "transport"; "rate_limited"; "posting_unknown" ];
  with_db (fun db o ->
      let f = fetching db o () in
      hint db ();
      ignore
        (Store.deactivate db ~order_id:o.id (Store.Pause "U123") ~now:S.now);
      check "pause invalidates wake" true (Q.ready_fires ~db ~now:S.now = []);
      check "no fire resurrection" true
        ((Option.get (Store.runtime db ~fire_id:f.id)).fire.state
       = T.Skipped_inactive))

let test_shared_handles () =
  let path = Filename.temp_file "clawq-root-schedule" ".db" in
  let a = Sqlite3.db_open path and b = Sqlite3.db_open path in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sqlite3.db_close a);
      ignore (Sqlite3.db_close b);
      Sys.remove path)
    (fun () ->
      Store.ensure_schema a;
      let o = S.seed a () in
      register a o "live";
      let lease = Option.get (claim a o "executor" S.now) in
      check "reconciler shares same claimed revision" true
        (claim b o "reconciler" S.now = None);
      register b o "ended";
      check "old completion accepted without consuming dirty end" true
        (finish a lease Q.Live S.now);
      let next = Option.get (claim b o "reconciler" S.now) in
      check "shared successor completes" true (finish b next Q.Live S.now);
      check "shared fallback persists" false (due a o (S.now +. 299.));
      ignore (Store.deactivate a ~order_id:o.id (Store.Pause "U123") ~now:S.now);
      check "stale epoch key cannot claim after pause" true
        (claim b o "old" (S.now +. 300.) = None))

let test_renewal () =
  with_db (fun db o ->
      let lease = Option.get (claim db o "executor" S.now) in
      let renewed =
        Option.get (Q.renew_resolution ~db ~lease ~now:(S.now +. 30.))
      in
      check "renewed owner blocks another process beyond original expiry" true
        (claim db o "other" (S.now +. 121.) = None);
      check "old finite token cannot finish after expiry" false
        (finish db lease Q.Live (S.now +. 121.));
      check "current token finishes" true
        (finish db renewed Q.Live (S.now +. 121.)))

let suite =
  [
    S.case "resolution renewal spans slow reads without surrendering owner"
      test_renewal;
    S.case "independent handles share revision lease and epoch fence"
      test_shared_handles;
    S.case "live fallback and changed metadata" test_fallback;
    S.case "dirty revision survives older inspection" test_dirty_race;
    S.case "transport retries cap independently of dirty hints" test_backoff;
    S.case "expired resolution owner cannot finish" test_expiry;
    S.case "ready consumed under existing lease and CAS once" test_take;
    S.case "holds transport and pause cannot accelerate" test_refusals;
  ]
