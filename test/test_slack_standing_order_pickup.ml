module S = Test_slack_standing_order_support
module F = Test_slack_standing_order_fire_store
module T = Slack_standing_order_types
module Store = Slack_standing_order_store
module C = Slack_huddle_classify
module Pickup = Slack_standing_order_pickup
module SO = Slack_standing_order_slack

let check = Alcotest.(check bool)
let integer = Alcotest.(check int)
let ts0 = "1700000010.000000"
let ts1 = "1700000310.000000"
let runtime = F.runtime
let attempt db id n = Option.get (Store.attempt (runtime db id) n)

let state db id expected =
  Alcotest.check S.fire_state_t "fire state" expected (runtime db id).fire.state

let fake_matchers : C.matchers =
  {
    first_reply =
      [ ("FAKE RATE", C.Rate_limited); ("FAKE UNDELIVERED", C.Not_delivered) ];
    any_reply = [ ("FAKE DISABLED", C.Disabled); ("FAKE SPEND", C.Spend_limit) ];
  }

let context =
  C.
    {
      self_user_id = Some "UBOT";
      targets =
        [
          {
            target_user_id = "UCLAUDE";
            target_bot_id = "BCLAUDE";
            target_app_id = "ACLAUDE";
          };
        ];
      matchers = fake_matchers;
    }

let reply ?(thread = ts0) ?(ts = "1700000011.000000") ?(bot = "BCLAUDE") text =
  C.classify context
    (`Assoc
       [
         ("type", `String "event_callback");
         ( "event",
           `Assoc
             [
               ("type", `String "message");
               ("channel", `String "C123");
               ("thread_ts", `String thread);
               ("ts", `String ts);
               ("bot_id", `String bot);
               ("app_id", `String "ACLAUDE");
               ("username", `String "Claude [working]");
               ("text", `String text);
             ] );
       ])

let reaction ?(user = "UCLAUDE") ?(channel = Some "C123") ?(item_ts = Some ts0)
    ?item_file () =
  C.Target_reaction { channel; user; item_ts; item_file }

let apply ?(team = "T123") ?(app = "A123") db obs =
  Pickup.apply ~db ~team_id:team ~api_app_id:app obs

let queued db =
  Store.int_query db "SELECT COUNT(*) FROM slack_standing_order_observations" []

let no_reads _ _ = Lwt.return (Error SO.Transport)

let clear_reads method_ _ =
  Lwt.return
    (Ok
       (match method_ with
       | "reactions.get" ->
           `Assoc
             [
               ("message", `Assoc [ ("reactions", `List []) ]);
               ("file", `Assoc [ ("reactions", `List []) ]);
             ]
       | "conversations.replies" ->
           `Assoc
             [
               ("messages", `List []);
               ("has_more", `Bool false);
               ("response_metadata", `Assoc [ ("next_cursor", `String "") ]);
             ]
       | _ -> Alcotest.fail "unexpected API"))

let confirm ?(call = no_reads) db id =
  let now = Store.now () in
  let lease =
    match (runtime db id).claim with
    | Some l when l.until > now -> l
    | _ -> F.claim db id "pickup-test" now
  in
  Lwt_main.run (Pickup.confirm ~db ~call ~lease (runtime db id) ~now)

let record db id lease n ts ids =
  let before = runtime db id in
  let a =
    {
      (Option.get (Store.attempt before n)) with
      Store.relay_ts = Some ts;
      uploaded_file_ids = ids;
    }
  in
  let after =
    {
      before with
      attempts =
        a
        :: List.filter
             (fun (x : Store.relay_attempt) -> x.attempt_no <> n)
             before.attempts;
      fire = { before.fire with state = T.Posted; relay_ts = Some ts };
    }
  in
  check "record relay" true
    (F.ok (Store.cas db ~lease ~before ~after ~now:(Store.now ())))

let fixture f =
  S.at S.now;
  F.with_handles (fun db other ->
      let o, fire = F.seed db in
      let lease = F.claim db fire.id "pickup-test" S.now in
      F.prepare db fire.id lease S.now;
      check "dispatch" true (F.ok (F.reserve db fire.id lease S.now false));
      record db fire.id lease 0 ts0 [ "FONE" ];
      f db other o fire.id lease)

let test_notice_reply () =
  fixture (fun db _ _ id _ ->
      apply db (reply "FAKE RATE");
      confirm db id;
      state db id T.Posted;
      check "notice has no acknowledgement" true
        ((attempt db id 0).acknowledged_at = None);
      apply db (reply ~ts:"1700000012.000000" "ordinary");
      confirm db id;
      state db id T.Picked_up;
      let a = attempt db id 0 in
      check "reply acknowledges and picks up" true
        (a.acknowledged_at <> None && a.picked_up_at <> None);
      apply db (reply ~ts:"1700000012.000000" "FAKE RATE");
      confirm db id;
      check "stream edit does not replace first reply" true
        ((attempt db id 0).first_reply_ts = Some "1700000011.000000");
      state db id T.Picked_up)

let test_reactions () =
  List.iter
    (fun file ->
      fixture (fun db _ _ id _ ->
          apply db (reaction ~user:"U456" ());
          apply db (reaction ~item_ts:(Some "999.000001") ());
          confirm db id;
          state db id T.Posted;
          apply db
            (if file then
               reaction ~channel:None ~item_ts:None ~item_file:"FONE" ()
             else reaction ());
          confirm db id;
          state db id T.Acknowledged;
          check "reaction persisted" true (attempt db id 0).reaction_seen))
    [ false; true ]

let test_late text () =
  fixture (fun db _ _ id _ ->
      apply db (reply "ordinary");
      confirm db id;
      state db id T.Picked_up;
      apply db (reply ~ts:"1700000012.000000" text);
      confirm db id;
      state db id T.Not_delivered)

let test_later () =
  fixture (fun db _ _ id _ ->
      apply db (reply "ordinary");
      confirm db id;
      apply db (reply ~ts:"1700000012.000000" "FAKE RATE");
      apply db (reply ~ts:"1700000013.000000" "FAKE UNDELIVERED");
      confirm db id;
      state db id T.Picked_up;
      check "later first-only notice is ordinary" true
        ((attempt db id 0).rate_limited_at = None);
      apply db (reply ~bot:"BFOREIGN" "FAKE DISABLED");
      confirm db id;
      state db id T.Picked_up)

let test_scope team app () =
  fixture (fun db other _ id lease ->
      apply ~team ~app db (reaction ());
      apply ~team ~app db (reply "ordinary");
      let foreign_queued = queued db in
      confirm other id;
      let a = attempt db id 0 in
      check "foreign cannot advance" true
        (foreign_queued = 0 && (not a.reaction_seen) && a.picked_up_at = None);
      apply db (reaction ());
      integer "matching queued" 1 (queued db);
      check "apply preserves live lease" true
        ((runtime other id).claim = Some lease);
      confirm other id;
      state db id T.Acknowledged)

let test_failed_read () =
  fixture (fun db _ o id _ ->
      S.at (S.now +. 190.);
      confirm db id;
      state db id T.Posted;
      let incomplete method_ params =
        if method_ = "conversations.replies" then
          Lwt.return
            (Ok (`Assoc [ ("messages", `List []); ("has_more", `Bool true) ]))
        else clear_reads method_ params
      in
      confirm ~call:incomplete db id;
      state db id T.Posted;
      integer "indeterminate is not failure" 0
        (Pickup.failure_streak ~db ~order_id:o.id))

let test_no_pickup () =
  fixture (fun db _ o id _ ->
      S.at (S.now +. 189.);
      confirm ~call:clear_reads db id;
      state db id T.Posted;
      S.at (S.now +. 190.);
      confirm ~call:clear_reads db id;
      state db id T.No_pickup;
      confirm ~call:clear_reads db id;
      integer "count fire once" 1 (Pickup.failure_streak ~db ~order_id:o.id))

let test_informational () =
  fixture (fun db _ o id _ ->
      apply db (reaction ());
      confirm db id;
      check "reply deadline" true
        ((runtime db id).next_attempt_at = Some (S.now +. 3610.));
      S.at (S.now +. 3610.);
      confirm ~call:clear_reads db id;
      state db id T.Acknowledged;
      check "information reason" true
        ((runtime db id).fire.reason = Some "acknowledged_no_reply");
      integer "not a failure" 0 (Pickup.failure_streak ~db ~order_id:o.id))

let second db id lease =
  check "reserve re-post" true (F.ok (F.reserve db id lease S.now true));
  record db id lease 1 ts1 [ "FTWO" ]

let test_second_deadline () =
  fixture (fun db _ o id lease ->
      second db id lease;
      S.at (S.now +. 489.);
      confirm ~call:clear_reads db id;
      state db id T.Posted;
      S.at (S.now +. 490.);
      let read_count = ref 0 in
      let call method_ params =
        if method_ = "conversations.replies" then incr read_count;
        clear_reads method_ params
      in
      confirm ~call db id;
      state db id T.No_pickup;
      integer "confirm both retained relays" 2 !read_count;
      integer "two attempts one failure" 1
        (Pickup.failure_streak ~db ~order_id:o.id))

let test_late_first () =
  fixture (fun db _ _ id lease ->
      second db id lease;
      apply db (reaction ());
      confirm db id;
      let first = attempt db id 0 and second = attempt db id 1 in
      check "only original reacted" true
        (first.reaction_seen && not second.reaction_seen);
      check "second clock unaffected" true
        (second.pickup_started_at = Some (S.now +. 310.)
        && second.acknowledged_at = None);
      apply db (reply ~thread:ts1 ~ts:"1700000311.000000" "ordinary");
      confirm db id;
      state db id T.Picked_up;
      apply db (reply "FAKE DISABLED");
      confirm db id;
      state db id T.Not_delivered;
      apply db (reply ~ts:"1700000012.000000" "FAKE RATE");
      confirm db id;
      integer "never a third attempt" 2 (List.length (runtime db id).attempts))

let test_rate_wait () =
  fixture (fun db _ _ id _ ->
      S.at (S.now +. 11.);
      apply db (reply "FAKE RATE");
      confirm db id;
      check "fixed 300s retry" true
        ((runtime db id).next_attempt_at = Some (S.now +. 311.));
      S.at (S.now +. 190.);
      confirm ~call:clear_reads db id;
      state db id T.Posted;
      apply db (reaction ());
      confirm db id;
      state db id T.Acknowledged;
      check "reaction forbids unreserved re-post" true
        (not
           (F.ok
              (Store.reserve_dispatch db ~fire_id:id
                 ~lease:(Option.get (runtime db id).claim)
                 ~repost:true ~now:(Store.now ())))))

let test_pause () =
  fixture (fun db _ o id _ ->
      ignore
        (S.fire db o ~state:T.No_pickup ~dispatched_at:(S.now -. 20.) ~room:"R2"
           ());
      ignore
        (S.fire db o ~state:T.Not_delivered ~dispatched_at:(S.now -. 10.)
           ~room:"R3" ());
      S.at (S.now +. 190.);
      confirm ~call:clear_reads db id;
      integer "three failures" 3 (Pickup.failure_streak ~db ~order_id:o.id);
      let paused = Option.get (Store.find db o.id) in
      Alcotest.check S.state_t "auto paused" T.Paused paused.state;
      integer "one epoch change" (o.epoch + 1) paused.epoch;
      confirm ~call:clear_reads db id;
      integer "only paused once" paused.epoch
        (Option.get (Store.find db o.id)).epoch;
      apply db (reply "ordinary");
      confirm db id;
      state db id T.Picked_up;
      integer "pickup resets streak" 0
        (Pickup.failure_streak ~db ~order_id:o.id))

let test_accounting () =
  fixture (fun db _ o id _ ->
      let f =
        S.fire db o ~state:T.Failed ~dispatched_at:(S.now -. 10.) ~room:"R2" ()
      in
      List.iteri
        (fun i st ->
          ignore (S.fire db o ~state:st ~room:("R" ^ string_of_int (i + 3)) ()))
        [ T.No_notes; T.Unavailable; T.Skipped_cap; T.Skipped_no_transcript ];
      integer "neutral preserves failure" 1
        (Pickup.failure_streak ~db ~order_id:o.id);
      apply db (reply "ordinary");
      confirm db id;
      integer "dispatch order reset" 0
        (Pickup.failure_streak ~db ~order_id:o.id);
      apply db (reply ~ts:"1700000012.000000" "FAKE SPEND");
      confirm db id;
      integer "late revision once" 2 (Pickup.failure_streak ~db ~order_id:o.id);
      confirm db id;
      integer "no double counting" 2 (Pickup.failure_streak ~db ~order_id:o.id);
      check "old failure intact" true ((runtime db f.id).fire.state = T.Failed))

let test_scope_after_persist () =
  fixture (fun db other _ id _ ->
      apply db (reaction ());
      Store.exec db
        "UPDATE slack_standing_order_observations SET api_app_id='AFOREIGN'" [];
      confirm other id;
      state db id T.Posted;
      check "scope rechecked" false (attempt db id 0).reaction_seen)

let test_pre_relay_file () =
  F.with_handles (fun db _ ->
      let _, f = F.seed db in
      let lease = F.claim db f.id "pickup-test" S.now in
      F.prepare db f.id lease S.now;
      check "reserve" true (F.ok (F.reserve db f.id lease S.now false));
      let before = runtime db f.id in
      let a =
        {
          (Option.get (Store.attempt before 0)) with
          uploaded_file_ids = [ "FONE" ];
        }
      in
      check "persist unresolved upload" true
        (F.ok
           (Store.cas db ~lease ~before
              ~after:{ before with attempts = [ a ] }
              ~now:S.now));
      apply db (reaction ~channel:None ~item_ts:None ~item_file:"FONE" ());
      confirm db f.id;
      state db f.id T.Acknowledged;
      check "no invented pickup clock" true
        ((attempt db f.id 0).pickup_started_at = None))

let test_pages () =
  fixture (fun db _ _ id _ ->
      S.at (S.now +. 190.);
      let calls = ref 0 in
      let call method_ params =
        if method_ <> "conversations.replies" then clear_reads method_ params
        else (
          incr calls;
          Lwt.return
            (Ok
               (`Assoc
                  [
                    ("messages", `List []);
                    ("has_more", `Bool true);
                    ( "response_metadata",
                      `Assoc [ ("next_cursor", `String (string_of_int !calls)) ]
                    );
                  ])))
      in
      confirm ~call db id;
      state db id T.Posted;
      integer "bounded at five pages" 5 !calls;
      let calls = ref 0 in
      let call method_ params =
        if method_ <> "conversations.replies" then clear_reads method_ params
        else (
          incr calls;
          if !calls = 1 then
            Lwt.return
              (Ok
                 (`Assoc
                    [
                      ("messages", `List []);
                      ("has_more", `Bool true);
                      ( "response_metadata",
                        `Assoc [ ("next_cursor", `String "next") ] );
                    ]))
          else
            Lwt.return
              (Ok
                 (`Assoc
                    [
                      ( "messages",
                        `List
                          [
                            `Assoc
                              [
                                ("ts", `String "1700000012.000000");
                                ("thread_ts", `String ts0);
                                ("bot_id", `String "BCLAUDE");
                                ("app_id", `String "ACLAUDE");
                                ( "text",
                                  `String "reply with truncated ordinary text"
                                );
                                ("blocks", `List []);
                              ];
                          ] );
                      ("has_more", `Bool false);
                      ( "response_metadata",
                        `Assoc [ ("next_cursor", `String "") ] );
                    ])))
      in
      confirm ~call db id;
      state db id T.Picked_up;
      integer "second page checked" 2 !calls)

let test_read_reaction () =
  fixture (fun db _ _ id _ ->
      S.at (S.now +. 190.);
      let call method_ params =
        if method_ = "reactions.get" then
          Lwt.return
            (Ok
               (`Assoc
                  [
                    ( "message",
                      `Assoc
                        [
                          ( "reactions",
                            `List
                              [
                                `Assoc
                                  [
                                    ("name", `String "anything");
                                    ("count", `Int 1);
                                    ("users", `List [ `String "UCLAUDE" ]);
                                  ];
                              ] );
                        ] );
                    ("file", `Assoc [ ("reactions", `List []) ]);
                  ]))
        else clear_reads method_ params
      in
      confirm ~call db id;
      state db id T.Acknowledged)

let test_corrupt_queue () =
  fixture (fun db other _ id _ ->
      apply db (reply "FAKE RATE");
      let payload =
        Store.query db "SELECT payload FROM slack_standing_order_observations"
          [] (fun s -> Sql_util.text_column s 0)
      in
      check "no notice text or username persisted" true
        (List.for_all
           (fun p ->
             (not (String_util.contains p "FAKE"))
             && not (String_util.contains p "working"))
           payload);
      Store.exec db
        "INSERT INTO slack_standing_order_observations \
         (fire_id,attempt_no,team_id,api_app_id,payload,observed_at) VALUES \
         (?,0,'T123','A123','[]',?)"
        [ Store.text id; Sqlite3.Data.FLOAT S.now ];
      check "corrupt queue fails closed" true
        (try
           confirm other id;
           false
         with Failure _ -> true);
      integer "transaction retains both hints" 2 (queued db);
      check "no partial reduction" true ((attempt db id 0).first_reply_ts = None))

let test_lease_loss () =
  fixture (fun db other _ id lease ->
      apply db (reply "ordinary");
      S.at lease.until;
      let successor = F.claim other id "new-worker" (Store.now ()) in
      Lwt_main.run
        (Pickup.confirm ~db
           ~call:(fun _ _ -> Alcotest.fail "stale lease cannot read")
           ~lease (runtime db id) ~now:(Store.now ()));
      integer "hint retained for successor" 1 (queued db);
      Lwt_main.run
        (Pickup.confirm ~db:other ~call:no_reads ~lease:successor
           (runtime other id) ~now:(Store.now ()));
      state db id T.Picked_up)

let test_pause_epoch () =
  fixture (fun db _ o id _ ->
      ignore
        (F.ok
           (Store.deactivate db ~order_id:o.id (Store.Pause "U456") ~now:S.now));
      let current = Option.get (Store.find db o.id) in
      check "old epoch cannot pause again" false
        (F.ok
           (Store.pause_if_epoch db ~order_id:o.id ~epoch:o.epoch
              ~by:"automatic_failures" ~now:S.now));
      integer "unchanged epoch" current.epoch
        (Option.get (Store.find db o.id)).epoch)

let test_pause_rollback () =
  fixture (fun db _ o id _ ->
      ignore (S.fire db o ~state:T.Failed ~room:"R2" ());
      ignore (S.fire db o ~state:T.Failed ~room:"R3" ());
      Store.exec db
        "CREATE TRIGGER reject_pause BEFORE UPDATE ON slack_standing_orders \
         WHEN NEW.state='paused' BEGIN SELECT RAISE(FAIL,'test pause \
         failure'); END"
        [];
      S.at (S.now +. 190.);
      check "pause failure surfaced" true
        (try
           confirm ~call:clear_reads db id;
           false
         with Failure _ -> true);
      state db id T.Posted;
      Alcotest.check S.state_t "order remains active" T.Active
        (Option.get (Store.find db o.id)).state;
      Store.exec db "DROP TRIGGER reject_pause" [];
      confirm ~call:clear_reads db id;
      state db id T.No_pickup;
      Alcotest.check S.state_t "retry pauses atomically" T.Paused
        (Option.get (Store.find db o.id)).state)

let test_reopen () =
  let path = Filename.temp_file "huddle-pickup" ".db" in
  let db = Sqlite3.db_open path in
  let _, f = F.seed db in
  let lease = F.claim db f.id "pickup-test" S.now in
  F.prepare db f.id lease S.now;
  check "dispatch" true (F.ok (F.reserve db f.id lease S.now false));
  record db f.id lease 0 ts0 [ "FONE" ];
  apply ~team:"TFOREIGN" db (reply "ordinary");
  apply db (reaction ~channel:None ~item_ts:None ~item_file:"FONE" ());
  ignore (Sqlite3.db_close db);
  let db = Sqlite3.db_open path in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sqlite3.db_close db);
      Sys.remove path)
    (fun () ->
      integer "only matching scope durable" 1 (queued db);
      confirm db f.id;
      state db f.id T.Acknowledged)

let test_expiry_during_read () =
  fixture (fun db _ _ id _ ->
      S.at (S.now +. 190.);
      let lease = F.claim db id "pickup-test" (Store.now ()) in
      let call method_ params =
        S.at lease.until;
        clear_reads method_ params
      in
      Lwt_main.run
        (Pickup.confirm ~db ~call ~lease (runtime db id) ~now:(S.now +. 190.));
      state db id T.Posted;
      check "expired worker cannot schedule" true
        ((runtime db id).next_attempt_at = None))

let test_hard_read () =
  fixture (fun db _ o id _ ->
      confirm
        ~call:(fun _ _ -> Lwt.return (Error (SO.Slack_error "missing_scope")))
        db id;
      let order = Option.get (Store.find db o.id) in
      Alcotest.check S.state_t "hard read suspends" T.Suspended order.state;
      check "method retained" true
        (order.suspended_reason = Some "missing_scope:reactions.get");
      state db id T.Posted)

let test_no_new_failure () =
  fixture (fun db _ o id _ ->
      List.iter
        (fun room ->
          let f = S.fire db o ~state:T.Failed ~room () in
          Store.exec db
            "UPDATE slack_standing_order_fires SET epoch=? WHERE id=?"
            [ Store.int (o.epoch - 1); Store.text f.id ])
        [ "R2"; "R3"; "R4" ];
      confirm db id;
      integer "old streak retained" 3 (Pickup.failure_streak ~db ~order_id:o.id);
      Alcotest.check S.state_t "new attempt has no failed result" T.Active
        (Option.get (Store.find db o.id)).state)

let test_unreserved_repost () =
  fixture (fun db _ _ id lease ->
      let before = runtime db id in
      let after =
        {
          before with
          current_attempt = 1;
          attempts = before.attempts @ [ Store.empty_attempt 1 ];
          fire = { before.fire with state = T.Uploading; relay_ts = None };
        }
      in
      check "prepare unreserved re-post" true
        (F.ok (Store.cas db ~lease ~before ~after ~now:S.now));
      apply db (reaction ());
      confirm db id;
      state db id T.Acknowledged;
      let first = attempt db id 0 and second = attempt db id 1 in
      check "only original has evidence" true
        (first.reaction_seen && (not second.reaction_seen)
        && second.acknowledged_at = None);
      check "second clock remains unresolved" true
        (second.pickup_started_at = None && second.dispatched_at = None);
      check "new reservation suppressed" false
        (F.ok (F.reserve db id lease S.now true)))

let test_refused_repost hard observation expected () =
  fixture (fun db _ o id lease ->
      let before = runtime db id in
      let prepared =
        {
          (Store.empty_attempt 1) with
          uploaded_file_ids = [ "FTWO" ];
          upload_started_at = Some S.now;
        }
      in
      let after =
        {
          before with
          current_attempt = 1;
          attempts = before.attempts @ [ prepared ];
          fire = { before.fire with state = T.Uploading; relay_ts = None };
        }
      in
      check "prepare re-post" true
        (F.ok (Store.cas db ~lease ~before ~after ~now:S.now));
      if hard then
        check "hard refusal suspends" true
          (F.ok
             (Store.deny_and_suspend_if_epoch db ~lease ~before:(runtime db id)
                ~reason:"missing_scope:files.info" ~now:S.now))
      else (
        check "pause wins" true
          (F.ok
             (Store.pause_if_epoch db ~order_id:o.id ~epoch:o.epoch ~by:"U456"
                ~now:S.now));
        check "inactive refusal recorded" true
          (F.ok (F.change db lease (runtime db id) T.Skipped_inactive S.now)));
      state db id (if hard then T.Denied else T.Skipped_inactive);
      let inactive = Option.get (Store.find db o.id) in
      Alcotest.check S.state_t "inactive order"
        (if hard then T.Suspended else T.Paused)
        inactive.state;
      integer "refusal advances epoch once" (o.epoch + 1) inactive.epoch;
      let assert_fenced () =
        check "inactive order and epoch unchanged" true
          (Store.find db o.id = Some inactive);
        check "original fire epoch retained" true
          ((runtime db id).fire.epoch = o.epoch);
        check "prepared attempt entirely unchanged" true
          (attempt db id 1 = prepared);
        check "no new reservation" false
          (F.ok (F.reserve db id lease S.now true));
        integer "only original dispatch debited" 1
          (Store.dispatches_since db ~order_id:o.id ~since:0.)
      in
      assert_fenced ();
      List.iter
        (fun target ->
          check "state change without retained evidence rejected" true
            (Result.is_error (F.change db lease (runtime db id) target S.now)))
        T.[ Acknowledged; Picked_up; Not_delivered ];
      apply db observation;
      integer "late observation queued" 1 (queued db);
      confirm db id;
      state db id expected;
      integer "late observation consumed" 0 (queued db);
      let original = attempt db id 0 in
      check "matching original evidence recorded" true
        (match expected with
        | T.Acknowledged -> original.reaction_seen
        | T.Picked_up -> original.picked_up_at <> None
        | T.Not_delivered -> original.not_delivered
        | _ -> false);
      assert_fenced ();
      if expected = T.Picked_up then
        List.iteri
          (fun i text ->
            apply db
              (reply ~ts:(Printf.sprintf "170000001%d.000000" (i + 2)) text);
            confirm db id;
            state db id T.Not_delivered;
            integer "late override consumed" 0 (queued db);
            assert_fenced ())
          [ "FAKE DISABLED"; "FAKE SPEND" ])

let test_malformed_reads () =
  List.iter
    (fun malformed_reaction ->
      fixture (fun db _ _ id _ ->
          S.at (S.now +. 190.);
          let call method_ params =
            if malformed_reaction && method_ = "reactions.get" then
              let reactions =
                `List
                  [
                    `Assoc
                      [
                        ("count", `Int 2);
                        ("users", `List [ `String "U456"; `String "U456" ]);
                      ];
                  ]
              in
              Lwt.return
                (Ok
                   (`Assoc
                      [
                        ("message", `Assoc [ ("reactions", reactions) ]);
                        ("file", `Assoc [ ("reactions", reactions) ]);
                      ]))
            else if
              (not malformed_reaction) && method_ = "conversations.replies"
            then
              Lwt.return
                (Ok
                   (`Assoc
                      [
                        ( "messages",
                          `List
                            [
                              `Assoc
                                [
                                  ("ts", `String "1700000012.000000");
                                  ("bot_id", `String "BCLAUDE");
                                  ("text", `String "ordinary");
                                ];
                            ] );
                        ("has_more", `Bool false);
                        ( "response_metadata",
                          `Assoc [ ("next_cursor", `String "") ] );
                      ]))
            else clear_reads method_ params
          in
          confirm ~call db id;
          state db id T.Posted))
    [ true; false ]

let test_own_relay () =
  F.with_handles (fun db other ->
      let _, f = F.seed db in
      let lease = F.claim db f.id "pickup-test" S.now in
      F.prepare db f.id lease S.now;
      check "dispatch" true (F.ok (F.reserve db f.id lease S.now false));
      let before = runtime db f.id in
      let nonce = String.make 32 'a' in
      let a =
        {
          (Option.get (Store.attempt before 0)) with
          uploaded_file_ids = [ "FONE" ];
        }
      in
      check "persist unresolved relay" true
        (F.ok
           (Store.cas db ~lease ~before
              ~after:
                {
                  before with
                  nonce = Some nonce;
                  attempts = [ a ];
                  fire = { before.fire with state = T.Posting_unknown };
                }
              ~now:S.now));
      let observation =
        C.Own_relay { channel = "C123"; ts = ts0; file_ids = [ "FONE" ]; nonce }
      in
      apply ~team:"TFOREIGN" db observation;
      apply ~app:"AFOREIGN" db observation;
      apply db
        (C.Own_relay
           { channel = "C123"; ts = ts0; file_ids = [ "FOTHER" ]; nonce });
      integer "foreign envelope or files ignored" 0 (queued db);
      apply db observation;
      integer "own relay hint queued" 1 (queued db);
      check "observer never takes lease" true
        ((runtime other f.id).claim = Some lease);
      confirm other f.id;
      state db f.id T.Posted;
      check "resolved relay clock" true
        ((attempt db f.id 0).relay_ts = Some ts0
        && (attempt db f.id 0).pickup_started_at = Some (S.now +. 10.)))

let suite =
  [
    S.case "first reply matching notice never picks up; unmatched does"
      test_notice_reply;
    S.case "any emoji on message or file acknowledges" test_reactions;
    S.case "late disabled overrides pickup" (test_late "FAKE DISABLED");
    S.case "late spend limit overrides pickup" (test_late "FAKE SPEND");
    S.case "first-only phrases on later replies and username never misclassify"
      test_later;
    S.case "foreign envelope team cannot advance matching relay"
      (test_scope "TFOREIGN" "A123");
    S.case "foreign Clawq app cannot advance matching reaction"
      (test_scope "T123" "AFOREIGN");
    S.case "persisted foreign scope cannot advance matching reaction"
      test_scope_after_persist;
    S.case "failed pickup read stays pending" test_failed_read;
    S.case "no pickup confirmed after 180" test_no_pickup;
    S.case "ack with no reply at 3600 is informational" test_informational;
    S.case "second attempt deadline independent of first" test_second_deadline;
    S.case "late first attempt notices and reactions retained" test_late_first;
    S.case "rate-limited re-post wait survives first deadline" test_rate_wait;
    S.case "three consecutive failures pause once" test_pause;
    S.case "failure results follow dispatch order with late revisions"
      test_accounting;
    S.case "file reaction before relay timestamp acknowledges"
      test_pre_relay_file;
    S.case "reply pagination is bounded and recovers a lost reply" test_pages;
    S.case "confirmation recovers a lost acknowledgement reaction"
      test_read_reaction;
    S.case "typed queue rejects corruption transactionally" test_corrupt_queue;
    S.case "expired lease cannot consume queued observations" test_lease_loss;
    S.case "automatic pause fences old epochs" test_pause_epoch;
    S.case "accounting and automatic pause roll back together"
      test_pause_rollback;
    S.case "scope and observations survive persistence and reopen" test_reopen;
    S.case "lease expiry during confirmation cannot schedule"
      test_expiry_during_read;
    S.case "hard pickup read preserves exact missing scope method"
      test_hard_read;
    S.case "historical failures cannot pause a new unfailed fire"
      test_no_new_failure;
    S.case "late original reaction suppresses prepared unreserved re-post"
      test_unreserved_repost;
    S.case "late original reaction after paused re-post"
      (test_refused_repost false (reaction ()) T.Acknowledged);
    S.case "late original reply after paused re-post"
      (test_refused_repost false (reply "ordinary") T.Picked_up);
    S.case "late original disabled after paused re-post"
      (test_refused_repost false (reply "FAKE DISABLED") T.Not_delivered);
    S.case "late original spend limit after paused re-post"
      (test_refused_repost false (reply "FAKE SPEND") T.Not_delivered);
    S.case "late original reaction after hard refused re-post"
      (test_refused_repost true (reaction ()) T.Acknowledged);
    S.case "late original reply after hard refused re-post"
      (test_refused_repost true (reply "ordinary") T.Picked_up);
    S.case "late original disabled after hard refused re-post"
      (test_refused_repost true (reply "FAKE DISABLED") T.Not_delivered);
    S.case "late original spend limit after hard refused re-post"
      (test_refused_repost true (reply "FAKE SPEND") T.Not_delivered);
    S.case "malformed reaction and target reply reads remain indeterminate"
      test_malformed_reads;
    S.case "scoped own relay observations resolve timestamps under lease"
      test_own_relay;
  ]
