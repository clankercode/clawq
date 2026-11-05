module S = Test_slack_standing_order_support
module E = Test_slack_standing_order_exec_support
module T = Slack_standing_order_types
module Store = Slack_standing_order_store
module D = Slack_standing_order_dispatch
module P = Slack_standing_order_provenance
module Ctx = Slack_standing_order_ctx
module SO = Slack_standing_order_slack

let check = Alcotest.(check bool)
let integer = Alcotest.(check int)
let ok = function Ok x -> x | Error _ -> Alcotest.fail "expected success"
let ts0 = "1700000041.000001"
let ts1 = "1700000341.000001"
let nonce = String.make 32 'a'

let message ts ids =
  `Assoc
    [
      ("ts", `String ts);
      ("user", `String "UBOT");
      ("bot_id", `String "BOWN");
      ("text", `String (Slack_relay_guard.footer ~nonce));
      ("files", `List (List.map (fun id -> `Assoc [ ("id", `String id) ]) ids));
    ]

let page messages = S.ok [ ("messages", `List messages); S.no_more ]

let persist e lease before after =
  check "fixture CAS" true
    (ok (Store.cas e.E.db ~lease ~before ~after ~now:(Store.now ())))

let seed ?(ids = [ "FORIGINAL" ]) e lease r unknown =
  let a =
    {
      (Store.empty_attempt 0) with
      uploaded_file_ids = ids;
      upload_started_at = Some (Store.now ());
    }
  in
  persist e lease r
    {
      r with
      nonce = Some nonce;
      attempts = [ a ];
      fire = { r.fire with state = T.Uploading };
    };
  check "first reservation" true
    (ok
       (Store.reserve_dispatch e.db ~fire_id:r.fire.id ~lease ~repost:false
          ~now:(Store.now ())));
  let r = E.runtime e r.fire.id in
  let a =
    {
      (Option.get (Store.attempt r 0)) with
      relay_ts = (if unknown then None else Some ts0);
      first_reply_ts = (if unknown then None else Some "1700000042.000001");
      rate_limited_at = (if unknown then None else Some (Store.now ()));
    }
  in
  persist e lease r
    {
      r with
      attempts = [ a ];
      fire =
        {
          r.fire with
          state = (if unknown then T.Posting_unknown else T.Posted);
          relay_ts = a.relay_ts;
          reason = Some "complete_unknown";
        };
    };
  E.runtime e r.fire.id

let pause e =
  ignore
    (ok
       (Store.deactivate e.E.db ~order_id:e.order.id (Store.Pause "U123")
          ~now:(Store.now ())))

let history e messages =
  S.override e.E.mock "conversations.history" (fun params ->
      ( 200,
        if P.field "limit" params = "1" || E.get "limit" params = `Int 1 then
          Test_slack_standing_order_provenance.set "ok" (`Bool true)
            (Test_slack_standing_order_provenance.page [ !(e.root) ])
        else page messages ))

let pickup_reads e =
  S.override e.E.mock "reactions.get" (fun _ ->
      ( 200,
        S.ok
          [
            ("message", `Assoc [ ("reactions", `List []) ]);
            ("file", `Assoc [ ("reactions", `List []) ]);
          ] ))

let fresh_lease e id =
  Option.get
    (ok
       (Store.claim e.E.db ~fire_id:id ~owner:"executor-test"
          ~now:(Store.now ())))

let call_repost e lease r payload =
  D.repost ~ctx:(E.ctx e) ~db:e.E.db ~manager:e.manager ~http:e.mock.http
    ~upload_http:(fun _ ->
      Lwt.return
        Slack_standing_order_upload.{ status = 200; headers = []; body = "" })
    ~lease r payload

let test_rate ?(both = false) fixture () =
  fixture (fun (e : E.env) lease (r : Store.fire_runtime) payload ->
      let r = seed e lease r false in
      if both then
        Store.exec e.db
          "UPDATE slack_standing_order_fires SET state='posting_unknown' WHERE \
           id=?"
          [ Store.text r.fire.id ];
      let notice = Store.now () in
      history e [ message ts0 [ "FORIGINAL" ] ];
      pickup_reads e;
      S.override e.mock "files.completeUploadExternal" (fun _ ->
          ( 200,
            S.ok
              [
                ( "files",
                  `List
                    [
                      `Assoc
                        [
                          ( "shares",
                            `Assoc
                              [
                                ( "private",
                                  `Assoc
                                    [
                                      ( "C123",
                                        `List [ `Assoc [ ("ts", `String ts1) ] ]
                                      );
                                    ] );
                              ] );
                        ];
                    ] );
              ] ));
      S.at (notice +. 299.);
      let lease = fresh_lease e r.fire.id in
      Lwt_main.run (call_repost e lease r payload);
      integer "no attempt 1 at 299" 0 (E.count e "files.completeUploadExternal");
      S.at (notice +. 300.);
      Lwt_main.run (call_repost e lease (E.runtime e r.fire.id) payload);
      Lwt_main.run (call_repost e lease (E.runtime e r.fire.id) payload);
      let saved = E.runtime e r.fire.id in
      integer "one attempt 1 at 300" 1
        (E.count e "files.completeUploadExternal");
      integer "two retained identities" 2 (List.length saved.attempts);
      integer "one repost debit" 1 saved.fire.reposts;
      check "fresh relay identity" true
        ((Option.get (Store.attempt saved 1)).relay_ts = Some ts1);
      integer "cap includes repost" 2
        (Store.dispatches_since e.db ~order_id:e.order.id ~since:0.);
      check "original retained" true
        ((Option.get (Store.attempt saved 0)).relay_ts = Some ts0))

let test_adopt ?(terminal = false) fixture paused () =
  fixture (fun (e : E.env) lease (r : Store.fire_runtime) payload ->
      let r = seed e lease r false in
      check "second reserved before crash" true
        (ok
           (Store.reserve_dispatch e.db ~fire_id:r.fire.id ~lease ~repost:true
              ~now:(Store.now ())));
      let r = E.runtime e r.fire.id in
      persist e lease r
        { r with fire = { r.fire with state = T.Posting_unknown } };
      (if terminal then
         let r = E.runtime e r.fire.id in
         let first =
           { (Option.get (Store.attempt r 0)) with not_delivered = true }
         in
         persist e lease r
           {
             r with
             attempts = [ first; Option.get (Store.attempt r 1) ];
             fire = { r.fire with state = T.Not_delivered };
           });
      if paused then pause e
      else
        Store.exec e.db
          "UPDATE slack_standing_orders SET max_fires_per_day=2 WHERE id=?"
          [ Store.text e.order.id ];
      history e [ message ts0 [ "FORIGINAL" ]; message ts1 [ "FSECOND" ] ];
      Lwt_main.run (call_repost e lease (E.runtime e r.fire.id) payload);
      let saved = E.runtime e r.fire.id in
      let second = Option.get (Store.attempt saved 1) in
      if terminal then E.check_state e r.fire.id T.Not_delivered;
      check "adopts second beside original" true
        (second.relay_ts = Some ts1 && second.uploaded_file_ids = [ "FSECOND" ]);
      Alcotest.(check (option (float 0.000001)))
        "fresh pickup clock" (Some 1700000341.000001) second.pickup_started_at;
      integer "both identities retained" 2 (List.length saved.attempts);
      integer "existing repost debit" 1 saved.fire.reposts;
      integer "adoption keeps debit exactly once" 2
        (Store.dispatches_since e.db ~order_id:e.order.id ~since:0.);
      E.no_upload e)

let test_unknown fixture () =
  fixture (fun (e : E.env) lease (r : Store.fire_runtime) payload ->
      let r = seed e lease r true in
      let ctx =
        {
          (E.ctx e) with
          Ctx.wait =
            (fun ~seconds ->
              S.at (Store.now () +. seconds);
              Lwt.return_unit);
        }
      in
      Lwt_main.run
        (D.repost ~ctx ~db:e.db ~manager:e.manager ~http:e.mock.http
           ~upload_http:(fun _ -> Alcotest.fail "unknown must not upload")
           ~lease r payload);
      E.check_state e r.fire.id T.Posting_unknown;
      E.no_upload e)

let test_pause fixture unknown () =
  fixture (fun (e : E.env) lease (r : Store.fire_runtime) payload ->
      let r = seed e lease r unknown in
      pause e;
      let before = E.runtime e r.fire.id in
      let ctx =
        {
          (E.ctx e) with
          Ctx.wait =
            (fun ~seconds ->
              S.at (Store.now () +. seconds);
              Lwt.return_unit);
        }
      in
      Lwt_main.run
        (D.repost ~ctx ~db:e.db ~manager:e.manager ~http:e.mock.http ~lease
           before payload);
      let saved = E.runtime e r.fire.id in
      check "pause retains prior status" true
        (saved.fire.state = before.fire.state);
      integer "no repost debit" 0 saved.fire.reposts;
      E.no_upload e)

let test_precedence fixture () =
  fixture (fun (e : E.env) _lease (r : Store.fire_runtime) _payload ->
      let a = { (Store.empty_attempt 0) with relay_ts = Some ts0 } in
      let result =
        Lwt_main.run
          (D.resolve_share ~ctx:(E.ctx e)
             ~call:(fun _ _ -> Alcotest.fail "complete evidence precedes reads")
             ~order:e.order ~attempt_no:0
             { r with attempts = [ a ] })
      in
      check "complete first" true (result = D.Shared ts0);
      let observed = { r with attempts = [ a ] } in
      let result =
        Lwt_main.run
          (D.resolve_share ~ctx:(E.ctx e)
             ~observe:(fun () -> Some observed)
             ~call:(fun _ _ -> Alcotest.fail "observer precedes files.info")
             ~order:e.order ~attempt_no:0 r)
      in
      check "observer second" true (result = D.Shared ts0);
      let a = { (Store.empty_attempt 0) with uploaded_file_ids = [ "FONE" ] } in
      let call method_ _ =
        Alcotest.(check string) "files info third" "files.info" method_;
        Lwt.return
          (Ok
             (`Assoc
                [
                  ( "file",
                    `Assoc
                      [
                        ("id", `String "FONE");
                        ( "shares",
                          `Assoc
                            [
                              ( "private",
                                `Assoc
                                  [
                                    ( "C123",
                                      `List [ `Assoc [ ("ts", `String ts1) ] ]
                                    );
                                  ] );
                            ] );
                      ] );
                ]))
      in
      let result =
        Lwt_main.run
          (D.resolve_share ~ctx:(E.ctx e) ~call ~order:e.order ~attempt_no:0
             { r with attempts = [ a ] })
      in
      check "files info third" true (result = D.Shared ts1))

let test_absence fixture () =
  fixture (fun (e : E.env) _lease (r : Store.fire_runtime) _payload ->
      List.iter
        (fun kind ->
          let a =
            {
              (Store.empty_attempt 0) with
              uploaded_file_ids = [ "FONE"; "FTWO" ];
              dispatched_at = Some (Store.now ());
            }
          in
          let r =
            {
              r with
              nonce = Some nonce;
              attempts = [ a ];
              fire =
                {
                  r.fire with
                  reason =
                    Some
                      (if kind = `Successful then "complete_unknown"
                       else "complete_rejected_before_share");
                };
            }
          in
          let ctx =
            {
              (E.ctx e) with
              Ctx.wait =
                (fun ~seconds ->
                  S.at (Store.now () +. seconds);
                  Lwt.return_unit);
            }
          in
          let call method_ params =
            Lwt.return
              (if method_ = "files.info" then
                 if kind = `Unavailable then Error (SO.Http_status 503)
                 else
                   Ok
                     (`Assoc
                        [
                          ( "file",
                            `Assoc
                              [
                                ("id", List.assoc "file" params);
                                ( "shares",
                                  if kind = `Missing then `Null
                                  else
                                    `Assoc
                                      [
                                        ("public", `Assoc []);
                                        ("private", `Assoc []);
                                      ] );
                              ] );
                        ])
               else if kind = `Truncated then
                 Ok
                   (`Assoc [ ("messages", `List []); ("has_more", `Bool true) ])
               else Ok (page []))
          in
          let result =
            Lwt_main.run
              (D.resolve_share ~ctx ~call ~order:e.order ~attempt_no:0 r)
          in
          check "all three proofs required" true
            (result = if kind = `Rejected then D.Proven_absent else D.Ambiguous))
        [ `Rejected; `Successful; `Missing; `Unavailable; `Truncated ])

let test_wait_cancel fixture () =
  fixture (fun (e : E.env) _lease (r : Store.fire_runtime) _payload ->
      let pending_waits = ref 0 in
      let ctx =
        {
          (E.ctx e) with
          Ctx.wait =
            (fun ~seconds:_ ->
              incr pending_waits;
              let p, _ = Lwt.task () in
              Lwt.on_cancel p (fun () -> decr pending_waits);
              p);
        }
      in
      let a = { (Store.empty_attempt 0) with uploaded_file_ids = [ "FONE" ] } in
      let call _ _ =
        let open Lwt.Syntax in
        let* () = Lwt.pause () in
        Lwt.return
          (Ok
             (`Assoc
                [
                  ( "file",
                    `Assoc
                      [
                        ("id", `String "FONE");
                        ( "shares",
                          `Assoc
                            [
                              ( "private",
                                `Assoc
                                  [
                                    ( "C123",
                                      `List [ `Assoc [ ("ts", `String ts1) ] ]
                                    );
                                  ] );
                            ] );
                      ] );
                ]))
      in
      let result =
        Lwt_main.run
          (D.resolve_share ~ctx ~call ~order:e.order ~attempt_no:0
             { r with attempts = [ a ] })
      in
      check "early share" true (result = D.Shared ts1);
      integer "no pending waits after early success" 0 !pending_waits)

let test_suppress fixture mode () =
  fixture (fun (e : E.env) lease (r : Store.fire_runtime) payload ->
      let r = seed e lease r false in
      let notice = Store.now () in
      history e [ message ts0 [ "FORIGINAL" ] ];
      pickup_reads e;
      S.at (notice +. 300.);
      let lease = fresh_lease e r.fire.id in
      (match mode with
      | `Cap ->
          Store.exec e.db
            "UPDATE slack_standing_orders SET max_fires_per_day=1 WHERE id=?"
            [ Store.text e.order.id ]
      | `Reaction ->
          S.override e.mock "reactions.get" (fun _ ->
              let item =
                `Assoc
                  [
                    ( "reactions",
                      `List
                        [
                          `Assoc
                            [
                              ("users", `List [ `String "UCLAUDE" ]);
                              ("count", `Int 1);
                            ];
                        ] );
                  ]
              in
              (200, S.ok [ ("file", item); ("message", item) ]))
      | `Reply ->
          e.replies :=
            Test_slack_standing_order_provenance.page
              [
                `Assoc
                  [
                    ("ts", `String "1700000340.000001");
                    ("user", `String "UCLAUDE");
                  ];
              ]
      | `Pause -> pause e);
      Lwt_main.run (call_repost e lease (E.runtime e r.fire.id) payload);
      E.no_upload e;
      let saved = E.runtime e r.fire.id in
      integer "no new debit" 0 saved.fire.reposts;
      check "original identity retained" true
        ((Option.get (Store.attempt saved 0)).relay_ts = Some ts0);
      if mode = `Cap then E.check_state e r.fire.id T.Skipped_cap)

let test_paused_first fixture () =
  fixture (fun (e : E.env) lease (r : Store.fire_runtime) payload ->
      let r = seed e lease r true in
      pause e;
      S.override e.mock "files.info" (fun params ->
          ( 200,
            S.ok
              [
                ( "file",
                  `Assoc
                    [
                      ("id", E.get "file" params);
                      ( "shares",
                        `Assoc
                          [
                            ( "private",
                              `Assoc
                                [
                                  ( "C123",
                                    `List [ `Assoc [ ("ts", `String ts0) ] ] );
                                ] );
                          ] );
                    ] );
              ] ));
      Lwt_main.run (call_repost e lease (E.runtime e r.fire.id) payload);
      check "paused original share captured under Track" true
        ((Option.get (Store.attempt (E.runtime e r.fire.id) 0)).relay_ts
       = Some ts0);
      E.no_upload e)

let test_complete_timeout fixture second () =
  fixture (fun (e : E.env) lease (r : Store.fire_runtime) payload ->
      let r, lease =
        if second then (
          let r = seed e lease r false in
          let notice = Store.now () in
          history e [ message ts0 [ "FORIGINAL" ] ];
          pickup_reads e;
          S.at (notice +. 300.);
          (r, fresh_lease e r.fire.id))
        else (r, lease)
      in
      let completes = ref 0 and uploads = ref 0 in
      let http ~verb ~token ~method_ ~params =
        if method_ = "files.completeUploadExternal" then (
          incr completes;
          Lwt.fail Lwt_unix.Timeout)
        else e.mock.http ~verb ~token ~method_ ~params
      in
      let upload_http _ =
        incr uploads;
        Lwt.return
          Slack_standing_order_upload.{ status = 200; headers = []; body = "" }
      in
      let ctx =
        {
          (E.ctx e) with
          Ctx.wait =
            (fun ~seconds ->
              S.at (Store.now () +. seconds);
              Lwt.return_unit);
        }
      in
      Lwt_main.run
        ((if second then D.repost else D.deliver)
           ~ctx ~db:e.db ~manager:e.manager ~http ~upload_http ~lease r payload);
      E.check_state e r.fire.id T.Posting_unknown;
      Lwt_main.run
        (D.repost ~ctx ~db:e.db ~manager:e.manager ~http ~upload_http ~lease
           (E.runtime e r.fire.id) payload);
      integer "one complete despite timeout" 1 !completes;
      integer "one file set despite timeout" 1 !uploads;
      let saved = E.runtime e r.fire.id in
      integer "repost slot never refunded"
        (if second then 1 else 0)
        saved.fire.reposts;
      integer "all debits retained"
        (if second then 2 else 1)
        (Store.dispatches_since e.db ~order_id:e.order.id ~since:0.))

let test_exact_ids fixture wrong () =
  fixture (fun (e : E.env) lease (r : Store.fire_runtime) payload ->
      let r = seed e lease r false in
      let a =
        { (Store.empty_attempt 1) with uploaded_file_ids = [ "FSECOND" ] }
      in
      persist e lease r { r with attempts = r.attempts @ [ a ] };
      check "second reserved" true
        (ok
           (Store.reserve_dispatch e.db ~fire_id:r.fire.id ~lease ~repost:true
              ~now:(Store.now ())));
      let r = E.runtime e r.fire.id in
      persist e lease r
        { r with fire = { r.fire with state = T.Posting_unknown } };
      history e
        [
          message ts0 [ "FORIGINAL" ];
          message ts1 [ (if wrong then "FOTHER" else "FSECOND") ];
        ];
      let ctx =
        {
          (E.ctx e) with
          Ctx.wait =
            (fun ~seconds ->
              S.at (Store.now () +. seconds);
              Lwt.return_unit);
        }
      in
      Lwt_main.run
        (D.repost ~ctx ~db:e.db ~manager:e.manager ~http:e.mock.http ~lease
           (E.runtime e r.fire.id) payload);
      let saved = E.runtime e r.fire.id in
      let a = Option.get (Store.attempt saved 1) in
      check "persisted exact identity" true (a.uploaded_file_ids = [ "FSECOND" ]);
      check "only exact set adopted" true
        (a.relay_ts = if wrong then None else Some ts1);
      integer "unchanged repost debit" 1 saved.fire.reposts;
      E.no_upload e)

let test_original_history fixture () =
  List.iter
    (fun no_ids ->
      fixture (fun (e : E.env) lease (r : Store.fire_runtime) payload ->
          let r =
            seed ~ids:(if no_ids then [] else [ "FORIGINAL" ]) e lease r true
          in
          history e [ message ts0 [ "FORIGINAL" ] ];
          let ctx =
            {
              (E.ctx e) with
              Ctx.wait =
                (fun ~seconds ->
                  S.at (Store.now () +. seconds);
                  Lwt.return_unit);
            }
          in
          Lwt_main.run
            (D.repost ~ctx ~db:e.db ~manager:e.manager ~http:e.mock.http ~lease
               r payload);
          let saved = E.runtime e r.fire.id in
          let first = Option.get (Store.attempt saved 0) in
          check "original adopted as original" true
            (first.relay_ts = Some ts0
            && first.uploaded_file_ids = [ "FORIGINAL" ]);
          integer "no repost" 0 saved.fire.reposts;
          E.no_upload e))
    [ false; true ]
