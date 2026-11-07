module S = Test_slack_standing_order_support
module RS = Test_slack_standing_order_recovery_store
module R = Slack_standing_order_recovery_store
module Store = Slack_standing_order_store
module Sql = Slack_standing_order_sql
module T = Slack_standing_order_types

let check = Alcotest.(check bool)

let prepared fn =
  let db = RS.fresh () in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      let o = RS.new_order db "so_notice" in
      let gap, owner, c = RS.scan db o in
      check "candidate" true
        (R.save_page db ~order_id:o.id ~owner ~coverage:c
           ~candidates:[ ("1700000001.1", "R123", "1700000001") ]
           ~now:S.now);
      RS.finish db o gap owner c;
      let card =
        Store.insert_card db ~kind:T.Pause_button ~order:o ~created_by:"U123"
          ~now:S.now ()
      in
      check "batch" true
        (R.create_batch db ~order:o ~owner ~batch_id:"batch" ~card_id:card.id
           ~roots:[ ("1700000001.1", "1700000001") ]
           ~now:S.now);
      check "attempt" true
        (R.reserve_notice db ~batch_id:"batch" ~owner ~now:S.now);
      fn db o gap owner c)

let test_new_coverage () =
  prepared (fun db o gap owner c ->
      check "bind" true
        (R.bind_notice db ~batch_id:"batch" ~owner ~ts:"1700000001.2"
           ~release_at:(S.now +. 600.) ~now:S.now);
      check "renew" true (R.renew db ~order_id:o.id ~owner ~now:(S.now +. 100.));
      check "new coverage" true
        (R.begin_scan db ~order:o ~owner ~mode:R.Gap_recovery ~gap
           ~oldest:c.R.oldest ~latest:"1700000200.0" ~now:(S.now +. 100.));
      (* Keep lease alive without changing the actual pause coverage. *)
      List.iter
        (fun delta ->
          check "renew" true
            (R.renew db ~order_id:o.id ~owner ~now:(S.now +. delta)))
        [ 200.; 300.; 400.; 500.; 600. ];
      check "incomplete pause sweep forbids release" false
        (R.release_batch db ~batch_id:"batch" ~owner ~now:(S.now +. 600.)))

let test_advertised () =
  prepared (fun db _ _ owner _ ->
      Sql.exec db
        "UPDATE slack_standing_order_recovery_batches SET release_at=? WHERE \
         id='batch'"
        [ Sqlite3.Data.FLOAT (S.now +. 900.) ];
      check "bind may not shorten advertised hold" false
        (R.bind_notice db ~batch_id:"batch" ~owner ~ts:"1700000001.2"
           ~release_at:(S.now +. 600.) ~now:S.now))

let suite =
  [
    S.case "new coverage requires complete pause sweep" test_new_coverage;
    S.case "binding preserves advertised opportunity" test_advertised;
  ]

open Test_slack_standing_order_catchup_support
open Lwt.Syntax

let test_notice e =
  let f = fire e 1 and f2 = fire e 2 in
  let b = prepare e [ f; f2; f ] in
  let again = prepare e [ f ] in
  check "duplicates use durable batch" true (again.id = b.id);
  let before = Option.get (Store.find e.db e.order.id) in
  let base = !(e.answer) in
  (e.answer :=
     fun method_ p ->
       if method_ = "chat.postMessage" && get "blocks" p <> `Null then (
         check "batch attempted before actual HTTP" true
           (status e b = "notice_attempted");
         let card = Store.find_card e.other b.id |> Option.get in
         check "unbound card persisted before post" true (card.message_ts = None);
         eq "batch cardinality" 2 (R.batch_count e.other ~batch_id:b.id);
         check "top level" true (get "thread_ts" p = `Null);
         let text = field "text" p in
         check "fixed count copy" true
           (Test_helpers.string_contains text
              "clawq was offline; 2 huddles will be relayed at ");
         check "pause instruction" true
           (Test_helpers.string_contains text "Pause SO-1 to stop them.");
         check "no target mention or content" true
           ((not (Test_helpers.string_contains (Yojson.Safe.to_string p) "<@"))
           && not (Test_helpers.string_contains text "Update the project")));
       base method_ p);
  let* () = ensure e b in
  eq "one notice" 1 (List.length (notices e));
  check "confirmed" true (status e b = "confirmed");
  let card = Store.find_card e.other b.id |> Option.get in
  let after = batch e b in
  check "exact notice binding" true
    (card.message_ts = after.notice_ts
    && card.order_epoch = e.order.epoch
    && card.team_id = e.order.team_id
    && card.app_id = e.order.api_app_id
    && card.channel_id = e.order.channel_id);
  check "activation not changed" true
    (Option.get (Store.find e.db e.order.id) = before);
  let* () = ensure e b in
  eq "confirmed duplicate does not post" 1 (List.length (notices e));
  let f3 = fire e 3 in
  let b2 = prepare e [ f; f3 ] in
  check "new fire requires later batch" true (b2.id <> b.id);
  eq "advertised membership frozen" 2 (R.batch_count e.db ~batch_id:b.id);
  eq "new notice only new candidate" 1 (R.batch_count e.db ~batch_id:b2.id);
  let* () = ensure e b2 in
  eq "two immutable batches" 2 (List.length (notices e));
  check "not released early" false (allowed e f);
  let deadline = Option.get after.release_at in
  advance e (deadline -. !(e.clock) -. 1.);
  check "599 seconds held" false (release e b);
  check "599 admission held" false (allowed e f);
  advance e 1.;
  check "600 seconds release" true (release e b);
  check "600 admission positive control" true (allowed e f);
  Lwt.return_unit

let test_delayed e =
  let f = fire e 1 in
  let b = prepare e [ f ] in
  let initial = !(e.clock) in
  let base = !(e.answer) in
  (e.answer :=
     fun m p ->
       let* response = base m p in
       if m = "chat.postMessage" && get "blocks" p <> `Null then advance e 200.;
       Lwt.return response);
  let* () = ensure e b in
  Alcotest.(check (float 0.001))
    "600 from response not dispatch" (initial +. 800.)
    (Option.get (batch e b).release_at);
  advance e 599.;
  check "delayed response 599 held" false (release e b);
  advance e 1.;
  check "delayed response 600 released" true (release e b);
  Lwt.return_unit

let test_restart e =
  let f = fire e 1 in
  let b = prepare e [ f ] in
  let* () = ensure e b in
  let deadline = (batch e b).release_at in
  advance e 200.;
  let reopened = Sqlite3.db_open e.path in
  let manager =
    Session.create ~config:(Session.get_config e.manager) ~db:reopened ()
  in
  let* () =
    Lwt.finalize
      (fun () -> ensure ~db:reopened ~manager e b)
      (fun () ->
        ignore (Sqlite3.db_close reopened);
        Lwt.return_unit)
  in
  check "restart preserves deadline" true ((batch e b).release_at = deadline);
  eq "restart no blind notice" 1 (List.length (notices e));
  advance e 400.;
  check "original hold can release" true (release e b);
  Lwt.return_unit

let suite =
  suite
  @ [
      case "persisted immutable top-level notice and 599/600" test_notice;
      case "delayed response grants full 600" test_delayed;
      case "DB reopen at 200 preserves deadline" test_restart;
    ]

let suite = suite @ Test_slack_standing_order_catchup_faults.suite
let suite = suite @ Test_slack_standing_order_catchup_fences.suite
let suite = suite @ Test_slack_standing_order_catchup_edges.suite
let suite = suite @ Test_slack_standing_order_catchup_dispatch.suite
