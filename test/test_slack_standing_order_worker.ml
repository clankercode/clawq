open Lwt.Syntax
open Test_slack_standing_order_exec_support
module E = Slack_standing_order_exec
module Ctx = Slack_standing_order_ctx

let check = Alcotest.(check bool)

let rec flush n =
  if n = 0 then Lwt.return_unit
  else
    let* () = Lwt.pause () in
    flush (n - 1)

type timer = { due : float; promise : unit Lwt.t; wake : unit Lwt.u }
type clock = { mutable now : float; mutable timers : timer list }

let clock () =
  let c = { now = Store.now (); timers = [] } in
  (Store.clock := fun () -> c.now);
  c

let wait c ~seconds =
  let promise, wake = Lwt.task () in
  c.timers <- { due = c.now +. seconds; promise; wake } :: c.timers;
  promise

let pending c = List.filter (fun t -> Lwt.is_sleeping t.promise) c.timers

let advance c seconds =
  c.now <- c.now +. seconds;
  List.iter
    (fun t ->
      if Lwt.is_sleeping t.promise && t.due <= c.now then Lwt.wakeup t.wake ())
    c.timers;
  c.timers <- pending c;
  flush 20

let context e c = { (ctx e) with Ctx.wait = wait c }

let cleanup c =
  let stranded = pending c in
  List.iter (fun t -> Lwt.cancel t.promise) stranded;
  Alcotest.(check int)
    "no stranded timers before cleanup" 0 (List.length stranded)

let test_due () =
  with_env (fun e ->
      (* A reserved fire from a previous process may be tracked, never resent. *)
      let fire =
        S.fire e.db e.order ~room:"R123" ~state:T.Posted
          ~dispatched_at:(Store.now ()) ~relay_ts:"1700000041.000001" ()
      in
      let c = clock () in
      let calls = ref 0 in
      let http ~verb:_ ~token:_ ~method_:_ ~params:_ =
        incr calls;
        Lwt.return (503, "{}")
      in
      Lwt_main.run
        (Lwt.finalize
           (fun () ->
             E.start ~ctx:(context e c) ~db:e.db ~manager:e.manager ~http ();
             let* () = advance c 4. in
             Alcotest.(check int) "no wake at four" 0 !calls;
             let* () = advance c 1. in
             Alcotest.(check int) "one wake at five" 1 !calls;
             check "read-only fire retained" true
               ((runtime e fire.id).fire.dispatched_at <> None);
             Lwt.return_unit)
           (fun () ->
             let* () = E.stop () in
             cleanup c;
             Lwt.return_unit)))

let test_tick_failure ?(failure = Failure "injected Store.list failure") () =
  with_env (fun e ->
      ignore
        (S.fire e.db e.order ~room:"R123" ~state:T.Posted
           ~dispatched_at:(Store.now ()) ~relay_ts:"1700000041.000001" ()
          : T.fire);
      let c = clock () in
      let calls = ref 0 and fail_next = ref false in
      let list_orders () =
        if !fail_next then (
          fail_next := false;
          raise failure);
        Store.list e.db ~include_cancelled:true ()
      in
      let http ~verb:_ ~token:_ ~method_:_ ~params:_ =
        incr calls;
        Lwt.return (503, "{}")
      in
      let (), logs =
        Lwt_main.run
          (S.with_log_capture (fun () ->
               Lwt.finalize
                 (fun () ->
                   E.start ~ctx:(context e c) ~db:e.db ~manager:e.manager ~http
                     ~list_orders ();
                   fail_next := true;
                   let* () = advance c 5. in
                   Alcotest.(check int) "failed tick does no work" 0 !calls;
                   let* () = advance c 4. in
                   Alcotest.(check int) "retry waits five seconds" 0 !calls;
                   let* () = advance c 1. in
                   Alcotest.(check int) "later tick processes due fire" 1 !calls;
                   Lwt.return_unit)
                 (fun () ->
                   let* () = E.stop () in
                   cleanup c;
                   Lwt.return_unit)))
      in
      check "tick warning includes exception" true
        (List.mem
           ( Logs.Warning,
             "standing-order worker tick failed: " ^ Printexc.to_string failure
           )
           logs);
      check "worker start is logged" true
        (List.mem (Logs.Info, "standing-order worker started") logs);
      Alcotest.(check int)
        "stop adds no cancellation warning" 1
        (List.length
           (List.filter (fun (level, _) -> level = Logs.Warning) logs)))

let test_wait_failure ?(stop_in_backoff = false) ?(reject = false) () =
  with_env (fun e ->
      ignore
        (S.fire e.db e.order ~room:"R123" ~state:T.Posted
           ~dispatched_at:(Store.now ()) ~relay_ts:"1700000041.000001" ()
          : T.fire);
      let c = clock () in
      let calls = ref 0 and failures = ref 0 and remaining = ref 4 in
      let failure = Failure "worker wait unavailable" in
      let ctx =
        {
          (context e c) with
          Ctx.wait =
            (fun ~seconds ->
              if !remaining > 0 then (
                decr remaining;
                incr failures;
                if reject then
                  let* () = Lwt.pause () in
                  Lwt.fail failure
                else raise failure)
              else wait c ~seconds);
        }
      in
      let http ~verb:_ ~token:_ ~method_:_ ~params:_ =
        incr calls;
        Lwt.return (503, "{}")
      in
      let (), logs =
        Lwt_main.run
          (S.with_log_capture (fun () ->
               Lwt.finalize
                 (fun () ->
                   E.start ~ctx ~db:e.db ~manager:e.manager ~http
                     ~fallback_sleep:(fun seconds -> wait c ~seconds)
                     ();
                   Alcotest.(check int)
                     "failure yields before backoff setup" 1 !failures;
                   let* () = flush 20 in
                   Alcotest.(check int)
                     "failed head wait yields, then tries one backoff" 2
                     !failures;
                   let* () = flush 20 in
                   Alcotest.(check int)
                     "no retries without time advancing" 2 !failures;
                   Alcotest.(check int) "no work before backoff" 0 !calls;
                   if stop_in_backoff then Lwt.return_unit
                   else (
                     remaining := 0;
                     let* () = advance c 4. in
                     Alcotest.(check int) "fallback waits five seconds" 0 !calls;
                     let* () = advance c 1. in
                     Alcotest.(check int)
                       "later work without a second wait" 1 !calls;
                     Lwt.return_unit))
                 (fun () ->
                   let stopped = E.stop () in
                   let* () = flush 20 in
                   check "stop drains without advancing time" false
                     (Lwt.is_sleeping stopped);
                   let* () = stopped in
                   check "worker uninstalled" true (!E.worker = None);
                   cleanup c;
                   Lwt.return_unit)))
      in
      check "wait warning includes exception" true
        (List.mem
           ( Logs.Warning,
             "standing-order worker tick failed: " ^ Printexc.to_string failure
           )
           logs);
      check "backoff warning includes exception" true
        (List.mem
           ( Logs.Warning,
             "standing-order worker backoff failed: "
             ^ Printexc.to_string failure )
           logs);
      Alcotest.(check int)
        "stop adds no cancellation warning" 2
        (List.length
           (List.filter (fun (level, _) -> level = Logs.Warning) logs)))

let test_job_failure ?(failure = Failure "channel clock unavailable") () =
  with_env (fun e ->
      ignore
        (S.fire e.db e.order ~room:"R123" ~state:T.Posted
           ~dispatched_at:(Store.now ()) ~relay_ts:"1700000041.000001" ()
          : T.fire);
      let c = clock () in
      let calls = ref 0 and fail_clock = ref false and escaped = ref [] in
      let ctx =
        {
          (context e c) with
          Ctx.now =
            (fun () -> if !fail_clock then raise failure else Store.now ());
        }
      in
      let http ~verb:_ ~token:_ ~method_:_ ~params:_ =
        incr calls;
        Lwt.return (503, "{}")
      in
      let previous = !Lwt.async_exception_hook in
      let (), logs =
        Fun.protect
          ~finally:(fun () -> Lwt.async_exception_hook := previous)
          (fun () ->
            (Lwt.async_exception_hook := fun exn -> escaped := exn :: !escaped);
            Lwt_main.run
              (S.with_log_capture (fun () ->
                   Lwt.finalize
                     (fun () ->
                       E.start ~ctx ~db:e.db ~manager:e.manager ~http ();
                       (* The per-order guard also sees this failure, but the
                          channel-wide due read is outside that guard. *)
                       fail_clock := true;
                       let* () = advance c 5. in
                       Alcotest.(check int) "failed job does no work" 0 !calls;
                       fail_clock := false;
                       let* () = advance c 5. in
                       Alcotest.(check int)
                         "later pass retries the same channel" 1 !calls;
                       Lwt.return_unit)
                     (fun () ->
                       let* () = E.stop () in
                       cleanup c;
                       Lwt.return_unit))))
      in
      Alcotest.(check (list string))
        "job exception never reaches Lwt.async" []
        (List.map Printexc.to_string !escaped);
      check "channel warning includes exception" true
        (List.mem
           ( Logs.Warning,
             "standing-order worker channel C123 failed: "
             ^ Printexc.to_string failure )
           logs))

let test_renew () =
  with_env (fun e ->
      let fire = S.fire e.db e.order ~room:"R123" () in
      let c = clock () in
      let done_, wake = Lwt.task () and accepting = ref (fun () -> false) in
      Slack_standing_order_sql.exec e.db "CREATE TABLE renewal_count(n INTEGER)"
        [];
      Slack_standing_order_sql.exec e.db "INSERT INTO renewal_count VALUES(0)"
        [];
      Slack_standing_order_sql.exec e.db
        "CREATE TRIGGER count_renew AFTER UPDATE OF lease_until ON \
         slack_standing_order_fires WHEN NEW.lease_until > OLD.lease_until \
         BEGIN UPDATE renewal_count SET n=n+1; END"
        [];
      let renewals () =
        Slack_standing_order_sql.int_query e.db "SELECT n FROM renewal_count" []
      in
      Lwt_main.run
        (let work =
           E.with_lease ~ctx:(context e c) ~db:e.db ~fire_id:fire.id
             (fun ctx _ ->
               accepting := ctx.Ctx.accepting;
               done_)
         in
         let initial = Option.get (runtime e fire.id).claim in
         let* () = advance c 29. in
         check "no renewal at 29" true
           ((Option.get (runtime e fire.id).claim).until = initial.until);
         Alcotest.(check int) "zero renewals at 29" 0 (renewals ());
         let* () = advance c 1. in
         Alcotest.(check int) "one renewal at 30" 1 (renewals ());
         Alcotest.(check (float 0.))
           "renewal until now plus 120" (c.now +. 120.)
           (Option.get (runtime e fire.id).claim).until;
         let* () = advance c 100. in
         check "accepting after original lease expiry" true (!accepting ());
         Lwt.wakeup wake ();
         let* () = work in
         check "lease released" true ((runtime e fire.id).claim = None);
         cleanup c;
         Lwt.return_unit))

let test_loss () =
  with_env (fun e ->
      let fire = S.fire e.db e.order ~room:"R123" () in
      let c = clock () in
      let done_, wake = Lwt.task () in
      let accepted = ref (fun () -> true) in
      Lwt_main.run
        (let work =
           E.with_lease ~ctx:(context e c) ~db:e.db ~fire_id:fire.id
             (fun ctx _ ->
               accepted := ctx.Ctx.accepting;
               done_)
         in
         let* () = advance c 121. in
         check "expired lease refuses dispatch" false (!accepted ());
         Lwt.wakeup wake ();
         let* () = work in
         cleanup c;
         Lwt.return_unit))

let test_stop () =
  with_env (fun e ->
      let c = clock () in
      Lwt_main.run
        (E.start ~ctx:(context e c) ~db:e.db ~manager:e.manager ();
         E.start ~ctx:(context e c) ~db:e.db ~manager:e.manager ();
         Alcotest.(check int) "one worker timer" 1 (List.length (pending c));
         E.wake ();
         let* () = flush 20 in
         Alcotest.(check int)
           "wake cancels losing timer" 1
           (List.length (pending c));
         let* () = E.stop () in
         check "worker uninstalled" true (!E.worker = None);
         cleanup c;
         E.start ~ctx:(context e c) ~db:e.db ~manager:e.manager ();
         Alcotest.(check int)
           "restart installs one worker" 1
           (List.length (pending c));
         let* () = E.stop () in
         cleanup c;
         Lwt.return_unit))

let test_hold () =
  with_env (fun e ->
      seen e;
      ready e ();
      let fire = S.fire e.db e.order ~room:"RPERSISTED" () in
      let c = clock () in
      Lwt_main.run
        (Lwt.finalize
           (fun () ->
             E.start ~ctx:(context e c) ~db:e.db ~manager:e.manager
               ~http:e.mock.http ();
             let* () = advance c 5. in
             Alcotest.(check int)
               "no old-root scan or claim" 0
               (List.length !(e.mock.calls));
             let r = runtime e fire.id in
             check "P4-held retry" true
               (r.claim = None
               && r.fire.reason = Some "p4_recovery_held"
               && r.next_attempt_at = None);
             Lwt.return_unit)
           (fun () ->
             let* () = E.stop () in
             cleanup c;
             Lwt.return_unit)))

let test_pacing () =
  with_env (fun e ->
      let c = clock () in
      let ctx = E.context ~wait:(wait c) ~accepting:(fun () -> true) () in
      Lwt_main.run
        (let* () = ctx.limit ~channel_id:"C123" in
         let next = ctx.limit ~channel_id:"C123" in
         let other = ctx.limit ~channel_id:"C456" in
         check "other channel proceeds" false (Lwt.is_sleeping other);
         let* () = advance c 0.999 in
         check "same-channel post still waits" true (Lwt.is_sleeping next);
         let* () = advance c 0.001 in
         let* () = next in
         cleanup c;
         Lwt.return_unit))

let suite =
  [
    S.case "worker wakes due work at five fake seconds" test_due;
    S.case "worker survives a failing tick" (fun () -> test_tick_failure ());
    S.case "worker survives unexpected cancellation" (fun () ->
        test_tick_failure ~failure:Lwt.Canceled ());
    S.case "wait setup failures back off before later work" (fun () ->
        test_wait_failure ());
    S.case "wait rejections back off before later work" (fun () ->
        test_wait_failure ~reject:true ());
    S.case "stop drains during fallback backoff" (fun () ->
        test_wait_failure ~stop_in_backoff:true ());
    S.case "a raising channel job is logged and retried" (fun () ->
        test_job_failure ());
    S.case "unexpected channel cancellation is logged and retried" (fun () ->
        test_job_failure ~failure:Lwt.Canceled ());
    S.case "lease renews at thirty fake seconds" test_renew;
    S.case "expired or lost lease stops dispatch" test_loss;
    S.case "stop cancels idle worker timer" test_stop;
    S.case "condition wake cancels losing timer" test_stop;
    S.case "stop and restart do not duplicate hooks" test_stop;
    S.case "startup holds pre-dispatch fires and never scans old roots"
      test_hold;
    S.case "post pacing at least one second" test_pacing;
  ]
