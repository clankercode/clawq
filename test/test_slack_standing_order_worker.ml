open Lwt.Syntax
open Test_slack_standing_order_exec_support
module E = Slack_standing_order_exec

let check = Alcotest.(check bool)

include Test_slack_standing_order_worker_support

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

let observe_ready e ?(user = "USLACKBOT") ?(bot = `Null) () =
  let message =
    set "type" (str "message")
      (set "user" (str user)
         (set "bot_id" bot
            (set "channel" (str "C123")
               (set "thread_ts" (str root_ts) (set "ts" (str ready_ts) F.ready)))))
  in
  let envelope = Test_slack_event_fixtures.envelope message in
  Slack_huddle_observer.observe_db ~db:e.db
    ~watch:(Slack_huddle_watch.of_db e.db)
    ~now:(Store.now ()) envelope

let test_ready_wake () =
  with_env (fun e ->
      let c = clock () in
      (* Keep provenance pending so this test counts fetch attempts, not delivery. *)
      e.replies := F.page [ !(e.root) ];
      Lwt_main.run
        (Lwt.finalize
           (fun () ->
             E.start ~ctx:(context e c) ~db:e.db ~manager:e.manager
               ~http:e.mock.http ();
             let fire =
               S.fire e.db e.order ~room:"R123"
                 ~date_start:(string_of_int start) ~state:T.Fetching ()
             in
             Slack_standing_order_sql.exec e.db
               "UPDATE slack_standing_order_fires SET \
                root_ts=?,reason='notes_not_ready',next_attempt_at=? WHERE \
                id=?"
               Sqlite3.Data.
                 [ TEXT root_ts; FLOAT (c.now +. 300.); TEXT fire.id ];
             observe_ready e ~user:"UHUMAN" ();
             observe_ready e ~bot:(str "BOTHER") ();
             let* () = flush 30 in
             Alcotest.(check int)
               "spoofs cannot wake fetch" 0
               (count e "conversations.replies");
             observe_ready e ();
             observe_ready e ();
             let* () = flush 100 in
             Alcotest.(check int)
               "one immediate worker fetch" 1
               (count e "conversations.replies");
             let r = runtime e fire.id in
             Alcotest.(check int) "epoch unchanged" fire.epoch r.fire.epoch;
             check "worker released lease" true (r.claim = None);
             check "still notes backoff" true (r.next_attempt_at > Some c.now);
             observe_ready e ();
             let* () = advance c 5. in
             Alcotest.(check int)
               "duplicate hydration consumed once" 1
               (count e "conversations.replies");
             no_upload e;
             Lwt.return_unit)
           (fun () ->
             let* () = E.stop () in
             cleanup c;
             Lwt.return_unit)))

let test_live_budget () =
  with_env (fun e ->
      let c = clock () in
      e.root :=
        set "room"
          (set "has_ended" (`Bool false)
             (set "date_end" `Null (get "room" !(e.root))))
          !(e.root);
      Lwt_main.run
        (Lwt.finalize
           (fun () ->
             E.start ~ctx:(context e c) ~db:e.db ~manager:e.manager
               ~http:e.mock.http ();
             seen e;
             E.wake ();
             let* () = flush 40 in
             let pickup =
               Test_slack_event_fixtures.reaction ~user:"UCLAUDE"
                 ~item:
                   (obj
                      [
                        ("type", str "message");
                        ("channel", str "C123");
                        ("ts", str "1700000999.000100");
                      ])
                 ()
             in
             Slack_huddle_observer.observe_db ~db:e.db
               ~watch:(Slack_huddle_watch.of_db e.db)
               ~now:c.now
               (Test_slack_event_fixtures.envelope pickup);
             seen e;
             E.wake ();

             let rec ticks n =
               if n = 0 then Lwt.return_unit
               else
                 let* () = advance c 5. in
                 ticks (n - 1)
             in
             let* () = ticks 60 in
             Alcotest.(check int)
               "no artificial live fire" 0
               (List.length (fires e));
             List.iter
               (fun method_ -> Alcotest.(check int) method_ 2 (count e method_))
               [
                 "auth.test";
                 "bots.info";
                 "conversations.info";
                 "conversations.members";
                 "users.list";
                 "conversations.history";
               ];
             Alcotest.(check int)
               "actual HTTP ceiling" 12
               (List.length !(e.mock.calls));
             no_upload e;
             Printf.eprintf
               "B853 healthy through 300s: six methods each=2; total=12; \
                content=0; upload=0\n\
                %!";
             Lwt.return_unit)
           (fun () ->
             let* () = E.stop () in
             cleanup c;
             Lwt.return_unit)))

let histogram e =
  S.methods_called e.mock
  |> List.sort_uniq String.compare
  |> List.map (fun name -> (name, count e name))

let print_histogram label e =
  Printf.eprintf "%s actual HTTP: %s\n%!" label
    (histogram e
    |> List.map (fun (name, n) -> name ^ "=" ^ string_of_int n)
    |> String.concat ", ")

let test_resolution_failures () =
  with_env (fun e ->
      let c = clock () in
      S.override e.mock "conversations.history" (fun _ -> (503, S.ok []));
      Lwt_main.run
        (Lwt.finalize
           (fun () ->
             E.start ~ctx:(context e c) ~db:e.db ~manager:e.manager
               ~http:e.mock.http ();
             seen e;
             E.wake ();
             let* () = flush 40 in
             let rec ticks n =
               if n = 0 then Lwt.return_unit
               else
                 let* () = advance c 5. in
                 ticks (n - 1)
             in
             let* () = ticks 120 in
             List.iter
               (fun method_ -> Alcotest.(check int) method_ 3 (count e method_))
               [
                 "auth.test";
                 "bots.info";
                 "conversations.info";
                 "conversations.members";
                 "users.list";
                 "conversations.history";
               ];
             Alcotest.(check int)
               "failed roots do not spin" 18
               (List.length !(e.mock.calls));
             Alcotest.(check int) "no invented fire" 0 (List.length (fires e));
             print_histogram "B853 failure through 600s" e;
             Lwt.return_unit)
           (fun () ->
             let* () = E.stop () in
             cleanup c;
             Lwt.return_unit)))

let test_local_deny () =
  with_env (fun e ->
      let c = clock () in
      e.root :=
        set "room"
          (set "has_ended" (`Bool false)
             (set "date_end" `Null (get "room" !(e.root))))
          !(e.root);
      Lwt_main.run
        (Lwt.finalize
           (fun () ->
             E.start ~ctx:(context e c) ~db:e.db ~manager:e.manager
               ~http:e.mock.http ();
             seen e;
             E.wake ();
             let* () = flush 40 in
             Alcotest.(check int)
               "initial resolution" 6
               (List.length !(e.mock.calls));
             Session.update_config e.manager
               (S.profile (S.config ()) ~denied:[ "slack_huddle_content" ] ());
             let* () = advance c 5. in
             Alcotest.(check int)
               "local denial without fallback or HTTP" 6
               (List.length !(e.mock.calls));
             check "suspended immediately" true
               ((Option.get (Store.find e.db e.order.id)).state = T.Suspended);
             Lwt.return_unit)
           (fun () ->
             let* () = E.stop () in
             cleanup c;
             Lwt.return_unit)))

let test_feature_reenable () =
  with_env (fun e ->
      let c = clock () in
      e.root :=
        set "room"
          (set "has_ended" (`Bool false)
             (set "date_end" `Null (get "room" !(e.root))))
          !(e.root);
      (* Leave notes pending after the verified end, avoiding relay writes. *)
      e.replies := F.page [ !(e.root) ];
      Lwt_main.run
        (Lwt.finalize
           (fun () ->
             E.start ~ctx:(context e c) ~db:e.db ~manager:e.manager
               ~http:e.mock.http ();
             seen e;
             E.wake ();
             let* () = flush 40 in
             Alcotest.(check int)
               "initial live reads" 6
               (List.length !(e.mock.calls));
             Session.update_config e.manager (S.config ~enabled:false ());
             let* () = advance c 5. in
             e.root :=
               set "room"
                 (set "has_ended" (`Bool true)
                    (set "date_end" (`Int 1700000030) (get "room" !(e.root))))
                 !(e.root);
             seen e;
             observe_ready e ();
             let* () = advance c 600. in
             Alcotest.(check int)
               "disabled end and ready cause zero HTTP" 6
               (List.length !(e.mock.calls));
             Alcotest.(check int)
               "disabled creates no fire" 0
               (List.length (fires e));
             Session.update_config e.manager (S.config ());
             let* () = advance c 5. in
             Alcotest.(check int)
               "reenabled worker creates eligible fire" 1
               (List.length (fires e));
             check "reenable performs fresh Grant and exact-root reads" true
               (count e "auth.test" >= 2 && count e "conversations.history" >= 2);
             let order = Option.get (Store.find e.db e.order.id) in
             Alcotest.check S.state_t "temporary disable keeps order active"
               T.Active order.state;
             Alcotest.(check int)
               "temporary disable keeps epoch" e.order.epoch order.epoch;
             no_upload e;
             Lwt.return_unit)
           (fun () ->
             let* () = E.stop () in
             cleanup c;
             Lwt.return_unit)))

let test_ready_content ?(cooldown = 0.) () =
  with_env (fun e ->
      let c = clock () in
      let config = Session.get_config e.manager in
      let rule : Runtime_config.egress_rule =
        {
          host = "files.slack.com";
          path = Some "/upload/*";
          method_ = Some "POST";
          action = Allow;
          log_policy = No_log;
        }
      in
      Session.update_config e.manager
        {
          config with
          egress =
            {
              config.egress with
              default_allowlist = rule :: config.egress.default_allowlist;
            };
        };
      S.override e.mock "conversations.history" (fun p ->
          ( 200,
            S.ok
              [
                ( "messages",
                  if get "limit" p = `Int 1 then `List [ !(e.root) ]
                  else `List [] );
                S.no_more;
              ] ));
      S.override e.mock "files.getUploadURLExternal" (fun _ ->
          ( 200,
            S.ok
              [
                ("file_id", str "FUPLOAD");
                ("upload_url", str "https://files.slack.com/upload/fixture");
              ] ));
      let shares =
        obj
          [
            ( "private",
              obj
                [ ("C123", `List [ obj [ ("ts", str "1700000100.000001") ] ]) ]
            );
          ]
      in
      S.override e.mock "files.completeUploadExternal" (fun _ ->
          (200, S.ok [ ("files", `List [ obj [ ("shares", shares) ] ]) ]));
      let limiter =
        Slack_read_limiter.create_with ~now:(fun () -> c.now) ~wait:(wait c)
      in
      if cooldown > 0. then
        Slack_read_limiter.note_retry_after limiter ~key:"T123:A123:auth.test"
          cooldown;
      let context = { (context e c) with Ctx.limiter = Some limiter } in
      let uploads = ref 0 in
      Lwt_main.run
        (Lwt.finalize
           (fun () ->
             E.start ~ctx:context ~db:e.db ~manager:e.manager ~http:e.mock.http
               ~upload_http:(fun _ ->
                 incr uploads;
                 Lwt.return
                   Slack_standing_order_upload.
                     { status = 200; headers = []; body = "" })
               ();
             cover_order e.db e.order;
             let fire =
               S.fire e.db e.order ~room:"R123"
                 ~date_start:(string_of_int start) ~state:T.Fetching ()
             in
             Slack_standing_order_sql.exec e.db
               "UPDATE slack_standing_order_fires SET \
                root_ts=?,reason='notes_not_ready',next_attempt_at=? WHERE \
                id=?"
               Sqlite3.Data.
                 [ TEXT root_ts; FLOAT (c.now +. 300.); TEXT fire.id ];
             Slack_standing_order_recovery_store.record_root e.db ~order:e.order
               ~root_ts ~room_id:"R123" ~date_start:(string_of_int start)
               ~fire_id:(Some fire.id);
             observe_ready e ();
             observe_ready e ();
             let rec until n =
               if count e "files.completeUploadExternal" > 0 then
                 Lwt.return_unit
               else if n = 0 then
                 Alcotest.fail "no share within sixty fake seconds"
               else
                 let* () = advance c 1. in
                 until (n - 1)
             in
             let* () =
               if cooldown = 0. then Lwt.return_unit
               else
                 let* () = advance c (cooldown -. 1.) in
                 Alcotest.(check int)
                   "ready cannot bypass shared Retry-After" 0
                   (List.length !(e.mock.calls));
                 Lwt.return_unit
             in
             let* () = until 59 in
             let* () = flush 100 in
             Alcotest.(check int)
               "one verified content fetch" 1
               (count e "canvases.getContent");
             Alcotest.(check int) "one raw upload" 1 !uploads;
             Alcotest.(check int)
               "one share" 1
               (count e "files.completeUploadExternal");
             check "same epoch" true
               ((runtime e fire.id).fire.epoch = e.order.epoch);
             Alcotest.(check int)
               "only real dispatch consumes cap" 1
               (Store.dispatches_since e.db ~order_id:e.order.id ~since:0.);
             print_histogram "B852 verified notes to first share" e;
             Printf.eprintf
               "B852 first share after %.0f fake seconds; raw_upload=%d\n%!"
               (c.now -. (ended +. 10.))
               !uploads;
             Lwt.return_unit)
           (fun () ->
             let* () = E.stop () in
             cleanup c;
             Lwt.return_unit)))

let suite =
  [
    S.case "feature reenable resumes real worker after live end and ready"
      test_feature_reenable;
    S.case "ready hint wakes real worker across SQL due and retry guard"
      test_ready_wake;
    S.case "unchanged live root has twelve actual reads through 300 seconds"
      test_live_budget;
    S.case "notes wake verifies content and shares under sixty seconds"
      (fun () -> test_ready_content ());
    S.case "notes wake preserves shared transport Retry-After" (fun () ->
        test_ready_content ~cooldown:17. ());
    S.case "root failures back off through six hundred seconds"
      test_resolution_failures;
    S.case "local config denial suspends between fallback probes"
      test_local_deny;
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

let suite = suite @ Test_slack_standing_order_worker_suppression.suite
