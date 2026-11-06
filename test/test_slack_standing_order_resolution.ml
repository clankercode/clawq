open Lwt.Syntax
open Test_slack_standing_order_exec_support
module E = Slack_standing_order_exec
module Q = Slack_standing_order_schedule
module Ctx = Slack_standing_order_ctx
module W = Test_slack_standing_order_worker

let check = Alcotest.(check bool)

(* Stall renewal to model a paused actor while other processes advance time. *)
let context e =
  { (ctx e) with Ctx.wait = (fun ~seconds:_ -> fst (Lwt.task ())) }

let live e =
  e.root :=
    set "room"
      (set "has_ended" (`Bool false)
         (set "date_end" `Null (get "room" !(e.root))))
      !(e.root);
  seen e

let resolve ?ctx:context_ e http =
  E.resolve_pending
    ~ctx:(Option.value context_ ~default:(context e))
    ~db:e.db ~manager:e.manager ~http e.order

let takeover e ?(root = root_ts) () =
  match
    Q.claim_resolution ~db:e.db ~key:(Q.key e.order root) ~owner_id:"successor"
      ~now:(Store.now ())
  with
  | Some lease -> lease
  | None -> Alcotest.fail "successor could not claim expired root"

let test_held_grant ?(paginated = false) () =
  with_env (fun e ->
      live e;
      let blocked, wake = Lwt.task () in
      let methods = ref [] in
      let held = if paginated then "conversations.members" else "auth.test" in
      let http ~verb ~token ~method_ ~params =
        methods := method_ :: !methods;
        if method_ = held && not (List.mem_assoc "cursor" params) then
          let* () = blocked in
          if paginated then
            Lwt.return
              ( 200,
                Yojson.Safe.to_string
                  (S.ok
                     [
                       ("members", `List [ str "U123" ]);
                       ( "response_metadata",
                         obj [ ("next_cursor", str "page2") ] );
                     ]) )
          else e.mock.http ~verb ~token ~method_ ~params
        else e.mock.http ~verb ~token ~method_ ~params
      in
      Lwt_main.run
        (let work = resolve e http in
         let* () = W.flush 20 in
         let expected = if paginated then 4 else 1 in
         Alcotest.(check int)
           "held real Grant response" expected (List.length !methods);
         S.at (Store.now () +. 121.);
         let successor = takeover e () in
         Lwt.wakeup wake ();
         let* () = work in
         Alcotest.(check int)
           "stale actor sends no next Grant page or read" expected
           (List.length !methods);
         check "successor still owns" true
           (Q.owns_resolution ~db:e.db ~lease:successor ~now:(Store.now ()));
         Alcotest.(check int)
           "stale actor creates no fire" 0
           (List.length (fires e));
         Lwt.return_unit))

let test_queued ?(root_read = false) ?posture () =
  with_env (fun e ->
      live e;
      let c = W.clock () in
      let limiter =
        Slack_read_limiter.create_with ~now:(fun () -> c.now) ~wait:(W.wait c)
      in
      let method_ =
        if root_read then "conversations.history" else "auth.test"
      in
      Slack_read_limiter.note_retry_after limiter ~key:("T123:A123:" ^ method_)
        17.;
      let ctx = { (context e) with Ctx.limiter = Some limiter } in
      Lwt_main.run
        (let work = resolve ~ctx e e.mock.http in
         let* () = W.flush 20 in
         let expected = if root_read then 5 else 0 in
         Alcotest.(check int)
           "HTTP before queued method" expected
           (List.length !(e.mock.calls));
         check "actual transport is waiting on shared limiter" true
           (Lwt.is_sleeping work);
         let successor =
           match posture with
           | None ->
               c.now <- c.now +. 121.;
               Some (takeover e ())
           | Some "reload" ->
               Session.update_config e.manager (S.config ());
               c.now <- c.now +. 17.;
               None
           | Some _ ->
               e.accepting := false;
               c.now <- c.now +. 17.;
               None
         in
         let* () = W.advance c 0. in
         let* () = work in
         Alcotest.(check int)
           "post-wait owner fence blocks HTTP" expected
           (List.length !(e.mock.calls));
         (match successor with
         | Some lease ->
             check "successor retained after waiter wakes" true
               (Q.owns_resolution ~db:e.db ~lease ~now:c.now)
         | None ->
             Q.dirty_channel ~db:e.db ~channel:e.order.channel_id;
             check "temporary posture cannot permanently settle root" true
               (Q.resolution_due ~db:e.db ~key:(Q.key e.order root_ts)
                  ~now:(c.now +. 121.)));
         W.cleanup c;
         Lwt.return_unit))

let second_root = "1700000002.000100"

let two_roots e =
  live e;
  let room =
    Option.get
      (Slack_huddle_classify.room_of_json
         (set "id" (str "RSECOND") (get "room" !(e.root))))
  in
  L.record_seen ~db:e.db ~team:e.order.team_id ~channel:e.order.channel_id
    ~source:"event" ~now:(Store.now ()) ~root_ts:second_root ~file_ids:[] room;
  S.override e.mock "conversations.history" (fun p ->
      let root = set "ts" (get "latest" p) !(e.root) in
      (200, S.ok [ ("messages", `List [ root ]) ]))

let test_cached_validation ?(take_over = false) () =
  with_env (fun e ->
      two_roots e;
      let c = W.clock () in
      let limiter =
        Slack_read_limiter.create_with ~now:(fun () -> c.now) ~wait:(W.wait c)
      in
      let ctx = { (context e) with Ctx.limiter = Some limiter } in
      Lwt_main.run
        (let work = resolve ~ctx e e.mock.http in
         let* () = W.flush 20 in
         Alcotest.(check int)
           "first root read" 1
           (count e "conversations.history");
         check "second owner's root request waits its method slot" true
           (Lwt.is_sleeping work);
         let successor =
           if take_over then (
             c.now <- c.now +. 121.;
             Some (takeover e ~root:second_root ()))
           else None
         in
         let* () = W.advance c (if take_over then 0. else 1.) in
         let* () = work in
         Alcotest.(check int)
           "read validation reused once" 1 (count e "auth.test");
         Alcotest.(check int)
           "current root transport respects its own owner"
           (if take_over then 1 else 2)
           (count e "conversations.history");
         Option.iter
           (fun lease ->
             check "second root successor preserved" true
               (Q.owns_resolution ~db:e.db ~lease ~now:c.now))
           successor;
         W.cleanup c;
         Lwt.return_unit))

let test_stale_hard () =
  with_env (fun e ->
      live e;
      let fire = S.fire e.db e.order ~room:"RPENDING" ~state:T.Fetching () in
      let before = runtime e fire.id in
      let blocked, wake = Lwt.task () in
      let http ~verb:_ ~token:_ ~method_:_ ~params:_ =
        let* () = blocked in
        Lwt.return (200, {|{"ok":false,"error":"invalid_auth"}|})
      in
      Lwt_main.run
        (let work = resolve e http in
         let* () = W.flush 20 in
         S.at (Store.now () +. 121.);
         let successor = takeover e () in
         Lwt.wakeup wake ();
         let* () = work in
         let current = Option.get (Store.find e.db e.order.id) in
         Alcotest.check S.state_t "stale invalid_auth cannot suspend" T.Active
           current.state;
         Alcotest.(check int) "epoch unchanged" e.order.epoch current.epoch;
         check "suspension reason unchanged" true
           (current.suspended_reason = None);
         check "successor ownership unchanged" true
           (Q.owns_resolution ~db:e.db ~lease:successor ~now:(Store.now ()));
         check "pre-dispatch fire and attempts unchanged" true
           (runtime e fire.id = before);
         Lwt.return_unit))

let test_current_hard () =
  with_env (fun e ->
      live e;
      let fire = S.fire e.db e.order ~room:"RPENDING" ~state:T.Fetching () in
      S.override e.mock "auth.test" (fun _ ->
          (200, obj [ ("ok", `Bool false); ("error", str "invalid_auth") ]));
      Lwt_main.run
        (E.pass ~ctx:(context e) ~db:e.db ~manager:e.manager ~http:e.mock.http
           ());
      let current = Option.get (Store.find e.db e.order.id) in
      Alcotest.check S.state_t "current hard failure suspends" T.Suspended
        current.state;
      Alcotest.(check int)
        "suspension bumps epoch once" (e.order.epoch + 1) current.epoch;
      check "current hard reason retained" true
        (current.suspended_reason = Some "credentials_invalid");
      check "suspension sweeps pre-dispatch fire" true
        ((runtime e fire.id).fire.state = T.Skipped_inactive);
      Alcotest.(check int)
        "current owner reports once" 1
        (count e "chat.postMessage"))

let test_queued_fire_grant () =
  with_env (fun e ->
      (* The ended root legitimately inserts a fire before its fresh Fire grant. *)
      seen e;
      let c = W.clock () in
      let limiter =
        Slack_read_limiter.create_with ~now:(fun () -> c.now) ~wait:(W.wait c)
      in
      let ctx = { (context e) with Ctx.limiter = Some limiter } in
      let http ~verb ~token ~method_ ~params =
        if method_ = "conversations.history" then
          Slack_read_limiter.note_retry_after limiter ~key:"T123:A123:auth.test"
            17.;
        e.mock.http ~verb ~token ~method_ ~params
      in
      Lwt_main.run
        (let work = resolve ~ctx e http in
         let* () = W.flush 20 in
         Alcotest.(check int)
           "Resolve grant and exact root completed" 6
           (List.length !(e.mock.calls));
         Alcotest.(check int)
           "fire inserted while ownership was valid" 1
           (List.length (fires e));
         check "fresh Fire grant waiting on shared limiter" true
           (Lwt.is_sleeping work);
         c.now <- c.now +. 121.;
         let successor = takeover e () in
         let* () = W.advance c 0. in
         let* () = work in
         Alcotest.(check int)
           "stale root actor cannot dispatch queued Fire grant" 6
           (List.length !(e.mock.calls));
         check "successor remains owner" true
           (Q.owns_resolution ~db:e.db ~lease:successor ~now:c.now);
         no_upload e;
         W.cleanup c;
         Lwt.return_unit))

let suite =
  [
    S.case "queued Fire validation still fences the resolving root owner"
      test_queued_fire_grant;
    S.case "held Grant response loses dispatch authority on takeover" (fun () ->
        test_held_grant ());
    S.case "paginated Grant stops at takeover" (fun () ->
        test_held_grant ~paginated:true ());
    S.case "queued Grant checks owner after shared limiter" (fun () ->
        test_queued ());
    S.case "queued exact root checks owner after shared limiter" (fun () ->
        test_queued ~root_read:true ());
    S.case "cached read validation rebuilds transport for next root" (fun () ->
        test_cached_validation ());
    S.case "cached read validation fences next root takeover" (fun () ->
        test_cached_validation ~take_over:true ());
    S.case "stale invalid_auth preserves order successor and fires"
      test_stale_hard;
    S.case "current invalid_auth still suspends sweeps and reports"
      test_current_hard;
    S.case "config publication during Grant wait preserves root" (fun () ->
        test_queued ~posture:"reload" ());
    S.case "config publication during root wait preserves root" (fun () ->
        test_queued ~root_read:true ~posture:"reload" ());
    S.case "stopped accepting during root wait preserves root" (fun () ->
        test_queued ~root_read:true ~posture:"stop" ());
  ]
