open Lwt.Syntax
module L = Slack_read_limiter

let check = Alcotest.(check bool)

type timer = { due : float; promise : unit Lwt.t; wake : unit Lwt.u }
type clock = { mutable now : float; mutable timers : timer list }

let clock () = { now = 0.; timers = [] }

let wait c ~seconds =
  let promise, wake = Lwt.task () in
  c.timers <- { due = c.now +. seconds; promise; wake } :: c.timers;
  promise

let rec flush n =
  if n = 0 then Lwt.return_unit
  else
    let* () = Lwt.pause () in
    flush (n - 1)

let advance c seconds =
  c.now <- c.now +. seconds;
  List.iter
    (fun t ->
      if Lwt.is_sleeping t.promise && t.due <= c.now then Lwt.wakeup t.wake ())
    c.timers;
  c.timers <- List.filter (fun t -> Lwt.is_sleeping t.promise) c.timers;
  flush 20

let limiter c = L.create_with ~now:(fun () -> c.now) ~wait:(wait c)

let cleanup c =
  let pending = List.filter (fun t -> Lwt.is_sleeping t.promise) c.timers in
  List.iter (fun t -> Lwt.cancel t.promise) pending;
  Alcotest.(check int) "no dangling timers" 0 (List.length pending)

let with_clock f =
  let c = clock () in
  Fun.protect
    ~finally:(fun () -> List.iter (fun t -> Lwt.cancel t.promise) c.timers)
    (fun () ->
      Lwt_main.run
        (let* () = f c (limiter c) in
         cleanup c;
         Lwt.return_unit))

let key = "T123:A123:conversations.history"

let test_slots () =
  with_clock (fun c l ->
      let* () = L.wrap l ~key () in
      let second = L.wrap l ~key () and third = L.wrap l ~key () in
      let* () = L.wrap l ~key:"T123:A123:files.info" () in
      let* () = L.wrap l ~key:"T456:A123:conversations.history" () in
      let* () = L.wrap l ~key:"T123:A456:conversations.history" () in
      check "second waits" true (Lwt.is_sleeping second);
      let* () = advance c 0.999 in
      check "full second required" true (Lwt.is_sleeping second);
      let* () = advance c 0.001 in
      let* () = second in
      check "third waits another slot" true (Lwt.is_sleeping third);
      let* () = advance c 1. in
      third)

let test_cooldown () =
  with_clock (fun c l ->
      let* () = L.wrap l ~key () in
      let next = L.wrap l ~key () in
      L.note_retry_after l ~key 17.;
      L.note_retry_after l ~key 2.;
      let* () = advance c 1. in
      check "queued waiter rechecks new cooldown" true (Lwt.is_sleeping next);
      let* () = advance c 15.999 in
      check "shorter cooldown cannot release early" true (Lwt.is_sleeping next);
      let* () = advance c 0.001 in
      next)

let test_priority () =
  with_clock (fun c l ->
      let* () = L.wrap l ~key () in
      let maintenance = L.wrap l ~key ~priority:Maintenance () in
      let live = L.wrap l ~key ~priority:Live () in
      let pause = L.wrap l ~key ~priority:Pause () in
      let* () = advance c 1. in
      let* () = pause in
      check "live still waits" true (Lwt.is_sleeping live);
      check "maintenance still waits" true (Lwt.is_sleeping maintenance);
      let* () = advance c 1. in
      let* () = live in
      let* () = advance c 1. in
      maintenance)

let test_cancel () =
  with_clock (fun c l ->
      let* () = L.wrap l ~key () in
      let cancelled = L.wrap l ~key () in
      let next = L.wrap l ~key () in
      Lwt.cancel cancelled;
      let* () = flush 20 in
      check "cancel propagates" true
        (Lwt.state cancelled = Lwt.Fail Lwt.Canceled);
      let* () = advance c 1. in
      next)

let test_posts () =
  with_clock (fun c l ->
      let* () = L.post_pacer l ~channel:"C123" () in
      let second = L.post_pacer l ~channel:"C123" () in
      let* () = L.post_pacer l ~channel:"C456" () in
      check "channel shares post budget" true (Lwt.is_sleeping second);
      let* () = advance c 1. in
      second)

module S = Test_slack_standing_order_support
module SO = Slack_standing_order_slack

let snapshot db cfg =
  match SO.snapshot ~db ~config:cfg ~channel_id:"C123" ~order_ref:None with
  | Ok snapshot -> snapshot
  | Error error -> Alcotest.fail error

let check_bool = check

let expect_error label expected result =
  check label true (result = Error expected)

let expect_ok label = function
  | Ok json -> json
  | Error error -> Alcotest.fail (label ^ ": " ^ SO.error_code error)

let test_joint_post_budget () =
  with_clock (fun c limiter ->
      let open Lwt.Syntax in
      let db = S.fresh_db () and cfg = S.config () and calls = ref 0 in
      let* () = L.post_pacer limiter ~channel:"C123" () in
      let call =
        SO.make ~db
          ~current_config:(fun () -> cfg)
          ~snapshot:(snapshot db cfg) ~channel_id:"C123"
          ~audit:"standing_order:so_1:fire_1" ~limiter
          ~http:(fun ~verb:_ ~token:_ ~method_:_ ~params:_ ->
            incr calls;
            Lwt.return (200, Yojson.Safe.to_string (S.ok [])))
          ()
      in
      let queued =
        call "files.completeUploadExternal" [ ("channel_id", `String "C123") ]
      in
      check "public pacer and transport share the same post budget" true
        (Lwt.is_sleeping queued);
      L.note_retry_after limiter ~key:"unbound:files.completeUploadExternal" 17.;
      let* () = advance c 1. in
      check "later 429 still binds after post deadline" true
        (Lwt.is_sleeping queued);
      let* () = advance c 15.999 in
      check "joint reservation cannot escape cooldown" true
        (Lwt.is_sleeping queued);
      let* () = advance c 0.001 in
      let+ result = queued in
      ignore (expect_ok "joint reservation" result);
      Alcotest.(check int) "one HTTP at final deadline" 1 !calls)

let test_completion_retry () =
  with_clock (fun c limiter ->
      let open Lwt.Syntax in
      let db = S.fresh_db () and cfg = S.config () and calls = ref 0 in
      let call =
        SO.make ~db
          ~current_config:(fun () -> cfg)
          ~snapshot:(snapshot db cfg) ~channel_id:"C123"
          ~audit:"standing_order:so_1:fire_1" ~limiter
          ~http_response:(fun ~verb:_ ~token:_ ~method_:_ ~params:_ ->
            incr calls;
            Lwt.return
              {
                SO.status = 429;
                headers = [ ("Retry-After", "17") ];
                body = "";
              })
          ()
      in
      let* result =
        call "files.completeUploadExternal" [ ("channel_id", `String "C123") ]
      in
      expect_error "completion reports 429 without replay" SO.Rate_limited
        result;
      let* () = advance c 100. in
      Alcotest.(check int) "transport never replays a write" 1 !calls;
      Lwt.return_unit)

let test_header_adapters () =
  List.iter
    (fun adapter ->
      Test_slack_standing_order_exec_support.with_env (fun e ->
          with_clock (fun c limiter ->
              let open Lwt.Syntax in
              let cfg = Session.get_config e.manager in
              let calls = ref 0 in
              let http_response ~verb:_ ~token:_ ~method_:_ ~params:_ =
                incr calls;
                Lwt.return
                  {
                    SO.status = 429;
                    headers = [ ("Retry-After", "17") ];
                    body = "";
                  }
              in
              let ctx =
                {
                  (Test_slack_standing_order_exec_support.ctx e) with
                  Slack_standing_order_ctx.limiter = Some limiter;
                }
              in
              let* () =
                match adapter with
                | `Fetch ->
                    let+ result =
                      Slack_standing_order_fetch.evaluate ~ctx ~db:e.db
                        ~manager:e.manager ~http_response
                        ~stage:Slack_standing_order_grant.Resolve e.order
                    in
                    check_bool "fetch preserves typed refusal" true
                      (Result.is_error result)
                | `Track ->
                    let fire =
                      S.fire e.db e.order ~room:"RTEST"
                        ~state:Slack_standing_order_types.Posted
                        ~dispatched_at:S.now ()
                    in
                    let runtime =
                      Option.get
                        (Slack_standing_order_store.runtime e.db
                           ~fire_id:fire.id)
                    in
                    Lwt.catch
                      (fun () ->
                        let+ _ =
                          Slack_standing_order_dispatch.track ~ctx ~db:e.db
                            ~manager:e.manager ~http_response runtime e.order
                        in
                        Alcotest.fail "expected tracking refusal")
                      (function
                        | Slack_standing_order_dispatch.Tracking_refused _ ->
                            Lwt.return_unit
                        | exn -> Lwt.fail exn)
              in
              Alcotest.(check int)
                "header adapter reaches one actual HTTP" 1 !calls;
              let snap = snapshot e.db cfg in
              let call =
                SO.make ~db:e.db
                  ~current_config:(fun () -> cfg)
                  ~snapshot:snap ~channel_id:"C123"
                  ~audit:"standing_order:so_1:probe" ~limiter ~team_id:"T123"
                  ~app_id:"A123"
                  ~http:(fun ~verb:_ ~token:_ ~method_:_ ~params:_ ->
                    Lwt.return (200, Yojson.Safe.to_string (S.ok [])))
                  ()
              in
              let queued = call "auth.test" [] in
              let* () = advance c 16.999 in
              check_bool "adapter kept actual Retry-After" true
                (Lwt.is_sleeping queued);
              let* () = advance c 0.001 in
              let+ result = queued in
              ignore (expect_ok "shared adapter budget" result))))
    [ `Fetch; `Track ]

let suite =
  List.map
    (fun (name, f) -> S.case name f)
    [
      ( "public post pacer and transport share joint cooldown",
        test_joint_post_budget );
      ("completion 429 never replays the write", test_completion_retry);
      ("fetch and tracking preserve grant headers", test_header_adapters);
      ("same installation and method serialize", test_slots);
      ("queued waiters honor extended retry after", test_cooldown);
      ("pause then live precede queued maintenance", test_priority);
      ("cancel removes waiter and timer", test_cancel);
      ("channel post pacing is shared", test_posts);
    ]
