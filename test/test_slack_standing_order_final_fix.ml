open Lwt.Syntax
open Test_slack_standing_order_exec_support
module E = Slack_standing_order_exec
module D = Slack_standing_order_dispatch
module Relay = Test_slack_standing_order_relay
module H = Test_slack_standing_order_repost_support
module Grant = Slack_standing_order_grant
module Report = Slack_standing_order_report
module Sql = Slack_standing_order_sql
module Ctx = Slack_standing_order_ctx

let check = Alcotest.(check bool)
let integer = Alcotest.(check int)
let debits e = Store.dispatches_since e.db ~order_id:e.order.id ~since:0.

let test_admin_fence stage () =
  Relay.with_delivery (fun e lease r payload ->
      Test_slack_standing_order_grant_support.make_admin e.db "U123";
      Session.update_config e.manager
        (S.with_slack (Session.get_config e.manager) (fun sc ->
             {
               sc with
               standing_orders =
                 { sc.standing_orders with creator_policy = So_creator_admin };
             }));
      let allocations = ref 0 and uploads = ref 0 and armed = ref false in
      let scanned = ref false in
      let pending, release = Lwt.task () in
      let http ~verb ~token ~method_ ~params =
        let* response = e.mock.http ~verb ~token ~method_ ~params in
        if method_ = "files.getUploadURLExternal" then incr allocations;
        if method_ = "conversations.history" then scanned := true;
        (if method_ = "users.list" then
           armed :=
             match stage with
             | `Allocate -> !scanned && !allocations = 0
             | `Upload -> !allocations > 0 && !uploads = 0
             | `Complete -> !uploads > 0);
        Lwt.return response
      in
      let context =
        {
          (ctx e) with
          Ctx.limit =
            (fun ~channel_id:_ -> if !armed then pending else Lwt.return_unit);
        }
      in
      let upload_http req =
        incr uploads;
        Relay.raw_success req
      in
      Lwt_main.run
        (let delivery =
           D.deliver ~ctx:context ~db:e.db ~manager:e.manager ~http ~upload_http
             ~lease r payload
         in
         Lwt.finalize
           (fun () ->
             let* () = Lwt.pause () in
             check "final write limiter is pending" true
               (!armed && Lwt.is_sleeping delivery);
             check "admin removed during limiter wait" true
               (Admin.remove_admin ~db:e.db ~channel:"slack" ~sender_id:"U123");
             Lwt.wakeup_later release ();
             delivery)
           (fun () ->
             Lwt.cancel pending;
             Lwt.cancel delivery;
             Lwt.return_unit));
      integer "no subsequent raw writes"
        (if stage = `Complete then 1 else 0)
        !uploads;
      integer "no completion" 0 (count e "files.completeUploadExternal");
      integer "no dispatch debit" 0 (debits e);
      let order = Option.get (Store.find e.db e.order.id) in
      check "typed admin refusal suspends" true
        (order.state = T.Suspended
        && order.suspended_reason = Some "grantor_not_admin");
      Lwt_main.run
        (E.report_order ~ctx:(ctx e) ~db:e.db ~manager:e.manager
           ~http:e.mock.http e.order);
      Alcotest.(check (list (pair string string)))
        "grantor report"
        [ ("grantor_unauthorised", "sent") ]
        (Report.statuses ~db:e.db ~order_id:e.order.id))

let complete_pass ?(upload_http = Relay.raw_success) e =
  E.pass ~ctx:(ctx e) ~db:e.db ~manager:e.manager ~roots:[] ~http:e.mock.http
    ~upload_http ()

let test_write_authority method_ fault () =
  Relay.with_delivery (fun e lease r _payload ->
      Store.release e.db ~fire_id:r.fire.id ~lease ~now:(Store.now ());
      S.override e.mock "conversations.history" (fun p ->
          ( 200,
            S.ok
              [
                ( "messages",
                  if get "limit" p = `Int 1 then `List [ !(e.root) ]
                  else `List [] );
                S.no_more;
              ] ));
      (match fault with
      | `Scope ->
          S.override e.mock method_ (fun _ ->
              (200, obj [ ("ok", `Bool false); ("error", str "missing_scope") ]))
      | `Egress ->
          let cfg = Session.get_config e.manager in
          let rules =
            List.filter
              (fun (rule : Runtime_config.egress_rule) ->
                rule.host = "files.slack.com" && method_ <> "upload")
              cfg.egress.default_allowlist
          in
          let rules =
            rules
            @ List.filter_map
                (fun (m, verb) ->
                  if m = method_ then None
                  else Some (S.slack_rule ~method_:verb ("/api/" ^ m)))
                Slack_standing_order_slack.methods
          in
          Session.update_config e.manager
            { cfg with egress = { cfg.egress with default_allowlist = rules } });
      H.pickup_reads e;
      let raw = ref 0 in
      let upload_http req =
        incr raw;
        Relay.raw_success req
      in
      Lwt_main.run (complete_pass ~upload_http e);
      integer "no unauthorized raw write"
        (if method_ = "files.completeUploadExternal" then 1 else 0)
        !raw;
      let allocations = count e "files.getUploadURLExternal" in
      let reason =
        match fault with
        | `Scope -> Grant.Missing_scope method_
        | `Egress -> Grant.Egress_denied method_
      in
      let order = Option.get (Store.find e.db e.order.id) in
      check "write authority failure suspends" true (order.state = T.Suspended);
      check "exact write operation retained" true
        (order.suspended_reason = Some (Grant.reason_code reason));
      let saved = runtime e r.fire.id in
      check "fixed fire cause" true
        (saved.fire.reason = Some (Grant.reason_code reason));
      let dispatched =
        fault = `Scope && method_ = "files.completeUploadExternal"
      in
      integer "only dispatched completion debited"
        (if dispatched then 1 else 0)
        (debits e);
      check "retained completed attempt" true
        ((not dispatched) || (D.attempt saved).dispatched_at <> None);
      check "no delivery retry" true (not (Store.sources_mutable saved));
      Lwt_main.run (complete_pass ~upload_http e);
      integer "no new allocation on next pass" allocations
        (count e "files.getUploadURLExternal");
      integer "no new upload on next pass"
        (if method_ = "files.completeUploadExternal" then 1 else 0)
        !raw;
      check "retained attempt unchanged" true
        ((runtime e r.fire.id).attempts = saved.attempts);
      integer "retained debit unchanged"
        (if dispatched then 1 else 0)
        (debits e);
      integer "no blind completion retry"
        (if dispatched then 1 else 0)
        (count e "files.completeUploadExternal");
      Alcotest.(check (list (pair string string)))
        "one exact fixed report"
        [ (Report.cause_key (Report.of_hard_reason reason), "sent") ]
        (Report.statuses ~db:e.db ~order_id:e.order.id))

let seed_terminal e room at =
  let f = S.fire e.db e.order ~room () in
  with_claim e f.id (fun lease r ->
      H.persist e lease r { r with fire = { r.fire with state = T.Fetching } };
      let r = H.seed e lease (runtime e f.id) true in
      let ts =
        Printf.sprintf "%.6f"
          (at
          +. float_of_int
               (Sql.int_query e.db
                  "SELECT COUNT(*) FROM slack_standing_order_fires" []))
      in
      let a =
        {
          (D.attempt r) with
          picked_up_at = Some at;
          acknowledged_at = Some at;
          relay_ts = Some ts;
        }
      in
      H.persist e lease r
        {
          (D.update_attempt r a) with
          fire = { r.fire with state = T.Picked_up; relay_ts = Some ts };
        });
  Sql.exec e.db
    "UPDATE slack_standing_order_dispatches SET dispatched_at=? WHERE fire_id=?"
    [ Sqlite3.Data.FLOAT at; Sql.text f.id ];
  f.id

let successor e =
  ignore
    (H.ok
       (Store.deactivate e.db ~order_id:e.order.id (Store.Cancel "U123")
          ~now:(Store.now ())));
  let order = S.seed e.db () in
  let card =
    Store.insert_card e.db ~kind:T.Pause_button ~order ~created_by:"U123"
      ~now:(Store.now ()) ()
  in
  Store.set_card_message_ts e.db ~card_id:card.id "1700000000.000200";
  { e with order }

let cancelled_history e =
  let rec seed e n ids =
    if n = 3 then (e, ids)
    else
      let batch =
        List.init 2 (fun i ->
            seed_terminal e
              (Printf.sprintf "RCANCEL%d_%d" n i)
              (Store.now () -. 7200.))
      in
      seed (successor e) (n + 1) (ids @ batch)
  in
  seed e 0 []

let test_worker_cancelled_history () =
  with_env (fun e ->
      let e, retained = cancelled_history e in
      Test_slack_standing_order_exec.setup e;
      H.pickup_reads e;
      e.mock.calls := [];
      let c = Test_slack_standing_order_worker.clock () in
      Lwt_main.run
        (Lwt.finalize
           (fun () ->
             E.start
               ~ctx:(Test_slack_standing_order_worker.context e c)
               ~db:e.db ~manager:e.manager ~http:e.mock.http
               ~upload_http:Relay.raw_success ();
             cover_order e.db e.order;
             seen e;
             ready e ();
             E.wake ();
             let* () = Test_slack_standing_order_worker.flush 100 in
             let rec dispatch_first = function
               | "files.completeUploadExternal" :: _ -> true
               | "reactions.get" :: _ | [] -> false
               | _ :: rest -> dispatch_first rest
             in
             check "newest order dispatches before cancelled history" true
               (dispatch_first (S.methods_called e.mock));
             integer "one terminal poll across all channel orders" 1
               (List.length
                  (List.filter
                     (fun id -> (runtime e id).next_attempt_at <> None)
                     retained));
             integer "one historical poll reads file and message reactions" 2
               (count e "reactions.get");
             Lwt.return_unit)
           (fun () ->
             let* () = E.stop () in
             Test_slack_standing_order_worker.cleanup c;
             Lwt.return_unit)))

let test_acknowledged_grant_retry () =
  with_env (fun e ->
      let e, retained = cancelled_history e in
      let now = Store.now () in
      Report.init e.db;
      Slack_standing_order_pickup.init e.db;
      List.iter
        (fun id ->
          Sql.exec e.db
            "UPDATE slack_standing_order_dispatches SET picked_up_at=NULL, \
             acknowledged_at=?,pickup_started_at=CAST(relay_ts AS REAL) WHERE \
             fire_id=?"
            [ Sqlite3.Data.FLOAT (now -. 7200.); Sql.text id ];
          Sql.exec e.db
            "UPDATE slack_standing_order_fires SET \
             state='acknowledged',reason='acknowledged_no_reply' WHERE id=?"
            [ Sql.text id ];
          (* Isolate tracking calls from the independent outcome report path. *)
          Sql.exec e.db
            "INSERT OR IGNORE INTO slack_standing_order_reports \
             (order_id,cause_key,period_key,fire_id,status) \
             VALUES(?,'reply_delayed','once',?,'sent')"
            [ Sql.text (runtime e id).fire.order_id; Sql.text id ])
        retained;
      integer "no observations queued" 0
        (Sql.int_query e.db
           "SELECT COUNT(*) FROM slack_standing_order_observations" []);
      let calls = ref 0 in
      let http ~verb:_ ~token:_ ~method_ ~params:_ =
        Alcotest.(check string) "tracking grant fails first" "auth.test" method_;
        incr calls;
        Lwt.return (503, "{}")
      in
      let pass () =
        E.pass ~ctx:(ctx e) ~db:e.db ~manager:e.manager ~http ~roots:[] ()
      in
      integer "six retained acknowledgements are due" 6
        (List.length (Store.due e.db ~now));
      Lwt_main.run (pass ());
      integer "failed grant uses one channel terminal budget" 1 !calls;
      let attempted =
        List.filter (fun id -> (runtime e id).next_attempt_at <> None) retained
      in
      integer "one terminal cadence armed before failed grant" 1
        (List.length attempted);
      List.iter
        (fun id ->
          check "acknowledged marker survives empty drain" true
            ((runtime e id).fire.reason = Some "acknowledged_no_reply");
          check "failed grant persists full cadence" true
            ((runtime e id).next_attempt_at = Some (now +. 300.)))
        attempted;
      (* Each subsequent job may track one other retained fire, never retry
         one already attempted. Once all six are scheduled, no calls remain. *)
      for i = 1 to 5 do
        S.at (now +. float_of_int (i * 5));
        Lwt_main.run (pass ());
        integer "one terminal grant per channel job" (i + 1) !calls
      done;
      List.iter
        (fun seconds ->
          S.at (now +. seconds);
          Lwt_main.run (pass ());
          integer "no terminal repoll before 300 seconds" 6 !calls)
        [ 30.; 100.; 299. ];
      S.at (now +. 300.);
      Lwt_main.run (pass ());
      integer "first terminal becomes due at 300 seconds" 7 !calls)

let test_worker_history () =
  with_env (fun e ->
      Test_slack_standing_order_exec.setup e;
      Sql.exec e.db
        "UPDATE slack_standing_orders SET max_fires_per_day=1000 WHERE id=?"
        [ Sql.text e.order.id ];
      let now = Store.now () in
      let old =
        List.init 12 (fun i ->
            seed_terminal e ("ROLD" ^ string_of_int i) (now -. (31. *. 86400.)))
      in
      let retained =
        List.init 12 (fun i ->
            seed_terminal e ("RRECENT" ^ string_of_int i) (now -. 60.))
      in
      H.pickup_reads e;
      let epochs id =
        Sql.int_query e.db
          "SELECT lease_epoch FROM slack_standing_order_fires WHERE id=?"
          [ Sql.text id ]
      in
      let calls = ref [] in
      let http ~verb ~token ~method_ ~params =
        calls := method_ :: !calls;
        e.mock.http ~verb ~token ~method_ ~params
      in
      let c = Test_slack_standing_order_worker.clock () in
      let w = Test_slack_standing_order_worker.context e c in
      Lwt_main.run
        (Lwt.finalize
           (fun () ->
             E.start ~ctx:w ~db:e.db ~manager:e.manager ~http
               ~upload_http:Relay.raw_success ();
             cover_order e.db e.order;
             seen e;
             ready e ();
             E.wake ();
             let* () = Test_slack_standing_order_worker.flush 100 in
             List.iter
               (fun id ->
                 check "expired history never claimed or polled" true
                   (epochs id = 1))
               old;
             let chronological = List.rev !calls in
             let rec dispatch_first = function
               | "files.completeUploadExternal" :: _ -> true
               | "reactions.get" :: _ | [] -> false
               | _ :: rest -> dispatch_first rest
             in
             check "fresh huddle dispatch precedes retained tracking" true
               (dispatch_first chronological);
             integer "one retained terminal polled per pass" 2
               (count e "reactions.get");
             let polled = List.filter (fun id -> epochs id > 1) retained in
             integer "bounded retained batch" 1 (List.length polled);
             let id = List.hd polled in
             let before_epoch = epochs id in
             check "terminal has future poll" true
               (Option.fold ~none:false
                  ~some:(fun t -> t >= c.now +. 300.)
                  (runtime e id).next_attempt_at);
             let* () = Test_slack_standing_order_worker.advance c 5. in
             integer "retained terminal is not repolled at five seconds"
               before_epoch (epochs id);
             Slack_standing_order_pickup.enqueue ~db:e.db ~team_id:"T123"
               ~api_app_id:"A123" ~now:c.now
               (Slack_huddle_classify.Target_reply
                  {
                    channel = "C123";
                    thread_ts = Option.get (runtime e id).fire.relay_ts;
                    ts = Printf.sprintf "%.6f" (c.now +. 1.);
                    username = None;
                    bot_id = "BCLAUDE";
                    app_id = Some "ACLAUDE";
                    kind_if_first = Notice Disabled;
                    kind_if_later = Notice Disabled;
                  });
             E.wake ();
             let* () = Test_slack_standing_order_worker.flush 100 in
             check "queued late notice drains before next poll" true
               ((runtime e id).fire.state = T.Not_delivered);
             Lwt.return_unit)
           (fun () ->
             let* () = E.stop () in
             Test_slack_standing_order_worker.cleanup c;
             Lwt.return_unit)))

let test_expired_cancelled () =
  with_env (fun e ->
      let at = Store.now () -. (31. *. 86400.) in
      List.iteri
        (fun i state ->
          let id = seed_terminal e ("REXPIRED" ^ string_of_int i) at in
          Sql.exec e.db
            "UPDATE slack_standing_order_fires SET state=? WHERE id=?"
            [ Sql.text (T.fire_state_to_string state); Sql.text id ])
        T.[ No_pickup; Not_delivered; Picked_up ];
      ignore
        (Store.deactivate e.db ~order_id:e.order.id (Store.Cancel "U123")
           ~now:(Store.now ()));
      e.mock.calls := [];
      Lwt_main.run (complete_pass e);
      integer "cancelled expired history makes zero HTTP calls" 0
        (List.length !(e.mock.calls)))

let suite =
  List.map
    (fun (stage, name) ->
      S.case (name ^ " limiter rechecks admin row") (test_admin_fence stage))
    [
      (`Allocate, "allocation");
      (`Upload, "raw upload");
      (`Complete, "completion");
    ]
  @ List.map
      (fun m ->
        S.case
          (m ^ " missing scope suspends and reports")
          (test_write_authority m `Scope))
      [ "files.getUploadURLExternal"; "files.completeUploadExternal" ]
  @ List.map
      (fun m ->
        S.case
          (m ^ " write-only egress refusal suspends and reports")
          (test_write_authority m `Egress))
      [ "files.getUploadURLExternal"; "upload"; "files.completeUploadExternal" ]
  @ [
      S.case "worker shares terminal budget across cancelled orders"
        test_worker_cancelled_history;
      S.case "aged acknowledged grant failure retains cadence and budget"
        test_acknowledged_grant_retry;
      S.case "worker bounds historical tracking and terminal polling"
        test_worker_history;
      S.case "expired completed history stays offline on cancelled orders"
        test_expired_cancelled;
    ]
