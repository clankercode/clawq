open Test_slack_standing_order_reconcile_support
open Lwt.Syntax

let pending db =
  Sql.int_query db
    "SELECT count(*) FROM slack_order_ingress WHERE applied=0 AND pause=0" []

let socket_loop e socket handle =
  Slack_socket.run
    ~sleep:(fun seconds -> wait e.clock ~seconds)
    ~now:(fun () -> e.clock.now)
    ~connect:(fun () -> Lwt.return socket.Test_slack_socket.transport)
    ~handle_event:handle
    ~on_health:(fun _ -> ())
    ()

let orphan_ingress ?(ack_failure = false) () =
  with_env ~socket:true ~disk:true (fun e ->
      with_transport e (fun () ->
          let socket = Test_slack_socket.make_socket () in
          let handlers = ref 0 in
          let handle body =
            incr handlers;
            let+ _ =
              Slack.handle_event
                ~config:
                  (Option.get
                     (Session.get_config e.f.base.manager).channels.slack)
                ~session_manager:e.f.base.manager body
            in
            ()
          in
          let connector = socket_loop e socket handle in
          let* () = settle 40 in
          let* () = socket.emit {|{"type":"hello"}|} in
          let* () = pass e in
          check "initial full coverage" true (covered e);
          let body =
            event ~id:"Ev-review-ack-loss"
              (obj
                 [
                   ("type", str "message");
                   ("channel", str "C123");
                   ("user", str "U123");
                   ("ts", str "1700000090.000123");
                   ("text", str "ordinary fixture metadata");
                 ])
          in
          let ack, ack_u = Lwt.task () in
          socket.ack_pending := Some ack;
          let delivery =
            socket.emit
              (Yojson.Safe.to_string
                 (obj
                    [
                      ("type", str "events_api");
                      ("envelope_id", str "review-ack-loss");
                      ("payload", Yojson.Safe.from_string body);
                    ]))
          in
          check "actual ACK is blocked" true (Lwt.is_sleeping delivery);
          eq "ordinary metadata reserved before ACK" 1 (pending e.f.db);
          eq "handler has not started" 0 !handlers;
          let* () =
            if ack_failure then (
              Lwt.wakeup_later_exn ack_u (Failure "injected ACK failure");
              delivery)
            else socket.emit {|{"type":"disconnect"}|}
          in
          let* () = settle 40 in
          let* () = Slack_standing_order_resolution_lease.cancel connector in
          eq "disconnect lost handler before dispatch" 0 !handlers;
          let* () = C.stop () in
          check "fixture DB actually closes" true (Sqlite3.db_close e.f.db);
          let db = Sqlite3.db_open (Option.get e.disk_path) in
          let manager =
            Session.create ~config:(Session.get_config e.f.base.manager) ~db ()
          in
          C.configure ~db ~manager ~ctx:e.ctx;
          let socket2 = Test_slack_socket.make_socket () in
          let connector2 = socket_loop e socket2 (fun _ -> Lwt.return_unit) in
          Lwt.finalize
            (fun () ->
              let* () = settle 40 in
              let* () = socket2.emit {|{"type":"hello"}|} in
              let rec scans n =
                if n = 0 then Lwt.return_unit
                else
                  let* () =
                    drain e.clock
                      (C.pass_with ~ctx:e.ctx ~http:e.http ~db ~manager e.order)
                  in
                  let* () = advance e.clock (e.clock.now +. 301.) in
                  scans (n - 1)
              in
              let* () = scans 3 in
              Printf.printf
                "REVIEW_ORPHAN after_reopen pending=%d phase=%s covered=%b\n%!"
                (pending db)
                (Option.value (R.phase db e.order.id) ~default:"none")
                (R.covered db e.order.id e.order.epoch);
              eq "full replay retires covered abandoned ordinary delivery" 0
                (pending db);
              check "new hello plus full replay reopens without redelivery" true
                (R.covered db e.order.id e.order.epoch);
              eq "recovery does not invent handler dispatch" 0 !handlers;
              Lwt.return_unit)
            (fun () ->
              let* () =
                Slack_standing_order_resolution_lease.cancel connector2
              in
              let* () = C.stop () in
              ignore (Sqlite3.db_close db);
              Lwt.return_unit)))

let gap_key_store_failure_reopen () =
  with_env ~disk:true (fun e ->
      let* () = pass e in
      let last =
        Option.get
          (Option.get (R.load_coverage e.f.db ~order_id:e.order.id))
            .last_success
      in
      let* () = advance e.clock (e.clock.now +. 172900.) in
      !Slack_socket.on_gap ~cause:"handler_failure";
      let upper = e.clock.now in
      let timestamp = Slack_standing_order_reconcile_scan.timestamp in
      let key =
        "reconcile_gap:" ^ timestamp last ^ "-" ^ timestamp (upper -. 172800.)
      in
      Sql.exec e.f.db
        "CREATE TEMP TRIGGER review_fail_gap_key BEFORE INSERT ON \
         slack_standing_order_reports WHEN NEW.cause_key LIKE \
         'reconcile_gap:%' BEGIN SELECT RAISE(ABORT,'review-gap-key-fault'); \
         END"
        [];
      let* () = pass e in
      check "failed key does not move watermark" true
        ((Option.get (R.load_coverage e.f.db ~order_id:e.order.id)).last_success
       = Some last);
      eq "failed exact key is absent" 0
        (Sql.int_query e.f.db
           "SELECT count(*) FROM slack_standing_order_reports WHERE cause_key=?"
           [ Sql.text key ]);
      let* () = C.stop () in
      check "fixture DB actually closes after fault" true
        (Sqlite3.db_close e.f.db);
      let* () = advance e.clock (e.clock.now +. 121.) in
      let db = Sqlite3.db_open (Option.get e.disk_path) in
      let manager =
        Session.create ~config:(Session.get_config e.f.base.manager) ~db ()
      in
      C.configure ~db ~manager ~ctx:e.ctx;
      check "startup cannot retain old verified release authority" false
        (R.covered db e.order.id e.order.epoch);
      check "startup preserves actual old success watermark" true
        ((Option.get (R.load_coverage db ~order_id:e.order.id)).last_success
       = Some last);
      let new_upper = e.clock.now in
      let histories = count e "conversations.history" in
      let prior = !(e.answer) and posts = ref 0 in
      (e.answer :=
         fun m p ->
           if m = "chat.postMessage" then (
             incr posts;
             Lwt.return (503, obj []))
           else prior m p);
      Lwt.finalize
        (fun () ->
          let* () =
            drain e.clock
              (C.pass_with ~ctx:e.ctx ~http:e.http ~db ~manager e.order)
          in
          check "restart performs fresh bounded history sweep" true
            (count e "conversations.history" > histories);
          let coverage = Option.get (R.load_coverage db ~order_id:e.order.id) in
          Alcotest.(check string)
            "new generation has new fixed upper bound" (timestamp new_upper)
            coverage.latest;
          check "new watermark belongs only to newly completed sweep" true
            (coverage.last_success = Some new_upper);
          let keys =
            Sql.query db
              "SELECT cause_key FROM slack_standing_order_reports WHERE \
               cause_key LIKE 'reconcile_gap:%'"
              [] (fun s -> Sql_util.text_column s 0)
          in
          Printf.printf "REVIEW_GAP_KEY original=%s actual=%s covered=%b\n%!"
            key (String.concat "," keys)
            (R.covered db e.order.id e.order.epoch);
          check "original fixed report key survives fault then immediate reopen"
            true (List.mem key keys);
          eq "original report is unique" 1
            (List.length (List.filter (( = ) key) keys));
          let shifted = "reconcile_gap:1700000100.000000-1700000321.000000" in
          check "later scan cannot substitute a shifted original window" false
            (List.mem shifted keys);
          check "later uncovered tail is separately reported" true
            (List.mem "reconcile_gap:1700000200.000000-1700000321.000000" keys);
          check "new sweep succeeds after preserving original report obligation"
            true
            (R.covered db e.order.id e.order.epoch);
          check "original report transport failure remains inspectable" true
            (Sql.int_query db
               "SELECT count(*) FROM slack_standing_order_reports WHERE \
                cause_key=? AND status<>'sent'"
               [ Sql.text key ]
            = 1);
          check "own report HTTP failure actually reached transport" true
            (!posts > 0);
          let count_before = !posts in
          let* () = C.stop () in
          let db2 = Sqlite3.db_open (Option.get e.disk_path) in
          let manager2 =
            Session.create ~config:(Session.get_config manager) ~db:db2 ()
          in
          C.configure ~db:db2 ~manager:manager2 ~ctx:e.ctx;
          Lwt.finalize
            (fun () ->
              let* () =
                drain e.clock
                  (C.pass_with ~ctx:e.ctx ~http:e.http ~db:db2 ~manager:manager2
                     e.order)
              in
              eq "ambiguous original reports never blindly repost after reopen"
                count_before !posts;
              eq "original failed report identity survives another Session" 1
                (Sql.int_query db2
                   "SELECT count(*) FROM slack_standing_order_reports WHERE \
                    cause_key=?"
                   [ Sql.text key ]);
              Lwt.return_unit)
            (fun () ->
              let* () = C.stop () in
              ignore (Sqlite3.db_close db2);
              Lwt.return_unit))
        (fun () ->
          let* () = C.stop () in
          ignore (Sqlite3.db_close db);
          Lwt.return_unit))

let suite =
  [
    S.case "preACK disconnect replay survives actual DB reopen"
      (orphan_ingress ~ack_failure:false);
    S.case "ACK failure replay survives actual DB reopen"
      (orphan_ingress ~ack_failure:true);
    S.case "failed gap INSERT then immediate DB reopen preserves exact window"
      gap_key_store_failure_reopen;
  ]

let ordinary_body ?(ts = "1700000090.000321") ?(text = "ordinary metadata") id =
  event ~id
    (obj
       [
         ("type", str "message");
         ("channel", str "C123");
         ("user", str "U123");
         ("ts", str ts);
         ("text", str text);
       ])

let pending_all db =
  Sql.int_query db "SELECT count(*) FROM slack_order_ingress WHERE applied=0" []

let active_old_handler () =
  with_env ~disk:true (fun e ->
      with_transport e (fun () ->
          let* () = pass e in
          let gate, u = Lwt.task () in
          let entered = ref false and prior = !(e.answer) in
          (e.answer :=
             fun m p ->
               if m = "users.info" && Slack_huddles.field "user" p = "U123" then (
                 entered := true;
                 let* () = gate in
                 prior m p)
               else prior m p);
          let body = ordinary_body "Ev-live-owner" in
          let old =
            Slack.handle_event
              ~config:
                (Option.get (Session.get_config e.f.base.manager).channels.slack)
              ~session_manager:e.f.base.manager body
          in
          let* () = settle 40 in
          check "real ordinary handler is blocked" true !entered;
          let* () = C.stop () in
          let db = Sqlite3.db_open (Option.get e.disk_path) in
          let manager =
            Session.create ~config:(Session.get_config e.f.base.manager) ~db ()
          in
          C.configure ~db ~manager ~ctx:e.ctx;
          Lwt.finalize
            (fun () ->
              let run () =
                drain e.clock
                  (C.pass_with ~ctx:e.ctx ~http:e.http ~db ~manager e.order)
              in
              let* () = run () in
              check
                "new Session/handle full sweep cannot abandon still-running \
                 old handler"
                false
                (R.covered db e.order.id e.order.epoch);
              eq "active obligation survives" 1 (pending_all db);
              check "old handler still owns its work" true (Lwt.is_sleeping old);
              (* A duplicate may complete before the original. Its completion
                 must not remove the original live obligation. *)
              let a =
                Slack_standing_order_admission.start_handler db ~team:"T123"
                  ~app:"A123" ~channel:"C123"
                  ~identity:"message:1700000090.000321"
              in
              Slack_standing_order_admission.finish_handler db a ~success:true;
              let* () = run () in
              check "completed duplicate cannot release concurrent old handler"
                false
                (R.covered db e.order.id e.order.epoch);
              Lwt.wakeup_later u ();
              let* _ = drain e.clock old in
              let* () = run () in
              check "both handles see coverage after actual handler drains" true
                (R.covered db e.order.id e.order.epoch
                && R.covered e.f.db e.order.id e.order.epoch);
              Lwt.return_unit)
            (fun () ->
              Lwt.cancel old;
              let* () = C.stop () in
              ignore (Sqlite3.db_close db);
              Lwt.return_unit)))

let conservative_obligation kind () =
  with_env (fun e ->
      let* () = pass e in
      let ts =
        if kind = "outside" then "1699999990.000321" else "1700000090.000321"
      in
      let text = if kind = "pause" then "pause SO-1" else "ordinary metadata" in
      let body = ordinary_body ~ts ~text ("Ev-held-" ^ kind) in
      !Slack_thread_ingress.on_ingress_admit ~body;
      if kind = "unknown" then
        Sql.exec e.f.db "DELETE FROM slack_order_ingress_delivery" [];
      !Slack_socket.on_gap ~cause:"handler_failure";
      let* () = pass e in
      eq "unresolved metadata remains pending" 1 (pending_all e.f.db);
      check "complete replay retains conservative obligation" false (covered e);
      eq "no relay allocation" 0 (count e "files.getUploadURLExternal");
      if kind = "unknown" then
        with_transport e (fun () ->
            let* _ =
              Slack.handle_event
                ~config:
                  (Option.get
                     (Session.get_config e.f.base.manager).channels.slack)
                ~session_manager:e.f.base.manager body
            in
            let* () = pass e in
            check "real application can settle legacy unknown metadata" true
              (covered e);
            Lwt.return_unit)
      else Lwt.return_unit)

let handler_failure () =
  with_env (fun e ->
      with_transport e (fun () ->
          let* () = pass e in
          let entered = ref false in
          let failing_handler ?thread_run:_ ?thread_context:_ ~config:_
              ~session_manager:_ ?send_message_fn:_ ?run_update_command:_
              ?event_limiter:_ _ =
            entered := true;
            Lwt.fail_with "injected handler failure"
          in
          let* failed =
            Lwt.catch
              (fun () ->
                let* _ =
                  Slack_thread_ingress.handle_event
                    ~handle_message:failing_handler
                    ~config:
                      (Option.get
                         (Session.get_config e.f.base.manager).channels.slack)
                    ~session_manager:e.f.base.manager
                    (ordinary_body ~text:"/fixture <@UBOT>" "Ev-failed-handler")
                in
                Lwt.return_false)
              (function
                | Failure message when message = "injected handler failure" ->
                    Lwt.return_true
                | exn -> Lwt.fail exn)
          in
          check "actual injected handler was entered" true !entered;
          check "actual handler fails" true failed;
          eq "failed ordinary remains pending" 1 (pending_all e.f.db);
          let* () = pass e in
          eq "verified replay accounts abandoned handler" 0 (pending_all e.f.db);
          check "new full sweep restores admission after handler failure" true
            (covered e);
          Lwt.return_unit))

let stale_ticket () =
  with_env (fun e ->
      let* () = pass e in
      let body = ordinary_body "Ev-stale-handler-ticket" in
      !Slack_thread_ingress.on_ingress_admit ~body;
      let begin_ () = !Slack_thread_ingress.on_ingress_begin ~body in
      let old = begin_ () and newer = begin_ () in
      newer ~success:false;
      old ~success:true;
      eq "old success cannot apply a newer failed generation" 1
        (pending_all e.f.db);
      !Slack_socket.on_gap ~cause:"handler_failure";
      let* () = pass e in
      eq "full new replay settles the failed latest delivery" 0
        (pending_all e.f.db);
      check "no active handler remains after both completions" true (covered e);
      Lwt.return_unit)

let suite =
  suite
  @ [
      S.case
        "independent handle preserves old active handler across reconfigure"
        active_old_handler;
      S.case "unresolved exact pause remains independent of replay"
        (conservative_obligation "pause");
      S.case "out of bounded history ordinary remains held"
        (conservative_obligation "outside");
      S.case "unknown legacy delivery metadata remains held"
        (conservative_obligation "unknown");
      S.case "actual failed ordinary handler is recovered by complete replay"
        handler_failure;
      S.case "stale handler success cannot apply newer failed obligation"
        stale_ticket;
    ]

let process_owner () =
  with_env ~disk:true (fun e ->
      let* () = pass e in
      let body = ordinary_body "Ev-process-owner" in
      !Slack_thread_ingress.on_ingress_admit ~body;
      !Slack_socket.on_gap ~cause:"handler_failure";
      let ready_r, ready_w = Unix.pipe () and stop_r, stop_w = Unix.pipe () in
      match Unix.fork () with
      | 0 -> (
          Unix.close ready_r;
          Unix.close stop_w;
          try
            let db = Sqlite3.db_open (Option.get e.disk_path) in
            ignore
              (Slack_standing_order_admission.start_handler db ~team:"T123"
                 ~app:"A123" ~channel:"C123"
                 ~identity:"message:1700000090.000321");
            ignore (Sqlite3.db_close db);
            ignore (Unix.write ready_w (Bytes.of_string "R") 0 1);
            ignore (Unix.read stop_r (Bytes.create 1) 0 1);
            Unix._exit 0
          with _ -> Unix._exit 2)
      | pid ->
          Unix.close ready_w;
          Unix.close stop_r;
          let reaped = ref false in
          let reap () =
            if not !reaped then (
              ignore (Unix.write stop_w (Bytes.of_string "S") 0 1);
              let _, status = Unix.waitpid [] pid in
              reaped := true;
              check "fixture process exits normally" true
                (status = Unix.WEXITED 0))
          in
          Lwt.finalize
            (fun () ->
              let byte = Bytes.create 1 in
              eq "independent process published active ticket" 1
                (Unix.read ready_r byte 0 1);
              let* () = pass e in
              check "live independent process owner blocks completed replay"
                false (covered e);
              eq "active ordinary remains pending" 1 (pending_all e.f.db);
              reap ();
              let* () = pass e in
              eq "proven dead process handler is durably accounted" 0
                (pending_all e.f.db);
              check "full sweep after process exit restores coverage" true
                (covered e);
              Lwt.return_unit)
            (fun () ->
              reap ();
              Unix.close ready_r;
              Unix.close stop_w;
              Lwt.return_unit))

let unknown_owner () =
  with_env (fun e ->
      let* () = pass e in
      let body = ordinary_body "Ev-unknown-owner" in
      !Slack_thread_ingress.on_ingress_admit ~body;
      let finish = !Slack_thread_ingress.on_ingress_begin ~body in
      Fun.protect
        ~finally:(fun () -> finish ~success:false)
        (fun () ->
          Sql.exec e.f.db "UPDATE slack_order_ingress_handlers SET boot=''" []);
      (* Preserve a crash's active record, with insufficient owner proof. *)
      Sql.exec e.f.db "UPDATE slack_order_ingress_handlers SET state='active'"
        [];
      !Slack_socket.on_gap ~cause:"handler_failure";
      let* () = pass e in
      check "unknown process incarnation cannot be declared abandoned" false
        (covered e);
      eq "unknown owner remains pending" 1 (pending_all e.f.db);
      Lwt.return_unit)

let suite =
  suite
  @ [
      S.case "live and dead independent process owners are distinguished"
        process_owner;
      S.case "unknown process owner remains conservative" unknown_owner;
    ]

let unknown_lifecycle kind () =
  with_env (fun e ->
      let* () = pass e in
      let body = ordinary_body ("Ev-unknown-" ^ kind) in
      !Slack_thread_ingress.on_ingress_admit ~body;
      let table, known =
        if kind = "delivery" then ("slack_order_ingress_delivery", "reserved")
        else
          let finish = !Slack_thread_ingress.on_ingress_begin ~body in
          finish ~success:false;
          ("slack_order_ingress_handlers", "abandoned")
      in
      Sql.exec e.f.db ("UPDATE " ^ table ^ " SET state='future-state'") [];
      !Slack_socket.on_gap ~cause:"handler_failure";
      let* () = pass e in
      check "unknown lifecycle state cannot certify abandonment" false
        (covered e);
      eq "unknown lifecycle obligation remains pending" 1 (pending_all e.f.db);
      Sql.exec e.f.db ("UPDATE " ^ table ^ " SET state=?") [ Sql.text known ];
      let* () = pass e in
      check "known abandoned lifecycle permits full replay" true (covered e);
      Lwt.return_unit)

let suite =
  suite
  @ [
      S.case "unknown delivery lifecycle state remains conservative"
        (unknown_lifecycle "delivery");
      S.case "unknown handler lifecycle state remains conservative"
        (unknown_lifecycle "handler");
    ]

let live_preack_callback () =
  with_env ~socket:true (fun e ->
      with_transport e (fun () ->
          let socket = Test_slack_socket.make_socket () in
          let handlers = ref 0 in
          let handle body =
            incr handlers;
            let+ _ =
              Slack.handle_event
                ~config:
                  (Option.get
                     (Session.get_config e.f.base.manager).channels.slack)
                ~session_manager:e.f.base.manager body
            in
            ()
          in
          let connector = socket_loop e socket handle in
          Lwt.finalize
            (fun () ->
              let* () = settle 40 in
              let* () = socket.emit {|{"type":"hello"}|} in
              let* () = pass e in
              let* () = advance e.clock (e.clock.now +. 10.) in
              let body = ordinary_body "Ev-live-preack" in
              let ack, ack_u = Lwt.task () in
              socket.ack_pending := Some ack;
              let delivery =
                socket.emit
                  (Yojson.Safe.to_string
                     (obj
                        [
                          ("type", str "events_api");
                          ("envelope_id", str "live-preack");
                          ("payload", Yojson.Safe.from_string body);
                        ]))
              in
              check "actual preACK callback is live" true
                (Lwt.is_sleeping delivery);
              let config = Session.get_config e.f.base.manager in
              Session.update_config e.f.base.manager
                { config with log = config.log };
              let* () = pass e in
              check "config gap does not cancel actual ACK callback" true
                (Lwt.is_sleeping delivery);
              eq "ordinary handler still has not started" 0 !handlers;
              check "full new sweep cannot abandon a still-live ACK callback"
                false (covered e);
              eq "live preACK obligation remains pending" 1 (pending_all e.f.db);
              Lwt.wakeup_later ack_u ();
              let* () = delivery in
              let* () = settle 40 in
              let* () = pass e in
              eq "ACK hands off exactly one actual handler" 1 !handlers;
              eq "actual handler application settles obligation" 0
                (pending_all e.f.db);
              check "completed callback and handler permit new verification"
                true (covered e);
              Lwt.return_unit)
            (fun () -> Slack_standing_order_resolution_lease.cancel connector)))

let suite =
  suite
  @ [
      S.case "still-live preACK callback survives a new config generation"
        live_preack_callback;
    ]
