open Test_slack_standing_order_reconcile_support
open Lwt.Syntax
module New = Test_slack_standing_order_reconcile_recovery

let keys db =
  Sql.query db
    "SELECT cause_key FROM slack_standing_order_reports WHERE cause_key LIKE \
     'reconcile_gap:%' ORDER BY cause_key"
    [] (fun s -> Sql_util.text_column s 0)

let intent_fault ~reopen () =
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
      let stamp = Slack_standing_order_reconcile_scan.timestamp in
      let original =
        "reconcile_gap:" ^ stamp last ^ "-" ^ stamp (upper -. 172800.)
      in
      Sql.exec e.f.db
        "CREATE TEMP TRIGGER rereview_fail_intent BEFORE INSERT ON \
         slack_order_gap_report_intents BEGIN SELECT \
         RAISE(ABORT,'rereview-intent-fault'); END"
        [];
      let* () = pass e in
      check "intent failure leaves watermark unchanged" true
        ((Option.get (R.load_coverage e.f.db ~order_id:e.order.id)).last_success
       = Some last);
      eq "intent INSERT really aborted" 0
        (Sql.int_query e.f.db
           "SELECT count(*) FROM slack_order_gap_report_intents" []);
      eq "no report inserted before intent" 0 (List.length (keys e.f.db));
      check "fault did not open admission" false (covered e);
      let failed = Option.get (R.load_coverage e.f.db ~order_id:e.order.id) in
      Alcotest.(check string)
        "failed pass retains fixed upper bound" (stamp upper) failed.latest;
      Alcotest.(check string)
        "failed pass remains swept, not complete" "swept"
        (Option.get (R.phase e.f.db e.order.id));
      let* db, manager =
        if reopen then (
          let* () = C.stop () in
          check "real DB closes before any successful intent retry" true
            (Sqlite3.db_close e.f.db);
          let* () = advance e.clock (e.clock.now +. 121.) in
          let db = Sqlite3.db_open (Option.get e.disk_path) in
          let manager =
            Session.create ~config:(Session.get_config e.f.base.manager) ~db ()
          in
          C.configure ~db ~manager ~ctx:e.ctx;
          check "startup cannot reuse old release authority" false
            (R.covered db e.order.id e.order.epoch);
          check "startup keeps actual old watermark" true
            ((Option.get (R.load_coverage db ~order_id:e.order.id)).last_success
           = Some last);
          Lwt.return (db, manager))
        else (
          Sql.exec e.f.db "DROP TRIGGER rereview_fail_intent" [];
          let* () = advance e.clock (e.clock.now +. 121.) in
          Lwt.return (e.f.db, e.f.base.manager))
      in
      Lwt.finalize
        (fun () ->
          let histories = count e "conversations.history" in
          let new_upper = e.clock.now in
          let* () =
            drain e.clock
              (C.pass_with ~ctx:e.ctx ~http:e.http ~db ~manager e.order)
          in
          let actual = keys db in
          let shifted =
            "reconcile_gap:" ^ stamp last ^ "-" ^ stamp (new_upper -. 172800.)
          in
          Printf.printf
            "REREVIEW_INTENT reopen=%b original=%s actual=%s covered=%b \
             last_success=%.6f new_history=%d\n\
             %!"
            reopen original (String.concat "," actual)
            (R.covered db e.order.id e.order.epoch)
            (Option.get
               (Option.get (R.load_coverage db ~order_id:e.order.id))
                 .last_success)
            (count e "conversations.history" - histories);
          check "new completed sweep permits admission" true
            (R.covered db e.order.id e.order.epoch);
          if reopen then (
            check "restart does a new full history sweep" true
              (count e "conversations.history" > histories);
            check
              "original exact key survives intent fault and immediate reopen"
              true (List.mem original actual);
            check "shifted substitution is forbidden" false
              (List.mem shifted actual);
            eq "original and separate contiguous tail materialized" 2
              (List.length actual);
            check "separate contiguous tail survives" true
              (List.mem
                 ("reconcile_gap:"
                 ^ stamp (upper -. 172800.)
                 ^ "-"
                 ^ stamp (new_upper -. 172800.))
                 actual))
          else
            check "same-process retry control retains exact original key" true
              (List.mem original actual);
          Lwt.return_unit)
        (fun () ->
          if reopen then (
            let* () = C.stop () in
            ignore (Sqlite3.db_close db);
            Lwt.return_unit)
          else Lwt.return_unit))

let live_owner boot_mode () =
  with_env (fun e ->
      with_transport e (fun () ->
          let* () = pass e in
          let gate, _ = Lwt.task () in
          let entered = ref false and prior = !(e.answer) in
          (e.answer :=
             fun m p ->
               if m = "users.info" && Slack_huddles.field "user" p = "U123" then (
                 entered := true;
                 let* () = gate in
                 prior m p)
               else prior m p);
          let work =
            Slack.handle_event
              ~config:
                (Option.get (Session.get_config e.f.base.manager).channels.slack)
              ~session_manager:e.f.base.manager
              (New.ordinary_body ("Ev-rereview-owner-" ^ boot_mode))
          in
          Lwt.finalize
            (fun () ->
              let* () = settle 40 in
              check "actual common handler entered blocking I/O" true !entered;
              check "actual ordinary handler remains live" true
                (Lwt.is_sleeping work);
              eq "exactly one actual active ticket" 1
                (Sql.int_query e.f.db
                   "SELECT count(*) FROM slack_order_ingress_handlers WHERE \
                    state='active'"
                   []);
              let original_boot = Slack_standing_order_admission.boot_id () in
              check "host supplied nonempty process proof" true
                (original_boot <> "");
              if boot_mode <> "valid" then
                Sql.exec e.f.db "UPDATE slack_order_ingress_handlers SET boot=?"
                  [
                    Sql.text
                      (if boot_mode = "empty" then ""
                       else if boot_mode = "contradictory" then
                         "11111111-1111-4111-8111-111111111111"
                       else "unknown-boot-metadata");
                  ];
              Session.update_config e.f.base.manager
                (Session.get_config e.f.base.manager);
              let* () = pass e in
              Printf.printf
                "REREVIEW_OWNER boot=%s handler_sleeping=%b pending=%d \
                 covered=%b abandoned=%d\n\
                 %!"
                boot_mode (Lwt.is_sleeping work) (New.pending_all e.f.db)
                (covered e)
                (Sql.int_query e.f.db
                   "SELECT count(*) FROM slack_order_ingress_handlers WHERE \
                    state='abandoned'"
                   []);
              check "probe did not complete actual handler" true
                (Lwt.is_sleeping work);
              check
                "live owner stays held for valid, empty and malformed metadata"
                false (covered e);
              eq "live ordinary obligation preserved" 1 (New.pending_all e.f.db);
              eq "live ticket never abandoned" 0
                (Sql.int_query e.f.db
                   "SELECT count(*) FROM slack_order_ingress_handlers WHERE \
                    state='abandoned'"
                   []);
              Sql.exec e.f.db "UPDATE slack_order_ingress_handlers SET boot=?"
                [ Sql.text original_boot ];
              Lwt.cancel work;
              let* canceled =
                Lwt.catch
                  (fun () ->
                    let* _ = work in
                    Lwt.return_false)
                  (function
                    | Lwt.Canceled -> Lwt.return_true | exn -> Lwt.fail exn)
              in
              check "actual handler cancellation propagated" true canceled;
              let* () = pass e in
              check
                "actual cancellation plus full replay positive control recovers"
                true (covered e);
              Lwt.return_unit)
            (fun () ->
              Lwt.cancel work;
              Lwt.return_unit)))

let suite =
  [
    S.case "intent INSERT failure same-process retains exact interval"
      (intent_fault ~reopen:false);
    S.case "intent INSERT failure immediate reopen retains exact interval"
      (intent_fault ~reopen:true);
    S.case "actual live handler valid owner stays held" (live_owner "valid");
    S.case "actual live handler empty owner stays held" (live_owner "empty");
    S.case "actual live handler malformed owner stays held"
      (live_owner "malformed");
  ]

module A = Slack_standing_order_admission

let representation_boundaries () =
  let boot = A.boot_id () and host = Unix.gethostname () in
  let pid = Unix.getpid () in
  let start = Option.get (Session_host_direct.proc_start_token pid) in
  let dead ?(host = host) ?(boot = boot) ?(pid = pid) ?(start = start) () =
    A.demonstrably_dead ~id:"absent-fix2-owner" ~host ~boot ~pid ~start
  in
  check "kernel boot representation accepted" true (A.valid_boot boot);
  check "kernel process token accepted" true (A.valid_start start);
  check "known local non-ticket is recoverable" true (dead ());
  let old_boot =
    if boot = "11111111-1111-4111-8111-111111111111" then
      "22222222-2222-4222-8222-222222222222"
    else "11111111-1111-4111-8111-111111111111"
  in
  check "valid old boot proves dead incarnation" true (dead ~boot:old_boot ());
  List.iter
    (fun boot ->
      check ("invalid boot rejected: " ^ boot) false (A.valid_boot boot);
      check "invalid boot never proves death" false (dead ~boot ()))
    [
      "";
      "unknown-boot-metadata";
      "11111111-1111-4111-8111-11111111111g";
      "111111111111-4111-8111-111111111111";
      "AAAAAAAA-AAAA-4AAA-8AAA-AAAAAAAAAAAA";
      boot ^ "\n";
    ];
  List.iter
    (fun start ->
      check ("invalid start rejected: " ^ start) false (A.valid_start start);
      check "invalid start blocks even valid old-boot comparison" false
        (dead ~boot:old_boot ~start ()))
    [
      "";
      "unknown-start";
      "-1";
      "+1";
      " 1";
      "1\n";
      "01";
      "1.0";
      "18446744073709551616";
      "999999999999999999999";
    ];
  List.iter
    (fun start -> check "uint64 boundaries accepted" true (A.valid_start start))
    [ "0"; "1"; "18446744073709551615" ];
  List.iter
    (fun pid ->
      check "nonpositive pid blocks old boot" false
        (dead ~boot:old_boot ~pid ()))
    [ 0; -1; min_int; max_int; 2147483648 ];
  check "foreign host cannot prove local death" false
    (dead ~host:(host ^ "-other") ());
  List.iter
    (fun value -> eq "non-integer persisted pid rejected" 0 (A.owner_pid value))
    Sqlite3.Data.
      [
        TEXT "123";
        TEXT "bad";
        FLOAT 1.5;
        NULL;
        INT 0L;
        INT (-1L);
        INT Int64.max_int;
        INT 2147483648L;
      ];
  eq "positive integer pid retained" pid (A.owner_pid (Sql.int pid));
  let parent = Unix.getppid () in
  let parent_start = Option.get (Session_host_direct.proc_start_token parent) in
  check "actual live external process is held" false
    (dead ~pid:parent ~start:parent_start ());
  let different_start = if parent_start = "0" then "1" else "0" in
  check "valid PID reuse token proves prior incarnation dead" true
    (dead ~pid:parent ~start:different_start ());
  Lwt.return_unit

let coverage e = Option.get (R.load_coverage e.f.db ~order_id:e.order.id)

let intent_count db =
  Sql.int_query db "SELECT count(*) FROM slack_order_gap_report_intents" []

let fault db =
  Sql.exec db
    "CREATE TRIGGER fix2_preserve_fault BEFORE INSERT ON \
     slack_order_gap_report_intents BEGIN SELECT \
     RAISE(ABORT,'fix2-preservation-fault'); END"
    []

let unfault db = Sql.exec db "DROP TRIGGER fix2_preserve_fault" []

let failed_window e =
  let* () = pass e in
  let last = Option.get (coverage e).last_success in
  let* () = advance e.clock (e.clock.now +. 172900.) in
  !Slack_socket.on_gap ~cause:"handler_failure";
  fault e.f.db;
  let* () = pass e in
  let c = coverage e in
  check "actual intent INSERT aborted with no replacement" true
    (intent_count e.f.db = 0);
  check "incomplete fixed window remains swept" true
    (R.phase e.f.db e.order.id = Some "swept");
  check "failed intent leaves old watermark" true (c.last_success = Some last);
  let stamp = Slack_standing_order_reconcile_scan.timestamp in
  Lwt.return
    ( c,
      "reconcile_gap:" ^ stamp last ^ "-"
      ^ stamp (float_of_string c.latest -. 172800.) )

let retained db order_id c =
  let current = Option.get (R.load_coverage db ~order_id) in
  check "original bounds and actual watermark retained" true (current = c);
  eq "preservation fault inserts no intent" 0 (intent_count db)

let startup_preservation_failure () =
  with_env ~disk:true (fun e ->
      let* c, original = failed_window e in
      let* () = C.stop () in
      check "disk DB closes before retry" true (Sqlite3.db_close e.f.db);
      let* () = advance e.clock (e.clock.now +. 121.) in
      let db = Sqlite3.db_open (Option.get e.disk_path) in
      let manager =
        Session.create ~config:(Session.get_config e.f.base.manager) ~db ()
      in
      Lwt.finalize
        (fun () ->
          let refused () =
            let failed =
              try
                R.recover_startup db ~now:e.clock.now;
                false
              with
              | Failure message
              when Test_helpers.string_contains message "CONSTRAINT"
                   && Test_helpers.string_contains message
                        "slack_order_gap_report_intents"
              ->
                true
            in
            check "startup refuses actual preservation write failure" true
              failed;
            retained db e.order.id c;
            check "reset rolls back to original incomplete phase" true
              (R.phase db e.order.id = Some "swept");
            check "startup failure cannot release admission" false
              (R.covered db e.order.id e.order.epoch)
          in
          refused ();
          refused ();
          unfault db;
          let histories = count e "conversations.history" in
          C.configure ~db ~manager ~ctx:e.ctx;
          eq "startup reconstructs original intent before reset" 1
            (intent_count db);
          check "reconstruction cannot retain stale release authority" false
            (R.covered db e.order.id e.order.epoch);
          eq "reconstruction has no network effect" histories
            (count e "conversations.history");
          let* () =
            drain e.clock
              (C.pass_with ~ctx:e.ctx ~http:e.http ~db ~manager e.order)
          in
          check "retry completes full new sweep" true
            (count e "conversations.history" > histories);
          check "original exact report survives repeated preservation refusal"
            true
            (List.mem original (keys db));
          eq "separate tail also materializes" 2 (List.length (keys db));
          check "fresh sweep after successful preservation reopens" true
            (R.covered db e.order.id e.order.epoch);
          Lwt.return_unit)
        (fun () ->
          let* () = C.stop () in
          ignore (Sqlite3.db_close db);
          Lwt.return_unit))

let reset_preservation mode () =
  with_env (fun e ->
      let* c, original = failed_window e in
      let* () = advance e.clock (e.clock.now +. 121.) in
      if mode = "epoch" then (
        Sql.exec e.f.db
          "UPDATE slack_standing_orders SET epoch=epoch+1 WHERE id=?"
          [ Sql.text e.order.id ];
        let claim () =
          R.claim e.f.db ~order_id:e.order.id ~owner_id:"fix2-new-epoch"
            ~now:e.clock.now
        in
        check "epoch reset refuses when old interval cannot persist" true
          (claim () = None);
        retained e.f.db e.order.id c;
        check "epoch failure retains swept phase" true
          (R.phase e.f.db e.order.id = Some "swept");
        check "changed epoch is never covered" false
          (R.covered e.f.db e.order.id (e.order.epoch + 1));
        unfault e.f.db;
        let owner = Option.get (claim ()) in
        check "epoch retry resets only after preserving interval" true
          (R.phase e.f.db e.order.id = Some "idle");
        eq "epoch retry preserves original intent" 1 (intent_count e.f.db);
        let order = { e.order with epoch = e.order.epoch + 1 } in
        let gap = Option.get (C.connected_gap e.f.db order) in
        check "new epoch starts new full sweep" true
          (R.begin_scan e.f.db ~order ~owner ~mode:R.Gap_recovery ~gap
             ~oldest:c.oldest
             ~latest:(Slack_standing_order_reconcile_scan.timestamp e.clock.now)
             ~now:e.clock.now);
        check "new epoch remains unverified" false
          (R.covered e.f.db order.id order.epoch);
        Lwt.return_unit)
      else (
        if mode = "cursor" then (
          let owner =
            Option.get
              (R.claim e.f.db ~order_id:e.order.id ~owner_id:"fix2-cursor"
                 ~now:e.clock.now)
          in
          check "invalid cursor rejected" false
            (R.save_page e.f.db ~order_id:e.order.id ~owner
               ~coverage:{ c with history_cursor = String.make 1025 'x' }
               ~candidates:[] ~now:e.clock.now);
          retained e.f.db e.order.id c;
          check "cursor reset replays pause without losing fixed bounds" true
            (R.phase e.f.db e.order.id = Some "pause");
          ignore
            (R.yield_owner e.f.db ~order_id:e.order.id ~owner ~now:e.clock.now));
        Session.update_config e.f.base.manager
          (Session.get_config e.f.base.manager);
        let histories = count e "conversations.history" in
        let* () = pass e in
        retained e.f.db e.order.id c;
        eq "failed new-gap preservation precedes new network sweep" histories
          (count e "conversations.history");
        check "new gap cannot advance without preserving old bounds" false
          (covered e);
        unfault e.f.db;
        let retry_at =
          List.hd
            (Sql.query e.f.db
               "SELECT retry_at FROM slack_order_reconcile_retry WHERE \
                order_id=?"
               [ Sql.text e.order.id ]
               (fun s -> Option.get (Sql.opt_float_column s 0)))
        in
        check "preservation refusal honors production retry backoff" true
          (retry_at > e.clock.now);
        let* () = advance e.clock retry_at in
        let* () = pass e in
        check "new generation performs mandatory sweep" true
          (count e "conversations.history" > histories);
        check "new generation materializes exact original report" true
          (List.mem original (keys e.f.db));
        eq "new gap reports separate contiguous tail" 2
          (List.length (keys e.f.db));
        check "new gap opens only after successful retry and sweep" true
          (covered e);
        Lwt.return_unit))

let suite =
  suite
  @ [
      S.case "live ticket blocks contradictory valid old-boot metadata"
        (live_owner "contradictory");
      S.case "owner representation boundaries and legitimate death controls"
        representation_boundaries;
      S.case
        "startup preservation failure retains bounds and denies until retry"
        startup_preservation_failure;
      S.case "new-gap preservation failure retains original bounds"
        (reset_preservation "gap");
      S.case "epoch reset atomically preserves original interval"
        (reset_preservation "epoch");
      S.case "cursor reset retains bounds through failed new-gap preservation"
        (reset_preservation "cursor");
    ]

let persisted_malformed_owner field () =
  with_env (fun e ->
      let* () = pass e in
      let body = New.ordinary_body ("Ev-malformed-" ^ field) in
      !Slack_thread_ingress.on_ingress_admit ~body;
      let finish = !Slack_thread_ingress.on_ingress_begin ~body in
      finish ~success:false;
      (* Simulate an interrupted owner without a positive process-local ticket.
       Invalid start/PID must block even an otherwise valid old-boot proof. *)
      Sql.exec e.f.db
        "UPDATE slack_order_ingress_handlers SET \
         state='active',boot='11111111-1111-4111-8111-111111111111'"
        [];
      let assignment =
        match field with
        | "start" -> "start='unknown-start-metadata'"
        | "pid" -> "pid='unknown-pid-metadata'"
        | "pid-overflow" -> "pid=2147483648"
        | _ -> assert false
      in
      Sql.exec e.f.db
        ("UPDATE slack_order_ingress_handlers SET " ^ assignment)
        [];
      Session.update_config e.f.base.manager
        (Session.get_config e.f.base.manager);
      let* () = pass e in
      eq "persisted malformed incarnation stays pending after full sweep" 1
        (New.pending_all e.f.db);
      check "malformed proof never opens admission" false (covered e);
      eq "malformed row was not abandoned" 0
        (Sql.int_query e.f.db
           "SELECT count(*) FROM slack_order_ingress_handlers WHERE \
            state='abandoned'"
           []);
      Sql.exec e.f.db
        "UPDATE slack_order_ingress_handlers SET boot=?,pid=?,start=?"
        Sql.
          [
            text (A.boot_id ());
            int (Unix.getpid ());
            text
              (Option.get
                 (Session_host_direct.proc_start_token (Unix.getpid ())));
          ];
      let* () = pass e in
      check "valid absent local ticket permits full replay" true (covered e);
      Lwt.return_unit)

let suite =
  suite
  @ [
      S.case "persisted malformed start blocks old-boot death comparison"
        (persisted_malformed_owner "start");
      S.case "persisted malformed pid remains conservative"
        (persisted_malformed_owner "pid");
      S.case "persisted overflowing pid cannot alias a process"
        (persisted_malformed_owner "pid-overflow");
    ]
