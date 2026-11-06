open Test_slack_standing_order_reconcile_support
open Lwt.Syntax
module Store = Slack_standing_order_store

let renewed_owner () =
  with_env ~disk:true (fun e ->
      let other = Sqlite3.db_open (Option.get e.disk_path) in
      Lwt.finalize
        (fun () ->
          let start = e.clock.now in
          Slack_read_limiter.note_retry_after (Option.get e.ctx.limiter)
            ~key:"T123:A123:auth.test" 200.;
          let task =
            C.pass_with ~ctx:e.ctx ~http:e.http ~db:e.f.db
              ~manager:e.f.base.manager e.order
          in
          let* () = advance e.clock (start +. 150.) in
          check "pass remains in real limiter beyond original 120-second lease"
            true (Lwt.is_sleeping task);
          eq "no hung HTTP used to simulate lease wait" 0
            (List.length !(e.calls));
          check "independent handle sees positive renewed lease" true
            (Sql.int_query other
               "SELECT count(*) FROM slack_standing_order_recovery_passes \
                WHERE until>?"
               [ Sqlite3.Data.FLOAT e.clock.now ]
            = 1);
          check "second handle cannot take positively renewed owner" true
            (R.claim other ~order_id:e.order.id ~owner_id:"competitor"
               ~now:e.clock.now
            = None);
          eq "scan never holds fire/channel relay lease" 0
            (Sql.int_query other
               "SELECT count(*) FROM slack_standing_order_fires WHERE \
                claim_owner IS NOT NULL"
               []);
          let* () = drain e.clock task in
          check "renewed pass completes real coverage" true (covered e);
          Lwt.return_unit)
        (fun () ->
          ignore (Sqlite3.db_close other);
          Lwt.return_unit))

let slice_takeover () =
  with_env ~disk:true (fun e ->
      let other = Sqlite3.db_open (Option.get e.disk_path) in
      let stolen = ref None and takeover = ref Lwt.return_unit in
      let pages = ref 0 in
      let previous = !(e.answer) in
      e.roots :=
        List.init 21 (fun i ->
            obj
              [
                ("ts", str (Printf.sprintf "17000000%02d.000001" i));
                ("reply_count", `Int 1);
                ("latest_reply", str "1700000099.000001");
              ]);
      (e.answer :=
         fun m p ->
           if
             List.mem m [ "conversations.history"; "conversations.replies" ]
             && P.get "limit" p = `Int 200
           then incr pages;
           previous m p);
      Sqlite3.create_fun0 e.f.db "take_slice" (fun () ->
          eq "yield after exactly twenty actual pages" 20 !pages;
          (takeover :=
             let* () = Lwt.pause () in
             stolen :=
               R.claim other ~order_id:e.order.id ~owner_id:"slice-successor"
                 ~now:e.clock.now;
             Lwt.return_unit);
          Sqlite3.Data.NULL);
      Sql.exec e.f.db
        "CREATE TEMP TRIGGER observe_slice AFTER UPDATE OF until ON \
         slack_standing_order_recovery_passes WHEN NEW.until<OLD.until AND \
         NEW.phase='pause' BEGIN SELECT take_slice(); END"
        [];
      Lwt.finalize
        (fun () ->
          let* () = pass e in
          let* () = !takeover in
          check "successor really claimed between slices" true (!stolen <> None);
          check "budget exhaustion never certified coverage" false (covered e);
          let before =
            Option.get (R.load_coverage other ~order_id:e.order.id)
          in
          let replies = count e "conversations.replies" in
          eq "nineteen replies persisted after one history page" 19 replies;
          Sql.exec e.f.db "DROP TRIGGER observe_slice" [];
          check "successor releases continuation" true
            (R.yield_owner other ~order_id:e.order.id
               ~owner:(Option.get !stolen) ~now:e.clock.now);
          let* () = pass e in
          let after = Option.get (R.load_coverage other ~order_id:e.order.id) in
          check "takeover resumes identical fixed window" true
            (before.oldest = after.oldest && before.latest = after.latest);
          eq "finished threads never reread across takeover" 21
            (count e "conversations.replies");
          check "remaining pages complete and verify" true (covered e);
          Lwt.return_unit)
        (fun () ->
          ignore (Sqlite3.exec e.f.db "DROP TRIGGER IF EXISTS observe_slice");
          ignore (Sqlite3.db_close other);
          Lwt.return_unit))

let unknown_tools e =
  let* () = pass e in
  let before = R.status_json e.f.db ~order_id:e.order.id in
  let* tool =
    F.invoke e.f "slack_standing_order_show" (obj [ ("order", str "SO-1") ])
  in
  let cli =
    Slack_standing_order_cli.cmd ~db:e.f.db
      ~config:(Session.get_config e.f.base.manager)
      [ "orders"; "show"; e.order.id ]
  in
  List.iter
    (fun output ->
      check "real tool and CLI expose first-window uncertainty" true
        (String_util.contains output "coverage_before_window_unknown"))
    [ tool; cli ];
  check "show/status preserves recovery metadata" true
    (before = R.status_json e.f.db ~order_id:e.order.id);
  eq "first install invents no gap report" 0
    (Sql.int_query e.f.db
       "SELECT count(*) FROM slack_standing_order_reports WHERE cause_key LIKE \
        'reconcile_gap:%'"
       []);
  Lwt.return_unit

let gap_report_reopen () =
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
      let window =
        Slack_standing_order_reconcile_scan.timestamp last
        ^ "-"
        ^ Slack_standing_order_reconcile_scan.timestamp (upper -. 172800.)
      in
      let key = "reconcile_gap:" ^ window in
      Sql.exec e.f.db
        "CREATE TEMP TRIGGER fail_gap_key BEFORE INSERT ON \
         slack_standing_order_reports WHEN NEW.cause_key LIKE \
         'reconcile_gap:%' BEGIN SELECT RAISE(ABORT,'gap-key-fault'); END"
        [];
      let* () = pass e in
      check "report-key failure prevents watermark advance" true
        ((Option.get (R.load_coverage e.f.db ~order_id:e.order.id)).last_success
       = Some last);
      Sql.exec e.f.db "DROP TRIGGER fail_gap_key" [];
      let prior = !(e.answer) and posts = ref [] in
      (e.answer :=
         fun m p ->
           if m = "chat.postMessage" then (
             posts := p :: !posts;
             Lwt.return (503, obj []))
           else prior m p);
      let* () = advance e.clock (e.clock.now +. 121.) in
      let* () = pass e in
      eq "exact fixed window persisted once" 1
        (Sql.int_query e.f.db
           "SELECT count(*) FROM slack_standing_order_reports WHERE cause_key=?"
           [ Sql.text key ]);
      check "fixed management report actually attempted" true (!posts <> []);
      List.iter
        (fun p ->
          Alcotest.(check string)
            "management channel pinned" e.order.management_channel_id
            (Slack_huddles.field "channel" p);
          Alcotest.(check string)
            "management thread pinned" e.order.management_thread_ts
            (Slack_huddles.field "thread_ts" p);
          check "fixed report includes exact window" true
            (String_util.contains (Slack_huddles.field "text" p) window))
        !posts;
      let* () = C.stop () in
      let reopened = Sqlite3.db_open (Option.get e.disk_path) in
      Lwt.finalize
        (fun () ->
          eq "pending/ambiguous report identity survives actual reopen" 1
            (Sql.int_query reopened
               "SELECT count(*) FROM slack_standing_order_reports WHERE \
                cause_key=? AND status<>'sent'"
               [ Sql.text key ]);
          let count = List.length !posts in
          let manager =
            Session.create
              ~config:(Session.get_config e.f.base.manager)
              ~db:reopened ()
          in
          C.configure ~db:reopened ~manager ~ctx:e.ctx;
          let* () =
            drain e.clock
              (C.pass_with ~ctx:e.ctx ~http:e.http ~db:reopened ~manager e.order)
          in
          eq "lost report response never blindly reposted" count
            (List.length !posts);
          eq "once-window key remains unique" 1
            (Sql.int_query reopened
               "SELECT count(*) FROM slack_standing_order_reports WHERE \
                cause_key=?"
               [ Sql.text key ]);
          Lwt.return_unit)
        (fun () ->
          let* () = C.stop () in
          ignore (Sqlite3.db_close reopened);
          Lwt.return_unit))

let suite =
  [
    S.case "renewal remains positive beyond 120 seconds of actual limiter wait"
      renewed_owner;
    S.case "page20 yields to another handle and resumes persisted threads"
      slice_takeover;
    case "first unknown coverage exposed through registered tool and CLI"
      unknown_tools;
    S.case "gap key precedes watermark and failed transport survives reopen"
      gap_report_reopen;
  ]

let ordinary_http_frontier e =
  with_transport e (fun () ->
      let* () = pass e in
      !Slack_socket.on_gap ~cause:"handler_failure";
      let gate, u = Lwt.task () and entered = ref false in
      let prior = !(e.answer) in
      (e.answer :=
         fun m p ->
           if m = "users.info" && Slack_huddles.field "user" p = "U123" then (
             entered := true;
             let* () = gate in
             prior m p)
           else prior m p);
      let message =
        obj
          [
            ("type", str "message");
            ("channel", str "C123");
            ("user", str "U123");
            ("ts", str "1700000090.000123");
            ("text", str "ordinary metadata");
          ]
      in
      let sc =
        Option.get (Session.get_config e.f.base.manager).channels.slack
      in
      let request json valid =
        let body = Yojson.Safe.to_string json in
        let timestamp = string_of_int (int_of_float (Unix.gettimeofday ())) in
        let signature =
          if valid then
            "v0="
            ^ Digestif.SHA256.(
                hmac_string ~key:sc.signing_secret
                  ("v0:" ^ timestamp ^ ":" ^ body)
                |> to_hex)
          else "v0=bad"
        in
        let headers =
          Cohttp.Header.of_list
            [
              ("x-slack-signature", signature);
              ("x-slack-request-timestamp", timestamp);
            ]
        in
        let req =
          Cohttp.Request.make ~meth:`POST ~headers
            (Uri.of_string ("http://localhost" ^ sc.events_path))
        in
        Http_server_webhooks.handle ~session_manager:e.f.base.manager
          ~auth_token:(Some "fixture") ~slack_config:sc `POST sc.events_path req
          (Cohttp_lwt.Body.of_string body)
      in
      let body =
        Yojson.Safe.from_string (event ~id:"Ev-ordinary-frontier" message)
      in
      let* _ = request body false in
      let* _ = request (body |> set "team_id" (str "TOTHER")) true in
      eq "invalid signature and foreign signed installation cannot reserve" 0
        (Sql.int_query e.f.db "SELECT count(*) FROM slack_order_ingress" []);
      let* _ = request body true in
      check "ordinary authenticated handler is actually blocked" true !entered;
      eq "ordinary unapplied identity has no fabricated pause flag" 1
        (Sql.int_query e.f.db
           "SELECT count(*) FROM slack_order_ingress WHERE applied=0 AND \
            pause=0"
           []);
      let* () = pass e in
      check "complete scan cannot verify through unapplied ordinary ingress"
        false (covered e);
      Lwt.wakeup_later u ();
      let* () = advance e.clock (e.clock.now +. 3.) in
      eq "ordinary handler drains applied frontier" 0
        (Sql.int_query e.f.db
           "SELECT count(*) FROM slack_order_ingress WHERE applied=0" []);
      let* () = pass e in
      check "applied ordinary ingress permits actual scan verification" true
        (covered e);
      eq "ordinary frontier never produces relay" 0
        (count e "files.completeUploadExternal");
      Lwt.return_unit)

let reload_while_waiting source rollback e =
  with_transport e (fun () ->
      let* () = pass e in
      let manager = e.f.base.manager in
      let old = Session.get_config manager in
      let next =
        S.with_slack old (fun sc ->
            { sc with bot_token = "xoxb-reloaded-fixture" })
      in
      let _, identity_before, _, _ =
        Slack_standing_order_rollout_store.read_anchor e.f.db
      in
      Slack_read_limiter.note_retry_after (Option.get e.ctx.limiter)
        ~key:"T123:A123:auth.test" 200.;
      let before = List.length !(e.calls) in
      let pending =
        C.pass_with ~ctx:e.ctx ~http:e.http ~db:e.f.db ~manager e.order
      in
      let current_config = ref old
      and sandbox = ref (Daemon_util.make_sandbox old) in
      let result =
        Daemon_config_reload.apply_runtime_config_reload ~source ~current_config
          ~session_manager:manager ~sandbox ~db:None ~tool_registry:None
          ~after_publish:(fun () ->
            if rollback then failwith "injected post-publication rollback")
          ~new_config:next ()
      in
      check "reload outcome matches real rollback" rollback
        (Result.is_error result);
      check "publisher closes coverage synchronously" false (covered e);
      check "config or rollback visible to manager" true
        (Session.get_config manager == if rollback then old else next);
      let* () = advance e.clock (e.clock.now +. 201.) in
      let* () = pending in
      eq "stale delayed grant emits no HTTP" before (List.length !(e.calls));
      let* () = pass e in
      check "reload requires and obtains fresh full coverage" true (covered e);
      let _, identity_after, _, _ =
        Slack_standing_order_rollout_store.read_anchor e.f.db
      in
      Alcotest.(check string)
        "token rotation/rollback never mints installation" identity_before
        identity_after;
      Lwt.return_unit)

let suite =
  suite
  @ [
      case "signed ordinary ingress blocks fixed-bound applied frontier"
        ordinary_http_frontier;
    ]
  @ List.concat_map
      (fun source ->
        List.map
          (fun rollback ->
            case
              (source
              ^
              if rollback then " actual rollback during limiter"
              else " actual reload during limiter")
              (reload_while_waiting source rollback))
          [ false; true ])
      [ "sighup"; "config_file_watch" ]

let retained_pickup_closed e =
  with_transport e (fun () ->
      let fire =
        S.fire e.f.db e.order ~room:"RRETAINED" ~state:T.Posted
          ~dispatched_at:(e.clock.now -. 1000.) ~relay_ts:"1699999100.000001" ()
      in
      Sql.exec e.f.db
        "UPDATE slack_standing_order_fires SET \
         root_ts=?,reposts=1,current_attempt=1,relay_ts='1699999200.000001',uploaded_file_ids='[\"FSECOND\"]' \
         WHERE id=?"
        Sql.[ text root_ts; text fire.id ];
      Sql.exec e.f.db
        "UPDATE slack_standing_order_dispatches SET \
         uploaded_file_ids='[\"FFIRST\"]' WHERE fire_id=?"
        [ Sql.text fire.id ];
      Sql.exec e.f.db
        "INSERT INTO \
         slack_standing_order_dispatches(fire_id,attempt_no,uploaded_file_ids,relay_ts,dispatched_at) \
         VALUES(?,1,'[\"FSECOND\"]','1699999200.000001',?)"
        [ Sql.text fire.id; Sqlite3.Data.FLOAT (e.clock.now -. 900.) ];
      let gate, _ = Lwt.task () in
      let failed = ref true and prior = !(e.answer) in
      (e.answer :=
         fun m p ->
           match m with
           | "auth.test"
             when match Lwt.get SO.call_policy with
                  | Some p -> p.SO.priority = Slack_read_limiter.Maintenance
                  | _ -> false ->
               gate
           | "reactions.get" ->
               Lwt.return
                 ( 200,
                   S.ok
                     [
                       ("message", obj [ ("reactions", `List []) ]);
                       ("file", obj [ ("reactions", `List []) ]);
                     ] )
           | "conversations.replies" ->
               if !failed then Lwt.return (503, obj [])
               else
                 Lwt.return
                   ( 200,
                     page
                       [
                         obj
                           [
                             ("ts", str "1700000099.000001");
                             ("thread_ts", Slack_huddles.get "ts" p);
                             ("user", str "UCLAUDE");
                             ("bot_id", str S.claude.bot_id);
                             ("app_id", str S.claude.app_id);
                             ("text", str "accepted");
                           ];
                       ] )
           | _ -> prior m p);
      C.start ~db:e.f.db ~manager:e.f.base.manager;
      E.start ~ctx:e.ctx ~db:e.f.db ~manager:e.f.base.manager ~http:e.http ();
      let* () = advance e.clock (e.clock.now +. 10.) in
      check "C/E run concurrently behind held first Grant" false (covered e);
      let saved = Option.get (Store.runtime e.f.db ~fire_id:fire.id) in
      check "partial retained pickup never becomes early no_pickup" true
        (saved.fire.state <> T.No_pickup);
      eq "partial pickup never counts terminal failure" 0
        (Slack_standing_order_pickup.failure_streak ~db:e.f.db
           ~order_id:e.order.id);
      failed := false;
      let* () = advance e.clock (e.clock.now +. 121.) in
      let saved = Option.get (Store.runtime e.f.db ~fire_id:fire.id) in
      check "existing receipt/pickup survives closed recovery admission" true
        (saved.fire.state = T.Picked_up);
      List.iter
        (fun ts ->
          check "both retained relay identities read through real transport"
            true
            (List.exists
               (fun (_, m, p) ->
                 m = "conversations.replies" && Slack_huddles.field "ts" p = ts)
               !(e.calls)))
        [ "1699999100.000001"; "1699999200.000001" ];
      List.iter
        (fun m -> eq ("closed recovery no " ^ m) 0 (count e m))
        [ "files.getUploadURLExternal"; "files.completeUploadExternal" ];
      eq "two retained attempts remain, never third" 2
        (List.length saved.attempts);
      print_histogram "retained-tracking-closed-recovery" !(e.calls);
      Lwt.return_unit)

let suite =
  suite
  @ [
      case "both retained attempts track while actual C/E recovery is closed"
        retained_pickup_closed;
    ]

let receipt_during_maintenance e =
  let* () = pass e in
  let fire =
    S.fire e.f.db e.order ~room:"RRECEIPT" ~state:T.Posted
      ~dispatched_at:e.clock.now ~relay_ts:"1700000100.000001" ()
  in
  e.roots :=
    List.init 100 (fun i ->
        obj
          [
            ("ts", str (Printf.sprintf "17000000%02d.000001" i));
            ("reply_count", `Int 1);
            ("latest_reply", str "1700000099.000001");
          ]);
  let prior = !(e.answer) and background = ref 0 in
  let held, u = Lwt.task () in
  (e.answer :=
     fun m p ->
       if
         m = "conversations.replies"
         && Slack_huddles.field "ts" p <> "1700000100.000001"
       then (
         incr background;
         if !background = 1 then
           let* () = held in
           prior m p
         else prior m p)
       else if m = "conversations.replies" then
         Lwt.return
           ( 200,
             page
               [
                 obj
                   [
                     ("ts", str "1700000101.000002");
                     ("thread_ts", str "1700000100.000001");
                     ("user", str S.claude.user_id);
                     ("bot_id", str S.claude.bot_id);
                     ("app_id", str S.claude.app_id);
                     ("text", str "accepted");
                   ];
               ] )
       else if m = "reactions.get" then
         Lwt.return
           ( 200,
             S.ok
               [
                 ("message", obj [ ("reactions", `List []) ]);
                 ("file", obj [ ("reactions", `List []) ]);
               ] )
       else prior m p);
  let* () = advance e.clock (e.clock.now +. 300.) in
  let start = e.clock.now in
  let scan =
    C.pass_with ~ctx:e.ctx ~http:e.http ~db:e.f.db ~manager:e.f.base.manager
      e.order
  in
  let* () = advance e.clock (start +. 2.) in
  eq "one actual background reply remains outstanding" 1 !background;
  E.start ~ctx:e.ctx ~db:e.f.db ~manager:e.f.base.manager ~http:e.http ();
  let* () = advance e.clock (start +. 15.) in
  check "receipt reads bypass outstanding background HTTP" true
    ((Option.get (Store.runtime e.f.db ~fire_id:fire.id)).fire.state
   = T.Picked_up);
  eq "no queued background fanout before receipt" 1 !background;
  check "receipt finishes while maintenance is unfinished" true
    (Lwt.is_sleeping scan);
  Lwt.wakeup_later u ();
  let* () = drain e.clock scan in
  check "actual maintenance still spans over 99 seconds" true
    (e.clock.now -. start > 99.);
  eq "no relay-producing completion from retained pickup" 0
    (count e "files.completeUploadExternal");
  Lwt.return_unit

let suite =
  suite
  @ [
      case "receipt HTTP preempts one outstanding long maintenance reply"
        receipt_during_maintenance;
    ]

let unfinished_retry e =
  let* () = pass e in
  let original = Option.get (R.load_coverage e.f.db ~order_id:e.order.id) in
  let parents =
    List.init 21 (fun i ->
        obj
          [
            ("ts", str (Printf.sprintf "17000000%02d.000001" i));
            ("reply_count", `Int 1);
            ("latest_reply", str "1700000099.000001");
          ])
  in
  e.roots := parents;
  let failing = ref true and prior = !(e.answer) in
  (e.answer :=
     fun m p ->
       if
         m = "conversations.replies"
         && Slack_huddles.field "ts" p = "1700000019.000001"
         && !failing
       then Lwt.return (503, obj [])
       else prior m p);
  let* () = advance e.clock (e.clock.now +. 180000.) in
  let fixed = e.clock.now in
  let* () = pass e in
  let failed = Option.get (R.load_coverage e.f.db ~order_id:e.order.id) in
  check "failed later unit preserves prior complete watermark" true
    (failed.last_success = original.last_success);
  Alcotest.(check string)
    "full bounded 48h lower bound"
    (Printf.sprintf "%.6f" (fixed -. 172800.))
    failed.oldest;
  Alcotest.(check string)
    "fixed pass upper bound"
    (Printf.sprintf "%.6f" fixed)
    failed.latest;
  List.iter
    (fun (_, m, p) ->
      if m = "conversations.history" && P.get "limit" p = `Int 200 then (
        Alcotest.(check string)
          "actual full-window HTTP oldest" failed.oldest
          (Slack_huddles.field "oldest" p);
        Alcotest.(check string)
          "actual full-window HTTP latest" failed.latest
          (Slack_huddles.field "latest" p)))
    (List.filter (fun (t, _, _) -> t >= fixed) !(e.calls));
  check "typed real reply failure remains inspectable" true
    (Sql.int_query e.f.db
       "SELECT count(*) FROM slack_order_scan_status WHERE \
        method='conversations.replies'"
       []
    = 1);
  let before = List.length !(e.calls) in
  let* () = advance e.clock (e.clock.now +. 5.) in
  let* () = pass e in
  eq "five-second tick cannot retry unfinished page" before
    (List.length !(e.calls));
  failing := false;
  let* () = advance e.clock (e.clock.now +. 121.) in
  let* () = pass e in
  let done_ = Option.get (R.load_coverage e.f.db ~order_id:e.order.id) in
  check "retry resumes exact original bounds" true
    (failed.oldest = done_.oldest && failed.latest = done_.latest);
  eq "twenty-one successful units plus one failed read, no completed rereads" 22
    (count e "conversations.replies");
  check "resumed work completes coverage" true (covered e);
  Lwt.return_unit

let fresh_reopen () =
  with_env ~disk:true (fun e ->
      let* () = pass e in
      let _, uuid, _, _ =
        Slack_standing_order_rollout_store.read_anchor e.f.db
      in
      let* () = C.stop () in
      check "fresh fixture DB actually closes" true (Sqlite3.db_close e.f.db);
      let db = Sqlite3.db_open (Option.get e.disk_path) in
      let manager =
        Session.create ~config:(Session.get_config e.f.base.manager) ~db ()
      in
      Lwt.finalize
        (fun () ->
          C.configure ~db ~manager ~ctx:e.ctx;
          check "fresh restart closes previous admission" false
            (R.covered db e.order.id e.order.epoch);
          let* () =
            drain e.clock
              (C.pass_with ~ctx:e.ctx ~http:e.http ~db ~manager e.order)
          in
          check "fresh proof reused only after ordinary full scan" true
            (R.covered db e.order.id e.order.epoch);
          check "fresh restart retains automatic origin" true
            (R.rollout_readiness db ~order_id:e.order.id
            = R.Ready R.Fresh_no_legacy_work);
          let _, after, _, _ =
            Slack_standing_order_rollout_store.read_anchor db
          in
          Alcotest.(check string) "fresh UUID never regenerated" uuid after;
          Lwt.return_unit)
        (fun () ->
          let* () = C.stop () in
          ignore (Sqlite3.db_close db);
          Lwt.return_unit))

let suite =
  suite
  @ [
      case
        "unfinished full-window page retries preserve bounds and completed \
         units"
        unfinished_retry;
      S.case "ordinary fresh proof survives actual DB close and reopen"
        fresh_reopen;
    ]
