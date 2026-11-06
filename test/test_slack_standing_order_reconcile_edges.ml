open Test_slack_standing_order_reconcile_support
open Lwt.Syntax

let suite = []

let hung_scan e =
  let* () = pass e in
  let prior = !(e.answer) and canceled = ref false in
  let blocked, _ = Lwt.task () in
  Lwt.on_cancel blocked (fun () -> canceled := true);
  (e.answer :=
     fun m p ->
       if m = "conversations.history" && P.get "limit" p = `Int 200 then blocked
       else prior m p);
  let previous = Option.get (R.load_coverage e.f.db ~order_id:e.order.id) in
  let* () = advance e.clock (e.clock.now +. 300.) in
  let work =
    C.pass_with ~ctx:e.ctx ~http:e.http ~db:e.f.db ~manager:e.f.base.manager
      e.order
  in
  let* () = advance e.clock (e.clock.now +. 31.) in
  check "hung read canceled by injected deadline" true !canceled;
  let* () = settle 200 in
  check "scan returned after timeout" false (Lwt.is_sleeping work);
  let* () = work in
  check "partial maintenance does not advance last success" true
    ((Option.get (R.load_coverage e.f.db ~order_id:e.order.id)).last_success
   = previous.last_success);
  check "continuously covered maintenance timeout keeps live continuity" true
    (covered e);
  Lwt.return_unit

let config_during_wait e =
  let* () = pass e in
  let limiter = Option.get e.ctx.Ctx.limiter in
  Slack_read_limiter.note_retry_after limiter ~key:"T123:A123:auth.test" 200.;
  let old = Session.get_config e.f.base.manager in
  let before = List.length !(e.calls) in
  let work =
    C.pass_with ~ctx:e.ctx ~http:e.http ~db:e.f.db ~manager:e.f.base.manager
      e.order
  in
  let* () = settle 30 in
  let rotated =
    S.with_slack old (fun sc -> { sc with bot_token = "xoxb-rotated-fixture" })
  in
  Session.update_config e.f.base.manager rotated;
  check "rotation closes gate synchronously while waiting" false (covered e);
  observe e
    (root
    |> set "channel" (str "C123")
    |> set "type" (str "message")
    |> set "user" (str "USLACKBOT"));
  let* () = advance e.clock (e.clock.now +. 201.) in
  let* () = work in
  eq "old delayed grant cannot dispatch HTTP" before (List.length !(e.calls));
  let* () = pass e in
  check "matching rotated installation can reverify" true (covered e);
  check "ready hint/root survives config publication" true
    (Sql.int_query e.f.db
       "SELECT count(*) FROM slack_order_root_schedule WHERE root=?"
       [ Sql.text root_ts ]
    > 0);
  Lwt.return_unit

let missed_pause e =
  let* () = pass e in
  let old = Option.get (R.load_coverage e.f.db ~order_id:e.order.id) in
  e.roots :=
    [
      obj
        [
          ("ts", str "1700000080.000001");
          ("user", str "U456");
          ("text", str "pause SO-1");
        ];
    ];
  let* () = advance e.clock (e.clock.now +. 300.) in
  let* () = pass e in
  check "contradictory missed pause closes immediately" false (covered e);
  check "contradictory pass cannot advance watermark" true
    ((Option.get (R.load_coverage e.f.db ~order_id:e.order.id)).last_success
   = old.last_success);
  let* () = pass e in
  Alcotest.check S.state_t "new full sweep applies missed pause" T.Paused
    (Option.get (Slack_standing_order_store.find e.f.db e.order.id)).state;
  Lwt.return_unit

let suite =
  suite
  @ [
      case "hung maintenance read times out and preserves incomplete watermark"
        hung_scan;
      case "token publication while limiter waits invalidates and retains hint"
        config_during_wait;
      case "missed pause converts healthy maintenance into a real gap"
        missed_pause;
    ]

let shared_owner () =
  with_env ~disk:true (fun e ->
      let other = Sqlite3.db_open (Option.get e.disk_path) in
      Lwt.finalize
        (fun () ->
          let prior = !(e.answer) and canceled = ref false in
          let blocked, _ = Lwt.task () in
          Lwt.on_cancel blocked (fun () -> canceled := true);
          (e.answer :=
             fun m p ->
               if m = "conversations.history" then blocked else prior m p);
          let running =
            C.pass_with ~ctx:e.ctx ~http:e.http ~db:e.f.db
              ~manager:e.f.base.manager e.order
          in
          let* () = settle 30 in
          let before = List.length !(e.calls) in
          let manager =
            Session.create
              ~config:(Session.get_config e.f.base.manager)
              ~db:other ()
          in
          let* () =
            C.pass_with ~ctx:e.ctx ~http:e.http ~db:other ~manager e.order
          in
          eq "independent handle cannot scan through live owner" before
            (List.length !(e.calls));
          Sql.exec other
            "UPDATE slack_standing_order_recovery_passes SET until=? WHERE \
             order_id=?"
            Sql.[ Sqlite3.Data.FLOAT e.clock.now; text e.order.id ];
          let* () = advance e.clock (e.clock.now +. 30.) in
          let* () = running in
          check "expired owner cancels its HTTP wait" true !canceled;
          check "expired owner leaves admission closed" false (covered e);
          eq "expired owner cannot advance success" 0
            (Sql.int_query other
               "SELECT count(*) FROM slack_standing_order_recovery_passes \
                WHERE last_success IS NOT NULL"
               []);
          Lwt.return_unit)
        (fun () ->
          ignore (Sqlite3.db_close other);
          Lwt.return_unit))

let first_unknown_gap e =
  let* () = pass e in
  let status =
    R.status_json e.f.db ~order_id:e.order.id |> Yojson.Safe.to_string
  in
  check "first bounded coverage exposes prior uncertainty" true
    (String_util.contains status "coverage_before_window_unknown");
  eq "no fabricated exact downtime report" 0
    (Sql.int_query e.f.db
       "SELECT count(*) FROM slack_standing_order_reports WHERE cause_key LIKE \
        'reconcile_gap:%'"
       []);
  let last =
    Option.get
      (Option.get (R.load_coverage e.f.db ~order_id:e.order.id)).last_success
  in
  let* () = advance e.clock (e.clock.now +. 172900.) in
  !Slack_socket.on_gap ~cause:"handler_failure";
  let prior = !(e.answer) in
  (e.answer :=
     fun m p ->
       if m = "chat.postMessage" then Lwt.return (503, obj []) else prior m p);
  let upper = e.clock.now in
  let* () = pass e in
  let window =
    Slack_standing_order_reconcile_scan.timestamp last
    ^ "-"
    ^ Slack_standing_order_reconcile_scan.timestamp (upper -. 172800.)
  in
  check "exact half-open uncovered ts window persisted before advancing" true
    (Sql.int_query e.f.db
       "SELECT count(*) FROM slack_standing_order_reports WHERE cause_key=?"
       [ Sql.text ("reconcile_gap:" ^ window) ]
    = 1);
  check "failed report remains inspectable" true
    (List.exists
       (fun (_, status) -> status <> "sent")
       (Slack_standing_order_report.statuses ~db:e.f.db ~order_id:e.order.id));
  Lwt.return_unit

let suite =
  suite
  @ [
      S.case "renewal loss and concurrent independent DB handle stay closed"
        shared_owner;
      case "bounded first coverage and exact durable downtime report"
        first_unknown_gap;
    ]

let supervised_service ?(failure = Failure "injected service tick failure") e =
  with_transport e (fun () ->
      let* () = C.stop () in
      let fault = ref false
      and recovering = ref false
      and closed_before_backoff = ref false in
      let wait ~seconds =
        if !recovering then (
          closed_before_backoff := not (covered e);
          recovering := false);
        let* () = wait e.clock ~seconds in
        if seconds = 5. && !fault then (
          fault := false;
          recovering := true;
          Lwt.fail failure)
        else Lwt.return_unit
      in
      let ctx = { e.ctx with Ctx.wait } in
      C.configure ~db:e.f.db ~manager:e.f.base.manager ~ctx;
      C.start ~db:e.f.db ~manager:e.f.base.manager;
      let* () = advance e.clock (e.clock.now +. 3.) in
      check "service fixture starts continuously covered" true (covered e);
      fault := true;
      let* () = advance e.clock (e.clock.now +. 3.) in
      check "service failure closes synchronously before retry wait" true
        !closed_before_backoff;
      let* () = advance e.clock (e.clock.now +. 10.) in
      check "supervision automatically revalidates coverage" true (covered e);
      Lwt.return_unit)

let suite =
  suite
  @ [
      case "service supervision closes before backoff and restarts"
        (supervised_service ~failure:(Failure "injected service tick failure"));
      case "unexpected service cancellation closes and restarts"
        (supervised_service ~failure:Lwt.Canceled);
    ]

let published_refusal change e =
  let* () = pass e in
  check "policy control is covered before publication" true (covered e);
  let old = Session.get_config e.f.base.manager in
  Session.update_config e.f.base.manager (change old);
  check "policy publication synchronously closes admission" false (covered e);
  let before = List.length !(e.calls) in
  let* () = pass e in
  eq "local refusal sends zero actual HTTP" before (List.length !(e.calls));
  Lwt.return_unit

let private_policy e =
  let* () = pass e in
  let before = List.length !(e.calls) and prior = !(e.answer) in
  (e.answer :=
     fun m p ->
       if m = "conversations.info" then
         let+ status, json = prior m p in
         ( status,
           json
           |> set "channel"
                (Slack_huddles.get "channel" json
                |> set "is_private" (`Bool true)) )
       else prior m p);
  Session.update_config e.f.base.manager
    (S.with_slack (Session.get_config e.f.base.manager) (fun sc ->
         { sc with private_channel_policy = Pc_deny }));
  check "private policy publication closes immediately" false (covered e);
  let* () = pass e in
  check "fresh remote posture is consulted before refusal" true
    (List.length !(e.calls) > before);
  eq "private refusal produces zero content reads" 0 (count e "files.info");
  eq "private refusal produces zero relay completions" 0
    (count e "files.completeUploadExternal");
  Alcotest.check S.state_t "private refusal suspends order" T.Suspended
    (Option.get (Slack_standing_order_store.find e.f.db e.order.id)).state;
  Lwt.return_unit

let suite =
  suite
  @ List.map
      (fun (name, change) -> case name (published_refusal change))
      [
        ( "reload empty target policy independently denies",
          fun cfg ->
            S.with_slack cfg (fun sc ->
                {
                  sc with
                  standing_orders =
                    { sc.standing_orders with allowed_targets = [] };
                }) );
        ( "reload grantor allowlist independently denies",
          fun cfg ->
            S.with_slack cfg (fun sc -> { sc with allow_users = [ "UOTHER" ] })
        );
        ( "reload channel allowlist independently denies",
          fun cfg ->
            S.with_slack cfg (fun sc -> { sc with allow_channels = [] }) );
        ( "reload egress policy independently denies",
          fun (cfg : Runtime_config.t) ->
            { cfg with egress = { cfg.egress with default_allowlist = [] } } );
        ( "reload capability policy independently denies",
          fun (cfg : Runtime_config.t) ->
            { cfg with security = { cfg.security with tools_enabled = false } }
        );
        ( "reload inactive credential independently denies",
          fun cfg -> S.with_slack cfg (fun sc -> { sc with bot_token = "" }) );
      ]
  @ [
      case "reload private-channel policy checks current posture" private_policy;
    ]

let first_grant_blocks_running_complete e =
  let module Store = Slack_standing_order_store in
  let module Dispatch = Slack_standing_order_dispatch in
  let cfg = Session.get_config e.f.base.manager in
  Session.update_config e.f.base.manager
    {
      cfg with
      egress =
        {
          cfg.egress with
          default_allowlist =
            {
              Runtime_config.host = "files.slack.com";
              path = Some "/upload/*";
              method_ = Some "POST";
              action = Allow;
              log_policy = No_log;
            }
            :: cfg.egress.default_allowlist;
        };
    };
  let* () = pass e in
  let fire =
    S.fire e.f.db e.order ~room:"R123" ~date_start:"1700000010"
      ~state:T.Uploading ()
  in
  Sql.exec e.f.db
    "UPDATE slack_standing_order_fires SET root_ts=?,canvas_file_id='FCANVAS', \
     notes_ready_ts='1700000060.000100' WHERE id=?"
    Sql.[ text root_ts; text fire.id ];
  R.record_root e.f.db ~order:e.order ~root_ts ~room_id:"R123"
    ~date_start:"1700000010" ~fire_id:(Some fire.id);
  let payload : Slack_standing_order_provenance.verified =
    {
      root_ts;
      thread_ts = root_ts;
      room_id = "R123";
      date_start = 1700000010;
      date_end = Some 1700000030;
      notes_ready_ts = "1700000060.000100";
      canvas_id = "FCANVAS";
      notes_md = "fixture notes";
      transcript = None;
      transcript_unavailable = false;
    }
  in
  let blocked, _ = Lwt.task () in
  let holding = ref false and entered = ref false and uploaded = ref false in
  let previous = !(e.answer) in
  (e.answer :=
     fun method_ params ->
       match method_ with
       | "auth.test" when !holding ->
           entered := true;
           blocked
       | "files.getUploadURLExternal" ->
           Lwt.return
             ( 200,
               S.ok
                 [
                   ("file_id", str "FOWN");
                   ("upload_url", str "https://files.slack.com/upload/fixture");
                 ] )
       | _ -> previous method_ params);
  let raw_http _ =
    uploaded := true;
    Lwt.return
      Slack_standing_order_upload.{ status = 200; headers = []; body = "" }
  in
  let until = e.clock.now +. 12. in
  Slack_read_limiter.note_retry_after
    (Option.get e.ctx.Ctx.limiter)
    ~key:"T123:A123:files.completeUploadExternal" 12.;
  let lease =
    Store.claim e.f.db ~fire_id:fire.id ~owner:"existing-live-job"
      ~now:e.clock.now
    |> Result.get_ok |> Option.get
  in
  let delivery =
    Dispatch.deliver ~ctx:e.ctx ~db:e.f.db ~manager:e.f.base.manager
      ~http:e.http ~upload_http:raw_http ~lease
      (Option.get (Store.runtime e.f.db ~fire_id:fire.id))
      payload
  in
  let* () = advance e.clock (until -. 2.) in
  check "existing job uploaded before gap" true !uploaded;
  check "existing completion is waiting in real limiter" true
    (Lwt.is_sleeping delivery);
  !Slack_socket.on_gap ~cause:"hello";
  !Slack_socket.on_connected ();
  check "hello closes before first Grant" false (covered e);
  holding := true;
  let reconcile =
    C.pass_with ~ctx:e.ctx ~http:e.http ~db:e.f.db ~manager:e.f.base.manager
      e.order
  in
  Lwt.finalize
    (fun () ->
      let* () = advance e.clock (until +. 1.) in
      check "first actual Reconcile Grant HTTP held" true !entered;
      check "reconcile remains held" true (Lwt.is_sleeping reconcile);
      let* () = delivery in
      eq "running job cannot complete while gap Grant waits" 0
        (count e "files.completeUploadExternal");
      check "refused completion never reserves a dispatch" true
        ((Option.get (Store.runtime e.f.db ~fire_id:fire.id)).fire.dispatched_at
       = None);
      Lwt.return_unit)
    (fun () ->
      Lwt.cancel reconcile;
      Lwt.cancel delivery;
      Store.release e.f.db ~fire_id:fire.id ~lease ~now:e.clock.now;
      settle 40)

let suite =
  suite
  @ [
      case "gap first Grant blocks a running job's final completion"
        first_grant_blocks_running_complete;
    ]

let stale_phase_after_gap () =
  with_env ~socket:true (fun e ->
      !Slack_socket.on_gap ~cause:"hello";
      !Slack_socket.on_connected ();
      let* () = pass e in
      check "initial sweep is complete" true (covered e);
      E.start ~ctx:e.ctx ~db:e.f.db ~manager:e.f.base.manager ~http:e.http ();
      !Slack_socket.on_gap ~cause:"disconnect";
      let before = List.length !(e.calls) in
      observe e
        (root
        |> set "channel" (str "C123")
        |> set "type" (str "message")
        |> set "user" (str "USLACKBOT"));
      E.wake ();
      let* () = advance e.clock (e.clock.now +. 10.) in
      eq "old complete phase cannot authorize root reads after disconnect"
        before (List.length !(e.calls));
      check "gap event stays queued locally" true
        (List.exists
           (fun (r : Slack_huddle_ledger.row) -> r.root_ts = Some root_ts)
           (Slack_huddle_ledger.list_rows ~db:e.f.db ~team:e.order.team_id
              ~channel:e.order.channel_id));
      e.roots := [ root ];
      !Slack_socket.on_gap ~cause:"hello";
      !Slack_socket.on_connected ();
      let* () = pass e in
      check "new generation sweep preserves and resolves queued root" true
        (covered e);
      Lwt.return_unit)

let suite =
  suite
  @ [
      S.case "stale complete phase cannot expose gap roots before replay"
        stale_phase_after_gap;
    ]
