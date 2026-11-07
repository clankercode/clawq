open Test_slack_standing_order_catchup_support
(** The Task 5 admission adapter is injected through the existing executor
    context. Task 6 owns wiring it into daemon dispatch; P3 startup stays shut.
*)

open Lwt.Syntax
module D = Slack_standing_order_dispatch
module P = Slack_standing_order_provenance

let test_dispatch kind e =
  let cfg = Session.get_config e.manager in
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
      cfg with
      egress =
        {
          cfg.egress with
          default_allowlist = rule :: cfg.egress.default_allowlist;
        };
    };
  let f = fire e 1 in
  let b = prepare e [ f ] in
  let* () = ensure e b in
  Sql.exec e.db
    "UPDATE slack_standing_order_fires SET \
     state='uploading',canvas_file_id='FCANVAS',notes_ready_ts='1700000060.000100' \
     WHERE id=?"
    [ Sql.text f.id ];
  let payload : P.verified =
    {
      root_ts = "1700000001.000001";
      thread_ts = "1700000001.000001";
      room_id = "R1";
      date_start = 1700000001;
      date_end = Some 1700000030;
      notes_ready_ts = "1700000060.000100";
      canvas_id = "FCANVAS";
      notes_md = "private meeting data";
      transcript = None;
      transcript_unavailable = false;
    }
  in
  let base = !(e.answer) and completed = ref 0 and raw = ref 0 in
  let response, wake = Lwt.task () in
  let cooldown, wake_cooldown = Lwt.task () in
  let waiting = ref false in
  let limiter =
    Slack_read_limiter.create_with
      ~now:(fun () -> !(e.clock))
      ~wait:(fun ~seconds ->
        if seconds > 10. then (
          waiting := true;
          cooldown)
        else (
          e.clock := !(e.clock) +. seconds;
          Lwt.return_unit))
  in

  (e.answer :=
     fun m p ->
       match m with
       | "conversations.history" -> Lwt.return (200, page [])
       | "files.getUploadURLExternal" ->
           Lwt.return
             ( 200,
               S.ok
                 [
                   ("file_id", str "FOWN");
                   ("upload_url", str "https://files.slack.com/upload/fixture");
                 ] )
       | "files.completeUploadExternal" ->
           incr completed;
           let* () = response in
           Lwt.return
             ( 200,
               S.ok
                 [
                   ( "files",
                     `List
                       [
                         obj
                           [
                             ("id", str "FOWN");
                             ( "shares",
                               obj
                                 [
                                   ( "private",
                                     obj
                                       [
                                         ( "C123",
                                           `List
                                             [
                                               obj
                                                 [
                                                   ( "ts",
                                                     str "1700000800.000100" );
                                                 ];
                                             ] );
                                       ] );
                                 ] );
                           ];
                       ] );
                 ] )
       | _ -> base m p);
  let raw_http _ =
    incr raw;
    Lwt.return
      Slack_standing_order_upload.{ status = 200; headers = []; body = "" }
  in
  let context =
    {
      e.ctx with
      Ctx.limiter = Some limiter;
      accepting = (fun () -> R.relay_allowed e.db ~fire_id:f.id ~now:!(e.clock));
    }
  in
  let run () =
    let lease =
      Store.claim e.db ~fire_id:f.id ~owner:"catchup-dispatch" ~now:!(e.clock)
      |> ok |> Option.get
    in
    let runtime = Store.runtime e.db ~fire_id:f.id |> Option.get in
    let task =
      D.deliver ~ctx:context ~db:e.db ~manager:e.manager ~http:e.http
        ~upload_http:raw_http ~lease runtime payload
    in
    (task, lease)
  in
  let before = List.length !(e.calls) in
  let task, lease = run () in
  let* () = task in
  eq "held catchup actual dispatch sends zero HTTP" before
    (List.length !(e.calls));
  eq "held catchup no raw upload" 0 !raw;
  Store.release e.db ~fire_id:f.id ~lease ~now:!(e.clock);
  advance e 600.;
  check "notice hold released" true (release e b);
  if List.mem kind [ "gap_wait"; "current_wait" ] then
    Slack_read_limiter.note_retry_after limiter
      ~key:"T123:A123:files.completeUploadExternal" 60.;
  let task, lease = run () in
  let* () =
    if List.mem kind [ "gap_wait"; "current_wait" ] then (
      check "real complete limiter waits" true !waiting;
      eq "complete HTTP not dispatched before last wait" 0 !completed;
      if kind = "gap_wait" then
        ignore
          (R.close_gap e.other ~team_id:e.order.team_id
             ~app_id:e.order.api_app_id ~channel_id:e.order.channel_id
             ~cause:"complete-wait" ~now:!(e.clock)
          |> ok);
      e.clock := !(e.clock) +. 60.;
      renew e;
      Lwt.wakeup wake_cooldown ();
      Test_slack_standing_order_worker_support.flush 10)
    else Lwt.return_unit
  in
  if kind = "gap_wait" then (
    let* () = task in
    eq "new gap after final wait forbids actual complete" 0 !completed;
    check "gap did not debit relay dispatch" true
      ((Store.runtime e.other ~fire_id:f.id |> Option.get).fire.dispatched_at
     = None);
    Store.release e.db ~fire_id:f.id ~lease ~now:!(e.clock);
    Lwt.cancel response;
    Lwt.return_unit)
  else (
    eq "actual complete invoked after hold" 1 !completed;
    eq "actual raw upload positive control" 1 !raw;
    let dispatched = Store.runtime e.other ~fire_id:f.id |> Option.get in
    check "attempt before HTTP completion" true
      (dispatched.fire.dispatched_at <> None);
    let* () = if kind = "pause" then click e b () else Lwt.return_unit in
    Lwt.wakeup wake ();
    let* () = task in
    let after = Store.runtime e.other ~fire_id:f.id |> Option.get in
    check "completion identity retained" true
      (after.fire.relay_ts = Some "1700000800.000100");
    check "recorded single dispatched attempt" true
      (List.length after.attempts = 1);
    eq "one actual completion only" 1 !completed;
    if kind <> "pause" then
      check "duplicate after dispatch reuses immutable batch" true
        ((prepare e [ f ]).id = b.id);
    if kind = "pause" then (
      check "pause forbids repost" true after.fire.repost_forbidden;
      check "order paused during complete" true
        ((Option.get (Store.find e.other e.order.id)).state = T.Paused);
      let receipts = List.map (fun (_, p) -> field "text" p) (posts e) in
      check "pause receipt discloses dispatched relay" true
        (List.exists
           (fun text -> Test_helpers.string_contains text "already")
           receipts));
    Store.release e.db ~fire_id:f.id ~lease ~now:!(e.clock);
    Lwt.return_unit)

let suite =
  [
    case "actual held dispatch then successful complete"
      (test_dispatch "current");
    case "actual Pause during complete preserves dispatched relay"
      (test_dispatch "pause");
    case "actual complete rechecks recovery after limiter wait"
      (test_dispatch "gap_wait");
    case "actual complete after limiter wait positive control"
      (test_dispatch "current_wait");
  ]
