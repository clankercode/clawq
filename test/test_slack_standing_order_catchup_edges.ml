open Test_slack_standing_order_catchup_support
open Lwt.Syntax
module F = Test_slack_standing_order_catchup_faults
module Fence = Test_slack_standing_order_catchup_fences

let test_cooldown kind e =
  let f = fire e 1 in
  let b = prepare e [ f ] in
  let blocked, wake = Lwt.task () in
  let waiting = ref false in
  let limiter =
    Slack_read_limiter.create_with
      ~now:(fun () -> !(e.clock))
      ~wait:(fun ~seconds:_ ->
        waiting := true;
        blocked)
  in
  Slack_read_limiter.note_retry_after limiter ~key:"T123:A123:chat.postMessage"
    17.;
  let ctx = { e.ctx with Ctx.limiter = Some limiter } in
  let task = ensure ~ctx e b in
  check "actual method cooldown holds HTTP" true !waiting;
  eq "no notice in server cooldown" 0 (List.length (notices e));
  if kind = "delayed" then advance e 200.
  else (
    e.clock := !(e.clock) +. 17.;
    Fence.mutate e kind);
  Lwt.wakeup wake ();
  let* () = task in
  eq "cooldown fence or positive control"
    (if kind = "current" || kind = "delayed" then 1 else 0)
    (List.length (notices e));
  if kind = "current" || kind = "delayed" then
    Alcotest.(check (float 0.001))
      "600 seconds from delayed cooldown response" (!(e.clock) +. 600.)
      (Option.get (batch e b).release_at);
  Lwt.return_unit

let test_prepare_atomic e =
  let f = fire e 1 in
  let cards =
    Sql.int_query e.other "SELECT count(*) FROM slack_standing_order_cards" []
  in
  Sql.exec e.db
    "CREATE TRIGGER abort_batch BEFORE INSERT ON \
     slack_standing_order_recovery_batches BEGIN SELECT RAISE(ABORT,'fault'); \
     END"
    [];
  let failed =
    try
      ignore (prepare e [ f ]);
      false
    with Failure _ -> true
  in
  check "batch insertion fault surfaced" true failed;
  eq "batch failure rolls back card" cards
    (Sql.int_query e.other "SELECT count(*) FROM slack_standing_order_cards" []);
  eq "no half batch" 0
    (Sql.int_query e.other
       "SELECT count(*) FROM slack_standing_order_recovery_batches" []);
  Sql.exec e.db "DROP TRIGGER abort_batch" [];
  let b = prepare e [ f ] in
  let* () = ensure e b in
  eq "retry concrete notice works" 1 (List.length !(e.shared));
  Lwt.return_unit

let test_reservation_fault e =
  let f = fire e 1 in
  let b = prepare e [ f ] in
  Sql.exec e.db
    "CREATE TRIGGER abort_attempt BEFORE UPDATE OF status ON \
     slack_standing_order_recovery_batches WHEN NEW.status='notice_attempted' \
     BEGIN SELECT RAISE(ABORT,'fault'); END"
    [];
  let* () = ensure e b in
  eq "failed durable attempt forbids HTTP" 0 (List.length (notices e));
  check "failed attempt rollback" true (status e b = "pending");
  Sql.exec e.db "DROP TRIGGER abort_attempt" [];
  let* () = ensure e b in
  eq "retry after proven no attempt" 1 (List.length (notices e));
  Lwt.return_unit

let test_binding_fault e =
  let f = fire e 1 in
  let b = prepare e [ f ] in
  Sql.exec e.db
    "CREATE TRIGGER abort_bind BEFORE UPDATE OF message_ts ON \
     slack_standing_order_cards WHEN NEW.id=OLD.id AND NEW.message_ts IS NOT \
     NULL BEGIN SELECT RAISE(ABORT,'fault'); END"
    [];
  let* () = ensure e b in
  eq "one share before binding failure" 1 (List.length !(e.shared));
  check "binding failed remains attempted" true (status e b = "notice_attempted");
  check "unbound card cannot admit" false (allowed e f);
  Sql.exec e.db "DROP TRIGGER abort_bind" [];
  advance e 200.;
  let db = Sqlite3.db_open e.path in
  let manager = Session.create ~config:(Session.get_config e.manager) ~db () in
  let* () =
    Lwt.finalize
      (fun () -> ensure ~db ~manager e b)
      (fun () ->
        ignore (Sqlite3.db_close db);
        Lwt.return_unit)
  in
  check "reopen adopts after actual bind fault" true (status e b = "confirmed");
  eq "binding fault never resends" 1 (List.length (notices e));
  Alcotest.(check (float 0.001))
    "adoption hold after bind fault" (!(e.clock) +. 600.)
    (Option.get (batch e b).release_at);
  Lwt.return_unit

let test_missing_scope e =
  let f = fire e 1 in
  let b = prepare e [ f ] in
  F.lose_response e ~crash:true ~shared:true;
  let* () = F.attempt e b in
  let base = !(e.answer) in
  (e.answer :=
     fun m p ->
       if m = "conversations.history" then
         Lwt.return
           (200, S.obj [ ("ok", `Bool false); ("error", str "missing_scope") ])
       else base m p);
  let* () = ensure e b in
  let o = Option.get (Store.find e.other e.order.id) in
  check "hard history refusal suspends" true (o.state = T.Suspended);
  check "hard refusal cancels pending batch" true (status e b = "cancelled");
  check "exact failing method inspectable" true
    (o.suspended_reason = Some "missing_scope:conversations.history");
  eq "hard refusal no resend" 1 (List.length (notices e));
  Lwt.return_unit

let test_status e =
  let f = fire e 1 in
  let b = prepare e [ f ] in
  F.lose_response e ~crash:true ~shared:false;
  let* () = F.attempt e b in
  let* () = ensure e b in
  let json = R.status_json e.other ~order_id:e.order.id in
  check "unknown batch available through show facade" true
    (Test_helpers.string_contains (Yojson.Safe.to_string json) "notice_unknown");
  Lwt.return_unit

let test_zone e =
  let previous = Sys.getenv_opt "TZ" in
  let local_before = Unix.localtime S.now in
  Test_slack_standing_order_relay.with_tz "Australia/Sydney" (fun () ->
      let notice =
        Render.catchup_notice e.order ~card_id:"soc_fixed" ~count:3
          ~release_at:1700000700.
      in
      Alcotest.(check string)
        "explicit local time and zone"
        "clawq was offline; 3 huddles will be relayed at 09:25 AEDT. Pause \
         SO-1 to stop them."
        notice.text);
  Alcotest.(check (option string))
    "timezone restored including absence" previous (Sys.getenv_opt "TZ");
  check "libc timezone restored" true (Unix.localtime S.now = local_before);
  Lwt.return_unit

let test_initial_deny kind e =
  let f = fire e 1 in
  let b = prepare e [ f ] in
  Fence.mutate e kind;
  let* () = ensure e b in
  eq "initial capability denial zero HTTP" 0 (List.length !(e.calls));
  check "initial capability denial suspends" true
    ((Option.get (Store.find e.other e.order.id)).state = T.Suspended);
  check "initial capability denial cancels batch" true (status e b = "cancelled");
  let statuses =
    Slack_standing_order_report.statuses ~db:e.other ~order_id:e.order.id
  in
  check "denial report remains local only" true
    (List.exists (fun (_, s) -> s = "local_only") statuses);
  Lwt.return_unit

let suite =
  [
    case "batch and card commit together" test_prepare_atomic;
    case "failed attempt transaction forbids post" test_reservation_fault;
    case "actual binding failure then DB reopen adopts" test_binding_fault;
    case "hard adoption history failure preserves method" test_missing_scope;
    case "unknown is inspectable without report delivery" test_status;
    case "catch-up copy includes daemon timezone" test_zone;
  ]
  @ List.map
      (fun kind -> case ("Retry-After wait " ^ kind) (test_cooldown kind))
      [ "content"; "egress"; "owner"; "gap"; "current"; "delayed" ]

let suite =
  suite
  @ List.map
      (fun kind ->
        case
          ("initial zero HTTP capability denial " ^ kind)
          (test_initial_deny kind))
      [ "content"; "huddles"; "fire" ]

let test_local_notice_policy kind e =
  let admin = kind = "revoked_admin" || kind = "still_admin" in
  if admin then (
    Session.update_config e.manager (S.config ~creator:So_creator_admin ());
    Sql.exec e.other
      "INSERT INTO channel_admins(channel,sender_id) VALUES('slack','U123')" []);
  Sql.exec e.other "UPDATE slack_standing_orders SET expires_at=? WHERE id=?"
    [
      Sql.text
        (Time_util.sql_datetime_utc
           ~t:(!(e.clock) +. if kind = "expired" then 10. else 300.)
           ());
      Sql.text e.order.id;
    ];
  let f = fire e 1 in
  let b = prepare e [ f ] in
  Sql.exec e.other "CREATE TABLE test_notice_attempts(batch_id TEXT)" [];
  Sql.exec e.other
    "CREATE TRIGGER record_notice_attempt AFTER UPDATE OF status ON \
     slack_standing_order_recovery_batches WHEN NEW.status='notice_attempted' \
     BEGIN INSERT INTO test_notice_attempts VALUES(NEW.id); END"
    [];
  let captured = Session.get_config e.manager in
  let blocked, wake = Lwt.task () in
  let waiting = ref false in
  let limiter =
    Slack_read_limiter.create_with
      ~now:(fun () -> !(e.clock))
      ~wait:(fun ~seconds ->
        if !waiting then (
          advance e seconds;
          Lwt.return_unit)
        else (
          waiting := true;
          blocked))
  in
  Slack_read_limiter.note_retry_after limiter ~key:"T123:A123:chat.postMessage"
    17.;
  let ctx = { e.ctx with Ctx.limiter = Some limiter } in
  let task = ensure ~ctx e b in
  check "real post cooldown reached after Grant reads" true !waiting;
  eq "five Grant reads before cooldown" 5 (List.length !(e.calls));
  eq "no notice while waiting" 0 (List.length (notices e));
  check "no reservation while waiting" true (status e b = "pending");
  advance e 200.;
  if kind = "revoked_admin" then
    check "second handle removes admin" true
      (Admin.remove_admin ~db:e.other ~channel:"slack" ~sender_id:"U123");
  check "same config publication during cooldown" true
    (Session.get_config e.manager == captured);
  check "same live recovery owner" true
    (R.fenced e.other ~order_id:e.order.id ~owner:e.owner ~now:!(e.clock));
  Lwt.wakeup wake ();
  let* () = task in
  let denied = kind = "expired" || kind = "revoked_admin" in
  eq "current local policy gates actual notice post"
    (if denied then 0 else 1)
    (List.length (notices e));
  let after = batch e b in
  let card = Store.find_card e.other b.id |> Option.get in
  eq "observed durable attempt transitions"
    (if denied then 0 else 1)
    (Sql.int_query e.other "SELECT count(*) FROM test_notice_attempts" []);
  if denied then (
    check "denied notice never reserves an attempt" true
      (after.release_at = None);
    check "denied notice no batch binding" true (after.notice_ts = None);
    check "denied notice no card binding" true (card.message_ts = None);
    if admin then (
      let order = Store.find e.other e.order.id |> Option.get in
      check "admin refusal suspends atomically" true
        (order.state = T.Suspended
        && order.suspended_reason = Some "grantor_not_admin");
      check "suspension cancels batch" true (status e b = "cancelled"))
    else
      check "expiry stays pending without attempt" true (status e b = "pending"))
  else (
    check "successful notice confirms" true (status e b = "confirmed");
    check "successful notice binds exact card" true
      (after.notice_ts <> None && after.notice_ts = card.message_ts));
  Lwt.return_unit

let test_inflight_pause kind e =
  Session.update_config e.manager
    (S.with_allow_users (Session.get_config e.manager) [ "U123" ]);
  let f = fire e 1 in
  let b = prepare e [ f ] in
  let* () = ensure e b in
  if kind = "released" then (
    advance e 600.;
    check "released notice control" true (release e b));
  let card_id, message_ts =
    if kind = "ordinary" then
      let card =
        Sql.query e.other
          "SELECT id FROM slack_standing_order_cards WHERE id<>?"
          [ Sql.text b.id ]
          (fun s -> Sql_util.text_column s 0)
        |> List.hd |> Store.find_card e.other |> Option.get
      in
      (Some card.id, card.message_ts)
    else (None, None)
  in
  let base = !(e.answer) in
  let blocked, wake = Lwt.task () in
  let waiting = ref false in
  (e.answer :=
     fun m p ->
       let* response = base m p in
       if m = "conversations.members" then (
         waiting := true;
         let* () = blocked in
         Lwt.return response)
       else Lwt.return response);
  let task = click e b ?card_id ?message_ts () in
  check "actual Slack ingress awaits membership HTTP" true !waiting;
  let active = Store.find e.other e.order.id |> Option.get in
  check "click has not paused before membership" true (active.state = T.Active);
  let pending =
    if kind = "resumed" || kind = "ordinary" then (
      ignore
        (Store.deactivate e.other ~order_id:e.order.id (Store.Pause "U123")
           ~now:!(e.clock)
        |> ok);
      let paused = Store.find e.other e.order.id |> Option.get in
      let resumed =
        Store.resume e.other ~order_id:e.order.id ~expected_epoch:paused.epoch
          ~active_since:"1700000200.000001" ~bot_user_id:"UBOT" ~bot_id:"BOWN"
          ~now:!(e.clock)
        |> ok
      in
      eq "resume advances twice" (e.order.epoch + 2) resumed.epoch;
      e.clock := !(e.clock) +. 121.;
      let gap =
        R.close_gap e.other ~team_id:resumed.team_id ~app_id:resumed.api_app_id
          ~channel_id:resumed.channel_id ~cause:"resumed" ~now:!(e.clock)
        |> ok
      in
      check "new epoch connected" true (R.mark_connected e.other ~token:gap);
      let owner =
        R.claim e.other ~order_id:resumed.id ~owner_id:"resumed-owner"
          ~now:!(e.clock)
        |> Option.get
      in
      check "new epoch scan" true
        (R.begin_scan e.other ~order:resumed ~owner ~mode:R.Gap_recovery ~gap
           ~oldest:"1700000200.000001" ~latest:"1700000221.000000"
           ~now:!(e.clock));
      check "new epoch pause sweep" true
        (R.complete_pause e.other ~order_id:resumed.id ~owner ~now:!(e.clock));
      check "new epoch history" true
        (R.complete_history e.other ~order_id:resumed.id ~owner ~now:!(e.clock));
      check "new epoch complete" true
        (R.finish_scan e.other ~order_id:resumed.id ~owner ~now:!(e.clock));
      check "new epoch verified" true
        (R.verify_gap e.other ~token:gap ~owner ~through:"1700000221.000000"
           ~now:!(e.clock));
      R.record_root ~recovery:true e.other ~order:resumed
        ~root_ts:"1700000201.000001" ~room_id:"RNEW" ~date_start:"1700000201"
        ~fire_id:None;
      let nf =
        Store.insert_fire e.other ~order:resumed ~root_ts:"1700000201.000001"
          ~room_id:"RNEW" ~date_start:"1700000201" ~now:!(e.clock) ()
        |> ok |> Option.get
      in
      let nb =
        C.prepare ~db:e.other ~order:resumed ~fire_ids:[ nf.id ] ~now:!(e.clock)
      in
      check "new batch is pending before old response" true
        (status e nb = "pending");
      Some (resumed, nf, nb, Store.runtime e.other ~fire_id:nf.id))
    else None
  in
  (match kind with
  | "batch_message" ->
      Sql.exec e.other
        "UPDATE slack_standing_order_recovery_batches SET notice_ts=? WHERE \
         id=?"
        [ Sql.text "1700000100.999999"; Sql.text b.id ]
  | "card_message" ->
      Store.set_card_message_ts e.other ~card_id:b.id "1700000100.999999"
  | "missing_batch" ->
      Sql.exec e.other
        "DELETE FROM slack_standing_order_recovery_batches WHERE id=?"
        [ Sql.text b.id ]
  | _ -> ());
  Lwt.wakeup wake ();
  let* () = task in
  (match pending with
  | None when List.mem kind [ "batch_message"; "card_message"; "missing_batch" ]
    ->
      check "changed notice binding cannot pause current epoch" true
        (Store.find e.other e.order.id = Some active)
  | Some (resumed, nf, nb, _) when kind = "ordinary" ->
      let paused = Store.find e.other e.order.id |> Option.get in
      check "ordinary activation button remains epoch flexible" true
        (paused.state = T.Paused && paused.epoch = resumed.epoch + 1);
      check "ordinary pause sweeps new batch" true (status e nb = "cancelled");
      check "ordinary pause sweeps new fire" true
        ((Store.runtime e.other ~fire_id:nf.id |> Option.get).fire.state
       = T.Skipped_inactive)
  | None ->
      check "same member and notice really pause without race" true
        ((Store.find e.other e.order.id |> Option.get).state = T.Paused);
      check "successful pause cancels original batch" true
        (status e b = "cancelled");
      check "successful pause sweeps fire" true
        ((Store.runtime e.other ~fire_id:f.id |> Option.get).fire.state
       = T.Skipped_inactive)
  | Some (resumed, nf, nb, before) ->
      check "stale inflight click preserves resumed order" true
        (Store.find e.other e.order.id = Some resumed);
      check "stale click preserves new pending batch" true
        (status e nb = "pending");
      check "stale click preserves exact new fire runtime" true
        (Store.runtime e.other ~fire_id:nf.id = before);
      check "old batch remains cancelled" true (status e b = "cancelled"));
  Lwt.return_unit

let suite =
  suite
  @ List.map
      (fun kind ->
        case
          ("local notice policy after cooldown " ^ kind)
          (test_local_notice_policy kind))
      [ "expired"; "unexpired"; "revoked_admin"; "still_admin" ]
  @ List.map
      (fun kind ->
        case ("inflight catch-up pause " ^ kind) (test_inflight_pause kind))
      [
        "resumed";
        "current";
        "ordinary";
        "released";
        "batch_message";
        "card_message";
        "missing_batch";
      ]
