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
