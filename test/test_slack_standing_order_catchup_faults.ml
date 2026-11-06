open Test_slack_standing_order_catchup_support
open Lwt.Syntax

let lose_response e ~crash ~shared =
  let base = !(e.answer) in
  e.answer :=
    fun m p ->
      if m = "chat.postMessage" && get "blocks" p <> `Null then
        let* _ = if shared then base m p else Lwt.return (200, `Null) in
        if crash then Lwt.fail Lwt.Canceled else Lwt.fail_with "lost response"
      else base m p

let attempt e b =
  Lwt.catch
    (fun () -> ensure e b)
    (function Lwt.Canceled -> Lwt.return_unit | exn -> Lwt.fail exn)

let test_adopt ?(delay = 200.) crash e =
  let f = fire e 1 in
  let b = prepare e [ f ] in
  lose_response e ~crash ~shared:true;
  let* () = attempt e b in
  check "one external side effect" true (List.length !(e.shared) = 1);
  check "held until adoption" false (allowed e f);
  check "write ahead survives loss" true
    (List.mem (status e b) [ "notice_attempted"; "notice_unknown" ]);
  advance e delay;
  let confirmed_now = ref nan in
  let original = !(e.answer) in
  let history = ref 0 in
  (e.answer :=
     fun m p ->
       if m = "conversations.history" then (
         incr history;
         if field "cursor" p = "" then
           Lwt.return (200, page ~cursor:"next" ~more:true [])
         else (
           confirmed_now := !(e.clock);
           Lwt.return (200, page !(e.shared))))
       else original m p);
  let reopened = Sqlite3.db_open e.path in
  let manager =
    Session.create ~config:(Session.get_config e.manager) ~db:reopened ()
  in
  let* () =
    Lwt.finalize
      (fun () -> ensure ~db:reopened ~manager e b)
      (fun () ->
        ignore (Sqlite3.db_close reopened);
        Lwt.return_unit)
  in
  eq "complete paginated adoption" 2 !history;
  eq "no resend" 1 (List.length (notices e));
  check "adopted exact own ts" true
    ((batch e b).notice_ts = Some (field "ts" (List.hd !(e.shared))));
  Alcotest.(check (float 0.001))
    "fresh 600 from adoption" (!confirmed_now +. 600.)
    (Option.get (batch e b).release_at);
  advance e 599.;
  check "adoption at 599 held" false (release e b);
  advance e 1.;
  check "adoption at 600 released" true (release e b);
  check "adoption admission positive control" true (allowed e f);
  Lwt.return_unit

let test_unknown kind e =
  let f = fire e 1 in
  let b = prepare e [ f ] in
  lose_response e ~crash:true ~shared:true;
  let* () = attempt e b in
  let own = List.hd !(e.shared) in
  let alien =
    match kind with
    | "human" -> [ set "user" (str "U456") own ]
    | "bot" -> [ set "bot_id" (str "BOTHER") own ]
    | "card" ->
        [
          set "blocks"
            (Render.catchup_notice e.order ~card_id:"soc_other" ~count:1
               ~release_at:!(e.clock))
              .blocks
            own;
        ]
    | "channel" -> [ set "channel" (str "COTHER") own ]
    | "thread" -> [ set "thread_ts" (str "1700000001.1") own ]
    | "multiple" -> [ own; set "ts" (str "1700000100.002000") own ]
    | "missing" -> []
    | _ -> [ own ]
  in
  let base = !(e.answer) in
  (e.answer :=
     fun m p ->
       if m = "conversations.history" then
         Lwt.return
           ( 200,
             match kind with
             | "incomplete" ->
                 S.ok [ ("messages", `List alien); ("has_more", `Bool true) ]
             | "limited" -> set "is_limited" (`Bool true) (page alien)
             | "repeated" -> page ~cursor:"repeat" ~more:true alien
             | "bound" ->
                 page
                   ~cursor:(string_of_int (List.length !(e.calls)))
                   ~more:true alien
             | _ -> page alien )
       else base m p);
  let* () = ensure e b in
  let* () = ensure e b in
  check "unknown stays inspectable" true (status e b = "notice_unknown");
  check "unknown no release" false (release e b);
  check "unknown no relay admission" false (allowed e f);
  eq "unknown never resends" 1 (List.length (notices e));
  eq "one management report across retries" 1
    (List.length (List.filter (fun (_, p) -> get "blocks" p = `Null) (posts e)));
  let statuses =
    Slack_standing_order_report.statuses ~db:e.other ~order_id:e.order.id
  in
  check "once-only report durable" true
    (List.mem ("catchup_notice_unknown:" ^ b.id, "sent") statuses);
  Lwt.return_unit

let test_reclaim confirmed e =
  let f = fire e 1 in
  let b = prepare e [ f ] in
  if not confirmed then lose_response e ~crash:true ~shared:true;
  let* () = attempt e b in
  let prior = (batch e b).release_at in
  e.clock := !(e.clock) +. 200.;
  let owner =
    R.claim e.other ~order_id:e.order.id ~owner_id:"second-daemon"
      ~now:!(e.clock)
    |> Option.get
  in
  check "old owner cannot reserve" false
    (R.reserve_notice e.db ~batch_id:b.id ~owner:e.owner ~now:!(e.clock));
  let manager =
    Session.create ~config:(Session.get_config e.manager) ~db:e.other ()
  in
  let e2 = { e with db = e.other; other = e.db; owner; manager } in
  let* () = ensure e2 b in
  eq "reclaim one actual notice" 1 (List.length (notices e));
  check "new owner owns existing batch" true
    (R.batch_fenced e.other (batch e b) owner !(e.clock));
  if confirmed then
    check "reclaim does not restart confirmed timer" true
      ((batch e b).release_at = prior)
  else
    Alcotest.(check (float 0.001))
      "unknown reclaimed adoption full 600" (!(e.clock) +. 600.)
      (Option.get (batch e b).release_at);
  advance e2 600.;
  check "new owner release positive" true (release e2 b);
  check "old owner cannot transition" false
    (R.release_batch e.db ~batch_id:b.id ~owner:e.owner ~now:!(e.clock));
  Lwt.return_unit

let test_pause when_ op e =
  let f = fire e 1 in
  let b = prepare e [ f ] in
  let pause () =
    match op with
    | "button" ->
        let* () = click e b () in
        Lwt.return_unit
    | _ ->
        ignore
          (Store.deactivate e.other ~order_id:e.order.id
             (match op with
             | "suspend" -> Store.Suspend "test"
             | "cancel" -> Store.Cancel "U456"
             | _ -> Store.Pause "U456")
             ~now:!(e.clock)
          |> ok);
        Lwt.return_unit
  in
  let* () = if when_ = "before" then pause () else Lwt.return_unit in
  let* () = ensure e b in
  let* () = if when_ = "after" then pause () else Lwt.return_unit in
  check "lifecycle cancels batch transactionally" true (status e b = "cancelled");
  check "cancelled batch cannot release" false (release e b);
  check "cancelled fire cannot relay" false (allowed e f);
  let after = Option.get (Store.find e.db e.order.id) in
  if after.state <> T.Cancelled then
    ignore
      (Store.resume e.db ~order_id:after.id ~expected_epoch:after.epoch
         ~active_since:"1700000200.000001" ~bot_user_id:"UBOT" ~bot_id:"BOWN"
         ~now:!(e.clock)
      |> ok);
  let* () = ensure e b in
  check "resume never revives batch" true (status e b = "cancelled");
  eq "no new notice after lifecycle/resume"
    (if when_ = "before" then 0 else 1)
    (List.length (notices e));
  Lwt.return_unit

let test_spoof_click kind e =
  let f = fire e 1 in
  let b = prepare e [ f ] in
  let* () = ensure e b in
  let n = List.length !(e.calls) in
  let* () =
    match kind with
    | "team" -> click ~team:"TOTHER" e b ()
    | "app" -> click ~app:"AOTHER" e b ()
    | "channel" -> click ~channel:"COTHER" e b ()
    | "message" -> click ~message_ts:"1700000101.999999" e b ()
    | _ -> assert false
  in
  eq "invalid card binding zero HTTP" n (List.length !(e.calls));
  check "invalid card does not pause" true
    ((Option.get (Store.find e.db e.order.id)).state = T.Active);
  Lwt.return_unit

let test_stale_card e =
  let f = fire e 1 in
  let b = prepare e [ f ] in
  let* () = ensure e b in
  ignore
    (Store.deactivate e.db ~order_id:e.order.id (Store.Pause "U123")
       ~now:!(e.clock)
    |> ok);
  let after = Option.get (Store.find e.db e.order.id) in
  ignore
    (Store.resume e.db ~order_id:e.order.id ~expected_epoch:after.epoch
       ~active_since:"1700000200.000001" ~bot_user_id:"UBOT" ~bot_id:"BOWN"
       ~now:!(e.clock)
    |> ok);
  let before = List.length !(e.calls) in
  let* () = click e b () in
  eq "old catchup button zero HTTP after resume" before (List.length !(e.calls));
  check "old notice does not pause new epoch" true
    ((Option.get (Store.find e.db e.order.id)).state = T.Active);
  Lwt.return_unit

let test_gap e =
  let f = fire e 1 in
  let b = prepare e [ f ] in
  let* () = ensure e b in
  advance e 599.;
  let token =
    R.close_gap e.other ~team_id:e.order.team_id ~app_id:e.order.api_app_id
      ~channel_id:e.order.channel_id ~cause:"new-disconnect" ~now:!(e.clock)
    |> ok
  in
  advance e 1.;
  check "disconnect blocks release" false (release e b);
  check "hello" true (R.mark_connected e.db ~token);
  check "hello alone cannot release" false (release e b);
  check "new scan" true
    (R.begin_scan e.db ~order:e.order ~owner:e.owner ~mode:R.Gap_recovery
       ~gap:token ~oldest:e.coverage.oldest ~latest:"1700000700.000000"
       ~now:!(e.clock));
  check "new sweep incomplete" false (release e b);
  check "pause complete" true
    (R.complete_pause e.db ~order_id:e.order.id ~owner:e.owner ~now:!(e.clock));
  check "history complete" true
    (R.complete_history e.db ~order_id:e.order.id ~owner:e.owner ~now:!(e.clock));
  check "scan complete" true
    (R.finish_scan e.db ~order_id:e.order.id ~owner:e.owner ~now:!(e.clock));
  check "gap verified" true
    (R.verify_gap e.db ~token ~owner:e.owner ~through:"1700000700.000000"
       ~now:!(e.clock));
  let* () = ensure e b in
  eq "new gap confirmed notice never repeated" 1 (List.length (notices e));
  check "new verified gap rebind and release" true (release e b);
  check "new gap positive admission" true (allowed e f);
  Lwt.return_unit

let suite =
  [
    case "timeout after share adopts paginated own marker" (test_adopt false);
    case "crash after success before binding adopts after DB reopen"
      (test_adopt true);
    case "confirmed owner reclaim preserves timer" (test_reclaim true);
    case "unknown owner reclaim adopts without resend" (test_reclaim false);
    case "new gap requires completed sweep before release" test_gap;
    case "catch-up Pause button member outside allowlist"
      (test_pause "after" "button");
    case "old catch-up card cannot pause resumed epoch" test_stale_card;
  ]
  @ List.map
      (fun kind -> case ("unknown notice: " ^ kind) (test_unknown kind))
      [
        "missing";
        "human";
        "bot";
        "card";
        "channel";
        "thread";
        "multiple";
        "incomplete";
        "limited";
        "repeated";
        "bound";
      ]
  @ List.concat_map
      (fun when_ ->
        List.map
          (fun op -> case (when_ ^ " notice " ^ op) (test_pause when_ op))
          [ "pause"; "suspend"; "cancel" ])
      [ "before"; "after" ]
  @ List.map
      (fun kind -> case ("Pause card rejects " ^ kind) (test_spoof_click kind))
      [ "team"; "app"; "channel"; "message" ]

let test_takeover_during_response e =
  let f = fire e 1 in
  let b = prepare e [ f ] in
  let base = !(e.answer) in
  let response, wake = Lwt.task () in
  (e.answer :=
     fun m p ->
       let* result = base m p in
       if m = "chat.postMessage" && get "blocks" p <> `Null then
         let* () = response in
         Lwt.return result
       else Lwt.return result);
  let old_task = ensure e b in
  eq "share visible while old response held" 1 (List.length !(e.shared));
  e.clock := !(e.clock) +. 200.;
  let owner =
    R.claim e.other ~order_id:e.order.id ~owner_id:"takeover-in-flight"
      ~now:!(e.clock)
    |> Option.get
  in
  let manager =
    Session.create ~config:(Session.get_config e.manager) ~db:e.other ()
  in
  let e2 = { e with db = e.other; other = e.db; owner; manager } in
  let* () = ensure e2 b in
  let bound = batch e b in
  check "second owner adopted exact existing post" true (bound.notice_ts <> None);
  Lwt.wakeup wake ();
  let* () = old_task in
  check "stale success cannot rewrite new binding or timer" true
    (batch e b = bound);
  eq "two owners one actual side effect" 1 (List.length !(e.shared));
  eq "two owners one notice invocation" 1 (List.length (notices e));
  Lwt.return_unit

let suite =
  suite
  @ [
      case "long outage adoption grants fresh full 600"
        (test_adopt ~delay:2000. true);
      case "two owners adopt while old post response remains held"
        test_takeover_during_response;
    ]
