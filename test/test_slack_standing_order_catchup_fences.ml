open Test_slack_standing_order_catchup_support
open Lwt.Syntax

let mutate e kind =
  let cfg = Session.get_config e.manager in
  let publish f = Session.update_config e.manager (f cfg) in
  match kind with
  | "content" | "huddles" | "fire" ->
      publish (fun cfg ->
          S.profile cfg
            ~denied:
              [
                (match kind with
                | "content" -> "slack_huddle_content"
                | "huddles" -> "slack_huddles"
                | _ -> "slack_standing_order_fire");
              ]
            ())
  | "egress" ->
      publish (fun cfg ->
          { cfg with egress = { cfg.egress with default_allowlist = [] } })
  | "disabled" ->
      publish (fun cfg ->
          S.with_slack cfg (fun sc ->
              {
                sc with
                standing_orders =
                  { sc.standing_orders with orders_enabled = false };
              }))
  | "credential" ->
      publish (fun cfg ->
          S.with_slack cfg (fun sc -> { sc with bot_token = "" }))
  | "target" ->
      publish (fun cfg ->
          S.with_slack cfg (fun sc ->
              {
                sc with
                standing_orders =
                  { sc.standing_orders with allowed_targets = [] };
              }))
  | "config" -> publish (fun cfg -> { cfg with channels = cfg.channels })
  | "owner" ->
      e.clock := !(e.clock) +. 121.;
      ignore
        (R.claim e.other ~order_id:e.order.id ~owner_id:"takeover"
           ~now:!(e.clock)
        |> Option.get)
  | "gap" ->
      ignore
        (R.close_gap e.other ~team_id:e.order.team_id ~app_id:e.order.api_app_id
           ~channel_id:e.order.channel_id ~cause:"during-wait" ~now:!(e.clock)
        |> ok)
  | "pause" ->
      ignore
        (Store.deactivate e.other ~order_id:e.order.id (Store.Pause "U456")
           ~now:!(e.clock)
        |> ok)
  | "rollout" ->
      Sql.exec e.other "DELETE FROM slack_standing_order_activation_bindings" []
  | "current" -> ()
  | _ -> assert false

let test_postwait kind e =
  let f = fire e 1 in
  let b = prepare e [ f ] in
  let blocked, wake = Lwt.task () in
  let limits = ref 0 and waiting = ref false in
  let ctx =
    {
      e.ctx with
      Ctx.limit =
        (fun ~channel_id:_ ->
          incr limits;
          if !limits = 6 then (
            waiting := true;
            blocked)
          else Lwt.return_unit);
    }
  in
  let task = ensure ~ctx e b in
  check "actual post transport is waiting" true !waiting;
  eq "no HTTP post before wake" 0 (List.length (notices e));
  mutate e kind;
  Lwt.wakeup wake ();
  let* () = task in
  eq "last wait retains fence"
    (if kind = "current" then 1 else 0)
    (List.length (notices e));
  check "no premature relay admission" false (allowed e f);
  if kind <> "current" then
    check "not confirmed after lost fence" true ((batch e b).notice_ts = None);
  Lwt.return_unit

let test_response kind method_ e =
  let f = fire e 1 in
  let b = prepare e [ f ] in
  let* () =
    if method_ = "history" then (
      Test_slack_standing_order_catchup_faults.lose_response e ~crash:true
        ~shared:true;
      Test_slack_standing_order_catchup_faults.attempt e b)
    else Lwt.return_unit
  in
  let base = !(e.answer) in
  let response, wake = Lwt.task () in
  let waiting = ref false in
  (e.answer :=
     fun m p ->
       if
         (m
         =
         if method_ = "history" then "conversations.history"
         else "chat.postMessage")
         && (method_ = "history" || get "blocks" p <> `Null)
       then (
         let* result = base m p in
         waiting := true;
         let* () = response in
         Lwt.return result)
       else base m p);
  let task = ensure e b in
  check "actual audited HTTP response held" true !waiting;
  let before = R.batch_details e.other ~batch_id:b.id in
  let n = List.length !(e.calls) in
  mutate e kind;
  Lwt.wakeup wake ();
  let* () = task in
  if kind = "current" then
    check "current response binds positive control" true
      ((batch e b).notice_ts <> None)
  else (
    eq "lost response fence no subsequent HTTP" n (List.length !(e.calls));
    check "stale response cannot bind" true ((batch e b).notice_ts = None);
    if kind <> "pause" then
      check "stale response no status mutation" true
        (R.batch_details e.other ~batch_id:b.id = before));
  Lwt.return_unit

let test_concurrent e =
  let f = fire e 1 in
  let b = prepare e [ f ] in
  let base = !(e.answer) in
  let response, wake = Lwt.task () in
  let first = ref true in
  (e.answer :=
     fun m p ->
       if m = "chat.postMessage" && get "blocks" p <> `Null && !first then (
         first := false;
         let* () = response in
         base m p)
       else base m p);
  let first_task = ensure e b in
  check "first reserves before HTTP" true (status e b = "notice_attempted");
  let manager =
    Session.create ~config:(Session.get_config e.manager) ~db:e.other ()
  in
  let* () = ensure ~db:e.other ~manager e b in
  eq "second handle does not send" 1 (List.length (notices e));
  Lwt.wakeup wake ();
  let* () = first_task in
  eq "one actual notice across both handles" 1 (List.length !(e.shared));
  check "eventual response binds existing unknown attempt" true
    (status e b = "confirmed");
  Lwt.return_unit

let suite =
  [ case "two handles racing actual post never duplicate" test_concurrent ]
  @ List.map
      (fun kind -> case ("post-wait fence " ^ kind) (test_postwait kind))
      [
        "content";
        "huddles";
        "fire";
        "egress";
        "disabled";
        "credential";
        "target";
        "config";
        "owner";
        "gap";
        "pause";
        "rollout";
        "current";
      ]
  @ List.concat_map
      (fun method_ ->
        List.map
          (fun kind ->
            case
              (method_ ^ " response fence " ^ kind)
              (test_response kind method_))
          [ "config"; "owner"; "gap"; "pause"; "current" ])
      [ "post"; "history" ]
