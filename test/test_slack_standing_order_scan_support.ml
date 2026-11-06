include Test_slack_standing_order_exec_support
module Fetch = Slack_standing_order_fetch
module Scan = Slack_standing_order_reconcile_scan
module R = Slack_standing_order_recovery_store
module Q = Slack_standing_order_schedule
module Journal = Slack_standing_order_pause_store
module Grant = Slack_standing_order_grant
module SO = Slack_standing_order_slack
module Sql = Slack_standing_order_sql
module Pages = Slack_standing_order_pages
module Ctx = Slack_standing_order_ctx
module Scan_store = Slack_standing_order_scan_store

let check = Alcotest.(check bool)
let page messages = F.page messages |> set "ok" (`Bool true)
let unwrap = function Ok v -> v | Error _ -> Alcotest.fail "expected success"
let now = 1700000100.
let parent_ts = "1700000005.000001"

let message ?(user = "U456") ?thread ?(ts = "1700000070.000001") text =
  obj
    ([ ("ts", str ts); ("user", str user); ("text", str text) ]
    @ Option.fold ~none:[]
        ~some:(fun root -> [ ("thread_ts", str root) ])
        thread)

let with_scan ?(config = Fun.id) ?(at_time = now) f =
  with_env (fun old ->
      S.at at_time;
      let manager =
        Session.create ~config:(config (S.config ())) ~db:old.db ()
      in
      let e = { old with manager } in
      let limiter =
        Slack_read_limiter.create_with ~now:Store.now ~wait:(fun ~seconds ->
            S.at (Store.now () +. seconds);
            Lwt.return_unit)
      in
      let context =
        {
          (ctx e) with
          Ctx.limiter = Some limiter;
          wait = (fun ~seconds:_ -> fst (Lwt.task ()));
        }
      in
      let token =
        unwrap
          (R.close_gap e.db ~team_id:e.order.team_id ~app_id:e.order.api_app_id
             ~channel_id:e.order.channel_id ~cause:"test" ~now:(Store.now ()))
      in
      check "connected" true (R.mark_connected e.db ~token);
      let owner =
        Option.get
          (R.claim e.db ~order_id:e.order.id ~owner_id:"scan-test"
             ~now:(Store.now ()))
      in
      check "begin fixed coverage" true
        (R.begin_scan e.db ~order:e.order ~owner ~mode:R.Gap_recovery ~gap:token
           ~oldest:(Option.get e.order.active_since)
           ~latest:(Scan.timestamp at_time) ~now:(Store.now ()));
      S.override e.mock "conversations.history" (fun p ->
          if get "limit" p = `Int 1 then (200, page [ !(e.root) ])
          else (200, page []));
      let validated =
        unwrap
          (Lwt_main.run
             (Grant.evaluate ~db:e.db
                ~current_config:(fun () -> Session.get_config manager)
                ~http:e.mock.http ~limiter
                ~before_send:(fun () ->
                  if
                    R.replay_fenced e.db ~order:e.order ~owner
                      ~now:(Store.now ())
                  then Ok ()
                  else Error SO.Dispatch_refused)
                ~now:(Store.now ())
                ~stage:
                  (Grant.Reconcile { reconcile_id = Scan.reconcile_id owner })
                ~unit_label:"ignored"
                (Grant.subject_of_order e.order)))
      in
      f e context owner token validated)

let sweep e ctx owner v =
  Lwt_main.run
    (Scan.sweep_with_grant ~db:e.db ~manager:e.manager ~ctx ~owner
       ~order:e.order ~validated:v)

let discover e ctx owner v =
  Lwt_main.run
    (Scan.discover ~db:e.db ~manager:e.manager ~ctx ~owner ~order:e.order
       ~call:v.Grant.call)

let state e = (Option.get (Store.find e.db e.order.id)).state
let phase e = R.phase e.db e.order.id

let posts e =
  !(e.mock.calls)
  |> List.filter_map (fun (_, m, p) ->
      if m = "chat.postMessage" then Some p else None)
