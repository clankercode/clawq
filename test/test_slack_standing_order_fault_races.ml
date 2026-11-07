open Slack_standing_order_fault_support
open Lwt.Syntax
module D = Slack_standing_order_dispatch
module Pause = Slack_standing_order_pause
module M = Test_slack_standing_order_fault_matrix

let pause e =
  Session.update_config e.manager
    (S.with_allow_users (Session.get_config e.manager) [ "U123" ]);
  check "pause actor is excluded from the current turn allowlist" false
    (List.mem "U456"
       (Option.get (Session.get_config e.manager).channels.slack).allow_users);
  H.drain e.server.clock
    (Pause.run ~manager:e.manager ~now:e.server.clock.now ~team_id:"T123"
       ~app_id:"A123" ~channel_id:"C123" ~user_id:"U456"
       ~target:(`Order e.order.id) ~source:Pause.Text ())

let prepare e =
  let* () = pass e in
  let* () = M.capture_pre e T.Uploading in
  let fire = one e in
  let lease =
    Store.claim e.db ~fire_id:fire.id ~owner:"old-daemon"
      ~now:e.server.clock.now
    |> Result.get_ok |> Option.get
  in
  let* payload =
    H.drain e.server.clock
      (Slack_standing_order_fetch.fetch ~ctx:e.ctx ~db:e.db ~manager:e.manager
         ~http:(server_http e.server) ~lease
         (Option.get (Store.runtime e.db ~fire_id:fire.id)))
  in
  let payload = payload |> Result.get_ok |> Option.get in
  Lwt.return (fire, lease, payload)

let expired ~in_complete e =
  let* fire, lease, payload = prepare e in
  let other = Sqlite3.db_open e.path in
  let gate, wake = Lwt.task () and entered = ref false in
  let ready = e.server.clock.now in
  if in_complete then
    e.server.hook <-
      (fun m _ ->
        if m = "files.completeUploadExternal" then (
          entered := true;
          gate)
        else Lwt.return_unit)
  else
    Slack_read_limiter.note_retry_after (Option.get e.ctx.limiter)
      ~key:"T123:A123:files.completeUploadExternal" 200.;
  let pending =
    D.deliver ~ctx:e.ctx ~db:e.db ~manager:e.manager
      ~http:(server_http e.server) ~upload_http:(server_upload e.server) ~lease
      (Option.get (Store.runtime e.db ~fire_id:fire.id))
      payload
  in
  Lwt.finalize
    (fun () ->
      let* () = advance e 12. in
      check "actual completion or limiter is held" true
        (Lwt.is_sleeping pending);
      if in_complete then check "actual external complete entered" true !entered;
      (* No heartbeat in this direct dispatcher: model a stopped old daemon's
       renewal while its already-issued remote operation still exists. *)
      e.server.clock.now <- ready +. 130.;
      let successor =
        Store.claim other ~fire_id:fire.id ~owner:"new-daemon"
          ~now:e.server.clock.now
        |> Result.get_ok |> Option.get
      in
      let stale = Option.get (Store.runtime e.db ~fire_id:fire.id) in
      check "expired original owner cannot CAS through independent handle" true
        (Store.cas e.db ~lease ~before:stale ~after:stale
           ~now:e.server.clock.now
        = Ok false);
      if in_complete then Lwt.wakeup_later wake ();
      let* () = H.drain e.server.clock pending in
      if not in_complete then
        eq "expired limiter waiter sends no completion" 0 (relays e);
      Store.release other ~fire_id:fire.id ~lease:successor
        ~now:e.server.clock.now;
      let manager =
        Session.create ~config:(Session.get_config e.manager) ~db:other ()
      in
      let ctx = context e.server e.accepting in
      let* () =
        H.drain e.server.clock
          (E.process_fire ~ctx ~db:other ~manager ~http:(server_http e.server)
             ~upload_http:(server_upload e.server) ~allow_new:true
             (Option.get (Store.runtime other ~fire_id:fire.id)).fire)
      in
      eq "successor sends or adopts exactly one actual share" 1 (relays e);
      let after = Option.get (Store.runtime other ~fire_id:fire.id) in
      check "successor retained actual Slack timestamp" true
        (after.fire.relay_ts <> None);
      eq "no third dispatch" 1
        (List.length
           (List.filter
              (fun (a : Store.relay_attempt) -> a.dispatched_at <> None)
              after.attempts));
      Lwt.return_unit)
    (fun () ->
      Lwt.cancel pending;
      ignore (Sqlite3.db_close other);
      Lwt.return_unit)

let pause_complete e =
  let* fire, lease, payload = prepare e in
  let gate, u = Lwt.task () and entered = ref false in
  e.server.hook <-
    (fun m _ ->
      if m = "files.completeUploadExternal" then (
        entered := true;
        gate)
      else Lwt.return_unit);
  let pending =
    D.deliver ~ctx:e.ctx ~db:e.db ~manager:e.manager
      ~http:(server_http e.server) ~upload_http:(server_upload e.server) ~lease
      (Option.get (Store.runtime e.db ~fire_id:fire.id))
      payload
  in
  let* () = advance e 12. in
  check "complete truly dispatched before pause" true !entered;
  let* outcome = pause e in
  let receipt = Pause.receipt ~display_no:1 ~by:"U456" outcome in
  check "receipt discloses already-dispatched relay" true
    (String_util.contains receipt "already being sent before the pause");
  Lwt.wakeup_later u ();
  let* () = H.drain e.server.clock pending in
  Store.release e.db ~fire_id:fire.id ~lease ~now:e.server.clock.now;
  let saved = Option.get (Store.runtime e.db ~fire_id:fire.id) in
  eq "pause racing external completion preserves one share" 1 (relays e);
  check "post-pause response keeps exact timestamp" true
    (saved.fire.relay_ts <> None && saved.fire.repost_forbidden);
  let* () = restart e in
  start e;
  let* () = advance e 1000. in
  eq "no new dispatch after pause" 1 (relays e);
  Lwt.return_unit

let pause_wait kind e =
  let* () = pass e in
  let* () =
    if kind = "held599" then (
      e.server.roots <- [ ended_root ];
      ignore (install_instance e ended_root);
      let* () = pass e in
      start e;
      advance e 599.)
    else (
      if kind = "backoff" then e.server.ready <- false;
      if kind = "posting_unknown" then
        inject e.server Complete_after_share Timeout;
      if kind = "rate_limited" then
        Slack_read_limiter.note_retry_after (Option.get e.ctx.limiter)
          ~key:"T123:A123:files.completeUploadExternal" 300.;
      start e;
      live e;
      if kind = "backoff" then
        until e "real notes backoff"
          (fun () ->
            List.exists
              (fun (f : T.fire) -> f.reason = Some "notes_not_ready")
              (fires e))
          59
      else if kind = "posting_unknown" then
        until e "actual ambiguous completion" (fun () -> e.server.hits <> []) 59
      else advance e 20.)
  in
  let prior = relays e in
  eq "pause reaches the intended pre-dispatch or already-shared boundary"
    (if kind = "posting_unknown" then 1 else 0)
    prior;
  let* _ = pause e in
  let* () = restart e in
  start e;
  let* () = advance e 1100. in
  eq "pause during wait forbids any later share" prior (relays e);
  Lwt.return_unit

let delayed_share e =
  let* () = pass e in
  e.server.complete_shares <- false;
  e.server.visibility_delay <- 80.;
  inject e.server Complete_after_share Timeout;
  start e;
  live e;
  let* () = until e "lost complete really shared" (fun () -> relays e = 1) 59 in
  let* () = restart e in
  start e;
  let* () = advance e 200. in
  eq "delayed history and lost event still adopt one original share" 1
    (relays e);
  check "read-only adoption captures original identity" true
    ((one e).relay_ts <> None);
  let* () = advance e 900. in
  eq "no timeout-induced repost" 1 (relays e);
  Lwt.return_unit

let proven_no_share e =
  let* () = pass e in
  let rejected = ref false in
  e.server.override <-
    (fun m _ ->
      if m = "files.completeUploadExternal" && not !rejected then (
        rejected := true;
        Some
          (200, obj [ ("ok", `Bool false); ("error", str "invalid_arguments") ]))
      else None);
  start e;
  live e;
  let* () = advance e 500. in
  eq "proven no-share permits exactly one real successful repost" 1 (relays e);
  let r = Option.get (Store.runtime e.db ~fire_id:(one e).id) in
  eq "single repost debit" 1 r.fire.reposts;
  eq "both actual upload identities retained" 2 (List.length r.attempts);
  let* () = restart e in
  start e;
  let* () = advance e 1000. in
  eq "no third attempt after restart" 1 (relays e);
  Lwt.return_unit

let suite =
  [
    case "two SQLite handles expire lease during actual final limiter wait"
      (expired ~in_complete:false);
    case "two SQLite handles expire lease mid actual remote completion"
      (expired ~in_complete:true);
    case "nonallowlisted human pause races actual complete and retains receipt"
      pause_complete;
    case "lost event and lost complete response adopt delayed real server share"
      delayed_share;
    case "only proven rejected completion authorizes the one repost"
      proven_no_share;
  ]
  @ List.map
      (fun kind -> case ("pause during actual " ^ kind) (pause_wait kind))
      [ "backoff"; "held599"; "rate_limited"; "posting_unknown" ]

let two_shares ~ack e =
  let* () = pass e in
  start e;
  live e;
  let* () =
    until e "first real rate-notice fixture share" (fun () -> relays e = 1) 59
  in
  let* () = E.stop () in
  let fire = one e in
  let lease =
    Store.claim e.db ~fire_id:fire.id ~owner:"known-notice"
      ~now:e.server.clock.now
    |> Result.get_ok |> Option.get
  in
  let before = Option.get (Store.runtime e.db ~fire_id:fire.id) in
  let a = Option.get (Store.attempt before 0) in
  (* Exact Claude notice wordings are a deferred live fact. Inject the accepted
     typed notice evidence, keeping the genuine first allocation/share IDs. *)
  let a =
    {
      a with
      Store.first_reply_ts = Some (Printf.sprintf "%.6f" e.server.clock.now);
      rate_limited_at = Some e.server.clock.now;
      reaction_seen = ack;
    }
  in
  let after =
    {
      before with
      Store.attempts = [ a ];
      next_attempt_at = Some (e.server.clock.now +. 300.);
    }
  in
  check "typed known notice persists through normal CAS" true
    (Store.cas e.db ~lease ~before ~after ~now:e.server.clock.now = Ok true);
  let* call =
    H.drain e.server.clock
      (D.track ~ctx:e.ctx ~db:e.db ~manager:e.manager
         ~http:(server_http e.server) after e.order)
  in
  let* verified =
    H.drain e.server.clock
      (Slack_standing_order_provenance.verify ~call ~order:e.order
         ~room_id:"R123" ~date_start:"1700000010" ~root_ts:H.root_ts)
  in
  let payload = verified |> Result.get_ok in
  Store.release e.db ~fire_id:fire.id ~lease ~now:e.server.clock.now;
  let* () = advance e 300. in
  let lease =
    Store.claim e.db ~fire_id:fire.id ~owner:"known-notice-repost"
      ~now:e.server.clock.now
    |> Result.get_ok |> Option.get
  in
  let* () =
    H.drain e.server.clock
      (D.repost ~ctx:e.ctx ~db:e.db ~manager:e.manager
         ~http:(server_http e.server) ~upload_http:(server_upload e.server)
         ~lease
         (Option.get (Store.runtime e.db ~fire_id:fire.id))
         payload)
  in
  Store.release e.db ~fire_id:fire.id ~lease ~now:e.server.clock.now;
  eq "real same-instance share count matches known notice authority"
    (if ack then 1 else 2)
    (relays e);
  let* () = restart e in
  let* () = pass e in
  let* () = worker e in
  let* () = advance e 500. in
  let* () = worker e in
  eq "restart never permits a third actual message"
    (if ack then 1 else 2)
    (relays e);
  let saved = Option.get (Store.runtime e.db ~fire_id:fire.id) in
  eq "at most one repost across real handles and process memory loss"
    (if ack then 0 else 1)
    saved.fire.reposts;
  bounds e.server;
  Lwt.return_unit

let suite =
  suite
  @ [
      case "known rate notice permits exactly two actual server shares"
        (two_shares ~ack:false);
      case "acknowledged known rate notice forbids second actual share"
        (two_shares ~ack:true);
    ]
