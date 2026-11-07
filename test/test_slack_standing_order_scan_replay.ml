open Test_slack_standing_order_scan_support
open Lwt.Syntax

let test_replay_pacing_and_cooldown () =
  with_scan (fun e oldctx owner _ v ->
      let module W = Test_slack_standing_order_worker in
      let c = W.clock () in
      c.now <- Store.now ();
      (Store.clock := fun () -> c.now);
      let limiter =
        Slack_read_limiter.create_with ~now:(fun () -> c.now) ~wait:(W.wait c)
      in
      let ctx =
        { oldctx with Ctx.limiter = Some limiter; now = (fun () -> c.now) }
      in
      S.override e.mock "conversations.history" (fun _ ->
          (200, page [ message "pause SO-1" ]));
      let times = ref [] in
      let http ~verb ~token ~method_ ~params =
        if method_ = "chat.postMessage" then times := !times @ [ c.now ];
        e.mock.http ~verb ~token ~method_ ~params
      in
      let factory ~before_send =
        SO.make ~db:e.db
          ~current_config:(fun () -> Session.get_config e.manager)
          ~snapshot:v.snapshot ~channel_id:"C123" ~team_id:"T123" ~app_id:"A123"
          ~capability_fence:true ~limiter ~http ~before_send
          ~audit:
            (SO.audit_label ~order_id:(Some e.order.id)
               ~unit_label:"reconcile:pacing")
          ()
      in
      let call = factory ~before_send:(fun () -> Ok ()) in
      let relay =
        factory ~before_send:(fun () ->
            if R.fenced e.db ~order_id:e.order.id ~owner ~now:c.now then Ok ()
            else Error SO.Dispatch_refused)
      in
      Slack_read_limiter.note_retry_after limiter
        ~key:"T123:A123:files.completeUploadExternal" 17.;
      Lwt_main.run
        (Lwt.finalize
           (fun () ->
             let queued =
               relay "files.completeUploadExternal"
                 [ ("channel_id", str "C123") ]
             in
             let work =
               Scan.sweep_with_grant ~db:e.db ~manager:e.manager ~ctx ~owner
                 ~order:e.order ~validated:{ v with Grant.call }
             in
             let* () = W.flush 30 in
             Alcotest.check S.state_t "privacy pause wins while relay waits"
               T.Paused (state e);
             Alcotest.(check int)
               "first notice dispatched" 1 (List.length !times);
             Slack_read_limiter.note_retry_after limiter
               ~key:"T123:A123:chat.postMessage" 17.;
             let* () = W.advance c 1. in
             Alcotest.(check int)
               "queued receipt cannot escape extended Retry-After" 1
               (List.length !times);
             let* () = W.advance c 16. in
             (* The denied queued completion still reserves the channel slot. *)
             let* () = W.advance c 2. in
             let* result = work in
             unwrap result;
             let* result = queued in
             check "queued relay final fence denied after pause" true
               (result = Error SO.Dispatch_refused);
             check "shared channel pacing and cooldown" true
               (match !times with [ a; b ] -> b -. a >= 17. | _ -> false);
             Alcotest.(check int)
               "no queued completion HTTP" 0
               (count e "files.completeUploadExternal");
             Lwt.return_unit)
           (fun () ->
             W.cleanup c;
             Lwt.return_unit)))

let test_receipt_failures () =
  with_scan (fun e ctx owner _ v ->
      S.override e.mock "conversations.history" (fun _ ->
          (200, page [ message "pause SO-1" ]));
      let m =
        {
          Journal.ts = "1700000070.000001";
          thread_ts = "1700000070.000001";
          user_id = "U456";
        }
      in
      S.override e.mock "chat.postMessage" (fun _ ->
          check "outcome durably recorded before send" true
            (Journal.remember e.db e.order m = "complete");
          (500, obj []));
      unwrap (sweep e ctx owner v);
      Alcotest.check S.state_t "notification failures cannot undo pause"
        T.Paused (state e);
      List.iter
        (fun kind ->
          check "ambiguous notification retained" true
            (Journal.notification_status e.db e.order m kind = Some "unknown"))
        [ "pause_notice"; "pause_receipt" ])

let test_pause_failure_retry () =
  with_scan (fun e ctx owner _ v ->
      S.override e.mock "conversations.history" (fun _ ->
          (200, page [ message "pause SO-1" ]));
      Sql.exec e.db
        "CREATE TRIGGER refuse_scan_pause BEFORE UPDATE ON \
         slack_standing_orders WHEN NEW.state='paused' BEGIN SELECT \
         RAISE(ABORT,'injected pause failure'); END"
        [];
      let m =
        {
          Journal.ts = "1700000070.000001";
          thread_ts = "1700000070.000001";
          user_id = "U456";
        }
      in
      let result =
        Lwt_main.run
          (Scan.sweep ~db:e.db ~manager:e.manager ~ctx ~owner ~order:e.order
             ~call:v.Grant.call)
      in
      check "failed pause is typed failure" true
        (match result with Error (Scan.Grant_failure _) -> true | _ -> false);
      Alcotest.check S.state_t "failed write remains active" T.Active (state e);
      check "failed write cannot complete command" true
        (Journal.remember e.db e.order m = "pending");
      check "failed write keeps pause barrier" true (phase e = Some "pause");
      Alcotest.(check int) "no false success receipt" 0 (List.length (posts e));
      Sql.exec e.db "DROP TRIGGER refuse_scan_pause" [];
      unwrap
        (Lwt_main.run
           (Scan.sweep ~db:e.db ~manager:e.manager ~ctx ~owner ~order:e.order
              ~call:v.Grant.call));
      Alcotest.check S.state_t "retry actually commits pause" T.Paused (state e);
      check "retry durably completes command" true
        (Journal.remember e.db e.order m = "complete");
      Alcotest.(check int)
        "one notice and one receipt" 2
        (List.length (posts e)))

let test_rejected_pause_duplicate () =
  with_scan (fun e ctx owner _ v ->
      S.edit_user e.mock.world "U456" (set "is_restricted" (`Bool true));
      let m =
        {
          Journal.ts = "1700000070.000001";
          thread_ts = "1700000070.000001";
          user_id = "U456";
        }
      in
      let run () =
        Lwt_main.run
          (Slack_standing_order_pause.run ~manager:e.manager ~now:(Store.now ())
             ~message:m
             ~replay:
               {
                 Slack_standing_order_pause.call = v.Grant.call;
                 reconcile_id = Scan.reconcile_id owner;
                 snapshot_id = v.snapshot.id;
                 limiter = Option.get ctx.Ctx.limiter;
                 check =
                   (fun () ->
                     R.replay_fenced e.db ~order:e.order ~owner
                       ~now:(Store.now ()));
               }
             ~team_id:"T123" ~app_id:"A123" ~channel_id:"C123" ~user_id:"U456"
             ~target:(`Order e.order.id) ~source:Slack_standing_order_pause.Text
             ())
      in
      ignore v;
      let refused = function
        | Slack_standing_order_pause.Refused Grant.Member_guest -> true
        | _ -> false
      in
      check "guest refused initially" true (refused (run ()));
      check "duplicate refusal never becomes successful pause" true
        (refused (run ()));
      Alcotest.check S.state_t "guest cannot pause" T.Active (state e))

let suite =
  [
    S.case "pause replay shares queued relay pacer and Retry-After"
      test_replay_pacing_and_cooldown;
    S.case "notification failures preserve committed pause outcome"
      test_receipt_failures;
    S.case "transient pause write retries actual public sweep"
      test_pause_failure_retry;
    S.case "duplicate refused pause preserves original outcome"
      test_rejected_pause_duplicate;
  ]
