open Test_slack_standing_order_scan_support
open Lwt.Syntax

let test_incomplete_never_no_notes () =
  with_env (fun e ->
      seen e;
      resolve e;
      let f = one_fire e in
      e.replies := S.ok [ ("messages", `List []); ("has_more", `Bool true) ];
      S.at (ended +. 3601.);
      ignore (fetch e f.id);
      Alcotest.check S.fire_state_t "incomplete read keeps retryable work"
        T.Fetching (runtime e f.id).fire.state)

let suite =
  [
    S.case "incomplete replies never prove no notes"
      test_incomplete_never_no_notes;
  ]

let test_scan_candidate () =
  with_scan (fun e ctx owner token v ->
      S.override e.mock "conversations.history" (fun _ ->
          (200, page [ !(e.root) ]));
      unwrap (sweep e ctx owner v);
      check "pause barrier completed" true (phase e = Some "history");
      unwrap (discover e ctx owner v);
      let fire = one_fire e in
      check "candidate holds correct root" true
        ((runtime e fire.id).root_ts = Some root_ts);
      check "source pinned" true ((runtime e fire.id).canvas_id = Some "FCANVAS");
      check "full discovery complete" true (phase e = Some "swept");
      check "no coverage verified yet" false
        (R.relay_allowed e.db ~fire_id:fire.id ~now:(Store.now ()));
      check "finish metadata scan" true
        (R.finish_scan e.db ~order_id:e.order.id ~owner ~now:(Store.now ()));
      check "no fabricated grant freshness" false
        (R.verify_gap e.db ~token ~owner ~through:"1700000100.000000"
           ~now:(Store.now ()));
      no_upload e)

let test_pause_location location () =
  with_scan
    ~config:(fun c -> S.with_allow_users c [ "U123" ])
    (fun e ctx owner _ v ->
      let p = message "pause SO-1" in
      (match location with
      | "top" ->
          S.override e.mock "conversations.history" (fun _ -> (200, page [ p ]))
      | "ordinary" | "huddle" | "relay" ->
          let root = if location = "huddle" then root_ts else parent_ts in
          if location = "huddle" then seen e;
          (if location = "relay" then
             let fire =
               S.fire e.db e.order ~state:T.Posted ~room:"RRELAY" ~relay_ts:root
                 ~dispatched_at:(Store.now ()) ()
             in
             ignore fire);
          S.override e.mock "conversations.history" (fun _ ->
              ( 200,
                page
                  (if location = "ordinary" then
                     [
                       set "reply_count" (`Int 1)
                         (message ~ts:root "ordinary parent");
                     ]
                   else []) ));
          S.override e.mock "conversations.replies" (fun p ->
              let thread = S.param p "ts" in
              (200, page [ message ~thread "pause SO-1" ]))
      | _ -> assert false);
      unwrap (sweep e ctx owner v);
      Alcotest.check S.state_t "nonallowlisted member paused" T.Paused (state e);
      check "pause closes discovery" true (phase e = Some "pause");
      Alcotest.(check int)
        "notice and late receipt exactly once" 2
        (List.length (posts e));
      List.iter
        (fun p ->
          check "fixed own marker" true
            (Test_helpers.string_contains (S.param p "text") "clawq-pause/1 ");
          check "same channel only" true (S.param p "channel" = "C123"))
        (posts e);
      let audits =
        Egress_audit.query ~db:e.db
          ~tool_name:
            ("standing_order:" ^ e.order.id ^ ":reconcile:"
           ^ Scan.reconcile_id owner)
          ()
      in
      check "actual calls audited under original persisted reconcile snapshot"
        true
        (audits <> []
        && List.for_all
             (fun (a : Egress_audit.event) ->
               a.snapshot_id = Some v.snapshot.id)
             audits);
      check "snapshot has no forged fire" true
        (v.snapshot.standing_order
        = Some { Access_snapshot.order_id = e.order.id; fire_id = None });
      check "unpersisted alternate notifications absent" true
        (List.length (posts e) = 2);
      check "notification recorded" true
        (Journal.notification_status e.db e.order
           {
             Journal.ts = "1700000070.000001";
             thread_ts =
               (if location = "top" then "1700000070.000001"
                else if location = "huddle" then root_ts
                else parent_ts);
             user_id = "U456";
           }
           "pause_receipt"
        = Some "confirmed");
      check "discovery after pause denied" true
        (Result.is_error (discover e ctx owner v)))

let test_pause_reject kind () =
  with_scan (fun e ctx owner _ v ->
      let p =
        message (if kind = "near" then "pause SO-1 please" else "pause SO-1")
      in
      let p =
        match kind with
        | "bot_message" -> set "bot_id" (str "BSPOOF") p
        | "edit" -> set "edited" (obj [ ("user", str "U456") ]) p
        | "nested" ->
            set "text" (str "hello") (set "message" (message "pause SO-1") p)
        | _ -> p
      in
      (match kind with
      | "deleted" ->
          S.edit_user e.mock.world "U456" (set "deleted" (`Bool true))
      | "guest" ->
          S.edit_user e.mock.world "U456" (set "is_restricted" (`Bool true))
      | "nonhuman" ->
          S.edit_user e.mock.world "U456" (set "is_bot" (`Bool true))
      | _ -> ());
      S.override e.mock "conversations.history" (fun _ -> (200, page [ p ]));
      unwrap (sweep e ctx owner v);
      Alcotest.check S.state_t "invalid pause cannot deactivate" T.Active
        (state e);
      check "all safe pages covered" true (phase e = Some "history"))

let test_pause_incomplete kind () =
  with_scan (fun e ctx owner _ v ->
      let p = message "pause SO-1" in
      let response =
        match kind with
        | "truncated" ->
            S.ok [ ("messages", `List [ p ]); ("has_more", `Bool true) ]
        | "limited" -> set "is_limited" (`Bool true) (page [ p ])
        | _ -> S.ok [ ("messages", `String "malformed") ]
      in
      S.override e.mock "conversations.history" (fun _ -> (200, response));
      check "incomplete sweep refused" true
        (Result.is_error (sweep e ctx owner v));
      Alcotest.check S.state_t "no partial replay" T.Active (state e);
      check "barrier remains closed" true (phase e = Some "pause");
      check "no success watermark" true
        ((Option.get (R.load_coverage e.db ~order_id:e.order.id)).last_success
       = None))

let second_root = "1700000002.000100"

let test_partial_root kind () =
  with_scan (fun e ctx owner _ v ->
      unwrap (sweep e ctx owner v);
      let second =
        set "ts" (str second_root)
          (set "room"
             (set "id" (str "RSECOND") (get "room" !(e.root)))
             !(e.root))
      in
      S.override e.mock "conversations.history" (fun p ->
          if get "limit" p <> `Int 1 then (200, page [ !(e.root); second ])
          else if S.param p "latest" = root_ts then (200, page [ !(e.root) ])
          else if kind = "429" then (429, obj [])
          else if kind = "500" then (500, obj [])
          else if kind = "transport" then (200, `String "malformed")
          else if kind = "wrong" then (200, page [ !(e.root) ])
          else if kind = "limited" then
            (200, set "is_limited" (`Bool true) (page [ second ]))
          else
            ( 200,
              S.ok [ ("messages", `List [ second ]); ("has_more", `Bool true) ]
            ));
      let result = discover e ctx owner v in
      check "second root typed failure" true (Result.is_error result);
      Alcotest.(check int)
        "first candidate survives partial page" 1
        (List.length (fires e));
      check "containing history page incomplete" true (phase e = Some "history");
      let c = Option.get (R.load_coverage e.db ~order_id:e.order.id) in
      check "page cursor and success do not advance" true
        (c.history_cursor = "" && c.last_success = None);
      let fire = one_fire e in
      check "no partial release" false
        (R.relay_allowed e.db ~fire_id:fire.id ~now:(Store.now ())))

let test_missing_scope method_ () =
  with_scan (fun e ctx owner _ v ->
      unwrap (sweep e ctx owner v);
      S.override e.mock "conversations.history" (fun _ ->
          (200, page [ !(e.root) ]));
      S.override e.mock method_ (fun _ ->
          (200, obj [ ("ok", `Bool false); ("error", str "missing_scope") ]));
      let result = discover e ctx owner v in
      check "method-specific hard scope failure" true
        (match result with
        | Error (Scan.Grant_failure { reason = Grant.Missing_scope m; _ })
        | Error
            (Scan.Root_failure
               (Fetch.Authority { reason = Grant.Missing_scope m; _ })) ->
            m = method_
        | _ -> false);
      if method_ <> "conversations.history" then
        Alcotest.check S.state_t "typed root authority suspends" T.Suspended
          (state e);
      check "no success coverage" true
        ((Option.get (R.load_coverage e.db ~order_id:e.order.id)).last_success
       = None))

let test_long_live () =
  with_scan (fun e ctx owner _ v ->
      let start = 1699700000 in
      let old_root = "1699700000.000001" in
      e.root :=
        set "ts" (str old_root)
          (set "room"
             (set "thread_root_ts" (str old_root)
                (set "canvas_thread_ts" (str old_root)
                   (set "date_start" (`Int start)
                      (set "date_end" `Null
                         (set "has_ended" (`Bool false) (get "room" !(e.root)))))))
             !(e.root));
      e.replies := page [ !(e.root) ];
      L.record_seen ~db:e.db ~team:"T123" ~channel:"C123" ~source:"event"
        ~now:(Store.now ()) ~root_ts:old_root ~file_ids:[]
        (Option.get (Slack_huddle_classify.room_of_json (get "room" !(e.root))));
      unwrap (sweep e ctx owner v);
      unwrap (discover e ctx owner v);
      let row = List.hd (L.list_rows ~db:e.db ~team:"T123" ~channel:"C123") in
      check "long live row not finalized by age" false row.finalised;
      Alcotest.(check int) "live huddle no fire" 0 (List.length (fires e)))

let test_missing_exact kind () =
  with_scan (fun e ctx owner _ v ->
      seen e;
      unwrap (sweep e ctx owner v);
      S.override e.mock "conversations.history" (fun p ->
          ( 200,
            page
              (if get "limit" p = `Int 1 && kind = "neighbor" then
                 [ set "ts" (str parent_ts) !(e.root) ]
               else []) ));
      let result = discover e ctx owner v in
      check "complete exact absence versus neighbor" (kind = "absent")
        (Result.is_ok result);
      check "nothing relayed" true (fires e = []))

let test_epoch () =
  with_scan (fun e ctx owner _ v ->
      ignore
        (unwrap
           (Store.deactivate e.db ~order_id:e.order.id (Store.Pause "U123")
              ~now:(Store.now ())));
      let before = List.length !(e.mock.calls) in
      check "old epoch cannot scan" true (Result.is_error (sweep e ctx owner v));
      Alcotest.(check int)
        "stale owner zero HTTP" before
        (List.length !(e.mock.calls)))

let test_indeterminate_member () =
  with_scan (fun e ctx owner _ v ->
      S.override e.mock "conversations.history" (fun _ ->
          (200, page [ message "pause SO-1" ]));
      S.override e.mock "users.info" (fun _ -> (500, obj []));
      check "membership uncertainty blocks sweep" true
        (Result.is_error (sweep e ctx owner v));
      check "barrier not completed" true (phase e = Some "pause");
      Alcotest.check S.state_t "pause not falsely claimed" T.Active (state e))

let suite =
  suite
  @ [
      S.case "scan discovers held source-bound candidate" test_scan_candidate;
      S.case "live huddle older than 48 hours never times out" test_long_live;
      S.case "confirmed missing exact root" (test_missing_exact "absent");
      S.case "neighbor never proves root absent" (test_missing_exact "neighbor");
      S.case "stale epoch blocks all scan HTTP" test_epoch;
      S.case "membership uncertainty keeps barrier closed"
        test_indeterminate_member;
    ]
  @ List.map
      (fun kind -> S.case ("pause in " ^ kind) (test_pause_location kind))
      [ "top"; "ordinary"; "huddle"; "relay" ]
  @ List.map
      (fun kind -> S.case ("reject pause " ^ kind) (test_pause_reject kind))
      [
        "near"; "bot_message"; "edit"; "nested"; "deleted"; "guest"; "nonhuman";
      ]
  @ List.map
      (fun kind ->
        S.case ("incomplete pause " ^ kind) (test_pause_incomplete kind))
      [ "truncated"; "limited"; "malformed" ]
  @ List.map
      (fun kind ->
        S.case ("partial candidate page " ^ kind) (test_partial_root kind))
      [ "429"; "500"; "transport"; "wrong"; "limited"; "truncated" ]
  @ List.map
      (fun method_ ->
        S.case ("scope failure " ^ method_) (test_missing_scope method_))
      [
        "conversations.history";
        "conversations.replies";
        "files.info";
        "canvases.getContent";
      ]

let test_thread_page_failure () =
  with_scan (fun e ctx owner _ v ->
      S.override e.mock "conversations.history" (fun _ ->
          ( 200,
            page [ set "reply_count" (`Int 1) (message ~ts:parent_ts "parent") ]
          ));
      S.override e.mock "conversations.replies" (fun _ ->
          ( 200,
            S.ok
              [
                ("messages", `List [ message ~thread:parent_ts "pause SO-1" ]);
                ("has_more", `Bool true);
              ] ));
      check "truncated thread blocks whole sweep" true
        (Result.is_error (sweep e ctx owner v));
      Alcotest.check S.state_t "collected commands not prematurely replayed"
        T.Active (state e);
      S.override e.mock "conversations.replies" (fun _ ->
          (200, page [ message ~thread:parent_ts "pause SO-1" ]));
      unwrap (sweep e ctx owner v);
      Alcotest.check S.state_t "retry applies exactly once" T.Paused (state e);
      Alcotest.(check int) "one notice one receipt" 2 (List.length (posts e)))

let test_pause_order () =
  with_scan (fun e ctx owner _ v ->
      let later =
        message ~user:"U456" ~ts:"01700000070.10000000000000002" "pause SO-1"
      in
      let earlier =
        message ~user:"U123" ~ts:"1700000070.10000000000000001" "pause SO-1"
      in
      S.override e.mock "conversations.history" (fun _ ->
          (200, page [ later; earlier ]));
      unwrap (sweep e ctx owner v);
      let o = Option.get (Store.find e.db e.order.id) in
      check "decimal ordering preserves sub-float precision" true
        (o.paused_by = Some "U123");
      Alcotest.(check int)
        "two receipts no redundant grantor notice" 2
        (List.length (posts e)))

let test_downtime_bound () =
  with_scan (fun e ctx owner _ v ->
      let historical = message ~ts:"1699999999.000001" "pause SO-1" in
      S.override e.mock "conversations.history" (fun p ->
          check "pause window predates reset activation" true
            (Slack_standing_order_provenance.compare_ts (S.param p "oldest")
               (Option.get e.order.active_since)
            < 0);
          (200, page [ historical ]));
      unwrap (sweep e ctx owner v);
      Alcotest.check S.state_t "downtime pause before activation not lost"
        T.Paused (state e))

let test_duplicate_cursor () =
  with_scan (fun e ctx owner _ v ->
      S.override e.mock "conversations.history" (fun _ ->
          ( 200,
            set "response_metadata"
              (obj [ ("next_cursor", str "duplicate") ])
              (set "has_more" (`Bool true) (page [])) ));
      check "cursor loop typed incomplete" true
        (match sweep e ctx owner v with
        | Error (Scan.Page_incomplete { code = Pages.Repeated_cursor; _ }) ->
            true
        | _ -> false);
      check "no successful watermark" true
        ((Option.get (R.load_coverage e.db ~order_id:e.order.id)).last_success
       = None))

let test_no_notes_once () =
  with_scan ~at_time:1700004000. (fun e ctx owner _ v ->
      S.override e.mock "conversations.history" (fun _ ->
          (200, page [ !(e.root) ]));
      e.replies := page [ !(e.root) ];
      unwrap (sweep e ctx owner v);
      unwrap (discover e ctx owner v);
      check "confirmed ended empty notes finalised" true
        (List.for_all
           (fun (r : L.row) -> r.finalised)
           (L.list_rows ~db:e.db ~team:"T123" ~channel:"C123"));
      check "no relay created" true (fires e = []))

let test_stub_hydration () =
  with_scan (fun e ctx owner _ v ->
      ready e ();
      unwrap (sweep e ctx owner v);
      unwrap (discover e ctx owner v);
      let fire = one_fire e in
      check "ready stub resolved by pinned root" true
        ((runtime e fire.id).canvas_id = Some "FCANVAS"))

let test_unrelated_canvas () =
  with_scan (fun e ctx owner _ v ->
      seen e;
      e.replies :=
        page
          [
            message ~thread:root_ts "notes"
            |> set "files" (`List [ obj [ ("id", str "FCANVAS") ] ]);
          ];
      unwrap (sweep e ctx owner v);
      unwrap (discover e ctx owner v);
      check "human attachment not candidate" true (fires e = []))

let test_reused_room () =
  with_scan (fun e ctx owner _ v ->
      let old = !(e.root) in
      let newer =
        set "ts" (str second_root)
          (set "room" (set "date_start" (`Int 1700000002) (get "room" old)) old)
      in
      let other_canvas = set "id" (str "FNEW") !(e.canvas) in
      let newer =
        set "files"
          (`List [ obj [ ("id", str "FNEW") ] ])
          (set "room"
             (set "transcript_file_id" (str "FNEW")
                (set "thread_root_ts" (str second_root)
                   (set "canvas_thread_ts" (str second_root) (get "room" newer))))
             newer)
      in
      S.override e.mock "conversations.history" (fun p ->
          if get "limit" p = `Int 1 then
            (200, page [ (if S.param p "latest" = root_ts then old else newer) ])
          else (200, page [ newer; old ]));
      S.override e.mock "conversations.replies" (fun p ->
          let root = S.param p "ts" in
          let ready =
            F.ready
            |> set "thread_ts" (str root)
            |> set "files"
                 (`List
                    [
                      obj
                        [
                          ( "id",
                            str (if root = root_ts then "FCANVAS" else "FNEW")
                          );
                        ];
                    ])
          in
          (200, page [ (if root = root_ts then old else newer); ready ]));
      S.override e.mock "files.info" (fun p ->
          ( 200,
            S.ok
              [
                ( "file",
                  if S.param p "file" = "FNEW" then
                    set "shares"
                      (obj
                         [
                           ( "private",
                             obj [ ("C123", `List [ F.share second_root ]) ] );
                         ])
                      other_canvas
                  else !(e.canvas) );
              ] ));
      unwrap (sweep e ctx owner v);
      unwrap (discover e ctx owner v);
      Alcotest.(check int) "both room uses retained" 2 (List.length (fires e));
      let rows = L.list_rows ~db:e.db ~team:"T123" ~channel:"C123" in
      check "late old root cannot overwrite new ledger instance" true
        (List.exists
           (fun (r : L.row) ->
             r.date_start = Some 1700000002 && r.root_ts = Some second_root)
           rows);
      let old_fire =
        List.find (fun (f : T.fire) -> f.date_start = "1700000001") (fires e)
      in
      check "old fire retains old canvas" true
        ((runtime e old_fire.id).canvas_id = Some "FCANVAS"))

let test_local_capability_denial () =
  with_scan (fun e ctx owner _ v ->
      let cfg = Session.get_config e.manager in
      Session.update_config e.manager
        { cfg with security = { cfg.security with tools_enabled = false } };
      let before = List.length !(e.mock.calls) in
      check "denial closes scan" true (Result.is_error (sweep e ctx owner v));
      Alcotest.(check int)
        "zero prohibited HTTP" before
        (List.length !(e.mock.calls)))

let test_postcommit_revocation ~egress () =
  with_scan (fun e ctx owner _ v ->
      S.override e.mock "conversations.history" (fun _ ->
          (200, page [ message "pause SO-1" ]));
      let http ~verb ~token ~method_ ~params =
        let* response = e.mock.http ~verb ~token ~method_ ~params in
        (if method_ = "chat.postMessage" then
           let cfg = Session.get_config e.manager in
           Session.update_config e.manager
             (if egress then
                { cfg with egress = { cfg.egress with default_allowlist = [] } }
              else
                {
                  cfg with
                  security = { cfg.security with tools_enabled = false };
                }));
        Lwt.return response
      in
      (* Reconstruct at the authorized factory with the SAME persisted snapshot and limiter. *)
      let call =
        SO.make ~db:e.db
          ~current_config:(fun () -> Session.get_config e.manager)
          ~snapshot:v.snapshot ~channel_id:e.order.channel_id
          ~team_id:e.order.team_id ~app_id:e.order.api_app_id
          ~capability_fence:true ?limiter:ctx.Ctx.limiter ~http
          ~audit:
            (SO.audit_label ~order_id:(Some e.order.id)
               ~unit_label:"reconcile:revoke")
          ()
      in
      let v = { v with Grant.call } in
      unwrap (sweep e ctx owner v);
      Alcotest.check S.state_t "pause survives notification revocation" T.Paused
        (state e);
      Alcotest.(check int)
        "only visible notice before revoke" 1
        (List.length (posts e));
      let m =
        {
          Journal.ts = "1700000070.000001";
          thread_ts = "1700000070.000001";
          user_id = "U456";
        }
      in
      check "receipt terminal local only" true
        (Journal.notification_status e.db e.order m "pause_receipt"
        = Some (if egress then "unknown" else "local_only"));
      Session.update_config e.manager (S.config ());
      Lwt_main.run
        (Slack_standing_order_pause.post_receipt ~manager:e.manager
           ~order:e.order ~message:m ~channel_id:"C123" ~thread_ts:m.thread_ts
           "fixed receipt");
      Alcotest.(check int)
        "re-enable does not auto-send local-only receipt" 1
        (List.length (posts e)))

let suite =
  suite
  @ [
      S.case "thread retry never replays partial sweep" test_thread_page_failure;
      S.case "pause order uses exact decimal timestamps" test_pause_order;
      S.case "pause downtime bound independent of activation"
        test_downtime_bound;
      S.case "repeated cursor is incomplete" test_duplicate_cursor;
      S.case "confirmed no notes timeout" test_no_notes_once;
      S.case "ready stub hydration" test_stub_hydration;
      S.case "unrelated canvas unavailable" test_unrelated_canvas;
      S.case "reused room pinned independently" test_reused_room;
      S.case "capability denial zero HTTP" test_local_capability_denial;
      S.case "postcommit revocation keeps pause local"
        (test_postcommit_revocation ~egress:false);
    ]

let test_notification_crash_reopen () =
  with_scan (fun e ctx owner _ v ->
      S.override e.mock "conversations.history" (fun _ ->
          (200, page [ message "pause SO-1" ]));
      let http ~verb ~token ~method_ ~params =
        let* response = e.mock.http ~verb ~token ~method_ ~params in
        if method_ = "chat.postMessage" then Lwt.fail Lwt.Canceled
        else Lwt.return response
      in
      let call =
        SO.make ~db:e.db
          ~current_config:(fun () -> Session.get_config e.manager)
          ~snapshot:v.snapshot ~channel_id:"C123" ~capability_fence:true
          ?limiter:ctx.Ctx.limiter ~http
          ~audit:
            (SO.audit_label ~order_id:(Some e.order.id)
               ~unit_label:"reconcile:crash")
          ()
      in
      let v = { v with Grant.call } in
      Lwt_main.run
        (Lwt.catch
           (fun () ->
             let* _ =
               Scan.sweep_with_grant ~db:e.db ~manager:e.manager ~ctx ~owner
                 ~order:e.order ~validated:v
             in
             Alcotest.fail "expected process cancellation")
           (function Lwt.Canceled -> Lwt.return_unit | exn -> Lwt.fail exn));
      Alcotest.check S.state_t "pause committed before crash" T.Paused (state e);
      Alcotest.(check int)
        "one notification escaped before crash" 1
        (List.length (posts e));
      let path = Filename.temp_file "pause-reopen" ".db" in
      Fun.protect
        ~finally:(fun () -> Sys.remove path)
        (fun () ->
          Sql.exec e.db "VACUUM INTO ?" [ Sql.text path ];
          let db = Sqlite3.db_open path in
          Fun.protect
            ~finally:(fun () -> ignore (Sqlite3.db_close db))
            (fun () ->
              let manager =
                Session.create ~config:(Session.get_config e.manager) ~db ()
              in
              let m =
                {
                  Journal.ts = "1700000070.000001";
                  thread_ts = "1700000070.000001";
                  user_id = "U456";
                }
              in
              check "ambiguous send remains reserved on reopen" true
                (Journal.notification_status db e.order m "pause_notice"
                = Some "attempted");
              let before = List.length (posts e) in
              let result =
                Lwt_main.run
                  (Slack_standing_order_pause.run ~manager ~message:m
                     ~team_id:"T123" ~app_id:"A123" ~channel_id:"C123"
                     ~user_id:"U456" ~target:(`Order e.order.id)
                     ~source:Slack_standing_order_pause.Text ())
              in
              check "durable command dedup avoids repeat notice" true
                (match result with
                | Slack_standing_order_pause.Already_paused _ -> true
                | _ -> false);
              Alcotest.(check int)
                "no duplicate HTTP after reopen" before
                (List.length (posts e)))))

let test_event_sweep_dedup () =
  let module F = Test_slack_standing_order_fixtures in
  F.with_orders (fun env ->
      let o = F.seed env () in
      let route =
        {
          Slack_thread_route.team = "T123";
          app = "A123";
          channel = "C123";
          root = "1700000070.000001";
        }
      in
      let* _ =
        Slack_standing_order_pause.handle_text ~manager:env.base.manager ~route
          ~user:"U456" ~ts:"1700000070.000001" "pause SO-1"
      in
      let before = List.length !(env.posted) in
      let* _ =
        Slack_standing_order_pause.handle_text ~manager:env.base.manager ~route
          ~user:"U456" ~ts:"1700000070.000001" "pause SO-1"
      in
      Alcotest.(check int)
        "durable identity dedups repeated event/receipt" before
        (List.length !(env.posted));
      check "sweep has no pending event command" true
        (Journal.messages env.db o = []);
      Lwt.return_unit)

let test_member_malformed_page () =
  with_scan (fun e ctx owner _ v ->
      S.override e.mock "conversations.history" (fun _ ->
          (200, page [ message "pause SO-1" ]));
      S.override e.mock "conversations.members" (fun _ ->
          (200, S.ok [ ("members", `List []); ("has_more", `Bool true) ]));
      check "indeterminate membership cannot certify coverage" true
        (Result.is_error (sweep e ctx owner v));
      check "no pause certificate" true (phase e = Some "pause"))

let test_source_malformed method_ () =
  with_scan (fun e ctx owner _ v ->
      seen e;
      unwrap (sweep e ctx owner v);
      S.override e.mock method_ (fun p ->
          if method_ = "conversations.replies" then
            (200, set "is_limited" (`Bool true) (page []))
          else if method_ = "files.info" then
            (200, S.ok [ ("file", `String "malformed") ])
          else if method_ = "canvases.getContent" then
            (200, S.ok [ ("content", `List []) ])
          else if get "limit" p = `Int 1 then
            (200, S.ok [ ("messages", `List []) ])
          else (200, page []));
      check "malformed source is incomplete not unavailable" true
        (match discover e ctx owner v with
        | Error (Scan.Root_failure (Fetch.Incomplete { method_ = m; _ })) ->
            m = method_
        | _ -> false);
      check "coverage not completed" true (phase e = Some "history"))

let test_fire_insert_failure () =
  with_scan (fun e ctx owner _ v ->
      seen e;
      unwrap (sweep e ctx owner v);
      Sql.exec e.db
        "CREATE TRIGGER refuse_scan_fire BEFORE INSERT ON \
         slack_standing_order_fires BEGIN SELECT RAISE(ABORT,'injected'); END"
        [];
      check "fire insertion failure propagates" true
        (match discover e ctx owner v with
        | Error
            (Scan.Root_failure
               (Fetch.Incomplete { code = Pages.Store_failed; _ })) ->
            true
        | _ -> false);
      check "no false successful page" true (phase e = Some "history"))

let test_replies_paged_ready () =
  with_scan (fun e ctx owner _ v ->
      seen e;
      unwrap (sweep e ctx owner v);
      S.override e.mock "conversations.replies" (fun p ->
          if S.param p "cursor" = "" then
            ( 200,
              set "has_more" (`Bool true)
                (set "response_metadata"
                   (obj [ ("next_cursor", str "hydration") ])
                   (page [ !(e.root) ])) )
          else (200, set "ok" (`Bool true) !(e.replies)));
      unwrap (discover e ctx owner v);
      Alcotest.(check int)
        "ready hydration on second page creates once" 1
        (List.length (fires e)))

let test_postwait_fence kind () =
  with_scan (fun e oldctx owner _ v ->
      let module W = Test_slack_standing_order_worker in
      let c = W.clock () in
      c.now <- Store.now ();
      let limiter =
        Slack_read_limiter.create_with ~now:(fun () -> c.now) ~wait:(W.wait c)
      in
      let ctx =
        { oldctx with Ctx.limiter = Some limiter; now = (fun () -> c.now) }
      in
      let call =
        SO.make ~db:e.db
          ~current_config:(fun () -> Session.get_config e.manager)
          ~snapshot:v.snapshot ~channel_id:"C123" ~capability_fence:true
          ~team_id:"T123" ~app_id:"A123" ~limiter ~http:e.mock.http
          ~audit:
            (SO.audit_label ~order_id:(Some e.order.id)
               ~unit_label:"reconcile:wait")
          ()
      in
      let v = { v with Grant.call } in
      Slack_read_limiter.note_retry_after limiter
        ~key:"T123:A123:conversations.history" 17.;
      Lwt_main.run
        (Lwt.finalize
           (fun () ->
             let work =
               Scan.sweep_with_grant ~db:e.db ~manager:e.manager ~ctx ~owner
                 ~order:e.order ~validated:v
             in
             let* () = W.flush 20 in
             let before = List.length !(e.mock.calls) in
             check "scan waiting at real shared transport" true
               (Lwt.is_sleeping work);
             (match kind with
             | "gap" ->
                 ignore
                   (unwrap
                      (R.close_gap e.db ~team_id:"T123" ~app_id:"A123"
                         ~channel_id:"C123" ~cause:"disconnect" ~now:c.now))
             | "lease" -> c.now <- c.now +. 121.
             | _ -> Session.update_config e.manager (S.config ()));
             let* () = W.advance c 17. in
             let* result = work in
             check "post-wait fence refuses scan" true (Result.is_error result);
             Alcotest.(check int)
               "no stale HTTP" before
               (List.length !(e.mock.calls));
             Lwt.return_unit)
           (fun () ->
             W.cleanup c;
             Lwt.return_unit)))

let suite =
  suite
  @ [
      S.case "notice crash and DB reopen no duplicate"
        test_notification_crash_reopen;
      S.case "event and sweep share durable pause identity"
        test_event_sweep_dedup;
      S.case "malformed membership blocks barrier" test_member_malformed_page;
      S.case "fire insert failure is typed" test_fire_insert_failure;
      S.case "late hydration after paginated replies" test_replies_paged_ready;
    ]
  @ List.map
      (fun m -> S.case ("malformed source " ^ m) (test_source_malformed m))
      [ "conversations.replies"; "files.info"; "canvases.getContent" ]
  @ List.map
      (fun kind ->
        S.case ("transport wait loses " ^ kind) (test_postwait_fence kind))
      [ "gap"; "lease"; "config" ]

let test_partial_page_resume () =
  with_scan (fun e ctx owner _ v ->
      unwrap (sweep e ctx owner v);
      let failed = ref true in
      let second =
        !(e.root)
        |> set "ts" (str second_root)
        |> set "room"
             (get "room" !(e.root)
             |> set "id" (str "RSECOND")
             |> set "has_ended" (`Bool false)
             |> set "date_end" `Null)
      in
      S.override e.mock "conversations.history" (fun p ->
          if get "limit" p <> `Int 1 then (200, page [ !(e.root); second ])
          else if S.param p "latest" = root_ts then (200, page [ !(e.root) ])
          else if !failed then (500, obj [])
          else (200, page [ second ]));
      check "partial page fails" true (Result.is_error (discover e ctx owner v));
      let first_reads = count e "files.info" in
      let failure_at = Store.now () in
      S.at (failure_at +. 90.);
      check "owner renews before expiry" true
        (R.renew e.db ~order_id:e.order.id ~owner ~now:(Store.now ()));
      S.at (failure_at +. 121.);
      failed := false;
      unwrap (discover e ctx owner v);
      Alcotest.(check int)
        "completed candidate not refetched on page retry" first_reads
        (count e "files.info");
      Alcotest.(check int) "candidate remains unique" 1 (List.length (fires e));
      Alcotest.(check int)
        "one actual grant for entire pass" 1 (count e "auth.test");
      check "resumed page now complete" true (phase e = Some "swept"))

let test_candidate_before_schedule_crash () =
  with_scan (fun e ctx owner _ v ->
      let room =
        Option.get (Slack_huddle_classify.room_of_json (get "room" !(e.root)))
      in
      ignore
        (Slack_standing_order_resolution.record ~db:e.db ~order:e.order
           ~root:root_ts room ~now:(Store.now ()));
      check "crash before ledger update" true
        (L.list_rows ~db:e.db ~team:"T123" ~channel:"C123" = []);
      unwrap (sweep e ctx owner v);
      unwrap (discover e ctx owner v);
      let fire = one_fire e in
      check "pinned durable root recovered without ledger" true
        ((runtime e fire.id).root_ts = Some root_ts))

let test_before_activation () =
  with_scan (fun e ctx owner _ v ->
      e.root :=
        set "room"
          (set "date_start" (`Int 1699999999) (get "room" !(e.root)))
          !(e.root);
      seen e;
      unwrap (sweep e ctx owner v);
      unwrap (discover e ctx owner v);
      check "old instance deliberately ineligible" true
        (Scan_store.previous_outcome e.db e.order root_ts = Some "ineligible");
      check "ineligible inspection completes page" true (phase e = Some "swept");
      check "no pre-activation fire" true (fires e = []);
      Alcotest.(check int)
        "no content reads for ineligible instance" 0 (count e "files.info"))

let suite =
  suite
  @ [
      S.case "partial candidate page resumes completed units"
        test_partial_page_resume;
    ]
  @ Test_slack_standing_order_scan_replay.suite
  @ [
      S.case "durable candidate survives pre-schedule crash"
        test_candidate_before_schedule_crash;
      S.case "deliberate ineligibility is successful inspection"
        test_before_activation;
      S.case "postcommit egress revocation cannot undo pause"
        (test_postcommit_revocation ~egress:true);
    ]

let test_pause_scope_failure () =
  with_scan (fun e ctx owner _ v ->
      S.override e.mock "conversations.history" (fun _ ->
          (200, page [ message "pause SO-1" ]));
      S.override e.mock "users.info" (fun _ ->
          (200, obj [ ("ok", `Bool false); ("error", str "missing_scope") ]));
      check "membership keeps hard method-specific scope refusal" true
        (match sweep e ctx owner v with
        | Error
            (Scan.Grant_failure
               { cls = Grant.Hard; reason = Grant.Missing_scope "users.info" })
          ->
            true
        | _ -> false);
      check "membership scope failure keeps barrier" true
        (phase e = Some "pause"))

let suite =
  suite
  @ [
      S.case "pause membership preserves missing_scope class"
        test_pause_scope_failure;
    ]
