module S = Test_slack_standing_order_support
module T = Slack_standing_order_types
module Store = Slack_standing_order_store
module Report = Slack_standing_order_report
module Grant = Slack_standing_order_grant
module Ctx = Slack_standing_order_ctx
module SQL = Slack_standing_order_sql
open Lwt.Syntax

let check = Alcotest.(check bool)
let count = Alcotest.(check int)
let text = Alcotest.(check string)
let statuses = Alcotest.(check (list (pair string string)))
let contains = String_util.contains

let with_env ?(config = S.config ()) f =
  Test_helpers.with_temp_home (fun _ ->
      let db = S.fresh_db () in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close db))
        (fun () ->
          let order = S.seed db ~state:T.Suspended () in
          let manager = Session.create ~config () in
          let mock = S.mock () in
          let ctx =
            Ctx.make
              ~accepting:(fun () -> true)
              ~limit:(fun ~channel_id ->
                text "management channel paced" "C123" channel_id;
                Lwt.return_unit)
              ()
          in
          f db order manager mock ctx))

let post db order manager mock ctx ?fire_id cause =
  Report.post_once ~ctx ~db ~manager ~http:mock.S.http ?fire_id order cause

let twice f =
  Lwt_main.run
    (let* () = f () in
     f ())

let received db (order : T.order) = Report.statuses ~db ~order_id:order.id

let check_suspended db (order : T.order) =
  let after = Option.get (Store.find db order.T.id) in
  Alcotest.check S.state_t "suspension preserved" T.Suspended after.state;
  count "epoch preserved" order.epoch after.epoch

let test_once () =
  with_env (fun db o manager mock ctx ->
      S.at 1700006399.;
      twice (fun () -> post db o manager mock ctx Report.No_pickup);
      twice (fun () -> post db o manager mock ctx Report.Cap_hit);
      count "one post per cause" 2 (List.length !(mock.calls));
      S.at 1700006400.;
      twice (fun () -> post db o manager mock ctx Report.Cap_hit);
      count "UTC midnight permits cap" 3 (List.length !(mock.calls));
      twice (fun () -> post db o manager mock ctx Report.No_pickup);
      count "non-cap stays once" 3 (List.length !(mock.calls));
      statuses "all receipts, including daily caps"
        [ ("cap_hit", "sent"); ("cap_hit", "sent"); ("no_pickup", "sent") ]
        (received db o);
      List.iter
        (fun (_, method_, params) ->
          text "only posts" "chat.postMessage" method_;
          text "immutable management channel" "C123" (S.param params "channel");
          text "immutable management thread" "100.000001"
            (S.param params "thread_ts"))
        !(mock.calls);
      check_suspended db o)

let copies =
  Report.
    [
      ( Missing_scope "files.info",
        "Slack permission is missing for files.info; ask an operator to check \
         scopes and reinstall." );
      ( Target_not_member,
        "the target is no longer a channel member; the order is suspended." );
      (No_pickup, "Claude did not acknowledge the relay within 3 minutes.");
      (Not_delivered, "Claude reported that the relay was not delivered.");
      ( Grantor_unauthorised,
        "the grantor no longer has authority; the order is suspended." );
      ( Posture_changed,
        "channel privacy or membership changed; the order is suspended." );
      ( Repeated_failures,
        "3 consecutive deliveries failed; the order is paused." );
      (Cap_hit, "the daily relay cap was reached.");
      ( Transcript_unavailable,
        "transcript unavailable (Slack API change?); the configured notes-only \
         or skip policy applies." );
      ( Reconcile_gap "1700000000.000001-1700003600.000001",
        "reconciliation did not cover 1700000000.000001-1700003600.000001." );
      ( No_notes_hint,
        "no AI notes appeared within 60 minutes; check automatic huddle notes."
      );
      ( Reply_delayed,
        "Claude acknowledged the relay but has not replied within 60 minutes; \
         this is information only." );
      ( Authority_refused Grant.Bot_not_member,
        "authority check refused the relay: clawq is not a member of this \
         channel." );
    ]

let test_copy () =
  with_env (fun _db o _manager _mock _ctx ->
      let o =
        {
          o with
          action =
            S.relay_action
              ~instruction:"SECRET_SLACK_BODY TRANSCRIPT_QUOTE FILE_TITLE"
              ~target:{ S.claude with label = "DISPLAY_NAME" }
              ();
        }
      in
      List.iter
        (fun (cause, suffix) ->
          let out = Report.text o cause in
          text "exact fixed copy" ("<@U123> Standing order SO-1: " ^ suffix) out;
          List.iter
            (fun secret ->
              check "untrusted content absent" false (contains out secret))
            [
              "<@UCLAUDE>";
              "SECRET_SLACK_BODY";
              "TRANSCRIPT_QUOTE";
              "FILE_TITLE";
              "DISPLAY_NAME";
            ])
        copies;
      List.iter
        (fun cause ->
          check "unsafe cause rejected" true
            (try
               ignore (Report.text o cause);
               false
             with Invalid_argument _ -> true))
        Report.
          [
            Missing_scope "<!channel> SECRET";
            Reconcile_gap "private meeting title";
            Authority_refused (Grant.Missing_scope "BODY");
            Authority_refused (Grant.Tool_denied "BODY");
            Authority_refused (Grant.Indeterminate "BODY");
          ])

let test_ambiguity () =
  with_env (fun db o manager mock ctx ->
      let attempts = ref 0 in
      let observed = ref [] in
      let http ~verb:_ ~token:_ ~method_:_ ~params:_ =
        incr attempts;
        observed := received db o;
        Lwt.fail Lwt_unix.Timeout
      in
      let send () =
        Report.post_once ~ctx ~db ~manager ~http o Report.No_pickup
      in
      twice send;
      count "ambiguous never retried" 1 !attempts;
      statuses "reserved before HTTP" [ ("no_pickup", "ambiguous") ] !observed;
      statuses "ambiguity visible"
        [ ("no_pickup", "ambiguous") ]
        (received db o);
      count "no other calls" 0 (List.length !(mock.calls)))

let test_denied () =
  with_env ~config:(S.config ~strict_rules:[] ()) (fun db o manager mock ctx ->
      let _, logs =
        Lwt_main.run
          (S.with_log_capture (fun () ->
               post db o manager mock ctx Report.Target_not_member))
      in
      statuses "refused, never delivered"
        [ ("target_not_member", "refused") ]
        (received db o);
      check "refusal logged" true
        (List.exists (fun (_, msg) -> contains msg "refused") logs);
      List.iter
        (fun (_, msg) ->
          check "no content logged" false (contains msg "Standing order SO-1:"))
        logs;
      count "denied egress zero HTTP" 0 (List.length !(mock.calls));
      check_suspended db o;
      twice (fun () -> post db o manager mock ctx Report.Target_not_member);
      count "refused never retried" 0 (List.length !(mock.calls)))

let test_nonce () =
  with_env (fun db o manager mock ctx ->
      let f = S.fire db o ~room:"R123" () in
      let secret = Result.get_ok (Slack_standing_order_relay.secret db) in
      let nonce =
        Slack_standing_order_relay.nonce ~secret ~order_id:o.id
          ~room_id:f.room_id ~date_start:f.date_start
      in
      SQL.exec db "UPDATE slack_standing_order_fires SET nonce=? WHERE id=?"
        [ SQL.text nonce; SQL.text f.id ];
      check "nonce persisted" true
        ((Option.get (Store.runtime db ~fire_id:f.id)).nonce = Some nonce);
      List.iter
        (fun (cause, _) ->
          check "runtime nonce absent" false
            (contains (Report.text o cause) nonce);
          Lwt_main.run (post db o manager mock ctx ~fire_id:f.id cause))
        copies;
      List.iter
        (fun (_, _, params) ->
          check "nonce absent from posted report" false
            (contains (Yojson.Safe.to_string params) nonce))
        !(mock.calls))

let pending db (o : T.order) cause_key =
  Report.init db;
  SQL.exec db
    "INSERT INTO \
     slack_standing_order_reports(order_id,cause_key,period_key,status) \
     VALUES(?,?,'once','pending')"
    [ SQL.text o.T.id; SQL.text cause_key ]

let test_capability_denies () =
  List.iter
    (fun capability ->
      List.iter
        (fun preexisting ->
          with_env (fun db o manager mock ctx ->
              let cause =
                if preexisting then Report.No_pickup
                else Report.Authority_refused (Grant.Tool_denied capability)
              in
              let key =
                if preexisting then "no_pickup"
                else "authority_refused:tool_denied:" ^ capability
              in
              if preexisting then pending db o key;
              Session.update_config manager
                (S.profile (S.config ()) ~denied:[ capability ] ());
              twice (fun () -> post db o manager mock ctx cause);
              statuses "local only receipt"
                [ (key, "local_only") ]
                (received db o);
              count "zero HTTP including reads" 0 (List.length !(mock.calls));
              count "no snapshot for denied capability" 0
                (SQL.int_query db "SELECT COUNT(*) FROM access_snapshots" []);
              check_suspended db o;
              Session.update_config manager (S.config ());
              twice (fun () -> post db o manager mock ctx cause);
              count "re-enable never sends" 0 (List.length !(mock.calls));
              statuses "stays local only"
                [ (key, "local_only") ]
                (received db o)))
        [ false; true ])
    Grant.capability_names

let test_reload_deny () =
  List.iter
    (fun capability ->
      with_env (fun db o manager mock ctx ->
          pending db o "no_pickup";
          let limits = ref 0 in
          let ctx =
            {
              ctx with
              Ctx.limit =
                (fun ~channel_id:_ ->
                  incr limits;
                  let* () = Lwt.pause () in
                  Session.update_config manager
                    (S.profile (S.config ()) ~denied:[ capability ] ());
                  mock.calls := [];
                  Lwt.return_unit);
            }
          in
          twice (fun () -> post db o manager mock ctx Report.No_pickup);
          count "one limiter await" 1 !limits;
          count "no HTTP after reload" 0 (List.length !(mock.calls));
          statuses "pending becomes local"
            [ ("no_pickup", "local_only") ]
            (received db o);
          check_suspended db o;
          Session.update_config manager (S.config ());
          Lwt_main.run (post db o manager mock ctx Report.No_pickup);
          count "never deferred" 0 (List.length !(mock.calls))))
    Grant.capability_names

let test_tool_denied_cause () =
  List.iter
    (fun capability ->
      with_env (fun db o manager mock ctx ->
          twice (fun () ->
              post db o manager mock ctx
                (Report.Authority_refused (Grant.Tool_denied capability)));
          count "historical tool denial sends nothing after re-enable" 0
            (List.length !(mock.calls));
          count "no standing order snapshot" 0
            (SQL.int_query db "SELECT COUNT(*) FROM access_snapshots" []);
          statuses "historical denial remains local"
            [ ("authority_refused:tool_denied:" ^ capability, "local_only") ]
            (received db o)))
    Grant.capability_names

let test_concurrent () =
  with_env (fun db o manager mock ctx ->
      let ctx = { ctx with Ctx.limit = (fun ~channel_id:_ -> Lwt.pause ()) } in
      Lwt_main.run
        (Lwt.join
           [
             post db o manager mock ctx Report.No_pickup;
             post db o manager mock ctx Report.No_pickup;
           ]);
      count "one concurrent attempt" 1 (List.length !(mock.calls));
      statuses "one sent receipt" [ ("no_pickup", "sent") ] (received db o))

let test_drain () =
  with_env (fun db o manager mock ctx ->
      let accepting = ref true in
      let ctx =
        {
          ctx with
          Ctx.accepting = (fun () -> !accepting);
          limit =
            (fun ~channel_id:_ ->
              let* () = Lwt.pause () in
              accepting := false;
              Lwt.return_unit);
        }
      in
      Lwt_main.run (post db o manager mock ctx Report.No_pickup);
      count "drain prevents write" 0 (List.length !(mock.calls));
      statuses "unattempted remains pending"
        [ ("no_pickup", "pending") ]
        (received db o))

let test_audit_failure () =
  with_env (fun db o manager mock ctx ->
      Sqlite3.Rc.check
        (Sqlite3.exec db
           "CREATE TRIGGER fail_report_audit BEFORE INSERT ON egress_audit \
            BEGIN SELECT RAISE(FAIL, 'fixture'); END");
      twice (fun () -> post db o manager mock ctx Report.No_pickup);
      count "audit failure zero HTTP" 0 (List.length !(mock.calls));
      statuses "audit failure refused"
        [ ("no_pickup", "refused") ]
        (received db o))

let hard_routes =
  Grant.
    [
      (Missing_scope "files.info", "missing_scope:files.info");
      (Target_not_member, "target_not_member");
      (Channel_shared, "posture_changed");
      (Channel_has_guests, "posture_changed");
      (Private_policy_denied, "posture_changed");
      (Grantor_unavailable, "grantor_unauthorised");
      (Grantor_guest, "grantor_unauthorised");
      (Grantor_not_member, "grantor_unauthorised");
      (Grantor_not_allowed, "grantor_unauthorised");
      (Grantor_not_admin, "grantor_unauthorised");
      (Credentials_invalid, "authority_refused:credentials_invalid");
      (Identity_changed, "authority_refused:identity_changed");
      (Target_not_allowlisted, "authority_refused:target_not_allowlisted");
      (Team_mismatch, "authority_refused:team_mismatch");
      (App_mismatch, "authority_refused:app_mismatch");
      (Bot_not_member, "authority_refused:bot_not_member");
      (Target_unavailable, "authority_refused:target_unavailable");
      (Egress_denied "files.info", "authority_refused:egress_denied:files.info");
      (Duplicate_order 2, "authority_refused:duplicate_order:SO-2");
    ]

let test_hard_routes () =
  List.iter
    (fun (reason, key) ->
      with_env (fun db o manager mock ctx ->
          twice (fun () ->
              post db o manager mock ctx (Report.of_hard_reason reason));
          count "one report per hard cause" 1 (List.length !(mock.calls));
          statuses "routed receipt" [ (key, "sent") ] (received db o);
          check_suspended db o))
    hard_routes

let test_transcript () =
  with_env (fun db o manager mock ctx ->
      let send () =
        match Report.of_transcript ~unavailable:true with
        | None -> Alcotest.fail "missing API report"
        | Some cause -> post db o manager mock ctx cause
      in
      twice send;
      count "one API change report" 1 (List.length !(mock.calls));
      statuses "API change receipt"
        [ ("transcript_unavailable", "sent") ]
        (received db o))

let test_no_transcript () =
  with_env (fun db o manager mock ctx ->
      let send () =
        match Report.of_transcript ~unavailable:false with
        | None -> Lwt.return_unit
        | Some cause -> post db o manager mock ctx cause
      in
      twice send;
      count "true notes only no post" 0 (List.length !(mock.calls));
      statuses "no API report" [] (received db o))

let test_no_notes () =
  with_env (fun db o manager mock ctx ->
      let f = S.fire db o ~state:T.No_notes ~room:"R123" () in
      let send () =
        match Report.of_fire f with
        | None -> Alcotest.fail "missing setup hint"
        | Some cause -> post db o manager mock ctx cause
      in
      twice send;
      statuses "one hint" [ ("no_notes_hint", "sent") ] (received db o);
      count "one setup hint" 1 (List.length !(mock.calls)))

let test_success () =
  with_env (fun db o manager mock ctx ->
      List.iter
        (fun state ->
          let f = S.fire db o ~state ~room:(T.fire_state_to_string state) () in
          let send () =
            match Report.of_fire f with
            | None -> Lwt.return_unit
            | Some cause -> post db o manager mock ctx cause
          in
          twice send)
        T.[ Picked_up; Skipped_inactive; Skipped_no_transcript ];
      count "no successful or skipped report" 0 (List.length !(mock.calls));
      statuses "show only" [] (received db o))

let test_show () =
  with_env (fun db o manager mock ctx ->
      let http ~verb:_ ~token:_ ~method_:_ ~params:_ =
        Lwt.fail Lwt_unix.Timeout
      in
      Lwt_main.run (Report.post_once ~ctx ~db ~manager ~http o Report.No_pickup);
      Session.update_config manager (S.config ~strict_rules:[] ());
      Lwt_main.run (post db o manager mock ctx Report.Target_not_member);
      let receipts = received db o in
      let json =
        Slack_standing_order_render.order_detail_json ~reports:receipts o
          ~fires:[]
      in
      let expected =
        `List
          [
            S.obj
              [
                ("cause_key", S.str "no_pickup"); ("status", S.str "ambiguous");
              ];
            S.obj
              [
                ("cause_key", S.str "target_not_member");
                ("status", S.str "refused");
              ];
          ]
      in
      check "show JSON receipts" true
        (Slack_huddles.get "report_statuses" json = expected);
      text "defaults remain compatible" "[]"
        (Yojson.Safe.to_string
           (Slack_huddles.get "report_statuses"
              (Slack_standing_order_render.order_detail_json o ~fires:[]))))

let test_tool_show () =
  let module F = Test_slack_standing_order_fixtures in
  F.with_orders (fun env ->
      let o = F.seed env () in
      pending env.db o "no_pickup";
      SQL.exec env.db
        "UPDATE slack_standing_order_reports SET status='ambiguous' WHERE \
         order_id=?"
        [ SQL.text o.id ];
      let* output =
        F.invoke env "slack_standing_order_show"
          (S.obj [ ("order", S.str "SO-1") ])
      in
      let json = Yojson.Safe.from_string output in
      check "tool show exposes stored attempts" true
        (Slack_huddles.get "report_statuses" json
        = `List
            [
              S.obj
                [
                  ("cause_key", S.str "no_pickup"); ("status", S.str "ambiguous");
                ];
            ]);
      Lwt.return_unit)

let suite =
  [
    S.case "report once per cause and cap once per UTC day" test_once;
    S.case "reports contain ids and enumerated reasons only" test_copy;
    S.case "report failure never retries ambiguity" test_ambiguity;
    S.case "denied report preserves suspension" test_denied;
    S.case "nonce absent from reports" test_nonce;
    S.case "post once capability denies remain local" test_capability_denies;
    S.case "post once reload deny blocks pending report" test_reload_deny;
    S.case "historical tool denial never sends after re-enable"
      test_tool_denied_cause;
    S.case "concurrent reports reserve only once" test_concurrent;
    S.case "drain during limiter prevents report" test_drain;
    S.case "strict audit failure blocks report" test_audit_failure;
    S.case "hard refusal routes once by cause" test_hard_routes;
    S.case "API disappearance reports once" test_transcript;
    S.case "notes only has no API change report" test_no_transcript;
    S.case "no notes hint once" test_no_notes;
    S.case "successful pickup has no report" test_success;
    S.case "show exposes receipts without content" test_show;
    S.case "tool show exposes report statuses" test_tool_show;
  ]
