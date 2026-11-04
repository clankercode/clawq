module E = Slack_standing_order_exec
open Test_slack_standing_order_exec_support
module G = Slack_standing_order_grant

let check = Alcotest.(check bool)

let test_stub () =
  with_env (fun e ->
      ready e ();
      e.canvas :=
        set "canvas_metadata"
          (obj [ ("room_id", str "RWRONG"); ("date_start", `Int 1) ])
          !(e.canvas);
      resolve e;
      resolve e;
      let f = one_fire e in
      Alcotest.(check string) "root supplies room" "R123" f.room_id;
      Alcotest.(check string)
        "root supplies start" (string_of_int start) f.date_start;
      check "root persisted before fetch" true
        ((runtime e f.id).root_ts = Some root_ts);
      check "all history reads pinned" true
        (List.for_all
           (fun (_, m, p) ->
             m <> "conversations.history"
             || get "oldest" p = str root_ts
                && get "latest" p = str root_ts
                && get "inclusive" p = `Bool true
                && get "limit" p = `Int 1)
           !(e.mock.calls));
      Alcotest.(check int)
        "resolve does not fetch canvas metadata" 0 (count e "files.info");
      check "worker pacing used" true (!(e.limits) > 0);
      no_upload e)

let test_eligibility () =
  with_env (fun e ->
      let order = { e.order with active_since = Some "999999999.999999" } in
      let eligible ?(order = order) ?(date_start = 1000000000)
          ?(ts = "1000000001.1") ?(epoch = order.epoch) () =
        E.eligible ~order ~date_start ~notes_ready_ts:ts ~fire_epoch:epoch
      in
      check "digit boundary eligible" true (eligible ());
      check "reverse boundary" false
        (eligible
           ~order:{ order with active_since = Some "1000000000.000001" }
           ~date_start:999999999 ());
      List.iter
        (fun active_since ->
          check "malformed or absent activation" false
            (eligible ~order:{ order with active_since } ()))
        [
          None;
          Some "";
          Some "bogus";
          Some "nan";
          Some "100.1.2";
          Some ".1";
          Some "100.";
        ];
      check "confirm or resume during huddle" false
        (eligible
           ~order:{ order with active_since = Some "1000000000.000001" }
           ());
      check "notes before activation" false (eligible ~ts:"999999999.999998" ());
      check "malformed notes" false (eligible ~ts:"broken" ());
      check "epoch mismatch" false (eligible ~epoch:(order.epoch - 1) ()))

let test_upgrade () =
  with_env (fun e ->
      seen e;
      ready e ();
      resolve e;
      let id = (one_fire e).id in
      let first = Option.get (fetch e id) in
      check "initial notes only" true (first.transcript = None);
      let later = "1700000070.000100" in
      ready e ~ts:later ();
      e.replies :=
        F.page
          [
            !(e.root);
            set "ts" (str later) (set "thread_ts" (str root_ts) F.ready);
          ];
      enable_transcript e;
      let second = Option.get (fetch e id) in
      check "upgraded payload" true (second.transcript <> None);
      check "latest ready retained" true
        ((runtime e id).notes_ready_ts = Some later);
      with_claim e id (fun lease r ->
          let after =
            {
              r with
              fire = { r.fire with state = T.Uploading };
              attempts =
                [
                  {
                    (Store.empty_attempt 0) with
                    uploaded_file_ids = [ "FPREPARED" ];
                    upload_started_at = Some (Store.now ());
                  };
                ];
            }
          in
          check "upload transition" true
            (Store.cas e.db ~lease ~before:r ~after ~now:(Store.now ())
            = Ok true);
          ());
      let latest = "1700000080.000100" in
      with_claim e id (fun lease r ->
          check "CAS refuses changing source while retaining stale uploads" true
            (Result.is_error
               (Store.cas e.db ~lease ~before:r
                  ~after:{ r with transcript_id = Some "FSTALE" }
                  ~now:(Store.now ()))));
      ready e ~ts:latest ();
      e.replies :=
        F.page
          [
            !(e.root);
            set "ts" (str latest) (set "thread_ts" (str root_ts) F.ready);
          ];
      e.canvas := set "huddle_transcript_file_id" (str "FNEW") !(e.canvas);
      e.transcript := set "id" (str "FNEW") !(e.transcript);
      ignore (Option.get (fetch e id));
      check "new transcript replaces previous source before dispatch" true
        ((runtime e id).transcript_id = Some "FNEW");
      check "new ready invalidates prepared uploads" true
        (List.for_all
           (fun (a : Store.relay_attempt) ->
             a.uploaded_file_ids = [] && a.upload_started_at = None)
           (runtime e id).attempts);
      with_claim e id (fun lease _ ->
          check "one reservation" true
            (Store.reserve_dispatch e.db ~fire_id:id ~lease ~repost:false
               ~now:(Store.now ())
            = Ok true));
      let before = runtime e id in
      ready e ~ts:"1700000090.000100" ();
      resolve e;
      let calls = List.length !(e.mock.calls) in
      check "posting has no fetch" true (fetch e id = None);
      check "source ids frozen" true
        ((runtime e id).transcript_id = before.transcript_id
        && (runtime e id).notes_ready_ts = before.notes_ready_ts);
      with_claim e id (fun lease r ->
          check "CAS refuses source replacement after posting" true
            (Result.is_error
               (Store.cas e.db ~lease ~before:r
                  ~after:{ r with transcript_id = Some "FTOOLATE" }
                  ~now:(Store.now ()))));
      Alcotest.(check int) "still one fire" 1 (List.length (fires e));
      Alcotest.(check int)
        "no reads after posting" calls
        (List.length !(e.mock.calls));
      Alcotest.(check int)
        "one complete reservation" 1
        (Store.dispatches_since e.db ~order_id:e.order.id ~since:0.);
      no_upload e)

let test_no_notes () =
  with_env (fun e ->
      seen e;
      e.replies := F.page [ !(e.root) ];
      resolve e;
      let id = (one_fire e).id in
      let attempt at delay =
        S.at at;
        check "waiting" true (fetch e id = None);
        check_state e id T.Fetching;
        Alcotest.(check (option (float 0.)))
          "relative next attempt"
          (Some (min (ended +. 3600.) (at +. delay)))
          (runtime e id).next_attempt_at
      in
      attempt (ended +. 10.) 120.;
      attempt (ended +. 130.) 300.;
      attempt (ended +. 430.) 600.;
      attempt (ended +. 1030.) 1200.;
      attempt (ended +. 2230.) 1200.;
      attempt (ended +. 3430.) 1200.;
      S.at (ended +. 3600.);
      check "final deadline" true (fetch e id = None);
      check_state e id T.No_notes;
      check "fixed reason" true ((runtime e id).fire.reason = Some "no_notes");
      no_upload e)

let test_feature_absent () =
  List.iter
    (fun when_no ->
      with_env (fun e ->
          let action = S.relay_action ~when_no () in
          Sql_util.exec_with_params e.db
            "UPDATE slack_standing_orders SET action_json=? WHERE id=?"
            [
              Sqlite3.Data.TEXT
                (Yojson.Safe.to_string (T.action_to_json action));
              Sqlite3.Data.TEXT e.order.id;
            ];
          seen e;
          ready e ();
          enable_transcript e;
          e.transcript := obj [ ("id", str "FTRANS") ];
          resolve e;
          let id = (one_fire e).id in
          (match (when_no, fetch e id) with
          | T.Relay_notes, Some v ->
              check "verified notes with API warning" true
                (v.transcript = None && v.transcript_unavailable)
          | Skip, None -> check_state e id T.Skipped_no_transcript
          | _ -> Alcotest.fail "wrong transcript fallback");
          no_upload e))
    [ T.Relay_notes; T.Skip ]

let test_refusals () =
  List.iter
    (fun kind ->
      with_env (fun e ->
          seen e;
          resolve e;
          let id = (one_fire e).id in
          e.mock.calls := [];
          (match kind with
          | `Hard -> Session.update_config e.manager (S.config ~targets:[] ())
          | `Transient ->
              S.override e.mock "auth.test" (fun _ -> (503, S.ok []))
          | `Inactive ->
              Session.update_config e.manager (S.config ~enabled:false ())
          | `Cap ->
              List.iter
                (fun i ->
                  ignore
                    (S.fire e.db e.order ~dispatched_at:(Store.now ())
                       ~room:("RCAP" ^ string_of_int i)
                       ()))
                (List.init 10 Fun.id));
          check "refusal has no payload" true (fetch e id = None);
          List.iter
            (fun m -> Alcotest.(check int) "zero content HTTP" 0 (count e m))
            [
              "conversations.history";
              "conversations.replies";
              "files.info";
              "canvases.getContent";
            ];
          (match kind with
          | `Hard ->
              check_state e id T.Denied;
              check "suspends same epoch" true
                ((Option.get (Store.find e.db e.order.id)).state = T.Suspended)
          | `Transient ->
              check_state e id T.Detected;
              Alcotest.(check (option (float 0.)))
                "transient backoff"
                (Some (Store.now () +. 120.))
                (runtime e id).next_attempt_at
          | `Inactive -> check_state e id T.Skipped_inactive
          | `Cap -> check_state e id T.Skipped_cap);
          no_upload e))
    [ `Hard; `Transient; `Inactive; `Cap ]

let test_api_and_rejection () =
  List.iter
    (fun kind ->
      with_env (fun e ->
          seen e;
          ready e ();
          resolve e;
          let id = (one_fire e).id in
          (match kind with
          | `Rejected ->
              e.root :=
                set "room"
                  (set "id" (str "RWRONG") (get "room" !(e.root)))
                  !(e.root)
          | `Scope ->
              S.override e.mock "files.info" (fun _ ->
                  ( 200,
                    obj [ ("ok", `Bool false); ("error", str "missing_scope") ]
                  ))
          | `Transport ->
              S.override e.mock "files.info" (fun _ -> (503, S.ok []))
          | `Audit ->
              S.override e.mock "conversations.replies" (fun _ ->
                  Sql_util.exec_exn e.db
                    "CREATE TRIGGER fail_content_audit BEFORE INSERT ON \
                     egress_audit BEGIN SELECT RAISE(FAIL,'private audit \
                     failure'); END";
                  (200, set "ok" (`Bool true) !(e.replies))));
          check "never falls back from failed source" true (fetch e id = None);
          (match kind with
          | `Rejected -> check_state e id T.Unavailable
          | `Scope ->
              check_state e id T.Denied;
              check "typed scope reason" true
                ((runtime e id).fire.reason = Some "missing_scope:files.info");
              check "missing scope suspends" true
                ((Option.get (Store.find e.db e.order.id)).state = T.Suspended)
          | `Transport | `Audit ->
              check_state e id T.Fetching;
              check "retry scheduled" true
                ((runtime e id).next_attempt_at = Some (Store.now () +. 120.)));
          no_upload e))
    [ `Rejected; `Scope; `Transport; `Audit ]

let test_reason_codes () =
  with_env (fun e ->
      seen e;
      ready e ();
      resolve e;
      let id = (one_fire e).id in
      let private_text = "Private transcript Alice said secret budget" in
      S.override e.mock "files.info" (fun _ ->
          ( 200,
            obj
              [
                ("ok", `Bool false);
                ("error", str private_text);
                ("body", str private_text);
              ] ));
      ignore (fetch e id);
      check "enumerated code only" true
        ((runtime e id).fire.reason = Some "indeterminate:files.info");
      let shown =
        Yojson.Safe.to_string
          (Slack_standing_order_render.order_detail_json ~fires:(fires e)
             (Option.get (Store.find e.db e.order.id)))
      in
      check "show contains no response body" false
        (let n = String.length private_text in
         let rec contains i =
           i + n <= String.length shown
           && (String.sub shown i n = private_text || contains (i + 1))
         in
         contains 0))

let test_resolve_grant () =
  with_env (fun e ->
      List.iter
        (fun i ->
          ignore
            (S.fire e.db e.order ~dispatched_at:(Store.now ())
               ~room:("RCAP" ^ string_of_int i)
               ()))
        (List.init 10 Fun.id);
      let grant () =
        Lwt_main.run
          (G.evaluate ~db:e.db
             ~current_config:(fun () -> Session.get_config e.manager)
             ~http:e.mock.http ~stage:G.Resolve ~unit_label:"resolve"
             (G.subject_of_order e.order))
      in
      (match grant () with
      | Ok v ->
          check "resolve snapshot has no fire" true
            (v.snapshot.standing_order
            = Some { Access_snapshot.order_id = e.order.id; fire_id = None })
      | Error _ -> Alcotest.fail "Resolve must not refuse at cap");
      seen e;
      ready e ();
      resolve e;
      let f = List.find (fun (f : T.fire) -> f.room_id = "R123") (fires e) in
      check_state e f.id T.Skipped_cap;
      Sql_util.exec_exn e.db "DELETE FROM slack_standing_order_cards";
      e.mock.calls := [];
      (match grant () with
      | Error f ->
          check "unannounced Resolve refuses" true
            (f.reason = G.Activation_unannounced)
      | Ok _ -> Alcotest.fail "unannounced Resolve passed");
      Alcotest.(check int)
        "unannounced costs zero HTTP" 0
        (List.length !(e.mock.calls)))

let test_transient_backoff () =
  with_env (fun e ->
      seen e;
      ready e ();
      resolve e;
      let id = (one_fire e).id in
      S.override e.mock "files.info" (fun _ -> (503, S.ok []));
      List.iter
        (fun delay ->
          let now = Store.now () in
          check "transient waits" true (fetch e id = None);
          check_state e id T.Fetching;
          check "exact transient delay" true
            ((runtime e id).next_attempt_at = Some (now +. delay));
          S.at (now +. delay))
        [ 120.; 300.; 600.; 1200.; 1200. ];
      e.replies := F.page [ !(e.root) ];
      S.at (ended +. 3590.);
      check "no-notes deadline independent of transient retries" true
        (fetch e id = None);
      check "no-notes retry capped to end" true
        ((runtime e id).next_attempt_at = Some (ended +. 3600.));
      S.at (ended +. 3600.);
      ignore (fetch e id);
      check_state e id T.No_notes)

let test_conflict () =
  with_env (fun e ->
      let other = S.seed e.db ~channel:"C999" () in
      let f = S.fire e.db other ~room:"ROTHER" () in
      Sql_util.exec_with_params e.db
        "UPDATE slack_standing_order_fires SET transcript_file_id='FTRANS' \
         WHERE id=?"
        [ Sqlite3.Data.TEXT f.id ];
      seen e;
      ready e ();
      enable_transcript e;
      resolve e;
      let id = (one_fire e).id in
      check "conflicting transcript withheld" true (fetch e id = None);
      check_state e id T.Unavailable;
      check "fixed conflict reason" true
        ((runtime e id).fire.reason = Some "transcript_conflict");
      Alcotest.(check int)
        "conflict blocks content fetch" 0
        (count e "canvases.getContent");
      no_upload e)

let test_checkpoint () =
  with_env (fun e ->
      seen e;
      ready e ();
      enable_transcript e;
      resolve e;
      let id = (one_fire e).id in
      S.override e.mock "canvases.getContent" (fun _ ->
          let r = runtime e id in
          check "ids persisted before content HTTP" true
            (r.root_ts = Some root_ts
            && r.canvas_id = Some "FCANVAS"
            && r.transcript_id = Some "FTRANS"
            && r.notes_ready_ts = Some ready_ts);
          (200, S.ok [ ("content", str "private meeting notes") ]));
      ignore (Option.get (fetch e id));
      let new_start = start + 3600 in
      let room =
        Option.get
          (Slack_huddle_classify.room_of_json
             (set "date_start" (`Int new_start) (get "room" !(e.root))))
      in
      L.record_seen ~db:e.db ~team:"T123" ~channel:"C123" ~source:"event"
        ~now:(Store.now ()) ~root_ts:"1700003601.000100" ~file_ids:[] room;
      e.root :=
        set "room" (set "id" (str "RWRONG") (get "room" !(e.root))) !(e.root);
      ignore (fetch e id);
      check_state e id T.Unavailable;
      let row =
        List.find
          (fun (r : L.row) -> r.kind = L.Room)
          (L.list_rows ~db:e.db ~team:"T123" ~channel:"C123")
      in
      check "old fetch cannot mutate reused room" true
        (row.date_start = Some new_start
        && row.transcript_file_id = None
        && not row.finalised))

let test_no_content_cache () =
  with_env (fun e ->
      seen e;
      ready e ();
      resolve e;
      let id = (one_fire e).id in
      ignore (Option.get (fetch e id));
      List.iter
        (fun table ->
          let values =
            Slack_standing_order_sql.query e.db ("SELECT * FROM " ^ table) []
              (fun stmt ->
                List.init (Sqlite3.column_count stmt) (fun i ->
                    Sqlite3.column stmt i))
            |> List.concat
          in
          check "meeting content absent from storage" false
            (List.mem (Sqlite3.Data.TEXT "private meeting notes") values))
        [
          "slack_huddles";
          "slack_standing_order_fires";
          "slack_standing_order_fetch";
        ])

let test_epoch_guard () =
  with_env (fun e ->
      seen e;
      ready e ();
      resolve e;
      let id = (one_fire e).id in
      S.override e.mock "files.info" (fun _ ->
          Sql_util.exec_with_params e.db
            "UPDATE slack_standing_orders SET epoch=epoch+1 WHERE id=?"
            [ Sqlite3.Data.TEXT e.order.id ];
          (200, obj [ ("ok", `Bool false); ("error", str "missing_scope") ]));
      check "hard refusal with a newer epoch" true (fetch e id = None);
      check_state e id T.Denied;
      check "late refusal cannot suspend new epoch" true
        ((Option.get (Store.find e.db e.order.id)).state = T.Active))

let test_waiting_eligibility () =
  with_env (fun e ->
      e.root :=
        set "room"
          (set "date_start" (`Int 1700000000) (get "room" !(e.root)))
          !(e.root);
      ready e ();
      resolve e;
      Alcotest.(check int)
        "confirm/resume during huddle inserts nothing" 0
        (List.length (fires e));
      let f = S.fire e.db e.order ~room:"R123" ~date_start:"1700000000" () in
      check "legacy ineligible fire withheld" true (fetch e f.id = None);
      check_state e f.id T.Skipped_inactive;
      Alcotest.(check int)
        "ineligible fire never fetches content" 0 (count e "files.info"))

let test_stale_ready () =
  with_env (fun e ->
      seen e;
      ready e ();
      resolve e;
      let id = (one_fire e).id in
      ignore (Option.get (fetch e id));
      let before = runtime e id and calls = count e "canvases.getContent" in
      e.replies :=
        F.page
          [
            !(e.root);
            set "ts" (str "1700000050.000100")
              (set "thread_ts" (str root_ts) F.ready);
          ];
      check "older ready waits" true (fetch e id = None);
      check "latest evidence cannot regress" true
        ((runtime e id).notes_ready_ts = before.notes_ready_ts);
      Alcotest.(check int)
        "no stale content read" calls
        (count e "canvases.getContent"))

let test_drain () =
  with_env (fun e ->
      seen e;
      let ctx =
        {
          (ctx e) with
          Slack_standing_order_ctx.limit =
            (fun ~channel_id:_ ->
              e.accepting := false;
              Lwt.pause ());
        }
      in
      let cancelled =
        Lwt_main.run
          (Lwt.catch
             (fun () ->
               let open Lwt.Syntax in
               let+ () =
                 E.resolve_pending ~ctx ~db:e.db ~manager:e.manager
                   ~http:e.mock.http e.order
               in
               false)
             (function Lwt.Canceled -> Lwt.return true | exn -> Lwt.fail exn))
      in
      check "drain cancels after limiter await" true cancelled;
      Alcotest.(check int)
        "zero HTTP after drain" 0
        (List.length !(e.mock.calls)))

let test_optional_end () =
  with_env (fun e ->
      e.root :=
        set "room" (set "date_end" `Null (get "room" !(e.root))) !(e.root);
      seen e;
      e.replies := F.page [ !(e.root) ];
      resolve e;
      let first_observed_end = Store.now () and id = (one_fire e).id in
      ignore (fetch e id);
      S.at (first_observed_end +. 3590.);
      ignore (fetch e id);
      check "missing optional end cannot extend first observation deadline" true
        ((runtime e id).next_attempt_at = Some (first_observed_end +. 3600.));
      S.at (first_observed_end +. 3600.);
      ignore (fetch e id);
      check_state e id T.No_notes)

let suite =
  [
    S.case "stub resolves from thread root not canvas metadata" test_stub;
    S.case
      "eligibility compares slack ts numerically across digit-length boundaries"
      test_eligibility;
    S.case "second ready before dispatch upgrades; after dispatch ignored"
      test_upgrade;
    S.case "no notes retries 2 5 10 20 then finalises at 60" test_no_notes;
    S.case "feature absent obeys notes or skip" test_feature_absent;
    S.case "hard refusal zero content HTTP; transient schedules retry"
      test_refusals;
    S.case "rejected source unavailable; transport and audit never notes-only"
      test_api_and_rejection;
    S.case "fire reasons are enumerated codes" test_reason_codes;
    S.case "Resolve is announced and uncapped with an unbound fire ref"
      test_resolve_grant;
    S.case
      "transient retries repeat 20 minutes without extending notes deadline"
      test_transient_backoff;
    S.case "transcript uniqueness conflict is unavailable before content"
      test_conflict;
    S.case "source ids precede content; reused room is untouched"
      test_checkpoint;
    S.case "meeting content is never cached in database" test_no_content_cache;
    S.case "hard refusal cannot suspend a newer epoch" test_epoch_guard;
    S.case "waiting fires require activation before huddle start"
      test_waiting_eligibility;
    S.case "older ready evidence cannot regress sources" test_stale_ready;
    S.case "shared worker context stops HTTP on drain" test_drain;
    S.case "missing optional end retains the first observed deadline"
      test_optional_end;
  ]
