open Slack_standing_order_fault_support
open Lwt.Syntax
module Rollout = Slack_standing_order_rollout_store
module I = Slack_standing_order_receipt_import
module Import = Test_slack_standing_order_receipt_import

let readiness_agrees e denied =
  let r = R.rollout_readiness e.db ~order_id:e.order.id in
  check "one readiness decision" denied
    (match r with R.Denied _ -> true | R.Ready _ -> false);
  let status =
    R.status_json e.db ~order_id:e.order.id |> Yojson.Safe.to_string
  in
  let cli =
    Slack_standing_order_cli.cmd ~db:e.db
      ~config:(Session.get_config e.manager)
      [ "orders"; "show"; e.order.id ]
  in
  let name = R.readiness_name r in
  check "status has exact readiness reason" true
    (String_util.contains status name);
  check "CLI has same exact readiness reason" true
    (String_util.contains cli name)

let proof_corruption kind e =
  let* () = pass e in
  let _, uuid, _, _ = Rollout.read_anchor e.db in
  (match kind with
  | "missing_state" ->
      Sql.exec e.db "DELETE FROM slack_standing_order_rollout_state" []
  | "corrupt_digest" ->
      Sql.exec e.db
        "DROP TRIGGER slack_standing_order_feature_version_no_update" [];
      Sql.exec e.db
        "UPDATE slack_standing_order_feature_version SET bootstrap_digest='bad'"
        []
  | "missing_activation" ->
      Sql.exec e.db "DELETE FROM slack_standing_order_activation_bindings" []
  | "missing_schema" ->
      Sql.exec e.db "DROP TABLE slack_standing_order_recovery_candidates" []
  | _ -> assert false);
  readiness_agrees e true;
  ignore (install_instance e ended_root);
  e.server.roots <- [ ended_root ];
  let refused = ref [] in
  let deny_boundary label work =
    Lwt.catch work (fun exn ->
        readiness_agrees e true;
        refused := label :: !refused;
        Printf.eprintf "T8_INVALID_PROOF boundary=%s exception=%s\n%!" label
          (Printexc.to_string exn);
        Lwt.return_unit)
  in
  let* () = deny_boundary "reconciler" (fun () -> pass e) in
  (* Coverage is a scan certificate, independent of rollout authority. Its
     prior validity cannot substitute for the readiness fence. *)
  check "invalid proof blocks durable readiness fence" false
    (R.ready e.db e.order.id);
  (* Running the ordinary observer/worker must not repair or auto-freshen. *)
  let* () =
    deny_boundary "worker-start/observer" (fun () ->
        start e;
        observe e (event_root ended_root);
        observe e (ready_for ended_root);
        advance e 30.)
  in
  let* () = deny_boundary "worker-pass" (fun () -> worker e) in
  readiness_agrees e true;
  eq "invalid proof zero allocations" 0
    (count e.server "files.getUploadURLExternal");
  eq "invalid proof zero actual shares" 0 (relays e);
  let _, same, _, _ = Rollout.read_anchor e.db in
  Alcotest.(check string) "no new installation invented" uuid same;
  Lwt.return_unit

let fresh_reopen e =
  let confirmed_at = e.server.clock.now in
  let* () = pass e in
  let _, uuid, _, _ = Rollout.read_anchor e.db in
  let other = Sqlite3.db_open e.path in
  Lwt.finalize
    (fun () ->
      Store.ensure_schema other;
      let _, other_uuid, _, _ = Rollout.read_anchor other in
      Alcotest.(check string)
        "second independent initializer reuses UUID" uuid other_uuid;
      eq "single feature bootstrap" 1
        (Sql.int_query other
           "SELECT count(*) FROM slack_standing_order_feature_version" []);
      readiness_agrees e false;
      let* () = restart e in
      let* () = pass e in
      readiness_agrees e false;
      e.server.calls <- [];
      start e;
      let* () = advance e 3. in
      let* () = advance e (max 0. (S.now +. 40. -. e.server.clock.now)) in
      let at = e.server.clock.now in
      live e;
      let* () =
        until e "fresh proof with reopened real DB still shares under a minute"
          (fun () -> relays e = 1)
          (max 0 (int_of_float (59. -. (e.server.clock.now -. confirmed_at))))
      in
      check "ordinary confirmation through reopen to actual share <60" true
        (e.server.clock.now -. confirmed_at < 60.);
      Printf.eprintf
        "T8_CONFIRM_TO_SHARE elapsed=%.0f reopen=true handles=2 import=false\n\
         %!"
        (e.server.clock.now -. confirmed_at);
      check "fresh no-import SLO after reopen" true
        (e.server.clock.now -. at < 60.);
      check "same instance ready from independent handle" true
        (R.rollout_readiness other ~order_id:e.order.id
        = R.Ready R.Fresh_no_legacy_work);
      Lwt.return_unit)
    (fun () ->
      ignore (Sqlite3.db_close other);
      Lwt.return_unit)

let legacy e =
  let config = Session.get_config e.manager in
  let* () = stop () in
  check "fresh worker DB closed before replacing fixture" true
    (Sqlite3.db_close e.db);
  e.closed <- true;
  let db, order, installation = Import.legacy () in
  Access_snapshot.init_schema db;
  Admin.init_schema db;
  Egress_audit.init_schema db;
  Setup_plan_apply.init_schema db;
  Room_activity_ledger.init_schema db;
  let card =
    Store.insert_card db ~kind:T.Pause_button ~order
      ~created_by:order.grantor_user_id ~now:S.now ()
  in
  Store.set_card_message_ts db ~card_id:card.id "1700000000.000200";
  Sys.remove e.path;
  Sql.exec db "VACUUM INTO ?" [ Sql.text e.path ];
  ignore (Sqlite3.db_close db);
  e.db <- Sqlite3.db_open e.path;
  e.closed <- false;
  e.order <- order;
  e.manager <- Session.create ~config ~db:e.db ();
  e.ctx <- context e.server e.accepting;
  configure e;
  Lwt.return installation

let legacy_pending partial e =
  let* installation = legacy e in
  (if partial then
     let receipt = Import.receipt e.order installation in
     let valid = I.validate e.db ~receipt |> Import.ok in
     (* No completed inventory manifest: a validated item alone is insufficient. *)
     check "partial apply without completion is refused" true
       (Result.is_error
          (I.apply e.db [ valid ] ~manifest_digest:(String.make 64 'a'))));
  readiness_agrees e true;
  e.server.calls <- [];
  e.server.roots <- [ ended_root ];
  ignore (install_instance e ended_root);
  start e;
  observe e (event_root ended_root);
  observe e (ready_for ended_root);
  let* () = advance e 1000. in
  eq "legacy pending inventory zero new relay" 0 (relays e);
  eq "legacy pending inventory zero raw upload" 0 (count e.server "raw_upload");
  let* () = restart e in
  let* () = pass e in
  readiness_agrees e true;
  eq "reopen never auto-freshens P3 work" 0 (relays e);
  Lwt.return_unit

let manual e =
  let* installation = legacy e in
  let receipt = Import.receipt e.order installation in
  let receipt =
    {
      receipt with
      I.root_ts = H.root_ts;
      room_id = "R123";
      date_start = "1700000010";
      evidence_digest = "";
    }
  in
  let receipt = { receipt with I.evidence_digest = I.receipt_digest receipt } in
  let manifest = Import.inventory e.db e.order installation [ receipt ] in
  let inputs = Import.validated e.db manifest [ receipt ] in
  let digest = I.inventory_digest manifest in
  ignore (I.apply e.db inputs ~manifest_digest:digest |> Import.ok);
  (* Model the historical no-nonce own relay on the independent server. No
     executor attempt is manufactured and this never grants repost authority. *)
  let i = install_instance e ended_root in
  save_message e.server
    {
      json =
        obj
          [
            ("ts", str receipt.relay_ts);
            ("user", str "UBOT");
            ("bot_id", str "BOWN");
            ( "files",
              `List
                (List.map
                   (fun id -> obj [ ("id", str id) ])
                   receipt.own_file_ids) );
          ];
      instance = Some i;
      kind = "relay";
      visible_at = 0.;
    };
  let other = Sqlite3.db_open e.path in
  Lwt.finalize
    (fun () ->
      ignore (I.verify_rollout other ~manifest_digest:digest |> Import.ok);
      readiness_agrees e false;
      let* () = restart e in
      e.server.roots <- [ ended_root ];
      let* () = pass e in
      start e;
      observe e (event_root ended_root);
      observe e (ready_for ended_root);
      observe e (ready_for ended_root);
      let* () = advance e 1000. in
      eq "manual instance never relayed again through event/history" 1
        (relays e);
      eq "manual proof has no automatic attempt" 0
        (Sql.int_query e.db
           "SELECT count(*) FROM slack_standing_order_dispatches" []);
      let* () = stop () in
      let paused =
        Store.deactivate e.db ~order_id:e.order.id (Store.Pause "U123")
          ~now:e.server.clock.now
        |> Result.get_ok
      in
      ignore
        (Store.resume e.db ~order_id:e.order.id
           ~expected_epoch:paused.after.epoch ~active_since:"1700000020.000001"
           ~bot_user_id:"UBOT" ~bot_id:"BOWN" ~now:e.server.clock.now
        |> Result.get_ok);
      let* () = restart e in
      start e;
      observe e (event_root ended_root);
      observe e (ready_for ended_root);
      let* () = advance e 1000. in
      eq "manual suppression monotonic through resume and reopened discovery" 1
        (relays e);
      eq "suppressed source causes no allocation" 0
        (count e.server "files.getUploadURLExternal");
      check "second handle sees same immutable tombstone" true
        (R.suppressed other ~order_id:e.order.id ~root_ts:H.root_ts
           ~room_id:"R123" ~date_start:"1700000010");
      (* A distinct missed instance after the new activation remains recoverable. *)
      let new_root =
        ended_root
        |> set "ts" (str "1700000030.000100")
        |> set "room"
             (get "room" ended_root
             |> set "id" (str "RNEW")
             |> set "date_start" (`Int 1700000030)
             |> set "date_end" (`Int 1700000050)
             |> set "thread_root_ts" (str "1700000030.000100")
             |> set "canvas_thread_ts" (str "1700000030.000100"))
      in
      e.server.roots <- [ ended_root; new_root ];
      ignore (install_instance e new_root);
      (* Notes-only uses the same verified canvas metadata, with exact new shares. *)
      e.server.transcript <- false;
      e.server.override <-
        (fun m p ->
          if m = "files.info" && field "file" p = "FCANVAS" then
            Some
              ( 200,
                S.ok
                  [
                    ( "file",
                      P.canvas
                      |> set "shares"
                           (obj
                              [
                                ( "private",
                                  obj
                                    [
                                      ( "C123",
                                        `List
                                          [
                                            P.share "1700000030.000100";
                                            P.share P.ready_ts;
                                          ] );
                                    ] );
                              ]) );
                  ] )
          else None);
      C.trigger ();
      let* () = advance e 1300. in
      eq
        "different genuinely missed instance recovers while manual stays \
         suppressed"
        2 (relays e);
      bounds e.server;
      Lwt.return_unit)
    (fun () ->
      ignore (Sqlite3.db_close other);
      Lwt.return_unit)

let suite =
  [
    S.case
      "ordinary fresh binding survives reopen and independent initialization"
      (fun () ->
        with_server (fun server ->
            server.clock.now <- S.now;
            server.ready_ts <- "1700000040.000100";
            with_env ~server fresh_reopen));
    case "P3 absent inventory stays denied through actual worker"
      (legacy_pending false);
    case "P3 partial inventory stays denied through actual worker"
      (legacy_pending true);
    case "manual no-nonce suppression survives events history resume reopen"
      manual;
  ]
  @ List.map
      (fun kind ->
        case ("all admission fences deny " ^ kind) (proof_corruption kind))
      [
        "missing_state";
        "corrupt_digest";
        "missing_activation";
        "missing_schema";
      ]
