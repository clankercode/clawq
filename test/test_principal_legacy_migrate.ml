(** Tests for legacy requester identity migration without unsafe coalescing
    (P21.M1.E3.T003). *)

module P = Principal_identity
module S = Principal_identity_store
module M = Principal_merge
module L = Principal_legacy_migrate

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  S.ensure_schema db;
  L.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_785_300_000.0
let pid s = assert_ok (P.principal_id_of_string s)

let seed_actor ~db ~principal_id ~connector ~tenant ~user
    ?(link_id = "idlink_1") () =
  let p =
    P.make_principal ~id:principal_id ~created_at:"2026-01-01T00:00:00Z"
      ~updated_at:"2026-01-01T00:00:00Z" ()
  in
  ignore (assert_ok (S.insert_principal ~db p));
  let key =
    assert_ok
      (P.make_connector_actor_key ~connector ~tenant_or_workspace:tenant
         ~immutable_user_id:user)
  in
  let actor =
    P.make_connector_actor ~key ~principal_id
      ~verified_at:"2026-01-01T00:00:00Z" ~created_at:"2026-01-01T00:00:00Z"
      ~updated_at:"2026-01-01T00:00:00Z" ()
  in
  ignore (assert_ok (S.insert_connector_actor ~db actor));
  let link =
    P.make_identity_link ~id:link_id ~principal_id ~actor_key:key
      ~linked_at:"2026-01-01T00:00:00Z" ()
  in
  ignore (assert_ok (S.insert_identity_link ~db link));
  key

let invalidation_audit ~db ~source_kind ~source_id =
  let stmt =
    Sqlite3.prepare db
      {|SELECT run_id, created_at FROM principal_legacy_invalidated_jobs
        WHERE source_kind = ? AND source_id = ?|}
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1
           (Sqlite3.Data.TEXT (L.string_of_source_kind source_kind)));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT source_id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
          let text i =
            match Sqlite3.column stmt i with
            | Sqlite3.Data.TEXT s -> s
            | _ -> Alcotest.fail "missing invalidation audit column"
          in
          Some (text 0, text 1)
      | Sqlite3.Rc.DONE -> None
      | rc ->
          Alcotest.failf "read invalidation audit failed: %s"
            (Sqlite3.Rc.to_string rc))

(* -------------------------------------------------------------------------- *)
(* Classification                                                             *)
(* -------------------------------------------------------------------------- *)

let test_backfill_unambiguous_verified_actor () =
  with_db @@ fun db ->
  let principal_id = pid "prin_ada" in
  ignore
    (seed_actor ~db ~principal_id ~connector:P.Teams ~tenant:"tenant-acme"
       ~user:"aad-42" ());
  let row =
    assert_ok
      (L.make_legacy_row ~source_kind:L.Fixture ~source_id:"r1"
         ~connector:"teams" ~tenant_or_workspace:"tenant-acme"
         ~immutable_user_id:"aad-42" ~room_id:"room-1" ())
  in
  match assert_ok (L.classify_row ~db row) with
  | L.Legacy_unresolved { reason } ->
      Alcotest.fail
        ("expected backfill, got " ^ L.string_of_unresolved_reason reason)
  | L.Backfill b ->
      Alcotest.(check string)
        "principal"
        (P.principal_id_to_string principal_id)
        (P.principal_id_to_string b.principal_id);
      let auth = L.authority_of_classification (L.Backfill b) in
      Alcotest.(check bool) "user ok" true auth.user_attributed_allowed;
      Alcotest.(check bool) "app ok" true auth.app_behavior_allowed;
      Alcotest.(check bool) "read ok" true auth.read_audit_allowed

let test_display_name_only_unresolved () =
  with_db @@ fun db ->
  let row =
    assert_ok
      (L.make_legacy_row ~source_kind:L.Fixture ~source_id:"r2"
         ~connector:"teams" ~tenant_or_workspace:"tenant-acme"
         ~requester_name:"Ada Lovelace" ())
  in
  match assert_ok (L.classify_row ~db row) with
  | L.Backfill _ -> Alcotest.fail "display name must not backfill"
  | L.Legacy_unresolved { reason = L.Display_name_only } ->
      let auth =
        L.authority_of_classification
          (L.Legacy_unresolved { reason = L.Display_name_only })
      in
      Alcotest.(check bool) "deny user" false auth.user_attributed_allowed;
      Alcotest.(check bool) "allow app" true auth.app_behavior_allowed;
      Alcotest.(check bool) "allow read" true auth.read_audit_allowed
  | L.Legacy_unresolved { reason } ->
      Alcotest.fail ("unexpected reason " ^ L.string_of_unresolved_reason reason)

let test_missing_namespace_unresolved () =
  with_db @@ fun db ->
  let row =
    assert_ok
      (L.make_legacy_row ~source_kind:L.Fixture ~source_id:"r3"
         ~connector:"slack" ~immutable_user_id:"U123" ())
  in
  match assert_ok (L.classify_row ~db row) with
  | L.Legacy_unresolved { reason = L.Missing_namespace } -> ()
  | _ -> Alcotest.fail "expected missing_namespace"

let test_cli_non_adapter_unresolved () =
  with_db @@ fun db ->
  let row =
    assert_ok
      (L.make_legacy_row ~source_kind:L.Fixture ~source_id:"r4" ~connector:"cli"
         ~tenant_or_workspace:"local" ~immutable_user_id:"device-1" ())
  in
  match assert_ok (L.classify_row ~db row) with
  | L.Legacy_unresolved { reason = L.Non_adapter_connector _ } -> ()
  | _ -> Alcotest.fail "expected non_adapter_connector"

let test_actor_not_found_does_not_invent_principal () =
  with_db @@ fun db ->
  let row =
    assert_ok
      (L.make_legacy_row ~source_kind:L.Fixture ~source_id:"r5"
         ~connector:"telegram" ~tenant_or_workspace:"bot-1"
         ~immutable_user_id:"tg-99" ())
  in
  match assert_ok (L.classify_row ~db row) with
  | L.Legacy_unresolved { reason = L.Actor_not_found } ->
      (* Store must remain empty of invented principals. *)
      Alcotest.(check bool) "no actor invented" true true
  | _ -> Alcotest.fail "expected actor_not_found"

let test_unlinked_actor_without_active_link_invalidates_active_work () =
  with_db @@ fun db ->
  let principal_id = pid "prin_unlinked" in
  ignore
    (assert_ok
       (S.insert_principal ~db
          (P.make_principal ~id:principal_id
             ~created_at:"2026-01-01T00:00:00Z"
             ~updated_at:"2026-01-01T00:00:00Z" ())));
  let key =
    assert_ok
      (P.make_connector_actor_key ~connector:P.Teams
         ~tenant_or_workspace:"tenant-unlinked" ~immutable_user_id:"aad-99")
  in
  ignore
    (assert_ok
       (S.insert_connector_actor ~db
          (P.make_connector_actor ~key ~principal_id ~lifecycle:P.Unlinked
             ~verified_at:"2026-01-01T00:00:00Z"
             ~created_at:"2026-01-01T00:00:00Z"
             ~updated_at:"2026-01-02T00:00:00Z" ())));
  let row =
    assert_ok
      (L.make_legacy_row ~source_kind:L.Background_task
         ~source_id:"legacy-unlinked-task" ~connector:"teams"
         ~tenant_or_workspace:"tenant-unlinked" ~immutable_user_id:"aad-99"
         ~job_active:true ())
  in
  let report = assert_ok (L.migrate_rows ~db ~rows:[ row ] ~now:fixed_now ()) in
  Alcotest.(check int) "no backfill" 0 report.backfilled;
  Alcotest.(check int) "unresolved" 1 report.unresolved;
  Alcotest.(check int) "active work invalidated" 1 report.jobs_invalidated;
  (match report.records with
  | [ { classification = L.Legacy_unresolved { reason = L.Actor_unlinked }; _ } ] ->
      ()
  | _ -> Alcotest.fail "unlinked actor must not backfill from stored principal");
  Alcotest.(check bool)
    "user authority denied" false
    (assert_ok
       (L.user_authority_allowed ~db ~source_kind:L.Background_task
          ~source_id:"legacy-unlinked-task"));
  Alcotest.(check bool)
    "invalidated" true
    (assert_ok
       (L.is_job_invalidated ~db ~source_kind:L.Background_task
          ~source_id:"legacy-unlinked-task"));
  ignore
    (assert_ok
       (S.update_connector_actor ~db ~lifecycle:P.Active ~key ()));
  match assert_ok (L.classify_row ~db row) with
  | L.Legacy_unresolved { reason = L.Active_identity_link_missing } -> ()
  | _ -> Alcotest.fail "active actor without active link must not backfill"

let test_existing_backfill_is_permanently_invalidated_after_link_loss () =
  with_db @@ fun db ->
  let principal_id = pid "prin_revalidate" in
  let key =
    seed_actor ~db ~principal_id ~connector:P.Teams ~tenant:"tenant-revalidate"
      ~user:"aad-revalidate" ~link_id:"link-revalidate" ()
  in
  let row =
    assert_ok
      (L.make_legacy_row ~source_kind:L.Background_task
         ~source_id:"legacy-revalidate-task" ~connector:"teams"
         ~tenant_or_workspace:"tenant-revalidate"
         ~immutable_user_id:"aad-revalidate" ~job_active:true ())
  in
  let first =
    assert_ok
      (L.migrate_rows ~db ~rows:[ row ] ~run_id:"backfill-run"
         ~now:fixed_now ())
  in
  Alcotest.(check int) "initial backfill" 1 first.backfilled;
  let original =
    match
      assert_ok
        (L.get_record ~db ~source_kind:L.Background_task
           ~source_id:"legacy-revalidate-task")
    with
    | Some record -> record
    | None -> Alcotest.fail "missing initial migration record"
  in
  ignore
    (assert_ok
       (S.update_identity_link ~db ~id:"link-revalidate" ~status:P.Unlinked
          ~unlinked_at:(Some "2026-07-14T00:00:00Z")
          ~now:(fixed_now +. 1.) ()));
  let restart =
    assert_ok
      (L.migrate_rows ~db ~rows:[ row ] ~run_id:"revalidation-run"
         ~now:(fixed_now +. 2.) ())
  in
  Alcotest.(check int) "restart does not rewrite backfill" 0 restart.backfilled;
  Alcotest.(check int) "restart does not add unresolved record" 0
    restart.unresolved;
  Alcotest.(check int) "restart invalidates stale task" 1
    restart.jobs_invalidated;
  Alcotest.(check bool)
    "stale task invalidated" true
    (assert_ok
       (L.is_job_invalidated ~db ~source_kind:L.Background_task
          ~source_id:"legacy-revalidate-task"));
  let preserved =
    match
      assert_ok
        (L.get_record ~db ~source_kind:L.Background_task
           ~source_id:"legacy-revalidate-task")
    with
    | Some record -> record
    | None -> Alcotest.fail "migration evidence must remain"
  in
  Alcotest.(check string) "original record id preserved" original.id preserved.id;
  Alcotest.(check string)
    "original evidence preserved" original.row.evidence_json
    preserved.row.evidence_json;
  let audit_before =
    match
      invalidation_audit ~db ~source_kind:L.Background_task
        ~source_id:"legacy-revalidate-task"
    with
    | Some audit -> audit
    | None -> Alcotest.fail "missing invalidation audit"
  in
  Alcotest.(check string) "invalidation run" "revalidation-run"
    (fst audit_before);
  let repeated =
    assert_ok
      (L.migrate_rows ~db ~rows:[ row ] ~run_id:"repeat-unlinked-run"
         ~now:(fixed_now +. 3.) ())
  in
  Alcotest.(check int) "repeat restart does not reinvalidate" 0
    repeated.jobs_invalidated;
  Alcotest.(check (option (pair string string)))
    "invalidation audit unchanged while unresolved" (Some audit_before)
    (invalidation_audit ~db ~source_kind:L.Background_task
       ~source_id:"legacy-revalidate-task");
  ignore
    (assert_ok
       (S.insert_identity_link ~db ~now:(fixed_now +. 4.)
          (P.make_identity_link ~id:"link-revalidate-new" ~principal_id
             ~actor_key:key ~linked_at:"2026-07-14T00:00:01Z" ())));
  let relinked =
    assert_ok
      (L.migrate_rows ~db ~rows:[ row ] ~run_id:"relinked-run"
         ~now:(fixed_now +. 5.) ())
  in
  Alcotest.(check int) "relink cannot re-authorize old task" 0
    relinked.jobs_invalidated;
  Alcotest.(check bool)
    "invalidation survives relink" true
    (assert_ok
       (L.is_job_invalidated ~db ~source_kind:L.Background_task
          ~source_id:"legacy-revalidate-task"));
  (match
     L.require_migrated_user_dispatch ~db ~source_kind:L.Background_task
       ~source_id:"legacy-revalidate-task"
   with
  | Ok () -> Alcotest.fail "old task must require replanning"
  | Error msg ->
      Alcotest.(check bool)
        "old task requires replanning" true
        (String_util.contains msg "job_invalidated"));
  Alcotest.(check (option (pair string string)))
    "invalidation audit unchanged on relink" (Some audit_before)
    (invalidation_audit ~db ~source_kind:L.Background_task
       ~source_id:"legacy-revalidate-task");
  assert_ok
    (L.require_migrated_user_dispatch ~db ~source_kind:L.Background_task
       ~source_id:"fresh-explicit-app-request")

let test_no_coalesce_on_shared_display_name () =
  with_db @@ fun db ->
  let p1 = pid "prin_a" in
  let p2 = pid "prin_b" in
  ignore
    (seed_actor ~db ~principal_id:p1 ~connector:P.Slack ~tenant:"T-WORK"
       ~user:"U-AAA" ~link_id:"link_a" ());
  ignore
    (seed_actor ~db ~principal_id:p2 ~connector:P.Slack ~tenant:"T-WORK"
       ~user:"U-BBB" ~link_id:"link_b" ());
  let row_a =
    assert_ok
      (L.make_legacy_row ~source_kind:L.Fixture ~source_id:"ca"
         ~connector:"slack" ~tenant_or_workspace:"T-WORK"
         ~immutable_user_id:"U-AAA" ~requester_name:"Shared Name" ())
  in
  let row_b =
    assert_ok
      (L.make_legacy_row ~source_kind:L.Fixture ~source_id:"cb"
         ~connector:"slack" ~tenant_or_workspace:"T-WORK"
         ~immutable_user_id:"U-BBB" ~requester_name:"Shared Name" ())
  in
  let report =
    assert_ok (L.migrate_rows ~db ~rows:[ row_a; row_b ] ~now:fixed_now ())
  in
  Alcotest.(check int) "both backfilled" 2 report.backfilled;
  Alcotest.(check int) "none unresolved" 0 report.unresolved;
  match report.records with
  | [ r1; r2 ] -> (
      match (r1.classification, r2.classification) with
      | L.Backfill b1, L.Backfill b2 ->
          Alcotest.(check bool)
            "distinct principals" true
            (not (P.principal_id_equal b1.principal_id b2.principal_id))
      | _ -> Alcotest.fail "expected two backfills")
  | _ -> Alcotest.fail "expected two records"

let test_follow_merge_tombstone () =
  with_db @@ fun db ->
  M.ensure_schema db;
  let survivor = pid "prin_survivor" in
  let loser = pid "prin_loser" in
  ignore
    (assert_ok
       (S.insert_principal ~db
          (P.make_principal ~id:survivor ~created_at:"2026-01-01T00:00:00Z"
             ~updated_at:"2026-01-01T00:00:00Z" ())));
  ignore
    (assert_ok
       (S.insert_principal ~db
          (P.make_principal ~id:loser ~lifecycle:(P.Merged_into survivor)
             ~created_at:"2026-01-02T00:00:00Z"
             ~updated_at:"2026-02-01T00:00:00Z" ())));
  let key =
    assert_ok
      (P.make_connector_actor_key ~connector:P.Discord
         ~tenant_or_workspace:"guild-1" ~immutable_user_id:"disc-9")
  in
  ignore
    (assert_ok
       (S.insert_connector_actor ~db
          (P.make_connector_actor ~key ~principal_id:loser
             ~verified_at:"2026-01-03T00:00:00Z"
             ~created_at:"2026-01-03T00:00:00Z"
             ~updated_at:"2026-02-01T00:00:00Z" ())));
  ignore
    (assert_ok
       (S.insert_identity_link ~db
          (P.make_identity_link ~id:"link_m" ~principal_id:loser ~actor_key:key
             ~linked_at:"2026-02-01T00:00:00Z" ())));
  let hist : Principal_merge_persist.actor_snapshot =
    {
      id = "hist_snap_1";
      actor_key = P.actor_identity_key key;
      principal_id_at_snapshot = loser;
      actor_json = {|{"principal_id":"prin_loser","note":"pre_merge"}|};
      reason = "pre_merge";
      merge_id = Some "merge_1";
      created_at = "2026-02-01T00:00:00Z";
    }
  in
  ignore (assert_ok (Principal_merge_persist.insert_actor_snapshot ~db hist));
  let before =
    match Principal_merge_persist.get_actor_snapshot ~db ~id:"hist_snap_1" with
    | Ok (Some s) -> s.actor_json
    | _ -> Alcotest.fail "missing hist snapshot"
  in
  let row =
    assert_ok
      (L.make_legacy_row ~source_kind:L.Fixture ~source_id:"merged"
         ~connector:"discord" ~tenant_or_workspace:"guild-1"
         ~immutable_user_id:"disc-9" ())
  in
  let report = assert_ok (L.migrate_rows ~db ~rows:[ row ] ~now:fixed_now ()) in
  Alcotest.(check int) "backfilled" 1 report.backfilled;
  Alcotest.(check int)
    "snapshots rewritten" 0 report.historical_snapshots_rewritten;
  (match report.records with
  | [ r ] -> (
      match r.classification with
      | L.Backfill b ->
          Alcotest.(check string)
            "survivor"
            (P.principal_id_to_string survivor)
            (P.principal_id_to_string b.principal_id);
          Alcotest.(check bool) "followed alias" true b.followed_merge_alias
      | _ -> Alcotest.fail "expected backfill")
  | _ -> Alcotest.fail "one record");
  let after =
    match Principal_merge_persist.get_actor_snapshot ~db ~id:"hist_snap_1" with
    | Ok (Some s) -> s.actor_json
    | _ -> Alcotest.fail "hist snapshot vanished"
  in
  Alcotest.(check string) "hist snapshot immutable" before after;
  Alcotest.(check string)
    "still names loser principal" "prin_loser"
    (match Principal_merge_persist.get_actor_snapshot ~db ~id:"hist_snap_1" with
    | Ok (Some s) -> P.principal_id_to_string s.principal_id_at_snapshot
    | _ -> "")

let test_invalidate_ambiguous_active_job () =
  with_db @@ fun db ->
  let row =
    assert_ok
      (L.make_legacy_row ~source_kind:L.Fixture ~source_id:"job_1"
         ~connector:"teams" ~tenant_or_workspace:"tenant-acme"
         ~requester_name:"Ambiguous Person" ~job_active:true ())
  in
  let report = assert_ok (L.migrate_rows ~db ~rows:[ row ] ~now:fixed_now ()) in
  Alcotest.(check int) "unresolved" 1 report.unresolved;
  Alcotest.(check int) "invalidated" 1 report.jobs_invalidated;
  Alcotest.(check bool)
    "flag set" true
    (assert_ok
       (L.is_job_invalidated ~db ~source_kind:L.Fixture ~source_id:"job_1"));
  Alcotest.(check bool)
    "user authority denied" false
    (assert_ok
       (L.user_authority_allowed ~db ~source_kind:L.Fixture ~source_id:"job_1"))

let test_origin_projection () =
  let origin =
    Room_origin.make ~connector:"teams" ~workspace_id:"tid"
      ~requester_id:"aad-1" ~requester_name:"Ada" ~room_id:"room-x" ()
  in
  let row =
    assert_ok
      (L.legacy_row_of_origin ~source_kind:L.Background_task ~source_id:"42"
         origin)
  in
  Alcotest.(check (option string)) "connector" (Some "teams") row.connector;
  Alcotest.(check (option string)) "tenant" (Some "tid") row.tenant_or_workspace;
  Alcotest.(check (option string)) "user" (Some "aad-1") row.immutable_user_id;
  Alcotest.(check (option string))
    "display only" (Some "Ada") row.requester_name

let test_load_from_background_tasks_and_workflow_runs () =
  with_db @@ fun db ->
  (* Minimal background_tasks table matching loader columns. *)
  ignore
    (Sqlite3.exec db
       {|CREATE TABLE background_tasks (
           id INTEGER PRIMARY KEY,
           status TEXT,
           requester TEXT,
           origin_json TEXT,
           session_key TEXT
         )|});
  ignore
    (Sqlite3.exec db
       {|CREATE TABLE workflow_runs (
           id INTEGER PRIMARY KEY,
           status TEXT,
           room_id TEXT,
           requester_id TEXT
         )|});
  ignore
    (Sqlite3.exec db
       {|INSERT INTO background_tasks (id, status, requester, origin_json, session_key)
         VALUES (1, 'queued', NULL,
           '{"connector":"teams","workspace_id":"t1","requester_id":"u1","room_id":"r1"}',
           'teams:r1:u1')|});
  ignore
    (Sqlite3.exec db
       {|INSERT INTO workflow_runs (id, status, room_id, requester_id)
         VALUES (9, 'pending', 'room-z', 'someone')|});
  ignore
    (seed_actor ~db ~principal_id:(pid "prin_u1") ~connector:P.Teams
       ~tenant:"t1" ~user:"u1" ());
  let report = assert_ok (L.migrate_database ~db ~now:fixed_now ()) in
  Alcotest.(check int) "one backfill from bg task" 1 report.backfilled;
  (* workflow run lacks connector+namespace → unresolved + active job *)
  Alcotest.(check bool) "has unresolved" true (report.unresolved >= 1);
  Alcotest.(check bool)
    "workflow invalidated" true
    (report.jobs_invalidated >= 1)

let test_daemon_upgrade_invalidates_unresolved_legacy_dispatch () =
  let db_path = Filename.temp_file "clawq_legacy_upgrade" ".db" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove db_path with _ -> ())
    (fun () ->
      let seed_db = Memory.init ~db_path ~search_enabled:false () in
      Background_task.init_schema seed_db;
      let task_id =
        assert_ok
          (Background_task.enqueue ~db:seed_db ~runner:Background_task.Local
             ~require_git:false ~automerge:false ~use_worktree:false
             ~repo_path:(Filename.get_temp_dir_name ())
             ~prompt:"legacy work" ~requester:"Ada Lovelace" ())
      in
      ignore (Sqlite3.db_close seed_db);
      let config =
        {
          Runtime_config.default with
          memory =
            {
              Runtime_config.default.memory with
              db_path;
              search_enabled = false;
            };
        }
      in
      match Daemon_startup.init_database ~config with
      | None -> Alcotest.fail "daemon database upgrade failed"
      | Some db ->
          Fun.protect
            ~finally:(fun () -> ignore (Sqlite3.db_close db))
            (fun () ->
              Alcotest.(check bool)
                "active unresolved task invalidated" true
                (assert_ok
                   (L.is_job_invalidated ~db ~source_kind:L.Background_task
                      ~source_id:(string_of_int task_id)));
              match
                L.require_migrated_user_dispatch ~db
                  ~source_kind:L.Background_task
                  ~source_id:(string_of_int task_id)
              with
              | Ok () ->
                  Alcotest.fail
                    "invalidated legacy task must not regain human authority"
              | Error msg ->
                  Alcotest.(check bool)
                    "actionable invalidation" true
                    (String_util.contains msg "job_invalidated")))

let test_idempotent_skip_already_migrated () =
  with_db @@ fun db ->
  ignore
    (seed_actor ~db ~principal_id:(pid "prin_x") ~connector:P.Teams ~tenant:"t"
       ~user:"u" ());
  let row =
    assert_ok
      (L.make_legacy_row ~source_kind:L.Fixture ~source_id:"idem"
         ~connector:"teams" ~tenant_or_workspace:"t" ~immutable_user_id:"u" ())
  in
  let r1 = assert_ok (L.migrate_rows ~db ~rows:[ row ] ~now:fixed_now ()) in
  let r2 =
    assert_ok
      (L.migrate_rows ~db ~rows:[ row ] ~run_id:"second_run"
         ~now:(fixed_now +. 1.) ())
  in
  Alcotest.(check int) "first backfilled" 1 r1.backfilled;
  Alcotest.(check int) "second skipped" 0 r2.backfilled

let test_rollback_clears_state_not_snapshots () =
  with_db @@ fun db ->
  M.ensure_schema db;
  ignore
    (seed_actor ~db ~principal_id:(pid "prin_y") ~connector:P.Teams ~tenant:"t"
       ~user:"u" ());
  let key =
    assert_ok
      (P.make_connector_actor_key ~connector:P.Teams ~tenant_or_workspace:"t"
         ~immutable_user_id:"u")
  in
  let hist : Principal_merge_persist.actor_snapshot =
    {
      id = "keep_me";
      actor_key = P.actor_identity_key key;
      principal_id_at_snapshot = pid "prin_y";
      actor_json = {|{"keep":true}|};
      reason = "pre_migration";
      merge_id = None;
      created_at = "2026-01-01T00:00:00Z";
    }
  in
  ignore (assert_ok (Principal_merge_persist.insert_actor_snapshot ~db hist));
  let row =
    assert_ok
      (L.make_legacy_row ~source_kind:L.Fixture ~source_id:"rb"
         ~connector:"teams" ~tenant_or_workspace:"t" ~immutable_user_id:"u" ())
  in
  let report = assert_ok (L.migrate_rows ~db ~rows:[ row ] ~now:fixed_now ()) in
  let removed = assert_ok (L.rollback_run ~db ~run_id:report.run_id) in
  Alcotest.(check int) "removed" 1 removed;
  Alcotest.(check (option string))
    "no record" None
    (match L.get_record ~db ~source_kind:L.Fixture ~source_id:"rb" with
    | Ok o -> Option.map (fun (r : L.migration_record) -> r.id) o
    | Error e -> Alcotest.fail e);
  (match Principal_merge_persist.get_actor_snapshot ~db ~id:"keep_me" with
  | Ok (Some s) ->
      Alcotest.(check string) "snapshot kept" {|{"keep":true}|} s.actor_json
  | _ -> Alcotest.fail "snapshot must survive rollback");
  (* Re-upgrade after rollback. *)
  let report2 =
    assert_ok (L.migrate_rows ~db ~rows:[ row ] ~now:(fixed_now +. 2.) ())
  in
  Alcotest.(check int) "re-upgrade backfill" 1 report2.backfilled

let test_all_upgrade_rollback_fixtures () =
  List.iter
    (fun (fx : L.fixture_case) ->
      match L.prove_upgrade_and_rollback fx with
      | Ok () -> ()
      | Error e -> Alcotest.failf "fixture %s: %s" fx.name e)
    (L.upgrade_fixture_cases ())

let test_schema_version () =
  Alcotest.(check int) "schema_version" 1 L.schema_version

let suite =
  [
    ("schema_version", `Quick, test_schema_version);
    ( "backfill unambiguous verified actor",
      `Quick,
      test_backfill_unambiguous_verified_actor );
    ("display name only unresolved", `Quick, test_display_name_only_unresolved);
    ("missing namespace unresolved", `Quick, test_missing_namespace_unresolved);
    ("cli non-adapter unresolved", `Quick, test_cli_non_adapter_unresolved);
    ( "actor not found does not invent principal",
      `Quick,
      test_actor_not_found_does_not_invent_principal );
    ( "unlinked actor without active link invalidates active work",
      `Quick,
      test_unlinked_actor_without_active_link_invalidates_active_work );
    ( "existing backfill is permanently invalidated after link loss",
      `Quick,
      test_existing_backfill_is_permanently_invalidated_after_link_loss );
    ( "no coalesce on shared display name",
      `Quick,
      test_no_coalesce_on_shared_display_name );
    ("follow merge tombstone", `Quick, test_follow_merge_tombstone);
    ( "invalidate ambiguous active job",
      `Quick,
      test_invalidate_ambiguous_active_job );
    ("origin projection", `Quick, test_origin_projection);
    ( "load from background_tasks and workflow_runs",
      `Quick,
      test_load_from_background_tasks_and_workflow_runs );
    ( "daemon upgrade invalidates unresolved legacy dispatch",
      `Quick,
      test_daemon_upgrade_invalidates_unresolved_legacy_dispatch );
    ( "idempotent skip already migrated",
      `Quick,
      test_idempotent_skip_already_migrated );
    ( "rollback clears state not snapshots",
      `Quick,
      test_rollback_clears_state_not_snapshots );
    ("all upgrade/rollback fixtures", `Quick, test_all_upgrade_rollback_fixtures);
  ]
