(** Tests for Actor_snapshot propagation through durable jobs, retries, and
    outbox (P21.M1.E3.T005). *)

module P = Principal_identity
module S = Principal_identity_store
module B = Github_account_binding
module A = Actor_snapshot
module Job = Github_durable_job_actor_attribution
module O = Github_delivery_outbox
module D = Github_delivery_intent
module Proj = Github_item_projection
module E = Github_event_envelope
module Bg = Github_room_background_work
module RS = Github_route_store
module Attr = Github_action_actor_attribution
module L = Principal_legacy_migrate

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  S.ensure_schema db;
  B.ensure_schema db;
  O.ensure_schema db;
  Github_work_item.init_schema db;
  Background_task.init_schema db;
  RS.ensure_schema db;
  Setup_plan_apply.init_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_785_300_000.0
let room_id = "room-teams-shared"
let item_key = "pr:acme/widget:42"
let pid s = assert_ok (P.principal_id_of_string s)

let sample_key ?(connector = P.Teams) ?(tenant = "tenant-acme")
    ?(user = "user-ada") () =
  assert_ok
    (P.make_connector_actor_key ~connector ~tenant_or_workspace:tenant
       ~immutable_user_id:user)

let seed_principal ~db ~id ?(revision = 1) () =
  let p =
    P.make_principal ~id:(pid id) ~revision ~created_at:"2026-01-01T00:00:00Z"
      ~updated_at:"2026-01-01T00:00:00Z" ()
  in
  ignore (assert_ok (S.insert_principal ~db ~now:fixed_now p))

let seed_actor_and_link ~db ~principal_id ~key ?(link_id = "idlink_ada") () =
  let actor =
    P.make_connector_actor ~key ~principal_id
      ~display:
        {
          display_name = Some "Ada";
          avatar_url = None;
          email = None;
          extra = [];
        }
      ~verified_at:"2026-07-01T00:00:00Z" ~created_at:"2026-07-01T00:00:00Z"
      ~updated_at:"2026-07-01T00:00:00Z" ()
  in
  let actor = assert_ok (S.insert_connector_actor ~db ~now:fixed_now actor) in
  let link =
    P.make_identity_link ~id:link_id ~principal_id ~actor_key:key
      ~linked_at:"2026-07-01T00:00:00Z" ()
  in
  let link = assert_ok (S.insert_identity_link ~db ~now:fixed_now link) in
  (actor, link)

let seed_binding ~db ~principal_id ?(id = "ghbind_ada")
    ?(lineage_id = "lineage_ada") () =
  let identity =
    assert_ok (B.make_account_identity ~app_id:42 ~github_user_id:9001L ())
  in
  let b =
    B.make_binding ~id ~principal_id ~identity ~lineage_id
      ~authorization_status:B.Authorized
      ~display:{ B.login = Some "ada"; avatar_url = None }
      ~vault_ref:(assert_ok (B.make_vault_ref "vault_opaque_only"))
      ~created_at:"2026-07-01T00:00:00Z" ~updated_at:"2026-07-01T00:00:00Z" ()
  in
  assert_ok (B.insert ~db ~now:fixed_now b)

let seed_ada ~db =
  let principal_id = pid "prin_ada" in
  seed_principal ~db ~id:"prin_ada" ();
  let key = sample_key () in
  let _actor, link = seed_actor_and_link ~db ~principal_id ~key () in
  let binding = seed_binding ~db ~principal_id () in
  (principal_id, key, link, binding)

let contains hay needle =
  let hay = String.lowercase_ascii hay in
  let needle = String.lowercase_ascii needle in
  try
    let _ = Str.search_forward (Str.regexp_string needle) hay 0 in
    true
  with Not_found -> false

let sample_intent ?(id = "ghdi_job_1") ?(room_id = room_id)
    ?(item_key = item_key) ?(now = fixed_now) () : D.intent =
  let proj : Proj.projection =
    {
      room_id;
      item_key;
      title = Some "Add feature";
      state = Some "open";
      draft = Some false;
      merged = None;
      labels = [ "enhancement" ];
      assignees = [ "alice" ];
      head_sha = Some "abc123";
      html_url = Some "https://github.com/acme/widget/pull/42";
      last_event_at = Some "2024-01-01T00:00:00Z";
      last_family = Some E.Lifecycle;
      comment_count = 0;
      revision = 1;
      card_kind = Proj.Lifecycle;
    }
  in
  let intent = D.of_projection ~room_id ~projection:proj ~now () in
  { intent with id }

let caps ~background : RS.capability_policy =
  {
    allow_reply = false;
    allow_label = false;
    allow_assign = false;
    allow_review = false;
    allow_merge = false;
    allow_close = false;
    extra = (if background then [ (Bg.capability_key, true) ] else []);
  }

let make_route ~id ~policy : RS.t =
  {
    id;
    destination = RS.Room room_id;
    selector = RS.Repo "acme/widget";
    filter = RS.default_filter;
    comment_mode = RS.default_comment_mode;
    capability_policy = policy;
    enabled = true;
    revision = "1";
    managed_bundle_id = None;
    managed_feature_id = None;
    provenance =
      {
        created_by = Some "test";
        created_via = Some "test";
        setup_plan_id = None;
        notes = None;
      };
    created_at = "2024-01-01T00:00:00Z";
    updated_at = "2024-01-01T00:00:00Z";
  }

let pilot_on =
  {
    Bg.enabled = true;
    pilot_name = "p19-room-background-work-pilot";
    expires_at = None;
  }

let bg_req ?(dedup = "room:room-teams-shared:bg:dedup-1") () : Bg.request =
  {
    room_id;
    item_key = Some "issue:acme/widget:9";
    prompt = "summarize discussion";
    runner_pref = None;
    thread_ref = Some "thread:msg-1";
    dedup_key = dedup;
  }

(* 1. capture for delayed job pins delayed_job_id and is never authority *)
let test_capture_for_delayed_job () =
  with_db @@ fun db ->
  let _pid, key, _link, binding = seed_ada ~db in
  let snap =
    assert_ok
      (Job.capture_for_delayed_job ~db ~actor_key:key
         ~delayed_job_id:"outbox_ghdi_1" ~account_binding_id:binding.id ~room_id
         ~session_id:"sess_ada" ~now:fixed_now ())
  in
  Alcotest.(check string) "reason" "delayed_job" snap.reason;
  Alcotest.(check (option string))
    "delayed_job_id" (Some "outbox_ghdi_1") snap.work_refs.delayed_job_id;
  Alcotest.(check (option string))
    "room source only" (Some room_id) snap.source.room_id;
  Alcotest.(check bool) "never authority" false (A.is_authority snap);
  Alcotest.(check (option string))
    "account lineage" (Some "lineage_ada") snap.lineage.account_lineage_id;
  let j = assert_ok (Job.snapshot_to_storage_json snap) in
  Alcotest.(check bool) "no token material" false (A.contains_token_material j)

(* 2. outbox enqueue stores snapshot; claim + retry preserve it *)
let test_outbox_enqueue_claim_retry_preserve_snapshot () =
  with_db @@ fun db ->
  let _pid, key, _link, binding = seed_ada ~db in
  let intent = sample_intent ~id:"ghdi_snap_1" () in
  let snap =
    assert_ok
      (Job.capture_for_delayed_job ~db ~actor_key:key ~delayed_job_id:intent.id
         ~account_binding_id:binding.id ~room_id ~now:fixed_now ())
  in
  let entry =
    assert_ok
      (O.enqueue ~db ~room_id ~item_key:intent.item_key ~intent
         ~actor_snapshot:snap ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "has snapshot json" true
    (Option.is_some entry.actor_snapshot_json);
  let stored =
    match assert_ok (O.snapshot_of_entry entry) with
    | Some s -> s
    | None -> Alcotest.fail "expected snapshot on entry"
  in
  Alcotest.(check string) "snapshot id" snap.id stored.id;
  Alcotest.(check string)
    "principal lineage" "prin_ada"
    (P.principal_id_to_string stored.lineage.principal_id);
  (* Claim (restart recovery path) preserves snapshot. *)
  let claimed = assert_ok (O.claim_due ~db ~now:fixed_now ~limit:10 ()) in
  Alcotest.(check int) "claimed 1" 1 (List.length claimed);
  let c0 = List.hd claimed in
  Alcotest.(check string) "same id" entry.id c0.id;
  (match assert_ok (O.snapshot_of_entry c0) with
  | Some s -> Alcotest.(check string) "claim keeps snap" snap.id s.id
  | None -> Alcotest.fail "snapshot lost on claim");
  (* Transient failure → pending retry; snapshot must survive. *)
  let after_fail =
    assert_ok
      (O.mark_failure ~db ~id:entry.id ~error:"connector timeout"
         ~now:(fixed_now +. 1.) ())
  in
  Alcotest.(check bool)
    "still pending or dead" true
    (match after_fail.status with
    | O.Pending | O.Dead_letter -> true
    | _ -> false);
  (match assert_ok (O.snapshot_of_entry after_fail) with
  | Some s -> Alcotest.(check string) "retry keeps snap" snap.id s.id
  | None -> Alcotest.fail "snapshot lost on retry");
  (* Re-enqueue same intent is idempotent and keeps initiating snapshot. *)
  let again =
    assert_ok
      (O.enqueue ~db ~room_id ~item_key:intent.item_key ~intent
         ~actor_snapshot:snap ~now:(fixed_now +. 2.) ())
  in
  match assert_ok (O.snapshot_of_entry again) with
  | Some s -> Alcotest.(check string) "idempotent keep" snap.id s.id
  | None -> Alcotest.fail "snapshot lost on re-enqueue"

(* 3. outbox re-resolve succeeds; split / revoke fail closed *)
let test_outbox_execution_reresolve_and_fail_closed () =
  with_db @@ fun db ->
  let _principal_id, key, link, binding = seed_ada ~db in
  let intent = sample_intent ~id:"ghdi_exec_1" () in
  let snap =
    assert_ok
      (Job.capture_for_delayed_job ~db ~actor_key:key ~delayed_job_id:intent.id
         ~account_binding_id:binding.id ~room_id ~now:fixed_now ())
  in
  let entry =
    assert_ok
      (O.enqueue ~db ~room_id ~item_key:intent.item_key ~intent
         ~actor_snapshot:snap ~now:fixed_now ())
  in
  let env =
    match
      Job.prepare_execution_of_json ~db ~job_id:entry.id
        ~snapshot_json:entry.actor_snapshot_json ~require_snapshot:true ()
    with
    | Ok (Some e) -> e
    | Ok None -> Alcotest.fail "expected envelope"
    | Error e -> Alcotest.fail e
  in
  Alcotest.(check bool) "usable" true env.live_authority.usable;
  Alcotest.(check string) "job id" entry.id env.job_id;
  Alcotest.(check string)
    "principal lineage" "prin_ada" env.principal_lineage_id;
  (* Split/unlink: fail closed. *)
  let new_pid = pid "prin_new_empty" in
  ignore
    (assert_ok
       (S.insert_principal ~db ~now:(fixed_now +. 1.)
          (P.make_principal ~id:new_pid ~created_at:"2026-07-13T00:00:00Z"
             ~updated_at:"2026-07-13T00:00:00Z" ())));
  ignore
    (assert_ok
       (S.update_identity_link ~db ~id:link.id ~status:P.Unlinked
          ~unlinked_at:(Some "2026-07-13T12:00:00Z") ~now:(fixed_now +. 1.) ()));
  ignore
    (assert_ok
       (S.insert_identity_link ~db ~now:(fixed_now +. 2.)
          (P.make_identity_link ~id:"idlink_new" ~principal_id:new_pid
             ~actor_key:key ~linked_at:"2026-07-13T12:00:00Z" ())));
  ignore
    (assert_ok
       (S.update_connector_actor ~db ~key ~principal_id:new_pid
          ~now:(fixed_now +. 2.) ()));
  (match
     Job.prepare_execution_of_json ~db ~job_id:entry.id
       ~snapshot_json:entry.actor_snapshot_json ~require_snapshot:true ()
   with
  | Ok (Some _) -> Alcotest.fail "expected fail closed after split"
  | Ok None -> Alcotest.fail "expected error not None"
  | Error msg ->
      Alcotest.(check bool)
        "mentions unusable/authority" true
        (contains msg "unusable" || contains msg "authority"
       || contains msg "refused" || contains msg "principal"));
  (* Snapshot evidence on the row is unchanged. *)
  match assert_ok (O.snapshot_of_entry entry) with
  | Some s ->
      Alcotest.(check string)
        "evidence still ada" "prin_ada"
        (P.principal_id_to_string s.lineage.principal_id)
  | None -> Alcotest.fail "evidence missing"

(* 4. never borrow another participant on re-enqueue or exec *)
let test_never_borrow_other_participant () =
  with_db @@ fun db ->
  let _pid, key_ada, _link, binding = seed_ada ~db in
  seed_principal ~db ~id:"prin_bob" ();
  let key_bob = sample_key ~user:"user-bob" () in
  let _ =
    seed_actor_and_link ~db ~principal_id:(pid "prin_bob") ~key:key_bob
      ~link_id:"idlink_bob" ()
  in
  let intent = sample_intent ~id:"ghdi_borrow_1" () in
  let snap_ada =
    assert_ok
      (Job.capture_for_delayed_job ~db ~actor_key:key_ada
         ~delayed_job_id:intent.id ~account_binding_id:binding.id ~room_id
         ~now:fixed_now ())
  in
  let _ =
    assert_ok
      (O.enqueue ~db ~room_id ~item_key:intent.item_key ~intent
         ~actor_snapshot:snap_ada ~now:fixed_now ())
  in
  let snap_bob =
    assert_ok
      (Job.capture_for_delayed_job ~db ~actor_key:key_bob
         ~delayed_job_id:intent.id ~room_id ~now:(fixed_now +. 1.) ())
  in
  (match
     O.enqueue ~db ~room_id ~item_key:intent.item_key ~intent
       ~actor_snapshot:snap_bob ~now:(fixed_now +. 1.) ()
   with
  | Ok _ -> Alcotest.fail "expected conflict on borrow re-enqueue"
  | Error e ->
      Alcotest.(check bool)
        "mentions conflict/borrow" true
        (contains e "conflict" || contains e "borrow" || contains e "refuses"));
  (* claimed_actor mismatch at exec *)
  match
    Job.prepare_execution ~db ~job_id:intent.id ~snapshot:snap_ada
      ~claimed_actor:key_bob ()
  with
  | Ok _ -> Alcotest.fail "expected borrow reject at exec"
  | Error inv ->
      let msg = Job.string_of_exec_invalidation inv in
      Alcotest.(check bool)
        "borrow message" true
        (contains msg "another participant" || contains msg "claimed")

(* 5. work item + cancel + retry preserve snapshot; cancelled fails closed *)
let test_work_item_cancel_retry_preserve_and_fail_closed () =
  with_db @@ fun db ->
  let _pid, key, _link, binding = seed_ada ~db in
  let snap =
    assert_ok
      (Job.capture_for_delayed_job ~db ~actor_key:key
         ~delayed_job_id:"work_dedup_1" ~account_binding_id:binding.id ~room_id
         ~now:fixed_now ())
  in
  let item =
    assert_ok
      (Bg.enqueue_work_item ~db
         ~req:(bg_req ~dedup:"work_dedup_1" ())
         ~actor_snapshot:snap ())
  in
  Alcotest.(check bool)
    "has snap" true
    (Option.is_some item.actor_snapshot_json);
  let snap_id = snap.id in
  (match assert_ok (Github_work_item.snapshot_of_item item) with
  | Some s -> Alcotest.(check string) "id" snap_id s.id
  | None -> Alcotest.fail "missing snap");
  (* Cancel preserves snapshot. *)
  let cancelled = assert_ok (Bg.cancel_work_item ~db ~id:item.id ()) in
  Alcotest.(check bool)
    "cancelled terminal" true
    (cancelled.status = Github_work_item.Cancelled);
  (match assert_ok (Github_work_item.snapshot_of_item cancelled) with
  | Some s -> Alcotest.(check string) "cancel keeps" snap_id s.id
  | None -> Alcotest.fail "snap lost on cancel");
  (match
     Job.prepare_execution_of_json ~db ~job_id:(string_of_int item.id)
       ~snapshot_json:cancelled.actor_snapshot_json ~require_snapshot:true
       ~cancelled:true ()
   with
  | Ok _ -> Alcotest.fail "cancelled must fail closed"
  | Error msg ->
      Alcotest.(check bool) "mentions cancel" true (contains msg "cancel"));
  (* Retry re-queues and still carries snapshot. *)
  let retried = assert_ok (Bg.request_retry ~db ~id:item.id ()) in
  Alcotest.(check bool)
    "queued again" true
    (retried.status = Github_work_item.Queued);
  (match assert_ok (Github_work_item.snapshot_of_item retried) with
  | Some s -> Alcotest.(check string) "retry keeps" snap_id s.id
  | None -> Alcotest.fail "snap lost on retry");
  match
    Job.prepare_execution_of_json ~db ~job_id:(string_of_int item.id)
      ~snapshot_json:retried.actor_snapshot_json ~require_snapshot:true ()
  with
  | Ok (Some env) ->
      Alcotest.(check bool) "usable after retry" true env.live_authority.usable
  | Ok None -> Alcotest.fail "expected env"
  | Error e -> Alcotest.fail e

(* 6. background plan pins snapshot; revocation fails closed at exec *)
let test_background_plan_and_account_revocation () =
  with_db @@ fun db ->
  let _pid, key, _link, binding = seed_ada ~db in
  let route = make_route ~id:"rt_bg" ~policy:(caps ~background:true) in
  let principal =
    Setup_plan.{ id = "prin_ada"; kind = Principal; label = Some "Ada" }
  in
  let plan =
    assert_ok
      (Bg.plan_background ~db ~principal
         ~req:(bg_req ~dedup:"plan_dedup" ())
         ~base_revision:"rev-1" ~route ~pilot:pilot_on ~actor_key:key
         ~account_binding_id:binding.id ~session_id:"sess_ada" ~now:fixed_now ())
  in
  Alcotest.(check bool) "plan has snapshot" true (Attr.has_actor_snapshot plan);
  let snap =
    match assert_ok (Attr.snapshot_of_plan plan) with
    | Some s -> s
    | None -> Alcotest.fail "plan snapshot"
  in
  Alcotest.(check (option string))
    "delayed job id set" (Some plan.id) snap.work_refs.delayed_job_id;
  ignore
    (assert_ok
       (B.update_authorization_status ~db ~id:binding.id ~status:B.Revoked
          ~now:(fixed_now +. 1.) ()));
  match Job.prepare_execution ~db ~job_id:plan.id ~snapshot:snap () with
  | Ok _ -> Alcotest.fail "expected revoke fail closed"
  | Error inv ->
      let msg = Job.string_of_exec_invalidation inv in
      Alcotest.(check bool)
        "mentions authority" true
        (contains msg "unusable" || contains msg "account"
       || contains msg "authority" || contains msg "refused")

let test_worker_retry_revalidates_work_item_snapshot () =
  with_db @@ fun db ->
  let _pid, key, _link, binding = seed_ada ~db in
  let snap =
    assert_ok
      (Job.capture_for_delayed_job ~db ~actor_key:key
         ~delayed_job_id:"retry_guard_item" ~account_binding_id:binding.id
         ~room_id ~now:fixed_now ())
  in
  let item =
    assert_ok
      (Bg.enqueue_work_item ~db
         ~req:(bg_req ~dedup:"retry_guard_item" ())
         ~actor_snapshot:snap ())
  in
  let task_id =
    assert_ok
      (Background_task.enqueue ~db ~runner:Background_task.Local
         ~require_git:false ~automerge:false ~use_worktree:false
         ~repo_path:(Filename.get_temp_dir_name ())
         ~prompt:"durable retry" ())
  in
  Github_work_item.attach_task ~db ~id:item.id ~background_task_id:task_id;
  Background_task.finish ~db ~id:task_id ~status:Background_task.Failed
    ~result_preview:"initial failure";
  ignore
    (assert_ok
       (B.update_authorization_status ~db ~id:binding.id ~status:B.Revoked
          ~now:(fixed_now +. 1.) ()));
  (match Github_work_item.require_actor_snapshot_current ~db item with
  | Ok () -> Alcotest.fail "revoked snapshot must fail worker preflight"
  | Error msg ->
      Alcotest.(check bool)
        "actionable failure" true
        (contains msg "not longer executable" || contains msg "unusable"));
  match Background_task.retry ~db ~id:task_id with
  | Ok _ -> Alcotest.fail "retry must not requeue revoked human attribution"
  | Error msg ->
      Alcotest.(check bool)
        "retry cites durable guard" true
        (contains msg "human-attributed" || contains msg "attribution")

let invalidate_revalidated_legacy_background_task_without_work_item ~db ~task_id =
  (match Github_work_item.find_by_task ~db ~background_task_id:task_id with
  | None -> ()
  | Some _ -> Alcotest.fail "fixture must have no GitHub work item");
  let _principal_id, _key, link, _binding = seed_ada ~db in
  let row =
    assert_ok
      (L.make_legacy_row ~source_kind:L.Background_task
         ~source_id:(string_of_int task_id) ~connector:"teams"
         ~tenant_or_workspace:"tenant-acme" ~immutable_user_id:"user-ada"
         ~job_active:true ())
  in
  let initial =
    assert_ok
      (L.migrate_rows ~db ~rows:[ row ] ~run_id:"initial-backfill"
         ~now:fixed_now ())
  in
  Alcotest.(check int) "legacy task backfilled first" 1 initial.backfilled;
  ignore
    (assert_ok
       (S.update_identity_link ~db ~id:link.id ~status:P.Unlinked
          ~unlinked_at:(Some "2026-07-14T00:00:00Z")
          ~now:(fixed_now +. 1.) ()));
  let restart =
    assert_ok
      (L.migrate_rows ~db ~rows:[ row ] ~run_id:"restart-revalidation"
         ~now:(fixed_now +. 2.) ())
  in
  Alcotest.(check int) "legacy task invalidated on restart" 1
    restart.jobs_invalidated;
  Alcotest.(check bool)
    "revalidated legacy task invalidated" true
    (assert_ok
       (L.is_job_invalidated ~db ~source_kind:L.Background_task
          ~source_id:(string_of_int task_id)))

let enqueue_legacy_background_task ~db ~prompt =
  assert_ok
    (Background_task.enqueue ~db ~runner:Background_task.Local
       ~require_git:false ~automerge:false ~use_worktree:false
       ~repo_path:(Filename.get_temp_dir_name ()) ~prompt ())

let test_worker_spawn_rejects_invalidated_task_without_work_item () =
  with_db @@ fun db ->
  let task_id = enqueue_legacy_background_task ~db ~prompt:"legacy spawn" in
  invalidate_revalidated_legacy_background_task_without_work_item ~db ~task_id;
  let task =
    match Background_task.get_task ~db ~id:task_id with
    | Some task -> task
    | None -> Alcotest.fail "missing background task"
  in
  let spawned = ref false in
  Background_task.spawn_task ~db
    ~run_simple_command:(fun ~cwd:_ _argv ->
      spawned := true;
      Lwt.return (0, "", ""))
    task;
  Alcotest.(check bool) "refused before worktree/spawn" false !spawned;
  match Background_task.get_task ~db ~id:task_id with
  | None -> Alcotest.fail "missing failed background task"
  | Some failed ->
      Alcotest.(check string)
        "status failed" "failed"
        (Background_task.string_of_status failed.status);
      Alcotest.(check bool)
        "invalidation reported" true
        (match failed.result_preview with
        | Some msg -> contains msg "invalidated"
        | None -> false)

let test_local_runner_rejects_invalidated_task_without_work_item () =
  with_db @@ fun db ->
  let task_id = enqueue_legacy_background_task ~db ~prompt:"legacy local" in
  invalidate_revalidated_legacy_background_task_without_work_item ~db ~task_id;
  let run_turn_calls = ref 0 in
  Background_task.start_queued_with_local_runner ~db
    ~run_turn:(fun
        ~key:_
        ~message:_
        ?model:_
        ?agent_name:_
        ?cwd:_
        ?context_snapshot:_
        ~interrupt_check:_
        ~on_history_update:_
        ()
      ->
      incr run_turn_calls;
      Lwt.return "unexpected local turn")
    ~on_task_started:(fun _ -> Lwt.return_unit)
    ~on_task_finished:(fun _ -> Lwt.return_unit)
    ();
  Lwt_main.run (Lwt.pause ());
  Alcotest.(check int) "run_turn not called" 0 !run_turn_calls;
  match Background_task.get_task ~db ~id:task_id with
  | None -> Alcotest.fail "missing failed background task"
  | Some failed ->
      Alcotest.(check string)
        "status failed" "failed"
        (Background_task.string_of_status failed.status);
      Alcotest.(check bool)
        "invalidation reported" true
        (match failed.result_preview with
        | Some msg -> contains msg "invalidated"
        | None -> false)

let test_local_runner_revalidates_work_item_snapshot () =
  with_db @@ fun db ->
  let _pid, key, _link, binding = seed_ada ~db in
  let snap =
    assert_ok
      (Job.capture_for_delayed_job ~db ~actor_key:key
         ~delayed_job_id:"local_runner_guard_item"
         ~account_binding_id:binding.id ~room_id ~now:fixed_now ())
  in
  let item =
    assert_ok
      (Bg.enqueue_work_item ~db
         ~req:(bg_req ~dedup:"local_runner_guard_item" ())
         ~actor_snapshot:snap ())
  in
  let task_id =
    assert_ok
      (Background_task.enqueue ~db ~runner:Background_task.Local
         ~require_git:false ~automerge:false ~use_worktree:false
         ~repo_path:(Filename.get_temp_dir_name ())
         ~prompt:"durable local dispatch" ())
  in
  Github_work_item.attach_task ~db ~id:item.id ~background_task_id:task_id;
  assert_ok (Github_work_item.require_actor_snapshot_current ~db item);
  ignore
    (assert_ok
       (B.update_authorization_status ~db ~id:binding.id ~status:B.Revoked
          ~now:(fixed_now +. 1.) ()));
  let run_turn_calls = ref 0 in
  Background_task.start_queued_with_local_runner ~db
    ~run_turn:(fun
        ~key:_
        ~message:_
        ?model:_
        ?agent_name:_
        ?cwd:_
        ?context_snapshot:_
        ~interrupt_check:_
        ~on_history_update:_
        ()
      ->
      incr run_turn_calls;
      Lwt.return "unexpected local turn")
    ~on_task_started:(fun _ -> Lwt.return_unit)
    ~on_task_finished:(fun _ -> Lwt.return_unit)
    ();
  Lwt_main.run (Lwt.pause ());
  Alcotest.(check int) "run_turn not called" 0 !run_turn_calls;
  match Background_task.get_task ~db ~id:task_id with
  | None -> Alcotest.fail "missing failed background task"
  | Some failed ->
      Alcotest.(check string)
        "status failed" "failed"
        (Background_task.string_of_status failed.status);
      Alcotest.(check bool)
        "authority currentness reported" true
        (match failed.result_preview with
        | Some msg ->
            contains msg "human attribution"
            || contains msg "no longer executable"
            || contains msg "unusable"
        | None -> false)

let test_worker_retry_rejects_invalidated_task_without_work_item () =
  with_db @@ fun db ->
  let task_id = enqueue_legacy_background_task ~db ~prompt:"legacy retry" in
  invalidate_revalidated_legacy_background_task_without_work_item ~db ~task_id;
  Background_task.finish ~db ~id:task_id ~status:Background_task.Failed
    ~result_preview:"initial failure";
  (match Background_task.retry ~db ~id:task_id with
  | Ok _ -> Alcotest.fail "retry must not requeue an invalidated legacy task"
  | Error msg ->
      Alcotest.(check bool) "invalidation reported" true
        (contains msg "invalidated"));
  match Background_task.get_task ~db ~id:task_id with
  | None -> Alcotest.fail "missing failed background task"
  | Some task ->
      Alcotest.(check string)
        "task remains failed" "failed"
        (Background_task.string_of_status task.status);
      Alcotest.(check int) "retry count unchanged" 0 task.retry_count

(* 9. legacy jobs without snapshot still allowed when not required *)
let test_legacy_without_snapshot () =
  with_db @@ fun db ->
  let intent = sample_intent ~id:"ghdi_legacy" () in
  let entry =
    assert_ok
      (O.enqueue ~db ~room_id ~item_key:intent.item_key ~intent ~now:fixed_now
         ())
  in
  Alcotest.(check bool)
    "no snap" true
    (Option.is_none entry.actor_snapshot_json);
  match
    Job.prepare_execution_of_json ~db ~job_id:entry.id
      ~snapshot_json:entry.actor_snapshot_json ()
  with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "legacy should not require envelope"
  | Error e -> Alcotest.fail e

(* 8. room history cannot supply identity; storage rejects tokens *)
let test_guards_and_token_reject () =
  let msg = Job.reject_identity_from_room_history ~room_id in
  Alcotest.(check bool) "room" true (contains msg "room");
  Alcotest.(check bool)
    "cannot supply" true
    (contains msg "cannot supply" || contains msg "cannot");
  let initiating = sample_key ~user:"user-ada" () in
  let other = sample_key ~user:"user-bob" () in
  (match Job.assert_not_borrowed_identity ~initiating ~claimed:other with
  | Ok () -> Alcotest.fail "borrow"
  | Error e ->
      Alcotest.(check bool)
        "other" true
        (contains e "another participant" || contains e "claimed"));
  let bad =
    `Assoc
      [
        ("version", `Int 1);
        ("id", `String "x");
        ("access_token", `String "secret-value");
      ]
  in
  match Job.snapshot_of_storage_json bad with
  | Ok _ -> Alcotest.fail "token payload must fail"
  | Error e ->
      Alcotest.(check bool)
        "token reject" true
        (contains e "token" || contains e "secret")

let suite =
  [
    ("capture for delayed job", `Quick, test_capture_for_delayed_job);
    ( "outbox enqueue claim retry preserve snapshot",
      `Quick,
      test_outbox_enqueue_claim_retry_preserve_snapshot );
    ( "outbox execution reresolve and fail closed",
      `Quick,
      test_outbox_execution_reresolve_and_fail_closed );
    ( "never borrow other participant",
      `Quick,
      test_never_borrow_other_participant );
    ( "work item cancel retry preserve and fail closed",
      `Quick,
      test_work_item_cancel_retry_preserve_and_fail_closed );
    ( "background plan and account revocation",
      `Quick,
      test_background_plan_and_account_revocation );
    ( "worker retry revalidates work item snapshot",
      `Quick,
      test_worker_retry_revalidates_work_item_snapshot );
    ( "worker spawn rejects invalidated task without work item",
      `Quick,
      test_worker_spawn_rejects_invalidated_task_without_work_item );
    ( "local runner rejects invalidated task without work item",
      `Quick,
      test_local_runner_rejects_invalidated_task_without_work_item );
    ( "local runner revalidates work item snapshot",
      `Quick,
      test_local_runner_revalidates_work_item_snapshot );
    ( "worker retry rejects invalidated task without work item",
      `Quick,
      test_worker_retry_rejects_invalidated_task_without_work_item );
    ("legacy without snapshot", `Quick, test_legacy_without_snapshot);
    ("guards and token reject", `Quick, test_guards_and_token_reject);
  ]
