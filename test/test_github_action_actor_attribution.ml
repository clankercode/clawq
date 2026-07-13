(** Tests for Actor_snapshot on P19 action intents / confirmations
    (P21.M1.E3.T002). *)

module P = Principal_identity
module S = Principal_identity_store
module B = Github_account_binding
module A = Actor_snapshot
module Attr = Github_action_actor_attribution
module W = Github_action_workflow
module Collab = Github_collab_actions
module RS = Github_route_store

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  S.ensure_schema db;
  B.ensure_schema db;
  RS.ensure_schema db;
  Setup_plan_apply.init_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_785_300_000.0
let base_revision = "rev-config-1"
let room_id = "room-teams-shared"
let item_key = "item:acme/widget:pr:7"
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

let principal_plan =
  Setup_plan.{ id = "prin_ada"; kind = Principal; label = Some "Ada" }

let caps ~reply : RS.capability_policy =
  {
    allow_reply = reply;
    allow_label = false;
    allow_assign = false;
    allow_review = false;
    allow_merge = false;
    allow_close = false;
    extra = [];
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

let comment_action =
  W.Collab (Collab.Comment { item_key; body = "LGTM — confirm with snapshot." })

let contains hay needle =
  let hay = String.lowercase_ascii hay in
  let needle = String.lowercase_ascii needle in
  try
    let _ = Str.search_forward (Str.regexp_string needle) hay 0 in
    true
  with Not_found -> false

let seed_ada ~db =
  let principal_id = pid "prin_ada" in
  seed_principal ~db ~id:"prin_ada" ();
  let key = sample_key () in
  let _actor, link = seed_actor_and_link ~db ~principal_id ~key () in
  let binding = seed_binding ~db ~principal_id () in
  (principal_id, key, link, binding)

(* 1. capture + attach embeds snapshot on plan and restamps pending store *)
let test_preview_pins_actor_snapshot () =
  with_db @@ fun db ->
  let _pid, key, _link, binding = seed_ada ~db in
  let route = make_route ~id:"rt_snap" ~policy:(caps ~reply:true) in
  let plan =
    assert_ok
      (W.preview ~db ~principal:principal_plan ~room_id ~action:comment_action
         ~base_revision ~route ~actor_key:key ~account_binding_id:binding.id
         ~session_id:"sess_ada" ~now:fixed_now ())
  in
  Alcotest.(check bool) "has snapshot" true (Attr.has_actor_snapshot plan);
  let snap =
    match assert_ok (Attr.snapshot_of_plan plan) with
    | Some s -> s
    | None -> Alcotest.fail "expected snapshot"
  in
  Alcotest.(check string)
    "principal lineage" "prin_ada"
    (P.principal_id_to_string snap.lineage.principal_id);
  Alcotest.(check (option string))
    "account lineage" (Some "lineage_ada") snap.lineage.account_lineage_id;
  Alcotest.(check (option string))
    "room source context only" (Some room_id) snap.source.room_id;
  Alcotest.(check (option string))
    "intent id = plan id" (Some plan.id) snap.work_refs.intent_id;
  Alcotest.(check bool) "never authority" false (A.is_authority snap);
  (* Stored plan keeps the same digest + snapshot. *)
  (match Setup_plan_apply.get_plan ~db ~plan_id:plan.id with
  | None -> Alcotest.fail "plan not stored"
  | Some stored ->
      Alcotest.(check string) "digest match" plan.digest stored.digest;
      Alcotest.(check bool)
        "stored has snapshot" true
        (Attr.has_actor_snapshot stored));
  let planned = Yojson.Safe.to_string plan.planned_state in
  Alcotest.(check bool)
    "planned_state has actor_snapshot" true
    (contains planned "actor_snapshot");
  Alcotest.(check bool)
    "apply data has lineage" true
    (contains (Yojson.Safe.to_string plan.apply_payload.data) "actor_lineage");
  Alcotest.(check bool)
    "readiness mentions snapshot" true
    (List.exists
       (fun (r : Setup_plan.readiness_item) -> r.name = "actor_snapshot")
       plan.readiness)

(* 2. apply re-resolves usable authority; ordinary collab fail-closes without
   a live REST dispatcher (no false Applied receipt). *)
let test_apply_reresolves_usable_authority () =
  with_db @@ fun db ->
  let _pid, key, _link, binding = seed_ada ~db in
  let route = make_route ~id:"rt_apply" ~policy:(caps ~reply:true) in
  let plan =
    assert_ok
      (W.preview ~db ~principal:principal_plan ~room_id ~action:comment_action
         ~base_revision ~route ~actor_key:key ~account_binding_id:binding.id
         ~now:fixed_now ())
  in
  let env =
    match Attr.prepare_dispatch ~db ~plan () with
    | Ok env -> env
    | Error inv -> Alcotest.fail (Attr.string_of_invalidation inv)
  in
  Alcotest.(check bool) "live usable" true env.live_authority.usable;
  Alcotest.(check string) "plan id" plan.id env.plan_id;
  Alcotest.(check (option string))
    "account lineage on envelope" (Some "lineage_ada") env.account_lineage_id;
  match
    assert_ok
      (W.apply_confirmed ~db ~plan_id:plan.id ~digest:plan.digest
         ~principal:principal_plan ~current_base_revision:base_revision
         ~now:fixed_now ())
  with
  | Setup_plan_apply.Applied _ ->
      Alcotest.fail
        "collab apply must fail closed without a live GitHub REST dispatcher"
  | Setup_plan_apply.Rejected { reason; message } ->
      Alcotest.(check string)
        "apply_error" "apply_error"
        (Setup_plan_apply.string_of_reject_reason reason);
      Alcotest.(check bool) "mentions dispatcher" true
        (contains message "dispatcher")

(* 3. unlink / split invalidates confirmation at apply *)
let test_unlink_invalidates_confirmation () =
  with_db @@ fun db ->
  let _principal_id, key, link, binding = seed_ada ~db in
  let route = make_route ~id:"rt_unlink" ~policy:(caps ~reply:true) in
  let plan =
    assert_ok
      (W.preview ~db ~principal:principal_plan ~room_id ~action:comment_action
         ~base_revision ~route ~actor_key:key ~account_binding_id:binding.id
         ~now:fixed_now ())
  in
  (* Simulate split: supersede old link, new principal + link, move actor. *)
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
  match
    assert_ok
      (W.apply_confirmed ~db ~plan_id:plan.id ~digest:plan.digest
         ~principal:principal_plan ~current_base_revision:base_revision
         ~now:(fixed_now +. 3.) ())
  with
  | Setup_plan_apply.Applied _ ->
      Alcotest.fail "expected confirmation invalidation after unlink"
  | Setup_plan_apply.Rejected { message; _ } ->
      Alcotest.(check bool)
        "mentions invalidation" true
        (contains message "invalidat"
        || contains message "authority"
        || contains message "unusable")

(* 4. target / policy change invalidates confirmation *)
let test_target_change_invalidates () =
  with_db @@ fun db ->
  let _pid, key, _link, _binding = seed_ada ~db in
  let route = make_route ~id:"rt_tgt" ~policy:(caps ~reply:true) in
  let plan =
    assert_ok
      (W.preview ~db ~principal:principal_plan ~room_id ~action:comment_action
         ~base_revision ~route ~actor_key:key ~now:fixed_now ())
  in
  let current =
    {
      (Attr.target_fingerprint_of_plan plan) with
      item_key = Some "item:acme/widget:pr:999";
    }
  in
  match
    assert_ok
      (W.apply_confirmed ~db ~plan_id:plan.id ~digest:plan.digest
         ~principal:principal_plan ~current_base_revision:base_revision
         ~current_target:current ~now:fixed_now ())
  with
  | Setup_plan_apply.Applied _ -> Alcotest.fail "expected target change reject"
  | Setup_plan_apply.Rejected { message; _ } ->
      Alcotest.(check bool)
        "mentions target/item" true
        (contains message "item_key" || contains message "target")

(* 5. Room history cannot supply identity *)
let test_room_history_cannot_supply_identity () =
  let msg = Attr.reject_identity_from_room_history ~room_id in
  Alcotest.(check bool) "mentions room" true (contains msg "room");
  Alcotest.(check bool)
    "mentions cannot supply" true
    (contains msg "cannot supply" || contains msg "cannot");
  let initiating = sample_key ~user:"user-ada" () in
  let other = sample_key ~user:"user-bob" () in
  (match Attr.assert_not_borrowed_identity ~initiating ~claimed:other with
  | Ok () -> Alcotest.fail "expected borrow reject"
  | Error e ->
      Alcotest.(check bool)
        "mentions other participant" true
        (contains e "another participant" || contains e "claimed"));
  match Attr.assert_not_borrowed_identity ~initiating ~claimed:initiating with
  | Ok () -> ()
  | Error e -> Alcotest.fail e

(* 6. attach_to_plan pure embed + extract roundtrip; no token material *)
let test_attach_extract_roundtrip_no_tokens () =
  with_db @@ fun db ->
  let principal_id, key, _link, binding = seed_ada ~db in
  let snap =
    assert_ok
      (Attr.capture_for_intent ~db ~actor_key:key ~account_binding_id:binding.id
         ~room_id ~intent_id:"intent_pure" ~now:fixed_now ())
  in
  let route = make_route ~id:"rt_pure" ~policy:(caps ~reply:true) in
  (* Build a plan without going through actor attach first. *)
  let plan0 =
    assert_ok
      (Collab.plan_action ~db ~principal:principal_plan ~room_id
         ~action:(Collab.Comment { item_key; body = "pure attach" })
         ~base_revision ~route ~now:fixed_now ())
  in
  let plan = Attr.attach_to_plan ~plan:plan0 ~snapshot:snap () in
  Alcotest.(check bool)
    "digest changed after attach" true
    (plan.digest <> plan0.digest);
  let back =
    match assert_ok (Attr.snapshot_of_plan plan) with
    | Some s -> s
    | None -> Alcotest.fail "missing"
  in
  Alcotest.(check string) "id" snap.id back.id;
  Alcotest.(check string)
    "principal"
    (P.principal_id_to_string principal_id)
    (P.principal_id_to_string back.lineage.principal_id);
  let data_json = plan.apply_payload.data in
  Alcotest.(check bool)
    "no token material" false
    (A.contains_token_material data_json);
  Alcotest.(check bool)
    "authority false on data" true
    (contains (Yojson.Safe.to_string data_json) "\"authority\":false"
    || contains (Yojson.Safe.to_string data_json) "authority")

(* 7. legacy plan without snapshot still reaches apply; ordinary collab
   fail-closes without a live REST dispatcher (no false Applied receipt). *)
let test_legacy_plan_without_snapshot_still_applies () =
  with_db @@ fun db ->
  let route = make_route ~id:"rt_legacy" ~policy:(caps ~reply:true) in
  let plan =
    assert_ok
      (W.preview ~db ~principal:principal_plan ~room_id ~action:comment_action
         ~base_revision ~route ~now:fixed_now ())
  in
  Alcotest.(check bool) "no snapshot" false (Attr.has_actor_snapshot plan);
  match
    assert_ok
      (W.apply_confirmed ~db ~plan_id:plan.id ~digest:plan.digest
         ~principal:principal_plan ~current_base_revision:base_revision
         ~now:fixed_now ())
  with
  | Setup_plan_apply.Applied _ ->
      Alcotest.fail
        "collab apply must fail closed without a live GitHub REST dispatcher"
  | Setup_plan_apply.Rejected { reason; message } ->
      Alcotest.(check string)
        "apply_error" "apply_error"
        (Setup_plan_apply.string_of_reject_reason reason);
      Alcotest.(check bool) "mentions dispatcher" true
        (contains message "dispatcher")

(* 8. account revocation invalidates confirmation *)
let test_account_lineage_change_invalidates () =
  with_db @@ fun db ->
  let _principal_id, key, _link, binding = seed_ada ~db in
  let route = make_route ~id:"rt_acct" ~policy:(caps ~reply:true) in
  let plan =
    assert_ok
      (W.preview ~db ~principal:principal_plan ~room_id ~action:comment_action
         ~base_revision ~route ~actor_key:key ~account_binding_id:binding.id
         ~now:fixed_now ())
  in
  ignore
    (assert_ok
       (B.update_authorization_status ~db ~id:binding.id ~status:B.Revoked
          ~now:(fixed_now +. 1.) ()));
  match
    assert_ok
      (W.apply_confirmed ~db ~plan_id:plan.id ~digest:plan.digest
         ~principal:principal_plan ~current_base_revision:base_revision
         ~now:(fixed_now +. 2.) ())
  with
  | Setup_plan_apply.Applied _ ->
      Alcotest.fail "expected account revocation invalidation"
  | Setup_plan_apply.Rejected { message; _ } ->
      Alcotest.(check bool)
        "mentions authority/account" true
        (contains message "account"
        || contains message "authority"
        || contains message "invalidat"
        || contains message "lineage"
        || contains message "unusable")

let suite =
  [
    ("preview pins actor snapshot", `Quick, test_preview_pins_actor_snapshot);
    ( "apply re-resolves usable authority then fail-closes without dispatcher",
      `Quick,
      test_apply_reresolves_usable_authority );
    ( "unlink invalidates confirmation",
      `Quick,
      test_unlink_invalidates_confirmation );
    ("target change invalidates", `Quick, test_target_change_invalidates);
    ( "room history cannot supply identity",
      `Quick,
      test_room_history_cannot_supply_identity );
    ( "attach extract roundtrip no tokens",
      `Quick,
      test_attach_extract_roundtrip_no_tokens );
    ( "legacy plan without snapshot fail-closes without dispatcher",
      `Quick,
      test_legacy_plan_without_snapshot_still_applies );
    ( "account lineage change invalidates",
      `Quick,
      test_account_lineage_change_invalidates );
  ]
