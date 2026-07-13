(** Tests for room-agent confirm/apply + repair via shared framework
    (P20.M2.E1.T002). *)

open Setup_room_wizard_types
module Apply = Room_agent_setup_apply

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  Apply.init_schemas db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_700_000_000.0
let base_revision = "rev-room-agent-1"
let room = "19:abc@thread.tacv2"

let sample_principal =
  Setup_plan.
    {
      id = "principal:pilot-admin";
      kind = Principal;
      label = Some "Pilot Admin";
    }

let global_actor : Setup_plan_consent.actor =
  {
    principal_id = sample_principal.id;
    role = Global_admin;
    source_room_id = Some "admin-room";
  }

let room_actor ~room_id : Setup_plan_consent.actor =
  {
    principal_id = sample_principal.id;
    role = Room_admin room_id;
    source_room_id = Some room_id;
  }

let other_room_actor : Setup_plan_consent.actor =
  {
    principal_id = sample_principal.id;
    role = Room_admin "19:other@thread.tacv2";
    source_room_id = Some "19:other@thread.tacv2";
  }

let make_teams_cfg () : Runtime_config.t =
  let json =
    Yojson.Safe.from_string
      {|{"channels": {"teams": {"app_id": "test-app", "app_secret": "secret-value-xyz", "tenant_id": "tenant", "webhook_path": "/webhook", "service_url": "https://smba.trafficmanager.net"}}}|}
  in
  Config_loader.parse_config json

let make_teams_cfg_with_bundle () : Runtime_config.t =
  let json =
    Yojson.Safe.from_string
      {|{
        "channels": {
          "teams": {
            "app_id": "test-app",
            "app_secret": "secret-value-xyz",
            "tenant_id": "tenant",
            "webhook_path": "/webhook",
            "service_url": "https://smba.trafficmanager.net"
          }
        },
        "access_bundles": [
          {
            "id": "pilot-tools",
            "status": "active",
            "allowed_tools": ["shell", "web_search"]
          }
        ]
      }|}
  in
  Config_loader.parse_config json

let sample_state ?(profile_id = "pilot-agent") ?(model = "openai:gpt-5.4")
    ?(connector_type = "teams") ?(connector_room = room)
    ?(access_bundle_ids = []) () : wizard_state =
  {
    default_state with
    profile_id;
    model;
    max_tool_iterations = 25;
    access_bundle_ids;
    connector_type;
    connector_room;
    connector_active = true;
    memory_scope_kind = "room";
    memory_scope_key = connector_room;
    budget_reset_period = "monthly";
  }

let make_req ?(actor = global_actor) ?(destination_room = Some room)
    ?(revision = base_revision) ~plan_id ~digest () : Apply.apply_request =
  {
    plan_id;
    digest;
    principal = sample_principal;
    current_base_revision = revision;
    destination_room;
    now = fixed_now;
    actor;
  }

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e
let contains hay needle = Test_helpers.string_contains hay needle

(* 1. Happy path: plan_and_store → apply_confirmed receipt-only success *)
let test_apply_success_receipt_only () =
  with_db @@ fun db ->
  let cfg = make_teams_cfg () in
  let state = sample_state () in
  let plan =
    assert_ok
      (Apply.plan_and_store ~db ~cfg ~state ~principal:sample_principal
         ~base_revision ~now:fixed_now ~id:"plan_ra_ok" ())
  in
  Alcotest.(check bool)
    "Room_profile plan" true
    (Apply.is_room_profile_plan plan);
  Alcotest.(check bool) "readiness ok" true (Setup_plan.readiness_ok plan);
  let config_calls = ref 0 in
  let outcome =
    Apply.apply_confirmed ~db
      ~config_apply:(fun ~plan:_ ~receipt_id:_ ->
        incr config_calls;
        Ok ())
      (make_req ~plan_id:plan.id ~digest:plan.digest ())
  in
  match outcome with
  | Apply.Rejected { reason; message } ->
      Alcotest.fail (Printf.sprintf "%s: %s" reason message)
  | Apply.Applied { receipt_id; first_time; config_mutated; attached_bundles }
    ->
      Alcotest.(check bool) "first time" true first_time;
      Alcotest.(check bool)
        "receipt non-empty" true
        (String.length receipt_id > 0);
      Alcotest.(check bool) "config hook ran" true config_mutated;
      Alcotest.(check int) "config apply once" 1 !config_calls;
      Alcotest.(check (list string)) "no bundles" [] attached_bundles;
      let audits = Setup_plan_apply.list_audit ~db ~plan_id:plan.id () in
      Alcotest.(check bool)
        "applied audit" true
        (List.exists (fun a -> a.Setup_plan_apply.outcome = "applied") audits)

(* 2. Idempotent retry: same receipt, apply_ops not re-run, advanced revision ok *)
let test_apply_idempotent () =
  with_db @@ fun db ->
  let cfg = make_teams_cfg () in
  let state = sample_state () in
  let plan =
    assert_ok
      (Apply.plan_and_store ~db ~cfg ~state ~principal:sample_principal
         ~base_revision ~now:fixed_now ~id:"plan_ra_idem" ())
  in
  let calls = ref 0 in
  let config_apply ~plan:_ ~receipt_id:_ =
    incr calls;
    Ok ()
  in
  let first =
    Apply.apply_confirmed ~db ~config_apply
      (make_req ~plan_id:plan.id ~digest:plan.digest ())
  in
  let second =
    Apply.apply_confirmed ~db ~config_apply
      (make_req ~plan_id:plan.id ~digest:plan.digest
         ~revision:"rev-advanced-after-apply" ())
  in
  match (first, second) with
  | ( Apply.Applied { receipt_id = r1; first_time = true; _ },
      Apply.Applied { receipt_id = r2; first_time = false; config_mutated; _ } )
    ->
      Alcotest.(check string) "same receipt" r1 r2;
      Alcotest.(check int) "config apply once" 1 !calls;
      Alcotest.(check bool) "no re-mutate on idempotent" false config_mutated;
      let audits = Setup_plan_apply.list_audit ~db ~plan_id:plan.id () in
      Alcotest.(check bool)
        "idempotent audit" true
        (List.exists
           (fun a -> a.Setup_plan_apply.outcome = "applied_idempotent")
           audits)
  | Apply.Rejected { reason; message }, _
  | _, Apply.Rejected { reason; message } ->
      Alcotest.fail (Printf.sprintf "%s: %s" reason message)
  | _ -> Alcotest.fail "expected applied then idempotent applied"

(* 3. Room-admin authority for destination room succeeds *)
let test_room_admin_authority () =
  with_db @@ fun db ->
  let cfg = make_teams_cfg () in
  let state = sample_state () in
  let plan =
    assert_ok
      (Apply.plan_and_store ~db ~cfg ~state ~principal:sample_principal
         ~base_revision ~now:fixed_now ~id:"plan_ra_admin" ())
  in
  let outcome =
    Apply.apply_confirmed ~db
      (make_req ~actor:(room_actor ~room_id:room) ~plan_id:plan.id
         ~digest:plan.digest ())
  in
  match outcome with
  | Apply.Applied { first_time = true; _ } -> ()
  | Apply.Applied { first_time = false; _ } ->
      Alcotest.fail "expected first-time apply"
  | Apply.Rejected { reason; message } ->
      Alcotest.fail (Printf.sprintf "%s: %s" reason message)

(* 4. Cross-room without consent denied *)
let test_cross_room_authority_denied () =
  with_db @@ fun db ->
  let cfg = make_teams_cfg () in
  let state = sample_state () in
  let plan =
    assert_ok
      (Apply.plan_and_store ~db ~cfg ~state ~principal:sample_principal
         ~base_revision ~now:fixed_now ~id:"plan_ra_xroom" ())
  in
  let outcome =
    Apply.apply_confirmed ~db
      (make_req ~actor:other_room_actor ~plan_id:plan.id ~digest:plan.digest ())
  in
  match outcome with
  | Apply.Applied _ -> Alcotest.fail "expected authority denied"
  | Apply.Rejected { reason; message } ->
      Alcotest.(check string) "reason" "authority_denied" reason;
      let lower = String.lowercase_ascii message in
      Alcotest.(check bool)
        "mentions consent or admin" true
        (contains lower "consent" || contains lower "admin"
       || contains lower "room")

(* 5. Cross-room with explicit destination consent allowed *)
let test_cross_room_with_consent () =
  with_db @@ fun db ->
  let cfg = make_teams_cfg () in
  let state = sample_state () in
  let plan =
    assert_ok
      (Apply.plan_and_store ~db ~cfg ~state ~principal:sample_principal
         ~base_revision ~now:fixed_now ~id:"plan_ra_consent" ())
  in
  let dest_admin : Setup_plan_consent.actor =
    {
      principal_id = "principal:dest-admin";
      role = Room_admin room;
      source_room_id = Some room;
    }
  in
  ignore
    (assert_ok
       (Setup_plan_consent.grant_consent ~db ~destination_room_id:room
          ~actor:dest_admin ~plan_id:plan.id ~signal:Explicit_confirm
          ~now:fixed_now ()));
  let outcome =
    Apply.apply_confirmed ~db
      (make_req ~actor:other_room_actor ~plan_id:plan.id ~digest:plan.digest ())
  in
  match outcome with
  | Apply.Applied { first_time = true; _ } -> ()
  | Apply.Applied { first_time = false; _ } ->
      Alcotest.fail "expected first-time apply"
  | Apply.Rejected { reason; message } ->
      Alcotest.fail (Printf.sprintf "%s: %s" reason message)

(* 6. Managed bundle attach on apply when plan names bundles *)
let test_bundle_attach () =
  with_db @@ fun db ->
  let cfg = make_teams_cfg_with_bundle () in
  let state = sample_state ~access_bundle_ids:[ "pilot-tools" ] () in
  let plan =
    assert_ok
      (Apply.plan_and_store ~db ~cfg ~state ~principal:sample_principal
         ~base_revision ~now:fixed_now ~id:"plan_ra_bundle" ())
  in
  Alcotest.(check bool) "readiness ok" true (Setup_plan.readiness_ok plan);
  let outcome =
    Apply.apply_confirmed ~db (make_req ~plan_id:plan.id ~digest:plan.digest ())
  in
  match outcome with
  | Apply.Rejected { reason; message } ->
      Alcotest.fail (Printf.sprintf "%s: %s" reason message)
  | Apply.Applied { attached_bundles; first_time; config_mutated; _ } ->
      Alcotest.(check bool) "first time" true first_time;
      Alcotest.(check bool) "receipt-only default" false config_mutated;
      Alcotest.(check (list string))
        "attached pilot-tools" [ "pilot-tools" ] attached_bundles;
      Alcotest.(check bool)
        "setup-owned" true
        (Setup_plan_bundle.is_setup_owned ~db ~room_id:room
           ~bundle_id:"pilot-tools" ());
      let feature = Apply.feature_id_for_profile ~profile_id:"pilot-agent" in
      Alcotest.(check int)
        "one feature" 1
        (Setup_plan_bundle.count_attached_features ~db ~room_id:room
           ~bundle_id:"pilot-tools" ());
      Alcotest.(check bool)
        "feature id shape" true
        (String.starts_with ~prefix:"room_profile:" feature)

(* 7. Stale revision rejected; repair rebuilds applyable plan *)
let test_stale_and_repair () =
  with_db @@ fun db ->
  let cfg = make_teams_cfg () in
  let state = sample_state () in
  let plan =
    assert_ok
      (Apply.plan_and_store ~db ~cfg ~state ~principal:sample_principal
         ~base_revision ~now:fixed_now ~id:"plan_ra_stale" ())
  in
  let stale =
    Apply.apply_confirmed ~db
      (make_req ~plan_id:plan.id ~digest:plan.digest
         ~revision:"rev-moved-forward" ())
  in
  (match stale with
  | Apply.Rejected { reason; _ } ->
      Alcotest.(check string) "stale" "stale_revision" reason
  | Apply.Applied _ -> Alcotest.fail "expected stale revision reject");
  match
    Apply.repair_if_stale ~db ~cfg ~state ~plan
      ~current_base_revision:"rev-moved-forward" ~now:fixed_now ()
  with
  | Error e -> Alcotest.fail e
  | Ok (`Current _) -> Alcotest.fail "expected repaired plan"
  | Ok (`Repaired repaired) -> (
      Alcotest.(check bool) "new id" true (repaired.id <> plan.id);
      Alcotest.(check string)
        "new base" "rev-moved-forward" repaired.base_revision;
      Alcotest.(check bool)
        "same principal" true
        (repaired.principal.id = plan.principal.id);
      let outcome =
        Apply.apply_confirmed ~db
          (make_req ~plan_id:repaired.id ~digest:repaired.digest
             ~revision:"rev-moved-forward" ())
      in
      match outcome with
      | Apply.Applied { first_time = true; _ } -> ()
      | Apply.Applied { first_time = false; _ } ->
          Alcotest.fail "expected first-time apply of repaired plan"
      | Apply.Rejected { reason; message } ->
          Alcotest.fail (Printf.sprintf "%s: %s" reason message))

(* 8. Digest mismatch rejected; no bundle attach *)
let test_digest_mismatch () =
  with_db @@ fun db ->
  let cfg = make_teams_cfg_with_bundle () in
  let state = sample_state ~access_bundle_ids:[ "pilot-tools" ] () in
  let plan =
    assert_ok
      (Apply.plan_and_store ~db ~cfg ~state ~principal:sample_principal
         ~base_revision ~now:fixed_now ~id:"plan_ra_dig" ())
  in
  let outcome =
    Apply.apply_confirmed ~db
      {
        (make_req ~plan_id:plan.id ~digest:plan.digest ()) with
        digest = String.make 64 'a';
      }
  in
  match outcome with
  | Apply.Applied _ -> Alcotest.fail "expected digest mismatch"
  | Apply.Rejected { reason; _ } ->
      Alcotest.(check string) "reason" "digest_mismatch" reason;
      Alcotest.(check bool)
        "no linkage" false
        (Setup_plan_bundle.is_setup_owned ~db ~room_id:room
           ~bundle_id:"pilot-tools" ())

(* 9. Current plan repair is a no-op when fresh *)
let test_repair_current_noop () =
  with_db @@ fun db ->
  let cfg = make_teams_cfg () in
  let state = sample_state () in
  let plan =
    assert_ok
      (Apply.plan_and_store ~db ~cfg ~state ~principal:sample_principal
         ~base_revision ~now:fixed_now ~id:"plan_ra_cur" ())
  in
  match
    Apply.repair_if_stale ~db ~cfg ~state ~plan
      ~current_base_revision:base_revision ~now:fixed_now ()
  with
  | Ok (`Current p) -> Alcotest.(check string) "same id" plan.id p.id
  | Ok (`Repaired _) -> Alcotest.fail "should not rebuild fresh plan"
  | Error e -> Alcotest.fail e

let suite =
  [
    ("apply success receipt-only", `Quick, test_apply_success_receipt_only);
    ("apply idempotent", `Quick, test_apply_idempotent);
    ("room-admin authority", `Quick, test_room_admin_authority);
    ("cross-room authority denied", `Quick, test_cross_room_authority_denied);
    ("cross-room with consent", `Quick, test_cross_room_with_consent);
    ("managed bundle attach", `Quick, test_bundle_attach);
    ("stale revision and repair", `Quick, test_stale_and_repair);
    ("digest mismatch", `Quick, test_digest_mismatch);
    ("repair current is no-op", `Quick, test_repair_current_noop);
  ]
