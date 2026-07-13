(** Shared setup-framework contract tests (P20.M2.E1.T003).

    Runs both the room-agent and GitHub-route setup adapters through the same
    planning/apply failure cases so they cannot drift from [Setup_plan_apply]
    semantics:

    - digest mismatch apply reject
    - stale base_revision reject
    - authority denied
    - idempotent re-apply

    Boundary: docs/setup-framework-boundary.md Canonical:
    docs/plans/2026-07-12-github-item-room-routing.md ADR:
    docs/adr/0003-require-plan-confirm-apply-for-agent-setup.md *)

open Setup_room_wizard_types
module Gh_store = Github_route_store
module Gh_admin = Github_route_admin
module Gh_apply = Github_route_apply
module Ra_apply = Room_agent_setup_apply

(* ── Shared result shape ─────────────────────────────────────────── *)

type contract_outcome =
  | Applied of { receipt_id : string; first_time : bool }
  | Rejected of { reason : string; message : string }

let string_of_outcome = function
  | Applied { receipt_id; first_time } ->
      Printf.sprintf "Applied(receipt=%s,first=%b)" receipt_id first_time
  | Rejected { reason; message } ->
      Printf.sprintf "Rejected(%s: %s)" reason message

let assert_rejected ~adapter ~case outcome expected_reason =
  match outcome with
  | Rejected { reason; message } ->
      Alcotest.(check string)
        (Printf.sprintf "%s/%s reject reason" adapter case)
        expected_reason reason;
      Alcotest.(check bool)
        (Printf.sprintf "%s/%s reject message non-empty" adapter case)
        true
        (String.trim message <> "")
  | Applied _ as o ->
      Alcotest.fail
        (Printf.sprintf "%s/%s: expected reject %s, got %s" adapter case
           expected_reason (string_of_outcome o))

let assert_applied ~adapter ~case ~first_time_expected outcome =
  match outcome with
  | Applied { receipt_id; first_time } ->
      Alcotest.(check bool)
        (Printf.sprintf "%s/%s first_time" adapter case)
        first_time_expected first_time;
      Alcotest.(check bool)
        (Printf.sprintf "%s/%s receipt non-empty" adapter case)
        true
        (String.length receipt_id > 0);
      receipt_id
  | Rejected { reason; message } ->
      Alcotest.fail
        (Printf.sprintf "%s/%s: expected applied, got %s: %s" adapter case
           reason message)

(* ── Room-agent adapter fixtures ─────────────────────────────────── *)

let ra_fixed_now = 1_700_000_000.0
let ra_base_revision = "rev-contract-room-1"
let ra_room = "19:contract@thread.tacv2"

let ra_principal =
  Setup_plan.
    {
      id = "principal:contract-admin";
      kind = Principal;
      label = Some "Contract Admin";
    }

let ra_global_actor : Setup_plan_consent.actor =
  {
    principal_id = ra_principal.id;
    role = Global_admin;
    source_room_id = Some "admin-room";
  }

let ra_denied_actor : Setup_plan_consent.actor =
  {
    principal_id = ra_principal.id;
    role = Room_admin "19:other@thread.tacv2";
    source_room_id = Some "19:other@thread.tacv2";
  }

let ra_teams_cfg () : Runtime_config.t =
  let json =
    Yojson.Safe.from_string
      {|{"channels": {"teams": {"app_id": "test-app", "app_secret": "secret-value-xyz", "tenant_id": "tenant", "webhook_path": "/webhook", "service_url": "https://smba.trafficmanager.net"}}}|}
  in
  Config_loader.parse_config json

let ra_state () : wizard_state =
  {
    default_state with
    profile_id = "contract-agent";
    model = "openai:gpt-5.4";
    max_tool_iterations = 25;
    connector_type = "teams";
    connector_room = ra_room;
    connector_active = true;
    memory_scope_kind = "room";
    memory_scope_key = ra_room;
    budget_reset_period = "monthly";
  }

let with_ra_db f =
  let db = Sqlite3.db_open ":memory:" in
  Ra_apply.init_schemas db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let ra_of_outcome = function
  | Ra_apply.Applied { receipt_id; first_time; _ } ->
      Applied { receipt_id; first_time }
  | Ra_apply.Rejected { reason; message } -> Rejected { reason; message }

let ra_plan_and_store ~db ~id =
  match
    Ra_apply.plan_and_store ~db ~cfg:(ra_teams_cfg ()) ~state:(ra_state ())
      ~principal:ra_principal ~base_revision:ra_base_revision ~now:ra_fixed_now
      ~id ()
  with
  | Ok p -> p
  | Error e -> Alcotest.fail ("room-agent plan_and_store: " ^ e)

let ra_apply ~db ~(plan : Setup_plan.t) ?(digest = plan.digest)
    ?(revision = ra_base_revision) ?(actor = ra_global_actor) () =
  ra_of_outcome
    (Ra_apply.apply_confirmed ~db
       {
         plan_id = plan.id;
         digest;
         principal = ra_principal;
         current_base_revision = revision;
         destination_room = Some ra_room;
         now = ra_fixed_now;
         actor;
       })

(* ── GitHub-route adapter fixtures ───────────────────────────────── *)

let gh_fixed_now = 1_700_000_000.0
let gh_base_revision = "rev-contract-gh-1"
let gh_room_id = "room-contract-1"
let gh_room = Gh_store.Room gh_room_id
let gh_selector = Gh_store.Repo "Acme/Contract"

let gh_principal =
  Setup_plan.
    {
      id = "principal:contract-alice";
      kind = Principal;
      label = Some "Contract Alice";
    }

let gh_global_actor : Setup_plan_consent.actor =
  {
    principal_id = gh_principal.id;
    role = Global_admin;
    source_room_id = Some gh_room_id;
  }

let gh_denied_actor : Setup_plan_consent.actor =
  {
    principal_id = gh_principal.id;
    role = None_;
    source_room_id = Some gh_room_id;
  }

let with_gh_db f =
  let db = Sqlite3.db_open ":memory:" in
  Gh_store.ensure_schema db;
  Setup_plan_apply.init_schema db;
  Setup_plan_bundle.init_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let gh_of_outcome_with_audit ~db ~plan_id = function
  | Gh_apply.Applied { receipt_id; _ } ->
      (* GitHub adapter does not surface first_time; recover from audit. *)
      let audits = Setup_plan_apply.list_audit ~db ~plan_id () in
      let first_time =
        not
          (List.exists
             (fun a -> a.Setup_plan_apply.outcome = "applied_idempotent")
             audits)
      in
      Applied { receipt_id; first_time }
  | Gh_apply.Rejected { reason; message } -> Rejected { reason; message }

let gh_plan_create ~db ~route_id =
  match
    Gh_admin.plan_create ~db ~principal:gh_principal ~destination:gh_room
      ~selector:gh_selector ~base_revision:gh_base_revision ~now:gh_fixed_now
      ~route_id ()
  with
  | Ok p -> p
  | Error e -> Alcotest.fail ("github plan_create: " ^ e)

let gh_apply ~db ~(plan : Setup_plan.t) ?(digest = plan.digest)
    ?(revision = gh_base_revision) ?(actor = gh_global_actor) () =
  let req : Gh_apply.apply_request =
    {
      plan_id = plan.id;
      digest;
      principal = gh_principal;
      current_base_revision = revision;
      destination_room = Some gh_room_id;
      destination_session = None;
      now = gh_fixed_now;
      actor;
      auth_snapshot = None;
      installation = None;
    }
  in
  gh_of_outcome_with_audit ~db ~plan_id:plan.id
    (Gh_apply.apply_confirmed ~db req)

(* ── Contract cases (both adapters) ──────────────────────────────── *)

let test_digest_mismatch_room_agent () =
  with_ra_db @@ fun db ->
  let plan = ra_plan_and_store ~db ~id:"plan_contract_ra_dig" in
  let outcome = ra_apply ~db ~plan ~digest:(String.make 64 'a') () in
  assert_rejected ~adapter:"room_agent" ~case:"digest_mismatch" outcome
    "digest_mismatch"

let test_digest_mismatch_github () =
  with_gh_db @@ fun db ->
  let plan = gh_plan_create ~db ~route_id:"rt_contract_dig" in
  let outcome = gh_apply ~db ~plan ~digest:(String.make 64 'a') () in
  assert_rejected ~adapter:"github_route" ~case:"digest_mismatch" outcome
    "digest_mismatch";
  match Gh_store.get ~db ~id:"rt_contract_dig" with
  | Ok None -> ()
  | Ok (Some _) ->
      Alcotest.fail "github: route must not exist after digest reject"
  | Error e -> Alcotest.fail e

let test_stale_revision_room_agent () =
  with_ra_db @@ fun db ->
  let plan = ra_plan_and_store ~db ~id:"plan_contract_ra_stale" in
  let outcome = ra_apply ~db ~plan ~revision:"rev-moved-forward" () in
  assert_rejected ~adapter:"room_agent" ~case:"stale_revision" outcome
    "stale_revision"

let test_stale_revision_github () =
  with_gh_db @@ fun db ->
  let plan = gh_plan_create ~db ~route_id:"rt_contract_stale" in
  let outcome = gh_apply ~db ~plan ~revision:"rev-moved-forward" () in
  assert_rejected ~adapter:"github_route" ~case:"stale_revision" outcome
    "stale_revision";
  match Gh_store.get ~db ~id:"rt_contract_stale" with
  | Ok None -> ()
  | Ok (Some _) ->
      Alcotest.fail "github: route must not exist after stale reject"
  | Error e -> Alcotest.fail e

let test_authority_denied_room_agent () =
  with_ra_db @@ fun db ->
  let plan = ra_plan_and_store ~db ~id:"plan_contract_ra_auth" in
  let outcome = ra_apply ~db ~plan ~actor:ra_denied_actor () in
  assert_rejected ~adapter:"room_agent" ~case:"authority_denied" outcome
    "authority_denied"

let test_authority_denied_github () =
  with_gh_db @@ fun db ->
  let plan = gh_plan_create ~db ~route_id:"rt_contract_auth" in
  let outcome = gh_apply ~db ~plan ~actor:gh_denied_actor () in
  assert_rejected ~adapter:"github_route" ~case:"authority_denied" outcome
    "authority_denied";
  match Gh_store.get ~db ~id:"rt_contract_auth" with
  | Ok None -> ()
  | Ok (Some _) ->
      Alcotest.fail "github: route must not exist after authority deny"
  | Error e -> Alcotest.fail e

let test_idempotent_reapply_room_agent () =
  with_ra_db @@ fun db ->
  let plan = ra_plan_and_store ~db ~id:"plan_contract_ra_idem" in
  let first = ra_apply ~db ~plan () in
  let r1 =
    assert_applied ~adapter:"room_agent" ~case:"idempotent_first"
      ~first_time_expected:true first
  in
  (* Retry with advanced revision + would-be-denied actor: identity match wins. *)
  let second =
    ra_apply ~db ~plan ~revision:"rev-advanced-after-apply"
      ~actor:ra_denied_actor ()
  in
  let r2 =
    assert_applied ~adapter:"room_agent" ~case:"idempotent_second"
      ~first_time_expected:false second
  in
  Alcotest.(check string) "room_agent same receipt" r1 r2;
  let audits = Setup_plan_apply.list_audit ~db ~plan_id:plan.id () in
  Alcotest.(check bool)
    "room_agent applied_idempotent audit" true
    (List.exists
       (fun a -> a.Setup_plan_apply.outcome = "applied_idempotent")
       audits)

let test_idempotent_reapply_github () =
  with_gh_db @@ fun db ->
  let plan = gh_plan_create ~db ~route_id:"rt_contract_idem" in
  let first = gh_apply ~db ~plan () in
  let r1 =
    assert_applied ~adapter:"github_route" ~case:"idempotent_first"
      ~first_time_expected:true first
  in
  let second =
    gh_apply ~db ~plan ~revision:"rev-advanced-after-apply"
      ~actor:gh_denied_actor ()
  in
  let r2 =
    assert_applied ~adapter:"github_route" ~case:"idempotent_second"
      ~first_time_expected:false second
  in
  Alcotest.(check string) "github_route same receipt" r1 r2;
  let audits = Setup_plan_apply.list_audit ~db ~plan_id:plan.id () in
  Alcotest.(check bool)
    "github_route applied_idempotent audit" true
    (List.exists
       (fun a -> a.Setup_plan_apply.outcome = "applied_idempotent")
       audits);
  match Gh_store.get ~db ~id:"rt_contract_idem" with
  | Ok (Some r) -> Alcotest.(check bool) "route still enabled" true r.enabled
  | Ok None -> Alcotest.fail "github: route missing after idempotent re-apply"
  | Error e -> Alcotest.fail e

(* ── Paired matrix: same reason strings across adapters ──────────── *)

let test_paired_reject_reasons () =
  (* Ensure both adapters surface the canonical Setup_plan_apply reason strings
     for the shared contract cases (not adapter-private renames). *)
  let ra_reasons = ref [] in
  let gh_reasons = ref [] in
  with_ra_db @@ fun db ->
  let plan = ra_plan_and_store ~db ~id:"plan_contract_ra_pair" in
  (match ra_apply ~db ~plan ~digest:(String.make 64 'b') () with
  | Rejected { reason; _ } -> ra_reasons := reason :: !ra_reasons
  | Applied _ -> Alcotest.fail "room_agent pair: expected digest reject");
  (match ra_apply ~db ~plan ~revision:"rev-stale" () with
  | Rejected { reason; _ } -> ra_reasons := reason :: !ra_reasons
  | Applied _ -> Alcotest.fail "room_agent pair: expected stale reject");
  (match ra_apply ~db ~plan ~actor:ra_denied_actor () with
  | Rejected { reason; _ } -> ra_reasons := reason :: !ra_reasons
  | Applied _ -> Alcotest.fail "room_agent pair: expected auth reject");
  with_gh_db @@ fun db ->
  let plan = gh_plan_create ~db ~route_id:"rt_contract_pair" in
  (match gh_apply ~db ~plan ~digest:(String.make 64 'b') () with
  | Rejected { reason; _ } -> gh_reasons := reason :: !gh_reasons
  | Applied _ -> Alcotest.fail "github pair: expected digest reject");
  (match gh_apply ~db ~plan ~revision:"rev-stale" () with
  | Rejected { reason; _ } -> gh_reasons := reason :: !gh_reasons
  | Applied _ -> Alcotest.fail "github pair: expected stale reject");
  (match gh_apply ~db ~plan ~actor:gh_denied_actor () with
  | Rejected { reason; _ } -> gh_reasons := reason :: !gh_reasons
  | Applied _ -> Alcotest.fail "github pair: expected auth reject");
  let sort = List.sort String.compare in
  Alcotest.(check (list string))
    "identical reject reason sets" (sort !ra_reasons) (sort !gh_reasons);
  Alcotest.(check (list string))
    "canonical reasons present"
    (sort [ "digest_mismatch"; "stale_revision"; "authority_denied" ])
    (sort !ra_reasons)

let suite =
  [
    ("room_agent digest mismatch", `Quick, test_digest_mismatch_room_agent);
    ("github_route digest mismatch", `Quick, test_digest_mismatch_github);
    ("room_agent stale base_revision", `Quick, test_stale_revision_room_agent);
    ("github_route stale base_revision", `Quick, test_stale_revision_github);
    ("room_agent authority denied", `Quick, test_authority_denied_room_agent);
    ("github_route authority denied", `Quick, test_authority_denied_github);
    ( "room_agent idempotent re-apply",
      `Quick,
      test_idempotent_reapply_room_agent );
    ("github_route idempotent re-apply", `Quick, test_idempotent_reapply_github);
    ("paired reject reasons identical", `Quick, test_paired_reject_reasons);
  ]
