(** Tests for admin GitHub user-auth enablement readiness and repair
    (P21.M4.E1.T002).

    Validates:
    - Combined readiness gates App/master-key/callback/device/expiry/delivery/
      permissions/webhooks and rollout flags
    - Plan-confirm-apply enable/disable production gate
    - Users authorize only themselves (admin-for-other refused)
    - Room-scoped enablement requires Room consent
    - Diagnostics/plans never embed secret material
    - Minimal-build disabled guidance *)

module E = Github_user_auth_enablement
module Auth = Github_user_auth_readiness
module Rollout = Github_attribution_rollout
module Cli = Github_user_auth_enablement_cli
module MinCli = Github_user_auth_enablement_cli_min

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  E.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_785_500_000.0

let ready_user_auth ?(device_flow_requested = false)
    ?(device_flow_enabled = false) () : Auth.config_snapshot =
  {
    host = "github.com";
    app_id = Some 42;
    client_id_handle = Some "h:client-id";
    client_secret_handle = Some "h:client-secret-VALUE-do-not-leak";
    callback_uri = Some "https://clawq.example/github/oauth/callback";
    expiring_user_tokens = true;
    device_flow_requested;
    device_flow_enabled;
    master_key_present = true;
    permissions = [ ("pull_requests", "write"); ("issues", "write") ];
    private_continuation_ready = true;
  }

let full_evidence ?(user_auth = ready_user_auth ())
    ?(webhook_secret_handle = Some "h:webhook-secret-SECRET")
    ?(webhook_endpoint_ready = true) ?(revocation_webhook_ready = true)
    ?(principal_ready = true) ?(vault_ready = true) ?(policy_ready = true)
    ?(private_delivery_ready = true) ?(repair_ready = true)
    ?(backout_ready = true) ?(account_admin_surface_ready = true)
    ?(stage = Rollout.Safe_default)
    ?(production = Rollout.default_production_gate) ?(room_scoped = false)
    ?(room_consent_present = false) ?(now = fixed_now) () : E.evidence =
  E.evidence_with_user_auth user_auth ~webhook_secret_handle
    ~webhook_endpoint_ready ~revocation_webhook_ready ~principal_ready
    ~vault_ready ~policy_ready ~private_delivery_ready ~repair_ready
    ~backout_ready ~account_admin_surface_ready ~stage ~production
    ~pilot_gates:(Rollout.default_pilot_gates ())
    ~now ~room_scoped ~room_consent_present ()

let level_of name (r : E.readiness_report) =
  match List.find_opt (fun (c : E.check) -> c.name = name) r.checks with
  | Some c -> E.string_of_level c.level
  | None -> "missing"

(* -------------------------------------------------------------------------- *)
(* Readiness                                                                  *)
(* -------------------------------------------------------------------------- *)

let test_all_ready_enables () =
  let r = E.assess (full_evidence ()) in
  Alcotest.(check bool) "can_enable" true r.can_enable_production;
  Alcotest.(check string) "overall" "pass" (E.string_of_level r.overall);
  Alcotest.(check bool) "can_act_as_user" true r.user_auth.can_act_as_user;
  Alcotest.(check bool) "cannot disable when off" false r.can_disable_production

let test_missing_master_key_blocks () =
  let ua = ready_user_auth () in
  let ua = { ua with master_key_present = false } in
  let r = E.assess (full_evidence ~user_auth:ua ()) in
  Alcotest.(check bool) "blocked" false r.can_enable_production;
  Alcotest.(check string)
    "auth_master_key fail" "fail"
    (level_of "auth_master_key" r);
  Alcotest.(check bool)
    "repair mentions master key" true
    (List.exists
       (fun s -> String_util.contains s "master key")
       (E.repair_guidance r))

let test_missing_webhook_blocks () =
  let r =
    E.assess
      (full_evidence ~webhook_secret_handle:None ~webhook_endpoint_ready:false
         ~revocation_webhook_ready:false ())
  in
  Alcotest.(check bool) "blocked" false r.can_enable_production;
  Alcotest.(check string) "webhook_secret" "fail" (level_of "webhook_secret" r);
  Alcotest.(check string)
    "revocation_webhook" "fail"
    (level_of "revocation_webhook" r)

let test_incomplete_rollout_flags_block () =
  let r = E.assess (full_evidence ~vault_ready:false ~backout_ready:false ()) in
  Alcotest.(check bool) "blocked" false r.can_enable_production;
  Alcotest.(check string) "vault_ready" "fail" (level_of "vault_ready" r);
  Alcotest.(check string) "backout_ready" "fail" (level_of "backout_ready" r)

let test_rollback_stage_blocks_enable () =
  let r = E.assess (full_evidence ~stage:Rollout.Rollback ()) in
  Alcotest.(check bool) "blocked" false r.can_enable_production;
  Alcotest.(check string) "stage" "fail" (level_of "stage_allows_enable" r)

let test_room_consent_required () =
  let r =
    E.assess (full_evidence ~room_scoped:true ~room_consent_present:false ())
  in
  Alcotest.(check bool) "blocked without consent" false r.can_enable_production;
  Alcotest.(check string) "room_consent fail" "fail" (level_of "room_consent" r);
  let r_ok =
    E.assess (full_evidence ~room_scoped:true ~room_consent_present:true ())
  in
  Alcotest.(check bool) "ok with consent" true r_ok.can_enable_production

let test_constraints_always_present () =
  let r = E.assess (full_evidence ()) in
  Alcotest.(check bool)
    "self-authorize constraint" true
    (List.exists
       (fun s -> String_util.contains s "authorize only themselves")
       r.constraints);
  Alcotest.(check bool)
    "room consent constraint" true
    (List.exists (fun s -> String_util.contains s "Room consent") r.constraints);
  Alcotest.(check bool)
    "no admin-for-user oauth" true
    (List.exists
       (fun s -> String_util.contains s "never start OAuth")
       r.constraints)

let test_refuse_authorize_for_other () =
  match
    E.refuse_authorize_for_other ~admin_principal_id:"admin-1"
      ~subject_principal_id:"user-2"
  with
  | Ok () -> Alcotest.fail "expected refuse"
  | Error e ->
      Alcotest.(check bool)
        "mentions only themselves" true
        (String_util.contains e "only themselves");
      Alcotest.(check bool)
        "mentions subject" true
        (String_util.contains e "user-2");
      ()

let test_refuse_same_principal_ok () =
  match
    E.refuse_authorize_for_other ~admin_principal_id:"p1"
      ~subject_principal_id:"p1"
  with
  | Ok () -> ()
  | Error e -> Alcotest.fail e

let test_redaction_no_secret_handles () =
  let secret = "h:client-secret-VALUE-do-not-leak" in
  let r = E.assess (full_evidence ()) in
  let out = E.format_readiness r in
  Alcotest.(check bool)
    "format hides secret handle" false
    (String_util.contains out secret);
  let json = E.readiness_to_json r in
  Alcotest.(check bool)
    "json hides secret" false
    (E.json_contains_plaintext ~json ~plaintext:secret);
  let repair = E.format_repair r in
  Alcotest.(check bool)
    "repair hides secret" false
    (String_util.contains repair secret)

(* -------------------------------------------------------------------------- *)
(* Plan-confirm-apply                                                         *)
(* -------------------------------------------------------------------------- *)

let test_plan_enable_and_apply () =
  with_db @@ fun db ->
  let evidence = full_evidence () in
  match
    E.plan_enable ~db ~admin_principal_id:"admin-p1"
      ~reason:"lab enable after readiness" ~audit_ref:"audit-enable-1" ~evidence
      ~now:fixed_now ()
  with
  | Error e -> Alcotest.fail e
  | Ok plan -> (
      Alcotest.(check bool) "can_apply" true plan.can_apply;
      Alcotest.(check string)
        "kind" "enable_production"
        (E.string_of_enablement_kind plan.kind);
      Alcotest.(check bool)
        "digest non-empty" true
        (String.trim plan.digest <> "");
      (* Wrong digest refused. *)
      (match
         E.apply_plan ~db ~plan_id:plan.plan_id ~presented_digest:"deadbeef"
           ~evidence ~now:fixed_now ()
       with
      | E.Digest_mismatch _ -> ()
      | other ->
          Alcotest.fail
            (match other with
            | E.Applied _ -> "unexpected Applied"
            | _ -> "expected Digest_mismatch"));
      match
        E.apply_plan ~db ~plan_id:plan.plan_id ~presented_digest:plan.digest
          ~evidence ~now:fixed_now ()
      with
      | E.Applied { gate; message; _ } ->
          Alcotest.(check bool) "production on" true gate.production.enabled;
          Alcotest.(check string)
            "stage production"
            (Rollout.stage_to_string Rollout.P21_production)
            (Rollout.stage_to_string gate.stage);
          Alcotest.(check int) "revision advanced" 1 gate.revision;
          Alcotest.(check bool)
            "message mentions readiness" true
            (String_util.contains message "readiness"
            || String_util.contains message "production");
          (* Second apply of same plan refused. *)
          (match
             E.apply_plan ~db ~plan_id:plan.plan_id
               ~presented_digest:plan.digest ~evidence ~now:fixed_now ()
           with
          | E.Refused _ -> ()
          | _ -> Alcotest.fail "expected refuse on re-apply");
          (* Gate load reflects enable. *)
          let loaded = E.load_gate ~db () in
          Alcotest.(check bool) "loaded enabled" true loaded.production.enabled
      | E.Refused { reason; _ } -> Alcotest.fail ("refused: " ^ reason)
      | E.Stale_revision s -> Alcotest.fail ("stale: " ^ s)
      | E.Digest_mismatch s -> Alcotest.fail ("digest: " ^ s)
      | E.Expired s -> Alcotest.fail ("expired: " ^ s)
      | E.Not_found s -> Alcotest.fail ("not found: " ^ s))

let test_plan_enable_blocked_when_not_ready () =
  with_db @@ fun db ->
  let evidence = full_evidence ~vault_ready:false () in
  match
    E.plan_enable ~db ~admin_principal_id:"admin-p1" ~reason:"premature"
      ~audit_ref:"audit-x" ~evidence ~now:fixed_now ()
  with
  | Error e -> Alcotest.fail e
  | Ok plan -> (
      Alcotest.(check bool) "not applyable" false plan.can_apply;
      Alcotest.(check bool)
        "has readiness conflict" true
        (List.exists
           (fun (c : E.conflict) -> c.code = "readiness_incomplete")
           plan.hard_conflicts);
      match
        E.apply_plan ~db ~plan_id:plan.plan_id ~presented_digest:plan.digest
          ~evidence ~now:fixed_now ()
      with
      | E.Refused _ -> ()
      | _ -> Alcotest.fail "expected refuse")

let test_plan_disable_after_enable () =
  with_db @@ fun db ->
  let evidence = full_evidence () in
  let enable_plan =
    match
      E.plan_enable ~db ~admin_principal_id:"admin-p1" ~reason:"on"
        ~audit_ref:"a1" ~evidence ~now:fixed_now ()
    with
    | Ok p -> p
    | Error e -> Alcotest.fail e
  in
  (match
     E.apply_plan ~db ~plan_id:enable_plan.plan_id
       ~presented_digest:enable_plan.digest ~evidence ~now:fixed_now ()
   with
  | E.Applied _ -> ()
  | E.Refused { reason; _ } -> Alcotest.fail reason
  | _ -> Alcotest.fail "enable apply failed");
  let gate = E.load_gate ~db () in
  let evidence =
    E.evidence_from_gate ~gate ~user_auth:(ready_user_auth ())
      ~webhook_secret_handle:(Some "h:wh") ~webhook_endpoint_ready:true
      ~revocation_webhook_ready:true ~principal_ready:true ~vault_ready:true
      ~policy_ready:true ~private_delivery_ready:true ~repair_ready:true
      ~backout_ready:true ~account_admin_surface_ready:true ~now:fixed_now ()
  in
  let disable_plan =
    match
      E.plan_disable ~db ~admin_principal_id:"admin-p1" ~reason:"rollback lab"
        ~audit_ref:"a-disable" ~evidence ~now:(fixed_now +. 10.) ()
    with
    | Ok p -> p
    | Error e -> Alcotest.fail e
  in
  Alcotest.(check bool) "disable can_apply" true disable_plan.can_apply;
  match
    E.apply_plan ~db ~plan_id:disable_plan.plan_id
      ~presented_digest:disable_plan.digest ~evidence ~now:(fixed_now +. 10.) ()
  with
  | E.Applied { gate; _ } ->
      Alcotest.(check bool) "production off" false gate.production.enabled;
      Alcotest.(check string)
        "safe default"
        (Rollout.stage_to_string Rollout.Safe_default)
        (Rollout.stage_to_string gate.stage)
  | E.Refused { reason; _ } -> Alcotest.fail reason
  | _ -> Alcotest.fail "disable apply failed"

let test_plan_json_redacted () =
  with_db @@ fun db ->
  let secret = "h:client-secret-VALUE-do-not-leak" in
  let evidence = full_evidence () in
  match
    E.plan_enable ~db ~admin_principal_id:"admin-p1" ~reason:"redact check"
      ~audit_ref:"audit-r" ~evidence ~now:fixed_now ()
  with
  | Error e -> Alcotest.fail e
  | Ok plan ->
      let json = E.plan_to_json plan in
      Alcotest.(check bool)
        "plan json no secret" false
        (E.json_contains_plaintext ~json ~plaintext:secret);
      let formatted = E.format_plan plan in
      Alcotest.(check bool)
        "plan text no secret" false
        (String_util.contains formatted secret)

let test_default_gate_safe () =
  with_db @@ fun db ->
  let g = E.load_gate ~db () in
  Alcotest.(check bool) "default off" false g.production.enabled;
  Alcotest.(check string)
    "safe default stage"
    (Rollout.stage_to_string Rollout.Safe_default)
    (Rollout.stage_to_string g.stage);
  Alcotest.(check int) "revision 0" 0 g.revision

let test_min_cli_disabled () =
  let out = MinCli.cmd [ "user-auth"; "status" ] in
  Alcotest.(check bool)
    "mentions minimal" true
    (String_util.contains out "minimal");
  Alcotest.(check bool)
    "mentions user-auth" true
    (String_util.contains out "user-auth")

let test_cli_readiness_with_db () =
  with_db @@ fun db ->
  (* Without READY env, readiness should fail closed. *)
  let out = Cli.cmd_with_db ~db [ "readiness" ] in
  Alcotest.(check bool)
    "reports readiness" true
    (String_util.contains out "enablement readiness"
    || String_util.contains out "can_enable_production");
  Alcotest.(check bool)
    "includes constraints" true
    (String_util.contains out "Constraints"
    || String_util.contains out "authorize only themselves")

let test_cli_enable_requires_admin () =
  with_db @@ fun db ->
  let prev_admin = Sys.getenv_opt Cli.admin_env_var in
  let prev_pid = Sys.getenv_opt Cli.principal_env_var in
  Unix.putenv Cli.admin_env_var "";
  Unix.putenv Cli.principal_env_var "admin-1";
  Fun.protect
    ~finally:(fun () ->
      (match prev_admin with
      | Some v -> Unix.putenv Cli.admin_env_var v
      | None -> Unix.putenv Cli.admin_env_var "");
      match prev_pid with
      | Some v -> Unix.putenv Cli.principal_env_var v
      | None -> Unix.putenv Cli.principal_env_var "")
    (fun () ->
      let out =
        Cli.cmd_with_db ~db [ "enable"; "--reason"; "x"; "--audit-ref"; "y" ]
      in
      Alcotest.(check bool)
        "requires admin" true
        (String_util.contains out "CLAWQ_ADMIN"
        || String_util.contains out "admin privileges"))

let suite =
  [
    Alcotest.test_case "all readiness pass enables production eligibility"
      `Quick test_all_ready_enables;
    Alcotest.test_case "missing master key blocks enable and offers repair"
      `Quick test_missing_master_key_blocks;
    Alcotest.test_case "missing webhook config blocks enable" `Quick
      test_missing_webhook_blocks;
    Alcotest.test_case "incomplete rollout flags block enable" `Quick
      test_incomplete_rollout_flags_block;
    Alcotest.test_case "rollback stage blocks enable" `Quick
      test_rollback_stage_blocks_enable;
    Alcotest.test_case "Room-scoped enablement requires Room consent" `Quick
      test_room_consent_required;
    Alcotest.test_case "capability constraints always present" `Quick
      test_constraints_always_present;
    Alcotest.test_case "admin cannot authorize for another Principal" `Quick
      test_refuse_authorize_for_other;
    Alcotest.test_case "self-authorize same principal allowed as identity check"
      `Quick test_refuse_same_principal_ok;
    Alcotest.test_case "readiness format and json redact secret handles" `Quick
      test_redaction_no_secret_handles;
    Alcotest.test_case "default gate is safe disabled" `Quick
      test_default_gate_safe;
    Alcotest.test_case "plan-confirm-apply enables production gate" `Quick
      test_plan_enable_and_apply;
    Alcotest.test_case "plan enable blocked when not ready" `Quick
      test_plan_enable_blocked_when_not_ready;
    Alcotest.test_case "plan-confirm-apply disables production gate" `Quick
      test_plan_disable_after_enable;
    Alcotest.test_case "plan json and text redact secrets" `Quick
      test_plan_json_redacted;
    Alcotest.test_case "minimal CLI returns disabled guidance" `Quick
      test_min_cli_disabled;
    Alcotest.test_case "CLI readiness reports redacted summary" `Quick
      test_cli_readiness_with_db;
    Alcotest.test_case "CLI enable requires CLAWQ_ADMIN" `Quick
      test_cli_enable_requires_admin;
  ]
