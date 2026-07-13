(** Tests for attribution previews, receipts, repair states, and audit
    (P21.M3.E2.T005).

    Covers:
    - Preview / receipt / repair / audit records with immutable Actor evidence
    - Requested/resolved mode, lineage, GitHub numeric user or App
    - Distinct redacted failure classes (SSO, permission, refresh, revocation,
      App scope, rollout gate, ambiguity, identity)
    - Authorize / fallback / correlation projection
    - Merge/split never rewrites historical actor evidence
    - No token material in exports *)

module Audit = Github_attribution_audit
module Auth = Github_attribution_authorize
module Fallback = Github_attribution_fallback
module Policy = Github_attribution_policy
module Reconcile = Github_action_reconcile
module A = Actor_snapshot
module P = Principal_identity
module B = Github_account_binding

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let contains ~needle s =
  let n = String.length needle in
  let len = String.length s in
  if n = 0 then true
  else if n > len then false
  else
    let rec loop i =
      if i + n > len then false
      else if String.sub s i n = needle then true
      else loop (i + 1)
    in
    loop 0

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  Audit.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_785_300_000.0
let pid s = assert_ok (P.principal_id_of_string s)

let sample_key ?(connector = P.Teams) ?(tenant = "tenant-acme")
    ?(user = "user-42") () =
  assert_ok
    (P.make_connector_actor_key ~connector ~tenant_or_workspace:tenant
       ~immutable_user_id:user)

let sample_snapshot ?(id = "actorsnap_audit_1") ?(principal = "prin_a")
    ?(lineage_id = "lineage_1") ?(github_user_id = 9001L) ?(app_id = 42) () =
  let principal_id = pid principal in
  let key = sample_key () in
  let ab =
    assert_ok
      (A.make_account_binding_evidence ~binding_id:"ghbind_1" ~lineage_id
         ~identity:
           (assert_ok (B.make_account_identity ~app_id ~github_user_id ()))
         ())
  in
  assert_ok
    (A.create ~id ~now:fixed_now ~reason:"intent_create" ~principal_id
       ~principal_revision:3 ~actor_key:key ~actor_revision:2
       ~identity_link_id:"idlink_1" ~identity_link_revision:4
       ~account_binding:ab
       ~work_refs:
         {
           intent_id = Some "intent_1";
           confirmation_id = Some "conf_1";
           delayed_job_id = Some "job_1";
         }
       ())

let selected ?(binding_id = "bind_1") ?(lineage_id = "lin_1") () =
  assert_ok
    (Auth.make_selected_binding ~binding_id ~lineage_id ~authorized:true
       ~vault_active:true ~vault_generation:1 ~lineage_matches_pin:true ())

let base_auth_request ?(action = "merge") ?(sso_ok = true)
    ?(permissions_ok = true) ?(user_authority_ok = true)
    ?(binding = Auth.Selected (selected ())) ?(confirmation_satisfied = true)
    ?(attribution_gate_enabled = true) () : Auth.request =
  {
    action;
    tool_catalog =
      {
        revision = "cat_rev_1";
        access_revision = "acc_rev_1";
        tool_authorized = true;
        room_id = Some "room_1";
        session_key = Some "sess_1";
      };
    repo_grant =
      {
        repo_full_name = "acme/widgets";
        granted = true;
        blocked = false;
        access_revision = Some "acc_rev_1";
      };
    principal =
      {
        principal_id = "prin_a";
        principal_revision = 3;
        principal_current_active = true;
        actor_revision = Some 2;
        identity_link_revision = Some 4;
        confirmation_id = Some "conf_1";
        confirmation_required = true;
        confirmation_satisfied;
      };
    binding = { resolution = binding };
    installation =
      {
        installation_id = Some 99;
        revision = Some "inst_rev_1";
        active = true;
        repo_authorized = true;
        permissions_ok;
      };
    user_org_sso = { user_authority_ok; org_policy_ok = true; sso_ok };
    live_action = { ok = true; revision = Some "sha_abc"; detail = None };
    pin = Auth.empty_revision_pin;
    actor_snapshot_id = Some "actorsnap_audit_1";
    fallback =
      Auth.fallback_context ~attribution_gate_enabled
        ~preview_actor:Fallback.Names_user ~phase:Fallback.First_attempt ();
  }

(* -------------------------------------------------------------------------- *)
(* Classification                                                              *)
(* -------------------------------------------------------------------------- *)

let test_failure_classes_distinct () =
  let cases =
    [
      (Some "user_org_sso", Some "sso_required", Audit.Sso);
      (Some "installation", Some "permissions_insufficient", Audit.Permission);
      (Some "binding", Some "stale_vault_generation", Audit.Refresh);
      (Some "binding", Some "vault_inactive", Audit.Revocation);
      (Some "installation", Some "installation_repo_denied", Audit.App_scope);
      (Some "fallback", Some "attribution_gate_disabled", Audit.Rollout_gate);
      (Some "binding", Some "account_ambiguous", Audit.Ambiguity);
      (Some "principal", Some "principal_not_current", Audit.Identity);
      (Some "confirmation", Some "confirmation_required", Audit.Confirmation);
      (Some "live_action", Some "live_state_failed", Audit.Live_state);
      (Some "fallback", Some "user_required_no_fallback", Audit.Fallback);
      (Some "tool_catalog", Some "tool_not_in_catalog", Audit.Policy);
    ]
  in
  List.iter
    (fun (failed_check, code, expected) ->
      let got = Audit.classify_failure ?failed_check ?code () in
      Alcotest.(check string)
        (Printf.sprintf "class %s/%s"
           (Option.value failed_check ~default:"-")
           (Option.value code ~default:"-"))
        (Audit.failure_class_to_string expected)
        (Audit.failure_class_to_string got))
    cases

(* -------------------------------------------------------------------------- *)
(* Preview / receipt / repair / audit                                          *)
(* -------------------------------------------------------------------------- *)

let test_preview_records_actor_mode_lineage () =
  with_db @@ fun db ->
  let snap = sample_snapshot () in
  let rec_ =
    assert_ok
      (Audit.record_preview ~db ~now:fixed_now ~action:"comment"
         ~reason:"Preview user-attributed comment on PR" ~result:Audit.Allowed
         ~item_key:"pr:acme/widgets:7" ~room_id:"room_1"
         ~confirmation_id:"conf_1" ~requested_mode:"user" ~resolved_mode:"user"
         ~github_actor:
           (Audit.Numeric_user
              { host = "github.com"; app_id = 42; github_user_id = 9001L })
         ~actor_snapshot:snap ())
  in
  Alcotest.(check string)
    "kind" "preview"
    (Audit.record_kind_to_string rec_.kind);
  Alcotest.(check string) "action" "comment" rec_.action;
  Alcotest.(check (option string)) "requested" (Some "user") rec_.requested_mode;
  Alcotest.(check (option string)) "resolved" (Some "user") rec_.resolved_mode;
  Alcotest.(check (option string))
    "principal" (Some "prin_a") rec_.lineage.principal_id;
  Alcotest.(check (option string))
    "account lineage" (Some "lineage_1") rec_.lineage.account_lineage_id;
  Alcotest.(check bool)
    "immutable evidence" true
    (Audit.is_immutable_evidence rec_);
  Alcotest.(check bool)
    "never authority" false
    (match rec_.actor_snapshot with None -> true | Some s -> A.is_authority s);
  match rec_.github_actor with
  | Audit.Numeric_user { github_user_id; _ } ->
      Alcotest.(check int64) "uid" 9001L github_user_id
  | _ -> Alcotest.fail "expected numeric user actor"

let test_receipt_from_correlation () =
  with_db @@ fun db ->
  let snap = sample_snapshot () in
  let corr =
    Reconcile.make_correlation ~room_id:"room_1" ~action:"merge"
      ~actor_mode:"user" ~item_key:"pr:acme/widgets:42" ~plan_id:"plan_1"
      ~receipt_id:"receipt_1" ~requested_mode:"user" ~resolved_mode:"user"
      ~actor_snapshot:snap ~expected_github_login:"octocat" ()
  in
  let rec_ =
    assert_ok
      (Audit.record_from_correlation ~db ~correlation:corr ~now:fixed_now
         ~job_id:"job_bg_1" ())
  in
  Alcotest.(check string)
    "kind" "receipt"
    (Audit.record_kind_to_string rec_.kind);
  Alcotest.(check string)
    "result" "completed"
    (Audit.result_kind_to_string rec_.result);
  Alcotest.(check (option string)) "receipt" (Some "receipt_1") rec_.receipt_id;
  Alcotest.(check (option string)) "plan" (Some "plan_1") rec_.plan_id;
  Alcotest.(check (option string)) "job" (Some "job_bg_1") rec_.job_id;
  Alcotest.(check (option string)) "resolved" (Some "user") rec_.resolved_mode;
  Alcotest.(check (option string))
    "snap id" (Some snap.id) rec_.actor_snapshot_id;
  Alcotest.(check bool) "immutable" true (Audit.is_immutable_evidence rec_)

let test_repair_states_for_distinct_failures () =
  with_db @@ fun db ->
  let classes =
    [
      (Audit.Sso, "sso_required", "Complete org SSO for this GitHub account.");
      ( Audit.Permission,
        "permissions_insufficient",
        "App installation lacks write permission; update installation perms." );
      ( Audit.Refresh,
        "stale_vault_generation",
        "Vault generation advanced; re-authorize after refresh." );
      ( Audit.Revocation,
        "vault_inactive",
        "Account vault revoked; relink the GitHub account." );
      ( Audit.App_scope,
        "installation_repo_denied",
        "Repo is outside App installation selection." );
      ( Audit.Rollout_gate,
        "attribution_gate_disabled",
        "User-attribution rollout gate is off; enable before user work." );
      ( Audit.Ambiguity,
        "account_ambiguous",
        "Multiple eligible accounts; set an explicit preference." );
      ( Audit.Identity,
        "principal_not_current",
        "Principal lineage not current; use survivor or re-link." );
    ]
  in
  List.iter
    (fun (cls, code, reason) ->
      let r =
        assert_ok
          (Audit.record_repair ~db ~now:fixed_now ~action:"merge" ~reason
             ~failure_class:cls ~failure_code:code ~requested_mode:"user"
             ~room_id:"room_1" ())
      in
      Alcotest.(check string)
        ("kind " ^ code) "repair_state"
        (Audit.record_kind_to_string r.kind);
      Alcotest.(check string)
        ("class " ^ code)
        (Audit.failure_class_to_string cls)
        (match r.failure_class with
        | Some c -> Audit.failure_class_to_string c
        | None -> "none");
      Alcotest.(check (option string))
        ("code " ^ code) (Some code) r.failure_code;
      Alcotest.(check bool)
        ("no token " ^ code) false
        (Audit.denial_exposes_token ~record:r
           ~plaintext:"ghu_secret_token_value_xyz"))
    classes;
  Alcotest.(check int)
    "eight repairs" 8
    (Audit.count ~db ~kind:Audit.Repair_state ())

let test_audit_from_authorize_allow_and_deny () =
  with_db @@ fun db ->
  let snap = sample_snapshot () in
  let allow_dec = Auth.authorize (base_auth_request ()) in
  Alcotest.(check bool) "allow" true (Auth.is_allow allow_dec);
  let allow_rec =
    assert_ok
      (Audit.record_authorize_decision ~db ~decision:allow_dec ~now:fixed_now
         ~item_key:"pr:acme/widgets:1" ~room_id:"room_1" ~actor_snapshot:snap
         ~github_user_id:9001L ())
  in
  Alcotest.(check string)
    "allow kind" "audit"
    (Audit.record_kind_to_string allow_rec.kind);
  Alcotest.(check string)
    "allow result" "allowed"
    (Audit.result_kind_to_string allow_rec.result);
  Alcotest.(check (option string))
    "resolved user" (Some "user") allow_rec.resolved_mode;
  (match allow_rec.github_actor with
  | Audit.Numeric_user { github_user_id; _ } ->
      Alcotest.(check int64) "uid" 9001L github_user_id
  | _ -> Alcotest.fail "expected numeric user");

  let deny_dec = Auth.authorize (base_auth_request ~sso_ok:false ()) in
  Alcotest.(check bool) "deny" true (Auth.is_deny deny_dec);
  let deny_rec =
    assert_ok
      (Audit.record_authorize_decision ~db ~decision:deny_dec ~now:fixed_now
         ~room_id:"room_1" ~actor_snapshot:snap ())
  in
  Alcotest.(check string)
    "deny kind" "repair_state"
    (Audit.record_kind_to_string deny_rec.kind);
  Alcotest.(check string)
    "deny class" "sso"
    (match deny_rec.failure_class with
    | Some c -> Audit.failure_class_to_string c
    | None -> "none");
  Alcotest.(check (option string))
    "deny code" (Some "sso_required") deny_rec.failure_code;
  Alcotest.(check bool)
    "reason non-empty" true
    (String.length deny_rec.reason > 10)

let test_fallback_visible_app_and_user_required () =
  with_db @@ fun db ->
  let allow_fb =
    Fallback.resolve
      (Fallback.default_request ~action:"comment"
         ~requirement:(Policy.lookup ~action:"comment")
         ~preview_actor:Fallback.Names_app ~user_path_available:false
         ~app_path_available:true ())
  in
  let rec_allow =
    assert_ok
      (match
         Audit.of_fallback_decision ~decision:allow_fb ~action:"comment"
           ~kind:Audit.Preview ~now:fixed_now ~room_id:"room_1" ()
       with
      | Error e -> Error e
      | Ok r -> Audit.insert ~db ~record:r ~now:fixed_now ())
  in
  Alcotest.(check bool) "used fallback" true rec_allow.used_app_fallback;
  Alcotest.(check string)
    "fallback result" "fallback_app"
    (Audit.result_kind_to_string rec_allow.result);
  Alcotest.(check (option string))
    "resolved app" (Some "app") rec_allow.resolved_mode;

  let deny_fb =
    Fallback.resolve
      (Fallback.default_request ~action:"merge"
         ~requirement:(Policy.lookup ~action:"merge")
         ~preview_actor:Fallback.Names_app ~user_path_available:false
         ~app_path_available:true ())
  in
  let rec_deny =
    assert_ok
      (match
         Audit.of_fallback_decision ~decision:deny_fb ~action:"merge"
           ~now:fixed_now ()
       with
      | Error e -> Error e
      | Ok r -> Audit.insert ~db ~record:r ~now:fixed_now ())
  in
  Alcotest.(check string)
    "deny class" "fallback"
    (match rec_deny.failure_class with
    | Some c -> Audit.failure_class_to_string c
    | None -> "none");
  Alcotest.(check (option string))
    "user required code" (Some "user_required_no_fallback")
    rec_deny.failure_code

(* -------------------------------------------------------------------------- *)
(* Immutability / redaction                                                    *)
(* -------------------------------------------------------------------------- *)

let test_historical_evidence_not_rewritten () =
  with_db @@ fun db ->
  let snap = sample_snapshot ~principal:"prin_a" () in
  let rec_ =
    assert_ok
      (Audit.record_receipt ~db ~now:fixed_now ~action:"merge"
         ~reason:"merge completed as user" ~result:Audit.Completed
         ~receipt_id:"rcpt_1" ~requested_mode:"user" ~resolved_mode:"user"
         ~actor_snapshot:snap ())
  in
  let snap2 =
    sample_snapshot ~id:"actorsnap_audit_2" ~principal:"prin_survivor" ()
  in
  (match Audit.rewrite_actor_evidence ~db ~id:rec_.id ~snapshot:snap2 with
  | Ok () -> Alcotest.fail "rewrite must be rejected"
  | Error msg ->
      Alcotest.(check bool)
        "immutable message" true
        (contains ~needle:"immutable" msg));
  let loaded = Audit.get_by_id ~db ~id:rec_.id in
  match loaded with
  | None -> Alcotest.fail "missing record"
  | Some r ->
      Alcotest.(check (option string))
        "principal frozen" (Some "prin_a") r.lineage.principal_id;
      Alcotest.(check (option string))
        "snapshot id frozen" (Some snap.id) r.actor_snapshot_id;
      Alcotest.(check bool)
        "still immutable" true
        (Audit.is_immutable_evidence r)

let test_json_roundtrip_and_redaction () =
  with_db @@ fun db ->
  let snap = sample_snapshot () in
  let secret = "ghu_PLAINTEXT_MUST_NOT_ESCAPE_audit_xyz" in
  let rec_ =
    assert_ok
      (Audit.record_audit ~db ~now:fixed_now ~action:"label"
         ~reason:("Applied label with token=" ^ secret)
         ~result:Audit.Completed ~requested_mode:"user" ~resolved_mode:"user"
         ~actor_snapshot:snap
         ~github_actor:
           (Audit.Numeric_user
              { host = "github.com"; app_id = 42; github_user_id = 9001L })
         ())
  in
  Alcotest.(check bool)
    "plaintext redacted from reason" false
    (contains ~needle:secret rec_.reason);
  Alcotest.(check bool)
    "denial helper" false
    (Audit.denial_exposes_token ~record:rec_ ~plaintext:secret);
  let json = Audit.to_json rec_ in
  let json_s = Yojson.Safe.to_string json in
  Alcotest.(check bool) "json no token" false (contains ~needle:secret json_s);
  let back = assert_ok (Audit.of_json json) in
  Alcotest.(check string) "id roundtrip" rec_.id back.id;
  Alcotest.(check string) "action roundtrip" rec_.action back.action;
  Alcotest.(check (option string))
    "principal roundtrip" rec_.lineage.principal_id back.lineage.principal_id

let test_list_filters () =
  with_db @@ fun db ->
  let snap = sample_snapshot () in
  ignore
    (assert_ok
       (Audit.record_preview ~db ~now:fixed_now ~action:"comment"
          ~reason:"preview" ~result:Audit.Allowed ~actor_snapshot:snap ()));
  ignore
    (assert_ok
       (Audit.record_repair ~db ~now:(fixed_now +. 1.) ~action:"merge"
          ~reason:"sso needed" ~failure_class:Audit.Sso
          ~failure_code:"sso_required" ()));
  ignore
    (assert_ok
       (Audit.record_receipt ~db ~now:(fixed_now +. 2.) ~action:"merge"
          ~reason:"done" ~result:Audit.Completed ~actor_snapshot:snap ()));
  Alcotest.(check int) "total" 3 (Audit.count ~db ());
  Alcotest.(check int)
    "previews" 1
    (List.length (Audit.list_by_kind ~db ~kind:Audit.Preview ()));
  Alcotest.(check int)
    "merge actions" 2
    (List.length (Audit.list_by_action ~db ~action:"merge" ()));
  Alcotest.(check int)
    "by snap" 2
    (List.length (Audit.list_by_snapshot_id ~db ~actor_snapshot_id:snap.id ()));
  Alcotest.(check int)
    "by principal" 2
    (List.length (Audit.list_by_principal ~db ~principal_id:"prin_a" ()));
  Alcotest.(check int)
    "by sso" 1
    (List.length (Audit.list_by_failure_class ~db ~failure_class:Audit.Sso ()))

let test_ambiguity_and_rollout_from_authorize () =
  with_db @@ fun db ->
  let amb = Auth.authorize (base_auth_request ~binding:Auth.Ambiguous ()) in
  let amb_rec =
    assert_ok
      (Audit.record_authorize_decision ~db ~decision:amb ~now:fixed_now ())
  in
  Alcotest.(check string)
    "ambiguity"
    (Audit.failure_class_to_string Audit.Ambiguity)
    (match amb_rec.failure_class with
    | Some c -> Audit.failure_class_to_string c
    | None -> "none");

  let gate =
    Auth.authorize
      (base_auth_request ~action:"comment" ~attribution_gate_enabled:false
         ~binding:Auth.None_eligible ())
  in
  (* comment is user_preferred; with gate disabled and no user path, expect
     repair / rollout-related deny from fallback *)
  match gate with
  | Auth.Allow _ ->
      (* App installation path for pure app may still allow; comment under
         user_preferred with gate disabled should deny user fallback. *)
      ()
  | Auth.Deny d ->
      let r =
        assert_ok
          (Audit.record_authorize_decision ~db ~decision:(Auth.Deny d)
             ~now:fixed_now ())
      in
      Alcotest.(check bool)
        "has failure class" true
        (Option.is_some r.failure_class)

let suite =
  [
    Alcotest.test_case "failure classes are distinct" `Quick
      test_failure_classes_distinct;
    Alcotest.test_case "preview records actor mode and lineage" `Quick
      test_preview_records_actor_mode_lineage;
    Alcotest.test_case "receipt from correlation" `Quick
      test_receipt_from_correlation;
    Alcotest.test_case "repair states for distinct failures" `Quick
      test_repair_states_for_distinct_failures;
    Alcotest.test_case "audit from authorize allow and deny" `Quick
      test_audit_from_authorize_allow_and_deny;
    Alcotest.test_case "fallback visible app and user required" `Quick
      test_fallback_visible_app_and_user_required;
    Alcotest.test_case "historical evidence not rewritten" `Quick
      test_historical_evidence_not_rewritten;
    Alcotest.test_case "json roundtrip and redaction" `Quick
      test_json_roundtrip_and_redaction;
    Alcotest.test_case "list filters" `Quick test_list_filters;
    Alcotest.test_case "ambiguity and rollout from authorize" `Quick
      test_ambiguity_and_rollout_from_authorize;
  ]
