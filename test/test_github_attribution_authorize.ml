(** Tests for pure attribution authorization after all policy checks
    (P21.M3.E2.T003). *)

module A = Github_attribution_authorize
module Policy = Github_attribution_policy

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

let selected ?(binding_id = "bind_1") ?(lineage_id = "lin_1")
    ?(authorized = true) ?(vault_active = true) ?(vault_generation = 1)
    ?(lineage_matches_pin = true) () =
  assert_ok
    (A.make_selected_binding ~binding_id ~lineage_id ~authorized ~vault_active
       ~vault_generation ~lineage_matches_pin ())

let base_request ?(action = "merge") ?(tool_authorized = true)
    ?(repo_granted = true) ?(repo_blocked = false) ?(principal_current = true)
    ?(confirmation_required = true) ?(confirmation_satisfied = true)
    ?(confirmation_id = Some "conf_1") ?(binding = A.Selected (selected ()))
    ?(installation_active = true) ?(installation_repo_ok = true)
    ?(permissions_ok = true) ?(user_authority_ok = true) ?(org_policy_ok = true)
    ?(sso_ok = true) ?(live_ok = true) ?(live_detail = None)
    ?(live_revision = Some "sha_abc") ?(pin = A.empty_revision_pin)
    ?(actor_snapshot_id = Some "snap_1") ?(catalog_revision = "cat_rev_1")
    ?(access_revision = "acc_rev_1") ?(principal_revision = 3)
    ?(installation_revision = Some "inst_rev_1") () : A.request =
  {
    action;
    tool_catalog =
      {
        revision = catalog_revision;
        access_revision;
        tool_authorized;
        room_id = Some "room_1";
        session_key = Some "sess_1";
      };
    repo_grant =
      {
        repo_full_name = "acme/widgets";
        granted = repo_granted;
        blocked = repo_blocked;
        access_revision = Some access_revision;
      };
    principal =
      {
        principal_id = "prin_a";
        principal_revision;
        principal_current_active = principal_current;
        actor_revision = Some 2;
        identity_link_revision = Some 4;
        confirmation_id;
        confirmation_required;
        confirmation_satisfied;
      };
    binding = { resolution = binding };
    installation =
      {
        installation_id = Some 99;
        revision = installation_revision;
        active = installation_active;
        repo_authorized = installation_repo_ok;
        permissions_ok;
      };
    user_org_sso = { user_authority_ok; org_policy_ok; sso_ok };
    live_action =
      { ok = live_ok; revision = live_revision; detail = live_detail };
    pin;
    actor_snapshot_id;
  }

let expect_allow ?(mode = A.User) decision =
  match decision with
  | A.Allow a ->
      Alcotest.(check string)
        "mode"
        (A.resolved_mode_to_string mode)
        (A.resolved_mode_to_string a.mode);
      a
  | A.Deny d ->
      Alcotest.fail
        (Printf.sprintf "expected Allow, got Deny check=%s code=%s"
           d.failed_check d.repair.code)

let expect_deny ~check ~code decision =
  match decision with
  | A.Deny d ->
      Alcotest.(check string) "failed_check" check d.failed_check;
      Alcotest.(check string) "repair.code" code d.repair.code;
      Alcotest.(check bool) "is_deny" true (A.is_deny decision);
      d
  | A.Allow _ -> Alcotest.fail "expected Deny, got Allow"

(* -------------------------------------------------------------------------- *)
(* Happy paths                                                                 *)
(* -------------------------------------------------------------------------- *)

let test_allow_user_required_merge () =
  let d = A.authorize (base_request ~action:"merge" ()) in
  let a = expect_allow ~mode:A.User d in
  Alcotest.(check string) "action" "merge" a.requirement.action;
  Alcotest.(check string)
    "attribution" "user_required"
    (Policy.attribution_to_string a.requirement.attribution);
  Alcotest.(check (option string)) "binding" (Some "bind_1") a.binding_id;
  Alcotest.(check (option string)) "principal" (Some "prin_a") a.principal_id;
  Alcotest.(check (option string))
    "catalog rev" (Some "cat_rev_1") a.revisions.tool_catalog_revision;
  Alcotest.(check (option int))
    "vault gen" (Some 1) a.revisions.vault_generation;
  (* No token/lease fields in JSON contract. *)
  let json = A.decision_to_json d in
  match json with
  | `Assoc fields ->
      Alcotest.(check bool)
        "issues_token false" true
        (List.assoc "issues_token" fields = `Bool false);
      Alcotest.(check bool)
        "issues_lease false" true
        (List.assoc "issues_lease" fields = `Bool false)
  | _ -> Alcotest.fail "decision json must be object"

let test_allow_app_installation_comment () =
  let d =
    A.authorize
      (base_request ~action:"comment" ~confirmation_required:false
         ~confirmation_satisfied:true ~confirmation_id:None
         ~binding:A.Not_required ~user_authority_ok:true ())
  in
  let a = expect_allow ~mode:A.App d in
  Alcotest.(check string) "action" "comment" a.requirement.action;
  Alcotest.(check (option string)) "no binding" None a.binding_id

let test_allow_with_matching_pins () =
  let pin : A.revision_pin =
    {
      tool_catalog_revision = Some "cat_rev_1";
      access_revision = Some "acc_rev_1";
      principal_revision = Some 3;
      binding_lineage_id = Some "lin_1";
      vault_generation = Some 1;
      installation_revision = Some "inst_rev_1";
      confirmation_id = Some "conf_1";
      actor_snapshot_id = Some "snap_1";
      live_state_revision = Some "sha_abc";
    }
  in
  let d = A.authorize (base_request ~pin ()) in
  ignore (expect_allow d)

(* -------------------------------------------------------------------------- *)
(* Tool catalog / repo grant                                                   *)
(* -------------------------------------------------------------------------- *)

let test_deny_tool_not_in_catalog () =
  let d = A.authorize (base_request ~tool_authorized:false ()) in
  ignore (expect_deny ~check:"tool_catalog" ~code:"tool_not_in_catalog" d)

let test_deny_stale_tool_catalog_revision () =
  let pin =
    { A.empty_revision_pin with tool_catalog_revision = Some "old_cat" }
  in
  let d = A.authorize (base_request ~pin ()) in
  ignore
    (expect_deny ~check:"tool_catalog" ~code:"stale_tool_catalog_revision" d)

let test_deny_repo_not_granted () =
  let d = A.authorize (base_request ~repo_granted:false ()) in
  ignore (expect_deny ~check:"repo_grant" ~code:"repo_not_granted" d)

let test_deny_repo_blocked_wins () =
  let d = A.authorize (base_request ~repo_granted:true ~repo_blocked:true ()) in
  ignore (expect_deny ~check:"repo_grant" ~code:"repo_blocked" d)

let test_deny_stale_access_revision () =
  let pin = { A.empty_revision_pin with access_revision = Some "old_acc" } in
  let d = A.authorize (base_request ~pin ()) in
  ignore (expect_deny ~check:"repo_grant" ~code:"stale_access_revision" d)

(* -------------------------------------------------------------------------- *)
(* Principal / confirmation                                                    *)
(* -------------------------------------------------------------------------- *)

let test_deny_principal_not_current () =
  let d = A.authorize (base_request ~principal_current:false ()) in
  ignore (expect_deny ~check:"principal" ~code:"principal_not_current" d)

let test_deny_stale_principal_revision () =
  let pin = { A.empty_revision_pin with principal_revision = Some 1 } in
  let d = A.authorize (base_request ~principal_revision:3 ~pin ()) in
  ignore (expect_deny ~check:"principal" ~code:"stale_principal_revision" d)

let test_deny_confirmation_required () =
  let d =
    A.authorize
      (base_request ~confirmation_required:true ~confirmation_satisfied:false ())
  in
  ignore (expect_deny ~check:"confirmation" ~code:"confirmation_required" d)

let test_deny_stale_confirmation () =
  let pin = { A.empty_revision_pin with confirmation_id = Some "conf_old" } in
  let d = A.authorize (base_request ~confirmation_id:(Some "conf_1") ~pin ()) in
  ignore (expect_deny ~check:"confirmation" ~code:"stale_confirmation" d)

(* -------------------------------------------------------------------------- *)
(* Binding / ambiguous / none                                                  *)
(* -------------------------------------------------------------------------- *)

let test_deny_account_ambiguous () =
  let d = A.authorize (base_request ~binding:A.Ambiguous ()) in
  ignore (expect_deny ~check:"binding" ~code:"account_ambiguous" d)

let test_deny_none_eligible () =
  let d = A.authorize (base_request ~binding:A.None_eligible ()) in
  ignore (expect_deny ~check:"binding" ~code:"no_eligible_account" d)

let test_deny_binding_not_authorized () =
  let d =
    A.authorize
      (base_request ~binding:(A.Selected (selected ~authorized:false ())) ())
  in
  ignore (expect_deny ~check:"binding" ~code:"binding_not_authorized" d)

let test_deny_vault_inactive () =
  let d =
    A.authorize
      (base_request ~binding:(A.Selected (selected ~vault_active:false ())) ())
  in
  ignore (expect_deny ~check:"binding" ~code:"vault_inactive" d)

let test_deny_lineage_mismatch () =
  let d =
    A.authorize
      (base_request
         ~binding:(A.Selected (selected ~lineage_matches_pin:false ()))
         ())
  in
  ignore (expect_deny ~check:"binding" ~code:"lineage_mismatch" d)

let test_deny_stale_vault_generation () =
  let pin = { A.empty_revision_pin with vault_generation = Some 1 } in
  let d =
    A.authorize
      (base_request
         ~binding:(A.Selected (selected ~vault_generation:2 ()))
         ~pin ())
  in
  ignore (expect_deny ~check:"binding" ~code:"stale_vault_generation" d)

let test_deny_stale_binding_lineage () =
  let pin = { A.empty_revision_pin with binding_lineage_id = Some "lin_old" } in
  let d = A.authorize (base_request ~pin ()) in
  ignore (expect_deny ~check:"binding" ~code:"stale_binding_lineage" d)

let test_user_required_rejects_not_required_binding () =
  let d =
    A.authorize (base_request ~action:"merge" ~binding:A.Not_required ())
  in
  ignore (expect_deny ~check:"binding" ~code:"binding_required" d)

(* -------------------------------------------------------------------------- *)
(* Installation / SSO / live                                                   *)
(* -------------------------------------------------------------------------- *)

let test_deny_installation_inactive () =
  let d = A.authorize (base_request ~installation_active:false ()) in
  ignore (expect_deny ~check:"installation" ~code:"installation_inactive" d)

let test_deny_installation_repo () =
  let d = A.authorize (base_request ~installation_repo_ok:false ()) in
  ignore (expect_deny ~check:"installation" ~code:"installation_repo_denied" d)

let test_deny_permissions () =
  let d = A.authorize (base_request ~permissions_ok:false ()) in
  ignore (expect_deny ~check:"installation" ~code:"permissions_insufficient" d)

let test_deny_stale_installation_revision () =
  let pin =
    { A.empty_revision_pin with installation_revision = Some "old_inst" }
  in
  let d = A.authorize (base_request ~pin ()) in
  ignore
    (expect_deny ~check:"installation" ~code:"stale_installation_revision" d)

let test_deny_user_authority () =
  let d = A.authorize (base_request ~user_authority_ok:false ()) in
  ignore (expect_deny ~check:"user_org_sso" ~code:"user_authority_lost" d)

let test_deny_org_policy () =
  let d = A.authorize (base_request ~org_policy_ok:false ()) in
  ignore (expect_deny ~check:"user_org_sso" ~code:"org_policy_denied" d)

let test_deny_sso () =
  let d = A.authorize (base_request ~sso_ok:false ()) in
  ignore (expect_deny ~check:"user_org_sso" ~code:"sso_required" d)

let test_deny_live_state () =
  let d =
    A.authorize
      (base_request ~live_ok:false
         ~live_detail:(Some "head_sha mismatch: planned abc live def") ())
  in
  let denied = expect_deny ~check:"live_action" ~code:"live_state_failed" d in
  Alcotest.(check bool)
    "message mentions head" true
    (contains ~needle:"head_sha" denied.repair.message)

let test_deny_stale_live_revision () =
  let pin =
    { A.empty_revision_pin with live_state_revision = Some "sha_old" }
  in
  let d = A.authorize (base_request ~live_revision:(Some "sha_abc") ~pin ()) in
  ignore (expect_deny ~check:"live_action" ~code:"stale_live_state_revision" d)

let test_deny_stale_actor_snapshot () =
  let pin = { A.empty_revision_pin with actor_snapshot_id = Some "snap_old" } in
  let d =
    A.authorize (base_request ~actor_snapshot_id:(Some "snap_1") ~pin ())
  in
  ignore (expect_deny ~check:"actor_snapshot" ~code:"stale_actor_snapshot" d)

let test_deny_empty_action () =
  let d = A.authorize (base_request ~action:"   " ()) in
  ignore (expect_deny ~check:"policy" ~code:"empty_action" d)

let test_unknown_action_fail_closed_user_required () =
  (* Unknown actions are User_required Critical via policy; still need binding. *)
  let d = A.authorize (base_request ~action:"totally_new_mutation" ()) in
  let a = expect_allow ~mode:A.User d in
  Alcotest.(check string)
    "critical" "critical"
    (Policy.risk_tier_to_string a.requirement.tier)

let test_repair_never_looks_like_token () =
  let d = A.authorize (base_request ~sso_ok:false ()) in
  match d with
  | A.Deny denied ->
      let blob =
        Yojson.Safe.to_string (A.decision_to_json d)
        ^ denied.repair.message ^ denied.repair.code
      in
      List.iter
        (fun needle ->
          Alcotest.(check bool) ("no " ^ needle) false (contains ~needle blob))
        [ "ghu_"; "ghr_"; "Bearer "; "access_token"; "refresh_token" ]
  | A.Allow _ -> Alcotest.fail "expected deny"

let test_make_selected_binding_rejects_empty () =
  (match A.make_selected_binding ~binding_id:"" ~lineage_id:"lin" () with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "empty binding_id");
  match A.make_selected_binding ~binding_id:"b" ~lineage_id:"" () with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "empty lineage_id"

let test_string_of_decision () =
  let allow_s =
    A.string_of_decision
      (A.authorize
         (base_request ~action:"label" ~binding:A.Not_required
            ~confirmation_required:false ~confirmation_id:None ()))
  in
  Alcotest.(check bool)
    "allow prefix" true
    (String.length allow_s >= 5 && String.sub allow_s 0 5 = "allow");
  let deny_s =
    A.string_of_decision (A.authorize (base_request ~sso_ok:false ()))
  in
  Alcotest.(check bool)
    "deny prefix" true
    (String.length deny_s >= 4 && String.sub deny_s 0 4 = "deny")

let test_app_path_skips_user_authority () =
  (* App path: user_authority_ok=false is ignored; org/sso still apply. *)
  let d =
    A.authorize
      (base_request ~action:"comment" ~binding:A.Not_required
         ~confirmation_required:false ~confirmation_id:None
         ~user_authority_ok:false ())
  in
  ignore (expect_allow ~mode:A.App d);
  let d2 =
    A.authorize
      (base_request ~action:"comment" ~binding:A.Not_required
         ~confirmation_required:false ~confirmation_id:None ~sso_ok:false ())
  in
  ignore (expect_deny ~check:"user_org_sso" ~code:"sso_required" d2)

let suite =
  [
    Alcotest.test_case "allow User_required merge" `Quick
      test_allow_user_required_merge;
    Alcotest.test_case "allow App_installation comment" `Quick
      test_allow_app_installation_comment;
    Alcotest.test_case "allow with matching pins" `Quick
      test_allow_with_matching_pins;
    Alcotest.test_case "deny tool not in catalog" `Quick
      test_deny_tool_not_in_catalog;
    Alcotest.test_case "deny stale tool catalog revision" `Quick
      test_deny_stale_tool_catalog_revision;
    Alcotest.test_case "deny repo not granted" `Quick test_deny_repo_not_granted;
    Alcotest.test_case "deny repo blocked wins" `Quick
      test_deny_repo_blocked_wins;
    Alcotest.test_case "deny stale access revision" `Quick
      test_deny_stale_access_revision;
    Alcotest.test_case "deny principal not current" `Quick
      test_deny_principal_not_current;
    Alcotest.test_case "deny stale principal revision" `Quick
      test_deny_stale_principal_revision;
    Alcotest.test_case "deny confirmation required" `Quick
      test_deny_confirmation_required;
    Alcotest.test_case "deny stale confirmation" `Quick
      test_deny_stale_confirmation;
    Alcotest.test_case "deny account ambiguous" `Quick
      test_deny_account_ambiguous;
    Alcotest.test_case "deny none eligible" `Quick test_deny_none_eligible;
    Alcotest.test_case "deny binding not authorized" `Quick
      test_deny_binding_not_authorized;
    Alcotest.test_case "deny vault inactive" `Quick test_deny_vault_inactive;
    Alcotest.test_case "deny lineage mismatch" `Quick test_deny_lineage_mismatch;
    Alcotest.test_case "deny stale vault generation" `Quick
      test_deny_stale_vault_generation;
    Alcotest.test_case "deny stale binding lineage" `Quick
      test_deny_stale_binding_lineage;
    Alcotest.test_case "User_required rejects Not_required binding" `Quick
      test_user_required_rejects_not_required_binding;
    Alcotest.test_case "deny installation inactive" `Quick
      test_deny_installation_inactive;
    Alcotest.test_case "deny installation repo" `Quick
      test_deny_installation_repo;
    Alcotest.test_case "deny permissions" `Quick test_deny_permissions;
    Alcotest.test_case "deny stale installation revision" `Quick
      test_deny_stale_installation_revision;
    Alcotest.test_case "deny user authority" `Quick test_deny_user_authority;
    Alcotest.test_case "deny org policy" `Quick test_deny_org_policy;
    Alcotest.test_case "deny sso" `Quick test_deny_sso;
    Alcotest.test_case "deny live state" `Quick test_deny_live_state;
    Alcotest.test_case "deny stale live revision" `Quick
      test_deny_stale_live_revision;
    Alcotest.test_case "deny stale actor snapshot" `Quick
      test_deny_stale_actor_snapshot;
    Alcotest.test_case "deny empty action" `Quick test_deny_empty_action;
    Alcotest.test_case "unknown action fail-closed User_required" `Quick
      test_unknown_action_fail_closed_user_required;
    Alcotest.test_case "repair never looks like token" `Quick
      test_repair_never_looks_like_token;
    Alcotest.test_case "make_selected_binding rejects empty" `Quick
      test_make_selected_binding_rejects_empty;
    Alcotest.test_case "string_of_decision" `Quick test_string_of_decision;
    Alcotest.test_case "App path skips user_authority" `Quick
      test_app_path_skips_user_authority;
  ]
