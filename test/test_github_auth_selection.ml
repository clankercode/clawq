(** Tests for deterministic GitHub PAT vs App auth selection (P19.M2.E1.T005).
*)

module A = Github_auth_selection
module S = Github_app_installation_scope

let fixed_now = 1_700_000_000.0
let account = S.{ login = "acme-corp"; id = 99; account_type = "Organization" }
let perms = [ ("issues", "write"); ("metadata", "read") ]

let sample_app ?(app_id = 42) ?(installation_id = 1001) () :
    Runtime_config.github_app_config =
  {
    app_id;
    private_key_path = "/tmp/github-app.pem";
    webhook_secret = "whsec";
    installations = [ { installation_id; repos = [ "acme-corp/alpha" ] } ];
  }

let sample_scope ?(installation_id = 1001) ?(selection = S.All_repos)
    ?(repositories = []) ?(revoked = []) ?(status = S.Active)
    ?(login = "acme-corp") () : S.t =
  S.with_revision
    {
      installation_id;
      app_id = Some 42;
      account = { account with login };
      selection;
      repositories;
      revoked_repositories = revoked;
      permissions = perms;
      status;
      revision = "";
      updated_at = Time_util.iso8601_utc ~t:fixed_now ();
    }

let repo_ref name : S.repo_ref =
  { full_name = name; id = None; private_ = Some false }

let check_reason msg expected (sel : A.selection) =
  Alcotest.(check string)
    msg
    (A.selection_reason_to_string expected)
    (A.selection_reason_to_string sel.reason)

let check_chosen_pat (sel : A.selection) =
  match sel.chosen with
  | `Pat -> ()
  | `App id -> Alcotest.fail (Printf.sprintf "expected Pat, got App %d" id)
  | `None -> Alcotest.fail "expected Pat, got None"

let check_chosen_app ~iid (sel : A.selection) =
  match sel.chosen with
  | `App id -> Alcotest.(check int) "installation_id" iid id
  | `Pat -> Alcotest.fail "expected App, got Pat"
  | `None -> Alcotest.fail "expected App, got None"

(* Manual substring check for OCaml 5.1. *)
let contains_ci hay needle =
  let hay = String.lowercase_ascii hay in
  let needle = String.lowercase_ascii needle in
  let n = String.length needle in
  let h = String.length hay in
  if n = 0 then true
  else if n > h then false
  else
    let rec loop i =
      if i > h - n then false
      else if String.sub hay i n = needle then true
      else loop (i + 1)
    in
    loop 0

let check_explanation_has msg needle (sel : A.selection) =
  Alcotest.(check bool) msg true (contains_ci sel.explanation needle)

(* 1. PAT-only exact repo selects Pat *)
let test_pat_only_exact_repo () =
  let auth = A.snapshot_of_parts ~pat:"ghp_test_token_value" () in
  let sel = A.select_for_repo ~auth ~repo_full_name:"acme-corp/alpha" () in
  Alcotest.(check string) "mode" "pat_only" (A.auth_mode_to_string sel.mode);
  check_reason "reason" A.Pat_exact_repo sel;
  check_chosen_pat sel;
  Alcotest.(check (option int)) "no installation" None sel.installation_id;
  Alcotest.(check (option string)) "repo" (Some "acme-corp/alpha") sel.repo

(* 2. App-only with authorized installation selects App *)
let test_app_only_authorized () =
  let auth = A.snapshot_of_parts ~app:(sample_app ()) () in
  let installation = sample_scope ~selection:S.All_repos () in
  let sel =
    A.select_for_repo ~auth ~installation ~repo_full_name:"acme-corp/alpha" ()
  in
  Alcotest.(check string) "mode" "app_only" (A.auth_mode_to_string sel.mode);
  check_reason "reason" A.App_installation_scope sel;
  check_chosen_app ~iid:1001 sel;
  Alcotest.(check (option int)) "installation" (Some 1001) sel.installation_id

(* 3. Mixed both viable prefers App and explains *)
let test_mixed_prefers_app () =
  let auth =
    A.snapshot_of_parts ~pat:"ghp_test_token_value" ~app:(sample_app ()) ()
  in
  let installation = sample_scope ~selection:S.All_repos () in
  let sel =
    A.select_for_repo ~auth ~installation ~repo_full_name:"acme-corp/alpha" ()
  in
  Alcotest.(check string) "mode" "mixed" (A.auth_mode_to_string sel.mode);
  check_reason "reason" A.App_preferred_when_mixed sel;
  check_chosen_app ~iid:1001 sel;
  check_explanation_has "explains prefer App" "prefer" sel;
  check_explanation_has "mentions App" "App" sel

(* 4. Mixed App suspended falls back to PAT *)
let test_mixed_suspended_falls_back_pat () =
  let auth =
    A.snapshot_of_parts ~pat:"ghp_test_token_value" ~app:(sample_app ()) ()
  in
  let installation =
    sample_scope ~status:(S.Suspended { reason = Some "billing" }) ()
  in
  let sel =
    A.select_for_repo ~auth ~installation ~repo_full_name:"acme-corp/alpha" ()
  in
  Alcotest.(check string) "mode" "mixed" (A.auth_mode_to_string sel.mode);
  check_reason "reason" A.Pat_fallback_exact_repo sel;
  check_chosen_pat sel;
  check_explanation_has "mentions suspended" "suspend" sel;
  check_explanation_has "mentions PAT" "PAT" sel

(* 5. Org route PAT-only rejected with explanation containing "App" *)
let test_org_pat_only_rejected () =
  let auth = A.snapshot_of_parts ~pat:"ghp_test_token_value" () in
  let sel = A.select_for_org_route ~auth ~org:"acme-corp" () in
  check_reason "reason" A.Rejected_org_requires_app sel;
  Alcotest.(check bool) "chosen none" true (sel.chosen = `None);
  check_explanation_has "mentions App" "App" sel;
  check_explanation_has "mentions PAT" "PAT" sel

(* 6. Org route with matching active installation accepts App *)
let test_org_matching_active_accepts () =
  let auth = A.snapshot_of_parts ~app:(sample_app ()) () in
  let installation = sample_scope ~login:"acme-corp" ~status:S.Active () in
  let sel = A.select_for_org_route ~auth ~installation ~org:"Acme-Corp" () in
  Alcotest.(check string) "mode" "app_only" (A.auth_mode_to_string sel.mode);
  check_reason "reason" A.App_installation_scope sel;
  check_chosen_app ~iid:1001 sel

(* 7. Org route wrong account rejected *)
let test_org_wrong_account_rejected () =
  let auth = A.snapshot_of_parts ~app:(sample_app ()) () in
  let installation = sample_scope ~login:"other-org" ~status:S.Active () in
  let sel = A.select_for_org_route ~auth ~installation ~org:"acme-corp" () in
  check_reason "reason" A.Rejected_org_requires_app sel;
  Alcotest.(check bool) "chosen none" true (sel.chosen = `None);
  check_explanation_has "mentions mismatch" "does not match" sel

(* 8. migration_safe errors when PAT dropped without confirm *)
let test_migration_safe_errors_drop_without_confirm () =
  let before = A.snapshot_of_parts ~pat:"ghp_old" () in
  let after = A.snapshot_of_parts ~app:(sample_app ()) () in
  match A.migration_safe ~before ~after ~confirmed_apply:false with
  | Ok () -> Alcotest.fail "expected error when PAT dropped without confirm"
  | Error msg ->
      Alcotest.(check bool) "mentions PAT" true (contains_ci msg "PAT");
      Alcotest.(check bool) "mentions confirm" true (contains_ci msg "confirm")

(* 9. migration_safe ok when PAT retained *)
let test_migration_safe_ok_pat_retained () =
  let before = A.snapshot_of_parts ~pat:"ghp_old" () in
  let after = A.snapshot_of_parts ~pat:"ghp_old" ~app:(sample_app ()) () in
  match A.migration_safe ~before ~after ~confirmed_apply:false with
  | Ok () ->
      Alcotest.(check bool)
        "preserves" true
        (A.migration_preserves_pat ~before ~after)
  | Error e -> Alcotest.fail e

(* 10. migration_safe ok when confirmed_apply drops PAT *)
let test_migration_safe_ok_confirmed_drop () =
  let before = A.snapshot_of_parts ~pat:"ghp_old" () in
  let after = A.snapshot_of_parts ~app:(sample_app ()) () in
  match A.migration_safe ~before ~after ~confirmed_apply:true with
  | Ok () ->
      Alcotest.(check bool)
        "does not preserve without keep" false
        (A.migration_preserves_pat ~before ~after)
  | Error e -> Alcotest.fail e

(* 11. can_claim_org_scope false for PAT-only *)
let test_can_claim_org_scope_pat_only () =
  let auth = A.snapshot_of_parts ~pat:"ghp_test_token_value" () in
  let installation = Some (sample_scope ()) in
  Alcotest.(check bool)
    "pat cannot claim org" false
    (A.can_claim_org_scope ~auth ~installation);
  let auth_app = A.snapshot_of_parts ~app:(sample_app ()) () in
  Alcotest.(check bool)
    "app+active can claim" true
    (A.can_claim_org_scope ~auth:auth_app ~installation);
  Alcotest.(check bool)
    "app without installation cannot" false
    (A.can_claim_org_scope ~auth:auth_app ~installation:None)

(* Extra: snapshot / classify helpers + selected-repos unauthorized fallback *)
let test_snapshot_and_classify () =
  let pat_auth = Some (Runtime_config.GithubPat "ghp_abc1234") in
  let app_auth = Some (Runtime_config.GithubApp (sample_app ())) in
  Alcotest.(check string)
    "pat classify" "pat_only"
    (A.auth_mode_to_string (A.classify_auth pat_auth));
  Alcotest.(check string)
    "app classify" "app_only"
    (A.auth_mode_to_string (A.classify_auth app_auth));
  let mixed = A.snapshot_of_parts ~pat:"ghp_x" ~app:(sample_app ()) () in
  Alcotest.(check string)
    "mixed classify" "mixed"
    (A.auth_mode_to_string (A.classify_snapshot mixed));
  let snap_pat = A.snapshot_of_auth pat_auth in
  Alcotest.(check bool) "pat present" true snap_pat.pat_token_present;
  Alcotest.(check bool) "no app" true (snap_pat.app = None)

let test_selected_repos_unauthorized_pat_fallback () =
  let auth =
    A.snapshot_of_parts ~pat:"ghp_test_token_value" ~app:(sample_app ()) ()
  in
  let installation =
    sample_scope ~selection:S.Selected_repos
      ~repositories:[ repo_ref "acme-corp/other" ]
      ()
  in
  let sel =
    A.select_for_repo ~auth ~installation ~repo_full_name:"acme-corp/alpha" ()
  in
  check_reason "fallback" A.Pat_fallback_exact_repo sel;
  check_chosen_pat sel

let test_scope_must_belong_to_configured_app_installation () =
  let app = sample_app () in
  let auth = A.snapshot_of_parts ~pat:"ghp_test_token_value" ~app () in
  let mismatched_scope =
    S.with_revision { (sample_scope ()) with app_id = Some 7 }
  in
  let repo_selection =
    A.select_for_repo ~auth ~installation:mismatched_scope
      ~repo_full_name:"acme-corp/alpha" ()
  in
  check_reason "PAT fallback" A.Pat_fallback_exact_repo repo_selection;
  check_chosen_pat repo_selection;
  Alcotest.(check bool)
    "cannot claim Org through mismatched App" false
    (A.can_claim_org_scope ~auth ~installation:(Some mismatched_scope));
  let org_selection =
    A.select_for_org_route ~auth ~installation:mismatched_scope ~org:"acme-corp"
      ()
  in
  check_reason "Org rejects mismatched App" A.Rejected_org_requires_app
    org_selection

let suite =
  [
    Alcotest.test_case "PAT-only exact repo selects Pat" `Quick
      test_pat_only_exact_repo;
    Alcotest.test_case "App-only authorized installation selects App" `Quick
      test_app_only_authorized;
    Alcotest.test_case "Mixed both viable prefers App and explains" `Quick
      test_mixed_prefers_app;
    Alcotest.test_case "Mixed App suspended falls back to PAT" `Quick
      test_mixed_suspended_falls_back_pat;
    Alcotest.test_case "Org route PAT-only rejected explains App" `Quick
      test_org_pat_only_rejected;
    Alcotest.test_case "Org route matching active installation accepts App"
      `Quick test_org_matching_active_accepts;
    Alcotest.test_case "Org route wrong account rejected" `Quick
      test_org_wrong_account_rejected;
    Alcotest.test_case "migration_safe errors when PAT dropped without confirm"
      `Quick test_migration_safe_errors_drop_without_confirm;
    Alcotest.test_case "migration_safe ok when PAT retained" `Quick
      test_migration_safe_ok_pat_retained;
    Alcotest.test_case "migration_safe ok when confirmed_apply drops PAT" `Quick
      test_migration_safe_ok_confirmed_drop;
    Alcotest.test_case "can_claim_org_scope false for PAT-only" `Quick
      test_can_claim_org_scope_pat_only;
    Alcotest.test_case "snapshot/classify helpers" `Quick
      test_snapshot_and_classify;
    Alcotest.test_case "selected-repos unauthorized falls back to PAT" `Quick
      test_selected_repos_unauthorized_pat_fallback;
    Alcotest.test_case "scope belongs to configured App installation" `Quick
      test_scope_must_belong_to_configured_app_installation;
  ]
