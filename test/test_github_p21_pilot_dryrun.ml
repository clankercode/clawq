(** P21 Teams dual-attribution pilot dry-run (P21.M4.E2.T003).

    Proves the software contract for staged rollout gates, dual-Principal
    isolation semantics, and attribution matrix / fallback paths without live
    Teams, GitHub OAuth, or public webhooks. Does NOT claim a live pilot ran.

    Live pilot remains BLOCKED without: Teams pilot room + dual GitHub user
    OAuth credentials + public webhook/callback URL. See
    docs/pilots/p21-teams-dual-attribution-pilot-runbook.md §14. *)

module R = Github_attribution_rollout
module Policy = Github_attribution_policy
module F = Github_attribution_fallback

let fixed_now = 1_700_000_000.0
let future_exp = "2099-01-01T00:00:00Z"
let contains = Test_helpers.string_contains

let repo_root () =
  let rec find_from dir =
    let has_file name = Sys.file_exists (Filename.concat dir name) in
    if has_file "dune-project" && has_file "src" && has_file "docs" then
      Some dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then None else find_from parent
  in
  match find_from (Sys.getcwd ()) with
  | Some dir -> dir
  | None ->
      let exe =
        if Filename.is_relative Sys.executable_name then
          Filename.concat (Sys.getcwd ()) Sys.executable_name
        else Sys.executable_name
      in
      find_from (Filename.dirname exe) |> Option.value ~default:(Sys.getcwd ())

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let doc path_rel =
  let path = Filename.concat (repo_root ()) path_rel in
  Alcotest.(check bool) (path_rel ^ " exists") true (Sys.file_exists path);
  read_file path

let must_contain ~label ~doc phrases =
  List.iter
    (fun phrase ->
      Alcotest.(check bool)
        (Printf.sprintf "%s contains %S" label phrase)
        true (contains doc phrase))
    phrases

let expect_path expected got =
  Alcotest.(check string)
    "effective path"
    (R.effective_path_to_string expected)
    (R.effective_path_to_string got)

let expect_denied ~code got =
  match got with
  | R.Path_denied { code = c; message } ->
      Alcotest.(check string) "deny code" code c;
      Alcotest.(check bool) "message non-empty" true (String.trim message <> "");
      message
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Path_denied %s, got %s" code
           (R.effective_path_to_string other))

let expect_transition_ok req =
  match R.validate_transition req with
  | Ok r -> r
  | Error e -> Alcotest.fail ("expected Ok transition, got: " ^ e)

let expect_fallback_allow ?(mode = F.User) ?(used_app_fallback = false) d =
  match d with
  | F.Allow a ->
      Alcotest.(check string)
        "mode"
        (F.actor_mode_to_string mode)
        (F.actor_mode_to_string a.mode);
      Alcotest.(check bool)
        "used_app_fallback" used_app_fallback a.used_app_fallback;
      a
  | F.Deny den ->
      Alcotest.fail (Printf.sprintf "expected Allow, got Deny code=%s" den.code)

let expect_fallback_deny ~code d =
  match d with
  | F.Deny den ->
      Alcotest.(check string) "deny code" code den.code;
      den
  | F.Allow a ->
      Alcotest.fail
        (Printf.sprintf "expected Deny, got Allow mode=%s"
           (F.actor_mode_to_string a.mode))

let production_on =
  {
    R.enabled = true;
    audit_ref = Some "aud-p21-pilot-dryrun";
    enabled_at = Some "2026-07-13T00:00:00Z";
  }

let prod_input ~action ?(user_auth_available = true) () =
  R.default_resolve_input ~action ~stage:R.P21_production
    ~production:production_on ~readiness:R.all_ready ~now:fixed_now
    ~user_auth_available ()

(* -------------------------------------------------------------------------- *)
(* Docs presence (runbook / checklist / receipt)                                *)
(* -------------------------------------------------------------------------- *)

let test_pilot_docs_present_and_state_blocked_contract () =
  let runbook = doc "docs/pilots/p21-teams-dual-attribution-pilot-runbook.md" in
  let checklist =
    doc "docs/pilots/p21-teams-dual-attribution-pilot-checklist.md"
  in
  let receipt = doc "docs/pilots/p21-redacted-pilot-receipt-template.md" in
  must_contain ~label:"runbook" ~doc:runbook
    [
      "P21.M4.E2.T003";
      "Dry-run / blocked environments";
      "dual GitHub user OAuth";
      "public webhook";
      "Teams pilot room";
      "safe_default";
      "p21_production";
      "web OAuth";
      "device";
      "User_required";
      "User_preferred";
      "no App/PAT fallback";
      "relink";
      "revoke";
      "key rotation";
      "SSO";
      "restart";
      "webhook";
      "rollback";
      "cleanup";
      "github_p21_pilot_dryrun";
      "must not";
      "claim a live pilot";
      "Do **not** claim the live pilot executed";
    ];
  must_contain ~label:"checklist" ~doc:checklist
    [
      "P21.M4.E2.T003";
      "Mode:";
      "Live";
      "Dry-run / blocked";
      "BLOCKED";
      "github_p21_pilot_dryrun";
      "Principal A";
      "Principal B";
      "Participant C";
      "safe_default";
      "no_residual_authority";
    ];
  must_contain ~label:"receipt" ~doc:receipt
    [
      "P21.M4.E2.T003";
      "P21.M4.E2.T004";
      "dry-run_blocked";
      "Never";
      "secrets";
      "safe_default";
      "web PKCE";
      "device";
      "no_residual_authority";
      "access_token";
      "refresh_token";
    ]

(* -------------------------------------------------------------------------- *)
(* Staged rollout gates (pilot path through production → rollback → cleanup)    *)
(* -------------------------------------------------------------------------- *)

let test_staged_rollout_gate_sequence () =
  (* Start safe: User_required denied. *)
  ignore
    (expect_denied ~code:"user_required_gate_disabled"
       (R.resolve (R.default_resolve_input ~action:"merge" ~now:fixed_now ())));
  Alcotest.(check bool)
    "user_required disabled by default" true
    (R.user_required_disabled_by_default ());

  (* Production enable requires readiness. *)
  (match
     R.validate_transition
       {
         R.kind = R.Gate_production_enable;
         from_stage = R.Safe_default;
         pilot = None;
         production = Some production_on;
         rollback = None;
         cleanup = None;
         readiness = R.empty_readiness;
         audit_ref = Some "aud-p21-pilot-dryrun";
       }
   with
  | Error e ->
      Alcotest.(check bool)
        "mentions readiness" true
        (contains e "readiness" || contains e "ready")
  | Ok _ -> Alcotest.fail "production enable without readiness must fail");

  let enabled =
    expect_transition_ok
      {
        R.kind = R.Gate_production_enable;
        from_stage = R.Safe_default;
        pilot = None;
        production = Some production_on;
        rollback = None;
        cleanup = None;
        readiness = R.all_ready;
        audit_ref = Some "aud-p21-pilot-dryrun";
      }
  in
  Alcotest.(check string)
    "to production" "p21_production"
    (R.stage_to_string enabled.to_stage);
  Alcotest.(check bool) "prod on" true enabled.production.enabled;

  (* Under production: User_required and User_preferred open; reads App. *)
  expect_path R.Path_user (R.resolve (prod_input ~action:"merge" ()));
  expect_path R.Path_user (R.resolve (prod_input ~action:"comment" ()));
  expect_path R.Path_app_primary (R.resolve (prod_input ~action:"read" ()));

  (* Rollback closes user paths without reopening pilot App. *)
  let rb =
    {
      R.active = true;
      reason = "p21 dual-attr pilot end";
      audit_ref = Some "aud-rb";
      restores_stage = R.Safe_default;
    }
  in
  let rolled =
    expect_transition_ok
      {
        R.kind = R.Gate_rollback;
        from_stage = R.P21_production;
        pilot = None;
        production = Some production_on;
        rollback = Some rb;
        cleanup = None;
        readiness = R.all_ready;
        audit_ref = Some "aud-rb";
      }
  in
  Alcotest.(check string)
    "to rollback" "rollback"
    (R.stage_to_string rolled.to_stage);
  Alcotest.(check bool)
    "prod off after rollback" false rolled.production.enabled;
  ignore
    (expect_denied ~code:"rollout_rollback_active"
       (R.resolve
          (R.default_resolve_input ~action:"merge" ~stage:R.Rollback
             ~production:production_on ~readiness:R.all_ready ~now:fixed_now
             ~user_auth_available:true ())));

  (* Cleanup → safe_default with residual proof. *)
  let cleanup =
    {
      R.active = true;
      audit_ref = Some "aud-c";
      residual_authority_cleared = true;
      pilot_credentials_destroyed = true;
      bindings_unlinked = true;
    }
  in
  let cleaned =
    expect_transition_ok
      {
        R.kind = R.Gate_cleanup;
        from_stage = R.Rollback;
        pilot = None;
        production = None;
        rollback = None;
        cleanup = Some cleanup;
        readiness = R.empty_readiness;
        audit_ref = Some "aud-c";
      }
  in
  Alcotest.(check string)
    "to safe" "safe_default"
    (R.stage_to_string cleaned.to_stage);
  Alcotest.(check bool)
    "no residual" true
    (R.no_residual_authority ~production:R.default_production_gate
       ~pilot_gates:(R.default_pilot_gates ()) ~now:fixed_now ~cleanup)

let test_p19_pilot_not_production_substitute () =
  (* With production off, active P19 pilot opens Path_pilot_app only for
     pilot_allowed rows — never as silent substitute after production disable. *)
  let pilot =
    {
      R.enabled = true;
      pilot_name = "p19-merge-pilot";
      expires_at = Some future_exp;
      audit_ref = Some "aud-p19-side";
    }
  in
  expect_path R.Path_pilot_app
    (R.resolve
       (R.default_resolve_input ~action:"merge" ~stage:R.P19_pilot
          ~pilot_gates:[ pilot ] ~now:fixed_now ()));
  (* Preferred ordinary mutation is not pilot_allowed. *)
  let denied =
    R.resolve
      (R.default_resolve_input ~action:"comment" ~stage:R.P19_pilot
         ~pilot_gates:[ pilot ] ~now:fixed_now ())
  in
  ignore (expect_denied ~code:"attribution_gate_disabled" denied);
  (* Production path never claims pilot_app for User_required when gate on. *)
  expect_path R.Path_user (R.resolve (prod_input ~action:"merge" ()))

(* -------------------------------------------------------------------------- *)
(* Dual-Principal isolation (logical handles; pure policy surface)              *)
(* -------------------------------------------------------------------------- *)

type pilot_principal = {
  principal_id : string;
  link_flow : string; (* "web" | "device" | "none" *)
  github_user_id : int64 option;
  lineage_id : string option;
  binding_authorized : bool;
}
(** Logical dual-Principal fixture used only for dry-run isolation assertions.
    No network, vault, or DB. *)

let principal_a =
  {
    principal_id = "prin_a_web";
    link_flow = "web";
    github_user_id = Some 1001L;
    lineage_id = Some "lin_a";
    binding_authorized = true;
  }

let principal_b =
  {
    principal_id = "prin_b_device";
    link_flow = "device";
    github_user_id = Some 1002L;
    lineage_id = Some "lin_b";
    binding_authorized = true;
  }

let principal_c =
  {
    principal_id = "prin_c_unlinked";
    link_flow = "none";
    github_user_id = None;
    lineage_id = None;
    binding_authorized = false;
  }

let user_path_for (p : pilot_principal) =
  p.binding_authorized && Option.is_some p.github_user_id

let test_dual_principal_isolation_contract () =
  Alcotest.(check bool)
    "A and B distinct principals" true
    (principal_a.principal_id <> principal_b.principal_id);
  Alcotest.(check bool)
    "A and B distinct github users" true
    (principal_a.github_user_id <> principal_b.github_user_id);
  Alcotest.(check bool)
    "A and B distinct lineage" true
    (principal_a.lineage_id <> principal_b.lineage_id);
  Alcotest.(check bool) "A web" true (principal_a.link_flow = "web");
  Alcotest.(check bool) "B device" true (principal_b.link_flow = "device");
  Alcotest.(check bool) "C unlinked" true (not (user_path_for principal_c));

  (* Under production, linked Principals open Path_user at the rollout layer. *)
  expect_path R.Path_user
    (R.resolve (prod_input ~action:"comment" ~user_auth_available:true ()));
  expect_path R.Path_user
    (R.resolve (prod_input ~action:"merge" ~user_auth_available:true ()));
  (* Unlinked C: User_required never falls back to App when preview names App. *)
  ignore
    (expect_fallback_deny ~code:"user_required_no_fallback"
       (F.resolve
          (F.default_request ~action:"merge" ~preview_actor:F.Names_app
             ~attribution_gate_enabled:true ~user_path_available:false
             ~app_path_available:true ())));
  (* Unlinked C with user-named preview stays on User (no silent App) so
     authorize can emit binding repair — never App mode. *)
  let c_user_named =
    expect_fallback_allow ~mode:F.User ~used_app_fallback:false
      (F.resolve
         (F.default_request ~action:"merge" ~preview_actor:F.Names_user
            ~attribution_gate_enabled:true ~user_path_available:false
            ~app_path_available:true ()))
  in
  Alcotest.(check bool)
    "C not app fallback" false c_user_named.used_app_fallback;

  (* Cross-Principal borrow: locked User mode cannot switch to App on retry. *)
  let locked_retry =
    F.resolve
      (F.default_request ~action:"comment" ~preview_actor:F.Names_app
         ~attribution_gate_enabled:true ~user_path_available:false
         ~app_path_available:true
         ~phase:(F.Retry { locked_mode = F.User })
         ())
  in
  ignore
    (expect_fallback_deny ~code:"locked_user_path_unavailable" locked_retry);

  (* Distinct lineage ids must not coalesce in dry-run bookkeeping. *)
  let lineages =
    List.filter_map
      (fun p -> p.lineage_id)
      [ principal_a; principal_b; principal_c ]
  in
  Alcotest.(check int) "two lineages" 2 (List.length lineages);
  Alcotest.(check bool)
    "lineages unique" true
    (List.length lineages = List.length (List.sort_uniq String.compare lineages))

(* -------------------------------------------------------------------------- *)
(* Attribution matrix paths for pilot action families                           *)
(* -------------------------------------------------------------------------- *)

let preferred_actions = [ "comment"; "label"; "assign"; "review_request" ]

let required_actions =
  [
    "review_submit";
    "issue_create";
    "issue_close";
    "issue_reopen";
    "workflow_dispatch";
    "code_change";
    "merge";
    "room_background_work";
  ]

let read_actions =
  [ "read"; "search"; "get_status"; "get_item"; "list_room_items" ]

let test_attribution_matrix_pilot_families () =
  List.iter
    (fun action ->
      let row = R.lookup ~action in
      Alcotest.(check string)
        (action ^ " target app") "app_installation"
        (Policy.attribution_to_string row.target);
      expect_path R.Path_app_primary (R.resolve (prod_input ~action ())))
    read_actions;
  List.iter
    (fun action ->
      let row = R.lookup ~action in
      Alcotest.(check string)
        (action ^ " target preferred")
        "user_preferred"
        (Policy.attribution_to_string row.target);
      Alcotest.(check bool)
        (action ^ " visible fallback")
        true
        (Policy.permits_app_fallback row.target);
      Alcotest.(check string)
        (action ^ " fallback rule")
        "visible_app_fallback"
        (R.fallback_rule_to_string row.fallback);
      expect_path R.Path_user (R.resolve (prod_input ~action ())))
    preferred_actions;
  List.iter
    (fun action ->
      let row = R.lookup ~action in
      Alcotest.(check string)
        (action ^ " target required")
        "user_required"
        (Policy.attribution_to_string row.target);
      Alcotest.(check bool)
        (action ^ " no app fallback")
        false
        (Policy.permits_app_fallback row.target);
      Alcotest.(check string)
        (action ^ " no_fallback rule")
        "no_fallback"
        (R.fallback_rule_to_string row.fallback);
      Alcotest.(check string)
        (action ^ " pin delayed") "pin_actor_lineage"
        (R.delayed_rule_to_string row.delayed);
      expect_path R.Path_user (R.resolve (prod_input ~action ()));
      (* Safe default still denies. *)
      ignore
        (expect_denied ~code:"user_required_gate_disabled"
           (R.resolve (R.default_resolve_input ~action ~now:fixed_now ()))))
    required_actions

let test_fallback_visible_app_and_user_required () =
  (* User_preferred: user path for Principal A. *)
  ignore
    (expect_fallback_allow ~mode:F.User ~used_app_fallback:false
       (F.resolve
          (F.default_request ~action:"comment" ~preview_actor:F.Names_user
             ~attribution_gate_enabled:true ~user_path_available:true ())));
  (* Visible App fallback when preview names App. *)
  ignore
    (expect_fallback_allow ~mode:F.App ~used_app_fallback:true
       (F.resolve
          (F.default_request ~action:"comment" ~preview_actor:F.Names_app
             ~attribution_gate_enabled:true ~user_path_available:false
             ~app_path_available:true ())));
  (* No silent App when preview names user. *)
  let silent =
    F.resolve
      (F.default_request ~action:"comment" ~preview_actor:F.Names_user
         ~attribution_gate_enabled:true ~user_path_available:false
         ~app_path_available:true ())
  in
  let a = expect_fallback_allow ~mode:F.User ~used_app_fallback:false silent in
  Alcotest.(check bool) "not app fallback" false a.used_app_fallback;
  (* User_required never App. *)
  List.iter
    (fun action ->
      ignore
        (expect_fallback_deny ~code:"user_required_no_fallback"
           (F.resolve
              (F.default_request ~action ~preview_actor:F.Names_app
                 ~attribution_gate_enabled:true ~user_path_available:false
                 ~app_path_available:true ()))))
    required_actions

let test_safe_default_after_cleanup_matrix () =
  let cleanup =
    {
      R.active = true;
      audit_ref = Some "aud-c";
      residual_authority_cleared = true;
      pilot_credentials_destroyed = true;
      bindings_unlinked = true;
    }
  in
  Alcotest.(check bool) "cleanup_complete" true (R.cleanup_complete cleanup);
  List.iter
    (fun action ->
      ignore
        (expect_denied ~code:"user_required_gate_disabled"
           (R.resolve (R.default_resolve_input ~action ~now:fixed_now ()))))
    [ "merge"; "review_submit"; "code_change" ];
  List.iter
    (fun action ->
      ignore
        (expect_denied ~code:"attribution_gate_disabled"
           (R.resolve (R.default_resolve_input ~action ~now:fixed_now ()))))
    preferred_actions

let test_matrix_json_and_docs_no_live_claim () =
  let j = R.matrix_to_json (R.matrix ()) in
  let s = Yojson.Safe.to_string j in
  Alcotest.(check bool)
    "matrix_version present" true
    (contains s "matrix_version");
  Alcotest.(check bool)
    "no access_token" false
    (contains (String.lowercase_ascii s) "\"access_token\"");
  let runbook = doc "docs/pilots/p21-teams-dual-attribution-pilot-runbook.md" in
  Alcotest.(check bool)
    "does not claim live executed" false
    (contains runbook "Live pilot PASS executed in this environment");
  Alcotest.(check bool)
    "states blocked contract" true
    (contains runbook "Dry-run / blocked environments")

(* -------------------------------------------------------------------------- *)
(* Suite                                                                        *)
(* -------------------------------------------------------------------------- *)

let suite =
  [
    ( "pilot docs present and state blocked contract",
      `Quick,
      test_pilot_docs_present_and_state_blocked_contract );
    ( "staged rollout gate sequence production rollback cleanup",
      `Quick,
      test_staged_rollout_gate_sequence );
    ( "p19 pilot is not production substitute",
      `Quick,
      test_p19_pilot_not_production_substitute );
    ( "dual principal isolation contract",
      `Quick,
      test_dual_principal_isolation_contract );
    ( "attribution matrix pilot families",
      `Quick,
      test_attribution_matrix_pilot_families );
    ( "fallback visible app and user required",
      `Quick,
      test_fallback_visible_app_and_user_required );
    ( "safe default after cleanup matrix",
      `Quick,
      test_safe_default_after_cleanup_matrix );
    ( "matrix json and docs no live claim",
      `Quick,
      test_matrix_json_and_docs_no_live_claim );
  ]
