(** Tests for P19 → P21 attribution migration matrix and staged rollout
    (P21.M3.E2.T006). *)

module R = Github_attribution_rollout
module Policy = Github_attribution_policy

let fixed_now = 1_700_000_000.0
(* 2023-11-14T22:13:20Z approx; use expires_at well after this. *)

let future_exp = "2099-01-01T00:00:00Z"
let past_exp = "2000-01-01T00:00:00Z"

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

let expect_transition_err ~substr req =
  match R.validate_transition req with
  | Ok r ->
      Alcotest.fail
        (Printf.sprintf "expected Error containing %S, got Ok to_stage=%s"
           substr
           (R.stage_to_string r.to_stage))
  | Error e ->
      if not (Test_helpers.string_contains e substr) then
        Alcotest.fail (Printf.sprintf "error %S does not contain %S" e substr)

(* -------------------------------------------------------------------------- *)
(* Matrix                                                                       *)
(* -------------------------------------------------------------------------- *)

let test_matrix_version () =
  Alcotest.(check int) "matrix_version" 1 R.matrix_version;
  Alcotest.(check int) "schema_version" R.matrix_version R.schema_version

let test_matrix_covers_reads_mutations_background () =
  let rows = R.matrix () in
  let has action =
    List.exists (fun (r : R.matrix_row) -> r.action = action) rows
  in
  List.iter
    (fun a -> if not (has a) then Alcotest.fail ("matrix missing " ^ a))
    [
      "read";
      "search";
      "get_status";
      "comment";
      "label";
      "assign";
      "review_request";
      "review_submit";
      "issue_create";
      "issue_close";
      "issue_reopen";
      "workflow_dispatch";
      "code_change";
      "merge";
      "room_background_work";
      "pat_read";
    ];
  let has_read =
    List.exists (fun (r : R.matrix_row) -> r.surface = R.Read) rows
  in
  let has_mut =
    List.exists (fun (r : R.matrix_row) -> r.surface = R.Mutation) rows
  in
  let has_bg =
    List.exists (fun (r : R.matrix_row) -> r.surface = R.Background) rows
  in
  Alcotest.(check bool) "has read" true has_read;
  Alcotest.(check bool) "has mutation" true has_mut;
  Alcotest.(check bool) "has background" true has_bg

let test_matrix_covers_policy_defaults () =
  match R.matrix_covers_policy_defaults () with
  | Ok () -> ()
  | Error e -> Alcotest.fail e

let test_matrix_row_semantics_user_required () =
  let r = R.lookup ~action:"merge" in
  Alcotest.(check string)
    "target" "user_required"
    (Policy.attribution_to_string r.target);
  Alcotest.(check bool) "pilot_allowed" true r.pilot_allowed;
  Alcotest.(check (option string))
    "pilot_name" (Some "p19-merge-pilot") r.pilot_name;
  Alcotest.(check string)
    "preview" "preview_user_only"
    (R.preview_rule_to_string r.preview);
  Alcotest.(check string)
    "fallback" "no_fallback"
    (R.fallback_rule_to_string r.fallback);
  Alcotest.(check string)
    "delayed" "pin_actor_lineage"
    (R.delayed_rule_to_string r.delayed);
  Alcotest.(check bool) "prod gate" true r.production_requires_user_gate

let test_matrix_row_semantics_user_preferred () =
  let r = R.lookup ~action:"comment" in
  Alcotest.(check string)
    "target" "user_preferred"
    (Policy.attribution_to_string r.target);
  Alcotest.(check bool) "no pilot" false r.pilot_allowed;
  Alcotest.(check string)
    "fallback" "visible_app_fallback"
    (R.fallback_rule_to_string r.fallback)

let test_matrix_row_semantics_read () =
  let r = R.lookup ~action:"read" in
  Alcotest.(check string)
    "target" "app_installation"
    (Policy.attribution_to_string r.target);
  Alcotest.(check bool) "no prod gate" false r.production_requires_user_gate;
  Alcotest.(check string)
    "legacy" "legacy_app"
    (R.legacy_path_to_string r.legacy)

let test_lookup_aliases_and_unknown () =
  let r = R.lookup ~action:"  submit_review " in
  Alcotest.(check string) "alias" "review_submit" r.action;
  let u = R.lookup ~action:"totally_unknown_xyz" in
  Alcotest.(check string)
    "unknown target" "user_required"
    (Policy.attribution_to_string u.target);
  Alcotest.(check bool) "unknown no pilot" false u.pilot_allowed;
  Alcotest.(check bool) "unknown prod gate" true u.production_requires_user_gate

let test_matrix_json_no_secrets () =
  let j = R.matrix_to_json (R.matrix ()) in
  let s = Yojson.Safe.to_string j in
  Alcotest.(check bool)
    "has matrix_version" true
    (Test_helpers.string_contains s "matrix_version");
  Alcotest.(check bool)
    "no token-looking fields" false
    (Test_helpers.string_contains (String.lowercase_ascii s) "\"access_token\"");
  Alcotest.(check bool)
    "no refresh" false
    (Test_helpers.string_contains (String.lowercase_ascii s) "refresh_token")

(* -------------------------------------------------------------------------- *)
(* Defaults / stages                                                            *)
(* -------------------------------------------------------------------------- *)

let test_safe_defaults () =
  Alcotest.(check bool)
    "user_required disabled by default" true
    (R.user_required_disabled_by_default ());
  Alcotest.(check string)
    "default stage" "safe_default"
    (R.stage_to_string R.default_stage);
  Alcotest.(check bool) "prod off" false R.default_production_gate.enabled;
  List.iter
    (fun (g : R.pilot_gate) ->
      Alcotest.(check bool) (g.pilot_name ^ " off") false g.enabled)
    (R.default_pilot_gates ())

let test_stage_roundtrip () =
  List.iter
    (fun st ->
      match R.stage_of_string (R.stage_to_string st) with
      | Ok st' ->
          Alcotest.(check string)
            "roundtrip" (R.stage_to_string st) (R.stage_to_string st')
      | Error e -> Alcotest.fail e)
    (R.stages ())

(* -------------------------------------------------------------------------- *)
(* Pilot gate activity                                                          *)
(* -------------------------------------------------------------------------- *)

let test_pilot_gate_requires_expiry () =
  let open_ended =
    {
      R.enabled = true;
      pilot_name = "p19-merge-pilot";
      expires_at = None;
      audit_ref = Some "aud-1";
    }
  in
  Alcotest.(check bool)
    "open-ended inactive" false
    (R.pilot_gate_active ~now:fixed_now open_ended);
  let active =
    {
      R.enabled = true;
      pilot_name = "p19-merge-pilot";
      expires_at = Some future_exp;
      audit_ref = Some "aud-1";
    }
  in
  Alcotest.(check bool)
    "future expiry active" true
    (R.pilot_gate_active ~now:fixed_now active);
  let expired = { active with expires_at = Some past_exp } in
  Alcotest.(check bool)
    "past expiry inactive" false
    (R.pilot_gate_active ~now:fixed_now expired)

(* -------------------------------------------------------------------------- *)
(* Resolve                                                                      *)
(* -------------------------------------------------------------------------- *)

let test_resolve_reads_always_app () =
  List.iter
    (fun action ->
      let p = R.resolve (R.default_resolve_input ~action ~now:fixed_now ()) in
      expect_path R.Path_app_primary p)
    [ "read"; "search"; "get_status"; "get_item" ]

let test_resolve_safe_default_denies_user_required () =
  let p =
    R.resolve (R.default_resolve_input ~action:"merge" ~now:fixed_now ())
  in
  ignore (expect_denied ~code:"user_required_gate_disabled" p)

let test_resolve_pilot_app_when_gate_active () =
  let pilot =
    {
      R.enabled = true;
      pilot_name = "p19-merge-pilot";
      expires_at = Some future_exp;
      audit_ref = Some "aud-pilot";
    }
  in
  let p =
    R.resolve
      (R.default_resolve_input ~action:"merge" ~stage:R.P19_pilot
         ~pilot_gates:[ pilot ] ~now:fixed_now ())
  in
  expect_path R.Path_pilot_app p

let test_resolve_pilot_off_no_silent_app () =
  let pilot =
    {
      R.enabled = false;
      pilot_name = "p19-merge-pilot";
      expires_at = Some future_exp;
      audit_ref = None;
    }
  in
  let p =
    R.resolve
      (R.default_resolve_input ~action:"merge" ~stage:R.P19_pilot
         ~pilot_gates:[ pilot ] ~now:fixed_now ~user_auth_available:false ())
  in
  let msg = expect_denied ~code:"user_required_gate_disabled" p in
  Alcotest.(check bool)
    "mentions no App/PAT fallback" true
    (Test_helpers.string_contains msg "no App/PAT fallback"
    || Test_helpers.string_contains msg "App/PAT")

let test_resolve_production_user_path () =
  let production =
    {
      R.enabled = true;
      audit_ref = Some "aud-prod";
      enabled_at = Some "2026-07-13T00:00:00Z";
    }
  in
  let p =
    R.resolve
      (R.default_resolve_input ~action:"merge" ~stage:R.P21_production
         ~production ~readiness:R.all_ready ~now:fixed_now
         ~user_auth_available:true ())
  in
  expect_path R.Path_user p

let test_resolve_production_incomplete_readiness () =
  let production =
    { R.enabled = true; audit_ref = Some "aud-prod"; enabled_at = None }
  in
  let readiness = { R.empty_readiness with principal_ready = true } in
  let p =
    R.resolve
      (R.default_resolve_input ~action:"merge" ~stage:R.P21_production
         ~production ~readiness ~now:fixed_now ~user_auth_available:true ())
  in
  ignore (expect_denied ~code:"user_required_gate_disabled" p)

let test_resolve_user_preferred_needs_gate () =
  let denied =
    R.resolve (R.default_resolve_input ~action:"comment" ~now:fixed_now ())
  in
  ignore (expect_denied ~code:"attribution_gate_disabled" denied);
  let production =
    { R.enabled = true; audit_ref = Some "a"; enabled_at = None }
  in
  let ok =
    R.resolve
      (R.default_resolve_input ~action:"comment" ~stage:R.P21_production
         ~production ~readiness:R.all_ready ~now:fixed_now
         ~user_auth_available:true ())
  in
  expect_path R.Path_user ok

let test_resolve_rollback_denies_without_substitution () =
  let production =
    { R.enabled = true; audit_ref = Some "a"; enabled_at = None }
  in
  let p =
    R.resolve
      (R.default_resolve_input ~action:"merge" ~stage:R.Rollback ~production
         ~readiness:R.all_ready ~now:fixed_now ())
  in
  ignore (expect_denied ~code:"rollout_rollback_active" p)

let test_resolve_cleanup_denies () =
  let p =
    R.resolve
      (R.default_resolve_input ~action:"comment" ~stage:R.Cleanup ~now:fixed_now
         ())
  in
  ignore (expect_denied ~code:"rollout_cleanup_active" p)

(* -------------------------------------------------------------------------- *)
(* Transitions                                                                  *)
(* -------------------------------------------------------------------------- *)

let test_transition_pilot_enable () =
  let pilot =
    {
      R.enabled = true;
      pilot_name = "p19-merge-pilot";
      expires_at = Some future_exp;
      audit_ref = Some "aud-1";
    }
  in
  let r =
    expect_transition_ok
      {
        R.kind = R.Gate_pilot_enable;
        from_stage = R.Safe_default;
        pilot = Some pilot;
        production = None;
        rollback = None;
        cleanup = None;
        readiness = R.empty_readiness;
        audit_ref = Some "aud-1";
      }
  in
  Alcotest.(check string) "to pilot" "p19_pilot" (R.stage_to_string r.to_stage);
  Alcotest.(check bool) "prod still off" false r.production.enabled

let test_transition_pilot_enable_requires_expiry () =
  let pilot =
    {
      R.enabled = true;
      pilot_name = "p19-merge-pilot";
      expires_at = None;
      audit_ref = Some "aud-1";
    }
  in
  expect_transition_err ~substr:"expires_at"
    {
      R.kind = R.Gate_pilot_enable;
      from_stage = R.Safe_default;
      pilot = Some pilot;
      production = None;
      rollback = None;
      cleanup = None;
      readiness = R.empty_readiness;
      audit_ref = Some "aud-1";
    }

let test_transition_production_enable_needs_readiness () =
  let production =
    { R.enabled = true; audit_ref = Some "aud-p"; enabled_at = None }
  in
  expect_transition_err ~substr:"readiness"
    {
      R.kind = R.Gate_production_enable;
      from_stage = R.Safe_default;
      pilot = None;
      production = Some production;
      rollback = None;
      cleanup = None;
      readiness = R.empty_readiness;
      audit_ref = Some "aud-p";
    };
  let r =
    expect_transition_ok
      {
        R.kind = R.Gate_production_enable;
        from_stage = R.Safe_default;
        pilot = None;
        production = Some production;
        rollback = None;
        cleanup = None;
        readiness = R.all_ready;
        audit_ref = Some "aud-p";
      }
  in
  Alcotest.(check string)
    "to production" "p21_production"
    (R.stage_to_string r.to_stage)

let test_transition_production_blocked_during_rollback () =
  let production =
    { R.enabled = true; audit_ref = Some "aud-p"; enabled_at = None }
  in
  expect_transition_err ~substr:"rollback"
    {
      R.kind = R.Gate_production_enable;
      from_stage = R.Rollback;
      pilot = None;
      production = Some production;
      rollback = None;
      cleanup = None;
      readiness = R.all_ready;
      audit_ref = Some "aud-p";
    }

let test_transition_rollback_restores_safe () =
  let rb =
    {
      R.active = true;
      reason = "incident: revoke blast radius";
      audit_ref = Some "aud-rb";
      restores_stage = R.Safe_default;
    }
  in
  let r =
    expect_transition_ok
      {
        R.kind = R.Gate_rollback;
        from_stage = R.P21_production;
        pilot = None;
        production =
          Some
            {
              enabled = true;
              audit_ref = Some "old";
              enabled_at = Some "2026-07-01T00:00:00Z";
            };
        rollback = Some rb;
        cleanup = None;
        readiness = R.all_ready;
        audit_ref = Some "aud-rb";
      }
  in
  Alcotest.(check string)
    "to rollback" "rollback"
    (R.stage_to_string r.to_stage);
  Alcotest.(check bool) "prod off" false r.production.enabled

let test_transition_cleanup_complete () =
  let c =
    {
      R.active = true;
      audit_ref = Some "aud-c";
      residual_authority_cleared = true;
      pilot_credentials_destroyed = true;
      bindings_unlinked = true;
    }
  in
  let r =
    expect_transition_ok
      {
        R.kind = R.Gate_cleanup;
        from_stage = R.Rollback;
        pilot = None;
        production = None;
        rollback = None;
        cleanup = Some c;
        readiness = R.empty_readiness;
        audit_ref = Some "aud-c";
      }
  in
  Alcotest.(check string)
    "to safe" "safe_default"
    (R.stage_to_string r.to_stage);
  Alcotest.(check bool) "cleanup_complete" true (R.cleanup_complete c)

let test_transition_cleanup_incomplete_rejected () =
  let c =
    {
      R.active = true;
      audit_ref = Some "aud-c";
      residual_authority_cleared = false;
      pilot_credentials_destroyed = true;
      bindings_unlinked = true;
    }
  in
  expect_transition_err ~substr:"residual_authority"
    {
      R.kind = R.Gate_cleanup;
      from_stage = R.Rollback;
      pilot = None;
      production = None;
      rollback = None;
      cleanup = Some c;
      readiness = R.empty_readiness;
      audit_ref = Some "aud-c";
    }

let test_no_residual_authority () =
  let pilots = R.default_pilot_gates () in
  let cleanup =
    {
      R.active = true;
      audit_ref = Some "a";
      residual_authority_cleared = true;
      pilot_credentials_destroyed = true;
      bindings_unlinked = true;
    }
  in
  Alcotest.(check bool)
    "clean" true
    (R.no_residual_authority ~production:R.default_production_gate
       ~pilot_gates:pilots ~now:fixed_now ~cleanup);
  let dirty_prod = { R.default_production_gate with enabled = true } in
  Alcotest.(check bool)
    "prod residual" false
    (R.no_residual_authority ~production:dirty_prod ~pilot_gates:pilots
       ~now:fixed_now ~cleanup)

let test_weakening_heuristic () =
  Alcotest.(check bool)
    "cleanup to prod weakens" true
    (R.transition_weakens_confirmation ~from_stage:R.Cleanup
       ~to_stage:R.P21_production);
  Alcotest.(check bool)
    "rollback to pilot weakens" true
    (R.transition_weakens_confirmation ~from_stage:R.Rollback
       ~to_stage:R.P19_pilot)

let test_readiness_missing_list () =
  let r = { R.empty_readiness with vault_ready = true } in
  let missing = R.readiness_missing r in
  Alcotest.(check bool)
    "lists principal" true
    (List.mem "principal_ready" missing);
  Alcotest.(check bool) "omits vault" false (List.mem "vault_ready" missing);
  Alcotest.(check bool) "not complete" false (R.readiness_complete r)

let suite =
  [
    Alcotest.test_case "matrix version" `Quick test_matrix_version;
    Alcotest.test_case "matrix covers reads mutations background" `Quick
      test_matrix_covers_reads_mutations_background;
    Alcotest.test_case "matrix covers policy defaults" `Quick
      test_matrix_covers_policy_defaults;
    Alcotest.test_case "matrix User_required semantics" `Quick
      test_matrix_row_semantics_user_required;
    Alcotest.test_case "matrix User_preferred semantics" `Quick
      test_matrix_row_semantics_user_preferred;
    Alcotest.test_case "matrix read semantics" `Quick
      test_matrix_row_semantics_read;
    Alcotest.test_case "lookup aliases and unknown fail closed" `Quick
      test_lookup_aliases_and_unknown;
    Alcotest.test_case "matrix json has no secrets" `Quick
      test_matrix_json_no_secrets;
    Alcotest.test_case "safe defaults" `Quick test_safe_defaults;
    Alcotest.test_case "stage roundtrip" `Quick test_stage_roundtrip;
    Alcotest.test_case "pilot gate requires expiry" `Quick
      test_pilot_gate_requires_expiry;
    Alcotest.test_case "resolve reads always App" `Quick
      test_resolve_reads_always_app;
    Alcotest.test_case "resolve safe default denies User_required" `Quick
      test_resolve_safe_default_denies_user_required;
    Alcotest.test_case "resolve pilot App when gate active" `Quick
      test_resolve_pilot_app_when_gate_active;
    Alcotest.test_case "resolve pilot off no silent App" `Quick
      test_resolve_pilot_off_no_silent_app;
    Alcotest.test_case "resolve production user path" `Quick
      test_resolve_production_user_path;
    Alcotest.test_case "resolve production incomplete readiness" `Quick
      test_resolve_production_incomplete_readiness;
    Alcotest.test_case "resolve User_preferred needs gate" `Quick
      test_resolve_user_preferred_needs_gate;
    Alcotest.test_case "resolve rollback denies without substitution" `Quick
      test_resolve_rollback_denies_without_substitution;
    Alcotest.test_case "resolve cleanup denies" `Quick
      test_resolve_cleanup_denies;
    Alcotest.test_case "transition pilot enable" `Quick
      test_transition_pilot_enable;
    Alcotest.test_case "transition pilot enable requires expiry" `Quick
      test_transition_pilot_enable_requires_expiry;
    Alcotest.test_case "transition production enable needs readiness" `Quick
      test_transition_production_enable_needs_readiness;
    Alcotest.test_case "transition production blocked during rollback" `Quick
      test_transition_production_blocked_during_rollback;
    Alcotest.test_case "transition rollback restores safe" `Quick
      test_transition_rollback_restores_safe;
    Alcotest.test_case "transition cleanup complete" `Quick
      test_transition_cleanup_complete;
    Alcotest.test_case "transition cleanup incomplete rejected" `Quick
      test_transition_cleanup_incomplete_rejected;
    Alcotest.test_case "no residual authority" `Quick test_no_residual_authority;
    Alcotest.test_case "weakening heuristic" `Quick test_weakening_heuristic;
    Alcotest.test_case "readiness missing list" `Quick
      test_readiness_missing_list;
  ]
