(** Tests for route/App readiness, match explain, audit correlation, and secret
    redaction (P19.M2.E3.T004). *)

module Ops = Github_route_ops
module S = Github_route_store
module M = Github_route_match
module Auth = Github_auth_selection
module Inst = Github_app_installation_scope

let fixed_now = 1_700_000_000.0
let contains hay needle = Test_helpers.string_contains hay needle

let find_check name (report : Ops.readiness_report) =
  List.find_opt (fun (c : Ops.check) -> c.name = name) report.checks

let require_check name report =
  match find_check name report with
  | Some c -> c
  | None -> Alcotest.fail (Printf.sprintf "missing check %s" name)

let status_str = Ops.check_status_to_string

let sample_route ?(id = "rt-1") ?(selector = S.Repo "acme/widget")
    ?(enabled = true) ?(setup_plan_id = Some "plan-1") ?(revision = "1") () :
    S.t =
  {
    id;
    destination = S.Room "room-teams-1";
    selector;
    filter = S.default_filter;
    comment_mode = S.default_comment_mode;
    capability_policy = S.default_capability_policy;
    enabled;
    revision;
    managed_bundle_id = None;
    managed_feature_id = None;
    provenance =
      {
        created_by = Some "alice";
        created_via = Some "setup_plan";
        setup_plan_id;
        notes = None;
      };
    created_at = "2024-01-01T00:00:00Z";
    updated_at = "2024-01-01T00:00:00Z";
  }

let sample_scope ?(installation_id = 1001) ?(login = "acme")
    ?(status = Inst.Active) () : Inst.t =
  Inst.with_revision
    {
      installation_id;
      app_id = Some 42;
      account = { login; id = 99; account_type = "Organization" };
      selection = Inst.All_repos;
      repositories = [];
      revoked_repositories = [];
      permissions = [ ("issues", "write") ];
      status;
      revision = "";
      updated_at = Time_util.iso8601_utc ~t:fixed_now ();
    }

let sample_app () : Runtime_config.github_app_config =
  {
    app_id = 42;
    private_key_path = "/tmp/github-app.pem";
    webhook_secret = "whsec";
    installations = [ { installation_id = 1001; repos = [ "acme/widget" ] } ];
  }

(* 1. Missing installation → Fail with repair mentioning App install *)
let test_missing_installation () =
  let report = Ops.assess_readiness ~route:(sample_route ()) () in
  let c = require_check "app_scope" report in
  Alcotest.(check string) "status" "fail" (status_str c.status);
  Alcotest.(check bool)
    "message mentions missing" true
    (contains (String.lowercase_ascii c.message) "missing"
    || contains (String.lowercase_ascii c.message) "install");
  (match c.repair with
  | None -> Alcotest.fail "expected repair step"
  | Some r ->
      let rl = String.lowercase_ascii r in
      Alcotest.(check bool)
        "repair mentions App install" true
        (contains rl "app" && contains rl "install"));
  Alcotest.(check string) "overall fail" "fail" (status_str report.overall);
  Alcotest.(check (option string)) "route id" (Some "rt-1") report.route_id;
  Alcotest.(check (option string))
    "setup plan correlated" (Some "plan-1") report.setup_plan_id;
  Alcotest.(check (option int)) "no installation id" None report.installation_id

(* 2. PAT claiming org → Fail with migration guidance *)
let test_pat_claiming_org () =
  let route = sample_route ~id:"rt-org" ~selector:(S.Org "acme") () in
  let auth = Auth.snapshot_of_parts ~pat:"ghp_only_pat_token" () in
  let report = Ops.assess_readiness ~route ~auth () in
  let c = require_check "org_auth" report in
  Alcotest.(check string) "status" "fail" (status_str c.status);
  let ml = String.lowercase_ascii c.message in
  Alcotest.(check bool) "mentions org" true (contains ml "org");
  Alcotest.(check bool) "mentions pat" true (contains ml "pat");
  (match c.repair with
  | None -> Alcotest.fail "expected migration repair"
  | Some r ->
      let rl = String.lowercase_ascii r in
      Alcotest.(check bool)
        "repair migration to App" true
        (contains rl "migrat" || contains rl "app");
      Alcotest.(check bool) "mentions app" true (contains rl "app"));
  Alcotest.(check string) "overall fail" "fail" (status_str report.overall)

(* 2b. App + active installation may claim org *)
let test_app_org_ok () =
  let route = sample_route ~id:"rt-org" ~selector:(S.Org "acme") () in
  let auth = Auth.snapshot_of_parts ~app:(sample_app ()) () in
  let installation = sample_scope ~login:"acme" () in
  let report =
    Ops.assess_readiness ~route ~auth ~installation ~base_revision:"rev-1"
      ~current_revision:"rev-1" ()
  in
  let org = require_check "org_auth" report in
  let scope = require_check "app_scope" report in
  Alcotest.(check string) "org pass" "pass" (status_str org.status);
  Alcotest.(check string) "scope pass" "pass" (status_str scope.status);
  Alcotest.(check string) "overall pass" "pass" (status_str report.overall);
  Alcotest.(check (option int))
    "installation id" (Some 1001) report.installation_id

(* 3. Stale revision → Fail with re-plan repair *)
let test_stale_revision () =
  let installation = sample_scope () in
  let report =
    Ops.assess_readiness ~installation ~base_revision:"rev-old"
      ~current_revision:"rev-new" ()
  in
  let c = require_check "revision" report in
  Alcotest.(check string) "status fail" "fail" (status_str c.status);
  Alcotest.(check bool)
    "mentions stale" true
    (contains (String.lowercase_ascii c.message) "stale"
    || contains (String.lowercase_ascii c.message) "rev-old");
  (match c.repair with
  | None -> Alcotest.fail "expected re-plan repair"
  | Some r ->
      let rl = String.lowercase_ascii r in
      Alcotest.(check bool)
        "repair re-plan" true
        (contains rl "regenerat" || contains rl "re-plan" || contains rl "plan"));
  Alcotest.(check string) "overall fail" "fail" (status_str report.overall)

(* 4. tools/mcp/credentials/egress/connector/delivery flags surface checks *)
let test_flags_surface () =
  let installation = sample_scope () in
  let report =
    Ops.assess_readiness ~installation ~tools_granted:false ~mcp_ok:false
      ~credentials_ok:false ~egress_ok:false ~connector_ok:false
      ~delivery_ok:false ()
  in
  let names =
    [
      "grants"; "tools"; "mcp"; "credentials"; "egress"; "connector"; "delivery";
    ]
  in
  List.iter
    (fun name ->
      let c = require_check name report in
      Alcotest.(check string) (name ^ " fails") "fail" (status_str c.status);
      Alcotest.(check bool)
        (name ^ " has repair") true (Option.is_some c.repair))
    names;
  Alcotest.(check string) "overall fail" "fail" (status_str report.overall)

(* 4b. All flags ok → Pass *)
let test_flags_ok () =
  let installation = sample_scope () in
  let report =
    Ops.assess_readiness ~installation ~tools_granted:true ~mcp_ok:true
      ~credentials_ok:true ~egress_ok:true ~connector_ok:true ~delivery_ok:true
      ~base_revision:"r1" ~current_revision:"r1" ()
  in
  Alcotest.(check string) "overall pass" "pass" (status_str report.overall);
  List.iter
    (fun name ->
      let c = require_check name report in
      Alcotest.(check string) (name ^ " pass") "pass" (status_str c.status))
    [
      "app_scope";
      "grants";
      "tools";
      "mcp";
      "credentials";
      "egress";
      "connector";
      "delivery";
      "revision";
    ]

(* 5. explain_match for Matched / Muted / No_route *)
let test_explain_matched () =
  let route =
    sample_route ~id:"rt-win"
      ~selector:
        (S.Item
           { repo_full_name = "acme/widget"; kind = `Pull_request; number = 42 })
      ()
  in
  let shadowed_route =
    sample_route ~id:"rt-shadow" ~selector:(S.Org "acme") ()
  in
  let decision = M.Matched { route; specificity = `Item } in
  let rep = Ops.explain_match ~decision ~shadowed:[ shadowed_route ] () in
  Alcotest.(check bool)
    "summary mentions matched" true
    (contains (String.lowercase_ascii rep.decision_summary) "match");
  Alcotest.(check (option string)) "winner" (Some "rt-win") rep.winner_route_id;
  Alcotest.(check (list string)) "shadowed" [ "rt-shadow" ] rep.shadowed;
  Alcotest.(check bool) "has predicates" true (List.length rep.predicates > 0);
  Alcotest.(check bool)
    "final reason non-empty" true
    (String.length rep.final_reason > 0)

let test_explain_muted () =
  let route =
    sample_route ~id:"rt-mute" ~enabled:false ~selector:(S.Repo "acme/widget")
      ()
  in
  let decision =
    M.Muted { route; specificity = `Repo; reason = "route disabled" }
  in
  let rep = Ops.explain_match ~decision () in
  Alcotest.(check bool)
    "summary mute" true
    (contains (String.lowercase_ascii rep.decision_summary) "mute");
  Alcotest.(check (option string)) "winner" (Some "rt-mute") rep.winner_route_id;
  Alcotest.(check string) "final reason" "route disabled" rep.final_reason;
  Alcotest.(check (list string)) "no shadowed" [] rep.shadowed

let test_explain_no_route () =
  let rep = Ops.explain_match ~decision:M.No_route () in
  Alcotest.(check bool)
    "summary no route" true
    (contains (String.lowercase_ascii rep.decision_summary) "no route");
  Alcotest.(check (option string)) "no winner" None rep.winner_route_id;
  Alcotest.(check bool)
    "final reason mentions none" true
    (contains (String.lowercase_ascii rep.final_reason) "no")

(* 6. redact_json removes secrets and truncates long strings *)
let test_redact_json () =
  let long = String.make 500 'x' in
  let pem =
    "-----BEGIN RSA PRIVATE KEY-----\n\
     MIIEowIBAAKCAQEA...\n\
     -----END RSA PRIVATE KEY-----"
  in
  let json =
    `Assoc
      [
        ("private_key", `String pem);
        ("client_secret", `String "cs_super_secret");
        ("webhook_secret", `String "whsec_abc");
        ("token", `String "ghp_abcdefghijklmnopqrstuvwxyz");
        ("authorization", `String "Bearer ghp_abcdefghijklmnopqrstuvwxyz");
        ("pem", `String pem);
        ("safe_note", `String "hello");
        ("payload", `String long);
        ( "nested",
          `Assoc
            [
              ("access_token", `String "tok_nested"); ("ok", `String "visible");
            ] );
        ("list", `List [ `String "Bearer abcdefghijklmnopqrstuvwxyz012345" ]);
      ]
  in
  let red = Ops.redact_json json in
  let s = Yojson.Safe.to_string red in
  Alcotest.(check bool)
    "no private key body" false
    (contains s "BEGIN RSA PRIVATE KEY");
  Alcotest.(check bool) "no client secret" false (contains s "cs_super_secret");
  Alcotest.(check bool) "no webhook secret" false (contains s "whsec_abc");
  Alcotest.(check bool)
    "no ghp token" false
    (contains s "ghp_abcdefghijklmnopqrstuvwxyz");
  Alcotest.(check bool) "no bearer raw" false (contains s "Bearer ghp_");
  Alcotest.(check bool) "safe note kept" true (contains s "hello");
  Alcotest.(check bool) "nested ok kept" true (contains s "visible");
  Alcotest.(check bool)
    "redacted marker present" true
    (contains s "REDACTED" || contains s "***");
  (* Long non-secret strings are bounded. *)
  match red with
  | `Assoc fields -> (
      match List.assoc_opt "payload" fields with
      | Some (`String p) ->
          Alcotest.(check bool)
            "payload truncated" true
            (String.length p < 500 && contains p "more bytes")
      | _ -> Alcotest.fail "payload missing or wrong type")
  | _ -> Alcotest.fail "expected object"

(* 7. audit_event details never contain pem/secret tokens *)
let test_audit_event_redacts () =
  let pem =
    "-----BEGIN PRIVATE KEY-----\nSECRETPEMDATA\n-----END PRIVATE KEY-----"
  in
  let details =
    `Assoc
      [
        ("private_key", `String pem);
        ("webhook_secret", `String "supersecret");
        ("token", `String "ghp_should_not_leak");
        ("note", `String "route applied");
      ]
  in
  let rec_ =
    Ops.audit_event ~setup_plan_id:"plan-9" ~route_id:"rt-9"
      ~installation_id:1001 ~action:"route.apply" ~details ~now:fixed_now ()
  in
  Alcotest.(check string) "action" "route.apply" rec_.action;
  Alcotest.(check (option string)) "setup" (Some "plan-9") rec_.setup_plan_id;
  Alcotest.(check (option string)) "route" (Some "rt-9") rec_.route_id;
  Alcotest.(check (option int)) "installation" (Some 1001) rec_.installation_id;
  Alcotest.(check bool)
    "timestamp non-empty" true
    (String.length rec_.timestamp > 0);
  let dumped = Yojson.Safe.to_string rec_.details in
  Alcotest.(check bool) "no pem" false (contains dumped "SECRETPEMDATA");
  Alcotest.(check bool) "no BEGIN" false (contains dumped "BEGIN PRIVATE");
  Alcotest.(check bool) "no secret" false (contains dumped "supersecret");
  Alcotest.(check bool) "no ghp" false (contains dumped "ghp_should_not_leak");
  Alcotest.(check bool) "note kept" true (contains dumped "route applied")

(* 8. overall Fail if any Fail; Warn if any Warn else Pass *)
let test_overall_aggregation () =
  (* Force a Warn-only path: missing current revision with base present + good
     installation defaults. *)
  let installation = sample_scope () in
  let warn_report =
    Ops.assess_readiness ~installation ~base_revision:"rev-only" ()
  in
  let rev = require_check "revision" warn_report in
  Alcotest.(check string) "revision warn" "warn" (status_str rev.status);
  Alcotest.(check string) "overall warn" "warn" (status_str warn_report.overall);
  let pass_report =
    Ops.assess_readiness ~installation ~base_revision:"r" ~current_revision:"r"
      ()
  in
  Alcotest.(check string) "overall pass" "pass" (status_str pass_report.overall);
  let fail_report = Ops.assess_readiness () in
  Alcotest.(check string) "overall fail" "fail" (status_str fail_report.overall)

let suite =
  [
    ( "missing installation fails with App install repair",
      `Quick,
      test_missing_installation );
    ( "PAT claiming org fails with migration guidance",
      `Quick,
      test_pat_claiming_org );
    ("App org with installation passes", `Quick, test_app_org_ok);
    ("stale revision fails with re-plan repair", `Quick, test_stale_revision);
    ( "tools/mcp/credentials/egress/connector/delivery flags",
      `Quick,
      test_flags_surface );
    ("all flags ok overall pass", `Quick, test_flags_ok);
    ("explain_match Matched", `Quick, test_explain_matched);
    ("explain_match Muted", `Quick, test_explain_muted);
    ("explain_match No_route", `Quick, test_explain_no_route);
    ("redact_json secrets and long strings", `Quick, test_redact_json);
    ("audit_event redacts pem and secrets", `Quick, test_audit_event_redacts);
    ("overall Fail/Warn/Pass aggregation", `Quick, test_overall_aggregation);
  ]
