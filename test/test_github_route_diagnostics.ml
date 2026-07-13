(** Tests for route/filter setup diagnostics and redacted export
    (P20.M2.E2.T001). *)

module S = Github_route_store
module F = Github_route_filter
module D = Github_route_diagnostics
module Inst = Github_app_installation_scope
module E = Github_event_envelope
module En = Github_filter_enrichment
module Admin = Github_route_admin

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  S.ensure_schema db;
  Setup_plan_apply.init_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_700_000_000.0
let room = S.Room "room-teams-1"
let base_revision = "rev-config-1"
let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e
let contains hay needle = Test_helpers.string_contains hay needle

let principal =
  Setup_plan.{ id = "principal:alice"; kind = Principal; label = Some "Alice" }

let advanced_filter () =
  assert_ok
    (F.validate
       {
         F.default with
         include_events = [ "pull_request" ];
         exclude_events = [ "issue_comment" ];
         include_repos = [];
         exclude_repos = [ "acme/other" ];
         pr =
           {
             F.empty_pr with
             labels = Some { op = `In; values = [ "bug" ] };
             author = Some { op = `Eq; values = [ "alice" ] };
             changed_path = Some { op = `Glob; values = [ "src/**" ] };
             draft = Some false;
           };
         issue =
           {
             F.empty_issue with
             milestone = Some { op = `Eq; values = [ "v1" ] };
           };
       })

let create_route ~db ?(id = "rt-1") ?(selector = S.Repo "acme/widget")
    ?(filter = S.default_filter) ?(enabled = true) ?managed_bundle_id
    ?managed_feature_id ?setup_plan_id () =
  let provenance =
    {
      S.created_by = Some "alice";
      created_via = Some "setup_plan";
      setup_plan_id;
      notes = None;
    }
  in
  assert_ok
    (S.create ~db ~id ~destination:room ~selector ~filter ~enabled ~provenance
       ~now:fixed_now ?managed_bundle_id ?managed_feature_id ())

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

let make_envelope ?(event = "pull_request") ?(action = Some "opened")
    ?(repo = "acme/widget") () : E.t =
  {
    version = E.envelope_version;
    delivery_id = Some "deliv-1";
    installation_id = Some 99;
    event;
    action;
    repo_full_name = repo;
    org = Some "acme";
    item_kind = Some E.Pull_request;
    item_number = Some 42;
    item_node_id = None;
    item_url = None;
    html_url = None;
    family = E.Lifecycle;
    actor = { E.empty_actor with login = Some "alice" };
    item_author = Some "alice";
    before = None;
    after =
      Some
        {
          E.empty_safe_state with
          labels = [ "bug" ];
          draft = Some false;
          base_ref = Some "main";
          state = Some "open";
        };
    transfer = None;
    received_at = Some "2024-01-01T00:00:00Z";
    event_at = None;
    head_sha = Some "abc123";
    unsupported = false;
    skip_reason = None;
  }

let json_keys = function
  | `Assoc fields -> List.map fst fields |> List.sort String.compare
  | _ -> []

let member_opt key = function
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

(* 1. Predicate counts for advanced filter *)
let test_predicate_counts () =
  let f = advanced_filter () in
  let c = D.count_predicates f in
  Alcotest.(check int) "include_events" 1 c.include_events;
  Alcotest.(check int) "exclude_events" 1 c.exclude_events;
  Alcotest.(check int) "exclude_repos" 1 c.exclude_repos;
  Alcotest.(check int) "pr predicates" 4 c.pr_predicates;
  Alcotest.(check int) "issue predicates" 1 c.issue_predicates;
  Alcotest.(check int) "advanced_total" 5 c.advanced_total;
  Alcotest.(check int) "baseline_total" 3 c.baseline_total

(* 2. Export shape: routes list, schema version, stable keys *)
let test_export_shape () =
  with_db @@ fun db ->
  let filter = advanced_filter () in
  ignore
    (create_route ~db ~id:"rt_adv" ~filter ~managed_bundle_id:"bundle-1"
       ~managed_feature_id:"feat-gh" ~setup_plan_id:"plan-9" ());
  ignore
    (create_route ~db ~id:"rt_base" ~selector:(S.Org "acme") ~enabled:false ());
  let plan =
    assert_ok
      (Admin.plan_create ~db ~principal ~destination:room
         ~selector:(S.Repo "acme/other") ~base_revision ~now:fixed_now
         ~route_id:"rt_plan_only" ())
  in
  let installation = sample_scope () in
  let exp =
    assert_ok
      (D.collect ~db ~destination:room ~installation ~plan
         ~catalog_revision:"cat-rev-abc" ~catalog_access_revision:"access-1"
         ~now:fixed_now ())
  in
  Alcotest.(check (option string))
    "destination" (Some "room:room-teams-1") exp.destination;
  Alcotest.(check int) "current schema" 1 exp.current_filter_schema_version;
  Alcotest.(check int) "route count" 2 exp.route_count;
  Alcotest.(check int) "enabled count" 1 exp.enabled_count;
  Alcotest.(check (option string)) "plan id" (Some plan.id) exp.plan_id;
  Alcotest.(check (option string))
    "plan base rev" (Some base_revision) exp.plan_base_revision;
  Alcotest.(check (option string))
    "catalog rev" (Some "cat-rev-abc") exp.catalog_revision;
  Alcotest.(check (option string))
    "catalog access" (Some "access-1") exp.catalog_access_revision;
  Alcotest.(check string) "app status" "active" exp.app_scope.status;
  Alcotest.(check (option int))
    "installation" (Some 1001) exp.app_scope.installation_id;
  Alcotest.(check bool) "delivery present" true (Option.is_some exp.delivery);
  Alcotest.(check bool)
    "diagnostics non-empty" true
    (List.length exp.diagnostics > 0);
  let adv =
    List.find (fun (r : D.route_export) -> r.id = "rt_adv") exp.routes
  in
  Alcotest.(check int) "filter schema on route" 1 adv.filter_schema_version;
  Alcotest.(check int) "advanced preds" 5 adv.predicate_counts.advanced_total;
  Alcotest.(check bool) "has advanced" true adv.has_advanced;
  Alcotest.(check bool) "requires paths" true adv.requires_changed_paths;
  Alcotest.(check (option string))
    "managed bundle" (Some "bundle-1") adv.managed_bundle_id;
  Alcotest.(check (option string))
    "setup plan on route" (Some "plan-9") adv.setup_plan_id;
  let j = D.to_json exp in
  let keys = json_keys j in
  List.iter
    (fun k ->
      Alcotest.(check bool)
        (Printf.sprintf "export has %s" k)
        true (List.mem k keys))
    [
      "app_scope";
      "current_filter_schema_version";
      "diagnostics";
      "enabled_count";
      "exported_at";
      "route_count";
      "routes";
      "plan_id";
      "plan_base_revision";
      "catalog_revision";
      "delivery";
      "repair_hints";
    ];
  match member_opt "routes" j with
  | Some (`List (_ :: _ as routes_j)) -> (
      match List.hd routes_j with
      | `Assoc fields as route_j -> (
          let rkeys = List.map fst fields |> List.sort String.compare in
          List.iter
            (fun k ->
              Alcotest.(check bool)
                (Printf.sprintf "route has %s" k)
                true (List.mem k rkeys))
            [
              "id";
              "revision";
              "selector_key";
              "filter_schema_version";
              "predicate_counts";
              "enabled";
            ];
          match member_opt "predicate_counts" route_j with
          | Some (`Assoc pc) ->
              let pkeys = List.map fst pc in
              Alcotest.(check bool)
                "advanced_total key" true
                (List.mem "advanced_total" pkeys)
          | _ -> Alcotest.fail "predicate_counts missing")
      | _ -> Alcotest.fail "route not object")
  | _ -> Alcotest.fail "routes missing or empty"

(* 3. Redaction: secrets never appear in export JSON or diagnostics lines *)
let test_redaction_no_secrets () =
  with_db @@ fun db ->
  (* Store a route with advanced filter (no secrets in store). Poison the
     export path by attaching a plan whose planned_state would be secret-bearing
     if we re-exported raw plan JSON — diagnostics must only expose plan id /
     base_revision / digest, never PEM/webhook material. *)
  ignore (create_route ~db ~id:"rt_sec" ());
  let plan =
    assert_ok
      (Admin.plan_create ~db ~principal ~destination:room
         ~selector:(S.Repo "acme/widget") ~base_revision ~now:fixed_now ())
  in
  let pem =
    "-----BEGIN PRIVATE KEY-----\n\
     SECRETPEMDATA_should_not_leak\n\
     -----END PRIVATE KEY-----"
  in
  let installation = sample_scope () in
  let exp =
    assert_ok
      (D.collect ~db ~destination:room ~installation ~plan
         ~catalog_revision:pem (* even if caller passes junk, redact binds *)
         ~now:fixed_now ())
  in
  let j = D.to_json exp in
  let dumped = Yojson.Safe.to_string j in
  Alcotest.(check bool) "no SECRETPEM" false (contains dumped "SECRETPEMDATA");
  Alcotest.(check bool)
    "no BEGIN PRIVATE" false
    (contains dumped "BEGIN PRIVATE");
  Alcotest.(check bool) "no whsec raw" false (contains dumped "whsec");
  Alcotest.(check bool) "no ghp_" false (contains dumped "ghp_");
  (* Explicitly inject secret-shaped fields through redact path (export is
     nested under a poisoned object; redaction must still strip secrets). *)
  let poisoned =
    Github_route_ops.redact_json
      (`Assoc
         [
           ("private_key", `String pem);
           ("webhook_secret", `String "whsec_supersecret");
           ("token", `String "ghp_should_not_leak");
           ("export", j);
         ])
  in
  let poisoned_s = Yojson.Safe.to_string poisoned in
  Alcotest.(check bool)
    "poisoned no pem" false
    (contains poisoned_s "SECRETPEMDATA");
  Alcotest.(check bool)
    "poisoned no whsec" false
    (contains poisoned_s "whsec_supersecret");
  Alcotest.(check bool)
    "poisoned no ghp" false
    (contains poisoned_s "ghp_should_not_leak");
  (* Diagnostics lines must not contain PEM body either *)
  let lines = String.concat "\n" (D.format_diagnostics exp) in
  Alcotest.(check bool) "diag no pem" false (contains lines "SECRETPEMDATA");
  Alcotest.(check bool)
    "diag no raw webhook body keys" false
    (contains lines "raw_webhook" || contains lines "comment_body")

(* 4. Diagnostics lines include revisions, managed access, app scope, delivery *)
let test_diagnostics_lines () =
  with_db @@ fun db ->
  (* PR-only advanced filter so envelope dry-run can Match cleanly. *)
  let filter =
    assert_ok
      (F.validate
         {
           F.default with
           pr =
             {
               F.empty_pr with
               labels = Some { op = `In; values = [ "bug" ] };
               author = Some { op = `Eq; values = [ "alice" ] };
             };
         })
  in
  ignore
    (create_route ~db ~id:"rt_diag" ~filter ~managed_bundle_id:"b1"
       ~managed_feature_id:"f1" ~setup_plan_id:"plan-1" ());
  let plan =
    assert_ok
      (Admin.plan_create ~db ~principal ~destination:room
         ~selector:(S.Repo "acme/other") ~base_revision ~now:fixed_now ())
  in
  let installation = sample_scope () in
  let env = make_envelope () in
  let enrichment : En.enrichment =
    { paths = None; teams = None; reasons = []; complete = true }
  in
  let exp =
    assert_ok
      (D.collect ~db ~destination:room ~installation ~plan
         ~catalog_revision:"cat-9" ~envelope:env ~enrichment ~now:fixed_now ())
  in
  let lines = D.format_diagnostics exp in
  let blob = String.concat "\n" lines in
  Alcotest.(check bool)
    "has filter schema" true
    (contains blob "filter_schema_current=1");
  Alcotest.(check bool)
    "has destination" true
    (contains blob "destination=room:room-teams-1");
  Alcotest.(check bool)
    "has plan rev" true
    (contains blob "plan_base_revision=");
  Alcotest.(check bool)
    "has catalog" true
    (contains blob "catalog_revision=cat-9");
  Alcotest.(check bool) "has app_scope" true (contains blob "app_scope status=");
  Alcotest.(check bool) "has delivery" true (contains blob "delivery overall=");
  Alcotest.(check bool) "has route line" true (contains blob "route id=rt_diag");
  Alcotest.(check bool) "has managed" true (contains blob "managed=bundle=b1");
  Alcotest.(check bool)
    "has winning selector" true
    (contains blob "winning_selector=");
  Alcotest.(check bool) "has decision" true (contains blob "decision=");
  Alcotest.(check (option string))
    "winning selector set" (Some "repo:acme/widget") exp.winning_selector;
  Alcotest.(check (option string)) "decision set" (Some "Matched") exp.decision;
  Alcotest.(check bool)
    "predicate reasons non-empty" true
    (List.length exp.predicate_reasons > 0);
  Alcotest.(check bool)
    "enrichment status non-empty" true
    (List.length exp.enrichment_status > 0)

(* 5. Export never includes raw webhook body or private comment fields *)
let test_no_webhook_or_comment_content () =
  with_db @@ fun db ->
  ignore (create_route ~db ~id:"rt_clean" ());
  let env = make_envelope () in
  let exp =
    assert_ok (D.collect ~db ~destination:room ~envelope:env ~now:fixed_now ())
  in
  let dumped = Yojson.Safe.to_string (D.to_json exp) in
  (* Envelope fields that must never appear as raw export content *)
  Alcotest.(check bool) "no head_sha leak" false (contains dumped "abc123");
  Alcotest.(check bool)
    "no raw delivery id as webhook body" false
    (contains dumped "\"raw\"");
  Alcotest.(check bool)
    "no comment_body" false
    (contains dumped "comment_body" || contains dumped "comment body");
  Alcotest.(check bool)
    "no private_key field" false
    (contains dumped "private_key")

(* 6. Missing installation surfaces repair hints *)
let test_repair_hints_missing_install () =
  with_db @@ fun db ->
  ignore (create_route ~db ~id:"rt_rep" ());
  let exp = assert_ok (D.collect ~db ~destination:room ~now:fixed_now ()) in
  Alcotest.(check (option string))
    "readiness fail" (Some "fail") exp.readiness_overall;
  Alcotest.(check bool)
    "repair hints present" true
    (List.length exp.repair_hints > 0);
  let blob = String.concat " " exp.repair_hints in
  Alcotest.(check bool)
    "mentions app/install" true
    (contains (String.lowercase_ascii blob) "app"
    || contains (String.lowercase_ascii blob) "install")

let suite =
  [
    ("predicate counts for advanced filter", `Quick, test_predicate_counts);
    ("export shape and stable keys", `Quick, test_export_shape);
    ("redaction excludes secrets", `Quick, test_redaction_no_secrets);
    ( "diagnostics lines cover revisions managed app delivery",
      `Quick,
      test_diagnostics_lines );
    ( "export excludes raw webhook and comment content",
      `Quick,
      test_no_webhook_or_comment_content );
    ( "missing installation yields repair hints",
      `Quick,
      test_repair_hints_missing_install );
  ]
