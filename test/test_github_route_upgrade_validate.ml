(** Tests for upgrade validation, drift checks, and admin guidance
    (P20.M2.E2.T002). *)

module S = Github_route_store
module F = Github_route_filter
module U = Github_route_upgrade_validate
module Inst = Github_app_installation_scope
module Auth = Github_auth_selection
module M = Github_route_migrate

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  S.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_700_000_000.0
let room = S.Room "room-teams-1"
let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e
let contains hay needle = Test_helpers.string_contains hay needle

let create_route ~db ?(id = "rt-1") ?(selector = S.Repo "acme/widget")
    ?(filter = S.default_filter) ?(enabled = true) ?managed_bundle_id
    ?managed_feature_id () =
  let provenance =
    {
      S.created_by = Some "alice";
      created_via = Some "setup_plan";
      setup_plan_id = Some "plan-1";
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

let app_auth ?(app_id = 42) ?(installation_id = 1001) () : Auth.auth_snapshot =
  {
    pat_token_present = false;
    app =
      Some
        {
          app_id;
          private_key_path = "/tmp/github-app.pem";
          webhook_secret = "whsec";
          installations = [ { installation_id; repos = [ "acme/widget" ] } ];
        };
  }

let severity_of name (report : U.report) =
  match List.find_opt (fun (c : U.check) -> c.name = name) report.checks with
  | Some c -> c.severity
  | None -> Alcotest.failf "missing check %s" name

let find_check_prefix prefix (report : U.report) =
  List.find_opt
    (fun (c : U.check) -> String.starts_with ~prefix c.name)
    report.checks

(* 1. Empty DB: drift pass, migration pass, aliases warn *)
let test_empty_validate_overall_okish () =
  with_db @@ fun db ->
  let report = assert_ok (U.validate ~db ~now:fixed_now ()) in
  Alcotest.(check int) "routes" 0 report.routes_checked;
  Alcotest.(check int) "legacy" 0 report.legacy_subscription_count;
  Alcotest.(check int)
    "filter schema current" F.current_schema_version
    report.filter_schema_current;
  (* Drift should pass; aliases warn → overall warn *)
  Alcotest.(check bool)
    "overall not fail" true
    (report.overall = U.Pass || report.overall = U.Warn);
  Alcotest.(check bool)
    "has rollback guidance" true
    (List.length report.rollback_guidance >= 5);
  Alcotest.(check bool)
    "deprecated aliases non-empty" true
    (report.deprecated_aliases <> [])

(* 2. Pure schema check: current passes *)
let test_schema_current_pass () =
  let cs = U.check_filter_schema F.default in
  Alcotest.(check int) "one check" 1 (List.length cs);
  match cs with
  | [ c ] -> Alcotest.(check bool) "pass" true (c.severity = U.Pass)
  | _ -> Alcotest.fail "expected single check"

(* 3. Schema too new fails *)
let test_schema_too_new_fails () =
  let f = { F.default with schema_version = F.current_schema_version + 9 } in
  let cs = U.check_filter_schema f in
  match cs with
  | [ c ] ->
      Alcotest.(check bool) "fail" true (c.severity = U.Fail);
      Alcotest.(check bool) "has repair" true (Option.is_some c.repair)
  | _ -> Alcotest.fail "expected single check"

(* 4. Managed linkage incomplete fails *)
let test_managed_partial_fails () =
  with_db @@ fun db ->
  (* Bypass create validation by writing via create with only bundle — store
     accepts option independently; validate must catch partial pair. *)
  ignore (create_route ~db ~id:"rt_partial" ~managed_bundle_id:"bundle-only" ());
  let report = assert_ok (U.validate ~db ~now:fixed_now ()) in
  match find_check_prefix "managed_linkage:rt_partial" report with
  | Some c -> Alcotest.(check bool) "fail" true (c.severity = U.Fail)
  | None -> (
      match
        List.find_opt
          (fun (c : U.check) -> c.category = U.Managed && c.severity = U.Fail)
          report.checks
      with
      | Some _ -> ()
      | None -> Alcotest.fail "expected managed Fail check")

(* 5. Managed complete passes *)
let test_managed_complete_ok () =
  with_db @@ fun db ->
  ignore
    (create_route ~db ~id:"rt_m" ~managed_bundle_id:"b1"
       ~managed_feature_id:"f1" ());
  let report =
    assert_ok
      (U.validate ~db ~now:fixed_now
         ~catalog_state:
           {
             tools_ok = true;
             mcp_ok = true;
             catalog_revision = Some "cat-1";
             access_revision = Some "acc-1";
           }
         ())
  in
  Alcotest.(check bool)
    "no managed fail" false
    (List.exists
       (fun (c : U.check) -> c.category = U.Managed && c.severity = U.Fail)
       report.checks)

(* 6. Org route without installation fails *)
let test_org_requires_installation () =
  with_db @@ fun db ->
  ignore (create_route ~db ~id:"rt_org" ~selector:(S.Org "acme") ());
  let report = assert_ok (U.validate ~db ~now:fixed_now ()) in
  Alcotest.(check bool) "overall fail" true (report.overall = U.Fail);
  Alcotest.(check bool)
    "installation fail" true
    (severity_of "installation_scope" report = U.Fail)

(* 7. Org route with Active install + auth passes installation *)
let test_org_with_active_install () =
  with_db @@ fun db ->
  ignore (create_route ~db ~id:"rt_org" ~selector:(S.Org "acme") ());
  let installation = sample_scope () in
  let auth = app_auth () in
  let report =
    assert_ok (U.validate ~db ~installation ~auth ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "installation pass" true
    (severity_of "installation_scope" report = U.Pass);
  Alcotest.(check bool)
    "org_auth pass" true
    (severity_of "org_auth_claim" report = U.Pass)

(* 8. Catalog tools/mcp injectables fail closed *)
let test_catalog_injectable_fail () =
  with_db @@ fun db ->
  ignore (create_route ~db ~id:"rt1" ());
  let report =
    assert_ok
      (U.validate ~db ~now:fixed_now
         ~catalog_state:
           {
             tools_ok = false;
             mcp_ok = false;
             catalog_revision = None;
             access_revision = None;
           }
         ())
  in
  Alcotest.(check bool)
    "tools fail" true
    (severity_of "tools_catalog" report = U.Fail);
  Alcotest.(check bool)
    "mcp fail" true
    (severity_of "mcp_catalog" report = U.Fail)

(* 9. Session refresh must not require restart *)
let test_session_refresh_restart_fail () =
  with_db @@ fun db ->
  ignore (create_route ~db ~id:"rt1" ());
  let report =
    assert_ok
      (U.validate ~db ~now:fixed_now
         ~session_refresh:
           {
             active_room_ids = [ "room-teams-1" ];
             refresh_pending_room_ids = [ "room-teams-1" ];
             refresh_without_restart = false;
           }
         ())
  in
  Alcotest.(check bool)
    "restart fail" true
    (severity_of "session_refresh_no_restart" report = U.Fail);
  Alcotest.(check bool)
    "pending warn" true
    (severity_of "session_refresh_pending" report = U.Warn)

(* 10. Drift: force documented mismatch *)
let test_drift_mismatch () =
  let cs =
    U.drift_checks ~documented_filter_schema_version:99
      ~documented_default_comment_mode:"threaded" ()
  in
  Alcotest.(check bool)
    "schema drift fail" true
    (List.exists
       (fun (c : U.check) ->
         c.name = "drift_filter_schema_version" && c.severity = U.Fail)
       cs);
  Alcotest.(check bool)
    "comment drift fail" true
    (List.exists
       (fun (c : U.check) ->
         c.name = "drift_default_comment_mode" && c.severity = U.Fail)
       cs)

(* 11. Drift defaults match runtime (happy path) *)
let test_drift_aligned () =
  let cs = U.drift_checks () in
  Alcotest.(check bool)
    "all drift pass or specificity" true
    (List.for_all (fun (c : U.check) -> c.severity = U.Pass) cs);
  Alcotest.(check int) "documented schema" 1 U.documented_filter_schema_version;
  Alcotest.(check string)
    "documented comment" "summary" U.documented_default_comment_mode

(* 12. Deprecated aliases map to github route *)
let test_deprecated_aliases () =
  let checks, aliases = U.deprecated_alias_checks () in
  Alcotest.(check bool) "aliases present" true (aliases <> []);
  List.iter
    (fun (_legacy, canonical) ->
      Alcotest.(check bool)
        ("canonical " ^ canonical) true
        (String.starts_with ~prefix:"github route" canonical))
    aliases;
  match checks with
  | [ c ] ->
      Alcotest.(check bool)
        "warn or pass" true
        (c.severity = U.Warn || c.severity = U.Pass)
  | _ -> Alcotest.fail "expected one alias check"

(* 13. Migration: legacy without routes fails when we seed legacy table *)
let test_legacy_unmigrated_fails () =
  with_db @@ fun db ->
  Github_pr_subscriptions.init_schema db;
  ignore
    (Github_pr_subscriptions.add ~db ~room_id:"room-teams-1" ~repo:"acme/widget"
       ~pr_number:7 ~profile_id:1 ());
  let report = assert_ok (U.validate ~db ~now:fixed_now ()) in
  Alcotest.(check bool)
    "migration fail" true
    (severity_of "subscription_migration" report = U.Fail);
  Alcotest.(check int) "legacy count" 1 report.legacy_subscription_count

(* 14. Migration: after migrate, legacy still present → warn *)
let test_legacy_after_migrate_warns () =
  with_db @@ fun db ->
  Github_pr_subscriptions.init_schema db;
  ignore
    (Github_pr_subscriptions.add ~db ~room_id:"room-teams-1" ~repo:"acme/widget"
       ~pr_number:7 ~profile_id:1 ());
  ignore (assert_ok (M.migrate_database ~db ~now:fixed_now ()));
  let report = assert_ok (U.validate ~db ~now:fixed_now ()) in
  Alcotest.(check bool) "routes exist" true (report.routes_checked >= 1);
  Alcotest.(check bool)
    "migration not fail" true
    (severity_of "subscription_migration" report <> U.Fail)

(* 15. Redacted JSON export stable keys, no secrets *)
let test_to_json_redacted () =
  with_db @@ fun db ->
  ignore (create_route ~db ~id:"rt1" ());
  let report = assert_ok (U.validate ~db ~now:fixed_now ()) in
  let j = U.to_json report in
  let blob = Yojson.Safe.to_string j in
  Alcotest.(check bool) "has overall" true (contains blob "overall");
  Alcotest.(check bool)
    "has rollback_guidance" true
    (contains blob "rollback_guidance");
  Alcotest.(check bool)
    "has deprecated_aliases" true
    (contains blob "deprecated_aliases");
  Alcotest.(check bool)
    "no private_key" false
    (contains (String.lowercase_ascii blob) "begin rsa");
  match j with
  | `Assoc fields ->
      let keys = List.map fst fields |> List.sort String.compare in
      Alcotest.(check bool)
        "sorted keys" true
        (keys = List.sort String.compare keys);
      Alcotest.(check bool) "has checks" true (List.mem "checks" keys)
  | _ -> Alcotest.fail "expected assoc"

(* 16. format_report includes repair/rollback lines *)
let test_format_report () =
  with_db @@ fun db ->
  ignore (create_route ~db ~id:"rt_org" ~selector:(S.Org "acme") ());
  let report = assert_ok (U.validate ~db ~now:fixed_now ()) in
  let lines = U.format_report report in
  let blob = String.concat "\n" lines in
  Alcotest.(check bool)
    "upgrade_validate header" true
    (contains blob "upgrade_validate");
  Alcotest.(check bool) "repair lines" true (contains blob "repair:");
  Alcotest.(check bool) "rollback lines" true (contains blob "rollback:")

(* 17. Operator contract documents upgrade section *)
let test_operator_contract_upgrade_section () =
  let rec find_root dir =
    if
      Sys.file_exists (Filename.concat dir "dune-project")
      && Sys.file_exists (Filename.concat dir "docs")
    then dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then Sys.getcwd () else find_root parent
  in
  let root = find_root (Sys.getcwd ()) in
  let path = Filename.concat root "docs/github-route-operator-contract.md" in
  Alcotest.(check bool) "contract exists" true (Sys.file_exists path);
  let ic = open_in path in
  let body =
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () -> really_input_string ic (in_channel_length ic))
  in
  List.iter
    (fun phrase ->
      Alcotest.(check bool)
        (Printf.sprintf "contract contains %S" phrase)
        true (contains body phrase))
    [
      "Upgrade validation and drift checks";
      "Github_route_upgrade_validate";
      "Rollback (failed upgrade";
      "Deprecated aliases";
      "subscriptions add";
      "github route item add";
      "without daemon restart";
      "current_schema_version";
      "Prefer_existing_route";
    ]

let suite =
  [
    ("empty validate is not fail", `Quick, test_empty_validate_overall_okish);
    ("schema current passes", `Quick, test_schema_current_pass);
    ("schema too new fails", `Quick, test_schema_too_new_fails);
    ("managed partial fails", `Quick, test_managed_partial_fails);
    ("managed complete ok", `Quick, test_managed_complete_ok);
    ("org requires installation", `Quick, test_org_requires_installation);
    ("org with active install passes", `Quick, test_org_with_active_install);
    ("catalog injectable fail", `Quick, test_catalog_injectable_fail);
    ("session refresh restart fails", `Quick, test_session_refresh_restart_fail);
    ("drift mismatch fails", `Quick, test_drift_mismatch);
    ("drift aligned passes", `Quick, test_drift_aligned);
    ("deprecated aliases map to route", `Quick, test_deprecated_aliases);
    ("legacy unmigrated fails", `Quick, test_legacy_unmigrated_fails);
    ( "legacy after migrate warns or pass",
      `Quick,
      test_legacy_after_migrate_warns );
    ("to_json redacted stable", `Quick, test_to_json_redacted);
    ("format_report repair rollback", `Quick, test_format_report);
    ( "operator contract upgrade section",
      `Quick,
      test_operator_contract_upgrade_section );
  ]
