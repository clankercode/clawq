(** Tests for confirmed GitHub route apply + managed Room access refresh
    (P19.M2.E3.T003). *)

module S = Github_route_store
module Admin = Github_route_admin
module Apply = Github_route_apply
module Auth = Github_auth_selection
module Inst = Github_app_installation_scope

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  S.ensure_schema db;
  Setup_plan_apply.init_schema db;
  Setup_plan_bundle.init_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_700_000_000.0
let base_revision = "rev-config-1"
let room = S.Room "room-teams-1"
let repo_sel = S.Repo "Acme/Widget"
let org_sel = S.Org "AcmeCorp"

let principal =
  Setup_plan.{ id = "principal:alice"; kind = Principal; label = Some "Alice" }

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e
let contains hay needle = Test_helpers.string_contains hay needle

let sample_app ?(app_id = 42) ?(installation_id = 1001) () :
    Runtime_config.github_app_config =
  {
    app_id;
    private_key_path = "/tmp/github-app.pem";
    webhook_secret = "whsec";
    installations = [ { installation_id; repos = [ "AcmeCorp/widget" ] } ];
  }

let sample_scope ?(installation_id = 1001) ?(login = "AcmeCorp")
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

let allow_admin = true

let make_req ?(digest_override = None) ?(destination_room = Some "room-teams-1")
    ?(is_global_admin = allow_admin) ?(is_room_admin = fun ~room_id:_ -> false)
    ?auth_snapshot ?installation ~plan_id ~digest () : Apply.apply_request =
  {
    plan_id;
    digest = (match digest_override with Some d -> d | None -> digest);
    principal;
    current_base_revision = base_revision;
    destination_room;
    now = fixed_now;
    is_global_admin;
    is_room_admin;
    auth_snapshot;
    installation;
  }

(* 1. Happy path: apply create plan → route exists + Applied *)
let test_happy_path_create () =
  with_db @@ fun db ->
  let plan =
    assert_ok
      (Admin.plan_create ~db ~principal ~destination:room ~selector:repo_sel
         ~base_revision ~now:fixed_now ~route_id:"rt_happy" ())
  in
  let outcome =
    Apply.apply_confirmed ~db (make_req ~plan_id:plan.id ~digest:plan.digest ())
  in
  match outcome with
  | Apply.Rejected { reason; message } ->
      Alcotest.fail (Printf.sprintf "%s: %s" reason message)
  | Apply.Applied { receipt_id; route_ids; catalog_refresh_rooms } -> (
      Alcotest.(check bool)
        "receipt non-empty" true
        (String.length receipt_id > 0);
      Alcotest.(check (list string)) "route ids" [ "rt_happy" ] route_ids;
      Alcotest.(check bool)
        "refresh room listed" true
        (List.mem "room-teams-1" catalog_refresh_rooms);
      match S.get ~db ~id:"rt_happy" with
      | Error e -> Alcotest.fail e
      | Ok None -> Alcotest.fail "route missing after apply"
      | Ok (Some r) ->
          Alcotest.(check bool) "enabled" true r.enabled;
          Alcotest.(check string)
            "selector" "repo:acme/widget"
            (S.canonical_selector_key r.selector))

(* 2. Digest mismatch rejected *)
let test_digest_mismatch () =
  with_db @@ fun db ->
  let plan =
    assert_ok
      (Admin.plan_create ~db ~principal ~destination:room ~selector:repo_sel
         ~base_revision ~now:fixed_now ~route_id:"rt_dig" ())
  in
  let outcome =
    Apply.apply_confirmed ~db
      (make_req ~plan_id:plan.id ~digest:plan.digest
         ~digest_override:(Some (String.make 64 'a'))
         ())
  in
  match outcome with
  | Apply.Applied _ -> Alcotest.fail "expected digest mismatch"
  | Apply.Rejected { reason; message } -> (
      Alcotest.(check string) "reason" "digest_mismatch" reason;
      Alcotest.(check bool)
        "message mentions digest" true
        (contains (String.lowercase_ascii message) "digest");
      (* No route created. *)
      match S.get ~db ~id:"rt_dig" with
      | Ok None -> ()
      | Ok (Some _) -> Alcotest.fail "route should not exist"
      | Error e -> Alcotest.fail e)

(* 3. PAT + Org route plan rejected with App guidance *)
let test_pat_org_rejected () =
  with_db @@ fun db ->
  let plan =
    assert_ok
      (Admin.plan_create ~db ~principal ~destination:room ~selector:org_sel
         ~base_revision ~now:fixed_now ~route_id:"rt_org_pat" ())
  in
  let auth = Auth.snapshot_of_parts ~pat:"ghp_test_token_only" () in
  let outcome =
    Apply.apply_confirmed ~db
      (make_req ~plan_id:plan.id ~digest:plan.digest ~auth_snapshot:auth ())
  in
  match outcome with
  | Apply.Applied _ -> Alcotest.fail "PAT must not claim Org"
  | Apply.Rejected { reason; message } -> (
      Alcotest.(check string) "reason" "org_requires_app" reason;
      let lower = String.lowercase_ascii message in
      Alcotest.(check bool) "mentions app" true (contains lower "app");
      Alcotest.(check bool)
        "mentions migration or migrate" true
        (contains lower "migrat" || contains lower "github app");
      Alcotest.(check bool) "mentions org" true (contains lower "org");
      match S.get ~db ~id:"rt_org_pat" with
      | Ok None -> ()
      | Ok (Some _) -> Alcotest.fail "org route must not be created under PAT"
      | Error e -> Alcotest.fail e)

(* 4. App + Org with installation allowed *)
let test_app_org_allowed () =
  with_db @@ fun db ->
  let plan =
    assert_ok
      (Admin.plan_create ~db ~principal ~destination:room ~selector:org_sel
         ~base_revision ~now:fixed_now ~route_id:"rt_org_app" ())
  in
  let auth = Auth.snapshot_of_parts ~app:(sample_app ()) () in
  let installation = sample_scope () in
  let outcome =
    Apply.apply_confirmed ~db
      (make_req ~plan_id:plan.id ~digest:plan.digest ~auth_snapshot:auth
         ~installation ())
  in
  match outcome with
  | Apply.Rejected { reason; message } ->
      Alcotest.fail (Printf.sprintf "%s: %s" reason message)
  | Apply.Applied { route_ids; _ } -> (
      Alcotest.(check (list string)) "route ids" [ "rt_org_app" ] route_ids;
      match S.get ~db ~id:"rt_org_app" with
      | Ok (Some r) ->
          Alcotest.(check bool) "enabled" true r.enabled;
          Alcotest.(check string)
            "org selector" "org:acmecorp"
            (S.canonical_selector_key r.selector)
      | Ok None -> Alcotest.fail "org route missing"
      | Error e -> Alcotest.fail e)

(* 5. Catalog refresh hook called with room id *)
let test_catalog_refresh_hook () =
  with_db @@ fun db ->
  let plan =
    assert_ok
      (Admin.plan_create ~db ~principal ~destination:room ~selector:repo_sel
         ~base_revision ~now:fixed_now ~route_id:"rt_refresh" ())
  in
  let called = ref [] in
  let hook ~room_id = called := room_id :: !called in
  let outcome =
    Apply.apply_confirmed ~db ~on_catalog_refresh:hook
      (make_req ~plan_id:plan.id ~digest:plan.digest ())
  in
  match outcome with
  | Apply.Rejected { message; _ } -> Alcotest.fail message
  | Apply.Applied { catalog_refresh_rooms; _ } ->
      Alcotest.(check bool)
        "hook invoked" true
        (List.mem "room-teams-1" !called);
      Alcotest.(check bool)
        "rooms list has dest" true
        (List.mem "room-teams-1" catalog_refresh_rooms)

(* 6. Managed bundle attach when payload has feature_id *)
let test_managed_bundle_attach () =
  with_db @@ fun db ->
  let plan =
    assert_ok
      (Admin.plan_create ~db ~principal ~destination:room ~selector:repo_sel
         ~base_revision ~now:fixed_now ~route_id:"rt_managed"
         ~managed_bundle_id:"bundle-gh-tools"
         ~managed_feature_id:"feat-route-rt_managed" ())
  in
  let outcome =
    Apply.apply_confirmed ~db (make_req ~plan_id:plan.id ~digest:plan.digest ())
  in
  match outcome with
  | Apply.Rejected { message; _ } -> Alcotest.fail message
  | Apply.Applied _ -> (
      match S.get ~db ~id:"rt_managed" with
      | Ok (Some r) ->
          Alcotest.(check (option string))
            "bundle on route" (Some "bundle-gh-tools") r.managed_bundle_id;
          Alcotest.(check (option string))
            "feature on route" (Some "feat-route-rt_managed")
            r.managed_feature_id;
          Alcotest.(check bool)
            "setup-owned linkage" true
            (Setup_plan_bundle.is_setup_owned ~db ~room_id:"room-teams-1"
               ~bundle_id:"bundle-gh-tools" ());
          let attached =
            Setup_plan_bundle.list_attached ~db ~room_id:"room-teams-1" ()
          in
          Alcotest.(check bool)
            "feature present" true
            (List.exists
               (fun (l : Setup_plan_bundle.linkage) ->
                 l.provenance.feature_id = "feat-route-rt_managed"
                 && l.bundle_id = "bundle-gh-tools")
               attached)
      | Ok None -> Alcotest.fail "route missing"
      | Error e -> Alcotest.fail e)

(* 7. Authority fail rejects *)
let test_authority_fail () =
  with_db @@ fun db ->
  let plan =
    assert_ok
      (Admin.plan_create ~db ~principal ~destination:room ~selector:repo_sel
         ~base_revision ~now:fixed_now ~route_id:"rt_auth" ())
  in
  let outcome =
    Apply.apply_confirmed ~db
      (make_req ~plan_id:plan.id ~digest:plan.digest ~is_global_admin:false
         ~is_room_admin:(fun ~room_id:_ -> false)
         ())
  in
  match outcome with
  | Apply.Applied _ -> Alcotest.fail "expected authority denial"
  | Apply.Rejected { reason; message } -> (
      Alcotest.(check string) "reason" "authority_denied" reason;
      Alcotest.(check bool)
        "message mentions admin/authority" true
        (contains (String.lowercase_ascii message) "admin"
        || contains (String.lowercase_ascii message) "authority");
      match S.get ~db ~id:"rt_auth" with
      | Ok None -> ()
      | Ok (Some _) -> Alcotest.fail "route must not be created"
      | Error e -> Alcotest.fail e)

let suite =
  [
    ("happy path apply create", `Quick, test_happy_path_create);
    ("digest mismatch rejected", `Quick, test_digest_mismatch);
    ("PAT + Org rejected with App guidance", `Quick, test_pat_org_rejected);
    ("App + Org with installation allowed", `Quick, test_app_org_allowed);
    ("catalog refresh hook called", `Quick, test_catalog_refresh_hook);
    ("managed bundle attach on feature_id", `Quick, test_managed_bundle_attach);
    ("authority fail rejects", `Quick, test_authority_fail);
  ]
