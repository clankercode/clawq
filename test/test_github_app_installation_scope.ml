(** Tests for live GitHub App installation Org/repository scope
    (P19.M2.E1.T003). *)

module S = Github_app_installation_scope

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  S.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e
let fixed_now = 1_700_000_000.0
let account = S.{ login = "acme-corp"; id = 99; account_type = "Organization" }

let repo name ?id ?(private_ = false) () : S.repo_ref =
  { full_name = name; id; private_ = Some private_ }

let perms = [ ("issues", "write"); ("metadata", "read") ]

let sample_scope ?(installation_id = 1001) ?(selection = S.All_repos)
    ?(repositories = []) ?(revoked = []) ?(status = S.Active)
    ?(app_id = Some 42) () : S.t =
  S.with_revision
    {
      installation_id;
      app_id;
      account;
      selection;
      repositories;
      revoked_repositories = revoked;
      permissions = perms;
      status;
      revision = "";
      updated_at = Time_util.iso8601_utc ~t:fixed_now ();
    }

let create_all ~db ?(installation_id = 1001) ?(repos = []) () =
  assert_ok
    (S.apply_event ~db ~now:fixed_now
       (S.Installation_created
          {
            installation_id;
            account;
            selection = S.All_repos;
            repositories = repos;
            permissions = perms;
            app_id = Some 42;
          }))

let create_selected ~db ?(installation_id = 1001) ~repos () =
  assert_ok
    (S.apply_event ~db ~now:fixed_now
       (S.Installation_created
          {
            installation_id;
            account;
            selection = S.Selected_repos;
            repositories = repos;
            permissions = perms;
            app_id = Some 42;
          }))

(* 1. upsert + get roundtrip fields *)
let test_upsert_get_roundtrip () =
  with_db @@ fun db ->
  let scope =
    sample_scope ~selection:S.Selected_repos
      ~repositories:
        [ repo "acme-corp/alpha" ~id:1 (); repo "acme-corp/beta" ~id:2 () ]
      ()
  in
  let stored = assert_ok (S.upsert ~db scope) in
  Alcotest.(check bool)
    "revision non-empty" true
    (String.length stored.revision > 0);
  let loaded = assert_ok (S.get ~db ~installation_id:1001) in
  match loaded with
  | None -> Alcotest.fail "expected row"
  | Some got ->
      Alcotest.(check int) "installation_id" 1001 got.installation_id;
      Alcotest.(check (option int)) "app_id" (Some 42) got.app_id;
      Alcotest.(check string) "login" "acme-corp" got.account.login;
      Alcotest.(check int) "account id" 99 got.account.id;
      Alcotest.(check string)
        "account type" "Organization" got.account.account_type;
      Alcotest.(check bool)
        "selected mode" true
        (got.selection = S.Selected_repos);
      Alcotest.(check int) "repo count" 2 (List.length got.repositories);
      Alcotest.(check (list (pair string string)))
        "permissions" perms
        (List.sort (fun (a, _) (b, _) -> String.compare a b) got.permissions);
      Alcotest.(check bool) "active" true (got.status = S.Active);
      Alcotest.(check string) "revision" stored.revision got.revision;
      Alcotest.(check string) "updated_at" scope.updated_at got.updated_at

(* 2. all-repos: is_repo_authorized any name true when Active *)
let test_all_repos_authorizes_any () =
  with_db @@ fun db ->
  let scope = Option.get (create_all ~db ()) in
  Alcotest.(check bool)
    "alpha" true
    (S.is_repo_authorized scope ~repo_full_name:"acme-corp/alpha");
  Alcotest.(check bool)
    "future-repo" true
    (S.is_repo_authorized scope ~repo_full_name:"acme-corp/brand-new");
  Alcotest.(check bool)
    "case insensitive" true
    (S.is_repo_authorized scope ~repo_full_name:"Acme-Corp/Alpha")

(* 3. selected: only listed repos authorized *)
let test_selected_only_listed () =
  with_db @@ fun db ->
  let scope =
    Option.get
      (create_selected ~db
         ~repos:[ repo "acme-corp/alpha" ~id:1 (); repo "acme-corp/beta" () ]
         ())
  in
  Alcotest.(check bool)
    "alpha ok" true
    (S.is_repo_authorized scope ~repo_full_name:"acme-corp/alpha");
  Alcotest.(check bool)
    "beta ok" true
    (S.is_repo_authorized scope ~repo_full_name:"acme-corp/beta");
  Alcotest.(check bool)
    "gamma denied" false
    (S.is_repo_authorized scope ~repo_full_name:"acme-corp/gamma")

(* 4. repos_removed selected: immediately unauthorized *)
let test_repos_removed_selected () =
  with_db @@ fun db ->
  ignore
    (create_selected ~db
       ~repos:
         [ repo "acme-corp/alpha" ~id:1 (); repo "acme-corp/beta" ~id:2 () ]
       ());
  let scope =
    Option.get
    @@ assert_ok
         (S.apply_event ~db ~now:(fixed_now +. 1.)
            (S.Repos_removed
               {
                 installation_id = 1001;
                 repositories = [ repo "acme-corp/alpha" ~id:1 () ];
               }))
  in
  Alcotest.(check bool)
    "alpha unauthorized" false
    (S.is_repo_authorized scope ~repo_full_name:"acme-corp/alpha");
  Alcotest.(check bool)
    "beta still ok" true
    (S.is_repo_authorized scope ~repo_full_name:"acme-corp/beta");
  Alcotest.(check int) "one repo left" 1 (List.length scope.repositories)

(* 5. suspension: all unauthorized *)
let test_suspension_unauthorized () =
  with_db @@ fun db ->
  ignore (create_all ~db ());
  let scope =
    Option.get
    @@ assert_ok
         (S.apply_event ~db ~now:(fixed_now +. 1.)
            (S.Installation_suspend
               { installation_id = 1001; reason = Some "billing" }))
  in
  (match scope.status with
  | S.Suspended { reason = Some "billing" } -> ()
  | _ -> Alcotest.fail "expected suspended");
  Alcotest.(check bool)
    "any repo denied" false
    (S.is_repo_authorized scope ~repo_full_name:"acme-corp/alpha");
  (* Unsuspend restores *)
  let active =
    Option.get
    @@ assert_ok
         (S.apply_event ~db ~now:(fixed_now +. 2.)
            (S.Installation_unsuspend { installation_id = 1001 }))
  in
  Alcotest.(check bool) "active again" true (active.status = S.Active);
  Alcotest.(check bool)
    "authorized again" true
    (S.is_repo_authorized active ~repo_full_name:"acme-corp/alpha")

(* 6. deletion: get Deleted; unauthorized; apply returns None *)
let test_deletion () =
  with_db @@ fun db ->
  ignore (create_all ~db ());
  let applied =
    assert_ok
      (S.apply_event ~db ~now:(fixed_now +. 1.)
         (S.Installation_deleted { installation_id = 1001 }))
  in
  Alcotest.(check bool) "apply returns None" true (applied = None);
  let got = assert_ok (S.get ~db ~installation_id:1001) in
  match got with
  | None -> Alcotest.fail "soft-delete keeps row"
  | Some scope ->
      Alcotest.(check bool) "deleted status" true (scope.status = S.Deleted);
      Alcotest.(check bool)
        "unauthorized" false
        (S.is_repo_authorized scope ~repo_full_name:"acme-corp/alpha");
      (* Idempotent second delete *)
      let again =
        assert_ok
          (S.apply_event ~db ~now:(fixed_now +. 2.)
             (S.Installation_deleted { installation_id = 1001 }))
      in
      Alcotest.(check bool) "second delete None" true (again = None)

let test_stale_create_and_snapshot_cannot_reactivate_deleted () =
  with_db @@ fun db ->
  ignore (create_all ~db ());
  ignore
    (assert_ok
       (S.apply_event ~db ~now:(fixed_now +. 1.)
          (S.Installation_deleted { installation_id = 1001 })));
  let stale_create =
    assert_ok
      (S.apply_event ~db ~now:(fixed_now +. 2.)
         (S.Installation_created
            {
              installation_id = 1001;
              account;
              selection = S.All_repos;
              repositories = [ repo "acme-corp/reactivated" () ];
              permissions = perms;
              app_id = Some 42;
            }))
  in
  Alcotest.(check bool) "stale create returns no active scope" true
    (stale_create = None);
  let snapshot =
    sample_scope ~repositories:[ repo "acme-corp/reactivated" () ] ()
  in
  let reconciled = assert_ok (S.reconcile_from_snapshot ~db ~snapshot) in
  Alcotest.(check bool) "snapshot retains tombstone" true
    (reconciled.status = S.Deleted);
  Alcotest.(check bool)
    "reactivated repo remains denied" false
    (S.is_repo_authorized reconciled ~repo_full_name:"acme-corp/reactivated")

(* 7. idempotent double apply of create *)
let test_idempotent_create () =
  with_db @@ fun db ->
  let first = Option.get (create_all ~db ~repos:[ repo "acme/a" () ] ()) in
  let second =
    Option.get
    @@ assert_ok
         (S.apply_event ~db ~now:(fixed_now +. 99.)
            (S.Installation_created
               {
                 installation_id = 1001;
                 account;
                 selection = S.All_repos;
                 repositories = [ repo "acme/a" () ];
                 permissions = perms;
                 app_id = Some 42;
               }))
  in
  Alcotest.(check string) "same revision" first.revision second.revision;
  Alcotest.(check string) "same updated_at" first.updated_at second.updated_at

(* 8. startup snapshot reconcile overwrites drift *)
let test_snapshot_overwrites_drift () =
  with_db @@ fun db ->
  ignore
    (create_selected ~db
       ~repos:
         [ repo "acme-corp/stale" ~id:9 (); repo "acme-corp/keep" ~id:1 () ]
       ());
  (* Event drift: revoke one, add junk *)
  ignore
  @@ assert_ok
       (S.apply_event ~db ~now:(fixed_now +. 1.)
          (S.Repos_removed
             {
               installation_id = 1001;
               repositories = [ repo "acme-corp/keep" () ];
             }));
  let snapshot =
    sample_scope ~selection:S.Selected_repos
      ~repositories:
        [ repo "acme-corp/keep" ~id:1 (); repo "acme-corp/fresh" ~id:3 () ]
      ~status:S.Active ()
  in
  let reconciled = assert_ok (S.reconcile_from_snapshot ~db ~snapshot) in
  Alcotest.(check bool)
    "keep authorized" true
    (S.is_repo_authorized reconciled ~repo_full_name:"acme-corp/keep");
  Alcotest.(check bool)
    "fresh authorized" true
    (S.is_repo_authorized reconciled ~repo_full_name:"acme-corp/fresh");
  Alcotest.(check bool)
    "stale unauthorized" false
    (S.is_repo_authorized reconciled ~repo_full_name:"acme-corp/stale");
  Alcotest.(check int) "two repos" 2 (List.length reconciled.repositories);
  Alcotest.(check int)
    "revoked cleared" 0
    (List.length reconciled.revoked_repositories);
  (* Idempotent re-reconcile *)
  let again = assert_ok (S.reconcile_from_snapshot ~db ~snapshot:reconciled) in
  Alcotest.(check string) "stable revision" reconciled.revision again.revision

(* 9. repos_added selected expands allowlist *)
let test_repos_added_selected () =
  with_db @@ fun db ->
  ignore (create_selected ~db ~repos:[ repo "acme-corp/alpha" ~id:1 () ] ());
  let scope =
    Option.get
    @@ assert_ok
         (S.apply_event ~db ~now:(fixed_now +. 1.)
            (S.Repos_added
               {
                 installation_id = 1001;
                 repositories = [ repo "acme-corp/beta" ~id:2 () ];
               }))
  in
  Alcotest.(check int) "two repos" 2 (List.length scope.repositories);
  Alcotest.(check bool)
    "beta authorized" true
    (S.is_repo_authorized scope ~repo_full_name:"acme-corp/beta");
  (* Double-add idempotent *)
  let again =
    Option.get
    @@ assert_ok
         (S.apply_event ~db ~now:(fixed_now +. 2.)
            (S.Repos_added
               {
                 installation_id = 1001;
                 repositories = [ repo "acme-corp/beta" ~id:2 () ];
               }))
  in
  Alcotest.(check string) "same revision" scope.revision again.revision

(* 10. all-repos + revoked fails closed for removed repo *)
let test_all_repos_revoked_fail_closed () =
  with_db @@ fun db ->
  ignore (create_all ~db ~repos:[ repo "acme-corp/tracked" ~id:5 () ] ());
  let scope =
    Option.get
    @@ assert_ok
         (S.apply_event ~db ~now:(fixed_now +. 1.)
            (S.Repos_removed
               {
                 installation_id = 1001;
                 repositories = [ repo "acme-corp/revoked" ~id:7 () ];
               }))
  in
  Alcotest.(check bool)
    "revoked denied" false
    (S.is_repo_authorized scope ~repo_full_name:"acme-corp/revoked");
  Alcotest.(check bool)
    "other still allowed" true
    (S.is_repo_authorized scope ~repo_full_name:"acme-corp/other");
  Alcotest.(check bool)
    "revoked list non-empty" true
    (List.length scope.revoked_repositories > 0);
  (* Re-grant via Repos_added clears denylist entry *)
  let restored =
    Option.get
    @@ assert_ok
         (S.apply_event ~db ~now:(fixed_now +. 2.)
            (S.Repos_added
               {
                 installation_id = 1001;
                 repositories = [ repo "acme-corp/revoked" ~id:7 () ];
               }))
  in
  Alcotest.(check bool)
    "re-granted ok" true
    (S.is_repo_authorized restored ~repo_full_name:"acme-corp/revoked")

(* 11. ensure_schema idempotent *)
let test_ensure_schema_idempotent () =
  with_db @@ fun db ->
  S.ensure_schema db;
  S.ensure_schema db;
  ignore (create_all ~db ());
  let rows = assert_ok (S.list ~db) in
  Alcotest.(check int) "one installation" 1 (List.length rows)

let suite =
  [
    ("upsert + get roundtrip fields", `Quick, test_upsert_get_roundtrip);
    ( "all-repos authorizes any when Active",
      `Quick,
      test_all_repos_authorizes_any );
    ("selected only listed repos", `Quick, test_selected_only_listed);
    ( "repos_removed selected immediately unauthorized",
      `Quick,
      test_repos_removed_selected );
    ("suspension unauthorized all", `Quick, test_suspension_unauthorized);
    ("deletion soft-delete unauthorized", `Quick, test_deletion);
    ( "stale create/snapshot cannot reactivate deleted installation",
      `Quick,
      test_stale_create_and_snapshot_cannot_reactivate_deleted );
    ("idempotent double create", `Quick, test_idempotent_create);
    ( "snapshot reconcile overwrites drift",
      `Quick,
      test_snapshot_overwrites_drift );
    ("repos_added selected expands allowlist", `Quick, test_repos_added_selected);
    ( "all-repos revoked fails closed",
      `Quick,
      test_all_repos_revoked_fail_closed );
    ("ensure_schema idempotent", `Quick, test_ensure_schema_idempotent);
  ]
