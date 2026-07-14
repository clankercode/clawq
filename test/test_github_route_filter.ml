(** Tests for versioned advanced PR/Issue route filters (P20.M1.E1.T001). *)

module F = Github_route_filter
module S = Github_route_store

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let assert_error_contains needle = function
  | Ok _ -> Alcotest.fail ("expected error containing: " ^ needle)
  | Error msg ->
      Alcotest.(check bool)
        ("error mentions " ^ needle)
        true
        (Test_helpers.string_contains
           (String.lowercase_ascii msg)
           (String.lowercase_ascii needle))

let test_default_is_v1_empty () =
  Alcotest.(check int)
    "schema_version" F.current_schema_version F.default.schema_version;
  Alcotest.(check (list string)) "include_events" [] F.default.include_events;
  Alcotest.(check (list string)) "exclude_events" [] F.default.exclude_events;
  Alcotest.(check bool) "no advanced" false (F.has_advanced F.default);
  Alcotest.(check bool)
    "no path demand" false
    (F.requires_changed_paths F.default);
  Alcotest.(check bool)
    "no team demand" false
    (F.requires_team_membership F.default)

let test_migrate_v0_keeps_empty_include_exclude () =
  let v0 : F.v0 =
    {
      include_events = [];
      exclude_events = [];
      include_repos = [];
      exclude_repos = [];
    }
  in
  let v1 = F.migrate_v0_to_v1 v0 in
  Alcotest.(check int) "version" 1 v1.schema_version;
  Alcotest.(check (list string)) "include empty as-is" [] v1.include_events;
  Alcotest.(check (list string)) "exclude empty as-is" [] v1.exclude_events;
  Alcotest.(check bool) "advanced empty" false (F.has_advanced v1);
  let v0b : F.v0 =
    {
      include_events = [ "pull_request" ];
      exclude_events = [ "issue_comment" ];
      include_repos = [ "acme/a" ];
      exclude_repos = [ "acme/b" ];
    }
  in
  let v1b = F.migrate_v0_to_v1 v0b in
  Alcotest.(check (list string))
    "include preserved" [ "pull_request" ] v1b.include_events;
  Alcotest.(check (list string))
    "exclude preserved" [ "issue_comment" ] v1b.exclude_events;
  Alcotest.(check (list string)) "include_repos" [ "acme/a" ] v1b.include_repos;
  Alcotest.(check (list string)) "exclude_repos" [ "acme/b" ] v1b.exclude_repos

let test_of_json_v0_baseline_migrates () =
  let j =
    `Assoc
      [
        ("include_events", `List [ `String "issues" ]);
        ("exclude_events", `List []);
        ("include_repos", `List []);
        ("exclude_repos", `List []);
      ]
  in
  let f = assert_ok (F.of_json j) in
  Alcotest.(check int) "migrated version" 1 f.schema_version;
  Alcotest.(check (list string)) "include" [ "issues" ] f.include_events;
  Alcotest.(check bool) "no advanced" false (F.has_advanced f)

let test_of_json_v0_empty_stays_empty () =
  let j =
    `Assoc
      [
        ("include_events", `List []);
        ("exclude_events", `List []);
        ("include_repos", `List []);
        ("exclude_repos", `List []);
      ]
  in
  let f = assert_ok (F.of_json j) in
  Alcotest.(check (list string)) "empty include" [] f.include_events;
  Alcotest.(check (list string)) "empty exclude" [] f.exclude_events

let test_roundtrip_advanced_pr_and_issue () =
  let f : F.t =
    {
      F.default with
      include_events = [ "pull_request"; "issues" ];
      pr =
        {
          base_branch = Some { op = `Glob; values = [ "release/*"; "main" ] };
          head_branch = Some { op = `Neq; values = [ "main" ] };
          changed_path = Some { op = `Glob; values = [ "src/**" ] };
          labels = Some { op = `In; values = [ "needs-review"; "p20" ] };
          author = Some { op = `Eq; values = [ "alice" ] };
          team = Some { op = `In; values = [ "acme/backend" ] };
          draft = Some false;
        };
      issue =
        {
          labels = Some { op = `Not_in; values = [ "wontfix" ] };
          author = Some { op = `In; values = [ "bob" ] };
          team = Some { op = `In; values = [ "acme/triage" ] };
          assignee = Some { op = `In; values = [ "carol" ] };
          milestone = Some { op = `Eq; values = [ "v1.0" ] };
        };
    }
  in
  let f = assert_ok (F.validate f) in
  Alcotest.(check bool) "has advanced" true (F.has_advanced f);
  Alcotest.(check bool) "needs paths" true (F.requires_changed_paths f);
  Alcotest.(check bool) "needs team" true (F.requires_team_membership f);
  let j = F.to_json f in
  let got = assert_ok (F.of_json j) in
  Alcotest.(check int) "version" 1 got.schema_version;
  Alcotest.(check (list string))
    "include"
    [ "pull_request"; "issues" ]
    got.include_events;
  (match got.pr.base_branch with
  | Some { op = `Glob; values } ->
      Alcotest.(check (list string)) "base" [ "release/*"; "main" ] values
  | _ -> Alcotest.fail "base_branch");
  (match got.pr.draft with
  | Some false -> ()
  | _ -> Alcotest.fail "draft false");
  (match got.issue.milestone with
  | Some { op = `Eq; values = [ "v1.0" ] } -> ()
  | _ -> Alcotest.fail "milestone");
  match got.issue.assignee with
  | Some { op = `In; values = [ "carol" ] } -> ()
  | _ -> Alcotest.fail "assignee"

let test_validate_rejects_bad_operators_and_values () =
  let bad_op =
    {
      F.default with
      pr = { F.empty_pr with labels = Some { op = `In; values = [] } };
    }
  in
  assert_error_contains "non-empty" (F.validate bad_op);
  let bad_eq =
    {
      F.default with
      pr = { F.empty_pr with author = Some { op = `Eq; values = [ "a"; "b" ] } };
    }
  in
  assert_error_contains "exactly one" (F.validate bad_eq);
  assert_error_contains "unknown set operator" (F.set_op_of_string "regex");
  assert_error_contains "unknown glob operator" (F.glob_op_of_string "regex")

let test_reject_raw_json_predicates () =
  let cases =
    [
      ( `Assoc
          [
            ("schema_version", `Int 1);
            ("include_events", `List []);
            ("exclude_events", `List []);
            ("include_repos", `List []);
            ("exclude_repos", `List []);
            ( "predicates",
              `List
                [
                  `Assoc
                    [
                      ("jsonpath", `String "$.labels[*]"); ("op", `String "in");
                    ];
                ] );
          ],
        "raw" );
      ( `Assoc
          [
            ("schema_version", `Int 1);
            ("include_events", `List []);
            ("exclude_events", `List []);
            ("include_repos", `List []);
            ("exclude_repos", `List []);
            ("expr", `String "labels contains 'bug'");
          ],
        "raw" );
      ( `Assoc
          [
            ("schema_version", `Int 1);
            ("include_events", `List []);
            ("exclude_events", `List []);
            ("include_repos", `List []);
            ("exclude_repos", `List []);
            ( "pr",
              `Assoc
                [
                  ( "labels",
                    `Assoc
                      [
                        ("jsonpath", `String "$.labels");
                        ("op", `String "in");
                        ("values", `List [ `String "bug" ]);
                      ] );
                ] );
          ],
        "raw" );
      ( `Assoc
          [
            ("include_events", `List []);
            ("pr", `Assoc [ ("labels", `String "bug") ]);
          ],
        "schema_version" );
      ( `Assoc
          [
            ("schema_version", `Int 0);
            ("pr", `Assoc [ ("labels", `String "bug") ]);
          ],
        "schema_version" );
      ( `Assoc
          [
            ("schema_version", `Int 1);
            ("advanced", `Assoc [ ("expr", `String "labels contains bug") ]);
          ],
        "raw" );
      ( `Assoc [ ("schema_version", `Int 1); ("advanced", `String "raw") ],
        "object" );
      ( `Assoc
          [
            ("schema_version", `Int 1);
            ("include_events", `List []);
            ("exclude_events", `List []);
            ("include_repos", `List []);
            ("exclude_repos", `List []);
            ( "pr",
              `Assoc
                [
                  ( "unknown_field",
                    `Assoc
                      [
                        ("op", `String "in"); ("values", `List [ `String "x" ]);
                      ] );
                ] );
          ],
        "unknown" );
    ]
  in
  List.iteri
    (fun i (j, needle) ->
      match F.of_json j with
      | Ok _ -> Alcotest.fail (Printf.sprintf "case %d expected reject" i)
      | Error msg ->
          Alcotest.(check bool)
            (Printf.sprintf "case %d contains %s" i needle)
            true
            (Test_helpers.string_contains
               (String.lowercase_ascii msg)
               (String.lowercase_ascii needle)))
    cases

let test_advanced_wrapper_parses_and_is_exclusive () =
  let wrapped =
    `Assoc
      [
        ("schema_version", `Int 1);
        ( "advanced",
          `Assoc
            [
              ("pr", `Assoc [ ("labels", `String "ready") ]);
              ("issue", `Assoc [ ("author", `String "alice") ]);
            ] );
      ]
  in
  let parsed = assert_ok (F.of_json wrapped) in
  Alcotest.(check bool)
    "wrapper preserves advanced fields" true (F.has_advanced parsed);
  assert_error_contains "cannot be combined"
    (F.of_json
       (`Assoc
          [
            ("schema_version", `Int 1);
            ("pr", `Assoc [ ("labels", `String "ready") ]);
            ("advanced", `Assoc [ ("issue", `Assoc []) ]);
          ]))

let test_noncanonical_advanced_keys_are_rejected () =
  let typed_pr = `Assoc [ ("labels", `String "ready") ] in
  assert_error_contains "unknown field"
    (F.of_json (`Assoc [ ("schema_version", `Int 1); ("PR", typed_pr) ]));
  assert_error_contains "unknown or raw field"
    (F.of_json
       (`Assoc
          [
            ("schema_version", `Int 1); ("advanced", `Assoc [ ("PR", typed_pr) ]);
          ]))

let test_noncanonical_nested_filter_keys_are_rejected () =
  let label_match =
    `Assoc [ ("op", `String "in"); ("values", `List [ `String "ready" ]) ]
  in
  assert_error_contains "unknown field"
    (F.of_json
       (`Assoc
          [
            ("schema_version", `Int 1);
            ("pr", `Assoc [ ("LABELS", label_match) ]);
          ]));
  assert_error_contains "unknown field"
    (F.of_json
       (`Assoc
          [
            ("schema_version", `Int 1);
            ("issue", `Assoc [ ("AUTHOR", `String "alice") ]);
          ]));
  assert_error_contains "unknown field"
    (F.of_json
       (`Assoc
          [
            ("schema_version", `Int 1);
            ( "advanced",
              `Assoc
                [
                  ( "pr",
                    `Assoc
                      [
                        ( "labels",
                          `Assoc
                            [
                              ("OP", `String "in");
                              ("values", `List [ `String "ready" ]);
                            ] );
                      ] );
                ] );
          ]))

let test_validate_ops_from_json () =
  let j =
    `Assoc
      [
        ("schema_version", `Int 1);
        ("include_events", `List []);
        ("exclude_events", `List []);
        ("include_repos", `List []);
        ("exclude_repos", `List []);
        ( "pr",
          `Assoc
            [
              ( "base_branch",
                `Assoc
                  [
                    ("op", `String "glob");
                    ("values", `List [ `String "feature/*" ]);
                  ] );
              ("draft", `Assoc [ ("op", `String "is"); ("value", `Bool true) ]);
            ] );
        ( "issue",
          `Assoc
            [
              ( "milestone",
                `Assoc
                  [ ("op", `String "eq"); ("values", `List [ `String "M1" ]) ]
              );
            ] );
      ]
  in
  let f = assert_ok (F.of_json j) in
  (match f.pr.base_branch with
  | Some { op = `Glob; values = [ "feature/*" ] } -> ()
  | _ -> Alcotest.fail "base_branch glob");
  (match f.pr.draft with Some true -> () | _ -> Alcotest.fail "draft true");
  match f.issue.milestone with
  | Some { op = `Eq; values = [ "M1" ] } -> ()
  | _ -> Alcotest.fail "milestone"

let test_store_persists_schema_version_and_advanced () =
  let db = Sqlite3.db_open ":memory:" in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      S.ensure_schema db;
      let filter : S.event_filter =
        {
          F.default with
          include_events = [ "pull_request" ];
          pr =
            {
              F.empty_pr with
              head_branch = Some { op = `Glob; values = [ "clawq/*" ] };
              draft = Some false;
            };
          issue =
            {
              F.empty_issue with
              labels = Some { op = `In; values = [ "bug" ] };
            };
        }
      in
      let r =
        assert_ok
          (S.create ~db ~id:"rt_adv" ~destination:(S.Room "r1")
             ~selector:(S.Repo "acme/widget") ~filter ~now:1_700_000_000.0 ())
      in
      Alcotest.(check int) "created version" 1 r.filter.schema_version;
      match S.get ~db ~id:r.id with
      | Error e -> Alcotest.fail e
      | Ok None -> Alcotest.fail "missing"
      | Ok (Some got) -> (
          Alcotest.(check int) "loaded version" 1 got.filter.schema_version;
          Alcotest.(check bool) "advanced" true (F.has_advanced got.filter);
          (match got.filter.pr.head_branch with
          | Some { op = `Glob; values = [ "clawq/*" ] } -> ()
          | _ -> Alcotest.fail "head_branch");
          match got.filter.issue.labels with
          | Some { op = `In; values = [ "bug" ] } -> ()
          | _ -> Alcotest.fail "labels"))

let test_store_loads_legacy_v0_filter_json () =
  let db = Sqlite3.db_open ":memory:" in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      S.ensure_schema db;
      (* Insert a route with pre-versioned (v0) filter JSON as stored by P19. *)
      let v0_filter =
        Yojson.Safe.to_string
          (`Assoc
             [
               ("include_events", `List []);
               ("exclude_events", `List []);
               ("include_repos", `List []);
               ("exclude_repos", `List []);
             ])
      in
      let sql =
        {|INSERT INTO github_routes
          (id, destination_key, destination_kind, destination_id, selector_key,
           selector_json, filter_json, comment_mode, capability_policy_json,
           enabled, revision, managed_bundle_id, managed_feature_id,
           provenance_json, created_at, updated_at)
          VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, NULL, NULL, ?, ?, ?)|}
      in
      let stmt = Sqlite3.prepare db sql in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          let bind i v = ignore (Sqlite3.bind stmt i v) in
          bind 1 (Sqlite3.Data.TEXT "rt_legacy");
          bind 2 (Sqlite3.Data.TEXT "room:r1");
          bind 3 (Sqlite3.Data.TEXT "room");
          bind 4 (Sqlite3.Data.TEXT "r1");
          bind 5 (Sqlite3.Data.TEXT "repo:acme/widget");
          bind 6 (Sqlite3.Data.TEXT {|{"type":"repo","repo":"acme/widget"}|});
          bind 7 (Sqlite3.Data.TEXT v0_filter);
          bind 8 (Sqlite3.Data.TEXT "summary");
          bind 9
            (Sqlite3.Data.TEXT
               {|{"allow_reply":false,"allow_label":false,"allow_assign":false,"allow_review":false,"allow_merge":false,"allow_close":false,"extra":{}}|});
          bind 10 (Sqlite3.Data.INT 1L);
          bind 11 (Sqlite3.Data.TEXT "1");
          bind 12 (Sqlite3.Data.TEXT "{}");
          bind 13 (Sqlite3.Data.TEXT "2024-01-01T00:00:00Z");
          bind 14 (Sqlite3.Data.TEXT "2024-01-01T00:00:00Z");
          match Sqlite3.step stmt with
          | Sqlite3.Rc.DONE -> ()
          | rc -> Alcotest.fail ("insert legacy: " ^ Sqlite3.Rc.to_string rc));
      match S.get ~db ~id:"rt_legacy" with
      | Error e -> Alcotest.fail e
      | Ok None -> Alcotest.fail "missing legacy"
      | Ok (Some got) ->
          Alcotest.(check int) "migrated on read" 1 got.filter.schema_version;
          Alcotest.(check (list string))
            "empty include as-is" [] got.filter.include_events;
          Alcotest.(check bool) "no advanced" false (F.has_advanced got.filter))

let test_store_default_filter_alias () =
  Alcotest.(check int) "store default version" 1 S.default_filter.schema_version;
  Alcotest.(check bool)
    "same empty include" true
    (S.default_filter.include_events = [])

let suite =
  [
    ("default is v1 empty advanced", `Quick, test_default_is_v1_empty);
    ( "migrate_v0_to_v1 keeps empty include/exclude",
      `Quick,
      test_migrate_v0_keeps_empty_include_exclude );
    ("of_json v0 baseline migrates", `Quick, test_of_json_v0_baseline_migrates);
    ("of_json v0 empty stays empty", `Quick, test_of_json_v0_empty_stays_empty);
    ( "roundtrip advanced PR and Issue fields",
      `Quick,
      test_roundtrip_advanced_pr_and_issue );
    ( "validate rejects bad operators/values",
      `Quick,
      test_validate_rejects_bad_operators_and_values );
    ("reject raw JSON predicates", `Quick, test_reject_raw_json_predicates);
    ( "advanced wrapper parses and is exclusive",
      `Quick,
      test_advanced_wrapper_parses_and_is_exclusive );
    ( "noncanonical advanced keys are rejected",
      `Quick,
      test_noncanonical_advanced_keys_are_rejected );
    ( "noncanonical nested filter keys are rejected",
      `Quick,
      test_noncanonical_nested_filter_keys_are_rejected );
    ("validate ops from json", `Quick, test_validate_ops_from_json);
    ( "store persists schema_version and advanced",
      `Quick,
      test_store_persists_schema_version_and_advanced );
    ( "store loads legacy v0 filter_json unchanged baseline",
      `Quick,
      test_store_loads_legacy_v0_filter_json );
    ( "store default_filter is versioned",
      `Quick,
      test_store_default_filter_alias );
  ]
