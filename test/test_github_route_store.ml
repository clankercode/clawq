(** Tests for durable GitHub Item/Repo/Org route store (P19.M2.E2.T002). *)

module S = Github_route_store

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  S.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_700_000_000.0
let room = S.Room "room-teams-1"
let session = S.Session "teams:room-teams-1:alice"

let item_pr =
  S.Item { repo_full_name = "Acme/Widget"; kind = `Pull_request; number = 42 }

let item_issue =
  S.Item { repo_full_name = "acme/widget"; kind = `Issue; number = 7 }

let repo_sel = S.Repo "Acme/Widget"
let org_sel = S.Org "Acme"
let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let create_room_item ?db ?(id = "route-1") ?(on_collision = `Reject)
    ?(enabled = true) ?(selector = item_pr) ?(destination = room)
    ?(comment_mode = S.default_comment_mode) ?(filter = S.default_filter)
    ?(capability_policy = S.default_capability_policy) ?provenance () =
  let run db =
    S.create ~db ~id ~destination ~selector ~filter ~comment_mode
      ~capability_policy ~enabled ~now:fixed_now ~on_collision ?provenance ()
  in
  match db with Some db -> run db | None -> with_db run

let test_create_get_roundtrip () =
  with_db @@ fun db ->
  let prov =
    {
      S.created_by = Some "alice";
      created_via = Some "cli";
      setup_plan_id = Some "plan-1";
      notes = Some "initial";
    }
  in
  let r =
    assert_ok
      (create_room_item ~db ~id:"rt_round" ~provenance:prov
         ~comment_mode:S.Threaded
         ~filter:
           {
             S.default_filter with
             include_events = [ "pull_request" ];
             exclude_events = [ "issue_comment" ];
             include_repos = [];
             exclude_repos = [ "acme/other" ];
           }
         ~capability_policy:
           {
             allow_reply = true;
             allow_label = true;
             allow_assign = false;
             allow_review = false;
             allow_merge = false;
             allow_close = false;
             extra = [ ("allow_react", true) ];
           }
         ())
  in
  Alcotest.(check string) "id" "rt_round" r.id;
  Alcotest.(check string)
    "dest key" "room:room-teams-1"
    (S.destination_key r.destination);
  Alcotest.(check string)
    "selector key" "item:acme/widget:pr:42"
    (S.canonical_selector_key r.selector);
  Alcotest.(check bool) "enabled" true r.enabled;
  Alcotest.(check string) "revision" "1" r.revision;
  Alcotest.(check string)
    "created_at"
    (Time_util.iso8601_utc ~t:fixed_now ())
    r.created_at;
  match S.get ~db ~id:r.id with
  | Error e -> Alcotest.fail e
  | Ok None -> Alcotest.fail "missing"
  | Ok (Some got) ->
      Alcotest.(check string) "roundtrip id" r.id got.id;
      Alcotest.(check string)
        "comment" "threaded"
        (match got.comment_mode with
        | Off -> "off"
        | Summary -> "summary"
        | Threaded -> "threaded");
      Alcotest.(check (list string))
        "include_events" [ "pull_request" ] got.filter.include_events;
      Alcotest.(check bool) "allow_reply" true got.capability_policy.allow_reply;
      Alcotest.(check bool) "allow_label" true got.capability_policy.allow_label;
      Alcotest.(check (option string))
        "created_by" (Some "alice") got.provenance.created_by;
      Alcotest.(check (option string))
        "setup_plan" (Some "plan-1") got.provenance.setup_plan_id

let test_reject_collision () =
  with_db @@ fun db ->
  ignore (assert_ok (create_room_item ~db ~id:"rt_a" ()));
  match create_room_item ~db ~id:"rt_b" ~on_collision:`Reject () with
  | Ok _ -> Alcotest.fail "expected collision reject"
  | Error msg ->
      Alcotest.(check bool)
        "mentions active/exists" true
        (Test_helpers.string_contains (String.lowercase_ascii msg) "already"
        || Test_helpers.string_contains (String.lowercase_ascii msg) "collision"
        )

let test_replace_collision () =
  with_db @@ fun db ->
  let old = assert_ok (create_room_item ~db ~id:"rt_old" ()) in
  let neu =
    assert_ok
      (create_room_item ~db ~id:"rt_new" ~on_collision:`Replace
         ~comment_mode:S.Off ())
  in
  Alcotest.(check string) "new id" "rt_new" neu.id;
  Alcotest.(check bool) "new enabled" true neu.enabled;
  (match S.get ~db ~id:old.id with
  | Ok (Some o) -> Alcotest.(check bool) "old disabled" false o.enabled
  | Ok None -> Alcotest.fail "old missing"
  | Error e -> Alcotest.fail e);
  match S.find_active ~db ~destination:room ~selector:item_pr with
  | Ok (Some a) -> Alcotest.(check string) "active is new" "rt_new" a.id
  | Ok None -> Alcotest.fail "no active"
  | Error e -> Alcotest.fail e

let test_update_revision_occ () =
  with_db @@ fun db ->
  let r = assert_ok (create_room_item ~db ~id:"rt_occ" ()) in
  let updated =
    assert_ok
      (S.update ~db ~id:r.id ~expected_revision:r.revision ~comment_mode:S.Off
         ~now:(fixed_now +. 1.) ())
  in
  Alcotest.(check string) "bumped" "2" updated.revision;
  Alcotest.(check bool)
    "mode off" true
    (match updated.comment_mode with Off -> true | _ -> false);
  match
    S.update ~db ~id:r.id ~expected_revision:r.revision ~enabled:false
      ~now:(fixed_now +. 2.) ()
  with
  | Ok _ -> Alcotest.fail "stale revision should fail"
  | Error msg ->
      Alcotest.(check bool)
        "conflict" true
        (Test_helpers.string_contains (String.lowercase_ascii msg) "revision"
        || Test_helpers.string_contains (String.lowercase_ascii msg) "conflict"
        )

let test_session_destination () =
  with_db @@ fun db ->
  let r =
    assert_ok
      (create_room_item ~db ~id:"rt_sess" ~destination:session
         ~selector:item_issue ())
  in
  Alcotest.(check string)
    "dest key" "session:teams:room-teams-1:alice"
    (S.destination_key r.destination);
  match S.find_active ~db ~destination:session ~selector:item_issue with
  | Ok (Some a) -> Alcotest.(check string) "found" "rt_sess" a.id
  | Ok None -> Alcotest.fail "not found"
  | Error e -> Alcotest.fail e

let test_repo_org_selector_keys () =
  Alcotest.(check string)
    "item pr" "item:acme/widget:pr:42"
    (S.canonical_selector_key item_pr);
  Alcotest.(check string)
    "item issue" "item:acme/widget:issue:7"
    (S.canonical_selector_key item_issue);
  Alcotest.(check string)
    "repo" "repo:acme/widget"
    (S.canonical_selector_key repo_sel);
  Alcotest.(check string) "org" "org:acme" (S.canonical_selector_key org_sel);
  Alcotest.(check bool)
    "distinct" true
    (S.canonical_selector_key item_pr <> S.canonical_selector_key repo_sel
    && S.canonical_selector_key repo_sel <> S.canonical_selector_key org_sel);
  with_db @@ fun db ->
  ignore (assert_ok (create_room_item ~db ~id:"rt_item" ~selector:item_pr ()));
  ignore (assert_ok (create_room_item ~db ~id:"rt_repo" ~selector:repo_sel ()));
  ignore (assert_ok (create_room_item ~db ~id:"rt_org" ~selector:org_sel ()));
  let rows = assert_ok (S.list_for_destination ~db ~destination:room) in
  Alcotest.(check int) "three routes" 3 (List.length rows)

let test_list_for_destination () =
  with_db @@ fun db ->
  ignore (assert_ok (create_room_item ~db ~id:"rt_1" ~selector:item_pr ()));
  ignore (assert_ok (create_room_item ~db ~id:"rt_2" ~selector:item_issue ()));
  ignore
    (assert_ok
       (create_room_item ~db ~id:"rt_s" ~destination:session ~selector:item_pr
          ()));
  let room_routes = assert_ok (S.list_for_destination ~db ~destination:room) in
  Alcotest.(check int) "room count" 2 (List.length room_routes);
  let ids =
    List.map (fun r -> r.S.id) room_routes |> List.sort String.compare
  in
  Alcotest.(check (list string)) "room ids" [ "rt_1"; "rt_2" ] ids;
  let sess_routes =
    assert_ok (S.list_for_destination ~db ~destination:session)
  in
  Alcotest.(check int) "session count" 1 (List.length sess_routes)

let test_disable_frees_slot () =
  with_db @@ fun db ->
  let r = assert_ok (create_room_item ~db ~id:"rt_dis" ()) in
  let _ =
    assert_ok
      (S.update ~db ~id:r.id ~expected_revision:r.revision ~enabled:false
         ~now:(fixed_now +. 1.) ())
  in
  (match S.find_active ~db ~destination:room ~selector:item_pr with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "should be no active"
  | Error e -> Alcotest.fail e);
  let neu =
    assert_ok (create_room_item ~db ~id:"rt_after" ~on_collision:`Reject ())
  in
  Alcotest.(check string) "new active" "rt_after" neu.id;
  Alcotest.(check bool) "enabled" true neu.enabled

let test_schema_idempotent () =
  with_db @@ fun db ->
  S.ensure_schema db;
  S.ensure_schema db;
  ignore (assert_ok (create_room_item ~db ~id:"rt_schema" ()))

let test_fields_persist () =
  with_db @@ fun db ->
  let filter =
    {
      S.default_filter with
      include_events = [ "issues"; "pull_request" ];
      exclude_events = [ "issue_comment" ];
      include_repos = [ "acme/a" ];
      exclude_repos = [ "acme/b" ];
    }
  in
  let caps =
    {
      S.allow_reply = true;
      allow_label = false;
      allow_assign = true;
      allow_review = false;
      allow_merge = true;
      allow_close = false;
      extra = [ ("allow_milestone", false); ("allow_react", true) ];
    }
  in
  let r =
    assert_ok
      (S.create ~db ~id:"rt_fields" ~destination:room ~selector:repo_sel ~filter
         ~comment_mode:S.Off ~capability_policy:caps
         ~managed_bundle_id:"bundle-1" ~managed_feature_id:"feat-1"
         ~now:fixed_now ())
  in
  match S.get ~db ~id:r.id with
  | Error e -> Alcotest.fail e
  | Ok None -> Alcotest.fail "missing"
  | Ok (Some got) ->
      Alcotest.(check (list string))
        "inc events" filter.include_events got.filter.include_events;
      Alcotest.(check (list string))
        "exc events" filter.exclude_events got.filter.exclude_events;
      Alcotest.(check (list string))
        "inc repos" filter.include_repos got.filter.include_repos;
      Alcotest.(check (list string))
        "exc repos" filter.exclude_repos got.filter.exclude_repos;
      Alcotest.(check bool) "reply" true got.capability_policy.allow_reply;
      Alcotest.(check bool) "assign" true got.capability_policy.allow_assign;
      Alcotest.(check bool) "merge" true got.capability_policy.allow_merge;
      Alcotest.(check (option string))
        "bundle" (Some "bundle-1") got.managed_bundle_id;
      Alcotest.(check (option string))
        "feature" (Some "feat-1") got.managed_feature_id;
      Alcotest.(check bool)
        "mode off" true
        (match got.comment_mode with Off -> true | _ -> false)

let index_exists db name =
  let sql = "SELECT 1 FROM sqlite_master WHERE type = 'index' AND name = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT name));
      Sqlite3.step stmt = Sqlite3.Rc.ROW)

let test_indexes_exist () =
  with_db @@ fun db ->
  Alcotest.(check bool)
    "active unique" true
    (index_exists db "idx_github_routes_active_dest_sel");
  Alcotest.(check bool)
    "destination idx" true
    (index_exists db "idx_github_routes_destination");
  Alcotest.(check bool)
    "selector idx" true
    (index_exists db "idx_github_routes_selector");
  (* Exercise indexes via successful filtered queries. *)
  ignore (assert_ok (create_room_item ~db ~id:"rt_idx" ()));
  ignore (assert_ok (S.find_active ~db ~destination:room ~selector:item_pr));
  ignore (assert_ok (S.list_for_destination ~db ~destination:room))

let test_defaults () =
  Alcotest.(check bool)
    "default comment summary" true
    (match S.default_comment_mode with Summary -> true | _ -> false);
  Alcotest.(check (list string))
    "empty include_events" [] S.default_filter.include_events;
  Alcotest.(check bool)
    "caps default off" false S.default_capability_policy.allow_merge

let suite =
  [
    ("create room+item get roundtrip", `Quick, test_create_get_roundtrip);
    ("reject collision same dest+selector", `Quick, test_reject_collision);
    ("replace collision disables old", `Quick, test_replace_collision);
    ("update revision OCC", `Quick, test_update_revision_occ);
    ("session destination", `Quick, test_session_destination);
    ("repo and org selector keys", `Quick, test_repo_org_selector_keys);
    ("list_for_destination", `Quick, test_list_for_destination);
    ("disable frees unique slot", `Quick, test_disable_frees_slot);
    ("ensure_schema idempotent", `Quick, test_schema_idempotent);
    ("comment_mode filter capability persist", `Quick, test_fields_persist);
    ("indexes exist", `Quick, test_indexes_exist);
    ("defaults", `Quick, test_defaults);
  ]
