(** Tests for indexed/cached advanced route matching (P20.M1.E2.T002). *)

module S = Github_route_store
module E = Github_event_envelope
module F = Github_route_filter
module En = Github_filter_enrichment
module M = Github_route_match
module A = Github_route_match_advanced

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  S.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_700_000_000.0
let room = S.Room "room-teams-1"
let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let create ~db ?(id = "route-1") ?(enabled = true)
    ?(selector = S.Repo "acme/widget") ?(destination = room)
    ?(filter = S.default_filter) () =
  assert_ok
    (S.create ~db ~id ~destination ~selector ~filter ~enabled ~now:fixed_now ())

let make_envelope ?(event = "pull_request") ?(action = Some "opened")
    ?(repo = "acme/widget") ?(org = Some "acme") ?(kind = Some E.Pull_request)
    ?(number = Some 42) ?(family = E.Lifecycle) ?(delivery_id = Some "deliv-1")
    ?(actor_login = Some "alice") ?(labels = [ "bug" ]) ?(draft = Some false)
    ?(base_ref = Some "main") ?(assignees = []) ?(milestone = None)
    ?(head_sha = Some "abc123") () : E.t =
  {
    version = E.envelope_version;
    delivery_id;
    installation_id = Some 99;
    event;
    action;
    repo_full_name = repo;
    org;
    item_kind = kind;
    item_number = number;
    item_node_id = None;
    item_url = None;
    html_url = None;
    family;
    actor = { E.empty_actor with login = actor_login };
    item_author = actor_login;
    before = None;
    after =
      Some
        {
          E.empty_safe_state with
          labels;
          draft;
          base_ref;
          assignees;
          milestone;
          state = Some "open";
        };
    transfer = None;
    received_at = Some "2024-01-01T00:00:00Z";
    event_at = None;
    head_sha;
    unsupported = false;
    skip_reason = None;
  }

let specificity_str = function
  | `Item -> "item"
  | `Repo -> "repo"
  | `Org -> "org"

let expect_matched ~id ~spec decision =
  match decision with
  | M.Matched { route; specificity } ->
      Alcotest.(check string) "matched id" id route.id;
      Alcotest.(check string)
        "matched specificity" (specificity_str spec)
        (specificity_str specificity)
  | M.Muted { route; specificity; reason } ->
      Alcotest.failf "expected Matched, got Muted id=%s spec=%s reason=%s"
        route.id
        (specificity_str specificity)
        reason
  | M.No_route -> Alcotest.fail "expected Matched, got No_route"

let expect_muted ~id ~spec ?reason_contains decision =
  match decision with
  | M.Muted { route; specificity; reason } -> (
      Alcotest.(check string) "muted id" id route.id;
      Alcotest.(check string)
        "muted specificity" (specificity_str spec)
        (specificity_str specificity);
      match reason_contains with
      | None -> ()
      | Some needle ->
          Alcotest.(check bool)
            ("reason contains " ^ needle)
            true
            (Test_helpers.string_contains
               (String.lowercase_ascii reason)
               (String.lowercase_ascii needle)))
  | M.Matched { route; specificity } ->
      Alcotest.failf "expected Muted, got Matched id=%s spec=%s" route.id
        (specificity_str specificity)
  | M.No_route -> Alcotest.fail "expected Muted, got No_route"

let expect_no_route decision =
  match decision with
  | M.No_route -> ()
  | M.Matched { route; _ } ->
      Alcotest.failf "expected No_route, got Matched id=%s" route.id
  | M.Muted { route; reason; _ } ->
      Alcotest.failf "expected No_route, got Muted id=%s reason=%s" route.id
        reason

let pr_label_filter labels =
  assert_ok
    (F.validate
       {
         F.default with
         pr = { F.empty_pr with labels = Some { op = `In; values = labels } };
       })

let pr_path_filter patterns =
  assert_ok
    (F.validate
       {
         F.default with
         pr =
           {
             F.empty_pr with
             changed_path = Some { op = `Glob; values = patterns };
           };
       })

let pr_team_filter teams =
  assert_ok
    (F.validate
       {
         F.default with
         pr = { F.empty_pr with team = Some { op = `In; values = teams } };
       })

(* 1. Baseline-only route still Matched (no advanced work). *)
let test_baseline_matched () =
  with_db @@ fun db ->
  ignore (create ~db ~id:"rt_repo" ~selector:(S.Repo "acme/widget") ());
  let env = make_envelope () in
  expect_matched ~id:"rt_repo" ~spec:`Repo
    (A.resolve ~db ~destination:room ~envelope:env ())

(* 2. Advanced PR labels pass → Matched. *)
let test_advanced_labels_matched () =
  with_db @@ fun db ->
  let filter = pr_label_filter [ "bug"; "enhancement" ] in
  ignore (create ~db ~id:"rt_repo" ~filter ());
  let env = make_envelope ~labels:[ "bug" ] () in
  expect_matched ~id:"rt_repo" ~spec:`Repo
    (A.resolve ~db ~destination:room ~envelope:env ())

(* 3. Advanced PR labels fail → Muted (no Org fallthrough). *)
let test_advanced_labels_muted_no_fallthrough () =
  with_db @@ fun db ->
  ignore (create ~db ~id:"rt_org" ~selector:(S.Org "acme") ());
  let filter = pr_label_filter [ "security" ] in
  ignore (create ~db ~id:"rt_repo" ~selector:(S.Repo "acme/widget") ~filter ());
  let env = make_envelope ~labels:[ "bug" ] () in
  expect_muted ~id:"rt_repo" ~spec:`Repo ~reason_contains:"pr.labels"
    (A.resolve ~db ~destination:room ~envelope:env ())

(* 4. Demanded path enrichment missing → fail closed Muted. *)
let test_missing_path_enrichment_fail_closed () =
  with_db @@ fun db ->
  let filter = pr_path_filter [ "src/**" ] in
  ignore (create ~db ~id:"rt_repo" ~filter ());
  let env = make_envelope () in
  (* No enrichment, no fetcher → incomplete / fail closed. *)
  expect_muted ~id:"rt_repo" ~spec:`Repo ~reason_contains:"enrichment"
    (A.resolve ~db ~destination:room ~envelope:env ())

(* 5. Path enrichment Error (rate limited) → fail closed. *)
let test_path_enrichment_error_fail_closed () =
  with_db @@ fun db ->
  let filter = pr_path_filter [ "src/**" ] in
  ignore (create ~db ~id:"rt_repo" ~filter ());
  let env = make_envelope () in
  let enrichment : En.enrichment =
    {
      paths = Some (Error "rate_limited");
      teams = None;
      reasons = [ "rate_limited" ];
      complete = false;
    }
  in
  expect_muted ~id:"rt_repo" ~spec:`Repo ~reason_contains:"rate_limited"
    (A.resolve ~db ~destination:room ~envelope:env ~enrichment ())

(* 6. Path enrichment Ok and path matches → Matched. *)
let test_path_enrichment_matched () =
  with_db @@ fun db ->
  let filter = pr_path_filter [ "src/**" ] in
  ignore (create ~db ~id:"rt_repo" ~filter ());
  let env = make_envelope () in
  let enrichment : En.enrichment =
    {
      paths = Some (Ok [ "src/main.ml"; "README.md" ]);
      teams = None;
      reasons = [];
      complete = true;
    }
  in
  expect_matched ~id:"rt_repo" ~spec:`Repo
    (A.resolve ~db ~destination:room ~envelope:env ~enrichment ())

(* 7. Team demanded with missing enrichment → fail closed. *)
let test_team_enrichment_missing_fail_closed () =
  with_db @@ fun db ->
  let filter = pr_team_filter [ "platform" ] in
  ignore (create ~db ~id:"rt_org" ~selector:(S.Org "acme") ~filter ());
  let env = make_envelope () in
  expect_muted ~id:"rt_org" ~spec:`Org ~reason_contains:"enrichment"
    (A.resolve ~db ~destination:room ~envelope:env ())

(* 8. Index: build + candidates for repo/org; resolve_with index agrees. *)
let test_index_candidates_and_resolve () =
  with_db @@ fun db ->
  ignore (create ~db ~id:"rt_org" ~selector:(S.Org "acme") ());
  ignore (create ~db ~id:"rt_repo" ~selector:(S.Repo "acme/widget") ());
  ignore (create ~db ~id:"rt_other" ~selector:(S.Repo "other/repo") ());
  ignore
    (create ~db ~id:"rt_item"
       ~selector:
         (S.Item
            {
              repo_full_name = "acme/widget";
              kind = `Pull_request;
              number = 42;
            })
       ());
  let idx = assert_ok (A.build_index_from_db ~db ~destination:room) in
  Alcotest.(check int) "index size" 4 (A.index_size idx);
  let env = make_envelope () in
  let cands = A.index_candidates idx ~envelope:env in
  let ids =
    List.map (fun (r : S.t) -> r.id) cands |> List.sort String.compare
  in
  Alcotest.(check (list string))
    "candidates exclude other repo"
    [ "rt_item"; "rt_org"; "rt_repo" ]
    ids;
  expect_matched ~id:"rt_item" ~spec:`Item
    (A.resolve ~db ~destination:room ~envelope:env ~index:idx ())

(* 9. Index cache hit avoids rebuild; invalidate drops entry. *)
let test_index_cache () =
  with_db @@ fun db ->
  ignore (create ~db ~id:"rt_repo" ());
  let cache = A.create_index_cache ~ttl_s:60.0 () in
  let idx1 =
    assert_ok
      (A.get_or_build_index ~cache ~db ~destination:room ~now:fixed_now ())
  in
  Alcotest.(check int) "size" 1 (A.index_size idx1);
  (* Add another route after cache build — cache still serves old index. *)
  ignore (create ~db ~id:"rt_org" ~selector:(S.Org "acme") ());
  let idx2 =
    assert_ok
      (A.get_or_build_index ~cache ~db ~destination:room ~now:fixed_now ())
  in
  Alcotest.(check int) "stale cache size" 1 (A.index_size idx2);
  A.invalidate_index ~cache ~destination:room;
  let idx3 =
    assert_ok
      (A.get_or_build_index ~cache ~db ~destination:room ~now:fixed_now ())
  in
  Alcotest.(check int) "rebuilt size" 2 (A.index_size idx3);
  let env = make_envelope () in
  expect_matched ~id:"rt_repo" ~spec:`Repo
    (A.resolve ~db ~destination:room ~envelope:env ~index_cache:cache
       ~now:fixed_now ())

(* 10. Enrichment cache used by obtain_enrichment / resolve. *)
let test_enrichment_cache_on_resolve () =
  with_db @@ fun db ->
  let filter = pr_path_filter [ "lib/**" ] in
  ignore (create ~db ~id:"rt_repo" ~filter ());
  let env = make_envelope () in
  let calls = ref 0 in
  let fetch_paths ~envelope:_ =
    incr calls;
    Ok [ "lib/foo.ml" ]
  in
  let cache = En.create_cache ~ttl_s:60.0 () in
  expect_matched ~id:"rt_repo" ~spec:`Repo
    (A.resolve ~db ~destination:room ~envelope:env ~fetch_paths ~cache
       ~now:fixed_now ());
  expect_matched ~id:"rt_repo" ~spec:`Repo
    (A.resolve ~db ~destination:room ~envelope:env ~fetch_paths ~cache
       ~now:fixed_now ());
  Alcotest.(check int) "fetcher called once (cache hit)" 1 !calls

(* 11. try_accept with advanced mute does not record. *)
let test_try_accept_advanced_muted () =
  with_db @@ fun db ->
  M.ensure_schema db;
  let filter = pr_label_filter [ "security" ] in
  ignore (create ~db ~id:"rt_repo" ~filter ());
  let env = make_envelope ~labels:[ "bug" ] ~delivery_id:(Some "d-adv") () in
  match A.try_accept ~db ~destination:room ~envelope:env ~now:fixed_now () with
  | M.Not_accepted (M.Muted _) -> ()
  | M.Accepted _ -> Alcotest.fail "advanced mute must not accept"
  | M.Duplicate _ -> Alcotest.fail "must not duplicate on first muted"
  | M.Not_accepted _ -> Alcotest.fail "expected Muted Not_accepted"

(* 12. try_accept advanced match is durable / idempotent. *)
let test_try_accept_advanced_matched () =
  with_db @@ fun db ->
  M.ensure_schema db;
  let filter = pr_label_filter [ "bug" ] in
  ignore (create ~db ~id:"rt_repo" ~filter ());
  let env = make_envelope ~labels:[ "bug" ] ~delivery_id:(Some "d-ok") () in
  (match A.try_accept ~db ~destination:room ~envelope:env ~now:fixed_now () with
  | M.Accepted (M.Matched { route; _ }) ->
      Alcotest.(check string) "accepted id" "rt_repo" route.id
  | _ -> Alcotest.fail "expected Accepted Matched");
  match A.try_accept ~db ~destination:room ~envelope:env ~now:fixed_now () with
  | M.Duplicate { delivery_id; _ } ->
      Alcotest.(check string) "delivery" "d-ok" delivery_id
  | _ -> Alcotest.fail "second accept must be Duplicate"

(* 13. Disabled most-specific still mutes with advanced present on Org. *)
let test_disabled_item_no_fallthrough_with_advanced () =
  with_db @@ fun db ->
  let filter = pr_label_filter [ "bug" ] in
  ignore (create ~db ~id:"rt_org" ~selector:(S.Org "acme") ~filter ());
  ignore
    (create ~db ~id:"rt_item"
       ~selector:
         (S.Item
            {
              repo_full_name = "acme/widget";
              kind = `Pull_request;
              number = 42;
            })
       ~enabled:false ());
  let env = make_envelope ~labels:[ "bug" ] () in
  expect_muted ~id:"rt_item" ~spec:`Item ~reason_contains:"disabled"
    (A.resolve ~db ~destination:room ~envelope:env ())

(* 14. advanced_allows unit: empty advanced Ok; missing team Error. *)
let test_advanced_allows_unit () =
  let env = make_envelope () in
  (match
     A.advanced_allows ~filter:F.default ~envelope:env
       ~enrichment:En.empty_enrichment ()
   with
  | Ok () -> ()
  | Error e -> Alcotest.failf "empty advanced should allow: %s" e);
  let filter = pr_team_filter [ "platform" ] in
  let incomplete : En.enrichment =
    {
      paths = None;
      teams = Some (Error "access_denied");
      reasons = [ "access_denied" ];
      complete = false;
    }
  in
  match A.advanced_allows ~filter ~envelope:env ~enrichment:incomplete () with
  | Error reason ->
      Alcotest.(check bool)
        "mentions enrichment" true
        (Test_helpers.string_contains
           (String.lowercase_ascii reason)
           "enrichment")
  | Ok () -> Alcotest.fail "incomplete team enrichment must reject"

let test_normalized_item_author_and_head_ref_drive_advanced_filters () =
  with_db @@ fun db ->
  let filter =
    assert_ok
      (F.validate
         {
           F.default with
           pr =
             {
               F.empty_pr with
               head_branch = Some { op = `Glob; values = [ "feature/**" ] };
               author = Some { op = `Eq; values = [ "item-author" ] };
             };
         })
  in
  ignore (create ~db ~id:"rt-normalized-pr" ~filter ());
  let payload =
    `Assoc
      [
        ( "repository",
          `Assoc
            [
              ("full_name", `String "acme/widget");
              ("owner", `Assoc [ ("login", `String "acme") ]);
            ] );
        ("action", `String "opened");
        ("sender", `Assoc [ ("login", `String "webhook-sender") ]);
        ( "pull_request",
          `Assoc
            [
              ("number", `Int 42);
              ("node_id", `String "PR_normalized");
              ("state", `String "open");
              ("user", `Assoc [ ("login", `String "item-author") ]);
              ("base", `Assoc [ ("ref", `String "main") ]);
              ("head", `Assoc [ ("ref", `String "feature/p20") ]);
            ] );
      ]
  in
  let envelope =
    match
      E.normalize ~delivery_id:"normalized-pr" ~event:"pull_request" ~payload ()
    with
    | E.Ok_envelope envelope -> envelope
    | E.Unsupported { reason; _ } | E.Error reason -> Alcotest.fail reason
  in
  Alcotest.(check (option string))
    "sender remains actor" (Some "webhook-sender") envelope.actor.login;
  Alcotest.(check (option string))
    "item author is independent" (Some "item-author") envelope.item_author;
  let roundtrip = assert_ok (E.of_safe_json (E.to_safe_json envelope)) in
  Alcotest.(check (option string))
    "safe JSON retains item author" (Some "item-author") roundtrip.item_author;
  Alcotest.(check (option string))
    "safe JSON retains head ref" (Some "feature/p20")
    (Option.bind roundtrip.after (fun state -> state.head_ref));
  expect_matched ~id:"rt-normalized-pr" ~spec:`Repo
    (A.resolve ~db ~destination:room ~envelope ());
  let issue_filter =
    assert_ok
      (F.validate
         {
           F.default with
           issue =
             {
               F.empty_issue with
               author = Some { op = `Eq; values = [ "issue-author" ] };
             };
         })
  in
  let issue_destination = S.Room "room-normalized-issue" in
  ignore
    (create ~db ~id:"rt-normalized-issue" ~destination:issue_destination
       ~filter:issue_filter ());
  let issue_payload =
    `Assoc
      [
        ( "repository",
          `Assoc
            [
              ("full_name", `String "acme/widget");
              ("owner", `Assoc [ ("login", `String "acme") ]);
            ] );
        ("action", `String "opened");
        ("sender", `Assoc [ ("login", `String "triage-bot") ]);
        ( "issue",
          `Assoc
            [
              ("number", `Int 7);
              ("node_id", `String "I_normalized");
              ("state", `String "open");
              ("user", `Assoc [ ("login", `String "issue-author") ]);
            ] );
      ]
  in
  let issue =
    match
      E.normalize ~delivery_id:"normalized-issue" ~event:"issues"
        ~payload:issue_payload ()
    with
    | E.Ok_envelope envelope -> envelope
    | E.Unsupported { reason; _ } | E.Error reason -> Alcotest.fail reason
  in
  Alcotest.(check (option string))
    "issue sender remains actor" (Some "triage-bot") issue.actor.login;
  expect_matched ~id:"rt-normalized-issue" ~spec:`Repo
    (A.resolve ~db ~destination:issue_destination ~envelope:issue ())

let suite =
  [
    ("baseline matched", `Quick, test_baseline_matched);
    ("advanced labels matched", `Quick, test_advanced_labels_matched);
    ( "advanced labels muted no fallthrough",
      `Quick,
      test_advanced_labels_muted_no_fallthrough );
    ( "missing path enrichment fail closed",
      `Quick,
      test_missing_path_enrichment_fail_closed );
    ( "path enrichment error fail closed",
      `Quick,
      test_path_enrichment_error_fail_closed );
    ("path enrichment matched", `Quick, test_path_enrichment_matched);
    ( "team enrichment missing fail closed",
      `Quick,
      test_team_enrichment_missing_fail_closed );
    ("index candidates and resolve", `Quick, test_index_candidates_and_resolve);
    ("index cache", `Quick, test_index_cache);
    ("enrichment cache on resolve", `Quick, test_enrichment_cache_on_resolve);
    ("try_accept advanced muted", `Quick, test_try_accept_advanced_muted);
    ("try_accept advanced matched", `Quick, test_try_accept_advanced_matched);
    ( "disabled item no fallthrough with advanced",
      `Quick,
      test_disabled_item_no_fallthrough_with_advanced );
    ("advanced_allows unit", `Quick, test_advanced_allows_unit);
    ( "normalized item author and head ref drive filters",
      `Quick,
      test_normalized_item_author_and_head_ref_drive_advanced_filters );
  ]
