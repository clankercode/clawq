(** Tests for Item > Repo > Org no-fallthrough route matching (P19.M2.E2.T003).
*)

module S = Github_route_store
module E = Github_event_envelope
module M = Github_route_match

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  S.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_700_000_000.0
let room = S.Room "room-teams-1"
let other_room = S.Room "room-other"
let session = S.Session "teams:room-teams-1:alice"

let item_pr =
  S.Item { repo_full_name = "Acme/Widget"; kind = `Pull_request; number = 42 }

let item_issue =
  S.Item { repo_full_name = "acme/widget"; kind = `Issue; number = 7 }

let repo_sel = S.Repo "Acme/Widget"
let org_sel = S.Org "Acme"
let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let create ~db ?(id = "route-1") ?(enabled = true) ?(selector = item_pr)
    ?(destination = room) ?(filter = S.default_filter) () =
  assert_ok
    (S.create ~db ~id ~destination ~selector ~filter ~enabled ~now:fixed_now ())

let make_envelope ?(event = "pull_request") ?(action = Some "opened")
    ?(repo = "acme/widget") ?(org = Some "acme") ?(kind = Some E.Pull_request)
    ?(number = Some 42) ?(family = E.Lifecycle) ?(delivery_id = Some "deliv-1")
    () : E.t =
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
    actor = E.empty_actor;
    before = None;
    after = None;
    transfer = None;
    received_at = Some "2024-01-01T00:00:00Z";
    event_at = None;
    head_sha = None;
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
  | M.Matched { route; specificity } ->
      Alcotest.failf "expected No_route, got Matched id=%s spec=%s" route.id
        (specificity_str specificity)
  | M.Muted { route; specificity; reason } ->
      Alcotest.failf "expected No_route, got Muted id=%s spec=%s reason=%s"
        route.id
        (specificity_str specificity)
        reason

(* 1. Only Org → Matched Org *)
let test_only_org_matched () =
  with_db @@ fun db ->
  ignore (create ~db ~id:"rt_org" ~selector:org_sel ());
  let env = make_envelope () in
  expect_matched ~id:"rt_org" ~spec:`Org
    (M.resolve ~db ~destination:room ~envelope:env ())

(* 2. Item + Org both exist, Item enabled → Matched Item (not Org) *)
let test_item_wins_over_org () =
  with_db @@ fun db ->
  ignore (create ~db ~id:"rt_org" ~selector:org_sel ());
  ignore (create ~db ~id:"rt_item" ~selector:item_pr ());
  let env = make_envelope () in
  expect_matched ~id:"rt_item" ~spec:`Item
    (M.resolve ~db ~destination:room ~envelope:env ())

(* 3. Item disabled + Org enabled → Muted Item (NO Org fallthrough) *)
let test_disabled_item_no_org_fallthrough () =
  with_db @@ fun db ->
  ignore (create ~db ~id:"rt_org" ~selector:org_sel ~enabled:true ());
  ignore (create ~db ~id:"rt_item" ~selector:item_pr ~enabled:false ());
  let env = make_envelope () in
  expect_muted ~id:"rt_item" ~spec:`Item ~reason_contains:"disabled"
    (M.resolve ~db ~destination:room ~envelope:env ())

(* 4. Item enabled but filter excludes event → Muted Item (no Org fallthrough) *)
let test_filtered_item_no_org_fallthrough () =
  with_db @@ fun db ->
  ignore (create ~db ~id:"rt_org" ~selector:org_sel ());
  ignore
    (create ~db ~id:"rt_item" ~selector:item_pr
       ~filter:
         {
           S.include_events = [];
           exclude_events = [ "pull_request" ];
           include_repos = [];
           exclude_repos = [];
         }
       ());
  let env = make_envelope ~event:"pull_request" () in
  expect_muted ~id:"rt_item" ~spec:`Item ~reason_contains:"filter"
    (M.resolve ~db ~destination:room ~envelope:env ())

(* 5. Repo + Org: Repo wins when present *)
let test_repo_wins_over_org () =
  with_db @@ fun db ->
  ignore (create ~db ~id:"rt_org" ~selector:org_sel ());
  ignore (create ~db ~id:"rt_repo" ~selector:repo_sel ());
  let env = make_envelope () in
  expect_matched ~id:"rt_repo" ~spec:`Repo
    (M.resolve ~db ~destination:room ~envelope:env ())

(* 6. No routes → No_route *)
let test_no_routes () =
  with_db @@ fun db ->
  let env = make_envelope () in
  expect_no_route (M.resolve ~db ~destination:room ~envelope:env ())

(* 7. Wrong destination's routes ignored *)
let test_wrong_destination_ignored () =
  with_db @@ fun db ->
  ignore
    (create ~db ~id:"rt_other" ~destination:other_room ~selector:org_sel ());
  ignore (create ~db ~id:"rt_sess" ~destination:session ~selector:item_pr ());
  let env = make_envelope () in
  expect_no_route (M.resolve ~db ~destination:room ~envelope:env ());
  (* Other destination still matches its own routes. *)
  expect_matched ~id:"rt_other" ~spec:`Org
    (M.resolve ~db ~destination:other_room ~envelope:env ())

(* 8. filter include/exclude behavior *)
let test_filter_include_exclude () =
  let env_pr = make_envelope ~event:"pull_request" ~family:E.Lifecycle () in
  let env_comment =
    make_envelope ~event:"issue_comment" ~family:E.Comment
      ~action:(Some "created") ()
  in
  (* empty include + empty exclude → allow *)
  Alcotest.(check bool)
    "default allow" true
    (M.filter_allows S.default_filter env_pr);
  (* exclude wins *)
  let excl =
    {
      S.include_events = [ "pull_request" ];
      exclude_events = [ "pull_request" ];
      include_repos = [];
      exclude_repos = [];
    }
  in
  Alcotest.(check bool) "exclude wins" false (M.filter_allows excl env_pr);
  (* non-empty include requires membership *)
  let only_comment =
    {
      S.include_events = [ "issue_comment" ];
      exclude_events = [];
      include_repos = [];
      exclude_repos = [];
    }
  in
  Alcotest.(check bool)
    "include miss" false
    (M.filter_allows only_comment env_pr);
  Alcotest.(check bool)
    "include hit" true
    (M.filter_allows only_comment env_comment);
  (* family string matching *)
  let by_family =
    {
      S.include_events = [ "lifecycle" ];
      exclude_events = [];
      include_repos = [];
      exclude_repos = [];
    }
  in
  Alcotest.(check bool) "family include" true (M.filter_allows by_family env_pr);
  let excl_family =
    {
      S.include_events = [];
      exclude_events = [ "comment" ];
      include_repos = [];
      exclude_repos = [];
    }
  in
  Alcotest.(check bool)
    "family exclude" false
    (M.filter_allows excl_family env_comment)

(* 9. Org include_repos narrows *)
let test_org_include_repos_narrows () =
  with_db @@ fun db ->
  ignore
    (create ~db ~id:"rt_org" ~selector:org_sel
       ~filter:
         {
           S.include_events = [];
           exclude_events = [];
           include_repos = [ "acme/widget" ];
           exclude_repos = [];
         }
       ());
  let env_ok = make_envelope ~repo:"acme/widget" () in
  expect_matched ~id:"rt_org" ~spec:`Org
    (M.resolve ~db ~destination:room ~envelope:env_ok ());
  let env_other = make_envelope ~repo:"acme/other" () in
  expect_muted ~id:"rt_org" ~spec:`Org ~reason_contains:"repo"
    (M.resolve ~db ~destination:room ~envelope:env_other ());
  (* exclude_repos also mutes *)
  with_db @@ fun db ->
  ignore
    (create ~db ~id:"rt_org2" ~selector:org_sel
       ~filter:
         {
           S.include_events = [];
           exclude_events = [];
           include_repos = [];
           exclude_repos = [ "acme/widget" ];
         }
       ());
  expect_muted ~id:"rt_org2" ~spec:`Org ~reason_contains:"repo"
    (M.resolve ~db ~destination:room ~envelope:env_ok ())

(* 10. PR vs Issue item selectors distinct *)
let test_pr_vs_issue_item_distinct () =
  with_db @@ fun db ->
  ignore (create ~db ~id:"rt_pr" ~selector:item_pr ());
  ignore (create ~db ~id:"rt_issue" ~selector:item_issue ());
  ignore (create ~db ~id:"rt_org" ~selector:org_sel ());
  let env_pr = make_envelope ~kind:(Some E.Pull_request) ~number:(Some 42) () in
  expect_matched ~id:"rt_pr" ~spec:`Item
    (M.resolve ~db ~destination:room ~envelope:env_pr ());
  let env_issue =
    make_envelope ~event:"issues" ~kind:(Some E.Issue) ~number:(Some 7) ()
  in
  expect_matched ~id:"rt_issue" ~spec:`Item
    (M.resolve ~db ~destination:room ~envelope:env_issue ());
  (* Different PR number does not match Item PR 42; falls to Org (no Repo). *)
  let env_other_pr =
    make_envelope ~kind:(Some E.Pull_request) ~number:(Some 99) ()
  in
  expect_matched ~id:"rt_org" ~spec:`Org
    (M.resolve ~db ~destination:room ~envelope:env_other_pr ())

(* Extra: disabled Repo mutes Org (same no-fallthrough at Repo level) *)
let test_disabled_repo_no_org_fallthrough () =
  with_db @@ fun db ->
  ignore (create ~db ~id:"rt_org" ~selector:org_sel ());
  ignore (create ~db ~id:"rt_repo" ~selector:repo_sel ~enabled:false ());
  let env = make_envelope () in
  expect_muted ~id:"rt_repo" ~spec:`Repo ~reason_contains:"disabled"
    (M.resolve ~db ~destination:room ~envelope:env ())

(* Item > Repo > Org full stack: Item present wins *)
let test_item_over_repo_over_org () =
  with_db @@ fun db ->
  ignore (create ~db ~id:"rt_org" ~selector:org_sel ());
  ignore (create ~db ~id:"rt_repo" ~selector:repo_sel ());
  ignore (create ~db ~id:"rt_item" ~selector:item_pr ());
  let env = make_envelope () in
  expect_matched ~id:"rt_item" ~spec:`Item
    (M.resolve ~db ~destination:room ~envelope:env ())

(* Same delivery+item yields single Matched decision (at-most-one accept) *)
let test_at_most_one_matched_decision () =
  with_db @@ fun db ->
  ignore (create ~db ~id:"rt_org" ~selector:org_sel ());
  ignore (create ~db ~id:"rt_repo" ~selector:repo_sel ());
  ignore (create ~db ~id:"rt_item" ~selector:item_pr ());
  let env = make_envelope ~delivery_id:(Some "same-delivery") () in
  let d1 = M.resolve ~db ~destination:room ~envelope:env () in
  let d2 = M.resolve ~db ~destination:room ~envelope:env () in
  (match (d1, d2) with
  | M.Matched { route = r1; _ }, M.Matched { route = r2; _ } ->
      Alcotest.(check string) "same winner" r1.id r2.id;
      Alcotest.(check string) "item route" "rt_item" r1.id
  | _ -> Alcotest.fail "expected Matched twice");
  (* Only one accepted route id appears — not multi-match fanout. *)
  match d1 with
  | M.Matched _ -> ()
  | _ -> Alcotest.fail "expected single Matched"

let suite =
  [
    ("only Org matched", `Quick, test_only_org_matched);
    ("Item wins over Org", `Quick, test_item_wins_over_org);
    ( "disabled Item no Org fallthrough",
      `Quick,
      test_disabled_item_no_org_fallthrough );
    ( "filtered Item no Org fallthrough",
      `Quick,
      test_filtered_item_no_org_fallthrough );
    ("Repo wins over Org", `Quick, test_repo_wins_over_org);
    ("no routes", `Quick, test_no_routes);
    ("wrong destination ignored", `Quick, test_wrong_destination_ignored);
    ("filter include/exclude", `Quick, test_filter_include_exclude);
    ("Org include_repos narrows", `Quick, test_org_include_repos_narrows);
    ("PR vs Issue item distinct", `Quick, test_pr_vs_issue_item_distinct);
    ( "disabled Repo no Org fallthrough",
      `Quick,
      test_disabled_repo_no_org_fallthrough );
    ("Item over Repo over Org", `Quick, test_item_over_repo_over_org);
    ("at most one Matched decision", `Quick, test_at_most_one_matched_decision);
  ]
