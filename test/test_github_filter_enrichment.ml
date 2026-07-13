(** Tests for demand-driven path/team enrichment (P20.M1.E1.T002). *)

module F = Github_route_filter
module E = Github_event_envelope
module En = Github_filter_enrichment

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let make_envelope ?(event = "pull_request") ?(action = Some "opened")
    ?(repo = "acme/widget") ?(org = Some "acme") ?(kind = Some E.Pull_request)
    ?(number = Some 42) ?(family = E.Lifecycle) ?(delivery_id = Some "deliv-1")
    ?(installation_id = Some 99) ?(actor_login = Some "alice")
    ?(item_author = actor_login) ?(head_sha = Some "abc123") () : E.t =
  {
    version = E.envelope_version;
    delivery_id;
    installation_id;
    event;
    action;
    repo_full_name = repo;
    org;
    item_kind = kind;
    item_number = number;
    item_node_id = Some "PR_kwDOABC";
    item_url = Some "https://api.github.com/repos/acme/widget/pulls/42";
    html_url = Some "https://github.com/acme/widget/pull/42";
    family;
    actor = { E.empty_actor with login = actor_login };
    item_author;
    before = None;
    after =
      Some
        {
          E.empty_safe_state with
          title = Some "Add feature";
          state = Some "open";
          draft = Some false;
          head_sha;
        };
    transfer = None;
    received_at = Some "2024-01-01T00:00:00Z";
    event_at = Some "2024-01-01T00:00:00Z";
    head_sha;
    unsupported = false;
    skip_reason = None;
  }

let filter_with ?changed_path ?pr_team ?issue_team () : F.t =
  let pr =
    {
      F.empty_pr with
      changed_path =
        (match changed_path with
        | None -> None
        | Some globs -> Some { op = `Glob; values = globs });
      team =
        (match pr_team with
        | None -> None
        | Some teams -> Some { op = `In; values = teams });
    }
  in
  let issue =
    {
      F.empty_issue with
      team =
        (match issue_team with
        | None -> None
        | Some teams -> Some { op = `In; values = teams });
    }
  in
  assert_ok (F.validate { F.default with pr; issue })

(** 1. Demand detection mirrors filter helpers. *)
let test_demand_detection () =
  let d0 = En.demand_of_filter F.default in
  Alcotest.(check bool) "default no paths" false d0.need_paths;
  Alcotest.(check bool) "default no teams" false d0.need_teams;
  let d_paths =
    En.demand_of_filter (filter_with ~changed_path:[ "src/**" ] ())
  in
  Alcotest.(check bool) "paths demanded" true d_paths.need_paths;
  Alcotest.(check bool) "teams not demanded" false d_paths.need_teams;
  Alcotest.(check bool)
    "matches requires_changed_paths" true
    (F.requires_changed_paths (filter_with ~changed_path:[ "src/**" ] ()));
  let d_team =
    En.demand_of_filter (filter_with ~pr_team:[ "acme/backend" ] ())
  in
  Alcotest.(check bool) "paths not demanded" false d_team.need_paths;
  Alcotest.(check bool) "teams demanded" true d_team.need_teams;
  Alcotest.(check bool)
    "matches requires_team_membership" true
    (F.requires_team_membership (filter_with ~pr_team:[ "acme/backend" ] ()));
  let d_issue =
    En.demand_of_filter (filter_with ~issue_team:[ "acme/triage" ] ())
  in
  Alcotest.(check bool) "issue team demands membership" true d_issue.need_teams;
  let d_both =
    En.demand_of_filter
      (filter_with ~changed_path:[ "src/**" ] ~pr_team:[ "acme/backend" ] ())
  in
  Alcotest.(check bool) "both paths" true d_both.need_paths;
  Alcotest.(check bool) "both teams" true d_both.need_teams

(** 2. No fetch when nothing is demanded. *)
let test_no_fetch_when_not_demanded () =
  let path_calls = ref 0 in
  let team_calls = ref 0 in
  let fetch_paths ~envelope:_ =
    incr path_calls;
    Ok [ "src/a.ml" ]
  in
  let fetch_teams ~envelope:_ ~team_slugs:_ =
    incr team_calls;
    Ok [ "acme/backend" ]
  in
  let env = make_envelope () in
  let e =
    En.enrich ~filter:F.default ~envelope:env ~fetch_paths ~fetch_teams ()
  in
  Alcotest.(check int) "paths fetcher not called" 0 !path_calls;
  Alcotest.(check int) "teams fetcher not called" 0 !team_calls;
  Alcotest.(check bool) "paths None" true (e.paths = None);
  Alcotest.(check bool) "teams None" true (e.teams = None);
  Alcotest.(check bool) "complete" true e.complete;
  Alcotest.(check (list string)) "no reasons" [] e.reasons

(** 3. Paths demanded → fetch once; teams remain None. *)
let test_paths_only_fetches_paths () =
  let path_calls = ref 0 in
  let team_calls = ref 0 in
  let fetch_paths ~(envelope : E.t) =
    incr path_calls;
    Alcotest.(check string) "repo" "acme/widget" envelope.repo_full_name;
    Ok [ "src/a.ml"; "src/b.ml" ]
  in
  let fetch_teams ~envelope:_ ~team_slugs:_ =
    incr team_calls;
    Ok []
  in
  let filter = filter_with ~changed_path:[ "src/**" ] () in
  let e =
    En.enrich ~filter ~envelope:(make_envelope ()) ~fetch_paths ~fetch_teams ()
  in
  Alcotest.(check int) "paths called once" 1 !path_calls;
  Alcotest.(check int) "teams not called" 0 !team_calls;
  (match e.paths with
  | Some (Ok paths) ->
      Alcotest.(check (list string)) "paths" [ "src/a.ml"; "src/b.ml" ] paths
  | _ -> Alcotest.fail "expected Ok paths");
  Alcotest.(check bool) "teams None" true (e.teams = None);
  Alcotest.(check bool) "complete" true e.complete

(** 4. Teams demanded → fetch once; paths remain None. *)
let test_teams_only_fetches_teams () =
  let path_calls = ref 0 in
  let team_calls = ref 0 in
  let fetch_paths ~envelope:_ =
    incr path_calls;
    Ok []
  in
  let fetch_teams ~(envelope : E.t) ~team_slugs =
    incr team_calls;
    Alcotest.(check (option string))
      "item author" (Some "alice") envelope.item_author;
    Alcotest.(check (list string)) "slugs" [ "acme/backend" ] team_slugs;
    Ok [ "acme/backend" ]
  in
  let filter = filter_with ~pr_team:[ "acme/backend" ] () in
  let e =
    En.enrich ~filter ~envelope:(make_envelope ()) ~fetch_paths ~fetch_teams ()
  in
  Alcotest.(check int) "paths not called" 0 !path_calls;
  Alcotest.(check int) "teams called once" 1 !team_calls;
  (match e.teams with
  | Some (Ok teams) ->
      Alcotest.(check (list string)) "teams" [ "acme/backend" ] teams
  | _ -> Alcotest.fail "expected Ok teams");
  Alcotest.(check bool) "paths None" true (e.paths = None);
  Alcotest.(check bool) "complete" true e.complete

(** 5. Fetch failure → Error + incomplete (fail closed, never broad allow). *)
let test_fetch_failure_is_not_allow () =
  let fetch_paths ~envelope:_ = Error "rate_limited" in
  let filter = filter_with ~changed_path:[ "src/**" ] () in
  let e = En.enrich ~filter ~envelope:(make_envelope ()) ~fetch_paths () in
  (match e.paths with
  | Some (Error "rate_limited") -> ()
  | _ -> Alcotest.fail "expected rate_limited");
  Alcotest.(check bool) "not complete" false e.complete;
  Alcotest.(check bool) "demanded_ok false" false (En.demanded_ok e);
  Alcotest.(check (list string)) "reasons" [ "rate_limited" ] e.reasons

(** 6. Rate-limit gate short-circuits without calling fetcher. *)
let test_rate_limit_gate_skips_fetch () =
  let path_calls = ref 0 in
  let fetch_paths ~envelope:_ =
    incr path_calls;
    Ok [ "x" ]
  in
  let filter = filter_with ~changed_path:[ "**" ] () in
  let e =
    En.enrich ~filter ~envelope:(make_envelope ()) ~fetch_paths
      ~rate_limited:(fun () -> true)
      ()
  in
  Alcotest.(check int) "no fetch" 0 !path_calls;
  (match e.paths with
  | Some (Error "rate_limited") -> ()
  | _ -> Alcotest.fail "expected rate_limited");
  Alcotest.(check bool) "incomplete" false e.complete

(** 7. Access-scope gate short-circuits. *)
let test_access_denied_skips_fetch () =
  let team_calls = ref 0 in
  let fetch_teams ~envelope:_ ~team_slugs:_ =
    incr team_calls;
    Ok []
  in
  let filter = filter_with ~pr_team:[ "acme/backend" ] () in
  let e =
    En.enrich ~filter ~envelope:(make_envelope ()) ~fetch_teams
      ~access_allowed:(fun () -> false)
      ()
  in
  Alcotest.(check int) "no fetch" 0 !team_calls;
  (match e.teams with
  | Some (Error "access_denied") -> ()
  | _ -> Alcotest.fail "expected access_denied");
  Alcotest.(check bool) "incomplete" false e.complete

(** 8. Missing fetcher when demanded → fetcher_unavailable, fail closed. *)
let test_missing_fetcher_unavailable () =
  let filter = filter_with ~changed_path:[ "src/**" ] () in
  let e = En.enrich ~filter ~envelope:(make_envelope ()) () in
  (match e.paths with
  | Some (Error "fetcher_unavailable") -> ()
  | _ -> Alcotest.fail "expected fetcher_unavailable");
  Alcotest.(check bool) "incomplete" false e.complete

(** 9. Paths demand on non-PR → not_a_pr without fetch. *)
let test_not_a_pr_without_fetch () =
  let path_calls = ref 0 in
  let fetch_paths ~envelope:_ =
    incr path_calls;
    Ok []
  in
  let filter = filter_with ~changed_path:[ "src/**" ] () in
  let env =
    make_envelope ~event:"issues" ~kind:(Some E.Issue) ~number:(Some 7) ()
  in
  let e = En.enrich ~filter ~envelope:env ~fetch_paths () in
  Alcotest.(check int) "no fetch" 0 !path_calls;
  match e.paths with
  | Some (Error "not_a_pr") -> ()
  | _ -> Alcotest.fail "expected not_a_pr"

(** 10. Team demand without item author → missing_item_author. *)
let test_missing_item_author_without_fetch () =
  let team_calls = ref 0 in
  let fetch_teams ~envelope:_ ~team_slugs:_ =
    incr team_calls;
    Ok []
  in
  let filter = filter_with ~issue_team:[ "acme/triage" ] () in
  let env =
    make_envelope ~actor_login:(Some "webhook-sender") ~item_author:None ()
  in
  let e = En.enrich ~filter ~envelope:env ~fetch_teams () in
  Alcotest.(check int) "no fetch" 0 !team_calls;
  match e.teams with
  | Some (Error "missing_item_author") -> ()
  | _ -> Alcotest.fail "expected missing_item_author"

(** 11. Cache hit avoids second fetch (same install/repo/item revision). *)
let test_cache_hit_skips_second_fetch () =
  let path_calls = ref 0 in
  let fetch_paths ~envelope:_ =
    incr path_calls;
    Ok [ "src/cached.ml" ]
  in
  let cache = En.create_cache ~ttl_s:60.0 () in
  let filter = filter_with ~changed_path:[ "src/**" ] () in
  let env = make_envelope ~head_sha:(Some "rev1") () in
  let now = 1_700_000_000.0 in
  let e1 = En.enrich ~filter ~envelope:env ~fetch_paths ~cache ~now () in
  let e2 =
    En.enrich ~filter ~envelope:env ~fetch_paths ~cache ~now:(now +. 1.0) ()
  in
  Alcotest.(check int) "fetched once" 1 !path_calls;
  (match (e1.paths, e2.paths) with
  | Some (Ok [ "src/cached.ml" ]), Some (Ok [ "src/cached.ml" ]) -> ()
  | _ -> Alcotest.fail "expected cached paths");
  (* TTL expiry re-fetches. *)
  let e3 =
    En.enrich ~filter ~envelope:env ~fetch_paths ~cache ~now:(now +. 120.0) ()
  in
  Alcotest.(check int) "re-fetched after TTL" 2 !path_calls;
  match e3.paths with
  | Some (Ok [ "src/cached.ml" ]) -> ()
  | _ -> Alcotest.fail "expected paths after refresh"

(** 12. Cache key includes installation / repo / revision. *)
let test_cache_key_identity () =
  let env =
    make_envelope ~installation_id:(Some 7) ~head_sha:(Some "deadbeef") ()
  in
  let k = En.cache_key_paths env in
  Alcotest.(check bool) "has install" true (Test_helpers.string_contains k "7");
  Alcotest.(check bool)
    "has repo" true
    (Test_helpers.string_contains k "acme/widget");
  Alcotest.(check bool)
    "has rev" true
    (Test_helpers.string_contains k "deadbeef");
  Alcotest.(check string) "revision helper" "deadbeef" (En.item_revision env);
  let slugs = [ "acme/backend"; "acme/triage" ] in
  let tk = En.cache_key_teams env ~team_slugs:slugs in
  Alcotest.(check bool)
    "teams key has item author" true
    (Test_helpers.string_contains tk "alice")

(** 13. team_slugs_of_filter collects PR + Issue teams. *)
let test_team_slugs_collected () =
  let f =
    filter_with
      ~pr_team:[ "acme/backend"; "acme/backend" ]
      ~issue_team:[ "acme/triage" ] ()
  in
  let slugs = En.team_slugs_of_filter f in
  Alcotest.(check (list string))
    "deduped"
    [ "acme/backend"; "acme/triage" ]
    slugs

(** 14. Only paths demanded when both fetchers present but filter wants paths.
*)
let test_selective_demand_with_both_fetchers () =
  let path_calls = ref 0 in
  let team_calls = ref 0 in
  let fetch_paths ~envelope:_ =
    incr path_calls;
    Ok [ "docs/x.md" ]
  in
  let fetch_teams ~envelope:_ ~team_slugs:_ =
    incr team_calls;
    Ok [ "acme/docs" ]
  in
  (* labels alone do not demand path/team enrichment *)
  let filter =
    assert_ok
      (F.validate
         {
           F.default with
           pr =
             { F.empty_pr with labels = Some { op = `In; values = [ "bug" ] } };
         })
  in
  let e =
    En.enrich ~filter ~envelope:(make_envelope ()) ~fetch_paths ~fetch_teams ()
  in
  Alcotest.(check int) "no path fetch for labels-only" 0 !path_calls;
  Alcotest.(check int) "no team fetch for labels-only" 0 !team_calls;
  Alcotest.(check bool) "paths None" true (e.paths = None);
  Alcotest.(check bool) "teams None" true (e.teams = None);
  Alcotest.(check bool) "complete" true e.complete

let suite =
  [
    ("demand detection from filter helpers", `Quick, test_demand_detection);
    ("no fetch when not demanded", `Quick, test_no_fetch_when_not_demanded);
    ("paths only fetches paths", `Quick, test_paths_only_fetches_paths);
    ("teams only fetches teams", `Quick, test_teams_only_fetches_teams);
    ("fetch failure is not allow", `Quick, test_fetch_failure_is_not_allow);
    ("rate limit gate skips fetch", `Quick, test_rate_limit_gate_skips_fetch);
    ("access denied skips fetch", `Quick, test_access_denied_skips_fetch);
    ("missing fetcher unavailable", `Quick, test_missing_fetcher_unavailable);
    ("not a pr without fetch", `Quick, test_not_a_pr_without_fetch);
    ( "missing item author without fetch",
      `Quick,
      test_missing_item_author_without_fetch );
    ("cache hit skips second fetch", `Quick, test_cache_hit_skips_second_fetch);
    ("cache key install repo revision", `Quick, test_cache_key_identity);
    ("team slugs collected and deduped", `Quick, test_team_slugs_collected);
    ( "labels-only does not demand enrichment",
      `Quick,
      test_selective_demand_with_both_fetchers );
  ]
