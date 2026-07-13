(** Migration fixtures, rate-limit fail-closed behavior, and Org-scale budgets
    (P20.M1.E2.T003).

    - Baseline→advanced migration parity of match decisions
    - Coverage of every advanced PR+Issue predicate used by filter_eval
    - Deterministic rate-limit mute with explainable reasons (no
      allow-on-missing)
    - Documented Org-scale candidate/match/enrichment budgets (no live network)
*)

module S = Github_route_store
module E = Github_event_envelope
module F = Github_route_filter
module Ev = Github_route_filter_eval
module En = Github_filter_enrichment
module M = Github_route_match
module A = Github_route_match_advanced
module B = Github_route_match_bench
module P = Github_route_filter_preview

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  S.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_700_000_000.0
let room = S.Room "room-fixtures-1"
let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let create ~db ?(id = "route-1") ?(enabled = true)
    ?(selector = S.Repo "acme/widget") ?(destination = room)
    ?(filter = S.default_filter) () =
  assert_ok
    (S.create ~db ~id ~destination ~selector ~filter ~enabled ~now:fixed_now ())

let make_pr_envelope ?(event = "pull_request") ?(action = Some "opened")
    ?(repo = "acme/widget") ?(org = Some "acme") ?(number = 42)
    ?(delivery_id = Some "deliv-1") ?(actor_login = Some "alice")
    ?(labels = [ "bug" ]) ?(draft = Some false) ?(base_ref = Some "main")
    ?(assignees = []) ?(milestone = None) ?(head_sha = Some "abc123") () : E.t =
  {
    version = E.envelope_version;
    delivery_id;
    installation_id = Some 99;
    event;
    action;
    repo_full_name = repo;
    org;
    item_kind = Some E.Pull_request;
    item_number = Some number;
    item_node_id = None;
    item_url = None;
    html_url = None;
    family = E.Lifecycle;
    actor = { E.empty_actor with login = actor_login };
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

let make_issue_envelope ?(event = "issues") ?(action = Some "opened")
    ?(repo = "acme/widget") ?(org = Some "acme") ?(number = 7)
    ?(delivery_id = Some "deliv-issue-1") ?(actor_login = Some "bob")
    ?(labels = [ "ready" ]) ?(assignees = [ "carol" ])
    ?(milestone = Some "v1.0") () : E.t =
  {
    version = E.envelope_version;
    delivery_id;
    installation_id = Some 99;
    event;
    action;
    repo_full_name = repo;
    org;
    item_kind = Some E.Issue;
    item_number = Some number;
    item_node_id = None;
    item_url = None;
    html_url = None;
    family = E.Lifecycle;
    actor = { E.empty_actor with login = actor_login };
    before = None;
    after =
      Some
        {
          E.empty_safe_state with
          labels;
          assignees;
          milestone;
          state = Some "open";
        };
    transfer = None;
    received_at = Some "2024-01-01T00:00:00Z";
    event_at = None;
    head_sha = None;
    unsupported = false;
    skip_reason = None;
  }

let decision_kind = function
  | M.Matched _ -> "Matched"
  | M.Muted _ -> "Muted"
  | M.No_route -> "No_route"

let decision_route_id = function
  | M.Matched { route; _ } | M.Muted { route; _ } -> Some route.id
  | M.No_route -> None

let expect_muted ?reason_contains decision =
  match decision with
  | M.Muted { reason; _ } -> (
      match reason_contains with
      | None -> reason
      | Some needle ->
          Alcotest.(check bool)
            ("reason contains " ^ needle)
            true
            (Test_helpers.string_contains
               (String.lowercase_ascii reason)
               (String.lowercase_ascii needle));
          reason)
  | M.Matched { route; _ } ->
      Alcotest.failf "expected Muted, got Matched id=%s" route.id
  | M.No_route -> Alcotest.fail "expected Muted, got No_route"

let expect_matched ~id decision =
  match decision with
  | M.Matched { route; _ } -> Alcotest.(check string) "matched id" id route.id
  | M.Muted { route; reason; _ } ->
      Alcotest.failf "expected Matched, got Muted id=%s reason=%s" route.id
        reason
  | M.No_route -> Alcotest.fail "expected Matched, got No_route"

let validate f = assert_ok (F.validate f)

(* ---- Migration fixtures: baseline → advanced parity ---- *)

type migration_case = {
  name : string;
  v0 : F.v0;
  selector : S.selector;
  envelope : E.t;
      (** Expected baseline allow under [filter_allows] after migration. *)
  expect_baseline_allow : bool;
}

let migration_cases : migration_case list =
  [
    {
      name = "empty baseline allow-all PR";
      v0 =
        {
          include_events = [];
          exclude_events = [];
          include_repos = [];
          exclude_repos = [];
        };
      selector = S.Repo "acme/widget";
      envelope = make_pr_envelope ();
      expect_baseline_allow = true;
    };
    {
      name = "include_events pull_request only";
      v0 =
        {
          include_events = [ "pull_request" ];
          exclude_events = [];
          include_repos = [];
          exclude_repos = [];
        };
      selector = S.Repo "acme/widget";
      envelope = make_pr_envelope ();
      expect_baseline_allow = true;
    };
    {
      name = "exclude_events mutes comment family";
      v0 =
        {
          include_events = [];
          exclude_events = [ "issue_comment" ];
          include_repos = [];
          exclude_repos = [];
        };
      selector = S.Repo "acme/widget";
      envelope =
        make_pr_envelope ~event:"issue_comment" ~action:(Some "created") ();
      expect_baseline_allow = false;
    };
    {
      name = "include_repos narrows Org-style filter";
      v0 =
        {
          include_events = [];
          exclude_events = [];
          include_repos = [ "acme/widget" ];
          exclude_repos = [];
        };
      selector = S.Org "acme";
      envelope = make_pr_envelope ~repo:"acme/widget" ();
      expect_baseline_allow = true;
    };
    {
      name = "exclude_repos mutes secret repo";
      v0 =
        {
          include_events = [];
          exclude_events = [];
          include_repos = [];
          exclude_repos = [ "acme/secret" ];
        };
      selector = S.Org "acme";
      envelope = make_pr_envelope ~repo:"acme/secret" ~org:(Some "acme") ();
      expect_baseline_allow = false;
    };
    {
      name = "issue lifecycle with empty advanced";
      v0 =
        {
          include_events = [ "issues" ];
          exclude_events = [];
          include_repos = [];
          exclude_repos = [];
        };
      selector = S.Repo "acme/widget";
      envelope = make_issue_envelope ();
      expect_baseline_allow = true;
    };
  ]

let test_migration_parity_match_decisions () =
  (* Fresh db per case so disabled more-specific routes cannot shadow later
     Org cases (Item > Repo > Org no-fallthrough). *)
  List.iteri
    (fun i (c : migration_case) ->
      with_db @@ fun db ->
      let migrated = F.migrate_v0_to_v1 c.v0 in
      Alcotest.(check bool)
        (c.name ^ ": advanced empty after migrate")
        false (F.has_advanced migrated);
      Alcotest.(check int) (c.name ^ ": schema v1") 1 migrated.schema_version;
      Alcotest.(check bool)
        (c.name ^ ": filter_allows parity")
        c.expect_baseline_allow
        (M.filter_allows migrated c.envelope);
      let id = Printf.sprintf "mig_%02d" i in
      ignore (create ~db ~id ~selector:c.selector ~filter:migrated ());
      let baseline = M.resolve ~db ~destination:room ~envelope:c.envelope () in
      let advanced = A.resolve ~db ~destination:room ~envelope:c.envelope () in
      Alcotest.(check string)
        (c.name ^ ": decision kind parity")
        (decision_kind baseline) (decision_kind advanced);
      Alcotest.(check (option string))
        (c.name ^ ": route id parity")
        (decision_route_id baseline)
        (decision_route_id advanced);
      let expected_kind =
        if c.expect_baseline_allow then "Matched" else "Muted"
      in
      Alcotest.(check string)
        (c.name ^ ": expected kind")
        expected_kind (decision_kind advanced))
    migration_cases

let test_migration_json_v0_roundtrip_parity () =
  (* v0 JSON without schema_version migrates identically to explicit migrate. *)
  List.iter
    (fun (c : migration_case) ->
      let j =
        `Assoc
          [
            ( "include_events",
              `List (List.map (fun s -> `String s) c.v0.include_events) );
            ( "exclude_events",
              `List (List.map (fun s -> `String s) c.v0.exclude_events) );
            ( "include_repos",
              `List (List.map (fun s -> `String s) c.v0.include_repos) );
            ( "exclude_repos",
              `List (List.map (fun s -> `String s) c.v0.exclude_repos) );
          ]
      in
      let from_json = assert_ok (F.of_json j) in
      let from_migrate = F.migrate_v0_to_v1 c.v0 in
      Alcotest.(check bool)
        (c.name ^ ": has_advanced")
        (F.has_advanced from_migrate)
        (F.has_advanced from_json);
      Alcotest.(check bool)
        (c.name ^ ": allows")
        (M.filter_allows from_migrate c.envelope)
        (M.filter_allows from_json c.envelope))
    migration_cases

(* ---- Advanced PR+Issue predicate combination fixtures ---- *)

let full_pr_filter () =
  validate
    {
      F.default with
      pr =
        {
          base_branch = Some { op = `Glob; values = [ "main"; "release/*" ] };
          head_branch = Some { op = `Neq; values = [ "main" ] };
          changed_path = Some { op = `Glob; values = [ "src/**" ] };
          labels = Some { op = `In; values = [ "bug"; "security" ] };
          author = Some { op = `Eq; values = [ "alice" ] };
          team = Some { op = `In; values = [ "platform" ] };
          draft = Some false;
        };
    }

let full_issue_filter () =
  validate
    {
      F.default with
      issue =
        {
          labels = Some { op = `In; values = [ "ready" ] };
          author = Some { op = `In; values = [ "bob" ] };
          team = Some { op = `In; values = [ "triage" ] };
          assignee = Some { op = `In; values = [ "carol" ] };
          milestone = Some { op = `Eq; values = [ "v1.0" ] };
        };
    }

let pr_enrichment ~paths ~teams : En.enrichment =
  {
    paths = Some (Ok paths);
    teams = Some (Ok teams);
    reasons = [];
    complete = true;
  }

let issue_enrichment ~teams : En.enrichment =
  { paths = None; teams = Some (Ok teams); reasons = []; complete = true }

(** Every PR advanced field alone: positive match + negative mute. *)
let test_pr_predicate_combinations () =
  with_db @@ fun db ->
  let env =
    make_pr_envelope ~labels:[ "bug" ] ~actor_login:(Some "alice")
      ~draft:(Some false) ~base_ref:(Some "main") ()
  in
  let enr = pr_enrichment ~paths:[ "src/foo.ml" ] ~teams:[ "platform" ] in
  let cases : (string * F.t * bool * string option) list =
    [
      ( "pr.base_branch pass",
        validate
          {
            F.default with
            pr =
              {
                F.empty_pr with
                base_branch = Some { op = `Eq; values = [ "main" ] };
              };
          },
        true,
        None );
      ( "pr.base_branch fail",
        validate
          {
            F.default with
            pr =
              {
                F.empty_pr with
                base_branch = Some { op = `Eq; values = [ "develop" ] };
              };
          },
        false,
        Some "pr.base_branch" );
      ( "pr.head_branch fail missing subject",
        validate
          {
            F.default with
            pr =
              {
                F.empty_pr with
                head_branch = Some { op = `Eq; values = [ "feature/x" ] };
              };
          },
        false,
        Some "pr.head_branch" );
      ( "pr.changed_path pass",
        validate
          {
            F.default with
            pr =
              {
                F.empty_pr with
                changed_path = Some { op = `Glob; values = [ "src/**" ] };
              };
          },
        true,
        None );
      ( "pr.changed_path fail",
        validate
          {
            F.default with
            pr =
              {
                F.empty_pr with
                changed_path = Some { op = `Glob; values = [ "docs/**" ] };
              };
          },
        false,
        Some "pr.changed_path" );
      ( "pr.labels pass",
        validate
          {
            F.default with
            pr =
              { F.empty_pr with labels = Some { op = `In; values = [ "bug" ] } };
          },
        true,
        None );
      ( "pr.labels fail",
        validate
          {
            F.default with
            pr =
              {
                F.empty_pr with
                labels = Some { op = `In; values = [ "security" ] };
              };
          },
        false,
        Some "pr.labels" );
      ( "pr.author pass",
        validate
          {
            F.default with
            pr =
              {
                F.empty_pr with
                author = Some { op = `Eq; values = [ "alice" ] };
              };
          },
        true,
        None );
      ( "pr.author fail",
        validate
          {
            F.default with
            pr =
              {
                F.empty_pr with
                author = Some { op = `Eq; values = [ "mallory" ] };
              };
          },
        false,
        Some "pr.author" );
      ( "pr.team pass",
        validate
          {
            F.default with
            pr =
              {
                F.empty_pr with
                team = Some { op = `In; values = [ "platform" ] };
              };
          },
        true,
        None );
      ( "pr.team fail",
        validate
          {
            F.default with
            pr =
              {
                F.empty_pr with
                team = Some { op = `In; values = [ "security" ] };
              };
          },
        false,
        Some "pr.team" );
      ( "pr.draft pass",
        validate { F.default with pr = { F.empty_pr with draft = Some false } },
        true,
        None );
      ( "pr.draft fail",
        validate { F.default with pr = { F.empty_pr with draft = Some true } },
        false,
        Some "pr.draft" );
    ]
  in
  List.iteri
    (fun i (name, filter, expect_match, reason_needle) ->
      let id = Printf.sprintf "pr_combo_%02d" i in
      ignore (create ~db ~id ~filter ());
      let d =
        A.resolve ~db ~destination:room ~envelope:env ~enrichment:enr ()
      in
      if expect_match then expect_matched ~id d
      else ignore (expect_muted ?reason_contains:reason_needle d);
      ignore (S.update ~db ~id ~enabled:false ~now:fixed_now ()))
    cases

(** Every Issue advanced field alone. *)
let test_issue_predicate_combinations () =
  with_db @@ fun db ->
  let env = make_issue_envelope () in
  let enr = issue_enrichment ~teams:[ "triage" ] in
  let cases : (string * F.t * bool * string option) list =
    [
      ( "issue.labels pass",
        validate
          {
            F.default with
            issue =
              {
                F.empty_issue with
                labels = Some { op = `In; values = [ "ready" ] };
              };
          },
        true,
        None );
      ( "issue.labels fail",
        validate
          {
            F.default with
            issue =
              {
                F.empty_issue with
                labels = Some { op = `In; values = [ "blocked" ] };
              };
          },
        false,
        Some "issue.labels" );
      ( "issue.author pass",
        validate
          {
            F.default with
            issue =
              {
                F.empty_issue with
                author = Some { op = `Eq; values = [ "bob" ] };
              };
          },
        true,
        None );
      ( "issue.author fail",
        validate
          {
            F.default with
            issue =
              {
                F.empty_issue with
                author = Some { op = `Eq; values = [ "alice" ] };
              };
          },
        false,
        Some "issue.author" );
      ( "issue.team pass",
        validate
          {
            F.default with
            issue =
              {
                F.empty_issue with
                team = Some { op = `In; values = [ "triage" ] };
              };
          },
        true,
        None );
      ( "issue.team fail",
        validate
          {
            F.default with
            issue =
              {
                F.empty_issue with
                team = Some { op = `In; values = [ "platform" ] };
              };
          },
        false,
        Some "issue.team" );
      ( "issue.assignee pass",
        validate
          {
            F.default with
            issue =
              {
                F.empty_issue with
                assignee = Some { op = `In; values = [ "carol" ] };
              };
          },
        true,
        None );
      ( "issue.assignee fail",
        validate
          {
            F.default with
            issue =
              {
                F.empty_issue with
                assignee = Some { op = `In; values = [ "dave" ] };
              };
          },
        false,
        Some "issue.assignee" );
      ( "issue.milestone pass",
        validate
          {
            F.default with
            issue =
              {
                F.empty_issue with
                milestone = Some { op = `Eq; values = [ "v1.0" ] };
              };
          },
        true,
        None );
      ( "issue.milestone fail",
        validate
          {
            F.default with
            issue =
              {
                F.empty_issue with
                milestone = Some { op = `Eq; values = [ "v2.0" ] };
              };
          },
        false,
        Some "issue.milestone" );
    ]
  in
  List.iteri
    (fun i (name, filter, expect_match, reason_needle) ->
      let id = Printf.sprintf "iss_combo_%02d" i in
      ignore (create ~db ~id ~filter ());
      let d =
        A.resolve ~db ~destination:room ~envelope:env ~enrichment:enr ()
      in
      if expect_match then expect_matched ~id d
      else ignore (expect_muted ?reason_contains:reason_needle d);
      ignore (S.update ~db ~id ~enabled:false ~now:fixed_now ()))
    cases

(** Full AND composition of all PR fields, and of all Issue fields. *)
let test_full_and_composition_pr_and_issue () =
  with_db @@ fun db ->
  let pr_f = full_pr_filter () in
  Alcotest.(check int)
    "all PR advanced fields present" 7
    (B.count_advanced_field_steps ~filter:pr_f);
  ignore (create ~db ~id:"full_pr" ~filter:pr_f ());
  let env = make_pr_envelope () in
  let enr_ok = pr_enrichment ~paths:[ "src/a.ml" ] ~teams:[ "platform" ] in
  (* head_branch is missing on envelope → fail closed on full filter. *)
  ignore
    (expect_muted ~reason_contains:"pr.head_branch"
       (A.resolve ~db ~destination:room ~envelope:env ~enrichment:enr_ok ()));
  (* Override via pure eval context to prove AND of remaining fields. *)
  let pr_ctx : Ev.pr_context =
    {
      base_branch = Some "main";
      head_branch = Some "feature/x";
      changed_paths = Some [ "src/a.ml" ];
      labels = [ "bug" ];
      author = Some "alice";
      teams = Some [ "platform" ];
      draft = Some false;
    }
  in
  Alcotest.(check bool)
    "full PR AND allows" true
    (Ev.eval_pr ~filter:pr_f ~ctx:pr_ctx ());
  Alcotest.(check bool)
    "full PR AND rejects wrong team" false
    (Ev.eval_pr ~filter:pr_f ~ctx:{ pr_ctx with teams = Some [ "other" ] } ());
  ignore (S.update ~db ~id:"full_pr" ~enabled:false ~now:fixed_now ());
  let iss_f = full_issue_filter () in
  Alcotest.(check int)
    "all Issue advanced fields present" 5
    (B.count_advanced_field_steps ~filter:iss_f);
  ignore (create ~db ~id:"full_iss" ~filter:iss_f ());
  let ienv = make_issue_envelope () in
  let ienr = issue_enrichment ~teams:[ "triage" ] in
  expect_matched ~id:"full_iss"
    (A.resolve ~db ~destination:room ~envelope:ienv ~enrichment:ienr ());
  ignore (S.update ~db ~id:"full_iss" ~enabled:false ~now:fixed_now ());
  (* Combined PR+Issue on one filter: both sections are always AND-ed. A PR
     envelope must satisfy issue predicates against its after-state (and vice
     versa), so fixtures use compatible values. *)
  let both =
    validate
      {
        F.default with
        pr =
          {
            F.empty_pr with
            labels = Some { op = `In; values = [ "bug"; "ready" ] };
          };
        issue =
          {
            F.empty_issue with
            labels = Some { op = `In; values = [ "bug"; "ready" ] };
          };
      }
  in
  ignore (create ~db ~id:"both_sec" ~filter:both ());
  expect_matched ~id:"both_sec"
    (A.resolve ~db ~destination:room
       ~envelope:(make_pr_envelope ~labels:[ "bug" ] ())
       ());
  expect_matched ~id:"both_sec"
    (A.resolve ~db ~destination:room
       ~envelope:(make_issue_envelope ~labels:[ "ready" ] ())
       ~enrichment:(issue_enrichment ~teams:[])
       ());
  (* AND fails closed when either section rejects. *)
  ignore
    (expect_muted ~reason_contains:"pr.labels"
       (A.resolve ~db ~destination:room
          ~envelope:(make_pr_envelope ~labels:[ "docs" ] ())
          ()))

(* ---- Rate-limit fail-closed + explainable ---- *)

let test_rate_limit_gate_fail_closed_explainable () =
  with_db @@ fun db ->
  let filter =
    validate
      {
        F.default with
        pr =
          {
            F.empty_pr with
            changed_path = Some { op = `Glob; values = [ "src/**" ] };
            team = Some { op = `In; values = [ "platform" ] };
          };
      }
  in
  ignore (create ~db ~id:"rt_rl" ~filter ());
  let env = make_pr_envelope () in
  let path_calls = ref 0 in
  let team_calls = ref 0 in
  let fetch_paths ~envelope:_ =
    incr path_calls;
    Ok [ "src/x.ml" ]
  in
  let fetch_teams ~envelope:_ ~team_slugs:_ =
    incr team_calls;
    Ok [ "platform" ]
  in
  let decision =
    A.resolve ~db ~destination:room ~envelope:env ~fetch_paths ~fetch_teams
      ~rate_limited:(fun () -> true)
      ()
  in
  let reason = expect_muted ~reason_contains:"rate_limited" decision in
  Alcotest.(check bool)
    "explain mentions enrichment" true
    (Test_helpers.string_contains (String.lowercase_ascii reason) "enrichment");
  Alcotest.(check int) "paths not fetched when rate limited" 0 !path_calls;
  Alcotest.(check int) "teams not fetched when rate limited" 0 !team_calls;
  (* Preview surface is also explainable. *)
  let enrichment : En.enrichment =
    {
      paths = Some (Error "rate_limited");
      teams = Some (Error "rate_limited");
      reasons = [ "rate_limited" ];
      complete = false;
    }
  in
  let prev = P.preview ~db ~destination:room ~envelope:env ~enrichment () in
  Alcotest.(check string) "preview decision" "Muted" prev.decision;
  (* Explainable: mute is structured; rate_limited appears in enrichment_status
     (stable codes) and/or predicate detail for demanded fields. *)
  let status_blob =
    String.lowercase_ascii (String.concat " " prev.enrichment_status)
  in
  Alcotest.(check bool)
    "preview enrichment_status mentions rate_limited" true
    (Test_helpers.string_contains status_blob "rate_limited");
  Alcotest.(check bool)
    "preview final_reason non-empty" true
    (String.trim prev.final_reason <> "")

let test_rate_limit_never_allow_on_missing () =
  with_db @@ fun db ->
  (* Org broader route would match if fallthrough/allow-on-missing existed. *)
  ignore (create ~db ~id:"rt_org" ~selector:(S.Org "acme") ());
  let filter =
    validate
      {
        F.default with
        pr =
          {
            F.empty_pr with
            changed_path = Some { op = `Glob; values = [ "**" ] };
          };
      }
  in
  ignore (create ~db ~id:"rt_repo" ~selector:(S.Repo "acme/widget") ~filter ());
  let env = make_pr_envelope () in
  let d =
    A.resolve ~db ~destination:room ~envelope:env
      ~rate_limited:(fun () -> true)
      ~fetch_paths:(fun ~envelope:_ -> Ok [ "any.ml" ])
      ()
  in
  match d with
  | M.Muted { route; reason; _ } ->
      Alcotest.(check string) "most-specific muted" "rt_repo" route.id;
      Alcotest.(check bool)
        "not allow-on-missing" true
        (Test_helpers.string_contains
           (String.lowercase_ascii reason)
           "rate_limited"
        || Test_helpers.string_contains
             (String.lowercase_ascii reason)
             "enrichment")
  | M.Matched { route; _ } ->
      Alcotest.failf "rate-limit must not allow; got Matched %s" route.id
  | M.No_route -> Alcotest.fail "expected Muted, not No_route"

let test_access_denied_fail_closed () =
  with_db @@ fun db ->
  let filter =
    validate
      {
        F.default with
        pr =
          { F.empty_pr with team = Some { op = `In; values = [ "platform" ] } };
      }
  in
  ignore (create ~db ~id:"rt_team" ~filter ());
  let env = make_pr_envelope () in
  let calls = ref 0 in
  let fetch_teams ~envelope:_ ~team_slugs:_ =
    incr calls;
    Ok [ "platform" ]
  in
  let d =
    A.resolve ~db ~destination:room ~envelope:env ~fetch_teams
      ~access_allowed:(fun () -> false)
      ()
  in
  ignore (expect_muted ~reason_contains:"access_denied" d);
  Alcotest.(check int) "no team fetch" 0 !calls

(* ---- Org-scale budgets ---- *)

let test_org_scale_candidate_budget () =
  with_db @@ fun db ->
  let setup =
    assert_ok
      (B.install_org_scale_routes ~db ~destination:room
         ~sibling_repos:B.org_scale_sibling_repo_routes ~include_item:true ())
  in
  Alcotest.(check int)
    "index holds org+repo+item+siblings"
    (3 + B.org_scale_sibling_repo_routes)
    (A.index_size setup.index);
  let cands = A.index_candidates setup.index ~envelope:setup.envelope in
  Alcotest.(check bool)
    "candidates within budget" true
    (List.length cands <= B.org_scale_max_candidates);
  Alcotest.(check int) "exactly item+repo+org" 3 (List.length cands);
  let ids =
    List.map (fun (r : S.t) -> r.id) cands |> List.sort String.compare
  in
  Alcotest.(check (list string))
    "no sibling repos in candidates"
    [ "bench_item"; "bench_org"; "bench_repo" ]
    ids;
  let costs =
    B.measure_indexed_resolve ~db ~destination:room ~envelope:setup.envelope
      ~index:setup.index ()
  in
  assert_ok (B.assert_costs_within_budget costs);
  expect_matched ~id:"bench_item" costs.decision;
  Alcotest.(check int) "no enrichment for baseline filter" 0 costs.path_fetches;
  Alcotest.(check int) "no team fetches" 0 costs.team_fetches

let test_org_scale_enrichment_budget_cold_and_warm () =
  with_db @@ fun db ->
  let filter =
    validate
      {
        F.default with
        pr =
          {
            F.empty_pr with
            changed_path = Some { op = `Glob; values = [ "src/**" ] };
            team = Some { op = `In; values = [ "platform" ] };
          };
      }
  in
  let setup =
    assert_ok
      (B.install_org_scale_routes ~db ~destination:room
         ~sibling_repos:B.org_scale_sibling_repo_routes ~include_item:false
         ~target_filter:filter ())
  in
  let cache = En.create_cache ~ttl_s:60.0 () in
  let fetch_paths ~envelope:_ = Ok [ "src/main.ml" ] in
  let fetch_teams ~envelope:_ ~team_slugs:_ = Ok [ "platform" ] in
  let cold =
    B.measure_indexed_resolve ~db ~destination:room ~envelope:setup.envelope
      ~index:setup.index ~fetch_paths ~fetch_teams ~cache ~now:fixed_now ()
  in
  assert_ok (B.assert_costs_within_budget cold);
  Alcotest.(check bool) "cold path fetches ≤ 1" true (cold.path_fetches <= 1);
  Alcotest.(check bool) "cold team fetches ≤ 1" true (cold.team_fetches <= 1);
  Alcotest.(check bool)
    "cold total enrichment within budget" true
    (cold.path_fetches + cold.team_fetches
    <= B.max_enrichment_fetches_per_cold_resolve);
  expect_matched ~id:setup.target_repo_route_id cold.decision;
  let warm =
    B.measure_indexed_resolve ~db ~destination:room ~envelope:setup.envelope
      ~index:setup.index ~fetch_paths ~fetch_teams ~cache ~now:fixed_now ()
  in
  Alcotest.(check int)
    "warm path fetches" B.max_enrichment_fetches_warm_cache warm.path_fetches;
  Alcotest.(check int)
    "warm team fetches" B.max_enrichment_fetches_warm_cache warm.team_fetches;
  expect_matched ~id:setup.target_repo_route_id warm.decision;
  (* Rate-limit still within candidate budget and zero enrichment fetches. *)
  let limited =
    B.measure_indexed_resolve ~db ~destination:room ~envelope:setup.envelope
      ~index:setup.index ~fetch_paths ~fetch_teams
      ~rate_limited:(fun () -> true)
      ()
  in
  Alcotest.(check bool)
    "rate-limit still bounds candidates" true
    (limited.candidates <= B.org_scale_max_candidates);
  Alcotest.(check int) "rate-limit skips path fetch" 0 limited.path_fetches;
  Alcotest.(check int) "rate-limit skips team fetch" 0 limited.team_fetches;
  ignore (expect_muted ~reason_contains:"rate_limited" limited.decision)

let test_org_scale_match_cost_budget_documented () =
  (* Pure documentation assertions: budgets are positive and internally
     consistent (no network, no store). *)
  Alcotest.(check bool)
    "sibling scale is Org-sized" true
    (B.org_scale_sibling_repo_routes >= 100);
  Alcotest.(check bool)
    "max candidates << siblings" true
    (B.org_scale_max_candidates < B.org_scale_sibling_repo_routes);
  Alcotest.(check int)
    "cold enrichment budget" 2 B.max_enrichment_fetches_per_cold_resolve;
  Alcotest.(check int)
    "warm enrichment budget" 0 B.max_enrichment_fetches_warm_cache;
  let full =
    validate { (full_pr_filter ()) with issue = (full_issue_filter ()).issue }
  in
  let steps = B.count_advanced_field_steps ~filter:full in
  Alcotest.(check int) "PR+Issue field count" 12 steps;
  let units =
    B.estimate_match_cost_units ~candidates:B.org_scale_max_candidates
      ~filter:full
  in
  Alcotest.(check bool)
    "full synthetic cost within budget" true
    (units <= B.max_match_eval_cost_units);
  (* Semantics unchanged: estimate is pure arithmetic. *)
  Alcotest.(check int)
    "estimate formula"
    (B.org_scale_max_candidates + 1 + steps)
    units

let suite =
  [
    ( "migration parity match decisions",
      `Quick,
      test_migration_parity_match_decisions );
    ( "migration json v0 roundtrip parity",
      `Quick,
      test_migration_json_v0_roundtrip_parity );
    ("pr predicate combinations", `Quick, test_pr_predicate_combinations);
    ("issue predicate combinations", `Quick, test_issue_predicate_combinations);
    ( "full AND composition pr and issue",
      `Quick,
      test_full_and_composition_pr_and_issue );
    ( "rate-limit gate fail closed explainable",
      `Quick,
      test_rate_limit_gate_fail_closed_explainable );
    ( "rate-limit never allow-on-missing",
      `Quick,
      test_rate_limit_never_allow_on_missing );
    ("access denied fail closed", `Quick, test_access_denied_fail_closed);
    ("org-scale candidate budget", `Quick, test_org_scale_candidate_budget);
    ( "org-scale enrichment budget cold and warm",
      `Quick,
      test_org_scale_enrichment_budget_cold_and_warm );
    ( "org-scale match cost budget documented",
      `Quick,
      test_org_scale_match_cost_budget_documented );
  ]
