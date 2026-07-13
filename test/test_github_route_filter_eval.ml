(** Tests for PR advanced filter predicates (P20.M1.E1.T003).

    Fixtures cover positive/negative composition, rename paths, deleted
    branches, missing team access, and rate-limited enrichment. *)

module F = Github_route_filter
module Ev = Github_route_filter_eval
module E = Github_event_envelope
module En = Github_filter_enrichment

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let pr_filter ?(base_branch : F.glob_match option = None)
    ?(head_branch : F.glob_match option = None)
    ?(changed_path : F.glob_match option = None)
    ?(labels : F.set_match option = None) ?(author : F.set_match option = None)
    ?(team : F.set_match option = None) ?(draft : bool option = None)
    ?(include_events = []) ?(exclude_events = []) ?(include_repos = [])
    ?(exclude_repos = []) () : F.t =
  assert_ok
    (F.validate
       {
         F.default with
         include_events;
         exclude_events;
         include_repos;
         exclude_repos;
         pr =
           {
             base_branch;
             head_branch;
             changed_path;
             labels;
             author;
             team;
             draft;
           };
       })

let ctx ?(base_branch = None) ?(head_branch = None) ?(changed_paths = None)
    ?(labels = []) ?(author = None) ?(teams = None) ?(draft = None) () :
    Ev.pr_context =
  { base_branch; head_branch; changed_paths; labels; author; teams; draft }

let allows ~filter ~ctx = Ev.eval_pr ~filter ~ctx ()
let check_bool msg expected got = Alcotest.(check bool) msg expected got

(** ---- Glob unit cases ---- *)

let test_match_glob_basics () =
  check_bool "exact" true (Ev.match_glob ~pattern:"main" ~value:"main");
  check_bool "exact miss" false (Ev.match_glob ~pattern:"main" ~value:"Main");
  check_bool "star any" true (Ev.match_glob ~pattern:"*" ~value:"anything/here");
  check_bool "release/* hit" true
    (Ev.match_glob ~pattern:"release/*" ~value:"release/1.0");
  check_bool "release/* miss multi" false
    (Ev.match_glob ~pattern:"release/*" ~value:"release/1.0/rc");
  check_bool "release/** multi" true
    (Ev.match_glob ~pattern:"release/**" ~value:"release/1.0/rc");
  check_bool "src/**/*.ml" true
    (Ev.match_glob ~pattern:"src/**/*.ml" ~value:"src/foo/bar.ml");
  check_bool "segment *.ml" true
    (Ev.match_glob ~pattern:"*.ml" ~value:"main.ml");
  check_bool "segment *.ml miss" false
    (Ev.match_glob ~pattern:"*.ml" ~value:"src/main.ml")

(** ---- Branch predicates ---- *)

let test_base_head_branch_positive_negative () =
  let filter =
    pr_filter
      ~base_branch:(Some { op = `Glob; values = [ "release/*"; "main" ] })
      ~head_branch:(Some { op = `Neq; values = [ "main" ] })
      ()
  in
  let good =
    ctx ~base_branch:(Some "release/1.2") ~head_branch:(Some "feature/x") ()
  in
  check_bool "base glob + head neq" true (allows ~filter ~ctx:good);
  let base_exact =
    ctx ~base_branch:(Some "main") ~head_branch:(Some "dev") ()
  in
  check_bool "base exact main via glob list" true
    (allows ~filter ~ctx:base_exact);
  let bad_base =
    ctx ~base_branch:(Some "develop") ~head_branch:(Some "feature/x") ()
  in
  check_bool "base miss" false (allows ~filter ~ctx:bad_base);
  let head_main =
    ctx ~base_branch:(Some "main") ~head_branch:(Some "main") ()
  in
  check_bool "head is main rejected" false (allows ~filter ~ctx:head_main);
  (* case-sensitive branches *)
  let case_f =
    pr_filter ~base_branch:(Some { op = `Eq; values = [ "Main" ] }) ()
  in
  check_bool "branch case-sensitive" false
    (allows ~filter:case_f ~ctx:(ctx ~base_branch:(Some "main") ()))

let test_deleted_branches_fail_closed () =
  let filter =
    pr_filter ~base_branch:(Some { op = `Eq; values = [ "main" ] }) ()
  in
  check_bool "missing base fails closed" false
    (allows ~filter ~ctx:(ctx ~base_branch:None ()));
  let filter_h =
    pr_filter ~head_branch:(Some { op = `In; values = [ "dev"; "feat" ] }) ()
  in
  check_bool "missing head fails closed" false
    (allows ~filter:filter_h ~ctx:(ctx ~head_branch:None ()))

(** ---- Paths: positive/negative, rename, missing enrichment ---- *)

let test_changed_paths_glob_and_rename () =
  let filter =
    pr_filter
      ~changed_path:(Some { op = `Glob; values = [ "src/**"; "docs/*.md" ] })
      ()
  in
  check_bool "src path hits" true
    (allows ~filter
       ~ctx:(ctx ~changed_paths:(Some [ "README.md"; "src/lib/a.ml" ]) ()));
  check_bool "docs md hits" true
    (allows ~filter ~ctx:(ctx ~changed_paths:(Some [ "docs/guide.md" ]) ()));
  check_bool "unrelated paths" false
    (allows ~filter
       ~ctx:(ctx ~changed_paths:(Some [ "Makefile"; "bin/run" ]) ()));
  (* Rename: both previous and new path present — either may match *)
  check_bool "rename old path" true
    (allows ~filter
       ~ctx:
         (ctx ~changed_paths:(Some [ "src/old_name.ml"; "lib/new_name.ml" ]) ()));
  check_bool "rename new path only under docs" true
    (allows ~filter
       ~ctx:(ctx ~changed_paths:(Some [ "pkg/old.md"; "docs/new.md" ]) ()));
  check_bool "empty known paths no hit" false
    (allows ~filter ~ctx:(ctx ~changed_paths:(Some []) ()));
  (* not_in: reject if any path matches *)
  let excl =
    pr_filter
      ~changed_path:(Some { op = `Not_in; values = [ "secret.env" ] })
      ()
  in
  check_bool "not_in clean" true
    (allows ~filter:excl ~ctx:(ctx ~changed_paths:(Some [ "src/a.ml" ]) ()));
  check_bool "not_in secret present" false
    (allows ~filter:excl
       ~ctx:(ctx ~changed_paths:(Some [ "src/a.ml"; "secret.env" ]) ()))

let test_rate_limited_and_missing_path_enrichment () =
  let filter =
    pr_filter ~changed_path:(Some { op = `Glob; values = [ "src/**" ] }) ()
  in
  check_bool "paths None rate-limited fails closed" false
    (allows ~filter ~ctx:(ctx ~changed_paths:None ()));
  (* Enrichment Error maps to None via pr_context_of_envelope *)
  let env : E.t =
    {
      version = E.envelope_version;
      delivery_id = Some "d1";
      installation_id = Some 1;
      event = "pull_request";
      action = Some "opened";
      repo_full_name = "acme/widget";
      org = Some "acme";
      item_kind = Some E.Pull_request;
      item_number = Some 1;
      item_node_id = None;
      item_url = None;
      html_url = None;
      family = E.Lifecycle;
      actor = { E.empty_actor with login = Some "alice" };
      before = None;
      after =
        Some
          {
            E.empty_safe_state with
            draft = Some false;
            base_ref = Some "main";
            labels = [ "bug" ];
          };
      transfer = None;
      received_at = None;
      event_at = None;
      head_sha = Some "abc";
      unsupported = false;
      skip_reason = None;
    }
  in
  let enr : En.enrichment =
    {
      paths = Some (Error "rate_limited");
      teams = None;
      reasons = [ "rate_limited" ];
      complete = false;
    }
  in
  let c = Ev.pr_context_of_envelope ~envelope:env ~enrichment:enr () in
  check_bool "enrichment error → None paths" true (c.changed_paths = None);
  check_bool "eval fail closed on rate limit" false (allows ~filter ~ctx:c)

(** ---- Labels / author / draft ---- *)

let test_labels_author_draft_case_and_composition () =
  let filter =
    pr_filter
      ~labels:(Some { op = `In; values = [ "needs-review"; "P20" ] })
      ~author:(Some { op = `Eq; values = [ "Alice" ] })
      ~draft:(Some false) ()
  in
  let good =
    ctx ~labels:[ "bug"; "Needs-Review" ] ~author:(Some "alice")
      ~draft:(Some false) ()
  in
  check_bool "label+author CI + non-draft" true (allows ~filter ~ctx:good);
  check_bool "label miss" false
    (allows ~filter
       ~ctx:
         (ctx ~labels:[ "bug" ] ~author:(Some "alice") ~draft:(Some false) ()));
  check_bool "author miss" false
    (allows ~filter
       ~ctx:(ctx ~labels:[ "p20" ] ~author:(Some "bob") ~draft:(Some false) ()));
  check_bool "draft miss" false
    (allows ~filter
       ~ctx:(ctx ~labels:[ "p20" ] ~author:(Some "alice") ~draft:(Some true) ()));
  check_bool "missing author fail closed" false
    (allows ~filter
       ~ctx:(ctx ~labels:[ "p20" ] ~author:None ~draft:(Some false) ()));
  check_bool "missing draft fail closed" false
    (allows ~filter
       ~ctx:(ctx ~labels:[ "p20" ] ~author:(Some "alice") ~draft:None ()));
  (* not_in labels *)
  let no_wontfix =
    pr_filter ~labels:(Some { op = `Not_in; values = [ "wontfix" ] }) ()
  in
  check_bool "empty labels pass not_in" true
    (allows ~filter:no_wontfix ~ctx:(ctx ~labels:[] ()));
  check_bool "wontfix rejected" false
    (allows ~filter:no_wontfix ~ctx:(ctx ~labels:[ "WontFix" ] ()))

(** ---- Team membership / missing access ---- *)

let test_team_membership_and_missing_access () =
  let filter =
    pr_filter
      ~team:(Some { op = `In; values = [ "acme/backend"; "acme/core" ] })
      ()
  in
  check_bool "member hit CI" true
    (allows ~filter ~ctx:(ctx ~teams:(Some [ "Acme/Backend" ]) ()));
  check_bool "non-member" false
    (allows ~filter ~ctx:(ctx ~teams:(Some [ "acme/frontend" ]) ()));
  check_bool "known empty membership" false
    (allows ~filter ~ctx:(ctx ~teams:(Some []) ()));
  check_bool "missing team access / not enriched" false
    (allows ~filter ~ctx:(ctx ~teams:None ()));
  (* rate_limited enrichment → None teams *)
  let enr : En.enrichment =
    {
      paths = None;
      teams = Some (Error "rate_limited");
      reasons = [ "rate_limited" ];
      complete = false;
    }
  in
  let env : E.t =
    {
      version = E.envelope_version;
      delivery_id = None;
      installation_id = None;
      event = "pull_request";
      action = Some "opened";
      repo_full_name = "acme/widget";
      org = Some "acme";
      item_kind = Some E.Pull_request;
      item_number = Some 2;
      item_node_id = None;
      item_url = None;
      html_url = None;
      family = E.Lifecycle;
      actor = { E.empty_actor with login = Some "carol" };
      before = None;
      after = Some E.empty_safe_state;
      transfer = None;
      received_at = None;
      event_at = None;
      head_sha = None;
      unsupported = false;
      skip_reason = None;
    }
  in
  let c = Ev.pr_context_of_envelope ~envelope:env ~enrichment:enr () in
  check_bool "teams from rate limit None" true (c.teams = None);
  check_bool "fail closed rate-limited teams" false (allows ~filter ~ctx:c);
  (* access_denied same path *)
  let enr2 = { enr with teams = Some (Error "access_denied") } in
  let c2 = Ev.pr_context_of_envelope ~envelope:env ~enrichment:enr2 () in
  check_bool "fail closed access_denied teams" false (allows ~filter ~ctx:c2);
  (* not_in team: known non-member allows *)
  let excl =
    pr_filter ~team:(Some { op = `Not_in; values = [ "acme/bots" ] }) ()
  in
  check_bool "not_in team ok" true
    (allows ~filter:excl ~ctx:(ctx ~teams:(Some [ "acme/backend" ]) ()));
  check_bool "not_in team hit" false
    (allows ~filter:excl ~ctx:(ctx ~teams:(Some [ "acme/bots" ]) ()))

(** ---- Full positive / negative composition ---- *)

let test_full_positive_negative_composition () =
  let filter =
    pr_filter
      ~base_branch:(Some { op = `In; values = [ "main"; "develop" ] })
      ~head_branch:(Some { op = `Glob; values = [ "feature/*" ] })
      ~changed_path:(Some { op = `Glob; values = [ "src/**" ] })
      ~labels:(Some { op = `In; values = [ "ready" ] })
      ~author:(Some { op = `In; values = [ "alice"; "bob" ] })
      ~team:(Some { op = `In; values = [ "acme/backend" ] })
      ~draft:(Some false) ()
  in
  let good =
    ctx ~base_branch:(Some "main") ~head_branch:(Some "feature/p20")
      ~changed_paths:(Some [ "src/github_route_filter_eval.ml" ])
      ~labels:[ "ready"; "p20" ] ~author:(Some "Bob")
      ~teams:(Some [ "acme/backend" ]) ~draft:(Some false) ()
  in
  check_bool "full positive" true (allows ~filter ~ctx:good);
  (* flip each dimension *)
  check_bool "neg base" false
    (allows ~filter ~ctx:{ good with base_branch = Some "release" });
  check_bool "neg head" false
    (allows ~filter ~ctx:{ good with head_branch = Some "hotfix/x" });
  check_bool "neg paths" false
    (allows ~filter ~ctx:{ good with changed_paths = Some [ "docs/a.md" ] });
  check_bool "neg labels" false
    (allows ~filter ~ctx:{ good with labels = [ "wip" ] });
  check_bool "neg author" false
    (allows ~filter ~ctx:{ good with author = Some "eve" });
  check_bool "neg team" false
    (allows ~filter ~ctx:{ good with teams = Some [ "acme/other" ] });
  check_bool "neg draft" false
    (allows ~filter ~ctx:{ good with draft = Some true });
  (* empty advanced allows all *)
  check_bool "empty advanced allow" true
    (allows ~filter:F.default ~ctx:(ctx ()))

(** ---- Baseline composition ---- *)

let test_baseline_and_pr_composition () =
  let filter =
    pr_filter ~include_events:[ "pull_request" ]
      ~exclude_events:[ "issue_comment" ] ~exclude_repos:[ "acme/secret" ]
      ~labels:(Some { op = `In; values = [ "go" ] })
      ()
  in
  let c = ctx ~labels:[ "go" ] () in
  check_bool "baseline+pr ok" true
    (Ev.eval_pr_with_baseline ~filter ~event:"pull_request" ~repo:"acme/widget"
       ~ctx:c ());
  check_bool "event excluded" false
    (Ev.eval_pr_with_baseline ~filter ~event:"issue_comment" ~repo:"acme/widget"
       ~ctx:c ());
  check_bool "repo excluded" false
    (Ev.eval_pr_with_baseline ~filter ~event:"pull_request" ~repo:"acme/secret"
       ~ctx:c ());
  check_bool "pr labels fail after baseline" false
    (Ev.eval_pr_with_baseline ~filter ~event:"pull_request" ~repo:"acme/widget"
       ~ctx:(ctx ~labels:[] ()) ());
  check_bool "family token in include" true
    (Ev.eval_baseline
       ~filter:(pr_filter ~include_events:[ "lifecycle" ] ())
       ~event:"pull_request" ~family:"lifecycle" ())

let test_pr_context_of_envelope_fields () =
  let env : E.t =
    {
      version = E.envelope_version;
      delivery_id = None;
      installation_id = Some 9;
      event = "pull_request";
      action = Some "opened";
      repo_full_name = "acme/widget";
      org = Some "acme";
      item_kind = Some E.Pull_request;
      item_number = Some 7;
      item_node_id = None;
      item_url = None;
      html_url = None;
      family = E.Lifecycle;
      actor = { E.empty_actor with login = Some "Dana" };
      before = None;
      after =
        Some
          {
            E.empty_safe_state with
            labels = [ "a"; "b" ];
            draft = Some true;
            base_ref = Some "develop";
          };
      transfer = None;
      received_at = None;
      event_at = None;
      head_sha = Some "deadbeef";
      unsupported = false;
      skip_reason = None;
    }
  in
  let enr : En.enrichment =
    {
      paths = Some (Ok [ "src/a.ml"; "src/b.ml" ]);
      teams = Some (Ok [ "acme/backend" ]);
      reasons = [];
      complete = true;
    }
  in
  let c = Ev.pr_context_of_envelope ~envelope:env ~enrichment:enr () in
  Alcotest.(check (option string)) "base" (Some "develop") c.base_branch;
  Alcotest.(check (option string)) "author" (Some "Dana") c.author;
  Alcotest.(check (list string)) "labels" [ "a"; "b" ] c.labels;
  Alcotest.(check (option bool)) "draft" (Some true) c.draft;
  Alcotest.(check (option (list string)))
    "paths"
    (Some [ "src/a.ml"; "src/b.ml" ])
    c.changed_paths;
  Alcotest.(check (option (list string)))
    "teams" (Some [ "acme/backend" ]) c.teams

let suite =
  [
    ("match_glob basics", `Quick, test_match_glob_basics);
    ( "base/head branch positive negative",
      `Quick,
      test_base_head_branch_positive_negative );
    ("deleted branches fail closed", `Quick, test_deleted_branches_fail_closed);
    ("changed paths glob and rename", `Quick, test_changed_paths_glob_and_rename);
    ( "rate-limited missing path enrichment",
      `Quick,
      test_rate_limited_and_missing_path_enrichment );
    ( "labels author draft case composition",
      `Quick,
      test_labels_author_draft_case_and_composition );
    ( "team membership and missing access",
      `Quick,
      test_team_membership_and_missing_access );
    ( "full positive negative composition",
      `Quick,
      test_full_positive_negative_composition );
    ("baseline and pr composition", `Quick, test_baseline_and_pr_composition);
    ("pr_context_of_envelope fields", `Quick, test_pr_context_of_envelope_fields);
  ]
