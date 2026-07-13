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
  check_bool "base glob + head neq" true (allows ~filter ~ctx:good);
  let base_exact =
    ctx ~base_branch:(Some "main") ~head_branch:(Some "dev") ()
  check_bool "base exact main via glob list" true
    (allows ~filter ~ctx:base_exact);
  let bad_base =
    ctx ~base_branch:(Some "develop") ~head_branch:(Some "feature/x") ()
  check_bool "base miss" false (allows ~filter ~ctx:bad_base);
  let head_main =
    ctx ~base_branch:(Some "main") ~head_branch:(Some "main") ()
  check_bool "head is main rejected" false (allows ~filter ~ctx:head_main);
  (* case-sensitive branches *)
  let case_f =
    pr_filter ~base_branch:(Some { op = `Eq; values = [ "Main" ] }) ()
  check_bool "branch case-sensitive" false
    (allows ~filter:case_f ~ctx:(ctx ~base_branch:(Some "main") ()))

let test_deleted_branches_fail_closed () =
    pr_filter ~base_branch:(Some { op = `Eq; values = [ "main" ] }) ()
  check_bool "missing base fails closed" false
    (allows ~filter ~ctx:(ctx ~base_branch:None ()));
  let filter_h =
    pr_filter ~head_branch:(Some { op = `In; values = [ "dev"; "feat" ] }) ()
  check_bool "missing head fails closed" false
    (allows ~filter:filter_h ~ctx:(ctx ~head_branch:None ()))

(** ---- Paths: positive/negative, rename, missing enrichment ---- *)

let test_changed_paths_glob_and_rename () =
      ~changed_path:(Some { op = `Glob; values = [ "src/**"; "docs/*.md" ] })
  check_bool "src path hits" true
    (allows ~filter
       ~ctx:(ctx ~changed_paths:(Some [ "README.md"; "src/lib/a.ml" ]) ()));
  check_bool "docs md hits" true
    (allows ~filter ~ctx:(ctx ~changed_paths:(Some [ "docs/guide.md" ]) ()));
  check_bool "unrelated paths" false
       ~ctx:(ctx ~changed_paths:(Some [ "Makefile"; "bin/run" ]) ()));
  (* Rename: both previous and new path present — either may match *)
  check_bool "rename old path" true
       ~ctx:
         (ctx ~changed_paths:(Some [ "src/old_name.ml"; "lib/new_name.ml" ]) ()));
  check_bool "rename new path only under docs" true
       ~ctx:(ctx ~changed_paths:(Some [ "pkg/old.md"; "docs/new.md" ]) ()));
  check_bool "empty known paths no hit" false
    (allows ~filter ~ctx:(ctx ~changed_paths:(Some []) ()));
  (* not_in: reject if any path matches *)
  let excl =
      ~changed_path:(Some { op = `Not_in; values = [ "secret.env" ] })
  check_bool "not_in clean" true
    (allows ~filter:excl ~ctx:(ctx ~changed_paths:(Some [ "src/a.ml" ]) ()));
  check_bool "not_in secret present" false
    (allows ~filter:excl
       ~ctx:(ctx ~changed_paths:(Some [ "src/a.ml"; "secret.env" ]) ()))

let test_rate_limited_and_missing_path_enrichment () =
    pr_filter ~changed_path:(Some { op = `Glob; values = [ "src/**" ] }) ()
  check_bool "paths None rate-limited fails closed" false
    (allows ~filter ~ctx:(ctx ~changed_paths:None ()));
  (* Enrichment Error maps to None via pr_context_of_envelope *)
  let env : E.t =
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
            E.empty_safe_state with
            draft = Some false;
            base_ref = Some "main";
            labels = [ "bug" ];
      transfer = None;
      received_at = None;
      event_at = None;
      head_sha = Some "abc";
      unsupported = false;
      skip_reason = None;
    }
  let enr : En.enrichment =
      paths = Some (Error "rate_limited");
      teams = None;
      reasons = [ "rate_limited" ];
      complete = false;
  let c = Ev.pr_context_of_envelope ~envelope:env ~enrichment:enr () in
  check_bool "enrichment error → None paths" true (c.changed_paths = None);
  check_bool "eval fail closed on rate limit" false (allows ~filter ~ctx:c)

(** ---- Labels / author / draft ---- *)

let test_labels_author_draft_case_and_composition () =
      ~labels:(Some { op = `In; values = [ "needs-review"; "P20" ] })
      ~author:(Some { op = `Eq; values = [ "Alice" ] })
      ~draft:(Some false) ()
    ctx ~labels:[ "bug"; "Needs-Review" ] ~author:(Some "alice")
  check_bool "label+author CI + non-draft" true (allows ~filter ~ctx:good);
  check_bool "label miss" false
         (ctx ~labels:[ "bug" ] ~author:(Some "alice") ~draft:(Some false) ()));
  check_bool "author miss" false
       ~ctx:(ctx ~labels:[ "p20" ] ~author:(Some "bob") ~draft:(Some false) ()));
  check_bool "draft miss" false
       ~ctx:(ctx ~labels:[ "p20" ] ~author:(Some "alice") ~draft:(Some true) ()));
  check_bool "missing author fail closed" false
       ~ctx:(ctx ~labels:[ "p20" ] ~author:None ~draft:(Some false) ()));
  check_bool "missing draft fail closed" false
       ~ctx:(ctx ~labels:[ "p20" ] ~author:(Some "alice") ~draft:None ()));
  (* not_in labels *)
  let no_wontfix =
    pr_filter ~labels:(Some { op = `Not_in; values = [ "wontfix" ] }) ()
  check_bool "empty labels pass not_in" true
    (allows ~filter:no_wontfix ~ctx:(ctx ~labels:[] ()));
  check_bool "wontfix rejected" false
    (allows ~filter:no_wontfix ~ctx:(ctx ~labels:[ "WontFix" ] ()))

(** ---- Team membership / missing access ---- *)

let test_team_membership_and_missing_access () =
      ~team:(Some { op = `In; values = [ "acme/backend"; "acme/core" ] })
  check_bool "member hit CI" true
    (allows ~filter ~ctx:(ctx ~teams:(Some [ "Acme/Backend" ]) ()));
  check_bool "non-member" false
    (allows ~filter ~ctx:(ctx ~teams:(Some [ "acme/frontend" ]) ()));
  check_bool "known empty membership" false
    (allows ~filter ~ctx:(ctx ~teams:(Some []) ()));
  check_bool "missing team access / not enriched" false
    (allows ~filter ~ctx:(ctx ~teams:None ()));
  (* rate_limited enrichment → None teams *)
      paths = None;
      teams = Some (Error "rate_limited");
      delivery_id = None;
      installation_id = None;
      item_number = Some 2;
      actor = { E.empty_actor with login = Some "carol" };
      after = Some E.empty_safe_state;
      head_sha = None;
  check_bool "teams from rate limit None" true (c.teams = None);
  check_bool "fail closed rate-limited teams" false (allows ~filter ~ctx:c);
  (* access_denied same path *)
  let enr2 = { enr with teams = Some (Error "access_denied") } in
  let c2 = Ev.pr_context_of_envelope ~envelope:env ~enrichment:enr2 () in
  check_bool "fail closed access_denied teams" false (allows ~filter ~ctx:c2);
  (* not_in team: known non-member allows *)
    pr_filter ~team:(Some { op = `Not_in; values = [ "acme/bots" ] }) ()
  check_bool "not_in team ok" true
    (allows ~filter:excl ~ctx:(ctx ~teams:(Some [ "acme/backend" ]) ()));
  check_bool "not_in team hit" false
    (allows ~filter:excl ~ctx:(ctx ~teams:(Some [ "acme/bots" ]) ()))

(** ---- Full positive / negative composition ---- *)

let test_full_positive_negative_composition () =
      ~base_branch:(Some { op = `In; values = [ "main"; "develop" ] })
      ~head_branch:(Some { op = `Glob; values = [ "feature/*" ] })
      ~changed_path:(Some { op = `Glob; values = [ "src/**" ] })
      ~labels:(Some { op = `In; values = [ "ready" ] })
      ~author:(Some { op = `In; values = [ "alice"; "bob" ] })
      ~team:(Some { op = `In; values = [ "acme/backend" ] })
    ctx ~base_branch:(Some "main") ~head_branch:(Some "feature/p20")
      ~changed_paths:(Some [ "src/github_route_filter_eval.ml" ])
      ~labels:[ "ready"; "p20" ] ~author:(Some "Bob")
      ~teams:(Some [ "acme/backend" ]) ~draft:(Some false) ()
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
    pr_filter ~include_events:[ "pull_request" ]
      ~exclude_events:[ "issue_comment" ] ~exclude_repos:[ "acme/secret" ]
      ~labels:(Some { op = `In; values = [ "go" ] })
  let c = ctx ~labels:[ "go" ] () in
  check_bool "baseline+pr ok" true
    (Ev.eval_pr_with_baseline ~filter ~event:"pull_request" ~repo:"acme/widget"
       ~ctx:c ());
  check_bool "event excluded" false
    (Ev.eval_pr_with_baseline ~filter ~event:"issue_comment" ~repo:"acme/widget"
  check_bool "repo excluded" false
    (Ev.eval_pr_with_baseline ~filter ~event:"pull_request" ~repo:"acme/secret"
  check_bool "pr labels fail after baseline" false
       ~ctx:(ctx ~labels:[] ()) ());
  check_bool "family token in include" true
    (Ev.eval_baseline
       ~filter:(pr_filter ~include_events:[ "lifecycle" ] ())
       ~event:"pull_request" ~family:"lifecycle" ())

let test_pr_context_of_envelope_fields () =
      installation_id = Some 9;
      item_number = Some 7;
      actor = { E.empty_actor with login = Some "Dana" };
            labels = [ "a"; "b" ];
            draft = Some true;
            base_ref = Some "develop";
      head_sha = Some "deadbeef";
      paths = Some (Ok [ "src/a.ml"; "src/b.ml" ]);
      teams = Some (Ok [ "acme/backend" ]);
      reasons = [];
      complete = true;
  Alcotest.(check (option string)) "base" (Some "develop") c.base_branch;
  Alcotest.(check (option string)) "author" (Some "Dana") c.author;
  Alcotest.(check (list string)) "labels" [ "a"; "b" ] c.labels;
  Alcotest.(check (option bool)) "draft" (Some true) c.draft;
  Alcotest.(check (option (list string)))
    "paths"
    (Some [ "src/a.ml"; "src/b.ml" ])
    c.changed_paths;
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
      test_rate_limited_and_missing_path_enrichment );
    ( "labels author draft case composition",
      test_labels_author_draft_case_and_composition );
    ( "team membership and missing access",
      test_team_membership_and_missing_access );
    ( "full positive negative composition",
      test_full_positive_negative_composition );
    ("baseline and pr composition", `Quick, test_baseline_and_pr_composition);
    ("pr_context_of_envelope fields", `Quick, test_pr_context_of_envelope_fields);
  ]
||||||| dba8c5b5
(** Tests for Issue advanced filter predicates (P20.M1.E1.T004).

    Shared eval helpers may also cover PR fields (T003). Fixtures here focus on
    Issue labels, author/team, multi-assignee, cleared milestone, deleted
    labels/users, transfer, missing team access, and rate-limited enrichment. *)



let issue_filter ?(labels : F.set_match option = None)
    ?(author : F.set_match option = None) ?(team : F.set_match option = None)
    ?(assignee : F.set_match option = None)
    ?(milestone : F.set_match option = None) ?(include_events = [])
    ?(exclude_events = []) ?(include_repos = []) ?(exclude_repos = []) () : F.t
    =
         issue = { labels; author; team; assignee; milestone };

let ctx ?(labels = []) ?(author = None) ?(teams = None) ?(assignees = [])
    ?(milestone = None) () : Ev.issue_context =
  { labels; author; teams; assignees; milestone }

let allows ~filter ~ctx = Ev.eval_issue ~filter ~ctx ()

let sample_issue_envelope ?(action = "opened") ?(login = Some "alice")
    ?(labels = [ "bug" ]) ?(assignees = [ "bob" ]) ?(milestone = Some "v1.0")
    ?(transfer = None) ?(repo = "acme/widget") () : E.t =
    delivery_id = Some "d-issue-1";
    installation_id = Some 42;
    event = "issues";
    action = Some action;
    repo_full_name = repo;
    item_kind = Some E.Issue;
    item_number = Some 99;
    family = (if action = "transferred" then E.Lifecycle else E.Lifecycle);
    actor = { E.empty_actor with login };
          state = Some "open";
          assignees;
          milestone;
    transfer;

(** ---- Labels / author case and composition ---- *)

let test_labels_author_case_and_composition () =
    issue_filter
      ~labels:(Some { op = `In; values = [ "needs-triage"; "P20" ] })
  let good = ctx ~labels:[ "bug"; "Needs-Triage" ] ~author:(Some "alice") () in
  check_bool "label+author CI" true (allows ~filter ~ctx:good);
    (allows ~filter ~ctx:(ctx ~labels:[ "bug" ] ~author:(Some "alice") ()));
    (allows ~filter ~ctx:(ctx ~labels:[ "p20" ] ~author:(Some "bob") ()));
    (allows ~filter ~ctx:(ctx ~labels:[ "p20" ] ~author:None ()));
  (* not_in labels; empty known labels pass *)
    issue_filter ~labels:(Some { op = `Not_in; values = [ "wontfix" ] }) ()
  check_bool "wontfix rejected CI" false

(** ---- Multi-assignee ---- *)

let test_multi_assignee () =
    issue_filter ~assignee:(Some { op = `In; values = [ "Carol"; "Dana" ] }) ()
  check_bool "one of multi-assignees hits CI" true
    (allows ~filter ~ctx:(ctx ~assignees:[ "alice"; "carol"; "eve" ] ()));
  check_bool "all multi-assignees none match" false
    (allows ~filter ~ctx:(ctx ~assignees:[ "alice"; "bob" ] ()));
  check_bool "single assignee hit" true
    (allows ~filter ~ctx:(ctx ~assignees:[ "DANA" ] ()));
  (* Empty assignee list = known unassigned, not missing *)
  check_bool "empty assignees fail in" false
    (allows ~filter ~ctx:(ctx ~assignees:[] ()));
  let unassigned_only =
    issue_filter ~assignee:(Some { op = `Not_in; values = [ "botservice" ] }) ()
  check_bool "empty assignees pass not_in" true
    (allows ~filter:unassigned_only ~ctx:(ctx ~assignees:[] ()));
  check_bool "botservice on multi list rejected" false
    (allows ~filter:unassigned_only
       ~ctx:(ctx ~assignees:[ "human"; "BotService" ] ()));
  (* eq against multi-valued assignees: intersection with single value *)
  let eq_bob =
    issue_filter ~assignee:(Some { op = `Eq; values = [ "bob" ] }) ()
  check_bool "eq among multi" true
    (allows ~filter:eq_bob ~ctx:(ctx ~assignees:[ "alice"; "Bob" ] ()));
  check_bool "eq miss multi" false
    (allows ~filter:eq_bob ~ctx:(ctx ~assignees:[ "alice" ] ()))

(** ---- Cleared milestone ---- *)

let test_cleared_milestone () =
  let want_v1 =
    issue_filter ~milestone:(Some { op = `Eq; values = [ "v1.0" ] }) ()
  check_bool "milestone title CI hit" true
    (allows ~filter:want_v1 ~ctx:(ctx ~milestone:(Some "V1.0") ()));
  check_bool "cleared milestone fails eq" false
    (allows ~filter:want_v1 ~ctx:(ctx ~milestone:None ()));
  check_bool "wrong milestone fails eq" false
    (allows ~filter:want_v1 ~ctx:(ctx ~milestone:(Some "v2.0") ()));
  let not_v1 =
    issue_filter ~milestone:(Some { op = `Neq; values = [ "v1.0" ] }) ()
  check_bool "cleared milestone passes neq (known absence)" true
    (allows ~filter:not_v1 ~ctx:(ctx ~milestone:None ()));
  check_bool "other title passes neq" true
    (allows ~filter:not_v1 ~ctx:(ctx ~milestone:(Some "v2") ()));
  check_bool "v1 fails neq" false
    (allows ~filter:not_v1 ~ctx:(ctx ~milestone:(Some "V1.0") ()));
  let not_in_closed =
      ~milestone:(Some { op = `Not_in; values = [ "closed-out"; "parked" ] })
  check_bool "cleared passes not_in closed milestones" true
    (allows ~filter:not_in_closed ~ctx:(ctx ~milestone:None ()));
  check_bool "parked rejected" false
    (allows ~filter:not_in_closed ~ctx:(ctx ~milestone:(Some "Parked") ()))

(** ---- Deleted labels / users ---- *)

let test_deleted_labels_and_users () =
  (* Labels removed → known empty list *)
  let needs_bug =
    issue_filter ~labels:(Some { op = `In; values = [ "bug" ] }) ()
  check_bool "deleted all labels fail in" false
    (allows ~filter:needs_bug ~ctx:(ctx ~labels:[] ()));
  let exclude_bug =
    issue_filter ~labels:(Some { op = `Not_in; values = [ "bug" ] }) ()
  check_bool "deleted labels pass not_in" true
    (allows ~filter:exclude_bug ~ctx:(ctx ~labels:[] ()));
  (* Deleted/missing author fails closed when author predicate set *)
  let from_alice =
    issue_filter ~author:(Some { op = `Eq; values = [ "alice" ] }) ()
  check_bool "deleted author fail closed" false
    (allows ~filter:from_alice ~ctx:(ctx ~author:None ()));
  (* Deleted assignee removed from list; remaining multi-assignees still match *)
  let needs_bob =
    issue_filter ~assignee:(Some { op = `In; values = [ "bob" ] }) ()
  check_bool "deleted bob from multi fails" false
    (allows ~filter:needs_bob ~ctx:(ctx ~assignees:[ "carol" ] ()));
  check_bool "bob still present among multi" true
    (allows ~filter:needs_bob ~ctx:(ctx ~assignees:[ "carol"; "bob" ] ()))

(** ---- Team membership / missing access / rate-limited enrichment ---- *)

let test_team_missing_access_and_rate_limit () =
      ~team:(Some { op = `In; values = [ "acme/triage"; "acme/core" ] })
    (allows ~filter ~ctx:(ctx ~teams:(Some [ "Acme/Triage" ]) ()));
  check_bool "missing team access / not enriched fails closed" false
  (* rate_limited enrichment → None teams via issue_context_of_envelope *)
  let enr_rl : En.enrichment =
  let env = sample_issue_envelope () in
  let c = Ev.issue_context_of_envelope ~envelope:env ~enrichment:enr_rl () in
  check_bool "rate_limited teams → None" true (c.teams = None);
  let enr_ad =
      enr_rl with
      teams = Some (Error "access_denied");
      reasons = [ "access_denied" ];
  let c2 = Ev.issue_context_of_envelope ~envelope:env ~enrichment:enr_ad () in
  (* Successful enrichment *)
  let enr_ok : En.enrichment =
      teams = Some (Ok [ "acme/triage" ]);
  let c3 = Ev.issue_context_of_envelope ~envelope:env ~enrichment:enr_ok () in
  check_bool "enriched teams allow" true (allows ~filter ~ctx:c3);
  (* not_in team with known membership *)
    issue_filter ~team:(Some { op = `Not_in; values = [ "acme/bots" ] }) ()
    (allows ~filter:excl ~ctx:(ctx ~teams:(Some [ "acme/triage" ]) ()));
  check_bool "not_in team missing still fail closed" false
    (allows ~filter:excl ~ctx:(ctx ~teams:None ()))

(** ---- Transfer: current state predicates still apply ---- *)

let test_transfer_uses_current_state () =
      ~labels:(Some { op = `In; values = [ "moved" ] })
      ~assignee:(Some { op = `In; values = [ "carol" ] })
      ~milestone:(Some { op = `Eq; values = [ "inbox" ] })
      ~include_events:[ "issues" ] ()
  let env =
    sample_issue_envelope ~action:"transferred" ~login:(Some "alice")
      ~labels:[ "Moved"; "ops" ] ~assignees:[ "Carol"; "dave" ]
      ~milestone:(Some "Inbox")
      ~transfer:
        (Some { E.from_repo = Some "acme/old"; to_repo = Some "acme/widget" })
      ~repo:"acme/widget" ()
  let c = Ev.issue_context_of_envelope ~envelope:env () in
  Alcotest.(check (list string)) "transfer labels" [ "Moved"; "ops" ] c.labels;
  Alcotest.(check (list string))
    "transfer assignees" [ "Carol"; "dave" ] c.assignees;
  Alcotest.(check (option string))
    "transfer milestone" (Some "Inbox") c.milestone;
  check_bool "transfer state matches advanced filter" true
    (allows ~filter ~ctx:c);
  check_bool "transfer + baseline ok" true
    (Ev.eval_issue_with_baseline ~filter ~event:"issues" ~repo:"acme/widget"
  (* After transfer, cleared milestone + missing label fail *)
  let cleared = { c with labels = [ "ops" ]; milestone = None } in
  check_bool "post-transfer cleared milestone / missing label" false
    (allows ~filter ~ctx:cleared)


      ~team:(Some { op = `In; values = [ "acme/triage" ] })
      ~assignee:(Some { op = `In; values = [ "carol"; "dana" ] })
      ~milestone:(Some { op = `Eq; values = [ "v1.0" ] })
    ctx ~labels:[ "ready"; "p20" ] ~author:(Some "Bob")
      ~teams:(Some [ "acme/triage" ]) ~assignees:[ "eve"; "Carol" ]
      ~milestone:(Some "V1.0") ()
  check_bool "neg assignee" false
    (allows ~filter ~ctx:{ good with assignees = [ "eve" ] });
  check_bool "neg milestone" false
    (allows ~filter ~ctx:{ good with milestone = Some "v2" });
  check_bool "cleared milestone neg" false
    (allows ~filter ~ctx:{ good with milestone = None });
  check_bool "missing team fail closed" false
    (allows ~filter ~ctx:{ good with teams = None });


let test_baseline_and_issue_composition () =
    issue_filter ~include_events:[ "issues" ]
  check_bool "baseline+issue ok" true
    (Ev.eval_issue_with_baseline ~filter ~event:"issue_comment"
       ~repo:"acme/widget" ~ctx:c ());
    (Ev.eval_issue_with_baseline ~filter ~event:"issues" ~repo:"acme/secret"
  check_bool "issue labels fail after baseline" false
       ~ctx:(ctx ~labels:[] ()) ())

(** ---- issue_context_of_envelope fields ---- *)

let test_issue_context_of_envelope_fields () =
    sample_issue_envelope ~login:(Some "Dana") ~labels:[ "a"; "b" ]
      ~assignees:[ "x"; "y" ] ~milestone:(Some "M1") ()
  let c = Ev.issue_context_of_envelope ~envelope:env ~enrichment:enr () in
  Alcotest.(check (list string)) "assignees" [ "x"; "y" ] c.assignees;
  Alcotest.(check (option string)) "milestone" (Some "M1") c.milestone;
    "teams" (Some [ "acme/triage" ]) c.teams;
  (* blank milestone title → cleared *)
  let env2 =
    sample_issue_envelope ~milestone:(Some "   ") ~assignees:[] ~labels:[] ()
  let c2 = Ev.issue_context_of_envelope ~envelope:env2 () in
  Alcotest.(check (option string)) "blank milestone cleared" None c2.milestone;
  Alcotest.(check (list string)) "empty assignees" [] c2.assignees

    ( "labels author case composition",
      test_labels_author_case_and_composition );
    ("multi-assignee", `Quick, test_multi_assignee);
    ("cleared milestone", `Quick, test_cleared_milestone);
    ("deleted labels and users", `Quick, test_deleted_labels_and_users);
    ( "team missing access and rate limit",
      test_team_missing_access_and_rate_limit );
    ("transfer uses current state", `Quick, test_transfer_uses_current_state);
    ( "baseline and issue composition",
      test_baseline_and_issue_composition );
    ( "issue_context_of_envelope fields",
      test_issue_context_of_envelope_fields );
