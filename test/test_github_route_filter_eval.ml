(** Tests for Issue advanced filter predicates (P20.M1.E1.T004).

    Shared eval helpers may also cover PR fields (T003). Fixtures here focus on
    Issue labels, author/team, multi-assignee, cleared milestone, deleted
    labels/users, transfer, missing team access, and rate-limited enrichment. *)

module F = Github_route_filter
module Ev = Github_route_filter_eval
module E = Github_event_envelope
module En = Github_filter_enrichment

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let issue_filter ?(labels : F.set_match option = None)
    ?(author : F.set_match option = None) ?(team : F.set_match option = None)
    ?(assignee : F.set_match option = None)
    ?(milestone : F.set_match option = None) ?(include_events = [])
    ?(exclude_events = []) ?(include_repos = []) ?(exclude_repos = []) () : F.t
    =
  assert_ok
    (F.validate
       {
         F.default with
         include_events;
         exclude_events;
         include_repos;
         exclude_repos;
         issue = { labels; author; team; assignee; milestone };
       })

let ctx ?(labels = []) ?(author = None) ?(teams = None) ?(assignees = [])
    ?(milestone = None) () : Ev.issue_context =
  { labels; author; teams; assignees; milestone }

let allows ~filter ~ctx = Ev.eval_issue ~filter ~ctx ()
let check_bool msg expected got = Alcotest.(check bool) msg expected got

let sample_issue_envelope ?(action = "opened") ?(login = Some "alice")
    ?(labels = [ "bug" ]) ?(assignees = [ "bob" ]) ?(milestone = Some "v1.0")
    ?(transfer = None) ?(repo = "acme/widget") () : E.t =
  {
    version = E.envelope_version;
    delivery_id = Some "d-issue-1";
    installation_id = Some 42;
    event = "issues";
    action = Some action;
    repo_full_name = repo;
    org = Some "acme";
    item_kind = Some E.Issue;
    item_number = Some 99;
    item_node_id = None;
    item_url = None;
    html_url = None;
    family = (if action = "transferred" then E.Lifecycle else E.Lifecycle);
    actor = { E.empty_actor with login };
    before = None;
    after =
      Some
        {
          E.empty_safe_state with
          state = Some "open";
          labels;
          assignees;
          milestone;
        };
    transfer;
    received_at = None;
    event_at = None;
    head_sha = None;
    unsupported = false;
    skip_reason = None;
  }

(** ---- Labels / author case and composition ---- *)

let test_labels_author_case_and_composition () =
  let filter =
    issue_filter
      ~labels:(Some { op = `In; values = [ "needs-triage"; "P20" ] })
      ~author:(Some { op = `Eq; values = [ "Alice" ] })
      ()
  in
  let good = ctx ~labels:[ "bug"; "Needs-Triage" ] ~author:(Some "alice") () in
  check_bool "label+author CI" true (allows ~filter ~ctx:good);
  check_bool "label miss" false
    (allows ~filter ~ctx:(ctx ~labels:[ "bug" ] ~author:(Some "alice") ()));
  check_bool "author miss" false
    (allows ~filter ~ctx:(ctx ~labels:[ "p20" ] ~author:(Some "bob") ()));
  check_bool "missing author fail closed" false
    (allows ~filter ~ctx:(ctx ~labels:[ "p20" ] ~author:None ()));
  (* not_in labels; empty known labels pass *)
  let no_wontfix =
    issue_filter ~labels:(Some { op = `Not_in; values = [ "wontfix" ] }) ()
  in
  check_bool "empty labels pass not_in" true
    (allows ~filter:no_wontfix ~ctx:(ctx ~labels:[] ()));
  check_bool "wontfix rejected CI" false
    (allows ~filter:no_wontfix ~ctx:(ctx ~labels:[ "WontFix" ] ()))

(** ---- Multi-assignee ---- *)

let test_multi_assignee () =
  let filter =
    issue_filter ~assignee:(Some { op = `In; values = [ "Carol"; "Dana" ] }) ()
  in
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
  in
  check_bool "empty assignees pass not_in" true
    (allows ~filter:unassigned_only ~ctx:(ctx ~assignees:[] ()));
  check_bool "botservice on multi list rejected" false
    (allows ~filter:unassigned_only
       ~ctx:(ctx ~assignees:[ "human"; "BotService" ] ()));
  (* eq against multi-valued assignees: intersection with single value *)
  let eq_bob =
    issue_filter ~assignee:(Some { op = `Eq; values = [ "bob" ] }) ()
  in
  check_bool "eq among multi" true
    (allows ~filter:eq_bob ~ctx:(ctx ~assignees:[ "alice"; "Bob" ] ()));
  check_bool "eq miss multi" false
    (allows ~filter:eq_bob ~ctx:(ctx ~assignees:[ "alice" ] ()))

(** ---- Cleared milestone ---- *)

let test_cleared_milestone () =
  let want_v1 =
    issue_filter ~milestone:(Some { op = `Eq; values = [ "v1.0" ] }) ()
  in
  check_bool "milestone title CI hit" true
    (allows ~filter:want_v1 ~ctx:(ctx ~milestone:(Some "V1.0") ()));
  check_bool "cleared milestone fails eq" false
    (allows ~filter:want_v1 ~ctx:(ctx ~milestone:None ()));
  check_bool "wrong milestone fails eq" false
    (allows ~filter:want_v1 ~ctx:(ctx ~milestone:(Some "v2.0") ()));
  let not_v1 =
    issue_filter ~milestone:(Some { op = `Neq; values = [ "v1.0" ] }) ()
  in
  check_bool "cleared milestone passes neq (known absence)" true
    (allows ~filter:not_v1 ~ctx:(ctx ~milestone:None ()));
  check_bool "other title passes neq" true
    (allows ~filter:not_v1 ~ctx:(ctx ~milestone:(Some "v2") ()));
  check_bool "v1 fails neq" false
    (allows ~filter:not_v1 ~ctx:(ctx ~milestone:(Some "V1.0") ()));
  let not_in_closed =
    issue_filter
      ~milestone:(Some { op = `Not_in; values = [ "closed-out"; "parked" ] })
      ()
  in
  check_bool "cleared passes not_in closed milestones" true
    (allows ~filter:not_in_closed ~ctx:(ctx ~milestone:None ()));
  check_bool "parked rejected" false
    (allows ~filter:not_in_closed ~ctx:(ctx ~milestone:(Some "Parked") ()))

(** ---- Deleted labels / users ---- *)

let test_deleted_labels_and_users () =
  (* Labels removed → known empty list *)
  let needs_bug =
    issue_filter ~labels:(Some { op = `In; values = [ "bug" ] }) ()
  in
  check_bool "deleted all labels fail in" false
    (allows ~filter:needs_bug ~ctx:(ctx ~labels:[] ()));
  let exclude_bug =
    issue_filter ~labels:(Some { op = `Not_in; values = [ "bug" ] }) ()
  in
  check_bool "deleted labels pass not_in" true
    (allows ~filter:exclude_bug ~ctx:(ctx ~labels:[] ()));
  (* Deleted/missing author fails closed when author predicate set *)
  let from_alice =
    issue_filter ~author:(Some { op = `Eq; values = [ "alice" ] }) ()
  in
  check_bool "deleted author fail closed" false
    (allows ~filter:from_alice ~ctx:(ctx ~author:None ()));
  (* Deleted assignee removed from list; remaining multi-assignees still match *)
  let needs_bob =
    issue_filter ~assignee:(Some { op = `In; values = [ "bob" ] }) ()
  in
  check_bool "deleted bob from multi fails" false
    (allows ~filter:needs_bob ~ctx:(ctx ~assignees:[ "carol" ] ()));
  check_bool "bob still present among multi" true
    (allows ~filter:needs_bob ~ctx:(ctx ~assignees:[ "carol"; "bob" ] ()))

(** ---- Team membership / missing access / rate-limited enrichment ---- *)

let test_team_missing_access_and_rate_limit () =
  let filter =
    issue_filter
      ~team:(Some { op = `In; values = [ "acme/triage"; "acme/core" ] })
      ()
  in
  check_bool "member hit CI" true
    (allows ~filter ~ctx:(ctx ~teams:(Some [ "Acme/Triage" ]) ()));
  check_bool "non-member" false
    (allows ~filter ~ctx:(ctx ~teams:(Some [ "acme/frontend" ]) ()));
  check_bool "known empty membership" false
    (allows ~filter ~ctx:(ctx ~teams:(Some []) ()));
  check_bool "missing team access / not enriched fails closed" false
    (allows ~filter ~ctx:(ctx ~teams:None ()));
  (* rate_limited enrichment → None teams via issue_context_of_envelope *)
  let enr_rl : En.enrichment =
    {
      paths = None;
      teams = Some (Error "rate_limited");
      reasons = [ "rate_limited" ];
      complete = false;
    }
  in
  let env = sample_issue_envelope () in
  let c = Ev.issue_context_of_envelope ~envelope:env ~enrichment:enr_rl () in
  check_bool "rate_limited teams → None" true (c.teams = None);
  check_bool "fail closed rate-limited teams" false (allows ~filter ~ctx:c);
  let enr_ad =
    {
      enr_rl with
      teams = Some (Error "access_denied");
      reasons = [ "access_denied" ];
    }
  in
  let c2 = Ev.issue_context_of_envelope ~envelope:env ~enrichment:enr_ad () in
  check_bool "fail closed access_denied teams" false (allows ~filter ~ctx:c2);
  (* Successful enrichment *)
  let enr_ok : En.enrichment =
    {
      paths = None;
      teams = Some (Ok [ "acme/triage" ]);
      reasons = [];
      complete = true;
    }
  in
  let c3 = Ev.issue_context_of_envelope ~envelope:env ~enrichment:enr_ok () in
  check_bool "enriched teams allow" true (allows ~filter ~ctx:c3);
  (* not_in team with known membership *)
  let excl =
    issue_filter ~team:(Some { op = `Not_in; values = [ "acme/bots" ] }) ()
  in
  check_bool "not_in team ok" true
    (allows ~filter:excl ~ctx:(ctx ~teams:(Some [ "acme/triage" ]) ()));
  check_bool "not_in team missing still fail closed" false
    (allows ~filter:excl ~ctx:(ctx ~teams:None ()))

(** ---- Transfer: current state predicates still apply ---- *)

let test_transfer_uses_current_state () =
  let filter =
    issue_filter
      ~labels:(Some { op = `In; values = [ "moved" ] })
      ~assignee:(Some { op = `In; values = [ "carol" ] })
      ~milestone:(Some { op = `Eq; values = [ "inbox" ] })
      ~include_events:[ "issues" ] ()
  in
  let env =
    sample_issue_envelope ~action:"transferred" ~login:(Some "alice")
      ~labels:[ "Moved"; "ops" ] ~assignees:[ "Carol"; "dave" ]
      ~milestone:(Some "Inbox")
      ~transfer:
        (Some { E.from_repo = Some "acme/old"; to_repo = Some "acme/widget" })
      ~repo:"acme/widget" ()
  in
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
       ~ctx:c ());
  (* After transfer, cleared milestone + missing label fail *)
  let cleared = { c with labels = [ "ops" ]; milestone = None } in
  check_bool "post-transfer cleared milestone / missing label" false
    (allows ~filter ~ctx:cleared)

(** ---- Full positive / negative composition ---- *)

let test_full_positive_negative_composition () =
  let filter =
    issue_filter
      ~labels:(Some { op = `In; values = [ "ready" ] })
      ~author:(Some { op = `In; values = [ "alice"; "bob" ] })
      ~team:(Some { op = `In; values = [ "acme/triage" ] })
      ~assignee:(Some { op = `In; values = [ "carol"; "dana" ] })
      ~milestone:(Some { op = `Eq; values = [ "v1.0" ] })
      ()
  in
  let good =
    ctx ~labels:[ "ready"; "p20" ] ~author:(Some "Bob")
      ~teams:(Some [ "acme/triage" ]) ~assignees:[ "eve"; "Carol" ]
      ~milestone:(Some "V1.0") ()
  in
  check_bool "full positive" true (allows ~filter ~ctx:good);
  check_bool "neg labels" false
    (allows ~filter ~ctx:{ good with labels = [ "wip" ] });
  check_bool "neg author" false
    (allows ~filter ~ctx:{ good with author = Some "eve" });
  check_bool "neg team" false
    (allows ~filter ~ctx:{ good with teams = Some [ "acme/other" ] });
  check_bool "neg assignee" false
    (allows ~filter ~ctx:{ good with assignees = [ "eve" ] });
  check_bool "neg milestone" false
    (allows ~filter ~ctx:{ good with milestone = Some "v2" });
  check_bool "cleared milestone neg" false
    (allows ~filter ~ctx:{ good with milestone = None });
  check_bool "missing team fail closed" false
    (allows ~filter ~ctx:{ good with teams = None });
  check_bool "empty advanced allow" true
    (allows ~filter:F.default ~ctx:(ctx ()))

(** ---- Baseline composition ---- *)

let test_baseline_and_issue_composition () =
  let filter =
    issue_filter ~include_events:[ "issues" ]
      ~exclude_events:[ "issue_comment" ] ~exclude_repos:[ "acme/secret" ]
      ~labels:(Some { op = `In; values = [ "go" ] })
      ()
  in
  let c = ctx ~labels:[ "go" ] () in
  check_bool "baseline+issue ok" true
    (Ev.eval_issue_with_baseline ~filter ~event:"issues" ~repo:"acme/widget"
       ~ctx:c ());
  check_bool "event excluded" false
    (Ev.eval_issue_with_baseline ~filter ~event:"issue_comment"
       ~repo:"acme/widget" ~ctx:c ());
  check_bool "repo excluded" false
    (Ev.eval_issue_with_baseline ~filter ~event:"issues" ~repo:"acme/secret"
       ~ctx:c ());
  check_bool "issue labels fail after baseline" false
    (Ev.eval_issue_with_baseline ~filter ~event:"issues" ~repo:"acme/widget"
       ~ctx:(ctx ~labels:[] ()) ())

(** ---- issue_context_of_envelope fields ---- *)

let test_issue_context_of_envelope_fields () =
  let env =
    sample_issue_envelope ~login:(Some "Dana") ~labels:[ "a"; "b" ]
      ~assignees:[ "x"; "y" ] ~milestone:(Some "M1") ()
  in
  let enr : En.enrichment =
    {
      paths = None;
      teams = Some (Ok [ "acme/triage" ]);
      reasons = [];
      complete = true;
    }
  in
  let c = Ev.issue_context_of_envelope ~envelope:env ~enrichment:enr () in
  Alcotest.(check (option string)) "author" (Some "Dana") c.author;
  Alcotest.(check (list string)) "labels" [ "a"; "b" ] c.labels;
  Alcotest.(check (list string)) "assignees" [ "x"; "y" ] c.assignees;
  Alcotest.(check (option string)) "milestone" (Some "M1") c.milestone;
  Alcotest.(check (option (list string)))
    "teams" (Some [ "acme/triage" ]) c.teams;
  (* blank milestone title → cleared *)
  let env2 =
    sample_issue_envelope ~milestone:(Some "   ") ~assignees:[] ~labels:[] ()
  in
  let c2 = Ev.issue_context_of_envelope ~envelope:env2 () in
  Alcotest.(check (option string)) "blank milestone cleared" None c2.milestone;
  Alcotest.(check (list string)) "empty assignees" [] c2.assignees

let suite =
  [
    ( "labels author case composition",
      `Quick,
      test_labels_author_case_and_composition );
    ("multi-assignee", `Quick, test_multi_assignee);
    ("cleared milestone", `Quick, test_cleared_milestone);
    ("deleted labels and users", `Quick, test_deleted_labels_and_users);
    ( "team missing access and rate limit",
      `Quick,
      test_team_missing_access_and_rate_limit );
    ("transfer uses current state", `Quick, test_transfer_uses_current_state);
    ( "full positive negative composition",
      `Quick,
      test_full_positive_negative_composition );
    ( "baseline and issue composition",
      `Quick,
      test_baseline_and_issue_composition );
    ( "issue_context_of_envelope fields",
      `Quick,
      test_issue_context_of_envelope_fields );
  ]
