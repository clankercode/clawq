(** Tests for normalized CI, review, and mergeability event types. *)

let ci_summary_of_check_run () =
  let event =
    Github_webhook.CheckRun
      {
        owner = "acme";
        repo = "backend";
        name = "test";
        status = "completed";
        conclusion = "failure";
        pr_number = Some 42;
        html_url = "https://github.com/acme/backend/actions";
        head_sha = "abc123";
        actor = "bot";
        details_url = "";
      }
  in
  match Github_webhook.ci_summary_of_event event with
  | Some ci ->
      Alcotest.(check bool) "kind" true (ci.kind = `CheckRun);
      Alcotest.(check string) "name" "test" ci.name;
      Alcotest.(check string) "status" "completed" ci.status;
      Alcotest.(check string) "conclusion" "failure" ci.conclusion;
      Alcotest.(check (option int)) "pr_number" (Some 42) ci.pr_number
  | None -> Alcotest.fail "expected Some ci_summary"

let ci_summary_of_workflow_run () =
  let event =
    Github_webhook.WorkflowRun
      {
        owner = "acme";
        repo = "backend";
        name = "ci";
        status = "completed";
        conclusion = "success";
        pr_number = None;
        html_url = "https://github.com/acme/backend/actions/runs/55";
        head_sha = "def456";
        actor = "ci-bot";
      }
  in
  match Github_webhook.ci_summary_of_event event with
  | Some ci ->
      Alcotest.(check bool) "kind" true (ci.kind = `WorkflowRun);
      Alcotest.(check string) "name" "ci" ci.name;
      Alcotest.(check (option int)) "pr_number" None ci.pr_number
  | None -> Alcotest.fail "expected Some ci_summary"

let ci_summary_of_check_suite () =
  let event =
    Github_webhook.CheckSuite
      {
        owner = "acme";
        repo = "backend";
        status = "completed";
        conclusion = "success";
        pr_number = Some 10;
        html_url = "https://github.com/acme/backend";
        head_sha = "ghi789";
        actor = "ci-bot";
      }
  in
  match Github_webhook.ci_summary_of_event event with
  | Some ci ->
      Alcotest.(check bool) "kind" true (ci.kind = `CheckSuite);
      Alcotest.(check string) "name" "" ci.name;
      Alcotest.(check (option int)) "pr_number" (Some 10) ci.pr_number
  | None -> Alcotest.fail "expected Some ci_summary"

let ci_summary_of_non_ci_event () =
  let event =
    Github_webhook.PullRequest
      {
        action = "opened";
        owner = "acme";
        repo = "backend";
        pr_number = 42;
        pr_title = "Fix";
        pr_body = "";
        pr_author = "alice";
        base_branch = "main";
        head_branch = "fix";
        html_url = "";
      }
  in
  match Github_webhook.ci_summary_of_event event with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for non-CI event"

let review_summary_of_approved () =
  let body =
    {|{"action":"submitted","review":{"id":1,"user":{"login":"bob"},"state":"approved","body":"LGTM"},"pull_request":{"number":42},"repository":{"name":"backend","owner":{"login":"acme"}}}|}
  in
  match Github_webhook.parse_event ~event_type:"pull_request_review" ~body with
  | Github_webhook.PullRequestReview _ as event -> (
      match Github_webhook.review_summary_of_event event with
      | Some review ->
          Alcotest.(check bool)
            "state" true
            (review.state = Github_webhook.Approved);
          Alcotest.(check string) "reviewer" "bob" review.reviewer;
          Alcotest.(check string) "body" "LGTM" review.body;
          Alcotest.(check int) "pr_number" 42 review.pr_number
      | None -> Alcotest.fail "expected Some review_summary")
  | _ -> Alcotest.fail "expected PullRequestReview"

let review_summary_of_changes_requested () =
  let body =
    {|{"action":"submitted","review":{"id":2,"user":{"login":"carol"},"state":"changes_requested","body":"fix this"},"pull_request":{"number":7},"repository":{"name":"repo","owner":{"login":"o"}}}|}
  in
  match Github_webhook.parse_event ~event_type:"pull_request_review" ~body with
  | Github_webhook.PullRequestReview _ as event -> (
      match Github_webhook.review_summary_of_event event with
      | Some review ->
          Alcotest.(check bool)
            "state" true
            (review.state = Github_webhook.ChangesRequested);
          Alcotest.(check string)
            "raw_state" "changes_requested" review.raw_state
      | None -> Alcotest.fail "expected Some review_summary")
  | _ -> Alcotest.fail "expected PullRequestReview"

let review_comment_json =
  {|{"action":"created","comment":{"id":284312630,"user":{"login":"Codertocat"},"body":"Maybe you should use more emoji.\n/clawq what do you think?","diff_hunk":"@@ -1 +1 @@\n-# Hello-World","path":"README.md","html_url":"https://github.com/Codertocat/Hello-World/pull/2#discussion_r284312630"},"pull_request":{"number":2,"title":"Update the README","body":"Simple change","state":"open","html_url":"https://github.com/Codertocat/Hello-World/pull/2","user":{"login":"Codertocat"},"base":{"ref":"master"},"head":{"ref":"changes"}},"repository":{"name":"Hello-World","owner":{"login":"Codertocat"}}}|}

let review_summary_of_review_comment () =
  match
    Github_webhook.parse_event ~event_type:"pull_request_review_comment"
      ~body:review_comment_json
  with
  | Github_webhook.PrReviewComment _ as event -> (
      match Github_webhook.review_summary_of_event event with
      | Some review ->
          Alcotest.(check bool)
            "state" true
            (review.state = Github_webhook.Commented);
          Alcotest.(check string) "raw_state" "commented" review.raw_state;
          Alcotest.(check string) "reviewer" "Codertocat" review.reviewer;
          Alcotest.(check int) "pr_number" 2 review.pr_number
      | None -> Alcotest.fail "expected Some review_summary")
  | _ -> Alcotest.fail "expected PrReviewComment"

let review_summary_of_non_review_event () =
  let event =
    Github_webhook.CheckRun
      {
        owner = "acme";
        repo = "backend";
        name = "test";
        status = "completed";
        conclusion = "success";
        pr_number = None;
        html_url = "";
        head_sha = "";
        actor = "";
        details_url = "";
      }
  in
  match Github_webhook.review_summary_of_event event with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for non-review event"

let mergeability_detects_label_changes () =
  let body =
    {|{"action":"labeled","pull_request":{"number":1},"label":{"name":"bug"},"changes":{"labels":{"added":[{"name":"bug"}],"removed":[]}},"repository":{"name":"r","owner":{"login":"o"}}}|}
  in
  let changes =
    Github_webhook.detect_mergeability_changes ~event_type:"pull_request" ~body
  in
  Alcotest.(check bool) "has changes" true (List.length changes > 0)

let mergeability_detects_mergeable_state () =
  let body =
    {|{"action":"synchronize","pull_request":{"number":1,"mergeable":true},"repository":{"name":"r","owner":{"login":"o"}}}|}
  in
  let changes =
    Github_webhook.detect_mergeability_changes ~event_type:"pull_request" ~body
  in
  let has_mergeable =
    List.exists
      (function
        | Github_webhook.MergeableStateChanged { mergeable } -> mergeable
        | _ -> false)
      changes
  in
  Alcotest.(check bool) "mergeable detected" true has_mergeable

let mergeability_detects_checks_status () =
  let body =
    {|{"action":"completed","check_run":{"status":"completed","conclusion":"failure"},"repository":{"name":"r","owner":{"login":"o"}}}|}
  in
  let changes =
    Github_webhook.detect_mergeability_changes ~event_type:"check_run" ~body
  in
  let has_checks =
    List.exists
      (function Github_webhook.ChecksStatusChanged _ -> true | _ -> false)
      changes
  in
  Alcotest.(check bool) "checks status detected" true has_checks

let mergeability_non_pr_event () =
  let changes =
    Github_webhook.detect_mergeability_changes ~event_type:"push" ~body:"{}"
  in
  Alcotest.(check int) "no changes" 0 (List.length changes)

let mergeability_detects_labeled_action () =
  let body =
    {|{"action":"labeled","pull_request":{"number":1},"label":{"name":"bug"},"repository":{"name":"r","owner":{"login":"o"}}}|}
  in
  let changes =
    Github_webhook.detect_mergeability_changes ~event_type:"pull_request" ~body
  in
  let has_label =
    List.exists
      (function
        | Github_webhook.LabelsChanged { added; _ } -> List.mem "bug" added
        | _ -> false)
      changes
  in
  Alcotest.(check bool) "labeled detected" true has_label

let mergeability_detects_unlabeled_action () =
  let body =
    {|{"action":"unlabeled","pull_request":{"number":1},"label":{"name":"bug"},"repository":{"name":"r","owner":{"login":"o"}}}|}
  in
  let changes =
    Github_webhook.detect_mergeability_changes ~event_type:"pull_request" ~body
  in
  let has_label =
    List.exists
      (function
        | Github_webhook.LabelsChanged { removed; _ } -> List.mem "bug" removed
        | _ -> false)
      changes
  in
  Alcotest.(check bool) "unlabeled detected" true has_label

let parse_review_requested () =
  let body =
    {|{"action":"review_requested","pull_request":{"number":42,"title":"Fix bug","body":"desc","state":"open","html_url":"https://github.com/acme/backend/pull/42","user":{"login":"alice"},"base":{"ref":"main"},"head":{"ref":"fix"},"requested_reviewers":[{"login":"bob"}]},"repository":{"name":"backend","owner":{"login":"acme"}},"sender":{"login":"alice"}}|}
  in
  match Github_webhook.parse_event ~event_type:"pull_request" ~body with
  | Github_webhook.PullRequest e ->
      Alcotest.(check string) "action" "review_requested" e.action;
      Alcotest.(check int) "pr_number" 42 e.pr_number;
      Alcotest.(check string) "author" "alice" e.pr_author
  | _ -> Alcotest.fail "expected PullRequest for review_requested"

let parse_review_request_removed () =
  let body =
    {|{"action":"review_request_removed","pull_request":{"number":42,"title":"Fix bug","body":"desc","state":"open","html_url":"https://github.com/acme/backend/pull/42","user":{"login":"alice"},"base":{"ref":"main"},"head":{"ref":"fix"}},"repository":{"name":"backend","owner":{"login":"acme"}},"sender":{"login":"alice"}}|}
  in
  match Github_webhook.parse_event ~event_type:"pull_request" ~body with
  | Github_webhook.PullRequest e ->
      Alcotest.(check string) "action" "review_request_removed" e.action;
      Alcotest.(check int) "pr_number" 42 e.pr_number
  | _ -> Alcotest.fail "expected PullRequest for review_request_removed"

let mergeability_detects_in_progress_check_run () =
  let body =
    {|{"action":"created","check_run":{"status":"in_progress","conclusion":""},"repository":{"name":"r","owner":{"login":"o"}}}|}
  in
  let changes =
    Github_webhook.detect_mergeability_changes ~event_type:"check_run" ~body
  in
  let has_pending =
    List.exists
      (function
        | Github_webhook.ChecksStatusChanged { pending; _ } -> pending = 1
        | _ -> false)
      changes
  in
  Alcotest.(check bool) "in_progress detected as pending" true has_pending

let mergeability_detects_queued_check_run () =
  let body =
    {|{"action":"created","check_run":{"status":"queued","conclusion":""},"repository":{"name":"r","owner":{"login":"o"}}}|}
  in
  let changes =
    Github_webhook.detect_mergeability_changes ~event_type:"check_run" ~body
  in
  let has_pending =
    List.exists
      (function
        | Github_webhook.ChecksStatusChanged { pending; _ } -> pending = 1
        | _ -> false)
      changes
  in
  Alcotest.(check bool) "queued detected as pending" true has_pending

let mergeability_detects_in_progress_workflow_run () =
  let body =
    {|{"action":"in_progress","workflow_run":{"status":"in_progress","conclusion":""},"repository":{"name":"r","owner":{"login":"o"}}}|}
  in
  let changes =
    Github_webhook.detect_mergeability_changes ~event_type:"workflow_run" ~body
  in
  let has_pending =
    List.exists
      (function
        | Github_webhook.ChecksStatusChanged { pending; _ } -> pending = 1
        | _ -> false)
      changes
  in
  Alcotest.(check bool)
    "in_progress workflow detected as pending" true has_pending

let ci_summary_suite =
  [
    Alcotest.test_case "check run summary" `Quick ci_summary_of_check_run;
    Alcotest.test_case "workflow run summary" `Quick ci_summary_of_workflow_run;
    Alcotest.test_case "check suite summary" `Quick ci_summary_of_check_suite;
    Alcotest.test_case "non-CI event returns None" `Quick
      ci_summary_of_non_ci_event;
  ]

let review_summary_suite =
  [
    Alcotest.test_case "approved review" `Quick review_summary_of_approved;
    Alcotest.test_case "changes requested review" `Quick
      review_summary_of_changes_requested;
    Alcotest.test_case "review comment" `Quick review_summary_of_review_comment;
    Alcotest.test_case "non-review event returns None" `Quick
      review_summary_of_non_review_event;
  ]

let mergeability_suite =
  [
    Alcotest.test_case "label changes" `Quick mergeability_detects_label_changes;
    Alcotest.test_case "mergeable state" `Quick
      mergeability_detects_mergeable_state;
    Alcotest.test_case "checks status" `Quick mergeability_detects_checks_status;
    Alcotest.test_case "non-PR event" `Quick mergeability_non_pr_event;
    Alcotest.test_case "labeled action" `Quick
      mergeability_detects_labeled_action;
    Alcotest.test_case "unlabeled action" `Quick
      mergeability_detects_unlabeled_action;
    Alcotest.test_case "in_progress check_run" `Quick
      mergeability_detects_in_progress_check_run;
    Alcotest.test_case "queued check_run" `Quick
      mergeability_detects_queued_check_run;
    Alcotest.test_case "in_progress workflow_run" `Quick
      mergeability_detects_in_progress_workflow_run;
  ]

let parse_review_suite =
  [
    Alcotest.test_case "review requested" `Quick parse_review_requested;
    Alcotest.test_case "review request removed" `Quick
      parse_review_request_removed;
  ]
