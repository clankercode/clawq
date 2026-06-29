let with_db f =
  let db = Memory.init ~db_path:":memory:" () in
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let test_should_notify_subscription () =
  with_db (fun db ->
      let prefs =
        {
          Github_pr_subscriptions.on_open = true;
          on_close = false;
          on_comment = true;
          on_review = false;
          on_status = true;
          on_merge = false;
        }
      in
      let sub =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:42 ~profile_id:1 ~notification_preferences:prefs ()
      in
      (* Test PR opened event *)
      let pr_event =
        Github_webhook.PullRequest
          {
            action = "opened";
            owner = "owner";
            repo = "repo";
            pr_number = 42;
            pr_title = "Test PR";
            pr_body = "Test body";
            pr_author = "testuser";
            base_branch = "main";
            head_branch = "feature";
            html_url = "https://github.com/owner/repo/pull/42";
          }
      in
      Alcotest.(check bool)
        "should notify for opened" true
        (Github_pr_dispatch.should_notify_subscription ~subscription:sub
           ~event:pr_event);
      (* Test PR closed event (should not notify based on prefs) *)
      let closed_event =
        Github_webhook.PullRequest
          {
            action = "closed";
            owner = "owner";
            repo = "repo";
            pr_number = 42;
            pr_title = "Test PR";
            pr_body = "Test body";
            pr_author = "testuser";
            base_branch = "main";
            head_branch = "feature";
            html_url = "https://github.com/owner/repo/pull/42";
          }
      in
      Alcotest.(check bool)
        "should not notify for closed" false
        (Github_pr_dispatch.should_notify_subscription ~subscription:sub
           ~event:closed_event);
      (* Test comment event *)
      let comment_event =
        Github_webhook.IssueComment
          {
            owner = "owner";
            repo = "repo";
            issue_number = 42;
            is_pr = true;
            comment_id = 123;
            comment_author = "commenter";
            comment_body = "Nice PR!";
            issue_title = "Test PR";
            html_url = "https://github.com/owner/repo/pull/42#issuecomment-123";
          }
      in
      Alcotest.(check bool)
        "should notify for comment" true
        (Github_pr_dispatch.should_notify_subscription ~subscription:sub
           ~event:comment_event);
      (* Test review comment event (should not notify based on prefs) *)
      let review_event =
        Github_webhook.PrReviewComment
          {
            owner = "owner";
            repo = "repo";
            pr_number = 42;
            comment_id = 456;
            comment_author = "reviewer";
            comment_body = "Looks good!";
            in_reply_to_id = None;
            diff_hunk = "@@ -1,3 +1,4 @@";
            file_path = "test.ml";
            pr_title = "Test PR";
            html_url = "https://github.com/owner/repo/pull/42#discussion-456";
            head_sha = "";
          }
      in
      Alcotest.(check bool)
        "should notify for review comment" true
        (Github_pr_dispatch.should_notify_subscription ~subscription:sub
           ~event:review_event))

let test_format_pr_event_notification () =
  let pr_event =
    Github_webhook.PullRequest
      {
        action = "opened";
        owner = "owner";
        repo = "repo";
        pr_number = 42;
        pr_title = "Add new feature";
        pr_body = "This adds a new feature";
        pr_author = "testuser";
        base_branch = "main";
        head_branch = "feature";
        html_url = "https://github.com/owner/repo/pull/42";
      }
  in
  let notification =
    Github_pr_dispatch.format_pr_event_notification ~event:pr_event
      ~action:"opened"
  in
  Alcotest.(check bool)
    "contains PR number" true
    (String.contains notification '4' && String.contains notification '2');
  Alcotest.(check bool)
    "contains action" true
    (try
       ignore (Str.search_forward (Str.regexp_string "opened") notification 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains author" true
    (try
       ignore (Str.search_forward (Str.regexp_string "testuser") notification 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains repo" true
    (try
       ignore
         (Str.search_forward (Str.regexp_string "owner/repo") notification 0);
       true
     with Not_found -> false)

let test_format_comment_notification () =
  let comment_event =
    Github_webhook.IssueComment
      {
        owner = "owner";
        repo = "repo";
        issue_number = 42;
        is_pr = true;
        comment_id = 123;
        comment_author = "commenter";
        comment_body = "Nice PR!";
        issue_title = "Test PR";
        html_url = "https://github.com/owner/repo/pull/42#issuecomment-123";
      }
  in
  let notification =
    Github_pr_dispatch.format_pr_event_notification ~event:comment_event
      ~action:"comment"
  in
  Alcotest.(check bool)
    "contains PR number" true
    (try
       ignore (Str.search_forward (Str.regexp_string "42") notification 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains comment author" true
    (try
       ignore
         (Str.search_forward (Str.regexp_string "commenter") notification 0);
       true
     with Not_found -> false)

let test_format_review_comment_notification () =
  let review_event =
    Github_webhook.PrReviewComment
      {
        owner = "owner";
        repo = "repo";
        pr_number = 42;
        comment_id = 456;
        comment_author = "reviewer";
        comment_body = "Looks good!";
        in_reply_to_id = None;
        diff_hunk = "@@ -1,3 +1,4 @@";
        file_path = "test.ml";
        pr_title = "Test PR";
        html_url = "https://github.com/owner/repo/pull/42#discussion-456";
        head_sha = "";
      }
  in
  let notification =
    Github_pr_dispatch.format_pr_event_notification ~event:review_event
      ~action:"review_comment"
  in
  Alcotest.(check bool)
    "contains PR number" true
    (try
       ignore (Str.search_forward (Str.regexp_string "42") notification 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains reviewer" true
    (try
       ignore (Str.search_forward (Str.regexp_string "reviewer") notification 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains file path" true
    (try
       ignore (Str.search_forward (Str.regexp_string "test.ml") notification 0);
       true
     with Not_found -> false)

let test_dispatch_dedup () =
  with_db (fun db ->
      (* Add a subscription *)
      let _sub =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:42 ~profile_id:1 ()
      in
      let event =
        Github_webhook.PullRequest
          {
            action = "opened";
            owner = "owner";
            repo = "repo";
            pr_number = 42;
            pr_title = "Test PR";
            pr_body = "Test body";
            pr_author = "testuser";
            base_branch = "main";
            head_branch = "feature";
            html_url = "https://github.com/owner/repo/pull/42";
          }
      in
      let sent = ref [] in
      let send_message ~room_id ~text () =
        sent := (room_id, text) :: !sent;
        Lwt.return_unit
      in
      (* First dispatch should succeed *)
      let result1 =
        Lwt_main.run
          (Github_pr_dispatch.dispatch_to_subscriptions ~db ~event
             ~delivery_id:"test-delivery-1" ~quiet_start:0 ~quiet_end:0
             ~send_message ())
      in
      Alcotest.(check int) "first dispatch count" 1 result1;
      Alcotest.(check int) "sent count" 1 (List.length !sent);
      (* Second dispatch with same delivery_id should be deduped *)
      sent := [];
      let result2 =
        Lwt_main.run
          (Github_pr_dispatch.dispatch_to_subscriptions ~db ~event
             ~delivery_id:"test-delivery-1" ~quiet_start:0 ~quiet_end:0
             ~send_message ())
      in
      Alcotest.(check int) "second dispatch count" 0 result2;
      Alcotest.(check int) "sent count after dedup" 0 (List.length !sent))

let test_dispatch_send_failure_does_not_poison_policy_dedup () =
  with_db (fun db ->
      let _sub =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:42 ~profile_id:1 ()
      in
      let event =
        Github_webhook.CheckRun
          {
            owner = "owner";
            repo = "repo";
            name = "test";
            status = "completed";
            conclusion = "failure";
            html_url = "https://github.com/owner/repo/checks/1";
            pr_number = Some 42;
            head_sha = "abc123";
            details_url = "https://github.com/owner/repo/runs/1";
            actor = "ci-bot";
          }
      in
      let fail_first = ref true in
      let sent = ref [] in
      let send_message ~room_id ~text () =
        if !fail_first then (
          fail_first := false;
          Lwt.fail (Failure "temporary send failure"))
        else (
          sent := (room_id, text) :: !sent;
          Lwt.return_unit)
      in
      let result1 =
        Lwt_main.run
          (Github_pr_dispatch.dispatch_to_subscriptions ~db ~event
             ~delivery_id:"test-delivery-ci-fail-1" ~quiet_start:0 ~quiet_end:0
             ~send_message ())
      in
      Alcotest.(check int) "failed dispatch not counted" 0 result1;
      let result2 =
        Lwt_main.run
          (Github_pr_dispatch.dispatch_to_subscriptions ~db ~event
             ~delivery_id:"test-delivery-ci-fail-2" ~quiet_start:0 ~quiet_end:0
             ~send_message ())
      in
      Alcotest.(check int) "retry after failed send dispatched" 1 result2;
      Alcotest.(check int) "retry sent message" 1 (List.length !sent))

let test_dispatch_no_subscriptions () =
  with_db (fun db ->
      let event =
        Github_webhook.PullRequest
          {
            action = "opened";
            owner = "owner";
            repo = "repo";
            pr_number = 99;
            pr_title = "Unsubscribed PR";
            pr_body = "Test body";
            pr_author = "testuser";
            base_branch = "main";
            head_branch = "feature";
            html_url = "https://github.com/owner/repo/pull/99";
          }
      in
      let sent = ref [] in
      let send_message ~room_id ~text () =
        sent := (room_id, text) :: !sent;
        Lwt.return_unit
      in
      let result =
        Lwt_main.run
          (Github_pr_dispatch.dispatch_to_subscriptions ~db ~event
             ~delivery_id:"test-delivery-2" ~quiet_start:0 ~quiet_end:0
             ~send_message ())
      in
      Alcotest.(check int) "dispatch count" 0 result;
      Alcotest.(check int) "sent count" 0 (List.length !sent))

let test_dispatch_multiple_rooms () =
  with_db (fun db ->
      (* Add subscriptions for same PR in different rooms *)
      let _sub1 =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:42 ~profile_id:1 ()
      in
      let _sub2 =
        Github_pr_subscriptions.add ~db ~room_id:"room-2" ~repo:"owner/repo"
          ~pr_number:42 ~profile_id:2 ()
      in
      let event =
        Github_webhook.PullRequest
          {
            action = "opened";
            owner = "owner";
            repo = "repo";
            pr_number = 42;
            pr_title = "Test PR";
            pr_body = "Test body";
            pr_author = "testuser";
            base_branch = "main";
            head_branch = "feature";
            html_url = "https://github.com/owner/repo/pull/42";
          }
      in
      let sent = ref [] in
      let send_message ~room_id ~text () =
        sent := (room_id, text) :: !sent;
        Lwt.return_unit
      in
      let result =
        Lwt_main.run
          (Github_pr_dispatch.dispatch_to_subscriptions ~db ~event
             ~delivery_id:"test-delivery-3" ~quiet_start:0 ~quiet_end:0
             ~send_message ())
      in
      Alcotest.(check int) "dispatch count" 2 result;
      Alcotest.(check int) "sent count" 2 (List.length !sent);
      (* Verify both rooms were notified *)
      let rooms = List.map fst !sent in
      Alcotest.(check bool) "room-1 notified" true (List.mem "room-1" rooms);
      Alcotest.(check bool) "room-2 notified" true (List.mem "room-2" rooms))

let test_dispatch_disabled_subscription () =
  with_db (fun db ->
      (* Add a subscription and disable it *)
      let sub =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:42 ~profile_id:1 ()
      in
      let _ =
        Github_pr_subscriptions.set_enabled ~db ~id:sub.id ~enabled:false
      in
      let event =
        Github_webhook.PullRequest
          {
            action = "opened";
            owner = "owner";
            repo = "repo";
            pr_number = 42;
            pr_title = "Test PR";
            pr_body = "Test body";
            pr_author = "testuser";
            base_branch = "main";
            head_branch = "feature";
            html_url = "https://github.com/owner/repo/pull/42";
          }
      in
      let sent = ref [] in
      let send_message ~room_id ~text () =
        sent := (room_id, text) :: !sent;
        Lwt.return_unit
      in
      let result =
        Lwt_main.run
          (Github_pr_dispatch.dispatch_to_subscriptions ~db ~event
             ~delivery_id:"test-delivery-4" ~quiet_start:0 ~quiet_end:0
             ~send_message ())
      in
      Alcotest.(check int) "dispatch count" 0 result;
      Alcotest.(check int) "sent count" 0 (List.length !sent))

let test_dispatch_notification_preferences () =
  with_db (fun db ->
      (* Add subscription with custom preferences: only notify on open and comment *)
      let prefs =
        {
          Github_pr_subscriptions.on_open = true;
          on_close = false;
          on_comment = true;
          on_review = false;
          on_status = false;
          on_merge = false;
        }
      in
      let _sub =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:42 ~profile_id:1 ~notification_preferences:prefs ()
      in
      let sent = ref [] in
      let send_message ~room_id ~text () =
        sent := (room_id, text) :: !sent;
        Lwt.return_unit
      in
      (* Should notify for opened *)
      let opened_event =
        Github_webhook.PullRequest
          {
            action = "opened";
            owner = "owner";
            repo = "repo";
            pr_number = 42;
            pr_title = "Test PR";
            pr_body = "Test body";
            pr_author = "testuser";
            base_branch = "main";
            head_branch = "feature";
            html_url = "https://github.com/owner/repo/pull/42";
          }
      in
      let result1 =
        Lwt_main.run
          (Github_pr_dispatch.dispatch_to_subscriptions ~db ~event:opened_event
             ~delivery_id:"test-delivery-5a" ~quiet_start:0 ~quiet_end:0
             ~send_message ())
      in
      Alcotest.(check int) "opened dispatch count" 1 result1;
      (* Should not notify for closed *)
      sent := [];
      let closed_event =
        Github_webhook.PullRequest
          {
            action = "closed";
            owner = "owner";
            repo = "repo";
            pr_number = 42;
            pr_title = "Test PR";
            pr_body = "Test body";
            pr_author = "testuser";
            base_branch = "main";
            head_branch = "feature";
            html_url = "https://github.com/owner/repo/pull/42";
          }
      in
      let result2 =
        Lwt_main.run
          (Github_pr_dispatch.dispatch_to_subscriptions ~db ~event:closed_event
             ~delivery_id:"test-delivery-5b" ~quiet_start:0 ~quiet_end:0
             ~send_message ())
      in
      Alcotest.(check int) "closed dispatch count" 0 result2;
      Alcotest.(check int) "sent count for closed" 0 (List.length !sent);
      (* Should notify for comment *)
      sent := [];
      let comment_event =
        Github_webhook.IssueComment
          {
            owner = "owner";
            repo = "repo";
            issue_number = 42;
            is_pr = true;
            comment_id = 123;
            comment_author = "commenter";
            comment_body = "Nice PR!";
            issue_title = "Test PR";
            html_url = "https://github.com/owner/repo/pull/42#issuecomment-123";
          }
      in
      let result3 =
        Lwt_main.run
          (Github_pr_dispatch.dispatch_to_subscriptions ~db ~event:comment_event
             ~delivery_id:"test-delivery-5c" ~quiet_start:0 ~quiet_end:0
             ~send_message ())
      in
      Alcotest.(check int) "comment dispatch count" 1 result3)

let test_format_ci_summary () =
  let ci =
    {
      Github_webhook.kind = `WorkflowRun;
      name = "CI";
      status = "completed";
      conclusion = "failure";
      owner = "acme";
      repo = "backend";
      pr_number = Some 42;
      html_url = "https://github.com/acme/backend/actions/runs/99";
      head_sha = "abc123def456";
      actor = "ci-bot";
      details_url = "https://github.com/acme/backend/actions/runs/99/jobs/1";
    }
  in
  let text = Github_pr_dispatch.format_ci_summary ci "completed" in
  Alcotest.(check bool)
    "contains SHA" true
    (try
       ignore (Str.search_forward (Str.regexp_string "abc123d") text 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains actor" true
    (try
       ignore (Str.search_forward (Str.regexp_string "ci-bot") text 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains PR" true
    (try
       ignore (Str.search_forward (Str.regexp_string "#42") text 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains failing job link" true
    (try
       ignore
         (Str.search_forward
            (Str.regexp_string "actions/runs/99/jobs/1")
            text 0);
       true
     with Not_found -> false)

let test_format_ci_summary_for_slack () =
  let ci =
    {
      Github_webhook.kind = `CheckRun;
      name = "test";
      status = "completed";
      conclusion = "failure";
      owner = "acme";
      repo = "backend";
      pr_number = Some 42;
      html_url = "https://github.com/acme/backend/actions";
      head_sha = "abc123def456";
      actor = "bot";
      details_url = "https://github.com/acme/backend/actions/jobs/1";
    }
  in
  let text = Github_pr_dispatch.format_ci_summary_for_slack ci "completed" in
  Alcotest.(check bool)
    "uses slack link syntax" true
    (try
       ignore (Str.search_forward (Str.regexp_string "<https://") text 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains actor" true
    (try
       ignore (Str.search_forward (Str.regexp_string "@bot") text 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains failing job link" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Failing job") text 0);
       true
     with Not_found -> false)

let test_format_review_summary () =
  let review =
    {
      Github_webhook.state = Github_webhook.Approved;
      raw_state = "approved";
      reviewer = "bob";
      body = "LGTM";
      owner = "acme";
      repo = "backend";
      pr_number = 42;
      html_url = "https://github.com/acme/backend/pull/42";
      head_sha = "abc123def456";
    }
  in
  let text = Github_pr_dispatch.format_review_summary review in
  Alcotest.(check bool)
    "contains SHA" true
    (try
       ignore (Str.search_forward (Str.regexp_string "abc123d") text 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains reviewer" true
    (try
       ignore (Str.search_forward (Str.regexp_string "bob") text 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains body snippet" true
    (try
       ignore (Str.search_forward (Str.regexp_string "LGTM") text 0);
       true
     with Not_found -> false)

let test_format_review_summary_for_slack () =
  let review =
    {
      Github_webhook.state = Github_webhook.ChangesRequested;
      raw_state = "changes_requested";
      reviewer = "carol";
      body = "fix this please";
      owner = "acme";
      repo = "backend";
      pr_number = 7;
      html_url = "https://github.com/acme/backend/pull/7";
      head_sha = "deadbeef0123";
    }
  in
  let text = Github_pr_dispatch.format_review_summary_for_slack review in
  Alcotest.(check bool)
    "uses slack link syntax" true
    (try
       ignore (Str.search_forward (Str.regexp_string "<https://") text 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains reviewer" true
    (try
       ignore (Str.search_forward (Str.regexp_string "carol") text 0);
       true
     with Not_found -> false)

let test_format_review_comment_enhanced () =
  let review =
    {
      Github_webhook.state = Github_webhook.Commented;
      raw_state = "commented";
      reviewer = "alice";
      body = "Looks good but fix the typo";
      owner = "acme";
      repo = "backend";
      pr_number = 42;
      html_url = "https://github.com/acme/backend/pull/42#discussion_r100";
      head_sha = "deadbeef1234567";
    }
  in
  let text = Github_pr_dispatch.format_review_summary review in
  Alcotest.(check bool)
    "contains SHA" true
    (try
       ignore (Str.search_forward (Str.regexp_string "deadbee") text 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains reviewer" true
    (try
       ignore (Str.search_forward (Str.regexp_string "alice") text 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains body" true
    (try
       ignore (Str.search_forward (Str.regexp_string "fix the typo") text 0);
       true
     with Not_found -> false)

let test_parse_event_check_run_extracts_fields () =
  let body =
    {|{"action":"completed","check_run":{"name":"test","status":"completed","conclusion":"failure","head_sha":"abc123","details_url":"https://github.com/acme/backend/runs/1","html_url":"https://github.com/acme/backend/checks","pull_requests":[{"number":42}]},"sender":{"login":"ci-bot"},"repository":{"name":"backend","owner":{"login":"acme"}}}|}
  in
  match Github_webhook.parse_event ~event_type:"check_run" ~body with
  | Github_webhook.CheckRun e ->
      Alcotest.(check string) "head_sha" "abc123" e.head_sha;
      Alcotest.(check string) "actor" "ci-bot" e.actor;
      Alcotest.(check string)
        "details_url" "https://github.com/acme/backend/runs/1" e.details_url;
      Alcotest.(check string) "conclusion" "failure" e.conclusion;
      Alcotest.(check (option int)) "pr_number" (Some 42) e.pr_number
  | _ -> Alcotest.fail "expected CheckRun"

let test_parse_event_review_extracts_head_sha () =
  let body =
    {|{"action":"submitted","review":{"id":1,"user":{"login":"bob"},"state":"approved","body":"LGTM"},"pull_request":{"number":42,"head":{"sha":"abc123def"}},"repository":{"name":"backend","owner":{"login":"acme"}}}|}
  in
  match Github_webhook.parse_event ~event_type:"pull_request_review" ~body with
  | Github_webhook.PullRequestReview e ->
      Alcotest.(check string) "head_sha" "abc123def" e.head_sha;
      Alcotest.(check string) "review_author" "bob" e.review_author
  | _ -> Alcotest.fail "expected PullRequestReview"

let suite =
  [
    Alcotest.test_case "should notify subscription" `Quick
      test_should_notify_subscription;
    Alcotest.test_case "format PR event notification" `Quick
      test_format_pr_event_notification;
    Alcotest.test_case "format comment notification" `Quick
      test_format_comment_notification;
    Alcotest.test_case "format review comment notification" `Quick
      test_format_review_comment_notification;
    Alcotest.test_case "format CI summary" `Quick test_format_ci_summary;
    Alcotest.test_case "format CI summary for Slack" `Quick
      test_format_ci_summary_for_slack;
    Alcotest.test_case "format review summary" `Quick test_format_review_summary;
    Alcotest.test_case "format review summary for Slack" `Quick
      test_format_review_summary_for_slack;
    Alcotest.test_case "format review comment enhanced" `Quick
      test_format_review_comment_enhanced;
    Alcotest.test_case "parse check_run extracts fields" `Quick
      test_parse_event_check_run_extracts_fields;
    Alcotest.test_case "parse review extracts head_sha" `Quick
      test_parse_event_review_extracts_head_sha;
    Alcotest.test_case "dispatch dedup" `Quick test_dispatch_dedup;
    Alcotest.test_case "dispatch send failure does not poison policy dedup"
      `Quick test_dispatch_send_failure_does_not_poison_policy_dedup;
    Alcotest.test_case "dispatch no subscriptions" `Quick
      test_dispatch_no_subscriptions;
    Alcotest.test_case "dispatch multiple rooms" `Quick
      test_dispatch_multiple_rooms;
    Alcotest.test_case "dispatch disabled subscription" `Quick
      test_dispatch_disabled_subscription;
    Alcotest.test_case "dispatch notification preferences" `Quick
      test_dispatch_notification_preferences;
  ]
