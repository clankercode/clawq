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
             ~delivery_id:"test-delivery-1" ~send_message ())
      in
      Alcotest.(check int) "first dispatch count" 1 result1;
      Alcotest.(check int) "sent count" 1 (List.length !sent);
      (* Second dispatch with same delivery_id should be deduped *)
      sent := [];
      let result2 =
        Lwt_main.run
          (Github_pr_dispatch.dispatch_to_subscriptions ~db ~event
             ~delivery_id:"test-delivery-1" ~send_message ())
      in
      Alcotest.(check int) "second dispatch count" 0 result2;
      Alcotest.(check int) "sent count after dedup" 0 (List.length !sent))

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
             ~delivery_id:"test-delivery-2" ~send_message ())
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
             ~delivery_id:"test-delivery-3" ~send_message ())
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
             ~delivery_id:"test-delivery-4" ~send_message ())
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
             ~delivery_id:"test-delivery-5a" ~send_message ())
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
             ~delivery_id:"test-delivery-5b" ~send_message ())
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
             ~delivery_id:"test-delivery-5c" ~send_message ())
      in
      Alcotest.(check int) "comment dispatch count" 1 result3)

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
    Alcotest.test_case "dispatch dedup" `Quick test_dispatch_dedup;
    Alcotest.test_case "dispatch no subscriptions" `Quick
      test_dispatch_no_subscriptions;
    Alcotest.test_case "dispatch multiple rooms" `Quick
      test_dispatch_multiple_rooms;
    Alcotest.test_case "dispatch disabled subscription" `Quick
      test_dispatch_disabled_subscription;
    Alcotest.test_case "dispatch notification preferences" `Quick
      test_dispatch_notification_preferences;
  ]
