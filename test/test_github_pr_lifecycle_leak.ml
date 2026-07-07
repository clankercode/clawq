(** Subscription lifecycle and leak tests.

    Verifies that disabled, deleted, unauthorized, moved-room, and ungranted
    subscriptions do not leak updates through the dispatch path. *)

let with_db f = Test_helpers.with_memory_store f

let make_pr_event ~repo ~pr_number =
  let owner, name =
    match String.split_on_char '/' repo with
    | [ o; n ] -> (o, n)
    | _ -> ("owner", "repo")
  in
  Github_webhook.PullRequest
    {
      action = "opened";
      owner;
      repo = name;
      pr_number;
      pr_title = "Test PR";
      pr_body = "body";
      pr_author = "alice";
      base_branch = "main";
      head_branch = "feature";
      html_url = Printf.sprintf "https://github.com/%s/pull/%d" repo pr_number;
    }

let make_comment_event ~repo ~issue_number =
  let owner, name =
    match String.split_on_char '/' repo with
    | [ o; n ] -> (o, n)
    | _ -> ("owner", "repo")
  in
  Github_webhook.IssueComment
    {
      owner;
      repo = name;
      issue_number;
      is_pr = true;
      comment_id = 100;
      comment_author = "bob";
      comment_body = "Nice PR!";
      issue_title = "Test PR";
      html_url =
        Printf.sprintf "https://github.com/%s/pull/%d#issuecomment-100" repo
          issue_number;
    }

let make_review_event ~repo ~pr_number =
  let owner, name =
    match String.split_on_char '/' repo with
    | [ o; n ] -> (o, n)
    | _ -> ("owner", "repo")
  in
  Github_webhook.PrReviewComment
    {
      owner;
      repo = name;
      pr_number;
      comment_id = 200;
      comment_author = "carol";
      comment_body = "Looks good!";
      in_reply_to_id = None;
      diff_hunk = "@@ -1,3 +1,4 @@";
      file_path = "src/main.ml";
      pr_title = "Test PR";
      html_url =
        Printf.sprintf "https://github.com/%s/pull/%d#discussion_r200" repo
          pr_number;
      head_sha = "";
    }

let track_sent () =
  let sent = ref [] in
  let send_message ~room_id ~text () =
    sent := (room_id, text) :: !sent;
    Lwt.return "msg-1"
  in
  (sent, send_message)

let dispatch db event delivery_id send_message =
  Lwt_main.run
    (Github_pr_dispatch.dispatch_to_subscriptions ~db ~event ~delivery_id
       ~quiet_start:0 ~quiet_end:0 ~send_message ())

(* ---- disabled subscription does not leak ---- *)

let test_disabled_no_leak () =
  with_db (fun db ->
      let sub =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:42 ~profile_id:1 ()
      in
      ignore (Github_pr_subscriptions.set_enabled ~db ~id:sub.id ~enabled:false);
      let sent, send_message = track_sent () in
      let event = make_pr_event ~repo:"owner/repo" ~pr_number:42 in
      let count = dispatch db event "lifecycle-disable-1" send_message in
      Alcotest.(check int) "no dispatch" 0 count;
      Alcotest.(check int) "no messages sent" 0 (List.length !sent))

(* ---- deleted subscription does not leak ---- *)

let test_deleted_no_leak () =
  with_db (fun db ->
      let _sub =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:42 ~profile_id:1 ()
      in
      let removed =
        Github_pr_subscriptions.remove ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:42
      in
      Alcotest.(check bool) "subscription removed" true removed;
      let sent, send_message = track_sent () in
      let event = make_pr_event ~repo:"owner/repo" ~pr_number:42 in
      let count = dispatch db event "lifecycle-delete-1" send_message in
      Alcotest.(check int) "no dispatch" 0 count;
      Alcotest.(check int) "no messages sent" 0 (List.length !sent))

(* ---- subscription re-enabled after disable does not leak before re-enable ---- *)

let test_reenabled_lifecycle () =
  with_db (fun db ->
      let sub =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:42 ~profile_id:1 ()
      in
      ignore (Github_pr_subscriptions.set_enabled ~db ~id:sub.id ~enabled:false);
      let sent, send_message = track_sent () in
      let event = make_pr_event ~repo:"owner/repo" ~pr_number:42 in
      (* Disabled: should not dispatch *)
      let count = dispatch db event "lifecycle-reenable-1" send_message in
      Alcotest.(check int) "disabled: no dispatch" 0 count;
      Alcotest.(check int) "disabled: no messages" 0 (List.length !sent);
      (* Re-enable *)
      ignore (Github_pr_subscriptions.set_enabled ~db ~id:sub.id ~enabled:true);
      sent := [];
      let count = dispatch db event "lifecycle-reenable-2" send_message in
      Alcotest.(check int) "re-enabled: dispatched" 1 count;
      Alcotest.(check int) "re-enabled: message sent" 1 (List.length !sent))

(* ---- dispatch failure in send_message does not leak to other rooms ---- *)

let test_send_failure_no_leak () =
  with_db (fun db ->
      let _sub1 =
        Github_pr_subscriptions.add ~db ~room_id:"room-fail" ~repo:"owner/repo"
          ~pr_number:42 ~profile_id:1 ()
      in
      let _sub2 =
        Github_pr_subscriptions.add ~db ~room_id:"room-ok" ~repo:"owner/repo"
          ~pr_number:42 ~profile_id:2 ()
      in
      let sent = ref [] in
      let send_message ~room_id ~text () =
        if room_id = "room-fail" then Lwt.fail (Failure "room unavailable")
        else (
          sent := (room_id, text) :: !sent;
          Lwt.return "msg-1")
      in
      let event = make_pr_event ~repo:"owner/repo" ~pr_number:42 in
      let count = dispatch db event "lifecycle-fail-1" send_message in
      Alcotest.(check int) "only successful room counted" 1 count;
      Alcotest.(check int) "only ok room received message" 1 (List.length !sent);
      match !sent with
      | (room_id, _) :: _ ->
          Alcotest.(check string) "sent to room-ok" "room-ok" room_id
      | [] -> Alcotest.fail "expected one message")

(* ---- room deleted via delete_by_room does not leak ---- *)

let test_room_deleted_no_leak () =
  with_db (fun db ->
      let _sub1 =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:42 ~profile_id:1 ()
      in
      let _sub2 =
        Github_pr_subscriptions.add ~db ~room_id:"room-2" ~repo:"owner/repo"
          ~pr_number:42 ~profile_id:2 ()
      in
      let deleted =
        Github_pr_subscriptions.delete_by_room ~db ~room_id:"room-1"
      in
      Alcotest.(check int) "deleted 1 subscription" 1 deleted;
      let sent, send_message = track_sent () in
      let event = make_pr_event ~repo:"owner/repo" ~pr_number:42 in
      let count = dispatch db event "lifecycle-room-del-1" send_message in
      Alcotest.(check int) "only room-2 dispatched" 1 count;
      Alcotest.(check int) "only room-2 message" 1 (List.length !sent);
      match !sent with
      | (room_id, _) :: _ ->
          Alcotest.(check string) "room-2 notified" "room-2" room_id
      | [] -> Alcotest.fail "expected room-2 message")

(* ---- repo deleted via delete_by_repo does not leak ---- *)

let test_repo_deleted_no_leak () =
  with_db (fun db ->
      let _sub1 =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo1"
          ~pr_number:1 ~profile_id:1 ()
      in
      let _sub2 =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo2"
          ~pr_number:1 ~profile_id:1 ()
      in
      let deleted =
        Github_pr_subscriptions.delete_by_repo ~db ~repo:"owner/repo1"
      in
      Alcotest.(check int) "deleted repo1 subs" 1 deleted;
      let sent, send_message = track_sent () in
      let event1 = make_pr_event ~repo:"owner/repo1" ~pr_number:1 in
      let count1 = dispatch db event1 "lifecycle-repo-del-1" send_message in
      Alcotest.(check int) "repo1: no dispatch" 0 count1;
      Alcotest.(check int) "repo1: no messages" 0 (List.length !sent);
      let event2 = make_pr_event ~repo:"owner/repo2" ~pr_number:1 in
      let count2 = dispatch db event2 "lifecycle-repo-del-2" send_message in
      Alcotest.(check int) "repo2: dispatched" 1 count2;
      Alcotest.(check int) "repo2: message sent" 1 (List.length !sent))

(* ---- notification preferences: closed-only subscription leaks only closed ---- *)

let test_closed_only_preference_leak () =
  with_db (fun db ->
      let prefs =
        {
          Github_pr_subscriptions.on_open = false;
          on_close = true;
          on_comment = false;
          on_review = false;
          on_status = false;
          on_merge = false;
        }
      in
      let _sub =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:42 ~profile_id:1 ~notification_preferences:prefs ()
      in
      let sent, send_message = track_sent () in
      (* opened should not leak *)
      let opened_event = make_pr_event ~repo:"owner/repo" ~pr_number:42 in
      let count1 =
        dispatch db opened_event "lifecycle-pref-opened" send_message
      in
      Alcotest.(check int) "opened: no dispatch" 0 count1;
      Alcotest.(check int) "opened: no messages" 0 (List.length !sent);
      (* comment should not leak *)
      sent := [];
      let comment_event =
        make_comment_event ~repo:"owner/repo" ~issue_number:42
      in
      let count2 =
        dispatch db comment_event "lifecycle-pref-comment" send_message
      in
      Alcotest.(check int) "comment: no dispatch" 0 count2;
      Alcotest.(check int) "comment: no messages" 0 (List.length !sent);
      (* review should not leak *)
      sent := [];
      let review_event = make_review_event ~repo:"owner/repo" ~pr_number:42 in
      let count3 =
        dispatch db review_event "lifecycle-pref-review" send_message
      in
      Alcotest.(check int) "review: no dispatch" 0 count3;
      Alcotest.(check int) "review: no messages" 0 (List.length !sent))

(* ---- multiple lifecycle operations in sequence do not leak ---- *)

let test_cascading_lifecycle_no_leak () =
  with_db (fun db ->
      let sub1 =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:1 ~profile_id:1 ()
      in
      let _sub2 =
        Github_pr_subscriptions.add ~db ~room_id:"room-2" ~repo:"owner/repo"
          ~pr_number:1 ~profile_id:2 ()
      in
      let _sub3 =
        Github_pr_subscriptions.add ~db ~room_id:"room-3" ~repo:"owner/repo"
          ~pr_number:1 ~profile_id:3 ()
      in
      let sent, send_message = track_sent () in
      let event = make_pr_event ~repo:"owner/repo" ~pr_number:1 in
      (* Baseline: all 3 rooms *)
      let count = dispatch db event "lifecycle-cascade-1" send_message in
      Alcotest.(check int) "baseline: 3 dispatched" 3 count;
      (* Disable room-1 *)
      sent := [];
      ignore
        (Github_pr_subscriptions.set_enabled ~db ~id:sub1.id ~enabled:false);
      let count = dispatch db event "lifecycle-cascade-2" send_message in
      Alcotest.(check int) "disable room-1: 2 dispatched" 2 count;
      let rooms = List.map fst !sent in
      Alcotest.(check bool) "room-1 not sent" false (List.mem "room-1" rooms);
      (* Delete room-2 *)
      sent := [];
      let deleted =
        Github_pr_subscriptions.delete_by_room ~db ~room_id:"room-2"
      in
      Alcotest.(check int) "deleted room-2" 1 deleted;
      let count = dispatch db event "lifecycle-cascade-3" send_message in
      Alcotest.(check int) "delete room-2: 1 dispatched" 1 count;
      match !sent with
      | [ (room_id, _) ] ->
          Alcotest.(check string) "only room-3" "room-3" room_id
      | _ -> Alcotest.fail "expected exactly room-3")

(* ---- disabled subscription does not leak across event types ---- *)

let test_disabled_no_leak_across_events () =
  with_db (fun db ->
      let sub =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:42 ~profile_id:1 ()
      in
      ignore (Github_pr_subscriptions.set_enabled ~db ~id:sub.id ~enabled:false);
      let sent, send_message = track_sent () in
      (* PR event *)
      let pr_event = make_pr_event ~repo:"owner/repo" ~pr_number:42 in
      let count1 = dispatch db pr_event "lifecycle-multi-1" send_message in
      Alcotest.(check int) "PR: no dispatch" 0 count1;
      (* Comment event *)
      let comment_event =
        make_comment_event ~repo:"owner/repo" ~issue_number:42
      in
      let count2 = dispatch db comment_event "lifecycle-multi-2" send_message in
      Alcotest.(check int) "comment: no dispatch" 0 count2;
      Alcotest.(check int) "total: no messages" 0 (List.length !sent))

(* ---- subscription count stays consistent through lifecycle ---- *)

let test_count_consistent_through_lifecycle () =
  with_db (fun db ->
      let count_initial = Github_pr_subscriptions.count ~db () in
      Alcotest.(check int) "initial count" 0 count_initial;
      let sub =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:42 ~profile_id:1 ()
      in
      Alcotest.(check int) "after add" 1 (Github_pr_subscriptions.count ~db ());
      ignore (Github_pr_subscriptions.set_enabled ~db ~id:sub.id ~enabled:false);
      Alcotest.(check int)
        "after disable (count unchanged)" 1
        (Github_pr_subscriptions.count ~db ());
      ignore (Github_pr_subscriptions.set_enabled ~db ~id:sub.id ~enabled:true);
      Alcotest.(check int)
        "after re-enable (count unchanged)" 1
        (Github_pr_subscriptions.count ~db ());
      let removed =
        Github_pr_subscriptions.remove ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:42
      in
      Alcotest.(check bool) "removed" true removed;
      Alcotest.(check int)
        "after remove" 0
        (Github_pr_subscriptions.count ~db ()))

(* ---- find_by_room does not leak deleted subscriptions ---- *)

let test_find_by_room_no_leak () =
  with_db (fun db ->
      let _sub1 =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:1 ~profile_id:1 ()
      in
      let _sub2 =
        Github_pr_subscriptions.add ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:2 ~profile_id:1 ()
      in
      let subs_before =
        Github_pr_subscriptions.find_by_room ~db ~room_id:"room-1"
      in
      Alcotest.(check int) "before remove: 2" 2 (List.length subs_before);
      let removed =
        Github_pr_subscriptions.remove ~db ~room_id:"room-1" ~repo:"owner/repo"
          ~pr_number:1
      in
      Alcotest.(check bool) "removed" true removed;
      let subs_after =
        Github_pr_subscriptions.find_by_room ~db ~room_id:"room-1"
      in
      Alcotest.(check int) "after remove: 1" 1 (List.length subs_after);
      let remaining = List.hd subs_after in
      Alcotest.(check int) "remaining is pr 2" 2 remaining.pr_number)

let suite =
  [
    Alcotest.test_case "disabled subscription does not leak" `Quick
      test_disabled_no_leak;
    Alcotest.test_case "deleted subscription does not leak" `Quick
      test_deleted_no_leak;
    Alcotest.test_case "re-enabled subscription lifecycle" `Quick
      test_reenabled_lifecycle;
    Alcotest.test_case "send failure does not leak to other rooms" `Quick
      test_send_failure_no_leak;
    Alcotest.test_case "room deleted does not leak" `Quick
      test_room_deleted_no_leak;
    Alcotest.test_case "repo deleted does not leak" `Quick
      test_repo_deleted_no_leak;
    Alcotest.test_case "closed-only preference does not leak other events"
      `Quick test_closed_only_preference_leak;
    Alcotest.test_case "cascading lifecycle operations do not leak" `Quick
      test_cascading_lifecycle_no_leak;
    Alcotest.test_case "disabled does not leak across event types" `Quick
      test_disabled_no_leak_across_events;
    Alcotest.test_case "count consistent through lifecycle" `Quick
      test_count_consistent_through_lifecycle;
    Alcotest.test_case "find_by_room does not leak deleted" `Quick
      test_find_by_room_no_leak;
  ]
