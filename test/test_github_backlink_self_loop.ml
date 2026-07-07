(** Backlink self-loop and audit traceability tests.

    Verifies:
    - Bot replies retain the clawq-reply marker and are detected as self-loops
    - Provenance footer is correctly embedded between response and marker
    - Self-loop comments do not retrigger the webhook handler
    - Ledger events correctly trace room scope to GitHub actions
    - Audit export includes backlink provenance for GitHub→room and room→GitHub
*)

let with_db f = Test_helpers.with_memory_store f

(* ---- helpers ---- *)

let contains_substring s sub =
  let s_len = String.length s in
  let sub_len = String.length sub in
  let rec search i =
    if i + sub_len > s_len then false
    else if String.sub s i sub_len = sub then true
    else search (i + 1)
  in
  sub_len = 0 || search 0

let metadata_string key json =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt key fields with Some (`String s) -> s | _ -> "")
  | _ -> ""

let metadata_int key json =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt key fields with Some (`Int n) -> n | _ -> -1)
  | _ -> -1

(* ---- is_bot_reply: marker detection ---- *)

let test_bot_reply_marker_detected () =
  let text = "Some response\n<!-- clawq-reply -->" in
  Alcotest.(check bool) "detected" true (Github.is_bot_reply text)

let test_bot_reply_marker_not_in_normal_comment () =
  let text = "Normal user comment about the PR" in
  Alcotest.(check bool) "not detected" false (Github.is_bot_reply text)

let test_bot_reply_marker_at_start () =
  let text = "<!-- clawq-reply -->" in
  Alcotest.(check bool) "detected at start" true (Github.is_bot_reply text)

let test_bot_reply_marker_embedded_in_long_text () =
  let text =
    "Here is my analysis of the PR.\n\n\
     The changes look good.\n\
     <!-- clawq-reply -->\n\
     Some trailing text"
  in
  Alcotest.(check bool) "detected in middle" true (Github.is_bot_reply text)

let test_bot_reply_partial_marker_not_detected () =
  (* Only the exact marker should match *)
  let text = "Some text <!-- clawq-repl -->" in
  Alcotest.(check bool) "partial not detected" false (Github.is_bot_reply text)

(* ---- is_provenance_comment: provenance detection ---- *)

let test_provenance_comment_detected () =
  let text =
    "Response\n\
     ---\n\
     <sub>Slack | #general by @alice</sub>\n\
     <!-- clawq-provenance: {\"connector\":\"slack\"} -->\n\
     <!-- clawq-reply -->"
  in
  Alcotest.(check bool)
    "provenance detected" true
    (Github.is_provenance_comment text)

let test_provenance_comment_not_in_plain_text () =
  let text = "Normal comment" in
  Alcotest.(check bool) "not detected" false (Github.is_provenance_comment text)

(* ---- format_reply: marker preservation ---- *)

let test_format_reply_preserves_marker () =
  let reply = Github.format_reply ~command:"review" ~response:"Looks good!" in
  Alcotest.(check bool)
    "has marker" true
    (contains_substring reply "<!-- clawq-reply -->")

let test_format_reply_with_empty_command () =
  let reply = Github.format_reply ~command:"" ~response:"Hello" in
  Alcotest.(check bool)
    "has marker" true
    (contains_substring reply "<!-- clawq-reply -->");
  Alcotest.(check bool)
    "response present" true
    (contains_substring reply "Hello")

let test_format_reply_with_command () =
  let reply =
    Github.format_reply ~command:"status" ~response:"All checks pass"
  in
  Alcotest.(check bool)
    "has marker" true
    (contains_substring reply "<!-- clawq-reply -->");
  Alcotest.(check bool)
    "has quoted command" true
    (contains_substring reply "> /clawq status");
  Alcotest.(check bool)
    "has response" true
    (contains_substring reply "All checks pass")

(* ---- format_reply_with_provenance: footer ordering ---- *)

let test_provenance_between_response_and_marker () =
  let prov =
    {
      Github.connector = Some "slack";
      room_id = Some "#general";
      room_name = None;
      requester_id = Some "alice";
      thread_id = None;
      task_id = None;
    }
  in
  let reply =
    Github.format_reply_with_provenance ~command:"run tests"
      ~response:"Tests passed" ~provenance:(Some prov)
  in
  Alcotest.(check bool)
    "has response" true
    (contains_substring reply "Tests passed");
  Alcotest.(check bool)
    "has provenance" true
    (contains_substring reply "<!-- clawq-provenance:");
  Alcotest.(check bool)
    "has marker" true
    (contains_substring reply "<!-- clawq-reply -->");
  (* Verify ordering: provenance before marker *)
  let provenance_pos =
    let target = "<!-- clawq-provenance:" in
    let tlen = String.length target in
    let slen = String.length reply in
    let rec search i =
      if i + tlen > slen then -1
      else if String.sub reply i tlen = target then i
      else search (i + 1)
    in
    search 0
  in
  let marker_pos =
    let target = "<!-- clawq-reply -->" in
    let tlen = String.length target in
    let slen = String.length reply in
    let rec search i =
      if i + tlen > slen then -1
      else if String.sub reply i tlen = target then i
      else search (i + 1)
    in
    search 0
  in
  Alcotest.(check bool)
    "provenance before marker" true
    (provenance_pos < marker_pos && provenance_pos >= 0)

let test_provenance_none_omits_footer () =
  let reply =
    Github.format_reply_with_provenance ~command:"test" ~response:"Done"
      ~provenance:None
  in
  Alcotest.(check bool)
    "no provenance" false
    (contains_substring reply "<!-- clawq-provenance:");
  Alcotest.(check bool)
    "has marker" true
    (contains_substring reply "<!-- clawq-reply -->")

let test_provenance_empty_provenance_omits_footer () =
  let reply =
    Github.format_reply_with_provenance ~command:"test" ~response:"Done"
      ~provenance:(Some Github.empty_provenance)
  in
  Alcotest.(check bool)
    "no provenance footer" false
    (contains_substring reply "<!-- clawq-provenance:");
  Alcotest.(check bool)
    "has marker" true
    (contains_substring reply "<!-- clawq-reply -->")

let test_provenance_with_task_id () =
  let prov =
    {
      Github.connector = Some "teams";
      room_id = Some "room-42";
      room_name = Some "Engineering";
      requester_id = Some "bob";
      thread_id = Some "t-123";
      task_id = Some 7;
    }
  in
  let reply =
    Github.format_reply_with_provenance ~command:"" ~response:"Completed"
      ~provenance:(Some prov)
  in
  Alcotest.(check bool)
    "has task_id in provenance" true
    (contains_substring reply "Task #7");
  Alcotest.(check bool) "has requester" true (contains_substring reply "@bob");
  Alcotest.(check bool)
    "provenance has structured data" true
    (contains_substring reply "\"task_id\":7")

(* ---- self-loop: bot reply text does not retrigger ---- *)

let test_bot_reply_text_as_comment_body_would_not_retrigger () =
  (* Simulate: if a webhook delivers a comment that is a bot reply,
     the handler should detect it and not retrigger *)
  let bot_text =
    Github.format_reply ~command:"review" ~response:"Here is my review"
  in
  Alcotest.(check bool)
    "bot reply detected as self-loop" true
    (Github.is_bot_reply bot_text);
  (* User comments should not be detected *)
  let user_text = "/clawq review this PR please" in
  Alcotest.(check bool)
    "user command not detected as bot reply" false
    (Github.is_bot_reply user_text)

let test_bot_reply_with_provenance_detected_as_self_loop () =
  let prov =
    {
      Github.connector = Some "discord";
      room_id = Some "channel-123";
      room_name = None;
      requester_id = Some "carol";
      thread_id = None;
      task_id = Some 42;
    }
  in
  let reply =
    Github.format_reply_with_provenance ~command:"build" ~response:"Build OK"
      ~provenance:(Some prov)
  in
  Alcotest.(check bool)
    "reply with provenance detected" true
    (Github.is_bot_reply reply);
  Alcotest.(check bool)
    "provenance detected" true
    (Github.is_provenance_comment reply)

(* ---- webhook handler: bot reply comment is skipped ---- *)

let test_webhook_handler_skips_bot_reply_comment () =
  (* Verify the exact code path used in handle_webhook:
     comment_body_of_event extracts the body, then is_bot_reply detects
     the marker. For full handler integration, see test_github.ml
     "bot self-loop protection" test. *)
  let bot_text = Github.format_reply ~command:"review" ~response:"LGTM" in
  let user_text = "/clawq review this PR" in
  (* IssueComment event with bot reply body *)
  let bot_comment_event =
    Github_webhook.IssueComment
      {
        owner = "org";
        repo = "repo";
        issue_number = 42;
        is_pr = true;
        comment_id = 999;
        comment_author = "bot";
        comment_body = bot_text;
        issue_title = "Test PR";
        html_url = "https://github.com/org/repo/pull/42#issuecomment-999";
      }
  in
  let user_comment_event =
    Github_webhook.IssueComment
      {
        owner = "org";
        repo = "repo";
        issue_number = 42;
        is_pr = true;
        comment_id = 1000;
        comment_author = "alice";
        comment_body = user_text;
        issue_title = "Test PR";
        html_url = "https://github.com/org/repo/pull/42#issuecomment-1000";
      }
  in
  (* Extract comment body using the same function the handler uses *)
  let bot_body = Github.comment_body_of_event bot_comment_event in
  let user_body = Github.comment_body_of_event user_comment_event in
  Alcotest.(check bool)
    "bot comment body is Some" true (Option.is_some bot_body);
  Alcotest.(check bool)
    "user comment body is Some" true (Option.is_some user_body);
  Alcotest.(check bool)
    "bot body detected as self-loop" true
    (Github.is_bot_reply (Option.get bot_body));
  Alcotest.(check bool)
    "user body NOT detected as self-loop" false
    (Github.is_bot_reply (Option.get user_body));
  (* PrReviewComment event with bot reply body *)
  let bot_review_event =
    Github_webhook.PrReviewComment
      {
        owner = "org";
        repo = "repo";
        pr_number = 42;
        comment_id = 888;
        comment_author = "bot";
        comment_body = bot_text;
        in_reply_to_id = None;
        diff_hunk = "@@ -1,3 +1,4 @@";
        file_path = "test.ml";
        pr_title = "Test PR";
        html_url = "https://github.com/org/repo/pull/42#discussion_r888";
        head_sha = "";
      }
  in
  let bot_review_body = Github.comment_body_of_event bot_review_event in
  Alcotest.(check bool)
    "bot review body detected" true
    (Option.is_some bot_review_body
    && Github.is_bot_reply (Option.get bot_review_body));
  (* Verify the marker appears exactly once *)
  let marker = "<!-- clawq-reply -->" in
  let marker_count = ref 0 in
  let s = bot_text in
  let slen = String.length s in
  let mlen = String.length marker in
  let rec count i =
    if i + mlen > slen then ()
    else if String.sub s i mlen = marker then (
      incr marker_count;
      count (i + mlen))
    else count (i + 1)
  in
  count 0;
  Alcotest.(check int) "exactly one marker" 1 !marker_count

(* ---- backlink audit: room scope → GitHub action ---- *)

let test_backlink_traces_room_scope_to_github_action () =
  with_db (fun db ->
      (* Record a subscription delivery from room-1 to GitHub PR *)
      Room_github_backlinks.record_subscription_delivery ~db ~repo:"org/repo"
        ~pr_number:10 ~room_id:"room-1" ~event_type:"pull_request"
        ~github_url:"https://github.com/org/repo/pull/10" ();
      (* Record a triggered run from room-1 to GitHub *)
      Room_github_backlinks.record_triggered_run ~db ~repo:"org/repo"
        ~pr_number:10 ~github_item_type:Room_github_backlinks.Pr_comment
        ~room_id:"room-1" ~room_item_type:Room_github_backlinks.Review_run
        ~room_item_id:"run-1"
        ~github_url:"https://github.com/org/repo/pull/10#issuecomment-99" ();
      (* Verify room-1 traces to both GitHub actions *)
      let room_links =
        Room_github_backlinks.find_by_room ~db ~room_id:"room-1" ()
      in
      Alcotest.(check int) "room-1 has 2 backlinks" 2 (List.length room_links);
      let directions =
        List.map
          (fun bl ->
            Room_github_backlinks.direction_to_string
              bl.Room_github_backlinks.direction)
          room_links
        |> List.sort String.compare
      in
      Alcotest.(check (list string))
        "directions"
        [ "github_to_room"; "room_to_github" ]
        directions;
      let relationships =
        List.map
          (fun bl ->
            Room_github_backlinks.relationship_to_string
              bl.Room_github_backlinks.relationship)
          room_links
        |> List.sort String.compare
      in
      Alcotest.(check (list string))
        "relationships"
        [ "subscription_delivery"; "triggered_run" ]
        relationships)

let test_backlink_traces_github_event_to_room_message () =
  with_db (fun db ->
      (* Record that a GitHub event delivered to room-1 *)
      Room_github_backlinks.record_subscription_delivery ~db ~repo:"org/repo"
        ~pr_number:5 ~room_id:"room-1" ~event_type:"pull_request"
        ~github_url:"https://github.com/org/repo/pull/5" ();
      (* Find backlinks from GitHub side *)
      let github_links =
        Room_github_backlinks.find_by_github ~db ~repo:"org/repo"
          ~github_item_type:Room_github_backlinks.Pr_comment ()
      in
      Alcotest.(check int) "found via github" 1 (List.length github_links);
      let bl = List.hd github_links in
      Alcotest.(check string) "room_id" "room-1" bl.room_id;
      Alcotest.(check string)
        "direction" "github_to_room"
        (Room_github_backlinks.direction_to_string bl.direction);
      Alcotest.(check string)
        "relationship" "subscription_delivery"
        (Room_github_backlinks.relationship_to_string bl.relationship))

let test_backlink_ci_notification_traces_to_room () =
  with_db (fun db ->
      Room_github_backlinks.record_ci_notification ~db ~repo:"org/repo"
        ~pr_number:20 ~github_item_type:Room_github_backlinks.Check_run
        ~github_url:"https://github.com/org/repo/runs/42" ~room_id:"room-ci"
        ~room_item_id:"msg-42" ();
      let github_links =
        Room_github_backlinks.find_by_github ~db ~repo:"org/repo"
          ~github_item_type:Room_github_backlinks.Check_run ()
      in
      Alcotest.(check int) "found CI notification" 1 (List.length github_links);
      let bl = List.hd github_links in
      Alcotest.(check string) "room_id" "room-ci" bl.room_id;
      Alcotest.(check string)
        "relationship" "ci_notification"
        (Room_github_backlinks.relationship_to_string bl.relationship);
      Alcotest.(check (option string))
        "room_item_id preserved" (Some "msg-42") bl.room_item_id)

let test_backlink_by_repo_pr_traces_all_rooms () =
  with_db (fun db ->
      (* PR #30 delivers to 3 different rooms *)
      Room_github_backlinks.record_subscription_delivery ~db ~repo:"org/repo"
        ~pr_number:30 ~room_id:"room-a" ~event_type:"pull_request" ();
      Room_github_backlinks.record_subscription_delivery ~db ~repo:"org/repo"
        ~pr_number:30 ~room_id:"room-b" ~event_type:"issue_comment" ();
      Room_github_backlinks.record_ci_notification ~db ~repo:"org/repo"
        ~pr_number:30 ~github_item_type:Room_github_backlinks.Workflow_run
        ~room_id:"room-c" ();
      let pr_links =
        Room_github_backlinks.find_by_repo_pr ~db ~repo:"org/repo" ~pr_number:30
          ()
      in
      Alcotest.(check int) "3 backlinks for PR #30" 3 (List.length pr_links);
      let rooms =
        List.map (fun bl -> bl.Room_github_backlinks.room_id) pr_links
        |> List.sort String.compare
      in
      Alcotest.(check (list string))
        "rooms"
        [ "room-a"; "room-b"; "room-c" ]
        rooms)

(* ---- audit export: GitHub events categorized correctly ---- *)

let test_audit_export_categorizes_github_backlink_events () =
  with_db (fun db ->
      let cfg = Runtime_config.default in
      let room_id = "room-gh-audit" in
      (* Record ledger events that mirror what dispatch creates *)
      ignore
        (Room_activity_ledger.record_github_update_delivered ~db ~room_id
           ~delivery_id:"audit-del-1" ~repo:"org/repo" ~pr_number:42
           ~event_type:"pull_request" ~payload_summary:"PR #42: test" ());
      ignore
        (Room_activity_ledger.record_github_update_denied ~db ~room_id
           ~delivery_id:"audit-del-2" ~repo:"org/repo" ~pr_number:42
           ~event_type:"pull_request" ~deny_reason:"duplicate"
           ~payload_summary:"PR #42: test" ());
      ignore
        (Room_activity_ledger.record_github_update_skipped ~db ~room_id
           ~delivery_id:"audit-del-3" ~repo:"org/repo" ~pr_number:43
           ~event_type:"pull_request" ~reason:"no_subscriptions"
           ~payload_summary:"PR #43: skipped" ());
      (* Record backlinks *)
      Room_github_backlinks.record_subscription_delivery ~db ~repo:"org/repo"
        ~pr_number:42 ~room_id ~event_type:"pull_request"
        ~github_url:"https://github.com/org/repo/pull/42" ();
      let exp = Room_audit_export.generate ~cfg ~db ~room_id () in
      (* All 3 ledger events should appear in export *)
      Alcotest.(check int) "3 events" 3 exp.total_count;
      let gh_count =
        Option.value ~default:0 (List.assoc_opt "github" exp.category_counts)
      in
      Alcotest.(check int) "all github category" 3 gh_count;
      (* Verify backlink exists for this room *)
      let backlinks = Room_github_backlinks.find_by_room ~db ~room_id () in
      Alcotest.(check int) "backlink recorded" 1 (List.length backlinks))

let test_audit_export_categorizes_delivery_events () =
  with_db (fun db ->
      let cfg = Runtime_config.default in
      let room_id = "room-dlv-audit" in
      let _evt1 =
        Room_activity_ledger.record_delivery_attempt ~db ~room_id
          ~connector:"slack" ~task_id:10 ()
      in
      let _evt2 =
        Room_activity_ledger.record_delivery_success ~db ~room_id
          ~connector:"slack" ~task_id:10 ~message_id:"msg-abc"
          ~thread_id:"thread-1" ()
      in
      let exp = Room_audit_export.generate ~cfg ~db ~room_id () in
      Alcotest.(check int) "2 events" 2 exp.total_count;
      let dlv_count =
        Option.value ~default:0 (List.assoc_opt "delivery" exp.category_counts)
      in
      Alcotest.(check int) "all delivery category" 2 dlv_count)

let test_audit_export_github_and_delivery_mixed () =
  with_db (fun db ->
      let cfg = Runtime_config.default in
      let room_id = "room-mixed-audit" in
      (* GitHub event *)
      ignore
        (Room_activity_ledger.record_github_update_delivered ~db ~room_id
           ~delivery_id:"mix-1" ~repo:"org/repo" ~pr_number:1
           ~event_type:"pull_request" ~payload_summary:"opened" ());
      (* Delivery event *)
      let _evt =
        Room_activity_ledger.record_delivery_success ~db ~room_id
          ~connector:"discord" ~task_id:1 ~message_id:"dmsg-1" ()
      in
      (* Memory event *)
      ignore
        (Room_activity_ledger.append ~db ~room_id ~event_type:"memory_saved"
           ~timestamp:(Room_activity_ledger.timestamp_now ())
           ~actor:"user" ~metadata:(`Assoc []));
      let exp = Room_audit_export.generate ~cfg ~db ~room_id () in
      Alcotest.(check int) "3 events" 3 exp.total_count;
      Alcotest.(check bool)
        "has github" true
        (List.mem_assoc "github" exp.category_counts);
      Alcotest.(check bool)
        "has delivery" true
        (List.mem_assoc "delivery" exp.category_counts);
      Alcotest.(check bool)
        "has memory" true
        (List.mem_assoc "memory" exp.category_counts))

let test_audit_export_json_includes_backlink_metadata () =
  with_db (fun db ->
      let cfg = Runtime_config.default in
      let room_id = "room-json-audit" in
      ignore
        (Room_activity_ledger.record_github_update_delivered ~db ~room_id
           ~delivery_id:"json-del-1" ~repo:"org/repo" ~pr_number:7
           ~event_type:"check_run" ~payload_summary:"CI passed"
           ~connector:"telegram" ());
      (* Record a backlink so provenance is traceable *)
      Room_github_backlinks.record_subscription_delivery ~db ~repo:"org/repo"
        ~pr_number:7 ~room_id ~event_type:"check_run"
        ~github_url:"https://github.com/org/repo/runs/42" ();
      let exp = Room_audit_export.generate ~cfg ~db ~room_id () in
      let json_str = Room_audit_export.export_to_json_string exp in
      let json = Yojson.Safe.from_string json_str in
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "room_id" room_id
        (json |> member "room_id" |> to_string);
      let events = json |> member "events" |> to_list in
      Alcotest.(check int) "1 event" 1 (List.length events);
      let evt = List.hd events in
      Alcotest.(check string)
        "category is github" "github"
        (evt |> member "category" |> to_string);
      (* Metadata should be redacted but present *)
      Alcotest.(check bool)
        "metadata_redacted present" true
        (evt |> member "metadata_redacted" <> `Null);
      (* Verify exported metadata contains traceable GitHub fields *)
      let redacted_meta = evt |> member "metadata_redacted" in
      Alcotest.(check string)
        "exported repo" "org/repo"
        (redacted_meta |> member "repo" |> Yojson.Safe.Util.to_string);
      Alcotest.(check int)
        "exported pr_number" 7
        (redacted_meta |> member "pr_number" |> Yojson.Safe.Util.to_int);
      Alcotest.(check string)
        "exported connector" "telegram"
        (redacted_meta |> member "connector" |> Yojson.Safe.Util.to_string);
      Alcotest.(check string)
        "exported event_type" "check_run"
        (redacted_meta |> member "event_type" |> Yojson.Safe.Util.to_string);
      (* Verify the backlink exists and can trace back to the room *)
      let backlinks = Room_github_backlinks.find_by_room ~db ~room_id () in
      Alcotest.(check int) "backlink for room" 1 (List.length backlinks);
      let bl = List.hd backlinks in
      Alcotest.(check string) "backlink repo" "org/repo" bl.repo;
      Alcotest.(check (option int)) "backlink pr_number" (Some 7) bl.pr_number;
      Alcotest.(check string)
        "backlink direction" "github_to_room"
        (Room_github_backlinks.direction_to_string bl.direction);
      Alcotest.(check string)
        "backlink relationship" "subscription_delivery"
        (Room_github_backlinks.relationship_to_string bl.relationship);
      (* Verify backlink is findable from GitHub side *)
      let gh_links =
        Room_github_backlinks.find_by_github ~db ~repo:"org/repo"
          ~github_item_type:Room_github_backlinks.Check_run ()
      in
      Alcotest.(check int) "findable from github" 1 (List.length gh_links);
      Alcotest.(check string)
        "gh link room_id" room_id (List.hd gh_links).room_id)

let test_audit_export_tracks_delivery_connector () =
  with_db (fun db ->
      let cfg = Runtime_config.default in
      let room_id = "room-connector-audit" in
      let _evt =
        Room_activity_ledger.record_delivery_success ~db ~room_id
          ~connector:"teams" ~task_id:42 ~message_id:"tid-123"
          ~thread_id:"t-456" ()
      in
      let exp = Room_audit_export.generate ~cfg ~db ~room_id () in
      Alcotest.(check int) "1 event" 1 exp.total_count;
      let evt = List.hd exp.events in
      Alcotest.(check string) "actor is connector" "teams" evt.event.actor;
      Alcotest.(check int)
        "task_id in metadata" 42
        (metadata_int "task_id" evt.event.metadata);
      Alcotest.(check string)
        "message_id in metadata" "tid-123"
        (metadata_string "message_id" evt.event.metadata))

(* ---- room isolation: events from other rooms not included ---- *)

let test_audit_export_room_isolation () =
  with_db (fun db ->
      let cfg = Runtime_config.default in
      ignore
        (Room_activity_ledger.record_github_update_delivered ~db
           ~room_id:"room-alpha" ~delivery_id:"iso-1" ~repo:"org/repo"
           ~pr_number:1 ~event_type:"pull_request" ~payload_summary:"opened" ());
      ignore
        (Room_activity_ledger.record_github_update_delivered ~db
           ~room_id:"room-beta" ~delivery_id:"iso-2" ~repo:"org/repo"
           ~pr_number:2 ~event_type:"pull_request" ~payload_summary:"closed" ());
      Room_github_backlinks.record_subscription_delivery ~db ~repo:"org/repo"
        ~pr_number:1 ~room_id:"room-alpha" ~event_type:"pull_request" ();
      Room_github_backlinks.record_subscription_delivery ~db ~repo:"org/repo"
        ~pr_number:2 ~room_id:"room-beta" ~event_type:"pull_request" ();
      let exp_alpha =
        Room_audit_export.generate ~cfg ~db ~room_id:"room-alpha" ()
      in
      Alcotest.(check int) "alpha only sees its events" 1 exp_alpha.total_count;
      let exp_beta =
        Room_audit_export.generate ~cfg ~db ~room_id:"room-beta" ()
      in
      Alcotest.(check int) "beta only sees its events" 1 exp_beta.total_count;
      let alpha_links =
        Room_github_backlinks.find_by_room ~db ~room_id:"room-alpha" ()
      in
      Alcotest.(check int)
        "alpha backlinks isolated" 1 (List.length alpha_links);
      let beta_links =
        Room_github_backlinks.find_by_room ~db ~room_id:"room-beta" ()
      in
      Alcotest.(check int) "beta backlinks isolated" 1 (List.length beta_links))

(* ---- dispatch integration: delivered events include backlink ---- *)

let test_dispatch_creates_backlink_and_ledger_event () =
  with_db (fun db ->
      let _sub =
        Github_pr_subscriptions.add ~db ~room_id:"room-dispatch"
          ~repo:"org/repo" ~pr_number:50 ~profile_id:1 ()
      in
      let event =
        Github_webhook.PullRequest
          {
            action = "opened";
            owner = "org";
            repo = "repo";
            pr_number = 50;
            pr_title = "New feature";
            pr_body = "Body";
            pr_author = "dev";
            base_branch = "main";
            head_branch = "feat";
            html_url = "https://github.com/org/repo/pull/50";
          }
      in
      let send_message ~room_id ~text () =
        ignore (room_id, text);
        Lwt.return "msg-dispatch-1"
      in
      let result =
        Lwt_main.run
          (Github_pr_dispatch.dispatch_to_subscriptions ~db ~event
             ~delivery_id:"dispatch-bl-1" ~connector:"slack" ~quiet_start:0
             ~quiet_end:0 ~send_message ())
      in
      Alcotest.(check int) "dispatched" 1 result;
      (* Verify ledger event *)
      let events =
        Room_activity_ledger.query ~db ~room_id:"room-dispatch"
          ~event_type:"github_update_delivered" ()
      in
      Alcotest.(check int) "ledger event" 1 (List.length events);
      let evt = List.hd events in
      Alcotest.(check string)
        "delivery_id" "dispatch-bl-1"
        (metadata_string "delivery_id" evt.metadata);
      Alcotest.(check string)
        "repo" "org/repo"
        (metadata_string "repo" evt.metadata);
      Alcotest.(check string)
        "connector" "slack"
        (metadata_string "connector" evt.metadata);
      (* Verify backlink with strong assertions on trace fields *)
      let backlinks =
        Room_github_backlinks.find_by_room ~db ~room_id:"room-dispatch" ()
      in
      Alcotest.(check int) "backlink count" 1 (List.length backlinks);
      let bl = List.hd backlinks in
      Alcotest.(check string) "backlink repo" "org/repo" bl.repo;
      Alcotest.(check (option int)) "backlink pr_number" (Some 50) bl.pr_number;
      Alcotest.(check string)
        "backlink direction" "github_to_room"
        (Room_github_backlinks.direction_to_string bl.direction);
      Alcotest.(check string)
        "backlink relationship" "subscription_delivery"
        (Room_github_backlinks.relationship_to_string bl.relationship);
      Alcotest.(check (option string))
        "backlink github_url" (Some "https://github.com/org/repo/pull/50")
        bl.github_url;
      Alcotest.(check string) "backlink room_id" "room-dispatch" bl.room_id)

let test_dispatch_notification_contains_footer_links () =
  with_db (fun db ->
      let _sub =
        Github_pr_subscriptions.add ~db ~room_id:"room-footer" ~repo:"org/repo"
          ~pr_number:60 ~profile_id:1 ()
      in
      let event =
        Github_webhook.CheckRun
          {
            owner = "org";
            repo = "repo";
            name = "build";
            status = "completed";
            conclusion = "success";
            pr_number = Some 60;
            html_url = "https://github.com/org/repo/runs/999";
            head_sha = "abc1234";
            actor = "ci-bot";
            details_url = "";
          }
      in
      let captured_text = ref "" in
      let send_message ~room_id ~text () =
        ignore room_id;
        captured_text := text;
        Lwt.return "msg-footer-1"
      in
      let _result =
        Lwt_main.run
          (Github_pr_dispatch.dispatch_to_subscriptions ~db ~event
             ~delivery_id:"footer-del-1" ~quiet_start:0 ~quiet_end:0
             ~send_message ())
      in
      Alcotest.(check bool)
        "notification has PR link" true
        (contains_substring !captured_text "https://github.com/org/repo/pull/60");
      Alcotest.(check bool)
        "notification has check link" true
        (contains_substring !captured_text
           "https://github.com/org/repo/runs/999"))

(* ---- multiple rooms: each gets its own backlink and ledger entry ---- *)

let test_multiple_rooms_get_independent_backlinks () =
  with_db (fun db ->
      let _sub1 =
        Github_pr_subscriptions.add ~db ~room_id:"room-x" ~repo:"org/repo"
          ~pr_number:70 ~profile_id:1 ()
      in
      let _sub2 =
        Github_pr_subscriptions.add ~db ~room_id:"room-y" ~repo:"org/repo"
          ~pr_number:70 ~profile_id:2 ()
      in
      let event =
        Github_webhook.PullRequest
          {
            action = "opened";
            owner = "org";
            repo = "repo";
            pr_number = 70;
            pr_title = "Multi-room test";
            pr_body = "Body";
            pr_author = "dev";
            base_branch = "main";
            head_branch = "feat";
            html_url = "https://github.com/org/repo/pull/70";
          }
      in
      let send_message ~room_id ~text () =
        ignore (room_id, text);
        Lwt.return (Printf.sprintf "msg-%s" room_id)
      in
      let result =
        Lwt_main.run
          (Github_pr_dispatch.dispatch_to_subscriptions ~db ~event
             ~delivery_id:"multi-room-1" ~quiet_start:0 ~quiet_end:0
             ~send_message ())
      in
      Alcotest.(check int) "both rooms dispatched" 2 result;
      (* Each room has its own ledger event *)
      let events_x =
        Room_activity_ledger.query ~db ~room_id:"room-x"
          ~event_type:"github_update_delivered" ()
      in
      Alcotest.(check int) "room-x ledger" 1 (List.length events_x);
      let events_y =
        Room_activity_ledger.query ~db ~room_id:"room-y"
          ~event_type:"github_update_delivered" ()
      in
      Alcotest.(check int) "room-y ledger" 1 (List.length events_y);
      (* Each room has its own backlink *)
      let links_x =
        Room_github_backlinks.find_by_room ~db ~room_id:"room-x" ()
      in
      Alcotest.(check int) "room-x backlink" 1 (List.length links_x);
      let links_y =
        Room_github_backlinks.find_by_room ~db ~room_id:"room-y" ()
      in
      Alcotest.(check int) "room-y backlink" 1 (List.length links_y);
      (* PR #70 backlinks from both rooms *)
      let pr_links =
        Room_github_backlinks.find_by_repo_pr ~db ~repo:"org/repo" ~pr_number:70
          ()
      in
      Alcotest.(check int) "PR #70 has 2 backlinks" 2 (List.length pr_links))

(* ---- record_provenance_comment: provenance backlink ---- *)

let test_record_provenance_comment () =
  with_db (fun db ->
      let _inserted =
        Room_github_backlinks.record_provenance_comment ~db ~repo:"org/repo"
          ~pr_number:42 ~github_item_id:"999"
          ~github_url:"https://github.com/org/repo/pull/42#issuecomment-999"
          ~room_id:"github:org/repo:pr:42" ~room_item_id:"999" ()
      in
      (* Verify the backlink was inserted *)
      let links =
        Room_github_backlinks.find_by_repo_pr ~db ~repo:"org/repo" ~pr_number:42
          ()
      in
      Alcotest.(check int) "one provenance backlink" 1 (List.length links);
      let bl = List.hd links in
      Alcotest.(check string)
        "relationship" "provenance_comment"
        (Room_github_backlinks.relationship_to_string bl.relationship);
      Alcotest.(check string)
        "direction" "room_to_github"
        (Room_github_backlinks.direction_to_string bl.direction);
      Alcotest.(check string)
        "github_item_type" "pr_comment"
        (Room_github_backlinks.github_item_type_to_string bl.github_item_type);
      Alcotest.(check string)
        "github_item_id" "999"
        (Option.value ~default:"" bl.github_item_id);
      (* Idempotency: duplicate insert is a no-op *)
      let _inserted2 =
        Room_github_backlinks.record_provenance_comment ~db ~repo:"org/repo"
          ~pr_number:42 ~github_item_id:"999" ~room_id:"github:org/repo:pr:42"
          ~room_item_id:"999" ()
      in
      let links2 =
        Room_github_backlinks.find_by_repo_pr ~db ~repo:"org/repo" ~pr_number:42
          ()
      in
      Alcotest.(check int) "still one after duplicate" 1 (List.length links2))

let suite =
  [
    (* Bot self-loop marker detection *)
    Alcotest.test_case "bot reply marker detected" `Quick
      test_bot_reply_marker_detected;
    Alcotest.test_case "bot reply marker not in normal comment" `Quick
      test_bot_reply_marker_not_in_normal_comment;
    Alcotest.test_case "bot reply marker at start" `Quick
      test_bot_reply_marker_at_start;
    Alcotest.test_case "bot reply marker embedded in long text" `Quick
      test_bot_reply_marker_embedded_in_long_text;
    Alcotest.test_case "partial marker not detected" `Quick
      test_bot_reply_partial_marker_not_detected;
    (* Provenance comment detection *)
    Alcotest.test_case "provenance comment detected" `Quick
      test_provenance_comment_detected;
    Alcotest.test_case "provenance comment not in plain text" `Quick
      test_provenance_comment_not_in_plain_text;
    (* Reply formatting with markers *)
    Alcotest.test_case "format reply preserves marker" `Quick
      test_format_reply_preserves_marker;
    Alcotest.test_case "format reply with empty command" `Quick
      test_format_reply_with_empty_command;
    Alcotest.test_case "format reply with command" `Quick
      test_format_reply_with_command;
    (* Provenance footer ordering *)
    Alcotest.test_case "provenance between response and marker" `Quick
      test_provenance_between_response_and_marker;
    Alcotest.test_case "provenance none omits footer" `Quick
      test_provenance_none_omits_footer;
    Alcotest.test_case "provenance empty omits footer" `Quick
      test_provenance_empty_provenance_omits_footer;
    Alcotest.test_case "provenance with task_id" `Quick
      test_provenance_with_task_id;
    (* Self-loop: bot replies do not retrigger *)
    Alcotest.test_case "bot reply text detected as self-loop" `Quick
      test_bot_reply_text_as_comment_body_would_not_retrigger;
    Alcotest.test_case "bot reply with provenance detected as self-loop" `Quick
      test_bot_reply_with_provenance_detected_as_self_loop;
    Alcotest.test_case "webhook handler skips bot reply comment" `Quick
      test_webhook_handler_skips_bot_reply_comment;
    (* Backlink audit traceability *)
    Alcotest.test_case "backlink traces room scope to github action" `Quick
      test_backlink_traces_room_scope_to_github_action;
    Alcotest.test_case "backlink traces github event to room message" `Quick
      test_backlink_traces_github_event_to_room_message;
    Alcotest.test_case "backlink CI notification traces to room" `Quick
      test_backlink_ci_notification_traces_to_room;
    Alcotest.test_case "backlink by repo PR traces all rooms" `Quick
      test_backlink_by_repo_pr_traces_all_rooms;
    (* Audit export categorization *)
    Alcotest.test_case "audit export categorizes github events" `Quick
      test_audit_export_categorizes_github_backlink_events;
    Alcotest.test_case "audit export categorizes delivery events" `Quick
      test_audit_export_categorizes_delivery_events;
    Alcotest.test_case "audit export mixed github and delivery" `Quick
      test_audit_export_github_and_delivery_mixed;
    Alcotest.test_case "audit export json includes backlink metadata" `Quick
      test_audit_export_json_includes_backlink_metadata;
    Alcotest.test_case "audit export tracks delivery connector" `Quick
      test_audit_export_tracks_delivery_connector;
    (* Room isolation *)
    Alcotest.test_case "audit export room isolation" `Quick
      test_audit_export_room_isolation;
    (* Dispatch integration *)
    Alcotest.test_case "dispatch creates backlink and ledger event" `Quick
      test_dispatch_creates_backlink_and_ledger_event;
    Alcotest.test_case "dispatch notification contains footer links" `Quick
      test_dispatch_notification_contains_footer_links;
    Alcotest.test_case "multiple rooms get independent backlinks" `Quick
      test_multiple_rooms_get_independent_backlinks;
    (* Provenance_comment backlink *)
    Alcotest.test_case
      "record_provenance_comment inserts with correct relationship" `Quick
      test_record_provenance_comment;
  ]
