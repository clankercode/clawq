(** Dispatch GitHub PR events to subscribed rooms/threads.

    When a GitHub webhook event matches a subscribed PR, this module formats a
    notification and delivers it to the appropriate room and thread based on the
    subscription configuration. *)

(** Deduplication cache to prevent duplicate event delivery. Uses delivery_id as
    the key. *)
let dedup = Channel_util.Lru_dedup.create 500

(** Format a PR event notification message. *)
let format_pr_event_notification ~(event : Github_webhook.parsed_event)
    ~(action : string) =
  match event with
  | PullRequest pr ->
      let emoji =
        match action with
        | "opened" -> "\xF0\x9F\x9F\xA2"
        | "reopened" -> "\xF0\x9F\x94\x84"
        | "synchronize" -> "\xF0\x9F\x94\x84"
        | "ready_for_review" -> "\xE2\x9C\x85"
        | "closed" -> "\xF0\x9F\x94\xB4"
        | _ -> "\xF0\x9F\x93\x8C"
      in
      Printf.sprintf
        "%s **PR #%d** %s by @%s\n\
         Repository: %s/%s\n\
         Branch: `%s` \xE2\x86\x92 `%s`\n\
         Title: %s\n\
         URL: %s"
        emoji pr.pr_number action pr.pr_author pr.owner pr.repo pr.head_branch
        pr.base_branch pr.pr_title pr.html_url
  | IssueComment comment when comment.is_pr ->
      Printf.sprintf
        "\xF0\x9F\x92\xAC **New comment on PR #%d** by @%s\n\
         Repository: %s/%s\n\
         Title: %s\n\
         Comment:\n\
         > %s\n\
         URL: %s"
        comment.issue_number comment.comment_author comment.owner comment.repo
        comment.issue_title
        (if String.length comment.comment_body > 500 then
           String.sub comment.comment_body 0 497 ^ "..."
         else comment.comment_body)
        comment.html_url
  | PrReviewComment review ->
      Printf.sprintf
        "\xF0\x9F\x93\x9D **Review comment on PR #%d** by @%s\n\
         Repository: %s/%s\n\
         File: `%s`\n\
         Comment:\n\
         > %s\n\
         URL: %s"
        review.pr_number review.comment_author review.owner review.repo
        review.file_path
        (if String.length review.comment_body > 500 then
           String.sub review.comment_body 0 497 ^ "..."
         else review.comment_body)
        review.html_url
  | PullRequestReview review ->
      Printf.sprintf
        "\xF0\x9F\x93\x9D **PR review #%d** by @%s\n\
         Repository: %s/%s\n\
         State: %s\n\
         URL: %s"
        review.pr_number review.review_author review.owner review.repo
        review.state review.html_url
  | CheckRun check ->
      Printf.sprintf
        "\xF0\x9F\x94\xA7 **Check run: %s** %s\n\
         Repository: %s/%s\n\
         Status: %s | Conclusion: %s\n\
         URL: %s"
        check.name action check.owner check.repo check.status check.conclusion
        check.html_url
  | CheckSuite suite ->
      Printf.sprintf
        "\xF0\x9F\x94\xA7 **Check suite** %s\n\
         Repository: %s/%s\n\
         Status: %s | Conclusion: %s\n\
         URL: %s"
        action suite.owner suite.repo suite.status suite.conclusion
        suite.html_url
  | WorkflowRun run ->
      Printf.sprintf
        "\xE2\x9A\x99\xEF\xB8\x8F **Workflow: %s** %s\n\
         Repository: %s/%s\n\
         Status: %s | Conclusion: %s\n\
         URL: %s"
        run.name action run.owner run.repo run.status run.conclusion
        run.html_url
  | _ -> ""

(** Map a GitHub webhook event type to a subscription notification preference
    key. Returns [None] if the event type doesn't match any preference. *)
let event_type_to_preference_key (event : Github_webhook.parsed_event) =
  match event with
  | PullRequest pr -> (
      match pr.action with
      | "opened" | "reopened" | "ready_for_review" -> Some "opened"
      | "synchronize" -> Some "status" (* Treat sync as status update *)
      | "closed" -> Some "closed"
      | _ -> None)
  | IssueComment _ -> Some "comment"
  | PrReviewComment _ ->
      Some "review_comment" (* Use review_comment, not review *)
  | PullRequestReview _ -> Some "review"
  | CheckRun _ | CheckSuite _ | WorkflowRun _ -> Some "status"
  | Ignored -> None

(** Check if a subscription should be notified for a given event. *)
let should_notify_subscription
    ~(subscription : Github_pr_subscriptions.subscription)
    ~(event : Github_webhook.parsed_event) =
  let event_type = event_type_to_preference_key event in
  match event_type with
  | Some et ->
      Github_pr_subscriptions.should_notify ~subscription ~event_type:et
  | None -> false

(** [dispatch_to_subscriptions ~db ~event ~delivery_id ~send_message] dispatches
    a GitHub PR event to all subscribed rooms.

    - [db] SQLite database handle
    - [event] parsed GitHub webhook event
    - [delivery_id] GitHub delivery ID for deduplication
    - [send_message] callback to send a message to a room. The callback receives
      [~room_id] and [~text].

    Returns the number of rooms notified. *)
let dispatch_to_subscriptions ~(db : Sqlite3.db)
    ~(event : Github_webhook.parsed_event) ~(delivery_id : string)
    ~(send_message : room_id:string -> text:string -> unit -> unit Lwt.t) () =
  let open Lwt.Syntax in
  (* Deduplicate by delivery_id *)
  if
    delivery_id <> "" && Channel_util.Lru_dedup.check_and_mark dedup delivery_id
  then (
    Logs.debug (fun m ->
        m "GitHub PR dispatch: ignoring duplicate delivery %s" delivery_id);
    Lwt.return 0)
  else
    let repo, pr_number =
      match event with
      | Github_webhook.PullRequest pr -> (pr.owner ^ "/" ^ pr.repo, pr.pr_number)
      | Github_webhook.IssueComment comment when comment.is_pr ->
          (comment.owner ^ "/" ^ comment.repo, comment.issue_number)
      | Github_webhook.PrReviewComment review ->
          (review.owner ^ "/" ^ review.repo, review.pr_number)
      | Github_webhook.PullRequestReview review ->
          (review.owner ^ "/" ^ review.repo, review.pr_number)
      | Github_webhook.CheckRun check -> (
          match check.pr_number with
          | Some pr_n -> (check.owner ^ "/" ^ check.repo, pr_n)
          | None -> ("", 0))
      | Github_webhook.CheckSuite suite -> (
          match suite.pr_number with
          | Some pr_n -> (suite.owner ^ "/" ^ suite.repo, pr_n)
          | None -> ("", 0))
      | Github_webhook.WorkflowRun run -> (
          match run.pr_number with
          | Some pr_n -> (run.owner ^ "/" ^ run.repo, pr_n)
          | None -> ("", 0))
      | _ -> ("", 0)
    in
    if repo = "" || pr_number <= 0 then Lwt.return 0
    else
      (* Find all subscriptions for this repo/PR *)
      let subscriptions =
        Github_pr_subscriptions.find_by_repo_pr ~db ~repo ~pr_number
      in
      if subscriptions = [] then (
        Logs.debug (fun m ->
            m "GitHub PR dispatch: no subscriptions for %s PR #%d" repo
              pr_number);
        Lwt.return 0)
      else
        let action =
          match event with
          | Github_webhook.PullRequest pr -> pr.action
          | Github_webhook.IssueComment _ -> "comment"
          | Github_webhook.PrReviewComment _ -> "review_comment"
          | Github_webhook.PullRequestReview review -> review.state
          | Github_webhook.CheckRun check ->
              if check.conclusion <> "" then check.conclusion else check.status
          | Github_webhook.CheckSuite suite ->
              if suite.conclusion <> "" then suite.conclusion else suite.status
          | Github_webhook.WorkflowRun run ->
              if run.conclusion <> "" then run.conclusion else run.status
          | Github_webhook.Ignored -> "unknown"
        in
        let text = format_pr_event_notification ~event ~action in
        if text = "" then Lwt.return 0
        else
          let* count =
            Lwt_list.fold_left_s
              (fun acc subscription ->
                if not (should_notify_subscription ~subscription ~event) then
                  Lwt.return acc
                else
                  Lwt.catch
                    (fun () ->
                      let* () =
                        send_message ~room_id:subscription.room_id ~text ()
                      in
                      Logs.info (fun m ->
                          m "GitHub PR dispatch: notified room %s for %s PR #%d"
                            subscription.room_id repo pr_number);
                      Lwt.return (acc + 1))
                    (fun exn ->
                      Logs.err (fun m ->
                          m "GitHub PR dispatch: failed to notify room %s: %s"
                            subscription.room_id (Printexc.to_string exn));
                      Lwt.return acc))
              0 subscriptions
          in
          if count > 0 then
            Logs.info (fun m ->
                m "GitHub PR dispatch: notified %d rooms for %s PR #%d" count
                  repo pr_number);
          Lwt.return count
