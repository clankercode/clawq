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

let format_ci_summary (ci : Github_webhook.ci_summary) action =
  let kind_str =
    match ci.kind with
    | `CheckRun -> "Check run"
    | `CheckSuite -> "Check suite"
    | `WorkflowRun -> "Workflow"
  in
  let label = if ci.name <> "" then Printf.sprintf ": %s" ci.name else "" in
  let conclusion_str =
    if ci.conclusion <> "" then ci.conclusion else ci.status
  in
  let sha_str =
    if ci.head_sha <> "" then
      let short =
        if String.length ci.head_sha > 7 then String.sub ci.head_sha 0 7
        else ci.head_sha
      in
      Printf.sprintf "\nSHA: `%s`" short
    else ""
  in
  let actor_str =
    if ci.actor <> "" then Printf.sprintf "\nActor: @%s" ci.actor else ""
  in
  let pr_str =
    match ci.pr_number with
    | Some n -> Printf.sprintf "\nPR: #%d" n
    | None -> ""
  in
  let job_link =
    if
      ci.details_url <> ""
      && (ci.conclusion = "failure"
         || ci.conclusion = "timed_out"
         || ci.conclusion = "cancelled")
    then Printf.sprintf "\nFailing job: %s" ci.details_url
    else ""
  in
  Printf.sprintf
    "%s **%s%s** %s\n\
     Repository: %s/%s%s%s%s\n\
     Status: %s | Conclusion: %s%s\n\
     URL: %s"
    (match ci.kind with
    | `WorkflowRun -> "\xE2\x9A\x99\xEF\xB8\x8F"
    | _ -> "\xF0\x9F\x94\xA7")
    kind_str label action ci.owner ci.repo pr_str sha_str actor_str ci.status
    conclusion_str job_link ci.html_url

let format_review_summary (review : Github_webhook.review_summary) =
  let state_emoji =
    match review.state with
    | Github_webhook.Approved -> "\xE2\x9C\x85"
    | Github_webhook.ChangesRequested -> "\xF0\x9F\x94\xB4"
    | Github_webhook.Commented -> "\xF0\x9F\x92\xAC"
    | Github_webhook.Dismissed -> "\xF0\x9F\x9A\xAB"
    | Github_webhook.Pending -> "\xE2\x8F\xB3"
    | Github_webhook.Unknown_review_state _ -> "\xF0\x9F\x93\x8C"
  in
  let sha_str =
    if review.head_sha <> "" then
      let short =
        if String.length review.head_sha > 7 then String.sub review.head_sha 0 7
        else review.head_sha
      in
      Printf.sprintf "\nSHA: `%s`" short
    else ""
  in
  let body_snippet =
    if review.body <> "" then
      let truncated =
        if String.length review.body > 120 then
          String.sub review.body 0 117 ^ "..."
        else review.body
      in
      Printf.sprintf "\n> %s" truncated
    else ""
  in
  Printf.sprintf
    "%s **PR #%d review** by @%s\nRepository: %s/%s\nState: %s%s%s\nURL: %s"
    state_emoji review.pr_number review.reviewer review.owner review.repo
    review.raw_state sha_str body_snippet review.html_url

let format_mergeability_change = function
  | Github_webhook.MergeableStateChanged { mergeable } ->
      Some (Printf.sprintf "Mergeable: %s" (if mergeable then "yes" else "no"))
  | Github_webhook.LabelsChanged { added; removed } ->
      let parts = [] in
      let parts =
        if added <> [] then
          ("Labels added: " ^ String.concat ", " added) :: parts
        else parts
      in
      let parts =
        if removed <> [] then
          ("Labels removed: " ^ String.concat ", " removed) :: parts
        else parts
      in
      if parts = [] then None else Some (String.concat "; " (List.rev parts))
  | Github_webhook.ReviewDecisionChanged { decision } ->
      Some (Printf.sprintf "Review decision: %s" decision)
  | Github_webhook.ChecksStatusChanged { total; passed; failed; pending } ->
      Some
        (Printf.sprintf "Checks: %d total, %d passed, %d failed, %d pending"
           total passed failed pending)

(** {1 Slack mrkdwn formatters}

    Use [<url|label>] link syntax instead of markdown [label](url). *)

let format_ci_summary_for_slack (ci : Github_webhook.ci_summary) action =
  let kind_str =
    match ci.kind with
    | `CheckRun -> "Check run"
    | `CheckSuite -> "Check suite"
    | `WorkflowRun -> "Workflow"
  in
  let label = if ci.name <> "" then Printf.sprintf ": %s" ci.name else "" in
  let conclusion_str =
    if ci.conclusion <> "" then ci.conclusion else ci.status
  in
  let icon =
    match ci.kind with
    | `WorkflowRun -> "\xE2\x9A\x99\xEF\xB8\x8F"
    | _ -> "\xF0\x9F\x94\xA7"
  in
  let sha_str =
    if ci.head_sha <> "" then
      let short =
        if String.length ci.head_sha > 7 then String.sub ci.head_sha 0 7
        else ci.head_sha
      in
      Printf.sprintf "\nSHA: `%s`" short
    else ""
  in
  let actor_str =
    if ci.actor <> "" then Printf.sprintf "\nActor: @%s" ci.actor else ""
  in
  let pr_str =
    match ci.pr_number with
    | Some n -> Printf.sprintf "\nPR: #%d" n
    | None -> ""
  in
  let job_link =
    if
      ci.details_url <> ""
      && (ci.conclusion = "failure"
         || ci.conclusion = "timed_out"
         || ci.conclusion = "cancelled")
    then Printf.sprintf "\n<%s|Failing job>" ci.details_url
    else ""
  in
  Printf.sprintf
    "%s *%s%s* %s\n%s/%s%s%s%s\nStatus: %s | Conclusion: %s%s\n<%s|View>" icon
    kind_str label action ci.owner ci.repo pr_str sha_str actor_str ci.status
    conclusion_str job_link ci.html_url

let format_review_summary_for_slack (review : Github_webhook.review_summary) =
  let state_emoji =
    match review.state with
    | Github_webhook.Approved -> "\xE2\x9C\x85"
    | Github_webhook.ChangesRequested -> "\xF0\x9F\x94\xB4"
    | Github_webhook.Commented -> "\xF0\x9F\x92\xAC"
    | Github_webhook.Dismissed -> "\xF0\x9F\x9A\xAB"
    | Github_webhook.Pending -> "\xE2\x8F\xB3"
    | Github_webhook.Unknown_review_state _ -> "\xF0\x9F\x93\x8C"
  in
  let sha_str =
    if review.head_sha <> "" then
      let short =
        if String.length review.head_sha > 7 then String.sub review.head_sha 0 7
        else review.head_sha
      in
      Printf.sprintf "\nSHA: `%s`" short
    else ""
  in
  let body_snippet =
    if review.body <> "" then
      let truncated =
        if String.length review.body > 120 then
          String.sub review.body 0 117 ^ "..."
        else review.body
      in
      Printf.sprintf "\n> %s" truncated
    else ""
  in
  Printf.sprintf "%s *PR #%d review* by @%s\n%s/%s\nState: %s%s%s\n<%s|View>"
    state_emoji review.pr_number review.reviewer review.owner review.repo
    review.raw_state sha_str body_snippet review.html_url

(** Format a PR event notification using Slack mrkdwn links for CI and review
    events. Falls back to standard formatting for other event types. *)
let format_pr_event_notification_for_slack
    ~(event : Github_webhook.parsed_event) ~(action : string) =
  match event with
  | CheckRun _ | CheckSuite _ | WorkflowRun _ -> (
      match Github_webhook.ci_summary_of_event event with
      | Some ci -> format_ci_summary_for_slack ci action
      | None -> format_pr_event_notification ~event ~action)
  | PullRequestReview _ | PrReviewComment _ -> (
      match Github_webhook.review_summary_of_event event with
      | Some review -> format_review_summary_for_slack review
      | None -> format_pr_event_notification ~event ~action)
  | _ -> format_pr_event_notification ~event ~action

(** Map a GitHub webhook event type to a subscription notification preference
    key. Returns [None] if the event type doesn't match any preference. *)

(** {1 CI event info extraction}

    Helpers to extract fields needed for dedup key construction, keeping
    [Github_pr_policy] free of [Github_webhook] dependencies. *)

type ci_info = { ci_name : string; ci_conclusion : string; is_ci : bool }

let ci_info_of_event (event : Github_webhook.parsed_event) =
  match event with
  | CheckRun check ->
      {
        ci_name = check.name;
        ci_conclusion =
          (if check.conclusion <> "" then check.conclusion else check.status);
        is_ci = true;
      }
  | CheckSuite suite ->
      {
        ci_name = "";
        ci_conclusion =
          (if suite.conclusion <> "" then suite.conclusion else suite.status);
        is_ci = true;
      }
  | WorkflowRun run ->
      {
        ci_name = run.name;
        ci_conclusion =
          (if run.conclusion <> "" then run.conclusion else run.status);
        is_ci = true;
      }
  | _ -> { ci_name = ""; ci_conclusion = ""; is_ci = false }

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
    ?(connector : string = "")
    ?(quiet_start : int = Github_pr_policy.default_quiet_start)
    ?(quiet_end : int = Github_pr_policy.default_quiet_end)
    ?(max_per_hour : int = 0) ?(dedupe_seconds : int = 60)
    ~(send_message : room_id:string -> text:string -> unit -> unit Lwt.t) () =
  let open Lwt.Syntax in
  (* Deduplicate by delivery_id (in-memory LRU for fast path) *)
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
        let is_slack = String.lowercase_ascii connector = "slack" in
        let text =
          if is_slack then format_pr_event_notification_for_slack ~event ~action
          else
            match event with
            | CheckRun _ | CheckSuite _ | WorkflowRun _ -> (
                match Github_webhook.ci_summary_of_event event with
                | Some ci -> format_ci_summary ci action
                | None -> format_pr_event_notification ~event ~action)
            | PullRequestReview _ | PrReviewComment _ -> (
                match Github_webhook.review_summary_of_event event with
                | Some review -> format_review_summary review
                | None -> format_pr_event_notification ~event ~action)
            | _ -> format_pr_event_notification ~event ~action
        in
        if text = "" then Lwt.return 0
        else
          (* Pre-compute CI info and dedup key once for all subscriptions *)
          let ci = ci_info_of_event event in
          let event_type_str = Github_webhook.event_type_string event in
          let now_hour =
            let tm = Unix.localtime (Unix.gettimeofday ()) in
            tm.Unix.tm_hour
          in
          let* count =
            Lwt_list.fold_left_s
              (fun acc subscription ->
                if not (should_notify_subscription ~subscription ~event) then
                  Lwt.return acc
                else
                  (* Apply policy gates (dedup + quiet hours + rate limit) *)
                  let dedup_key =
                    Github_pr_policy.make_dedup_key ~repo ~pr_number
                      ~ci_name:ci.ci_name ~ci_conclusion:ci.ci_conclusion
                      ~is_ci:ci.is_ci ~delivery_id
                  in
                  let policy_result =
                    Github_pr_policy.decide ~db ~dedup_key
                      ~room_id:subscription.room_id ~hour:now_hour ~quiet_start
                      ~quiet_end ~max_per_hour ~dedupe_seconds ()
                  in
                  match policy_result with
                  | Github_pr_policy.Denied reason ->
                      Logs.info (fun m ->
                          m
                            "GitHub PR dispatch: policy denied room %s for %s \
                             PR #%d: %s"
                            subscription.room_id repo pr_number
                            (Github_pr_policy.reason_to_string reason));
                      Lwt.return acc
                  | Github_pr_policy.Allowed ->
                      Lwt.catch
                        (fun () ->
                          let* () =
                            send_message ~room_id:subscription.room_id ~text ()
                          in
                          Github_pr_policy.record_delivery ~db ~dedup_key
                            ~room_id:subscription.room_id ~repo ~pr_number
                            ~event_type:event_type_str;
                          Logs.info (fun m ->
                              m
                                "GitHub PR dispatch: notified room %s for %s \
                                 PR #%d"
                                subscription.room_id repo pr_number);
                          Lwt.return (acc + 1))
                        (fun exn ->
                          Logs.err (fun m ->
                              m
                                "GitHub PR dispatch: failed to notify room %s: \
                                 %s"
                                subscription.room_id (Printexc.to_string exn));
                          Lwt.return acc))
              0 subscriptions
          in
          if count > 0 then
            Logs.info (fun m ->
                m "GitHub PR dispatch: notified %d rooms for %s PR #%d" count
                  repo pr_number);
          Lwt.return count
