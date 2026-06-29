type pr_event = {
  action : string;
  owner : string;
  repo : string;
  pr_number : int;
  pr_title : string;
  pr_body : string;
  pr_author : string;
  base_branch : string;
  head_branch : string;
  html_url : string;
}

type issue_comment_event = {
  owner : string;
  repo : string;
  issue_number : int;
  is_pr : bool;
  comment_id : int;
  comment_author : string;
  comment_body : string;
  issue_title : string;
  html_url : string;
}

type pr_review_comment_event = {
  owner : string;
  repo : string;
  pr_number : int;
  comment_id : int;
  comment_author : string;
  comment_body : string;
  in_reply_to_id : int option;
  diff_hunk : string;
  file_path : string;
  pr_title : string;
  html_url : string;
  head_sha : string;
}

type pr_review_event = {
  owner : string;
  repo : string;
  pr_number : int;
  review_id : int;
  review_author : string;
  state : string;
  body : string;
  html_url : string;
  head_sha : string;
}

type check_run_event = {
  owner : string;
  repo : string;
  name : string;
  status : string;
  conclusion : string;
  pr_number : int option;
  html_url : string;
  head_sha : string;
  actor : string;
  details_url : string;
}

type check_suite_event = {
  owner : string;
  repo : string;
  status : string;
  conclusion : string;
  pr_number : int option;
  html_url : string;
  head_sha : string;
  actor : string;
}

type workflow_run_event = {
  owner : string;
  repo : string;
  name : string;
  status : string;
  conclusion : string;
  pr_number : int option;
  html_url : string;
  head_sha : string;
  actor : string;
}

type parsed_event =
  | PullRequest of pr_event
  | IssueComment of issue_comment_event
  | PrReviewComment of pr_review_comment_event
  | PullRequestReview of pr_review_event
  | CheckRun of check_run_event
  | CheckSuite of check_suite_event
  | WorkflowRun of workflow_run_event
  | Ignored

type ci_summary = {
  kind : [ `CheckRun | `CheckSuite | `WorkflowRun ];
  name : string;
  status : string;
  conclusion : string;
  owner : string;
  repo : string;
  pr_number : int option;
  html_url : string;
  head_sha : string;
  actor : string;
  details_url : string;
}
(** Normalized CI summary: stable typed representation of check_run,
    check_suite, and workflow_run events. *)

(** Normalized review summary: stable typed representation of
    pull_request_review events with state classification. *)
type review_state =
  | Approved
  | ChangesRequested
  | Commented
  | Dismissed
  | Pending
  | Unknown_review_state of string

type review_summary = {
  state : review_state;
  raw_state : string;
  reviewer : string;
  body : string;
  owner : string;
  repo : string;
  pr_number : int;
  html_url : string;
  head_sha : string;
}

(** Mergeability-relevant change detected from PR events. *)
type mergeability_change =
  | MergeableStateChanged of { mergeable : bool }
  | LabelsChanged of { added : string list; removed : string list }
  | ReviewDecisionChanged of { decision : string }
  | ChecksStatusChanged of {
      total : int;
      passed : int;
      failed : int;
      pending : int;
    }

(** Summarize a CI event into a normalized [ci_summary]. Returns [None] if the
    event is not a CI event. *)
let ci_summary_of_event event =
  match event with
  | CheckRun e ->
      Some
        {
          kind = `CheckRun;
          name = e.name;
          status = e.status;
          conclusion = e.conclusion;
          owner = e.owner;
          repo = e.repo;
          pr_number = e.pr_number;
          html_url = e.html_url;
          head_sha = e.head_sha;
          actor = e.actor;
          details_url = e.details_url;
        }
  | CheckSuite e ->
      Some
        {
          kind = `CheckSuite;
          name = "";
          status = e.status;
          conclusion = e.conclusion;
          owner = e.owner;
          repo = e.repo;
          pr_number = e.pr_number;
          html_url = e.html_url;
          head_sha = e.head_sha;
          actor = e.actor;
          details_url = "";
        }
  | WorkflowRun e ->
      Some
        {
          kind = `WorkflowRun;
          name = e.name;
          status = e.status;
          conclusion = e.conclusion;
          owner = e.owner;
          repo = e.repo;
          pr_number = e.pr_number;
          html_url = e.html_url;
          head_sha = e.head_sha;
          actor = e.actor;
          details_url = "";
        }
  | PullRequest _ | IssueComment _ | PrReviewComment _ | PullRequestReview _
  | Ignored ->
      None

(** Parse a raw GitHub review state string into a normalized [review_state]. *)
let parse_review_state = function
  | "approved" -> Approved
  | "changes_requested" -> ChangesRequested
  | "commented" -> Commented
  | "dismissed" -> Dismissed
  | "pending" -> Pending
  | other -> Unknown_review_state other

(** Summarize a review event into a normalized [review_summary]. Returns [None]
    if the event is not a review event. *)
let review_summary_of_event event =
  match event with
  | PullRequestReview e ->
      Some
        {
          state = parse_review_state e.state;
          raw_state = e.state;
          reviewer = e.review_author;
          body = e.body;
          owner = e.owner;
          repo = e.repo;
          pr_number = e.pr_number;
          html_url = e.html_url;
          head_sha = e.head_sha;
        }
  | PrReviewComment e ->
      Some
        {
          state = Commented;
          raw_state = "commented";
          reviewer = e.comment_author;
          body = e.comment_body;
          owner = e.owner;
          repo = e.repo;
          pr_number = e.pr_number;
          html_url = e.html_url;
          head_sha = e.head_sha;
        }
  | PullRequest _ | IssueComment _ | CheckRun _ | CheckSuite _ | WorkflowRun _
  | Ignored ->
      None

(** Detect mergeability-relevant changes from a PR webhook event. Returns a list
    of detected changes (may be empty if no mergeability signal is present). *)
let detect_mergeability_changes ~event_type ~body =
  try
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    match event_type with
    | "pull_request" ->
        let action = try json |> member "action" |> to_string with _ -> "" in
        let changes = json |> member "changes" in
        let detected = ref [] in
        (* Detect label changes - handle both formats: *)
        (* 1. GitHub "labeled"/"unlabeled" action with top-level label *)
        (match action with
        | "labeled" -> (
            try
              let label = json |> member "label" in
              let name = label |> member "name" |> to_string in
              if name <> "" then
                detected :=
                  LabelsChanged { added = [ name ]; removed = [] } :: !detected
            with _ -> ())
        | "unlabeled" -> (
            try
              let label = json |> member "label" in
              let name = label |> member "name" |> to_string in
              if name <> "" then
                detected :=
                  LabelsChanged { added = []; removed = [ name ] } :: !detected
            with _ -> ())
        | _ -> (
            (* 2. Changes-based format *)
            try
              let labels = changes |> member "labels" in
              let added =
                labels |> member "added" |> to_list
                |> List.filter_map (fun j ->
                    try Some (j |> member "name" |> to_string) with _ -> None)
              in
              let removed =
                labels |> member "removed" |> to_list
                |> List.filter_map (fun j ->
                    try Some (j |> member "name" |> to_string) with _ -> None)
              in
              if added <> [] || removed <> [] then
                detected := LabelsChanged { added; removed } :: !detected
            with _ -> ()));
        (* Detect mergeable state changes on synchronize/edit *)
        (match action with
        | "synchronize" | "edited" -> (
            let pr = json |> member "pull_request" in
            try
              let mergeable = pr |> member "mergeable" |> to_bool in
              detected := MergeableStateChanged { mergeable } :: !detected
            with _ -> ())
        | _ -> ());
        (* Detect review decision changes *)
        (try
           let pr = json |> member "pull_request" in
           let decision = pr |> member "review_decision" |> to_string_option in
           match decision with
           | Some d when d <> "" ->
               detected := ReviewDecisionChanged { decision = d } :: !detected
           | _ -> ()
         with _ -> ());
        List.rev !detected
    | "check_run" | "check_suite" | "workflow_run" ->
        (* CI events can indicate checks status changes *)
        let detected = ref [] in
        (match event_type with
        | "check_run" ->
            let check_run = json |> member "check_run" in
            let status =
              try check_run |> member "status" |> to_string with _ -> ""
            in
            if status = "completed" then begin
              let conclusion =
                try check_run |> member "conclusion" |> to_string with _ -> ""
              in
              let passed = if conclusion = "success" then 1 else 0 in
              let failed =
                if conclusion = "failure" || conclusion = "timed_out" then 1
                else 0
              in
              detected :=
                ChecksStatusChanged { total = 1; passed; failed; pending = 0 }
                :: !detected
            end
            else if
              status = "in_progress" || status = "queued"
              || status = "requested"
            then
              detected :=
                ChecksStatusChanged
                  { total = 1; passed = 0; failed = 0; pending = 1 }
                :: !detected
        | "check_suite" ->
            let cs = json |> member "check_suite" in
            let status =
              try cs |> member "status" |> to_string with _ -> ""
            in
            if status = "completed" then begin
              let conclusion =
                try cs |> member "conclusion" |> to_string with _ -> ""
              in
              let passed = if conclusion = "success" then 1 else 0 in
              let failed =
                if conclusion = "failure" || conclusion = "timed_out" then 1
                else 0
              in
              detected :=
                ChecksStatusChanged { total = 1; passed; failed; pending = 0 }
                :: !detected
            end
            else if
              status = "in_progress" || status = "requested"
              || status = "queued"
            then
              detected :=
                ChecksStatusChanged
                  { total = 1; passed = 0; failed = 0; pending = 1 }
                :: !detected
        | "workflow_run" ->
            let wr = json |> member "workflow_run" in
            let status =
              try wr |> member "status" |> to_string with _ -> ""
            in
            if status = "completed" then begin
              let conclusion =
                try wr |> member "conclusion" |> to_string with _ -> ""
              in
              let passed = if conclusion = "success" then 1 else 0 in
              let failed =
                if conclusion = "failure" || conclusion = "timed_out" then 1
                else 0
              in
              detected :=
                ChecksStatusChanged { total = 1; passed; failed; pending = 0 }
                :: !detected
            end
            else if
              status = "in_progress" || status = "requested"
              || status = "queued"
            then
              detected :=
                ChecksStatusChanged
                  { total = 1; passed = 0; failed = 0; pending = 1 }
                :: !detected
        | _ -> ());
        List.rev !detected
    | _ -> []
  with _ -> []

let verify_signature ~secret ~body ~signature_header =
  let prefix = "sha256=" in
  let prefix_len = String.length prefix in
  if
    String.length signature_header <= prefix_len
    || String.sub signature_header 0 prefix_len <> prefix
  then false
  else
    let expected =
      "sha256=" ^ Digestif.SHA256.(hmac_string ~key:secret body |> to_hex)
    in
    Eqaf.equal expected signature_header

let parse_event ~event_type ~body =
  try
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    let repo_json = json |> member "repository" in
    let owner =
      try repo_json |> member "owner" |> member "login" |> to_string
      with _ -> ""
    in
    let repo = try repo_json |> member "name" |> to_string with _ -> "" in
    match event_type with
    | "pull_request" -> (
        let action = try json |> member "action" |> to_string with _ -> "" in
        match action with
        | "opened" | "edited" | "reopened" | "synchronize" | "ready_for_review"
        | "closed" | "review_requested" | "review_request_removed" ->
            let pr = json |> member "pull_request" in
            let pr_number = try pr |> member "number" |> to_int with _ -> 0 in
            let pr_title =
              try pr |> member "title" |> to_string with _ -> ""
            in
            let pr_body = try pr |> member "body" |> to_string with _ -> "" in
            let pr_author =
              try pr |> member "user" |> member "login" |> to_string
              with _ -> ""
            in
            let base_branch =
              try pr |> member "base" |> member "ref" |> to_string
              with _ -> ""
            in
            let head_branch =
              try pr |> member "head" |> member "ref" |> to_string
              with _ -> ""
            in
            let html_url =
              try pr |> member "html_url" |> to_string with _ -> ""
            in
            PullRequest
              {
                action;
                owner;
                repo;
                pr_number;
                pr_title;
                pr_body;
                pr_author;
                base_branch;
                head_branch;
                html_url;
              }
        | _ -> Ignored)
    | "issue_comment" -> (
        let action = try json |> member "action" |> to_string with _ -> "" in
        match action with
        | "created" ->
            let issue = json |> member "issue" in
            let issue_number =
              try issue |> member "number" |> to_int with _ -> 0
            in
            let is_pr =
              try
                ignore (issue |> member "pull_request");
                true
              with _ -> false
            in
            let is_pr = is_pr && issue |> member "pull_request" <> `Null in
            let issue_title =
              try issue |> member "title" |> to_string with _ -> ""
            in
            let comment = json |> member "comment" in
            let comment_id =
              try comment |> member "id" |> to_int with _ -> 0
            in
            let comment_author =
              try comment |> member "user" |> member "login" |> to_string
              with _ -> ""
            in
            let comment_body =
              try comment |> member "body" |> to_string with _ -> ""
            in
            let html_url =
              try comment |> member "html_url" |> to_string with _ -> ""
            in
            IssueComment
              {
                owner;
                repo;
                issue_number;
                is_pr;
                comment_id;
                comment_author;
                comment_body;
                issue_title;
                html_url;
              }
        | _ -> Ignored)
    | "pull_request_review_comment" -> (
        let action = try json |> member "action" |> to_string with _ -> "" in
        match action with
        | "created" ->
            let comment = json |> member "comment" in
            let pr = json |> member "pull_request" in
            let pr_number = try pr |> member "number" |> to_int with _ -> 0 in
            let comment_id =
              try comment |> member "id" |> to_int with _ -> 0
            in
            let comment_author =
              try comment |> member "user" |> member "login" |> to_string
              with _ -> ""
            in
            let comment_body =
              try comment |> member "body" |> to_string with _ -> ""
            in
            let in_reply_to_id =
              try Some (comment |> member "in_reply_to_id" |> to_int)
              with _ -> None
            in
            let diff_hunk =
              try comment |> member "diff_hunk" |> to_string with _ -> ""
            in
            let file_path =
              try comment |> member "path" |> to_string with _ -> ""
            in
            let pr_title =
              try pr |> member "title" |> to_string with _ -> ""
            in
            let html_url =
              try comment |> member "html_url" |> to_string with _ -> ""
            in
            let head_sha =
              try pr |> member "head" |> member "sha" |> to_string
              with _ -> ""
            in
            PrReviewComment
              {
                owner;
                repo;
                pr_number;
                comment_id;
                comment_author;
                comment_body;
                in_reply_to_id;
                diff_hunk;
                file_path;
                pr_title;
                html_url;
                head_sha;
              }
        | _ -> Ignored)
    | "pull_request_review" -> (
        let action = try json |> member "action" |> to_string with _ -> "" in
        match action with
        | "submitted" | "edited" ->
            let review = json |> member "review" in
            let pr = json |> member "pull_request" in
            let pr_number = try pr |> member "number" |> to_int with _ -> 0 in
            let review_id = try review |> member "id" |> to_int with _ -> 0 in
            let review_author =
              try review |> member "user" |> member "login" |> to_string
              with _ -> ""
            in
            let state =
              try review |> member "state" |> to_string with _ -> ""
            in
            let body =
              try review |> member "body" |> to_string with _ -> ""
            in
            let html_url =
              try review |> member "html_url" |> to_string with _ -> ""
            in
            let head_sha =
              try pr |> member "head" |> member "sha" |> to_string
              with _ -> ""
            in
            PullRequestReview
              {
                owner;
                repo;
                pr_number;
                review_id;
                review_author;
                state;
                body;
                html_url;
                head_sha;
              }
        | _ -> Ignored)
    | "check_run" -> (
        let action = try json |> member "action" |> to_string with _ -> "" in
        match action with
        | "completed" | "created" | "requested_action" ->
            let check_run = json |> member "check_run" in
            let name =
              try check_run |> member "name" |> to_string with _ -> ""
            in
            let status =
              try check_run |> member "status" |> to_string with _ -> ""
            in
            let conclusion =
              try check_run |> member "conclusion" |> to_string with _ -> ""
            in
            let html_url =
              try check_run |> member "html_url" |> to_string with _ -> ""
            in
            let pr_number =
              try
                Some
                  (check_run |> member "pull_requests" |> to_list |> List.hd
                 |> member "number" |> to_int)
              with _ -> None
            in
            let head_sha =
              try check_run |> member "head_sha" |> to_string with _ -> ""
            in
            let actor =
              try json |> member "sender" |> member "login" |> to_string
              with _ -> ""
            in
            let details_url =
              try check_run |> member "details_url" |> to_string with _ -> ""
            in
            CheckRun
              {
                owner;
                repo;
                name;
                status;
                conclusion;
                pr_number;
                html_url;
                head_sha;
                actor;
                details_url;
              }
        | _ -> Ignored)
    | "check_suite" -> (
        let action = try json |> member "action" |> to_string with _ -> "" in
        match action with
        | "completed" | "requested" | "rerequested" ->
            let check_suite = json |> member "check_suite" in
            let status =
              try check_suite |> member "status" |> to_string with _ -> ""
            in
            let conclusion =
              try check_suite |> member "conclusion" |> to_string with _ -> ""
            in
            let html_url =
              try check_suite |> member "html_url" |> to_string with _ -> ""
            in
            let pr_number =
              try
                Some
                  (check_suite |> member "pull_requests" |> to_list |> List.hd
                 |> member "number" |> to_int)
              with _ -> None
            in
            let head_sha =
              try check_suite |> member "head_sha" |> to_string with _ -> ""
            in
            let actor =
              try json |> member "sender" |> member "login" |> to_string
              with _ -> ""
            in
            CheckSuite
              {
                owner;
                repo;
                status;
                conclusion;
                pr_number;
                html_url;
                head_sha;
                actor;
              }
        | _ -> Ignored)
    | "workflow_run" -> (
        let action = try json |> member "action" |> to_string with _ -> "" in
        match action with
        | "completed" | "requested" | "in_progress" ->
            let workflow_run = json |> member "workflow_run" in
            let name =
              try workflow_run |> member "name" |> to_string with _ -> ""
            in
            let status =
              try workflow_run |> member "status" |> to_string with _ -> ""
            in
            let conclusion =
              try workflow_run |> member "conclusion" |> to_string
              with _ -> ""
            in
            let html_url =
              try workflow_run |> member "html_url" |> to_string with _ -> ""
            in
            let pr_number =
              try
                Some
                  (workflow_run |> member "pull_requests" |> to_list |> List.hd
                 |> member "number" |> to_int)
              with _ -> None
            in
            let head_sha =
              try workflow_run |> member "head_sha" |> to_string with _ -> ""
            in
            let actor =
              try workflow_run |> member "actor" |> member "login" |> to_string
              with _ -> (
                try json |> member "sender" |> member "login" |> to_string
                with _ -> "")
            in
            WorkflowRun
              {
                owner;
                repo;
                name;
                status;
                conclusion;
                pr_number;
                html_url;
                head_sha;
                actor;
              }
        | _ -> Ignored)
    | _ -> Ignored
  with _ -> Ignored

let session_key event =
  match event with
  | PullRequest e ->
      Printf.sprintf "github:%s/%s:pr:%d" e.owner e.repo e.pr_number
  | IssueComment e ->
      if e.is_pr then
        Printf.sprintf "github:%s/%s:pr:%d" e.owner e.repo e.issue_number
      else Printf.sprintf "github:%s/%s:issue:%d" e.owner e.repo e.issue_number
  | PrReviewComment e ->
      Printf.sprintf "github:%s/%s:pr:%d" e.owner e.repo e.pr_number
  | PullRequestReview e ->
      Printf.sprintf "github:%s/%s:pr:%d" e.owner e.repo e.pr_number
  | CheckRun e ->
      Printf.sprintf "github:%s/%s:check_run:%s" e.owner e.repo e.name
  | CheckSuite e -> Printf.sprintf "github:%s/%s:check_suite" e.owner e.repo
  | WorkflowRun e ->
      Printf.sprintf "github:%s/%s:workflow:%s" e.owner e.repo e.name
  | Ignored -> "github:unknown"

let repo_of_event event =
  match event with
  | PullRequest e -> (e.owner, e.repo)
  | IssueComment e -> (e.owner, e.repo)
  | PrReviewComment e -> (e.owner, e.repo)
  | PullRequestReview e -> (e.owner, e.repo)
  | CheckRun e -> (e.owner, e.repo)
  | CheckSuite e -> (e.owner, e.repo)
  | WorkflowRun e -> (e.owner, e.repo)
  | Ignored -> ("", "")

let author_of_event event =
  match event with
  | PullRequest e -> e.pr_author
  | IssueComment e -> e.comment_author
  | PrReviewComment e -> e.comment_author
  | PullRequestReview e -> e.review_author
  | CheckRun _ | CheckSuite _ | WorkflowRun _ -> "github-app"
  | Ignored -> ""

let event_type_string event =
  match event with
  | PullRequest _ -> "pull_request"
  | IssueComment _ -> "issue_comment"
  | PrReviewComment _ -> "pull_request_review_comment"
  | PullRequestReview _ -> "pull_request_review"
  | CheckRun _ -> "check_run"
  | CheckSuite _ -> "check_suite"
  | WorkflowRun _ -> "workflow_run"
  | Ignored -> "ignored"

let extract_clawq ~event ~pr_files =
  let text =
    match event with
    | PullRequest e -> e.pr_body
    | IssueComment e -> e.comment_body
    | PrReviewComment e -> e.comment_body
    | PullRequestReview e -> e.body
    | CheckRun _ | CheckSuite _ | WorkflowRun _ -> ""
    | Ignored -> ""
  in
  let lines = String.split_on_char '\n' text in
  let rec find_clawq = function
    | [] -> None
    | line :: rest ->
        let trimmed = String.trim line in
        let lower = String.lowercase_ascii trimmed in
        if String.length lower >= 6 && String.sub lower 0 6 = "/clawq" then (
          let command_first_line =
            let after_prefix =
              String.sub trimmed 6 (String.length trimmed - 6)
            in
            String.trim after_prefix
          in
          let rec collect_lines acc = function
            | [] -> List.rev acc
            | l :: rest2 ->
                if String.trim l = "" then List.rev acc
                else collect_lines (l :: acc) rest2
          in
          let more_lines = collect_lines [] rest in
          let user_message =
            if more_lines = [] then command_first_line
            else command_first_line ^ "\n" ^ String.concat "\n" more_lines
          in
          let truncate s max_len =
            if String.length s <= max_len then s
            else String.sub s 0 max_len ^ "..."
          in
          let buf = Buffer.create 1024 in
          Buffer.add_string buf "## GitHub Context\n";
          let owner, repo = repo_of_event event in
          Buffer.add_string buf
            (Printf.sprintf "Repository: %s/%s\n" owner repo);
          (match event with
          | PullRequest e ->
              Buffer.add_string buf
                (Printf.sprintf "PR #%d: \"%s\"\n" e.pr_number e.pr_title);
              Buffer.add_string buf
                (Printf.sprintf "Author: @%s | base: %s -> head: %s\n"
                   e.pr_author e.base_branch e.head_branch);
              Buffer.add_string buf
                (Printf.sprintf "\nPR Description:\n  %s\n"
                   (truncate e.pr_body 2000));
              Buffer.add_string buf
                (Printf.sprintf "\nEvent: pull_request %s\n" e.action);
              Buffer.add_string buf (Printf.sprintf "PR URL: %s\n" e.html_url)
          | IssueComment e ->
              let kind = if e.is_pr then "PR" else "Issue" in
              Buffer.add_string buf
                (Printf.sprintf "%s #%d: \"%s\"\n" kind e.issue_number
                   e.issue_title);
              Buffer.add_string buf
                (Printf.sprintf "\nEvent: issue_comment created\n");
              Buffer.add_string buf
                (Printf.sprintf "Full comment by @%s:\n  %s\n" e.comment_author
                   (truncate e.comment_body 2000));
              Buffer.add_string buf
                (Printf.sprintf "Comment URL: %s\n" e.html_url)
          | PrReviewComment e ->
              Buffer.add_string buf
                (Printf.sprintf "PR #%d: \"%s\"\n" e.pr_number e.pr_title);
              Buffer.add_string buf
                (Printf.sprintf "\nEvent: pull_request_review_comment created\n");
              Buffer.add_string buf (Printf.sprintf "File: %s\n" e.file_path);
              Buffer.add_string buf
                (Printf.sprintf "Diff hunk:\n  %s\n" e.diff_hunk);
              Buffer.add_string buf
                (Printf.sprintf "Full comment by @%s:\n  %s\n" e.comment_author
                   (truncate e.comment_body 2000));
              Buffer.add_string buf
                (Printf.sprintf "Comment URL: %s\n" e.html_url)
          | PullRequestReview e ->
              Buffer.add_string buf
                (Printf.sprintf "PR #%d review by @%s\n" e.pr_number
                   e.review_author);
              Buffer.add_string buf (Printf.sprintf "State: %s\n" e.state);
              if e.head_sha <> "" then
                Buffer.add_string buf (Printf.sprintf "SHA: %s\n" e.head_sha);
              if e.body <> "" then
                Buffer.add_string buf
                  (Printf.sprintf "Review body: %s\n" (truncate e.body 2000));
              Buffer.add_string buf
                (Printf.sprintf "Review URL: %s\n" e.html_url)
          | CheckRun e ->
              Buffer.add_string buf (Printf.sprintf "Check run: %s\n" e.name);
              Buffer.add_string buf
                (Printf.sprintf "Status: %s | Conclusion: %s\n" e.status
                   e.conclusion);
              if e.head_sha <> "" then
                Buffer.add_string buf (Printf.sprintf "SHA: %s\n" e.head_sha);
              if e.actor <> "" then
                Buffer.add_string buf (Printf.sprintf "Actor: %s\n" e.actor);
              if
                e.details_url <> ""
                && (e.conclusion = "failure" || e.conclusion = "timed_out")
              then
                Buffer.add_string buf
                  (Printf.sprintf "Failing job: %s\n" e.details_url);
              Buffer.add_string buf (Printf.sprintf "URL: %s\n" e.html_url)
          | CheckSuite e ->
              Buffer.add_string buf (Printf.sprintf "Check suite\n");
              Buffer.add_string buf
                (Printf.sprintf "Status: %s | Conclusion: %s\n" e.status
                   e.conclusion);
              if e.head_sha <> "" then
                Buffer.add_string buf (Printf.sprintf "SHA: %s\n" e.head_sha);
              if e.actor <> "" then
                Buffer.add_string buf (Printf.sprintf "Actor: %s\n" e.actor);
              Buffer.add_string buf (Printf.sprintf "URL: %s\n" e.html_url)
          | WorkflowRun e ->
              Buffer.add_string buf (Printf.sprintf "Workflow: %s\n" e.name);
              Buffer.add_string buf
                (Printf.sprintf "Status: %s | Conclusion: %s\n" e.status
                   e.conclusion);
              if e.head_sha <> "" then
                Buffer.add_string buf (Printf.sprintf "SHA: %s\n" e.head_sha);
              if e.actor <> "" then
                Buffer.add_string buf (Printf.sprintf "Actor: %s\n" e.actor);
              Buffer.add_string buf (Printf.sprintf "URL: %s\n" e.html_url)
          | Ignored -> ());
          if pr_files <> [] then begin
            let count = List.length pr_files in
            Buffer.add_string buf
              (Printf.sprintf "\nChanged files (%d):\n" count);
            let show = min 20 count in
            List.iteri
              (fun i (filename, _status, additions, deletions) ->
                if i < show then
                  Buffer.add_string buf
                    (Printf.sprintf "  - %s (+%d -%d)\n" filename additions
                       deletions))
              pr_files;
            if count > 20 then
              Buffer.add_string buf
                (Printf.sprintf "  ... and %d more\n" (count - 20));
            let owner2, repo2 = repo_of_event event in
            let pr_n =
              match event with
              | PullRequest e -> e.pr_number
              | PrReviewComment e -> e.pr_number
              | IssueComment e -> e.issue_number
              | PullRequestReview e -> e.pr_number
              | CheckRun _ | CheckSuite _ | WorkflowRun _ | Ignored -> 0
            in
            Buffer.add_string buf
              (Printf.sprintf
                 "\nTo inspect the full diff: `gh pr diff %d --repo %s/%s`\n"
                 pr_n owner2 repo2)
          end;
          let preamble = Buffer.contents buf in
          Some (user_message, preamble))
        else find_clawq rest
  in
  find_clawq lines
