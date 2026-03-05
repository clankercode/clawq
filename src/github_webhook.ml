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
}

type parsed_event =
  | PullRequest of pr_event
  | IssueComment of issue_comment_event
  | PrReviewComment of pr_review_comment_event
  | Ignored

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
        | "opened" | "edited" | "reopened" ->
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
  | Ignored -> "github:unknown"

let repo_of_event event =
  match event with
  | PullRequest e -> (e.owner, e.repo)
  | IssueComment e -> (e.owner, e.repo)
  | PrReviewComment e -> (e.owner, e.repo)
  | Ignored -> ("", "")

let author_of_event event =
  match event with
  | PullRequest e -> e.pr_author
  | IssueComment e -> e.comment_author
  | PrReviewComment e -> e.comment_author
  | Ignored -> ""

let event_type_string event =
  match event with
  | PullRequest _ -> "pull_request"
  | IssueComment _ -> "issue_comment"
  | PrReviewComment _ -> "pull_request_review_comment"
  | Ignored -> "ignored"

let extract_clawq ~event ~pr_files =
  let text =
    match event with
    | PullRequest e -> e.pr_body
    | IssueComment e -> e.comment_body
    | PrReviewComment e -> e.comment_body
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
              | Ignored -> 0
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
