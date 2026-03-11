let default_github_api_base = "https://api.github.com"

let github_api_base () =
  match Sys.getenv_opt "CLAWQ_GITHUB_API_BASE" with
  | Some base when String.trim base <> "" -> String.trim base
  | _ -> default_github_api_base

let redact_token token =
  let len = String.length token in
  if len <= 8 then "..." else String.sub token 0 8 ^ "..."

let auth_headers (auth : Runtime_config.github_auth) =
  match auth with
  | GithubPat token ->
      Logs.debug (fun m -> m "GitHub auth: PAT %s" (redact_token token));
      [
        ("Authorization", "Bearer " ^ token);
        ("Accept", "application/vnd.github+json");
        ("X-GitHub-Api-Version", "2022-11-28");
      ]

let post_comment ~auth ~owner ~repo ~issue_number ~body =
  let open Lwt.Syntax in
  let uri =
    Printf.sprintf "%s/repos/%s/%s/issues/%d/comments" (github_api_base ())
      owner repo issue_number
  in
  let headers = auth_headers auth in
  let req_body = `Assoc [ ("body", `String body) ] |> Yojson.Safe.to_string in
  let* status, _body = Http_client.post_json ~uri ~headers ~body:req_body in
  if status < 200 || status >= 300 then
    Logs.warn (fun m ->
        m "GitHub API post_comment %s/%s#%d returned %d" owner repo issue_number
          status);
  Lwt.return_unit

let reply_to_review_comment ~auth ~owner ~repo ~pull_number ~comment_id ~body =
  let open Lwt.Syntax in
  let uri =
    Printf.sprintf "%s/repos/%s/%s/pulls/%d/comments/%d/replies"
      (github_api_base ()) owner repo pull_number comment_id
  in
  let headers = auth_headers auth in
  let req_body = `Assoc [ ("body", `String body) ] |> Yojson.Safe.to_string in
  let* status, _body = Http_client.post_json ~uri ~headers ~body:req_body in
  if status < 200 || status >= 300 then
    Logs.warn (fun m ->
        m "GitHub API reply_to_review_comment %s/%s#%d comment=%d returned %d"
          owner repo pull_number comment_id status);
  Lwt.return_unit

let get_pr_files ~auth ~owner ~repo ~pull_number =
  let open Lwt.Syntax in
  let headers = auth_headers auth in
  let rec fetch_page page acc =
    if page > 3 then Lwt.return (List.rev acc)
    else
      let uri =
        Printf.sprintf "%s/repos/%s/%s/pulls/%d/files?per_page=100&page=%d"
          (github_api_base ()) owner repo pull_number page
      in
      let* status, body = Http_client.get ~uri ~headers in
      if status <> 200 then begin
        Logs.warn (fun m ->
            m "GitHub API get_pr_files %s/%s#%d page=%d returned %d" owner repo
              pull_number page status);
        Lwt.return (List.rev acc)
      end
      else
        let files =
          try
            let json = Yojson.Safe.from_string body in
            let open Yojson.Safe.Util in
            json |> to_list
            |> List.map (fun f ->
                let filename =
                  try f |> member "filename" |> to_string with _ -> ""
                in
                let file_status =
                  try f |> member "status" |> to_string with _ -> ""
                in
                let additions =
                  try f |> member "additions" |> to_int with _ -> 0
                in
                let deletions =
                  try f |> member "deletions" |> to_int with _ -> 0
                in
                (filename, file_status, additions, deletions))
          with _ -> []
        in
        let acc = acc @ files in
        if List.length files < 100 then Lwt.return acc
        else fetch_page (page + 1) acc
  in
  fetch_page 1 []
