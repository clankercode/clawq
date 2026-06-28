let default_github_api_base = "https://api.github.com"

let github_api_base () =
  match Sys.getenv_opt "CLAWQ_GITHUB_API_BASE" with
  | Some base when String.trim base <> "" -> String.trim base
  | _ -> default_github_api_base

let redact_token = String_util.redact_token

(* Synchronous headers for PAT auth (backward compat). *)
let pat_headers token =
  Logs.debug (fun m -> m "GitHub auth: PAT %s" (redact_token token));
  [
    ("Authorization", "Bearer " ^ token);
    ("Accept", "application/vnd.github+json");
    ("X-GitHub-Api-Version", "2022-11-28");
  ]

(* Asynchronous auth headers that support both PAT and GitHub App auth.
   [app_token] is required when [auth = GithubApp _].
   [repo_full_name] is used to look up the correct installation. *)
let auth_headers_lwt ~(app_token : Github_app_token.t option)
    ?(repo_full_name = "") (auth : Runtime_config.github_auth) =
  let open Lwt.Syntax in
  match auth with
  | GithubPat token -> Lwt.return (pat_headers token)
  | GithubApp _config -> (
      match app_token with
      | None ->
          Logs.err (fun m ->
              m
                "GitHub auth: GithubApp auth requested but no \
                 Github_app_token.t provided");
          Lwt.return []
      | Some tok -> (
          match
            Github_app_token.find_installation_for_repo tok ~repo_full_name
          with
          | None ->
              Logs.err (fun m ->
                  m "GitHub auth: no installation configured for repo %s"
                    repo_full_name);
              Lwt.return []
          | Some inst -> (
              let* result =
                Github_app_token.get_installation_token tok
                  ~installation_id:inst.installation_id ~repos:inst.repos
              in
              match result with
              | Ok token ->
                  Logs.debug (fun m ->
                      m "GitHub auth: App installation %d token=%s"
                        inst.installation_id (redact_token token));
                  Lwt.return
                    [
                      ("Authorization", "Bearer " ^ token);
                      ("Accept", "application/vnd.github+json");
                      ("X-GitHub-Api-Version", "2022-11-28");
                    ]
              | Error msg ->
                  Logs.err (fun m ->
                      m "GitHub auth: failed to get installation token: %s" msg);
                  Lwt.return [])))

(* Legacy synchronous auth_headers for PAT-only callers. *)
let auth_headers (auth : Runtime_config.github_auth) =
  match auth with GithubPat token -> pat_headers token | GithubApp _ -> []

let post_comment ~(app_token : Github_app_token.t option) ~auth ~owner ~repo
    ~issue_number ~body =
  let open Lwt.Syntax in
  let repo_full_name = owner ^ "/" ^ repo in
  let uri =
    Printf.sprintf "%s/repos/%s/%s/issues/%d/comments" (github_api_base ())
      owner repo issue_number
  in
  let* headers = auth_headers_lwt ~app_token ~repo_full_name auth in
  let req_body = `Assoc [ ("body", `String body) ] |> Yojson.Safe.to_string in
  let* status, _body = Http_client.post_json ~uri ~headers ~body:req_body in
  if status < 200 || status >= 300 then
    Logs.warn (fun m ->
        m "GitHub API post_comment %s/%s#%d returned %d" owner repo issue_number
          status);
  Lwt.return_unit

let reply_to_review_comment ~(app_token : Github_app_token.t option) ~auth
    ~owner ~repo ~pull_number ~comment_id ~body =
  let open Lwt.Syntax in
  let repo_full_name = owner ^ "/" ^ repo in
  let uri =
    Printf.sprintf "%s/repos/%s/%s/pulls/%d/comments/%d/replies"
      (github_api_base ()) owner repo pull_number comment_id
  in
  let* headers = auth_headers_lwt ~app_token ~repo_full_name auth in
  let req_body = `Assoc [ ("body", `String body) ] |> Yojson.Safe.to_string in
  let* status, _body = Http_client.post_json ~uri ~headers ~body:req_body in
  if status < 200 || status >= 300 then
    Logs.warn (fun m ->
        m "GitHub API reply_to_review_comment %s/%s#%d comment=%d returned %d"
          owner repo pull_number comment_id status);
  Lwt.return_unit

let add_reaction ~(app_token : Github_app_token.t option) ~auth ~owner ~repo
    ~comment_id ~content ~(comment_type : [ `Issue | `Review ]) =
  let open Lwt.Syntax in
  let repo_full_name = owner ^ "/" ^ repo in
  let segment =
    match comment_type with `Issue -> "issues" | `Review -> "pulls"
  in
  let uri =
    Printf.sprintf "%s/repos/%s/%s/%s/comments/%d/reactions"
      (github_api_base ()) owner repo segment comment_id
  in
  let* headers = auth_headers_lwt ~app_token ~repo_full_name auth in
  let req_body =
    `Assoc [ ("content", `String content) ] |> Yojson.Safe.to_string
  in
  let* status, _body = Http_client.post_json ~uri ~headers ~body:req_body in
  if status < 200 || status >= 300 then
    Logs.warn (fun m ->
        m "GitHub API add_reaction %s/%s comment=%d returned %d" owner repo
          comment_id status);
  Lwt.return_unit

let post_comment_returning_id ~(app_token : Github_app_token.t option) ~auth
    ~owner ~repo ~issue_number ~body =
  let open Lwt.Syntax in
  let repo_full_name = owner ^ "/" ^ repo in
  let uri =
    Printf.sprintf "%s/repos/%s/%s/issues/%d/comments" (github_api_base ())
      owner repo issue_number
  in
  let* headers = auth_headers_lwt ~app_token ~repo_full_name auth in
  let req_body = `Assoc [ ("body", `String body) ] |> Yojson.Safe.to_string in
  let* status, resp_body = Http_client.post_json ~uri ~headers ~body:req_body in
  if status < 200 || status >= 300 then begin
    Logs.warn (fun m ->
        m "GitHub API post_comment_returning_id %s/%s#%d returned %d" owner repo
          issue_number status);
    Lwt.return None
  end
  else
    let id =
      try
        Some
          (Yojson.Safe.from_string resp_body
          |> Yojson.Safe.Util.member "id"
          |> Yojson.Safe.Util.to_int)
      with _ -> None
    in
    Lwt.return id

let edit_comment ~(app_token : Github_app_token.t option) ~auth ~owner ~repo
    ~comment_id ~body =
  let open Lwt.Syntax in
  let repo_full_name = owner ^ "/" ^ repo in
  let uri =
    Printf.sprintf "%s/repos/%s/%s/issues/comments/%d" (github_api_base ())
      owner repo comment_id
  in
  let* headers = auth_headers_lwt ~app_token ~repo_full_name auth in
  let req_body = `Assoc [ ("body", `String body) ] |> Yojson.Safe.to_string in
  let* status, _body = Http_client.patch_json ~uri ~headers ~body:req_body in
  if status < 200 || status >= 300 then
    Logs.warn (fun m ->
        m "GitHub API edit_comment %s/%s comment=%d returned %d" owner repo
          comment_id status);
  Lwt.return_unit

let get_pr_files ~(app_token : Github_app_token.t option) ~auth ~owner ~repo
    ~pull_number =
  let open Lwt.Syntax in
  let repo_full_name = owner ^ "/" ^ repo in
  let* headers = auth_headers_lwt ~app_token ~repo_full_name auth in
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
