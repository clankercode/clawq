(* Disable warning 16 (unerasable-optional-argument) for this module. The
   optional [resolve_headers] parameter on API functions is intentionally
   placed before required labeled arguments — it is always explicitly passed
   by callers that need lease-based auth, and defaults to [None] otherwise. *)
[@@@warning "-16"]

let default_github_api_base = "https://api.github.com"

let github_api_base () =
  match Sys.getenv_opt "CLAWQ_GITHUB_API_BASE" with
  | Some base when String.trim base <> "" -> String.trim base
  | _ -> default_github_api_base

let redact_token = String_util.redact_token

let pat_headers token =
  [
    ("Authorization", "Bearer " ^ token);
    ("Accept", "application/vnd.github+json");
    ("X-GitHub-Api-Version", "2022-11-28");
  ]

let auth_headers (auth : Runtime_config.github_auth) =
  match auth with
  | GithubPat token ->
      Logs.debug (fun m -> m "GitHub auth: PAT %s" (redact_token token));
      [
        ("Authorization", "Bearer " ^ token);
        ("Accept", "application/vnd.github+json");
        ("X-GitHub-Api-Version", "2022-11-28");
      ]
  | GithubApp _app ->
      (* GitHub App auth requires JWT signing to generate installation access
         tokens. This is not yet implemented. Raise an explicit error rather than
         making unauthenticated API calls that would silently fail with 401. *)
      Logs.err (fun m ->
          m
            "GitHub App auth: API calls not yet supported. Install a PAT or \
             implement JWT token generation.");
      failwith "GitHub App auth not yet implemented for outbound API calls"

(* Asynchronous auth headers that support both PAT and GitHub App auth.
   [app_token] is required when [auth = GithubApp _].
   [repo_full_name] is used to look up the correct installation. *)
let auth_headers_lwt ~(app_token : Github_app_token.t option)
    ?(repo_full_name = "")
    ?(egress_rules = ([] : Runtime_config_types.egress_rule list))
    ?(egress_audit = Policy_http_client.no_audit)
    (auth : Runtime_config.github_auth) =
  let open Lwt.Syntax in
  match auth with
  | GithubPat token ->
      Lwt.return
        [
          ("Authorization", "Bearer " ^ token);
          ("Accept", "application/vnd.github+json");
          ("X-GitHub-Api-Version", "2022-11-28");
        ]
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
                  ~egress_rules ~egress_audit ()
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

(* Resolve GitHub auth headers through the credential lease API when
   [github_config.auth_credential_handle] is set. The snapshot scopes which
   credential handles are allowed — missing or unauthorized handles are denied
   before any API call. Falls back to legacy [auth_headers_lwt] when no handle
   is configured. *)
let resolve_github_auth_headers ~(config : Runtime_config.t)
    ~(snapshot : Access_snapshot.t) ~(app_token : Github_app_token.t option)
    ?(repo_full_name = "")
    ?(egress_rules = ([] : Runtime_config_types.egress_rule list))
    ?(egress_audit = Policy_http_client.no_audit)
    (github_config : Runtime_config.github_config) :
    (string * string) list option Lwt.t =
  let open Lwt.Syntax in
  match github_config.auth_credential_handle with
  | None ->
      (* Legacy path: resolve auth directly from the config. Thread egress
         policy through this path too because GitHub App auth may need to mint
         an installation token before the API request itself is sent. *)
      let* headers =
        auth_headers_lwt ~app_token ~repo_full_name ~egress_rules ~egress_audit
          github_config.auth
      in
      Lwt.return (Some headers)
  | Some handle_id -> (
      match
        Credential_lease.resolve_snapshot_lease ~config ~snapshot ~handle_id
          ~header_name:"Authorization"
      with
      | Error err ->
          let msg = Credential_lease.resolution_error_to_string err in
          Logs.err (fun m ->
              m "GitHub auth: credential lease denied for handle '%s': %s"
                handle_id msg);
          Lwt.return None
      | Ok lease ->
          let raw_token =
            let result = ref "" in
            Credential_lease.apply_headers lease (fun headers ->
                result :=
                  List.fold_left
                    (fun acc (name, value) ->
                      if name = "Authorization" then value else acc)
                    "" headers);
            !result
          in
          if raw_token = "" then (
            Logs.err (fun m ->
                m
                  "GitHub auth: credential lease for handle '%s' resolved but \
                   produced no Authorization header"
                  handle_id);
            Lwt.return None)
          else
            let prefix =
              if
                String.length raw_token >= 7
                && String.sub raw_token 0 7 = "Bearer "
              then ""
              else "Bearer "
            in
            Lwt.return
              (Some
                 [
                   ("Authorization", prefix ^ raw_token);
                   ("Accept", "application/vnd.github+json");
                   ("X-GitHub-Api-Version", "2022-11-28");
                 ]))

(* Type for pre-resolved header functions. Callers that use the credential
   lease system pass this to override the legacy auth path. *)

type resolve_headers_fn = string -> (string * string) list option Lwt.t

(* Policy-aware HTTP helpers. When [rules] is non-empty, requests go through
   {!Policy_http_client} for egress policy evaluation. When [rules] is empty,
   requests bypass policy (backward-compatible default for callers that don't
   pass egress rules). *)
let maybe_post_json ~rules ~uri ~headers ~body ?audit () =
  match rules with
  | [] ->
      let open Lwt.Syntax in
      let* r = Http_client.post_json ~uri ~headers ~body in
      Lwt.return (Ok r)
  | _ -> Policy_http_client.post_json ~rules ~uri ~headers ~body ?audit ()

let maybe_get ~rules ~uri ~headers ?audit () =
  match rules with
  | [] ->
      let open Lwt.Syntax in
      let* r = Http_client.get ~uri ~headers in
      Lwt.return (Ok r)
  | _ -> Policy_http_client.get ~rules ~uri ~headers ?audit ()

let maybe_patch_json ~rules ~uri ~headers ~body ?audit () =
  match rules with
  | [] ->
      let open Lwt.Syntax in
      let* r = Http_client.patch_json ~uri ~headers ~body in
      Lwt.return (Ok r)
  | _ -> Policy_http_client.patch_json ~rules ~uri ~headers ~body ?audit ()

let get_auth_headers ~(app_token : Github_app_token.t option)
    ~(auth : Runtime_config.github_auth)
    ?(resolve_headers = (None : resolve_headers_fn option))
    ?(egress_rules = ([] : Runtime_config_types.egress_rule list))
    ?(egress_audit = Policy_http_client.no_audit) ~repo_full_name () =
  match resolve_headers with
  | Some f -> f repo_full_name
  | None ->
      let open Lwt.Syntax in
      let* headers =
        auth_headers_lwt ~app_token ~repo_full_name ~egress_rules ~egress_audit
          auth
      in
      Lwt.return (Some headers)

let post_comment ~(app_token : Github_app_token.t option) ~auth
    ?(resolve_headers = (None : resolve_headers_fn option))
    ?(egress_rules = ([] : Runtime_config_types.egress_rule list))
    ?(egress_audit = Policy_http_client.no_audit) ~owner ~repo ~issue_number
    ~body () =
  let open Lwt.Syntax in
  let repo_full_name = owner ^ "/" ^ repo in
  let uri =
    Printf.sprintf "%s/repos/%s/%s/issues/%d/comments" (github_api_base ())
      owner repo issue_number
  in
  let* headers =
    get_auth_headers ~app_token ~auth ~resolve_headers ~egress_rules
      ~egress_audit ~repo_full_name ()
  in
  match headers with
  | None ->
      Logs.err (fun m ->
          m "GitHub API post_comment: credential lease denied, aborting");
      Lwt.return_unit
  | Some headers ->
      let req_body =
        `Assoc [ ("body", `String body) ] |> Yojson.Safe.to_string
      in
      let* result =
        maybe_post_json ~rules:egress_rules ~uri ~headers ~body:req_body
          ~audit:egress_audit ()
      in
      (match result with
      | Error err ->
          Logs.warn (fun m ->
              m "GitHub API post_comment %s/%s#%d: egress denied: %s" owner repo
                issue_number
                (Policy_http_client.policy_error_to_string err))
      | Ok (status, _body) ->
          if status < 200 || status >= 300 then
            Logs.warn (fun m ->
                m "GitHub API post_comment %s/%s#%d returned %d" owner repo
                  issue_number status));
      Lwt.return_unit

let reply_to_review_comment ~(app_token : Github_app_token.t option) ~auth
    ?(resolve_headers = (None : resolve_headers_fn option))
    ?(egress_rules = ([] : Runtime_config_types.egress_rule list))
    ?(egress_audit = Policy_http_client.no_audit) ~owner ~repo ~pull_number
    ~comment_id ~body () =
  let open Lwt.Syntax in
  let repo_full_name = owner ^ "/" ^ repo in
  let uri =
    Printf.sprintf "%s/repos/%s/%s/pulls/%d/comments/%d/replies"
      (github_api_base ()) owner repo pull_number comment_id
  in
  let* headers =
    get_auth_headers ~app_token ~auth ~resolve_headers ~egress_rules
      ~egress_audit ~repo_full_name ()
  in
  match headers with
  | None ->
      Logs.err (fun m ->
          m
            "GitHub API reply_to_review_comment: credential lease denied, \
             aborting");
      Lwt.return_unit
  | Some headers ->
      let req_body =
        `Assoc [ ("body", `String body) ] |> Yojson.Safe.to_string
      in
      let* result =
        maybe_post_json ~rules:egress_rules ~uri ~headers ~body:req_body
          ~audit:egress_audit ()
      in
      (match result with
      | Error err ->
          Logs.warn (fun m ->
              m
                "GitHub API reply_to_review_comment %s/%s#%d comment=%d: \
                 egress denied: %s"
                owner repo pull_number comment_id
                (Policy_http_client.policy_error_to_string err))
      | Ok (status, _body) ->
          if status < 200 || status >= 300 then
            Logs.warn (fun m ->
                m
                  "GitHub API reply_to_review_comment %s/%s#%d comment=%d \
                   returned %d"
                  owner repo pull_number comment_id status));
      Lwt.return_unit

let add_reaction ~(app_token : Github_app_token.t option) ~auth
    ?(resolve_headers = (None : resolve_headers_fn option))
    ?(egress_rules = ([] : Runtime_config_types.egress_rule list))
    ?(egress_audit = Policy_http_client.no_audit) ~owner ~repo ~comment_id
    ~content ~(comment_type : [ `Issue | `Review ]) () =
  let open Lwt.Syntax in
  let repo_full_name = owner ^ "/" ^ repo in
  let segment =
    match comment_type with `Issue -> "issues" | `Review -> "pulls"
  in
  let uri =
    Printf.sprintf "%s/repos/%s/%s/%s/comments/%d/reactions"
      (github_api_base ()) owner repo segment comment_id
  in
  let* headers =
    get_auth_headers ~app_token ~auth ~resolve_headers ~egress_rules
      ~egress_audit ~repo_full_name ()
  in
  match headers with
  | None ->
      Logs.err (fun m ->
          m "GitHub API add_reaction: credential lease denied, aborting");
      Lwt.return_unit
  | Some headers ->
      let req_body =
        `Assoc [ ("content", `String content) ] |> Yojson.Safe.to_string
      in
      let* result =
        maybe_post_json ~rules:egress_rules ~uri ~headers ~body:req_body
          ~audit:egress_audit ()
      in
      (match result with
      | Error err ->
          Logs.warn (fun m ->
              m "GitHub API add_reaction %s/%s comment=%d: egress denied: %s"
                owner repo comment_id
                (Policy_http_client.policy_error_to_string err))
      | Ok (status, _body) ->
          if status < 200 || status >= 300 then
            Logs.warn (fun m ->
                m "GitHub API add_reaction %s/%s comment=%d returned %d" owner
                  repo comment_id status));
      Lwt.return_unit

let post_comment_returning_id ~(app_token : Github_app_token.t option) ~auth
    ?(resolve_headers = (None : resolve_headers_fn option))
    ?(egress_rules = ([] : Runtime_config_types.egress_rule list))
    ?(egress_audit = Policy_http_client.no_audit) ~owner ~repo ~issue_number
    ~body () =
  let open Lwt.Syntax in
  let repo_full_name = owner ^ "/" ^ repo in
  let uri =
    Printf.sprintf "%s/repos/%s/%s/issues/%d/comments" (github_api_base ())
      owner repo issue_number
  in
  let* headers =
    get_auth_headers ~app_token ~auth ~resolve_headers ~egress_rules
      ~egress_audit ~repo_full_name ()
  in
  match headers with
  | None ->
      Logs.err (fun m ->
          m
            "GitHub API post_comment_returning_id: credential lease denied, \
             aborting");
      Lwt.return None
  | Some headers -> (
      let req_body =
        `Assoc [ ("body", `String body) ] |> Yojson.Safe.to_string
      in
      let* result =
        maybe_post_json ~rules:egress_rules ~uri ~headers ~body:req_body
          ~audit:egress_audit ()
      in
      match result with
      | Error err ->
          Logs.warn (fun m ->
              m
                "GitHub API post_comment_returning_id %s/%s#%d: egress denied: \
                 %s"
                owner repo issue_number
                (Policy_http_client.policy_error_to_string err));
          Lwt.return None
      | Ok (status, resp_body) ->
          if status < 200 || status >= 300 then begin
            Logs.warn (fun m ->
                m "GitHub API post_comment_returning_id %s/%s#%d returned %d"
                  owner repo issue_number status);
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
            Lwt.return id)

let edit_comment ~(app_token : Github_app_token.t option) ~auth
    ?(resolve_headers = (None : resolve_headers_fn option))
    ?(egress_rules = ([] : Runtime_config_types.egress_rule list))
    ?(egress_audit = Policy_http_client.no_audit) ~owner ~repo ~comment_id ~body
    () =
  let open Lwt.Syntax in
  let repo_full_name = owner ^ "/" ^ repo in
  let uri =
    Printf.sprintf "%s/repos/%s/%s/issues/comments/%d" (github_api_base ())
      owner repo comment_id
  in
  let* headers =
    get_auth_headers ~app_token ~auth ~resolve_headers ~egress_rules
      ~egress_audit ~repo_full_name ()
  in
  match headers with
  | None ->
      Logs.err (fun m ->
          m "GitHub API edit_comment: credential lease denied, aborting");
      Lwt.return_unit
  | Some headers ->
      let req_body =
        `Assoc [ ("body", `String body) ] |> Yojson.Safe.to_string
      in
      let* result =
        maybe_patch_json ~rules:egress_rules ~uri ~headers ~body:req_body
          ~audit:egress_audit ()
      in
      (match result with
      | Error err ->
          Logs.warn (fun m ->
              m "GitHub API edit_comment %s/%s comment=%d: egress denied: %s"
                owner repo comment_id
                (Policy_http_client.policy_error_to_string err))
      | Ok (status, _body) ->
          if status < 200 || status >= 300 then
            Logs.warn (fun m ->
                m "GitHub API edit_comment %s/%s comment=%d returned %d" owner
                  repo comment_id status));
      Lwt.return_unit

let get_pr_files ~(app_token : Github_app_token.t option) ~auth
    ?(resolve_headers = (None : resolve_headers_fn option))
    ?(egress_rules = ([] : Runtime_config_types.egress_rule list))
    ?(egress_audit = Policy_http_client.no_audit) ~owner ~repo ~pull_number () =
  let open Lwt.Syntax in
  let repo_full_name = owner ^ "/" ^ repo in
  let* headers =
    get_auth_headers ~app_token ~auth ~resolve_headers ~egress_rules
      ~egress_audit ~repo_full_name ()
  in
  match headers with
  | None ->
      Logs.err (fun m ->
          m "GitHub API get_pr_files: credential lease denied, aborting");
      Lwt.return []
  | Some headers ->
      let rec fetch_page page acc =
        if page > 3 then Lwt.return (List.rev acc)
        else
          let uri =
            Printf.sprintf "%s/repos/%s/%s/pulls/%d/files?per_page=100&page=%d"
              (github_api_base ()) owner repo pull_number page
          in
          let* result =
            maybe_get ~rules:egress_rules ~uri ~headers ~audit:egress_audit ()
          in
          match result with
          | Error err ->
              Logs.warn (fun m ->
                  m
                    "GitHub API get_pr_files %s/%s#%d page=%d: egress denied: \
                     %s"
                    owner repo pull_number page
                    (Policy_http_client.policy_error_to_string err));
              Lwt.return (List.rev acc)
          | Ok (status, body) ->
              if status <> 200 then begin
                Logs.warn (fun m ->
                    m "GitHub API get_pr_files %s/%s#%d page=%d returned %d"
                      owner repo pull_number page status);
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
