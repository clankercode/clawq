[@@@warning "-16"]

type webhook_result = Ok of string | BadSignature

let bot_reply_marker = "<!-- clawq-reply -->"

let format_reply ~command ~response =
  let base =
    if command = "" then response
    else Printf.sprintf "> /clawq %s\n\n%s" command response
  in
  base ^ "\n" ^ bot_reply_marker

let is_bot_reply text =
  try
    ignore (Str.search_forward (Str.regexp_string bot_reply_marker) text 0);
    true
  with Not_found -> false

let dedup = Channel_util.Lru_dedup.create 500

let is_event_allowed ~(repo_config : Runtime_config.github_repo_config)
    ~event_type =
  repo_config.react_to = [] || List.mem event_type repo_config.react_to

let is_user_allowed ~(repo_config : Runtime_config.github_repo_config) ~sender =
  match repo_config.allow_users with
  | [ "*" ] -> true
  | users -> sender <> "" && List.mem sender users

let split_repo_full_name repo_full_name =
  match String.split_on_char '/' repo_full_name with
  | [ owner; repo ] when owner <> "" && repo <> "" -> Some (owner, repo)
  | _ -> None

let fetch_pr_files ~(repo_config : Runtime_config.github_repo_config)
    ~(github_config : Runtime_config.github_config)
    ?(resolve_headers = (None : Github_api.resolve_headers_fn option))
    ?(egress_rules = ([] : Runtime_config_types.egress_rule list))
    ?(egress_audit = Policy_http_client.no_audit) ~api_limiter ~owner ~repo
    event =
  let open Lwt.Syntax in
  if not repo_config.include_pr_files then Lwt.return []
  else
    let pr_n =
      match event with
      | Github_webhook.PullRequest e -> e.pr_number
      | Github_webhook.PrReviewComment e -> e.pr_number
      | Github_webhook.IssueComment e -> if e.is_pr then e.issue_number else 0
      | Github_webhook.PullRequestReview e -> e.pr_number
      | Github_webhook.CheckRun _ | Github_webhook.CheckSuite _
      | Github_webhook.WorkflowRun _ | Github_webhook.Ignored ->
          0
    in
    if pr_n <= 0 then Lwt.return []
    else
      Lwt.catch
        (fun () ->
          let* _ok =
            Rate_limiter.check_and_consume api_limiter
              ~key:(Printf.sprintf "github:%s/%s" owner repo)
          in
          Github_api.get_pr_files
            ~app_token:(Github_app_token.resolve_app_token ())
            ~auth:github_config.auth ~resolve_headers ~egress_rules
            ~egress_audit ~owner ~repo ~pull_number:pr_n ())
        (fun exn ->
          Logs.warn (fun m ->
              m "GitHub: failed to fetch PR files: %s" (Printexc.to_string exn));
          Lwt.return [])

let comment_body_of_event = function
  | Github_webhook.IssueComment e -> Some e.comment_body
  | Github_webhook.PrReviewComment e -> Some e.comment_body
  | Github_webhook.PullRequestReview e -> Some e.body
  | Github_webhook.PullRequest _ | Github_webhook.CheckRun _
  | Github_webhook.CheckSuite _ | Github_webhook.WorkflowRun _
  | Github_webhook.Ignored ->
      None

let acknowledge_reaction ~(github_config : Runtime_config.github_config)
    ?(resolve_headers = (None : Github_api.resolve_headers_fn option))
    ?(egress_rules = ([] : Runtime_config_types.egress_rule list))
    ?(egress_audit = Policy_http_client.no_audit) ~api_limiter ~owner ~repo
    event =
  Lwt.catch
    (fun () ->
      let open Lwt.Syntax in
      let* _ok =
        Rate_limiter.check_and_consume api_limiter
          ~key:(Printf.sprintf "github:%s/%s" owner repo)
      in
      match event with
      | Github_webhook.IssueComment e ->
          Github_api.add_reaction
            ~app_token:(Github_app_token.resolve_app_token ())
            ~auth:github_config.auth ~resolve_headers ~egress_rules
            ~egress_audit ~owner ~repo ~comment_id:e.comment_id ~content:"eyes"
            ~comment_type:`Issue ()
      | Github_webhook.PrReviewComment e ->
          Github_api.add_reaction
            ~app_token:(Github_app_token.resolve_app_token ())
            ~auth:github_config.auth ~resolve_headers ~egress_rules
            ~egress_audit ~owner ~repo ~comment_id:e.comment_id ~content:"eyes"
            ~comment_type:`Review ()
      | _ -> Lwt.return_unit)
    (fun exn ->
      Logs.warn (fun m ->
          m "GitHub: failed to add reaction: %s" (Printexc.to_string exn));
      Lwt.return_unit)

let issue_number_of_event = function
  | Github_webhook.PullRequest e -> e.pr_number
  | Github_webhook.IssueComment e -> e.issue_number
  | Github_webhook.PrReviewComment e -> e.pr_number
  | Github_webhook.PullRequestReview e -> e.pr_number
  | Github_webhook.CheckRun _ | Github_webhook.CheckSuite _
  | Github_webhook.WorkflowRun _ | Github_webhook.Ignored ->
      0

let post_reply ~(github_config : Runtime_config.github_config)
    ?(resolve_headers = (None : Github_api.resolve_headers_fn option))
    ?(egress_rules = ([] : Runtime_config_types.egress_rule list))
    ?(egress_audit = Policy_http_client.no_audit) ~api_limiter ~owner ~repo
    ~placeholder_id event ~reply_text =
  let open Lwt.Syntax in
  Lwt.catch
    (fun () ->
      let* _ok =
        Rate_limiter.check_and_consume api_limiter
          ~key:(Printf.sprintf "github:%s/%s" owner repo)
      in
      match placeholder_id with
      | Some cid ->
          Github_api.edit_comment
            ~app_token:(Github_app_token.resolve_app_token ())
            ~auth:github_config.auth ~resolve_headers ~egress_rules
            ~egress_audit ~owner ~repo ~comment_id:cid ~body:reply_text ()
      | None -> (
          match event with
          | Github_webhook.PrReviewComment e ->
              Github_api.reply_to_review_comment
                ~app_token:(Github_app_token.resolve_app_token ())
                ~auth:github_config.auth ~resolve_headers ~egress_rules
                ~egress_audit ~owner ~repo ~pull_number:e.pr_number
                ~comment_id:e.comment_id ~body:reply_text ()
          | Github_webhook.PullRequest e ->
              Github_api.post_comment
                ~app_token:(Github_app_token.resolve_app_token ())
                ~auth:github_config.auth ~resolve_headers ~egress_rules
                ~egress_audit ~owner ~repo ~issue_number:e.pr_number
                ~body:reply_text ()
          | Github_webhook.IssueComment e ->
              Github_api.post_comment
                ~app_token:(Github_app_token.resolve_app_token ())
                ~auth:github_config.auth ~resolve_headers ~egress_rules
                ~egress_audit ~owner ~repo ~issue_number:e.issue_number
                ~body:reply_text ()
          | Github_webhook.PullRequestReview e ->
              Github_api.post_comment
                ~app_token:(Github_app_token.resolve_app_token ())
                ~auth:github_config.auth ~resolve_headers ~egress_rules
                ~egress_audit ~owner ~repo ~issue_number:e.pr_number
                ~body:reply_text ()
          | Github_webhook.CheckRun _ | Github_webhook.CheckSuite _
          | Github_webhook.WorkflowRun _ | Github_webhook.Ignored ->
              Lwt.return_unit))
    (fun exn ->
      Logs.err (fun m ->
          m "GitHub: failed to post reply: %s" (Printexc.to_string exn));
      Lwt.return_unit)

let run_clawq_command ~(github_config : Runtime_config.github_config)
    ?(resolve_headers = (None : Github_api.resolve_headers_fn option))
    ?(egress_rules = ([] : Runtime_config_types.egress_rule list))
    ?(egress_audit = Policy_http_client.no_audit) ~(session_manager : Session.t)
    ~api_limiter ~owner ~repo ~author event ~user_message ~preamble =
  let open Lwt.Syntax in
  let key = Github_webhook.session_key event in
  let full_message = preamble ^ "\n\n" ^ user_message in
  let gh_channel_name = "github:" ^ owner ^ "/" ^ repo in
  (* Acknowledge with eyes reaction *)
  let* () =
    acknowledge_reaction ~github_config ~resolve_headers ~egress_rules
      ~egress_audit ~api_limiter ~owner ~repo event
  in
  (* Post placeholder comment for non-review-comment events *)
  let* placeholder_id =
    match event with
    | Github_webhook.PrReviewComment _ -> Lwt.return None
    | _ ->
        let issue_n = issue_number_of_event event in
        if issue_n > 0 then
          Lwt.catch
            (fun () ->
              let* _ok =
                Rate_limiter.check_and_consume api_limiter
                  ~key:(Printf.sprintf "github:%s/%s" owner repo)
              in
              let placeholder =
                Printf.sprintf
                  "> /clawq %s\n\n\xE2\x8F\xB3 Working on it...\n%s"
                  user_message bot_reply_marker
              in
              Github_api.post_comment_returning_id
                ~app_token:(Github_app_token.resolve_app_token ())
                ~auth:github_config.auth ~resolve_headers ~egress_rules
                ~egress_audit ~owner ~repo ~issue_number:issue_n
                ~body:placeholder ())
            (fun exn ->
              Logs.warn (fun m ->
                  m "GitHub: failed to post placeholder: %s"
                    (Printexc.to_string exn));
              Lwt.return None)
        else Lwt.return None
  in
  (* Register channel notifier so autonomous/deferred responses reach GitHub *)
  if Option.is_none (Session.find_registered_notifier session_manager ~key) then
    Session.register_channel_notifier session_manager ~key (fun text ->
        let body = text ^ "\n" ^ bot_reply_marker in
        let issue_n = issue_number_of_event event in
        if issue_n > 0 then
          Lwt.catch
            (fun () ->
              Github_api.post_comment
                ~app_token:(Github_app_token.resolve_app_token ())
                ~auth:github_config.auth ~resolve_headers ~egress_rules
                ~egress_audit ~owner ~repo ~issue_number:issue_n ~body ())
            (fun exn ->
              Logs.err (fun m ->
                  m "GitHub: channel notifier failed: %s"
                    (Printexc.to_string exn));
              Lwt.return_unit)
        else Lwt.return_unit);
  (* Run agent turn *)
  Session.register_connector_capabilities session_manager ~key
    Connector_capabilities.github;
  let* result =
    Lwt.catch
      (fun () ->
        let* response =
          Session.turn session_manager ~key ~message:full_message
            ~channel_name:gh_channel_name ~channel_type:"dm" ~sender_id:author
            ~channel:"github"
            ~channel_id:(owner ^ "/" ^ repo)
            ~snapshot_work_type:Access_snapshot.GitHub_trigger ()
        in
        Lwt.return (Result.Ok response))
      (fun exn -> Lwt.return (Result.Error (Printexc.to_string exn)))
  in
  let reply_text, log_result =
    match result with
    | Result.Ok response ->
        (format_reply ~command:user_message ~response, "replied")
    | Result.Error err ->
        Logs.err (fun m -> m "GitHub: agent error for %s/%s: %s" owner repo err);
        ( Printf.sprintf "Sorry, an error occurred: %s\n%s" err bot_reply_marker,
          "error commented" )
  in
  (* Edit placeholder or post new comment with final response *)
  let* () =
    post_reply ~github_config ~resolve_headers ~egress_rules ~egress_audit
      ~api_limiter ~owner ~repo ~placeholder_id event ~reply_text
  in
  Logs.info (fun m ->
      m "GitHub: %s/%s %s by @%s -> %s" owner repo
        (Github_webhook.event_type_string event)
        author log_result);
  Lwt.return (Ok log_result)

(** Detect and trigger review runs from label events. When a PR is labeled with
    a review-trigger label (e.g., "review", "security"), creates a review run
    record. Returns the number of review runs triggered. *)
let trigger_review_runs_from_labels ~(db : Sqlite3.db) ~event_type ~body =
  if event_type <> "pull_request" then 0
  else
    try
      let json = Yojson.Safe.from_string body in
      let open Yojson.Safe.Util in
      let action = json |> member "action" |> to_string in
      if action <> "labeled" then 0
      else
        let label = json |> member "label" in
        let label_name = label |> member "name" |> to_string in
        let pr = json |> member "pull_request" in
        let pr_number = pr |> member "number" |> to_int in
        let repo_full_name =
          json |> member "repository" |> member "full_name" |> to_string
        in
        let head_sha = pr |> member "head" |> member "sha" |> to_string in
        match
          Github_review_run.trigger_from_label ~db ~repo:repo_full_name
            ~pr_number ~head_sha ~label:label_name
        with
        | Some run ->
            Logs.info (fun m ->
                m "GitHub: triggered %s review run for %s PR #%d (label: %s)"
                  (Github_review_run.run_kind_to_string
                     run.Github_review_run.run_kind)
                  repo_full_name pr_number label_name);
            1
        | None -> 0
    with exn ->
      Logs.debug (fun m ->
          m "GitHub: label trigger detection failed: %s"
            (Printexc.to_string exn));
      0

let handle_webhook ~(repo_config : Runtime_config.github_repo_config)
    ~(github_config : Runtime_config.github_config)
    ?(config : Runtime_config.t option) ~(session_manager : Session.t)
    ~(api_limiter : Rate_limiter.t) ~event_type ~body ~headers =
  let open Lwt.Syntax in
  let signature_header =
    match Cohttp.Header.get headers "x-hub-signature-256" with
    | Some v -> v
    | None -> ""
  in
  if
    not
      (Github_webhook.verify_signature ~secret:repo_config.webhook_secret ~body
         ~signature_header)
  then Lwt.return BadSignature
  else
    (* Delivery deduplication *)
    let delivery_id =
      Cohttp.Header.get headers "x-github-delivery" |> Option.value ~default:""
    in
    if
      delivery_id <> ""
      && Channel_util.Lru_dedup.check_and_mark dedup delivery_id
    then begin
      Logs.info (fun m ->
          m "GitHub: ignoring duplicate delivery %s" delivery_id);
      Lwt.return (Ok "duplicate")
    end
    else
      let prepared =
        Github_hooks.prepare_event ~event_name:event_type ~headers
          ~raw_body:body
      in
      let payload_repo = String.lowercase_ascii prepared.repo_full_name in
      let configured_repo = String.lowercase_ascii repo_config.name in
      if payload_repo <> configured_repo then begin
        Logs.warn (fun m ->
            m
              "GitHub: ignoring webhook on %s because payload repo %S did not \
               match configured repo %S"
              repo_config.webhook_path prepared.repo_full_name repo_config.name);
        Lwt.return (Ok "repo mismatch")
      end
      else if not (is_event_allowed ~repo_config ~event_type) then
        Lwt.return (Ok "filtered")
      else
        (* Create an access snapshot for credential scoping. The snapshot
           captures which credential handles are allowed by the current access
           policy. GitHub API calls resolve credentials through this snapshot,
           denying missing or unauthorized handles before any API call.
           When config is not provided (e.g. in tests), skip lease-based auth. *)
        let resolve_headers, egress_rules, egress_audit =
          match config with
          | Some config ->
              let session_key = "github:" ^ prepared.repo_full_name in
              let snapshot =
                Access_snapshot.create ~config
                  ~work_type:Access_snapshot.GitHub_trigger ~session_key ()
              in
              let access =
                Runtime_config.resolve_effective_access config ~session_key ()
              in
              let egress_rules = access.egress_rules in
              let egress_audit =
                {
                  Policy_http_client.no_audit with
                  session_key = Some session_key;
                  snapshot_id = Some snapshot.Access_snapshot.id;
                  profile_id = snapshot.Access_snapshot.profile_id;
                  tool_name = Some "github";
                }
              in
              ( Some
                  (fun repo_full_name ->
                    Github_api.resolve_github_auth_headers ~config ~snapshot
                      ~app_token:(Github_app_token.resolve_app_token ())
                      ~repo_full_name ~egress_rules ~egress_audit github_config),
                egress_rules,
                egress_audit )
          | None -> (None, [], Policy_http_client.no_audit)
        in
        (* Installation identity check: for GitHub App auth, verify that the
           installation_id in the webhook payload matches a configured
           installation and that the repo is within its scope. This blocks
           uninstalled repos before PR file fetch, hook execution, or
           background launch. *)
        let installation_denied =
          match github_config.auth with
          | Runtime_config.GithubPat _ -> false
          | Runtime_config.GithubApp _ -> (
              match prepared.installation_id with
              | None ->
                  Logs.warn (fun m ->
                      m
                        "GitHub: denying webhook for %s — no installation_id \
                         in payload"
                        prepared.repo_full_name);
                  true
              | Some inst_id -> (
                  match Github_app_token.resolve_app_token () with
                  | None ->
                      Logs.warn (fun m ->
                          m
                            "GitHub: denying webhook for %s — no app token \
                             available"
                            prepared.repo_full_name);
                      true
                  | Some tok ->
                      if
                        Github_app_token.verify_installation tok
                          ~installation_id:inst_id
                          ~repo_full_name:prepared.repo_full_name
                      then false
                      else (
                        Logs.warn (fun m ->
                            m
                              "GitHub: denying webhook for %s — installation \
                               %d not authorized for this repo"
                              prepared.repo_full_name inst_id);
                        true)))
        in
        if installation_denied then
          Lwt.return (Ok "installation not authorized")
        else if prepared.is_user_generated then
          let sender = prepared.sender_login in
          if not (is_user_allowed ~repo_config ~sender) then begin
            Logs.info (fun m ->
                m "GitHub: ignoring event %s from unauthorized user @%s"
                  event_type sender);
            Lwt.return (Ok "user not allowed")
          end
          else
            let event = Github_webhook.parse_event ~event_type ~body in
            (* Bot self-loop protection: skip comments containing our reply marker *)
            let body_text = comment_body_of_event event in
            if Option.is_some body_text && is_bot_reply (Option.get body_text)
            then begin
              Logs.debug (fun m ->
                  m "GitHub: ignoring bot self-reply (delivery=%s)" delivery_id);
              Lwt.return (Ok "bot self-reply")
            end
            else
              match event with
              | Github_webhook.Ignored ->
                  let* hook_count =
                    Github_hooks.run_matching_hooks ~session_manager ~prepared
                      ~github_config:(Some github_config) ~resolve_headers
                      ~egress_rules ~egress_audit ~api_limiter ()
                  in
                  if hook_count > 0 then
                    Lwt.return (Ok (Printf.sprintf "hooked:%d" hook_count))
                  else Lwt.return (Ok "ignored")
              | _ -> (
                  (* Dispatch to subscribed rooms/threads *)
                  let* _dispatched =
                    match Session.get_db session_manager with
                    | Some db ->
                        Github_pr_dispatch.dispatch_to_subscriptions ~db ~event
                          ~delivery_id ~snapshot_id:egress_audit.snapshot_id
                          ~send_message:(fun ~room_id ~text () ->
                            (* Try to find a notifier for this room *)
                            match
                              Session.find_registered_notifier session_manager
                                ~key:room_id
                            with
                            | Some notifier -> notifier text
                            | None ->
                                Logs.debug (fun m ->
                                    m
                                      "GitHub PR dispatch: no notifier for \
                                       room %s"
                                      room_id);
                                Lwt.return_unit)
                          ()
                    | None -> Lwt.return 0
                  in
                  (* Trigger review runs from label events *)
                  let* _review_runs =
                    match Session.get_db session_manager with
                    | Some db ->
                        Lwt.return
                          (trigger_review_runs_from_labels ~db ~event_type ~body)
                    | None -> Lwt.return 0
                  in
                  let author =
                    let parsed = Github_webhook.author_of_event event in
                    if parsed <> "" then parsed else prepared.sender_login
                  in
                  let owner, repo =
                    match split_repo_full_name repo_config.name with
                    | Some parts -> parts
                    | None -> Github_webhook.repo_of_event event
                  in
                  let* pr_files =
                    fetch_pr_files ~repo_config ~github_config ~resolve_headers
                      ~egress_rules ~egress_audit ~api_limiter ~owner ~repo
                      event
                  in
                  match Github_webhook.extract_clawq ~event ~pr_files with
                  | Some (user_message, preamble) ->
                      run_clawq_command ~github_config ~resolve_headers
                        ~egress_rules ~egress_audit ~session_manager
                        ~api_limiter ~owner ~repo ~author event ~user_message
                        ~preamble
                  | None ->
                      let* hook_count =
                        Github_hooks.run_matching_hooks ~session_manager
                          ~prepared ~github_config:(Some github_config)
                          ~resolve_headers ~egress_rules ~egress_audit
                          ~api_limiter ()
                      in
                      if hook_count > 0 then
                        Lwt.return (Ok (Printf.sprintf "hooked:%d" hook_count))
                      else Lwt.return (Ok "no /clawq command"))
        else
          (* Non-user-generated events (check_run, workflow_run, etc.) *)
          let event = Github_webhook.parse_event ~event_type ~body in
          (* Dispatch to subscribed rooms/threads *)
          let* _dispatched =
            match Session.get_db session_manager with
            | Some db ->
                Github_pr_dispatch.dispatch_to_subscriptions ~db ~event
                  ~delivery_id ~snapshot_id:egress_audit.snapshot_id
                  ~send_message:(fun ~room_id ~text () ->
                    (* Try to find a notifier for this room *)
                    match
                      Session.find_registered_notifier session_manager
                        ~key:room_id
                    with
                    | Some notifier -> notifier text
                    | None ->
                        Logs.debug (fun m ->
                            m "GitHub PR dispatch: no notifier for room %s"
                              room_id);
                        Lwt.return_unit)
                  ()
            | None -> Lwt.return 0
          in
          let* hook_count =
            Github_hooks.run_matching_hooks ~session_manager ~prepared
              ~github_config:(Some github_config) ~resolve_headers ~egress_rules
              ~egress_audit ~api_limiter ()
          in
          if hook_count > 0 then
            Lwt.return (Ok (Printf.sprintf "hooked:%d" hook_count))
          else Lwt.return (Ok "ignored")
