[@@@warning "-16"]

type webhook_result = Ok of string | BadSignature

let bot_reply_marker = "<!-- clawq-reply -->"

type provenance = {
  connector : string option;  (** e.g. "slack", "discord", "teams" *)
  room_id : string option;  (** Room/channel identifier *)
  room_name : string option;  (** Human-readable room name if available *)
  requester_id : string option;  (** User who triggered the request *)
  thread_id : string option;  (** Thread context if applicable *)
  task_id : int option;  (** Background task ID if applicable *)
}
(** Provenance context for bot-authored GitHub comments. Identifies where a
    request originated without leaking private room content. *)

let empty_provenance =
  {
    connector = None;
    room_id = None;
    room_name = None;
    requester_id = None;
    thread_id = None;
    task_id = None;
  }

(** [provenance_of_room_origin origin ?task_id ()] builds a provenance from a
    {!Room_origin.t}. The [room_id] is sanitized to avoid leaking internal IDs
    when no human-readable name is available. *)
let provenance_of_room_origin ?task_id (origin : Room_origin.t) () =
  {
    connector = origin.connector;
    room_id = origin.room_id;
    room_name = None;
    requester_id =
      (match origin.requester_name with
      | Some _ -> origin.requester_name
      | None -> origin.requester_id);
    thread_id = origin.thread_id;
    task_id;
  }

(** [connector_display_name connector] returns a human-readable connector name.
*)
let connector_display_name = function
  | Some "slack" -> "Slack"
  | Some "discord" -> "Discord"
  | Some "teams" -> "Teams"
  | Some "telegram" -> "Telegram"
  | Some "web" -> "Web"
  | Some "github" -> "GitHub"
  | Some s -> s
  | None -> "CLI"

(** [format_provenance_footer prov] formats a non-intrusive provenance footer
    for a GitHub comment. Returns [None] if provenance is empty. The footer
    includes a visible subtext line and an HTML comment with structured data for
    programmatic parsing. Private room content is never included.

    The HTML comment uses [<!-- clawq-provenance: ... -->] format and must
    appear before the [<!-- clawq-reply -->] self-loop marker. *)
let format_provenance_footer (prov : provenance) =
  if prov = empty_provenance then None
  else
    let visible_parts = ref [] in
    let json_fields = ref [] in
    Option.iter
      (fun c ->
        visible_parts := connector_display_name (Some c) :: !visible_parts;
        json_fields := ("connector", `String c) :: !json_fields)
      prov.connector;
    (match prov.room_name with
    | Some name ->
        visible_parts := Printf.sprintf "#%s" name :: !visible_parts;
        json_fields := ("room_name", `String name) :: !json_fields
    | None ->
        Option.iter
          (fun rid ->
            (* Include room_id in visible output for Slack-style channel IDs
               (C=channel, G=group, DM=D, or #-prefixed names). Internal IDs
               that don't match these patterns are JSON-only for privacy. *)
            if
              String.length rid > 0
              && (rid.[0] = '#'
                 || rid.[0] = 'C'
                 || rid.[0] = 'G'
                 || rid.[0] = 'D')
            then visible_parts := Printf.sprintf "#%s" rid :: !visible_parts;
            json_fields := ("room_id", `String rid) :: !json_fields)
          prov.room_id);
    Option.iter
      (fun r ->
        visible_parts := Printf.sprintf "by @%s" r :: !visible_parts;
        json_fields := ("requester", `String r) :: !json_fields)
      prov.requester_id;
    Option.iter
      (fun tid ->
        visible_parts := Printf.sprintf "Task #%d" tid :: !visible_parts;
        json_fields := ("task_id", `Int tid) :: !json_fields)
      prov.task_id;
    if !visible_parts = [] then None
    else
      let visible = String.concat " | " (List.rev !visible_parts) in
      let json_str = `Assoc (List.rev !json_fields) |> Yojson.Safe.to_string in
      Some
        (Printf.sprintf "\n---\n<sub>%s</sub>\n<!-- clawq-provenance: %s -->"
           visible json_str)

let format_reply ~command ~response =
  let base =
    if command = "" then response
    else Printf.sprintf "> /clawq %s\n\n%s" command response
  in
  base ^ "\n" ^ bot_reply_marker

(** [format_reply_with_provenance ~command ~response ~provenance] formats a
    reply with optional provenance footer. The provenance appears between the
    response and the self-loop marker. *)
let format_reply_with_provenance ~command ~response
    ~(provenance : provenance option) =
  let base =
    if command = "" then response
    else Printf.sprintf "> /clawq %s\n\n%s" command response
  in
  match provenance with
  | None -> base ^ "\n" ^ bot_reply_marker
  | Some prov -> (
      match format_provenance_footer prov with
      | None -> base ^ "\n" ^ bot_reply_marker
      | Some footer -> base ^ footer ^ "\n" ^ bot_reply_marker)

let is_bot_reply text =
  try
    ignore (Str.search_forward (Str.regexp_string bot_reply_marker) text 0);
    true
  with Not_found -> false

(** [is_provenance_comment text] checks if text contains a provenance HTML
    comment. *)
let is_provenance_comment text =
  try
    ignore
      (Str.search_forward (Str.regexp_string "<!-- clawq-provenance:") text 0);
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
          let* () =
            Github_api.edit_comment
              ~app_token:(Github_app_token.resolve_app_token ())
              ~auth:github_config.auth ~resolve_headers ~egress_rules
              ~egress_audit ~owner ~repo ~comment_id:cid ~body:reply_text ()
          in
          Lwt.return (Some (string_of_int cid))
      | None -> (
          match event with
          | Github_webhook.PrReviewComment e ->
              let* () =
                Github_api.reply_to_review_comment
                  ~app_token:(Github_app_token.resolve_app_token ())
                  ~auth:github_config.auth ~resolve_headers ~egress_rules
                  ~egress_audit ~owner ~repo ~pull_number:e.pr_number
                  ~comment_id:e.comment_id ~body:reply_text ()
              in
              (* reply_to_review_comment does not return the new comment ID *)
              Lwt.return None
          | Github_webhook.PullRequest e ->
              let* id =
                Github_api.post_comment_returning_id
                  ~app_token:(Github_app_token.resolve_app_token ())
                  ~auth:github_config.auth ~resolve_headers ~egress_rules
                  ~egress_audit ~owner ~repo ~issue_number:e.pr_number
                  ~body:reply_text ()
              in
              Lwt.return (Option.map string_of_int id)
          | Github_webhook.IssueComment e ->
              let* id =
                Github_api.post_comment_returning_id
                  ~app_token:(Github_app_token.resolve_app_token ())
                  ~auth:github_config.auth ~resolve_headers ~egress_rules
                  ~egress_audit ~owner ~repo ~issue_number:e.issue_number
                  ~body:reply_text ()
              in
              Lwt.return (Option.map string_of_int id)
          | Github_webhook.PullRequestReview e ->
              let* id =
                Github_api.post_comment_returning_id
                  ~app_token:(Github_app_token.resolve_app_token ())
                  ~auth:github_config.auth ~resolve_headers ~egress_rules
                  ~egress_audit ~owner ~repo ~issue_number:e.pr_number
                  ~body:reply_text ()
              in
              Lwt.return (Option.map string_of_int id)
          | Github_webhook.CheckRun _ | Github_webhook.CheckSuite _
          | Github_webhook.WorkflowRun _ | Github_webhook.Ignored ->
              Lwt.return None))
    (fun exn ->
      Logs.err (fun m ->
          m "GitHub: failed to post reply: %s" (Printexc.to_string exn));
      Lwt.return None)

let run_clawq_command ~(github_config : Runtime_config.github_config)
    ?(resolve_headers = (None : Github_api.resolve_headers_fn option))
    ?(egress_rules = ([] : Runtime_config_types.egress_rule list))
    ?(egress_audit = Policy_http_client.no_audit) ?(db : Sqlite3.db option)
    ~(session_manager : Session.t) ~api_limiter ~owner ~repo ~author event
    ~user_message ~preamble =
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
  let* posted_comment_id =
    post_reply ~github_config ~resolve_headers ~egress_rules ~egress_audit
      ~api_limiter ~owner ~repo ~placeholder_id event ~reply_text
  in
  (* Record provenance backlink if we have the comment ID and DB *)
  (match (posted_comment_id, db) with
  | Some comment_id, Some db ->
      let pr_number = issue_number_of_event event in
      if pr_number > 0 then
        let session_key = Github_webhook.session_key event in
        let repo_full_name = owner ^ "/" ^ repo in
        Room_github_backlinks.record_provenance_comment ~db ~repo:repo_full_name
          ~pr_number ~github_item_id:comment_id ~room_id:session_key ()
  | _ -> ());
  Logs.info (fun m ->
      m "GitHub: %s/%s %s by @%s -> %s" owner repo
        (Github_webhook.event_type_string event)
        author log_result);
  Lwt.return (Ok log_result)

(* B771: durable work-item execution path for /clawq, selected by leading
   "runner="/"host=" tokens. The envelope is persisted before any agent
   launch (duplicate deliveries collapse onto one item) and the final reply
   is published idempotently from the work-item record. *)

(* Best-effort final-message extraction from a runner log. Codex `exec
   --json` emits JSONL with agent messages; other runners print plain text.
   Falls back to the raw tail. *)
let extract_final_agent_message log_text =
  let lines = String.split_on_char '\n' log_text in
  let agent_text line =
    match Yojson.Safe.from_string (String.trim line) with
    | exception _ -> None
    | json -> (
        let open Yojson.Safe.Util in
        match member "item" json with
        | `Assoc _ as item -> (
            match (member "type" item, member "text" item) with
            | `String "agent_message", `String text when String.trim text <> ""
              ->
                Some text
            | _ | (exception _) -> None)
        | _ | (exception _) -> None)
  in
  let rec last_agent acc = function
    | [] -> acc
    | line :: rest ->
        last_agent
          (match agent_text line with Some t -> Some t | None -> acc)
          rest
  in
  match last_agent None lines with
  | Some text -> text
  | None -> String.trim log_text

let work_item_reply_limit = 8000

let work_item_prompt (item : Github_work_item.t) =
  String.concat "\n\n"
    [
      item.preamble;
      "## Request\n" ^ item.prompt;
      "## Execution contract\n\
       - Answer or plan only: do NOT create commits, branches, tags, or pull \
       requests.\n\
       - Your final message is posted verbatim to the GitHub thread; write it \
       for the requester.";
    ]

let publish_work_item_result ~(github_config : Runtime_config.github_config)
    ?(resolve_headers = (None : Github_api.resolve_headers_fn option))
    ?(egress_rules = ([] : Runtime_config_types.egress_rule list))
    ?(egress_audit = Policy_http_client.no_audit) ~api_limiter ~db
    (item : Github_work_item.t) =
  let open Lwt.Syntax in
  match Github_work_item.get ~db ~id:item.id with
  | None -> Lwt.return_unit
  | Some item when Github_work_item.already_published item -> Lwt.return_unit
  | Some item when not (Github_work_item.is_terminal_status item.status) ->
      Lwt.return_unit
  | Some item -> (
      match split_repo_full_name item.repo_full_name with
      | None -> Lwt.return_unit
      | Some (owner, repo) ->
          let body_text =
            let summary =
              Option.value item.result_summary ~default:"(no output captured)"
            in
            let summary =
              if String.length summary > work_item_reply_limit then
                String.sub summary 0 work_item_reply_limit ^ "\n... (truncated)"
              else summary
            in
            match item.status with
            | Github_work_item.Succeeded ->
                format_reply ~command:item.prompt ~response:summary
            | _ ->
                Printf.sprintf "> /clawq %s\n\nWork item %s: %s\n%s"
                  (String.sub item.prompt 0
                     (min 120 (String.length item.prompt)))
                  (Github_work_item.string_of_status item.status)
                  summary bot_reply_marker
          in
          let* posted =
            Lwt.catch
              (fun () ->
                let* _ok =
                  Rate_limiter.check_and_consume api_limiter
                    ~key:(Printf.sprintf "github:%s/%s" owner repo)
                in
                match item.ack_comment_id with
                | Some cid ->
                    let* () =
                      Github_api.edit_comment
                        ~app_token:(Github_app_token.resolve_app_token ())
                        ~auth:github_config.auth ~resolve_headers ~egress_rules
                        ~egress_audit ~owner ~repo ~comment_id:cid
                        ~body:body_text ()
                    in
                    Lwt.return (Result.Ok (Some cid))
                | None ->
                    let* id =
                      Github_api.post_comment_returning_id
                        ~app_token:(Github_app_token.resolve_app_token ())
                        ~auth:github_config.auth ~resolve_headers ~egress_rules
                        ~egress_audit ~owner ~repo
                        ~issue_number:item.issue_number ~body:body_text ()
                    in
                    Lwt.return (Result.Ok id))
              (fun exn -> Lwt.return (Result.Error (Printexc.to_string exn)))
          in
          (match posted with
          | Result.Ok comment_id ->
              ignore
                (Github_work_item.record_publication ~db ~id:item.id ~comment_id
                   ~publication_status:"published")
          | Result.Error msg ->
              ignore
                (Github_work_item.record_publication ~db ~id:item.id
                   ~comment_id:None ~publication_status:("failed: " ^ msg));
              Logs.err (fun m ->
                  m "GitHub work item %d: publication failed: %s" item.id msg));
          Lwt.return_unit)

(* Follow the owning background task to a terminal state, then record and
   publish the result. Crash-safe: recover_work_items redoes this after a
   daemon restart. *)
let watch_work_item ~(github_config : Runtime_config.github_config)
    ?(resolve_headers = (None : Github_api.resolve_headers_fn option))
    ?(egress_rules = ([] : Runtime_config_types.egress_rule list))
    ?(egress_audit = Policy_http_client.no_audit) ~api_limiter ~db
    (item : Github_work_item.t) ~task_id =
  let open Lwt.Syntax in
  let rec follow () =
    let* outcome =
      Background_task.wait_until_terminal ~timeout_seconds:3600.0
        ~poll_seconds:2.0 ~db ~id:task_id ()
    in
    match outcome with
    | Background_task.Finished task -> Lwt.return (Some task)
    | Background_task.Not_found -> Lwt.return None
    | Background_task.Timeout _ -> follow ()
    | Background_task.Interrupted task ->
        if Background_task.is_terminal_status task.status then
          Lwt.return (Some task)
        else follow ()
  in
  let* task = follow () in
  (match task with
  | None ->
      Github_work_item.record_result ~db ~id:item.id
        ~status:Github_work_item.Failed
        ~result_kind:Github_work_item.Result_failed
        ~result_summary:"Background task record disappeared"
  | Some task ->
      let summary =
        let log_text =
          match task.log_path with
          | Some path -> Background_task.read_log_tail path (64 * 1024)
          | None -> ""
        in
        let extracted = extract_final_agent_message log_text in
        if String.trim extracted <> "" then extracted
        else Option.value task.result_preview ~default:""
      in
      let status, kind =
        match task.status with
        | Background_task.Succeeded ->
            (Github_work_item.Succeeded, Github_work_item.Reply)
        | Background_task.Cancelled ->
            (Github_work_item.Cancelled, Github_work_item.Result_failed)
        | _ -> (Github_work_item.Failed, Github_work_item.Result_failed)
      in
      Github_work_item.record_result ~db ~id:item.id ~status ~result_kind:kind
        ~result_summary:summary);
  publish_work_item_result ~github_config ~resolve_headers ~egress_rules
    ~egress_audit ~api_limiter ~db item

let run_clawq_work_item ~(github_config : Runtime_config.github_config)
    ?(resolve_headers = (None : Github_api.resolve_headers_fn option))
    ?(egress_rules = ([] : Runtime_config_types.egress_rule list))
    ?(egress_audit = Policy_http_client.no_audit) ~db
    ~(config : Runtime_config.t option) ~api_limiter ~delivery_id ~owner ~repo
    ~author event ~(options : Github_work_item.command_options) ~preamble =
  let open Lwt.Syntax in
  let repo_full_name = owner ^ "/" ^ repo in
  let issue_number = issue_number_of_event event in
  let comment_id, is_pr =
    match event with
    | Github_webhook.IssueComment e -> (Some e.comment_id, e.is_pr)
    | Github_webhook.PrReviewComment e -> (Some e.comment_id, true)
    | _ -> (None, false)
  in
  let dedup_key =
    Github_work_item.dedup_key_for ~repo_full_name ~issue_number ~comment_id
      ~delivery_id:(Some delivery_id)
  in
  match
    Github_work_item.create_if_new ~db ~dedup_key ~delivery_id ~repo_full_name
      ~is_pr ~issue_number ~requester:author ?runner_pref:options.runner_opt
      ?host_pref:options.host_opt ~prompt:options.request ~preamble ()
  with
  | Result.Error msg -> Lwt.return (Ok ("work item refused: " ^ msg))
  | Result.Ok (Github_work_item.Duplicate existing) ->
      Logs.info (fun m ->
          m "GitHub: duplicate delivery for work item %d (%s); not launching"
            existing.id dedup_key);
      Lwt.return (Ok "duplicate work item")
  | Result.Ok (Github_work_item.Created item) -> (
      let* () =
        acknowledge_reaction ~github_config ~resolve_headers ~egress_rules
          ~egress_audit ~api_limiter ~owner ~repo event
      in
      let* ack_id =
        Lwt.catch
          (fun () ->
            let* _ok =
              Rate_limiter.check_and_consume api_limiter
                ~key:(Printf.sprintf "github:%s/%s" owner repo)
            in
            Github_api.post_comment_returning_id
              ~app_token:(Github_app_token.resolve_app_token ())
              ~auth:github_config.auth ~resolve_headers ~egress_rules
              ~egress_audit ~owner ~repo ~issue_number
              ~body:
                (Printf.sprintf
                   "> /clawq %s\n\n\xE2\x8F\xB3 Queued as work item %d...\n%s"
                   options.request item.id bot_reply_marker)
              ())
          (fun _ -> Lwt.return None)
      in
      (match ack_id with
      | Some cid ->
          Github_work_item.set_ack_comment ~db ~id:item.id ~comment_id:cid
      | None -> ());
      let allow_claude =
        match config with
        | Some cfg -> cfg.security.allow_anthropic_oauth_inference
        | None -> false
      in
      let preferred =
        match options.runner_opt with
        | Some r when String.lowercase_ascii r <> "auto" ->
            Background_task.runner_of_string r
        | _ -> None
      in
      let block reason =
        Github_work_item.record_result ~db ~id:item.id
          ~status:Github_work_item.Blocked
          ~result_kind:Github_work_item.Result_blocked ~result_summary:reason;
        Github_work_item.set_status ~db ~id:item.id
          ~status:Github_work_item.Blocked;
        (* Blocked is non-terminal for retries, but the requester still gets
           an actionable reply now. *)
        let* () =
          match item.ack_comment_id with
          | _ -> (
              match Github_work_item.get ~db ~id:item.id with
              | Some fresh -> (
                  match fresh.ack_comment_id with
                  | Some cid ->
                      Lwt.catch
                        (fun () ->
                          Github_api.edit_comment
                            ~app_token:(Github_app_token.resolve_app_token ())
                            ~auth:github_config.auth ~resolve_headers
                            ~egress_rules ~egress_audit ~owner ~repo
                            ~comment_id:cid
                            ~body:
                              (Printf.sprintf "Work item %d is blocked: %s\n%s"
                                 item.id reason bot_reply_marker)
                            ())
                        (fun _ -> Lwt.return_unit)
                  | None -> Lwt.return_unit)
              | None -> Lwt.return_unit)
        in
        Lwt.return (Ok "work item blocked")
      in
      match (options.runner_opt, preferred) with
      | Some r, None when String.lowercase_ascii r <> "auto" ->
          block
            (Printf.sprintf
               "Unknown runner %S. Use runner=auto, codex, or claude." r)
      | _ -> (
          match
            Background_task.resolve_runner ~check_available:true ?preferred
              ~allow_claude ()
          with
          | Error msg -> block msg
          | Ok (runner, auto_model) -> (
              let repo_path =
                match config with
                | Some cfg -> Runtime_config.effective_workspace cfg
                | None -> Filename.get_temp_dir_name ()
              in
              match
                Background_task.enqueue ~db ~runner ?model:auto_model
                  ~require_git:false ~automerge:false ~use_worktree:false
                  ?host_kind:options.host_opt ~repo_path
                  ~prompt:(work_item_prompt item) ~channel:"github"
                  ~channel_id:repo_full_name
                  ~session_key:(Github_webhook.session_key event)
                  ~requester:author ()
              with
              | Error msg -> block msg
              | Ok task_id ->
                  Github_work_item.attach_task ~db ~id:item.id
                    ~background_task_id:task_id;
                  Lwt.async (fun () ->
                      watch_work_item ~github_config ~resolve_headers
                        ~egress_rules ~egress_audit ~api_limiter ~db item
                        ~task_id);
                  Lwt.return (Ok (Printf.sprintf "work item %d queued" item.id))
              )))

(* Restart recovery: re-align work items with their background tasks and
   re-arm publication for anything terminal-but-unpublished. Call once at
   channel startup. *)
let recover_work_items ~(github_config : Runtime_config.github_config)
    ?(resolve_headers = (None : Github_api.resolve_headers_fn option))
    ?(egress_rules = ([] : Runtime_config_types.egress_rule list))
    ?(egress_audit = Policy_http_client.no_audit) ~api_limiter ~db () =
  let items = Github_work_item.list ~db () in
  List.iter
    (fun (item : Github_work_item.t) ->
      let terminal = Github_work_item.is_terminal_status item.status in
      match item.background_task_id with
      | None -> ()
      | Some task_id -> (
          match Background_task.get_task ~db ~id:task_id with
          | None -> ()
          | Some task ->
              if not terminal then
                ignore
                  (Github_work_item.sync_from_task ~db item
                     ~task_status:task.status ~task_result:task.result_preview);
              let needs_publication =
                match Github_work_item.get ~db ~id:item.id with
                | Some fresh ->
                    Github_work_item.is_terminal_status fresh.status
                    && not (Github_work_item.already_published fresh)
                | None -> false
              in
              if needs_publication then
                Lwt.async (fun () ->
                    publish_work_item_result ~github_config ~resolve_headers
                      ~egress_rules ~egress_audit ~api_limiter ~db item)
              else if
                (not terminal)
                && not (Background_task.is_terminal_status task.status)
              then
                Lwt.async (fun () ->
                    watch_work_item ~github_config ~resolve_headers
                      ~egress_rules ~egress_audit ~api_limiter ~db item ~task_id)
          ))
    items

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

(** [launch_triggered_review_runs ~db ~config ~session_manager ~event ~pr_files]
    finds pending review runs for the event's repo/PR and launches them as
    background tasks under each subscribed room's profile policy. The run prompt
    includes PR metadata, changed files, the access snapshot, room origin, and
    runner policy.

    Should be called after [trigger_review_runs_from_labels] creates the review
    run record and after PR files have been fetched.

    Returns the number of runs launched. *)
let launch_triggered_review_runs ~(db : Sqlite3.db) ~(config : Runtime_config.t)
    ~(session_manager : Session.t) ~(event : Github_webhook.parsed_event)
    ~(pr_files : (string * string * int * int) list) ?agent_name () =
  let open Lwt.Syntax in
  let repo, pr_number =
    match event with
    | Github_webhook.PullRequest pr -> (pr.owner ^ "/" ^ pr.repo, pr.pr_number)
    | _ -> ("", 0)
  in
  if repo = "" || pr_number <= 0 then Lwt.return 0
  else
    let all_runs = Github_review_run.find_by_repo_pr ~db ~repo ~pr_number in
    let pending =
      List.filter
        (fun (r : Github_review_run.review_run) ->
          r.status = Github_review_run.Pending)
        all_runs
    in
    if pending = [] then Lwt.return 0
    else
      (* Filter to enabled subscriptions only *)
      let subscriptions =
        Github_pr_subscriptions.find_by_repo_pr ~db ~repo ~pr_number
        |> List.filter (fun (sub : Github_pr_subscriptions.subscription) ->
            sub.enabled)
      in
      if subscriptions = [] then (
        List.iter
          (fun (run : Github_review_run.review_run) ->
            ignore
              (Github_review_run.set_failed ~db ~id:run.id
                 ~error_message:"No enabled room subscriptions for this PR"))
          pending;
        Lwt.return 0)
      else
        match event with
        | Github_webhook.PullRequest pr ->
            let* count =
              Lwt_list.fold_left_s
                (fun acc (run : Github_review_run.review_run) ->
                  let results =
                    List.filter_map
                      (fun (sub : Github_pr_subscriptions.subscription) ->
                        match
                          Session.find_registered_notifier session_manager
                            ~key:sub.room_id
                        with
                        | None ->
                            Logs.debug (fun m ->
                                m "Review run launch: no notifier for room %s"
                                  sub.room_id);
                            None
                        | Some _ ->
                            let requester_id =
                              "github:" ^ Github_webhook.author_of_event event
                            in
                            Some
                              (Background_task.launch_triggered_run ~db ~config
                                 ~review_run:run ~room_id:sub.room_id
                                 ~requester_id ~pr_title:pr.pr_title
                                 ~pr_author:pr.pr_author ~pr_body:pr.pr_body
                                 ~base_branch:pr.base_branch
                                 ~head_branch:pr.head_branch ~pr_files
                                 ?agent_name ()))
                      subscriptions
                  in
                  let launched =
                    List.fold_left
                      (fun acc r ->
                        match r with Result.Ok _ -> acc + 1 | _ -> acc)
                      0 results
                  in
                  Lwt.return (acc + launched))
                0 pending
            in
            Lwt.return count
        | _ -> Lwt.return 0

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
                            | Some notifier ->
                                let* () = notifier text in
                                Lwt.return ""
                            | None ->
                                Logs.debug (fun m ->
                                    m
                                      "GitHub PR dispatch: no notifier for \
                                       room %s"
                                      room_id);
                                Lwt.return "")
                          ()
                    | None -> Lwt.return 0
                  in
                  (* Trigger review runs from label events (creates DB records) *)
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
                  (* Launch triggered review runs as background tasks under
                     room profile policy. Requires PR files and config. *)
                  let* _launched_runs =
                    match (Session.get_db session_manager, config) with
                    | Some db, Some cfg ->
                        launch_triggered_review_runs ~db ~config:cfg
                          ~session_manager ~event ~pr_files
                          ?agent_name:repo_config.agent_name ()
                    | _ -> Lwt.return 0
                  in
                  let db_opt = Session.get_db session_manager in
                  match Github_webhook.extract_clawq ~event ~pr_files with
                  | Some (user_message, preamble) -> (
                      let options =
                        Github_work_item.parse_command_options user_message
                      in
                      match db_opt with
                      | Some db
                        when Github_work_item.wants_work_item options
                             && options.request <> "" ->
                          Github_work_item.init_schema db;
                          run_clawq_work_item ~github_config ~resolve_headers
                            ~egress_rules ~egress_audit ~db ~config ~api_limiter
                            ~delivery_id ~owner ~repo ~author event ~options
                            ~preamble
                      | _ ->
                          run_clawq_command ~github_config ~resolve_headers
                            ~egress_rules ~egress_audit ?db:db_opt
                            ~session_manager ~api_limiter ~owner ~repo ~author
                            event ~user_message ~preamble)
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
                    | Some notifier ->
                        let* () = notifier text in
                        Lwt.return ""
                    | None ->
                        Logs.debug (fun m ->
                            m "GitHub PR dispatch: no notifier for room %s"
                              room_id);
                        Lwt.return "")
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
