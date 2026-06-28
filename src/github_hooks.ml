(* GitHub-specific webhook hook infrastructure.

   Disable warning 16 (unerasable-optional-argument) — optional
   [resolve_headers] parameter uses same pattern as github_api.ml. *)
[@@@warning "-16"]

(* Built on top of Webhook_handler for generic utilities (JSON path traversal,
   frontmatter parsing, template rendering, match rule evaluation, delivery
   snapshots). This module supplies GitHub-specific event preparation, context
   extraction, session key derivation, and prompt assembly. *)

type match_rule = Webhook_handler.match_rule = {
  path : string;
  expected : string;
}

type hook = {
  name : string;
  repo : string;
  event : string;
  enabled : bool;
  post_back_to_github : bool;
  match_rules : match_rule list;
  prompt_template : string;
  source_path : string;
}

type prepared_event = {
  event_name : string;
  delivery_id : string;
  snapshot_path : string option;
  raw_body : string;
  payload_json : Yojson.Safe.t option;
  context_json : Yojson.Safe.t option;
  repo_full_name : string;
  sender_login : string;
  is_user_generated : bool;
  installation_id : int option;
}

(* ---- Directory layout ---- *)

let workspace_root () = Filename.concat (Dot_dir.ensure ()) "workspace"
let hooks_dir () = Filename.concat (workspace_root ()) "gh-hooks"

let deliveries_dir () =
  Filename.concat
    (Filename.concat (workspace_root ()) "tmp")
    "github-deliveries"

(* ---- Re-exported generic utilities (backward compat) ---- *)

let lookup_json_path = Webhook_handler.lookup_json_path
let string_of_json = Webhook_handler.string_of_json
let first_some = Webhook_handler.first_some
let first_string = Webhook_handler.first_string
let first_int = Webhook_handler.first_int
let render_template = Webhook_handler.render_template
let value_matches = Webhook_handler.value_matches
let max_inline_payload_chars = Webhook_handler.default_max_inline_payload_chars

(* ---- GitHub-specific event classification ---- *)

let is_user_generated_event = function
  | "workflow_job" | "workflow_run" | "check_run" | "check_suite" | "ping" ->
      false
  | _ -> true

(* ---- GitHub-specific payload extraction ---- *)

let repo_full_name_of_payload json =
  match Webhook_handler.first_string json [ "repository.full_name" ] with
  | Some repo -> repo
  | None -> (
      match
        ( Webhook_handler.first_string json [ "repository.owner.login" ],
          Webhook_handler.first_string json [ "repository.name" ] )
      with
      | Some owner, Some repo -> owner ^ "/" ^ repo
      | _ -> "")

let sender_login_of_payload json =
  match Webhook_handler.first_string json [ "sender.login" ] with
  | Some sender -> sender
  | None ->
      Option.value
        (Webhook_handler.first_string json
           [
             "comment.user.login";
             "review.user.login";
             "issue.user.login";
             "pull_request.user.login";
           ])
        ~default:""

(* ---- GitHub-specific context JSON ---- *)

let build_context_json ~event_name ~delivery_id ~snapshot_path ~payload =
  let get_string paths = Webhook_handler.first_string payload paths in
  let get_int paths = Webhook_handler.first_int payload paths in
  let pull_request_number =
    get_int
      [
        "pull_request.number";
        "issue.number";
        "workflow_run.pull_requests.0.number";
        "check_run.pull_requests.0.number";
        "check_suite.pull_requests.0.number";
      ]
  in
  let issue_number = get_int [ "issue.number" ] in
  let add_opt_string key value acc =
    match value with Some v -> (key, `String v) :: acc | None -> acc
  in
  let add_opt_int key value acc =
    match value with Some v -> (key, `Int v) :: acc | None -> acc
  in
  let is_pull_request =
    match Webhook_handler.lookup_json_path payload "pull_request.number" with
    | Some _ -> true
    | None -> (
        match Webhook_handler.lookup_json_path payload "issue.pull_request" with
        | Some `Null | None -> false
        | Some _ -> true)
  in
  let fields =
    []
    |> add_opt_string "action" (get_string [ "action" ])
    |> add_opt_string "sender" (Some (sender_login_of_payload payload))
    |> add_opt_string "repo" (Some (repo_full_name_of_payload payload))
    |> add_opt_string "title"
         (get_string
            [
              "pull_request.title";
              "issue.title";
              "workflow_run.name";
              "workflow_job.name";
            ])
    |> add_opt_string "body"
         (get_string
            [ "comment.body"; "review.body"; "issue.body"; "pull_request.body" ])
    |> add_opt_string "comment_body"
         (get_string [ "comment.body"; "review.body" ])
    |> add_opt_string "status"
         (get_string
            [
              "workflow_job.status";
              "workflow_run.status";
              "check_run.status";
              "check_suite.status";
              "status";
            ])
    |> add_opt_string "conclusion"
         (get_string
            [
              "workflow_job.conclusion";
              "workflow_run.conclusion";
              "check_run.conclusion";
              "check_suite.conclusion";
              "conclusion";
            ])
    |> add_opt_string "branch"
         (get_string
            [
              "workflow_job.head_branch";
              "workflow_run.head_branch";
              "check_suite.head_branch";
              "pull_request.base.ref";
              "ref";
            ])
    |> add_opt_string "head_sha"
         (get_string
            [
              "workflow_job.head_sha";
              "workflow_run.head_sha";
              "check_run.head_sha";
              "check_suite.head_sha";
              "pull_request.head.sha";
              "after";
            ])
    |> add_opt_string "html_url"
         (get_string
            [
              "workflow_job.html_url";
              "workflow_run.html_url";
              "check_run.html_url";
              "check_run.details_url";
              "pull_request.html_url";
              "comment.html_url";
              "issue.html_url";
            ])
    |> add_opt_string "payload_path" snapshot_path
    |> add_opt_int "pull_request_number" pull_request_number
    |> add_opt_int "issue_number" issue_number
    |> add_opt_int "workflow_run_id"
         (get_int [ "workflow_run.id"; "workflow_job.run_id" ])
    |> add_opt_int "workflow_job_id" (get_int [ "workflow_job.id" ])
    |> add_opt_int "check_run_id" (get_int [ "check_run.id" ])
    |> add_opt_int "check_suite_id" (get_int [ "check_suite.id" ])
  in
  `Assoc
    (("event_name", `String event_name)
    :: ("delivery_id", `String delivery_id)
    :: ("is_pull_request", `Bool is_pull_request)
    :: ("raw", payload) :: List.rev fields)

(* ---- Hook directory management ---- *)

let ensure_hook_dirs () =
  Workspace_scaffold.ensure_dir (hooks_dir ());
  Workspace_scaffold.ensure_dir (deliveries_dir ())

(* ---- Delivery snapshots (delegates to Webhook_handler) ---- *)

let cleanup_delivery_snapshots () =
  ensure_hook_dirs ();
  Webhook_handler.cleanup_delivery_snapshots ~dir:(deliveries_dir ()) ()

let write_delivery_snapshot ~delivery_id ~raw_body =
  ensure_hook_dirs ();
  Webhook_handler.write_delivery_snapshot ~dir:(deliveries_dir ()) ~delivery_id
    ~raw_body

(* ---- Event preparation ---- *)

let prepare_event ~event_name ~headers ~raw_body =
  let delivery_id =
    Cohttp.Header.get headers "x-github-delivery" |> Option.value ~default:""
  in
  let snapshot_path = write_delivery_snapshot ~delivery_id ~raw_body in
  let payload_json =
    try Some (Yojson.Safe.from_string raw_body) with _ -> None
  in
  let repo_full_name, sender_login, context_json, installation_id =
    match payload_json with
    | Some payload ->
        ( repo_full_name_of_payload payload,
          sender_login_of_payload payload,
          Some
            (build_context_json ~event_name ~delivery_id ~snapshot_path ~payload),
          Webhook_handler.first_int payload [ "installation.id" ] )
    | None -> ("", "", None, None)
  in
  {
    event_name;
    delivery_id;
    snapshot_path;
    raw_body;
    payload_json;
    context_json;
    repo_full_name;
    sender_login;
    is_user_generated = is_user_generated_event event_name;
    installation_id;
  }

(* ---- Hook loading (uses generic frontmatter parser) ---- *)

let load_hook_file path =
  let content = Webhook_handler.read_file path in
  let lines = String.split_on_char '\n' content in
  let fm = Webhook_handler.parse_frontmatter lines in
  let repo =
    List.assoc_opt "repo" fm.fields |> Option.value ~default:"" |> String.trim
  in
  let post_back_to_github =
    match List.assoc_opt "post_back_to_github" fm.fields with
    | Some v -> Option.value (Webhook_handler.parse_bool v) ~default:false
    | None -> false
  in
  let prompt_template = String.concat "\n" fm.body_lines |> String.trim in
  let fallback_name = Filename.basename path |> Filename.remove_extension in
  let hook =
    {
      name = (if fm.name = "" then fallback_name else fm.name);
      repo = String.trim repo;
      event = String.trim fm.event;
      enabled = fm.enabled;
      post_back_to_github;
      match_rules = fm.match_rules;
      prompt_template;
      source_path = path;
    }
  in
  if hook.repo = "" then
    Error
      (Printf.sprintf
         "hook %s is missing required frontmatter field 'repo'; set repo: \
          owner/repo"
         path)
  else if hook.event = "" then
    Error
      (Printf.sprintf
         "hook %s is missing required frontmatter field 'event'; set event: \
          <github-event-name>"
         path)
  else if hook.prompt_template = "" then
    Error (Printf.sprintf "hook %s has an empty prompt body" path)
  else Ok hook

let load_hooks ~repo_full_name ~event_name =
  ensure_hook_dirs ();
  let paths =
    Webhook_handler.load_hook_files ~dir:(hooks_dir ()) ~suffix:".md"
  in
  List.fold_left
    (fun acc path ->
      match load_hook_file path with
      | Ok hook
        when hook.enabled && hook.repo = repo_full_name
             && hook.event = event_name ->
          hook :: acc
      | Ok _ -> acc
      | Error err ->
          Logs.warn (fun m -> m "GitHub hooks: %s" err);
          acc)
    [] paths
  |> List.rev

(* ---- Hook matching (delegates to Webhook_handler) ---- *)

let hook_matches hook context_json =
  Webhook_handler.rules_match hook.match_rules context_json

(* ---- Prompt building ---- *)

let truncate_payload s = Webhook_handler.truncate_payload s

let build_prompt ~hook ~prepared ~context_json =
  let rendered =
    Webhook_handler.render_template ~template:hook.prompt_template ~context_json
  in
  let payload_note =
    match prepared.snapshot_path with
    | Some path ->
        Printf.sprintf
          "\n\n\
           ## Raw Webhook Payload\n\
           Saved at: %s\n\
           If that path is not readable from the agent sandbox, use the inline \
           JSON below.\n\n\
           ```json\n\
           %s\n\
           ```"
          path
          (Webhook_handler.truncate_payload prepared.raw_body)
    | None ->
        Printf.sprintf
          "\n\n\
           ## Raw Webhook Payload\n\
           No snapshot path was available. Use the inline JSON below.\n\n\
           ```json\n\
           %s\n\
           ```"
          (Webhook_handler.truncate_payload prepared.raw_body)
  in
  Printf.sprintf
    "## GitHub Hook Context\n\
     Hook: %s\n\
     Event: %s\n\
     Repository: %s\n\
     Delivery: %s\n\
     Source: %s\n\n\
     %s%s"
    hook.name prepared.event_name prepared.repo_full_name
    (if prepared.delivery_id = "" then "(missing)" else prepared.delivery_id)
    hook.source_path rendered payload_note

(* ---- Session key derivation ---- *)

let default_session_key prepared =
  match prepared.context_json with
  | None -> "github:unknown"
  | Some context -> (
      let repo =
        match Webhook_handler.lookup_json_path context "repo" with
        | Some (`String s) when s <> "" -> s
        | _ -> prepared.repo_full_name
      in
      match Webhook_handler.first_int context [ "pull_request_number" ] with
      | Some n -> Printf.sprintf "github:%s:pr:%d" repo n
      | None -> (
          match Webhook_handler.first_int context [ "issue_number" ] with
          | Some n -> Printf.sprintf "github:%s:issue:%d" repo n
          | None -> (
              match Webhook_handler.first_int context [ "workflow_run_id" ] with
              | Some n -> Printf.sprintf "github:%s:workflow_run:%d" repo n
              | None -> (
                  match
                    Webhook_handler.first_int context [ "workflow_job_id" ]
                  with
                  | Some n -> Printf.sprintf "github:%s:workflow_job:%d" repo n
                  | None -> (
                      match
                        Webhook_handler.first_int context [ "check_run_id" ]
                      with
                      | Some n -> Printf.sprintf "github:%s:check_run:%d" repo n
                      | None ->
                          let suffix =
                            if prepared.delivery_id <> "" then
                              prepared.delivery_id
                            else "delivery"
                          in
                          Printf.sprintf "github:%s:event:%s:%s" repo
                            prepared.event_name
                            (Webhook_handler.sanitize_filename_component suffix)
                      )))))

(* ---- Hook execution ---- *)

let post_hook_response_to_github ~(github_config : Runtime_config.github_config)
    ?(resolve_headers = (None : Github_api.resolve_headers_fn option))
    ~api_limiter ~context_json ~response () =
  let open Lwt.Syntax in
  let owner, repo =
    match Webhook_handler.first_string context_json [ "repo" ] with
    | Some r -> (
        match String.split_on_char '/' r with
        | [ o; rp ] -> (o, rp)
        | _ -> ("", ""))
    | None -> ("", "")
  in
  let issue_number =
    Webhook_handler.first_int context_json
      [ "pull_request_number"; "issue_number" ]
  in
  if owner = "" || repo = "" then (
    Logs.warn (fun m ->
        m
          "GitHub hooks: post_back_to_github skipped — could not extract \
           owner/repo from context");
    Lwt.return ())
  else
    match issue_number with
    | None ->
        Logs.info (fun m ->
            m
              "GitHub hooks: post_back_to_github skipped — no issue/PR number \
               in context for %s/%s"
              owner repo);
        Lwt.return ()
    | Some n ->
        Lwt.catch
          (fun () ->
            let* _ok =
              Rate_limiter.check_and_consume api_limiter
                ~key:(Printf.sprintf "github:%s/%s" owner repo)
            in
            let body = response ^ "\n<!-- clawq-reply -->" in
            Github_api.post_comment
              ~app_token:(Github_app_token.resolve_app_token ())
              ~auth:github_config.auth ~resolve_headers ~owner ~repo
              ~issue_number:n ~body ())
          (fun exn ->
            Logs.err (fun m ->
                m "GitHub hooks: post_back_to_github failed for %s/%s#%d: %s"
                  owner repo n (Printexc.to_string exn));
            Lwt.return ())

let run_matching_hooks ~(session_manager : Session.t)
    ~(github_config : Runtime_config.github_config option)
    ?(resolve_headers = (None : Github_api.resolve_headers_fn option))
    ~api_limiter ~prepared () =
  let open Lwt.Syntax in
  match prepared.context_json with
  | None -> Lwt.return 0
  | Some context_json ->
      let hooks =
        load_hooks ~repo_full_name:prepared.repo_full_name
          ~event_name:prepared.event_name
      in
      let matched =
        List.filter (fun hook -> hook_matches hook context_json) hooks
      in
      let* count =
        Lwt_list.fold_left_s
          (fun acc hook ->
            let key = default_session_key prepared in
            let message = build_prompt ~hook ~prepared ~context_json in
            let channel_name = "github:" ^ prepared.repo_full_name in
            let sender_id =
              if prepared.sender_login <> "" then prepared.sender_login
              else "github-webhook"
            in
            Logs.info (fun m ->
                m "GitHub hooks: invoking hook %s for %s %s key=%s sender=%s"
                  hook.name prepared.repo_full_name prepared.event_name key
                  sender_id);
            let* ran =
              Lwt.catch
                (fun () ->
                  let* response =
                    Session.turn session_manager ~key ~message ~channel_name
                      ~channel_type:"dm" ~sender_id ~channel:"github"
                      ~channel_id:prepared.repo_full_name
                      ~snapshot_work_type:Access_snapshot.GitHub_trigger ()
                  in
                  Logs.info (fun m ->
                      m "GitHub hooks: ran hook %s for %s %s response=%S"
                        hook.name prepared.repo_full_name prepared.event_name
                        response);
                  match github_config with
                  | Some gc when hook.post_back_to_github ->
                      let* () =
                        post_hook_response_to_github ~github_config:gc
                          ~resolve_headers ~api_limiter ~context_json ~response
                          ()
                      in
                      Lwt.return 1
                  | _ ->
                      if hook.post_back_to_github then
                        Logs.warn (fun m ->
                            m
                              "GitHub hooks: post_back_to_github requested for \
                               hook %s but no github_config available"
                              hook.name);
                      Lwt.return 1)
                (fun exn ->
                  Logs.err (fun m ->
                      m "GitHub hooks: hook %s failed: %s" hook.name
                        (Printexc.to_string exn));
                  Lwt.return 0)
            in
            Lwt.return (acc + ran))
          0 matched
      in
      if matched <> [] then
        Logs.info (fun m ->
            m "GitHub hooks: %d/%d hooks matched for %s %s" count
              (List.length matched) prepared.repo_full_name prepared.event_name);
      Lwt.return count
