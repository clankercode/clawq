type match_rule = { path : string; expected : string }

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
}

let workspace_root () = Filename.concat (Dot_dir.ensure ()) "workspace"
let hooks_dir () = Filename.concat (workspace_root ()) "gh-hooks"

let deliveries_dir () =
  Filename.concat
    (Filename.concat (workspace_root ()) "tmp")
    "github-deliveries"

let max_inline_payload_chars = 12_000
let delivery_retention_seconds = 48. *. 3600.

let is_user_generated_event = function
  | "workflow_job" | "workflow_run" | "check_run" | "check_suite" | "ping" ->
      false
  | _ -> true

let sanitize_filename_component s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' ->
          Buffer.add_char buf c
      | _ -> Buffer.add_char buf '_')
    s;
  let cleaned = Buffer.contents buf in
  if cleaned = "" then "delivery" else cleaned

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let write_file path content =
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc content)

let is_digits s =
  let len = String.length s in
  len > 0
  &&
  let rec loop i =
    if i >= len then true
    else match s.[i] with '0' .. '9' -> loop (i + 1) | _ -> false
  in
  loop 0

let rec nth_opt xs idx =
  match (xs, idx) with
  | [], _ -> None
  | x :: _, 0 -> Some x
  | _ :: rest, n when n > 0 -> nth_opt rest (n - 1)
  | _ -> None

let lookup_json_path json path =
  let segments =
    String.split_on_char '.' path
    |> List.map String.trim
    |> List.filter (fun s -> s <> "")
  in
  let rec loop current = function
    | [] -> Some current
    | seg :: rest -> (
        match current with
        | `Assoc fields -> (
            match List.assoc_opt seg fields with
            | Some value -> loop value rest
            | None -> None)
        | `List items when is_digits seg -> (
            match nth_opt items (int_of_string seg) with
            | Some value -> loop value rest
            | None -> None)
        | _ -> None)
  in
  loop json segments

let string_of_json = function
  | `Null -> ""
  | `String s -> s
  | `Bool b -> string_of_bool b
  | `Int i -> string_of_int i
  | `Intlit s -> s
  | `Float f -> string_of_float f
  | `List _ as json -> Yojson.Safe.pretty_to_string json
  | `Assoc _ as json -> Yojson.Safe.pretty_to_string json

let first_some f values =
  let rec loop = function
    | [] -> None
    | x :: rest -> (
        match f x with Some _ as found -> found | None -> loop rest)
  in
  loop values

let first_string json paths =
  first_some
    (fun path ->
      match lookup_json_path json path with
      | Some (`String s) when String.trim s <> "" -> Some (String.trim s)
      | Some value ->
          let s = string_of_json value |> String.trim in
          if s = "" then None else Some s
      | None -> None)
    paths

let first_int json paths =
  first_some
    (fun path ->
      match lookup_json_path json path with
      | Some (`Int i) -> Some i
      | Some (`Intlit s) -> ( try Some (int_of_string s) with _ -> None)
      | Some (`String s) -> (
          try Some (int_of_string (String.trim s)) with _ -> None)
      | _ -> None)
    paths

let repo_full_name_of_payload json =
  match first_string json [ "repository.full_name" ] with
  | Some repo -> repo
  | None -> (
      match
        ( first_string json [ "repository.owner.login" ],
          first_string json [ "repository.name" ] )
      with
      | Some owner, Some repo -> owner ^ "/" ^ repo
      | _ -> "")

let sender_login_of_payload json =
  match first_string json [ "sender.login" ] with
  | Some sender -> sender
  | None ->
      Option.value
        (first_string json
           [
             "comment.user.login";
             "review.user.login";
             "issue.user.login";
             "pull_request.user.login";
           ])
        ~default:""

let build_context_json ~event_name ~delivery_id ~snapshot_path ~payload =
  let get_string paths = first_string payload paths in
  let get_int paths = first_int payload paths in
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
    match lookup_json_path payload "pull_request.number" with
    | Some _ -> true
    | None -> (
        match lookup_json_path payload "issue.pull_request" with
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

let ensure_hook_dirs () =
  Workspace_scaffold.ensure_dir (hooks_dir ());
  Workspace_scaffold.ensure_dir (deliveries_dir ())

let cleanup_delivery_snapshots () =
  ensure_hook_dirs ();
  let dir = deliveries_dir () in
  let now = Unix.gettimeofday () in
  try
    Sys.readdir dir
    |> Array.iter (fun name ->
        let path = Filename.concat dir name in
        try
          let stats = Unix.stat path in
          if now -. stats.Unix.st_mtime > delivery_retention_seconds then
            Sys.remove path
        with exn ->
          Logs.warn (fun m ->
              m "GitHub hooks: failed cleaning snapshot %s: %s" path
                (Printexc.to_string exn)));
    ()
  with exn ->
    Logs.warn (fun m ->
        m "GitHub hooks: failed scanning delivery snapshots: %s"
          (Printexc.to_string exn))

let write_delivery_snapshot ~delivery_id ~raw_body =
  ensure_hook_dirs ();
  cleanup_delivery_snapshots ();
  let stamp = int_of_float (Unix.gettimeofday ()) in
  let base =
    Printf.sprintf "%d-%s.json" stamp (sanitize_filename_component delivery_id)
  in
  let path = Filename.concat (deliveries_dir ()) base in
  try
    write_file path raw_body;
    Some path
  with exn ->
    Logs.warn (fun m ->
        m "GitHub hooks: failed writing delivery snapshot %s: %s" path
          (Printexc.to_string exn));
    None

let prepare_event ~event_name ~headers ~raw_body =
  let delivery_id =
    Cohttp.Header.get headers "x-github-delivery" |> Option.value ~default:""
  in
  let snapshot_path = write_delivery_snapshot ~delivery_id ~raw_body in
  let payload_json =
    try Some (Yojson.Safe.from_string raw_body) with _ -> None
  in
  let repo_full_name, sender_login, context_json =
    match payload_json with
    | Some payload ->
        ( repo_full_name_of_payload payload,
          sender_login_of_payload payload,
          Some
            (build_context_json ~event_name ~delivery_id ~snapshot_path ~payload)
        )
    | None -> ("", "", None)
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
  }

let parse_bool s =
  match String.lowercase_ascii (String.trim s) with
  | "true" | "yes" | "on" -> Some true
  | "false" | "no" | "off" -> Some false
  | _ -> None

let parse_frontmatter lines =
  match lines with
  | "---" :: rest ->
      let rec loop current_section name repo event enabled post_back_to_github
          match_rules = function
        | [] ->
            ( name,
              repo,
              event,
              enabled,
              post_back_to_github,
              List.rev match_rules,
              [] )
        | "---" :: body ->
            ( name,
              repo,
              event,
              enabled,
              post_back_to_github,
              List.rev match_rules,
              body )
        | line :: more -> (
            let raw = String.trim line in
            if raw = "" || raw.[0] = '#' then
              loop current_section name repo event enabled post_back_to_github
                match_rules more
            else if
              current_section = "match"
              && String.length line > 0
              && line.[0] = ' '
            then
              match String.index_opt raw ':' with
              | Some idx ->
                  let key = String.sub raw 0 idx |> String.trim in
                  let value =
                    String.sub raw (idx + 1) (String.length raw - idx - 1)
                    |> String.trim
                  in
                  let rule = { path = key; expected = value } in
                  loop current_section name repo event enabled
                    post_back_to_github (rule :: match_rules) more
              | None ->
                  loop current_section name repo event enabled
                    post_back_to_github match_rules more
            else
              match String.index_opt raw ':' with
              | Some idx ->
                  let key = String.sub raw 0 idx |> String.trim in
                  let value =
                    String.sub raw (idx + 1) (String.length raw - idx - 1)
                    |> String.trim
                  in
                  let section =
                    if key = "match" && value = "" then "match" else ""
                  in
                  let name = if key = "name" then value else name in
                  let repo = if key = "repo" then value else repo in
                  let event = if key = "event" then value else event in
                  let enabled =
                    if key = "enabled" then
                      Option.value (parse_bool value) ~default:enabled
                    else enabled
                  in
                  let post_back_to_github =
                    if key = "post_back_to_github" then
                      Option.value (parse_bool value)
                        ~default:post_back_to_github
                    else post_back_to_github
                  in
                  loop section name repo event enabled post_back_to_github
                    match_rules more
              | None ->
                  loop current_section name repo event enabled
                    post_back_to_github match_rules more)
      in
      loop "" "" "" "" true false [] rest
  | _ -> ("", "", "", true, false, [], lines)

let load_hook_file path =
  let content = read_file path in
  let lines = String.split_on_char '\n' content in
  let name, repo, event, enabled, post_back_to_github, match_rules, body_lines =
    parse_frontmatter lines
  in
  let prompt_template = String.concat "\n" body_lines |> String.trim in
  let fallback_name = Filename.basename path |> Filename.remove_extension in
  let hook =
    {
      name = (if name = "" then fallback_name else name);
      repo = String.trim repo;
      event = String.trim event;
      enabled;
      post_back_to_github;
      match_rules;
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
    try
      Sys.readdir (hooks_dir ())
      |> Array.to_list
      |> List.filter (fun name -> Filename.check_suffix name ".md")
      |> List.map (Filename.concat (hooks_dir ()))
      |> List.sort String.compare
    with _ -> []
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

let rec value_matches json expected =
  let expected = String.trim expected in
  let expected_lower = String.lowercase_ascii expected in
  match (expected_lower, json) with
  | "exists", `Null -> false
  | "exists", _ -> true
  | _, `List items ->
      List.exists (fun item -> value_matches item expected) items
  | _, _ ->
      let actual =
        string_of_json json |> String.trim |> String.lowercase_ascii
      in
      actual = expected_lower

let hook_matches hook context_json =
  List.for_all
    (fun rule ->
      match lookup_json_path context_json rule.path with
      | Some value -> value_matches value rule.expected
      | None -> false)
    hook.match_rules

let truncate_payload s =
  if String.length s <= max_inline_payload_chars then s
  else
    String.sub s 0 max_inline_payload_chars
    ^ Printf.sprintf "\n... [truncated %d chars]"
        (String.length s - max_inline_payload_chars)

let render_template ~template ~context_json =
  (* TODO B381 follow-up: add relative file includes rooted under
     ~/.clawq/workspace/gh-hooks/ once the sandbox/file-access contract is
     well defined across connectors and subagents. *)
  let pattern = Str.regexp "{{\\([^}]+\\)}}" in
  Str.global_substitute pattern
    (fun matched ->
      let expr = Str.matched_group 1 matched |> String.trim in
      if String.length expr >= 5 && String.sub expr 0 5 = "json " then
        let path = String.sub expr 5 (String.length expr - 5) |> String.trim in
        match lookup_json_path context_json path with
        | Some json -> Yojson.Safe.pretty_to_string json
        | None -> ""
      else if String.length expr >= 8 && String.sub expr 0 8 = "include " then
        "[include not implemented yet]"
      else
        match lookup_json_path context_json expr with
        | Some json -> string_of_json json
        | None -> "")
    template

let default_session_key prepared =
  match prepared.context_json with
  | None -> "github:unknown"
  | Some context -> (
      let repo =
        match lookup_json_path context "repo" with
        | Some (`String s) when s <> "" -> s
        | _ -> prepared.repo_full_name
      in
      match first_int context [ "pull_request_number" ] with
      | Some n -> Printf.sprintf "github:%s:pr:%d" repo n
      | None -> (
          match first_int context [ "issue_number" ] with
          | Some n -> Printf.sprintf "github:%s:issue:%d" repo n
          | None -> (
              match first_int context [ "workflow_run_id" ] with
              | Some n -> Printf.sprintf "github:%s:workflow_run:%d" repo n
              | None -> (
                  match first_int context [ "workflow_job_id" ] with
                  | Some n -> Printf.sprintf "github:%s:workflow_job:%d" repo n
                  | None -> (
                      match first_int context [ "check_run_id" ] with
                      | Some n -> Printf.sprintf "github:%s:check_run:%d" repo n
                      | None ->
                          let suffix =
                            if prepared.delivery_id <> "" then
                              prepared.delivery_id
                            else "delivery"
                          in
                          Printf.sprintf "github:%s:event:%s:%s" repo
                            prepared.event_name
                            (sanitize_filename_component suffix))))))

let build_prompt ~hook ~prepared ~context_json =
  let rendered = render_template ~template:hook.prompt_template ~context_json in
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
          (truncate_payload prepared.raw_body)
    | None ->
        Printf.sprintf
          "\n\n\
           ## Raw Webhook Payload\n\
           No snapshot path was available. Use the inline JSON below.\n\n\
           ```json\n\
           %s\n\
           ```"
          (truncate_payload prepared.raw_body)
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

let run_matching_hooks ~(session_manager : Session.t) ~prepared =
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
            if hook.post_back_to_github then
              Logs.info (fun m ->
                  m
                    "GitHub hooks: hook %s requested post_back_to_github but \
                     posting follow-up comments is not implemented yet"
                    hook.name);
            Logs.info (fun m ->
                m "GitHub hooks: invoking hook %s for %s %s key=%s sender=%s"
                  hook.name prepared.repo_full_name prepared.event_name key
                  sender_id);
            let* ran =
              Lwt.catch
                (fun () ->
                  let* response =
                    Session.turn session_manager ~key ~message ~channel_name
                      ~channel_type:"dm" ~sender_id ()
                  in
                  Logs.info (fun m ->
                      m "GitHub hooks: ran hook %s for %s %s response=%S"
                        hook.name prepared.repo_full_name prepared.event_name
                        response);
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
