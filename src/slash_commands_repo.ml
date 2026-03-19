(* Format functions and shared handler for /repo slash command. *)

let format_repo_status ~connector ~(repo_info : Repo_manager.repo_info)
    ~(status : Repo_manager.repo_status) =
  let c = Format_adapter.code connector in
  let dirty_str = if status.dirty then " (dirty)" else "" in
  let ahead_behind =
    match (status.ahead, status.behind) with
    | 0, 0 -> ""
    | a, 0 -> Printf.sprintf " [ahead %d]" a
    | 0, b -> Printf.sprintf " [behind %d]" b
    | a, b -> Printf.sprintf " [ahead %d, behind %d]" a b
  in
  let url_line =
    match repo_info.repo_url with Some url -> "\nURL: " ^ c url | None -> ""
  in
  let fetch_line =
    match repo_info.last_fetched_at with
    | Some ts -> "\nLast fetched: " ^ ts
    | None -> ""
  in
  let error_line =
    match repo_info.last_fetch_error with
    | Some e -> "\nLast fetch error: " ^ e
    | None -> ""
  in
  let managed_str = if repo_info.is_managed then " (managed)" else " (local)" in
  Printf.sprintf "Repository%s\nPath: %s%s\nBranch: %s @ %s%s%s%s%s" managed_str
    (c repo_info.local_path) url_line (c status.branch) (c status.commit_short)
    dirty_str ahead_behind fetch_line error_line

let format_repo_not_associated ~connector =
  "No repository associated with this session.\n\nUsage:\n  "
  ^ Format_adapter.code connector "/repo <url>"
  ^ " — clone and associate\n  "
  ^ Format_adapter.code connector "/repo <path>"
  ^ " — associate with local repo"

let format_repo_associated ~connector ~path ~message =
  message ^ "\nCWD set to " ^ Format_adapter.code connector path

let format_repo_forgotten ~connector:_ = "Repository association removed."
let format_repo_updated ~connector:_ ~message = message
let format_repo_error ~connector:_ ~error = "Error: " ^ error

(* Shared handler for all connectors. Takes callbacks to send replies
   and update CWD. Returns unit Lwt.t *)
let handle_repo_action ~db ~session_key ~connector ~send_reply ~set_cwd action =
  let open Lwt.Syntax in
  let open Slash_commands_fmt in
  match action with
  | RepoStatus -> (
      match Repo_manager.get_repo ~db ~session_key with
      | None -> send_reply (format_repo_not_associated ~connector)
      | Some repo_info -> (
          match Repo_manager.repo_status ~path:repo_info.local_path with
          | Ok status ->
              send_reply (format_repo_status ~connector ~repo_info ~status)
          | Error e ->
              send_reply
                (format_repo_error ~connector
                   ~error:(Printf.sprintf "Cannot read repo status: %s" e))))
  | RepoAssociate url_or_path -> (
      let* () = send_reply "Associating repository..." in
      let* result = Repo_manager.associate ~db ~session_key ~url_or_path in
      match result with
      | Ok (local_path, msg) ->
          set_cwd local_path;
          send_reply
            (format_repo_associated ~connector ~path:local_path ~message:msg)
      | Error e -> send_reply (format_repo_error ~connector ~error:e))
  | RepoForget ->
      Repo_manager.forget_repo ~db ~session_key;
      send_reply (format_repo_forgotten ~connector)
  | RepoUpdate -> (
      let* () = send_reply "Updating repository..." in
      let* result = Repo_manager.force_update ~db ~session_key in
      match result with
      | Ok msg -> send_reply (format_repo_updated ~connector ~message:msg)
      | Error e -> send_reply (format_repo_error ~connector ~error:e))
