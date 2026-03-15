type merge_result =
  | Merged of { branch : string; commit_count : int }
  | Conflict of { branch : string; message : string }
  | Error of string
  | No_worktree
  | Already_merged
  | Dirty_worktree of { branch : string; details : string }

let repo_mutexes : (string, Lwt_mutex.t) Hashtbl.t = Hashtbl.create 8

let repo_mutex repo_path =
  match Hashtbl.find_opt repo_mutexes repo_path with
  | Some m -> m
  | None ->
      let m = Lwt_mutex.create () in
      Hashtbl.replace repo_mutexes repo_path m;
      m

let run_git ~cwd args =
  let argv = Array.concat [ [| "git"; "-C"; cwd |]; args ] in
  Background_task.run_simple_command ~cwd argv

let detect_target_branch ~repo_path =
  let open Lwt.Syntax in
  let* exit_code, stdout, _stderr =
    run_git ~cwd:repo_path [| "symbolic-ref"; "--short"; "HEAD" |]
  in
  if exit_code = 0 then Lwt.return (String.trim stdout) else Lwt.return "master"

let commits_ahead ~repo_path ~branch ~target =
  let open Lwt.Syntax in
  let* exit_code, stdout, _stderr =
    run_git ~cwd:repo_path
      [| "rev-list"; "--count"; Printf.sprintf "%s..%s" target branch |]
  in
  if exit_code = 0 then
    Lwt.return (try int_of_string (String.trim stdout) with _ -> 0)
  else Lwt.return 0

let rebase_onto ~worktree_path ~target =
  let open Lwt.Syntax in
  let* exit_code, _stdout, stderr =
    run_git ~cwd:worktree_path [| "rebase"; target |]
  in
  if exit_code = 0 then Lwt.return (Ok ())
  else
    let* _abort_code, _, _ =
      run_git ~cwd:worktree_path [| "rebase"; "--abort" |]
    in
    Lwt.return
      (Result.Error (Printf.sprintf "Rebase failed: %s" (String.trim stderr)))

let ff_merge ~repo_path ~branch ~target =
  let open Lwt.Syntax in
  let* exit_code_target, target_sha, _ =
    run_git ~cwd:repo_path [| "rev-parse"; target |]
  in
  let* exit_code_branch, branch_sha, _ =
    run_git ~cwd:repo_path [| "rev-parse"; branch |]
  in
  if exit_code_target <> 0 || exit_code_branch <> 0 then
    Lwt.return (Result.Error "Failed to resolve branch refs")
  else
    let target_sha = String.trim target_sha in
    let branch_sha = String.trim branch_sha in
    let* exit_code_mb, merge_base, _ =
      run_git ~cwd:repo_path [| "merge-base"; target; branch |]
    in
    let merge_base = String.trim merge_base in
    if exit_code_mb <> 0 || merge_base <> target_sha then
      Lwt.return
        (Result.Error
           (Printf.sprintf
              "Cannot fast-forward: target %s is not an ancestor of branch %s"
              target branch))
    else
      let* exit_code, _stdout, stderr =
        run_git ~cwd:repo_path
          [| "update-ref"; Printf.sprintf "refs/heads/%s" target; branch_sha |]
      in
      if exit_code = 0 then Lwt.return (Ok ())
      else
        Lwt.return
          (Result.Error
             (Printf.sprintf "Fast-forward merge failed: %s"
                (String.trim stderr)))

let cleanup ~repo_path ~worktree_path ~branch =
  let open Lwt.Syntax in
  let* _exit1, _, _ =
    run_git ~cwd:repo_path [| "worktree"; "remove"; "--force"; worktree_path |]
  in
  let* _exit2, _, _ = run_git ~cwd:repo_path [| "branch"; "-d"; branch |] in
  Lwt.return_unit

let check_dirty_worktree ~worktree_path =
  let open Lwt.Syntax in
  let* exit_code, stdout, _stderr =
    run_git ~cwd:worktree_path [| "status"; "--porcelain" |]
  in
  if exit_code <> 0 then Lwt.return (Some "unable to check worktree status")
  else
    let trimmed = String.trim stdout in
    if trimmed = "" then Lwt.return None
    else
      let first_line =
        match String.index_opt trimmed '\n' with
        | Some i -> String.sub trimmed 0 i
        | None -> trimmed
      in
      Lwt.return (Some first_line)

let merge_and_cleanup ~repo_path ~worktree_path ~branch =
  let open Lwt.Syntax in
  let* dirty = check_dirty_worktree ~worktree_path in
  match dirty with
  | Some details -> Lwt.return (Dirty_worktree { branch; details })
  | None -> (
      let* target = detect_target_branch ~repo_path in
      let* count = commits_ahead ~repo_path ~branch ~target in
      if count = 0 then
        let* () = cleanup ~repo_path ~worktree_path ~branch in
        Lwt.return Already_merged
      else
        let* rebase_result = rebase_onto ~worktree_path ~target in
        match rebase_result with
        | Result.Error msg -> Lwt.return (Conflict { branch; message = msg })
        | Ok () -> (
            let* ff_result = ff_merge ~repo_path ~branch ~target in
            match ff_result with
            | Result.Error msg -> Lwt.return (Error msg)
            | Ok () ->
                let* () = cleanup ~repo_path ~worktree_path ~branch in
                Lwt.return (Merged { branch; commit_count = count })))

let try_automerge ~db (task : Background_task.task) =
  match task.worktree_path with
  | None ->
      Background_task.set_merge_status ~db ~id:task.id ~merge_status:"error";
      Lwt.return No_worktree
  | Some worktree_path when task.branch = "" ->
      Background_task.set_merge_status ~db ~id:task.id ~merge_status:"error";
      Lwt.return (Error "no branch name recorded")
  | Some worktree_path ->
      Lwt_mutex.with_lock (repo_mutex task.repo_path) (fun () ->
          let open Lwt.Syntax in
          let* result =
            Lwt.catch
              (fun () ->
                merge_and_cleanup ~repo_path:task.repo_path ~worktree_path
                  ~branch:task.branch)
              (fun exn -> Lwt.return (Error (Printexc.to_string exn)))
          in
          let merge_status =
            match result with
            | Merged _ -> "merged"
            | Conflict _ -> "conflict"
            | Error _ -> "error"
            | No_worktree -> "error"
            | Already_merged -> "merged"
            | Dirty_worktree _ -> "dirty"
          in
          Background_task.set_merge_status ~db ~id:task.id ~merge_status;
          Lwt.return result)

let finalize_task ~db (task : Background_task.task) =
  match task.worktree_path with
  | None -> Lwt.return No_worktree
  | Some worktree_path when task.branch = "" ->
      Lwt.return (Error "no branch name recorded")
  | Some worktree_path ->
      Lwt_mutex.with_lock (repo_mutex task.repo_path) (fun () ->
          let open Lwt.Syntax in
          let* result =
            Lwt.catch
              (fun () ->
                merge_and_cleanup ~repo_path:task.repo_path ~worktree_path
                  ~branch:task.branch)
              (fun exn -> Lwt.return (Error (Printexc.to_string exn)))
          in
          let merge_status =
            match result with
            | Merged _ -> "merged"
            | Conflict _ -> "conflict"
            | Error _ -> "error"
            | No_worktree -> "error"
            | Already_merged -> "merged"
            | Dirty_worktree _ -> "dirty"
          in
          Background_task.set_merge_status ~db ~id:task.id ~merge_status;
          Lwt.return result)

let format_result = function
  | Merged { branch; commit_count } ->
      Printf.sprintf "Merged branch %s (%d commit%s) and cleaned up worktree."
        branch commit_count
        (if commit_count = 1 then "" else "s")
  | Conflict { branch; message } ->
      Printf.sprintf
        "Merge conflict on branch %s: %s\n\
         The worktree is still intact. Resolve conflicts manually and retry."
        branch message
  | Error msg -> Printf.sprintf "Merge error: %s" msg
  | No_worktree -> "No worktree found for this task."
  | Already_merged ->
      "No new commits — branch was already up to date. Cleaned up worktree."
  | Dirty_worktree { branch; details } ->
      Printf.sprintf
        "Worktree for branch %s has uncommitted changes: %s\n\
         Commit or discard the changes before finalizing."
        branch details

let finalize_tool ~db =
  {
    Tool.name = "background_finalize";
    description =
      "Finalize a completed background task by rebasing its worktree branch \
       onto the target branch, fast-forward merging, and cleaning up the \
       worktree. Use after a task succeeds but automerge was not set or \
       automerge failed with conflicts.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "id",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String "The background task ID to finalize." );
                    ] );
              ] );
          ("required", `List [ `String "id" ]);
          ("additionalProperties", `Bool false);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let id = try args |> member "id" |> to_int with _ -> -1 in
        if id < 0 then
          Lwt.return
            "Error: parameter \"id\" must be a positive integer task ID. Use \
             background_task_list to find task IDs."
        else
          match Background_task.get_task ~db ~id with
          | None ->
              Lwt.return
                (Printf.sprintf
                   "Error: no background task found with id %d. Use \
                    background_task_list to see available tasks."
                   id)
          | Some task when task.worktree_path = None ->
              Lwt.return
                (Printf.sprintf
                   "Error: task %d has no worktree — nothing to finalize." id)
          | Some task ->
              let open Lwt.Syntax in
              let* result = finalize_task ~db task in
              Lwt.return (format_result result));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }
