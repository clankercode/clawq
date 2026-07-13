include Background_task_log

let request_resume ~message ~db ~id =
  match get_task ~db ~id with
  | None -> Error (Printf.sprintf "No background task found with id %d" id)
  | Some task when task.runner <> Local && not (resume_supported task) ->
      Error
        (Printf.sprintf
           "Task %d cannot be resumed because it does not have an isolated \
            worktree-backed session. The original worktree may have been \
            removed or is no longer a git worktree. Re-run it as a normal \
            background worktree task first."
           id)
  | Some task -> (
      let normalized_message = Option.map String.trim message in
      (match task.status with
      | Running -> (
          if task.runner = Local then
            match Hashtbl.find_opt running id with
            | Some st -> st.cancelled := true
            | None -> ()
          else
            match task.pid with
            | Some pid when pid > 0 ->
                Process_group.terminate_blocking pid;
                (* Wait briefly for the process group to fully exit before
                   requeueing, so the next scheduler tick does not race
                   against the dying process. *)
                let deadline = Unix.gettimeofday () +. 3.0 in
                while
                  Process_group.group_alive pid
                  && Unix.gettimeofday () < deadline
                do
                  Unix.sleepf 0.1
                done
            | _ -> ())
      | _ -> ());
      match normalized_message with
      | Some "" -> Error "Message must not be empty"
      | Some text -> (
          match queue_message ~db ~task_id:id ~message:text with
          | Error msg -> Error msg
          | Ok _queue_id ->
              requeue_for_resume ~db ~id
                ~result_preview:"Queued new message for resumed background task";
              Ok
                (Printf.sprintf
                   "Queued message for background task %d. The task will \
                    resume and receive it as a chat message."
                   id))
      | None ->
          requeue_for_resume ~db ~id
            ~result_preview:"Queued background task for native runner resume";
          Ok (Printf.sprintf "Queued background task %d for resume" id))

let completion_sentinel = "OK_TASK_DONE_CHECKED_REBASED_COMMITED"

let completion_pass_message () =
  String.concat "\n"
    [
      "COMPLETION VERIFICATION — Your background task has finished its primary \
       work.";
      "Before this task can be finalized, complete these steps:";
      "";
      "1. Stage and commit ALL remaining changes (if any):";
      "   git add -A && git commit -m \"<appropriate commit msg here>\"";
      "2. Rebase your branch against the master branch (not origin/master):";
      "   git rebase master";
      "3. REVIEW AND FIX — Thoroughly review all changes on this branch (git \
       diff master..HEAD). Check for:";
      "   - Correctness: logic errors, off-by-one, missing edge cases";
      "   - Quality: code style, naming, unnecessary complexity";
      "   - Completeness: missing tests, incomplete implementations, TODOs";
      "   - Safety: no secrets, no injection vulnerabilities, no regressions";
      "   Fix any issues found, commit the fixes, and re-review until clean.";
      "4. Run any relevant tests/checks to verify your work, fix any issues";
      "5. Verify the worktree is completely clean: git status";
      "6. When ALL steps are done, output exactly as the only thing on the \
       last line:";
      "   " ^ completion_sentinel;
    ]

let request_completion_pass ~db ~id =
  let message = completion_pass_message () in
  ignore (queue_message ~db ~task_id:id ~message);
  requeue_for_resume ~db ~id ~result_preview:"completion pass queued";
  set_merge_status ~db ~id ~merge_status:"completion_pass"

let cancel_with_signal ~send_signal ~db ~id
    ?(terminate_group = Process_group.terminate_blocking) () =
  match get_task ~db ~id with
  | None -> Error (Printf.sprintf "No background task found with id %d" id)
  | Some task -> (
      match task.status with
      | Cancelled -> Error (Printf.sprintf "Task %d is already cancelled" id)
      | Succeeded | Failed | DirtyWorktree ->
          Error (Printf.sprintf "Task %d is already finished" id)
      | Queued ->
          ignore
            (mark_cancelled ~db ~id
               ~result_preview:"Cancelled before execution started");
          Ok "Cancelled queued task"
      | Running -> (
          match task.pid with
          | Some pid when pid > 0 ->
              (try send_signal (-pid) Sys.sigterm with _ -> ());
              terminate_group pid;
              ignore
                (mark_cancelled ~db ~id
                   ~result_preview:"Cancellation requested for running task");
              Ok (Printf.sprintf "Sent SIGTERM to task %d (pid %d)" id pid)
          | Some _ | None ->
              (match Hashtbl.find_opt running id with
              | Some st -> st.cancelled := true
              | None -> ());
              ignore
                (mark_cancelled ~db ~id ~result_preview:"Cancelled local task");
              Ok "Cancelled running task"))

let cancel ~db ~id = cancel_with_signal ~send_signal:Unix.kill ~db ~id ()

let retry ~db ~id =
  match get_task ~db ~id with
  | None -> Error (Printf.sprintf "No background task found with id %d" id)
  | Some task when not (task.status = Failed || task.status = DirtyWorktree) ->
      Error
        (Printf.sprintf
           "Task %d has status '%s' — only failed tasks can be retried" id
           (string_of_status task.status))
  | Some task when task.retry_count >= max_retry_count ->
      Error
        (Printf.sprintf
           "Task %d has already been retried %d/%d times — maximum retries \
            exceeded"
           id task.retry_count max_retry_count)
  | Some task -> (
      let human_dispatch_guard () =
        (* As in [spawn_task], reject an upgrade-invalidated legacy task before
           consulting the optional GitHub work-item table.  Fresh/no-record
           App/PAT/unattributed tasks remain outside user-migration authority. *)
        try
          let ( let* ) = Result.bind in
          let* invalidated =
            Principal_legacy_migrate.is_job_invalidated ~db
              ~source_kind:Principal_legacy_migrate.Background_task
              ~source_id:(string_of_int id)
          in
          if invalidated then
            Error
              (Printf.sprintf
                 "legacy background task %d was invalidated during principal \
                  migration; inspect principal_legacy_invalidated_jobs and \
                  re-plan with verified identity"
                 id)
          else if not (Github_work_item.schema_exists db) then Ok ()
          else
            match Github_work_item.find_by_task ~db ~background_task_id:id with
            | None -> Ok ()
            | Some item -> (
                match item.actor_snapshot_json with
                | None -> Ok ()
                | Some _ ->
                    let* () =
                      Github_work_item.require_actor_snapshot_current ~db item
                    in
                    Principal_legacy_migrate.require_migrated_user_dispatch ~db
                      ~source_kind:Principal_legacy_migrate.Background_task
                      ~source_id:(string_of_int id))
        with exn ->
          Error
            ("could not inspect durable human attribution before retry: "
           ^ Printexc.to_string exn)
      in
      match human_dispatch_guard () with
      | Error err ->
          Error ("Retry refused for human-attributed durable work: " ^ err)
      | Ok () ->
          let new_retry_count = task.retry_count + 1 in
          let sql =
            "UPDATE background_tasks SET status = 'queued', pid = NULL, \
             result_preview = NULL, started_at = NULL, finished_at = NULL, \
             worktree_path = NULL, log_path = NULL, branch = '', merge_status \
             = NULL, retry_count = ? WHERE id = ?"
          in
          let stmt = Sqlite3.prepare db sql in
          Fun.protect
            ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
            (fun () ->
              ignore
                (Sqlite3.bind stmt 1
                   (Sqlite3.Data.INT (Int64.of_int new_retry_count)));
              ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int id)));
              match Sqlite3.step stmt with
              | Sqlite3.Rc.DONE ->
                  Ok
                    (Printf.sprintf "Re-queued task %d (retry %d/%d)" id
                       new_retry_count max_retry_count)
              | rc ->
                  Error
                    (Printf.sprintf "Failed to retry task %d: %s" id
                       (Sqlite3.Rc.to_string rc))))

type evidence = {
  original_prompt : string;
  log_tail : string option;
  worktree_diff_stat : string option;
  result_preview : string option;
  health : health;
  status : status;
}

let gather_evidence (task : task) =
  let log_tail =
    match log_excerpt ~lines:40 task with
    | Ok text -> Some text
    | Error _ -> None
  in
  let worktree_diff_stat =
    match task.worktree_path with
    | Some wt when Sys.file_exists wt && path_is_git_repo wt ->
        let cmd =
          Printf.sprintf "git -C %s diff HEAD --stat 2>&1" (Filename.quote wt)
        in
        let ic = Unix.open_process_in cmd in
        let buf = Buffer.create 256 in
        (try
           while Buffer.length buf < 3000 do
             Buffer.add_char buf (input_char ic)
           done
         with End_of_file -> ());
        ignore (Unix.close_process_in ic);
        let s = String.trim (Buffer.contents buf) in
        if s = "" then None else Some s
    | _ -> None
  in
  {
    original_prompt = task.prompt;
    log_tail;
    worktree_diff_stat;
    result_preview = task.result_preview;
    health = diagnose_health task;
    status = task.status;
  }

let build_recovery_prompt ~original_id evidence =
  let or_none = function None -> "none" | Some s -> s in
  let commit_line =
    "- CRITICAL: You MUST `git add` and `git commit` all changes before \
     reporting completion. Verify with `git status` that the worktree is \
     clean. Tasks with uncommitted changes are marked as dirty-worktree \
     failures regardless of exit code."
  in
  String.concat "\n"
    [
      Printf.sprintf
        "You are a replacement background coding agent. A previous task (id \
         %d) %s and you are continuing the work."
        original_id
        (string_of_status evidence.status);
      "";
      "Original Goal:";
      evidence.original_prompt;
      "";
      "Previous Result:";
      or_none evidence.result_preview;
      "";
      "Previous Worktree Changes (git diff --stat):";
      or_none evidence.worktree_diff_stat;
      "";
      "Previous Log Tail:";
      or_none evidence.log_tail;
      "";
      "Execution contract:";
      commit_line;
      "- Before reporting completion, rebase your branch against the master \
       branch (e.g., `git rebase master`) to ensure your changes are up to \
       date. If the rebase has conflicts, resolve straightforward ones and \
       continue.";
      "- Work only inside this directory/worktree.";
      "- Do not inspect or modify the original source repo path directly; use \
       only the files available in the current worktree.";
      "- Make the smallest focused change that completes the task well.";
      "- Run relevant verification when practical and mention what you ran.";
      "- Summarize the changes, results, and any follow-up concerns at the end.";
      "- Do not push or perform destructive git history edits.";
    ]

let recover ~db ~id ?runner ?model () =
  match get_task ~db ~id with
  | None -> Error (Printf.sprintf "No background task found with id %d" id)
  | Some task ->
      let health = diagnose_health task in
      let recoverable =
        match task.status with
        | Failed | DirtyWorktree | Cancelled -> true
        | Running -> (
            match health with
            | Stalled | Zombie | Process_missing | Startup_failed -> true
            | _ -> false)
        | Queued | Succeeded -> false
      in
      if not recoverable then
        let reason =
          match task.status with
          | Running ->
              Printf.sprintf
                "Task %d is still actively running — cancel it first or wait \
                 for it to finish"
                id
          | Succeeded ->
              Printf.sprintf "Task %d already succeeded — nothing to recover" id
          | Queued ->
              Printf.sprintf
                "Task %d is queued — cancel it first if you want to replace it"
                id
          | _ -> Printf.sprintf "Task %d cannot be recovered" id
        in
        Error reason
      else begin
        (* If running but stuck, cancel it first *)
        (match task.status with Running -> ignore (cancel ~db ~id) | _ -> ());
        let evidence = gather_evidence task in
        let prompt = build_recovery_prompt ~original_id:id evidence in
        let effective_runner =
          match runner with Some r -> r | None -> task.runner
        in
        let effective_model =
          match model with Some _ -> model | None -> task.model
        in
        match
          enqueue ~db ~runner:effective_runner ?model:effective_model
            ~require_git:false ~automerge:task.automerge
            ~use_worktree:task.use_worktree ~repo_path:task.repo_path ~prompt
            ?session_key:task.session_key ?channel:task.channel
            ?channel_id:task.channel_id ~parent_task_id:id ()
        with
        | Ok new_id ->
            set_replaced_by ~db ~id ~replaced_by_id:new_id;
            Ok (new_id, effective_runner)
        | Error msg -> Error msg
      end
