include Background_task_control

type context_origin = {
  connector : string option;
  workspace_id : string option;
  room_id : string option;
  requester_id : string option;
  source_message_id : string option;
  thread_id : string option;
}

let nonempty = function Some "" | None -> None | Some _ as value -> value

let make_context_origin ?connector ?workspace_id ?room_id ?requester_id
    ?source_message_id ?thread_id () =
  {
    connector;
    workspace_id;
    room_id;
    requester_id;
    source_message_id;
    thread_id;
  }

let context_origin_of_session_key key =
  let parts = String.split_on_char ':' key in
  match Room_session.parse_child_thread_key key with
  | Some child ->
      make_context_origin ~connector:child.connector ~room_id:child.room_id
        ?source_message_id:child.source_message_id ?thread_id:child.thread_id ()
  | None -> (
      match parts with
      | [ (("slack" | "discord" | "telegram") as connector); room_id ] ->
          make_context_origin ~connector ~room_id ()
      | (("slack" | "discord" | "telegram") as connector)
        :: room_id :: requester_parts ->
          make_context_origin ~connector ~room_id
            ?requester_id:(nonempty (Some (String.concat ":" requester_parts)))
            ()
      | [ "teams"; room_id ] ->
          make_context_origin ~connector:"teams" ~room_id ()
      | "teams" :: team_id :: conversation_parts when conversation_parts <> []
        ->
          make_context_origin ~connector:"teams" ~workspace_id:team_id
            ~room_id:(String.concat ":" conversation_parts)
            ()
      | _ -> (
          match Room_session.parse key with
          | None -> make_context_origin ()
          | Some session ->
              make_context_origin
                ~connector:(Room_session.channel_to_string session.channel)
                ~room_id:session.channel_id
                ?requester_id:(nonempty (Some session.sender_id))
                ()))

let room_binding_candidates ~session_key origin =
  let add seen value =
    match value with
    | None | Some "" -> seen
    | Some value when List.mem value seen -> seen
    | Some value -> value :: seen
  in
  let seen = add [] origin.room_id in
  let seen =
    match (origin.connector, origin.room_id) with
    | Some connector, Some room_id ->
        add seen (Some (connector ^ ":" ^ room_id))
    | _ -> seen
  in
  List.rev (add seen (Some session_key))

(** Derive room-origin fields from a tool invoke context's session key and the
    DB room-profile binding when available. Returns
    [(profile_id, origin_json, thread_id, requester)] suitable for passing to
    {!Background_task.enqueue}. *)
let origin_fields_from_context ~db ?context () =
  let session_key =
    match context with
    | Some (c : Tool.invoke_context) -> c.session_key
    | None -> None
  in
  match session_key with
  | None -> (None, None, None, None)
  | Some key ->
      let origin_fields = context_origin_of_session_key key in
      let profile_id =
        room_binding_candidates ~session_key:key origin_fields
        |> List.find_map (fun room_id ->
            match Memory.get_room_profile_binding ~db ~room_id with
            | Some b -> Some b.profile_id
            | None -> None)
      in
      let origin =
        Room_origin.make ?connector:origin_fields.connector
          ?workspace_id:origin_fields.workspace_id
          ?room_id:origin_fields.room_id
          ?requester_id:origin_fields.requester_id
          ?source_message_id:origin_fields.source_message_id
          ?thread_id:origin_fields.thread_id ?profile_id ()
      in
      let origin_json =
        if Room_origin.is_empty origin then None
        else Some (Room_origin.to_compact_json_string origin)
      in
      let requester =
        match Room_origin.display_summary origin with
        | s when s <> "CLI room=- requester=-" -> Some s
        | _ -> None
      in
      (profile_id, origin_json, origin_fields.thread_id, requester)

let ensure_roots () =
  ensure_dir (clawq_dir ());
  ensure_dir (worktree_root ());
  ensure_dir (log_root ())

let routing_from_context ?context ?notify_cfg () =
  let session_key =
    match context with
    | Some c -> c.Tool.session_key
    | None ->
        let value =
          try Some (Sys.getenv "CLAWQ_SESSION_ID") with Not_found -> None
        in
        Option.bind value (fun raw ->
            let trimmed = String.trim raw in
            if trimmed = "" then None else Some trimmed)
  in
  match session_key with
  | Some key -> (
      match Restart_notify.parse_channel_from_key key with
      | Some (channel, channel_id) ->
          (session_key, Some channel, Some channel_id)
      | None -> (session_key, None, None))
  | None -> (
      match notify_cfg with
      | Some notify ->
          let notify : Runtime_config.notify_config = notify in
          (None, Some notify.channel, Some notify.target)
      | None -> (None, None, None))

let build_delegate_prompt ~automerge:_ ~goal =
  let commit_line =
    "- CRITICAL: You MUST `git add` and `git commit` all changes before \
     reporting completion. Verify with `git status` that the worktree is \
     clean. Tasks with uncommitted changes are marked as dirty-worktree \
     failures regardless of exit code."
  in
  String.concat "\n"
    [
      "You are a delegated background coding agent running in the target \
       directory.";
      "";
      "Goal:";
      goal;
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

let delegate_enqueue ?context ?notify_cfg ?(check_available = true)
    ?(automerge = true) ?(use_worktree = true) ?(acp = false)
    ?(allow_claude = true) ?follow_up_prompt ~db ?preferred_runner ?model
    ?repo_path ?branch ?access_snapshot_id ~default_repo_path ~goal () =
  let chosen_repo_path =
    match repo_path with
    | Some path when String.trim path <> "" -> path
    | _ -> default_repo_path
  in
  if String.trim chosen_repo_path = "" then
    Error "Could not determine a repository path for delegation"
  else
    (* B649/B651: when use_worktree=true the harvest step requires the
       working path to be a git repo. Resolve symlinks so a workspace
       symlinked to a real repo is accepted; reject non-git paths upfront
       with a clear error so the task is not enqueued at all (instead of
       running and reporting status=dirty-worktree at the end). *)
    let chosen_repo_path =
      try Unix.realpath chosen_repo_path with _ -> chosen_repo_path
    in
    let validation =
      if use_worktree then validate_repo_path ~require_git:true chosen_repo_path
      else validate_workspace_path chosen_repo_path
    in
    match validation with
    | Error msg ->
        Error
          (Printf.sprintf
             "%s\n\
              Delegate with use_worktree=true requires a git repository; pass \
              use_worktree=false to run in a plain workspace, or point \
              repo_path at an actual git checkout."
             msg)
    | Ok () -> (
        match
          resolve_runner ~check_available ?preferred:preferred_runner
            ~allow_claude ()
        with
        | Error _ as err -> err
        | Ok (runner, auto_model) -> (
            let effective_model =
              match model with Some _ -> model | None -> auto_model
            in
            let prompt = build_delegate_prompt ~automerge ~goal in
            let session_key, channel, channel_id =
              routing_from_context ?context ?notify_cfg ()
            in
            let profile_id, origin_json, thread_id, requester =
              origin_fields_from_context ~db ?context ()
            in
            match
              enqueue ~db ~runner ?model:effective_model ~require_git:false
                ~automerge ~use_worktree ~acp ?follow_up_prompt
                ~repo_path:chosen_repo_path ~prompt ?branch ?session_key
                ?channel ?channel_id ?profile_id ?origin_json ?thread_id
                ?requester ?access_snapshot_id ()
            with
            | Ok id -> Ok (id, runner, chosen_repo_path)
            | Error _ as err -> err))

let runner_to_framework_runner (r : runner) : Runner_framework.runner =
  match r with
  | Codex -> Codex
  | Claude -> Claude
  | Kimi -> Kimi
  | Gemini -> Gemini
  | Opencode -> Opencode
  | Cursor -> Cursor
  | Local -> assert false

let invocation_to_framework (inv : invocation) : Runner_framework.invocation =
  match inv with Fresh -> Fresh | Resume s -> Resume s

let command_of_task_with_invocation task invocation =
  let def =
    Runner_framework.runner_def_of_runner
      (runner_to_framework_runner task.runner)
  in
  Runner_framework.build_command_for ~model:task.model ~prompt:task.prompt
    ~runner_session_id:task.runner_session_id def
    (invocation_to_framework invocation)

let command_argv_of_task_with_invocation task invocation =
  let def =
    Runner_framework.runner_def_of_runner
      (runner_to_framework_runner task.runner)
  in
  match invocation_to_framework invocation with
  | Fresh ->
      def.build_fresh_argv ~model:task.model ~prompt:task.prompt
        ~pre_session_id:None
  | Resume prompt ->
      let mode =
        Runner_framework.resume_mode_of
          ~runner_session_id:task.runner_session_id
      in
      def.build_resume_argv ~model:task.model ~resume_mode:mode ~prompt

let command_of_task task = command_argv_of_task_with_invocation task Fresh

let read_into_buffer_and_log ic oc buf =
  let rec loop () =
    let open Lwt.Syntax in
    let* chunk = Lwt_io.read ~count:4096 ic in
    if chunk = "" then Lwt.return_unit
    else begin
      Buffer.add_string buf chunk;
      let* () = Lwt_io.write oc chunk in
      loop ()
    end
  in
  loop ()

let run_command_capture ~cwd ~argv ~log_path =
  let proc =
    Process_group.start ~cwd ~env:(Unix.environment ())
      (Process_group.Exec argv)
  in
  let stdout_buf = Buffer.create 1024 in
  let stderr_buf = Buffer.create 256 in
  let open Lwt.Syntax in
  let* result =
    Lwt.finalize
      (fun () ->
        Lwt_io.with_file ~mode:Lwt_io.Output log_path (fun log_oc ->
            let* () =
              Lwt.join
                [
                  read_into_buffer_and_log proc.Process_group.stdout log_oc
                    stdout_buf;
                  read_into_buffer_and_log proc.Process_group.stderr log_oc
                    stderr_buf;
                ]
            in
            let* status = Process_group.wait proc.pid in
            Lwt.return
              ( proc.pid,
                exit_code_of_status status,
                Buffer.contents stdout_buf,
                Buffer.contents stderr_buf )))
      (fun () -> Process_group.close proc)
  in
  Lwt.return result

let run_simple_command ~cwd argv =
  let proc =
    Process_group.start ~cwd ~env:(Unix.environment ())
      (Process_group.Exec argv)
  in
  let open Lwt.Syntax in
  Lwt.finalize
    (fun () ->
      let* stdout, stderr =
        Lwt.both
          (Lwt_io.read proc.Process_group.stdout)
          (Lwt_io.read proc.Process_group.stderr)
      in
      let* status = Process_group.wait proc.pid in
      Lwt.return (exit_code_of_status status, stdout, stderr))
    (fun () -> Process_group.close proc)

let prepare_worktree ?(run_simple_command = run_simple_command) task =
  let log_path = Option.value task.log_path ~default:(task_log_path task.id) in
  ensure_roots ();
  ensure_parent_dir log_path;
  let open Lwt.Syntax in
  if (not task.use_worktree) || not (path_is_git_repo task.repo_path) then
    (* Non-git directory or use_worktree=false: run agent directly in the path
       without worktree isolation. *)
    Lwt.return (Ok ("", task.repo_path, log_path))
  else
    let branch =
      if task.branch <> "" then task.branch else default_branch_name task.id
    in
    let worktree_path =
      Option.value task.worktree_path ~default:(task_worktree_path task.id)
    in
    if Sys.file_exists worktree_path then
      Lwt.return (Ok (branch, worktree_path, log_path))
    else if Option.is_some task.worktree_path then
      let* exit_code, stdout, stderr =
        run_simple_command ~cwd:task.repo_path
          [|
            "git";
            "-C";
            task.repo_path;
            "worktree";
            "add";
            worktree_path;
            branch;
          |]
      in
      if exit_code = 0 then Lwt.return (Ok (branch, worktree_path, log_path))
      else
        Lwt.return
          (Error
             (Printf.sprintf "git worktree add failed (exit %d): %s%s" exit_code
                stdout stderr))
    else
      let* exit_code, stdout, stderr =
        run_simple_command ~cwd:task.repo_path
          [|
            "git";
            "-C";
            task.repo_path;
            "worktree";
            "add";
            "-b";
            branch;
            worktree_path;
          |]
      in
      if exit_code = 0 then Lwt.return (Ok (branch, worktree_path, log_path))
      else
        Lwt.return
          (Error
             (Printf.sprintf "git worktree add failed (exit %d): %s%s" exit_code
                stdout stderr))

let spawn_task ?(on_task_started = fun _ -> Lwt.return_unit)
    ?(on_task_finished = fun _ -> Lwt.return_unit)
    ?(run_simple_command = run_simple_command) ?command_override
    ?(augment_env = fun ~session_key:_ ~task_id:_ env -> env) ~db (task : task)
    =
  Hashtbl.replace running task.id { cancelled = ref false };
  Lwt.async (fun () ->
      let open Lwt.Syntax in
      let finalize () =
        Hashtbl.remove running task.id;
        Lwt.return_unit
      in
      Lwt.finalize
        (fun () ->
          let* prepared = prepare_worktree ~run_simple_command task in
          match prepared with
          | Error err ->
              finish ~db ~id:task.id ~status:Failed ~result_preview:err;
              Lwt.return_unit
          | Ok (branch, worktree_path, log_path) ->
              let task_for_command =
                {
                  task with
                  branch;
                  worktree_path = Some worktree_path;
                  log_path = Some log_path;
                }
              in
              let queued_messages = list_queued_messages ~db ~task_id:task.id in
              let invocation =
                if queued_messages <> [] || resume_supported task then
                  Resume
                    (resume_prompt_of_messages
                       (List.map
                          (fun (msg : queued_message) -> msg.message)
                          queued_messages))
                else Fresh
              in
              if task.acp && task.runner = Local then begin
                Logs.err (fun m ->
                    m "ACP mode is not supported with the Local runner");
                let () =
                  ignore
                    (finalize_completed_task ~db ~id:task.id ~exit_code:1
                       ~output:
                         "Error: ACP mode is not supported with the Local \
                          runner")
                in
                Lwt.return_unit
              end
              else if task.acp && command_override = None then begin
                (* ACP interactive path *)
                let acp_command =
                  Runner_framework.acp_argv_of_runner
                    (runner_to_framework_runner task.runner)
                in
                let effective_prompt =
                  match invocation with
                  | Fresh -> task_for_command.prompt
                  | Resume resume_text -> resume_text
                in
                ignore
                  (set_running ~db ~id:task.id ~branch ~worktree_path ~log_path
                     ~pid:0);
                let* () =
                  match get_task ~db ~id:task.id with
                  | Some t -> on_task_started t
                  | None -> Lwt.return_unit
                in
                let* exit_code, output =
                  Acp_client.run_task ~db ~task_id:task.id ~log_path
                    ~cwd:worktree_path ~prompt_text:effective_prompt
                    ~command:acp_command ()
                in
                let () =
                  if exit_code = 0 then
                    List.iter
                      (fun (msg : queued_message) ->
                        delete_queued_message ~db ~queue_id:msg.id)
                      queued_messages
                in
                ignore
                  (finalize_completed_task ~db ~id:task.id ~exit_code ~output);
                let* () =
                  match get_task ~db ~id:task.id with
                  | Some finished_task -> on_task_finished finished_task
                  | None -> Lwt.return_unit
                in
                Lwt.return_unit
              end
              else begin
                (* Legacy CLI-argument spawn path *)
                let command, pre_session_id =
                  match command_override with
                  | Some cmd -> (cmd, None)
                  | None ->
                      let result =
                        command_of_task_with_invocation task_for_command
                          invocation
                      in
                      ( Process_group.Exec result.Runner_framework.argv,
                        result.Runner_framework.pre_generated_session_id )
                in
                (match pre_session_id with
                | Some sid ->
                    set_runner_session_id ~db ~id:task.id ~runner_session_id:sid
                | None -> ());
                let base_env =
                  Runtime_config.augment_env_path (Unix.environment ())
                in
                let env =
                  match task.session_key with
                  | Some sk ->
                      augment_env ~session_key:sk ~task_id:task.id base_env
                  | None -> base_env
                in
                write_log_preamble ~log_path ~task_id:task.id ~command;
                let spawn_time = Unix.gettimeofday () in
                let proc =
                  Process_group.start_to_file ~cwd:worktree_path ~env ~log_path
                    command
                in
                let pid = proc.file_pid in
                if
                  not
                    (set_running ~db ~id:task.id ~branch ~worktree_path
                       ~log_path ~pid)
                then begin
                  Logs.warn (fun m ->
                      m "Background task %d: set_running failed; killing pid %d"
                        task.id pid);
                  let err_msg =
                    Printf.sprintf
                      "set_running failed: task %d was no longer queued (pid \
                       %d killed)"
                      task.id pid
                  in
                  append_log_error ~log_path err_msg;
                  let* () = Process_group.terminate_immediately pid in
                  let* _ = Process_group.wait pid in
                  finish ~db ~id:task.id ~status:Failed ~result_preview:err_msg;
                  Lwt.return_unit
                end
                else
                  let* () =
                    match get_task ~db ~id:task.id with
                    | Some t -> on_task_started t
                    | None -> Lwt.return_unit
                  in
                  let* status = Process_group.wait pid in
                  let exit_code = exit_code_of_status status in
                  let () =
                    if exit_code = 0 then
                      List.iter
                        (fun (msg : queued_message) ->
                          delete_queued_message ~db ~queue_id:msg.id)
                        queued_messages
                  in
                  (* B210 watchdog: after child exits, give process group
                     2s then kill remaining members (e.g. grandchildren) *)
                  Lwt.async (fun () ->
                      let open Lwt.Syntax in
                      let* () = Lwt_unix.sleep 2.0 in
                      Logs.info (fun m ->
                          m
                            "Background task %d: child exited, killing \
                             remaining process group members"
                            task.id);
                      Process_group.signal_group pid Sys.sigkill;
                      Lwt.return_unit);
                  let elapsed = Unix.gettimeofday () -. spawn_time in
                  let output =
                    let raw = read_log_tail log_path preview_limit in
                    if elapsed < 5.0 then
                      Printf.sprintf "%s\n[clawq] process exited in %.1fs" raw
                        elapsed
                    else raw
                  in
                  (* Extract runner session ID from log if not set *)
                  (let current =
                     match get_task ~db ~id:task.id with
                     | Some t -> t.runner_session_id
                     | None -> None
                   in
                   if current = None then
                     let def =
                       Runner_framework.runner_def_of_runner
                         (runner_to_framework_runner task.runner)
                     in
                     let full_output = read_log_tail log_path (64 * 1024) in
                     match
                       Runner_framework.extract_session_id def full_output
                     with
                     | Some sid ->
                         set_runner_session_id ~db ~id:task.id
                           ~runner_session_id:sid
                     | None -> ());
                  let exit_code = exit_code_of_status status in
                  ignore
                    (finalize_completed_task ~db ~id:task.id ~exit_code ~output);
                  let* () =
                    match get_task ~db ~id:task.id with
                    | Some finished_task -> on_task_finished finished_task
                    | None -> Lwt.return_unit
                  in
                  Lwt.return_unit
              end)
        finalize)

let default_spawn_task ?augment_env ~on_task_started ~on_task_finished ~db task
    =
  spawn_task ?augment_env ~on_task_started ~on_task_finished ~db task

let local_task_timeout_seconds = Background_task_local.timeout_seconds_default

let local_task_deps : Background_task_local.deps =
  {
    prepare_worktree = (fun task -> prepare_worktree task);
    finish;
    get_task;
    set_running;
    list_queued_messages;
    delete_queued_message;
    resume_prompt_of_messages;
  }

let spawn_local_task ?timeout_seconds ~run_turn ~on_task_started
    ~on_task_finished ~db task =
  Background_task_local.spawn ?timeout_seconds local_task_deps ~run_turn
    ~on_task_started ~on_task_finished ~db task

let rec take n = function
  | [] -> []
  | _ when n <= 0 -> []
  | x :: xs -> x :: take (n - 1) xs

let available_worker_slots ?max_running_tasks (tasks : task list) =
  match max_running_tasks with
  | None -> None
  | Some max_running_tasks ->
      let running_count =
        List.fold_left
          (fun acc (task : task) ->
            if task.status = Running then acc + 1 else acc)
          0 tasks
      in
      Some (max 0 (max max_running_tasks 0 - running_count))

let local_worker_slots ?max_local_running_tasks (tasks : task list) =
  match max_local_running_tasks with
  | None -> None
  | Some max_local_running_tasks ->
      let running_count =
        List.fold_left
          (fun acc (task : task) ->
            if task.runner = Local && task.status = Running then acc + 1
            else acc)
          0 tasks
      in
      Some (max 0 (max max_local_running_tasks 0 - running_count))

let queued_tasks_ready_to_start ?max_running_tasks ?max_local_running_tasks
    (tasks : task list) : task list =
  let queued =
    List.filter
      (fun (task : task) ->
        task.status = Queued && not (Hashtbl.mem running task.id))
      tasks
  in
  let queued =
    match local_worker_slots ?max_local_running_tasks tasks with
    | None -> queued
    | Some local_slots ->
        let remaining = ref local_slots in
        List.filter
          (fun (task : task) ->
            if task.runner <> Local then true
            else if !remaining > 0 then begin
              decr remaining;
              true
            end
            else false)
          queued
  in
  match available_worker_slots ?max_running_tasks tasks with
  | None -> queued
  | Some slots -> take slots queued

let start_queued_with_callback_impl ?max_running_tasks ?max_local_running_tasks
    ~spawn_task ~on_task_started ~on_task_finished ~db () =
  let queued =
    queued_tasks_ready_to_start ?max_running_tasks ?max_local_running_tasks
      (list_tasks ~db)
  in
  List.iter (spawn_task ~on_task_started ~on_task_finished ~db) queued

let start_queued_with_callback ?max_running_tasks ?max_local_running_tasks
    ?augment_env ~on_task_finished ~db
    ?(on_task_started = fun _ -> Lwt.return_unit) () =
  start_queued_with_callback_impl ?max_running_tasks ?max_local_running_tasks
    ~spawn_task:(default_spawn_task ?augment_env)
    ~on_task_started ~on_task_finished ~db ()

let start_queued ?max_running_tasks ?max_local_running_tasks ?augment_env ~db ()
    =
  start_queued_with_callback ?max_running_tasks ?max_local_running_tasks
    ?augment_env
    ~on_task_finished:(fun _ -> Lwt.return_unit)
    ~db ()

let start_queued_with_local_runner ~run_turn ?timeout_seconds ?max_running_tasks
    ?max_local_running_tasks ?augment_env ~on_task_finished ~on_task_started ~db
    () =
  let spawn ~on_task_started ~on_task_finished ~db (task : task) =
    if task.runner = Local then
      spawn_local_task ?timeout_seconds ~run_turn ~on_task_started
        ~on_task_finished ~db task
    else
      default_spawn_task ?augment_env ~on_task_started ~on_task_finished ~db
        task
  in
  start_queued_with_callback_impl ?max_running_tasks ?max_local_running_tasks
    ~spawn_task:spawn ~on_task_started ~on_task_finished ~db ()

let clear_all_tracked () = Hashtbl.clear running

let reap_dead_running_tasks ~db ~on_task_finished =
  let running_in_db =
    List.filter
      (fun (t : task) -> t.status = Running && not (Hashtbl.mem running t.id))
      (list_tasks ~db)
  in
  let count = ref 0 in
  List.iter
    (fun task ->
      let pid_alive =
        match task.pid with
        | Some pid when pid > 0 -> Process_group.group_alive pid
        | _ -> false
      in
      if not pid_alive then begin
        let reason =
          match task.pid with
          | Some _pid when _pid <= 0 ->
              Printf.sprintf
                "Local in-process task %d did not survive daemon restart — use \
                 'background retry %d' to re-queue"
                task.id task.id
          | Some pid ->
              Printf.sprintf
                "Process group %d no longer alive (orphaned/crashed) — use \
                 'background retry %d' to re-queue"
                pid task.id
          | None ->
              Printf.sprintf
                "No PID recorded for running task — use 'background retry %d' \
                 to re-queue"
                task.id
        in
        Logs.warn (fun m ->
            m "Reaping stale background task %d: %s" task.id reason);
        finish ~db ~id:task.id ~status:Failed ~result_preview:reason;
        incr count;
        Lwt.async (fun () ->
            match get_task ~db ~id:task.id with
            | Some t -> on_task_finished t
            | None -> Lwt.return_unit)
      end)
    running_in_db;
  !count

(** Derive the default repo path for room-launched background tasks. Uses the
    room workspace directory when available, falling back to the configured
    workspace root. *)
let room_default_repo_path room_id =
  Room_workspace.workspace_path ~create:true room_id

(** Launch a room background task under the room's profile policy. Uses child
    room session context, room CWD, profile_id, origin metadata, and durable
    queue semantics. Launch errors are returned so the caller can record them as
    task-visible failures.

    @param goal The prompt/goal for the background task.
    @param preferred_runner Optional runner preference (e.g. [Some Local]).
    @param agent_name Optional agent template name.
    @param use_worktree
      Whether to create a git worktree (default false for room workspaces which
      are plain directories).

    Returns [Ok bg_task_id] on success or [Error msg] on failure. *)
let launch_room_bg_task ~db ~session_key ~connector ~room_id ~requester_id ~goal
    ?preferred_runner ?agent_name ?thread_id ?model_override ?notify_cfg
    ?(use_worktree = false) ?access_snapshot_id () =
  let profile_id =
    match Memory.get_room_profile_binding ~db ~room_id with
    | Some b -> Some b.profile_id
    | None -> None
  in
  let origin =
    Room_origin.make ~connector ~room_id ~requester_id ?profile_id ()
  in
  let origin_json =
    if Room_origin.is_empty origin then None
    else Some (Room_origin.to_compact_json_string origin)
  in
  let requester = Some requester_id in
  let default_repo_path = room_default_repo_path room_id in
  match preferred_runner with
  | Some Local -> (
      (* Native/local runner: enqueue directly with runner=Local *)
      match
        enqueue ~db ~runner:Local ~use_worktree ~require_git:false
          ~automerge:false ~repo_path:default_repo_path ~prompt:goal ?agent_name
          ?model:model_override ~session_key ?profile_id ?origin_json ?thread_id
          ?requester ?access_snapshot_id ()
      with
      | Ok id -> Ok id
      | Error msg -> Error msg)
  | _ -> (
      (* External runner: use delegate_enqueue for auto runner selection *)
      let context : Tool.invoke_context =
        { Tool.default_context with session_key = Some session_key }
      in
      match
        delegate_enqueue ~db ~context ?notify_cfg ~use_worktree
          ~check_available:true ?preferred_runner ?model:model_override
          ?access_snapshot_id ~default_repo_path ~goal ()
      with
      | Ok (id, _runner, _repo) -> Ok id
      | Error msg -> Error msg)

let readopt_running_tasks ~db ~on_task_finished =
  let pid_or_group_alive pid =
    Process_group.group_alive pid
    ||
      try
        Unix.kill pid 0;
        true
      with Unix.Unix_error _ -> false
  in
  let all_running =
    List.filter
      (fun (t : task) -> t.status = Running && not (Hashtbl.mem running t.id))
      (list_tasks ~db)
  in
  List.iter
    (fun (t : task) ->
      match t.pid with
      | Some _pid when _pid <= 0 ->
          Logs.info (fun m ->
              m
                "Skipping readopt for local task %d (in-process tasks cannot \
                 survive daemon restart)"
                t.id)
      | _ -> ())
    all_running;
  let orphaned =
    List.filter
      (fun (t : task) ->
        match t.pid with
        | Some pid when pid > 0 -> pid_or_group_alive pid
        | _ -> false)
      all_running
  in
  let count = ref 0 in
  List.iter
    (fun task ->
      match task.pid with
      | Some pid ->
          Hashtbl.replace running task.id { cancelled = ref false };
          incr count;
          Lwt.async (fun () ->
              Lwt.finalize
                (fun () ->
                  let open Lwt.Syntax in
                  (* The readopted process may not be our child (reparented to
                     init after daemon restart), so waitpid can fail with
                     ECHILD. Fall back to polling group_alive in that case. *)
                  let* exit_code =
                    Lwt.catch
                      (fun () ->
                        let* status = Lwt_unix.waitpid [] pid in
                        Lwt.return (exit_code_of_status (snd status)))
                      (function
                        | Unix.Unix_error (Unix.ECHILD, _, _) ->
                            (* ECHILD means the process is not our child
                               (likely reparented to init after daemon
                               restart). Poll group_alive: once the process
                               group is gone, return 0 (clean exit) since we
                               cannot inspect the real exit status. Return 1
                               only if we time out while still alive. *)
                            let deadline = Unix.gettimeofday () +. 300.0 in
                            let rec poll () =
                              if not (Process_group.group_alive pid) then
                                Lwt.return 0
                              else if Unix.gettimeofday () >= deadline then
                                Lwt.return 1
                              else
                                let* () = Lwt_unix.sleep 5.0 in
                                poll ()
                            in
                            poll ()
                        | exn -> Lwt.fail exn)
                  in
                  (* B210 watchdog: kill remaining process group members *)
                  Lwt.async (fun () ->
                      let open Lwt.Syntax in
                      let* () = Lwt_unix.sleep 2.0 in
                      Process_group.signal_group pid Sys.sigkill;
                      Lwt.return_unit);
                  let output =
                    match task.log_path with
                    | Some path -> read_log_tail path preview_limit
                    | None -> ""
                  in
                  ignore
                    (finalize_completed_task ~db ~id:task.id ~exit_code ~output);
                  match get_task ~db ~id:task.id with
                  | Some t -> on_task_finished t
                  | None -> Lwt.return_unit)
                (fun () ->
                  Hashtbl.remove running task.id;
                  Lwt.return_unit))
      | None -> ())
    orphaned;
  !count
