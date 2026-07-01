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
    ?repo_path ?branch ?access_snapshot_id ?session_record_id ?agent_name
    ~default_repo_path ~goal () =
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
                ?requester ?access_snapshot_id ?agent_name ()
            with
            | Ok id ->
                (* Create initial checklist item for room-origin tasks *)
                if Option.is_some origin_json then begin
                  let item =
                    Room_progress_checklist.append ~db ~task_id:id
                      ~title:"Task accepted" ?session_record_id ()
                  in
                  ignore
                    (Room_progress_checklist.update_state ~db ~id:item.id
                       ~state:Current ())
                end;
                Ok (id, runner, chosen_repo_path)
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
      (fun (t : task) ->
        t.status = Running
        && (not (Hashtbl.mem running t.id))
        && t.runner <> Local)
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

(** B736: Re-enqueue stale Local tasks after a daemon restart.

    Scans [background_task_db] for rows with [runner = Local] and
    [status = Running] that are no longer tracked in the in-memory hashtable
    (i.e. the daemon was restarted). For each:

    - If [restart_policy = Fail], mark as [Failed] with
      ["Interrupted by daemon restart (restart_policy=fail)"].
    - If the task's room profile budget is exceeded, mark as [Failed] with
      ["Budget_exceeded_on_restart"].
    - If [restart_count >= max_restarts], mark as [Failed] with
      ["Max_restarts_exceeded"] and fire [on_task_finished].
    - Otherwise, transition to [Queued], increment [restart_count], and fire
      [on_task_finished] so the normal scheduler picks it up.

    Returns the count of tasks that were re-enqueued. *)
let reenqueue_stale_local_tasks ~db ~on_task_finished =
  let stale_local =
    List.filter
      (fun (t : task) ->
        t.runner = Local && t.status = Running && not (Hashtbl.mem running t.id))
      (list_tasks ~db)
  in
  let re_enqueued = ref 0 in
  List.iter
    (fun (task : task) ->
      if task.restart_policy = Fail then begin
        let reason =
          Printf.sprintf
            "Interrupted_by_restart: Local task %d interrupted by daemon \
             restart (restart_policy=fail) — use 'background retry %d' to \
             re-queue"
            task.id task.id
        in
        Logs.warn (fun m -> m "B736: %s" reason);
        finish ~db ~id:task.id ~status:Failed ~result_preview:reason;
        Lwt.async (fun () ->
            match get_task ~db ~id:task.id with
            | Some t -> on_task_finished t
            | None -> Lwt.return_unit)
      end
      else if task.restart_count >= task.max_restarts then begin
        let reason =
          Printf.sprintf
            "Max_restarts_exceeded: Local task %d exceeded max restarts (%d) — \
             giving up"
            task.id task.max_restarts
        in
        Logs.warn (fun m -> m "B736: %s" reason);
        finish ~db ~id:task.id ~status:Failed ~result_preview:reason;
        record_background_task_event_for_task ~db
          ~event_type:"background_task_max_restarts_exceeded"
          [
            ("restart_count", `Int task.restart_count);
            ("max_restarts", `Int task.max_restarts);
          ]
          task;
        Lwt.async (fun () ->
            match get_task ~db ~id:task.id with
            | Some t -> on_task_finished t
            | None -> Lwt.return_unit)
      end
      else
        let budget_ok =
          match task.profile_id with
          | None -> true
          | Some profile_id -> (
              match Room_budget.get_profile_budget ~db ~profile_id with
              | None -> true
              | Some state -> not state.limit_exceeded)
        in
        if not budget_ok then begin
          let reason =
            Printf.sprintf
              "Budget_exceeded_on_restart: Local task %d cannot be re-enqueued \
               — room budget exceeded (profile_id=%d)"
              task.id
              (Option.value task.profile_id ~default:0)
          in
          Logs.warn (fun m -> m "B736: %s" reason);
          finish ~db ~id:task.id ~status:Failed ~result_preview:reason;
          Lwt.async (fun () ->
              match get_task ~db ~id:task.id with
              | Some t -> on_task_finished t
              | None -> Lwt.return_unit)
        end
        else begin
          let new_count = task.restart_count + 1 in
          let sql =
            "UPDATE background_tasks SET status = 'queued', restart_count = ?, \
             pid = NULL, started_at = NULL, finished_at = NULL WHERE id = ?"
          in
          let stmt = Sqlite3.prepare db sql in
          Fun.protect
            ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
            (fun () ->
              ignore
                (Sqlite3.bind stmt 1
                   (Sqlite3.Data.INT (Int64.of_int new_count)));
              ignore
                (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int task.id)));
              ignore (Sqlite3.step stmt));
          Logs.info (fun m ->
              m
                "B736: Re-enqueued Local task %d after daemon restart \
                 (restart_count=%d/%d)"
                task.id new_count task.max_restarts);
          record_background_task_event_for_task ~db
            ~event_type:"background_task_re_enqueued"
            [
              ("restart_count", `Int new_count);
              ("max_restarts", `Int task.max_restarts);
              ("reason", `String "daemon_restart");
            ]
            task;
          incr re_enqueued;
          (* Wake the scheduler so the re-enqueued task is picked up *)
          signal_enqueue ()
        end)
    stale_local;
  !re_enqueued

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
let check_blocked_repo_grants ~(config : Runtime_config.t) ~session_key
    ~requester_id ~room_id () : (unit, string) result =
  let access =
    Runtime_config.resolve_effective_access config ~session_key
      ?room_profile:None ()
  in
  if access.blocked_repo_grants = [] then Ok ()
  else
    let blocked_repos =
      access.blocked_repo_grants
      |> List.filter_map (fun (item : Runtime_config.effective_access_item) ->
          match Runtime_config.repo_grant_of_json_string item.value with
          | Some rg -> Some rg.repo
          | None -> None)
    in
    let repo_list = String.concat ", " blocked_repos in
    Logs.warn (fun m ->
        m "Room bg task denied: %d blocked repo grant(s) for %s in room %s"
          (List.length access.blocked_repo_grants)
          requester_id room_id);
    Error
      (Printf.sprintf
         "Access denied: %d repo grant(s) blocked by security policy: %s. \
          Adjust codebase_grants or security.allowed_cwd_patterns to include \
          these repositories."
         (List.length access.blocked_repo_grants)
         repo_list)

let launch_room_bg_task ~db ~session_key ~connector ~room_id ~requester_id ~goal
    ?preferred_runner ?agent_name ?thread_id ?model_override ?notify_cfg
    ?(use_worktree = false) ?access_snapshot_id ?config () =
  let ( let* ) = Result.bind in
  let profile_id =
    match Memory.get_room_profile_binding ~db ~room_id with
    | Some b -> Some b.profile_id
    | None -> None
  in
  (* Auto-create an access snapshot when config is available and no snapshot ID
     was explicitly provided. This captures the effective access policy at task
     launch time, so the background task inherits the room's repo grants and
     other access rights. *)
  let profile_id_str = Option.map string_of_int profile_id in
  let effective_snapshot_id =
    match (access_snapshot_id, config) with
    | Some id, _ -> Some id
    | None, Some cfg ->
        Some
          (Access_snapshot.record_for_work ~db ~config:cfg
             ~work_type:Access_snapshot.Background_task ~session_key ~room_id
             ?profile_id:profile_id_str ())
    | None, None -> None
  in
  (* Enforce repo grants: deny when blocked by security policy. *)
  let* () =
    match config with
    | Some cfg ->
        check_blocked_repo_grants ~config:cfg ~session_key ~requester_id
          ~room_id ()
    | None -> Ok ()
  in
  let origin =
    Room_origin.make ~connector ~room_id ~requester_id ?thread_id ?profile_id ()
  in
  let origin_json =
    if Room_origin.is_empty origin then None
    else Some (Room_origin.to_compact_json_string origin)
  in
  (* Create room session record when config and snapshot are available. *)
  let session_record_id =
    match (config, effective_snapshot_id) with
    | Some cfg, Some snap_id -> (
        try
          let record =
            Room_session_record.assemble_and_persist ~db ~config:cfg
              ~access_snapshot_id:snap_id ~origin ~session_key ~room_id ()
          in
          Some record.id
        with _ ->
          (* Table may not exist in test/minimal contexts *)
          None)
    | _ -> None
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
          ?requester ?access_snapshot_id:effective_snapshot_id ()
      with
      | Ok id ->
          (* Create initial checklist item for room-origin tasks *)
          if Option.is_some origin_json then begin
            let item =
              Room_progress_checklist.append ~db ~task_id:id
                ~title:"Task accepted" ?session_record_id ()
            in
            ignore
              (Room_progress_checklist.update_state ~db ~id:item.id
                 ~state:Current ())
          end;
          Ok id
      | Error msg -> Error msg)
  | _ -> (
      (* External runner: use delegate_enqueue for auto runner selection *)
      let context : Tool.invoke_context =
        { Tool.default_context with session_key = Some session_key }
      in
      match
        delegate_enqueue ~db ~context ?notify_cfg ~use_worktree
          ~check_available:true ?preferred_runner ?model:model_override
          ?access_snapshot_id:effective_snapshot_id ?session_record_id
          ?agent_name ~default_repo_path ~goal ()
      with
      | Ok (id, _runner, _repo) -> Ok id
      | Error msg -> Error msg)

(** [launch_triggered_run ~db ~config ~review_run ~room_id ~requester_id
     ~pr_title ~pr_author ~pr_body ~base_branch ~head_branch ~pr_files ()]
    launches a triggered review run as a background task under the room's
    profile policy. The run prompt includes PR metadata, changed files, the
    access snapshot, room/thread origin, and runner policy.

    Authorization is enforced by the access snapshot and blocked-repo-grant
    check inside {!launch_room_bg_task}. Unauthorized repos or blocked grants
    fail before runner spawn.

    The [room_id] is used as-is for the session key (subscriptions store
    fully-qualified room IDs like ["slack:C123"]). Profile resolution and
    workspace path are derived from [room_id] by {!launch_room_bg_task}.

    Returns [Ok task_id] on success or [Error msg] on failure. *)
let launch_triggered_run ~db ~(config : Runtime_config.t) ~review_run ~room_id
    ~requester_id ~pr_title ~pr_author ~pr_body ~base_branch ~head_branch
    ~pr_files ?agent_name () =
  let open Github_review_run in
  let base_prompt =
    build_review_prompt ~repo:review_run.repo ~pr_number:review_run.pr_number
      ~pr_title ~pr_author ~pr_body ~base_branch ~head_branch
      ~head_sha:review_run.head_sha ~pr_files ~run_kind:review_run.run_kind
      ~trigger_source:review_run.trigger_source ()
  in
  (* Enrich prompt with access snapshot, room origin, budget, and runner
     policy context. This gives the background task visibility into the
     security constraints it operates under. *)
  let access =
    Runtime_config.resolve_effective_access config ~session_key:room_id ()
  in
  let origin = Room_origin.make ~room_id ~requester_id () in
  let enrichment_buf = Buffer.create 512 in
  let blocked_count = List.length access.blocked_repo_grants in
  let snap_id =
    Access_snapshot.record_for_work ~db ~config
      ~work_type:Access_snapshot.Background_task ~session_key:room_id ()
  in
  Buffer.add_string enrichment_buf "\n## Access Snapshot\n";
  Buffer.add_string enrichment_buf (Printf.sprintf "Snapshot ID: %s\n" snap_id);
  if access.allowed_tools <> [] then
    Buffer.add_string enrichment_buf
      (Printf.sprintf "Allowed tools: %s\n"
         (String.concat ", "
            (List.map
               (fun (i : Runtime_config.effective_access_item) -> i.value)
               access.allowed_tools)));
  if access.denied_tools <> [] then
    Buffer.add_string enrichment_buf
      (Printf.sprintf "Denied tools: %s\n"
         (String.concat ", "
            (List.map
               (fun (i : Runtime_config.effective_access_item) -> i.value)
               access.denied_tools)));
  if access.repo_grants <> [] then
    Buffer.add_string enrichment_buf
      (Printf.sprintf "Repo grants: %s\n"
         (String.concat ", "
            (List.map
               (fun (i : Runtime_config.effective_access_item) -> i.value)
               access.repo_grants)));
  if blocked_count > 0 then
    Buffer.add_string enrichment_buf
      (Printf.sprintf "Blocked repo grants: %d\n" blocked_count);
  Buffer.add_string enrichment_buf "\n## Room Origin\n";
  Buffer.add_string enrichment_buf
    (Printf.sprintf "%s\n" (Room_origin.display_summary origin));
  (match access.budget_refs with
  | [] -> ()
  | refs ->
      Buffer.add_string enrichment_buf "\n## Budget\n";
      Buffer.add_string enrichment_buf
        (Printf.sprintf "Budget refs: %s\n"
           (String.concat ", "
              (List.map
                 (fun (i : Runtime_config.effective_access_item) -> i.value)
                 refs))));
  Buffer.add_string enrichment_buf "\n## Runner Policy\n";
  if access.mcp_servers <> [] then
    Buffer.add_string enrichment_buf
      (Printf.sprintf "MCP servers: %s\n"
         (String.concat ", "
            (List.map
               (fun (i : Runtime_config.effective_access_item) -> i.value)
               access.mcp_servers)));
  if access.skills <> [] then
    Buffer.add_string enrichment_buf
      (Printf.sprintf "Skills: %s\n"
         (String.concat ", "
            (List.map
               (fun (i : Runtime_config.effective_access_item) -> i.value)
               access.skills)));
  Buffer.add_string enrichment_buf
    (Printf.sprintf "Egress rules: %d\n" (List.length access.egress_rules));
  let prompt = base_prompt ^ Buffer.contents enrichment_buf in
  (* Authorization: verify the target repo is granted by the room's access
     policy with at least a read capability. Empty repo_grants means no repos
     are granted, so the launch is denied. *)
  let repo_granted =
    access.repo_grants <> []
    && List.exists
         (fun (item : Runtime_config.effective_access_item) ->
           match Runtime_config.repo_grant_of_json_string item.value with
           | Some rg ->
               let has_read =
                 rg.capabilities = []
                 || List.mem Runtime_config.Read rg.capabilities
               in
               if not has_read then false
               else
                 let pattern = String.lowercase_ascii rg.repo in
                 let target = String.lowercase_ascii review_run.repo in
                 pattern = target
                 || String.length pattern > 1
                    && pattern.[String.length pattern - 1] = '*'
                    && String.starts_with
                         ~prefix:
                           (String.sub pattern 0 (String.length pattern - 1))
                         target
           | None -> false)
         access.repo_grants
  in
  if not repo_granted then (
    let msg =
      Printf.sprintf
        "Access denied: repo %s is not in the room's granted repositories"
        review_run.repo
    in
    (* Only mark the review run as failed if no room has launched it yet *)
    let is_pending =
      match find_by_id ~db ~id:review_run.id with
      | Some r -> r.status = Pending
      | None -> false
    in
    if is_pending then
      ignore (set_failed ~db ~id:review_run.id ~error_message:msg);
    Logs.warn (fun m ->
        m "Review run %d denied for room %s: %s" review_run.id room_id msg);
    Error msg)
  else
    match
      launch_room_bg_task ~db ~session_key:room_id ~connector:"" ~room_id
        ~requester_id ~goal:prompt ?agent_name ~use_worktree:false
        ~access_snapshot_id:snap_id ~config ()
    with
    | Ok task_id ->
        (* set_running only succeeds when status is 'pending'. For multi-room
         subscriptions, the first launch sets running; subsequent launches
         succeed but cannot attach to the same review_run row. This is
         expected: each room gets its own background task. *)
        let attached = set_running ~db ~id:review_run.id ~task_id in
        if not attached then
          Logs.info (fun m ->
              m
                "Review run %d already launched; bg task %d for room %s is an \
                 additional launch"
                review_run.id task_id room_id)
        else
          Logs.info (fun m ->
              m "Review run %d launched as bg task %d for %s PR #%d in room %s"
                review_run.id task_id review_run.repo review_run.pr_number
                room_id);
        Ok task_id
    | Error msg ->
        (* Only mark the review run as failed if it is still pending (no room
         has launched it yet). If another room already launched, keep the
         running status so the overall review is not aborted. *)
        let is_pending =
          match find_by_id ~db ~id:review_run.id with
          | Some r -> r.status = Pending
          | None -> false
        in
        if is_pending then
          ignore
            (set_failed ~db ~id:review_run.id
               ~error_message:("Launch failed: " ^ msg));
        Logs.warn (fun m ->
            m "Review run %d launch failed for %s PR #%d in room %s: %s"
              review_run.id review_run.repo review_run.pr_number room_id msg);
        Error msg

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

(** {1 Workflow run triggers} *)

(** [trigger_workflow_from_room_command ~db ~config ~pipeline_name ~inputs
     ~room_id ~requester_id ()] triggers a workflow run from a room command.
    Validates the pipeline, creates a workflow run record, and launches it as a
    background task with room progress reporting.

    Returns [Ok (workflow_run, task_id)] on success or [Error msg] on failure.
*)
let trigger_workflow_from_room_command ~db ~(config : Runtime_config.t)
    ~pipeline_name ~inputs ~room_id ~requester_id () =
  match Structured_pipeline.find_pipeline pipeline_name with
  | None ->
      Error
        (Printf.sprintf
           "Pipeline \"%s\" not found. Use 'clawq pipeline list' to see \
            available pipelines."
           pipeline_name)
  | Some pipeline -> (
      match
        Workflow_run_trigger.validate_and_resolve_inputs ~pipeline ~inputs ()
      with
      | Error msg -> Error msg
      | Ok effective_inputs -> (
          let run =
            Workflow_run_trigger.create ~db ~pipeline_name:pipeline.name
              ~pipeline_version:pipeline.version ~inputs:effective_inputs
              ~trigger_source:
                (Workflow_run_trigger.Room_command { room_id; requester_id })
              ~room_id ~requester_id ()
          in
          let prompt =
            Workflow_run_trigger.build_workflow_prompt ~pipeline
              ~inputs:effective_inputs ()
          in
          match
            launch_room_bg_task ~db ~session_key:room_id ~connector:"" ~room_id
              ~requester_id ~goal:prompt ~use_worktree:false ~config ()
          with
          | Ok task_id ->
              let attached =
                Workflow_run_trigger.set_running ~db ~id:run.id ~task_id
              in
              if not attached then
                Logs.warn (fun m ->
                    m "Workflow run %d: set_running failed" run.id);
              Logs.info (fun m ->
                  m "Workflow run %d launched as bg task %d for pipeline %s"
                    run.id task_id pipeline_name);
              Ok (run, task_id)
          | Error msg ->
              ignore
                (Workflow_run_trigger.set_failed ~db ~id:run.id
                   ~error_message:("Launch failed: " ^ msg));
              Error msg))

(** [trigger_security_review_workflow ~db ~config ~review_run ~room_id
     ~requester_id ~pr_title ~pr_author ~pr_body ~base_branch ~head_branch
     ~pr_files ()] triggers a security review run. If a "security-review"
    pipeline is configured, uses it for multi-step analysis. Otherwise falls
    back to the single-prompt security scan.

    Returns [Ok task_id] on success or [Error msg] on failure. *)
let trigger_security_review_workflow ~db ~(config : Runtime_config.t)
    ~(review_run : Github_review_run.review_run) ~room_id ~requester_id
    ~pr_title ~pr_author ~pr_body ~base_branch ~head_branch ~pr_files () =
  match Structured_pipeline.find_pipeline "security-review" with
  | Some pipeline -> (
      let inputs =
        [
          ("repo", review_run.repo);
          ("pr_number", string_of_int review_run.pr_number);
          ("head_sha", review_run.head_sha);
          ("pr_title", pr_title);
          ("pr_author", pr_author);
          ("base_branch", base_branch);
          ("head_branch", head_branch);
        ]
      in
      match
        Workflow_run_trigger.validate_and_resolve_inputs ~pipeline ~inputs ()
      with
      | Error msg -> Error msg
      | Ok effective_inputs -> (
          let run =
            Workflow_run_trigger.create ~db ~pipeline_name:pipeline.name
              ~pipeline_version:pipeline.version ~inputs:effective_inputs
              ~trigger_source:
                (Workflow_run_trigger.Room_command { room_id; requester_id })
              ~room_id ~requester_id ()
          in
          let prompt =
            Workflow_run_trigger.build_workflow_prompt ~pipeline
              ~inputs:effective_inputs ()
            ^ "\n\n"
            ^ Github_review_run.build_review_prompt ~repo:review_run.repo
                ~pr_number:review_run.pr_number ~pr_title ~pr_author ~pr_body
                ~base_branch ~head_branch ~head_sha:review_run.head_sha
                ~pr_files ~run_kind:review_run.run_kind
                ~trigger_source:review_run.trigger_source ()
          in
          let snap_id =
            Access_snapshot.record_for_work ~db ~config
              ~work_type:Access_snapshot.Background_task ~session_key:room_id ()
          in
          match
            launch_room_bg_task ~db ~session_key:room_id ~connector:"" ~room_id
              ~requester_id ~goal:prompt ~use_worktree:false
              ~access_snapshot_id:snap_id ~config ()
          with
          | Ok task_id ->
              let attached =
                Workflow_run_trigger.set_running ~db ~id:run.id ~task_id
              in
              if not attached then
                Logs.warn (fun m ->
                    m "Workflow run %d: set_running failed" run.id);
              ignore
                (Github_review_run.set_running ~db ~id:review_run.id ~task_id);
              Logs.info (fun m ->
                  m
                    "Security review pipeline run %d launched as bg task %d \
                     for %s PR #%d"
                    run.id task_id review_run.repo review_run.pr_number);
              Ok task_id
          | Error msg ->
              ignore
                (Workflow_run_trigger.set_failed ~db ~id:run.id
                   ~error_message:("Launch failed: " ^ msg));
              let is_pending =
                match Github_review_run.find_by_id ~db ~id:review_run.id with
                | Some r -> r.status = Github_review_run.Pending
                | None -> false
              in
              if is_pending then
                ignore
                  (Github_review_run.set_failed ~db ~id:review_run.id
                     ~error_message:("Pipeline launch failed: " ^ msg));
              Error msg))
  | None ->
      (* No pipeline configured: fall back to single-prompt security scan *)
      launch_triggered_run ~db ~config ~review_run ~room_id ~requester_id
        ~pr_title ~pr_author ~pr_body ~base_branch ~head_branch ~pr_files ()
