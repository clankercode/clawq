include Background_task_control
include Background_task_context

let ensure_roots () =
  ensure_dir (clawq_dir ());
  ensure_dir (worktree_root ());
  ensure_dir (log_root ())

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
    ?(augment_env = fun ~session_key:_ ~task_id:_ env -> env)
    ?(isolation_policy =
      Runner_isolation.{ mode = Off; backend = Sandbox.None; extra_paths = [] })
    ~db (task : task) =
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
                (* Legacy CLI-argument spawn path, hosted through the
                   session-host seam (B768). Prompt text reaches the host
                   only as a single Exec argv element. *)
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
                (* B775: hosted runners get a minimal allowlisted
                   environment and an argv-level OS sandbox; publisher and
                   cloud credentials are absent by construction. Fails
                   closed under isolation=require with no backend. *)
                match Runner_isolation.preflight isolation_policy with
                | Error err ->
                    append_log_error ~log_path err;
                    finish ~db ~id:task.id ~status:Failed ~result_preview:err;
                    Lwt.return_unit
                | Ok () -> (
                    let env =
                      if
                        isolation_policy.Runner_isolation.mode
                        <> Runner_isolation.Off
                      then Runner_isolation.minimal_env env
                      else env
                    in
                    let command =
                      match command with
                      | Process_group.Exec argv ->
                          let wrapped, applied =
                            Runner_isolation.wrap_argv isolation_policy
                              ~worktree:worktree_path ~log_path argv
                          in
                          if applied then
                            Logs.info (fun m ->
                                m
                                  "Background task %d: hosted runner sandboxed \
                                   via %s (%s)"
                                  task.id
                                  (Sandbox.backend_to_string
                                     isolation_policy.Runner_isolation.backend)
                                  (Runner_isolation.string_of_mode
                                     isolation_policy.Runner_isolation.mode));
                          Process_group.Exec wrapped
                      | Process_group.Shell _ as shell -> shell
                    in
                    write_log_preamble ~log_path ~task_id:task.id ~command;
                    let spawn_time = Unix.gettimeofday () in
                    match Session_host_registry.find task.host_kind with
                    | None ->
                        let err =
                          Session_host_registry.unknown_kind_error
                            task.host_kind
                        in
                        append_log_error ~log_path err;
                        finish ~db ~id:task.id ~status:Failed
                          ~result_preview:err;
                        Lwt.return_unit
                    | Some host -> (
                        let spec =
                          {
                            Session_host.command;
                            cwd = worktree_path;
                            env;
                            log_path;
                          }
                        in
                        let* started = host.Session_host.start spec in
                        match started with
                        | Error err ->
                            append_log_error ~log_path err;
                            finish ~db ~id:task.id ~status:Failed
                              ~result_preview:err;
                            Lwt.return_unit
                        | Ok session ->
                            let pid =
                              Option.value
                                (Session_host_direct.pid_of_session_ref session)
                                ~default:0
                            in
                            if
                              not
                                (set_running ~db ~id:task.id ~branch
                                   ~worktree_path ~log_path ~pid)
                            then begin
                              Logs.warn (fun m ->
                                  m
                                    "Background task %d: set_running failed; \
                                     cancelling host session %s/%s"
                                    task.id session.Session_host.host_kind
                                    session.Session_host.host_session_id);
                              let err_msg =
                                Printf.sprintf
                                  "set_running failed: task %d was no longer \
                                   queued (host session %s cancelled)"
                                  task.id session.Session_host.host_session_id
                              in
                              append_log_error ~log_path err_msg;
                              let* _ =
                                host.Session_host.cancel ~grace_seconds:0.0
                                  session
                              in
                              let* _ = host.Session_host.wait session in
                              finish ~db ~id:task.id ~status:Failed
                                ~result_preview:err_msg;
                              Lwt.return_unit
                            end
                            else begin
                              set_host_identity ~db ~id:task.id
                                ~host_kind:session.Session_host.host_kind
                                ~host_session_id:
                                  session.Session_host.host_session_id;
                              let* () =
                                match get_task ~db ~id:task.id with
                                | Some t -> on_task_started t
                                | None -> Lwt.return_unit
                              in
                              let* wait_result =
                                host.Session_host.wait session
                              in
                              let exit_code =
                                match wait_result with
                                | Ok code -> code
                                | Error msg ->
                                    append_log_error ~log_path msg;
                                    1
                              in
                              let () =
                                if exit_code = 0 then
                                  List.iter
                                    (fun (msg : queued_message) ->
                                      delete_queued_message ~db ~queue_id:msg.id)
                                    queued_messages
                              in
                              (* B210 watchdog: after child exits, give process
                             group 2s then kill remaining members (e.g.
                             grandchildren) *)
                              if pid > 0 then
                                Lwt.async (fun () ->
                                    let open Lwt.Syntax in
                                    let* () = Lwt_unix.sleep 2.0 in
                                    Logs.info (fun m ->
                                        m
                                          "Background task %d: child exited, \
                                           killing remaining process group \
                                           members"
                                          task.id);
                                    Process_group.signal_group pid Sys.sigkill;
                                    Lwt.return_unit);
                              let elapsed =
                                Unix.gettimeofday () -. spawn_time
                              in
                              let output =
                                let raw =
                                  read_log_tail log_path preview_limit
                                in
                                if elapsed < 5.0 then
                                  Printf.sprintf
                                    "%s\n[clawq] process exited in %.1fs" raw
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
                                 let full_output =
                                   read_log_tail log_path (64 * 1024)
                                 in
                                 match
                                   Runner_framework.extract_session_id def
                                     full_output
                                 with
                                 | Some sid ->
                                     set_runner_session_id ~db ~id:task.id
                                       ~runner_session_id:sid
                                 | None -> ());
                              ignore
                                (finalize_completed_task ~db ~id:task.id
                                   ~exit_code ~output);
                              let* () =
                                match get_task ~db ~id:task.id with
                                | Some finished_task ->
                                    on_task_finished finished_task
                                | None -> Lwt.return_unit
                              in
                              Lwt.return_unit
                            end))
              end)
        finalize)

let default_spawn_task ?augment_env ?isolation_policy ~on_task_started
    ~on_task_finished ~db task =
  spawn_task ?augment_env ?isolation_policy ~on_task_started ~on_task_finished
    ~db task

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
    ?augment_env ?isolation_policy ~on_task_finished ~db
    ?(on_task_started = fun _ -> Lwt.return_unit) () =
  start_queued_with_callback_impl ?max_running_tasks ?max_local_running_tasks
    ~spawn_task:(default_spawn_task ?augment_env ?isolation_policy)
    ~on_task_started ~on_task_finished ~db ()

let start_queued ?max_running_tasks ?max_local_running_tasks ?augment_env
    ?isolation_policy ~db () =
  start_queued_with_callback ?max_running_tasks ?max_local_running_tasks
    ?augment_env ?isolation_policy
    ~on_task_finished:(fun _ -> Lwt.return_unit)
    ~db ()

let start_queued_with_local_runner ~run_turn ?timeout_seconds ?max_running_tasks
    ?max_local_running_tasks ?augment_env ?isolation_policy ~on_task_finished
    ~on_task_started ~db () =
  let spawn ~on_task_started ~on_task_finished ~db (task : task) =
    if task.runner = Local then
      spawn_local_task ?timeout_seconds ~run_turn ~on_task_started
        ~on_task_finished ~db task
    else
      default_spawn_task ?augment_env ?isolation_policy ~on_task_started
        ~on_task_finished ~db task
  in
  start_queued_with_callback_impl ?max_running_tasks ?max_local_running_tasks
    ~spawn_task:spawn ~on_task_started ~on_task_finished ~db ()

let clear_all_tracked () = Hashtbl.clear running

(* B768: resolve the session host + durable session identity for a task.
   Rows recorded before the host seam fall back to bare-PID identity on the
   direct host (alive/dead detection only, no PID-reuse detection). *)
let host_session_of_task (task : task) :
    (Session_host.t * Session_host.session_ref) option =
  let kind =
    if String.trim task.host_kind = "" then Session_host_direct.kind
    else task.host_kind
  in
  match Session_host_registry.find kind with
  | None -> None
  | Some host ->
      let session_id =
        match task.host_session_id with
        | Some sid when String.trim sid <> "" -> Some sid
        | _ -> (
            match task.pid with
            | Some pid when pid > 0 && kind = Session_host_direct.kind ->
                Some (string_of_int pid)
            | _ -> None)
      in
      Option.map
        (fun sid ->
          ( host,
            {
              Session_host.host_kind = kind;
              host_session_id = sid;
              log_path = task.log_path;
            } ))
        session_id

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
      let health =
        match host_session_of_task task with
        | Some (host, session) -> host.Session_host.status session
        | None -> Session_host.Missing
      in
      if health <> Session_host.Live then begin
        let reason =
          match health with
          | Session_host.Stale ->
              Printf.sprintf
                "Host session %s is stale: identity no longer matches the \
                 recorded session (e.g. PID reused by another process) — use \
                 'background retry %d' to re-queue"
                (Option.value task.host_session_id ~default:"?")
                task.id
          | _ -> (
              match task.pid with
              | Some _pid when _pid <= 0 ->
                  Printf.sprintf
                    "Local in-process task %d did not survive daemon restart — \
                     use 'background retry %d' to re-queue"
                    task.id task.id
              | Some pid ->
                  Printf.sprintf
                    "Process group %d no longer alive (orphaned/crashed) — use \
                     'background retry %d' to re-queue"
                    pid task.id
              | None ->
                  Printf.sprintf
                    "No PID recorded for running task — use 'background retry \
                     %d' to re-queue"
                    task.id)
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

let readopt_running_tasks ~db ~on_task_finished =
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
  (* B768: adopt a session only when its host confirms the recorded identity
     is still live. A dead or stale (e.g. PID-reused) session is left for
     reap_dead_running_tasks to mark accurately. *)
  let adoptable =
    List.filter_map
      (fun (t : task) ->
        match host_session_of_task t with
        | Some (host, session)
          when host.Session_host.status session = Session_host.Live ->
            Some (t, host, session)
        | _ -> None)
      all_running
  in
  let count = ref 0 in
  List.iter
    (fun ((task : task), (host : Session_host.t), session) ->
      Hashtbl.replace running task.id { cancelled = ref false };
      incr count;
      Lwt.async (fun () ->
          Lwt.finalize
            (fun () ->
              let open Lwt.Syntax in
              let* wait_result = host.Session_host.wait session in
              let exit_code =
                match wait_result with Ok code -> code | Error _ -> 1
              in
              (* B210 watchdog: kill remaining process group members *)
              (match Session_host_direct.pid_of_session_ref session with
              | Some pid when pid > 0 ->
                  Lwt.async (fun () ->
                      let open Lwt.Syntax in
                      let* () = Lwt_unix.sleep 2.0 in
                      Process_group.signal_group pid Sys.sigkill;
                      Lwt.return_unit)
              | _ -> ());
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
              Lwt.return_unit)))
    adoptable;
  !count
