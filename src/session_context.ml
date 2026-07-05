open Session_types

let is_main_session_key key = key = "__main__"

let shell_visible_roots_summary ~workspace_only ~workspace ~extra_allowed_paths
    =
  if not workspace_only then
    "unrestricted host filesystem view (tool-level checks relaxed)"
  else
    let roots = workspace :: extra_allowed_paths in
    String.concat ", " (List.sort_uniq String.compare roots)

let shell_policy_summary mgr sandbox =
  let workspace_only = mgr.config.security.workspace_only in
  let allowlist = "shell allowlist + path checks" in
  let fs_policy, backend_effective, shell_is_sandboxed =
    match sandbox with
    | Some sb when workspace_only ->
        let backend = Sandbox.backend_to_string sb.Sandbox.backend in
        let policy =
          match sb.Sandbox.backend with
          | Sandbox.None ->
              "OS-level filesystem sandbox disabled; workspace boundaries are \
               enforced by tool validation only"
          | _ ->
              Printf.sprintf
                "OS-level filesystem sandbox enabled via %s with workspace \
                 isolation"
                backend
        in
        (policy, backend, sb.Sandbox.backend <> Sandbox.None)
    | Some sb ->
        ( "workspace_only disabled; shell can access the host filesystem",
          Sandbox.backend_to_string sb.Sandbox.backend,
          false )
    | None -> ("shell runtime context unavailable", "none", false)
  in
  let landlock_suffix =
    if mgr.landlock_enabled then "; landlock enabled for daemon process" else ""
  in
  ( allowlist ^ "; " ^ fs_policy ^ landlock_suffix,
    backend_effective,
    shell_is_sandboxed )

let active_background_task_summaries mgr =
  match mgr.db with
  | None -> []
  | Some db ->
      Background_task.init_schema db;
      Background_task.list_tasks ~db
      |> List.filter (fun (t : Background_task.task) ->
          match t.Background_task.status with
          | Background_task.Queued | Background_task.Running -> true
          | _ -> false)
      |> List.sort (fun (a : Background_task.task) (b : Background_task.task) ->
          compare a.Background_task.id b.Background_task.id)
      |> List.map (fun (t : Background_task.task) ->
          {
            Prompt_builder.id = t.Background_task.id;
            runner = Background_task.string_of_runner t.runner;
            repo_label = Filename.basename t.repo_path;
            branch = (if t.branch = "" then "(auto)" else t.branch);
            status = Background_task.string_of_status t.status;
            health =
              Background_task.string_of_health
                (Background_task.diagnose_health t);
            elapsed = Background_task.elapsed_string t;
          })

let runtime_context_details mgr ~agent ~key ~compacted_before_turn =
  let workspace = Runtime_config.effective_workspace mgr.config in
  let extra_allowed_paths =
    mgr.config.security.extra_allowed_paths
    |> List.map Runtime_config.expand_home
  in
  let shell_policy_summary, sandbox_backend_effective, shell_is_sandboxed =
    shell_policy_summary mgr mgr.sandbox
  in
  {
    Prompt_builder.session_id = key;
    session_name = (if is_main_session_key key then Some "main" else None);
    is_main_session = is_main_session_key key;
    heartbeat_routing_applies =
      Session_heartbeat.heartbeat_routing_applies mgr ~key;
    effective_workspace = workspace;
    workspace_only = mgr.config.security.workspace_only;
    sandbox_backend_requested = mgr.config.security.sandbox_backend;
    sandbox_backend_effective;
    shell_is_sandboxed;
    shell_policy_summary;
    shell_visible_roots_summary =
      shell_visible_roots_summary
        ~workspace_only:mgr.config.security.workspace_only ~workspace
        ~extra_allowed_paths;
    daemon_uptime_line =
      Daemon_status.daemon_runtime_context_line
        ~pid:(Daemon_status.read_current_daemon_pid ());
    background_tasks = active_background_task_summaries mgr;
    context_usage =
      Some (Agent.runtime_context_usage agent ~compacted_before_turn);
    tunnel_status_line =
      Some ("- Tunnel: " ^ !Prompt_builder.tunnel_status_line_fn ());
    task_tree_summary =
      (match mgr.db with
      | Some db ->
          Task_tree.init_schema db;
          let summary = Task_tree.render_focus ~db ~session_key:key in
          Some summary
      | None -> None);
  }

let format_context_block ?channel_name ?channel_type ?sender_id ?sender_name
    ?user_group () =
  let cn = match channel_name with Some n -> n | None -> "cli" in
  let ct = match channel_type with Some t -> t | None -> "dm" in
  let sender_part =
    match (sender_id, sender_name) with
    | Some id, Some name -> Printf.sprintf " sender=@%s (%s)" id name
    | Some id, None -> Printf.sprintf " sender=@%s" id
    | None, Some name -> Printf.sprintf " sender=%s" name
    | None, None -> ""
  in
  let group_part =
    match user_group with
    | Some g -> Printf.sprintf " user_group=%s" g
    | None -> ""
  in
  Printf.sprintf "[Context: channel=%s type=%s%s%s]" cn ct sender_part
    group_part

let inject_attachment_context agent attachments =
  match Prompt_builder.attachment_syntax_block attachments with
  | Some block ->
      agent.Agent.history <-
        Provider.make_message ~role:"system" ~content:block
        :: agent.Agent.history
  | None -> ()

let effective_message_for_turn ~message ?channel_name ?channel_type ?sender_id
    ?sender_name ?user_group () =
  match (channel_name, channel_type, sender_id, sender_name, user_group) with
  | None, None, None, None, None -> message
  | _ ->
      let ctx =
        format_context_block ?channel_name ?channel_type ?sender_id ?sender_name
          ?user_group ()
      in
      let trust_msg =
        match user_group with
        | Some "admin" -> "\n" ^ Admin.trust_description_admin
        | Some "guest" -> "\n" ^ Admin.trust_description_guest
        | _ -> ""
      in
      ctx ^ trust_msg ^ "\n" ^ message
