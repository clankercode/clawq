(** Startup helpers for daemon database and tool registry setup. *)

let init_database ~(config : Runtime_config.t) =
  let db_path =
    if config.memory.db_path <> "" then config.memory.db_path
    else Dot_dir.db_path ()
  in
  try
    let clawq_dir = Dot_dir.path () in
    (try if not (Sys.file_exists clawq_dir) then Sys.mkdir clawq_dir 0o755
     with _ -> ());
    let db =
      Memory.init ~db_path ~search_enabled:config.memory.search_enabled ()
    in
    Vector.init_schema db;
    Provider_quota.set_db db;
    if config.security.audit_enabled then begin
      Audit.init_schema db;
      Logs.info (fun m -> m "Audit trail enabled")
    end;
    Access_snapshot.init_schema db;
    Room_session_record.init_schema db;
    Logs.info (fun m ->
        m "SQLite memory initialized at %s (vector index enabled)" db_path);
    Some db
  with exn ->
    Logs.warn (fun m ->
        m "Failed to initialize SQLite memory: %s" (Printexc.to_string exn));
    None

let reconcile_room_profiles_at_startup ~db ~(config : Runtime_config.t) =
  match db with
  | Some db -> (
      try ignore (Memory.reconcile_room_profiles ~db ~config)
      with exn ->
        Logs.warn (fun m ->
            m "Room profile reconciliation failed at startup: %s"
              (Printexc.to_string exn)))
  | None -> ()

let init_tool_registry ~(config : Runtime_config.t)
    ~(current_config : Runtime_config.t ref) ~(sandbox : Sandbox.t) ~db =
  if config.security.tools_enabled then begin
    let registry = Tool_registry.create () in
    Tools_builtin.register_all ~config:!current_config ~sandbox ~db registry;
    let skills =
      Skills.load_all ~workspace_only:config.security.workspace_only
        ~allowed_commands:Tools_builtin.default_shell_allowlist ()
    in
    List.iter
      (fun s ->
        Tool_registry.register_skill registry s;
        Logs.info (fun m -> m "Loaded skill: %s" s.Tool.name))
      skills;
    Tool_registry.register registry (Skills.skill_create_tool ());
    let workspace = Runtime_config.effective_workspace config in
    Tool_registry.register registry
      (Skills.skill_list_tool ~workspace_dir:workspace ());
    let skill_cache = Skills.init_cache ~workspace_dir:workspace () in
    ignore (Agent_template.init_cache ~workspace_dir:workspace ());
    Tool_registry.register registry
      (Skills.use_skill_tool ~workspace_only:config.security.workspace_only ());
    Lwt.async (fun () -> Skills.skill_watcher_loop skill_cache);
    Session_turn.expand_skill_refs_fn := Skills.expand_skill_refs;
    (Agent.find_skill_for_reload_fn :=
       fun name ->
         match Skills.find_skill_md name with
         | Some s -> Some (s.meta.md_description, s.instructions)
         | None -> None);
    Logs.info (fun m ->
        m "Tools enabled, registered built-in tools + %d skills"
          (List.length skills));
    Some registry
  end
  else begin
    Logs.info (fun m ->
        m "Tools disabled (set security.tools_enabled to enable)");
    None
  end
