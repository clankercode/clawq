(** Room profile resolution, CWD management, instruction items, and model
    resolution for sessions. Operates on [Session_types.t] via parameter
    annotation to avoid circular module dependencies. *)

(** {1 CWD Policy} *)

let is_cwd_allowed (mgr : Session_types.t) ~cwd =
  let cfg = mgr.config in
  let is_prefix_of ~prefix path =
    let plen = String.length prefix in
    let pathlen = String.length path in
    if pathlen = plen then path = prefix
    else if pathlen > plen then
      String.sub path 0 plen = prefix && path.[plen] = '/'
    else false
  in
  let resolve p =
    try Unix.realpath p
    with Unix.Unix_error _ ->
      let dir = Filename.dirname p in
      let base = Filename.basename p in
      let real_dir = try Unix.realpath dir with Unix.Unix_error _ -> dir in
      Filename.concat real_dir base
  in
  let under_workspace_roots () =
    let workspace = Runtime_config.effective_workspace cfg in
    let real_workspace = resolve workspace in
    let real_cwd = resolve cwd in
    is_prefix_of ~prefix:real_workspace real_cwd
    || List.exists
         (fun extra ->
           let expanded = Runtime_config.expand_home extra in
           is_prefix_of ~prefix:(resolve expanded) real_cwd)
         cfg.security.extra_allowed_paths
  in
  let matches_patterns () =
    let patterns = cfg.security.allowed_cwd_patterns in
    if patterns = [] then false
    else
      List.exists
        (fun pat ->
          let expanded = Runtime_config.expand_cwd_pattern ~config:cfg pat in
          if expanded = "" then false
          else Path_util.glob_matches_path ~pattern:expanded cwd)
        patterns
  in
  let ws_ok = (not cfg.security.workspace_only) || under_workspace_roots () in
  let pat_ok = cfg.security.allowed_cwd_patterns = [] || matches_patterns () in
  ws_ok && pat_ok

let set_effective_cwd (mgr : Session_types.t) ~key ~cwd =
  if not (is_cwd_allowed mgr ~cwd) then begin
    Logs.warn (fun m ->
        m "[%s] Refusing to set CWD to %s: outside allowed CWD policy" key cwd);
    false
  end
  else begin
    (match Hashtbl.find_opt mgr.sessions key with
    | Some (agent, _, _) -> agent.Agent.effective_cwd <- Some cwd
    | None -> ());
    (match mgr.db with
    | Some db -> Memory.set_session_cwd ~db ~session_key:key ~cwd:(Some cwd)
    | None -> ());
    true
  end

let apply_cwd_change_for_turn (mgr : Session_types.t) ~key agent ~cwd =
  if is_cwd_allowed mgr ~cwd then begin
    let old_cwd = agent.Agent.effective_cwd in
    agent.Agent.effective_cwd <- Some cwd;
    (match old_cwd with
    | Some prev when prev <> cwd ->
        let event_msg =
          Provider.make_message ~role:"event"
            ~content:
              (Printf.sprintf "[system] Working directory changed from %s to %s"
                 prev cwd)
        in
        agent.Agent.history <- agent.Agent.history @ [ event_msg ]
    | _ -> ());
    match mgr.db with
    | Some db -> Memory.set_session_cwd ~db ~session_key:key ~cwd:(Some cwd)
    | None -> ()
  end
  else
    Logs.warn (fun m ->
        m
          "[%s] Ignoring CWD change to %s at turn start: outside allowed CWD \
           policy"
          key cwd)

(** {1 Room Profile Resolution} *)

let child_thread_parent_session_key key =
  match Room_session.parse_child_thread_key key with
  | Some child -> Some (child.connector ^ ":" ^ child.room_id)
  | None -> None

let room_profile_binding_matches_child (cfg : Runtime_config.t)
    (child : Room_session.child_thread) =
  List.exists
    (fun (b : Runtime_config.room_profile_binding) ->
      b.active
      && b.profile_id = child.profile_id
      && (b.room = child.room_id
         || b.room = child.connector ^ ":" ^ child.room_id))
    cfg.room_profile_bindings

let room_profile_binding_active_for_profile (cfg : Runtime_config.t) ~profile_id
    =
  List.exists
    (fun (b : Runtime_config.room_profile_binding) ->
      b.active && b.profile_id = profile_id)
    cfg.room_profile_bindings

let find_active_room_profile (cfg : Runtime_config.t) ~profile_id =
  List.find_opt
    (fun (p : Runtime_config.room_profile) ->
      p.id = profile_id && String.lowercase_ascii p.status <> "deleted")
    cfg.room_profiles

let resolve_room_profile_for_session (mgr : Session_types.t) ~key =
  match Runtime_config.resolve_room_profile mgr.config ~session_key:key with
  | Some _ as profile -> profile
  | None -> (
      match Room_session.parse_child_thread_key key with
      | Some child ->
          if not (room_profile_binding_matches_child mgr.config child) then None
          else find_active_room_profile mgr.config ~profile_id:child.profile_id
      | None -> (
          match Room_session.parse_routine_key key with
          | None -> None
          | Some routine ->
              if
                not
                  (room_profile_binding_active_for_profile mgr.config
                     ~profile_id:routine.profile_id)
              then None
              else
                find_active_room_profile mgr.config
                  ~profile_id:routine.profile_id))

(** {1 CWD Resolution} *)

let resolve_initial_cwd (mgr : Session_types.t) ~session_key ~db ~agent_template
    =
  let cfg = mgr.config in
  let cwd_from_template () =
    match agent_template with
    | Some (tmpl : Agent_template.t) -> (
        match tmpl.cwd with
        | Some cwd
          when Sys.file_exists cwd && Sys.is_directory cwd
               && is_cwd_allowed mgr ~cwd ->
            Some cwd
        | _ -> None)
    | None -> None
  in
  let cwd_from_config () =
    let ws = cfg.workspace in
    if ws = Runtime_config.default_workspace () then None
    else
      let expanded = Runtime_config.expand_home ws in
      if
        expanded <> "" && Sys.file_exists expanded && Sys.is_directory expanded
        && is_cwd_allowed mgr ~cwd:expanded
      then Some expanded
      else None
  in
  let cwd_from_db_key key =
    match db with
    | Some db -> (
        match Memory.get_session_cwd ~db ~session_key:key with
        | Some cwd when is_cwd_allowed mgr ~cwd -> Some cwd
        | _ -> None)
    | None -> None
  in
  let cwd_from_db () =
    match cwd_from_db_key session_key with
    | Some _ as r -> r
    | None -> (
        match child_thread_parent_session_key session_key with
        | Some parent_key -> cwd_from_db_key parent_key
        | None -> None)
  in
  let cwd_from_routine () =
    match Room_session.parse_routine_key session_key with
    | None -> None
    | Some routine ->
        let cwd =
          Room_workspace.routine_workspace_path ~create:true
            ~profile_id:routine.profile_id ~routine_id:routine.routine_id
        in
        if is_cwd_allowed mgr ~cwd then Some cwd else None
  in
  match cwd_from_db () with
  | Some _ as r -> r
  | None -> (
      match cwd_from_routine () with
      | Some _ as r -> r
      | None -> (
          match cwd_from_config () with
          | Some _ as r -> r
          | None -> cwd_from_template ()))

(** {1 Instruction Items} *)

let resolve_instruction_items_for_session (mgr : Session_types.t) ~key :
    Runtime_config.effective_instruction_item list =
  let room_profile = resolve_room_profile_for_session mgr ~key in
  let scope_key =
    match child_thread_parent_session_key key with
    | Some parent_key -> parent_key
    | None -> key
  in
  let effective_access =
    Runtime_config.resolve_effective_access mgr.config ~session_key:scope_key
      ?room_profile ()
  in
  effective_access.instruction_items

(** {1 Room Profile Template Fields} *)

let apply_room_profile_template_fields (mgr : Session_types.t) ~key agent =
  match resolve_room_profile_for_session mgr ~key with
  | None -> agent.Agent.room_profile_system_prompt <- None
  | Some profile ->
      agent.Agent.profiled_room <- true;
      let cfg = agent.Agent.config in
      let ad = cfg.agent_defaults in
      let ad =
        if profile.system_prompt <> "" then
          { ad with Runtime_config.system_prompt = profile.system_prompt }
        else ad
      in
      let ad =
        if profile.max_tool_iterations > 0 then
          {
            ad with
            Runtime_config.max_tool_iterations = profile.max_tool_iterations;
          }
        else ad
      in
      agent.Agent.config <- { cfg with agent_defaults = ad };
      agent.Agent.room_profile_system_prompt <-
        (if profile.system_prompt <> "" then Some profile.system_prompt
         else None)

(** {1 Model Resolution} *)

let resolve_model_for_session (mgr : Session_types.t) ~key : string option =
  let check_gate model =
    match
      Agent_template.check_model_security_gates ~config:mgr.config ~model
    with
    | Ok () -> true
    | Error msg ->
        Logs.warn (fun m ->
            m "[session] Model %S for %s denied by security gate: %s" model key
              msg);
        false
  in
  let try_model = function Some m when check_gate m -> Some m | _ -> None in
  let tier1 =
    match mgr.db with
    | Some db -> Memory.get_session_model_override ~db ~session_key:key
    | None -> None
  in
  match tier1 with
  | Some _ -> tier1
  | None -> (
      let tier2 =
        match resolve_room_profile_for_session mgr ~key with
        | Some p when p.model <> "" -> Some p.model
        | _ -> None
      in
      match try_model tier2 with
      | Some _ -> tier2
      | None ->
          let channel_type = Runtime_config.channel_type_of_session_key key in
          Runtime_config.channel_default_model mgr.config ~channel_type)
