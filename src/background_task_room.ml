(** Room-origin background task launches. *)

include Background_task_spawn

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
