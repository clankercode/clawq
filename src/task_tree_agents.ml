include Task_tree_templates

let prompt_for_agent_task (task : task) =
  match task.agent_prompt with
  | Some prompt when String.trim prompt <> "" -> Ok prompt
  | _ ->
      Error
        (Printf.sprintf
           "Task %s has no agent_prompt. Add agent_prompt to the task before \
            starting it as an agent."
           (display_id task.id))

let enqueue_agent_for_task ~db ~session_key ~repo_path ?(use_worktree = true)
    (task : task) =
  match task.agent_task_id with
  | Some id ->
      Ok
        ( id,
          Printf.sprintf
            "Task %s already has background task %d. Use background_task_list \
             or `clawq subagents list` to track it."
            (display_id task.id) id )
  | None -> (
      match prompt_for_agent_task task with
      | Error _ as err -> err
      | Ok prompt -> (
          match
            Background_task.enqueue ~db ~runner:Background_task.Local
              ?model:task.agent_model ~repo_path ~prompt ~use_worktree
              ?agent_name:task.agent_type ~session_key
              ?profile_id:task.profile_id ?origin_json:task.origin_json
              ?thread_id:task.thread_id ?requester:task.requester ()
          with
          | Error e -> Error e
          | Ok bg_id -> (
              match
                mark_agent_started ~db ~session_key ~id:task.id
                  ~agent_task_id:bg_id
              with
              | Error e -> Error e
              | Ok () ->
                  Ok
                    ( bg_id,
                      Printf.sprintf
                        "Queued task agent %d for %s. Use background_task_list \
                         or `clawq subagents list` to track it."
                        bg_id (display_id task.id) ))))

let start_ready_autostart_tasks ~db ~session_key ~repo_path =
  ready_autostart_tasks ~db ~session_key
  |> List.map (fun task ->
      enqueue_agent_for_task ~db ~session_key ~repo_path task)

let start_agent_tool ~db ?default_repo_path () : Tool.t =
  {
    name = "task_start_agent";
    description =
      "Start one task_tree task as a native/local subagent using the task's \
       agent metadata. The task must have agent_prompt set. agent_model and \
       agent_type are copied to the local background task when present.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ("id", `Assoc [ ("type", `String "string") ]);
                ( "repo_path",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Optional repository path. Defaults to the tool \
                           context cwd or configured workspace." );
                    ] );
                ( "use_worktree",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "Run the native subagent in a git worktree. Default: \
                           true." );
                    ] );
              ] );
          ("required", `List [ `String "id" ]);
          ("additionalProperties", `Bool false);
        ];
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let session_key =
          match context with
          | Some ctx -> (
              match ctx.Tool.session_key with Some k -> k | None -> "default")
          | None -> "default"
        in
        let id = try Some (args |> member "id" |> to_string) with _ -> None in
        let repo_path =
          try
            match args |> member "repo_path" with
            | `String s when String.trim s <> "" -> Some (String.trim s)
            | _ -> None
          with _ -> None
        in
        let repo_path =
          match repo_path with
          | Some _ -> repo_path
          | None -> (
              match context with
              | Some { Tool.effective_cwd = Some cwd; _ } -> Some cwd
              | _ -> default_repo_path)
        in
        let use_worktree =
          try
            args |> member "use_worktree" |> to_bool_option
            |> Option.value ~default:true
          with _ -> true
        in
        match (id, repo_path) with
        | None, _ ->
            Lwt.return
              "Error: parameter \"id\" is required. Provide an existing \
               task_tree task ID."
        | _, None ->
            Lwt.return
              "Error: repo_path is required. Provide repo_path, invoke the \
               tool from a session with an effective cwd, or configure a \
               workspace."
        | Some id, Some repo_path -> (
            let tasks = load_tasks ~db ~session_key () in
            let id = resolve_existing_id ~tasks ~id in
            match List.find_opt (fun task -> task.id = id) tasks with
            | None -> Lwt.return ("Error: " ^ not_found_error ~tasks ~id)
            | Some task -> (
                match
                  enqueue_agent_for_task ~db ~session_key ~repo_path
                    ~use_worktree task
                with
                | Ok (_, msg) -> Lwt.return msg
                | Error msg -> Lwt.return ("Error: " ^ msg))));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }
