let enqueue_tool_with_notify ~notify_cfg ~db =
  {
    Tool.name = "background_task_enqueue";
    description =
      "Queue a background coding task (Codex, Claude, Kimi, Gemini, Opencode, \
       Cursor, or Local) in its own git worktree. Lower-level alternative to \
       delegate — use when you need explicit control over runner, repo, \
       branch, or model. Use runner='local' with agent_name for in-process \
       agent tasks (no external CLI). Use delegate for simple 'spawn a \
       subagent' requests.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "runner",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "enum",
                        `List
                          [
                            `String "codex";
                            `String "claude";
                            `String "kimi";
                            `String "gemini";
                            `String "opencode";
                            `String "cursor";
                            `String "local";
                          ] );
                      ( "description",
                        `String
                          "Which coding CLI to run in the background worktree \
                           (required). Use 'local' for in-process agent \
                           execution (pair with agent_name)." );
                    ] );
                ( "repo_path",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Absolute or relative path to the git repository to \
                           use as the worktree source (required)." );
                    ] );
                ( "prompt",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Implementation prompt to hand to the coding agent \
                           (required)." );
                    ] );
                ( "branch",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Optional branch name for the new worktree. Defaults \
                           to clawq-bg-<task-id>." );
                    ] );
                ( "model",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Optional explicit model for the external runner, \
                           e.g. gpt-5.4 or claude-sonnet-4-6." );
                    ] );
                ( "automerge",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "Auto-rebase and fast-forward task branch on \
                           success. Default: true. Set false to skip \
                           automerge. Ignored if use_worktree is false." );
                    ] );
                ( "use_worktree",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "Run in a git worktree (default: true). Set false to \
                           run directly in the repo directory." );
                    ] );
                ( "acp",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "Use ACP (Agent Client Protocol) mode for \
                           bidirectional JSON-RPC communication. Default: \
                           false." );
                    ] );
                ( "agent_name",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Optional agent template name to use for the task \
                           (e.g. 'coder', 'reviewer'). When set, the task uses \
                           that agent's system prompt, tool restrictions, and \
                           model override." );
                    ] );
              ] );
          ( "required",
            `List [ `String "runner"; `String "repo_path"; `String "prompt" ] );
          ("additionalProperties", `Bool false);
        ];
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let runner_s =
          try args |> member "runner" |> to_string with _ -> ""
        in
        let repo_path =
          try args |> member "repo_path" |> to_string with _ -> ""
        in
        let prompt = try args |> member "prompt" |> to_string with _ -> "" in
        let branch =
          try
            match args |> member "branch" with
            | `String s when String.trim s <> "" -> Some s
            | _ -> None
          with _ -> None
        in
        let model =
          try
            match args |> member "model" with
            | `String s when String.trim s <> "" -> Some (String.trim s)
            | _ -> None
          with _ -> None
        in
        let automerge =
          try
            args |> member "automerge" |> to_bool_option
            |> Option.value ~default:true
          with _ -> true
        in
        let use_worktree =
          try
            args |> member "use_worktree" |> to_bool_option
            |> Option.value ~default:true
          with _ -> true
        in
        let acp =
          try
            args |> member "acp" |> to_bool_option
            |> Option.value ~default:false
          with _ -> false
        in
        let agent_name =
          try
            match args |> member "agent_name" with
            | `String s when String.trim s <> "" -> Some (String.trim s)
            | _ -> None
          with _ -> None
        in
        match Background_task.runner_of_string runner_s with
        | None ->
            Lwt.return
              "Error: runner must be 'codex', 'claude', 'kimi', 'gemini', \
               'opencode', 'cursor', or 'local'. Use 'local' with agent_name \
               for in-process agent execution."
        | Some runner when String.trim repo_path = "" ->
            Lwt.return "Error: repo_path is required"
        | Some _ when String.trim prompt = "" ->
            Lwt.return "Error: prompt is required"
        | Some runner -> (
            let session_key, channel, channel_id =
              Background_task.routing_from_context ?context ?notify_cfg ()
            in
            match
              Background_task.enqueue ~db ~runner ?model ~automerge
                ~use_worktree ~acp ?agent_name ~repo_path ~prompt ?branch
                ?session_key ?channel ?channel_id ()
            with
            | Ok id ->
                Lwt.return
                  (Printf.sprintf
                     "Queued background task %d (%s). Use background_task_list \
                      or `clawq background show %d` to track it."
                     id
                     (Background_task.string_of_runner runner)
                     id)
            | Error msg -> Lwt.return ("Error: " ^ msg)));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }

let enqueue_tool ~db = enqueue_tool_with_notify ~notify_cfg:None ~db

let list_tool ~db =
  {
    Tool.name = "background_task_list";
    description =
      "List background coding tasks or inspect one task by id, including \
       current status, repo, branch, log path, and result preview. The prompt \
       is truncated by default; pass full:true to include the complete \
       original prompt when needed.";
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
                        `String
                          "Optional task id to inspect. When omitted, returns \
                           the full task list." );
                    ] );
                ( "full",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "When true and an id is provided, include the full \
                           untruncated prompt. Defaults to false." );
                    ] );
              ] );
          ("additionalProperties", `Bool false);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let task_id =
          try Some (args |> member "id" |> to_int) with _ -> None
        in
        let full = try args |> member "full" |> to_bool with _ -> false in
        match task_id with
        | Some id -> (
            match Background_task.get_task ~db ~id with
            | Some task ->
                Lwt.return (Background_task.format_task_summary ~full task)
            | None ->
                Lwt.return
                  (Printf.sprintf "No background task found with id %d" id))
        | None ->
            let tasks, hidden = Background_task.list_tasks_for_display ~db in
            Lwt.return
              (Background_task.format_task_list_with_hidden tasks hidden));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let wait_tool ~db =
  {
    Tool.name = "background_task_wait";
    description =
      "Wait for a background coding task to finish (max 110 seconds). If the \
       task is still running when the timeout is reached, call this tool again \
       to continue waiting.";
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
                      ("description", `String "Task id to wait for (required)");
                    ] );
                ( "timeout_seconds",
                  `Assoc
                    [
                      ("type", `String "number");
                      ( "description",
                        `String
                          (Printf.sprintf
                             "Seconds to wait (default and max: %.0f). Values \
                              above the max are clamped."
                             Background_task.max_wait_seconds) );
                    ] );
              ] );
          ("required", `List [ `String "id" ]);
          ("additionalProperties", `Bool false);
        ];
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let id = try args |> member "id" |> to_int with _ -> -1 in
        let raw_timeout =
          try args |> member "timeout_seconds" |> to_number
          with _ -> Background_task.max_wait_seconds
        in
        let timeout_seconds =
          Float.min (Float.max raw_timeout 0.0) Background_task.max_wait_seconds
        in
        let was_clamped = raw_timeout > Background_task.max_wait_seconds in
        let interrupt_check =
          Option.bind context (fun c -> c.Tool.interrupt_check)
        in
        if id < 0 then
          Lwt.return "Error: id is required and must be a non-negative integer."
        else
          let open Lwt.Syntax in
          let* result =
            Background_task.wait_until_terminal ~timeout_seconds
              ?interrupt_check ~db ~id ()
          in
          match result with
          | Background_task.Finished task ->
              Lwt.return
                (Background_task.format_task_summary ~compact:true task)
          | Background_task.Timeout task ->
              let clamp_note =
                if was_clamped then
                  Printf.sprintf " (requested %.0fs clamped to max %.0fs)"
                    raw_timeout Background_task.max_wait_seconds
                else ""
              in
              Lwt.return
                (Printf.sprintf
                   "Task %d is still %s after waiting%s. To continue waiting, \
                    call background_task_wait again with {\"id\": %d}. You can \
                    also check progress with background_task_logs.\n\
                    runner: %s | runtime: %s | repo: %s"
                   id
                   (Background_task.string_of_status task.status)
                   clamp_note id
                   (Background_task.string_of_runner task.runner)
                   (Background_task.runtime_string task)
                   task.repo_path)
          | Background_task.Interrupted task ->
              Lwt.return
                (Printf.sprintf
                   "Task %d is still %s. Waiting was interrupted to process a \
                    new incoming message. Call background_task_wait again with \
                    {\"id\": %d} to resume waiting. You can also check \
                    progress with background_task_logs.\n\
                    runner: %s | runtime: %s | repo: %s"
                   id
                   (Background_task.string_of_status task.status)
                   id
                   (Background_task.string_of_runner task.runner)
                   (Background_task.runtime_string task)
                   task.repo_path)
          | Background_task.Not_found ->
              Lwt.return
                (Printf.sprintf "Error: No background task found with id %d" id));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let logs_tool ~db =
  {
    Tool.name = "background_task_logs";
    description =
      "Read lines from a background task log file. Supports offset-based \
       paging (like file_read) or tail-style retrieval.";
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
                        `String "Task id whose log should be read (required)" );
                    ] );
                ( "offset",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String
                          "1-indexed line number to start reading from. When \
                           set, returns lines starting at this position (paged \
                           mode). When omitted, returns trailing lines (tail \
                           mode)." );
                    ] );
                ( "limit",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String
                          "Max lines to return (default 20). In paged mode, \
                           controls window size. In tail mode, controls how \
                           many trailing lines." );
                    ] );
                ( "lines",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String
                          "Alias for limit (backward compatibility). If both \
                           limit and lines are set, limit takes precedence." );
                    ] );
              ] );
          ("required", `List [ `String "id" ]);
          ("additionalProperties", `Bool false);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let id = try args |> member "id" |> to_int with _ -> -1 in
        let offset = try args |> member "offset" |> to_int with _ -> 0 in
        let limit_explicit =
          try Some (args |> member "limit" |> to_int) with _ -> None
        in
        let lines_explicit =
          try Some (args |> member "lines" |> to_int) with _ -> None
        in
        let lines =
          match (limit_explicit, lines_explicit) with
          | Some l, _ -> l
          | None, Some l -> l
          | None, None -> 20
        in
        let lines = min lines Background_task.background_task_logs_max_lines in
        if id < 0 then Lwt.return "Error: id is required"
        else if offset < 0 then Lwt.return "Error: offset must be >= 1"
        else
          match Background_task.get_task ~db ~id with
          | None ->
              Lwt.return
                (Printf.sprintf "Error: No background task found with id %d" id)
          | Some task -> (
              match Background_task.log_excerpt ~offset ~lines task with
              | Ok text -> Lwt.return text
              | Error msg -> Lwt.return ("Error: " ^ msg)));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let delegate_tool_with_notify ?(check_available = true) ~db ~default_repo_path
    ~notify_cfg () =
  {
    Tool.name = "delegate";
    description =
      "Delegate a coding task to a background subagent (Codex, Claude, Kimi, \
       Gemini, Opencode, or Cursor) that runs in its own git worktree. Use \
       when asked to spawn subagents, use workers, or run tasks with a \
       specific model (e.g. 'use haiku to ...', 'delegate to sonnet'). \
       Auto-selects runner and repo by default.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "goal",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Implementation goal for the delegated coding task \
                           (required)." );
                    ] );
                ( "runner",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "enum",
                        `List
                          [
                            `String "auto";
                            `String "codex";
                            `String "claude";
                            `String "kimi";
                            `String "gemini";
                            `String "opencode";
                            `String "cursor";
                          ] );
                      ( "description",
                        `String
                          "Optional runner choice. 'auto' prefers Kimi, then \
                           Cursor, then Opencode (with zai-coding-plan/glm-5), \
                           then Claude, then Codex, then Gemini." );
                    ] );
                ( "repo_path",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Optional repository path. Defaults to the runtime \
                           workspace." );
                    ] );
                ( "branch",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Optional branch name for the worktree. Defaults to \
                           clawq-bg-<task-id>." );
                    ] );
                ( "model",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Optional explicit model for the external runner, \
                           e.g. gpt-5.4 or claude-sonnet-4-6." );
                    ] );
                ( "automerge",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "Auto-rebase and fast-forward task branch on \
                           success. Default: true. Set false to skip \
                           automerge. Ignored if use_worktree is false." );
                    ] );
                ( "use_worktree",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "Run in a git worktree (default: true). Set false to \
                           run directly in the repo directory." );
                    ] );
                ( "acp",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "Use ACP (Agent Client Protocol) mode for \
                           bidirectional JSON-RPC communication. Default: \
                           false." );
                    ] );
                ( "cwd",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Optional working directory for the delegated agent. \
                           Only used when use_worktree is false." );
                    ] );
              ] );
          ("required", `List [ `String "goal" ]);
          ("additionalProperties", `Bool false);
        ];
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let goal = try args |> member "goal" |> to_string with _ -> "" in
        let runner_pref, runner_error =
          try
            match args |> member "runner" |> to_string with
            | s when String.trim s = "" || String.lowercase_ascii s = "auto" ->
                (None, None)
            | s -> (
                match Background_task.runner_of_string s with
                | Some runner -> (Some runner, None)
                | None ->
                    ( None,
                      Some
                        "runner must be 'auto', 'codex', 'claude', 'kimi', \
                         'gemini', 'opencode', or 'cursor'" ))
          with _ -> (None, None)
        in
        let repo_path =
          try
            match args |> member "repo_path" with
            | `String s when String.trim s <> "" -> Some s
            | _ -> None
          with _ -> None
        in
        let branch =
          try
            match args |> member "branch" with
            | `String s when String.trim s <> "" -> Some s
            | _ -> None
          with _ -> None
        in
        let model =
          try
            match args |> member "model" with
            | `String s when String.trim s <> "" -> Some (String.trim s)
            | _ -> None
          with _ -> None
        in
        let automerge =
          try
            args |> member "automerge" |> to_bool_option
            |> Option.value ~default:true
          with _ -> true
        in
        let use_worktree =
          try
            args |> member "use_worktree" |> to_bool_option
            |> Option.value ~default:true
          with _ -> true
        in
        let acp =
          try
            args |> member "acp" |> to_bool_option
            |> Option.value ~default:false
          with _ -> false
        in
        let delegate_cwd =
          try
            match args |> member "cwd" with
            | `String s when String.trim s <> "" -> Some (String.trim s)
            | _ -> None
          with _ -> None
        in
        if String.trim goal = "" then Lwt.return "Error: goal is required"
        else if runner_error <> None then
          Lwt.return ("Error: " ^ Option.get runner_error)
        else
          let effective_repo_path =
            match (delegate_cwd, use_worktree) with
            | Some c, false -> Some c
            | _ -> repo_path
          in
          match
            Background_task.delegate_enqueue ?context ?notify_cfg
              ~check_available ~db ~automerge ~use_worktree ~acp
              ?preferred_runner:runner_pref ?model
              ?repo_path:effective_repo_path ?branch ~default_repo_path ~goal ()
          with
          | Ok (id, runner, repo) ->
              Lwt.return
                (Printf.sprintf
                   "Delegated task %d (%s) for %s. Use background_task_wait or \
                    `clawq background show %d` to track it."
                   id
                   (Background_task.string_of_runner runner)
                   repo id)
          | Error msg -> Lwt.return ("Error: " ^ msg));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }

let delegate_tool ?check_available ~db ~default_repo_path () =
  delegate_tool_with_notify ?check_available ~db ~default_repo_path
    ~notify_cfg:None ()

let resume_tool ~db =
  {
    Tool.name = "background_task_resume";
    description =
      "Resume a previously started background coding task using the runner's \
       built-in session resume support. Requires a worktree-backed task that \
       has already started at least once.";
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
                      ("description", `String "Task id to resume (required)");
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
          Lwt.return "Error: id is required and must be a non-negative integer."
        else
          match Background_task.request_resume ~db ~id ~message:None with
          | Ok msg -> Lwt.return msg
          | Error msg -> Lwt.return ("Error: " ^ msg));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }

let message_tool ~db =
  {
    Tool.name = "background_task_send_message";
    description =
      "Send a new chat message into a background coding task. The current run \
       is resumed with the runner's native continue/resume support and the \
       message is injected as a user chat message.";
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
                      ("description", `String "Task id to message (required)");
                    ] );
                ( "message",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Message to inject into the task chat (required)" );
                    ] );
              ] );
          ("required", `List [ `String "id"; `String "message" ]);
          ("additionalProperties", `Bool false);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let id = try args |> member "id" |> to_int with _ -> -1 in
        let message =
          try args |> member "message" |> to_string with _ -> ""
        in
        if id < 0 then
          Lwt.return "Error: id is required and must be a non-negative integer."
        else if String.trim message = "" then
          Lwt.return "Error: message is required and must not be empty."
        else
          match
            Background_task.request_resume ~db ~id ~message:(Some message)
          with
          | Ok msg -> Lwt.return msg
          | Error msg -> Lwt.return ("Error: " ^ msg));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }

let cancel_tool ~db =
  {
    Tool.name = "background_task_cancel";
    description = "Cancel a queued or running background coding task by id.";
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
                      ("description", `String "Task id to cancel (required)");
                    ] );
              ] );
          ("required", `List [ `String "id" ]);
          ("additionalProperties", `Bool false);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let id = try args |> member "id" |> to_int with _ -> -1 in
        if id < 0 then Lwt.return "Error: id is required"
        else
          match Background_task.cancel ~db ~id with
          | Ok msg -> Lwt.return msg
          | Error msg -> Lwt.return ("Error: " ^ msg));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }

let recover_tool ~db =
  {
    Tool.name = "background_task_recover";
    description =
      "Recover a failed or stuck background task by spawning a replacement \
       with full context from the original. Works on failed, dirty_worktree, \
       cancelled, or stuck (stalled/zombie/process-missing) tasks.";
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
                      ("description", `String "Task id to recover (required)");
                    ] );
                ( "runner",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Optional runner override \
                           (codex|claude|kimi|gemini|opencode|cursor)" );
                    ] );
                ( "model",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Optional model override");
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
            "Error: id is required and must be a positive integer. Provide the \
             numeric task id of the task to recover."
        else
          let runner =
            try
              let s = args |> member "runner" |> to_string in
              if String.trim s = "" then None
              else Background_task.runner_of_string s
            with _ -> None
          in
          let model =
            try
              let s = args |> member "model" |> to_string in
              if String.trim s = "" then None else Some s
            with _ -> None
          in
          match Background_task.recover ~db ~id ?runner ?model () with
          | Ok (new_id, effective_runner) ->
              Lwt.return
                (Printf.sprintf
                   "Recovered task %d → new task %d (%s). Use `background show \
                    %d` to track it."
                   id new_id
                   (Background_task.string_of_runner effective_runner)
                   new_id)
          | Error msg -> Lwt.return ("Error: " ^ msg));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }
