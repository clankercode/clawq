(* B712: Clean LLM-facing subagent tools wrapping the background task
   infrastructure. Two tools: subagent (spawn) and subagent_result (inspect/wait). *)

let max_result_chars = 20_000
let max_peek_lines = 200
let max_subagent_depth = 4
let max_subagent_result_wait_seconds = 270.0
let prompt_cache_wait_margin_seconds = 5.0
let spawn_prompt_preview_chars = 120

let prompt_cache_retention_seconds retention =
  match Scheduler.parse_duration_seconds (String.trim retention) with
  | Ok seconds -> Some seconds
  | Error _ -> None

let prompt_cache_retention_for_model ?model ~config () =
  let config =
    match model with
    | Some model when String.trim model <> "" ->
        let agent_defaults =
          {
            config.Runtime_config.agent_defaults with
            primary_model = String.trim model;
          }
        in
        { config with Runtime_config.agent_defaults }
    | _ -> config
  in
  let _provider_name, provider, _model =
    Provider_routing.select_provider ~config ()
  in
  provider.Runtime_config.prompt_cache_retention

let subagent_result_wait_timeout_seconds ?model ~config () =
  let cache_limited =
    match prompt_cache_retention_for_model ?model ~config () with
    | None -> max_subagent_result_wait_seconds
    | Some retention -> (
        match prompt_cache_retention_seconds retention with
        | Some seconds ->
            Float.max 0.0 (seconds -. prompt_cache_wait_margin_seconds)
        | None -> max_subagent_result_wait_seconds)
  in
  Float.min max_subagent_result_wait_seconds cache_limited

let nonblank_opt = function
  | Some s when String.trim s <> "" -> Some (String.trim s)
  | _ -> None

let effective_subagent_model ~config (task : Background_task.task) =
  let template_model =
    match nonblank_opt task.agent_name with
    | Some name -> (
        match Agent_template.resolve name with
        | Some template -> nonblank_opt template.model
        | None -> None)
    | None -> None
  in
  match (nonblank_opt task.model, template_model) with
  | (Some _ as model), _ -> model
  | None, (Some _ as model) -> model
  | None, None -> (
      match
        nonblank_opt config.Runtime_config.agent_defaults.subagent_default_model
      with
      | Some _ as model -> model
      | None ->
          nonblank_opt (Some config.Runtime_config.agent_defaults.primary_model)
      )

let subagent_result_wait_timeout_seconds_for_task ~config task =
  subagent_result_wait_timeout_seconds
    ?model:(effective_subagent_model ~config task)
    ~config ()

let subagent_depth ~db ~session_key =
  match Background_task.find_task_id_by_session_key ~db ~session_key with
  | None -> 0
  | Some task_id ->
      let rec count_depth id acc =
        match Background_task.get_task ~db ~id with
        | None -> acc
        | Some task -> (
            match task.parent_task_id with
            | Some parent_id -> count_depth parent_id (acc + 1)
            | None -> acc + 1)
      in
      count_depth task_id 0

let truncate_with_guidance ~max text =
  let len = String.length text in
  if len <= max then text
  else
    let prefix = String.sub text 0 max in
    Printf.sprintf
      "%s\n\n\
       [Truncated at %d chars — %d chars total. Use peek.lines or the output \
       log file for the full content.]"
      prefix max len

let spawn_prompt_preview text =
  let len = String.length text in
  if len <= spawn_prompt_preview_chars then text
  else String.sub text 0 spawn_prompt_preview_chars ^ "..."

let read_file_lines path =
  try
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let acc = ref [] in
        (try
           while true do
             acc := input_line ic :: !acc
           done
         with End_of_file -> ());
        List.rev !acc)
  with Sys_error _ -> []

let apply_peek ~lines ?regex ?after all_lines =
  (* Step 1: filter by regex if provided *)
  let filtered =
    match regex with
    | None -> all_lines
    | Some pat -> (
        try
          let re = Str.regexp_string pat in
          List.filter (fun line -> Str.string_match re line 0) all_lines
        with _ -> all_lines)
  in
  (* Step 2: apply 'after' offset (1-based) *)
  let after_offset = match after with Some n when n > 0 -> n | _ -> 0 in
  let after_lines =
    if after_offset > 0 then
      let len = List.length filtered in
      let rec drop n lst =
        if n <= 0 then lst
        else match lst with _ :: rest -> drop (n - 1) rest | [] -> []
      in
      drop (min after_offset len) filtered
    else filtered
  in
  (* Step 3: tail last N lines *)
  let n = min lines max_peek_lines in
  let len = List.length after_lines in
  let rec drop n lst =
    if n <= 0 then lst
    else match lst with _ :: rest -> drop (n - 1) rest | [] -> []
  in
  let tailed = if len > n then drop (len - n) after_lines else after_lines in
  String.concat "\n" tailed

let format_result (task : Background_task.task) ~verbose =
  let status = Background_task.string_of_status task.status in
  let runner = Background_task.string_of_runner task.runner in
  let runtime = Background_task.runtime_string task in
  let header =
    Printf.sprintf "Task %d — %s\nrunner: %s | runtime: %s" task.id status
      runner runtime
  in
  let result_text =
    match task.result_preview with
    | Some text when String.trim text <> "" -> text
    | _ -> "(no output)"
  in
  let output_path =
    match task.log_path with
    | Some p -> Printf.sprintf "\n\nOutput log: %s" p
    | None -> ""
  in
  let verbose_section =
    if verbose then
      let prompt_preview =
        let len = String.length task.prompt in
        if len <= 500 then task.prompt
        else String.sub task.prompt 0 500 ^ "...[truncated]"
      in
      Printf.sprintf "\n\nPrompt: %s" prompt_preview
    else ""
  in
  Printf.sprintf "%s\n\n%s%s%s" header result_text output_path verbose_section

type subagent_wait_result =
  | Wait_finished
  | Wait_timeout
  | Wait_interrupted
  | Wait_steering_message
  | Wait_not_found

let is_steering_message_interrupt = function
  | Some reason when reason = Agent.queued_message_interrupt_token -> true
  | _ -> false

let rec wait_for_subagent_result ?(poll_seconds = 1.0) ?interrupt_check
    ?steering_check ~db ~id ~timeout_seconds () =
  let open Lwt.Syntax in
  let continue_wait () =
    if timeout_seconds <= 0.0 then Lwt.return Wait_timeout
    else
      let sleep_for = Float.min poll_seconds timeout_seconds in
      let* () = Lwt_unix.sleep sleep_for in
      wait_for_subagent_result ~poll_seconds ?interrupt_check ?steering_check
        ~db ~id
        ~timeout_seconds:(timeout_seconds -. sleep_for)
        ()
  in
  match Background_task.get_task ~db ~id with
  | None -> Lwt.return Wait_not_found
  | Some task when Background_task.is_terminal_status task.status ->
      Lwt.return Wait_finished
  | Some _ -> (
      let interrupt_reason = Option.bind interrupt_check (fun f -> f ()) in
      match interrupt_reason with
      | reason when is_steering_message_interrupt reason -> (
          match steering_check with
          | None -> Lwt.return Wait_steering_message
          | Some has_steering_message ->
              let* has_steering = has_steering_message () in
              if has_steering then Lwt.return Wait_steering_message
              else continue_wait ())
      | Some _ -> Lwt.return Wait_interrupted
      | None -> continue_wait ())

let wait_timeout_guidance ~id =
  Printf.sprintf
    "Returned early before the prompt cache timeout. The sub-agent is still \
     running. Call subagent_result again with {\"id\": %d, \"wait\": true} to \
     continue blocking."
    id

let wait_steering_guidance ~id =
  Printf.sprintf
    "Returned early because the user added steering commands while waiting. \
     The sub-agent is still running. Handle the steering message, then call \
     subagent_result again with {\"id\": %d, \"wait\": true} to continue \
     blocking."
    id

let wait_interrupted_guidance ~id =
  Printf.sprintf
    "Waiting was interrupted while the sub-agent was still running. Call \
     subagent_result again with {\"id\": %d, \"wait\": true} to continue \
     blocking."
    id

let spawn_tool ~db =
  {
    Tool.name = "subagent";
    description =
      "Spawn a background subagent to work on a task autonomously. Returns a \
       task id and session key. The parent session is notified automatically \
       when the subagent completes.";
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
                          "The task prompt — what the subagent should do \
                           (required)." );
                    ] );
                ( "model",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Optional model override in provider:model format \
                           (e.g. openai-codex:gpt-5.4). Check the models list \
                           for available providers/models before setting this."
                      );
                    ] );
                ( "agent_template",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Optional agent template name (e.g. 'coder', \
                           'reviewer')." );
                    ] );
                ( "description",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String "Optional short label for status display." );
                    ] );
                ( "fork_context",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "When true, the parent session's conversation \
                           history is captured and injected into the child \
                           session before its first turn, giving the subagent \
                           context about what was discussed. Default: false." );
                    ] );
              ] );
          ("required", `List [ `String "goal" ]);
          ("additionalProperties", `Bool false);
        ];
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let goal = try args |> member "goal" |> to_string with _ -> "" in
        let model =
          try
            match args |> member "model" with
            | `String s when String.trim s <> "" -> Some (String.trim s)
            | _ -> None
          with _ -> None
        in
        let agent_template =
          try
            match args |> member "agent_template" with
            | `String s when String.trim s <> "" -> Some (String.trim s)
            | _ -> None
          with _ -> None
        in
        let description =
          try
            match args |> member "description" with
            | `String s when String.trim s <> "" -> Some (String.trim s)
            | _ -> None
          with _ -> None
        in
        let fork_context =
          try args |> member "fork_context" |> to_bool with _ -> false
        in
        if String.trim goal = "" then Lwt.return "Error: goal is required"
        else
          let parent_session_key =
            match context with
            | Some (c : Tool.invoke_context) -> c.session_key
            | None -> None
          in
          (* Check subagent depth limit *)
          let depth =
            match parent_session_key with
            | Some key -> subagent_depth ~db ~session_key:key
            | None -> 0
          in
          if depth >= max_subagent_depth then
            Lwt.return
              (Printf.sprintf
                 "Error: maximum subagent depth (%d) reached. Subagents cannot \
                  spawn further subagents at this depth."
                 max_subagent_depth)
          else
            let repo_path = Sys.getcwd () in
            let parent_task_id =
              match parent_session_key with
              | Some key ->
                  Background_task.find_task_id_by_session_key ~db
                    ~session_key:key
              | None -> None
            in
            let context_snapshot =
              if fork_context then
                match parent_session_key with
                | Some key ->
                    let history = Memory.load_history ~db ~session_key:key in
                    if history = [] then None
                    else
                      Some
                        (Yojson.Safe.to_string
                           (Provider.messages_to_json history))
                | None -> None
              else None
            in
            match
              Background_task.enqueue ~db ~runner:Background_task.Local
                ~use_worktree:false ~automerge:false ~repo_path ~prompt:goal
                ?model ?agent_name:agent_template
                ?session_key:parent_session_key ?parent_task_id ?description
                ?context_snapshot ()
            with
            | Ok id ->
                let session_key = Printf.sprintf "__bg_task:%d" id in
                let desc_suffix =
                  match description with
                  | Some d -> Printf.sprintf ": %s" d
                  | None -> ""
                in
                Lwt.return
                  (Printf.sprintf
                     "Spawned subagent task %d%s.\n\
                      Session key: %s\n\n\
                      Prompt preview: %s\n\n\
                      The parent session will be notified automatically when \
                      this subagent completes. Use subagent_result with \
                      {\"id\": %d} only if you need to inspect status, output, \
                      or logs before then."
                     id desc_suffix session_key
                     (spawn_prompt_preview goal)
                     id)
            | Error msg -> Lwt.return ("Error: " ^ msg));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }

let result_tool ~config ~db ?session_mgr () =
  {
    Tool.name = "subagent_result";
    description =
      "Check the status or wait for a background subagent task. Returns \
       status, output, and metadata. Use wait:true to block until completion \
       or until the adaptive prompt-cache-aware wait budget elapses. Use peek \
       to tail/filter the log file.";
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
                      ("description", `String "Task id (required).");
                    ] );
                ( "wait",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "Block until the task finishes, the adaptive \
                           prompt-cache-aware wait budget elapses, or a queued \
                           steering message arrives. Default: false." );
                    ] );
                ( "verbose",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "Include the task prompt preview in the result. \
                           Default: false." );
                    ] );
                ( "peek",
                  `Assoc
                    [
                      ("type", `String "object");
                      ( "properties",
                        `Assoc
                          [
                            ( "lines",
                              `Assoc
                                [
                                  ("type", `String "integer");
                                  ( "description",
                                    `String
                                      "Last N lines to return (default 20, max \
                                       200)." );
                                ] );
                            ( "regex",
                              `Assoc
                                [
                                  ("type", `String "string");
                                  ( "description",
                                    `String
                                      "Filter to lines matching this substring \
                                       before tailing." );
                                ] );
                            ( "after",
                              `Assoc
                                [
                                  ("type", `String "integer");
                                  ( "description",
                                    `String
                                      "Return all lines after this 1-based \
                                       line number." );
                                ] );
                          ] );
                      ( "description",
                        `String
                          "Tail/filter the task log file instead of returning \
                           the summary." );
                    ] );
              ] );
          ("required", `List [ `String "id" ]);
          ("additionalProperties", `Bool false);
        ];
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let id = try args |> member "id" |> to_int with _ -> -1 in
        let wait = try args |> member "wait" |> to_bool with _ -> false in
        let verbose =
          try args |> member "verbose" |> to_bool with _ -> false
        in
        let peek_lines, peek_regex, peek_after =
          match args |> member "peek" with
          | `Assoc _ as obj ->
              let lines =
                try min (obj |> member "lines" |> to_int) max_peek_lines
                with _ -> 20
              in
              let regex =
                try
                  match obj |> member "regex" with
                  | `String s when String.trim s <> "" -> Some (String.trim s)
                  | _ -> None
                with _ -> None
              in
              let after =
                try Some (obj |> member "after" |> to_int) with _ -> None
              in
              (Some lines, regex, after)
          | _ -> (None, None, None)
        in
        if id < 0 then
          Lwt.return "Error: id is required and must be a non-negative integer."
        else
          let open Lwt.Syntax in
          let initial_task = Background_task.get_task ~db ~id in
          let interrupt_check =
            Option.bind context (fun c -> c.Tool.interrupt_check)
          in
          let steering_check =
            match
              (session_mgr, Option.bind context (fun c -> c.Tool.session_key))
            with
            | Some mgr, Some key ->
                Some (fun () -> Session.has_queued_steering_message mgr ~key)
            | _ -> None
          in
          let* wait_guidance =
            match (wait, initial_task) with
            | false, _ | true, None -> Lwt.return_none
            | true, Some task -> (
                let timeout_seconds =
                  subagent_result_wait_timeout_seconds_for_task ~config task
                in
                let* wait_result =
                  wait_for_subagent_result ~timeout_seconds ?interrupt_check ~db
                    ?steering_check ~id ()
                in
                match wait_result with
                | Wait_finished | Wait_not_found -> Lwt.return_none
                | Wait_timeout -> Lwt.return_some (wait_timeout_guidance ~id)
                | Wait_steering_message ->
                    Lwt.return_some (wait_steering_guidance ~id)
                | Wait_interrupted ->
                    Lwt.return_some (wait_interrupted_guidance ~id))
          in
          let with_wait_guidance text =
            match wait_guidance with
            | Some guidance -> guidance ^ "\n\n" ^ text
            | None -> text
          in
          match Background_task.get_task ~db ~id with
          | None ->
              Lwt.return
                (Printf.sprintf "Error: No background task found with id %d" id)
          | Some task -> (
              (* If peek is requested, read and filter the log file *)
              match peek_lines with
              | Some n -> (
                  match task.log_path with
                  | None ->
                      Lwt.return
                        (with_wait_guidance
                           (Printf.sprintf
                              "Error: Task %d has no log file yet (status: %s)"
                              id
                              (Background_task.string_of_status task.status)))
                  | Some path when not (Sys.file_exists path) ->
                      Lwt.return
                        (with_wait_guidance
                           (Printf.sprintf
                              "Error: Log file does not exist yet: %s" path))
                  | Some path ->
                      let all_lines = read_file_lines path in
                      let text =
                        apply_peek ~lines:n ?regex:peek_regex ?after:peek_after
                          all_lines
                      in
                      let text = with_wait_guidance text in
                      let capped =
                        truncate_with_guidance ~max:max_result_chars text
                      in
                      Lwt.return capped)
              | None ->
                  let text = format_result task ~verbose in
                  let text = with_wait_guidance text in
                  let capped =
                    truncate_with_guidance ~max:max_result_chars text
                  in
                  Lwt.return capped));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }

(* B719: Periodic status emitter for running subagents *)

let list_running_subagents ~db =
  let all = Background_task.list_tasks ~db in
  List.filter_map
    (fun (t : Background_task.task) ->
      match (t.status, t.runner, t.use_worktree) with
      | Background_task.Running, Background_task.Local, false ->
          Some
            ( t.id,
              t.session_key,
              t.agent_name,
              t.description,
              t.started_at,
              t.prompt )
      | _ -> None)
    all

let format_subagent_status agents =
  if agents = [] then None
  else
    let lines =
      List.map
        (fun (id, _session_key, agent_name, description, started_at, _prompt) ->
          let runtime =
            match started_at with
            | Some started ->
                let start_time =
                  Background_task.parse_sqlite_datetime started
                in
                if start_time <= 0.0 then "-"
                else
                  Background_task.format_elapsed_seconds
                    (Unix.gettimeofday () -. start_time)
            | None -> "-"
          in
          let label =
            match (description, agent_name) with
            | Some d, _ when String.trim d <> "" -> String.trim d
            | _, Some a when String.trim a <> "" -> String.trim a
            | _ -> "(unnamed)"
          in
          Printf.sprintf "  - Task %d (%s): %s" id label runtime)
        agents
    in
    Some
      (Printf.sprintf "Running subagents (%d):\n%s" (List.length agents)
         (String.concat "\n" lines))

let run_subagent_status_loop ~db () =
  let open Lwt.Syntax in
  let rec loop () =
    let* () = Lwt_unix.sleep 1800.0 in
    let running = list_running_subagents ~db in
    (match format_subagent_status running with
    | Some summary -> Logs.info (fun m -> m "Subagent status: %s" summary)
    | None -> ());
    loop ()
  in
  loop ()
