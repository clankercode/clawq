open Command_bridge_helpers
open Command_bridge_session

type transcript_args = {
  id : int;
  regex : string option;
  max_lines : int option;
  export : bool;
}

let parse_transcript_args args =
  let rec loop id regex max_lines export = function
    | [] -> (
        match id with
        | Some id -> Ok { id; regex; max_lines; export }
        | None ->
            Error
              "Usage: transcript <id> [--regex PATTERN] [--max-lines N] \
               [--export]")
    | "--regex" :: value :: rest -> loop id (Some value) max_lines export rest
    | "--max-lines" :: value :: rest -> (
        try loop id regex (Some (int_of_string value)) export rest
        with _ -> Error "--max-lines must be an integer")
    | "--export" :: rest -> loop id regex max_lines true rest
    | arg :: rest -> (
        match id with
        | Some _ ->
            Error
              "Usage: transcript <id> [--regex PATTERN] [--max-lines N] \
               [--export]"
        | None -> (
            try loop (Some (int_of_string arg)) regex max_lines export rest
            with _ -> Error "background task id must be an integer"))
  in
  loop None None None false args

let cmd_background_transcript rest =
  let db = get_db () in
  Background_task.init_schema db;
  match parse_transcript_args rest with
  | Error msg -> "Error: " ^ msg
  | Ok parsed ->
      Background_task_transcript.render ~db ~id:parsed.id ?regex:parsed.regex
        ?max_lines:parsed.max_lines ~export:parsed.export ()

let require_local_subagent_task ~db id_s =
  let id = try int_of_string id_s with _ -> -1 in
  if id < 0 then Error "background task id must be an integer"
  else
    match Background_task.get_task ~db ~id with
    | None -> Error (Printf.sprintf "No background task found with id %d" id)
    | Some task when task.runner <> Background_task.Local ->
        Error
          (Printf.sprintf
             "Task %d is not a native/local subagent (runner is %s). Use \
              `clawq background ...` for non-local background tasks."
             id
             (Background_task.string_of_runner task.runner))
    | Some _ -> Ok id

let rec cmd_background args =
  match args with
  | "start" :: rest -> cmd_background ("add" :: rest)
  | [ "stop"; id_s ] -> cmd_background [ "cancel"; id_s ]
  | "send" :: id_s :: message_parts ->
      cmd_background ("message" :: id_s :: message_parts)
  | "transcript" :: rest -> cmd_background_transcript rest
  | [ "list" ] ->
      let db = get_db () in
      Background_task.init_schema db;
      let tasks, hidden = Background_task.list_tasks_for_display ~db in
      Background_task.format_task_list_with_hidden tasks hidden
  | [] ->
      let db = get_db () in
      Background_task.init_schema db;
      let tasks, hidden = Background_task.list_tasks_for_display ~db in
      let list_output =
        Background_task.format_task_list_with_hidden tasks hidden
      in
      list_output
      ^ "\n\n\
         Commands:\n\
        \  background list                                         - List all \
         tasks\n\
        \  background show <id>                                    - Show task \
         details\n\
        \  background add <codex|claude|kimi|gemini|opencode|cursor|local> \
         [--model <model>] [--agent <name>] [--host <direct|herdr|tmux>] \
         <repo> [--branch <name>] <prompt> - Queue a task\n\
        \  background start <runner> ...                           - Alias for \
         background add\n\
        \  background wait <id> [--timeout <seconds>]              - Wait for \
         completion\n\
        \  background logs <id> [--lines N] [--offset N] [--follow] - Show \
         task logs\n\
        \  background transcript <id> [--regex R] [--max-lines N] [--export] - \
         Show bounded task transcript\n\
        \  background resume <id>                                - Resume a task\n\
        \  background message <id> <message>...                  - Send a \
         message to a task\n\
        \  background send <id> <message>...                     - Alias for \
         background message\n\
        \  background cancel <id>                                  - Cancel a \
         task\n\
        \  background stop <id>                                    - Alias for \
         background cancel\n\
        \  background retry <id>                                   - Re-queue \
         a failed task\n\
        \  background recover <id> [--runner R] [--model M]        - Recover a \
         failed/stuck task with full context\n\
        \  background finalize <id>                                - Rebase \
         and fast-forward into target branch\n\
        \  background export-acp <id>                              - Export \
         ACP session history as JSONL"
  | [ "show"; id_s ] -> (
      let db = get_db () in
      Background_task.init_schema db;
      let id = try int_of_string id_s with _ -> -1 in
      if id < 0 then "Error: background task id must be an integer"
      else
        match Background_task.get_task ~db ~id with
        | None -> Printf.sprintf "No background task found with id %d" id
        | Some task -> Background_task.format_task_summary task)
  | "add" :: rest -> (
      let cfg = get_config () in
      let db = get_db () in
      Background_task.init_schema db;
      match parse_background_add_args rest with
      | Error msg -> "Error: " ^ msg
      | Ok parsed -> (
          let session_key, channel, channel_id =
            Background_task.routing_from_context ?notify_cfg:cfg.notify ()
          in
          match
            Background_task.enqueue ~db ~runner:parsed.runner
              ?model:parsed.model ?agent_name:parsed.agent_name
              ?host_kind:parsed.host_kind ~repo_path:parsed.repo_path
              ~prompt:parsed.prompt ?branch:parsed.branch ?session_key ?channel
              ?channel_id ()
          with
          | Ok id ->
              Printf.sprintf
                "Queued background task %d (%s). Use `clawq background wait \
                 %d` or `clawq background show %d` to track it."
                id
                (Background_task.string_of_runner parsed.runner)
                id id
          | Error msg -> "Error: " ^ msg))
  | "wait" :: rest -> (
      let db = get_db () in
      Background_task.init_schema db;
      match parse_background_wait_args rest with
      | Error msg -> "Error: " ^ msg
      | Ok parsed -> (
          let timeout_seconds =
            Float.min parsed.timeout_seconds Background_task.max_wait_seconds
          in
          let result =
            Lwt_main.run
              (Background_task.wait_until_terminal ~timeout_seconds ~db
                 ~id:parsed.id ())
          in
          match result with
          | Background_task.Finished task ->
              Background_task.format_task_summary task
          | Background_task.Timeout task ->
              Printf.sprintf
                "Task %d is still %s after waiting. Run `clawq background wait \
                 %d` to continue waiting, or `clawq background logs %d` to \
                 check progress.\n\n\
                 %s"
                parsed.id
                (Background_task.string_of_status task.status)
                parsed.id parsed.id
                (Background_task.format_task_summary task)
          | Background_task.Interrupted task ->
              Printf.sprintf
                "Task %d is still %s. Run `clawq background wait %d` to \
                 continue waiting, or `clawq background logs %d` to check \
                 progress.\n\n\
                 %s"
                parsed.id
                (Background_task.string_of_status task.status)
                parsed.id parsed.id
                (Background_task.format_task_summary task)
          | Background_task.Not_found ->
              Printf.sprintf "Error: No background task found with id %d"
                parsed.id))
  | "logs" :: rest -> (
      let db = get_db () in
      Background_task.init_schema db;
      match parse_background_logs_args rest with
      | Error msg -> "Error: " ^ msg
      | Ok parsed when parsed.follow && parsed.offset > 0 ->
          "Error: --follow and --offset cannot be used together"
      | Ok parsed when parsed.follow -> (
          let result =
            Lwt_main.run
              (Background_task.log_follow ~db ~id:parsed.id
                 ~initial_lines:parsed.lines ())
          in
          match result with Ok () -> "" | Error msg -> "Error: " ^ msg)
      | Ok parsed -> (
          match Background_task.get_task ~db ~id:parsed.id with
          | None ->
              Printf.sprintf "Error: No background task found with id %d"
                parsed.id
          | Some task -> (
              let db = get_db () in
              match
                Background_task.log_excerpt ~db ~offset:parsed.offset
                  ~lines:parsed.lines task
              with
              | Ok text -> text
              | Error msg -> "Error: " ^ msg)))
  | [ "resume"; id_s ] -> (
      let db = get_db () in
      Background_task.init_schema db;
      let id = try int_of_string id_s with _ -> -1 in
      if id < 0 then "Error: background task id must be an integer"
      else
        match Background_task.request_resume ~db ~id ~message:None with
        | Ok msg -> msg
        | Error msg -> "Error: " ^ msg)
  | "message" :: id_s :: message_parts -> (
      let db = get_db () in
      Background_task.init_schema db;
      let id = try int_of_string id_s with _ -> -1 in
      let message = String.concat " " message_parts |> String.trim in
      if id < 0 then "Error: background task id must be an integer"
      else if message = "" then
        "Usage: clawq background message <id> <message...>"
      else
        match
          Background_task.request_resume ~db ~id ~message:(Some message)
        with
        | Ok msg -> msg
        | Error msg -> "Error: " ^ msg)
  | [ "cancel"; id_s ] -> (
      let db = get_db () in
      Background_task.init_schema db;
      let id = try int_of_string id_s with _ -> -1 in
      if id < 0 then "Error: background task id must be an integer"
      else
        match Background_task.cancel ~db ~id with
        | Ok msg -> msg
        | Error msg -> "Error: " ^ msg)
  | [ "retry"; id_s ] -> (
      let db = get_db () in
      Background_task.init_schema db;
      let id = try int_of_string id_s with _ -> -1 in
      if id < 0 then "Error: background task id must be an integer"
      else
        match Background_task.retry ~db ~id with
        | Ok msg -> msg
        | Error msg -> "Error: " ^ msg)
  | "recover" :: id_s :: rest -> (
      let db = get_db () in
      Background_task.init_schema db;
      let id = try int_of_string id_s with _ -> -1 in
      if id < 0 then "Error: background task id must be an integer"
      else
        let runner =
          match rest with
          | "--runner" :: r :: _ -> Background_task.runner_of_string r
          | _ -> None
        in
        let model =
          match rest with
          | "--model" :: m :: _ -> Some m
          | _ :: "--model" :: m :: _ -> Some m
          | _ :: _ :: "--model" :: m :: _ -> Some m
          | _ -> None
        in
        match Background_task.recover ~db ~id ?runner ?model () with
        | Ok (new_id, effective_runner) ->
            Printf.sprintf
              "Recovered task %d → new task %d (%s). Use `clawq background \
               show %d` to track it."
              id new_id
              (Background_task.string_of_runner effective_runner)
              new_id
        | Error msg -> "Error: " ^ msg)
  | [ "export-acp"; id_s ] ->
      let db = get_db () in
      Background_task.init_schema db;
      let id = try int_of_string id_s with _ -> -1 in
      if id < 0 then "Error: background task id must be an integer"
      else if not (Acp_history.has_history ~db ~task_id:id) then
        Printf.sprintf "No ACP history found for task %d" id
      else Acp_history.export_jsonl ~db ~task_id:id
  | [ "finalize"; id_s ] | "finalize" :: id_s :: _ -> (
      let db = get_db () in
      Background_task.init_schema db;
      let id = try int_of_string id_s with _ -> -1 in
      if id < 0 then "Error: background task id must be an integer"
      else
        match Background_task.get_task ~db ~id with
        | None -> Printf.sprintf "Error: no background task found with id %d" id
        | Some task when task.worktree_path = None ->
            Printf.sprintf
              "Error: task %d has no worktree — nothing to finalize" id
        | Some task ->
            let result = Lwt_main.run (Worktree_merge.finalize_task ~db task) in
            Worktree_merge.format_result result)
  | _ ->
      "Usage: clawq background \
       <list|show|add|start|wait|logs|transcript|resume|message|send|cancel|stop|retry|recover|finalize|export-acp>\n\
      \  background list                                         - List queued \
       and completed tasks\n\
      \  background show <id>                                    - Show task \
       details\n\
      \  background add <codex|claude|kimi|gemini|opencode|cursor|local> \
       [--model <model>] [--agent <name>] <repo> [--branch <name>] <prompt> - \
       Queue a background runner\n\
      \  background start <runner> ...                           - Alias for \
       background add\n\
      \  background wait <id> [--timeout <seconds>]              - Wait for a \
       task to finish\n\
      \  background logs <id> [--lines N] [--offset N] [--follow] - Show task \
       log lines\n\
      \  background transcript <id> [--regex R] [--max-lines N] [--export] - \
       Show bounded task transcript\n\
      \  background resume <id>                                  - Resume a \
       started task with the runner's native session support\n\
      \  background message <id> <message...>                    - Inject a \
       chat message into a started task and resume it\n\
      \  background send <id> <message...>                       - Alias for \
       background message\n\
      \  background cancel <id>                                  - Cancel a \
       queued/running task\n\
      \  background stop <id>                                    - Alias for \
       background cancel\n\
      \  background retry <id>                                   - Re-queue a \
       failed task (max 3 retries)\n\
      \  background recover <id> [--runner R] [--model M]        - Recover a \
       failed/stuck task with full context\n\
      \  background finalize <id>                                - Rebase and \
       fast-forward into target branch\n\
      \  background export-acp <id>                              - Export ACP \
       session history as JSONL"

let cmd_subagents args =
  match args with
  | [] ->
      "Usage: clawq subagents <list|start|stop|send|transcript>\n\
      \  subagents list\n\
      \  subagents start [--model M] [--agent NAME] <repo> <prompt...>\n\
      \  subagents stop <id>\n\
      \  subagents send <id> <message...>\n\
      \  subagents transcript <id> [--regex R] [--max-lines N] [--export]"
  | [ "list" ] ->
      let db = get_db () in
      Background_task.init_schema db;
      Background_task.list_tasks ~db
      |> List.filter (fun (task : Background_task.task) ->
          task.runner = Background_task.Local)
      |> fun tasks -> Background_task.format_task_list_with_hidden tasks 0
  | "start" :: rest -> (
      let cfg = get_config () in
      let db = get_db () in
      let workspace = Runtime_config.effective_workspace cfg in
      ignore (Agent_template.init_cache ~workspace_dir:workspace ());
      Background_task.init_schema db;
      match parse_background_add_args ("local" :: rest) with
      | Error msg -> "Error: " ^ msg
      | Ok parsed -> (
          match parsed.agent_name with
          | Some name when Agent_template.resolve name = None ->
              Printf.sprintf "Error: agent template '%s' not found" name
          | _ -> (
              let session_key, channel, channel_id =
                Background_task.routing_from_context ?notify_cfg:cfg.notify ()
              in
              match
                Background_task.enqueue ~db ~runner:Background_task.Local
                  ?model:parsed.model ?agent_name:parsed.agent_name
                  ~repo_path:parsed.repo_path ~prompt:parsed.prompt
                  ?branch:parsed.branch ?session_key ?channel ?channel_id ()
              with
              | Ok id ->
                  Printf.sprintf
                    "Queued subagent task %d (local). Use `clawq subagents \
                     transcript %d` to inspect its bounded transcript."
                    id id
              | Error msg -> "Error: " ^ msg)))
  | [ "stop"; id_s ] -> (
      let db = get_db () in
      Background_task.init_schema db;
      match require_local_subagent_task ~db id_s with
      | Error msg -> "Error: " ^ msg
      | Ok _ -> cmd_background [ "cancel"; id_s ])
  | "send" :: id_s :: message_parts -> (
      let db = get_db () in
      Background_task.init_schema db;
      match require_local_subagent_task ~db id_s with
      | Error msg -> "Error: " ^ msg
      | Ok _ -> cmd_background ("message" :: id_s :: message_parts))
  | "transcript" :: rest -> (
      let db = get_db () in
      Background_task.init_schema db;
      match parse_transcript_args rest with
      | Error msg -> "Error: " ^ msg
      | Ok parsed -> (
          match require_local_subagent_task ~db (string_of_int parsed.id) with
          | Error msg -> "Error: " ^ msg
          | Ok _ ->
              Background_task_transcript.render ~db ~id:parsed.id
                ?regex:parsed.regex ?max_lines:parsed.max_lines
                ~export:parsed.export ()))
  | _ ->
      "Usage: clawq subagents <list|start|stop|send|transcript>\n\
      \  subagents list\n\
      \  subagents start [--model M] [--agent NAME] <repo> <prompt...>\n\
      \  subagents stop <id>\n\
      \  subagents send <id> <message...>\n\
      \  subagents transcript <id> [--regex R] [--max-lines N] [--export]"

let cmd_delegate args =
  let cfg = get_config () in
  let db = get_db () in
  Background_task.init_schema db;
  match parse_delegate_args args with
  | Error msg -> "Error: " ^ msg
  | Ok parsed -> (
      match
        Background_task.delegate_enqueue ~db ?notify_cfg:cfg.notify
          ?preferred_runner:parsed.preferred_runner ?model:parsed.model
          ?repo_path:parsed.repo_path ?branch:parsed.branch
          ~use_worktree:parsed.use_worktree
          ~default_repo_path:(default_delegate_repo_path cfg)
          ~goal:parsed.goal ()
      with
      | Ok (id, runner, repo_path) ->
          Printf.sprintf
            "Delegated task %d (%s) for %s. Use `clawq background wait %d` or \
             `clawq background show %d` to track it."
            id
            (Background_task.string_of_runner runner)
            repo_path id id
      | Error msg -> "Error: " ^ msg)

(* B774: subscriber-worker CLI. *)
let worker_usage =
  "Usage:\n\
  \  clawq worker run --server URL --id WORKER_ID --repos o/r[,o2/r2] [--token \
   T] [--runners codex,claude] [--hosts herdr,direct] [--poll SECS] [--lease \
   SECS] [--once]\n\
  \  clawq worker status --server URL [--token T]"

let parse_worker_flags args =
  let tbl = Hashtbl.create 8 in
  let flags = ref [] in
  let rec loop = function
    | [] -> Ok ()
    | key :: value :: rest
      when String.length key > 2
           && String.sub key 0 2 = "--"
           && value <> ""
           && not (String.length value > 2 && String.sub value 0 2 = "--") ->
        Hashtbl.replace tbl key value;
        loop rest
    | key :: rest when String.length key > 2 && String.sub key 0 2 = "--" ->
        flags := key :: !flags;
        loop rest
    | arg :: _ -> Error (Printf.sprintf "unexpected argument %S" arg)
  in
  Result.map (fun () -> (tbl, !flags)) (loop args)

let split_csv value =
  String.split_on_char ',' value
  |> List.map String.trim
  |> List.filter (fun s -> s <> "")

let cmd_worker args =
  match args with
  | "status" :: rest -> (
      match parse_worker_flags rest with
      | Error msg -> "Error: " ^ msg ^ "\n" ^ worker_usage
      | Ok (tbl, _) -> (
          match Hashtbl.find_opt tbl "--server" with
          | None -> "Error: --server URL is required\n" ^ worker_usage
          | Some server ->
              let headers =
                match Hashtbl.find_opt tbl "--token" with
                | Some t -> [ ("Authorization", "Bearer " ^ t) ]
                | None -> []
              in
              let status, body =
                Lwt_main.run
                  (Http_client.get ~uri:(server ^ "/worker/status") ~headers)
              in
              if status = 200 then body
              else Printf.sprintf "Error: HTTP %d %s" status body))
  | "run" :: rest | "once" :: rest -> (
      let once = args <> [] && List.hd args = "once" in
      match parse_worker_flags rest with
      | Error msg -> "Error: " ^ msg ^ "\n" ^ worker_usage
      | Ok (tbl, flags) -> (
          let get key = Hashtbl.find_opt tbl key in
          match (get "--server", get "--id", get "--repos") with
          | None, _, _ -> "Error: --server URL is required\n" ^ worker_usage
          | _, None, _ ->
              "Error: --id WORKER_ID (stable identity) is required\n"
              ^ worker_usage
          | _, _, None ->
              "Error: --repos owner/repo[,owner2/repo2] is required (workers \
               only see repositories they are allowed to serve)\n"
              ^ worker_usage
          | Some server, Some worker_id, Some repos ->
              let cfg_runtime = get_config () in
              let capabilities =
                {
                  Work_item_lease.worker_id;
                  runners =
                    (match get "--runners" with
                    | Some v -> split_csv v
                    | None -> [ "claude"; "codex" ]);
                  hosts =
                    (match get "--hosts" with
                    | Some v -> split_csv v
                    | None -> [ "herdr"; "direct" ]);
                  repos = split_csv repos;
                  max_concurrent = 1;
                }
              in
              let cfg =
                {
                  Worker_client.server;
                  token = Option.value (get "--token") ~default:"";
                  capabilities;
                  poll_seconds =
                    (match get "--poll" with
                    | Some v -> ( try float_of_string v with _ -> 15.0)
                    | None -> 15.0);
                  lease_seconds =
                    (match get "--lease" with
                    | Some v -> ( try int_of_string v with _ -> 120)
                    | None -> 120);
                  isolation =
                    Runner_isolation.policy_of_config cfg_runtime.security;
                }
              in
              let once = once || List.mem "--once" flags in
              if once then
                match Lwt_main.run (Worker_client.run_once ~cfg ()) with
                | Ok msg -> msg
                | Error msg -> "Error: " ^ msg
              else begin
                Printf.printf
                  "worker %s polling %s every %.0fs (Ctrl-C to stop)\n%!"
                  worker_id server cfg.poll_seconds;
                Lwt_main.run (Worker_client.run_loop ~cfg ());
                "worker loop exited"
              end))
  | _ -> worker_usage
