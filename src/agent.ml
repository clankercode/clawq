include Agent_0_compact

let restart_interrupt_token = "__clawq_restart__"
let queued_message_interrupt_token = "[queued inbound message]"

let is_queued_message_interrupt = function
  | Some reason when reason = queued_message_interrupt_token -> true
  | _ -> false

let active_workspace_files_for_config (config : Runtime_config.t) =
  let workspace = Runtime_config.effective_workspace config in
  let normalize_workspace_path path =
    let resolved =
      if Filename.is_relative path then Filename.concat workspace path else path
    in
    Path_util.normalize_path resolved
  in
  List.map
    (fun file -> (file, normalize_workspace_path file))
    config.prompt.workspace_files

let capture_active_workspace_file_state_for_config (config : Runtime_config.t) =
  active_workspace_files_for_config config
  |> List.map (fun (file, path) ->
      let digest =
        try Some (Digest.to_hex (Digest.file path)) with _ -> None
      in
      (file, digest))

let create ~config ?tool_registry ?agent_template ?cwd () =
  let system_prompt =
    Prompt_builder.build ~config ~tool_registry ?agent_template ()
  in
  let ws_doc_digests =
    match agent_template with
    | None -> Prompt_builder.workspace_doc_content_digests ~config ()
    | Some _ -> []
  in
  let pd =
    Prompt_builder.build_project_docs_message ~config ~ws_doc_digests ()
  in
  let dirs_seen = Hashtbl.create 16 in
  (match pd.git_root with
  | Some root -> Hashtbl.replace dirs_seen root true
  | None -> ());
  {
    history = [];
    config;
    system_prompt;
    observed_active_workspace_files =
      capture_active_workspace_file_state_for_config config;
    last_request_history_len = None;
    tool_registry;
    agent_template;
    compacted_mid_turn = false;
    effective_cwd = cwd;
    project_docs_content = pd.content;
    project_docs_digests = pd.digests;
    project_docs_subdir_digests = [];
    project_docs_git_root = pd.git_root;
    project_doc_dirs_seen = dirs_seen;
    on_project_doc_loaded = None;
  }

let inject_runtime_context messages runtime_context =
  let rec loop rev_prefix = function
    | [] -> List.rev rev_prefix
    | (msg : Provider.message) :: rest when msg.role = "user" ->
        List.rev_append rev_prefix
          ({ msg with content = runtime_context ^ "\n\n" ^ msg.content } :: rest)
    | msg :: rest -> loop (msg :: rev_prefix) rest
  in
  loop [] (List.rev messages) |> List.rev

let build_messages ?runtime_context agent =
  agent.system_prompt <-
    Prompt_builder.build ~config:agent.config ~tool_registry:agent.tool_registry
      ?agent_template:agent.agent_template ();
  let sys = Provider.make_message ~role:"system" ~content:agent.system_prompt in
  let dev =
    match agent.project_docs_content with
    | Some content -> [ Provider.make_message ~role:"developer" ~content ]
    | None -> []
  in
  let messages =
    (sys :: dev) @ List.rev (runtime_history_messages agent.history)
  in
  match runtime_context with
  | Some block when String.trim block <> "" ->
      inject_runtime_context messages block
  | _ -> messages

let estimate_history_delta_tokens ~history ~previous_request_history_len
    ~current_request_history_len =
  match previous_request_history_len with
  | None -> None
  | Some prev_len ->
      let added_messages = max 0 (current_request_history_len - prev_len) in
      if added_messages = 0 then None
      else
        let rec take n acc = function
          | _ when n <= 0 -> List.rev acc
          | [] -> List.rev acc
          | msg :: rest -> take (n - 1) (msg :: acc) rest
        in
        let total =
          history |> take added_messages []
          |> List.filter (fun (msg : Provider.message) -> msg.role <> "event")
          |> List.fold_left (fun acc msg -> acc + estimate_message_tokens msg) 0
        in
        if total > 0 then Some total else None

let runtime_context_usage agent ~compacted_before_turn =
  let context_window_tokens = context_window_for_agent agent in
  {
    Prompt_builder.history_messages = List.length agent.history;
    estimated_history_tokens = estimate_history_tokens agent.history;
    context_window_tokens;
    compaction_threshold_tokens = compaction_threshold_for_agent agent;
    max_messages_per_session = effective_max_messages agent;
    compacted_before_turn;
  }

let tools_json agent =
  match agent.tool_registry with
  | Some r when agent.config.security.tools_enabled ->
      if agent.config.agent_defaults.tool_search_enabled then
        Some (Tool_registry.to_openai_json_with_search r)
      else Some (Tool_registry.to_openai_json r)
  | _ -> None

let risk_level_to_string = function
  | Tool.Low -> "low"
  | Tool.Medium -> "medium"
  | Tool.High -> "high"

let active_workspace_files agent =
  active_workspace_files_for_config agent.config

let capture_active_workspace_file_state agent =
  capture_active_workspace_file_state_for_config agent.config

let changed_active_workspace_files before after =
  List.filter_map
    (fun (file, before_digest) ->
      let after_digest =
        match List.assoc_opt file after with Some d -> d | None -> None
      in
      if before_digest <> after_digest then Some file else None)
    before

let dedup_preserve_order items =
  let rec loop seen acc = function
    | [] -> List.rev acc
    | item :: rest when List.mem item seen -> loop seen acc rest
    | item :: rest -> loop (item :: seen) (item :: acc) rest
  in
  loop [] [] items

let workspace_refresh_event filenames =
  Provider.make_message ~role:"event"
    ~content:
      (Printf.sprintf
         "[workspace context refreshed after active workspace file update: %s]"
         (String.concat ", " filenames))

type workspace_refresh_observation = {
  message : Provider.message option;
  after_state : (string * string option) list;
}

let active_workspace_refresh_targets_from_call agent (tc : Provider.tool_call)
    result =
  if String.starts_with ~prefix:"Error:" result then None
  else
    let configured = active_workspace_files agent in
    let find_configured_file resolved_path =
      List.find_map
        (fun (file, configured_path) ->
          if resolved_path = configured_path then Some file else None)
        configured
    in
    try
      let open Yojson.Safe.Util in
      let args = Yojson.Safe.from_string tc.arguments in
      match tc.function_name with
      | "doc_write" ->
          let filename = args |> member "filename" |> to_string in
          if List.mem filename agent.config.prompt.workspace_files then
            Some [ filename ]
          else None
      | "file_write" | "file_append" | "file_edit" | "file_edit_lines" ->
          let workspace = Runtime_config.effective_workspace agent.config in
          let normalize_workspace_path path =
            let resolved =
              if Filename.is_relative path then Filename.concat workspace path
              else path
            in
            Path_util.normalize_path resolved
          in
          let path = args |> member "path" |> to_string in
          let resolved_path = normalize_workspace_path path in
          Option.map (fun file -> [ file ]) (find_configured_file resolved_path)
      | _ -> None
    with _ -> None

let observe_workspace_refresh agent tc result ~before_active_workspace_files =
  let direct_targets =
    match active_workspace_refresh_targets_from_call agent tc result with
    | Some files -> files
    | None -> []
  in
  let after_state = capture_active_workspace_file_state agent in
  let changed_targets =
    changed_active_workspace_files before_active_workspace_files after_state
  in
  let refreshed_files =
    dedup_preserve_order (direct_targets @ changed_targets)
  in
  let message =
    if String.starts_with ~prefix:"Error:" result then None
    else
      match refreshed_files with
      | [] -> None
      | filenames -> Some (workspace_refresh_event filenames)
  in
  { message; after_state }

let sync_observed_active_workspace_files agent =
  agent.observed_active_workspace_files <-
    capture_active_workspace_file_state agent

let restore_observed_active_workspace_files agent
    observed_active_workspace_files =
  let current_state = capture_active_workspace_file_state agent in
  agent.observed_active_workspace_files <-
    List.map
      (fun (file, current_digest) ->
        let restored_digest =
          match List.assoc_opt file observed_active_workspace_files with
          | Some digest -> digest
          | None -> current_digest
        in
        (file, restored_digest))
      current_state

let note_external_workspace_refresh_if_needed agent =
  let before_state = agent.observed_active_workspace_files in
  let after_state = capture_active_workspace_file_state agent in
  agent.observed_active_workspace_files <- after_state;
  match changed_active_workspace_files before_state after_state with
  | [] -> None
  | filenames ->
      let refresh_msg = workspace_refresh_event filenames in
      agent.history <- refresh_msg :: agent.history;
      Some refresh_msg

let refresh_project_docs_if_changed agent =
  if not agent.config.prompt.include_project_docs then None
  else
    let ws_doc_digests =
      match agent.agent_template with
      | None ->
          Prompt_builder.workspace_doc_content_digests ~config:agent.config ()
      | Some _ -> []
    in
    let pd =
      Prompt_builder.build_project_docs_message ~config:agent.config
        ~ws_doc_digests ()
    in
    let new_digests = pd.digests in
    if new_digests = agent.project_docs_digests then None
    else begin
      agent.project_docs_content <- pd.content;
      agent.project_docs_digests <- pd.digests;
      let event_msg =
        Provider.make_message ~role:"event"
          ~content:
            "[project instructions refreshed: root CLAUDE.md/AGENTS.md changed \
             since last turn]"
      in
      agent.history <- event_msg :: agent.history;
      (match agent.on_project_doc_loaded with
      | Some notify ->
          Lwt.async (fun () ->
              Lwt.catch
                (fun () ->
                  notify "Project instructions refreshed (files changed)")
                (fun _ -> Lwt.return_unit))
      | None -> ());
      Some event_msg
    end

let observe_project_docs agent (tc : Provider.tool_call) =
  if not agent.config.prompt.include_project_docs then []
  else
    let dir =
      match tc.function_name with
      | "file_read" | "file_write" | "file_edit" | "file_edit_lines"
      | "file_append" -> (
          try
            let args = Yojson.Safe.from_string tc.arguments in
            let open Yojson.Safe.Util in
            let path = args |> member "path" |> to_string in
            Some (Filename.dirname path)
          with _ -> None)
      | _ -> None
    in
    match dir with
    | None -> []
    | Some dir ->
        if Hashtbl.mem agent.project_doc_dirs_seen dir then []
        else begin
          Hashtbl.replace agent.project_doc_dirs_seen dir true;
          let git_root =
            match agent.project_docs_git_root with Some r -> r | None -> ""
          in
          let relative_label path =
            if
              git_root <> ""
              && String.length path > String.length git_root
              && String.sub path 0 (String.length git_root) = git_root
            then
              String.sub path
                (String.length git_root + 1)
                (String.length path - String.length git_root - 1)
            else path
          in
          List.filter_map
            (fun filename ->
              let path = Filename.concat dir filename in
              if Sys.file_exists path then begin
                let content =
                  Prompt_builder.read_file_limited path
                    agent.config.prompt.max_project_doc_chars
                in
                if content <> "" then begin
                  let digest = Digest.to_hex (Digest.string content) in
                  if
                    List.mem digest agent.project_docs_digests
                    || List.mem digest agent.project_docs_subdir_digests
                  then None
                  else begin
                    agent.project_docs_subdir_digests <-
                      digest :: agent.project_docs_subdir_digests;
                    let label = relative_label path in
                    let ts = Prompt_builder.now_utc_iso8601 () in
                    (match agent.on_project_doc_loaded with
                    | Some notify ->
                        Lwt.async (fun () ->
                            Lwt.catch
                              (fun () ->
                                notify
                                  (Printf.sprintf
                                     "Loaded project instructions: %s" label))
                              (fun _ -> Lwt.return_unit))
                    | None -> ());
                    Some
                      (Provider.make_message ~role:"user"
                         ~content:
                           (Printf.sprintf
                              "[system: project instructions loaded: %s from \
                               %s, loaded at %s]\n\n\
                               %s"
                              label path ts content))
                  end
                end
                else None
              end
              else None)
            Prompt_builder.project_doc_filenames
        end

let append_tool_history agent tool_msg refresh_msg_opt =
  agent.history <- tool_msg :: agent.history;
  match refresh_msg_opt with
  | Some refresh_msg ->
      agent.history <- refresh_msg :: agent.history;
      agent.observed_active_workspace_files <-
        capture_active_workspace_file_state agent
  | None -> ()

(* Handle a tool_search call by searching the registry and returning
   matching tool definitions as a tool_search_output message. *)
let resolve_tool_search agent (tc : Provider.tool_call) =
  let query =
    try
      let args = Yojson.Safe.from_string tc.arguments in
      let open Yojson.Safe.Util in
      try args |> member "query" |> to_string
      with _ -> (
        try args |> member "goal" |> to_string
        with _ -> Yojson.Safe.to_string args)
    with _ -> tc.arguments
  in
  match agent.tool_registry with
  | None ->
      Provider.make_tool_result ~tool_call_id:tc.id ~name:"tool_search"
        ~content:"No tool registry available"
  | Some registry ->
      let results = Tool_registry.search registry ~query in
      let top = List.filteri (fun i _ -> i < 10) results in
      let tools_json = `List (List.map Tool_registry.tool_to_openai_json top) in
      Logs.info (fun m ->
          m "Tool search query=%S found=%d tools" query (List.length top));
      Provider.make_tool_search_result ~tool_call_id:tc.id ~tools_json

let summarize_history_for_wipe history =
  let lines = ref [] in
  let add line = lines := line :: !lines in
  add "[Prior context summary]";
  List.iter
    (fun (msg : Provider.message) ->
      match msg.role with
      | "assistant" ->
          List.iter
            (fun (tc : Provider.tool_call) ->
              let args_preview =
                let s = tc.arguments in
                if String.length s > 60 then String.sub s 0 60 ^ "..." else s
              in
              add (Printf.sprintf "- %s(%s)" tc.function_name args_preview))
            msg.tool_calls;
          if msg.content <> "" then begin
            let preview =
              if String.length msg.content > 80 then
                String.sub msg.content 0 80 ^ "..."
              else msg.content
            in
            add ("- assistant: " ^ preview)
          end
      | "tool" ->
          let name = match msg.name with Some n -> n | None -> "tool" in
          let preview =
            if String.length msg.content > 60 then
              String.sub msg.content 0 60 ^ "..."
            else msg.content
          in
          add (Printf.sprintf "- %s result: %s" name preview)
      | _ -> ())
    (List.rev history);
  String.concat "\n" (List.rev !lines)

let perform_cwd_history_wipe agent =
  let reversed = List.rev agent.history in
  let first_user_msg =
    List.find_opt (fun (m : Provider.message) -> m.role = "user") reversed
  in
  let summary_text = summarize_history_for_wipe agent.history in
  let summary_msg =
    Provider.make_message ~role:"system" ~content:summary_text
  in
  agent.history <-
    (match first_user_msg with
    | Some msg -> [ summary_msg; msg ]
    | None -> [ summary_msg ])

(* Execute tool calls in order so workspace refresh events can attribute active
   prompt-file updates to the specific tool call that triggered them. *)
let execute_tool_calls_stream agent ~db ~audit_enabled ~session_key
    ?interrupt_check ?on_tool_round_complete ~on_chunk calls =
  let open Lwt.Syntax in
  let sk_tag = match session_key with Some s -> "[" ^ s ^ "] " | None -> "" in
  let notification_promises = ref [] in
  let notify_async thunk =
    notification_promises :=
      Lwt.catch thunk (fun exn ->
          Logs.warn (fun m ->
              m "Notification error: %s" (Printexc.to_string exn));
          Lwt.return_unit)
      :: !notification_promises
  in
  let pending_history_wipe = ref false in
  let interrupted = ref false in
  let check_interrupt () =
    if !interrupted then true
    else
      match interrupt_check with
      | Some check -> (
          match check () with
          | Some reason when reason = queued_message_interrupt_token -> false
          | Some _ ->
              interrupted := true;
              true
          | None -> false)
      | None -> false
  in
  let* results =
    Lwt_list.map_s
      (fun (tc : Provider.tool_call) ->
        Logs.info (fun m ->
            m "%sTool call: %s (id=%s) args=%s" sk_tag tc.function_name tc.id
              tc.arguments);
        (match (db, audit_enabled, session_key) with
        | Some db, true, Some sk ->
            let risk =
              match agent.tool_registry with
              | Some reg -> (
                  match Tool_registry.find reg tc.function_name with
                  | Some t -> risk_level_to_string t.risk_level
                  | None -> "unknown")
              | None -> "unknown"
            in
            Audit.log ~db
              (ToolInvocation
                 {
                   session_key = sk;
                   tool_name = tc.function_name;
                   risk_level = risk;
                   args_preview = tc.arguments;
                 })
        | _ -> ());
        notify_async (fun () ->
            on_chunk
              (Provider.ToolStart
                 {
                   id = tc.id;
                   name = tc.function_name;
                   arguments = tc.arguments;
                 }));
        if check_interrupt () then begin
          Logs.info (fun m ->
              m "%sSkipping tool %s (interrupted)" sk_tag tc.function_name);
          (match (db, audit_enabled, session_key) with
          | Some db, true, Some sk ->
              Audit.log ~db
                (ToolResult
                   {
                     session_key = sk;
                     tool_name = tc.function_name;
                     success = false;
                   })
          | _ -> ());
          let result_msg =
            Provider.make_tool_result ~tool_call_id:tc.id ~name:tc.function_name
              ~content:"[skipped: interrupted by user]"
          in
          notify_async (fun () ->
              on_chunk
                (Provider.ToolResult
                   {
                     id = tc.id;
                     name = tc.function_name;
                     result = "[skipped: interrupted by user]";
                     is_error = false;
                   }));
          Lwt.return (tc, result_msg, None)
        end
        else
          let before_active_workspace_files =
            capture_active_workspace_file_state agent
          in
          let is_tool_search = tc.function_name = "tool_search" in
          let streamed_output = ref false in
          let t0 = Unix.gettimeofday () in
          let* result_msg, result_for_event =
            if is_tool_search then
              let msg = resolve_tool_search agent tc in
              Lwt.return (msg, msg.Provider.content)
            else
              let* result =
                match agent.tool_registry with
                | None -> Lwt.return "Error: no tool registry available"
                | Some registry -> (
                    match Tool_registry.find registry tc.function_name with
                    | None ->
                        Lwt.return
                          (Printf.sprintf "Error: unknown tool '%s'"
                             tc.function_name)
                    | Some tool ->
                        Lwt.catch
                          (fun () ->
                            let args =
                              try Yojson.Safe.from_string tc.arguments
                              with _ ->
                                Logs.warn (fun m ->
                                    m
                                      "Tool call '%s': failed to parse \
                                       arguments as JSON (raw: %s)"
                                      tc.function_name tc.arguments);
                                `Assoc []
                            in
                            match Tool.validate_required_params tool args with
                            | Error msg -> Lwt.return msg
                            | Ok () -> (
                                let context =
                                  {
                                    Tool.session_key;
                                    send_progress =
                                      Some
                                        (fun text ->
                                          streamed_output := true;
                                          on_chunk
                                            (Provider.ToolOutputDelta
                                               { id = tc.id; chunk = text }));
                                    interrupt_check;
                                    inject_system_messages =
                                      Some
                                        (fun msgs ->
                                          let msgs =
                                            Skill_dedup.dedup_skill_injections
                                              ~history:agent.history msgs
                                          in
                                          List.iter
                                            (fun content ->
                                              agent.history <-
                                                Provider.make_message
                                                  ~role:"system" ~content
                                                :: agent.history)
                                            msgs);
                                    effective_cwd = agent.effective_cwd;
                                    request_cwd_change =
                                      Some
                                        (fun new_cwd wipe ->
                                          agent.effective_cwd <- Some new_cwd;
                                          if wipe then
                                            pending_history_wipe := true);
                                  }
                                in
                                match tool.invoke_stream with
                                | Some invoke_stream ->
                                    invoke_stream ~context
                                      ~on_output_chunk:(fun chunk ->
                                        streamed_output := true;
                                        on_chunk
                                          (Provider.ToolOutputDelta
                                             { id = tc.id; chunk }))
                                      args
                                | None -> tool.invoke ~context args))
                          (fun exn ->
                            Lwt.return
                              ("Error invoking tool: " ^ Printexc.to_string exn))
                    )
              in
              let* result_for_history =
                Tool_postprocess.process_tool_result ~config:agent.config ~db
                  ~session_key ~tool_name:tc.function_name
                  ~history:agent.history ~raw_result:result
              in
              let result_for_event =
                if !streamed_output then result_for_history else result
              in
              Lwt.return
                ( Provider.make_tool_result ~tool_call_id:tc.id
                    ~name:tc.function_name ~content:result_for_history,
                  result_for_event )
          in
          let invoke_duration = Unix.gettimeofday () -. t0 in
          Logs.info (fun m ->
              m "%sTool %s completed in %.3fs" sk_tag tc.function_name
                invoke_duration);
          let result = result_msg.Provider.content in
          let success =
            not (String.starts_with ~prefix:"Error:" result_for_event)
          in
          (match (db, audit_enabled, session_key) with
          | Some db, true, Some sk ->
              Audit.log ~db
                (ToolResult
                   { session_key = sk; tool_name = tc.function_name; success })
          | _ -> ());
          let preview =
            let limit = if success then 200 else 1000 in
            if String.length result > limit then
              String.sub result 0 limit ^ "..."
            else result
          in
          if success then
            Logs.info (fun m ->
                m "%sTool result: %s -> %s" sk_tag tc.function_name preview)
          else
            Logs.warn (fun m ->
                m "%sTool error: %s -> %s" sk_tag tc.function_name preview);
          notify_async (fun () ->
              on_chunk
                (Provider.ToolResult
                   {
                     id = tc.id;
                     name = tc.function_name;
                     result = result_for_event;
                     is_error = not success;
                   }));
          let refresh =
            observe_workspace_refresh agent tc result
              ~before_active_workspace_files
          in
          if Option.is_some refresh.message then
            agent.observed_active_workspace_files <- refresh.after_state;
          Lwt.return (tc, result_msg, refresh.message))
      calls
  in
  List.iter
    (fun ((_tc : Provider.tool_call), tool_msg, refresh_msg) ->
      append_tool_history agent tool_msg refresh_msg)
    results;
  if !pending_history_wipe then perform_cwd_history_wipe agent;
  List.iter
    (fun ((tc : Provider.tool_call), _, _) ->
      let doc_events = observe_project_docs agent tc in
      List.iter (fun msg -> agent.history <- msg :: agent.history) doc_events)
    results;
  let* () = Lwt.join !notification_promises in
  let* () =
    match on_tool_round_complete with
    | Some cb ->
        let pairs =
          List.map
            (fun (tc, (rm : Provider.message), _) -> (tc, rm.content))
            results
        in
        cb pairs
    | None -> Lwt.return_unit
  in
  Lwt.return_unit

let execute_tool_calls agent ~db ~audit_enabled ~session_key ?interrupt_check
    ?on_tool_round_complete calls =
  let open Lwt.Syntax in
  let sk_tag = match session_key with Some s -> "[" ^ s ^ "] " | None -> "" in
  let pending_history_wipe = ref false in
  let interrupted = ref false in
  let check_interrupt () =
    if !interrupted then true
    else
      match interrupt_check with
      | Some check -> (
          match check () with
          | Some reason when reason = queued_message_interrupt_token -> false
          | Some _ ->
              interrupted := true;
              true
          | None -> false)
      | None -> false
  in
  let* results =
    Lwt_list.map_s
      (fun (tc : Provider.tool_call) ->
        Logs.info (fun m ->
            m "%sTool call: %s (id=%s) args=%s" sk_tag tc.function_name tc.id
              tc.arguments);
        (match (db, audit_enabled, session_key) with
        | Some db, true, Some sk ->
            let risk =
              match agent.tool_registry with
              | Some reg -> (
                  match Tool_registry.find reg tc.function_name with
                  | Some t -> risk_level_to_string t.risk_level
                  | None -> "unknown")
              | None -> "unknown"
            in
            Audit.log ~db
              (ToolInvocation
                 {
                   session_key = sk;
                   tool_name = tc.function_name;
                   risk_level = risk;
                   args_preview = tc.arguments;
                 })
        | _ -> ());
        if check_interrupt () then begin
          Logs.info (fun m ->
              m "%sSkipping tool %s (interrupted)" sk_tag tc.function_name);
          (match (db, audit_enabled, session_key) with
          | Some db, true, Some sk ->
              Audit.log ~db
                (ToolResult
                   {
                     session_key = sk;
                     tool_name = tc.function_name;
                     success = false;
                   })
          | _ -> ());
          Lwt.return
            ( tc,
              Provider.make_tool_result ~tool_call_id:tc.id
                ~name:tc.function_name ~content:"[skipped: interrupted by user]",
              None )
        end
        else
          let before_active_workspace_files =
            capture_active_workspace_file_state agent
          in
          let is_tool_search = tc.function_name = "tool_search" in
          let* result_msg =
            if is_tool_search then Lwt.return (resolve_tool_search agent tc)
            else
              let* result =
                match agent.tool_registry with
                | None -> Lwt.return "Error: no tool registry available"
                | Some registry -> (
                    match Tool_registry.find registry tc.function_name with
                    | None ->
                        Lwt.return
                          (Printf.sprintf "Error: unknown tool '%s'"
                             tc.function_name)
                    | Some tool ->
                        Lwt.catch
                          (fun () ->
                            let args =
                              try Yojson.Safe.from_string tc.arguments
                              with _ ->
                                Logs.warn (fun m ->
                                    m
                                      "Tool call '%s': failed to parse \
                                       arguments as JSON (raw: %s)"
                                      tc.function_name tc.arguments);
                                `Assoc []
                            in
                            match Tool.validate_required_params tool args with
                            | Error msg -> Lwt.return msg
                            | Ok () ->
                                let context =
                                  {
                                    Tool.session_key;
                                    send_progress = None;
                                    interrupt_check;
                                    inject_system_messages =
                                      Some
                                        (fun msgs ->
                                          let msgs =
                                            Skill_dedup.dedup_skill_injections
                                              ~history:agent.history msgs
                                          in
                                          List.iter
                                            (fun content ->
                                              agent.history <-
                                                Provider.make_message
                                                  ~role:"system" ~content
                                                :: agent.history)
                                            msgs);
                                    effective_cwd = agent.effective_cwd;
                                    request_cwd_change =
                                      Some
                                        (fun new_cwd wipe ->
                                          agent.effective_cwd <- Some new_cwd;
                                          if wipe then
                                            pending_history_wipe := true);
                                  }
                                in
                                tool.invoke ~context args)
                          (fun exn ->
                            Lwt.return
                              ("Error invoking tool: " ^ Printexc.to_string exn))
                    )
              in
              let* result_for_history =
                Tool_postprocess.process_tool_result ~config:agent.config ~db
                  ~session_key ~tool_name:tc.function_name
                  ~history:agent.history ~raw_result:result
              in
              Lwt.return
                (Provider.make_tool_result ~tool_call_id:tc.id
                   ~name:tc.function_name ~content:result_for_history)
          in
          let result = result_msg.Provider.content in
          let success = not (String.starts_with ~prefix:"Error:" result) in
          (match (db, audit_enabled, session_key) with
          | Some db, true, Some sk ->
              Audit.log ~db
                (ToolResult
                   { session_key = sk; tool_name = tc.function_name; success })
          | _ -> ());
          let truncated =
            let limit = if success then 200 else 1000 in
            if String.length result > limit then
              String.sub result 0 limit ^ "..."
            else result
          in
          if success then
            Logs.info (fun m ->
                m "%sTool result: %s -> %s" sk_tag tc.function_name truncated)
          else
            Logs.warn (fun m ->
                m "%sTool error: %s -> %s" sk_tag tc.function_name truncated);
          let refresh =
            observe_workspace_refresh agent tc result
              ~before_active_workspace_files
          in
          if Option.is_some refresh.message then
            agent.observed_active_workspace_files <- refresh.after_state;
          Lwt.return (tc, result_msg, refresh.message))
      calls
  in
  (* Results already reflect execution order; append deterministically. *)
  List.iter
    (fun ((_tc : Provider.tool_call), tool_msg, refresh_msg) ->
      append_tool_history agent tool_msg refresh_msg)
    results;
  if !pending_history_wipe then perform_cwd_history_wipe agent;
  List.iter
    (fun ((tc : Provider.tool_call), _, _) ->
      let doc_events = observe_project_docs agent tc in
      List.iter (fun msg -> agent.history <- msg :: agent.history) doc_events)
    results;
  let* () =
    match on_tool_round_complete with
    | Some cb ->
        let pairs =
          List.map
            (fun (tc, (rm : Provider.message), _) -> (tc, rm.content))
            results
        in
        cb pairs
    | None -> Lwt.return_unit
  in
  Lwt.return_unit

let inject_search_context agent ~db ~user_message =
  let open Lwt.Syntax in
  if agent.config.memory.search_enabled then
    Lwt.catch
      (fun () ->
        (* FTS keyword search *)
        let keyword_results =
          Memory.search ~db ~query:user_message ~limit:5 ()
        in
        let keyword_strings =
          List.map
            (fun (m : Provider.message) ->
              if String.length m.content > 300 then
                String.sub m.content 0 300 ^ "..."
              else m.content)
            keyword_results
        in
        (* Vector search (if embedding provider is configured) *)
        let* vector_strings =
          if
            agent.config.memory.embedding_provider <> None
            || agent.config.memory.embedding_model <> None
          then
            Lwt.catch
              (fun () ->
                let* query_emb =
                  Vector.fetch_embedding ~config:agent.config ~text:user_message
                in
                let results =
                  Vector.search ~db ~query_embedding:query_emb ~limit:5 ()
                in
                Lwt.return results)
              (fun _exn -> Lwt.return [])
          else Lwt.return []
        in
        (* Merge results *)
        let merged =
          if vector_strings = [] then keyword_strings
          else
            Vector.merge_results ~keyword_results:keyword_strings
              ~vector_results:vector_strings
              ~keyword_weight:agent.config.memory.keyword_weight
              ~vector_weight:agent.config.memory.vector_weight
        in
        let top = List.filteri (fun i _ -> i < 3) merged in
        (* Core memories: always include for awareness *)
        let core_items =
          let all = Memory.list_core ~db () in
          List.filteri (fun i _ -> i < 10) all
        in
        let core_strings =
          List.map
            (fun (key, content, category) ->
              Printf.sprintf "[core:%s/%s] %s" category key content)
            core_items
        in
        match top @ core_strings with
        | [] -> Lwt.return_unit
        | parts ->
            let context_msg =
              Provider.make_message ~role:"system"
                ~content:
                  ("Relevant context from memory:\n" ^ String.concat "\n" parts)
            in
            agent.history <- context_msg :: agent.history;
            Lwt.return_unit)
      (fun _ -> Lwt.return_unit)
  else Lwt.return_unit

let filter_content_parts_for_model config content_parts =
  let model_id = config.Runtime_config.agent_defaults.primary_model in
  match Models_catalog.find_by_id model_id with
  | Some model_info when not model_info.Models_catalog.supports_vision ->
      let filtered =
        List.filter
          (function
            | Provider.Image_base64 _ -> false | Provider.Text _ -> true)
          content_parts
      in
      if List.length filtered < List.length content_parts then
        Logs.info (fun m ->
            m
              "Model %s does not support vision; filtering out %d image(s) \
               from message"
              model_id
              (List.length content_parts - List.length filtered));
      filtered
  | _ -> content_parts

let prepare_turn_history agent ~user_message ?(content_parts = [])
    ?(workspace_refresh_checked = false) ?db () =
  let open Lwt.Syntax in
  let* () =
    if workspace_refresh_checked then Lwt.return_unit
    else begin
      let _ = note_external_workspace_refresh_if_needed agent in
      let _ = refresh_project_docs_if_changed agent in
      Lwt.return_unit
    end
  in
  let* () =
    match db with
    | Some db -> inject_search_context agent ~db ~user_message
    | None -> Lwt.return_unit
  in
  let filtered_content_parts =
    filter_content_parts_for_model agent.config content_parts
  in
  let user_msg =
    match filtered_content_parts with
    | [] -> Provider.make_message ~role:"user" ~content:user_message
    | parts ->
        Provider.make_message_with_parts ~role:"user" ~content:user_message
          ~content_parts:(Provider.Text user_message :: parts)
  in
  agent.history <- user_msg :: agent.history;
  let* compacted = compact_history_if_needed agent ?db () in
  trim_history agent;
  Lwt.return compacted

let turn agent ~user_message ?db ?session_key ?interrupt_check ?inject_messages
    ?on_tool_round_complete ?runtime_context ?(history_prepared = false)
    ?on_history_update ?on_stuck () =
  let is_restart_interrupt = function
    | Some reason when reason = restart_interrupt_token -> true
    | _ -> false
  in
  let is_queued_message_interrupt = function
    | Some reason when reason = queued_message_interrupt_token -> true
    | _ -> false
  in
  let open Lwt.Syntax in
  let* _compaction_info =
    if history_prepared then Lwt.return_none
    else prepare_turn_history agent ~user_message ?db ()
  in
  let audit_enabled = agent.config.security.audit_enabled in
  let max_iters = agent.config.agent_defaults.max_tool_iterations in
  let tools = tools_json agent in
  (* Quota state: fetch from cache once per turn (non-blocking). *)
  let quota_states_opt =
    let qs = Provider_quota.get_all_cached () in
    if qs = [] then None else Some qs
  in
  (* Inject a quota notice into system prompt context when provider is >= 70%
     used.  runtime_context is shadowed here so the notice is visible to the
     LLM without altering the caller-provided context string. *)
  let runtime_context =
    match quota_states_opt with
    | None -> runtime_context
    | Some qs -> (
        let pn, _, _ =
          Provider.select_provider ~config:agent.config
            ?quota_states:quota_states_opt ()
        in
        let notice =
          match List.assoc_opt pn qs with
          | Some pq -> Provider_quota.quota_notice pq
          | None -> None
        in
        match notice with
        | None -> runtime_context
        | Some n ->
            Some
              (match runtime_context with
              | None -> n
              | Some rc -> rc ^ "\n\n" ^ n))
  in
  let resilient_complete config messages tools =
    let res = config.Runtime_config.resilience in
    let open Lwt.Syntax in
    let primary () =
      Provider.complete ~config ~messages ?tools ?session_key
        ?quota_states:quota_states_opt ()
    in
    let with_optional_fallback () =
      match res.fallback_provider with
      | Some fb_name ->
          let primary_name, _, _ =
            Provider.select_provider ~config ?quota_states:quota_states_opt ()
          in
          let fallback_name, _, _ =
            Provider.select_provider ~config ~preferred_provider:fb_name ()
          in
          if fallback_name = primary_name then primary ()
          else
            Resilience.with_fallback ~primary ~fallback:(fun () ->
                Provider.complete ~config ~messages ?tools ?session_key
                  ~preferred_provider:fb_name ())
      | None -> primary ()
    in
    let* timed =
      Resilience.with_timeout_retry ~timeout_s:res.timeout_s
        ~max_retries:res.max_retries ~base_delay_s:res.base_delay_s
        with_optional_fallback
    in
    match timed with Ok v -> Lwt.return v | Error e -> Lwt.fail_with e
  in
  let track_cost ~current_request_history_len response =
    let usage, model =
      match response with
      | Provider.Text { usage; model; _ } -> (usage, model)
      | Provider.ToolCalls { usage; model; _ } -> (usage, model)
    in
    match (usage, session_key) with
    | Some (pt, ct, api_cached), Some sid -> (
        if api_cached > pt && pt > 0 then
          Logs.warn (fun m ->
              m
                "[cache] provider reported cached=%d > prompt_tokens=%d; \
                 provider is not normalizing usage. Cache %% will be wrong."
                api_cached pt);
        if pt > 0 then
          Logs.info (fun m ->
              m "[cache] cached=%d/%d (%.0f%%) prompt tokens" api_cached pt
                (100.0 *. float_of_int api_cached /. float_of_int pt));
        Cost_tracker.record_turn ~model ~prompt_tokens:pt ~completion_tokens:ct
          ~session_id:sid;
        Model_preferences.increment_usage model |> ignore;
        match db with
        | Some db ->
            let pname, _, _ =
              Provider.select_provider ~config:agent.config ()
            in
            let prev = Request_stats.get_prev_totals ~db ~session_key:sid in
            let added =
              match prev with
              | Some (prev_pt, prev_ct, _) ->
                  let history_delta_tokens =
                    estimate_history_delta_tokens ~history:agent.history
                      ~previous_request_history_len:
                        agent.last_request_history_len
                      ~current_request_history_len
                    |> Option.value ~default:prev_ct
                  in
                  max 0 (pt - (prev_pt + history_delta_tokens))
              | None -> pt
            in
            let cache_hit =
              if api_cached > 0 then true
              else
                match prev with
                | Some (_, _, ts) when ts <> "" -> (
                    try
                      let stmt =
                        Sqlite3.prepare db
                          "SELECT (strftime('%s', 'now') - strftime('%s', ?1)) \
                           < 300"
                      in
                      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT ts));
                      Fun.protect
                        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
                        (fun () ->
                          match Sqlite3.step stmt with
                          | Sqlite3.Rc.ROW -> (
                              match Sqlite3.column stmt 0 with
                              | Sqlite3.Data.INT 1L -> true
                              | _ -> false)
                          | _ -> false)
                    with _ -> false)
                | _ -> false
            in
            let cost_usd_opt =
              match Cost_tracker.lookup_pricing model with
              | None -> None
              | Some _ ->
                  Some
                    (Cost_tracker.calculate_cost_with_cache ~model
                       ~prompt_tokens:pt ~completion_tokens:ct
                       ~added_prompt_tokens:added ~cache_hit
                       ~api_cached_tokens:api_cached ())
            in
            let cached_tokens =
              if api_cached > 0 then Some api_cached else None
            in
            Request_stats.record ~db ~session_key:sid ~provider:pname ~model
              ~prompt_tokens:pt ~completion_tokens:ct ?cost_usd:cost_usd_opt
              ~added_prompt_tokens:added ?cached_tokens ()
        | None -> ())
    | _ -> ()
  in
  let fire_history_update len_before =
    match on_history_update with
    | Some cb ->
        let len_after = List.length agent.history in
        if len_after > len_before then begin
          let reversed = List.rev agent.history in
          let new_msgs = List.filteri (fun i _ -> i >= len_before) reversed in
          cb new_msgs
        end
        else Lwt.return_unit
    | None -> Lwt.return_unit
  in
  let rec loop iteration =
    let runtime_context =
      match agent.effective_cwd with
      | Some cwd ->
          Prompt_builder.build_runtime_context ~config:agent.config
            ~effective_cwd:cwd ()
      | None -> runtime_context
    in
    let current_request_history_len = List.length agent.history in
    let* response =
      Lwt.catch
        (fun () ->
          let messages = build_messages ?runtime_context agent in
          resilient_complete agent.config messages tools)
        (fun exn ->
          if
            is_context_exhaustion_error (Printexc.to_string exn)
            && force_compress_history agent
          then begin
            Logs.warn (fun m ->
                m
                  "Context exhaustion detected; force-compressed history, \
                   retrying turn");
            let messages = build_messages ?runtime_context agent in
            resilient_complete agent.config messages tools
          end
          else Lwt.fail exn)
    in
    track_cost ~current_request_history_len response;
    agent.last_request_history_len <- Some current_request_history_len;
    match response with
    | Provider.Text { content; provider_response_items_json; thinking; _ } ->
        let thinking =
          if agent.config.agent_defaults.drop_thinking then None else thinking
        in
        agent.history <-
          Provider.make_message_full ~provider_response_items_json
            ~role:"assistant" ~content ~thinking ()
          :: agent.history;
        trim_history agent;
        Lwt.return content
    | Provider.ToolCalls { calls; _ } when tools = None ->
        let content =
          "I attempted to use tools ("
          ^ String.concat ", "
              (List.map
                 (fun (tc : Provider.tool_call) -> tc.function_name)
                 calls)
          ^ ") but tools are disabled. Set security.tools_enabled to true in "
          ^ Dot_dir.config_path () ^ " to enable them."
        in
        agent.history <-
          Provider.make_message ~role:"assistant" ~content :: agent.history;
        trim_history agent;
        Lwt.return content
    | Provider.ToolCalls { calls; _ } when iteration >= max_iters ->
        let content =
          "I've reached the maximum number of tool iterations. Here's what I \
           was trying to do: "
          ^ String.concat ", "
              (List.map
                 (fun (tc : Provider.tool_call) -> tc.function_name)
                 calls)
        in
        agent.history <-
          Provider.make_message ~role:"assistant" ~content :: agent.history;
        trim_history agent;
        Lwt.return content
    | Provider.ToolCalls { calls; provider_response_items_json; thinking; _ }
      -> (
        let len_before_tool_loop = List.length agent.history in
        let thinking =
          if agent.config.agent_defaults.drop_thinking then None else thinking
        in
        let assistant_msg =
          {
            Provider.role = "assistant";
            content = "";
            content_parts = [];
            tool_calls = calls;
            tool_call_id = None;
            name = None;
            provider_response_items_json;
            thinking;
          }
        in
        agent.history <- assistant_msg :: agent.history;
        let* () =
          execute_tool_calls agent ~db ~audit_enabled ~session_key
            ?interrupt_check ?on_tool_round_complete calls
        in
        (match inject_messages with
        | Some get_msgs ->
            let msgs = get_msgs () in
            List.iter
              (fun msg ->
                agent.history <-
                  Provider.make_message ~role:"user" ~content:msg
                  :: agent.history)
              msgs
        | None -> ());
        let* () = fire_history_update len_before_tool_loop in
        (* Check for stuck patterns after each tool call batch *)
        let stuck_signals =
          let result =
            Stuck_detector.check ~history:agent.history ~iteration ~max_iters
          in
          match result with
          | Stuck_detector.Definite signals -> Some signals
          | _ -> None
        in
        let* () =
          match (stuck_signals, on_stuck) with
          | Some signals, Some cb -> cb signals
          | _ -> Lwt.return_unit
        in
        match interrupt_check with
        | Some check -> (
            match check () with
            | interrupt when is_restart_interrupt interrupt ->
                Lwt.fail Restart_requested
            | interrupt when is_queued_message_interrupt interrupt ->
                (* Not a real interrupt: continue looping.  Queued messages
                   are picked up via inject_messages between tool batches.
                   Restart-resume turns remap this token to a real stop
                   signal in daemon_util.ml. *)
                loop (iteration + 1)
            | Some _ ->
                let partial =
                  "[Agent was interrupted mid-task] --- [NOTE: interrupted by \
                   user]"
                in
                agent.history <-
                  Provider.make_message ~role:"assistant" ~content:partial
                  :: agent.history;
                trim_history agent;
                Lwt.return partial
            | None -> loop (iteration + 1))
        | None -> loop (iteration + 1))
  in
  loop 0

let turn_stream agent ~user_message ?db ?session_key ?interrupt_check
    ?inject_messages ?on_tool_round_complete ?runtime_context
    ?(history_prepared = false) ?on_history_update ?on_stuck ~on_chunk () =
  let is_restart_interrupt = function
    | Some reason when reason = restart_interrupt_token -> true
    | _ -> false
  in
  let open Lwt.Syntax in
  let* _compaction_info =
    if history_prepared then Lwt.return_none
    else prepare_turn_history agent ~user_message ?db ()
  in
  let audit_enabled = agent.config.security.audit_enabled in
  let max_iters = agent.config.agent_defaults.max_tool_iterations in
  let tools = tools_json agent in
  (* Quota state: fetch from cache once per turn (non-blocking). *)
  let quota_states_opt =
    let qs = Provider_quota.get_all_cached () in
    if qs = [] then None else Some qs
  in
  (* Inject quota notice into system prompt context when provider >= 70% used. *)
  let runtime_context =
    match quota_states_opt with
    | None -> runtime_context
    | Some qs -> (
        let pn, _, _ =
          Provider.select_provider ~config:agent.config
            ?quota_states:quota_states_opt ()
        in
        let notice =
          match List.assoc_opt pn qs with
          | Some pq -> Provider_quota.quota_notice pq
          | None -> None
        in
        match notice with
        | None -> runtime_context
        | Some n ->
            Some
              (match runtime_context with
              | None -> n
              | Some rc -> rc ^ "\n\n" ^ n))
  in
  (* Buffer to accumulate streamed content for interrupt annotation. *)
  let partial_buf = Buffer.create 256 in
  let wrapped_on_chunk chunk =
    let open Lwt.Syntax in
    match chunk with
    | Provider.Delta text -> (
        Buffer.add_string partial_buf text;
        let* () = on_chunk chunk in
        match interrupt_check with
        | Some check -> (
            match check () with
            | interrupt when is_restart_interrupt interrupt ->
                Lwt.fail Restart_requested
            | interrupt when is_queued_message_interrupt interrupt ->
                Lwt.return_unit
            | Some _ ->
                let note = " --- [NOTE: interrupted by user]" in
                let* () = on_chunk (Provider.Delta note) in
                Lwt.fail (Interrupted (Buffer.contents partial_buf))
            | None -> Lwt.return_unit)
        | None -> Lwt.return_unit)
    | Provider.Done | Provider.ThinkingDelta _ | Provider.ToolCallDelta _
    | Provider.ToolStart _ | Provider.ToolOutputDelta _ | Provider.ToolResult _
      ->
        on_chunk chunk
  in
  let resilient_stream config messages tools on_chunk =
    let res = config.Runtime_config.resilience in
    let open Lwt.Syntax in
    Buffer.clear partial_buf;
    let primary () =
      Provider.complete_stream ~config ~messages ?tools ?session_key
        ?quota_states:quota_states_opt ~on_chunk ()
    in
    let with_optional_fallback () =
      match res.fallback_provider with
      | Some fb_name ->
          let primary_name, _, _ =
            Provider.select_provider ~config ?quota_states:quota_states_opt ()
          in
          let fallback_name, _, _ =
            Provider.select_provider ~config ~preferred_provider:fb_name ()
          in
          if fallback_name = primary_name then primary ()
          else
            Resilience.with_fallback ~primary ~fallback:(fun () ->
                Provider.complete_stream ~config ~messages ?tools ?session_key
                  ~preferred_provider:fb_name ~on_chunk ())
      | None -> primary ()
    in
    let* timed =
      Resilience.with_timeout_retry ~timeout_s:res.timeout_s
        ~max_retries:res.max_retries ~base_delay_s:res.base_delay_s
        with_optional_fallback
    in
    match timed with Ok v -> Lwt.return v | Error e -> Lwt.fail_with e
  in
  let track_cost ~current_request_history_len response =
    let usage, model =
      match response with
      | Provider.Text { usage; model; _ } -> (usage, model)
      | Provider.ToolCalls { usage; model; _ } -> (usage, model)
    in
    match (usage, session_key) with
    | Some (pt, ct, api_cached), Some sid -> (
        if api_cached > pt && pt > 0 then
          Logs.warn (fun m ->
              m
                "[cache] provider reported cached=%d > prompt_tokens=%d; \
                 provider is not normalizing usage. Cache %% will be wrong."
                api_cached pt);
        if pt > 0 then
          Logs.info (fun m ->
              m "[cache] cached=%d/%d (%.0f%%) prompt tokens" api_cached pt
                (100.0 *. float_of_int api_cached /. float_of_int pt));
        Cost_tracker.record_turn ~model ~prompt_tokens:pt ~completion_tokens:ct
          ~session_id:sid;
        Model_preferences.increment_usage model |> ignore;
        match db with
        | Some db ->
            let pname, _, _ =
              Provider.select_provider ~config:agent.config ()
            in
            let prev = Request_stats.get_prev_totals ~db ~session_key:sid in
            let added =
              match prev with
              | Some (prev_pt, prev_ct, _) ->
                  let history_delta_tokens =
                    estimate_history_delta_tokens ~history:agent.history
                      ~previous_request_history_len:
                        agent.last_request_history_len
                      ~current_request_history_len
                    |> Option.value ~default:prev_ct
                  in
                  max 0 (pt - (prev_pt + history_delta_tokens))
              | None -> pt
            in
            let cache_hit =
              if api_cached > 0 then true
              else
                match prev with
                | Some (_, _, ts) when ts <> "" -> (
                    try
                      let stmt =
                        Sqlite3.prepare db
                          "SELECT (strftime('%s', 'now') - strftime('%s', ?1)) \
                           < 300"
                      in
                      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT ts));
                      Fun.protect
                        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
                        (fun () ->
                          match Sqlite3.step stmt with
                          | Sqlite3.Rc.ROW -> (
                              match Sqlite3.column stmt 0 with
                              | Sqlite3.Data.INT 1L -> true
                              | _ -> false)
                          | _ -> false)
                    with _ -> false)
                | _ -> false
            in
            let cost_usd_opt =
              match Cost_tracker.lookup_pricing model with
              | None -> None
              | Some _ ->
                  Some
                    (Cost_tracker.calculate_cost_with_cache ~model
                       ~prompt_tokens:pt ~completion_tokens:ct
                       ~added_prompt_tokens:added ~cache_hit
                       ~api_cached_tokens:api_cached ())
            in
            let cached_tokens =
              if api_cached > 0 then Some api_cached else None
            in
            Request_stats.record ~db ~session_key:sid ~provider:pname ~model
              ~prompt_tokens:pt ~completion_tokens:ct ?cost_usd:cost_usd_opt
              ~added_prompt_tokens:added ?cached_tokens ()
        | None -> ())
    | _ -> ()
  in
  let fire_history_update len_before =
    match on_history_update with
    | Some cb ->
        let len_after = List.length agent.history in
        if len_after > len_before then begin
          let reversed = List.rev agent.history in
          let new_msgs = List.filteri (fun i _ -> i >= len_before) reversed in
          cb new_msgs
        end
        else Lwt.return_unit
    | None -> Lwt.return_unit
  in
  let rec loop iteration =
    let runtime_context =
      match agent.effective_cwd with
      | Some cwd ->
          Prompt_builder.build_runtime_context ~config:agent.config
            ~effective_cwd:cwd ()
      | None -> runtime_context
    in
    let current_request_history_len = List.length agent.history in
    Lwt.catch
      (fun () ->
        let* response =
          Lwt.catch
            (fun () ->
              let messages = build_messages ?runtime_context agent in
              resilient_stream agent.config messages tools wrapped_on_chunk)
            (fun exn ->
              if
                is_context_exhaustion_error (Printexc.to_string exn)
                && force_compress_history agent
              then begin
                Logs.warn (fun m ->
                    m
                      "Context exhaustion detected; force-compressed history, \
                       retrying turn");
                let messages = build_messages ?runtime_context agent in
                resilient_stream agent.config messages tools wrapped_on_chunk
              end
              else Lwt.fail exn)
        in
        track_cost ~current_request_history_len response;
        agent.last_request_history_len <- Some current_request_history_len;
        match response with
        | Provider.Text { content; provider_response_items_json; thinking; _ }
          ->
            let thinking =
              if agent.config.agent_defaults.drop_thinking then None
              else thinking
            in
            agent.history <-
              Provider.make_message_full ~provider_response_items_json
                ~role:"assistant" ~content ~thinking ()
              :: agent.history;
            trim_history agent;
            Lwt.return content
        | Provider.ToolCalls { calls; _ } when tools = None ->
            let content =
              "I attempted to use tools ("
              ^ String.concat ", "
                  (List.map
                     (fun (tc : Provider.tool_call) -> tc.function_name)
                     calls)
              ^ ") but tools are disabled."
            in
            agent.history <-
              Provider.make_message ~role:"assistant" ~content :: agent.history;
            trim_history agent;
            let* () = on_chunk (Provider.Delta content) in
            Lwt.return content
        | Provider.ToolCalls { calls; _ } when iteration >= max_iters ->
            let content =
              "I've reached the maximum number of tool iterations."
            in
            agent.history <-
              Provider.make_message ~role:"assistant" ~content :: agent.history;
            trim_history agent;
            let* () = on_chunk (Provider.Delta content) in
            Lwt.return content
        | Provider.ToolCalls
            { calls; provider_response_items_json; thinking; _ } -> (
            let len_before_tool_loop = List.length agent.history in
            let thinking =
              if agent.config.agent_defaults.drop_thinking then None
              else thinking
            in
            let assistant_msg =
              {
                Provider.role = "assistant";
                content = "";
                content_parts = [];
                tool_calls = calls;
                tool_call_id = None;
                name = None;
                provider_response_items_json;
                thinking;
              }
            in
            agent.history <- assistant_msg :: agent.history;
            let* () =
              execute_tool_calls_stream agent ~db ~audit_enabled ~session_key
                ?interrupt_check ?on_tool_round_complete ~on_chunk calls
            in
            (match inject_messages with
            | Some get_msgs ->
                let msgs = get_msgs () in
                List.iter
                  (fun msg ->
                    agent.history <-
                      Provider.make_message ~role:"user" ~content:msg
                      :: agent.history)
                  msgs
            | None -> ());
            let* () = fire_history_update len_before_tool_loop in
            (* Check for stuck patterns after each tool call batch *)
            let stuck_signals =
              let result =
                Stuck_detector.check ~history:agent.history ~iteration
                  ~max_iters
              in
              match result with
              | Stuck_detector.Definite signals -> Some signals
              | _ -> None
            in
            let* () =
              match (stuck_signals, on_stuck) with
              | Some signals, Some cb -> cb signals
              | _ -> Lwt.return_unit
            in
            match interrupt_check with
            | Some check -> (
                match check () with
                | interrupt when is_restart_interrupt interrupt ->
                    Lwt.fail Restart_requested
                | interrupt when is_queued_message_interrupt interrupt ->
                    loop (iteration + 1)
                | Some _ ->
                    let partial = " --- [NOTE: interrupted by user]" in
                    agent.history <-
                      Provider.make_message ~role:"assistant" ~content:partial
                      :: agent.history;
                    trim_history agent;
                    let* () = on_chunk (Provider.Delta partial) in
                    Lwt.return partial
                | None -> loop (iteration + 1))
            | None -> loop (iteration + 1)))
      (fun exn ->
        match exn with
        | Restart_requested -> Lwt.fail Restart_requested
        | Interrupted partial ->
            let annotated = partial ^ " --- [NOTE: interrupted by user]" in
            agent.history <-
              Provider.make_message ~role:"assistant" ~content:annotated
              :: agent.history;
            trim_history agent;
            Lwt.return annotated
        | exn -> Lwt.fail exn)
  in
  loop 0
