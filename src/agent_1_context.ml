include Agent_0_compact

let restart_interrupt_token = "__clawq_restart__"
let queued_message_interrupt_token = "[queued inbound message]"
let stop_interrupt_token = "__clawq_stop__"
let stopped_by_admin_message = "Stopped by admin."

let is_stop_interrupt = function
  | Some reason when reason = stop_interrupt_token -> true
  | _ -> false

let is_queued_message_interrupt = function
  | Some reason when reason = queued_message_interrupt_token -> true
  | _ -> false

let record_stopped_by_admin agent =
  agent.history <-
    Provider.make_message ~role:"assistant" ~content:stopped_by_admin_message
    :: agent.history;
  trim_history agent;
  stopped_by_admin_message

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

(* When an agent_template is supplied but its own model field is None, allow
   agent_defaults.subagent_default_model to take over. The template-level
   model still wins when it is set, preserving the strongest preference. *)
let apply_subagent_default_model ~(config : Runtime_config.t)
    ~(agent_template : Agent_template.t option) =
  match agent_template with
  | None -> config
  | Some tmpl -> (
      match tmpl.Agent_template.model with
      | Some _ -> config
      | None -> (
          match config.agent_defaults.subagent_default_model with
          | None -> config
          | Some m when String.trim m = "" -> config
          | Some m ->
              {
                config with
                agent_defaults =
                  { config.agent_defaults with primary_model = m };
              }))

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
      ?agent_template:agent.agent_template
      ?room_profile_system_prompt:agent.room_profile_system_prompt
      ~instruction_items:agent.instruction_items ();
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

(* B622: escalation-aware required-param validation. Track consecutive
   (tool_name, missing-params) repeats on the agent and return progressively
   stronger error messages so the model breaks out of the same-call loop.
   B677: when the same (tool, missing-params) pair recurs CLAWQ_MAX_IDENTICAL_
   PARAM_ERRORS times (default 3), set `agent.hard_abort_reason` so the turn
   loop terminates instead of letting the model burn another iteration. *)
let max_identical_param_errors () =
  match Sys.getenv_opt "CLAWQ_MAX_IDENTICAL_PARAM_ERRORS" with
  | Some v -> ( try max 2 (int_of_string v) with _ -> 3)
  | None -> 3

let validate_required_with_escalation agent (tool : Tool.t)
    (args : Yojson.Safe.t) : (unit, string) result =
  match Tool.find_missing_required_params tool args with
  | [] ->
      agent.last_missing_required_key <- None;
      agent.last_missing_required_count <- 0;
      Ok ()
  | missing ->
      let sorted = List.sort compare missing in
      let key = tool.name ^ "|" ^ String.concat "," sorted in
      let level =
        match agent.last_missing_required_key with
        | Some prev when prev = key ->
            agent.last_missing_required_count <-
              agent.last_missing_required_count + 1;
            agent.last_missing_required_count
        | _ ->
            agent.last_missing_required_key <- Some key;
            agent.last_missing_required_count <- 1;
            1
      in
      let threshold = max_identical_param_errors () in
      if level >= threshold && agent.hard_abort_reason = None then
        agent.hard_abort_reason <-
          Some
            (Printf.sprintf
               "Aborted turn after %d consecutive identical \
                parameter-validation failures on tool '%s' (missing: %s). The \
                model was looping on the same invalid call shape. Override \
                with CLAWQ_MAX_IDENTICAL_PARAM_ERRORS."
               level tool.name (String.concat "," sorted));
      Error
        (Tool.format_missing_required_error tool ~missing
           ~escalation_level:(level - 1) ())

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
    let instruction_texts =
      List.map
        (fun (item : Runtime_config.effective_instruction_item) ->
          item.instruction.text)
        agent.instruction_items
    in
    let pd =
      Prompt_builder.build_project_docs_message ~config:agent.config
        ?effective_cwd:agent.effective_cwd ~ws_doc_digests ~instruction_texts ()
    in
    let new_digests = pd.digests in
    (* Reload state when the docs change OR the resolved root moves (e.g. a
       room/thread session was bound to a new workspace subfolder via
       effective_cwd). The root may move without any docs being present, so the
       state update and the user-facing event are decoupled below. *)
    let docs_changed = new_digests <> agent.project_docs_digests in
    let root_changed = pd.git_root <> agent.project_docs_git_root in
    if (not docs_changed) && not root_changed then None
    else begin
      agent.project_docs_content <- pd.content;
      agent.project_docs_digests <- pd.digests;
      if root_changed then begin
        agent.project_docs_git_root <- pd.git_root;
        agent.project_docs_subdir_digests <- [];
        Hashtbl.reset agent.project_doc_dirs_seen;
        match pd.git_root with
        | Some root -> Hashtbl.replace agent.project_doc_dirs_seen root true
        | None -> ()
      end;
      (* Only surface an event/notification when project docs are actually
         present or their content changed — not for a doc-less root move (e.g.
         binding a room to a plain folder with no CLAUDE.md/AGENTS.md). *)
      if docs_changed || pd.content <> None then begin
        let event_msg =
          Provider.make_message ~role:"event"
            ~content:
              "[project instructions refreshed: root CLAUDE.md/AGENTS.md \
               changed since last turn]"
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
      else None
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
                  let instruction_digests =
                    List.map
                      (fun (item : Runtime_config.effective_instruction_item) ->
                        Digest.to_hex (Digest.string item.instruction.text))
                      agent.instruction_items
                  in
                  if
                    List.mem digest agent.project_docs_digests
                    || List.mem digest agent.project_docs_subdir_digests
                    || List.mem digest instruction_digests
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
