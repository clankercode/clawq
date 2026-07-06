include Agent_room_budget

let create ~config ?tool_registry ?agent_template ?cwd
    ?(instruction_items : Runtime_config.effective_instruction_item list = [])
    ?access_snapshot_id ?access_snapshot () =
  let config = apply_subagent_default_model ~config ~agent_template in
  let system_prompt =
    Prompt_builder.build ~config ~tool_registry ?agent_template
      ~instruction_items ()
  in
  let ws_doc_digests =
    match agent_template with
    | None -> Prompt_builder.workspace_doc_content_digests ~config ()
    | Some _ -> []
  in
  let instruction_texts =
    List.map
      (fun (item : Runtime_config.effective_instruction_item) ->
        item.instruction.text)
      instruction_items
  in
  let pd =
    Prompt_builder.build_project_docs_message ~config ?effective_cwd:cwd
      ~ws_doc_digests ~instruction_texts ()
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
    last_missing_required_key = None;
    last_missing_required_count = 0;
    hard_abort_reason = None;
    room_profile_system_prompt = None;
    profiled_room = false;
    instruction_items;
    access_snapshot_id;
    access_snapshot;
  }

let prepare_turn_history agent ~user_message ?(content_parts = [])
    ?(workspace_refresh_checked = false) ?db ?session_key ?room_id
    ?on_llm_call_debug:_on_llm_call_debug () =
  let open Lwt.Syntax in
  let* () =
    if workspace_refresh_checked then Lwt.return_unit
    else begin
      let _ = note_external_workspace_refresh_if_needed agent in
      let _ = refresh_project_docs_if_changed agent in
      Lwt.return_unit
    end
  in
  refresh_profiled_room_flag agent ?db ?session_key ?room_id ();
  let* () =
    match db with
    | Some db when agent.profiled_room -> (
        match scoped_memory_room_for_turn ~db ?session_key ?room_id () with
        | Some (scope_key, Some profile_id) ->
            Agent_2_tools.inject_search_context agent ~db ~user_message
              ~scope_kind:"room" ~scope_key ~principal_kind:"profile"
              ~principal_id:(string_of_int profile_id)
        | Some (scope_key, None) ->
            Agent_2_tools.inject_search_context agent ~db ~user_message
              ~scope_kind:"room" ~scope_key
        | None -> Lwt.return_unit)
    | Some db when unscoped_memory_context_allowed agent ->
        Agent_2_tools.inject_search_context agent ~db ~user_message
    | _ -> Lwt.return_unit
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
