include Agent_1_context

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

let room_profile_tool_denial agent ~session_key ~tool_name =
  (* P14.M2.E3.T001: when an access snapshot is set on the agent, use its
     resolved allowed/denied tools instead of re-resolving from the live
     config. This ensures config changes during execution don't alter
     in-flight access. *)
  match agent.access_snapshot with
  | Some snap -> Access_snapshot.tool_denial snap ~tool_name
  | None -> (
      match session_key with
      | None -> None
      | Some session_key ->
          Runtime_config.room_profile_tool_denial_for_session agent.config
            ~session_key ~tool_name)

let normalized_tool_call_json (tc : Provider.tool_call) =
  `Assoc
    [
      ("id", `String tc.id);
      ("name", `String tc.function_name);
      ("arguments", `String tc.arguments);
    ]
  |> Yojson.Safe.to_string

let raw_tool_call_data_for_log ?raw_tool_calls_json tc =
  match raw_tool_calls_json with
  | Some raw when String.trim raw <> "" -> raw
  | _ -> normalized_tool_call_json tc

let log_raw_tool_call_failure sk_tag ?raw_tool_calls_json
    (tc : Provider.tool_call) ~reason =
  Logs.warn (fun m ->
      m "%sRaw model tool-call data for failed %s (id=%s, reason=%s): %s" sk_tag
        tc.function_name tc.id reason
        (raw_tool_call_data_for_log ?raw_tool_calls_json tc))

let parse_tool_arguments (tc : Provider.tool_call) =
  try Ok (Yojson.Safe.from_string tc.arguments)
  with _ ->
    Error
      (Printf.sprintf
         "Error: Tool call '%s' failed to parse arguments as JSON (raw: %s). \
          Re-emit this tool call with a valid JSON object matching the tool \
          schema."
         tc.function_name tc.arguments)

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

(* B607: Execute tool calls in PARALLEL when the model emits more than one in
   a single response. Lwt_list.map_p preserves input order in its result list
   so history append remains deterministic. Per-tool ToolStart events fire
   up-front (before any tool completes) so consumers (status messages, UIs)
   show 'running N tools' immediately rather than revealing each as it
   finishes. Workspace-refresh attribution is still per-tool (each captures
   its own before-state) — slightly duplicate FS reads for batches that
   modify the same file, accepted in exchange for keeping attribution
   correct. *)
let execute_tool_calls_stream agent ~db ~audit_enabled ~session_key
    ?raw_tool_calls_json ?interrupt_check ?on_tool_round_complete
    ?on_llm_call_debug ~on_chunk calls =
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
  let reserved_no_arg_skills = Hashtbl.create 8 in
  (* B607: emit ToolStart for every call up-front so the UI sees "running N"
     immediately. Audit log entries also fire up-front to match. *)
  List.iter
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
               { id = tc.id; name = tc.function_name; arguments = tc.arguments })))
    calls;
  (* F1 fix: validate all tool calls serially before parallel execution.
     validate_required_with_escalation mutates agent.last_missing_required_key,
     agent.last_missing_required_count, and agent.hard_abort_reason, which
     would race under Lwt_list.map_p. Pre-validate serially, then use cached
     results in the parallel block.
     Note: agent.history, agent.effective_cwd, pending_history_wipe, and
     agent.observed_active_workspace_files are also mutated inside the parallel
     block via inject_system_messages/request_cwd_change callbacks and
     post-tool workspace refresh. These are safe under OCaml 5.1 cooperative
     Lwt scheduling (no yield points in the mutation paths). If multi-domain
     parallelism is introduced, these would need synchronization. *)
  let pre_validations =
    List.map
      (fun (tc : Provider.tool_call) ->
        if check_interrupt () then (tc, Error "[skipped: interrupted by user]")
        else
          match
            room_profile_tool_denial agent ~session_key
              ~tool_name:tc.function_name
          with
          | Some msg -> (tc, Error msg)
          | None -> (
              match tc.function_name with
              | "tool_search" -> (tc, Ok None)
              | _ -> (
                  match agent.tool_registry with
                  | None -> (tc, Error "Error: no tool registry available")
                  | Some registry -> (
                      match Tool_registry.find registry tc.function_name with
                      | None ->
                          ( tc,
                            Error
                              (Printf.sprintf "Error: unknown tool '%s'"
                                 tc.function_name) )
                      | Some tool -> (
                          match parse_tool_arguments tc with
                          | Error msg -> (tc, Error msg)
                          | Ok args -> (
                              match
                                validate_required_with_escalation agent tool
                                  args
                              with
                              | Error msg -> (tc, Error msg)
                              | Ok () -> (
                                  match
                                    Skill_invocation_guard.use_skill_loaded_noop
                                      ~reserved_no_arg_skills
                                      ~history:agent.history tool args
                                  with
                                  | Some response -> (tc, Error response)
                                  | None -> (tc, Ok (Some (tool, args))))))))))
      calls
  in
  let* results =
    Lwt_list.map_p
      (fun ((tc : Provider.tool_call), pre_validation) ->
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
          let streamed_output = ref false in
          let t0 = Unix.gettimeofday () in
          let* result_msg, raw_result =
            match pre_validation with
            | Error err_msg ->
                Lwt.return
                  ( Provider.make_tool_result ~tool_call_id:tc.id
                      ~name:tc.function_name ~content:err_msg,
                    err_msg )
            | Ok None ->
                let msg = resolve_tool_search agent tc in
                Lwt.return (msg, msg.Provider.content)
            | Ok (Some ((tool : Tool.t), args)) ->
                let* result =
                  Lwt.catch
                    (fun () ->
                      let egress_rules =
                        match session_key with
                        | Some key ->
                            let access =
                              Runtime_config.resolve_effective_access
                                agent.config ~session_key:key ()
                            in
                            access.egress_rules
                        | None -> []
                      in
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
                                      Provider.make_message ~role:"system"
                                        ~content
                                      :: agent.history)
                                  msgs);
                          effective_cwd = agent.effective_cwd;
                          request_cwd_change =
                            Some
                              (fun new_cwd wipe ->
                                agent.effective_cwd <- Some new_cwd;
                                if wipe then pending_history_wipe := true);
                          egress_rules;
                          snapshot_id = agent.access_snapshot_id;
                          profile_id =
                            (match agent.access_snapshot with
                            | Some snap -> snap.Access_snapshot.profile_id
                            | None -> None);
                          egress_audit_db = db;
                        }
                      in
                      match tool.invoke_stream with
                      | Some invoke_stream ->
                          invoke_stream ~context
                            ~on_output_chunk:(fun chunk ->
                              streamed_output := true;
                              on_chunk
                                (Provider.ToolOutputDelta { id = tc.id; chunk }))
                            args
                      | None -> tool.invoke ~context args)
                    (fun exn ->
                      Lwt.return
                        ("Error invoking tool: " ^ Printexc.to_string exn))
                in
                let* result_for_history =
                  Tool_postprocess.process_tool_result ~config:agent.config ~db
                    ~session_key ~tool_name:tc.function_name
                    ~history:agent.history ?on_llm_call_debug ~raw_result:result
                    ()
                in
                Lwt.return
                  ( Provider.make_tool_result ~tool_call_id:tc.id
                      ~name:tc.function_name ~content:result_for_history,
                    result )
          in
          let invoke_duration = Unix.gettimeofday () -. t0 in
          Logs.info (fun m ->
              m "%sTool %s completed in %.3fs" sk_tag tc.function_name
                invoke_duration);
          let result = result_msg.Provider.content in
          let success = not (String.starts_with ~prefix:"Error:" raw_result) in
          (* B625: stamp the structured is_error flag now that we've
             classified the tool result. Downstream Anthropic-format
             converters use this directly instead of re-detecting via the
             content prefix. *)
          let result_msg =
            { result_msg with Provider.is_error = not success }
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
          else begin
            Logs.warn (fun m ->
                m "%sTool error: %s -> %s" sk_tag tc.function_name preview);
            log_raw_tool_call_failure sk_tag ?raw_tool_calls_json tc
              ~reason:preview
          end;
          (* B598: when the model opts in via "forward_to_user": true in the
             tool call args, emit the full result as a ToolOutputDelta first
             so the channel session displays it as its own message before
             the ToolResult summary card. *)
          let forward_to_user =
            try
              let args = Yojson.Safe.from_string tc.arguments in
              match args with
              | `Assoc fields -> (
                  match List.assoc_opt "forward_to_user" fields with
                  | Some (`Bool b) -> b
                  | _ -> false)
              | _ -> false
            with _ -> false
          in
          if forward_to_user && success && not !streamed_output then
            notify_async (fun () ->
                on_chunk
                  (Provider.ToolOutputDelta { id = tc.id; chunk = raw_result }));
          notify_async (fun () ->
              on_chunk
                (Provider.ToolResult
                   {
                     id = tc.id;
                     name = tc.function_name;
                     result;
                     is_error = not success;
                   }));
          let refresh =
            observe_workspace_refresh agent tc result
              ~before_active_workspace_files
          in
          if Option.is_some refresh.message then
            agent.observed_active_workspace_files <- refresh.after_state;
          Lwt.return (tc, result_msg, refresh.message))
      pre_validations
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

let execute_tool_calls agent ~db ~audit_enabled ~session_key
    ?raw_tool_calls_json ?interrupt_check ?on_tool_round_complete
    ?on_llm_call_debug calls =
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
  let reserved_no_arg_skills = Hashtbl.create 8 in
  (* B607: emit tool invocation audit entries up-front for the whole batch. *)
  List.iter
    (fun (tc : Provider.tool_call) ->
      Logs.info (fun m ->
          m "%sTool call: %s (id=%s) args=%s" sk_tag tc.function_name tc.id
            tc.arguments);
      match (db, audit_enabled, session_key) with
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
      | _ -> ())
    calls;
  (* F1 fix: pre-validate tool calls serially (see streaming path above). *)
  let pre_validations =
    List.map
      (fun (tc : Provider.tool_call) ->
        if check_interrupt () then (tc, Error "[skipped: interrupted by user]")
        else
          match
            room_profile_tool_denial agent ~session_key
              ~tool_name:tc.function_name
          with
          | Some msg -> (tc, Error msg)
          | None -> (
              match tc.function_name with
              | "tool_search" -> (tc, Ok None)
              | _ -> (
                  match agent.tool_registry with
                  | None -> (tc, Error "Error: no tool registry available")
                  | Some registry -> (
                      match Tool_registry.find registry tc.function_name with
                      | None ->
                          ( tc,
                            Error
                              (Printf.sprintf "Error: unknown tool '%s'"
                                 tc.function_name) )
                      | Some tool -> (
                          match parse_tool_arguments tc with
                          | Error msg -> (tc, Error msg)
                          | Ok args -> (
                              match
                                validate_required_with_escalation agent tool
                                  args
                              with
                              | Error msg -> (tc, Error msg)
                              | Ok () -> (
                                  match
                                    Skill_invocation_guard.use_skill_loaded_noop
                                      ~reserved_no_arg_skills
                                      ~history:agent.history tool args
                                  with
                                  | Some response -> (tc, Error response)
                                  | None -> (tc, Ok (Some (tool, args))))))))))
      calls
  in
  let* results =
    Lwt_list.map_p
      (fun ((tc : Provider.tool_call), pre_validation) ->
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
          let* result_msg, result_for_status =
            match pre_validation with
            | Error err_msg ->
                Lwt.return
                  ( Provider.make_tool_result ~tool_call_id:tc.id
                      ~name:tc.function_name ~content:err_msg,
                    err_msg )
            | Ok None ->
                let msg = resolve_tool_search agent tc in
                Lwt.return (msg, msg.Provider.content)
            | Ok (Some ((tool : Tool.t), args)) ->
                let* result =
                  Lwt.catch
                    (fun () ->
                      let egress_rules =
                        match session_key with
                        | Some key ->
                            let access =
                              Runtime_config.resolve_effective_access
                                agent.config ~session_key:key ()
                            in
                            access.egress_rules
                        | None -> []
                      in
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
                                      Provider.make_message ~role:"system"
                                        ~content
                                      :: agent.history)
                                  msgs);
                          effective_cwd = agent.effective_cwd;
                          request_cwd_change =
                            Some
                              (fun new_cwd wipe ->
                                agent.effective_cwd <- Some new_cwd;
                                if wipe then pending_history_wipe := true);
                          egress_rules;
                          snapshot_id = agent.access_snapshot_id;
                          profile_id =
                            (match agent.access_snapshot with
                            | Some snap -> snap.Access_snapshot.profile_id
                            | None -> None);
                          egress_audit_db = db;
                        }
                      in
                      tool.invoke ~context args)
                    (fun exn ->
                      Lwt.return
                        ("Error invoking tool: " ^ Printexc.to_string exn))
                in
                let* result_for_history =
                  Tool_postprocess.process_tool_result ~config:agent.config ~db
                    ~session_key ~tool_name:tc.function_name
                    ~history:agent.history ?on_llm_call_debug ~raw_result:result
                    ()
                in
                Lwt.return
                  ( Provider.make_tool_result ~tool_call_id:tc.id
                      ~name:tc.function_name ~content:result_for_history,
                    result )
          in
          let result = result_msg.Provider.content in
          let success =
            not (String.starts_with ~prefix:"Error:" result_for_status)
          in
          (* B625: structured is_error stamp; see execute_tool_calls_stream. *)
          let result_msg =
            { result_msg with Provider.is_error = not success }
          in
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
          else begin
            Logs.warn (fun m ->
                m "%sTool error: %s -> %s" sk_tag tc.function_name truncated);
            log_raw_tool_call_failure sk_tag ?raw_tool_calls_json tc
              ~reason:truncated
          end;
          let refresh =
            observe_workspace_refresh agent tc result
              ~before_active_workspace_files
          in
          if Option.is_some refresh.message then
            agent.observed_active_workspace_files <- refresh.after_state;
          Lwt.return (tc, result_msg, refresh.message))
      pre_validations
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

let clip_memory_content content =
  if String.length content > 300 then String.sub content 0 300 ^ "..."
  else content

let scoped_memory_granted ~db ~scope_kind ~scope_key ?principal_kind
    ?principal_id ~capability () =
  match (principal_kind, principal_id) with
  | None, _ | _, None -> true
  | Some principal_kind, Some principal_id -> (
      match
        Memory.get_scope_by_kind_key ~db ~kind:scope_kind ~key:scope_key
      with
      | None -> false
      | Some scope ->
          let owns_scope =
            match
              (principal_kind, int_of_string_opt principal_id, scope.profile_id)
            with
            | "profile", Some profile_id, Some owner_id -> profile_id = owner_id
            | _ -> false
          in
          owns_scope
          || List.mem capability
               (Memory.resolve_grants ~db ~scope_id:scope.id ~principal_kind
                  ~principal_id))

let scoped_memory_strings ~db ~scope_kind ~scope_key ?principal_kind
    ?principal_id ~query () =
  if
    not
      (scoped_memory_granted ~db ~scope_kind ~scope_key ?principal_kind
         ?principal_id ~capability:"read" ())
  then []
  else
    let content_matches =
      Memory.query_scoped_memories ~db ~scope_kind ~scope_key
        ~content_search:query ~limit:10 ()
    in
    let all_when_no_content_match =
      if content_matches = [] then
        Memory.query_scoped_memories ~db ~scope_kind ~scope_key ~limit:10 ()
      else []
    in
    let rows = content_matches @ all_when_no_content_match in
    List.map
      (fun (m : Memory_types.scoped_memory) ->
        let content =
          match m.content with
          | Some c -> " " ^ clip_memory_content c
          | None -> ""
        in
        Printf.sprintf "[scoped:%s/%s#%d ref=%s]%s" m.scope_kind m.scope_key
          m.id m.reference content)
      rows

let inject_search_context ?scope_kind ?scope_key ?principal_kind ?principal_id
    agent ~db ~user_message =
  let open Lwt.Syntax in
  if agent.config.memory.search_enabled then
    Lwt.catch
      (fun () ->
        match (scope_kind, scope_key) with
        | Some scope_kind, Some scope_key ->
            let has_read_grant =
              scoped_memory_granted ~db ~scope_kind ~scope_key ?principal_kind
                ?principal_id ~capability:"read" ()
            in
            let scoped_message_results =
              if not has_read_grant then []
              else
                Memory.search ~db ~query:user_message ~scope_kind ~scope_key
                  ~limit:5 ()
                |> List.map (fun (m : Provider.message) ->
                    "[scoped-message:" ^ scope_kind ^ "/" ^ scope_key ^ "] "
                    ^ clip_memory_content m.content)
            in
            (* Vector search for scoped embeddings (if embedding provider is
               configured and room budget is not exceeded) *)
            let* scoped_vector_results =
              if not has_read_grant then Lwt.return []
              else if
                agent.config.memory.embedding_provider <> None
                || agent.config.memory.embedding_model <> None
              then
                (* Check room budget before making the embedding API call *)
                let budget_ok =
                  match
                    Memory.get_room_profile_binding ~db ~room_id:scope_key
                  with
                  | Some binding -> (
                      match
                        Room_budget.get_profile_budget ~db
                          ~profile_id:binding.profile_id
                      with
                      | Some state -> not state.Room_budget.limit_exceeded
                      | None -> true)
                  | None -> true
                in
                if not budget_ok then Lwt.return []
                else
                  Lwt.catch
                    (fun () ->
                      let* query_emb =
                        Vector.fetch_embedding ~config:agent.config
                          ~text:user_message
                      in
                      let results =
                        Vector.search ~db ~query_embedding:query_emb ~scope_kind
                          ~scope_key ~limit:5 ()
                      in
                      Lwt.return results)
                    (fun _exn -> Lwt.return [])
              else Lwt.return []
            in
            let scoped_rows =
              scoped_memory_strings ~db ~scope_kind ~scope_key ?principal_kind
                ?principal_id ~query:user_message ()
            in
            (* Merge keyword + vector results when vector results exist *)
            let keyword_strings =
              List.map
                (fun (s : string) ->
                  (* Strip provenance prefix for merge matching *)
                  match String.index_opt s ' ' with
                  | Some i -> String.sub s (i + 1) (String.length s - i - 1)
                  | None -> s)
                scoped_message_results
            in
            let merged_strings =
              if scoped_vector_results = [] then scoped_message_results
              else
                let merged =
                  Vector.merge_results ~keyword_results:keyword_strings
                    ~vector_results:scoped_vector_results
                    ~keyword_weight:agent.config.memory.keyword_weight
                    ~vector_weight:agent.config.memory.vector_weight
                in
                List.map
                  (fun content ->
                    "[scoped-message:" ^ scope_kind ^ "/" ^ scope_key ^ "] "
                    ^ clip_memory_content content)
                  merged
            in
            let parts = merged_strings @ scoped_rows in
            if parts = [] then Lwt.return_unit
            else begin
              let context_msg =
                Provider.make_message ~role:"system"
                  ~content:
                    ("Relevant scoped memory context:\n"
                   ^ String.concat "\n" parts)
              in
              agent.history <- context_msg :: agent.history;
              Lwt.return_unit
            end
        | _ -> (
            (* Legacy routing fallback: only unprofiled or scope-less sessions
               should reach this branch. Profiled room/thread turns must pass
               scope_kind/scope_key and use the scoped branch above. *)
            (* TODO(scoped-memory-audit): keep this global message search as an
               explicit legacy fallback; do not route profiled rooms here. *)
            (* FTS keyword search *)
            let keyword_results =
              Memory.search ~db ~query:user_message ~limit:5 ()
            in
            let keyword_strings =
              List.map
                (fun (m : Provider.message) -> clip_memory_content m.content)
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
                      Vector.fetch_embedding ~config:agent.config
                        ~text:user_message
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
            (* TODO(scoped-memory-audit): global core memory injection is a
               legacy fallback for unscoped turns; route to scoped memories if
               scope metadata becomes available here. *)
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
                      ("Relevant context from memory:\n"
                     ^ String.concat "\n" parts)
                in
                agent.history <- context_msg :: agent.history;
                Lwt.return_unit))
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
    ?(workspace_refresh_checked = false) ?db
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
