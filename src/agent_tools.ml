let risk_level_to_string = function
  | Tool.Low -> "low"
  | Tool.Medium -> "medium"
  | Tool.High -> "high"

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

let active_workspace_files config = active_workspace_files_for_config config

let capture_active_workspace_file_state config =
  capture_active_workspace_file_state_for_config config

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

let active_workspace_refresh_targets_from_call ~config
    ~(tool_registry : Tool_registry.t option) (tc : Provider.tool_call) result =
  if String.starts_with ~prefix:"Error:" result then None
  else
    let configured = active_workspace_files config in
    let find_configured_file resolved_path =
      List.find_map
        (fun (file, configured_path) ->
          if resolved_path = configured_path then Some file else None)
        configured
    in
    ignore tool_registry;
    try
      let open Yojson.Safe.Util in
      let args = Yojson.Safe.from_string tc.arguments in
      match tc.function_name with
      | "doc_write" ->
          let filename = args |> member "filename" |> to_string in
          if
            List.mem filename (config : Runtime_config.t).prompt.workspace_files
          then Some [ filename ]
          else None
      | "file_write" | "file_append" | "file_edit" | "file_edit_lines" ->
          let workspace = Runtime_config.effective_workspace config in
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

let observe_workspace_refresh ~config ~tool_registry tc result
    ~before_active_workspace_files =
  let direct_targets =
    match
      active_workspace_refresh_targets_from_call ~config ~tool_registry tc
        result
    with
    | Some files -> files
    | None -> []
  in
  let after_state = capture_active_workspace_file_state config in
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

let sync_observed_active_workspace_files ~config ~observed_ref =
  observed_ref := capture_active_workspace_file_state config

let restore_observed_active_workspace_files ~config ~observed_ref saved_state =
  let current_state = capture_active_workspace_file_state config in
  observed_ref :=
    List.map
      (fun (file, current_digest) ->
        let restored_digest =
          match List.assoc_opt file saved_state with
          | Some digest -> digest
          | None -> current_digest
        in
        (file, restored_digest))
      current_state

let note_external_workspace_refresh_if_needed ~config ~observed_ref ~history_ref
    =
  let before_state = !observed_ref in
  let after_state = capture_active_workspace_file_state config in
  observed_ref := after_state;
  match changed_active_workspace_files before_state after_state with
  | [] -> None
  | filenames ->
      let refresh_msg = workspace_refresh_event filenames in
      history_ref := refresh_msg :: !history_ref;
      Some refresh_msg

let append_tool_history ~history_ref ~observed_ref ~config tool_msg
    refresh_msg_opt =
  history_ref := tool_msg :: !history_ref;
  match refresh_msg_opt with
  | Some _refresh_msg ->
      history_ref := _refresh_msg :: !history_ref;
      observed_ref := capture_active_workspace_file_state config
  | None -> ()

let resolve_tool_search ~(tool_registry : Tool_registry.t option)
    (tc : Provider.tool_call) =
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
  match tool_registry with
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

let execute_tool_calls_stream ~config ~(tool_registry : Tool_registry.t option)
    ~history_ref ~observed_ref ~db ~audit_enabled ~session_key ?interrupt_check
    ~on_chunk calls =
  let open Lwt.Syntax in
  let queued_message_interrupt_token = "[queued inbound message]" in
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
              match tool_registry with
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
            capture_active_workspace_file_state config
          in
          let is_tool_search = tc.function_name = "tool_search" in
          let streamed_output = ref false in
          let t0 = Unix.gettimeofday () in
          let* result_msg, result_for_event =
            if is_tool_search then
              let msg = resolve_tool_search ~tool_registry tc in
              Lwt.return (msg, msg.Provider.content)
            else
              let* result =
                match tool_registry with
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
                                              ~history:!history_ref msgs
                                          in
                                          List.iter
                                            (fun content ->
                                              history_ref :=
                                                Provider.make_message
                                                  ~role:"system" ~content
                                                :: !history_ref)
                                            msgs);
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
                Tool_postprocess.process_tool_result ~config ~db ~session_key
                  ~tool_name:tc.function_name ~history:!history_ref
                  ~raw_result:result
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
            observe_workspace_refresh ~config ~tool_registry tc result
              ~before_active_workspace_files
          in
          if Option.is_some refresh.message then
            observed_ref := refresh.after_state;
          Lwt.return (tc, result_msg, refresh.message))
      calls
  in
  List.iter
    (fun ((_tc : Provider.tool_call), tool_msg, refresh_msg) ->
      append_tool_history ~history_ref ~observed_ref ~config tool_msg
        refresh_msg)
    results;
  let* () = Lwt.join !notification_promises in
  Lwt.return_unit

let execute_tool_calls ~config ~(tool_registry : Tool_registry.t option)
    ~history_ref ~observed_ref ~db ~audit_enabled ~session_key ?interrupt_check
    calls =
  let open Lwt.Syntax in
  let queued_message_interrupt_token = "[queued inbound message]" in
  let sk_tag = match session_key with Some s -> "[" ^ s ^ "] " | None -> "" in
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
              match tool_registry with
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
            capture_active_workspace_file_state config
          in
          let is_tool_search = tc.function_name = "tool_search" in
          let* result_msg =
            if is_tool_search then
              Lwt.return (resolve_tool_search ~tool_registry tc)
            else
              let* result =
                match tool_registry with
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
                                              ~history:!history_ref msgs
                                          in
                                          List.iter
                                            (fun content ->
                                              history_ref :=
                                                Provider.make_message
                                                  ~role:"system" ~content
                                                :: !history_ref)
                                            msgs);
                                  }
                                in
                                tool.invoke ~context args)
                          (fun exn ->
                            Lwt.return
                              ("Error invoking tool: " ^ Printexc.to_string exn))
                    )
              in
              let* result_for_history =
                Tool_postprocess.process_tool_result ~config ~db ~session_key
                  ~tool_name:tc.function_name ~history:!history_ref
                  ~raw_result:result
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
            observe_workspace_refresh ~config ~tool_registry tc result
              ~before_active_workspace_files
          in
          if Option.is_some refresh.message then
            observed_ref := refresh.after_state;
          Lwt.return (tc, result_msg, refresh.message))
      calls
  in
  List.iter
    (fun ((_tc : Provider.tool_call), tool_msg, refresh_msg) ->
      append_tool_history ~history_ref ~observed_ref ~config tool_msg
        refresh_msg)
    results;
  Lwt.return_unit
