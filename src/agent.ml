type t = {
  mutable history : Provider.message list;
  mutable config : Runtime_config.t;
  mutable system_prompt : string;
  tool_registry : Tool_registry.t option;
}

let create ~config ?tool_registry () =
  let system_prompt = Prompt_builder.build ~config ~tool_registry in
  {
    history = [];
    config;
    system_prompt;
    tool_registry;
  }

let build_messages agent =
  agent.system_prompt <- Prompt_builder.build ~config:agent.config ~tool_registry:agent.tool_registry;
  Provider.make_message ~role:"system" ~content:agent.system_prompt
  :: List.rev agent.history

let trim_history agent =
  let effective_max =
    let m = agent.config.memory.max_messages_per_session in
    if m <= 0 then 500 else min m 500
  in
  let len = List.length agent.history in
  if len > effective_max then
    agent.history <- List.filteri (fun i _ -> i < effective_max) agent.history

let tools_json agent =
  match agent.tool_registry with
  | Some r when agent.config.security.tools_enabled ->
    Some (Tool_registry.to_openai_json r)
  | _ -> None

let risk_level_to_string = function
  | Tool.Low -> "low" | Tool.Medium -> "medium" | Tool.High -> "high"

let inject_search_context agent ~db ~user_message =
  let open Lwt.Syntax in
  if agent.config.memory.search_enabled then
    Lwt.catch
      (fun () ->
        (* FTS keyword search *)
        let keyword_results = Memory.search ~db ~query:user_message ~limit:5 () in
        let keyword_strings = List.map (fun (m : Provider.message) ->
          if String.length m.content > 300
          then String.sub m.content 0 300 ^ "..."
          else m.content
        ) keyword_results in
        (* Vector search (if embedding provider is configured) *)
        let* vector_strings =
          if agent.config.memory.embedding_provider <> None
             || agent.config.memory.embedding_model <> None then
            Lwt.catch
              (fun () ->
                let* query_emb = Vector.fetch_embedding ~config:agent.config ~text:user_message in
                let results = Vector.search ~db ~query_embedding:query_emb ~limit:5 () in
                Lwt.return results)
              (fun _exn -> Lwt.return [])
          else
            Lwt.return []
        in
        (* Merge results *)
        let merged =
          if vector_strings = [] then
            keyword_strings
          else
            Vector.merge_results
              ~keyword_results:keyword_strings
              ~vector_results:vector_strings
              ~keyword_weight:agent.config.memory.keyword_weight
              ~vector_weight:agent.config.memory.vector_weight
        in
        let top = List.filteri (fun i _ -> i < 3) merged in
        (match top with
         | [] -> Lwt.return_unit
         | parts ->
           let context_msg = Provider.make_message ~role:"system"
               ~content:("Relevant context from memory:\n" ^
                         String.concat "\n" parts) in
           agent.history <- context_msg :: agent.history;
           Lwt.return_unit))
      (fun _ -> Lwt.return_unit)
  else
    Lwt.return_unit

let turn agent ~user_message ?db ?session_key () =
  let open Lwt.Syntax in
  let* () = match db with
    | Some db -> inject_search_context agent ~db ~user_message
    | None -> Lwt.return_unit
  in
  agent.history <-
    Provider.make_message ~role:"user" ~content:user_message :: agent.history;
  let audit_enabled = agent.config.security.audit_enabled in
  let max_iters = agent.config.agent_defaults.max_tool_iterations in
  let tools = tools_json agent in
  let resilient_complete config messages tools =
    let res = config.Runtime_config.resilience in
    let open Lwt.Syntax in
    let* timed =
      Resilience.with_timeout_retry ~timeout_s:res.timeout_s
      ~max_retries:res.max_retries ~base_delay_s:res.base_delay_s
      (fun () ->
        Resilience.with_fallback
          ~primary:(fun () -> Provider.complete ~config ~messages ?tools ())
          ~fallback:(fun () ->
            match res.fallback_provider with
            | Some fb_name ->
              let fb_config = { config with default_provider = Some fb_name } in
              Provider.complete ~config:fb_config ~messages ?tools ()
            | None -> Provider.complete ~config ~messages ?tools ()))
    in
    match timed with
    | Ok v -> Lwt.return v
    | Error e -> Lwt.fail_with e
  in
  let rec loop iteration =
    let messages = build_messages agent in
    let* response = resilient_complete agent.config messages tools in
    match response with
    | Provider.Text { content; _ } ->
      agent.history <-
        Provider.make_message ~role:"assistant" ~content :: agent.history;
      trim_history agent;
      Lwt.return content
    | Provider.ToolCalls { calls; _ } when tools = None ->
      let content = "I attempted to use tools (" ^
        (String.concat ", " (List.map (fun (tc : Provider.tool_call) -> tc.function_name) calls)) ^
        ") but tools are disabled. Set security.tools_enabled to true in ~/.clawq/config.json to enable them." in
      agent.history <-
        Provider.make_message ~role:"assistant" ~content :: agent.history;
      trim_history agent;
      Lwt.return content
    | Provider.ToolCalls { calls; _ } when iteration >= max_iters ->
      let content = "I've reached the maximum number of tool iterations. Here's what I was trying to do: " ^
                    (String.concat ", " (List.map (fun (tc : Provider.tool_call) -> tc.function_name) calls))
      in
      agent.history <-
        Provider.make_message ~role:"assistant" ~content :: agent.history;
      trim_history agent;
      Lwt.return content
    | Provider.ToolCalls { calls; _ } ->
      let assistant_msg =
        { Provider.role = "assistant"; content = "";
          tool_calls = calls; tool_call_id = None; name = None }
      in
      agent.history <- assistant_msg :: agent.history;
      let* () =
        Lwt_list.iter_s
          (fun (tc : Provider.tool_call) ->
            Logs.info (fun m -> m "Tool call: %s (id=%s) args=%s"
              tc.function_name tc.id tc.arguments);
            (match db, audit_enabled, session_key with
             | Some db, true, Some sk ->
               let risk = match agent.tool_registry with
                 | Some reg -> (match Tool_registry.find reg tc.function_name with
                   | Some t -> risk_level_to_string t.risk_level
                   | None -> "unknown")
                 | None -> "unknown"
               in
               Audit.log ~db (ToolInvocation {
                 session_key = sk; tool_name = tc.function_name;
                 risk_level = risk; args_preview = tc.arguments })
             | _ -> ());
            let* result =
              match agent.tool_registry with
              | None -> Lwt.return "Error: no tool registry available"
              | Some registry -> (
                match Tool_registry.find registry tc.function_name with
                | None ->
                  Lwt.return
                    (Printf.sprintf "Error: unknown tool '%s'" tc.function_name)
                | Some tool ->
                  Lwt.catch
                    (fun () ->
                      let args =
                        try Yojson.Safe.from_string tc.arguments
                        with _ -> `Assoc []
                      in
                      tool.invoke args)
                    (fun exn ->
                      Lwt.return
                        ("Error invoking tool: " ^ Printexc.to_string exn)))
            in
            let success = not (String.length result >= 6
                               && String.sub result 0 6 = "Error:") in
            (match db, audit_enabled, session_key with
             | Some db, true, Some sk ->
               Audit.log ~db (ToolResult {
                 session_key = sk; tool_name = tc.function_name; success })
             | _ -> ());
            let truncated =
              if String.length result > 200 then String.sub result 0 200 ^ "..."
              else result
            in
            Logs.info (fun m -> m "Tool result: %s -> %s" tc.function_name truncated);
            let tool_msg =
              Provider.make_tool_result ~tool_call_id:tc.id
                ~name:tc.function_name ~content:result
            in
            agent.history <- tool_msg :: agent.history;
            Lwt.return_unit)
          calls
      in
      loop (iteration + 1)
  in
  loop 0

let turn_stream agent ~user_message ?db ?session_key ~on_chunk () =
  let open Lwt.Syntax in
  let* () = match db with
    | Some db -> inject_search_context agent ~db ~user_message
    | None -> Lwt.return_unit
  in
  agent.history <-
    Provider.make_message ~role:"user" ~content:user_message :: agent.history;
  let audit_enabled = agent.config.security.audit_enabled in
  let max_iters = agent.config.agent_defaults.max_tool_iterations in
  let tools = tools_json agent in
  let resilient_stream config messages tools on_chunk =
    let res = config.Runtime_config.resilience in
    let open Lwt.Syntax in
    let* timed =
      Resilience.with_timeout_retry ~timeout_s:res.timeout_s
      ~max_retries:res.max_retries ~base_delay_s:res.base_delay_s
      (fun () ->
        Resilience.with_fallback
          ~primary:(fun () ->
            Provider.complete_stream ~config ~messages ?tools ~on_chunk ())
          ~fallback:(fun () ->
            match res.fallback_provider with
            | Some fb_name ->
              let fb_config = { config with default_provider = Some fb_name } in
              Provider.complete_stream ~config:fb_config ~messages ?tools ~on_chunk ()
            | None ->
              Provider.complete_stream ~config ~messages ?tools ~on_chunk ()))
    in
    match timed with
    | Ok v -> Lwt.return v
    | Error e -> Lwt.fail_with e
  in
  let rec loop iteration =
    let messages = build_messages agent in
    let* response = resilient_stream agent.config messages tools on_chunk in
    match response with
    | Provider.Text { content; _ } ->
      agent.history <-
        Provider.make_message ~role:"assistant" ~content :: agent.history;
      trim_history agent;
      Lwt.return content
    | Provider.ToolCalls { calls; _ } when tools = None ->
      let content = "I attempted to use tools (" ^
        (String.concat ", " (List.map (fun (tc : Provider.tool_call) -> tc.function_name) calls)) ^
        ") but tools are disabled." in
      agent.history <-
        Provider.make_message ~role:"assistant" ~content :: agent.history;
      trim_history agent;
      let* () = on_chunk (Provider.Delta content) in
      Lwt.return content
    | Provider.ToolCalls { calls; _ } when iteration >= max_iters ->
      let content = "I've reached the maximum number of tool iterations." in
      agent.history <-
        Provider.make_message ~role:"assistant" ~content :: agent.history;
      trim_history agent;
      let* () = on_chunk (Provider.Delta content) in
      Lwt.return content
    | Provider.ToolCalls { calls; _ } ->
      let assistant_msg =
        { Provider.role = "assistant"; content = "";
          tool_calls = calls; tool_call_id = None; name = None }
      in
      agent.history <- assistant_msg :: agent.history;
      let* () =
        Lwt_list.iter_s
          (fun (tc : Provider.tool_call) ->
            Logs.info (fun m -> m "Tool call: %s (id=%s) args=%s"
              tc.function_name tc.id tc.arguments);
            (match db, audit_enabled, session_key with
             | Some db, true, Some sk ->
               let risk = match agent.tool_registry with
                 | Some reg -> (match Tool_registry.find reg tc.function_name with
                   | Some t -> risk_level_to_string t.risk_level
                   | None -> "unknown")
                 | None -> "unknown"
               in
               Audit.log ~db (ToolInvocation {
                 session_key = sk; tool_name = tc.function_name;
                 risk_level = risk; args_preview = tc.arguments })
             | _ -> ());
            let* result =
              match agent.tool_registry with
              | None -> Lwt.return "Error: no tool registry available"
              | Some registry -> (
                match Tool_registry.find registry tc.function_name with
                | None ->
                  Lwt.return
                    (Printf.sprintf "Error: unknown tool '%s'" tc.function_name)
                | Some tool ->
                  Lwt.catch
                    (fun () ->
                      let args =
                        try Yojson.Safe.from_string tc.arguments
                        with _ -> `Assoc []
                      in
                      tool.invoke args)
                    (fun exn ->
                      Lwt.return
                        ("Error invoking tool: " ^ Printexc.to_string exn)))
            in
            let success = not (String.length result >= 6
                               && String.sub result 0 6 = "Error:") in
            (match db, audit_enabled, session_key with
             | Some db, true, Some sk ->
               Audit.log ~db (ToolResult {
                 session_key = sk; tool_name = tc.function_name; success })
             | _ -> ());
            let tool_msg =
              Provider.make_tool_result ~tool_call_id:tc.id
                ~name:tc.function_name ~content:result
            in
            agent.history <- tool_msg :: agent.history;
            Lwt.return_unit)
          calls
      in
      loop (iteration + 1)
  in
  loop 0
