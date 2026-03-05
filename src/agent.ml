type t = {
  mutable history : Provider.message list;
  config : Runtime_config.t;
  system_prompt : string;
  tool_registry : Tool_registry.t option;
}

let max_history = 50

let create ~config ?tool_registry () =
  {
    history = [];
    config;
    system_prompt = config.Runtime_config.agent_defaults.system_prompt;
    tool_registry;
  }

let build_messages agent =
  Provider.make_message ~role:"system" ~content:agent.system_prompt
  :: List.rev agent.history

let trim_history agent =
  let len = List.length agent.history in
  if len > max_history then
    agent.history <- List.filteri (fun i _ -> i < max_history) agent.history

let tools_json agent =
  match agent.tool_registry with
  | Some r when agent.config.security.tools_enabled ->
    Some (Tool_registry.to_openai_json r)
  | _ -> None

let risk_level_to_string = function
  | Tool.Low -> "low" | Tool.Medium -> "medium" | Tool.High -> "high"

let inject_search_context agent ~db ~user_message =
  if agent.config.memory.search_enabled then
    try
      let results = Memory.search ~db ~query:user_message ~limit:3 () in
      match results with
      | [] -> ()
      | msgs ->
        let context_parts = List.map (fun (m : Provider.message) ->
          Printf.sprintf "[%s]: %s" m.role
            (if String.length m.content > 300
             then String.sub m.content 0 300 ^ "..."
             else m.content)
        ) msgs in
        let context_msg = Provider.make_message ~role:"system"
            ~content:("Relevant context from memory:\n" ^
                      String.concat "\n" context_parts) in
        agent.history <- context_msg :: agent.history
    with _ -> ()

let turn agent ~user_message ?db ?session_key () =
  let open Lwt.Syntax in
  (match db with
   | Some db -> inject_search_context agent ~db ~user_message
   | None -> ());
  agent.history <-
    Provider.make_message ~role:"user" ~content:user_message :: agent.history;
  let audit_enabled = agent.config.security.audit_enabled in
  let max_iters = agent.config.agent_defaults.max_tool_iterations in
  let tools = tools_json agent in
  let rec loop iteration =
    let messages = build_messages agent in
    let* response = Provider.complete ~config:agent.config ~messages ?tools () in
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
  (match db with
   | Some db -> inject_search_context agent ~db ~user_message
   | None -> ());
  agent.history <-
    Provider.make_message ~role:"user" ~content:user_message :: agent.history;
  let audit_enabled = agent.config.security.audit_enabled in
  let max_iters = agent.config.agent_defaults.max_tool_iterations in
  let tools = tools_json agent in
  let rec loop iteration =
    let messages = build_messages agent in
    let* response = Provider.complete_stream ~config:agent.config ~messages ?tools
        ~on_chunk () in
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
