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

let turn agent ~user_message =
  let open Lwt.Syntax in
  agent.history <-
    Provider.make_message ~role:"user" ~content:user_message :: agent.history;
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
