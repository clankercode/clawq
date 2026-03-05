type message = {
  role : string;
  content : string;
  tool_calls : tool_call list;
  tool_call_id : string option;
  name : string option;
}

and tool_call = { id : string; function_name : string; arguments : string }

type completion_response =
  | Text of { content : string; model : string; usage : (int * int) option }
  | ToolCalls of {
      calls : tool_call list;
      model : string;
      usage : (int * int) option;
    }

let make_message ~role ~content =
  { role; content; tool_calls = []; tool_call_id = None; name = None }

let make_tool_result ~tool_call_id ~name ~content =
  { role = "tool"; content; tool_calls = []; tool_call_id = Some tool_call_id; name = Some name }

let message_to_json m =
  let fields = [ ("role", `String m.role) ] in
  let fields =
    match m.role with
    | "tool" ->
      let fields = fields @ [ ("content", `String m.content) ] in
      let fields =
        match m.tool_call_id with
        | Some id -> fields @ [ ("tool_call_id", `String id) ]
        | None -> fields
      in
      (match m.name with
       | Some n -> fields @ [ ("name", `String n) ]
       | None -> fields)
    | "assistant" when m.tool_calls <> [] ->
      let tc_json =
        `List
          (List.map
             (fun tc ->
               `Assoc
                 [
                   ("id", `String tc.id);
                   ("type", `String "function");
                   ( "function",
                     `Assoc
                       [
                         ("name", `String tc.function_name);
                         ("arguments", `String tc.arguments);
                       ] );
                 ])
             m.tool_calls)
      in
      fields @ [ ("tool_calls", tc_json) ]
    | _ -> fields @ [ ("content", `String m.content) ]
  in
  `Assoc fields

let messages_to_json messages = `List (List.map message_to_json messages)

let complete ~(config : Runtime_config.t) ~messages ?tools () =
  let open Lwt.Syntax in
  let model = config.agent_defaults.primary_model in
  let provider_name, provider =
    let find_named name =
      List.find_opt (fun (n, _) -> n = name) config.providers
    in
    let with_key =
      List.filter (fun (_, p) -> Runtime_config.is_key_set p.Runtime_config.api_key) config.providers
    in
    let preferred =
      match config.default_provider with
      | Some name ->
        (match find_named name with
         | Some (n, p) when Runtime_config.is_key_set p.api_key -> Some (n, p)
         | _ -> None)
      | None -> None
    in
    match preferred with
    | Some pair -> pair
    | None ->
      (match with_key with
       | (name, p) :: _ -> (name, p)
       | [] ->
         (match config.providers with
          | (name, p) :: _ -> (name, p)
          | [] ->
            ( "default",
              { Runtime_config.api_key = ""; base_url = None } )))
  in
  let base_url =
    match provider.base_url with
    | Some url -> url
    | None -> "https://openrouter.ai/api/v1"
  in
  let uri = base_url ^ "/chat/completions" in
  let body_fields =
    [
      ("model", `String model);
      ("messages", messages_to_json messages);
      ("temperature", `Float (max 1e-8 config.default_temperature));
    ]
  in
  let body_fields =
    match tools with
    | Some t when t <> `List [] -> body_fields @ [ ("tools", t) ]
    | _ -> body_fields
  in
  let body = `Assoc body_fields |> Yojson.Safe.to_string in
  let headers =
    [ ("Authorization", "Bearer " ^ provider.api_key) ]
  in
  Logs.info (fun m ->
      m "LLM request to %s provider=%s model=%s msgs=%d" uri provider_name
        model (List.length messages));
  let* status, response_body = Http_client.post_json ~uri ~headers ~body in
  if status < 200 || status >= 300 then begin
    if status = 400 then
      try
        let err_json = Yojson.Safe.from_string response_body in
        let open Yojson.Safe.Util in
        let failed_gen =
          try err_json |> member "error" |> member "failed_generation" |> to_string
          with _ -> ""
        in
        if failed_gen <> "" then
          Lwt.return (Text { content = failed_gen; model; usage = None })
        else
          Lwt.fail_with
            (Printf.sprintf "LLM API error (HTTP %d): %s" status response_body)
      with _ ->
        Lwt.fail_with
          (Printf.sprintf "LLM API error (HTTP %d): %s" status response_body)
    else
      Lwt.fail_with
        (Printf.sprintf "LLM API error (HTTP %d): %s" status response_body)
  end else
    let json =
      try Ok (Yojson.Safe.from_string response_body)
      with exn -> Error (Printexc.to_string exn)
    in
    match json with
    | Error msg ->
      Lwt.fail_with ("Failed to parse LLM response JSON: " ^ msg)
    | Ok json ->
      let open Yojson.Safe.Util in
      let choice =
        try json |> member "choices" |> index 0 |> member "message"
        with _ -> `Null
      in
      let tool_calls_json =
        try choice |> member "tool_calls" |> to_list with _ -> []
      in
      let resp_model =
        try json |> member "model" |> to_string with _ -> model
      in
      let usage =
        try
          let u = json |> member "usage" in
          let pt = u |> member "prompt_tokens" |> to_int in
          let ct = u |> member "completion_tokens" |> to_int in
          Some (pt, ct)
        with _ -> None
      in
      if tool_calls_json <> [] then
        let calls =
          List.filter_map
            (fun tc ->
              try
                let id = tc |> member "id" |> to_string in
                let fn = tc |> member "function" in
                let function_name = fn |> member "name" |> to_string in
                let arguments = fn |> member "arguments" |> to_string in
                Some { id; function_name; arguments }
              with _ -> None)
            tool_calls_json
        in
        Lwt.return (ToolCalls { calls; model = resp_model; usage })
      else
        let content =
          try choice |> member "content" |> to_string with _ -> ""
        in
        if content = "" then
          Lwt.fail_with "Failed to extract content from LLM response"
        else
          Lwt.return (Text { content; model = resp_model; usage })
