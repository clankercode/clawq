(* Provider implementation for Ollama /api/chat *)

let default_base_url = "http://localhost:11434"

let messages_to_ollama_json messages =
  List.filter_map
    (fun (m : Provider.message) ->
      match m.role with
      | "system" ->
          Some
            (`Assoc
               [ ("role", `String "system"); ("content", `String m.content) ])
      | "tool" ->
          (* Ollama tool results: role "tool" with content *)
          let fields =
            [ ("role", `String "tool"); ("content", `String m.content) ]
          in
          let fields =
            match m.tool_call_id with
            | Some id -> fields @ [ ("tool_call_id", `String id) ]
            | None -> fields
          in
          Some (`Assoc fields)
      | "assistant" when m.Provider.tool_calls <> [] ->
          let tool_calls_json =
            `List
              (List.map
                 (fun (tc : Provider.tool_call) ->
                   let args =
                     try Yojson.Safe.from_string tc.arguments
                     with _ -> `Assoc []
                   in
                   `Assoc
                     [
                       ("id", `String tc.id);
                       ( "function",
                         `Assoc
                           [
                             ("name", `String tc.function_name);
                             ("arguments", args);
                           ] );
                     ])
                 m.Provider.tool_calls)
          in
          Some
            (`Assoc
               [
                 ("role", `String "assistant");
                 ("content", `String "");
                 ("tool_calls", tool_calls_json);
               ])
      | role ->
          Some
            (`Assoc [ ("role", `String role); ("content", `String m.content) ]))
    messages

let tools_to_ollama_json tools =
  (* Ollama uses OpenAI-compatible tool format *)
  match tools with
  | Some (`List ts) when ts <> [] -> Some (`List ts)
  | _ -> None

let parse_tool_calls_from_message msg_json =
  try
    let open Yojson.Safe.Util in
    let tc_list = msg_json |> member "tool_calls" |> to_list in
    List.filter_map
      (fun tc ->
        try
          let fn = tc |> member "function" in
          let id = try tc |> member "id" |> to_string with _ -> "" in
          let function_name = fn |> member "name" |> to_string in
          let arguments =
            try
              let args = fn |> member "arguments" in
              match args with `String s -> s | _ -> Yojson.Safe.to_string args
            with _ -> "{}"
          in
          Some { Provider.id; function_name; arguments }
        with _ -> None)
      tc_list
  with _ -> []

let complete ~(config : Runtime_config.t)
    ~(provider : Runtime_config.provider_config) ~model ~messages ?tools () =
  let open Lwt.Syntax in
  let base_url =
    match provider.base_url with Some url -> url | None -> default_base_url
  in
  let uri = base_url ^ "/api/chat" in
  let ollama_messages = messages_to_ollama_json messages in
  let body_fields =
    [
      ("model", `String model);
      ("messages", `List ollama_messages);
      ("stream", `Bool false);
      ( "options",
        `Assoc [ ("temperature", `Float (max 1e-8 config.default_temperature)) ]
      );
    ]
  in
  let body_fields =
    match tools_to_ollama_json tools with
    | Some t -> body_fields @ [ ("tools", t) ]
    | None -> body_fields
  in
  let body = `Assoc body_fields |> Yojson.Safe.to_string in
  (* Ollama does not require an API key *)
  let headers = [] in
  Logs.info (fun m ->
      m "Ollama request to %s model=%s msgs=%d" uri model (List.length messages));
  let* status, response_body = Http_client.post_json ~uri ~headers ~body in
  if status < 200 || status >= 300 then
    Lwt.fail_with
      (Printf.sprintf "Ollama API error (HTTP %d): %s" status response_body)
  else
    try
      let json = Yojson.Safe.from_string response_body in
      let open Yojson.Safe.Util in
      let msg = json |> member "message" in
      let resp_model =
        try json |> member "model" |> to_string with _ -> model
      in
      let tool_calls = parse_tool_calls_from_message msg in
      if tool_calls <> [] then
        Lwt.return
          (Provider.ToolCalls
             { calls = tool_calls; model = resp_model; usage = None })
      else
        let raw_content =
          try msg |> member "content" |> to_string with _ -> ""
        in
        let content =
          match Provider.thinking_style_of_provider provider with
          | Provider.TaggedThinking ->
              fst (Provider.split_tagged_text raw_content)
          | Provider.NoThinking | Provider.ReasoningContent -> raw_content
        in
        if content = "" && raw_content = "" then
          Lwt.fail_with "Failed to extract content from Ollama response"
        else
          Lwt.return
            (Provider.Text { content; model = resp_model; usage = None })
    with exn ->
      Lwt.fail_with
        ("Failed to parse Ollama response: " ^ Printexc.to_string exn)

let complete_streaming ~(config : Runtime_config.t)
    ~(provider : Runtime_config.provider_config) ~model ~messages ?tools
    ~on_chunk () =
  let open Lwt.Syntax in
  let base_url =
    match provider.base_url with Some url -> url | None -> default_base_url
  in
  let uri = base_url ^ "/api/chat" in
  let ollama_messages = messages_to_ollama_json messages in
  let body_fields =
    [
      ("model", `String model);
      ("messages", `List ollama_messages);
      ("stream", `Bool true);
      ( "options",
        `Assoc [ ("temperature", `Float (max 1e-8 config.default_temperature)) ]
      );
    ]
  in
  let body_fields =
    match tools_to_ollama_json tools with
    | Some t -> body_fields @ [ ("tools", t) ]
    | None -> body_fields
  in
  let body = `Assoc body_fields |> Yojson.Safe.to_string in
  let headers = [] in
  Logs.info (fun m ->
      m "Ollama stream request to %s model=%s msgs=%d" uri model
        (List.length messages));
  let* status, stream = Http_client.post_stream ~uri ~headers ~body in
  if status < 200 || status >= 300 then begin
    let* chunks = Lwt_stream.to_list stream in
    let response_body = String.concat "" chunks in
    Lwt.fail_with
      (Printf.sprintf "Ollama API error (HTTP %d): %s" status response_body)
  end
  else
    (* NDJSON streaming: each line is a JSON object.
       done:true signals the end. Final message has the full response. *)
    let buf = Buffer.create 256 in
    let content_acc = Buffer.create 1024 in
    let resp_model = ref model in
    let tool_calls_acc : Provider.tool_call list ref = ref [] in
    let tagged_state = { Provider.in_thinking = false; pending = "" } in
    let process_line line =
      if line = "" then Lwt.return_unit
      else
        try
          let json = Yojson.Safe.from_string line in
          let open Yojson.Safe.Util in
          (try resp_model := json |> member "model" |> to_string with _ -> ());
          let done_flag =
            try json |> member "done" |> to_bool with _ -> false
          in
          if done_flag then begin
            (* Final message - check for tool calls *)
            let msg = try json |> member "message" with _ -> `Null in
            let tc = parse_tool_calls_from_message msg in
            if tc <> [] then tool_calls_acc := tc;
            let* () =
              match Provider.thinking_style_of_provider provider with
              | Provider.TaggedThinking ->
                  Provider.flush_tagged_content_delta ~state:tagged_state
                    ~content_acc ~on_chunk ()
              | Provider.NoThinking | Provider.ReasoningContent ->
                  Lwt.return_unit
            in
            let* () = on_chunk Provider.Done in
            Lwt.return_unit
          end
          else begin
            let msg = try json |> member "message" with _ -> `Null in
            let content =
              try msg |> member "content" |> to_string with _ -> ""
            in
            if content <> "" then begin
              match Provider.thinking_style_of_provider provider with
              | Provider.TaggedThinking ->
                  Provider.emit_tagged_content_delta ~state:tagged_state
                    ~content_acc ~on_chunk content
              | Provider.NoThinking | Provider.ReasoningContent ->
                  Buffer.add_string content_acc content;
                  on_chunk (Provider.Delta content)
            end
            else Lwt.return_unit
          end
        with _ -> Lwt.return_unit
    in
    let process_buffer () =
      let s = Buffer.contents buf in
      Buffer.clear buf;
      let lines = String.split_on_char '\n' s in
      let rec process_lines = function
        | [] -> Lwt.return_unit
        | [ last ] ->
            Buffer.add_string buf last;
            Lwt.return_unit
        | line :: rest ->
            let line =
              if String.length line > 0 && line.[String.length line - 1] = '\r'
              then String.sub line 0 (String.length line - 1)
              else line
            in
            let* () = process_line line in
            process_lines rest
      in
      process_lines lines
    in
    let* () =
      Lwt_stream.iter_s
        (fun chunk ->
          Buffer.add_string buf chunk;
          process_buffer ())
        stream
    in
    let remaining = Buffer.contents buf in
    let* () =
      if remaining <> "" then process_line remaining else Lwt.return_unit
    in
    let content = Buffer.contents content_acc in
    let final_model = !resp_model in
    if !tool_calls_acc <> [] then
      Lwt.return
        (Provider.ToolCalls
           { calls = !tool_calls_acc; model = final_model; usage = None })
    else
      Lwt.return (Provider.Text { content; model = final_model; usage = None })
