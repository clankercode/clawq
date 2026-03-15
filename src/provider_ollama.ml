(* Provider implementation for Ollama /api/chat *)

let default_base_url = "http://localhost:11434"

let messages_to_ollama_json messages =
  List.filter_map
    (fun (m : Provider.message) ->
      let sc = Provider.sanitize_utf8 m.content in
      match m.role with
      | "system" ->
          Some (`Assoc [ ("role", `String "system"); ("content", `String sc) ])
      | "tool" ->
          (* Ollama tool results: role "tool" with content *)
          let fields = [ ("role", `String "tool"); ("content", `String sc) ] in
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
          let fields =
            match m.Provider.content_parts with
            | [] -> [ ("role", `String role); ("content", `String sc) ]
            | parts ->
                let texts =
                  List.filter_map
                    (fun (p : Provider.content_part) ->
                      match p with
                      | Provider.Text s -> Some (Provider.sanitize_utf8 s)
                      | _ -> None)
                    parts
                in
                let images =
                  List.filter_map
                    (fun (p : Provider.content_part) ->
                      match p with
                      | Provider.Image_base64 { data; _ } -> Some (`String data)
                      | _ -> None)
                    parts
                in
                let content = String.concat "\n" texts in
                let base =
                  [ ("role", `String role); ("content", `String content) ]
                in
                if images <> [] then base @ [ ("images", `List images) ]
                else base
          in
          Some (`Assoc fields))
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
             {
               calls = tool_calls;
               model = resp_model;
               usage = None;
               provider_response_items_json = None;
               thinking = None;
             })
      else
        let raw_content =
          try msg |> member "content" |> to_string with _ -> ""
        in
        let thinking_style = Provider.thinking_style_of_provider provider in
        let content, thinking =
          match thinking_style with
          | Provider.TaggedThinking ->
              let visible, thought = Provider.split_tagged_text raw_content in
              (visible, if thought = "" then None else Some thought)
          | Provider.NoThinking | Provider.ReasoningContent ->
              (raw_content, None)
        in
        if content = "" && raw_content = "" then
          Lwt.fail_with "Failed to extract content from Ollama response"
        else
          Lwt.return
            (Provider.Text
               {
                 content;
                 model = resp_model;
                 usage = None;
                 provider_response_items_json = None;
                 thinking;
               })
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
  Http_client.post_stream_with ~uri ~headers ~body ~label:"Ollama API error"
    ~on_ok:(fun stream ->
      let buf = Buffer.create 256 in
      let content_acc = Buffer.create 1024 in
      let thinking_acc = Buffer.create 256 in
      let resp_model = ref model in
      let tool_calls_acc : Provider.tool_call list ref = ref [] in
      let tagged_state = { Provider.in_thinking = false; pending = "" } in
      let on_chunk chunk =
        (match chunk with
        | Provider.ThinkingDelta text -> Buffer.add_string thinking_acc text
        | _ -> ());
        on_chunk chunk
      in
      let process_line line =
        if line = "" then Lwt.return_unit
        else
          try
            let json = Yojson.Safe.from_string line in
            let open Yojson.Safe.Util in
            (try resp_model := json |> member "model" |> to_string
             with _ -> ());
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
      let* () =
        Lwt_stream.iter_s
          (fun chunk ->
            Buffer.add_string buf chunk;
            Provider.process_sse_buffer ~buf ~process_line ())
          stream
      in
      let remaining = Buffer.contents buf in
      let* () =
        if remaining <> "" then process_line remaining else Lwt.return_unit
      in
      let content = Buffer.contents content_acc in
      let thinking =
        let t = Buffer.contents thinking_acc in
        if t = "" then None else Some t
      in
      let final_model = !resp_model in
      Lwt.return
        (Provider.make_stream_result ~tool_calls:!tool_calls_acc ~content
           ~thinking ~model:final_model ~usage:None ()))
    ()
