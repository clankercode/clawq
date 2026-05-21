(* Provider implementation for Anthropic Messages API *)

let anthropic_version = "2023-06-01"

let messages_to_anthropic_json = Provider.messages_to_anthropic_json

let tools_to_anthropic_json tools =
  (* Convert OpenAI-format tools JSON to Anthropic function declarations.
     OpenAI: [{type:"function", function:{name,description,parameters:{type,properties,required}}}]
     Anthropic: [{name, description, input_schema:{type,properties,required}}] *)
  match tools with
  | None -> None
  | Some (`List ts) ->
      let converted =
        List.filter_map
          (fun t ->
            try
              let open Yojson.Safe.Util in
              let fn = t |> member "function" in
              let name = fn |> member "name" |> to_string in
              let description =
                try fn |> member "description" |> to_string with _ -> ""
              in
              let parameters =
                try fn |> member "parameters"
                with _ ->
                  `Assoc
                    [ ("type", `String "object"); ("properties", `Assoc []) ]
              in
              Some
                (`Assoc
                   [
                     ("name", `String name);
                     ("description", `String description);
                     ("input_schema", parameters);
                   ])
            with _ -> None)
          ts
      in
      if converted = [] then None else Some (`List converted)
  | Some _ -> None

let extract_system_prompt = Provider.extract_system_prompt

let parse_anthropic_response body model =
  try
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    let stop_reason =
      try json |> member "stop_reason" |> to_string with _ -> ""
    in
    let resp_model =
      try json |> member "model" |> to_string with _ -> model
    in
    let usage =
      try
        let u = json |> member "usage" in
        let input_tokens = u |> member "input_tokens" |> to_int in
        let output_tokens = u |> member "output_tokens" |> to_int in
        let cached =
          try u |> member "cache_read_input_tokens" |> to_int with _ -> 0
        in
        Some (input_tokens, output_tokens, cached)
      with _ -> None
    in
    let content_list = try json |> member "content" |> to_list with _ -> [] in
    let thinking_text =
      List.fold_left
        (fun acc block ->
          try
            let block_type = block |> member "type" |> to_string in
            if block_type = "thinking" then
              let t = block |> member "thinking" |> to_string in
              if acc = "" then t else acc ^ t
            else acc
          with _ -> acc)
        "" content_list
    in
    let thinking = if thinking_text = "" then None else Some thinking_text in
    if stop_reason = "tool_use" then
      let tool_calls =
        List.filter_map
          (fun block ->
            try
              let block_type = block |> member "type" |> to_string in
              if block_type = "tool_use" then
                let id = block |> member "id" |> to_string in
                let function_name = block |> member "name" |> to_string in
                let input = block |> member "input" in
                let arguments = Yojson.Safe.to_string input in
                Some { Provider.id; function_name; arguments }
              else None
            with _ -> None)
          content_list
      in
      if tool_calls <> [] then
        Ok
          (Provider.ToolCalls
             {
               calls = tool_calls;
               model = resp_model;
               usage;
               provider_response_items_json = None;
               thinking;
             })
      else Error "tool_use stop reason but no tool_use blocks found"
    else
      let content =
        List.fold_left
          (fun acc block ->
            try
              let block_type = block |> member "type" |> to_string in
              if block_type = "text" then
                let text = block |> member "text" |> to_string in
                if acc = "" then text else acc ^ text
              else acc
            with _ -> acc)
          "" content_list
      in
      Ok
        (Provider.Text
           {
             content;
             model = resp_model;
             usage;
             provider_response_items_json = None;
             thinking;
           })
  with exn ->
    Error ("Failed to parse Anthropic response: " ^ Printexc.to_string exn)

let complete ~(config : Runtime_config.t)
    ~(provider : Runtime_config.provider_config) ~model ~messages ?tools
    ?session_key:_ () =
  let open Lwt.Syntax in
  let base_url =
    match provider.base_url with
    | Some url -> url
    | None -> "https://api.anthropic.com"
  in
  let uri = base_url ^ "/v1/messages" in
  let system_from_messages = extract_system_prompt messages in
  let system_prompt = system_from_messages in
  let anthropic_messages = messages_to_anthropic_json messages in
  let body_fields =
    [
      ("model", `String model);
      ("max_tokens", `Int 8192);
      ("messages", `List anthropic_messages);
      ("temperature", `Float (max 1e-8 config.default_temperature));
    ]
  in
  let body_fields =
    if system_prompt <> "" then
      body_fields @ [ ("system", `String system_prompt) ]
    else body_fields
  in
  let body_fields =
    match tools_to_anthropic_json tools with
    | Some t -> body_fields @ [ ("tools", t) ]
    | None -> body_fields
  in
  let body_fields =
    match provider.thinking_budget_tokens with
    | Some budget when budget > 0 ->
        body_fields
        @ [
            ( "thinking",
              `Assoc
                [ ("type", `String "enabled"); ("budget_tokens", `Int budget) ]
            );
          ]
    | _ -> body_fields
  in
  let body = `Assoc body_fields |> Yojson.Safe.to_string in
  let headers =
    [
      ("x-api-key", provider.api_key); ("anthropic-version", anthropic_version);
    ]
  in
  Logs.info (fun m ->
      m "Anthropic request to %s model=%s msgs=%d" uri model
        (List.length messages));
  let* status, response_body = Http_client.post_json ~uri ~headers ~body in
  if status < 200 || status >= 300 then
    Lwt.fail_with
      (Printf.sprintf "Anthropic API error (HTTP %d): %s" status response_body)
  else
    match parse_anthropic_response response_body model with
    | Ok resp -> Lwt.return resp
    | Error msg -> Lwt.fail_with msg

let complete_streaming ~(config : Runtime_config.t)
    ~(provider : Runtime_config.provider_config) ~model ~messages ?tools
    ?session_key:_ ~on_chunk () =
  let open Lwt.Syntax in
  let base_url =
    match provider.base_url with
    | Some url -> url
    | None -> "https://api.anthropic.com"
  in
  let uri = base_url ^ "/v1/messages" in
  let system_from_messages = extract_system_prompt messages in
  let system_prompt = system_from_messages in
  let anthropic_messages = messages_to_anthropic_json messages in
  let body_fields =
    [
      ("model", `String model);
      ("max_tokens", `Int 8192);
      ("messages", `List anthropic_messages);
      ("temperature", `Float (max 1e-8 config.default_temperature));
      ("stream", `Bool true);
    ]
  in
  let body_fields =
    if system_prompt <> "" then
      body_fields @ [ ("system", `String system_prompt) ]
    else body_fields
  in
  let body_fields =
    match tools_to_anthropic_json tools with
    | Some t -> body_fields @ [ ("tools", t) ]
    | None -> body_fields
  in
  let body_fields =
    match provider.thinking_budget_tokens with
    | Some budget when budget > 0 ->
        body_fields
        @ [
            ( "thinking",
              `Assoc
                [ ("type", `String "enabled"); ("budget_tokens", `Int budget) ]
            );
          ]
    | _ -> body_fields
  in
  let body = `Assoc body_fields |> Yojson.Safe.to_string in
  let headers =
    [
      ("x-api-key", provider.api_key); ("anthropic-version", anthropic_version);
    ]
  in
  Logs.info (fun m ->
      m "Anthropic stream request to %s model=%s msgs=%d" uri model
        (List.length messages));
  Http_client.post_stream_with ~uri ~headers ~body ~label:"Anthropic API error"
    ~on_ok:(fun stream ->
      (* Parse Anthropic SSE stream.
           Events of interest:
             event: content_block_delta  -> delta.type="text_delta", delta.text
             event: content_block_start  -> type="tool_use" block started
             event: message_delta        -> may have stop_reason="tool_use"
             event: message_stop         -> stream done
        *)
      let buf = Buffer.create 256 in
      let content_acc = Buffer.create 1024 in
      let thinking_acc = Buffer.create 256 in
      let resp_model = ref model in
      let usage_acc = ref None in
      let tool_calls_acc : (int * string * string * Buffer.t) list ref
          (* index, id, name, args_buf *) =
        ref []
      in
      let current_block_type = ref "" in
      let current_block_index = ref 0 in
      let current_tool_id = ref "" in
      let current_tool_name = ref "" in
      let stop_reason = ref "" in
      let process_event event_type data_str =
        try
          let json = Yojson.Safe.from_string data_str in
          let open Yojson.Safe.Util in
          match event_type with
          | "message_start" ->
              (try
                 resp_model :=
                   json |> member "message" |> member "model" |> to_string
               with _ -> ());
              (try
                 let u = json |> member "message" |> member "usage" in
                 let it = u |> member "input_tokens" |> to_int in
                 let ot =
                   try u |> member "output_tokens" |> to_int with _ -> 0
                 in
                 let cached =
                   try u |> member "cache_read_input_tokens" |> to_int
                   with _ -> 0
                 in
                 usage_acc := Some (it, ot, cached)
               with _ -> ());
              Lwt.return_unit
          | "content_block_start" -> (
              try
                current_block_index := json |> member "index" |> to_int;
                let block = json |> member "content_block" in
                let btype = block |> member "type" |> to_string in
                current_block_type := btype;
                if btype = "tool_use" then begin
                  (current_tool_id :=
                     try block |> member "id" |> to_string with _ -> "");
                  (current_tool_name :=
                     try block |> member "name" |> to_string with _ -> "");
                  let args_buf = Buffer.create 256 in
                  tool_calls_acc :=
                    !tool_calls_acc
                    @ [
                        ( !current_block_index,
                          !current_tool_id,
                          !current_tool_name,
                          args_buf );
                      ];
                  on_chunk
                    (Provider.ToolCallDelta
                       {
                         index = !current_block_index;
                         id = Some !current_tool_id;
                         function_name = Some !current_tool_name;
                         arguments = None;
                       })
                end
                else Lwt.return_unit
              with _ -> Lwt.return_unit)
          | "content_block_delta" -> (
              try
                let delta = json |> member "delta" in
                let dtype = delta |> member "type" |> to_string in
                if dtype = "thinking_delta" || !current_block_type = "thinking"
                then begin
                  let thinking =
                    try delta |> member "thinking" |> to_string with _ -> ""
                  in
                  if thinking <> "" then begin
                    Buffer.add_string thinking_acc thinking;
                    on_chunk (Provider.ThinkingDelta thinking)
                  end
                  else Lwt.return_unit
                end
                else if dtype = "text_delta" then begin
                  let text = delta |> member "text" |> to_string in
                  Buffer.add_string content_acc text;
                  on_chunk (Provider.Delta text)
                end
                else begin
                  if dtype = "input_json_delta" then begin
                    let partial = delta |> member "partial_json" |> to_string in
                    List.iter
                      (fun (idx, _, _, args_buf) ->
                        if idx = !current_block_index then
                          Buffer.add_string args_buf partial)
                      !tool_calls_acc;
                    on_chunk
                      (Provider.ToolCallDelta
                         {
                           index = !current_block_index;
                           id = None;
                           function_name = None;
                           arguments = Some partial;
                         })
                  end
                  else Lwt.return_unit
                end
              with _ -> Lwt.return_unit)
          | "content_block_stop" ->
              current_block_type := "";
              current_block_index := 0;
              Lwt.return_unit
          | "message_delta" ->
              (try
                 let d = json |> member "delta" in
                 (stop_reason :=
                    try d |> member "stop_reason" |> to_string with _ -> "");
                 try
                   let u = json |> member "usage" in
                   let ot = u |> member "output_tokens" |> to_int in
                   usage_acc :=
                     match !usage_acc with
                     | Some (it, _, cached) -> Some (it, ot, cached)
                     | None -> Some (0, ot, 0)
                 with _ -> ()
               with _ -> ());
              Lwt.return_unit
          | "message_stop" -> on_chunk Provider.Done
          | _ -> Lwt.return_unit
        with _ -> Lwt.return_unit
      in
      let current_event = ref "" in
      let process_line line =
        let event_prefix = "event: " in
        let data_prefix = "data: " in
        let eplen = String.length event_prefix in
        let dplen = String.length data_prefix in
        if String.length line >= eplen && String.sub line 0 eplen = event_prefix
        then begin
          current_event := String.sub line eplen (String.length line - eplen);
          Lwt.return_unit
        end
        else if
          String.length line >= dplen && String.sub line 0 dplen = data_prefix
        then begin
          let data = String.sub line dplen (String.length line - dplen) in
          process_event !current_event data
        end
        else Lwt.return_unit
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
      let final_model = !resp_model in
      let tool_calls =
        List.map
          (fun (_, id, name, args_buf) ->
            {
              Provider.id;
              function_name = name;
              arguments = Buffer.contents args_buf;
            })
          !tool_calls_acc
      in
      let thinking =
        let t = Buffer.contents thinking_acc in
        if t = "" then None else Some t
      in
      Lwt.return
        (Provider.make_stream_result ~tool_calls ~content ~model:final_model
           ~usage:!usage_acc ~thinking ()))
    ()
