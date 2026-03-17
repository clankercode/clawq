(* Provider implementation for Cohere /v2/chat API *)

let cohere_base = "https://api.cohere.com"

let messages_to_cohere_json messages =
  (* Cohere v2/chat: role "user"/"assistant"/"system"/"tool".
     Tool results use role "tool" with tool_call_id. *)
  List.map
    (fun (m : Provider.message) ->
      let sc = Provider.sanitize_utf8 m.content in
      match m.role with
      | "tool" ->
          let fields = [ ("role", `String "tool"); ("content", `String sc) ] in
          let fields =
            match m.tool_call_id with
            | Some id -> fields @ [ ("tool_call_id", `String id) ]
            | None -> fields
          in
          `Assoc fields
      | "assistant" when m.Provider.tool_calls <> [] ->
          let tc_json =
            `List
              (List.map
                 (fun (tc : Provider.tool_call) ->
                   `Assoc
                     [
                       ("id", `String tc.id);
                       ("type", `String "function");
                       ( "function",
                         `Assoc
                           [
                             ("name", `String tc.function_name);
                             ( "arguments",
                               `String (Provider.sanitize_utf8 tc.arguments) );
                           ] );
                     ])
                 m.Provider.tool_calls)
          in
          `Assoc [ ("role", `String "assistant"); ("tool_calls", tc_json) ]
      | "developer" ->
          `Assoc [ ("role", `String "system"); ("content", `String sc) ]
      | role -> `Assoc [ ("role", `String role); ("content", `String sc) ])
    messages

let tools_to_cohere_json tools =
  (* Cohere v2 tool format: {type:"function", function:{name,description,parameters}} *)
  match tools with
  | None -> None
  | Some (`List ts) when ts <> [] -> Some (`List ts)
  | _ -> None

let parse_cohere_response body model =
  try
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    let resp_model =
      try json |> member "model" |> to_string with _ -> model
    in
    let usage =
      try
        let u = json |> member "usage" in
        let pt = u |> member "tokens" |> member "input_tokens" |> to_int in
        let ct = u |> member "tokens" |> member "output_tokens" |> to_int in
        Some (pt, ct, 0)
      with _ -> None
    in
    let message = try json |> member "message" with _ -> `Null in
    let finish_reason =
      try json |> member "finish_reason" |> to_string with _ -> ""
    in
    let tool_calls =
      if finish_reason = "TOOL_CALL" then
        try
          message |> member "tool_calls" |> to_list
          |> List.filter_map (fun tc ->
              try
                let id = tc |> member "id" |> to_string in
                let fn = tc |> member "function" in
                let function_name = fn |> member "name" |> to_string in
                let arguments = fn |> member "arguments" |> to_string in
                Some { Provider.id; function_name; arguments }
              with _ -> None)
        with _ -> []
      else []
    in
    if tool_calls <> [] then
      Ok
        (Provider.ToolCalls
           {
             calls = tool_calls;
             model = resp_model;
             usage;
             provider_response_items_json = None;
             thinking = None;
           })
    else
      let content =
        try
          let content_list = message |> member "content" |> to_list in
          List.fold_left
            (fun acc c ->
              try
                let t = c |> member "text" |> to_string in
                if acc = "" then t else acc ^ t
              with _ -> acc)
            "" content_list
        with _ -> (
          try message |> member "content" |> to_string with _ -> "")
      in
      Ok
        (Provider.Text
           {
             content;
             model = resp_model;
             usage;
             provider_response_items_json = None;
             thinking = None;
           })
  with exn ->
    Error ("Failed to parse Cohere response: " ^ Printexc.to_string exn)

let complete ~(config : Runtime_config.t)
    ~(provider : Runtime_config.provider_config) ~model ~messages ?tools
    ?session_key:_ () =
  let open Lwt.Syntax in
  let base_url =
    match provider.base_url with Some url -> url | None -> cohere_base
  in
  let uri = base_url ^ "/v2/chat" in
  let cohere_messages = messages_to_cohere_json messages in
  let body_fields =
    [
      ("model", `String model);
      ("messages", `List cohere_messages);
      ("temperature", `Float (max 1e-8 config.default_temperature));
    ]
  in
  let body_fields =
    match tools_to_cohere_json tools with
    | Some t -> body_fields @ [ ("tools", t) ]
    | None -> body_fields
  in
  let body = `Assoc body_fields |> Yojson.Safe.to_string in
  let headers = [ ("Authorization", "Bearer " ^ provider.api_key) ] in
  Logs.info (fun m ->
      m "Cohere request to %s model=%s msgs=%d" uri model (List.length messages));
  let* status, response_body = Http_client.post_json ~uri ~headers ~body in
  if status < 200 || status >= 300 then
    Lwt.fail_with
      (Printf.sprintf "Cohere API error (HTTP %d): %s" status response_body)
  else
    match parse_cohere_response response_body model with
    | Ok resp -> Lwt.return resp
    | Error msg -> Lwt.fail_with msg

let complete_streaming ~(config : Runtime_config.t)
    ~(provider : Runtime_config.provider_config) ~model ~messages ?tools
    ?session_key:_ ~on_chunk () =
  let open Lwt.Syntax in
  let base_url =
    match provider.base_url with Some url -> url | None -> cohere_base
  in
  let uri = base_url ^ "/v2/chat" in
  let cohere_messages = messages_to_cohere_json messages in
  let body_fields =
    [
      ("model", `String model);
      ("messages", `List cohere_messages);
      ("temperature", `Float (max 1e-8 config.default_temperature));
      ("stream", `Bool true);
    ]
  in
  let body_fields =
    match tools_to_cohere_json tools with
    | Some t -> body_fields @ [ ("tools", t) ]
    | None -> body_fields
  in
  let body = `Assoc body_fields |> Yojson.Safe.to_string in
  let headers = [ ("Authorization", "Bearer " ^ provider.api_key) ] in
  Logs.info (fun m ->
      m "Cohere stream request to %s model=%s msgs=%d" uri model
        (List.length messages));
  Http_client.post_stream_with ~uri ~headers ~body ~label:"Cohere API error"
    ~on_ok:(fun stream ->
      let buf = Buffer.create 256 in
      let content_acc = Buffer.create 1024 in
      let resp_model = ref model in
      let usage_acc = ref None in
      let tool_calls_acc : Provider.tool_call list ref = ref [] in
      let current_event = ref "" in
      let process_event event_type data_str =
        try
          let json = Yojson.Safe.from_string data_str in
          let open Yojson.Safe.Util in
          match event_type with
          | "content-delta" -> (
              try
                let text =
                  json |> member "delta" |> member "message" |> member "content"
                  |> member "text" |> to_string
                in
                if text <> "" then begin
                  Buffer.add_string content_acc text;
                  on_chunk (Provider.Delta text)
                end
                else Lwt.return_unit
              with _ -> Lwt.return_unit)
          | "tool-call-delta" ->
              (* Incremental tool call deltas; full tool calls arrive in
               tool-calls-chunk so we only need to acknowledge this event. *)
              Lwt.return_unit
          | "tool-calls-chunk" ->
              (try
                 let tc_list = json |> member "tool_calls" |> to_list in
                 List.iter
                   (fun tc ->
                     try
                       let id = tc |> member "id" |> to_string in
                       let fn = tc |> member "function" in
                       let function_name = fn |> member "name" |> to_string in
                       let arguments = fn |> member "arguments" |> to_string in
                       tool_calls_acc :=
                         !tool_calls_acc
                         @ [ { Provider.id; function_name; arguments } ]
                     with _ -> ())
                   tc_list
               with _ -> ());
              Lwt.return_unit
          | "message-end" ->
              (try
                 let u =
                   json |> member "delta" |> member "usage" |> member "tokens"
                 in
                 let pt = u |> member "input_tokens" |> to_int in
                 let ct = u |> member "output_tokens" |> to_int in
                 usage_acc := Some (pt, ct, 0)
               with _ -> ());
              on_chunk Provider.Done
          | _ -> Lwt.return_unit
        with _ -> Lwt.return_unit
      in
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
      Lwt.return
        (Provider.make_stream_result ~tool_calls:!tool_calls_acc ~content
           ~model:final_model ~usage:!usage_acc ()))
    ()
