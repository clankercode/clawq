(* Provider implementation for Google Gemini GenerateContent API *)

let gemini_base = "https://generativelanguage.googleapis.com/v1beta"

let messages_to_gemini_contents messages =
  (* Gemini uses "contents" with role "user"/"model" and parts array.
     System messages are handled separately via systemInstruction.
     Tool results use role "user" with functionResponse parts.
     Tool calls in assistant messages use functionCall parts. *)
  List.filter_map
    (fun (m : Provider.message) ->
      match m.role with
      | "system" | "developer" -> None
      | "tool" ->
          let sc = Provider.sanitize_utf8 m.content in
          let response_data =
            try Yojson.Safe.from_string sc with _ -> `String sc
          in
          let fn_name = match m.name with Some n -> n | None -> "unknown" in
          Some
            (`Assoc
               [
                 ("role", `String "user");
                 ( "parts",
                   `List
                     [
                       `Assoc
                         [
                           ( "functionResponse",
                             `Assoc
                               [
                                 ("name", `String fn_name);
                                 ( "response",
                                   `Assoc [ ("content", response_data) ] );
                               ] );
                         ];
                     ] );
               ])
      | "assistant" when m.Provider.tool_calls <> [] ->
          let parts =
            List.map
              (fun (tc : Provider.tool_call) ->
                let args =
                  try Yojson.Safe.from_string tc.arguments with _ -> `Assoc []
                in
                `Assoc
                  [
                    ( "functionCall",
                      `Assoc
                        [ ("name", `String tc.function_name); ("args", args) ]
                    );
                  ])
              m.Provider.tool_calls
          in
          Some (`Assoc [ ("role", `String "model"); ("parts", `List parts) ])
      | "assistant" ->
          let sc = Provider.sanitize_utf8 m.content in
          Some
            (`Assoc
               [
                 ("role", `String "model");
                 ("parts", `List [ `Assoc [ ("text", `String sc) ] ]);
               ])
      | _ ->
          let sc = Provider.sanitize_utf8 m.content in
          let parts =
            match m.Provider.content_parts with
            | [] -> `List [ `Assoc [ ("text", `String sc) ] ]
            | cparts ->
                `List
                  (List.map
                     (fun (part : Provider.content_part) ->
                       match part with
                       | Provider.Text s ->
                           `Assoc
                             [ ("text", `String (Provider.sanitize_utf8 s)) ]
                       | Provider.Image_base64 { data; media_type } ->
                           `Assoc
                             [
                               ( "inlineData",
                                 `Assoc
                                   [
                                     ("mimeType", `String media_type);
                                     ("data", `String data);
                                   ] );
                             ])
                     cparts)
          in
          Some (`Assoc [ ("role", `String "user"); ("parts", parts) ]))
    messages

let extract_system_prompt = Provider.extract_system_prompt

let tools_to_gemini_json tools =
  match tools with
  | None -> None
  | Some (`List ts) when ts <> [] ->
      let function_declarations =
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
                     ("parameters", parameters);
                   ])
            with _ -> None)
          ts
      in
      if function_declarations = [] then None
      else
        Some
          (`List
             [
               `Assoc [ ("function_declarations", `List function_declarations) ];
             ])
  | _ -> None

let parse_gemini_response body model =
  try
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    let candidate =
      try json |> member "candidates" |> index 0 with _ -> `Null
    in
    let resp_model =
      try json |> member "modelVersion" |> to_string with _ -> model
    in
    let usage =
      try
        let u = json |> member "usageMetadata" in
        let pt = u |> member "promptTokenCount" |> to_int in
        let ct = u |> member "candidatesTokenCount" |> to_int in
        let cached =
          try u |> member "cachedContentTokenCount" |> to_int with _ -> 0
        in
        Some (pt, ct, cached)
      with _ -> None
    in
    let parts =
      try candidate |> member "content" |> member "parts" |> to_list
      with _ -> []
    in
    (* Check for function calls *)
    let _tc_counter = ref 0 in
    let tool_calls =
      List.filter_map
        (fun part ->
          try
            let fc = part |> member "functionCall" in
            let name = fc |> member "name" |> to_string in
            let args = fc |> member "args" in
            let arguments = Yojson.Safe.to_string args in
            let idx = !_tc_counter in
            incr _tc_counter;
            let id = Printf.sprintf "gemini_%s_%d" name idx in
            Some { Provider.id; function_name = name; arguments }
          with _ -> None)
        parts
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
      let text =
        List.fold_left
          (fun acc part ->
            try
              let t = part |> member "text" |> to_string in
              if acc = "" then t else acc ^ t
            with _ -> acc)
          "" parts
      in
      Ok
        (Provider.Text
           {
             content = text;
             model = resp_model;
             usage;
             provider_response_items_json = None;
             thinking = None;
           })
  with exn ->
    Error ("Failed to parse Gemini response: " ^ Printexc.to_string exn)

let make_request_body ~config ~messages ~tools =
  let contents = messages_to_gemini_contents messages in
  let system_prompt = extract_system_prompt messages in
  let body_fields =
    [
      ("contents", `List contents);
      ( "generationConfig",
        `Assoc
          [
            ( "temperature",
              `Float (max 1e-8 config.Runtime_config.default_temperature) );
            ("maxOutputTokens", `Int 8192);
          ] );
    ]
  in
  let body_fields =
    if system_prompt <> "" then
      body_fields
      @ [
          ( "systemInstruction",
            `Assoc
              [
                ("parts", `List [ `Assoc [ ("text", `String system_prompt) ] ]);
              ] );
        ]
    else body_fields
  in
  let body_fields =
    match tools_to_gemini_json tools with
    | Some t -> body_fields @ [ ("tools", t) ]
    | None -> body_fields
  in
  `Assoc body_fields |> Yojson.Safe.to_string

let complete ~(config : Runtime_config.t)
    ~(provider : Runtime_config.provider_config) ~model ~messages ?tools
    ?session_key:_ () =
  let open Lwt.Syntax in
  let api_key = provider.api_key in
  let base_url =
    match provider.base_url with Some url -> url | None -> gemini_base
  in
  let uri =
    Printf.sprintf "%s/models/%s:generateContent?key=%s" base_url model api_key
  in
  let body = make_request_body ~config ~messages ~tools in
  let headers = [] in
  Logs.info (fun m ->
      m "Gemini request model=%s msgs=%d" model (List.length messages));
  let timeout_s = Option.value ~default:180.0 provider.http_timeout_s in
  let* status, response_body =
    Http_client.post_json_with_timeout ~timeout_s ~uri ~headers ~body
  in
  if status < 200 || status >= 300 then
    Lwt.fail_with
      (Printf.sprintf "Gemini API error (HTTP %d): %s" status response_body)
  else
    match parse_gemini_response response_body model with
    | Ok resp -> Lwt.return resp
    | Error msg -> Lwt.fail_with msg

let complete_streaming ~(config : Runtime_config.t)
    ~(provider : Runtime_config.provider_config) ~model ~messages ?tools
    ?session_key:_ ~on_chunk () =
  let open Lwt.Syntax in
  let api_key = provider.api_key in
  let base_url =
    match provider.base_url with Some url -> url | None -> gemini_base
  in
  let uri =
    Printf.sprintf "%s/models/%s:streamGenerateContent?key=%s&alt=sse" base_url
      model api_key
  in
  let body = make_request_body ~config ~messages ~tools in
  let headers = [] in
  Logs.info (fun m ->
      m "Gemini stream request model=%s msgs=%d" model (List.length messages));
  Http_client.post_stream_with ?stream_idle_timeout_s:provider.http_timeout_s
    ~uri ~headers ~body ~label:"Gemini API error"
    ~on_ok:(fun stream ->
      let buf = Buffer.create 256 in
      let content_acc = Buffer.create 1024 in
      let resp_model = ref model in
      let usage_acc = ref None in
      let tool_calls_acc : Provider.tool_call list ref = ref [] in
      let tc_counter = ref 0 in
      let process_line line =
        let prefix = "data: " in
        let plen = String.length prefix in
        if String.length line >= plen && String.sub line 0 plen = prefix then begin
          let data = String.sub line plen (String.length line - plen) in
          if data = "[DONE]" then begin
            let* () = on_chunk Provider.Done in
            Lwt.return_unit
          end
          else
            try
              let json = Yojson.Safe.from_string data in
              let open Yojson.Safe.Util in
              (try resp_model := json |> member "modelVersion" |> to_string
               with _ -> ());
              (try
                 let u = json |> member "usageMetadata" in
                 let pt = u |> member "promptTokenCount" |> to_int in
                 let ct = u |> member "candidatesTokenCount" |> to_int in
                 let cached =
                   try u |> member "cachedContentTokenCount" |> to_int
                   with _ -> 0
                 in
                 usage_acc := Some (pt, ct, cached)
               with _ -> ());
              let parts =
                try
                  json |> member "candidates" |> index 0 |> member "content"
                  |> member "parts" |> to_list
                with _ -> []
              in
              Lwt_list.iter_s
                (fun part ->
                  let* () =
                    try
                      let text = part |> member "text" |> to_string in
                      if text <> "" then begin
                        Buffer.add_string content_acc text;
                        on_chunk (Provider.Delta text)
                      end
                      else Lwt.return_unit
                    with _ -> Lwt.return_unit
                  in
                  (try
                     let fc = part |> member "functionCall" in
                     let name = fc |> member "name" |> to_string in
                     let args = fc |> member "args" in
                     let arguments = Yojson.Safe.to_string args in
                     let idx = !tc_counter in
                     incr tc_counter;
                     let id = Printf.sprintf "gemini_%s_%d" name idx in
                     tool_calls_acc :=
                       !tool_calls_acc
                       @ [ { Provider.id; function_name = name; arguments } ]
                   with _ -> ());
                  Lwt.return_unit)
                parts
            with _ -> Lwt.return_unit
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
