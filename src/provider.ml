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
  {
    role = "tool";
    content;
    tool_calls = [];
    tool_call_id = Some tool_call_id;
    name = Some name;
  }

let message_to_json m =
  let fields = [ ("role", `String m.role) ] in
  let fields =
    match m.role with
    | "tool" -> (
        let fields = fields @ [ ("content", `String m.content) ] in
        let fields =
          match m.tool_call_id with
          | Some id -> fields @ [ ("tool_call_id", `String id) ]
          | None -> fields
        in
        match m.name with
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

let default_base_url_for name =
  match name with
  | "zai_coding" -> "https://api.z.ai/api/coding/paas/v4"
  | "zai" -> "https://api.z.ai/api/paas/v4"
  | _ -> "https://openrouter.ai/api/v1"

let strip_date_suffix s =
  let len = String.length s in
  if len >= 9 && s.[len - 9] = '-' then
    let suffix = String.sub s (len - 8) 8 in
    let all_digits =
      try
        String.iter (fun c -> if c < '0' || c > '9' then raise Exit) suffix;
        true
      with Exit -> false
    in
    if all_digits then String.sub s 0 (len - 9) else s
  else s

let normalize_model_name s =
  String.lowercase_ascii (strip_date_suffix (String.trim s))

let find_provider_for_model ~providers ~model_name =
  let norm = normalize_model_name model_name in
  let match_provider (name, (p : Runtime_config.provider_config)) =
    let norm_name = String.lowercase_ascii name in
    if
      String.length norm >= String.length norm_name
      && String.sub norm 0 (String.length norm_name) = norm_name
      && Runtime_config.is_key_set p.api_key
    then Some (name, p)
    else
      match p.default_model with
      | Some dm ->
          let norm_dm = normalize_model_name dm in
          if norm = norm_dm && Runtime_config.is_key_set p.api_key then
            Some (name, p)
          else None
      | None -> None
  in
  List.find_map match_provider providers

let select_provider ~(config : Runtime_config.t) =
  let find_named name =
    List.find_opt (fun (n, _) -> n = name) config.providers
  in
  let with_key =
    List.filter
      (fun (_, p) -> Runtime_config.is_key_set p.Runtime_config.api_key)
      config.providers
  in
  let model_target =
    Runtime_config.effective_primary_target config.agent_defaults
  in
  let raw_model = String.trim config.agent_defaults.primary_model in
  let model_provider_preferred =
    match model_target.provider with
    | Some name -> (
        match find_named name with
        | Some (n, p) when Runtime_config.is_key_set p.api_key -> Some (n, p)
        | _ -> None)
    | None -> None
  in
  let config_provider_preferred =
    match config.default_provider with
    | Some name -> (
        match find_named name with
        | Some (n, p) when Runtime_config.is_key_set p.api_key -> Some (n, p)
        | _ -> None)
    | None -> None
  in
  let model_routed =
    match model_target.provider with
    | None ->
        find_provider_for_model ~providers:config.providers
          ~model_name:model_target.model
    | Some _ -> None
  in
  let chosen =
    match model_provider_preferred with
    | Some pair -> pair
    | None -> (
        match model_routed with
        | Some pair -> pair
        | None -> (
            match config_provider_preferred with
            | Some pair -> pair
            | None -> (
                match with_key with
                | (name, p) :: _ -> (name, p)
                | [] -> (
                    match config.providers with
                    | (name, p) :: _ -> (name, p)
                    | [] ->
                        ( "default",
                          {
                            Runtime_config.api_key = "";
                            base_url = None;
                            default_model = None;
                          } )))))
  in
  let provider_name, provider = chosen in
  let model =
    match model_target.provider with
    | Some requested when requested = provider_name -> model_target.model
    | Some _ -> raw_model
    | _ -> (
        match provider.default_model with Some m -> m | None -> raw_model)
  in
  (provider_name, provider, model)

let complete ~(config : Runtime_config.t) ~messages ?tools () =
  let open Lwt.Syntax in
  let provider_name, provider, model = select_provider ~config in
  let base_url =
    match provider.base_url with
    | Some url -> url
    | None -> default_base_url_for provider_name
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
  let headers = [ ("Authorization", "Bearer " ^ provider.api_key) ] in
  Logs.info (fun m ->
      m "LLM request to %s provider=%s model=%s msgs=%d" uri provider_name model
        (List.length messages));
  let* status, response_body = Http_client.post_json ~uri ~headers ~body in
  if status < 200 || status >= 300 then begin
    if status = 400 then
      try
        let err_json = Yojson.Safe.from_string response_body in
        let open Yojson.Safe.Util in
        let failed_gen =
          try
            err_json |> member "error" |> member "failed_generation"
            |> to_string
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
  end
  else
    let json =
      try Ok (Yojson.Safe.from_string response_body)
      with exn -> Error (Printexc.to_string exn)
    in
    match json with
    | Error msg -> Lwt.fail_with ("Failed to parse LLM response JSON: " ^ msg)
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
            List.mapi
              (fun i tc ->
                try
                  let id = tc |> member "id" |> to_string in
                  let fn = tc |> member "function" in
                  let function_name = fn |> member "name" |> to_string in
                  let arguments = fn |> member "arguments" |> to_string in
                  Some { id; function_name; arguments }
                with _ ->
                  Logs.warn (fun m ->
                      m "LLM response dropped malformed tool_call at index=%d" i);
                  None)
              tool_calls_json
            |> List.filter_map (fun x -> x)
          in
          Lwt.return (ToolCalls { calls; model = resp_model; usage })
        else
          let content =
            try choice |> member "content" |> to_string with _ -> ""
          in
          if content = "" then
            Lwt.fail_with "Failed to extract content from LLM response"
          else Lwt.return (Text { content; model = resp_model; usage })

type stream_event =
  | Delta of string
  | ToolCallDelta of {
      index : int;
      id : string option;
      function_name : string option;
      arguments : string option;
    }
  | Done

let parse_sse_line line =
  let prefix = "data: " in
  let plen = String.length prefix in
  if String.length line >= plen && String.sub line 0 plen = prefix then
    let data = String.sub line plen (String.length line - plen) in
    if data = "[DONE]" then Some `Done
    else try Some (`Json (Yojson.Safe.from_string data)) with _ -> None
  else None

let process_sse_stream stream ~on_chunk =
  let open Lwt.Syntax in
  let buf = Buffer.create 256 in
  let content_acc = Buffer.create 1024 in
  let tool_calls_acc : (int * string * string * Buffer.t) list ref = ref [] in
  let resp_model = ref "" in
  let usage_acc = ref None in
  let process_line line =
    match parse_sse_line line with
    | Some `Done -> on_chunk Done
    | Some (`Json json) -> (
        let open Yojson.Safe.Util in
        (try resp_model := json |> member "model" |> to_string with _ -> ());
        (try
           let u = json |> member "usage" in
           let pt = u |> member "prompt_tokens" |> to_int in
           let ct = u |> member "completion_tokens" |> to_int in
           usage_acc := Some (pt, ct)
         with _ -> ());
        let delta =
          try json |> member "choices" |> index 0 |> member "delta"
          with _ -> `Null
        in
        let content_delta =
          try Some (delta |> member "content" |> to_string) with _ -> None
        in
        match content_delta with
        | Some c when c <> "" ->
            Buffer.add_string content_acc c;
            on_chunk (Delta c)
        | _ ->
            let tc_deltas =
              try delta |> member "tool_calls" |> to_list with _ -> []
            in
            if tc_deltas <> [] then begin
              let* () =
                Lwt_list.iter_s
                  (fun tc ->
                    let idx =
                      try tc |> member "index" |> to_int with _ -> 0
                    in
                    let id =
                      try Some (tc |> member "id" |> to_string) with _ -> None
                    in
                    let fn_name =
                      try
                        Some
                          (tc |> member "function" |> member "name" |> to_string)
                      with _ -> None
                    in
                    let fn_args =
                      try
                        Some
                          (tc |> member "function" |> member "arguments"
                         |> to_string)
                      with _ -> None
                    in
                    (* accumulate tool call data *)
                    let existing =
                      List.find_opt
                        (fun (i, _, _, _) -> i = idx)
                        !tool_calls_acc
                    in
                    (match existing with
                    | None ->
                        let args_buf = Buffer.create 256 in
                        (match fn_args with
                        | Some a -> Buffer.add_string args_buf a
                        | None -> ());
                        let tc_id = match id with Some i -> i | None -> "" in
                        let tc_name =
                          match fn_name with Some n -> n | None -> ""
                        in
                        tool_calls_acc :=
                          !tool_calls_acc @ [ (idx, tc_id, tc_name, args_buf) ]
                    | Some (_, _, _, args_buf) -> (
                        match fn_args with
                        | Some a -> Buffer.add_string args_buf a
                        | None -> ()));
                    on_chunk
                      (ToolCallDelta
                         {
                           index = idx;
                           id;
                           function_name = fn_name;
                           arguments = fn_args;
                         }))
                  tc_deltas
              in
              Lwt.return_unit
            end
            else Lwt.return_unit)
    | None -> Lwt.return_unit
  in
  let process_buffer () =
    let s = Buffer.contents buf in
    Buffer.clear buf;
    let lines = String.split_on_char '\n' s in
    let rec process_lines = function
      | [] -> Lwt.return_unit
      | [ last ] ->
          (* last element may be incomplete - put back in buffer *)
          Buffer.add_string buf last;
          Lwt.return_unit
      | line :: rest ->
          let line =
            if String.length line > 0 && line.[String.length line - 1] = '\r'
            then String.sub line 0 (String.length line - 1)
            else line
          in
          let* () = if line <> "" then process_line line else Lwt.return_unit in
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
  (* process any remaining data in buffer *)
  let remaining = Buffer.contents buf in
  let* () =
    if remaining <> "" then process_line remaining else Lwt.return_unit
  in
  let content = Buffer.contents content_acc in
  let model = if !resp_model <> "" then !resp_model else "unknown" in
  let tool_calls =
    List.map
      (fun (_, id, name, args_buf) ->
        { id; function_name = name; arguments = Buffer.contents args_buf })
      !tool_calls_acc
  in
  if tool_calls <> [] then
    Lwt.return (ToolCalls { calls = tool_calls; model; usage = !usage_acc })
  else Lwt.return (Text { content; model; usage = !usage_acc })

let complete_stream ~(config : Runtime_config.t) ~messages ?tools ~on_chunk () =
  let open Lwt.Syntax in
  let provider_name, provider, model = select_provider ~config in
  let base_url =
    match provider.base_url with
    | Some url -> url
    | None -> default_base_url_for provider_name
  in
  let uri = base_url ^ "/chat/completions" in
  let body_fields =
    [
      ("model", `String model);
      ("messages", messages_to_json messages);
      ("temperature", `Float (max 1e-8 config.default_temperature));
      ("stream", `Bool true);
    ]
  in
  let body_fields =
    match tools with
    | Some t when t <> `List [] -> body_fields @ [ ("tools", t) ]
    | _ -> body_fields
  in
  let body = `Assoc body_fields |> Yojson.Safe.to_string in
  let headers = [ ("Authorization", "Bearer " ^ provider.api_key) ] in
  Logs.info (fun m ->
      m "LLM stream request to %s provider=%s model=%s msgs=%d" uri
        provider_name model (List.length messages));
  let* status, stream = Http_client.post_stream ~uri ~headers ~body in
  if status < 200 || status >= 300 then begin
    (* collect error body from stream *)
    let* chunks = Lwt_stream.to_list stream in
    let response_body = String.concat "" chunks in
    Lwt.fail_with
      (Printf.sprintf "LLM API error (HTTP %d): %s" status response_body)
  end
  else process_sse_stream stream ~on_chunk
