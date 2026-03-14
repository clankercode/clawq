let responses_uri = Openai_codex_oauth.codex_base_url ^ "/responses"

let string_contains s sub =
  let ls = String.length s and lsub = String.length sub in
  if lsub = 0 then true
  else if ls < lsub then false
  else
    let rec loop i =
      if i > ls - lsub then false
      else if String.sub s i lsub = sub then true
      else loop (i + 1)
    in
    loop 0

let strip_provider_prefix model =
  Runtime_config.strip_model_provider_prefix model

let sanitize_content_part part =
  match part with
  | `Assoc fields ->
      let kept =
        List.filter
          (fun (k, _) -> match k with "type" | "text" -> true | _ -> false)
          fields
      in
      `Assoc kept
  | other -> other

let sanitize_input_item item =
  match item with
  | `Assoc fields -> (
      let item_type =
        match List.assoc_opt "type" fields with
        | Some (`String t) -> t
        | _ -> ""
      in
      match item_type with
      | "function_call" ->
          (* Keep only fields accepted by the Responses API input *)
          let kept =
            List.filter
              (fun (k, _) ->
                match k with
                | "type" | "call_id" | "name" | "arguments" -> true
                | _ -> false)
              fields
          in
          Some (`Assoc kept)
      | "message" ->
          (* Rebuild as a clean assistant message *)
          let role =
            match List.assoc_opt "role" fields with
            | Some (`String r) -> r
            | _ -> "assistant"
          in
          let content =
            match List.assoc_opt "content" fields with
            | Some (`List parts) -> `List (List.map sanitize_content_part parts)
            | Some other -> other
            | None -> `List []
          in
          Some (`Assoc [ ("role", `String role); ("content", content) ])
      | "reasoning" ->
          (* Output-only item; requires store=true to reference by ID.
             Drop it entirely — cannot be replayed as inline input. *)
          None
      | _ ->
          (* Strip server-assigned id from unknown output-only types to avoid
             HTTP 404 when store=false and the server no longer holds the item. *)
          let kept = List.filter (fun (k, _) -> k <> "id") fields in
          Some (`Assoc kept))
  | other -> Some other

let restore_provider_response_items msg =
  match msg.Provider.provider_response_items_json with
  | Some raw -> (
      try
        match Yojson.Safe.from_string raw with
        | `List items ->
            Some (`List (List.filter_map sanitize_input_item items))
        | item -> sanitize_input_item item
      with _ -> None)
  | None -> None

let message_to_input (msg : Provider.message) =
  let sc = Provider.sanitize_utf8 msg.content in
  match msg.role with
  | "user" ->
      Some
        (`Assoc
           [
             ("role", `String "user");
             ( "content",
               `List
                 [
                   `Assoc
                     [ ("type", `String "input_text"); ("text", `String sc) ];
                 ] );
           ])
  | "assistant" -> (
      let fallback () =
        let entries = ref [] in
        if msg.content <> "" then
          entries :=
            !entries
            @ [
                `Assoc
                  [
                    ("role", `String "assistant");
                    ( "content",
                      `List
                        [
                          `Assoc
                            [
                              ("type", `String "output_text");
                              ("text", `String sc);
                            ];
                        ] );
                  ];
              ];
        List.iter
          (fun (tc : Provider.tool_call) ->
            entries :=
              !entries
              @ [
                  `Assoc
                    [
                      ("type", `String "function_call");
                      ("call_id", `String tc.id);
                      ("name", `String tc.function_name);
                      ( "arguments",
                        `String (Provider.sanitize_utf8 tc.arguments) );
                    ];
                ])
          msg.tool_calls;
        Some (`List !entries)
      in
      match restore_provider_response_items msg with
      | Some (`List _ as items) -> Some items
      | Some item -> Some item
      | None -> fallback ())
  | "tool" -> (
      match msg.tool_call_id with
      | Some tool_call_id ->
          Some
            (`Assoc
               [
                 ("type", `String "function_call_output");
                 ("call_id", `String tool_call_id);
                 ("output", `String sc);
               ])
      | None -> None)
  | _ -> None

let messages_to_input messages =
  List.fold_left
    (fun acc msg ->
      match message_to_input msg with
      | Some (`List items) -> acc @ items
      | Some item -> acc @ [ item ]
      | None -> acc)
    [] messages

let tools_to_responses_tools = function
  | None -> `List []
  | Some (`List tools) ->
      let mapped =
        List.filter_map
          (function
            | `Assoc fields -> (
                match List.assoc_opt "function" fields with
                | Some (`Assoc fn_fields) ->
                    let required name =
                      match List.assoc_opt name fn_fields with
                      | Some v -> v
                      | None -> `Null
                    in
                    Some
                      (`Assoc
                         [
                           ("type", `String "function");
                           ("name", required "name");
                           ("description", required "description");
                           ("parameters", required "parameters");
                         ])
                | _ -> None)
            | _ -> None)
          tools
      in
      `List mapped
  | Some _ -> `List []

let collect_call_ids items =
  List.filter_map
    (fun item ->
      match item with
      | `Assoc fields
        when match List.assoc_opt "type" fields with
             | Some (`String "function_call") -> true
             | _ -> false -> (
          match List.assoc_opt "call_id" fields with
          | Some (`String id) -> Some id
          | _ -> (
              match List.assoc_opt "id" fields with
              | Some (`String id) -> Some id
              | _ -> None))
      | _ -> None)
    items

let collect_output_ids items =
  List.filter_map
    (fun item ->
      match item with
      | `Assoc fields
        when match List.assoc_opt "type" fields with
             | Some (`String "function_call_output") -> true
             | _ -> false -> (
          match List.assoc_opt "call_id" fields with
          | Some (`String id) -> Some id
          | _ -> None)
      | _ -> None)
    items

let validate_codex_input_items items =
  let call_ids = collect_call_ids items in
  let output_ids = collect_output_ids items in
  List.filter
    (fun item ->
      match item with
      | `Assoc fields -> (
          match List.assoc_opt "type" fields with
          | Some (`String "function_call") ->
              let id =
                Option.value ~default:""
                  (match List.assoc_opt "call_id" fields with
                  | Some (`String id) -> Some id
                  | _ -> (
                      match List.assoc_opt "id" fields with
                      | Some (`String id) -> Some id
                      | _ -> None))
              in
              if not (List.mem id output_ids) then begin
                Logs.warn (fun m ->
                    m
                      "Codex: dropping orphaned function_call call_id=%s (no \
                       output)"
                      id);
                false
              end
              else true
          | Some (`String "function_call_output") ->
              let id =
                Option.value ~default:""
                  (match List.assoc_opt "call_id" fields with
                  | Some (`String id) -> Some id
                  | _ -> None)
              in
              if not (List.mem id call_ids) then begin
                Logs.warn (fun m ->
                    m
                      "Codex: dropping orphaned function_call_output \
                       call_id=%s (no call)"
                      id);
                false
              end
              else true
          | _ -> true)
      | _ -> true)
    items

let extract_instructions messages =
  let system_parts =
    List.filter_map
      (fun (msg : Provider.message) ->
        if msg.role = "system" && msg.content <> "" then Some msg.content
        else None)
      messages
  in
  let non_system =
    List.filter (fun (msg : Provider.message) -> msg.role <> "system") messages
  in
  let instructions = String.concat "\n\n" system_parts in
  (instructions, non_system)

let item_type_str item =
  match item with
  | `Assoc fields -> (
      match List.assoc_opt "type" fields with
      | Some (`String t) -> t
      | _ -> (
          match List.assoc_opt "role" fields with
          | Some (`String r) -> r
          | _ -> "unknown"))
  | _ -> "unknown"

let is_user_item item =
  match item with
  | `Assoc fields -> (
      match List.assoc_opt "role" fields with
      | Some (`String "user") -> true
      | _ -> false)
  | _ -> false

let is_assistant_item item =
  match item with
  | `Assoc fields -> (
      match List.assoc_opt "role" fields with
      | Some (`String "assistant") -> true
      | _ -> (
          match List.assoc_opt "type" fields with
          | Some (`String "function_call") -> true
          | _ -> false))
  | _ -> false

let synthetic_assistant_msg text =
  `Assoc
    [
      ("role", `String "assistant");
      ( "content",
        `List
          [ `Assoc [ ("type", `String "output_text"); ("text", `String text) ] ]
      );
    ]

(* Insert synthetic assistant turns where the Responses API requires them:
   - After function_call_output items before user messages
   - Before the first item if it is not a user message *)
let fixup_input_item_ordering items =
  let rec loop acc prev_type = function
    | [] -> List.rev acc
    | item :: rest ->
        let cur_type = item_type_str item in
        let needs_synthetic =
          prev_type = "function_call_output" && is_user_item item
        in
        if needs_synthetic then begin
          Logs.debug (fun m ->
              m
                "Codex: inserting synthetic assistant turn between \
                 function_call_output and user message");
          loop
            (item :: synthetic_assistant_msg "(continued)" :: acc)
            cur_type rest
        end
        else loop (item :: acc) cur_type rest
  in
  let items =
    match items with
    | first :: _
      when (not (is_user_item first)) && not (is_assistant_item first) ->
        (* Unusual: first item is not user or assistant *)
        items
    | first :: rest when not (is_user_item first) ->
        (* First item is assistant — prepend a synthetic user greeting so the
           API sees a user turn first *)
        Logs.debug (fun m ->
            m
              "Codex: prepending synthetic user turn before initial assistant \
               item");
        `Assoc
          [
            ("role", `String "user");
            ( "content",
              `List
                [
                  `Assoc
                    [
                      ("type", `String "input_text");
                      ("text", `String "(session resumed)");
                    ];
                ] );
          ]
        :: first :: rest
    | _ -> items
  in
  loop [] "" items

let build_body ~model ~messages tools =
  let instructions, non_system_messages = extract_instructions messages in
  let non_system_messages =
    Message_history.ensure_tool_group_integrity non_system_messages
  in
  let input_items =
    validate_codex_input_items (messages_to_input non_system_messages)
  in
  let input_items = fixup_input_item_ordering input_items in
  let input_items =
    if input_items = [] then begin
      Logs.warn (fun m ->
          m "Codex: no input items after serialization; inserting placeholder");
      [
        `Assoc
          [
            ("role", `String "user");
            ( "content",
              `List
                [
                  `Assoc
                    [
                      ("type", `String "input_text");
                      ("text", `String "(no history)");
                    ];
                ] );
          ];
      ]
    end
    else input_items
  in
  `Assoc
    ([
       ("model", `String (strip_provider_prefix model));
       ("input", `List input_items);
       ("instructions", `String instructions);
       ("stream", `Bool true);
       ("store", `Bool false);
       ("parallel_tool_calls", `Bool true);
     ]
    @
    match tools_to_responses_tools tools with
    | `List [] -> []
    | mapped -> [ ("tools", mapped) ])
  |> Yojson.Safe.to_string

let append_unique_tool_call acc call =
  if List.exists (fun existing -> existing.Provider.id = call.Provider.id) acc
  then acc
  else acc @ [ call ]

let usage_of_json json =
  let open Yojson.Safe.Util in
  try
    let pt = json |> member "input_tokens" |> to_int in
    let ct = json |> member "output_tokens" |> to_int in
    Some (pt, ct)
  with _ -> None

let extract_final_output response_json =
  let open Yojson.Safe.Util in
  let output = try response_json |> member "output" |> to_list with _ -> [] in
  List.fold_left
    (fun (text_acc, tool_acc) item ->
      let item_type = try item |> member "type" |> to_string with _ -> "" in
      if item_type = "message" then
        let content = try item |> member "content" |> to_list with _ -> [] in
        let text =
          List.fold_left
            (fun acc part ->
              let part_type =
                try part |> member "type" |> to_string with _ -> ""
              in
              if (part_type = "output_text" || part_type = "text") && acc = ""
              then try part |> member "text" |> to_string with _ -> acc
              else acc)
            text_acc content
        in
        (text, tool_acc)
      else if item_type = "function_call" || item_type = "tool_call" then
        let call =
          {
            Provider.id =
              (try item |> member "call_id" |> to_string
               with _ -> ( try item |> member "id" |> to_string with _ -> ""));
            function_name =
              (try item |> member "name" |> to_string with _ -> "");
            arguments =
              (try item |> member "arguments" |> to_string with _ -> "");
          }
        in
        (text_acc, append_unique_tool_call tool_acc call)
      else (text_acc, tool_acc))
    ("", []) output

let provider_response_items_json response_json =
  let open Yojson.Safe.Util in
  try Some (Yojson.Safe.to_string (response_json |> member "output"))
  with _ -> None

let process_stream stream ~on_chunk =
  let open Lwt.Syntax in
  let content_acc = Buffer.create 1024 in
  let tool_buffers : (int, string * string * Buffer.t) Hashtbl.t =
    Hashtbl.create 8
  in
  let usage_acc = ref None in
  let model_acc = ref "" in
  let response_items_json_acc = ref None in
  let parse_json_line line =
    let prefix = "data: " in
    if
      String.length line >= String.length prefix
      && String.sub line 0 (String.length prefix) = prefix
    then
      let data =
        String.sub line (String.length prefix)
          (String.length line - String.length prefix)
      in
      if data = "[DONE]" then Some `Done
      else try Some (`Json (Yojson.Safe.from_string data)) with _ -> None
    else None
  in
  let handle_json json =
    let open Yojson.Safe.Util in
    let event_type = try json |> member "type" |> to_string with _ -> "" in
    if
      event_type = "response.output_text.delta"
      || event_type = "response.text.delta"
    then
      let delta = try json |> member "delta" |> to_string with _ -> "" in
      if delta <> "" then begin
        Buffer.add_string content_acc delta;
        on_chunk (Provider.Delta delta)
      end
      else Lwt.return_unit
    else if
      event_type = "response.output_item.added"
      || event_type = "response.output_item.done"
    then
      let item = json |> member "item" in
      let item_type = try item |> member "type" |> to_string with _ -> "" in
      if item_type = "function_call" || item_type = "tool_call" then begin
        let idx =
          try json |> member "output_index" |> to_int
          with _ -> Hashtbl.length tool_buffers
        in
        let call_id =
          try item |> member "call_id" |> to_string
          with _ -> ( try item |> member "id" |> to_string with _ -> "")
        in
        let name = try item |> member "name" |> to_string with _ -> "" in
        let args_buf =
          match Hashtbl.find_opt tool_buffers idx with
          | Some (_, _, buf) -> buf
          | None -> Buffer.create 128
        in
        Hashtbl.replace tool_buffers idx (call_id, name, args_buf);
        Lwt.return_unit
      end
      else Lwt.return_unit
    else if
      event_type = "response.function_call_arguments.delta"
      || event_type = "response.tool_call_arguments.delta"
    then begin
      let idx = try json |> member "output_index" |> to_int with _ -> 0 in
      let delta = try json |> member "delta" |> to_string with _ -> "" in
      let call_id, name, buf =
        match Hashtbl.find_opt tool_buffers idx with
        | Some triple -> triple
        | None ->
            let fresh = ("", "", Buffer.create 128) in
            Hashtbl.add tool_buffers idx fresh;
            fresh
      in
      Buffer.add_string buf delta;
      on_chunk
        (Provider.ToolCallDelta
           {
             index = idx;
             id = (if call_id = "" then None else Some call_id);
             function_name = (if name = "" then None else Some name);
             arguments = (if delta = "" then None else Some delta);
           })
    end
    else if event_type = "response.completed" || event_type = "response.done"
    then begin
      let response_json = json |> member "response" in
      let fallback_text, _fallback_tools = extract_final_output response_json in
      if Buffer.length content_acc = 0 && fallback_text <> "" then
        Buffer.add_string content_acc fallback_text;
      (* Backfill tool entries using output array index, only for missing/empty *)
      let output =
        try response_json |> member "output" |> to_list with _ -> []
      in
      List.iteri
        (fun arr_idx item ->
          let item_type =
            try item |> member "type" |> to_string with _ -> ""
          in
          if item_type = "function_call" || item_type = "tool_call" then
            match Hashtbl.find_opt tool_buffers arr_idx with
            | Some (_, _, buf) when Buffer.length buf > 0 -> ()
            | _ ->
                let call_id =
                  try item |> member "call_id" |> to_string
                  with _ -> (
                    try item |> member "id" |> to_string with _ -> "")
                in
                let name =
                  try item |> member "name" |> to_string with _ -> ""
                in
                let args =
                  try item |> member "arguments" |> to_string with _ -> ""
                in
                let buf = Buffer.create (String.length args) in
                Buffer.add_string buf args;
                Hashtbl.replace tool_buffers arr_idx (call_id, name, buf))
        output;
      let model =
        try response_json |> member "model" |> to_string with _ -> ""
      in
      if model <> "" then model_acc := model;
      usage_acc := usage_of_json (response_json |> member "usage");
      response_items_json_acc := provider_response_items_json response_json;
      on_chunk Provider.Done
    end
    else Lwt.return_unit
  in
  let buffer = Buffer.create 256 in
  let handle_line line =
    match parse_json_line line with
    | Some `Done -> on_chunk Provider.Done
    | Some (`Json json) -> handle_json json
    | None -> Lwt.return_unit
  in
  let* () =
    Lwt_stream.iter_s
      (fun chunk ->
        Buffer.add_string buffer chunk;
        Provider.process_sse_buffer ~buf:buffer ~process_line:handle_line ())
      stream
  in
  let* () =
    let remaining = Buffer.contents buffer in
    if remaining = "" then Lwt.return_unit else handle_line remaining
  in
  let tool_calls =
    Hashtbl.to_seq tool_buffers
    |> List.of_seq
    |> List.sort (fun (a, _) (b, _) -> compare a b)
    |> List.map (fun (_, (id, name, buf)) ->
        { Provider.id; function_name = name; arguments = Buffer.contents buf })
  in
  let model = if !model_acc = "" then "openai-codex" else !model_acc in
  let text = Buffer.contents content_acc in
  Lwt.return
    (Provider.make_stream_result ~tool_calls ~content:text ~model
       ~usage:!usage_acc ~provider_response_items_json:!response_items_json_acc
       ())

let do_request ~provider_name ~provider ~model ~messages ?tools ~on_chunk () =
  let open Lwt.Syntax in
  let* auth =
    Openai_codex_oauth.get_auth_header ~provider_name:(Some provider_name)
      ~provider
  in
  match auth with
  | Error msg -> Lwt.fail_with msg
  | Ok (access_token, account_id) ->
      let headers =
        [
          ("Authorization", "Bearer " ^ access_token);
          ("originator", "clawq");
          ("session_id", Printf.sprintf "%d" (Openai_codex_oauth.now_ms ()));
          ("User-Agent", "clawq/0.1.0-dev");
        ]
        @
        match account_id with
        | Some account_id -> [ ("ChatGPT-Account-Id", account_id) ]
        | None -> []
      in
      let body = build_body ~model ~messages tools in
      Http_client.post_stream_with ~uri:responses_uri ~headers ~body
        ~label:"OpenAI Codex error"
        ~on_error:(fun r ->
          let open Lwt.Syntax in
          let* body = Http_client.collect_error_body r.stream in
          (* Codex returns {"detail":"Bad Request"} (no further detail) for some
             400s including context-window overflows.  Only rewrite to include
             "context length" (which triggers is_context_exhaustion_error recovery
             in agent.ml) when the request is large enough that context exhaustion
             is plausible — otherwise the recovery path fires incorrectly on small
             requests and collapses the history to empty. *)
          let estimated_tokens =
            List.fold_left
              (fun acc (m : Provider.message) ->
                let tc_args =
                  List.fold_left
                    (fun a (tc : Provider.tool_call) ->
                      a + String.length tc.arguments)
                    0 m.tool_calls
                in
                acc + ((String.length m.content + tc_args + 3) / 4))
              0 messages
          in
          let large_request =
            List.length messages > 150 || estimated_tokens > 75_000
          in
          let msg =
            if
              r.status = 400
              && string_contains body "Bad Request"
              && large_request
            then
              Printf.sprintf
                "OpenAI Codex error (HTTP %d): possible context length issue \
                 (msgs=%d ~%dk tok; raw: %s)"
                r.status (List.length messages) (estimated_tokens / 1000) body
            else Printf.sprintf "OpenAI Codex error (HTTP %d): %s" r.status body
          in
          Lwt.fail_with msg)
        ~on_ok:(fun stream -> process_stream stream ~on_chunk)
        ()

let complete ~(config : Runtime_config.t) ~provider ~model ~messages ?tools () =
  let provider_name =
    match
      List.find_opt
        (fun (_, candidate) -> candidate = provider)
        config.providers
    with
    | Some (name, _) -> name
    | None -> Openai_codex_oauth.default_provider_name
  in
  do_request ~provider_name ~provider ~model ~messages ?tools
    ~on_chunk:(fun _ -> Lwt.return_unit)
    ()

let complete_streaming ~(config : Runtime_config.t) ~provider ~model ~messages
    ?tools ~on_chunk () =
  let provider_name =
    match
      List.find_opt
        (fun (_, candidate) -> candidate = provider)
        config.providers
    with
    | Some (name, _) -> name
    | None -> Openai_codex_oauth.default_provider_name
  in
  do_request ~provider_name ~provider ~model ~messages ?tools ~on_chunk ()
