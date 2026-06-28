(* Provider implementation for MiniMax Anthropic-compatible API *)

let minimax_base = "https://api.minimax.io"

(* B646: MiniMax returns HTTP 500 + body containing error code "1234" with
   message "Network error, please try again later" several times per day.
   The global retry layer (3 attempts, 1s/2s) gives up fast. Add an inline
   500-1234 retry with longer exponential backoff (5s, 15s, 45s) up to 3
   extra attempts, AND surface the upstream error_id in the WARN so the
   user can correlate with MiniMax support. *)
let is_minimax_transient_500 body =
  try
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    let code =
      try json |> member "error" |> member "code" |> to_string with _ -> ""
    in
    code = "1234"
  with _ -> false

let extract_minimax_error_id body =
  try
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    let msg =
      try json |> member "error" |> member "message" |> to_string with _ -> ""
    in
    (* MiniMax's message: "Network error, error id: <hex>, please try again later" *)
    let prefix = "error id: " in
    match String.index_opt msg 'e' with
    | None -> None
    | Some _ -> (
        match
          try Some (Str.search_forward (Str.regexp_string prefix) msg 0)
          with Not_found -> None
        with
        | None -> None
        | Some i ->
            let start = i + String.length prefix in
            let endi =
              try String.index_from msg start ','
              with Not_found -> String.length msg
            in
            Some (String.sub msg start (endi - start)))
  with _ -> None

let api_model_name model =
  match String.lowercase_ascii (String.trim model) with
  | "minimax-m2.7" -> "MiniMax-M2.7"
  | "minimax-m2.7-highspeed" -> "MiniMax-M2.7-highspeed"
  | "minimax-m2.5" -> "MiniMax-M2.5"
  | "minimax-m2.5-highspeed" -> "MiniMax-M2.5-highspeed"
  | "minimax-m2.1" -> "MiniMax-M2.1"
  | "minimax-m2.1-highspeed" -> "MiniMax-M2.1-highspeed"
  | "minimax-m2" -> "MiniMax-M2"
  | _ -> model

let messages_to_anthropic_json msgs =
  (* B644: MiniMax enforces strict adjacency: every assistant tool_use must
     be immediately followed by the user-with-tool_result group that pairs
     it. Use the strict walker as the final defense after
     reorder_tool_groups + ensure_tool_group_integrity. *)
  Provider.messages_to_anthropic_json ~strict_pairing:true msgs

let tools_to_anthropic_json tools =
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

let parse_response body model =
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
        (* B608: MiniMax (Anthropic-compatible) reports input_tokens =
           NEW (uncached) tokens and cache_read_input_tokens separately.
           Normalize to OpenAI-style total-prompt-tokens so the cache-hit
           log and cost calculation are consistent across providers. *)
        Some (input_tokens + cached, output_tokens, cached)
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
               provider_response_items_json = Some body;
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
    Error ("Failed to parse MiniMax response: " ^ Printexc.to_string exn)

let complete ~(config : Runtime_config.t)
    ~(provider : Runtime_config.provider_config) ~model ~messages ?tools
    ?session_key:_ () =
  let open Lwt.Syntax in
  let max_tokens = Option.value ~default:8192 provider.max_output_tokens in
  let base_url =
    match provider.base_url with Some url -> url | None -> minimax_base
  in
  let uri = base_url ^ "/anthropic/v1/messages" in
  let api_model = api_model_name model in
  let system_prompt = extract_system_prompt messages in
  (* B620: strip orphan tool_use/tool_result pairs before conversion so the
     Anthropic adjacency requirement is satisfied even after session resume
     dropped intermediate state. *)
  let messages = Message_history.ensure_tool_group_integrity messages in
  let anthropic_messages = messages_to_anthropic_json messages in
  let body_fields =
    [
      ("model", `String api_model);
      ("max_tokens", `Int max_tokens);
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
    [ ("x-api-key", provider.api_key); ("anthropic-version", "2023-06-01") ]
  in
  Logs.info (fun m ->
      m "MiniMax request to %s model=%s api_model=%s msgs=%d" uri model
        api_model (List.length messages));
  (* B646: inline retry loop for transient HTTP 500 / code 1234 with longer
     exponential backoff than the global retry layer provides. *)
  let rec attempt_with_backoff n =
    let* status, response_body = Http_client.post_json ~uri ~headers ~body in
    if status = 500 && is_minimax_transient_500 response_body && n < 3 then begin
      let delay = 5.0 *. Float.pow 3.0 (Float.of_int n) in
      let eid = extract_minimax_error_id response_body in
      Logs.warn (fun m ->
          m
            "MiniMax transient 500 (code 1234, error_id=%s) — extra attempt \
             %d/3 in %.0fs"
            (Option.value eid ~default:"?")
            (n + 1) delay);
      let* () = Lwt_unix.sleep delay in
      attempt_with_backoff (n + 1)
    end
    else Lwt.return (status, response_body)
  in
  let* status, response_body = attempt_with_backoff 0 in
  if status < 200 || status >= 300 then begin
    (* B637: log request body size on any 4xx/5xx so 404-nginx and other
       silent rejections are easier to diagnose. *)
    Logs.warn (fun m ->
        m
          "MiniMax HTTP %d — request body %d bytes (~%dk), response body %d \
           bytes"
          status (String.length body)
          (String.length body / 1024)
          (String.length response_body));
    Logs.warn (fun m ->
        m "MiniMax body shape on error (HTTP %d): %s" status
          (* B642: truncate to last 24 messages by default; set
             MINIMAX_DEBUG_BODY=1 to see the full history shape. *)
          (let tail =
             match Sys.getenv_opt "MINIMAX_DEBUG_BODY" with
             | Some v when v <> "" && v <> "0" -> 0
             | _ -> 24
           in
           Provider.summarize_anthropic_messages ~tail anthropic_messages));
    Lwt.fail_with
      (Printf.sprintf "MiniMax API error (HTTP %d): %s" status response_body)
  end
  else
    match parse_response response_body model with
    | Ok resp -> Lwt.return resp
    | Error msg -> Lwt.fail_with msg

(* B640: SSE accumulator state, lifted out of [complete_streaming] so tests
   can drive [process_sse_event] with a synthetic list of (event_type, data)
   pairs without standing up an HTTP server. Same state, same code path —
   the streaming function below threads this record through [process_sse_event].
   Fields are mutable so closures can update in place via record assignment. *)
type stream_state = {
  content_acc : Buffer.t;
  thinking_acc : Buffer.t;
  mutable tool_calls_acc : (int * string * string * Buffer.t) list;
  mutable current_block_type : string;
  mutable current_block_index : int;
  mutable current_tool_id : string;
  mutable current_tool_name : string;
  mutable stop_reason : string;
  mutable usage_acc : (int * int * int) option;
  mutable resp_model : string;
  mutable raw_tool_events : Yojson.Safe.t list;
}

let make_stream_state ~model =
  {
    content_acc = Buffer.create 1024;
    thinking_acc = Buffer.create 256;
    tool_calls_acc = [];
    current_block_type = "";
    current_block_index = 0;
    current_tool_id = "";
    current_tool_name = "";
    stop_reason = "";
    usage_acc = None;
    resp_model = model;
    raw_tool_events = [];
  }

let process_sse_event ~(state : stream_state)
    ~(on_chunk : Provider.stream_event -> unit Lwt.t) ~event_type ~data_str =
  try
    let json = Yojson.Safe.from_string data_str in
    let open Yojson.Safe.Util in
    match event_type with
    | "message_start" ->
        (try
           state.resp_model <-
             json |> member "message" |> member "model" |> to_string
         with _ -> ());
        (try
           let u = json |> member "message" |> member "usage" in
           let it = u |> member "input_tokens" |> to_int in
           let ot = try u |> member "output_tokens" |> to_int with _ -> 0 in
           let cached =
             try u |> member "cache_read_input_tokens" |> to_int with _ -> 0
           in
           (* B608: normalize to total-input semantics (see complete). *)
           state.usage_acc <- Some (it + cached, ot, cached)
         with _ -> ());
        Lwt.return_unit
    | "content_block_start" -> (
        try
          state.current_block_index <- json |> member "index" |> to_int;
          let block = json |> member "content_block" in
          let btype = block |> member "type" |> to_string in
          state.current_block_type <- btype;
          if btype = "tool_use" then begin
            state.raw_tool_events <-
              state.raw_tool_events
              @ [
                  `Assoc
                    [
                      ("event", `String event_type);
                      ("data_raw", `String data_str);
                    ];
                ];
            state.current_tool_id <-
              (try block |> member "id" |> to_string with _ -> "");
            state.current_tool_name <-
              (try block |> member "name" |> to_string with _ -> "");
            let args_buf = Buffer.create 256 in
            (* B634: seed args_buf from content_block.input when the server
               embeds the full tool input at start time (observed in MiniMax
               traffic) so we don't end up with empty args. *)
            (match try Some (block |> member "input") with _ -> None with
            | Some (`Assoc kvs) when kvs <> [] ->
                Buffer.add_string args_buf (Yojson.Safe.to_string (`Assoc kvs))
            | Some (`Assoc _) | Some `Null | None -> ()
            | Some other ->
                Buffer.add_string args_buf (Yojson.Safe.to_string other));
            state.tool_calls_acc <-
              state.tool_calls_acc
              @ [
                  ( state.current_block_index,
                    state.current_tool_id,
                    state.current_tool_name,
                    args_buf );
                ];
            on_chunk
              (Provider.ToolCallDelta
                 {
                   index = state.current_block_index;
                   id = Some state.current_tool_id;
                   function_name = Some state.current_tool_name;
                   arguments =
                     (let s = Buffer.contents args_buf in
                      if s = "" then None else Some s);
                 })
          end
          else Lwt.return_unit
        with _ -> Lwt.return_unit)
    | "content_block_delta" -> (
        try
          let delta = json |> member "delta" in
          let dtype = delta |> member "type" |> to_string in
          if dtype = "thinking_delta" || state.current_block_type = "thinking"
          then begin
            let thinking =
              try delta |> member "thinking" |> to_string with _ -> ""
            in
            if thinking <> "" then begin
              Buffer.add_string state.thinking_acc thinking;
              on_chunk (Provider.ThinkingDelta thinking)
            end
            else Lwt.return_unit
          end
          else if dtype = "text_delta" then begin
            let text = delta |> member "text" |> to_string in
            Buffer.add_string state.content_acc text;
            on_chunk (Provider.Delta text)
          end
          else begin
            if dtype = "input_json_delta" then begin
              state.raw_tool_events <-
                state.raw_tool_events
                @ [
                    `Assoc
                      [
                        ("event", `String event_type);
                        ("data_raw", `String data_str);
                      ];
                  ];
              let partial = delta |> member "partial_json" |> to_string in
              List.iter
                (fun (idx, _, _, args_buf) ->
                  if idx = state.current_block_index then
                    Buffer.add_string args_buf partial)
                state.tool_calls_acc;
              on_chunk
                (Provider.ToolCallDelta
                   {
                     index = state.current_block_index;
                     id = None;
                     function_name = None;
                     arguments = Some partial;
                   })
            end
            else Lwt.return_unit
          end
        with _ -> Lwt.return_unit)
    | "content_block_stop" ->
        state.current_block_type <- "";
        state.current_block_index <- 0;
        Lwt.return_unit
    | "message_delta" ->
        (try
           let d = json |> member "delta" in
           state.stop_reason <-
             (try d |> member "stop_reason" |> to_string with _ -> "");
           try
             let u = json |> member "usage" in
             let ot = u |> member "output_tokens" |> to_int in
             state.usage_acc <-
               (match state.usage_acc with
               | Some (it, _, cached) -> Some (it, ot, cached)
               | None -> Some (0, ot, 0))
           with _ -> ()
         with _ -> ());
        Lwt.return_unit
    | "message_stop" -> on_chunk Provider.Done
    | _ -> Lwt.return_unit
  with _ -> Lwt.return_unit

let finalize_stream_tool_calls (state : stream_state) =
  List.map
    (fun (_, id, name, args_buf) ->
      let raw_args = Buffer.contents args_buf in
      let arguments =
        if raw_args = "" then begin
          Logs.warn (fun m ->
              m
                "MiniMax stream produced tool_use '%s' (id=%s) with NO \
                 input_json_delta events — falling back to \"{}\". Set \
                 MINIMAX_DEBUG_SSE=1 to capture the raw SSE for diagnosis."
                name id);
          "{}"
        end
        else raw_args
      in
      { Provider.id; function_name = name; arguments })
    state.tool_calls_acc

let finalize_stream_provider_response_items_json state =
  match state.raw_tool_events with
  | [] -> None
  | events -> Some (Yojson.Safe.to_string (`List events))

let complete_streaming ~(config : Runtime_config.t)
    ~(provider : Runtime_config.provider_config) ~model ~messages ?tools
    ?session_key:_ ~on_chunk () =
  let open Lwt.Syntax in
  let max_tokens = Option.value ~default:8192 provider.max_output_tokens in
  let base_url =
    match provider.base_url with Some url -> url | None -> minimax_base
  in
  let uri = base_url ^ "/anthropic/v1/messages" in
  let api_model = api_model_name model in
  let system_prompt = extract_system_prompt messages in
  (* B620: see complete() for rationale. *)
  let messages = Message_history.ensure_tool_group_integrity messages in
  let anthropic_messages = messages_to_anthropic_json messages in
  let body_fields =
    [
      ("model", `String api_model);
      ("max_tokens", `Int max_tokens);
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
    [ ("x-api-key", provider.api_key); ("anthropic-version", "2023-06-01") ]
  in
  Logs.info (fun m ->
      m "MiniMax stream request to %s model=%s api_model=%s msgs=%d" uri model
        api_model (List.length messages));
  let on_error (r : Http_client.stream_response) =
    let open Lwt.Syntax in
    let* err_body = Http_client.collect_error_body r.stream in
    (* B637: log request body size for streaming errors too. *)
    Logs.warn (fun m ->
        m
          "MiniMax stream HTTP %d — request body %d bytes (~%dk), response \
           body %d bytes"
          r.status (String.length body)
          (String.length body / 1024)
          (String.length err_body));
    Logs.warn (fun m ->
        m "MiniMax stream body shape on error (HTTP %d): %s" r.status
          (* B642: truncate to last 24 messages by default; set
             MINIMAX_DEBUG_BODY=1 to see the full history shape. *)
          (let tail =
             match Sys.getenv_opt "MINIMAX_DEBUG_BODY" with
             | Some v when v <> "" && v <> "0" -> 0
             | _ -> 24
           in
           Provider.summarize_anthropic_messages ~tail anthropic_messages));
    (* B646: log error_id for transient 500 / code 1234 so it can be reported. *)
    (match extract_minimax_error_id err_body with
    | Some eid when r.status = 500 ->
        Logs.warn (fun m ->
            m
              "MiniMax stream upstream error_id=%s (HTTP 500) — report to \
               support"
              eid)
    | _ -> ());
    Lwt.fail_with
      (Printf.sprintf "MiniMax API error (HTTP %d): %s" r.status err_body)
  in
  (* B658: per-chunk idle timeout via provider.http_timeout_s. *)
  Http_client.post_stream_with ?stream_idle_timeout_s:provider.http_timeout_s
    ~uri ~headers ~body ~label:"MiniMax API error" ~on_error
    ~on_ok:(fun stream ->
      let buf = Buffer.create 256 in
      let state = make_stream_state ~model in
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
          process_sse_event ~state ~on_chunk ~event_type:!current_event
            ~data_str:data
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
      let content = Buffer.contents state.content_acc in
      let final_model = state.resp_model in
      let tool_calls = finalize_stream_tool_calls state in
      let thinking =
        let t = Buffer.contents state.thinking_acc in
        if t = "" then None else Some t
      in
      let provider_response_items_json =
        finalize_stream_provider_response_items_json state
      in
      Lwt.return
        (Provider.make_stream_result ~tool_calls ~content ~model:final_model
           ~usage:state.usage_acc ~provider_response_items_json ~thinking ()))
    ()
