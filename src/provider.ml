include Provider_types
include Provider_streaming
include Provider_routing

let complete ~(config : Runtime_config.t) ~messages ?tools ?session_key
    ?preferred_provider ?quota_states () =
  let open Lwt.Syntax in
  let provider_name, provider, model =
    select_provider ~config ?preferred_provider ?quota_states ()
  in
  let model = normalize_model_casing ~provider_name model in
  let kind = detect_kind ~name:provider_name provider in
  let sk_tag = match session_key with Some s -> "[" ^ s ^ "] " | None -> "" in
  (* Dispatch to native handler if registered *)
  match List.assoc_opt kind !native_complete with
  | Some fn ->
      Logs.info (fun m ->
          m "%s-> LLM provider=%s model=%s msgs=%d ~%dk tok" sk_tag
            provider_name model (List.length messages)
            (estimate_messages_tokens messages / 1000));
      fn ~config ~provider ~model ~messages ?tools ?session_key ()
  | None -> (
      let base_url =
        match provider.base_url with
        | Some url -> url
        | None -> default_base_url_for provider_name
      in
      let uri = base_url ^ "/chat/completions" in
      let temp_locked = model_requires_temperature_one model in
      (* B638: apply tool-group integrity before conversion. Z.ai (glm-5.1)
         strict-checks message shape and rejects with code 1214 "messages
         parameter is illegal" when orphan tool_use / tool_result pairs
         survive (typically after session resume drops intermediate state).
         Inlined here to avoid a cyclic dependency on Message_history
         (which itself imports Provider for the message type).
         B675: also enforce adjacency — kimi (Moonshot OpenAI-compat) rejects
         with HTTP 400 if an assistant tool_calls message is not immediately
         followed by tool messages for each id. Intervening system/user
         messages (e.g. observer notes injected on resume) break this. *)
      let messages =
        messages |> inline_ensure_tool_group_integrity |> reorder_tool_groups
      in
      let body_fields =
        [
          ("model", `String model);
          ( "messages",
            messages_to_json
              ~require_reasoning_content:
                (model_requires_reasoning_content model)
              messages );
        ]
      in
      let body_fields =
        if temp_locked then body_fields
        else
          body_fields
          @ [ ("temperature", `Float (max 1e-8 config.default_temperature)) ]
      in
      let body_fields =
        match tools with
        | Some t when t <> `List [] -> body_fields @ [ ("tools", t) ]
        | _ -> body_fields
      in
      let body_fields =
        match config.agent_defaults.reasoning_effort with
        | Some re -> body_fields @ [ ("reasoning_effort", `String re) ]
        | None -> body_fields
      in
      let body_fields =
        body_fields @ provider_extra_body_fields ~provider_name ~provider
      in
      let body_fields =
        match (kind, provider.prompt_cache_retention) with
        | (OpenAICompat | OpenAICodex), Some r ->
            body_fields @ [ ("prompt_cache_retention", `String r) ]
        | _ -> body_fields
      in
      let body = `Assoc body_fields |> Yojson.Safe.to_string in
      let headers = [ ("Authorization", "Bearer " ^ provider.api_key) ] in
      Logs.info (fun m ->
          m "%s-> LLM provider=%s model=%s msgs=%d ~%dk tok" sk_tag
            provider_name model (List.length messages)
            (estimate_messages_tokens messages / 1000));
      (* B647: honor per-provider HTTP timeout when configured. Fall back
         to the unparameterized post_json (uses global default) when None. *)
      let* status, response_body =
        match provider.http_timeout_s with
        | Some timeout_s ->
            Http_client.post_json_with_timeout ~timeout_s ~uri ~headers ~body
        | None -> Http_client.post_json ~uri ~headers ~body
      in
      if status < 200 || status >= 300 then begin
        (* B638: dump the failing request/response to a one-shot diagnostic
           file when ZAI_DEBUG_BODY=1, so the exact 1214-triggering body
           can be inspected. *)
        (match Sys.getenv_opt "ZAI_DEBUG_BODY" with
        | Some v when v <> "" && v <> "0" -> (
            let path =
              Printf.sprintf "/tmp/clawq-zai-debug-%d-%d.json" (Unix.getpid ())
                (int_of_float (Unix.gettimeofday ()))
            in
            try
              let oc = open_out path in
              output_string oc
                (Printf.sprintf
                   "{\"provider\":%S,\"model\":%S,\"status\":%d,\"request\":%s,\"response\":%s}"
                   provider_name model status body response_body);
              close_out oc;
              Logs.warn (fun m ->
                  m "ZAI_DEBUG_BODY: wrote failing %s/%s exchange to %s"
                    provider_name model path)
            with _ -> ())
        | _ -> ());
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
              Lwt.return
                (Text
                   {
                     content = failed_gen;
                     model;
                     usage = None;
                     provider_response_items_json = None;
                     thinking = None;
                   })
            else
              Lwt.fail_with
                (Printf.sprintf "LLM API error (HTTP %d): %s" status
                   response_body)
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
                let cached =
                  try
                    u
                    |> member "prompt_tokens_details"
                    |> member "cached_tokens" |> to_int
                  with _ -> 0
                in
                Some (pt, ct, cached)
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
                          m
                            "LLM response dropped malformed tool_call at \
                             index=%d"
                            i);
                      None)
                  tool_calls_json
                |> List.filter_map (fun x -> x)
              in
              Lwt.return
                (ToolCalls
                   {
                     calls;
                     model = resp_model;
                     usage;
                     provider_response_items_json = None;
                     thinking = None;
                   })
            else
              let raw_content =
                try choice |> member "content" |> to_string with _ -> ""
              in
              let thinking_style =
                thinking_style_of_provider ~provider_name provider
              in
              let content, thinking_text =
                match thinking_style with
                | TaggedThinking ->
                    let visible, thought = split_tagged_text raw_content in
                    (visible, if thought = "" then None else Some thought)
                | ReasoningContent ->
                    let rc =
                      try
                        Some (choice |> member "reasoning_content" |> to_string)
                      with _ -> None
                    in
                    (raw_content, rc)
                | NoThinking -> (raw_content, None)
              in
              if content = "" && raw_content = "" then
                Lwt.fail_with "Failed to extract content from LLM response"
              else
                Lwt.return
                  (Text
                     {
                       content;
                       model = resp_model;
                       usage;
                       provider_response_items_json = None;
                       thinking = thinking_text;
                     }))

let complete_stream ~(config : Runtime_config.t) ~messages ?tools ?session_key
    ?preferred_provider ?quota_states ~on_chunk () =
  let open Lwt.Syntax in
  let provider_name, provider, model =
    select_provider ~config ?preferred_provider ?quota_states ()
  in
  let model = normalize_model_casing ~provider_name model in
  let kind = detect_kind ~name:provider_name provider in
  let sk_tag = match session_key with Some s -> "[" ^ s ^ "] " | None -> "" in
  (* Dispatch to native stream handler if registered *)
  match List.assoc_opt kind !native_stream with
  | Some fn ->
      Logs.info (fun m ->
          m "%s-> LLM provider=%s model=%s msgs=%d ~%dk tok" sk_tag
            provider_name model (List.length messages)
            (estimate_messages_tokens messages / 1000));
      fn ~config ~provider ~model ~messages ?tools ?session_key ~on_chunk ()
  | None ->
      let base_url =
        match provider.base_url with
        | Some url -> url
        | None -> default_base_url_for provider_name
      in
      let uri = base_url ^ "/chat/completions" in
      let temp_locked = model_requires_temperature_one model in
      (* B675: enforce both integrity and adjacency on the streaming path too. *)
      let messages =
        messages |> inline_ensure_tool_group_integrity |> reorder_tool_groups
      in
      let body_fields =
        [
          ("model", `String model);
          ( "messages",
            messages_to_json
              ~require_reasoning_content:
                (model_requires_reasoning_content model)
              messages );
          ("stream", `Bool true);
        ]
      in
      let body_fields =
        if temp_locked then body_fields
        else
          body_fields
          @ [ ("temperature", `Float (max 1e-8 config.default_temperature)) ]
      in
      let body_fields =
        match tools with
        | Some t when t <> `List [] -> body_fields @ [ ("tools", t) ]
        | _ -> body_fields
      in
      let body_fields =
        match config.agent_defaults.reasoning_effort with
        | Some re -> body_fields @ [ ("reasoning_effort", `String re) ]
        | None -> body_fields
      in
      let body_fields =
        body_fields @ provider_extra_body_fields ~provider_name ~provider
      in
      let body_fields =
        match (kind, provider.prompt_cache_retention) with
        | (OpenAICompat | OpenAICodex), Some r ->
            body_fields @ [ ("prompt_cache_retention", `String r) ]
        | _ -> body_fields
      in
      let body = `Assoc body_fields |> Yojson.Safe.to_string in
      let headers = [ ("Authorization", "Bearer " ^ provider.api_key) ] in
      Logs.info (fun m ->
          m "%s-> LLM provider=%s model=%s msgs=%d ~%dk tok" sk_tag
            provider_name model (List.length messages)
            (estimate_messages_tokens messages / 1000));
      (* B658: gate body-stream reads on provider.http_timeout_s so a
         dropped TCP / silent stall surfaces as a clear failure instead of
         wedging the agent loop. *)
      Http_client.post_stream_with
        ?stream_idle_timeout_s:provider.http_timeout_s ~uri ~headers ~body
        ~label:"LLM API error"
        ~on_ok:(fun stream ->
          process_sse_stream
            ~thinking_style:(thinking_style_of_provider ~provider_name provider)
            stream ~on_chunk)
        ()
