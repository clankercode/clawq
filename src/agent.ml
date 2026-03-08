type t = {
  mutable history : Provider.message list;
  mutable config : Runtime_config.t;
  mutable system_prompt : string;
  tool_registry : Tool_registry.t option;
}

exception Interrupted of string

let create ~config ?tool_registry () =
  let system_prompt = Prompt_builder.build ~config ~tool_registry () in
  { history = []; config; system_prompt; tool_registry }

let inject_runtime_context messages runtime_context =
  let rec loop rev_prefix = function
    | [] -> List.rev rev_prefix
    | (msg : Provider.message) :: rest when msg.role = "user" ->
        List.rev_append rev_prefix
          ({ msg with content = runtime_context ^ "\n\n" ^ msg.content } :: rest)
    | msg :: rest -> loop (msg :: rev_prefix) rest
  in
  loop [] (List.rev messages) |> List.rev

let build_messages ?runtime_context agent =
  agent.system_prompt <-
    Prompt_builder.build ~config:agent.config ~tool_registry:agent.tool_registry
      ();
  let messages =
    Provider.make_message ~role:"system" ~content:agent.system_prompt
    :: List.rev agent.history
  in
  match runtime_context with
  | Some block when String.trim block <> "" ->
      inject_runtime_context messages block
  | _ -> messages

let estimate_tokens content = (String.length content + 3) / 4

let estimate_history_tokens history =
  List.fold_left
    (fun acc (m : Provider.message) -> acc + estimate_tokens m.content)
    0 history

let context_window_for_agent agent =
  let model =
    Runtime_config.effective_primary_model agent.config.agent_defaults
  in
  match Runtime_config.context_window_for_model model with
  | Some w -> w
  | None -> 128000

let compaction_threshold_for_agent agent =
  let token_budget = context_window_for_agent agent in
  let percent =
    Runtime_config.effective_compaction_threshold_percent agent.config.memory
  in
  token_budget * percent / 100

let effective_max_messages agent =
  let m = agent.config.memory.max_messages_per_session in
  if m <= 0 then 500 else min m 500

let runtime_context_usage agent ~compacted_before_turn =
  let context_window_tokens = context_window_for_agent agent in
  {
    Prompt_builder.history_messages = List.length agent.history;
    estimated_history_tokens = estimate_history_tokens agent.history;
    context_window_tokens;
    compaction_threshold_tokens = compaction_threshold_for_agent agent;
    max_messages_per_session = effective_max_messages agent;
    compacted_before_turn;
  }

(* Number of most-recent messages kept verbatim after compaction. *)
let compaction_keep_recent = 20

(* History must have more than this many messages before force-compression
   is attempted (to avoid compressing already-tiny histories). *)
let context_recovery_min_history = 6

(* Number of most-recent messages to retain during force-compression. *)
let force_compress_keep = 4

(* Bound tool output persisted into conversation history. *)
let max_tool_result_chars = 12000

(* Backstop: enforce the hard message-count cap only. Token-based compaction
   (with LLM summarisation) is handled by compact_history_if_needed before
   each turn; this function is a cheap post-response safety net. *)
let trim_history agent =
  let effective_max = effective_max_messages agent in
  let len = List.length agent.history in
  if len > effective_max then
    agent.history <- List.filteri (fun i _ -> i < effective_max) agent.history

(* Emergency context-exhaustion recovery: drop all but the last
   force_compress_keep messages (no LLM call possible at this point).
   Returns true if compression was performed. *)
let force_compress_history agent =
  let len = List.length agent.history in
  if len > context_recovery_min_history then begin
    let recent =
      List.filteri (fun i _ -> i < force_compress_keep) agent.history
    in
    let bounded_recent =
      List.map
        (fun (m : Provider.message) ->
          if String.length m.content <= max_tool_result_chars then m
          else
            {
              m with
              content =
                String.sub m.content 0 max_tool_result_chars
                ^ "\n\n[truncated during emergency context recovery]";
            })
        recent
    in
    agent.history <- bounded_recent;
    true
  end
  else false

let string_contains_ci s sub =
  let sl = String.lowercase_ascii s in
  let subl = String.lowercase_ascii sub in
  let ls = String.length sl and lsub = String.length subl in
  if lsub > ls then false
  else
    let found = ref false in
    for i = 0 to ls - lsub do
      if (not !found) && String.sub sl i lsub = subl then found := true
    done;
    !found

let is_context_exhaustion_error msg =
  List.exists (string_contains_ci msg)
    [
      "context length";
      "too long";
      "maximum context";
      "context window";
      "token limit";
    ]

let truncate_for_history s ~max_chars =
  if String.length s <= max_chars then s
  else
    let omitted = String.length s - max_chars in
    String.sub s 0 max_chars
    ^ Printf.sprintf
        "\n\n[truncated %d chars to keep context within model limits]" omitted

let summarize_messages agent messages =
  let open Lwt.Syntax in
  let content =
    List.map
      (fun (m : Provider.message) ->
        Printf.sprintf "[%s]: %s" m.role
          (if String.length m.content > 2000 then
             String.sub m.content 0 2000 ^ "..."
           else m.content))
      messages
    |> String.concat "\n"
  in
  let prompt =
    "Summarize the following conversation excerpt concisely. Preserve key \
     facts, decisions, and context that would be needed to continue the \
     conversation. Output only the summary, no preamble.\n\n" ^ content
  in
  let msgs =
    [
      Provider.make_message ~role:"system"
        ~content:
          "You are a conversation summarizer. Be concise but preserve all \
           important context.";
      Provider.make_message ~role:"user" ~content:prompt;
    ]
  in
  Lwt.catch
    (fun () ->
      let* response =
        Provider.complete ~config:agent.config ~messages:msgs ()
      in
      match response with
      | Provider.Text { content; _ } -> Lwt.return content
      | Provider.ToolCalls _ -> Lwt.return content)
    (fun exn ->
      Logs.warn (fun m ->
          m "History summarization failed: %s" (Printexc.to_string exn));
      Lwt.return
        (Printf.sprintf "[Summary of %d messages - summarization failed]"
           (List.length messages)))

let compact_history_if_needed agent =
  let open Lwt.Syntax in
  let effective_max = effective_max_messages agent in
  let len = List.length agent.history in
  let compaction_threshold = compaction_threshold_for_agent agent in
  let current_tokens = estimate_history_tokens agent.history in
  if len > effective_max || current_tokens > compaction_threshold then begin
    let history_chrono = List.rev agent.history in
    let total = List.length history_chrono in
    (* Keep the most-recent messages verbatim; only summarise older ones. *)
    let keep = min compaction_keep_recent total in
    let compact_count = total - keep in
    if compact_count = 0 then Lwt.return false
    else begin
      Logs.info (fun m ->
          m "Compacting history: %d messages -> summarise %d, keep %d recent"
            total compact_count keep);
      let to_compact =
        List.filteri (fun i _ -> i < compact_count) history_chrono
      in
      let to_keep =
        List.filteri (fun i _ -> i >= compact_count) history_chrono
      in
      let mid = compact_count / 2 in
      let first_half = List.filteri (fun i _ -> i < mid) to_compact in
      let second_half = List.filteri (fun i _ -> i >= mid) to_compact in
      let* summary1 = summarize_messages agent first_half in
      let* summary2 = summarize_messages agent second_half in
      let merged_summary =
        Printf.sprintf
          "[Conversation history compacted]\n\n\
           Earlier context:\n\
           %s\n\n\
           Recent context:\n\
           %s"
          summary1 summary2
      in
      (* Use "assistant" role so build_messages produces exactly one
         "system" message (the real system prompt prepended at call time).
         A second "system"-role message here would confuse many LLM APIs. *)
      let summary_msg =
        Provider.make_message ~role:"assistant" ~content:merged_summary
      in
      (* Rebuild history (newest-first) as: recent messages then summary. *)
      agent.history <- List.rev (summary_msg :: to_keep);
      Lwt.return true
    end
  end
  else Lwt.return false

let tools_json agent =
  match agent.tool_registry with
  | Some r when agent.config.security.tools_enabled ->
      if agent.config.agent_defaults.tool_search_enabled then
        Some (Tool_registry.to_openai_json_with_search r)
      else Some (Tool_registry.to_openai_json r)
  | _ -> None

let risk_level_to_string = function
  | Tool.Low -> "low"
  | Tool.Medium -> "medium"
  | Tool.High -> "high"

(* Handle a tool_search call by searching the registry and returning
   matching tool definitions as a tool_search_output message. *)
let resolve_tool_search agent (tc : Provider.tool_call) =
  let query =
    try
      let args = Yojson.Safe.from_string tc.arguments in
      let open Yojson.Safe.Util in
      try args |> member "query" |> to_string
      with _ -> (
        try args |> member "goal" |> to_string
        with _ -> Yojson.Safe.to_string args)
    with _ -> tc.arguments
  in
  match agent.tool_registry with
  | None ->
      Provider.make_tool_result ~tool_call_id:tc.id ~name:"tool_search"
        ~content:"No tool registry available"
  | Some registry ->
      let results = Tool_registry.search registry ~query in
      let top = List.filteri (fun i _ -> i < 10) results in
      let tools_json = `List (List.map Tool_registry.tool_to_openai_json top) in
      Logs.info (fun m ->
          m "Tool search query=%S found=%d tools" query (List.length top));
      Provider.make_tool_search_result ~tool_call_id:tc.id ~tools_json

(* Execute all tool calls in parallel, then append results to history in the
   original call order. Parallel execution reduces latency when the LLM issues
   multiple independent tool calls in a single turn. *)
let execute_tool_calls_stream agent ~db ~audit_enabled ~session_key ~on_chunk
    calls =
  let open Lwt.Syntax in
  let sk_tag = match session_key with Some s -> "[" ^ s ^ "] " | None -> "" in
  let* results =
    Lwt_list.map_p
      (fun (tc : Provider.tool_call) ->
        Logs.info (fun m ->
            m "%sTool call: %s (id=%s) args=%s" sk_tag tc.function_name tc.id
              tc.arguments);
        (match (db, audit_enabled, session_key) with
        | Some db, true, Some sk ->
            let risk =
              match agent.tool_registry with
              | Some reg -> (
                  match Tool_registry.find reg tc.function_name with
                  | Some t -> risk_level_to_string t.risk_level
                  | None -> "unknown")
              | None -> "unknown"
            in
            Audit.log ~db
              (ToolInvocation
                 {
                   session_key = sk;
                   tool_name = tc.function_name;
                   risk_level = risk;
                   args_preview = tc.arguments;
                 })
        | _ -> ());
        let* () =
          on_chunk
            (Provider.ToolStart
               { id = tc.id; name = tc.function_name; arguments = tc.arguments })
        in
        let is_tool_search = tc.function_name = "tool_search" in
        let streamed_output = ref false in
        let* result_msg, result_for_event =
          if is_tool_search then
            let msg = resolve_tool_search agent tc in
            Lwt.return (msg, msg.Provider.content)
          else
            let* result =
              match agent.tool_registry with
              | None -> Lwt.return "Error: no tool registry available"
              | Some registry -> (
                  match Tool_registry.find registry tc.function_name with
                  | None ->
                      Lwt.return
                        (Printf.sprintf "Error: unknown tool '%s'"
                           tc.function_name)
                  | Some tool ->
                      Lwt.catch
                        (fun () ->
                          let args =
                            try Yojson.Safe.from_string tc.arguments
                            with _ -> `Assoc []
                          in
                          let context =
                            {
                              Tool.session_key;
                              send_progress =
                                Some
                                  (fun text ->
                                    streamed_output := true;
                                    on_chunk
                                      (Provider.ToolOutputDelta
                                         { id = tc.id; chunk = text }));
                            }
                          in
                          match tool.invoke_stream with
                          | Some invoke_stream ->
                              invoke_stream ~context
                                ~on_output_chunk:(fun chunk ->
                                  streamed_output := true;
                                  on_chunk
                                    (Provider.ToolOutputDelta
                                       { id = tc.id; chunk }))
                                args
                          | None -> tool.invoke ~context args)
                        (fun exn ->
                          Lwt.return
                            ("Error invoking tool: " ^ Printexc.to_string exn)))
            in
            let result_for_history =
              truncate_for_history result ~max_chars:max_tool_result_chars
            in
            let result_for_event =
              if !streamed_output then result_for_history else result
            in
            Lwt.return
              ( Provider.make_tool_result ~tool_call_id:tc.id
                  ~name:tc.function_name ~content:result_for_history,
                result_for_event )
        in
        let result = result_msg.Provider.content in
        let success =
          not
            (String.length result_for_event >= 6
            && String.sub result_for_event 0 6 = "Error:")
        in
        (match (db, audit_enabled, session_key) with
        | Some db, true, Some sk ->
            Audit.log ~db
              (ToolResult
                 { session_key = sk; tool_name = tc.function_name; success })
        | _ -> ());
        let preview =
          let limit = if success then 200 else 1000 in
          if String.length result > limit then String.sub result 0 limit ^ "..."
          else result
        in
        if success then
          Logs.info (fun m ->
              m "%sTool result: %s -> %s" sk_tag tc.function_name preview)
        else
          Logs.warn (fun m ->
              m "%sTool error: %s -> %s" sk_tag tc.function_name preview);
        let* () =
          on_chunk
            (Provider.ToolResult
               {
                 id = tc.id;
                 name = tc.function_name;
                 result = result_for_event;
                 is_error = not success;
               })
        in
        Lwt.return (tc, result_msg))
      calls
  in
  List.iter
    (fun ((_tc : Provider.tool_call), tool_msg) ->
      agent.history <- tool_msg :: agent.history)
    results;
  Lwt.return_unit

let execute_tool_calls agent ~db ~audit_enabled ~session_key calls =
  let open Lwt.Syntax in
  let sk_tag = match session_key with Some s -> "[" ^ s ^ "] " | None -> "" in
  let* results =
    Lwt_list.map_p
      (fun (tc : Provider.tool_call) ->
        Logs.info (fun m ->
            m "%sTool call: %s (id=%s) args=%s" sk_tag tc.function_name tc.id
              tc.arguments);
        (match (db, audit_enabled, session_key) with
        | Some db, true, Some sk ->
            let risk =
              match agent.tool_registry with
              | Some reg -> (
                  match Tool_registry.find reg tc.function_name with
                  | Some t -> risk_level_to_string t.risk_level
                  | None -> "unknown")
              | None -> "unknown"
            in
            Audit.log ~db
              (ToolInvocation
                 {
                   session_key = sk;
                   tool_name = tc.function_name;
                   risk_level = risk;
                   args_preview = tc.arguments;
                 })
        | _ -> ());
        let is_tool_search = tc.function_name = "tool_search" in
        let* result_msg =
          if is_tool_search then Lwt.return (resolve_tool_search agent tc)
          else
            let* result =
              match agent.tool_registry with
              | None -> Lwt.return "Error: no tool registry available"
              | Some registry -> (
                  match Tool_registry.find registry tc.function_name with
                  | None ->
                      Lwt.return
                        (Printf.sprintf "Error: unknown tool '%s'"
                           tc.function_name)
                  | Some tool ->
                      Lwt.catch
                        (fun () ->
                          let args =
                            try Yojson.Safe.from_string tc.arguments
                            with _ -> `Assoc []
                          in
                          let context =
                            { Tool.session_key; send_progress = None }
                          in
                          tool.invoke ~context args)
                        (fun exn ->
                          Lwt.return
                            ("Error invoking tool: " ^ Printexc.to_string exn)))
            in
            let result_for_history =
              truncate_for_history result ~max_chars:max_tool_result_chars
            in
            Lwt.return
              (Provider.make_tool_result ~tool_call_id:tc.id
                 ~name:tc.function_name ~content:result_for_history)
        in
        let result = result_msg.Provider.content in
        let success =
          not (String.length result >= 6 && String.sub result 0 6 = "Error:")
        in
        (match (db, audit_enabled, session_key) with
        | Some db, true, Some sk ->
            Audit.log ~db
              (ToolResult
                 { session_key = sk; tool_name = tc.function_name; success })
        | _ -> ());
        let truncated =
          let limit = if success then 200 else 1000 in
          if String.length result > limit then String.sub result 0 limit ^ "..."
          else result
        in
        if success then
          Logs.info (fun m ->
              m "%sTool result: %s -> %s" sk_tag tc.function_name truncated)
        else
          Logs.warn (fun m ->
              m "%sTool error: %s -> %s" sk_tag tc.function_name truncated);
        Lwt.return (tc, result_msg))
      calls
  in
  (* Append results in original call order so history is deterministic *)
  List.iter
    (fun ((_tc : Provider.tool_call), tool_msg) ->
      agent.history <- tool_msg :: agent.history)
    results;
  Lwt.return_unit

let inject_search_context agent ~db ~user_message =
  let open Lwt.Syntax in
  if agent.config.memory.search_enabled then
    Lwt.catch
      (fun () ->
        (* FTS keyword search *)
        let keyword_results =
          Memory.search ~db ~query:user_message ~limit:5 ()
        in
        let keyword_strings =
          List.map
            (fun (m : Provider.message) ->
              if String.length m.content > 300 then
                String.sub m.content 0 300 ^ "..."
              else m.content)
            keyword_results
        in
        (* Vector search (if embedding provider is configured) *)
        let* vector_strings =
          if
            agent.config.memory.embedding_provider <> None
            || agent.config.memory.embedding_model <> None
          then
            Lwt.catch
              (fun () ->
                let* query_emb =
                  Vector.fetch_embedding ~config:agent.config ~text:user_message
                in
                let results =
                  Vector.search ~db ~query_embedding:query_emb ~limit:5 ()
                in
                Lwt.return results)
              (fun _exn -> Lwt.return [])
          else Lwt.return []
        in
        (* Merge results *)
        let merged =
          if vector_strings = [] then keyword_strings
          else
            Vector.merge_results ~keyword_results:keyword_strings
              ~vector_results:vector_strings
              ~keyword_weight:agent.config.memory.keyword_weight
              ~vector_weight:agent.config.memory.vector_weight
        in
        let top = List.filteri (fun i _ -> i < 3) merged in
        (* Core memories: always include for awareness *)
        let core_items =
          let all = Memory.list_core ~db () in
          List.filteri (fun i _ -> i < 10) all
        in
        let core_strings =
          List.map
            (fun (key, content, category) ->
              Printf.sprintf "[core:%s/%s] %s" category key content)
            core_items
        in
        match top @ core_strings with
        | [] -> Lwt.return_unit
        | parts ->
            let context_msg =
              Provider.make_message ~role:"system"
                ~content:
                  ("Relevant context from memory:\n" ^ String.concat "\n" parts)
            in
            agent.history <- context_msg :: agent.history;
            Lwt.return_unit)
      (fun _ -> Lwt.return_unit)
  else Lwt.return_unit

let prepare_turn_history agent ~user_message ?db () =
  let open Lwt.Syntax in
  let* () =
    match db with
    | Some db -> inject_search_context agent ~db ~user_message
    | None -> Lwt.return_unit
  in
  agent.history <-
    Provider.make_message ~role:"user" ~content:user_message :: agent.history;
  compact_history_if_needed agent

let turn agent ~user_message ?db ?session_key ?interrupt_check ?runtime_context
    ?(history_prepared = false) () =
  let open Lwt.Syntax in
  let* _compacted =
    if history_prepared then Lwt.return false
    else prepare_turn_history agent ~user_message ?db ()
  in
  let audit_enabled = agent.config.security.audit_enabled in
  let max_iters = agent.config.agent_defaults.max_tool_iterations in
  let tools = tools_json agent in
  let resilient_complete config messages tools =
    let res = config.Runtime_config.resilience in
    let open Lwt.Syntax in
    let primary () = Provider.complete ~config ~messages ?tools () in
    let with_optional_fallback () =
      match res.fallback_provider with
      | Some fb_name ->
          let fb_config = { config with default_provider = Some fb_name } in
          let primary_name, _, _ = Provider.select_provider ~config in
          let fallback_name, _, _ =
            Provider.select_provider ~config:fb_config
          in
          if fallback_name = primary_name then primary ()
          else
            Resilience.with_fallback ~primary ~fallback:(fun () ->
                Provider.complete ~config:fb_config ~messages ?tools ())
      | None -> primary ()
    in
    let* timed =
      Resilience.with_timeout_retry ~timeout_s:res.timeout_s
        ~max_retries:res.max_retries ~base_delay_s:res.base_delay_s
        with_optional_fallback
    in
    match timed with Ok v -> Lwt.return v | Error e -> Lwt.fail_with e
  in
  let track_cost response =
    let usage, model =
      match response with
      | Provider.Text { usage; model; _ } -> (usage, model)
      | Provider.ToolCalls { usage; model; _ } -> (usage, model)
    in
    match (usage, session_key) with
    | Some (pt, ct), Some sid ->
        Cost_tracker.record_turn ~model ~prompt_tokens:pt ~completion_tokens:ct
          ~session_id:sid
    | _ -> ()
  in
  let rec loop iteration =
    let* response =
      Lwt.catch
        (fun () ->
          let messages = build_messages ?runtime_context agent in
          resilient_complete agent.config messages tools)
        (fun exn ->
          if
            is_context_exhaustion_error (Printexc.to_string exn)
            && force_compress_history agent
          then begin
            Logs.warn (fun m ->
                m
                  "Context exhaustion detected; force-compressed history, \
                   retrying turn");
            let messages = build_messages ?runtime_context agent in
            resilient_complete agent.config messages tools
          end
          else Lwt.fail exn)
    in
    track_cost response;
    match response with
    | Provider.Text { content; _ } ->
        agent.history <-
          Provider.make_message ~role:"assistant" ~content :: agent.history;
        trim_history agent;
        Lwt.return content
    | Provider.ToolCalls { calls; _ } when tools = None ->
        let content =
          "I attempted to use tools ("
          ^ String.concat ", "
              (List.map
                 (fun (tc : Provider.tool_call) -> tc.function_name)
                 calls)
          ^ ") but tools are disabled. Set security.tools_enabled to true in \
             ~/.clawq/config.json to enable them."
        in
        agent.history <-
          Provider.make_message ~role:"assistant" ~content :: agent.history;
        trim_history agent;
        Lwt.return content
    | Provider.ToolCalls { calls; _ } when iteration >= max_iters ->
        let content =
          "I've reached the maximum number of tool iterations. Here's what I \
           was trying to do: "
          ^ String.concat ", "
              (List.map
                 (fun (tc : Provider.tool_call) -> tc.function_name)
                 calls)
        in
        agent.history <-
          Provider.make_message ~role:"assistant" ~content :: agent.history;
        trim_history agent;
        Lwt.return content
    | Provider.ToolCalls { calls; _ } -> (
        let assistant_msg =
          {
            Provider.role = "assistant";
            content = "";
            tool_calls = calls;
            tool_call_id = None;
            name = None;
          }
        in
        agent.history <- assistant_msg :: agent.history;
        let* () =
          execute_tool_calls agent ~db ~audit_enabled ~session_key calls
        in
        match interrupt_check with
        | Some check -> (
            match check () with
            | Some _ ->
                let partial =
                  "[Agent was interrupted mid-task] --- [NOTE: interrupted by \
                   user]"
                in
                agent.history <-
                  Provider.make_message ~role:"assistant" ~content:partial
                  :: agent.history;
                trim_history agent;
                Lwt.return partial
            | None -> loop (iteration + 1))
        | None -> loop (iteration + 1))
  in
  loop 0

let turn_stream agent ~user_message ?db ?session_key ?interrupt_check
    ?runtime_context ?(history_prepared = false) ~on_chunk () =
  let open Lwt.Syntax in
  let* _compacted =
    if history_prepared then Lwt.return false
    else prepare_turn_history agent ~user_message ?db ()
  in
  let audit_enabled = agent.config.security.audit_enabled in
  let max_iters = agent.config.agent_defaults.max_tool_iterations in
  let tools = tools_json agent in
  (* Buffer to accumulate streamed content for interrupt annotation. *)
  let partial_buf = Buffer.create 256 in
  let wrapped_on_chunk chunk =
    let open Lwt.Syntax in
    match chunk with
    | Provider.Delta text -> (
        Buffer.add_string partial_buf text;
        let* () = on_chunk chunk in
        match interrupt_check with
        | Some check -> (
            match check () with
            | Some _ ->
                let note = " --- [NOTE: interrupted by user]" in
                let* () = on_chunk (Provider.Delta note) in
                Lwt.fail (Interrupted (Buffer.contents partial_buf))
            | None -> Lwt.return_unit)
        | None -> Lwt.return_unit)
    | Provider.Done | Provider.ThinkingDelta _ | Provider.ToolCallDelta _
    | Provider.ToolStart _ | Provider.ToolOutputDelta _ | Provider.ToolResult _
      ->
        on_chunk chunk
  in
  let resilient_stream config messages tools on_chunk =
    let res = config.Runtime_config.resilience in
    let open Lwt.Syntax in
    Buffer.clear partial_buf;
    let primary () =
      Provider.complete_stream ~config ~messages ?tools ~on_chunk ()
    in
    let with_optional_fallback () =
      match res.fallback_provider with
      | Some fb_name ->
          let fb_config = { config with default_provider = Some fb_name } in
          let primary_name, _, _ = Provider.select_provider ~config in
          let fallback_name, _, _ =
            Provider.select_provider ~config:fb_config
          in
          if fallback_name = primary_name then primary ()
          else
            Resilience.with_fallback ~primary ~fallback:(fun () ->
                Provider.complete_stream ~config:fb_config ~messages ?tools
                  ~on_chunk ())
      | None -> primary ()
    in
    let* timed =
      Resilience.with_timeout_retry ~timeout_s:res.timeout_s
        ~max_retries:res.max_retries ~base_delay_s:res.base_delay_s
        with_optional_fallback
    in
    match timed with Ok v -> Lwt.return v | Error e -> Lwt.fail_with e
  in
  let track_cost response =
    let usage, model =
      match response with
      | Provider.Text { usage; model; _ } -> (usage, model)
      | Provider.ToolCalls { usage; model; _ } -> (usage, model)
    in
    match (usage, session_key) with
    | Some (pt, ct), Some sid ->
        Cost_tracker.record_turn ~model ~prompt_tokens:pt ~completion_tokens:ct
          ~session_id:sid
    | _ -> ()
  in
  let rec loop iteration =
    Lwt.catch
      (fun () ->
        let* response =
          Lwt.catch
            (fun () ->
              let messages = build_messages ?runtime_context agent in
              resilient_stream agent.config messages tools wrapped_on_chunk)
            (fun exn ->
              if
                is_context_exhaustion_error (Printexc.to_string exn)
                && force_compress_history agent
              then begin
                Logs.warn (fun m ->
                    m
                      "Context exhaustion detected; force-compressed history, \
                       retrying turn");
                let messages = build_messages ?runtime_context agent in
                resilient_stream agent.config messages tools wrapped_on_chunk
              end
              else Lwt.fail exn)
        in
        track_cost response;
        match response with
        | Provider.Text { content; _ } ->
            agent.history <-
              Provider.make_message ~role:"assistant" ~content :: agent.history;
            trim_history agent;
            Lwt.return content
        | Provider.ToolCalls { calls; _ } when tools = None ->
            let content =
              "I attempted to use tools ("
              ^ String.concat ", "
                  (List.map
                     (fun (tc : Provider.tool_call) -> tc.function_name)
                     calls)
              ^ ") but tools are disabled."
            in
            agent.history <-
              Provider.make_message ~role:"assistant" ~content :: agent.history;
            trim_history agent;
            let* () = on_chunk (Provider.Delta content) in
            Lwt.return content
        | Provider.ToolCalls { calls; _ } when iteration >= max_iters ->
            let content =
              "I've reached the maximum number of tool iterations."
            in
            agent.history <-
              Provider.make_message ~role:"assistant" ~content :: agent.history;
            trim_history agent;
            let* () = on_chunk (Provider.Delta content) in
            Lwt.return content
        | Provider.ToolCalls { calls; _ } -> (
            let assistant_msg =
              {
                Provider.role = "assistant";
                content = "";
                tool_calls = calls;
                tool_call_id = None;
                name = None;
              }
            in
            agent.history <- assistant_msg :: agent.history;
            let* () =
              execute_tool_calls_stream agent ~db ~audit_enabled ~session_key
                ~on_chunk calls
            in
            match interrupt_check with
            | Some check -> (
                match check () with
                | Some _ ->
                    let partial = " --- [NOTE: interrupted by user]" in
                    agent.history <-
                      Provider.make_message ~role:"assistant" ~content:partial
                      :: agent.history;
                    trim_history agent;
                    let* () = on_chunk (Provider.Delta partial) in
                    Lwt.return partial
                | None -> loop (iteration + 1))
            | None -> loop (iteration + 1)))
      (fun exn ->
        match exn with
        | Interrupted partial ->
            let annotated = partial ^ " --- [NOTE: interrupted by user]" in
            agent.history <-
              Provider.make_message ~role:"assistant" ~content:annotated
              :: agent.history;
            trim_history agent;
            Lwt.return annotated
        | exn -> Lwt.fail exn)
  in
  loop 0
