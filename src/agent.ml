type t = {
  mutable history : Provider.message list;
  mutable config : Runtime_config.t;
  mutable system_prompt : string;
  mutable observed_active_workspace_files : (string * string option) list;
  tool_registry : Tool_registry.t option;
  mutable compacted_mid_turn : bool;
}

exception Interrupted of string
exception Restart_requested

type compaction_info = {
  pre_tokens : int;
  post_tokens : int;
  context_window : int;
}

type compact_callbacks = {
  on_step_start : string -> string -> unit Lwt.t;
      (** [on_step_start name emoji] called when a compaction sub-step begins *)
  on_step_done : string -> float -> unit Lwt.t;
      (** [on_step_done name duration_s] called when a sub-step finishes *)
}

let string_contains_ci_small s sub =
  let sl = String.lowercase_ascii s in
  let subl = String.lowercase_ascii sub in
  let ls = String.length sl and lsub = String.length subl in
  if lsub > ls then false
  else
    let rec loop i =
      if i > ls - lsub then false
      else if String.sub sl i lsub = subl then true
      else loop (i + 1)
    in
    loop 0

let () =
  Resilience.register_non_retriable (function
    | Restart_requested | Interrupted _ -> true
    | Failure msg ->
        string_contains_ci_small msg
          "no tool call found for function call output"
    | _ -> false)

let restart_interrupt_token = "__clawq_restart__"
let queued_message_interrupt_token = "[queued inbound message]"

let is_queued_message_interrupt = function
  | Some reason when reason = queued_message_interrupt_token -> true
  | _ -> false

let active_workspace_files_for_config (config : Runtime_config.t) =
  let workspace = Runtime_config.effective_workspace config in
  let normalize_workspace_path path =
    let resolved =
      if Filename.is_relative path then Filename.concat workspace path else path
    in
    Path_util.normalize_path resolved
  in
  List.map
    (fun file -> (file, normalize_workspace_path file))
    config.prompt.workspace_files

let capture_active_workspace_file_state_for_config (config : Runtime_config.t) =
  active_workspace_files_for_config config
  |> List.map (fun (file, path) ->
      let digest =
        try Some (Digest.to_hex (Digest.file path)) with _ -> None
      in
      (file, digest))

let create ~config ?tool_registry () =
  let system_prompt = Prompt_builder.build ~config ~tool_registry () in
  {
    history = [];
    config;
    system_prompt;
    observed_active_workspace_files =
      capture_active_workspace_file_state_for_config config;
    tool_registry;
    compacted_mid_turn = false;
  }

let is_session_event_message (msg : Provider.message) = msg.role = "event"

let runtime_history_messages history =
  List.filter
    (fun (msg : Provider.message) -> not (is_session_event_message msg))
    history

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
    :: List.rev (runtime_history_messages agent.history)
  in
  match runtime_context with
  | Some block when String.trim block <> "" ->
      inject_runtime_context messages block
  | _ -> messages

let estimate_tokens content = (String.length content + 3) / 4

let estimate_message_tokens (m : Provider.message) =
  let content_tokens = estimate_tokens m.content in
  (* Tool-call arguments live in m.tool_calls when content = "" — must count
     them or the threshold check grossly underestimates context usage. *)
  let tool_call_tokens =
    List.fold_left
      (fun acc (tc : Provider.tool_call) ->
        acc + estimate_tokens tc.function_name + estimate_tokens tc.arguments)
      0 m.tool_calls
  in
  content_tokens + tool_call_tokens

let estimate_history_tokens history =
  List.fold_left
    (fun acc m -> acc + estimate_message_tokens m)
    0
    (runtime_history_messages history)

let context_window_for_agent agent =
  let model =
    Runtime_config.effective_primary_model agent.config.agent_defaults
  in
  match
    Runtime_config.context_window_for_model
      ~configured_limits:agent.config.model_context_limits model
  with
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

let assert_history_bound ~where agent =
  let len = List.length agent.history in
  let max_messages = effective_max_messages agent in
  if len > max_messages then
    invalid_arg
      (Printf.sprintf
         "AgentLoop invariant violated at %s: history length %d exceeds max %d"
         where len max_messages)

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

(* Adjust a chronological split so that orphaned tool-result messages at the
   start of [to_keep] are moved back into [to_compact]. A tool result is
   orphaned if the assistant message whose tool_calls produced it lives in
   [to_compact] (or is absent entirely). *)
let adjust_split_for_tool_groups = Message_history.adjust_split_for_tool_groups

(* Collect all tool_call ids present in assistant messages of [msgs]. *)
let collect_tool_call_ids = Message_history.collect_tool_call_ids

(* Collect all tool_call_ids referenced by tool-result messages in [msgs]. *)
let collect_tool_result_ids = Message_history.collect_tool_result_ids

(* Safety-net: remove orphaned tool results (no matching assistant tool_call)
   and strip dangling tool_calls from assistant messages (no matching tool
   result). Works on messages in any order. *)
let ensure_tool_group_integrity = Message_history.ensure_tool_group_integrity

let trim_history agent =
  let effective_max = effective_max_messages agent in
  let coq_input =
    Agent_loop_conformance.provider_to_coq_history agent.history
  in
  let coq_trimmed = Clawq_core.AgentLoop.trim_history effective_max coq_input in
  let coq_output =
    Clawq_core.AgentLoop.ensure_tool_group_integrity coq_trimmed
  in
  agent.history <- Agent_loop_conformance.coq_to_provider_history coq_output;
  assert_history_bound ~where:"trim_history" agent

let force_compress_history agent =
  let len = List.length agent.history in
  if len > context_recovery_min_history then begin
    let coq_input =
      Agent_loop_conformance.provider_to_coq_history agent.history
    in
    let coq_compressed =
      Clawq_core.AgentLoop.force_compress_history force_compress_keep coq_input
    in
    let coq_output =
      Clawq_core.AgentLoop.ensure_tool_group_integrity coq_compressed
    in
    let result = Agent_loop_conformance.coq_to_provider_history coq_output in
    let bounded =
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
        result
    in
    let compressed =
      if bounded = [] then begin
        Logs.warn (fun m ->
            m
              "force_compress_history: integrity check emptied history; \
               keeping raw compressed slice");
        let raw_result =
          Agent_loop_conformance.coq_to_provider_history coq_compressed
        in
        if raw_result = [] then result else raw_result
      end
      else bounded
    in
    agent.history <- compressed;
    assert_history_bound ~where:"force_compress_history" agent;
    true
  end
  else begin
    assert_history_bound ~where:"force_compress_history_noop" agent;
    false
  end

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

(* --- Pre-compaction memory flush ---------------------------------------- *)

(* Tool schemas and dispatch for the pre-compaction flush agent.
   Intentionally self-contained — duplicates a subset of tools_builtin.ml to
   avoid depending on tool_registry/tools_builtin, keeping the flush loop
   lightweight and free of side-effects from the full tool infrastructure. *)
let flush_memory_tool_schemas =
  `List
    [
      `Assoc
        [
          ("type", `String "function");
          ( "function",
            `Assoc
              [
                ("name", `String "memory_store");
                ( "description",
                  `String
                    "Store a persistent key-value memory that survives across \
                     sessions. Overwrites if the key already exists." );
                ( "parameters",
                  `Assoc
                    [
                      ("type", `String "object");
                      ( "properties",
                        `Assoc
                          [
                            ( "key",
                              `Assoc
                                [
                                  ("type", `String "string");
                                  ( "description",
                                    `String "Unique key for the memory" );
                                ] );
                            ( "content",
                              `Assoc
                                [
                                  ("type", `String "string");
                                  ("description", `String "Content to store");
                                ] );
                            ( "category",
                              `Assoc
                                [
                                  ("type", `String "string");
                                  ( "description",
                                    `String
                                      "Category for the memory (default: \
                                       general)" );
                                ] );
                          ] );
                      ("required", `List [ `String "key"; `String "content" ]);
                    ] );
              ] );
        ];
      `Assoc
        [
          ("type", `String "function");
          ( "function",
            `Assoc
              [
                ("name", `String "memory_recall");
                ( "description",
                  `String
                    "Search persistent memories by full-text query and return \
                     matching key-content pairs" );
                ( "parameters",
                  `Assoc
                    [
                      ("type", `String "object");
                      ( "properties",
                        `Assoc
                          [
                            ( "query",
                              `Assoc
                                [
                                  ("type", `String "string");
                                  ("description", `String "Search query");
                                ] );
                            ( "limit",
                              `Assoc
                                [
                                  ("type", `String "integer");
                                  ( "description",
                                    `String
                                      "Maximum number of results (default: 5)"
                                  );
                                ] );
                          ] );
                      ("required", `List [ `String "query" ]);
                    ] );
              ] );
        ];
      `Assoc
        [
          ("type", `String "function");
          ( "function",
            `Assoc
              [
                ("name", `String "memory_forget");
                ( "description",
                  `String "Delete a persistent memory by its exact key" );
                ( "parameters",
                  `Assoc
                    [
                      ("type", `String "object");
                      ( "properties",
                        `Assoc
                          [
                            ( "key",
                              `Assoc
                                [
                                  ("type", `String "string");
                                  ( "description",
                                    `String "Key of the memory to remove" );
                                ] );
                          ] );
                      ("required", `List [ `String "key" ]);
                    ] );
              ] );
        ];
      `Assoc
        [
          ("type", `String "function");
          ( "function",
            `Assoc
              [
                ("name", `String "memory_list");
                ( "description",
                  `String
                    "List all persistent memories, optionally filtered by \
                     category" );
                ( "parameters",
                  `Assoc
                    [
                      ("type", `String "object");
                      ( "properties",
                        `Assoc
                          [
                            ( "category",
                              `Assoc
                                [
                                  ("type", `String "string");
                                  ( "description",
                                    `String
                                      "Optional category filter (omit for all)"
                                  );
                                ] );
                          ] );
                      ("required", `List []);
                    ] );
              ] );
        ];
    ]

let dispatch_flush_tool_call ~db (tc : Provider.tool_call) =
  let open Yojson.Safe.Util in
  let args = try Yojson.Safe.from_string tc.arguments with _ -> `Assoc [] in
  let content =
    match tc.function_name with
    | "memory_store" ->
        let key = try args |> member "key" |> to_string with _ -> "" in
        let content =
          try args |> member "content" |> to_string with _ -> ""
        in
        let category =
          try args |> member "category" |> to_string with _ -> "general"
        in
        if key = "" then "Error: key is required"
        else if content = "" then "Error: content is required"
        else begin
          Memory.store_core ~db ~key ~content ~category ();
          Printf.sprintf "Stored memory: %s" key
        end
    | "memory_recall" ->
        let query = try args |> member "query" |> to_string with _ -> "" in
        let limit = try args |> member "limit" |> to_int with _ -> 5 in
        if query = "" then "Error: query is required"
        else
          let results = Memory.recall_core ~db ~query ~limit in
          if results = [] then "No matching memories found"
          else
            List.map
              (fun (key, content, category) ->
                Printf.sprintf "[%s] (%s): %s" key category content)
              results
            |> String.concat "\n"
    | "memory_forget" ->
        let key = try args |> member "key" |> to_string with _ -> "" in
        if key = "" then "Error: key is required"
        else if Memory.forget_core ~db ~key then
          Printf.sprintf "Deleted memory: %s" key
        else Printf.sprintf "No memory found with key: %s" key
    | "memory_list" ->
        let category =
          try args |> member "category" |> to_string with _ -> ""
        in
        let results = Memory.list_core ~db ~category () in
        if results = [] then "No memories found"
        else
          List.map
            (fun (key, content, cat) ->
              Printf.sprintf "[%s] (%s): %s" key cat content)
            results
          |> String.concat "\n"
    | other -> Printf.sprintf "Error: unknown tool '%s'" other
  in
  Provider.make_tool_result ~tool_call_id:tc.id ~name:tc.function_name ~content

let flush_trigger_message =
  "URGENT: The conversation above is about to be compacted — older messages \
   will be summarized and details permanently lost. You MUST save important \
   durable information to memory NOW or it will be gone forever.\n\n\
   Use the memory tools to:\n\
   1. First, list existing memories to avoid duplicates.\n\
   2. Store key facts, decisions, user preferences, commitments, and action \
   items.\n\
   3. Update outdated memories based on what you learned in this conversation.\n\
   4. Forget memories contradicted by new information.\n\n\
   Focus on cross-session value: preferences, decisions, project knowledge, \
   patterns.\n\
   Do NOT store ephemeral debugging details unless they reveal a reusable \
   insight.\n\n\
   Act now — this context will not be available after compaction."

let flush_memories_before_compaction ~config ~system_prompt ~db ~to_compact =
  let open Lwt.Syntax in
  let n_msgs = List.length to_compact in
  Logs.info (fun m ->
      m "Pre-compaction memory flush: processing %d messages" n_msgs);
  let to_compact = ensure_tool_group_integrity to_compact in
  (* Send the full history as context so memories from any part of the
     conversation can be extracted.  The large stable prefix is automatically
     cached by OpenAI after the first call (~50% discount on subsequent
     iterations in the loop), keeping per-iteration cost reasonable. *)
  let messages =
    ref
      ([ Provider.make_message ~role:"system" ~content:system_prompt ]
      @ to_compact
      @ [ Provider.make_message ~role:"user" ~content:flush_trigger_message ])
  in
  let stored = ref 0 in
  let forgotten = ref 0 in
  let max_iters = 10 in
  let rec loop iter =
    if iter >= max_iters then Lwt.return_unit
    else
      let* response =
        Provider.complete ~config ~messages:!messages
          ~tools:flush_memory_tool_schemas ()
      in
      match response with
      | Provider.Text { content; _ } ->
          (* Text response = done — log if non-empty *)
          if String.trim content <> "" then
            Logs.debug (fun m ->
                m "Pre-compaction flush final text: %s"
                  (if String.length content > 200 then
                     String.sub content 0 200 ^ "..."
                   else content));
          Lwt.return_unit
      | Provider.ToolCalls { calls; _ } ->
          let assistant_msg =
            {
              (Provider.make_message ~role:"assistant" ~content:"") with
              tool_calls = calls;
            }
          in
          let results =
            List.map
              (fun (tc : Provider.tool_call) ->
                let result = dispatch_flush_tool_call ~db tc in
                (match tc.function_name with
                | "memory_store" ->
                    if not (String.starts_with ~prefix:"Error:" result.content)
                    then incr stored
                | "memory_forget" ->
                    if
                      (not (String.starts_with ~prefix:"Error:" result.content))
                      && not (String.starts_with ~prefix:"No " result.content)
                    then incr forgotten
                | _ -> ());
                result)
              calls
          in
          messages := !messages @ [ assistant_msg ] @ results;
          loop (iter + 1)
  in
  let* () = loop 0 in
  Logs.info (fun m ->
      m "Pre-compaction memory flush: stored %d, forgotten %d memories" !stored
        !forgotten);
  Lwt.return_unit

let compact_history_if_needed agent ?db () =
  let open Lwt.Syntax in
  let effective_max = effective_max_messages agent in
  let len = List.length agent.history in
  let compaction_threshold = compaction_threshold_for_agent agent in
  let current_tokens = estimate_history_tokens agent.history in
  let cw = context_window_for_agent agent in
  if len > effective_max || current_tokens > compaction_threshold then begin
    let history_chrono = List.rev agent.history in
    let total = List.length history_chrono in
    (* Keep the most-recent messages verbatim; only summarise older ones. *)
    let keep = min compaction_keep_recent total in
    let compact_count = total - keep in
    if compact_count = 0 then Lwt.return_none
    else begin
      Logs.info (fun m ->
          m "Compacting history: %d messages -> summarise %d, keep %d recent"
            total compact_count keep);
      let to_compact_raw =
        List.filteri (fun i _ -> i < compact_count) history_chrono
      in
      let to_keep_raw =
        List.filteri (fun i _ -> i >= compact_count) history_chrono
      in
      let to_compact, to_keep =
        adjust_split_for_tool_groups to_compact_raw to_keep_raw
      in
      if to_compact = [] then Lwt.return_none
      else begin
        (* Launch background memory flush before compaction destroys detail *)
        (match db with
        | Some db when agent.config.memory.pre_compaction_flush ->
            let snapshot = List.map Fun.id to_compact in
            let flush_config = agent.config in
            let system_prompt = agent.system_prompt in
            Lwt.async (fun () ->
                Lwt.catch
                  (fun () ->
                    flush_memories_before_compaction ~config:flush_config
                      ~system_prompt ~db ~to_compact:snapshot)
                  (fun exn ->
                    Logs.warn (fun m ->
                        m "Pre-compaction memory flush failed: %s"
                          (Printexc.to_string exn));
                    Lwt.return_unit))
        | _ ->
            Logs.debug (fun m ->
                m "Pre-compaction memory flush: skipped (disabled)"));
        (* Preserve the chronologically last message from integrity stripping:
           when called mid-turn, the newest assistant message may have
           tool_calls whose results haven't been appended yet. *)
        let to_keep =
          match List.rev to_keep with
          | last :: rest ->
              List.rev
                (last :: List.rev (ensure_tool_group_integrity (List.rev rest)))
          | [] -> []
        in
        let mid = List.length to_compact / 2 in
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
        let post_tokens = estimate_history_tokens agent.history in
        Lwt.return_some
          { pre_tokens = current_tokens; post_tokens; context_window = cw }
      end
    end
  end
  else Lwt.return_none

let force_compact_history agent ?db ?compact_cbs () =
  let open Lwt.Syntax in
  let pre_tokens = estimate_history_tokens agent.history in
  let cw = context_window_for_agent agent in
  let history_chrono = List.rev agent.history in
  let total = List.length history_chrono in
  let keep = min compaction_keep_recent total in
  let compact_count = total - keep in
  if compact_count = 0 then Lwt.return_none
  else begin
    Logs.info (fun m ->
        m
          "Force-compacting history: %d messages -> summarise %d, keep %d \
           recent"
          total compact_count keep);
    let to_compact_raw =
      List.filteri (fun i _ -> i < compact_count) history_chrono
    in
    let to_keep_raw =
      List.filteri (fun i _ -> i >= compact_count) history_chrono
    in
    let to_compact, to_keep =
      adjust_split_for_tool_groups to_compact_raw to_keep_raw
    in
    if to_compact = [] then Lwt.return_none
    else begin
      (* Launch memory flush before compaction destroys detail.
         With compact_cbs: run synchronously so progress can be tracked.
         Without compact_cbs: run in background (existing behaviour). *)
      let* () =
        match (db, compact_cbs, agent.config.memory.pre_compaction_flush) with
        | Some db, Some cbs, true ->
            let snapshot = List.map Fun.id to_compact in
            let flush_config = agent.config in
            let system_prompt = agent.system_prompt in
            let t0 = Unix.gettimeofday () in
            let* () = cbs.on_step_start "Save memories" "\xf0\x9f\xa7\xa0" in
            let* () =
              Lwt.catch
                (fun () ->
                  flush_memories_before_compaction ~config:flush_config
                    ~system_prompt ~db ~to_compact:snapshot)
                (fun exn ->
                  Logs.warn (fun m ->
                      m "Pre-compaction memory flush failed: %s"
                        (Printexc.to_string exn));
                  Lwt.return_unit)
            in
            cbs.on_step_done "Save memories" (Unix.gettimeofday () -. t0)
        | Some db, None, true ->
            let snapshot = List.map Fun.id to_compact in
            let flush_config = agent.config in
            let system_prompt = agent.system_prompt in
            Lwt.async (fun () ->
                Lwt.catch
                  (fun () ->
                    flush_memories_before_compaction ~config:flush_config
                      ~system_prompt ~db ~to_compact:snapshot)
                  (fun exn ->
                    Logs.warn (fun m ->
                        m "Pre-compaction memory flush failed: %s"
                          (Printexc.to_string exn));
                    Lwt.return_unit));
            Lwt.return_unit
        | _ ->
            Logs.debug (fun m ->
                m "Pre-compaction memory flush: skipped (disabled)");
            Lwt.return_unit
      in
      (* Preserve the chronologically last message from integrity stripping:
         when called mid-turn (e.g. via compact_history tool), the newest
         assistant message has tool_calls whose results haven't been appended
         yet.  Stripping those calls causes "No tool output found" errors. *)
      let to_keep =
        match List.rev to_keep with
        | last :: rest ->
            List.rev
              (last :: List.rev (ensure_tool_group_integrity (List.rev rest)))
        | [] -> []
      in
      let mid = List.length to_compact / 2 in
      let first_half = List.filteri (fun i _ -> i < mid) to_compact in
      let second_half = List.filteri (fun i _ -> i >= mid) to_compact in
      let* summary1 =
        match compact_cbs with
        | Some cbs ->
            let t0 = Unix.gettimeofday () in
            let* () =
              cbs.on_step_start "Summarize (part 1)" "\xe2\x9c\x82\xef\xb8\x8f"
            in
            let* s = summarize_messages agent first_half in
            let* () =
              cbs.on_step_done "Summarize (part 1)" (Unix.gettimeofday () -. t0)
            in
            Lwt.return s
        | None -> summarize_messages agent first_half
      in
      let* summary2 =
        match compact_cbs with
        | Some cbs ->
            let t0 = Unix.gettimeofday () in
            let* () =
              cbs.on_step_start "Summarize (part 2)" "\xe2\x9c\x82\xef\xb8\x8f"
            in
            let* s = summarize_messages agent second_half in
            let* () =
              cbs.on_step_done "Summarize (part 2)" (Unix.gettimeofday () -. t0)
            in
            Lwt.return s
        | None -> summarize_messages agent second_half
      in
      let merged_summary =
        Printf.sprintf
          "[Conversation history compacted]\n\n\
           Earlier context:\n\
           %s\n\n\
           Recent context:\n\
           %s"
          summary1 summary2
      in
      let summary_msg =
        Provider.make_message ~role:"assistant" ~content:merged_summary
      in
      agent.history <- List.rev (summary_msg :: to_keep);
      let post_tokens = estimate_history_tokens agent.history in
      Lwt.return_some { pre_tokens; post_tokens; context_window = cw }
    end
  end

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

let active_workspace_files agent =
  active_workspace_files_for_config agent.config

let capture_active_workspace_file_state agent =
  capture_active_workspace_file_state_for_config agent.config

let changed_active_workspace_files before after =
  List.filter_map
    (fun (file, before_digest) ->
      let after_digest =
        match List.assoc_opt file after with Some d -> d | None -> None
      in
      if before_digest <> after_digest then Some file else None)
    before

let dedup_preserve_order items =
  let rec loop seen acc = function
    | [] -> List.rev acc
    | item :: rest when List.mem item seen -> loop seen acc rest
    | item :: rest -> loop (item :: seen) (item :: acc) rest
  in
  loop [] [] items

let workspace_refresh_event filenames =
  Provider.make_message ~role:"event"
    ~content:
      (Printf.sprintf
         "[workspace context refreshed after active workspace file update: %s]"
         (String.concat ", " filenames))

type workspace_refresh_observation = {
  message : Provider.message option;
  after_state : (string * string option) list;
}

let active_workspace_refresh_targets_from_call agent (tc : Provider.tool_call)
    result =
  if String.starts_with ~prefix:"Error:" result then None
  else
    let configured = active_workspace_files agent in
    let find_configured_file resolved_path =
      List.find_map
        (fun (file, configured_path) ->
          if resolved_path = configured_path then Some file else None)
        configured
    in
    try
      let open Yojson.Safe.Util in
      let args = Yojson.Safe.from_string tc.arguments in
      match tc.function_name with
      | "doc_write" ->
          let filename = args |> member "filename" |> to_string in
          if List.mem filename agent.config.prompt.workspace_files then
            Some [ filename ]
          else None
      | "file_write" | "file_append" | "file_edit" | "file_edit_lines" ->
          let workspace = Runtime_config.effective_workspace agent.config in
          let normalize_workspace_path path =
            let resolved =
              if Filename.is_relative path then Filename.concat workspace path
              else path
            in
            Path_util.normalize_path resolved
          in
          let path = args |> member "path" |> to_string in
          let resolved_path = normalize_workspace_path path in
          Option.map (fun file -> [ file ]) (find_configured_file resolved_path)
      | _ -> None
    with _ -> None

let observe_workspace_refresh agent tc result ~before_active_workspace_files =
  let direct_targets =
    match active_workspace_refresh_targets_from_call agent tc result with
    | Some files -> files
    | None -> []
  in
  let after_state = capture_active_workspace_file_state agent in
  let changed_targets =
    changed_active_workspace_files before_active_workspace_files after_state
  in
  let refreshed_files =
    dedup_preserve_order (direct_targets @ changed_targets)
  in
  let message =
    if String.starts_with ~prefix:"Error:" result then None
    else
      match refreshed_files with
      | [] -> None
      | filenames -> Some (workspace_refresh_event filenames)
  in
  { message; after_state }

let sync_observed_active_workspace_files agent =
  agent.observed_active_workspace_files <-
    capture_active_workspace_file_state agent

let restore_observed_active_workspace_files agent
    observed_active_workspace_files =
  let current_state = capture_active_workspace_file_state agent in
  agent.observed_active_workspace_files <-
    List.map
      (fun (file, current_digest) ->
        let restored_digest =
          match List.assoc_opt file observed_active_workspace_files with
          | Some digest -> digest
          | None -> current_digest
        in
        (file, restored_digest))
      current_state

let note_external_workspace_refresh_if_needed agent =
  let before_state = agent.observed_active_workspace_files in
  let after_state = capture_active_workspace_file_state agent in
  agent.observed_active_workspace_files <- after_state;
  match changed_active_workspace_files before_state after_state with
  | [] -> None
  | filenames ->
      let refresh_msg = workspace_refresh_event filenames in
      agent.history <- refresh_msg :: agent.history;
      Some refresh_msg

let append_tool_history agent tool_msg refresh_msg_opt =
  agent.history <- tool_msg :: agent.history;
  match refresh_msg_opt with
  | Some refresh_msg ->
      agent.history <- refresh_msg :: agent.history;
      agent.observed_active_workspace_files <-
        capture_active_workspace_file_state agent
  | None -> ()

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

(* Execute tool calls in order so workspace refresh events can attribute active
   prompt-file updates to the specific tool call that triggered them. *)
let execute_tool_calls_stream agent ~db ~audit_enabled ~session_key
    ?interrupt_check ~on_chunk calls =
  let open Lwt.Syntax in
  let sk_tag = match session_key with Some s -> "[" ^ s ^ "] " | None -> "" in
  let interrupted = ref false in
  let check_interrupt () =
    if !interrupted then true
    else
      match interrupt_check with
      | Some check -> (
          match check () with
          | Some reason when reason = queued_message_interrupt_token -> false
          | Some _ ->
              interrupted := true;
              true
          | None -> false)
      | None -> false
  in
  let* results =
    Lwt_list.map_s
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
        if check_interrupt () then begin
          Logs.info (fun m ->
              m "%sSkipping tool %s (interrupted)" sk_tag tc.function_name);
          (match (db, audit_enabled, session_key) with
          | Some db, true, Some sk ->
              Audit.log ~db
                (ToolResult
                   {
                     session_key = sk;
                     tool_name = tc.function_name;
                     success = false;
                   })
          | _ -> ());
          let result_msg =
            Provider.make_tool_result ~tool_call_id:tc.id ~name:tc.function_name
              ~content:"[skipped: interrupted by user]"
          in
          let* () =
            on_chunk
              (Provider.ToolResult
                 {
                   id = tc.id;
                   name = tc.function_name;
                   result = "[skipped: interrupted by user]";
                   is_error = false;
                 })
          in
          Lwt.return (tc, result_msg, None)
        end
        else
          let before_active_workspace_files =
            capture_active_workspace_file_state agent
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
                                interrupt_check;
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
                              ("Error invoking tool: " ^ Printexc.to_string exn))
                    )
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
            not (String.starts_with ~prefix:"Error:" result_for_event)
          in
          (match (db, audit_enabled, session_key) with
          | Some db, true, Some sk ->
              Audit.log ~db
                (ToolResult
                   { session_key = sk; tool_name = tc.function_name; success })
          | _ -> ());
          let preview =
            let limit = if success then 200 else 1000 in
            if String.length result > limit then
              String.sub result 0 limit ^ "..."
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
          let refresh =
            observe_workspace_refresh agent tc result
              ~before_active_workspace_files
          in
          if Option.is_some refresh.message then
            agent.observed_active_workspace_files <- refresh.after_state;
          Lwt.return (tc, result_msg, refresh.message))
      calls
  in
  List.iter
    (fun ((_tc : Provider.tool_call), tool_msg, refresh_msg) ->
      append_tool_history agent tool_msg refresh_msg)
    results;
  Lwt.return_unit

let execute_tool_calls agent ~db ~audit_enabled ~session_key ?interrupt_check
    calls =
  let open Lwt.Syntax in
  let sk_tag = match session_key with Some s -> "[" ^ s ^ "] " | None -> "" in
  let interrupted = ref false in
  let check_interrupt () =
    if !interrupted then true
    else
      match interrupt_check with
      | Some check -> (
          match check () with
          | Some reason when reason = queued_message_interrupt_token -> false
          | Some _ ->
              interrupted := true;
              true
          | None -> false)
      | None -> false
  in
  let* results =
    Lwt_list.map_s
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
        if check_interrupt () then begin
          Logs.info (fun m ->
              m "%sSkipping tool %s (interrupted)" sk_tag tc.function_name);
          (match (db, audit_enabled, session_key) with
          | Some db, true, Some sk ->
              Audit.log ~db
                (ToolResult
                   {
                     session_key = sk;
                     tool_name = tc.function_name;
                     success = false;
                   })
          | _ -> ());
          Lwt.return
            ( tc,
              Provider.make_tool_result ~tool_call_id:tc.id
                ~name:tc.function_name ~content:"[skipped: interrupted by user]",
              None )
        end
        else
          let before_active_workspace_files =
            capture_active_workspace_file_state agent
          in
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
                              {
                                Tool.session_key;
                                send_progress = None;
                                interrupt_check;
                              }
                            in
                            tool.invoke ~context args)
                          (fun exn ->
                            Lwt.return
                              ("Error invoking tool: " ^ Printexc.to_string exn))
                    )
              in
              let result_for_history =
                truncate_for_history result ~max_chars:max_tool_result_chars
              in
              Lwt.return
                (Provider.make_tool_result ~tool_call_id:tc.id
                   ~name:tc.function_name ~content:result_for_history)
          in
          let result = result_msg.Provider.content in
          let success = not (String.starts_with ~prefix:"Error:" result) in
          (match (db, audit_enabled, session_key) with
          | Some db, true, Some sk ->
              Audit.log ~db
                (ToolResult
                   { session_key = sk; tool_name = tc.function_name; success })
          | _ -> ());
          let truncated =
            let limit = if success then 200 else 1000 in
            if String.length result > limit then
              String.sub result 0 limit ^ "..."
            else result
          in
          if success then
            Logs.info (fun m ->
                m "%sTool result: %s -> %s" sk_tag tc.function_name truncated)
          else
            Logs.warn (fun m ->
                m "%sTool error: %s -> %s" sk_tag tc.function_name truncated);
          let refresh =
            observe_workspace_refresh agent tc result
              ~before_active_workspace_files
          in
          if Option.is_some refresh.message then
            agent.observed_active_workspace_files <- refresh.after_state;
          Lwt.return (tc, result_msg, refresh.message))
      calls
  in
  (* Results already reflect execution order; append deterministically. *)
  List.iter
    (fun ((_tc : Provider.tool_call), tool_msg, refresh_msg) ->
      append_tool_history agent tool_msg refresh_msg)
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

let filter_content_parts_for_model config content_parts =
  let model_id = config.Runtime_config.agent_defaults.primary_model in
  match Models_catalog.find_by_id model_id with
  | Some model_info when not model_info.Models_catalog.supports_vision ->
      let filtered =
        List.filter
          (function
            | Provider.Image_base64 _ -> false | Provider.Text _ -> true)
          content_parts
      in
      if List.length filtered < List.length content_parts then
        Logs.info (fun m ->
            m
              "Model %s does not support vision; filtering out %d image(s) \
               from message"
              model_id
              (List.length content_parts - List.length filtered));
      filtered
  | _ -> content_parts

let prepare_turn_history agent ~user_message ?(content_parts = [])
    ?(workspace_refresh_checked = false) ?db () =
  let open Lwt.Syntax in
  let* () =
    if workspace_refresh_checked then Lwt.return_unit
    else
      let _ = note_external_workspace_refresh_if_needed agent in
      Lwt.return_unit
  in
  let* () =
    match db with
    | Some db -> inject_search_context agent ~db ~user_message
    | None -> Lwt.return_unit
  in
  let filtered_content_parts =
    filter_content_parts_for_model agent.config content_parts
  in
  let user_msg =
    match filtered_content_parts with
    | [] -> Provider.make_message ~role:"user" ~content:user_message
    | parts ->
        Provider.make_message_with_parts ~role:"user" ~content:user_message
          ~content_parts:(Provider.Text user_message :: parts)
  in
  agent.history <- user_msg :: agent.history;
  let* compacted = compact_history_if_needed agent ?db () in
  trim_history agent;
  Lwt.return compacted

let turn agent ~user_message ?db ?session_key ?interrupt_check ?inject_messages
    ?runtime_context ?(history_prepared = false) ?on_history_update () =
  let is_restart_interrupt = function
    | Some reason when reason = restart_interrupt_token -> true
    | _ -> false
  in
  let is_queued_message_interrupt = function
    | Some reason when reason = queued_message_interrupt_token -> true
    | _ -> false
  in
  let open Lwt.Syntax in
  let* _compaction_info =
    if history_prepared then Lwt.return_none
    else prepare_turn_history agent ~user_message ?db ()
  in
  let audit_enabled = agent.config.security.audit_enabled in
  let max_iters = agent.config.agent_defaults.max_tool_iterations in
  let tools = tools_json agent in
  (* Quota state: fetch from cache once per turn (non-blocking). *)
  let quota_states_opt =
    let qs = Provider_quota.get_all_cached () in
    if qs = [] then None else Some qs
  in
  (* Inject a quota notice into system prompt context when provider is >= 70%
     used.  runtime_context is shadowed here so the notice is visible to the
     LLM without altering the caller-provided context string. *)
  let runtime_context =
    match quota_states_opt with
    | None -> runtime_context
    | Some qs -> (
        let pn, _, _ =
          Provider.select_provider ~config:agent.config
            ?quota_states:quota_states_opt ()
        in
        let notice =
          match List.assoc_opt pn qs with
          | Some pq -> Provider_quota.quota_notice pq
          | None -> None
        in
        match notice with
        | None -> runtime_context
        | Some n ->
            Some
              (match runtime_context with
              | None -> n
              | Some rc -> rc ^ "\n\n" ^ n))
  in
  let resilient_complete config messages tools =
    let res = config.Runtime_config.resilience in
    let open Lwt.Syntax in
    let primary () =
      Provider.complete ~config ~messages ?tools ?session_key
        ?quota_states:quota_states_opt ()
    in
    let with_optional_fallback () =
      match res.fallback_provider with
      | Some fb_name ->
          let fb_config = { config with default_provider = Some fb_name } in
          let primary_name, _, _ = Provider.select_provider ~config () in
          let fallback_name, _, _ =
            Provider.select_provider ~config:fb_config ()
          in
          if fallback_name = primary_name then primary ()
          else
            Resilience.with_fallback ~primary ~fallback:(fun () ->
                Provider.complete ~config:fb_config ~messages ?tools
                  ?session_key ())
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
    | Some (pt, ct), Some sid -> (
        Cost_tracker.record_turn ~model ~prompt_tokens:pt ~completion_tokens:ct
          ~session_id:sid;
        match db with
        | Some db ->
            let pname, _, _ =
              Provider.select_provider ~config:agent.config ()
            in
            let cost_usd_opt =
              match Cost_tracker.lookup_pricing model with
              | None -> None
              | Some _ ->
                  Some
                    (Cost_tracker.calculate_cost ~model ~prompt_tokens:pt
                       ~completion_tokens:ct)
            in
            Request_stats.record ~db ~session_key:sid ~provider:pname ~model
              ~prompt_tokens:pt ~completion_tokens:ct ?cost_usd:cost_usd_opt ()
        | None -> ())
    | _ -> ()
  in
  let fire_history_update len_before =
    match on_history_update with
    | Some cb ->
        let len_after = List.length agent.history in
        if len_after > len_before then begin
          let reversed = List.rev agent.history in
          let new_msgs = List.filteri (fun i _ -> i >= len_before) reversed in
          cb new_msgs
        end
        else Lwt.return_unit
    | None -> Lwt.return_unit
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
    | Provider.Text { content; provider_response_items_json; _ } ->
        agent.history <-
          Provider.make_message_full ~provider_response_items_json
            ~role:"assistant" ~content
          :: agent.history;
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
    | Provider.ToolCalls { calls; provider_response_items_json; _ } -> (
        let len_before_tool_loop = List.length agent.history in
        let assistant_msg =
          {
            Provider.role = "assistant";
            content = "";
            content_parts = [];
            tool_calls = calls;
            tool_call_id = None;
            name = None;
            provider_response_items_json;
          }
        in
        agent.history <- assistant_msg :: agent.history;
        let* () =
          execute_tool_calls agent ~db ~audit_enabled ~session_key
            ?interrupt_check calls
        in
        (match inject_messages with
        | Some get_msgs ->
            let msgs = get_msgs () in
            List.iter
              (fun msg ->
                agent.history <-
                  Provider.make_message ~role:"user" ~content:msg
                  :: agent.history)
              msgs
        | None -> ());
        let* () = fire_history_update len_before_tool_loop in
        match interrupt_check with
        | Some check -> (
            match check () with
            | interrupt when is_restart_interrupt interrupt ->
                Lwt.fail Restart_requested
            | interrupt when is_queued_message_interrupt interrupt ->
                loop (iteration + 1)
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
    ?inject_messages ?runtime_context ?(history_prepared = false)
    ?on_history_update ~on_chunk () =
  let is_restart_interrupt = function
    | Some reason when reason = restart_interrupt_token -> true
    | _ -> false
  in
  let open Lwt.Syntax in
  let* _compaction_info =
    if history_prepared then Lwt.return_none
    else prepare_turn_history agent ~user_message ?db ()
  in
  let audit_enabled = agent.config.security.audit_enabled in
  let max_iters = agent.config.agent_defaults.max_tool_iterations in
  let tools = tools_json agent in
  (* Quota state: fetch from cache once per turn (non-blocking). *)
  let quota_states_opt =
    let qs = Provider_quota.get_all_cached () in
    if qs = [] then None else Some qs
  in
  (* Inject quota notice into system prompt context when provider >= 70% used. *)
  let runtime_context =
    match quota_states_opt with
    | None -> runtime_context
    | Some qs -> (
        let pn, _, _ =
          Provider.select_provider ~config:agent.config
            ?quota_states:quota_states_opt ()
        in
        let notice =
          match List.assoc_opt pn qs with
          | Some pq -> Provider_quota.quota_notice pq
          | None -> None
        in
        match notice with
        | None -> runtime_context
        | Some n ->
            Some
              (match runtime_context with
              | None -> n
              | Some rc -> rc ^ "\n\n" ^ n))
  in
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
            | interrupt when is_restart_interrupt interrupt ->
                Lwt.fail Restart_requested
            | interrupt when is_queued_message_interrupt interrupt ->
                Lwt.return_unit
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
      Provider.complete_stream ~config ~messages ?tools ?session_key
        ?quota_states:quota_states_opt ~on_chunk ()
    in
    let with_optional_fallback () =
      match res.fallback_provider with
      | Some fb_name ->
          let fb_config = { config with default_provider = Some fb_name } in
          let primary_name, _, _ = Provider.select_provider ~config () in
          let fallback_name, _, _ =
            Provider.select_provider ~config:fb_config ()
          in
          if fallback_name = primary_name then primary ()
          else
            Resilience.with_fallback ~primary ~fallback:(fun () ->
                Provider.complete_stream ~config:fb_config ~messages ?tools
                  ?session_key ~on_chunk ())
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
    | Some (pt, ct), Some sid -> (
        Cost_tracker.record_turn ~model ~prompt_tokens:pt ~completion_tokens:ct
          ~session_id:sid;
        match db with
        | Some db ->
            let pname, _, _ =
              Provider.select_provider ~config:agent.config ()
            in
            let cost_usd_opt =
              match Cost_tracker.lookup_pricing model with
              | None -> None
              | Some _ ->
                  Some
                    (Cost_tracker.calculate_cost ~model ~prompt_tokens:pt
                       ~completion_tokens:ct)
            in
            Request_stats.record ~db ~session_key:sid ~provider:pname ~model
              ~prompt_tokens:pt ~completion_tokens:ct ?cost_usd:cost_usd_opt ()
        | None -> ())
    | _ -> ()
  in
  let fire_history_update len_before =
    match on_history_update with
    | Some cb ->
        let len_after = List.length agent.history in
        if len_after > len_before then begin
          let reversed = List.rev agent.history in
          let new_msgs = List.filteri (fun i _ -> i >= len_before) reversed in
          cb new_msgs
        end
        else Lwt.return_unit
    | None -> Lwt.return_unit
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
        | Provider.Text { content; provider_response_items_json; _ } ->
            agent.history <-
              Provider.make_message_full ~provider_response_items_json
                ~role:"assistant" ~content
              :: agent.history;
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
        | Provider.ToolCalls { calls; provider_response_items_json; _ } -> (
            let len_before_tool_loop = List.length agent.history in
            let assistant_msg =
              {
                Provider.role = "assistant";
                content = "";
                content_parts = [];
                tool_calls = calls;
                tool_call_id = None;
                name = None;
                provider_response_items_json;
              }
            in
            agent.history <- assistant_msg :: agent.history;
            let* () =
              execute_tool_calls_stream agent ~db ~audit_enabled ~session_key
                ?interrupt_check ~on_chunk calls
            in
            (match inject_messages with
            | Some get_msgs ->
                let msgs = get_msgs () in
                List.iter
                  (fun msg ->
                    agent.history <-
                      Provider.make_message ~role:"user" ~content:msg
                      :: agent.history)
                  msgs
            | None -> ());
            let* () = fire_history_update len_before_tool_loop in
            match interrupt_check with
            | Some check -> (
                match check () with
                | interrupt when is_restart_interrupt interrupt ->
                    Lwt.fail Restart_requested
                | interrupt when is_queued_message_interrupt interrupt ->
                    loop (iteration + 1)
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
        | Restart_requested -> Lwt.fail Restart_requested
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
