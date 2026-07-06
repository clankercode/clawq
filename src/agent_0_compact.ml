include Agent_compact_support

let trim_history agent =
  let effective_max = effective_max_messages agent in
  let coq_input =
    Agent_loop_conformance.provider_to_coq_history agent.history
  in
  let coq_trimmed = Clawq_core.AgentLoop.trim_history effective_max coq_input in
  let coq_output =
    Clawq_core.AgentLoop.ensure_tool_group_integrity coq_trimmed
  in
  let provider_history =
    Agent_loop_conformance.coq_to_provider_history_with_names
      ~original_messages:agent.history coq_output
  in
  (* B626: the Coq-extracted integrity path strips orphan tool_call ids but
     does not drop the resulting fully-empty assistant messages or model
     provider_response_items_json/thinking. Re-run the native pass so the
     downstream Anthropic-format converters don't see empty assistants
     (which Anthropic 400s on). *)
  agent.history <- Message_history.ensure_tool_group_integrity provider_history;
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
    let result =
      Agent_loop_conformance.coq_to_provider_history_with_names
        ~original_messages:agent.history coq_output
    in
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
    let used_fallback = ref false in
    let compressed =
      if bounded = [] then begin
        used_fallback := true;
        Logs.warn (fun m ->
            m
              "force_compress_history: integrity check emptied history; \
               keeping raw compressed slice");
        let raw_result =
          Agent_loop_conformance.coq_to_provider_history_with_names
            ~original_messages:agent.history coq_compressed
        in
        if raw_result = [] then result else raw_result
      end
      else bounded
    in
    (* B626: re-run native pass to drop fully-empty assistants the Coq path
       leaves behind. Skip the re-run when we used the fallback above
       (otherwise we'd strip the same orphans the fallback intentionally
       kept to avoid an empty history). *)
    agent.history <-
      (if !used_fallback then compressed
       else Message_history.ensure_tool_group_integrity compressed);
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

let summarize_messages_with_config ?on_llm_call_debug config messages =
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
     conversation. Output only the summary, no preamble.\n\
     IMPORTANT: Do NOT include skill instruction bodies (messages starting \
     with \"[Skill: ...]\") in the summary. Only note which skills were loaded \
     by name — the system will automatically reload them.\n\n" ^ content
  in
  let msgs =
    [
      Provider.make_message ~role:"system"
        ~content:
          "You are a conversation summarizer. Be concise but preserve all \
           important context. Exclude skill instruction bodies from the \
           summary — just note skill names that were active.";
      Provider.make_message ~role:"user" ~content:prompt;
    ]
  in
  Lwt.catch
    (fun () ->
      let provider, _, _ = Provider.select_provider ~config () in
      let started_at = Unix.gettimeofday () in
      let* response = Provider.complete ~config ~messages:msgs () in
      let duration_s = Unix.gettimeofday () -. started_at in
      let* () =
        Agent_debug.notify ?on_llm_call_debug ~provider ~duration_s response
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

let summarize_messages ?on_llm_call_debug agent messages =
  summarize_messages_with_config ?on_llm_call_debug agent.config messages

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
                                    `String
                                      "Unique key for the memory (required)" );
                                ] );
                            ( "content",
                              `Assoc
                                [
                                  ("type", `String "string");
                                  ( "description",
                                    `String "Content to store (required)" );
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
                                  ( "description",
                                    `String "Search query (required)" );
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
                                    `String
                                      "Key of the memory to remove (required)"
                                  );
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
  let memory_forget_requires_session_error =
    "Error: memory_forget requires an active session context so the deleted \
     memory can be archived. Retry from an interactive session, or overwrite \
     the memory with memory_store instead of deleting it."
  in
  let args =
    try Yojson.Safe.from_string tc.arguments
    with _ ->
      Logs.warn (fun m ->
          m
            "dispatch_flush_tool_call '%s': failed to parse arguments as JSON \
             (raw: %s)"
            tc.function_name tc.arguments);
      `Assoc []
  in
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
          (* TODO(scoped-memory-audit): compaction flush tool calls currently
             read legacy global core memories because no memory scope is passed
             into dispatch_flush_tool_call. Thread scope context here before
             enabling this recall path for profiled room/thread compactions. *)
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
        else memory_forget_requires_session_error
    | "memory_list" ->
        let category =
          try args |> member "category" |> to_string with _ -> ""
        in
        (* TODO(scoped-memory-audit): this list path exposes legacy global core
           memory. Thread scope context into flush memory tools before enabling
           it for profiled room/thread compactions. *)
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

let flush_memories_before_compaction ?on_llm_call_debug ~config ~system_prompt
    ~db ~to_compact () =
  let open Lwt.Syntax in
  let n_msgs = List.length to_compact in
  Logs.info (fun m ->
      m "Pre-compaction memory flush: processing %d messages" n_msgs);
  let to_compact = ensure_tool_group_integrity to_compact in
  let to_compact = sanitize_messages_for_flush to_compact in
  if to_compact = [] then (
    Logs.debug (fun m ->
        m "Pre-compaction memory flush: no messages after sanitization");
    Lwt.return_unit)
  else
    let messages =
      ref
        ([ Provider.make_message ~role:"system" ~content:system_prompt ]
        @ to_compact
        @ [ Provider.make_message ~role:"user" ~content:flush_trigger_message ]
        )
    in
    let stored = ref 0 in
    let forgotten = ref 0 in
    let max_iters = 10 in
    let rec loop iter =
      if iter >= max_iters then Lwt.return_unit
      else
        let* response =
          let provider, _, _ = Provider.select_provider ~config () in
          let started_at = Unix.gettimeofday () in
          let* response =
            Provider.complete ~config ~messages:!messages
              ~tools:flush_memory_tool_schemas ()
          in
          let duration_s = Unix.gettimeofday () -. started_at in
          let* () =
            Agent_debug.notify ?on_llm_call_debug ~provider ~duration_s response
          in
          Lwt.return response
        in
        match response with
        | Provider.Text { content; _ } ->
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
                      if
                        not (String.starts_with ~prefix:"Error:" result.content)
                      then incr stored
                  | "memory_forget" ->
                      if
                        (not
                           (String.starts_with ~prefix:"Error:" result.content))
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
        m "Pre-compaction memory flush: stored %d, forgotten %d memories"
          !stored !forgotten);
    Lwt.return_unit

let compact_history_if_needed agent ?db ?on_llm_call_debug () =
  let open Lwt.Syntax in
  let effective_max = effective_max_messages agent in
  let len = List.length agent.history in
  let compaction_threshold = compaction_threshold_for_agent agent in
  let current_tokens = estimate_history_tokens agent.history in
  let cw = context_window_for_agent agent in
  if len > effective_max || current_tokens > compaction_threshold then begin
    let history_chrono = List.rev agent.history in
    let total = List.length history_chrono in
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
        (match db with
        | Some db
          when agent.config.memory.pre_compaction_flush
               && not (profiled_room_active agent) ->
            let snapshot = List.map Fun.id to_compact in
            let flush_config = agent.config in
            let system_prompt = agent.system_prompt in
            Lwt.async (fun () ->
                Lwt.catch
                  (fun () ->
                    flush_memories_before_compaction ~config:flush_config
                      ~system_prompt ~db ~to_compact:snapshot ?on_llm_call_debug
                      ())
                  (fun exn ->
                    Logs.warn (fun m ->
                        m "Pre-compaction memory flush failed: %s"
                          (Printexc.to_string exn));
                    Lwt.return_unit))
        | _ ->
            Logs.debug (fun m ->
                m "Pre-compaction memory flush: skipped (disabled)"));
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
          summarize_messages ?on_llm_call_debug agent first_half
        in
        let* summary2 =
          summarize_messages ?on_llm_call_debug agent second_half
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
        let skill_msgs = reload_skills_after_compaction ~to_compact ~to_keep in
        let scoped_msgs =
          preserve_scoped_memory_references_after_compaction ~to_compact
            ~to_keep
        in
        agent.history <-
          List.rev (skill_msgs @ [ summary_msg ] @ scoped_msgs @ to_keep);
        let post_tokens = estimate_history_tokens agent.history in
        Lwt.return_some
          { pre_tokens = current_tokens; post_tokens; context_window = cw }
      end
    end
  end
  else Lwt.return_none

let force_compact_history agent ?db ?compact_cbs ?on_llm_call_debug () =
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
      let* () =
        match
          ( db,
            compact_cbs,
            agent.config.memory.pre_compaction_flush
            && not (profiled_room_active agent) )
        with
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
                    ~system_prompt ~db ~to_compact:snapshot ?on_llm_call_debug
                    ())
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
                      ~system_prompt ~db ~to_compact:snapshot ?on_llm_call_debug
                      ())
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
            let* s = summarize_messages ?on_llm_call_debug agent first_half in
            let* () =
              cbs.on_step_done "Summarize (part 1)" (Unix.gettimeofday () -. t0)
            in
            Lwt.return s
        | None -> summarize_messages ?on_llm_call_debug agent first_half
      in
      let* summary2 =
        match compact_cbs with
        | Some cbs ->
            let t0 = Unix.gettimeofday () in
            let* () =
              cbs.on_step_start "Summarize (part 2)" "\xe2\x9c\x82\xef\xb8\x8f"
            in
            let* s = summarize_messages ?on_llm_call_debug agent second_half in
            let* () =
              cbs.on_step_done "Summarize (part 2)" (Unix.gettimeofday () -. t0)
            in
            Lwt.return s
        | None -> summarize_messages ?on_llm_call_debug agent second_half
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
      let skill_msgs = reload_skills_after_compaction ~to_compact ~to_keep in
      let scoped_msgs =
        preserve_scoped_memory_references_after_compaction ~to_compact ~to_keep
      in
      agent.history <-
        List.rev (skill_msgs @ [ summary_msg ] @ scoped_msgs @ to_keep);
      let post_tokens = estimate_history_tokens agent.history in
      Lwt.return_some { pre_tokens; post_tokens; context_window = cw }
    end
  end

(* --- Split-mode compaction for Session.compact (no-lock-during-LLM) --- *)

let plan_force_compact agent =
  let pre_tokens = estimate_history_tokens agent.history in
  let cw = context_window_for_agent agent in
  let history_chrono = List.rev agent.history in
  let total = List.length history_chrono in
  let keep = min compaction_keep_recent total in
  let compact_count = total - keep in
  if compact_count = 0 then None
  else begin
    let to_compact_raw =
      List.filteri (fun i _ -> i < compact_count) history_chrono
    in
    let to_keep_raw =
      List.filteri (fun i _ -> i >= compact_count) history_chrono
    in
    let to_compact, to_keep =
      adjust_split_for_tool_groups to_compact_raw to_keep_raw
    in
    if to_compact = [] then None
    else
      Some
        {
          cp_config = agent.config;
          cp_system_prompt = agent.system_prompt;
          cp_pre_tokens = pre_tokens;
          cp_context_window = cw;
          cp_to_compact = to_compact;
          cp_to_keep = to_keep;
          cp_history_length = List.length agent.history;
          cp_profiled_room = profiled_room_active agent;
        }
  end

let execute_compact_plan plan ?db ?compact_cbs ?on_llm_call_debug () =
  let open Lwt.Syntax in
  let config = plan.cp_config in
  let system_prompt = plan.cp_system_prompt in
  let to_compact = plan.cp_to_compact in
  let* () =
    match
      ( db,
        compact_cbs,
        config.memory.pre_compaction_flush && not plan.cp_profiled_room )
    with
    | Some db, Some cbs, true ->
        let snapshot = List.map Fun.id to_compact in
        let t0 = Unix.gettimeofday () in
        let* () = cbs.on_step_start "Save memories" "\xf0\x9f\xa7\xa0" in
        let* () =
          Lwt.catch
            (fun () ->
              flush_memories_before_compaction ~config ~system_prompt ~db
                ~to_compact:snapshot ?on_llm_call_debug ())
            (fun exn ->
              Logs.warn (fun m ->
                  m "Pre-compaction memory flush failed: %s"
                    (Printexc.to_string exn));
              Lwt.return_unit)
        in
        cbs.on_step_done "Save memories" (Unix.gettimeofday () -. t0)
    | Some db, None, true ->
        let snapshot = List.map Fun.id to_compact in
        Lwt.async (fun () ->
            Lwt.catch
              (fun () ->
                flush_memories_before_compaction ~config ~system_prompt ~db
                  ~to_compact:snapshot ?on_llm_call_debug ())
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
        let* s =
          summarize_messages_with_config ?on_llm_call_debug config first_half
        in
        let* () =
          cbs.on_step_done "Summarize (part 1)" (Unix.gettimeofday () -. t0)
        in
        Lwt.return s
    | None ->
        summarize_messages_with_config ?on_llm_call_debug config first_half
  in
  let* summary2 =
    match compact_cbs with
    | Some cbs ->
        let t0 = Unix.gettimeofday () in
        let* () =
          cbs.on_step_start "Summarize (part 2)" "\xe2\x9c\x82\xef\xb8\x8f"
        in
        let* s =
          summarize_messages_with_config ?on_llm_call_debug config second_half
        in
        let* () =
          cbs.on_step_done "Summarize (part 2)" (Unix.gettimeofday () -. t0)
        in
        Lwt.return s
    | None ->
        summarize_messages_with_config ?on_llm_call_debug config second_half
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
  Lwt.return merged_summary

let apply_compact_result agent plan ~summary =
  let current_len = List.length agent.history in
  let delta = current_len - plan.cp_history_length in
  if delta < 0 then begin
    Logs.warn (fun m ->
        m
          "Compact apply: history shrank (%d -> %d) during execution — session \
           may have been reset, skipping apply"
          plan.cp_history_length current_len);
    None
  end
  else begin
    let new_msgs =
      if delta > 0 then List.filteri (fun i _ -> i < delta) agent.history
      else []
    in
    let to_keep =
      match List.rev plan.cp_to_keep with
      | last :: rest ->
          List.rev
            (last :: List.rev (ensure_tool_group_integrity (List.rev rest)))
      | [] -> []
    in
    let summary_msg =
      Provider.make_message ~role:"assistant" ~content:summary
    in
    let skill_msgs =
      reload_skills_after_compaction ~to_compact:plan.cp_to_compact ~to_keep
    in
    let scoped_msgs =
      preserve_scoped_memory_references_after_compaction
        ~to_compact:plan.cp_to_compact ~to_keep
    in
    agent.history <-
      new_msgs @ List.rev (skill_msgs @ [ summary_msg ] @ scoped_msgs @ to_keep);
    let post_tokens = estimate_history_tokens agent.history in
    Some
      {
        pre_tokens = plan.cp_pre_tokens;
        post_tokens;
        context_window = plan.cp_context_window;
      }
  end

(* B710: Pre-switch context check. When switching to a model whose context
   window cannot fit the current history, force-compact history first. This
   prevents context overflow on the new model when possible. Temporarily sets
   the agent to use the new model for threshold calculation, then restores the
   original config if no compaction was needed. *)
let pre_switch_compact_if_needed agent ~new_model ?db ?on_llm_call_debug () =
  let open Lwt.Syntax in
  let current_tokens = estimate_history_tokens agent.history in
  let new_context_window =
    Runtime_config.context_window_for_model
      ~configured_limits:agent.config.model_context_limits new_model
  in
  match new_context_window with
  | None ->
      (* Unknown model context window — skip check, let normal flow handle it *)
      Lwt.return_none
  | Some new_cw ->
      if current_tokens <= new_cw then
        (* Current usage fits in new window *)
        Lwt.return_none
      else begin
        Logs.info (fun m ->
            m
              "B710: Pre-switch compaction needed — current tokens %d exceeds \
               new model '%s' context window %d"
              current_tokens new_model new_cw);
        (* Temporarily set the agent to use the new model so that
           context_window_for_agent returns the new window during compaction *)
        let original_config = agent.config in
        let cfg = agent.config in
        let ad = { cfg.agent_defaults with primary_model = new_model } in
        agent.config <- { cfg with agent_defaults = ad };
        let* result =
          Lwt.catch
            (fun () -> force_compact_history agent ?db ?on_llm_call_debug ())
            (fun exn ->
              Logs.warn (fun m ->
                  m "B710: Pre-switch compaction failed: %s"
                    (Printexc.to_string exn));
              (* Restore original config on failure *)
              agent.config <- original_config;
              Lwt.return_none)
        in
        match result with
        | Some info ->
            Logs.info (fun m ->
                m
                  "B710: Pre-switch compaction complete — %d -> %d tokens \
                   (window: %d)"
                  info.pre_tokens info.post_tokens info.context_window);
            if info.post_tokens > new_cw then
              Logs.warn (fun m ->
                  m
                    "B710: Pre-switch compaction still exceeds new model '%s' \
                     context window: %d tokens > %d"
                    new_model info.post_tokens new_cw);
            (* Keep the new model config — caller will apply it anyway *)
            Lwt.return_some info
        | None ->
            (* Compaction had nothing to do (no messages to compact).
               Restore original config — caller will apply the switch. *)
            agent.config <- original_config;
            Lwt.return_none
      end
