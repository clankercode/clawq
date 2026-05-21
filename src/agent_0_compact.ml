type t = {
  mutable history : Provider.message list;
  mutable config : Runtime_config.t;
  mutable system_prompt : string;
  mutable observed_active_workspace_files : (string * string option) list;
  mutable last_request_history_len : int option;
  tool_registry : Tool_registry.t option;
  agent_template : Agent_template.t option;
  mutable compacted_mid_turn : bool;
  mutable effective_cwd : string option;
  mutable project_docs_content : string option;
  mutable project_docs_digests : string list;
  mutable project_docs_subdir_digests : string list;
  mutable project_docs_git_root : string option;
  mutable project_doc_dirs_seen : (string, bool) Hashtbl.t;
  mutable on_project_doc_loaded : (string -> unit Lwt.t) option;
  (* B622: track consecutive (tool_name, sorted-missing-params) repeats so
     we can escalate the validation error after the model fails to honor
     it. Reset on any successful tool call OR a tool call with a different
     missing-params key. *)
  mutable last_missing_required_key : string option;
  mutable last_missing_required_count : int;
}

exception Interrupted of string
exception Restart_requested

type compaction_info = {
  pre_tokens : int;
  post_tokens : int;
  context_window : int;
}

type compact_plan = {
  cp_config : Runtime_config.t;
  cp_system_prompt : string;
  cp_pre_tokens : int;
  cp_context_window : int;
  cp_to_compact : Provider.message list;
  cp_to_keep : Provider.message list;
  cp_history_length : int;
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

let is_session_event_message (msg : Provider.message) = msg.role = "event"

let runtime_history_messages history =
  List.filter
    (fun (msg : Provider.message) -> not (is_session_event_message msg))
    history

let estimate_tokens content = (String.length content + 3) / 4

let estimate_message_tokens (m : Provider.message) =
  let content_tokens = estimate_tokens m.content in
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

(* Number of most-recent messages kept verbatim after compaction. *)
let compaction_keep_recent = 20

(* Maximum number of skills to auto-reload with full instructions after
   compaction. Skills beyond this cap are listed by name only. *)
let max_skills_to_autoload = 4

(* Extract skill names from compacted messages that aren't already in kept
   messages, so we can auto-reload them after compaction. *)
let skills_to_reload ~to_compact ~to_keep =
  let kept_skills = Hashtbl.create 4 in
  List.iter
    (fun (msg : Provider.message) ->
      if msg.role = "system" then
        match Skill_dedup.extract_skill_name_from_injection msg.content with
        | Some name -> Hashtbl.replace kept_skills name ()
        | None -> ())
    to_keep;
  let seen = Hashtbl.create 4 in
  List.filter_map
    (fun (msg : Provider.message) ->
      if msg.role = "system" then
        match Skill_dedup.extract_skill_name_from_injection msg.content with
        | Some name
          when (not (Hashtbl.mem kept_skills name))
               && not (Hashtbl.mem seen name) ->
            Hashtbl.replace seen name ();
            Some name
        | _ -> None
      else None)
    to_compact

let find_skill_for_reload_fn : (string -> (string * string) option) ref =
  ref (fun _name -> None)

let reload_skills_after_compaction ~to_compact ~to_keep =
  let names = skills_to_reload ~to_compact ~to_keep in
  let resolved =
    List.filter_map
      (fun name ->
        match !find_skill_for_reload_fn name with
        | Some (_desc, instructions) -> Some (name, instructions)
        | None ->
            Logs.debug (fun m ->
                m "Skill '%s' no longer available for post-compaction reload"
                  name);
            None)
      names
  in
  let auto, overflow =
    let rec split n acc = function
      | [] -> (List.rev acc, [])
      | rest when n <= 0 -> (List.rev acc, rest)
      | x :: xs -> split (n - 1) (x :: acc) xs
    in
    split max_skills_to_autoload [] resolved
  in
  let skill_msgs =
    List.map
      (fun (name, instructions) ->
        let content =
          Printf.sprintf "[Skill: %s (autoloaded after compaction)]\n%s" name
            instructions
        in
        Provider.make_message ~role:"system" ~content)
      auto
  in
  let overflow_msg =
    match overflow with
    | [] -> []
    | _ ->
        let names_str =
          String.concat ", " (List.map (fun (name, _) -> name) overflow)
        in
        let content =
          Printf.sprintf
            "[Skills not auto-loaded after compaction: %s]\n\
             The above skills were previously loaded but were not \
             auto-reloaded to keep\n\
             context compact. To reload any of them, use \
             use_skill(name='skill-name')."
            names_str
        in
        [ Provider.make_message ~role:"system" ~content ]
  in
  skill_msgs @ overflow_msg

(* History must have more than this many messages before force-compression
   is attempted (to avoid compressing already-tiny histories). *)
let context_recovery_min_history = 6

(* Number of most-recent messages to retain during force-compression. *)
let force_compress_keep = 4

(* Bound tool output persisted into conversation history. *)
let max_tool_result_chars = 12000
let adjust_split_for_tool_groups = Message_history.adjust_split_for_tool_groups
let collect_tool_call_ids = Message_history.collect_tool_call_ids
let collect_tool_result_ids = Message_history.collect_tool_result_ids
let ensure_tool_group_integrity = Message_history.ensure_tool_group_integrity
let sanitize_messages_for_flush = Message_history.sanitize_messages_for_flush

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
    let compressed =
      if bounded = [] then begin
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
    (* B626: same rationale as trim_history — re-run native pass to drop
       fully-empty assistants the Coq path leaves behind. *)
    agent.history <- Message_history.ensure_tool_group_integrity compressed;
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

let summarize_messages_with_config config messages =
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
      let* response = Provider.complete ~config ~messages:msgs () in
      match response with
      | Provider.Text { content; _ } -> Lwt.return content
      | Provider.ToolCalls _ -> Lwt.return content)
    (fun exn ->
      Logs.warn (fun m ->
          m "History summarization failed: %s" (Printexc.to_string exn));
      Lwt.return
        (Printf.sprintf "[Summary of %d messages - summarization failed]"
           (List.length messages)))

let summarize_messages agent messages =
  summarize_messages_with_config agent.config messages

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
          Provider.complete ~config ~messages:!messages
            ~tools:flush_memory_tool_schemas ()
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
        let summary_msg =
          Provider.make_message ~role:"assistant" ~content:merged_summary
        in
        let skill_msgs = reload_skills_after_compaction ~to_compact ~to_keep in
        agent.history <- List.rev (skill_msgs @ [ summary_msg ] @ to_keep);
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
      let skill_msgs = reload_skills_after_compaction ~to_compact ~to_keep in
      agent.history <- List.rev (skill_msgs @ [ summary_msg ] @ to_keep);
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
        }
  end

let execute_compact_plan plan ?db ?compact_cbs () =
  let open Lwt.Syntax in
  let config = plan.cp_config in
  let system_prompt = plan.cp_system_prompt in
  let to_compact = plan.cp_to_compact in
  let* () =
    match (db, compact_cbs, config.memory.pre_compaction_flush) with
    | Some db, Some cbs, true ->
        let snapshot = List.map Fun.id to_compact in
        let t0 = Unix.gettimeofday () in
        let* () = cbs.on_step_start "Save memories" "\xf0\x9f\xa7\xa0" in
        let* () =
          Lwt.catch
            (fun () ->
              flush_memories_before_compaction ~config ~system_prompt ~db
                ~to_compact:snapshot)
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
                  ~to_compact:snapshot)
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
        let* s = summarize_messages_with_config config first_half in
        let* () =
          cbs.on_step_done "Summarize (part 1)" (Unix.gettimeofday () -. t0)
        in
        Lwt.return s
    | None -> summarize_messages_with_config config first_half
  in
  let* summary2 =
    match compact_cbs with
    | Some cbs ->
        let t0 = Unix.gettimeofday () in
        let* () =
          cbs.on_step_start "Summarize (part 2)" "\xe2\x9c\x82\xef\xb8\x8f"
        in
        let* s = summarize_messages_with_config config second_half in
        let* () =
          cbs.on_step_done "Summarize (part 2)" (Unix.gettimeofday () -. t0)
        in
        Lwt.return s
    | None -> summarize_messages_with_config config second_half
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
    agent.history <- new_msgs @ List.rev (skill_msgs @ [ summary_msg ] @ to_keep);
    let post_tokens = estimate_history_tokens agent.history in
    Some
      {
        pre_tokens = plan.cp_pre_tokens;
        post_tokens;
        context_window = plan.cp_context_window;
      }
  end
