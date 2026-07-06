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
  (* B677: hard turn-abort signal. Set by validate_required_with_escalation
     when last_missing_required_count crosses CLAWQ_MAX_IDENTICAL_PARAM_ERRORS
     (default 3). The main turn loop checks this after tool execution and
     terminates the turn with a clear user-facing message. Reset on any
     successful tool call. *)
  mutable hard_abort_reason : string option;
  (* Room profile system_prompt override. When [Some s] and [s <> ""],
     Prompt_builder.build uses this instead of agent_template.system_prompt.
     Set by apply_room_profile_template_fields in session_core. *)
  mutable room_profile_system_prompt : string option;
  mutable profiled_room : bool;
  (* Layered instructions resolved from access scopes. Carries provenance
     labels for deterministic ordering: default → workspace → channel → room.
     Passed to Prompt_builder.build for injection into the system prompt. *)
  mutable instruction_items : Runtime_config.effective_instruction_item list;
  (* P14.M2.E3.T001: effective-access snapshot. When set, access checks
     during this turn use the snapshot's resolved values instead of
     re-resolving from the live config. Ensures config changes during
     execution don't alter in-flight access. *)
  mutable access_snapshot_id : string option;
  mutable access_snapshot : Access_snapshot.t option;
}

exception Interrupted of string
exception Restart_requested
exception Stop_requested
exception Budget_exceeded of string

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
  cp_profiled_room : bool;
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
    | Restart_requested | Stop_requested | Interrupted _ | Budget_exceeded _ ->
        true
    | Failure msg ->
        string_contains_ci_small msg
          "no tool call found for function call output"
    | _ -> false)

let is_session_event_message (msg : Provider.message) = msg.role = "event"

let profiled_room_active agent =
  agent.profiled_room
  ||
  match agent.room_profile_system_prompt with
  | Some s when String.trim s <> "" -> true
  | _ -> false

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

let scoped_memory_reference_lines messages =
  messages
  |> List.concat_map (fun (msg : Provider.message) ->
      if msg.role <> "system" then []
      else
        msg.content |> String.split_on_char '\n'
        |> List.filter_map (fun line ->
            let line = String.trim line in
            if
              String.starts_with ~prefix:"[scoped:" line
              || String.starts_with ~prefix:"[scoped-message:" line
            then Some line
            else None))

let preserve_scoped_memory_references_after_compaction ~to_compact ~to_keep =
  let kept = Hashtbl.create 8 in
  List.iter
    (fun line -> Hashtbl.replace kept line ())
    (scoped_memory_reference_lines to_keep);
  let seen = Hashtbl.create 8 in
  let refs =
    scoped_memory_reference_lines to_compact
    |> List.filter (fun line ->
        (not (Hashtbl.mem kept line)) && not (Hashtbl.mem seen line))
    |> List.map (fun line ->
        Hashtbl.replace seen line ();
        line)
  in
  match refs with
  | [] -> []
  | _ ->
      [
        Provider.make_message ~role:"system"
          ~content:
            ("[Scoped memory references preserved after compaction]\n"
           ^ String.concat "\n" refs);
      ]

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
