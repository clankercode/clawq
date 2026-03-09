type special_command_handler =
  key:string ->
  message:string ->
  send_progress:(string -> unit Lwt.t) option ->
  interrupt_check:(unit -> string option) option ->
  string option Lwt.t

type queued_message = {
  message : string;
  content_parts : Provider.content_part list;
  attachments : (string * string) list;
  channel_name : string option;
  channel_type : string option;
  sender_id : string option;
  sender_name : string option;
  channel : string option;
  channel_id : string option;
}

type continuation_state = {
  mutable cancel : unit Lwt.u option;
  mutable disarmed : bool;
}

type t = {
  mutable config : Runtime_config.t;
  sessions : (string, Agent.t * Lwt_mutex.t * string option ref) Hashtbl.t;
  sessions_lock : Lwt_mutex.t;
  tool_registry : Tool_registry.t option;
  sandbox : Sandbox.t option;
  landlock_enabled : bool;
  db : Sqlite3.db option;
  mutable draining : bool;
  in_flight_count : int ref;
  channel_notifiers : (string, string -> unit Lwt.t) Hashtbl.t;
  status_message_factories : (string, unit -> Status_message.t) Hashtbl.t;
  rich_notifiers :
    (string, Rich_message.t -> Rich_message.send_result Lwt.t) Hashtbl.t;
  deferred_responses : (string, unit) Hashtbl.t;
  queued_messages : (string, queued_message list) Hashtbl.t;
  continuation_checks : (string, continuation_state) Hashtbl.t;
  mutable special_command_handler : special_command_handler option;
}

type drain_progress = {
  before_turn : unit -> unit Lwt.t;
  after_all : unit -> unit Lwt.t;
}

let queued_message_response = "__clawq_message_queued__"

let draining_message =
  "Daemon is restarting, please wait a moment and try again."

let autonomous_stay_idle_message = "STAY_IDLE"

let autonomous_continuation_prompt =
  "Autonomous session check-in: continue working if more remains; otherwise \
   reply exactly " ^ autonomous_stay_idle_message

let default_autonomous_continuation_delay = 10.0

let continuation_state mgr ~key =
  match Hashtbl.find_opt mgr.continuation_checks key with
  | Some state -> state
  | None ->
      let state = { cancel = None; disarmed = false } in
      Hashtbl.replace mgr.continuation_checks key state;
      state

let clear_pending_continuation state =
  match state.cancel with
  | Some cancel ->
      Lwt.wakeup_later cancel ();
      state.cancel <- None
  | None -> ()

let with_continuation_state mgr ~key f =
  Lwt_mutex.with_lock mgr.sessions_lock (fun () ->
      f (continuation_state mgr ~key))

let cancel_autonomous_continuation mgr ~key =
  with_continuation_state mgr ~key (fun state ->
      clear_pending_continuation state;
      Lwt.return_unit)

let mark_autonomous_activity_started mgr ~key =
  with_continuation_state mgr ~key (fun state ->
      state.disarmed <- false;
      clear_pending_continuation state;
      Lwt.return_unit)

let create ~config ?tool_registry ?sandbox ?(landlock_enabled = false) ?db () =
  {
    config;
    sessions = Hashtbl.create 16;
    sessions_lock = Lwt_mutex.create ();
    tool_registry;
    sandbox;
    landlock_enabled;
    db;
    draining = false;
    in_flight_count = ref 0;
    channel_notifiers = Hashtbl.create 16;
    status_message_factories = Hashtbl.create 16;
    rich_notifiers = Hashtbl.create 16;
    deferred_responses = Hashtbl.create 16;
    queued_messages = Hashtbl.create 16;
    continuation_checks = Hashtbl.create 16;
    special_command_handler = None;
  }

let is_draining mgr = mgr.draining

let set_special_command_handler mgr handler =
  mgr.special_command_handler <- Some handler

let start_draining mgr =
  Lwt_mutex.with_lock mgr.sessions_lock (fun () ->
      mgr.draining <- true;
      Lwt.return_unit)

let stop_draining mgr =
  Lwt_mutex.with_lock mgr.sessions_lock (fun () ->
      mgr.draining <- false;
      Lwt.return_unit)

let current_in_flight mgr = !(mgr.in_flight_count)

let with_in_flight mgr f =
  incr mgr.in_flight_count;
  Lwt.finalize f (fun () ->
      decr mgr.in_flight_count;
      Lwt.return_unit)

let register_channel_notifier mgr ~key notify =
  Hashtbl.replace mgr.channel_notifiers key notify

let unregister_channel_notifier mgr ~key =
  Hashtbl.remove mgr.channel_notifiers key;
  Hashtbl.remove mgr.status_message_factories key

let register_status_message_factory mgr ~key factory =
  Hashtbl.replace mgr.status_message_factories key factory

let register_rich_notifier mgr ~key notify =
  Hashtbl.replace mgr.rich_notifiers key notify

let unregister_rich_notifier mgr ~key = Hashtbl.remove mgr.rich_notifiers key
let find_rich_notifier mgr ~key = Hashtbl.find_opt mgr.rich_notifiers key

let set_response_deferred mgr ~key =
  Hashtbl.replace mgr.deferred_responses key ()

let response_deferred mgr ~key = Hashtbl.mem mgr.deferred_responses key

let take_response_deferred mgr ~key =
  let deferred = response_deferred mgr ~key in
  if deferred then Hashtbl.remove mgr.deferred_responses key;
  deferred

let clear_response_deferred mgr ~key = Hashtbl.remove mgr.deferred_responses key
let is_queued_message_response response = response = queued_message_response

let queueable_channel_key key =
  match Restart_notify.parse_channel_from_key key with
  | Some ("web", _) -> false
  | Some _ -> true
  | None -> false

let enqueue_message_if_busy mgr ~key queued_message =
  Lwt_mutex.with_lock mgr.sessions_lock (fun () ->
      match Hashtbl.find_opt mgr.sessions key with
      | Some (_, mutex, interrupt)
        when Lwt_mutex.is_locked mutex && queueable_channel_key key
             && Hashtbl.mem mgr.channel_notifiers key ->
          let existing =
            match Hashtbl.find_opt mgr.queued_messages key with
            | Some msgs -> msgs
            | None -> []
          in
          Hashtbl.replace mgr.queued_messages key (existing @ [ queued_message ]);
          Logs.info (fun m ->
              m "Queued inbound message for busy session %s (queue depth: %d)"
                key
                (List.length existing + 1));
          if !interrupt = None then
            interrupt := Some Agent.queued_message_interrupt_token;
          Lwt.return_true
      | _ -> Lwt.return_false)

let take_next_queued_message mgr ~key =
  match Hashtbl.find_opt mgr.queued_messages key with
  | Some (msg :: rest) ->
      if rest = [] then Hashtbl.remove mgr.queued_messages key
      else Hashtbl.replace mgr.queued_messages key rest;
      Some msg
  | _ -> None

let take_all_queued_messages mgr ~key =
  match Hashtbl.find_opt mgr.queued_messages key with
  | Some msgs ->
      Hashtbl.remove mgr.queued_messages key;
      msgs
  | None -> []

let take_all_queued_messages_for_injection mgr ~key =
  let msgs = take_all_queued_messages mgr ~key in
  let count = List.length msgs in
  if count > 0 then
    Logs.info (fun m ->
        m "Injecting %d queued message(s) into session %s" count key);
  msgs

let queued_message_prompt message =
  "A new message arrived while you were working. Treat it as steering "
  ^ "information or a side-question — incorporate it without interrupting "
  ^ "your current task unless it explicitly asks you to stop or change "
  ^ "course.\n\nInjected message:\n" ^ message

let resumable_channel = function
  | "telegram" | "slack" | "discord" -> true
  | _ -> false

let interrupt_resumable_channel_sessions mgr =
  Lwt_mutex.with_lock mgr.sessions_lock (fun () ->
      Hashtbl.iter
        (fun key _ ->
          match
            ( Restart_notify.parse_channel_from_key key,
              Hashtbl.find_opt mgr.sessions key )
          with
          | Some (channel, _), Some (_, _, interrupt)
            when resumable_channel channel ->
              interrupt := Some Agent.restart_interrupt_token
          | _ -> ())
        mgr.channel_notifiers;
      Lwt.return_unit)

let find_registered_notifier mgr ~key =
  Hashtbl.find_opt mgr.channel_notifiers key

let with_registered_notifier mgr ~key ~notify f =
  let prev = find_registered_notifier mgr ~key in
  register_channel_notifier mgr ~key notify;
  Lwt.finalize f (fun () ->
      (match prev with
      | Some old -> register_channel_notifier mgr ~key old
      | None -> unregister_channel_notifier mgr ~key);
      Lwt.return_unit)

let compaction_notice =
  "Compacting earlier chat history to stay within the model's context window."

let notify_compaction_if_needed ?notify compacted =
  match (compacted, notify) with
  | true, Some send ->
      Lwt.catch
        (fun () -> send compaction_notice)
        (fun exn ->
          Logs.warn (fun m ->
              m "Failed to send compaction notice: %s" (Printexc.to_string exn));
          Lwt.return_unit)
  | _ -> Lwt.return_unit

let handle_special_command mgr ~key ~message ?send_progress ?interrupt_check ()
    =
  match mgr.special_command_handler with
  | None -> Lwt.return_none
  | Some handler -> handler ~key ~message ~send_progress ~interrupt_check

let notify_channel_sessions mgr message =
  let notifiers = Hashtbl.to_seq_values mgr.channel_notifiers |> List.of_seq in
  Lwt_list.iter_p
    (fun notify ->
      Lwt.catch
        (fun () -> notify message)
        (fun exn ->
          Logs.warn (fun m ->
              m "Failed to send drain warning: %s" (Printexc.to_string exn));
          Lwt.return_unit))
    notifiers

let get_or_create_locked mgr ~key =
  match Hashtbl.find_opt mgr.sessions key with
  | Some triple -> triple
  | None ->
      let agent =
        Agent.create ~config:mgr.config ?tool_registry:mgr.tool_registry ()
      in
      (match mgr.db with
      | Some db ->
          let history = Memory.load_history ~db ~session_key:key in
          if history <> [] then begin
            let sanitized =
              Message_history.ensure_tool_group_integrity history
            in
            agent.history <- List.rev sanitized;
            Logs.info (fun m ->
                m "Restored %d messages for session %s" (List.length sanitized)
                  key);
            if List.length sanitized <> List.length history then
              Memory.replace_session_messages ~db ~session_key:key sanitized
          end
      | None -> ());
      (match mgr.db with
      | Some db ->
          let loaded_len = List.length agent.history in
          let max_msgs = mgr.config.memory.max_messages_per_session in
          if max_msgs > 0 && loaded_len > max_msgs * 2 then
            Memory.cleanup_session ~db ~session_key:key ~max_messages:max_msgs
              ~max_age_days:mgr.config.memory.max_message_age_days
      | None -> ());
      Agent.trim_history agent;
      let mutex = Lwt_mutex.create () in
      let interrupt = ref None in
      let triple = (agent, mutex, interrupt) in
      Hashtbl.replace mgr.sessions key triple;
      triple

let with_session_lock mgr ~key f =
  let open Lwt.Syntax in
  let* agent, mutex, interrupt =
    Lwt_mutex.with_lock mgr.sessions_lock (fun () ->
        let agent, mutex, interrupt = get_or_create_locked mgr ~key in
        let* () = Lwt_mutex.lock mutex in
        Lwt.return (agent, mutex, interrupt))
  in
  Lwt.finalize
    (fun () -> f agent interrupt)
    (fun () ->
      Lwt_mutex.unlock mutex;
      Lwt.return_unit)

let try_session_lock mgr ~key f =
  let open Lwt.Syntax in
  let* state =
    Lwt_mutex.with_lock mgr.sessions_lock (fun () ->
        let agent, mutex, interrupt = get_or_create_locked mgr ~key in
        if Lwt_mutex.is_locked mutex then Lwt.return_none
        else
          let* () = Lwt_mutex.lock mutex in
          Lwt.return_some (agent, mutex, interrupt))
  in
  match state with
  | None -> Lwt.return_none
  | Some (agent, mutex, interrupt) ->
      Lwt.finalize
        (fun () ->
          let* result = f agent interrupt in
          Lwt.return_some result)
        (fun () ->
          Lwt_mutex.unlock mutex;
          Lwt.return_unit)

let with_session_lock_unless_draining mgr ~key ~on_draining f =
  let open Lwt.Syntax in
  let* state =
    Lwt_mutex.with_lock mgr.sessions_lock (fun () ->
        if mgr.draining then Lwt.return_none
        else
          let agent, mutex, interrupt = get_or_create_locked mgr ~key in
          let* () = Lwt_mutex.lock mutex in
          Lwt.return_some (agent, mutex, interrupt))
  in
  match state with
  | None -> on_draining ()
  | Some (agent, mutex, interrupt) ->
      Lwt.finalize
        (fun () -> f agent interrupt)
        (fun () ->
          Lwt_mutex.unlock mutex;
          Lwt.return_unit)

let set_interrupt_if_present mgr ~key message =
  Lwt_mutex.with_lock mgr.sessions_lock (fun () ->
      (match Hashtbl.find_opt mgr.sessions key with
      | Some (_, _, interrupt) -> interrupt := Some message
      | None -> ());
      Lwt.return_unit)

let interrupt_check_if_present mgr ~key () =
  match Hashtbl.find_opt mgr.sessions key with
  | Some (_, _, interrupt) -> !interrupt
  | None -> None

let is_main_session_key key = key = "__main__"

let shell_visible_roots_summary ~workspace_only ~workspace ~extra_allowed_paths
    =
  if not workspace_only then
    "unrestricted host filesystem view (tool-level checks relaxed)"
  else
    let roots = workspace :: extra_allowed_paths in
    String.concat ", " (List.sort_uniq String.compare roots)

let shell_policy_summary mgr sandbox =
  let workspace_only = mgr.config.security.workspace_only in
  let allowlist = "shell allowlist + path checks" in
  let fs_policy, backend_effective, shell_is_sandboxed =
    match sandbox with
    | Some sb when workspace_only ->
        let backend = Sandbox.backend_to_string sb.Sandbox.backend in
        let policy =
          match sb.Sandbox.backend with
          | Sandbox.None ->
              "OS-level filesystem sandbox disabled; workspace boundaries are \
               enforced by tool validation only"
          | _ ->
              Printf.sprintf
                "OS-level filesystem sandbox enabled via %s with workspace \
                 isolation"
                backend
        in
        (policy, backend, sb.Sandbox.backend <> Sandbox.None)
    | Some sb ->
        ( "workspace_only disabled; shell can access the host filesystem",
          Sandbox.backend_to_string sb.Sandbox.backend,
          false )
    | None -> ("shell runtime context unavailable", "none", false)
  in
  let landlock_suffix =
    if mgr.landlock_enabled then "; landlock enabled for daemon process" else ""
  in
  ( allowlist ^ "; " ^ fs_policy ^ landlock_suffix,
    backend_effective,
    shell_is_sandboxed )

let active_background_task_summaries mgr =
  match mgr.db with
  | None -> []
  | Some db ->
      Background_task.init_schema db;
      Background_task.list_tasks ~db
      |> List.filter (fun t ->
          match t.Background_task.status with
          | Background_task.Queued | Background_task.Running -> true
          | _ -> false)
      |> List.sort (fun a b ->
          compare a.Background_task.id b.Background_task.id)
      |> List.map (fun t ->
          {
            Prompt_builder.id = t.Background_task.id;
            runner = Background_task.string_of_runner t.runner;
            repo_label = Filename.basename t.repo_path;
            branch = (if t.branch = "" then "(auto)" else t.branch);
            status = Background_task.string_of_status t.status;
          })

let runtime_context_details mgr ~agent ~key ~compacted_before_turn =
  let workspace = Runtime_config.effective_workspace mgr.config in
  let extra_allowed_paths =
    mgr.config.security.extra_allowed_paths
    |> List.map Runtime_config.expand_home
  in
  let shell_policy_summary, sandbox_backend_effective, shell_is_sandboxed =
    shell_policy_summary mgr mgr.sandbox
  in
  {
    Prompt_builder.session_id = key;
    session_name = (if is_main_session_key key then Some "main" else None);
    is_main_session = is_main_session_key key;
    heartbeat_routing_applies =
      is_main_session_key key && mgr.config.heartbeat.heartbeat_enabled;
    effective_workspace = workspace;
    workspace_only = mgr.config.security.workspace_only;
    sandbox_backend_requested = mgr.config.security.sandbox_backend;
    sandbox_backend_effective;
    shell_is_sandboxed;
    shell_policy_summary;
    shell_visible_roots_summary =
      shell_visible_roots_summary
        ~workspace_only:mgr.config.security.workspace_only ~workspace
        ~extra_allowed_paths;
    background_tasks = active_background_task_summaries mgr;
    context_usage =
      Some (Agent.runtime_context_usage agent ~compacted_before_turn);
  }

let format_context_block ?channel_name ?channel_type ?sender_id ?sender_name ()
    =
  let cn = match channel_name with Some n -> n | None -> "cli" in
  let ct = match channel_type with Some t -> t | None -> "dm" in
  let sender_part =
    match (sender_id, sender_name) with
    | Some id, Some name -> Printf.sprintf " sender=@%s (%s)" id name
    | Some id, None -> Printf.sprintf " sender=@%s" id
    | None, Some name -> Printf.sprintf " sender=%s" name
    | None, None -> ""
  in
  Printf.sprintf "[Context: channel=%s type=%s%s]" cn ct sender_part

let inject_attachment_context agent attachments =
  match Prompt_builder.attachment_syntax_block attachments with
  | Some block ->
      agent.Agent.history <-
        Provider.make_message ~role:"system" ~content:block
        :: agent.Agent.history
  | None -> ()

let record_agent_turn mgr ~key ?channel ?channel_id () =
  match mgr.db with
  | Some db ->
      Memory.upsert_session_state ~db ~session_key:key ~turn:"agent" ?channel
        ?channel_id ()
  | None -> ()

let mark_response_sent mgr ~key =
  match mgr.db with
  | Some db -> Memory.mark_response_sent ~db ~session_key:key
  | None -> ()

let load_pending_agent_sessions mgr ~max_age_seconds =
  match mgr.db with
  | Some db -> Memory.load_pending_agent_sessions ~db ~max_age_seconds
  | None -> []

let persist_new_messages mgr ~key ~history_before agent =
  match mgr.db with
  | Some db ->
      let new_messages = List.length agent.Agent.history - history_before in
      if new_messages > 0 then begin
        let reversed = List.rev agent.Agent.history in
        let to_persist =
          let skip = history_before in
          List.filteri (fun i _ -> i >= skip) reversed
        in
        List.iter
          (fun msg -> Memory.store_message ~db ~session_key:key msg)
          to_persist
      end
  | None -> ()

let persist_compacted_history mgr ~key agent =
  match mgr.db with
  | Some db ->
      let messages = List.rev agent.Agent.history in
      Memory.replace_session_messages ~db ~session_key:key messages
  | None -> ()

let respond_if_draining ?on_chunk mgr =
  let open Lwt.Syntax in
  if mgr.draining then
    match on_chunk with
    | None -> Lwt.return_some draining_message
    | Some send ->
        let* () = send (Provider.Delta draining_message) in
        let* () = send Provider.Done in
        Lwt.return_some draining_message
  else Lwt.return_none

let consolidated_status_on_chunk
    ~(agent_defaults : Runtime_config.agent_defaults) ~thinking_buf sm =
  function
  | Provider.ToolStart { id; name; arguments } ->
      let summary =
        Stream_visibility.summarize_tool_arguments ~name arguments
      in
      Status_message.tool_start sm ~id ~name ~summary
  | Provider.ToolResult { id; name; result; is_error } ->
      Status_message.tool_result sm ~id ~name ~result ~is_error
  | Provider.ThinkingDelta text ->
      if agent_defaults.show_thinking then begin
        Buffer.add_string thinking_buf text;
        Status_message.update_thinking sm text
      end
      else Lwt.return_unit
  | Provider.Delta _ | Provider.ToolCallDelta _ | Provider.ToolOutputDelta _
  | Provider.Done ->
      Lwt.return_unit

let stream_turn_with_visibility mgr ~notify agent ~key ~effective_message
    ~persisted_up_to ~interrupt_check ~inject_messages ~runtime_context
    ~on_history_update =
  let open Lwt.Syntax in
  let agent_defaults = mgr.config.agent_defaults in
  let use_consolidated =
    agent_defaults.show_tool_calls
    && agent_defaults.tool_status_mode = "consolidated"
  in
  let status_factory =
    if use_consolidated then Hashtbl.find_opt mgr.status_message_factories key
    else None
  in
  match status_factory with
  | Some factory ->
      let sm = factory () in
      let thinking_buf = Buffer.create 256 in
      let on_chunk =
        consolidated_status_on_chunk ~agent_defaults ~thinking_buf sm
      in
      let* response =
        Agent.turn_stream agent ~user_message:effective_message ?db:mgr.db
          ~session_key:key ~interrupt_check ~inject_messages ?runtime_context
          ~history_prepared:true ~on_history_update ~on_chunk ()
      in
      let* () = Status_message.finalize sm in
      let thinking = Buffer.contents thinking_buf in
      let* () =
        if agent_defaults.show_thinking && thinking <> "" then
          notify (Stream_visibility.thinking_message thinking)
        else Lwt.return_unit
      in
      persist_new_messages mgr ~key ~history_before:!persisted_up_to agent;
      (match mgr.db with
      | Some db when mgr.config.security.audit_enabled ->
          Audit.log ~db
            (ChatMessage
               {
                 session_key = key;
                 role = "assistant";
                 content_preview = response;
               })
      | _ -> ());
      Lwt.return response
  | None ->
      let visibility = Stream_visibility.create () in
      let settings : Stream_visibility.settings =
        {
          show_thinking = agent_defaults.show_thinking;
          show_tool_calls = agent_defaults.show_tool_calls;
          notify_tool_starts = false;
          notify_tool_successes = true;
        }
      in
      let* response =
        Agent.turn_stream agent ~user_message:effective_message ?db:mgr.db
          ~session_key:key ~interrupt_check ~inject_messages ?runtime_context
          ~history_prepared:true ~on_history_update
          ~on_chunk:(Stream_visibility.on_chunk visibility ~settings ~notify)
          ()
      in
      let thinking = Stream_visibility.thinking_text visibility in
      let* () =
        if settings.show_thinking && thinking <> "" then
          notify (Stream_visibility.thinking_message thinking)
        else Lwt.return_unit
      in
      persist_new_messages mgr ~key ~history_before:!persisted_up_to agent;
      (match mgr.db with
      | Some db when mgr.config.security.audit_enabled ->
          Audit.log ~db
            (ChatMessage
               {
                 session_key = key;
                 role = "assistant";
                 content_preview = response;
               })
      | _ -> ());
      Lwt.return response

let normalize_incoming_message mgr ~key ~message =
  let open Lwt.Syntax in
  if String.length message > 0 && message.[0] = '!' then begin
    let raw = String.sub message 1 (String.length message - 1) in
    let normalized = if String.trim raw = "" then "[interrupted]" else raw in
    let session_exists = Hashtbl.mem mgr.sessions key in
    let session_busy =
      match Hashtbl.find_opt mgr.sessions key with
      | Some (_, mutex, _) -> Lwt_mutex.is_locked mutex
      | None -> false
    in
    Logs.info (fun m ->
        m
          "Bang message received for session %s: raw=%S normalized=%S \
           session_exists=%b session_busy=%b"
          key raw normalized session_exists session_busy);
    let* () = set_interrupt_if_present mgr ~key normalized in
    Lwt.return normalized
  end
  else Lwt.return message

let effective_message_for_turn ~message ?channel_name ?channel_type ?sender_id
    ?sender_name () =
  match (channel_name, channel_type, sender_id, sender_name) with
  | None, None, None, None -> message
  | _ ->
      let ctx =
        format_context_block ?channel_name ?channel_type ?sender_id ?sender_name
          ()
      in
      ctx ^ "\n" ^ message

let run_locked_turn mgr ~key agent interrupt ~message ?(content_parts = [])
    ?(attachments = []) ?channel_name ?channel_type ?sender_id ?sender_name
    ?channel ?channel_id () =
  let open Lwt.Syntax in
  let interrupt_check () = !interrupt in
  interrupt := None;
  (match mgr.db with
  | Some db when mgr.config.security.audit_enabled ->
      Audit.log ~db
        (ChatMessage
           { session_key = key; role = "user"; content_preview = message })
  | _ -> ());
  inject_attachment_context agent attachments;
  let effective_message =
    effective_message_for_turn ~message ?channel_name ?channel_type ?sender_id
      ?sender_name ()
  in
  let history_before = List.length agent.history in
  let notify = find_registered_notifier mgr ~key in
  let* compacted =
    Agent.prepare_turn_history agent ~user_message:effective_message
      ~content_parts ?db:mgr.db ()
  in
  let* () = notify_compaction_if_needed ?notify compacted in
  if compacted then persist_compacted_history mgr ~key agent
  else persist_new_messages mgr ~key ~history_before agent;
  let runtime_context =
    Prompt_builder.build_runtime_context ~config:mgr.config
      ~details:
        (runtime_context_details mgr ~agent ~key
           ~compacted_before_turn:compacted)
      ()
  in
  let prepared_history_len = List.length agent.history in
  record_agent_turn mgr ~key ?channel ?channel_id ();
  let persisted_up_to = ref prepared_history_len in
  let on_history_update new_msgs =
    match mgr.db with
    | Some db ->
        List.iter
          (fun msg -> Memory.store_message ~db ~session_key:key msg)
          new_msgs;
        persisted_up_to := List.length agent.Agent.history
    | None -> ()
  in
  let inject_messages () =
    let msgs = take_all_queued_messages_for_injection mgr ~key in
    List.map
      (fun (qm : queued_message) ->
        queued_message_prompt
          (effective_message_for_turn ~message:qm.message
             ?channel_name:qm.channel_name ?channel_type:qm.channel_type
             ?sender_id:qm.sender_id ?sender_name:qm.sender_name ()))
      msgs
  in
  let* response =
    Lwt.catch
      (fun () ->
        let* draining_response = respond_if_draining mgr in
        match draining_response with
        | Some response -> Lwt.return response
        | None -> (
            match notify with
            | Some send
              when mgr.config.agent_defaults.show_thinking
                   || mgr.config.agent_defaults.show_tool_calls ->
                stream_turn_with_visibility mgr ~notify:send agent ~key
                  ~effective_message ~persisted_up_to ~interrupt_check
                  ~inject_messages ~runtime_context ~on_history_update
            | _ ->
                Agent.turn agent ~user_message:effective_message ?db:mgr.db
                  ~session_key:key ~interrupt_check ~inject_messages
                  ?runtime_context ~history_prepared:true ~on_history_update ()))
      (function
        | Agent.Restart_requested ->
            persist_new_messages mgr ~key ~history_before:!persisted_up_to agent;
            set_response_deferred mgr ~key;
            Lwt.return draining_message
        | exn -> Lwt.fail exn)
  in
  (match notify with
  | Some _
    when mgr.config.agent_defaults.show_thinking
         || mgr.config.agent_defaults.show_tool_calls ->
      ()
  | _ ->
      if not (response_deferred mgr ~key) then begin
        persist_new_messages mgr ~key ~history_before:!persisted_up_to agent;
        match mgr.db with
        | Some db when mgr.config.security.audit_enabled ->
            Audit.log ~db
              (ChatMessage
                 {
                   session_key = key;
                   role = "assistant";
                   content_preview = response;
                 })
        | _ -> ()
      end);
  Lwt.return response

let rec drain_queued_messages_loop mgr ~key agent interrupt ?on_drain_progress
    ~drained_any () =
  match
    (take_next_queued_message mgr ~key, find_registered_notifier mgr ~key)
  with
  | Some queued, Some notify ->
      let open Lwt.Syntax in
      Logs.info (fun m -> m "Sending queued message to LLM for session %s" key);
      let* () =
        match on_drain_progress with
        | Some dp -> dp.before_turn ()
        | None -> Lwt.return_unit
      in
      let injected_message =
        queued_message_prompt
          (effective_message_for_turn ~message:queued.message
             ?channel_name:queued.channel_name ?channel_type:queued.channel_type
             ?sender_id:queued.sender_id ?sender_name:queued.sender_name ())
      in
      let* response =
        run_locked_turn mgr ~key agent interrupt ~message:injected_message
          ~content_parts:queued.content_parts ?channel_name:queued.channel_name
          ?channel_type:queued.channel_type ?sender_id:queued.sender_id
          ?sender_name:queued.sender_name ?channel:queued.channel
          ?channel_id:queued.channel_id ()
      in
      let* () = notify response in
      if not (take_response_deferred mgr ~key) then mark_response_sent mgr ~key;
      drain_queued_messages_loop mgr ~key agent interrupt ?on_drain_progress
        ~drained_any:true ()
  | Some queued, None ->
      Logs.warn (fun m ->
          m
            "Dropping queued message for session %s: no notifier registered \
             (message: %s)"
            key
            (if String.length queued.message > 80 then
               String.sub queued.message 0 80 ^ "..."
             else queued.message));
      Lwt.return_unit
  | None, _ ->
      if drained_any then
        let open Lwt.Syntax in
        let* () =
          match on_drain_progress with
          | Some dp -> dp.after_all ()
          | None -> Lwt.return_unit
        in
        Lwt.return_unit
      else Lwt.return_unit

let drain_queued_messages mgr ~key agent interrupt ?on_drain_progress () =
  drain_queued_messages_loop mgr ~key agent interrupt ?on_drain_progress
    ~drained_any:false ()

let rec turn mgr ~key ~message ?(content_parts = []) ?(attachments = [])
    ?channel_name ?channel_type ?sender_id ?sender_name ?channel ?channel_id ()
    =
  let open Lwt.Syntax in
  let* () = mark_autonomous_activity_started mgr ~key in
  let* message = normalize_incoming_message mgr ~key ~message in
  let* handled =
    handle_special_command mgr ~key ~message
      ?send_progress:(find_registered_notifier mgr ~key)
      ~interrupt_check:(interrupt_check_if_present mgr ~key)
      ()
  in
  match handled with
  | Some response -> Lwt.return response
  | None ->
      let queued_message =
        {
          message;
          content_parts;
          attachments;
          channel_name;
          channel_type;
          sender_id;
          sender_name;
          channel;
          channel_id;
        }
      in
      let* queued = enqueue_message_if_busy mgr ~key queued_message in
      if queued then Lwt.return queued_message_response
      else
        with_session_lock_unless_draining mgr ~key
          ~on_draining:(fun () ->
            let* draining_response = respond_if_draining mgr in
            match draining_response with
            | Some response -> Lwt.return response
            | None -> Lwt.return draining_message)
          (fun agent interrupt ->
            with_in_flight mgr (fun () ->
                let* response =
                  run_locked_turn mgr ~key agent interrupt ~message
                    ~content_parts ~attachments ?channel_name ?channel_type
                    ?sender_id ?sender_name ?channel ?channel_id ()
                in
                let* () = drain_queued_messages mgr ~key agent interrupt () in
                Lwt.return response))

let delegate_turn mgr ~prompt ~send_reply =
  if mgr.draining then
    Lwt.async (fun () ->
        Lwt.catch
          (fun () -> send_reply draining_message)
          (fun _ -> Lwt.return_unit))
  else
    Lwt.async (fun () ->
        with_in_flight mgr (fun () ->
            let agent =
              Agent.create ~config:mgr.config ?tool_registry:mgr.tool_registry
                ()
            in
            Lwt.catch
              (fun () ->
                let open Lwt.Syntax in
                let* response = Agent.turn agent ~user_message:prompt () in
                send_reply response)
              (fun exn ->
                Logs.err (fun m ->
                    m "Delegation failed: %s" (Printexc.to_string exn));
                Lwt.catch
                  (fun () ->
                    send_reply
                      (Printf.sprintf "Delegation failed: %s"
                         (Printexc.to_string exn)))
                  (fun _ -> Lwt.return_unit))))

let snapshot_history mgr ~key =
  Lwt_mutex.with_lock mgr.sessions_lock (fun () ->
      match Hashtbl.find_opt mgr.sessions key with
      | Some (agent, _, _) ->
          let history = List.rev agent.Agent.history in
          Lwt.return (Message_history.ensure_tool_group_integrity history)
      | None ->
          let history =
            match mgr.db with
            | Some db -> Memory.load_history ~db ~session_key:key
            | None -> []
          in
          Lwt.return history)

let fork_and_run mgr ~parent_key ~prompt ~send_reply =
  if mgr.draining then
    Lwt.async (fun () ->
        Lwt.catch
          (fun () -> send_reply draining_message)
          (fun _ -> Lwt.return_unit))
  else
    Lwt.async (fun () ->
        with_in_flight mgr (fun () ->
            let open Lwt.Syntax in
            let* parent_history = snapshot_history mgr ~key:parent_key in
            let agent =
              Agent.create ~config:mgr.config ?tool_registry:mgr.tool_registry
                ()
            in
            agent.Agent.history <- List.rev parent_history;
            Lwt.catch
              (fun () ->
                let* response = Agent.turn agent ~user_message:prompt () in
                send_reply response)
              (fun exn ->
                Logs.err (fun m ->
                    m "Fork failed for parent=%s: %s" parent_key
                      (Printexc.to_string exn));
                Lwt.catch
                  (fun () ->
                    send_reply
                      (Printf.sprintf "Fork failed: %s" (Printexc.to_string exn)))
                  (fun _ -> Lwt.return_unit))))

let get_config mgr = mgr.config
let get_tool_registry mgr = mgr.tool_registry

let update_config mgr config =
  mgr.config <- config;
  Hashtbl.iter
    (fun _ (agent, _, _) -> agent.Agent.config <- config)
    mgr.sessions

let turn_stream mgr ~key ~message ?(content_parts = []) ?(attachments = [])
    ?channel_name ?channel_type ?sender_id ?sender_name ?channel ?channel_id
    ?on_drain_progress ~on_chunk () =
  let open Lwt.Syntax in
  let* () = mark_autonomous_activity_started mgr ~key in
  let* message = normalize_incoming_message mgr ~key ~message in
  let send_progress text = on_chunk (Provider.Delta (text ^ "\n")) in
  let* handled =
    handle_special_command mgr ~key ~message ~send_progress
      ~interrupt_check:(interrupt_check_if_present mgr ~key)
      ()
  in
  match handled with
  | Some response ->
      let* () = on_chunk (Provider.Delta response) in
      let* () = on_chunk Provider.Done in
      Lwt.return response
  | None ->
      let queued_message =
        {
          message;
          content_parts;
          attachments;
          channel_name;
          channel_type;
          sender_id;
          sender_name;
          channel;
          channel_id;
        }
      in
      let* queued = enqueue_message_if_busy mgr ~key queued_message in
      if queued then Lwt.return queued_message_response
      else
        with_session_lock_unless_draining mgr ~key
          ~on_draining:(fun () ->
            let* draining_response = respond_if_draining ~on_chunk mgr in
            match draining_response with
            | Some response -> Lwt.return response
            | None -> Lwt.return draining_message)
          (fun agent interrupt ->
            with_in_flight mgr (fun () ->
                let interrupt_check () = !interrupt in
                interrupt := None;
                (match mgr.db with
                | Some db when mgr.config.security.audit_enabled ->
                    Audit.log ~db
                      (ChatMessage
                         {
                           session_key = key;
                           role = "user";
                           content_preview = message;
                         })
                | _ -> ());
                inject_attachment_context agent attachments;
                let effective_message =
                  match
                    (channel_name, channel_type, sender_id, sender_name)
                  with
                  | None, None, None, None -> message
                  | _ ->
                      let ctx =
                        format_context_block ?channel_name ?channel_type
                          ?sender_id ?sender_name ()
                      in
                      ctx ^ "\n" ^ message
                in
                let history_before = List.length agent.history in
                let* compacted =
                  Agent.prepare_turn_history agent
                    ~user_message:effective_message ~content_parts ?db:mgr.db ()
                in
                let* () =
                  notify_compaction_if_needed
                    ~notify:(fun text ->
                      on_chunk (Provider.Delta (text ^ "\n")))
                    compacted
                in
                if compacted then persist_compacted_history mgr ~key agent
                else persist_new_messages mgr ~key ~history_before agent;
                let runtime_context =
                  Prompt_builder.build_runtime_context ~config:mgr.config
                    ~details:
                      (runtime_context_details mgr ~agent ~key
                         ~compacted_before_turn:compacted)
                    ()
                in
                let prepared_history_len = List.length agent.history in
                record_agent_turn mgr ~key ?channel ?channel_id ();
                let persisted_up_to = ref prepared_history_len in
                let on_history_update new_msgs =
                  match mgr.db with
                  | Some db ->
                      List.iter
                        (fun msg ->
                          Memory.store_message ~db ~session_key:key msg)
                        new_msgs;
                      persisted_up_to := List.length agent.Agent.history
                  | None -> ()
                in
                let inject_messages () =
                  let msgs = take_all_queued_messages_for_injection mgr ~key in
                  List.map
                    (fun (qm : queued_message) ->
                      queued_message_prompt
                        (effective_message_for_turn ~message:qm.message
                           ?channel_name:qm.channel_name
                           ?channel_type:qm.channel_type ?sender_id:qm.sender_id
                           ?sender_name:qm.sender_name ()))
                    msgs
                in
                let* response =
                  Lwt.catch
                    (fun () ->
                      let* draining_response =
                        respond_if_draining ~on_chunk mgr
                      in
                      match draining_response with
                      | Some response -> Lwt.return response
                      | None ->
                          Agent.turn_stream agent
                            ~user_message:effective_message ?db:mgr.db
                            ~session_key:key ~interrupt_check ~inject_messages
                            ?runtime_context ~history_prepared:true
                            ~on_history_update ~on_chunk ())
                    (function
                      | Agent.Restart_requested ->
                          persist_new_messages mgr ~key
                            ~history_before:!persisted_up_to agent;
                          set_response_deferred mgr ~key;
                          let* () =
                            on_chunk (Provider.Delta draining_message)
                          in
                          let* () = on_chunk Provider.Done in
                          Lwt.return draining_message
                      | exn -> Lwt.fail exn)
                in
                if not (response_deferred mgr ~key) then begin
                  persist_new_messages mgr ~key ~history_before:!persisted_up_to
                    agent;
                  match mgr.db with
                  | Some db when mgr.config.security.audit_enabled ->
                      Audit.log ~db
                        (ChatMessage
                           {
                             session_key = key;
                             role = "assistant";
                             content_preview = response;
                           })
                  | _ -> ()
                end;
                let* () =
                  drain_queued_messages mgr ~key agent interrupt
                    ?on_drain_progress ()
                in
                Lwt.return response))

let reset mgr ~key =
  let open Lwt.Syntax in
  let* held_mutex =
    Lwt_mutex.with_lock mgr.sessions_lock (fun () ->
        match Hashtbl.find_opt mgr.sessions key with
        | Some (_, mutex, _) ->
            let* () = Lwt_mutex.lock mutex in
            (match mgr.db with
            | Some db -> Memory.clear_session ~db ~session_key:key
            | None -> ());
            Hashtbl.remove mgr.deferred_responses key;
            Hashtbl.remove mgr.queued_messages key;
            Hashtbl.remove mgr.continuation_checks key;
            unregister_channel_notifier mgr ~key;
            unregister_rich_notifier mgr ~key;
            Hashtbl.remove mgr.sessions key;
            Lwt.return (Some mutex)
        | None ->
            (match mgr.db with
            | Some db -> Memory.clear_session ~db ~session_key:key
            | None -> ());
            Hashtbl.remove mgr.deferred_responses key;
            Hashtbl.remove mgr.queued_messages key;
            Hashtbl.remove mgr.continuation_checks key;
            unregister_channel_notifier mgr ~key;
            unregister_rich_notifier mgr ~key;
            Lwt.return None)
  in
  match held_mutex with
  | Some mutex ->
      Lwt_mutex.unlock mutex;
      Lwt.return_unit
  | None -> Lwt.return_unit

let compact mgr ~key =
  let open Lwt.Syntax in
  with_session_lock mgr ~key (fun agent _interrupt ->
      let* compacted = Agent.force_compact_history agent in
      if compacted then begin
        persist_compacted_history mgr ~key agent;
        Lwt.return true
      end
      else Lwt.return false)

let rec schedule_autonomous_continuation
    ?(delay = default_autonomous_continuation_delay)
    ?(around_turn = fun f -> f ())
    ?(on_response = fun _response -> Lwt.return_unit) mgr ~key =
  let open Lwt.Syntax in
  let* should_schedule, cancel_waiter =
    with_continuation_state mgr ~key (fun state ->
        if state.disarmed then Lwt.return (false, None)
        else begin
          clear_pending_continuation state;
          let cancel_waiter, cancel = Lwt.wait () in
          state.cancel <- Some cancel;
          Lwt.return (true, Some cancel_waiter)
        end)
  in
  match (should_schedule, cancel_waiter) with
  | false, _ | _, None -> Lwt.return_unit
  | true, Some cancel_waiter ->
      let* cancelled =
        Lwt.pick
          [
            (let* () = Lwt_unix.sleep delay in
             Lwt.return_false);
            (let* () = cancel_waiter in
             Lwt.return_true);
          ]
      in
      if cancelled then Lwt.return_unit
      else
        let* () =
          match find_registered_notifier mgr ~key with
          | Some notify ->
              let labeled =
                "[automatic continuation check-in]\n"
                ^ autonomous_continuation_prompt
              in
              Lwt.catch (fun () -> notify labeled) (fun _ -> Lwt.return_unit)
          | None -> Lwt.return_unit
        in
        let* response =
          Lwt.catch
            (fun () ->
              around_turn (fun () ->
                  turn mgr ~key ~message:autonomous_continuation_prompt ()))
            (fun exn ->
              Logs.warn (fun m ->
                  m "Autonomous continuation prompt failed for %s: %s" key
                    (Printexc.to_string exn));
              Lwt.return "")
        in
        let trimmed = String.trim response in
        if trimmed = queued_message_response then Lwt.return_unit
        else if trimmed = autonomous_stay_idle_message then
          with_continuation_state mgr ~key (fun state ->
              state.disarmed <- true;
              state.cancel <- None;
              Lwt.return_unit)
        else begin
          let* () =
            Lwt.catch
              (fun () -> on_response trimmed)
              (fun exn ->
                Logs.warn (fun m ->
                    m "Autonomous continuation on_response failed for %s: %s"
                      key (Printexc.to_string exn));
                Lwt.return_unit)
          in
          let* () = cancel_autonomous_continuation mgr ~key in
          schedule_autonomous_continuation ~delay ~around_turn ~on_response mgr
            ~key
        end

let process_autonomous_turn_result
    ?(delay = default_autonomous_continuation_delay)
    ?(around_turn = fun f -> f ())
    ?(on_response = fun _response -> Lwt.return_unit) mgr ~key ~response =
  let trimmed = String.trim response in
  if trimmed = "" || trimmed = "HEARTBEAT_OK" then Lwt.return_unit
  else if trimmed = autonomous_stay_idle_message then
    with_continuation_state mgr ~key (fun state ->
        state.disarmed <- true;
        clear_pending_continuation state;
        Lwt.return_unit)
  else
    schedule_autonomous_continuation ~delay ~around_turn ~on_response mgr ~key
