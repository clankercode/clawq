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
  message_id : string option;
}

type continuation_state = {
  mutable cancel : unit Lwt.u option;
  mutable disarmed : bool;
}

type live_activity_snapshot = { active : bool; generation : int }

type live_activity_state = {
  mutable active_scopes : int;
  mutable generation : int;
  mutable changed : unit Lwt.t;
  mutable wake_changed : unit Lwt.u;
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
  live_activity : (string, live_activity_state) Hashtbl.t;
  continuation_checks : (string, continuation_state) Hashtbl.t;
  mutable special_command_handler : special_command_handler option;
}

type drain_progress = {
  before_turn : string option -> unit Lwt.t;
  after_turn : string option -> unit Lwt.t;
  after_all : unit -> unit Lwt.t;
}

let queued_message_response = "__clawq_message_queued__"

let draining_message =
  "Daemon is restarting, please wait a moment and try again."

let autonomous_stay_idle_message = "STAY_IDLE"

let autonomous_continuation_prompt =
  "Autonomous session check-in: continue working if more remains; otherwise \
   reply exactly " ^ autonomous_stay_idle_message

let keepalive_nudge_prompt =
  "[Automated Keepalive Check-In]\n\
   Continue working on your tasks if any remain.\n\n\
   If you have nothing to do and want to remain idle, reply exactly: "
  ^ autonomous_stay_idle_message

let get_context_usage_percent mgr ~key =
  match Hashtbl.find_opt mgr.sessions key with
  | Some (agent, _, _) ->
      let estimated_tokens = Agent.estimate_history_tokens agent.history in
      let context_window = Agent.context_window_for_agent agent in
      if context_window > 0 then
        let percent = min 100 (estimated_tokens * 100 / context_window) in
        Some (percent, estimated_tokens, context_window)
      else None
  | None -> None

let compaction_suggestion_for_prompt mgr ~key =
  match get_context_usage_percent mgr ~key with
  | Some (percent, estimated_tokens, context_window) when percent > 40 ->
      Printf.sprintf
        "\n\n\
         [Context usage: %d%% (%d/%d tokens). Consider running compaction \
         using the `compact_history` tool, the `/compact` command, or `clawq \
         session compact %s` to free up context space and avoid inefficient \
         token usage.]"
        percent estimated_tokens context_window key
  | _ -> ""

let default_autonomous_continuation_delay = 90.0

let create_live_activity_state () =
  let changed, wake_changed = Lwt.wait () in
  { active_scopes = 0; generation = 0; changed; wake_changed }

let live_activity_state mgr ~key =
  match Hashtbl.find_opt mgr.live_activity key with
  | Some state -> state
  | None ->
      let state = create_live_activity_state () in
      Hashtbl.replace mgr.live_activity key state;
      state

let snapshot_live_activity state =
  { active = state.active_scopes > 0; generation = state.generation }

let advance_live_activity state =
  let prev_wake = state.wake_changed in
  let changed, wake_changed = Lwt.wait () in
  state.generation <- state.generation + 1;
  state.changed <- changed;
  state.wake_changed <- wake_changed;
  Lwt.wakeup_later prev_wake ()

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

let current_live_activity mgr ~key =
  Lwt_mutex.with_lock mgr.sessions_lock (fun () ->
      let state = live_activity_state mgr ~key in
      Lwt.return (snapshot_live_activity state))

let rec wait_for_live_activity_change mgr ~key ~after_generation =
  let open Lwt.Syntax in
  let* snapshot, changed =
    Lwt_mutex.with_lock mgr.sessions_lock (fun () ->
        let state = live_activity_state mgr ~key in
        Lwt.return (snapshot_live_activity state, state.changed))
  in
  if snapshot.generation <> after_generation then Lwt.return snapshot
  else
    let* () = changed in
    wait_for_live_activity_change mgr ~key ~after_generation

let start_live_activity mgr ~key =
  Lwt_mutex.with_lock mgr.sessions_lock (fun () ->
      let state = live_activity_state mgr ~key in
      let was_inactive = state.active_scopes = 0 in
      state.active_scopes <- state.active_scopes + 1;
      if was_inactive then advance_live_activity state;
      Lwt.return_unit)

let stop_live_activity mgr ~key =
  Lwt_mutex.with_lock mgr.sessions_lock (fun () ->
      let state = live_activity_state mgr ~key in
      if state.active_scopes > 0 then begin
        state.active_scopes <- state.active_scopes - 1;
        if state.active_scopes = 0 then advance_live_activity state
      end;
      Lwt.return_unit)

let with_live_activity mgr ~key f =
  let open Lwt.Syntax in
  let* () = start_live_activity mgr ~key in
  Lwt.finalize f (fun () -> stop_live_activity mgr ~key)

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
    live_activity = Hashtbl.create 16;
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

(* Temporarily suppress all channel-visible output (notifier, status messages)
   for the duration of [f]. Used during autonomous continuation turns when the
   check-in message is hidden from the user, so thinking/tool-call status/
   compaction notices don't leak to the channel either. *)
let with_suppressed_channel_output mgr ~key f =
  let prev_notify = Hashtbl.find_opt mgr.channel_notifiers key in
  let prev_factory = Hashtbl.find_opt mgr.status_message_factories key in
  Hashtbl.remove mgr.channel_notifiers key;
  Hashtbl.remove mgr.status_message_factories key;
  Lwt.finalize f (fun () ->
      (match prev_notify with
      | Some n -> Hashtbl.replace mgr.channel_notifiers key n
      | None -> ());
      (match prev_factory with
      | Some fac -> Hashtbl.replace mgr.status_message_factories key fac
      | None -> ());
      Lwt.return_unit)

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
              m "[%s] Queued inbound message for busy session (queue depth: %d)"
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
        m "[%s] Injecting %d queued message(s) into session" key count);
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

let compaction_notice (info : Agent.compaction_info) =
  let pre_k = info.pre_tokens / 1000 in
  let post_k = info.post_tokens / 1000 in
  let cw_k = info.context_window / 1000 in
  Printf.sprintf
    "\xF0\x9F\x97\x9C\xEF\xB8\x8F Compacting conversation history (%dk \
     \xe2\x86\x92 %dk tokens, context window: %dk)"
    pre_k post_k cw_k

let notify_compaction_if_needed ?notify compaction_info =
  match (compaction_info, notify) with
  | Some info, Some send ->
      Lwt.catch
        (fun () -> send (compaction_notice info))
        (fun exn ->
          Logs.warn (fun m ->
              m "Failed to send compaction notice: %s" (Printexc.to_string exn));
          Lwt.return_unit)
  | _ -> Lwt.return_unit

let event_display_text (msg : Provider.message) =
  if msg.role <> "event" then None
  else
    let text = String.trim msg.content in
    let len = String.length text in
    if len >= 2 && text.[0] = '[' && text.[len - 1] = ']' then
      Some (String.sub text 1 (len - 2))
    else Some text

let notify_event_text ?notify ?on_chunk text =
  let send =
    match notify with
    | Some send ->
        let text = if Option.is_some on_chunk then text ^ "\n\n" else text in
        Some (fun () -> send text)
    | None ->
        Option.map
          (fun send -> fun () -> send (Provider.Delta (text ^ "\n\n")))
          on_chunk
  in
  match send with
  | None -> Lwt.return_unit
  | Some send ->
      Lwt.catch send (fun exn ->
          Logs.warn (fun m ->
              m "Failed to send workspace refresh notice: %s"
                (Printexc.to_string exn));
          Lwt.return_unit)

let notify_event_messages ?notify ?on_chunk messages =
  Lwt_list.iter_s
    (fun msg ->
      match event_display_text msg with
      | Some text -> notify_event_text ?notify ?on_chunk text
      | None -> Lwt.return_unit)
    messages

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
      (match mgr.db with
      | Some db -> (
          match Memory.load_session_workspace_state ~db ~session_key:key with
          | Some observed_active_workspace_files ->
              Agent.restore_observed_active_workspace_files agent
                observed_active_workspace_files
          | None -> Agent.sync_observed_active_workspace_files agent)
      | None -> ());
      let mutex = Lwt_mutex.create () in
      let interrupt = ref None in
      let triple = (agent, mutex, interrupt) in
      Hashtbl.replace mgr.sessions key triple;
      triple

let with_session_lock mgr ~key f =
  let open Lwt.Syntax in
  (* Release sessions_lock before blocking on per-session mutex to avoid
     deadlock: other operations (message dispatch, draining, etc.) also need
     sessions_lock; holding it while waiting for a busy session's mutex would
     block everything. *)
  let* agent, mutex, interrupt =
    Lwt_mutex.with_lock mgr.sessions_lock (fun () ->
        let agent, mutex, interrupt = get_or_create_locked mgr ~key in
        Lwt.return (agent, mutex, interrupt))
  in
  let* () = Lwt_mutex.lock mutex in
  Lwt.finalize
    (fun () -> f agent interrupt)
    (fun () ->
      Lwt_mutex.unlock mutex;
      Lwt.return_unit)

let try_session_lock mgr ~key f =
  let open Lwt.Syntax in
  (* Release sessions_lock before checking per-session mutex to avoid holding
     sessions_lock while blocking.  try_session_lock is non-blocking on the
     per-session mutex, but we still release sessions_lock first to stay
     consistent with the deadlock-avoidance pattern. *)
  let* agent, mutex, interrupt =
    Lwt_mutex.with_lock mgr.sessions_lock (fun () ->
        let agent, mutex, interrupt = get_or_create_locked mgr ~key in
        Lwt.return (agent, mutex, interrupt))
  in
  if Lwt_mutex.is_locked mutex then Lwt.return_none
  else
    let* () = Lwt_mutex.lock mutex in
    Lwt.finalize
      (fun () ->
        let* result = f agent interrupt in
        Lwt.return_some result)
      (fun () ->
        Lwt_mutex.unlock mutex;
        Lwt.return_unit)

let with_session_lock_unless_draining mgr ~key ~on_draining f =
  let open Lwt.Syntax in
  (* Release sessions_lock before blocking on per-session mutex to avoid
     deadlock: interrupt_resumable_channel_sessions and start_draining also
     need sessions_lock; if a session is busy with an LLM call, holding
     sessions_lock while waiting for the per-session mutex would block them. *)
  let* state =
    Lwt_mutex.with_lock mgr.sessions_lock (fun () ->
        if mgr.draining then Lwt.return_none
        else
          let agent, mutex, interrupt = get_or_create_locked mgr ~key in
          Lwt.return_some (agent, mutex, interrupt))
  in
  match state with
  | None -> on_draining ()
  | Some (agent, mutex, interrupt) ->
      let* () = Lwt_mutex.lock mutex in
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
            health =
              Background_task.string_of_health
                (Background_task.diagnose_health t);
            elapsed = Background_task.elapsed_string t;
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
    task_tree_summary =
      (match mgr.db with
      | Some db ->
          Task_tree.init_schema db;
          let summary = Task_tree.render_compact ~db ~session_key:key in
          Some summary
      | None -> None);
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

let persist_session_workspace_state mgr ~key agent =
  match mgr.db with
  | Some db when agent.Agent.history <> [] ->
      Memory.store_session_workspace_state ~db ~session_key:key
        ~observed_active_workspace_files:
          agent.Agent.observed_active_workspace_files
  | _ -> ()

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
      end;
      persist_session_workspace_state mgr ~key agent
  | None -> ()

let persist_compacted_history mgr ~key agent =
  match mgr.db with
  | Some db ->
      let messages = List.rev agent.Agent.history in
      Memory.replace_session_messages ~db ~session_key:key messages;
      persist_session_workspace_state mgr ~key agent
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
      if agent.Agent.compacted_mid_turn then begin
        persist_compacted_history mgr ~key agent;
        agent.Agent.compacted_mid_turn <- false
      end
      else persist_new_messages mgr ~key ~history_before:!persisted_up_to agent;
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
      if agent.Agent.compacted_mid_turn then begin
        persist_compacted_history mgr ~key agent;
        agent.Agent.compacted_mid_turn <- false
      end
      else persist_new_messages mgr ~key ~history_before:!persisted_up_to agent;
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
  let refresh_messages =
    match Agent.note_external_workspace_refresh_if_needed agent with
    | Some msg -> [ msg ]
    | None -> []
  in
  let* () = notify_event_messages ?notify refresh_messages in
  let* compaction_info =
    Agent.prepare_turn_history agent ~user_message:effective_message
      ~content_parts ~workspace_refresh_checked:true ?db:mgr.db ()
  in
  let compacted = Option.is_some compaction_info in
  let* () = notify_compaction_if_needed ?notify compaction_info in
  if compacted then persist_compacted_history mgr ~key agent
  else begin
    if refresh_messages <> [] then persist_new_messages mgr ~key ~history_before agent;
    persist_new_messages mgr ~key ~history_before agent
  end;
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
    (match mgr.db with
    | Some db ->
        List.iter
          (fun msg -> Memory.store_message ~db ~session_key:key msg)
          new_msgs;
        persisted_up_to := List.length agent.Agent.history
    | None -> ());
    notify_event_messages ?notify new_msgs
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
            if agent.Agent.compacted_mid_turn then begin
              persist_compacted_history mgr ~key agent;
              agent.Agent.compacted_mid_turn <- false
            end
            else
              persist_new_messages mgr ~key ~history_before:!persisted_up_to
                agent;
            set_response_deferred mgr ~key;
            Lwt.return draining_message
        | exn ->
            (* Persist whatever state we have before propagating the error.
               If compact_history ran mid-turn, the tool result is already in
               agent.history but the DB still has the pre-result snapshot that
               compact_fn wrote.  Overwrite it now so we don't leave an
               orphaned tool call that breaks every subsequent LLM call. *)
            if agent.Agent.compacted_mid_turn then begin
              persist_compacted_history mgr ~key agent;
              agent.Agent.compacted_mid_turn <- false
            end
            else
              persist_new_messages mgr ~key ~history_before:!persisted_up_to
                agent;
            Lwt.fail exn)
  in
  (match notify with
  | Some _
    when mgr.config.agent_defaults.show_thinking
         || mgr.config.agent_defaults.show_tool_calls ->
      ()
  | _ ->
      if not (response_deferred mgr ~key) then begin
        if agent.Agent.compacted_mid_turn then begin
          persist_compacted_history mgr ~key agent;
          agent.Agent.compacted_mid_turn <- false
        end
        else
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
        | Some dp -> dp.before_turn queued.message_id
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
      let* () =
        match on_drain_progress with
        | Some dp -> dp.after_turn queued.message_id
        | None -> Lwt.return_unit
      in
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
  with_live_activity mgr ~key (fun () ->
      drain_queued_messages_loop mgr ~key agent interrupt ?on_drain_progress
        ~drained_any:false ())

let rec turn mgr ~key ~message ?(content_parts = []) ?(attachments = [])
    ?channel_name ?channel_type ?sender_id ?sender_name ?channel ?channel_id
    ?message_id ?before_drain () =
  with_live_activity mgr ~key (fun () ->
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
              message_id;
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
                    let* () =
                      match before_drain with
                      | Some f -> f response
                      | None -> Lwt.return_unit
                    in
                    let* () =
                      drain_queued_messages mgr ~key agent interrupt ()
                    in
                    Lwt.return response)))

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
let get_db mgr = mgr.db

let update_config ?(source = "") mgr config =
  let old_model = mgr.config.Runtime_config.agent_defaults.primary_model in
  let new_model = config.Runtime_config.agent_defaults.primary_model in
  mgr.config <- config;
  if old_model <> new_model then
    if source = "" then
      Logs.info (fun m ->
          m "Primary model changed from '%s' to '%s'" old_model new_model)
    else
      Logs.info (fun m ->
          m "Primary model changed from '%s' to '%s' [source: %s]" old_model
            new_model source);
  Hashtbl.iter
    (fun key (agent, _, _) ->
      agent.Agent.config <- config;
      Agent.sync_observed_active_workspace_files agent;
      persist_session_workspace_state mgr ~key agent)
    mgr.sessions

let turn_stream mgr ~key ~message ?(content_parts = []) ?(attachments = [])
    ?channel_name ?channel_type ?sender_id ?sender_name ?channel ?channel_id
    ?message_id ?on_drain_progress ?before_drain ~on_chunk () =
  with_live_activity mgr ~key (fun () ->
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
              message_id;
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
                    let notify = find_registered_notifier mgr ~key in
                    let refresh_messages =
                      match
                        Agent.note_external_workspace_refresh_if_needed agent
                      with
                      | Some msg -> [ msg ]
                      | None -> []
                    in
                    let* () =
                      notify_event_messages ?notify ~on_chunk refresh_messages
                    in
                    let* compaction_info =
                      Agent.prepare_turn_history agent
                        ~user_message:effective_message ~content_parts
                        ~workspace_refresh_checked:true ?db:mgr.db ()
                    in
                    let compacted = Option.is_some compaction_info in
                    let* () =
                      notify_compaction_if_needed
                        ~notify:(fun text ->
                          on_chunk (Provider.Delta (text ^ "\n")))
                        compaction_info
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
                      (match mgr.db with
                      | Some db ->
                          List.iter
                            (fun msg ->
                              Memory.store_message ~db ~session_key:key msg)
                            new_msgs;
                          persisted_up_to := List.length agent.Agent.history
                      | None -> ());
                      notify_event_messages ?notify ~on_chunk new_msgs
                    in
                    let inject_messages () =
                      let msgs =
                        take_all_queued_messages_for_injection mgr ~key
                      in
                      List.map
                        (fun (qm : queued_message) ->
                          queued_message_prompt
                            (effective_message_for_turn ~message:qm.message
                               ?channel_name:qm.channel_name
                               ?channel_type:qm.channel_type
                               ?sender_id:qm.sender_id
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
                                ~session_key:key ~interrupt_check
                                ~inject_messages ?runtime_context
                                ~history_prepared:true ~on_history_update
                                ~on_chunk ())
                        (function
                          | Agent.Restart_requested ->
                              if agent.Agent.compacted_mid_turn then begin
                                persist_compacted_history mgr ~key agent;
                                agent.Agent.compacted_mid_turn <- false
                              end
                              else
                                persist_new_messages mgr ~key
                                  ~history_before:!persisted_up_to agent;
                              set_response_deferred mgr ~key;
                              let* () =
                                on_chunk (Provider.Delta draining_message)
                              in
                              let* () = on_chunk Provider.Done in
                              Lwt.return draining_message
                          | exn ->
                              if agent.Agent.compacted_mid_turn then begin
                                persist_compacted_history mgr ~key agent;
                                agent.Agent.compacted_mid_turn <- false
                              end
                              else
                                persist_new_messages mgr ~key
                                  ~history_before:!persisted_up_to agent;
                              Lwt.fail exn)
                    in
                    if not (response_deferred mgr ~key) then begin
                      if agent.Agent.compacted_mid_turn then begin
                        persist_compacted_history mgr ~key agent;
                        agent.Agent.compacted_mid_turn <- false
                      end
                      else
                        persist_new_messages mgr ~key
                          ~history_before:!persisted_up_to agent;
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
                      match before_drain with
                      | Some f -> f response
                      | None -> Lwt.return_unit
                    in
                    let* () =
                      drain_queued_messages mgr ~key agent interrupt
                        ?on_drain_progress ()
                    in
                    Lwt.return response)))

let reset mgr ~key =
  let open Lwt.Syntax in
  let* held_mutex =
    Lwt_mutex.with_lock mgr.sessions_lock (fun () ->
        let clear_db () =
          match mgr.db with
          | Some db ->
              let pending_cleared = Memory.queue_clear ~db ~session_key:key in
              if pending_cleared > 0 then
                Logs.info (fun m ->
                    m
                      "Session reset cleared %d pending inbound queue rows for \
                       %s"
                      pending_cleared key);
              Memory.clear_session ~db ~session_key:key
          | None -> ()
        in
        match Hashtbl.find_opt mgr.sessions key with
        | Some (_, mutex, _) ->
            let* () = Lwt_mutex.lock mutex in
            clear_db ();
            Hashtbl.remove mgr.deferred_responses key;
            Hashtbl.remove mgr.queued_messages key;
            Hashtbl.remove mgr.continuation_checks key;
            unregister_channel_notifier mgr ~key;
            unregister_rich_notifier mgr ~key;
            Hashtbl.remove mgr.sessions key;
            Lwt.return (Some mutex)
        | None ->
            clear_db ();
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
  Logs.info (fun m ->
      m "/compact requested for session %s — starting compaction" key);
  (* Use with_session_lock which calls get_or_create_locked, ensuring the
     session is loaded from DB if it exists but isn't in memory yet (e.g.
     after daemon restart). *)
  with_session_lock mgr ~key (fun agent _interrupt ->
      let* compaction_info = Agent.force_compact_history agent ?db:mgr.db () in
      match compaction_info with
      | Some _ ->
          persist_compacted_history mgr ~key agent;
          Lwt.return (Ok true)
      | None -> Lwt.return (Ok false))

let rec schedule_autonomous_continuation ?delay ?(around_turn = fun f -> f ())
    ?(on_response = fun _response -> Lwt.return_unit) mgr ~key =
  let delay =
    match delay with
    | Some d -> d
    | None -> mgr.config.agent_defaults.autonomous_continuation_delay
  in
  let open Lwt.Syntax in
  if not mgr.config.agent_defaults.autonomous_continuation_enabled then
    Lwt.return_unit
  else
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
          let compaction_suggestion =
            compaction_suggestion_for_prompt mgr ~key
          in
          let prompt_with_suggestion =
            autonomous_continuation_prompt ^ compaction_suggestion
          in
          let* () =
            if mgr.config.agent_defaults.send_continuation_checkin then
              match find_registered_notifier mgr ~key with
              | Some notify ->
                  let labeled =
                    "[automatic continuation check-in]\n"
                    ^ prompt_with_suggestion
                  in
                  Lwt.catch
                    (fun () -> notify labeled)
                    (fun _ -> Lwt.return_unit)
              | None -> Lwt.return_unit
            else Lwt.return_unit
          in
          let run_continuation_turn () =
            Lwt.catch
              (fun () ->
                around_turn (fun () ->
                    turn mgr ~key ~message:prompt_with_suggestion ()))
              (fun exn ->
                Logs.warn (fun m ->
                    m "Autonomous continuation prompt failed for %s: %s" key
                      (Printexc.to_string exn));
                Lwt.return "")
          in
          let* response =
            if mgr.config.agent_defaults.send_continuation_checkin then
              run_continuation_turn ()
            else with_suppressed_channel_output mgr ~key run_continuation_turn
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
            schedule_autonomous_continuation ~delay ~around_turn ~on_response
              mgr ~key
          end

let process_autonomous_turn_result ?delay ?(around_turn = fun f -> f ())
    ?(on_response = fun _response -> Lwt.return_unit) mgr ~key ~response =
  let delay =
    match delay with
    | Some d -> d
    | None -> mgr.config.agent_defaults.autonomous_continuation_delay
  in
  let trimmed = String.trim response in
  if trimmed = "" || trimmed = "HEARTBEAT_OK" then Lwt.return_unit
  else if trimmed = autonomous_stay_idle_message then
    with_continuation_state mgr ~key (fun state ->
        state.disarmed <- true;
        clear_pending_continuation state;
        Lwt.return_unit)
  else
    schedule_autonomous_continuation ~delay ~around_turn ~on_response mgr ~key
