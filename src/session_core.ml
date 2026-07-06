include Session_types

let sanitize_session_key key =
  let buf = Buffer.create (String.length key) in
  String.iter
    (fun c ->
      match c with
      | '/' | '\\' | '\x00' -> Buffer.add_char buf '_'
      | _ -> Buffer.add_char buf c)
    key;
  (* Collapse any ".." to "__" to prevent path traversal *)
  let s = Buffer.contents buf in
  let len = String.length s in
  let result = Bytes.of_string s in
  for i = 0 to len - 2 do
    if Bytes.get result i = '.' && Bytes.get result (i + 1) = '.' then begin
      Bytes.set result i '_';
      Bytes.set result (i + 1) '_'
    end
  done;
  Bytes.to_string result

let queued_message_response = "__clawq_message_queued__"

let draining_message =
  "Daemon is restarting, please wait a moment and try again."

let is_admin_stop_message ~user_group message =
  match user_group with
  | Some "admin" ->
      let normalized = String.lowercase_ascii (String.trim message) in
      normalized = "stop" || normalized = "/stop"
  | _ -> false

let is_queued_admin_stop_message (msg : queued_message) =
  (not msg.bang) && is_admin_stop_message ~user_group:msg.user_group msg.message

include Session_queue
include Session_activity

let postmortem_session_prefix = "__postmortem_"

let rec root_postmortem_session_key session_key =
  let prefix_len = String.length postmortem_session_prefix in
  if
    String.length session_key >= prefix_len
    && String.sub session_key 0 prefix_len = postmortem_session_prefix
  then
    root_postmortem_session_key
      (String.sub session_key prefix_len
         (String.length session_key - prefix_len))
  else session_key

let is_postmortem_session_key key =
  let prefix_len = String.length postmortem_session_prefix in
  String.length key >= prefix_len
  && String.sub key 0 prefix_len = postmortem_session_prefix

let load_restorable_history mgr ~key =
  let key = sanitize_session_key key in
  match mgr.db with
  | Some db when not (is_postmortem_session_key key) ->
      let history = Memory.load_history ~db ~session_key:key in
      if history = [] then []
      else
        let sanitized = Message_history.ensure_tool_group_integrity history in
        if List.length sanitized <> List.length history then
          Memory.replace_session_messages ~db ~session_key:key sanitized;
        sanitized
  | _ -> []

let get_context_usage_percent mgr ~key =
  let key = sanitize_session_key key in
  match Hashtbl.find_opt mgr.sessions key with
  | Some (agent, _, _) ->
      let estimated_tokens = Agent.estimate_history_tokens agent.history in
      let context_window = Agent.context_window_for_agent agent in
      if context_window > 0 then
        let percent = min 100 (estimated_tokens * 100 / context_window) in
        Some (percent, estimated_tokens, context_window)
      else None
  | None -> None

let skill_loaded_in_context mgr ~key name =
  let key = sanitize_session_key key in
  match Hashtbl.find_opt mgr.sessions key with
  | Some (agent, _, _) ->
      Skill_dedup.skill_loaded_in_history agent.Agent.history name
  | None ->
      Skill_dedup.skill_loaded_in_history
        (load_restorable_history mgr ~key)
        name

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
    silent_channel_notifiers = Hashtbl.create 16;
    alert_channel_notifiers = Hashtbl.create 16;
    status_message_factories = Hashtbl.create 16;
    connector_capabilities = Hashtbl.create 16;
    interrupt_finalizers = Hashtbl.create 8;
    rich_notifiers = Hashtbl.create 16;
    deferred_responses = Hashtbl.create 16;
    queued_messages = Hashtbl.create 16;
    live_activity = Hashtbl.create 16;
    continuation_checks = Hashtbl.create 16;
    special_command_handler = None;
    observer_last_checked = Hashtbl.create 8;
    postmortem_circuit_breakers = Hashtbl.create 8;
    pending_questions = Hashtbl.create 8;
    question_callbacks = Hashtbl.create 16;
    session_callbacks = Hashtbl.create 16;
  }

let is_draining mgr = mgr.draining

let set_special_command_handler mgr handler =
  mgr.special_command_handler <- Some handler

let start_draining mgr =
  Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
    ~label:"sessions_lock" mgr.sessions_lock (fun () ->
      mgr.draining <- true;
      (* Interrupt ALL active sessions so long-running tool calls (e.g.
         background_task_wait) can detect imminent restart and return early,
         rather than blocking drain until the drain timeout forces restart. *)
      Hashtbl.iter
        (fun _key (_agent, _mutex, interrupt) ->
          interrupt := Some Agent.restart_interrupt_token)
        mgr.sessions;
      Lwt.return_unit)

let stop_draining mgr =
  Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
    ~label:"sessions_lock" mgr.sessions_lock (fun () ->
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
  Hashtbl.remove mgr.silent_channel_notifiers key;
  Hashtbl.remove mgr.alert_channel_notifiers key;
  Hashtbl.remove mgr.status_message_factories key;
  Hashtbl.remove mgr.connector_capabilities key;
  Hashtbl.remove mgr.interrupt_finalizers key;
  Hashtbl.remove mgr.rich_notifiers key

let register_silent_channel_notifier mgr ~key notify =
  Hashtbl.replace mgr.silent_channel_notifiers key notify

let find_silent_channel_notifier mgr ~key =
  Hashtbl.find_opt mgr.silent_channel_notifiers key

let register_alert_channel_notifier mgr ~key notify =
  Hashtbl.replace mgr.alert_channel_notifiers key notify

let find_alert_channel_notifier mgr ~key =
  Hashtbl.find_opt mgr.alert_channel_notifiers key

let register_status_message_factory mgr ~key factory =
  Hashtbl.replace mgr.status_message_factories key factory

let register_connector_capabilities mgr ~key caps =
  Hashtbl.replace mgr.connector_capabilities key caps

let find_connector_capabilities mgr ~key =
  Hashtbl.find_opt mgr.connector_capabilities key

let register_interrupt_finalizer mgr ~key cb =
  Hashtbl.replace mgr.interrupt_finalizers key cb

let unregister_interrupt_finalizer mgr ~key =
  Hashtbl.remove mgr.interrupt_finalizers key

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

let should_suppress_response response =
  is_queued_message_response response || Group_chat_filter.is_no_reply response

let queueable_channel_key key =
  match Restart_notify.parse_channel_from_key key with
  | Some ("web", _) -> false
  | Some _ -> true
  | None -> false

let run_interrupt_finalizer mgr ~key =
  match Hashtbl.find_opt mgr.interrupt_finalizers key with
  | Some cb ->
      Lwt.async (fun () ->
          Lwt.catch cb (fun exn ->
              Logs.warn (fun m ->
                  m "[%s] interrupt finalizer error: %s" key
                    (Printexc.to_string exn));
              Lwt.return_unit))
  | None -> ()

let question_cancelled_sentinel = "__clawq_question_cancelled__"

let register_pending_question mgr ~key =
  let promise, resolver = Lwt.wait () in
  Hashtbl.replace mgr.pending_questions key resolver;
  (promise, resolver)

let cancel_pending_question mgr ~key =
  match Hashtbl.find_opt mgr.pending_questions key with
  | Some resolver ->
      Hashtbl.remove mgr.pending_questions key;
      Lwt.wakeup_later resolver question_cancelled_sentinel
  | None -> ()

let has_pending_question mgr ~key = Hashtbl.mem mgr.pending_questions key

let handle_queued_admin_stop mgr ~key interrupt (msg : queued_message) =
  Logs.info (fun m -> m "[%s] Handling queued admin stop command" key);
  cancel_pending_question mgr ~key;
  interrupt := Some Agent.stop_interrupt_token;
  delete_queued_message_row mgr msg

let stop_busy_session_if_admin_stop mgr ~key ~message ?user_group () =
  if not (is_admin_stop_message ~user_group message) then Lwt.return_false
  else
    Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
      ~label:"sessions_lock" mgr.sessions_lock (fun () ->
        match Hashtbl.find_opt mgr.sessions key with
        | Some (_, mutex, interrupt) when Lwt_mutex.is_locked mutex ->
            cancel_pending_question mgr ~key;
            interrupt := Some Agent.stop_interrupt_token;
            Lwt.return_true
        | _ -> Lwt.return_false)

let register_question_callbacks mgr ~key ~callbacks =
  Logs.debug (fun m ->
      m "[%s] Registering %d question callback(s)" key (List.length callbacks));
  let cb_ids = List.map fst callbacks in
  Hashtbl.replace mgr.session_callbacks key cb_ids;
  List.iter
    (fun (cb_id, answer_text) ->
      Hashtbl.replace mgr.question_callbacks cb_id answer_text)
    callbacks

let resolve_question_callback mgr ~key ~callback_id =
  match Hashtbl.find_opt mgr.question_callbacks callback_id with
  | Some answer_text -> (
      Logs.debug (fun m ->
          m "[%s] Resolving question callback %s -> %s" key callback_id
            answer_text);
      (* Clean up sibling callbacks registered for the same question *)
      (match Hashtbl.find_opt mgr.session_callbacks key with
      | Some cb_ids ->
          List.iter
            (fun cb_id -> Hashtbl.remove mgr.question_callbacks cb_id)
            cb_ids;
          Hashtbl.remove mgr.session_callbacks key
      | None -> Hashtbl.remove mgr.question_callbacks callback_id);
      match Hashtbl.find_opt mgr.pending_questions key with
      | Some resolver ->
          Hashtbl.remove mgr.pending_questions key;
          Lwt.wakeup_later resolver answer_text;
          true
      | None ->
          Logs.warn (fun m ->
              m "[%s] Question callback %s matched but no pending question" key
                callback_id);
          false)
  | None -> false

let clear_question_callbacks mgr ~key:_ ~callback_ids =
  List.iter
    (fun cb_id -> Hashtbl.remove mgr.question_callbacks cb_id)
    callback_ids

let enqueue_message_if_busy mgr ~key ?raw_message queued_message =
  Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
    ~label:"sessions_lock" mgr.sessions_lock (fun () ->
      let control_message =
        Option.value raw_message ~default:queued_message.message
      in
      let is_bang =
        String.length control_message > 0 && control_message.[0] = '!'
      in
      let is_admin_stop =
        is_admin_stop_message ~user_group:queued_message.user_group
          control_message
      in
      (* If a pending question is waiting, intercept the reply. *)
      let consumed_by_question =
        match Hashtbl.find_opt mgr.pending_questions key with
        | Some resolver when (not is_bang) && not is_admin_stop ->
            Hashtbl.remove mgr.pending_questions key;
            Lwt.wakeup_later resolver queued_message.message;
            true
        | Some _ when is_bang || is_admin_stop ->
            (* Bang falls through to the normal queuing path; admin stop falls
               through to the clean stop path. Both cancel active questions. *)
            cancel_pending_question mgr ~key;
            false
        | _ -> false
      in
      if consumed_by_question then Lwt.return_true
      else
        match Hashtbl.find_opt mgr.sessions key with
        | Some (_, mutex, interrupt)
          when Lwt_mutex.is_locked mutex && is_admin_stop ->
            interrupt := Some Agent.stop_interrupt_token;
            Lwt.return_true
        | Some (_, mutex, interrupt)
          when Lwt_mutex.is_locked mutex && queueable_channel_key key ->
            let existing =
              match Hashtbl.find_opt mgr.queued_messages key with
              | Some msgs -> msgs
              | None -> []
            in
            let msg =
              { queued_message with bang = is_bang; deferred_followup = false }
            in
            let msg =
              {
                msg with
                inbound_queue_id =
                  persist_queued_message mgr ~key ~source:"live" msg;
              }
            in
            Hashtbl.replace mgr.queued_messages key (existing @ [ msg ]);
            Logs.info (fun m ->
                m
                  "[%s] Queued inbound message for busy session (queue depth: \
                   %d)"
                  key
                  (List.length existing + 1));
            (* NOTE: queued_message_interrupt_token does not interrupt the
                 normal agent loop (agent.ml checks it but continues looping).
                 Its effects: (1) inject_messages picks up queued messages
                 between tool-call batches when wired by run_locked_turn, and
                 (2) restart-resume turns remap it to a real stop signal in
                 daemon_resume.ml:restart_resume_interrupt_check. *)
            if !interrupt = None then
              interrupt := Some Agent.queued_message_interrupt_token;
            run_interrupt_finalizer mgr ~key;
            Lwt.return_true
        | _ -> Lwt.return_false)

let take_all_queued_messages_for_injection ?interrupt mgr ~key =
  let existing =
    match Hashtbl.find_opt mgr.queued_messages key with
    | Some msgs -> msgs
    | None -> []
  in
  let injected_rev, remaining_rev =
    List.fold_left
      (fun (injected, remaining) (msg : queued_message) ->
        if msg.deferred_followup then (injected, msg :: remaining)
        else
          match interrupt with
          | Some interrupt when is_queued_admin_stop_message msg ->
              handle_queued_admin_stop mgr ~key interrupt msg;
              (injected, remaining)
          | _ ->
              delete_queued_message_row mgr msg;
              (msg :: injected, remaining))
      ([], []) existing
  in
  let remaining = List.rev remaining_rev in
  if remaining = [] then Hashtbl.remove mgr.queued_messages key
  else Hashtbl.replace mgr.queued_messages key remaining;
  let msgs = List.rev injected_rev in
  let count = List.length msgs in
  if count > 0 then
    Logs.info (fun m ->
        m "[%s] Injecting %d queued message(s) into session" key count);
  msgs

(* Heartbeat and debug management extracted to Session_heartbeat *)

let interrupt_resumable_channel_sessions mgr =
  Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
    ~label:"sessions_lock" mgr.sessions_lock (fun () ->
      Hashtbl.iter
        (fun key _ ->
          match
            ( Restart_notify.parse_channel_from_key key,
              Hashtbl.find_opt mgr.sessions key )
          with
          | Some (channel, _), Some (_, _, interrupt)
            when Session_heartbeat.resumable_channel channel ->
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
      | None -> Hashtbl.remove mgr.channel_notifiers key);
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

let resolve_agent_template_for_key mgr ~key =
  let bindings = mgr.config.Runtime_config.agent_bindings in
  if bindings = [] then None
  else
    let channel_id, sender_id =
      match Restart_notify.parse_channel_from_key key with
      | Some (_channel, cid) -> (cid, "")
      | None -> ("", "")
    in
    let agent_name =
      Agent_router.resolve ~bindings ~channel_id ~sender_id ~guild_id:None
    in
    if agent_name = "default" then None else Agent_template.resolve agent_name

let get_or_create_locked mgr ~key =
  let key = sanitize_session_key key in
  match Hashtbl.find_opt mgr.sessions key with
  | Some triple -> triple
  | None ->
      let agent_template = resolve_agent_template_for_key mgr ~key in
      let tool_registry =
        match (agent_template, mgr.tool_registry) with
        | Some tmpl, Some reg ->
            Some (Agent_template.filter_tool_registry reg tmpl)
        | _ -> mgr.tool_registry
      in
      (* CWD precedence: /repo explicit (set at turn time via set_effective_cwd)
         > DB room workspace > config workspace default > agent_template.cwd >
         global. At session creation, /repo explicit hasn't fired yet, so we
         resolve the remaining layers.  Resolve before Agent.create so that
         project_docs_digests are computed with the correct effective_cwd. *)
      let initial_cwd =
        Session_room_profile.resolve_initial_cwd mgr ~session_key:key ~db:mgr.db
          ~agent_template
      in
      (* Resolve effective access to get layered instructions with provenance.
         This ensures deterministic ordering: default → workspace → channel → room.
         Uses resolve_room_profile_for_session for proper child-thread/routine handling. *)
      let instruction_items =
        Session_room_profile.resolve_instruction_items_for_session mgr ~key
      in
      let agent =
        Agent.create ~config:mgr.config ?tool_registry ?agent_template
          ?cwd:initial_cwd ~instruction_items ()
      in
      let history = load_restorable_history mgr ~key in
      if history <> [] then begin
        agent.history <- List.rev history;
        Logs.info (fun m ->
            m "Restored %d messages for session %s" (List.length history) key)
      end;
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
      (* effective_cwd already set via Agent.create ~cwd:initial_cwd above *)
      (* Model resolution:
         session DB override > room profile > channel default > global *)
      (match Session_room_profile.resolve_model_for_session mgr ~key with
      | Some model ->
          let cfg = agent.Agent.config in
          let agent_defaults =
            { cfg.agent_defaults with primary_model = model }
          in
          agent.Agent.config <- { cfg with agent_defaults }
      | None -> ());
      (* Template fields: room profile system_prompt / max_tool_iterations *)
      Session_room_profile.apply_room_profile_template_fields mgr ~key agent;
      let mutex = Lwt_mutex.create () in
      let interrupt = ref None in
      let triple = (agent, mutex, interrupt) in
      Hashtbl.replace mgr.sessions key triple;
      triple

let with_session_lock ?session_warn_timeout ?session_fatal_timeout mgr ~key f =
  let open Lwt.Syntax in
  let key = sanitize_session_key key in
  (* Release sessions_lock before blocking on per-session mutex to avoid
     deadlock: other operations (message dispatch, draining, etc.) also need
     sessions_lock; holding it while waiting for a busy session's mutex would
     block everything. If reset() replaces/removes the session while we are
     waiting, discard the stale mutex and retry with the current session. *)
  let rec acquire_current_session () =
    let* _agent, mutex, _interrupt =
      Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
        ~label:(Printf.sprintf "sessions_lock/with_session_lock[%s]" key)
        mgr.sessions_lock (fun () ->
          let agent, mutex, interrupt = get_or_create_locked mgr ~key in
          Lwt.return (agent, mutex, interrupt))
    in
    let* () =
      Lwt_util.lock_with_timeout ?warn_timeout:session_warn_timeout
        ?fatal_timeout:session_fatal_timeout
        ~label:(Printf.sprintf "session_mutex/with_session_lock[%s]" key)
        mutex
    in
    let* state =
      Lwt.catch
        (fun () ->
          Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
            ~label:
              (Printf.sprintf "sessions_lock/with_session_lock_recheck[%s]" key)
            mgr.sessions_lock (fun () ->
              match Hashtbl.find_opt mgr.sessions key with
              | Some (agent, current_mutex, interrupt)
                when current_mutex == mutex ->
                  Lwt.return (`Acquired (agent, current_mutex, interrupt))
              | Some _ ->
                  Logs.warn (fun m ->
                      m
                        "Session %s was replaced while waiting for its mutex; \
                         retrying with the current session mutex"
                        key);
                  Lwt.return `Stale
              | None ->
                  Logs.warn (fun m ->
                      m
                        "Session %s was reset while waiting for its mutex; \
                         re-creating fresh session"
                        key);
                  Lwt.return `Stale))
        (fun exn ->
          Lwt_mutex.unlock mutex;
          Lwt.fail exn)
    in
    match state with
    | `Acquired state -> Lwt.return state
    | `Stale ->
        Lwt_mutex.unlock mutex;
        acquire_current_session ()
  in
  let* agent, mutex, interrupt = acquire_current_session () in
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
    Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
      ~label:(Printf.sprintf "sessions_lock/try_session_lock[%s]" key)
      mgr.sessions_lock (fun () ->
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
    Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
      ~label:
        (Printf.sprintf "sessions_lock/with_session_lock_unless_draining[%s]"
           key) mgr.sessions_lock (fun () ->
        if mgr.draining then Lwt.return_none
        else
          let agent, mutex, interrupt = get_or_create_locked mgr ~key in
          Lwt.return_some (agent, mutex, interrupt))
  in
  match state with
  | None -> on_draining ()
  | Some (agent, mutex, interrupt) ->
      let* () =
        Lwt_util.lock_with_timeout
          ~label:
            (Printf.sprintf
               "session_mutex/with_session_lock_unless_draining[%s]" key)
          mutex
      in
      Lwt.finalize
        (fun () -> f agent interrupt)
        (fun () ->
          Lwt_mutex.unlock mutex;
          Lwt.return_unit)

let set_interrupt_if_present mgr ~key message =
  Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
    ~label:"sessions_lock" mgr.sessions_lock (fun () ->
      (match Hashtbl.find_opt mgr.sessions key with
      | Some (_, _, interrupt) -> interrupt := Some message
      | None -> ());
      Lwt.return_unit)

let interrupt_check_if_present mgr ~key () =
  match Hashtbl.find_opt mgr.sessions key with
  | Some (_, _, interrupt) -> !interrupt
  | None -> None

include Session_context
include Session_persistence

let runtime_context_block mgr ~key =
  with_session_lock mgr ~key (fun agent _interrupt ->
      let details =
        runtime_context_details mgr ~agent ~key ~compacted_before_turn:false
      in
      let text =
        match
          Prompt_builder.build_runtime_context ~config:mgr.config
            ~force_full:true ~details ()
        with
        | Some ctx -> ctx
        | None -> "(dynamic prompt disabled -- no runtime context generated)"
      in
      Lwt.return text)

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

let heartbeat_noop_suffix_matches ~heartbeat_prompt = function
  | [ (user_msg : Provider.message); (assistant_msg : Provider.message) ] ->
      user_msg.role = "user"
      && user_msg.content = heartbeat_prompt
      && user_msg.content_parts = []
      && user_msg.tool_calls = []
      && user_msg.tool_call_id = None
      && assistant_msg.role = "assistant"
      && String.trim assistant_msg.content = "HEARTBEAT_OK"
      && assistant_msg.content_parts = []
      && assistant_msg.tool_calls = []
      && assistant_msg.tool_call_id = None
  | _ -> false

let split_history_at n history =
  let rec loop i acc rest =
    if i <= 0 then (List.rev acc, rest)
    else
      match rest with
      | [] -> (List.rev acc, [])
      | x :: xs -> loop (i - 1) (x :: acc) xs
  in
  loop n [] history

let prune_noop_heartbeat_turn mgr ~key ~before_history ~heartbeat_prompt =
  match mgr.db with
  | None -> Lwt.return_false
  | Some db ->
      with_session_lock mgr ~key (fun agent _interrupt ->
          let current_history = List.rev agent.Agent.history in
          let before_len = List.length before_history in
          let prefix, suffix = split_history_at before_len current_history in
          if
            prefix = before_history
            && List.length current_history = before_len + 2
            && heartbeat_noop_suffix_matches ~heartbeat_prompt suffix
          then begin
            agent.Agent.history <- List.rev before_history;
            Memory.replace_session_messages ~db ~session_key:key before_history;
            persist_session_workspace_state mgr ~key agent;
            Logs.info (fun m ->
                m
                  "Heartbeat: pruned trivial HEARTBEAT_OK turn from normal \
                   history for %s"
                  key);
            Lwt.return_true
          end
          else Lwt.return_false)
