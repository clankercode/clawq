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
  user_group : string option;
  channel : string option;
  channel_id : string option;
  message_id : string option;
  inbound_queue_id : int option;
      (** SQLite inbound_queue row id if persisted for crash recovery. *)
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
  mutable sandbox : Sandbox.t option;
  landlock_enabled : bool;
  db : Sqlite3.db option;
  mutable draining : bool;
  in_flight_count : int ref;
  channel_notifiers : (string, string -> unit Lwt.t) Hashtbl.t;
  silent_channel_notifiers : (string, string -> unit Lwt.t) Hashtbl.t;
  alert_channel_notifiers : (string, string -> unit Lwt.t) Hashtbl.t;
  status_message_factories : (string, unit -> Status_message.t) Hashtbl.t;
  connector_capabilities : (string, Connector_capabilities.t) Hashtbl.t;
  interrupt_finalizers : (string, unit -> unit Lwt.t) Hashtbl.t;
  rich_notifiers :
    (string, Rich_message.t -> Rich_message.send_result Lwt.t) Hashtbl.t;
  deferred_responses : (string, unit) Hashtbl.t;
  queued_messages : (string, queued_message list) Hashtbl.t;
  live_activity : (string, live_activity_state) Hashtbl.t;
  continuation_checks : (string, continuation_state) Hashtbl.t;
  mutable special_command_handler : special_command_handler option;
  observer_last_checked : (string, int) Hashtbl.t;
      (** Maps session_key -> history length at last observer check *)
  postmortem_circuit_breakers : (string, unit) Hashtbl.t;
      (** Root sessions that have already launched a postmortem. *)
  pending_questions : (string, string Lwt.u) Hashtbl.t;
}

type drain_progress = {
  before_turn : string option -> unit Lwt.t;
  after_turn : string option -> unit Lwt.t;
  after_all : unit -> unit Lwt.t;
}

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

let autonomous_stay_idle_message = "STAY_IDLE"

(* STAY_IDLE remains a valid hidden control response for runtime logic, but do
   not mention or spell it out in visible autonomous check-in/keepalive prompt
   text. Advertising the token makes the agent over-index on idling and tempts
   future prompt edits to reintroduce the same behavioral bug. *)
let autonomous_continuation_prompt =
  "Autonomous session check-in: continue working if more remains."

let keepalive_nudge_prompt =
  "[Automated Keepalive Check-In]\n\
   Continue working on your tasks if any remain."

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
  Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
    ~label:"sessions_lock" mgr.sessions_lock (fun () ->
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
  Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
    ~label:"sessions_lock" mgr.sessions_lock (fun () ->
      let state = live_activity_state mgr ~key in
      Lwt.return (snapshot_live_activity state))

let rec wait_for_live_activity_change mgr ~key ~after_generation =
  let open Lwt.Syntax in
  let* snapshot, changed =
    Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
      ~label:"sessions_lock" mgr.sessions_lock (fun () ->
        let state = live_activity_state mgr ~key in
        Lwt.return (snapshot_live_activity state, state.changed))
  in
  if snapshot.generation <> after_generation then Lwt.return snapshot
  else
    let* () = changed in
    wait_for_live_activity_change mgr ~key ~after_generation

let start_live_activity mgr ~key =
  Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
    ~label:"sessions_lock" mgr.sessions_lock (fun () ->
      let state = live_activity_state mgr ~key in
      let was_inactive = state.active_scopes = 0 in
      state.active_scopes <- state.active_scopes + 1;
      if was_inactive then advance_live_activity state;
      Lwt.return_unit)

let stop_live_activity mgr ~key =
  Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
    ~label:"sessions_lock" mgr.sessions_lock (fun () ->
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
  Hashtbl.remove mgr.interrupt_finalizers key

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

let enqueue_message_if_busy mgr ~key queued_message =
  Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
    ~label:"sessions_lock" mgr.sessions_lock (fun () ->
      let is_bang =
        String.length queued_message.message > 0
        && queued_message.message.[0] = '!'
      in
      (* If a pending question is waiting, intercept the reply. *)
      let consumed_by_question =
        match Hashtbl.find_opt mgr.pending_questions key with
        | Some resolver when not is_bang ->
            Hashtbl.remove mgr.pending_questions key;
            Lwt.wakeup_later resolver queued_message.message;
            true
        | Some _ when is_bang ->
            (* Bang cancels the pending question AND falls through to set
               interrupt token via normal queuing path. *)
            cancel_pending_question mgr ~key;
            false
        | _ -> false
      in
      if consumed_by_question then Lwt.return_true
      else
        match Hashtbl.find_opt mgr.sessions key with
        | Some (_, mutex, interrupt)
          when Lwt_mutex.is_locked mutex && queueable_channel_key key ->
            let existing =
              match Hashtbl.find_opt mgr.queued_messages key with
              | Some msgs -> msgs
              | None -> []
            in
            let inbound_queue_id =
              match mgr.db with
              | Some db -> (
                  let payload_json =
                    Yojson.Safe.to_string
                      (`Assoc
                         [
                           ("message", `String queued_message.message);
                           ("bang", `Bool is_bang);
                         ])
                  in
                  try
                    Some
                      (Memory.queue_enqueue ~db ~session_key:key ~source:"live"
                         ~payload_json)
                  with exn ->
                    Logs.warn (fun m ->
                        m "[%s] Failed to persist queued message to SQLite: %s"
                          key (Printexc.to_string exn));
                    None)
              | None -> None
            in
            let msg = { queued_message with inbound_queue_id } in
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
               daemon_util.ml:restart_resume_interrupt_check. *)
            if !interrupt = None then
              interrupt := Some Agent.queued_message_interrupt_token;
            (match Hashtbl.find_opt mgr.interrupt_finalizers key with
            | Some cb ->
                Lwt.async (fun () ->
                    Lwt.catch cb (fun exn ->
                        Logs.warn (fun m ->
                            m "[%s] interrupt finalizer error: %s" key
                              (Printexc.to_string exn));
                        Lwt.return_unit))
            | None -> ());
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
      (match mgr.db with
      | Some db ->
          List.iter
            (fun (msg : queued_message) ->
              match msg.inbound_queue_id with
              | Some qid -> ignore (Memory.queue_delete ~db ~queue_id:qid)
              | None -> ())
            msgs
      | None -> ());
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

let heartbeat_supported_channel = function
  | "telegram" | "slack" | "discord" | "teams" -> true
  | _ -> false

let heartbeat_supported_session_key key =
  match Restart_notify.parse_channel_from_key key with
  | Some (channel, _) -> heartbeat_supported_channel channel
  | None -> false

let heartbeat_unsupported_reason key =
  Printf.sprintf
    "Heartbeat can only be enabled for Telegram, Slack, Discord, or Teams \
     sessions. Session '%s' is not eligible."
    key

let resumable_channel = function
  | "telegram" | "slack" | "discord" -> true
  | _ -> false

let session_heartbeat_opt_in mgr ~key =
  match mgr.db with
  | Some db when heartbeat_supported_session_key key ->
      Memory.session_heartbeat_enabled ~db ~session_key:key
  | _ -> false

let heartbeat_routing_applies mgr ~key =
  mgr.config.heartbeat.enabled && session_heartbeat_opt_in mgr ~key

let set_session_heartbeat mgr ~key ~enabled =
  if not (heartbeat_supported_session_key key) then
    Error (heartbeat_unsupported_reason key)
  else
    match mgr.db with
    | Some db ->
        Memory.set_session_heartbeat ~db ~session_key:key ~enabled;
        Ok ()
    | None -> Error "Heartbeat routing is unavailable (no database)."

let list_heartbeat_session_keys mgr =
  match mgr.db with
  | Some db ->
      Memory.list_heartbeat_session_keys ~db
      |> List.filter heartbeat_supported_session_key
  | None -> []

let session_heartbeat_status_text mgr ~key =
  if not (heartbeat_supported_session_key key) then
    heartbeat_unsupported_reason key
  else
    let session_enabled = session_heartbeat_opt_in mgr ~key in
    let global_enabled = mgr.config.heartbeat.enabled in
    if global_enabled then
      Printf.sprintf "Session %s: heartbeat = %s" key
        (if session_enabled then "on" else "off")
    else
      Printf.sprintf
        "Session %s: heartbeat = %s (global heartbeat disabled in config)" key
        (if session_enabled then "on" else "off")

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
      let agent =
        Agent.create ~config:mgr.config ?tool_registry ?agent_template ()
      in
      let is_postmortem =
        let plen = String.length postmortem_session_prefix in
        String.length key >= plen
        && String.sub key 0 plen = postmortem_session_prefix
      in
      (match mgr.db with
      | Some db ->
          if not is_postmortem then begin
            let history = Memory.load_history ~db ~session_key:key in
            if history <> [] then begin
              let sanitized =
                Message_history.ensure_tool_group_integrity history
              in
              agent.history <- List.rev sanitized;
              Logs.info (fun m ->
                  m "Restored %d messages for session %s"
                    (List.length sanitized) key);
              if List.length sanitized <> List.length history then
                Memory.replace_session_messages ~db ~session_key:key sanitized
            end
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
      (match agent_template with
      | Some tmpl -> (
          match tmpl.Agent_template.cwd with
          | Some cwd when Sys.file_exists cwd && Sys.is_directory cwd ->
              agent.Agent.effective_cwd <- Some cwd
          | _ -> ())
      | None -> ());
      (match mgr.db with
      | Some db -> (
          match Memory.get_session_cwd ~db ~session_key:key with
          | Some cwd -> agent.Agent.effective_cwd <- Some cwd
          | None -> ())
      | None -> ());
      (match mgr.db with
      | Some db -> (
          match Memory.get_session_model_override ~db ~session_key:key with
          | Some model ->
              let cfg = agent.Agent.config in
              let agent_defaults =
                { cfg.agent_defaults with primary_model = model }
              in
              agent.Agent.config <- { cfg with agent_defaults }
          | None -> ())
      | None -> ());
      let mutex = Lwt_mutex.create () in
      let interrupt = ref None in
      let triple = (agent, mutex, interrupt) in
      Hashtbl.replace mgr.sessions key triple;
      triple

let with_session_lock ?session_warn_timeout ?session_fatal_timeout mgr ~key f =
  let open Lwt.Syntax in
  (* Release sessions_lock before blocking on per-session mutex to avoid
     deadlock: other operations (message dispatch, draining, etc.) also need
     sessions_lock; holding it while waiting for a busy session's mutex would
     block everything. *)
  let* agent, mutex, interrupt =
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
      |> List.filter (fun (t : Background_task.task) ->
          match t.Background_task.status with
          | Background_task.Queued | Background_task.Running -> true
          | _ -> false)
      |> List.sort (fun (a : Background_task.task) (b : Background_task.task) ->
          compare a.Background_task.id b.Background_task.id)
      |> List.map (fun (t : Background_task.task) ->
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
    heartbeat_routing_applies = heartbeat_routing_applies mgr ~key;
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
    daemon_uptime_line =
      Daemon_status.daemon_runtime_context_line
        ~pid:(Daemon_status.read_current_daemon_pid ());
    background_tasks = active_background_task_summaries mgr;
    context_usage =
      Some (Agent.runtime_context_usage agent ~compacted_before_turn);
    tunnel_status_line =
      Some ("- Tunnel: " ^ !Prompt_builder.tunnel_status_line_fn ());
    task_tree_summary =
      (match mgr.db with
      | Some db ->
          Task_tree.init_schema db;
          let summary = Task_tree.render_focus ~db ~session_key:key in
          Some summary
      | None -> None);
  }

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

let format_context_block ?channel_name ?channel_type ?sender_id ?sender_name
    ?user_group () =
  let cn = match channel_name with Some n -> n | None -> "cli" in
  let ct = match channel_type with Some t -> t | None -> "dm" in
  let sender_part =
    match (sender_id, sender_name) with
    | Some id, Some name -> Printf.sprintf " sender=@%s (%s)" id name
    | Some id, None -> Printf.sprintf " sender=@%s" id
    | None, Some name -> Printf.sprintf " sender=%s" name
    | None, None -> ""
  in
  let group_part =
    match user_group with
    | Some g -> Printf.sprintf " user_group=%s" g
    | None -> ""
  in
  Printf.sprintf "[Context: channel=%s type=%s%s%s]" cn ct sender_part
    group_part

let inject_attachment_context agent attachments =
  match Prompt_builder.attachment_syntax_block attachments with
  | Some block ->
      agent.Agent.history <-
        Provider.make_message ~role:"system" ~content:block
        :: agent.Agent.history
  | None -> ()

let record_agent_turn mgr ~key ?channel ?channel_id () =
  (match (channel, channel_id) with
  | Some _, None | None, Some _ ->
      Logs.warn (fun m ->
          m
            "record_agent_turn: mismatched channel routing for session %s \
             (channel=%s channel_id=%s); session will not be resumable on \
             restart"
            key
            (Option.value ~default:"<none>" channel)
            (Option.value ~default:"<none>" channel_id))
  | _ -> ());
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

let effective_message_for_turn ~message ?channel_name ?channel_type ?sender_id
    ?sender_name ?user_group () =
  match (channel_name, channel_type, sender_id, sender_name, user_group) with
  | None, None, None, None, None -> message
  | _ ->
      let ctx =
        format_context_block ?channel_name ?channel_type ?sender_id ?sender_name
          ?user_group ()
      in
      let trust_msg =
        match user_group with
        | Some "admin" -> "\n" ^ Admin.trust_description_admin
        | Some "guest" -> "\n" ^ Admin.trust_description_guest
        | _ -> ""
      in
      ctx ^ trust_msg ^ "\n" ^ message

let snapshot_history mgr ~key =
  Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
    ~label:"sessions_lock" mgr.sessions_lock (fun () ->
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

let get_config mgr = mgr.config
let get_tool_registry mgr = mgr.tool_registry
let get_db mgr = mgr.db
let set_sandbox mgr sandbox = mgr.sandbox <- Some sandbox
let session_count mgr = Hashtbl.length mgr.sessions

let active_session_count mgr =
  Hashtbl.fold
    (fun _key state acc -> if state.active_scopes > 0 then acc + 1 else acc)
    mgr.live_activity 0

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

let set_session_model mgr ~key ~model =
  (match Hashtbl.find_opt mgr.sessions key with
  | Some (agent, _, _) ->
      let cfg = agent.Agent.config in
      let agent_defaults = { cfg.agent_defaults with primary_model = model } in
      agent.Agent.config <- { cfg with agent_defaults }
  | None -> ());
  match mgr.db with
  | Some db -> Memory.set_session_model_override ~db ~session_key:key ~model
  | None -> ()

let get_session_effective_model mgr ~key =
  match Hashtbl.find_opt mgr.sessions key with
  | Some (agent, _, _) -> agent.Agent.config.agent_defaults.primary_model
  | None -> (
      match mgr.db with
      | Some db -> (
          match Memory.get_session_model_override ~db ~session_key:key with
          | Some model -> model
          | None -> mgr.config.agent_defaults.primary_model)
      | None -> mgr.config.agent_defaults.primary_model)

let reset mgr ~key =
  let open Lwt.Syntax in
  (* Two-phase reset to avoid deadlock.  Phase 1 holds sessions_lock (quick,
     no blocking on per-session mutex) to clean DB and remove the session from
     all hashtables.  Phase 2 releases sessions_lock first, then waits for an
     in-progress turn by acquiring the per-session mutex, and does a second
     DB cleanup to catch any writes the old turn persisted between phases. *)
  let clear_db () =
    match mgr.db with
    | Some db ->
        let pending_cleared = Memory.queue_clear ~db ~session_key:key in
        if pending_cleared > 0 then
          Logs.info (fun m ->
              m "Session reset cleared %d pending inbound queue rows for %s"
                pending_cleared key);
        Memory.archive_session ~db ~session_key:key;
        Memory.clear_session ~db ~session_key:key
    | None -> ()
  in
  let remove_from_tables () =
    Hashtbl.remove mgr.deferred_responses key;
    Hashtbl.remove mgr.queued_messages key;
    Hashtbl.remove mgr.continuation_checks key;
    Hashtbl.remove mgr.observer_last_checked key;
    Hashtbl.remove mgr.postmortem_circuit_breakers
      (root_postmortem_session_key key);
    unregister_channel_notifier mgr ~key;
    unregister_rich_notifier mgr ~key;
    Hashtbl.remove mgr.sessions key
  in
  (* Phase 1: under sessions_lock — get mutex ref, clean DB, remove session *)
  let* mutex_opt =
    Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
      ~label:(Printf.sprintf "sessions_lock/reset[%s]" key) mgr.sessions_lock
      (fun () ->
        let m =
          match Hashtbl.find_opt mgr.sessions key with
          | Some (_, mutex, _) -> Some mutex
          | None -> None
        in
        clear_db ();
        remove_from_tables ();
        Lwt.return m)
  in
  (* Phase 2: outside sessions_lock — wait for in-progress turn, re-clear DB *)
  let* () =
    match mutex_opt with
    | Some mutex ->
        let* () =
          Lwt_util.lock_with_timeout
            ~label:(Printf.sprintf "session_mutex/reset[%s]" key)
            mutex
        in
        (match mgr.db with
        | Some db -> Memory.clear_session ~db ~session_key:key
        | None -> ());
        Lwt_mutex.unlock mutex;
        Lwt.return_unit
    | None -> Lwt.return_unit
  in
  let active_bg_tasks =
    match mgr.db with
    | Some db -> (
        try
          Background_task.init_schema db;
          Background_task.count_active_for_session ~db ~session_key:key
        with _ -> 0)
    | None -> 0
  in
  Lwt.return active_bg_tasks

(* Step tuple: (name, emoji, started_at option, done_at option)
   None/None = Pending, Some t0/None = Running, Some t0/Some t1 = Done *)
let compact_progress_render ~steps ~overall_start ~finished =
  let buf = Buffer.create 256 in
  if finished then begin
    let dur =
      match
        Status_message.format_duration_opt
          (Unix.gettimeofday () -. overall_start)
      with
      | Some s -> " \xe2\x80\x94 " ^ s
      | None -> ""
    in
    Buffer.add_string buf
      (Printf.sprintf "\xe2\x9c\x85 Session history compacted%s\n" dur)
  end
  else
    Buffer.add_string buf
      "\xf0\x9f\x97\x9c\xef\xb8\x8f Compacting session history\xe2\x80\xa6\n";
  List.iter
    (fun (name, emoji, started_at_opt, done_at_opt) ->
      match (started_at_opt, done_at_opt) with
      | None, _ ->
          (* Pending -- not yet started *)
          Buffer.add_string buf
            (Printf.sprintf "\xe2\x97\x8c %s %s\n" emoji name)
      | Some _, None ->
          (* Running *)
          Buffer.add_string buf
            (Printf.sprintf "\xe2\x8f\xb3 %s %s\n" emoji name)
      | Some started_at, Some done_at ->
          (* Done *)
          let dur_part =
            match
              Status_message.format_duration_opt (done_at -. started_at)
            with
            | Some s -> " \xe2\x80\x94 " ^ s
            | None -> ""
          in
          Buffer.add_string buf
            (Printf.sprintf "\xe2\x9c\x93 %s %s%s\n" emoji name dur_part))
    !steps;
  let s = Buffer.contents buf in
  let n = String.length s in
  if n > 0 && s.[n - 1] = '\n' then String.sub s 0 (n - 1) else s

let compact mgr ~key ?notifier () =
  let open Lwt.Syntax in
  Logs.info (fun m ->
      m "/compact requested for session %s — starting compaction" key);
  (* Mutable progress state, shared between callbacks and finalization. *)
  let steps : (string * string * float option * float option) list ref =
    ref []
  in
  let msg_id : string option ref = ref None in
  let overall_start = ref (Unix.gettimeofday ()) in
  let send_or_edit_opt =
    match notifier with
    | None -> None
    | Some (n : Status_message.notifier) ->
        let f text =
          Lwt.catch
            (fun () ->
              match !msg_id with
              | None ->
                  let* id = n.send text in
                  msg_id := Some id;
                  Lwt.return_unit
              | Some id ->
                  let* new_id_opt = n.edit id text in
                  (match new_id_opt with
                  | Some new_id -> msg_id := Some new_id
                  | None -> ());
                  Lwt.return_unit)
            (fun exn ->
              Logs.warn (fun m ->
                  m "Compact progress update failed: %s"
                    (Printexc.to_string exn));
              Lwt.return_unit)
        in
        Some f
  in
  let render ~finished =
    compact_progress_render ~steps ~overall_start:!overall_start ~finished
  in
  let compact_cbs =
    match send_or_edit_opt with
    | None -> None
    | Some soe ->
        let on_step_start name _emoji =
          (* Transition the matching Pending step to Running *)
          steps :=
            List.map
              (fun (n', e, s, d) ->
                if n' = name && s = None then
                  (n', e, Some (Unix.gettimeofday ()), d)
                else (n', e, s, d))
              !steps;
          soe (render ~finished:false)
        in
        let on_step_done name dur =
          steps :=
            List.map
              (fun (n', e, s, d) ->
                match s with
                | Some t0 when n' = name && d = None ->
                    (n', e, s, Some (t0 +. dur))
                | _ -> (n', e, s, d))
              !steps;
          soe (render ~finished:false)
        in
        Some Agent.{ on_step_start; on_step_done }
  in
  (* Three-phase compaction: plan (lock), execute (no lock), apply (lock).
     This avoids holding the session mutex during LLM calls which can take
     70+ seconds and cause the fatal timeout in lock_with_timeout. *)
  let* result =
    Lwt.catch
      (fun () ->
        (* Phase 1: Plan -- acquire lock, snapshot state, release lock. *)
        let* plan_opt =
          with_session_lock mgr ~key (fun agent _interrupt ->
              let plan = Agent.plan_force_compact agent in
              (* Set up progress steps while we have the agent *)
              let* () =
                match (plan, send_or_edit_opt) with
                | Some _, Some soe ->
                    let has_mem_flush =
                      agent.Agent.config.memory.pre_compaction_flush
                      && Option.is_some mgr.db
                    in
                    overall_start := Unix.gettimeofday ();
                    steps :=
                      (if has_mem_flush then
                         [ ("Save memories", "\xf0\x9f\xa7\xa0", None, None) ]
                       else [])
                      @ [
                          ( "Summarize (part 1)",
                            "\xe2\x9c\x82\xef\xb8\x8f",
                            None,
                            None );
                          ( "Summarize (part 2)",
                            "\xe2\x9c\x82\xef\xb8\x8f",
                            None,
                            None );
                        ];
                    soe (render ~finished:false)
                | _ -> Lwt.return_unit
              in
              Lwt.return plan)
        in
        match plan_opt with
        | None -> Lwt.return (Ok false)
        | Some plan ->
            (* Phase 2: Execute -- LLM calls with NO lock held. *)
            let* summary =
              Agent.execute_compact_plan plan ?db:mgr.db ?compact_cbs ()
            in
            (* Phase 3: Apply -- re-acquire lock, reconcile, persist. *)
            with_session_lock mgr ~key (fun agent _interrupt ->
                match Agent.apply_compact_result agent plan ~summary with
                | Some _ ->
                    persist_compacted_history mgr ~key agent;
                    Lwt.return (Ok true)
                | None ->
                    Logs.warn (fun m ->
                        m
                          "Compact apply skipped for session %s — history \
                           changed during execution"
                          key);
                    Lwt.return (Ok false)))
      (fun exn -> Lwt.return (Error (Printexc.to_string exn)))
  in
  let* () =
    match (result, send_or_edit_opt) with
    | Ok true, Some soe -> soe (render ~finished:true)
    | Ok false, Some soe ->
        soe
          "Nothing to compact \xe2\x80\x94 session history is already short \
           enough."
    | Error _, Some soe when !msg_id <> None ->
        Lwt.catch
          (fun () -> soe "\xe2\x9d\x8c Compaction failed")
          (fun _ -> Lwt.return_unit)
    | _ -> Lwt.return_unit
  in
  Lwt.return result

(* Produce a full JSON dump of the current epoch for a session.
   Used by /debug_dump_chat to send session state as a file attachment. *)
let dump_json mgr ~key =
  match mgr.db with
  | None ->
      Yojson.Safe.pretty_to_string
        (`Assoc
           [ ("session_key", `String key); ("error", `String "no database") ])
  | Some db -> (
      let config = mgr.config in
      let msg_json (row : Memory.raw_message) =
        let content_field =
          match Yojson.Safe.from_string row.content with
          | json -> json
          | exception _ -> `String row.content
        in
        `Assoc
          [
            ("role", `String row.role);
            ("content", content_field);
            ("created_at", `String row.created_at);
          ]
      in
      match
        Memory.load_epoch_messages ~db ~session_key:key ~epoch:Memory.Current
      with
      | None ->
          Yojson.Safe.pretty_to_string
            (`Assoc
               [
                 ("session_key", `String key);
                 ("epoch", `String "current");
                 ("error", `String "no messages found");
               ])
      | Some rows ->
          let epochs = Memory.list_session_epochs ~db ~session_key:key in
          let archived =
            List.filter (fun (e : Memory.session_epoch) -> not e.current) epochs
          in
          let archived_epoch_count = List.length archived in
          let total_archived_messages =
            List.fold_left
              (fun acc (e : Memory.session_epoch) -> acc + e.message_count)
              0 archived
          in
          let system_prompt =
            Prompt_builder.build ~config ~tool_registry:None ()
          in
          Yojson.Safe.pretty_to_string
            (`Assoc
               [
                 ("session_key", `String key);
                 ("epoch", `String "current");
                 ("system_prompt", `String system_prompt);
                 ("archived_epoch_count", `Int archived_epoch_count);
                 ("total_archived_messages", `Int total_archived_messages);
                 ("total_messages", `Int (List.length rows));
                 ("messages", `List (List.map msg_json rows));
               ]))
