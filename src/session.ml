type special_command_handler =
  key:string ->
  message:string ->
  send_progress:(string -> unit Lwt.t) option ->
  string option Lwt.t

type queued_message = {
  message : string;
  attachments : (string * string) list;
  channel_name : string option;
  channel_type : string option;
  sender_id : string option;
  sender_name : string option;
  channel : string option;
  channel_id : string option;
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
  deferred_responses : (string, unit) Hashtbl.t;
  queued_messages : (string, queued_message list) Hashtbl.t;
  mutable special_command_handler : special_command_handler option;
}

let queued_message_response = "__clawq_message_queued__"

let draining_message =
  "Daemon is restarting, please wait a moment and try again."

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
    deferred_responses = Hashtbl.create 16;
    queued_messages = Hashtbl.create 16;
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
  Hashtbl.remove mgr.channel_notifiers key

let set_response_deferred mgr ~key =
  Hashtbl.replace mgr.deferred_responses key ()

let response_deferred mgr ~key = Hashtbl.mem mgr.deferred_responses key

let take_response_deferred mgr ~key =
  let deferred = response_deferred mgr ~key in
  if deferred then Hashtbl.remove mgr.deferred_responses key;
  deferred

let clear_response_deferred mgr ~key =
  Hashtbl.remove mgr.deferred_responses key

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
          if !interrupt = None then interrupt := Some "[queued inbound message]";
          Lwt.return_true
      | _ -> Lwt.return_false)

let take_next_queued_message mgr ~key =
  match Hashtbl.find_opt mgr.queued_messages key with
  | Some (msg :: rest) ->
      if rest = [] then Hashtbl.remove mgr.queued_messages key
      else Hashtbl.replace mgr.queued_messages key rest;
      Some msg
  | _ -> None

let queued_message_prompt message =
  "A new message arrived in this same channel while you were working on the "
  ^ "previous task. Respond to it now. If it still makes sense afterward, "
  ^ "continue the previous task.\n\nNew message:\n" ^ message

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

let with_registered_notifier mgr ~key ~notify f =
  register_channel_notifier mgr ~key notify;
  Lwt.finalize f (fun () ->
      unregister_channel_notifier mgr ~key;
      Lwt.return_unit)

let find_registered_notifier mgr ~key =
  Hashtbl.find_opt mgr.channel_notifiers key

let handle_special_command mgr ~key ~message ?send_progress () =
  match mgr.special_command_handler with
  | None -> Lwt.return_none
  | Some handler -> handler ~key ~message ~send_progress

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
            agent.history <- List.rev history;
            Logs.info (fun m ->
                m "Restored %d messages for session %s" (List.length history)
                  key)
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

let stream_turn_with_visibility mgr ~notify agent ~key ~effective_message
    ~prepared_history_len ~interrupt_check ~runtime_context =
  let open Lwt.Syntax in
  let visibility = Stream_visibility.create () in
  let settings : Stream_visibility.settings =
    {
      show_thinking = mgr.config.agent_defaults.show_thinking;
      show_tool_calls = mgr.config.agent_defaults.show_tool_calls;
      notify_tool_starts = false;
      notify_tool_successes = true;
    }
  in
  let* response =
    Agent.turn_stream agent ~user_message:effective_message ?db:mgr.db
      ~session_key:key ~interrupt_check ?runtime_context ~history_prepared:true
      ~on_chunk:(Stream_visibility.on_chunk visibility ~settings ~notify)
      ()
  in
  let thinking = Stream_visibility.thinking_text visibility in
  let* () =
    if settings.show_thinking && thinking <> "" then
      notify (Stream_visibility.thinking_message thinking)
    else Lwt.return_unit
  in
  persist_new_messages mgr ~key ~history_before:prepared_history_len agent;
  (match mgr.db with
  | Some db when mgr.config.security.audit_enabled ->
      Audit.log ~db
        (ChatMessage
           { session_key = key; role = "assistant"; content_preview = response })
  | _ -> ());
  Lwt.return response

let normalize_incoming_message mgr ~key ~message =
  let open Lwt.Syntax in
  if String.length message > 0 && message.[0] = '!' then begin
    let raw = String.sub message 1 (String.length message - 1) in
    let normalized = if String.trim raw = "" then "[interrupted]" else raw in
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

let run_locked_turn mgr ~key agent interrupt ~message ?(attachments = [])
    ?channel_name ?channel_type ?sender_id ?sender_name ?channel ?channel_id ()
    =
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
  let* compacted =
    Agent.prepare_turn_history agent ~user_message:effective_message ?db:mgr.db
      ()
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
  let notify = find_registered_notifier mgr ~key in
  record_agent_turn mgr ~key ?channel ?channel_id ();
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
                  ~effective_message ~prepared_history_len ~interrupt_check
                  ~runtime_context
            | _ ->
                Agent.turn agent ~user_message:effective_message ?db:mgr.db
                  ~session_key:key ~interrupt_check ?runtime_context
                  ~history_prepared:true ()))
      (function
        | Agent.Restart_requested ->
            persist_new_messages mgr ~key ~history_before:prepared_history_len
              agent;
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
        persist_new_messages mgr ~key ~history_before:prepared_history_len agent;
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

let rec drain_queued_messages mgr ~key agent interrupt =
  match
    (take_next_queued_message mgr ~key, find_registered_notifier mgr ~key)
  with
  | Some queued, Some notify ->
      let open Lwt.Syntax in
      let injected_message =
        queued_message_prompt
          (effective_message_for_turn ~message:queued.message
             ?channel_name:queued.channel_name ?channel_type:queued.channel_type
             ?sender_id:queued.sender_id ?sender_name:queued.sender_name ())
      in
      let* response =
        run_locked_turn mgr ~key agent interrupt ~message:injected_message
          ?channel_name:queued.channel_name ?channel_type:queued.channel_type
          ?sender_id:queued.sender_id ?sender_name:queued.sender_name
          ?channel:queued.channel ?channel_id:queued.channel_id ()
      in
      let* () = notify response in
      if not (take_response_deferred mgr ~key) then mark_response_sent mgr ~key;
      drain_queued_messages mgr ~key agent interrupt
  | Some _, None -> Lwt.return_unit
  | None, _ -> Lwt.return_unit

let turn mgr ~key ~message ?(attachments = []) ?channel_name ?channel_type
    ?sender_id ?sender_name ?channel ?channel_id () =
  let open Lwt.Syntax in
  let* message = normalize_incoming_message mgr ~key ~message in
  let* handled =
    handle_special_command mgr ~key ~message
      ?send_progress:(find_registered_notifier mgr ~key)
      ()
  in
  match handled with
  | Some response -> Lwt.return response
  | None ->
      let queued_message =
        {
          message;
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
                  run_locked_turn mgr ~key agent interrupt ~message ~attachments
                    ?channel_name ?channel_type ?sender_id ?sender_name ?channel
                    ?channel_id ()
                in
                let* () = drain_queued_messages mgr ~key agent interrupt in
                Lwt.return response))

let get_config mgr = mgr.config

let update_config mgr config =
  mgr.config <- config;
  Hashtbl.iter
    (fun _ (agent, _, _) -> agent.Agent.config <- config)
    mgr.sessions

let turn_stream mgr ~key ~message ?(attachments = []) ?channel_name
    ?channel_type ?sender_id ?sender_name ?channel ?channel_id ~on_chunk () =
  let open Lwt.Syntax in
  let* message = normalize_incoming_message mgr ~key ~message in
  let send_progress text = on_chunk (Provider.Delta (text ^ "\n")) in
  let* handled = handle_special_command mgr ~key ~message ~send_progress () in
  match handled with
  | Some response ->
      let* () = on_chunk (Provider.Delta response) in
      let* () = on_chunk Provider.Done in
      Lwt.return response
  | None ->
      let queued_message =
        {
          message;
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
                    ~user_message:effective_message ?db:mgr.db ()
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
                            ~session_key:key ~interrupt_check ?runtime_context
                            ~history_prepared:true ~on_chunk ())
                    (function
                      | Agent.Restart_requested ->
                          persist_new_messages mgr ~key
                            ~history_before:prepared_history_len agent;
                          set_response_deferred mgr ~key;
                          let* () =
                            on_chunk (Provider.Delta draining_message)
                          in
                          let* () = on_chunk Provider.Done in
                          Lwt.return draining_message
                      | exn -> Lwt.fail exn)
                in
                if not (response_deferred mgr ~key) then begin
                  persist_new_messages mgr ~key
                    ~history_before:prepared_history_len agent;
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
                let* () = drain_queued_messages mgr ~key agent interrupt in
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
            unregister_channel_notifier mgr ~key;
            Hashtbl.remove mgr.sessions key;
            Lwt.return (Some mutex)
        | None ->
            (match mgr.db with
            | Some db -> Memory.clear_session ~db ~session_key:key
            | None -> ());
            Hashtbl.remove mgr.deferred_responses key;
            Hashtbl.remove mgr.queued_messages key;
            unregister_channel_notifier mgr ~key;
            Lwt.return None)
  in
  match held_mutex with
  | Some mutex ->
      Lwt_mutex.unlock mutex;
      Lwt.return_unit
  | None -> Lwt.return_unit
