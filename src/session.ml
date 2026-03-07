type special_command_handler =
  key:string ->
  message:string ->
  send_progress:(string -> unit Lwt.t) option ->
  string option Lwt.t

type t = {
  mutable config : Runtime_config.t;
  sessions : (string, Agent.t * Lwt_mutex.t * string option ref) Hashtbl.t;
  sessions_lock : Lwt_mutex.t;
  tool_registry : Tool_registry.t option;
  db : Sqlite3.db option;
  mutable draining : bool;
  in_flight_count : int ref;
  channel_notifiers : (string, string -> unit Lwt.t) Hashtbl.t;
  mutable special_command_handler : special_command_handler option;
}

let draining_message =
  "Daemon is restarting, please wait a moment and try again."

let create ~config ?tool_registry ?db () =
  {
    config;
    sessions = Hashtbl.create 16;
    sessions_lock = Lwt_mutex.create ();
    tool_registry;
    db;
    draining = false;
    in_flight_count = ref 0;
    channel_notifiers = Hashtbl.create 16;
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

let turn mgr ~key ~message ?(attachments = []) ?channel_name ?channel_type
    ?sender_id ?sender_name ?channel ?channel_id () =
  let open Lwt.Syntax in
  (* Messages starting with '!' are direct interrupt commands: interrupt any
     ongoing agent turn and return the remainder of the message verbatim. *)
  if String.length message > 0 && message.[0] = '!' then begin
    let raw = String.sub message 1 (String.length message - 1) in
    let direct_msg = if String.trim raw = "" then "[interrupted]" else raw in
    let* () = set_interrupt_if_present mgr ~key direct_msg in
    Lwt.return direct_msg
  end
  else begin
    let* handled =
      handle_special_command mgr ~key ~message
        ?send_progress:(find_registered_notifier mgr ~key)
        ()
    in
    match handled with
    | Some response -> Lwt.return response
    | None ->
        with_session_lock_unless_draining mgr ~key
          ~on_draining:(fun () ->
            let* draining_response = respond_if_draining mgr in
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
                let* () =
                  Agent.prepare_turn_history agent
                    ~user_message:effective_message ?db:mgr.db ()
                in
                persist_new_messages mgr ~key ~history_before agent;
                let prepared_history_len = List.length agent.history in
                record_agent_turn mgr ~key ?channel ?channel_id ();
                let* response =
                  let* draining_response = respond_if_draining mgr in
                  match draining_response with
                  | Some response -> Lwt.return response
                  | None ->
                      Agent.turn agent ~user_message:effective_message
                        ?db:mgr.db ~session_key:key ~interrupt_check
                        ~history_prepared:true ()
                in
                persist_new_messages mgr ~key
                  ~history_before:prepared_history_len agent;
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
                Lwt.return response))
  end

let get_config mgr = mgr.config

let update_config mgr config =
  mgr.config <- config;
  Hashtbl.iter
    (fun _ (agent, _, _) -> agent.Agent.config <- config)
    mgr.sessions

let turn_stream mgr ~key ~message ?(attachments = []) ?channel_name
    ?channel_type ?sender_id ?sender_name ?channel ?channel_id ~on_chunk () =
  let open Lwt.Syntax in
  (* '!' prefix: interrupt any ongoing turn and return the message directly. *)
  if String.length message > 0 && message.[0] = '!' then begin
    let raw = String.sub message 1 (String.length message - 1) in
    let direct_msg = if String.trim raw = "" then "[interrupted]" else raw in
    let* () = set_interrupt_if_present mgr ~key direct_msg in
    let* () = on_chunk (Provider.Delta direct_msg) in
    let* () = on_chunk Provider.Done in
    Lwt.return direct_msg
  end
  else begin
    let send_progress text = on_chunk (Provider.Delta (text ^ "\n")) in
    let* handled = handle_special_command mgr ~key ~message ~send_progress () in
    match handled with
    | Some response ->
        let* () = on_chunk (Provider.Delta response) in
        let* () = on_chunk Provider.Done in
        Lwt.return response
    | None ->
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
                let* () =
                  Agent.prepare_turn_history agent
                    ~user_message:effective_message ?db:mgr.db ()
                in
                persist_new_messages mgr ~key ~history_before agent;
                let prepared_history_len = List.length agent.history in
                record_agent_turn mgr ~key ?channel ?channel_id ();
                let* response =
                  let* draining_response = respond_if_draining ~on_chunk mgr in
                  match draining_response with
                  | Some response -> Lwt.return response
                  | None ->
                      Agent.turn_stream agent ~user_message:effective_message
                        ?db:mgr.db ~session_key:key ~interrupt_check
                        ~history_prepared:true ~on_chunk ()
                in
                persist_new_messages mgr ~key
                  ~history_before:prepared_history_len agent;
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
                Lwt.return response))
  end

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
            unregister_channel_notifier mgr ~key;
            Hashtbl.remove mgr.sessions key;
            Lwt.return (Some mutex)
        | None ->
            (match mgr.db with
            | Some db -> Memory.clear_session ~db ~session_key:key
            | None -> ());
            unregister_channel_notifier mgr ~key;
            Lwt.return None)
  in
  match held_mutex with
  | Some mutex ->
      Lwt_mutex.unlock mutex;
      Lwt.return_unit
  | None -> Lwt.return_unit
