type t = {
  mutable config : Runtime_config.t;
  sessions : (string, Agent.t * Lwt_mutex.t * string option ref) Hashtbl.t;
  tool_registry : Tool_registry.t option;
  db : Sqlite3.db option;
}

let create ~config ?tool_registry ?db () =
  { config; sessions = Hashtbl.create 16; tool_registry; db }

let get_or_create mgr ~key =
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

let turn mgr ~key ~message ?(attachments = []) ?channel_name ?channel_type
    ?sender_id ?sender_name () =
  let open Lwt.Syntax in
  (* Messages starting with '!' are direct interrupt commands: interrupt any
     ongoing agent turn and return the remainder of the message verbatim. *)
  if String.length message > 0 && message.[0] = '!' then begin
    let raw = String.sub message 1 (String.length message - 1) in
    let direct_msg = if String.trim raw = "" then "[interrupted]" else raw in
    (match Hashtbl.find_opt mgr.sessions key with
    | Some (_, _, interrupt) -> interrupt := Some direct_msg
    | None -> ());
    Lwt.return direct_msg
  end
  else begin
    let agent, mutex, interrupt = get_or_create mgr ~key in
    let interrupt_check () = !interrupt in
    Lwt_mutex.with_lock mutex (fun () ->
        interrupt := None;
        (match mgr.db with
        | Some db when mgr.config.security.audit_enabled ->
            Audit.log ~db
              (ChatMessage
                 { session_key = key; role = "user"; content_preview = message })
        | _ -> ());
        inject_attachment_context agent attachments;
        let effective_message =
          match (channel_name, channel_type, sender_id, sender_name) with
          | None, None, None, None -> message
          | _ ->
              let ctx =
                format_context_block ?channel_name ?channel_type ?sender_id
                  ?sender_name ()
              in
              ctx ^ "\n" ^ message
        in
        let history_before = List.length agent.history in
        let* response =
          Agent.turn agent ~user_message:effective_message ?db:mgr.db
            ~session_key:key ~interrupt_check ()
        in
        (match mgr.db with
        | Some db ->
            let new_messages = List.length agent.history - history_before in
            if new_messages > 0 then begin
              let reversed = List.rev agent.history in
              let to_persist =
                let skip = history_before in
                List.filteri (fun i _ -> i >= skip) reversed
              in
              List.iter
                (fun msg -> Memory.store_message ~db ~session_key:key msg)
                to_persist
            end;
            if mgr.config.security.audit_enabled then
              Audit.log ~db
                (ChatMessage
                   {
                     session_key = key;
                     role = "assistant";
                     content_preview = response;
                   })
        | None -> ());
        Lwt.return response)
  end

let get_config mgr = mgr.config

let update_config mgr config =
  mgr.config <- config;
  Hashtbl.iter
    (fun _ (agent, _, _) -> agent.Agent.config <- config)
    mgr.sessions

let turn_stream mgr ~key ~message ?(attachments = []) ?channel_name
    ?channel_type ?sender_id ?sender_name ~on_chunk () =
  let open Lwt.Syntax in
  (* '!' prefix: interrupt any ongoing turn and return the message directly. *)
  if String.length message > 0 && message.[0] = '!' then begin
    let raw = String.sub message 1 (String.length message - 1) in
    let direct_msg = if String.trim raw = "" then "[interrupted]" else raw in
    (match Hashtbl.find_opt mgr.sessions key with
    | Some (_, _, interrupt) -> interrupt := Some direct_msg
    | None -> ());
    let* () = on_chunk (Provider.Delta direct_msg) in
    let* () = on_chunk Provider.Done in
    Lwt.return direct_msg
  end
  else begin
    let agent, mutex, interrupt = get_or_create mgr ~key in
    let interrupt_check () = !interrupt in
    Lwt_mutex.with_lock mutex (fun () ->
        interrupt := None;
        (match mgr.db with
        | Some db when mgr.config.security.audit_enabled ->
            Audit.log ~db
              (ChatMessage
                 { session_key = key; role = "user"; content_preview = message })
        | _ -> ());
        inject_attachment_context agent attachments;
        let effective_message =
          match (channel_name, channel_type, sender_id, sender_name) with
          | None, None, None, None -> message
          | _ ->
              let ctx =
                format_context_block ?channel_name ?channel_type ?sender_id
                  ?sender_name ()
              in
              ctx ^ "\n" ^ message
        in
        let history_before = List.length agent.history in
        let* response =
          Agent.turn_stream agent ~user_message:effective_message ?db:mgr.db
            ~session_key:key ~interrupt_check ~on_chunk ()
        in
        (match mgr.db with
        | Some db ->
            let new_messages = List.length agent.history - history_before in
            if new_messages > 0 then begin
              let reversed = List.rev agent.history in
              let to_persist =
                let skip = history_before in
                List.filteri (fun i _ -> i >= skip) reversed
              in
              List.iter
                (fun msg -> Memory.store_message ~db ~session_key:key msg)
                to_persist
            end;
            if mgr.config.security.audit_enabled then
              Audit.log ~db
                (ChatMessage
                   {
                     session_key = key;
                     role = "assistant";
                     content_preview = response;
                   })
        | None -> ());
        Lwt.return response)
  end

let reset mgr ~key =
  (match mgr.db with
  | Some db -> Memory.clear_session ~db ~session_key:key
  | None -> ());
  Hashtbl.remove mgr.sessions key
