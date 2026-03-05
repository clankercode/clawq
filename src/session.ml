type t = {
  mutable config : Runtime_config.t;
  sessions : (string, Agent.t * Lwt_mutex.t) Hashtbl.t;
  tool_registry : Tool_registry.t option;
  db : Sqlite3.db option;
}

let create ~config ?tool_registry ?db () =
  { config; sessions = Hashtbl.create 16; tool_registry; db }

let get_or_create mgr ~key =
  match Hashtbl.find_opt mgr.sessions key with
  | Some pair -> pair
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
      let pair = (agent, mutex) in
      Hashtbl.replace mgr.sessions key pair;
      pair

let turn mgr ~key ~message =
  let open Lwt.Syntax in
  let agent, mutex = get_or_create mgr ~key in
  Lwt_mutex.with_lock mutex (fun () ->
      (match mgr.db with
      | Some db when mgr.config.security.audit_enabled ->
          Audit.log ~db
            (ChatMessage
               { session_key = key; role = "user"; content_preview = message })
      | _ -> ());
      let history_before = List.length agent.history in
      let* response =
        Agent.turn agent ~user_message:message ?db:mgr.db ~session_key:key ()
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

let get_config mgr = mgr.config

let update_config mgr config =
  mgr.config <- config;
  Hashtbl.iter (fun _ (agent, _) -> agent.Agent.config <- config) mgr.sessions

let turn_stream mgr ~key ~message ~on_chunk =
  let open Lwt.Syntax in
  let agent, mutex = get_or_create mgr ~key in
  Lwt_mutex.with_lock mutex (fun () ->
      (match mgr.db with
      | Some db when mgr.config.security.audit_enabled ->
          Audit.log ~db
            (ChatMessage
               { session_key = key; role = "user"; content_preview = message })
      | _ -> ());
      let history_before = List.length agent.history in
      let* response =
        Agent.turn_stream agent ~user_message:message ?db:mgr.db
          ~session_key:key ~on_chunk ()
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

let reset mgr ~key =
  (match mgr.db with
  | Some db -> Memory.clear_session ~db ~session_key:key
  | None -> ());
  Hashtbl.remove mgr.sessions key
