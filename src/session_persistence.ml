open Session_types

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
