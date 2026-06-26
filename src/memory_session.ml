open Memory_types
open Memory_core

let store_session_workspace_state ~db ~session_key
    ~observed_active_workspace_files =
  let observed_files_json =
    `List
      (List.map
         (fun (file, digest) ->
           `Assoc
             [
               ("file", `String file);
               ( "digest",
                 match digest with Some value -> `String value | None -> `Null
               );
             ])
         observed_active_workspace_files)
    |> Yojson.Safe.to_string
  in
  let sql =
    "INSERT INTO session_workspace_state (session_key, observed_files_json, \
     updated_at) VALUES (?, ?, datetime('now')) ON CONFLICT(session_key) DO \
     UPDATE SET observed_files_json = excluded.observed_files_json, updated_at \
     = datetime('now')"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT observed_files_json));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> ()
      | rc ->
          Logs.warn (fun m ->
              m "Failed to store session workspace state: %s"
                (Sqlite3.Rc.to_string rc)))

let load_session_workspace_state ~db ~session_key =
  let sql =
    "SELECT observed_files_json FROM session_workspace_state WHERE session_key \
     = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.TEXT json -> (
              try
                let open Yojson.Safe.Util in
                Some
                  (Yojson.Safe.from_string json
                  |> to_list
                  |> List.filter_map (fun entry ->
                      try
                        let file = entry |> member "file" |> to_string in
                        let digest =
                          match entry |> member "digest" with
                          | `Null -> None
                          | value -> Some (to_string value)
                        in
                        Some (file, digest)
                      with _ -> None))
              with _ -> None)
          | _ -> None)
      | _ -> None)

let upsert_session_state ~db ~session_key ~turn ?channel ?channel_id
    ?response_sent_at () =
  let sql =
    "INSERT INTO session_state (session_key, turn, channel, channel_id, \
     response_sent_at, last_active) VALUES (?, ?, ?, ?, ?, datetime('now')) ON \
     CONFLICT(session_key) DO UPDATE SET turn = excluded.turn, channel = \
     COALESCE(excluded.channel, session_state.channel), channel_id = \
     COALESCE(excluded.channel_id, session_state.channel_id), response_sent_at \
     = excluded.response_sent_at, last_active = datetime('now')"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT turn));
  ignore
    (Sqlite3.bind stmt 3
       (match channel with
       | Some value -> Sqlite3.Data.TEXT value
       | None -> Sqlite3.Data.NULL));
  ignore
    (Sqlite3.bind stmt 4
       (match channel_id with
       | Some value -> Sqlite3.Data.TEXT value
       | None -> Sqlite3.Data.NULL));
  ignore
    (Sqlite3.bind stmt 5
       (match response_sent_at with
       | Some value -> Sqlite3.Data.TEXT value
       | None -> Sqlite3.Data.NULL));
  (match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE -> ()
  | rc ->
      Logs.warn (fun m ->
          m "Failed to upsert session state: %s" (Sqlite3.Rc.to_string rc)));
  ignore (Sqlite3.finalize stmt)

let mark_response_sent ~db ~session_key =
  let sql =
    "UPDATE session_state SET turn = 'user', response_sent_at = \
     datetime('now'), last_active = datetime('now') WHERE session_key = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
  (match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE -> ()
  | rc ->
      Logs.warn (fun m ->
          m "Failed to mark response sent: %s" (Sqlite3.Rc.to_string rc)));
  ignore (Sqlite3.finalize stmt)

let set_session_keepalive ~db ~session_key ~enabled =
  let sql =
    "INSERT INTO session_state (session_key, keepalive_enabled) VALUES (?, ?) \
     ON CONFLICT(session_key) DO UPDATE SET keepalive_enabled = \
     excluded.keepalive_enabled"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (if enabled then 1L else 0L)));
  (match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE -> ()
  | rc ->
      Logs.warn (fun m ->
          m "Failed to set session keepalive: %s" (Sqlite3.Rc.to_string rc)));
  ignore (Sqlite3.finalize stmt)

let set_session_heartbeat ~db ~session_key ~enabled =
  let sql =
    "INSERT INTO session_state (session_key, heartbeat_enabled) VALUES (?, ?) \
     ON CONFLICT(session_key) DO UPDATE SET heartbeat_enabled = \
     excluded.heartbeat_enabled"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (if enabled then 1L else 0L)));
  (match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE -> ()
  | rc ->
      Logs.warn (fun m ->
          m "Failed to set session heartbeat: %s" (Sqlite3.Rc.to_string rc)));
  ignore (Sqlite3.finalize stmt)

let session_heartbeat_enabled ~db ~session_key =
  let sql =
    "SELECT COALESCE(heartbeat_enabled, 0) FROM session_state WHERE \
     session_key = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
  let enabled =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match Sqlite3.column stmt 0 with
        | Sqlite3.Data.INT n -> n <> 0L
        | _ -> false)
    | _ -> false
  in
  ignore (Sqlite3.finalize stmt);
  enabled

let set_session_model_override ~db ~session_key ~model =
  let sql =
    "INSERT INTO session_state (session_key, model_override) VALUES (?, ?) ON \
     CONFLICT(session_key) DO UPDATE SET model_override = \
     excluded.model_override"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT model));
  (match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE -> ()
  | rc ->
      Logs.warn (fun m ->
          m "Failed to set session model override: %s" (Sqlite3.Rc.to_string rc)));
  ignore (Sqlite3.finalize stmt)

let get_session_model_override ~db ~session_key =
  let sql = "SELECT model_override FROM session_state WHERE session_key = ?" in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match Sqlite3.column stmt 0 with
        | Sqlite3.Data.TEXT s -> Some s
        | _ -> None)
    | _ -> None
  in
  ignore (Sqlite3.finalize stmt);
  result

let clear_session_model_override ~db ~session_key =
  let sql =
    "UPDATE session_state SET model_override = NULL WHERE session_key = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
  ignore (Sqlite3.step stmt);
  ignore (Sqlite3.finalize stmt)

let set_session_cwd ~db ~session_key ~cwd =
  let sql =
    "INSERT INTO session_state (session_key, effective_cwd) VALUES (?, ?) ON \
     CONFLICT(session_key) DO UPDATE SET effective_cwd = \
     excluded.effective_cwd"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
  ignore
    (Sqlite3.bind stmt 2
       (match cwd with
       | Some c -> Sqlite3.Data.TEXT c
       | None -> Sqlite3.Data.NULL));
  (match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE -> ()
  | rc ->
      Logs.warn (fun m ->
          m "Failed to set session cwd: %s" (Sqlite3.Rc.to_string rc)));
  ignore (Sqlite3.finalize stmt)

let get_session_cwd ~db ~session_key =
  let sql = "SELECT effective_cwd FROM session_state WHERE session_key = ?" in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match Sqlite3.column stmt 0 with
        | Sqlite3.Data.TEXT s -> Some s
        | _ -> None)
    | _ -> None
  in
  ignore (Sqlite3.finalize stmt);
  result

let list_keepalive_session_keys ~db =
  let sql =
    "SELECT session_key FROM session_state WHERE keepalive_enabled = 1"
  in
  let stmt = Sqlite3.prepare db sql in
  let keys = ref [] in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    match Sqlite3.column stmt 0 with
    | Sqlite3.Data.TEXT s -> keys := s :: !keys
    | _ -> ()
  done;
  ignore (Sqlite3.finalize stmt);
  List.rev !keys

let list_heartbeat_session_keys ~db =
  let sql =
    "SELECT session_key FROM session_state WHERE heartbeat_enabled = 1 ORDER \
     BY session_key"
  in
  let stmt = Sqlite3.prepare db sql in
  let keys = ref [] in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    match Sqlite3.column stmt 0 with
    | Sqlite3.Data.TEXT s -> keys := s :: !keys
    | _ -> ()
  done;
  ignore (Sqlite3.finalize stmt);
  List.rev !keys

let load_pending_agent_sessions ~db ~max_age_seconds =
  let sql =
    "SELECT session_key, channel, channel_id FROM session_state WHERE turn = \
     'agent' AND response_sent_at IS NULL AND last_active >= datetime('now', \
     '-' || ? || ' seconds') ORDER BY last_active DESC"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int max_age_seconds)));
  let rows = ref [] in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    let get_opt index =
      match Sqlite3.column stmt index with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    in
    match get_opt 0 with
    | Some session_key -> rows := (session_key, get_opt 1, get_opt 2) :: !rows
    | None -> ()
  done;
  ignore (Sqlite3.finalize stmt);
  List.rev !rows

let get_session_channel ~db ~session_key =
  let sql =
    "SELECT channel, channel_id FROM session_state WHERE session_key = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
  let result =
    if Sqlite3.step stmt = Sqlite3.Rc.ROW then
      match (Sqlite3.column stmt 0, Sqlite3.column stmt 1) with
      (* B656: treat empty strings the same as NULL so callers don't fall
         through to "unsupported channel" delivery paths for sessions that
         have no channel binding. *)
      | Sqlite3.Data.TEXT channel, Sqlite3.Data.TEXT channel_id
        when channel <> "" && channel_id <> "" ->
          Some (channel, channel_id)
      | _ -> None
    else None
  in
  ignore (Sqlite3.finalize stmt);
  result

let parse_channel_from_session_key key =
  match String.split_on_char ':' key with
  | channel :: _ when channel <> "" -> Some channel
  | _ -> None

let list_session_infos ~db ?channel ?prefix ?(activity = Any) ?only_main
    ?(include_postmortem = false) () =
  let sql =
    "SELECT k.session_key, s.channel, s.channel_id, s.turn, \
     s.response_sent_at, s.last_active, (SELECT COUNT(*) FROM messages m WHERE \
     m.session_key = k.session_key), (SELECT COUNT(*) FROM session_log_epochs \
     e WHERE e.session_key = k.session_key), COALESCE(s.keepalive_enabled, 0), \
     COALESCE(s.heartbeat_enabled, 0), s.effective_cwd FROM (SELECT \
     session_key FROM messages UNION SELECT session_key FROM session_state \
     UNION SELECT session_key FROM session_log_epochs) k LEFT JOIN \
     session_state s ON s.session_key = k.session_key ORDER BY k.session_key"
  in
  let stmt = Sqlite3.prepare db sql in
  let rows = ref [] in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    let text_opt index =
      match Sqlite3.column stmt index with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    in
    let int_value index =
      match Sqlite3.column stmt index with
      | Sqlite3.Data.INT n -> Int64.to_int n
      | _ -> 0
    in
    let session_key = match text_opt 0 with Some s -> s | None -> "" in
    if session_key <> "" then
      rows :=
        {
          session_key;
          channel = text_opt 1;
          channel_id = text_opt 2;
          turn = text_opt 3;
          response_sent_at = text_opt 4;
          last_active = text_opt 5;
          message_count = int_value 6;
          archived_epoch_count = int_value 7;
          keepalive_enabled = int_value 8 <> 0;
          heartbeat_enabled = int_value 9 <> 0;
          effective_cwd = text_opt 10;
        }
        :: !rows
  done;
  ignore (Sqlite3.finalize stmt);
  List.rev !rows
  |> List.filter (fun row ->
      let effective_channel =
        match row.channel with
        | Some value -> Some value
        | None -> parse_channel_from_session_key row.session_key
      in
      let channel_ok =
        match channel with
        | None -> true
        | Some expected -> effective_channel = Some expected
      in
      let prefix_ok =
        match prefix with
        | None -> true
        | Some value ->
            let plen = String.length value in
            String.length row.session_key >= plen
            && String.sub row.session_key 0 plen = value
      in
      let main_ok =
        match only_main with
        | None -> true
        | Some expected -> row.session_key = "__main__" = expected
      in
      let activity_ok =
        match activity with
        | Any -> true
        | Active -> row.turn = Some "agent"
        | Inactive -> row.turn <> Some "agent"
      in
      let postmortem_ok =
        include_postmortem
        || not
             (let pfx = "__postmortem_" in
              let plen = String.length pfx in
              String.length row.session_key >= plen
              && String.sub row.session_key 0 plen = pfx)
      in
      channel_ok && prefix_ok && main_ok && activity_ok && postmortem_ok)

let list_session_epochs ~db ~session_key =
  let current_rows = load_raw_history ~db ~session_key in
  let current_epoch =
    let first_at =
      match current_rows with row :: _ -> Some row.created_at | [] -> None
    in
    let last_at =
      match List.rev current_rows with
      | row :: _ -> Some row.created_at
      | [] -> None
    in
    {
      epoch_id = None;
      label = "current";
      current = true;
      message_count = List.length current_rows;
      first_message_at = first_at;
      last_message_at = last_at;
      recorded_at = None;
    }
  in
  let sql =
    "SELECT id, message_count, first_message_at, last_message_at, archived_at \
     FROM session_log_epochs WHERE session_key = ? ORDER BY id DESC"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
  let archived = ref [] in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    let text_opt index =
      match Sqlite3.column stmt index with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    in
    let epoch_id =
      match Sqlite3.column stmt 0 with
      | Sqlite3.Data.INT n -> Some (Int64.to_int n)
      | _ -> None
    in
    let message_count =
      match Sqlite3.column stmt 1 with
      | Sqlite3.Data.INT n -> Int64.to_int n
      | _ -> 0
    in
    let label =
      match epoch_id with Some id -> string_of_int id | None -> "archive"
    in
    archived :=
      {
        epoch_id;
        label;
        current = false;
        message_count;
        first_message_at = text_opt 2;
        last_message_at = text_opt 3;
        recorded_at = text_opt 4;
      }
      :: !archived
  done;
  ignore (Sqlite3.finalize stmt);
  current_epoch :: List.rev !archived

let load_epoch_messages ~db ~session_key ~epoch =
  match epoch with
  | Current -> Some (load_raw_history ~db ~session_key)
  | Archived epoch_id ->
      let owner_stmt =
        Sqlite3.prepare db
          "SELECT session_key FROM session_log_epochs WHERE id = ?"
      in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize owner_stmt))
        (fun () ->
          ignore
            (Sqlite3.bind owner_stmt 1
               (Sqlite3.Data.INT (Int64.of_int epoch_id)));
          match Sqlite3.step owner_stmt with
          | Sqlite3.Rc.ROW -> (
              match Sqlite3.column owner_stmt 0 with
              | Sqlite3.Data.TEXT owner when owner = session_key ->
                  let sql =
                    "SELECT ordinal, role, content, tool_call_id, tool_name, \
                     tool_calls_json, provider_response_items_json, \
                     thinking_content, created_at FROM \
                     session_log_epoch_messages WHERE epoch_id = ? ORDER BY \
                     ordinal ASC"
                  in
                  let stmt = Sqlite3.prepare db sql in
                  ignore
                    (Sqlite3.bind stmt 1
                       (Sqlite3.Data.INT (Int64.of_int epoch_id)));
                  let rows = ref [] in
                  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
                    let text_opt index =
                      match Sqlite3.column stmt index with
                      | Sqlite3.Data.TEXT s -> Some s
                      | _ -> None
                    in
                    let ordinal =
                      match Sqlite3.column stmt 0 with
                      | Sqlite3.Data.INT n -> Int64.to_int n
                      | _ -> 0
                    in
                    let role =
                      match text_opt 1 with Some s -> s | None -> ""
                    in
                    let content =
                      match text_opt 2 with Some s -> s | None -> ""
                    in
                    let created_at =
                      match text_opt 8 with Some s -> s | None -> ""
                    in
                    rows :=
                      {
                        id = ordinal;
                        role;
                        content;
                        tool_call_id = text_opt 3;
                        tool_name = text_opt 4;
                        tool_calls_json = text_opt 5;
                        provider_response_items_json = text_opt 6;
                        thinking_content = text_opt 7;
                        created_at;
                      }
                      :: !rows
                  done;
                  ignore (Sqlite3.finalize stmt);
                  Some (List.rev !rows)
              | _ -> None)
          | _ -> None)
