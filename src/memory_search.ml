open Memory_types
open Memory_0_schema
open Memory_core

let insert_archive_raw_message ~db ~table_name ~id_column ~id_value ~ordinal
    (row : raw_message) =
  let sql =
    Printf.sprintf
      "INSERT INTO %s (%s, ordinal, role, content, tool_call_id, tool_name, \
       tool_calls_json, provider_response_items_json, thinking_content, \
       created_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      table_name id_column
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int id_value)));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int ordinal)));
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT row.role));
      ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT row.content));
      ignore
        (Sqlite3.bind stmt 5
           (match row.tool_call_id with
           | Some v -> Sqlite3.Data.TEXT v
           | None -> Sqlite3.Data.NULL));
      ignore
        (Sqlite3.bind stmt 6
           (match row.tool_name with
           | Some v -> Sqlite3.Data.TEXT v
           | None -> Sqlite3.Data.NULL));
      ignore
        (Sqlite3.bind stmt 7
           (match row.tool_calls_json with
           | Some v -> Sqlite3.Data.TEXT v
           | None -> Sqlite3.Data.NULL));
      ignore
        (Sqlite3.bind stmt 8
           (match row.provider_response_items_json with
           | Some v -> Sqlite3.Data.TEXT v
           | None -> Sqlite3.Data.NULL));
      ignore
        (Sqlite3.bind stmt 9
           (match row.thinking_content with
           | Some v -> Sqlite3.Data.TEXT v
           | None -> Sqlite3.Data.NULL));
      ignore (Sqlite3.bind stmt 10 (Sqlite3.Data.TEXT row.created_at));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> ()
      | rc ->
          failwith
            (Printf.sprintf "SQLite error: %s (sql: INSERT INTO %s ...)"
               (Sqlite3.Rc.to_string rc) table_name))

let archive_session ~db ~session_key =
  let live_messages = load_raw_history ~db ~session_key in
  let epoch_sql =
    "SELECT id, message_count, first_message_at, last_message_at, archived_at \
     FROM session_log_epochs WHERE session_key = ? ORDER BY id ASC"
  in
  let epoch_stmt = Sqlite3.prepare db epoch_sql in
  ignore (Sqlite3.bind epoch_stmt 1 (Sqlite3.Data.TEXT session_key));
  let epochs = ref [] in
  while Sqlite3.step epoch_stmt = Sqlite3.Rc.ROW do
    let text_opt i =
      match Sqlite3.column epoch_stmt i with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    in
    let epoch_id =
      match Sqlite3.column epoch_stmt 0 with
      | Sqlite3.Data.INT n -> Int64.to_int n
      | _ -> 0
    in
    let message_count =
      match Sqlite3.column epoch_stmt 1 with
      | Sqlite3.Data.INT n -> Int64.to_int n
      | _ -> 0
    in
    epochs :=
      (epoch_id, message_count, text_opt 2, text_opt 3, text_opt 4) :: !epochs
  done;
  ignore (Sqlite3.finalize epoch_stmt);
  let epochs = List.rev !epochs in
  let session_state_json =
    let sql =
      "SELECT turn, channel, channel_id, response_sent_at, last_active, \
       keepalive_enabled, heartbeat_enabled, model_override, effective_cwd \
       FROM session_state WHERE session_key = ?"
    in
    let stmt = Sqlite3.prepare db sql in
    ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
    let result =
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
          let text_opt i =
            match Sqlite3.column stmt i with
            | Sqlite3.Data.TEXT s -> Some s
            | _ -> None
          in
          let int_opt i =
            match Sqlite3.column stmt i with
            | Sqlite3.Data.INT n -> Some (Int64.to_int n)
            | _ -> None
          in
          let json_opt key = function
            | Some v -> (key, `String v)
            | None -> (key, `Null)
          in
          Some
            (Yojson.Safe.to_string
               (`Assoc
                  [
                    json_opt "turn" (text_opt 0);
                    json_opt "channel" (text_opt 1);
                    json_opt "channel_id" (text_opt 2);
                    json_opt "response_sent_at" (text_opt 3);
                    json_opt "last_active" (text_opt 4);
                    ( "keepalive_enabled",
                      `Int (Option.value ~default:0 (int_opt 5)) );
                    ( "heartbeat_enabled",
                      `Int (Option.value ~default:0 (int_opt 6)) );
                    json_opt "model_override" (text_opt 7);
                    json_opt "effective_cwd" (text_opt 8);
                  ]))
      | _ -> None
    in
    ignore (Sqlite3.finalize stmt);
    result
  in
  let workspace_state_json =
    let sql =
      "SELECT observed_files_json FROM session_workspace_state WHERE \
       session_key = ?"
    in
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
  in
  let summaries = Summary_store.list_for_session ~db ~session_key in
  if
    live_messages = [] && epochs = [] && session_state_json = None
    && workspace_state_json = None
    && summaries = []
  then ()
  else begin
    exec_exn db "BEGIN TRANSACTION";
    try
      let first_at =
        match live_messages with msg :: _ -> Some msg.created_at | [] -> None
      in
      let last_at =
        match List.rev live_messages with
        | msg :: _ -> Some msg.created_at
        | [] -> None
      in
      let ins_sql =
        "INSERT INTO session_archives (session_key, message_count, \
         epoch_count, first_message_at, last_message_at) VALUES (?, ?, ?, ?, \
         ?)"
      in
      let ins_stmt = Sqlite3.prepare db ins_sql in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize ins_stmt))
        (fun () ->
          ignore (Sqlite3.bind ins_stmt 1 (Sqlite3.Data.TEXT session_key));
          ignore
            (Sqlite3.bind ins_stmt 2
               (Sqlite3.Data.INT (Int64.of_int (List.length live_messages))));
          ignore
            (Sqlite3.bind ins_stmt 3
               (Sqlite3.Data.INT (Int64.of_int (List.length epochs))));
          ignore
            (Sqlite3.bind ins_stmt 4
               (match first_at with
               | Some s -> Sqlite3.Data.TEXT s
               | None -> Sqlite3.Data.NULL));
          ignore
            (Sqlite3.bind ins_stmt 5
               (match last_at with
               | Some s -> Sqlite3.Data.TEXT s
               | None -> Sqlite3.Data.NULL));
          match Sqlite3.step ins_stmt with
          | Sqlite3.Rc.DONE -> ()
          | rc ->
              failwith
                (Printf.sprintf
                   "SQLite error: %s (sql: INSERT INTO session_archives ...)"
                   (Sqlite3.Rc.to_string rc)));
      let archive_id = Sqlite3.last_insert_rowid db |> Int64.to_int in
      List.iteri
        (fun ordinal row ->
          insert_archive_raw_message ~db ~table_name:"session_archive_messages"
            ~id_column:"archive_id" ~id_value:archive_id ~ordinal row)
        live_messages;
      List.iteri
        (fun ordinal
             ( orig_epoch_id,
               message_count,
               first_msg_at,
               last_msg_at,
               orig_archived_at ) ->
          let ep_sql =
            "INSERT INTO session_archive_epochs (archive_id, orig_epoch_id, \
             ordinal, message_count, first_message_at, last_message_at, \
             orig_archived_at) VALUES (?, ?, ?, ?, ?, ?, ?)"
          in
          let ep_stmt = Sqlite3.prepare db ep_sql in
          Fun.protect
            ~finally:(fun () -> ignore (Sqlite3.finalize ep_stmt))
            (fun () ->
              ignore
                (Sqlite3.bind ep_stmt 1
                   (Sqlite3.Data.INT (Int64.of_int archive_id)));
              ignore
                (Sqlite3.bind ep_stmt 2
                   (Sqlite3.Data.INT (Int64.of_int orig_epoch_id)));
              ignore
                (Sqlite3.bind ep_stmt 3
                   (Sqlite3.Data.INT (Int64.of_int ordinal)));
              ignore
                (Sqlite3.bind ep_stmt 4
                   (Sqlite3.Data.INT (Int64.of_int message_count)));
              ignore
                (Sqlite3.bind ep_stmt 5
                   (match first_msg_at with
                   | Some s -> Sqlite3.Data.TEXT s
                   | None -> Sqlite3.Data.NULL));
              ignore
                (Sqlite3.bind ep_stmt 6
                   (match last_msg_at with
                   | Some s -> Sqlite3.Data.TEXT s
                   | None -> Sqlite3.Data.NULL));
              ignore
                (Sqlite3.bind ep_stmt 7
                   (match orig_archived_at with
                   | Some s -> Sqlite3.Data.TEXT s
                   | None -> Sqlite3.Data.NULL));
              match Sqlite3.step ep_stmt with
              | Sqlite3.Rc.DONE -> ()
              | rc ->
                  failwith
                    (Printf.sprintf
                       "SQLite error: %s (sql: INSERT INTO \
                        session_archive_epochs ...)"
                       (Sqlite3.Rc.to_string rc)));
          let archive_epoch_id = Sqlite3.last_insert_rowid db |> Int64.to_int in
          let msg_sql =
            "SELECT ordinal, role, content, tool_call_id, tool_name, \
             tool_calls_json, provider_response_items_json, thinking_content, \
             created_at FROM session_log_epoch_messages WHERE epoch_id = ? \
             ORDER BY ordinal ASC"
          in
          let msg_stmt = Sqlite3.prepare db msg_sql in
          ignore
            (Sqlite3.bind msg_stmt 1
               (Sqlite3.Data.INT (Int64.of_int orig_epoch_id)));
          while Sqlite3.step msg_stmt = Sqlite3.Rc.ROW do
            let text_opt i =
              match Sqlite3.column msg_stmt i with
              | Sqlite3.Data.TEXT s -> Some s
              | _ -> None
            in
            let ord =
              match Sqlite3.column msg_stmt 0 with
              | Sqlite3.Data.INT n -> Int64.to_int n
              | _ -> 0
            in
            let row =
              {
                id = ord;
                role = (match text_opt 1 with Some s -> s | None -> "");
                content = (match text_opt 2 with Some s -> s | None -> "");
                tool_call_id = text_opt 3;
                tool_name = text_opt 4;
                tool_calls_json = text_opt 5;
                provider_response_items_json = text_opt 6;
                thinking_content = text_opt 7;
                created_at = (match text_opt 8 with Some s -> s | None -> "");
              }
            in
            insert_archive_raw_message ~db
              ~table_name:"session_archive_epoch_messages"
              ~id_column:"archive_epoch_id" ~id_value:archive_epoch_id
              ~ordinal:ord row
          done;
          ignore (Sqlite3.finalize msg_stmt))
        epochs;
      let summaries_json =
        if summaries = [] then None
        else
          Some
            (Yojson.Safe.to_string
               (`List
                  (List.map
                     (fun (s : Summary_store.summary_record) ->
                       `Assoc
                         [
                           ("summary_id", `String s.summary_id);
                           ("tool_name", `String s.tool_name);
                           ("original_bytes", `Int s.original_bytes);
                           ("summary_bytes", `Int s.summary_bytes);
                           ("model_used", `String s.model_used);
                           ("created_at", `String s.created_at);
                         ])
                     summaries)))
      in
      let meta_sql =
        "INSERT INTO session_archive_metadata (archive_id, session_state_json, \
         workspace_state_json, summaries_json) VALUES (?, ?, ?, ?)"
      in
      let meta_stmt = Sqlite3.prepare db meta_sql in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize meta_stmt))
        (fun () ->
          ignore
            (Sqlite3.bind meta_stmt 1
               (Sqlite3.Data.INT (Int64.of_int archive_id)));
          ignore
            (Sqlite3.bind meta_stmt 2
               (match session_state_json with
               | Some s -> Sqlite3.Data.TEXT s
               | None -> Sqlite3.Data.NULL));
          ignore
            (Sqlite3.bind meta_stmt 3
               (match workspace_state_json with
               | Some s -> Sqlite3.Data.TEXT s
               | None -> Sqlite3.Data.NULL));
          ignore
            (Sqlite3.bind meta_stmt 4
               (match summaries_json with
               | Some s -> Sqlite3.Data.TEXT s
               | None -> Sqlite3.Data.NULL));
          match Sqlite3.step meta_stmt with
          | Sqlite3.Rc.DONE -> ()
          | rc ->
              failwith
                (Printf.sprintf
                   "SQLite error: %s (sql: INSERT INTO \
                    session_archive_metadata ...)"
                   (Sqlite3.Rc.to_string rc)));
      exec_exn db "COMMIT"
    with exn ->
      (try exec_exn db "ROLLBACK" with _ -> ());
      Logs.warn (fun m ->
          m "Failed to archive session %s: %s" session_key
            (Printexc.to_string exn))
  end

let list_archive_sessions ~db () =
  let sql =
    "SELECT session_key, COUNT(*) FROM session_archives GROUP BY session_key \
     ORDER BY MAX(archive_id) DESC"
  in
  let stmt = Sqlite3.prepare db sql in
  let rows = ref [] in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        let key =
          match Sqlite3.column stmt 0 with Sqlite3.Data.TEXT s -> s | _ -> ""
        in
        let count =
          match Sqlite3.column stmt 1 with
          | Sqlite3.Data.INT n -> Int64.to_int n
          | _ -> 0
        in
        if key <> "" then rows := (key, count) :: !rows
      done;
      List.rev !rows)

let list_archives_for_session ~db ~session_key =
  let sql =
    "SELECT archive_id, session_key, archived_at, message_count, epoch_count, \
     first_message_at, last_message_at FROM session_archives WHERE session_key \
     = ? ORDER BY archive_id DESC"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
  let rows = ref [] in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        let text_opt i =
          match Sqlite3.column stmt i with
          | Sqlite3.Data.TEXT s -> Some s
          | _ -> None
        in
        let int_val i =
          match Sqlite3.column stmt i with
          | Sqlite3.Data.INT n -> Int64.to_int n
          | _ -> 0
        in
        rows :=
          {
            archive_id = int_val 0;
            session_key = (match text_opt 1 with Some s -> s | None -> "");
            archived_at = (match text_opt 2 with Some s -> s | None -> "");
            message_count = int_val 3;
            epoch_count = int_val 4;
            first_message_at = text_opt 5;
            last_message_at = text_opt 6;
          }
          :: !rows
      done;
      List.rev !rows)

let get_archive_info ~db ~archive_id =
  let sql =
    "SELECT archive_id, session_key, archived_at, message_count, epoch_count, \
     first_message_at, last_message_at FROM session_archives WHERE archive_id \
     = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int archive_id)));
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
          let text_opt i =
            match Sqlite3.column stmt i with
            | Sqlite3.Data.TEXT s -> Some s
            | _ -> None
          in
          let int_val i =
            match Sqlite3.column stmt i with
            | Sqlite3.Data.INT n -> Int64.to_int n
            | _ -> 0
          in
          Some
            {
              archive_id = int_val 0;
              session_key = (match text_opt 1 with Some s -> s | None -> "");
              archived_at = (match text_opt 2 with Some s -> s | None -> "");
              message_count = int_val 3;
              epoch_count = int_val 4;
              first_message_at = text_opt 5;
              last_message_at = text_opt 6;
            }
      | _ -> None)

let load_archive_messages ~db ~archive_id =
  let sql =
    "SELECT ordinal, role, content, tool_call_id, tool_name, tool_calls_json, \
     provider_response_items_json, thinking_content, created_at FROM \
     session_archive_messages WHERE archive_id = ? ORDER BY ordinal ASC"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int archive_id)));
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let rows = ref [] in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        let text_opt index =
          match Sqlite3.column stmt index with
          | Sqlite3.Data.TEXT s -> Some s
          | _ -> None
        in
        let text index = match text_opt index with Some s -> s | None -> "" in
        let id =
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.INT n -> Int64.to_int n
          | _ -> 0
        in
        rows :=
          {
            id;
            role = text 1;
            content = text 2;
            tool_call_id = text_opt 3;
            tool_name = text_opt 4;
            tool_calls_json = text_opt 5;
            provider_response_items_json = text_opt 6;
            thinking_content = text_opt 7;
            created_at = text 8;
          }
          :: !rows
      done;
      List.rev !rows)

let snapshot_format_version = 1

let export_snapshot ~db ~path =
  let memories = list_core ~db () in
  let count = List.length memories in
  let now = Time_util.iso8601_utc () in
  let json =
    `Assoc
      [
        ("format_version", `Int snapshot_format_version);
        ("exported_at", `String now);
        ("schema_version", `Int schema_version);
        ("memory_count", `Int count);
        ( "memories",
          `List
            (List.map
               (fun (key, content, category) ->
                 `Assoc
                   [
                     ("key", `String key);
                     ("content", `String content);
                     ("category", `String category);
                   ])
               memories) );
      ]
  in
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc (Yojson.Safe.pretty_to_string json ^ "\n"));
  count

let import_snapshot ~db ~path =
  let ic = open_in path in
  let content =
    Fun.protect
      ~finally:(fun () -> close_in ic)
      (fun () -> really_input_string ic (in_channel_length ic))
  in
  let json = Yojson.Safe.from_string content in
  let open Yojson.Safe.Util in
  (* Accept both format_version (new) and version (legacy) *)
  let fv =
    try json |> member "format_version" |> to_int
    with _ -> ( try json |> member "version" |> to_int with _ -> 0)
  in
  if fv < 1 || fv > snapshot_format_version then
    failwith
      (Printf.sprintf
         "Unsupported snapshot format_version %d (this build supports up to %d)"
         fv snapshot_format_version);
  let memories = json |> member "memories" |> to_list in
  List.iter
    (fun m ->
      let key = m |> member "key" |> to_string in
      let content = m |> member "content" |> to_string in
      let category =
        try m |> member "category" |> to_string with _ -> "general"
      in
      store_core ~db ~key ~content ~category ())
    memories;
  List.length memories

let search ~db ~query ?session_key ?scope_kind ?scope_key ~limit () =
  let scoped = scope_kind <> None || scope_key <> None in
  let clauses = ref [ "messages_fts MATCH ?" ] in
  let params = ref [ Sqlite3.Data.TEXT (fts5_safe_query query) ] in
  let add_clause clause data =
    clauses := clause :: !clauses;
    params := data :: !params
  in
  Option.iter
    (fun session -> add_clause "f.session_key = ?" (Sqlite3.Data.TEXT session))
    session_key;
  Option.iter
    (fun kind -> add_clause "s.kind = ?" (Sqlite3.Data.TEXT kind))
    scope_kind;
  Option.iter
    (fun key -> add_clause "s.key = ?" (Sqlite3.Data.TEXT key))
    scope_key;
  let scope_join =
    if scoped then
      " JOIN scoped_memories sm ON (sm.reference = 'message:' || CAST(m.id AS \
       TEXT) OR sm.reference = CAST(m.id AS TEXT)) JOIN memory_scopes s ON \
       s.id = sm.scope_id"
    else ""
  in
  let sql =
    "SELECT m.role, m.content, m.tool_call_id, m.tool_name, m.tool_calls_json, \
     m.provider_response_items_json FROM messages m JOIN messages_fts f ON \
     m.id = f.rowid" ^ scope_join ^ " WHERE "
    ^ String.concat " AND " (List.rev !clauses)
    ^ " ORDER BY f.rank LIMIT ?"
  in
  let stmt = Sqlite3.prepare db sql in
  List.iteri
    (fun index data -> ignore (Sqlite3.bind stmt (index + 1) data))
    (List.rev !params);
  ignore
    (Sqlite3.bind stmt
       (List.length !params + 1)
       (Sqlite3.Data.INT (Int64.of_int limit)));
  let messages = ref [] in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    let role =
      match Sqlite3.column stmt 0 with Sqlite3.Data.TEXT s -> s | _ -> ""
    in
    let content =
      match Sqlite3.column stmt 1 with Sqlite3.Data.TEXT s -> s | _ -> ""
    in
    let tool_call_id =
      match Sqlite3.column stmt 2 with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    in
    let name =
      match Sqlite3.column stmt 3 with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    in
    let tool_calls =
      match Sqlite3.column stmt 4 with
      | Sqlite3.Data.TEXT s -> (
          try
            let json = Yojson.Safe.from_string s in
            let open Yojson.Safe.Util in
            json |> to_list
            |> List.map (fun tc ->
                {
                  Provider.id = tc |> member "id" |> to_string;
                  function_name = tc |> member "function_name" |> to_string;
                  arguments = tc |> member "arguments" |> to_string;
                })
          with _ -> [])
      | _ -> []
    in
    let provider_response_items_json =
      match Sqlite3.column stmt 5 with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    in
    messages :=
      {
        Provider.role;
        content;
        content_parts = [];
        tool_calls;
        tool_call_id;
        name;
        provider_response_items_json;
        thinking = None;
        is_error = false;
      }
      :: !messages
  done;
  ignore (Sqlite3.finalize stmt);
  List.rev !messages

let escape_like s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '%' | '_' | '\\' ->
          Buffer.add_char buf '\\';
          Buffer.add_char buf c
      | _ -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let search_session_history ~db ~session_key ~query ~limit () =
  let like_pattern = "%" ^ escape_like query ^ "%" in
  let current_results =
    let fts_results =
      try
        let sql =
          "SELECT m.role, m.content, m.created_at, 'current' AS source FROM \
           messages m JOIN messages_fts f ON m.id = f.rowid WHERE messages_fts \
           MATCH ? AND f.session_key = ? ORDER BY f.rank LIMIT ?"
        in
        let stmt = Sqlite3.prepare db sql in
        Fun.protect
          ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
          (fun () ->
            (* B654: escape FTS5 colons/quotes *)
            ignore
              (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT (fts5_safe_query query)));
            ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT session_key));
            ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.INT (Int64.of_int limit)));
            let rows = ref [] in
            while Sqlite3.step stmt = Sqlite3.Rc.ROW do
              let role =
                match Sqlite3.column stmt 0 with
                | Sqlite3.Data.TEXT s -> s
                | _ -> ""
              in
              let content =
                match Sqlite3.column stmt 1 with
                | Sqlite3.Data.TEXT s -> s
                | _ -> ""
              in
              let created_at =
                match Sqlite3.column stmt 2 with
                | Sqlite3.Data.TEXT s -> s
                | _ -> ""
              in
              rows :=
                ({ role; content; created_at; source = "current" }
                  : history_search_result)
                :: !rows
            done;
            Some (List.rev !rows))
      with _ -> None
    in
    match fts_results with
    | Some r -> r
    | None ->
        let sql =
          "SELECT role, content, created_at, 'current' AS source FROM messages \
           WHERE session_key = ? AND content LIKE ? ESCAPE '\\' ORDER BY \
           created_at DESC LIMIT ?"
        in
        let stmt = Sqlite3.prepare db sql in
        Fun.protect
          ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
          (fun () ->
            ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
            ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT like_pattern));
            ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.INT (Int64.of_int limit)));
            let rows = ref [] in
            while Sqlite3.step stmt = Sqlite3.Rc.ROW do
              let role =
                match Sqlite3.column stmt 0 with
                | Sqlite3.Data.TEXT s -> s
                | _ -> ""
              in
              let content =
                match Sqlite3.column stmt 1 with
                | Sqlite3.Data.TEXT s -> s
                | _ -> ""
              in
              let created_at =
                match Sqlite3.column stmt 2 with
                | Sqlite3.Data.TEXT s -> s
                | _ -> ""
              in
              rows :=
                ({ role; content; created_at; source = "current" }
                  : history_search_result)
                :: !rows
            done;
            List.rev !rows)
  in
  let archived_results =
    let sql =
      "SELECT em.role, em.content, em.created_at, 'epoch:' || e.id AS source \
       FROM session_log_epoch_messages em JOIN session_log_epochs e ON \
       em.epoch_id = e.id WHERE e.session_key = ? AND em.content LIKE ? ESCAPE \
       '\\' ORDER BY em.created_at DESC LIMIT ?"
    in
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
        ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT like_pattern));
        ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.INT (Int64.of_int limit)));
        let rows = ref [] in
        while Sqlite3.step stmt = Sqlite3.Rc.ROW do
          let role =
            match Sqlite3.column stmt 0 with
            | Sqlite3.Data.TEXT s -> s
            | _ -> ""
          in
          let content =
            match Sqlite3.column stmt 1 with
            | Sqlite3.Data.TEXT s -> s
            | _ -> ""
          in
          let created_at =
            match Sqlite3.column stmt 2 with
            | Sqlite3.Data.TEXT s -> s
            | _ -> ""
          in
          let source =
            match Sqlite3.column stmt 3 with
            | Sqlite3.Data.TEXT s -> s
            | _ -> "epoch:?"
          in
          rows :=
            ({ role; content; created_at; source } : history_search_result)
            :: !rows
        done;
        List.rev !rows)
  in
  let merged = current_results @ archived_results in
  let sorted =
    List.sort
      (fun (a : history_search_result) (b : history_search_result) ->
        String.compare b.created_at a.created_at)
      merged
  in
  if List.length sorted <= limit then sorted
  else List.filteri (fun i _ -> i < limit) sorted
