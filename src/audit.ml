type event =
  | ChatMessage of { session_key : string; role : string; content_preview : string }
  | ToolInvocation of { session_key : string; tool_name : string;
                        risk_level : string; args_preview : string }
  | ToolResult of { session_key : string; tool_name : string; success : bool }
  | ConfigChange of { field : string; old_value : string; new_value : string }
  | DaemonEvent of { action : string; details : string }

type row = {
  id : int;
  timestamp : string;
  event_type : string;
  session_key : string option;
  details : string option;
  tool_name : string option;
  risk_level : string option;
}

let init_schema db =
  let sql = "CREATE TABLE IF NOT EXISTS audit_log (\n\
             \  id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
             \  timestamp TEXT NOT NULL DEFAULT (datetime('now')),\n\
             \  event_type TEXT NOT NULL,\n\
             \  session_key TEXT,\n\
             \  details TEXT,\n\
             \  tool_name TEXT,\n\
             \  risk_level TEXT\n\
             )" in
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc -> failwith (Printf.sprintf "Audit schema error: %s" (Sqlite3.Rc.to_string rc))

let log ~db event =
  let event_type, session_key, details, tool_name, risk_level =
    match event with
    | ChatMessage { session_key; role; content_preview } ->
      ("chat_message", Some session_key,
       Some (Printf.sprintf "%s: %s" role
               (if String.length content_preview > 200
                then String.sub content_preview 0 200 ^ "..."
                else content_preview)),
       None, None)
    | ToolInvocation { session_key; tool_name; risk_level; args_preview } ->
      ("tool_invocation", Some session_key,
       Some (if String.length args_preview > 200
             then String.sub args_preview 0 200 ^ "..."
             else args_preview),
       Some tool_name, Some risk_level)
    | ToolResult { session_key; tool_name; success } ->
      ("tool_result", Some session_key,
       Some (if success then "success" else "failure"),
       Some tool_name, None)
    | ConfigChange { field; old_value; new_value } ->
      ("config_change", None,
       Some (Printf.sprintf "%s: %s -> %s" field old_value new_value),
       None, None)
    | DaemonEvent { action; details } ->
      ("daemon_event", None, Some (Printf.sprintf "%s: %s" action details),
       None, None)
  in
  let sql = "INSERT INTO audit_log (event_type, session_key, details, tool_name, risk_level) \
             VALUES (?, ?, ?, ?, ?)" in
  let stmt = Sqlite3.prepare db sql in
  let bind_opt stmt idx = function
    | Some s -> ignore (Sqlite3.bind stmt idx (Sqlite3.Data.TEXT s))
    | None -> ignore (Sqlite3.bind stmt idx Sqlite3.Data.NULL)
  in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT event_type));
  bind_opt stmt 2 session_key;
  bind_opt stmt 3 details;
  bind_opt stmt 4 tool_name;
  bind_opt stmt 5 risk_level;
  (match Sqlite3.step stmt with
   | Sqlite3.Rc.DONE -> ()
   | rc -> Logs.warn (fun m -> m "Audit log failed: %s" (Sqlite3.Rc.to_string rc)));
  ignore (Sqlite3.finalize stmt)

let query ~db ?event_type ?session_key ~limit () =
  let conditions = ref [] in
  let params = ref [] in
  (match event_type with
   | Some et -> conditions := "event_type = ?" :: !conditions;
     params := et :: !params
   | None -> ());
  (match session_key with
   | Some sk -> conditions := "session_key = ?" :: !conditions;
     params := sk :: !params
   | None -> ());
  let where = match !conditions with
    | [] -> ""
    | conds -> " WHERE " ^ String.concat " AND " (List.rev conds)
  in
  let sql = Printf.sprintf
      "SELECT id, timestamp, event_type, session_key, details, tool_name, risk_level \
       FROM audit_log%s ORDER BY id DESC LIMIT ?" where in
  let stmt = Sqlite3.prepare db sql in
  let idx = ref 1 in
  List.iter (fun p ->
    ignore (Sqlite3.bind stmt !idx (Sqlite3.Data.TEXT p));
    incr idx
  ) (List.rev !params);
  ignore (Sqlite3.bind stmt !idx (Sqlite3.Data.INT (Int64.of_int limit)));
  let rows = ref [] in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    let text_opt i = match Sqlite3.column stmt i with
      | Sqlite3.Data.TEXT s -> Some s | _ -> None in
    let id = match Sqlite3.column stmt 0 with
      | Sqlite3.Data.INT i -> Int64.to_int i | _ -> 0 in
    let timestamp = match Sqlite3.column stmt 1 with
      | Sqlite3.Data.TEXT s -> s | _ -> "" in
    let event_type = match Sqlite3.column stmt 2 with
      | Sqlite3.Data.TEXT s -> s | _ -> "" in
    rows := { id; timestamp; event_type;
              session_key = text_opt 3; details = text_opt 4;
              tool_name = text_opt 5; risk_level = text_opt 6 } :: !rows
  done;
  ignore (Sqlite3.finalize stmt);
  List.rev !rows
