type history_entry = {
  id : int;
  task_id : int;
  seq : int;
  direction : string;
  msg_type : string;
  update_type : string option;
  role : string option;
  content_text : string option;
  raw_json : string;
  tool_call_id : string option;
  created_at : string;
}

let init_schema db =
  let exec sql =
    match Sqlite3.exec db sql with
    | Sqlite3.Rc.OK -> ()
    | rc ->
        failwith
          (Printf.sprintf "SQLite acp_history error: %s (sql: %s)"
             (Sqlite3.Rc.to_string rc) sql)
  in
  exec
    "CREATE TABLE IF NOT EXISTS acp_history (\n\
    \  id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \  task_id INTEGER NOT NULL,\n\
    \  seq INTEGER NOT NULL,\n\
    \  direction TEXT NOT NULL,\n\
    \  msg_type TEXT NOT NULL,\n\
    \  update_type TEXT,\n\
    \  role TEXT,\n\
    \  content_text TEXT,\n\
    \  raw_json TEXT NOT NULL,\n\
    \  tool_call_id TEXT,\n\
    \  created_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \  FOREIGN KEY (task_id) REFERENCES background_tasks(id)\n\
     )";
  exec
    "CREATE INDEX IF NOT EXISTS idx_acp_history_task ON acp_history (task_id, \
     seq)"

let next_seq ~db ~task_id =
  let sql =
    "SELECT COALESCE(MAX(seq), 0) + 1 FROM acp_history WHERE task_id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int task_id)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.INT i -> Int64.to_int i
          | _ -> 1)
      | _ -> 1)

let record ~db ~task_id ~direction ~msg_type ?update_type ?role ?content_text
    ?tool_call_id ~raw_json () =
  let seq = next_seq ~db ~task_id in
  let sql =
    "INSERT INTO acp_history (task_id, seq, direction, msg_type, update_type, \
     role, content_text, raw_json, tool_call_id) VALUES (?, ?, ?, ?, ?, ?, ?, \
     ?, ?)"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let bind_int i v =
        ignore (Sqlite3.bind stmt i (Sqlite3.Data.INT (Int64.of_int v)))
      in
      let bind_text i v = ignore (Sqlite3.bind stmt i (Sqlite3.Data.TEXT v)) in
      let bind_opt i = function
        | Some v -> bind_text i v
        | None -> ignore (Sqlite3.bind stmt i Sqlite3.Data.NULL)
      in
      bind_int 1 task_id;
      bind_int 2 seq;
      bind_text 3 direction;
      bind_text 4 msg_type;
      bind_opt 5 update_type;
      bind_opt 6 role;
      bind_opt 7 content_text;
      bind_text 8 (Yojson.Safe.to_string raw_json);
      bind_opt 9 tool_call_id;
      ignore (Sqlite3.step stmt))

let sql_text = function Sqlite3.Data.TEXT s -> Some s | _ -> None
let sql_int = function Sqlite3.Data.INT i -> Some (Int64.to_int i) | _ -> None

let entry_of_stmt stmt =
  {
    id = Option.value (sql_int (Sqlite3.column stmt 0)) ~default:0;
    task_id = Option.value (sql_int (Sqlite3.column stmt 1)) ~default:0;
    seq = Option.value (sql_int (Sqlite3.column stmt 2)) ~default:0;
    direction = Sqlite3.column stmt 3 |> sql_text |> Option.value ~default:"";
    msg_type = Sqlite3.column stmt 4 |> sql_text |> Option.value ~default:"";
    update_type = Sqlite3.column stmt 5 |> sql_text;
    role = Sqlite3.column stmt 6 |> sql_text;
    content_text = Sqlite3.column stmt 7 |> sql_text;
    raw_json = Sqlite3.column stmt 8 |> sql_text |> Option.value ~default:"{}";
    tool_call_id = Sqlite3.column stmt 9 |> sql_text;
    created_at = Sqlite3.column stmt 10 |> sql_text |> Option.value ~default:"";
  }

let get_history ~db ~task_id =
  let sql =
    "SELECT id, task_id, seq, direction, msg_type, update_type, role, \
     content_text, raw_json, tool_call_id, created_at FROM acp_history WHERE \
     task_id = ? ORDER BY seq ASC"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int task_id)));
      let rows = ref [] in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        rows := entry_of_stmt stmt :: !rows
      done;
      List.rev !rows)

let export_jsonl ~db ~task_id =
  let entries = get_history ~db ~task_id in
  entries |> List.map (fun e -> e.raw_json) |> String.concat "\n"

let has_history ~db ~task_id =
  let sql = "SELECT COUNT(*) FROM acp_history WHERE task_id = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int task_id)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.INT i -> Int64.to_int i > 0
          | _ -> false)
      | _ -> false)

let format_for_display ~db ~task_id ?(max_lines = 200) () =
  let entries = get_history ~db ~task_id in
  let buf = Buffer.create 4096 in
  let line_count = ref 0 in
  let add_line s =
    if !line_count < max_lines then begin
      Buffer.add_string buf s;
      Buffer.add_char buf '\n';
      incr line_count
    end
  in
  add_line (Printf.sprintf "=== ACP Session for task %d ===" task_id);
  List.iter
    (fun entry ->
      if !line_count >= max_lines then ()
      else
        match entry.msg_type with
        | "prompt" -> (
            add_line "";
            add_line "--- User ---";
            match entry.content_text with
            | Some text -> add_line text
            | None -> ())
        | "update" -> (
            match entry.update_type with
            | Some "agent_message_chunk" -> (
                add_line "";
                add_line "--- Agent ---";
                match entry.content_text with
                | Some text -> add_line text
                | None -> ())
            | Some "thought_message_chunk" -> (
                add_line "";
                add_line "--- Thought ---";
                match entry.content_text with
                | Some text -> add_line text
                | None -> ())
            | Some "tool_call" ->
                let title =
                  match entry.content_text with Some t -> t | None -> "tool"
                in
                let status_str =
                  match entry.tool_call_id with
                  | Some id -> Printf.sprintf " [%s]" id
                  | None -> ""
                in
                add_line "";
                add_line (Printf.sprintf "--- Tool: %s%s ---" title status_str)
            | Some "tool_call_update" ->
                let status_str =
                  match entry.content_text with
                  | Some s -> s
                  | None -> "updated"
                in
                add_line (Printf.sprintf "  [%s]" status_str)
            | Some "plan" -> (
                add_line "";
                add_line "--- Plan ---";
                match entry.content_text with
                | Some text -> add_line text
                | None -> ())
            | Some typ -> add_line (Printf.sprintf "  [%s]" typ)
            | None -> (
                match entry.content_text with
                | Some text -> add_line text
                | None -> ()))
        | "response" -> (
            match entry.content_text with
            | Some text ->
                add_line "";
                add_line (Printf.sprintf "=== Stop: %s ===" text)
            | None -> ())
        | _ -> ())
    entries;
  if !line_count >= max_lines then
    Buffer.add_string buf
      (Printf.sprintf "\n(Output truncated at %d lines)\n" max_lines);
  Buffer.contents buf
