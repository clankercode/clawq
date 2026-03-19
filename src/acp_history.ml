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

let entries_to_document ~task_id entries =
  let open Content_dsl in
  let header =
    Paragraph [ Bold (Printf.sprintf "ACP Session for task %d" task_id) ]
  in
  let blocks =
    List.filter_map
      (fun entry ->
        match entry.msg_type with
        | "prompt" ->
            let content =
              match entry.content_text with Some t -> t | None -> ""
            in
            Some
              [
                Separator;
                Paragraph [ Emoji "\xF0\x9F\x91\xA4"; Text " "; Bold "User" ];
                Paragraph [ Text content ];
              ]
        | "update" -> (
            match entry.update_type with
            | Some "agent_message_chunk" ->
                let content =
                  match entry.content_text with Some t -> t | None -> ""
                in
                Some
                  [
                    Paragraph
                      [ Emoji "\xF0\x9F\xA4\x96"; Text " "; Bold "Agent" ];
                    Paragraph [ Text content ];
                  ]
            | Some "thought_message_chunk" ->
                let content =
                  match entry.content_text with Some t -> t | None -> ""
                in
                Some [ ThinkingPreview content ]
            | Some "tool_call" ->
                let name =
                  match entry.content_text with Some t -> t | None -> "tool"
                in
                Some
                  [
                    ToolEntry
                      {
                        emoji = "\xF0\x9F\x94\xA7";
                        name;
                        summary = entry.tool_call_id;
                        state = Running;
                        timing = None;
                        preview = None;
                        error_detail = None;
                        connector_char = None;
                      };
                  ]
            | Some "tool_call_update" ->
                let summary =
                  match entry.content_text with
                  | Some t -> t
                  | None -> "updated"
                in
                Some
                  [
                    ToolEntry
                      {
                        emoji = "\xF0\x9F\x94\xA7";
                        name = "tool";
                        summary = Some summary;
                        state = Done;
                        timing = None;
                        preview = None;
                        error_detail = None;
                        connector_char = None;
                      };
                  ]
            | Some "plan" ->
                let content =
                  match entry.content_text with Some t -> t | None -> ""
                in
                Some
                  [
                    Paragraph
                      [ Emoji "\xF0\x9F\x93\x8B"; Text " "; Bold "Plan" ];
                    CodeBlock { language = None; content };
                  ]
            | Some typ -> Some [ Paragraph [ Italic ("[" ^ typ ^ "]") ] ]
            | None ->
                let content =
                  match entry.content_text with Some t -> t | None -> ""
                in
                Some [ Paragraph [ Text content ] ])
        | "response" ->
            let content =
              match entry.content_text with Some t -> t | None -> ""
            in
            Some
              [ Separator; Paragraph [ Bold "Stop"; Text ": "; Code content ] ]
        | _ -> None)
      entries
    |> List.flatten
  in
  header :: blocks

let format_for_display_rich ~db ~task_id ?(connector = Format_adapter.Plain)
    ?(max_lines = 200) () =
  let entries = get_history ~db ~task_id in
  let doc = entries_to_document ~task_id entries in
  let full = Content_dsl.render_document connector doc in
  let lines = String.split_on_char '\n' full in
  if List.length lines <= max_lines then full
  else
    let taken = List.filteri (fun i _ -> i < max_lines) lines in
    String.concat "\n" taken
    ^ Printf.sprintf "\n\n(Output truncated at %d lines)\n" max_lines

let format_for_display ~db ~task_id ?(max_lines = 200) () =
  format_for_display_rich ~db ~task_id ~connector:Format_adapter.Plain
    ~max_lines ()
