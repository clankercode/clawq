type summary_record = {
  summary_id : string;
  session_key : string;
  tool_name : string;
  original_content : string;
  summary_content : string;
  context_snippet : string;
  original_bytes : int;
  original_lines : int;
  original_tokens_est : int;
  summary_bytes : int;
  summary_lines : int;
  summary_tokens_est : int;
  model_used : string;
  created_at : string;
}

let exec_exn db sql = Sql_util.exec_exn db sql

let init_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS summaries (\n\
    \  summary_id TEXT PRIMARY KEY,\n\
    \  session_key TEXT NOT NULL,\n\
    \  tool_name TEXT NOT NULL,\n\
    \  original_content TEXT NOT NULL,\n\
    \  summary_content TEXT NOT NULL,\n\
    \  context_snippet TEXT NOT NULL DEFAULT '',\n\
    \  original_bytes INTEGER NOT NULL,\n\
    \  original_lines INTEGER NOT NULL,\n\
    \  original_tokens_est INTEGER NOT NULL,\n\
    \  summary_bytes INTEGER NOT NULL,\n\
    \  summary_lines INTEGER NOT NULL,\n\
    \  summary_tokens_est INTEGER NOT NULL,\n\
    \  model_used TEXT NOT NULL,\n\
    \  created_at TEXT NOT NULL DEFAULT (datetime('now'))\n\
     )";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_summaries_session ON \
     summaries(session_key, created_at DESC)";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_summaries_created ON summaries(created_at)"

let estimate_tokens s = (String.length s + 3) / 4

let count_lines s =
  if String.length s = 0 then 0
  else begin
    let n = ref 1 in
    String.iter (fun c -> if c = '\n' then incr n) s;
    !n
  end

let store ~db (r : summary_record) =
  let sql =
    "INSERT INTO summaries (summary_id, session_key, tool_name, \
     original_content, summary_content, context_snippet, original_bytes, \
     original_lines, original_tokens_est, summary_bytes, summary_lines, \
     summary_tokens_est, model_used, created_at) VALUES (?, ?, ?, ?, ?, ?, ?, \
     ?, ?, ?, ?, ?, ?, ?)"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let open Sqlite3.Data in
      let binds =
        [|
          TEXT r.summary_id;
          TEXT r.session_key;
          TEXT r.tool_name;
          TEXT r.original_content;
          TEXT r.summary_content;
          TEXT r.context_snippet;
          INT (Int64.of_int r.original_bytes);
          INT (Int64.of_int r.original_lines);
          INT (Int64.of_int r.original_tokens_est);
          INT (Int64.of_int r.summary_bytes);
          INT (Int64.of_int r.summary_lines);
          INT (Int64.of_int r.summary_tokens_est);
          TEXT r.model_used;
          TEXT r.created_at;
        |]
      in
      Array.iteri (fun i v -> ignore (Sqlite3.bind stmt (i + 1) v)) binds;
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> ()
      | rc ->
          failwith
            (Printf.sprintf "Failed to store summary %s: %s" r.summary_id
               (Sqlite3.Rc.to_string rc)))

let find ~db ~summary_id =
  let sql =
    "SELECT summary_id, session_key, tool_name, original_content, \
     summary_content, context_snippet, original_bytes, original_lines, \
     original_tokens_est, summary_bytes, summary_lines, summary_tokens_est, \
     model_used, created_at FROM summaries WHERE summary_id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT summary_id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
          let col i = Sqlite3.column stmt i in
          let to_s = function Sqlite3.Data.TEXT s -> s | _ -> "" in
          let to_i = function Sqlite3.Data.INT n -> Int64.to_int n | _ -> 0 in
          Some
            {
              summary_id = to_s (col 0);
              session_key = to_s (col 1);
              tool_name = to_s (col 2);
              original_content = to_s (col 3);
              summary_content = to_s (col 4);
              context_snippet = to_s (col 5);
              original_bytes = to_i (col 6);
              original_lines = to_i (col 7);
              original_tokens_est = to_i (col 8);
              summary_bytes = to_i (col 9);
              summary_lines = to_i (col 10);
              summary_tokens_est = to_i (col 11);
              model_used = to_s (col 12);
              created_at = to_s (col 13);
            }
      | _ -> None)

let delete_for_session ~db ~session_key =
  let sql = "DELETE FROM summaries WHERE session_key = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
      ignore (Sqlite3.step stmt))

let purge_older_than ~db ~max_age_days =
  let sql =
    Printf.sprintf
      "DELETE FROM summaries WHERE created_at < datetime('now', '-%d days')"
      max_age_days
  in
  match Sqlite3.exec db sql with Sqlite3.Rc.OK -> Sqlite3.changes db | _ -> 0

let list_for_session ~db ~session_key =
  let sql =
    "SELECT summary_id, session_key, tool_name, original_content, \
     summary_content, context_snippet, original_bytes, original_lines, \
     original_tokens_est, summary_bytes, summary_lines, summary_tokens_est, \
     model_used, created_at FROM summaries WHERE session_key = ? ORDER BY \
     created_at ASC"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
      let rows = ref [] in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        let col i = Sqlite3.column stmt i in
        let to_s = function Sqlite3.Data.TEXT s -> s | _ -> "" in
        let to_i = function Sqlite3.Data.INT n -> Int64.to_int n | _ -> 0 in
        rows :=
          {
            summary_id = to_s (col 0);
            session_key = to_s (col 1);
            tool_name = to_s (col 2);
            original_content = to_s (col 3);
            summary_content = to_s (col 4);
            context_snippet = to_s (col 5);
            original_bytes = to_i (col 6);
            original_lines = to_i (col 7);
            original_tokens_est = to_i (col 8);
            summary_bytes = to_i (col 9);
            summary_lines = to_i (col 10);
            summary_tokens_est = to_i (col 11);
            model_used = to_s (col 12);
            created_at = to_s (col 13);
          }
          :: !rows
      done;
      List.rev !rows)

let generate_id () =
  let bytes = Mirage_crypto_rng.generate 6 in
  let buf = Buffer.create 16 in
  Buffer.add_string buf "sum_";
  String.iter
    (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c)))
    bytes;
  Buffer.contents buf
