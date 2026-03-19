type postmortem = {
  id : int;
  session_key : string;
  created_at : string;
  pattern : string;
  evidence_json : string;
  correction_injected : string;
  outcome : string option;
  doc_path : string;
}

let postmortem_of_stmt stmt =
  let text_col i =
    match Sqlite3.column stmt i with Sqlite3.Data.TEXT s -> s | _ -> ""
  in
  let int_col i =
    match Sqlite3.column stmt i with
    | Sqlite3.Data.INT n -> Int64.to_int n
    | _ -> 0
  in
  let text_opt_col i =
    match Sqlite3.column stmt i with
    | Sqlite3.Data.TEXT s -> Some s
    | Sqlite3.Data.NULL -> None
    | _ -> None
  in
  {
    id = int_col 0;
    session_key = text_col 1;
    created_at = text_col 2;
    pattern = text_col 3;
    evidence_json = text_col 4;
    correction_injected = text_col 5;
    outcome = text_opt_col 6;
    doc_path = text_col 7;
  }

let insert_postmortem ~db ~session_key ~pattern ~evidence_json
    ~correction_injected ~doc_path =
  let sql =
    "INSERT INTO postmortems (session_key, pattern, evidence_json, \
     correction_injected, doc_path) VALUES (?, ?, ?, ?, ?)"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT pattern));
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT evidence_json));
      ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT correction_injected));
      ignore (Sqlite3.bind stmt 5 (Sqlite3.Data.TEXT doc_path));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Int64.to_int (Sqlite3.last_insert_rowid db)
      | rc ->
          failwith
            (Printf.sprintf "insert_postmortem failed: %s"
               (Sqlite3.Rc.to_string rc)))

let list_postmortems ~db ?session_key ?(limit = 100) () =
  let sql, bind_key =
    match session_key with
    | None ->
        ( "SELECT id, session_key, created_at, pattern, evidence_json, \
           correction_injected, outcome, doc_path FROM postmortems ORDER BY id \
           DESC LIMIT ?",
          false )
    | Some _ ->
        ( "SELECT id, session_key, created_at, pattern, evidence_json, \
           correction_injected, outcome, doc_path FROM postmortems WHERE \
           session_key = ? ORDER BY id DESC LIMIT ?",
          true )
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      (if bind_key then
         match session_key with
         | Some k ->
             ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT k));
             ignore
               (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int limit)))
         | None -> ());
      if not bind_key then
        ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int limit)));
      let rows = ref [] in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        rows := postmortem_of_stmt stmt :: !rows
      done;
      List.rev !rows)

let update_postmortem_outcome ~db ~id ~outcome =
  let sql = "UPDATE postmortems SET outcome = ? WHERE id = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT outcome));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int id)));
      ignore (Sqlite3.step stmt))

let pending_question_upsert ~db ~session_key ~questions_json ~question_index =
  let sql =
    "INSERT INTO pending_questions (session_key, questions_json, \
     question_index) VALUES (?, ?, ?) ON CONFLICT(session_key) DO UPDATE SET \
     questions_json = excluded.questions_json, question_index = \
     excluded.question_index"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT questions_json));
      ignore
        (Sqlite3.bind stmt 3 (Sqlite3.Data.INT (Int64.of_int question_index)));
      ignore (Sqlite3.step stmt))

let pending_question_delete ~db ~session_key =
  let sql = "DELETE FROM pending_questions WHERE session_key = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
      ignore (Sqlite3.step stmt))

let pending_question_list_all ~db =
  let sql =
    "SELECT session_key, questions_json, question_index FROM pending_questions"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let rows = ref [] in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        let session_key =
          match Sqlite3.column stmt 0 with Sqlite3.Data.TEXT s -> s | _ -> ""
        in
        let questions_json =
          match Sqlite3.column stmt 1 with
          | Sqlite3.Data.TEXT s -> s
          | _ -> "[]"
        in
        let question_index =
          match Sqlite3.column stmt 2 with
          | Sqlite3.Data.INT n -> Int64.to_int n
          | _ -> 0
        in
        rows := (session_key, questions_json, question_index) :: !rows
      done;
      List.rev !rows)
