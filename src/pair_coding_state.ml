(* SQLite-backed blackboard for pair coding sessions. *)

let exec_exn db sql = Sql_util.exec_exn db sql

type pair_config = {
  task_description : string;
  max_review_rounds : int;
  interrupt_mode : Pair_coding_types.interrupt_mode;
  workspace : string option;
  worktree_path : string option;
  branch_name : string option;
  auto_swap_roles : bool;
  coder_model : string option;
  observer_model : string option;
  coordinator_model : string option;
}

type session_record = {
  id : string;
  config : pair_config;
  phase : Pair_coding_types.phase;
  coder_key : string;
  observer_key : string;
  coordinator_key : string;
  coder_approved : bool;
  observer_approved : bool;
  coder_comment : string;
  observer_comment : string;
  review_round : int;
  active : bool;
  created_at : string;
  finished_at : string option;
}

let init_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS pair_session (\n\
    \     id TEXT PRIMARY KEY,\n\
    \     task_description TEXT NOT NULL,\n\
    \     max_review_rounds INTEGER NOT NULL DEFAULT 3,\n\
    \     interrupt_mode TEXT NOT NULL DEFAULT 'asap',\n\
    \     workspace TEXT,\n\
    \     worktree_path TEXT,\n\
    \     branch_name TEXT,\n\
    \     auto_swap_roles INTEGER NOT NULL DEFAULT 0,\n\
    \     coder_model TEXT,\n\
    \     observer_model TEXT,\n\
    \     coordinator_model TEXT,\n\
    \     phase TEXT NOT NULL DEFAULT 'coding',\n\
    \     coder_key TEXT NOT NULL,\n\
    \     observer_key TEXT NOT NULL,\n\
    \     coordinator_key TEXT NOT NULL,\n\
    \     coder_approved INTEGER NOT NULL DEFAULT 0,\n\
    \     observer_approved INTEGER NOT NULL DEFAULT 0,\n\
    \     coder_comment TEXT NOT NULL DEFAULT '',\n\
    \     observer_comment TEXT NOT NULL DEFAULT '',\n\
    \     review_round INTEGER NOT NULL DEFAULT 0,\n\
    \     active INTEGER NOT NULL DEFAULT 1,\n\
    \     created_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \     finished_at TEXT\n\
    \   )";
  exec_exn db
    "CREATE TABLE IF NOT EXISTS pair_note (\n\
    \     id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \     session_id TEXT NOT NULL REFERENCES pair_session(id),\n\
    \     description TEXT NOT NULL,\n\
    \     category TEXT,\n\
    \     severity TEXT NOT NULL DEFAULT 'medium',\n\
    \     file TEXT,\n\
    \     line INTEGER,\n\
    \     resolved INTEGER NOT NULL DEFAULT 0,\n\
    \     created_at TEXT NOT NULL DEFAULT (datetime('now'))\n\
    \   )";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_pair_note_session ON pair_note(session_id)"

let generate_id () = Printf.sprintf "%016Lx" (Random.int64 Int64.max_int)

let create_session ~db ~(config : pair_config) =
  let rec try_id () =
    let id = generate_id () in
    let stmt =
      Sqlite3.prepare db "SELECT COUNT(*) FROM pair_session WHERE id = ?"
    in
    let exists =
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
          match Sqlite3.step stmt with
          | Sqlite3.Rc.ROW -> Sqlite3.column_int stmt 0 > 0
          | _ -> false)
    in
    if exists then try_id () else id
  in
  let id = try_id () in
  let coder_key = Printf.sprintf "pair:%s:coder" id in
  let observer_key = Printf.sprintf "pair:%s:obsrv" id in
  let coordinator_key = Printf.sprintf "pair:%s:coord" id in
  let stmt =
    Sqlite3.prepare db
      "INSERT INTO pair_session (id, task_description, max_review_rounds, \
       interrupt_mode, workspace, worktree_path, branch_name, auto_swap_roles, \
       coder_model, observer_model, coordinator_model, coder_key, \
       observer_key, coordinator_key) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, \
       ?, ?, ?)"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let opt_text = function Some s -> Sqlite3.Data.TEXT s | None -> NULL in
      ignore (Sqlite3.bind stmt 1 (TEXT id));
      ignore (Sqlite3.bind stmt 2 (TEXT config.task_description));
      ignore (Sqlite3.bind stmt 3 (INT (Int64.of_int config.max_review_rounds)));
      ignore
        (Sqlite3.bind stmt 4
           (TEXT
              (Pair_coding_types.interrupt_mode_to_string config.interrupt_mode)));
      ignore (Sqlite3.bind stmt 5 (opt_text config.workspace));
      ignore (Sqlite3.bind stmt 6 (opt_text config.worktree_path));
      ignore (Sqlite3.bind stmt 7 (opt_text config.branch_name));
      ignore
        (Sqlite3.bind stmt 8 (INT (if config.auto_swap_roles then 1L else 0L)));
      ignore (Sqlite3.bind stmt 9 (opt_text config.coder_model));
      ignore (Sqlite3.bind stmt 10 (opt_text config.observer_model));
      ignore (Sqlite3.bind stmt 11 (opt_text config.coordinator_model));
      ignore (Sqlite3.bind stmt 12 (TEXT coder_key));
      ignore (Sqlite3.bind stmt 13 (TEXT observer_key));
      ignore (Sqlite3.bind stmt 14 (TEXT coordinator_key));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> ()
      | rc ->
          failwith
            (Printf.sprintf "Failed to create pair session: %s"
               (Sqlite3.Rc.to_string rc)));
  id

let opt_text_col stmt i =
  match Sqlite3.column stmt i with Sqlite3.Data.TEXT s -> Some s | _ -> None

let text_col stmt i =
  match Sqlite3.column stmt i with Sqlite3.Data.TEXT s -> s | _ -> ""

let int_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.INT n -> Int64.to_int n
  | _ -> 0

let bool_col stmt i = int_col stmt i <> 0

let record_of_row stmt =
  let phase_str = text_col stmt 3 in
  let phase =
    match Pair_coding_types.phase_of_string phase_str with
    | Some p -> p
    | None -> Pair_coding_types.Coding
  in
  let interrupt_mode_str = text_col stmt 14 in
  let interrupt_mode =
    match Pair_coding_types.interrupt_mode_of_string interrupt_mode_str with
    | Some m -> m
    | None -> Pair_coding_types.Asap
  in
  {
    id = text_col stmt 0;
    config =
      {
        task_description = text_col stmt 1;
        max_review_rounds = int_col stmt 2;
        interrupt_mode;
        workspace = opt_text_col stmt 15;
        worktree_path = opt_text_col stmt 16;
        branch_name = opt_text_col stmt 17;
        auto_swap_roles = bool_col stmt 18;
        coder_model = opt_text_col stmt 19;
        observer_model = opt_text_col stmt 20;
        coordinator_model = opt_text_col stmt 21;
      };
    phase;
    coder_key = text_col stmt 4;
    observer_key = text_col stmt 5;
    coordinator_key = text_col stmt 6;
    coder_approved = bool_col stmt 7;
    observer_approved = bool_col stmt 8;
    coder_comment = text_col stmt 9;
    observer_comment = text_col stmt 10;
    review_round = int_col stmt 11;
    active = bool_col stmt 12;
    created_at = text_col stmt 13;
    finished_at = opt_text_col stmt 22;
  }

let session_select_cols =
  "id, task_description, max_review_rounds, phase, coder_key, observer_key, \
   coordinator_key, coder_approved, observer_approved, coder_comment, \
   observer_comment, review_round, active, created_at, interrupt_mode, \
   workspace, worktree_path, branch_name, auto_swap_roles, coder_model, \
   observer_model, coordinator_model, finished_at"

let load_session ~db ~id =
  let stmt =
    Sqlite3.prepare db
      (Printf.sprintf "SELECT %s FROM pair_session WHERE id = ?"
         session_select_cols)
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Some (record_of_row stmt)
      | _ -> None)

let update_phase ~db ~id (phase : Pair_coding_types.phase) =
  let stmt =
    Sqlite3.prepare db "UPDATE pair_session SET phase = ? WHERE id = ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1 (TEXT (Pair_coding_types.phase_to_string phase)));
      ignore (Sqlite3.bind stmt 2 (TEXT id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> ()
      | rc ->
          failwith
            (Printf.sprintf "Failed to update phase: %s"
               (Sqlite3.Rc.to_string rc)))

let set_approval ~db ~id ~(role : Pair_coding_types.role) ~approved ~comment =
  exec_exn db "BEGIN IMMEDIATE";
  let committed = ref false in
  Fun.protect
    ~finally:(fun () ->
      if not !committed then try exec_exn db "ROLLBACK" with _ -> ())
    (fun () ->
      let col_approved, col_comment =
        match role with
        | Pair_coding_types.Coder -> ("coder_approved", "coder_comment")
        | Observer -> ("observer_approved", "observer_comment")
        | Coordinator -> failwith "Coordinator cannot set approval"
      in
      let sql =
        Printf.sprintf "UPDATE pair_session SET %s = ?, %s = ? WHERE id = ?"
          col_approved col_comment
      in
      let stmt = Sqlite3.prepare db sql in
      ignore (Sqlite3.bind stmt 1 (INT (if approved then 1L else 0L)));
      ignore (Sqlite3.bind stmt 2 (TEXT comment));
      ignore (Sqlite3.bind stmt 3 (TEXT id));
      (match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> ()
      | rc ->
          ignore (Sqlite3.finalize stmt);
          failwith
            (Printf.sprintf "Failed to set approval: %s"
               (Sqlite3.Rc.to_string rc)));
      ignore (Sqlite3.finalize stmt);
      (* Check if both are now approved *)
      let check_stmt =
        Sqlite3.prepare db
          "SELECT coder_approved, observer_approved FROM pair_session WHERE id \
           = ?"
      in
      ignore (Sqlite3.bind check_stmt 1 (TEXT id));
      let both =
        match Sqlite3.step check_stmt with
        | Sqlite3.Rc.ROW -> bool_col check_stmt 0 && bool_col check_stmt 1
        | _ -> false
      in
      ignore (Sqlite3.finalize check_stmt);
      exec_exn db "COMMIT";
      committed := true;
      both)

let add_note ~db ~session_id ~description ?category ~severity ?file ?line () =
  let stmt =
    Sqlite3.prepare db
      "INSERT INTO pair_note (session_id, description, category, severity, \
       file, line) VALUES (?, ?, ?, ?, ?, ?)"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let opt_text = function Some s -> Sqlite3.Data.TEXT s | None -> NULL in
      let opt_int = function
        | Some n -> Sqlite3.Data.INT (Int64.of_int n)
        | None -> NULL
      in
      ignore (Sqlite3.bind stmt 1 (TEXT session_id));
      ignore (Sqlite3.bind stmt 2 (TEXT description));
      ignore
        (Sqlite3.bind stmt 3
           (opt_text (Option.map Pair_coding_types.category_to_string category)));
      ignore
        (Sqlite3.bind stmt 4
           (TEXT (Pair_coding_types.severity_to_string severity)));
      ignore (Sqlite3.bind stmt 5 (opt_text file));
      ignore (Sqlite3.bind stmt 6 (opt_int line));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> ()
      | rc ->
          failwith
            (Printf.sprintf "Failed to add note: %s" (Sqlite3.Rc.to_string rc)));
  let id = Int64.to_int (Sqlite3.last_insert_rowid db) in
  id

let load_notes ~db ~session_id =
  let stmt =
    Sqlite3.prepare db
      "SELECT id, description, category, severity, file, line, resolved, \
       created_at FROM pair_note WHERE session_id = ? ORDER BY id ASC"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_id));
      let notes = ref [] in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        let sev_str = text_col stmt 3 in
        let severity =
          match Pair_coding_types.severity_of_string sev_str with
          | Some s -> s
          | None -> Pair_coding_types.Medium
        in
        let cat_str = opt_text_col stmt 2 in
        let category =
          Option.bind cat_str Pair_coding_types.category_of_string
        in
        let line_val =
          match Sqlite3.column stmt 5 with
          | Sqlite3.Data.INT n -> Some (Int64.to_int n)
          | _ -> None
        in
        let note : Pair_coding_types.note =
          {
            id = int_col stmt 0;
            description = text_col stmt 1;
            category;
            severity;
            file = opt_text_col stmt 4;
            line = line_val;
            resolved = bool_col stmt 6;
            created_at_ms = 0;
          }
        in
        notes := note :: !notes
      done;
      List.rev !notes)

let resolve_note ~db ~note_id =
  let stmt =
    Sqlite3.prepare db "UPDATE pair_note SET resolved = 1 WHERE id = ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (INT (Int64.of_int note_id)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> ()
      | rc ->
          failwith
            (Printf.sprintf "Failed to resolve note: %s"
               (Sqlite3.Rc.to_string rc)))

let finish_session ~db ~id =
  let stmt =
    Sqlite3.prepare db
      "UPDATE pair_session SET active = 0, finished_at = datetime('now') WHERE \
       id = ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> ()
      | rc ->
          failwith
            (Printf.sprintf "Failed to finish session: %s"
               (Sqlite3.Rc.to_string rc)))

let list_sessions ~db ?(active_only = false) () =
  let sql =
    if active_only then
      Printf.sprintf
        "SELECT %s FROM pair_session WHERE active = 1 ORDER BY created_at DESC"
        session_select_cols
    else
      Printf.sprintf "SELECT %s FROM pair_session ORDER BY created_at DESC"
        session_select_cols
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let sessions = ref [] in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        sessions := record_of_row stmt :: !sessions
      done;
      List.rev !sessions)

let update_review_round ~db ~id ~round =
  let stmt =
    Sqlite3.prepare db "UPDATE pair_session SET review_round = ? WHERE id = ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (INT (Int64.of_int round)));
      ignore (Sqlite3.bind stmt 2 (TEXT id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> ()
      | rc ->
          failwith
            (Printf.sprintf "Failed to update review_round: %s"
               (Sqlite3.Rc.to_string rc)))

let clear_approvals ~db ~id =
  let stmt =
    Sqlite3.prepare db
      "UPDATE pair_session SET coder_approved = 0, observer_approved = 0, \
       coder_comment = '', observer_comment = '' WHERE id = ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (TEXT id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> ()
      | rc ->
          failwith
            (Printf.sprintf "Failed to clear approvals: %s"
               (Sqlite3.Rc.to_string rc)))
