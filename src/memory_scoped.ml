open Memory_types
open Memory_0_schema

(* --- scoped memories --- *)

let text_col stmt index =
  match Sqlite3.column stmt index with Sqlite3.Data.TEXT s -> s | _ -> ""

let text_opt_col stmt index =
  match Sqlite3.column stmt index with
  | Sqlite3.Data.TEXT s -> Some s
  | _ -> None

let int_col stmt index =
  match Sqlite3.column stmt index with
  | Sqlite3.Data.INT n -> Int64.to_int n
  | _ -> 0

let int_opt_col stmt index =
  match Sqlite3.column stmt index with
  | Sqlite3.Data.INT n -> Some (Int64.to_int n)
  | _ -> None

let bind_text_option stmt index = function
  | Some value -> ignore (Sqlite3.bind stmt index (Sqlite3.Data.TEXT value))
  | None -> ignore (Sqlite3.bind stmt index Sqlite3.Data.NULL)

let bind_int_option stmt index = function
  | Some value ->
      ignore (Sqlite3.bind stmt index (Sqlite3.Data.INT (Int64.of_int value)))
  | None -> ignore (Sqlite3.bind stmt index Sqlite3.Data.NULL)

let memory_scope_of_stmt stmt =
  {
    id = int_col stmt 0;
    kind = text_col stmt 1;
    key = text_col stmt 2;
    profile_id = int_opt_col stmt 3;
    parent_scope_id = int_opt_col stmt 4;
    provenance = text_col stmt 5;
    created_at = text_col stmt 6;
    updated_at = text_col stmt 7;
  }

let scoped_memory_of_stmt stmt =
  {
    id = int_col stmt 0;
    scope_id = int_col stmt 1;
    scope_kind = text_col stmt 2;
    scope_key = text_col stmt 3;
    content = text_opt_col stmt 4;
    reference = text_col stmt 5;
    provenance = text_col stmt 6;
    created_at = text_col stmt 7;
    updated_at = text_col stmt 8;
    redacted_at = text_opt_col stmt 9;
    redaction_reason = text_opt_col stmt 10;
    redaction_metadata = text_opt_col stmt 11;
  }

let get_scope_by_kind_key ~db ~kind ~key =
  let sql =
    "SELECT id, kind, key, profile_id, parent_scope_id, provenance, \
     created_at, updated_at FROM memory_scopes WHERE kind = ? AND key = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT kind));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT key));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Some (memory_scope_of_stmt stmt)
      | _ -> None)

let get_scope ~db ~id =
  let sql =
    "SELECT id, kind, key, profile_id, parent_scope_id, provenance, \
     created_at, updated_at FROM memory_scopes WHERE id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int id)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Some (memory_scope_of_stmt stmt)
      | _ -> None)

let create_scope ~db ~kind ~key ?profile_id ?parent_scope_id
    ?(provenance = "unknown") () =
  match get_scope_by_kind_key ~db ~kind ~key with
  | Some scope -> scope
  | None -> (
      let sql =
        "INSERT OR IGNORE INTO memory_scopes (kind, key, profile_id, \
         parent_scope_id, provenance) VALUES (?, ?, ?, ?, ?)"
      in
      let stmt = Sqlite3.prepare db sql in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT kind));
          ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT key));
          bind_int_option stmt 3 profile_id;
          bind_int_option stmt 4 parent_scope_id;
          ignore (Sqlite3.bind stmt 5 (Sqlite3.Data.TEXT provenance));
          match Sqlite3.step stmt with
          | Sqlite3.Rc.DONE -> ()
          | rc ->
              failwith
                (Printf.sprintf "create_scope failed: %s"
                   (Sqlite3.Rc.to_string rc)));
      match get_scope_by_kind_key ~db ~kind ~key with
      | Some scope -> scope
      | None -> failwith "create_scope failed: inserted scope was not found")

let list_scopes ~db ?kind ?limit ?(offset = 0) () =
  match limit with
  | Some n when n <= 0 -> []
  | _ ->
      let clauses, params =
        match kind with
        | Some k -> ([ "kind = ?" ], [ Sqlite3.Data.TEXT k ])
        | None -> ([], [])
      in
      let sql =
        "SELECT id, kind, key, profile_id, parent_scope_id, provenance, \
         created_at, updated_at FROM memory_scopes"
        ^ (match clauses with
          | [] -> ""
          | _ -> " WHERE " ^ String.concat " AND " clauses)
        ^ " ORDER BY id"
        ^
        match limit with
        | Some _ -> " LIMIT ? OFFSET ?"
        | None -> if offset > 0 then " LIMIT -1 OFFSET ?" else ""
      in
      let stmt = Sqlite3.prepare db sql in
      let scopes = ref [] in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          let bind_index = ref 1 in
          List.iter
            (fun data ->
              ignore (Sqlite3.bind stmt !bind_index data);
              incr bind_index)
            params;
          (match limit with
          | Some n ->
              ignore
                (Sqlite3.bind stmt !bind_index
                   (Sqlite3.Data.INT (Int64.of_int n)));
              incr bind_index;
              ignore
                (Sqlite3.bind stmt !bind_index
                   (Sqlite3.Data.INT (Int64.of_int (max 0 offset))))
          | None ->
              if offset > 0 then
                ignore
                  (Sqlite3.bind stmt !bind_index
                     (Sqlite3.Data.INT (Int64.of_int offset))));
          while Sqlite3.step stmt = Sqlite3.Rc.ROW do
            scopes := memory_scope_of_stmt stmt :: !scopes
          done);
      List.rev !scopes

let get_scoped_memory ~db ~id =
  let sql =
    "SELECT m.id, m.scope_id, s.kind, s.key, m.content, m.reference, \
     m.provenance, m.created_at, m.updated_at, m.redacted_at, \
     m.redaction_reason, m.redaction_metadata FROM scoped_memories m JOIN \
     memory_scopes s ON s.id = m.scope_id WHERE m.id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int id)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Some (scoped_memory_of_stmt stmt)
      | _ -> None)

let find_scoped_memory_id ~db ~scope_id ~reference =
  let sql =
    "SELECT id FROM scoped_memories WHERE scope_id = ? AND reference = ? LIMIT \
     1"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int scope_id)));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT reference));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Some (int_col stmt 0)
      | _ -> None)

let upsert_scoped_memory ~db ~scope_id ~reference ?content
    ?(provenance = "unknown") () =
  if reference = "" then failwith "upsert_scoped_memory: reference is required";
  let inserted_or_updated_id = ref None in
  exec_exn db "BEGIN IMMEDIATE";
  (try
     (match find_scoped_memory_id ~db ~scope_id ~reference with
     | Some id ->
         let stmt =
           Sqlite3.prepare db
             "UPDATE scoped_memories SET content = ?, provenance = ?, \
              updated_at = datetime('now') WHERE id = ?"
         in
         Fun.protect
           ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
           (fun () ->
             bind_text_option stmt 1 content;
             ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT provenance));
             ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.INT (Int64.of_int id)));
             match Sqlite3.step stmt with
             | Sqlite3.Rc.DONE -> inserted_or_updated_id := Some id
             | rc ->
                 failwith
                   (Printf.sprintf "upsert_scoped_memory update failed: %s"
                      (Sqlite3.Rc.to_string rc)))
     | None ->
         let stmt =
           Sqlite3.prepare db
             "INSERT INTO scoped_memories (scope_id, content, reference, \
              provenance) VALUES (?, ?, ?, ?)"
         in
         Fun.protect
           ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
           (fun () ->
             ignore
               (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int scope_id)));
             bind_text_option stmt 2 content;
             ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT reference));
             ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT provenance));
             match Sqlite3.step stmt with
             | Sqlite3.Rc.DONE ->
                 inserted_or_updated_id :=
                   Some (Int64.to_int (Sqlite3.last_insert_rowid db))
             | rc ->
                 failwith
                   (Printf.sprintf "upsert_scoped_memory insert failed: %s"
                      (Sqlite3.Rc.to_string rc))));
     exec_exn db "COMMIT"
   with exn ->
     (try exec_exn db "ROLLBACK" with _ -> ());
     raise exn);
  match !inserted_or_updated_id with
  | Some id -> (
      match get_scoped_memory ~db ~id with
      | Some row -> row
      | None -> failwith "upsert_scoped_memory: stored row was not found")
  | None -> failwith "upsert_scoped_memory: no row stored"

let query_scoped_memories ~db ?scope_kind ?content_search ?provenance
    ?(limit = 50) ?(offset = 0) () =
  if limit <= 0 then []
  else
    let clauses = ref [ "m.redacted_at IS NULL" ] in
    let params = ref [] in
    let add_clause clause data =
      clauses := clause :: !clauses;
      params := data :: !params
    in
    Option.iter
      (fun k -> add_clause "s.kind = ?" (Sqlite3.Data.TEXT k))
      scope_kind;
    Option.iter
      (fun q ->
        if q <> "" then
          add_clause "m.content LIKE ?" (Sqlite3.Data.TEXT ("%" ^ q ^ "%")))
      content_search;
    Option.iter
      (fun p -> add_clause "m.provenance = ?" (Sqlite3.Data.TEXT p))
      provenance;
    let sql =
      "SELECT m.id, m.scope_id, s.kind, s.key, m.content, m.reference, \
       m.provenance, m.created_at, m.updated_at, m.redacted_at, \
       m.redaction_reason, m.redaction_metadata FROM scoped_memories m JOIN \
       memory_scopes s ON s.id = m.scope_id WHERE "
      ^ String.concat " AND " (List.rev !clauses)
      ^ " ORDER BY m.id LIMIT ? OFFSET ?"
    in
    let stmt = Sqlite3.prepare db sql in
    let rows = ref [] in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        let bind_index = ref 1 in
        List.iter
          (fun data ->
            ignore (Sqlite3.bind stmt !bind_index data);
            incr bind_index)
          (List.rev !params);
        ignore
          (Sqlite3.bind stmt !bind_index
             (Sqlite3.Data.INT (Int64.of_int limit)));
        incr bind_index;
        ignore
          (Sqlite3.bind stmt !bind_index
             (Sqlite3.Data.INT (Int64.of_int (max 0 offset))));
        while Sqlite3.step stmt = Sqlite3.Rc.ROW do
          rows := scoped_memory_of_stmt stmt :: !rows
        done);
    List.rev !rows

let delete_scoped_memory ~db ~id =
  let stmt = Sqlite3.prepare db "DELETE FROM scoped_memories WHERE id = ?" in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int id)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Sqlite3.changes db > 0
      | _ -> false)
