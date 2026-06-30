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

let sanitize_content_preview ?(max_len = 200) content =
  let s =
    if String.length content > max_len then String.sub content 0 max_len ^ "..."
    else content
  in
  Str.global_replace (Str.regexp "Bearer [A-Za-z0-9._+/=-]+") "[REDACTED]" s

type ledger_fn =
  room_id:string ->
  event_type:string ->
  actor:string ->
  metadata:Yojson.Safe.t ->
  unit

let emit_memory_event ?ledger ~scope_kind ~scope_key ~event_type ~actor
    ~memory_id ?visibility ?content_preview () =
  match ledger with
  | None -> ()
  | Some emit ->
      let fields =
        [
          ("memory_id", `Int memory_id);
          ("scope_kind", `String scope_kind);
          ("scope_key", `String scope_key);
          ("principal", `String actor);
        ]
      in
      let fields =
        match visibility with
        | Some v -> ("visibility", `String (visibility_to_string v)) :: fields
        | None -> fields
      in
      let fields =
        match content_preview with
        | Some p -> ("content_preview", `String p) :: fields
        | None -> fields
      in
      emit ~room_id:scope_key ~event_type ~actor ~metadata:(`Assoc fields)

let emit_grant_event ?ledger ~scope_kind ~scope_key ~event_type ~actor
    ~principal_kind ~principal_id ~capability () =
  match ledger with
  | None -> ()
  | Some emit ->
      let metadata =
        `Assoc
          [
            ("scope_kind", `String scope_kind);
            ("scope_key", `String scope_key);
            ("principal_kind", `String principal_kind);
            ("principal_id", `String principal_id);
            ("capability", `String capability);
          ]
      in
      emit ~room_id:scope_key ~event_type ~actor ~metadata

let sqlite_column_exists ~db ~table_name ~column_name =
  let stmt =
    Sqlite3.prepare db (Printf.sprintf "PRAGMA table_info(%s)" table_name)
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let found = ref false in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        match Sqlite3.column stmt 1 with
        | Sqlite3.Data.TEXT s when s = column_name -> found := true
        | _ -> ()
      done;
      !found)

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
    visibility =
      (match text_opt_col stmt 7 with
      | Some s -> visibility_of_string s
      | None -> Public);
    created_at = text_col stmt 8;
    updated_at = text_col stmt 9;
    redacted_at = text_opt_col stmt 10;
    redaction_reason = text_opt_col stmt 11;
    redaction_metadata = text_opt_col stmt 12;
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

let memory_grant_admin_error = "memory grant mutations require admin privileges"

let require_memory_grant_admin ~is_admin =
  if is_admin then Ok () else Error memory_grant_admin_error

let grant_access ~db ~is_admin ~scope_id ~principal_kind ~principal_id
    ~capability ?(grantor_kind = "admin") ?(grantor_id = "cli") ?ledger () =
  match require_memory_grant_admin ~is_admin with
  | Error _ as err -> err
  | Ok () -> (
      let sql =
        "INSERT OR IGNORE INTO memory_grants (scope_id, principal_kind, \
         principal_id, capability, grantor_kind, grantor_id) VALUES (?, ?, ?, \
         ?, ?, ?)"
      in
      let stmt = Sqlite3.prepare db sql in
      try
        Fun.protect
          ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
          (fun () ->
            ignore
              (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int scope_id)));
            ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT principal_kind));
            ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT principal_id));
            ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT capability));
            ignore (Sqlite3.bind stmt 5 (Sqlite3.Data.TEXT grantor_kind));
            ignore (Sqlite3.bind stmt 6 (Sqlite3.Data.TEXT grantor_id));
            match Sqlite3.step stmt with
            | Sqlite3.Rc.DONE ->
                (match get_scope ~db ~id:scope_id with
                | Some scope ->
                    emit_grant_event ?ledger ~scope_kind:scope.kind
                      ~scope_key:scope.key ~event_type:"scope_granted"
                      ~actor:grantor_id ~principal_kind ~principal_id
                      ~capability ()
                | None -> ());
                Ok ()
            | rc ->
                Error
                  (Printf.sprintf "failed to create memory grant: %s"
                     (Sqlite3.Rc.to_string rc)))
      with exn ->
        Error ("failed to create memory grant: " ^ Printexc.to_string exn))

let revoke_access ~db ~is_admin ~scope_id ~principal_kind ~principal_id
    ~capability ?ledger () =
  match require_memory_grant_admin ~is_admin with
  | Error _ as err -> err
  | Ok () -> (
      let sql =
        "DELETE FROM memory_grants WHERE scope_id = ? AND principal_kind = ? \
         AND principal_id = ? AND capability = ?"
      in
      let stmt = Sqlite3.prepare db sql in
      try
        Fun.protect
          ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
          (fun () ->
            ignore
              (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int scope_id)));
            ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT principal_kind));
            ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT principal_id));
            ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT capability));
            match Sqlite3.step stmt with
            | Sqlite3.Rc.DONE ->
                let changes = Sqlite3.changes db in
                (if changes > 0 then
                   match get_scope ~db ~id:scope_id with
                   | Some scope ->
                       emit_grant_event ?ledger ~scope_kind:scope.kind
                         ~scope_key:scope.key ~event_type:"scope_revoked"
                         ~actor:"admin" ~principal_kind ~principal_id
                         ~capability ()
                   | None -> ());
                Ok changes
            | rc ->
                Error
                  (Printf.sprintf "failed to revoke memory grant: %s"
                     (Sqlite3.Rc.to_string rc)))
      with exn ->
        Error ("failed to revoke memory grant: " ^ Printexc.to_string exn))

let scope_grant_of_stmt stmt =
  {
    id = int_col stmt 0;
    scope_id = int_col stmt 1;
    principal_kind = text_col stmt 2;
    principal_id = text_col stmt 3;
    capability = text_col stmt 4;
    grantor_kind = text_col stmt 5;
    grantor_id = text_col stmt 6;
    created_at = text_col stmt 7;
    expires_at = text_opt_col stmt 8;
  }

let list_grants ~db ~scope_id =
  let sql =
    "SELECT id, scope_id, principal_kind, principal_id, capability, \
     grantor_kind, grantor_id, created_at, expires_at FROM memory_grants WHERE \
     scope_id = ? ORDER BY id"
  in
  let stmt = Sqlite3.prepare db sql in
  let grants = ref [] in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int scope_id)));
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        grants := scope_grant_of_stmt stmt :: !grants
      done;
      List.rev !grants)

let list_scopes_granted_to_principal ~db ~principal_kind ~principal_id
    ~capability =
  let revoked_clause =
    if
      sqlite_column_exists ~db ~table_name:"memory_grants"
        ~column_name:"revoked_at"
    then " AND revoked_at IS NULL"
    else ""
  in
  let sql =
    "SELECT DISTINCT s.id, s.kind, s.key, s.profile_id, s.parent_scope_id, \
     s.provenance, s.created_at, s.updated_at FROM memory_grants g JOIN \
     memory_scopes s ON s.id = g.scope_id WHERE g.principal_kind = ? AND \
     g.principal_id = ? AND g.capability = ? AND (g.expires_at IS NULL OR \
     datetime(g.expires_at) > datetime('now'))" ^ revoked_clause
  in
  let stmt = Sqlite3.prepare db sql in
  let scopes = ref [] in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT principal_kind));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT principal_id));
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT capability));
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        scopes := memory_scope_of_stmt stmt :: !scopes
      done;
      List.rev !scopes)

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
     m.provenance, m.visibility, m.created_at, m.updated_at, m.redacted_at, \
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
    "SELECT id FROM scoped_memories WHERE scope_id = ? AND reference = ? AND \
     redacted_at IS NULL LIMIT 1"
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
    ?(provenance = "unknown") ?visibility ?ledger () =
  if reference = "" then failwith "upsert_scoped_memory: reference is required";
  let inserted_or_updated_id = ref None in
  exec_exn db "BEGIN IMMEDIATE";
  (try
     (match find_scoped_memory_id ~db ~scope_id ~reference with
     | Some id -> (
         (* Update: only change visibility if explicitly specified *)
         match visibility with
         | Some vis ->
             let stmt =
               Sqlite3.prepare db
                 "UPDATE scoped_memories SET content = ?, provenance = ?, \
                  visibility = ?, updated_at = datetime('now') WHERE id = ?"
             in
             Fun.protect
               ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
               (fun () ->
                 bind_text_option stmt 1 content;
                 ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT provenance));
                 ignore
                   (Sqlite3.bind stmt 3
                      (Sqlite3.Data.TEXT (visibility_to_string vis)));
                 ignore
                   (Sqlite3.bind stmt 4 (Sqlite3.Data.INT (Int64.of_int id)));
                 match Sqlite3.step stmt with
                 | Sqlite3.Rc.DONE -> inserted_or_updated_id := Some id
                 | rc ->
                     failwith
                       (Printf.sprintf "upsert_scoped_memory update failed: %s"
                          (Sqlite3.Rc.to_string rc)))
         | None ->
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
                 ignore
                   (Sqlite3.bind stmt 3 (Sqlite3.Data.INT (Int64.of_int id)));
                 match Sqlite3.step stmt with
                 | Sqlite3.Rc.DONE -> inserted_or_updated_id := Some id
                 | rc ->
                     failwith
                       (Printf.sprintf "upsert_scoped_memory update failed: %s"
                          (Sqlite3.Rc.to_string rc))))
     | None ->
         let stmt =
           Sqlite3.prepare db
             "INSERT INTO scoped_memories (scope_id, content, reference, \
              provenance, visibility) VALUES (?, ?, ?, ?, ?)"
         in
         Fun.protect
           ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
           (fun () ->
             ignore
               (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int scope_id)));
             bind_text_option stmt 2 content;
             ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT reference));
             ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT provenance));
             ignore
               (Sqlite3.bind stmt 5
                  (Sqlite3.Data.TEXT
                     (visibility_to_string
                        (Option.value ~default:Public visibility))));
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
      | Some row ->
          emit_memory_event ?ledger ~scope_kind:row.scope_kind
            ~scope_key:row.scope_key ~event_type:"memory_saved"
            ~actor:provenance ~memory_id:row.id ~visibility:row.visibility
            ?content_preview:(Option.map sanitize_content_preview content)
            ();
          row
      | None -> failwith "upsert_scoped_memory: stored row was not found")
  | None -> failwith "upsert_scoped_memory: no row stored"

let query_scoped_memories ~db ?scope_kind ?scope_key ?content_search ?provenance
    ?visibility ?(limit = 50) ?(offset = 0) () =
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
      (fun k -> add_clause "s.key = ?" (Sqlite3.Data.TEXT k))
      scope_key;
    Option.iter
      (fun q ->
        if q <> "" then
          add_clause "m.content LIKE ?" (Sqlite3.Data.TEXT ("%" ^ q ^ "%")))
      content_search;
    Option.iter
      (fun p -> add_clause "m.provenance = ?" (Sqlite3.Data.TEXT p))
      provenance;
    Option.iter
      (fun v ->
        add_clause "m.visibility = ?"
          (Sqlite3.Data.TEXT (visibility_to_string v)))
      visibility;
    let sql =
      "SELECT m.id, m.scope_id, s.kind, s.key, m.content, m.reference, \
       m.provenance, m.visibility, m.created_at, m.updated_at, m.redacted_at, \
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

let delete_scoped_memory ~db ~id ?ledger () =
  let existing = get_scoped_memory ~db ~id in
  let stmt = Sqlite3.prepare db "DELETE FROM scoped_memories WHERE id = ?" in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int id)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE ->
          let success = Sqlite3.changes db > 0 in
          if success then
            Option.iter
              (fun (m : scoped_memory) ->
                emit_memory_event ?ledger ~scope_kind:m.scope_kind
                  ~scope_key:m.scope_key ~event_type:"memory_hard_purged"
                  ~actor:"admin" ~memory_id:m.id ~visibility:m.visibility
                  ?content_preview:
                    (Option.map sanitize_content_preview m.content)
                  ())
              existing;
          success
      | _ -> false)

(** Update the content of an existing scoped memory, preserving the old content
    in provenance metadata. Returns the updated memory row or [None] if the
    memory does not exist. *)
let correct_scoped_memory ~db ~id ~content ?(provenance = "corrected") ?ledger
    () =
  match get_scoped_memory ~db ~id with
  | None -> None
  | Some existing ->
      let old_content = Option.value ~default:"(empty)" existing.content in
      let old_provenance = existing.provenance in
      let new_provenance =
        Printf.sprintf "%s | prev_provenance: %s | prev_content: %s" provenance
          old_provenance
          (if String.length old_content > 200 then
             String.sub old_content 0 197 ^ "..."
           else old_content)
      in
      let stmt =
        Sqlite3.prepare db
          "UPDATE scoped_memories SET content = ?, provenance = ?, updated_at \
           = datetime('now') WHERE id = ?"
      in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT content));
          ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT new_provenance));
          ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.INT (Int64.of_int id)));
          match Sqlite3.step stmt with
          | Sqlite3.Rc.DONE ->
              let result = get_scoped_memory ~db ~id in
              Option.iter
                (fun (row : scoped_memory) ->
                  emit_memory_event ?ledger ~scope_kind:row.scope_kind
                    ~scope_key:row.scope_key ~event_type:"memory_corrected"
                    ~actor:provenance ~memory_id:row.id
                    ~visibility:row.visibility
                    ~content_preview:(sanitize_content_preview content)
                    ())
                result;
              result
          | _ -> None)

(** Soft-delete (redact) a scoped memory by setting [redacted_at],
    [redaction_reason], clearing the content, and redacting any provenance that
    may contain previous content. The row remains in the database for audit
    purposes but content is no longer visible. *)
let redact_scoped_memory ~db ~id ?(reason = "user request") ?ledger () =
  let existing = get_scoped_memory ~db ~id in
  let stmt =
    Sqlite3.prepare db
      "UPDATE scoped_memories SET redacted_at = datetime('now'), \
       redaction_reason = ?, content = NULL, reference = COALESCE(reference, \
       'redacted:' || id), provenance = CASE WHEN provenance LIKE \
       '%prev_content:%' THEN 'redacted' ELSE provenance END, updated_at = \
       datetime('now') WHERE id = ? AND redacted_at IS NULL"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT reason));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int id)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE ->
          let success = Sqlite3.changes db > 0 in
          if success then
            Option.iter
              (fun (m : scoped_memory) ->
                emit_memory_event ?ledger ~scope_kind:m.scope_kind
                  ~scope_key:m.scope_key ~event_type:"memory_forgotten"
                  ~actor:"user" ~memory_id:m.id ~visibility:m.visibility
                  ?content_preview:
                    (Option.map sanitize_content_preview m.content)
                  ())
              existing;
          success
      | _ -> false)

let resolve_grants ~db ~scope_id ~principal_kind ~principal_id =
  let revoked_clause =
    if
      sqlite_column_exists ~db ~table_name:"memory_grants"
        ~column_name:"revoked_at"
    then " AND revoked_at IS NULL"
    else ""
  in
  let sql =
    "SELECT DISTINCT capability FROM memory_grants WHERE scope_id = ? AND \
     principal_kind = ? AND principal_id = ? AND (expires_at IS NULL OR \
     datetime(expires_at) > datetime('now'))" ^ revoked_clause
    ^ " ORDER BY capability"
  in
  let stmt = Sqlite3.prepare db sql in
  let capabilities = ref [] in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int scope_id)));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT principal_kind));
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT principal_id));
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        match Sqlite3.column stmt 0 with
        | Sqlite3.Data.TEXT s -> capabilities := s :: !capabilities
        | _ -> ()
      done;
      List.rev !capabilities)

(* --- team grants (for team visibility) --- *)

let team_grant_of_stmt stmt =
  {
    id = int_col stmt 0;
    memory_id = int_col stmt 1;
    principal_kind = text_col stmt 2;
    principal_id = text_col stmt 3;
    granted_at = text_col stmt 4;
  }

let add_team_grant ~db ~memory_id ~principal_kind ~principal_id ?ledger () =
  let sql =
    "INSERT OR IGNORE INTO memory_team_grants (memory_id, principal_kind, \
     principal_id) VALUES (?, ?, ?)"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int memory_id)));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT principal_kind));
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT principal_id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE ->
          let success = Sqlite3.changes db > 0 in
          if success then
            Option.iter
              (fun (m : scoped_memory) ->
                emit_memory_event ?ledger ~scope_kind:m.scope_kind
                  ~scope_key:m.scope_key ~event_type:"team_grant_added"
                  ~actor:"admin" ~memory_id:m.id ~visibility:m.visibility
                  ?content_preview:
                    (Option.map sanitize_content_preview m.content)
                  ())
              (get_scoped_memory ~db ~id:memory_id);
          success
      | _ -> false)

let remove_team_grant ~db ~memory_id ~principal_kind ~principal_id ?ledger () =
  let sql =
    "DELETE FROM memory_team_grants WHERE memory_id = ? AND principal_kind = ? \
     AND principal_id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int memory_id)));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT principal_kind));
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT principal_id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE ->
          let success = Sqlite3.changes db > 0 in
          if success then
            Option.iter
              (fun (m : scoped_memory) ->
                emit_memory_event ?ledger ~scope_kind:m.scope_kind
                  ~scope_key:m.scope_key ~event_type:"team_grant_removed"
                  ~actor:"admin" ~memory_id:m.id ~visibility:m.visibility
                  ?content_preview:
                    (Option.map sanitize_content_preview m.content)
                  ())
              (get_scoped_memory ~db ~id:memory_id);
          success
      | _ -> false)

let list_team_grants ~db ~memory_id =
  let sql =
    "SELECT id, memory_id, principal_kind, principal_id, granted_at FROM \
     memory_team_grants WHERE memory_id = ? ORDER BY id"
  in
  let stmt = Sqlite3.prepare db sql in
  let grants = ref [] in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int memory_id)));
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        grants := team_grant_of_stmt stmt :: !grants
      done;
      List.rev !grants)

let has_team_grant ~db ~memory_id ~principal_kind ~principal_id =
  let sql =
    "SELECT 1 FROM memory_team_grants WHERE memory_id = ? AND principal_kind = \
     ? AND principal_id = ? LIMIT 1"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int memory_id)));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT principal_kind));
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT principal_id));
      Sqlite3.step stmt = Sqlite3.Rc.ROW)

(** Check if a principal can see a memory based on its visibility.
    - Public: always visible
    - Private: only visible if principal is the scope owner (profile_id match)
    - Team: visible if principal has an explicit team_grant *)
let can_see_memory ~db ~(scoped_mem : scoped_memory) ~principal_kind
    ~principal_id ~scope_profile_id =
  match scoped_mem.visibility with
  | Public -> true
  | Private -> (
      match scope_profile_id with
      | Some owner_id -> owner_id = principal_id
      | None -> false)
  | Team ->
      has_team_grant ~db ~memory_id:scoped_mem.id ~principal_kind ~principal_id

(** Get the visibility of a memory by its id. Returns [None] if the memory does
    not exist or is redacted. *)
let get_memory_visibility ~db ~id =
  let sql =
    "SELECT visibility FROM scoped_memories WHERE id = ? AND redacted_at IS \
     NULL"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int id)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.TEXT s -> visibility_of_string_opt s
          | _ -> None)
      | _ -> None)

(** Set the visibility of a memory. Returns true if the update succeeded. *)
let set_memory_visibility ~db ~id ~(visibility : memory_visibility) =
  let stmt =
    Sqlite3.prepare db
      "UPDATE scoped_memories SET visibility = ?, updated_at = datetime('now') \
       WHERE id = ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1
           (Sqlite3.Data.TEXT (visibility_to_string visibility)));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int id)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Sqlite3.changes db > 0
      | _ -> false)
