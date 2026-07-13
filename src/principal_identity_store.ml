(* SQLite persistence for Principals, Connector actors, and Identity Links.
   See principal_identity_store.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module P = Principal_identity

(* -------------------------------------------------------------------------- *)
(* Helpers                                                                    *)
(* -------------------------------------------------------------------------- *)

let exec_schema db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "principal_identity_store schema error: %s (sql: %s)"
           (Sqlite3.Rc.to_string rc) sql)

let text_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.TEXT s -> s
  | Sqlite3.Data.NULL -> ""
  | Sqlite3.Data.INT n -> Int64.to_string n
  | _ -> ""

let opt_text_col stmt i =
  match Sqlite3.column stmt i with Sqlite3.Data.TEXT s -> Some s | _ -> None

let int_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.INT n -> Int64.to_int n
  | Sqlite3.Data.TEXT s -> ( try int_of_string s with _ -> 0)
  | _ -> 0

let with_immediate_tx db f =
  let mode =
    match Sqlite3.exec db "BEGIN IMMEDIATE" with
    | Sqlite3.Rc.OK -> `Outer
    | _ -> (
        match Sqlite3.exec db "SAVEPOINT principal_identity_store" with
        | Sqlite3.Rc.OK -> `Savepoint
        | rc ->
            `Fail
              (Printf.sprintf "BEGIN IMMEDIATE/SAVEPOINT failed: %s (%s)"
                 (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))
  in
  match mode with
  | `Fail e -> Error e
  | (`Outer | `Savepoint) as kind -> (
      let commit () =
        match kind with
        | `Outer -> (
            match Sqlite3.exec db "COMMIT" with
            | Sqlite3.Rc.OK -> Ok ()
            | rc ->
                ignore (Sqlite3.exec db "ROLLBACK");
                Error
                  (Printf.sprintf "COMMIT failed: %s" (Sqlite3.Rc.to_string rc))
            )
        | `Savepoint -> (
            match
              Sqlite3.exec db "RELEASE SAVEPOINT principal_identity_store"
            with
            | Sqlite3.Rc.OK -> Ok ()
            | rc ->
                ignore
                  (Sqlite3.exec db
                     "ROLLBACK TO SAVEPOINT principal_identity_store");
                Error
                  (Printf.sprintf "RELEASE SAVEPOINT failed: %s"
                     (Sqlite3.Rc.to_string rc)))
      in
      let rollback () =
        match kind with
        | `Outer -> ignore (Sqlite3.exec db "ROLLBACK")
        | `Savepoint ->
            ignore
              (Sqlite3.exec db "ROLLBACK TO SAVEPOINT principal_identity_store");
            ignore
              (Sqlite3.exec db "RELEASE SAVEPOINT principal_identity_store")
      in
      try
        let result = f () in
        match result with
        | Ok _ -> (
            match commit () with
            | Ok () -> result
            | Error e ->
                rollback ();
                Error e)
        | Error _ ->
            rollback ();
            result
      with exn ->
        rollback ();
        Error
          (Printf.sprintf "principal_identity_store transaction aborted: %s"
             (Printexc.to_string exn)))

let bump_revision rev = rev + 1

let generate_principal_id ?(now = Unix.gettimeofday ()) () =
  let ts = int_of_float now in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "prin_%d_%06d" ts rand

let generate_link_id ?(now = Unix.gettimeofday ()) () =
  let ts = int_of_float now in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "idlink_%d_%06d" ts rand

let ensure_timestamps ~now created_at updated_at =
  let now_s = Time_util.iso8601_utc ~t:now () in
  let created = if String.trim created_at = "" then now_s else created_at in
  let updated = if String.trim updated_at = "" then now_s else updated_at in
  (created, updated)

(* -------------------------------------------------------------------------- *)
(* Lifecycle / status codecs (local; aligned with Principal_identity strings) *)
(* -------------------------------------------------------------------------- *)

let principal_lifecycle_of_string s : (P.principal_lifecycle, string) result =
  match s with
  | "active" -> Ok P.Active
  | "disabled" -> Ok P.Disabled
  | s when String.length s > 12 && String.sub s 0 12 = "merged_into:" -> (
      match
        P.principal_id_of_string (String.sub s 12 (String.length s - 12))
      with
      | Ok id -> Ok (P.Merged_into id)
      | Error e -> Error e)
  | s -> Error (Printf.sprintf "unknown principal_lifecycle: %s" s)

let actor_lifecycle_of_string s : (P.actor_lifecycle, string) result =
  match s with
  | "active" -> Ok P.Active
  | "unlinked" -> Ok P.Unlinked
  | "disabled" -> Ok P.Disabled
  | s -> Error (Printf.sprintf "unknown actor_lifecycle: %s" s)

let identity_link_status_of_string s : (P.identity_link_status, string) result =
  match s with
  | "active" -> Ok P.Active
  | "unlinked" -> Ok P.Unlinked
  | "superseded" -> Ok P.Superseded
  | s -> Error (Printf.sprintf "unknown identity_link_status: %s" s)

(* -------------------------------------------------------------------------- *)
(* Schema                                                                     *)
(* -------------------------------------------------------------------------- *)

let ensure_schema db =
  let principals =
    {|CREATE TABLE IF NOT EXISTS principals (
      id TEXT PRIMARY KEY NOT NULL,
      version INTEGER NOT NULL,
      lifecycle TEXT NOT NULL,
      revision INTEGER NOT NULL,
      display_json TEXT NOT NULL,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    )|}
  in
  let connector_actors =
    {|CREATE TABLE IF NOT EXISTS connector_actors (
      actor_key TEXT PRIMARY KEY NOT NULL,
      version INTEGER NOT NULL,
      connector TEXT NOT NULL,
      tenant_or_workspace TEXT NOT NULL,
      immutable_user_id TEXT NOT NULL,
      principal_id TEXT NOT NULL,
      lifecycle TEXT NOT NULL,
      revision INTEGER NOT NULL,
      display_json TEXT NOT NULL,
      verified_at TEXT,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    )|}
  in
  let identity_links =
    {|CREATE TABLE IF NOT EXISTS identity_links (
      id TEXT PRIMARY KEY NOT NULL,
      version INTEGER NOT NULL,
      principal_id TEXT NOT NULL,
      actor_key TEXT NOT NULL,
      connector TEXT NOT NULL,
      tenant_or_workspace TEXT NOT NULL,
      immutable_user_id TEXT NOT NULL,
      status TEXT NOT NULL,
      revision INTEGER NOT NULL,
      linked_at TEXT NOT NULL,
      unlinked_at TEXT
    )|}
  in
  (* Collision-safe: at most one active identity link per Connector actor key. *)
  let uniq_active_link =
    {|CREATE UNIQUE INDEX IF NOT EXISTS idx_identity_links_active_actor
      ON identity_links(actor_key) WHERE status = 'active'|}
  in
  let idx_actor_principal =
    {|CREATE INDEX IF NOT EXISTS idx_connector_actors_principal
      ON connector_actors(principal_id)|}
  in
  let idx_link_principal =
    {|CREATE INDEX IF NOT EXISTS idx_identity_links_principal
      ON identity_links(principal_id)|}
  in
  let idx_link_actor =
    {|CREATE INDEX IF NOT EXISTS idx_identity_links_actor
      ON identity_links(actor_key)|}
  in
  List.iter (exec_schema db)
    [
      principals;
      connector_actors;
      identity_links;
      uniq_active_link;
      idx_actor_principal;
      idx_link_principal;
      idx_link_actor;
    ]

(* -------------------------------------------------------------------------- *)
(* Principals                                                                 *)
(* -------------------------------------------------------------------------- *)

let principal_columns =
  {|id, version, lifecycle, revision, display_json, created_at, updated_at|}

let principal_of_stmt stmt : (P.principal, string) result =
  let id_s = text_col stmt 0 in
  let version = int_col stmt 1 in
  let lifecycle_s = text_col stmt 2 in
  let revision = int_col stmt 3 in
  let display_json_s = text_col stmt 4 in
  let created_at = text_col stmt 5 in
  let updated_at = text_col stmt 6 in
  match P.principal_id_of_string id_s with
  | Error e -> Error e
  | Ok id -> (
      match principal_lifecycle_of_string lifecycle_s with
      | Error e -> Error e
      | Ok lifecycle -> (
          match
            P.display_metadata_of_json (Yojson.Safe.from_string display_json_s)
          with
          | Error e -> Error e
          | Ok display ->
              Ok
                {
                  P.version;
                  id;
                  lifecycle;
                  revision;
                  display;
                  created_at;
                  updated_at;
                }))

let get_principal ~db ~id =
  let sql =
    Printf.sprintf "SELECT %s FROM principals WHERE id = ? LIMIT 1"
      principal_columns
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT (P.principal_id_to_string id)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match principal_of_stmt stmt with
          | Ok p -> Ok (Some p)
          | Error e -> Error e)
      | Sqlite3.Rc.DONE -> Ok None
      | rc ->
          Error
            (Printf.sprintf "get_principal failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let insert_principal ~db ?(now = Unix.gettimeofday ()) (p : P.principal) =
  let created_at, updated_at =
    ensure_timestamps ~now p.created_at p.updated_at
  in
  let p = { p with created_at; updated_at } in
  let sql =
    {|INSERT INTO principals
      (id, version, lifecycle, revision, display_json, created_at, updated_at)
      VALUES (?, ?, ?, ?, ?, ?, ?)|}
  in
  with_immediate_tx db (fun () ->
      let stmt = Sqlite3.prepare db sql in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          let bind i v = ignore (Sqlite3.bind stmt i v) in
          bind 1 (Sqlite3.Data.TEXT (P.principal_id_to_string p.id));
          bind 2 (Sqlite3.Data.INT (Int64.of_int p.version));
          bind 3
            (Sqlite3.Data.TEXT (P.string_of_principal_lifecycle p.lifecycle));
          bind 4 (Sqlite3.Data.INT (Int64.of_int p.revision));
          bind 5
            (Sqlite3.Data.TEXT
               (Yojson.Safe.to_string (P.display_metadata_to_json p.display)));
          bind 6 (Sqlite3.Data.TEXT p.created_at);
          bind 7 (Sqlite3.Data.TEXT p.updated_at);
          match Sqlite3.step stmt with
          | Sqlite3.Rc.DONE -> Ok p
          | Sqlite3.Rc.CONSTRAINT ->
              Error
                (Printf.sprintf "principal already exists: id=%s (collision)"
                   (P.principal_id_to_string p.id))
          | rc ->
              Error
                (Printf.sprintf "insert_principal failed: %s (%s)"
                   (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))))

let update_principal ~db ?expected_revision ?lifecycle ?display
    ?(now = Unix.gettimeofday ()) ~id () =
  let now_s = Time_util.iso8601_utc ~t:now () in
  with_immediate_tx db (fun () ->
      match get_principal ~db ~id with
      | Error e -> Error e
      | Ok None ->
          Error
            (Printf.sprintf "principal not found: %s"
               (P.principal_id_to_string id))
      | Ok (Some cur) -> (
          match expected_revision with
          | Some exp when exp <> cur.revision ->
              Error
                (Printf.sprintf
                   "revision conflict for principal %s: expected %d, found %d"
                   (P.principal_id_to_string id)
                   exp cur.revision)
          | _ ->
              let next =
                {
                  cur with
                  lifecycle =
                    (match lifecycle with Some l -> l | None -> cur.lifecycle);
                  display =
                    (match display with Some d -> d | None -> cur.display);
                  revision = bump_revision cur.revision;
                  updated_at = now_s;
                }
              in
              let sql =
                {|UPDATE principals SET
                  lifecycle = ?,
                  revision = ?,
                  display_json = ?,
                  updated_at = ?
                WHERE id = ?|}
              in
              let stmt = Sqlite3.prepare db sql in
              Fun.protect
                ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
                (fun () ->
                  let bind i v = ignore (Sqlite3.bind stmt i v) in
                  bind 1
                    (Sqlite3.Data.TEXT
                       (P.string_of_principal_lifecycle next.lifecycle));
                  bind 2 (Sqlite3.Data.INT (Int64.of_int next.revision));
                  bind 3
                    (Sqlite3.Data.TEXT
                       (Yojson.Safe.to_string
                          (P.display_metadata_to_json next.display)));
                  bind 4 (Sqlite3.Data.TEXT next.updated_at);
                  bind 5 (Sqlite3.Data.TEXT (P.principal_id_to_string next.id));
                  match Sqlite3.step stmt with
                  | Sqlite3.Rc.DONE -> Ok next
                  | rc ->
                      Error
                        (Printf.sprintf "update_principal failed: %s (%s)"
                           (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))))

(* -------------------------------------------------------------------------- *)
(* Connector actors                                                           *)
(* -------------------------------------------------------------------------- *)

let actor_columns =
  {|actor_key, version, connector, tenant_or_workspace, immutable_user_id,
    principal_id, lifecycle, revision, display_json, verified_at,
    created_at, updated_at|}

let connector_actor_of_stmt stmt : (P.connector_actor, string) result =
  let _actor_key = text_col stmt 0 in
  let version = int_col stmt 1 in
  let connector_s = text_col stmt 2 in
  let tenant = text_col stmt 3 in
  let user = text_col stmt 4 in
  let pid_s = text_col stmt 5 in
  let lifecycle_s = text_col stmt 6 in
  let revision = int_col stmt 7 in
  let display_json_s = text_col stmt 8 in
  let verified_at = opt_text_col stmt 9 in
  let created_at = text_col stmt 10 in
  let updated_at = text_col stmt 11 in
  match
    ( P.connector_of_string connector_s,
      P.principal_id_of_string pid_s,
      actor_lifecycle_of_string lifecycle_s,
      P.display_metadata_of_json (Yojson.Safe.from_string display_json_s) )
  with
  | Error e, _, _, _ | _, Error e, _, _ | _, _, Error e, _ | _, _, _, Error e ->
      Error e
  | Ok connector, Ok principal_id, Ok lifecycle, Ok display -> (
      match
        P.make_connector_actor_key ~connector ~tenant_or_workspace:tenant
          ~immutable_user_id:user
      with
      | Error e -> Error e
      | Ok key ->
          Ok
            {
              P.version;
              key;
              principal_id;
              lifecycle;
              revision;
              display;
              verified_at;
              created_at;
              updated_at;
            })

let get_connector_actor_by_identity_key ~db ~identity_key =
  let sql =
    Printf.sprintf "SELECT %s FROM connector_actors WHERE actor_key = ? LIMIT 1"
      actor_columns
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT identity_key));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match connector_actor_of_stmt stmt with
          | Ok a -> Ok (Some a)
          | Error e -> Error e)
      | Sqlite3.Rc.DONE -> Ok None
      | rc ->
          Error
            (Printf.sprintf "get_connector_actor failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let get_connector_actor ~db ~key =
  get_connector_actor_by_identity_key ~db
    ~identity_key:(P.actor_identity_key key)

let insert_connector_actor ~db ?(now = Unix.gettimeofday ())
    (a : P.connector_actor) =
  let created_at, updated_at =
    ensure_timestamps ~now a.created_at a.updated_at
  in
  let a = { a with created_at; updated_at } in
  let identity_key = P.actor_identity_key a.key in
  let sql =
    {|INSERT INTO connector_actors
      (actor_key, version, connector, tenant_or_workspace, immutable_user_id,
       principal_id, lifecycle, revision, display_json, verified_at,
       created_at, updated_at)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)|}
  in
  with_immediate_tx db (fun () ->
      let stmt = Sqlite3.prepare db sql in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          let bind i v = ignore (Sqlite3.bind stmt i v) in
          bind 1 (Sqlite3.Data.TEXT identity_key);
          bind 2 (Sqlite3.Data.INT (Int64.of_int a.version));
          bind 3 (Sqlite3.Data.TEXT (P.string_of_connector a.key.connector));
          bind 4 (Sqlite3.Data.TEXT a.key.scope.tenant_or_workspace);
          bind 5 (Sqlite3.Data.TEXT a.key.scope.immutable_user_id);
          bind 6 (Sqlite3.Data.TEXT (P.principal_id_to_string a.principal_id));
          bind 7 (Sqlite3.Data.TEXT (P.string_of_actor_lifecycle a.lifecycle));
          bind 8 (Sqlite3.Data.INT (Int64.of_int a.revision));
          bind 9
            (Sqlite3.Data.TEXT
               (Yojson.Safe.to_string (P.display_metadata_to_json a.display)));
          bind 10
            (match a.verified_at with
            | Some s -> Sqlite3.Data.TEXT s
            | None -> Sqlite3.Data.NULL);
          bind 11 (Sqlite3.Data.TEXT a.created_at);
          bind 12 (Sqlite3.Data.TEXT a.updated_at);
          match Sqlite3.step stmt with
          | Sqlite3.Rc.DONE -> Ok a
          | Sqlite3.Rc.CONSTRAINT ->
              Error
                (Printf.sprintf
                   "connector_actor collision for key=%s (already exists; one \
                    active owner)"
                   identity_key)
          | rc ->
              Error
                (Printf.sprintf "insert_connector_actor failed: %s (%s)"
                   (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))))

let update_connector_actor ~db ?expected_revision ?principal_id ?lifecycle
    ?display ?verified_at ?(now = Unix.gettimeofday ()) ~key () =
  let now_s = Time_util.iso8601_utc ~t:now () in
  let identity_key = P.actor_identity_key key in
  with_immediate_tx db (fun () ->
      match get_connector_actor ~db ~key with
      | Error e -> Error e
      | Ok None ->
          Error (Printf.sprintf "connector_actor not found: %s" identity_key)
      | Ok (Some cur) -> (
          match expected_revision with
          | Some exp when exp <> cur.revision ->
              Error
                (Printf.sprintf
                   "revision conflict for connector_actor %s: expected %d, \
                    found %d"
                   identity_key exp cur.revision)
          | _ ->
              let next =
                {
                  cur with
                  principal_id =
                    (match principal_id with
                    | Some p -> p
                    | None -> cur.principal_id);
                  lifecycle =
                    (match lifecycle with Some l -> l | None -> cur.lifecycle);
                  display =
                    (match display with Some d -> d | None -> cur.display);
                  verified_at =
                    (match verified_at with
                    | Some v -> v
                    | None -> cur.verified_at);
                  revision = bump_revision cur.revision;
                  updated_at = now_s;
                }
              in
              let sql =
                {|UPDATE connector_actors SET
                  principal_id = ?,
                  lifecycle = ?,
                  revision = ?,
                  display_json = ?,
                  verified_at = ?,
                  updated_at = ?
                WHERE actor_key = ?|}
              in
              let stmt = Sqlite3.prepare db sql in
              Fun.protect
                ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
                (fun () ->
                  let bind i v = ignore (Sqlite3.bind stmt i v) in
                  bind 1
                    (Sqlite3.Data.TEXT
                       (P.principal_id_to_string next.principal_id));
                  bind 2
                    (Sqlite3.Data.TEXT
                       (P.string_of_actor_lifecycle next.lifecycle));
                  bind 3 (Sqlite3.Data.INT (Int64.of_int next.revision));
                  bind 4
                    (Sqlite3.Data.TEXT
                       (Yojson.Safe.to_string
                          (P.display_metadata_to_json next.display)));
                  bind 5
                    (match next.verified_at with
                    | Some s -> Sqlite3.Data.TEXT s
                    | None -> Sqlite3.Data.NULL);
                  bind 6 (Sqlite3.Data.TEXT next.updated_at);
                  bind 7 (Sqlite3.Data.TEXT identity_key);
                  match Sqlite3.step stmt with
                  | Sqlite3.Rc.DONE -> Ok next
                  | rc ->
                      Error
                        (Printf.sprintf "update_connector_actor failed: %s (%s)"
                           (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))))

(* -------------------------------------------------------------------------- *)
(* Identity links                                                             *)
(* -------------------------------------------------------------------------- *)

let link_columns =
  {|id, version, principal_id, actor_key, connector, tenant_or_workspace,
    immutable_user_id, status, revision, linked_at, unlinked_at|}

let identity_link_of_stmt stmt : (P.identity_link, string) result =
  let id = text_col stmt 0 in
  let version = int_col stmt 1 in
  let pid_s = text_col stmt 2 in
  let _actor_key = text_col stmt 3 in
  let connector_s = text_col stmt 4 in
  let tenant = text_col stmt 5 in
  let user = text_col stmt 6 in
  let status_s = text_col stmt 7 in
  let revision = int_col stmt 8 in
  let linked_at = text_col stmt 9 in
  let unlinked_at = opt_text_col stmt 10 in
  match
    ( P.principal_id_of_string pid_s,
      P.connector_of_string connector_s,
      identity_link_status_of_string status_s )
  with
  | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e
  | Ok principal_id, Ok connector, Ok status -> (
      match
        P.make_connector_actor_key ~connector ~tenant_or_workspace:tenant
          ~immutable_user_id:user
      with
      | Error e -> Error e
      | Ok actor_key ->
          Ok
            {
              P.version;
              id;
              principal_id;
              actor_key;
              status;
              revision;
              linked_at;
              unlinked_at;
            })

let get_identity_link ~db ~id =
  let sql =
    Printf.sprintf "SELECT %s FROM identity_links WHERE id = ? LIMIT 1"
      link_columns
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match identity_link_of_stmt stmt with
          | Ok l -> Ok (Some l)
          | Error e -> Error e)
      | Sqlite3.Rc.DONE -> Ok None
      | rc ->
          Error
            (Printf.sprintf "get_identity_link failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let get_active_identity_link ~db ~key =
  let identity_key = P.actor_identity_key key in
  let sql =
    Printf.sprintf
      "SELECT %s FROM identity_links WHERE actor_key = ? AND status = 'active' \
       LIMIT 1"
      link_columns
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT identity_key));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match identity_link_of_stmt stmt with
          | Ok l -> Ok (Some l)
          | Error e -> Error e)
      | Sqlite3.Rc.DONE -> Ok None
      | rc ->
          Error
            (Printf.sprintf "get_active_identity_link failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let insert_identity_link ~db ?(now = Unix.gettimeofday ()) (l : P.identity_link)
    =
  let id = if String.trim l.id = "" then generate_link_id ~now () else l.id in
  let linked_at =
    if String.trim l.linked_at = "" then Time_util.iso8601_utc ~t:now ()
    else l.linked_at
  in
  let l = { l with id; linked_at } in
  let identity_key = P.actor_identity_key l.actor_key in
  let sql =
    {|INSERT INTO identity_links
      (id, version, principal_id, actor_key, connector, tenant_or_workspace,
       immutable_user_id, status, revision, linked_at, unlinked_at)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)|}
  in
  with_immediate_tx db (fun () ->
      let stmt = Sqlite3.prepare db sql in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          let bind i v = ignore (Sqlite3.bind stmt i v) in
          bind 1 (Sqlite3.Data.TEXT l.id);
          bind 2 (Sqlite3.Data.INT (Int64.of_int l.version));
          bind 3 (Sqlite3.Data.TEXT (P.principal_id_to_string l.principal_id));
          bind 4 (Sqlite3.Data.TEXT identity_key);
          bind 5
            (Sqlite3.Data.TEXT (P.string_of_connector l.actor_key.connector));
          bind 6 (Sqlite3.Data.TEXT l.actor_key.scope.tenant_or_workspace);
          bind 7 (Sqlite3.Data.TEXT l.actor_key.scope.immutable_user_id);
          bind 8 (Sqlite3.Data.TEXT (P.string_of_identity_link_status l.status));
          bind 9 (Sqlite3.Data.INT (Int64.of_int l.revision));
          bind 10 (Sqlite3.Data.TEXT l.linked_at);
          bind 11
            (match l.unlinked_at with
            | Some s -> Sqlite3.Data.TEXT s
            | None -> Sqlite3.Data.NULL);
          match Sqlite3.step stmt with
          | Sqlite3.Rc.DONE -> Ok l
          | Sqlite3.Rc.CONSTRAINT ->
              Error
                (Printf.sprintf
                   "identity_link collision for actor_key=%s (active link \
                    already exists or id conflict)"
                   identity_key)
          | rc ->
              Error
                (Printf.sprintf "insert_identity_link failed: %s (%s)"
                   (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))))

let update_identity_link ~db ?expected_revision ?status ?principal_id
    ?unlinked_at ?(now = Unix.gettimeofday ()) ~id () =
  let now_s = Time_util.iso8601_utc ~t:now () in
  with_immediate_tx db (fun () ->
      match get_identity_link ~db ~id with
      | Error e -> Error e
      | Ok None -> Error (Printf.sprintf "identity_link not found: %s" id)
      | Ok (Some cur) -> (
          match expected_revision with
          | Some exp when exp <> cur.revision ->
              Error
                (Printf.sprintf
                   "revision conflict for identity_link %s: expected %d, found \
                    %d"
                   id exp cur.revision)
          | _ -> (
              let next_status =
                match status with Some s -> s | None -> cur.status
              in
              let next_unlinked =
                match unlinked_at with
                | Some v -> v
                | None -> (
                    match (status, cur.unlinked_at) with
                    | Some P.Unlinked, None | Some P.Superseded, None ->
                        Some now_s
                    | Some P.Active, _ -> None
                    | _ -> cur.unlinked_at)
              in
              let next =
                {
                  cur with
                  status = next_status;
                  principal_id =
                    (match principal_id with
                    | Some p -> p
                    | None -> cur.principal_id);
                  unlinked_at = next_unlinked;
                  revision = bump_revision cur.revision;
                }
              in
              (* Soft pre-check for active-link collision (UNIQUE also enforces). *)
              let collision_ok =
                match next_status with
                | P.Active -> (
                    match get_active_identity_link ~db ~key:next.actor_key with
                    | Error e -> Error e
                    | Ok None -> Ok ()
                    | Ok (Some other) when other.id = next.id -> Ok ()
                    | Ok (Some other) ->
                        Error
                          (Printf.sprintf
                             "cannot activate identity_link %s: active link %s \
                              already owns actor_key=%s"
                             next.id other.id
                             (P.actor_identity_key next.actor_key)))
                | P.Unlinked | P.Superseded -> Ok ()
              in
              match collision_ok with
              | Error e -> Error e
              | Ok () ->
                  let sql =
                    {|UPDATE identity_links SET
                      principal_id = ?,
                      status = ?,
                      revision = ?,
                      unlinked_at = ?
                    WHERE id = ?|}
                  in
                  let stmt = Sqlite3.prepare db sql in
                  Fun.protect
                    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
                    (fun () ->
                      let bind i v = ignore (Sqlite3.bind stmt i v) in
                      bind 1
                        (Sqlite3.Data.TEXT
                           (P.principal_id_to_string next.principal_id));
                      bind 2
                        (Sqlite3.Data.TEXT
                           (P.string_of_identity_link_status next.status));
                      bind 3 (Sqlite3.Data.INT (Int64.of_int next.revision));
                      bind 4
                        (match next.unlinked_at with
                        | Some s -> Sqlite3.Data.TEXT s
                        | None -> Sqlite3.Data.NULL);
                      bind 5 (Sqlite3.Data.TEXT next.id);
                      match Sqlite3.step stmt with
                      | Sqlite3.Rc.DONE -> (
                          match get_identity_link ~db ~id:next.id with
                          | Ok (Some l) -> Ok l
                          | Ok None -> Error "update succeeded but row missing"
                          | Error e -> Error e)
                      | Sqlite3.Rc.CONSTRAINT ->
                          Error
                            (Printf.sprintf
                               "identity_link collision for actor_key=%s \
                                (active link already exists)"
                               (P.actor_identity_key next.actor_key))
                      | rc ->
                          Error
                            (Printf.sprintf
                               "update_identity_link failed: %s (%s)"
                               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))))))

(* -------------------------------------------------------------------------- *)
(* First-seen atomic creation                                                 *)
(* -------------------------------------------------------------------------- *)

let create_first_seen ~db ~key ?principal_id ?(display = P.empty_display)
    ?verified_at ?(now = Unix.gettimeofday ()) () =
  let now_s = Time_util.iso8601_utc ~t:now () in
  let identity_key = P.actor_identity_key key in
  with_immediate_tx db (fun () ->
      (* Reject if actor already exists — one active owner. *)
      match get_connector_actor ~db ~key with
      | Error e -> Error e
      | Ok (Some existing) ->
          Error
            (Printf.sprintf
               "connector_actor collision for key=%s (already owned by \
                principal=%s)"
               identity_key
               (P.principal_id_to_string existing.principal_id))
      | Ok None -> (
          let pid =
            match principal_id with
            | Some id -> id
            | None -> (
                match
                  P.principal_id_of_string (generate_principal_id ~now ())
                with
                | Ok id -> id
                | Error e -> failwith e)
          in
          let principal =
            P.make_principal ~id:pid ~display ~created_at:now_s
              ~updated_at:now_s ()
          in
          let actor =
            P.make_connector_actor ~key ~principal_id:pid ~display ?verified_at
              ~created_at:now_s ~updated_at:now_s ()
          in
          let link =
            P.make_identity_link ~id:(generate_link_id ~now ())
              ~principal_id:pid ~actor_key:key ~linked_at:now_s ()
          in
          (* Nested calls open savepoints under the outer IMMEDIATE tx. *)
          match insert_principal ~db ~now principal with
          | Error e -> Error e
          | Ok principal -> (
              match insert_connector_actor ~db ~now actor with
              | Error e -> Error e
              | Ok actor -> (
                  match insert_identity_link ~db ~now link with
                  | Error e -> Error e
                  | Ok link -> Ok (principal, actor, link)))))

(* -------------------------------------------------------------------------- *)
(* Listing helpers                                                            *)
(* -------------------------------------------------------------------------- *)

let list_connector_actors_for_principal ~db ~principal_id =
  let sql =
    Printf.sprintf
      "SELECT %s FROM connector_actors WHERE principal_id = ? ORDER BY \
       actor_key ASC"
      actor_columns
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1
           (Sqlite3.Data.TEXT (P.principal_id_to_string principal_id)));
      let rec loop acc =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> (
            match connector_actor_of_stmt stmt with
            | Ok a -> loop (a :: acc)
            | Error e -> Error e)
        | Sqlite3.Rc.DONE -> Ok (List.rev acc)
        | rc ->
            Error
              (Printf.sprintf
                 "list_connector_actors_for_principal failed: %s (%s)"
                 (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))
      in
      loop [])

let list_active_identity_links_for_principal ~db ~principal_id =
  let sql =
    Printf.sprintf
      "SELECT %s FROM identity_links WHERE principal_id = ? AND status = \
       'active' ORDER BY id ASC"
      link_columns
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1
           (Sqlite3.Data.TEXT (P.principal_id_to_string principal_id)));
      let rec loop acc =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> (
            match identity_link_of_stmt stmt with
            | Ok l -> loop (l :: acc)
            | Error e -> Error e)
        | Sqlite3.Rc.DONE -> Ok (List.rev acc)
        | rc ->
            Error
              (Printf.sprintf
                 "list_active_identity_links_for_principal failed: %s (%s)"
                 (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))
      in
      loop [])
