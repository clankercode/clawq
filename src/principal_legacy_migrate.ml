(* Migrate legacy requester identities without unsafe coalescing
   (P21.M1.E3.T003). See principal_legacy_migrate.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module P = Principal_identity
module S = Principal_identity_store
module M = Principal_merge

let schema_version = 1

(* -------------------------------------------------------------------------- *)
(* Helpers                                                                    *)
(* -------------------------------------------------------------------------- *)

let exec_schema db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "principal_legacy_migrate schema error: %s (sql: %s)"
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

let bool_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.INT n -> n <> 0L
  | Sqlite3.Data.TEXT s -> s = "1" || String.lowercase_ascii s = "true"
  | _ -> false

let trim_opt = function
  | None -> None
  | Some s ->
      let t = String.trim s in
      if t = "" then None else Some t

let with_immediate_tx db f =
  let mode =
    match Sqlite3.exec db "BEGIN IMMEDIATE" with
    | Sqlite3.Rc.OK -> `Outer
    | _ -> (
        match Sqlite3.exec db "SAVEPOINT principal_legacy_migrate" with
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
              Sqlite3.exec db "RELEASE SAVEPOINT principal_legacy_migrate"
            with
            | Sqlite3.Rc.OK -> Ok ()
            | rc ->
                ignore
                  (Sqlite3.exec db
                     "ROLLBACK TO SAVEPOINT principal_legacy_migrate");
                Error
                  (Printf.sprintf "RELEASE SAVEPOINT failed: %s"
                     (Sqlite3.Rc.to_string rc)))
      in
      let rollback () =
        match kind with
        | `Outer -> ignore (Sqlite3.exec db "ROLLBACK")
        | `Savepoint ->
            ignore
              (Sqlite3.exec db "ROLLBACK TO SAVEPOINT principal_legacy_migrate");
            ignore
              (Sqlite3.exec db "RELEASE SAVEPOINT principal_legacy_migrate")
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
          (Printf.sprintf "principal_legacy_migrate transaction aborted: %s"
             (Printexc.to_string exn)))

let table_exists db name =
  let sql =
    "SELECT 1 FROM sqlite_master WHERE type='table' AND name=? LIMIT 1"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT name));
      match Sqlite3.step stmt with Sqlite3.Rc.ROW -> true | _ -> false)

(* -------------------------------------------------------------------------- *)
(* Schema                                                                     *)
(* -------------------------------------------------------------------------- *)

let ensure_schema db =
  exec_schema db
    {|CREATE TABLE IF NOT EXISTS principal_legacy_migration_runs (
      run_id TEXT PRIMARY KEY,
      schema_version INTEGER NOT NULL,
      started_at TEXT NOT NULL,
      finished_at TEXT,
      backfilled INTEGER NOT NULL DEFAULT 0,
      unresolved INTEGER NOT NULL DEFAULT 0,
      jobs_invalidated INTEGER NOT NULL DEFAULT 0,
      rolled_back INTEGER NOT NULL DEFAULT 0
    )|};
  exec_schema db
    {|CREATE TABLE IF NOT EXISTS principal_legacy_migration_records (
      id TEXT PRIMARY KEY,
      run_id TEXT NOT NULL,
      source_kind TEXT NOT NULL,
      source_id TEXT NOT NULL,
      status TEXT NOT NULL,
      classification_kind TEXT NOT NULL,
      unresolved_reason TEXT,
      actor_key TEXT,
      principal_id TEXT,
      followed_merge_alias INTEGER NOT NULL DEFAULT 0,
      actor_revision INTEGER,
      identity_link_id TEXT,
      user_attributed_allowed INTEGER NOT NULL DEFAULT 0,
      app_behavior_allowed INTEGER NOT NULL DEFAULT 1,
      read_audit_allowed INTEGER NOT NULL DEFAULT 1,
      evidence_json TEXT NOT NULL,
      connector TEXT,
      tenant_or_workspace TEXT,
      immutable_user_id TEXT,
      requester_name TEXT,
      room_id TEXT,
      session_id TEXT,
      origin_json TEXT,
      raw_requester TEXT,
      job_active INTEGER NOT NULL DEFAULT 0,
      created_at TEXT NOT NULL,
      UNIQUE(source_kind, source_id)
    )|};
  exec_schema db
    {|CREATE INDEX IF NOT EXISTS idx_principal_legacy_mig_run
      ON principal_legacy_migration_records(run_id)|};
  exec_schema db
    {|CREATE TABLE IF NOT EXISTS principal_legacy_invalidated_jobs (
      source_kind TEXT NOT NULL,
      source_id TEXT NOT NULL,
      run_id TEXT NOT NULL,
      reason TEXT NOT NULL,
      created_at TEXT NOT NULL,
      PRIMARY KEY (source_kind, source_id)
    )|}

(* -------------------------------------------------------------------------- *)
(* Source kinds / rows                                                        *)
(* -------------------------------------------------------------------------- *)

type source_kind = Background_task | Workflow_run | Fixture

let string_of_source_kind = function
  | Background_task -> "background_task"
  | Workflow_run -> "workflow_run"
  | Fixture -> "fixture"

let source_kind_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "background_task" -> Ok Background_task
  | "workflow_run" -> Ok Workflow_run
  | "fixture" -> Ok Fixture
  | other -> Error (Printf.sprintf "unknown source_kind: %s" other)

type legacy_row = {
  source_kind : source_kind;
  source_id : string;
  connector : string option;
  tenant_or_workspace : string option;
  immutable_user_id : string option;
  requester_name : string option;
  room_id : string option;
  session_id : string option;
  origin_json : string option;
  raw_requester : string option;
  job_active : bool;
  evidence_json : string;
}

let evidence_of_fields ~connector ~tenant_or_workspace ~immutable_user_id
    ~requester_name ~room_id ~session_id ~origin_json ~raw_requester ~job_active
    =
  let opt k = function None -> [] | Some s -> [ (k, `String s) ] in
  `Assoc
    ([
       ("schema_version", `Int schema_version); ("job_active", `Bool job_active);
     ]
    @ opt "connector" connector
    @ opt "tenant_or_workspace" tenant_or_workspace
    @ opt "immutable_user_id" immutable_user_id
    @ opt "requester_name" requester_name
    @ opt "room_id" room_id
    @ opt "session_id" session_id
    @ opt "origin_json" origin_json
    @ opt "raw_requester" raw_requester)

let make_legacy_row ~source_kind ~source_id ?connector ?tenant_or_workspace
    ?immutable_user_id ?requester_name ?room_id ?session_id ?origin_json
    ?raw_requester ?(job_active = false) ?evidence_json () =
  let source_id = String.trim source_id in
  if source_id = "" then Error "source_id must be non-empty"
  else
    let connector = trim_opt connector in
    let tenant_or_workspace = trim_opt tenant_or_workspace in
    let immutable_user_id = trim_opt immutable_user_id in
    let requester_name = trim_opt requester_name in
    let room_id = trim_opt room_id in
    let session_id = trim_opt session_id in
    let origin_json = trim_opt origin_json in
    let raw_requester = trim_opt raw_requester in
    let evidence_json =
      match evidence_json with
      | Some s when String.trim s <> "" -> String.trim s
      | _ ->
          Yojson.Safe.to_string
            (evidence_of_fields ~connector ~tenant_or_workspace
               ~immutable_user_id ~requester_name ~room_id ~session_id
               ~origin_json ~raw_requester ~job_active)
    in
    Ok
      {
        source_kind;
        source_id;
        connector;
        tenant_or_workspace;
        immutable_user_id;
        requester_name;
        room_id;
        session_id;
        origin_json;
        raw_requester;
        job_active;
        evidence_json;
      }

let legacy_row_of_origin ~source_kind ~source_id ?(job_active = false)
    ?raw_requester ?session_id (o : Room_origin.t) =
  make_legacy_row ~source_kind ~source_id ?connector:o.connector
    ?tenant_or_workspace:o.workspace_id ?immutable_user_id:o.requester_id
    ?requester_name:o.requester_name ?room_id:o.room_id ?session_id
    ?origin_json:(Some (Room_origin.to_json_string o))
    ?raw_requester ~job_active ()

(* -------------------------------------------------------------------------- *)
(* Classification                                                             *)
(* -------------------------------------------------------------------------- *)

type unresolved_reason =
  | Missing_connector
  | Missing_namespace
  | Missing_user_id
  | Display_name_only
  | Non_adapter_connector of string
  | Malformed_actor_key of string
  | Actor_not_found
  | Actor_disabled
  | Principal_not_active of string
  | Ambiguous_evidence of string
  | Coalesce_refused of string

let string_of_unresolved_reason = function
  | Missing_connector -> "missing_connector"
  | Missing_namespace -> "missing_namespace"
  | Missing_user_id -> "missing_user_id"
  | Display_name_only -> "display_name_only"
  | Non_adapter_connector s -> "non_adapter_connector:" ^ s
  | Malformed_actor_key s -> "malformed_actor_key:" ^ s
  | Actor_not_found -> "actor_not_found"
  | Actor_disabled -> "actor_disabled"
  | Principal_not_active s -> "principal_not_active:" ^ s
  | Ambiguous_evidence s -> "ambiguous_evidence:" ^ s
  | Coalesce_refused s -> "coalesce_refused:" ^ s

let unresolved_reason_of_string s =
  let s = String.trim s in
  let strip prefix =
    let n = String.length prefix in
    if String.length s > n && String.sub s 0 n = prefix then
      Some (String.sub s n (String.length s - n))
    else None
  in
  match s with
  | "missing_connector" -> Ok Missing_connector
  | "missing_namespace" -> Ok Missing_namespace
  | "missing_user_id" -> Ok Missing_user_id
  | "display_name_only" -> Ok Display_name_only
  | "actor_not_found" -> Ok Actor_not_found
  | "actor_disabled" -> Ok Actor_disabled
  | _ -> (
      match strip "non_adapter_connector:" with
      | Some rest -> Ok (Non_adapter_connector rest)
      | None -> (
          match strip "malformed_actor_key:" with
          | Some rest -> Ok (Malformed_actor_key rest)
          | None -> (
              match strip "principal_not_active:" with
              | Some rest -> Ok (Principal_not_active rest)
              | None -> (
                  match strip "ambiguous_evidence:" with
                  | Some rest -> Ok (Ambiguous_evidence rest)
                  | None -> (
                      match strip "coalesce_refused:" with
                      | Some rest -> Ok (Coalesce_refused rest)
                      | None ->
                          Error
                            (Printf.sprintf "unknown unresolved_reason: %s" s)))
              )))

type classification =
  | Backfill of {
      actor_key : P.connector_actor_key;
      principal_id : P.principal_id;
      followed_merge_alias : bool;
      actor_revision : int;
      identity_link_id : string option;
    }
  | Legacy_unresolved of { reason : unresolved_reason }

(** Adapter-verifiable connectors that can carry namespace+user identity. Web is
    only accepted when tenant looks like an issuer (contains "://") and user is
    present — matching bootstrap shape, not bare session metadata. *)
let adapter_connector_of_string s =
  match P.connector_of_string s with
  | Error e -> Error e
  | Ok ((P.Cli | P.Direct) as c) ->
      Error
        (string_of_unresolved_reason
           (Non_adapter_connector (P.string_of_connector c)))
  | Ok (P.Web as c) -> Ok c
  | Ok ((P.Teams | P.Slack | P.Discord | P.Telegram) as c) -> Ok c

let rec follow_merge_alias ~db ~seen (id : P.principal_id) =
  let id_s = P.principal_id_to_string id in
  if List.exists (String.equal id_s) seen then
    Error (Printf.sprintf "principal merge alias cycle involving %s" id_s)
  else
    match S.get_principal ~db ~id with
    | Error e -> Error e
    | Ok None -> Ok (id, false)
    | Ok (Some p) -> (
        match p.lifecycle with
        | P.Merged_into target -> (
            match follow_merge_alias ~db ~seen:(id_s :: seen) target with
            | Error e -> Error e
            | Ok (root, _) -> Ok (root, true))
        | P.Active | P.Disabled -> Ok (id, false))

(** Detect conflicting immutable user claims in free-form evidence without
    coalescing on display names. *)
let detect_conflicting_user_ids (row : legacy_row) =
  match (row.immutable_user_id, row.raw_requester) with
  | Some uid, Some raw ->
      let raw = String.trim raw in
      let uid = String.trim uid in
      if raw <> "" && uid <> "" && raw <> uid && not (String.contains raw ' ')
      then
        (* raw looks like a distinct id, not a display phrase *)
        if
          (not (String.contains raw '@'))
          && String.length raw < 128
          && not
               (String.equal
                  (String.lowercase_ascii raw)
                  (String.lowercase_ascii uid))
        then
          (* Only refuse when raw is a different bare id-like token. *)
          if String.equal raw uid then None
          else if
            (* Allow raw equal to actor key or namespaced forms containing uid *)
            let contains =
              let n = String.length uid in
              let m = String.length raw in
              let rec loop i =
                if i + n > m then false
                else if String.sub raw i n = uid then true
                else loop (i + 1)
              in
              loop 0
            in
            contains
          then None
          else
            Some (Coalesce_refused ("conflicting_user_ids:" ^ uid ^ "<>" ^ raw))
        else None
      else None
  | _ -> None

let classify_shape (row : legacy_row) :
    (P.connector_actor_key, unresolved_reason) result =
  match detect_conflicting_user_ids row with
  | Some reason -> Error reason
  | None -> (
      match row.connector with
      | None -> (
          match
            (row.immutable_user_id, row.requester_name, row.raw_requester)
          with
          | None, Some _, _ | None, _, Some _ -> Error Display_name_only
          | None, None, None -> Error Missing_connector
          | Some _, _, _ -> Error Missing_connector)
      | Some conn_s -> (
          match adapter_connector_of_string conn_s with
          | Error msg
            when String.starts_with ~prefix:"non_adapter_connector:" msg ->
              Error (Non_adapter_connector conn_s)
          | Error msg -> Error (Malformed_actor_key msg)
          | Ok connector -> (
              match row.tenant_or_workspace with
              | None -> Error Missing_namespace
              | Some tenant -> (
                  match row.immutable_user_id with
                  | None -> (
                      match row.requester_name with
                      | Some _ -> Error Display_name_only
                      | None -> Error Missing_user_id)
                  | Some user -> (
                      (* Web requires issuer-shaped tenant (OIDC issuer URL). *)
                      match connector with
                      | P.Web
                        when not
                               (String.contains tenant ':'
                              || String.contains tenant '/') ->
                          Error
                            (Non_adapter_connector
                               "web_without_issuer_namespace")
                      | P.Cli | P.Direct ->
                          Error
                            (Non_adapter_connector
                               (P.string_of_connector connector))
                      | P.Teams | P.Slack | P.Discord | P.Telegram | P.Web -> (
                          match
                            P.make_connector_actor_key ~connector
                              ~tenant_or_workspace:tenant
                              ~immutable_user_id:user
                          with
                          | Error e -> Error (Malformed_actor_key e)
                          | Ok key -> Ok key))))))

let classify_row ~db (row : legacy_row) =
  match classify_shape row with
  | Error reason -> Ok (Legacy_unresolved { reason })
  | Ok actor_key -> (
      match S.get_connector_actor ~db ~key:actor_key with
      | Error e -> Error e
      | Ok None -> Ok (Legacy_unresolved { reason = Actor_not_found })
      | Ok (Some actor) -> (
          match actor.lifecycle with
          | P.Disabled -> Ok (Legacy_unresolved { reason = Actor_disabled })
          | P.Unlinked | P.Active -> (
              let link_id_opt =
                match S.get_active_identity_link ~db ~key:actor_key with
                | Ok (Some link) -> Some link.id
                | _ -> None
              in
              let owner_pid =
                match S.get_active_identity_link ~db ~key:actor_key with
                | Ok (Some link) -> link.principal_id
                | _ -> actor.principal_id
              in
              match follow_merge_alias ~db ~seen:[] owner_pid with
              | Error e -> Error e
              | Ok (root_pid, followed) -> (
                  match S.get_principal ~db ~id:root_pid with
                  | Error e -> Error e
                  | Ok None ->
                      Ok
                        (Legacy_unresolved
                           {
                             reason =
                               Principal_not_active
                                 (P.principal_id_to_string root_pid);
                           })
                  | Ok (Some p) -> (
                      match p.lifecycle with
                      | P.Active ->
                          Ok
                            (Backfill
                               {
                                 actor_key;
                                 principal_id = root_pid;
                                 followed_merge_alias = followed;
                                 actor_revision = actor.revision;
                                 identity_link_id = link_id_opt;
                               })
                      | P.Disabled | P.Merged_into _ ->
                          Ok
                            (Legacy_unresolved
                               {
                                 reason =
                                   Principal_not_active
                                     (P.principal_id_to_string root_pid);
                               }))))))

type authority = {
  user_attributed_allowed : bool;
  app_behavior_allowed : bool;
  read_audit_allowed : bool;
}

let authority_of_classification = function
  | Backfill _ ->
      {
        user_attributed_allowed = true;
        app_behavior_allowed = true;
        read_audit_allowed = true;
      }
  | Legacy_unresolved _ ->
      {
        user_attributed_allowed = false;
        app_behavior_allowed = true;
        read_audit_allowed = true;
      }

(* -------------------------------------------------------------------------- *)
(* Migration records                                                          *)
(* -------------------------------------------------------------------------- *)

type migration_status = Backfilled | Unresolved | Job_invalidated

let string_of_migration_status = function
  | Backfilled -> "backfilled"
  | Unresolved -> "legacy_unresolved"
  | Job_invalidated -> "job_invalidated"

let migration_status_of_string = function
  | "backfilled" -> Ok Backfilled
  | "legacy_unresolved" -> Ok Unresolved
  | "job_invalidated" -> Ok Job_invalidated
  | other -> Error (Printf.sprintf "unknown migration_status: %s" other)

type migration_record = {
  id : string;
  run_id : string;
  row : legacy_row;
  classification : classification;
  status : migration_status;
  authority : authority;
  created_at : string;
}

type migrate_report = {
  run_id : string;
  backfilled : int;
  unresolved : int;
  jobs_invalidated : int;
  records : migration_record list;
  historical_snapshots_rewritten : int;
}

let generate_run_id ?(now = Unix.gettimeofday ()) () =
  let ts_ms = Int64.of_float (now *. 1000.) in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "plegmig_run_%Ld_%06d" ts_ms rand

let generate_record_id ?(now = Unix.gettimeofday ()) () =
  let ts_ms = Int64.of_float (now *. 1000.) in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "plegmig_rec_%Ld_%06d" ts_ms rand

let status_of ~job_active classification =
  match classification with
  | Backfill _ -> Backfilled
  | Legacy_unresolved _ -> if job_active then Job_invalidated else Unresolved

let insert_run ~db ~run_id ~started_at =
  let sql =
    {|INSERT INTO principal_legacy_migration_runs
      (run_id, schema_version, started_at, rolled_back)
      VALUES (?, ?, ?, 0)|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT run_id));
      ignore
        (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int schema_version)));
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT started_at));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok ()
      | rc ->
          Error
            (Printf.sprintf "insert migration run failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let finish_run ~db ~run_id ~finished_at ~backfilled ~unresolved
    ~jobs_invalidated =
  let sql =
    {|UPDATE principal_legacy_migration_runs
      SET finished_at = ?, backfilled = ?, unresolved = ?, jobs_invalidated = ?
      WHERE run_id = ?|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT finished_at));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int backfilled)));
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.INT (Int64.of_int unresolved)));
      ignore
        (Sqlite3.bind stmt 4 (Sqlite3.Data.INT (Int64.of_int jobs_invalidated)));
      ignore (Sqlite3.bind stmt 5 (Sqlite3.Data.TEXT run_id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok ()
      | rc ->
          Error
            (Printf.sprintf "finish migration run failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let get_existing_source ~db ~source_kind ~source_id =
  let sql =
    {|SELECT id FROM principal_legacy_migration_records
      WHERE source_kind = ? AND source_id = ? LIMIT 1|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1
           (Sqlite3.Data.TEXT (string_of_source_kind source_kind)));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT source_id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Ok (Some (text_col stmt 0))
      | Sqlite3.Rc.DONE -> Ok None
      | rc ->
          Error
            (Printf.sprintf "get_existing_source failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let insert_record ~db (r : migration_record) =
  let ( classification_kind,
        unresolved_reason,
        actor_key,
        principal_id,
        followed,
        actor_revision,
        identity_link_id ) =
    match r.classification with
    | Backfill b ->
        ( "backfill",
          None,
          Some (P.actor_identity_key b.actor_key),
          Some (P.principal_id_to_string b.principal_id),
          b.followed_merge_alias,
          Some b.actor_revision,
          b.identity_link_id )
    | Legacy_unresolved { reason } ->
        ( "legacy_unresolved",
          Some (string_of_unresolved_reason reason),
          None,
          None,
          false,
          None,
          None )
  in
  let sql =
    {|INSERT INTO principal_legacy_migration_records (
      id, run_id, source_kind, source_id, status, classification_kind,
      unresolved_reason, actor_key, principal_id, followed_merge_alias,
      actor_revision, identity_link_id, user_attributed_allowed,
      app_behavior_allowed, read_audit_allowed, evidence_json,
      connector, tenant_or_workspace, immutable_user_id, requester_name,
      room_id, session_id, origin_json, raw_requester, job_active, created_at
    ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let bind i v = ignore (Sqlite3.bind stmt i v) in
      let bind_opt i = function
        | None -> bind i Sqlite3.Data.NULL
        | Some s -> bind i (Sqlite3.Data.TEXT s)
      in
      let bind_opt_int i = function
        | None -> bind i Sqlite3.Data.NULL
        | Some n -> bind i (Sqlite3.Data.INT (Int64.of_int n))
      in
      bind 1 (Sqlite3.Data.TEXT r.id);
      bind 2 (Sqlite3.Data.TEXT r.run_id);
      bind 3 (Sqlite3.Data.TEXT (string_of_source_kind r.row.source_kind));
      bind 4 (Sqlite3.Data.TEXT r.row.source_id);
      bind 5 (Sqlite3.Data.TEXT (string_of_migration_status r.status));
      bind 6 (Sqlite3.Data.TEXT classification_kind);
      bind_opt 7 unresolved_reason;
      bind_opt 8 actor_key;
      bind_opt 9 principal_id;
      bind 10 (Sqlite3.Data.INT (if followed then 1L else 0L));
      bind_opt_int 11 actor_revision;
      bind_opt 12 identity_link_id;
      bind 13
        (Sqlite3.Data.INT
           (if r.authority.user_attributed_allowed then 1L else 0L));
      bind 14
        (Sqlite3.Data.INT (if r.authority.app_behavior_allowed then 1L else 0L));
      bind 15
        (Sqlite3.Data.INT (if r.authority.read_audit_allowed then 1L else 0L));
      bind 16 (Sqlite3.Data.TEXT r.row.evidence_json);
      bind_opt 17 r.row.connector;
      bind_opt 18 r.row.tenant_or_workspace;
      bind_opt 19 r.row.immutable_user_id;
      bind_opt 20 r.row.requester_name;
      bind_opt 21 r.row.room_id;
      bind_opt 22 r.row.session_id;
      bind_opt 23 r.row.origin_json;
      bind_opt 24 r.row.raw_requester;
      bind 25 (Sqlite3.Data.INT (if r.row.job_active then 1L else 0L));
      bind 26 (Sqlite3.Data.TEXT r.created_at);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok ()
      | rc ->
          Error
            (Printf.sprintf "insert migration record failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let insert_invalidation ~db ~run_id ~source_kind ~source_id ~reason ~created_at
    =
  let sql =
    {|INSERT OR REPLACE INTO principal_legacy_invalidated_jobs
      (source_kind, source_id, run_id, reason, created_at)
      VALUES (?, ?, ?, ?, ?)|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1
           (Sqlite3.Data.TEXT (string_of_source_kind source_kind)));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT source_id));
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT run_id));
      ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT reason));
      ignore (Sqlite3.bind stmt 5 (Sqlite3.Data.TEXT created_at));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok ()
      | rc ->
          Error
            (Printf.sprintf "insert invalidation failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let record_of_stmt stmt : (migration_record, string) result =
  let ( let* ) = Result.bind in
  let id = text_col stmt 0 in
  let run_id = text_col stmt 1 in
  let* source_kind = source_kind_of_string (text_col stmt 2) in
  let source_id = text_col stmt 3 in
  let* status = migration_status_of_string (text_col stmt 4) in
  let classification_kind = text_col stmt 5 in
  let unresolved_reason = opt_text_col stmt 6 in
  let actor_key_s = opt_text_col stmt 7 in
  let principal_id_s = opt_text_col stmt 8 in
  let followed = bool_col stmt 9 in
  let actor_revision =
    match Sqlite3.column stmt 10 with
    | Sqlite3.Data.INT n -> Some (Int64.to_int n)
    | Sqlite3.Data.TEXT s -> ( try Some (int_of_string s) with _ -> None)
    | _ -> None
  in
  let identity_link_id = opt_text_col stmt 11 in
  let user_ok = bool_col stmt 12 in
  let app_ok = bool_col stmt 13 in
  let read_ok = bool_col stmt 14 in
  let evidence_json = text_col stmt 15 in
  let connector = opt_text_col stmt 16 in
  let tenant = opt_text_col stmt 17 in
  let user = opt_text_col stmt 18 in
  let requester_name = opt_text_col stmt 19 in
  let room_id = opt_text_col stmt 20 in
  let session_id = opt_text_col stmt 21 in
  let origin_json = opt_text_col stmt 22 in
  let raw_requester = opt_text_col stmt 23 in
  let job_active = bool_col stmt 24 in
  let created_at = text_col stmt 25 in
  let row =
    {
      source_kind;
      source_id;
      connector;
      tenant_or_workspace = tenant;
      immutable_user_id = user;
      requester_name;
      room_id;
      session_id;
      origin_json;
      raw_requester;
      job_active;
      evidence_json;
    }
  in
  let* classification =
    match classification_kind with
    | "backfill" -> (
        match (actor_key_s, principal_id_s, actor_revision) with
        | Some ak, Some pid_s, Some rev ->
            let* actor_key =
              match (connector, tenant, user) with
              | Some c, Some t, Some u -> (
                  match P.connector_of_string c with
                  | Error e -> Error e
                  | Ok connector ->
                      P.make_connector_actor_key ~connector
                        ~tenant_or_workspace:t ~immutable_user_id:u)
              | _ ->
                  Error
                    (Printf.sprintf
                       "backfill record %s missing connector fields (key=%s)" id
                       ak)
            in
            let* principal_id = P.principal_id_of_string pid_s in
            Ok
              (Backfill
                 {
                   actor_key;
                   principal_id;
                   followed_merge_alias = followed;
                   actor_revision = rev;
                   identity_link_id;
                 })
        | _ -> Error (Printf.sprintf "incomplete backfill record %s" id))
    | "legacy_unresolved" -> (
        match unresolved_reason with
        | None ->
            Error (Printf.sprintf "unresolved record %s missing reason" id)
        | Some rs ->
            let* reason = unresolved_reason_of_string rs in
            Ok (Legacy_unresolved { reason }))
    | other -> Error (Printf.sprintf "unknown classification_kind: %s" other)
  in
  Ok
    {
      id;
      run_id;
      row;
      classification;
      status;
      authority =
        {
          user_attributed_allowed = user_ok;
          app_behavior_allowed = app_ok;
          read_audit_allowed = read_ok;
        };
      created_at;
    }

let select_record_columns =
  "id, run_id, source_kind, source_id, status, classification_kind, \
   unresolved_reason, actor_key, principal_id, followed_merge_alias, \
   actor_revision, identity_link_id, user_attributed_allowed, \
   app_behavior_allowed, read_audit_allowed, evidence_json, connector, \
   tenant_or_workspace, immutable_user_id, requester_name, room_id, \
   session_id, origin_json, raw_requester, job_active, created_at"

let get_record ~db ~source_kind ~source_id =
  ensure_schema db;
  let sql =
    Printf.sprintf
      "SELECT %s FROM principal_legacy_migration_records WHERE source_kind = ? \
       AND source_id = ? LIMIT 1"
      select_record_columns
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1
           (Sqlite3.Data.TEXT (string_of_source_kind source_kind)));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT source_id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match record_of_stmt stmt with
          | Ok r -> Ok (Some r)
          | Error e -> Error e)
      | Sqlite3.Rc.DONE -> Ok None
      | rc ->
          Error
            (Printf.sprintf "get_record failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let list_records_for_run ~db ~run_id =
  ensure_schema db;
  let sql =
    Printf.sprintf
      "SELECT %s FROM principal_legacy_migration_records WHERE run_id = ? \
       ORDER BY created_at ASC, id ASC"
      select_record_columns
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT run_id));
      let acc = ref [] in
      let rec loop () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> (
            match record_of_stmt stmt with
            | Error e -> Error e
            | Ok r ->
                acc := r :: !acc;
                loop ())
        | Sqlite3.Rc.DONE -> Ok (List.rev !acc)
        | rc ->
            Error
              (Printf.sprintf "list_records_for_run failed: %s (%s)"
                 (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))
      in
      loop ())

let is_job_invalidated ~db ~source_kind ~source_id =
  ensure_schema db;
  let sql =
    {|SELECT 1 FROM principal_legacy_invalidated_jobs
      WHERE source_kind = ? AND source_id = ? LIMIT 1|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1
           (Sqlite3.Data.TEXT (string_of_source_kind source_kind)));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT source_id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Ok true
      | Sqlite3.Rc.DONE -> Ok false
      | rc ->
          Error
            (Printf.sprintf "is_job_invalidated failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let user_authority_allowed ~db ~source_kind ~source_id =
  match get_record ~db ~source_kind ~source_id with
  | Error e -> Error e
  | Ok None -> Ok false
  | Ok (Some r) -> Ok r.authority.user_attributed_allowed

(* -------------------------------------------------------------------------- *)
(* Migrate                                                                    *)
(* -------------------------------------------------------------------------- *)

let migrate_rows ~db ~rows ?run_id ?(now = Unix.gettimeofday ()) () =
  let ( let* ) = Result.bind in
  ensure_schema db;
  S.ensure_schema db;
  let run_id =
    match run_id with
    | Some s when String.trim s <> "" -> String.trim s
    | _ -> generate_run_id ~now ()
  in
  let started_at = Time_util.iso8601_utc ~t:now () in
  (* Snapshot count before migration — must not change. *)
  let count_actor_snapshots () =
    if not (table_exists db "actor_snapshots") then 0
    else
      let stmt = Sqlite3.prepare db "SELECT COUNT(*) FROM actor_snapshots" in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          match Sqlite3.step stmt with
          | Sqlite3.Rc.ROW -> int_col stmt 0
          | _ -> 0)
  in
  let snap_before = count_actor_snapshots () in
  with_immediate_tx db (fun () ->
      let* () = insert_run ~db ~run_id ~started_at in
      let rec go acc_back acc_unres acc_inv acc_recs = function
        | [] -> Ok (acc_back, acc_unres, acc_inv, List.rev acc_recs)
        | row :: rest -> (
            match
              get_existing_source ~db ~source_kind:row.source_kind
                ~source_id:row.source_id
            with
            | Error e -> Error e
            | Ok (Some _) ->
                (* Already migrated under a prior unrolled-back run — skip. *)
                go acc_back acc_unres acc_inv acc_recs rest
            | Ok None -> (
                match classify_row ~db row with
                | Error e -> Error e
                | Ok classification ->
                    let authority =
                      authority_of_classification classification
                    in
                    let status =
                      status_of ~job_active:row.job_active classification
                    in
                    let rec_id = generate_record_id ~now () in
                    let created_at = Time_util.iso8601_utc ~t:now () in
                    let record =
                      {
                        id = rec_id;
                        run_id;
                        row;
                        classification;
                        status;
                        authority;
                        created_at;
                      }
                    in
                    let* () = insert_record ~db record in
                    let* acc_inv =
                      match status with
                      | Job_invalidated ->
                          let reason =
                            match classification with
                            | Legacy_unresolved { reason } ->
                                string_of_unresolved_reason reason
                            | Backfill _ -> "unexpected"
                          in
                          let* () =
                            insert_invalidation ~db ~run_id
                              ~source_kind:row.source_kind
                              ~source_id:row.source_id ~reason ~created_at
                          in
                          Ok (acc_inv + 1)
                      | Backfilled | Unresolved -> Ok acc_inv
                    in
                    let acc_back, acc_unres =
                      match classification with
                      | Backfill _ -> (acc_back + 1, acc_unres)
                      | Legacy_unresolved _ -> (acc_back, acc_unres + 1)
                    in
                    go acc_back acc_unres acc_inv (record :: acc_recs) rest))
      in
      let* backfilled, unresolved, jobs_invalidated, records =
        go 0 0 0 [] rows
      in
      let finished_at = Time_util.iso8601_utc ~t:now () in
      let* () =
        finish_run ~db ~run_id ~finished_at ~backfilled ~unresolved
          ~jobs_invalidated
      in
      let snap_after = count_actor_snapshots () in
      if snap_after <> snap_before then
        Error "migration invariant violated: actor_snapshots count changed"
      else
        Ok
          {
            run_id;
            backfilled;
            unresolved;
            jobs_invalidated;
            records;
            historical_snapshots_rewritten = 0;
          })
(* -------------------------------------------------------------------------- *)
(* Load legacy from DB                                                        *)
(* -------------------------------------------------------------------------- *)

let bg_task_active status =
  match String.lowercase_ascii (String.trim status) with
  | "queued" | "running" -> true
  | _ -> false

let workflow_active status =
  match String.lowercase_ascii (String.trim status) with
  | "pending" | "running" -> true
  | _ -> false

let load_background_tasks db =
  if not (table_exists db "background_tasks") then Ok []
  else
    let has_origin = table_exists db "background_tasks" in
    (* Columns may be partially migrated; probe with a broad SELECT. *)
    let sql =
      "SELECT id, status, requester, origin_json, session_key FROM \
       background_tasks"
    in
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        let acc = ref [] in
        let rec loop () =
          match Sqlite3.step stmt with
          | Sqlite3.Rc.ROW -> (
              let id = text_col stmt 0 in
              let status = text_col stmt 1 in
              let requester = opt_text_col stmt 2 in
              let origin_json = opt_text_col stmt 3 in
              let session_key = opt_text_col stmt 4 in
              let origin =
                match origin_json with
                | Some s -> Room_origin.of_json_string_opt s
                | None -> None
              in
              let base =
                match origin with
                | Some o ->
                    legacy_row_of_origin ~source_kind:Background_task
                      ~source_id:id ~job_active:(bg_task_active status)
                      ?raw_requester:requester ?session_id:session_key o
                | None ->
                    make_legacy_row ~source_kind:Background_task ~source_id:id
                      ?raw_requester:requester ?session_id:session_key
                      ?origin_json ~job_active:(bg_task_active status) ()
              in
              match base with
              | Error e -> Error e
              | Ok row ->
                  acc := row :: !acc;
                  loop ())
          | Sqlite3.Rc.DONE -> Ok (List.rev !acc)
          | rc ->
              Error
                (Printf.sprintf "load_background_tasks failed: %s (%s)"
                   (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))
        in
        let _ = has_origin in
        loop ())

let load_workflow_runs db =
  if not (table_exists db "workflow_runs") then Ok []
  else
    let sql = "SELECT id, status, room_id, requester_id FROM workflow_runs" in
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        let acc = ref [] in
        let rec loop () =
          match Sqlite3.step stmt with
          | Sqlite3.Rc.ROW -> (
              let id = text_col stmt 0 in
              let status = text_col stmt 1 in
              let room_id = opt_text_col stmt 2 in
              let requester_id = opt_text_col stmt 3 in
              match
                make_legacy_row ~source_kind:Workflow_run ~source_id:id
                  ?immutable_user_id:requester_id ?room_id
                  ~job_active:(workflow_active status) ()
              with
              | Error e -> Error e
              | Ok row ->
                  acc := row :: !acc;
                  loop ())
          | Sqlite3.Rc.DONE -> Ok (List.rev !acc)
          | rc ->
              Error
                (Printf.sprintf "load_workflow_runs failed: %s (%s)"
                   (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))
        in
        loop ())

let load_legacy_from_db ~db =
  ensure_schema db;
  match load_background_tasks db with
  | Error e -> Error e
  | Ok bg -> (
      match load_workflow_runs db with
      | Error e -> Error e
      | Ok wf -> Ok (bg @ wf))

let migrate_database ~db ?run_id ?now () =
  match load_legacy_from_db ~db with
  | Error e -> Error e
  | Ok rows -> migrate_rows ~db ~rows ?run_id ?now ()

(* -------------------------------------------------------------------------- *)
(* Rollback                                                                   *)
(* -------------------------------------------------------------------------- *)

let rollback_run ~db ~run_id =
  ensure_schema db;
  let run_id = String.trim run_id in
  if run_id = "" then Error "run_id must be non-empty"
  else
    with_immediate_tx db (fun () ->
        let count_sql =
          "SELECT COUNT(*) FROM principal_legacy_migration_records WHERE \
           run_id = ?"
        in
        let count =
          let stmt = Sqlite3.prepare db count_sql in
          Fun.protect
            ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
            (fun () ->
              ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT run_id));
              match Sqlite3.step stmt with
              | Sqlite3.Rc.ROW -> int_col stmt 0
              | _ -> 0)
        in
        let del_inv =
          "DELETE FROM principal_legacy_invalidated_jobs WHERE run_id = ?"
        in
        let del_rec =
          "DELETE FROM principal_legacy_migration_records WHERE run_id = ?"
        in
        let mark_run =
          "UPDATE principal_legacy_migration_runs SET rolled_back = 1 WHERE \
           run_id = ?"
        in
        let run_del sql =
          let stmt = Sqlite3.prepare db sql in
          Fun.protect
            ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
            (fun () ->
              ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT run_id));
              match Sqlite3.step stmt with
              | Sqlite3.Rc.DONE -> Ok ()
              | rc ->
                  Error
                    (Printf.sprintf "rollback step failed: %s (%s)"
                       (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))
        in
        match run_del del_inv with
        | Error e -> Error e
        | Ok () -> (
            match run_del del_rec with
            | Error e -> Error e
            | Ok () -> (
                match run_del mark_run with
                | Error e -> Error e
                | Ok () -> Ok count)))

(* -------------------------------------------------------------------------- *)
(* Fixtures                                                                   *)
(* -------------------------------------------------------------------------- *)

type fixture_case = {
  name : string;
  rows : legacy_row list;
  seed : db:Sqlite3.db -> unit;
  expect_backfilled : int;
  expect_unresolved : int;
  expect_jobs_invalidated : int;
}

let assert_ok_row = function
  | Ok r -> r
  | Error e -> failwith ("fixture row: " ^ e)

let seed_principal_actor ~db ~principal_id ~connector ~tenant ~user
    ?(link_id = "idlink_fix") ?(created_at = "2026-01-01T00:00:00Z") () =
  let pid =
    match P.principal_id_of_string principal_id with
    | Ok p -> p
    | Error e -> failwith e
  in
  let key =
    match
      P.make_connector_actor_key ~connector ~tenant_or_workspace:tenant
        ~immutable_user_id:user
    with
    | Ok k -> k
    | Error e -> failwith e
  in
  let p = P.make_principal ~id:pid ~created_at ~updated_at:created_at () in
  ignore (S.insert_principal ~db p);
  let actor =
    P.make_connector_actor ~key ~principal_id:pid ~verified_at:created_at
      ~created_at ~updated_at:created_at ()
  in
  ignore (S.insert_connector_actor ~db actor);
  let link =
    P.make_identity_link ~id:link_id ~principal_id:pid ~actor_key:key
      ~linked_at:created_at ()
  in
  ignore (S.insert_identity_link ~db link);
  (pid, key)

let upgrade_fixture_cases () =
  let row_unambiguous =
    assert_ok_row
      (make_legacy_row ~source_kind:Fixture ~source_id:"fx_safe_1"
         ~connector:"teams" ~tenant_or_workspace:"tenant-acme"
         ~immutable_user_id:"aad-42" ~room_id:"room-1" ())
  in
  let row_display_only =
    assert_ok_row
      (make_legacy_row ~source_kind:Fixture ~source_id:"fx_display_1"
         ~connector:"teams" ~tenant_or_workspace:"tenant-acme"
         ~requester_name:"Ada Lovelace" ~room_id:"room-1" ())
  in
  let row_missing_ns =
    assert_ok_row
      (make_legacy_row ~source_kind:Fixture ~source_id:"fx_no_ns"
         ~connector:"slack" ~immutable_user_id:"U123" ())
  in
  let row_active_ambiguous =
    assert_ok_row
      (make_legacy_row ~source_kind:Fixture ~source_id:"fx_job_ambig"
         ~connector:"teams" ~tenant_or_workspace:"tenant-acme"
         ~requester_name:"Someone" ~job_active:true ())
  in
  let row_cli =
    assert_ok_row
      (make_legacy_row ~source_kind:Fixture ~source_id:"fx_cli" ~connector:"cli"
         ~tenant_or_workspace:"local" ~immutable_user_id:"device-1" ())
  in
  let row_same_display_a =
    assert_ok_row
      (make_legacy_row ~source_kind:Fixture ~source_id:"fx_coalesce_a"
         ~connector:"slack" ~tenant_or_workspace:"T-WORK"
         ~immutable_user_id:"U-AAA" ~requester_name:"Shared Name" ())
  in
  let row_same_display_b =
    assert_ok_row
      (make_legacy_row ~source_kind:Fixture ~source_id:"fx_coalesce_b"
         ~connector:"slack" ~tenant_or_workspace:"T-WORK"
         ~immutable_user_id:"U-BBB" ~requester_name:"Shared Name" ())
  in
  let row_merged =
    assert_ok_row
      (make_legacy_row ~source_kind:Fixture ~source_id:"fx_merged"
         ~connector:"discord" ~tenant_or_workspace:"guild-1"
         ~immutable_user_id:"disc-9" ())
  in
  [
    {
      name = "unambiguous_backfill";
      rows = [ row_unambiguous ];
      seed =
        (fun ~db ->
          S.ensure_schema db;
          ignore
            (seed_principal_actor ~db ~principal_id:"prin_safe"
               ~connector:P.Teams ~tenant:"tenant-acme" ~user:"aad-42" ()));
      expect_backfilled = 1;
      expect_unresolved = 0;
      expect_jobs_invalidated = 0;
    };
    {
      name = "display_name_only_unresolved";
      rows = [ row_display_only ];
      seed = (fun ~db -> S.ensure_schema db);
      expect_backfilled = 0;
      expect_unresolved = 1;
      expect_jobs_invalidated = 0;
    };
    {
      name = "missing_namespace_unresolved";
      rows = [ row_missing_ns ];
      seed = (fun ~db -> S.ensure_schema db);
      expect_backfilled = 0;
      expect_unresolved = 1;
      expect_jobs_invalidated = 0;
    };
    {
      name = "active_ambiguous_job_invalidated";
      rows = [ row_active_ambiguous ];
      seed = (fun ~db -> S.ensure_schema db);
      expect_backfilled = 0;
      expect_unresolved = 1;
      expect_jobs_invalidated = 1;
    };
    {
      name = "cli_non_adapter_unresolved";
      rows = [ row_cli ];
      seed = (fun ~db -> S.ensure_schema db);
      expect_backfilled = 0;
      expect_unresolved = 1;
      expect_jobs_invalidated = 0;
    };
    {
      name = "no_coalesce_on_display_name";
      rows = [ row_same_display_a; row_same_display_b ];
      seed =
        (fun ~db ->
          S.ensure_schema db;
          ignore
            (seed_principal_actor ~db ~principal_id:"prin_a" ~connector:P.Slack
               ~tenant:"T-WORK" ~user:"U-AAA" ~link_id:"link_a" ());
          ignore
            (seed_principal_actor ~db ~principal_id:"prin_b" ~connector:P.Slack
               ~tenant:"T-WORK" ~user:"U-BBB" ~link_id:"link_b" ()));
      expect_backfilled = 2;
      expect_unresolved = 0;
      expect_jobs_invalidated = 0;
    };
    {
      name = "follow_merge_tombstone_without_rewrite";
      rows = [ row_merged ];
      seed =
        (fun ~db ->
          S.ensure_schema db;
          M.ensure_schema db;
          let survivor =
            match P.principal_id_of_string "prin_survivor" with
            | Ok p -> p
            | Error e -> failwith e
          in
          let loser =
            match P.principal_id_of_string "prin_loser" with
            | Ok p -> p
            | Error e -> failwith e
          in
          let p_s =
            P.make_principal ~id:survivor ~created_at:"2026-01-01T00:00:00Z"
              ~updated_at:"2026-01-01T00:00:00Z" ()
          in
          let p_l =
            P.make_principal ~id:loser ~lifecycle:(P.Merged_into survivor)
              ~created_at:"2026-01-02T00:00:00Z"
              ~updated_at:"2026-02-01T00:00:00Z" ()
          in
          ignore (S.insert_principal ~db p_s);
          ignore (S.insert_principal ~db p_l);
          let key =
            match
              P.make_connector_actor_key ~connector:P.Discord
                ~tenant_or_workspace:"guild-1" ~immutable_user_id:"disc-9"
            with
            | Ok k -> k
            | Error e -> failwith e
          in
          (* Live rows still name the loser; migration follows Merged_into to
             the survivor without rewriting historical snapshots. *)
          let actor =
            P.make_connector_actor ~key ~principal_id:loser
              ~verified_at:"2026-01-03T00:00:00Z"
              ~created_at:"2026-01-03T00:00:00Z"
              ~updated_at:"2026-02-01T00:00:00Z" ()
          in
          ignore (S.insert_connector_actor ~db actor);
          let link =
            P.make_identity_link ~id:"link_merged" ~principal_id:loser
              ~actor_key:key ~linked_at:"2026-02-01T00:00:00Z" ()
          in
          ignore (S.insert_identity_link ~db link);
          (* Historical snapshot retains pre-merge principal evidence. *)
          let snap : Principal_merge_persist.actor_snapshot =
            {
              id = "hist_snap_merged_1";
              actor_key = P.actor_identity_key key;
              principal_id_at_snapshot = loser;
              actor_json =
                Yojson.Safe.to_string
                  (`Assoc
                     [
                       ("principal_id", `String "prin_loser");
                       ("note", `String "pre_merge");
                     ]);
              reason = "pre_merge";
              merge_id = Some "merge_test_1";
              created_at = "2026-02-01T00:00:00Z";
            }
          in
          ignore (Principal_merge_persist.insert_actor_snapshot ~db snap));
      expect_backfilled = 1;
      expect_unresolved = 0;
      expect_jobs_invalidated = 0;
    };
    {
      name = "actor_not_found_unresolved";
      rows =
        [
          assert_ok_row
            (make_legacy_row ~source_kind:Fixture ~source_id:"fx_no_actor"
               ~connector:"telegram" ~tenant_or_workspace:"bot-1"
               ~immutable_user_id:"tg-99" ());
        ];
      seed = (fun ~db -> S.ensure_schema db);
      expect_backfilled = 0;
      expect_unresolved = 1;
      expect_jobs_invalidated = 0;
    };
  ]

let run_upgrade_fixture (fx : fixture_case) =
  let db = Sqlite3.db_open ":memory:" in
  try
    ensure_schema db;
    S.ensure_schema db;
    fx.seed ~db;
    match migrate_rows ~db ~rows:fx.rows ~now:1_785_300_000.0 () with
    | Error e ->
        ignore (Sqlite3.db_close db);
        Error e
    | Ok report ->
        if report.backfilled <> fx.expect_backfilled then (
          ignore (Sqlite3.db_close db);
          Error
            (Printf.sprintf "%s: backfilled got %d want %d" fx.name
               report.backfilled fx.expect_backfilled))
        else if report.unresolved <> fx.expect_unresolved then (
          ignore (Sqlite3.db_close db);
          Error
            (Printf.sprintf "%s: unresolved got %d want %d" fx.name
               report.unresolved fx.expect_unresolved))
        else if report.jobs_invalidated <> fx.expect_jobs_invalidated then (
          ignore (Sqlite3.db_close db);
          Error
            (Printf.sprintf "%s: jobs_invalidated got %d want %d" fx.name
               report.jobs_invalidated fx.expect_jobs_invalidated))
        else if report.historical_snapshots_rewritten <> 0 then (
          ignore (Sqlite3.db_close db);
          Error
            (Printf.sprintf "%s: snapshots rewritten = %d" fx.name
               report.historical_snapshots_rewritten))
        else Ok (report, db)
  with exn ->
    ignore (Sqlite3.db_close db);
    Error (Printexc.to_string exn)

let count_snapshots db =
  if not (table_exists db "actor_snapshots") then 0
  else
    let stmt = Sqlite3.prepare db "SELECT COUNT(*) FROM actor_snapshots" in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        match Sqlite3.step stmt with Sqlite3.Rc.ROW -> int_col stmt 0 | _ -> 0)

let get_snapshot_json db id =
  if not (table_exists db "actor_snapshots") then None
  else
    let stmt =
      Sqlite3.prepare db
        "SELECT actor_json FROM actor_snapshots WHERE id = ? LIMIT 1"
    in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> Some (text_col stmt 0)
        | _ -> None)

let check_authority_records fx_name (records : migration_record list) =
  let rec loop = function
    | [] -> Ok ()
    | (r : migration_record) :: rest -> (
        match r.classification with
        | Backfill _ ->
            if not r.authority.user_attributed_allowed then
              Error (fx_name ^ ": backfill must allow user authority")
            else if not r.authority.app_behavior_allowed then
              Error (fx_name ^ ": app must remain allowed")
            else if not r.authority.read_audit_allowed then
              Error (fx_name ^ ": read/audit must remain allowed")
            else loop rest
        | Legacy_unresolved _ ->
            if r.authority.user_attributed_allowed then
              Error
                (fx_name ^ ": unresolved must deny user-attributed authority")
            else if not r.authority.app_behavior_allowed then
              Error (fx_name ^ ": unresolved must allow App behavior")
            else if not r.authority.read_audit_allowed then
              Error (fx_name ^ ": unresolved must allow read/audit")
            else loop rest)
  in
  loop records

let prove_upgrade_and_rollback (fx : fixture_case) =
  let ( let* ) = Result.bind in
  match run_upgrade_fixture fx with
  | Error e -> Error e
  | Ok (report, db) ->
      let close () = ignore (Sqlite3.db_close db) in
      let body () =
        let snap_count = count_snapshots db in
        let hist_json = get_snapshot_json db "hist_snap_merged_1" in
        let* () = check_authority_records fx.name report.records in
        let* removed = rollback_run ~db ~run_id:report.run_id in
        if removed <> List.length report.records then
          Error
            (Printf.sprintf "%s: rollback removed %d want %d" fx.name removed
               (List.length report.records))
        else
          let* remaining = list_records_for_run ~db ~run_id:report.run_id in
          if remaining <> [] then
            Error (fx.name ^ ": records remain after rollback")
          else if count_snapshots db <> snap_count then
            Error (fx.name ^ ": snapshot count changed on rollback")
          else if hist_json <> get_snapshot_json db "hist_snap_merged_1" then
            Error (fx.name ^ ": historical snapshot content rewritten")
          else
            let* report2 =
              migrate_rows ~db ~rows:fx.rows ~now:1_785_300_001.0 ()
            in
            if report2.backfilled <> fx.expect_backfilled then
              Error
                (Printf.sprintf "%s: re-upgrade backfilled %d want %d" fx.name
                   report2.backfilled fx.expect_backfilled)
            else if report2.unresolved <> fx.expect_unresolved then
              Error
                (Printf.sprintf "%s: re-upgrade unresolved %d want %d" fx.name
                   report2.unresolved fx.expect_unresolved)
            else if report2.historical_snapshots_rewritten <> 0 then
              Error (fx.name ^ ": re-upgrade rewrote historical snapshots")
            else Ok ()
      in
      let result = try body () with exn -> Error (Printexc.to_string exn) in
      close ();
      result
