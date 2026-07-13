(* Principal-owned GitHub account bindings (P21.M1.E2.T001).
   See github_account_binding.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module P = Principal_identity
module S = Principal_identity_store

let schema_version = 1
let default_host = "github.com"

(* -------------------------------------------------------------------------- *)
(* Authorization status                                                       *)
(* -------------------------------------------------------------------------- *)

type authorization_status =
  | Pending
  | Authorized
  | Disabled
  | Revoked
  | Unlinked

let string_of_authorization_status = function
  | Pending -> "pending"
  | Authorized -> "authorized"
  | Disabled -> "disabled"
  | Revoked -> "revoked"
  | Unlinked -> "unlinked"

let authorization_status_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "pending" -> Ok Pending
  | "authorized" -> Ok Authorized
  | "disabled" -> Ok Disabled
  | "revoked" -> Ok Revoked
  | "unlinked" -> Ok Unlinked
  | s -> Error (Printf.sprintf "unknown authorization_status: %s" s)

(* -------------------------------------------------------------------------- *)
(* Vault ref / identity / display                                             *)
(* -------------------------------------------------------------------------- *)

type vault_ref = string

let make_vault_ref s =
  let t = String.trim s in
  if t = "" then Error "vault_ref must be non-empty" else Ok t

let vault_ref_to_string r = r

type account_identity = { host : string; app_id : int; github_user_id : int64 }

let make_account_identity ?(host = default_host) ~app_id ~github_user_id () =
  let host = String.trim host in
  if host = "" then Error "host must be non-empty"
  else if app_id <= 0 then Error "app_id must be positive"
  else if github_user_id <= 0L then Error "github_user_id must be positive"
  else Ok { host; app_id; github_user_id }

let account_identity_key (i : account_identity) =
  Printf.sprintf "host:%s:app:%d:user:%Ld" i.host i.app_id i.github_user_id

let uniqueness_domain (i : account_identity) =
  Printf.sprintf "%s:app:%d" i.host i.app_id

let account_identity_equal a b =
  String.equal a.host b.host && a.app_id = b.app_id
  && Int64.equal a.github_user_id b.github_user_id

type display = { login : string option; avatar_url : string option }

let empty_display = { login = None; avatar_url = None }

(* -------------------------------------------------------------------------- *)
(* Binding                                                                    *)
(* -------------------------------------------------------------------------- *)

type binding = {
  version : int;
  id : string;
  principal_id : P.principal_id;
  identity : account_identity;
  display : display;
  authorization_status : authorization_status;
  revision : int;
  lineage_id : string;
  vault_ref : vault_ref option;
  created_at : string;
  updated_at : string;
}

let make_binding ~id ~principal_id ~identity ?(display = empty_display)
    ?(authorization_status = Pending) ?(revision = 1) ?(lineage_id = "")
    ?vault_ref ?(created_at = "") ?(updated_at = "") () =
  let lineage_id =
    let t = String.trim lineage_id in
    if t = "" then id else t
  in
  {
    version = schema_version;
    id;
    principal_id;
    identity;
    display;
    authorization_status;
    revision;
    lineage_id;
    vault_ref;
    created_at;
    updated_at;
  }

type binding_snapshot = {
  id : string;
  binding_id : string;
  principal_id_at_snapshot : P.principal_id;
  lineage_id : string;
  binding_json : string;
  reason : string;
  related_id : string option;
  created_at : string;
}

(* -------------------------------------------------------------------------- *)
(* JSON                                                                       *)
(* -------------------------------------------------------------------------- *)

let opt_string_json = function None -> `Null | Some s -> `String s

let display_to_json (d : display) =
  `Assoc
    [
      ("login", opt_string_json d.login);
      ("avatar_url", opt_string_json d.avatar_url);
    ]

let display_of_json = function
  | `Assoc fields ->
      let login =
        match List.assoc_opt "login" fields with
        | None | Some `Null -> None
        | Some (`String s) -> Some s
        | _ -> None
      in
      let avatar_url =
        match List.assoc_opt "avatar_url" fields with
        | None | Some `Null -> None
        | Some (`String s) -> Some s
        | _ -> None
      in
      Ok { login; avatar_url }
  | _ -> Error "display must be an object"

let identity_to_json (i : account_identity) =
  `Assoc
    [
      ("host", `String i.host);
      ("app_id", `Int i.app_id);
      ("github_user_id", `Intlit (Int64.to_string i.github_user_id));
    ]

let json_int64 = function
  | `Int n -> Ok (Int64.of_int n)
  | `Intlit s -> ( try Ok (Int64.of_string s) with _ -> Error "invalid int64")
  | _ -> Error "expected int64"

let json_int = function
  | `Int n -> Ok n
  | `Intlit s -> ( try Ok (int_of_string s) with _ -> Error "invalid int")
  | _ -> Error "expected int"

let identity_of_json = function
  | `Assoc fields -> (
      match
        ( List.assoc_opt "host" fields,
          List.assoc_opt "app_id" fields,
          List.assoc_opt "github_user_id" fields )
      with
      | Some (`String host), Some app_j, Some uid_j -> (
          match (json_int app_j, json_int64 uid_j) with
          | Ok app_id, Ok github_user_id ->
              make_account_identity ~host ~app_id ~github_user_id ()
          | Error e, _ | _, Error e -> Error e)
      | _ -> Error "identity requires host, app_id, github_user_id")
  | _ -> Error "identity must be an object"

let binding_to_json (b : binding) =
  `Assoc
    [
      ("version", `Int b.version);
      ("id", `String b.id);
      ("principal_id", `String (P.principal_id_to_string b.principal_id));
      ("identity", identity_to_json b.identity);
      ("display", display_to_json b.display);
      ( "authorization_status",
        `String (string_of_authorization_status b.authorization_status) );
      ("revision", `Int b.revision);
      ("lineage_id", `String b.lineage_id);
      ("vault_ref", match b.vault_ref with None -> `Null | Some r -> `String r);
      ("created_at", `String b.created_at);
      ("updated_at", `String b.updated_at);
    ]

let binding_of_json = function
  | `Assoc fields -> (
      let req name =
        match List.assoc_opt name fields with
        | Some v -> Ok v
        | None -> Error (Printf.sprintf "missing field %s" name)
      in
      match
        ( req "id",
          req "principal_id",
          req "identity",
          req "authorization_status",
          req "revision",
          req "lineage_id" )
      with
      | ( Ok (`String id),
          Ok (`String pid_s),
          Ok identity_j,
          Ok (`String status_s),
          Ok rev_j,
          Ok (`String lineage_id) ) -> (
          match
            ( P.principal_id_of_string pid_s,
              identity_of_json identity_j,
              authorization_status_of_string status_s,
              json_int rev_j )
          with
          | Ok principal_id, Ok identity, Ok authorization_status, Ok revision
            -> (
              let version =
                match List.assoc_opt "version" fields with
                | Some j -> (
                    match json_int j with
                    | Ok n -> n
                    | Error _ -> schema_version)
                | None -> schema_version
              in
              let display =
                match List.assoc_opt "display" fields with
                | None -> Ok empty_display
                | Some j -> display_of_json j
              in
              let vault_ref =
                match List.assoc_opt "vault_ref" fields with
                | None | Some `Null -> Ok None
                | Some (`String s) -> (
                    match make_vault_ref s with
                    | Ok r -> Ok (Some r)
                    | Error e -> Error e)
                | _ -> Error "vault_ref must be string or null"
              in
              let created_at =
                match List.assoc_opt "created_at" fields with
                | Some (`String s) -> s
                | _ -> ""
              in
              let updated_at =
                match List.assoc_opt "updated_at" fields with
                | Some (`String s) -> s
                | _ -> ""
              in
              match (display, vault_ref) with
              | Ok display, Ok vault_ref ->
                  Ok
                    {
                      version;
                      id;
                      principal_id;
                      identity;
                      display;
                      authorization_status;
                      revision;
                      lineage_id;
                      vault_ref;
                      created_at;
                      updated_at;
                    }
              | Error e, _ | _, Error e -> Error e)
          | Error e, _, _, _
          | _, Error e, _, _
          | _, _, Error e, _
          | _, _, _, Error e ->
              Error e)
      | Error e, _, _, _, _, _
      | _, Error e, _, _, _, _
      | _, _, Error e, _, _, _
      | _, _, _, Error e, _, _
      | _, _, _, _, Error e, _
      | _, _, _, _, _, Error e ->
          Error e
      | _ -> Error "binding json type mismatch")
  | _ -> Error "binding must be an object"

let binding_snapshot_to_json (s : binding_snapshot) =
  `Assoc
    [
      ("id", `String s.id);
      ("binding_id", `String s.binding_id);
      ( "principal_id_at_snapshot",
        `String (P.principal_id_to_string s.principal_id_at_snapshot) );
      ("lineage_id", `String s.lineage_id);
      ("binding_json", `String s.binding_json);
      ("reason", `String s.reason);
      ( "related_id",
        match s.related_id with None -> `Null | Some r -> `String r );
      ("created_at", `String s.created_at);
    ]

(* -------------------------------------------------------------------------- *)
(* SQLite helpers                                                             *)
(* -------------------------------------------------------------------------- *)

let exec_schema db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "github_account_binding schema error: %s (sql: %s)"
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

let int64_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.INT n -> n
  | Sqlite3.Data.TEXT s -> ( try Int64.of_string s with _ -> 0L)
  | _ -> 0L

let with_immediate_tx db f =
  let mode =
    match Sqlite3.exec db "BEGIN IMMEDIATE" with
    | Sqlite3.Rc.OK -> `Outer
    | _ -> (
        match Sqlite3.exec db "SAVEPOINT github_account_binding" with
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
              Sqlite3.exec db "RELEASE SAVEPOINT github_account_binding"
            with
            | Sqlite3.Rc.OK -> Ok ()
            | rc ->
                ignore
                  (Sqlite3.exec db
                     "ROLLBACK TO SAVEPOINT github_account_binding");
                Error
                  (Printf.sprintf "RELEASE SAVEPOINT failed: %s"
                     (Sqlite3.Rc.to_string rc)))
      in
      let rollback () =
        match kind with
        | `Outer -> ignore (Sqlite3.exec db "ROLLBACK")
        | `Savepoint ->
            ignore
              (Sqlite3.exec db "ROLLBACK TO SAVEPOINT github_account_binding");
            ignore (Sqlite3.exec db "RELEASE SAVEPOINT github_account_binding")
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
          (Printf.sprintf "github_account_binding transaction aborted: %s"
             (Printexc.to_string exn)))

let generate_binding_id ?(now = Unix.gettimeofday ()) () =
  let ts = int_of_float now in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "ghbind_%d_%06d" ts rand

let generate_snapshot_id ?(now = Unix.gettimeofday ()) () =
  let ts = int_of_float now in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "ghbsnap_%d_%06d" ts rand

let ensure_timestamps ~now created_at updated_at =
  let now_s = Time_util.iso8601_utc ~t:now () in
  let created = if String.trim created_at = "" then now_s else created_at in
  let updated = if String.trim updated_at = "" then now_s else updated_at in
  (created, updated)

(* -------------------------------------------------------------------------- *)
(* Schema                                                                     *)
(* -------------------------------------------------------------------------- *)

let ensure_schema db =
  S.ensure_schema db;
  let bindings =
    {|CREATE TABLE IF NOT EXISTS github_account_bindings (
      id TEXT PRIMARY KEY NOT NULL,
      version INTEGER NOT NULL,
      principal_id TEXT NOT NULL,
      host TEXT NOT NULL,
      app_id INTEGER NOT NULL,
      github_user_id INTEGER NOT NULL,
      login TEXT,
      avatar_url TEXT,
      authorization_status TEXT NOT NULL,
      revision INTEGER NOT NULL,
      lineage_id TEXT NOT NULL,
      vault_ref TEXT,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    )|}
  in
  (* One active owner per immutable GitHub identity. *)
  let uniq_identity =
    {|CREATE UNIQUE INDEX IF NOT EXISTS idx_github_account_bindings_identity
      ON github_account_bindings(host, app_id, github_user_id)|}
  in
  let idx_principal =
    {|CREATE INDEX IF NOT EXISTS idx_github_account_bindings_principal
      ON github_account_bindings(principal_id)|}
  in
  let idx_lineage =
    {|CREATE INDEX IF NOT EXISTS idx_github_account_bindings_lineage
      ON github_account_bindings(lineage_id)|}
  in
  let snapshots =
    {|CREATE TABLE IF NOT EXISTS github_account_binding_snapshots (
      id TEXT PRIMARY KEY NOT NULL,
      binding_id TEXT NOT NULL,
      principal_id_at_snapshot TEXT NOT NULL,
      lineage_id TEXT NOT NULL,
      binding_json TEXT NOT NULL,
      reason TEXT NOT NULL,
      related_id TEXT,
      created_at TEXT NOT NULL
    )|}
  in
  let idx_snap_binding =
    {|CREATE INDEX IF NOT EXISTS idx_github_account_binding_snapshots_binding
      ON github_account_binding_snapshots(binding_id)|}
  in
  List.iter (exec_schema db)
    [
      bindings;
      uniq_identity;
      idx_principal;
      idx_lineage;
      snapshots;
      idx_snap_binding;
    ]

(* -------------------------------------------------------------------------- *)
(* Row codecs                                                                 *)
(* -------------------------------------------------------------------------- *)

let binding_of_stmt stmt : (binding, string) result =
  let id = text_col stmt 0 in
  let version = int_col stmt 1 in
  let principal_s = text_col stmt 2 in
  let host = text_col stmt 3 in
  let app_id = int_col stmt 4 in
  let github_user_id = int64_col stmt 5 in
  let login = opt_text_col stmt 6 in
  let avatar_url = opt_text_col stmt 7 in
  let status_s = text_col stmt 8 in
  let revision = int_col stmt 9 in
  let lineage_id = text_col stmt 10 in
  let vault_ref = opt_text_col stmt 11 in
  let created_at = text_col stmt 12 in
  let updated_at = text_col stmt 13 in
  match
    ( P.principal_id_of_string principal_s,
      make_account_identity ~host ~app_id ~github_user_id (),
      authorization_status_of_string status_s )
  with
  | Ok principal_id, Ok identity, Ok authorization_status -> (
      let vault_ref =
        match vault_ref with
        | None -> Ok None
        | Some s -> (
            match make_vault_ref s with
            | Ok r -> Ok (Some r)
            | Error e -> Error e)
      in
      match vault_ref with
      | Error e -> Error e
      | Ok vault_ref ->
          Ok
            {
              version;
              id;
              principal_id;
              identity;
              display = { login; avatar_url };
              authorization_status;
              revision;
              lineage_id;
              vault_ref;
              created_at;
              updated_at;
            })
  | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e

let select_binding_cols =
  {|SELECT id, version, principal_id, host, app_id, github_user_id,
           login, avatar_url, authorization_status, revision, lineage_id,
           vault_ref, created_at, updated_at
    FROM github_account_bindings|}

let snapshot_of_stmt stmt : (binding_snapshot, string) result =
  let id = text_col stmt 0 in
  let binding_id = text_col stmt 1 in
  let principal_s = text_col stmt 2 in
  let lineage_id = text_col stmt 3 in
  let binding_json = text_col stmt 4 in
  let reason = text_col stmt 5 in
  let related_id = opt_text_col stmt 6 in
  let created_at = text_col stmt 7 in
  match P.principal_id_of_string principal_s with
  | Error e -> Error e
  | Ok principal_id_at_snapshot ->
      Ok
        {
          id;
          binding_id;
          principal_id_at_snapshot;
          lineage_id;
          binding_json;
          reason;
          related_id;
          created_at;
        }

(* -------------------------------------------------------------------------- *)
(* Insert / get / list / delete                                               *)
(* -------------------------------------------------------------------------- *)

let insert ~db ?(now = Unix.gettimeofday ()) (b : binding) =
  let id =
    let t = String.trim b.id in
    if t = "" then generate_binding_id ~now () else t
  in
  let lineage_id =
    let t = String.trim b.lineage_id in
    if t = "" then id else t
  in
  let created_at, updated_at =
    ensure_timestamps ~now b.created_at b.updated_at
  in
  let sql =
    {|INSERT INTO github_account_bindings
        (id, version, principal_id, host, app_id, github_user_id,
         login, avatar_url, authorization_status, revision, lineage_id,
         vault_ref, created_at, updated_at)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)|}
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int b.version)));
  ignore
    (Sqlite3.bind stmt 3
       (Sqlite3.Data.TEXT (P.principal_id_to_string b.principal_id)));
  ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT b.identity.host));
  ignore
    (Sqlite3.bind stmt 5 (Sqlite3.Data.INT (Int64.of_int b.identity.app_id)));
  ignore (Sqlite3.bind stmt 6 (Sqlite3.Data.INT b.identity.github_user_id));
  (match b.display.login with
  | None -> ignore (Sqlite3.bind stmt 7 Sqlite3.Data.NULL)
  | Some s -> ignore (Sqlite3.bind stmt 7 (Sqlite3.Data.TEXT s)));
  (match b.display.avatar_url with
  | None -> ignore (Sqlite3.bind stmt 8 Sqlite3.Data.NULL)
  | Some s -> ignore (Sqlite3.bind stmt 8 (Sqlite3.Data.TEXT s)));
  ignore
    (Sqlite3.bind stmt 9
       (Sqlite3.Data.TEXT
          (string_of_authorization_status b.authorization_status)));
  ignore (Sqlite3.bind stmt 10 (Sqlite3.Data.INT (Int64.of_int b.revision)));
  ignore (Sqlite3.bind stmt 11 (Sqlite3.Data.TEXT lineage_id));
  (match b.vault_ref with
  | None -> ignore (Sqlite3.bind stmt 12 Sqlite3.Data.NULL)
  | Some r -> ignore (Sqlite3.bind stmt 12 (Sqlite3.Data.TEXT r)));
  ignore (Sqlite3.bind stmt 13 (Sqlite3.Data.TEXT created_at));
  ignore (Sqlite3.bind stmt 14 (Sqlite3.Data.TEXT updated_at));
  match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE ->
      ignore (Sqlite3.finalize stmt);
      Ok
        {
          b with
          id;
          lineage_id;
          created_at;
          updated_at;
          version = schema_version;
        }
  | rc ->
      let err = Sqlite3.errmsg db in
      ignore (Sqlite3.finalize stmt);
      let lower = String.lowercase_ascii err in
      if
        String.length lower > 0
        && String.contains lower 'u'
        &&
        let contains sub =
          let n = String.length sub in
          let m = String.length lower in
          let rec loop i =
            if i + n > m then false
            else if String.sub lower i n = sub then true
            else loop (i + 1)
          in
          loop 0
        in
        contains "unique"
      then
        Error
          (Printf.sprintf
             "github account identity already bound: %s (host/app/user must be \
              unique)"
             (account_identity_key b.identity))
      else
        Error
          (Printf.sprintf "insert github_account_binding failed: %s (%s)"
             (Sqlite3.Rc.to_string rc) err)

let get ~db ~id =
  let sql = select_binding_cols ^ " WHERE id = ? LIMIT 1" in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
  match Sqlite3.step stmt with
  | Sqlite3.Rc.ROW -> (
      let r = binding_of_stmt stmt in
      ignore (Sqlite3.finalize stmt);
      match r with Ok b -> Ok (Some b) | Error e -> Error e)
  | Sqlite3.Rc.DONE ->
      ignore (Sqlite3.finalize stmt);
      Ok None
  | rc ->
      let err = Sqlite3.errmsg db in
      ignore (Sqlite3.finalize stmt);
      Error
        (Printf.sprintf "get github_account_binding failed: %s (%s)"
           (Sqlite3.Rc.to_string rc) err)

let get_by_identity ~db ~identity =
  let sql =
    select_binding_cols
    ^ " WHERE host = ? AND app_id = ? AND github_user_id = ? LIMIT 1"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT identity.host));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int identity.app_id)));
  ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.INT identity.github_user_id));
  match Sqlite3.step stmt with
  | Sqlite3.Rc.ROW -> (
      let r = binding_of_stmt stmt in
      ignore (Sqlite3.finalize stmt);
      match r with Ok b -> Ok (Some b) | Error e -> Error e)
  | Sqlite3.Rc.DONE ->
      ignore (Sqlite3.finalize stmt);
      Ok None
  | rc ->
      let err = Sqlite3.errmsg db in
      ignore (Sqlite3.finalize stmt);
      Error
        (Printf.sprintf "get_by_identity github_account_binding failed: %s (%s)"
           (Sqlite3.Rc.to_string rc) err)

let list_for_principal ~db ~principal_id =
  let sql =
    select_binding_cols
    ^ " WHERE principal_id = ? ORDER BY created_at ASC, id ASC"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore
    (Sqlite3.bind stmt 1
       (Sqlite3.Data.TEXT (P.principal_id_to_string principal_id)));
  let rec loop acc =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match binding_of_stmt stmt with
        | Ok b -> loop (b :: acc)
        | Error e ->
            ignore (Sqlite3.finalize stmt);
            Error e)
    | Sqlite3.Rc.DONE ->
        ignore (Sqlite3.finalize stmt);
        Ok (List.rev acc)
    | rc ->
        let err = Sqlite3.errmsg db in
        ignore (Sqlite3.finalize stmt);
        Error
          (Printf.sprintf "list_for_principal failed: %s (%s)"
             (Sqlite3.Rc.to_string rc) err)
  in
  loop []

let list_for_app_user ~db ~app_id ~github_user_id ?host () =
  if app_id <= 0 then Error "app_id must be positive"
  else if github_user_id <= 0L then Error "github_user_id must be positive"
  else
    let sql, bind_host =
      match host with
      | None ->
          ( select_binding_cols
            ^ " WHERE app_id = ? AND github_user_id = ? ORDER BY created_at \
               ASC, id ASC",
            None )
      | Some h ->
          let h = String.trim h in
          if h = "" then
            ( select_binding_cols
              ^ " WHERE app_id = ? AND github_user_id = ? ORDER BY created_at \
                 ASC, id ASC",
              None )
          else
            ( select_binding_cols
              ^ " WHERE app_id = ? AND github_user_id = ? AND host = ? ORDER \
                 BY created_at ASC, id ASC",
              Some h )
    in
    let stmt = Sqlite3.prepare db sql in
    ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int app_id)));
    ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT github_user_id));
    (match bind_host with
    | None -> ()
    | Some h -> ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT h)));
    let rec loop acc =
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match binding_of_stmt stmt with
          | Ok b -> loop (b :: acc)
          | Error e ->
              ignore (Sqlite3.finalize stmt);
              Error e)
      | Sqlite3.Rc.DONE ->
          ignore (Sqlite3.finalize stmt);
          Ok (List.rev acc)
      | rc ->
          let err = Sqlite3.errmsg db in
          ignore (Sqlite3.finalize stmt);
          Error
            (Printf.sprintf "list_for_app_user failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) err)
    in
    loop []

let delete ~db ~id =
  let sql = "DELETE FROM github_account_bindings WHERE id = ?" in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
  match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE ->
      let changes = Sqlite3.changes db in
      ignore (Sqlite3.finalize stmt);
      if changes = 0 then
        Error (Printf.sprintf "github_account_binding not found: %s" id)
      else Ok ()
  | rc ->
      let err = Sqlite3.errmsg db in
      ignore (Sqlite3.finalize stmt);
      Error
        (Printf.sprintf "delete github_account_binding failed: %s (%s)"
           (Sqlite3.Rc.to_string rc) err)

(* -------------------------------------------------------------------------- *)
(* Mutations                                                                  *)
(* -------------------------------------------------------------------------- *)

let revision_conflict ~id ~expected ~actual =
  Printf.sprintf
    "revision conflict for github_account_binding %s: expected %d, actual %d" id
    expected actual

let load_for_update ~db ~id =
  match get ~db ~id with
  | Error e -> Error e
  | Ok None -> Error (Printf.sprintf "github_account_binding not found: %s" id)
  | Ok (Some b) -> Ok b

let write_row ~db (b : binding) =
  let sql =
    {|UPDATE github_account_bindings SET
        principal_id = ?,
        login = ?,
        avatar_url = ?,
        authorization_status = ?,
        revision = ?,
        vault_ref = ?,
        updated_at = ?
      WHERE id = ?|}
  in
  let stmt = Sqlite3.prepare db sql in
  ignore
    (Sqlite3.bind stmt 1
       (Sqlite3.Data.TEXT (P.principal_id_to_string b.principal_id)));
  (match b.display.login with
  | None -> ignore (Sqlite3.bind stmt 2 Sqlite3.Data.NULL)
  | Some s -> ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT s)));
  (match b.display.avatar_url with
  | None -> ignore (Sqlite3.bind stmt 3 Sqlite3.Data.NULL)
  | Some s -> ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT s)));
  ignore
    (Sqlite3.bind stmt 4
       (Sqlite3.Data.TEXT
          (string_of_authorization_status b.authorization_status)));
  ignore (Sqlite3.bind stmt 5 (Sqlite3.Data.INT (Int64.of_int b.revision)));
  (match b.vault_ref with
  | None -> ignore (Sqlite3.bind stmt 6 Sqlite3.Data.NULL)
  | Some r -> ignore (Sqlite3.bind stmt 6 (Sqlite3.Data.TEXT r)));
  ignore (Sqlite3.bind stmt 7 (Sqlite3.Data.TEXT b.updated_at));
  ignore (Sqlite3.bind stmt 8 (Sqlite3.Data.TEXT b.id));
  match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE ->
      ignore (Sqlite3.finalize stmt);
      Ok b
  | rc ->
      let err = Sqlite3.errmsg db in
      ignore (Sqlite3.finalize stmt);
      Error
        (Printf.sprintf "update github_account_binding failed: %s (%s)"
           (Sqlite3.Rc.to_string rc) err)

let check_expected_revision b expected =
  match expected with
  | None -> Ok ()
  | Some exp when exp = b.revision -> Ok ()
  | Some exp ->
      Error (revision_conflict ~id:b.id ~expected:exp ~actual:b.revision)

let update ~db ?expected_revision ?display ?authorization_status ?vault_ref
    ?(now = Unix.gettimeofday ()) ~id () =
  match load_for_update ~db ~id with
  | Error e -> Error e
  | Ok b -> (
      match check_expected_revision b expected_revision with
      | Error e -> Error e
      | Ok () ->
          let now_s = Time_util.iso8601_utc ~t:now () in
          let display = match display with Some d -> d | None -> b.display in
          let authorization_status =
            match authorization_status with
            | Some s -> s
            | None -> b.authorization_status
          in
          let vault_ref =
            match vault_ref with Some v -> v | None -> b.vault_ref
          in
          let next =
            {
              b with
              display;
              authorization_status;
              vault_ref;
              revision = b.revision + 1;
              updated_at = now_s;
            }
          in
          write_row ~db next)

let update_display ~db ?expected_revision ?login ?avatar_url
    ?(now = Unix.gettimeofday ()) ~id () =
  match load_for_update ~db ~id with
  | Error e -> Error e
  | Ok b ->
      let display =
        {
          login = (match login with Some v -> v | None -> b.display.login);
          avatar_url =
            (match avatar_url with Some v -> v | None -> b.display.avatar_url);
        }
      in
      update ~db ?expected_revision ~display ~now ~id ()

let update_authorization_status ~db ?expected_revision
    ?(now = Unix.gettimeofday ()) ~id ~status () =
  update ~db ?expected_revision ~authorization_status:status ~now ~id ()

let set_vault_ref ~db ?expected_revision ?(now = Unix.gettimeofday ()) ~id
    ~vault_ref () =
  update ~db ?expected_revision ~vault_ref ~now ~id ()

(* -------------------------------------------------------------------------- *)
(* Snapshots                                                                  *)
(* -------------------------------------------------------------------------- *)

let insert_snapshot ~db (s : binding_snapshot) =
  let sql =
    {|INSERT INTO github_account_binding_snapshots
        (id, binding_id, principal_id_at_snapshot, lineage_id, binding_json,
         reason, related_id, created_at)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?)|}
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT s.id));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT s.binding_id));
  ignore
    (Sqlite3.bind stmt 3
       (Sqlite3.Data.TEXT (P.principal_id_to_string s.principal_id_at_snapshot)));
  ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT s.lineage_id));
  ignore (Sqlite3.bind stmt 5 (Sqlite3.Data.TEXT s.binding_json));
  ignore (Sqlite3.bind stmt 6 (Sqlite3.Data.TEXT s.reason));
  (match s.related_id with
  | None -> ignore (Sqlite3.bind stmt 7 Sqlite3.Data.NULL)
  | Some r -> ignore (Sqlite3.bind stmt 7 (Sqlite3.Data.TEXT r)));
  ignore (Sqlite3.bind stmt 8 (Sqlite3.Data.TEXT s.created_at));
  match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE ->
      ignore (Sqlite3.finalize stmt);
      Ok s
  | rc ->
      let err = Sqlite3.errmsg db in
      ignore (Sqlite3.finalize stmt);
      Error
        (Printf.sprintf "insert binding snapshot failed: %s (%s)"
           (Sqlite3.Rc.to_string rc) err)

let snapshot_from_binding ~now ?reason ?related_id ?snapshot_id (b : binding) =
  let now_s = Time_util.iso8601_utc ~t:now () in
  let id =
    match snapshot_id with
    | Some s when String.trim s <> "" -> s
    | _ -> generate_snapshot_id ~now ()
  in
  {
    id;
    binding_id = b.id;
    principal_id_at_snapshot = b.principal_id;
    lineage_id = b.lineage_id;
    binding_json = Yojson.Safe.to_string (binding_to_json b);
    reason = (match reason with Some r -> r | None -> "snapshot");
    related_id;
    created_at = now_s;
  }

let snapshot ~db ?(now = Unix.gettimeofday ()) ?reason ?related_id ?snapshot_id
    ~id () =
  match load_for_update ~db ~id with
  | Error e -> Error e
  | Ok b ->
      let s = snapshot_from_binding ~now ?reason ?related_id ?snapshot_id b in
      insert_snapshot ~db s

let get_snapshot ~db ~id =
  let sql =
    {|SELECT id, binding_id, principal_id_at_snapshot, lineage_id, binding_json,
             reason, related_id, created_at
      FROM github_account_binding_snapshots WHERE id = ? LIMIT 1|}
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
  match Sqlite3.step stmt with
  | Sqlite3.Rc.ROW -> (
      let r = snapshot_of_stmt stmt in
      ignore (Sqlite3.finalize stmt);
      match r with Ok s -> Ok (Some s) | Error e -> Error e)
  | Sqlite3.Rc.DONE ->
      ignore (Sqlite3.finalize stmt);
      Ok None
  | rc ->
      let err = Sqlite3.errmsg db in
      ignore (Sqlite3.finalize stmt);
      Error
        (Printf.sprintf "get_snapshot failed: %s (%s)" (Sqlite3.Rc.to_string rc)
           err)

let list_snapshots_for_binding ~db ~binding_id =
  let sql =
    {|SELECT id, binding_id, principal_id_at_snapshot, lineage_id, binding_json,
             reason, related_id, created_at
      FROM github_account_binding_snapshots
      WHERE binding_id = ?
      ORDER BY created_at ASC, id ASC|}
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT binding_id));
  let rec loop acc =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match snapshot_of_stmt stmt with
        | Ok s -> loop (s :: acc)
        | Error e ->
            ignore (Sqlite3.finalize stmt);
            Error e)
    | Sqlite3.Rc.DONE ->
        ignore (Sqlite3.finalize stmt);
        Ok (List.rev acc)
    | rc ->
        let err = Sqlite3.errmsg db in
        ignore (Sqlite3.finalize stmt);
        Error
          (Printf.sprintf "list_snapshots_for_binding failed: %s (%s)"
             (Sqlite3.Rc.to_string rc) err)
  in
  loop []

(* -------------------------------------------------------------------------- *)
(* Principal adoption                                                         *)
(* -------------------------------------------------------------------------- *)

let adopt_to_principal ~db ?expected_revision ?(now = Unix.gettimeofday ())
    ?(reason = "pre_adopt") ?related_id ~id ~to_principal () =
  with_immediate_tx db (fun () ->
      match load_for_update ~db ~id with
      | Error e -> Error e
      | Ok b -> (
          match check_expected_revision b expected_revision with
          | Error e -> Error e
          | Ok () -> (
              if P.principal_id_equal b.principal_id to_principal then
                (* Idempotent no-op reassignment: still snapshot for evidence. *)
                let s = snapshot_from_binding ~now ~reason ?related_id b in
                match insert_snapshot ~db s with
                | Error e -> Error e
                | Ok s -> Ok (b, s)
              else
                let s = snapshot_from_binding ~now ~reason ?related_id b in
                match insert_snapshot ~db s with
                | Error e -> Error e
                | Ok s -> (
                    let now_s = Time_util.iso8601_utc ~t:now () in
                    let next =
                      {
                        b with
                        principal_id = to_principal;
                        revision = b.revision + 1;
                        updated_at = now_s;
                      }
                    in
                    match write_row ~db next with
                    | Error e -> Error e
                    | Ok next -> Ok (next, s)))))

let slot_of (b : binding) = uniqueness_domain b.identity
let identity_key_of (b : binding) = account_identity_key b.identity

let adopt_all_for_principal ~db ?(now = Unix.gettimeofday ())
    ?(reason = "pre_merge") ?related_id ~from_principal ~to_principal () =
  if P.principal_id_equal from_principal to_principal then Ok []
  else
    match
      ( list_for_principal ~db ~principal_id:from_principal,
        list_for_principal ~db ~principal_id:to_principal )
    with
    | Error e, _ | _, Error e -> Error (`Msg e)
    | Ok loser_bindings, Ok survivor_bindings -> (
        let survivor_by_identity =
          List.fold_left
            (fun acc b -> (identity_key_of b, b) :: acc)
            [] survivor_bindings
        in
        let survivor_slots =
          List.fold_left
            (fun acc b -> (slot_of b, b) :: acc)
            [] survivor_bindings
        in
        (* Detect exclusive-slot conflicts before any write. *)
        let conflicts =
          List.filter_map
            (fun (lb : binding) ->
              let idk = identity_key_of lb in
              match List.assoc_opt idk survivor_by_identity with
              | Some _ -> None (* identical identity → coalesce *)
              | None -> (
                  match List.assoc_opt (slot_of lb) survivor_slots with
                  | None -> None
                  | Some sb ->
                      Some
                        (Printf.sprintf
                           "exclusive GitHub slot %s: survivor holds user %Ld, \
                            loser holds distinct user %Ld (refuse silent \
                            credential overwrite)"
                           (slot_of lb) sb.identity.github_user_id
                           lb.identity.github_user_id)))
            loser_bindings
        in
        match conflicts with
        | c :: _ -> Error (`Conflict c)
        | [] -> (
            let run () =
              let rec go acc = function
                | [] -> Ok (List.rev acc)
                | (lb : binding) :: rest -> (
                    let idk = identity_key_of lb in
                    match List.assoc_opt idk survivor_by_identity with
                    | Some _ -> (
                        (* Coalesce: snapshot loser evidence, drop loser row. *)
                        let s =
                          snapshot_from_binding ~now ~reason ?related_id lb
                        in
                        match insert_snapshot ~db s with
                        | Error e -> Error e
                        | Ok s -> (
                            match delete ~db ~id:lb.id with
                            | Error e -> Error e
                            | Ok () -> go ((lb, s) :: acc) rest))
                    | None -> (
                        let s =
                          snapshot_from_binding ~now ~reason ?related_id lb
                        in
                        match insert_snapshot ~db s with
                        | Error e -> Error e
                        | Ok s -> (
                            let now_s = Time_util.iso8601_utc ~t:now () in
                            let next =
                              {
                                lb with
                                principal_id = to_principal;
                                revision = lb.revision + 1;
                                updated_at = now_s;
                              }
                            in
                            match write_row ~db next with
                            | Error e -> Error e
                            | Ok next -> go ((next, s) :: acc) rest)))
              in
              go [] loser_bindings
            in
            match with_immediate_tx db run with
            | Ok v -> Ok v
            | Error e -> Error (`Msg e)))
