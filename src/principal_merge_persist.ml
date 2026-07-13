(* Persistence for Principal merge state (P21.M1.E1.T011).
   Schema, external accounts, preferences, pending auth, actor snapshots,
   and merge receipts. Logic lives in Principal_merge. *)

module P = Principal_identity
module S = Principal_identity_store

(* -------------------------------------------------------------------------- *)
(* Helpers                                                                    *)
(* -------------------------------------------------------------------------- *)

let exec_schema db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "principal_merge schema error: %s (sql: %s)"
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

(** Run [f] under BEGIN IMMEDIATE (or a savepoint). [f] must return
    [('a, string) result]; commit failures become [Error]. *)
let with_immediate_tx db f =
  let mode =
    match Sqlite3.exec db "BEGIN IMMEDIATE" with
    | Sqlite3.Rc.OK -> `Outer
    | _ -> (
        match Sqlite3.exec db "SAVEPOINT principal_merge" with
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
            match Sqlite3.exec db "RELEASE SAVEPOINT principal_merge" with
            | Sqlite3.Rc.OK -> Ok ()
            | rc ->
                ignore (Sqlite3.exec db "ROLLBACK TO SAVEPOINT principal_merge");
                Error
                  (Printf.sprintf "RELEASE SAVEPOINT failed: %s"
                     (Sqlite3.Rc.to_string rc)))
      in
      let rollback () =
        match kind with
        | `Outer -> ignore (Sqlite3.exec db "ROLLBACK")
        | `Savepoint ->
            ignore (Sqlite3.exec db "ROLLBACK TO SAVEPOINT principal_merge");
            ignore (Sqlite3.exec db "RELEASE SAVEPOINT principal_merge")
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
          (Printf.sprintf "principal_merge transaction aborted: %s"
             (Printexc.to_string exn)))

let iso_now ?(now = Unix.gettimeofday ()) () = Time_util.iso8601_utc ~t:now ()

let generate_merge_id ?(now = Unix.gettimeofday ()) () =
  let ts = int_of_float now in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "pmerge_%d_%06d" ts rand

let generate_snapshot_id ?(now = Unix.gettimeofday ()) () =
  let ts = int_of_float now in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "asnap_%d_%06d" ts rand

let generate_account_id ?(now = Unix.gettimeofday ()) () =
  let ts = int_of_float now in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "pacc_%d_%06d" ts rand

(* -------------------------------------------------------------------------- *)
(* Schema                                                                     *)
(* -------------------------------------------------------------------------- *)

let ensure_schema db =
  S.ensure_schema db;
  let receipts =
    {|CREATE TABLE IF NOT EXISTS principal_merge_receipts (
      id TEXT PRIMARY KEY NOT NULL,
      link_tx_id TEXT,
      survivor_id TEXT NOT NULL,
      loser_id TEXT NOT NULL,
      adopted_actor_keys_json TEXT NOT NULL,
      adopted_link_ids_json TEXT NOT NULL,
      preference_resolutions_json TEXT NOT NULL,
      pending_auth_invalidated INTEGER NOT NULL,
      actor_snapshot_ids_json TEXT NOT NULL,
      survivor_revision_after INTEGER NOT NULL,
      loser_revision_after INTEGER NOT NULL,
      applied_at TEXT NOT NULL,
      notes_json TEXT NOT NULL
    )|}
  in
  let uniq_link_tx =
    {|CREATE UNIQUE INDEX IF NOT EXISTS idx_principal_merge_receipts_link_tx
      ON principal_merge_receipts(link_tx_id) WHERE link_tx_id IS NOT NULL|}
  in
  let accounts =
    {|CREATE TABLE IF NOT EXISTS principal_external_accounts (
      id TEXT PRIMARY KEY NOT NULL,
      principal_id TEXT NOT NULL,
      account_kind TEXT NOT NULL,
      uniqueness_domain TEXT NOT NULL,
      account_identity TEXT NOT NULL,
      exclusive_slot INTEGER NOT NULL,
      revision INTEGER NOT NULL,
      payload_json TEXT NOT NULL,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    )|}
  in
  let uniq_account =
    {|CREATE UNIQUE INDEX IF NOT EXISTS idx_principal_external_accounts_identity
      ON principal_external_accounts(account_kind, uniqueness_domain, account_identity)|}
  in
  let idx_account_principal =
    {|CREATE INDEX IF NOT EXISTS idx_principal_external_accounts_principal
      ON principal_external_accounts(principal_id)|}
  in
  let prefs =
    {|CREATE TABLE IF NOT EXISTS principal_preferences (
      principal_id TEXT NOT NULL,
      pref_key TEXT NOT NULL,
      pref_value TEXT NOT NULL,
      revision INTEGER NOT NULL,
      updated_at TEXT NOT NULL,
      PRIMARY KEY (principal_id, pref_key)
    )|}
  in
  let pending =
    {|CREATE TABLE IF NOT EXISTS principal_pending_auth (
      principal_id TEXT PRIMARY KEY NOT NULL,
      pending_count INTEGER NOT NULL
    )|}
  in
  let snapshots =
    {|CREATE TABLE IF NOT EXISTS actor_snapshots (
      id TEXT PRIMARY KEY NOT NULL,
      actor_key TEXT NOT NULL,
      principal_id_at_snapshot TEXT NOT NULL,
      actor_json TEXT NOT NULL,
      reason TEXT NOT NULL,
      merge_id TEXT,
      created_at TEXT NOT NULL
    )|}
  in
  let idx_snap_actor =
    {|CREATE INDEX IF NOT EXISTS idx_actor_snapshots_actor
      ON actor_snapshots(actor_key)|}
  in
  List.iter (exec_schema db)
    [
      receipts;
      uniq_link_tx;
      accounts;
      uniq_account;
      idx_account_principal;
      prefs;
      pending;
      snapshots;
      idx_snap_actor;
    ]

(* -------------------------------------------------------------------------- *)
(* External accounts                                                          *)
(* -------------------------------------------------------------------------- *)

type external_account = {
  id : string;
  principal_id : P.principal_id;
  account_kind : string;
  uniqueness_domain : string;
  account_identity : string;
  exclusive_slot : bool;
  revision : int;
  payload_json : string;
  created_at : string;
  updated_at : string;
}

let external_account_of_stmt stmt : (external_account, string) result =
  let id = text_col stmt 0 in
  let pid_s = text_col stmt 1 in
  let account_kind = text_col stmt 2 in
  let uniqueness_domain = text_col stmt 3 in
  let account_identity = text_col stmt 4 in
  let exclusive_slot = int_col stmt 5 <> 0 in
  let revision = int_col stmt 6 in
  let payload_json = text_col stmt 7 in
  let created_at = text_col stmt 8 in
  let updated_at = text_col stmt 9 in
  match P.principal_id_of_string pid_s with
  | Error e -> Error e
  | Ok principal_id ->
      Ok
        {
          id;
          principal_id;
          account_kind;
          uniqueness_domain;
          account_identity;
          exclusive_slot;
          revision;
          payload_json;
          created_at;
          updated_at;
        }

let put_external_account ~db ?(now = Unix.gettimeofday ())
    (a : external_account) =
  let now_s = iso_now ~now () in
  let id =
    if String.trim a.id = "" then generate_account_id ~now () else a.id
  in
  let created_at =
    if String.trim a.created_at = "" then now_s else a.created_at
  in
  let a = { a with id; created_at; updated_at = now_s } in
  let sql =
    {|INSERT INTO principal_external_accounts
      (id, principal_id, account_kind, uniqueness_domain, account_identity,
       exclusive_slot, revision, payload_json, created_at, updated_at)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      ON CONFLICT(id) DO UPDATE SET
        principal_id = excluded.principal_id,
        account_kind = excluded.account_kind,
        uniqueness_domain = excluded.uniqueness_domain,
        account_identity = excluded.account_identity,
        exclusive_slot = excluded.exclusive_slot,
        revision = excluded.revision,
        payload_json = excluded.payload_json,
        updated_at = excluded.updated_at|}
  in
  with_immediate_tx db (fun () ->
      let stmt = Sqlite3.prepare db sql in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          let bind i v = ignore (Sqlite3.bind stmt i v) in
          bind 1 (Sqlite3.Data.TEXT a.id);
          bind 2 (Sqlite3.Data.TEXT (P.principal_id_to_string a.principal_id));
          bind 3 (Sqlite3.Data.TEXT a.account_kind);
          bind 4 (Sqlite3.Data.TEXT a.uniqueness_domain);
          bind 5 (Sqlite3.Data.TEXT a.account_identity);
          bind 6
            (Sqlite3.Data.INT (Int64.of_int (if a.exclusive_slot then 1 else 0)));
          bind 7 (Sqlite3.Data.INT (Int64.of_int a.revision));
          bind 8 (Sqlite3.Data.TEXT a.payload_json);
          bind 9 (Sqlite3.Data.TEXT a.created_at);
          bind 10 (Sqlite3.Data.TEXT a.updated_at);
          match Sqlite3.step stmt with
          | Sqlite3.Rc.DONE -> Ok a
          | Sqlite3.Rc.CONSTRAINT ->
              Error
                (Printf.sprintf
                   "external_account collision for %s/%s/%s (already bound)"
                   a.account_kind a.uniqueness_domain a.account_identity)
          | rc ->
              Error
                (Printf.sprintf "put_external_account failed: %s (%s)"
                   (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))))

let list_external_accounts ~db ~principal_id =
  let sql =
    {|SELECT id, principal_id, account_kind, uniqueness_domain, account_identity,
             exclusive_slot, revision, payload_json, created_at, updated_at
      FROM principal_external_accounts
      WHERE principal_id = ?
      ORDER BY account_kind, uniqueness_domain, account_identity|}
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
            match external_account_of_stmt stmt with
            | Ok a -> loop (a :: acc)
            | Error e -> Error e)
        | Sqlite3.Rc.DONE -> Ok (List.rev acc)
        | rc ->
            Error
              (Printf.sprintf "list_external_accounts failed: %s (%s)"
                 (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))
      in
      loop [])

let reassign_external_account ~db ~id ~to_principal ~now_s =
  let sql =
    {|UPDATE principal_external_accounts
      SET principal_id = ?, revision = revision + 1, updated_at = ?
      WHERE id = ?|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1
           (Sqlite3.Data.TEXT (P.principal_id_to_string to_principal)));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT now_s));
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok ()
      | rc ->
          Error
            (Printf.sprintf "reassign_external_account failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let delete_external_account ~db ~id =
  let sql = "DELETE FROM principal_external_accounts WHERE id = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok ()
      | rc ->
          Error
            (Printf.sprintf "delete_external_account failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

(* -------------------------------------------------------------------------- *)
(* Preferences                                                                *)
(* -------------------------------------------------------------------------- *)

type preference = {
  principal_id : P.principal_id;
  key : string;
  value : string;
  revision : int;
  updated_at : string;
}

let put_preference ~db ?(now = Unix.gettimeofday ()) ~principal_id ~key ~value
    ?(revision = 1) () =
  let now_s = iso_now ~now () in
  let sql =
    {|INSERT INTO principal_preferences
      (principal_id, pref_key, pref_value, revision, updated_at)
      VALUES (?, ?, ?, ?, ?)
      ON CONFLICT(principal_id, pref_key) DO UPDATE SET
        pref_value = excluded.pref_value,
        revision = excluded.revision,
        updated_at = excluded.updated_at|}
  in
  with_immediate_tx db (fun () ->
      let stmt = Sqlite3.prepare db sql in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          let bind i v = ignore (Sqlite3.bind stmt i v) in
          bind 1 (Sqlite3.Data.TEXT (P.principal_id_to_string principal_id));
          bind 2 (Sqlite3.Data.TEXT key);
          bind 3 (Sqlite3.Data.TEXT value);
          bind 4 (Sqlite3.Data.INT (Int64.of_int revision));
          bind 5 (Sqlite3.Data.TEXT now_s);
          match Sqlite3.step stmt with
          | Sqlite3.Rc.DONE ->
              Ok { principal_id; key; value; revision; updated_at = now_s }
          | rc ->
              Error
                (Printf.sprintf "put_preference failed: %s (%s)"
                   (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))))

let list_preferences ~db ~principal_id =
  let sql =
    {|SELECT principal_id, pref_key, pref_value, revision, updated_at
      FROM principal_preferences WHERE principal_id = ?
      ORDER BY pref_key ASC|}
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
            match P.principal_id_of_string (text_col stmt 0) with
            | Error e -> Error e
            | Ok pid ->
                loop
                  ({
                     principal_id = pid;
                     key = text_col stmt 1;
                     value = text_col stmt 2;
                     revision = int_col stmt 3;
                     updated_at = text_col stmt 4;
                   }
                  :: acc))
        | Sqlite3.Rc.DONE -> Ok (List.rev acc)
        | rc ->
            Error
              (Printf.sprintf "list_preferences failed: %s (%s)"
                 (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))
      in
      loop [])

let delete_preference ~db ~principal_id ~key =
  let sql =
    "DELETE FROM principal_preferences WHERE principal_id = ? AND pref_key = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1
           (Sqlite3.Data.TEXT (P.principal_id_to_string principal_id)));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT key));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok ()
      | rc ->
          Error
            (Printf.sprintf "delete_preference failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

(* -------------------------------------------------------------------------- *)
(* Pending authorization                                                      *)
(* -------------------------------------------------------------------------- *)

let set_pending_authorization_count ~db ~principal_id ~count =
  let sql =
    {|INSERT INTO principal_pending_auth (principal_id, pending_count)
      VALUES (?, ?)
      ON CONFLICT(principal_id) DO UPDATE SET pending_count = excluded.pending_count|}
  in
  with_immediate_tx db (fun () ->
      let stmt = Sqlite3.prepare db sql in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          ignore
            (Sqlite3.bind stmt 1
               (Sqlite3.Data.TEXT (P.principal_id_to_string principal_id)));
          ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int count)));
          match Sqlite3.step stmt with
          | Sqlite3.Rc.DONE -> Ok ()
          | rc ->
              Error
                (Printf.sprintf
                   "set_pending_authorization_count failed: %s (%s)"
                   (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))))

let get_pending_authorization_count ~db ~principal_id =
  let sql =
    "SELECT pending_count FROM principal_pending_auth WHERE principal_id = ? \
     LIMIT 1"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1
           (Sqlite3.Data.TEXT (P.principal_id_to_string principal_id)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Ok (int_col stmt 0)
      | Sqlite3.Rc.DONE -> Ok 0
      | rc ->
          Error
            (Printf.sprintf "get_pending_authorization_count failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

(* -------------------------------------------------------------------------- *)
(* Actor snapshots                                                            *)
(* -------------------------------------------------------------------------- *)

type actor_snapshot = {
  id : string;
  actor_key : string;
  principal_id_at_snapshot : P.principal_id;
  actor_json : string;
  reason : string;
  merge_id : string option;
  created_at : string;
}

let actor_snapshot_of_stmt stmt : (actor_snapshot, string) result =
  let id = text_col stmt 0 in
  let actor_key = text_col stmt 1 in
  let pid_s = text_col stmt 2 in
  let actor_json = text_col stmt 3 in
  let reason = text_col stmt 4 in
  let merge_id = opt_text_col stmt 5 in
  let created_at = text_col stmt 6 in
  match P.principal_id_of_string pid_s with
  | Error e -> Error e
  | Ok principal_id_at_snapshot ->
      Ok
        {
          id;
          actor_key;
          principal_id_at_snapshot;
          actor_json;
          reason;
          merge_id;
          created_at;
        }

let insert_actor_snapshot ~db (s : actor_snapshot) =
  let sql =
    {|INSERT INTO actor_snapshots
      (id, actor_key, principal_id_at_snapshot, actor_json, reason, merge_id,
       created_at)
      VALUES (?, ?, ?, ?, ?, ?, ?)|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let bind i v = ignore (Sqlite3.bind stmt i v) in
      bind 1 (Sqlite3.Data.TEXT s.id);
      bind 2 (Sqlite3.Data.TEXT s.actor_key);
      bind 3
        (Sqlite3.Data.TEXT (P.principal_id_to_string s.principal_id_at_snapshot));
      bind 4 (Sqlite3.Data.TEXT s.actor_json);
      bind 5 (Sqlite3.Data.TEXT s.reason);
      bind 6
        (match s.merge_id with
        | Some m -> Sqlite3.Data.TEXT m
        | None -> Sqlite3.Data.NULL);
      bind 7 (Sqlite3.Data.TEXT s.created_at);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok s
      | rc ->
          Error
            (Printf.sprintf "insert_actor_snapshot failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let get_actor_snapshot ~db ~id =
  let sql =
    {|SELECT id, actor_key, principal_id_at_snapshot, actor_json, reason,
             merge_id, created_at
      FROM actor_snapshots WHERE id = ? LIMIT 1|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match actor_snapshot_of_stmt stmt with
          | Ok s -> Ok (Some s)
          | Error e -> Error e)
      | Sqlite3.Rc.DONE -> Ok None
      | rc ->
          Error
            (Printf.sprintf "get_actor_snapshot failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let list_actor_snapshots_for_actor ~db ~actor_key =
  let sql =
    {|SELECT id, actor_key, principal_id_at_snapshot, actor_json, reason,
             merge_id, created_at
      FROM actor_snapshots WHERE actor_key = ? ORDER BY created_at ASC, id ASC|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT actor_key));
      let rec loop acc =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> (
            match actor_snapshot_of_stmt stmt with
            | Ok s -> loop (s :: acc)
            | Error e -> Error e)
        | Sqlite3.Rc.DONE -> Ok (List.rev acc)
        | rc ->
            Error
              (Printf.sprintf "list_actor_snapshots_for_actor failed: %s (%s)"
                 (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))
      in
      loop [])

(* -------------------------------------------------------------------------- *)
(* Receipt / preference-resolution types (shared with Principal_merge)        *)
(* -------------------------------------------------------------------------- *)

type preference_resolution = {
  key : string;
  outcome : [ `Adopted_from_loser | `Kept_survivor | `Identical ];
  survivor_value : string option;
  loser_value : string option;
}

type merge_receipt = {
  id : string;
  link_tx_id : string option;
  survivor_id : P.principal_id;
  loser_id : P.principal_id;
  adopted_actor_keys : string list;
  adopted_link_ids : string list;
  preference_resolutions : preference_resolution list;
  pending_auth_invalidated : int;
  actor_snapshot_ids : string list;
  survivor_revision_after : int;
  loser_revision_after : int;
  applied_at : string;
  notes : string list;
}

(* -------------------------------------------------------------------------- *)
(* JSON codecs for receipt storage                                            *)
(* -------------------------------------------------------------------------- *)

let string_list_to_json xs = `List (List.map (fun s -> `String s) xs)

let string_list_of_json = function
  | `List items ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | `String s :: rest -> loop (s :: acc) rest
        | _ -> Error "expected string list"
      in
      loop [] items
  | `Null -> Ok []
  | _ -> Error "expected JSON list"

let pref_outcome_to_string = function
  | `Adopted_from_loser -> "adopted_from_loser"
  | `Kept_survivor -> "kept_survivor"
  | `Identical -> "identical"

let pref_outcome_of_string = function
  | "adopted_from_loser" -> Ok `Adopted_from_loser
  | "kept_survivor" -> Ok `Kept_survivor
  | "identical" -> Ok `Identical
  | s -> Error (Printf.sprintf "unknown preference outcome: %s" s)

let preference_resolution_to_json (r : preference_resolution) =
  let opt = function None -> `Null | Some s -> `String s in
  `Assoc
    [
      ("key", `String r.key);
      ("outcome", `String (pref_outcome_to_string r.outcome));
      ("survivor_value", opt r.survivor_value);
      ("loser_value", opt r.loser_value);
    ]

let preference_resolution_of_json = function
  | `Assoc _ as j -> (
      let key =
        match Yojson.Safe.Util.member "key" j with
        | `String s -> Ok s
        | _ -> Error "preference_resolution missing key"
      in
      let outcome =
        match Yojson.Safe.Util.member "outcome" j with
        | `String s -> pref_outcome_of_string s
        | _ -> Error "preference_resolution missing outcome"
      in
      let opt_str name =
        match Yojson.Safe.Util.member name j with
        | `String s -> Some s
        | `Null | _ -> None
      in
      match (key, outcome) with
      | Error e, _ | _, Error e -> Error e
      | Ok key, Ok outcome ->
          Ok
            {
              key;
              outcome;
              survivor_value = opt_str "survivor_value";
              loser_value = opt_str "loser_value";
            })
  | _ -> Error "preference_resolution must be object"

let preference_resolutions_to_json rs =
  `List (List.map preference_resolution_to_json rs)

let preference_resolutions_of_json = function
  | `List items ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | x :: rest -> (
            match preference_resolution_of_json x with
            | Ok r -> loop (r :: acc) rest
            | Error e -> Error e)
      in
      loop [] items
  | `Null -> Ok []
  | _ -> Error "preference_resolutions must be list"

let receipt_of_stmt stmt : (merge_receipt, string) result =
  let id = text_col stmt 0 in
  let link_tx_id = opt_text_col stmt 1 in
  let survivor_s = text_col stmt 2 in
  let loser_s = text_col stmt 3 in
  let actors_json = text_col stmt 4 in
  let links_json = text_col stmt 5 in
  let prefs_json = text_col stmt 6 in
  let pending_auth_invalidated = int_col stmt 7 in
  let snaps_json = text_col stmt 8 in
  let survivor_revision_after = int_col stmt 9 in
  let loser_revision_after = int_col stmt 10 in
  let applied_at = text_col stmt 11 in
  let notes_json = text_col stmt 12 in
  match
    ( P.principal_id_of_string survivor_s,
      P.principal_id_of_string loser_s,
      string_list_of_json (Yojson.Safe.from_string actors_json),
      string_list_of_json (Yojson.Safe.from_string links_json),
      preference_resolutions_of_json (Yojson.Safe.from_string prefs_json),
      string_list_of_json (Yojson.Safe.from_string snaps_json),
      string_list_of_json (Yojson.Safe.from_string notes_json) )
  with
  | ( Ok survivor_id,
      Ok loser_id,
      Ok adopted_actor_keys,
      Ok adopted_link_ids,
      Ok preference_resolutions,
      Ok actor_snapshot_ids,
      Ok notes ) ->
      Ok
        {
          id;
          link_tx_id;
          survivor_id;
          loser_id;
          adopted_actor_keys;
          adopted_link_ids;
          preference_resolutions;
          pending_auth_invalidated;
          actor_snapshot_ids;
          survivor_revision_after;
          loser_revision_after;
          applied_at;
          notes;
        }
  | Error e, _, _, _, _, _, _
  | _, Error e, _, _, _, _, _
  | _, _, Error e, _, _, _, _
  | _, _, _, Error e, _, _, _
  | _, _, _, _, Error e, _, _
  | _, _, _, _, _, Error e, _
  | _, _, _, _, _, _, Error e ->
      Error e

let insert_merge_receipt ~db (r : merge_receipt) =
  let sql =
    {|INSERT INTO principal_merge_receipts
      (id, link_tx_id, survivor_id, loser_id, adopted_actor_keys_json,
       adopted_link_ids_json, preference_resolutions_json,
       pending_auth_invalidated, actor_snapshot_ids_json,
       survivor_revision_after, loser_revision_after, applied_at, notes_json)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let bind i v = ignore (Sqlite3.bind stmt i v) in
      bind 1 (Sqlite3.Data.TEXT r.id);
      bind 2
        (match r.link_tx_id with
        | Some s -> Sqlite3.Data.TEXT s
        | None -> Sqlite3.Data.NULL);
      bind 3 (Sqlite3.Data.TEXT (P.principal_id_to_string r.survivor_id));
      bind 4 (Sqlite3.Data.TEXT (P.principal_id_to_string r.loser_id));
      bind 5
        (Sqlite3.Data.TEXT
           (Yojson.Safe.to_string (string_list_to_json r.adopted_actor_keys)));
      bind 6
        (Sqlite3.Data.TEXT
           (Yojson.Safe.to_string (string_list_to_json r.adopted_link_ids)));
      bind 7
        (Sqlite3.Data.TEXT
           (Yojson.Safe.to_string
              (preference_resolutions_to_json r.preference_resolutions)));
      bind 8 (Sqlite3.Data.INT (Int64.of_int r.pending_auth_invalidated));
      bind 9
        (Sqlite3.Data.TEXT
           (Yojson.Safe.to_string (string_list_to_json r.actor_snapshot_ids)));
      bind 10 (Sqlite3.Data.INT (Int64.of_int r.survivor_revision_after));
      bind 11 (Sqlite3.Data.INT (Int64.of_int r.loser_revision_after));
      bind 12 (Sqlite3.Data.TEXT r.applied_at);
      bind 13
        (Sqlite3.Data.TEXT (Yojson.Safe.to_string (string_list_to_json r.notes)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok r
      | Sqlite3.Rc.CONSTRAINT ->
          Error
            (Printf.sprintf "merge receipt collision for id=%s or link_tx_id"
               r.id)
      | rc ->
          Error
            (Printf.sprintf "insert_merge_receipt failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let get_merge_receipt ~db ~id =
  let sql =
    {|SELECT id, link_tx_id, survivor_id, loser_id, adopted_actor_keys_json,
             adopted_link_ids_json, preference_resolutions_json,
             pending_auth_invalidated, actor_snapshot_ids_json,
             survivor_revision_after, loser_revision_after, applied_at,
             notes_json
      FROM principal_merge_receipts WHERE id = ? LIMIT 1|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match receipt_of_stmt stmt with
          | Ok r -> Ok (Some r)
          | Error e -> Error e)
      | Sqlite3.Rc.DONE -> Ok None
      | rc ->
          Error
            (Printf.sprintf "get_merge_receipt failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let get_merge_receipt_by_link_tx ~db ~link_tx_id =
  let sql =
    {|SELECT id, link_tx_id, survivor_id, loser_id, adopted_actor_keys_json,
             adopted_link_ids_json, preference_resolutions_json,
             pending_auth_invalidated, actor_snapshot_ids_json,
             survivor_revision_after, loser_revision_after, applied_at,
             notes_json
      FROM principal_merge_receipts WHERE link_tx_id = ? LIMIT 1|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT link_tx_id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match receipt_of_stmt stmt with
          | Ok r -> Ok (Some r)
          | Error e -> Error e)
      | Sqlite3.Rc.DONE -> Ok None
      | rc ->
          Error
            (Printf.sprintf "get_merge_receipt_by_link_tx failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let find_receipt_for_pair ~db ~survivor_id ~loser_id =
  let sql =
    {|SELECT id, link_tx_id, survivor_id, loser_id, adopted_actor_keys_json,
             adopted_link_ids_json, preference_resolutions_json,
             pending_auth_invalidated, actor_snapshot_ids_json,
             survivor_revision_after, loser_revision_after, applied_at,
             notes_json
      FROM principal_merge_receipts
      WHERE survivor_id = ? AND loser_id = ?
      ORDER BY applied_at DESC LIMIT 1|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1
           (Sqlite3.Data.TEXT (P.principal_id_to_string survivor_id)));
      ignore
        (Sqlite3.bind stmt 2
           (Sqlite3.Data.TEXT (P.principal_id_to_string loser_id)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match receipt_of_stmt stmt with
          | Ok r -> Ok (Some r)
          | Error e -> Error e)
      | Sqlite3.Rc.DONE -> Ok None
      | rc ->
          Error
            (Printf.sprintf "find_receipt_for_pair failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))
