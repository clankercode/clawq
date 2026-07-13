(* Unlink / split and identity revocation lifecycle (P21.M1.E1.T012).
   See principal_unlink_split.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module P = Principal_identity
module S = Principal_identity_store
module M = Principal_merge

let protocol_version = 1
let default_plan_ttl_seconds = 30. *. 60.

(* -------------------------------------------------------------------------- *)
(* Helpers                                                                    *)
(* -------------------------------------------------------------------------- *)

let exec_schema db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "principal_unlink_split schema error: %s (sql: %s)"
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

let iso_now ?(now = Unix.gettimeofday ()) () = Time_util.iso8601_utc ~t:now ()

let generate_id ~prefix ?(now = Unix.gettimeofday ()) () =
  let ts = int_of_float now in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "%s_%d_%06d" prefix ts rand

let generate_plan_id ?(now = Unix.gettimeofday ()) () =
  generate_id ~prefix:"psplit" ~now ()

let generate_unlink_id ?(now = Unix.gettimeofday ()) () =
  generate_id ~prefix:"punlink" ~now ()

let generate_principal_id ?(now = Unix.gettimeofday ()) () =
  generate_id ~prefix:"prin" ~now ()

let generate_link_id ?(now = Unix.gettimeofday ()) () =
  generate_id ~prefix:"idlink" ~now ()

let generate_lease_id ?(now = Unix.gettimeofday ()) () =
  generate_id ~prefix:"please" ~now ()

let digest_hex payload =
  let open Digestif.SHA256 in
  to_hex (digest_string payload)

let is_revision_conflict msg =
  let lower = String.lowercase_ascii msg in
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
  contains "revision conflict" || contains "revision conflict for"

type tx_err =
  | Tx_msg of string
  | Tx_stale of string
  | Tx_refused of {
      reason : string;
      conflicts : ownership_conflict list;
      preview : split_preview option;
    }

and ownership_conflict =
  | Account_not_owned of { account_id : string; summary : string }
  | Preference_not_owned of { key : string; summary : string }
  | Reverse_merge_forbidden of { summary : string }
  | Other of { code : string; summary : string }

and ownership_intent =
  | Retain_on_source
  | Explicit_rebind of {
      account_ids : string list;
      preference_keys : string list;
    }

and split_preview = {
  source_principal_id : P.principal_id;
  actor_key : string;
  ownership : ownership_intent;
  accounts_retained : string list;
  accounts_to_rebind : string list;
  preferences_retained : string list;
  preferences_to_rebind : string list;
  pending_auth_to_invalidate : int;
  leases_to_invalidate : int;
  hard_conflicts : ownership_conflict list;
  notes : string list;
}

let with_tx db (f : unit -> ('a, tx_err) result) : ('a, tx_err) result =
  let mode =
    match Sqlite3.exec db "BEGIN IMMEDIATE" with
    | Sqlite3.Rc.OK -> `Outer
    | _ -> (
        match Sqlite3.exec db "SAVEPOINT principal_unlink_split" with
        | Sqlite3.Rc.OK -> `Savepoint
        | rc ->
            `Fail
              (Printf.sprintf "BEGIN IMMEDIATE/SAVEPOINT failed: %s (%s)"
                 (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))
  in
  match mode with
  | `Fail e -> Error (Tx_msg e)
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
              Sqlite3.exec db "RELEASE SAVEPOINT principal_unlink_split"
            with
            | Sqlite3.Rc.OK -> Ok ()
            | rc ->
                ignore
                  (Sqlite3.exec db
                     "ROLLBACK TO SAVEPOINT principal_unlink_split");
                Error
                  (Printf.sprintf "RELEASE SAVEPOINT failed: %s"
                     (Sqlite3.Rc.to_string rc)))
      in
      let rollback () =
        match kind with
        | `Outer -> ignore (Sqlite3.exec db "ROLLBACK")
        | `Savepoint ->
            ignore
              (Sqlite3.exec db "ROLLBACK TO SAVEPOINT principal_unlink_split");
            ignore (Sqlite3.exec db "RELEASE SAVEPOINT principal_unlink_split")
      in
      try
        match f () with
        | Ok v -> (
            match commit () with
            | Ok () -> Ok v
            | Error e ->
                rollback ();
                Error (Tx_msg e))
        | Error e ->
            rollback ();
            Error e
      with exn ->
        rollback ();
        Error
          (Tx_msg
             (Printf.sprintf "principal_unlink_split transaction aborted: %s"
                (Printexc.to_string exn))))

(* -------------------------------------------------------------------------- *)
(* Lease status                                                               *)
(* -------------------------------------------------------------------------- *)

type lease_status = Active | Invalidated | Rebind_required

let string_of_lease_status = function
  | Active -> "active"
  | Invalidated -> "invalidated"
  | Rebind_required -> "rebind_required"

let lease_status_of_string = function
  | "active" -> Ok Active
  | "invalidated" -> Ok Invalidated
  | "rebind_required" -> Ok Rebind_required
  | s -> Error (Printf.sprintf "unknown lease_status: %s" s)

type account_lease = {
  id : string;
  principal_id : P.principal_id;
  account_id : string option;
  actor_key : string option;
  status : lease_status;
  revision : int;
  created_at : string;
  updated_at : string;
}

(* -------------------------------------------------------------------------- *)
(* Plan status                                                                *)
(* -------------------------------------------------------------------------- *)

type plan_status =
  | Planned
  | Confirmed
  | Applied
  | Rejected
  | Expired
  | Cancelled
  | Stale_revision

let string_of_plan_status = function
  | Planned -> "planned"
  | Confirmed -> "confirmed"
  | Applied -> "applied"
  | Rejected -> "rejected"
  | Expired -> "expired"
  | Cancelled -> "cancelled"
  | Stale_revision -> "stale_revision"

let plan_status_of_string = function
  | "planned" -> Ok Planned
  | "confirmed" -> Ok Confirmed
  | "applied" -> Ok Applied
  | "rejected" -> Ok Rejected
  | "expired" -> Ok Expired
  | "cancelled" -> Ok Cancelled
  | "stale_revision" -> Ok Stale_revision
  | s -> Error (Printf.sprintf "unknown plan_status: %s" s)

let plan_status_is_terminal = function
  | Applied | Rejected | Expired | Cancelled | Stale_revision -> true
  | Planned | Confirmed -> false

type split_plan = {
  version : int;
  id : string;
  source_principal_id : P.principal_id;
  source_revision : int;
  actor_key : P.connector_actor_key;
  actor_revision : int;
  ownership : ownership_intent;
  admin_principal_id : P.principal_id option;
  preview : split_preview;
  digest : string;
  status : plan_status;
  created_at : string;
  expires_at : string;
  confirmed_at : string option;
  applied_at : string option;
  reject_reason : string option;
  new_principal_id : P.principal_id option;
}

type unlink_receipt = {
  id : string;
  plan_id : string;
  source_principal_id : P.principal_id;
  new_principal_id : P.principal_id;
  actor_key : string;
  unlinked_link_id : string option;
  new_link_id : string;
  rebound_account_ids : string list;
  rebound_preference_keys : string list;
  pending_auth_invalidated : int;
  leases_invalidated : int;
  actor_snapshot_ids : string list;
  source_revision_after : int;
  new_principal_revision : int;
  actor_revision_after : int;
  applied_at : string;
  notes : string list;
}

type apply_status =
  | Applied of unlink_receipt
  | Idempotent of unlink_receipt
  | Refused of {
      reason : string;
      conflicts : ownership_conflict list;
      preview : split_preview option;
    }
  | Stale_revision of string

(* -------------------------------------------------------------------------- *)
(* JSON codecs for plan persistence                                           *)
(* -------------------------------------------------------------------------- *)

let ownership_to_json = function
  | Retain_on_source -> `Assoc [ ("kind", `String "retain_on_source") ]
  | Explicit_rebind { account_ids; preference_keys } ->
      `Assoc
        [
          ("kind", `String "explicit_rebind");
          ("account_ids", `List (List.map (fun s -> `String s) account_ids));
          ( "preference_keys",
            `List (List.map (fun s -> `String s) preference_keys) );
        ]

let ownership_of_json = function
  | `Assoc fields as j -> (
      match Yojson.Safe.Util.member "kind" j with
      | `String "retain_on_source" -> Ok Retain_on_source
      | `String "explicit_rebind" ->
          let account_ids =
            match Yojson.Safe.Util.member "account_ids" j with
            | `List xs ->
                List.filter_map (function `String s -> Some s | _ -> None) xs
            | _ -> []
          in
          let preference_keys =
            match Yojson.Safe.Util.member "preference_keys" j with
            | `List xs ->
                List.filter_map (function `String s -> Some s | _ -> None) xs
            | _ -> []
          in
          Ok (Explicit_rebind { account_ids; preference_keys })
      | _ -> Error "ownership.kind missing or unknown")
  | _ -> Error "ownership must be object"

let conflict_to_json = function
  | Account_not_owned { account_id; summary } ->
      `Assoc
        [
          ("kind", `String "account_not_owned");
          ("account_id", `String account_id);
          ("summary", `String summary);
        ]
  | Preference_not_owned { key; summary } ->
      `Assoc
        [
          ("kind", `String "preference_not_owned");
          ("key", `String key);
          ("summary", `String summary);
        ]
  | Reverse_merge_forbidden { summary } ->
      `Assoc
        [
          ("kind", `String "reverse_merge_forbidden");
          ("summary", `String summary);
        ]
  | Other { code; summary } ->
      `Assoc
        [
          ("kind", `String "other");
          ("code", `String code);
          ("summary", `String summary);
        ]

let conflict_of_json = function
  | `Assoc _ as j -> (
      match Yojson.Safe.Util.member "kind" j with
      | `String "account_not_owned" ->
          Ok
            (Account_not_owned
               {
                 account_id =
                   (match Yojson.Safe.Util.member "account_id" j with
                   | `String s -> s
                   | _ -> "");
                 summary =
                   (match Yojson.Safe.Util.member "summary" j with
                   | `String s -> s
                   | _ -> "");
               })
      | `String "preference_not_owned" ->
          Ok
            (Preference_not_owned
               {
                 key =
                   (match Yojson.Safe.Util.member "key" j with
                   | `String s -> s
                   | _ -> "");
                 summary =
                   (match Yojson.Safe.Util.member "summary" j with
                   | `String s -> s
                   | _ -> "");
               })
      | `String "reverse_merge_forbidden" ->
          Ok
            (Reverse_merge_forbidden
               {
                 summary =
                   (match Yojson.Safe.Util.member "summary" j with
                   | `String s -> s
                   | _ -> "reverse merge forbidden");
               })
      | `String "other" ->
          Ok
            (Other
               {
                 code =
                   (match Yojson.Safe.Util.member "code" j with
                   | `String s -> s
                   | _ -> "other");
                 summary =
                   (match Yojson.Safe.Util.member "summary" j with
                   | `String s -> s
                   | _ -> "");
               })
      | _ -> Error "unknown conflict kind")
  | _ -> Error "conflict must be object"

let string_list_to_json xs = `List (List.map (fun s -> `String s) xs)

let string_list_of_json = function
  | `List xs ->
      Ok (List.filter_map (function `String s -> Some s | _ -> None) xs)
  | `Null -> Ok []
  | _ -> Error "expected string list"

let preview_to_json (p : split_preview) =
  `Assoc
    [
      ( "source_principal_id",
        `String (P.principal_id_to_string p.source_principal_id) );
      ("actor_key", `String p.actor_key);
      ("ownership", ownership_to_json p.ownership);
      ("accounts_retained", string_list_to_json p.accounts_retained);
      ("accounts_to_rebind", string_list_to_json p.accounts_to_rebind);
      ("preferences_retained", string_list_to_json p.preferences_retained);
      ("preferences_to_rebind", string_list_to_json p.preferences_to_rebind);
      ("pending_auth_to_invalidate", `Int p.pending_auth_to_invalidate);
      ("leases_to_invalidate", `Int p.leases_to_invalidate);
      ("hard_conflicts", `List (List.map conflict_to_json p.hard_conflicts));
      ("notes", string_list_to_json p.notes);
    ]

let preview_of_json = function
  | `Assoc _ as j -> (
      match
        ( Yojson.Safe.Util.member "source_principal_id" j,
          Yojson.Safe.Util.member "actor_key" j )
      with
      | `String pid_s, `String actor_key -> (
          match P.principal_id_of_string pid_s with
          | Error e -> Error e
          | Ok source_principal_id -> (
              match
                ownership_of_json (Yojson.Safe.Util.member "ownership" j)
              with
              | Error e -> Error e
              | Ok ownership ->
                  let sl key =
                    match
                      string_list_of_json (Yojson.Safe.Util.member key j)
                    with
                    | Ok xs -> xs
                    | Error _ -> []
                  in
                  let conflicts =
                    match Yojson.Safe.Util.member "hard_conflicts" j with
                    | `List xs ->
                        List.filter_map
                          (fun x ->
                            match conflict_of_json x with
                            | Ok c -> Some c
                            | Error _ -> None)
                          xs
                    | _ -> []
                  in
                  let int_field key =
                    match Yojson.Safe.Util.member key j with
                    | `Int n -> n
                    | _ -> 0
                  in
                  Ok
                    {
                      source_principal_id;
                      actor_key;
                      ownership;
                      accounts_retained = sl "accounts_retained";
                      accounts_to_rebind = sl "accounts_to_rebind";
                      preferences_retained = sl "preferences_retained";
                      preferences_to_rebind = sl "preferences_to_rebind";
                      pending_auth_to_invalidate =
                        int_field "pending_auth_to_invalidate";
                      leases_to_invalidate = int_field "leases_to_invalidate";
                      hard_conflicts = conflicts;
                      notes = sl "notes";
                    }))
      | _ -> Error "preview missing source_principal_id or actor_key")
  | _ -> Error "preview must be object"

let sort_assoc_keys = function
  | `Assoc fields ->
      `Assoc (List.sort (fun (a, _) (b, _) -> String.compare a b) fields)
  | other -> other

let plan_canonical_body (plan : split_plan) =
  sort_assoc_keys
    (`Assoc
       [
         ("version", `Int plan.version);
         ("id", `String plan.id);
         ( "source_principal_id",
           `String (P.principal_id_to_string plan.source_principal_id) );
         ("source_revision", `Int plan.source_revision);
         ("actor_key", P.connector_actor_key_to_json plan.actor_key);
         ("actor_revision", `Int plan.actor_revision);
         ("ownership", ownership_to_json plan.ownership);
         ( "admin_principal_id",
           match plan.admin_principal_id with
           | None -> `Null
           | Some id -> `String (P.principal_id_to_string id) );
         ("preview", preview_to_json plan.preview);
         ("created_at", `String plan.created_at);
         ("expires_at", `String plan.expires_at);
       ])

let compute_plan_digest (plan : split_plan) =
  digest_hex (Yojson.Safe.to_string (plan_canonical_body plan))

(* -------------------------------------------------------------------------- *)
(* Schema                                                                     *)
(* -------------------------------------------------------------------------- *)

let ensure_schema db =
  M.ensure_schema db;
  let plans =
    {|CREATE TABLE IF NOT EXISTS principal_split_plans (
      id TEXT PRIMARY KEY NOT NULL,
      version INTEGER NOT NULL,
      source_principal_id TEXT NOT NULL,
      source_revision INTEGER NOT NULL,
      actor_key_json TEXT NOT NULL,
      actor_identity_key TEXT NOT NULL,
      actor_revision INTEGER NOT NULL,
      ownership_json TEXT NOT NULL,
      admin_principal_id TEXT,
      preview_json TEXT NOT NULL,
      digest TEXT NOT NULL,
      status TEXT NOT NULL,
      created_at TEXT NOT NULL,
      expires_at TEXT NOT NULL,
      confirmed_at TEXT,
      applied_at TEXT,
      reject_reason TEXT,
      new_principal_id TEXT
    )|}
  in
  let receipts =
    {|CREATE TABLE IF NOT EXISTS principal_unlink_receipts (
      id TEXT PRIMARY KEY NOT NULL,
      plan_id TEXT NOT NULL UNIQUE,
      source_principal_id TEXT NOT NULL,
      new_principal_id TEXT NOT NULL,
      actor_key TEXT NOT NULL,
      unlinked_link_id TEXT,
      new_link_id TEXT NOT NULL,
      rebound_account_ids_json TEXT NOT NULL,
      rebound_preference_keys_json TEXT NOT NULL,
      pending_auth_invalidated INTEGER NOT NULL,
      leases_invalidated INTEGER NOT NULL,
      actor_snapshot_ids_json TEXT NOT NULL,
      source_revision_after INTEGER NOT NULL,
      new_principal_revision INTEGER NOT NULL,
      actor_revision_after INTEGER NOT NULL,
      applied_at TEXT NOT NULL,
      notes_json TEXT NOT NULL
    )|}
  in
  let leases =
    {|CREATE TABLE IF NOT EXISTS principal_account_leases (
      id TEXT PRIMARY KEY NOT NULL,
      principal_id TEXT NOT NULL,
      account_id TEXT,
      actor_key TEXT,
      status TEXT NOT NULL,
      revision INTEGER NOT NULL,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    )|}
  in
  let idx_lease_principal =
    {|CREATE INDEX IF NOT EXISTS idx_principal_account_leases_principal
      ON principal_account_leases(principal_id)|}
  in
  let idx_lease_actor =
    {|CREATE INDEX IF NOT EXISTS idx_principal_account_leases_actor
      ON principal_account_leases(actor_key)|}
  in
  List.iter (exec_schema db)
    [ plans; receipts; leases; idx_lease_principal; idx_lease_actor ]

(* -------------------------------------------------------------------------- *)
(* Account leases CRUD                                                        *)
(* -------------------------------------------------------------------------- *)

let put_account_lease ~db ?(now = Unix.gettimeofday ()) (l : account_lease) =
  let now_s = iso_now ~now () in
  let id = if String.trim l.id = "" then generate_lease_id ~now () else l.id in
  let created_at =
    if String.trim l.created_at = "" then now_s else l.created_at
  in
  let l = { l with id; created_at; updated_at = now_s } in
  let sql =
    {|INSERT INTO principal_account_leases
      (id, principal_id, account_id, actor_key, status, revision, created_at, updated_at)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?)
      ON CONFLICT(id) DO UPDATE SET
        principal_id = excluded.principal_id,
        account_id = excluded.account_id,
        actor_key = excluded.actor_key,
        status = excluded.status,
        revision = excluded.revision,
        updated_at = excluded.updated_at|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let bind i v = ignore (Sqlite3.bind stmt i v) in
      bind 1 (Sqlite3.Data.TEXT l.id);
      bind 2 (Sqlite3.Data.TEXT (P.principal_id_to_string l.principal_id));
      bind 3
        (match l.account_id with
        | None -> Sqlite3.Data.NULL
        | Some s -> Sqlite3.Data.TEXT s);
      bind 4
        (match l.actor_key with
        | None -> Sqlite3.Data.NULL
        | Some s -> Sqlite3.Data.TEXT s);
      bind 5 (Sqlite3.Data.TEXT (string_of_lease_status l.status));
      bind 6 (Sqlite3.Data.INT (Int64.of_int l.revision));
      bind 7 (Sqlite3.Data.TEXT l.created_at);
      bind 8 (Sqlite3.Data.TEXT l.updated_at);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok l
      | rc ->
          Error
            (Printf.sprintf "put_account_lease failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let lease_of_stmt stmt =
  let id = text_col stmt 0 in
  let pid_s = text_col stmt 1 in
  let account_id = opt_text_col stmt 2 in
  let actor_key = opt_text_col stmt 3 in
  let status_s = text_col stmt 4 in
  let revision = int_col stmt 5 in
  let created_at = text_col stmt 6 in
  let updated_at = text_col stmt 7 in
  match (P.principal_id_of_string pid_s, lease_status_of_string status_s) with
  | Error e, _ | _, Error e -> Error e
  | Ok principal_id, Ok status ->
      Ok
        {
          id;
          principal_id;
          account_id;
          actor_key;
          status;
          revision;
          created_at;
          updated_at;
        }

let list_account_leases ~db ~principal_id =
  let sql =
    {|SELECT id, principal_id, account_id, actor_key, status, revision,
             created_at, updated_at
      FROM principal_account_leases WHERE principal_id = ?
      ORDER BY id|}
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
            match lease_of_stmt stmt with
            | Ok l -> loop (l :: acc)
            | Error e -> Error e)
        | Sqlite3.Rc.DONE -> Ok (List.rev acc)
        | rc ->
            Error
              (Printf.sprintf "list_account_leases failed: %s (%s)"
                 (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))
      in
      loop [])

let list_account_leases_for_actor ~db ~actor_key =
  let sql =
    {|SELECT id, principal_id, account_id, actor_key, status, revision,
             created_at, updated_at
      FROM principal_account_leases WHERE actor_key = ?
      ORDER BY id|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT actor_key));
      let rec loop acc =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> (
            match lease_of_stmt stmt with
            | Ok l -> loop (l :: acc)
            | Error e -> Error e)
        | Sqlite3.Rc.DONE -> Ok (List.rev acc)
        | rc ->
            Error
              (Printf.sprintf "list_account_leases_for_actor failed: %s (%s)"
                 (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))
      in
      loop [])

let invalidate_leases ~db ~principal_id ~actor_key ~now_s =
  (* Actor-scoped active leases → Invalidated; other active on principal →
     Rebind_required (explicit rebind needed; no silent authority transfer). *)
  let leases =
    match list_account_leases ~db ~principal_id with
    | Error e -> Error e
    | Ok xs -> Ok xs
  in
  match leases with
  | Error e -> Error e
  | Ok xs ->
      let rec go count = function
        | [] -> Ok count
        | (l : account_lease) :: rest -> (
            if l.status <> Active then go count rest
            else
              let status =
                match l.actor_key with
                | Some k when String.equal k actor_key -> Invalidated
                | _ -> Rebind_required
              in
              let updated =
                { l with status; revision = l.revision + 1; updated_at = now_s }
              in
              match put_account_lease ~db updated with
              | Error e -> Error e
              | Ok _ -> go (count + 1) rest)
      in
      go 0 xs

(* -------------------------------------------------------------------------- *)
(* Plan / receipt persistence                                                 *)
(* -------------------------------------------------------------------------- *)

let insert_split_plan ~db (plan : split_plan) =
  let sql =
    {|INSERT INTO principal_split_plans
      (id, version, source_principal_id, source_revision, actor_key_json,
       actor_identity_key, actor_revision, ownership_json, admin_principal_id,
       preview_json, digest, status, created_at, expires_at, confirmed_at,
       applied_at, reject_reason, new_principal_id)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let bind i v = ignore (Sqlite3.bind stmt i v) in
      bind 1 (Sqlite3.Data.TEXT plan.id);
      bind 2 (Sqlite3.Data.INT (Int64.of_int plan.version));
      bind 3
        (Sqlite3.Data.TEXT (P.principal_id_to_string plan.source_principal_id));
      bind 4 (Sqlite3.Data.INT (Int64.of_int plan.source_revision));
      bind 5
        (Sqlite3.Data.TEXT
           (Yojson.Safe.to_string
              (P.connector_actor_key_to_json plan.actor_key)));
      bind 6 (Sqlite3.Data.TEXT (P.actor_identity_key plan.actor_key));
      bind 7 (Sqlite3.Data.INT (Int64.of_int plan.actor_revision));
      bind 8
        (Sqlite3.Data.TEXT
           (Yojson.Safe.to_string (ownership_to_json plan.ownership)));
      bind 9
        (match plan.admin_principal_id with
        | None -> Sqlite3.Data.NULL
        | Some id -> Sqlite3.Data.TEXT (P.principal_id_to_string id));
      bind 10
        (Sqlite3.Data.TEXT
           (Yojson.Safe.to_string (preview_to_json plan.preview)));
      bind 11 (Sqlite3.Data.TEXT plan.digest);
      bind 12 (Sqlite3.Data.TEXT (string_of_plan_status plan.status));
      bind 13 (Sqlite3.Data.TEXT plan.created_at);
      bind 14 (Sqlite3.Data.TEXT plan.expires_at);
      bind 15
        (match plan.confirmed_at with
        | None -> Sqlite3.Data.NULL
        | Some s -> Sqlite3.Data.TEXT s);
      bind 16
        (match plan.applied_at with
        | None -> Sqlite3.Data.NULL
        | Some s -> Sqlite3.Data.TEXT s);
      bind 17
        (match plan.reject_reason with
        | None -> Sqlite3.Data.NULL
        | Some s -> Sqlite3.Data.TEXT s);
      bind 18
        (match plan.new_principal_id with
        | None -> Sqlite3.Data.NULL
        | Some id -> Sqlite3.Data.TEXT (P.principal_id_to_string id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok plan
      | Sqlite3.Rc.CONSTRAINT ->
          Error (Printf.sprintf "split plan id already exists: %s" plan.id)
      | rc ->
          Error
            (Printf.sprintf "insert_split_plan failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let update_split_plan ~db (plan : split_plan) =
  let sql =
    {|UPDATE principal_split_plans SET
      status = ?, confirmed_at = ?, applied_at = ?, reject_reason = ?,
      new_principal_id = ?, digest = ?
      WHERE id = ?|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let bind i v = ignore (Sqlite3.bind stmt i v) in
      bind 1 (Sqlite3.Data.TEXT (string_of_plan_status plan.status));
      bind 2
        (match plan.confirmed_at with
        | None -> Sqlite3.Data.NULL
        | Some s -> Sqlite3.Data.TEXT s);
      bind 3
        (match plan.applied_at with
        | None -> Sqlite3.Data.NULL
        | Some s -> Sqlite3.Data.TEXT s);
      bind 4
        (match plan.reject_reason with
        | None -> Sqlite3.Data.NULL
        | Some s -> Sqlite3.Data.TEXT s);
      bind 5
        (match plan.new_principal_id with
        | None -> Sqlite3.Data.NULL
        | Some id -> Sqlite3.Data.TEXT (P.principal_id_to_string id));
      bind 6 (Sqlite3.Data.TEXT plan.digest);
      bind 7 (Sqlite3.Data.TEXT plan.id);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok plan
      | rc ->
          Error
            (Printf.sprintf "update_split_plan failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let plan_of_stmt stmt =
  let id = text_col stmt 0 in
  let version = int_col stmt 1 in
  let source_s = text_col stmt 2 in
  let source_revision = int_col stmt 3 in
  let actor_key_json = text_col stmt 4 in
  let actor_revision = int_col stmt 6 in
  let ownership_json = text_col stmt 7 in
  let admin_s = opt_text_col stmt 8 in
  let preview_json = text_col stmt 9 in
  let digest = text_col stmt 10 in
  let status_s = text_col stmt 11 in
  let created_at = text_col stmt 12 in
  let expires_at = text_col stmt 13 in
  let confirmed_at = opt_text_col stmt 14 in
  let applied_at = opt_text_col stmt 15 in
  let reject_reason = opt_text_col stmt 16 in
  let new_pid_s = opt_text_col stmt 17 in
  match P.principal_id_of_string source_s with
  | Error e -> Error e
  | Ok source_principal_id -> (
      match
        P.connector_actor_key_of_json (Yojson.Safe.from_string actor_key_json)
      with
      | Error e -> Error e
      | Ok actor_key -> (
          match ownership_of_json (Yojson.Safe.from_string ownership_json) with
          | Error e -> Error e
          | Ok ownership -> (
              match preview_of_json (Yojson.Safe.from_string preview_json) with
              | Error e -> Error e
              | Ok preview -> (
                  match plan_status_of_string status_s with
                  | Error e -> Error e
                  | Ok status -> (
                      let admin_principal_id =
                        match admin_s with
                        | None -> Ok None
                        | Some s -> (
                            match P.principal_id_of_string s with
                            | Ok id -> Ok (Some id)
                            | Error e -> Error e)
                      in
                      let new_principal_id =
                        match new_pid_s with
                        | None -> Ok None
                        | Some s -> (
                            match P.principal_id_of_string s with
                            | Ok id -> Ok (Some id)
                            | Error e -> Error e)
                      in
                      match (admin_principal_id, new_principal_id) with
                      | Error e, _ | _, Error e -> Error e
                      | Ok admin_principal_id, Ok new_principal_id ->
                          Ok
                            {
                              version;
                              id;
                              source_principal_id;
                              source_revision;
                              actor_key;
                              actor_revision;
                              ownership;
                              admin_principal_id;
                              preview;
                              digest;
                              status;
                              created_at;
                              expires_at;
                              confirmed_at;
                              applied_at;
                              reject_reason;
                              new_principal_id;
                            })))))

let get_split_plan ~db ~id =
  let sql =
    {|SELECT id, version, source_principal_id, source_revision, actor_key_json,
             actor_identity_key, actor_revision, ownership_json,
             admin_principal_id, preview_json, digest, status, created_at,
             expires_at, confirmed_at, applied_at, reject_reason,
             new_principal_id
      FROM principal_split_plans WHERE id = ?|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match plan_of_stmt stmt with
          | Ok p -> Ok (Some p)
          | Error e -> Error e)
      | Sqlite3.Rc.DONE -> Ok None
      | rc ->
          Error
            (Printf.sprintf "get_split_plan failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let receipt_to_json_lists r =
  ( Yojson.Safe.to_string (string_list_to_json r.rebound_account_ids),
    Yojson.Safe.to_string (string_list_to_json r.rebound_preference_keys),
    Yojson.Safe.to_string (string_list_to_json r.actor_snapshot_ids),
    Yojson.Safe.to_string (string_list_to_json r.notes) )

let insert_unlink_receipt ~db (r : unlink_receipt) =
  let acc_j, pref_j, snap_j, notes_j = receipt_to_json_lists r in
  let sql =
    {|INSERT INTO principal_unlink_receipts
      (id, plan_id, source_principal_id, new_principal_id, actor_key,
       unlinked_link_id, new_link_id, rebound_account_ids_json,
       rebound_preference_keys_json, pending_auth_invalidated,
       leases_invalidated, actor_snapshot_ids_json, source_revision_after,
       new_principal_revision, actor_revision_after, applied_at, notes_json)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let bind i v = ignore (Sqlite3.bind stmt i v) in
      bind 1 (Sqlite3.Data.TEXT r.id);
      bind 2 (Sqlite3.Data.TEXT r.plan_id);
      bind 3
        (Sqlite3.Data.TEXT (P.principal_id_to_string r.source_principal_id));
      bind 4 (Sqlite3.Data.TEXT (P.principal_id_to_string r.new_principal_id));
      bind 5 (Sqlite3.Data.TEXT r.actor_key);
      bind 6
        (match r.unlinked_link_id with
        | None -> Sqlite3.Data.NULL
        | Some s -> Sqlite3.Data.TEXT s);
      bind 7 (Sqlite3.Data.TEXT r.new_link_id);
      bind 8 (Sqlite3.Data.TEXT acc_j);
      bind 9 (Sqlite3.Data.TEXT pref_j);
      bind 10 (Sqlite3.Data.INT (Int64.of_int r.pending_auth_invalidated));
      bind 11 (Sqlite3.Data.INT (Int64.of_int r.leases_invalidated));
      bind 12 (Sqlite3.Data.TEXT snap_j);
      bind 13 (Sqlite3.Data.INT (Int64.of_int r.source_revision_after));
      bind 14 (Sqlite3.Data.INT (Int64.of_int r.new_principal_revision));
      bind 15 (Sqlite3.Data.INT (Int64.of_int r.actor_revision_after));
      bind 16 (Sqlite3.Data.TEXT r.applied_at);
      bind 17 (Sqlite3.Data.TEXT notes_j);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok r
      | Sqlite3.Rc.CONSTRAINT ->
          Error
            (Printf.sprintf "unlink receipt already exists for plan_id=%s"
               r.plan_id)
      | rc ->
          Error
            (Printf.sprintf "insert_unlink_receipt failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let receipt_of_stmt stmt =
  let id = text_col stmt 0 in
  let plan_id = text_col stmt 1 in
  let source_s = text_col stmt 2 in
  let new_s = text_col stmt 3 in
  let actor_key = text_col stmt 4 in
  let unlinked_link_id = opt_text_col stmt 5 in
  let new_link_id = text_col stmt 6 in
  let acc_j = text_col stmt 7 in
  let pref_j = text_col stmt 8 in
  let pending = int_col stmt 9 in
  let leases = int_col stmt 10 in
  let snap_j = text_col stmt 11 in
  let source_rev = int_col stmt 12 in
  let new_rev = int_col stmt 13 in
  let actor_rev = int_col stmt 14 in
  let applied_at = text_col stmt 15 in
  let notes_j = text_col stmt 16 in
  let sl s =
    match string_list_of_json (Yojson.Safe.from_string s) with
    | Ok xs -> xs
    | Error _ -> []
  in
  match (P.principal_id_of_string source_s, P.principal_id_of_string new_s) with
  | Error e, _ | _, Error e -> Error e
  | Ok source_principal_id, Ok new_principal_id ->
      Ok
        {
          id;
          plan_id;
          source_principal_id;
          new_principal_id;
          actor_key;
          unlinked_link_id;
          new_link_id;
          rebound_account_ids = sl acc_j;
          rebound_preference_keys = sl pref_j;
          pending_auth_invalidated = pending;
          leases_invalidated = leases;
          actor_snapshot_ids = sl snap_j;
          source_revision_after = source_rev;
          new_principal_revision = new_rev;
          actor_revision_after = actor_rev;
          applied_at;
          notes = sl notes_j;
        }

let get_unlink_receipt ~db ~id =
  let sql =
    {|SELECT id, plan_id, source_principal_id, new_principal_id, actor_key,
             unlinked_link_id, new_link_id, rebound_account_ids_json,
             rebound_preference_keys_json, pending_auth_invalidated,
             leases_invalidated, actor_snapshot_ids_json, source_revision_after,
             new_principal_revision, actor_revision_after, applied_at, notes_json
      FROM principal_unlink_receipts WHERE id = ?|}
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
            (Printf.sprintf "get_unlink_receipt failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let get_unlink_receipt_by_plan ~db ~plan_id =
  let sql =
    {|SELECT id, plan_id, source_principal_id, new_principal_id, actor_key,
             unlinked_link_id, new_link_id, rebound_account_ids_json,
             rebound_preference_keys_json, pending_auth_invalidated,
             leases_invalidated, actor_snapshot_ids_json, source_revision_after,
             new_principal_revision, actor_revision_after, applied_at, notes_json
      FROM principal_unlink_receipts WHERE plan_id = ?|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT plan_id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match receipt_of_stmt stmt with
          | Ok r -> Ok (Some r)
          | Error e -> Error e)
      | Sqlite3.Rc.DONE -> Ok None
      | rc ->
          Error
            (Printf.sprintf "get_unlink_receipt_by_plan failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

(* -------------------------------------------------------------------------- *)
(* Preview                                                                    *)
(* -------------------------------------------------------------------------- *)

let count_active_leases leases =
  List.fold_left
    (fun n (l : account_lease) -> if l.status = Active then n + 1 else n)
    0 leases

let build_preview ~db ~source_principal_id ~actor_key
    ?(ownership = Retain_on_source) () =
  let actor_ikey = P.actor_identity_key actor_key in
  match S.get_principal ~db ~id:source_principal_id with
  | Error e -> Error e
  | Ok None ->
      Error
        (Printf.sprintf "source principal not found: %s"
           (P.principal_id_to_string source_principal_id))
  | Ok (Some source) -> (
      match source.lifecycle with
      | P.Disabled ->
          Error
            (Printf.sprintf "source principal %s is disabled"
               (P.principal_id_to_string source_principal_id))
      | P.Merged_into t ->
          Error
            (Printf.sprintf
               "source principal %s is merged_into %s (tombstone cannot own \
                actors; reverse merge is forbidden — use unlink on the \
                survivor)"
               (P.principal_id_to_string source_principal_id)
               (P.principal_id_to_string t))
      | P.Active -> (
          match S.get_connector_actor ~db ~key:actor_key with
          | Error e -> Error e
          | Ok None ->
              Error (Printf.sprintf "connector actor not found: %s" actor_ikey)
          | Ok (Some actor) -> (
              if
                not
                  (P.principal_id_equal actor.principal_id source_principal_id)
              then
                Error
                  (Printf.sprintf "actor %s is owned by %s, not source %s"
                     actor_ikey
                     (P.principal_id_to_string actor.principal_id)
                     (P.principal_id_to_string source_principal_id))
              else
                match
                  ( M.list_external_accounts ~db
                      ~principal_id:source_principal_id,
                    M.list_preferences ~db ~principal_id:source_principal_id,
                    M.get_pending_authorization_count ~db
                      ~principal_id:source_principal_id,
                    list_account_leases ~db ~principal_id:source_principal_id,
                    Github_account_binding.list_for_principal ~db
                      ~principal_id:source_principal_id )
                with
                | Error e, _, _, _, _
                | _, Error e, _, _, _
                | _, _, Error e, _, _
                | _, _, _, Error e, _
                | _, _, _, _, Error e ->
                    Error e
                | Ok accounts, Ok prefs, Ok pending, Ok leases, Ok gh_bindings
                  ->
                    let all_acc_ids =
                      List.map (fun (a : M.external_account) -> a.id) accounts
                    in
                    let all_pref_keys =
                      List.map (fun (p : M.preference) -> p.key) prefs
                    in
                    let gh_binding_ids =
                      List.map
                        (fun (b : Github_account_binding.binding) -> b.id)
                        gh_bindings
                    in
                    let rebind_acc, rebind_pref, conflicts =
                      match ownership with
                      | Retain_on_source -> ([], [], [])
                      | Explicit_rebind { account_ids; preference_keys } ->
                          let acc_conflicts =
                            List.filter_map
                              (fun aid ->
                                if List.exists (String.equal aid) all_acc_ids
                                then None
                                else
                                  Some
                                    (Account_not_owned
                                       {
                                         account_id = aid;
                                         summary =
                                           Printf.sprintf
                                             "account %s is not owned by \
                                              source principal %s"
                                             aid
                                             (P.principal_id_to_string
                                                source_principal_id);
                                       }))
                              account_ids
                          in
                          let pref_conflicts =
                            List.filter_map
                              (fun k ->
                                if List.exists (String.equal k) all_pref_keys
                                then None
                                else
                                  Some
                                    (Preference_not_owned
                                       {
                                         key = k;
                                         summary =
                                           Printf.sprintf
                                             "preference %s is not owned by \
                                              source principal %s"
                                             k
                                             (P.principal_id_to_string
                                                source_principal_id);
                                       }))
                              preference_keys
                          in
                          (* P21.M1.E2.T002: refuse any attempt to rebind a
                             live GitHub account binding on split. *)
                          let gh_requested =
                            List.filter
                              (fun id ->
                                List.exists (String.equal id) gh_binding_ids)
                              account_ids
                          in
                          let gh_conflicts =
                            match
                              Github_account_ownership_policy
                              .evaluate_split_ownership ~db ~source_principal_id
                                ~requested_binding_ids:gh_requested ()
                            with
                            | Ok
                                (Github_account_ownership_policy.Split_refuse
                                   { conflicts = cs; _ }) ->
                                List.map
                                  (fun (c :
                                         Github_account_ownership_policy
                                         .split_conflict) ->
                                    Other
                                      {
                                        code = "github_binding_split_refuse";
                                        summary = c.summary;
                                      })
                                  cs
                            | Ok (Github_account_ownership_policy.Split_ok _)
                            | Error _ ->
                                []
                          in
                          ( account_ids,
                            preference_keys,
                            acc_conflicts @ pref_conflicts @ gh_conflicts )
                    in
                    let retained_acc =
                      List.filter
                        (fun id ->
                          not (List.exists (String.equal id) rebind_acc))
                        all_acc_ids
                    in
                    let retained_pref =
                      List.filter
                        (fun k ->
                          not (List.exists (String.equal k) rebind_pref))
                        all_pref_keys
                    in
                    let lease_n = count_active_leases leases in
                    Ok
                      {
                        source_principal_id;
                        actor_key = actor_ikey;
                        ownership;
                        accounts_retained = retained_acc;
                        accounts_to_rebind = rebind_acc;
                        preferences_retained = retained_pref;
                        preferences_to_rebind = rebind_pref;
                        pending_auth_to_invalidate = pending;
                        leases_to_invalidate = lease_n;
                        hard_conflicts = conflicts;
                        notes =
                          [
                            "unlink is identity split, not reverse merge";
                            "new principal starts empty unless explicit rebind";
                            "pending auth and account leases are invalidated";
                            "historical actor snapshots remain immutable";
                            Printf.sprintf
                              "github_bindings_retained_on_source=%d"
                              (List.length gh_binding_ids);
                          ];
                      })))

let preview_unlink ~db ~source_principal_id ~actor_key
    ?(ownership = Retain_on_source) () =
  build_preview ~db ~source_principal_id ~actor_key ~ownership ()

(* -------------------------------------------------------------------------- *)
(* Plan lifecycle                                                             *)
(* -------------------------------------------------------------------------- *)

let make_split_plan ~db ~id ~source_principal_id ~actor_key
    ?(ownership = Retain_on_source) ?admin_principal_id
    ?(ttl_seconds = default_plan_ttl_seconds) ?(now = Unix.gettimeofday ()) () =
  if String.trim id = "" then Error "split plan id must be non-empty"
  else if ttl_seconds <= 0. then Error "ttl_seconds must be positive"
  else
    match S.get_principal ~db ~id:source_principal_id with
    | Error e -> Error e
    | Ok None ->
        Error
          (Printf.sprintf "source principal not found: %s"
             (P.principal_id_to_string source_principal_id))
    | Ok (Some source) -> (
        match S.get_connector_actor ~db ~key:actor_key with
        | Error e -> Error e
        | Ok None ->
            Error
              (Printf.sprintf "connector actor not found: %s"
                 (P.actor_identity_key actor_key))
        | Ok (Some actor) -> (
            match
              build_preview ~db ~source_principal_id ~actor_key ~ownership ()
            with
            | Error e -> Error e
            | Ok preview when preview.hard_conflicts <> [] ->
                Error
                  (Printf.sprintf
                     "ownership conflicts refuse split plan (%d conflict(s))"
                     (List.length preview.hard_conflicts))
            | Ok preview ->
                let created_at = iso_now ~now () in
                let expires_at = iso_now ~now:(now +. ttl_seconds) () in
                let draft =
                  {
                    version = protocol_version;
                    id;
                    source_principal_id;
                    source_revision = source.revision;
                    actor_key;
                    actor_revision = actor.revision;
                    ownership;
                    admin_principal_id;
                    preview;
                    digest = "";
                    status = Planned;
                    created_at;
                    expires_at;
                    confirmed_at = None;
                    applied_at = None;
                    reject_reason = None;
                    new_principal_id = None;
                  }
                in
                let plan = { draft with digest = compute_plan_digest draft } in
                insert_split_plan ~db plan))

let plan_is_expired ?(now = Unix.gettimeofday ()) (plan : split_plan) =
  let now_s = iso_now ~now () in
  String.compare now_s plan.expires_at > 0

let digests_equal a b =
  String.length a = String.length b
  &&
  let acc = ref 0 in
  for i = 0 to String.length a - 1 do
    acc := !acc lor (Char.code a.[i] lxor Char.code b.[i])
  done;
  !acc = 0

let confirm_split_plan ~db ~id ~presented_digest ?confirming_principal
    ?(now = Unix.gettimeofday ()) () =
  match get_split_plan ~db ~id with
  | Error e -> Error e
  | Ok None -> Error (Printf.sprintf "split plan not found: %s" id)
  | Ok (Some plan) -> (
      if plan_status_is_terminal plan.status && plan.status <> Confirmed then
        Error
          (Printf.sprintf "split plan %s is terminal (%s)" id
             (string_of_plan_status plan.status))
      else if plan.status = Confirmed then Ok plan
      else if plan.status <> Planned then
        Error
          (Printf.sprintf "split plan %s cannot be confirmed from %s" id
             (string_of_plan_status plan.status))
      else if plan_is_expired ~now plan then (
        let plan =
          {
            plan with
            status = Expired;
            reject_reason = Some "plan expired before confirm";
          }
        in
        ignore (update_split_plan ~db plan);
        Error "split plan expired")
      else if not (digests_equal presented_digest plan.digest) then
        Error "split plan digest mismatch on confirm"
      else
        match (plan.admin_principal_id, confirming_principal) with
        | Some admin, None ->
            Error "admin split plan requires confirming_principal"
        | Some admin, Some conf when not (P.principal_id_equal admin conf) ->
            Error "confirming principal does not match admin_principal_id"
        | _ ->
            let plan =
              {
                plan with
                status = Confirmed;
                confirmed_at = Some (iso_now ~now ());
              }
            in
            update_split_plan ~db plan)

let cancel_split_plan ~db ~id ?(now = Unix.gettimeofday ()) () =
  match get_split_plan ~db ~id with
  | Error e -> Error e
  | Ok None -> Error (Printf.sprintf "split plan not found: %s" id)
  | Ok (Some plan) ->
      if plan_status_is_terminal plan.status then
        Error
          (Printf.sprintf "split plan %s is already terminal (%s)" id
             (string_of_plan_status plan.status))
      else
        let plan =
          {
            plan with
            status = Cancelled;
            reject_reason = Some "cancelled";
            confirmed_at = plan.confirmed_at;
          }
        in
        update_split_plan ~db plan

(* -------------------------------------------------------------------------- *)
(* Apply core                                                                 *)
(* -------------------------------------------------------------------------- *)

let check_expected ~label ~expected ~actual =
  match expected with
  | Some exp when exp <> actual ->
      Error
        (Printf.sprintf
           "revision conflict for %s: expected %d, found %d (concurrent \
            unlink/split CAS fail closed)"
           label exp actual)
  | _ -> Ok ()

let unlink_active_link ~db ~actor_key ~now ~now_s =
  match S.get_active_identity_link ~db ~key:actor_key with
  | Error e -> Error (Tx_msg e)
  | Ok None -> Ok None
  | Ok (Some link) -> (
      match
        S.update_identity_link ~db ~id:link.id ~expected_revision:link.revision
          ~status:P.Unlinked ~unlinked_at:(Some now_s) ~now ()
      with
      | Ok l -> Ok (Some l.id)
      | Error e when is_revision_conflict e -> Error (Tx_stale e)
      | Error e -> Error (Tx_msg e))

let rebind_accounts ~db ~account_ids ~to_principal ~now_s =
  let rec go done_ids = function
    | [] -> Ok (List.rev done_ids)
    | aid :: rest -> (
        match
          Principal_merge_persist.reassign_external_account ~db ~id:aid
            ~to_principal ~now_s
        with
        | Error e -> Error e
        | Ok () -> go (aid :: done_ids) rest)
  in
  go [] account_ids

let rebind_prefs ~db ~preference_keys ~from_principal ~to_principal ~now =
  let rec go done_keys = function
    | [] -> Ok (List.rev done_keys)
    | k :: rest -> (
        match M.list_preferences ~db ~principal_id:from_principal with
        | Error e -> Error e
        | Ok prefs -> (
            match List.find_opt (fun (p : M.preference) -> p.key = k) prefs with
            | None -> Error (Printf.sprintf "preference %s vanished" k)
            | Some pref -> (
                match
                  M.put_preference ~db ~now ~principal_id:to_principal ~key:k
                    ~value:pref.value ()
                with
                | Error e -> Error e
                | Ok _ -> (
                    match
                      Principal_merge_persist.delete_preference ~db
                        ~principal_id:from_principal ~key:k
                    with
                    | Error e -> Error e
                    | Ok () -> go (k :: done_keys) rest))))
  in
  go [] preference_keys

let map_tx_rev = function
  | Error e when is_revision_conflict e -> Error (Tx_stale e)
  | Error e -> Error (Tx_msg e)
  | Ok v -> Ok v

let map_tx = function Error e -> Error (Tx_msg e) | Ok v -> Ok v

let apply_split_in_tx ~db ~(plan : split_plan) ~now ~unlink_id =
  let now_s = iso_now ~now () in
  let actor_ikey = P.actor_identity_key plan.actor_key in
  let ( let* ) = Result.bind in
  let* source =
    match S.get_principal ~db ~id:plan.source_principal_id with
    | Error e -> Error (Tx_msg e)
    | Ok None -> Error (Tx_msg "source principal disappeared under lock")
    | Ok (Some s) -> Ok s
  in
  let* () =
    match
      check_expected ~label:"source_principal"
        ~expected:(Some plan.source_revision) ~actual:source.revision
    with
    | Error e -> Error (Tx_stale e)
    | Ok () -> Ok ()
  in
  let* actor =
    match S.get_connector_actor ~db ~key:plan.actor_key with
    | Error e -> Error (Tx_msg e)
    | Ok None -> Error (Tx_msg "actor disappeared under lock")
    | Ok (Some a) -> Ok a
  in
  let* () =
    match
      check_expected ~label:"actor" ~expected:(Some plan.actor_revision)
        ~actual:actor.revision
    with
    | Error e -> Error (Tx_stale e)
    | Ok () -> Ok ()
  in
  let* () =
    if P.principal_id_equal actor.principal_id plan.source_principal_id then
      Ok ()
    else
      Error
        (Tx_stale
           (Printf.sprintf "actor ownership changed under lock: now %s"
              (P.principal_id_to_string actor.principal_id)))
  in
  let snap_id = Principal_merge_persist.generate_snapshot_id ~now () in
  let snap : Principal_merge_persist.actor_snapshot =
    {
      id = snap_id;
      actor_key = actor_ikey;
      principal_id_at_snapshot = actor.principal_id;
      actor_json = Yojson.Safe.to_string (P.connector_actor_to_json actor);
      reason = "pre_unlink";
      merge_id = Some plan.id;
      created_at = now_s;
    }
  in
  let* _ = map_tx (Principal_merge_persist.insert_actor_snapshot ~db snap) in
  let* new_pid =
    match P.principal_id_of_string (generate_principal_id ~now ()) with
    | Error e -> Error (Tx_msg e)
    | Ok id -> Ok id
  in
  let new_principal =
    P.make_principal ~id:new_pid ~revision:1 ~created_at:now_s ~updated_at:now_s
      ()
  in
  let* new_principal = map_tx (S.insert_principal ~db ~now new_principal) in
  let* unlinked_link_id =
    unlink_active_link ~db ~actor_key:plan.actor_key ~now ~now_s
  in
  let new_link_id = generate_link_id ~now () in
  let new_link =
    P.make_identity_link ~id:new_link_id ~principal_id:new_pid
      ~actor_key:plan.actor_key ~revision:1 ~linked_at:now_s ()
  in
  let* new_link = map_tx (S.insert_identity_link ~db ~now new_link) in
  let* actor_after =
    map_tx_rev
      (S.update_connector_actor ~db ~key:plan.actor_key
         ~expected_revision:actor.revision ~principal_id:new_pid
         ~lifecycle:P.Active ~now ())
  in
  let* rebound_account_ids, rebound_preference_keys =
    match plan.ownership with
    | Retain_on_source -> Ok ([], [])
    | Explicit_rebind { account_ids; preference_keys } -> (
        match rebind_accounts ~db ~account_ids ~to_principal:new_pid ~now_s with
        | Error e -> Error (Tx_msg e)
        | Ok accs -> (
            match
              rebind_prefs ~db ~preference_keys
                ~from_principal:plan.source_principal_id ~to_principal:new_pid
                ~now
            with
            | Error e -> Error (Tx_msg e)
            | Ok prefs -> Ok (accs, prefs)))
  in
  let pending = plan.preview.pending_auth_to_invalidate in
  (* Share the canonical GitHub invalidation lifecycle for Connector
     unlink/split: zero pending auth on source (old lineage fails closed) and
     record a redacted invalidate receipt. Credentials retained on source are
     not destroyed here — only explicit account unlink/revocation does that. *)
  let _inv =
    Github_user_auth_invalidate.invalidate_for_connector_split ~db
      ~source_principal_id:plan.source_principal_id ~actor_key:actor_ikey
      ~related_id:plan.id ~now ()
  in
  if pending > 0 then
    ignore
      (M.set_pending_authorization_count ~db
         ~principal_id:plan.source_principal_id ~count:0);
  ignore (M.set_pending_authorization_count ~db ~principal_id:new_pid ~count:0);
  let* leases_invalidated =
    map_tx
      (invalidate_leases ~db ~principal_id:plan.source_principal_id
         ~actor_key:actor_ikey ~now_s)
  in
  let* source_after =
    map_tx_rev
      (S.update_principal ~db ~id:plan.source_principal_id
         ~expected_revision:source.revision ~now ())
  in
  let receipt =
    {
      id = unlink_id;
      plan_id = plan.id;
      source_principal_id = plan.source_principal_id;
      new_principal_id = new_pid;
      actor_key = actor_ikey;
      unlinked_link_id;
      new_link_id = new_link.id;
      rebound_account_ids;
      rebound_preference_keys;
      pending_auth_invalidated = pending;
      leases_invalidated;
      actor_snapshot_ids = [ snap_id ];
      source_revision_after = source_after.revision;
      new_principal_revision = new_principal.revision;
      actor_revision_after = actor_after.revision;
      applied_at = now_s;
      notes =
        plan.preview.notes
        @ [ "authority revoked on split; new principal has no credentials" ];
    }
  in
  let* receipt = map_tx (insert_unlink_receipt ~db receipt) in
  let plan =
    {
      plan with
      status = Applied;
      applied_at = Some now_s;
      new_principal_id = Some new_pid;
    }
  in
  let* _ = map_tx (update_split_plan ~db plan) in
  Ok receipt

let apply_split_plan ~db ~id ?expected_source_revision ?expected_actor_revision
    ?(now = Unix.gettimeofday ()) () =
  match get_unlink_receipt_by_plan ~db ~plan_id:id with
  | Error e -> Refused { reason = e; conflicts = []; preview = None }
  | Ok (Some r) -> Idempotent r
  | Ok None -> (
      match get_split_plan ~db ~id with
      | Error e -> Refused { reason = e; conflicts = []; preview = None }
      | Ok None ->
          Refused
            {
              reason = Printf.sprintf "split plan not found: %s" id;
              conflicts = [];
              preview = None;
            }
      | Ok (Some plan) when plan.status = Applied -> (
          match get_unlink_receipt_by_plan ~db ~plan_id:id with
          | Ok (Some r) -> Idempotent r
          | _ ->
              Refused
                {
                  reason = "plan marked applied but receipt missing";
                  conflicts = [];
                  preview = Some plan.preview;
                })
      | Ok (Some plan) when plan.status <> Confirmed ->
          Refused
            {
              reason =
                Printf.sprintf "split plan must be confirmed (status=%s)"
                  (string_of_plan_status plan.status);
              conflicts = [];
              preview = Some plan.preview;
            }
      | Ok (Some plan) when plan_is_expired ~now plan ->
          ignore
            (update_split_plan ~db
               {
                 plan with
                 status = Expired;
                 reject_reason = Some "plan expired before apply";
               });
          Refused
            {
              reason = "split plan expired";
              conflicts = [];
              preview = Some plan.preview;
            }
      | Ok (Some plan) when plan.preview.hard_conflicts <> [] ->
          Refused
            {
              reason = "ownership conflicts refuse split apply";
              conflicts = plan.preview.hard_conflicts;
              preview = Some plan.preview;
            }
      | Ok (Some plan) -> (
          match
            ( check_expected ~label:"source_principal"
                ~expected:expected_source_revision ~actual:plan.source_revision,
              check_expected ~label:"actor" ~expected:expected_actor_revision
                ~actual:plan.actor_revision )
          with
          (* expected_* compare against bound plan values when provided; live
             CAS happens inside the transaction against plan.*_revision. *)
          | Error e, _ | _, Error e -> Stale_revision e
          | Ok (), Ok () -> (
              let unlink_id = generate_unlink_id ~now () in
              match
                with_tx db (fun () ->
                    apply_split_in_tx ~db ~plan ~now ~unlink_id)
              with
              | Ok receipt -> Applied receipt
              | Error (Tx_stale e) -> Stale_revision e
              | Error (Tx_refused r) ->
                  Refused
                    {
                      reason = r.reason;
                      conflicts = r.conflicts;
                      preview = r.preview;
                    }
              | Error (Tx_msg e) when is_revision_conflict e -> Stale_revision e
              | Error (Tx_msg e) ->
                  Refused
                    { reason = e; conflicts = []; preview = Some plan.preview })
          ))

let unlink_actor ~db ~source_principal_id ~actor_key
    ?(ownership = Retain_on_source) ?expected_source_revision
    ?expected_actor_revision ?plan_id ?unlink_id ?(now = Unix.gettimeofday ())
    () =
  let plan_id =
    match plan_id with Some id -> id | None -> generate_plan_id ~now ()
  in
  (* Idempotent by plan_id. *)
  match get_unlink_receipt_by_plan ~db ~plan_id with
  | Error e -> Refused { reason = e; conflicts = []; preview = None }
  | Ok (Some r) -> Idempotent r
  | Ok None -> (
      (* Pre-check CAS expectations against live state before planning. *)
      match
        ( S.get_principal ~db ~id:source_principal_id,
          S.get_connector_actor ~db ~key:actor_key )
      with
      | Error e, _ | _, Error e ->
          Refused { reason = e; conflicts = []; preview = None }
      | Ok None, _ ->
          Refused
            {
              reason =
                Printf.sprintf "source principal not found: %s"
                  (P.principal_id_to_string source_principal_id);
              conflicts = [];
              preview = None;
            }
      | _, Ok None ->
          Refused
            {
              reason =
                Printf.sprintf "connector actor not found: %s"
                  (P.actor_identity_key actor_key);
              conflicts = [];
              preview = None;
            }
      | Ok (Some source), Ok (Some actor) -> (
          match
            ( check_expected ~label:"source_principal"
                ~expected:expected_source_revision ~actual:source.revision,
              check_expected ~label:"actor" ~expected:expected_actor_revision
                ~actual:actor.revision )
          with
          | Error e, _ | _, Error e -> Stale_revision e
          | Ok (), Ok () -> (
              match
                make_split_plan ~db ~id:plan_id ~source_principal_id ~actor_key
                  ~ownership ~now ()
              with
              | Error e ->
                  Refused { reason = e; conflicts = []; preview = None }
              | Ok plan -> (
                  (* Self-service: confirm with own digest. *)
                  match
                    confirm_split_plan ~db ~id:plan.id
                      ~presented_digest:plan.digest ~now ()
                  with
                  | Error e ->
                      Refused
                        {
                          reason = e;
                          conflicts = [];
                          preview = Some plan.preview;
                        }
                  | Ok confirmed -> (
                      let unlink_id =
                        match unlink_id with
                        | Some id -> id
                        | None -> generate_unlink_id ~now ()
                      in
                      match
                        with_tx db (fun () ->
                            apply_split_in_tx ~db ~plan:confirmed ~now
                              ~unlink_id)
                      with
                      | Ok receipt -> Applied receipt
                      | Error (Tx_stale e) -> Stale_revision e
                      | Error (Tx_refused r) ->
                          Refused
                            {
                              reason = r.reason;
                              conflicts = r.conflicts;
                              preview = r.preview;
                            }
                      | Error (Tx_msg e) when is_revision_conflict e ->
                          Stale_revision e
                      | Error (Tx_msg e) ->
                          Refused
                            {
                              reason = e;
                              conflicts = [];
                              preview = Some confirmed.preview;
                            })))))

let refuse_reverse_merge ~db ~survivor_id ~loser_id
    ?(now = Unix.gettimeofday ()) () =
  let _ = (db, now) in
  let summary =
    Printf.sprintf
      "reverse merge refused: cannot restore tombstone %s into survivor %s; \
       unlink is an identity split to a new empty Principal, not reverse \
       credential adoption"
      (P.principal_id_to_string loser_id)
      (P.principal_id_to_string survivor_id)
  in
  Refused
    {
      reason = summary;
      conflicts = [ Reverse_merge_forbidden { summary } ];
      preview = None;
    }
