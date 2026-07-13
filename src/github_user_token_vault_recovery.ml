(** Vault backup/restore and key-compromise recovery (P21.M2.E4.T008).

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md,
    docs/adr/0006-use-principal-owned-github-user-tokens.md, and
    docs/github-vault-recovery.md. *)

module V = Github_user_token_vault
module MK = Github_user_token_master_key
module Gate = Github_user_authorization_gate

(* -------------------------------------------------------------------------- *)
(* Contract constants                                                         *)
(* -------------------------------------------------------------------------- *)

let backup_schema_version = 1

(** V1 makes no whole-store anti-rollback claim without an external monotonic
    anchor. Always false — asserted by tests. *)
let whole_store_rollback_detectable_without_external_anchor = false

let whole_store_rollback_limitation_tag =
  "whole_store_rollback_not_detectable_without_external_monotonic_anchor"

let whole_store_rollback_limitation_statement =
  "A whole-store rollback under the same available key is not detectable \
   without an external monotonic anchor. Record AEAD and token-generation CAS \
   do not detect replacement of the entire store with an internally consistent \
   older snapshot encrypted under an available key."

let compromise_relink_required_tag =
  "key_compromise_requires_destructive_disable_and_relink"

(* -------------------------------------------------------------------------- *)
(* Types                                                                      *)
(* -------------------------------------------------------------------------- *)

type operator_proof = {
  operator_id : string;
  approval : string;
  acknowledged_limitations : string list;
}

type sealed_envelope = {
  id : string;
  principal_id : string;
  github_user_id : int64;
  app_id : int;
  host : string;
  record_version : int;
  key_id : MK.key_id;
  key_version : MK.key_version;
  key_fingerprint : string;
  generation : int;
  scopes : string list;
  expires_at : string;
  ciphertext : string;
  created_at : string;
  updated_at : string;
}

type backup = {
  backup_schema_version : int;
  vault_schema_version : int;
  exported_at : string;
  required_key_ids : string list;
  envelopes : sealed_envelope list;
}

type compatibility_issue =
  | Unsupported_backup_schema of { version : int }
  | Unsupported_vault_schema of { version : int }
  | Unsupported_record_version of { id : string; version : int }
  | Missing_required_key of { key_id : string }
  | Unopenable_envelope of { id : string; reason : V.denial }
  | Empty_backup

type recovery_state = {
  user_authorization_enabled : bool;
  last_event : string;
  last_reason : string option;
  last_operator_id : string option;
  last_event_at : string option;
  compromised_key_ids : string list;
  requires_relink : bool;
  requires_key_rotation : bool;
}

type destroy_hooks = {
  destroy_bindings : unit -> (int, string) result;
  destroy_leases : unit -> (int, string) result;
  destroy_pending_extra : unit -> (int, string) result;
}

let default_destroy_hooks : destroy_hooks =
  {
    destroy_bindings = (fun () -> Ok 0);
    destroy_leases = (fun () -> Ok (Github_user_token_lease.discard_all ()));
    destroy_pending_extra = (fun () -> Ok 0);
  }

type restore_result = {
  imported : int;
  required_key_ids : string list;
  authorization_disabled : bool;
  leases_discarded : int;
  bindings_destroyed : int;
  approved_by : string;
}

type denial =
  | Operator_proof_required of string
  | Compatibility of compatibility_issue list
  | Vault of V.denial
  | Invalid_input of string
  | Storage of string
  | Hook of string

type compromise_result = {
  authorization_disabled : bool;
  vault_records_destroyed : int;
  pending_auth_tx_destroyed : int;
  rewrap_jobs_destroyed : int;
  bindings_destroyed : int;
  leases_discarded : int;
  pending_extra_destroyed : int;
  affected_key_ids : string list;
  requires_key_rotation : bool;
  requires_relink : bool;
  approved_by : string;
}

(* -------------------------------------------------------------------------- *)
(* String helpers                                                             *)
(* -------------------------------------------------------------------------- *)

let string_of_compatibility_issue = function
  | Unsupported_backup_schema { version } ->
      Printf.sprintf "unsupported_backup_schema:%d" version
  | Unsupported_vault_schema { version } ->
      Printf.sprintf "unsupported_vault_schema:%d" version
  | Unsupported_record_version { id; version } ->
      Printf.sprintf "unsupported_record_version:id=%s version=%d" id version
  | Missing_required_key { key_id } ->
      Printf.sprintf "missing_required_key:%s" key_id
  | Unopenable_envelope { id; reason } ->
      Printf.sprintf "unopenable_envelope:id=%s reason=%s" id
        (V.string_of_denial reason)
  | Empty_backup -> "empty_backup"

let string_of_denial = function
  | Operator_proof_required msg ->
      Printf.sprintf "operator_proof_required:%s" msg
  | Compatibility issues ->
      "compatibility:"
      ^ String.concat ";" (List.map string_of_compatibility_issue issues)
  | Vault d -> "vault:" ^ V.string_of_denial d
  | Invalid_input msg -> Printf.sprintf "invalid_input:%s" msg
  | Storage msg -> Printf.sprintf "storage:%s" msg
  | Hook msg -> Printf.sprintf "hook:%s" msg

let denial_exposes_token ~denial ~plaintext =
  if plaintext = "" then false
  else String_util.contains (string_of_denial denial) plaintext

let list_contains_tag tags tag =
  List.exists (fun t -> String.equal (String.trim t) tag) tags

(* -------------------------------------------------------------------------- *)
(* Operator proof                                                             *)
(* -------------------------------------------------------------------------- *)

let make_operator_proof ~operator_id ~approval ~acknowledged_limitations () =
  let operator_id = String.trim operator_id in
  let approval = String.trim approval in
  if operator_id = "" then Error "operator_id must be non-empty"
  else if approval = "" then Error "approval must be non-empty (operator proof)"
  else Ok { operator_id; approval; acknowledged_limitations }

let require_proof ~proof ~required_tags =
  match
    make_operator_proof ~operator_id:proof.operator_id ~approval:proof.approval
      ~acknowledged_limitations:proof.acknowledged_limitations ()
  with
  | Error msg -> Error (Operator_proof_required msg)
  | Ok proof ->
      let missing =
        List.filter
          (fun tag ->
            not (list_contains_tag proof.acknowledged_limitations tag))
          required_tags
      in
      if missing <> [] then
        Error
          (Operator_proof_required
             (Printf.sprintf "missing limitation acknowledgment(s): %s"
                (String.concat "," missing)))
      else Ok proof

(* -------------------------------------------------------------------------- *)
(* SQLite helpers                                                             *)
(* -------------------------------------------------------------------------- *)

let exec_schema db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf
           "github_user_token_vault_recovery schema error: %s (sql: %s)"
           (Sqlite3.Rc.to_string rc) sql)

let ensure_schema db =
  V.ensure_schema db;
  let events =
    {|CREATE TABLE IF NOT EXISTS github_user_token_vault_recovery_events (
      id TEXT PRIMARY KEY NOT NULL,
      event_kind TEXT NOT NULL,
      reason TEXT,
      operator_id TEXT,
      details_json TEXT NOT NULL,
      created_at TEXT NOT NULL
    )|}
  in
  match Gate.ensure_schema db with
  | Error e ->
      failwith
        (Printf.sprintf "github_user_token_vault_recovery gate schema error: %s"
           e)
  | Ok () -> exec_schema db events

let text_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.TEXT s -> s
  | Sqlite3.Data.NULL -> ""
  | Sqlite3.Data.INT n -> Int64.to_string n
  | _ -> ""

let text_opt_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.TEXT s when s <> "" -> Some s
  | Sqlite3.Data.NULL | Sqlite3.Data.TEXT _ -> None
  | Sqlite3.Data.INT n -> Some (Int64.to_string n)
  | _ -> None

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

let storage_err label rc db =
  Storage
    (Printf.sprintf "%s failed: %s (%s)" label (Sqlite3.Rc.to_string rc)
       (Sqlite3.errmsg db))

(* -------------------------------------------------------------------------- *)
(* State load / update                                                        *)
(* -------------------------------------------------------------------------- *)

let string_list_of_json s =
  match Yojson.Safe.from_string s with
  | exception _ -> Error "compromised_key_ids_json is not valid JSON"
  | `List items ->
      let rec go acc = function
        | [] -> Ok (List.rev acc)
        | `String x :: rest -> go (x :: acc) rest
        | _ -> Error "compromised_key_ids_json must be a string array"
      in
      go [] items
  | _ -> Error "compromised_key_ids_json must be a JSON array"

let string_list_to_json xs =
  Yojson.Safe.to_string (`List (List.map (fun s -> `String s) xs))

let default_state : recovery_state =
  {
    user_authorization_enabled = true;
    last_event = "none";
    last_reason = None;
    last_operator_id = None;
    last_event_at = None;
    compromised_key_ids = [];
    requires_relink = false;
    requires_key_rotation = false;
  }

let load_state ~db =
  ensure_schema db;
  let sql =
    {|SELECT user_authorization_enabled, last_event, last_reason,
             last_operator_id, last_event_at, compromised_key_ids_json,
             requires_relink, requires_key_rotation
      FROM github_user_token_vault_recovery_state WHERE id = 1|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          let enabled = int_col stmt 0 <> 0 in
          let last_event = text_col stmt 1 in
          let last_reason = text_opt_col stmt 2 in
          let last_operator_id = text_opt_col stmt 3 in
          let last_event_at = text_opt_col stmt 4 in
          let keys_json = text_col stmt 5 in
          let requires_relink = int_col stmt 6 <> 0 in
          let requires_key_rotation = int_col stmt 7 <> 0 in
          match string_list_of_json keys_json with
          | Error e -> Error e
          | Ok compromised_key_ids ->
              Ok
                {
                  user_authorization_enabled = enabled;
                  last_event;
                  last_reason;
                  last_operator_id;
                  last_event_at;
                  compromised_key_ids;
                  requires_relink;
                  requires_key_rotation;
                })
      | Sqlite3.Rc.DONE -> Ok default_state
      | rc ->
          Error
            (Printf.sprintf "load recovery state failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let user_authorization_enabled ~db =
  match load_state ~db with
  | Ok s -> Ok s.user_authorization_enabled
  | Error e -> Error e

let write_state ~db ~state =
  let sql =
    {|UPDATE github_user_token_vault_recovery_state SET
        user_authorization_enabled = ?,
        last_event = ?,
        last_reason = ?,
        last_operator_id = ?,
        last_event_at = ?,
        compromised_key_ids_json = ?,
        requires_relink = ?,
        requires_key_rotation = ?
      WHERE id = 1|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let bind i d = ignore (Sqlite3.bind stmt i d) in
      bind 1
        (Sqlite3.Data.INT (if state.user_authorization_enabled then 1L else 0L));
      bind 2 (Sqlite3.Data.TEXT state.last_event);
      bind 3
        (match state.last_reason with
        | None -> Sqlite3.Data.NULL
        | Some s -> Sqlite3.Data.TEXT s);
      bind 4
        (match state.last_operator_id with
        | None -> Sqlite3.Data.NULL
        | Some s -> Sqlite3.Data.TEXT s);
      bind 5
        (match state.last_event_at with
        | None -> Sqlite3.Data.NULL
        | Some s -> Sqlite3.Data.TEXT s);
      bind 6 (Sqlite3.Data.TEXT (string_list_to_json state.compromised_key_ids));
      bind 7 (Sqlite3.Data.INT (if state.requires_relink then 1L else 0L));
      bind 8 (Sqlite3.Data.INT (if state.requires_key_rotation then 1L else 0L));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok ()
      | rc -> Error (storage_err "update recovery state" rc db))

let record_event ~db ~event_kind ~reason ~operator_id ~details ~now =
  let id =
    Printf.sprintf "ghvault_recov_%d_%06d" (int_of_float now)
      (Random.int 1_000_000)
  in
  let created_at = Time_util.iso8601_utc ~t:now () in
  let details_json = Yojson.Safe.to_string details in
  let sql =
    {|INSERT INTO github_user_token_vault_recovery_events
      (id, event_kind, reason, operator_id, details_json, created_at)
      VALUES (?,?,?,?,?,?)|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let bind i d = ignore (Sqlite3.bind stmt i d) in
      bind 1 (Sqlite3.Data.TEXT id);
      bind 2 (Sqlite3.Data.TEXT event_kind);
      bind 3
        (match reason with
        | None -> Sqlite3.Data.NULL
        | Some s -> Sqlite3.Data.TEXT s);
      bind 4 (Sqlite3.Data.TEXT operator_id);
      bind 5 (Sqlite3.Data.TEXT details_json);
      bind 6 (Sqlite3.Data.TEXT created_at);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok ()
      | rc -> Error (storage_err "insert recovery event" rc db))

let state_to_json (s : recovery_state) : Yojson.Safe.t =
  `Assoc
    [
      ("user_authorization_enabled", `Bool s.user_authorization_enabled);
      ("last_event", `String s.last_event);
      ( "last_reason",
        match s.last_reason with None -> `Null | Some r -> `String r );
      ( "last_operator_id",
        match s.last_operator_id with None -> `Null | Some o -> `String o );
      ( "last_event_at",
        match s.last_event_at with None -> `Null | Some t -> `String t );
      ( "compromised_key_ids",
        `List (List.map (fun k -> `String k) s.compromised_key_ids) );
      ("requires_relink", `Bool s.requires_relink);
      ("requires_key_rotation", `Bool s.requires_key_rotation);
      ( "whole_store_rollback_detectable_without_external_anchor",
        `Bool whole_store_rollback_detectable_without_external_anchor );
    ]

(* -------------------------------------------------------------------------- *)
(* Scopes JSON                                                                *)
(* -------------------------------------------------------------------------- *)

let scopes_to_json scopes =
  Yojson.Safe.to_string (`List (List.map (fun s -> `String s) scopes))

let scopes_of_json s =
  match Yojson.Safe.from_string s with
  | exception _ -> Error "scopes_json invalid"
  | `List items ->
      let rec go acc = function
        | [] -> Ok (List.rev acc)
        | `String x :: rest -> go (x :: acc) rest
        | _ -> Error "scopes_json must be string array"
      in
      go [] items
  | _ -> Error "scopes_json must be array"

(* -------------------------------------------------------------------------- *)
(* Export                                                                     *)
(* -------------------------------------------------------------------------- *)

let load_all_envelopes db : (sealed_envelope list, string) result =
  let sql =
    {|SELECT id, principal_id, github_user_id, app_id, host, record_version,
             key_id, key_version, key_fingerprint, generation, scopes_json,
             expires_at, ciphertext, created_at, updated_at
      FROM github_user_token_vault
      ORDER BY id ASC|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let rec go acc =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> (
            let scopes_json = text_col stmt 10 in
            match scopes_of_json scopes_json with
            | Error e -> Error e
            | Ok scopes ->
                let env =
                  {
                    id = text_col stmt 0;
                    principal_id = text_col stmt 1;
                    github_user_id = int64_col stmt 2;
                    app_id = int_col stmt 3;
                    host = text_col stmt 4;
                    record_version = int_col stmt 5;
                    key_id = text_col stmt 6;
                    key_version = int_col stmt 7;
                    key_fingerprint = text_col stmt 8;
                    generation = int_col stmt 9;
                    scopes;
                    expires_at = text_col stmt 11;
                    ciphertext = text_col stmt 12;
                    created_at = text_col stmt 13;
                    updated_at = text_col stmt 14;
                  }
                in
                go (env :: acc))
        | Sqlite3.Rc.DONE -> Ok (List.rev acc)
        | rc ->
            Error
              (Printf.sprintf "export scan failed: %s (%s)"
                 (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))
      in
      go [])

let distinct_sorted_key_ids envelopes =
  let tbl = Hashtbl.create 8 in
  List.iter
    (fun (e : sealed_envelope) -> Hashtbl.replace tbl e.key_id ())
    envelopes;
  Hashtbl.fold (fun k () acc -> k :: acc) tbl [] |> List.sort String.compare

let export_backup ~db ?(now = Unix.gettimeofday ()) () =
  ensure_schema db;
  match load_all_envelopes db with
  | Error e -> Error e
  | Ok envelopes ->
      let required_key_ids = distinct_sorted_key_ids envelopes in
      Ok
        {
          backup_schema_version;
          vault_schema_version = V.schema_version;
          exported_at = Time_util.iso8601_utc ~t:now ();
          required_key_ids;
          envelopes;
        }

let envelope_to_json (e : sealed_envelope) : Yojson.Safe.t =
  `Assoc
    [
      ("id", `String e.id);
      ("principal_id", `String e.principal_id);
      ("github_user_id", `Intlit (Int64.to_string e.github_user_id));
      ("app_id", `Int e.app_id);
      ("host", `String e.host);
      ("record_version", `Int e.record_version);
      ("key_id", `String e.key_id);
      ("key_version", `Int e.key_version);
      ("key_fingerprint", `String e.key_fingerprint);
      ("generation", `Int e.generation);
      ("scopes", `List (List.map (fun s -> `String s) e.scopes));
      ("expires_at", `String e.expires_at);
      ("ciphertext", `String e.ciphertext);
      ("created_at", `String e.created_at);
      ("updated_at", `String e.updated_at);
    ]

let backup_to_json (b : backup) : Yojson.Safe.t =
  `Assoc
    [
      ("backup_schema_version", `Int b.backup_schema_version);
      ("vault_schema_version", `Int b.vault_schema_version);
      ("exported_at", `String b.exported_at);
      ( "required_key_ids",
        `List (List.map (fun k -> `String k) b.required_key_ids) );
      ("envelopes", `List (List.map envelope_to_json b.envelopes));
      ( "whole_store_rollback_detectable_without_external_anchor",
        `Bool whole_store_rollback_detectable_without_external_anchor );
      ( "whole_store_rollback_limitation",
        `String whole_store_rollback_limitation_statement );
    ]

let json_string_field fields name =
  match List.assoc_opt name fields with
  | Some (`String s) -> Ok s
  | Some _ -> Error (Printf.sprintf "field %s must be string" name)
  | None -> Error (Printf.sprintf "missing field %s" name)

let json_int_field fields name =
  match List.assoc_opt name fields with
  | Some (`Int i) -> Ok i
  | Some (`Intlit s) -> (
      try Ok (int_of_string s)
      with _ -> Error (Printf.sprintf "field %s invalid int" name))
  | Some _ -> Error (Printf.sprintf "field %s must be int" name)
  | None -> Error (Printf.sprintf "missing field %s" name)

let json_int64_field fields name =
  match List.assoc_opt name fields with
  | Some (`Int i) -> Ok (Int64.of_int i)
  | Some (`Intlit s) -> (
      try Ok (Int64.of_string s)
      with _ -> Error (Printf.sprintf "field %s invalid int64" name))
  | Some _ -> Error (Printf.sprintf "field %s must be int64" name)
  | None -> Error (Printf.sprintf "missing field %s" name)

let json_string_list_field fields name =
  match List.assoc_opt name fields with
  | Some (`List items) ->
      let rec go acc = function
        | [] -> Ok (List.rev acc)
        | `String x :: rest -> go (x :: acc) rest
        | _ -> Error (Printf.sprintf "field %s must be string array" name)
      in
      go [] items
  | Some _ -> Error (Printf.sprintf "field %s must be array" name)
  | None -> Error (Printf.sprintf "missing field %s" name)

let envelope_of_json = function
  | `Assoc fields ->
      let ( let* ) = Result.bind in
      let* id = json_string_field fields "id" in
      let* principal_id = json_string_field fields "principal_id" in
      let* github_user_id = json_int64_field fields "github_user_id" in
      let* app_id = json_int_field fields "app_id" in
      let* host = json_string_field fields "host" in
      let* record_version = json_int_field fields "record_version" in
      let* key_id = json_string_field fields "key_id" in
      let* key_version = json_int_field fields "key_version" in
      let* key_fingerprint = json_string_field fields "key_fingerprint" in
      let* generation = json_int_field fields "generation" in
      let* scopes = json_string_list_field fields "scopes" in
      let* expires_at = json_string_field fields "expires_at" in
      let* ciphertext = json_string_field fields "ciphertext" in
      let* created_at = json_string_field fields "created_at" in
      let* updated_at = json_string_field fields "updated_at" in
      if id = "" then Error "envelope id empty"
      else if ciphertext = "" then Error "envelope ciphertext empty"
      else
        Ok
          {
            id;
            principal_id;
            github_user_id;
            app_id;
            host;
            record_version;
            key_id;
            key_version;
            key_fingerprint;
            generation;
            scopes;
            expires_at;
            ciphertext;
            created_at;
            updated_at;
          }
  | _ -> Error "envelope must be object"

let backup_of_json = function
  | `Assoc fields -> (
      let ( let* ) = Result.bind in
      let* bsv = json_int_field fields "backup_schema_version" in
      let* vsv = json_int_field fields "vault_schema_version" in
      let* exported_at = json_string_field fields "exported_at" in
      let* required_key_ids =
        json_string_list_field fields "required_key_ids"
      in
      match List.assoc_opt "envelopes" fields with
      | None -> Error "missing field envelopes"
      | Some (`List items) ->
          let rec go acc = function
            | [] -> Ok (List.rev acc)
            | item :: rest -> (
                match envelope_of_json item with
                | Error e -> Error e
                | Ok e -> go (e :: acc) rest)
          in
          let* envelopes = go [] items in
          Ok
            {
              backup_schema_version = bsv;
              vault_schema_version = vsv;
              exported_at;
              required_key_ids;
              envelopes;
            }
      | Some _ -> Error "envelopes must be array")
  | _ -> Error "backup must be object"

let backup_contains_plaintext ~backup ~plaintext =
  if plaintext = "" then false
  else
    let json = backup_to_json backup in
    V.json_contains_plaintext ~json ~plaintext

(* -------------------------------------------------------------------------- *)
(* Compatibility                                                              *)
(* -------------------------------------------------------------------------- *)

let check_compatibility ~(keys : V.key_provider) ~(backup : backup) () =
  let issues = ref [] in
  let add i = issues := i :: !issues in
  if backup.backup_schema_version <> backup_schema_version then
    add (Unsupported_backup_schema { version = backup.backup_schema_version });
  if backup.vault_schema_version > V.schema_version then
    add (Unsupported_vault_schema { version = backup.vault_schema_version });
  if backup.envelopes = [] then add Empty_backup;
  List.iter
    (fun (e : sealed_envelope) ->
      if e.record_version < 1 || e.record_version > V.schema_version then
        add
          (Unsupported_record_version { id = e.id; version = e.record_version }))
    backup.envelopes;
  (* Every declared required key and every envelope key must resolve. *)
  let key_set =
    let t = Hashtbl.create 8 in
    List.iter (fun k -> Hashtbl.replace t k ()) backup.required_key_ids;
    List.iter
      (fun (e : sealed_envelope) -> Hashtbl.replace t e.key_id ())
      backup.envelopes;
    t
  in
  Hashtbl.iter
    (fun key_id () ->
      match keys.resolve ~key_id with
      | Error () -> add (Missing_required_key { key_id })
      | Ok _ -> ())
    key_set;
  List.iter
    (fun (e : sealed_envelope) ->
      if not (String.starts_with ~prefix:"$VAULT_AAD_V1:" e.ciphertext) then
        add (Unopenable_envelope { id = e.id; reason = V.Corrupt_envelope }))
    backup.envelopes;
  match List.rev !issues with [] -> Ok () | xs -> Error xs

(** Open each sealed envelope in a throwaway DB to prove key/schema fit. *)
let verify_envelopes_openable ~(keys : V.key_provider) ~(backup : backup) =
  let mem = Sqlite3.db_open ":memory:" in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close mem))
    (fun () ->
      V.ensure_schema mem;
      let issues = ref [] in
      List.iter
        (fun (e : sealed_envelope) ->
          let sql =
            {|INSERT INTO github_user_token_vault
              (id, principal_id, github_user_id, app_id, host, record_version,
               key_id, key_version, key_fingerprint, generation, active,
               scopes_json, expires_at, ciphertext, created_at, updated_at)
              VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)|}
          in
          let stmt = Sqlite3.prepare mem sql in
          let insert_ok =
            Fun.protect
              ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
              (fun () ->
                let bind i d = ignore (Sqlite3.bind stmt i d) in
                bind 1 (Sqlite3.Data.TEXT e.id);
                bind 2 (Sqlite3.Data.TEXT e.principal_id);
                bind 3 (Sqlite3.Data.INT e.github_user_id);
                bind 4 (Sqlite3.Data.INT (Int64.of_int e.app_id));
                bind 5 (Sqlite3.Data.TEXT e.host);
                bind 6 (Sqlite3.Data.INT (Int64.of_int e.record_version));
                bind 7 (Sqlite3.Data.TEXT e.key_id);
                bind 8 (Sqlite3.Data.INT (Int64.of_int e.key_version));
                bind 9 (Sqlite3.Data.TEXT e.key_fingerprint);
                bind 10 (Sqlite3.Data.INT (Int64.of_int e.generation));
                bind 11 (Sqlite3.Data.INT 1L);
                bind 12 (Sqlite3.Data.TEXT (scopes_to_json e.scopes));
                bind 13 (Sqlite3.Data.TEXT e.expires_at);
                bind 14 (Sqlite3.Data.TEXT e.ciphertext);
                bind 15 (Sqlite3.Data.TEXT e.created_at);
                bind 16 (Sqlite3.Data.TEXT e.updated_at);
                match Sqlite3.step stmt with
                | Sqlite3.Rc.DONE -> true
                | _ -> false)
          in
          if not insert_ok then
            issues :=
              Unopenable_envelope { id = e.id; reason = V.Storage "insert" }
              :: !issues
          else
            match V.read ~db:mem ~keys ~id:e.id () with
            | Ok _ -> ()
            | Error d ->
                issues :=
                  Unopenable_envelope { id = e.id; reason = d } :: !issues)
        backup.envelopes;
      match List.rev !issues with [] -> Ok () | xs -> Error xs)

(* -------------------------------------------------------------------------- *)
(* Vault replace helpers                                                      *)
(* -------------------------------------------------------------------------- *)

let wipe_vault ~db =
  match Sqlite3.exec db "DELETE FROM github_user_token_vault" with
  | Sqlite3.Rc.OK -> Ok (Sqlite3.changes db)
  | rc -> Error (storage_err "wipe vault" rc db)

let insert_envelope ~db (e : sealed_envelope) =
  let sql =
    {|INSERT INTO github_user_token_vault
      (id, principal_id, github_user_id, app_id, host, record_version,
       key_id, key_version, key_fingerprint, generation, active, scopes_json,
       expires_at, ciphertext, created_at, updated_at)
      VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let bind i d = ignore (Sqlite3.bind stmt i d) in
      bind 1 (Sqlite3.Data.TEXT e.id);
      bind 2 (Sqlite3.Data.TEXT e.principal_id);
      bind 3 (Sqlite3.Data.INT e.github_user_id);
      bind 4 (Sqlite3.Data.INT (Int64.of_int e.app_id));
      bind 5 (Sqlite3.Data.TEXT e.host);
      bind 6 (Sqlite3.Data.INT (Int64.of_int e.record_version));
      bind 7 (Sqlite3.Data.TEXT e.key_id);
      bind 8 (Sqlite3.Data.INT (Int64.of_int e.key_version));
      bind 9 (Sqlite3.Data.TEXT e.key_fingerprint);
      bind 10 (Sqlite3.Data.INT (Int64.of_int e.generation));
      (* Restore starts with authorization disabled; vault rows stay sealed but
         inactive until operator re-enable / relink (T004 active flag). *)
      bind 11 (Sqlite3.Data.INT 0L);
      bind 12 (Sqlite3.Data.TEXT (scopes_to_json e.scopes));
      bind 13 (Sqlite3.Data.TEXT e.expires_at);
      bind 14 (Sqlite3.Data.TEXT e.ciphertext);
      bind 15 (Sqlite3.Data.TEXT e.created_at);
      bind 16 (Sqlite3.Data.TEXT e.updated_at);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok ()
      | rc -> Error (storage_err "insert envelope" rc db))

let destroy_vault_for_keys ~db ~key_ids =
  match key_ids with
  | None -> wipe_vault ~db
  | Some [] -> Ok 0
  | Some ids ->
      let rec go acc = function
        | [] -> Ok acc
        | key_id :: rest -> (
            let sql = "DELETE FROM github_user_token_vault WHERE key_id = ?" in
            let stmt = Sqlite3.prepare db sql in
            let res =
              Fun.protect
                ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
                (fun () ->
                  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT key_id));
                  match Sqlite3.step stmt with
                  | Sqlite3.Rc.DONE -> Ok (Sqlite3.changes db)
                  | rc -> Error (storage_err "destroy vault by key" rc db))
            in
            match res with Error e -> Error e | Ok n -> go (acc + n) rest)
      in
      go 0 ids

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

let destroy_pending_auth_tx ~db =
  if not (table_exists db "github_user_auth_tx") then Ok 0
  else
    match Sqlite3.exec db "DELETE FROM github_user_auth_tx" with
    | Sqlite3.Rc.OK -> Ok (Sqlite3.changes db)
    | rc -> Error (storage_err "destroy auth_tx" rc db)

let destroy_rewrap_jobs ~db =
  if not (table_exists db "github_user_token_rewrap") then Ok 0
  else
    match Sqlite3.exec db "DELETE FROM github_user_token_rewrap" with
    | Sqlite3.Rc.OK -> Ok (Sqlite3.changes db)
    | rc -> Error (storage_err "destroy rewrap jobs" rc db)

let run_hook name f =
  match f () with Ok n -> Ok n | Error e -> Error (Hook (name ^ ": " ^ e))

let commit_event ~db ~state ~event_kind ~operator_id ~details ~now =
  match write_state ~db ~state with
  | Error e -> Error e
  | Ok () ->
      record_event ~db ~event_kind ~reason:state.last_reason ~operator_id
        ~details ~now

(* -------------------------------------------------------------------------- *)
(* Restore                                                                    *)
(* -------------------------------------------------------------------------- *)

let restore ~db ~(keys : V.key_provider) ~(proof : operator_proof)
    ~(backup : backup) ?(hooks = default_destroy_hooks)
    ?(now = Unix.gettimeofday ()) () =
  ensure_schema db;
  match
    require_proof ~proof ~required_tags:[ whole_store_rollback_limitation_tag ]
  with
  | Error e -> Error e
  | Ok proof -> (
      match check_compatibility ~keys ~backup () with
      | Error issues -> Error (Compatibility issues)
      | Ok () -> (
          match verify_envelopes_openable ~keys ~backup with
          | Error issues -> Error (Compatibility issues)
          | Ok () -> (
              let state : recovery_state =
                {
                  user_authorization_enabled = false;
                  last_event = "restore";
                  last_reason =
                    Some
                      "backup restore; authorization disabled pending \
                       reconciliation";
                  last_operator_id = Some proof.operator_id;
                  last_event_at = Some (Time_util.iso8601_utc ~t:now ());
                  compromised_key_ids = [];
                  requires_relink = false;
                  requires_key_rotation = false;
                }
              in
              match write_state ~db ~state with
              | Error e -> Error e
              | Ok () -> (
                  match wipe_vault ~db with
                  | Error e -> Error e
                  | Ok _ -> (
                      let rec insert_all = function
                        | [] -> Ok ()
                        | e :: rest -> (
                            match insert_envelope ~db e with
                            | Error e -> Error e
                            | Ok () -> insert_all rest)
                      in
                      match insert_all backup.envelopes with
                      | Error e -> Error e
                      | Ok () -> (
                          match
                            run_hook "destroy_leases" hooks.destroy_leases
                          with
                          | Error e -> Error e
                          | Ok leases_discarded -> (
                              match
                                run_hook "destroy_bindings"
                                  hooks.destroy_bindings
                              with
                              | Error e -> Error e
                              | Ok bindings_destroyed -> (
                                  match destroy_rewrap_jobs ~db with
                                  | Error e -> Error e
                                  | Ok _ -> (
                                      let details =
                                        `Assoc
                                          [
                                            ( "imported",
                                              `Int
                                                (List.length backup.envelopes)
                                            );
                                            ( "required_key_ids",
                                              `List
                                                (List.map
                                                   (fun k -> `String k)
                                                   backup.required_key_ids) );
                                            ( "leases_discarded",
                                              `Int leases_discarded );
                                            ( "bindings_destroyed",
                                              `Int bindings_destroyed );
                                            ( "whole_store_rollback_detectable",
                                              `Bool
                                                whole_store_rollback_detectable_without_external_anchor
                                            );
                                          ]
                                      in
                                      match
                                        commit_event ~db ~state
                                          ~event_kind:"restore"
                                          ~operator_id:proof.operator_id
                                          ~details ~now
                                      with
                                      | Error e -> Error e
                                      | Ok () ->
                                          Ok
                                            {
                                              imported =
                                                List.length backup.envelopes;
                                              required_key_ids =
                                                backup.required_key_ids;
                                              authorization_disabled = true;
                                              leases_discarded;
                                              bindings_destroyed;
                                              approved_by = proof.operator_id;
                                            })))))))))

(* -------------------------------------------------------------------------- *)
(* Compromise disable                                                         *)
(* -------------------------------------------------------------------------- *)

let compromise_disable ~db ~(proof : operator_proof) ~reason ?affected_key_ids
    ?(hooks = default_destroy_hooks) ?(now = Unix.gettimeofday ()) () =
  ensure_schema db;
  let reason = String.trim reason in
  if reason = "" then Error (Invalid_input "reason must be non-empty")
  else
    match
      require_proof ~proof ~required_tags:[ compromise_relink_required_tag ]
    with
    | Error e -> Error e
    | Ok proof -> (
        let affected =
          match affected_key_ids with
          | Some ids ->
              ids |> List.map String.trim
              |> List.filter (fun s -> s <> "")
              |> List.sort_uniq String.compare
          | None -> (
              match V.list_distinct_key_ids ~db with
              | Ok ids -> ids
              | Error _ -> [])
        in
        let key_filter =
          match affected_key_ids with None -> None | Some _ -> Some affected
        in
        let disabled_state =
          match load_state ~db with
          | Error e -> Error (Storage e)
          | Ok prev ->
              let state : recovery_state =
                {
                  user_authorization_enabled = false;
                  last_event = "compromise_disable";
                  last_reason = Some reason;
                  last_operator_id = Some proof.operator_id;
                  last_event_at = Some (Time_util.iso8601_utc ~t:now ());
                  compromised_key_ids =
                    List.sort_uniq String.compare
                      (prev.compromised_key_ids @ affected);
                  requires_relink = true;
                  requires_key_rotation = true;
                }
              in
              write_state ~db ~state
        in
        match disabled_state with
        | Error e -> Error e
        | Ok () -> (
            match destroy_vault_for_keys ~db ~key_ids:key_filter with
            | Error e -> Error e
            | Ok vault_records_destroyed -> (
                match destroy_pending_auth_tx ~db with
                | Error e -> Error e
                | Ok pending_auth_tx_destroyed -> (
                    match destroy_rewrap_jobs ~db with
                    | Error e -> Error e
                    | Ok rewrap_jobs_destroyed -> (
                        match
                          run_hook "destroy_bindings" hooks.destroy_bindings
                        with
                        | Error e -> Error e
                        | Ok bindings_destroyed -> (
                            match
                              run_hook "destroy_leases" hooks.destroy_leases
                            with
                            | Error e -> Error e
                            | Ok leases_discarded -> (
                                match
                                  run_hook "destroy_pending_extra"
                                    hooks.destroy_pending_extra
                                with
                                | Error e -> Error e
                                | Ok pending_extra_destroyed -> (
                                    match load_state ~db with
                                    | Error e -> Error (Storage e)
                                    | Ok prev -> (
                                        let compromised =
                                          List.sort_uniq String.compare
                                            (prev.compromised_key_ids @ affected)
                                        in
                                        let state : recovery_state =
                                          {
                                            user_authorization_enabled = false;
                                            last_event = "compromise_disable";
                                            last_reason = Some reason;
                                            last_operator_id =
                                              Some proof.operator_id;
                                            last_event_at =
                                              Some
                                                (Time_util.iso8601_utc ~t:now ());
                                            compromised_key_ids = compromised;
                                            requires_relink = true;
                                            requires_key_rotation = true;
                                          }
                                        in
                                        let details =
                                          `Assoc
                                            [
                                              ( "vault_records_destroyed",
                                                `Int vault_records_destroyed );
                                              ( "pending_auth_tx_destroyed",
                                                `Int pending_auth_tx_destroyed
                                              );
                                              ( "rewrap_jobs_destroyed",
                                                `Int rewrap_jobs_destroyed );
                                              ( "bindings_destroyed",
                                                `Int bindings_destroyed );
                                              ( "leases_discarded",
                                                `Int leases_discarded );
                                              ( "pending_extra_destroyed",
                                                `Int pending_extra_destroyed );
                                              ( "affected_key_ids",
                                                `List
                                                  (List.map
                                                     (fun k -> `String k)
                                                     affected) );
                                              ( "requires_key_rotation",
                                                `Bool true );
                                              ("requires_relink", `Bool true);
                                            ]
                                        in
                                        match
                                          commit_event ~db ~state
                                            ~event_kind:"compromise_disable"
                                            ~operator_id:proof.operator_id
                                            ~details ~now
                                        with
                                        | Error e -> Error e
                                        | Ok () ->
                                            Ok
                                              {
                                                authorization_disabled = true;
                                                vault_records_destroyed;
                                                pending_auth_tx_destroyed;
                                                rewrap_jobs_destroyed;
                                                bindings_destroyed;
                                                leases_discarded;
                                                pending_extra_destroyed;
                                                affected_key_ids = affected;
                                                requires_key_rotation = true;
                                                requires_relink = true;
                                                approved_by = proof.operator_id;
                                              })))))))))
