(* Process GitHub App authorization revocation webhooks (P21.M3.E1.T003).
   See github_user_auth_revocation_webhook.mli. *)

module I = Github_app_webhook_ingress
module V = Github_user_token_vault
module B = Github_account_binding
module C = Github_user_token_cas
module L = Github_user_token_lease

(* -------------------------------------------------------------------------- *)
(* Types                                                                      *)
(* -------------------------------------------------------------------------- *)

type verified_revocation = {
  delivery_id : string;
  app_id : int;
  github_user_id : int64;
  action : string;
  event : string;
}

type verify_denial =
  | Ingress of I.reject_reason * string
  | Duplicate_delivery of string
  | Wrong_event of string
  | Wrong_action of string
  | Missing_sender
  | Invalid_payload of string

let string_of_verify_denial = function
  | Ingress (r, msg) ->
      Printf.sprintf "ingress:%s:%s" (I.reject_reason_to_string r) msg
  | Duplicate_delivery id -> Printf.sprintf "duplicate_delivery:%s" id
  | Wrong_event e -> Printf.sprintf "wrong_event:%s" e
  | Wrong_action a -> Printf.sprintf "wrong_action:%s" a
  | Missing_sender -> "missing_sender"
  | Invalid_payload msg -> Printf.sprintf "invalid_payload:%s" msg

type verifier =
  db:Sqlite3.db ->
  webhook_secret:string ->
  expected_app_id:int ->
  ?now:float ->
  request:I.request ->
  unit ->
  (verified_revocation, verify_denial) result

type binding_effect = {
  binding_id : string;
  principal_id : string;
  host : string;
  vault_id : string option;
  prior_generation : int option;
  new_generation : int option;
  already_revoked : bool;
  secrets_destroyed : bool;
  leases_invalidated : int;
}

type receipt = {
  id : string;
  delivery_id : string;
  app_id : int;
  github_user_id : int64;
  action : string;
  bindings_matched : int;
  bindings_revoked : int;
  secrets_destroyed : int;
  leases_invalidated : int;
  orphan_secrets_destroyed : int;
  already_processed : bool;
  effects : binding_effect list;
  created_at : string;
}

type outcome =
  | Applied of receipt
  | Duplicate of receipt
  | Ignored of { reason : string; message : string }

type denial =
  | Verify of verify_denial
  | Binding of string
  | Vault of V.denial
  | Cas of C.denial
  | Storage of string
  | Invalid_input of string

let string_of_denial = function
  | Verify d -> "verify:" ^ string_of_verify_denial d
  | Binding msg -> "binding:" ^ msg
  | Vault d -> "vault:" ^ V.string_of_denial d
  | Cas d -> "cas:" ^ C.string_of_denial d
  | Storage msg -> "storage:" ^ msg
  | Invalid_input msg -> "invalid_input:" ^ msg

let denial_exposes_token ~denial ~plaintext =
  if plaintext = "" then false
  else String_util.contains (string_of_denial denial) plaintext

(* -------------------------------------------------------------------------- *)
(* JSON helpers                                                               *)
(* -------------------------------------------------------------------------- *)

let json_int64 = function
  | `Int n -> Some (Int64.of_int n)
  | `Intlit s -> ( try Some (Int64.of_string s) with _ -> None)
  | `Float f when Float.is_integer f -> Some (Int64.of_float f)
  | _ -> None

let binding_effect_to_json (e : binding_effect) : Yojson.Safe.t =
  `Assoc
    [
      ("binding_id", `String e.binding_id);
      ("principal_id", `String e.principal_id);
      ("host", `String e.host);
      ("vault_id", match e.vault_id with Some id -> `String id | None -> `Null);
      ( "prior_generation",
        match e.prior_generation with Some g -> `Int g | None -> `Null );
      ( "new_generation",
        match e.new_generation with Some g -> `Int g | None -> `Null );
      ("already_revoked", `Bool e.already_revoked);
      ("secrets_destroyed", `Bool e.secrets_destroyed);
      ("leases_invalidated", `Int e.leases_invalidated);
    ]

let binding_effect_of_json = function
  | `Assoc fields -> (
      let get_s k =
        match List.assoc_opt k fields with
        | Some (`String s) -> Some s
        | _ -> None
      in
      let get_b k =
        match List.assoc_opt k fields with Some (`Bool b) -> b | _ -> false
      in
      let get_i k =
        match List.assoc_opt k fields with
        | Some (`Int n) -> Some n
        | Some (`Intlit s) -> ( try Some (int_of_string s) with _ -> None)
        | _ -> None
      in
      match (get_s "binding_id", get_s "principal_id", get_s "host") with
      | Some binding_id, Some principal_id, Some host ->
          Ok
            {
              binding_id;
              principal_id;
              host;
              vault_id = get_s "vault_id";
              prior_generation = get_i "prior_generation";
              new_generation = get_i "new_generation";
              already_revoked = get_b "already_revoked";
              secrets_destroyed = get_b "secrets_destroyed";
              leases_invalidated =
                Option.value (get_i "leases_invalidated") ~default:0;
            }
      | _ -> Error "binding_effect missing required fields")
  | _ -> Error "binding_effect must be object"

let receipt_to_json (r : receipt) : Yojson.Safe.t =
  `Assoc
    [
      ("id", `String r.id);
      ("delivery_id", `String r.delivery_id);
      ("app_id", `Int r.app_id);
      ("github_user_id", `Intlit (Int64.to_string r.github_user_id));
      ("action", `String r.action);
      ("bindings_matched", `Int r.bindings_matched);
      ("bindings_revoked", `Int r.bindings_revoked);
      ("secrets_destroyed", `Int r.secrets_destroyed);
      ("leases_invalidated", `Int r.leases_invalidated);
      ("orphan_secrets_destroyed", `Int r.orphan_secrets_destroyed);
      ("already_processed", `Bool r.already_processed);
      ("effects", `List (List.map binding_effect_to_json r.effects));
      ("created_at", `String r.created_at);
    ]

let receipt_of_json = function
  | `Assoc fields -> (
      let get_s k =
        match List.assoc_opt k fields with
        | Some (`String s) -> Some s
        | _ -> None
      in
      let get_i k =
        match List.assoc_opt k fields with
        | Some (`Int n) -> Some n
        | Some (`Intlit s) -> ( try Some (int_of_string s) with _ -> None)
        | _ -> None
      in
      let get_i64 k =
        match List.assoc_opt k fields with
        | Some j -> json_int64 j
        | None -> None
      in
      let get_b k =
        match List.assoc_opt k fields with Some (`Bool b) -> b | _ -> false
      in
      match
        ( get_s "id",
          get_s "delivery_id",
          get_i "app_id",
          get_i64 "github_user_id",
          get_s "action",
          get_s "created_at" )
      with
      | ( Some id,
          Some delivery_id,
          Some app_id,
          Some github_user_id,
          Some action,
          Some created_at ) -> (
          let effects_res =
            match List.assoc_opt "effects" fields with
            | Some (`List xs) ->
                let rec loop acc = function
                  | [] -> Ok (List.rev acc)
                  | j :: rest -> (
                      match binding_effect_of_json j with
                      | Ok e -> loop (e :: acc) rest
                      | Error e -> Error e)
                in
                loop [] xs
            | Some _ -> Error "effects must be array"
            | None -> Ok []
          in
          match effects_res with
          | Error e -> Error e
          | Ok effects ->
              Ok
                {
                  id;
                  delivery_id;
                  app_id;
                  github_user_id;
                  action;
                  bindings_matched =
                    Option.value (get_i "bindings_matched") ~default:0;
                  bindings_revoked =
                    Option.value (get_i "bindings_revoked") ~default:0;
                  secrets_destroyed =
                    Option.value (get_i "secrets_destroyed") ~default:0;
                  leases_invalidated =
                    Option.value (get_i "leases_invalidated") ~default:0;
                  orphan_secrets_destroyed =
                    Option.value (get_i "orphan_secrets_destroyed") ~default:0;
                  already_processed = get_b "already_processed";
                  effects;
                  created_at;
                })
      | _ -> Error "receipt missing required fields")
  | _ -> Error "receipt must be object"

let receipt_contains_plaintext ~receipt ~plaintext =
  if plaintext = "" then false
  else
    let s = Yojson.Safe.to_string (receipt_to_json receipt) in
    String_util.contains s plaintext

let string_of_receipt (r : receipt) =
  Printf.sprintf
    "revocation receipt id=%s delivery=%s app=%d user=%Ld matched=%d \
     revoked=%d secrets_destroyed=%d leases=%d orphans=%d already=%b"
    r.id r.delivery_id r.app_id r.github_user_id r.bindings_matched
    r.bindings_revoked r.secrets_destroyed r.leases_invalidated
    r.orphan_secrets_destroyed r.already_processed

(* -------------------------------------------------------------------------- *)
(* Schema                                                                     *)
(* -------------------------------------------------------------------------- *)

let generate_receipt_id ?(now = Unix.gettimeofday ()) () =
  let ts = int_of_float now in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "ghrev_%d_%06d" ts rand

let ensure_schema db =
  Sqlite3.busy_timeout db 5_000;
  I.ensure_schema db;
  B.ensure_schema db;
  V.ensure_schema db;
  let table_sql =
    {|CREATE TABLE IF NOT EXISTS github_user_auth_revocation_receipts (
      delivery_id TEXT PRIMARY KEY NOT NULL,
      id TEXT NOT NULL UNIQUE,
      app_id INTEGER NOT NULL,
      github_user_id INTEGER NOT NULL,
      action TEXT NOT NULL,
      receipt_json TEXT NOT NULL,
      created_at TEXT NOT NULL
    )|}
  in
  let idx =
    {|CREATE INDEX IF NOT EXISTS idx_gh_user_auth_revocation_user
      ON github_user_auth_revocation_receipts(app_id, github_user_id)|}
  in
  List.iter
    (fun sql ->
      match Sqlite3.exec db sql with
      | Sqlite3.Rc.OK -> ()
      | rc ->
          failwith
            (Printf.sprintf
               "github_user_auth_revocation_webhook schema error: %s (sql: %s)"
               (Sqlite3.Rc.to_string rc) sql))
    [ table_sql; idx ]

let get_receipt_by_delivery ~db ~delivery_id =
  let delivery_id = String.trim delivery_id in
  if delivery_id = "" then Error "delivery_id must be non-empty"
  else
    let sql =
      {|SELECT receipt_json FROM github_user_auth_revocation_receipts
        WHERE delivery_id = ? LIMIT 1|}
    in
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT delivery_id));
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> (
            let raw =
              match Sqlite3.column stmt 0 with
              | Sqlite3.Data.TEXT s -> s
              | _ -> ""
            in
            try
              match receipt_of_json (Yojson.Safe.from_string raw) with
              | Ok r -> Ok (Some { r with already_processed = true })
              | Error e -> Error e
            with Yojson.Json_error msg -> Error msg)
        | Sqlite3.Rc.DONE -> Ok None
        | rc ->
            Error
              (Printf.sprintf "get_receipt_by_delivery failed: %s (%s)"
                 (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let insert_receipt ~db ~(receipt : receipt) =
  let sql =
    {|INSERT INTO github_user_auth_revocation_receipts
        (delivery_id, id, app_id, github_user_id, action, receipt_json, created_at)
      VALUES (?, ?, ?, ?, ?, ?, ?)|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let bind i d = ignore (Sqlite3.bind stmt i d) in
      bind 1 (Sqlite3.Data.TEXT receipt.delivery_id);
      bind 2 (Sqlite3.Data.TEXT receipt.id);
      bind 3 (Sqlite3.Data.INT (Int64.of_int receipt.app_id));
      bind 4 (Sqlite3.Data.INT receipt.github_user_id);
      bind 5 (Sqlite3.Data.TEXT receipt.action);
      bind 6
        (Sqlite3.Data.TEXT (Yojson.Safe.to_string (receipt_to_json receipt)));
      bind 7 (Sqlite3.Data.TEXT receipt.created_at);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok ()
      | Sqlite3.Rc.CONSTRAINT -> Error `Duplicate
      | rc ->
          Error
            (`Db
               (Printf.sprintf "insert revocation receipt failed: %s (%s)"
                  (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))))

(* -------------------------------------------------------------------------- *)
(* Verifier                                                                   *)
(* -------------------------------------------------------------------------- *)

let extract_sender_id (payload : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  match member "sender" payload with
  | `Null -> None
  | sender -> json_int64 (member "id" sender)

let parse_verified_from_accepted ~(accepted : I.accepted) ~expected_app_id =
  let event = String.trim accepted.event in
  if not (String.equal event "github_app_authorization") then
    Error (Wrong_event event)
  else
    let action =
      match accepted.action with
      | Some a -> String.trim a
      | None -> (
          match Yojson.Safe.Util.member "action" accepted.payload with
          | `String s -> String.trim s
          | _ -> "")
    in
    if action = "" then Error (Invalid_payload "missing action")
    else if not (String.equal action "revoked") then Error (Wrong_action action)
    else
      match extract_sender_id accepted.payload with
      | None -> Error Missing_sender
      | Some github_user_id when github_user_id <= 0L ->
          Error (Invalid_payload "sender.id must be positive")
      | Some github_user_id ->
          let app_id =
            match accepted.app_id with
            | Some id when id > 0 -> id
            | _ -> expected_app_id
          in
          if app_id <= 0 then Error (Invalid_payload "app_id must be positive")
          else
            Ok
              {
                delivery_id = accepted.delivery_id;
                app_id;
                github_user_id;
                action;
                event;
              }

let default_verifier ~db ~webhook_secret ~expected_app_id
    ?(now = Unix.gettimeofday ()) ~request () =
  match
    I.verify_and_accept ~db ~webhook_secret ~expected_app_id ~now request
  with
  | I.Rejected { reason; message } -> Error (Ingress (reason, message))
  | I.Duplicate { delivery_id } -> Error (Duplicate_delivery delivery_id)
  | I.Accepted accepted ->
      parse_verified_from_accepted ~accepted ~expected_app_id

(* -------------------------------------------------------------------------- *)
(* Binding / vault effects                                                    *)
(* -------------------------------------------------------------------------- *)

let account_of_binding (b : B.binding) : V.account_key =
  {
    principal_id = Principal_identity.principal_id_to_string b.principal_id;
    github_user_id = b.identity.github_user_id;
    app_id = b.identity.app_id;
    host = b.identity.host;
  }

let mark_binding_revoked ~db ~now ~(binding : B.binding) =
  match binding.authorization_status with
  | B.Revoked -> Ok (binding, true)
  | _ -> (
      match
        B.update_authorization_status ~db ~expected_revision:binding.revision
          ~now ~id:binding.id ~status:B.Revoked ()
      with
      | Error e -> Error (Binding e)
      | Ok updated -> Ok (updated, false))

let destroy_vault_secret ~db ~vault_id =
  match V.destroy ~db ~id:vault_id with
  | Ok () -> Ok true
  | Error V.Not_found -> Ok false
  | Error d -> Error (Vault d)

let list_vault_ids_for_app_user ~db ~app_id ~github_user_id =
  let sql =
    {|SELECT id FROM github_user_token_vault
      WHERE app_id = ? AND github_user_id = ?
      ORDER BY id ASC|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int app_id)));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT github_user_id));
      let rec loop acc =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> (
            match Sqlite3.column stmt 0 with
            | Sqlite3.Data.TEXT id -> loop (id :: acc)
            | _ -> loop acc)
        | Sqlite3.Rc.DONE -> Ok (List.rev acc)
        | rc ->
            Error
              (Storage
                 (Printf.sprintf "list vault ids failed: %s (%s)"
                    (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))
      in
      loop [])

let revoke_one_binding ~db ~keys ~now ~(binding : B.binding) :
    (binding_effect, denial) result =
  let principal_id =
    Principal_identity.principal_id_to_string binding.principal_id
  in
  let host = binding.identity.host in
  let vault_id_opt =
    match binding.vault_ref with
    | Some vr -> Some (B.vault_ref_to_string vr)
    | None -> None
  in
  match vault_id_opt with
  | None -> (
      match mark_binding_revoked ~db ~now ~binding with
      | Error e -> Error e
      | Ok (_updated, already) ->
          Ok
            {
              binding_id = binding.id;
              principal_id;
              host;
              vault_id = None;
              prior_generation = None;
              new_generation = None;
              already_revoked = already;
              secrets_destroyed = false;
              leases_invalidated = 0;
            })
  | Some vault_id -> (
      match V.get_meta ~db ~id:vault_id with
      | Error V.Not_found -> (
          (* Binding points at missing vault: still revoke binding status. *)
          match mark_binding_revoked ~db ~now ~binding with
          | Error e -> Error e
          | Ok (_updated, already) ->
              Ok
                {
                  binding_id = binding.id;
                  principal_id;
                  host;
                  vault_id = Some vault_id;
                  prior_generation = None;
                  new_generation = None;
                  already_revoked = already;
                  secrets_destroyed = false;
                  leases_invalidated = 0;
                })
      | Error d -> Error (Vault d)
      | Ok None -> (
          match mark_binding_revoked ~db ~now ~binding with
          | Error e -> Error e
          | Ok (_updated, already) ->
              Ok
                {
                  binding_id = binding.id;
                  principal_id;
                  host;
                  vault_id = Some vault_id;
                  prior_generation = None;
                  new_generation = None;
                  already_revoked = already;
                  secrets_destroyed = false;
                  leases_invalidated = 0;
                })
      | Ok (Some meta) -> (
          let expected = account_of_binding binding in
          let prior_generation = meta.generation in
          let already_inactive = not meta.active in
          let cas_result =
            if already_inactive then
              (* Vault already inactive: ensure binding is Revoked without CAS. *)
              match mark_binding_revoked ~db ~now ~binding with
              | Error e -> Error e
              | Ok (_b, already_binding) ->
                  let leases = L.discard_for_vault ~vault_id in
                  Ok
                    (prior_generation, prior_generation, already_binding, leases)
            else
              match
                C.revoke ~db ~keys ~now ~id:vault_id
                  ~expected_generation:prior_generation ~expected
                  ~binding_id:binding.id ()
              with
              | Error (C.Vault (V.Active_conflict _ | V.Not_active))
              | Error (C.Vault (V.Generation_conflict _)) -> (
                  (* Concurrent revoke or race: fall back to mark + continue. *)
                  match mark_binding_revoked ~db ~now ~binding with
                  | Error e -> Error e
                  | Ok (_b, already_binding) ->
                      let leases = L.discard_for_vault ~vault_id in
                      Ok
                        ( prior_generation,
                          prior_generation,
                          already_binding,
                          leases ))
              | Error d -> Error (Cas d)
              | Ok t ->
                  Ok
                    ( prior_generation,
                      t.record.generation,
                      false,
                      t.leases_invalidated )
          in
          match cas_result with
          | Error e -> Error e
          | Ok (prior_gen, new_gen, already_revoked, leases_invalidated) -> (
              match destroy_vault_secret ~db ~vault_id with
              | Error e -> Error e
              | Ok destroyed ->
                  (* Clear vault_ref on binding after secret destruction so
                     stale handles cannot be re-used. *)
                  let _clear =
                    match B.get ~db ~id:binding.id with
                    | Ok (Some b) when b.vault_ref <> None ->
                        ignore
                          (B.set_vault_ref ~db ~expected_revision:b.revision
                             ~now ~id:b.id ~vault_ref:None ())
                    | _ -> ()
                  in
                  Ok
                    {
                      binding_id = binding.id;
                      principal_id;
                      host;
                      vault_id = Some vault_id;
                      prior_generation = Some prior_gen;
                      new_generation = Some new_gen;
                      already_revoked;
                      secrets_destroyed = destroyed;
                      leases_invalidated;
                    })))

let destroy_orphan_vaults ~db ~app_id ~github_user_id ~known_vault_ids =
  match list_vault_ids_for_app_user ~db ~app_id ~github_user_id with
  | Error e -> Error e
  | Ok ids ->
      let known =
        List.fold_left
          (fun acc id ->
            Hashtbl.replace acc id ();
            acc)
          (Hashtbl.create 8) known_vault_ids
      in
      let rec go acc = function
        | [] -> Ok acc
        | id :: rest -> (
            if Hashtbl.mem known id then go acc rest
            else
              (* Fail closed: discard leases, then destroy sealed row. *)
              let _ = L.discard_for_vault ~vault_id:id in
              match destroy_vault_secret ~db ~vault_id:id with
              | Error e -> Error e
              | Ok true -> go (acc + 1) rest
              | Ok false -> go acc rest)
      in
      go 0 ids

(* -------------------------------------------------------------------------- *)
(* Process                                                                    *)
(* -------------------------------------------------------------------------- *)

let process_verified ~db ~keys ?(now = Unix.gettimeofday ())
    ~(verified : verified_revocation) () =
  ensure_schema db;
  let delivery_id = String.trim verified.delivery_id in
  let app_id = verified.app_id in
  let github_user_id = verified.github_user_id in
  let action = String.trim verified.action in
  let event = String.trim verified.event in
  if delivery_id = "" then Error (Invalid_input "delivery_id must be non-empty")
  else if app_id <= 0 then Error (Invalid_input "app_id must be positive")
  else if github_user_id <= 0L then
    Error (Invalid_input "github_user_id must be positive")
  else
    match get_receipt_by_delivery ~db ~delivery_id with
    | Error e -> Error (Storage e)
    | Ok (Some prior) -> Ok (Duplicate prior)
    | Ok None -> (
        if not (String.equal event "github_app_authorization") then
          Ok
            (Ignored
               {
                 reason = "wrong_event";
                 message =
                   Printf.sprintf "event %S is not github_app_authorization"
                     event;
               })
        else if not (String.equal action "revoked") then
          Ok
            (Ignored
               {
                 reason = "wrong_action";
                 message =
                   Printf.sprintf
                     "action %S is not revoked; no bindings mutated" action;
               })
        else
          match B.list_for_app_user ~db ~app_id ~github_user_id () with
          | Error e -> Error (Binding e)
          | Ok bindings -> (
              let rec apply acc = function
                | [] -> Ok (List.rev acc)
                | b :: rest -> (
                    match revoke_one_binding ~db ~keys ~now ~binding:b with
                    | Error e -> Error e
                    | Ok effect -> apply (effect :: acc) rest)
              in
              match apply [] bindings with
              | Error e -> Error e
              | Ok effects -> (
                  let known_vault_ids =
                    List.filter_map
                      (fun (e : binding_effect) -> e.vault_id)
                      effects
                  in
                  match
                    destroy_orphan_vaults ~db ~app_id ~github_user_id
                      ~known_vault_ids
                  with
                  | Error e -> Error e
                  | Ok orphan_secrets_destroyed -> (
                      let bindings_matched = List.length effects in
                      let bindings_revoked =
                        List.fold_left
                          (fun n (e : binding_effect) ->
                            if e.already_revoked then n else n + 1)
                          0 effects
                      in
                      let secrets_destroyed =
                        List.fold_left
                          (fun n (e : binding_effect) ->
                            if e.secrets_destroyed then n + 1 else n)
                          0 effects
                      in
                      let leases_invalidated =
                        List.fold_left
                          (fun n (e : binding_effect) ->
                            n + e.leases_invalidated)
                          0 effects
                      in
                      let created_at = Time_util.iso8601_utc ~t:now () in
                      let receipt =
                        {
                          id = generate_receipt_id ~now ();
                          delivery_id;
                          app_id;
                          github_user_id;
                          action;
                          bindings_matched;
                          bindings_revoked;
                          secrets_destroyed;
                          leases_invalidated;
                          orphan_secrets_destroyed;
                          already_processed = false;
                          effects;
                          created_at;
                        }
                      in
                      match insert_receipt ~db ~receipt with
                      | Ok () -> Ok (Applied receipt)
                      | Error `Duplicate -> (
                          match get_receipt_by_delivery ~db ~delivery_id with
                          | Ok (Some prior) -> Ok (Duplicate prior)
                          | Ok None ->
                              Error
                                (Storage
                                   "receipt insert raced but prior missing")
                          | Error e -> Error (Storage e))
                      | Error (`Db msg) -> Error (Storage msg)))))

let process ~db ~keys ~webhook_secret ~expected_app_id
    ?(verify = default_verifier) ?(now = Unix.gettimeofday ()) ~request () =
  ensure_schema db;
  match verify ~db ~webhook_secret ~expected_app_id ~now ~request () with
  | Error (Duplicate_delivery delivery_id) -> (
      match get_receipt_by_delivery ~db ~delivery_id with
      | Ok (Some prior) -> Ok (Duplicate prior)
      | Ok None ->
          (* Ingress reserved the delivery but no receipt yet (partial prior
             failure, or non-revocation event). Surface as verify denial. *)
          Error (Verify (Duplicate_delivery delivery_id))
      | Error e -> Error (Storage e))
  | Error d -> Error (Verify d)
  | Ok verified -> process_verified ~db ~keys ~now ~verified ()
