(* Shared verified pending-credential activation transaction (P21.M2.E2.T004).
   See github_user_auth_activate.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module Tx = Github_user_auth_tx
module V = Github_user_token_vault
module B = Github_account_binding
module S = Github_user_token_store
module Op = Github_account_ownership_policy
module P = Principal_identity
module PS = Principal_identity_store

let schema_version = 1
let default_ttl_seconds = 900.0

(* -------------------------------------------------------------------------- *)
(* Types                                                                      *)
(* -------------------------------------------------------------------------- *)

type pending_credential = {
  access_token : string;
  refresh_token : string option;
  scopes : string list;
  expires_in : int;
  token_type : string option;
}

type github_user = { id : int64; login : string; avatar_url : string option }
type fetch_user = access_token:string -> (github_user, string) result

type activation_status =
  | Pending_confirmation
  | Activated
  | Destroyed
  | Expired
  | Cancelled
  | Rejected

type activation_mode = New_binding | Supersede_pending

type redacted_plan = {
  plan_id : string;
  digest : string;
  principal_id : string;
  principal_revision : int;
  base_revision : string;
  flow_kind : Tx.flow_kind;
  auth_tx_id : string;
  host : string;
  app_id : int;
  github_user_id : int64;
  login : string;
  avatar_url : string option;
  scopes : string list;
  vault_id : string;
  vault_generation : int;
  binding_id : string;
  binding_revision : int;
  mode : activation_mode;
  created_at : string;
  expires_at : string;
}

type activation = {
  version : int;
  id : string;
  status : activation_status;
  principal_id : string;
  principal_revision : int;
  flow_kind : Tx.flow_kind;
  auth_tx_id : string;
  base_revision : string;
  host : string;
  app_id : int;
  github_user_id : int64;
  login : string;
  avatar_url : string option;
  scopes : string list;
  vault_id : string;
  vault_generation : int;
  binding_id : string;
  binding_revision : int;
  mode : activation_mode;
  plan_id : string;
  plan_digest : string;
  created_at : string;
  expires_at : string;
  activated_at : string option;
  destroyed_at : string option;
  terminal_reason : string option;
  updated_at : string;
}

type prepared = {
  activation : activation;
  plan : redacted_plan;
  confirmation_token : string;
  vault : V.vault_record;
  binding : B.binding;
  github_user : github_user;
}

type activated = {
  activation : activation;
  plan : redacted_plan;
  vault : V.vault_record;
  binding : B.binding;
}

type failure_kind =
  | Invalid_credential of string
  | Incomplete_exchange
  | Replay
  | Expired
  | Cancelled
  | Principal_changed of string
  | Identity_mismatch of string
  | User_probe of string
  | Collision of string
  | Confirmation_mismatch
  | Plan_mismatch
  | Not_found
  | Already_activated
  | Destroyed_status
  | Partial of string
  | Storage of string
  | Invalid of string

type failure = {
  kind : failure_kind;
  message : string;
  activation : activation option;
}

type row = {
  act : activation;
  confirmation_token_hash : string;
  plan_json : string;
}

(* -------------------------------------------------------------------------- *)
(* Codecs                                                                     *)
(* -------------------------------------------------------------------------- *)

let string_of_activation_status = function
  | Pending_confirmation -> "pending_confirmation"
  | Activated -> "activated"
  | Destroyed -> "destroyed"
  | Expired -> "expired"
  | Cancelled -> "cancelled"
  | Rejected -> "rejected"

let activation_status_of_string = function
  | "pending_confirmation" -> Ok Pending_confirmation
  | "activated" -> Ok Activated
  | "destroyed" -> Ok Destroyed
  | "expired" -> Ok Expired
  | "cancelled" -> Ok Cancelled
  | "rejected" -> Ok Rejected
  | s -> Error (Printf.sprintf "unknown github_user_auth_activate status: %s" s)

let status_is_terminal = function Pending_confirmation -> false | _ -> true

let string_of_activation_mode = function
  | New_binding -> "new_binding"
  | Supersede_pending -> "supersede_pending"

let activation_mode_of_string = function
  | "new_binding" -> Ok New_binding
  | "supersede_pending" -> Ok Supersede_pending
  | s -> Error (Printf.sprintf "unknown activation_mode: %s" s)

let string_of_failure_kind = function
  | Invalid_credential _ -> "invalid_credential"
  | Incomplete_exchange -> "incomplete_exchange"
  | Replay -> "replay"
  | Expired -> "expired"
  | Cancelled -> "cancelled"
  | Principal_changed _ -> "principal_changed"
  | Identity_mismatch _ -> "identity_mismatch"
  | User_probe _ -> "user_probe"
  | Collision _ -> "collision"
  | Confirmation_mismatch -> "confirmation_mismatch"
  | Plan_mismatch -> "plan_mismatch"
  | Not_found -> "not_found"
  | Already_activated -> "already_activated"
  | Destroyed_status -> "destroyed_status"
  | Partial _ -> "partial"
  | Storage _ -> "storage"
  | Invalid _ -> "invalid"

let fail ?(activation = None) kind message = Error { kind; message; activation }

let has_active_binding ~(binding : B.binding) =
  match binding.B.authorization_status with B.Authorized -> true | _ -> false

let is_activated (a : activation) =
  match a.status with Activated -> true | _ -> false

(* -------------------------------------------------------------------------- *)
(* Pending credential validation                                              *)
(* -------------------------------------------------------------------------- *)

let make_pending_credential ~access_token ?refresh_token ?(scopes = [])
    ~expires_in ?token_type () =
  let access_token = String.trim access_token in
  if access_token = "" then Error "access_token must be non-empty"
  else if expires_in <= 0 then Error "expires_in must be positive"
  else
    let refresh_token =
      match refresh_token with
      | Some r when String.trim r = "" -> None
      | Some r -> Some (String.trim r)
      | None -> None
    in
    let token_type =
      match token_type with
      | Some t when String.trim t = "" -> None
      | Some t -> Some (String.trim t)
      | None -> None
    in
    let scopes =
      scopes |> List.map String.trim |> List.filter (fun s -> s <> "")
    in
    Ok { access_token; refresh_token; scopes; expires_in; token_type }

(* Web/device projection helpers that accepted flow-specific token types were
   removed: they created a dependency cycle with
   Github_user_auth_pkce_callback / device_poll. Callers project through
   [make_pending_credential] instead. *)

(* -------------------------------------------------------------------------- *)
(* Crypto / ids                                                               *)
(* -------------------------------------------------------------------------- *)

let ensure_rng = lazy (Mirage_crypto_rng_unix.use_default ())

let generate_hex n =
  Lazy.force ensure_rng;
  Digestif.SHA256.(digest_string (Mirage_crypto_rng.generate n) |> to_hex)

let generate_id ?(now = Unix.gettimeofday ()) () =
  let ts = int_of_float now in
  Printf.sprintf "gh_user_auth_act_%d_%s" ts (String.sub (generate_hex 8) 0 12)

let generate_confirmation_token () = generate_hex 32

let hash_confirmation_token token =
  Digestif.SHA256.(digest_string token |> to_hex)

let constant_time_equal a b = Eqaf.equal a b
let now_iso ?(now = Unix.gettimeofday ()) () = Time_util.iso8601_utc ~t:now ()

let expires_at_iso ~now ~expires_in =
  Time_util.iso8601_utc ~t:(now +. float_of_int expires_in) ()

let is_expired_iso ~now expires_at =
  let now_s = now_iso ~now () in
  String.compare now_s expires_at >= 0

(* -------------------------------------------------------------------------- *)
(* Redacted plan                                                              *)
(* -------------------------------------------------------------------------- *)

let scopes_to_json scopes = `List (List.map (fun s -> `String s) scopes)

let scopes_of_json = function
  | `List items ->
      let rec go acc = function
        | [] -> Ok (List.rev acc)
        | `String s :: rest -> go (s :: acc) rest
        | _ -> Error "scopes must be a JSON string list"
      in
      go [] items
  | _ -> Error "scopes must be a JSON list"

let redacted_plan_body (p : redacted_plan) : Yojson.Safe.t =
  `Assoc
    [
      ("plan_id", `String p.plan_id);
      ("principal_id", `String p.principal_id);
      ("principal_revision", `Int p.principal_revision);
      ("base_revision", `String p.base_revision);
      ("flow_kind", `String (Tx.string_of_flow_kind p.flow_kind));
      ("auth_tx_id", `String p.auth_tx_id);
      ("host", `String p.host);
      ("app_id", `Int p.app_id);
      ("github_user_id", `Intlit (Int64.to_string p.github_user_id));
      ("login", `String p.login);
      ( "avatar_url",
        match p.avatar_url with None -> `Null | Some u -> `String u );
      ("scopes", scopes_to_json p.scopes);
      ("vault_id", `String p.vault_id);
      ("vault_generation", `Int p.vault_generation);
      ("binding_id", `String p.binding_id);
      ("binding_revision", `Int p.binding_revision);
      ("mode", `String (string_of_activation_mode p.mode));
      ("created_at", `String p.created_at);
      ("expires_at", `String p.expires_at);
    ]

let compute_plan_digest (p : redacted_plan) =
  let body = redacted_plan_body p |> Yojson.Safe.to_string in
  Digestif.SHA256.(digest_string body |> to_hex)

let redacted_plan_to_json (p : redacted_plan) =
  match redacted_plan_body p with
  | `Assoc fields -> `Assoc (("digest", `String p.digest) :: fields)
  | other -> other

let json_assoc = function `Assoc fields -> fields | _ -> []

let json_string fields name =
  match List.assoc_opt name fields with
  | Some (`String s) when String.trim s <> "" -> Ok (String.trim s)
  | Some (`String _) -> Error (name ^ " must be non-empty")
  | _ -> Error (name ^ " missing or not a string")

let json_int fields name =
  match List.assoc_opt name fields with
  | Some (`Int n) -> Ok n
  | Some (`Intlit s) -> (
      match int_of_string_opt s with
      | Some n -> Ok n
      | None -> Error (name ^ " not an int"))
  | _ -> Error (name ^ " missing or not an int")

let json_int64 fields name =
  match List.assoc_opt name fields with
  | Some (`Int n) -> Ok (Int64.of_int n)
  | Some (`Intlit s) -> (
      match Int64.of_string_opt s with
      | Some n -> Ok n
      | None -> Error (name ^ " not an int64"))
  | _ -> Error (name ^ " missing or not an int64")

let redacted_plan_of_json (j : Yojson.Safe.t) =
  let fields = json_assoc j in
  let ( let* ) = Result.bind in
  let* plan_id = json_string fields "plan_id" in
  let* principal_id = json_string fields "principal_id" in
  let* principal_revision = json_int fields "principal_revision" in
  let* base_revision = json_string fields "base_revision" in
  let* fk_s = json_string fields "flow_kind" in
  let* flow_kind = Tx.flow_kind_of_string fk_s in
  let* auth_tx_id = json_string fields "auth_tx_id" in
  let* host = json_string fields "host" in
  let* app_id = json_int fields "app_id" in
  let* github_user_id = json_int64 fields "github_user_id" in
  let* login = json_string fields "login" in
  let avatar_url =
    match List.assoc_opt "avatar_url" fields with
    | Some (`String u) -> Some u
    | _ -> None
  in
  let* scopes =
    match List.assoc_opt "scopes" fields with
    | None -> Error "scopes missing"
    | Some scopes_j -> scopes_of_json scopes_j
  in
  let* vault_id = json_string fields "vault_id" in
  let* vault_generation = json_int fields "vault_generation" in
  let* binding_id = json_string fields "binding_id" in
  let* binding_revision = json_int fields "binding_revision" in
  let* mode_s = json_string fields "mode" in
  let* mode = activation_mode_of_string mode_s in
  let* created_at = json_string fields "created_at" in
  let* expires_at = json_string fields "expires_at" in
  let digest =
    match List.assoc_opt "digest" fields with Some (`String d) -> d | _ -> ""
  in
  Ok
    {
      plan_id;
      digest;
      principal_id;
      principal_revision;
      base_revision;
      flow_kind;
      auth_tx_id;
      host;
      app_id;
      github_user_id;
      login;
      avatar_url;
      scopes;
      vault_id;
      vault_generation;
      binding_id;
      binding_revision;
      mode;
      created_at;
      expires_at;
    }

(* -------------------------------------------------------------------------- *)
(* Schema                                                                     *)
(* -------------------------------------------------------------------------- *)

let ensure_schema db =
  Tx.ensure_schema db;
  V.ensure_schema db;
  B.ensure_schema db;
  PS.ensure_schema db;
  let table =
    {|CREATE TABLE IF NOT EXISTS github_user_auth_activate (
      id TEXT PRIMARY KEY NOT NULL,
      version INTEGER NOT NULL,
      status TEXT NOT NULL,
      principal_id TEXT NOT NULL,
      principal_revision INTEGER NOT NULL,
      flow_kind TEXT NOT NULL,
      auth_tx_id TEXT NOT NULL UNIQUE,
      base_revision TEXT NOT NULL,
      host TEXT NOT NULL,
      app_id INTEGER NOT NULL,
      github_user_id INTEGER NOT NULL,
      login TEXT NOT NULL,
      avatar_url TEXT,
      scopes_json TEXT NOT NULL,
      vault_id TEXT NOT NULL,
      vault_generation INTEGER NOT NULL,
      binding_id TEXT NOT NULL,
      binding_revision INTEGER NOT NULL,
      mode TEXT NOT NULL,
      plan_id TEXT NOT NULL,
      plan_digest TEXT NOT NULL,
      plan_json TEXT NOT NULL,
      confirmation_token_hash TEXT NOT NULL,
      created_at TEXT NOT NULL,
      expires_at TEXT NOT NULL,
      activated_at TEXT,
      destroyed_at TEXT,
      terminal_reason TEXT,
      updated_at TEXT NOT NULL
    )|}
  in
  let idx_principal =
    {|CREATE INDEX IF NOT EXISTS idx_github_user_auth_activate_principal
      ON github_user_auth_activate(principal_id, status)|}
  in
  let idx_binding =
    {|CREATE INDEX IF NOT EXISTS idx_github_user_auth_activate_binding
      ON github_user_auth_activate(binding_id)|}
  in
  List.iter
    (fun sql ->
      match Sqlite3.exec db sql with
      | Sqlite3.Rc.OK -> ()
      | rc ->
          failwith
            (Printf.sprintf "github_user_auth_activate schema error: %s"
               (Sqlite3.Rc.to_string rc)))
    [ table; idx_principal; idx_binding ]

(* -------------------------------------------------------------------------- *)
(* SQLite helpers                                                             *)
(* -------------------------------------------------------------------------- *)

let text_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.TEXT s -> s
  | Sqlite3.Data.NULL -> ""
  | _ -> ""

let opt_text_col stmt i =
  match Sqlite3.column stmt i with Sqlite3.Data.TEXT s -> Some s | _ -> None

let int_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.INT n -> Int64.to_int n
  | Sqlite3.Data.TEXT s -> int_of_string s
  | _ -> 0

let int64_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.INT n -> n
  | Sqlite3.Data.TEXT s -> Int64.of_string s
  | _ -> 0L

let begin_immediate ~db =
  match Sqlite3.exec db "BEGIN IMMEDIATE" with
  | Sqlite3.Rc.OK -> Ok ()
  | rc ->
      Error
        (Printf.sprintf "BEGIN IMMEDIATE failed: %s (%s)"
           (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))

let rollback ~db = ignore (Sqlite3.exec db "ROLLBACK")

let commit ~db =
  match Sqlite3.exec db "COMMIT" with
  | Sqlite3.Rc.OK -> Ok ()
  | rc ->
      Error
        (Printf.sprintf "COMMIT failed: %s (%s)" (Sqlite3.Rc.to_string rc)
           (Sqlite3.errmsg db))

let row_of_stmt stmt : (row, string) result =
  let ( let* ) = Result.bind in
  let id = text_col stmt 0 in
  let version = int_col stmt 1 in
  let* status = activation_status_of_string (text_col stmt 2) in
  let principal_id = text_col stmt 3 in
  let principal_revision = int_col stmt 4 in
  let* flow_kind = Tx.flow_kind_of_string (text_col stmt 5) in
  let auth_tx_id = text_col stmt 6 in
  let base_revision = text_col stmt 7 in
  let host = text_col stmt 8 in
  let app_id = int_col stmt 9 in
  let github_user_id = int64_col stmt 10 in
  let login = text_col stmt 11 in
  let avatar_url = opt_text_col stmt 12 in
  let scopes_json = text_col stmt 13 in
  let* scopes =
    try scopes_of_json (Yojson.Safe.from_string scopes_json)
    with _ -> Error "scopes_json parse failed"
  in
  let vault_id = text_col stmt 14 in
  let vault_generation = int_col stmt 15 in
  let binding_id = text_col stmt 16 in
  let binding_revision = int_col stmt 17 in
  let* mode = activation_mode_of_string (text_col stmt 18) in
  let plan_id = text_col stmt 19 in
  let plan_digest = text_col stmt 20 in
  let plan_json = text_col stmt 21 in
  let confirmation_token_hash = text_col stmt 22 in
  let created_at = text_col stmt 23 in
  let expires_at = text_col stmt 24 in
  let activated_at = opt_text_col stmt 25 in
  let destroyed_at = opt_text_col stmt 26 in
  let terminal_reason = opt_text_col stmt 27 in
  let updated_at = text_col stmt 28 in
  Ok
    {
      act =
        {
          version;
          id;
          status;
          principal_id;
          principal_revision;
          flow_kind;
          auth_tx_id;
          base_revision;
          host;
          app_id;
          github_user_id;
          login;
          avatar_url;
          scopes;
          vault_id;
          vault_generation;
          binding_id;
          binding_revision;
          mode;
          plan_id;
          plan_digest;
          created_at;
          expires_at;
          activated_at;
          destroyed_at;
          terminal_reason;
          updated_at;
        };
      confirmation_token_hash;
      plan_json;
    }

let select_sql =
  {|SELECT id, version, status, principal_id, principal_revision, flow_kind,
           auth_tx_id, base_revision, host, app_id, github_user_id, login,
           avatar_url, scopes_json, vault_id, vault_generation, binding_id,
           binding_revision, mode, plan_id, plan_digest, plan_json,
           confirmation_token_hash, created_at, expires_at, activated_at,
           destroyed_at, terminal_reason, updated_at
    FROM github_user_auth_activate |}

let load_by_id ~db ~id : (row option, string) result =
  let sql = select_sql ^ "WHERE id = ? LIMIT 1" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match row_of_stmt stmt with Ok r -> Ok (Some r) | Error e -> Error e)
      | Sqlite3.Rc.DONE -> Ok None
      | rc ->
          Error
            (Printf.sprintf "activate SELECT by id failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let load_by_auth_tx ~db ~auth_tx_id : (row option, string) result =
  let sql = select_sql ^ "WHERE auth_tx_id = ? LIMIT 1" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT auth_tx_id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match row_of_stmt stmt with Ok r -> Ok (Some r) | Error e -> Error e)
      | Sqlite3.Rc.DONE -> Ok None
      | rc ->
          Error
            (Printf.sprintf "activate SELECT by auth_tx failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let insert_row ~db (r : row) : (unit, string) result =
  let a = r.act in
  let sql =
    {|INSERT INTO github_user_auth_activate
      (id, version, status, principal_id, principal_revision, flow_kind,
       auth_tx_id, base_revision, host, app_id, github_user_id, login,
       avatar_url, scopes_json, vault_id, vault_generation, binding_id,
       binding_revision, mode, plan_id, plan_digest, plan_json,
       confirmation_token_hash, created_at, expires_at, activated_at,
       destroyed_at, terminal_reason, updated_at)
      VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)|}
  in
  let stmt = Sqlite3.prepare db sql in
  let bind i v = ignore (Sqlite3.bind stmt i v) in
  bind 1 (Sqlite3.Data.TEXT a.id);
  bind 2 (Sqlite3.Data.INT (Int64.of_int a.version));
  bind 3 (Sqlite3.Data.TEXT (string_of_activation_status a.status));
  bind 4 (Sqlite3.Data.TEXT a.principal_id);
  bind 5 (Sqlite3.Data.INT (Int64.of_int a.principal_revision));
  bind 6 (Sqlite3.Data.TEXT (Tx.string_of_flow_kind a.flow_kind));
  bind 7 (Sqlite3.Data.TEXT a.auth_tx_id);
  bind 8 (Sqlite3.Data.TEXT a.base_revision);
  bind 9 (Sqlite3.Data.TEXT a.host);
  bind 10 (Sqlite3.Data.INT (Int64.of_int a.app_id));
  bind 11 (Sqlite3.Data.INT a.github_user_id);
  bind 12 (Sqlite3.Data.TEXT a.login);
  bind 13
    (match a.avatar_url with
    | None -> Sqlite3.Data.NULL
    | Some u -> Sqlite3.Data.TEXT u);
  bind 14 (Sqlite3.Data.TEXT (Yojson.Safe.to_string (scopes_to_json a.scopes)));
  bind 15 (Sqlite3.Data.TEXT a.vault_id);
  bind 16 (Sqlite3.Data.INT (Int64.of_int a.vault_generation));
  bind 17 (Sqlite3.Data.TEXT a.binding_id);
  bind 18 (Sqlite3.Data.INT (Int64.of_int a.binding_revision));
  bind 19 (Sqlite3.Data.TEXT (string_of_activation_mode a.mode));
  bind 20 (Sqlite3.Data.TEXT a.plan_id);
  bind 21 (Sqlite3.Data.TEXT a.plan_digest);
  bind 22 (Sqlite3.Data.TEXT r.plan_json);
  bind 23 (Sqlite3.Data.TEXT r.confirmation_token_hash);
  bind 24 (Sqlite3.Data.TEXT a.created_at);
  bind 25 (Sqlite3.Data.TEXT a.expires_at);
  bind 26
    (match a.activated_at with
    | None -> Sqlite3.Data.NULL
    | Some t -> Sqlite3.Data.TEXT t);
  bind 27
    (match a.destroyed_at with
    | None -> Sqlite3.Data.NULL
    | Some t -> Sqlite3.Data.TEXT t);
  bind 28
    (match a.terminal_reason with
    | None -> Sqlite3.Data.NULL
    | Some t -> Sqlite3.Data.TEXT t);
  bind 29 (Sqlite3.Data.TEXT a.updated_at);
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE -> Ok ()
  | rc ->
      let msg = Sqlite3.errmsg db in
      if
        String_util.contains (String.lowercase_ascii msg) "unique"
        || String_util.contains (String.lowercase_ascii msg) "constraint"
      then Error (Printf.sprintf "unique constraint: %s" msg)
      else
        Error
          (Printf.sprintf "activate insert failed: %s (%s)"
             (Sqlite3.Rc.to_string rc) msg)

let update_status ~db ~id ~status ?activated_at ?destroyed_at ?terminal_reason
    ~updated_at () =
  let sql =
    {|UPDATE github_user_auth_activate
      SET status = ?, activated_at = COALESCE(?, activated_at),
          destroyed_at = COALESCE(?, destroyed_at),
          terminal_reason = COALESCE(?, terminal_reason),
          updated_at = ?
      WHERE id = ?|}
  in
  let stmt = Sqlite3.prepare db sql in
  let bind i v = ignore (Sqlite3.bind stmt i v) in
  bind 1 (Sqlite3.Data.TEXT (string_of_activation_status status));
  bind 2
    (match activated_at with
    | None -> Sqlite3.Data.NULL
    | Some t -> Sqlite3.Data.TEXT t);
  bind 3
    (match destroyed_at with
    | None -> Sqlite3.Data.NULL
    | Some t -> Sqlite3.Data.TEXT t);
  bind 4
    (match terminal_reason with
    | None -> Sqlite3.Data.NULL
    | Some t -> Sqlite3.Data.TEXT t);
  bind 5 (Sqlite3.Data.TEXT updated_at);
  bind 6 (Sqlite3.Data.TEXT id);
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE -> Ok ()
  | rc ->
      Error
        (Printf.sprintf "activate status update failed: %s"
           (Sqlite3.Rc.to_string rc))

(* -------------------------------------------------------------------------- *)
(* Introspection                                                              *)
(* -------------------------------------------------------------------------- *)

let get ~db ~id =
  ensure_schema db;
  match load_by_id ~db ~id with
  | Error e -> fail (Storage e) e
  | Ok None -> Ok None
  | Ok (Some r) -> Ok (Some r.act)

let get_by_auth_tx ~db ~auth_tx_id =
  ensure_schema db;
  match load_by_auth_tx ~db ~auth_tx_id with
  | Error e -> fail (Storage e) e
  | Ok None -> Ok None
  | Ok (Some r) -> Ok (Some r.act)

let get_plan ~db ~activation_id =
  ensure_schema db;
  match load_by_id ~db ~id:activation_id with
  | Error e -> fail (Storage e) e
  | Ok None -> Ok None
  | Ok (Some r) -> (
      match
        try redacted_plan_of_json (Yojson.Safe.from_string r.plan_json)
        with exn -> Error (Printexc.to_string exn)
      with
      | Error e -> fail (Storage e) e
      | Ok plan ->
          let plan =
            if plan.digest = "" then { plan with digest = r.act.plan_digest }
            else plan
          in
          Ok (Some plan))

let redacted_summary (a : activation) =
  String.concat "\n"
    [
      "GitHub user auth activation (redacted)";
      Printf.sprintf "  id: %s" a.id;
      Printf.sprintf "  status: %s" (string_of_activation_status a.status);
      Printf.sprintf "  principal: %s" a.principal_id;
      Printf.sprintf "  principal_revision: %d" a.principal_revision;
      Printf.sprintf "  auth_tx_id: %s" a.auth_tx_id;
      Printf.sprintf "  flow: %s" (Tx.string_of_flow_kind a.flow_kind);
      Printf.sprintf "  vault_id: %s" a.vault_id;
      Printf.sprintf "  vault_generation: %d" a.vault_generation;
      Printf.sprintf "  binding_id: %s" a.binding_id;
      Printf.sprintf "  github_user_id: %Ld" a.github_user_id;
      Printf.sprintf "  login: %s" a.login;
      Printf.sprintf "  mode: %s" (string_of_activation_mode a.mode);
      Printf.sprintf "  plan_id: %s" a.plan_id;
      Printf.sprintf "  plan_digest: %s" a.plan_digest;
      "  (access_token, refresh_token, and confirmation_token are never \
       included)";
    ]

let redacted_prepared_summary (p : prepared) =
  let base = redacted_summary p.activation in
  base
  ^ Printf.sprintf "\n  binding_status: %s"
      (B.string_of_authorization_status p.binding.B.authorization_status)

(* -------------------------------------------------------------------------- *)
(* Destroy helpers                                                            *)
(* -------------------------------------------------------------------------- *)

let destroy_vault_best_effort ~db ~id =
  match V.destroy ~db ~id with Ok () | Error V.Not_found -> () | Error _ -> ()

let destroy_pending_binding_material ~db ~(act : activation) =
  match B.get ~db ~id:act.binding_id with
  | Error _ -> destroy_vault_best_effort ~db ~id:act.vault_id
  | Ok None -> destroy_vault_best_effort ~db ~id:act.vault_id
  | Ok (Some binding) -> (
      match binding.B.authorization_status with
      | B.Authorized -> ()
      | B.Pending | B.Disabled | B.Revoked | B.Unlinked ->
          let vault_matches =
            match binding.B.vault_ref with
            | Some vref ->
                String.equal (B.vault_ref_to_string vref) act.vault_id
            | None -> false
          in
          if vault_matches then destroy_vault_best_effort ~db ~id:act.vault_id;
          (match binding.B.authorization_status with
          | B.Pending -> ignore (B.delete ~db ~id:binding.B.id)
          | _ ->
              ignore
                (B.set_vault_ref ~db ~id:binding.B.id ~vault_ref:None
                   ~expected_revision:binding.B.revision ()));
          if not vault_matches then
            destroy_vault_best_effort ~db ~id:act.vault_id)

let apply_terminal ~db ~(row : row) ~status ~reason ~now =
  let updated_at = now_iso ~now () in
  if status <> Activated then destroy_pending_binding_material ~db ~act:row.act;
  match
    update_status ~db ~id:row.act.id ~status ~destroyed_at:updated_at
      ~terminal_reason:reason ~updated_at ()
  with
  | Error e -> Error e
  | Ok () ->
      Ok
        {
          row.act with
          status;
          destroyed_at = Some updated_at;
          terminal_reason = Some reason;
          updated_at;
        }

let mark_destroyed ~db ~row ~reason ~now =
  apply_terminal ~db ~row ~status:Destroyed ~reason ~now

let mark_rejected ~db ~row ~reason ~now =
  apply_terminal ~db ~row ~status:Rejected ~reason ~now

let mark_expired ~db ~row ~now =
  apply_terminal ~db ~row ~status:Expired ~reason:"activation expired" ~now

(* -------------------------------------------------------------------------- *)
(* Auth-tx / Principal eligibility                                            *)
(* -------------------------------------------------------------------------- *)

let auth_tx_eligible ~(tx : Tx.t) =
  match (tx.flow_kind, tx.status) with
  | Tx.Web_pkce, Tx.Completed -> Ok ()
  | Tx.Web_pkce, Tx.Open ->
      Error
        ( Incomplete_exchange,
          "web PKCE authorization must complete the one-shot exchange before \
           activation" )
  | Tx.Device, (Tx.Open | Tx.Completed) -> Ok ()
  | _, Tx.Cancelled ->
      Error (Cancelled, "authorization transaction is cancelled")
  | _, Tx.Expired -> Error (Expired, "authorization transaction is expired")
  | _, (Tx.Superseded | Tx.Rejected) ->
      Error
        ( Incomplete_exchange,
          Printf.sprintf
            "authorization transaction is terminal (%s); not eligible for \
             activation"
            (Tx.string_of_status tx.status) )

let resolve_principal_safe ~db ~principal_id ?expected_revision () =
  match P.principal_id_of_string principal_id with
  | Error e -> Error (Principal_changed e, e)
  | Ok pid -> (
      match
        Op.resolve_principal_lineage ~db ~principal_id:pid ?expected_revision ()
      with
      | Error e -> Error (Storage e, e)
      | Ok (Op.Current_active { revision }) -> Ok (pid, revision)
      | Ok (Op.Tombstone { merged_into }) ->
          Error
            ( Principal_changed "tombstone",
              Printf.sprintf "principal %s is merged_into %s" principal_id
                (P.principal_id_to_string merged_into) )
      | Ok (Op.Disabled { summary }) ->
          Error (Principal_changed summary, summary)
      | Ok (Op.Missing { summary }) -> Error (Principal_changed summary, summary)
      | Ok (Op.Stale_revision { expected; actual }) ->
          Error
            ( Principal_changed "revision_conflict",
              Printf.sprintf
                "principal revision conflict: expected %d actual %d" expected
                actual ))

let check_intended_account ~(tx : Tx.t) ~(user : github_user) =
  match tx.Tx.intended_account.Tx.github_user_id with
  | Some pin when not (Int64.equal pin user.id) ->
      Error
        ( Identity_mismatch "intended_user",
          Printf.sprintf
            "GitHub user id %Ld does not match intended account pin %Ld" user.id
            pin )
  | _ -> (
      match tx.Tx.intended_account.Tx.login_hint with
      | Some hint when String.trim hint <> "" ->
          let hint = String.lowercase_ascii (String.trim hint) in
          let login = String.lowercase_ascii (String.trim user.login) in
          if hint <> login then
            Error
              ( Identity_mismatch "login_hint",
                Printf.sprintf
                  "GitHub login %S does not match intended login hint"
                  user.login )
          else Ok ()
      | _ -> Ok ())

(* -------------------------------------------------------------------------- *)
(* Seal vault + binding                                                       *)
(* -------------------------------------------------------------------------- *)

let seal_vault_and_binding ~db ~keys ~(tx : Tx.t) ~tokens ~scopes ~expires_at
    ~(github_user : github_user) ~now ?vault_id ?binding_id ~mode
    ~existing_pending () =
  let app = tx.Tx.app in
  match P.principal_id_of_string tx.Tx.principal_id with
  | Error e -> Error (Printf.sprintf "principal_id invalid: %s" e)
  | Ok principal_id -> (
      match
        V.make_account_key ~principal_id:tx.Tx.principal_id
          ~github_user_id:github_user.id ~app_id:app.app_id ~host:app.host ()
      with
      | Error e -> Error e
      | Ok account -> (
          (match existing_pending with
          | Some (prev : B.binding) -> (
              match prev.B.vault_ref with
              | Some vref -> (
                  let vid = B.vault_ref_to_string vref in
                  match vault_id with
                  | Some want when String.equal want vid -> ()
                  | _ -> destroy_vault_best_effort ~db ~id:vid)
              | None -> ())
          | None -> ());
          let create_vault () =
            V.create ~db ~keys ?id:vault_id ~now ~account ~tokens ~scopes
              ~expires_at ()
            |> Result.map_error (fun d ->
                Printf.sprintf "vault seal failed: %s" (V.string_of_denial d))
          in
          let vault_res =
            match (mode, existing_pending) with
            | Supersede_pending, Some prev -> (
                match prev.B.vault_ref with
                | Some vref -> (
                    let vid = B.vault_ref_to_string vref in
                    let reuse =
                      match vault_id with
                      | None -> true
                      | Some id -> String.equal id vid
                    in
                    if not reuse then create_vault ()
                    else
                      match V.get_meta ~db ~id:vid with
                      | Ok (Some meta) when meta.V.active -> (
                          match
                            V.replace ~db ~keys ~now ~id:vid
                              ~expected_generation:meta.V.generation
                              ~expected:account ~tokens ~scopes ~expires_at ()
                          with
                          | Ok vault -> Ok vault
                          | Error d ->
                              Error
                                (Printf.sprintf "vault replace failed: %s"
                                   (V.string_of_denial d)))
                      | _ ->
                          destroy_vault_best_effort ~db ~id:vid;
                          create_vault ())
                | None -> create_vault ())
            | _ -> create_vault ()
          in
          match vault_res with
          | Error e -> Error e
          | Ok vault -> (
              match B.make_vault_ref vault.V.id with
              | Error e ->
                  destroy_vault_best_effort ~db ~id:vault.V.id;
                  Error e
              | Ok vault_ref -> (
                  match
                    B.make_account_identity ~host:app.host ~app_id:app.app_id
                      ~github_user_id:github_user.id ()
                  with
                  | Error e ->
                      destroy_vault_best_effort ~db ~id:vault.V.id;
                      Error e
                  | Ok identity -> (
                      let display =
                        {
                          B.login = Some github_user.login;
                          avatar_url = github_user.avatar_url;
                        }
                      in
                      match (mode, existing_pending) with
                      | Supersede_pending, Some prev -> (
                          match
                            B.update ~db ~now ~id:prev.B.id
                              ~expected_revision:prev.B.revision ~display
                              ~authorization_status:B.Pending
                              ~vault_ref:(Some vault_ref) ()
                          with
                          | Ok binding -> Ok (vault, binding)
                          | Error e ->
                              destroy_vault_best_effort ~db ~id:vault.V.id;
                              Error
                                (Printf.sprintf
                                   "binding update failed after vault seal \
                                    (vault destroyed): %s"
                                   e))
                      | _ -> (
                          let binding_id =
                            match binding_id with
                            | Some id when String.trim id <> "" ->
                                String.trim id
                            | _ ->
                                Printf.sprintf "ghbind_%s_%Ld"
                                  (String.trim tx.Tx.id) github_user.id
                          in
                          let binding =
                            B.make_binding ~id:binding_id ~principal_id
                              ~identity ~display ~authorization_status:B.Pending
                              ~vault_ref ()
                          in
                          match B.insert ~db ~now binding with
                          | Ok binding -> Ok (vault, binding)
                          | Error e ->
                              destroy_vault_best_effort ~db ~id:vault.V.id;
                              Error
                                (Printf.sprintf
                                   "binding insert failed after vault seal \
                                    (vault destroyed, no active binding): %s"
                                   e)))))))

(* -------------------------------------------------------------------------- *)
(* Finalize prepare after seal                                                *)
(* -------------------------------------------------------------------------- *)

let finalize_prepared ~db ~(tx : Tx.t) ~(github_user : github_user)
    ~(credential : pending_credential) ~(vault : V.vault_record)
    ~(binding : B.binding) ~mode ~principal_revision ~now ~ttl_seconds
    ?activation_id ?plan_id () =
  if has_active_binding ~binding then (
    destroy_vault_best_effort ~db ~id:vault.V.id;
    ignore (B.delete ~db ~id:binding.B.id);
    fail (Partial "refused Authorized from prepare")
      "refusing Authorized binding from prepare; activation requires private \
       confirmation")
  else
    let created_at = now_iso ~now () in
    let expires_at = Time_util.iso8601_utc ~t:(now +. ttl_seconds) () in
    let act_id =
      match activation_id with
      | Some id when String.trim id <> "" -> String.trim id
      | _ -> generate_id ~now ()
    in
    let plan_id =
      match plan_id with
      | Some id when String.trim id <> "" -> String.trim id
      | _ -> "gh_act_plan_" ^ String.sub (generate_hex 8) 0 16
    in
    let plan_base : redacted_plan =
      {
        plan_id;
        digest = "";
        principal_id = tx.Tx.principal_id;
        principal_revision;
        base_revision = tx.Tx.base_revision;
        flow_kind = tx.Tx.flow_kind;
        auth_tx_id = tx.Tx.id;
        host = tx.Tx.app.Tx.host;
        app_id = tx.Tx.app.Tx.app_id;
        github_user_id = github_user.id;
        login = github_user.login;
        avatar_url = github_user.avatar_url;
        scopes = credential.scopes;
        vault_id = vault.V.id;
        vault_generation = vault.V.generation;
        binding_id = binding.B.id;
        binding_revision = binding.B.revision;
        mode;
        created_at;
        expires_at;
      }
    in
    let digest = compute_plan_digest plan_base in
    let plan = { plan_base with digest } in
    let confirmation_token = generate_confirmation_token () in
    let confirmation_token_hash = hash_confirmation_token confirmation_token in
    let act : activation =
      {
        version = schema_version;
        id = act_id;
        status = Pending_confirmation;
        principal_id = tx.Tx.principal_id;
        principal_revision;
        flow_kind = tx.Tx.flow_kind;
        auth_tx_id = tx.Tx.id;
        base_revision = tx.Tx.base_revision;
        host = tx.Tx.app.Tx.host;
        app_id = tx.Tx.app.Tx.app_id;
        github_user_id = github_user.id;
        login = github_user.login;
        avatar_url = github_user.avatar_url;
        scopes = credential.scopes;
        vault_id = vault.V.id;
        vault_generation = vault.V.generation;
        binding_id = binding.B.id;
        binding_revision = binding.B.revision;
        mode;
        plan_id;
        plan_digest = digest;
        created_at;
        expires_at;
        activated_at = None;
        destroyed_at = None;
        terminal_reason = None;
        updated_at = created_at;
      }
    in
    let row =
      {
        act;
        confirmation_token_hash;
        plan_json = Yojson.Safe.to_string (redacted_plan_to_json plan);
      }
    in
    match insert_row ~db row with
    | Error e ->
        destroy_pending_binding_material ~db ~act;
        fail (Partial e)
          (Printf.sprintf
             "activation insert failed (pending material destroyed): %s" e)
    | Ok () ->
        Ok
          {
            activation = act;
            plan;
            confirmation_token;
            vault;
            binding;
            github_user;
          }

(* -------------------------------------------------------------------------- *)
(* Prepare                                                                    *)
(* -------------------------------------------------------------------------- *)

let prepare ~db ~keys ?fetch_user ~auth_tx_id ~credential
    ?(now = Unix.gettimeofday ()) ?(ttl_seconds = default_ttl_seconds)
    ?activation_id ?vault_id ?binding_id ?plan_id () =
  ensure_schema db;
  let auth_tx_id = String.trim auth_tx_id in
  if auth_tx_id = "" then
    fail (Invalid "auth_tx_id") "auth_tx_id must be non-empty"
  else
    match
      make_pending_credential ~access_token:credential.access_token
        ?refresh_token:credential.refresh_token ~scopes:credential.scopes
        ~expires_in:credential.expires_in ?token_type:credential.token_type ()
    with
    | Error e -> fail (Invalid_credential e) e
    | Ok credential -> (
        match Tx.get ~db ~id:auth_tx_id with
        | Error e -> fail (Storage e) e
        | Ok None ->
            fail Incomplete_exchange
              (Printf.sprintf "authorization transaction not found: %s"
                 auth_tx_id)
        | Ok (Some tx) -> (
            if Tx.is_expired ~now tx then
              fail Expired
                "authorization transaction expired; refusing activation"
            else
              match auth_tx_eligible ~tx with
              | Error (kind, msg) -> fail kind msg
              | Ok () -> (
                  match load_by_auth_tx ~db ~auth_tx_id with
                  | Error e -> fail (Storage e) e
                  | Ok (Some existing) ->
                      fail ~activation:(Some existing.act) Replay
                        (Printf.sprintf
                           "activation already exists for auth_tx %s \
                            (status=%s)"
                           auth_tx_id
                           (string_of_activation_status existing.act.status))
                  | Ok None -> (
                      let fetch =
                        match fetch_user with
                        | Some f -> f
                        | None ->
                            fun ~access_token:_ ->
                              Error
                                "fetch_user not provided: inject GitHub /user \
                                 probe to obtain numeric user id before \
                                 sealing"
                      in
                      match fetch ~access_token:credential.access_token with
                      | Error e ->
                          fail (User_probe e)
                            (Printf.sprintf "GitHub /user probe failed: %s" e)
                      | Ok (user : github_user) -> (
                          if user.id <= 0L then
                            fail (Identity_mismatch "user_id")
                              "GitHub user id must be positive"
                          else if String.trim user.login = "" then
                            fail (Identity_mismatch "login")
                              "GitHub login must be non-empty"
                          else
                            match check_intended_account ~tx ~user with
                            | Error (kind, msg) -> fail kind msg
                            | Ok () -> (
                                match
                                  resolve_principal_safe ~db
                                    ~principal_id:tx.Tx.principal_id ()
                                with
                                | Error (kind, msg) -> fail kind msg
                                | Ok (principal_id, principal_revision) -> (
                                    match
                                      B.make_account_identity
                                        ~host:tx.Tx.app.Tx.host
                                        ~app_id:tx.Tx.app.Tx.app_id
                                        ~github_user_id:user.id ()
                                    with
                                    | Error e -> fail (Identity_mismatch e) e
                                    | Ok identity -> (
                                        match
                                          B.get_by_identity ~db ~identity
                                        with
                                        | Error e -> fail (Storage e) e
                                        | Ok (Some existing)
                                          when not
                                                 (P.principal_id_equal
                                                    existing.B.principal_id
                                                    principal_id) ->
                                            fail (Collision "other_principal")
                                              (Printf.sprintf
                                                 "GitHub account already bound \
                                                  to another Principal \
                                                  (binding %s); pending \
                                                  material not sealed; prior \
                                                  state preserved"
                                                 existing.B.id)
                                        | Ok (Some existing) -> (
                                            match
                                              existing.B.authorization_status
                                            with
                                            | B.Authorized ->
                                                fail
                                                  (Collision
                                                     "already_authorized")
                                                  (Printf.sprintf
                                                     "GitHub account already \
                                                      Authorized for this \
                                                      Principal (binding %s); \
                                                      prior state preserved; \
                                                      unlink/relink required \
                                                      for a new credential \
                                                      lineage"
                                                     existing.B.id)
                                            | B.Disabled | B.Revoked
                                            | B.Unlinked ->
                                                fail (Collision "not_pending")
                                                  (Printf.sprintf
                                                     "existing binding %s has \
                                                      status %s; activation \
                                                      refuses silent \
                                                      resurrection (prior \
                                                      state preserved)"
                                                     existing.B.id
                                                     (B
                                                      .string_of_authorization_status
                                                        existing
                                                          .B
                                                           .authorization_status))
                                            | B.Pending -> (
                                                let mode = Supersede_pending in
                                                let tokens : S.plaintext_tokens
                                                    =
                                                  {
                                                    access_token =
                                                      credential.access_token;
                                                    refresh_token =
                                                      credential.refresh_token;
                                                  }
                                                in
                                                let token_expires_at =
                                                  expires_at_iso ~now
                                                    ~expires_in:
                                                      credential.expires_in
                                                in
                                                match
                                                  seal_vault_and_binding ~db
                                                    ~keys ~tx ~tokens
                                                    ~scopes:credential.scopes
                                                    ~expires_at:token_expires_at
                                                    ~github_user:user ~now
                                                    ?vault_id ?binding_id ~mode
                                                    ~existing_pending:
                                                      (Some existing) ()
                                                with
                                                | Error e ->
                                                    fail (Partial e)
                                                      (e
                                                     ^ " (no active binding \
                                                        introduced)")
                                                | Ok (vault, binding) ->
                                                    finalize_prepared ~db ~tx
                                                      ~github_user:user
                                                      ~credential ~vault
                                                      ~binding ~mode
                                                      ~principal_revision ~now
                                                      ~ttl_seconds
                                                      ?activation_id ?plan_id ()
                                                ))
                                        | Ok None -> (
                                            let mode = New_binding in
                                            let tokens : S.plaintext_tokens =
                                              {
                                                access_token =
                                                  credential.access_token;
                                                refresh_token =
                                                  credential.refresh_token;
                                              }
                                            in
                                            let token_expires_at =
                                              expires_at_iso ~now
                                                ~expires_in:
                                                  credential.expires_in
                                            in
                                            match
                                              seal_vault_and_binding ~db ~keys
                                                ~tx ~tokens
                                                ~scopes:credential.scopes
                                                ~expires_at:token_expires_at
                                                ~github_user:user ~now ?vault_id
                                                ?binding_id ~mode
                                                ~existing_pending:None ()
                                            with
                                            | Error e ->
                                                fail (Partial e)
                                                  (e
                                                 ^ " (no active binding \
                                                    introduced)")
                                            | Ok (vault, binding) ->
                                                finalize_prepared ~db ~tx
                                                  ~github_user:user ~credential
                                                  ~vault ~binding ~mode
                                                  ~principal_revision ~now
                                                  ~ttl_seconds ?activation_id
                                                  ?plan_id ())))))))))

(* -------------------------------------------------------------------------- *)
(* Confirm                                                                    *)
(* -------------------------------------------------------------------------- *)

let reject_and_fail ~db ~row ~kind ~reason ~message ~now =
  match mark_rejected ~db ~row ~reason ~now with
  | Error e -> fail (Storage e) e
  | Ok rejected ->
      fail ~activation:(Some rejected) kind
        (message ^ "; pending material destroyed")

let confirm ~db ~keys ~activation_id ~confirmation_token ?expected_principal_id
    ?expected_plan_digest ?(now = Unix.gettimeofday ()) () =
  ensure_schema db;
  ignore keys;
  let activation_id = String.trim activation_id in
  let confirmation_token = String.trim confirmation_token in
  if activation_id = "" then
    fail (Invalid "activation_id") "activation_id must be non-empty"
  else if confirmation_token = "" then
    fail Confirmation_mismatch "confirmation_token must be non-empty"
  else
    match load_by_id ~db ~id:activation_id with
    | Error e -> fail (Storage e) e
    | Ok None ->
        fail Not_found (Printf.sprintf "activation not found: %s" activation_id)
    | Ok (Some row) -> (
        let act0 = row.act in
        match act0.status with
        | Activated ->
            fail ~activation:(Some act0) Already_activated
              "activation already completed"
        | Destroyed ->
            fail ~activation:(Some act0) Destroyed_status
              "activation was destroyed; pending material is gone"
        | Expired ->
            fail ~activation:(Some act0) Expired "activation is expired"
        | Cancelled ->
            fail ~activation:(Some act0) Cancelled "activation is cancelled"
        | Rejected ->
            fail ~activation:(Some act0) (Invalid "rejected")
              "activation was rejected"
        | Pending_confirmation -> (
            if is_expired_iso ~now act0.expires_at then
              match mark_expired ~db ~row ~now with
              | Error e -> fail (Storage e) e
              | Ok expired ->
                  fail ~activation:(Some expired) Expired
                    "activation expired before confirmation; pending material \
                     destroyed"
            else
              let presented_hash = hash_confirmation_token confirmation_token in
              if
                not
                  (constant_time_equal presented_hash
                     row.confirmation_token_hash)
              then
                reject_and_fail ~db ~row ~kind:Confirmation_mismatch
                  ~reason:"confirmation_mismatch"
                  ~message:"confirmation token mismatch" ~now
              else
                match expected_plan_digest with
                | Some d
                  when not
                         (constant_time_equal (String.trim d) act0.plan_digest)
                  ->
                    reject_and_fail ~db ~row ~kind:Plan_mismatch
                      ~reason:"plan_digest_mismatch"
                      ~message:"plan digest mismatch" ~now
                | _ -> (
                    match expected_principal_id with
                    | Some pid
                      when not
                             (String.equal (String.trim pid) act0.principal_id)
                      ->
                        reject_and_fail ~db ~row
                          ~kind:(Principal_changed "presented_principal")
                          ~reason:"principal_id_mismatch"
                          ~message:
                            "presented principal does not match activation"
                          ~now
                    | _ -> (
                        match
                          resolve_principal_safe ~db
                            ~principal_id:act0.principal_id
                            ~expected_revision:act0.principal_revision ()
                        with
                        | Error (kind, msg) ->
                            reject_and_fail ~db ~row ~kind
                              ~reason:("principal_changed:" ^ msg)
                              ~message:(msg ^ "; prior state preserved")
                              ~now
                        | Ok _ -> (
                            match Tx.get ~db ~id:act0.auth_tx_id with
                            | Error e -> fail (Storage e) e
                            | Ok None ->
                                reject_and_fail ~db ~row
                                  ~kind:Incomplete_exchange
                                  ~reason:"auth_tx_missing"
                                  ~message:
                                    "source authorization transaction missing"
                                  ~now
                            | Ok (Some tx) -> (
                                let refuse_tx kind reason message =
                                  reject_and_fail ~db ~row ~kind ~reason
                                    ~message ~now
                                in
                                match tx.Tx.status with
                                | Tx.Cancelled ->
                                    refuse_tx Cancelled "auth_tx_cancelled"
                                      "authorization cancelled before confirm"
                                | Tx.Expired ->
                                    refuse_tx Expired "auth_tx_expired"
                                      "authorization expired before confirm"
                                | Tx.Rejected | Tx.Superseded ->
                                    refuse_tx Incomplete_exchange
                                      ("auth_tx_"
                                      ^ Tx.string_of_status tx.Tx.status)
                                      (Printf.sprintf
                                         "authorization transaction is %s"
                                         (Tx.string_of_status tx.Tx.status))
                                | Tx.Open | Tx.Completed -> (
                                    if
                                      not
                                        (String.equal tx.Tx.principal_id
                                           act0.principal_id)
                                    then
                                      refuse_tx (Principal_changed "auth_tx")
                                        "auth_tx_principal_changed"
                                        "authorization Principal changed"
                                    else
                                      match begin_immediate ~db with
                                      | Error e -> fail (Storage e) e
                                      | Ok () -> (
                                          let fail_tx kind msg =
                                            rollback ~db;
                                            reject_and_fail ~db ~row ~kind
                                              ~reason:msg ~message:msg ~now
                                          in
                                          match
                                            B.get ~db ~id:act0.binding_id
                                          with
                                          | Error e -> fail_tx (Storage e) e
                                          | Ok None ->
                                              fail_tx Not_found
                                                "binding missing at confirm"
                                          | Ok (Some binding) -> (
                                              match
                                                P.principal_id_of_string
                                                  act0.principal_id
                                              with
                                              | Error e ->
                                                  fail_tx (Principal_changed e)
                                                    e
                                              | Ok expected_pid -> (
                                                  if
                                                    not
                                                      (P.principal_id_equal
                                                         binding.B.principal_id
                                                         expected_pid)
                                                  then
                                                    fail_tx
                                                      (Principal_changed
                                                         "binding")
                                                      "binding Principal \
                                                       changed before confirm"
                                                  else
                                                    match
                                                      binding
                                                        .B.authorization_status
                                                    with
                                                    | B.Authorized ->
                                                        fail_tx
                                                          (Collision
                                                             "already_authorized")
                                                          "binding already \
                                                           Authorized; \
                                                           refusing \
                                                           double-activate"
                                                    | B.Pending -> (
                                                        match
                                                          B
                                                          .update_authorization_status
                                                            ~db ~now
                                                            ~id:binding.B.id
                                                            ~expected_revision:
                                                              binding.B.revision
                                                            ~status:B.Authorized
                                                            ()
                                                        with
                                                        | Error e ->
                                                            fail_tx (Storage e)
                                                              e
                                                        | Ok binding' -> (
                                                            let updated_at =
                                                              now_iso ~now ()
                                                            in
                                                            match
                                                              update_status ~db
                                                                ~id:act0.id
                                                                ~status:
                                                                  Activated
                                                                ~activated_at:
                                                                  updated_at
                                                                ~updated_at ()
                                                            with
                                                            | Error e ->
                                                                fail_tx
                                                                  (Storage e) e
                                                            | Ok () -> (
                                                                match
                                                                  commit ~db
                                                                with
                                                                | Error e ->
                                                                    rollback ~db;
                                                                    fail
                                                                      (Storage e)
                                                                      e
                                                                | Ok () -> (
                                                                    match
                                                                      V.get_meta
                                                                        ~db
                                                                        ~id:
                                                                          act0
                                                                            .vault_id
                                                                    with
                                                                    | Error d ->
                                                                        fail
                                                                          (Storage
                                                                             (V
                                                                              .string_of_denial
                                                                                d))
                                                                          (V
                                                                           .string_of_denial
                                                                             d)
                                                                    | Ok None ->
                                                                        fail
                                                                          Not_found
                                                                          "vault \
                                                                           missing \
                                                                           after \
                                                                           activate"
                                                                    | Ok
                                                                        (Some
                                                                           vault)
                                                                      -> (
                                                                        match
                                                                          get_plan
                                                                            ~db
                                                                            ~activation_id
                                                                        with
                                                                        | Error
                                                                            e ->
                                                                            Error
                                                                              e
                                                                        | Ok
                                                                            None
                                                                          ->
                                                                            fail
                                                                              Not_found
                                                                              "plan \
                                                                               missing \
                                                                               after \
                                                                               activate"
                                                                        | Ok
                                                                            (Some
                                                                               plan)
                                                                          ->
                                                                            let act
                                                                                =
                                                                              {
                                                                                act0
                                                                                with
                                                                                status =
                                                                                Activated;
                                                                                activated_at =
                                                                                Some
                                                                                updated_at;
                                                                                updated_at;
                                                                              }
                                                                            in
                                                                            Ok
                                                                              {
                                                                                activation =
                                                                                act;
                                                                                plan;
                                                                                vault;
                                                                                binding =
                                                                                binding';
                                                                              })
                                                                    ))))
                                                    | other ->
                                                        fail_tx
                                                          (Collision
                                                             (B
                                                              .string_of_authorization_status
                                                                other))
                                                          (Printf.sprintf
                                                             "binding status \
                                                              %s is not \
                                                              Pending at \
                                                              confirm"
                                                             (B
                                                              .string_of_authorization_status
                                                                other)))))))))))
        )

(* -------------------------------------------------------------------------- *)
(* Destroy                                                                    *)
(* -------------------------------------------------------------------------- *)

let destroy ~db ~keys ~activation_id ?(reason = "destroyed")
    ?(now = Unix.gettimeofday ()) () =
  ensure_schema db;
  ignore keys;
  let activation_id = String.trim activation_id in
  if activation_id = "" then
    fail (Invalid "activation_id") "activation_id must be non-empty"
  else
    match load_by_id ~db ~id:activation_id with
    | Error e -> fail (Storage e) e
    | Ok None ->
        fail Not_found (Printf.sprintf "activation not found: %s" activation_id)
    | Ok (Some row) -> (
        match row.act.status with
        | Destroyed -> Ok row.act
        | Activated ->
            fail ~activation:(Some row.act) Already_activated
              "refusing destroy of activated binding; use unlink/revoke \
               lifecycle"
        | Pending_confirmation | Expired | Cancelled | Rejected -> (
            match mark_destroyed ~db ~row ~reason ~now with
            | Error e -> fail (Storage e) e
            | Ok act -> Ok act))
