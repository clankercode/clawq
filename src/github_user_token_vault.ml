(** Fail-closed mutable GitHub user-token vault CRUD (P21.M2.E4.T002).

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0006-use-principal-owned-github-user-tokens.md. *)

module S = Github_user_token_store
module MK = Github_user_token_master_key

let schema_version = 1
let default_host = "github.com"
let aes_key_len = 32
let envelope_prefix = "$VAULT_AAD_V1:"

(* -------------------------------------------------------------------------- *)
(* Types                                                                      *)
(* -------------------------------------------------------------------------- *)

type account_key = {
  principal_id : string;
  github_user_id : int64;
  app_id : int;
  host : string;
}

type vault_record = {
  id : string;
  account : account_key;
  record_version : int;
  key_id : MK.key_id;
  key_version : MK.key_version;
  generation : int;
  scopes : string list;
  expires_at : string;
  created_at : string;
  updated_at : string;
}

type opened = { record : vault_record; tokens : S.plaintext_tokens }

type denial =
  | Master_key_not_ready of MK.not_ready_reason list
  | Missing_key of { key_id : MK.key_id }
  | Wrong_key
  | Corrupt_envelope
  | Unsupported_version of { version : int }
  | Swapped_record
  | Account_mismatch of { expected : account_key; found : account_key }
  | Not_found
  | Already_exists
  | Generation_conflict of { expected : int; actual : int }
  | Crypto_failure
  | Invalid_input of string
  | Storage of string

type key_material = {
  key_id : MK.key_id;
  key_version : MK.key_version;
  aes_key : string;
}

type key_provider = {
  readiness : unit -> MK.readiness;
  resolve : key_id:MK.key_id -> (key_material, unit) result;
  active : unit -> (key_material, denial) result;
}

(* -------------------------------------------------------------------------- *)
(* Denial helpers                                                             *)
(* -------------------------------------------------------------------------- *)

let string_of_denial = function
  | Master_key_not_ready rs ->
      let rs = List.map MK.string_of_reason rs |> String.concat "," in
      Printf.sprintf "master_key_not_ready:%s"
        (if rs = "" then "unknown" else rs)
  | Missing_key { key_id } -> Printf.sprintf "missing_key:%s" key_id
  | Wrong_key -> "wrong_key"
  | Corrupt_envelope -> "corrupt_envelope"
  | Unsupported_version { version } ->
      Printf.sprintf "unsupported_version:%d" version
  | Swapped_record -> "swapped_record"
  | Account_mismatch { expected; found } ->
      Printf.sprintf "account_mismatch:expected=%s/%Ld/%d@%s found=%s/%Ld/%d@%s"
        expected.principal_id expected.github_user_id expected.app_id
        expected.host found.principal_id found.github_user_id found.app_id
        found.host
  | Not_found -> "not_found"
  | Already_exists -> "already_exists"
  | Generation_conflict { expected; actual } ->
      Printf.sprintf "generation_conflict:expected=%d actual=%d" expected actual
  | Crypto_failure -> "crypto_failure"
  | Invalid_input msg -> Printf.sprintf "invalid_input:%s" msg
  | Storage msg -> Printf.sprintf "storage:%s" msg

let denial_exposes_token ~denial ~plaintext =
  if plaintext = "" then false
  else String_util.contains (string_of_denial denial) plaintext

(* -------------------------------------------------------------------------- *)
(* Account key                                                                *)
(* -------------------------------------------------------------------------- *)

let make_account_key ~principal_id ~github_user_id ~app_id
    ?(host = default_host) () =
  let principal_id = String.trim principal_id in
  let host = String.trim host in
  if principal_id = "" then Error "principal_id must be non-empty"
  else if github_user_id <= 0L then Error "github_user_id must be positive"
  else if app_id <= 0 then Error "app_id must be positive"
  else if host = "" then Error "host must be non-empty"
  else Ok { principal_id; github_user_id; app_id; host }

let account_equal (a : account_key) (b : account_key) =
  String.equal a.principal_id b.principal_id
  && Int64.equal a.github_user_id b.github_user_id
  && a.app_id = b.app_id && String.equal a.host b.host

(* -------------------------------------------------------------------------- *)
(* Key provider + fingerprint (non-secret classification aid)                 *)
(* -------------------------------------------------------------------------- *)

let validate_aes_key aes_key =
  if String.length aes_key <> aes_key_len then
    Error
      (Printf.sprintf "aes_key must be exactly %d bytes (AES-256)" aes_key_len)
  else Ok ()

(** Truncated SHA-256 of the AES key. Not secret; distinguishes Wrong_key from
    Swapped_record when AEAD auth fails. *)
let key_fingerprint aes_key =
  let hex = Digestif.SHA256.(digest_string aes_key |> to_hex) in
  String.sub hex 0 16

let make_static_key_provider ~readiness ~keys () =
  let table : (string, key_material) Hashtbl.t = Hashtbl.create 8 in
  List.iter (fun (k : key_material) -> Hashtbl.replace table k.key_id k) keys;
  let resolve ~key_id =
    match Hashtbl.find_opt table key_id with Some m -> Ok m | None -> Error ()
  in
  let active () =
    match readiness with
    | MK.Ready { active = meta; _ } -> (
        match Hashtbl.find_opt table meta.key_id with
        | Some m -> Ok m
        | None -> Error (Missing_key { key_id = meta.key_id }))
    | MK.NotReady { reasons; _ } -> Error (Master_key_not_ready reasons)
  in
  { readiness = (fun () -> readiness); resolve; active }

let make_single_key_provider ~key_id ~key_version ~aes_key () =
  match validate_aes_key aes_key with
  | Error e -> Error e
  | Ok () ->
      let key_id = String.trim key_id in
      if key_id = "" then Error "key_id must be non-empty"
      else if key_version <= 0 then Error "key_version must be positive"
      else
        let material = { key_id; key_version; aes_key } in
        let meta : MK.key_metadata =
          {
            key_id;
            key_version;
            role = MK.Active;
            source_kind = MK.Env { var_name = MK.default_env_var };
          }
        in
        let readiness = MK.Ready { active = meta; available = [] } in
        Ok (make_static_key_provider ~readiness ~keys:[ material ] ())

(* -------------------------------------------------------------------------- *)
(* Envelope: AEAD-bound JSON payload                                          *)
(* -------------------------------------------------------------------------- *)

let aad_of ~account ~record_version ~generation ~key_id =
  Printf.sprintf
    "clawq-gh-user-vault-v1|principal=%s|gh_user=%Ld|app=%d|host=%s|ver=%d|gen=%d|key=%s"
    account.principal_id account.github_user_id account.app_id account.host
    record_version generation key_id

let payload_to_json ~account ~generation ~tokens ~scopes ~expires_at :
    Yojson.Safe.t =
  let fields =
    [
      ("v", `Int schema_version);
      ("principal_id", `String account.principal_id);
      ("github_user_id", `Intlit (Int64.to_string account.github_user_id));
      ("app_id", `Int account.app_id);
      ("host", `String account.host);
      ("generation", `Int generation);
      ("access_token", `String tokens.S.access_token);
      ("scopes", `List (List.map (fun s -> `String s) scopes));
      ("expires_at", `String expires_at);
    ]
  in
  let fields =
    match tokens.refresh_token with
    | None -> ("refresh_token", `Null) :: fields
    | Some r -> ("refresh_token", `String r) :: fields
  in
  `Assoc fields

let json_string_field name j =
  match Yojson.Safe.Util.member name j with
  | `String s -> Ok s
  | `Null -> Error ()
  | _ -> Error ()

let json_int_field name j =
  match Yojson.Safe.Util.member name j with
  | `Int i -> Ok i
  | `Intlit s -> ( try Ok (int_of_string s) with Failure _ -> Error ())
  | _ -> Error ()

let json_int64_field name j =
  match Yojson.Safe.Util.member name j with
  | `Int i -> Ok (Int64.of_int i)
  | `Intlit s -> ( try Ok (Int64.of_string s) with Failure _ -> Error ())
  | _ -> Error ()

let json_string_list_field name j =
  match Yojson.Safe.Util.member name j with
  | `List items ->
      let rec go acc = function
        | [] -> Ok (List.rev acc)
        | `String s :: rest -> go (s :: acc) rest
        | _ -> Error ()
      in
      go [] items
  | `Null -> Ok []
  | _ -> Error ()

type parsed_payload = {
  account : account_key;
  generation : int;
  tokens : S.plaintext_tokens;
  scopes : string list;
  expires_at : string;
}

let parse_payload_json (j : Yojson.Safe.t) : (parsed_payload, denial) result =
  match j with
  | `Assoc _ -> (
      match json_int_field "v" j with
      | Error () -> Error Corrupt_envelope
      | Ok payload_version when payload_version <> schema_version ->
          Error (Unsupported_version { version = payload_version })
      | Ok _payload_version -> (
          match
            ( json_string_field "principal_id" j,
              json_int64_field "github_user_id" j,
              json_int_field "app_id" j,
              json_string_field "host" j,
              json_int_field "generation" j,
              json_string_field "access_token" j,
              json_string_list_field "scopes" j,
              json_string_field "expires_at" j )
          with
          | ( Ok principal_id,
              Ok github_user_id,
              Ok app_id,
              Ok host,
              Ok generation,
              Ok access_token,
              Ok scopes,
              Ok expires_at ) ->
              let access_token = String.trim access_token in
              if access_token = "" then Error Corrupt_envelope
              else
                let refresh_token =
                  match Yojson.Safe.Util.member "refresh_token" j with
                  | `String s when String.trim s <> "" -> Some (String.trim s)
                  | _ -> None
                in
                Ok
                  {
                    account = { principal_id; github_user_id; app_id; host };
                    generation;
                    tokens = { S.access_token; refresh_token };
                    scopes;
                    expires_at;
                  }
          | _ -> Error Corrupt_envelope))
  | _ -> Error Corrupt_envelope

let seal_envelope ~aes_key ~account ~record_version ~generation ~key_id ~tokens
    ~scopes ~expires_at =
  match validate_aes_key aes_key with
  | Error _ -> Error Crypto_failure
  | Ok () ->
      let access = String.trim tokens.S.access_token in
      if access = "" then Error (Invalid_input "access_token must be non-empty")
      else
        let tokens = { tokens with access_token = access } in
        let aad = aad_of ~account ~record_version ~generation ~key_id in
        let plaintext =
          Yojson.Safe.to_string
            (payload_to_json ~account ~generation ~tokens ~scopes ~expires_at)
        in
        let handle = S.encrypt_with_aad ~key:aes_key ~aad ~plaintext in
        if not (S.is_aad_handle handle) then Error Crypto_failure
        else
          let body =
            String.sub handle
              (String.length "$ENC_AAD_V1:")
              (String.length handle - String.length "$ENC_AAD_V1:")
          in
          Ok (envelope_prefix ^ body)

let open_envelope ~aes_key ~account ~record_version ~generation ~key_id
    ~ciphertext ~key_fp_stored : (parsed_payload, denial) result =
  match validate_aes_key aes_key with
  | Error _ -> Error Crypto_failure
  | Ok () -> (
      let plen = String.length envelope_prefix in
      if String.length ciphertext <= plen then Error Corrupt_envelope
      else if String.sub ciphertext 0 plen <> envelope_prefix then
        Error Corrupt_envelope
      else
        let body =
          String.sub ciphertext plen (String.length ciphertext - plen)
        in
        if body = "" then Error Corrupt_envelope
        else
          match Base64.decode body with
          | Error _ -> Error Corrupt_envelope
          | Ok combined when String.length combined < 13 ->
              Error Corrupt_envelope
          | Ok _ -> (
              let store_handle = "$ENC_AAD_V1:" ^ body in
              let aad = aad_of ~account ~record_version ~generation ~key_id in
              match
                S.decrypt_with_aad ~key:aes_key ~aad ~handle:store_handle
              with
              | Error msg ->
                  if
                    String_util.contains msg "base64"
                    || String_util.contains msg "too short"
                    || String_util.contains msg "not an AAD"
                  then Error Corrupt_envelope
                  else if String_util.contains msg "AAD decryption failed" then
                    (* Well-formed envelope, auth fail: wrong material vs
                       binding/swap under the declared key. *)
                    if String.equal (key_fingerprint aes_key) key_fp_stored then
                      Error Swapped_record
                    else Error Wrong_key
                  else Error Crypto_failure
              | Ok plaintext -> (
                  match Yojson.Safe.from_string plaintext with
                  | exception _ -> Error Corrupt_envelope
                  | j -> (
                      match parse_payload_json j with
                      | Error e -> Error e
                      | Ok payload ->
                          if not (account_equal payload.account account) then
                            Error Swapped_record
                          else if payload.generation <> generation then
                            Error Swapped_record
                          else Ok payload))))

(* -------------------------------------------------------------------------- *)
(* SQLite helpers                                                             *)
(* -------------------------------------------------------------------------- *)

let exec_schema db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "github_user_token_vault schema error: %s (sql: %s)"
           (Sqlite3.Rc.to_string rc) sql)

let ensure_schema db =
  let table =
    {|CREATE TABLE IF NOT EXISTS github_user_token_vault (
      id TEXT PRIMARY KEY NOT NULL,
      principal_id TEXT NOT NULL,
      github_user_id INTEGER NOT NULL,
      app_id INTEGER NOT NULL,
      host TEXT NOT NULL,
      record_version INTEGER NOT NULL,
      key_id TEXT NOT NULL,
      key_version INTEGER NOT NULL,
      key_fingerprint TEXT NOT NULL,
      generation INTEGER NOT NULL,
      scopes_json TEXT NOT NULL,
      expires_at TEXT NOT NULL,
      ciphertext TEXT NOT NULL,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    )|}
  in
  let uniq_account =
    {|CREATE UNIQUE INDEX IF NOT EXISTS idx_gh_user_token_vault_account
      ON github_user_token_vault(principal_id, github_user_id, app_id, host)|}
  in
  let idx_key =
    {|CREATE INDEX IF NOT EXISTS idx_gh_user_token_vault_key_id
      ON github_user_token_vault(key_id)|}
  in
  List.iter (exec_schema db) [ table; uniq_account; idx_key ]

let scopes_to_json scopes =
  Yojson.Safe.to_string (`List (List.map (fun s -> `String s) scopes))

let scopes_of_json s =
  match Yojson.Safe.from_string s with
  | exception _ -> Error Corrupt_envelope
  | `List items ->
      let rec go acc = function
        | [] -> Ok (List.rev acc)
        | `String x :: rest -> go (x :: acc) rest
        | _ -> Error Corrupt_envelope
      in
      go [] items
  | _ -> Error Corrupt_envelope

let generate_id ?(now = Unix.gettimeofday ()) () =
  let ts = int_of_float now in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "ghvault_%d_%06d" ts rand

let text_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.TEXT s -> s
  | Sqlite3.Data.NULL -> ""
  | Sqlite3.Data.INT n -> Int64.to_string n
  | _ -> ""

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

type row = {
  meta : vault_record;
  ciphertext : string;
  key_fingerprint : string;
}

let row_of_stmt stmt : (row, denial) result =
  let id = text_col stmt 0 in
  let principal_id = text_col stmt 1 in
  let github_user_id = int64_col stmt 2 in
  let app_id = int_col stmt 3 in
  let host = text_col stmt 4 in
  let record_version = int_col stmt 5 in
  let key_id = text_col stmt 6 in
  let key_version = int_col stmt 7 in
  let key_fingerprint = text_col stmt 8 in
  let generation = int_col stmt 9 in
  let scopes_json = text_col stmt 10 in
  let expires_at = text_col stmt 11 in
  let ciphertext = text_col stmt 12 in
  let created_at = text_col stmt 13 in
  let updated_at = text_col stmt 14 in
  match scopes_of_json scopes_json with
  | Error e -> Error e
  | Ok scopes ->
      Ok
        {
          meta =
            {
              id;
              account = { principal_id; github_user_id; app_id; host };
              record_version;
              key_id;
              key_version;
              generation;
              scopes;
              expires_at;
              created_at;
              updated_at;
            };
          ciphertext;
          key_fingerprint;
        }

let select_sql =
  {|SELECT id, principal_id, github_user_id, app_id, host, record_version,
           key_id, key_version, key_fingerprint, generation, scopes_json,
           expires_at, ciphertext, created_at, updated_at
    FROM github_user_token_vault |}

let load_by_id db id : (row option, denial) result =
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
            (Storage
               (Printf.sprintf "SELECT by id failed: %s (%s)"
                  (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))))

let load_by_account db (account : account_key) : (row option, denial) result =
  let sql =
    select_sql
    ^ "WHERE principal_id = ? AND github_user_id = ? AND app_id = ? AND host = \
       ? LIMIT 1"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT account.principal_id));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT account.github_user_id));
      ignore
        (Sqlite3.bind stmt 3 (Sqlite3.Data.INT (Int64.of_int account.app_id)));
      ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT account.host));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match row_of_stmt stmt with Ok r -> Ok (Some r) | Error e -> Error e)
      | Sqlite3.Rc.DONE -> Ok None
      | rc ->
          Error
            (Storage
               (Printf.sprintf "SELECT by account failed: %s (%s)"
                  (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))))

let open_row ~keys (row : row) : (opened, denial) result =
  let meta = row.meta in
  if meta.record_version <= 0 || meta.record_version > schema_version then
    Error (Unsupported_version { version = meta.record_version })
  else
    match keys.resolve ~key_id:meta.key_id with
    | Error () -> Error (Missing_key { key_id = meta.key_id })
    | Ok material -> (
        match
          open_envelope ~aes_key:material.aes_key ~account:meta.account
            ~record_version:meta.record_version ~generation:meta.generation
            ~key_id:meta.key_id ~ciphertext:row.ciphertext
            ~key_fp_stored:row.key_fingerprint
        with
        | Error e -> Error e
        | Ok payload ->
            Ok
              {
                record =
                  {
                    meta with
                    scopes = payload.scopes;
                    expires_at = payload.expires_at;
                  };
                tokens = payload.tokens;
              })

(* -------------------------------------------------------------------------- *)
(* CRUD                                                                       *)
(* -------------------------------------------------------------------------- *)

let create ~db ~keys ?id ?(now = Unix.gettimeofday ()) ~account ~tokens ~scopes
    ~expires_at () =
  let expires_at = String.trim expires_at in
  if expires_at = "" then Error (Invalid_input "expires_at must be non-empty")
  else
    match keys.active () with
    | Error e -> Error e
    | Ok material -> (
        match validate_aes_key material.aes_key with
        | Error _ -> Error Crypto_failure
        | Ok () -> (
            let id =
              match id with
              | Some s when String.trim s <> "" -> String.trim s
              | _ -> generate_id ~now ()
            in
            let generation = 1 in
            let record_version = schema_version in
            let fp = key_fingerprint material.aes_key in
            match
              seal_envelope ~aes_key:material.aes_key ~account ~record_version
                ~generation ~key_id:material.key_id ~tokens ~scopes ~expires_at
            with
            | Error e -> Error e
            | Ok ciphertext ->
                let created_at = Time_util.iso8601_utc ~t:now () in
                let updated_at = created_at in
                let sql =
                  {|INSERT INTO github_user_token_vault
                    (id, principal_id, github_user_id, app_id, host, record_version,
                     key_id, key_version, key_fingerprint, generation, scopes_json,
                     expires_at, ciphertext, created_at, updated_at)
                    VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)|}
                in
                let stmt = Sqlite3.prepare db sql in
                Fun.protect
                  ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
                  (fun () ->
                    let bind i d = ignore (Sqlite3.bind stmt i d) in
                    bind 1 (Sqlite3.Data.TEXT id);
                    bind 2 (Sqlite3.Data.TEXT account.principal_id);
                    bind 3 (Sqlite3.Data.INT account.github_user_id);
                    bind 4 (Sqlite3.Data.INT (Int64.of_int account.app_id));
                    bind 5 (Sqlite3.Data.TEXT account.host);
                    bind 6 (Sqlite3.Data.INT (Int64.of_int record_version));
                    bind 7 (Sqlite3.Data.TEXT material.key_id);
                    bind 8
                      (Sqlite3.Data.INT (Int64.of_int material.key_version));
                    bind 9 (Sqlite3.Data.TEXT fp);
                    bind 10 (Sqlite3.Data.INT (Int64.of_int generation));
                    bind 11 (Sqlite3.Data.TEXT (scopes_to_json scopes));
                    bind 12 (Sqlite3.Data.TEXT expires_at);
                    bind 13 (Sqlite3.Data.TEXT ciphertext);
                    bind 14 (Sqlite3.Data.TEXT created_at);
                    bind 15 (Sqlite3.Data.TEXT updated_at);
                    match Sqlite3.step stmt with
                    | Sqlite3.Rc.DONE ->
                        Ok
                          {
                            id;
                            account;
                            record_version;
                            key_id = material.key_id;
                            key_version = material.key_version;
                            generation;
                            scopes;
                            expires_at;
                            created_at;
                            updated_at;
                          }
                    | rc ->
                        let msg = Sqlite3.errmsg db in
                        if
                          Sqlite3.Rc.to_string rc = "CONSTRAINT"
                          || String_util.contains
                               (String.lowercase_ascii msg)
                               "unique"
                        then Error Already_exists
                        else
                          Error
                            (Storage
                               (Printf.sprintf "INSERT failed: %s (%s)"
                                  (Sqlite3.Rc.to_string rc) msg)))))

let read ~db ~keys ?expected ~id () =
  match load_by_id db id with
  | Error e -> Error e
  | Ok None -> Error Not_found
  | Ok (Some row) -> (
      match expected with
      | Some exp when not (account_equal exp row.meta.account) ->
          Error (Account_mismatch { expected = exp; found = row.meta.account })
      | _ -> open_row ~keys row)

let read_by_account ~db ~keys ~account () =
  match load_by_account db account with
  | Error e -> Error e
  | Ok None -> Error Not_found
  | Ok (Some row) -> open_row ~keys row

let get_meta ~db ~id =
  match load_by_id db id with
  | Error e -> Error e
  | Ok None -> Ok None
  | Ok (Some row) -> Ok (Some row.meta)

let get_meta_by_account ~db ~account =
  match load_by_account db account with
  | Error e -> Error e
  | Ok None -> Ok None
  | Ok (Some row) -> Ok (Some row.meta)

let replace ~db ~keys ?(now = Unix.gettimeofday ()) ~id ~expected_generation
    ~tokens ~scopes ~expires_at () =
  let expires_at = String.trim expires_at in
  if expires_at = "" then Error (Invalid_input "expires_at must be non-empty")
  else if expected_generation < 1 then
    Error (Invalid_input "expected_generation must be positive")
  else
    match keys.active () with
    | Error e -> Error e
    | Ok material -> (
        match validate_aes_key material.aes_key with
        | Error _ -> Error Crypto_failure
        | Ok () -> (
            match load_by_id db id with
            | Error e -> Error e
            | Ok None -> Error Not_found
            | Ok (Some row) -> (
                let meta = row.meta in
                if meta.generation <> expected_generation then
                  Error
                    (Generation_conflict
                       {
                         expected = expected_generation;
                         actual = meta.generation;
                       })
                else
                  let new_generation = expected_generation + 1 in
                  let record_version = schema_version in
                  let fp = key_fingerprint material.aes_key in
                  match
                    seal_envelope ~aes_key:material.aes_key
                      ~account:meta.account ~record_version
                      ~generation:new_generation ~key_id:material.key_id ~tokens
                      ~scopes ~expires_at
                  with
                  | Error e -> Error e
                  | Ok ciphertext ->
                      let updated_at = Time_util.iso8601_utc ~t:now () in
                      let sql =
                        {|UPDATE github_user_token_vault
                          SET record_version = ?, key_id = ?, key_version = ?,
                              key_fingerprint = ?, generation = ?, scopes_json = ?,
                              expires_at = ?, ciphertext = ?, updated_at = ?
                          WHERE id = ? AND generation = ?|}
                      in
                      let stmt = Sqlite3.prepare db sql in
                      Fun.protect
                        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
                        (fun () ->
                          let bind i d = ignore (Sqlite3.bind stmt i d) in
                          bind 1
                            (Sqlite3.Data.INT (Int64.of_int record_version));
                          bind 2 (Sqlite3.Data.TEXT material.key_id);
                          bind 3
                            (Sqlite3.Data.INT
                               (Int64.of_int material.key_version));
                          bind 4 (Sqlite3.Data.TEXT fp);
                          bind 5
                            (Sqlite3.Data.INT (Int64.of_int new_generation));
                          bind 6 (Sqlite3.Data.TEXT (scopes_to_json scopes));
                          bind 7 (Sqlite3.Data.TEXT expires_at);
                          bind 8 (Sqlite3.Data.TEXT ciphertext);
                          bind 9 (Sqlite3.Data.TEXT updated_at);
                          bind 10 (Sqlite3.Data.TEXT id);
                          bind 11
                            (Sqlite3.Data.INT (Int64.of_int expected_generation));
                          match Sqlite3.step stmt with
                          | Sqlite3.Rc.DONE ->
                              if Sqlite3.changes db <> 1 then
                                Error
                                  (Generation_conflict
                                     {
                                       expected = expected_generation;
                                       actual = meta.generation;
                                     })
                              else
                                Ok
                                  {
                                    id;
                                    account = meta.account;
                                    record_version;
                                    key_id = material.key_id;
                                    key_version = material.key_version;
                                    generation = new_generation;
                                    scopes;
                                    expires_at;
                                    created_at = meta.created_at;
                                    updated_at;
                                  }
                          | rc ->
                              Error
                                (Storage
                                   (Printf.sprintf "UPDATE failed: %s (%s)"
                                      (Sqlite3.Rc.to_string rc)
                                      (Sqlite3.errmsg db)))))))

let destroy ~db ~id =
  let sql = "DELETE FROM github_user_token_vault WHERE id = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE ->
          if Sqlite3.changes db = 0 then Error Not_found else Ok ()
      | rc ->
          Error
            (Storage
               (Printf.sprintf "DELETE failed: %s (%s)"
                  (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))))

(* -------------------------------------------------------------------------- *)
(* Introspection                                                              *)
(* -------------------------------------------------------------------------- *)

let record_to_json (r : vault_record) : Yojson.Safe.t =
  `Assoc
    [
      ("id", `String r.id);
      ("principal_id", `String r.account.principal_id);
      ("github_user_id", `Intlit (Int64.to_string r.account.github_user_id));
      ("app_id", `Int r.account.app_id);
      ("host", `String r.account.host);
      ("record_version", `Int r.record_version);
      ("key_id", `String r.key_id);
      ("key_version", `Int r.key_version);
      ("generation", `Int r.generation);
      ("scopes", `List (List.map (fun s -> `String s) r.scopes));
      ("expires_at", `String r.expires_at);
      ("created_at", `String r.created_at);
      ("updated_at", `String r.updated_at);
    ]

let rec json_contains_plaintext ~json ~plaintext =
  if plaintext = "" then false
  else
    match json with
    | `String s -> String.equal s plaintext || String_util.contains s plaintext
    | `Assoc fields ->
        List.exists
          (fun (_k, v) -> json_contains_plaintext ~json:v ~plaintext)
          fields
    | `List items ->
        List.exists (fun v -> json_contains_plaintext ~json:v ~plaintext) items
    | _ -> false

let row_contains_plaintext ~db ~id ~plaintext =
  match load_by_id db id with
  | Error e -> Error e
  | Ok None -> Error Not_found
  | Ok (Some row) ->
      let meta = row.meta in
      let haystacks =
        [
          meta.id;
          meta.account.principal_id;
          meta.account.host;
          meta.key_id;
          meta.expires_at;
          meta.created_at;
          meta.updated_at;
          scopes_to_json meta.scopes;
          row.ciphertext;
          row.key_fingerprint;
        ]
      in
      Ok
        (List.exists
           (fun s -> plaintext <> "" && String_util.contains s plaintext)
           haystacks)
