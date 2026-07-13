(* Start GitHub App device authorization with private code delivery
   (P21.M2.E3.T001).
   See github_user_auth_device.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module Tx = Github_user_auth_tx
module D = Github_user_auth_delivery
module V = Github_user_token_vault
module S = Github_user_token_store
module MK = Github_user_token_master_key

let schema_version = 1
let default_host = "github.com"
let device_code_path = "/login/device/code"
let envelope_prefix = "$DEVICE_AAD_V1:"
let aes_key_len = 32

(* -------------------------------------------------------------------------- *)
(* Injectable boundaries                                                      *)
(* -------------------------------------------------------------------------- *)

type http_post =
  url:string ->
  headers:(string * string) list ->
  body:string ->
  (int * string, string) result

type resolve_client_id = handle:string -> (string, string) result

(* -------------------------------------------------------------------------- *)
(* Types                                                                      *)
(* -------------------------------------------------------------------------- *)

type device_code_response = {
  device_code : string;
  user_code : string;
  verification_uri : string;
  verification_uri_complete : string option;
  expires_in : int;
  interval : int;
}

type session = {
  version : int;
  id : string;
  tx_id : string;
  principal_id : string;
  app : Tx.app_client;
  intended_account : Tx.intended_account;
  key_id : MK.key_id;
  key_version : MK.key_version;
  interval_seconds : int;
  expires_at : string;
  next_poll_at : string;
  created_at : string;
  updated_at : string;
}

type opened_secrets = {
  device_code : string;
  user_code : string;
  verification_uri : string;
  verification_uri_complete : string option;
}

type refuse_reason =
  | Device_flow_disabled
  | Master_key_not_ready of MK.not_ready_reason list
  | No_private_channel
  | Invalid_input of string
  | Http of string
  | Storage of string
  | Crypto_failure
  | Delivery of D.refuse_reason

type refuse_error = {
  reason : refuse_reason;
  message : string;
  room_safe_progress : D.progress_content option;
}

type start_result = {
  session : session;
  tx : Tx.t;
  delivery_plan : D.delivery_plan;
}

(* -------------------------------------------------------------------------- *)
(* Refusal helpers                                                            *)
(* -------------------------------------------------------------------------- *)

let string_of_refuse_reason = function
  | Device_flow_disabled -> "device_flow_disabled"
  | Master_key_not_ready rs ->
      let rs = List.map MK.string_of_reason rs |> String.concat "," in
      Printf.sprintf "master_key_not_ready:%s"
        (if rs = "" then "unknown" else rs)
  | No_private_channel -> "no_private_channel"
  | Invalid_input msg -> "invalid_input:" ^ msg
  | Http msg -> "http:" ^ msg
  | Storage msg -> "storage:" ^ msg
  | Crypto_failure -> "crypto_failure"
  | Delivery r -> "delivery:" ^ D.string_of_refuse_reason r

let refuse reason message ?(room_safe_progress = None) () : refuse_error =
  { reason; message; room_safe_progress }

let room_progress_phase phase =
  match D.make_progress ~phase () with Ok p -> Some p | Error _ -> None

(* -------------------------------------------------------------------------- *)
(* URL                                                                        *)
(* -------------------------------------------------------------------------- *)

let normalize_host host =
  let h = String.lowercase_ascii (String.trim host) in
  let h =
    if String.length h >= 8 && String.sub h 0 8 = "https://" then
      String.sub h 8 (String.length h - 8)
    else if String.length h >= 7 && String.sub h 0 7 = "http://" then
      String.sub h 7 (String.length h - 7)
    else h
  in
  match String.split_on_char '/' h with
  | host_part :: _ -> (
      match String.split_on_char ':' host_part with
      | host_only :: _ -> host_only
      | [] -> "")
  | [] -> ""

let device_code_url ?(host = default_host) () =
  let host = normalize_host host in
  let host = if host = "" then default_host else host in
  Printf.sprintf "https://%s%s" host device_code_path

(* -------------------------------------------------------------------------- *)
(* Parse device-code response                                                 *)
(* -------------------------------------------------------------------------- *)

let form_assoc body =
  (* GitHub may return application/x-www-form-urlencoded. *)
  try
    Uri.query_of_encoded body
    |> List.filter_map (fun (k, vs) ->
        match vs with
        | v :: _ -> Some (String.lowercase_ascii (String.trim k), v)
        | [] -> None)
  with _ -> []

let json_string_opt name j =
  match Yojson.Safe.Util.member name j with
  | `String s ->
      let t = String.trim s in
      if t = "" then None else Some t
  | _ -> None

let json_int_opt name j =
  match Yojson.Safe.Util.member name j with
  | `Int i -> Some i
  | `Intlit s -> ( try Some (int_of_string s) with Failure _ -> None)
  | `String s -> ( try Some (int_of_string (String.trim s)) with _ -> None)
  | _ -> None

let assoc_find key pairs =
  List.find_map
    (fun (k, v) -> if String.equal k key then Some v else None)
    pairs

let assoc_int key pairs =
  match assoc_find key pairs with
  | None -> None
  | Some s -> ( try Some (int_of_string (String.trim s)) with _ -> None)

let build_response ~device_code ~user_code ~verification_uri
    ~verification_uri_complete ~expires_in ~interval =
  let device_code = String.trim device_code in
  let user_code = String.trim user_code in
  let verification_uri = String.trim verification_uri in
  if device_code = "" then Error "device_code missing from GitHub response"
  else if user_code = "" then Error "user_code missing from GitHub response"
  else if verification_uri = "" then
    Error "verification_uri missing from GitHub response"
  else if expires_in <= 0 then Error "expires_in must be positive"
  else if interval <= 0 then Error "interval must be positive"
  else
    let verification_uri_complete =
      match verification_uri_complete with
      | None -> None
      | Some s ->
          let t = String.trim s in
          if t = "" then None else Some t
    in
    Ok
      {
        device_code;
        user_code;
        verification_uri;
        verification_uri_complete;
        expires_in;
        interval;
      }

let parse_device_code_response ~body =
  let body = String.trim body in
  if body = "" then Error "empty device-code response"
  else
    (* Prefer JSON when the body looks like an object. *)
    let trimmed = String.trim body in
    if String.length trimmed > 0 && trimmed.[0] = '{' then
      try
        let j = Yojson.Safe.from_string trimmed in
        match
          ( json_string_opt "device_code" j,
            json_string_opt "user_code" j,
            json_string_opt "verification_uri" j )
        with
        | Some device_code, Some user_code, Some verification_uri ->
            let expires_in =
              match json_int_opt "expires_in" j with Some n -> n | None -> 900
            in
            let interval =
              match json_int_opt "interval" j with Some n -> n | None -> 5
            in
            build_response ~device_code ~user_code ~verification_uri
              ~verification_uri_complete:
                (json_string_opt "verification_uri_complete" j)
              ~expires_in ~interval
        | _ -> Error "device-code JSON missing required fields"
      with _ -> Error "device-code response is not valid JSON"
    else
      let pairs = form_assoc body in
      match
        ( assoc_find "device_code" pairs,
          assoc_find "user_code" pairs,
          assoc_find "verification_uri" pairs )
      with
      | Some device_code, Some user_code, Some verification_uri ->
          let expires_in =
            match assoc_int "expires_in" pairs with Some n -> n | None -> 900
          in
          let interval =
            match assoc_int "interval" pairs with Some n -> n | None -> 5
          in
          build_response ~device_code ~user_code ~verification_uri
            ~verification_uri_complete:
              (assoc_find "verification_uri_complete" pairs)
            ~expires_in ~interval
      | _ -> (
          (* Some GitHub error responses are form-encoded error=... *)
          match assoc_find "error" pairs with
          | Some err ->
              let desc =
                match assoc_find "error_description" pairs with
                | Some d -> d
                | None -> err
              in
              Error (Printf.sprintf "GitHub device-code error: %s" desc)
          | None -> Error "device-code response missing required fields")

(* -------------------------------------------------------------------------- *)
(* Sealed payload (device secrets)                                            *)
(* -------------------------------------------------------------------------- *)

let aad_of ~principal_id ~tx_id ~app_id ~host ~version =
  Printf.sprintf
    "clawq-gh-user-device-v1|principal=%s|tx=%s|app=%d|host=%s|ver=%d"
    (String.trim principal_id) (String.trim tx_id) app_id (String.trim host)
    version

let secrets_to_json (s : opened_secrets) : Yojson.Safe.t =
  let fields =
    [
      ("v", `Int schema_version);
      ("device_code", `String s.device_code);
      ("user_code", `String s.user_code);
      ("verification_uri", `String s.verification_uri);
    ]
  in
  let fields =
    match s.verification_uri_complete with
    | None -> ("verification_uri_complete", `Null) :: fields
    | Some u -> ("verification_uri_complete", `String u) :: fields
  in
  `Assoc fields

let secrets_of_json (j : Yojson.Safe.t) : (opened_secrets, string) result =
  match j with
  | `Assoc _ -> (
      let open Yojson.Safe.Util in
      try
        let v =
          match member "v" j with
          | `Int i -> i
          | `Intlit s -> int_of_string s
          | _ -> failwith "missing v"
        in
        if v <> schema_version then
          Error (Printf.sprintf "unsupported device payload version %d" v)
        else
          let device_code =
            member "device_code" j |> to_string |> String.trim
          in
          let user_code = member "user_code" j |> to_string |> String.trim in
          let verification_uri =
            member "verification_uri" j |> to_string |> String.trim
          in
          let verification_uri_complete =
            match member "verification_uri_complete" j with
            | `String s when String.trim s <> "" -> Some (String.trim s)
            | _ -> None
          in
          if device_code = "" || user_code = "" || verification_uri = "" then
            Error "device payload missing codes"
          else
            Ok
              {
                device_code;
                user_code;
                verification_uri;
                verification_uri_complete;
              }
      with
      | Failure msg -> Error msg
      | _ -> Error "invalid device payload json")
  | _ -> Error "device payload must be a JSON object"

let validate_aes_key aes_key =
  if String.length aes_key <> aes_key_len then
    Error
      (Printf.sprintf "aes_key must be exactly %d bytes (AES-256)" aes_key_len)
  else Ok ()

let seal_secrets ~aes_key ~aad ~(secrets : opened_secrets) =
  match validate_aes_key aes_key with
  | Error _ -> Error Crypto_failure
  | Ok () ->
      let plaintext = Yojson.Safe.to_string (secrets_to_json secrets) in
      let handle = S.encrypt_with_aad ~key:aes_key ~aad ~plaintext in
      if not (S.is_aad_handle handle) then Error Crypto_failure
      else
        let body =
          String.sub handle
            (String.length "$ENC_AAD_V1:")
            (String.length handle - String.length "$ENC_AAD_V1:")
        in
        Ok (envelope_prefix ^ body)

let open_sealed ~aes_key ~aad ~ciphertext =
  match validate_aes_key aes_key with
  | Error _ -> Error Crypto_failure
  | Ok () -> (
      let plen = String.length envelope_prefix in
      if String.length ciphertext <= plen then Error Crypto_failure
      else if String.sub ciphertext 0 plen <> envelope_prefix then
        Error Crypto_failure
      else
        let body =
          String.sub ciphertext plen (String.length ciphertext - plen)
        in
        let store_handle = "$ENC_AAD_V1:" ^ body in
        match S.decrypt_with_aad ~key:aes_key ~aad ~handle:store_handle with
        | Error _ -> Error Crypto_failure
        | Ok plaintext -> (
            match Yojson.Safe.from_string plaintext with
            | exception _ -> Error Crypto_failure
            | j -> (
                match secrets_of_json j with
                | Error _ -> Error Crypto_failure
                | Ok secrets -> Ok secrets)))

(* -------------------------------------------------------------------------- *)
(* Schema                                                                     *)
(* -------------------------------------------------------------------------- *)

let exec_schema db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "github_user_auth_device schema error: %s (sql: %s)"
           (Sqlite3.Rc.to_string rc) sql)

let ensure_schema db =
  Tx.ensure_schema db;
  let table =
    {|CREATE TABLE IF NOT EXISTS github_user_auth_device (
      id TEXT PRIMARY KEY NOT NULL,
      version INTEGER NOT NULL,
      tx_id TEXT NOT NULL UNIQUE,
      principal_id TEXT NOT NULL,
      host TEXT NOT NULL,
      app_id INTEGER NOT NULL,
      client_id_handle TEXT NOT NULL,
      intended_account_json TEXT NOT NULL,
      key_id TEXT NOT NULL,
      key_version INTEGER NOT NULL,
      ciphertext TEXT NOT NULL,
      interval_seconds INTEGER NOT NULL,
      expires_at TEXT NOT NULL,
      next_poll_at TEXT NOT NULL,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    )|}
  in
  let idx_principal =
    {|CREATE INDEX IF NOT EXISTS idx_github_user_auth_device_principal
      ON github_user_auth_device(principal_id)|}
  in
  let idx_expires =
    {|CREATE INDEX IF NOT EXISTS idx_github_user_auth_device_expires
      ON github_user_auth_device(expires_at)|}
  in
  List.iter (exec_schema db) [ table; idx_principal; idx_expires ]

(* -------------------------------------------------------------------------- *)
(* Row helpers                                                                *)
(* -------------------------------------------------------------------------- *)

let text_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.TEXT s -> s
  | Sqlite3.Data.NULL -> ""
  | _ -> ""

let int_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.INT n -> Int64.to_int n
  | Sqlite3.Data.TEXT s -> ( try int_of_string s with _ -> 0)
  | _ -> 0

let intended_account_to_json (a : Tx.intended_account) : Yojson.Safe.t =
  let fields = [] in
  let fields =
    match a.github_user_id with
    | None -> fields
    | Some id -> ("github_user_id", `String (Int64.to_string id)) :: fields
  in
  let fields =
    match a.login_hint with
    | None -> fields
    | Some h -> ("login_hint", `String h) :: fields
  in
  `Assoc (List.rev fields)

let intended_account_of_json (j : Yojson.Safe.t) :
    (Tx.intended_account, string) result =
  let open Yojson.Safe.Util in
  try
    let github_user_id =
      match member "github_user_id" j with
      | `Null -> None
      | `String s -> Some (Int64.of_string (String.trim s))
      | `Int i -> Some (Int64.of_int i)
      | `Intlit s -> Some (Int64.of_string s)
      | _ -> failwith "github_user_id must be string or int"
    in
    let login_hint =
      match member "login_hint" j with
      | `Null -> None
      | `String s ->
          let t = String.trim s in
          if t = "" then None else Some t
      | _ -> failwith "login_hint must be string"
    in
    Ok { github_user_id; login_hint }
  with
  | Failure msg -> Error msg
  | _ -> Error "invalid intended_account json"

let select_columns =
  {|id, version, tx_id, principal_id, host, app_id, client_id_handle,
    intended_account_json, key_id, key_version, interval_seconds,
    expires_at, next_poll_at, created_at, updated_at|}

let session_of_stmt stmt : (session, string) result =
  let id = text_col stmt 0 in
  let version = int_col stmt 1 in
  let tx_id = text_col stmt 2 in
  let principal_id = text_col stmt 3 in
  let host = text_col stmt 4 in
  let app_id = int_col stmt 5 in
  let client_id_handle = text_col stmt 6 in
  let intended_raw = text_col stmt 7 in
  let key_id = text_col stmt 8 in
  let key_version = int_col stmt 9 in
  let interval_seconds = int_col stmt 10 in
  let expires_at = text_col stmt 11 in
  let next_poll_at = text_col stmt 12 in
  let created_at = text_col stmt 13 in
  let updated_at = text_col stmt 14 in
  match Yojson.Safe.from_string intended_raw with
  | exception _ -> Error "intended_account_json is not valid JSON"
  | j -> (
      match intended_account_of_json j with
      | Error e -> Error e
      | Ok intended_account ->
          Ok
            {
              version;
              id;
              tx_id;
              principal_id;
              app = { host; app_id; client_id_handle };
              intended_account;
              key_id;
              key_version;
              interval_seconds;
              expires_at;
              next_poll_at;
              created_at;
              updated_at;
            })

let get_row_by ~db ~where ~bind_value =
  let sql =
    Printf.sprintf "SELECT %s FROM github_user_auth_device WHERE %s LIMIT 1"
      select_columns where
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT bind_value));
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match session_of_stmt stmt with
        | Ok s -> Ok (Some s)
        | Error e -> Error (refuse (Storage e) e ()))
    | Sqlite3.Rc.DONE -> Ok None
    | rc ->
        Error
          (refuse
             (Storage (Sqlite3.Rc.to_string rc))
             (Printf.sprintf "github_user_auth_device get failed: %s"
                (Sqlite3.Rc.to_string rc))
             ())
  in
  ignore (Sqlite3.finalize stmt);
  result

let get ~db ~id = get_row_by ~db ~where:"id = ?" ~bind_value:id
let get_by_tx ~db ~tx_id = get_row_by ~db ~where:"tx_id = ?" ~bind_value:tx_id

let ciphertext_of ~db ~id =
  let sql =
    "SELECT ciphertext FROM github_user_auth_device WHERE id = ? LIMIT 1"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> Ok (Some (text_col stmt 0))
    | Sqlite3.Rc.DONE -> Ok None
    | rc ->
        Error
          (refuse
             (Storage (Sqlite3.Rc.to_string rc))
             (Printf.sprintf "ciphertext load failed: %s"
                (Sqlite3.Rc.to_string rc))
             ())
  in
  ignore (Sqlite3.finalize stmt);
  result

let insert_session ~db (sess : session) ~ciphertext =
  let sql =
    {|INSERT INTO github_user_auth_device
      (id, version, tx_id, principal_id, host, app_id, client_id_handle,
       intended_account_json, key_id, key_version, ciphertext,
       interval_seconds, expires_at, next_poll_at, created_at, updated_at)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)|}
  in
  let stmt = Sqlite3.prepare db sql in
  let bind i v = ignore (Sqlite3.bind stmt i v) in
  bind 1 (Sqlite3.Data.TEXT sess.id);
  bind 2 (Sqlite3.Data.INT (Int64.of_int sess.version));
  bind 3 (Sqlite3.Data.TEXT sess.tx_id);
  bind 4 (Sqlite3.Data.TEXT sess.principal_id);
  bind 5 (Sqlite3.Data.TEXT sess.app.Tx.host);
  bind 6 (Sqlite3.Data.INT (Int64.of_int sess.app.Tx.app_id));
  bind 7 (Sqlite3.Data.TEXT sess.app.Tx.client_id_handle);
  bind 8
    (Sqlite3.Data.TEXT
       (Yojson.Safe.to_string (intended_account_to_json sess.intended_account)));
  bind 9 (Sqlite3.Data.TEXT sess.key_id);
  bind 10 (Sqlite3.Data.INT (Int64.of_int sess.key_version));
  bind 11 (Sqlite3.Data.TEXT ciphertext);
  bind 12 (Sqlite3.Data.INT (Int64.of_int sess.interval_seconds));
  bind 13 (Sqlite3.Data.TEXT sess.expires_at);
  bind 14 (Sqlite3.Data.TEXT sess.next_poll_at);
  bind 15 (Sqlite3.Data.TEXT sess.created_at);
  bind 16 (Sqlite3.Data.TEXT sess.updated_at);
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE -> Ok ()
  | rc ->
      Error
        (refuse
           (Storage (Sqlite3.Rc.to_string rc))
           (Printf.sprintf "github_user_auth_device insert failed: %s"
              (Sqlite3.Rc.to_string rc))
           ())

let generate_session_id ?(now = Unix.gettimeofday ()) () =
  let ts = int_of_float now in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "gh_user_auth_device_%d_%06d" ts rand

(* -------------------------------------------------------------------------- *)
(* Open secrets                                                               *)
(* -------------------------------------------------------------------------- *)

let open_secrets ~db ~keys ~id () =
  match get ~db ~id with
  | Error e -> Error e
  | Ok None ->
      Error
        (refuse (Storage "not_found")
           (Printf.sprintf "device session not found: %s" id)
           ())
  | Ok (Some sess) -> (
      match ciphertext_of ~db ~id with
      | Error e -> Error e
      | Ok None ->
          Error
            (refuse (Storage "missing_ciphertext")
               "device session ciphertext missing" ())
      | Ok (Some ciphertext) -> (
          match keys.V.resolve ~key_id:sess.key_id with
          | Error () ->
              Error
                (refuse (Master_key_not_ready [ MK.No_active ])
                   (Printf.sprintf "missing vault key for device session: %s"
                      sess.key_id)
                   ())
          | Ok material -> (
              let aad =
                aad_of ~principal_id:sess.principal_id ~tx_id:sess.tx_id
                  ~app_id:sess.app.Tx.app_id ~host:sess.app.Tx.host
                  ~version:sess.version
              in
              match
                open_sealed ~aes_key:material.V.aes_key ~aad ~ciphertext
              with
              | Error reason ->
                  Error
                    (refuse reason
                       "failed to open device session ciphertext (wrong key, \
                        corrupt envelope, or binding mismatch)"
                       ())
              | Ok secrets -> Ok (sess, secrets))))

(* -------------------------------------------------------------------------- *)
(* Request device code                                                        *)
(* -------------------------------------------------------------------------- *)

let request_device_code ~http_post ~host ~client_id =
  let url = device_code_url ~host () in
  let body = Uri.encoded_of_query [ ("client_id", [ client_id ]) ] in
  let headers =
    [
      ("Content-Type", "application/x-www-form-urlencoded");
      ("Accept", "application/json");
    ]
  in
  match http_post ~url ~headers ~body with
  | Error msg ->
      Error
        (refuse (Http msg)
           (Printf.sprintf "device-code request transport failed: %s" msg)
           ())
  | Ok (status, resp_body) -> (
      if status < 200 || status >= 300 then
        let snippet =
          let t = String.trim resp_body in
          if String.length t > 200 then String.sub t 0 200 ^ "..." else t
        in
        (* Never echo potential secrets; prefer structured error if present. *)
        let msg =
          match parse_device_code_response ~body:resp_body with
          | Error e when String_util.contains e "device-code error" -> e
          | _ ->
              Printf.sprintf "device-code request failed (HTTP %d)%s" status
                (if snippet = "" then "" else ": " ^ snippet)
        in
        Error (refuse (Http msg) msg ())
      else
        match parse_device_code_response ~body:resp_body with
        | Error e -> Error (refuse (Http e) e ())
        | Ok resp -> Ok resp)

(* -------------------------------------------------------------------------- *)
(* Start                                                                      *)
(* -------------------------------------------------------------------------- *)

let start ~db ?http_post ?resolve_client_id ~keys ~device_flow_enabled
    ~principal_id ~connector_actor ~source ~app
    ?(intended_account = Tx.empty_intended_account) ~base_revision
    ~continuation_handle ~channel ?shared_room_id ?(now = Unix.gettimeofday ())
    ?id ?tx_id () =
  ensure_schema db;
  let refuse_disabled () =
    Error
      (refuse Device_flow_disabled
         "GitHub device authorization is disabled for this setup. Enable \
          device flow on the GitHub App, or use web PKCE authorization \
          instead."
         ~room_safe_progress:(room_progress_phase "device_flow_disabled")
         ())
  in
  if not device_flow_enabled then refuse_disabled ()
  else
    match D.assert_private_channel channel with
    | Error e ->
        Error
          {
            reason =
              (match e.D.reason with
              | D.No_private_channel | D.Shared_room_blocked_private ->
                  No_private_channel
              | D.Invalid_channel _ -> No_private_channel
              | other -> Delivery other);
            message = e.D.message;
            room_safe_progress = e.D.room_safe_progress;
          }
    | Ok channel -> (
        match keys.V.active () with
        | Error (V.Master_key_not_ready rs) ->
            Error
              (refuse (Master_key_not_ready rs)
                 "Vault master key is not Ready; refuse device authorization \
                  start until an active write key is available."
                 ~room_safe_progress:(room_progress_phase "refused")
                 ())
        | Error d ->
            Error
              (refuse
                 (Invalid_input (V.string_of_denial d))
                 (Printf.sprintf "vault key provider refused: %s"
                    (V.string_of_denial d))
                 ())
        | Ok material -> (
            let host = normalize_host app.Tx.host in
            if host = "" || host <> default_host then
              Error
                (refuse (Invalid_input "host")
                   (Printf.sprintf
                      "V1 device authorization is GitHub.com-only; host %S \
                       refused"
                      app.Tx.host)
                   ())
            else if app.Tx.app_id <= 0 then
              Error
                (refuse (Invalid_input "app_id")
                   "app.Tx.app_id must be positive" ())
            else if String.trim app.Tx.client_id_handle = "" then
              Error
                (refuse (Invalid_input "client_id_handle")
                   "app.Tx.client_id_handle must be non-empty" ())
            else if String.trim principal_id = "" then
              Error
                (refuse (Invalid_input "principal_id")
                   "principal_id must be non-empty" ())
            else if String.trim base_revision = "" then
              Error
                (refuse (Invalid_input "base_revision")
                   "base_revision must be non-empty" ())
            else if String.trim continuation_handle = "" then
              Error
                (refuse (Invalid_input "continuation_handle")
                   "continuation_handle must be non-empty" ())
            else
              let resolve =
                match resolve_client_id with
                | Some f -> f
                | None ->
                    fun ~handle ->
                      Error
                        (Printf.sprintf
                           "resolve_client_id not provided for handle %s \
                            (inject a resolver or Secret_store wiring)"
                           handle)
              in
              match resolve ~handle:(String.trim app.Tx.client_id_handle) with
              | Error msg ->
                  Error
                    (refuse (Invalid_input msg)
                       (Printf.sprintf
                          "failed to resolve OAuth client id handle: %s" msg)
                       ())
              | Ok client_id -> (
                  let client_id = String.trim client_id in
                  if client_id = "" then
                    Error
                      (refuse (Invalid_input "empty client_id")
                         "resolved OAuth client id is empty" ())
                  else
                    let http =
                      match http_post with
                      | Some f -> f
                      | None ->
                          fun ~url:_ ~headers:_ ~body:_ ->
                            Error
                              "http_post not provided: inject a client or wire \
                               production HTTP"
                    in
                    match
                      request_device_code ~http_post:http ~host ~client_id
                    with
                    | Error e -> Error e
                    | Ok gh -> (
                        let session_id =
                          match id with
                          | Some i -> String.trim i
                          | None -> generate_session_id ~now ()
                        in
                        if session_id = "" then
                          Error
                            (refuse (Invalid_input "id")
                               "device session id must be non-empty" ())
                        else
                          let ttl = float_of_int gh.expires_in in
                          match
                            Tx.create ~db ~flow_kind:Tx.Device
                              ~principal_id:(String.trim principal_id)
                              ~connector_actor ~source
                              ~app:
                                {
                                  host;
                                  app_id = app.Tx.app_id;
                                  client_id_handle =
                                    String.trim app.Tx.client_id_handle;
                                }
                              ~intended_account
                              ~base_revision:(String.trim base_revision)
                              ~continuation_handle:
                                (String.trim continuation_handle)
                              ~ttl_seconds:ttl ~now ?id:tx_id ()
                          with
                          | Error e ->
                              Error
                                (refuse (Storage e)
                                   (Printf.sprintf
                                      "failed to create authorization \
                                       transaction: %s"
                                      e)
                                   ())
                          | Ok tx -> (
                              let secrets : opened_secrets =
                                {
                                  device_code = gh.device_code;
                                  user_code = gh.user_code;
                                  verification_uri = gh.verification_uri;
                                  verification_uri_complete =
                                    gh.verification_uri_complete;
                                }
                              in
                              let aad =
                                aad_of ~principal_id:tx.principal_id
                                  ~tx_id:tx.id ~app_id:tx.app.Tx.app_id
                                  ~host:tx.app.Tx.host ~version:schema_version
                              in
                              match
                                seal_secrets ~aes_key:material.V.aes_key ~aad
                                  ~secrets
                              with
                              | Error reason ->
                                  Error
                                    (refuse reason
                                       "failed to seal device_code under vault \
                                        master key"
                                       ())
                              | Ok ciphertext -> (
                                  let created_at =
                                    Time_util.iso8601_utc ~t:now ()
                                  in
                                  let expires_at =
                                    Time_util.iso8601_utc
                                      ~t:(now +. float_of_int gh.expires_in)
                                      ()
                                  in
                                  let next_poll_at =
                                    Time_util.iso8601_utc
                                      ~t:(now +. float_of_int gh.interval)
                                      ()
                                  in
                                  let sess : session =
                                    {
                                      version = schema_version;
                                      id = session_id;
                                      tx_id = tx.id;
                                      principal_id = tx.principal_id;
                                      app = tx.app;
                                      intended_account = tx.intended_account;
                                      key_id = material.V.key_id;
                                      key_version = material.V.key_version;
                                      interval_seconds = gh.interval;
                                      expires_at;
                                      next_poll_at;
                                      created_at;
                                      updated_at = created_at;
                                    }
                                  in
                                  match insert_session ~db sess ~ciphertext with
                                  | Error e -> Error e
                                  | Ok () -> (
                                      (* Private delivery: user_code +
                                         verification_uri only — never
                                         device_code on the wire to the user
                                         channel (poller reads sealed store). *)
                                      match
                                        D.make_device_codes
                                          ~user_code:gh.user_code
                                          ~verification_uri:gh.verification_uri
                                          ?verification_uri_complete:
                                            gh.verification_uri_complete ()
                                      with
                                      | Error e ->
                                          Error (refuse (Invalid_input e) e ())
                                      | Ok material_payload -> (
                                          let context = D.context_of_tx tx in
                                          match
                                            D.deliver ~context ~channel
                                              ~content:
                                                (D.Material material_payload)
                                              ?shared_room_id ()
                                          with
                                          | Error e ->
                                              Error
                                                {
                                                  reason =
                                                    (match e.D.reason with
                                                    | D.No_private_channel
                                                    | D
                                                      .Shared_room_blocked_private
                                                      ->
                                                        No_private_channel
                                                    | other -> Delivery other);
                                                  message = e.D.message;
                                                  room_safe_progress =
                                                    e.D.room_safe_progress;
                                                }
                                          | Ok plan ->
                                              Ok
                                                {
                                                  session = sess;
                                                  tx;
                                                  delivery_plan = plan;
                                                }))))))))

(* -------------------------------------------------------------------------- *)
(* Introspection (no secrets)                                                 *)
(* -------------------------------------------------------------------------- *)

let session_to_json (s : session) : Yojson.Safe.t =
  let intended =
    let fields = [] in
    let fields =
      match s.intended_account.github_user_id with
      | None -> fields
      | Some id -> ("github_user_id", `String (Int64.to_string id)) :: fields
    in
    let fields =
      match s.intended_account.login_hint with
      | None -> fields
      | Some h -> ("login_hint", `String h) :: fields
    in
    `Assoc (List.rev fields)
  in
  `Assoc
    [
      ("version", `Int s.version);
      ("id", `String s.id);
      ("tx_id", `String s.tx_id);
      ("principal_id", `String s.principal_id);
      ("host", `String s.app.Tx.host);
      ("app_id", `Int s.app.Tx.app_id);
      ("client_id_handle", `String s.app.Tx.client_id_handle);
      ("intended_account", intended);
      ("key_id", `String s.key_id);
      ("key_version", `Int s.key_version);
      ("interval_seconds", `Int s.interval_seconds);
      ("expires_at", `String s.expires_at);
      ("next_poll_at", `String s.next_poll_at);
      ("created_at", `String s.created_at);
      ("updated_at", `String s.updated_at);
    ]

let redacted_summary (s : session) =
  Printf.sprintf
    "device_session id=%s tx=%s principal=%s app=%d@%s interval=%ds \
     expires_at=%s next_poll_at=%s key_id=%s"
    s.id s.tx_id s.principal_id s.app.Tx.app_id s.app.Tx.host s.interval_seconds
    s.expires_at s.next_poll_at s.key_id

let start_result_redacted_summary (r : start_result) =
  Printf.sprintf "%s | delivery=%s"
    (redacted_summary r.session)
    (D.plan_redacted_summary r.delivery_plan)

let json_contains_plaintext ~json ~plaintext =
  if plaintext = "" then false
  else
    let rec walk = function
      | `String s -> String_util.contains s plaintext
      | `Assoc fields -> List.exists (fun (_, v) -> walk v) fields
      | `List xs -> List.exists walk xs
      | `Tuple xs -> List.exists walk xs
      | `Variant (_, Some v) -> walk v
      | `Variant (_, None) | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ ->
          false
    in
    walk json

let row_contains_plaintext ~db ~id ~plaintext =
  if plaintext = "" then Ok false
  else
    let sql =
      {|SELECT id, version, tx_id, principal_id, host, app_id, client_id_handle,
               intended_account_json, key_id, key_version, ciphertext,
               interval_seconds, expires_at, next_poll_at, created_at, updated_at
        FROM github_user_auth_device WHERE id = ? LIMIT 1|}
    in
    let stmt = Sqlite3.prepare db sql in
    ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
    let result =
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok false
      | Sqlite3.Rc.ROW ->
          let hit = ref false in
          for i = 0 to 15 do
            match Sqlite3.column stmt i with
            | Sqlite3.Data.TEXT s when String_util.contains s plaintext ->
                hit := true
            | _ -> ()
          done;
          Ok !hit
      | rc ->
          Error
            (refuse
               (Storage (Sqlite3.Rc.to_string rc))
               (Printf.sprintf "row_contains_plaintext failed: %s"
                  (Sqlite3.Rc.to_string rc))
               ())
    in
    ignore (Sqlite3.finalize stmt);
    result
