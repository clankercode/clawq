(** Versioned encrypted GitHub App user-token records (P21.M2.E4.T001).

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0006-use-principal-owned-github-user-tokens.md. *)

let schema_version = 1

(* -------------------------------------------------------------------------- *)
(* Secret backend                                                             *)
(* -------------------------------------------------------------------------- *)

type secret_backend = {
  put : name:string -> plaintext:string -> (string, string) result;
  get : handle:string -> (string, string) result;
  delete : handle:string -> (unit, string) result;
}

let make_in_memory_secret_store () =
  let table : (string, string) Hashtbl.t = Hashtbl.create 16 in
  let seq = ref 0 in
  let put ~name ~plaintext =
    if String.trim plaintext = "" then
      Error (Printf.sprintf "refusing to store empty secret: %s" name)
    else begin
      incr seq;
      let handle = Printf.sprintf "mem:%s:%d" name !seq in
      Hashtbl.replace table handle plaintext;
      Ok handle
    end
  in
  let get ~handle =
    match Hashtbl.find_opt table handle with
    | Some v -> Ok v
    | None -> Error (Printf.sprintf "secret handle not found: %s" handle)
  in
  let delete ~handle =
    Hashtbl.remove table handle;
    Ok ()
  in
  ({ put; get; delete }, table)

let secret_backend_of_secret_store_key ~key =
  let put ~name ~plaintext =
    if String.trim plaintext = "" then
      Error (Printf.sprintf "refusing to store empty secret: %s" name)
    else Ok (Secret_store.encrypt_secret ~key plaintext)
  in
  let get ~handle =
    match Secret_store.decrypt_secret ~key handle with
    | Ok v -> Ok v
    | Error msg -> Error msg
  in
  let delete ~handle:_ = Ok () in
  { put; get; delete }

(* -------------------------------------------------------------------------- *)
(* Record                                                                     *)
(* -------------------------------------------------------------------------- *)

type t = {
  version : int;
  principal_id : string;
  github_user_id : int64;
  access_token_handle : string;
  refresh_token_handle : string option;
  scopes : string list;
  expires_at : string;
  app_id : int;
}

type plaintext_tokens = { access_token : string; refresh_token : string option }

let make ?(version = schema_version) ~principal_id ~github_user_id
    ~access_token_handle ?refresh_token_handle ~scopes ~expires_at ~app_id () =
  let principal_id = String.trim principal_id in
  let access_token_handle = String.trim access_token_handle in
  let expires_at = String.trim expires_at in
  let refresh_token_handle =
    match refresh_token_handle with
    | None -> None
    | Some s ->
        let t = String.trim s in
        if t = "" then None else Some t
  in
  if principal_id = "" then Error "principal_id must be non-empty"
  else if access_token_handle = "" then
    Error "access_token_handle must be non-empty"
  else if expires_at = "" then Error "expires_at must be non-empty"
  else if app_id <= 0 then Error "app_id must be positive"
  else if version <= 0 then Error "version must be positive"
  else if github_user_id <= 0L then Error "github_user_id must be positive"
  else
    Ok
      {
        version;
        principal_id;
        github_user_id;
        access_token_handle;
        refresh_token_handle;
        scopes;
        expires_at;
        app_id;
      }

let seal ~store ~principal_id ~github_user_id ~tokens ~scopes ~expires_at
    ~app_id () =
  let access = String.trim tokens.access_token in
  if access = "" then Error "access_token must be non-empty"
  else
    match
      store.put
        ~name:
          (Printf.sprintf "gh_user_access:%s:%Ld" (String.trim principal_id)
             github_user_id)
        ~plaintext:access
    with
    | Error e -> Error e
    | Ok access_token_handle -> (
        match tokens.refresh_token with
        | None ->
            make ~principal_id ~github_user_id ~access_token_handle ~scopes
              ~expires_at ~app_id ()
        | Some refresh -> (
            let refresh = String.trim refresh in
            if refresh = "" then
              make ~principal_id ~github_user_id ~access_token_handle ~scopes
                ~expires_at ~app_id ()
            else
              match
                store.put
                  ~name:
                    (Printf.sprintf "gh_user_refresh:%s:%Ld"
                       (String.trim principal_id) github_user_id)
                  ~plaintext:refresh
              with
              | Error e -> Error e
              | Ok refresh_handle ->
                  make ~principal_id ~github_user_id ~access_token_handle
                    ~refresh_token_handle:refresh_handle ~scopes ~expires_at
                    ~app_id ()))

let resolve_tokens ~store (r : t) =
  match store.get ~handle:r.access_token_handle with
  | Error e -> Error e
  | Ok access_token -> (
      let access_token = String.trim access_token in
      if access_token = "" then Error "resolved access_token is empty"
      else
        match r.refresh_token_handle with
        | None -> Ok { access_token; refresh_token = None }
        | Some h -> (
            match store.get ~handle:h with
            | Error e -> Error e
            | Ok refresh ->
                let refresh = String.trim refresh in
                Ok
                  {
                    access_token;
                    refresh_token =
                      (if refresh = "" then None else Some refresh);
                  }))

let delete_tokens ~store (r : t) =
  match store.delete ~handle:r.access_token_handle with
  | Error e -> Error e
  | Ok () -> (
      match r.refresh_token_handle with
      | None -> Ok ()
      | Some h -> store.delete ~handle:h)

(* -------------------------------------------------------------------------- *)
(* Authenticated ciphertext with record-bound AAD                             *)
(* -------------------------------------------------------------------------- *)

let aad_prefix = "clawq-gh-user-token-v1"

let aad_of ~principal_id ~github_user_id ~app_id ~version =
  Printf.sprintf "%s|principal=%s|gh_user=%Ld|app=%d|ver=%d" aad_prefix
    (String.trim principal_id) github_user_id app_id version

let aad_handle_prefix = "$ENC_AAD_V1:"

let is_aad_handle value =
  let p = aad_handle_prefix in
  let plen = String.length p in
  String.length value > plen && String.sub value 0 plen = p

let encrypt_with_aad ~key ~aad ~plaintext =
  Mirage_crypto_rng_unix.use_default ();
  let nonce = Mirage_crypto_rng.generate 12 in
  let gcm_key = Mirage_crypto.AES.GCM.of_secret key in
  let ciphertext =
    Mirage_crypto.AES.GCM.authenticate_encrypt ~key:gcm_key ~nonce ~adata:aad
      plaintext
  in
  let combined = nonce ^ ciphertext in
  aad_handle_prefix ^ Base64.encode_exn combined

let decrypt_with_aad ~key ~aad ~handle =
  if not (is_aad_handle handle) then
    Error "not an AAD-bound encrypted token handle"
  else
    let encoded =
      String.sub handle
        (String.length aad_handle_prefix)
        (String.length handle - String.length aad_handle_prefix)
    in
    match Base64.decode encoded with
    | Error _ -> Error "Failed to decode base64 AAD ciphertext"
    | Ok combined -> (
        if String.length combined < 13 then Error "Encrypted AAD data too short"
        else
          let nonce = String.sub combined 0 12 in
          let ciphertext =
            String.sub combined 12 (String.length combined - 12)
          in
          let gcm_key = Mirage_crypto.AES.GCM.of_secret key in
          match
            Mirage_crypto.AES.GCM.authenticate_decrypt ~key:gcm_key ~nonce
              ~adata:aad ciphertext
          with
          | None ->
              Error
                "AAD decryption failed (wrong key, corrupted data, or record \
                 binding mismatch)"
          | Some plaintext -> Ok plaintext)

let seal_encrypted ~key ~principal_id ~github_user_id ~tokens ~scopes
    ~expires_at ~app_id () =
  let access = String.trim tokens.access_token in
  if access = "" then Error "access_token must be non-empty"
  else
    let aad =
      aad_of ~principal_id ~github_user_id ~app_id ~version:schema_version
    in
    let access_token_handle = encrypt_with_aad ~key ~aad ~plaintext:access in
    let refresh_token_handle =
      match tokens.refresh_token with
      | None -> None
      | Some r ->
          let r = String.trim r in
          if r = "" then None
          else Some (encrypt_with_aad ~key ~aad ~plaintext:r)
    in
    make ~principal_id ~github_user_id ~access_token_handle
      ?refresh_token_handle ~scopes ~expires_at ~app_id ()

let resolve_encrypted ~key (r : t) =
  let aad =
    aad_of ~principal_id:r.principal_id ~github_user_id:r.github_user_id
      ~app_id:r.app_id ~version:r.version
  in
  match decrypt_with_aad ~key ~aad ~handle:r.access_token_handle with
  | Error e -> Error e
  | Ok access_token -> (
      let access_token = String.trim access_token in
      if access_token = "" then Error "decrypted access_token is empty"
      else
        match r.refresh_token_handle with
        | None -> Ok { access_token; refresh_token = None }
        | Some h -> (
            match decrypt_with_aad ~key ~aad ~handle:h with
            | Error e -> Error e
            | Ok refresh ->
                let refresh = String.trim refresh in
                Ok
                  {
                    access_token;
                    refresh_token =
                      (if refresh = "" then None else Some refresh);
                  }))

(* -------------------------------------------------------------------------- *)
(* JSON — handles only, never plaintext tokens                                *)
(* -------------------------------------------------------------------------- *)

let to_json (r : t) : Yojson.Safe.t =
  let fields =
    [
      ("version", `Int r.version);
      ("principal_id", `String r.principal_id);
      ("github_user_id", `Intlit (Int64.to_string r.github_user_id));
      ("access_token_handle", `String r.access_token_handle);
      ("scopes", `List (List.map (fun s -> `String s) r.scopes));
      ("expires_at", `String r.expires_at);
      ("app_id", `Int r.app_id);
    ]
  in
  let fields =
    match r.refresh_token_handle with
    | None -> ("refresh_token_handle", `Null) :: fields
    | Some h -> ("refresh_token_handle", `String h) :: fields
  in
  `Assoc fields

let json_string_field name j =
  match Yojson.Safe.Util.member name j with
  | `String s -> Ok s
  | `Null -> Error (Printf.sprintf "missing string field %s" name)
  | _ -> Error (Printf.sprintf "field %s must be a string" name)

let json_int_field name j =
  match Yojson.Safe.Util.member name j with
  | `Int i -> Ok i
  | `Intlit s -> (
      try Ok (int_of_string s)
      with Failure _ -> Error (Printf.sprintf "field %s is not an int" name))
  | `Null -> Error (Printf.sprintf "missing int field %s" name)
  | _ -> Error (Printf.sprintf "field %s must be an integer" name)

let json_int64_field name j =
  match Yojson.Safe.Util.member name j with
  | `Int i -> Ok (Int64.of_int i)
  | `Intlit s -> (
      try Ok (Int64.of_string s)
      with Failure _ -> Error (Printf.sprintf "field %s is not an int64" name))
  | `Null -> Error (Printf.sprintf "missing int64 field %s" name)
  | _ -> Error (Printf.sprintf "field %s must be an integer" name)

let json_string_list_field name j =
  match Yojson.Safe.Util.member name j with
  | `List items ->
      let rec go acc = function
        | [] -> Ok (List.rev acc)
        | `String s :: rest -> go (s :: acc) rest
        | _ -> Error (Printf.sprintf "field %s must be a string list" name)
      in
      go [] items
  | `Null -> Ok []
  | _ -> Error (Printf.sprintf "field %s must be a list" name)

let of_json (j : Yojson.Safe.t) =
  match j with
  | `Assoc _ -> (
      match json_int_field "version" j with
      | Error e -> Error e
      | Ok version -> (
          match json_string_field "principal_id" j with
          | Error e -> Error e
          | Ok principal_id -> (
              match json_int64_field "github_user_id" j with
              | Error e -> Error e
              | Ok github_user_id -> (
                  match json_string_field "access_token_handle" j with
                  | Error e -> Error e
                  | Ok access_token_handle -> (
                      match json_string_list_field "scopes" j with
                      | Error e -> Error e
                      | Ok scopes -> (
                          match json_string_field "expires_at" j with
                          | Error e -> Error e
                          | Ok expires_at -> (
                              match json_int_field "app_id" j with
                              | Error e -> Error e
                              | Ok app_id ->
                                  let refresh_token_handle =
                                    match
                                      Yojson.Safe.Util.member
                                        "refresh_token_handle" j
                                    with
                                    | `String s when String.trim s <> "" ->
                                        Some s
                                    | _ -> None
                                  in
                                  make ~version ~principal_id ~github_user_id
                                    ~access_token_handle ?refresh_token_handle
                                    ~scopes ~expires_at ~app_id ())))))))
  | _ -> Error "github user token record must be a JSON object"

let export_json_string (r : t) = Yojson.Safe.to_string (to_json r)

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
