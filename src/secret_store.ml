(* Secret encryption at rest using AES-256-GCM *)
(* Master key derived from CLAWQ_MASTER_KEY env var via PBKDF2 *)

let pbkdf2_salt = "clawq-secret-store-v1"
let pbkdf2_iterations = 100_000
let test_iterations_override = ref None

(* Derive a 32-byte AES-256 key from a passphrase via PBKDF2-SHA256 *)
let derive_key ?(iterations = pbkdf2_iterations) ~passphrase () =
  let count =
    match !test_iterations_override with Some n -> n | None -> iterations
  in
  Pbkdf.pbkdf2 ~prf:`SHA256 ~password:passphrase ~salt:pbkdf2_salt ~count
    ~dk_len:32l

(* Get the master key from CLAWQ_MASTER_KEY env var *)
let get_master_key () =
  match Sys.getenv_opt "CLAWQ_MASTER_KEY" with
  | None -> Error "CLAWQ_MASTER_KEY environment variable is not set"
  | Some "" -> Error "CLAWQ_MASTER_KEY environment variable is empty"
  | Some passphrase -> Ok (derive_key ~passphrase ())

(* Encrypt a plaintext string. Returns base64-encoded "nonce:ciphertext" *)
let encrypt ~key plaintext =
  Mirage_crypto_rng_unix.use_default ();
  let nonce = Mirage_crypto_rng.generate 12 in
  let gcm_key = Mirage_crypto.AES.GCM.of_secret key in
  let ciphertext =
    Mirage_crypto.AES.GCM.authenticate_encrypt ~key:gcm_key ~nonce plaintext
  in
  (* Encode as: base64(nonce ++ ciphertext) *)
  let combined = nonce ^ ciphertext in
  Base64.encode_exn combined

(* Decrypt a base64-encoded "nonce:ciphertext" string *)
let decrypt ~key encoded =
  match Base64.decode encoded with
  | Error _ -> Error "Failed to decode base64"
  | Ok combined -> (
      if String.length combined < 13 then Error "Encrypted data too short"
      else
        let nonce = String.sub combined 0 12 in
        let ciphertext = String.sub combined 12 (String.length combined - 12) in
        let gcm_key = Mirage_crypto.AES.GCM.of_secret key in
        match
          Mirage_crypto.AES.GCM.authenticate_decrypt ~key:gcm_key ~nonce
            ciphertext
        with
        | None -> Error "Decryption failed (wrong key or corrupted data)"
        | Some plaintext -> Ok plaintext)

(* Encrypted secret prefix used in config values *)
let encrypted_prefix = "$ENC:"

(* Check if a value is an encrypted secret *)
let is_encrypted value =
  String.length value > String.length encrypted_prefix
  && String.sub value 0 (String.length encrypted_prefix) = encrypted_prefix

(* Encrypt a secret and return it with the $ENC: prefix *)
let encrypt_secret ~key plaintext = encrypted_prefix ^ encrypt ~key plaintext

(* Decrypt a $ENC: prefixed secret *)
let decrypt_secret ~key value =
  if is_encrypted value then
    let encoded =
      String.sub value
        (String.length encrypted_prefix)
        (String.length value - String.length encrypted_prefix)
    in
    decrypt ~key encoded
  else Ok value

(* Resolve a secret value: handle $ENV_VAR, $ENC:..., or passthrough *)
let resolve_secret ~encrypt_secrets value =
  if String.length value > 1 && value.[0] = '$' then begin
    if is_encrypted value then begin
      if encrypt_secrets then
        match get_master_key () with
        | Error msg ->
            Logs.warn (fun m -> m "Cannot decrypt secret: %s" msg);
            value
        | Ok key -> (
            match decrypt_secret ~key value with
            | Ok plaintext -> plaintext
            | Error msg ->
                Logs.warn (fun m -> m "Secret decryption failed: %s" msg);
                value)
      else
        (* encrypt_secrets disabled, cannot decrypt *)
        value
    end
    else begin
      (* $ENV_VAR indirection *)
      let var_name = String.sub value 1 (String.length value - 1) in
      try Sys.getenv var_name with Not_found -> value
    end
  end
  else value

(* Any config key whose name contains one of these substrings is treated as a
   secret (encrypted at rest, not logged).  Substring matching is intentionally
   broad: "token" catches "bot_token", "access_token", bare "token", etc. *)
let sensitive_substrings =
  [ "token"; "secret"; "password"; "api_key"; "private_key" ]

let contains_sub s sub = String_util.contains s sub
let is_secret_key key = List.exists (contains_sub key) sensitive_substrings

let maybe_encrypt_string ~key value =
  if
    value <> ""
    && (not (is_encrypted value))
    && String.length value > 1
    && value.[0] <> '$'
  then `String (encrypt_secret ~key value)
  else `String value

let rec encrypt_json_secrets ~key = function
  | `Assoc fields ->
      `Assoc
        (List.map
           (fun (name, value) ->
             if is_secret_key name then
               match value with
               | `String s -> (name, maybe_encrypt_string ~key s)
               | other -> (name, other)
             else (name, encrypt_json_secrets ~key value))
           fields)
  | `List items -> `List (List.map (encrypt_json_secrets ~key) items)
  | other -> other

(* Encrypt all known secrets in config and return updated JSON *)
let encrypt_config_secrets ~key json =
  try Ok (encrypt_json_secrets ~key json)
  with exn ->
    Error
      (Printf.sprintf "Failed to encrypt config secrets: %s"
         (Printexc.to_string exn))
