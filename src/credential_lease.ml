(* Credential lease resolution API

   Resolves credential handles to request-ready headers and environment
   variables at the call boundary.

   Security model:
   - [lease.identity] contains only redacted values — safe for logging,
     storage, prompts, and tool arguments.
   - [lease.decorations] is an abstract type that internally holds the raw
     credential values. Callers cannot construct, inspect, or extract values
     from decorations directly.
   - [apply_*] functions are the only way to access the raw values. They
     take [unit]-returning closures intended for side effects (HTTP requests,
     subprocess invocation) at the call boundary.
   - This is a trusted in-process API boundary, not capability security.
     The API prevents accidental credential leakage into logs, prompts,
     and tool arguments. It does not protect against malicious OCaml code
     that could use unsafe features to bypass the type system. *)

type redacted_identity = {
  handle_id : string;
  provider_type : string;
  description : string;
  redacted_value : string;
      (** First 3 chars + asterisks, for display/audit only. *)
}

type request_decoration =
  | Header of { name : string; value : string }
  | Env_var of { name : string; value : string }
  | Url_path_segment of { value : string }

type lease = {
  identity : redacted_identity;
  decorations : request_decoration list;
}

type resolution_error =
  | Handle_not_found of string
  | Handle_not_allowed of string
  | Env_var_unset of string
  | File_not_found of string
  | File_read_error of string * string  (** path, error message *)
  | Decryption_error of string
  | Prompt_not_supported

let redact_secret value =
  let len = String.length value in
  if len <= 3 then String.make len '*'
  else String.sub value 0 3 ^ String.make (len - 3) '*'

let resolve_env_var name =
  match Sys.getenv_opt name with
  | Some value when value <> "" -> Ok value
  | _ -> Error (Env_var_unset name)

let resolve_file path =
  let expanded = Runtime_config.expand_home path in
  if not (Sys.file_exists expanded) then Error (File_not_found expanded)
  else
    try
      let ic = open_in expanded in
      let value = In_channel.input_all ic in
      close_in ic;
      let trimmed = String.trim value in
      if trimmed = "" then Error (File_read_error (expanded, "empty file"))
      else Ok trimmed
    with
    | Sys_error msg -> Error (File_read_error (expanded, msg))
    | exn -> Error (File_read_error (expanded, Printexc.to_string exn))

let resolve_encrypted cipher_text =
  match Secret_store.get_master_key () with
  | Error msg -> Error (Decryption_error msg)
  | Ok key -> (
      match Secret_store.decrypt_secret ~key cipher_text with
      | Ok value -> Ok value
      | Error msg -> Error (Decryption_error msg))

let resolve_provider_value (provider : Runtime_config.credential_provider) =
  match provider with
  | Env_var { name } -> resolve_env_var name
  | File { path } -> resolve_file path
  | Encrypted { cipher_text } -> resolve_encrypted cipher_text
  | Prompt _ -> Error Prompt_not_supported

let make_lease (handle : Runtime_config.credential_handle)
    ~(header_name : string) : (lease, resolution_error) result =
  match resolve_provider_value handle.provider with
  | Error e -> Error e
  | Ok raw_value ->
      let redacted = redact_secret raw_value in
      let provider_type =
        match handle.provider with
        | Env_var _ -> "env_var"
        | File _ -> "file"
        | Encrypted _ -> "encrypted"
        | Prompt _ -> "prompt"
      in
      let identity =
        {
          handle_id = handle.id;
          provider_type;
          description = Option.value handle.description ~default:"";
          redacted_value = redacted;
        }
      in
      Ok
        {
          identity;
          decorations = [ Header { name = header_name; value = raw_value } ];
        }

let make_env_lease (handle : Runtime_config.credential_handle)
    ~(env_name : string) : (lease, resolution_error) result =
  match resolve_provider_value handle.provider with
  | Error e -> Error e
  | Ok raw_value ->
      let redacted = redact_secret raw_value in
      let provider_type =
        match handle.provider with
        | Env_var _ -> "env_var"
        | File _ -> "file"
        | Encrypted _ -> "encrypted"
        | Prompt _ -> "prompt"
      in
      let identity =
        {
          handle_id = handle.id;
          provider_type;
          description = Option.value handle.description ~default:"";
          redacted_value = redacted;
        }
      in
      Ok
        {
          identity;
          decorations = [ Env_var { name = env_name; value = raw_value } ];
        }

let make_url_lease (handle : Runtime_config.credential_handle) :
    (lease, resolution_error) result =
  match resolve_provider_value handle.provider with
  | Error e -> Error e
  | Ok raw_value ->
      let redacted = redact_secret raw_value in
      let provider_type =
        match handle.provider with
        | Env_var _ -> "env_var"
        | File _ -> "file"
        | Encrypted _ -> "encrypted"
        | Prompt _ -> "prompt"
      in
      let identity =
        {
          handle_id = handle.id;
          provider_type;
          description = Option.value handle.description ~default:"";
          redacted_value = redacted;
        }
      in
      Ok { identity; decorations = [ Url_path_segment { value = raw_value } ] }

let resolution_error_to_string = function
  | Handle_not_found id -> Printf.sprintf "credential handle '%s' not found" id
  | Handle_not_allowed id ->
      Printf.sprintf
        "credential handle '%s' is not allowed by current access policy" id
  | Env_var_unset name ->
      Printf.sprintf "environment variable '%s' is not set or empty" name
  | File_not_found path -> Printf.sprintf "credential file '%s' not found" path
  | File_read_error (path, msg) ->
      Printf.sprintf "error reading credential file '%s': %s" path msg
  | Decryption_error msg -> Printf.sprintf "decryption failed: %s" msg
  | Prompt_not_supported ->
      "prompt-based credentials are not supported for automatic resolution"

let find_handle ~(config : Runtime_config.t) ~(handle_id : string) =
  match Runtime_config.find_credential_handle config handle_id with
  | None -> Error (Handle_not_found handle_id)
  | Some handle -> Ok handle

let find_scoped_handle ~(config : Runtime_config.t)
    ~(allowed_handle_ids : string list) ~(handle_id : string) =
  if not (List.mem handle_id allowed_handle_ids) then
    Error (Handle_not_allowed handle_id)
  else find_handle ~config ~handle_id

let resolve_lease ~(config : Runtime_config.t) ~(handle_id : string)
    ~(header_name : string) : (lease, resolution_error) result =
  match find_handle ~config ~handle_id with
  | Error e -> Error e
  | Ok handle -> make_lease handle ~header_name

let resolve_env_lease ~(config : Runtime_config.t) ~(handle_id : string)
    ~(env_name : string) : (lease, resolution_error) result =
  match find_handle ~config ~handle_id with
  | Error e -> Error e
  | Ok handle -> make_env_lease handle ~env_name

let resolve_url_lease ~(config : Runtime_config.t) ~(handle_id : string) :
    (lease, resolution_error) result =
  match find_handle ~config ~handle_id with
  | Error e -> Error e
  | Ok handle -> make_url_lease handle

let resolve_scoped_lease ~(config : Runtime_config.t)
    ~(allowed_handle_ids : string list) ~(handle_id : string)
    ~(header_name : string) : (lease, resolution_error) result =
  match find_scoped_handle ~config ~allowed_handle_ids ~handle_id with
  | Error e -> Error e
  | Ok handle -> make_lease handle ~header_name

let resolve_scoped_env_lease ~(config : Runtime_config.t)
    ~(allowed_handle_ids : string list) ~(handle_id : string)
    ~(env_name : string) : (lease, resolution_error) result =
  match find_scoped_handle ~config ~allowed_handle_ids ~handle_id with
  | Error e -> Error e
  | Ok handle -> make_env_lease handle ~env_name

let resolve_scoped_url_lease ~(config : Runtime_config.t)
    ~(allowed_handle_ids : string list) ~(handle_id : string) :
    (lease, resolution_error) result =
  match find_scoped_handle ~config ~allowed_handle_ids ~handle_id with
  | Error e -> Error e
  | Ok handle -> make_url_lease handle

let handle_ids_of_effective_access (access : Runtime_config.effective_access) =
  List.map
    (fun (item : Runtime_config.effective_access_item) -> item.value)
    access.credential_handles

let resolve_effective_access_lease ~(config : Runtime_config.t)
    ~(access : Runtime_config.effective_access) ~(handle_id : string)
    ~(header_name : string) : (lease, resolution_error) result =
  resolve_scoped_lease ~config
    ~allowed_handle_ids:(handle_ids_of_effective_access access)
    ~handle_id ~header_name

let resolve_effective_access_env_lease ~(config : Runtime_config.t)
    ~(access : Runtime_config.effective_access) ~(handle_id : string)
    ~(env_name : string) : (lease, resolution_error) result =
  resolve_scoped_env_lease ~config
    ~allowed_handle_ids:(handle_ids_of_effective_access access)
    ~handle_id ~env_name

let resolve_effective_access_url_lease ~(config : Runtime_config.t)
    ~(access : Runtime_config.effective_access) ~(handle_id : string) :
    (lease, resolution_error) result =
  resolve_scoped_url_lease ~config
    ~allowed_handle_ids:(handle_ids_of_effective_access access)
    ~handle_id

let resolve_snapshot_lease ~(config : Runtime_config.t)
    ~(snapshot : Access_snapshot.t) ~(handle_id : string)
    ~(header_name : string) : (lease, resolution_error) result =
  resolve_scoped_lease ~config ~allowed_handle_ids:snapshot.credential_handles
    ~handle_id ~header_name

let resolve_snapshot_env_lease ~(config : Runtime_config.t)
    ~(snapshot : Access_snapshot.t) ~(handle_id : string) ~(env_name : string) :
    (lease, resolution_error) result =
  resolve_scoped_env_lease ~config ~allowed_handle_ids:snapshot.credential_handles
    ~handle_id ~env_name

let resolve_snapshot_url_lease ~(config : Runtime_config.t)
    ~(snapshot : Access_snapshot.t) ~(handle_id : string) :
    (lease, resolution_error) result =
  resolve_scoped_url_lease ~config ~allowed_handle_ids:snapshot.credential_handles
    ~handle_id

let apply_headers (lease : lease) f =
  let headers =
    List.filter_map
      (fun d ->
        match d with Header { name; value } -> Some (name, value) | _ -> None)
      lease.decorations
  in
  f headers

let apply_env_vars (lease : lease) f =
  let env_vars =
    List.filter_map
      (fun d ->
        match d with Env_var { name; value } -> Some (name, value) | _ -> None)
      lease.decorations
  in
  f env_vars

let apply_url_segment (lease : lease) f =
  let segment =
    List.find_map
      (fun d ->
        match d with Url_path_segment { value } -> Some value | _ -> None)
      lease.decorations
  in
  match segment with Some value -> f value | None -> f ""
