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

type request_decoration
(** Opaque request decoration. Internally holds raw credential values but
    callers cannot construct or inspect them. Use [apply_*] functions to perform
    side effects with the credentials at the call boundary. *)

type lease = {
  identity : redacted_identity;
      (** Redacted identity — safe for logging, storage, and display. *)
  decorations : request_decoration list;
      (** Opaque decorations holding raw credentials. Use [apply_*] functions to
          perform authenticated operations. Do not log, store, or marshal this
          field — it contains raw secrets in an opaque wrapper. *)
}

type resolution_error =
  | Handle_not_found of string
  | Handle_not_allowed of string
  | Env_var_unset of string
  | File_not_found of string
  | File_read_error of string * string
  | Decryption_error of string
  | Prompt_not_supported

val resolution_error_to_string : resolution_error -> string

val make_lease :
  Runtime_config.credential_handle ->
  header_name:string ->
  (lease, resolution_error) result

val make_env_lease :
  Runtime_config.credential_handle ->
  env_name:string ->
  (lease, resolution_error) result

val make_url_lease :
  Runtime_config.credential_handle -> (lease, resolution_error) result

val resolve_lease :
  config:Runtime_config.t ->
  handle_id:string ->
  header_name:string ->
  (lease, resolution_error) result

val resolve_env_lease :
  config:Runtime_config.t ->
  handle_id:string ->
  env_name:string ->
  (lease, resolution_error) result

val resolve_url_lease :
  config:Runtime_config.t ->
  handle_id:string ->
  (lease, resolution_error) result

val resolve_scoped_lease :
  config:Runtime_config.t ->
  allowed_handle_ids:string list ->
  handle_id:string ->
  header_name:string ->
  (lease, resolution_error) result
(** Resolve a handle only if [handle_id] appears in [allowed_handle_ids]. Policy
    denial happens before provider resolution, so unauthorized handles cannot
    read environment variables, files, or encrypted payloads. *)

val resolve_scoped_env_lease :
  config:Runtime_config.t ->
  allowed_handle_ids:string list ->
  handle_id:string ->
  env_name:string ->
  (lease, resolution_error) result

val resolve_scoped_url_lease :
  config:Runtime_config.t ->
  allowed_handle_ids:string list ->
  handle_id:string ->
  (lease, resolution_error) result

val resolve_effective_access_lease :
  config:Runtime_config.t ->
  access:Runtime_config.effective_access ->
  handle_id:string ->
  header_name:string ->
  (lease, resolution_error) result

val resolve_effective_access_env_lease :
  config:Runtime_config.t ->
  access:Runtime_config.effective_access ->
  handle_id:string ->
  env_name:string ->
  (lease, resolution_error) result

val resolve_effective_access_url_lease :
  config:Runtime_config.t ->
  access:Runtime_config.effective_access ->
  handle_id:string ->
  (lease, resolution_error) result

val resolve_snapshot_lease :
  config:Runtime_config.t ->
  snapshot:Access_snapshot.t ->
  handle_id:string ->
  header_name:string ->
  (lease, resolution_error) result

val resolve_snapshot_env_lease :
  config:Runtime_config.t ->
  snapshot:Access_snapshot.t ->
  handle_id:string ->
  env_name:string ->
  (lease, resolution_error) result

val resolve_snapshot_url_lease :
  config:Runtime_config.t ->
  snapshot:Access_snapshot.t ->
  handle_id:string ->
  (lease, resolution_error) result

val apply_headers : lease -> ((string * string) list -> unit) -> unit
(** [apply_headers lease f] calls [f] with HTTP headers containing the raw
    credential. [f] should perform the authenticated HTTP request as a side
    effect and return [unit]. *)

val apply_env_vars : lease -> ((string * string) list -> unit) -> unit
(** [apply_env_vars lease f] calls [f] with environment variables containing the
    raw credential. [f] should invoke the subprocess as a side effect and return
    [unit]. *)

val apply_url_segment : lease -> (string -> unit) -> unit
(** [apply_url_segment lease f] calls [f] with a URL path segment containing the
    raw credential. [f] should perform the authenticated HTTP request as a side
    effect and return [unit]. *)
