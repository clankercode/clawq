(** Persist one-time Principal-bound GitHub user authorization transactions
    (P21.M2.E1.T002).

    Versioned SQLite store for web (S256 PKCE) and device authorization flows.
    Each transaction binds:
    - flow kind
    - Principal
    - Connector actor
    - source Room or Session
    - App/client identity (host, app id, opaque client-id handle)
    - intended GitHub account (optional pin)
    - expiry
    - one-time state machine
    - base revision
    - continuation handle (opaque; secrets never stored here)

    Restart resumes an open unexpired transaction for the same bound context.
    Cancel, expiry, replay, swapped context, and competing completion are
    terminal: terminal statuses never reopen.

    OAuth secrets, device codes, PKCE verifiers, and access tokens are out of
    scope (later flow tasks). This module stores only non-secret correlation and
    context fields.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0006-use-principal-owned-github-user-tokens.md. *)

val schema_version : int
(** Row schema version; starts at 1. *)

val default_ttl_seconds : float
(** Default authorization transaction TTL: 15 minutes. *)

(** {1 Flow kind} *)

type flow_kind =
  | Web_pkce  (** Browser authorization with state + S256 PKCE. *)
  | Device  (** GitHub App device authorization. *)

val string_of_flow_kind : flow_kind -> string
val flow_kind_of_string : string -> (flow_kind, string) result

(** {1 Source Room / Session} *)

type source =
  | Room of string  (** Originating Room id (routing context only). *)
  | Session of string  (** Originating Session key (routing context only). *)

val string_of_source : source -> string
val source_kind_and_id : source -> string * string
val source_of_kind_id : kind:string -> id:string -> (source, string) result

(** {1 App / client (non-secret)} *)

type app_client = {
  host : string;  (** GitHub host; V1 live is [github.com]. *)
  app_id : int;  (** Numeric GitHub App id. *)
  client_id_handle : string;
      (** Opaque credential-store handle for the OAuth client id — never
          plaintext client secret. *)
}
(** App/client identity for the authorization request. *)

(** {1 Intended GitHub account} *)

type intended_account = {
  github_user_id : int64 option;
      (** Optional numeric GitHub user id pin when preference already selected.
      *)
  login_hint : string option;
      (** Optional login display hint; never authorization by itself. *)
}
(** Selection hint only; verified ownership is established after exchange. *)

val empty_intended_account : intended_account

(** {1 One-time status machine} *)

type status =
  | Open  (** Active; restart may resume while unexpired. *)
  | Completed  (** One-time success; never reopens. *)
  | Cancelled
  | Expired
  | Superseded  (** Replaced by a newer open tx for the same bind key. *)
  | Rejected
      (** Fail-closed terminal after swapped context or competing completion
          race. *)

val string_of_status : status -> string
val status_of_string : string -> (status, string) result

val status_is_terminal : status -> bool
(** [true] for every status except [Open]. Terminal states never reopen. *)

val status_is_resumable : status -> bool
(** [Open] only. *)

(** {1 Transaction record} *)

type t = {
  version : int;
  id : string;
  flow_kind : flow_kind;
  principal_id : string;
  connector_actor : Principal_identity.connector_actor_key;
  source : source;
  app : app_client;
  intended_account : intended_account;
  one_time_state : string;
      (** One-time correlation token (OAuth state / device correlation id). Not
          a secret credential; never a code_verifier, device_code, or access
          token. *)
  base_revision : string;
      (** Bound config/policy/principal revision at create for CAS-safe later
          steps. *)
  continuation_handle : string;
      (** Opaque private-delivery handle for URLs/codes (T003). Never embeds
          secrets. *)
  created_at : string;
  expires_at : string;
  status : status;
  terminal_reason : string option;
  completed_at : string option;
  cancelled_at : string option;
  updated_at : string;
}
(** Principal-bound one-time authorization transaction. *)

type bound_context = {
  principal_id : string;
  connector_actor : Principal_identity.connector_actor_key;
  source : source;
  app_id : int;
  base_revision : string;
}
(** Bound context required to resume or complete. Mismatch is swapped context.
*)

val ensure_schema : Sqlite3.db -> unit
(** Idempotent versioned SQLite schema for [github_user_auth_tx]. *)

val generate_one_time_state : unit -> string
(** Cryptographically random one-time correlation token (hex). *)

val generate_id : ?now:float -> unit -> string

val is_expired : ?now:float -> t -> bool
(** Lexicographic ISO-8601 compare of [now] against [expires_at]. *)

val actor_key_string : t -> string
(** [Principal_identity.actor_identity_key] for the bound Connector actor. *)

val context_matches : t -> bound_context -> bool
(** [true] when principal, actor, source, app id, and base revision all match.
*)

val create :
  db:Sqlite3.db ->
  flow_kind:flow_kind ->
  principal_id:string ->
  connector_actor:Principal_identity.connector_actor_key ->
  source:source ->
  app:app_client ->
  ?intended_account:intended_account ->
  base_revision:string ->
  continuation_handle:string ->
  ?ttl_seconds:float ->
  ?now:float ->
  ?id:string ->
  ?one_time_state:string ->
  unit ->
  (t, string) result
(** Create and persist an [Open] transaction.

    Supersedes any previous [Open] transaction for the same principal + source +
    flow_kind (atomic replace). Generates [one_time_state] when omitted. *)

val get : db:Sqlite3.db -> id:string -> (t option, string) result
(** Load by id (any status). *)

val find_by_one_time_state :
  db:Sqlite3.db -> one_time_state:string -> (t option, string) result
(** Lookup by one-time correlation token (callback / device poll hooks). *)

val resume :
  db:Sqlite3.db ->
  ?id:string ->
  context:bound_context ->
  flow_kind:flow_kind ->
  ?now:float ->
  unit ->
  (t, string) result
(** Restart-safe resume of an [Open] unexpired transaction for the same bound
    context and flow kind.

    When [id] is given, loads that row and verifies context + open + unexpired.
    Without [id], selects the latest open unexpired row for principal + source +
    flow_kind, then verifies full context. Expired open rows are marked
    [Expired] and rejected. Terminal statuses never resume. Swapped context
    rejects without reopening. *)

val cancel :
  db:Sqlite3.db ->
  id:string ->
  context:bound_context ->
  ?reason:string ->
  ?now:float ->
  unit ->
  (t, string) result
(** Cancel an open unexpired transaction. Terminal statuses fail closed. *)

val expire :
  db:Sqlite3.db -> id:string -> ?now:float -> unit -> (t, string) result
(** Mark [Open] past-[expires_at] as [Expired]. Rejects if not yet expired or
    already terminal. *)

val complete :
  db:Sqlite3.db ->
  id:string ->
  context:bound_context ->
  one_time_state:string ->
  ?now:float ->
  unit ->
  (t, string) result
(** One-time completion under matching context and one-time state.

    - Matching open unexpired → [Completed] (CAS; competing completion fails)
    - Replay of state after [Completed] → error; status stays terminal
    - Swapped context with matching one-time state → mark [Rejected], error
    - Wrong one-time state → error without status change
    - Expired open → mark [Expired], error *)

val redacted_summary : t -> string
(** Human summary without secrets (no tokens, codes, client secrets, or
    verifiers). *)
