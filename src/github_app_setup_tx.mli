(** Resumable GitHub App manifest setup transactions (P19.M2.E1.T001).

    Authorized setup creates an expiring persisted transaction bound to a
    principal, Room or Session, requested scope, and base revision. It emits the
    exact browser manifest create URL, requested permissions, subscribed events,
    and a one-time random state (CSRF). Restart safely resumes an unexpired open
    transaction. Channel-facing render is secret-free (no private key, client
    secret, webhook secret, or PEM material).

    Secrets from the callback exchange are out of scope here (T002). This module
    stores only non-secret transaction fields plus the opaque state token.

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md and
    docs/adr/0002-use-unified-live-github-app-routes.md. *)

type principal = {
  id : string;  (** Durable principal id of the authorizing actor. *)
  kind : string;  (** Free-form kind label (e.g. "principal", "cli"). *)
  label : string option;  (** Display only; never credentials. *)
}

type bind_target =
  | Room of string  (** Originating Room id. *)
  | Session of string  (** Originating Session key. *)

type repo_selection = All_repos | Selected of string list

type requested_scope = {
  org : string option;
      (** [None] = user-account apps/new; [Some org] = org apps/new. *)
  selection : repo_selection;
  permissions : (string * string) list;
      (** GitHub permission name → access level, e.g. [("issues", "write")]. *)
  events : string list;  (** Subscribed webhook event names. *)
}

type status = Open | Consumed | Expired | Superseded

type t = {
  id : string;
  principal : principal;
  bind : bind_target;
  scope : requested_scope;
  base_revision : string;
  state : string;  (** One-time CSRF / callback correlation token. *)
  manifest_url : string;  (** Exact browser URL including state + manifest. *)
  manifest_json : Yojson.Safe.t;
      (** Secret-free public App manifest body posted via the URL. *)
  public_base_url : string;
      (** Public base used to derive hook_url / redirect / callback. *)
  created_at : string;
  expires_at : string;
  status : status;
}

val default_ttl_seconds : float
(** Default transaction TTL: 30 minutes. *)

val default_permissions : (string * string) list
(** Conservative defaults aligned with live App routes / webhook consumers. *)

val default_events : string list
(** Default subscribed events for PR/Issue/CI ingress. *)

val default_hook_path : string
(** Shared App webhook path segment under [public_base_url]. *)

val default_callback_path : string
(** Browser callback path for the manifest flow (T002 exchange). *)

val ensure_schema : Sqlite3.db -> unit
(** Idempotent SQLite schema for [github_app_setup_tx]. *)

val generate_state : unit -> string
(** Cryptographically random one-time state token (hex). *)

val build_manifest_json :
  app_name:string ->
  public_base_url:string ->
  ?description:string ->
  ?hook_path:string ->
  ?callback_path:string ->
  ?url:string ->
  permissions:(string * string) list ->
  events:string list ->
  unit ->
  Yojson.Safe.t
(** Secret-free GitHub App Manifest body (name, urls, permissions, events). *)

val build_manifest_url :
  ?org:string -> state:string -> manifest_json:Yojson.Safe.t -> unit -> string
(** Exact GitHub apps/new URL with state + url-encoded manifest. *)

val create :
  db:Sqlite3.db ->
  principal:principal ->
  bind:bind_target ->
  base_revision:string ->
  public_base_url:string ->
  ?app_name:string ->
  ?description:string ->
  ?scope:requested_scope ->
  ?ttl_seconds:float ->
  ?now:float ->
  ?id:string ->
  ?state:string ->
  unit ->
  (t, string) result
(** Create and persist a new open transaction.

    Generates one-time [state], builds the secret-free manifest and exact
    browser URL, and supersedes any previous [Open] transaction for the same
    principal + bind target. Callers must authorize the principal before
    invoking [create]. *)

val resume :
  db:Sqlite3.db ->
  ?id:string ->
  principal_id:string ->
  bind:bind_target ->
  ?now:float ->
  unit ->
  (t, string) result
(** Resume an open, unexpired transaction for the same principal + bind.

    When [id] is given, loads that row and verifies principal + bind + open +
    unexpired. Without [id], selects the latest open unexpired row for the pair.
    Expired open rows are marked [Expired] and rejected. Mismatched principal
    cannot resume another's transaction. *)

val get : db:Sqlite3.db -> id:string -> (t option, string) result
(** Load a transaction by id (any status). *)

val find_by_state : db:Sqlite3.db -> state:string -> (t option, string) result
(** Lookup by one-time state (hook for T002 callback verification). *)

val mark_consumed :
  db:Sqlite3.db ->
  id:string ->
  principal_id:string ->
  ?now:float ->
  unit ->
  (t, string) result
(** Mark an open unexpired transaction consumed after successful callback
    exchange (T002). Enforces principal ownership. *)

val channel_render : t -> string
(** Channel-safe human summary. Never includes private_key, client_secret,
    webhook_secret, or PEM material. Shows public manifest URL (which embeds the
    one-time [state] query required by the browser flow), bind, scope summary,
    status, and expiry. Does not emit a standalone state field or the raw
    manifest JSON body. *)

val is_expired : ?now:float -> t -> bool
val status_to_string : status -> string
val status_of_string : string -> (status, string) result
val bind_to_string : bind_target -> string

val string_of_bind : bind_target -> string
(** Alias of [bind_to_string] for call-site clarity. *)
