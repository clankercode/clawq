(** Live GitHub App installation Org/repository scope (P19.M2.E1.T003).

    Persists installation account, repository selection mode, authorized
    repositories, permissions, suspension, and a content revision. Applies
    installation / installation_repositories / suspension / deletion events and
    startup API snapshots idempotently.

    Product rules:
    - Org scope = live GitHub App installation scope (not PAT).
    - All-repository installations: newly granted repos become eligible
      automatically; optional diagnostic repo list may track known names.
    - Selected-repository removals, suspension, and deletion fail closed
      immediately.
    - All-mode access revocations maintain a denylist so [is_repo_authorized]
      fails closed for the removed repo until a snapshot clears it.
    - Deletion creates a durable tombstone. Delayed create events and stale
      snapshots cannot reactivate that installation id.

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md. *)

type account = {
  login : string;
  id : int;
  account_type : string;  (** "User" | "Organization" *)
}

type selection_mode = All_repos | Selected_repos

type repo_ref = {
  full_name : string;  (** owner/name *)
  id : int option;
  private_ : bool option;
}

type permissions = (string * string) list
(** GitHub permission name → access level. *)

type status = Active | Suspended of { reason : string option } | Deleted

type t = {
  installation_id : int;
  app_id : int option;
  account : account;
  selection : selection_mode;
  repositories : repo_ref list;
      (** Empty + [All_repos] means all current+future. For [Selected_repos],
          the allowlist. For [All_repos], optional diagnostic known-repo list.
      *)
  revoked_repositories : repo_ref list;
      (** Fail-closed denylist for [All_repos] after [Repos_removed] events.
          Cleared by full snapshot reconcile. Ignored for [Selected_repos]. *)
  permissions : permissions;
  status : status;
  revision : string;
      (** Stable content digest for concurrency / idempotency. *)
  updated_at : string;
}

type event =
  | Installation_created of {
      installation_id : int;
      account : account;
      selection : selection_mode;
      repositories : repo_ref list;
      permissions : permissions;
      app_id : int option;
    }
  | Installation_deleted of { installation_id : int }
  | Installation_suspend of { installation_id : int; reason : string option }
  | Installation_unsuspend of { installation_id : int }
  | Repos_added of { installation_id : int; repositories : repo_ref list }
  | Repos_removed of { installation_id : int; repositories : repo_ref list }
      (** Match by full_name (case-insensitive) or id when present. *)
  | Snapshot of t  (** Full replace from live API startup reconcile. *)

val account_of_json : Yojson.Safe.t -> (account, string) result
val repos_of_json : Yojson.Safe.t -> (repo_ref list, string) result

val permissions_of_json : Yojson.Safe.t -> (permissions, string) result
(** Strict parsers reused by authenticated GitHub API ingress. *)

val ensure_schema : Sqlite3.db -> unit
(** Idempotent SQLite schema for installation scope. *)

val compute_revision : t -> string
(** Content digest of logical scope fields (excludes [revision]/[updated_at]).
*)

val with_revision : t -> t
(** Set [revision] from [compute_revision]. *)

val upsert : db:Sqlite3.db -> t -> (t, string) result
(** Persist scope (INSERT OR REPLACE). Recomputes revision from content. A
    deleted installation cannot be replaced with an active/suspended row. *)

val get : db:Sqlite3.db -> installation_id:int -> (t option, string) result
(** Load by installation id (any status, including [Deleted]). *)

val list : db:Sqlite3.db -> (t list, string) result
(** All persisted installations ordered by installation_id. *)

val delete : db:Sqlite3.db -> installation_id:int -> (unit, string) result
(** Hard-delete the row. Prefer [mark_deleted] / [Installation_deleted] for
    fail-closed audit retention. *)

val mark_deleted :
  db:Sqlite3.db ->
  installation_id:int ->
  ?now:float ->
  unit ->
  (t option, string) result
(** Soft-delete: status [Deleted]. Returns [None] if no row existed. *)

val apply_event :
  db:Sqlite3.db -> ?now:float -> event -> (t option, string) result
(** Idempotent event apply. Returns resulting scope, or [None] when
    deleted/gone. *)

val is_repo_authorized : t -> repo_full_name:string -> bool
(** Fail closed: [false] if [Suspended]/[Deleted]; [All_repos] [true] when
    [Active] unless repo is in [revoked_repositories]; [Selected_repos] only if
    listed. Matching is case-insensitive on full_name. *)

val reconcile_from_snapshot : db:Sqlite3.db -> snapshot:t -> (t, string) result
(** Startup reconcile: replace with live API snapshot; idempotent when content
    revision matches. Clears drift including revoked denylist, except that a
    durable deletion tombstone always remains fail closed. *)

val selection_mode_to_string : selection_mode -> string
val selection_mode_of_string : string -> (selection_mode, string) result
val status_to_string : status -> string
val status_of_string : string -> (status, string) result

val normalize_full_name : string -> string
(** Lowercase trim for comparison. *)
