(** Migrate legacy requester identities without unsafe coalescing
    (P21.M1.E3.T003).

    Legacy Room/session/task rows carry free-form requester strings, connector
    labels, and display names that predate durable Principals. This module:

    - Classifies each legacy row.
    - Backfills only rows with an unambiguous, adapter-verifiable Connector
      namespace (tenant/workspace) plus immutable user ID that resolve through
      an active verified actor and active identity link to a live Principal
      (following [Merged_into] tombstones).
    - Marks everything else [legacy_unresolved].
    - Preserves read/audit and allowed explicit App behavior for unresolved
      rows, and always denies user-attributed authority for them.
    - Never coalesces distinct identities by display name, email, Room, or
      Session; never creates Principals from untrusted legacy alone; never
      rewrites historical {!Principal_merge.actor_snapshot} / {!Actor_snapshot}
      rows.
    - Invalidates ambiguous active jobs for user-attributed work.
    - Records upgrade runs that can be rolled back without disturbing history.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0005-separate-human-principals-from-room-sessions.md. *)

module P = Principal_identity

val schema_version : int
(** Migration result schema version; starts at 1. *)

val ensure_schema : Sqlite3.db -> unit
(** Idempotent tables for migration runs, per-row results, and job
    invalidations. Does not alter historical snapshot tables. *)

(** {1 Legacy source rows} *)

type source_kind =
  | Background_task
  | Workflow_run
  | Fixture  (** Synthetic / fixture input (tests and operator imports). *)

val string_of_source_kind : source_kind -> string
val source_kind_of_string : string -> (source_kind, string) result

type legacy_row = {
  source_kind : source_kind;
  source_id : string;
      (** Opaque source primary key as string (task id, run id, fixture id). *)
  connector : string option;
      (** Connector label (e.g. ["teams"]). Display/process labels alone never
          establish identity. *)
  tenant_or_workspace : string option;
      (** Adapter namespace: tenant, workspace, guild, bot-account, or issuer.
      *)
  immutable_user_id : string option;
      (** Connector-stable human user id. Never a display name. *)
  requester_name : string option;
      (** Mutable display only; never used for backfill or coalescing. *)
  room_id : string option;
  session_id : string option;
  origin_json : string option;
  raw_requester : string option;
      (** Free-form [background_tasks.requester] / similar. *)
  job_active : bool;
      (** True when the source is a non-terminal job (queued/pending/running).
          Ambiguous active jobs are invalidated for user attribution. *)
  evidence_json : string;
      (** Frozen copy of the original requester evidence. Migration never
          mutates this payload after insert. *)
}
(** One pre-Principal requester identity observation. *)

val make_legacy_row :
  source_kind:source_kind ->
  source_id:string ->
  ?connector:string ->
  ?tenant_or_workspace:string ->
  ?immutable_user_id:string ->
  ?requester_name:string ->
  ?room_id:string ->
  ?session_id:string ->
  ?origin_json:string ->
  ?raw_requester:string ->
  ?job_active:bool ->
  ?evidence_json:string ->
  unit ->
  (legacy_row, string) result
(** Build a row. Rejects empty [source_id]. Fills [evidence_json] from fields
    when omitted. *)

val legacy_row_of_origin :
  source_kind:source_kind ->
  source_id:string ->
  ?job_active:bool ->
  ?raw_requester:string ->
  ?session_id:string ->
  Room_origin.t ->
  (legacy_row, string) result
(** Project a {!Room_origin.t} into a legacy row. [requester_name] is retained
    as display evidence only. *)

(** {1 Classification} *)

type unresolved_reason =
  | Missing_connector
  | Missing_namespace
  | Missing_user_id
  | Display_name_only  (** Only display/name evidence; no immutable user id. *)
  | Non_adapter_connector of string
      (** CLI/direct/process metadata cannot backfill without enrolment. *)
  | Malformed_actor_key of string
  | Actor_not_found
      (** Shape is unambiguous but no verified Connector actor exists yet;
          migration does not invent first-seen Principals from legacy. *)
  | Actor_disabled
  | Actor_unlinked
      (** An explicitly unlinked actor's stored principal is historical
          metadata, not backfill authority. *)
  | Actor_not_verified
      (** Only adapter-verified Connector actors may backfill legacy work. *)
  | Active_identity_link_missing
      (** An active verified actor still needs an active identity link; its
          stored principal alone is never enough. *)
  | Principal_not_active of string
  | Ambiguous_evidence of string
  | Coalesce_refused of string
      (** Would require merging distinct identities (e.g. conflicting user ids
          under one display name batch). *)

val string_of_unresolved_reason : unresolved_reason -> string

type classification =
  | Backfill of {
      actor_key : P.connector_actor_key;
      principal_id : P.principal_id;
      followed_merge_alias : bool;
          (** True when live ownership follows a [Merged_into] tombstone.
              Historical snapshots are not rewritten. *)
      actor_revision : int;
      identity_link_id : string option;
    }
  | Legacy_unresolved of { reason : unresolved_reason }

val classify_row :
  db:Sqlite3.db -> legacy_row -> (classification, string) result
(** Classification against live store state. Backfill requires an active,
    adapter-verified actor and active identity link. Never creates Principals,
    merges, or rewrites snapshots. *)

(** {1 Authority after migration} *)

type authority = {
  user_attributed_allowed : bool;
      (** True only for successful backfill to an Active Principal. Always false
          for [legacy_unresolved]. *)
  app_behavior_allowed : bool;
      (** Explicit App/PAT paths remain allowed (true for all classifications).
      *)
  read_audit_allowed : bool;  (** Always true; unresolved rows stay readable. *)
}

val authority_of_classification : classification -> authority
(** Authority contract for a classified row. *)

(** {1 Migration run / records} *)

type migration_status =
  | Backfilled
  | Unresolved
  | Job_invalidated
      (** Active job classified unresolved; user-attributed work denied and
          recorded as invalidated. *)

val string_of_migration_status : migration_status -> string

type migration_record = {
  id : string;
  run_id : string;
  row : legacy_row;
  classification : classification;
  status : migration_status;
  authority : authority;
  created_at : string;
}

type migrate_report = {
  run_id : string;
  backfilled : int;
  unresolved : int;
  jobs_invalidated : int;
  records : migration_record list;
  historical_snapshots_rewritten : int;
      (** Always [0]. Exposed so fixtures can assert the invariant. *)
}

val generate_run_id : ?now:float -> unit -> string
val generate_record_id : ?now:float -> unit -> string

val migrate_rows :
  db:Sqlite3.db ->
  rows:legacy_row list ->
  ?run_id:string ->
  ?now:float ->
  unit ->
  (migrate_report, string) result
(** Classify and persist results for the given rows. Idempotent per
    [(source_kind, source_id)] within a re-run of the same content: existing
    records for a source are left as-is when already migrated under a prior
    unrolled-back run. Never rewrites historical actor snapshots. Active
    ambiguous jobs are recorded in the invalidation table. *)

val load_legacy_from_db : db:Sqlite3.db -> (legacy_row list, string) result
(** Load candidate rows from [background_tasks] and [workflow_runs] when those
    tables exist; missing tables yield empty contribution. *)

val migrate_database :
  db:Sqlite3.db ->
  ?run_id:string ->
  ?now:float ->
  unit ->
  (migrate_report, string) result
(** Load legacy rows from the database and migrate them. *)

(** {1 Query helpers} *)

val get_record :
  db:Sqlite3.db ->
  source_kind:source_kind ->
  source_id:string ->
  (migration_record option, string) result

val list_records_for_run :
  db:Sqlite3.db -> run_id:string -> (migration_record list, string) result

val is_job_invalidated :
  db:Sqlite3.db ->
  source_kind:source_kind ->
  source_id:string ->
  (bool, string) result
(** True when an active ambiguous job was invalidated for user-attributed
    authority. *)

val user_authority_allowed :
  db:Sqlite3.db ->
  source_kind:source_kind ->
  source_id:string ->
  (bool, string) result
(** Fail closed: missing migration record or unresolved → [false]. *)

val require_migrated_user_dispatch :
  db:Sqlite3.db ->
  source_kind:source_kind ->
  source_id:string ->
  (unit, string) result
(** Guard a dispatch which explicitly claims human authority. Sources created
    after the upgrade have no migration record and remain outside this legacy
    guard. A migrated source must be backfilled and must not be invalidated;
    unresolved or invalidated sources fail closed. Explicit App/PAT paths do not
    call this human-authority guard. *)

(** {1 Rollback} *)

val rollback_run : db:Sqlite3.db -> run_id:string -> (int, string) result
(** Remove migration records and job invalidations for [run_id]. Returns the
    number of migration records removed. Does not delete Principals, identity
    links, or historical actor snapshots. Safe to re-run upgrade after. *)

(** {1 Fixtures (upgrade / rollback proofs)} *)

type fixture_case = {
  name : string;
  rows : legacy_row list;
  seed : db:Sqlite3.db -> unit;
      (** Optional store seeding (Principals / actors / merge tombstones /
          historical snapshots). *)
  expect_backfilled : int;
  expect_unresolved : int;
  expect_jobs_invalidated : int;
}

val upgrade_fixture_cases : unit -> fixture_case list
(** Built-in upgrade fixture cases used by tests and as living documentation. *)

val run_upgrade_fixture :
  fixture_case -> (migrate_report * Sqlite3.db, string) result
(** Open an in-memory DB, seed, migrate, and return the report (DB still open;
    caller must close). *)

val prove_upgrade_and_rollback : fixture_case -> (unit, string) result
(** End-to-end: upgrade → assert counts and snapshot immutability → rollback →
    assert clean migration state → re-upgrade succeeds. *)
