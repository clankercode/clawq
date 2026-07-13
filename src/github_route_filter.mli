(** Versioned advanced GitHub route filters (P20.M1.E1.T001).

    Extends the P19 baseline event/repo include-exclude filter with typed PR and
    Issue predicates. Schema version is persisted with each filter. Operators
    and field types are validated; raw JSON / free-form predicates are rejected.

    Migration: filters without [schema_version] (v0 baseline) migrate to v1 with
    empty advanced sections; empty include/exclude lists are preserved as-is
    (they still mean baseline allow-all / no excludes).

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md. *)

val current_schema_version : int
(** Latest supported filter schema version (currently 1). *)

type set_op = [ `Eq | `Neq | `In | `Not_in ]
(** Set/identity operators for labels, author, team, assignee, milestone. *)

type glob_op = [ `Eq | `Neq | `In | `Not_in | `Glob ]
(** Branch/path operators; [`Glob] is path/branch glob only (not free
    expressions). *)

type set_match = { op : set_op; values : string list }
type glob_match = { op : glob_op; values : string list }

type pr_advanced = {
  base_branch : glob_match option;
  head_branch : glob_match option;
  changed_path : glob_match option;
  labels : set_match option;
  author : set_match option;
  team : set_match option;
  draft : bool option;
      (** [Some true] = drafts only; [Some false] = non-drafts only; [None] =
          any *)
}

type issue_advanced = {
  labels : set_match option;
  author : set_match option;
  team : set_match option;
  assignee : set_match option;
  milestone : set_match option;
}

type t = {
  schema_version : int;
  include_events : string list;
      (** empty = baseline defaults (allow non-excluded) *)
  exclude_events : string list;
  include_repos : string list;
      (** for Org: optional narrow; empty = all authorized *)
  exclude_repos : string list;
  pr : pr_advanced;
  issue : issue_advanced;
}

type v0 = {
  include_events : string list;
  exclude_events : string list;
  include_repos : string list;
  exclude_repos : string list;
}
(** Pre-versioned baseline filter (P19). *)

val empty_pr : pr_advanced
val empty_issue : issue_advanced
val empty_advanced : pr_advanced * issue_advanced

val default : t
(** schema_version = [current_schema_version]; empty lists; empty advanced. *)

val of_v0 : v0 -> t
(** Lift a baseline filter to the current schema with empty advanced predicates.
*)

val migrate_v0_to_v1 : v0 -> t
(** Explicit v0 → v1 migration. Empty include/exclude lists are kept unchanged
    (not rewritten to default event tokens). Advanced PR/Issue sections empty.
*)

val validate : t -> (t, string) result
(** Check operators, value shapes, and schema version. *)

val to_json : t -> Yojson.Safe.t

val of_json : Yojson.Safe.t -> (t, string) result
(** Parse and validate. Missing [schema_version] is treated as v0 baseline and
    migrated. Rejects raw JSON predicates and unknown free-form predicate keys.
*)

val set_op_to_string : set_op -> string
val set_op_of_string : string -> (set_op, string) result
val glob_op_to_string : glob_op -> string
val glob_op_of_string : string -> (glob_op, string) result

val has_advanced : t -> bool
(** True when any PR or Issue advanced field is set. *)

val requires_changed_paths : t -> bool
(** True when PR [changed_path] is configured (enrichment demand signal). *)

val requires_team_membership : t -> bool
(** True when PR or Issue [team] is configured (enrichment demand signal). *)
