(** Full-build command bridge for route/App plan, explicit apply, inspection,
    and secret-free diagnostics. *)

val cmd_with_db :
  ?actor:Setup_plan_consent.actor ->
  db:Sqlite3.db ->
  config:Runtime_config.t ->
  string list ->
  string
(** Mutating commands require [actor] from a trusted authenticated adapter.
    Environment variables are deliberately not accepted as authority evidence.
    Read-only diagnostics remain available without an actor. *)

val cmd : string list -> string
