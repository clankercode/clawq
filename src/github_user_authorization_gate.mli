(** Durable fail-closed gate for Principal-owned GitHub user authorization.

    The recovery state table is deliberately initialized with authorization
    enabled for installations that have never entered restore or compromise
    recovery. Once recovery sets it disabled, callers must deny user-token
    issuance and use until an explicit future re-enable path exists. *)

val ensure_schema : Sqlite3.db -> (unit, string) result
(** Create and seed the singleton recovery gate state. *)

val is_enabled : db:Sqlite3.db -> (bool, string) result
(** Read the durable gate. Storage or schema failures return [Error] so callers
    can fail closed. *)
