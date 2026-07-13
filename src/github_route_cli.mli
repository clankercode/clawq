(** Full-build command bridge for route/App plan, explicit apply, inspection,
    and secret-free diagnostics. *)

val cmd_with_db :
  db:Sqlite3.db -> config:Runtime_config.t -> string list -> string

val cmd : string list -> string
