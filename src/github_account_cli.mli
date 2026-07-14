(** Full-build command bridge for Principal-owned GitHub account lifecycle
    (P21.M4.E1.T001).

    Subcommands route to redacted {!Github_account_admin_surface} views and
    link/relink/unlink plans. Authorization continuations are private-only via
    {!Github_user_auth_delivery}; this CLI never embeds URLs, device codes,
    callback errors, or account-control payloads.

    Minimal builds return disabled guidance via {!Github_account_cli_min}. *)

val principal_env_var : string
(** Environment variable that selects the current Principal. *)

val cmd_with_db : db:Sqlite3.db -> string list -> string
val cmd : string list -> string
