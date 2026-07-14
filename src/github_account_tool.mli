(** Narrow agent tool: redacted GitHub account status only (P21.M4.E1.T001).

    Returns redacted account + preference inspection for the current Principal.
    Never exposes vault tokens, sealed ciphertext, vault row ids, authorization
    URLs, device codes, or callback errors. Authorization continuations and
    lifecycle mutations are out of scope for agents; they happen through the CLI
    (or the configured Connector / app) and are delivered privately via
    {!Github_user_auth_delivery}.

    Use this tool when an agent needs to know "which GitHub accounts does the
    current Principal have and what is their authorization status?" without
    touching credentials. *)

val principal_env_var : string
(** Environment variable that selects the current Principal. *)

val tool : db:Sqlite3.db -> Tool.t
(** [github_account] tool: redacted status only. *)
