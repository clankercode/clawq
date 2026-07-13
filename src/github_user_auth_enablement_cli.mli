(** Full-build command bridge for admin GitHub user-auth enablement readiness
    and repair (P21.M4.E1.T002).

    Subcommands:
    - [status] — durable gate + readiness summary (redacted)
    - [readiness] — full readiness report
    - [repair] — actionable repair guidance for failing checks
    - [enable --reason R --audit-ref A] — plan production enable
    - [disable --reason R --audit-ref A] — plan production disable
    - [apply PLAN_ID DIGEST] — confirm + apply a stored plan
    - [plan show PLAN_ID] — show a stored plan

    Requires [CLAWQ_ADMIN=1] for mutating plan/apply paths and
    [CLAWQ_PRINCIPAL_ID] as the admin Principal. Evidence flags can be injected
    via environment variables for operator tooling (see module docs).

    Minimal builds return disabled guidance via
    {!Github_user_auth_enablement_cli_min}. *)

val admin_env_var : string
val principal_env_var : string
val cmd_with_db : db:Sqlite3.db -> string list -> string
val cmd : string list -> string
