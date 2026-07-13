(** Room-agent pilot planning via shared [Setup_plan] (P20.M2.E1.T001).

    Adapts the existing pilot wizard desired state into a typed, read-only,
    redacted, revision-bound plan for room-agent profile binding. Agent and CLI
    share this single adapter — no separate planning paths.

    Planning produces values only: no config write, no database mutation, no
    Connector or Session side effects. Optional [db] is used only for read-only
    readiness probes (budget state, ledger schema). Confirm/apply and durable
    plan storage: [Room_agent_setup_apply] (P20.M2.E1.T002).

    Apply payload kind is [Setup_plan.Room_profile]. Payloads and free-form
    state JSON are secret-free (ids/handles only — never channel tokens).

    Canonical contract:
    docs/adr/0003-require-plan-confirm-apply-for-agent-setup.md and
    docs/plans/2026-07-12-github-item-room-routing.md. *)

open Setup_room_wizard_types

val plan :
  cfg:Runtime_config.t ->
  state:wizard_state ->
  principal:Setup_plan.principal ->
  ?db:Sqlite3.db ->
  ?base_revision:string ->
  ?now:float ->
  ?id:Setup_plan.plan_id ->
  unit ->
  Setup_plan.t
(** Build a [Setup_plan.t] for the pilot room-agent bind/update.

    - [base_revision] defaults to [Setup_plan.base_revision_of_config cfg].
    - Diff covers profile upsert, connector binding, access bundles, memory
      scope, and budget when requested by [state].
    - Readiness reuses pilot wizard checks (config + optional DB probes).
    - Result is already redacted; re-[Setup_plan.redact] is a no-op on digest
      when inputs were secret-free. *)

val default_cli_principal : Setup_plan.principal
(** Shared CLI principal for non-interactive [rooms wizard plan]. *)
