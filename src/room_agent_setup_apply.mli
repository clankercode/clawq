(** Room-agent pilot confirm/apply and repair via shared setup framework
    (P20.M2.E1.T002).

    Builds on [Room_agent_setup_plan] (T001): store a revision-bound plan, then
    confirm/apply through [Setup_plan_apply] with consent-style authority
    ([Setup_plan_consent]) and setup-owned bundle linkage ([Setup_plan_bundle]).

    Domain mutation of config is supplied by an optional [config_apply] hook so
    callers can reuse [Setup_room_wizard.apply_plan] without a module cycle.
    When the hook is omitted, apply is receipt-only (plus managed-bundle attach
    when the plan names access bundles) — suitable when pure config mutation is
    too invasive for the call site (e.g. unit tests, agent dry-runs).

    Repair regenerates a stale or expired plan from current config + desired
    wizard state via the shared planner (no mutation of config).

    Canonical: docs/adr/0003-require-plan-confirm-apply-for-agent-setup.md. *)

open Setup_room_wizard_types

type apply_request = {
  plan_id : string;
  digest : string;
  principal : Setup_plan.principal;
  current_base_revision : string;
  destination_room : string option;
      (** Defaults to the plan destination room when omitted. Required for
          room-targeted plans. *)
  now : float;
  actor : Setup_plan_consent.actor;
      (** Consent-style authority (global-admin / room-admin / cross-room). *)
}

type apply_outcome =
  | Applied of {
      receipt_id : string;
      first_time : bool;
      config_mutated : bool;
          (** [true] when a [config_apply] hook ran on the first-time path. *)
      attached_bundles : string list;
          (** Bundle ids attached (or already setup-owned) for the destination.
          *)
    }
  | Rejected of { reason : string; message : string }

type config_apply =
  plan:Setup_plan.t -> receipt_id:string -> (unit, string) result
(** Optional config mutation after rechecks. Invoked only on first successful
    apply (not on idempotent retries). Typical production hook closes over
    [Setup_room_wizard.apply_plan ~db ~cfg ~state]. *)

val init_schemas : Sqlite3.db -> unit
(** Initialize setup plan, consent, and bundle tables. *)

val plan_and_store :
  db:Sqlite3.db ->
  cfg:Runtime_config.t ->
  state:wizard_state ->
  principal:Setup_plan.principal ->
  ?base_revision:string ->
  ?now:float ->
  ?id:Setup_plan.plan_id ->
  ?db_readiness:bool ->
  unit ->
  (Setup_plan.t, string) result
(** Plan via [Room_agent_setup_plan] and persist as pending.

    [db_readiness] (default [false]) passes [db] into readiness probes; leave
    false when ledger schemas may be missing. Does not mutate config. *)

val apply_confirmed :
  db:Sqlite3.db -> ?config_apply:config_apply -> apply_request -> apply_outcome
(** Confirm/apply a pending [Room_profile] plan.

    Order: load plan → resolve destination → [Setup_plan_apply.apply] with
    consent authority and domain adapter (bundle attach + optional config
    mutation). Retry-idempotent. *)

val repair_if_stale :
  db:Sqlite3.db ->
  cfg:Runtime_config.t ->
  state:wizard_state ->
  plan:Setup_plan.t ->
  current_base_revision:string ->
  ?now:float ->
  unit ->
  ([ `Current of Setup_plan.t | `Repaired of Setup_plan.t ], string) result
(** If [plan] is expired or its base revision differs from
    [current_base_revision], rebuild via [Room_agent_setup_plan.plan] (same
    desired state, fresh id/digest) and store the repaired plan. Otherwise
    returns [`Current plan]. Never mutates config. *)

val is_room_profile_plan : Setup_plan.t -> bool

val feature_id_for_profile : profile_id:string -> string
(** Managed feature id used for setup-owned bundle linkage. *)
