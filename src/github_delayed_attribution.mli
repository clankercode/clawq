(** Preserve pinned attribution through delayed and background work
    (P21.M3.E3.T003).

    Jobs pin immutable [Actor_snapshot] evidence plus a frozen prior attribution
    [Allow] (logical Principal/account binding lineage, requested and resolved
    mode, confirmation, expected GitHub actor) — never a credential.

    At execution this module:

    - re-resolves live Principal / identity link / account authority from the
      immutable snapshot via {!Github_durable_job_actor_attribution}
    - revalidates the prior [Allow] against live evidence with
      {!Github_attribution_authorize} / continuity checks
    - permits ordinary token-refresh generation advance inside the same logical
      binding lineage (vault_generation is not CAS-pinned across the delay)
    - fails closed on identity, binding lineage, actor, confirmation, mode,
      SSO/repo/policy, or authority change — never switches identity
    - optionally issues an opaque user lease for HTTP after revalidation

    Surfaces: [Setup_plan] (plan pin), durable jobs / work items / delivery
    outbox (storage JSON). Patterns mirror {!Github_collab_attribution} /
    {!Github_pr_review_attribution} for Allow embedding and
    {!Github_durable_job_actor_attribution} for snapshot propagation.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module Auth = Github_attribution_authorize
module Lease = Github_attribution_dispatch_lease
module Audit = Github_attribution_audit
module Job = Github_durable_job_actor_attribution
module A = Actor_snapshot
module Policy = Github_attribution_policy
module V = Github_user_token_vault

val schema_version : int
(** Delayed-attribution pin schema; starts at 1. *)

val field_attribution_allow : string
(** JSON field name for the frozen prior Allow (plans / envelopes). *)

val field_expected_github_actor : string
val field_requested_mode : string
val field_resolved_mode : string
val field_used_app_fallback : string

(** {1 Prior Allow JSON (shared collab/pr shape)} *)

val allow_to_json : Auth.allow -> Yojson.Safe.t
(** Redacted prior Allow suitable for durable embedding. Never embeds tokens. *)

val allow_of_json : Yojson.Safe.t -> (Auth.allow, string) result
(** Parse a previously embedded prior Allow. *)

(** {1 Delayed attribution pin} *)

type pin = {
  job_id : string;
  snapshot : A.t;
      (** Immutable initiating Actor evidence; never reusable authority. *)
  allow : Auth.allow;
      (** Frozen prior Allow: mode, requirement, lineage, confirmation pins. *)
  expected_github_actor : Audit.github_actor;
      (** Expected GitHub actor named at preview (numeric user or App). *)
  confirmation_id : string option;
      (** Explicit action confirmation bound to this delayed work when required.
      *)
}
(** Full delayed-work attribution pin. Token-free by construction. *)

val make_pin :
  job_id:string ->
  snapshot:A.t ->
  allow:Auth.allow ->
  ?expected_github_actor:Audit.github_actor ->
  ?confirmation_id:string ->
  unit ->
  (pin, string) result
(** Build a pin. Rejects empty [job_id] and authority-claiming snapshots. *)

val pin_to_storage_json : pin -> (Yojson.Safe.t, string) result
(** Combined token-free envelope for durable storage. *)

val pin_of_storage_json : Yojson.Safe.t -> (pin, string) result
(** Parse a previously stored combined pin envelope. *)

val allow_storage_json_of_pin : pin -> (Yojson.Safe.t, string) result
(** Allow-only JSON for columns that already hold [actor_snapshot_json]
    separately. *)

val pin_of_parts :
  job_id:string ->
  snapshot_json:Yojson.Safe.t option ->
  allow_json:Yojson.Safe.t option ->
  ?expected_github_actor_json:Yojson.Safe.t option ->
  ?require_both:bool ->
  unit ->
  (pin option, string) result
(** Reassemble a pin from separate storage columns. When [require_both] is false
    (default), missing both sides yields [Ok None] (legacy rows). When one side
    is present without the other, returns [Error] (broken pin). *)

(** {1 Plan attach / extract} *)

val attach_pin_to_plan : plan:Setup_plan.t -> pin:pin -> unit -> Setup_plan.t
(** Embed snapshot + Allow + mode + expected actor into plan data/planned_state
    and recompute digest. Does not persist. *)

val attach_and_restamp :
  db:Sqlite3.db ->
  plan:Setup_plan.t ->
  pin:pin ->
  unit ->
  (Setup_plan.t, string) result
(** [attach_pin_to_plan] then replace the pending stored plan. *)

val pin_of_plan : Setup_plan.t -> (pin option, string) result
(** Load pin from plan. [Ok None] when neither snapshot nor Allow present. *)

val has_attribution_allow : Setup_plan.t -> bool
val allow_of_plan : Setup_plan.t -> (Auth.allow option, string) result

(** {1 Delayed revalidation (generation may advance in lineage)} *)

val pin_for_delayed_revalidate : Auth.allow -> Auth.revision_pin
(** Pins from a prior Allow with [vault_generation = None] so ordinary refresh
    may advance generation inside the same [binding_lineage_id]. All other
    lineage / confirmation / snapshot / policy pins remain enforced. *)

val request_with_delayed_pin :
  live:Auth.request -> prior:Auth.allow -> Auth.request
(** Replace [live.pin] with {!pin_for_delayed_revalidate}. *)

type exec_invalidation =
  | Snapshot of Job.exec_invalidation
  | Pin_missing of string
  | Pin_malformed of string
  | Authorization of Auth.deny
  | Continuity of Lease.denial
  | Lineage_break of string
  | Expected_actor_mismatch of string
  | Job_cancelled of string

val string_of_exec_invalidation : exec_invalidation -> string

type exec_envelope = {
  job_id : string;
  pin : pin;
  snapshot_env : Job.exec_envelope;
      (** Live re-resolved Actor authority from the immutable snapshot. *)
  fresh_allow : Auth.allow;
      (** Fresh Allow after delayed revalidation (may have advanced generation).
      *)
  generation_advanced : bool;
      (** True when live vault generation advanced past the original pin. *)
}
(** Secret-free execution envelope. Live credentials must still be leased
    separately (or via {!issue_for_delayed_dispatch}). *)

val prepare_execution :
  db:Sqlite3.db ->
  job_id:string ->
  pin:pin ->
  live:Auth.request ->
  ?claimed_actor:Principal_identity.connector_actor_key ->
  ?cancelled:bool ->
  ?require_expected_actor_match:bool ->
  unit ->
  (exec_envelope, exec_invalidation) result
(** Re-resolve snapshot authority, revalidate prior Allow with generation
    advance permitted inside lineage, enforce mode/principal/binding continuity.
    Fails closed on lineage / identity / authority breaks. *)

val prepare_execution_of_storage :
  db:Sqlite3.db ->
  job_id:string ->
  snapshot_json:Yojson.Safe.t option ->
  allow_json:Yojson.Safe.t option ->
  live:Auth.request ->
  ?expected_github_actor_json:Yojson.Safe.t option ->
  ?require_pin:bool ->
  ?claimed_actor:Principal_identity.connector_actor_key ->
  ?cancelled:bool ->
  unit ->
  (exec_envelope option, string) result
(** Storage-column path. When both sides absent and [require_pin] is false,
    returns [Ok None] (legacy unattributed jobs). *)

(** {1 Dispatch after delayed revalidation} *)

type issued = { envelope : exec_envelope; issued : Lease.issued }
(** Revalidated envelope plus opaque lease (User) or App path without lease. *)

val issue_for_delayed_dispatch :
  db:Sqlite3.db ->
  job_id:string ->
  pin:pin ->
  live:Auth.request ->
  ?vault_id:string ->
  ?expected:V.account_key ->
  ?claimed_actor:Principal_identity.connector_actor_key ->
  ?cancelled:bool ->
  ?now:float ->
  ?ttl_seconds:float ->
  unit ->
  (issued, string) result
(** {!prepare_execution} then issue an opaque lease. Generation race is checked
    against the {e fresh} (post-refresh) generation only, not the original pin,
    so ordinary refresh across the delay does not deny. *)

val revoke_issued_lease : Lease.issued -> unit
(** Best-effort revoke of a just-issued user lease. *)

(** {1 Conflicting pin rejection (write-once / first-wins)} *)

val reject_conflicting_pin :
  existing:pin -> offered:pin -> (unit, string) result
(** [Ok ()] when initiating snapshot lineage and Allow principal/binding/mode
    match; [Error] when an offered pin would borrow or replace identity. *)
