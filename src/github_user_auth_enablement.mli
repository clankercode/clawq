(** Admin enablement readiness and repair for GitHub user authorization
    (P21.M4.E1.T002).

    Composes existing pure layers into an admin surface:

    - {!Github_user_auth_readiness} — App identity, OAuth client/callback,
      device flow, expiring tokens, master key, permissions, private
      continuation
    - {!Github_attribution_rollout} — production readiness flags, stage, and
      audited [Gate_production_enable] / [Gate_production_disable] transitions
    - {!Github_account_admin_surface} — repair path availability for subject
      Principals (presence only; this module never starts OAuth for a user)

    Contract:
    - Admin enables or disables the capability (production attribution gate).
    - Authenticated users authorize only themselves; admins never authorize on
      behalf of another Principal through this surface.
    - Room access / capability binding requires the applicable Room consent and
      is refused when consent is missing.
    - Diagnostics and plans are redacted: handles, hostnames, counts, booleans
      only — never tokens, secrets, device codes, or callback error bodies.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/pilots/p21-attribution-migration-rollout.md. *)

module Auth = Github_user_auth_readiness
module Rollout = Github_attribution_rollout

val schema_version : int
(** Surface / store schema version; starts at 1. *)

val default_plan_ttl_seconds : float
(** Default enablement plan TTL: 30 minutes. *)

val ensure_schema : Sqlite3.db -> unit
(** Ensure durable enablement gate + plan tables. *)

(** {1 Check level} *)

type level = Pass | Warn | Fail

val string_of_level : level -> string

type check = {
  name : string;
  level : level;
  category : string;
      (** Stable category: [app], [vault], [policy], [delivery], [webhook],
          [rollout], [repair], [identity], [room], [auth_config]. *)
  detail : string;
  repair : string;  (** Actionable operator guidance; empty when [Pass]. *)
}
(** Named readiness/repair check. [detail] and [repair] must never embed secret
    material. *)

val check_to_json : check -> Yojson.Safe.t

(** {1 Injectable evidence snapshot}

    Callers assemble this from App setup, vault readiness, policy freeze,
    private delivery probes, webhook config, and current rollout gate state.
    Deliberately not full [Runtime_config.t]. *)

type evidence = {
  user_auth : Auth.config_snapshot;
      (** Feeds {!Github_user_auth_readiness.evaluate}. *)
  webhook_secret_handle : string option;
      (** Opaque credential-store handle only (not plaintext). *)
  webhook_endpoint_ready : bool;
      (** Shared App webhook path reachable / registered. *)
  revocation_webhook_ready : bool;
      (** App authorization revocation webhook path ready. *)
  principal_ready : bool;
  vault_ready : bool;
  policy_ready : bool;
  private_delivery_ready : bool;
  repair_ready : bool;
      (** Admin diagnostics / account admin surface available. *)
  backout_ready : bool;
  account_admin_surface_ready : bool;
      (** Redacted account admin surface module available (repair path). *)
  stage : Rollout.stage;
  production : Rollout.production_gate;
  pilot_gates : Rollout.pilot_gate list;
  now : float;
  room_scoped : bool;
      (** When true, Room consent is required for capability binding. *)
  room_consent_present : bool;
      (** Operator/Room consent evidence for the target Room. *)
}
(** Pure evaluation input. *)

val empty_evidence : unit -> evidence
(** Safe defaults: empty user-auth snapshot, all readiness false, stage
    [Safe_default], production off. *)

val evidence_with_user_auth :
  Auth.config_snapshot ->
  ?webhook_secret_handle:string option ->
  ?webhook_endpoint_ready:bool ->
  ?revocation_webhook_ready:bool ->
  ?principal_ready:bool ->
  ?vault_ready:bool ->
  ?policy_ready:bool ->
  ?private_delivery_ready:bool ->
  ?repair_ready:bool ->
  ?backout_ready:bool ->
  ?account_admin_surface_ready:bool ->
  ?stage:Rollout.stage ->
  ?production:Rollout.production_gate ->
  ?pilot_gates:Rollout.pilot_gate list ->
  ?now:float ->
  ?room_scoped:bool ->
  ?room_consent_present:bool ->
  unit ->
  evidence

(** {1 Readiness report} *)

type readiness_report = {
  checks : check list;
  overall : level;
  can_enable_production : bool;
      (** True only when every required check is [Pass], user-auth
          [can_act_as_user], rollout readiness complete, and stage permits
          enable. *)
  can_disable_production : bool;
      (** True when production is currently enabled (disable is always
          plan-confirm-apply even if readiness is incomplete). *)
  user_auth : Auth.readiness;
  rollout_readiness : Rollout.readiness;
  stage : Rollout.stage;
  production_enabled : bool;
  missing : string list;  (** Stable failing check names. *)
  constraints : string list;
      (** Always-present operator constraints (self-authorize only, Room
          consent, no admin-for-user OAuth). *)
  notes : string list;
}

val assess : evidence -> readiness_report
(** Evaluate combined enablement readiness. Pure. *)

val overall : check list -> level

val format_readiness : readiness_report -> string
(** Human-readable report; never prints secret material. *)

val readiness_to_json : readiness_report -> Yojson.Safe.t
(** Redacted JSON export. *)

val repair_guidance : readiness_report -> string list
(** Ordered repair strings for non-Pass checks (empty when all Pass). *)

val format_repair : readiness_report -> string
(** Human-readable repair guidance. *)

(** {1 Capability constraints (always enforced)} *)

val capability_constraints : string list
(** Canonical constraint strings attached to every plan and report. *)

val refuse_authorize_for_other :
  admin_principal_id:string ->
  subject_principal_id:string ->
  (unit, string) result
(** [Error] when admin ≠ subject. Admins enable capability only; users authorize
    only themselves. *)

val require_room_consent :
  room_scoped:bool -> room_consent_present:bool -> (unit, string) result
(** [Error] when Room-scoped enablement lacks consent. *)

(** {1 Durable gate state} *)

type gate_state = {
  stage : Rollout.stage;
  production : Rollout.production_gate;
  revision : int;
  updated_at : string;
  last_admin_principal_id : string option;
  last_reason : string option;
  last_audit_ref : string option;
}

val default_gate_state : unit -> gate_state
(** Safe default: [Safe_default], production off, revision 0. *)

val load_gate : db:Sqlite3.db -> unit -> gate_state
(** Load singleton gate; creates default row when missing. *)

val gate_to_json : gate_state -> Yojson.Safe.t
val format_gate : gate_state -> string

val evidence_from_gate :
  gate:gate_state ->
  user_auth:Auth.config_snapshot ->
  ?webhook_secret_handle:string option ->
  ?webhook_endpoint_ready:bool ->
  ?revocation_webhook_ready:bool ->
  ?principal_ready:bool ->
  ?vault_ready:bool ->
  ?policy_ready:bool ->
  ?private_delivery_ready:bool ->
  ?repair_ready:bool ->
  ?backout_ready:bool ->
  ?account_admin_surface_ready:bool ->
  ?pilot_gates:Rollout.pilot_gate list ->
  ?now:float ->
  ?room_scoped:bool ->
  ?room_consent_present:bool ->
  unit ->
  evidence
(** Build evidence seeded from durable gate stage/production. *)

(** {1 Plan-confirm-apply enable / disable} *)

type enablement_kind = Enable_production | Disable_production

val string_of_enablement_kind : enablement_kind -> string
val enablement_kind_of_string : string -> (enablement_kind, string) result

type conflict = { code : string; summary : string }

type enablement_plan = {
  version : int;
  plan_id : string;
  kind : enablement_kind;
  admin_principal_id : string;
  reason : string;
  audit_ref : string;
  from_stage : Rollout.stage;
  expected_revision : int;
  expected_production_enabled : bool;
  can_apply : bool;
  hard_conflicts : conflict list;
  readiness_overall : string;
  missing_checks : string list;
  notes : string list;
  constraints : string list;
  digest : string;
  created_at : string;
  expires_at : string;
}
(** Revision-bound enablement plan. Never embeds secrets. *)

val plan_to_json : enablement_plan -> Yojson.Safe.t
val format_plan : enablement_plan -> string

val plan_enable :
  db:Sqlite3.db ->
  admin_principal_id:string ->
  reason:string ->
  audit_ref:string ->
  evidence:evidence ->
  ?plan_id:string ->
  ?ttl_seconds:float ->
  ?now:float ->
  unit ->
  (enablement_plan, string) result
(** Build + store a production-enable plan. Fails closed when readiness is
    incomplete, Room consent missing (when room-scoped), or stage forbids
    enable. Does not mutate the gate. *)

val plan_disable :
  db:Sqlite3.db ->
  admin_principal_id:string ->
  reason:string ->
  audit_ref:string ->
  evidence:evidence ->
  ?plan_id:string ->
  ?ttl_seconds:float ->
  ?now:float ->
  unit ->
  (enablement_plan, string) result
(** Build + store a production-disable plan (safe default). Allowed even when
    readiness is incomplete. *)

type apply_status =
  | Applied of {
      plan : enablement_plan;
      gate : gate_state;
      message : string;
      applied_at : string;
    }
  | Refused of { reason : string; conflicts : conflict list }
  | Stale_revision of string
  | Digest_mismatch of string
  | Expired of string
  | Not_found of string

val apply_plan :
  db:Sqlite3.db ->
  plan_id:string ->
  presented_digest:string ->
  evidence:evidence ->
  ?now:float ->
  unit ->
  apply_status
(** Confirm + apply by digest. Revalidates readiness for enable at apply time.
    Uses {!Github_attribution_rollout.validate_transition}. Advances gate
    revision under CAS. Never starts user OAuth. *)

val get_plan :
  db:Sqlite3.db -> plan_id:string -> unit -> (enablement_plan, string) result

val list_plans : db:Sqlite3.db -> ?limit:int -> unit -> enablement_plan list
(** Newest first. *)

(** {1 Redaction helpers} *)

val json_contains_plaintext : json:Yojson.Safe.t -> plaintext:string -> bool
(** Test helper: true if [plaintext] appears anywhere in the JSON tree. *)

val format_status : gate:gate_state -> readiness:readiness_report -> string
(** Combined gate + readiness status for CLI. *)
