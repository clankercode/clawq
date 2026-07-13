(** P19 → P21 attribution migration state matrix and staged rollout gates
    (P21.M3.E2.T006).

    Owns the versioned action state matrix that maps every P19 read, mutation,
    and background action from legacy App/PAT behavior to App_installation,
    User_preferred, or User_required — including preview-actor, fallback,
    delayed-work, receipt, and webhook semantics.

    Staged rollout is explicit and audited:
    - [Safe_default]: high-risk [User_required] off; P19 pilot gates off.
    - [P19_pilot]: named, time-bounded App pilot for [pilot_allowed] families.
    - [P21_production]: user attribution after Principal/vault/policy/private-
      delivery/repair/backout readiness pass.
    - [Rollback]: restore the safe disabled state without actor-mode
      substitution or confirmation weakening.
    - [Cleanup]: prove no residual pilot/production authority after disable.

    Pure decision layer: no I/O, tokens, or leases. Callers inject gate state
    and readiness evidence. Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/pilots/p21-attribution-migration-rollout.md. *)

module Policy = Github_attribution_policy

val matrix_version : int
(** Action state matrix schema version; starts at 1. Bump when matrix rows,
    stage semantics, or gate shapes change incompatibly. *)

val schema_version : int
(** Alias of [matrix_version] for parity with other attribution modules. *)

(** {1 Surface kinds} *)

type surface =
  | Read
      (** App-first reads / search / status; no user attribution required. *)
  | Mutation  (** Confirmed write / high-risk action. *)
  | Background
      (** Room-triggered delayed / background work that pins actor lineage. *)

val surface_to_string : surface -> string

(** {1 Legacy P19 execution path} *)

type legacy_path =
  | Legacy_app
      (** Production App installation path (reads, ambient automation). *)
  | Legacy_pat  (** Exact-repo PAT compatibility path. *)
  | Legacy_pilot_app
      (** App only under a named, time-bounded P19 pilot gate (off by default).
      *)
  | Legacy_denied
      (** Not available under P19 outside pilot / not implemented as App. *)

val legacy_path_to_string : legacy_path -> string

(** {1 Preview / fallback / delayed / receipt / webhook semantics} *)

type preview_rule =
  | Preview_not_required
      (** Pure App reads / App_installation primary path; no user preview. *)
  | Preview_names_actor
      (** Confirmation envelope must name the intended GitHub actor (User or App
          for visible fallback). *)
  | Preview_user_only
      (** [User_required]: preview must name the Principal-owned user. *)

val preview_rule_to_string : preview_rule -> string

type fallback_rule =
  | No_fallback
      (** Never App/PAT fallback (User_required, pure App primary, PAT). *)
  | Visible_app_fallback
      (** User_preferred: App only when policy permits and preview names App. *)

val fallback_rule_to_string : fallback_rule -> string

type delayed_rule =
  | No_delay_pin  (** Immediate path; no durable lineage pin. *)
  | Pin_actor_lineage
      (** Pin immutable Actor evidence + logical binding lineage; refresh may
          advance generation in-lineage only. *)

val delayed_rule_to_string : delayed_rule -> string

type receipt_rule =
  | Receipt_app_actor  (** Record App installation actor labels. *)
  | Receipt_resolved_mode
      (** Record requested/resolved mode, lineage, and redacted reason. *)
  | Receipt_pilot
      (** Record pilot_name + gate state + App actor (P19 interim). *)

val receipt_rule_to_string : receipt_rule -> string

type webhook_rule =
  | Webhook_ambient
      (** Ordinary ingress; no native user-attribution receipt match required.
      *)
  | Webhook_match_receipt
      (** Match resulting webhook to native attribution receipt exactly once. *)
  | Webhook_self_loop_guard
      (** Clawq-originated mutations must not re-notify the Room as external. *)

val webhook_rule_to_string : webhook_rule -> string

(** {1 Matrix row} *)

type matrix_row = {
  action : string;  (** Canonical action id (lowercase snake_case). *)
  surface : surface;
  legacy : legacy_path;
  target : Policy.attribution;
      (** P21 target attribution for the action family. *)
  tier : Policy.risk_tier;
  pilot_allowed : bool;
      (** When true, P19 may enable App under a named pilot until production. *)
  pilot_name : string option;
      (** Canonical P19 pilot gate name when [pilot_allowed]. *)
  preview : preview_rule;
  fallback : fallback_rule;
  delayed : delayed_rule;
  receipt : receipt_rule;
  webhook : webhook_rule;
  production_requires_user_gate : bool;
      (** When true, production path needs the P21 attribution gate enabled and
          readiness complete. Pure App reads set this false. *)
}
(** One versioned migration row. Upgrades must not silently change [target] or
    weaken [preview]/[fallback] for an existing action id. *)

val matrix : unit -> matrix_row list
(** Full built-in matrix for every known P19 read / mutation / background
    family. Order is stable for audit dumps. *)

val lookup : action:string -> matrix_row
(** Lookup by action id (case-insensitive, trimmed). Accepts aliases consistent
    with {!Github_attribution_policy.lookup}. Unknown actions fail closed as
    [User_required] / [Critical] with no pilot and production gate required. *)

val matrix_to_json : matrix_row list -> Yojson.Safe.t
(** Redacted matrix export (no secrets). Includes [matrix_version]. *)

val row_to_json : matrix_row -> Yojson.Safe.t

(** {1 Rollout stages} *)

type stage =
  | Safe_default
      (** Install / upgrade default: high-risk User_required disabled; all P19
          pilot gates off. Reads and pure App paths still work. *)
  | P19_pilot
      (** One or more named, time-bounded App pilot gates enabled. Not
          production user attribution. *)
  | P21_production
      (** User attribution production path after readiness + audited enable. *)
  | Rollback
      (** Transitioning back to safe disabled without actor-mode substitution.
      *)
  | Cleanup
      (** Post-disable residual-authority proof before declaring complete. *)

val stage_to_string : stage -> string
val stage_of_string : string -> (stage, string) result

val default_stage : stage
(** [Safe_default]. *)

val stages : unit -> stage list
(** Canonical ordered stage list for docs / enumeration. *)

(** {1 Gates} *)

type pilot_gate = {
  enabled : bool;
  pilot_name : string;
  expires_at : string option;  (** ISO-8601 UTC; required when enabling. *)
  audit_ref : string option;  (** Redacted operator receipt / plan id. *)
}
(** Named, time-bounded P19 App pilot. Safe default: [enabled=false]. *)

type production_gate = {
  enabled : bool;
      (** P21 user-attribution gate. When false, User_required / User_preferred
          cannot run (and never fall back to App/PAT). *)
  audit_ref : string option;
  enabled_at : string option;  (** ISO-8601 UTC when last enabled. *)
}
(** Global production enablement. Safe default: [enabled=false]. *)

type rollback_gate = {
  active : bool;
  reason : string;  (** Redacted operator reason. *)
  audit_ref : string option;
  restores_stage : stage;  (** Always [Safe_default] for a complete rollback. *)
}
(** Explicit rollback request. Does not substitute actor modes on in-flight
    work; drains or fails with reconfirmation. *)

type cleanup_gate = {
  active : bool;
  audit_ref : string option;
  residual_authority_cleared : bool;
      (** Operator-asserted: gates off, routes quiet, outbox idle, credentials
          destroyed as applicable. *)
  pilot_credentials_destroyed : bool;
  bindings_unlinked : bool;
}
(** Cleanup completion evidence. Cleanup succeeds only when residual authority
    is cleared. *)

val default_pilot_gate : pilot_name:string -> pilot_gate
(** [enabled=false], no expiry, no audit. *)

val default_production_gate : production_gate
(** [enabled=false]. *)

val default_rollback_gate : rollback_gate
(** Inactive; restores [Safe_default]. *)

val default_cleanup_gate : cleanup_gate
(** Inactive; residual flags false. *)

val default_pilot_gates : unit -> pilot_gate list
(** One default-off gate per matrix row with [pilot_allowed=true], using the
    row's canonical [pilot_name]. *)

val pilot_gate_active : now:float -> pilot_gate -> bool
(** [true] when enabled and not past [expires_at]. Missing/empty expiry while
    enabled is treated as inactive (fail closed — never open-ended). *)

val pilot_gate_to_json : pilot_gate -> Yojson.Safe.t
val production_gate_to_json : production_gate -> Yojson.Safe.t
val rollback_gate_to_json : rollback_gate -> Yojson.Safe.t
val cleanup_gate_to_json : cleanup_gate -> Yojson.Safe.t

(** {1 Production readiness} *)

type readiness = {
  principal_ready : bool;
      (** Verified Principals / bindings for the target Room/actors. *)
  vault_ready : bool;  (** Vault master-key + CRUD + CAS ready. *)
  policy_ready : bool;
      (** Attribution policy / tool catalog frozen correctly. *)
  private_delivery_ready : bool;
      (** Private authorization delivery supported for the Connector. *)
  repair_ready : bool;  (** Admin repair / diagnostics paths available. *)
  backout_ready : bool;
      (** Documented rollback + cleanup path verified for this deployment. *)
}
(** All flags must be true before production enable is accepted. *)

val readiness_complete : readiness -> bool

val empty_readiness : readiness
(** All false (safe). *)

val all_ready : readiness
(** All true (test / post-verification convenience). *)

val readiness_to_json : readiness -> Yojson.Safe.t

val readiness_missing : readiness -> string list
(** Stable list of incomplete readiness field names. *)

(** {1 Effective path resolution} *)

type effective_path =
  | Path_app_primary
      (** Pure App_installation / ambient App (not a user→App fallback). *)
  | Path_pat_compat  (** Legacy PAT exact-repo path. *)
  | Path_user  (** Principal-owned user attribution (production). *)
  | Path_visible_app_fallback
      (** User_preferred App only when preview names App (production). *)
  | Path_pilot_app  (** P19 named pilot App path (interim). *)
  | Path_denied of {
      code : string;
      message : string;
          (** Actionable redacted reason (repair / enable / wait). *)
    }

val effective_path_to_string : effective_path -> string
val effective_path_to_json : effective_path -> Yojson.Safe.t

type resolve_input = {
  action : string;
  stage : stage;
  production : production_gate;
  pilot_gates : pilot_gate list;
      (** Active pilot configuration; lookup by [pilot_name] for the row. *)
  readiness : readiness;
  now : float;  (** Unix time for pilot expiry checks. *)
  user_auth_available : bool;
      (** Whether user auth plumbing is configured at all (vs gate off). *)
}
(** Injectable facts for pure resolution. *)

val resolve : resolve_input -> effective_path
(** Resolve the effective attribution path for [action] under current stage and
    gates.

    Rules (summary):
    - Pure App_installation (reads): always [Path_app_primary].
    - [Safe_default] / [Rollback] / incomplete readiness: User_required and
      User_preferred denied when production gate off; pilot path only if a
      matching pilot gate is active for [pilot_allowed] rows.
    - [P19_pilot]: matching active pilot → [Path_pilot_app]; never pretends to
      be production User; no silent fallback when pilot off.
    - [P21_production]: requires production gate + readiness; User_required →
      [Path_user]; User_preferred → [Path_user] (visible App fallback is a
      separate authorize/fallback concern when preview names App).
    - [Cleanup]: same deny surface as safe default; residual authority must be
      cleared separately via {!cleanup_complete}.
    - Rollback never opens a pilot or production path and never substitutes
      actor mode. *)

val default_resolve_input :
  action:string ->
  ?stage:stage ->
  ?production:production_gate ->
  ?pilot_gates:pilot_gate list ->
  ?readiness:readiness ->
  ?now:float ->
  ?user_auth_available:bool ->
  unit ->
  resolve_input
(** Defaults: [Safe_default], production off, default-off pilot gates, empty
    readiness, [now=0.], [user_auth_available=false]. *)

(** {1 Staged transitions (audited gates)} *)

type gate_kind =
  | Gate_pilot_enable
  | Gate_pilot_disable
  | Gate_production_enable
  | Gate_production_disable
  | Gate_rollback
  | Gate_cleanup

val gate_kind_to_string : gate_kind -> string

type transition_request = {
  kind : gate_kind;
  from_stage : stage;
  pilot : pilot_gate option;
      (** Required for pilot enable/disable (name + expiry on enable). *)
  production : production_gate option;
  rollback : rollback_gate option;
  cleanup : cleanup_gate option;
  readiness : readiness;
  audit_ref : string option;
}
(** Operator transition intent. Pure validation only. *)

type transition_result = {
  to_stage : stage;
  production : production_gate;
  message : string;  (** Redacted operator summary. *)
}
(** Accepted transition outcome. Callers persist gates / stage. *)

val validate_transition :
  transition_request -> (transition_result, string) result
(** Accept only explicit audited transitions:

    - [Gate_pilot_enable]: from Safe_default or P19_pilot; requires
      [enabled=true], non-empty [pilot_name], non-empty [expires_at], audit_ref;
      does not enable production.
    - [Gate_pilot_disable]: returns Safe_default when no other pilots remain
      (caller tracks remaining); never enables production.
    - [Gate_production_enable]: readiness complete; production gate enabled with
      audit; pilot high-risk path is not reopened as App fallback.
    - [Gate_production_disable] / [Gate_rollback]: restore Safe_default; clear
      production enable; no actor-mode substitution.
    - [Gate_cleanup]: requires residual_authority_cleared and related flags;
      lands in Safe_default.

    Rejects transitions that would silently change actor or weaken confirmation
    (e.g. enabling production without readiness, open-ended pilot, rollback that
    re-enables pilot). *)

val cleanup_complete : cleanup_gate -> bool
(** [true] when cleanup is active and residual/pilot/binding flags are set. *)

val no_residual_authority :
  production:production_gate ->
  pilot_gates:pilot_gate list ->
  now:float ->
  cleanup:cleanup_gate ->
  bool
(** [true] when production off, no active pilots, and cleanup residual flags
    hold. Used as the post-cleanup proof predicate. *)

(** {1 Invariants helpers} *)

val matrix_covers_policy_defaults : unit -> (unit, string) result
(** Every {!Github_attribution_policy.defaults} action appears in the matrix
    with matching target attribution, tier, and pilot_allowed. *)

val user_required_disabled_by_default : unit -> bool
(** [true] when default production gate is off and default stage is
    [Safe_default] (upgrade-safe). *)

val transition_weakens_confirmation : from_stage:stage -> to_stage:stage -> bool
(** [true] for illegal silent weakenings (e.g. Cleanup→P21_production without
    gate validation). Used by tests; [validate_transition] is the authority. *)
