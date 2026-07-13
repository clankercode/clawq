(** Resolve attribution authorization after all current policy checks
    (P21.M3.E2.T003).

    Pure / injectable decision: intersect the frozen current-turn Room/session
    Tool catalog, repo grant, Principal/confirmation, logical binding lineage
    and state, App installation/permissions/repo selection, user/Org/SSO
    authority, and live action state into a typed [Allow] / [Deny].

    Records checked revisions and an actionable redacted repair reason. Issues
    {b no} token and {b no} lease — lease issuance is P21.M3.E2.T007 after a
    final revalidation immediately before HTTP dispatch.

    Stale pins (frozen revisions that no longer match live injected evidence)
    and ambiguous account resolution deny closed.

    Callers assemble evidence from {!Tool_catalog}, access/repo grants,
    {!Actor_snapshot}, {!Github_eligible_account_resolve},
    {!Github_app_installation_scope}, SSO/permission probes, and action-specific
    live state. This module never opens vault material, never issues leases, and
    never consults display names or recency.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module Policy = Github_attribution_policy

val schema_version : int
(** Authorize decision schema / export version; starts at 1. *)

(** {1 Resolved attribution mode on Allow} *)

type resolved_mode =
  | App
      (** Act as the GitHub App installation (App-first / Pat_compat App path).
      *)
  | User
      (** Act as the selected Principal-owned GitHub user (User_required). *)

val resolved_mode_to_string : resolved_mode -> string

(** {1 Checked revisions}

    Snapshot of every revision / lineage pin examined during authorization.
    Present on both [Allow] and [Deny] for audit, revalidation (T007), and
    repair. Contains no tokens. *)

type checked_revisions = {
  policy_action : string;
  requirement_attribution : string;
  requirement_tier : string;
  tool_catalog_revision : string option;
  access_revision : string option;
  principal_id : string option;
  principal_revision : int option;
  actor_revision : int option;
  identity_link_revision : int option;
  binding_id : string option;
  binding_lineage_id : string option;
  vault_generation : int option;
  installation_id : int option;
  installation_revision : string option;
  confirmation_id : string option;
  actor_snapshot_id : string option;
  live_state_revision : string option;
}

val empty_checked_revisions :
  policy_action:string ->
  requirement_attribution:string ->
  requirement_tier:string ->
  checked_revisions

val checked_revisions_to_json : checked_revisions -> Yojson.Safe.t
(** Redacted JSON; never includes tokens or secret material. *)

(** {1 Repair reason (Deny)} *)

type repair = {
  code : string;
      (** Stable machine code, e.g. ["account_ambiguous"],
          ["stale_vault_generation"], ["sso_required"]. *)
  message : string;
      (** Actionable, operator/user-facing text. Never embeds tokens, vault
          ciphertext, or raw credentials. *)
}

val repair_to_json : repair -> Yojson.Safe.t

(** {1 Decision} *)

type allow = {
  mode : resolved_mode;
  requirement : Policy.requirement;
  revisions : checked_revisions;
  binding_id : string option;
  principal_id : string option;
}
(** Authorization granted for [mode]. Still does not issue a token or lease. *)

type deny = {
  failed_check : string;
      (** Which intersection surface failed first (stable short name). *)
  repair : repair;
  requirement : Policy.requirement option;
      (** [None] only when action id itself is empty / unusable before policy
          lookup. *)
  revisions : checked_revisions;
}
(** Authorization denied. [repair] is actionable; [revisions] still records what
    was checked. *)

type decision = Allow of allow | Deny of deny

val is_allow : decision -> bool
val is_deny : decision -> bool

val decision_to_json : decision -> Yojson.Safe.t
(** Redacted diagnostic / audit JSON. Never issues tokens; never embeds secrets.
*)

val string_of_decision : decision -> string
(** One-line non-secret summary. *)

(** {1 Injectable evidence inputs}

    All fields are pure facts assembled by the caller. Missing, stale, or
    ambiguous inputs deny. *)

type tool_catalog_evidence = {
  revision : string;  (** Frozen catalog content revision. *)
  access_revision : string;  (** Effective-access revision bound at freeze. *)
  tool_authorized : bool;
      (** Requested GitHub tool resolves in the frozen catalog. *)
  room_id : string option;
  session_key : string option;
}
(** Frozen current-turn Room/session Tool catalog. *)

type repo_grant_evidence = {
  repo_full_name : string;
  granted : bool;  (** Repo appears in effective repo grants. *)
  blocked : bool;  (** Repo is on the blocked grant list (deny-wins). *)
  access_revision : string option;
}
(** Room / access-snapshot repository grant for the target repo. *)

type principal_confirmation_evidence = {
  principal_id : string;
  principal_revision : int;
  principal_current_active : bool;
      (** Live Principal is current active lineage (not tombstone/disabled). *)
  actor_revision : int option;
  identity_link_revision : int option;
  confirmation_id : string option;
  confirmation_required : bool;
      (** High-risk actions require explicit action confirmation (distinct from
          OAuth authorization). *)
  confirmation_satisfied : bool;
      (** Confirmation present, unexpired, and bound to this action/intent. *)
}
(** Principal lineage + action confirmation (not OAuth). *)

type selected_binding = {
  binding_id : string;
  lineage_id : string;
      (** Logical binding lineage; ordinary refresh may advance generation
          inside this lineage. *)
  authorized : bool;  (** Binding status is [Authorized]. *)
  vault_active : bool;  (** Vault row present and active (meta only). *)
  vault_generation : int;  (** Current vault generation (no token material). *)
  lineage_matches_pin : bool;
      (** When a snapshot/intent pin is present, live lineage matches it. *)
}
(** One currently-valid Principal-owned account selection (evidence only). *)

type binding_resolution =
  | Not_required
      (** App-installation / non-user path does not need a user binding. *)
  | None_eligible  (** No currently valid account for this Principal/context. *)
  | Ambiguous
      (** Multiple eligible accounts and no deterministic preference. *)
  | Selected of selected_binding

type binding_lineage_evidence = { resolution : binding_resolution }
(** Logical binding lineage and eligibility state from
    {!Github_eligible_account_resolve} (or equivalent pure projection). *)

type installation_evidence = {
  installation_id : int option;
  revision : string option;
      (** {!Github_app_installation_scope} content revision. *)
  active : bool;  (** Installation is Active (not suspended/deleted). *)
  repo_authorized : bool;
      (** Target repo is within installation selection / not revoked. *)
  permissions_ok : bool;
      (** Installation permissions cover the action family. *)
}
(** App installation status, permissions, and repository selection. *)

type user_org_sso_evidence = {
  user_authority_ok : bool;
      (** GitHub user still holds authority for the action (not removed /
          blocked). For App-only paths callers may set [true] when not
          applicable. *)
  org_policy_ok : bool;
      (** Org / repository policy permits the action for this actor. *)
  sso_ok : bool;
      (** SAML/SSO session and authorization remain valid when required. *)
}
(** Live user, organization, and SSO authority intersection. *)

type live_action_evidence = {
  ok : bool;
      (** Action-specific live state ok (head SHA, mergeability, branch
          protection, etc.). *)
  revision : string option;
      (** Optional live-state pin (e.g. planned head SHA). *)
  detail : string option;
      (** Redacted detail when [ok = false]; never secret material. *)
}
(** Action-family live state at authorization time. *)

type revision_pin = {
  tool_catalog_revision : string option;
  access_revision : string option;
  principal_revision : int option;
  binding_lineage_id : string option;
  vault_generation : int option;
  installation_revision : string option;
  confirmation_id : string option;
  actor_snapshot_id : string option;
  live_state_revision : string option;
}
(** Frozen expected revisions from preview / intent / delayed work. Any
    [Some expected] that differs from live evidence is stale and denies. [None]
    fields are not CAS-checked. *)

val empty_revision_pin : revision_pin

type request = {
  action : string;
      (** Canonical mutation id (see {!Github_attribution_policy.lookup}). *)
  tool_catalog : tool_catalog_evidence;
  repo_grant : repo_grant_evidence;
  principal : principal_confirmation_evidence;
  binding : binding_lineage_evidence;
  installation : installation_evidence;
  user_org_sso : user_org_sso_evidence;
  live_action : live_action_evidence;
  pin : revision_pin;
  actor_snapshot_id : string option;
      (** Immutable Actor snapshot id when pinned for this work. *)
}
(** Complete injectable authorization request. Pure; no I/O. *)

(** {1 Authorize} *)

val authorize : request -> decision
(** Intersect all current policy surfaces and return typed [Allow] or [Deny].

    Order (first failure wins, fail closed):
    + policy requirement for [action]
    + frozen Tool catalog authorization + revision pin
    + repo grant (grant required; blocked deny-wins) + access revision pin
    + Principal current active lineage + revision pin
    + action confirmation when required
    + binding lineage / eligibility (required for [User_required]; ambiguous and
      none-eligible always deny when a user binding is required)
    + vault active + generation pin + lineage pin
    + App installation active + repo selection + permissions + revision pin
    + user / Org / SSO authority (required for [User_required]; App path still
      checks org_policy_ok and sso_ok when those apply to installation scope)
    + live action state + revision pin
    + confirmation id pin and actor snapshot id pin

    Never returns tokens or leases. *)

(** {1 Test / caller helpers} *)

val make_selected_binding :
  binding_id:string ->
  lineage_id:string ->
  ?authorized:bool ->
  ?vault_active:bool ->
  ?vault_generation:int ->
  ?lineage_matches_pin:bool ->
  unit ->
  (selected_binding, string) result
(** Reject empty ids. Defaults: authorized, vault active, generation 1, lineage
    matches pin. *)
