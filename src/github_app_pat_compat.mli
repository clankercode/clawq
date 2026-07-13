(** Preserve App/PAT and minimal-build compatibility when P21 user authorization
    is disabled or unconfigured (P21.M4.E1.T004).

    Pure composition of:

    - {!Github_auth_selection} — PAT remains exact-Repo only; Org requires App
    - {!Github_attribution_rollout} — [Path_app_primary] / [Path_pat_compat]
      stay open at safe-default; user-attributed work fails closed
    - {!Github_attribution_fallback} — gate-off never silently substitutes App
      or PAT for [User_required] / [User_preferred]
    - Minimal-build CLI stubs — [Github_account_cli_min] and
      [Github_user_auth_enablement_cli_min] refuse without integrations

    Migrations are additive: PAT config is retained until confirmed apply, and
    enablement schema uses [CREATE TABLE IF NOT EXISTS] only.

    Canonical:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/pilots/p21-attribution-migration-rollout.md. *)

module Auth = Github_auth_selection
module Policy = Github_attribution_policy
module Rollout = Github_attribution_rollout
module Fallback = Github_attribution_fallback

val schema_version : int
(** Compatibility surface version; starts at 1. *)

(** {1 User-auth context (disabled / unconfigured)} *)

type user_auth_context = {
  available : bool;
      (** Whether user-auth plumbing is configured at all (OAuth client, vault,
          etc.). Independent of the production gate. *)
  production_enabled : bool;
  stage : Rollout.stage;
  readiness : Rollout.readiness;
  pilot_gates : Rollout.pilot_gate list;
  now : float;
}
(** Injectable facts for pure resolution under a given install state. *)

val user_auth_off : unit -> user_auth_context
(** Safe default after P21 install: unavailable, production off, [Safe_default],
    empty readiness, default-off pilots. *)

val user_auth_unconfigured : unit -> user_auth_context
(** Alias for {!user_auth_off} — user auth not configured. *)

(** {1 Rollout path with user auth off / unconfigured} *)

val resolve_action :
  ?ctx:user_auth_context -> action:string -> unit -> Rollout.effective_path
(** Resolve the effective attribution path for [action]. Defaults to
    {!user_auth_off}. *)

val is_app_primary : Rollout.effective_path -> bool
val is_pat_compat : Rollout.effective_path -> bool

val is_denied_without_app_pat_fallback : Rollout.effective_path -> bool
(** [true] for [Path_denied] whose message/code asserts no silent App/PAT
    substitution (gate off / user auth unavailable). *)

val app_read_actions : unit -> string list
(** Matrix actions with target [App_installation] (reads / ambient App). *)

val pat_read_actions : unit -> string list
(** Matrix actions with target [Pat_compat]. *)

val user_attributed_actions : unit -> string list
(** Matrix actions with target [User_preferred] or [User_required]. *)

val policy_permitted_with_user_auth_off : action:string -> bool
(** [true] when the action remains open under user-auth off: target is
    [App_installation] or [Pat_compat]. User-attributed actions are [false]
    (fail closed; not silent App/PAT). *)

(** {1 Transport selection (PAT exact-Repo)} *)

val select_transport :
  auth:Auth.auth_snapshot ->
  ?installation:Github_app_installation_scope.t ->
  repo_full_name:string ->
  unit ->
  Auth.selection
(** Deterministic PAT vs App selection for an exact repo. *)

val select_org_transport :
  auth:Auth.auth_snapshot ->
  ?installation:Github_app_installation_scope.t ->
  org:string ->
  unit ->
  Auth.selection
(** Org routes require App; PAT cannot claim Org. *)

val pat_is_exact_repo_only : Auth.selection -> bool
(** [true] when the selection chose PAT for an exact repo (reasons
    [Pat_exact_repo] or [Pat_fallback_exact_repo]) or rejected Org with
    [Rejected_org_requires_app]. Never claims Org via PAT. *)

(** {1 Fallback with attribution gate disabled} *)

val fallback_with_gate_off :
  action:string ->
  ?requirement:Policy.requirement ->
  ?preview_actor:Fallback.preview_actor ->
  ?user_path_available:bool ->
  ?app_path_available:bool ->
  unit ->
  Fallback.decision
(** Gate disabled. User-attributed actions deny with
    [attribution_gate_disabled]; pure [App_installation] still allows App
    (primary, not fallback). Pass [requirement] for matrix read actions that are
    not in {!Policy.defaults} (unknown actions fail closed as [User_required]).
*)

(** {1 Additive migration} *)

val migration_is_additive :
  before:Auth.auth_snapshot ->
  after:Auth.auth_snapshot ->
  confirmed_apply:bool ->
  (unit, string) result
(** Wraps {!Github_auth_selection.migration_safe}: PAT retained until confirmed
    apply. *)

val schema_ddl_is_additive : ddl:string -> bool
(** [true] when [ddl] is create-if-not-exists style and contains no destructive
    [DROP TABLE] / [DROP INDEX]. *)

val enablement_schema_is_additive : unit -> bool
(** Enablement gate tables use [CREATE TABLE IF NOT EXISTS] only (additive). *)

(** {1 Minimal-build surfaces (no integration dependency)} *)

type min_surface = {
  command_prefix : string;  (** e.g. ["github account"]. *)
  stub_module : string;  (** e.g. ["Github_account_cli_min"]. *)
  disabled_message : string;  (** Full disabled guidance string. *)
}
(** Minimal-build CLI surface that refuses safely. *)

val min_build_surfaces : unit -> min_surface list
(** Account lifecycle + user-auth enablement stubs. Messages never embed tokens.
*)

val min_account_disabled_message : unit -> string
val min_user_auth_disabled_message : unit -> string

val min_surfaces_refuse_without_integrations : unit -> bool
(** Every min surface message mentions the minimal-build boundary and the full
    binary. *)

(** {1 Compatibility report (pure self-check)} *)

type check = { name : string; ok : bool; detail : string }

type report = {
  checks : check list;
  all_ok : bool;
  user_auth : user_auth_context;
}

val evaluate_compatibility : ?ctx:user_auth_context -> unit -> report
(** Pure invariant suite: App/PAT reads open, PAT exact-Repo, user-attributed
    denied without App/PAT fallback, migration additive, min stubs present. *)

val format_report : report -> string
(** Human-readable report; never prints secrets. *)

val report_to_json : report -> Yojson.Safe.t
(** Redacted JSON export. *)
