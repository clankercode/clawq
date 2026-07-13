(** Visible App fallback and fail-closed user-required behavior
    (P21.M3.E2.T004).

    Pure decision layer that sits above policy lookup and beside
    {!Github_attribution_authorize}:

    - [User_preferred] may resolve to App {b only} when action policy permits
      App fallback ({!Github_attribution_policy.permits_app_fallback}) {b and}
      the current preview explicitly names the App actor.
    - [User_required], attribution-gate disabled, and post-confirm authority
      loss {b never} fall back to App or PAT; they return repair or
      reconfirmation.
    - Actor mode cannot change during retry: a locked mode from a prior attempt
      is revalidated, never switched (User ↛ App, App ↛ User).

    Issues no token and no lease. Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module Policy = Github_attribution_policy

val schema_version : int
(** Fallback decision schema / export version; starts at 1. *)

(** {1 Actor mode} *)

type actor_mode =
  | App
      (** GitHub App installation path (primary App_installation, or visible
          User_preferred fallback). *)
  | User  (** Principal-owned GitHub user path. *)

val actor_mode_to_string : actor_mode -> string
val actor_mode_of_string : string -> (actor_mode, string) result

(** {1 Preview actor}

    What the {b current} confirmation/preview envelope explicitly names. Silent
    or omitted App is not enough for fallback. *)

type preview_actor =
  | Names_user
      (** Preview names the Principal-owned user as the GitHub actor. *)
  | Names_app
      (** Preview explicitly names the App installation as the actor (visible
          fallback / App-first). *)
  | Names_pat
      (** Preview names a legacy PAT path. Never accepted as a silent fallback
          from a user path. *)

val preview_actor_to_string : preview_actor -> string
val preview_names_app : preview_actor -> bool

(** {1 Attempt phase / mode lock} *)

type phase =
  | First_attempt
      (** Initial authorize for a fresh preview; mode may be chosen under the
          fallback rules. *)
  | Retry of { locked_mode : actor_mode }
      (** Retry of the same work unit. [locked_mode] must be revalidated; it
          cannot change to another actor. *)
  | Post_confirm of { locked_mode : actor_mode }
      (** After action confirmation. Mode is locked; authority loss returns
          reconfirmation rather than App/PAT fallback. *)

val phase_to_string : phase -> string
val locked_mode : phase -> actor_mode option

(** {1 Decision} *)

type deny_kind =
  | Repair
      (** Operator/user must repair readiness (link account, enable gate,
          restore authority) before retrying. *)
  | Reconfirmation
      (** Prior confirmation is no longer valid; re-preview / re-confirm. *)

val deny_kind_to_string : deny_kind -> string

type allow = {
  mode : actor_mode;
  used_app_fallback : bool;
      (** [true] when [User_preferred] resolved to App via visible, policy-
          permitted fallback (not pure App_installation). *)
  requirement : Policy.requirement;
  reason : string;  (** Short non-secret rationale for audit. *)
}

type deny = {
  code : string;
      (** Stable machine code, e.g. ["user_required_no_fallback"],
          ["app_fallback_not_previewed"], ["attribution_gate_disabled"],
          ["post_confirm_authority_lost"], ["actor_mode_locked"]. *)
  message : string;  (** Actionable, redacted operator/user text. *)
  kind : deny_kind;
  requirement : Policy.requirement option;
  attempted_mode : actor_mode option;
      (** Mode that was locked or preferred before denial. *)
}

type decision = Allow of allow | Deny of deny

val is_allow : decision -> bool
val is_deny : decision -> bool
val decision_to_json : decision -> Yojson.Safe.t
val string_of_decision : decision -> string

(** {1 Injectable input} *)

type request = {
  action : string;
      (** Canonical mutation id; looked up via {!Policy.lookup} when
          [requirement] is [None]. *)
  requirement : Policy.requirement option;
      (** Optional pre-looked-up requirement (tests / authorize handoff). *)
  attribution_gate_enabled : bool;
      (** P21 user-attribution gate. When [false], user-attributed work cannot
          fall back to App/PAT (fail closed). Pure [App_installation] still
          resolves to App (not a fallback). *)
  preview_actor : preview_actor;
      (** Actor named by the {b current} preview / confirmation envelope. *)
  phase : phase;
  user_path_available : bool;
      (** Currently valid Principal-owned user path (eligible binding, vault,
          etc.). Callers project this from eligibility/authorize evidence. *)
  app_path_available : bool;
      (** App installation path currently usable (active install, repo,
          permissions). *)
  post_confirm_authority_lost : bool;
      (** User/Org/SSO/binding authority lost after confirmation. Never falls
          back to App/PAT; returns reconfirmation. *)
}
(** Pure injectable facts. No I/O, tokens, or leases. *)

val resolve : request -> decision
(** Apply visible-fallback and fail-closed rules.

    Order (first applicable wins):
    + empty / unusable action
    + post-confirm authority loss → reconfirmation (never App/PAT)
    + attribution-gate disabled on user-attributed requirements → repair (never
      App/PAT fallback); pure [App_installation] still allows App
    + locked mode on Retry / Post_confirm → revalidate same mode only
    + [User_required] → User only; never App/PAT
    + [User_preferred] → User when path available and preview names user; App
      only when policy permits and preview names App; never silent App
    + [App_installation] → App (primary, not fallback)
    + [Pat_compat] → App-compat primary path only when preview names PAT/App;
      never chosen as fallback from a user path

    Never returns tokens or leases. *)

(** {1 Helpers for authorize / callers} *)

val needs_user_binding : actor_mode -> bool
(** [true] for [User]; [false] for [App]. *)

val default_request :
  action:string ->
  ?requirement:Policy.requirement ->
  ?attribution_gate_enabled:bool ->
  ?preview_actor:preview_actor ->
  ?phase:phase ->
  ?user_path_available:bool ->
  ?app_path_available:bool ->
  ?post_confirm_authority_lost:bool ->
  unit ->
  request
(** Test / caller convenience. Defaults: gate enabled, preview names user, first
    attempt, user+app paths available, no post-confirm authority loss. *)
