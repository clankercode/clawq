(** Independently gated, fresh-confirmed PR merge with live policy checks
    (P19.M4.E2.T003 / P21.M3.E3.T009).

    Merge is a high-risk action, independently enabled per route via
    [capability_policy.allow_merge] (defaults false) and a separate named
    time-bounded pilot gate (off by default). It is never implied by write
    ([allow_reply]/[allow_label]/[allow_assign]) or review ([allow_review]).

    Before preview and immediately before execution, validate current head,
    draft, mergeability, required checks/reviews, branch policy, allowed method,
    actor mode, and authority. Changed prerequisites cause no attempt
    (fail-closed). Planning produces confirmable [Setup_plan] values only — no
    live GitHub mutation.

    P19 path: App-attributed under the named pilot (not production-ready). P21
    production path: when [user_auth_available] is true, capability + live
    policy authorize the plan; live execution requires [User_required]
    attribution authorize + a fresh current Principal user lease via
    [Github_merge_attribution]. App/PAT is never a silent fallback when user
    auth is unavailable.

    Canonical contracts: docs/plans/2026-07-12-github-item-room-routing.md and
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

type merge_method = Merge | Squash | Rebase

type actor_mode =
  | App  (** P19 pilot-only App attribution; not production-ready *)
  | User  (** P21 User_required path (production); not silently App-fallback *)

type pilot_gate = {
  enabled : bool;
  pilot_name : string;
  expires_at : string option;
      (** ISO-8601 UTC; [None] = no expiry while enabled *)
}

type live_policy = {
  head_sha : string;
  is_draft : bool;
  mergeable : bool;
  required_checks_ok : bool;
  required_reviews_ok : bool;
  branch_policy_ok : bool;
  allowed_methods : merge_method list;
  actor_mode : actor_mode;
  authority_ok : bool;
}
(** Live merge-policy snapshot observed at plan or apply time. Callers obtain
    this from GitHub / branch protection immediately before use; this module
    never fabricates mergeability. *)

type merge_request = {
  item_key : string;
  method_ : merge_method;
  head_sha : string;
      (** Exact head SHA displayed to the actor; must match
          [live_policy.head_sha]. *)
  commit_title : string option;
  commit_message : string option;
}

val default_pilot_gate : pilot_gate
(** Off-by-default independent merge pilot ([enabled = false]). *)

val merge_method_to_string : merge_method -> string
val merge_method_of_string : string -> (merge_method, string) result
val actor_mode_to_string : actor_mode -> string

val authorize_merge :
  route:Github_route_store.t option ->
  pilot:pilot_gate ->
  user_auth_available:bool ->
  req:merge_request ->
  policy:live_policy ->
  ?now:float ->
  unit ->
  (unit, string) result
(** Authorize a merge attempt:

    1. P19 pilot path: when the named pilot is enabled and unexpired, require
    [allow_merge] + live policy (App actor allowed under pilot). 2. P21
    production path: when [user_auth_available] is true (even if pilot is off),
    require [allow_merge] + live policy with [User] actor mode; execution still
    requires User_required attribution + user lease. 3. Otherwise deny (not
    available outside pilot; no App/PAT fallback). 4. Non-empty [req.head_sha]
    must equal [policy.head_sha]; not draft; mergeable; required
    checks/reviews/branch/method/authority ok.

    Fail-closed on any failure. Does not mutate GitHub state. *)

val revalidate_for_apply :
  planned_head_sha:string ->
  planned_method:merge_method ->
  current:live_policy ->
  (unit, string) result
(** Immediate pre-execution revalidation against a fresh [live_policy]. If head,
    draft, mergeability, checks, reviews, branch policy, method, or authority no
    longer hold, return [Error] so no merge is attempted. *)

val plan_merge :
  db:Sqlite3.db ->
  principal:Setup_plan.principal ->
  room_id:string ->
  pilot:pilot_gate ->
  user_auth_available:bool ->
  req:merge_request ->
  policy:live_policy ->
  base_revision:string ->
  ?route:Github_route_store.t ->
  ?now:float ->
  unit ->
  (Setup_plan.t, string) result
(** Confirmable plan for merge. Apply kind [Generic "github_merge"]. Payload is
    secret-free and includes [head_sha], merge method, and pilot name. No live
    GitHub mutation. *)

val apply_confirmed :
  db:Sqlite3.db ->
  plan_id:string ->
  digest:string ->
  principal:Setup_plan.principal ->
  current_base_revision:string ->
  ?current_policy:live_policy ->
  ?now:float ->
  unit ->
  (Setup_plan_apply.outcome, string) result
(** Confirm/apply a pending merge plan via [Setup_plan_apply].

    Rechecks plan id + digest, principal, expiry, destination room, and
    [current_base_revision]. When [current_policy] is supplied, revalidates live
    merge prerequisites against the planned head/method embedded in the plan;
    changed prerequisites reject with no merge attempt. Domain adapter records a
    receipt only (no live GitHub API in this task). *)

val is_merge_plan : Setup_plan.t -> bool
(** True when [apply_payload.kind] is [Generic "github_merge"]. *)

val receipt_safe_error : string -> string
(** Projection-safe error receipt text: redacts bearer tokens, GitHub PATs, and
    token/secret key=value shapes. *)
