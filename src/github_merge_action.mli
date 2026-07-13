(** Independently gated, fresh-confirmed PR merge with live policy checks
    (P19.M4.E2.T003).

    Merge is a high-risk App-attributed action, independently enabled per route
    via [capability_policy.allow_merge] (defaults false) and a separate named
    time-bounded pilot gate (off by default). It is never implied by write
    ([allow_reply]/[allow_label]/[allow_assign]) or review ([allow_review]).

    Before preview and immediately before execution, validate current head,
    draft, mergeability, required checks/reviews, branch policy, allowed method,
    actor mode, and authority. Changed prerequisites cause no attempt
    (fail-closed). Planning produces confirmable [Setup_plan] values only — no
    live GitHub mutation. Production enablement remains gated by the P21
    [User_required] attribution rollout; if user auth is unavailable outside the
    pilot there is no App/PAT fallback.

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md. *)

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

    1. Independent pilot gate must be enabled and unexpired (or, when pilot is
    off/expired, P21 user auth must be available — never App/PAT fallback). 2.
    Route must grant [capability_policy.allow_merge] (separate from write /
    review). 3. Non-empty [req.head_sha] must equal [policy.head_sha]. 4. Live
    policy: not draft; mergeable; required checks/reviews ok; branch policy ok;
    requested method in [allowed_methods]; authority ok; actor mode consistent
    with pilot vs user path.

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
