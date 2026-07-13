(** Policy-gated PR reviewer requests and high-risk review submission
    (P19.M4.E1.T004).

    Reviewer requests follow ordinary metadata policy ([allow_review]). PR
    review submission (comment / approve / request changes) is a high-risk
    App-attributed action available only inside a named, time-bounded pilot gate
    that is off by default. Outside that pilot it is denied and must not be
    presented as production-ready. Production availability waits for P21
    [User_required] attribution; if P21 user auth is disabled/unavailable there
    is no App/PAT fallback.

    Planning produces confirmable [Setup_plan] values only — no live GitHub
    mutation. GitHub rejection and projection failures should be surfaced via
    [receipt_safe_error] (projection-safe, secret-free).

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md. *)

type review_kind = Comment | Approve | Request_changes

type pilot_gate = {
  enabled : bool;
  pilot_name : string;
  expires_at : string option;
      (** ISO-8601 UTC; [None] = no expiry while enabled *)
}

type request_reviewers = {
  item_key : string;
  reviewers : string list;
  head_sha : string option;
}

type submit_review = {
  item_key : string;
  kind : review_kind;
  head_sha : string;  (** required exact head SHA displayed to the actor *)
  body : string option;
  actor_login : string option;
}

val default_pilot_gate : pilot_gate
(** Off-by-default pilot gate ([enabled = false]). *)

val review_kind_to_string : review_kind -> string
val review_kind_of_string : string -> (review_kind, string) result

val authorize_request_reviewers :
  route:Github_route_store.t option ->
  req:request_reviewers ->
  (unit, string) result
(** Ordinary metadata path: require a route with
    [capability_policy.allow_review]. Deny if route is missing or capability is
    false. Validates [item_key] and non-empty [reviewers]. *)

val authorize_submit_review :
  route:Github_route_store.t option ->
  pilot:pilot_gate ->
  user_auth_available:bool ->
  req:submit_review ->
  ?now:float ->
  unit ->
  (unit, string) result
(** High-risk review submission authorization:

    1. If pilot is not enabled or has expired → deny (not available outside
    pilot; not production-ready). P21 user-auth absence never falls back to
    App/PAT for a production path. 2. When pilot is on: require route with
    [allow_review]. 3. Require non-empty exact [head_sha]. 4. Optional
    self-approve guard: [Approve] with [actor_login] matching a reviewer-style
    self target is denied when login is non-empty and equals a reserved self
    marker is not used; instead, empty actor is allowed and callers may pass
    author login equality checks externally. Simple rule: if [Approve] and
    [actor_login] is [Some ""] after trim → deny.

    Document: P21 will require [User_required]; P19 pilot allows App-attributed
    submission only while the gate is enabled and unexpired. *)

val plan_request_reviewers :
  db:Sqlite3.db ->
  principal:Setup_plan.principal ->
  room_id:string ->
  req:request_reviewers ->
  base_revision:string ->
  ?route:Github_route_store.t ->
  ?now:float ->
  unit ->
  (Setup_plan.t, string) result
(** Confirmable plan for requesting reviewers. Apply kind
    [Generic "github_request_reviewers"]. No live GitHub mutation. *)

val plan_submit_review :
  db:Sqlite3.db ->
  principal:Setup_plan.principal ->
  room_id:string ->
  pilot:pilot_gate ->
  user_auth_available:bool ->
  req:submit_review ->
  base_revision:string ->
  ?route:Github_route_store.t ->
  ?now:float ->
  unit ->
  (Setup_plan.t, string) result
(** Confirmable plan for high-risk review submission. Apply kind
    [Generic "github_submit_review"]. Payload includes [head_sha], review
    [kind], and pilot name. No live GitHub mutation. *)

val receipt_safe_error : string -> string
(** Projection-safe error receipt text: redacts bearer tokens, GitHub PATs, and
    token/secret key=value shapes so GitHub rejection and projection failures
    never embed credentials. *)
