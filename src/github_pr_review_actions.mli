(** Policy-gated PR reviewer requests and high-risk review submission
    (P19.M4.E1.T004 / P21.M3.E3.T002).

    Reviewer requests follow ordinary metadata policy ([allow_review]) and P21
    [User_preferred] attribution (native user lease or explicitly previewed
    policy-permitted App fallback) via {!Github_pr_review_attribution}.

    PR review submission (comment / approve / request changes) is high-risk:
    - P21 production: [User_required] — current Principal user lease +
      confirmation only; App/PAT fallback is forbidden.
    - P19 interim: App-attributed only inside a named, time-bounded pilot gate
      (off by default). Not production-ready and never a silent substitute when
      user auth is unavailable.

    Planning produces confirmable [Setup_plan] values only — no live GitHub
    mutation. Attribution authorize, dispatch lease, and audit live in
    {!Github_pr_review_attribution}. GitHub rejection and projection failures
    should be surfaced via [receipt_safe_error] (projection-safe, secret-free).

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md and
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

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

    1. When the P19 pilot is enabled and unexpired: require route with
    [allow_review], non-empty [head_sha], and Approve actor_login rules (App
    interim path). 2. Else when [user_auth_available]: same input/route checks
    for the P21 [User_required] path (lease + attribution at
    {!Github_pr_review_attribution} dispatch). 3. Else deny: not available
    outside pilot and not production-ready; P21 user-auth absence never falls
    back to App/PAT.

    Self-approve against the PR author is enforced in
    {!Github_pr_review_attribution.revalidate_live}. Empty [actor_login] on
    Approve when provided as [Some ""] is denied here. *)

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
