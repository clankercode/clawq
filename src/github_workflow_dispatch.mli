(** Confirmed typed GitHub Actions [workflow_dispatch] (P19.M4.E2.T006 /
    P21.M3.E3.T007).

    High-risk action with two authorized paths:

    - P19 App pilot: named, time-bounded pilot gate (off by default). Outside
      that pilot, App attribution is denied and must not be presented as
      production-ready.
    - P21 [User_required] production: when [user_auth_available] is true, route
      \+ input checks authorize the plan; execution still requires
      {!Github_workflow_dispatch_attribution} (authorize + current Principal
      user lease + confirmation). If P21 user auth is disabled/unavailable there
      is no App/PAT fallback.

    Capability: route [capability_policy.extra] key ["workflow_dispatch"]
    (bool), analogous to first-class [allow_merge] — independent of write/review
    and defaults off when absent.

    Planning produces confirmable [Setup_plan] values only — no live GitHub
    mutation. Unknown inputs (when an allowed-input schema is supplied), empty
    workflow/ref identity, secret-shaped input keys/values, disabled pilot
    without user auth, and missing capability fail closed. GitHub rejection and
    projection failures should be surfaced via [receipt_safe_error]
    (projection-safe, secret-free).

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md and
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

type pilot_gate = {
  enabled : bool;
  pilot_name : string;
  expires_at : string option;
      (** ISO-8601 UTC; [None] = no expiry while enabled *)
}

type request = {
  repo_full_name : string;  (** [owner/repo] *)
  workflow_id : string;
      (** Numeric workflow id or workflow file name, e.g. ["ci.yml"] *)
  ref_ : string;  (** Git ref (branch / tag / SHA) to dispatch against *)
  inputs : (string * string) list;
      (** Typed workflow inputs; values are strings only (GitHub API shape). *)
  item_key : string option;  (** Optional item correlation for receipts *)
  allowed_input_names : string list option;
      (** When [Some names], input keys not in [names] fail closed (unknown
          inputs). [None] accepts any non-empty key that is not secret-shaped.
      *)
}

val capability_key : string
(** Extra capability policy key: ["workflow_dispatch"]. *)

val default_pilot_gate : pilot_gate
(** Off-by-default pilot gate ([enabled = false]). *)

val has_workflow_dispatch_capability :
  Github_route_store.capability_policy -> bool
(** True when [extra] contains [(capability_key, true)]. Absent or false →
    denied (defaults off, like [allow_merge]). *)

val authorize :
  route:Github_route_store.t option ->
  pilot:pilot_gate ->
  user_auth_available:bool ->
  req:request ->
  ?now:float ->
  unit ->
  (unit, string) result
(** High-risk workflow_dispatch authorization:

    1. If pilot is active (enabled and unexpired) or [user_auth_available] is
    true: proceed to capability + input checks. 2. Else deny (not available
    outside pilot; not production-ready). P21 user-auth absence never falls back
    to App/PAT for a production path. 3. Require a route with [extra] capability
    [workflow_dispatch=true]. 4. Require non-empty [repo_full_name] (owner/repo
    form), [workflow_id], and [ref_]. 5. Validate typed inputs: non-empty keys,
    string values (enforced by type), reject secret-shaped keys/values, and when
    [allowed_input_names] is set reject unknown keys. *)

val plan_dispatch :
  db:Sqlite3.db ->
  principal:Setup_plan.principal ->
  room_id:string ->
  pilot:pilot_gate ->
  user_auth_available:bool ->
  req:request ->
  base_revision:string ->
  ?route:Github_route_store.t ->
  ?now:float ->
  unit ->
  (Setup_plan.t, string) result
(** Confirmable plan for typed workflow_dispatch. Apply kind
    [Generic "github_workflow_dispatch"]. Payload includes repo, workflow_id,
    ref, secret-free inputs, and pilot name. No live GitHub mutation. *)

val receipt_safe_error : string -> string
(** Projection-safe error receipt text: redacts bearer tokens, GitHub PATs, and
    token/secret key=value shapes so GitHub rejection and projection failures
    never embed credentials. *)
