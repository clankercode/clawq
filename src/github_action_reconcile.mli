(** Reconcile GitHub action receipts with resulting webhooks without loops
    (P19.M4.E2.T004 / P21.M1.E3.T006).

    Every mutation, background action, workflow, code job, and merge records a
    secret-free correlation (Room, item, action, actor mode/evidence, plan /
    receipt / delivery / GitHub refs) together with an immutable initiating
    [Actor_snapshot], requested/resolved attribution, and revision pins.
    Snapshots are historical evidence only — never reusable authority.

    Verified webhooks that match an open correlation close that receipt exactly
    once, update the item projection, and never re-enqueue delivery work or emit
    duplicate visible output. Close preserves the frozen snapshot and
    attribution across later Principal merge/rename. Unrelated human events
    remain distinct ([Ignored_human_event]); a different Principal's identity
    cannot close another Principal's receipt.

    Canonical contracts: docs/plans/2026-07-12-github-item-room-routing.md and
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module A = Actor_snapshot

type correlation = {
  room_id : string;
  item_key : string option;
  action : string;
  plan_id : string option;
  receipt_id : string option;
  delivery_id : string option;
  github_ref : string option;
  actor_mode : string;
      (** Resolved attribution mode (app | pat | user | pilot). Primary field
          for P19 callers; equals [resolved_mode] when that is set. *)
  requested_mode : string option;
      (** Attribution mode requested at intent time (evidence only). *)
  resolved_mode : string option;
      (** Explicit resolved mode. When [None], [actor_mode] is the resolved
          value. *)
  actor_snapshot : A.t option;
      (** Immutable initiating Actor evidence. Never reusable authority. *)
  expected_github_login : string option;
      (** When set (user-attributed), only that login (or bot/app self-events /
          exact delivery_id) may close this correlation. Prevents unrelated
          human actions and other Principals from claiming the receipt. *)
}

type reconcile_result =
  | Closed of { correlation : correlation; first_time : bool }
  | No_matching_receipt
  | Already_closed
  | Ignored_human_event

val ensure_schema : Sqlite3.db -> unit
(** Table [github_action_correlations]. Idempotent. Secret-free columns only. *)

val empty_attribution_fields :
  actor_mode:string ->
  ?requested_mode:string ->
  ?resolved_mode:string ->
  ?actor_snapshot:A.t ->
  ?expected_github_login:string ->
  unit ->
  string * string option * string option * A.t option * string option
(** Convenience packer for the attribution tail of [correlation]. Returns
    [(actor_mode, requested_mode, resolved_mode, actor_snapshot,
     expected_github_login)]. *)

val make_correlation :
  room_id:string ->
  action:string ->
  actor_mode:string ->
  ?item_key:string ->
  ?plan_id:string ->
  ?receipt_id:string ->
  ?delivery_id:string ->
  ?github_ref:string ->
  ?requested_mode:string ->
  ?resolved_mode:string ->
  ?actor_snapshot:A.t ->
  ?expected_github_login:string ->
  unit ->
  correlation
(** Build a correlation. When [resolved_mode] is omitted, [actor_mode] is used.
    Rejects non-empty snapshots that claim authority (defense in depth). *)

val resolved_attribution : correlation -> string
(** [resolved_mode] if set, else [actor_mode]. *)

val requested_attribution : correlation -> string option

val snapshot_is_authority : correlation -> bool
(** Always [false]. Snapshots on receipts are never reusable authority. *)

val correlation_of_applied_plan :
  plan:Setup_plan.t ->
  receipt_id:string ->
  ?requested_mode:string ->
  ?resolved_mode:string ->
  ?actor_mode:string ->
  ?delivery_id:string ->
  ?github_ref:string ->
  ?expected_github_login:string ->
  unit ->
  (correlation, string) result
(** Project Room, item, action fingerprint, plan id, receipt, pinned
    [Actor_snapshot], and attribution modes from an applied plan. Fails when the
    plan has no destination room. Defaults [actor_mode]/[resolved_mode] from
    plan [attribution] when present, else ["app"]. Snapshot is extracted via
    [Github_action_actor_attribution] when pinned. *)

val record_correlation :
  db:Sqlite3.db ->
  correlation:correlation ->
  ?now:float ->
  unit ->
  (unit, string) result
(** Persist an open correlation for a confirmed GitHub action. Fields are
    redacted before storage (no bearer tokens, PATs, or secret-shaped
    key=value). Actor snapshots are stored as secret-free JSON with
    [authority=false]. *)

val record_from_applied_plan :
  db:Sqlite3.db ->
  plan:Setup_plan.t ->
  receipt_id:string ->
  ?requested_mode:string ->
  ?resolved_mode:string ->
  ?actor_mode:string ->
  ?delivery_id:string ->
  ?github_ref:string ->
  ?expected_github_login:string ->
  ?now:float ->
  unit ->
  (correlation, string) result
(** [correlation_of_applied_plan] then [record_correlation]. Returns the
    recorded correlation. *)

val get_by_receipt_id : db:Sqlite3.db -> receipt_id:string -> correlation option
(** Load the correlation for a receipt (open or closed). *)

val get_by_plan_id : db:Sqlite3.db -> plan_id:string -> correlation option

val reconcile_webhook :
  db:Sqlite3.db ->
  room_id:string ->
  envelope:Github_event_envelope.t ->
  ?now:float ->
  unit ->
  reconcile_result
(** Match [delivery_id] / [item_key] / action fingerprints to open correlations;
    close exactly once; update the projection via the room event journal; do not
    enqueue new outbox work for self-events. Closed correlation retains the
    initiating snapshot, requested/resolved attribution, action identity, and
    revisions unchanged.

    Matching isolation:
    - Bot/app self-events may close by fingerprint / delivery id.
    - Human events close only on exact [delivery_id] match or when
      [expected_github_login] equals the webhook actor login (native
      user-attribution). Unrelated human actions → [Ignored_human_event].
    - A second matching webhook after close → [Already_closed].
    - Bot/app events without a match → [No_matching_receipt]. *)
