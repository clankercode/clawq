(** Attribution previews, receipts, repair states, and audit (P21.M3.E2.T005).

    Durable, write-once records for the attribution lifecycle:

    - [Preview] — pre-confirm envelope naming requested/resolved mode, Actor
      evidence, and expected GitHub actor
    - [Receipt] — post-dispatch / applied-action outcome with immutable
      initiating evidence (pairs with {!Github_action_reconcile})
    - [Repair_state] — actionable redacted repair for deny paths (SSO,
      permission, refresh, revocation, App scope, rollout gate, ambiguity,
      identity, …)
    - [Audit] — secret-free export of a decision / lifecycle step

    Every record freezes Actor evidence, Principal/account lineage, GitHub
    numeric user or App, requested/resolved mode, fallback reason,
    operation/item, confirmation/job, and result. Failure classes are distinct
    and redacted. Later Principal merge/split never rewrites historical actor
    evidence (INSERT-only for snapshot columns; no update path).

    Issues no token and no lease. Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module A = Actor_snapshot
module Auth = Github_attribution_authorize
module Fallback = Github_attribution_fallback
module Policy = Github_attribution_policy
module Reconcile = Github_action_reconcile

val schema_version : int
(** Audit record schema / export version; starts at 1. *)

(** {1 Record kind} *)

type record_kind =
  | Preview
      (** Pre-confirm attribution preview / confirmation envelope evidence. *)
  | Receipt
      (** Applied-action / dispatch receipt with frozen initiating evidence. *)
  | Repair_state
      (** Private/operator repair guidance after deny (not Room-exportable
          progress). *)
  | Audit  (** General redacted audit of a lifecycle step / decision. *)

val record_kind_to_string : record_kind -> string
val record_kind_of_string : string -> (record_kind, string) result

(** {1 Result} *)

type result_kind =
  | Allowed  (** Authorization / preview resolved allow. *)
  | Denied  (** Authorization / fallback denied. *)
  | Fallback_app
      (** [User_preferred] resolved via visible, policy-permitted App fallback.
      *)
  | Completed  (** Dispatch / action completed (receipt). *)
  | Failed  (** Dispatch / action failed after attempt. *)
  | Pending_repair  (** Waiting on operator/user repair. *)
  | Reconfirm  (** Prior confirmation invalid; re-preview required. *)

val result_kind_to_string : result_kind -> string
val result_kind_of_string : string -> (result_kind, string) result

(** {1 Distinct failure classes (redacted)}

    SSO, permission, refresh, revocation, App scope, rollout gate, ambiguity,
    and identity failures are first-class and never collapse into a single
    "error". Additional classes cover common authorize/fallback denials. *)

type failure_class =
  | Sso  (** SAML/SSO session or authorization required / lost. *)
  | Permission  (** Insufficient installation / Org / repo permissions. *)
  | Refresh
      (** Token generation / vault generation refresh race or stale pin. *)
  | Revocation
      (** Vault/binding/installation revoked, disabled, or authority lost. *)
  | App_scope
      (** Installation repo selection / App scope no longer covers the action.
      *)
  | Rollout_gate  (** Attribution / pilot / production rollout gate disabled. *)
  | Ambiguity  (** Multiple eligible accounts; no deterministic preference. *)
  | Identity
      (** Principal / actor / lineage / binding identity break or missing. *)
  | Policy  (** Attribution policy / tool catalog / repo grant. *)
  | Confirmation  (** Action confirmation missing or stale. *)
  | Live_state  (** Live action-family state failed. *)
  | Fallback  (** Visible fallback / mode-lock denial. *)
  | Other of string  (** Reserved; prefer a first-class class when possible. *)

val failure_class_to_string : failure_class -> string
val failure_class_of_string : string -> (failure_class, string) result

val classify_failure :
  ?failed_check:string -> ?code:string -> unit -> failure_class
(** Map authorize / fallback / lease denial codes to a distinct class.
    Precedence prefers explicit [code] matches, then [failed_check] families. *)

(** {1 GitHub actor evidence (numeric user or App)} *)

type github_actor =
  | Numeric_user of { host : string; app_id : int; github_user_id : int64 }
      (** Principal-owned GitHub numeric user under one App/host. *)
  | App of { installation_id : int option; app_id : int option }
      (** GitHub App installation path (primary or visible fallback). *)
  | Unspecified  (** Not yet known / not applicable. *)

val github_actor_to_string : github_actor -> string
val github_actor_to_json : github_actor -> Yojson.Safe.t
val github_actor_of_json : Yojson.Safe.t -> (github_actor, string) result

(** {1 Lineage pin (Principal / account)} *)

type lineage_pin = {
  principal_id : string option;
  principal_revision : int option;
  actor_identity_key : string option;
  actor_revision : int option;
  identity_link_revision : int option;
  account_lineage_id : string option;
  binding_id : string option;
}
(** Logical Principal/account lineage frozen on the record. Never rewritten
    after insert. *)

val empty_lineage_pin : lineage_pin
val lineage_pin_to_json : lineage_pin -> Yojson.Safe.t
val lineage_pin_of_snapshot : A.t -> lineage_pin

(** {1 Record} *)

type t = {
  id : string;
  kind : record_kind;
  schema_version : int;
  created_at : string;
  action : string;  (** Canonical mutation / operation id. *)
  item_key : string option;
  room_id : string option;
  confirmation_id : string option;
  job_id : string option;  (** Delayed job / durable work id when applicable. *)
  plan_id : string option;
  receipt_id : string option;
  requested_mode : string option;
  resolved_mode : string option;
  used_app_fallback : bool;
  fallback_reason : string option;
      (** Short non-secret rationale when fallback was used or denied. *)
  github_actor : github_actor;
  lineage : lineage_pin;
  actor_snapshot : A.t option;
      (** Immutable Actor evidence. Never reusable authority. *)
  actor_snapshot_id : string option;
  result : result_kind;
  failure_class : failure_class option;
  failure_code : string option;
  reason : string;  (** Actionable redacted operator/user text. *)
  revisions_json : string option;
      (** Optional secret-free checked-revisions JSON dump. *)
}
(** Write-once attribution lifecycle record. Construct via {!make} /
    {!record_*}; field updates after insert are not supported for evidence
    columns. *)

val generate_id : ?now:float -> ?kind:record_kind -> unit -> string
(** Opaque id: ["ghattr_<kind>_<ms>_<rand>"]. *)

val make :
  ?id:string ->
  ?now:float ->
  kind:record_kind ->
  action:string ->
  result:result_kind ->
  reason:string ->
  ?item_key:string ->
  ?room_id:string ->
  ?confirmation_id:string ->
  ?job_id:string ->
  ?plan_id:string ->
  ?receipt_id:string ->
  ?requested_mode:string ->
  ?resolved_mode:string ->
  ?used_app_fallback:bool ->
  ?fallback_reason:string ->
  ?github_actor:github_actor ->
  ?lineage:lineage_pin ->
  ?actor_snapshot:A.t ->
  ?actor_snapshot_id:string ->
  ?failure_class:failure_class ->
  ?failure_code:string ->
  ?revisions_json:string ->
  unit ->
  (t, string) result
(** Build a redacted in-memory record. Rejects empty [action]/[reason] and
    authority-claiming snapshots. Redacts secret-shaped text. Does not persist.
*)

val to_json : t -> Yojson.Safe.t
(** Redacted JSON export. Never embeds tokens. *)

val of_json : Yojson.Safe.t -> (t, string) result
(** Parse a previously written record. Rejects payloads with token-like keys in
    the actor snapshot. *)

val redacted_summary : t -> string
(** One-line non-secret summary. *)

val is_immutable_evidence : t -> bool
(** [true] when the record carries frozen actor evidence (snapshot present) and
    claims no authority. *)

(** {1 Persistence (write-once)} *)

val ensure_schema : Sqlite3.db -> unit
(** Table [github_attribution_audit]. Idempotent. Secret-free columns only. *)

val insert :
  db:Sqlite3.db -> record:t -> ?now:float -> unit -> (t, string) result
(** INSERT-only. Never updates actor_snapshot / lineage / mode columns on an
    existing id. Returns the stored (redacted) record. *)

val get_by_id : db:Sqlite3.db -> id:string -> t option

val list_by_kind :
  db:Sqlite3.db -> kind:record_kind -> ?limit:int -> unit -> t list

val list_by_action :
  db:Sqlite3.db -> action:string -> ?limit:int -> unit -> t list

val list_by_snapshot_id :
  db:Sqlite3.db -> actor_snapshot_id:string -> ?limit:int -> unit -> t list

val list_by_principal :
  db:Sqlite3.db -> principal_id:string -> ?limit:int -> unit -> t list

val list_by_failure_class :
  db:Sqlite3.db -> failure_class:failure_class -> ?limit:int -> unit -> t list

val count : db:Sqlite3.db -> ?kind:record_kind -> unit -> int

(** {1 Convenience recorders} *)

val record_preview :
  db:Sqlite3.db ->
  action:string ->
  reason:string ->
  result:result_kind ->
  ?item_key:string ->
  ?room_id:string ->
  ?confirmation_id:string ->
  ?job_id:string ->
  ?plan_id:string ->
  ?requested_mode:string ->
  ?resolved_mode:string ->
  ?used_app_fallback:bool ->
  ?fallback_reason:string ->
  ?github_actor:github_actor ->
  ?lineage:lineage_pin ->
  ?actor_snapshot:A.t ->
  ?actor_snapshot_id:string ->
  ?failure_class:failure_class ->
  ?failure_code:string ->
  ?revisions_json:string ->
  ?now:float ->
  unit ->
  (t, string) result

val record_receipt :
  db:Sqlite3.db ->
  action:string ->
  reason:string ->
  result:result_kind ->
  ?item_key:string ->
  ?room_id:string ->
  ?confirmation_id:string ->
  ?job_id:string ->
  ?plan_id:string ->
  ?receipt_id:string ->
  ?requested_mode:string ->
  ?resolved_mode:string ->
  ?used_app_fallback:bool ->
  ?fallback_reason:string ->
  ?github_actor:github_actor ->
  ?lineage:lineage_pin ->
  ?actor_snapshot:A.t ->
  ?actor_snapshot_id:string ->
  ?failure_class:failure_class ->
  ?failure_code:string ->
  ?revisions_json:string ->
  ?now:float ->
  unit ->
  (t, string) result

val record_repair :
  db:Sqlite3.db ->
  action:string ->
  reason:string ->
  failure_class:failure_class ->
  failure_code:string ->
  ?result:result_kind ->
  ?item_key:string ->
  ?room_id:string ->
  ?confirmation_id:string ->
  ?job_id:string ->
  ?plan_id:string ->
  ?requested_mode:string ->
  ?resolved_mode:string ->
  ?used_app_fallback:bool ->
  ?fallback_reason:string ->
  ?github_actor:github_actor ->
  ?lineage:lineage_pin ->
  ?actor_snapshot:A.t ->
  ?actor_snapshot_id:string ->
  ?revisions_json:string ->
  ?now:float ->
  unit ->
  (t, string) result
(** Defaults [result] to [Pending_repair] (or [Reconfirm] when
    [failure_class = Fallback] and code suggests reconfirmation). *)

val record_audit :
  db:Sqlite3.db ->
  action:string ->
  reason:string ->
  result:result_kind ->
  ?item_key:string ->
  ?room_id:string ->
  ?confirmation_id:string ->
  ?job_id:string ->
  ?plan_id:string ->
  ?receipt_id:string ->
  ?requested_mode:string ->
  ?resolved_mode:string ->
  ?used_app_fallback:bool ->
  ?fallback_reason:string ->
  ?github_actor:github_actor ->
  ?lineage:lineage_pin ->
  ?actor_snapshot:A.t ->
  ?actor_snapshot_id:string ->
  ?failure_class:failure_class ->
  ?failure_code:string ->
  ?revisions_json:string ->
  ?now:float ->
  unit ->
  (t, string) result

(** {1 Project from authorize / fallback / reconcile} *)

val lineage_of_checked_revisions : Auth.checked_revisions -> lineage_pin

val github_actor_of_revisions :
  Auth.checked_revisions ->
  ?binding_github_user_id:int64 ->
  unit ->
  github_actor
(** Prefer numeric user when binding user id is known; else App installation
    from revisions. *)

val of_authorize_decision :
  decision:Auth.decision ->
  ?kind:record_kind ->
  ?item_key:string ->
  ?room_id:string ->
  ?job_id:string ->
  ?plan_id:string ->
  ?receipt_id:string ->
  ?actor_snapshot:A.t ->
  ?github_user_id:int64 ->
  ?now:float ->
  unit ->
  (t, string) result
(** Project a pure authorize decision into an audit/preview/repair record
    (default kind: [Audit] on allow, [Repair_state] on deny). Does not persist.
*)

val of_fallback_decision :
  decision:Fallback.decision ->
  action:string ->
  ?kind:record_kind ->
  ?item_key:string ->
  ?room_id:string ->
  ?confirmation_id:string ->
  ?job_id:string ->
  ?requested_mode:string ->
  ?actor_snapshot:A.t ->
  ?lineage:lineage_pin ->
  ?now:float ->
  unit ->
  (t, string) result
(** Project a fallback mode-selection decision. Does not persist. *)

val of_correlation :
  correlation:Reconcile.correlation ->
  ?kind:record_kind ->
  ?result:result_kind ->
  ?reason:string ->
  ?job_id:string ->
  ?used_app_fallback:bool ->
  ?fallback_reason:string ->
  ?now:float ->
  unit ->
  (t, string) result
(** Project a reconcile correlation into a [Receipt] (default) audit record.
    Does not persist. *)

val record_authorize_decision :
  db:Sqlite3.db ->
  decision:Auth.decision ->
  ?kind:record_kind ->
  ?item_key:string ->
  ?room_id:string ->
  ?job_id:string ->
  ?plan_id:string ->
  ?receipt_id:string ->
  ?actor_snapshot:A.t ->
  ?github_user_id:int64 ->
  ?now:float ->
  unit ->
  (t, string) result
(** [of_authorize_decision] then [insert]. *)

val record_from_correlation :
  db:Sqlite3.db ->
  correlation:Reconcile.correlation ->
  ?kind:record_kind ->
  ?result:result_kind ->
  ?reason:string ->
  ?job_id:string ->
  ?used_app_fallback:bool ->
  ?fallback_reason:string ->
  ?now:float ->
  unit ->
  (t, string) result
(** [of_correlation] then [insert]. *)

(** {1 Immutability / redaction guards} *)

val rewrite_actor_evidence :
  db:Sqlite3.db -> id:string -> snapshot:A.t -> (unit, string) result
(** Always [Error]: historical actor evidence is immutable. Provided so callers
    and tests can assert there is no update path for merge/split rewrites. *)

val contains_token_material : t -> bool
(** Heuristic: true when reason/revisions/snapshot JSON look secret-shaped. *)

val denial_exposes_token : record:t -> plaintext:string -> bool
(** Test helper: true if [plaintext] appears in JSON or summary. *)
