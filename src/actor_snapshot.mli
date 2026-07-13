(** Immutable Actor snapshots for intents and delayed work (P21.M1.E3.T001).

    An [Actor_snapshot] freezes attribution evidence at the moment an intent,
    confirmation, or delayed job is created:

    - Principal and logical lineage (Principal + optional account binding)
    - Identity-link revision at capture
    - Display evidence (presentation only; never identity)
    - Source Room / Session / message context
    - Optional account binding reference (opaque ids + lineage; never tokens)
    - Intent / confirmation / delayed-job references

    Snapshots are versioned and immutable. They contain no token, vault
    ciphertext, or reusable credential authority. Merge and display rename
    preserve the frozen evidence; live execution always re-resolves current
    Principal, identity link, account binding, and policy. Split, unlink, or
    revocation forces that re-resolution and typically breaks authority until
    the new Principal is independently bound and authorized.

    Distinct from {!Principal_merge.actor_snapshot}, which is a lightweight
    pre-merge/pre-unlink historical row. This module is the durable attribution
    record for intents and delayed work.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0005-separate-human-principals-from-room-sessions.md. *)

module P = Principal_identity
module B = Github_account_binding

val schema_version : int
(** Snapshot schema version; starts at 1. *)

(** {1 Source context (Room / Session / message — non-identity)} *)

type source_context = {
  room_id : string option;
  session_id : string option;
  message_id : string option;
}
(** Execution origin only. Never a Principal id, never an actor key, never
    authority. *)

val empty_source_context : source_context

(** {1 Work references} *)

type work_refs = {
  intent_id : string option;
      (** Action intent / preview correlation id (non-secret). *)
  confirmation_id : string option;
      (** Explicit confirmation id; not OAuth authorization. *)
  delayed_job_id : string option;  (** Durable job / outbox id when delayed. *)
}
(** References to initiating work. Empty fields are allowed for pure capture. *)

val empty_work_refs : work_refs

(** {1 Optional account binding evidence (no credentials)} *)

type account_binding_evidence = {
  binding_id : string;
  lineage_id : string;
      (** Logical binding lineage pinned at capture; ordinary refresh may
          advance token generation within this lineage, but unlink/revoke/relink
          starts a new lineage and breaks authority. *)
  identity : B.account_identity;
      (** Immutable host + App + numeric user at capture. *)
  authorization_status : B.authorization_status;
      (** Status observed at capture (evidence only). *)
}
(** Account binding snapshot fragment. Deliberately omits [vault_ref] and any
    token material. *)

val make_account_binding_evidence :
  binding_id:string ->
  lineage_id:string ->
  identity:B.account_identity ->
  ?authorization_status:B.authorization_status ->
  unit ->
  (account_binding_evidence, string) result
(** Rejects empty [binding_id] / [lineage_id]. Defaults status to [Authorized].
*)

val account_binding_evidence_of_binding : B.binding -> account_binding_evidence
(** Project a live binding into token-free evidence (drops [vault_ref]). *)

(** {1 Logical lineage pin} *)

type logical_lineage = {
  principal_id : P.principal_id;
      (** Principal owning the actor when the snapshot was taken. *)
  principal_revision : int;
      (** Principal revision at capture (CAS evidence; not live authority). *)
  actor_key : P.connector_actor_key;
  actor_revision : int;
  identity_link_id : string option;
  identity_link_revision : int;
      (** Active identity-link revision at capture. Split/unlink advances or
          supersedes the live link; re-resolution detects the break. *)
  account_lineage_id : string option;
      (** Optional account [lineage_id] pin when a binding was selected. *)
}
(** Logical Principal / actor / link / account lineage frozen for delayed work.
    Live authority is never taken from this pin alone. *)

(** {1 Snapshot} *)

type t = {
  version : int;
  id : string;
  lineage : logical_lineage;
  display : P.display_metadata;
      (** Display evidence frozen at capture. Renames after capture do not
          rewrite the snapshot and do not change identity. *)
  source : source_context;
  account_binding : account_binding_evidence option;
  work_refs : work_refs;
  reason : string;
      (** Capture reason, e.g. ["intent_create"], ["confirmation"],
          ["delayed_job"]. *)
  captured_at : string;  (** ISO-8601 UTC. *)
}
(** Versioned immutable Actor snapshot. Construct via {!create} only in
    production paths; field updates after create are not supported (records are
    treated as write-once). *)

val generate_id : ?now:float -> unit -> string
(** Opaque snapshot id: ["actorsnap_<ms>_<rand>"]. *)

val create :
  ?id:string ->
  ?now:float ->
  ?reason:string ->
  principal_id:P.principal_id ->
  ?principal_revision:int ->
  actor_key:P.connector_actor_key ->
  ?actor_revision:int ->
  ?identity_link_id:string ->
  ?identity_link_revision:int ->
  ?display:P.display_metadata ->
  ?source:source_context ->
  ?account_binding:account_binding_evidence ->
  ?work_refs:work_refs ->
  ?captured_at:string ->
  unit ->
  (t, string) result
(** Build an immutable snapshot. Fills [id] and [captured_at] when omitted.
    Rejects non-positive revisions. [account_lineage_id] is taken from
    [account_binding] when present. Never accepts tokens or vault material. *)

val create_from_live :
  db:Sqlite3.db ->
  ?id:string ->
  ?now:float ->
  ?reason:string ->
  actor_key:P.connector_actor_key ->
  ?account_binding_id:string ->
  ?source:source_context ->
  ?work_refs:work_refs ->
  ?display:P.display_metadata ->
  unit ->
  (t, string) result
(** Capture from current store state: active identity link (preferred) or
    connector actor ownership, optional binding by id. Fails closed when the
    actor is missing or disabled. Display defaults to the live actor display. *)

(** {1 Immutability / authority invariants} *)

val is_authority : t -> bool
(** Always [false]. Snapshots are never reusable authority. *)

val contains_token_material : Yojson.Safe.t -> bool
(** Heuristic: true when JSON object keys look like secrets (token, secret,
    password, refresh, bearer, authorization header material, vault ciphertext,
    etc.). Used by redaction guards and tests. *)

(** {1 JSON (redacted by construction)} *)

val to_json : t -> Yojson.Safe.t
(** Full evidence JSON. Never includes tokens or vault refs. *)

val of_json : Yojson.Safe.t -> (t, string) result
(** Parse a previously written snapshot. Rejects payloads that carry token-like
    keys anywhere in the tree. *)

val to_redacted_json : t -> Yojson.Safe.t
(** Explicit redacted export for logs/audit. Same core fields as {!to_json};
    additionally strips email from display evidence and marks [authority=false].
*)

val redacted_summary : t -> string
(** One-line non-secret summary for audit / receipts. *)

(** {1 Re-resolve current authority}

    After merge, rename, split, or revocation, delayed work must re-resolve live
    Principal / link / binding state. The snapshot is never reused as authority.
*)

type authority_break =
  | Actor_missing
  | Actor_disabled
  | Actor_unlinked
  | Identity_link_missing
  | Identity_link_inactive of { status : P.identity_link_status }
  | Identity_link_revision_changed of { expected : int; actual : int }
  | Principal_missing
  | Principal_disabled
  | Principal_changed of {
      snapshot_principal : P.principal_id;
      live_principal : P.principal_id;
    }
      (** Live owner differs from snapshot and is not explained by a
          [Merged_into] alias of the snapshot Principal. Typical after split. *)
  | Account_binding_missing
  | Account_lineage_changed of { expected : string; actual : string }
  | Account_not_authorized of { status : B.authorization_status }
  | Account_owner_mismatch of { owner : P.principal_id }

type current_authority = {
  live_principal_id : P.principal_id option;
      (** Current Principal after following [Merged_into] aliases from the live
          actor link (when resolvable). *)
  live_principal_revision : int option;
  live_actor_revision : int option;
  live_identity_link_id : string option;
  live_identity_link_revision : int option;
  live_account_binding : B.binding option;
      (** Live binding looked up by snapshot [binding_id] when present; never
          returns token plaintext (vault_ref remains opaque only). *)
  followed_merge_alias : bool;
      (** True when snapshot Principal was a merge tombstone (or intermediate)
          and live ownership resolves through the survivor. Evidence is
          preserved; credentials still require live binding eligibility. *)
  breaks : authority_break list;
      (** Empty when attribution and (if pinned) account authority remain usable
          under current state. *)
  usable : bool;
      (** [true] only when [breaks] is empty and a live Active Principal owns
          the actor (via active link), and any pinned account binding is still
          Authorized under that Principal with the same lineage. *)
}
(** Result of re-resolving live authority from a historical snapshot. *)

val string_of_authority_break : authority_break -> string

val re_resolve_current_authority :
  db:Sqlite3.db -> t -> (current_authority, string) result
(** Re-resolve live actor → identity link → Principal (following merge
    tombstones) and optional account binding. Merge preserves usable attribution
    when the actor was adopted onto the survivor and any pinned binding was
    adopted with the same [lineage_id]. Split/unlink moves the actor to a new
    Principal and surfaces [Principal_changed] / link breaks. Revoked or
    unlinked bindings surface account breaks. Never elevates the snapshot into
    reusable authority. *)
