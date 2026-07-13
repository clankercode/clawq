(** Verified ownership and duplicate-account policy for GitHub account bindings
    (P21.M1.E2.T002).

    Attach requires a verified, unexpired identity assertion bound to the
    initiating Principal's current active lineage. Duplicate App/numeric-user
    ownership is refused by default. An audited admin exception is the only
    override path and never creates two live owners. Merge and split ownership
    conflicts refuse safely; CAS races fail closed (no silent authority move or
    unauthorized duplicate).

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0006-use-principal-owned-github-user-tokens.md. *)

module P = Principal_identity
module B = Github_account_binding

val schema_version : int
(** Policy schema version; starts at 1. *)

val default_assertion_ttl_seconds : float
(** Default identity-assertion TTL: 15 minutes. *)

(** {1 Verified identity assertion}

    Evidence that a trusted authorization path verified the GitHub numeric user
    for a specific App under a live Principal lineage. Assertions are not
    credentials: they never carry tokens. *)

type identity_assertion = {
  version : int;
  principal_id : P.principal_id;
      (** Initiating Principal that may own the binding after attach. *)
  principal_revision : int;
      (** CAS-bound Principal revision at assertion construction. *)
  identity : B.account_identity;
      (** Verified host + App + numeric GitHub user (immutable identity). *)
  verified_at : string;  (** ISO-8601 UTC when GitHub identity was verified. *)
  expires_at : string;  (** ISO-8601 UTC; attach must run before this. *)
  source_auth_tx_id : string option;
      (** Optional authorization transaction correlation id (non-secret). *)
  initiating_actor_key : P.connector_actor_key option;
      (** Optional verified Connector actor that initiated authorization. *)
}

val make_identity_assertion :
  principal_id:P.principal_id ->
  ?principal_revision:int ->
  identity:B.account_identity ->
  verified_at:string ->
  ?expires_at:string ->
  ?ttl_seconds:float ->
  ?source_auth_tx_id:string ->
  ?initiating_actor_key:P.connector_actor_key ->
  ?now:float ->
  unit ->
  (identity_assertion, string) result
(** Build an assertion. Requires non-empty [verified_at]. [expires_at] must be
    strictly after [verified_at] (or [now] when computing from TTL). Defaults:
    [principal_revision = 1], TTL [default_assertion_ttl_seconds]. *)

val assertion_is_unexpired : ?now:float -> identity_assertion -> bool
(** Lexicographic ISO-8601 compare of [now] against [expires_at]. *)

val validate_assertion :
  ?now:float -> identity_assertion -> (unit, string) result
(** Structural validation: version, non-empty verified_at, unexpired, positive
    principal_revision. Does not consult the store. *)

(** {1 Current Principal lineage} *)

type principal_lineage =
  | Current_active of { revision : int }
      (** Active Principal; may own bindings. *)
  | Tombstone of { merged_into : P.principal_id }
      (** Merged_into alias — cannot own new credentials. *)
  | Disabled of { summary : string }
  | Missing of { summary : string }
  | Stale_revision of { expected : int; actual : int }

val resolve_principal_lineage :
  db:Sqlite3.db ->
  principal_id:P.principal_id ->
  ?expected_revision:int ->
  unit ->
  (principal_lineage, string) result
(** Resolve whether [principal_id] is the current active lineage, optionally
    CAS-checking [expected_revision]. *)

(** {1 Attach denials} *)

type attach_denial =
  | Assertion_invalid of string
  | Assertion_expired of { expires_at : string }
  | Principal_not_current of string
  | Principal_revision_conflict of { expected : int; actual : int }
  | Duplicate_ownership of {
      existing_binding_id : string;
      owner_principal_id : P.principal_id;
      identity_key : string;
    }  (** Default policy: App+numeric user already owned. *)
  | Race of string
      (** CAS / concurrent uniqueness race; fail closed, no silent move. *)
  | Other of string

val string_of_attach_denial : attach_denial -> string

(** {1 Audited admin exception}

    The only path that may reassign an already-owned App+numeric user identity
    onto another Principal. Never creates two live owners. *)

type admin_exception = {
  admin_principal_id : P.principal_id;
  reason : string;
      (** Non-empty operator reason recorded in the redacted audit event. *)
  allow_reassign : bool;
      (** When [true], adopt an existing binding from another Principal under
          snapshot+CAS. When [false], admin path still cannot override
          duplicates. *)
}

val make_admin_exception :
  admin_principal_id:P.principal_id ->
  reason:string ->
  ?allow_reassign:bool ->
  unit ->
  (admin_exception, string) result
(** Require non-empty trimmed [reason]. [allow_reassign] defaults to [true]. *)

(** {1 Redacted audit} *)

type audit_kind =
  | Attach_succeeded
  | Attach_idempotent
  | Attach_refused
  | Admin_exception_attach
  | Admin_exception_reassign
  | Merge_conflict_refused
  | Split_conflict_refused
  | Race_refused

val string_of_audit_kind : audit_kind -> string

type redacted_audit = {
  id : string;
  kind : audit_kind;
  principal_id : string;
  identity_key : string;
  admin_principal_id : string option;
  binding_id : string option;
  reason : string option;
  timestamp : string;
  details : Yojson.Safe.t;
}
(** Never carries tokens, vault ciphertext, or OAuth secrets. *)

val redacted_audit_to_json : redacted_audit -> Yojson.Safe.t

(** {1 Attach (policy-gated)} *)

type attach_outcome =
  | Attached of {
      binding : B.binding;
      audit : redacted_audit;
      reassigned_from : P.principal_id option;
          (** [Some prior] when admin reassign moved ownership. *)
    }
  | Refused of { denial : attach_denial; audit : redacted_audit }

val attach_account :
  db:Sqlite3.db ->
  assertion:identity_assertion ->
  ?admin:admin_exception ->
  ?display:B.display ->
  ?authorization_status:B.authorization_status ->
  ?vault_ref:B.vault_ref ->
  ?id:string ->
  ?lineage_id:string ->
  ?now:float ->
  ?audit_sink:(redacted_audit -> unit) ->
  unit ->
  attach_outcome
(** Single IMMEDIATE transaction:

    1. Validate assertion (verified + unexpired). 2. Require initiating
    Principal is [Active] current lineage and matches [principal_revision]
    (CAS). 3. Look up existing binding for the App+numeric user identity:
    - Absent → insert under the asserting Principal.
    - Same Principal → idempotent success (no silent lineage rewrite).
    - Other Principal → refuse [Duplicate_ownership] unless [admin] with
      [allow_reassign], which snapshots then adopts under CAS. 4. Uniqueness /
      revision races surface as [Race] / [Duplicate_ownership] and roll back
      (fail closed).

    Emits a redacted audit event on every path. *)

(** {1 Merge ownership conflicts}

    Distinct exclusive host/App slots with different numeric users refuse merge.
    Identical identities coalesce (no credential copy). *)

type merge_conflict = {
  uniqueness_domain : string;
  summary : string;
  survivor_binding_id : string;
  loser_binding_id : string;
}

type merge_ownership_decision =
  | Merge_ok of {
      coalesce_binding_ids : string list;
          (** Loser rows whose identity already exists on the survivor. *)
      reassign_binding_ids : string list;
          (** Loser rows that would move to the survivor. *)
    }
  | Merge_refuse of { conflicts : merge_conflict list; audit : redacted_audit }

val evaluate_merge_ownership :
  db:Sqlite3.db ->
  from_principal:P.principal_id ->
  to_principal:P.principal_id ->
  ?now:float ->
  ?audit_sink:(redacted_audit -> unit) ->
  unit ->
  (merge_ownership_decision, string) result
(** Pure decision from current binding rows. Does not mutate. Same rule used by
    {!Github_account_binding.adopt_all_for_principal} and merge preview. *)

val detect_merge_conflicts :
  survivor_bindings:B.binding list ->
  loser_bindings:B.binding list ->
  merge_conflict list
(** Pure conflict list; empty means merge-safe for GitHub bindings. *)

(** {1 Split ownership conflicts}

    Unlink/split never transfers GitHub account bindings or credentials by
    default. Requested rebind of a GitHub binding id is refused (fail closed)
    unless an explicit future admin path is supplied — V1 refuses all requested
    GitHub rebinds. *)

type split_conflict = { binding_id : string; summary : string }

type split_ownership_decision =
  | Split_ok of {
      retained_binding_ids : string list;
          (** All GitHub bindings remain on the source Principal. *)
    }
  | Split_refuse of { conflicts : split_conflict list; audit : redacted_audit }

val evaluate_split_ownership :
  db:Sqlite3.db ->
  source_principal_id:P.principal_id ->
  ?requested_binding_ids:string list ->
  ?now:float ->
  ?audit_sink:(redacted_audit -> unit) ->
  unit ->
  (split_ownership_decision, string) result
(** Default ([requested_binding_ids = []]): [Split_ok] with all current GitHub
    bindings retained on the source. Non-empty requested rebinds are refused. *)
