(** Vault backup/restore and key-compromise recovery (P21.M2.E4.T008).

    Encrypted backup export carries sealed envelopes and key IDs only — never
    plaintext tokens or master-key material. Restore requires an explicit
    operator proof, schema/key-ID compatibility checks, starts with user
    authorization disabled, and discards leases. Suspected master-key compromise
    or unrecoverable loss disables authorization, destroys affected vault tokens
    and pending authorization material, marks keys for rotation, and requires
    safe relink where confidentiality cannot be proven.

    {2 Whole-store rollback limitation (V1)}

    Record AEAD and token-generation CAS detect row swap and live stale writes,
    but {b cannot} detect replacement of the entire store with an internally
    consistent older snapshot encrypted under an available key. That requires a
    monotonic anchor outside both the database and its backup.
    {!whole_store_rollback_detectable_without_external_anchor} is therefore
    always [false]. Backup selection and restore authorization are an explicit
    operational trust boundary.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md,
    docs/adr/0006-use-principal-owned-github-user-tokens.md, and
    docs/github-vault-recovery.md. *)

(** {1 Contract constants} *)

val backup_schema_version : int
(** Backup envelope document schema version; starts at 1. *)

val whole_store_rollback_detectable_without_external_anchor : bool
(** Always [false]. Documented V1 limitation: a whole-store rollback under the
    same available key is not detectable without an external monotonic anchor.
    Tests assert this constant. *)

val whole_store_rollback_limitation_tag : string
(** Acknowledgment tag operators must include in
    [operator_proof.acknowledged_limitations] before restore. *)

val whole_store_rollback_limitation_statement : string
(** Plain-language statement of the whole-store rollback limitation. *)

val compromise_relink_required_tag : string
(** Acknowledgment tag operators must include before {!compromise_disable}.
    Confidentiality of material sealed under a compromised key cannot be proven;
    relink is required. *)

(** {1 Operator proof} *)

type operator_proof = {
  operator_id : string;
      (** Non-empty operator identity for the audit event (never a secret). *)
  approval : string;
      (** Explicit non-empty operator approval proof (token or signed statement
          reference). Empty approval fails closed. *)
  acknowledged_limitations : string list;
      (** Must include the operation-specific limitation tag(s). *)
}
(** Explicit operator authorization for restore / compromise response. *)

val make_operator_proof :
  operator_id:string ->
  approval:string ->
  acknowledged_limitations:string list ->
  unit ->
  (operator_proof, string) result
(** Validate non-empty [operator_id] and [approval]. *)

(** {1 Backup document (encrypted envelopes + key IDs only)} *)

type sealed_envelope = {
  id : string;
  principal_id : string;
  github_user_id : int64;
  app_id : int;
  host : string;
  record_version : int;
  key_id : Github_user_token_master_key.key_id;
  key_version : Github_user_token_master_key.key_version;
  key_fingerprint : string;
      (** Non-secret classification aid; not key material. *)
  generation : int;
  scopes : string list;
  expires_at : string;
  ciphertext : string;
      (** Authenticated vault envelope only — never plaintext tokens. *)
  created_at : string;
  updated_at : string;
}
(** One exported sealed vault row. No plaintext tokens or AES key bytes. *)

type backup = {
  backup_schema_version : int;
  vault_schema_version : int;
  exported_at : string;
  required_key_ids : string list;
      (** Distinct [key_id] values required to open any envelope in this backup.
      *)
  envelopes : sealed_envelope list;
}
(** Portable backup document. Serializable to JSON without secrets. *)

val export_backup :
  db:Sqlite3.db -> ?now:float -> unit -> (backup, string) result
(** Export all vault rows as sealed envelopes plus required key IDs. Fails if
    the vault schema is missing. Never includes plaintext tokens or key
    material. *)

val backup_to_json : backup -> Yojson.Safe.t
(** JSON form of a backup. Never embeds plaintext tokens or key material. *)

val backup_of_json : Yojson.Safe.t -> (backup, string) result
(** Parse a backup document. Rejects unknown schema and malformed envelopes. *)

val backup_contains_plaintext : backup:backup -> plaintext:string -> bool
(** Test helper: true if [plaintext] appears in any backup text field (including
    ciphertext string bytes). Real tokens must not appear. *)

(** {1 Compatibility} *)

type compatibility_issue =
  | Unsupported_backup_schema of { version : int }
  | Unsupported_vault_schema of { version : int }
  | Unsupported_record_version of { id : string; version : int }
  | Missing_required_key of { key_id : string }
  | Unopenable_envelope of {
      id : string;
      reason : Github_user_token_vault.denial;
    }
  | Empty_backup

val string_of_compatibility_issue : compatibility_issue -> string

val check_compatibility :
  keys:Github_user_token_vault.key_provider ->
  backup:backup ->
  unit ->
  (unit, compatibility_issue list) result
(** Fail closed unless backup/vault/record schemas are supported and every
    [required_key_id] resolves and opens at least one envelope sealed under it
    (empty backups are rejected). *)

(** {1 Authorization gate (post restore / compromise)} *)

type recovery_state = {
  user_authorization_enabled : bool;
      (** [false] after restore or compromise until operators complete safe
          recovery. *)
  last_event : string;  (** [none] | [restore] | [compromise_disable]. *)
  last_reason : string option;
  last_operator_id : string option;
  last_event_at : string option;
  compromised_key_ids : string list;
  requires_relink : bool;
  requires_key_rotation : bool;
}
(** Durable recovery gate. Singleton row. *)

val ensure_schema : Sqlite3.db -> unit
(** Idempotent recovery state + event tables; also ensures vault schema. *)

val load_state : db:Sqlite3.db -> (recovery_state, string) result
(** Load singleton recovery state (defaults: authorization enabled, no event).
*)

val user_authorization_enabled : db:Sqlite3.db -> (bool, string) result
(** Convenience for readiness / act-as-user gates. *)

(** {1 Optional destroy hooks (bindings / leases not yet fully modeled)} *)

type destroy_hooks = {
  destroy_bindings : unit -> (int, string) result;
      (** Destroy Principal-owned GitHub account bindings / preferences. Returns
          count destroyed. Default: no-op [Ok 0]. *)
  destroy_leases : unit -> (int, string) result;
      (** Discard access-token leases. Default: no-op [Ok 0]. *)
  destroy_pending_extra : unit -> (int, string) result;
      (** Extra pending credential material (device codes, PKCE secrets, etc.).
          Default: no-op [Ok 0]. *)
}
(** Injectable cleanup for surfaces owned by later modules. *)

val default_destroy_hooks : destroy_hooks

(** {1 Restore} *)

type restore_result = {
  imported : int;
  required_key_ids : string list;
  authorization_disabled : bool;
      (** Always [true] after a successful restore. *)
  leases_discarded : int;
  bindings_destroyed : int;
  approved_by : string;  (** Operator id from the required proof. *)
}
(** Outcome of a successful restore. Authorization remains disabled. *)

type denial =
  | Operator_proof_required of string
  | Compatibility of compatibility_issue list
  | Vault of Github_user_token_vault.denial
  | Invalid_input of string
  | Storage of string
  | Hook of string

val string_of_denial : denial -> string
val denial_exposes_token : denial:denial -> plaintext:string -> bool

val restore :
  db:Sqlite3.db ->
  keys:Github_user_token_vault.key_provider ->
  proof:operator_proof ->
  backup:backup ->
  ?hooks:destroy_hooks ->
  ?now:float ->
  unit ->
  (restore_result, denial) result
(** Restore sealed envelopes after operator proof and compatibility checks.

    - Requires [whole_store_rollback_limitation_tag] in
      [proof.acknowledged_limitations]
    - Requires non-empty [proof.approval]
    - Replaces live vault rows with the backup set (ciphertext only)
    - Discards leases via hooks
    - Disables user authorization
    - Does not re-enable act-as-user; reconciliation + re-link remain operator
      steps
    - Does not claim whole-store anti-rollback protection *)

(** {1 Key-compromise / unrecoverable-loss response} *)

type compromise_result = {
  authorization_disabled : bool;  (** Always [true]. *)
  vault_records_destroyed : int;
  pending_auth_tx_destroyed : int;
  rewrap_jobs_destroyed : int;
  bindings_destroyed : int;
  leases_discarded : int;
  pending_extra_destroyed : int;
  affected_key_ids : string list;
  requires_key_rotation : bool;  (** Always [true]. *)
  requires_relink : bool;
      (** Always [true] — confidentiality cannot be proven. *)
  approved_by : string;  (** Operator id from the required proof. *)
}
(** Destructive compromise response. No in-place recovery shortcut. *)

val compromise_disable :
  db:Sqlite3.db ->
  proof:operator_proof ->
  reason:string ->
  ?affected_key_ids:string list ->
  ?hooks:destroy_hooks ->
  ?now:float ->
  unit ->
  (compromise_result, denial) result
(** Suspected master-key compromise or unrecoverable loss.

    - Requires [compromise_relink_required_tag] in
      [proof.acknowledged_limitations]
    - Disables all Principal-owned GitHub user authorization
    - Destroys vault records (all, or only those under [affected_key_ids])
    - Destroys pending authorization transactions and staged rewrap jobs
    - Invokes binding / lease / pending hooks
    - Marks affected keys compromised and requires key rotation
    - Requires safe relink; never falls back to App attribution for
      [User_required] work during recovery
    - Does not embed or return token plaintext *)

val state_to_json : recovery_state -> Yojson.Safe.t
(** Redacted state JSON; never key material or tokens. *)
