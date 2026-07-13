(** Staged master-key rotation and resumable rewrap (P21.M2.E4.T007).

    Install a new vault master key as Active, rewrap Principal-owned GitHub user
    token records under generation CAS without advancing token generation,
    resume after crashes from durable key-id checkpoints, verify full coverage
    before retiring the old key, and allow rollback only while both keys remain
    authorized.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0006-use-principal-owned-github-user-tokens.md. *)

(** {1 Job phases} *)

type phase =
  | In_progress
      (** New key is Active; rewrap may still leave rows under [from_key_id]. *)
  | Verified
      (** Every live vault row is under [to_key_id]; old key still authorized.
      *)
  | Completed  (** Old key may be retired; rollback is closed. *)
  | Rolled_back  (** Rows restored under [from_key_id]; rotation cancelled. *)

val string_of_phase : phase -> string

type job = {
  id : string;
  from_key_id : Github_user_token_master_key.key_id;
  from_key_version : Github_user_token_master_key.key_version;
  to_key_id : Github_user_token_master_key.key_id;
  to_key_version : Github_user_token_master_key.key_version;
  phase : phase;
  last_processed_id : string option;
  rewrapped_count : int;
  conflict_count : int;
  created_at : string;
  updated_at : string;
}

type progress = {
  total_records : int;
  on_from_key : int;
  on_to_key : int;
  other_keys : string list;
}

type batch_result = {
  job : job;
  attempted : int;
  rewrapped : int;
  skipped_already : int;
  conflicts : int;
  remaining_on_from : int;
      (** For forward batches: rows still under from_key. For rollback batches:
          rows still under to_key (not yet restored). *)
}

(** {1 Denials (fail closed)} *)

type denial =
  | Vault of Github_user_token_vault.denial
  | No_active_rotation
  | Rotation_already_active of { id : string }
  | Job_not_found
  | Premature_retire of { remaining_on_from : int; other_keys : string list }
  | Rollback_unavailable of string
  | Unknown_or_mixed_key of {
      record_id : string;
      key_id : string;
      allowed : string list;
    }
  | Key_not_authorized of {
      key_id : Github_user_token_master_key.key_id;
      role : string;
    }
  | Active_key_mismatch of {
      expected : Github_user_token_master_key.key_id;
      actual : Github_user_token_master_key.key_id option;
    }
  | Invalid_input of string
  | Invalid_state of string
  | Storage of string

val string_of_denial : denial -> string
val denial_exposes_token : denial:denial -> plaintext:string -> bool

(** {1 Schema / load} *)

val ensure_schema : Sqlite3.db -> unit
(** Idempotent vault + rewrap job tables. *)

val load : db:Sqlite3.db -> id:string -> (job option, denial) result
val load_active : db:Sqlite3.db -> (job option, denial) result
val progress : db:Sqlite3.db -> job -> (progress, denial) result

val job_to_json : job -> Yojson.Safe.t
(** Metadata only; never key material or token plaintext. *)

(** {1 Rotation lifecycle} *)

val start :
  db:Sqlite3.db ->
  keys:Github_user_token_vault.key_provider ->
  ?id:string ->
  ?now:float ->
  from_key_id:Github_user_token_master_key.key_id ->
  from_key_version:Github_user_token_master_key.key_version ->
  to_key_id:Github_user_token_master_key.key_id ->
  to_key_version:Github_user_token_master_key.key_version ->
  unit ->
  (job, denial) result
(** Begin staged rotation. Requires:
    - no other in_progress/verified job
    - provider Active key is [to_key_id]/[to_key_version]
    - both from and to materials resolve
    - vault rows use only from/to key IDs (mixed/unknown fail closed) *)

val rewrap_batch :
  db:Sqlite3.db ->
  keys:Github_user_token_vault.key_provider ->
  ?job_id:string ->
  ?limit:int ->
  ?now:float ->
  unit ->
  (batch_result, denial) result
(** Rewrap up to [limit] rows still under [from_key_id] onto [to_key_id] with
    generation CAS (generation not advanced) and a fresh AEAD nonce per rewrite.
    Resumable after crash: remaining work is key-id driven. Concurrent create
    under the active (to) key is left alone; concurrent replace that advances
    generation is retried or counted as conflict without losing authority. *)

val verify_completion :
  db:Sqlite3.db ->
  keys:Github_user_token_vault.key_provider ->
  ?job_id:string ->
  ?now:float ->
  unit ->
  (job, denial) result
(** Mark [Verified] only when no live row remains on [from_key_id] and no
    unknown key IDs appear. Both keys must still resolve. *)

val complete_retire :
  db:Sqlite3.db ->
  keys:Github_user_token_vault.key_provider ->
  ?job_id:string ->
  ?now:float ->
  unit ->
  (job, denial) result
(** Retire authorization for the old key only after [Verified] and a second
    coverage check. Premature retire from [In_progress] is rejected. After
    [Completed], rollback is unavailable. *)

val rollback_batch :
  db:Sqlite3.db ->
  keys:Github_user_token_vault.key_provider ->
  ?job_id:string ->
  ?limit:int ->
  ?now:float ->
  unit ->
  (batch_result, denial) result
(** Rewrap rows from to→from while both keys remain authorized and the job is
    [In_progress] or [Verified]. Rejected after [Completed]. *)

val rollback_all :
  db:Sqlite3.db ->
  keys:Github_user_token_vault.key_provider ->
  ?job_id:string ->
  ?limit:int ->
  ?now:float ->
  unit ->
  (batch_result, denial) result
(** Drive [rollback_batch] until [Rolled_back] or failure. *)
