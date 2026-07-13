(** Persistence helpers for Principal merge state (P21.M1.E1.T011).

    Schema and CRUD for external accounts, preferences, pending-auth counters,
    immutable actor snapshots, and merge receipts. Merge logic is in
    {!Principal_merge}. *)

val ensure_schema : Sqlite3.db -> unit
val iso_now : ?now:float -> unit -> string
val generate_merge_id : ?now:float -> unit -> string
val generate_snapshot_id : ?now:float -> unit -> string
val generate_account_id : ?now:float -> unit -> string

type external_account = {
  id : string;
  principal_id : Principal_identity.principal_id;
  account_kind : string;
  uniqueness_domain : string;
  account_identity : string;
  exclusive_slot : bool;
  revision : int;
  payload_json : string;
  created_at : string;
  updated_at : string;
}

val put_external_account :
  db:Sqlite3.db ->
  ?now:float ->
  external_account ->
  (external_account, string) result

val list_external_accounts :
  db:Sqlite3.db ->
  principal_id:Principal_identity.principal_id ->
  (external_account list, string) result

val reassign_external_account :
  db:Sqlite3.db ->
  id:string ->
  to_principal:Principal_identity.principal_id ->
  now_s:string ->
  (unit, string) result

val delete_external_account :
  db:Sqlite3.db -> id:string -> (unit, string) result

type preference = {
  principal_id : Principal_identity.principal_id;
  key : string;
  value : string;
  revision : int;
  updated_at : string;
}

val put_preference :
  db:Sqlite3.db ->
  ?now:float ->
  principal_id:Principal_identity.principal_id ->
  key:string ->
  value:string ->
  ?revision:int ->
  unit ->
  (preference, string) result

val list_preferences :
  db:Sqlite3.db ->
  principal_id:Principal_identity.principal_id ->
  (preference list, string) result

val delete_preference :
  db:Sqlite3.db ->
  principal_id:Principal_identity.principal_id ->
  key:string ->
  (unit, string) result

val set_pending_authorization_count :
  db:Sqlite3.db ->
  principal_id:Principal_identity.principal_id ->
  count:int ->
  (unit, string) result

val get_pending_authorization_count :
  db:Sqlite3.db ->
  principal_id:Principal_identity.principal_id ->
  (int, string) result

type actor_snapshot = {
  id : string;
  actor_key : string;
  principal_id_at_snapshot : Principal_identity.principal_id;
  actor_json : string;
  reason : string;
  merge_id : string option;
  created_at : string;
}

val insert_actor_snapshot :
  db:Sqlite3.db -> actor_snapshot -> (actor_snapshot, string) result

val get_actor_snapshot :
  db:Sqlite3.db -> id:string -> (actor_snapshot option, string) result

val list_actor_snapshots_for_actor :
  db:Sqlite3.db -> actor_key:string -> (actor_snapshot list, string) result

type preference_resolution = {
  key : string;
  outcome : [ `Adopted_from_loser | `Kept_survivor | `Identical ];
  survivor_value : string option;
  loser_value : string option;
}

type merge_receipt = {
  id : string;
  link_tx_id : string option;
  survivor_id : Principal_identity.principal_id;
  loser_id : Principal_identity.principal_id;
  adopted_actor_keys : string list;
  adopted_link_ids : string list;
  preference_resolutions : preference_resolution list;
  pending_auth_invalidated : int;
  actor_snapshot_ids : string list;
  survivor_revision_after : int;
  loser_revision_after : int;
  applied_at : string;
  notes : string list;
}

val insert_merge_receipt :
  db:Sqlite3.db -> merge_receipt -> (merge_receipt, string) result

val get_merge_receipt :
  db:Sqlite3.db -> id:string -> (merge_receipt option, string) result

val get_merge_receipt_by_link_tx :
  db:Sqlite3.db -> link_tx_id:string -> (merge_receipt option, string) result

val find_receipt_for_pair :
  db:Sqlite3.db ->
  survivor_id:Principal_identity.principal_id ->
  loser_id:Principal_identity.principal_id ->
  (merge_receipt option, string) result
