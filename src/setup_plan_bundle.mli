(** Setup-owned Room access-bundle attachment and detachment (P19.M1.E1.T004).

    Confirmed setup atomically attaches or reuses a provenance-tracked managed
    bundle. Removing the last managed feature detaches only setup-owned linkage
    and preserves manual grants and unrelated bundles. Reruns are idempotent and
    inspectable. *)

type provenance = {
  setup_plan_id : string option;
  owner : string;  (** Always "setup" for managed linkages created here. *)
  feature_id : string;
      (** Managed feature that requires the bundle (e.g. route id). *)
  created_at : string;
}

type linkage = {
  id : string;
  room_id : string;
  bundle_id : string;
  provenance : provenance;
  status : string;  (** "attached" | "detached" *)
  attached_at : string;
  detached_at : string option;
}

type attach_result =
  | Attached of { linkage : linkage; first_time : bool }
  | Reused of { linkage : linkage }
      (** Same room+bundle+feature already attached. *)

type detach_result =
  | Detached of { linkage : linkage }
  | Still_attached of {
      linkage : linkage;
      remaining_features : int;
          (** Other setup-owned features still using this room+bundle. *)
    }
  | Not_found
  | Preserved_manual  (** Target is not setup-owned; left untouched. *)

val init_schema : Sqlite3.db -> unit

val attach :
  db:Sqlite3.db ->
  room_id:string ->
  bundle_id:string ->
  feature_id:string ->
  ?setup_plan_id:string ->
  ?now:float ->
  unit ->
  (attach_result, string) result
(** Atomically attach or reuse a setup-owned managed linkage. Idempotent on
    (room_id, bundle_id, feature_id). *)

val record_managed_feature :
  db:Sqlite3.db ->
  room_id:string ->
  bundle_id:string ->
  feature_id:string ->
  unit ->
  (unit, string) result
(** Alias of attach without plan id — for feature registration. *)

val remove_managed_feature :
  db:Sqlite3.db ->
  room_id:string ->
  bundle_id:string ->
  feature_id:string ->
  ?now:float ->
  unit ->
  (detach_result, string) result
(** Detach only when this was the last setup-owned feature for room+bundle.
    Manual grants (non-setup provenance) are never removed. *)

val inspect_room : db:Sqlite3.db -> room_id:string -> unit -> linkage list
(** All setup-owned linkages for a room (attached and detached history). *)

val list_attached : db:Sqlite3.db -> room_id:string -> unit -> linkage list

val is_setup_owned :
  db:Sqlite3.db -> room_id:string -> bundle_id:string -> unit -> bool

val count_attached_features :
  db:Sqlite3.db -> room_id:string -> bundle_id:string -> unit -> int
