(** Reusable typed admin setup plans (plan → confirm → apply).

    Planning produces values only: no config, database, external-system,
    Connector, or Session mutation. Confirm/apply, consent, and setup-owned
    access-bundle attachment belong to later tasks (T002–T004).

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md and
    docs/adr/0003-require-plan-confirm-apply-for-agent-setup.md.

    Apply payloads and free-form state JSON must be secret-free (credential
    handle ids only). Digest is computed over redacted canonical content so
    persist/render never need a second secret-bearing form. *)

type plan_id = string
type principal_kind = Principal | Channel_actor | Cli | System

type principal = {
  id : string;  (** Durable principal id when known; else stable actor key. *)
  kind : principal_kind;
  label : string option;  (** Display only; never credentials. *)
}

type context = {
  room_id : string option;
  session_key : string option;
  connector : string option;
  profile_id : string option;
  extra : (string * Yojson.Safe.t) list;
}

type readiness_status = Pass | Fail | Warn

type readiness_item = {
  name : string;
  status : readiness_status;
  message : string;
}

type warning = { code : string; message : string }

type diff_op =
  | Create of { path : string; value : Yojson.Safe.t }
  | Update of { path : string; from_ : Yojson.Safe.t; to_ : Yojson.Safe.t }
  | Delete of { path : string; old : Yojson.Safe.t }
  | Bind of { path : string; target : string; active : bool }
  | Note of { path : string; message : string }

type apply_kind =
  | Room_profile
  | Github_app_setup
  | Github_route
  | Access_bundle
  | Generic of string

type apply_payload = {
  kind : apply_kind;
  ops : Yojson.Safe.t;
      (** Canonical operations for apply (T002). Secret-free. *)
  data : Yojson.Safe.t;  (** Adapter blob; secret-free (handles/ids only). *)
}

type t = {
  id : plan_id;
  principal : principal;
  source : context;
  destination : context;
  current_state : Yojson.Safe.t;
  planned_state : Yojson.Safe.t;
  diff : diff_op list;
  readiness : readiness_item list;
  warnings : warning list;
  base_revision : string;
  created_at : string;
  expires_at : string;
  digest : string;
  apply_payload : apply_payload;
}

val default_ttl_seconds : float
(** Default plan TTL: 15 minutes. *)

val generate_id : ?now:float -> unit -> plan_id
val base_revision_of_config : Runtime_config.t -> string

val make :
  principal:principal ->
  source:context ->
  destination:context ->
  current_state:Yojson.Safe.t ->
  planned_state:Yojson.Safe.t ->
  diff:diff_op list ->
  readiness:readiness_item list ->
  warnings:warning list ->
  base_revision:string ->
  apply_payload:apply_payload ->
  ?ttl_seconds:float ->
  ?now:float ->
  ?id:plan_id ->
  unit ->
  t

val compute_digest : t -> string
(** Recompute digest from plan fields (excluding [digest] itself). *)

val is_expired : ?now:float -> t -> bool
val readiness_ok : t -> bool

val redact : t -> t
(** Defense-in-depth: walk free-form Yojson with [Config_show.redact_json].
    Valid plans are already secret-free; digest should be unchanged. *)

val to_canonical_json : t -> Yojson.Safe.t
(** Canonical body for hashing (no [digest] field; sorted object keys). *)

val to_persist_json : t -> Yojson.Safe.t
val of_persist_json : Yojson.Safe.t -> (t, string) result
val to_render_json : t -> Yojson.Safe.t
val format_summary : t -> string

val digests_equal : string -> string -> bool
(** Constant-time digest comparison for confirm/apply (T002). *)
