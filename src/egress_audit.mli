(** Egress audit event recording.

    Records every egress policy decision (allowed or denied) into a dedicated
    SQLite table for compliance and debugging. All sensitive fields (host,
    method, credential IDs) are redacted or aliased before storage. *)

type decision = Allowed | Denied

val decision_to_string : decision -> string

type event = {
  id : int;
  timestamp : string;
  decision : decision;
  host_redacted : string;
  method_redacted : string option;
  path_redacted : string option;
  matched_rule_index : int;
  session_key : string option;
  snapshot_id : string option;
  tool_name : string option;
  profile_id : string option;
  credential_handle_ids : string list;
      (** Alias IDs only -- never actual credential values. *)
}

val redact_host : string -> string
(** Redact a hostname for audit storage. *)

val redact_method : string -> string
(** Redact an HTTP method for audit storage. *)

val redact_path : string -> string
(** Redact a URL path for audit storage. *)

val init_schema : Sqlite3.db -> unit
(** Create the egress_audit table and indexes if they do not exist. *)

val record :
  db:Sqlite3.db ->
  decision:decision ->
  host:string ->
  ?method_:string ->
  ?path:string ->
  matched_rule_index:int ->
  ?session_key:string ->
  ?snapshot_id:string ->
  ?tool_name:string ->
  ?profile_id:string ->
  ?credential_handle_ids:string list ->
  unit ->
  unit
(** Record an egress audit event. All sensitive fields are redacted before
    storage. Credential IDs are stored as-is (they are opaque aliases, never
    actual values). *)

val query :
  db:Sqlite3.db ->
  ?decision:decision ->
  ?session_key:string ->
  ?tool_name:string ->
  ?from_timestamp:string ->
  ?to_timestamp:string ->
  ?limit:int ->
  unit ->
  event list
(** Query audit events with optional filters. *)

val event_to_json : event -> Yojson.Safe.t
(** Serialize an event to JSON. *)

val delete_before : db:Sqlite3.db -> before_timestamp:string -> int
(** Delete audit events older than the given timestamp. Returns the number of
    deleted rows. *)

val purge_older_than : db:Sqlite3.db -> days:int -> ?now:float -> unit -> int
(** B830 retention: delete events with [timestamp] strictly older than [days]
    days before [now] via {!delete_before}. [days <= 0] is a no-op returning 0.
*)
