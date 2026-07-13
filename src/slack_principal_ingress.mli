(** Slack Socket Mode / Events API ingress: envelope validation and canonical
    human identity derivation (P21.M1.E1.T006).

    Production trust boundary for Socket Mode is the app-token-authenticated
    WebSocket (URL obtained via [apps.connections.open] over TLS). This module
    pure-validates inbound WSS JSON:

    - hello [connection_info.app_id] against the expected app
    - envelope [type] and non-empty unique [envelope_id]
    - enterprise/team workspace namespace
    - immutable Slack [user_id] (never display name alone)
    - reject [bot_id] / bot messages
    - acknowledge each envelope_id exactly once (injectable seen set)

    HTTP Events API mode cannot inherit Socket Mode trust: it requires separate
    Slack signing-secret HMAC-SHA256 verification via
    [verify_events_api_signature].

    Canonical human identity is workspace-scoped [team_id] (+ optional
    [enterprise_id]) and immutable [user_id] only. *)

(** {1 Identity} *)

type human_identity = {
  team_id : string;  (** Slack workspace/team id (T...). *)
  enterprise_id : string option;  (** Enterprise Grid id when present (E...). *)
  user_id : string;  (** Immutable Slack user id (U...). *)
}
(** Workspace-scoped immutable human identity. Display names are never part of
    this record. *)

type envelope_meta = {
  envelope_id : string;
  envelope_type : string;
  accepts_response_payload : bool;
}

type verified_event = {
  envelope : envelope_meta;
  api_app_id : string option;
  team_id : string;
  enterprise_id : string option;
  event_type : string;
  channel_id : string option;
  event_ts : string option;
}

type outcome =
  | Human of {
      identity : human_identity;
      display_name : string option;
          (** Optional presentation only; not identity. *)
      event : verified_event;
      ack : Yojson.Safe.t;
          (** [\{"envelope_id": ...\}] — caller must send at most once. *)
    }
  | Hello of { app_id : string; num_connections : int option }
  | Disconnect of { reason : string option }
  | Ack_only of {
      envelope_id : string;
      envelope_type : string;
      reason : string;
      ack : Yojson.Safe.t;
    }
      (** Valid envelope that should be acked but does not establish a human
          principal (e.g. non-message events). *)
  | Bot_rejected of string
  | Replay of string
      (** envelope_id already seen — do not re-ack as success. *)
  | Invalid of string

(** {1 Envelope_id dedupe (ack-once / replay)} *)

type seen_set = { has : string -> bool; mark : string -> unit }
(** Injectable seen-set for envelope_id uniqueness. Production may back this
    with durable storage; tests use [empty_seen_set]. *)

val empty_seen_set : unit -> seen_set
(** Fresh in-memory Hashtbl-backed seen set. *)

(** {1 Validation} *)

val make_ack : envelope_id:string -> Yojson.Safe.t
(** Socket Mode acknowledgement payload: [\{"envelope_id": id\}]. *)

val validate_hello : ?expected_app_id:string -> Yojson.Safe.t -> outcome
(** Validate a Socket Mode [hello] message. When [expected_app_id] is set,
    [connection_info.app_id] must match. *)

val validate_socket_message :
  ?expected_app_id:string ->
  ?expected_team_id:string ->
  ?expected_enterprise_id:string ->
  ?seen:seen_set ->
  Yojson.Safe.t ->
  outcome
(** Pure validation of one Socket Mode WebSocket JSON message (hello,
    disconnect, or envelope). On first acceptance of an envelope_id, marks it in
    [seen] and returns an [ack] for the caller to send exactly once. *)

val validate_socket_message_string :
  ?expected_app_id:string ->
  ?expected_team_id:string ->
  ?expected_enterprise_id:string ->
  ?seen:seen_set ->
  string ->
  outcome
(** Parse JSON string then [validate_socket_message]. Fail closed on parse
    error. *)

(** {1 HTTP Events API (separate trust path)} *)

val verify_events_api_signature :
  ?now:float ->
  ?max_skew_s:float ->
  signing_secret:string ->
  timestamp:string ->
  body:string ->
  signature:string ->
  unit ->
  (unit, string) result
(** Slack signing-secret HMAC-SHA256 verification for HTTP Events API mode.
    Computes [v0=hex(HMAC_SHA256(secret, "v0:" ^ timestamp ^ ":" ^ body))] and
    compares in constant time. Rejects empty secret, unparseable/skewed
    timestamps (default ±300s), and signature mismatch. This path cannot inherit
    Socket Mode trust. *)

(** {1 Canonical keys} *)

val human_identity_key : human_identity -> string
(** Canonical key without display fields: [team:<team_id>:user:<user_id>] or
    [enterprise:<enterprise_id>:team:<team_id>:user:<user_id>]. *)

val workspace_scope : human_identity -> string
(** Namespace segment for Principal connector scope: [team_id], or
    [enterprise_id/team_id] when enterprise is present. *)

val to_connector_actor_key :
  human_identity -> (Principal_identity.connector_actor_key, string) result
(** Build a Slack [Principal_identity.connector_actor_key] from verified
    identity. *)
