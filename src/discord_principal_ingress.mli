(** Discord Gateway / interaction ingress principal derivation (P21.M1.E1.T007).

    Production trust boundary for chat traffic is a successfully authenticated
    Discord Gateway WSS session (bot token Identify, Ready application identity,
    session id, and monotonic dispatch sequence). This module fails closed when
    that session context is missing or inconsistent, then derives an immutable
    guild-scoped human identity from Discord snowflakes.

    Canonical human identity is [guild_id] + [user_id] only. Bots, webhooks,
    display names, and DM-ambiguous traffic never establish a human principal.

    HTTP Interactions (slash commands etc.) may carry an Ed25519 body signature
    ([X-Signature-Ed25519] / [X-Signature-Timestamp]). Verification is supported
    via [mirage-crypto-ec] Ed25519 when the application public key is provided.
    Callers that skip signature verification must only do so after another
    authenticated boundary; [require_signature:true] is fail-closed by default
    for the interaction entry point. *)

type gateway_session = {
  session_id : string;
  application_id : string;
  ready : bool;
  last_seq : int option;
      (** Highest dispatch sequence accepted so far; used to reject regressions.
      *)
}
(** Authenticated Gateway session established after Identify/Ready. *)

type human_identity = {
  guild_id : string;  (** immutable Discord guild snowflake *)
  user_id : string;  (** immutable Discord user snowflake *)
}

type verified_context = {
  source : [ `Gateway | `Interaction ];
  application_id : string option;
  session_id : string option;
  seq : int option;
}

type outcome =
  | Human of {
      identity : human_identity;
      display_name : string option;
      context : verified_context;
    }
  | Bot_rejected of string
  | Invalid of string

val check_gateway_session :
  ?expected_application_id:string ->
  ?seq:int ->
  gateway_session ->
  (unit, string) result
(** Fail closed unless the session is Ready, has non-empty session/application
    ids, matches [expected_application_id] when given, and [seq] (when given)
    does not regress relative to [last_seq]. *)

val derive_from_gateway :
  session:gateway_session ->
  ?expected_application_id:string ->
  ?seq:int ->
  ?event_name:string ->
  payload_json:Yojson.Safe.t ->
  unit ->
  outcome
(** Derive a human principal from a Gateway dispatch payload (e.g.
    MESSAGE_CREATE [d] object). Requires a Ready session. Rejects bots,
    webhooks, missing guild/user snowflakes, and non-digit ids. *)

val derive_from_fields :
  session:gateway_session ->
  ?expected_application_id:string ->
  ?seq:int ->
  guild_id:string option ->
  user_id:string option ->
  ?bot:bool ->
  ?webhook_id:string ->
  ?display_name:string ->
  unit ->
  outcome
(** Same checks as [derive_from_gateway] for already-extracted fields. *)

val verify_interaction_signature :
  public_key_hex:string ->
  signature_hex:string ->
  timestamp:string ->
  body:string ->
  (unit, string) result
(** Discord Interaction Ed25519 verify: [msg = timestamp ^ body], signature and
    public key are hex-encoded 64- and 32-byte values respectively. *)

val derive_from_interaction :
  ?public_key_hex:string ->
  ?signature_hex:string ->
  ?timestamp:string ->
  ?body:string ->
  ?require_signature:bool ->
  ?expected_application_id:string ->
  interaction_json:Yojson.Safe.t ->
  unit ->
  outcome
(** Derive identity from an Interaction payload. When [require_signature] is
    true (default), signature headers + body + application public key must
    verify with Ed25519; otherwise fail closed. Guild + user snowflakes are
    required; bots rejected. *)

val human_identity_key : human_identity -> string
(** Canonical key: [guild:<guild_id>:user:<user_id>]. Display fields omitted. *)

val is_snowflake : string -> bool
(** Non-empty decimal digit string (Discord snowflake shape). *)

val connector_actor_key_of_identity :
  human_identity -> (Principal_identity.connector_actor_key, string) result
(** Map to the shared Principal domain model ([tenant_or_workspace = guild_id],
    [immutable_user_id = user_id]). *)
