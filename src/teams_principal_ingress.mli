(** Teams Bot Connector ingress: JWT verification and canonical human identity
    derivation (P21.M1.E1.T005).

    Verifies Bot Connector OpenID metadata/JWKS, RS256 signature, issuer,
    audience, time claims, tenant, activity [serviceUrl] against the token
    claim, and channel/key endorsements when supplied. Keys are cached with
    rotation/refetch. Fail closed on fetch, key, claim, tenant, or provenance
    failure.

    Canonical human identity is immutable tenant plus Teams AAD object id only.
    Bots and mutable display fields never establish a human principal. *)

type verified_claims = {
  issuer : string;
  audience : string;
  tenant_id : string;
  app_id : string option;
  service_url : string option;
  exp : float;
  nbf : float option;
}

type human_identity = {
  tenant_id : string;
  aad_object_id : string;  (** immutable Teams user id from verified activity *)
}

type outcome =
  | Human of {
      identity : human_identity;
      display_name : string option;
      claims : verified_claims;
    }
  | Bot_rejected of string
  | Invalid of string

type jwks_fetch = unit -> (Yojson.Safe.t, string) result
type metadata_fetch = unit -> (Yojson.Safe.t, string) result

val verify_and_derive :
  ?jwks_fetch:jwks_fetch ->
  ?metadata_fetch:metadata_fetch ->
  ?now:float ->
  ?expected_audience:string ->
  bearer_token:string ->
  activity_json:Yojson.Safe.t ->
  unit ->
  outcome
(** Verify the Bot Framework bearer JWT and derive a human identity from the
    activity. Optional fetch hooks enable offline/tests; default fetchers hit
    Bot Connector OpenID metadata and JWKS over HTTPS. *)

val clear_key_cache : unit -> unit
(** Drop cached OpenID issuer/JWKS state (tests / forced rotation). *)

val openid_configuration_url : string
(** Default Bot Connector OpenID metadata URL. *)

val normalize_service_url : string -> string
(** Normalize [serviceUrl] for claim comparison (trim, strip trailing [/]). *)

val human_identity_key : human_identity -> string
(** Canonical key: [tenant:<tid>:user:<aad_object_id>]. Display fields omitted.
*)
