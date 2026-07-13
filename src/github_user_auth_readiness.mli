(** GitHub App user-authorization readiness (P21.M2.E1.T001).

    Pure evaluation of a config snapshot for Principal-owned act-as-user
    authorization. Managed setup must refuse act-as-user until every required
    check passes.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

type level = Pass | Warn | Fail

type check = { name : string; level : level; detail : string; repair : string }
(** Named readiness check. [detail] and [repair] must never embed secret
    material — only handles, hostnames, and counts. *)

type config_snapshot = {
  host : string;  (** GitHub host. V1 live support is [github.com] only. *)
  app_id : int option;  (** Numeric GitHub App id. *)
  client_id_handle : string option;
      (** Opaque credential-store handle for the OAuth client id (not
          plaintext). *)
  client_secret_handle : string option;
      (** Opaque credential-store handle for the OAuth client secret. *)
  callback_uri : string option;
      (** Exact OAuth callback URI registered for the App (web flow). *)
  expiring_user_tokens : bool;
      (** GitHub App user tokens must expire; non-expiring is refused. *)
  device_flow_requested : bool;
      (** Caller/operator requested device authorization for this setup. *)
  device_flow_enabled : bool;
      (** App/device settings allow device flow. Checked only when requested. *)
  master_key_present : bool;
      (** Vault master key available from the external key source. *)
  permissions : (string * string) list;
      (** App permission name × access level pairs required for user-attributed
          work (e.g. [("pull_requests", "write")]). *)
  private_continuation_ready : bool;
      (** Private delivery path available for auth URLs, device codes, and
          account-selection controls (Rooms see only neutral status). *)
}
(** Deliberately not full [Runtime_config.t]: callers assemble this from App
    setup credentials, vault readiness, policy, and private-delivery probes. *)

type readiness = {
  checks : check list;
  can_act_as_user : bool;
      (** True only when every required check is [Pass]. Managed setup must
          refuse act-as-user while this is false. *)
}

val string_of_level : level -> string

val evaluate : config_snapshot -> readiness
(** Run all readiness checks and compute [can_act_as_user]. *)

val overall : check list -> level
(** Worst level among checks ([Fail] > [Warn] > [Pass]). *)

val format : readiness -> string
(** Human-readable report; never prints secret material. *)
