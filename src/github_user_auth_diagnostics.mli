(** User-authorization diagnostics and metrics (P21.M4.E1.T003).

    Pure, redacted diagnostics surface for GitHub user-authorization. Status and
    metrics distinguish SSO, permission, refresh, rate-limit, revocation,
    App/repo scope, expiry, ambiguity, private-delivery, and identity failures
    with actionable safe guidance — never tokens, sealed ciphertext, OAuth
    client secrets, reusable device/user codes, vault row ids, or raw vault
    refs.

    Sections assemble counts and shapes from existing authoritative modules
    without performing network I/O:

    - {!Github_user_auth_readiness} — readiness check levels and
      [can_act_as_user]
    - {!Github_account_admin_surface} — binding state and vault-attached flags
    - {!Github_attribution_audit} — failure classes and result kinds
    - {!Github_user_token_refresh} — refresh outcomes / denials / flights
    - {!Github_user_auth_invalidate} — revocation receipts
    - {!Github_user_auth_delivery} — private-delivery refusals
    - {!Github_attribution_authorize} — authorize denials

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0006-use-principal-owned-github-user-tokens.md. *)

val schema_version : int
(** Diagnostics schema / export version; starts at 1. *)

(** {1 Diagnostic failure classes}

    First-class classes from the acceptance contract. Distinct so status and
    metrics never collapse SSO vs permission vs rate-limit vs private-delivery
    into a single "error". *)

type failure_class =
  | Sso  (** SAML/SSO session or authorization required / lost. *)
  | Permission  (** Insufficient installation / Org / repo permissions. *)
  | Refresh
      (** Token generation refresh race, stale pin, or refresh failure. *)
  | Rate_limit
      (** GitHub HTTP 429 / abuse / [slow_down] provider throttling. *)
  | Revocation
      (** Vault/binding/installation revoked, disabled, or authority lost. *)
  | App_scope
      (** Installation repo selection / App scope no longer covers the action.
      *)
  | Expiry  (** Authorization transaction, token, or device flow expired. *)
  | Ambiguity  (** Multiple eligible accounts; no deterministic preference. *)
  | Private_delivery
      (** Private channel absent or material blocked from shared Room. *)
  | Identity
      (** Principal / actor / lineage / binding identity break or missing. *)
  | Policy  (** Attribution policy / tool catalog / repo grant. *)
  | Confirmation  (** Action confirmation missing or stale. *)
  | Rollout_gate  (** Attribution / pilot / production gate disabled. *)
  | Other of string  (** Reserved; prefer a first-class class when possible. *)

val failure_class_to_string : failure_class -> string
val failure_class_of_string : string -> (failure_class, string) result

val all_failure_classes : failure_class list
(** Stable ordered list of first-class classes (excludes [Other _]). *)

val guidance_for : failure_class -> string
(** Actionable, secret-free operator/user guidance for [failure_class]. Never
    embeds tokens, codes, URLs with secrets, or vault material. *)

val severity_of : failure_class -> string
(** ["fail"] | ["warn"] — severity hint for status surfaces. *)

(** {1 Classification}

    Map codes and typed denials from neighboring modules onto distinct
    [failure_class] values. Precedence prefers explicit [code] matches. *)

val classify_code :
  ?failed_check:string -> ?code:string -> unit -> failure_class
(** Classify a stable machine code (and optional failed-check surface). Extends
    {!Github_attribution_audit.classify_failure} with rate-limit, expiry, and
    private-delivery classes. *)

val classify_audit_class :
  Github_attribution_audit.failure_class -> failure_class
(** Project an attribution-audit failure class into diagnostics classes. *)

val classify_refresh_denial : Github_user_token_refresh.denial -> failure_class
(** Map a refresh denial (including HTTP 429 → [Rate_limit],
    [Refresh_token_expired] → [Expiry]). *)

val classify_delivery_refuse :
  Github_user_auth_delivery.refuse_reason -> failure_class
(** Private-delivery refusals → [Private_delivery] (or [Identity] for missing
    Principal). *)

val classify_authorize_deny : Github_attribution_authorize.deny -> failure_class
(** Project an authorize [Deny] via its repair code / failed_check. *)

(** {1 Status entries}

    One actionable status line for a single observed failure or class summary.
    Never carries secrets. *)

type status_entry = {
  failure_class : failure_class;
  code : string;
      (** Stable machine code (e.g. ["sso_required"], ["http_denial:429"]). *)
  message : string;  (** Short redacted description. *)
  guidance : string;  (** Actionable safe guidance. *)
  severity : string;  (** ["fail"] | ["warn"]. *)
  source : string option;
      (** Optional origin surface: ["authorize"], ["refresh"], ["delivery"],
          ["audit"], ["readiness"]. *)
}

val make_status_entry :
  failure_class:failure_class ->
  code:string ->
  ?message:string ->
  ?guidance:string ->
  ?source:string ->
  unit ->
  status_entry
(** Default [guidance] from {!guidance_for}; default [message] from code. *)

val status_entry_to_json : status_entry -> Yojson.Safe.t
val status_entry_format : status_entry -> string
val status_of_authorize_deny : Github_attribution_authorize.deny -> status_entry
val status_of_refresh_denial : Github_user_token_refresh.denial -> status_entry

val status_of_delivery_refuse :
  Github_user_auth_delivery.refuse_error -> status_entry

val status_of_audit_record : Github_attribution_audit.t -> status_entry option
(** [Some] when the record carries a failure class or deny-like result. *)

(** {1 Failure-class metrics}

    Pure counts keyed by first-class [failure_class]. *)

type class_metrics = {
  observations : int;
  by_class : (string * int) list;
      (** [failure_class] string → count, sorted by descending count then name.
      *)
}

val empty_class_metrics : class_metrics
val class_metrics_of_status_entries : status_entry list -> class_metrics

val class_metrics_of_audit_records :
  Github_attribution_audit.t list -> class_metrics

val class_metrics_of_refresh_denials :
  Github_user_token_refresh.denial list -> class_metrics

val class_metrics_of_delivery_refuses :
  Github_user_auth_delivery.refuse_error list -> class_metrics

val merge_class_metrics : class_metrics -> class_metrics -> class_metrics
val class_metrics_to_json : class_metrics -> Yojson.Safe.t
val class_metrics_format : class_metrics -> string list

(** {1 Readiness counters}

    Aggregated from {!Github_user_auth_readiness.evaluate} outputs. Pure counts;
    never embeds [detail] / [repair] text that could leak handle values. *)

type readiness_counters = {
  evaluations : int;
  pass_count : int;
  warn_count : int;
  fail_count : int;
  can_act_as_user_count : int;
  failing_check_counts : (string * int) list;
      (** Check name → fail count, sorted by descending count then name. *)
  repairs_pending : int;
}

val empty_readiness_counters : readiness_counters

val readiness_counters_of_snapshot :
  Github_user_auth_readiness.readiness -> readiness_counters

(** {1 Binding state counters}

    Aggregated from {!Github_account_admin_surface} redacted account views —
    counts only, never per-binding tokens or [vault_ref] strings. *)

type binding_state_counters = {
  bindings : int;
  vault_attached_count : int;
  vault_detached_count : int;
  authorization_status_counts : (string * int) list;
  distinct_hosts : (string * int) list;
  distinct_apps : (int * int) list;
}

val empty_binding_state_counters : binding_state_counters

val binding_state_counters_of_accounts :
  Github_account_admin_surface.redacted_account list -> binding_state_counters

(** {1 Refresh outcome counters} *)

type refresh_outcome_counters = {
  observations : int;
  successes : int;
  refreshes_performed : int;
  joined_flight_count : int;
  refreshed_reused_count : int;
  in_flight_denied : int;
  denial_counts : (string * int) list;
  flight_phase_counts : (string * int) list;
}

val empty_refresh_outcome_counters : refresh_outcome_counters

val refresh_outcome_counters_of_outcomes :
  Github_user_token_refresh.outcome list -> refresh_outcome_counters

val refresh_outcome_counters_of_denials :
  Github_user_token_refresh.denial list -> refresh_outcome_counters

val refresh_outcome_counters_of_flights :
  Github_user_token_refresh.flight list -> refresh_outcome_counters

(** {1 Revocation outcome counters} *)

type revocation_outcome_counters = {
  receipts : int;
  effects_total : int;
  bindings_matched_total : int;
  pending_auth_invalidated_total : int;
  secrets_destroyed_total : int;
  leases_invalidated_total : int;
  lineages_broken_total : int;
  remote_attempted_total : int;
  remote_succeeded_total : int;
  remote_failed_total : int;
  kind_counts : (string * int) list;
  remote_mode_counts : (string * int) list;
}

val empty_revocation_outcome_counters : revocation_outcome_counters

val revocation_outcome_counters_of_receipts :
  Github_user_auth_invalidate.receipt list -> revocation_outcome_counters

(** {1 Attribution deny class counters} *)

type attribution_deny_counters = {
  observations : int;
  by_failure_class : (string * int) list;
  by_result_kind : (string * int) list;
  by_record_kind : (string * int) list;
  repair_pending_count : int;
  deny_count : int;
  fallback_app_count : int;
  reconfirm_count : int;
}

val empty_attribution_deny_counters : attribution_deny_counters

val attribution_deny_counters_of_records :
  Github_attribution_audit.t list -> attribution_deny_counters

(** {1 Combined counters / status snapshot} *)

type counters = {
  generated_at : string;
  schema_version : int;
  readiness : readiness_counters;
  bindings : binding_state_counters;
  refresh : refresh_outcome_counters;
  revocation : revocation_outcome_counters;
  attribution_deny : attribution_deny_counters;
  class_metrics : class_metrics;
  status : status_entry list;
      (** Recent actionable status entries (newest first when folded from
          sources). Cap is caller-controlled. *)
  notes : string list;
}

val empty_counters : ?now:float -> unit -> counters
val with_readiness : counters -> readiness_counters -> counters
val with_bindings : counters -> binding_state_counters -> counters
val with_refresh : counters -> refresh_outcome_counters -> counters
val with_revocation : counters -> revocation_outcome_counters -> counters
val with_attribution_deny : counters -> attribution_deny_counters -> counters
val with_class_metrics : counters -> class_metrics -> counters
val with_status : counters -> status_entry list -> counters
val with_notes : counters -> string list -> counters

val merge_readiness :
  readiness_counters -> readiness_counters -> readiness_counters

val merge_bindings :
  binding_state_counters -> binding_state_counters -> binding_state_counters

val merge_refresh :
  refresh_outcome_counters ->
  refresh_outcome_counters ->
  refresh_outcome_counters

val merge_revocation :
  revocation_outcome_counters ->
  revocation_outcome_counters ->
  revocation_outcome_counters

val merge_attribution_deny :
  attribution_deny_counters ->
  attribution_deny_counters ->
  attribution_deny_counters

val merge_counters : counters -> counters -> counters
(** Per-section merge. [generated_at] and [notes] prefer the left-hand value;
    [status] concatenates left then right. *)

(** {1 Convenience snapshots} *)

val of_readiness_snapshots :
  counters -> Github_user_auth_readiness.readiness list -> counters

val of_redacted_accounts :
  counters -> Github_account_admin_surface.redacted_account list -> counters

val of_refresh_outcomes :
  counters -> Github_user_token_refresh.outcome list -> counters

val of_refresh_denials :
  counters -> Github_user_token_refresh.denial list -> counters

val of_refresh_flights :
  counters -> Github_user_token_refresh.flight list -> counters

val of_revocation_receipts :
  counters -> Github_user_auth_invalidate.receipt list -> counters

val of_attribution_audit_records :
  counters -> Github_attribution_audit.t list -> counters

val of_delivery_refuses :
  counters -> Github_user_auth_delivery.refuse_error list -> counters

val of_authorize_denies :
  counters -> Github_attribution_authorize.deny list -> counters

(** {1 Redacted JSON / text exports} *)

val readiness_counters_to_json : readiness_counters -> Yojson.Safe.t
val binding_state_counters_to_json : binding_state_counters -> Yojson.Safe.t
val refresh_outcome_counters_to_json : refresh_outcome_counters -> Yojson.Safe.t

val revocation_outcome_counters_to_json :
  revocation_outcome_counters -> Yojson.Safe.t

val attribution_deny_counters_to_json :
  attribution_deny_counters -> Yojson.Safe.t

val to_json : counters -> Yojson.Safe.t

val of_json : Yojson.Safe.t -> (counters, string) result
(** Parse a previously exported JSON snapshot. Rejects payloads that contain
    forbidden secret-shaped keys (e.g. ["access_token"], ["refresh_token"],
    ["client_secret"], ["vault_ref"], ["device_code"], ["user_code"]). *)

val readiness_counters_format : readiness_counters -> string list
val binding_state_counters_format : binding_state_counters -> string list
val refresh_outcome_counters_format : refresh_outcome_counters -> string list

val revocation_outcome_counters_format :
  revocation_outcome_counters -> string list

val attribution_deny_counters_format : attribution_deny_counters -> string list

val format_diagnostics : counters -> string list
(** Channel-safe human-readable lines (same data as JSON, no secrets). *)

val format_status : status_entry list -> string list
(** Format only status entries with guidance. *)

(** {1 Redaction helpers (test / public contract)} *)

val counters_contains_plaintext : counters -> plaintext:string -> bool

val readiness_contains_plaintext :
  readiness_counters -> plaintext:string -> bool

val binding_state_contains_plaintext :
  binding_state_counters -> plaintext:string -> bool

val refresh_outcome_contains_plaintext :
  refresh_outcome_counters -> plaintext:string -> bool

val revocation_outcome_contains_plaintext :
  revocation_outcome_counters -> plaintext:string -> bool

val attribution_deny_contains_plaintext :
  attribution_deny_counters -> plaintext:string -> bool

val status_contains_plaintext : status_entry list -> plaintext:string -> bool

val json_contains_plaintext : json:Yojson.Safe.t -> plaintext:string -> bool
(** Walk a JSON tree for a plain substring. *)
