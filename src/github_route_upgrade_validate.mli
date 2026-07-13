(** Upgrade validation, drift checks, and admin guidance for GitHub routes
    (P20.M2.E2.T002).

    Validates filter schema versions, route/subscription migration readiness,
    managed linkage consistency, installation scope for Org routes, tool/MCP
    catalog state (injectable), and active Session refresh readiness. Drift
    checks compare runtime constants to documented defaults and surface
    deprecated compatibility aliases.

    Secrets never appear in reports; export always goes through
    [Github_route_ops.redact_json].

    Canonical: docs/plans/2026-07-12-github-item-room-routing.md. Operator:
    docs/github-route-operator-contract.md (Upgrade validation). *)

type severity = Pass | Warn | Fail

type category =
  | Schema
  | Migration
  | Managed
  | Installation
  | Catalog
  | Session
  | Drift
  | Alias

type check = {
  name : string;
  category : category;
  severity : severity;
  message : string;
  repair : string option;
}

type catalog_state = {
  tools_ok : bool;
  mcp_ok : bool;
  catalog_revision : string option;
  access_revision : string option;
}
(** Injectable tool/MCP catalog snapshot for a Room or global scope. *)

type session_refresh_state = {
  active_room_ids : string list;
      (** Rooms with an active Session that should pick up catalog changes. *)
  refresh_pending_room_ids : string list;
      (** Rooms marked for next-turn catalog refresh (no daemon restart). *)
  refresh_without_restart : bool;
      (** True when the runtime can refresh active Sessions without restart. *)
}
(** Injectable active-Session / next-turn catalog refresh readiness. *)

type report = {
  generated_at : string;
  overall : severity;
  checks : check list;
  filter_schema_current : int;
  envelope_version : int;
  routes_checked : int;
  legacy_subscription_count : int;
  deprecated_aliases : (string * string) list;
      (** [(legacy_cli, canonical_cli)] pairs still accepted as aliases. *)
  repair_guidance : string list;
  rollback_guidance : string list;
}

val documented_filter_schema_version : int
(** Documented product defaults used by drift checks (must match runtime). *)

val documented_envelope_version : int

val documented_default_comment_mode : string
(** Always ["summary"]. *)

val documented_comment_modes : string list
(** ["off"; "summary"; "threaded"]. *)

val documented_specificity_order : string
(** ["Item > Repo > Org"]. *)

val severity_to_string : severity -> string
val category_to_string : category -> string

val default_catalog_state : catalog_state
(** All-ok catalog with no revision metadata. *)

val default_session_refresh : session_refresh_state
(** Empty active set; [refresh_without_restart=true] (contract default). *)

val check_filter_schema : Github_route_filter.t -> check list
(** Pure schema-version checks for one filter. *)

val check_managed_linkage : Github_route_store.t -> check list
(** Pure managed bundle/feature consistency for one route. *)

val drift_checks :
  ?documented_filter_schema_version:int ->
  ?documented_envelope_version:int ->
  ?documented_default_comment_mode:string ->
  unit ->
  check list
(** Compare runtime constants to documented defaults. Injectable documented
    values are for tests that force drift. *)

val deprecated_alias_checks : unit -> check list * (string * string) list
(** Surface compatibility CLI aliases and require they map to route store APIs
    (no dual-write). *)

val repair_guidance_lines : check list -> string list
(** Actionable repair lines from Fail/Warn checks. *)

val rollback_guidance_lines : unit -> string list
(** Standard rollback steps for a failed upgrade / migration cutover. *)

val validate :
  db:Sqlite3.db ->
  ?destination:Github_route_store.destination ->
  ?installation:Github_app_installation_scope.t ->
  ?auth:Github_auth_selection.auth_snapshot ->
  ?catalog_state:catalog_state ->
  ?session_refresh:session_refresh_state ->
  ?now:float ->
  unit ->
  (report, string) result
(** Full upgrade validation over stored routes (optionally scoped), legacy
    subscriptions, installation scope, injectable catalog/session state, and
    drift/alias checks. *)

val to_json : report -> Yojson.Safe.t
(** Stable key order; always redacted. *)

val format_report : report -> string list
(** Channel-safe admin summary lines. *)
