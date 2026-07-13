(** Route and filter setup diagnostics with redacted export (P20.M2.E2.T001).

    Operator/admin surface that lists destination routes, filter schema version,
    predicate counts, readiness repair hints, App scope, delivery health, and
    optional match-preview explain — without secrets, raw webhook bodies, or
    private comment content.

    Canonical: docs/plans/2026-07-12-github-item-room-routing.md. *)

type predicate_counts = {
  include_events : int;
  exclude_events : int;
  include_repos : int;
  exclude_repos : int;
  pr_predicates : int;
  issue_predicates : int;
  advanced_total : int;
  baseline_total : int;
}

type route_export = {
  id : string;
  destination_key : string;
  selector_key : string;
  specificity : string;  (** ["item"] | ["repo"] | ["org"] *)
  enabled : bool;
  revision : string;
  comment_mode : string;
  filter_schema_version : int;
  predicate_counts : predicate_counts;
  has_advanced : bool;
  requires_changed_paths : bool;
  requires_team_membership : bool;
  managed_bundle_id : string option;
  managed_feature_id : string option;
  setup_plan_id : string option;
  created_via : string option;
}

type app_scope_export = {
  installation_id : int option;
  status : string;
      (** ["missing"] | ["active"] | ["suspended"] | ["deleted"] *)
  account_login : string option;
  selection : string option;
  scope_revision : string option;
}

type delivery_health = {
  pending : int;
  in_flight : int;
  succeeded : int;
  dead_letter : int;
  superseded : int;
  overall : string;
      (** ["healthy"] | ["degraded"] | ["unhealthy"] | ["error"] *)
}

type export = {
  exported_at : string;
  destination : string option;
      (** Destination key when scoped; [None] for all routes. *)
  current_filter_schema_version : int;
  routes : route_export list;
  route_count : int;
  enabled_count : int;
  plan_id : string option;
  plan_base_revision : string option;
  plan_digest : string option;
  catalog_revision : string option;
  catalog_access_revision : string option;
  app_scope : app_scope_export;
  delivery : delivery_health option;
  readiness_overall : string option;
  repair_hints : string list;
  winning_selector : string option;
  decision : string option;
  final_reason : string option;
  predicate_reasons : string list;
  enrichment_status : string list;
  diagnostics : string list;  (** Channel-safe admin lines (already redacted). *)
}

val count_predicates : Github_route_filter.t -> predicate_counts
(** Count baseline list sizes and configured advanced PR/Issue predicates. *)

val of_route : Github_route_store.t -> route_export
(** Safe route summary for export (ids, revisions, counts — no secrets). *)

val collect :
  db:Sqlite3.db ->
  ?destination:Github_route_store.destination ->
  ?installation:Github_app_installation_scope.t ->
  ?auth:Github_auth_selection.auth_snapshot ->
  ?plan:Setup_plan.t ->
  ?catalog_revision:string ->
  ?catalog_access_revision:string ->
  ?envelope:Github_event_envelope.t ->
  ?enrichment:Github_filter_enrichment.enrichment ->
  ?tools_granted:bool ->
  ?mcp_ok:bool ->
  ?credentials_ok:bool ->
  ?egress_ok:bool ->
  ?connector_ok:bool ->
  ?delivery_ok:bool ->
  ?now:float ->
  unit ->
  (export, string) result
(** Build a redacted diagnostics export.

    - Lists routes for [destination] or all routes when omitted.
    - Includes filter schema version + predicate counts per route.
    - Aggregates App readiness + repair hints when installation/auth/plan given.
    - Pulls delivery outbox health for Room destinations.
    - Optional [envelope] attaches filter-preview winning selector / predicates
      (dry-run only; never includes raw webhook bodies or comment content). *)

val to_json : export -> Yojson.Safe.t
(** Stable key order; always passed through [Github_route_ops.redact_json]. *)

val format_diagnostics : export -> string list
(** Admin diagnostic lines (same as [export.diagnostics]). *)
