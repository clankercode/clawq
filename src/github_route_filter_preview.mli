(** Filter preview and structured explain for GitHub routes (P20.M1.E2.T001).

    Dry-run of destination-local Item > Repo > Org resolution plus advanced
    filter evaluation {b without} delivery or accept-ledger writes. Surfaces:

    - winning selector
    - every evaluated predicate (baseline + advanced) with pass/fail + detail
    - enrichment source/status (paths/teams demanded vs ok/error/missing)
    - shadowed broader routes (no-fallthrough)
    - final accept/reject reason ([Matched] / [Muted] / [No_route])

    Output is redacted and key-stable for agent and CLI setup surfaces.

    Canonical: docs/plans/2026-07-12-github-item-room-routing.md. *)

type predicate_result = {
  name : string;
  passed : bool;
  detail : string;  (** Stable, redacted explanation of subject vs filter. *)
}

type preview = {
  destination : string;  (** [Github_route_store.destination_key]. *)
  winning_selector : string option;
      (** [canonical_selector_key] of the winning route, if any. *)
  decision : string;  (** ["Matched"] | ["Muted"] | ["No_route"]. *)
  final_reason : string;
  predicates : predicate_result list;
  enrichment_status : string list;
  shadowed : string list;
      (** Less-specific candidates, as [id:selector_key], sorted stably. *)
  no_fallthrough : bool;
      (** [true] when a most-specific winner exists (fallthrough suppressed). *)
}

val preview :
  db:Sqlite3.db ->
  destination:Github_route_store.destination ->
  envelope:Github_event_envelope.t ->
  ?enrichment:Github_filter_enrichment.enrichment ->
  unit ->
  preview
(** Resolve candidate routes for [destination] + [envelope], evaluate the
    winning route's filter predicates (baseline + advanced PR/Issue) using
    optional demand-driven [enrichment], and return a structured dry-run
    explain. Never writes delivery/accept state. *)

val to_json : preview -> Yojson.Safe.t
(** Stable key order; string values redacted via [Github_route_ops.redact_json].
*)

val format_lines : preview -> string list
(** Channel-safe one-line-per-field human summary (no secrets). *)
