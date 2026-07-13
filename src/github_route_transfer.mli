(** Issue transfer route planning and accept deduplication (P19.M2.E2.T004).

    Transfers are evaluated against both source (old) and destination (new)
    repository/Org scope. Matching destinations are deduplicated so a Room that
    matches both scopes receives at most one accepted routed event, while
    distinct Rooms still fan out independently. *)

type room_id = string

type transfer_plan = {
  destinations : Github_route_store.destination list;
      (** Distinct destinations with a [Matched] decision on source and/or dest
          scope. *)
  per_destination :
    (Github_route_store.destination * Github_route_match.decision) list;
      (** Merged decision per distinct destination (Matched preferred; else
          Muted once). Pure [No_route] destinations are omitted. *)
}

val source_view : Github_event_envelope.t -> Github_event_envelope.t
(** Envelope with [repo_full_name]/[org] set to the transfer source repo. *)

val dest_view : Github_event_envelope.t -> Github_event_envelope.t
(** Envelope with [repo_full_name]/[org] set to the transfer destination repo.
*)

val transfer_stable_item_key : Github_event_envelope.t -> string
(** Canonical item key for transfer accepts: prefers [to_repo] issue identity so
    source+dest dual match cannot double-accept under different keys. *)

val plan_transfer :
  db:Sqlite3.db ->
  destinations:Github_route_store.destination list ->
  envelope:Github_event_envelope.t ->
  ?enrichment:Github_filter_enrichment.enrichment ->
  ?fetch_paths:Github_filter_enrichment.paths_fetch ->
  ?fetch_teams:Github_filter_enrichment.teams_fetch ->
  ?cache:Github_filter_enrichment.cache ->
  ?rate_limited:(unit -> bool) ->
  ?access_allowed:(unit -> bool) ->
  ?index:Github_route_match_advanced.route_index ->
  ?index_cache:Github_route_match_advanced.index_cache ->
  ?now:float ->
  unit ->
  transfer_plan
(** For [issues.transferred] envelopes:
    - Build source-view (repo=[from_repo]) and dest-view (repo=[to_repo])
    - For each distinct candidate destination, resolve both views
    - Destination is delivered when either view is [Matched]
    - Decision: prefer [Matched] from either; if only [Muted], record [Muted]
      once; omit pure [No_route] *)

val accept_transfer :
  db:Sqlite3.db ->
  destinations:Github_route_store.destination list ->
  envelope:Github_event_envelope.t ->
  ?enrichment:Github_filter_enrichment.enrichment ->
  ?fetch_paths:Github_filter_enrichment.paths_fetch ->
  ?fetch_teams:Github_filter_enrichment.teams_fetch ->
  ?cache:Github_filter_enrichment.cache ->
  ?rate_limited:(unit -> bool) ->
  ?access_allowed:(unit -> bool) ->
  ?index:Github_route_match_advanced.route_index ->
  ?index_cache:Github_route_match_advanced.index_cache ->
  ?now:float ->
  unit ->
  (Github_route_store.destination * Github_route_match.accept_result) list
(** [plan_transfer] then [try_accept] each matched destination with a
    transfer-stable item key so same-Room dual match produces at most one
    accept. Re-delivery of the same event yields [Duplicate] per
    already-accepted Room. *)
