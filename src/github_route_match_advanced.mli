(** Indexed and cached advanced route matching (P20.M1.E2.T002).

    Wraps destination-local [Github_route_match] (Item > Repo > Org,
    no-fallthrough) with typed advanced PR/Issue filter evaluation from
    [Github_route_filter_eval] {b before} a decision becomes [Matched].

    Enrichment for demanded changed-path / team fields is demand-driven via
    [Github_filter_enrichment]. Missing or incomplete enrichment never becomes
    allow (fail closed). Matching uses typed [Github_route_filter] predicates
    only — no raw JSON / free-form predicates.

    Optional destination-local index accelerates candidate lookup for Repo/Org
    (and Item) routes; an optional process-local index cache bounds rebuilds.

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md. *)

type decision = Github_route_match.decision
type accept_result = Github_route_match.accept_result
type specificity = Github_route_match.specificity

(** {1 Destination-local route index} *)

type route_index
(** Simple in-memory index of routes for one destination, keyed by normalized
    Item / Repo / Org selector identity. *)

val build_index :
  destination:Github_route_store.destination ->
  routes:Github_route_store.t list ->
  route_index
(** Build an index from an already-loaded route list. Routes for other
    destinations are ignored. *)

val build_index_from_db :
  db:Sqlite3.db ->
  destination:Github_route_store.destination ->
  (route_index, string) result
(** Load [list_for_destination] and [build_index]. *)

val index_destination : route_index -> Github_route_store.destination

val index_size : route_index -> int
(** Number of routes stored in the index. *)

val index_candidates :
  route_index -> envelope:Github_event_envelope.t -> Github_route_store.t list
(** Candidate routes whose selectors may apply to [envelope], looked up via
    Item/Repo/Org keys (then filtered with [selector_applies]). *)

(** {1 Index cache (process-local, TTL)} *)

type index_cache
(** Process-local cache of [route_index] values keyed by destination. *)

val default_index_ttl_s : float
(** Default index TTL (30s). *)

val create_index_cache : ?ttl_s:float -> unit -> index_cache

val get_or_build_index :
  cache:index_cache ->
  db:Sqlite3.db ->
  destination:Github_route_store.destination ->
  ?now:float ->
  unit ->
  (route_index, string) result
(** Return a non-expired cached index, or rebuild from the store. *)

val invalidate_index :
  cache:index_cache -> destination:Github_route_store.destination -> unit
(** Drop a destination entry (e.g. after route admin apply). *)

(** {1 Advanced filter evaluation} *)

val obtain_enrichment :
  filter:Github_route_filter.t ->
  envelope:Github_event_envelope.t ->
  ?enrichment:Github_filter_enrichment.enrichment ->
  ?fetch_paths:Github_filter_enrichment.paths_fetch ->
  ?fetch_teams:Github_filter_enrichment.teams_fetch ->
  ?cache:Github_filter_enrichment.cache ->
  ?now:float ->
  unit ->
  Github_filter_enrichment.enrichment
(** Use caller-supplied [enrichment], or demand-driven [enrich] when paths/teams
    are required. When nothing is demanded, returns [empty_enrichment]. *)

val advanced_allows :
  filter:Github_route_filter.t ->
  envelope:Github_event_envelope.t ->
  enrichment:Github_filter_enrichment.enrichment ->
  unit ->
  (unit, string) result
(** Evaluate typed advanced PR/Issue predicates only (baseline already handled
    by route match). [Ok ()] when advanced section is empty or all predicates
    pass. [Error reason] on reject, including fail-closed missing enrichment
    when demanded. *)

(** {1 Resolve / accept} *)

val resolve :
  db:Sqlite3.db ->
  destination:Github_route_store.destination ->
  envelope:Github_event_envelope.t ->
  ?enrichment:Github_filter_enrichment.enrichment ->
  ?fetch_paths:Github_filter_enrichment.paths_fetch ->
  ?fetch_teams:Github_filter_enrichment.teams_fetch ->
  ?cache:Github_filter_enrichment.cache ->
  ?index:route_index ->
  ?index_cache:index_cache ->
  ?now:float ->
  unit ->
  decision
(** Destination-local match with advanced filters.

    - Without [index] / [index_cache]: wraps [Github_route_match.resolve], then
      runs advanced evaluation before returning [Matched].
    - With [index] or [index_cache]: uses indexed candidate lookup, then the
      same Item > Repo > Org specificity, enabled, baseline, and advanced rules
      (no fallthrough).

    Advanced evaluation uses typed filters only. Demanded enrichment that is
    missing or incomplete yields [Muted] (fail closed), never a broader route.
*)

val try_accept :
  db:Sqlite3.db ->
  destination:Github_route_store.destination ->
  envelope:Github_event_envelope.t ->
  ?enrichment:Github_filter_enrichment.enrichment ->
  ?fetch_paths:Github_filter_enrichment.paths_fetch ->
  ?fetch_teams:Github_filter_enrichment.teams_fetch ->
  ?cache:Github_filter_enrichment.cache ->
  ?index:route_index ->
  ?index_cache:index_cache ->
  ?now:float ->
  ?item_key:string ->
  unit ->
  accept_result
(** [resolve] with advanced filters; on [Matched], durable accept ledger insert
    (same semantics as [Github_route_match.try_accept]). *)
