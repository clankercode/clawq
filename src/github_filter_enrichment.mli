(** Demand-driven changed-path and team-membership enrichment for advanced
    GitHub route filters (P20.M1.E1.T002).

    Given a normalized envelope and the winning route filter, fetch external
    data only when the filter demands it:

    - paths when [Github_route_filter.requires_changed_paths]
    - team membership when [Github_route_filter.requires_team_membership]

    Core demand detection and result assembly are pure; network access is
    injected via [paths_fetch] / [teams_fetch]. Optional in-memory cache keys by
    installation / repo / item revision with a bounded TTL. Rate-limit and
    access-scope gates are also injectable.

    Enrichment failure never becomes a broad allow: demanded fields that are
    missing or fail are [Error] reasons, and [complete] is false so advanced
    predicates can fail closed. Canonical contract:
    docs/plans/2026-07-12-github-item-room-routing.md. *)

(** {1 Demand detection} *)

type demand = { need_paths : bool; need_teams : bool }

val demand_of_filter : Github_route_filter.t -> demand
(** Pure demand signals from [requires_changed_paths] /
    [requires_team_membership]. *)

val team_slugs_of_filter : Github_route_filter.t -> string list
(** Configured team values from PR and Issue [team] predicates (deduped,
    order-preserving). Empty when team membership is not demanded. *)

(** {1 Injectable fetchers} *)

type paths_fetch =
  envelope:Github_event_envelope.t -> (string list, string) result
(** Return changed file paths for the envelope item (PR). Stable short error
    codes preferred: [rate_limited], [access_denied], [not_a_pr], [unavailable],
    [truncated]. *)

type teams_fetch =
  envelope:Github_event_envelope.t ->
  team_slugs:string list ->
  (string list, string) result
(** Return the subset of [team_slugs] of which the envelope actor is a member.
    Prefer stable codes: [rate_limited], [access_denied], [missing_actor],
    [unavailable]. *)

(** {1 Enrichment result} *)

type enrichment = {
  paths : (string list, string) result option;
      (** [None] when paths not demanded; [Some (Ok paths)] or
          [Some (Error reason)] when demanded. *)
  teams : (string list, string) result option;
      (** [None] when teams not demanded; [Some (Ok slugs)] or
          [Some (Error reason)] when demanded. *)
  reasons : string list;
      (** Unavailable / partial / gate reasons for diagnostics (stable codes).
      *)
  complete : bool;
      (** True only when every demanded field is [Ok]. False ⇒ fail closed. *)
}

val empty_enrichment : enrichment
(** No fields demanded; [complete = true], empty reasons. *)

val demanded_ok : enrichment -> bool
(** Alias of [complete]: safe for advanced predicates to treat as fail-closed
    gate. *)

(** {1 Cache (installation / repo / item revision, bounded TTL)} *)

type cache
(** Process-local enrichment cache. Not shared across processes. *)

val default_ttl_s : float
(** Default cache TTL in seconds (60s). *)

val default_max_entries : int
(** Soft cap on cache entries (evicts oldest on insert when exceeded). *)

val create_cache : ?ttl_s:float -> ?max_entries:int -> unit -> cache

val cache_key_paths : Github_event_envelope.t -> string
(** Pure key: [paths:install:repo:number:revision]. *)

val cache_key_teams :
  Github_event_envelope.t -> team_slugs:string list -> string
(** Pure key: [teams:install:repo:actor:slugs…:revision]. *)

val item_revision : Github_event_envelope.t -> string
(** Item revision for cache identity: head SHA when present, else item number,
    else ["unknown"]. *)

(** {1 Enrich} *)

val enrich :
  filter:Github_route_filter.t ->
  envelope:Github_event_envelope.t ->
  ?fetch_paths:paths_fetch ->
  ?fetch_teams:teams_fetch ->
  ?cache:cache ->
  ?now:float ->
  ?rate_limited:(unit -> bool) ->
  ?access_allowed:(unit -> bool) ->
  unit ->
  enrichment
(** Demand-driven enrichment.

    - When a field is not demanded, the corresponding option is [None] and no
      fetcher is invoked for that field.
    - When demanded and [rate_limited] is true → [Error "rate_limited"].
    - When demanded and [access_allowed] is false → [Error "access_denied"].
    - When paths demanded but envelope is not a PR with a number →
      [Error "not_a_pr"] (no fetch).
    - When teams demanded but actor login missing → [Error "missing_actor"] (no
      fetch).
    - When demanded and no fetcher is provided → [Error "fetcher_unavailable"].
    - Cache hits skip the fetcher. TTL/max-entries bound memory. *)
