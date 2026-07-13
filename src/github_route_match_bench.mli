(** Org-scale route matching budgets and pure measurement helpers
    (P20.M1.E2.T003).

    Documents agreed candidate / match / enrichment cost budgets for indexed
    advanced matching. All measurements are pure and deterministic with
    injectable fetchers — no live network. Budget checks never change match
    semantics; they only count candidates and enrichment invocations around
    [Github_route_match_advanced].

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md. *)

(** {1 Documented budgets (agreed Org-scale contract)} *)

val org_scale_sibling_repo_routes : int
(** Number of non-matching sibling [Repo] routes in the synthetic Org-scale
    fixture. Index must not surface them as candidates. *)

val org_scale_max_candidates : int
(** Maximum [index_candidates] size for one target envelope in the synthetic
    fixture: one Item + one Repo + one Org route that apply (other-repo routes
    excluded). *)

val max_enrichment_fetches_per_cold_resolve : int
(** Maximum path+team fetcher invocations per cold resolve when both fields are
    demanded (one paths fetch + one teams fetch). *)

val max_enrichment_fetches_warm_cache : int
(** Maximum fetcher invocations on a warm enrichment cache for the same envelope
    \+ demanded filter (must be 0). *)

val max_match_eval_cost_units : int
(** Abstract per-resolve match cost units for the synthetic Org-scale scenario:
    candidate enumeration + baseline filter + advanced predicate evaluation.
    Documented ceiling so regressions in scan-all-routes matching fail the
    benchmark. One unit ≈ one candidate considered or one advanced field
    evaluation step. *)

(** {1 Measurement} *)

type costs = {
  candidates : int;
      (** Size of [index_candidates] for the envelope (0 when no index). *)
  path_fetches : int;
  team_fetches : int;
  match_cost_units : int;
      (** Deterministic cost estimate: candidates + advanced field steps. *)
  decision : Github_route_match.decision;
}

val count_advanced_field_steps : filter:Github_route_filter.t -> int
(** Number of configured advanced PR/Issue fields (each is one eval step). *)

val estimate_match_cost_units :
  candidates:int -> filter:Github_route_filter.t -> int
(** [candidates + count_advanced_field_steps filter] (+1 baseline step). *)

val measure_indexed_resolve :
  db:Sqlite3.db ->
  destination:Github_route_store.destination ->
  envelope:Github_event_envelope.t ->
  ?index:Github_route_match_advanced.route_index ->
  ?fetch_paths:Github_filter_enrichment.paths_fetch ->
  ?fetch_teams:Github_filter_enrichment.teams_fetch ->
  ?cache:Github_filter_enrichment.cache ->
  ?rate_limited:(unit -> bool) ->
  ?access_allowed:(unit -> bool) ->
  ?now:float ->
  unit ->
  costs
(** Build or reuse [index], count candidates, wrap injectable fetchers with
    counters, run [Github_route_match_advanced.resolve], and return costs.
    Semantics identical to advanced resolve; counters are pure side channels. *)

(** {1 Synthetic Org-scale scenario} *)

type org_scale_setup = {
  destination : Github_route_store.destination;
  index : Github_route_match_advanced.route_index;
  envelope : Github_event_envelope.t;
  target_repo_route_id : string;
  org_route_id : string;
  item_route_id : string option;
  sibling_repo_count : int;
}

val install_org_scale_routes :
  db:Sqlite3.db ->
  destination:Github_route_store.destination ->
  ?org:string ->
  ?target_repo:string ->
  ?sibling_repos:int ->
  ?include_item:bool ->
  ?target_filter:Github_route_filter.t ->
  ?org_filter:Github_route_filter.t ->
  ?now:float ->
  unit ->
  (org_scale_setup, string) result
(** Install 1 Org + 1 target Repo (+ optional Item) + [sibling_repos] other Repo
    routes for the same destination. Returns index + a PR envelope for the
    target repo. Pure store writes; no network. *)

val assert_costs_within_budget : costs -> (unit, string) result
(** Check [candidates], enrichment fetches, and match cost against documented
    budgets. Does not interpret the match decision. *)
