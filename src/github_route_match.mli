(** Destination-local Item > Repo > Org route matching (P19.M2.E2.T003).

    Resolution is per destination. Among routes whose selectors apply to an
    envelope, the most-specific configured selector class wins
    ([Item > Repo > Org]) *before* enabled/filter evaluation. A disabled or
    filter-rejected Item/Repo route therefore never falls through to a broader
    Org (or Repo) route — narrow routes can mute broader feeds.

    After specificity + filter evaluation, [resolve] yields at most one accepted
    ([Matched]) decision per call (destination + envelope).

    [try_accept] adds a durable idempotency ledger so one GitHub delivery plus
    canonical item yields at most one accepted routed event per destination. *)

type match_input = {
  destination : Github_route_store.destination;
  envelope : Github_event_envelope.t;
}

type specificity = [ `Item | `Repo | `Org ]

type decision =
  | Matched of { route : Github_route_store.t; specificity : specificity }
  | Muted of {
      route : Github_route_store.t;
      specificity : specificity;
      reason : string;  (** disabled or filter rejected *)
    }
  | No_route

val resolve :
  db:Sqlite3.db ->
  destination:Github_route_store.destination ->
  envelope:Github_event_envelope.t ->
  unit ->
  decision
(** Load candidate routes for [destination].

    1. Collect routes whose selector applies to [envelope] (Item if item
    number+kind+repo match; Repo if repo match; Org if org match). 2. Pick the
    most specific present selector class: Item > Repo > Org among candidates
    that exist in the store (enabled *or* disabled). If a more-specific route
    exists, less-specific ones are ignored (no fallthrough). 3. If the
    most-specific route is disabled or fails [filter_allows] → [Muted] (not a
    less-specific route). 4. If enabled and filter passes → [Matched]. 5. If no
    route at any level → [No_route]. *)

val filter_allows :
  Github_route_store.event_filter -> Github_event_envelope.t -> bool
(** Event and (for Org narrowing) repository filter evaluation.

    Semantics:
    - [exclude_events] always deny when the envelope event name *or* family
      string matches an entry (case-insensitive). Exclude always wins.
    - [include_events]: if non-empty, the event name or family must be listed;
      if empty, allow all events that are not excluded.
    - [exclude_repos] deny when [envelope.repo_full_name] matches
      (case-insensitive; primarily for Org routes).
    - [include_repos]: if non-empty, repo must be a member; if empty, all
      authorized repos are allowed (subject to excludes). *)

val selector_applies :
  Github_route_store.selector -> Github_event_envelope.t -> bool
(** Whether a route selector could apply to this envelope (identity match only;
    does not consider enabled/filter). *)

val specificity_of_selector : Github_route_store.selector -> specificity

val specificity_order : specificity list
(** Authoritative selector precedence, consumed by [resolve]. *)

val ensure_schema : Sqlite3.db -> unit
(** Idempotent ledger schema for accepted routed events. *)

val canonical_item_key : Github_event_envelope.t -> string
(** Stable item identity for idempotency. *)

type accept_result =
  | Accepted of decision
  | Duplicate of {
      delivery_id : string;
      item_key : string;
      route_id : string option;
    }
  | Not_accepted of decision

val try_accept :
  db:Sqlite3.db ->
  destination:Github_route_store.destination ->
  envelope:Github_event_envelope.t ->
  ?now:float ->
  ?item_key:string ->
  unit ->
  accept_result
(** [resolve] then, on [Matched], insert durable unique (destination,
    delivery_id, item key). [item_key] overrides [canonical_item_key] when set
    (used for transfer-stable identity). Duplicates yield [Duplicate]. *)
