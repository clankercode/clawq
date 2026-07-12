(** Migrate legacy per-PR subscriptions into Item routes (P19.M2.E2.T005).

    Idempotently translates every legacy subscription (events, Room/profile
    binding, enabled state, audit/backlink references, PR identity) into a
    [Github_route_store] Item route with destination [Room] and selector
    [Item {kind=`Pull_request; ...}].

    Collision policy (documented winner):
    - [Prefer_existing_route] (default): keep any existing *active* route for
      the same destination+selector; among legacy-only collisions pick newest by
      [created_at] then [id].
    - [Prefer_legacy]: supersede an existing active route with the migrated
      winner ([on_collision:`Replace]).
    - [Prefer_newest]: compare existing active route vs legacy winner by
      [created_at]; keep the newer.

    Never leaves multiple active routes for the same destination+selector.
    Re-running the same legacy list is idempotent (no second active route).
    Provenance sets [created_via="migrate"]. Compatibility CLI aliases document
    delegation to route-store APIs after cutover — no dual-write. *)

type legacy_subscription = {
  id : string;
  room_id : string;
  repo_full_name : string;
  pr_number : int;
  enabled : bool;
  events : string list;
      (** mapped to [event_filter.include_events]; empty = baseline defaults *)
  profile_id : string option;
  backlink_ref : string option;
  audit_ref : string option;
  created_at : string option;
}

type collision_policy =
  | Prefer_existing_route
  | Prefer_legacy
  | Prefer_newest
      (** Documented winner. Default Prefer_existing_route if route exists; else
          Prefer_newest among legacy. *)

type resolution =
  | Created of Github_route_store.t
  | Updated of Github_route_store.t
  | Skipped of { reason : string; winner_route_id : string option }
  | Collided of { winner : Github_route_store.t; losers : string list }

type migrate_report = {
  resolutions : (legacy_subscription * resolution) list;
  active_routes : int;
}

val migrate_subscriptions :
  db:Sqlite3.db ->
  legacy:legacy_subscription list ->
  ?policy:collision_policy ->
  ?now:float ->
  unit ->
  (migrate_report, string) result
(** Idempotent: re-running same legacy list does not create duplicate active
    routes. Maps each legacy PR sub → Item selector
    [{repo, Pull_request, number}], destination Room. Events →
    [event_filter.include_events]; enabled preserved;
    [provenance.created_via="migrate"]. Collision with existing active route for
    same dest+selector: apply policy, record resolution. *)

val load_legacy_from_db :
  db:Sqlite3.db -> (legacy_subscription list, string) result
(** Read from [github_pr_subscriptions] if the table is present; else empty.
    Notification preference booleans are mapped to event name tokens. *)

val legacy_of_subscription :
  Github_pr_subscriptions.subscription -> legacy_subscription
(** Convert a live PR subscription row into the migration input shape. *)

val events_of_notification_preferences :
  Github_pr_subscriptions.notification_preferences -> string list
(** Map legacy [on_*] booleans to include-event tokens. All-default yields []
    (baseline allow-all). *)

val compatibility_cli_aliases : unit -> (string * string) list
(** e.g. [["subscriptions add", "github route item add"]; ...]. Documentation
    helpers: alias means "call route store APIs" — no dual-write. *)

val route_id_for_legacy : legacy_subscription -> string
(** Deterministic route id used for migrate inserts ([ghroute_migrate_...]). *)

val selector_of_legacy : legacy_subscription -> Github_route_store.selector

val destination_of_legacy :
  legacy_subscription -> Github_route_store.destination
