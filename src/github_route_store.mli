(** Durable versioned GitHub Item/Repo/Org routes (P19.M2.E2.T002).

    Persists routes with Room-first or direct Session destinations,
    Item/Repo/Org selectors, baseline event/repository filters, comment mode,
    independent action capabilities, managed-access linkage, enabled state,
    revision, provenance, and indexes.

    Uniqueness: at most one *active* (enabled) route per
    [(destination, canonical selector)]. [create] with [~on_collision:`Reject]
    (default) fails transactionally on collision; [`Replace] disables the prior
    active route then inserts the new one (deterministic winner: the new route).

    Optimistic concurrency: [revision] is a monotonic counter string; [update]
    with [~expected_revision] fails when the stored revision differs.

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md. *)

type destination = Room of string | Session of string

type item_ref = {
  repo_full_name : string;
  kind : [ `Pull_request | `Issue ];
  number : int;
}

type selector =
  | Item of item_ref
  | Repo of string  (** owner/repo *)
  | Org of string

type comment_mode = Off | Summary | Threaded

type event_filter = Github_route_filter.t
(** Versioned baseline + advanced PR/Issue filter. See [Github_route_filter].
    Empty include lists still mean baseline allow-all; advanced predicates are
    typed (no raw JSON). *)

type capability_policy = {
  allow_reply : bool;
  allow_label : bool;
  allow_assign : bool;
  allow_review : bool;
  allow_merge : bool;
  allow_close : bool;
  extra : (string * bool) list;
}

type provenance = {
  created_by : string option;
  created_via : string option;  (** setup_plan | cli | migrate | system *)
  setup_plan_id : string option;
  notes : string option;
}

type t = {
  id : string;
  destination : destination;
  selector : selector;
  filter : event_filter;
  comment_mode : comment_mode;
  capability_policy : capability_policy;
  enabled : bool;
  revision : string;
  managed_bundle_id : string option;
  managed_feature_id : string option;
  provenance : provenance;
  created_at : string;
  updated_at : string;
}

val canonical_selector_key : selector -> string
(** Deterministic key e.g. ["item:owner/repo:pr:42"], ["repo:owner/repo"],
    ["org:acme"]. Repo/org segments are lowercased. *)

val destination_key : destination -> string
(** ["room:ID"] | ["session:KEY"] *)

val ensure_schema : Sqlite3.db -> unit

val create :
  db:Sqlite3.db ->
  ?id:string ->
  destination:destination ->
  selector:selector ->
  ?filter:event_filter ->
  ?comment_mode:comment_mode ->
  ?capability_policy:capability_policy ->
  ?enabled:bool ->
  ?managed_bundle_id:string ->
  ?managed_feature_id:string ->
  ?provenance:provenance ->
  ?now:float ->
  ?on_collision:[ `Reject | `Replace ] ->
  unit ->
  (t, string) result
(** Unique among *enabled* (active) routes for destination+canonical selector.
    On [~on_collision:`Replace] the new route supersedes the old (old
    [enabled=false]). [~on_collision:`Reject] (default) returns an error. *)

val update :
  db:Sqlite3.db ->
  id:string ->
  ?expected_revision:string ->
  ?filter:event_filter ->
  ?comment_mode:comment_mode ->
  ?capability_policy:capability_policy ->
  ?enabled:bool ->
  ?managed_bundle_id:string option ->
  ?managed_feature_id:string option ->
  ?now:float ->
  unit ->
  (t, string) result
(** Bumps [revision]. If [expected_revision] is set and mismatches the stored
    value, returns a conflict error. Enabling a route that would collide with
    another active route for the same destination+selector fails. *)

val get : db:Sqlite3.db -> id:string -> (t option, string) result

val list_for_destination :
  db:Sqlite3.db -> destination:destination -> (t list, string) result

val list_all : db:Sqlite3.db -> (t list, string) result
(** List every route in deterministic creation order. *)

val find_active :
  db:Sqlite3.db ->
  destination:destination ->
  selector:selector ->
  (t option, string) result

val default_filter : event_filter
val default_capability_policy : capability_policy

val default_comment_mode : comment_mode
(** [Summary] *)
