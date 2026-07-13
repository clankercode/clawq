(** Apply confirmed GitHub route setup plans and refresh managed Room access
    (P19.M2.E3.T003).

    Atomic apply gates on plan identity (id + digest), principal, base revision,
    destination, authority, and — for Org selectors — live App installation
    scope ([can_claim_org_scope]). PAT-only Org plans are refused with App
    migration guidance. On success, route ops run via
    [Github_route_admin.apply_route_ops], setup-owned managed bundle linkage is
    attached when the payload carries feature/bundle ids, and the same apply
    transaction durably queues affected Rooms for next-turn catalog refresh.

    Canonical: docs/plans/2026-07-12-github-item-room-routing.md. *)

type refresh_hook = room_id:string -> unit
(** Invoked after a successful apply for each affected Room so the
    already-active Room picks up an updated Tool catalog on its next turn (no
    daemon restart). *)

type apply_request = {
  plan_id : string;
  digest : string;
  principal : Setup_plan.principal;
  current_base_revision : string;
  destination_room : string option;
      (** Expected destination Room; defaults to the plan destination when
          omitted. Required for Room-targeted plans. *)
  destination_session : string option;
      (** Expected direct Session destination for a Session-targeted GitHub App
          setup plan. Room route plans remain Room-targeted. *)
  now : float;
  is_global_admin : bool;
  is_room_admin : room_id:string -> bool;
  auth_snapshot : Github_auth_selection.auth_snapshot option;
      (** Current GitHub auth dual-field snapshot for Org-scope gates. *)
  installation : Github_app_installation_scope.t option;
      (** Live App installation scope; required (Active) to claim Org routes. *)
}

type apply_outcome =
  | Applied of {
      receipt_id : string;
      route_ids : string list;
      catalog_refresh_rooms : string list;
    }
  | Rejected of { reason : string; message : string }

val apply_confirmed :
  db:Sqlite3.db ->
  ?on_catalog_refresh:refresh_hook ->
  apply_request ->
  apply_outcome
(** Confirm/apply a pending [Github_route] plan.

    Order: 1. Refuse if the plan targets an Org selector and
    [Github_auth_selection.can_claim_org_scope] is false (PAT or missing Active
    App installation), with a message that mentions App migration. 2. Call
    [Setup_plan_apply.apply] with [Github_route_admin.apply_route_ops] and an
    authority check derived from [is_global_admin] / [is_room_admin] (plus
    identity/revision/digest rechecks in the apply engine). 3. On first-time
    success, attach setup-owned bundle links when create/update ops carry
    [managed_bundle_id] + [managed_feature_id]; App setup carries an explicit
    Room bundle/feature attachment. Detach on disable/remove when the route
    holds managed linkage. 4. Persist affected Room catalog refresh requests
    before commit, then invoke [on_catalog_refresh] after success for optional
    observer notification.

    Atomicity relies on [Setup_plan_apply]'s [BEGIN IMMEDIATE] transaction and
    [Github_route_store] SAVEPOINT nesting for domain mutations. *)
