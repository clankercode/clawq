(** The channels the huddle observer watches (spec §6.2). *)

module C = Slack_huddle_classify
module Store = Slack_standing_order_store
module T = Slack_standing_order_types

type t = {
  observed : team:string -> app:string -> channel:string -> bool;
  huddles : team:string -> app:string -> channel:string -> bool;
  context : team:string -> app:string -> channel:string -> C.context;
}

let none =
  {
    observed = (fun ~team:_ ~app:_ ~channel:_ -> false);
    huddles = (fun ~team:_ ~app:_ ~channel:_ -> false);
    context = (fun ~team:_ ~app:_ ~channel:_ -> C.empty_context);
  }

(* Fail-closed: a failed read observes nothing, so no ledger row is written,
   and its context is empty, which only leaves replies unclassified. *)
let orders ?(tracking = true) db ~team ~app ~channel =
  try
    if not tracking then
      Store.observed_orders db ~team_id:team ~api_app_id:app ~channel_id:channel
    else begin
      Store.ensure_schema db;
      Store.query_orders db
        "WHERE team_id=? AND api_app_id=? AND channel_id=? AND (state IN \
         ('active','paused') OR EXISTS (SELECT 1 FROM \
         slack_standing_order_fires f JOIN slack_standing_order_dispatches d \
         ON d.fire_id=f.id WHERE f.order_id=slack_standing_orders.id AND \
         d.dispatched_at IS NOT NULL)) ORDER BY display_no"
        [ Store.text team; Store.text app; Store.text channel ]
    end
  with exn ->
    Logs.warn (fun m ->
        m "Slack huddle watch: standing orders for %s unreadable: %s" channel
          (Printexc.to_string exn));
    []

(* Every order stores clawq's bot user from its own validated [auth.test];
   orders that disagree (a reinstall between them) leave own relays
   unclassifiable rather than trusting either id. *)
let self_user_id ~channel (orders : T.order list) =
  match
    List.sort_uniq String.compare
      (List.filter_map (fun (o : T.order) -> o.bot_user_id) orders)
  with
  | [ bot_user_id ] -> Some bot_user_id
  | [] -> None
  | ids ->
      Logs.warn (fun m ->
          m
            "Slack huddle watch: standing orders in %s name %d different clawq \
             bot users; own relays there are unclassifiable"
            channel (List.length ids));
      None

let target_of (o : T.order) : C.target =
  let t = T.action_target o.action in
  {
    C.target_user_id = t.user_id;
    target_bot_id = t.bot_id;
    target_app_id = t.app_id;
  }

let distinct_targets orders =
  List.fold_left
    (fun acc o ->
      let t = target_of o in
      if List.mem t acc then acc else acc @ [ t ])
    [] orders

(** Active/paused orders permit huddle hints. Dispatched fires retain their
    targets even on inactive orders, so late pickup/notices stay observable.
    Construction is lazy: only invoking a watch function reads the store. *)
let of_db db =
  {
    observed = (fun ~team ~app ~channel -> orders db ~team ~app ~channel <> []);
    huddles =
      (fun ~team ~app ~channel ->
        orders ~tracking:false db ~team ~app ~channel <> []);
    context =
      (fun ~team ~app ~channel ->
        let orders = orders db ~team ~app ~channel in
        {
          C.self_user_id = self_user_id ~channel orders;
          targets = distinct_targets orders;
          matchers = Slack_standing_order_pickup.matchers;
        });
  }

(** Test seam only: production never assigns it. Phase 2 changes [of_db], not
    this ref. *)
let source = ref of_db
