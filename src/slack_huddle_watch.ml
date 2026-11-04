(** The channels the huddle observer watches (spec §6.2). *)

type t = {
  observed : team:string -> app:string -> channel:string -> bool;
  context :
    team:string -> app:string -> channel:string -> Slack_huddle_classify.context;
}

let none =
  {
    observed = (fun ~team:_ ~app:_ ~channel:_ -> false);
    context =
      (fun ~team:_ ~app:_ ~channel:_ -> Slack_huddle_classify.empty_context);
  }

(** Phase 1 observes no channel. Phase 2 observes the channels with an active or
    paused standing order whose [team_id] and [api_app_id] match, with those
    orders' targets and clawq's bot user id as [context]. *)
let of_db (_ : Sqlite3.db) = none

(** Test seam only: production never assigns it. Phase 2 changes [of_db], not
    this ref. *)
let source = ref of_db
