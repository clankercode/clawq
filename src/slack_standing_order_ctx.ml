type t = {
  limit : channel_id:string -> unit Lwt.t;
  accepting : unit -> bool;
  now : unit -> float;
}
(** Shared worker dependencies. The worker supplies one limiter to all of its
    operations; all scheduling uses the store clock. *)

let make ~limit ~accepting () =
  { limit; accepting; now = Slack_standing_order_store.now }
