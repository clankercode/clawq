type wait = seconds:float -> unit Lwt.t

let default_wait ~seconds = Lwt_unix.sleep seconds

type t = {
  limiter : Slack_read_limiter.t option;
  limit : channel_id:string -> unit Lwt.t;
  accepting : unit -> bool;
  now : unit -> float;
  wait : wait;
}
(** Shared worker dependencies. The worker supplies one limiter to all of its
    operations; all scheduling uses the store clock. *)

let make ?limiter ?(wait = default_wait) ~limit ~accepting () =
  { limiter; limit; accepting; now = Slack_standing_order_store.now; wait }
