open Lwt.Syntax
module Store = Slack_standing_order_store

type fake_time = {
  mutable time : float;
  previous : unit -> float;
  mutable waits : (float * unit Lwt.t * unit Lwt.u) list;
}

let fake_time ~now =
  let t = { time = now; previous = !Store.clock; waits = [] } in
  (Store.clock := fun () -> t.time);
  t

let pending_waits t =
  List.length (List.filter (fun (_, p, _) -> Lwt.is_sleeping p) t.waits)

let wait t ~seconds =
  let p, wake = Lwt.task () in
  t.waits <- (t.time +. seconds, p, wake) :: t.waits;
  p

let advance t ~seconds =
  t.time <- t.time +. seconds;
  let due, later =
    List.partition (fun (deadline, _, _) -> deadline <= t.time) t.waits
  in
  t.waits <- later;
  List.iter
    (fun (_, p, wake) -> if Lwt.is_sleeping p then Lwt.wakeup_later wake ())
    due;
  Lwt.pause ()

let with_fake_time ~now f =
  let t = fake_time ~now in
  Lwt.finalize
    (fun () -> f t)
    (fun () ->
      Store.clock := t.previous;
      let pending = List.map (fun (_, p, _) -> p) t.waits in
      List.iter Lwt.cancel pending;
      t.waits <- [];
      Lwt.join
        (List.map
           (fun p -> Lwt.catch (fun () -> p) (fun _ -> Lwt.return_unit))
           pending))
