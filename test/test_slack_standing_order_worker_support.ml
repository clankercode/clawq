open Lwt.Syntax
open Test_slack_standing_order_exec_support
module Ctx = Slack_standing_order_ctx

let rec flush n =
  if n = 0 then Lwt.return_unit
  else
    let* () = Lwt.pause () in
    flush (n - 1)

type timer = { due : float; promise : unit Lwt.t; wake : unit Lwt.u }
type clock = { mutable now : float; mutable timers : timer list }

let clock () =
  let c = { now = Store.now (); timers = [] } in
  (Store.clock := fun () -> c.now);
  c

let wait c ~seconds =
  let promise, wake = Lwt.task () in
  c.timers <- { due = c.now +. seconds; promise; wake } :: c.timers;
  promise

let pending c = List.filter (fun t -> Lwt.is_sleeping t.promise) c.timers

let advance c seconds =
  c.now <- c.now +. seconds;
  List.iter
    (fun t ->
      if Lwt.is_sleeping t.promise && t.due <= c.now then Lwt.wakeup t.wake ())
    c.timers;
  c.timers <- pending c;
  flush 20

let context e c = { (ctx e) with Ctx.wait = wait c }

let cleanup c =
  let stranded = pending c in
  List.iter (fun t -> Lwt.cancel t.promise) stranded;
  Alcotest.(check int)
    "no stranded timers before cleanup" 0 (List.length stranded)
