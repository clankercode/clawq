open Lwt.Syntax
module Schedule = Slack_standing_order_schedule
module Ctx = Slack_standing_order_ctx

let cancel p =
  Lwt.cancel p;
  Lwt.catch
    (fun () -> p)
    (function Lwt.Canceled -> Lwt.return_unit | exn -> Lwt.fail exn)

let with_resolution_lease ~ctx ~db initial work =
  let current = ref initial and lost = ref false in
  let lease = { initial with Schedule.until = Float.max_float } in
  let accepting () =
    ctx.Ctx.accepting () && (not !lost)
    && Schedule.owns_resolution ~db ~lease ~now:(ctx.now ())
  in
  let rec renew () =
    let* () = ctx.wait ~seconds:30. in
    match Schedule.renew_resolution ~db ~lease:!current ~now:(ctx.now ()) with
    | Some next ->
        current := next;
        renew ()
    | None ->
        lost := true;
        Lwt.return_unit
  in
  let heartbeat =
    Lwt.catch renew (function
      | Lwt.Canceled -> Lwt.return_unit
      | _ ->
          lost := true;
          Lwt.return_unit)
  in
  Lwt.finalize
    (fun () -> work { ctx with Ctx.accepting } lease)
    (fun () -> cancel heartbeat)
