(* Reserve only at dispatch, so queued maintenance cannot pre-book live slots.
   No detached tasks: cancellation removes the ticket and cancels its timer. *)
include (
  struct
    open Lwt.Syntax

    type priority = Pause | Live | Maintenance
    type ticket = { priority : priority }

    type lane = {
      mutable next : float;
      mutable cooldown : float;
      mutable queue : ticket list;
    }

    type t = {
      now : unit -> float;
      wait : seconds:float -> unit Lwt.t;
      mutex : Lwt_mutex.t;
      changed : unit Lwt_condition.t;
      lanes : (string, lane) Hashtbl.t;
    }

    let create_with ~now ~wait =
      {
        now;
        wait;
        mutex = Lwt_mutex.create ();
        changed = Lwt_condition.create ();
        lanes = Hashtbl.create 16;
      }

    let create () =
      create_with ~now:Unix.gettimeofday ~wait:(fun ~seconds ->
          Lwt_unix.sleep seconds)

    let lane t key =
      match Hashtbl.find_opt t.lanes key with
      | Some lane -> lane
      | None ->
          let lane =
            { next = neg_infinity; cooldown = neg_infinity; queue = [] }
          in
          Hashtbl.add t.lanes key lane;
          lane

    let rank = function Pause -> 0 | Live -> 1 | Maintenance -> 2

    let first lane =
      List.fold_left
        (fun best ticket ->
          match best with
          | Some earlier when rank earlier.priority <= rank ticket.priority ->
              best
          | _ -> Some ticket)
        None lane.queue

    let reserve t keys priority =
      let ticket = { priority } in
      let lanes = List.map (lane t) keys in
      let rec attempt () =
        let* decision =
          Lwt_mutex.with_lock t.mutex (fun () ->
              let now = t.now () in
              let head =
                List.for_all
                  (fun lane ->
                    match first lane with
                    | Some x -> x == ticket
                    | None -> false)
                  lanes
              in
              let due =
                List.fold_left
                  (fun due lane -> max due (max lane.next lane.cooldown))
                  now lanes
              in
              if head && due <= now then (
                List.iter
                  (fun lane ->
                    lane.next <- now +. 1.;
                    lane.queue <- List.filter (fun x -> x != ticket) lane.queue)
                  lanes;
                Lwt_condition.broadcast t.changed ();
                Lwt.return `Done)
              else
                let changed = Lwt_condition.wait t.changed in
                Lwt.return
                  (if head then `Sleep (due -. now, changed)
                   else `Changed changed))
        in
        match decision with
        | `Done -> Lwt.return_unit
        | `Changed changed ->
            let* () = changed in
            attempt ()
        | `Sleep (seconds, changed) ->
            let timer =
              try t.wait ~seconds
              with exn ->
                Lwt.cancel changed;
                raise exn
            in
            let* () = Lwt.pick [ timer; changed ] in
            attempt ()
      in
      Lwt.finalize
        (fun () ->
          let* () =
            Lwt_mutex.with_lock t.mutex (fun () ->
                List.iter
                  (fun lane -> lane.queue <- lane.queue @ [ ticket ])
                  lanes;
                Lwt_condition.broadcast t.changed ();
                Lwt.return_unit)
          in
          attempt ())
        (fun () ->
          Lwt_mutex.with_lock t.mutex (fun () ->
              List.iter
                (fun lane ->
                  lane.queue <- List.filter (fun x -> x != ticket) lane.queue)
                lanes;
              Lwt_condition.broadcast t.changed ();
              Lwt.return_unit))

    let wrap t ~key ?(priority = Live) () =
      reserve t [ "method:" ^ key ] priority

    let post_pacer t ~channel () = reserve t [ "post:" ^ channel ] Live

    (* Both budgets must be reserved together: a 429 while waiting for a post
     slot must not release a call, nor collapse method/post spacing. *)
    let before_call t ~key ?channel ?(priority = Live) () =
      reserve t
        (("method:" ^ key)
        :: (match channel with None -> [] | Some c -> [ "post:" ^ c ]))
        priority

    let note_retry_after t ~key seconds =
      let seconds =
        if Float.is_finite seconds && seconds >= 0. then seconds else 60.
      in
      let lane = lane t ("method:" ^ key) in
      lane.cooldown <- max lane.cooldown (t.now () +. seconds);
      Lwt_condition.broadcast t.changed ()
  end :
    sig
      type t
      type priority = Pause | Live | Maintenance

      val create : unit -> t

      val create_with :
        now:(unit -> float) -> wait:(seconds:float -> unit Lwt.t) -> t

      val wrap : t -> key:string -> ?priority:priority -> unit -> unit Lwt.t
      val post_pacer : t -> channel:string -> unit -> unit Lwt.t

      val before_call :
        t ->
        key:string ->
        ?channel:string ->
        ?priority:priority ->
        unit ->
        unit Lwt.t

      val note_retry_after : t -> key:string -> float -> unit
    end)
