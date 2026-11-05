open Lwt.Syntax
include Slack_standing_order_fetch
module Dispatch = Slack_standing_order_dispatch
module Pickup = Slack_standing_order_pickup
module Report = Slack_standing_order_report

(* One budget per worker, shared by every read, relay, repost and report.
   P3 conservatively paces all calls; P4 owns the shared 429-aware limiter. *)
let context ?(wait = Ctx.default_wait) ~accepting () =
  let next = Hashtbl.create 8 in
  let limit ~channel_id =
    let now = Store.now () in
    let due =
      max now (Option.value (Hashtbl.find_opt next channel_id) ~default:now)
    in
    Hashtbl.replace next channel_id (due +. 1.);
    if due > now then wait ~seconds:(due -. now) else Lwt.return_unit
  in
  Ctx.make ~wait ~limit ~accepting ()

let hard_reason code =
  let fixed =
    Grant.
      [
        Credentials_invalid;
        Identity_changed;
        Target_not_allowlisted;
        Grantor_not_allowed;
        Grantor_not_admin;
        Team_mismatch;
        App_mismatch;
        Bot_not_member;
        Private_policy_denied;
        Channel_shared;
        Channel_has_guests;
        Grantor_unavailable;
        Grantor_guest;
        Grantor_not_member;
        Target_unavailable;
        Target_not_member;
      ]
  in
  let methods =
    List.concat_map
      (fun m -> Grant.[ Missing_scope m; Egress_denied m ])
      SO.operations
  in
  List.find_opt
    (fun r -> Grant.reason_code r = code)
    (fixed @ methods
    @ List.map (fun n -> Grant.Tool_denied n) Grant.capability_names)
  |> function
  | Some _ as r -> r
  | None ->
      let prefix = "duplicate_order:SO-" in
      if String.starts_with ~prefix code then
        Option.bind
          (int_of_string_opt
             (String.sub code (String.length prefix)
                (String.length code - String.length prefix)))
          (fun n -> if n > 0 then Some (Grant.Duplicate_order n) else None)
      else None

let pending_reports ~ctx ~db ~manager ?http (order : T.order) =
  Report.init db;
  (* A pending row predating a reload can never regain permission on re-enable. *)
  if
    SO.capability_denied
      (Session.get_config manager)
      ~channel_id:order.channel_id
    <> None
  then (
    Sql.exec db
      "UPDATE slack_standing_order_reports SET status='local_only' WHERE \
       order_id=? AND status='pending'"
      [ Sql.text order.id ];
    Lwt.return_unit)
  else
    let rows =
      Sql.query db
        "SELECT cause_key,fire_id,period_key FROM slack_standing_order_reports \
         WHERE order_id=? AND status='pending'"
        [ Sql.text order.id ]
        (fun s ->
          ( Sql_util.text_column s 0,
            (match Sqlite3.column s 1 with
            | Sqlite3.Data.TEXT s -> Some s
            | _ -> None),
            Sql_util.text_column s 2 ))
    in
    let causes =
      Report.
        [
          Target_not_member;
          No_pickup;
          Not_delivered;
          Grantor_unauthorised;
          Posture_changed;
          Repeated_failures;
          Cap_hit;
          Transcript_unavailable;
          No_notes_hint;
          Reply_delayed;
        ]
    in
    let decode key =
      match List.find_opt (fun c -> Report.cause_key c = key) causes with
      | Some c -> Some c
      | None ->
          if String.starts_with ~prefix:"authority_refused:" key then
            Option.map
              (fun r -> Report.Authority_refused r)
              (hard_reason (String.sub key 18 (String.length key - 18)))
          else if String.starts_with ~prefix:"missing_scope:" key then
            Some
              (Report.Missing_scope (String.sub key 14 (String.length key - 14)))
          else if String.starts_with ~prefix:"reconcile_gap:" key then
            Some
              (Report.Reconcile_gap (String.sub key 14 (String.length key - 14)))
          else None
    in
    Lwt_list.iter_s
      (fun (key, fire_id, period) ->
        match decode key with
        | None -> Lwt.return_unit
        | Some cause ->
            Report.post_once ~ctx ~db ~manager ?http ?fire_id ~period order
              cause)
      rows

let report_order ~ctx ~db ~manager ?http (original : T.order) =
  match Store.find db original.id with
  | None -> Lwt.return_unit
  | Some order ->
      let* () =
        match order.suspended_reason with
        | Some code -> (
            match hard_reason code with
            | Some reason ->
                Report.post_once ~ctx ~db ~manager ?http order
                  (Report.of_hard_reason reason)
            | None -> Lwt.return_unit)
        | None -> Lwt.return_unit
      in
      let* () =
        if
          order.state = T.Paused
          && Pickup.failure_streak ~db ~order_id:order.id >= 3
        then
          Report.post_once ~ctx ~db ~manager ?http order
            Report.Repeated_failures
        else Lwt.return_unit
      in
      (* Recover a missed outcome report without reopening historical tracking.
         At most one representative of each of the five reportable states. *)
      let* () =
        Store.query_fires db
          "WHERE order_id=? AND state IN \
           ('no_notes','no_pickup','not_delivered','skipped_cap','acknowledged') \
           AND (state<>'acknowledged' OR reason='acknowledged_no_reply') AND \
           (dispatched_at IS NULL OR EXISTS (SELECT 1 FROM \
           slack_standing_order_dispatches d WHERE \
           d.fire_id=slack_standing_order_fires.id AND d.dispatched_at>=?)) \
           GROUP BY state HAVING rowid=MAX(rowid)"
          [
            Store.text order.id;
            Sqlite3.Data.FLOAT (ctx.Ctx.now () -. (30. *. 86400.));
          ]
        |> Lwt_list.iter_s (fun fire ->
            match Report.of_fire fire with
            | None -> Lwt.return_unit
            | Some cause ->
                Report.post_once ~ctx ~db ~manager ?http ~fire_id:fire.id order
                  cause)
      in
      pending_reports ~ctx ~db ~manager ?http order

let local_gate ~db ~manager (order : T.order) ~now =
  match
    SO.capability_denied
      (Session.get_config manager)
      ~channel_id:order.channel_id
  with
  | None -> true
  | Some name ->
      ignore
        (Store.suspend_if_epoch db ~order_id:order.id ~epoch:order.epoch
           ~reason:(Grant.reason_code (Grant.Tool_denied name))
           ~now);
      false

let account ~db ~(order : T.order) ~now =
  let latest_failure =
    Store.query_fires db
      "WHERE order_id=? AND state IN ('no_pickup','not_delivered','failed') \
       ORDER BY created_at DESC,rowid DESC LIMIT 1"
      [ Store.text order.id ]
    |> fun fires -> List.nth_opt fires 0
  in
  match latest_failure with
  | Some fire
    when fire.epoch = order.epoch
         && Pickup.failure_streak ~db ~order_id:order.id >= 3 ->
      ignore
        (Store.pause_if_epoch db ~order_id:order.id ~epoch:fire.epoch
           ~by:"automatic_failures" ~now)
  | _ -> ()

let process_fire ~ctx ~db ~manager ?http ?upload_http ?renew_wait ~allow_new
    ?(poll = true) (fire : T.fire) =
  with_lease ~ctx ~db ?renew_wait ~fire_id:fire.id (fun ctx lease ->
      let current () = Dispatch.current db fire.id in
      let* () =
        if not (ctx.Ctx.accepting ()) then Lwt.return_unit
        else (
          ignore (Pickup.drain db ~lease ~fire_id:fire.id ~now:(ctx.now ()));
          let r = current () in
          if not poll then (
            Dispatch.save ~ctx ~db ~lease ~before:r
              ~after:(Pickup.schedule db r ~complete:false ~now:(ctx.now ()));
            Lwt.return_unit)
          else if
            Option.fold ~none:false
              ~some:(fun due -> due > ctx.now ())
              r.next_attempt_at
          then Lwt.return_unit
          else if Store.sources_mutable r then
            let* fetched = fetch ~ctx ~db ~manager ?http ~lease r in
            match fetched with
            | Ok (Some payload) when ctx.accepting () ->
                let* () =
                  Dispatch.deliver ~ctx ~db ~manager ?http ?upload_http ~lease
                    (current ()) payload
                in
                if payload.transcript_unavailable then
                  Report.post_once ~ctx ~db ~manager ?http ~fire_id:fire.id
                    (Option.get (Store.find db fire.order_id))
                    Report.Transcript_unavailable
                else Lwt.return_unit
            | _ -> Lwt.return_unit
          else if r.fire.dispatched_at <> None then (
            let order = Option.get (Store.find db r.fire.order_id) in
            if Pickup.terminal_tracking r then
              Dispatch.save ~ctx ~db ~lease ~before:r
                ~after:
                  {
                    r with
                    next_attempt_at =
                      Some (ctx.now () +. Pickup.tracking_poll_seconds);
                  };
            let* call = Dispatch.track ~ctx ~db ~manager ?http r order in
            let* () =
              Pickup.confirm ~db ~call ~lease (current ()) ~now:(ctx.now ())
            in
            if
              ctx.accepting ()
              && List.mem (current ()).fire.state
                   T.[ Posted; Posting; Posting_unknown; Uploading ]
            then
              Dispatch.reconcile ~ctx ~db ~manager ?http ?upload_http ~allow_new
                ~lease (current ())
            else Lwt.return_unit)
          else Lwt.return_unit)
      in
      let r = current () in
      Option.iter
        (fun order -> account ~db ~order ~now:(ctx.now ()))
        (Store.find db fire.order_id);
      let cause =
        if r.fire.reason = Some "transcript_unavailable" then
          Some Report.Transcript_unavailable
        else
          match Option.bind r.fire.reason hard_reason with
          | Some reason -> Some (Report.of_hard_reason reason)
          | None -> Report.of_fire r.fire
      in
      match cause with
      | None -> Lwt.return_unit
      | Some cause ->
          Report.post_once ~ctx ~db ~manager ?http ~fire_id:fire.id
            (Option.get (Store.find db fire.order_id))
            cause)

let channel_locks = Hashtbl.create 8

let pass ~ctx ~db ~manager ?http ?upload_http ?renew_wait ?orders ?roots
    ?(held = []) ?(read_only = []) () =
  Pickup.init db;
  let orders =
    Option.value orders ~default:(Store.list db ~include_cancelled:true ())
  in
  let channels =
    List.map (fun (o : T.order) -> o.channel_id) orders
    |> List.sort_uniq String.compare
  in
  Lwt_list.iter_p
    (fun channel ->
      let lock =
        match Hashtbl.find_opt channel_locks channel with
        | Some lock -> lock
        | None ->
            let lock = Lwt_mutex.create () in
            Hashtbl.add channel_locks channel lock;
            lock
      in
      Lwt_mutex.with_lock lock (fun () ->
          (* Leave room for new arrivals even with a month of retained relays. *)
          let terminal_polled = ref false in
          Lwt_list.iter_s
            (fun (order : T.order) ->
              let work () =
                if not (ctx.Ctx.accepting ()) then Lwt.return_unit
                else
                  let () = account ~db ~order ~now:(ctx.now ()) in
                  let allowed () =
                    local_gate ~db ~manager order ~now:(ctx.now ())
                  in
                  let* () =
                    if allowed () && order.state = T.Active && roots <> Some []
                    then resolve_pending ~ctx ~db ~manager ?http ?roots order
                    else Lwt.return_unit
                  in
                  let* () =
                    Lwt_list.iter_s
                      (fun (r : Store.fire_runtime) ->
                        if ctx.accepting () && not (List.mem r.fire.id held)
                        then
                          process_fire ~ctx ~db ~manager ?http ?upload_http
                            ?renew_wait ~allow_new:false ~poll:false r.fire
                        else Lwt.return_unit)
                      (Pickup.queued db ~order_id:order.id)
                  in
                  let* () =
                    if ctx.accepting () && allowed () then
                      Lwt_list.iter_s
                        (fun (fire : T.fire) ->
                          if
                            (not (ctx.accepting () && allowed ()))
                            || List.mem fire.id held
                          then Lwt.return_unit
                          else
                            Lwt.catch
                              (fun () ->
                                process_fire ~ctx ~db ~manager ?http
                                  ?upload_http ?renew_wait
                                  ~allow_new:(not (List.mem fire.id read_only))
                                  fire)
                              (function
                                | Dispatch.Tracking_refused (tracked, failure)
                                  ->
                                    if failure.Grant.cls = Grant.Hard then
                                      ignore
                                        (Store.suspend_if_epoch db
                                           ~order_id:tracked.T.id
                                           ~epoch:tracked.epoch
                                           ~reason:
                                             (Grant.reason_code failure.reason)
                                           ~now:(ctx.now ()));
                                    Lwt.return_unit
                                | Dispatch.Stop | Lwt.Canceled ->
                                    Lwt.return_unit
                                | exn -> Lwt.fail exn))
                        (Store.due db ~now:(ctx.now ())
                        |> List.filter (fun (r : Store.fire_runtime) ->
                            r.fire.order_id = order.id
                            && not (List.mem r.fire.id held))
                        |> List.filter (fun r ->
                            if not (Pickup.terminal_tracking r) then true
                            else if !terminal_polled then false
                            else (
                              terminal_polled := true;
                              true))
                        |> List.map (fun (r : Store.fire_runtime) -> r.fire))
                    else Lwt.return_unit
                  in
                  ignore (allowed ());
                  report_order ~ctx ~db ~manager ?http order
              in
              Lwt.catch work (function
                | Lwt.Canceled | Dispatch.Stop -> Lwt.return_unit
                | _ ->
                    Logs.warn (fun m ->
                        m "standing-order executor pass failed for %s" order.id);
                    Lwt.return_unit))
            (List.filter (fun (o : T.order) -> o.channel_id = channel) orders)))
    channels

type worker = {
  signal : unit Lwt_condition.t;
  mutable running : bool;
  mutable loop : unit Lwt.t;
  jobs : (string, unit Lwt.t) Hashtbl.t;
  drain : unit Lwt.t;
  drain_wake : unit Lwt.u;
  mutable collect : unit -> unit;
}

let worker : worker option ref = ref None

let wake () =
  Option.iter
    (fun w ->
      w.collect ();
      Lwt_condition.broadcast w.signal ())
    !worker

let stop () =
  match !worker with
  | None -> Lwt.return_unit
  | Some w ->
      w.running <- false;
      (Slack_huddle_observer.wake := fun () -> ());
      (Slack_huddle_observer.posture_changed := fun ~channel:_ -> ());
      if Lwt.is_sleeping w.drain then Lwt.wakeup_later w.drain_wake ();
      Lwt_condition.broadcast w.signal ();
      let* () = w.loop in
      let* () = Lwt.join (Hashtbl.to_seq_values w.jobs |> List.of_seq) in
      (match !worker with
      | Some current when current == w -> worker := None
      | _ -> ());
      Lwt.return_unit

let start ~ctx ~db ~manager ?http ?upload_http () =
  match !worker with
  | Some _ -> ()
  | None ->
      let signal = Lwt_condition.create () in
      let drain, drain_wake = Lwt.wait () in
      let w =
        {
          signal;
          running = true;
          loop = Lwt.return_unit;
          jobs = Hashtbl.create 8;
          drain;
          drain_wake;
          collect = (fun () -> ());
        }
      in
      let accepting () = w.running && ctx.Ctx.accepting () in
      let cancellable pending =
        Lwt.finalize
          (fun () ->
            Lwt.pick
              [
                pending;
                (let* () = Lwt.protected drain in
                 Lwt.fail Lwt.Canceled);
              ])
          (fun () -> cancel pending)
      in
      let wait ~seconds = cancellable (ctx.wait ~seconds) in
      let local =
        {
          ctx with
          Ctx.accepting;
          wait;
          limit =
            (fun ~channel_id ->
              if not (accepting ()) then Lwt.fail Lwt.Canceled
              else
                let* () = cancellable (ctx.limit ~channel_id) in
                if accepting () then Lwt.return_unit else Lwt.fail Lwt.Canceled);
        }
      in
      let orders () = Store.list db ~include_cancelled:true () in
      let rows () =
        List.concat_map
          (fun (o : T.order) ->
            L.list_rows ~db ~team:o.team_id ~channel:o.channel_id
            |> List.map (fun r -> (o, r)))
          (orders ())
      in
      (* Provenance belongs to the installation/channel/root, never to a
         mutable row snapshot. Replays and late ready hints cannot promote a
         pre-start root, even if its ledger row is replaced or pruned. *)
      let started_at = ctx.now () in
      let origins = Hashtbl.create 32 in
      let keys (o : T.order) (r : L.row) =
        List.filter_map Fun.id
          [ r.root_ts; r.notes_thread_ts; r.thread_root_ts; r.canvas_thread_ts ]
        |> List.map (fun ts -> (o.team_id, o.api_app_id, o.channel_id, ts))
      in
      List.iter
        (fun (o, r) ->
          List.iter (fun key -> Hashtbl.replace origins key false) (keys o r))
        (rows ());
      let persisted =
        List.concat_map
          (fun (o : T.order) ->
            Store.list_fires db ~order_id:o.id ~limit:max_int)
          (orders ())
      in
      let read_only = List.map (fun (f : T.fire) -> f.id) persisted in
      let held =
        persisted
        |> List.filter (fun (f : T.fire) ->
            f.dispatched_at = None
            && List.mem f.state T.[ Detected; Fetching; Uploading ])
        |> List.map (fun (f : T.fire) ->
            Sql.exec db
              "UPDATE slack_standing_order_fires SET \
               reason='p4_recovery_held',next_attempt_at=NULL WHERE id=? AND \
               dispatched_at IS NULL AND (claim_owner IS NULL OR lease_until \
               <= ?)"
              [ Sql.text f.id; Sqlite3.Data.FLOAT (ctx.now ()) ];
            f.id)
      in
      w.collect <-
        (fun () ->
          List.iter
            (fun (o, (r : L.row)) ->
              let keys = keys o r in
              let eligible =
                accepting () && r.source = "event"
                && r.first_seen_at >= started_at
                && not
                     (List.exists
                        (fun key -> Hashtbl.find_opt origins key = Some false)
                        keys)
              in
              List.iter
                (fun key ->
                  if not eligible then Hashtbl.replace origins key false
                  else if not (Hashtbl.mem origins key) then
                    Hashtbl.add origins key eligible)
                keys)
            (rows ()));
      let tick () =
        let all = orders () in
        let channels =
          List.map (fun (o : T.order) -> o.channel_id) all
          |> List.sort_uniq String.compare
        in
        List.iter
          (fun channel ->
            if not (Hashtbl.mem w.jobs channel) then (
              let job =
                Lwt.finalize
                  (fun () ->
                    let* () = Lwt.pause () in
                    Lwt_list.iter_s
                      (fun (order : T.order) ->
                        let roots =
                          Hashtbl.to_seq origins |> List.of_seq
                          |> List.filter_map
                               (fun ((team, app, ch, ts), eligible) ->
                                 if
                                   eligible && team = order.team_id
                                   && app = order.api_app_id
                                   && ch = order.channel_id
                                 then Some ts
                                 else None)
                        in
                        pass ~ctx:local ~db ~manager ?http ?upload_http
                          ~renew_wait:ctx.wait ~roots ~held ~read_only
                          ~orders:[ order ] ())
                      (List.filter
                         (fun (o : T.order) -> o.channel_id = channel)
                         all))
                  (fun () ->
                    Hashtbl.remove w.jobs channel;
                    Lwt.return_unit)
              in
              Hashtbl.add w.jobs channel job;
              Lwt.async (fun () -> job)))
          channels
      in
      let rec loop () =
        let timer = wait ~seconds:5. and event = Lwt_condition.wait signal in
        let* () =
          Lwt.finalize
            (fun () -> Lwt.pick [ timer; event ])
            (fun () ->
              let* () = cancel timer in
              cancel event)
        in
        if not w.running then Lwt.return_unit
        else (
          tick ();
          loop ())
      in
      worker := Some w;
      Slack_huddle_observer.wake := wake;
      (Slack_huddle_observer.posture_changed := fun ~channel:_ -> wake ());
      w.loop <-
        Lwt.catch loop (function
          | Lwt.Canceled -> Lwt.return_unit
          | _ ->
              Logs.warn (fun m ->
                  m "standing-order worker stopped unexpectedly");
              Lwt.return_unit)
