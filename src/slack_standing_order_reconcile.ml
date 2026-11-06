open Lwt.Syntax
module T = Slack_standing_order_types
module Ctx = Slack_standing_order_ctx
module Store = Slack_standing_order_store
module R = Slack_standing_order_recovery_store
module A = Slack_standing_order_admission
module Sql = Slack_standing_order_sql
module SO = Slack_standing_order_slack
module Grant = Slack_standing_order_grant
module Scan = Slack_standing_order_reconcile_scan
module Catchup = Slack_standing_order_catchup
module Exec = Slack_standing_order_exec
module Life = Slack_standing_order_reconcile_lifecycle
module Pages = Slack_standing_order_pages

let configure = Life.configure

let uncovered ~last_success ~now =
  if now -. last_success > 172800. then Some (last_success, now -. 172800.)
  else None

exception Slice_done
exception Owner_lost

let locks = Hashtbl.create 8
let owned = ref []

let lock db id =
  match
    List.find_opt
      (fun (other, key, _) -> other == db && key = id)
      (Option.value (Hashtbl.find_opt locks id) ~default:[])
  with
  | Some (_, _, m) -> m
  | None ->
      let m = Lwt_mutex.create () in
      Hashtbl.replace locks id
        ((db, id, m) :: Option.value (Hashtbl.find_opt locks id) ~default:[]);
      m

let connected_gap db (order : T.order) =
  let team, app, channel =
    (order.team_id, order.api_app_id, order.channel_id)
  in
  match
    Sql.query db
      "SELECT generation FROM slack_standing_order_recovery_gaps WHERE \
       installation=? AND team=? AND app=? AND channel=? AND connected=1"
      (A.scope db ~team ~app ~channel) (fun s -> Sql_util.int_column s 0)
  with
  | [ gap_generation ] ->
      Some
        { R.team_id = team; app_id = app; channel_id = channel; gap_generation }
  | _ -> None

let pending_batches db (order : T.order) =
  Sql.query db
    "SELECT id FROM slack_standing_order_recovery_batches WHERE order_id=? AND \
     epoch=? AND status<>'cancelled' ORDER BY rowid"
    Sql.[ text order.id; int order.epoch ]
    (fun s -> Sql_util.text_column s 0)
  |> List.filter_map (fun batch_id -> R.find_batch db ~batch_id)

let batches ~ctx ~db ~manager ?http ?http_response (order : T.order) =
  let ids =
    Sql.query db
      "SELECT f.id FROM slack_standing_order_recovery_candidates c JOIN \
       slack_standing_order_fires f ON f.order_id=c.order_id AND \
       f.root_ts=c.root_ts AND f.room_id=c.room_id AND \
       f.date_start=c.date_start WHERE c.order_id=? AND c.epoch=? AND \
       c.origin='recovery' AND c.batch_id IS NULL AND f.dispatched_at IS NULL \
       AND f.state IN ('detected','fetching','uploading') AND NOT \
       EXISTS(SELECT 1 FROM slack_standing_order_suppression s WHERE \
       s.order_id=c.order_id AND (s.root_ts=c.root_ts OR (s.room_id=c.room_id \
       AND s.date_start=c.date_start)))"
      Sql.[ text order.T.id; int order.epoch ]
      (fun s -> Sql_util.text_column s 0)
  in
  if ids <> [] then
    ignore (Catchup.prepare ~db ~order ~fire_ids:ids ~now:(ctx.Ctx.now ()));
  Lwt_list.iter_s
    (fun b ->
      let* () =
        Catchup.ensure_notice ~ctx ~db ~manager ?http ?http_response b
      in
      ignore (Catchup.release_due ~db ~batch_id:b.R.id ~now:(ctx.now ()));
      Lwt.return_unit)
    (pending_batches db order)

let record_gap db (order : T.order) previous now =
  Slack_standing_order_report.init db;
  match Option.bind previous (fun c -> c.R.last_success) with
  | None ->
      Sql.exec db
        "INSERT OR IGNORE INTO slack_order_coverage_origin \
         VALUES(?,'coverage_before_window_unknown')"
        [ Sql.text order.id ];
      Slack_standing_order_scan_store.status db ~order_id:order.id
        ~method_:"conversations.history" ~code:"coverage_before_window_unknown"
        ~resume_unit:""
  | Some last ->
      Option.iter
        (fun (interval_start, interval_end) ->
          (* Intent commits separately from report insertion. A restart must
             invalidate scan authority without losing the original interval.
             A later window adds only its uncovered tail, never a replacement. *)
          A.transaction db (fun () ->
              let watermark = Scan.timestamp interval_start in
              let prior =
                Sql.query db
                  "SELECT last FROM slack_order_gap_report_intents WHERE \
                   order_id=? AND watermark=? ORDER BY CAST(last AS REAL) DESC \
                   LIMIT 1"
                  Sql.[ text order.id; text watermark ]
                  (fun s -> Sql_util.text_column s 0)
                |> Sql.first
              in
              let segment_start = Option.value prior ~default:watermark in
              if float_of_string segment_start < interval_end then
                Sql.exec db
                  "INSERT OR IGNORE INTO slack_order_gap_report_intents \
                   VALUES(?,?,?,?)"
                  Sql.
                    [
                      text order.id;
                      text watermark;
                      text segment_start;
                      text (Scan.timestamp interval_end);
                    ]);
          Sql.query db
            "SELECT first,last FROM slack_order_gap_report_intents WHERE \
             order_id=? ORDER BY CAST(first AS REAL)"
            [ Sql.text order.id ]
            (fun s -> (Sql_util.text_column s 0, Sql_util.text_column s 1))
          |> List.iter (fun (segment_start, segment_end) ->
              let cause =
                Slack_standing_order_report.Reconcile_gap
                  (segment_start ^ "-" ^ segment_end)
              in
              Sql.exec db
                "INSERT OR IGNORE INTO \
                 slack_standing_order_reports(order_id,cause_key,period_key,status) \
                 VALUES(?,?,'once','pending')"
                Sql.
                  [
                    text order.id;
                    text (Slack_standing_order_report.cause_key cause);
                  ]))
        (uncovered ~last_success:last ~now)

let pass_with ~ctx ?http ?http_response ~db ~manager (original : T.order) =
  R.ensure_schema db;
  A.ensure_schema db;
  let mutex =
    lock db (original.team_id ^ original.api_app_id ^ original.channel_id)
  in
  Lwt_mutex.with_lock mutex (fun () ->
      match Store.find db original.id with
      | None -> Lwt.return_unit
      | Some order when order.state <> T.Active -> Lwt.return_unit
      | Some order -> (
          if not (Exec.local_gate ~db ~manager order ~now:(ctx.Ctx.now ())) then
            Lwt.return_unit
          else
            match connected_gap db order with
            | None -> Lwt.return_unit
            | Some gap -> (
                if not (A.due db order.id (ctx.now ())) then Lwt.return_unit
                else
                  let () =
                    List.iter
                      (fun (other, id, owner) ->
                        if other == db && id = order.id then
                          ignore
                            (R.yield_owner db ~order_id:id ~owner
                               ~now:(ctx.now ())))
                      !owned
                  in
                  match
                    R.claim db ~order_id:order.id
                      ~owner_id:(Store.new_id ~prefix:"reconcile")
                      ~now:(ctx.now ())
                  with
                  | None -> Lwt.return_unit
                  | Some owner ->
                      owned :=
                        (db, order.id, owner)
                        :: List.filter
                             (fun (other, id, _) ->
                               not (other == db && id = order.id))
                             !owned;
                      let config = Session.get_config manager in
                      let lost, lost_u = Lwt.task () in
                      let finished, finished_u = Lwt.task () in
                      let valid () =
                        ctx.accepting ()
                        && Session.get_config manager == config
                        && R.attempt_bool (fun () ->
                            R.owns db ~order_id:order.id ~owner
                              ~now:(ctx.now ())
                            && R.current_gap db gap ~connected:true)
                      in
                      let stop_lost () =
                        if Lwt.is_sleeping lost then
                          Lwt.wakeup_later_exn lost_u Owner_lost
                      in
                      let rec renew () =
                        if not (Lwt.is_sleeping finished) then Lwt.return_unit
                        else
                          let* tick =
                            Lwt.pick
                              [
                                (let+ () = ctx.wait ~seconds:30. in
                                 true);
                                (let+ () = Lwt.protected finished in
                                 false);
                              ]
                          in
                          if not tick then Lwt.return_unit
                          else if
                            valid ()
                            && R.renew db ~order_id:order.id ~owner
                                 ~now:(ctx.now ())
                          then renew ()
                          else (
                            stop_lost ();
                            Lwt.return_unit)
                      in
                      let heartbeat =
                        Lwt.catch renew (function
                          | Lwt.Canceled -> Lwt.return_unit
                          | _ ->
                              stop_lost ();
                              Lwt.return_unit)
                      in
                      let local = { ctx with Ctx.accepting = valid } in
                      let previous = R.load_coverage db ~order_id:order.id in
                      let mode =
                        if R.covered db order.id order.epoch then
                          R.Healthy_maintenance
                        else R.Gap_recovery
                      in
                      let oldest =
                        Scan.timestamp
                          (Scan.lower_bound
                             ~active_since:
                               (float_of_string (Option.get order.active_since))
                             ~now:(ctx.now ()))
                      in
                      let latest = Scan.timestamp (ctx.now ()) in
                      let run () =
                        if
                          not
                            (R.begin_scan db ~order ~owner ~mode ~gap ~oldest
                               ~latest ~now:(ctx.now ()))
                        then Lwt.fail Owner_lost
                        else
                          let before_send () =
                            if valid () then Ok ()
                            else Error SO.Dispatch_refused
                          in
                          let* grant =
                            Grant.evaluate ~db
                              ~current_config:(fun () ->
                                Session.get_config manager)
                              ?http ?http_response ?limiter:ctx.limiter
                              ~priority:Slack_read_limiter.Maintenance
                              ~limit:(fun () ->
                                ctx.limit ~channel_id:order.channel_id)
                              ~before_send ~now:(ctx.now ())
                              ~stage:
                                (Grant.Reconcile
                                   { reconcile_id = Scan.reconcile_id owner })
                              ~unit_label:"reconcile"
                              (Grant.subject_of_order order)
                          in
                          match grant with
                          | Error f ->
                              if valid () && f.cls = Grant.Hard then
                                ignore
                                  (Store.suspend_if_epoch db ~order_id:order.id
                                     ~epoch:order.epoch
                                     ~reason:(Grant.reason_code f.reason)
                                     ~now:(ctx.now ()));
                              if valid () then A.failed db order.id (ctx.now ());
                              Lwt.return_unit
                          | Ok v ->
                              let pages = ref 0 and cache = Hashtbl.create 20 in
                              let wrap call method_ params =
                                let window =
                                  method_ = "conversations.history"
                                  && List.assoc_opt "limit" params
                                     = Some (`Int 200)
                                in
                                let key =
                                  Yojson.Safe.to_string
                                    (`Assoc (List.remove_assoc "oldest" params))
                                in
                                let lower =
                                  match List.assoc_opt "oldest" params with
                                  | Some (`String ts) -> ts
                                  | _ -> ""
                                in
                                match
                                  if window then Hashtbl.find_opt cache key
                                  else None
                                with
                                | Some (oldest, result)
                                  when Slack_standing_order_provenance
                                       .compare_ts oldest lower
                                       <= 0 ->
                                    let filtered =
                                      Result.map
                                        (function
                                          | `Assoc fields ->
                                              `Assoc
                                                (List.map
                                                   (fun (k, v) ->
                                                     if k = "messages" then
                                                       ( k,
                                                         match v with
                                                         | `List xs ->
                                                             `List
                                                               (List.filter
                                                                  (fun m ->
                                                                    Slack_standing_order_provenance
                                                                    .compare_ts
                                                                      (Slack_huddles
                                                                       .field
                                                                         "ts" m)
                                                                      lower
                                                                    >= 0)
                                                                  xs)
                                                         | _ -> v )
                                                     else (k, v))
                                                   fields)
                                          | json -> json)
                                        result
                                    in
                                    Lwt.return filtered
                                | _ ->
                                    let page =
                                      List.mem method_
                                        [
                                          "conversations.history";
                                          "conversations.replies";
                                        ]
                                      && List.assoc_opt "limit" params
                                         = Some (`Int 200)
                                    in
                                    if page && !pages >= 20 then
                                      Lwt.fail Slice_done
                                    else (
                                      if page then incr pages;
                                      let pending = call method_ params in
                                      let* result =
                                        if not (Lwt.is_sleeping pending) then
                                          pending
                                        else
                                          Lwt.pick
                                            [
                                              pending;
                                              (let* () =
                                                 ctx.wait ~seconds:30.
                                               in
                                               Lwt.return (Error SO.Transport));
                                            ]
                                      in
                                      if window && Result.is_ok result then
                                        Hashtbl.replace cache key (lower, result);
                                      Lwt.return result)
                              in
                              let rec scan () =
                                Lwt.catch
                                  (fun () ->
                                    let* sweep =
                                      if R.phase db order.id = Some "pause" then
                                        Scan.sweep_with_grant_wrapped
                                          ~page_budget:max_int ~wrap ~db
                                          ~manager ~ctx:local ~owner
                                          ~validated:v ~order
                                      else Lwt.return (Ok ())
                                    in
                                    A.settle_pauses db;
                                    match sweep with
                                    | Error _ ->
                                        if valid () then
                                          A.failed db order.id (ctx.now ());
                                        Lwt.return_false
                                    | Ok () when not (valid ()) ->
                                        Lwt.return_false
                                    | Ok () -> (
                                        let* result =
                                          if
                                            R.phase db order.id = Some "history"
                                          then
                                            Scan.discover_wrapped
                                              ~page_budget:max_int ~wrap ~db
                                              ~manager ~ctx:local ~owner ~order
                                              ~call:v.call
                                          else Lwt.return (Ok ())
                                        in
                                        match result with
                                        | Error _ ->
                                            if valid () then
                                              A.failed db order.id (ctx.now ());
                                            Lwt.return_false
                                        | Ok () -> Lwt.return_true))
                                  (function
                                    | Slice_done ->
                                        pages := 0;
                                        let yielded = ctx.now () in
                                        if
                                          not
                                            (R.yield_owner db ~order_id:order.id
                                               ~owner ~now:yielded)
                                        then Lwt.fail Owner_lost
                                        else
                                          let* () =
                                            Lwt_mutex.unlock mutex;
                                            Lwt.finalize Lwt.pause (fun () ->
                                                Lwt_mutex.lock mutex)
                                          in
                                          if
                                            R.resume_owner db ~order_id:order.id
                                              ~owner ~now:yielded
                                            && valid ()
                                          then scan ()
                                          else Lwt.fail Owner_lost
                                    | exn -> Lwt.fail exn)
                              in
                              let* complete = scan () in
                              if (not complete) || not (valid ()) then
                                Lwt.return_unit
                              else (
                                (* Durable report intent is independent of scan cursors. *)
                                record_gap db order previous
                                  (float_of_string
                                     (Option.get
                                        (R.load_coverage db ~order_id:order.id))
                                       .latest);
                                if
                                  not
                                    (R.finish_scan db ~order_id:order.id ~owner
                                       ~now:(ctx.now ()))
                                then Lwt.return_unit
                                else if
                                  R.verify_gap db ~token:gap ~owner
                                    ~through:
                                      (Option.get
                                         (R.load_coverage db ~order_id:order.id))
                                        .latest ~now:(ctx.now ())
                                then (
                                  A.succeeded db order.id;
                                  let* () =
                                    batches ~ctx:local ~db ~manager ?http
                                      ?http_response order
                                  in
                                  Exec.wake ();
                                  Exec.pending_reports ~ctx:local ~db ~manager
                                    ?http ?http_response order)
                                else Lwt.return_unit)
                      in
                      Lwt.finalize
                        (fun () ->
                          Lwt.catch
                            (fun () ->
                              Lwt.pick [ SO.with_read_wait ctx.wait run; lost ])
                            (function
                              | Lwt.Canceled -> Lwt.fail Lwt.Canceled
                              | _ ->
                                  if valid () then
                                    A.failed db order.id (ctx.now ());
                                  Lwt.return_unit))
                        (fun () ->
                          if Lwt.is_sleeping finished then
                            Lwt.wakeup_later finished_u ();
                          Lwt.cancel lost;
                          (* A released batch retains its owner for the hold and dispatch.
                 The next tick renews it; a new pass re-adopts it only after coverage. *)
                          heartbeat))))

let pass ~db ~manager order =
  match !Life.configured with
  | Some c when c.db == db && c.manager == manager ->
      pass_with ~ctx:c.ctx ~db ~manager order
  | _ ->
      Lwt.fail_with
        "standing-order reconciler must be configured before ingress"

type worker = {
  signal : unit Lwt_condition.t;
  mutable pending : bool;
  mutable running : bool;
  mutable loop : unit Lwt.t;
  mutable timer : unit Lwt.t;
}

let worker : worker option ref = ref None

let trigger () =
  Option.iter
    (fun w ->
      w.pending <- true;
      Lwt_condition.broadcast w.signal ())
    !worker

let () = Life.enqueue := trigger

let start ~db ~manager =
  match (!worker, !Life.configured) with
  | Some _, _ -> ()
  | None, None -> invalid_arg "configure standing-order reconciler before start"
  | None, Some c ->
      if c.db != db || c.manager != manager then
        invalid_arg "reconciler context mismatch";
      let w =
        {
          signal = Lwt_condition.create ();
          pending = true;
          running = true;
          loop = Lwt.return_unit;
          timer = Lwt.return_unit;
        }
      in
      worker := Some w;
      let rec timer () =
        let* () = c.ctx.wait ~seconds:300. in
        if w.running then (
          trigger ();
          timer ())
        else Lwt.return_unit
      in
      let rec service () =
        let now = c.ctx.now () in
        let next_retry =
          Sql.query db
            "SELECT MIN(retry_at) FROM slack_order_reconcile_retry WHERE \
             retry_at>?"
            [ Sqlite3.Data.FLOAT now ] (fun s -> Sql.opt_float_column s 0)
          |> function
          | [ Some at ] -> max 0. (min 5. (at -. now))
          | _ -> 5.
        in
        let* () = c.ctx.wait ~seconds:next_retry in
        if not w.running then Lwt.return_unit
        else (
          List.iter
            (fun (other, id, owner) ->
              if
                other == db
                && R.phase db id = Some "complete"
                && R.owns db ~order_id:id ~owner ~now:(c.ctx.now ())
              then (
                ignore (R.renew db ~order_id:id ~owner ~now:(c.ctx.now ()));
                match Store.find db id with
                | None -> ()
                | Some order ->
                    List.iter
                      (fun b ->
                        ignore
                          (Catchup.release_due ~db ~batch_id:b.R.id
                             ~now:(c.ctx.now ())))
                      (pending_batches db order)))
            !owned;
          if
            Sql.int_query db
              "SELECT count(*) FROM slack_order_reconcile_retry WHERE \
               retry_at<=?"
              [ Sqlite3.Data.FLOAT (c.ctx.now ()) ]
            > 0
          then trigger ();
          service ())
      in
      let rec supervise work =
        Lwt.catch work (function
          | Lwt.Canceled when not w.running -> Lwt.return_unit
          | _ ->
              (* Closure precedes the first retry await, even on a service tick.
                 A failed durable close retains the local deny latch. *)
              (try Life.close c ~cause:"reconciler_failure" ~disconnect:false
               with _ -> ());
              Logs.warn (fun m ->
                  m "standing-order reconciler failed; retrying");
              let* () = c.ctx.wait ~seconds:5. in
              if w.running then (
                trigger ();
                supervise work)
              else Lwt.return_unit)
      in
      w.timer <- supervise (fun () -> Lwt.pick [ timer (); service () ]);
      let rec loop () =
        let* () =
          if w.pending then Lwt.return_unit else Lwt_condition.wait w.signal
        in
        if not w.running then Lwt.return_unit
        else (
          w.pending <- false;
          ignore (Life.scopes c);
          let* () = Lwt_list.iter_s (pass ~db ~manager) (Store.list db ()) in
          loop ())
      in
      w.loop <- supervise loop

let stop () =
  let configured = !Life.configured in
  let forget () =
    Option.iter
      (fun c ->
        owned := List.filter (fun (db, _, _) -> db != c.Life.db) !owned;
        Hashtbl.filter_map_inplace
          (fun _ entries ->
            match List.filter (fun (db, _, _) -> db != c.db) entries with
            | [] -> None
            | entries -> Some entries)
          locks)
      configured
  in
  match !worker with
  | None ->
      Fun.protect ~finally:forget Life.stop;
      Lwt.return_unit
  | Some w ->
      w.running <- false;
      Lwt.finalize
        (fun () ->
          Life.stop ();
          Lwt.return_unit)
        (fun () ->
          Lwt.cancel w.timer;
          Lwt.cancel w.loop;
          worker := None;
          forget ();
          let* () = Slack_standing_order_resolution_lease.cancel w.timer in
          Slack_standing_order_resolution_lease.cancel w.loop)
