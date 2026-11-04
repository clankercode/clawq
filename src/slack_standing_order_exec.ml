open Lwt.Syntax
(** Hint resolution and leased, content-free fetch scheduling. Content exists
    only in the returned verified payload, never in a database or reason. *)

module T = Slack_standing_order_types
module Store = Slack_standing_order_store
module SO = Slack_standing_order_slack
module Grant = Slack_standing_order_grant
module P = Slack_standing_order_provenance
module C = Slack_huddle_classify
module L = Slack_huddle_ledger
module Ctx = Slack_standing_order_ctx
module Sql = Slack_standing_order_sql

let valid_ts ts =
  T.valid_slack_ts ts
  && List.for_all (fun part -> part <> "") (String.split_on_char '.' ts)

let started_eligible ~(order : T.order) ~date_start ~fire_epoch =
  order.state = T.Active && order.epoch = fire_epoch && date_start >= 0
  &&
  match order.active_since with
  | Some ts when valid_ts ts -> P.compare_ts (string_of_int date_start) ts >= 0
  | _ -> false

let eligible ~order ~date_start ~notes_ready_ts ~fire_epoch =
  started_eligible ~order ~date_start ~fire_epoch
  && valid_ts notes_ready_ts
  && P.compare_ts notes_ready_ts (Option.get order.T.active_since) >= 0

let refresh_sources (r : Store.fire_runtime) ~canvas_id ~transcript_id
    ~notes_ready_ts =
  let next = { r with canvas_id; transcript_id; notes_ready_ts } in
  if not (Store.sources_changed r next) then next
  else
    {
      next with
      attempts =
        List.map
          (fun (a : Store.relay_attempt) ->
            { a with uploaded_file_ids = []; upload_started_at = None })
          r.attempts;
    }

(* A checkpoint keeps the end deadline and the two backoffs independent,
   including across worker restarts. No meeting content is stored here. *)
let ensure_fetch_schema db =
  Sql_util.exec_schema ~label:"standing_order_fetch" db
    "CREATE TABLE IF NOT EXISTS slack_standing_order_fetch (fire_id TEXT \
     PRIMARY KEY REFERENCES slack_standing_order_fires(id) ON DELETE CASCADE, \
     notes_deadline REAL NOT NULL, notes_retries INTEGER NOT NULL DEFAULT 0, \
     transient_retries INTEGER NOT NULL DEFAULT 0)"

let checkpoint db ~fire_id ~date_end ~now =
  ensure_fetch_schema db;
  let deadline = Option.fold ~none:now ~some:float_of_int date_end +. 3600. in
  Sql.exec db
    "INSERT INTO slack_standing_order_fetch(fire_id,notes_deadline) VALUES \
     (?,?) ON CONFLICT(fire_id) DO UPDATE SET \
     notes_deadline=MIN(notes_deadline,excluded.notes_deadline)"
    [ Sql.text fire_id; Sqlite3.Data.FLOAT deadline ]

let retry_state db ~fire_id =
  match
    Sql.query db
      "SELECT notes_deadline,notes_retries,transient_retries FROM \
       slack_standing_order_fetch WHERE fire_id=?"
      [ Sql.text fire_id ]
      (fun s ->
        ( Sqlite3.column_double s 0,
          Sql_util.int_column s 1,
          Sql_util.int_column s 2 ))
  with
  | [ s ] -> s
  | _ -> failwith "fetch checkpoint missing"

let backoff n = match n with 0 -> 120. | 1 -> 300. | 2 -> 600. | _ -> 1200.

let evaluate ~ctx ~db ~manager ?http ~stage (order : T.order) =
  let limit ~channel_id =
    if not (ctx.Ctx.accepting ()) then Lwt.fail Lwt.Canceled
    else
      let* () = ctx.limit ~channel_id in
      if ctx.accepting () then Lwt.return_unit else Lwt.fail Lwt.Canceled
  in
  Grant.evaluate ~db
    ~current_config:(fun () -> Session.get_config manager)
    ?http
    ~limit:(fun () -> limit ~channel_id:order.T.channel_id)
    ~now:(ctx.now ()) ~stage ~unit_label:"fire"
    (Grant.subject_of_order order)

let save ~ctx ~db ~lease ~(before : Store.fire_runtime) ~after =
  match Store.cas db ~lease ~before ~after ~now:(ctx.Ctx.now ()) with
  | Ok true -> Ok ()
  | Ok false -> Error "fetch_fence_lost"
  | Error _ -> Error "fetch_store_failed"

let finalise_ledger ~db (r : Store.fire_runtime) =
  match
    (Store.find db r.fire.order_id, C.seconds (`String r.fire.date_start))
  with
  | Some order, Some date_start ->
      L.set_verified ~db ~team:order.team_id ~channel:order.channel_id
        ~room_id:r.fire.room_id ~date_start ~transcript_file_id:r.transcript_id
        ~finalised:true
  | _ -> ()

let terminal ~ctx ~db ~lease (r : Store.fire_runtime) state reason =
  match
    save ~ctx ~db ~lease ~before:r
      ~after:
        {
          r with
          fire = { r.fire with state; reason = Some reason };
          next_attempt_at = None;
        }
  with
  | Error _ as e -> e
  | Ok () ->
      finalise_ledger ~db r;
      Ok ()

let retry ~ctx ~db ~lease (r : Store.fire_runtime) ~no_notes ~reason =
  let deadline, notes, transient = retry_state db ~fire_id:r.fire.id in
  let now = ctx.Ctx.now () in
  if no_notes && now >= deadline then
    terminal ~ctx ~db ~lease r T.No_notes "no_notes"
  else
    let next = now +. backoff (if no_notes then notes else transient) in
    let next = if no_notes then min deadline next else next in
    (* Count an attempted retry before scheduling it. A crash between these
       synchronous writes can only lengthen a backoff, never extend the deadline. *)
    let column = if no_notes then "notes_retries" else "transient_retries" in
    Sql.exec db
      ("UPDATE slack_standing_order_fetch SET " ^ column ^ "=MIN(3," ^ column
     ^ "+1) WHERE fire_id=?")
      [ Sql.text r.fire.id ];
    save ~ctx ~db ~lease ~before:r
      ~after:
        {
          r with
          fire = { r.fire with reason = Some reason };
          next_attempt_at = Some next;
        }

let refuse ~ctx ~db ~lease (r : Store.fire_runtime) (failure : Grant.failure) =
  let reason = Grant.reason_code failure.reason in
  match failure.cls with
  | Grant.Transient -> retry ~ctx ~db ~lease r ~no_notes:false ~reason
  | Hard -> (
      match
        Store.deny_and_suspend_if_epoch db ~lease ~before:r ~reason
          ~now:(ctx.Ctx.now ())
      with
      | Error _ -> Error "fetch_store_failed"
      | Ok false -> Error "fetch_fence_lost"
      | Ok true ->
          finalise_ledger ~db r;
          Ok ())
  | Inactive -> terminal ~ctx ~db ~lease r T.Skipped_inactive reason
  | Capped -> terminal ~ctx ~db ~lease r T.Skipped_cap reason

let read_root ~call ~channel ~root_ts =
  let+ response =
    P.request call "conversations.history"
      [
        ("channel", `String channel);
        ("oldest", `String root_ts);
        ("latest", `String root_ts);
        ("inclusive", `Bool true);
        ("limit", `Int 1);
      ]
  in
  Result.bind response (fun json ->
      match P.get "messages" json with
      | `List [ root ]
        when T.valid_slack_ts root_ts
             && P.field "ts" root = root_ts
             && P.same_channel channel root
             && P.field "call_family" (P.get "room" root) = "huddle" -> (
          match C.room_of_json (P.get "room" root) with
          | Some room
            when room.date_start <> None
                 && P.get "has_ended" (P.get "room" root) = `Bool true ->
              Ok (root, room)
          | _ -> Error (P.Rejected "root_mismatch"))
      | _ -> Error (P.Rejected "root_mismatch"))

let current_order ~db ~manager ~now (v : Grant.validated) (order : T.order) =
  if Session.get_config manager != v.config then None
  else
    match Store.find db order.id with
    | Some o
      when o.state = T.Active && o.epoch = order.epoch && Grant.unexpired o ~now
           && Store.activation_announced db ~order_id:o.id ->
        Some o
    | _ -> None

let resolve_pending ~ctx ~db ~manager ?http (order : T.order) =
  if not (ctx.Ctx.accepting ()) then Lwt.return_unit
  else
    let* grant = evaluate ~ctx ~db ~manager ?http ~stage:Grant.Resolve order in
    match grant with
    | Error { Grant.cls = Hard; reason } ->
        ignore
          (Store.suspend_if_epoch db ~order_id:order.id ~epoch:order.epoch
             ~reason:(Grant.reason_code reason) ~now:(ctx.now ()));
        Lwt.return_unit
    | Error _ -> Lwt.return_unit
    | Ok v ->
        let order = Option.get v.order in
        let rows =
          L.list_rows ~db ~team:order.team_id ~channel:order.channel_id
        in
        let roots =
          List.filter_map
            (fun (r : L.row) ->
              match r.kind with
              | L.Notes_stub -> r.notes_thread_ts
              | Room -> r.root_ts)
            rows
          |> List.sort_uniq String.compare
        in
        let rec resolve = function
          | [] -> Lwt.return_unit
          | _ when not (ctx.accepting ()) -> Lwt.return_unit
          | root_ts :: rest -> (
              let* root =
                read_root ~call:v.call ~channel:order.channel_id ~root_ts
              in
              if not (ctx.accepting ()) then Lwt.return_unit
              else
                match
                  (root, current_order ~db ~manager ~now:(ctx.now ()) v order)
                with
                | Ok (root, room), Some fresh
                  when started_eligible ~order:fresh
                         ~date_start:(Option.get room.date_start)
                         ~fire_epoch:order.epoch ->
                    let date_start = Option.get room.date_start in
                    L.record_seen ~db ~team:order.team_id
                      ~channel:order.channel_id ~source:"reconcile"
                      ~now:(ctx.now ()) ~root_ts ~file_ids:(P.file_ids root)
                      room;
                    let* () =
                      match
                        Store.insert_fire db ~order:fresh ~room_id:room.room_id
                          ~date_start:(string_of_int date_start) ~root_ts
                          ~now:(ctx.now ()) ()
                      with
                      | Ok (Some fire) -> (
                          checkpoint db ~fire_id:fire.id ~date_end:room.date_end
                            ~now:(ctx.now ());
                          match
                            Store.claim db ~fire_id:fire.id
                              ~owner:(Store.new_id ~prefix:"resolve")
                              ~now:(ctx.now ())
                          with
                          | Ok (Some lease) ->
                              Lwt.finalize
                                (fun () ->
                                  let r =
                                    Option.get
                                      (Store.runtime db ~fire_id:fire.id)
                                  in
                                  let after =
                                    { r with root_ts = Some root_ts }
                                  in
                                  match
                                    save ~ctx ~db ~lease ~before:r ~after
                                  with
                                  | Error _ -> Lwt.return_unit
                                  | Ok () ->
                                      checkpoint db ~fire_id:fire.id
                                        ~date_end:room.date_end
                                        ~now:(ctx.now ());
                                      let* fire_grant =
                                        evaluate ~ctx ~db ~manager ?http
                                          ~stage:
                                            (Grant.Fire
                                               {
                                                 fire_epoch = fire.epoch;
                                                 fire_id = fire.id;
                                               })
                                          fresh
                                      in
                                      (if ctx.accepting () then
                                         match fire_grant with
                                         | Ok _ -> ()
                                         | Error failure ->
                                             ignore
                                               (refuse ~ctx ~db ~lease after
                                                  failure));
                                      Lwt.return_unit)
                                (fun () ->
                                  Store.release db ~fire_id:fire.id ~lease
                                    ~now:(ctx.now ());
                                  Lwt.return_unit)
                          | _ -> Lwt.return_unit)
                      | Ok None -> Lwt.return_unit
                      | Error _ -> Lwt.fail_with "fire_insert_failed"
                    in
                    resolve rest
                | Error (P.Api { method_; error }), _ ->
                    let failure = P.grant_failure ~method_ error in
                    if failure.cls = Grant.Hard then
                      ignore
                        (Store.suspend_if_epoch db ~order_id:order.id
                           ~epoch:order.epoch
                           ~reason:(Grant.reason_code failure.reason)
                           ~now:(ctx.now ()));
                    Lwt.return_unit
                | _ -> resolve rest)
        in
        resolve roots

let transcript_conflict db ~fire_id id =
  match id with
  | None -> false
  | Some id ->
      Sql.query db
        "SELECT 1 FROM slack_standing_order_fires WHERE transcript_file_id=? \
         AND id<>?"
        [ Sql.text id; Sql.text fire_id ]
        ignore
      <> []

let fetch ~ctx ~db ~manager ?http ~lease (initial : Store.fire_runtime) =
  let run () =
    match Store.runtime db ~fire_id:initial.fire.id with
    | None -> Lwt.return (Error "fetch_missing")
    | Some r
      when (not (ctx.Ctx.accepting ()))
           || (not (Store.valid_lease lease r (ctx.now ())))
           || not (Store.sources_mutable r) ->
        Lwt.return (Ok None)
    | Some r -> (
        ensure_fetch_schema db;
        (* Legacy rows lack a checkpoint. First observation bounds an absent
           optional date_end; a verified end below tightens this bound. *)
        checkpoint db ~fire_id:r.fire.id ~date_end:None ~now:(ctx.now ());
        match Store.find db r.fire.order_id with
        | None ->
            Lwt.return
              (Result.map
                 (fun () -> None)
                 (terminal ~ctx ~db ~lease r T.Skipped_inactive "order_missing"))
        | Some order -> (
            let* grant =
              evaluate ~ctx ~db ~manager ?http
                ~stage:
                  (Grant.Fire { fire_epoch = r.fire.epoch; fire_id = r.fire.id })
                order
            in
            if not (ctx.accepting ()) then Lwt.return (Ok None)
            else
              match grant with
              | Error failure ->
                  Lwt.return
                    (Result.map
                       (fun () -> None)
                       (refuse ~ctx ~db ~lease r failure))
              | Ok v
                when not
                       (Option.fold ~none:false
                          ~some:(fun date_start ->
                            started_eligible ~order:(Option.get v.order)
                              ~date_start ~fire_epoch:r.fire.epoch)
                          (C.seconds (`String r.fire.date_start))) ->
                  Lwt.return
                    (Result.map
                       (fun () -> None)
                       (terminal ~ctx ~db ~lease r T.Skipped_inactive
                          "ineligible"))
              | Ok v -> (
                  let order = Option.get v.order in
                  let after =
                    if r.fire.state = T.Detected then
                      { r with fire = { r.fire with state = T.Fetching } }
                    else r
                  in
                  match save ~ctx ~db ~lease ~before:r ~after with
                  | Error _ as e -> Lwt.return e
                  | Ok () -> (
                      let latest = ref after in
                      let finish result =
                        let r = !latest in
                        let outcome =
                          match result with
                          | Error (P.Api { method_; error }) ->
                              refuse ~ctx ~db ~lease r
                                (P.grant_failure ~method_ error)
                          | Error P.Not_ready ->
                              retry ~ctx ~db ~lease r ~no_notes:true
                                ~reason:"notes_not_ready"
                          | Error (P.Rejected "transcript_conflict") ->
                              terminal ~ctx ~db ~lease r T.Unavailable
                                "transcript_conflict"
                          | Error (P.Rejected "ineligible") ->
                              terminal ~ctx ~db ~lease r T.Skipped_inactive
                                "ineligible"
                          | Error (P.Rejected "ready_stale") ->
                              retry ~ctx ~db ~lease r ~no_notes:false
                                ~reason:"ready_evidence_stale"
                          | Error (P.Unavailable | P.Rejected _) ->
                              terminal ~ctx ~db ~lease r T.Unavailable
                                "source_rejected"
                          | Ok _ -> assert false
                        in
                        Lwt.return (Result.map (fun () -> None) outcome)
                      in
                      let r = !latest in
                      match r.root_ts with
                      | None ->
                          Lwt.return
                            (Result.map
                               (fun () -> None)
                               (terminal ~ctx ~db ~lease r T.Unavailable
                                  "root_missing"))
                      | Some root_ts -> (
                          let* root =
                            read_root ~call:v.call ~channel:order.channel_id
                              ~root_ts
                          in
                          if not (ctx.accepting ()) then Lwt.return (Ok None)
                          else
                            match root with
                            | Error _ as e -> finish e
                            | Ok (_, room)
                              when room.room_id <> r.fire.room_id
                                   || C.seconds (`String r.fire.date_start)
                                      <> room.date_start ->
                                finish (Error (P.Rejected "root_mismatch"))
                            | Ok (_, room) -> (
                                checkpoint db ~fire_id:r.fire.id
                                  ~date_end:room.date_end ~now:(ctx.now ());
                                let before_content ~canvas_id ~transcript_id
                                    ~notes_ready_ts =
                                  let r = !latest in
                                  if
                                    not
                                      (eligible ~order
                                         ~date_start:
                                           (Option.get room.date_start)
                                         ~notes_ready_ts
                                         ~fire_epoch:r.fire.epoch)
                                  then Error (P.Rejected "ineligible")
                                  else if
                                    Option.fold ~none:false
                                      ~some:(fun previous ->
                                        valid_ts previous
                                        && P.compare_ts notes_ready_ts previous
                                           < 0)
                                      r.notes_ready_ts
                                  then Error (P.Rejected "ready_stale")
                                  else if
                                    transcript_conflict db ~fire_id:r.fire.id
                                      transcript_id
                                  then Error (P.Rejected "transcript_conflict")
                                  else
                                    let after =
                                      refresh_sources r
                                        ~canvas_id:(Some canvas_id)
                                        ~transcript_id
                                        ~notes_ready_ts:(Some notes_ready_ts)
                                    in
                                    match
                                      save ~ctx ~db ~lease ~before:r ~after
                                    with
                                    | Ok () ->
                                        latest := after;
                                        Ok ()
                                    | Error _ ->
                                        if
                                          transcript_conflict db
                                            ~fire_id:r.fire.id transcript_id
                                        then
                                          Error
                                            (P.Rejected "transcript_conflict")
                                        else Error P.Unavailable
                                in
                                let* verified =
                                  P.verify_with_source ~before_content
                                    ~call:v.call ~order ~room_id:r.fire.room_id
                                    ~date_start:r.fire.date_start ~root_ts
                                in
                                if not (ctx.accepting ()) then
                                  Lwt.return (Ok None)
                                else
                                  match verified with
                                  | Error _ as e -> finish e
                                  | Ok payload -> (
                                      let r = !latest in
                                      (* Revalidate after every content await. The fresh
                                   row is returned last, immediately before CAS. *)
                                      let* grant =
                                        evaluate ~ctx ~db ~manager ?http
                                          ~stage:
                                            (Grant.Fire
                                               {
                                                 fire_epoch = r.fire.epoch;
                                                 fire_id = r.fire.id;
                                               })
                                          order
                                      in
                                      if not (ctx.accepting ()) then
                                        Lwt.return (Ok None)
                                      else
                                        match grant with
                                        | Error failure ->
                                            Lwt.return
                                              (Result.map
                                                 (fun () -> None)
                                                 (refuse ~ctx ~db ~lease r
                                                    failure))
                                        | Ok fresh -> (
                                            let order =
                                              Option.get fresh.order
                                            in
                                            if
                                              not
                                                (eligible ~order
                                                   ~date_start:
                                                     payload.date_start
                                                   ~notes_ready_ts:
                                                     payload.notes_ready_ts
                                                   ~fire_epoch:r.fire.epoch)
                                            then
                                              Lwt.return
                                                (Result.map
                                                   (fun () -> None)
                                                   (terminal ~ctx ~db ~lease r
                                                      T.Skipped_inactive
                                                      "ineligible"))
                                            else
                                              let transcript_id =
                                                Option.map
                                                  (fun (t : P.transcript) ->
                                                    t.file_id)
                                                  payload.transcript
                                              in
                                              if
                                                transcript_conflict db
                                                  ~fire_id:r.fire.id
                                                  transcript_id
                                              then
                                                Lwt.return
                                                  (Result.map
                                                     (fun () -> None)
                                                     (terminal ~ctx ~db ~lease r
                                                        T.Unavailable
                                                        "transcript_conflict"))
                                              else
                                                let refreshed =
                                                  refresh_sources r
                                                    ~canvas_id:
                                                      (Some payload.canvas_id)
                                                    ~transcript_id
                                                    ~notes_ready_ts:
                                                      (Some
                                                         payload.notes_ready_ts)
                                                in
                                                let after =
                                                  {
                                                    refreshed with
                                                    canvas_id =
                                                      Some payload.canvas_id;
                                                    transcript_id;
                                                    notes_ready_ts =
                                                      Some
                                                        payload.notes_ready_ts;
                                                    next_attempt_at = None;
                                                    fire =
                                                      {
                                                        r.fire with
                                                        reason =
                                                          (if
                                                             payload
                                                               .transcript_unavailable
                                                           then
                                                             Some
                                                               "transcript_unavailable"
                                                           else None);
                                                      };
                                                  }
                                                in
                                                match
                                                  save ~ctx ~db ~lease ~before:r
                                                    ~after
                                                with
                                                | Error _ ->
                                                    if
                                                      transcript_conflict db
                                                        ~fire_id:r.fire.id
                                                        transcript_id
                                                    then
                                                      Lwt.return
                                                        (Result.map
                                                           (fun () -> None)
                                                           (terminal ~ctx ~db
                                                              ~lease r
                                                              T.Unavailable
                                                              "transcript_conflict"))
                                                    else
                                                      Lwt.return
                                                        (Error
                                                           "fetch_store_failed")
                                                | Ok () -> (
                                                    let skip =
                                                      match order.action with
                                                      | T.Relay_to_app_mention a
                                                        ->
                                                          payload.transcript
                                                          = None
                                                          && a
                                                               .when_no_transcript
                                                             = T.Skip
                                                    in
                                                    let outcome =
                                                      if skip then
                                                        terminal ~ctx ~db ~lease
                                                          after
                                                          T
                                                          .Skipped_no_transcript
                                                          (if
                                                             payload
                                                               .transcript_unavailable
                                                           then
                                                             "transcript_unavailable"
                                                           else "no_transcript")
                                                      else Ok ()
                                                    in
                                                    match outcome with
                                                    | Error _ as e ->
                                                        Lwt.return e
                                                    | Ok () ->
                                                        L.set_verified ~db
                                                          ~team:order.team_id
                                                          ~channel:
                                                            order.channel_id
                                                          ~room_id:
                                                            r.fire.room_id
                                                          ~date_start:
                                                            payload.date_start
                                                          ~transcript_file_id:
                                                            transcript_id
                                                          ~finalised:skip;
                                                        Lwt.return
                                                          (Ok
                                                             (if skip then None
                                                              else Some payload))
                                                    )))))))))
  in
  Lwt.catch run (function
    | Lwt.Canceled -> Lwt.fail Lwt.Canceled
    | _ -> Lwt.return (Error "fetch_internal"))
