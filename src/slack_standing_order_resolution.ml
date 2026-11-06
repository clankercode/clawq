open Lwt.Syntax
(** Typed, pinned historical inspection. Produces durable work, never a relay.
    The caller supplies the already validated per-pass transport. *)

module T = Slack_standing_order_types
module SO = Slack_standing_order_slack
module Grant = Slack_standing_order_grant
module Store = Slack_standing_order_store
module R = Slack_standing_order_recovery_store
module Q = Slack_standing_order_schedule
module P = Slack_standing_order_provenance
module Pages = Slack_standing_order_pages
module L = Slack_huddle_ledger
module C = Slack_huddle_classify
module Ctx = Slack_standing_order_ctx
module Sql = Slack_standing_order_sql

type incomplete_code = Pages.incomplete_code
type verified_root = { root_ts : string; room : C.room }
type verified_end = { root : verified_root; ended_at : int }
type candidate_id = string

type fixed_reason =
  | Before_activation
  | Inactive_epoch
  | Source_rejected
  | Already_delivered
  | Identity_missing

type resolution =
  | Live of verified_root
  | Not_ready of verified_end
  | Candidate of candidate_id
  | Ineligible of fixed_reason
  | Missing_exact_root
  | Unavailable of fixed_reason
  | No_notes of verified_end

type resolution_failure =
  | Authority of Grant.failure
  | Incomplete of { method_ : string; code : incomplete_code }
  | Lost_fence

let failure_of_provenance = function
  | P.Api { method_; error } -> Authority (P.grant_failure ~method_ error)
  | P.Incomplete { method_; code } -> Incomplete { method_; code }
  | _ ->
      Incomplete { method_ = "conversations.history"; code = Pages.Malformed }

let failure_for_schedule = function
  | Authority f -> f
  | Incomplete { method_; _ } -> Grant.failure (Grant.Indeterminate method_)
  | Lost_fence -> Grant.failure (Grant.Indeterminate "resolution_stopped")

let current ~ctx ~db ~manager ~validated (order : T.order) =
  ctx.Ctx.accepting ()
  && Session.get_config manager == validated.Grant.config
  &&
  match Store.find db order.id with
  | Some o ->
      o.state = T.Active && o.epoch = order.epoch
      && Grant.same_scope (Grant.subject_of_order order) o
      && Grant.unexpired o ~now:(ctx.now ())
      && Store.activation_announced db ~order_id:o.id
  | None -> false

let inspect ~call ~channel ~root_ts =
  let* result =
    call "conversations.history"
      [
        ("channel", `String channel);
        ("oldest", `String root_ts);
        ("latest", `String root_ts);
        ("inclusive", `Bool true);
        ("limit", `Int 1);
      ]
  in
  match result with
  | Error e ->
      Lwt.return
        (Error (Authority (P.grant_failure ~method_:"conversations.history" e)))
  | Ok json ->
      let bad code =
        Error (Incomplete { method_ = "conversations.history"; code })
      in
      Lwt.return
        (match (Pages.messages json, Pages.next ~seen:[] ~remaining:1 json) with
        | Error code, _ | _, Error code -> bad code
        | Ok [], Ok "" -> Ok None
        | Ok [ root ], Ok ""
          when P.field "ts" root = root_ts && P.same_channel channel root -> (
            if P.field "call_family" (P.get "room" root) <> "huddle" then
              Ok (Some (root, None))
            else
              match C.room_of_json (P.get "room" root) with
              | Some room when room.date_start <> None ->
                  Ok (Some (root, Some room))
              | _ -> bad Pages.Malformed)
        | _ -> bad Pages.Wrong_root)

let record ~db ~order ~root room ~now =
  let date_start = string_of_int (Option.get room.C.date_start) in
  R.record_root ~recovery:true db ~order ~root_ts:root ~room_id:room.room_id
    ~date_start ~fire_id:None;
  date_start

let inspect_candidate ~ctx ~db ~manager ~validated ~root (order : T.order) =
  let valid () = current ~ctx ~db ~manager ~validated order in
  let malformed = ref None in
  let call method_ params =
    if not (valid ()) then Lwt.return (Error SO.Dispatch_refused)
    else
      SO.with_policy
        ~check:(fun () -> if valid () then Ok () else Error SO.Dispatch_refused)
        ~priority:Slack_read_limiter.Maintenance
        (fun () ->
          let+ response = validated.Grant.call method_ params in
          if not (valid ()) then Error SO.Dispatch_refused
          else
            match response with
            | Error _ -> response
            | Ok json -> (
                let bad code =
                  malformed := Some (Incomplete { method_; code });
                  Error SO.Transport
                in
                match method_ with
                | "conversations.history" -> (
                    match
                      ( Pages.messages json,
                        Pages.next ~seen:[] ~remaining:1 json )
                    with
                    | Ok [ m ], Ok "" when P.field "ts" m <> root ->
                        bad Pages.Wrong_root
                    | Ok _, Ok "" -> response
                    | Error code, _ | _, Error code -> bad code
                    | _ -> bad Pages.Truncated)
                | "files.info" -> (
                    match P.get "file" json with
                    | `Assoc _ -> response
                    | _ -> bad Pages.Malformed)
                | "canvases.getContent" -> (
                    match P.get "content" json with
                    | `String _ -> response
                    | _ -> bad Pages.Malformed)
                | _ -> response))
  in
  let* result = inspect ~call ~channel:order.channel_id ~root_ts:root in
  if not (valid ()) then Lwt.return (Error Lost_fence)
  else
    match !malformed with
    | Some failure -> Lwt.return (Error failure)
    | None -> (
        match result with
        | Error _ as e -> Lwt.return e
        | Ok None -> Lwt.return (Ok Missing_exact_root)
        | Ok (Some (_, None)) -> Lwt.return (Ok (Unavailable Identity_missing))
        | Ok (Some (json, Some room)) -> (
            let start = record ~db ~order ~root room ~now:(ctx.now ()) in
            L.record_seen ~db ~team:order.team_id ~channel:order.channel_id
              ~source:"reconcile" ~now:(ctx.now ()) ~root_ts:root
              ~file_ids:(P.file_ids json) room;
            let verified_root = { root_ts = root; room } in
            if not room.has_ended then Lwt.return (Ok (Live verified_root))
            else if
              Option.fold ~none:true
                ~some:(fun active -> P.compare_ts start active < 0)
                order.active_since
            then Lwt.return (Ok (Ineligible Before_activation))
            else if
              R.suppressed db ~order_id:order.id ~root_ts:root
                ~room_id:room.room_id ~date_start:start
            then Lwt.return (Ok (Ineligible Already_delivered))
            else
              match room.date_end with
              | None -> Lwt.return (Ok (Unavailable Identity_missing))
              | Some ended_at when ended_at < Option.get room.date_start ->
                  Lwt.return
                    (Error
                       (Incomplete
                          {
                            method_ = "conversations.history";
                            code = Pages.Malformed;
                          }))
              | Some ended_at -> (
                  let verified_end = { root = verified_root; ended_at } in
                  let before_content ~canvas_id:_ ~transcript_id ~notes_ready_ts
                      =
                    if not (valid ()) then
                      Error
                        (P.Api
                           {
                             method_ = "files.info";
                             error = SO.Dispatch_refused;
                           })
                    else if
                      P.compare_ts notes_ready_ts
                        (Option.get order.active_since)
                      < 0
                    then Error (P.Rejected "ineligible")
                    else
                      match transcript_id with
                      | None -> Ok ()
                      | Some id ->
                          let conflicts =
                            Sql.query db
                              "SELECT 1 FROM slack_standing_order_fires WHERE \
                               transcript_file_id=? AND NOT (order_id=? AND \
                               room_id=? AND date_start=?)"
                              Sql.
                                [
                                  text id;
                                  text order.id;
                                  text room.room_id;
                                  text start;
                                ]
                              ignore
                          in
                          if conflicts = [] then Ok ()
                          else Error (P.Rejected "transcript_conflict")
                  in
                  let* payload =
                    P.verify_with_source_mode ~strict_pages:true ~before_content
                      ~call ~order ~room_id:room.room_id ~date_start:start
                      ~root_ts:root
                  in
                  if not (valid ()) then Lwt.return (Error Lost_fence)
                  else
                    match !malformed with
                    | Some failure -> Lwt.return (Error failure)
                    | None -> (
                        match payload with
                        | Error (P.Api _ | P.Incomplete _) as e ->
                            Lwt.return
                              (Result.map_error failure_of_provenance e)
                        | Error P.Not_ready ->
                            if ctx.now () < float_of_int ended_at +. 3600. then
                              Lwt.return (Ok (Not_ready verified_end))
                            else (
                              L.set_verified ~db ~team:order.team_id
                                ~channel:order.channel_id ~room_id:room.room_id
                                ~date_start:(Option.get room.date_start)
                                ~transcript_file_id:None ~finalised:true;
                              Lwt.return (Ok (No_notes verified_end)))
                        | Error (P.Rejected "ineligible") ->
                            Lwt.return (Ok (Ineligible Before_activation))
                        | Error (P.Unavailable | P.Rejected _) ->
                            Lwt.return (Ok (Unavailable Source_rejected))
                        | Ok payload ->
                            let transcript_id =
                              Option.map
                                (fun (t : P.transcript) -> t.file_id)
                                payload.transcript
                            in
                            let inserted =
                              Store.insert_fire db ~order ~room_id:room.room_id
                                ~date_start:start ~root_ts:root
                                ~canvas_file_id:payload.canvas_id
                                ?transcript_file_id:transcript_id
                                ~notes_ready_ts:payload.notes_ready_ts
                                ~now:(ctx.now ()) ()
                            in
                            let fire =
                              match inserted with
                              | Ok (Some f) -> Ok f
                              | Ok None -> (
                                  match
                                    Store.query_fires db
                                      "WHERE order_id=? AND room_id=? AND \
                                       date_start=?"
                                      Sql.
                                        [
                                          text order.id;
                                          text room.room_id;
                                          text start;
                                        ]
                                  with
                                  | [ f ] when f.epoch = order.epoch -> (
                                      match Store.runtime db ~fire_id:f.id with
                                      | Some r when r.root_ts = Some root ->
                                          Ok f
                                      | _ ->
                                          Error
                                            (Incomplete
                                               {
                                                 method_ =
                                                   "conversations.history";
                                                 code = Pages.Wrong_root;
                                               }))
                                  | _ -> Error Lost_fence)
                              | Error _ ->
                                  Error
                                    (Incomplete
                                       {
                                         method_ = "files.info";
                                         code = Pages.Store_failed;
                                       })
                            in
                            Lwt.return
                              (Result.map
                                 (fun (fire : T.fire) ->
                                   R.record_root ~recovery:true db ~order
                                     ~root_ts:root ~room_id:room.room_id
                                     ~date_start:start ~fire_id:(Some fire.id);
                                   L.set_verified ~db ~team:order.team_id
                                     ~channel:order.channel_id
                                     ~room_id:room.room_id
                                     ~date_start:(Option.get room.date_start)
                                     ~transcript_file_id:transcript_id
                                     ~finalised:false;
                                   ignore
                                     (L.record_notes_ready ~db
                                        ~team:order.team_id
                                        ~channel:order.channel_id
                                        ~source:"reconcile" ~now:(ctx.now ())
                                        ~thread_ts:payload.thread_ts
                                        ~ts:payload.notes_ready_ts
                                        ~file_ids:[ payload.canvas_id ]
                                        ~canvas_file_id:payload.canvas_id
                                        ~variant:C.Notes_only);
                                   Candidate fire.id)
                                 fire)))))

let resolve_with_grant ~ctx ~db ~manager ~validated ~root order =
  let run () =
    if not (current ~ctx ~db ~manager ~validated order) then
      Lwt.return (Error Lost_fence)
    else
      match
        Q.claim_resolution ~db ~key:(Q.key order root)
          ~owner_id:(Store.new_id ~prefix:"reconcile-root")
          ~now:(ctx.Ctx.now ())
      with
      | None -> Lwt.return (Error Lost_fence)
      | Some lease ->
          Slack_standing_order_resolution_lease.with_resolution_lease ~ctx ~db
            lease (fun ctx lease ->
              let* result =
                inspect_candidate ~ctx ~db ~manager ~validated ~root order
              in
              if
                (not (current ~ctx ~db ~manager ~validated order))
                || result = Error Lost_fence
              then Lwt.return (Error Lost_fence)
              else
                let saved =
                  match result with
                  | Ok outcome ->
                      Q.finish_resolution ~db ~lease ~now:(ctx.now ())
                        ~outcome:
                          (match outcome with
                          | Live _ -> Q.Live
                          | Not_ready _ -> Q.Ended
                          | _ -> Q.Settled)
                  | Error failure ->
                      Q.fail_resolution ~db ~lease ~now:(ctx.now ())
                        ~failure:(failure_for_schedule failure)
                in
                Lwt.return (if saved then result else Error Lost_fence))
  in
  Lwt.catch run (function
    | Lwt.Canceled -> Lwt.fail Lwt.Canceled
    | exn when Sql.is_db_error exn ->
        Lwt.return
          (Error
             (Incomplete
                { method_ = "conversations.history"; code = Pages.Store_failed }))
    | exn -> Lwt.fail exn)
