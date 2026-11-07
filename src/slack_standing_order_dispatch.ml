open Lwt.Syntax
(** Upload-v2 dispatch. Only the synchronous complete hook debits capacity; once
    invoked, completion evidence is recorded even after pause or drain. *)

module T = Slack_standing_order_types
module Store = Slack_standing_order_store
module SO = Slack_standing_order_slack
module Grant = Slack_standing_order_grant
module Relay = Slack_standing_order_relay
module Upload = Slack_standing_order_upload
module P = Slack_standing_order_provenance
module Ctx = Slack_standing_order_ctx
module Sql = Slack_standing_order_sql
module Recovery = Slack_standing_order_recovery_store

type wait = Ctx.wait

let default_wait = Ctx.default_wait

module Share = Slack_standing_order_share

let scan = Share.scan
let resolve_share = Share.resolve_share
let rejected_before_share = Share.rejected_before_share

type share_result = Share.share_result =
  | Shared of string
  | Proven_absent
  | Ambiguous

exception Stop = Slack_standing_order_share.Stop
exception Tracking_refused of T.order * Grant.failure

let current db fire_id =
  match Store.runtime db ~fire_id with Some r -> r | None -> raise Stop

let save ~ctx ~db ~lease ~before ~after =
  match Store.cas db ~lease ~before ~after ~now:(ctx.Ctx.now ()) with
  | Ok true -> ()
  | _ -> raise Stop

let update_attempt (r : Store.fire_runtime) (a : Store.relay_attempt) =
  {
    r with
    attempts =
      a
      :: List.filter
           (fun (old : Store.relay_attempt) -> old.attempt_no <> a.attempt_no)
           r.attempts;
  }

let attempt (r : Store.fire_runtime) =
  Option.value
    (Store.attempt r r.current_attempt)
    ~default:(Store.empty_attempt r.current_attempt)

let accepting ctx = if not (ctx.Ctx.accepting ()) then raise Stop

let limit ~ctx ~channel_id () =
  accepting ctx;
  let* () = ctx.Ctx.limit ~channel_id in
  accepting ctx;
  Lwt.return_unit

let set_state ~ctx ~db ~lease ~fire_id state reason next_attempt_at =
  let before = current db fire_id in
  save ~ctx ~db ~lease ~before
    ~after:
      { before with fire = { before.fire with state; reason }; next_attempt_at }

let dispatch_refused ~ctx ~db ~lease ~fire_id () =
  let r = current db fire_id in
  (if
     r.fire.state = T.Uploading
     && Recovery.relay_allowed db ~fire_id ~now:(ctx.Ctx.now ())
   then
     (* The transport deliberately hides synchronous fence details. Recover the
       row refusal without yielding so cap reporting keeps its actual cause. *)
     let capped =
       match Store.find db r.fire.order_id with
       | Some order ->
           Grant.stage_rows db ~now:(ctx.Ctx.now ())
             ~stage:(Grant.Fire { fire_epoch = r.fire.epoch; fire_id })
             (Grant.subject_of_order order)
           = Error Grant.Daily_cap_reached
       | None -> false
     in
     set_state ~ctx ~db ~lease ~fire_id
       (if capped then T.Skipped_cap else T.Skipped_inactive)
       (Some (if capped then "daily_cap_reached" else "dispatch_refused"))
       None);
  raise Stop

let source_matches (r : Store.fire_runtime) (payload : P.verified) =
  String.trim payload.notes_md <> ""
  && r.canvas_id = Some payload.canvas_id
  && r.root_ts = Some payload.root_ts
  && r.notes_ready_ts = Some payload.notes_ready_ts
  && r.transcript_id
     = Option.map (fun (t : P.transcript) -> t.file_id) payload.transcript
  && Option.fold ~none:true
       ~some:(fun (t : P.transcript) -> String.trim t.markdown <> "")
       payload.transcript
  && r.fire.room_id = payload.room_id
  && r.fire.date_start = string_of_int payload.date_start

let refuse ~ctx ~db ~lease ~fire_id (f : Grant.failure) =
  let r = current db fire_id in
  let reason = Grant.reason_code f.reason in
  match f.cls with
  | Grant.Hard ->
      ignore
        (Store.deny_and_suspend_if_epoch db ~lease ~before:r ~reason
           ~now:(ctx.Ctx.now ()))
  | Inactive ->
      set_state ~ctx ~db ~lease ~fire_id T.Skipped_inactive (Some reason) None
  | Capped ->
      set_state ~ctx ~db ~lease ~fire_id T.Skipped_cap (Some reason) None
  | Transient ->
      set_state ~ctx ~db ~lease ~fire_id r.fire.state (Some reason)
        (Some (ctx.Ctx.now () +. 120.))

module Validated_gaps = Ephemeron.K1.Make (struct
  type t = Grant.validated

  let equal = ( == )
  let hash _ = 0
end)

let validated_gaps = Validated_gaps.create 8

let evaluate ~ctx ~db ~manager ?http ?http_response ~lease
    (r : Store.fire_runtime) (order : T.order) =
  accepting ctx;
  let gap =
    try Some (Recovery.pass_token db r.fire.order_id) with _ -> None
  in
  let* result =
    Grant.evaluate ?limiter:ctx.Ctx.limiter ~db
      ~current_config:(fun () -> Session.get_config manager)
      ?http ?http_response
      ~limit:(limit ~ctx ~channel_id:order.channel_id)
      ~now:(ctx.Ctx.now ())
      ~stage:(Grant.Fire { fire_epoch = r.fire.epoch; fire_id = r.fire.id })
      ~unit_label:"dispatch"
      (Grant.subject_of_order order)
  in
  accepting ctx;
  match result with
  | Ok v ->
      Validated_gaps.replace validated_gaps v gap;
      Lwt.return v
  | Error f ->
      refuse ~ctx ~db ~lease ~fire_id:r.fire.id f;
      raise Stop

(* This hook is synchronous all the way through the DB transaction. No Lwt
   yield may separate its reservation from the transport invocation. *)
let fence ~ctx ~db ~manager ~lease ~(v : Grant.validated)
    ~(prepared : Store.fire_runtime) ~reserve () =
  try
    accepting ctx;
    if Session.get_config manager != v.config then raise Stop;
    (match Validated_gaps.find_opt validated_gaps v with
    | Some (Some token) when Recovery.current_gap db token ~connected:true -> ()
    | _ -> raise Stop);
    let r = current db prepared.fire.id in
    if not (Recovery.relay_allowed db ~fire_id:r.fire.id ~now:(ctx.now ())) then
      raise Stop;
    if
      (not (Store.valid_lease lease r (ctx.Ctx.now ())))
      || r.current_attempt <> prepared.current_attempt
      || r.canvas_id <> prepared.canvas_id
      || r.transcript_id <> prepared.transcript_id
      || r.notes_ready_ts <> prepared.notes_ready_ts
      || r.nonce <> prepared.nonce
      || r.attempts <> prepared.attempts
      || r.fire.state <> T.Uploading
      || r.fire.cancel_requested || r.fire.repost_forbidden
    then raise Stop;
    let order = Option.get v.order in
    (match v.config.channels.slack with
    | None -> raise Stop
    | Some sc -> (
        match Grant.grantor_policy ~db sc (Grant.subject_of_order order) with
        | Ok () -> ()
        | Error reason ->
            refuse ~ctx ~db ~lease ~fire_id:r.fire.id (Grant.failure reason);
            raise Stop));
    (match
       Grant.stage_rows db ~now:(ctx.Ctx.now ())
         ~stage:(Grant.Fire { fire_epoch = r.fire.epoch; fire_id = r.fire.id })
         (Grant.subject_of_order order)
     with
    | Ok _ -> ()
    | Error _ -> raise Stop);
    let authorize () =
      ctx.accepting ()
      && Session.get_config manager == v.config
      && (match Validated_gaps.find_opt validated_gaps v with
        | Some (Some token) -> Recovery.current_gap db token ~connected:true
        | _ -> false)
      && Recovery.relay_allowed db ~fire_id:r.fire.id ~now:(ctx.now ())
      && Store.valid_lease lease (current db r.fire.id) (ctx.now ())
      && Result.is_ok
           (Grant.stage_rows db ~now:(ctx.now ())
              ~stage:
                (Grant.Fire { fire_epoch = r.fire.epoch; fire_id = r.fire.id })
              (Grant.subject_of_order order))
      && Option.fold ~none:false
           ~some:(fun sc ->
             Result.is_ok
               (Grant.grantor_policy ~db sc (Grant.subject_of_order order)))
           v.config.channels.slack
    in
    (if reserve then
       match
         Store.reserve_dispatch ~check:authorize
           ~on_reserved:(fun () ->
             let gap = Recovery.pass_token db r.fire.order_id in
             Sql.exec db
               "INSERT OR REPLACE INTO slack_order_dispatch_admission \
                VALUES(?,?,?)"
               Sql.
                 [
                   text r.fire.id; int r.current_attempt; int gap.gap_generation;
                 ])
           db ~fire_id:r.fire.id ~lease ~repost:(r.current_attempt = 1)
           ~now:(ctx.Ctx.now ())
       with
       | Ok true -> ()
       | _ -> raise Stop
     else
       match Sql.in_tx db ~on_error:Fun.id (fun () -> Ok (authorize ())) with
       | Ok true -> ()
       | _ -> raise Stop);
    Ok ()
  with _ -> Error SO.Dispatch_refused

let oldest db (r : Store.fire_runtime) =
  match
    Sql.query db "SELECT strftime('%s',?)"
      [ Sql.text r.fire.created_at ]
      (fun s -> Sql_util.opt_text_column s 0)
  with
  | [ Some ts ] when Grant.canonical_sql_datetime r.fire.created_at -> ts
  | _ -> raise Stop

let record_posted ~ctx ~db ~lease ~fire_id ~ts ~ids =
  let before = current db fire_id in
  let tracking =
    List.mem before.fire.state
      T.
        [
          Acknowledged; Picked_up; Not_delivered; No_pickup; Skipped_cap; Failed;
        ]
  in
  let a =
    { (attempt before) with relay_ts = Some ts; uploaded_file_ids = ids }
  in
  let after = update_attempt before a in
  save ~ctx ~db ~lease ~before
    ~after:
      {
        after with
        fire =
          {
            after.fire with
            state = (if tracking then before.fire.state else T.Posted);
            relay_ts = Some ts;
            reason = (if tracking then before.fire.reason else None);
          };
        next_attempt_at = None;
      }

let deliver ~ctx ~db ~manager ?http ?http_response ?upload_http ~lease
    (initial : Store.fire_runtime) (payload : P.verified) =
  let fire_id = initial.fire.id in
  let fail reason =
    set_state ~ctx ~db ~lease ~fire_id T.Failed (Some reason) None;
    raise Stop
  in
  let retry reason =
    set_state ~ctx ~db ~lease ~fire_id T.Uploading (Some reason)
      (Some (ctx.Ctx.now () +. 120.));
    raise Stop
  in
  let hard_error method_ error =
    let failure = P.grant_failure ~method_ error in
    if failure.cls = Grant.Hard then (
      refuse ~ctx ~db ~lease ~fire_id failure;
      raise Stop)
  in
  let precomplete_error ~method_ reason error =
    hard_error method_ error;
    match error with
    | SO.Dispatch_refused -> dispatch_refused ~ctx ~db ~lease ~fire_id ()
    | SO.Rate_limited | SO.Transport | SO.Audit_unavailable
    | SO.Snapshot_unavailable ->
        retry reason
    | SO.Http_status code when code = 429 || (code >= 500 && code < 600) ->
        retry reason
    | _ -> fail reason
  in
  let run () =
    accepting ctx;
    let r = current db fire_id in
    if not (List.mem r.fire.state T.[ Fetching; Uploading ]) then raise Stop;
    if not (source_matches r payload) then fail "source_payload_missing";
    let order =
      match Store.find db r.fire.order_id with
      | Some o -> o
      | None -> raise Stop
    in
    let* v = evaluate ~ctx ~db ~manager ?http ?http_response ~lease r order in
    let nonce =
      match r.nonce with
      | Some n -> n
      | None ->
          let secret =
            match Relay.secret db with Ok s -> s | Error _ -> raise Stop
          in
          Relay.nonce ~secret ~order_id:order.id ~room_id:r.fire.room_id
            ~date_start:r.fire.date_start
    in
    let prepared = { r with nonce = Some nonce } in
    save ~ctx ~db ~lease ~before:r ~after:prepared;
    let* existing =
      scan ~ctx ~call:v.call ~order ~r:prepared ~nonce ~oldest:(oldest db r)
    in
    match existing with
    | Error () ->
        set_state ~ctx ~db ~lease ~fire_id r.fire.state
          (Some "relay_scan_ambiguous")
          (Some (ctx.Ctx.now () +. 120.));
        Lwt.return_unit
    | Ok found -> (
        let r = current db fire_id in
        if not (source_matches r payload) then raise Stop;
        let a = attempt r in
        (* The scan above still uses the retained exact file set. With no
           match and no reservation, no completion was sent: orphaned upload
           capabilities can be discarded and preparation safely restarted. *)
        if found = None && a.dispatched_at <> None then raise Stop;
        let r =
          if found = None && a.upload_started_at <> None then (
            let after =
              update_attempt r
                { a with uploaded_file_ids = []; upload_started_at = None }
            in
            save ~ctx ~db ~lease ~before:r ~after;
            current db fire_id)
          else r
        in
        let a = attempt r in
        let a =
          match found with
          | Some (_, ids) -> { a with uploaded_file_ids = ids }
          | None -> { a with upload_started_at = Some (ctx.Ctx.now ()) }
        in
        let after = update_attempt r a in
        save ~ctx ~db ~lease ~before:r
          ~after:
            {
              after with
              fire = { after.fire with state = T.Uploading; reason = None };
              next_attempt_at = None;
            };
        match found with
        | Some (ts, ids) ->
            let r = current db fire_id in
            let* v =
              evaluate ~ctx ~db ~manager ?http ?http_response ~lease r order
            in
            (match
               fence ~ctx ~db ~manager ~lease ~v ~prepared:r ~reserve:true ()
             with
            | Error _ -> raise Stop
            | Ok () -> ());
            record_posted ~ctx ~db ~lease ~fire_id ~ts ~ids;
            Lwt.return_unit
        | None -> (
            let notes_name, transcript_name = Relay.filenames payload in
            let files =
              (notes_name, payload.notes_md)
              ::
              (match (payload.transcript, transcript_name) with
              | Some t, Some filename -> [ (filename, t.markdown) ]
              | _ -> [])
            in
            let rec upload acc = function
              | [] -> Lwt.return (List.rev acc)
              | (filename, body) :: rest ->
                  let r = current db fire_id in
                  let* v =
                    evaluate ~ctx ~db ~manager ?http ?http_response ~lease r
                      order
                  in
                  let call =
                    SO.make ?limiter:ctx.Ctx.limiter ~capability_fence:true ~db
                      ~current_config:(fun () -> Session.get_config manager)
                      ~snapshot:v.snapshot ~channel_id:order.channel_id
                      ~audit:
                        (SO.audit_label ~order_id:(Some order.id)
                           ~unit_label:("fire:" ^ fire_id ^ ":upload_url"))
                      ?http ?http_response
                      ~limit:(limit ~ctx ~channel_id:order.channel_id)
                      ~before_send:
                        (fence ~ctx ~db ~manager ~lease ~v ~prepared:r
                           ~reserve:false)
                      ()
                  in
                  let* allocated =
                    Slack_file_upload.allocate ~call ~filename
                      ~length:(String.length body)
                  in
                  accepting ctx;
                  let id, uri =
                    match allocated with
                    | Ok ticket -> (ticket.file_id, ticket.upload_url)
                    | Error (Slack_file_upload.Remote error) ->
                        precomplete_error ~method_:"files.getUploadURLExternal"
                          "upload_allocation_failed" error
                    | Error _ -> fail "upload_allocation_invalid"
                  in
                  if List.mem id acc then fail "upload_allocation_invalid";
                  let r = current db fire_id in
                  if not (source_matches r payload) then raise Stop;
                  let a = attempt r in
                  let after =
                    update_attempt r
                      {
                        a with
                        uploaded_file_ids = a.uploaded_file_ids @ [ id ];
                      }
                  in
                  save ~ctx ~db ~lease ~before:r ~after;
                  let r = current db fire_id in
                  let* v =
                    evaluate ~ctx ~db ~manager ?http ?http_response ~lease r
                      order
                  in
                  let* uploaded =
                    Upload.post ?limiter:ctx.Ctx.limiter ~db
                      ~current_config:(fun () -> Session.get_config manager)
                      ~snapshot:v.snapshot ~channel_id:order.channel_id
                      ~audit:
                        (SO.audit_label ~order_id:(Some order.id)
                           ~unit_label:("fire:" ^ fire_id ^ ":upload"))
                      ?http:upload_http
                      ~limit:(limit ~ctx ~channel_id:order.channel_id)
                      ~before_send:
                        (fence ~ctx ~db ~manager ~lease ~v ~prepared:r
                           ~reserve:false)
                      ~uri ~body ()
                  in
                  accepting ctx;
                  (match uploaded with
                  | Ok () -> ()
                  | Error error ->
                      precomplete_error ~method_:"upload" "upload_failed" error);
                  upload (id :: acc) rest
            in
            let* ids = upload [] files in
            let r = current db fire_id in
            let* v =
              evaluate ~ctx ~db ~manager ?http ?http_response ~lease r order
            in
            let reserved = ref false in
            let before_send () =
              let result =
                fence ~ctx ~db ~manager ~lease ~v ~prepared:r ~reserve:true ()
              in
              (match result with Ok () -> reserved := true | Error _ -> ());
              result
            in
            let call =
              SO.make ?limiter:ctx.Ctx.limiter ~capability_fence:true ~db
                ~current_config:(fun () -> Session.get_config manager)
                ~snapshot:v.snapshot ~channel_id:order.channel_id
                ~audit:
                  (SO.audit_label ~order_id:(Some order.id)
                     ~unit_label:("fire:" ^ fire_id ^ ":complete"))
                ?http ?http_response
                ~limit:(limit ~ctx ~channel_id:order.channel_id)
                ~before_send ()
            in
            let* completed =
              Slack_file_upload.complete ~call ~channel_id:order.channel_id
                ~thread_ts:None
                ~initial_comment:
                  (Relay.initial_comment ~order ~verified:payload
                     ~team_url:v.team_url ~nonce)
                ~file_ids:ids
            in
            let completed = Result.map_error Upload.error_of_shared completed in
            (* No accepting/epoch fence here: an in-flight send belongs to the
                retained attempt even when pause won while HTTP was pending. *)
            (match completed with
            | Error error -> hard_error "files.completeUploadExternal" error
            | Ok _ -> ());
            match completed with
            | Error SO.Dispatch_refused ->
                dispatch_refused ~ctx ~db ~lease ~fire_id ()
            | Error _ when not !reserved -> fail "complete_preflight_failed"
            | Ok json
              when Relay.share_ts ~channel_id:order.channel_id json <> None ->
                record_posted ~ctx ~db ~lease ~fire_id
                  ~ts:
                    (Option.get
                       (Relay.share_ts ~channel_id:order.channel_id json))
                  ~ids;
                Lwt.return_unit
            | _ ->
                let before = current db fire_id in
                let rejected = rejected_before_share completed in
                let after =
                  update_attempt before
                    {
                      (attempt before) with
                      complete_rejected_before_share = rejected;
                    }
                in
                save ~ctx ~db ~lease ~before
                  ~after:
                    {
                      after with
                      fire =
                        {
                          after.fire with
                          state = T.Posting_unknown;
                          reason =
                            Some
                              (if rejected then "complete_rejected_before_share"
                               else "complete_unknown");
                        };
                      next_attempt_at = Some (ctx.Ctx.now ());
                    };
                Lwt.return_unit))
  in
  Lwt.catch run (function
    | Stop -> Lwt.return_unit
    | Lwt.Canceled -> Lwt.fail Lwt.Canceled
    | _ -> Lwt.return_unit)

let track ~ctx ~db ~manager ?http ?http_response (r : Store.fire_runtime)
    (order : T.order) =
  accepting ctx;
  let* result =
    Grant.evaluate ?limiter:ctx.Ctx.limiter ~db
      ~current_config:(fun () -> Session.get_config manager)
      ?http ?http_response
      ~limit:(limit ~ctx ~channel_id:order.channel_id)
      ~now:(ctx.Ctx.now ())
      ~stage:(Grant.Track { fire_id = r.fire.id })
      ~unit_label:"reconcile"
      (Grant.subject_of_order order)
  in
  accepting ctx;
  match result with
  | Error failure -> Lwt.fail (Tracking_refused (order, failure))
  | Ok v ->
      let call method_ params =
        accepting ctx;
        let* result = v.call method_ params in
        accepting ctx;
        Lwt.return result
      in
      Lwt.return call

let no_pickup (r : Store.fire_runtime) =
  not
    (List.exists
       (fun (a : Store.relay_attempt) ->
         a.reaction_seen || a.acknowledged_at <> None || a.picked_up_at <> None
         || a.not_delivered)
       r.attempts)

(* A rate notice is already classified and persisted by the observer. Re-read
   every retained relay and file before a fresh write; an incomplete read is
   never evidence of no reaction or no newer target reply. *)
let confirm_no_pickup ~call ~(order : T.order) (r : Store.fire_runtime) =
  let target = T.action_target order.action in
  let reactions json =
    match P.get "reactions" json with
    | `List xs ->
        List.for_all
          (fun reaction ->
            match (P.get "users" reaction, P.get "count" reaction) with
            | `List users, `Int count when count = List.length users ->
                not (List.mem (`String target.user_id) users)
            | _ -> false)
          xs
    | _ -> false
  in
  let read_reactions params key =
    let* result = call "reactions.get" params in
    match result with
    | Ok json when reactions (P.get key json) -> Lwt.return_unit
    | _ -> raise Stop
  in
  let rec replies (a : Store.relay_attempt) ts remaining seen cursor =
    let* result =
      call "conversations.replies"
        ([
           ("channel", `String order.channel_id);
           ("ts", `String ts);
           ("limit", `Int 200);
         ]
        @ if cursor = "" then [] else [ ("cursor", `String cursor) ])
    in
    match result with
    | Ok json -> (
        match P.get "messages" json with
        | `List messages when List.length messages <= 200 -> (
            if
              List.exists
                (fun m ->
                  (P.field "user" m = target.user_id
                  || P.field "bot_id" m = target.bot_id)
                  && Some (P.field "ts" m) <> a.first_reply_ts)
                messages
            then raise Stop;
            let cursor =
              P.get "response_metadata" json |> P.get "next_cursor"
            in
            match (P.get "has_more" json, cursor) with
            | (`Bool false | `Null), `String "" | `Bool false, `Null ->
                Lwt.return_unit
            | (`Bool true | `Bool false | `Null), `String next
              when next <> "" && remaining > 1 && not (List.mem next seen) ->
                replies a ts (remaining - 1) (next :: seen) next
            | _ -> raise Stop)
        | _ -> raise Stop)
    | _ -> raise Stop
  in
  Lwt_list.iter_s
    (fun (a : Store.relay_attempt) ->
      match a.relay_ts with
      | None -> Lwt.return_unit
      | Some ts ->
          let* () =
            read_reactions
              [
                ("channel", `String order.channel_id); ("timestamp", `String ts);
              ]
              "message"
          in
          let* () =
            Lwt_list.iter_s
              (fun id -> read_reactions [ ("file", `String id) ] "file")
              a.uploaded_file_ids
          in
          replies a ts 5 [] "")
    r.attempts

let reconcile ~ctx ~db ~manager ?http ?http_response ?upload_http
    ?(allow_new = true) ~lease (initial : Store.fire_runtime) =
  let fire_id = initial.fire.id in
  let run () =
    accepting ctx;
    let r = current db fire_id in
    let order =
      match Store.find db r.fire.order_id with
      | Some o -> o
      | None -> raise Stop
    in
    let* call = track ~ctx ~db ~manager ?http ?http_response r order in
    let nonce = match r.nonce with Some n -> n | None -> raise Stop in
    (* With no original timestamp, the shared nonce cannot distinguish an
       original history match from a new attempt. Re-establish absence even
       when resuming an unreserved preparation: retry reasons are not proof. *)
    let first = Option.get (Store.attempt r 0) in
    let* first_absent =
      if
        first.relay_ts = None
        && (r.current_attempt = 0
            && List.mem r.fire.state T.[ Posting; Posting_unknown ]
           || r.current_attempt = 1 && r.fire.state = T.Uploading
              && (attempt r).dispatched_at = None)
      then (
        let* result =
          resolve_share ~ctx ~call ~order ~attempt_no:0
            ~observe:(fun () -> Store.runtime db ~fire_id)
            r
        in
        match result with
        | Proven_absent ->
            (* Posting is eligible only for read-only adoption. Reposting
               still requires the durable unknown-completion path. *)
            Lwt.return (r.fire.state <> T.Posting)
        | Ambiguous -> raise Stop
        | Shared ts ->
            let r = current db fire_id in
            let first = Option.get (Store.attempt r 0) in
            let* ids =
              if first.uploaded_file_ids <> [] then
                Lwt.return first.uploaded_file_ids
              else
                let* found =
                  scan ~ctx ~call ~order
                    ~r:{ r with current_attempt = 0 }
                    ~nonce ~oldest:(oldest db r)
                in
                match found with
                | Ok (Some (found_ts, ids)) when found_ts = ts -> Lwt.return ids
                | _ -> raise Stop
            in
            (if r.current_attempt = 0 then
               record_posted ~ctx ~db ~lease ~fire_id ~ts ~ids
             else
               (* A late original share ends the unreserved second preparation.
                 Keep its identity on attempt zero and the current projection. *)
               let after =
                 update_attempt r
                   { first with relay_ts = Some ts; uploaded_file_ids = ids }
               in
               save ~ctx ~db ~lease ~before:r
                 ~after:
                   {
                     after with
                     fire = { after.fire with state = T.Posted; reason = None };
                     next_attempt_at = None;
                   });
            raise Stop)
      else Lwt.return false
    in
    let second =
      Option.value (Store.attempt r 1) ~default:(Store.empty_attempt 1)
    in
    let scan_runtime =
      {
        r with
        current_attempt = 1;
        attempts =
          second
          :: List.filter
               (fun (a : Store.relay_attempt) -> a.attempt_no <> 1)
               r.attempts;
      }
    in
    let* found =
      scan ~ctx ~call ~order ~r:scan_runtime ~nonce ~oldest:(oldest db r)
    in
    match found with
    | Error () -> Lwt.return_unit
    | Ok (Some (ts, ids)) ->
        (* Every complete was debited write-ahead. Adoption of a reserved
           attempt is read-only, including on paused and capped orders. *)
        if second.dispatched_at = None || r.current_attempt <> 1 then raise Stop;
        record_posted ~ctx ~db ~lease ~fire_id ~ts ~ids;
        Lwt.return_unit
    | Ok None ->
        let r = current db fire_id in
        if r.fire.reposts <> 0 then
          if second.relay_ts = None && second.dispatched_at <> None then (
            let* result =
              resolve_share ~ctx ~call ~order ~attempt_no:1
                ~observe:(fun () -> Store.runtime db ~fire_id)
                r
            in
            (match result with
            | Shared ts ->
                record_posted ~ctx ~db ~lease ~fire_id ~ts
                  ~ids:second.uploaded_file_ids
            | _ -> ());
            Lwt.return_unit)
          else Lwt.return_unit
        else
          let () =
            if
              r.current_attempt = 1
              && Option.fold ~none:false
                   ~some:(fun due -> ctx.Ctx.now () < due)
                   r.next_attempt_at
            then raise Stop
          in
          let writable r =
            Store.relay_allowed db r
            && (not r.Store.fire.repost_forbidden)
            && no_pickup r
          in
          let first = Option.get (Store.attempt r 0) in
          let* allowed =
            match (first.rate_limited_at, first.first_reply_ts) with
            | Some notice, Some _ when first.relay_ts <> None ->
                let due = notice +. 300. in
                if not (writable r) then Lwt.return false
                else if ctx.Ctx.now () < due then (
                  set_state ~ctx ~db ~lease ~fire_id r.fire.state r.fire.reason
                    (Some due);
                  Lwt.return false)
                else Lwt.return true
            | _ when first_absent -> Lwt.return true
            | _ when r.fire.state = T.Posting_unknown -> (
                let* result =
                  resolve_share ~ctx ~call ~order ~attempt_no:0
                    ~observe:(fun () -> Store.runtime db ~fire_id)
                    r
                in
                match result with
                | Shared ts ->
                    record_posted ~ctx ~db ~lease ~fire_id ~ts
                      ~ids:first.uploaded_file_ids;
                    Lwt.return false
                | Proven_absent -> Lwt.return true
                | Ambiguous -> Lwt.return false)
            | _ -> Lwt.return false
          in
          if
            (not allow_new) || (not allowed)
            || not (writable (current db fire_id))
          then Lwt.return_unit
          else
            let* () = confirm_no_pickup ~call ~order (current db fire_id) in
            let r = current db fire_id in
            if not (no_pickup r) then raise Stop;
            let* _ =
              evaluate ~ctx ~db ~manager ?http ?http_response ~lease r order
            in
            let root_ts =
              match r.root_ts with Some ts -> ts | None -> raise Stop
            in
            let* payload =
              P.verify ~call ~order ~room_id:r.fire.room_id
                ~date_start:r.fire.date_start ~root_ts
            in
            accepting ctx;
            let payload =
              match payload with Ok p -> p | Error _ -> raise Stop
            in
            let r = current db fire_id in
            if (not (source_matches r payload)) || not (no_pickup r) then
              raise Stop;
            let* _ =
              evaluate ~ctx ~db ~manager ?http ?http_response ~lease r order
            in
            let after = update_attempt r second in
            save ~ctx ~db ~lease ~before:r
              ~after:
                {
                  after with
                  current_attempt = 1;
                  fire =
                    {
                      after.fire with
                      state = T.Uploading;
                      relay_ts = None;
                      reason = None;
                    };
                  next_attempt_at = None;
                };
            deliver ~ctx ~db ~manager ?http ?http_response ?upload_http ~lease
              (current db fire_id) payload
  in
  Lwt.catch run (function
    | Tracking_refused _ as exn -> Lwt.fail exn
    | Lwt.Canceled -> Lwt.fail Lwt.Canceled
    | _ -> Lwt.return_unit)

let repost ~ctx ~db ~manager ?http ?http_response ?upload_http ~lease initial
    (_payload : P.verified) =
  reconcile ~ctx ~db ~manager ?http ?http_response ?upload_http ~lease initial
