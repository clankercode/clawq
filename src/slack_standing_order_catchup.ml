open Lwt.Syntax
(** Durable catch-up notices. This module never dispatches a relay and never
    installs daemon lifecycle hooks. All waits retain the captured recovery
    owner, gap and config publication; the last check and send are synchronous.
*)

module T = Slack_standing_order_types
module R = Slack_standing_order_recovery_store
module Store = Slack_standing_order_store
module Sql = Slack_standing_order_sql
module SO = Slack_standing_order_slack
module Grant = Slack_standing_order_grant
module Ctx = Slack_standing_order_ctx
module Render = Slack_standing_order_render
module Pages = Slack_standing_order_pages
module Report = Slack_standing_order_report

let prepare = R.prepare_batch

let hold_deadline ~advertised ~confirmed_now =
  max advertised (confirmed_now +. 600.)

let release_due ~db ~batch_id ~now =
  R.attempt_bool (fun () ->
      match R.find_batch db ~batch_id with
      | None -> false
      | Some b -> (
          match R.current_owner db ~order_id:b.order_id with
          | None -> false
          | Some owner -> R.release_batch db ~batch_id ~owner ~now))

let matches ~(order : T.order) ~card_id message =
  let field = Pages.field and get = Pages.get in
  field "user" message = Option.value order.bot_user_id ~default:""
  && field "user" message <> ""
  && field "bot_id" message = Option.value order.bot_id ~default:""
  && field "bot_id" message <> ""
  && (match get "channel" message with
    | `Null -> true
    | `String c -> c = order.channel_id
    | _ -> false)
  && (match get "thread_ts" message with
    | `Null -> true
    | `String ts -> ts = field "ts" message
    | _ -> false)
  &&
  match get "blocks" message with
  | `List blocks ->
      List.exists
        (fun block ->
          field "type" block = "actions"
          && field "block_id" block = Render.catchup_marker ~card_id
          &&
          match get "elements" block with
          | `List [ button ] ->
              field "type" button = "button"
              && field "action_id" button = Render.action_pause
              && field "value" button = card_id
          | _ -> false)
        blocks
  | _ -> false

let ensure_notice ~(ctx : Ctx.t) ~db ~manager ?http ?http_response
    (batch : R.batch) =
  let current_config () = Session.get_config manager in
  match
    ( R.find_batch db ~batch_id:batch.id,
      Store.find db batch.order_id,
      R.current_owner db ~order_id:batch.order_id )
  with
  | Some b, Some order, Some owner when b.epoch = order.epoch ->
      let config = current_config () and gap = R.pass_token db order.id in
      let check () =
        if
          R.attempt_bool (fun () ->
              ctx.accepting ()
              && current_config () == config
              && R.current_gap db gap ~connected:true
              && R.fenced db ~order_id:order.id ~owner ~now:(ctx.now ())
              && R.covered db order.id order.epoch
              && R.ready db order.id
              && R.phase db order.id = Some "complete")
        then Ok ()
        else Error SO.Dispatch_refused
      in
      let reconcile_id =
        "catchup:" ^ b.id ^ ":" ^ string_of_int owner.generation
      in
      let refuse failure =
        if check () <> Ok () || failure.Grant.cls <> Grant.Hard then
          Lwt.return_unit
        else (
          ignore
            (Sql.in_tx db ~on_error:Fun.id (fun () ->
                 if check () <> Ok () then Ok false
                 else
                   Store.suspend_if_epoch_in_tx db ~order_id:order.id
                     ~epoch:order.epoch
                     ~reason:(Grant.reason_code failure.reason)
                     ~now:(ctx.now ())));
          let accepting () =
            ctx.accepting ()
            && current_config () == config
            && R.current_gap db gap ~connected:true
            && R.replay_fenced db ~order ~owner ~now:(ctx.now ())
          in
          Report.post_once ~ctx:{ ctx with accepting } ~db ~manager ?http
            ?http_response ~reconcile_id order
            (Report.of_hard_reason failure.reason))
      in
      let run () =
        if
          check () <> Ok ()
          || not
               (R.adopt_batch_owner
                  ~check:(fun () -> check () = Ok ())
                  db ~batch_id:b.id ~owner ~now:(ctx.now ()))
        then Lwt.return_unit
        else
          match R.batch_details db ~batch_id:b.id with
          | Some (("confirmed" | "released" | "cancelled"), _) | None ->
              Lwt.return_unit
          | Some (status, card_id) -> (
              let* evaluated =
                Grant.evaluate ~db ~current_config ?http ?http_response
                  ?limiter:ctx.limiter
                  ~limit:(fun () -> ctx.limit ~channel_id:order.channel_id)
                  ~before_send:check ~now:(ctx.now ())
                  ~stage:(Grant.Reconcile { reconcile_id })
                  ~unit_label:"catchup"
                  (Grant.subject_of_order order)
              in
              if check () <> Ok () then Lwt.return_unit
              else
                match evaluated with
                | Error failure -> refuse failure
                | Ok validated ->
                    let call method_ params =
                      SO.with_policy ~check
                        ~priority:Slack_read_limiter.Maintenance (fun () ->
                          validated.call method_ params)
                    in
                    let confirm ts =
                      if check () = Ok () then
                        let advertised =
                          Option.value
                            (Option.get (R.find_batch db ~batch_id:b.id))
                              .release_at
                            ~default:(ctx.now () +. 600.)
                        in
                        ignore
                          (R.bind_notice
                             ~check:(fun () -> check () = Ok ())
                             db ~batch_id:b.id ~owner ~ts
                             ~release_at:
                               (hold_deadline ~advertised
                                  ~confirmed_now:(ctx.now ()))
                             ~now:(ctx.now ()))
                    in
                    let unknown () =
                      if check () <> Ok () then Lwt.return_unit
                      else (
                        ignore
                          (R.notice_unknown
                             ~check:(fun () -> check () = Ok ())
                             db ~batch_id:b.id ~owner ~now:(ctx.now ()));
                        Report.post_once
                          ~ctx:
                            {
                              ctx with
                              accepting = (fun () -> check () = Ok ());
                            }
                          ~db ~manager ?http ?http_response ~reconcile_id order
                          (Report.Catchup_notice_unknown b.id))
                    in
                    if status = "pending" then
                      let advertised = ctx.now () +. 600. in
                      let notice =
                        Render.catchup_notice order ~card_id
                          ~count:(R.batch_count db ~batch_id:b.id)
                          ~release_at:advertised
                      in
                      let before_send () =
                        match check () with
                        | Error _ as e -> e
                        | Ok () ->
                            if
                              R.reserve_notice
                                ~check:(fun () -> check () = Ok ())
                                ~advertised db ~batch_id:b.id ~owner
                                ~now:(ctx.now ())
                            then Ok ()
                            else Error SO.Dispatch_refused
                      in
                      let* result =
                        SO.with_policy ~check:before_send
                          ~priority:Slack_read_limiter.Maintenance (fun () ->
                            call "chat.postMessage"
                              [
                                ("channel", `String order.channel_id);
                                ("text", `String notice.text);
                                ("blocks", notice.blocks);
                                ("unfurl_links", `Bool false);
                                ("unfurl_media", `Bool false);
                              ])
                      in
                      if check () <> Ok () then Lwt.return_unit
                      else
                        match result with
                        | Ok json
                          when Pages.field "channel" json = order.channel_id
                               && T.valid_slack_ts (Pages.field "ts" json) ->
                            confirm (Pages.field "ts" json);
                            Lwt.return_unit
                        | Error error
                          when (Slack_standing_order_provenance.grant_failure
                                  ~method_:"chat.postMessage" error)
                                 .Grant.cls = Grant.Hard ->
                            refuse
                              (Slack_standing_order_provenance.grant_failure
                                 ~method_:"chat.postMessage" error)
                        | _ -> (
                            match R.batch_details db ~batch_id:b.id with
                            | Some ("notice_attempted", _) -> unknown ()
                            | _ -> Lwt.return_unit)
                    else if
                      List.mem status [ "notice_attempted"; "notice_unknown" ]
                    then
                      let advertised =
                        Option.value b.release_at ~default:(ctx.now ())
                      in
                      let oldest =
                        max (ctx.now () -. 172800.) (advertised -. 600.)
                      in
                      let latest = ctx.now () in
                      let failure = ref None in
                      let rec search cursor seen remaining found =
                        let* result =
                          call "conversations.history"
                            ([
                               ("channel", `String order.channel_id);
                               ("oldest", `String (Printf.sprintf "%.6f" oldest));
                               ("latest", `String (Printf.sprintf "%.6f" latest));
                               ("inclusive", `Bool true);
                               ("limit", `Int 200);
                             ]
                            @
                            if cursor = "" then []
                            else [ ("cursor", `String cursor) ])
                        in
                        if check () <> Ok () then Lwt.return_none
                        else
                          match result with
                          | Error error ->
                              failure :=
                                Some
                                  (Slack_standing_order_provenance.grant_failure
                                     ~method_:"conversations.history" error);
                              Lwt.return_none
                          | Ok json -> (
                              match
                                ( Pages.messages json,
                                  Pages.next ~seen ~remaining json )
                              with
                              | Ok messages, Ok next ->
                                  let found =
                                    List.fold_left
                                      (fun acc m ->
                                        let ts = Pages.field "ts" m in
                                        if
                                          matches ~order ~card_id m
                                          && Slack_standing_order_provenance
                                             .compare_ts ts
                                               (Printf.sprintf "%.6f" oldest)
                                             >= 0
                                          && Slack_standing_order_provenance
                                             .compare_ts ts
                                               (Printf.sprintf "%.6f" latest)
                                             <= 0
                                        then ts :: acc
                                        else acc)
                                      found messages
                                  in
                                  if next = "" then
                                    Lwt.return_some
                                      (List.sort_uniq String.compare found)
                                  else
                                    search next (next :: seen) (remaining - 1)
                                      found
                              | _ -> Lwt.return_none)
                      in
                      let* found = search "" [] 20 [] in
                      if check () <> Ok () then Lwt.return_unit
                      else
                        match found with
                        | Some [ ts ] ->
                            confirm ts;
                            Lwt.return_unit
                        | _ -> (
                            match !failure with
                            | Some failure when failure.Grant.cls = Grant.Hard
                              ->
                                refuse failure
                            | _ -> unknown ())
                    else Lwt.return_unit)
      in
      Lwt.catch run (function
        | Lwt.Canceled -> Lwt.fail Lwt.Canceled
        | exn when Sql.is_db_error exn -> Lwt.return_unit
        | exn -> Lwt.fail exn)
  | _ -> Lwt.return_unit
