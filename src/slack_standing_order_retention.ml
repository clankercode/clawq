open Lwt.Syntax
module C = Slack_standing_order_cleanup_store
module T = Slack_standing_order_types
module Ctx = Slack_standing_order_ctx
module SO = Slack_standing_order_slack
module Grant = Slack_standing_order_grant
module Store = Slack_standing_order_store
module Sql = Slack_standing_order_sql
module Pages = Slack_standing_order_pages

type source_state = Present | Missing | Unknown

(* Synthetic typed tombstones are supported conservatively. Slack's participant
   deletion UI shape is not yet proven; denied/inaccessible is never absence. *)
let source_state ~id = function
  | Error (SO.Slack_error "file_not_found") -> Missing
  | Error _ -> Unknown
  | Ok json ->
      let file = Pages.get "file" json in
      let typed key =
        match Pages.get key file with `Null | `Bool _ -> true | _ -> false
      in
      if
        Pages.field "id" file <> id
        || (match Pages.get "mode" file with
          | `Null | `String _ -> false
          | _ -> true)
        || not (List.for_all typed [ "deleted"; "is_deleted"; "is_tombstoned" ])
      then Unknown
      else if
        List.exists
          (fun key -> Pages.get key file = `Bool true)
          [ "deleted"; "is_deleted"; "is_tombstoned" ]
      then Missing
      else if Pages.get "mode" file = `String "tombstone" then Missing
      else if
        match Pages.get "mode" file with
        | `Null | `String _ -> true
        | _ -> false
      then Present
      else Unknown

let with_owner ~ctx ~db (job : C.job) owner work =
  let lost = ref false in
  let check () =
    if
      (not !lost) && ctx.Ctx.accepting ()
      && C.owns db ~job:job.id ~owner ~now:(ctx.now ())
    then Ok ()
    else Error SO.Dispatch_refused
  in
  let rec renew () =
    let* () = ctx.wait ~seconds:30. in
    if check () = Ok () && C.renew db ~job:job.id ~owner ~now:(ctx.now ()) then
      renew ()
    else (
      lost := true;
      Lwt.return_unit)
  in
  let heartbeat =
    Lwt.catch renew (function
      | Lwt.Canceled -> Lwt.return_unit
      | _ ->
          lost := true;
          Lwt.return_unit)
  in
  Lwt.finalize
    (fun () -> SO.with_read_wait ctx.wait (fun () -> work check))
    (fun () ->
      let* () = Slack_standing_order_resolution_lease.cancel heartbeat in
      C.release db ~job:job.id ~owner ~now:(ctx.now ());
      Lwt.return_unit)

let pass ~ctx ~db ~manager ?http ?http_response (order : T.order) =
  let budget = ref 20 in
  C.refresh db order ~now:(ctx.Ctx.now ());
  let work id =
    if !budget < 3 || not (ctx.accepting ()) then Lwt.return_unit
    else
      match (C.find db id, C.claim db ~id ~now:(ctx.now ())) with
      | Some job, Some owner ->
          with_owner ~ctx ~db job owner (fun check ->
              let authorize () =
                if !budget < 3 || check () <> Ok () then Lwt.return_none
                else (
                  budget := !budget - 2;
                  let* result =
                    Grant.evaluate ~db
                      ~current_config:(fun () -> Session.get_config manager)
                      ?http ?http_response ?limiter:ctx.limiter
                      ~priority:Slack_read_limiter.Maintenance
                      ~limit:(fun () -> ctx.limit ~channel_id:order.channel_id)
                      ~before_send:check
                      ~stage:(Grant.Cleanup { fire_id = job.id })
                      ~unit_label:("cleanup:" ^ job.id) ~now:(ctx.now ())
                      (Grant.subject_of_order order)
                  in
                  match result with
                  | Ok v -> Lwt.return_some v.call
                  | Error _ -> Lwt.return_none)
              in
              let rec poll index =
                match List.nth_opt (C.sources job) index with
                | None -> Lwt.return_unit
                | Some id when !budget >= 3 && check () = Ok () -> (
                    let* call = authorize () in
                    match call with
                    | None -> Lwt.return_unit
                    | Some call ->
                        decr budget;
                        let* result =
                          SO.with_policy
                            ~priority:Slack_read_limiter.Maintenance
                            ~check:(fun () ->
                              if
                                check () = Ok ()
                                && C.reserve_source db ~job:job.id ~owner
                                     ~now:(ctx.now ()) index
                              then Ok ()
                              else Error SO.Dispatch_refused)
                            (fun () ->
                              call "files.info" [ ("file", `String id) ])
                        in
                        let missing = source_state ~id result = Missing in
                        let finished =
                          missing || index + 1 >= List.length (C.sources job)
                        in
                        C.source_result db ~job:job.id ~owner ~now:(ctx.now ())
                          ~missing ~finished;
                        if missing then Lwt.return_unit else poll (index + 1))
                | _ -> Lwt.return_unit
              in
              let* () =
                if job.missing then Lwt.return_unit
                else
                  poll
                    (if job.source_cursor >= List.length (C.sources job) then 0
                     else job.source_cursor)
              in
              let current = Option.get (C.find db job.id) in
              let* () =
                if not current.missing then Lwt.return_unit
                else
                  Lwt_list.iter_s
                    (fun (id, status) ->
                      if status = "removed" || !budget < 3 || check () <> Ok ()
                      then Lwt.return_unit
                      else
                        let* call = authorize () in
                        match call with
                        | None -> Lwt.return_unit
                        | Some call ->
                            decr budget;
                            let* result =
                              SO.with_policy
                                ~priority:Slack_read_limiter.Maintenance
                                ~check:(fun () ->
                                  if
                                    check () = Ok ()
                                    && C.file_status db ~job:job.id ~owner
                                         ~now:(ctx.now ()) id "attempted"
                                  then Ok ()
                                  else Error SO.Dispatch_refused)
                                (fun () ->
                                  call "files.delete" [ ("file", `String id) ])
                            in
                            (match result with
                            | Ok _ | Error (SO.Slack_error "file_not_found") ->
                                ignore
                                  (C.file_status db ~job:job.id ~owner
                                     ~now:(ctx.now ()) id "removed")
                            | _ -> ());
                            Lwt.return_unit)
                    (C.files db job.id)
              in
              let* () =
                if
                  current.missing
                  && List.for_all
                       (fun (_, s) -> s = "removed")
                       (C.files db job.id)
                then
                  Lwt_list.iter_s
                    (fun (thread, status) ->
                      Slack_standing_order_cleanup_note.run ~db ~job:current
                        ~owner ~now:ctx.now ~check ~authorize ~budget
                        ~channel:order.channel_id thread status)
                    (C.threads db job.id)
                else Lwt.return_unit
              in
              C.finish db ~job:job.id ~owner ~now:(ctx.now ());
              C.defer db ~job:job.id ~owner ~now:(ctx.now ());
              Lwt.return_unit)
      | _ -> Lwt.return_unit
  in
  Lwt_list.iter_s work (C.due db ~order_id:order.id ~now:(ctx.now ()))

let orders ~db ~now =
  C.ensure_schema db;
  Store.list db ~include_cancelled:true ()
  |> List.filter (fun o ->
      C.refresh db o ~now;
      C.due db ~order_id:o.T.id ~now <> [])
