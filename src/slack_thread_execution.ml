open Lwt.Syntax

let requester (route : Slack_thread_route.t) (q : Session_types.queued_message)
    =
  match (q.channel, q.channel_id, q.sender_id, q.message_id) with
  | Some "slack", Some channel, Some user, Some message
    when channel = route.channel
         && Slack_huddles.valid_id "UW" user
         && Slack_huddles.valid_ts message ->
      Ok (user, message)
  | _ ->
      Error
        "Native Slack request lacks original verified human/message/room \
         provenance; retained"

let notify manager route text =
  let* _ =
    Slack_thread_api.call_current_exn ~manager ~route "chat.postMessage"
      [
        ("channel", `String route.Slack_thread_route.channel);
        ("thread_ts", `String route.root);
        ("text", `String text);
      ]
  in
  Lwt.return_unit

let execute manager ~key ~(message : Session_types.queued_message) ~run =
  match Slack_thread_route.of_key key with
  | None ->
      Lwt.map
        (fun response -> Session_types.Queue_executed response)
        (run
           ~verify_before_model:(fun _ -> Lwt.return_unit)
           ~room_classification:None message)
  | Some route -> (
      let refuse reason =
        let* () =
          match
            Option.bind message.message_id (Slack_thread_runs.find manager key)
          with
          | Some r -> Slack_thread_runs.finish r
          | None -> Lwt.return_unit
        in
        Lwt.return (Session_types.Queue_retained reason)
      in
      if not (Slack_thread_runs.accepting manager) then
        refuse "Slack connector shut down; request retained"
      else
        match
          match message.execution_origin with
          | Human_request -> requester route message
          | Synthetic (Some owner) when Slack_huddles.valid_id "UW" owner ->
              Ok (owner, "synthetic:" ^ Temp_downloads.generate_token ())
          | Synthetic _ ->
              Error
                "Synthetic Slack work lacks a persisted human owner; retained"
        with
        | Error reason -> refuse reason
        | Ok (user, message_id) -> (
            let* authorization =
              Slack_thread_api.authorize ~manager ~route ~user
            in
            match authorization with
            | Error reason -> refuse ("Slack queued request denied: " ^ reason)
            | Ok _ when not (Slack_thread_runs.accepting manager) ->
                refuse
                  "Slack connector shut down during authorization; request \
                   retained"
            | Ok _ ->
                let previous =
                  Option.bind message.message_id
                    (Slack_thread_runs.find manager key)
                in
                let r =
                  match previous with
                  | Some r -> r
                  | None ->
                      Slack_thread_runs.create ~manager ~route
                        ~message:message_id ~owner:user
                in
                Lwt.finalize
                  (fun () ->
                    let* started =
                      Lwt.catch
                        (fun () ->
                          let* () =
                            if previous = None then Slack_thread_runs.control r
                            else Lwt.return_unit
                          in
                          let* classification =
                            Slack_thread_runs.start_verified
                              ~persist_owner:
                                (message.execution_origin = Human_request)
                              r
                          in
                          Lwt.return (Ok classification))
                        (function
                          | Lwt.Canceled -> Lwt.fail Lwt.Canceled
                          | exn -> Lwt.return (Error (Printexc.to_string exn)))
                    in
                    match started with
                    | Error _ when r.stopped ->
                        Lwt.return Session_types.Queue_cancelled
                    | Error reason ->
                        Lwt.return (Session_types.Queue_retained reason)
                    | Ok _ when r.stopped ->
                        Lwt.return Session_types.Queue_cancelled
                    | Ok _ when not (Slack_thread_runs.owns r) ->
                        Lwt.return
                          (Session_types.Queue_retained
                             "Slack task finished before queued execution; \
                              request retained")
                    | Ok classification ->
                        let admin =
                          match Session.get_db manager with
                          | Some db ->
                              Admin.is_admin ~db ~channel:"slack"
                                ~sender_id:user
                          | None -> false
                        in
                        let current =
                          {
                            message with
                            user_group =
                              Some (if admin then "admin" else "guest");
                            snapshot_work_type = Some Access_snapshot.Room_turn;
                            has_external_users =
                              classification.has_external_users;
                          }
                        in
                        let suppressed =
                          Session.find_registered_notifier manager ~key = None
                        in
                        Session.register_channel_notifier manager ~key
                          (notify manager route);
                        let with_authority f =
                          match message.execution_origin with
                          | Human_request ->
                              (* This adapter runs live native turns as well
                                 as queue drains: only a live ingress cell for
                                 this exact message is not a drain. *)
                              let drained =
                                not
                                  (Slack_huddle_context.live ~key
                                     ~channel_id:route.channel ~user_id:user
                                     ~message_id)
                              in
                              Slack_huddle_context.with_request ~drained
                                ~session_manager:manager ~key
                                ~channel_id:route.channel ~user_id:user
                                ~message_id f
                          | Synthetic _ ->
                              Lwt.with_value Slack_huddle_context.current None
                                (fun () ->
                                  if suppressed then
                                    Session.with_suppressed_channel_output
                                      manager ~key f
                                  else f ())
                        in
                        with_authority (fun () ->
                            Lwt.catch
                              (fun () ->
                                Lwt.map
                                  (fun response ->
                                    Session_types.Queue_executed response)
                                  (run
                                     ~verify_before_model:
                                       (Slack_thread_runs.verify_before_model r)
                                     ~room_classification:(Some classification)
                                     current))
                              (function
                                | Session_queue.Request_retained _
                                  when r.stopped ->
                                    Lwt.return Session_types.Queue_cancelled
                                | Session_queue.Request_retained reason ->
                                    Lwt.return
                                      (Session_types.Queue_retained reason)
                                | exn -> Lwt.fail exn)))
                  (fun () -> Slack_thread_runs.finish r)))

let install manager =
  manager.Session.queued_execution_adapter <- Some (execute manager);
  manager.Session.synthetic_owner <-
    Some (fun ~key -> Slack_thread_route.load_owner manager key)

(** Completions are data from their original task, not a request by whichever
    human happens to hold the Session now. Older tasks without an origin use the
    persisted thread owner; conflicting origins fail closed. *)
let completion_owner manager ~key (task : Background_task.task) =
  match Slack_thread_route.of_key key with
  | None -> None
  | Some route ->
      Some
        (match task.origin_json with
        | None -> Slack_thread_route.load_owner manager key
        | Some json -> (
            match Room_origin.of_json_string_opt json with
            | Some origin
              when origin.connector = Some "slack"
                   && origin.room_id = Some route.channel
                   && origin.workspace_id = Some route.team
                   && origin.thread_id = Some route.root ->
                origin.requester_id
            | _ -> None))
