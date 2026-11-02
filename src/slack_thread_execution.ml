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
        (run message)
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
      match requester route message with
      | Error reason -> refuse reason
      | Ok (user, message_id) -> (
          let* authorization =
            Slack_thread_api.authorize ~manager ~route ~user
          in
          match authorization with
          | Error reason -> refuse ("Slack queued request denied: " ^ reason)
          | Ok (_, _, external_) ->
              let previous =
                Option.bind message.message_id
                  (Slack_thread_runs.find manager key)
              in
              let r =
                match previous with
                | Some r -> r
                | None ->
                    Slack_thread_runs.create ~manager ~route ~message:message_id
                      ~owner:user
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
                        let* () = Slack_thread_runs.start r in
                        Lwt.return (Ok ()))
                      (function
                        | Lwt.Canceled -> Lwt.fail Lwt.Canceled
                        | exn -> Lwt.return (Error (Printexc.to_string exn)))
                  in
                  match started with
                  | Error _ when r.stopped ->
                      Lwt.return Session_types.Queue_cancelled
                  | Error reason ->
                      Lwt.return (Session_types.Queue_retained reason)
                  | Ok () ->
                      let admin =
                        match Session.get_db manager with
                        | Some db ->
                            Admin.is_admin ~db ~channel:"slack" ~sender_id:user
                        | None -> false
                      in
                      let current =
                        {
                          message with
                          user_group = Some (if admin then "admin" else "guest");
                          snapshot_work_type = Some Access_snapshot.Room_turn;
                          has_external_users = external_;
                        }
                      in
                      Session.register_channel_notifier manager ~key
                        (notify manager route);
                      Slack_huddle_context.with_request ~session_manager:manager
                        ~key ~channel_id:route.channel ~user_id:user ~message_id
                        (fun () ->
                          Lwt.map
                            (fun response ->
                              Session_types.Queue_executed response)
                            (run current)))
                (fun () -> Slack_thread_runs.finish r)))

let install manager =
  manager.Session.queued_execution_adapter <- Some (execute manager)
