open Lwt.Syntax

let send ~manager ~post_text ~(target : Slack_reply_delivery.target) body =
  if Session.should_suppress_response body then Lwt.return_unit
  else
    let failed () =
      Lwt.fail (Slack_thread_api.Delivery_denied "Slack final delivery failed")
    in
    let config = Session.get_config manager in
    let deps = Slack_reply_delivery.make_deps ~manager ~post_text in
    (* Freeze presentation before metadata I/O; security still reads live config. *)
    let deps =
      { deps with Slack_reply_delivery.current_config = (fun () -> config) }
    in
    let policy =
      match config.channels.slack with
      | Some sc -> sc.long_reply
      | None -> Slack_config_types.default_long_reply
    in
    let deliver target =
      let* result = Slack_reply_delivery.deliver ~deps ~target ~body in
      match result with Ok _ -> Lwt.return_unit | Error _ -> failed ()
    in
    if
      policy.mode <> Slack_config_types.Canvas
      || not (Slack_long_reply_policy.overflows policy body)
    then deliver target
    else
      let* metadata =
        Slack_canvas.protect (fun () ->
            deps.call target "conversations.info"
              [ ("channel", `String target.channel_id) ])
      in
      match metadata with
      | Error Slack_canvas.Policy_denied -> failed ()
      | _ ->
          let conversation =
            match metadata with
            | Ok json -> (
                let room = Slack_canvas.get "channel" json in
                if Slack_canvas.field "id" room <> target.channel_id then
                  Slack_reply_context.Unknown
                else
                  match
                    ( Slack_canvas.get "is_im" room,
                      Slack_canvas.get "is_mpim" room )
                  with
                  | `Bool true, (`Bool false | `Null) ->
                      Slack_reply_context.Direct
                  | `Bool false, `Bool true -> Group_direct
                  | `Bool false, `Bool false -> Channel
                  | _ -> Unknown)
            | Error _ -> Slack_reply_context.Unknown
          in
          deliver { target with conversation }

let send_queued ~manager ~key ~(message : Session_types.queued_message)
    ~fallback body =
  let route = Slack_thread_route.of_key key in
  let destination =
    match route with
    | Some route -> (
        let requester =
          match message.execution_origin with
          | Human_request -> (
              match
                ( message.channel,
                  message.channel_id,
                  message.sender_id,
                  message.message_id )
              with
              | Some "slack", Some channel, Some user, Some ts
                when channel = route.channel
                     && Slack_huddles.valid_id "UW" user
                     && Slack_huddles.valid_ts ts ->
                  Ok user
              | _ -> Error "Missing Slack provenance")
          | Synthetic (Some owner) when Slack_huddles.valid_id "UW" owner ->
              Ok owner
          | Synthetic _ -> Error "Missing Slack owner"
        in
        match requester with
        | Error _ -> Some (Error ())
        | Ok requester -> Some (Ok (route.channel, Some route.root, requester)))
    | None when message.channel = Some "slack" && message.deferred_followup ->
        Some
          (match (message.channel_id, message.sender_id) with
          | Some channel, Some requester
            when Slack_huddles.valid_id "CDG" channel
                 && Slack_huddles.valid_id "UW" requester ->
              Ok (channel, None, requester)
          | _ -> Error ())
    | None -> None
  in
  match destination with
  | None -> fallback body
  | Some (Error ()) ->
      Lwt.fail
        (Slack_thread_api.Delivery_denied "Slack final provenance missing")
  | Some (Ok (channel_id, thread_ts, requester)) ->
      let target : Slack_reply_delivery.target =
        {
          session_key = key;
          channel_id;
          thread_ts;
          requester;
          source_ts = message.message_id;
          conversation = Slack_reply_context.Unknown;
        }
      in
      let post_text =
        Slack_final_text.make ~manager ~route
          ~destination:(Slack_reply_delivery.destination target)
          ()
      in
      send ~manager ~post_text ~target body

let install manager =
  Session.set_queued_final_sender manager (Some (send_queued ~manager))
