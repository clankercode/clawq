open Lwt.Syntax

type t = {
  final_text : Slack_final_text.send;
  post : text:string -> unit Lwt.t;
  post_with_id : text:string -> string Lwt.t;
  edit : ts:string -> text:string -> unit Lwt.t;
  delete : ts:string -> unit Lwt.t;
  reaction :
    method_:string -> timestamp:string -> emoji_name:string -> unit Lwt.t;
}

let make ~manager ~route ~channel_id ~thread_ts ?legacy_post () =
  let current f =
    match (Session.get_config manager).channels.slack with
    | Some sc -> f sc.bot_token
    | None -> Lwt.fail (Slack_thread_api.Delivery_denied "Slack disabled")
  in
  let native method_ params =
    match route with
    | Some route ->
        Slack_thread_api.call_current_exn ~manager ~route method_ params
    | None -> assert false
  in
  let parent =
    match thread_ts with None -> [] | Some ts -> [ ("thread_ts", `String ts) ]
  in
  let post ~text =
    Slack_api.skip_blank ~channel_id ~what:"message" text (fun () ->
        match route with
        | Some _ ->
            let+ _ =
              native "chat.postMessage"
                ([ ("channel", `String channel_id); ("text", `String text) ]
                @ parent)
            in
            ()
        | None ->
            current (fun bot_token ->
                match thread_ts with
                | Some thread_ts ->
                    Slack_api.send_message_reply ~bot_token ~channel_id ~text
                      ~thread_ts ()
                | None ->
                    (Option.value legacy_post ~default:Slack_api.send_message)
                      ~bot_token ~channel_id ~text))
  in
  let post_with_id ~text =
    if Slack_api.blank_text text then (
      Slack_api.log_blank ~channel_id ~what:"message";
      Lwt.return "")
    else
      match route with
      | Some _ ->
          let+ j =
            native "chat.postMessage"
              ([ ("channel", `String channel_id); ("text", `String text) ]
              @ parent)
          in
          Slack_huddles.field "ts" j
      | None ->
          current (fun bot_token ->
              Slack_api.send_message_with_id ~bot_token ~channel_id ~text)
  in
  let edit ~ts ~text =
    Slack_api.skip_blank ~channel_id ~what:"edit" text (fun () ->
        match route with
        | Some _ ->
            let+ _ =
              native "chat.update"
                [
                  ("channel", `String channel_id);
                  ("ts", `String ts);
                  ("text", `String text);
                ]
            in
            ()
        | None ->
            current (fun bot_token ->
                Slack_api.edit_message ~bot_token ~channel_id ~ts ~text))
  in
  let delete ~ts =
    match route with
    | Some _ ->
        let+ _ =
          native "chat.delete"
            [ ("channel", `String channel_id); ("ts", `String ts) ]
        in
        ()
    | None ->
        current (fun bot_token ->
            Slack_api.delete_message ~bot_token ~channel_id ~ts)
  in
  let reaction ~method_ ~timestamp ~emoji_name =
    match route with
    | Some route ->
        let+ _ =
          Slack_thread_api.call_current ~manager ~route method_
            [
              ("channel", `String channel_id);
              ("timestamp", `String timestamp);
              ("name", `String emoji_name);
            ]
        in
        ()
    | None ->
        current (fun bot_token ->
            let f =
              if method_ = "reactions.add" then Slack_api.add_reaction
              else Slack_api.remove_reaction
            in
            f ~bot_token ~channel_id ~timestamp ~emoji_name)
  in
  let destination =
    match Lwt.get Slack_huddle_context.current with
    | Some cell ->
        Option.map
          (fun (t : Slack_huddle_context.turn) ->
            {
              Slack_canvas.session_key = t.request.key;
              channel_id;
              thread_ts;
              requester = t.request.user_id;
            })
          !cell
    | None -> None
  in
  {
    final_text =
      Slack_final_text.make ~manager ~route ?legacy_post ?destination ();
    post;
    post_with_id;
    edit;
    delete;
    reaction;
  }

let status_notifier t : Status_message.notifier =
  {
    send = (fun ?parse_mode:_ text -> t.post_with_id ~text);
    edit =
      (fun ts ?parse_mode:_ text ->
        let+ () = t.edit ~ts ~text in
        None);
    delete = (fun ts -> t.delete ~ts);
  }
