open Lwt.Syntax
(** The only automatic final-text adapter. Capture ingress authority before
    Slack clears its live tool context; each send still checks current policy.
*)

type send =
  channel_id:string ->
  thread_ts:string option ->
  text:string ->
  (unit, Slack_canvas.error) result Lwt.t

(* [destination] is a server-owned binding for explicit legacy parents and
   queued follow-ups without a live ingress cell. It never supplies authority;
   the same current policy/membership checks still apply. *)
let make ~manager ~route ?legacy_post ?destination () : send =
  let ingress =
    match Lwt.get Slack_huddle_context.current with
    | Some cell -> !cell
    | None -> None
  in
  let inferred =
    match (route, ingress) with
    | Some (r : Slack_thread_route.t), _ ->
        let requester =
          match ingress with
          | Some t
            when t.request.manager == manager
                 && t.request.channel_id = r.channel ->
              t.request.user_id
          | _ ->
              Option.value
                (Slack_thread_route.load_owner manager
                   (Slack_thread_route.key r))
                ~default:""
        in
        Some
          {
            Slack_canvas.session_key = Slack_thread_route.key r;
            channel_id = r.channel;
            thread_ts = Some r.root;
            requester;
          }
    | None, Some t when t.request.manager == manager ->
        Some
          {
            Slack_canvas.session_key = t.request.key;
            channel_id = t.request.channel_id;
            thread_ts = None;
            requester = t.request.user_id;
          }
    | _ -> None
  in
  let destination =
    match destination with Some _ -> destination | None -> inferred
  in
  let authority =
    Option.map (Slack_canvas.make_authority ~manager ?route) destination
  in
  fun ~channel_id ~thread_ts ~text ->
    match (destination, authority) with
    | Some d, Some a when d.channel_id = channel_id && d.thread_ts = thread_ts
      -> (
        if Slack_api.blank_text text then Lwt.return (Ok ())
        else
          let* allowed = Slack_canvas.protect a.authorize in
          match allowed with
          | Error e -> Lwt.return (Error e)
          | Ok () -> (
              match (legacy_post, route, thread_ts) with
              | Some post, None, None -> (
                  match
                    Slack_thread_api.delivery_token ~manager
                      ~session_key:d.session_key ~channel:channel_id
                      "chat.postMessage" "POST"
                  with
                  | Error _ -> Lwt.return (Error Slack_canvas.Policy_denied)
                  | Ok _ ->
                      Lwt.catch
                        (fun () ->
                          match (Session.get_config manager).channels.slack with
                          | None ->
                              Lwt.return (Error Slack_canvas.Policy_denied)
                          | Some sc ->
                              let+ () =
                                post ~bot_token:sc.bot_token ~channel_id ~text
                              in
                              Ok ())
                        (function
                          | Lwt.Canceled -> Lwt.fail Lwt.Canceled
                          | _ ->
                              Lwt.return
                                (Error (Slack_canvas.Transport "text_failed"))))
              | _ ->
                  let+ result =
                    a.call "chat.postMessage"
                      ([
                         ("channel", `String channel_id);
                         ("text", `String text);
                         ("unfurl_links", `Bool false);
                         ("unfurl_media", `Bool false);
                         ("parse", `String "none");
                       ]
                      @
                      match thread_ts with
                      | None -> []
                      | Some ts -> [ ("thread_ts", `String ts) ])
                  in
                  Result.map (fun _ -> ()) result))
    | _ -> Lwt.return (Error Slack_canvas.Policy_denied)
