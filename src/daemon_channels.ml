(** Non-Telegram channel startup fanout for the daemon. *)

let start_non_telegram_channels ~(config : Runtime_config.t)
    ~(session_manager : Session.t) ~db ~discord_message_limiter
    ~slack_event_limiter =
  let always (_ : Runtime_config.t) = true in
  let has f (c : Runtime_config.t) = Option.is_some (f c.channels) in
  (* Order preserved from the previous hand-written fanout. Each channel closes
     over exactly the arguments it needs; the supervision (async + catch + log)
     is applied uniformly by the fold below. *)
  let channels : Channel.t list =
    [
      {
        Channel.name = "Discord";
        enabled = always;
        start =
          (fun () ->
            Discord.start ~config ~session_manager ~db
              ~message_limiter:discord_message_limiter);
      };
      {
        name = "Slack Socket Mode";
        enabled =
          (fun (c : Runtime_config.t) ->
            match c.channels.slack with
            | Some sc -> sc.socket_mode && sc.app_token <> ""
            | None -> false);
        start =
          (fun () ->
            Slack_socket.start ~config ~session_manager
              ~event_limiter:slack_event_limiter);
      };
      {
        name = "Mattermost";
        enabled = always;
        start = (fun () -> Mattermost.start ~config ~session_manager);
      };
      {
        name = "iMessage";
        enabled = always;
        start = (fun () -> Imessage.start ~config ~session_manager);
      };
      {
        name = "Signal";
        enabled = has (fun ch -> ch.signal);
        start = (fun () -> Signal.start ~config ~session_manager);
      };
      {
        name = "Matrix";
        enabled = has (fun ch -> ch.matrix);
        start = (fun () -> Matrix.start ~config ~session_manager);
      };
      {
        name = "IRC";
        enabled = has (fun ch -> ch.irc);
        start = (fun () -> Irc.start ~config ~session_manager);
      };
      {
        name = "Email";
        enabled = has (fun ch -> ch.email);
        start = (fun () -> Email_channel.start ~config ~session_manager);
      };
      {
        name = "Nostr";
        enabled = has (fun ch -> ch.nostr);
        start = (fun () -> Nostr.start ~config ~session_manager);
      };
      {
        name = "DingTalk";
        enabled = has (fun ch -> ch.dingtalk);
        start = (fun () -> Dingtalk.start ~config ~session_manager);
      };
      {
        name = "OneBot";
        enabled = has (fun ch -> ch.onebot);
        start = (fun () -> Onebot.start ~config ~session_manager);
      };
      {
        name = "Lark";
        enabled =
          (fun (c : Runtime_config.t) ->
            match c.channels.lark with Some lk -> lk.enabled | None -> false);
        start = (fun () -> Lark.start ~config ~session_manager);
      };
      {
        name = "Teams";
        enabled = has (fun ch -> ch.teams);
        start =
          (fun () -> Teams.start ~config ~_session_manager:session_manager);
      };
    ]
  in
  List.iter
    (fun (ch : Channel.t) ->
      if ch.enabled config then
        Lwt.async (fun () ->
            Lwt.catch ch.start (fun exn ->
                Logs.err (fun m ->
                    m "%s channel error: %s" ch.name (Printexc.to_string exn));
                Lwt.return_unit)))
    channels
