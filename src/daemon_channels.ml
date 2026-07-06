(** Non-Telegram channel startup fanout for the daemon. *)

let start_non_telegram_channels ~(config : Runtime_config.t)
    ~(session_manager : Session.t) ~db ~discord_message_limiter
    ~slack_event_limiter =
  Lwt.async (fun () ->
      Lwt.catch
        (fun () ->
          Discord.start ~config ~session_manager ~db
            ~message_limiter:discord_message_limiter)
        (fun exn ->
          Logs.err (fun m ->
              m "Discord channel error: %s" (Printexc.to_string exn));
          Lwt.return_unit));
  (match config.channels.slack with
  | Some sc when sc.socket_mode && sc.app_token <> "" ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () ->
              Slack_socket.start ~config ~session_manager
                ~event_limiter:slack_event_limiter)
            (fun exn ->
              Logs.err (fun m ->
                  m "Slack Socket Mode error: %s" (Printexc.to_string exn));
              Lwt.return_unit))
  | _ -> ());
  Lwt.async (fun () ->
      Lwt.catch
        (fun () -> Mattermost.start ~config ~session_manager)
        (fun exn ->
          Logs.err (fun m ->
              m "Mattermost channel error: %s" (Printexc.to_string exn));
          Lwt.return_unit));
  Lwt.async (fun () ->
      Lwt.catch
        (fun () -> Imessage.start ~config ~session_manager)
        (fun exn ->
          Logs.err (fun m ->
              m "iMessage channel error: %s" (Printexc.to_string exn));
          Lwt.return_unit));
  (match config.channels.signal with
  | Some _ ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () -> Signal.start ~config ~session_manager)
            (fun exn ->
              Logs.err (fun m ->
                  m "Signal channel error: %s" (Printexc.to_string exn));
              Lwt.return_unit))
  | None -> ());
  (match config.channels.matrix with
  | Some _ ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () -> Matrix.start ~config ~session_manager)
            (fun exn ->
              Logs.err (fun m ->
                  m "Matrix channel error: %s" (Printexc.to_string exn));
              Lwt.return_unit))
  | None -> ());
  (match config.channels.irc with
  | Some _ ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () -> Irc.start ~config ~session_manager)
            (fun exn ->
              Logs.err (fun m ->
                  m "IRC channel error: %s" (Printexc.to_string exn));
              Lwt.return_unit))
  | None -> ());
  (match config.channels.email with
  | Some _ ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () -> Email_channel.start ~config ~session_manager)
            (fun exn ->
              Logs.err (fun m ->
                  m "Email channel error: %s" (Printexc.to_string exn));
              Lwt.return_unit))
  | None -> ());
  (match config.channels.nostr with
  | Some _ ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () -> Nostr.start ~config ~session_manager)
            (fun exn ->
              Logs.err (fun m ->
                  m "Nostr channel error: %s" (Printexc.to_string exn));
              Lwt.return_unit))
  | None -> ());
  (match config.channels.dingtalk with
  | Some _ ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () -> Dingtalk.start ~config ~session_manager)
            (fun exn ->
              Logs.err (fun m ->
                  m "DingTalk channel error: %s" (Printexc.to_string exn));
              Lwt.return_unit))
  | None -> ());
  (match config.channels.onebot with
  | Some _ ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () -> Onebot.start ~config ~session_manager)
            (fun exn ->
              Logs.err (fun m ->
                  m "OneBot channel error: %s" (Printexc.to_string exn));
              Lwt.return_unit))
  | None -> ());
  (match config.channels.lark with
  | Some lk when lk.enabled ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () -> Lark.start ~config ~session_manager)
            (fun exn ->
              Logs.err (fun m ->
                  m "Lark channel error: %s" (Printexc.to_string exn));
              Lwt.return_unit))
  | Some _ | None -> ());
  match config.channels.teams with
  | Some _ ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () -> Teams.start ~config ~_session_manager:session_manager)
            (fun exn ->
              Logs.err (fun m ->
                  m "Teams channel error: %s" (Printexc.to_string exn));
              Lwt.return_unit))
  | None -> ()
