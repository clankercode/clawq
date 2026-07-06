open Telegram_api

let register ~(session_mgr : Session.t) ~key ~bot_token ~chat_id ~refresh_typing
    ~send_text =
  if Option.is_none (Session.find_rich_notifier session_mgr ~key) then
    Session.register_rich_notifier session_mgr ~key (fun msg ->
        let open Lwt.Syntax in
        match msg with
        | Rich_message.Text text ->
            let* () =
              send_chunked ~parse_mode:"MarkdownV2" ~bot_token ~chat_id
                ~text:(Telegram_format.markdown_to_mdv2 text)
                ()
            in
            refresh_typing ();
            Lwt.return Rich_message.{ message_id = "0"; callback_ids = [] }
        | Rich_message.TextWithButtons { text; button_rows } ->
            let now = Unix.gettimeofday () in
            let buttons =
              List.concat_map
                (fun row ->
                  List.map
                    (fun (btn : Rich_message.button) ->
                      (btn.label, btn.callback_id))
                    row)
                button_rows
            in
            let callback_ids =
              List.map
                (fun (label, cb_id) ->
                  Hashtbl.replace callback_routing cb_id (key, label, now);
                  cb_id)
                buttons
            in
            let* msg_id =
              send_message_with_keyboard ~disable_notification:false ~bot_token
                ~chat_id ~text ~buttons ()
            in
            refresh_typing ();
            Lwt.return Rich_message.{ message_id = msg_id; callback_ids }
        | Rich_message.Poll { question; options; allows_multiple } ->
            let* msg_id, poll_id =
              send_poll_api ~disable_notification:false ~bot_token ~chat_id
                ~question ~options ~allows_multiple ()
            in
            Hashtbl.replace poll_routing poll_id
              (key, chat_id, bot_token, options, Unix.gettimeofday ());
            refresh_typing ();
            Lwt.return Rich_message.{ message_id = msg_id; callback_ids = [] }
        | Rich_message.FileAttachment
            { filename; content; description; download_url; content_type = _ }
          ->
            let* _upload_ok =
              Lwt.catch
                (fun () ->
                  let* _r =
                    Telegram_api.send_document ~bot_token ~chat_id ~filename
                      ~content ()
                  in
                  Lwt.return true)
                (fun exn ->
                  Logs.warn (fun m ->
                      m "Telegram send_document failed: %s"
                        (Printexc.to_string exn));
                  Lwt.return false)
            in
            let* () =
              match download_url with
              | Some url ->
                  let desc =
                    if description <> "" then description else filename
                  in
                  send_text (desc ^ "\n\nDownload: " ^ url)
              | None -> Lwt.return_unit
            in
            refresh_typing ();
            Lwt.return Rich_message.{ message_id = "0"; callback_ids = [] })
