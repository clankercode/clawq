include Teams_api

let register ~session_manager ~key ~config ~service_url ~conversation_id
    ~reply_to_id ~send_reply ~send_adaptive_card =
  if Option.is_none (Session.find_rich_notifier session_manager ~key) then
    Session.register_rich_notifier session_manager ~key (fun msg ->
        let open Lwt.Syntax in
        match msg with
        | Rich_message.TextWithButtons { text; button_rows } ->
            let card =
              Question_presenter.build_teams_card_from_buttons ~text
                ~button_rows
            in
            let* _id =
              send_adaptive_card ~config ~service_url ~conversation_id
                ~reply_to_id ~card ()
            in
            Lwt.return Rich_message.{ message_id = "0"; callback_ids = [] }
        | Rich_message.Poll { question; options; allows_multiple } ->
            let card =
              Question_presenter.build_teams_poll_card ~question ~options
            in
            ignore allows_multiple;
            let* _id =
              send_adaptive_card ~config ~service_url ~conversation_id
                ~reply_to_id ~card ()
            in
            Lwt.return Rich_message.{ message_id = "0"; callback_ids = [] }
        | Rich_message.Text text ->
            let* _id =
              send_reply ?alert:(Some false) ~config ~service_url
                ~conversation_id ~reply_to_id ~text ?mention:None ()
            in
            Lwt.return Rich_message.{ message_id = "0"; callback_ids = [] }
        | Rich_message.FileAttachment _ ->
            Lwt.return Rich_message.{ message_id = "0"; callback_ids = [] })
