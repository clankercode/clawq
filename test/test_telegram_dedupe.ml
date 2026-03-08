let mk_update ~update_id ~chat_id =
  {
    Telegram.update_id;
    message_id = 0;
    chat_id;
    user_id = chat_id;
    text = "hello";
    voice_file_id = None;
    photo_file_id = None;
    document_file_id = None;
    document_name = None;
    caption = None;
  }

let reset_seen () = Hashtbl.reset Telegram.recently_seen_updates

let test_should_process_update_accepts_first_seen () =
  reset_seen ();
  Alcotest.(check bool)
    "first occurrence accepted" true
    (Telegram.should_process_update (mk_update ~update_id:42 ~chat_id:"1"))

let test_should_process_update_rejects_duplicate () =
  reset_seen ();
  ignore (Telegram.should_process_update (mk_update ~update_id:42 ~chat_id:"1"));
  Alcotest.(check bool)
    "duplicate rejected" false
    (Telegram.should_process_update (mk_update ~update_id:42 ~chat_id:"1"))

let test_should_process_update_scopes_by_chat () =
  reset_seen ();
  ignore (Telegram.should_process_update (mk_update ~update_id:42 ~chat_id:"1"));
  Alcotest.(check bool)
    "same update id other chat allowed" true
    (Telegram.should_process_update (mk_update ~update_id:42 ~chat_id:"2"))

let test_send_silent_chunked_forces_disable_notification () =
  let calls = ref [] in
  let fake_send_chunked ?(disable_notification = false) ?parse_mode:_
      ~bot_token:_ ~chat_id ~text () =
    calls := (chat_id, text, disable_notification) :: !calls;
    Lwt.return_unit
  in
  Lwt_main.run
    (Telegram.send_silent_chunked fake_send_chunked ~bot_token:"token"
       ~chat_id:"chat-1" ~text:"hello");
  Alcotest.(check (list (triple string string bool)))
    "silent chunked send"
    [ ("chat-1", "hello", true) ]
    (List.rev !calls)

let suite =
  [
    Alcotest.test_case "accepts first seen telegram update" `Quick
      test_should_process_update_accepts_first_seen;
    Alcotest.test_case "rejects duplicate telegram update" `Quick
      test_should_process_update_rejects_duplicate;
    Alcotest.test_case "scopes duplicate tracking by chat" `Quick
      test_should_process_update_scopes_by_chat;
    Alcotest.test_case "send silent chunked forces disable notification" `Quick
      test_send_silent_chunked_forces_disable_notification;
  ]
