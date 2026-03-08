let mk_update ~update_id ~chat_id =
  {
    Telegram.update_id;
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

let suite =
  [
    Alcotest.test_case "accepts first seen telegram update" `Quick
      test_should_process_update_accepts_first_seen;
    Alcotest.test_case "rejects duplicate telegram update" `Quick
      test_should_process_update_rejects_duplicate;
    Alcotest.test_case "scopes duplicate tracking by chat" `Quick
      test_should_process_update_scopes_by_chat;
  ]
