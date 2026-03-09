let mk_update ?(message_id = 0) ?(user_id = None) ?(text = "hello")
    ?voice_file_id ?photo_file_id ?document_file_id ?document_name ?caption
    ~update_id ~chat_id () =
  {
    Telegram.update_id;
    message_id;
    chat_id;
    user_id = Option.value user_id ~default:chat_id;
    text;
    voice_file_id;
    photo_file_id;
    document_file_id;
    document_name;
    caption;
  }

let reset_seen () = Hashtbl.reset Telegram.recently_seen_updates

let test_should_process_update_accepts_first_seen () =
  reset_seen ();
  Alcotest.(check bool)
    "first occurrence accepted" true
    (Telegram.should_process_update (mk_update ~update_id:42 ~chat_id:"1" ()))

let test_should_process_update_rejects_duplicate () =
  reset_seen ();
  ignore
    (Telegram.should_process_update (mk_update ~update_id:42 ~chat_id:"1" ()));
  Alcotest.(check bool)
    "duplicate rejected" false
    (Telegram.should_process_update (mk_update ~update_id:42 ~chat_id:"1" ()))

let test_should_process_update_scopes_by_chat () =
  reset_seen ();
  ignore
    (Telegram.should_process_update (mk_update ~update_id:42 ~chat_id:"1" ()));
  Alcotest.(check bool)
    "same update id other chat allowed" true
    (Telegram.should_process_update (mk_update ~update_id:42 ~chat_id:"2" ()))

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

let poll_error_testable =
  Alcotest.testable
    (fun fmt e ->
      match e with
      | Telegram.Conflict_webhook ->
          Format.pp_print_string fmt "Conflict_webhook"
      | Telegram.Conflict_duplicate_poller ->
          Format.pp_print_string fmt "Conflict_duplicate_poller"
      | Telegram.Other_error n -> Format.fprintf fmt "Other_error(%d)" n)
    ( = )

let test_parse_conflict_webhook () =
  let body =
    {|{"ok":false,"error_code":409,"description":"Conflict: can't use getUpdates method while webhook is active; use deleteWebhook to delete the webhook first"}|}
  in
  Alcotest.check poll_error_testable "webhook conflict"
    Telegram.Conflict_webhook
    (Telegram.parse_conflict_description body)

let test_parse_conflict_duplicate_poller () =
  let body =
    {|{"ok":false,"error_code":409,"description":"Conflict: terminated by other getUpdates request; make sure that only one bot instance is running"}|}
  in
  Alcotest.check poll_error_testable "duplicate poller"
    Telegram.Conflict_duplicate_poller
    (Telegram.parse_conflict_description body)

let test_parse_conflict_malformed () =
  Alcotest.check poll_error_testable "malformed defaults to duplicate poller"
    Telegram.Conflict_duplicate_poller
    (Telegram.parse_conflict_description "not json")

let test_text_coalescing_candidate_for_plain_text () =
  Alcotest.(check bool)
    "plain text can coalesce" true
    (Telegram.is_text_coalescing_candidate
       (mk_update ~update_id:1 ~message_id:10 ~chat_id:"1" ~text:"part 1" ()))

let test_text_coalescing_candidate_rejects_commands_and_media () =
  let command =
    mk_update ~update_id:1 ~message_id:10 ~chat_id:"1" ~text:"/update" ()
  in
  let photo =
    mk_update ~update_id:2 ~message_id:11 ~chat_id:"1" ~text:"caption"
      ~photo_file_id:"photo-1" ()
  in
  Alcotest.(check bool)
    "slash commands do not coalesce" false
    (Telegram.is_text_coalescing_candidate command);
  Alcotest.(check bool)
    "media messages do not coalesce" false
    (Telegram.is_text_coalescing_candidate photo)

let test_can_coalesce_text_updates_for_adjacent_fragments () =
  let pending =
    {
      Telegram.update =
        mk_update ~update_id:1 ~message_id:10 ~chat_id:"1" ~user_id:(Some "u")
          ~text:"hello " ();
      last_seen_at = 100.0;
      generation = 0;
    }
  in
  let incoming =
    mk_update ~update_id:2 ~message_id:11 ~chat_id:"1" ~user_id:(Some "u")
      ~text:"world" ()
  in
  Alcotest.(check bool)
    "adjacent fragments merge" true
    (Telegram.can_coalesce_text_updates ~now:100.2 pending incoming)

let test_can_coalesce_text_updates_rejects_gap_and_expiry () =
  let pending =
    {
      Telegram.update =
        mk_update ~update_id:1 ~message_id:10 ~chat_id:"1" ~user_id:(Some "u")
          ~text:"hello " ();
      last_seen_at = 100.0;
      generation = 0;
    }
  in
  let wrong_message =
    mk_update ~update_id:2 ~message_id:13 ~chat_id:"1" ~user_id:(Some "u")
      ~text:"world" ()
  in
  let expired =
    mk_update ~update_id:3 ~message_id:11 ~chat_id:"1" ~user_id:(Some "u")
      ~text:"world" ()
  in
  Alcotest.(check bool)
    "non-consecutive ids do not merge" false
    (Telegram.can_coalesce_text_updates ~now:100.2 pending wrong_message);
  Alcotest.(check bool)
    "expired fragments do not merge" false
    (Telegram.can_coalesce_text_updates
       ~now:(100.0 +. Telegram.text_coalesce_window_seconds +. 0.01)
       pending expired)

let test_merge_text_updates_keeps_latest_metadata () =
  let older =
    {
      Telegram.update =
        mk_update ~update_id:1 ~message_id:10 ~chat_id:"1" ~user_id:(Some "u")
          ~text:"hello " ();
      last_seen_at = 100.0;
      generation = 0;
    }
  in
  let newer =
    mk_update ~update_id:2 ~message_id:11 ~chat_id:"1" ~user_id:(Some "u")
      ~text:"world" ()
  in
  let merged = Telegram.merge_text_updates older newer in
  Alcotest.(check string) "text concatenated" "hello world" merged.text;
  Alcotest.(check int) "latest message id kept" 11 merged.message_id;
  Alcotest.(check int) "latest update id kept" 2 merged.update_id

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
    Alcotest.test_case "parse 409 conflict: webhook" `Quick
      test_parse_conflict_webhook;
    Alcotest.test_case "parse 409 conflict: duplicate poller" `Quick
      test_parse_conflict_duplicate_poller;
    Alcotest.test_case "parse 409 conflict: malformed body" `Quick
      test_parse_conflict_malformed;
    Alcotest.test_case "plain text updates can coalesce" `Quick
      test_text_coalescing_candidate_for_plain_text;
    Alcotest.test_case "commands and media do not coalesce" `Quick
      test_text_coalescing_candidate_rejects_commands_and_media;
    Alcotest.test_case "adjacent telegram fragments can coalesce" `Quick
      test_can_coalesce_text_updates_for_adjacent_fragments;
    Alcotest.test_case "coalescing rejects gaps and expiry" `Quick
      test_can_coalesce_text_updates_rejects_gap_and_expiry;
    Alcotest.test_case "merged telegram update keeps latest metadata" `Quick
      test_merge_text_updates_keeps_latest_metadata;
  ]
