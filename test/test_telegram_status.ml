let test_status_notifier_keeps_old_message_when_reanchor_send_fails () =
  let sent = ref [] in
  let deleted = ref [] in
  let transport : Telegram.status_transport =
    {
      send_with_id =
        (fun ?disable_notification:_
          ?parse_mode:_
          ~bot_token:_
          ~chat_id:_
          ~text
          ()
        ->
          sent := text :: !sent;
          Lwt.return "0");
      edit_text =
        (fun ?parse_mode:_ ~bot_token:_ ~chat_id:_ ~message_id:_ ~text:_ () ->
          Lwt.return_unit);
      delete_message =
        (fun ~bot_token:_ ~chat_id:_ ~message_id () ->
          deleted := message_id :: !deleted;
          Lwt.return_unit);
    }
  in
  let notifier =
    Telegram.make_status_notifier_with_transport transport ~bot_token:"token"
      ~chat_id:"chat-1"
  in
  Hashtbl.replace Telegram.latest_chat_msg_id "chat-1" 99;
  let reanchor_result =
    Lwt_main.run (notifier.edit "10" ~parse_mode:"HTML" "status update")
  in
  Alcotest.(check (option string))
    "failed replacement keeps existing message" None reanchor_result;
  Alcotest.(check int) "old message not deleted" 0 (List.length !deleted);
  Alcotest.(check int) "replacement send attempted once" 1 (List.length !sent)

let test_tool_result_details_callbacks_are_scoped () =
  let cb1 =
    Telegram.register_tool_result_details ~chat_id:"chat-a" ~user_id:"user-a"
      "first details"
  in
  let cb2 =
    Telegram.register_tool_result_details ~chat_id:"chat-b" ~user_id:"user-b"
      "second details"
  in
  Alcotest.(check bool) "callback ids differ" true (cb1 <> cb2);
  Alcotest.(check (option string))
    "first callback gets first payload" (Some "first details")
    (Telegram.take_tool_result_details ~chat_id:"chat-a" ~user_id:"user-a" cb1);
  Alcotest.(check (option string))
    "second callback still available" (Some "second details")
    (Telegram.take_tool_result_details ~chat_id:"chat-b" ~user_id:"user-b" cb2);
  Alcotest.(check (option string))
    "first callback is one-shot" None
    (Telegram.take_tool_result_details ~chat_id:"chat-a" ~user_id:"user-a" cb1)

let test_tool_result_details_require_matching_chat_and_user () =
  let callback =
    Telegram.register_tool_result_details ~chat_id:"chat-a" ~user_id:"user-a"
      "secret"
  in
  Alcotest.(check (option string))
    "wrong chat cannot read details" None
    (Telegram.take_tool_result_details ~chat_id:"chat-b" ~user_id:"user-a"
       callback);
  Alcotest.(check (option string))
    "wrong user cannot read details" None
    (Telegram.take_tool_result_details ~chat_id:"chat-a" ~user_id:"user-b"
       callback);
  Alcotest.(check (option string))
    "matching chat and user can read details" (Some "secret")
    (Telegram.take_tool_result_details ~chat_id:"chat-a" ~user_id:"user-a"
       callback)

let test_tool_result_details_evict_oldest_without_clearing_newer_entries () =
  let callbacks =
    List.init 257 (fun i ->
        Telegram.register_tool_result_details ~chat_id:"chat-a"
          ~user_id:"user-a"
          (Printf.sprintf "details-%d" i))
  in
  let first = List.hd callbacks in
  let last = List.nth callbacks 256 in
  Alcotest.(check (option string))
    "oldest callback evicted" None
    (Telegram.take_tool_result_details ~chat_id:"chat-a" ~user_id:"user-a" first);
  Alcotest.(check (option string))
    "newest callback preserved" (Some "details-256")
    (Telegram.take_tool_result_details ~chat_id:"chat-a" ~user_id:"user-a" last)

let test_status_notifier_invalid_non_numeric_send_id_is_suppressed () =
  let transport : Telegram.status_transport =
    {
      send_with_id =
        (fun ?disable_notification:_
          ?parse_mode:_
          ~bot_token:_
          ~chat_id:_
          ~text:_
          ()
        -> Lwt.return "not-a-message-id");
      edit_text =
        (fun ?parse_mode:_ ~bot_token:_ ~chat_id:_ ~message_id:_ ~text:_ () ->
          Lwt.return_unit);
      delete_message =
        (fun ~bot_token:_ ~chat_id:_ ~message_id:_ () -> Lwt.return_unit);
    }
  in
  let notifier =
    Telegram.make_status_notifier_with_transport transport ~bot_token:"token"
      ~chat_id:"chat-1"
  in
  let sm =
    Status_message.create ~debounce_interval:0.0 ~notifier ~parse_mode:"HTML" ()
  in
  Lwt_main.run
    (Status_message.tool_start sm ~id:"t1" ~name:"file_read" ~summary:None);
  Alcotest.(check (option string))
    "invalid telegram id does not get adopted" None sm.msg_id

let test_format_tool_result_detail_includes_tool_name_and_empty_output () =
  Alcotest.(check string)
    "empty output gets placeholder" "shell_exec\n[empty output]"
    (Telegram.format_tool_result_detail ~name:"shell_exec" ~result:"   ");
  Alcotest.(check string)
    "tool name prefixes detail" "file_read\nhello"
    (Telegram.format_tool_result_detail ~name:"file_read" ~result:"hello")

let suite =
  [
    Alcotest.test_case "status notifier keeps old message on failed reanchor"
      `Quick test_status_notifier_keeps_old_message_when_reanchor_send_fails;
    Alcotest.test_case "tool result details callbacks are scoped" `Quick
      test_tool_result_details_callbacks_are_scoped;
    Alcotest.test_case "tool result details require matching chat and user"
      `Quick test_tool_result_details_require_matching_chat_and_user;
    Alcotest.test_case
      "tool result details evict oldest without clearing newer entries" `Quick
      test_tool_result_details_evict_oldest_without_clearing_newer_entries;
    Alcotest.test_case "status notifier suppresses invalid non-numeric id"
      `Quick test_status_notifier_invalid_non_numeric_send_id_is_suppressed;
    Alcotest.test_case "tool result detail formatting" `Quick
      test_format_tool_result_detail_includes_tool_name_and_empty_output;
  ]
