let test_status_notifier_edits_in_place_without_reanchoring () =
  let edited = ref [] in
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
          Lwt.return "999");
      edit_text =
        (fun ?parse_mode:_ ~bot_token:_ ~chat_id:_ ~message_id ~text () ->
          edited := (message_id, text) :: !edited;
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
  let edit_result =
    Lwt_main.run (notifier.edit "10" ~parse_mode:"HTML" "status update")
  in
  Alcotest.(check (option string))
    "edit returns None (no message id change)" None edit_result;
  Alcotest.(check int) "no messages sent" 0 (List.length !sent);
  Alcotest.(check int) "no messages deleted" 0 (List.length !deleted);
  Alcotest.(check int) "edit called once" 1 (List.length !edited);
  Alcotest.(check string)
    "edit called with correct message_id" "10"
    (fst (List.hd !edited));
  Alcotest.(check string)
    "edit called with correct text" "status update"
    (snd (List.hd !edited))

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

let test_outbound_lock_timeout_cancels_timed_out_waiter () =
  let mutex = Lwt_mutex.create () in
  Lwt_main.run (Lwt_mutex.lock mutex);
  Lwt.async (fun () ->
      let open Lwt.Syntax in
      let* () = Lwt_unix.sleep 0.03 in
      Lwt_mutex.unlock mutex;
      Lwt.return_unit);
  let acquired =
    Lwt_main.run
      (Lwt.pick
         [
           (Lwt_util.with_lock_timeout ~label:"telegram-status-test"
              ~warn_timeout:0.01 ~fatal_timeout:1.0 mutex (fun () ->
                Lwt.return_true));
           (let open Lwt.Syntax in
            let* () = Lwt_unix.sleep 0.2 in
            Lwt.return_false);
         ])
  in
  Alcotest.(check bool)
    "lock retry still succeeds after warn timeout" true acquired

let test_format_tool_result_detail_includes_tool_name_and_empty_output () =
  Alcotest.(check string)
    "empty output gets placeholder" "shell_exec\n[empty output]"
    (Telegram.format_tool_result_detail ~name:"shell_exec" ~result:"   ");
  Alcotest.(check string)
    "tool name prefixes detail" "file_read\nhello"
    (Telegram.format_tool_result_detail ~name:"file_read" ~result:"hello")

let make_notifier () =
  let sent = ref 0 in
  let edited = ref 0 in
  let deleted = ref 0 in
  let notifier : Status_message.notifier =
    {
      send =
        (fun ?parse_mode:_ _text ->
          incr sent;
          Lwt.return "42");
      edit =
        (fun _id ?parse_mode:_ _text ->
          incr edited;
          Lwt.return None);
      delete =
        (fun _id ->
          incr deleted;
          Lwt.return_unit);
    }
  in
  (notifier, sent, edited, deleted)

let test_finalize_idempotent_no_tools () =
  let notifier, sent, _edited, deleted = make_notifier () in
  let sm =
    Status_message.create ~debounce_interval:0.0 ~notifier ~parse_mode:"HTML" ()
  in
  (* Force a msg_id by simulating a send - set it directly *)
  sm.msg_id <- Some "77";
  Lwt_main.run (Status_message.finalize sm);
  Alcotest.(check int) "first finalize: deletes message" 1 !deleted;
  Alcotest.(check int) "first finalize: no sends" 0 !sent;
  Lwt_main.run (Status_message.finalize sm);
  Alcotest.(check int) "second finalize: no additional delete" 1 !deleted

let test_finalize_idempotent_with_tools () =
  let notifier, _sent, edited, _deleted = make_notifier () in
  let sm =
    Status_message.create ~debounce_interval:0.0 ~notifier ~parse_mode:"HTML" ()
  in
  Lwt_main.run
    (Status_message.tool_start sm ~id:"t1" ~name:"file_read" ~summary:None);
  Lwt_main.run
    (Status_message.tool_start sm ~id:"t2" ~name:"file_write" ~summary:None);
  Lwt_main.run
    (Status_message.tool_start sm ~id:"t3" ~name:"shell_exec" ~summary:None);
  Lwt_main.run
    (Status_message.tool_start sm ~id:"t4" ~name:"http_get" ~summary:None);
  let edits_before = !edited in
  Lwt_main.run (Status_message.finalize sm);
  let edits_after_first = !edited in
  Alcotest.(check bool)
    "first finalize triggers edit" true
    (edits_after_first > edits_before);
  Lwt_main.run (Status_message.finalize sm);
  Alcotest.(check int)
    "second finalize triggers no additional edit" edits_after_first !edited

let suite =
  [
    Alcotest.test_case "status notifier edits in place without reanchoring"
      `Quick test_status_notifier_edits_in_place_without_reanchoring;
    Alcotest.test_case "tool result details callbacks are scoped" `Quick
      test_tool_result_details_callbacks_are_scoped;
    Alcotest.test_case "tool result details require matching chat and user"
      `Quick test_tool_result_details_require_matching_chat_and_user;
    Alcotest.test_case
      "tool result details evict oldest without clearing newer entries" `Quick
      test_tool_result_details_evict_oldest_without_clearing_newer_entries;
    Alcotest.test_case "status notifier suppresses invalid non-numeric id"
      `Quick test_status_notifier_invalid_non_numeric_send_id_is_suppressed;
    Alcotest.test_case "outbound lock timeout cancels timed out waiter" `Quick
      test_outbound_lock_timeout_cancels_timed_out_waiter;
    Alcotest.test_case "tool result detail formatting" `Quick
      test_format_tool_result_detail_includes_tool_name_and_empty_output;
    Alcotest.test_case "finalize is idempotent with no tools" `Quick
      test_finalize_idempotent_no_tools;
    Alcotest.test_case "finalize is idempotent with tools" `Quick
      test_finalize_idempotent_with_tools;
  ]
