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

let test_telegram_status_notifier_preserves_silent_send_after_lock_warn_timeout
    () =
  let outbound_mutex = Lwt_mutex.create () in
  let disable_notifications = ref [] in
  let sent = ref [] in
  let edited = ref [] in
  Lwt_main.run
    (let open Lwt.Syntax in
     let unlocker =
       Lwt.async (fun () ->
           let* () = Lwt_unix.sleep 0.03 in
           Lwt_mutex.unlock outbound_mutex;
           Lwt.return_unit)
     in
     let () = ignore unlocker in
     let* () = Lwt_mutex.lock outbound_mutex in
     let transport : Telegram.status_transport =
       {
         send_with_id =
           (fun ?disable_notification
             ?parse_mode:_
             ~bot_token:_
             ~chat_id:_
             ~text
             ()
           ->
             Lwt_util.with_lock_timeout ~label:"telegram-status-test"
               ~warn_timeout:0.01 ~fatal_timeout:0.2 outbound_mutex (fun () ->
                 disable_notifications :=
                   disable_notification :: !disable_notifications;
                 sent := text :: !sent;
                 Lwt.return "42"));
         edit_text =
           (fun ?parse_mode:_ ~bot_token:_ ~chat_id:_ ~message_id ~text () ->
             Lwt_util.with_lock_timeout ~label:"telegram-status-test"
               ~warn_timeout:0.01 ~fatal_timeout:0.2 outbound_mutex (fun () ->
                 edited := (message_id, text) :: !edited;
                 Lwt.return_unit));
         delete_message =
           (fun ~bot_token:_ ~chat_id:_ ~message_id:_ () -> Lwt.return_unit);
       }
     in
     let notifier =
       Telegram.make_status_notifier_with_transport transport ~bot_token:"token"
         ~chat_id:"chat-1"
     in
     let sm =
       Status_message.create ~debounce_interval:0.0 ~notifier ~parse_mode:"HTML"
         ()
     in
     let* () =
       Status_message.tool_start sm ~id:"t1" ~name:"shell_exec" ~summary:None
     in
     let* () =
       Status_message.tool_result sm ~id:"t1" ~name:"shell_exec" ~result:"ok"
         ~is_error:false
     in
     Lwt.return_unit);
  Alcotest.(check int)
    "initial send still succeeds after warn timeout" 1 (List.length !sent);
  Alcotest.(check int)
    "follow-up edit still succeeds after warn timeout" 1 (List.length !edited);
  Alcotest.(check (list (option bool)))
    "status sends stay silent" [ Some true ] !disable_notifications;
  Alcotest.(check (option string))
    "message id adopted after delayed send" (Some "42") (Some "42")

let test_status_message_tool_start_sends_immediately_without_debounce_delay () =
  let notifier, sent, edited, _deleted =
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
  in
  let sm =
    Status_message.create ~debounce_interval:0.5 ~notifier ~parse_mode:"HTML" ()
  in
  Lwt_main.run
    (Status_message.tool_start sm ~id:"t1" ~name:"shell_exec" ~summary:None);
  Alcotest.(check int) "first tool start sends immediately" 1 !sent;
  Alcotest.(check int) "no edit needed yet" 0 !edited

let test_consolidated_tool_start_and_result_use_single_status_message () =
  let transport_calls = ref [] in
  let transport : Telegram.status_transport =
    {
      send_with_id =
        (fun ?disable_notification
          ?parse_mode
          ~bot_token:_
          ~chat_id:_
          ~text
          ()
        ->
          transport_calls :=
            Printf.sprintf "send dn=%s pm=%s text=%s"
              (match disable_notification with
              | Some true -> "true"
              | Some false -> "false"
              | None -> "none")
              (match parse_mode with Some s -> s | None -> "none")
              text
            :: !transport_calls;
          Lwt.return "42");
      edit_text =
        (fun ?parse_mode ~bot_token:_ ~chat_id:_ ~message_id ~text () ->
          transport_calls :=
            Printf.sprintf "edit id=%s pm=%s text=%s" message_id
              (match parse_mode with Some s -> s | None -> "none")
              text
            :: !transport_calls;
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
    (let open Lwt.Syntax in
     let* () =
       Status_message.tool_start sm ~id:"t1" ~name:"shell_exec"
         ~summary:(Some "true")
     in
     let* () =
       Status_message.tool_result sm ~id:"t1" ~name:"shell_exec" ~result:"ok"
         ~is_error:false
     in
     Lwt.return_unit);
  let calls = List.rev !transport_calls in
  Alcotest.(check int) "one send then one edit" 2 (List.length calls);
  Alcotest.(check bool)
    "first call is silent HTML send" true
    (match calls with
    | first :: _ -> String.starts_with ~prefix:"send dn=true pm=HTML" first
    | [] -> false);
  Alcotest.(check bool)
    "second call edits same message" true
    (match calls with
    | _ :: second :: _ -> String.starts_with ~prefix:"edit id=42 pm=HTML" second
    | _ -> false)

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
  sm.msg_id <- Some "77";
  Lwt_main.run (Status_message.finalize sm);
  Lwt_main.run (Status_message.finalize sm);
  Alcotest.(check int) "no sends" 0 !sent;
  Alcotest.(check int) "delete called once" 1 !deleted

let test_finalize_idempotent_with_tools () =
  let notifier, _sent, edited, _deleted = make_notifier () in
  let sm =
    Status_message.create ~debounce_interval:0.0 ~notifier ~parse_mode:"HTML" ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       Status_message.tool_start sm ~id:"t1" ~name:"file_read" ~summary:None
     in
     let* () =
       Status_message.tool_start sm ~id:"t2" ~name:"file_write" ~summary:None
     in
     let* () =
       Status_message.tool_start sm ~id:"t3" ~name:"shell_exec" ~summary:None
     in
     let* () =
       Status_message.tool_start sm ~id:"t4" ~name:"http_get" ~summary:None
     in
     let* () = Status_message.finalize sm in
     Lwt.return_unit);
  let edited_after_first_finalize = !edited in
  Lwt_main.run (Status_message.finalize sm);
  Alcotest.(check bool)
    "first finalize edited consolidated status" true
    (edited_after_first_finalize > 0);
  Alcotest.(check int)
    "second finalize is a no-op" edited_after_first_finalize !edited

let suite =
  [
    Alcotest.test_case "edit notifier does not re-anchor" `Quick
      test_status_notifier_edits_in_place_without_reanchoring;
    Alcotest.test_case "tool result details are scoped" `Quick
      test_tool_result_details_callbacks_are_scoped;
    Alcotest.test_case "tool result details require matching chat and user"
      `Quick test_tool_result_details_require_matching_chat_and_user;
    Alcotest.test_case "tool result details evict oldest only" `Quick
      test_tool_result_details_evict_oldest_without_clearing_newer_entries;
    Alcotest.test_case "invalid status send id is suppressed" `Quick
      test_status_notifier_invalid_non_numeric_send_id_is_suppressed;
    Alcotest.test_case "silent status send survives warn timeout" `Quick
      test_telegram_status_notifier_preserves_silent_send_after_lock_warn_timeout;
    Alcotest.test_case "status tool start sends immediately" `Quick
      test_status_message_tool_start_sends_immediately_without_debounce_delay;
    Alcotest.test_case "consolidated tool status reuses one telegram message"
      `Quick test_consolidated_tool_start_and_result_use_single_status_message;
    Alcotest.test_case "format tool result detail includes tool name" `Quick
      test_format_tool_result_detail_includes_tool_name_and_empty_output;
    Alcotest.test_case "finalize without tools is idempotent" `Quick
      test_finalize_idempotent_no_tools;
    Alcotest.test_case "finalize with tools is idempotent" `Quick
      test_finalize_idempotent_with_tools;
  ]

let () =
  let open Alcotest in
  run "Telegram status helpers" [ ("telegram-status", suite) ]
