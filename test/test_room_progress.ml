open Background_task_0_format

let make_task ?(id = 1) ?(status = Running) ?(progress_state = None)
    ?(origin_json = None) ?(thread_id = None) ?(channel_id = None)
    ?(prompt = "Fix the bug in auth module") () =
  {
    id;
    runner = Codex;
    model = None;
    repo_path = "/tmp/test-repo";
    prompt;
    branch = "main";
    worktree_path = None;
    log_path = None;
    status;
    runner_session_id = None;
    session_key = None;
    channel = None;
    channel_id;
    pid = None;
    result_preview = None;
    created_at = "2026-06-28T00:00:00Z";
    started_at = None;
    finished_at = None;
    automerge = false;
    use_worktree = false;
    merge_status = None;
    retry_count = 0;
    parent_task_id = None;
    replaced_by = None;
    acp = false;
    agent_name = None;
    notification_status = None;
    notification_error = None;
    notification_attempts = 0;
    follow_up_prompt = None;
    profile_id = None;
    origin_json;
    thread_id;
    requester = None;
    progress_state;
  }

let test_format_progress_message_working () =
  let origin =
    Room_origin.to_compact_json_string
      (Room_origin.make ~connector:"slack" ~room_id:"C1" ())
  in
  let task = make_task ~status:Running ~origin_json:(Some origin) () in
  let msg = Room_progress.format_progress_message task in
  Alcotest.(check bool)
    "contains working" true
    (Test_helpers.string_contains msg "working");
  Alcotest.(check bool)
    "contains prompt snippet" true
    (Test_helpers.string_contains msg "Fix the bug")

let test_format_progress_message_completed () =
  let origin =
    Room_origin.to_compact_json_string
      (Room_origin.make ~connector:"slack" ~room_id:"C1" ())
  in
  let task = make_task ~status:Succeeded ~origin_json:(Some origin) () in
  let msg = Room_progress.format_progress_message task in
  Alcotest.(check bool)
    "contains completed" true
    (Test_helpers.string_contains msg "completed")

let test_format_progress_message_failed () =
  let origin =
    Room_origin.to_compact_json_string
      (Room_origin.make ~connector:"slack" ~room_id:"C1" ())
  in
  let task = make_task ~status:Failed ~origin_json:(Some origin) () in
  let msg = Room_progress.format_progress_message task in
  Alcotest.(check bool)
    "contains failed" true
    (Test_helpers.string_contains msg "failed")

let test_format_progress_message_explicit_state () =
  let task = make_task ~status:Running ~progress_state:(Some Blocked) () in
  let msg = Room_progress.format_progress_message task in
  Alcotest.(check bool)
    "contains blocked" true
    (Test_helpers.string_contains msg "blocked")

let test_format_progress_message_long_prompt () =
  let long_prompt = String.make 100 'x' in
  let task = make_task ~prompt:long_prompt () in
  let msg = Room_progress.format_progress_message task in
  Alcotest.(check bool)
    "truncated with ellipsis" true
    (Test_helpers.string_contains msg "...")

let test_deliver_first_send_records_msg_id () =
  Room_progress.clear_progress_msg_id ~task_id:42;
  let sent = ref [] in
  let send ~room_id:_ ?thread_id:_ ~text () =
    sent := text :: !sent;
    Lwt.return "msg_ts_123"
  in
  let edit ~room_id:_ ~msg_id:_ ~text:_ = Lwt.return_unit in
  let origin =
    Room_origin.to_compact_json_string
      (Room_origin.make ~connector:"slack" ~room_id:"C123" ())
  in
  let task = make_task ~id:42 ~origin_json:(Some origin) () in
  Lwt_main.run (Room_progress.deliver_progress_update ~send ~edit ~task ());
  Alcotest.(check int) "one message sent" 1 (List.length !sent);
  Alcotest.(check (option string))
    "msg_id recorded" (Some "msg_ts_123")
    (Hashtbl.find_opt Room_progress.progress_msg_ids 42)

let test_deliver_second_call_edits_in_place () =
  Hashtbl.replace Room_progress.progress_msg_ids 99 "existing_msg";
  let edited = ref [] in
  let send ~room_id:_ ?thread_id:_ ~text:_ () = Lwt.return "new_msg" in
  let edit ~room_id:_ ~msg_id ~text =
    edited := (msg_id, text) :: !edited;
    Lwt.return_unit
  in
  let origin =
    Room_origin.to_compact_json_string
      (Room_origin.make ~connector:"slack" ~room_id:"C456" ())
  in
  let task = make_task ~id:99 ~origin_json:(Some origin) () in
  Lwt_main.run (Room_progress.deliver_progress_update ~send ~edit ~task ());
  Alcotest.(check int) "one edit" 1 (List.length !edited);
  let msg_id, _ = List.hd !edited in
  Alcotest.(check string) "edits existing msg" "existing_msg" msg_id

let test_deliver_edit_failure_falls_back_to_send () =
  Hashtbl.replace Room_progress.progress_msg_ids 77 "deleted_msg";
  let sent = ref [] in
  let send ~room_id:_ ?thread_id:_ ~text () =
    sent := text :: !sent;
    Lwt.return "replacement_msg"
  in
  let edit ~room_id:_ ~msg_id:_ ~text:_ =
    Lwt.fail (Failure "message not found")
  in
  let origin =
    Room_origin.to_compact_json_string
      (Room_origin.make ~connector:"slack" ~room_id:"C789" ())
  in
  let task = make_task ~id:77 ~origin_json:(Some origin) () in
  Lwt_main.run (Room_progress.deliver_progress_update ~send ~edit ~task ());
  Alcotest.(check int) "fallback send" 1 (List.length !sent);
  Alcotest.(check (option string))
    "new msg_id recorded" (Some "replacement_msg")
    (Hashtbl.find_opt Room_progress.progress_msg_ids 77)

let test_deliver_empty_room_id_skips () =
  let sent = ref [] in
  let send ~room_id:_ ?thread_id:_ ~text () =
    sent := text :: !sent;
    Lwt.return "msg"
  in
  let edit ~room_id:_ ~msg_id:_ ~text:_ = Lwt.return_unit in
  (* No origin_json, no channel_id => empty room_id *)
  let task = make_task ~id:55 () in
  Lwt_main.run (Room_progress.deliver_progress_update ~send ~edit ~task ());
  Alcotest.(check int) "no message sent" 0 (List.length !sent)

let test_deliver_passes_thread_id () =
  Room_progress.clear_progress_msg_id ~task_id:60;
  let captured_thread = ref None in
  let send ~room_id:_ ?thread_id ~text:_ () =
    captured_thread := thread_id;
    Lwt.return "msg_ts"
  in
  let edit ~room_id:_ ~msg_id:_ ~text:_ = Lwt.return_unit in
  let origin =
    Room_origin.to_compact_json_string
      (Room_origin.make ~connector:"slack" ~room_id:"C100"
         ~thread_id:"1234.0000" ())
  in
  let task = make_task ~id:60 ~origin_json:(Some origin) () in
  Lwt_main.run (Room_progress.deliver_progress_update ~send ~edit ~task ());
  Alcotest.(check (option string))
    "thread_id passed" (Some "1234.0000") !captured_thread

let test_clear_progress_msg_id () =
  Hashtbl.replace Room_progress.progress_msg_ids 88 "some_msg";
  Room_progress.clear_progress_msg_id ~task_id:88;
  Alcotest.(check bool)
    "msg_id cleared" true
    (not (Hashtbl.mem Room_progress.progress_msg_ids 88))

let test_short_prompt_label_normal () =
  let task = make_task ~prompt:"Short prompt" () in
  let label = Room_progress.short_prompt_label task in
  Alcotest.(check string) "normal prompt" "Short prompt" label

let test_short_prompt_label_empty () =
  let task = make_task ~prompt:"" () in
  let label = Room_progress.short_prompt_label task in
  (* Falls back to task_label which includes runner/repo/branch *)
  Alcotest.(check bool) "fallback label not empty" true (String.length label > 0)

let test_short_prompt_label_multiline () =
  let task = make_task ~prompt:"First line\nSecond line\nThird" () in
  let label = Room_progress.short_prompt_label task in
  Alcotest.(check string) "first line only" "First line" label

let suite =
  [
    Alcotest.test_case "format progress message working" `Quick
      test_format_progress_message_working;
    Alcotest.test_case "format progress message completed" `Quick
      test_format_progress_message_completed;
    Alcotest.test_case "format progress message failed" `Quick
      test_format_progress_message_failed;
    Alcotest.test_case "format progress message explicit state" `Quick
      test_format_progress_message_explicit_state;
    Alcotest.test_case "format progress message long prompt" `Quick
      test_format_progress_message_long_prompt;
    Alcotest.test_case "deliver first send records msg_id" `Quick
      test_deliver_first_send_records_msg_id;
    Alcotest.test_case "deliver second call edits in place" `Quick
      test_deliver_second_call_edits_in_place;
    Alcotest.test_case "deliver edit failure falls back to send" `Quick
      test_deliver_edit_failure_falls_back_to_send;
    Alcotest.test_case "deliver empty room_id skips" `Quick
      test_deliver_empty_room_id_skips;
    Alcotest.test_case "deliver passes thread_id" `Quick
      test_deliver_passes_thread_id;
    Alcotest.test_case "clear progress msg_id" `Quick test_clear_progress_msg_id;
    Alcotest.test_case "short prompt label normal" `Quick
      test_short_prompt_label_normal;
    Alcotest.test_case "short prompt label empty" `Quick
      test_short_prompt_label_empty;
    Alcotest.test_case "short prompt label multiline" `Quick
      test_short_prompt_label_multiline;
  ]
