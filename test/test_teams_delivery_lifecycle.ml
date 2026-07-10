let with_db f = Test_helpers.with_memory_store f

let metadata_string key json =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt key fields with Some (`String s) -> s | _ -> "")
  | _ -> ""

let metadata_int key json =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt key fields with Some (`Int n) -> n | _ -> -1)
  | _ -> -1

let contains_substring = Test_helpers.string_contains

let test_generate_tracking_id_unique () =
  let id1 = Teams_delivery_lifecycle.generate_tracking_id () in
  let id2 = Teams_delivery_lifecycle.generate_tracking_id () in
  Alcotest.(check bool) "ids different" true (id1 <> id2);
  Alcotest.(check bool)
    "starts with dlv_" true
    (String.length id1 > 4 && String.sub id1 0 4 = "dlv_")

let test_record_scheduled () =
  with_db (fun db ->
      let tracking_id = Teams_delivery_lifecycle.generate_tracking_id () in
      Teams_delivery_lifecycle.record_scheduled ~db ~room_id:"room-1"
        ~connector:"teams" ~tracking_id ~task_id:42 ~thread_id:"thread-1" ();
      let events = Room_activity_ledger.query ~db ~room_id:"room-1" () in
      Alcotest.(check int) "one event" 1 (List.length events);
      let evt = List.hd events in
      Alcotest.(check string)
        "event_type" "teams_delivery_scheduled" evt.event_type;
      Alcotest.(check string) "actor" "teams" evt.actor;
      Alcotest.(check string)
        "tracking_id" tracking_id
        (metadata_string "tracking_id" evt.metadata);
      Alcotest.(check string)
        "lifecycle_state" "scheduled"
        (metadata_string "lifecycle_state" evt.metadata);
      Alcotest.(check int) "task_id" 42 (metadata_int "task_id" evt.metadata);
      Alcotest.(check string)
        "thread_id" "thread-1"
        (metadata_string "thread_id" evt.metadata))

let test_record_attempted () =
  with_db (fun db ->
      let tracking_id = Teams_delivery_lifecycle.generate_tracking_id () in
      Teams_delivery_lifecycle.record_attempted ~db ~room_id:"room-2"
        ~connector:"teams" ~tracking_id ~task_id:10 ();
      let events = Room_activity_ledger.query ~db ~room_id:"room-2" () in
      Alcotest.(check int) "one event" 1 (List.length events);
      let evt = List.hd events in
      Alcotest.(check string)
        "event_type" "teams_delivery_attempted" evt.event_type;
      Alcotest.(check string)
        "lifecycle_state" "attempted"
        (metadata_string "lifecycle_state" evt.metadata))

let test_record_transport_accepted () =
  with_db (fun db ->
      let tracking_id = Teams_delivery_lifecycle.generate_tracking_id () in
      Teams_delivery_lifecycle.record_transport_accepted ~db ~room_id:"room-3"
        ~connector:"teams" ~tracking_id ~task_id:11 ();
      let events = Room_activity_ledger.query ~db ~room_id:"room-3" () in
      let evt = List.hd events in
      Alcotest.(check string)
        "event_type" "teams_delivery_transport_accepted" evt.event_type)

let test_record_message_id_recorded () =
  with_db (fun db ->
      let tracking_id = Teams_delivery_lifecycle.generate_tracking_id () in
      Teams_delivery_lifecycle.record_message_id_recorded ~db ~room_id:"room-4"
        ~connector:"teams" ~tracking_id ~task_id:12 ~message_id:"activity-abc"
        ();
      let events = Room_activity_ledger.query ~db ~room_id:"room-4" () in
      let evt = List.hd events in
      Alcotest.(check string)
        "event_type" "teams_delivery_message_id_recorded" evt.event_type;
      Alcotest.(check string)
        "message_id" "activity-abc"
        (metadata_string "message_id" evt.metadata))

let test_record_edit_failed () =
  with_db (fun db ->
      let tracking_id = Teams_delivery_lifecycle.generate_tracking_id () in
      Teams_delivery_lifecycle.record_edit_failed ~db ~room_id:"room-5"
        ~connector:"teams" ~tracking_id ~task_id:13
        ~error:"Bearer sk-secret123456 HTTP 404" ();
      let events = Room_activity_ledger.query ~db ~room_id:"room-5" () in
      let evt = List.hd events in
      Alcotest.(check string)
        "event_type" "teams_delivery_edit_failed" evt.event_type;
      let err = metadata_string "error" evt.metadata in
      Alcotest.(check bool)
        "token redacted" true
        (not (contains_substring err "sk-secret123456")))

let test_record_fallback_sent () =
  with_db (fun db ->
      let tracking_id = Teams_delivery_lifecycle.generate_tracking_id () in
      Teams_delivery_lifecycle.record_fallback_sent ~db ~room_id:"room-6"
        ~connector:"teams" ~tracking_id ~task_id:14 ();
      let events = Room_activity_ledger.query ~db ~room_id:"room-6" () in
      let evt = List.hd events in
      Alcotest.(check string)
        "event_type" "teams_delivery_fallback_sent" evt.event_type)

let test_record_failed () =
  with_db (fun db ->
      let tracking_id = Teams_delivery_lifecycle.generate_tracking_id () in
      Teams_delivery_lifecycle.record_failed ~db ~room_id:"room-7"
        ~connector:"teams" ~tracking_id ~task_id:15
        ~error:"token=abc123def connection refused" ();
      let events = Room_activity_ledger.query ~db ~room_id:"room-7" () in
      let evt = List.hd events in
      Alcotest.(check string)
        "event_type" "teams_delivery_failed" evt.event_type;
      let err = metadata_string "error" evt.metadata in
      Alcotest.(check bool)
        "token redacted" true
        (not (contains_substring err "abc123def")))

let test_record_user_visible_unconfirmed () =
  with_db (fun db ->
      let tracking_id = Teams_delivery_lifecycle.generate_tracking_id () in
      Teams_delivery_lifecycle.record_user_visible_unconfirmed ~db
        ~room_id:"room-8" ~connector:"teams" ~tracking_id ~task_id:16 ();
      let events = Room_activity_ledger.query ~db ~room_id:"room-8" () in
      let evt = List.hd events in
      Alcotest.(check string)
        "event_type" "teams_delivery_user_visible_unconfirmed" evt.event_type;
      Alcotest.(check string)
        "lifecycle_state" "user_visible_unconfirmed"
        (metadata_string "lifecycle_state" evt.metadata))

let test_correlatable_tracking_id () =
  with_db (fun db ->
      let tracking_id = Teams_delivery_lifecycle.generate_tracking_id () in
      (* Record full lifecycle for one message *)
      Teams_delivery_lifecycle.record_scheduled ~db ~room_id:"room-10"
        ~connector:"teams" ~tracking_id ~task_id:20 ();
      Teams_delivery_lifecycle.record_attempted ~db ~room_id:"room-10"
        ~connector:"teams" ~tracking_id ~task_id:20 ();
      Teams_delivery_lifecycle.record_transport_accepted ~db ~room_id:"room-10"
        ~connector:"teams" ~tracking_id ~task_id:20 ();
      Teams_delivery_lifecycle.record_message_id_recorded ~db ~room_id:"room-10"
        ~connector:"teams" ~tracking_id ~task_id:20 ~message_id:"act-123" ();
      (* Query by tracking ID *)
      let lifecycle_events =
        Teams_delivery_lifecycle.query_by_tracking_id ~db ~tracking_id ()
      in
      Alcotest.(check int)
        "four lifecycle events" 4
        (List.length lifecycle_events);
      let states =
        List.map
          (fun (e : Room_activity_ledger.event) ->
            metadata_string "lifecycle_state" e.metadata)
          lifecycle_events
      in
      Alcotest.(check (list string))
        "lifecycle order"
        [
          "scheduled"; "attempted"; "transport_accepted"; "message_id_recorded";
        ]
        states)

let test_correlatable_tracking_id_edit_fallback () =
  with_db (fun db ->
      let tracking_id = Teams_delivery_lifecycle.generate_tracking_id () in
      (* Edit failure then fallback *)
      Teams_delivery_lifecycle.record_scheduled ~db ~room_id:"room-11"
        ~connector:"teams" ~tracking_id ~task_id:21 ();
      Teams_delivery_lifecycle.record_attempted ~db ~room_id:"room-11"
        ~connector:"teams" ~tracking_id ~task_id:21 ();
      Teams_delivery_lifecycle.record_edit_failed ~db ~room_id:"room-11"
        ~connector:"teams" ~tracking_id ~task_id:21 ~error:"HTTP 404" ();
      Teams_delivery_lifecycle.record_attempted ~db ~room_id:"room-11"
        ~connector:"teams" ~tracking_id ~task_id:21 ();
      Teams_delivery_lifecycle.record_fallback_sent ~db ~room_id:"room-11"
        ~connector:"teams" ~tracking_id ~task_id:21 ();
      Teams_delivery_lifecycle.record_message_id_recorded ~db ~room_id:"room-11"
        ~connector:"teams" ~tracking_id ~task_id:21 ~message_id:"new-act-456" ();
      let lifecycle_events =
        Teams_delivery_lifecycle.query_by_tracking_id ~db ~tracking_id ()
      in
      Alcotest.(check int)
        "six lifecycle events" 6
        (List.length lifecycle_events);
      let states =
        List.map
          (fun (e : Room_activity_ledger.event) ->
            metadata_string "lifecycle_state" e.metadata)
          lifecycle_events
      in
      Alcotest.(check (list string))
        "edit-fallback lifecycle"
        [
          "scheduled";
          "attempted";
          "edit_failed";
          "attempted";
          "fallback_sent";
          "message_id_recorded";
        ]
        states)

let test_lifecycle_state_of_string_roundtrip () =
  let states =
    Teams_delivery_lifecycle.
      [
        Scheduled;
        Generated;
        Attempted;
        Transport_accepted;
        Message_id_recorded;
        Edit_failed;
        Fallback_sent;
        Failed;
        User_visible_unconfirmed;
      ]
  in
  List.iter
    (fun state ->
      let s = Teams_delivery_lifecycle.string_of_lifecycle_state state in
      match Teams_delivery_lifecycle.lifecycle_state_of_string s with
      | Some roundtripped ->
          Alcotest.(check bool)
            (Printf.sprintf "roundtrip %s" s)
            true (roundtripped = state)
      | None ->
          Alcotest.fail (Printf.sprintf "failed to parse lifecycle state: %s" s))
    states

let test_is_terminal_lifecycle () =
  let open Teams_delivery_lifecycle in
  Alcotest.(check bool)
    "Message_id_recorded terminal" true
    (is_terminal_lifecycle Message_id_recorded);
  Alcotest.(check bool)
    "Edit_failed terminal" true
    (is_terminal_lifecycle Edit_failed);
  Alcotest.(check bool) "Failed terminal" true (is_terminal_lifecycle Failed);
  Alcotest.(check bool)
    "User_visible_unconfirmed terminal" true
    (is_terminal_lifecycle User_visible_unconfirmed);
  Alcotest.(check bool)
    "Scheduled not terminal" false
    (is_terminal_lifecycle Scheduled);
  Alcotest.(check bool)
    "Attempted not terminal" false
    (is_terminal_lifecycle Attempted);
  Alcotest.(check bool)
    "Transport_accepted not terminal" false
    (is_terminal_lifecycle Transport_accepted)

let test_teams_delivery_records_lifecycle () =
  (* Integration test: deliver_progress_update records Teams lifecycle states *)
  with_db (fun db ->
      let send_called = ref false in
      let send ~room_id ?thread_id:_ ~text:_ () =
        send_called := true;
        Lwt.return "teams-act-789"
      in
      let edit ~room_id:_ ~msg_id:_ ~text:_ = Lwt.return_unit in
      let task =
        {
          Background_task_0_format.id = 50;
          runner = Background_task_0_format.Codex;
          model = None;
          repo_path = "/tmp";
          prompt = "test teams lifecycle";
          branch = "main";
          worktree_path = None;
          log_path = None;
          status = Background_task_0_format.Running;
          runner_session_id = None;
          session_key = Some "teams:room-50:user-1";
          channel = Some "teams";
          channel_id = Some "room-50";
          pid = None;
          result_preview = None;
          created_at = "2026-06-29T00:00:00Z";
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
          description = None;
          context_snapshot = None;
          profile_id = None;
          origin_json =
            Some
              (Yojson.Safe.to_string
                 (`Assoc
                    [
                      ("room_id", `String "room-50");
                      ("connector", `String "teams");
                    ]));
          thread_id = None;
          requester = None;
          progress_state = None;
          access_snapshot_id = None;
          host_kind = "direct";
          host_session_id = None;
          restart_policy = Background_task.Reenqueue;
          restart_count = 0;
          max_restarts = Background_task.max_restarts_default;
        }
      in
      Lwt_main.run
        (Room_progress.deliver_progress_update ~send ~edit ~db ~task ());
      Alcotest.(check bool) "send called" true !send_called;
      (* Check lifecycle events were recorded *)
      let all_events = Room_activity_ledger.query ~db ~room_id:"room-50" () in
      let lifecycle_events =
        List.filter
          (fun (e : Room_activity_ledger.event) ->
            String.starts_with ~prefix:"teams_delivery_" e.event_type)
          all_events
      in
      Alcotest.(check bool)
        "lifecycle events recorded" true
        (List.length lifecycle_events >= 3);
      let event_types =
        List.map
          (fun (e : Room_activity_ledger.event) -> e.event_type)
          lifecycle_events
      in
      Alcotest.(check bool)
        "has scheduled" true
        (List.mem "teams_delivery_scheduled" event_types);
      Alcotest.(check bool)
        "has generated" true
        (List.mem "teams_delivery_generated" event_types);
      Alcotest.(check bool)
        "has attempted" true
        (List.mem "teams_delivery_attempted" event_types);
      Alcotest.(check bool)
        "has transport_accepted" true
        (List.mem "teams_delivery_transport_accepted" event_types);
      Alcotest.(check bool)
        "has message_id_recorded" true
        (List.mem "teams_delivery_message_id_recorded" event_types);
      (* Verify tracking IDs are consistent *)
      let tracking_ids =
        List.map
          (fun (e : Room_activity_ledger.event) ->
            metadata_string "tracking_id" e.metadata)
          lifecycle_events
      in
      let unique_ids = List.sort_uniq String.compare tracking_ids in
      Alcotest.(check int) "single tracking id" 1 (List.length unique_ids))

let test_non_teams_no_lifecycle () =
  (* Verify that non-Teams connectors do NOT record lifecycle events *)
  with_db (fun db ->
      let send ~room_id ?thread_id:_ ~text:_ () = Lwt.return "msg-1" in
      let edit ~room_id:_ ~msg_id:_ ~text:_ = Lwt.return_unit in
      let task =
        {
          Background_task_0_format.id = 60;
          runner = Background_task_0_format.Codex;
          model = None;
          repo_path = "/tmp";
          prompt = "test slack no lifecycle";
          branch = "main";
          worktree_path = None;
          log_path = None;
          status = Background_task_0_format.Running;
          runner_session_id = None;
          session_key = Some "slack:room-60:user-1";
          channel = Some "slack";
          channel_id = Some "room-60";
          pid = None;
          result_preview = None;
          created_at = "2026-06-29T00:00:00Z";
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
          description = None;
          context_snapshot = None;
          profile_id = None;
          origin_json =
            Some
              (Yojson.Safe.to_string
                 (`Assoc
                    [
                      ("room_id", `String "room-60");
                      ("connector", `String "slack");
                    ]));
          thread_id = None;
          requester = None;
          progress_state = None;
          access_snapshot_id = None;
          host_kind = "direct";
          host_session_id = None;
          restart_policy = Background_task.Reenqueue;
          restart_count = 0;
          max_restarts = Background_task.max_restarts_default;
        }
      in
      Lwt_main.run
        (Room_progress.deliver_progress_update ~send ~edit ~db ~task ());
      let all_events = Room_activity_ledger.query ~db ~room_id:"room-60" () in
      let lifecycle_events =
        List.filter
          (fun (e : Room_activity_ledger.event) ->
            String.starts_with ~prefix:"teams_delivery_" e.event_type)
          all_events
      in
      Alcotest.(check int)
        "no lifecycle events for slack" 0
        (List.length lifecycle_events))

let suite =
  [
    Alcotest.test_case "generate tracking id unique" `Quick
      test_generate_tracking_id_unique;
    Alcotest.test_case "record scheduled" `Quick test_record_scheduled;
    Alcotest.test_case "record attempted" `Quick test_record_attempted;
    Alcotest.test_case "record transport accepted" `Quick
      test_record_transport_accepted;
    Alcotest.test_case "record message id recorded" `Quick
      test_record_message_id_recorded;
    Alcotest.test_case "record edit failed" `Quick test_record_edit_failed;
    Alcotest.test_case "record fallback sent" `Quick test_record_fallback_sent;
    Alcotest.test_case "record failed" `Quick test_record_failed;
    Alcotest.test_case "record user visible unconfirmed" `Quick
      test_record_user_visible_unconfirmed;
    Alcotest.test_case "correlatable tracking id" `Quick
      test_correlatable_tracking_id;
    Alcotest.test_case "correlatable tracking id edit fallback" `Quick
      test_correlatable_tracking_id_edit_fallback;
    Alcotest.test_case "lifecycle state roundtrip" `Quick
      test_lifecycle_state_of_string_roundtrip;
    Alcotest.test_case "is terminal lifecycle" `Quick test_is_terminal_lifecycle;
    Alcotest.test_case "teams delivery records lifecycle" `Quick
      test_teams_delivery_records_lifecycle;
    Alcotest.test_case "non-teams no lifecycle" `Quick
      test_non_teams_no_lifecycle;
  ]
