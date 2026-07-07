(** Regression tests for Teams delivery lifecycle edge cases.

    Covers:
    - Empty activity ID (user_visible_unconfirmed)
    - Missing / invalid service_url guards
    - Card update failure with fallback to new card send
    - Edit failure with fallback to new message send
    - Scheduler / background delivery tracking ID correlation *)

open Background_task_0_format

let with_db f = Test_helpers.with_memory_store f

let metadata_string key json =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt key fields with Some (`String s) -> s | _ -> "")
  | _ -> ""

let contains_substring s sub =
  let s_len = String.length s in
  let sub_len = String.length sub in
  let rec search i =
    if i + sub_len > s_len then false
    else if String.sub s i sub_len = sub then true
    else search (i + 1)
  in
  sub_len = 0 || search 0

(** Create a minimal background task for testing. *)
let make_task ?(id = 100) ?(channel = Some "teams")
    ?(channel_id = Some "room-100") ?(prompt = "test prompt") () =
  {
    id;
    runner = Codex;
    model = None;
    repo_path = "/tmp";
    prompt;
    branch = "main";
    worktree_path = None;
    log_path = None;
    status = Running;
    runner_session_id = None;
    session_key = Some "teams:room-100:user-1";
    channel;
    channel_id;
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
                ("room_id", `String (Option.value channel_id ~default:""));
                ("connector", `String (Option.value channel ~default:""));
              ]));
    thread_id = None;
    requester = None;
    progress_state = None;
    access_snapshot_id = None;
    restart_policy = Background_task.Reenqueue;
    restart_count = 0;
    max_restarts = Background_task.max_restarts_default;
  }

(** {1 Empty activity ID / empty message ID regression tests} *)

(** When [send] returns an empty string, [deliver_progress_update] must record a
    [User_visible_unconfirmed] lifecycle event. *)
let test_empty_message_id_records_user_visible_unconfirmed () =
  with_db (fun db ->
      Hashtbl.remove Room_progress.progress_msg_ids 200;
      let send ~room_id:_ ?thread_id:_ ~text:_ () = Lwt.return "" in
      let edit ~room_id:_ ~msg_id:_ ~text:_ = Lwt.return_unit in
      let task = make_task ~id:200 () in
      Lwt_main.run
        (Room_progress.deliver_progress_update ~send ~edit ~db ~task ());
      (* We query all events for room-100 since tracking_id is generated
         internally by deliver_progress_update. *)
      let all_events = Room_activity_ledger.query ~db ~room_id:"room-100" () in
      let delivery_events =
        List.filter
          (fun (e : Room_activity_ledger.event) ->
            String.starts_with ~prefix:"teams_delivery_" e.event_type)
          all_events
      in
      Alcotest.(check bool)
        "has lifecycle events" true
        (List.length delivery_events > 0);
      let event_types =
        List.map
          (fun (e : Room_activity_ledger.event) -> e.event_type)
          delivery_events
      in
      Alcotest.(check bool)
        "has user_visible_unconfirmed" true
        (List.mem "teams_delivery_user_visible_unconfirmed" event_types);
      Hashtbl.remove Room_progress.progress_msg_ids 200)

(** When [send] returns "0" (a placeholder), [deliver_progress_update] must also
    record [User_visible_unconfirmed]. *)
let test_placeholder_message_id_zero_records_unconfirmed () =
  with_db (fun db ->
      Hashtbl.remove Room_progress.progress_msg_ids 201;
      let send ~room_id:_ ?thread_id:_ ~text:_ () = Lwt.return "0" in
      let edit ~room_id:_ ~msg_id:_ ~text:_ = Lwt.return_unit in
      let task = make_task ~id:201 () in
      Lwt_main.run
        (Room_progress.deliver_progress_update ~send ~edit ~db ~task ());
      let all_events = Room_activity_ledger.query ~db ~room_id:"room-100" () in
      let delivery_events =
        List.filter
          (fun (e : Room_activity_ledger.event) ->
            String.starts_with ~prefix:"teams_delivery_" e.event_type)
          all_events
      in
      let event_types =
        List.map
          (fun (e : Room_activity_ledger.event) -> e.event_type)
          delivery_events
      in
      Alcotest.(check bool)
        "has user_visible_unconfirmed for msg_id=0" true
        (List.mem "teams_delivery_user_visible_unconfirmed" event_types);
      Hashtbl.remove Room_progress.progress_msg_ids 201)

(** {1 Missing / invalid service_url guard tests} *)

(** [Teams.send_reply] with an empty service_url must short-circuit and return
    an empty activity ID without attempting HTTP. *)
let test_send_reply_empty_service_url () =
  let config = Test_teams.test_teams_config () in
  let result =
    Lwt_main.run
      (Teams.send_reply ~config ~service_url:"" ~conversation_id:"conv-1"
         ~reply_to_id:"" ~text:"hello" ())
  in
  Alcotest.(check string) "empty service_url returns empty" "" result

(** [Teams.send_reply] with a malformed service_url (no scheme) must
    short-circuit. *)
let test_send_reply_no_scheme_service_url () =
  let config = Test_teams.test_teams_config () in
  let result =
    Lwt_main.run
      (Teams.send_reply ~config ~service_url:"not-a-url"
         ~conversation_id:"conv-1" ~reply_to_id:"" ~text:"hello" ())
  in
  Alcotest.(check string) "no-scheme service_url returns empty" "" result

(** [Teams.send_reply] with an ftp:// scheme (invalid for Bot Framework) must
    short-circuit. *)
let test_send_reply_ftp_scheme_service_url () =
  let config = Test_teams.test_teams_config () in
  let result =
    Lwt_main.run
      (Teams.send_reply ~config ~service_url:"ftp://example.com"
         ~conversation_id:"conv-1" ~reply_to_id:"" ~text:"hello" ())
  in
  Alcotest.(check string) "ftp scheme returns empty" "" result

(** {1 Card update failure / fallback send tests} *)

(** When [edit_adaptive_card] raises an exception, [send_or_edit_card] must
    record [Edit_failed], fall back to [send_card], and record [Fallback_sent] +
    [Message_id_recorded]. *)
let test_card_edit_failure_falls_back_to_send () =
  with_db (fun db ->
      Hashtbl.remove Room_progress.progress_msg_ids 300;
      (* Seed a known message ID so the edit path is attempted *)
      Hashtbl.replace Room_progress.progress_msg_ids 300 "old-card-msg-id";
      let card_sent = ref false in
      let send_card ~room_id:_ ?thread_id:_ ~card:_ () =
        card_sent := true;
        Lwt.return "new-card-msg-id"
      in
      let edit_card ~room_id:_ ~msg_id:_ ~card:_ () =
        Lwt.fail (Failure "card edit HTTP 404")
      in
      let send_text ~room_id:_ ?thread_id:_ ~text:_ () =
        Lwt.return "should-not-be-called"
      in
      let edit_text ~room_id:_ ~msg_id:_ ~text:_ = Lwt.return_unit in
      let card = `Assoc [ ("type", `String "AdaptiveCard") ] in
      let tracking_id = Teams_delivery_lifecycle.generate_tracking_id () in
      let lifecycle_ctx : Room_progress.lifecycle_ctx =
        {
          db;
          room_id = "room-card";
          connector = "teams";
          tracking_id;
          task_id = 300;
          thread_id = None;
        }
      in
      let result, msg_id =
        Lwt_main.run
          (Room_progress.send_or_edit_card ~send_card:(Some send_card)
             ~edit_card:(Some edit_card) ~send_text ~edit_text
             ~room_id:"room-card" ~card ~fallback_text:"fallback" ~task_id:300
             ~lifecycle_ctx ())
      in
      Alcotest.(check bool) "card_sent after edit failure" true !card_sent;
      (match result with
      | Room_progress.Delivered -> ()
      | _ -> Alcotest.fail "expected Delivered after fallback");
      Alcotest.(check (option string))
        "new message ID" (Some "new-card-msg-id") msg_id;
      (* Verify lifecycle events *)
      let events =
        Teams_delivery_lifecycle.query_by_tracking_id ~db ~tracking_id ()
      in
      let event_types =
        List.map (fun (e : Room_activity_ledger.event) -> e.event_type) events
      in
      Alcotest.(check bool)
        "has edit_failed" true
        (List.mem "teams_delivery_edit_failed" event_types);
      Alcotest.(check bool)
        "has fallback_sent" true
        (List.mem "teams_delivery_fallback_sent" event_types);
      Alcotest.(check bool)
        "has message_id_recorded" true
        (List.mem "teams_delivery_message_id_recorded" event_types);
      (* Verify the token in error is not leaked *)
      let edit_failed_evt =
        List.find
          (fun (e : Room_activity_ledger.event) ->
            e.event_type = "teams_delivery_edit_failed")
          events
      in
      let error_str = metadata_string "error" edit_failed_evt.metadata in
      Alcotest.(check bool)
        "error contains reason" true
        (contains_substring error_str "card edit HTTP 404");
      Hashtbl.remove Room_progress.progress_msg_ids 300)

(** When card edit fails and send_card returns empty, delivery is marked
    [Delivery_failed]. *)
let test_card_edit_failure_send_also_fails () =
  with_db (fun db ->
      Hashtbl.replace Room_progress.progress_msg_ids 301 "old-id";
      let send_card ~room_id:_ ?thread_id:_ ~card:_ () = Lwt.return "" in
      let edit_card ~room_id:_ ~msg_id:_ ~card:_ () =
        Lwt.fail (Failure "edit failed")
      in
      let send_text ~room_id:_ ?thread_id:_ ~text:_ () =
        Lwt.return "should-not"
      in
      let edit_text ~room_id:_ ~msg_id:_ ~text:_ = Lwt.return_unit in
      let card = `Assoc [] in
      let tracking_id = Teams_delivery_lifecycle.generate_tracking_id () in
      let lifecycle_ctx : Room_progress.lifecycle_ctx =
        {
          db;
          room_id = "room-card-fail";
          connector = "teams";
          tracking_id;
          task_id = 301;
          thread_id = None;
        }
      in
      let result, _msg_id =
        Lwt_main.run
          (Room_progress.send_or_edit_card ~send_card:(Some send_card)
             ~edit_card:(Some edit_card) ~send_text ~edit_text
             ~room_id:"room-card-fail" ~card ~fallback_text:"fallback"
             ~task_id:301 ~lifecycle_ctx ())
      in
      (match result with
      | Room_progress.Delivery_failed _ -> ()
      | _ -> Alcotest.fail "expected Delivery_failed");
      let events =
        Teams_delivery_lifecycle.query_by_tracking_id ~db ~tracking_id ()
      in
      let event_types =
        List.map (fun (e : Room_activity_ledger.event) -> e.event_type) events
      in
      Alcotest.(check bool)
        "has user_visible_unconfirmed" true
        (List.mem "teams_delivery_user_visible_unconfirmed" event_types);
      Hashtbl.remove Room_progress.progress_msg_ids 301)

(** {1 Edit failure with fallback to send tests} *)

(** When [edit] raises, [send_or_edit] must record [Edit_failed] and fall back
    to [send], recording [Fallback_sent] on success. *)
let test_edit_failure_falls_back_to_send () =
  with_db (fun db ->
      Hashtbl.replace Room_progress.progress_msg_ids 400 "existing-msg-id";
      let send_called = ref false in
      let send ~room_id:_ ?thread_id:_ ~text:_ () =
        send_called := true;
        Lwt.return "fallback-msg-id"
      in
      let edit ~room_id:_ ~msg_id:_ ~text:_ =
        Lwt.fail (Failure "PUT returned 412 Precondition Failed")
      in
      let tracking_id = Teams_delivery_lifecycle.generate_tracking_id () in
      let lifecycle_ctx : Room_progress.lifecycle_ctx =
        {
          db;
          room_id = "room-fallback";
          connector = "teams";
          tracking_id;
          task_id = 400;
          thread_id = None;
        }
      in
      let result, msg_id =
        Lwt_main.run
          (Room_progress.send_or_edit ~send ~edit ~room_id:"room-fallback"
             ~text:"updated progress" ~task_id:400 ~lifecycle_ctx ())
      in
      Alcotest.(check bool) "send was called" true !send_called;
      (match result with
      | Room_progress.Delivered -> ()
      | _ -> Alcotest.fail "expected Delivered after fallback");
      Alcotest.(check (option string))
        "fallback msg id" (Some "fallback-msg-id") msg_id;
      let events =
        Teams_delivery_lifecycle.query_by_tracking_id ~db ~tracking_id ()
      in
      let event_types =
        List.map (fun (e : Room_activity_ledger.event) -> e.event_type) events
      in
      Alcotest.(check bool)
        "has edit_failed" true
        (List.mem "teams_delivery_edit_failed" event_types);
      Alcotest.(check bool)
        "has fallback_sent" true
        (List.mem "teams_delivery_fallback_sent" event_types);
      Hashtbl.remove Room_progress.progress_msg_ids 400)

(** When [edit] raises and [send] returns empty, delivery is [Delivery_failed].
*)
let test_edit_failure_send_also_fails () =
  with_db (fun db ->
      Hashtbl.replace Room_progress.progress_msg_ids 401 "existing-id";
      let send ~room_id:_ ?thread_id:_ ~text:_ () = Lwt.return "" in
      let edit ~room_id:_ ~msg_id:_ ~text:_ =
        Lwt.fail (Failure "edit failed")
      in
      let tracking_id = Teams_delivery_lifecycle.generate_tracking_id () in
      let lifecycle_ctx : Room_progress.lifecycle_ctx =
        {
          db;
          room_id = "room-double-fail";
          connector = "teams";
          tracking_id;
          task_id = 401;
          thread_id = None;
        }
      in
      let result, _ =
        Lwt_main.run
          (Room_progress.send_or_edit ~send ~edit ~room_id:"room-double-fail"
             ~text:"text" ~task_id:401 ~lifecycle_ctx ())
      in
      (match result with
      | Room_progress.Delivery_failed _ -> ()
      | _ -> Alcotest.fail "expected Delivery_failed");
      let events =
        Teams_delivery_lifecycle.query_by_tracking_id ~db ~tracking_id ()
      in
      let event_types =
        List.map (fun (e : Room_activity_ledger.event) -> e.event_type) events
      in
      Alcotest.(check bool)
        "has edit_failed" true
        (List.mem "teams_delivery_edit_failed" event_types);
      Alcotest.(check bool)
        "has user_visible_unconfirmed" true
        (List.mem "teams_delivery_user_visible_unconfirmed" event_types);
      Hashtbl.remove Room_progress.progress_msg_ids 401)

(** Successful edit records [Message_id_recorded] with the original message ID —
    no fallback needed. *)
let test_successful_edit_records_message_id_recorded () =
  with_db (fun db ->
      Hashtbl.replace Room_progress.progress_msg_ids 402 "known-msg-id";
      let send ~room_id:_ ?thread_id:_ ~text:_ () =
        Lwt.return "should-not-be-called"
      in
      let edit ~room_id:_ ~msg_id:_ ~text:_ = Lwt.return_unit in
      let tracking_id = Teams_delivery_lifecycle.generate_tracking_id () in
      let lifecycle_ctx : Room_progress.lifecycle_ctx =
        {
          db;
          room_id = "room-edit-ok";
          connector = "teams";
          tracking_id;
          task_id = 402;
          thread_id = None;
        }
      in
      let result, msg_id =
        Lwt_main.run
          (Room_progress.send_or_edit ~send ~edit ~room_id:"room-edit-ok"
             ~text:"updated" ~task_id:402 ~lifecycle_ctx ())
      in
      (match result with
      | Room_progress.Delivered -> ()
      | _ -> Alcotest.fail "expected Delivered");
      Alcotest.(check (option string))
        "same msg id" (Some "known-msg-id") msg_id;
      let events =
        Teams_delivery_lifecycle.query_by_tracking_id ~db ~tracking_id ()
      in
      let event_types =
        List.map (fun (e : Room_activity_ledger.event) -> e.event_type) events
      in
      Alcotest.(check bool)
        "has message_id_recorded" true
        (List.mem "teams_delivery_message_id_recorded" event_types);
      Alcotest.(check bool)
        "no edit_failed" false
        (List.mem "teams_delivery_edit_failed" event_types);
      Hashtbl.remove Room_progress.progress_msg_ids 402)

(** {1 Scheduler / background delivery correlation tests} *)

(** Two successive [deliver_progress_update] calls for the same task produce
    distinct tracking IDs (one per delivery), but each individual delivery's
    lifecycle events share a single tracking ID. *)
let test_two_deliveries_produce_distinct_tracking_ids () =
  with_db (fun db ->
      Hashtbl.remove Room_progress.progress_msg_ids 500;
      let send_count = ref 0 in
      let send ~room_id:_ ?thread_id:_ ~text:_ () =
        incr send_count;
        Lwt.return (Printf.sprintf "msg-delivery-%d" !send_count)
      in
      let edit ~room_id:_ ~msg_id:_ ~text:_ = Lwt.return_unit in
      let task = make_task ~id:500 () in
      (* First delivery *)
      Lwt_main.run
        (Room_progress.deliver_progress_update ~send ~edit ~db ~task ());
      (* Second delivery *)
      let task2 =
        { task with progress_state = Some Background_task_0_format.Working }
      in
      Lwt_main.run
        (Room_progress.deliver_progress_update ~send ~edit ~db ~task:task2 ());
      let all_events = Room_activity_ledger.query ~db ~room_id:"room-100" () in
      let lifecycle_events =
        List.filter
          (fun (e : Room_activity_ledger.event) ->
            String.starts_with ~prefix:"teams_delivery_" e.event_type)
          all_events
      in
      Alcotest.(check bool)
        "has lifecycle events from both deliveries" true
        (List.length lifecycle_events >= 6);
      (* Extract unique tracking IDs *)
      let tracking_ids =
        List.map
          (fun (e : Room_activity_ledger.event) ->
            metadata_string "tracking_id" e.metadata)
          lifecycle_events
        |> List.filter (fun s -> s <> "")
        |> List.sort_uniq String.compare
      in
      Alcotest.(check int)
        "two distinct tracking IDs" 2 (List.length tracking_ids);
      (* Each tracking ID should have events from the same delivery *)
      List.iter
        (fun tid ->
          let events_for_tid =
            List.filter
              (fun (e : Room_activity_ledger.event) ->
                metadata_string "tracking_id" e.metadata = tid)
              lifecycle_events
          in
          Alcotest.(check bool)
            (Printf.sprintf "tracking_id %s has events" tid)
            true
            (List.length events_for_tid >= 2))
        tracking_ids;
      Hashtbl.remove Room_progress.progress_msg_ids 500)

(** [deliver_progress_update] for a non-Teams connector (e.g. slack) must NOT
    record any [teams_delivery_*] lifecycle events. *)
let test_non_teams_connector_no_lifecycle () =
  with_db (fun db ->
      Hashtbl.remove Room_progress.progress_msg_ids 600;
      let send ~room_id:_ ?thread_id:_ ~text:_ () = Lwt.return "slack-msg-1" in
      let edit ~room_id:_ ~msg_id:_ ~text:_ = Lwt.return_unit in
      let task = make_task ~id:600 ~channel:(Some "slack") () in
      Lwt_main.run
        (Room_progress.deliver_progress_update ~send ~edit ~db ~task ());
      let all_events = Room_activity_ledger.query ~db ~room_id:"room-100" () in
      let lifecycle_events =
        List.filter
          (fun (e : Room_activity_ledger.event) ->
            String.starts_with ~prefix:"teams_delivery_" e.event_type)
          all_events
      in
      Alcotest.(check int)
        "no teams lifecycle for slack" 0
        (List.length lifecycle_events);
      Hashtbl.remove Room_progress.progress_msg_ids 600)

(** [deliver_progress_update] with no database does not crash and does not
    record lifecycle events. *)
let test_no_database_no_lifecycle_no_crash () =
  Hashtbl.remove Room_progress.progress_msg_ids 700;
  let send ~room_id:_ ?thread_id:_ ~text:_ () = Lwt.return "msg-no-db" in
  let edit ~room_id:_ ~msg_id:_ ~text:_ = Lwt.return_unit in
  let task = make_task ~id:700 () in
  (* Should not raise *)
  Lwt_main.run
    (Room_progress.deliver_progress_update ~send ~edit ?db:None ~task ());
  Hashtbl.remove Room_progress.progress_msg_ids 700

(** Empty room_id in origin metadata causes [Skipped] — no delivery attempted.
*)
let test_empty_room_id_skips_delivery () =
  with_db (fun db ->
      Hashtbl.remove Room_progress.progress_msg_ids 710;
      let send_called = ref false in
      let send ~room_id:_ ?thread_id:_ ~text:_ () =
        send_called := true;
        Lwt.return "should-not"
      in
      let edit ~room_id:_ ~msg_id:_ ~text:_ = Lwt.return_unit in
      let task =
        make_task ~id:710 ~channel_id:None ~channel:(Some "teams") ()
      in
      (* Override origin_json to have empty room_id *)
      let task = { task with origin_json = Some {|{"connector":"teams"}|} } in
      Lwt_main.run
        (Room_progress.deliver_progress_update ~send ~edit ~db ~task ());
      Alcotest.(check bool)
        "send not called for empty room_id" false !send_called;
      Hashtbl.remove Room_progress.progress_msg_ids 710)

(** {1 Lifecycle event error sanitization tests} *)

(** Edit failure with a bearer token in the error must have the token redacted.
*)
let test_edit_failure_error_redacts_tokens () =
  with_db (fun db ->
      Hashtbl.replace Room_progress.progress_msg_ids 800 "old-id";
      let send ~room_id:_ ?thread_id:_ ~text:_ () = Lwt.return "fallback-id" in
      let edit ~room_id:_ ~msg_id:_ ~text:_ =
        Lwt.fail
          (Failure
             "Bearer eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0 HTTP 401 \
              Unauthorized")
      in
      let tracking_id = Teams_delivery_lifecycle.generate_tracking_id () in
      let lifecycle_ctx : Room_progress.lifecycle_ctx =
        {
          db;
          room_id = "room-token-leak";
          connector = "teams";
          tracking_id;
          task_id = 800;
          thread_id = None;
        }
      in
      let result, _ =
        Lwt_main.run
          (Room_progress.send_or_edit ~send ~edit ~room_id:"room-token-leak"
             ~text:"text" ~task_id:800 ~lifecycle_ctx ())
      in
      (match result with
      | Room_progress.Delivered -> ()
      | _ -> Alcotest.fail "expected Delivered after fallback");
      let events =
        Teams_delivery_lifecycle.query_by_tracking_id ~db ~tracking_id ()
      in
      let edit_failed_evt =
        List.find_opt
          (fun (e : Room_activity_ledger.event) ->
            e.event_type = "teams_delivery_edit_failed")
          events
      in
      (match edit_failed_evt with
      | None -> Alcotest.fail "expected edit_failed event"
      | Some evt ->
          let error = metadata_string "error" evt.metadata in
          Alcotest.(check bool)
            "JWT token redacted" false
            (contains_substring error "eyJhbGciOiJIUzI1NiJ9"));
      Hashtbl.remove Room_progress.progress_msg_ids 800)

(** {1 Non-Teams connector skips lifecycle} *)

(** [deliver_final_message] for a Teams connector records lifecycle events
    including final delivery. *)
let test_final_message_records_lifecycle () =
  with_db (fun db ->
      Hashtbl.replace Room_progress.progress_msg_ids 900 "existing-final-msg";
      let send ~room_id:_ ?thread_id:_ ~text:_ () =
        Lwt.return "final-fallback-id"
      in
      let edit ~room_id:_ ~msg_id:_ ~text:_ = Lwt.return_unit in
      let task = make_task ~id:900 () in
      let task = { task with status = Succeeded } in
      let result =
        Lwt_main.run
          (Room_progress.deliver_final_message ~send ~edit ~db ~task ())
      in
      (match result with
      | Room_progress.Delivered -> ()
      | _ -> Alcotest.fail "expected Delivered for final message");
      let all_events = Room_activity_ledger.query ~db ~room_id:"room-100" () in
      let lifecycle_events =
        List.filter
          (fun (e : Room_activity_ledger.event) ->
            String.starts_with ~prefix:"teams_delivery_" e.event_type)
          all_events
      in
      Alcotest.(check bool)
        "final message has lifecycle events" true
        (List.length lifecycle_events >= 3);
      let event_types =
        List.map
          (fun (e : Room_activity_ledger.event) -> e.event_type)
          lifecycle_events
      in
      Alcotest.(check bool)
        "has message_id_recorded" true
        (List.mem "teams_delivery_message_id_recorded" event_types);
      Hashtbl.remove Room_progress.progress_msg_ids 900)

(** {1 Progress message ID management tests} *)

(** [send_or_edit] tracks the message ID in [progress_msg_ids] after first
    successful send. Subsequent calls use the tracked ID for edit. *)
let test_progress_msg_id_tracking_across_deliveries () =
  with_db (fun db ->
      Hashtbl.remove Room_progress.progress_msg_ids 950;
      let send_count = ref 0 in
      let send ~room_id:_ ?thread_id:_ ~text:_ () =
        incr send_count;
        Lwt.return (Printf.sprintf "msg-%d" !send_count)
      in
      let edit_calls = ref 0 in
      let edit ~room_id:_ ~msg_id:_ ~text:_ =
        incr edit_calls;
        Lwt.return_unit
      in
      let tracking_id_1 = Teams_delivery_lifecycle.generate_tracking_id () in
      let lifecycle_ctx_1 : Room_progress.lifecycle_ctx =
        {
          db;
          room_id = "room-tracking";
          connector = "teams";
          tracking_id = tracking_id_1;
          task_id = 950;
          thread_id = None;
        }
      in
      (* First call: no tracked msg_id, should send new *)
      let result1, msg1 =
        Lwt_main.run
          (Room_progress.send_or_edit ~send ~edit ~room_id:"room-tracking"
             ~text:"first" ~task_id:950 ~lifecycle_ctx:lifecycle_ctx_1 ())
      in
      (match result1 with
      | Room_progress.Delivered -> ()
      | _ -> Alcotest.fail "first delivery should succeed");
      Alcotest.(check (option string)) "first msg_id" (Some "msg-1") msg1;
      Alcotest.(check int) "send called once" 1 !send_count;
      Alcotest.(check int) "edit not called" 0 !edit_calls;
      (* Second call: should use tracked msg_id and edit *)
      let tracking_id_2 = Teams_delivery_lifecycle.generate_tracking_id () in
      let lifecycle_ctx_2 : Room_progress.lifecycle_ctx =
        {
          db;
          room_id = "room-tracking";
          connector = "teams";
          tracking_id = tracking_id_2;
          task_id = 950;
          thread_id = None;
        }
      in
      let result2, msg2 =
        Lwt_main.run
          (Room_progress.send_or_edit ~send ~edit ~room_id:"room-tracking"
             ~text:"second" ~task_id:950 ~lifecycle_ctx:lifecycle_ctx_2 ())
      in
      (match result2 with
      | Room_progress.Delivered -> ()
      | _ -> Alcotest.fail "second delivery should succeed");
      Alcotest.(check (option string))
        "second msg_id same as first" (Some "msg-1") msg2;
      Alcotest.(check int) "send still called once" 1 !send_count;
      Alcotest.(check int) "edit called once" 1 !edit_calls;
      Hashtbl.remove Room_progress.progress_msg_ids 950)

(** {1 Suite} *)

let suite =
  [
    Alcotest.test_case "empty message id records user_visible_unconfirmed"
      `Quick test_empty_message_id_records_user_visible_unconfirmed;
    Alcotest.test_case "placeholder message id 0 records unconfirmed" `Quick
      test_placeholder_message_id_zero_records_unconfirmed;
    Alcotest.test_case "send_reply empty service_url" `Quick
      test_send_reply_empty_service_url;
    Alcotest.test_case "send_reply no-scheme service_url" `Quick
      test_send_reply_no_scheme_service_url;
    Alcotest.test_case "send_reply ftp scheme service_url" `Quick
      test_send_reply_ftp_scheme_service_url;
    Alcotest.test_case "card edit failure falls back to send" `Quick
      test_card_edit_failure_falls_back_to_send;
    Alcotest.test_case "card edit failure send also fails" `Quick
      test_card_edit_failure_send_also_fails;
    Alcotest.test_case "edit failure falls back to send" `Quick
      test_edit_failure_falls_back_to_send;
    Alcotest.test_case "edit failure send also fails" `Quick
      test_edit_failure_send_also_fails;
    Alcotest.test_case "successful edit records message_id_recorded" `Quick
      test_successful_edit_records_message_id_recorded;
    Alcotest.test_case "two deliveries produce distinct tracking ids" `Quick
      test_two_deliveries_produce_distinct_tracking_ids;
    Alcotest.test_case "non-teams connector no lifecycle" `Quick
      test_non_teams_connector_no_lifecycle;
    Alcotest.test_case "no database no lifecycle no crash" `Quick
      test_no_database_no_lifecycle_no_crash;
    Alcotest.test_case "empty room_id skips delivery" `Quick
      test_empty_room_id_skips_delivery;
    Alcotest.test_case "edit failure error redacts tokens" `Quick
      test_edit_failure_error_redacts_tokens;
    Alcotest.test_case "final message records lifecycle" `Quick
      test_final_message_records_lifecycle;
    Alcotest.test_case "progress msg id tracking across deliveries" `Quick
      test_progress_msg_id_tracking_across_deliveries;
  ]
