(** Tests for [Room_ambient_delivery] — safe ambient follow-up delivery. *)

open Room_ambient_delivery

let with_db f =
  let db = Memory.init ~db_path:":memory:" () in
  Background_task.init_schema db;
  Task_tree_core.init_schema db;
  Room_watcher_decision.init_schema db;
  Room_activity_ledger.init_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let make_origin_json ?room_id ?thread_id ?requester () =
  let open Yojson.Safe in
  let fields =
    [
      ("connector", Some (`String "slack"));
      ("room_id", Option.map (fun s -> `String s) room_id);
      ("thread_id", Option.map (fun s -> `String s) thread_id);
      ("requester_name", Option.map (fun s -> `String s) requester);
    ]
    |> List.filter_map (fun (k, v) -> Option.map (fun v -> (k, v)) v)
  in
  Yojson.Safe.to_string (`Assoc fields)

let enqueue_bg_task ~db ?(runner = Background_task.Codex) ~repo_path ~prompt
    ?origin_json ?thread_id ?requester () =
  match
    Background_task.enqueue ~db ~runner ~repo_path ~prompt ?origin_json
      ?thread_id ?requester ()
  with
  | Ok id -> id
  | Error msg -> Alcotest.fail ("enqueue failed: " ^ msg)

let set_bg_created_at ~db ~id ~datetime =
  let sql = "UPDATE background_tasks SET created_at = ? WHERE id = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT datetime));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int id)));
      ignore (Sqlite3.step stmt))

let make_profile ?(ambient_enabled = false)
    ?(ambient_quiet_start = Ambient_policy.default_ambient_quiet_start)
    ?(ambient_quiet_end = Ambient_policy.default_ambient_quiet_end)
    ?(ambient_rate_limit_rph = 0) () =
  Ambient_policy.make_profile ~ambient_enabled ~ambient_quiet_start
    ~ambient_quiet_end ~ambient_rate_limit_rph ~id:"test-profile"
    ~display_name:(Some "Test") ~model:"gpt-5.4" ~system_prompt:""
    ~max_tool_iterations:10 ~status:"active" ~allowed_tools:[] ~denied_tools:[]
    ()

let make_stale_item ?(source = `Background_task) ?(id = "1")
    ?(title = "test task") ?(status = "queued") ?(room_id = Some "room-1")
    ?(thread_id = None) ?(requester = Some "Alice") ?(age_seconds = 7200.0) () =
  {
    Room_stale_query.source;
    id;
    title;
    status;
    room_id;
    thread_id;
    requester;
    created_at = "2026-06-28T00:00:00";
    age_seconds;
  }

let ok_sender ~room_id:_ ?thread_id:_ ~message:_ () = Lwt.return (Ok ())

let failing_sender ~room_id:_ ?thread_id:_ ~message:_ () =
  Lwt.return (Error "connector offline")

let recording_sender calls ~room_id ?thread_id ~message:_ () =
  calls := (room_id, thread_id) :: !calls;
  Lwt.return (Ok ())

(** Delivery is blocked when ambient is not enabled. *)
let test_policy_blocks_when_disabled () =
  with_db (fun db ->
      let profile = make_profile ~ambient_enabled:false () in
      let items = [ make_stale_item () ] in
      let results =
        Lwt_main.run
          (deliver_ambient_followups ~db ~profile ~room_id:"room-1"
             ~stale_items:items ~hour:12 ~budget_exceeded:false
             ~supports_ambient:true ~send_message:ok_sender ())
      in
      Alcotest.(check int) "one result" 1 (List.length results);
      let r = List.hd results in
      Alcotest.(check bool) "not acted" false r.acted;
      Alcotest.check
        (Alcotest.option Alcotest.string)
        "skip reason" (Some "policy_denied")
        (Option.map Room_watcher_decision.skip_reason_to_string r.skip_reason))

(** Delivery is blocked during quiet hours. *)
let test_policy_blocks_quiet_hours () =
  with_db (fun db ->
      let profile = make_profile ~ambient_enabled:true () in
      let items = [ make_stale_item () ] in
      let results =
        Lwt_main.run
          (deliver_ambient_followups ~db ~profile ~room_id:"room-1"
             ~stale_items:items ~hour:2 ~budget_exceeded:false
             ~supports_ambient:true ~send_message:ok_sender ())
      in
      let r = List.hd results in
      Alcotest.(check bool) "not acted" false r.acted;
      Alcotest.check
        (Alcotest.option Alcotest.string)
        "quiet hours" (Some "quiet_hours")
        (Option.map Room_watcher_decision.skip_reason_to_string r.skip_reason))

(** Delivery is blocked when rate limit exceeded. *)
let test_policy_blocks_rate_limited () =
  with_db (fun db ->
      let profile =
        make_profile ~ambient_enabled:true ~ambient_rate_limit_rph:1 ()
      in
      (* First delivery succeeds *)
      let items = [ make_stale_item ~id:"1" () ] in
      let _results1 =
        Lwt_main.run
          (deliver_ambient_followups ~db ~profile ~room_id:"room-1"
             ~stale_items:items ~hour:12 ~budget_exceeded:false
             ~supports_ambient:true ~send_message:ok_sender ())
      in
      (* Second delivery in the same hour should be rate limited *)
      let items2 = [ make_stale_item ~id:"2" () ] in
      let results2 =
        Lwt_main.run
          (deliver_ambient_followups ~db ~profile ~room_id:"room-1"
             ~stale_items:items2 ~hour:12 ~budget_exceeded:false
             ~supports_ambient:true ~send_message:ok_sender ())
      in
      let r = List.hd results2 in
      Alcotest.(check bool) "not acted" false r.acted;
      Alcotest.check
        (Alcotest.option Alcotest.string)
        "rate limited" (Some "rate_limited")
        (Option.map Room_watcher_decision.skip_reason_to_string r.skip_reason))

(** Delivery is blocked when budget exceeded. *)
let test_policy_blocks_budget_exceeded () =
  with_db (fun db ->
      let profile = make_profile ~ambient_enabled:true () in
      let items = [ make_stale_item () ] in
      let results =
        Lwt_main.run
          (deliver_ambient_followups ~db ~profile ~room_id:"room-1"
             ~stale_items:items ~hour:12 ~budget_exceeded:true
             ~supports_ambient:true ~send_message:ok_sender ())
      in
      let r = List.hd results in
      Alcotest.(check bool) "not acted" false r.acted;
      Alcotest.check
        (Alcotest.option Alcotest.string)
        "budget exceeded" (Some "budget_exceeded")
        (Option.map Room_watcher_decision.skip_reason_to_string r.skip_reason))

(** Delivery is blocked when connector does not support ambient. *)
let test_policy_blocks_connector_unsupported () =
  with_db (fun db ->
      let profile = make_profile ~ambient_enabled:true () in
      let items = [ make_stale_item () ] in
      let results =
        Lwt_main.run
          (deliver_ambient_followups ~db ~profile ~room_id:"room-1"
             ~stale_items:items ~hour:12 ~budget_exceeded:false
             ~supports_ambient:false ~send_message:ok_sender ())
      in
      let r = List.hd results in
      Alcotest.(check bool) "not acted" false r.acted;
      Alcotest.check
        (Alcotest.option Alcotest.string)
        "connector unsupported" (Some "connector_unsupported")
        (Option.map Room_watcher_decision.skip_reason_to_string r.skip_reason))

(** Successful delivery records in ledger and returns acted=true. *)
let test_successful_delivery () =
  with_db (fun db ->
      let profile = make_profile ~ambient_enabled:true () in
      let items = [ make_stale_item ~id:"42" ~title:"Fix bug" () ] in
      let results =
        Lwt_main.run
          (deliver_ambient_followups ~db ~profile ~room_id:"room-1"
             ~stale_items:items ~hour:12 ~budget_exceeded:false
             ~supports_ambient:true ~send_message:ok_sender ())
      in
      let r = List.hd results in
      Alcotest.(check bool) "acted" true r.acted;
      Alcotest.check
        (Alcotest.option Alcotest.string)
        "no skip" None
        (Option.map Room_watcher_decision.skip_reason_to_string r.skip_reason);
      Alcotest.check
        (Alcotest.option Alcotest.string)
        "no error" None r.delivery_error;
      (* Verify ledger entry *)
      let events =
        Room_activity_ledger.query ~db ~room_id:"room-1"
          ~event_type:"ambient_delivery" ()
      in
      Alcotest.(check int) "ledger entry" 1 (List.length events);
      let event = List.hd events in
      Alcotest.(check string) "actor" "ambient_watcher" event.actor)

(** Delivery failure is recorded in ledger with error. *)
let test_delivery_failure_recorded () =
  with_db (fun db ->
      let profile = make_profile ~ambient_enabled:true () in
      let items = [ make_stale_item ~id:"77" ~title:"Deploy" () ] in
      let results =
        Lwt_main.run
          (deliver_ambient_followups ~db ~profile ~room_id:"room-1"
             ~stale_items:items ~hour:12 ~budget_exceeded:false
             ~supports_ambient:true ~send_message:failing_sender ())
      in
      let r = List.hd results in
      Alcotest.(check bool) "not acted" false r.acted;
      Alcotest.check
        (Alcotest.option Alcotest.string)
        "delivery error" (Some "connector offline") r.delivery_error;
      (* Verify failure ledger entry *)
      let events =
        Room_activity_ledger.query ~db ~room_id:"room-1"
          ~event_type:"ambient_delivery_failed" ()
      in
      Alcotest.(check int) "failure ledger entry" 1 (List.length events);
      let event = List.hd events in
      Alcotest.(check string) "actor" "ambient_watcher" event.actor)

(** Material-change suppression: duplicate delivery is suppressed. *)
let test_material_change_suppression () =
  with_db (fun db ->
      let profile = make_profile ~ambient_enabled:true () in
      let items = [ make_stale_item ~id:"99" ~title:"Review PR" () ] in
      let _results1 =
        Lwt_main.run
          (deliver_ambient_followups ~db ~profile ~room_id:"room-1"
             ~stale_items:items ~hour:12 ~budget_exceeded:false
             ~supports_ambient:true ~send_message:ok_sender ())
      in
      (* Second call with same item/fingerprint should be suppressed *)
      let results2 =
        Lwt_main.run
          (deliver_ambient_followups ~db ~profile ~room_id:"room-1"
             ~stale_items:items ~hour:12 ~budget_exceeded:false
             ~supports_ambient:true ~send_message:ok_sender ())
      in
      let r = List.hd results2 in
      Alcotest.(check bool) "not acted" false r.acted;
      Alcotest.check
        (Alcotest.option Alcotest.string)
        "no material change" (Some "no_material_change")
        (Option.map Room_watcher_decision.skip_reason_to_string r.skip_reason);
      (* Only one ledger delivery event (not two) *)
      let events =
        Room_activity_ledger.query ~db ~room_id:"room-1"
          ~event_type:"ambient_delivery" ()
      in
      Alcotest.(check int) "one delivery" 1 (List.length events))

(** Material-change suppression should not be defeated by age alone. *)
let test_material_change_ignores_age_drift () =
  with_db (fun db ->
      let profile = make_profile ~ambient_enabled:true () in
      let first = [ make_stale_item ~id:"age" ~age_seconds:7200.0 () ] in
      let second = [ make_stale_item ~id:"age" ~age_seconds:10800.0 () ] in
      let _results1 =
        Lwt_main.run
          (deliver_ambient_followups ~db ~profile ~room_id:"room-1"
             ~stale_items:first ~hour:12 ~budget_exceeded:false
             ~supports_ambient:true ~send_message:ok_sender ())
      in
      let results2 =
        Lwt_main.run
          (deliver_ambient_followups ~db ~profile ~room_id:"room-1"
             ~stale_items:second ~hour:12 ~budget_exceeded:false
             ~supports_ambient:true ~send_message:ok_sender ())
      in
      let r = List.hd results2 in
      Alcotest.(check bool) "age-only drift suppressed" false r.acted;
      Alcotest.check
        (Alcotest.option Alcotest.string)
        "no material change" (Some "no_material_change")
        (Option.map Room_watcher_decision.skip_reason_to_string r.skip_reason);
      let events =
        Room_activity_ledger.query ~db ~room_id:"room-1"
          ~event_type:"ambient_delivery" ()
      in
      Alcotest.(check int) "one delivery" 1 (List.length events))

(** A same-call batch must enforce the hourly rate limit after each send. *)
let test_batch_rate_limit_enforced_per_item () =
  with_db (fun db ->
      let profile =
        make_profile ~ambient_enabled:true ~ambient_rate_limit_rph:1 ()
      in
      let items = [ make_stale_item ~id:"1" (); make_stale_item ~id:"2" () ] in
      let results =
        Lwt_main.run
          (deliver_ambient_followups ~db ~profile ~room_id:"room-1"
             ~stale_items:items ~hour:12 ~budget_exceeded:false
             ~supports_ambient:true ~send_message:ok_sender ())
      in
      let acted = List.filter (fun r -> r.acted) results in
      Alcotest.(check int) "only one item delivered" 1 (List.length acted);
      let skipped = List.filter (fun r -> not r.acted) results in
      Alcotest.(check int) "one item skipped" 1 (List.length skipped);
      let skip_reason = (List.hd skipped).skip_reason in
      Alcotest.check
        (Alcotest.option Alcotest.string)
        "rate limited" (Some "rate_limited")
        (Option.map Room_watcher_decision.skip_reason_to_string skip_reason))

(** Delivery should prefer each stale item's own thread id. *)
let test_delivery_uses_item_thread_id () =
  with_db (fun db ->
      let profile = make_profile ~ambient_enabled:true () in
      let calls = ref [] in
      let items =
        [
          make_stale_item ~id:"1" ~thread_id:(Some "thread-a") ();
          make_stale_item ~id:"2" ~thread_id:(Some "thread-b") ();
        ]
      in
      let results =
        Lwt_main.run
          (deliver_ambient_followups ~db ~profile ~room_id:"room-1"
             ~thread_id:"fallback-thread" ~stale_items:items ~hour:12
             ~budget_exceeded:false ~supports_ambient:true
             ~send_message:(recording_sender calls) ())
      in
      Alcotest.(check int) "two results" 2 (List.length results);
      Alcotest.(check (list (pair string (option string))))
        "per-item threads"
        [ ("room-1", Some "thread-a"); ("room-1", Some "thread-b") ]
        (List.rev !calls))

(** Multiple items: some allowed, some denied. *)
let test_mixed_results () =
  with_db (fun db ->
      let profile = make_profile ~ambient_enabled:true () in
      let items =
        [
          make_stale_item ~id:"1" ~title:"Task A" ();
          make_stale_item ~id:"2" ~title:"Task B" ();
          make_stale_item ~id:"3" ~title:"Task C" ();
        ]
      in
      let results =
        Lwt_main.run
          (deliver_ambient_followups ~db ~profile ~room_id:"room-1"
             ~stale_items:items ~hour:12 ~budget_exceeded:false
             ~supports_ambient:true ~send_message:ok_sender ())
      in
      Alcotest.(check int) "three results" 3 (List.length results);
      let acted = List.filter (fun r -> r.acted) results in
      Alcotest.(check int) "all acted" 3 (List.length acted);
      (* Verify three ledger events *)
      let events =
        Room_activity_ledger.query ~db ~room_id:"room-1"
          ~event_type:"ambient_delivery" ()
      in
      Alcotest.(check int) "three ledger events" 3 (List.length events))

(** Watcher decisions are recorded for policy denials. *)
let test_policy_denial_records_decision () =
  with_db (fun db ->
      let profile = make_profile ~ambient_enabled:false () in
      let items = [ make_stale_item ~id:"55" () ] in
      let _results =
        Lwt_main.run
          (deliver_ambient_followups ~db ~profile ~room_id:"room-1"
             ~stale_items:items ~hour:12 ~budget_exceeded:false
             ~supports_ambient:true ~send_message:ok_sender ())
      in
      let decisions =
        Room_watcher_decision.query_by_room ~db ~room_id:"room-1" ()
      in
      Alcotest.(check bool) "has decisions" true (List.length decisions > 0);
      let d = List.hd decisions in
      Alcotest.(check string)
        "outcome" "skipped"
        (Room_watcher_decision.outcome_to_string d.outcome))

(** Format followup message truncates long titles. *)
let test_format_truncates_long_title () =
  let long_title = String.make 100 'x' in
  let item = make_stale_item ~id:"1" ~title:long_title ~age_seconds:7200.0 () in
  let msg = format_followup_message item in
  Alcotest.(check bool)
    "message shorter than title" true
    (String.length msg < String.length long_title + 100);
  Alcotest.(check bool)
    "contains title hint" true
    (try
       ignore (Str.search_forward (Str.regexp "[.][.][.]") msg 0);
       true
     with Not_found -> false)

(** Format followup shows hours for long ages, minutes for short. *)
let test_format_age_display () =
  let item_hours = make_stale_item ~id:"1" ~age_seconds:7200.0 () in
  let msg_hours = format_followup_message item_hours in
  Alcotest.(check bool)
    "contains h" true
    (try
       ignore (Str.search_forward (Str.regexp "h") msg_hours 0);
       true
     with Not_found -> false);
  let item_mins = make_stale_item ~id:"2" ~age_seconds:300.0 () in
  let msg_mins = format_followup_message item_mins in
  Alcotest.(check bool)
    "contains m" true
    (try
       ignore (Str.search_forward (Str.regexp "m") msg_mins 0);
       true
     with Not_found -> false)

(** Empty stale items list returns empty results. *)
let test_empty_items () =
  with_db (fun db ->
      let profile = make_profile ~ambient_enabled:true () in
      let results =
        Lwt_main.run
          (deliver_ambient_followups ~db ~profile ~room_id:"room-1"
             ~stale_items:[] ~hour:12 ~budget_exceeded:false
             ~supports_ambient:true ~send_message:ok_sender ())
      in
      Alcotest.(check int) "empty" 0 (List.length results))

let suite =
  [
    Alcotest.test_case "policy blocks when ambient disabled" `Quick
      test_policy_blocks_when_disabled;
    Alcotest.test_case "policy blocks during quiet hours" `Quick
      test_policy_blocks_quiet_hours;
    Alcotest.test_case "policy blocks when rate limited" `Quick
      test_policy_blocks_rate_limited;
    Alcotest.test_case "policy blocks when budget exceeded" `Quick
      test_policy_blocks_budget_exceeded;
    Alcotest.test_case "policy blocks when connector unsupported" `Quick
      test_policy_blocks_connector_unsupported;
    Alcotest.test_case "successful delivery records in ledger" `Quick
      test_successful_delivery;
    Alcotest.test_case "delivery failure recorded in ledger" `Quick
      test_delivery_failure_recorded;
    Alcotest.test_case "material-change suppression" `Quick
      test_material_change_suppression;
    Alcotest.test_case "material-change ignores age drift" `Quick
      test_material_change_ignores_age_drift;
    Alcotest.test_case "batch rate limit enforced per item" `Quick
      test_batch_rate_limit_enforced_per_item;
    Alcotest.test_case "delivery uses item thread id" `Quick
      test_delivery_uses_item_thread_id;
    Alcotest.test_case "mixed results for multiple items" `Quick
      test_mixed_results;
    Alcotest.test_case "policy denial records watcher decision" `Quick
      test_policy_denial_records_decision;
    Alcotest.test_case "format truncates long title" `Quick
      test_format_truncates_long_title;
    Alcotest.test_case "format age display" `Quick test_format_age_display;
    Alcotest.test_case "empty items returns empty" `Quick test_empty_items;
  ]
