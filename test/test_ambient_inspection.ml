(** Tests for [Ambient_inspection] — admin inspection surface for ambient
    watcher state. *)

let with_db f =
  let db = Memory.init ~db_path:":memory:" () in
  Room_watcher_decision.init_schema db;
  Room_activity_ledger.init_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let make_profile ?(ambient_enabled = false)
    ?(ambient_quiet_start = Ambient_policy.default_ambient_quiet_start)
    ?(ambient_quiet_end = Ambient_policy.default_ambient_quiet_end)
    ?(ambient_rate_limit_rph = 0) () =
  {
    Runtime_config_types.id = "test-profile";
    display_name = Some "Test";
    model = "gpt-5.4";
    system_prompt = "";
    max_tool_iterations = 10;
    status = "active";
    allowed_tools = [];
    denied_tools = [];
    ambient_enabled;
    ambient_quiet_start;
    ambient_quiet_end;
    ambient_rate_limit_rph;
  }

let make_cfg ?profile ?(bindings = []) () =
  {
    Runtime_config.default with
    room_profiles = (match profile with Some p -> [ p ] | None -> []);
    room_profile_bindings = bindings;
  }

let record_decision ~db ~room_id ~outcome ?skip_reason ~item_id () =
  let fp =
    Printf.sprintf "fp:%s:%s" item_id
      (Room_watcher_decision.outcome_to_string outcome)
  in
  ignore
    (Room_watcher_decision.record ~db ~room_id ~watcher_type:Stale_task ~outcome
       ?skip_reason ~item_source:"background_task" ~item_id ~fingerprint:fp
       ~metadata:`Null ())

let record_delivery_failure ~db ~room_id ~item_id ~error =
  ignore
    (Room_activity_ledger.append_now ~db ~room_id
       ~event_type:"ambient_delivery_failed" ~actor:"ambient_watcher"
       ~metadata:
         (`Assoc
            [
              ("item_source", `String "background_task");
              ("item_id", `String item_id);
              ("error", `String error);
            ]))

(** Inspection with no profile bound returns defaults. *)
let test_no_profile () =
  with_db (fun db ->
      let cfg = make_cfg () in
      let r = Ambient_inspection.inspect ~db ~cfg ~room_id:"room-1" () in
      Alcotest.(check string) "room_id" "room-1" r.room_id;
      Alcotest.check
        (Alcotest.option Alcotest.string)
        "no profile" None r.profile_name;
      Alcotest.(check bool) "not enabled" false r.ambient_enabled;
      Alcotest.check
        (Alcotest.option (Alcotest.pair Alcotest.int Alcotest.int))
        "no quiet hours" None r.quiet_hours_range;
      Alcotest.(check int) "no rate limit" 0 r.rate_limit_rph;
      Alcotest.(check int) "no decisions" 0 r.decision_summary.total_decisions;
      Alcotest.(check int) "no failures" 0 (List.length r.delivery_failures);
      Alcotest.(check int) "no deliveries" 0 r.deliveries_this_hour)

(** Inspection with bound profile shows profile config. *)
let test_with_profile () =
  with_db (fun db ->
      let profile =
        make_profile ~ambient_enabled:true ~ambient_quiet_start:22
          ~ambient_quiet_end:7 ~ambient_rate_limit_rph:5 ()
      in
      let binding =
        Runtime_config_types.
          { profile_id = "test-profile"; room = "room-1"; active = true }
      in
      let cfg = make_cfg ~profile ~bindings:[ binding ] () in
      let r = Ambient_inspection.inspect ~db ~cfg ~room_id:"room-1" () in
      Alcotest.check
        (Alcotest.option Alcotest.string)
        "profile name" (Some "test-profile") r.profile_name;
      Alcotest.(check bool) "enabled" true r.ambient_enabled;
      Alcotest.check
        (Alcotest.option (Alcotest.pair Alcotest.int Alcotest.int))
        "quiet hours"
        (Some (22, 7))
        r.quiet_hours_range;
      Alcotest.(check int) "rate limit" 5 r.rate_limit_rph)

(** Quiet hours range is None when start == end (disabled). *)
let test_quiet_hours_disabled_when_equal () =
  with_db (fun db ->
      let profile =
        make_profile ~ambient_enabled:true ~ambient_quiet_start:8
          ~ambient_quiet_end:8 ()
      in
      let binding =
        Runtime_config_types.
          { profile_id = "test-profile"; room = "room-1"; active = true }
      in
      let cfg = make_cfg ~profile ~bindings:[ binding ] () in
      let r = Ambient_inspection.inspect ~db ~cfg ~room_id:"room-1" () in
      Alcotest.check
        (Alcotest.option (Alcotest.pair Alcotest.int Alcotest.int))
        "quiet hours disabled" None r.quiet_hours_range)

(** Inspection returns watcher decisions. *)
let test_returns_decisions () =
  with_db (fun db ->
      let cfg = make_cfg () in
      record_decision ~db ~room_id:"room-1" ~outcome:Acted ~item_id:"1" ();
      record_decision ~db ~room_id:"room-1" ~outcome:Skipped
        ~skip_reason:Rate_limited ~item_id:"2" ();
      record_decision ~db ~room_id:"room-1" ~outcome:Skipped
        ~skip_reason:Quiet_hours ~item_id:"3" ();
      let r = Ambient_inspection.inspect ~db ~cfg ~room_id:"room-1" () in
      Alcotest.(check int) "three decisions" 3 (List.length r.recent_decisions);
      Alcotest.(check int) "total" 3 r.decision_summary.total_decisions;
      Alcotest.(check int) "acted" 1 r.decision_summary.acted_count;
      Alcotest.(check int) "skipped" 2 r.decision_summary.skipped_count)

(** Inspection returns delivery failures. *)
let test_returns_delivery_failures () =
  with_db (fun db ->
      let cfg = make_cfg () in
      record_delivery_failure ~db ~room_id:"room-1" ~item_id:"10"
        ~error:"connector offline";
      record_delivery_failure ~db ~room_id:"room-1" ~item_id:"11"
        ~error:"timeout";
      let r = Ambient_inspection.inspect ~db ~cfg ~room_id:"room-1" () in
      Alcotest.(check int) "two failures" 2 (List.length r.delivery_failures))

(** Decisions are scoped by room. *)
let test_scoped_by_room () =
  with_db (fun db ->
      let cfg = make_cfg () in
      record_decision ~db ~room_id:"room-1" ~outcome:Acted ~item_id:"1" ();
      record_decision ~db ~room_id:"room-2" ~outcome:Skipped
        ~skip_reason:Rate_limited ~item_id:"2" ();
      let r1 = Ambient_inspection.inspect ~db ~cfg ~room_id:"room-1" () in
      let r2 = Ambient_inspection.inspect ~db ~cfg ~room_id:"room-2" () in
      Alcotest.(check int)
        "room-1 decisions" 1
        (List.length r1.recent_decisions);
      Alcotest.(check int)
        "room-2 decisions" 1
        (List.length r2.recent_decisions);
      Alcotest.(check int) "room-1 acted" 1 r1.decision_summary.acted_count;
      Alcotest.(check int) "room-2 acted" 0 r2.decision_summary.acted_count)

(** Decision limit is respected. *)
let test_decision_limit () =
  with_db (fun db ->
      let cfg = make_cfg () in
      for i = 1 to 10 do
        record_decision ~db ~room_id:"room-1" ~outcome:Acted
          ~item_id:(string_of_int i) ()
      done;
      let r =
        Ambient_inspection.inspect ~db ~cfg ~room_id:"room-1" ~decision_limit:5
          ()
      in
      Alcotest.(check int) "limited to 5" 5 (List.length r.recent_decisions);
      (* But summary is still over all decisions *)
      Alcotest.(check int)
        "total still 10" 10 r.decision_summary.total_decisions)

(** Format includes key information. *)
let test_format_includes_key_info () =
  with_db (fun db ->
      let profile =
        make_profile ~ambient_enabled:true ~ambient_quiet_start:23
          ~ambient_quiet_end:8 ~ambient_rate_limit_rph:3 ()
      in
      let binding =
        Runtime_config_types.
          { profile_id = "test-profile"; room = "room-1"; active = true }
      in
      let cfg = make_cfg ~profile ~bindings:[ binding ] () in
      record_decision ~db ~room_id:"room-1" ~outcome:Acted ~item_id:"1" ();
      record_decision ~db ~room_id:"room-1" ~outcome:Skipped
        ~skip_reason:Quiet_hours ~item_id:"2" ();
      record_delivery_failure ~db ~room_id:"room-1" ~item_id:"3"
        ~error:"connector offline";
      let r = Ambient_inspection.inspect ~db ~cfg ~room_id:"room-1" () in
      let output = Ambient_inspection.format_inspection r in
      Alcotest.(check bool)
        "contains room id" true
        (try
           ignore (Str.search_forward (Str.regexp "room-1") output 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "contains profile" true
        (try
           ignore (Str.search_forward (Str.regexp "test-profile") output 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "contains acted" true
        (try
           ignore (Str.search_forward (Str.regexp "acted") output 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "contains quiet hours" true
        (try
           ignore (Str.search_forward (Str.regexp "Quiet hours") output 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "contains delivery failures" true
        (try
           ignore (Str.search_forward (Str.regexp "Delivery Failures") output 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "contains error" true
        (try
           ignore (Str.search_forward (Str.regexp "connector offline") output 0);
           true
         with Not_found -> false))

(** Format with empty data shows helpful defaults. *)
let test_format_empty () =
  with_db (fun db ->
      let cfg = make_cfg () in
      let r = Ambient_inspection.inspect ~db ~cfg ~room_id:"room-empty" () in
      let output = Ambient_inspection.format_inspection r in
      Alcotest.(check bool)
        "contains (not bound)" true
        (try
           ignore (Str.search_forward (Str.regexp "(not bound)") output 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "contains (none) for decisions" true
        (try
           ignore (Str.search_forward (Str.regexp "(none)") output 0);
           true
         with Not_found -> false))

let suite =
  [
    Alcotest.test_case "no profile returns defaults" `Quick test_no_profile;
    Alcotest.test_case "with profile shows config" `Quick test_with_profile;
    Alcotest.test_case "quiet hours disabled when start == end" `Quick
      test_quiet_hours_disabled_when_equal;
    Alcotest.test_case "returns watcher decisions" `Quick test_returns_decisions;
    Alcotest.test_case "returns delivery failures" `Quick
      test_returns_delivery_failures;
    Alcotest.test_case "scoped by room" `Quick test_scoped_by_room;
    Alcotest.test_case "decision limit respected" `Quick test_decision_limit;
    Alcotest.test_case "format includes key info" `Quick
      test_format_includes_key_info;
    Alcotest.test_case "format empty shows defaults" `Quick test_format_empty;
  ]
