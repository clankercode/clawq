(* Tests for room profile reconciliation (config-to-DB sync) *)

let test_reconcile_empty_config_noop () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let issues = Memory.sync_config_to_db ~db ~config in
  Alcotest.(check int) "no issues" 0 (List.length issues);
  Alcotest.(check int)
    "no profiles" 0
    (List.length (Memory.list_room_profiles ~db));
  Alcotest.(check int)
    "no bindings" 0
    (List.length (Memory.list_room_profile_bindings_all ~db))

let test_reconcile_syncs_config_profiles () =
  let db = Memory.init ~db_path:":memory:" () in
  let config =
    {
      Runtime_config.default with
      room_profiles =
        [
          {
            id = "work";
            display_name = None;
            model = "openai:gpt-4o";
            system_prompt = "be helpful";
            max_tool_iterations = 5;
            status = "active";
            allowed_tools = [];
            denied_tools = [];
            ambient_enabled = false;
            ambient_quiet_start = 23;
            ambient_quiet_end = 8;
            ambient_rate_limit_rph = 0;
          };
        ];
      room_profile_bindings =
        [ { profile_id = "work"; room = "general"; active = true } ];
    }
  in
  let issues = Memory.sync_config_to_db ~db ~config in
  Alcotest.(check int) "no issues" 0 (List.length issues);
  let profiles = Memory.list_room_profiles ~db in
  Alcotest.(check int) "one profile" 1 (List.length profiles);
  let p = List.nth profiles 0 in
  Alcotest.(check string) "profile name" "work" p.name;
  let bindings = Memory.list_room_profile_bindings_all ~db in
  Alcotest.(check int) "one binding" 1 (List.length bindings);
  let b = List.nth bindings 0 in
  Alcotest.(check string) "room" "general" b.room_id;
  Alcotest.(check int) "profile_id" p.id b.profile_id

let test_reconcile_removes_stale_binding () =
  let db = Memory.init ~db_path:":memory:" () in
  (* Seed: config has profile "work" bound to "general" *)
  let config1 =
    {
      Runtime_config.default with
      room_profiles =
        [
          {
            id = "work";
            display_name = None;
            model = "openai:gpt-4o";
            system_prompt = "";
            max_tool_iterations = 10;
            status = "active";
            allowed_tools = [];
            denied_tools = [];
            ambient_enabled = false;
            ambient_quiet_start = 23;
            ambient_quiet_end = 8;
            ambient_rate_limit_rph = 0;
          };
        ];
      room_profile_bindings =
        [ { profile_id = "work"; room = "general"; active = true } ];
    }
  in
  ignore (Memory.sync_config_to_db ~db ~config:config1);
  Alcotest.(check int)
    "one binding" 1
    (List.length (Memory.list_room_profile_bindings_all ~db));
  (* Config change: now bound to "dev" instead of "general" *)
  let config2 =
    {
      config1 with
      room_profile_bindings =
        [ { profile_id = "work"; room = "dev"; active = true } ];
    }
  in
  let issues = Memory.sync_config_to_db ~db ~config:config2 in
  let stale_count =
    List.filter (fun s -> Test_helpers.string_contains s "stale binding") issues
    |> List.length
  in
  Alcotest.(check int) "stale binding reported" 1 stale_count;
  let bindings = Memory.list_room_profile_bindings_all ~db in
  Alcotest.(check int) "one binding" 1 (List.length bindings);
  let b = List.nth bindings 0 in
  Alcotest.(check string) "now dev" "dev" b.room_id

let test_reconcile_removes_orphan_profile () =
  let db = Memory.init ~db_path:":memory:" () in
  (* Seed: two profiles, one binding *)
  let config1 =
    {
      Runtime_config.default with
      room_profiles =
        [
          {
            id = "p1";
            display_name = None;
            model = "m1";
            system_prompt = "";
            max_tool_iterations = 10;
            status = "active";
            allowed_tools = [];
            denied_tools = [];
            ambient_enabled = false;
            ambient_quiet_start = 23;
            ambient_quiet_end = 8;
            ambient_rate_limit_rph = 0;
          };
          {
            id = "p2";
            display_name = None;
            model = "m2";
            system_prompt = "";
            max_tool_iterations = 10;
            status = "active";
            allowed_tools = [];
            denied_tools = [];
            ambient_enabled = false;
            ambient_quiet_start = 23;
            ambient_quiet_end = 8;
            ambient_rate_limit_rph = 0;
          };
        ];
      room_profile_bindings =
        [ { profile_id = "p1"; room = "r1"; active = true } ];
    }
  in
  ignore (Memory.sync_config_to_db ~db ~config:config1);
  Alcotest.(check int)
    "two profiles" 2
    (List.length (Memory.list_room_profiles ~db));
  (* Config change: remove p2 *)
  let config2 =
    {
      config1 with
      room_profiles =
        [
          {
            id = "p1";
            display_name = None;
            model = "m1";
            system_prompt = "";
            max_tool_iterations = 10;
            status = "active";
            allowed_tools = [];
            denied_tools = [];
            ambient_enabled = false;
            ambient_quiet_start = 23;
            ambient_quiet_end = 8;
            ambient_rate_limit_rph = 0;
          };
        ];
    }
  in
  let issues = Memory.sync_config_to_db ~db ~config:config2 in
  let orphan_count =
    List.filter
      (fun s -> Test_helpers.string_contains s "orphan profile")
      issues
    |> List.length
  in
  Alcotest.(check int) "orphan reported" 1 orphan_count;
  Alcotest.(check int)
    "one profile" 1
    (List.length (Memory.list_room_profiles ~db))

let test_reconcile_removes_stale_binding_and_orphan_profile () =
  let db = Memory.init ~db_path:":memory:" () in
  let config1 =
    {
      Runtime_config.default with
      room_profiles =
        [
          {
            id = "old";
            display_name = None;
            model = "m";
            system_prompt = "";
            max_tool_iterations = 10;
            status = "active";
            allowed_tools = [];
            denied_tools = [];
            ambient_enabled = false;
            ambient_quiet_start = 23;
            ambient_quiet_end = 8;
            ambient_rate_limit_rph = 0;
          };
        ];
      room_profile_bindings =
        [ { profile_id = "old"; room = "chat"; active = true } ];
    }
  in
  ignore (Memory.sync_config_to_db ~db ~config:config1);
  (* Remove everything from config *)
  let config2 = Runtime_config.default in
  let issues = Memory.sync_config_to_db ~db ~config:config2 in
  Alcotest.(check bool)
    "has stale issue" true
    (List.exists
       (fun s -> Test_helpers.string_contains s "stale binding")
       issues);
  Alcotest.(check bool)
    "has orphan issue" true
    (List.exists
       (fun s -> Test_helpers.string_contains s "orphan profile")
       issues);
  Alcotest.(check int)
    "no profiles" 0
    (List.length (Memory.list_room_profiles ~db));
  Alcotest.(check int)
    "no bindings" 0
    (List.length (Memory.list_room_profile_bindings_all ~db))

let test_reconcile_deterministic () =
  let db = Memory.init ~db_path:":memory:" () in
  let config =
    {
      Runtime_config.default with
      room_profiles =
        [
          {
            id = "p1";
            display_name = None;
            model = "m1";
            system_prompt = "";
            max_tool_iterations = 10;
            status = "active";
            allowed_tools = [];
            denied_tools = [];
            ambient_enabled = false;
            ambient_quiet_start = 23;
            ambient_quiet_end = 8;
            ambient_rate_limit_rph = 0;
          };
          {
            id = "p2";
            display_name = None;
            model = "m2";
            system_prompt = "";
            max_tool_iterations = 10;
            status = "active";
            allowed_tools = [];
            denied_tools = [];
            ambient_enabled = false;
            ambient_quiet_start = 23;
            ambient_quiet_end = 8;
            ambient_rate_limit_rph = 0;
          };
        ];
      room_profile_bindings =
        [
          { profile_id = "p1"; room = "r1"; active = true };
          { profile_id = "p2"; room = "r2"; active = true };
        ];
    }
  in
  (* Run sync twice *)
  let issues1 = Memory.sync_config_to_db ~db ~config in
  let issues2 = Memory.sync_config_to_db ~db ~config in
  Alcotest.(check int) "first sync no issues" 0 (List.length issues1);
  Alcotest.(check int) "second sync no issues" 0 (List.length issues2);
  Alcotest.(check int)
    "two profiles" 2
    (List.length (Memory.list_room_profiles ~db));
  Alcotest.(check int)
    "two bindings" 2
    (List.length (Memory.list_room_profile_bindings_all ~db))

let test_reconcile_ignores_inactive_bindings () =
  let db = Memory.init ~db_path:":memory:" () in
  let config =
    {
      Runtime_config.default with
      room_profiles =
        [
          {
            id = "p1";
            display_name = None;
            model = "m1";
            system_prompt = "";
            max_tool_iterations = 10;
            status = "active";
            allowed_tools = [];
            denied_tools = [];
            ambient_enabled = false;
            ambient_quiet_start = 23;
            ambient_quiet_end = 8;
            ambient_rate_limit_rph = 0;
          };
        ];
      room_profile_bindings =
        [ { profile_id = "p1"; room = "r1"; active = false } ];
    }
  in
  let issues = Memory.sync_config_to_db ~db ~config in
  Alcotest.(check int) "no issues" 0 (List.length issues);
  Alcotest.(check int)
    "no bindings" 0
    (List.length (Memory.list_room_profile_bindings_all ~db))

let test_reconcile_no_profile_behavior_unchanged () =
  (* When config has no profiles, init + reconcile = no-op *)
  let db = Memory.init ~db_path:":memory:" () in
  let config = Runtime_config.default in
  let issues = Memory.sync_config_to_db ~db ~config in
  Alcotest.(check int) "no issues" 0 (List.length issues);
  (* Verify get_room_profile_for_room still returns None *)
  let result = Memory.get_room_profile_for_room ~db ~room_id:"any-room" in
  Alcotest.(check bool) "no profile for room" true (result = None)

let test_reconcile_reports_duplicate_room_bindings () =
  (* Two active bindings for the same room: should be reported as duplicate
     and ALL conflicting bindings skipped (fail closed). *)
  let db = Memory.init ~db_path:":memory:" () in
  let config =
    {
      Runtime_config.default with
      room_profiles =
        [
          {
            id = "p1";
            display_name = None;
            model = "m1";
            system_prompt = "";
            max_tool_iterations = 10;
            status = "active";
            allowed_tools = [];
            denied_tools = [];
            ambient_enabled = false;
            ambient_quiet_start = 23;
            ambient_quiet_end = 8;
            ambient_rate_limit_rph = 0;
          };
          {
            id = "p2";
            display_name = None;
            model = "m2";
            system_prompt = "";
            max_tool_iterations = 10;
            status = "active";
            allowed_tools = [];
            denied_tools = [];
            ambient_enabled = false;
            ambient_quiet_start = 23;
            ambient_quiet_end = 8;
            ambient_rate_limit_rph = 0;
          };
        ];
      room_profile_bindings =
        [
          { profile_id = "p1"; room = "shared-room"; active = true };
          { profile_id = "p2"; room = "shared-room"; active = true };
        ];
    }
  in
  let issues = Memory.sync_config_to_db ~db ~config in
  let dup_room_count =
    List.filter
      (fun s -> Test_helpers.string_contains s "duplicate config binding")
      issues
    |> List.length
  in
  Alcotest.(check int) "duplicate room reported" 1 dup_room_count;
  (* Fail closed: no binding exists for the conflicted room *)
  let bindings = Memory.list_room_profile_bindings_all ~db in
  Alcotest.(check int)
    "no bindings for conflicted room" 0 (List.length bindings)

let test_reconcile_reports_duplicate_profile_bindings () =
  (* Two active bindings for the same profile to different rooms: should be
     reported as duplicate and ALL conflicting bindings skipped (fail closed). *)
  let db = Memory.init ~db_path:":memory:" () in
  let config =
    {
      Runtime_config.default with
      room_profiles =
        [
          {
            id = "shared-profile";
            display_name = None;
            model = "m";
            system_prompt = "";
            max_tool_iterations = 10;
            status = "active";
            allowed_tools = [];
            denied_tools = [];
            ambient_enabled = false;
            ambient_quiet_start = 23;
            ambient_quiet_end = 8;
            ambient_rate_limit_rph = 0;
          };
        ];
      room_profile_bindings =
        [
          { profile_id = "shared-profile"; room = "room-a"; active = true };
          { profile_id = "shared-profile"; room = "room-b"; active = true };
        ];
    }
  in
  let issues = Memory.sync_config_to_db ~db ~config in
  let dup_profile_count =
    List.filter
      (fun s -> Test_helpers.string_contains s "duplicate config binding")
      issues
    |> List.length
  in
  Alcotest.(check int) "duplicate profile reported" 1 dup_profile_count;
  (* Fail closed: no binding exists for the conflicted profile *)
  let bindings = Memory.list_room_profile_bindings_all ~db in
  Alcotest.(check int)
    "no bindings for conflicted profile" 0 (List.length bindings)

(* Production-path test: uses reconcile_room_profiles (the same helper
   called from daemon.ml) to verify config-reload reconciliation. *)
let test_reconcile_reload_helper_updates_db () =
  let db = Memory.init ~db_path:":memory:" () in
  (* Initial config: profile "work" bound to "general" *)
  let config1 =
    {
      Runtime_config.default with
      room_profiles =
        [
          {
            id = "work";
            display_name = None;
            model = "openai:gpt-4o";
            system_prompt = "be helpful";
            max_tool_iterations = 5;
            status = "active";
            allowed_tools = [];
            denied_tools = [];
            ambient_enabled = false;
            ambient_quiet_start = 23;
            ambient_quiet_end = 8;
            ambient_rate_limit_rph = 0;
          };
        ];
      room_profile_bindings =
        [ { profile_id = "work"; room = "general"; active = true } ];
    }
  in
  let _issues1 = Memory.reconcile_room_profiles ~db ~config:config1 in
  let bindings1 = Memory.list_room_profile_bindings_all ~db in
  Alcotest.(check int)
    "one binding after initial reconcile" 1 (List.length bindings1);
  Alcotest.(check string)
    "room is general" "general" (List.nth bindings1 0).room_id;
  (* Reload config: change binding to "dev" instead of "general" *)
  let config2 =
    {
      config1 with
      room_profile_bindings =
        [ { profile_id = "work"; room = "dev"; active = true } ];
    }
  in
  let issues2 = Memory.reconcile_room_profiles ~db ~config:config2 in
  (* Stale binding should be reported *)
  let has_stale =
    List.exists
      (fun s -> Test_helpers.string_contains s "stale binding")
      issues2
  in
  Alcotest.(check bool) "stale binding reported" true has_stale;
  (* DB should now reflect the new config *)
  let bindings2 = Memory.list_room_profile_bindings_all ~db in
  Alcotest.(check int) "one binding after reload" 1 (List.length bindings2);
  Alcotest.(check string) "room is now dev" "dev" (List.nth bindings2 0).room_id;
  (* get_room_profile_for_room should return the profile for "dev" *)
  let profile_opt = Memory.get_room_profile_for_room ~db ~room_id:"dev" in
  Alcotest.(check bool) "profile exists for dev" true (profile_opt <> None);
  (* get_room_profile_for_room should NOT return for "general" (stale) *)
  let stale_opt = Memory.get_room_profile_for_room ~db ~room_id:"general" in
  Alcotest.(check bool) "no profile for stale general" true (stale_opt = None)

(* Finding 1: config changes room->profile mapping; mismatch must be reported *)
let test_reconcile_detects_profile_mismatch () =
  let db = Memory.init ~db_path:":memory:" () in
  let config1 =
    {
      Runtime_config.default with
      room_profiles =
        [
          {
            id = "p1";
            display_name = None;
            model = "m1";
            system_prompt = "";
            max_tool_iterations = 10;
            status = "active";
            allowed_tools = [];
            denied_tools = [];
            ambient_enabled = false;
            ambient_quiet_start = 23;
            ambient_quiet_end = 8;
            ambient_rate_limit_rph = 0;
          };
          {
            id = "p2";
            display_name = None;
            model = "m2";
            system_prompt = "";
            max_tool_iterations = 10;
            status = "active";
            allowed_tools = [];
            denied_tools = [];
            ambient_enabled = false;
            ambient_quiet_start = 23;
            ambient_quiet_end = 8;
            ambient_rate_limit_rph = 0;
          };
        ];
      room_profile_bindings =
        [ { profile_id = "p1"; room = "general"; active = true } ];
    }
  in
  ignore (Memory.sync_config_to_db ~db ~config:config1);
  (* Config change: same room, different profile *)
  let config2 =
    {
      config1 with
      room_profile_bindings =
        [ { profile_id = "p2"; room = "general"; active = true } ];
    }
  in
  let issues = Memory.sync_config_to_db ~db ~config:config2 in
  let mismatch_count =
    List.filter
      (fun s ->
        Test_helpers.string_contains s "stale binding"
        && Test_helpers.string_contains s "profile changed")
      issues
    |> List.length
  in
  Alcotest.(check int) "mismatch issue reported" 1 mismatch_count;
  let bindings = Memory.list_room_profile_bindings_all ~db in
  Alcotest.(check int) "one binding" 1 (List.length bindings);
  let p2 = Option.get (Memory.get_room_profile_by_name ~db ~name:"p2") in
  Alcotest.(check int) "profile_id is p2" p2.id (List.nth bindings 0).profile_id

let suite =
  [
    Alcotest.test_case "reconcile empty config noop" `Quick
      test_reconcile_empty_config_noop;
    Alcotest.test_case "reconcile syncs config profiles" `Quick
      test_reconcile_syncs_config_profiles;
    Alcotest.test_case "reconcile removes stale binding" `Quick
      test_reconcile_removes_stale_binding;
    Alcotest.test_case "reconcile removes orphan profile" `Quick
      test_reconcile_removes_orphan_profile;
    Alcotest.test_case "reconcile removes stale and orphan" `Quick
      test_reconcile_removes_stale_binding_and_orphan_profile;
    Alcotest.test_case "reconcile deterministic" `Quick
      test_reconcile_deterministic;
    Alcotest.test_case "reconcile ignores inactive bindings" `Quick
      test_reconcile_ignores_inactive_bindings;
    Alcotest.test_case "reconcile no-profile behavior unchanged" `Quick
      test_reconcile_no_profile_behavior_unchanged;
    Alcotest.test_case "reconcile reports duplicate room bindings" `Quick
      test_reconcile_reports_duplicate_room_bindings;
    Alcotest.test_case "reconcile reports duplicate profile bindings" `Quick
      test_reconcile_reports_duplicate_profile_bindings;
    Alcotest.test_case "reconcile reload helper updates db" `Quick
      test_reconcile_reload_helper_updates_db;
    Alcotest.test_case "reconcile detects profile mismatch" `Quick
      test_reconcile_detects_profile_mismatch;
  ]
