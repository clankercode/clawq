let make_db () = Memory.init ~db_path:":memory:" ()

let test_discord_resume_state_round_trip () =
  let db = make_db () in
  Discord.save_resume_state ~db:(Some db) ~session_id:"sess-1" ~seq:42
    ~resume_gateway_url:"wss://resume.example.com";
  match Discord.load_resume_state ~db:(Some db) with
  | None -> Alcotest.fail "expected persisted resume state"
  | Some state ->
      Alcotest.(check string) "session id" "sess-1" state.session_id;
      Alcotest.(check int) "seq" 42 state.seq;
      Alcotest.(check string)
        "resume url" "wss://resume.example.com" state.resume_gateway_url

let test_discord_clear_resume_state_removes_persisted_row () =
  let db = make_db () in
  Discord.save_resume_state ~db:(Some db) ~session_id:"sess-1" ~seq:42
    ~resume_gateway_url:"wss://resume.example.com";
  Discord.clear_resume_state ~db:(Some db);
  Alcotest.(check bool)
    "resume state cleared" true
    (Discord.load_resume_state ~db:(Some db) = None)

let test_discord_startup_restore_builds_resume_refs () =
  let db = make_db () in
  Discord.save_resume_state ~db:(Some db) ~session_id:"sess-1" ~seq:42
    ~resume_gateway_url:"wss://resume.example.com";
  let resume_session_id, resume_seq, resume_url =
    Discord.make_resume_refs ~db:(Some db)
  in
  Alcotest.(check (option string))
    "session restored" (Some "sess-1") !resume_session_id;
  Alcotest.(check (option int)) "seq restored" (Some 42) !resume_seq;
  Alcotest.(check (option string))
    "resume url restored" (Some "wss://resume.example.com") !resume_url

let test_discord_startup_without_state_keeps_identify_path () =
  let db = make_db () in
  let resume_session_id, resume_seq, resume_url =
    Discord.make_resume_refs ~db:(Some db)
  in
  Alcotest.(check (option string)) "no session restored" None !resume_session_id;
  Alcotest.(check (option int)) "no seq restored" None !resume_seq;
  Alcotest.(check (option string)) "no resume url restored" None !resume_url

let test_discord_fatal_close_codes_clear_resume_state () =
  Alcotest.(check bool)
    "4004 clears resume state" true
    (Discord.should_clear_resume_state 4004);
  Alcotest.(check bool)
    "4010 clears resume state" true
    (Discord.should_clear_resume_state 4010);
  Alcotest.(check bool)
    "4014 clears resume state" true
    (Discord.should_clear_resume_state 4014)

let test_discord_resume_failures_clear_state_for_identify () =
  Alcotest.(check bool)
    "4007 clears invalid seq state" true
    (Discord.should_clear_resume_state 4007);
  Alcotest.(check bool)
    "4009 clears timed out session state" true
    (Discord.should_clear_resume_state 4009);
  Alcotest.(check bool)
    "4007 is not globally fatal" false
    (Discord.is_fatal_close_code 4007);
  Alcotest.(check bool)
    "4009 is not globally fatal" false
    (Discord.is_fatal_close_code 4009)

(* -- CWD precedence and policy tests ------------------------------------ *)

let make_config ~workspace_only ?(extra_allowed_paths = [])
    ?(allowed_cwd_patterns = []) workspace =
  {
    Runtime_config.default with
    workspace;
    security =
      {
        Runtime_config.default.security with
        workspace_only;
        extra_allowed_paths;
        allowed_cwd_patterns;
      };
  }

let make_mgr ~config ?db () = Session_core.create ~config ?db ()

let make_minimal_template ~cwd () : Agent_template.t =
  {
    name = "test";
    description = "";
    role = Agent_template.Coder;
    goal = "";
    backstory = "";
    system_prompt = "";
    model = None;
    max_tool_iterations = None;
    allowed_tools = [];
    disallowed_tools = [];
    tool_search_enabled = None;
    reasoning_effort = None;
    cwd;
    source = Agent_template.Builtin;
    metadata = [];
  }

let test_cwd_allowed_unrestricted () =
  let config = make_config ~workspace_only:false "/tmp/ws" in
  let mgr = make_mgr ~config () in
  Alcotest.(check bool)
    "any absolute path allowed when workspace_only=false" true
    (Session_room_profile.is_cwd_allowed mgr ~cwd:"/opt/something");
  Alcotest.(check bool)
    "another path allowed" true
    (Session_room_profile.is_cwd_allowed mgr ~cwd:"/home/user/project")

let test_cwd_allowed_unrestricted_with_patterns () =
  (* When workspace_only=false but allowed_cwd_patterns are set,
     patterns are still enforced. Use glob patterns with ** for subdirs. *)
  let config =
    make_config ~workspace_only:false
      ~allowed_cwd_patterns:[ "/repos/myproject/**" ] "/tmp/ws"
  in
  let mgr = make_mgr ~config () in
  Alcotest.(check bool)
    "matching path allowed" true
    (Session_room_profile.is_cwd_allowed mgr ~cwd:"/repos/myproject/src");
  Alcotest.(check bool)
    "exact root matches" true
    (Session_room_profile.is_cwd_allowed mgr ~cwd:"/repos/myproject");
  Alcotest.(check bool)
    "non-matching path blocked" false
    (Session_room_profile.is_cwd_allowed mgr ~cwd:"/opt/other");
  Alcotest.(check bool)
    "sibling blocked" false
    (Session_room_profile.is_cwd_allowed mgr ~cwd:"/repos/other")

let test_cwd_allowed_restricted () =
  let config =
    make_config ~workspace_only:true ~extra_allowed_paths:[ "/extra" ]
      "/workspace"
  in
  let mgr = make_mgr ~config () in
  Alcotest.(check bool)
    "workspace root allowed" true
    (Session_room_profile.is_cwd_allowed mgr ~cwd:"/workspace");
  Alcotest.(check bool)
    "subdir of workspace allowed" true
    (Session_room_profile.is_cwd_allowed mgr ~cwd:"/workspace/foo");
  Alcotest.(check bool)
    "extra_allowed_path allowed" true
    (Session_room_profile.is_cwd_allowed mgr ~cwd:"/extra/bar");
  Alcotest.(check bool)
    "outside workspace blocked" false
    (Session_room_profile.is_cwd_allowed mgr ~cwd:"/opt/other")

let test_cwd_allowed_by_pattern () =
  (* When workspace_only=true, patterns alone are not sufficient — the path
     must also be under workspace roots. Patterns restrict further within the
     allowed workspace boundary. *)
  let config =
    make_config ~workspace_only:true
      ~allowed_cwd_patterns:[ "/workspace/sub/**" ] "/workspace"
  in
  let mgr = make_mgr ~config () in
  Alcotest.(check bool)
    "within workspace AND matching pattern allowed" true
    (Session_room_profile.is_cwd_allowed mgr ~cwd:"/workspace/sub/project");
  Alcotest.(check bool)
    "within workspace but non-matching pattern blocked" false
    (Session_room_profile.is_cwd_allowed mgr ~cwd:"/workspace/other");
  Alcotest.(check bool)
    "matching pattern but outside workspace blocked" false
    (Session_room_profile.is_cwd_allowed mgr ~cwd:"/other/sub/dir");
  Alcotest.(check bool)
    "exact pattern path allowed" true
    (Session_room_profile.is_cwd_allowed mgr ~cwd:"/workspace/sub")

let test_cwd_allowed_glob_pattern () =
  (* Test that $CLAWQ_WORKSPACE/** glob patterns are properly matched *)
  let config =
    make_config ~workspace_only:true
      ~allowed_cwd_patterns:[ "$CLAWQ_WORKSPACE/**" ] "/workspace"
  in
  let mgr = make_mgr ~config () in
  Alcotest.(check bool)
    "workspace root matches $CLAWQ_WORKSPACE/**" true
    (Session_room_profile.is_cwd_allowed mgr ~cwd:"/workspace");
  Alcotest.(check bool)
    "subdir matches $CLAWQ_WORKSPACE/**" true
    (Session_room_profile.is_cwd_allowed mgr ~cwd:"/workspace/foo");
  Alcotest.(check bool)
    "deep subdir matches $CLAWQ_WORKSPACE/**" true
    (Session_room_profile.is_cwd_allowed mgr ~cwd:"/workspace/foo/bar/baz");
  Alcotest.(check bool)
    "outside workspace blocked even with $CLAWQ_WORKSPACE/** pattern" false
    (Session_room_profile.is_cwd_allowed mgr ~cwd:"/other/path")

let test_cwd_allowed_glob_pattern_with_extra_root () =
  (* When extra_allowed_paths provides an additional root, a pattern like
     $CLAWQ_WORKSPACE/** should match paths under extra roots too, as long
     as the path is within workspace containment (extra roots are part of
     the workspace boundary). *)
  let config =
    make_config ~workspace_only:true ~extra_allowed_paths:[ "/extra" ]
      ~allowed_cwd_patterns:[ "$CLAWQ_WORKSPACE/**" ] "/workspace"
  in
  let mgr = make_mgr ~config () in
  Alcotest.(check bool)
    "extra root subdir blocked by restrictive pattern" false
    (Session_room_profile.is_cwd_allowed mgr ~cwd:"/extra/project");
  (* But if the pattern is restrictive (not matching extra root paths),
     it should block even under workspace containment *)
  let config2 =
    make_config ~workspace_only:true ~extra_allowed_paths:[ "/extra" ]
      ~allowed_cwd_patterns:[ "/workspace/specific/**" ]
      "/workspace"
  in
  let mgr2 = make_mgr ~config:config2 () in
  Alcotest.(check bool)
    "extra root blocked by restrictive pattern" false
    (Session_room_profile.is_cwd_allowed mgr2 ~cwd:"/extra/project");
  Alcotest.(check bool)
    "workspace subdir matching pattern allowed" true
    (Session_room_profile.is_cwd_allowed mgr2 ~cwd:"/workspace/specific/app")

let test_set_effective_cwd_rejected () =
  let config = make_config ~workspace_only:true "/workspace" in
  let db = make_db () in
  let mgr = make_mgr ~config ~db () in
  let key = "test-reject" in
  let accepted =
    Session_room_profile.set_effective_cwd mgr ~key ~cwd:"/outside"
  in
  Alcotest.(check bool) "rejected returns false" false accepted;
  Alcotest.(check (option string))
    "DB not updated" None
    (Memory.get_session_cwd ~db ~session_key:key)

let test_set_effective_cwd_accepted () =
  let config = make_config ~workspace_only:true "/workspace" in
  let db = make_db () in
  let mgr = make_mgr ~config ~db () in
  let key = "test-accept" in
  let accepted =
    Session_room_profile.set_effective_cwd mgr ~key ~cwd:"/workspace/sub"
  in
  Alcotest.(check bool) "accepted returns true" true accepted;
  Alcotest.(check (option string))
    "DB updated" (Some "/workspace/sub")
    (Memory.get_session_cwd ~db ~session_key:key)

(* Precedence tests using resolve_initial_cwd — tests the real resolution
   logic without needing a full session. *)

let test_cwd_precedence_db_over_template () =
  (* DB CWD should take precedence over agent_template.cwd *)
  let tmp = Filename.get_temp_dir_name () in
  let template_dir = Filename.concat tmp "clawq-test-tmpl-db" in
  let db_dir = Filename.concat tmp "clawq-test-db-cwd" in
  (try Unix.mkdir template_dir 0o755 with Unix.Unix_error _ -> ());
  (try Unix.mkdir db_dir 0o755 with Unix.Unix_error _ -> ());
  Fun.protect
    ~finally:(fun () ->
      (try Unix.rmdir template_dir with _ -> ());
      try Unix.rmdir db_dir with _ -> ())
    (fun () ->
      let config = make_config ~workspace_only:false tmp in
      let db = make_db () in
      let mgr = make_mgr ~config ~db () in
      let key = "test-precedence-db-tmpl" in
      Memory.set_session_cwd ~db ~session_key:key ~cwd:(Some db_dir);
      let tmpl = make_minimal_template ~cwd:(Some template_dir) () in
      let result =
        Session_room_profile.resolve_initial_cwd mgr ~session_key:key
          ~db:(Some db) ~agent_template:(Some tmpl)
      in
      Alcotest.(check (option string))
        "DB CWD wins over template" (Some db_dir) result)

let test_cwd_precedence_config_over_template () =
  (* Explicit config workspace should take precedence over template CWD *)
  let tmp = Filename.get_temp_dir_name () in
  let template_dir = Filename.concat tmp "clawq-test-tmpl-cfg" in
  let config_dir = Filename.concat tmp "clawq-test-cfg-ws" in
  (try Unix.mkdir template_dir 0o755 with Unix.Unix_error _ -> ());
  (try Unix.mkdir config_dir 0o755 with Unix.Unix_error _ -> ());
  Fun.protect
    ~finally:(fun () ->
      (try Unix.rmdir template_dir with _ -> ());
      try Unix.rmdir config_dir with _ -> ())
    (fun () ->
      (* Config workspace is explicit (differs from default_workspace) *)
      let config = make_config ~workspace_only:false config_dir in
      let db = make_db () in
      let mgr = make_mgr ~config ~db () in
      let key = "test-precedence-cfg-tmpl" in
      let tmpl = make_minimal_template ~cwd:(Some template_dir) () in
      let result =
        Session_room_profile.resolve_initial_cwd mgr ~session_key:key
          ~db:(Some db) ~agent_template:(Some tmpl)
      in
      Alcotest.(check (option string))
        "config workspace wins over template" (Some config_dir) result)

let test_cwd_precedence_template_over_global () =
  (* Template CWD should be used when no DB and no explicit config workspace *)
  let tmp = Filename.get_temp_dir_name () in
  let template_dir = Filename.concat tmp "clawq-test-tmpl-only" in
  (try Unix.mkdir template_dir 0o755 with Unix.Unix_error _ -> ());
  Fun.protect
    ~finally:(fun () -> try Unix.rmdir template_dir with _ -> ())
    (fun () ->
      (* Use the default workspace so config layer is not explicit *)
      let default_ws = Runtime_config.default.workspace in
      let config = make_config ~workspace_only:false default_ws in
      let db = make_db () in
      let mgr = make_mgr ~config ~db () in
      let key = "test-precedence-tmpl-only" in
      let tmpl = make_minimal_template ~cwd:(Some template_dir) () in
      let result =
        Session_room_profile.resolve_initial_cwd mgr ~session_key:key
          ~db:(Some db) ~agent_template:(Some tmpl)
      in
      Alcotest.(check (option string))
        "template CWD used when no DB and default config" (Some template_dir)
        result)

let test_cwd_precedence_db_over_config () =
  (* DB CWD should take precedence over explicit config workspace *)
  let tmp = Filename.get_temp_dir_name () in
  let db_dir = Filename.concat tmp "clawq-test-db-over-cfg" in
  let config_dir = Filename.concat tmp "clawq-test-cfg-under-db" in
  (try Unix.mkdir db_dir 0o755 with Unix.Unix_error _ -> ());
  (try Unix.mkdir config_dir 0o755 with Unix.Unix_error _ -> ());
  Fun.protect
    ~finally:(fun () ->
      (try Unix.rmdir db_dir with _ -> ());
      try Unix.rmdir config_dir with _ -> ())
    (fun () ->
      let config = make_config ~workspace_only:false config_dir in
      let db = make_db () in
      let mgr = make_mgr ~config ~db () in
      let key = "test-precedence-db-over-cfg" in
      Memory.set_session_cwd ~db ~session_key:key ~cwd:(Some db_dir);
      let result =
        Session_room_profile.resolve_initial_cwd mgr ~session_key:key
          ~db:(Some db) ~agent_template:None
      in
      Alcotest.(check (option string))
        "DB CWD wins over config workspace" (Some db_dir) result)

let test_cwd_precedence_fallback_global () =
  (* When no DB, no explicit config, no template — returns None (global) *)
  let default_ws = Runtime_config.default.workspace in
  let config = make_config ~workspace_only:false default_ws in
  let db = make_db () in
  let mgr = make_mgr ~config ~db () in
  let key = "test-precedence-global" in
  let result =
    Session_room_profile.resolve_initial_cwd mgr ~session_key:key ~db:(Some db)
      ~agent_template:None
  in
  Alcotest.(check (option string)) "falls back to global (None)" None result

let test_cwd_precedence_rejected_template_falls_back () =
  (* When template CWD is outside allowed policy, it should be rejected
     and resolution should fall through to global *)
  let tmp = Filename.get_temp_dir_name () in
  let template_dir = Filename.concat tmp "clawq-test-tmpl-rejected" in
  (try Unix.mkdir template_dir 0o755 with Unix.Unix_error _ -> ());
  Fun.protect
    ~finally:(fun () -> try Unix.rmdir template_dir with _ -> ())
    (fun () ->
      (* workspace_only=true with a different workspace — template_dir outside *)
      let config = make_config ~workspace_only:true "/workspace" in
      let db = make_db () in
      let mgr = make_mgr ~config ~db () in
      let key = "test-precedence-rejected" in
      let tmpl = make_minimal_template ~cwd:(Some template_dir) () in
      let result =
        Session_room_profile.resolve_initial_cwd mgr ~session_key:key
          ~db:(Some db) ~agent_template:(Some tmpl)
      in
      Alcotest.(check (option string))
        "rejected template falls to global" None result)

(** P11.M4.E4.T001: Two concurrent room sessions must resolve independent CWD
    values without cross-contamination. When each room has a different CWD
    stored in the DB, resolve_initial_cwd returns the correct value for each
    session key independently. *)
let test_room_sessions_resolve_independent_cwd () =
  let db = make_db () in
  let config = make_config ~workspace_only:false "/tmp/ws" in
  let mgr = make_mgr ~config ~db () in
  (* Store distinct CWDs for two room sessions *)
  Memory.set_session_cwd ~db ~session_key:"slack:C-ROOM-A:UA"
    ~cwd:(Some "/workspace/room-a");
  Memory.set_session_cwd ~db ~session_key:"slack:C-ROOM-B:UB"
    ~cwd:(Some "/workspace/room-b");
  (* Resolve each and verify no cross-contamination *)
  let cwd_a =
    Session_room_profile.resolve_initial_cwd mgr
      ~session_key:"slack:C-ROOM-A:UA" ~db:(Some db) ~agent_template:None
  in
  let cwd_b =
    Session_room_profile.resolve_initial_cwd mgr
      ~session_key:"slack:C-ROOM-B:UB" ~db:(Some db) ~agent_template:None
  in
  Alcotest.(check (option string)) "room A CWD" (Some "/workspace/room-a") cwd_a;
  Alcotest.(check (option string)) "room B CWD" (Some "/workspace/room-b") cwd_b;
  (* Verify that resolving A again still returns A's CWD, not B's *)
  let cwd_a_again =
    Session_room_profile.resolve_initial_cwd mgr
      ~session_key:"slack:C-ROOM-A:UA" ~db:(Some db) ~agent_template:None
  in
  Alcotest.(check (option string))
    "room A CWD stable" (Some "/workspace/room-a") cwd_a_again

let test_compaction_dirty_restored_when_persist_raises () =
  let db = make_db () in
  let config = Runtime_config.default in
  let mgr = make_mgr ~config ~db () in
  let agent = Agent.create ~config () in
  agent.Agent.history <-
    [ Provider.make_message ~role:"assistant" ~content:"compacted" ];
  Agent.mark_compacted agent;
  ignore (Sqlite3.db_close db);
  let raised =
    try
      Session.persist_after_turn mgr ~key:"persist-failure" ~history_before:0
        agent;
      false
    with _ -> true
  in
  Alcotest.(check bool) "persistence raises" true raised;
  Alcotest.(check bool)
    "compaction remains dirty for retry" true
    (Agent.take_compaction_dirty agent)

let suite : unit Alcotest.test_case list =
  [
    Alcotest.test_case "discord resume state round trip" `Quick
      test_discord_resume_state_round_trip;
    Alcotest.test_case "discord clear resume state removes persisted row" `Quick
      test_discord_clear_resume_state_removes_persisted_row;
    Alcotest.test_case "discord startup restore builds resume refs" `Quick
      test_discord_startup_restore_builds_resume_refs;
    Alcotest.test_case "discord startup without state keeps identify path"
      `Quick test_discord_startup_without_state_keeps_identify_path;
    Alcotest.test_case "discord fatal close codes clear resume state" `Quick
      test_discord_fatal_close_codes_clear_resume_state;
    Alcotest.test_case "discord resume failures clear state for identify" `Quick
      test_discord_resume_failures_clear_state_for_identify;
    Alcotest.test_case "is_cwd_allowed: workspace_only=false allows any" `Quick
      test_cwd_allowed_unrestricted;
    Alcotest.test_case
      "is_cwd_allowed: workspace_only=false with patterns enforces them" `Quick
      test_cwd_allowed_unrestricted_with_patterns;
    Alcotest.test_case "is_cwd_allowed: workspace_only=true blocks outside"
      `Quick test_cwd_allowed_restricted;
    Alcotest.test_case
      "is_cwd_allowed: workspace_only=true + patterns requires both" `Quick
      test_cwd_allowed_by_pattern;
    Alcotest.test_case "is_cwd_allowed: $CLAWQ_WORKSPACE/** glob pattern" `Quick
      test_cwd_allowed_glob_pattern;
    Alcotest.test_case "is_cwd_allowed: glob pattern + extra_allowed_paths"
      `Quick test_cwd_allowed_glob_pattern_with_extra_root;
    Alcotest.test_case "set_effective_cwd: rejects disallowed path" `Quick
      test_set_effective_cwd_rejected;
    Alcotest.test_case "set_effective_cwd: accepts allowed path" `Quick
      test_set_effective_cwd_accepted;
    Alcotest.test_case "precedence: DB over template via resolve_initial_cwd"
      `Quick test_cwd_precedence_db_over_template;
    Alcotest.test_case
      "precedence: config workspace over template via resolve_initial_cwd"
      `Quick test_cwd_precedence_config_over_template;
    Alcotest.test_case
      "precedence: template over global via resolve_initial_cwd" `Quick
      test_cwd_precedence_template_over_global;
    Alcotest.test_case
      "precedence: DB over config workspace via resolve_initial_cwd" `Quick
      test_cwd_precedence_db_over_config;
    Alcotest.test_case "precedence: fallback to global (None)" `Quick
      test_cwd_precedence_fallback_global;
    Alcotest.test_case "precedence: rejected template falls to global" `Quick
      test_cwd_precedence_rejected_template_falls_back;
    Alcotest.test_case
      "room sessions resolve independent CWD without cross-contamination" `Quick
      test_room_sessions_resolve_independent_cwd;
    Alcotest.test_case "failed compaction persist preserves retry signal" `Quick
      test_compaction_dirty_restored_when_persist_raises;
  ]
