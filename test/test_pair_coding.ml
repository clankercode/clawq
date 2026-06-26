(* Tests for pair coding types, state, tools, prompts, and reports. *)

let test_db () =
  let db = Sqlite3.db_open ":memory:" in
  ignore (Sqlite3.exec db "PRAGMA busy_timeout = 5000");
  Pair_coding_state.init_schema db;
  db

(* === Type tests === *)

let test_role_roundtrip () =
  List.iter
    (fun role ->
      let s = Pair_coding_types.role_to_string role in
      let r = Pair_coding_types.role_of_string s in
      Alcotest.(check (option string))
        "role roundtrip"
        (Some (Pair_coding_types.role_to_string role))
        (Option.map Pair_coding_types.role_to_string r))
    [ Pair_coding_types.Coordinator; Coder; Observer ]

let test_phase_roundtrip () =
  List.iter
    (fun phase ->
      let s = Pair_coding_types.phase_to_string phase in
      let p = Pair_coding_types.phase_of_string s in
      Alcotest.(check (option string))
        "phase roundtrip"
        (Some (Pair_coding_types.phase_to_string phase))
        (Option.map Pair_coding_types.phase_to_string p))
    [ Pair_coding_types.Coding; Review; Iteration; Completion; Done ]

let test_severity_roundtrip () =
  List.iter
    (fun sev ->
      let s = Pair_coding_types.severity_to_string sev in
      Alcotest.(check (option string))
        "severity roundtrip" (Some s)
        (Option.map Pair_coding_types.severity_to_string
           (Pair_coding_types.severity_of_string s)))
    [ Pair_coding_types.Critical; High; Medium; Low ]

let test_category_roundtrip () =
  List.iter
    (fun cat ->
      let s = Pair_coding_types.category_to_string cat in
      Alcotest.(check (option string))
        "category roundtrip" (Some s)
        (Option.map Pair_coding_types.category_to_string
           (Pair_coding_types.category_of_string s)))
    [
      Pair_coding_types.Bug;
      Style;
      Architecture;
      Optimization;
      Question;
      Suggestion;
      Security;
      Other;
    ]

let test_transition_roundtrip () =
  List.iter
    (fun tr ->
      let s = Pair_coding_types.transition_to_string tr in
      Alcotest.(check (option string))
        "transition roundtrip" (Some s)
        (Option.map Pair_coding_types.transition_to_string
           (Pair_coding_types.transition_of_string s)))
    [
      Pair_coding_types.Start_review;
      Start_iteration;
      Complete;
      Finalize;
      Timeout;
      Abort;
    ]

let test_role_of_string_aliases () =
  Alcotest.(check (option string))
    "coord alias" (Some "coordinator")
    (Option.map Pair_coding_types.role_to_string
       (Pair_coding_types.role_of_string "coord"));
  Alcotest.(check (option string))
    "obsrv alias" (Some "observer")
    (Option.map Pair_coding_types.role_to_string
       (Pair_coding_types.role_of_string "obsrv"));
  Alcotest.(check bool)
    "invalid" true
    (Option.is_none (Pair_coding_types.role_of_string "invalid"))

let test_initial_state () =
  let s = Pair_coding_types.initial_state ~max_review_rounds:3 in
  Alcotest.(check string)
    "phase" "coding"
    (Pair_coding_types.phase_to_string s.phase);
  Alcotest.(check int) "review_round" 0 s.review_round;
  Alcotest.(check int) "max_review_rounds" 3 s.max_review_rounds;
  Alcotest.(check bool)
    "both_approved" false
    (Pair_coding_types.both_approved s);
  Alcotest.(check bool)
    "has_blocking_notes" false
    (Pair_coding_types.has_blocking_notes s)

let test_transition_coding_to_review () =
  let s = Pair_coding_types.initial_state ~max_review_rounds:3 in
  match Pair_coding_types.transition s Start_review with
  | Ok s' ->
      Alcotest.(check string)
        "phase" "review"
        (Pair_coding_types.phase_to_string s'.phase);
      Alcotest.(check int) "review_round" 1 s'.review_round
  | Error msg -> Alcotest.fail msg

let test_transition_review_to_iteration () =
  let s = Pair_coding_types.initial_state ~max_review_rounds:3 in
  let s = Result.get_ok (Pair_coding_types.transition s Start_review) in
  match Pair_coding_types.transition s Start_iteration with
  | Ok s' ->
      Alcotest.(check string)
        "phase" "iteration"
        (Pair_coding_types.phase_to_string s'.phase)
  | Error msg -> Alcotest.fail msg

let test_transition_iteration_to_review () =
  let s = Pair_coding_types.initial_state ~max_review_rounds:3 in
  let s = Result.get_ok (Pair_coding_types.transition s Start_review) in
  let s = Result.get_ok (Pair_coding_types.transition s Start_iteration) in
  match Pair_coding_types.transition s Start_review with
  | Ok s' ->
      Alcotest.(check string)
        "phase" "review"
        (Pair_coding_types.phase_to_string s'.phase);
      Alcotest.(check int) "review_round" 2 s'.review_round
  | Error msg -> Alcotest.fail msg

let test_transition_complete_requires_approval () =
  let s = Pair_coding_types.initial_state ~max_review_rounds:3 in
  let s = Result.get_ok (Pair_coding_types.transition s Start_review) in
  match Pair_coding_types.transition s Complete with
  | Ok _ -> Alcotest.fail "should require approval"
  | Error _ -> ()

let test_transition_complete_with_both_approved () =
  let s = Pair_coding_types.initial_state ~max_review_rounds:3 in
  let s = Result.get_ok (Pair_coding_types.transition s Start_review) in
  let s =
    {
      s with
      coder_approval =
        Some { approved = true; comment = "lgtm"; timestamp_ms = 0 };
      observer_approval =
        Some { approved = true; comment = "ok"; timestamp_ms = 0 };
    }
  in
  match Pair_coding_types.transition s Complete with
  | Ok s' ->
      Alcotest.(check string)
        "phase" "completion"
        (Pair_coding_types.phase_to_string s'.phase)
  | Error msg -> Alcotest.fail msg

let test_transition_complete_at_max_rounds () =
  let s =
    {
      (Pair_coding_types.initial_state ~max_review_rounds:2) with
      phase = Review;
      review_round = 2;
    }
  in
  match Pair_coding_types.transition s Complete with
  | Ok s' ->
      Alcotest.(check string)
        "phase" "completion"
        (Pair_coding_types.phase_to_string s'.phase)
  | Error msg -> Alcotest.fail msg

let test_transition_finalize () =
  let s =
    {
      (Pair_coding_types.initial_state ~max_review_rounds:3) with
      phase = Completion;
    }
  in
  match Pair_coding_types.transition s Finalize with
  | Ok s' ->
      Alcotest.(check string)
        "phase" "done"
        (Pair_coding_types.phase_to_string s'.phase)
  | Error msg -> Alcotest.fail msg

let test_transition_done_rejects () =
  let s =
    { (Pair_coding_types.initial_state ~max_review_rounds:3) with phase = Done }
  in
  match Pair_coding_types.transition s Start_review with
  | Ok _ -> Alcotest.fail "done should reject"
  | Error msg ->
      Alcotest.(check bool) "error message" true (String.length msg > 0)

let test_transition_invalid () =
  let s = Pair_coding_types.initial_state ~max_review_rounds:3 in
  match Pair_coding_types.transition s Complete with
  | Ok _ -> Alcotest.fail "coding->complete should be invalid"
  | Error _ -> ()

let test_transition_timeout_from_any () =
  List.iter
    (fun phase ->
      let s =
        { (Pair_coding_types.initial_state ~max_review_rounds:3) with phase }
      in
      match Pair_coding_types.transition s Timeout with
      | Ok s' ->
          Alcotest.(check string)
            "timeout -> done" "done"
            (Pair_coding_types.phase_to_string s'.phase)
      | Error msg -> Alcotest.fail msg)
    [ Pair_coding_types.Coding; Review; Iteration; Completion ]

let test_transition_abort_from_any () =
  List.iter
    (fun phase ->
      let s =
        { (Pair_coding_types.initial_state ~max_review_rounds:3) with phase }
      in
      match Pair_coding_types.transition s Abort with
      | Ok s' ->
          Alcotest.(check string)
            "abort -> done" "done"
            (Pair_coding_types.phase_to_string s'.phase)
      | Error _ -> Alcotest.fail "abort should always work")
    [ Coding; Review; Iteration; Completion ]

let test_both_approved () =
  let s = Pair_coding_types.initial_state ~max_review_rounds:3 in
  Alcotest.(check bool) "no approvals" false (Pair_coding_types.both_approved s);
  let s =
    {
      s with
      coder_approval = Some { approved = true; comment = ""; timestamp_ms = 0 };
    }
  in
  Alcotest.(check bool) "only coder" false (Pair_coding_types.both_approved s);
  let s =
    {
      s with
      observer_approval =
        Some { approved = true; comment = ""; timestamp_ms = 0 };
    }
  in
  Alcotest.(check bool) "both" true (Pair_coding_types.both_approved s);
  let s =
    {
      s with
      observer_approval =
        Some { approved = false; comment = ""; timestamp_ms = 0 };
    }
  in
  Alcotest.(check bool)
    "observer rejected" false
    (Pair_coding_types.both_approved s)

let test_has_blocking_notes () =
  let s = Pair_coding_types.initial_state ~max_review_rounds:3 in
  Alcotest.(check bool)
    "no notes" false
    (Pair_coding_types.has_blocking_notes s);
  let s =
    {
      s with
      notes =
        [
          {
            id = 1;
            description = "low note";
            category = None;
            severity = Low;
            file = None;
            line = None;
            resolved = false;
            created_at_ms = 0;
          };
        ];
    }
  in
  Alcotest.(check bool)
    "only low" false
    (Pair_coding_types.has_blocking_notes s);
  let s =
    {
      s with
      notes =
        [
          {
            id = 1;
            description = "critical note";
            category = None;
            severity = Critical;
            file = None;
            line = None;
            resolved = false;
            created_at_ms = 0;
          };
        ];
    }
  in
  Alcotest.(check bool)
    "critical unresolved" true
    (Pair_coding_types.has_blocking_notes s);
  let s =
    {
      s with
      notes =
        [
          {
            id = 1;
            description = "critical resolved";
            category = None;
            severity = Critical;
            file = None;
            line = None;
            resolved = true;
            created_at_ms = 0;
          };
        ];
    }
  in
  Alcotest.(check bool)
    "critical resolved" false
    (Pair_coding_types.has_blocking_notes s)

let test_role_key_suffix () =
  Alcotest.(check string)
    "coord" "coord"
    (Pair_coding_types.role_key_suffix Coordinator);
  Alcotest.(check string)
    "coder" "coder"
    (Pair_coding_types.role_key_suffix Coder);
  Alcotest.(check string)
    "obsrv" "obsrv"
    (Pair_coding_types.role_key_suffix Observer)

(* === DB state tests === *)

let test_create_session () =
  let db = test_db () in
  let config : Pair_coding_state.pair_config =
    {
      task_description = "Implement foo";
      max_review_rounds = 3;
      interrupt_mode = Asap;
      workspace = None;
      worktree_path = None;
      branch_name = None;
      auto_swap_roles = false;
      coder_model = None;
      observer_model = None;
      coordinator_model = None;
    }
  in
  let id = Pair_coding_state.create_session ~db ~config in
  Alcotest.(check bool) "id length = 16" true (String.length id = 16);
  let session = Pair_coding_state.load_session ~db ~id in
  Alcotest.(check bool) "session exists" true (Option.is_some session);
  let s = Option.get session in
  Alcotest.(check string) "task" "Implement foo" s.config.task_description;
  Alcotest.(check string)
    "phase" "coding"
    (Pair_coding_types.phase_to_string s.phase);
  Alcotest.(check bool) "active" true s.active;
  Alcotest.(check int) "review_round" 0 s.review_round

let test_id_length () =
  let db = test_db () in
  let config : Pair_coding_state.pair_config =
    {
      task_description = "test";
      max_review_rounds = 3;
      interrupt_mode = Asap;
      workspace = None;
      worktree_path = None;
      branch_name = None;
      auto_swap_roles = false;
      coder_model = None;
      observer_model = None;
      coordinator_model = None;
    }
  in
  for _ = 1 to 50 do
    let id = Pair_coding_state.create_session ~db ~config in
    Alcotest.(check bool) "id = 16 chars" true (String.length id = 16)
  done

let test_session_keys () =
  let db = test_db () in
  let config : Pair_coding_state.pair_config =
    {
      task_description = "test keys";
      max_review_rounds = 3;
      interrupt_mode = Asap;
      workspace = None;
      worktree_path = None;
      branch_name = None;
      auto_swap_roles = false;
      coder_model = None;
      observer_model = None;
      coordinator_model = None;
    }
  in
  let id = Pair_coding_state.create_session ~db ~config in
  let s = Option.get (Pair_coding_state.load_session ~db ~id) in
  Alcotest.(check string)
    "coder_key"
    (Printf.sprintf "pair:%s:coder" id)
    s.coder_key;
  Alcotest.(check string)
    "observer_key"
    (Printf.sprintf "pair:%s:obsrv" id)
    s.observer_key;
  Alcotest.(check string)
    "coordinator_key"
    (Printf.sprintf "pair:%s:coord" id)
    s.coordinator_key;
  (* Verify parse_channel_from_key works *)
  (match Restart_notify.parse_channel_from_key s.coder_key with
  | Some ("pair", _) -> ()
  | _ -> Alcotest.fail "coder_key should parse as pair channel");
  match Restart_notify.parse_channel_from_key s.observer_key with
  | Some ("pair", _) -> ()
  | _ -> Alcotest.fail "observer_key should parse as pair channel"

let test_update_phase () =
  let db = test_db () in
  let config : Pair_coding_state.pair_config =
    {
      task_description = "test phase";
      max_review_rounds = 3;
      interrupt_mode = Asap;
      workspace = None;
      worktree_path = None;
      branch_name = None;
      auto_swap_roles = false;
      coder_model = None;
      observer_model = None;
      coordinator_model = None;
    }
  in
  let id = Pair_coding_state.create_session ~db ~config in
  Pair_coding_state.update_phase ~db ~id Review;
  let s = Option.get (Pair_coding_state.load_session ~db ~id) in
  Alcotest.(check string)
    "phase updated" "review"
    (Pair_coding_types.phase_to_string s.phase)

let test_add_and_load_notes () =
  let db = test_db () in
  let config : Pair_coding_state.pair_config =
    {
      task_description = "test notes";
      max_review_rounds = 3;
      interrupt_mode = Asap;
      workspace = None;
      worktree_path = None;
      branch_name = None;
      auto_swap_roles = false;
      coder_model = None;
      observer_model = None;
      coordinator_model = None;
    }
  in
  let id = Pair_coding_state.create_session ~db ~config in
  let n1 =
    Pair_coding_state.add_note ~db ~session_id:id ~description:"Bug found"
      ~category:Bug ~severity:High ~file:"src/foo.ml" ~line:42 ()
  in
  let n2 =
    Pair_coding_state.add_note ~db ~session_id:id ~description:"Style issue"
      ~severity:Low ()
  in
  let notes = Pair_coding_state.load_notes ~db ~session_id:id in
  Alcotest.(check int) "two notes" 2 (List.length notes);
  let note1 = List.hd notes in
  Alcotest.(check int) "note1 id" n1 note1.id;
  Alcotest.(check string) "note1 desc" "Bug found" note1.description;
  Alcotest.(check string)
    "note1 severity" "high"
    (Pair_coding_types.severity_to_string note1.severity);
  Alcotest.(check (option string)) "note1 file" (Some "src/foo.ml") note1.file;
  Alcotest.(check (option int)) "note1 line" (Some 42) note1.line;
  Alcotest.(check bool) "note1 not resolved" false note1.resolved;
  let note2 = List.nth notes 1 in
  Alcotest.(check int) "note2 id" n2 note2.id;
  Alcotest.(check (option string)) "note2 no file" None note2.file

let test_resolve_note () =
  let db = test_db () in
  let config : Pair_coding_state.pair_config =
    {
      task_description = "test resolve";
      max_review_rounds = 3;
      interrupt_mode = Asap;
      workspace = None;
      worktree_path = None;
      branch_name = None;
      auto_swap_roles = false;
      coder_model = None;
      observer_model = None;
      coordinator_model = None;
    }
  in
  let id = Pair_coding_state.create_session ~db ~config in
  let nid =
    Pair_coding_state.add_note ~db ~session_id:id ~description:"fix me"
      ~severity:Medium ()
  in
  Pair_coding_state.resolve_note ~db ~note_id:nid;
  let notes = Pair_coding_state.load_notes ~db ~session_id:id in
  let note = List.hd notes in
  Alcotest.(check bool) "resolved" true note.resolved

let test_set_approval_atomic () =
  let db = test_db () in
  let config : Pair_coding_state.pair_config =
    {
      task_description = "test approval";
      max_review_rounds = 3;
      interrupt_mode = Asap;
      workspace = None;
      worktree_path = None;
      branch_name = None;
      auto_swap_roles = false;
      coder_model = None;
      observer_model = None;
      coordinator_model = None;
    }
  in
  let id = Pair_coding_state.create_session ~db ~config in
  let both1 =
    Pair_coding_state.set_approval ~db ~id ~role:Coder ~approved:true
      ~comment:"lgtm"
  in
  Alcotest.(check bool) "first approval not both" false both1;
  let both2 =
    Pair_coding_state.set_approval ~db ~id ~role:Observer ~approved:true
      ~comment:"ok"
  in
  Alcotest.(check bool) "second approval is both" true both2;
  let s = Option.get (Pair_coding_state.load_session ~db ~id) in
  Alcotest.(check bool) "coder_approved" true s.coder_approved;
  Alcotest.(check bool) "observer_approved" true s.observer_approved;
  Alcotest.(check string) "coder_comment" "lgtm" s.coder_comment;
  Alcotest.(check string) "observer_comment" "ok" s.observer_comment

let test_finish_session () =
  let db = test_db () in
  let config : Pair_coding_state.pair_config =
    {
      task_description = "test finish";
      max_review_rounds = 3;
      interrupt_mode = Asap;
      workspace = None;
      worktree_path = None;
      branch_name = None;
      auto_swap_roles = false;
      coder_model = None;
      observer_model = None;
      coordinator_model = None;
    }
  in
  let id = Pair_coding_state.create_session ~db ~config in
  Pair_coding_state.finish_session ~db ~id;
  let s = Option.get (Pair_coding_state.load_session ~db ~id) in
  Alcotest.(check bool) "not active" false s.active;
  Alcotest.(check bool) "has finished_at" true (Option.is_some s.finished_at)

let test_list_sessions () =
  let db = test_db () in
  let config : Pair_coding_state.pair_config =
    {
      task_description = "test list";
      max_review_rounds = 3;
      interrupt_mode = Asap;
      workspace = None;
      worktree_path = None;
      branch_name = None;
      auto_swap_roles = false;
      coder_model = None;
      observer_model = None;
      coordinator_model = None;
    }
  in
  let id1 = Pair_coding_state.create_session ~db ~config in
  let _id2 = Pair_coding_state.create_session ~db ~config in
  Pair_coding_state.finish_session ~db ~id:id1;
  let all = Pair_coding_state.list_sessions ~db () in
  Alcotest.(check int) "two sessions" 2 (List.length all);
  let active = Pair_coding_state.list_sessions ~db ~active_only:true () in
  Alcotest.(check int) "one active" 1 (List.length active)

let test_clear_approvals () =
  let db = test_db () in
  let config : Pair_coding_state.pair_config =
    {
      task_description = "test clear";
      max_review_rounds = 3;
      interrupt_mode = Asap;
      workspace = None;
      worktree_path = None;
      branch_name = None;
      auto_swap_roles = false;
      coder_model = None;
      observer_model = None;
      coordinator_model = None;
    }
  in
  let id = Pair_coding_state.create_session ~db ~config in
  ignore
    (Pair_coding_state.set_approval ~db ~id ~role:Coder ~approved:true
       ~comment:"a");
  Pair_coding_state.clear_approvals ~db ~id;
  let s = Option.get (Pair_coding_state.load_session ~db ~id) in
  Alcotest.(check bool) "coder cleared" false s.coder_approved;
  Alcotest.(check string) "comment cleared" "" s.coder_comment

let test_load_nonexistent () =
  let db = test_db () in
  let s = Pair_coding_state.load_session ~db ~id:"nope" in
  Alcotest.(check bool) "not found" true (Option.is_none s)

let test_session_with_models () =
  let db = test_db () in
  let config : Pair_coding_state.pair_config =
    {
      task_description = "test models";
      max_review_rounds = 5;
      interrupt_mode = Urgent_only;
      workspace = Some "/tmp/ws";
      worktree_path = None;
      branch_name = Some "feat/test";
      auto_swap_roles = false;
      coder_model = Some "openai:gpt-5.4";
      observer_model = Some "anthropic:claude-haiku-4-5-20251001";
      coordinator_model = Some "anthropic:claude-haiku-4-5-20251001";
    }
  in
  let id = Pair_coding_state.create_session ~db ~config in
  let s = Option.get (Pair_coding_state.load_session ~db ~id) in
  Alcotest.(check (option string))
    "coder_model" (Some "openai:gpt-5.4") s.config.coder_model;
  Alcotest.(check (option string))
    "observer_model" (Some "anthropic:claude-haiku-4-5-20251001")
    s.config.observer_model;
  Alcotest.(check int) "max_rounds" 5 s.config.max_review_rounds;
  Alcotest.(check (option string))
    "workspace" (Some "/tmp/ws") s.config.workspace;
  Alcotest.(check (option string))
    "branch" (Some "feat/test") s.config.branch_name

(* === Prompt tests === *)

let test_coder_prompt_contains_task () =
  let config : Pair_coding_state.pair_config =
    {
      task_description = "Build a REST API";
      max_review_rounds = 3;
      interrupt_mode = Asap;
      workspace = None;
      worktree_path = None;
      branch_name = None;
      auto_swap_roles = false;
      coder_model = None;
      observer_model = None;
      coordinator_model = None;
    }
  in
  let prompt = Pair_coding_prompts.coder_system_prompt ~config in
  Alcotest.(check bool)
    "contains task" true
    (try
       ignore
         (Str.search_forward (Str.regexp_string "Build a REST API") prompt 0);
       true
     with Not_found -> false)

let test_observer_prompt_contains_task () =
  let config : Pair_coding_state.pair_config =
    {
      task_description = "Refactor the auth module";
      max_review_rounds = 3;
      interrupt_mode = Asap;
      workspace = None;
      worktree_path = None;
      branch_name = None;
      auto_swap_roles = false;
      coder_model = None;
      observer_model = None;
      coordinator_model = None;
    }
  in
  let prompt = Pair_coding_prompts.observer_system_prompt ~config in
  Alcotest.(check bool)
    "contains task" true
    (try
       ignore
         (Str.search_forward
            (Str.regexp_string "Refactor the auth module")
            prompt 0);
       true
     with Not_found -> false)

let test_coordinator_prompt_contains_max_rounds () =
  let config : Pair_coding_state.pair_config =
    {
      task_description = "Fix bug";
      max_review_rounds = 5;
      interrupt_mode = Urgent_only;
      workspace = None;
      worktree_path = None;
      branch_name = None;
      auto_swap_roles = false;
      coder_model = None;
      observer_model = None;
      coordinator_model = None;
    }
  in
  let prompt = Pair_coding_prompts.coordinator_system_prompt ~config in
  Alcotest.(check bool)
    "contains max rounds" true
    (try
       ignore (Str.search_forward (Str.regexp_string "5") prompt 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains interrupt mode" true
    (try
       ignore (Str.search_forward (Str.regexp_string "urgent_only") prompt 0);
       true
     with Not_found -> false)

(* === Report tests === *)

let test_report_empty_session () =
  let db = test_db () in
  let config : Pair_coding_state.pair_config =
    {
      task_description = "Empty session test";
      max_review_rounds = 3;
      interrupt_mode = Asap;
      workspace = None;
      worktree_path = None;
      branch_name = None;
      auto_swap_roles = false;
      coder_model = None;
      observer_model = None;
      coordinator_model = None;
    }
  in
  let id = Pair_coding_state.create_session ~db ~config in
  let report = Pair_coding_report.generate ~db ~id in
  Alcotest.(check bool)
    "contains task" true
    (try
       ignore
         (Str.search_forward (Str.regexp_string "Empty session test") report 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains no notes" true
    (try
       ignore
         (Str.search_forward (Str.regexp_string "No notes recorded") report 0);
       true
     with Not_found -> false)

let test_report_with_notes () =
  let db = test_db () in
  let config : Pair_coding_state.pair_config =
    {
      task_description = "Session with notes";
      max_review_rounds = 3;
      interrupt_mode = Asap;
      workspace = None;
      worktree_path = None;
      branch_name = None;
      auto_swap_roles = false;
      coder_model = None;
      observer_model = None;
      coordinator_model = None;
    }
  in
  let id = Pair_coding_state.create_session ~db ~config in
  ignore
    (Pair_coding_state.add_note ~db ~session_id:id ~description:"Critical bug"
       ~category:Bug ~severity:Critical ~file:"src/main.ml" ~line:10 ());
  ignore
    (Pair_coding_state.add_note ~db ~session_id:id ~description:"Minor style"
       ~severity:Low ());
  let nid =
    Pair_coding_state.add_note ~db ~session_id:id ~description:"Resolved item"
      ~severity:Medium ()
  in
  Pair_coding_state.resolve_note ~db ~note_id:nid;
  let report = Pair_coding_report.generate ~db ~id in
  Alcotest.(check bool)
    "contains critical" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Critical bug") report 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains resolved count" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Resolved: 1") report 0);
       true
     with Not_found -> false)

let test_report_nonexistent () =
  let db = test_db () in
  let report = Pair_coding_report.generate ~db ~id:"nope" in
  Alcotest.(check bool)
    "error message" true
    (try
       ignore (Str.search_forward (Str.regexp_string "not found") report 0);
       true
     with Not_found -> false)

let test_format_duration () =
  Alcotest.(check string)
    "seconds" "45s"
    (Pair_coding_report.format_duration_secs 45.0);
  Alcotest.(check string)
    "minutes" "2m 30s"
    (Pair_coding_report.format_duration_secs 150.0);
  Alcotest.(check string)
    "hours" "1h 30m"
    (Pair_coding_report.format_duration_secs 5400.0)

(* === Tool round batch formatting === *)

let test_format_tool_round_batch () =
  let calls =
    [
      ( {
          Provider.id = "tc1";
          function_name = "file_read";
          arguments = {|{"path":"src/foo.ml"}|};
        },
        "file contents here" );
      ( {
          Provider.id = "tc2";
          function_name = "shell_exec";
          arguments = {|{"command":"make test"}|};
        },
        "all tests passed" );
    ]
  in
  let batch = Pair_coding_session.format_tool_round_batch calls in
  Alcotest.(check bool)
    "contains file_read" true
    (try
       ignore (Str.search_forward (Str.regexp_string "file_read") batch 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains shell_exec" true
    (try
       ignore (Str.search_forward (Str.regexp_string "shell_exec") batch 0);
       true
     with Not_found -> false)

(* === CLI command tests === *)

let test_cmd_pair_usage () =
  let result = Command_bridge_pair.cmd_pair [] in
  Alcotest.(check bool)
    "has usage" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Usage") result 0);
       true
     with Not_found -> false)

let test_cmd_pair_unknown () =
  let result = Command_bridge_pair.cmd_pair [ "badcmd" ] in
  Alcotest.(check bool)
    "unknown cmd" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Unknown") result 0);
       true
     with Not_found -> false)

let suite =
  [
    (* Type tests *)
    Alcotest.test_case "role roundtrip" `Quick test_role_roundtrip;
    Alcotest.test_case "phase roundtrip" `Quick test_phase_roundtrip;
    Alcotest.test_case "severity roundtrip" `Quick test_severity_roundtrip;
    Alcotest.test_case "category roundtrip" `Quick test_category_roundtrip;
    Alcotest.test_case "transition roundtrip" `Quick test_transition_roundtrip;
    Alcotest.test_case "role aliases" `Quick test_role_of_string_aliases;
    Alcotest.test_case "initial state" `Quick test_initial_state;
    Alcotest.test_case "coding->review" `Quick test_transition_coding_to_review;
    Alcotest.test_case "review->iteration" `Quick
      test_transition_review_to_iteration;
    Alcotest.test_case "iteration->review" `Quick
      test_transition_iteration_to_review;
    Alcotest.test_case "complete requires approval" `Quick
      test_transition_complete_requires_approval;
    Alcotest.test_case "complete with both approved" `Quick
      test_transition_complete_with_both_approved;
    Alcotest.test_case "complete at max rounds" `Quick
      test_transition_complete_at_max_rounds;
    Alcotest.test_case "finalize" `Quick test_transition_finalize;
    Alcotest.test_case "done rejects" `Quick test_transition_done_rejects;
    Alcotest.test_case "invalid transition" `Quick test_transition_invalid;
    Alcotest.test_case "timeout from any" `Quick
      test_transition_timeout_from_any;
    Alcotest.test_case "abort from any" `Quick test_transition_abort_from_any;
    Alcotest.test_case "both_approved predicate" `Quick test_both_approved;
    Alcotest.test_case "has_blocking_notes" `Quick test_has_blocking_notes;
    Alcotest.test_case "role_key_suffix" `Quick test_role_key_suffix;
    (* DB state tests *)
    Alcotest.test_case "create session" `Quick test_create_session;
    Alcotest.test_case "id length" `Quick test_id_length;
    Alcotest.test_case "session keys format" `Quick test_session_keys;
    Alcotest.test_case "update phase" `Quick test_update_phase;
    Alcotest.test_case "add and load notes" `Quick test_add_and_load_notes;
    Alcotest.test_case "resolve note" `Quick test_resolve_note;
    Alcotest.test_case "set approval atomic" `Quick test_set_approval_atomic;
    Alcotest.test_case "finish session" `Quick test_finish_session;
    Alcotest.test_case "list sessions" `Quick test_list_sessions;
    Alcotest.test_case "clear approvals" `Quick test_clear_approvals;
    Alcotest.test_case "load nonexistent" `Quick test_load_nonexistent;
    Alcotest.test_case "session with models" `Quick test_session_with_models;
    (* Prompt tests *)
    Alcotest.test_case "coder prompt has task" `Quick
      test_coder_prompt_contains_task;
    Alcotest.test_case "observer prompt has task" `Quick
      test_observer_prompt_contains_task;
    Alcotest.test_case "coordinator prompt has rounds" `Quick
      test_coordinator_prompt_contains_max_rounds;
    (* Report tests *)
    Alcotest.test_case "report empty session" `Quick test_report_empty_session;
    Alcotest.test_case "report with notes" `Quick test_report_with_notes;
    Alcotest.test_case "report nonexistent" `Quick test_report_nonexistent;
    Alcotest.test_case "format duration" `Quick test_format_duration;
    (* Tool round batch *)
    Alcotest.test_case "format tool round batch" `Quick
      test_format_tool_round_batch;
    (* CLI *)
    Alcotest.test_case "cmd pair usage" `Quick test_cmd_pair_usage;
    Alcotest.test_case "cmd pair unknown" `Quick test_cmd_pair_unknown;
  ]
