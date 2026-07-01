(** Tests for Github_review_run module. *)

let with_db f =
  let db = Memory.init ~db_path:":memory:" () in
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

(* --- run_kind serialization --- *)

let test_run_kind_code_review () =
  Alcotest.(check string)
    "code_review" "code_review"
    (Github_review_run.run_kind_to_string Code_review)

let test_run_kind_security_scan () =
  Alcotest.(check string)
    "security_scan" "security_scan"
    (Github_review_run.run_kind_to_string Security_scan)

let test_run_kind_custom () =
  Alcotest.(check string)
    "custom" "custom:my_scan"
    (Github_review_run.run_kind_to_string (Custom "my_scan"))

let test_run_kind_roundtrip () =
  let kinds =
    [
      Github_review_run.Code_review;
      Security_scan;
      Custom "test_scan";
      Custom "another";
    ]
  in
  List.iter
    (fun kind ->
      let s = Github_review_run.run_kind_to_string kind in
      let kind' = Github_review_run.run_kind_of_string s in
      if kind <> kind' then Alcotest.failf "roundtrip failed for %s" s)
    kinds

(* --- trigger_source serialization --- *)

let test_trigger_source_label () =
  Alcotest.(check string)
    "label" "label:review"
    (Github_review_run.trigger_source_to_string (Label "review"))

let test_trigger_source_room_command () =
  Alcotest.(check string)
    "room_command" "room_command:room1:user1"
    (Github_review_run.trigger_source_to_string
       (Room_command { room_id = "room1"; requester_id = "user1" }))

let test_trigger_source_manual () =
  Alcotest.(check string)
    "manual" "manual"
    (Github_review_run.trigger_source_to_string Manual)

(* --- label_to_run_kind --- *)

let test_label_review () =
  match Github_review_run.label_to_run_kind "review" with
  | Some Github_review_run.Code_review -> ()
  | _ -> Alcotest.fail "expected Code_review for 'review'"

let test_label_code_review () =
  match Github_review_run.label_to_run_kind "code-review" with
  | Some Github_review_run.Code_review -> ()
  | _ -> Alcotest.fail "expected Code_review for 'code-review'"

let test_label_needs_review () =
  match Github_review_run.label_to_run_kind "needs-review" with
  | Some Github_review_run.Code_review -> ()
  | _ -> Alcotest.fail "expected Code_review for 'needs-review'"

let test_label_security () =
  match Github_review_run.label_to_run_kind "security" with
  | Some Github_review_run.Security_scan -> ()
  | _ -> Alcotest.fail "expected Security_scan for 'security'"

let test_label_security_review () =
  match Github_review_run.label_to_run_kind "security-review" with
  | Some Github_review_run.Security_scan -> ()
  | _ -> Alcotest.fail "expected Security_scan for 'security-review'"

let test_label_bug_returns_none () =
  match Github_review_run.label_to_run_kind "bug" with
  | None -> ()
  | _ -> Alcotest.fail "expected None for 'bug'"

let test_label_case_insensitive () =
  match Github_review_run.label_to_run_kind "REVIEW" with
  | Some Github_review_run.Code_review -> ()
  | _ -> Alcotest.fail "expected Code_review for 'REVIEW'"

(* --- database operations --- *)

let test_create_and_find () =
  with_db (fun db ->
      let run =
        Github_review_run.create ~db ~repo:"owner/repo" ~pr_number:42
          ~head_sha:"abc123" ~run_kind:Code_review
          ~trigger_source:(Label "review") ()
      in
      Alcotest.(check int) "id" 1 run.id;
      Alcotest.(check string) "repo" "owner/repo" run.repo;
      Alcotest.(check int) "pr_number" 42 run.pr_number;
      Alcotest.(check string) "head_sha" "abc123" run.head_sha;
      Alcotest.(check string)
        "status" "pending"
        (Github_review_run.run_status_to_string run.status))

let test_create_idempotent () =
  with_db (fun db ->
      let run1 =
        Github_review_run.create ~db ~repo:"owner/repo" ~pr_number:42
          ~head_sha:"abc123" ~run_kind:Code_review
          ~trigger_source:(Label "review") ()
      in
      let run2 =
        Github_review_run.create ~db ~repo:"owner/repo" ~pr_number:42
          ~head_sha:"abc123" ~run_kind:Code_review
          ~trigger_source:(Label "review") ()
      in
      Alcotest.(check int) "same id" run1.id run2.id)

let test_different_run_kind_creates_new () =
  with_db (fun db ->
      let _run1 =
        Github_review_run.create ~db ~repo:"owner/repo" ~pr_number:42
          ~head_sha:"abc123" ~run_kind:Code_review
          ~trigger_source:(Label "review") ()
      in
      let run2 =
        Github_review_run.create ~db ~repo:"owner/repo" ~pr_number:42
          ~head_sha:"abc123" ~run_kind:Security_scan
          ~trigger_source:(Label "security") ()
      in
      Alcotest.(check int) "different id" 2 run2.id)

let test_different_sha_creates_new () =
  with_db (fun db ->
      let _run1 =
        Github_review_run.create ~db ~repo:"owner/repo" ~pr_number:42
          ~head_sha:"abc123" ~run_kind:Code_review
          ~trigger_source:(Label "review") ()
      in
      let run2 =
        Github_review_run.create ~db ~repo:"owner/repo" ~pr_number:42
          ~head_sha:"def456" ~run_kind:Code_review
          ~trigger_source:(Label "review") ()
      in
      Alcotest.(check int) "different id" 2 run2.id)

let test_set_running () =
  with_db (fun db ->
      let run =
        Github_review_run.create ~db ~repo:"owner/repo" ~pr_number:42
          ~head_sha:"abc123" ~run_kind:Code_review
          ~trigger_source:(Label "review") ()
      in
      let updated = Github_review_run.set_running ~db ~id:run.id ~task_id:100 in
      Alcotest.(check bool) "updated" true updated;
      match Github_review_run.find_by_id ~db ~id:run.id with
      | Some r ->
          Alcotest.(check string)
            "status" "running"
            (Github_review_run.run_status_to_string r.status);
          Alcotest.(check (option int)) "task_id" (Some 100) r.task_id
      | None -> Alcotest.fail "record not found")

let test_set_completed () =
  with_db (fun db ->
      let run =
        Github_review_run.create ~db ~repo:"owner/repo" ~pr_number:42
          ~head_sha:"abc123" ~run_kind:Code_review
          ~trigger_source:(Label "review") ()
      in
      ignore (Github_review_run.set_running ~db ~id:run.id ~task_id:100);
      let updated =
        Github_review_run.set_completed ~db ~id:run.id
          ~result_preview:"No issues found" ()
      in
      Alcotest.(check bool) "updated" true updated;
      match Github_review_run.find_by_id ~db ~id:run.id with
      | Some r ->
          Alcotest.(check string)
            "status" "completed"
            (Github_review_run.run_status_to_string r.status);
          Alcotest.(check (option string))
            "result_preview" (Some "No issues found") r.result_preview
      | None -> Alcotest.fail "record not found")

let test_set_failed () =
  with_db (fun db ->
      let run =
        Github_review_run.create ~db ~repo:"owner/repo" ~pr_number:42
          ~head_sha:"abc123" ~run_kind:Code_review
          ~trigger_source:(Label "review") ()
      in
      ignore (Github_review_run.set_running ~db ~id:run.id ~task_id:100);
      let updated =
        Github_review_run.set_failed ~db ~id:run.id
          ~error_message:"API rate limit exceeded"
      in
      Alcotest.(check bool) "updated" true updated;
      match Github_review_run.find_by_id ~db ~id:run.id with
      | Some r ->
          Alcotest.(check string)
            "status" "failed"
            (Github_review_run.run_status_to_string r.status);
          Alcotest.(check (option string))
            "error_message" (Some "API rate limit exceeded") r.error_message
      | None -> Alcotest.fail "record not found")

let test_find_by_repo_pr () =
  with_db (fun db ->
      ignore
        (Github_review_run.create ~db ~repo:"owner/repo" ~pr_number:42
           ~head_sha:"abc123" ~run_kind:Code_review
           ~trigger_source:(Label "review") ());
      ignore
        (Github_review_run.create ~db ~repo:"owner/repo" ~pr_number:42
           ~head_sha:"abc123" ~run_kind:Security_scan
           ~trigger_source:(Label "security") ());
      ignore
        (Github_review_run.create ~db ~repo:"owner/repo" ~pr_number:99
           ~head_sha:"xyz" ~run_kind:Code_review
           ~trigger_source:(Label "review") ());
      let runs =
        Github_review_run.find_by_repo_pr ~db ~repo:"owner/repo" ~pr_number:42
      in
      Alcotest.(check int) "count" 2 (List.length runs))

let test_find_pending () =
  with_db (fun db ->
      let run1 =
        Github_review_run.create ~db ~repo:"owner/repo" ~pr_number:1
          ~head_sha:"a" ~run_kind:Code_review ~trigger_source:(Label "review")
          ()
      in
      ignore
        (Github_review_run.create ~db ~repo:"owner/repo" ~pr_number:2
           ~head_sha:"b" ~run_kind:Code_review ~trigger_source:(Label "review")
           ());
      ignore (Github_review_run.set_running ~db ~id:run1.id ~task_id:100);
      ignore
        (Github_review_run.set_completed ~db ~id:run1.id ~result_preview:"done"
           ());
      let pending = Github_review_run.find_pending ~db () in
      Alcotest.(check int) "count" 1 (List.length pending))

let test_count_by_status () =
  with_db (fun db ->
      let run1 =
        Github_review_run.create ~db ~repo:"owner/repo" ~pr_number:1
          ~head_sha:"a" ~run_kind:Code_review ~trigger_source:(Label "review")
          ()
      in
      ignore
        (Github_review_run.create ~db ~repo:"owner/repo" ~pr_number:2
           ~head_sha:"b" ~run_kind:Code_review ~trigger_source:(Label "review")
           ());
      ignore (Github_review_run.set_running ~db ~id:run1.id ~task_id:100);
      let counts = Github_review_run.count_by_status ~db () in
      Alcotest.(check int)
        "pending" 1
        (Hashtbl.find_opt counts "pending" |> Option.value ~default:0);
      Alcotest.(check int)
        "running" 1
        (Hashtbl.find_opt counts "running" |> Option.value ~default:0))

let test_trigger_from_label () =
  with_db (fun db ->
      let result =
        Github_review_run.trigger_from_label ~db ~repo:"owner/repo"
          ~pr_number:42 ~head_sha:"abc123" ~label:"review"
      in
      Alcotest.(check bool) "triggered" true (Option.is_some result);
      let result2 =
        Github_review_run.trigger_from_label ~db ~repo:"owner/repo"
          ~pr_number:42 ~head_sha:"abc123" ~label:"bug"
      in
      Alcotest.(check bool) "not triggered" true (Option.is_none result2))

let test_trigger_from_room_command () =
  with_db (fun db ->
      let run =
        Github_review_run.trigger_from_room_command ~db ~repo:"owner/repo"
          ~pr_number:42 ~head_sha:"abc123" ~run_kind:Security_scan
          ~room_id:"room1" ~requester_id:"user1"
      in
      Alcotest.(check string) "repo" "owner/repo" run.repo;
      Alcotest.(check string)
        "run_kind" "security_scan"
        (Github_review_run.run_kind_to_string run.run_kind))

(* --- run_status serialization --- *)

let test_run_status_serialization () =
  let statuses =
    [
      (Github_review_run.Pending, "pending");
      (Running, "running");
      (Completed, "completed");
      (Failed, "failed");
    ]
  in
  List.iter
    (fun (status, expected) ->
      Alcotest.(check string)
        expected expected
        (Github_review_run.run_status_to_string status))
    statuses

let test_run_status_roundtrip () =
  let strings = [ "pending"; "running"; "completed"; "failed" ] in
  List.iter
    (fun s ->
      let status = Github_review_run.run_status_of_string s in
      let s' = Github_review_run.run_status_to_string status in
      Alcotest.(check string) s s s')
    strings

(* --- suite --- *)

let suite =
  [
    (* run_kind serialization *)
    ("run_kind code_review", `Quick, test_run_kind_code_review);
    ("run_kind security_scan", `Quick, test_run_kind_security_scan);
    ("run_kind custom", `Quick, test_run_kind_custom);
    ("run_kind roundtrip", `Quick, test_run_kind_roundtrip);
    (* trigger_source serialization *)
    ("trigger_source label", `Quick, test_trigger_source_label);
    ("trigger_source room_command", `Quick, test_trigger_source_room_command);
    ("trigger_source manual", `Quick, test_trigger_source_manual);
    (* label_to_run_kind *)
    ("label review", `Quick, test_label_review);
    ("label code-review", `Quick, test_label_code_review);
    ("label needs-review", `Quick, test_label_needs_review);
    ("label security", `Quick, test_label_security);
    ("label security-review", `Quick, test_label_security_review);
    ("label bug returns None", `Quick, test_label_bug_returns_none);
    ("label case insensitive", `Quick, test_label_case_insensitive);
    (* database operations *)
    ("create and find", `Quick, test_create_and_find);
    ("create idempotent", `Quick, test_create_idempotent);
    ( "different run_kind creates new",
      `Quick,
      test_different_run_kind_creates_new );
    ("different sha creates new", `Quick, test_different_sha_creates_new);
    ("set_running", `Quick, test_set_running);
    ("set_completed", `Quick, test_set_completed);
    ("set_failed", `Quick, test_set_failed);
    ("find_by_repo_pr", `Quick, test_find_by_repo_pr);
    ("find_pending", `Quick, test_find_pending);
    ("count_by_status", `Quick, test_count_by_status);
    ("trigger_from_label", `Quick, test_trigger_from_label);
    ("trigger_from_room_command", `Quick, test_trigger_from_room_command);
    (* run_status serialization *)
    ("run_status serialization", `Quick, test_run_status_serialization);
    ("run_status roundtrip", `Quick, test_run_status_roundtrip);
  ]
