(* B771: durable GitHub work-item envelope tests. *)

let with_db f =
  let db = Memory.init ~db_path:":memory:" () in
  Github_work_item.init_schema db;
  Background_task.init_schema db;
  f db

let create ~db ?(dedup_key = "o/r#1:comment:10") ?runner_pref ?host_pref
    ?(requester = "alice") ?(prompt = "explain the build") () =
  Github_work_item.create_if_new ~db ~dedup_key ~delivery_id:"d-1"
    ~repo_full_name:"o/r" ~issue_number:1 ~requester ?runner_pref ?host_pref
    ~prompt ~preamble:"## GitHub Context" ()

let created_exn = function
  | Ok (Github_work_item.Created item) -> item
  | Ok (Github_work_item.Duplicate _) -> Alcotest.fail "expected Created"
  | Error msg -> Alcotest.fail msg

let get_exn ~db ~id =
  match Github_work_item.get ~db ~id with
  | Some item -> item
  | None -> Alcotest.failf "work item %d not found" id

let test_create_and_duplicate () =
  with_db (fun db ->
      let item = created_exn (create ~db ()) in
      Alcotest.(check string)
        "queued" "queued"
        (Github_work_item.string_of_status item.status);
      Alcotest.(check string) "requester" "alice" item.requester;
      (* Same dedup key: duplicate delivery must not create a second item. *)
      (match create ~db ~requester:"mallory" () with
      | Ok (Github_work_item.Duplicate existing) ->
          Alcotest.(check int) "same item" item.id existing.id;
          Alcotest.(check string)
            "original requester kept" "alice" existing.requester
      | Ok (Github_work_item.Created _) ->
          Alcotest.fail "duplicate delivery created a second work item"
      | Error msg -> Alcotest.fail msg);
      Alcotest.(check int)
        "one row" 1
        (List.length (Github_work_item.list ~db ())))

let test_create_refuses_bad_input () =
  with_db (fun db ->
      (match create ~db ~requester:"  " () with
      | Error msg ->
          Alcotest.(check bool)
            "anonymous refused" true
            (String_util.contains msg "requester")
      | Ok _ -> Alcotest.fail "anonymous trigger must be refused");
      match
        Github_work_item.create_if_new ~db ~dedup_key:"x" ~repo_full_name:""
          ~issue_number:0 ~requester:"alice" ~prompt:"p" ()
      with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "missing repo/issue must be refused")

let test_dedup_key_forms () =
  let with_comment =
    Github_work_item.dedup_key_for ~repo_full_name:"o/r" ~issue_number:7
      ~comment_id:(Some 42) ~delivery_id:(Some "d")
  in
  Alcotest.(check string) "comment key" "o/r#7:comment:42" with_comment;
  let with_delivery =
    Github_work_item.dedup_key_for ~repo_full_name:"o/r" ~issue_number:7
      ~comment_id:None ~delivery_id:(Some "d-9")
  in
  Alcotest.(check string) "delivery key" "o/r#7:delivery:d-9" with_delivery

let test_parse_command_options () =
  let open Github_work_item in
  let opts = parse_command_options "runner=codex host=herdr summarize this" in
  Alcotest.(check (option string)) "runner" (Some "codex") opts.runner_opt;
  Alcotest.(check (option string)) "host" (Some "herdr") opts.host_opt;
  Alcotest.(check string) "request" "summarize this" opts.request;
  Alcotest.(check bool) "wants work item" true (wants_work_item opts);
  let bare = parse_command_options "just answer my question" in
  Alcotest.(check bool) "bare stays legacy" false (wants_work_item bare);
  Alcotest.(check string)
    "bare text untouched" "just answer my question" bare.request;
  (* Unknown key=value tokens are user text, not options. *)
  let unknown = parse_command_options "foo=bar what does foo=bar mean?" in
  Alcotest.(check bool)
    "unknown key not an option" false (wants_work_item unknown);
  Alcotest.(check string)
    "unknown key preserved" "foo=bar what does foo=bar mean?" unknown.request;
  let multiline = parse_command_options "runner=auto do this\nand also that" in
  Alcotest.(check (option string))
    "auto runner" (Some "auto") multiline.runner_opt;
  Alcotest.(check string)
    "multiline request kept" "do this\nand also that" multiline.request

let test_lifecycle_and_sync () =
  with_db (fun db ->
      let item = created_exn (create ~db ()) in
      Github_work_item.attach_task ~db ~id:item.id ~background_task_id:99;
      Github_work_item.set_status ~db ~id:item.id
        ~status:Github_work_item.Running;
      let running = get_exn ~db ~id:item.id in
      Alcotest.(check (option int))
        "task attached" (Some 99) running.background_task_id;
      Alcotest.(check bool)
        "started stamped" true
        (Option.is_some running.started_at);
      (* Restart recovery: task finished while daemon was down. *)
      let synced =
        Github_work_item.sync_from_task ~db running
          ~task_status:Background_task.Succeeded
          ~task_result:(Some "the answer")
      in
      (match synced with
      | Some fresh ->
          Alcotest.(check string)
            "succeeded" "succeeded"
            (Github_work_item.string_of_status fresh.status);
          Alcotest.(check (option string))
            "summary from task" (Some "the answer") fresh.result_summary
      | None -> Alcotest.fail "sync should surface a terminal item");
      Alcotest.(check bool)
        "terminal" true
        (Github_work_item.is_terminal_status (get_exn ~db ~id:item.id).status))

let test_sync_maps_failure_and_cancel () =
  with_db (fun db ->
      let item = created_exn (create ~db ()) in
      (match
         Github_work_item.sync_from_task ~db item
           ~task_status:Background_task.Cancelled ~task_result:None
       with
      | Some fresh ->
          Alcotest.(check string)
            "cancelled" "cancelled"
            (Github_work_item.string_of_status fresh.status)
      | None -> Alcotest.fail "cancel should be terminal");
      let item2 = created_exn (create ~db ~dedup_key:"o/r#1:comment:11" ()) in
      match
        Github_work_item.sync_from_task ~db item2
          ~task_status:Background_task.Failed ~task_result:(Some "boom")
      with
      | Some fresh ->
          Alcotest.(check string)
            "failed" "failed"
            (Github_work_item.string_of_status fresh.status)
      | None -> Alcotest.fail "failure should be terminal")

let test_publication_idempotent () =
  with_db (fun db ->
      let item = created_exn (create ~db ()) in
      let first =
        Github_work_item.record_publication ~db ~id:item.id
          ~comment_id:(Some 555) ~publication_status:"published"
      in
      Alcotest.(check bool) "first publication recorded" true first;
      let second =
        Github_work_item.record_publication ~db ~id:item.id
          ~comment_id:(Some 777) ~publication_status:"published"
      in
      Alcotest.(check bool) "second publication skipped" false second;
      let fresh = get_exn ~db ~id:item.id in
      Alcotest.(check (option int))
        "original comment id kept" (Some 555) fresh.published_comment_id;
      Alcotest.(check bool)
        "already published" true
        (Github_work_item.already_published fresh))

let test_ack_comment_and_blocked () =
  with_db (fun db ->
      let item = created_exn (create ~db ~host_pref:"herdr" ()) in
      Github_work_item.set_ack_comment ~db ~id:item.id ~comment_id:321;
      Alcotest.(check (option int))
        "ack recorded" (Some 321) (get_exn ~db ~id:item.id).ack_comment_id;
      Github_work_item.record_result ~db ~id:item.id
        ~status:Github_work_item.Blocked
        ~result_kind:Github_work_item.Result_blocked
        ~result_summary:"host unavailable";
      let fresh = get_exn ~db ~id:item.id in
      Alcotest.(check string)
        "blocked" "blocked"
        (Github_work_item.string_of_status fresh.status);
      Alcotest.(check bool)
        "blocked is not terminal" false
        (Github_work_item.is_terminal_status fresh.status))

let test_extract_final_agent_message () =
  let codex_log =
    String.concat "\n"
      [
        {|{"type":"turn.started"}|};
        {|{"type":"item.completed","item":{"type":"agent_message","text":"first"}}|};
        {|{"type":"item.completed","item":{"type":"agent_message","text":"final answer"}}|};
        {|{"type":"turn.completed"}|};
      ]
  in
  Alcotest.(check string)
    "codex jsonl last agent message" "final answer"
    (Github.extract_final_agent_message codex_log);
  Alcotest.(check string)
    "plain text falls through" "plain claude output"
    (Github.extract_final_agent_message "plain claude output\n")

let suite =
  [
    Alcotest.test_case "create is idempotent on dedup key" `Quick
      test_create_and_duplicate;
    Alcotest.test_case "create refuses anonymous/invalid triggers" `Quick
      test_create_refuses_bad_input;
    Alcotest.test_case "dedup key forms" `Quick test_dedup_key_forms;
    Alcotest.test_case "/clawq option parsing" `Quick test_parse_command_options;
    Alcotest.test_case "lifecycle and restart sync" `Quick
      test_lifecycle_and_sync;
    Alcotest.test_case "sync maps failure and cancel" `Quick
      test_sync_maps_failure_and_cancel;
    Alcotest.test_case "publication is idempotent" `Quick
      test_publication_idempotent;
    Alcotest.test_case "ack comment and blocked state" `Quick
      test_ack_comment_and_blocked;
    Alcotest.test_case "final agent message extraction" `Quick
      test_extract_final_agent_message;
  ]
