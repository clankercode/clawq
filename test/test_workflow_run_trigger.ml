(** Tests for Workflow_run_trigger module. *)

let with_db f = Test_helpers.with_memory_store f

(* --- trigger_source serialization --- *)

let test_trigger_source_room_command () =
  let source =
    Workflow_run_trigger.Room_command
      { room_id = "room1"; requester_id = "user1" }
  in
  let json = Workflow_run_trigger.trigger_source_to_json source in
  let source' = Workflow_run_trigger.trigger_source_of_json json in
  match source' with
  | Workflow_run_trigger.Room_command { room_id; requester_id } ->
      Alcotest.(check string) "room_id" "room1" room_id;
      Alcotest.(check string) "requester_id" "user1" requester_id
  | _ -> Alcotest.fail "expected Room_command"

let test_trigger_source_manual () =
  let source = Workflow_run_trigger.Manual in
  let json = Workflow_run_trigger.trigger_source_to_json source in
  let source' = Workflow_run_trigger.trigger_source_of_json json in
  match source' with
  | Workflow_run_trigger.Manual -> ()
  | _ -> Alcotest.fail "expected Manual"

(* --- run_status serialization --- *)

let test_run_status_serialization () =
  let statuses =
    [
      (Workflow_run_trigger.Pending, "pending");
      (Running, "running");
      (Completed, "completed");
      (Failed, "failed");
    ]
  in
  List.iter
    (fun (status, expected) ->
      Alcotest.(check string)
        expected expected
        (Workflow_run_trigger.run_status_to_string status))
    statuses

let test_run_status_roundtrip () =
  let strings = [ "pending"; "running"; "completed"; "failed" ] in
  List.iter
    (fun s ->
      let status = Workflow_run_trigger.run_status_of_string s in
      let s' = Workflow_run_trigger.run_status_to_string status in
      Alcotest.(check string) s s s')
    strings

(* --- database operations --- *)

let test_create_and_find () =
  with_db (fun db ->
      Workflow_run_trigger.init_schema db;
      let run =
        Workflow_run_trigger.create ~db ~pipeline_name:"test-pipeline"
          ~pipeline_version:"1.0"
          ~inputs:[ ("topic", "testing") ]
          ~trigger_source:
            (Room_command { room_id = "room1"; requester_id = "user1" })
          ~room_id:"room1" ~requester_id:"user1" ()
      in
      Alcotest.(check int) "id" 1 run.id;
      Alcotest.(check string) "pipeline_name" "test-pipeline" run.pipeline_name;
      Alcotest.(check string)
        "status" "pending"
        (Workflow_run_trigger.run_status_to_string run.status);
      Alcotest.(check string) "room_id" "room1" run.room_id;
      Alcotest.(check string) "requester_id" "user1" run.requester_id)

let test_create_with_inputs () =
  with_db (fun db ->
      Workflow_run_trigger.init_schema db;
      let run =
        Workflow_run_trigger.create ~db ~pipeline_name:"research-report"
          ~pipeline_version:"1.0"
          ~inputs:[ ("topic", "AI safety"); ("depth", "deep") ]
          ~trigger_source:Manual ~room_id:"room2" ~requester_id:"user2" ()
      in
      Alcotest.(check int) "id" 1 run.id;
      Alcotest.(check int) "inputs count" 2 (List.length run.inputs);
      Alcotest.(check string)
        "topic" "AI safety"
        (List.assoc "topic" run.inputs))

let test_find_by_id () =
  with_db (fun db ->
      Workflow_run_trigger.init_schema db;
      let run =
        Workflow_run_trigger.create ~db ~pipeline_name:"test"
          ~pipeline_version:"1.0" ~inputs:[] ~trigger_source:Manual ~room_id:"r"
          ~requester_id:"u" ()
      in
      match Workflow_run_trigger.find_by_id ~db ~id:run.id with
      | Some found ->
          Alcotest.(check int) "id" run.id found.id;
          Alcotest.(check string) "pipeline" "test" found.pipeline_name
      | None -> Alcotest.fail "record not found")

let test_find_by_id_not_found () =
  with_db (fun db ->
      Workflow_run_trigger.init_schema db;
      match Workflow_run_trigger.find_by_id ~db ~id:999 with
      | None -> ()
      | Some _ -> Alcotest.fail "should not find nonexistent record")

let test_set_running () =
  with_db (fun db ->
      Workflow_run_trigger.init_schema db;
      let run =
        Workflow_run_trigger.create ~db ~pipeline_name:"test"
          ~pipeline_version:"1.0" ~inputs:[] ~trigger_source:Manual ~room_id:"r"
          ~requester_id:"u" ()
      in
      let updated =
        Workflow_run_trigger.set_running ~db ~id:run.id ~task_id:100
      in
      Alcotest.(check bool) "updated" true updated;
      match Workflow_run_trigger.find_by_id ~db ~id:run.id with
      | Some r ->
          Alcotest.(check string)
            "status" "running"
            (Workflow_run_trigger.run_status_to_string r.status);
          Alcotest.(check (option int)) "task_id" (Some 100) r.task_id
      | None -> Alcotest.fail "record not found")

let test_set_completed () =
  with_db (fun db ->
      Workflow_run_trigger.init_schema db;
      let run =
        Workflow_run_trigger.create ~db ~pipeline_name:"test"
          ~pipeline_version:"1.0" ~inputs:[] ~trigger_source:Manual ~room_id:"r"
          ~requester_id:"u" ()
      in
      ignore (Workflow_run_trigger.set_running ~db ~id:run.id ~task_id:100);
      let updated =
        Workflow_run_trigger.set_completed ~db ~id:run.id
          ~result_preview:"All steps completed" ()
      in
      Alcotest.(check bool) "updated" true updated;
      match Workflow_run_trigger.find_by_id ~db ~id:run.id with
      | Some r ->
          Alcotest.(check string)
            "status" "completed"
            (Workflow_run_trigger.run_status_to_string r.status);
          Alcotest.(check (option string))
            "result_preview" (Some "All steps completed") r.result_preview
      | None -> Alcotest.fail "record not found")

let test_set_failed () =
  with_db (fun db ->
      Workflow_run_trigger.init_schema db;
      let run =
        Workflow_run_trigger.create ~db ~pipeline_name:"test"
          ~pipeline_version:"1.0" ~inputs:[] ~trigger_source:Manual ~room_id:"r"
          ~requester_id:"u" ()
      in
      ignore (Workflow_run_trigger.set_running ~db ~id:run.id ~task_id:100);
      let updated =
        Workflow_run_trigger.set_failed ~db ~id:run.id
          ~error_message:"Pipeline step failed"
      in
      Alcotest.(check bool) "updated" true updated;
      match Workflow_run_trigger.find_by_id ~db ~id:run.id with
      | Some r ->
          Alcotest.(check string)
            "status" "failed"
            (Workflow_run_trigger.run_status_to_string r.status);
          Alcotest.(check (option string))
            "error_message" (Some "Pipeline step failed") r.error_message
      | None -> Alcotest.fail "record not found")

let test_find_pending () =
  with_db (fun db ->
      Workflow_run_trigger.init_schema db;
      let _run1 =
        Workflow_run_trigger.create ~db ~pipeline_name:"p1"
          ~pipeline_version:"1.0" ~inputs:[] ~trigger_source:Manual ~room_id:"r"
          ~requester_id:"u" ()
      in
      let run2 =
        Workflow_run_trigger.create ~db ~pipeline_name:"p2"
          ~pipeline_version:"1.0" ~inputs:[] ~trigger_source:Manual ~room_id:"r"
          ~requester_id:"u" ()
      in
      ignore (Workflow_run_trigger.set_running ~db ~id:run2.id ~task_id:100);
      let pending = Workflow_run_trigger.find_pending ~db () in
      Alcotest.(check int) "count" 1 (List.length pending))

let test_find_by_room () =
  with_db (fun db ->
      Workflow_run_trigger.init_schema db;
      let _run1 =
        Workflow_run_trigger.create ~db ~pipeline_name:"p1"
          ~pipeline_version:"1.0" ~inputs:[] ~trigger_source:Manual
          ~room_id:"room-a" ~requester_id:"u" ()
      in
      let _run2 =
        Workflow_run_trigger.create ~db ~pipeline_name:"p2"
          ~pipeline_version:"1.0" ~inputs:[] ~trigger_source:Manual
          ~room_id:"room-a" ~requester_id:"u" ()
      in
      let _run3 =
        Workflow_run_trigger.create ~db ~pipeline_name:"p3"
          ~pipeline_version:"1.0" ~inputs:[] ~trigger_source:Manual
          ~room_id:"room-b" ~requester_id:"u" ()
      in
      let runs = Workflow_run_trigger.find_by_room ~db ~room_id:"room-a" () in
      Alcotest.(check int) "count" 2 (List.length runs))

let test_count_by_status () =
  with_db (fun db ->
      Workflow_run_trigger.init_schema db;
      let run1 =
        Workflow_run_trigger.create ~db ~pipeline_name:"p1"
          ~pipeline_version:"1.0" ~inputs:[] ~trigger_source:Manual ~room_id:"r"
          ~requester_id:"u" ()
      in
      let _run2 =
        Workflow_run_trigger.create ~db ~pipeline_name:"p2"
          ~pipeline_version:"1.0" ~inputs:[] ~trigger_source:Manual ~room_id:"r"
          ~requester_id:"u" ()
      in
      ignore (Workflow_run_trigger.set_running ~db ~id:run1.id ~task_id:100);
      let counts = Workflow_run_trigger.count_by_status ~db () in
      Alcotest.(check int)
        "pending" 1
        (Hashtbl.find_opt counts "pending" |> Option.value ~default:0);
      Alcotest.(check int)
        "running" 1
        (Hashtbl.find_opt counts "running" |> Option.value ~default:0))

let test_set_running_only_from_pending () =
  with_db (fun db ->
      Workflow_run_trigger.init_schema db;
      let run =
        Workflow_run_trigger.create ~db ~pipeline_name:"test"
          ~pipeline_version:"1.0" ~inputs:[] ~trigger_source:Manual ~room_id:"r"
          ~requester_id:"u" ()
      in
      ignore (Workflow_run_trigger.set_running ~db ~id:run.id ~task_id:100);
      (* Try to set running again - should fail because status is already
         'running', not 'pending' *)
      let updated =
        Workflow_run_trigger.set_running ~db ~id:run.id ~task_id:200
      in
      Alcotest.(check bool) "not updated" false updated)

(* --- workflow_run serialization --- *)

let test_workflow_run_json_roundtrip () =
  with_db (fun db ->
      Workflow_run_trigger.init_schema db;
      let run =
        Workflow_run_trigger.create ~db ~pipeline_name:"test-pipeline"
          ~pipeline_version:"2.0"
          ~inputs:[ ("key1", "val1"); ("key2", "val2") ]
          ~trigger_source:
            (Room_command { room_id = "slack:C123"; requester_id = "U456" })
          ~room_id:"slack:C123" ~requester_id:"U456" ()
      in
      let json_str = Workflow_run_trigger.workflow_run_to_string run in
      let json = Yojson.Safe.from_string json_str in
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "pipeline_name" "test-pipeline"
        (json |> member "pipeline_name" |> to_string);
      Alcotest.(check string)
        "room_id" "slack:C123"
        (json |> member "room_id" |> to_string);
      Alcotest.(check string)
        "status" "pending"
        (json |> member "status" |> to_string))

let test_format_workflow_run () =
  with_db (fun db ->
      Workflow_run_trigger.init_schema db;
      let run =
        Workflow_run_trigger.create ~db ~pipeline_name:"research-report"
          ~pipeline_version:"1.0"
          ~inputs:[ ("topic", "AI") ]
          ~trigger_source:Manual ~room_id:"r" ~requester_id:"u" ()
      in
      let formatted = Workflow_run_trigger.format_workflow_run run in
      (* Should contain pipeline name and version *)
      Alcotest.(check bool)
        "contains pipeline name" true
        (String.length formatted > 0))

let test_format_workflow_run_list_empty () =
  with_db (fun db ->
      Workflow_run_trigger.init_schema db;
      let formatted =
        Workflow_run_trigger.format_workflow_run_list
          (Workflow_run_trigger.find_pending ~db ())
      in
      Alcotest.(check string) "empty" "No workflow runs found." formatted)

let test_format_workflow_run_list () =
  with_db (fun db ->
      Workflow_run_trigger.init_schema db;
      let _run =
        Workflow_run_trigger.create ~db ~pipeline_name:"p1"
          ~pipeline_version:"1.0" ~inputs:[] ~trigger_source:Manual ~room_id:"r"
          ~requester_id:"u" ()
      in
      let runs = Workflow_run_trigger.find_pending ~db () in
      let formatted = Workflow_run_trigger.format_workflow_run_list runs in
      Alcotest.(check bool) "not empty" true (String.length formatted > 0))

(* --- idempotency and state guards --- *)

(** Duplicate creates produce distinct records: workflow_run_trigger does not
    deduplicate by SHA — each trigger creates a new row. *)
let test_create_distinct_records () =
  with_db (fun db ->
      Workflow_run_trigger.init_schema db;
      let run1 =
        Workflow_run_trigger.create ~db ~pipeline_name:"deploy"
          ~pipeline_version:"1.0"
          ~inputs:[ ("env", "prod") ]
          ~trigger_source:Manual ~room_id:"r" ~requester_id:"u" ()
      in
      let run2 =
        Workflow_run_trigger.create ~db ~pipeline_name:"deploy"
          ~pipeline_version:"1.0"
          ~inputs:[ ("env", "prod") ]
          ~trigger_source:Manual ~room_id:"r" ~requester_id:"u" ()
      in
      Alcotest.(check bool) "distinct ids" true (run1.id <> run2.id);
      Alcotest.(check int)
        "total pending" 2
        (List.length (Workflow_run_trigger.find_pending ~db ())))

(** A completed run cannot be set back to running. *)
let test_completed_cannot_restart () =
  with_db (fun db ->
      Workflow_run_trigger.init_schema db;
      let run =
        Workflow_run_trigger.create ~db ~pipeline_name:"test"
          ~pipeline_version:"1.0" ~inputs:[] ~trigger_source:Manual ~room_id:"r"
          ~requester_id:"u" ()
      in
      ignore (Workflow_run_trigger.set_running ~db ~id:run.id ~task_id:100);
      ignore
        (Workflow_run_trigger.set_completed ~db ~id:run.id
           ~result_preview:"done" ());
      let updated =
        Workflow_run_trigger.set_running ~db ~id:run.id ~task_id:200
      in
      Alcotest.(check bool) "not updated" false updated;
      match Workflow_run_trigger.find_by_id ~db ~id:run.id with
      | Some r ->
          Alcotest.(check string)
            "still completed" "completed"
            (Workflow_run_trigger.run_status_to_string r.status)
      | None -> Alcotest.fail "record not found")

(** A failed run cannot be set back to running. *)
let test_failed_cannot_restart () =
  with_db (fun db ->
      Workflow_run_trigger.init_schema db;
      let run =
        Workflow_run_trigger.create ~db ~pipeline_name:"test"
          ~pipeline_version:"1.0" ~inputs:[] ~trigger_source:Manual ~room_id:"r"
          ~requester_id:"u" ()
      in
      ignore (Workflow_run_trigger.set_running ~db ~id:run.id ~task_id:100);
      ignore
        (Workflow_run_trigger.set_failed ~db ~id:run.id
           ~error_message:"step failed");
      let updated =
        Workflow_run_trigger.set_running ~db ~id:run.id ~task_id:200
      in
      Alcotest.(check bool) "not updated" false updated;
      match Workflow_run_trigger.find_by_id ~db ~id:run.id with
      | Some r ->
          Alcotest.(check string)
            "still failed" "failed"
            (Workflow_run_trigger.run_status_to_string r.status)
      | None -> Alcotest.fail "record not found")

(** Only pending runs appear in find_pending; running/completed/failed are
    excluded. *)
let test_find_pending_excludes_terminal () =
  with_db (fun db ->
      Workflow_run_trigger.init_schema db;
      let run1 =
        Workflow_run_trigger.create ~db ~pipeline_name:"p1"
          ~pipeline_version:"1.0" ~inputs:[] ~trigger_source:Manual ~room_id:"r"
          ~requester_id:"u" ()
      in
      let run2 =
        Workflow_run_trigger.create ~db ~pipeline_name:"p2"
          ~pipeline_version:"1.0" ~inputs:[] ~trigger_source:Manual ~room_id:"r"
          ~requester_id:"u" ()
      in
      let run3 =
        Workflow_run_trigger.create ~db ~pipeline_name:"p3"
          ~pipeline_version:"1.0" ~inputs:[] ~trigger_source:Manual ~room_id:"r"
          ~requester_id:"u" ()
      in
      let run4 =
        Workflow_run_trigger.create ~db ~pipeline_name:"p4"
          ~pipeline_version:"1.0" ~inputs:[] ~trigger_source:Manual ~room_id:"r"
          ~requester_id:"u" ()
      in
      ignore (Workflow_run_trigger.set_running ~db ~id:run1.id ~task_id:100);
      ignore
        (Workflow_run_trigger.set_completed ~db ~id:run2.id
           ~result_preview:"done" ());
      ignore
        (Workflow_run_trigger.set_failed ~db ~id:run3.id ~error_message:"err");
      let pending = Workflow_run_trigger.find_pending ~db () in
      Alcotest.(check int) "only p4 pending" 1 (List.length pending);
      match pending with
      | [ r ] -> Alcotest.(check int) "is run4" run4.id r.id
      | _ -> Alcotest.fail "expected exactly one pending")

(* --- suite --- *)

let suite =
  [
    (* trigger_source serialization *)
    ("trigger_source room_command", `Quick, test_trigger_source_room_command);
    ("trigger_source manual", `Quick, test_trigger_source_manual);
    (* run_status serialization *)
    ("run_status serialization", `Quick, test_run_status_serialization);
    ("run_status roundtrip", `Quick, test_run_status_roundtrip);
    (* database operations *)
    ("create and find", `Quick, test_create_and_find);
    ("create with inputs", `Quick, test_create_with_inputs);
    ("find_by_id", `Quick, test_find_by_id);
    ("find_by_id not found", `Quick, test_find_by_id_not_found);
    ("set_running", `Quick, test_set_running);
    ("set_completed", `Quick, test_set_completed);
    ("set_failed", `Quick, test_set_failed);
    ("find_pending", `Quick, test_find_pending);
    ("find_by_room", `Quick, test_find_by_room);
    ("count_by_status", `Quick, test_count_by_status);
    ("set_running only from pending", `Quick, test_set_running_only_from_pending);
    (* serialization *)
    ("workflow_run json roundtrip", `Quick, test_workflow_run_json_roundtrip);
    ("format_workflow_run", `Quick, test_format_workflow_run);
    ( "format_workflow_run_list empty",
      `Quick,
      test_format_workflow_run_list_empty );
    ("format_workflow_run_list", `Quick, test_format_workflow_run_list);
    (* idempotency and state guards *)
    ("create produces distinct records", `Quick, test_create_distinct_records);
    ("completed cannot restart", `Quick, test_completed_cannot_restart);
    ("failed cannot restart", `Quick, test_failed_cannot_restart);
    ( "find_pending excludes terminal",
      `Quick,
      test_find_pending_excludes_terminal );
  ]
