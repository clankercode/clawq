let with_temp_workspace f =
  let dir = Filename.temp_file "clawq_bg_shell_test" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  Fun.protect
    (fun () -> f dir)
    ~finally:(fun () -> try Unix.rmdir dir with _ -> ())

let extract_job_id result =
  try
    let re = Str.regexp {|Background shell job #\([0-9]+\)|} in
    ignore (Str.search_forward re result 0);
    int_of_string (Str.matched_group 1 result)
  with _ -> -1

let test_basic_detach () =
  with_temp_workspace (fun workspace ->
      let sandbox =
        Sandbox.create ~backend:Sandbox.None ~workspace ~extra_allowed_paths:[]
          ~workspace_only:false ()
      in
      let tool =
        Tools_builtin.shell_exec ~workspace ~workspace_only:false
          ~allowed_commands:[] ~extra_allowed_paths:[] ~sandbox
      in
      let interrupted = ref None in
      let result =
        Lwt_main.run
          (let open Lwt.Syntax in
           let trigger =
             let* () = Lwt_unix.sleep 0.1 in
             interrupted := Some "stop";
             Lwt.return_unit
           in
           let invoke =
             tool.Tool.invoke
               ~context:
                 {
                   Tool.session_key = Some "web:test";
                   send_progress = None;
                   interrupt_check = Some (fun () -> !interrupted);
                   inject_system_messages = None;
                   effective_cwd = None;
                   request_cwd_change = None;
                   egress_rules = [];
                 }
               (`Assoc [ ("command", `String "echo hello; sleep 0.5") ])
           in
           let* result, () = Lwt.both invoke trigger in
           Lwt.return result)
      in
      Alcotest.(check bool)
        "contains job info" true
        (Test_helpers.string_contains result "Background shell job");
      let job_id = extract_job_id result in
      let wait_tool = Tools_bg_shell.bg_shell_wait () in
      ignore
        (Lwt_main.run
           (wait_tool.Tool.invoke
              (`Assoc [ ("id", `Int job_id); ("timeout_seconds", `Float 5.0) ])));
      let job = Bg_shell.find job_id in
      Alcotest.(check bool) "job found" true (job <> None);
      let job = Option.get job in
      let log = Bg_shell.read_log job () in
      Alcotest.(check bool)
        "log contains hello" true
        (Test_helpers.string_contains log "hello"))

let test_status_running () =
  with_temp_workspace (fun workspace ->
      let sandbox =
        Sandbox.create ~backend:Sandbox.None ~workspace ~extra_allowed_paths:[]
          ~workspace_only:false ()
      in
      let tool =
        Tools_builtin.shell_exec ~workspace ~workspace_only:false
          ~allowed_commands:[] ~extra_allowed_paths:[] ~sandbox
      in
      let interrupted = ref None in
      let result =
        Lwt_main.run
          (let open Lwt.Syntax in
           let trigger =
             let* () = Lwt_unix.sleep 0.1 in
             interrupted := Some "stop";
             Lwt.return_unit
           in
           let invoke =
             tool.Tool.invoke
               ~context:
                 {
                   Tool.session_key = Some "web:test";
                   send_progress = None;
                   interrupt_check = Some (fun () -> !interrupted);
                   inject_system_messages = None;
                   effective_cwd = None;
                   request_cwd_change = None;
                   egress_rules = [];
                 }
               (`Assoc [ ("command", `String "sleep 0.15") ])
           in
           let* result, () = Lwt.both invoke trigger in
           Lwt.return result)
      in
      Alcotest.(check bool)
        "contains job info" true
        (Test_helpers.string_contains result "Background shell job");
      let job_id = extract_job_id result in
      let status_tool = Tools_bg_shell.bg_shell_status () in
      let status_result =
        Lwt_main.run (status_tool.Tool.invoke (`Assoc [ ("id", `Int job_id) ]))
      in
      Alcotest.(check bool)
        "status says running" true
        (Test_helpers.string_contains status_result "Running");
      (* Wait for the job to finish *)
      let wait_tool = Tools_bg_shell.bg_shell_wait () in
      ignore
        (Lwt_main.run
           (wait_tool.Tool.invoke
              (`Assoc [ ("id", `Int job_id); ("timeout_seconds", `Float 5.0) ])));
      let status_result2 =
        Lwt_main.run (status_tool.Tool.invoke (`Assoc [ ("id", `Int job_id) ]))
      in
      Alcotest.(check bool)
        "status says finished" true
        (Test_helpers.string_contains status_result2 "Finished"))

let test_result_windowed () =
  with_temp_workspace (fun workspace ->
      let sandbox =
        Sandbox.create ~backend:Sandbox.None ~workspace ~extra_allowed_paths:[]
          ~workspace_only:false ()
      in
      let tool =
        Tools_builtin.shell_exec ~workspace ~workspace_only:false
          ~allowed_commands:[] ~extra_allowed_paths:[] ~sandbox
      in
      let interrupted = ref None in
      let detach_result =
        Lwt_main.run
          (let open Lwt.Syntax in
           let trigger =
             let* () = Lwt_unix.sleep 0.1 in
             interrupted := Some "stop";
             Lwt.return_unit
           in
           let invoke =
             tool.Tool.invoke
               ~context:
                 {
                   Tool.session_key = Some "web:test";
                   send_progress = None;
                   interrupt_check = Some (fun () -> !interrupted);
                   inject_system_messages = None;
                   effective_cwd = None;
                   request_cwd_change = None;
                   egress_rules = [];
                 }
               (`Assoc
                  [
                    ( "command",
                      `String
                        "for i in $(seq 1 20); do echo \"line $i\"; done; \
                         sleep 0.2" );
                  ])
           in
           let* result, () = Lwt.both invoke trigger in
           Lwt.return result)
      in
      let job_id = extract_job_id detach_result in
      (* Use bg_shell_wait to ensure the job is done *)
      let wait_tool = Tools_bg_shell.bg_shell_wait () in
      let _wait_result =
        Lwt_main.run
          (wait_tool.Tool.invoke
             (`Assoc [ ("id", `Int job_id); ("timeout_seconds", `Float 5.0) ]))
      in
      let result_tool = Tools_bg_shell.bg_shell_result () in
      let result =
        Lwt_main.run
          (result_tool.Tool.invoke
             (`Assoc [ ("id", `Int job_id); ("head", `Int 5) ]))
      in
      Alcotest.(check bool)
        "result contains line 1" true
        (Test_helpers.string_contains result "line 1");
      Alcotest.(check bool)
        "result contains Finished" true
        (Test_helpers.string_contains result "Finished"))

let test_exit_code_captured () =
  with_temp_workspace (fun workspace ->
      let sandbox =
        Sandbox.create ~backend:Sandbox.None ~workspace ~extra_allowed_paths:[]
          ~workspace_only:false ()
      in
      let tool =
        Tools_builtin.shell_exec ~workspace ~workspace_only:false
          ~allowed_commands:[] ~extra_allowed_paths:[] ~sandbox
      in
      let interrupted = ref None in
      let detach_result =
        Lwt_main.run
          (let open Lwt.Syntax in
           let trigger =
             let* () = Lwt_unix.sleep 0.1 in
             interrupted := Some "stop";
             Lwt.return_unit
           in
           let invoke =
             tool.Tool.invoke
               ~context:
                 {
                   Tool.session_key = Some "web:test";
                   send_progress = None;
                   interrupt_check = Some (fun () -> !interrupted);
                   inject_system_messages = None;
                   effective_cwd = None;
                   request_cwd_change = None;
                   egress_rules = [];
                 }
               (`Assoc [ ("command", `String "sleep 0.1; exit 42") ])
           in
           let* result, () = Lwt.both invoke trigger in
           Lwt.return result)
      in
      let job_id = extract_job_id detach_result in
      let wait_tool = Tools_bg_shell.bg_shell_wait () in
      let _wait_result =
        Lwt_main.run
          (wait_tool.Tool.invoke
             (`Assoc [ ("id", `Int job_id); ("timeout_seconds", `Float 5.0) ]))
      in
      let job = Option.get (Bg_shell.find job_id) in
      match job.status with
      | Bg_shell.Finished { exit_code } ->
          Alcotest.(check int) "exit code 42" 42 exit_code
      | _ -> Alcotest.fail "expected Finished status")

let test_not_found () =
  let status_tool = Tools_bg_shell.bg_shell_status () in
  let result =
    Lwt_main.run (status_tool.Tool.invoke (`Assoc [ ("id", `Int 99999) ]))
  in
  Alcotest.(check bool)
    "error message" true
    (Test_helpers.string_contains result "Error: no background shell job")

let test_result_while_running () =
  with_temp_workspace (fun workspace ->
      let sandbox =
        Sandbox.create ~backend:Sandbox.None ~workspace ~extra_allowed_paths:[]
          ~workspace_only:false ()
      in
      let tool =
        Tools_builtin.shell_exec ~workspace ~workspace_only:false
          ~allowed_commands:[] ~extra_allowed_paths:[] ~sandbox
      in
      let interrupted = ref None in
      let detach_result =
        Lwt_main.run
          (let open Lwt.Syntax in
           let trigger =
             let* () = Lwt_unix.sleep 0.1 in
             interrupted := Some "stop";
             Lwt.return_unit
           in
           let invoke =
             tool.Tool.invoke
               ~context:
                 {
                   Tool.session_key = Some "web:test";
                   send_progress = None;
                   interrupt_check = Some (fun () -> !interrupted);
                   inject_system_messages = None;
                   effective_cwd = None;
                   request_cwd_change = None;
                   egress_rules = [];
                 }
               (`Assoc [ ("command", `String "sleep 1") ])
           in
           let* result, () = Lwt.both invoke trigger in
           Lwt.return result)
      in
      let job_id = extract_job_id detach_result in
      let result_tool = Tools_bg_shell.bg_shell_result () in
      let result =
        Lwt_main.run (result_tool.Tool.invoke (`Assoc [ ("id", `Int job_id) ]))
      in
      Alcotest.(check bool)
        "error for running job" true
        (Test_helpers.string_contains result "still running");
      (* Clean up: kill the long-running process *)
      let job = Option.get (Bg_shell.find job_id) in
      Process_group.signal_group job.pid Sys.sigkill;
      Lwt_main.run (Lwt_unix.sleep 0.1))

let suite =
  [
    Alcotest.test_case "basic detach on interrupt" `Quick test_basic_detach;
    Alcotest.test_case "status shows running then finished" `Quick
      test_status_running;
    Alcotest.test_case "result with head window" `Quick test_result_windowed;
    Alcotest.test_case "exit code captured" `Quick test_exit_code_captured;
    Alcotest.test_case "not found error" `Quick test_not_found;
    Alcotest.test_case "result while running errors" `Quick
      test_result_while_running;
  ]
