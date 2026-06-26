(* Runner integration tests — all tagged Slow, skipped by `make test`.
   Run with: make test-run ARGS="test runner_integration"
   Or: make test-all *)

let lwt_run f = Lwt_main.run (f ())

(* --- Helpers --- *)

let read_channel ch =
  Lwt.catch (fun () -> Lwt_io.read ch) (fun _ -> Lwt.return "")

let run_binary_with_timeout ~timeout_s argv =
  let env = Unix.environment () in
  let proc = Process_group.start ~env (Process_group.Exec argv) in
  Lwt_main.run
    (let open Lwt.Syntax in
     let timed_out = ref false in
     Lwt.finalize
       (fun () ->
         let run_branch =
           let* stdout, stderr =
             Lwt.both (read_channel proc.stdout) (read_channel proc.stderr)
           in
           let* status = Process_group.wait proc.pid in
           if !timed_out then Lwt.return (Error "timeout")
           else
             let exit_code = Background_task.exit_code_of_status status in
             Lwt.return (Ok (exit_code, stdout, stderr))
         in
         let timeout_branch =
           let* () = Lwt_unix.sleep timeout_s in
           timed_out := true;
           let* () = Process_group.terminate proc.pid in
           Lwt.return (Error "timeout")
         in
         Lwt.pick [ run_branch; timeout_branch ])
       (fun () -> Process_group.close proc))

let is_auth_error output =
  let lower = String.lowercase_ascii output in
  List.exists
    (fun pattern ->
      try
        ignore (Str.search_forward (Str.regexp_string pattern) lower 0);
        true
      with Not_found -> false)
    [
      "unauthorized";
      "api key";
      "api_key";
      "401";
      "credentials";
      "authentication";
      "not authenticated";
      "invalid token";
      "permission denied";
      "access denied";
      "login required";
      "sign in";
      "no api key";
    ]

let all_external_runners =
  [
    Background_task.Codex;
    Background_task.Claude;
    Background_task.Kimi;
    Background_task.Gemini;
    Background_task.Opencode;
    Background_task.Cursor;
  ]

let runner_name (r : Background_task.runner) =
  Background_task.string_of_runner r

let runner_bin (r : Background_task.runner) = Background_task.runner_binary r

(* --- Tier 1: Version checks --- *)

let make_version_test (runner : Background_task.runner) () =
  if not (Background_task.runner_available runner) then Alcotest.skip ()
  else
    let binary = runner_bin runner in
    match run_binary_with_timeout ~timeout_s:10.0 [| binary; "--version" |] with
    | Error "timeout" ->
        Alcotest.failf "%s --version timed out after 10s" binary
    | Error msg -> Alcotest.failf "%s --version failed: %s" binary msg
    | Ok (exit_code, stdout, stderr) ->
        let combined = stdout ^ stderr in
        Alcotest.(check int)
          (Printf.sprintf "%s --version exit code" binary)
          0 exit_code;
        Alcotest.(check bool)
          (Printf.sprintf "%s --version produces output" binary)
          true
          (String.length (String.trim combined) > 0)

(* --- Tier 1: runner_available consistency --- *)

let test_runner_available_consistency () =
  List.iter
    (fun runner ->
      let binary = runner_bin runner in
      let cmd_exists = Background_task.command_exists binary in
      let available = Background_task.runner_available runner in
      Alcotest.(check bool)
        (Printf.sprintf "runner_available %s matches command_exists %s"
           (runner_name runner) binary)
        cmd_exists available)
    all_external_runners;
  (* Local is always available *)
  Alcotest.(check bool)
    "Local runner always available" true
    (Background_task.runner_available Background_task.Local)

(* --- Tier 1: resolve_runner picks available --- *)

let test_resolve_runner_consistency () =
  let any_available =
    List.exists
      (fun r -> Background_task.runner_available r)
      all_external_runners
  in
  match Background_task.resolve_runner () with
  | Ok (runner, _model) ->
      Alcotest.(check bool)
        "resolved runner is available" true
        (Background_task.runner_available runner);
      Alcotest.(check bool)
        "at least one runner is available" true any_available
  | Error _ ->
      Alcotest.(check bool)
        "no runner available matches error" false any_available

(* --- Tier 2: Fresh invocation --- *)

let with_temp_git_repo f =
  let dir = Filename.temp_dir "clawq-runner-integ" "" in
  Fun.protect
    (fun () ->
      ignore
        (Sys.command
           (Printf.sprintf
              "cd %s && git init -q && git config user.email 'test@test' && \
               git config user.name 'test' && git commit --allow-empty -m init \
               -q"
              (Filename.quote dir)));
      f dir)
    ~finally:(fun () -> Test_helpers.rm_tree dir)

let make_fresh_invoke_test (runner : Background_task.runner) () =
  if not (Background_task.runner_available runner) then Alcotest.skip ()
  else
    with_temp_git_repo (fun dir ->
        let rf_runner =
          match runner with
          | Background_task.Codex -> Runner_framework.Codex
          | Background_task.Claude -> Runner_framework.Claude
          | Background_task.Kimi -> Runner_framework.Kimi
          | Background_task.Gemini -> Runner_framework.Gemini
          | Background_task.Opencode -> Runner_framework.Opencode
          | Background_task.Cursor -> Runner_framework.Cursor
          | Background_task.Local -> Alcotest.fail "unexpected Local"
        in
        let def = Runner_framework.runner_def_of_runner rf_runner in
        let result =
          Runner_framework.build_command_for ~model:None
            ~prompt:"Respond with only the word hello" ~runner_session_id:None
            def Runner_framework.Fresh
        in
        let env = Unix.environment () in
        let proc =
          Process_group.start ~cwd:dir ~env (Process_group.Exec result.argv)
        in
        let outcome =
          lwt_run (fun () ->
              let open Lwt.Syntax in
              let timed_out = ref false in
              Lwt.finalize
                (fun () ->
                  let run_branch =
                    let* stdout, stderr =
                      Lwt.both (read_channel proc.stdout)
                        (read_channel proc.stderr)
                    in
                    let* status = Process_group.wait proc.pid in
                    if !timed_out then Lwt.return (Error "timeout")
                    else
                      let exit_code =
                        Background_task.exit_code_of_status status
                      in
                      Lwt.return (Ok (exit_code, stdout, stderr))
                  in
                  let timeout_branch =
                    let* () = Lwt_unix.sleep 90.0 in
                    timed_out := true;
                    let* () = Process_group.terminate proc.pid in
                    Lwt.return (Error "timeout")
                  in
                  Lwt.pick [ run_branch; timeout_branch ])
                (fun () -> Process_group.close proc))
        in
        match outcome with
        | Error "timeout" ->
            Alcotest.failf "%s fresh invoke timed out after 90s"
              (runner_name runner)
        | Error msg ->
            Alcotest.failf "%s fresh invoke error: %s" (runner_name runner) msg
        | Ok (_exit_code, stdout, stderr) ->
            let combined = stdout ^ stderr in
            if is_auth_error combined then Alcotest.skip ()
            else
              Alcotest.(check bool)
                (Printf.sprintf "%s fresh invoke produces output"
                   (runner_name runner))
                true
                (String.length (String.trim combined) > 0))

(* --- Tier 2: Codex session extraction --- *)

let test_codex_session_extraction () =
  if not (Background_task.runner_available Background_task.Codex) then
    Alcotest.skip ()
  else
    with_temp_git_repo (fun dir ->
        let def =
          Runner_framework.runner_def_of_runner Runner_framework.Codex
        in
        let result =
          Runner_framework.build_command_for ~model:None
            ~prompt:"Respond with only the word hello" ~runner_session_id:None
            def Runner_framework.Fresh
        in
        let env = Unix.environment () in
        let proc =
          Process_group.start ~cwd:dir ~env (Process_group.Exec result.argv)
        in
        let outcome =
          lwt_run (fun () ->
              let open Lwt.Syntax in
              let timed_out = ref false in
              Lwt.finalize
                (fun () ->
                  let run_branch =
                    let* stdout, stderr =
                      Lwt.both (read_channel proc.stdout)
                        (read_channel proc.stderr)
                    in
                    let* status = Process_group.wait proc.pid in
                    if !timed_out then Lwt.return (Error "timeout")
                    else
                      let exit_code =
                        Background_task.exit_code_of_status status
                      in
                      Lwt.return (Ok (exit_code, stdout, stderr))
                  in
                  let timeout_branch =
                    let* () = Lwt_unix.sleep 90.0 in
                    timed_out := true;
                    let* () = Process_group.terminate proc.pid in
                    Lwt.return (Error "timeout")
                  in
                  Lwt.pick [ run_branch; timeout_branch ])
                (fun () -> Process_group.close proc))
        in
        match outcome with
        | Error "timeout" -> Alcotest.fail "codex session extraction timed out"
        | Error msg -> Alcotest.failf "codex session extraction error: %s" msg
        | Ok (_, stdout, stderr) ->
            let combined = stdout ^ stderr in
            if is_auth_error combined then Alcotest.skip ()
            else
              let session_id =
                Runner_framework.extract_session_id def combined
              in
              Alcotest.(check bool)
                "codex session ID extracted from JSONL" true
                (Option.is_some session_id))

(* --- Tier 2: Claude session pre-generation --- *)

let test_claude_session_pre_gen () =
  let def = Runner_framework.runner_def_of_runner Runner_framework.Claude in
  let pre_id = Runner_framework.pre_generate_session_id def in
  Alcotest.(check bool)
    "Claude pre-generates a session UUID" true (Option.is_some pre_id);
  match pre_id with
  | None -> ()
  | Some uuid ->
      Alcotest.(check bool) "UUID is non-empty" true (String.length uuid > 0);
      (* UUID v4 format: 8-4-4-4-12 hex = 36 chars total *)
      Alcotest.(check int) "UUID length is 36" 36 (String.length uuid);
      let parts = String.split_on_char '-' uuid in
      let lengths = List.map String.length parts in
      Alcotest.(check (list int))
        "UUID segment lengths" [ 8; 4; 4; 4; 12 ] lengths;
      let is_hex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') in
      let hex_only s = String.to_seq s |> Seq.for_all is_hex in
      Alcotest.(check bool)
        "all segments are hex" true
        (List.for_all hex_only parts)

(* --- Tier 3: Local runner lifecycle --- *)

let test_local_runner_lifecycle () =
  let dir = Filename.temp_dir "clawq-bg-local-integ" "" in
  Fun.protect
    (fun () ->
      let db = Memory.init ~db_path:":memory:" () in
      Background_task.init_schema db;
      let id =
        match
          Background_task.enqueue ~db ~runner:Background_task.Local
            ~require_git:false ~use_worktree:false ~repo_path:dir
            ~prompt:"integration test prompt" ()
        with
        | Ok id -> id
        | Error msg -> Alcotest.fail msg
      in
      let task =
        match Background_task.get_task ~db ~id with
        | Some t -> t
        | None -> Alcotest.failf "expected task %d" id
      in
      Lwt_main.run
        (let open Lwt.Syntax in
         Background_task.spawn_local_task
           ~run_turn:(fun
               ~key:_
               ~message:_
               ?model:_
               ?agent_name:_
               ?cwd:_
               ~interrupt_check:_
               ~on_history_update:_
               ()
             -> Lwt.return "integration test done")
           ~on_task_started:(fun _ -> Lwt.return_unit)
           ~on_task_finished:(fun _ -> Lwt.return_unit)
           ~db task;
         let rec wait n =
           if n <= 0 then Lwt.return_unit
           else
             let* () = Lwt_unix.sleep 0.05 in
             match Background_task.get_task ~db ~id with
             | Some t when Background_task.is_terminal_status t.status ->
                 Lwt.return_unit
             | _ -> wait (n - 1)
         in
         wait 20);
      match Background_task.get_task ~db ~id with
      | None -> Alcotest.fail "expected task after spawn"
      | Some t ->
          Alcotest.(check string)
            "status is succeeded" "succeeded"
            (Background_task.string_of_status t.status);
          let preview = Option.value ~default:"" t.result_preview in
          Alcotest.(check bool)
            "result contains response" true
            (Test_helpers.string_contains preview "integration test done"))
    ~finally:(fun () -> Test_helpers.rm_tree dir)

(* --- Tier 3: Local runner on_task_finished fires --- *)

let test_local_runner_callback () =
  let dir = Filename.temp_dir "clawq-bg-local-cb" "" in
  Fun.protect
    (fun () ->
      let db = Memory.init ~db_path:":memory:" () in
      Background_task.init_schema db;
      let id =
        match
          Background_task.enqueue ~db ~runner:Background_task.Local
            ~require_git:false ~use_worktree:false ~repo_path:dir
            ~prompt:"callback test" ()
        with
        | Ok id -> id
        | Error msg -> Alcotest.fail msg
      in
      let task =
        match Background_task.get_task ~db ~id with
        | Some t -> t
        | None -> Alcotest.failf "expected task %d" id
      in
      let callback_fired = ref false in
      let callback_task_id = ref (-1) in
      Lwt_main.run
        (let open Lwt.Syntax in
         Background_task.spawn_local_task
           ~run_turn:(fun
               ~key:_
               ~message:_
               ?model:_
               ?agent_name:_
               ?cwd:_
               ~interrupt_check:_
               ~on_history_update:_
               ()
             -> Lwt.return "ok")
           ~on_task_started:(fun _ -> Lwt.return_unit)
           ~on_task_finished:(fun (t : Background_task.task) ->
             callback_fired := true;
             callback_task_id := t.id;
             Lwt.return_unit)
           ~db task;
         let rec wait n =
           if n <= 0 then Lwt.return_unit
           else
             let* () = Lwt_unix.sleep 0.05 in
             match Background_task.get_task ~db ~id with
             | Some t when Background_task.is_terminal_status t.status ->
                 Lwt.return_unit
             | _ -> wait (n - 1)
         in
         wait 20);
      Alcotest.(check bool)
        "on_task_finished callback fired" true !callback_fired;
      Alcotest.(check int)
        "callback received correct task id" id !callback_task_id)
    ~finally:(fun () -> Test_helpers.rm_tree dir)

(* --- Tier 3: Local runner timeout --- *)

let test_local_runner_timeout () =
  let dir = Filename.temp_dir "clawq-bg-local-to" "" in
  Fun.protect
    (fun () ->
      let db = Memory.init ~db_path:":memory:" () in
      Background_task.init_schema db;
      let id =
        match
          Background_task.enqueue ~db ~runner:Background_task.Local
            ~require_git:false ~use_worktree:false ~repo_path:dir
            ~prompt:"timeout test" ()
        with
        | Ok id -> id
        | Error msg -> Alcotest.fail msg
      in
      let task =
        match Background_task.get_task ~db ~id with
        | Some t -> t
        | None -> Alcotest.failf "expected task %d" id
      in
      Lwt_main.run
        (let open Lwt.Syntax in
         Background_task.spawn_local_task ~timeout_seconds:0.1
           ~run_turn:(fun
               ~key:_
               ~message:_
               ?model:_
               ?agent_name:_
               ?cwd:_
               ~interrupt_check:_
               ~on_history_update:_
               ()
             ->
             let* () = Lwt_unix.sleep 10.0 in
             Lwt.return "should not reach")
           ~on_task_started:(fun _ -> Lwt.return_unit)
           ~on_task_finished:(fun _ -> Lwt.return_unit)
           ~db task;
         Lwt_unix.sleep 0.5);
      match Background_task.get_task ~db ~id with
      | None -> Alcotest.fail "expected task after timeout"
      | Some t ->
          Alcotest.(check string)
            "status is failed" "failed"
            (Background_task.string_of_status t.status);
          let preview = Option.value ~default:"" t.result_preview in
          Alcotest.(check bool)
            "result mentions timeout" true
            (Test_helpers.string_contains preview "timed out"))
    ~finally:(fun () -> Test_helpers.rm_tree dir)

(* --- Tier 4: Production spawn path (default_spawn_task) --- *)

let make_spawn_task_test (runner : Background_task.runner) () =
  if not (Background_task.runner_available runner) then Alcotest.skip ()
  else
    Test_helpers.with_temp_home (fun _home ->
        with_temp_git_repo (fun dir ->
            let db = Memory.init ~db_path:":memory:" () in
            Background_task.init_schema db;
            let id =
              match
                Background_task.enqueue ~db ~runner ~require_git:true
                  ~use_worktree:false ~repo_path:dir
                  ~prompt:"Respond with only the word hello" ()
              with
              | Ok id -> id
              | Error msg -> Alcotest.failf "enqueue failed: %s" msg
            in
            let task =
              match Background_task.get_task ~db ~id with
              | Some t -> t
              | None -> Alcotest.failf "expected task %d" id
            in
            let finished = ref false in
            let finished_task = ref None in
            Lwt_main.run
              (let open Lwt.Syntax in
               Background_task.default_spawn_task
                 ~on_task_started:(fun _ -> Lwt.return_unit)
                 ~on_task_finished:(fun (t : Background_task.task) ->
                   finished := true;
                   finished_task := Some t;
                   Lwt.return_unit)
                 ~db task;
               (* Wait up to 90s for the task to finish *)
               let rec wait n =
                 if n <= 0 then Lwt.return_unit
                 else
                   let* () = Lwt_unix.sleep 1.0 in
                   match Background_task.get_task ~db ~id with
                   | Some t when Background_task.is_terminal_status t.status ->
                       (* Give 2.5s for the B210 grandchild cleanup watchdog *)
                       let* () = Lwt_unix.sleep 2.5 in
                       Lwt.return_unit
                   | _ -> wait (n - 1)
               in
               wait 90);
            let t =
              match Background_task.get_task ~db ~id with
              | Some t -> t
              | None -> Alcotest.failf "task %d not found after spawn" id
            in
            (* Check for auth errors in the result preview *)
            let preview = Option.value ~default:"" t.result_preview in
            if is_auth_error preview then Alcotest.skip ()
            else begin
              (* Verify task reached a terminal status *)
              Alcotest.(check bool)
                "task reached terminal status" true
                (Background_task.is_terminal_status t.status);
              (* Verify the task succeeded *)
              Alcotest.(check string)
                "task status is succeeded" "succeeded"
                (Background_task.string_of_status t.status);
              (* Verify log file was created and has content *)
              (match t.log_path with
              | Some log_path ->
                  Alcotest.(check bool)
                    "log file exists" true (Sys.file_exists log_path);
                  let log_content =
                    Background_task.read_log_tail log_path 4096
                  in
                  Alcotest.(check bool)
                    "log file has content" true
                    (String.length (String.trim log_content) > 0)
              | None -> Alcotest.fail "expected log_path to be set");
              (* Verify result preview is non-empty *)
              Alcotest.(check bool)
                "result_preview non-empty" true
                (String.length (String.trim preview) > 0);
              (* Verify on_task_finished callback fired *)
              Alcotest.(check bool)
                "on_task_finished callback fired" true !finished;
              (* After finish, pid is cleared to NULL by design *)
              Alcotest.(check bool)
                "pid cleared after completion" true (t.pid = None)
            end))

(* --- Suite --- *)

let suite =
  let version_tests =
    List.map
      (fun runner ->
        Alcotest.test_case
          (Printf.sprintf "%s version" (runner_name runner))
          `Slow (make_version_test runner))
      all_external_runners
  in
  let fresh_invoke_tests =
    List.map
      (fun runner ->
        Alcotest.test_case
          (Printf.sprintf "%s fresh invoke" (runner_name runner))
          `Slow
          (make_fresh_invoke_test runner))
      all_external_runners
  in
  let spawn_task_tests =
    List.map
      (fun runner ->
        Alcotest.test_case
          (Printf.sprintf "%s spawn_task" (runner_name runner))
          `Slow
          (make_spawn_task_test runner))
      all_external_runners
  in
  version_tests @ fresh_invoke_tests @ spawn_task_tests
  @ [
      Alcotest.test_case "codex session extraction" `Slow
        test_codex_session_extraction;
      Alcotest.test_case "claude session pre-gen" `Slow
        test_claude_session_pre_gen;
      Alcotest.test_case "local runner lifecycle" `Slow
        test_local_runner_lifecycle;
      Alcotest.test_case "local runner on_task_finished fires" `Slow
        test_local_runner_callback;
      Alcotest.test_case "local runner timeout" `Slow test_local_runner_timeout;
      Alcotest.test_case "runner_available consistency" `Slow
        test_runner_available_consistency;
      Alcotest.test_case "resolve_runner picks available" `Slow
        test_resolve_runner_consistency;
    ]
