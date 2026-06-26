let test_find_repo_root_returns_none_when_missing () =
  let result =
    Update_tool.find_repo_root ~start_path:"/tmp/not-a-repo/bin/clawq"
      ~exists:(fun _ -> false)
      ()
  in
  Alcotest.(check (option string)) "no repo root" None result

let test_run_update_continues_after_git_failure () =
  let commands = ref [] in
  let progress = ref [] in
  let signaled = ref None in
  let run_command ~cwd ~argv ~send_progress ~interrupt_check:_ =
    let open Lwt.Syntax in
    commands := (cwd, Array.to_list argv) :: !commands;
    let* () = send_progress (String.concat " " (Array.to_list argv)) in
    match Array.to_list argv with
    | [ "git"; "pull" ] -> Lwt.return 1
    | [ "make"; "build" ] -> Lwt.return 0
    | _ -> Lwt.return 99
  in
  let result =
    Lwt_main.run
      (Update_tool.run_update
         ~find_repo_root:(fun ?start_path:_ ?exists:_ () -> Some "/repo")
         ~run_command
         ~send_signal:(fun pid signal -> signaled := Some (pid, signal))
         ~is_draining:(fun () -> false)
         ~send_progress:(fun text ->
           progress := text :: !progress;
           Lwt.return_unit)
         ())
  in
  Alcotest.(check string)
    "success result" "Build complete. Sending restart signal..." result;
  Alcotest.(check bool)
    "final status not duplicated in progress" false
    (List.mem "Build complete. Sending restart signal..." !progress);
  Alcotest.(check (list (pair string (list string))))
    "git then make"
    [ ("/repo", [ "git"; "pull" ]); ("/repo", [ "make"; "build" ]) ]
    (List.rev !commands);
  Alcotest.(check bool)
    "git failure reported" true
    (List.mem "git pull failed (exit 1), continuing to build." !progress);
  Alcotest.(check (option (pair int int)))
    "restart signal sent"
    (Some (Unix.getpid (), Sys.sigusr1))
    !signaled

let test_run_update_aborts_on_build_failure () =
  let commands = ref [] in
  let progress = ref [] in
  let signaled = ref false in
  let run_command ~cwd ~argv ~send_progress ~interrupt_check:_ =
    let open Lwt.Syntax in
    commands := (cwd, Array.to_list argv) :: !commands;
    let* () = send_progress (String.concat " " (Array.to_list argv)) in
    match Array.to_list argv with
    | [ "git"; "pull" ] -> Lwt.return 0
    | [ "make"; "build" ] -> Lwt.return 2
    | _ -> Lwt.return 99
  in
  let result =
    Lwt_main.run
      (Update_tool.run_update
         ~find_repo_root:(fun ?start_path:_ ?exists:_ () -> Some "/repo")
         ~run_command
         ~send_signal:(fun _ _ -> signaled := true)
         ~is_draining:(fun () -> false)
         ~send_progress:(fun text ->
           progress := text :: !progress;
           Lwt.return_unit)
         ())
  in
  let expected_msg =
    "Build failed (exit 2). Restart aborted. Most relevant detail: Starting \
     update..."
  in
  Alcotest.(check string) "build failure result" expected_msg result;
  Alcotest.(check bool) "no restart signal" false !signaled;
  Alcotest.(check bool)
    "build failure reported" true
    (List.mem expected_msg !progress);
  Alcotest.(check (list (pair string (list string))))
    "commands still ran in order"
    [ ("/repo", [ "git"; "pull" ]); ("/repo", [ "make"; "build" ]) ]
    (List.rev !commands)

let test_run_update_reports_dune_lock_detail () =
  let progress = ref [] in
  let result =
    Lwt_main.run
      (Update_tool.run_update
         ~find_repo_root:(fun ?start_path:_ ?exists:_ () -> Some "/repo")
         ~run_command:(fun ~cwd:_ ~argv ~send_progress ~interrupt_check:_ ->
           let open Lwt.Syntax in
           match Array.to_list argv with
           | [ "git"; "pull" ] ->
               let* () = send_progress "Already up to date." in
               Lwt.return 0
           | [ "make"; "build" ] ->
               let* () =
                 send_progress "ERROR: Dune build lock present at _build/.lock"
               in
               let* () =
                 send_progress
                   "Another dune command may already be running in this repo \
                    build dir."
               in
               Lwt.return 2
           | _ -> Lwt.return 99)
         ~is_draining:(fun () -> false)
         ~send_progress:(fun text ->
           progress := text :: !progress;
           Lwt.return_unit)
         ())
  in
  Alcotest.(check bool)
    "mentions dune lock" true
    (String.contains result 'D'
    && String.length result > 0
    &&
      try
        ignore
          (Str.search_forward
             (Str.regexp_string "Dune lock contention")
             result 0);
        true
      with Not_found -> false);
  Alcotest.(check bool)
    "progress includes final message" true
    (List.exists (fun s -> String.length s > 0 && s = result) !progress)

let test_run_update_rejects_when_draining () =
  let ran = ref false in
  let progress = ref [] in
  let result =
    Lwt_main.run
      (Update_tool.run_update
         ~find_repo_root:(fun ?start_path:_ ?exists:_ () -> Some "/repo")
         ~run_command:(fun ~cwd:_ ~argv:_ ~send_progress:_ ~interrupt_check:_ ->
           ran := true;
           Lwt.return 0)
         ~is_draining:(fun () -> true)
         ~send_progress:(fun text ->
           progress := text :: !progress;
           Lwt.return_unit)
         ())
  in
  Alcotest.(check string)
    "draining result" "Restart already in progress, please wait." result;
  Alcotest.(check bool) "no command ran" false !ran;
  Alcotest.(check (list string))
    "draining progress"
    [ "Restart already in progress, please wait." ]
    (List.rev !progress)

let test_run_update_rejects_when_claimed () =
  let ran = ref false in
  let progress = ref [] in
  let result =
    Lwt_main.run
      (Update_tool.run_update
         ~find_repo_root:(fun ?start_path:_ ?exists:_ () -> Some "/repo")
         ~run_command:(fun ~cwd:_ ~argv:_ ~send_progress:_ ~interrupt_check:_ ->
           ran := true;
           Lwt.return 0)
         ~claim_update:(fun () -> Lwt.return false)
         ~is_draining:(fun () -> false)
         ~send_progress:(fun text ->
           progress := text :: !progress;
           Lwt.return_unit)
         ())
  in
  Alcotest.(check string)
    "claimed result" "Restart already in progress, please wait." result;
  Alcotest.(check bool) "no command ran" false !ran;
  Alcotest.(check (list string))
    "claimed progress"
    [ "Restart already in progress, please wait." ]
    (List.rev !progress)

let test_run_update_uses_binary_mode_when_repo_missing () =
  let commands = ref [] in
  let progress = ref [] in
  let signaled = ref None in
  let run_command ~cwd ~argv ~send_progress ~interrupt_check:_ =
    let open Lwt.Syntax in
    commands := (cwd, Array.to_list argv) :: !commands;
    let* () = send_progress (String.concat " " (Array.to_list argv)) in
    Lwt.return 0
  in
  let result =
    Lwt_main.run
      (Update_tool.run_update ~start_path:"/opt/clawq/bin/clawq"
         ~find_repo_root:(fun ?start_path:_ ?exists:_ () -> None)
         ~binary_url:(Some "https://example.invalid/clawq-linux") ~run_command
         ~send_signal:(fun pid signal -> signaled := Some (pid, signal))
         ~is_draining:(fun () -> false)
         ~send_progress:(fun text ->
           progress := text :: !progress;
           Lwt.return_unit)
         ())
  in
  Alcotest.(check string)
    "binary result" "Binary update complete. Sending restart signal..." result;
  Alcotest.(check (list (pair string (list string))))
    "download chmod mv"
    [
      ( "/opt/clawq/bin",
        [
          "curl";
          "-fL";
          "-o";
          "/opt/clawq/bin/clawq.download";
          "https://example.invalid/clawq-linux";
        ] );
      ("/opt/clawq/bin", [ "chmod"; "755"; "/opt/clawq/bin/clawq.download" ]);
      ( "/opt/clawq/bin",
        [ "mv"; "/opt/clawq/bin/clawq.download"; "/opt/clawq/bin/clawq" ] );
    ]
    (List.rev !commands);
  Alcotest.(check bool) "mode progress" true (List.mem "Mode: binary" !progress);
  Alcotest.(check (option (pair int int)))
    "restart signal sent"
    (Some (Unix.getpid (), Sys.sigusr1))
    !signaled

let test_run_update_binary_mode_requires_url () =
  let progress = ref [] in
  let result =
    Lwt_main.run
      (Update_tool.run_update ~mode:Update_tool.Binary
         ~find_repo_root:(fun ?start_path:_ ?exists:_ () -> None)
         ~binary_url:None
         ~is_draining:(fun () -> false)
         ~send_progress:(fun text ->
           progress := text :: !progress;
           Lwt.return_unit)
         ())
  in
  Alcotest.(check string)
    "binary url required"
    "Binary update mode requires CLAWQ_UPDATE_BINARY_URL to be set." result;
  Alcotest.(check (list string))
    "progress contains error"
    [ "Binary update mode requires CLAWQ_UPDATE_BINARY_URL to be set." ]
    (List.rev !progress)

let test_update_tool_writes_restart_marker_from_session_context () =
  let sent_signal = ref None in
  Restart_notify.remove ();
  let result =
    Lwt_main.run
      ((Update_tool.tool
          ~is_draining:(fun () -> false)
          ~find_repo_root:(fun ?start_path:_ ?exists:_ () -> Some "/repo")
          ~run_command:(fun
              ~cwd:_ ~argv:_ ~send_progress:_ ~interrupt_check:_ ->
            Lwt.return 0)
          ~send_signal:(fun pid sig_ -> sent_signal := Some (pid, sig_))
          ())
         .Tool.invoke
         ~context:
           {
             Tool.session_key = Some "telegram:123:456";
             send_progress = None;
             interrupt_check = None;
             inject_system_messages = None;
             effective_cwd = None;
             request_cwd_change = None;
           }
         (`Assoc [ ("mode", `String "git") ]))
  in
  Alcotest.(check string)
    "update result" "Build complete. Sending restart signal..." result;
  Alcotest.(check (option (pair string string)))
    "restart marker"
    (Some ("telegram", "123"))
    (Restart_notify.read ());
  Restart_notify.remove ();
  ignore sent_signal

let test_run_update_prepares_restart_before_signal () =
  let prepared = ref false in
  let signaled = ref false in
  let progress = ref [] in
  let result =
    Lwt_main.run
      (Update_tool.run_update
         ~find_repo_root:(fun ?start_path:_ ?exists:_ () -> Some "/repo")
         ~run_command:(fun ~cwd:_ ~argv:_ ~send_progress:_ ~interrupt_check:_ ->
           Lwt.return 0)
         ~prepare_restart:(fun () ->
           prepared := true;
           Lwt.return (Ok ()))
         ~send_signal:(fun _ _ -> signaled := true)
         ~is_draining:(fun () -> false)
         ~send_progress:(fun text ->
           progress := text :: !progress;
           Lwt.return_unit)
         ())
  in
  Alcotest.(check string)
    "success result" "Build complete. Sending restart signal..." result;
  Alcotest.(check bool) "prepare called" true !prepared;
  Alcotest.(check bool)
    "prepare progress emitted" true
    (List.mem "Restart requested. Finishing this turn before handoff..."
       !progress);
  Alcotest.(check bool) "restart signal sent" true !signaled

let test_run_update_sets_reexec_path_to_fresh_git_build () =
  let previous = Sys.getenv_opt Restart_exec.reexec_path_env in
  let repo_root = Filename.temp_file "clawq_update_repo" "" in
  Sys.remove repo_root;
  Unix.mkdir repo_root 0o755;
  let build_dir = Filename.concat repo_root "_build" in
  let default_dir = Filename.concat build_dir "default" in
  let src_dir = Filename.concat default_dir "src" in
  Unix.mkdir build_dir 0o755;
  Unix.mkdir default_dir 0o755;
  Unix.mkdir src_dir 0o755;
  let fresh_binary = Filename.concat src_dir "main.exe" in
  let oc = open_out fresh_binary in
  output_string oc "";
  close_out oc;
  Unix.putenv Restart_exec.reexec_path_env "";
  Fun.protect
    (fun () ->
      let signaled = ref false in
      let progress = ref [] in
      let send_progress text =
        progress := text :: !progress;
        Lwt.return_unit
      in
      let result =
        Lwt_main.run
          (Update_tool.run_update
             ~find_repo_root:(fun ?start_path:_ ?exists:_ () -> Some repo_root)
             ~run_command:(fun
                 ~cwd:_ ~argv:_ ~send_progress:_ ~interrupt_check:_ ->
               Lwt.return 0)
             ~send_signal:(fun _ _ -> signaled := true)
             ~is_draining:(fun () -> false)
             ~send_progress ())
      in
      Alcotest.(check string)
        "success result" "Build complete. Sending restart signal..." result;
      Alcotest.(check bool) "restart signal sent" true !signaled;
      Alcotest.(check (option string))
        "fresh restart path set" (Some fresh_binary)
        (Sys.getenv_opt Restart_exec.reexec_path_env))
    ~finally:(fun () ->
      match previous with
      | Some value -> Unix.putenv Restart_exec.reexec_path_env value
      | None -> Unix.putenv Restart_exec.reexec_path_env "")

let test_run_update_aborts_when_prepare_restart_fails () =
  let progress = ref [] in
  let signaled = ref false in
  let result =
    Lwt_main.run
      (Update_tool.run_update
         ~find_repo_root:(fun ?start_path:_ ?exists:_ () -> Some "/repo")
         ~run_command:(fun ~cwd:_ ~argv:_ ~send_progress:_ ~interrupt_check:_ ->
           Lwt.return 0)
         ~prepare_restart:(fun () ->
           Lwt.return
             (Error
                "Failed to acknowledge Telegram update 42 before restart. \
                 Restart aborted."))
         ~send_signal:(fun _ _ -> signaled := true)
         ~is_draining:(fun () -> false)
         ~send_progress:(fun text ->
           progress := text :: !progress;
           Lwt.return_unit)
         ())
  in
  Alcotest.(check string)
    "prepare failure result"
    "Failed to acknowledge Telegram update 42 before restart. Restart aborted."
    result;
  Alcotest.(check bool) "restart signal suppressed" false !signaled;
  Alcotest.(check bool)
    "prepare progress emitted" true
    (List.mem "Restart requested. Finishing this turn before handoff..."
       !progress);
  Alcotest.(check bool)
    "prepare failure reported" true
    (List.mem
       "Failed to acknowledge Telegram update 42 before restart. Restart \
        aborted."
       !progress)

let test_run_update_interrupts_running_command () =
  let interrupted = ref None in
  let progress = ref [] in
  let started_at = Unix.gettimeofday () in
  let result =
    Lwt_main.run
      (let open Lwt.Syntax in
       let trigger =
         let* () = Lwt_unix.sleep 0.1 in
         interrupted := Some "stop now";
         Lwt.return_unit
       in
       let run_command ~cwd:_ ~argv:_ ~send_progress ~interrupt_check =
         let open Lwt.Syntax in
         let* () = send_progress "simulated long-running command" in
         let rec loop () =
           match interrupt_check with
           | Some check -> (
               match check () with
               | Some reason when reason <> Agent.queued_message_interrupt_token
                 ->
                   Lwt.fail Update_tool.Interrupted_by_user
               | _ ->
                   let* () = Lwt_unix.sleep 0.05 in
                   loop ())
           | None ->
               let* () = Lwt_unix.sleep 10.0 in
               Lwt.return 0
         in
         loop ()
       in
       let run =
         Update_tool.run_update
           ~find_repo_root:(fun ?start_path:_ ?exists:_ () -> Some "/repo")
           ~run_command
           ~is_draining:(fun () -> false)
           ~send_progress:(fun text ->
             progress := text :: !progress;
             Lwt.return_unit)
           ~interrupt_check:(Some (fun () -> !interrupted))
           ()
       in
       let* result, () = Lwt.both run trigger in
       Lwt.return result)
  in
  let elapsed = Unix.gettimeofday () -. started_at in
  Alcotest.(check string)
    "interrupt result" "Update interrupted by user." result;
  Alcotest.(check bool) "returns promptly" true (elapsed < 2.0);
  Alcotest.(check bool)
    "progress started" true
    (List.mem "Starting update..." !progress)

let test_stream_process_interrupt_kills_descendants () =
  let interrupted = ref None in
  let workspace = Filename.get_temp_dir_name () in
  let pid_file = Filename.concat workspace "update-child.pid" in
  let argv =
    [|
      "sh";
      "-c";
      Printf.sprintf
        "sleep 10 & child=$!; printf \"%%s\" \"$child\" > %s; wait $child"
        (Filename.quote pid_file);
    |]
  in
  let result =
    Lwt_main.run
      (let open Lwt.Syntax in
       let trigger =
         let rec wait_for_pid_file attempts =
           if Sys.file_exists pid_file then Lwt.return_unit
           else if attempts <= 0 then Lwt.fail_with "child pid file not written"
           else
             let* () = Lwt_unix.sleep 0.02 in
             wait_for_pid_file (attempts - 1)
         in
         let* () = wait_for_pid_file 50 in
         interrupted := Some "stop now";
         Lwt.return_unit
       in
       let run =
         Lwt.catch
           (fun () ->
             let* _ =
               Update_tool.stream_process ~cwd:workspace ~argv
                 ~send_progress:(fun _ -> Lwt.return_unit)
                 ~interrupt_check:(Some (fun () -> !interrupted))
             in
             Lwt.return "completed")
           (function
             | Update_tool.Interrupted_by_user -> Lwt.return "interrupted"
             | exn -> Lwt.fail exn)
       in
       let* result, () = Lwt.both run trigger in
       Lwt.return result)
  in
  let child_pid =
    let ic = open_in pid_file in
    Fun.protect
      (fun () -> int_of_string (input_line ic))
      ~finally:(fun () -> close_in ic)
  in
  let rec wait_until_gone attempts =
    if attempts <= 0 || not (Test_helpers.process_exists child_pid) then ()
    else begin
      Unix.sleepf 0.05;
      wait_until_gone (attempts - 1)
    end
  in
  wait_until_gone 20;
  Alcotest.(check string) "interrupt result" "interrupted" result;
  Alcotest.(check bool)
    "child process terminated" false (Test_helpers.process_exists child_pid);
  Sys.remove pid_file

let test_progress_sender_renders_checklist () =
  let messages = ref [] in
  let edits = ref [] in
  let send_first text =
    messages := text :: !messages;
    Lwt.return "msg-1"
  in
  let edit msg_id text =
    edits := (msg_id, text) :: !edits;
    Lwt.return_unit
  in
  let send_progress, get_final =
    Update_tool.make_progress_sender ~send_first ~edit ~throttle:0.0
      ~mode:Update_tool.Auto ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = send_progress "Starting update..." in
     let* () = send_progress "Mode: git" in
     let* () = send_progress "Running: git pull" in
     let* () = send_progress "Already up to date." in
     let* () = send_progress "Running: make build" in
     let* () = send_progress "Build complete. Sending restart signal..." in
     Lwt.return_unit);
  (* Only one initial send *)
  Alcotest.(check int) "one send" 1 (List.length !messages);
  (* Multiple edits *)
  Alcotest.(check bool) "has edits" true (List.length !edits > 0);
  (* All edits to same msg id *)
  List.iter (fun (id, _) -> Alcotest.(check string) "edit id" "msg-1" id) !edits;
  (* Final text has checklist structure *)
  let final = get_final () in
  Alcotest.(check bool)
    "contains git pull" true
    (Update_tool.contains_sub final "git pull");
  Alcotest.(check bool)
    "contains make build" true
    (Update_tool.contains_sub final "make build");
  Alcotest.(check bool)
    "contains restart" true
    (Update_tool.contains_sub final "restart")

let test_progress_sender_handles_build_failure () =
  let messages = ref [] in
  let edits = ref [] in
  let send_first text =
    messages := text :: !messages;
    Lwt.return "msg-1"
  in
  let edit msg_id text =
    edits := (msg_id, text) :: !edits;
    Lwt.return_unit
  in
  let send_progress, get_final =
    Update_tool.make_progress_sender ~send_first ~edit ~throttle:0.0
      ~mode:Update_tool.Git ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = send_progress "Starting update..." in
     let* () = send_progress "Mode: git" in
     let* () = send_progress "Running: git pull" in
     let* () = send_progress "Running: make build" in
     let* () =
       send_progress "Build failed (exit 2). Restart aborted. Detail: error"
     in
     Lwt.return_unit);
  Alcotest.(check int) "one send" 1 (List.length !messages);
  let final = get_final () in
  Alcotest.(check bool)
    "contains failure marker" true
    (Update_tool.contains_sub final "Build failed")

let test_progress_sender_binary_mode () =
  let messages = ref [] in
  let edits = ref [] in
  let send_first text =
    messages := text :: !messages;
    Lwt.return "msg-1"
  in
  let edit msg_id text =
    edits := (msg_id, text) :: !edits;
    Lwt.return_unit
  in
  let send_progress, get_final =
    Update_tool.make_progress_sender ~send_first ~edit ~throttle:0.0
      ~mode:Update_tool.Binary ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = send_progress "Starting update..." in
     let* () = send_progress "Mode: binary" in
     let* () = send_progress "Running: curl -fL -o /tmp/clawq.download url" in
     let* () = send_progress "Running: chmod 755 /tmp/clawq.download" in
     let* () = send_progress "Running: mv /tmp/clawq.download /tmp/clawq" in
     let* () =
       send_progress "Binary update complete. Sending restart signal..."
     in
     Lwt.return_unit);
  Alcotest.(check int) "one send" 1 (List.length !messages);
  let final = get_final () in
  Alcotest.(check bool)
    "contains download" true
    (Update_tool.contains_sub final "download binary");
  Alcotest.(check bool)
    "contains permissions" true
    (Update_tool.contains_sub final "set permissions");
  Alcotest.(check bool)
    "contains replace" true
    (Update_tool.contains_sub final "replace executable");
  Alcotest.(check bool)
    "contains restart" true
    (Update_tool.contains_sub final "restart");
  Alcotest.(check bool)
    "mode is binary" true
    (Update_tool.contains_sub final "binary")

let test_offline_git_update_succeeds () =
  let progress = ref [] in
  let send_progress text =
    progress := text :: !progress;
    Lwt.return_unit
  in
  let result =
    Lwt_main.run
      (Update_tool.run_offline_update
         ~find_repo_root:(fun ?start_path:_ ?exists:_ () -> Some "/repo")
         ~run_command:(fun ~cwd:_ ~argv:_ ~send_progress:_ ~interrupt_check:_ ->
           Lwt.return 0)
         ~mode:Update_tool.Git ~send_progress ())
  in
  Alcotest.(check string)
    "adjusted git result"
    "Build complete. Next `clawq` invocation will use the updated version."
    result;
  Alcotest.(check bool)
    "offline warning present" true
    (List.mem "Note: no live daemon detected. Running update offline." !progress);
  Alcotest.(check bool) "mode progress" true (List.mem "Mode: git" !progress)

let test_offline_binary_update_succeeds () =
  let progress = ref [] in
  let send_progress text =
    progress := text :: !progress;
    Lwt.return_unit
  in
  let result =
    Lwt_main.run
      (Update_tool.run_offline_update
         ~find_repo_root:(fun ?start_path:_ ?exists:_ () -> None)
         ~run_command:(fun ~cwd:_ ~argv:_ ~send_progress:_ ~interrupt_check:_ ->
           Lwt.return 0)
         ~binary_url:(Some "https://example.invalid/clawq-linux")
         ~start_path:"/opt/clawq/bin/clawq" ~mode:Update_tool.Auto
         ~send_progress ())
  in
  Alcotest.(check string)
    "adjusted binary result"
    "Binary replaced. Next `clawq` invocation will use the updated version."
    result;
  Alcotest.(check bool)
    "offline warning present" true
    (List.mem "Note: no live daemon detected. Running update offline." !progress);
  Alcotest.(check bool) "mode progress" true (List.mem "Mode: binary" !progress)

let test_offline_update_build_failure () =
  let progress = ref [] in
  let send_progress text =
    progress := text :: !progress;
    Lwt.return_unit
  in
  let result =
    Lwt_main.run
      (Update_tool.run_offline_update
         ~find_repo_root:(fun ?start_path:_ ?exists:_ () -> Some "/repo")
         ~run_command:(fun ~cwd:_ ~argv ~send_progress:_ ~interrupt_check:_ ->
           match Array.to_list argv with
           | [ "git"; "pull" ] -> Lwt.return 0
           | [ "make"; "build" ] -> Lwt.return 2
           | _ -> Lwt.return 0)
         ~mode:Update_tool.Git ~send_progress ())
  in
  Alcotest.(check bool)
    "build failed in result" true
    (Update_tool.contains_sub result "Build failed");
  Alcotest.(check bool)
    "offline warning present" true
    (List.mem "Note: no live daemon detected. Running update offline." !progress)

let test_offline_update_no_repo_no_binary () =
  let progress = ref [] in
  let send_progress text =
    progress := text :: !progress;
    Lwt.return_unit
  in
  let result =
    Lwt_main.run
      (Update_tool.run_offline_update
         ~find_repo_root:(fun ?start_path:_ ?exists:_ () -> None)
         ~run_command:(fun ~cwd:_ ~argv:_ ~send_progress:_ ~interrupt_check:_ ->
           Lwt.return 0)
         ~binary_url:None ~mode:Update_tool.Auto ~send_progress ())
  in
  Alcotest.(check bool)
    "cannot find repo" true
    (Update_tool.contains_sub result "Cannot find repository root");
  Alcotest.(check bool)
    "offline warning present" true
    (List.mem "Note: no live daemon detected. Running update offline." !progress)

let suite =
  [
    Alcotest.test_case "find repo root returns none when missing" `Quick
      test_find_repo_root_returns_none_when_missing;
    Alcotest.test_case "run update continues after git failure" `Quick
      test_run_update_continues_after_git_failure;
    Alcotest.test_case "run update aborts on build failure" `Quick
      test_run_update_aborts_on_build_failure;
    Alcotest.test_case "run update reports dune lock detail" `Quick
      test_run_update_reports_dune_lock_detail;
    Alcotest.test_case "run update rejects when draining" `Quick
      test_run_update_rejects_when_draining;
    Alcotest.test_case "run update rejects when claimed" `Quick
      test_run_update_rejects_when_claimed;
    Alcotest.test_case "run update uses binary mode when repo missing" `Quick
      test_run_update_uses_binary_mode_when_repo_missing;
    Alcotest.test_case "run update binary mode requires url" `Quick
      test_run_update_binary_mode_requires_url;
    Alcotest.test_case "update tool writes restart marker from session context"
      `Quick test_update_tool_writes_restart_marker_from_session_context;
    Alcotest.test_case "run update prepares restart before signal" `Quick
      test_run_update_prepares_restart_before_signal;
    Alcotest.test_case "run update sets reexec path to fresh git build" `Quick
      test_run_update_sets_reexec_path_to_fresh_git_build;
    Alcotest.test_case "run update aborts when prepare restart fails" `Quick
      test_run_update_aborts_when_prepare_restart_fails;
    Alcotest.test_case "run update interrupts running command" `Quick
      test_run_update_interrupts_running_command;
    Alcotest.test_case "stream_process interrupt kills descendants" `Quick
      test_stream_process_interrupt_kills_descendants;
    Alcotest.test_case "progress sender renders checklist" `Quick
      test_progress_sender_renders_checklist;
    Alcotest.test_case "progress sender handles build failure" `Quick
      test_progress_sender_handles_build_failure;
    Alcotest.test_case "progress sender binary mode" `Quick
      test_progress_sender_binary_mode;
    Alcotest.test_case "offline git update succeeds" `Quick
      test_offline_git_update_succeeds;
    Alcotest.test_case "offline binary update succeeds" `Quick
      test_offline_binary_update_succeeds;
    Alcotest.test_case "offline update build failure" `Quick
      test_offline_update_build_failure;
    Alcotest.test_case "offline update no repo no binary" `Quick
      test_offline_update_no_repo_no_binary;
  ]
