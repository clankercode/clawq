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
  let run_command ~cwd ~argv ~send_progress =
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
  let run_command ~cwd ~argv ~send_progress =
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
  Alcotest.(check string)
    "build failure result" "Build failed (exit 2). Restart aborted." result;
  Alcotest.(check bool) "no restart signal" false !signaled;
  Alcotest.(check bool)
    "build failure reported" true
    (List.mem "Build failed (exit 2). Restart aborted." !progress);
  Alcotest.(check (list (pair string (list string))))
    "commands still ran in order"
    [ ("/repo", [ "git"; "pull" ]); ("/repo", [ "make"; "build" ]) ]
    (List.rev !commands)

let test_run_update_rejects_when_draining () =
  let ran = ref false in
  let progress = ref [] in
  let result =
    Lwt_main.run
      (Update_tool.run_update
         ~find_repo_root:(fun ?start_path:_ ?exists:_ () -> Some "/repo")
         ~run_command:(fun ~cwd:_ ~argv:_ ~send_progress:_ ->
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
         ~run_command:(fun ~cwd:_ ~argv:_ ~send_progress:_ ->
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
  let run_command ~cwd ~argv ~send_progress =
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
          ~run_command:(fun ~cwd:_ ~argv:_ ~send_progress:_ -> Lwt.return 0)
          ~send_signal:(fun pid sig_ -> sent_signal := Some (pid, sig_))
          ())
         .Tool.invoke
         ~context:
           { Tool.session_key = Some "telegram:123:456"; send_progress = None }
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
         ~run_command:(fun ~cwd:_ ~argv:_ ~send_progress:_ -> Lwt.return 0)
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

let test_run_update_aborts_when_prepare_restart_fails () =
  let progress = ref [] in
  let signaled = ref false in
  let result =
    Lwt_main.run
      (Update_tool.run_update
         ~find_repo_root:(fun ?start_path:_ ?exists:_ () -> Some "/repo")
         ~run_command:(fun ~cwd:_ ~argv:_ ~send_progress:_ -> Lwt.return 0)
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

let suite =
  [
    Alcotest.test_case "find repo root returns none when missing" `Quick
      test_find_repo_root_returns_none_when_missing;
    Alcotest.test_case "run update continues after git failure" `Quick
      test_run_update_continues_after_git_failure;
    Alcotest.test_case "run update aborts on build failure" `Quick
      test_run_update_aborts_on_build_failure;
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
    Alcotest.test_case "run update aborts when prepare restart fails" `Quick
      test_run_update_aborts_when_prepare_restart_fails;
  ]
