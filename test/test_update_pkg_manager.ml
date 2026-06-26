let always_exists _ = true

let rec remove_tree path =
  if Sys.file_exists path then
    if Sys.is_directory path then begin
      Array.iter
        (fun name -> remove_tree (Filename.concat path name))
        (Sys.readdir path);
      Unix.rmdir path
    end
    else Sys.remove path

let mkdir_p path =
  let rec loop dir =
    if dir = "" || dir = Filename.dirname dir then ()
    else if Sys.file_exists dir then ()
    else begin
      loop (Filename.dirname dir);
      Unix.mkdir dir 0o755
    end
  in
  loop path

let write_empty_exe path =
  let oc = open_out path in
  close_out oc;
  Unix.chmod path 0o755

(* Detect with deterministic inputs: realpath is identity unless overridden, and
   the CLI is assumed present unless overridden, so tests exercise the
   path-classification logic in isolation. *)
let detect ?(os = "Unix") ?(command_exists = always_exists) path =
  Update_pkg_manager.detect ~executable:path ~os
    ~realpath:(fun p -> p)
    ~command_exists ()

let manager_testable =
  Alcotest.testable
    (fun fmt m -> Format.pp_print_string fmt (Update_pkg_manager.name m))
    ( = )

let check_detect msg expected path =
  Alcotest.(check (option manager_testable)) msg expected (detect path)

let test_detect_npm () =
  check_detect "npm global lib" (Some Update_pkg_manager.Npm)
    "/usr/local/lib/node_modules/@clawq/clawq/bin/clawq";
  check_detect "npm-global prefix" (Some Update_pkg_manager.Npm)
    "/home/me/.npm-global/lib/node_modules/@clawq/clawq/bin/clawq"

let test_detect_pnpm () =
  check_detect "pnpm store" (Some Update_pkg_manager.Pnpm)
    "/home/me/.local/share/pnpm/global/5/node_modules/.pnpm/@clawq+clawq/node_modules/@clawq/clawq/bin/clawq"

let test_detect_yarn () =
  check_detect "yarn classic global" (Some Update_pkg_manager.Yarn)
    "/home/me/.config/yarn/global/node_modules/@clawq/clawq/bin/clawq"

let test_detect_bun () =
  check_detect "bun global" (Some Update_pkg_manager.Bun)
    "/home/me/.bun/install/global/node_modules/@clawq/clawq/bin/clawq"

let test_detect_homebrew () =
  check_detect "homebrew cellar (arm)" (Some Update_pkg_manager.Homebrew)
    "/opt/homebrew/Cellar/clawq/0.1.0/bin/clawq";
  check_detect "linuxbrew" (Some Update_pkg_manager.Homebrew)
    "/home/linuxbrew/.linuxbrew/Cellar/clawq/0.1.0/bin/clawq"

let test_detect_homebrew_intel () =
  (* Intel macOS uses a capital-C "Cellar" dir; detection must normalize case. *)
  check_detect "intel mac cellar" (Some Update_pkg_manager.Homebrew)
    "/usr/local/Cellar/clawq/0.1.0/bin/clawq"

let test_detect_npm_under_homebrew_prefix () =
  check_detect "npm under homebrew prefix is still npm"
    (Some Update_pkg_manager.Npm)
    "/opt/homebrew/lib/node_modules/@clawq/clawq/bin/clawq";
  check_detect "npm under linuxbrew prefix is still npm"
    (Some Update_pkg_manager.Npm)
    "/home/linuxbrew/.linuxbrew/lib/node_modules/@clawq/clawq/bin/clawq"

let test_detect_homebrew_excluded_on_windows () =
  Alcotest.(check (option manager_testable))
    "no homebrew on win32" None
    (detect ~os:"Win32" "C:\\opt\\homebrew\\Cellar\\clawq\\0.1.0\\bin\\clawq")

let test_detect_windows_npm () =
  Alcotest.(check (option manager_testable))
    "npm via AppData backslashes" (Some Update_pkg_manager.Npm)
    (detect ~os:"Win32"
       "C:\\Users\\me\\AppData\\Roaming\\npm\\node_modules\\@clawq\\clawq\\bin\\clawq")

let test_detect_unknown_path () =
  check_detect "plain bin dir" None "/usr/local/bin/clawq"

let test_detect_requires_cli_on_path () =
  Alcotest.(check (option manager_testable))
    "npm path but npm not installed -> None" None
    (detect
       ~command_exists:(fun _ -> false)
       "/usr/local/lib/node_modules/@clawq/clawq/bin/clawq")

let test_update_argv_unix () =
  let argv m = Array.to_list (Update_pkg_manager.update_argv ~os:"Unix" m) in
  Alcotest.(check (list string))
    "npm"
    [ "npm"; "install"; "-g"; "@clawq/clawq@latest" ]
    (argv Update_pkg_manager.Npm);
  Alcotest.(check (list string))
    "pnpm"
    [ "pnpm"; "add"; "-g"; "@clawq/clawq@latest" ]
    (argv Update_pkg_manager.Pnpm);
  Alcotest.(check (list string))
    "yarn"
    [ "yarn"; "global"; "add"; "@clawq/clawq@latest" ]
    (argv Update_pkg_manager.Yarn);
  Alcotest.(check (list string))
    "bun"
    [ "bun"; "add"; "-g"; "@clawq/clawq@latest" ]
    (argv Update_pkg_manager.Bun);
  Alcotest.(check (list string))
    "homebrew"
    [ "brew"; "upgrade"; "clawq" ]
    (argv Update_pkg_manager.Homebrew)

let test_update_argv_windows_exe_names () =
  let argv m = Array.to_list (Update_pkg_manager.update_argv ~os:"Win32" m) in
  Alcotest.(check (list string))
    "npm.cmd on windows"
    [ "npm.cmd"; "install"; "-g"; "@clawq/clawq@latest" ]
    (argv Update_pkg_manager.Npm);
  Alcotest.(check (list string))
    "bun.exe on windows"
    [ "bun.exe"; "add"; "-g"; "@clawq/clawq@latest" ]
    (argv Update_pkg_manager.Bun)

let test_describe_command () =
  Alcotest.(check string)
    "describe npm" "npm install -g @clawq/clawq@latest"
    (Update_pkg_manager.describe_command ~os:"Unix" Update_pkg_manager.Npm)

(* run_update integration: with no repo root and a detected manager, auto mode
   runs the package manager's command and signals a restart. *)
let test_run_update_auto_uses_pkg_manager () =
  let commands = ref [] in
  let signaled = ref None in
  let run_command ~cwd:_ ~argv ~send_progress ~interrupt_check:_ =
    let open Lwt.Syntax in
    commands := Array.to_list argv :: !commands;
    let* () = send_progress (String.concat " " (Array.to_list argv)) in
    Lwt.return 0
  in
  let result =
    Lwt_main.run
      (Update_tool.run_update
         ~find_repo_root:(fun ?start_path:_ ?exists:_ () -> None)
         ~detect_pkg_manager:(fun ~executable:_ -> Some Update_pkg_manager.Bun)
         ~run_command
         ~send_signal:(fun pid signal -> signaled := Some (pid, signal))
         ~is_draining:(fun () -> false)
         ~start_path:"/home/me/.bun/install/global/clawq/bin/clawq"
         ~send_progress:(fun _ -> Lwt.return_unit)
         ())
  in
  Alcotest.(check string)
    "pkg success result" "Package update complete. Sending restart signal..."
    result;
  Alcotest.(check (list (list string)))
    "ran bun add -g"
    [ [ "bun"; "add"; "-g"; "@clawq/clawq@latest" ] ]
    (List.rev !commands);
  Alcotest.(check bool) "restart signalled" true (!signaled <> None)

(* Auto mode must prefer a git checkout over the package manager, and must not
   even probe for a package manager when a repo root is present. *)
let test_run_update_auto_prefers_git_over_pkg () =
  let commands = ref [] in
  let detected = ref false in
  let run_command ~cwd:_ ~argv ~send_progress ~interrupt_check:_ =
    let open Lwt.Syntax in
    commands := Array.to_list argv :: !commands;
    let* () = send_progress (String.concat " " (Array.to_list argv)) in
    Lwt.return 0
  in
  let result =
    Lwt_main.run
      (Update_tool.run_update
         ~find_repo_root:(fun ?start_path:_ ?exists:_ () -> Some "/repo")
         ~detect_pkg_manager:(fun ~executable:_ ->
           detected := true;
           Some Update_pkg_manager.Npm)
         ~run_command
         ~send_signal:(fun _ _ -> ())
         ~is_draining:(fun () -> false)
         ~send_progress:(fun _ -> Lwt.return_unit)
         ())
  in
  Alcotest.(check string)
    "git path ran" "Build complete. Sending restart signal..." result;
  Alcotest.(check bool)
    "ran git pull" true
    (List.mem [ "git"; "pull" ] !commands);
  Alcotest.(check bool)
    "no package manager argv ran" false
    (List.exists (fun c -> List.mem "@clawq/clawq@latest" c) !commands);
  Alcotest.(check bool) "detection skipped when repo present" false !detected

(* pkg mode with no detectable manager reports an actionable, non-crashing
   message and does not signal a restart. *)
let test_run_update_pkg_mode_undetected () =
  let signaled = ref None in
  let result =
    Lwt_main.run
      (Update_tool.run_update ~mode:Update_tool.Pkg
         ~find_repo_root:(fun ?start_path:_ ?exists:_ () -> Some "/repo")
         ~detect_pkg_manager:(fun ~executable:_ -> None)
         ~run_command:(fun ~cwd:_ ~argv:_ ~send_progress:_ ~interrupt_check:_ ->
           Lwt.return 0)
         ~send_signal:(fun pid signal -> signaled := Some (pid, signal))
         ~is_draining:(fun () -> false)
         ~send_progress:(fun _ -> Lwt.return_unit)
         ())
  in
  Alcotest.(check bool)
    "mentions package manager" true
    (Test_helpers.string_contains result "Cannot detect a package manager");
  Alcotest.(check bool) "no restart signalled" true (!signaled = None)

let test_run_update_pkg_failure_aborts () =
  let signaled = ref None in
  let result =
    Lwt_main.run
      (Update_tool.run_update ~mode:Update_tool.Pkg
         ~find_repo_root:(fun ?start_path:_ ?exists:_ () -> None)
         ~detect_pkg_manager:(fun ~executable:_ -> Some Update_pkg_manager.Npm)
         ~run_command:(fun ~cwd:_ ~argv:_ ~send_progress:_ ~interrupt_check:_ ->
           Lwt.return 1)
         ~send_signal:(fun pid signal -> signaled := Some (pid, signal))
         ~is_draining:(fun () -> false)
         ~send_progress:(fun _ -> Lwt.return_unit)
         ())
  in
  Alcotest.(check bool)
    "reports npm failure" true
    (Test_helpers.string_contains result "update via npm failed");
  Alcotest.(check bool) "no restart on failure" true (!signaled = None)

let test_run_update_pkg_reexec_prefers_stable_path () =
  let previous_reexec = Sys.getenv_opt Restart_exec.reexec_path_env in
  let previous_path = Sys.getenv_opt "PATH" in
  let root =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "clawq-pkg-stable-%d" (Random.int 1_000_000))
  in
  let stable_dir = Filename.concat root "bin" in
  let store_dir = Filename.concat root "store/v1/bin" in
  let stable_path = Filename.concat stable_dir "clawq" in
  let store_path = Filename.concat store_dir "clawq" in
  Fun.protect
    (fun () ->
      mkdir_p stable_dir;
      mkdir_p store_dir;
      write_empty_exe stable_path;
      write_empty_exe store_path;
      Unix.putenv "PATH" stable_dir;
      Unix.putenv Restart_exec.reexec_path_env "";
      let result =
        Lwt_main.run
          (Update_tool.run_update ~mode:Update_tool.Pkg ~start_path:store_path
             ~find_repo_root:(fun ?start_path:_ ?exists:_ () -> Some "/repo")
             ~detect_pkg_manager:(fun ~executable:_ ->
               Some Update_pkg_manager.Pnpm)
             ~run_command:(fun
                 ~cwd:_ ~argv:_ ~send_progress:_ ~interrupt_check:_ ->
               Lwt.return 0)
             ~send_signal:(fun _ _ -> ())
             ~is_draining:(fun () -> false)
             ~send_progress:(fun _ -> Lwt.return_unit)
             ())
      in
      Alcotest.(check string)
        "pkg success result"
        "Package update complete. Sending restart signal..." result;
      Alcotest.(check (option string))
        "stable path selected" (Some stable_path)
        (Sys.getenv_opt Restart_exec.reexec_path_env))
    ~finally:(fun () ->
      (match previous_path with
      | Some value -> Unix.putenv "PATH" value
      | None -> Unix.putenv "PATH" "");
      (match previous_reexec with
      | Some value -> Unix.putenv Restart_exec.reexec_path_env value
      | None -> Unix.putenv Restart_exec.reexec_path_env "");
      remove_tree root)

let test_run_update_pkg_reexec_falls_back_to_resolved_path () =
  let previous_reexec = Sys.getenv_opt Restart_exec.reexec_path_env in
  let previous_path = Sys.getenv_opt "PATH" in
  let root =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "clawq-pkg-resolved-%d" (Random.int 1_000_000))
  in
  let real_dir = Filename.concat root "real/bin" in
  let link_dir = Filename.concat root "link/bin" in
  let real_path = Filename.concat real_dir "clawq" in
  let link_path = Filename.concat link_dir "clawq" in
  Fun.protect
    (fun () ->
      mkdir_p real_dir;
      mkdir_p link_dir;
      write_empty_exe real_path;
      Unix.symlink real_path link_path;
      Unix.putenv "PATH" "";
      Unix.putenv Restart_exec.reexec_path_env "";
      let result =
        Lwt_main.run
          (Update_tool.run_update ~mode:Update_tool.Pkg ~start_path:link_path
             ~find_repo_root:(fun ?start_path:_ ?exists:_ () -> None)
             ~detect_pkg_manager:(fun ~executable:_ ->
               Some Update_pkg_manager.Npm)
             ~run_command:(fun
                 ~cwd:_ ~argv:_ ~send_progress:_ ~interrupt_check:_ ->
               Lwt.return 0)
             ~send_signal:(fun _ _ -> ())
             ~is_draining:(fun () -> false)
             ~send_progress:(fun _ -> Lwt.return_unit)
             ())
      in
      Alcotest.(check string)
        "pkg success result"
        "Package update complete. Sending restart signal..." result;
      Alcotest.(check (option string))
        "resolved path selected" (Some real_path)
        (Sys.getenv_opt Restart_exec.reexec_path_env))
    ~finally:(fun () ->
      (match previous_path with
      | Some value -> Unix.putenv "PATH" value
      | None -> Unix.putenv "PATH" "");
      (match previous_reexec with
      | Some value -> Unix.putenv Restart_exec.reexec_path_env value
      | None -> Unix.putenv Restart_exec.reexec_path_env "");
      remove_tree root)

let suite =
  [
    ("detect npm", `Quick, test_detect_npm);
    ("detect pnpm", `Quick, test_detect_pnpm);
    ("detect yarn", `Quick, test_detect_yarn);
    ("detect bun", `Quick, test_detect_bun);
    ("detect homebrew", `Quick, test_detect_homebrew);
    ("detect homebrew intel", `Quick, test_detect_homebrew_intel);
    ( "detect npm under homebrew prefix",
      `Quick,
      test_detect_npm_under_homebrew_prefix );
    ( "homebrew excluded on windows",
      `Quick,
      test_detect_homebrew_excluded_on_windows );
    ("detect windows npm", `Quick, test_detect_windows_npm);
    ("detect unknown path", `Quick, test_detect_unknown_path);
    ("detect requires cli on path", `Quick, test_detect_requires_cli_on_path);
    ("update_argv unix", `Quick, test_update_argv_unix);
    ("update_argv windows exe names", `Quick, test_update_argv_windows_exe_names);
    ("describe_command", `Quick, test_describe_command);
    ( "run_update auto uses pkg manager",
      `Quick,
      test_run_update_auto_uses_pkg_manager );
    ( "run_update auto prefers git over pkg",
      `Quick,
      test_run_update_auto_prefers_git_over_pkg );
    ( "run_update pkg mode undetected",
      `Quick,
      test_run_update_pkg_mode_undetected );
    ("run_update pkg failure aborts", `Quick, test_run_update_pkg_failure_aborts);
    ( "run_update pkg reexec prefers stable path",
      `Quick,
      test_run_update_pkg_reexec_prefers_stable_path );
    ( "run_update pkg reexec falls back to resolved path",
      `Quick,
      test_run_update_pkg_reexec_falls_back_to_resolved_path );
  ]
