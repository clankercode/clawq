(* B775: hosted-runner isolation and credential-separation tests. *)

let hostile_env =
  [|
    "PATH=/usr/bin";
    "HOME=/home/worker";
    "USER=worker";
    "LC_ALL=C.UTF-8";
    "CLAWQ_RUNNER_TOKEN=rt-1";
    "GITHUB_TOKEN=ghp_secret";
    "GH_TOKEN=gho_secret";
    "SSH_AUTH_SOCK=/run/user/1000/ssh.sock";
    "AWS_SECRET_ACCESS_KEY=aws_secret";
    "GOOGLE_APPLICATION_CREDENTIALS=/creds.json";
    "OPENAI_API_KEY=sk-pay-as-you-go";
    "ANTHROPIC_API_KEY=sk-ant-pay";
    "GITHUB_APP_PRIVATE_KEY_PATH=/keys/app.pem";
  |]

let test_minimal_env_strips_credentials () =
  let filtered = Array.to_list (Runner_isolation.minimal_env hostile_env) in
  let has prefix =
    List.exists (fun e -> String_util.contains e prefix) filtered
  in
  Alcotest.(check bool) "PATH kept" true (has "PATH=");
  Alcotest.(check bool) "HOME kept" true (has "HOME=");
  Alcotest.(check bool) "locale kept" true (has "LC_ALL=");
  Alcotest.(check bool) "clawq runner vars kept" true (has "CLAWQ_RUNNER_TOKEN");
  List.iter
    (fun banned ->
      Alcotest.(check bool)
        (banned ^ " absent") false
        (List.exists
           (fun e ->
             String.length e > String.length banned
             && String.sub e 0 (String.length banned) = banned)
           filtered))
    [
      "GITHUB_TOKEN";
      "GH_TOKEN";
      "SSH_AUTH_SOCK";
      "AWS_SECRET_ACCESS_KEY";
      "GOOGLE_APPLICATION_CREDENTIALS";
      "OPENAI_API_KEY";
      "ANTHROPIC_API_KEY";
      "GITHUB_APP_PRIVATE_KEY_PATH";
    ]

let test_mode_parsing_fails_closed () =
  Alcotest.(check string)
    "off" "off"
    (Runner_isolation.string_of_mode (Runner_isolation.mode_of_string "off"));
  Alcotest.(check string)
    "prefer" "prefer"
    (Runner_isolation.string_of_mode (Runner_isolation.mode_of_string "prefer"));
  Alcotest.(check string)
    "require" "require"
    (Runner_isolation.string_of_mode
       (Runner_isolation.mode_of_string "require"));
  (* Unknown values must not silently disable isolation. *)
  Alcotest.(check string)
    "unknown maps to require" "require"
    (Runner_isolation.string_of_mode (Runner_isolation.mode_of_string "banana"))

let policy mode backend : Runner_isolation.policy =
  { Runner_isolation.mode; backend; extra_paths = [] }

let test_preflight () =
  (match
     Runner_isolation.preflight (policy Runner_isolation.Off Sandbox.None)
   with
  | Ok () -> ()
  | Error msg -> Alcotest.fail msg);
  (match
     Runner_isolation.preflight (policy Runner_isolation.Prefer Sandbox.None)
   with
  | Ok () -> ()
  | Error msg -> Alcotest.fail ("prefer must degrade with warning: " ^ msg));
  match
    Runner_isolation.preflight (policy Runner_isolation.Require Sandbox.None)
  with
  | Ok () -> Alcotest.fail "require without backend must fail closed"
  | Error msg ->
      Alcotest.(check bool)
        "actionable error" true
        (String_util.contains msg "bubblewrap"
        && String_util.contains msg "hosted_runner_isolation")

let hostile_prompt = "innocent\"; rm -rf /tmp/pwned; echo \"$(id)"

let test_wrap_argv_off_passthrough () =
  let argv = [| "codex"; "exec"; hostile_prompt |] in
  let wrapped, applied =
    Runner_isolation.wrap_argv
      (policy Runner_isolation.Off Sandbox.Bubblewrap)
      ~worktree:"/tmp" ~log_path:"/tmp/x.log" argv
  in
  Alcotest.(check bool) "not applied" false applied;
  Alcotest.(check int) "unchanged" (Array.length argv) (Array.length wrapped)

let test_wrap_argv_bwrap_shape () =
  let argv = [| "codex"; "exec"; hostile_prompt |] in
  let wrapped, applied =
    Runner_isolation.wrap_argv
      (policy Runner_isolation.Require Sandbox.Bubblewrap)
      ~worktree:"/tmp" ~log_path:"/tmp/task.log" argv
  in
  if not (Sandbox.is_available Sandbox.Bubblewrap) then Alcotest.skip ()
  else begin
    Alcotest.(check bool) "applied" true applied;
    Alcotest.(check string) "bwrap prefix" "bwrap" wrapped.(0);
    (* Runner argv survives verbatim after the "--" separator: the hostile
       prompt stays one element and is never joined into a shell string. *)
    let sep =
      let idx = ref (-1) in
      Array.iteri (fun i a -> if a = "--" && !idx < 0 then idx := i) wrapped;
      !idx
    in
    Alcotest.(check bool) "has -- separator" true (sep >= 0);
    let tail = Array.sub wrapped (sep + 1) (Array.length wrapped - sep - 1) in
    Alcotest.(check (array string)) "argv preserved" argv tail;
    Alcotest.(check bool)
      "worktree bound" true
      (Array.exists (fun a -> a = "/tmp") wrapped);
    Alcotest.(check bool)
      "pid namespace unshared" true
      (Array.exists (fun a -> a = "--unshare-pid") wrapped)
  end

let test_bwrap_blocks_path_escape () =
  if not (Sandbox.is_available Sandbox.Bubblewrap) then Alcotest.skip ()
  else
    (* A secret outside the worktree must be unreadable from inside. *)
    let secret = Filename.temp_file "clawq-secret" ".txt" in
    let oc = open_out secret in
    output_string oc "publisher-credential";
    close_out oc;
    Fun.protect
      ~finally:(fun () -> try Sys.remove secret with _ -> ())
      (fun () ->
        let worktree = Filename.temp_file "clawq-wt" "" in
        Sys.remove worktree;
        Unix.mkdir worktree 0o755;
        let sandbox_policy =
          {
            Runner_isolation.mode = Runner_isolation.Require;
            backend = Sandbox.Bubblewrap;
            extra_paths = [];
          }
        in
        (* /bin/cat <secret> — trusted test argv, untrusted-style target *)
        let wrapped, _ =
          Runner_isolation.wrap_argv sandbox_policy ~worktree
            ~log_path:(Filename.concat worktree "t.log")
            [| "cat"; secret |]
        in
        let exit_code, stdout, _stderr =
          Lwt_main.run
            (let open Lwt.Syntax in
             let proc =
               Process_group.start ~cwd:worktree ~env:[| "PATH=/usr/bin" |]
                 (Process_group.Exec wrapped)
             in
             Lwt.finalize
               (fun () ->
                 let* stdout, stderr =
                   Lwt.both
                     (Lwt_io.read proc.Process_group.stdout)
                     (Lwt_io.read proc.Process_group.stderr)
                 in
                 let* status = Process_group.wait proc.pid in
                 Lwt.return
                   (Background_task.exit_code_of_status status, stdout, stderr))
               (fun () -> Process_group.close proc))
        in
        Alcotest.(check bool) "cat fails outside sandbox" true (exit_code <> 0);
        Alcotest.(check bool)
          "secret does not leak" false
          (String_util.contains stdout "publisher-credential"))

let test_spawn_fails_closed_without_backend () =
  let repo = Filename.temp_file "clawq-iso-repo" "" in
  Sys.remove repo;
  Unix.mkdir repo 0o755;
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote repo))))
    (fun () ->
      let db = Memory.init ~db_path:":memory:" () in
      Background_task.init_schema db;
      let id =
        match
          Background_task.enqueue ~db ~runner:Background_task.Codex
            ~require_git:false ~use_worktree:false ~repo_path:repo
            ~prompt:"answer" ()
        with
        | Ok id -> id
        | Error msg -> Alcotest.fail msg
      in
      let rec wait_terminal n =
        let open Lwt.Syntax in
        match Background_task.get_task ~db ~id with
        | Some task when Background_task.is_terminal_status task.status ->
            Lwt.return task
        | _ when n <= 0 -> Alcotest.fail "task never became terminal"
        | _ ->
            let* () = Lwt_unix.sleep 0.02 in
            wait_terminal (n - 1)
      in
      let task =
        Lwt_main.run
          (let open Lwt.Syntax in
           Background_task.spawn_task ~db
             ~command_override:(Process_group.Exec [| "true" |])
             ~isolation_policy:
               {
                 Runner_isolation.mode = Runner_isolation.Require;
                 backend = Sandbox.None;
                 extra_paths = [];
               }
             (Option.get (Background_task.get_task ~db ~id));
           wait_terminal 200)
      in
      Alcotest.(check string)
        "failed closed" "failed"
        (Background_task.string_of_status task.status);
      match task.result_preview with
      | Some msg ->
          Alcotest.(check bool)
            "actionable reason" true
            (String_util.contains msg "hosted_runner_isolation")
      | None -> Alcotest.fail "expected a failure reason")

let test_policy_of_config_defaults () =
  let policy =
    Runner_isolation.policy_of_config Runtime_config.default.security
  in
  Alcotest.(check string)
    "default mode is off" "off"
    (Runner_isolation.string_of_mode policy.Runner_isolation.mode)

let suite =
  [
    Alcotest.test_case "minimal env strips credentials" `Quick
      test_minimal_env_strips_credentials;
    Alcotest.test_case "unknown isolation mode fails closed" `Quick
      test_mode_parsing_fails_closed;
    Alcotest.test_case "preflight enforces require" `Quick test_preflight;
    Alcotest.test_case "wrap_argv off is passthrough" `Quick
      test_wrap_argv_off_passthrough;
    Alcotest.test_case "bwrap wrap preserves argv verbatim" `Quick
      test_wrap_argv_bwrap_shape;
    Alcotest.test_case "bwrap blocks path escape to secrets" `Quick
      test_bwrap_blocks_path_escape;
    Alcotest.test_case "spawn fails closed without backend" `Quick
      test_spawn_fails_closed_without_backend;
    Alcotest.test_case "default config keeps legacy behavior" `Quick
      test_policy_of_config_defaults;
  ]
