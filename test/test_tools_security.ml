let with_temp_workspace f =
  let dir = Filename.temp_dir "clawq_tools_" "" in
  Fun.protect (fun () -> f dir) ~finally:(fun () -> Test_helpers.rm_tree dir)

let mk_none_sandbox ?(workspace = Sys.getcwd ()) () =
  {
    Sandbox.backend = Sandbox.None;
    workspace;
    extra_allowed_paths = [];
    isolate_filesystem = true;
  }

let assert_no_drift label =
  let path_drift, shell_drift, tokenizer_drift =
    Tools_builtin.get_drift_counters ()
  in
  Alcotest.(check int) (label ^ " path drift") 0 path_drift;
  Alcotest.(check int) (label ^ " shell drift") 0 shell_drift;
  Alcotest.(check int) (label ^ " tokenizer drift") 0 tokenizer_drift

let with_drift_check label f =
  Tools_builtin.reset_drift_counters ();
  Fun.protect f ~finally:(fun () -> assert_no_drift label)

let test_path_traversal_rejected () =
  with_temp_workspace (fun workspace ->
      with_drift_check "path traversal rejected" (fun () ->
          Alcotest.(check bool)
            "../ rejected" false
            (Tools_builtin.is_path_safe ~workspace "../etc/passwd");
          Alcotest.(check bool)
            "prefix escape rejected" false
            (Tools_builtin.is_path_safe ~workspace
               (workspace ^ "2/outside.txt"))))

let test_shell_allowlist_rejects_disallowed () =
  with_drift_check "shell allowlist rejects disallowed" (fun () ->
      let tool =
        Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
          ~allowed_commands:[ "ls" ] ~extra_allowed_paths:[]
          ~sandbox:(mk_none_sandbox ())
      in
      let args = `Assoc [ ("command", `String "echo hi") ] in
      let out = Lwt_main.run (tool.invoke args) in
      Alcotest.(check bool)
        "blocked" true
        (Test_helpers.string_contains out "not in the allowlist"))

let test_shell_allowlist_allows_command () =
  with_drift_check "shell allowlist allows command" (fun () ->
      let tool =
        Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
          ~allowed_commands:[ "ls" ] ~extra_allowed_paths:[]
          ~sandbox:(mk_none_sandbox ())
      in
      let args = `Assoc [ ("command", `String "ls .") ] in
      let out = Lwt_main.run (tool.invoke args) in
      Alcotest.(check bool)
        "success" true
        (Test_helpers.string_contains out "exit_code: 0"))

let test_shell_rejects_command_chaining () =
  with_drift_check "shell chaining rejected" (fun () ->
      let tool =
        Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
          ~allowed_commands:[ "ls" ] ~extra_allowed_paths:[]
          ~sandbox:(mk_none_sandbox ())
      in
      let args = `Assoc [ ("command", `String "ls && whoami") ] in
      let out = Lwt_main.run (tool.invoke args) in
      Alcotest.(check bool)
        "unsafe syntax blocked" true
        (Test_helpers.string_contains out "unsafe shell syntax"))

let test_shell_rejects_dollar_expansion () =
  with_drift_check "shell dollar expansion rejected" (fun () ->
      let tool =
        Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
          ~allowed_commands:[ "ls" ] ~extra_allowed_paths:[]
          ~sandbox:(mk_none_sandbox ())
      in
      let args = `Assoc [ ("command", `String "ls $HOME") ] in
      let out = Lwt_main.run (tool.invoke args) in
      Alcotest.(check bool)
        "dollar expansion blocked" true
        (Test_helpers.string_contains out "unsafe shell syntax"))

let test_shell_handles_quoted_args () =
  with_drift_check "shell quoted args" (fun () ->
      let tool =
        Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
          ~allowed_commands:[ "ls" ] ~extra_allowed_paths:[]
          ~sandbox:(mk_none_sandbox ())
      in
      let args = `Assoc [ ("command", `String "ls \".\"") ] in
      let out = Lwt_main.run (tool.invoke args) in
      Alcotest.(check bool)
        "quoted arg success" true
        (Test_helpers.string_contains out "exit_code: 0"))

let test_shell_streams_stdout_chunks () =
  with_drift_check "shell streams stdout chunks" (fun () ->
      let tool =
        Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
          ~allowed_commands:[ "echo" ] ~extra_allowed_paths:[]
          ~sandbox:(mk_none_sandbox ())
      in
      let args = `Assoc [ ("command", `String "echo hello") ] in
      match tool.invoke_stream with
      | None -> Alcotest.fail "expected shell streaming support"
      | Some invoke_stream ->
          let chunks = Buffer.create 16 in
          let out =
            Lwt_main.run
              (invoke_stream
                 ~on_output_chunk:(fun chunk ->
                   Buffer.add_string chunks chunk;
                   Lwt.return_unit)
                 args)
          in
          Alcotest.(check string)
            "streamed stdout" "hello\n" (Buffer.contents chunks);
          Alcotest.(check bool)
            "result still includes stdout" true
            (Test_helpers.string_contains out "hello"))

let test_shell_streams_stderr_chunks () =
  with_drift_check "shell streams stderr chunks" (fun () ->
      let tool =
        Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
          ~allowed_commands:[ "ls" ] ~extra_allowed_paths:[]
          ~sandbox:(mk_none_sandbox ())
      in
      let args = `Assoc [ ("command", `String "ls definitely-missing-file") ] in
      match tool.invoke_stream with
      | None -> Alcotest.fail "expected shell streaming support"
      | Some invoke_stream ->
          let chunks = Buffer.create 16 in
          let out =
            Lwt_main.run
              (invoke_stream
                 ~on_output_chunk:(fun chunk ->
                   Buffer.add_string chunks chunk;
                   Lwt.return_unit)
                 args)
          in
          Alcotest.(check bool)
            "streamed stderr" true
            (Test_helpers.string_contains (Buffer.contents chunks)
               "definitely-missing-file");
          Alcotest.(check bool)
            "result still includes stderr" true
            (Test_helpers.string_contains out "definitely-missing-file"))

let test_shell_rejects_absolute_path_arg () =
  with_drift_check "shell absolute path arg blocked" (fun () ->
      let tool =
        Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
          ~allowed_commands:[ "cat" ] ~extra_allowed_paths:[]
          ~sandbox:(mk_none_sandbox ())
      in
      let args = `Assoc [ ("command", `String "cat /etc/passwd") ] in
      let out = Lwt_main.run (tool.invoke args) in
      Alcotest.(check bool)
        "absolute path blocked" true
        (Test_helpers.string_contains out "disallowed in workspace_only mode"))

let test_shell_rejects_url_arg () =
  with_drift_check "shell url arg blocked" (fun () ->
      let tool =
        Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
          ~allowed_commands:[ "git" ] ~extra_allowed_paths:[]
          ~sandbox:(mk_none_sandbox ())
      in
      let args =
        `Assoc [ ("command", `String "git clone https://example.com/repo") ]
      in
      let out = Lwt_main.run (tool.invoke args) in
      Alcotest.(check bool)
        "url blocked" true
        (Test_helpers.string_contains out "disallowed in workspace_only mode"))

let test_shell_rejects_binary_path_bypass () =
  with_drift_check "shell binary path bypass blocked" (fun () ->
      let tool =
        Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
          ~allowed_commands:[ "ls" ] ~extra_allowed_paths:[]
          ~sandbox:(mk_none_sandbox ())
      in
      let args = `Assoc [ ("command", `String "./ls") ] in
      let out = Lwt_main.run (tool.invoke args) in
      Alcotest.(check bool)
        "binary path blocked" true
        (Test_helpers.string_contains out "binary path is disallowed"))

let test_shell_rejects_option_assigned_absolute_path () =
  with_drift_check "shell option assigned absolute path blocked" (fun () ->
      let tool =
        Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
          ~allowed_commands:[ "tar" ] ~extra_allowed_paths:[]
          ~sandbox:(mk_none_sandbox ())
      in
      let args =
        `Assoc [ ("command", `String "tar --file=/tmp/out.tar -cf out.tar .") ]
      in
      let out = Lwt_main.run (tool.invoke args) in
      Alcotest.(check bool)
        "assigned path blocked" true
        (Test_helpers.string_contains out "disallowed in workspace_only mode"))

let test_shell_rejects_git_network_subcommand () =
  with_drift_check "shell git network subcommand blocked" (fun () ->
      let tool =
        Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
          ~allowed_commands:[ "git" ] ~extra_allowed_paths:[]
          ~sandbox:(mk_none_sandbox ())
      in
      let args = `Assoc [ ("command", `String "git clone repo") ] in
      let out = Lwt_main.run (tool.invoke args) in
      Alcotest.(check bool)
        "git clone blocked" true
        (Test_helpers.string_contains out "disallowed in workspace_only mode"))

let test_shell_honors_explicit_cwd () =
  with_temp_workspace (fun workspace ->
      with_drift_check "shell explicit cwd" (fun () ->
          let subdir = Filename.concat workspace "nested" in
          Unix.mkdir subdir 0o755;
          Fun.protect
            (fun () ->
              let tool =
                Tools_builtin.shell_exec ~workspace ~workspace_only:true
                  ~allowed_commands:[ "pwd" ] ~extra_allowed_paths:[]
                  ~sandbox:(mk_none_sandbox ~workspace ())
              in
              let out =
                Lwt_main.run
                  (tool.invoke
                     (`Assoc
                        [
                          ("command", `String "pwd"); ("cwd", `String "nested");
                        ]))
              in
              Alcotest.(check bool)
                "success" true
                (Test_helpers.string_contains out "exit_code: 0");
              Alcotest.(check bool)
                "pwd uses nested cwd" true
                (Test_helpers.string_contains out subdir))
            ~finally:(fun () -> Unix.rmdir subdir)))

let test_shell_rejects_disallowed_cwd () =
  with_temp_workspace (fun workspace ->
      with_drift_check "shell disallowed cwd blocked" (fun () ->
          let tool =
            Tools_builtin.shell_exec ~workspace ~workspace_only:true
              ~allowed_commands:[ "pwd" ] ~extra_allowed_paths:[]
              ~sandbox:(mk_none_sandbox ~workspace ())
          in
          let out =
            Lwt_main.run
              (tool.invoke
                 (`Assoc [ ("command", `String "pwd"); ("cwd", `String "/tmp") ]))
          in
          Alcotest.(check bool)
            "disallowed cwd blocked" true
            (Test_helpers.string_contains out
               "cwd is disallowed in workspace_only mode")))

let test_shell_extra_allowed_paths_grants_access () =
  let base = Filename.get_temp_dir_name () in
  let workspace =
    Filename.concat base
      (Printf.sprintf "clawq_shell_ws_%d_%d" (Unix.getpid ()) (Random.bits ()))
  in
  let extra_dir =
    Filename.concat base
      (Printf.sprintf "clawq_shell_extra_%d_%d" (Unix.getpid ())
         (Random.bits ()))
  in
  Unix.mkdir workspace 0o755;
  Unix.mkdir extra_dir 0o755;
  Fun.protect
    (fun () ->
      with_drift_check "shell extra allowed paths grants access" (fun () ->
          let tool =
            Tools_builtin.shell_exec ~workspace ~workspace_only:true
              ~allowed_commands:[ "ls" ] ~extra_allowed_paths:[ extra_dir ]
              ~sandbox:(mk_none_sandbox ~workspace ())
          in
          let out =
            Lwt_main.run
              (tool.invoke
                 (`Assoc [ ("command", `String ("ls " ^ extra_dir)) ]))
          in
          Alcotest.(check bool)
            "extra allowed path usable in shell" true
            (Test_helpers.string_contains out "exit_code: 0");
          let tool_no_extra =
            Tools_builtin.shell_exec ~workspace ~workspace_only:true
              ~allowed_commands:[ "ls" ] ~extra_allowed_paths:[]
              ~sandbox:(mk_none_sandbox ())
          in
          let out2 =
            Lwt_main.run
              (tool_no_extra.invoke
                 (`Assoc [ ("command", `String ("ls " ^ extra_dir)) ]))
          in
          Alcotest.(check bool)
            "without extra_allowed_paths blocked in shell" true
            (Test_helpers.string_contains out2
               "disallowed in workspace_only mode")))
    ~finally:(fun () ->
      (try Unix.rmdir extra_dir with _ -> ());
      try Unix.rmdir workspace with _ -> ())

let test_file_edit_replaces_first_match () =
  with_temp_workspace (fun workspace ->
      let full_path = Filename.concat workspace "note.txt" in
      let oc = open_out full_path in
      output_string oc "abc abc";
      close_out oc;
      let tool =
        Tools_builtin.file_edit ~workspace ~workspace_only:true
          ~extra_allowed_paths:[]
      in
      let args =
        `Assoc
          [
            ("path", `String "note.txt");
            ("old_text", `String "abc");
            ("new_text", `String "xyz");
          ]
      in
      ignore (Lwt_main.run (tool.invoke args));
      let ic = open_in full_path in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      Alcotest.(check string) "only first replaced" "xyz abc" content)

let test_file_read_uses_configured_workspace_root () =
  let base = Filename.get_temp_dir_name () in
  let workspace =
    Filename.concat base
      (Printf.sprintf "clawq_read_%d_%d" (Unix.getpid ()) (Random.bits ()))
  in
  Unix.mkdir workspace 0o755;
  let path = Filename.concat workspace "EGO.md" in
  let oc = open_out path in
  output_string oc "identity";
  close_out oc;
  Fun.protect
    (fun () ->
      let tool =
        Tools_builtin.file_read ~workspace ~workspace_only:true
          ~extra_allowed_paths:[]
      in
      let abs_out =
        Lwt_main.run (tool.invoke (`Assoc [ ("path", `String path) ]))
      in
      Alcotest.(check string) "absolute path allowed" "identity" abs_out;
      let rel_out =
        Lwt_main.run (tool.invoke (`Assoc [ ("path", `String "EGO.md") ]))
      in
      Alcotest.(check string)
        "relative path resolved in workspace" "identity" rel_out)
    ~finally:(fun () ->
      (try Unix.unlink path with _ -> ());
      try Unix.rmdir workspace with _ -> ())

let test_grep_supports_regex_and_include_alias () =
  with_temp_workspace (fun workspace ->
      let full_path = Filename.concat workspace "runtime_config.ml" in
      let oc = open_out full_path in
      output_string oc
        "type provider_config = unit\nlet search_provider = \"brave\"\n";
      close_out oc;
      let tool =
        Tools_builtin.grep ~workspace ~workspace_only:true
          ~extra_allowed_paths:[]
      in
      let out =
        Lwt_main.run
          (tool.invoke
             (`Assoc
                [
                  ("pattern", `String "type provider_config|search_provider");
                  ("path", `String "runtime_config.ml");
                  ("include", `String "*.ml");
                ]))
      in
      Alcotest.(check bool)
        "regex matches first line" true
        (Test_helpers.string_contains out "type provider_config = unit");
      Alcotest.(check bool)
        "regex matches second line" true
        (Test_helpers.string_contains out "let search_provider = \"brave\""))

let test_grep_single_file_respects_include_filter () =
  with_temp_workspace (fun workspace ->
      let full_path = Filename.concat workspace "runtime_config.ml" in
      let oc = open_out full_path in
      output_string oc "let search_provider = \"brave\"\n";
      close_out oc;
      let tool =
        Tools_builtin.grep ~workspace ~workspace_only:true
          ~extra_allowed_paths:[]
      in
      let out =
        Lwt_main.run
          (tool.invoke
             (`Assoc
                [
                  ("pattern", `String "search_provider");
                  ("path", `String "runtime_config.ml");
                  ("include", `String "*.txt");
                ]))
      in
      Alcotest.(check bool)
        "non-matching include skips single file" true
        (Test_helpers.string_contains out "No matches found"))

let test_grep_honors_case_sensitive_flag () =
  with_temp_workspace (fun workspace ->
      let full_path = Filename.concat workspace "runtime_config.ml" in
      let oc = open_out full_path in
      output_string oc "let search_provider = \"brave\"\n";
      close_out oc;
      let tool =
        Tools_builtin.grep ~workspace ~workspace_only:true
          ~extra_allowed_paths:[]
      in
      let out =
        Lwt_main.run
          (tool.invoke
             (`Assoc
                [
                  ("pattern", `String "SEARCH_PROVIDER");
                  ("path", `String "runtime_config.ml");
                  ("case_sensitive", `Bool false);
                ]))
      in
      Alcotest.(check bool)
        "case-insensitive regex matches" true
        (Test_helpers.string_contains out "search_provider"))

let test_glob_with_root_subdirectory () =
  with_temp_workspace (fun workspace ->
      let subdir = Filename.concat workspace "subdir" in
      Unix.mkdir subdir 0o755;
      let file = Filename.concat subdir "foo.ml" in
      let oc = open_out file in
      output_string oc "let x = 1\n";
      close_out oc;
      let other = Filename.concat workspace "other.ml" in
      let oc2 = open_out other in
      output_string oc2 "let y = 2\n";
      close_out oc2;
      let tool =
        Tools_builtin.glob ~workspace ~workspace_only:false
          ~extra_allowed_paths:[]
      in
      let out =
        Lwt_main.run
          (tool.Tool.invoke
             (`Assoc
                [
                  ("pattern", `String "*.ml");
                  ("root", `String (Filename.concat workspace "subdir"));
                ]))
      in
      Alcotest.(check bool)
        "finds file in subdir" true
        (Test_helpers.string_contains out "foo.ml");
      Alcotest.(check bool)
        "does not include file outside subdir" false
        (Test_helpers.string_contains out "other.ml"))

let test_glob_invalid_root_returns_error () =
  with_temp_workspace (fun workspace ->
      let tool =
        Tools_builtin.glob ~workspace ~workspace_only:false
          ~extra_allowed_paths:[]
      in
      let out =
        Lwt_main.run
          (tool.Tool.invoke
             (`Assoc
                [
                  ("pattern", `String "*.ml");
                  ("root", `String "/nonexistent/path/xyz");
                ]))
      in
      Alcotest.(check bool)
        "error for nonexistent root" true
        (Test_helpers.string_contains out "Error"))

let test_glob_root_is_file_returns_error () =
  with_temp_workspace (fun workspace ->
      let file = Filename.concat workspace "notadir.ml" in
      let oc = open_out file in
      output_string oc "let x = 1\n";
      close_out oc;
      let tool =
        Tools_builtin.glob ~workspace ~workspace_only:false
          ~extra_allowed_paths:[]
      in
      let out =
        Lwt_main.run
          (tool.Tool.invoke
             (`Assoc [ ("pattern", `String "*.ml"); ("root", `String file) ]))
      in
      Alcotest.(check bool)
        "error for file used as root" true
        (Test_helpers.string_contains out "Error"))

let test_list_dir_with_custom_path () =
  with_temp_workspace (fun workspace ->
      let subdir = Filename.concat workspace "mysubdir" in
      Unix.mkdir subdir 0o755;
      let file = Filename.concat subdir "inside.ml" in
      let oc = open_out file in
      output_string oc "let y = 2\n";
      close_out oc;
      let other = Filename.concat workspace "outside.ml" in
      let oc2 = open_out other in
      output_string oc2 "let z = 3\n";
      close_out oc2;
      let tool =
        Tools_builtin.list_dir ~workspace ~workspace_only:false
          ~extra_allowed_paths:[]
      in
      let out =
        Lwt_main.run (tool.Tool.invoke (`Assoc [ ("path", `String subdir) ]))
      in
      Alcotest.(check bool)
        "finds file inside subdir" true
        (Test_helpers.string_contains out "inside.ml");
      Alcotest.(check bool)
        "does not include file outside subdir" false
        (Test_helpers.string_contains out "outside.ml"))

let test_list_dir_nonexistent_path_returns_error () =
  with_temp_workspace (fun workspace ->
      let tool =
        Tools_builtin.list_dir ~workspace ~workspace_only:false
          ~extra_allowed_paths:[]
      in
      let out =
        Lwt_main.run
          (tool.Tool.invoke (`Assoc [ ("path", `String "/nonexistent/xyz") ]))
      in
      Alcotest.(check bool)
        "error for nonexistent path" true
        (Test_helpers.string_contains out "Error"))

let test_list_dir_file_path_returns_error () =
  with_temp_workspace (fun workspace ->
      let file = Filename.concat workspace "afile.txt" in
      let oc = open_out file in
      output_string oc "hello\n";
      close_out oc;
      let tool =
        Tools_builtin.list_dir ~workspace ~workspace_only:false
          ~extra_allowed_paths:[]
      in
      let out =
        Lwt_main.run (tool.Tool.invoke (`Assoc [ ("path", `String file) ]))
      in
      Alcotest.(check bool)
        "error for file used as path" true
        (Test_helpers.string_contains out "Error"))

let test_grep_with_directory_arg () =
  with_temp_workspace (fun workspace ->
      let subdir = Filename.concat workspace "src" in
      Unix.mkdir subdir 0o755;
      let file = Filename.concat subdir "main.ml" in
      let oc = open_out file in
      output_string oc "let main () = print_endline \"hello\"\n";
      close_out oc;
      let other = Filename.concat workspace "README.md" in
      let oc2 = open_out other in
      output_string oc2 "let main = not_a_function\n";
      close_out oc2;
      let tool =
        Tools_builtin.grep ~workspace ~workspace_only:false
          ~extra_allowed_paths:[]
      in
      let out =
        Lwt_main.run
          (tool.Tool.invoke
             (`Assoc
                [
                  ("pattern", `String "let main");
                  ("path", `String (Filename.concat workspace "src"));
                ]))
      in
      Alcotest.(check bool)
        "finds match in subdir" true
        (Test_helpers.string_contains out "main.ml");
      Alcotest.(check bool)
        "does not search outside subdir" false
        (Test_helpers.string_contains out "README.md"))

let test_grep_invalid_path_returns_error () =
  with_temp_workspace (fun workspace ->
      let tool =
        Tools_builtin.grep ~workspace ~workspace_only:false
          ~extra_allowed_paths:[]
      in
      let out =
        Lwt_main.run
          (tool.Tool.invoke
             (`Assoc
                [
                  ("pattern", `String "hello");
                  ("path", `String "/nonexistent/path/xyz");
                ]))
      in
      Alcotest.(check bool)
        "error for nonexistent path" true
        (Test_helpers.string_contains out "Error"))

let test_transcribe_rejects_outside_workspace () =
  with_temp_workspace (fun _workspace ->
      let cfg = Runtime_config.default in
      let tool = Tools_builtin.transcribe ~config:cfg in
      let args = `Assoc [ ("file_path", `String "/etc/passwd") ] in
      let out = Lwt_main.run (tool.invoke args) in
      Alcotest.(check bool)
        "outside workspace blocked" true
        (Test_helpers.string_contains out "outside workspace"))

let test_extra_allowed_paths_grants_access () =
  let base = Filename.get_temp_dir_name () in
  let workspace =
    Filename.concat base
      (Printf.sprintf "clawq_ws_%d_%d" (Unix.getpid ()) (Random.bits ()))
  in
  let extra_dir =
    Filename.concat base
      (Printf.sprintf "clawq_extra_%d_%d" (Unix.getpid ()) (Random.bits ()))
  in
  Unix.mkdir workspace 0o755;
  Unix.mkdir extra_dir 0o755;
  let extra_file = Filename.concat extra_dir "notes.txt" in
  let oc = open_out extra_file in
  output_string oc "extra content";
  close_out oc;
  Fun.protect
    (fun () ->
      let tool =
        Tools_builtin.file_read ~workspace ~workspace_only:true
          ~extra_allowed_paths:[ extra_dir ]
      in
      let out =
        Lwt_main.run (tool.invoke (`Assoc [ ("path", `String extra_file) ]))
      in
      Alcotest.(check string) "extra allowed path readable" "extra content" out;
      let tool_no_extra =
        Tools_builtin.file_read ~workspace ~workspace_only:true
          ~extra_allowed_paths:[]
      in
      let out2 =
        Lwt_main.run
          (tool_no_extra.invoke (`Assoc [ ("path", `String extra_file) ]))
      in
      Alcotest.(check bool)
        "without extra_allowed_paths blocked" true
        (Test_helpers.string_contains out2 "outside workspace"))
    ~finally:(fun () ->
      (try Unix.unlink extra_file with _ -> ());
      (try Unix.rmdir extra_dir with _ -> ());
      try Unix.rmdir workspace with _ -> ())

let test_is_localhost_url_accepts_loopback_hosts () =
  Alcotest.(check bool)
    "localhost accepted" true
    (Tools_builtin.is_localhost_url "http://localhost:3000/health");
  Alcotest.(check bool)
    "ipv4 loopback accepted" true
    (Tools_builtin.is_localhost_url "https://127.0.0.1/api");
  Alcotest.(check bool)
    "ipv6 loopback accepted" true
    (Tools_builtin.is_localhost_url "http://[::1]/health")

let test_is_localhost_url_rejects_host_spoofing () =
  Alcotest.(check bool)
    "suffix host rejected" false
    (Tools_builtin.is_localhost_url "http://localhost.evil.com/x");
  Alcotest.(check bool)
    "userinfo spoof rejected" false
    (Tools_builtin.is_localhost_url "http://localhost@evil.com/x")

let test_file_read_large_file_requires_paged_read () =
  with_temp_workspace (fun workspace ->
      let full_path = Filename.concat workspace "big.txt" in
      let oc = open_out full_path in
      output_string oc (String.make 60000 'a');
      close_out oc;
      let tool =
        Tools_builtin.file_read ~workspace ~workspace_only:true
          ~extra_allowed_paths:[]
      in
      let out =
        Lwt_main.run (tool.invoke (`Assoc [ ("path", `String "big.txt") ]))
      in
      Alcotest.(check bool)
        "oversized read blocked with guidance" true
        (Test_helpers.string_contains out "offset/limit"))

let test_file_read_paged_window_with_line_numbers () =
  with_temp_workspace (fun workspace ->
      let full_path = Filename.concat workspace "paged.txt" in
      let oc = open_out full_path in
      output_string oc "one\ntwo\nthree\nfour\nfive";
      close_out oc;
      let tool =
        Tools_builtin.file_read ~workspace ~workspace_only:true
          ~extra_allowed_paths:[]
      in
      let out =
        Lwt_main.run
          (tool.invoke
             (`Assoc
                [
                  ("path", `String "paged.txt");
                  ("offset", `Int 2);
                  ("limit", `Int 2);
                ]))
      in
      Alcotest.(check bool)
        "includes line 2" true
        (Test_helpers.string_contains out "2: two");
      Alcotest.(check bool)
        "includes line 3" true
        (Test_helpers.string_contains out "3: three");
      Alcotest.(check bool)
        "omits line 1" false
        (Test_helpers.string_contains out "1: one"))

let test_file_read_paged_truncates_pathological_long_line () =
  with_temp_workspace (fun workspace ->
      let full_path = Filename.concat workspace "huge_line.txt" in
      let oc = open_out full_path in
      output_string oc (String.make 80000 'a');
      close_out oc;
      let tool =
        Tools_builtin.file_read ~workspace ~workspace_only:true
          ~extra_allowed_paths:[]
      in
      let out =
        Lwt_main.run
          (tool.invoke
             (`Assoc
                [
                  ("path", `String "huge_line.txt");
                  ("offset", `Int 1);
                  ("limit", `Int 1);
                ]))
      in
      Alcotest.(check bool)
        "long line is truncated" true
        (Test_helpers.string_contains out "(truncated");
      Alcotest.(check bool)
        "truncation note included" true
        (Test_helpers.string_contains out "long lines are truncated");
      Alcotest.(check bool)
        "paged output stays bounded" true
        (String.length out < 10000))

let test_file_read_rejects_invalid_offset_limit () =
  with_temp_workspace (fun workspace ->
      let full_path = Filename.concat workspace "window.txt" in
      let oc = open_out full_path in
      output_string oc "one\ntwo\nthree";
      close_out oc;
      let tool =
        Tools_builtin.file_read ~workspace ~workspace_only:true
          ~extra_allowed_paths:[]
      in
      let offset_err =
        Lwt_main.run
          (tool.invoke
             (`Assoc
                [
                  ("path", `String "window.txt");
                  ("offset", `Int 0);
                  ("limit", `Int 1);
                ]))
      in
      Alcotest.(check bool)
        "offset validation error" true
        (Test_helpers.string_contains offset_err "offset must be >= 1");
      let limit_low_err =
        Lwt_main.run
          (tool.invoke
             (`Assoc
                [
                  ("path", `String "window.txt");
                  ("offset", `Int 1);
                  ("limit", `Int 0);
                ]))
      in
      Alcotest.(check bool)
        "limit lower bound validation error" true
        (Test_helpers.string_contains limit_low_err "limit must be >= 1");
      let limit_high_err =
        Lwt_main.run
          (tool.invoke
             (`Assoc
                [
                  ("path", `String "window.txt");
                  ("offset", `Int 1);
                  ("limit", `Int 2001);
                ]))
      in
      Alcotest.(check bool)
        "limit upper bound validation error" true
        (Test_helpers.string_contains limit_high_err "limit must be <= 2000"))

let test_file_read_rejects_symlink_escape () =
  let base = Filename.get_temp_dir_name () in
  let workspace =
    Filename.concat base
      (Printf.sprintf "clawq_symlink_ws_%d_%d" (Unix.getpid ()) (Random.bits ()))
  in
  let outside_dir =
    Filename.concat base
      (Printf.sprintf "clawq_symlink_out_%d_%d" (Unix.getpid ())
         (Random.bits ()))
  in
  Unix.mkdir workspace 0o755;
  Unix.mkdir outside_dir 0o755;
  let outside_file = Filename.concat outside_dir "secret.txt" in
  let oc = open_out outside_file in
  output_string oc "secret";
  close_out oc;
  let link_path = Filename.concat workspace "escape.txt" in
  Unix.symlink outside_file link_path;
  Fun.protect
    (fun () ->
      let tool =
        Tools_builtin.file_read ~workspace ~workspace_only:true
          ~extra_allowed_paths:[]
      in
      let out =
        Lwt_main.run (tool.invoke (`Assoc [ ("path", `String "escape.txt") ]))
      in
      Alcotest.(check bool)
        "symlink escape blocked" true
        (Test_helpers.string_contains out "outside workspace"))
    ~finally:(fun () ->
      (try Unix.unlink link_path with _ -> ());
      (try Unix.unlink outside_file with _ -> ());
      (try Unix.rmdir outside_dir with _ -> ());
      try Unix.rmdir workspace with _ -> ())

(* B645 regression: file_write/file_append/file_edit/file_edit_lines must
   refuse any path under a .backlog/ directory and direct the agent to the
   `bl` CLI instead. Triggered by a teams agent that wrote a phantom B605
   bug file directly into /home/xertrov/src/clawq/.backlog/bugs/ with
   hallucinated content. *)
let test_file_write_rejects_backlog_path () =
  with_temp_workspace (fun workspace ->
      let backlog_path = ".backlog/bugs/B999-phantom.todo" in
      let tool =
        Tools_builtin.file_write ~workspace ~workspace_only:false
          ~extra_allowed_paths:[]
      in
      let out =
        Lwt_main.run
          (tool.invoke
             (`Assoc
                [
                  ("path", `String backlog_path);
                  ("content", `String "hallucinated bug content");
                ]))
      in
      Alcotest.(check bool)
        "refuses backlog path" true
        (Test_helpers.string_contains out "refusing to file_write");
      Alcotest.(check bool)
        "points to bl bug --simple" true
        (Test_helpers.string_contains out "bl bug --simple");
      Alcotest.(check bool)
        "file was NOT created" false
        (Sys.file_exists backlog_path))

let test_file_append_rejects_nested_backlog_path () =
  with_temp_workspace (fun workspace ->
      let nested = "subdir/.backlog/ideas/I999.todo" in
      let tool =
        Tools_builtin.file_append ~workspace ~workspace_only:false
          ~extra_allowed_paths:[]
      in
      let out =
        Lwt_main.run
          (tool.invoke
             (`Assoc [ ("path", `String nested); ("content", `String "x") ]))
      in
      Alcotest.(check bool)
        "refuses nested .backlog" true
        (Test_helpers.string_contains out ".backlog/ directory is managed");
      Alcotest.(check bool) "did not create file" false (Sys.file_exists nested))

let test_file_write_allows_nonbacklog_path () =
  with_temp_workspace (fun workspace ->
      let safe_path = "notes.txt" in
      let tool =
        Tools_builtin.file_write ~workspace ~workspace_only:false
          ~extra_allowed_paths:[]
      in
      let out =
        Lwt_main.run
          (tool.invoke
             (`Assoc [ ("path", `String safe_path); ("content", `String "ok") ]))
      in
      Alcotest.(check bool)
        "non-backlog write succeeds" true
        (Test_helpers.string_contains out "Written"))

let test_file_append_creates_and_appends () =
  with_temp_workspace (fun workspace ->
      let tool =
        Tools_builtin.file_append ~workspace ~workspace_only:true
          ~extra_allowed_paths:[]
      in
      ignore
        (Lwt_main.run
           (tool.invoke
              (`Assoc
                 [
                   ("path", `String "append.txt"); ("content", `String "hello");
                 ])));
      ignore
        (Lwt_main.run
           (tool.invoke
              (`Assoc
                 [
                   ("path", `String "append.txt"); ("content", `String " world");
                 ])));
      let full_path = Filename.concat workspace "append.txt" in
      let ic = open_in full_path in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      Alcotest.(check string) "append content" "hello world" content)

let test_file_edit_replace_all () =
  with_temp_workspace (fun workspace ->
      let full_path = Filename.concat workspace "replace_all.txt" in
      let oc = open_out full_path in
      output_string oc "a b a b";
      close_out oc;
      let tool =
        Tools_builtin.file_edit ~workspace ~workspace_only:true
          ~extra_allowed_paths:[]
      in
      ignore
        (Lwt_main.run
           (tool.invoke
              (`Assoc
                 [
                   ("path", `String "replace_all.txt");
                   ("old_text", `String "a");
                   ("new_text", `String "z");
                   ("replace_all", `Bool true);
                 ])));
      let ic = open_in full_path in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      Alcotest.(check string) "all replaced" "z b z b" content)

let test_file_edit_lines_replaces_range () =
  with_temp_workspace (fun workspace ->
      let full_path = Filename.concat workspace "lines.txt" in
      let oc = open_out full_path in
      output_string oc "one\ntwo\nthree\nfour";
      close_out oc;
      let tool =
        Tools_builtin.file_edit_lines ~workspace ~workspace_only:true
          ~extra_allowed_paths:[]
      in
      ignore
        (Lwt_main.run
           (tool.invoke
              (`Assoc
                 [
                   ("path", `String "lines.txt");
                   ("start_line", `Int 2);
                   ("end_line", `Int 3);
                   ("content", `String "TWO\nTHREE");
                 ])));
      let ic = open_in full_path in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      Alcotest.(check string)
        "line range replaced" "one\nTWO\nTHREE\nfour" content)

let find_tool_exn registry name =
  match Tool_registry.find registry name with
  | Some tool -> tool
  | None -> Alcotest.fail ("expected tool not found: " ^ name)

let test_register_all_file_read_path_policy_tracks_security_config () =
  let base = Filename.get_temp_dir_name () in
  let workspace =
    Filename.concat base
      (Printf.sprintf "clawq_reg_ws_%d_%d" (Unix.getpid ()) (Random.bits ()))
  in
  let extra_dir =
    Filename.concat base
      (Printf.sprintf "clawq_reg_extra_%d_%d" (Unix.getpid ()) (Random.bits ()))
  in
  Unix.mkdir workspace 0o755;
  Unix.mkdir extra_dir 0o755;
  let extra_file = Filename.concat extra_dir "outside.txt" in
  let oc = open_out extra_file in
  output_string oc "outside content";
  close_out oc;
  let mk_cfg ~dynamic_enabled ~workspace_only ~extra_allowed_paths =
    {
      Runtime_config.default with
      workspace;
      prompt = { Runtime_config.default.prompt with dynamic_enabled };
      security =
        {
          Runtime_config.default.security with
          workspace_only;
          extra_allowed_paths;
        };
    }
  in
  Fun.protect
    (fun () ->
      let cfg_blocked =
        mk_cfg ~dynamic_enabled:false ~workspace_only:true
          ~extra_allowed_paths:[]
      in
      let blocked_registry = Tool_registry.create () in
      Tools_builtin.register_all ~config:cfg_blocked
        ~sandbox:(mk_none_sandbox ~workspace:"/tmp" ())
        blocked_registry;
      let blocked_tool = find_tool_exn blocked_registry "file_read" in
      let blocked_out =
        Lwt_main.run
          (blocked_tool.invoke (`Assoc [ ("path", `String extra_file) ]))
      in
      Alcotest.(check bool)
        "workspace_only blocks outside file" true
        (Test_helpers.string_contains blocked_out "outside workspace");

      let cfg_extra_allowed =
        mk_cfg ~dynamic_enabled:true ~workspace_only:true
          ~extra_allowed_paths:[ extra_dir ]
      in
      let extra_registry = Tool_registry.create () in
      Tools_builtin.register_all ~config:cfg_extra_allowed
        ~sandbox:(mk_none_sandbox ~workspace:"/tmp" ())
        extra_registry;
      let extra_tool = find_tool_exn extra_registry "file_read" in
      let extra_out =
        Lwt_main.run
          (extra_tool.invoke (`Assoc [ ("path", `String extra_file) ]))
      in
      Alcotest.(check string)
        "extra_allowed_paths permits outside file" "outside content" extra_out;

      let cfg_workspace_off =
        mk_cfg ~dynamic_enabled:true ~workspace_only:false
          ~extra_allowed_paths:[]
      in
      let open_registry = Tool_registry.create () in
      Tools_builtin.register_all ~config:cfg_workspace_off
        ~sandbox:(mk_none_sandbox ~workspace:"/tmp" ())
        open_registry;
      let open_tool = find_tool_exn open_registry "file_read" in
      let open_out =
        Lwt_main.run
          (open_tool.invoke (`Assoc [ ("path", `String extra_file) ]))
      in
      Alcotest.(check string)
        "workspace_only=false allows outside file" "outside content" open_out)
    ~finally:(fun () ->
      (try Unix.unlink extra_file with _ -> ());
      (try Unix.rmdir extra_dir with _ -> ());
      try Unix.rmdir workspace with _ -> ())

let test_register_all_shell_path_policy_tracks_security_config () =
  let base = Filename.get_temp_dir_name () in
  let workspace =
    Filename.concat base
      (Printf.sprintf "clawq_reg_shell_ws_%d_%d" (Unix.getpid ())
         (Random.bits ()))
  in
  let extra_dir =
    Filename.concat base
      (Printf.sprintf "clawq_reg_shell_extra_%d_%d" (Unix.getpid ())
         (Random.bits ()))
  in
  Unix.mkdir workspace 0o755;
  Unix.mkdir extra_dir 0o755;
  let mk_cfg ~dynamic_enabled ~workspace_only ~extra_allowed_paths =
    {
      Runtime_config.default with
      workspace;
      prompt = { Runtime_config.default.prompt with dynamic_enabled };
      security =
        {
          Runtime_config.default.security with
          workspace_only;
          extra_allowed_paths;
        };
    }
  in
  Fun.protect
    (fun () ->
      let cfg_blocked =
        mk_cfg ~dynamic_enabled:false ~workspace_only:true
          ~extra_allowed_paths:[]
      in
      let blocked_registry = Tool_registry.create () in
      Tools_builtin.register_all ~config:cfg_blocked
        ~sandbox:(mk_none_sandbox ~workspace:"/tmp" ())
        blocked_registry;
      let blocked_tool = find_tool_exn blocked_registry "shell_exec" in
      let blocked_out =
        Lwt_main.run
          (blocked_tool.invoke
             (`Assoc [ ("command", `String ("ls " ^ extra_dir)) ]))
      in
      Alcotest.(check bool)
        "workspace_only blocks outside path arg" true
        (Test_helpers.string_contains blocked_out
           "disallowed in workspace_only mode");

      let cfg_extra_allowed =
        mk_cfg ~dynamic_enabled:true ~workspace_only:true
          ~extra_allowed_paths:[ extra_dir ]
      in
      let extra_registry = Tool_registry.create () in
      Tools_builtin.register_all ~config:cfg_extra_allowed
        ~sandbox:(mk_none_sandbox ~workspace:"/tmp" ())
        extra_registry;
      let extra_tool = find_tool_exn extra_registry "shell_exec" in
      let extra_out =
        Lwt_main.run
          (extra_tool.invoke
             (`Assoc [ ("command", `String ("ls " ^ extra_dir)) ]))
      in
      Alcotest.(check bool)
        "extra_allowed_paths permits shell path arg" true
        (Test_helpers.string_contains extra_out "exit_code: 0"))
    ~finally:(fun () ->
      (try Unix.rmdir extra_dir with _ -> ());
      try Unix.rmdir workspace with _ -> ())

let test_shell_exec_rejects_missing_command () =
  with_temp_workspace (fun workspace ->
      let sandbox = mk_none_sandbox ~workspace () in
      let cfg =
        {
          Runtime_config.default with
          workspace;
          security =
            {
              Runtime_config.default.security with
              workspace_only = false;
              extra_allowed_paths = [];
            };
        }
      in
      let registry = Tool_registry.create () in
      Tools_builtin.register_all ~config:cfg ~sandbox registry;
      let tool = find_tool_exn registry "shell_exec" in
      let out = Lwt_main.run (tool.Tool.invoke (`Assoc [])) in
      Alcotest.(check bool)
        "error mentions command" true
        (Test_helpers.string_contains out "command");
      Alcotest.(check bool)
        "error includes Example" true
        (Test_helpers.string_contains out "Example"))

let test_shell_exec_rejects_null_command () =
  with_temp_workspace (fun workspace ->
      let sandbox = mk_none_sandbox ~workspace () in
      let cfg =
        {
          Runtime_config.default with
          workspace;
          security =
            {
              Runtime_config.default.security with
              workspace_only = false;
              extra_allowed_paths = [];
            };
        }
      in
      let registry = Tool_registry.create () in
      Tools_builtin.register_all ~config:cfg ~sandbox registry;
      let tool = find_tool_exn registry "shell_exec" in
      let out =
        Lwt_main.run (tool.Tool.invoke (`Assoc [ ("command", `Null) ]))
      in
      Alcotest.(check bool)
        "error mentions command" true
        (Test_helpers.string_contains out "command");
      Alcotest.(check bool)
        "error includes Example" true
        (Test_helpers.string_contains out "Example"))

let test_validate_required_params_catches_missing () =
  let mock_tool : Tool.t =
    {
      name = "test_tool";
      description = "test";
      parameters_schema =
        `Assoc
          [
            ( "properties",
              `Assoc
                [
                  ("foo", `Assoc [ ("type", `String "string") ]);
                  ("bar", `Assoc [ ("type", `String "string") ]);
                ] );
            ("required", `List [ `String "foo"; `String "bar" ]);
          ];
      invoke = (fun ?context:_ _args -> Lwt.return "ok");
      invoke_stream = None;
      risk_level = Tool.Low;
      deferred = false;
    }
  in
  (match Tool.validate_required_params mock_tool (`Assoc []) with
  | Error msg ->
      Alcotest.(check bool)
        "mentions foo" true
        (Test_helpers.string_contains msg "'foo'");
      Alcotest.(check bool)
        "mentions bar" true
        (Test_helpers.string_contains msg "'bar'");
      Alcotest.(check bool)
        "mentions tool name" true
        (Test_helpers.string_contains msg "test_tool")
  | Ok () -> Alcotest.fail "expected Error for missing required params");
  match Tool.validate_required_params mock_tool (`Assoc [ ("foo", `Null) ]) with
  | Error msg ->
      Alcotest.(check bool)
        "null treated as missing" true
        (Test_helpers.string_contains msg "'foo'")
  | Ok () -> Alcotest.fail "expected Error for null required param"

(* B622: when the model emits the same missing-param failure repeatedly,
   the error escalates so the model gets a stronger nudge. Pass-then-fail
   resets the counter; fail-with-different-tool also resets per-key. *)
let test_missing_required_error_escalates_on_repeats () =
  let tool : Tool.t =
    {
      name = "shell_exec";
      description = "Run a shell command";
      parameters_schema =
        `Assoc
          [
            ( "properties",
              `Assoc [ ("command", `Assoc [ ("type", `String "string") ]) ] );
            ("required", `List [ `String "command" ]);
          ];
      invoke = (fun ?context:_ _args -> Lwt.return "ok");
      invoke_stream = None;
      risk_level = Tool.Low;
      deferred = false;
    }
  in
  let agent = Agent.create ~config:Runtime_config.default () in
  let go args = Agent.validate_required_with_escalation agent tool args in
  (match go (`Assoc []) with
  | Error msg ->
      Alcotest.(check bool)
        "level 0: no SECOND/STOP escalation prefix" true
        ((not (Test_helpers.string_contains msg "SECOND"))
        && not (Test_helpers.string_contains msg "STOP"));
      Alcotest.(check bool)
        "level 0: warns that repeating will abort" true
        (Test_helpers.string_contains msg "abort this turn")
  | Ok () -> Alcotest.fail "expected Error on first repeat");
  (match go (`Assoc []) with
  | Error msg ->
      Alcotest.(check bool)
        "level 1: SECOND notice present" true
        (Test_helpers.string_contains msg "SECOND");
      Alcotest.(check bool)
        "level 1: FINAL WARNING present" true
        (Test_helpers.string_contains msg "FINAL WARNING")
  | Ok () -> Alcotest.fail "expected Error on second repeat");
  (match go (`Assoc []) with
  | Error msg ->
      Alcotest.(check bool)
        "level 2+: STOP notice present" true
        (Test_helpers.string_contains msg "STOP");
      Alcotest.(check bool)
        "level 2+: ABORT notice present" true
        (Test_helpers.string_contains msg "ABORT")
  | Ok () -> Alcotest.fail "expected Error on third repeat");
  (* B677: after the threshold (default 3), the agent's hard_abort_reason
     should be set so the turn loop can terminate. *)
  Alcotest.(check bool)
    "B677: hard_abort_reason set after 3 identical failures" true
    (agent.hard_abort_reason <> None);
  (* Successful call clears the counter *)
  (match go (`Assoc [ ("command", `String "ls") ]) with
  | Ok () -> ()
  | Error msg -> Alcotest.fail ("expected Ok after providing arg: " ^ msg));
  match go (`Assoc []) with
  | Error msg ->
      Alcotest.(check bool)
        "back to level 0 after successful intervening call" true
        ((not (Test_helpers.string_contains msg "SECOND"))
        && not (Test_helpers.string_contains msg "STOP"))
  | Ok () -> Alcotest.fail "expected Error after reset"

(* B677: dedicated test — circuit breaker arms hard_abort_reason at the
   configured threshold. *)
let test_b677_circuit_breaker_arms_at_threshold () =
  let tool : Tool.t =
    {
      name = "web_search";
      description = "Web search";
      parameters_schema =
        `Assoc
          [
            ( "properties",
              `Assoc [ ("query", `Assoc [ ("type", `String "string") ]) ] );
            ("required", `List [ `String "query" ]);
          ];
      invoke = (fun ?context:_ _args -> Lwt.return "ok");
      invoke_stream = None;
      risk_level = Tool.Low;
      deferred = false;
    }
  in
  let agent = Agent.create ~config:Runtime_config.default () in
  let go args = Agent.validate_required_with_escalation agent tool args in
  (* Default threshold is 3. First two should not arm. *)
  ignore (go (`Assoc []));
  Alcotest.(check bool)
    "not armed after 1 failure" true
    (agent.hard_abort_reason = None);
  ignore (go (`Assoc []));
  Alcotest.(check bool)
    "not armed after 2 failures" true
    (agent.hard_abort_reason = None);
  ignore (go (`Assoc []));
  Alcotest.(check bool)
    "armed after 3 identical failures" true
    (agent.hard_abort_reason <> None);
  (* Reset on successful call. *)
  let _ = go (`Assoc [ ("query", `String "hello") ]) in
  (* hard_abort_reason is consumed by the turn loop in practice; here we
     verify the counter is reset so a new streak starts from 0. *)
  Alcotest.(check int)
    "counter reset to 0 after success" 0 agent.last_missing_required_count;
  ignore (go (`Assoc []));
  Alcotest.(check int)
    "next failure starts a fresh streak (count=1)" 1
    agent.last_missing_required_count

(* B677: changing tool or missing-param key resets the streak. *)
let test_b677_resets_on_different_key () =
  let tool_a : Tool.t =
    {
      name = "shell_exec";
      description = "Run shell";
      parameters_schema =
        `Assoc
          [
            ( "properties",
              `Assoc [ ("command", `Assoc [ ("type", `String "string") ]) ] );
            ("required", `List [ `String "command" ]);
          ];
      invoke = (fun ?context:_ _args -> Lwt.return "ok");
      invoke_stream = None;
      risk_level = Tool.Low;
      deferred = false;
    }
  in
  let tool_b : Tool.t = { tool_a with name = "use_skill" } in
  let agent = Agent.create ~config:Runtime_config.default () in
  ignore (Agent.validate_required_with_escalation agent tool_a (`Assoc []));
  ignore (Agent.validate_required_with_escalation agent tool_a (`Assoc []));
  Alcotest.(check int)
    "count=2 after two failures on tool_a" 2 agent.last_missing_required_count;
  ignore (Agent.validate_required_with_escalation agent tool_b (`Assoc []));
  Alcotest.(check int)
    "count reset to 1 when failing on different tool" 1
    agent.last_missing_required_count;
  Alcotest.(check bool)
    "circuit breaker not armed (streak interrupted)" true
    (agent.hard_abort_reason = None)

let test_validate_required_params_passes_valid () =
  let mock_tool : Tool.t =
    {
      name = "test_tool";
      description = "test";
      parameters_schema =
        `Assoc
          [
            ( "properties",
              `Assoc [ ("cmd", `Assoc [ ("type", `String "string") ]) ] );
            ("required", `List [ `String "cmd" ]);
          ];
      invoke = (fun ?context:_ _args -> Lwt.return "ok");
      invoke_stream = None;
      risk_level = Tool.Low;
      deferred = false;
    }
  in
  match
    Tool.validate_required_params mock_tool
      (`Assoc [ ("cmd", `String "hello") ])
  with
  | Ok () -> ()
  | Error msg -> Alcotest.fail ("expected Ok, got Error: " ^ msg)

let test_register_all_with_db_registers_memory_and_bg_tools () =
  let db = Memory.init ~db_path:":memory:" () in
  let registry = Tool_registry.create () in
  let cfg = Runtime_config.default in
  let sandbox = mk_none_sandbox ~workspace:"/tmp" () in
  Tools_builtin.register_all ~config:cfg ~sandbox ~db:(Some db) registry;
  List.iter
    (fun name ->
      Alcotest.(check bool)
        (name ^ " registered with db")
        true
        (Tool_registry.find registry name <> None))
    [
      "memory_store";
      "memory_recall";
      "memory_forget";
      "memory_list";
      "history_search";
      "background_task_enqueue";
      "background_task_list";
      "background_task_wait";
      "background_task_logs";
      "background_task_transcript";
      "background_task_resume";
      "background_task_send_message";
      "delegate";
      "background_task_cancel";
    ];
  let registry2 = Tool_registry.create () in
  Tools_builtin.register_all ~config:cfg ~sandbox registry2;
  List.iter
    (fun name ->
      Alcotest.(check bool)
        (name ^ " absent without db")
        true
        (Tool_registry.find registry2 name = None))
    [ "memory_store"; "delegate"; "background_task_enqueue"; "history_search" ]

let suite =
  [
    Alcotest.test_case "path traversal rejected" `Quick
      test_path_traversal_rejected;
    Alcotest.test_case "shell allowlist rejects" `Quick
      test_shell_allowlist_rejects_disallowed;
    Alcotest.test_case "shell allowlist allows" `Quick
      test_shell_allowlist_allows_command;
    Alcotest.test_case "shell chaining rejected" `Quick
      test_shell_rejects_command_chaining;
    Alcotest.test_case "shell dollar expansion rejected" `Quick
      test_shell_rejects_dollar_expansion;
    Alcotest.test_case "shell quoted args" `Quick test_shell_handles_quoted_args;
    Alcotest.test_case "shell streams stdout chunks" `Quick
      test_shell_streams_stdout_chunks;
    Alcotest.test_case "shell streams stderr chunks" `Quick
      test_shell_streams_stderr_chunks;
    Alcotest.test_case "shell absolute path arg blocked" `Quick
      test_shell_rejects_absolute_path_arg;
    Alcotest.test_case "shell url arg blocked" `Quick test_shell_rejects_url_arg;
    Alcotest.test_case "shell binary path bypass blocked" `Quick
      test_shell_rejects_binary_path_bypass;
    Alcotest.test_case "shell assigned absolute path blocked" `Quick
      test_shell_rejects_option_assigned_absolute_path;
    Alcotest.test_case "shell git network subcommand blocked" `Quick
      test_shell_rejects_git_network_subcommand;
    Alcotest.test_case "shell honors explicit cwd" `Quick
      test_shell_honors_explicit_cwd;
    Alcotest.test_case "shell rejects disallowed cwd" `Quick
      test_shell_rejects_disallowed_cwd;
    Alcotest.test_case "shell extra_allowed_paths grants access" `Quick
      test_shell_extra_allowed_paths_grants_access;
    Alcotest.test_case "file_edit first match" `Quick
      test_file_edit_replaces_first_match;
    Alcotest.test_case "file_read uses configured workspace" `Quick
      test_file_read_uses_configured_workspace_root;
    Alcotest.test_case "grep supports regex and include alias" `Quick
      test_grep_supports_regex_and_include_alias;
    Alcotest.test_case "grep single file respects include filter" `Quick
      test_grep_single_file_respects_include_filter;
    Alcotest.test_case "grep honors case_sensitive flag" `Quick
      test_grep_honors_case_sensitive_flag;
    Alcotest.test_case "glob with root subdirectory" `Quick
      test_glob_with_root_subdirectory;
    Alcotest.test_case "glob invalid root returns error" `Quick
      test_glob_invalid_root_returns_error;
    Alcotest.test_case "glob root is file returns error" `Quick
      test_glob_root_is_file_returns_error;
    Alcotest.test_case "list_dir with custom path" `Quick
      test_list_dir_with_custom_path;
    Alcotest.test_case "list_dir nonexistent path returns error" `Quick
      test_list_dir_nonexistent_path_returns_error;
    Alcotest.test_case "list_dir file path returns error" `Quick
      test_list_dir_file_path_returns_error;
    Alcotest.test_case "grep with directory arg" `Quick
      test_grep_with_directory_arg;
    Alcotest.test_case "grep invalid path returns error" `Quick
      test_grep_invalid_path_returns_error;
    Alcotest.test_case "file_read large file requires paging" `Quick
      test_file_read_large_file_requires_paged_read;
    Alcotest.test_case "file_read paged window" `Quick
      test_file_read_paged_window_with_line_numbers;
    Alcotest.test_case "file_read paged truncates long line" `Quick
      test_file_read_paged_truncates_pathological_long_line;
    Alcotest.test_case "file_read rejects invalid offset/limit" `Quick
      test_file_read_rejects_invalid_offset_limit;
    Alcotest.test_case "file_read rejects symlink escape" `Quick
      test_file_read_rejects_symlink_escape;
    Alcotest.test_case "B645: file_write refuses .backlog/ path" `Quick
      test_file_write_rejects_backlog_path;
    Alcotest.test_case "B645: file_append refuses nested .backlog/ path" `Quick
      test_file_append_rejects_nested_backlog_path;
    Alcotest.test_case "B645: file_write still allows non-backlog paths" `Quick
      test_file_write_allows_nonbacklog_path;
    Alcotest.test_case "file_append creates and appends" `Quick
      test_file_append_creates_and_appends;
    Alcotest.test_case "file_edit replace_all" `Quick test_file_edit_replace_all;
    Alcotest.test_case "file_edit_lines range replace" `Quick
      test_file_edit_lines_replaces_range;
    Alcotest.test_case "register_all file_read path policy" `Quick
      test_register_all_file_read_path_policy_tracks_security_config;
    Alcotest.test_case "register_all shell path policy" `Quick
      test_register_all_shell_path_policy_tracks_security_config;
    Alcotest.test_case "extra_allowed_paths grants access" `Quick
      test_extra_allowed_paths_grants_access;
    Alcotest.test_case "transcribe path policy" `Quick
      test_transcribe_rejects_outside_workspace;
    Alcotest.test_case "localhost url accepts loopback" `Quick
      test_is_localhost_url_accepts_loopback_hosts;
    Alcotest.test_case "localhost url rejects spoofing" `Quick
      test_is_localhost_url_rejects_host_spoofing;
    Alcotest.test_case "register_all with db registers memory and bg tools"
      `Quick test_register_all_with_db_registers_memory_and_bg_tools;
    Alcotest.test_case "shell_exec rejects missing command" `Quick
      test_shell_exec_rejects_missing_command;
    Alcotest.test_case "shell_exec rejects null command" `Quick
      test_shell_exec_rejects_null_command;
    Alcotest.test_case "validate_required_params catches missing" `Quick
      test_validate_required_params_catches_missing;
    Alcotest.test_case "validate_required_params passes valid" `Quick
      test_validate_required_params_passes_valid;
    Alcotest.test_case
      "B622: missing-required error escalates on repeats, resets on success"
      `Quick test_missing_required_error_escalates_on_repeats;
    Alcotest.test_case
      "B677: circuit breaker arms hard_abort_reason at threshold" `Quick
      test_b677_circuit_breaker_arms_at_threshold;
    Alcotest.test_case
      "B677: streak resets when failing on a different tool/key" `Quick
      test_b677_resets_on_different_key;
  ]
