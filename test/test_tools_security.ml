let contains hay needle =
  let hlen = String.length hay in
  let nlen = String.length needle in
  let rec loop i =
    if i + nlen > hlen then false
    else if String.sub hay i nlen = needle then true
    else loop (i + 1)
  in
  nlen = 0 || loop 0

let with_temp_workspace f =
  let base = Filename.get_temp_dir_name () in
  let dir =
    Filename.concat base
      (Printf.sprintf "clawq_tools_%d_%d" (Unix.getpid ()) (Random.bits ()))
  in
  (try Unix.rmdir dir with _ -> ());
  Unix.mkdir dir 0o755;
  let cwd = Sys.getcwd () in
  Fun.protect
    (fun () ->
      Sys.chdir dir;
      f dir)
    ~finally:(fun () ->
      Sys.chdir cwd;
      try Unix.rmdir dir with _ -> ())

let test_path_traversal_rejected () =
  with_temp_workspace (fun workspace ->
      Alcotest.(check bool)
        "../ rejected" false
        (Tools_builtin.is_path_safe ~workspace "../etc/passwd");
      Alcotest.(check bool)
        "prefix escape rejected" false
        (Tools_builtin.is_path_safe ~workspace (workspace ^ "2/outside.txt")))

let test_shell_allowlist_rejects_disallowed () =
  let tool =
    Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
      ~allowed_commands:[ "ls" ] ~extra_allowed_paths:[]
      ~sandbox:{ Sandbox.backend = Sandbox.None; workspace = Sys.getcwd () }
  in
  let args = `Assoc [ ("command", `String "echo hi") ] in
  let out = Lwt_main.run (tool.invoke args) in
  Alcotest.(check bool) "blocked" true (contains out "not in the allowlist")

let test_shell_allowlist_allows_command () =
  let tool =
    Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
      ~allowed_commands:[ "ls" ] ~extra_allowed_paths:[]
      ~sandbox:{ Sandbox.backend = Sandbox.None; workspace = Sys.getcwd () }
  in
  let args = `Assoc [ ("command", `String "ls .") ] in
  let out = Lwt_main.run (tool.invoke args) in
  Alcotest.(check bool) "success" true (contains out "exit_code: 0")

let test_shell_rejects_command_chaining () =
  let tool =
    Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
      ~allowed_commands:[ "ls" ] ~extra_allowed_paths:[]
      ~sandbox:{ Sandbox.backend = Sandbox.None; workspace = Sys.getcwd () }
  in
  let args = `Assoc [ ("command", `String "ls && whoami") ] in
  let out = Lwt_main.run (tool.invoke args) in
  Alcotest.(check bool)
    "unsafe syntax blocked" true
    (contains out "unsafe shell syntax")

let test_shell_rejects_dollar_expansion () =
  let tool =
    Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
      ~allowed_commands:[ "ls" ] ~extra_allowed_paths:[]
      ~sandbox:{ Sandbox.backend = Sandbox.None; workspace = Sys.getcwd () }
  in
  let args = `Assoc [ ("command", `String "ls $HOME") ] in
  let out = Lwt_main.run (tool.invoke args) in
  Alcotest.(check bool)
    "dollar expansion blocked" true
    (contains out "unsafe shell syntax")

let test_shell_handles_quoted_args () =
  let tool =
    Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
      ~allowed_commands:[ "ls" ] ~extra_allowed_paths:[]
      ~sandbox:{ Sandbox.backend = Sandbox.None; workspace = Sys.getcwd () }
  in
  let args = `Assoc [ ("command", `String "ls \".\"") ] in
  let out = Lwt_main.run (tool.invoke args) in
  Alcotest.(check bool) "quoted arg success" true (contains out "exit_code: 0")

let test_shell_streams_stdout_chunks () =
  let tool =
    Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
      ~allowed_commands:[ "echo" ] ~extra_allowed_paths:[]
      ~sandbox:{ Sandbox.backend = Sandbox.None; workspace = Sys.getcwd () }
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
        "result still includes stdout" true (contains out "hello")

let test_shell_streams_stderr_chunks () =
  let tool =
    Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
      ~allowed_commands:[ "ls" ] ~extra_allowed_paths:[]
      ~sandbox:{ Sandbox.backend = Sandbox.None; workspace = Sys.getcwd () }
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
        (contains (Buffer.contents chunks) "definitely-missing-file");
      Alcotest.(check bool)
        "result still includes stderr" true
        (contains out "definitely-missing-file")

let test_shell_rejects_absolute_path_arg () =
  let tool =
    Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
      ~allowed_commands:[ "cat" ] ~extra_allowed_paths:[]
      ~sandbox:{ Sandbox.backend = Sandbox.None; workspace = Sys.getcwd () }
  in
  let args = `Assoc [ ("command", `String "cat /etc/passwd") ] in
  let out = Lwt_main.run (tool.invoke args) in
  Alcotest.(check bool)
    "absolute path blocked" true
    (contains out "disallowed in workspace_only mode")

let test_shell_rejects_url_arg () =
  let tool =
    Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
      ~allowed_commands:[ "git" ] ~extra_allowed_paths:[]
      ~sandbox:{ Sandbox.backend = Sandbox.None; workspace = Sys.getcwd () }
  in
  let args =
    `Assoc [ ("command", `String "git clone https://example.com/repo") ]
  in
  let out = Lwt_main.run (tool.invoke args) in
  Alcotest.(check bool)
    "url blocked" true
    (contains out "disallowed in workspace_only mode")

let test_shell_rejects_binary_path_bypass () =
  let tool =
    Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
      ~allowed_commands:[ "ls" ] ~extra_allowed_paths:[]
      ~sandbox:{ Sandbox.backend = Sandbox.None; workspace = Sys.getcwd () }
  in
  let args = `Assoc [ ("command", `String "./ls") ] in
  let out = Lwt_main.run (tool.invoke args) in
  Alcotest.(check bool)
    "binary path blocked" true
    (contains out "binary path is disallowed")

let test_shell_rejects_option_assigned_absolute_path () =
  let tool =
    Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
      ~allowed_commands:[ "tar" ] ~extra_allowed_paths:[]
      ~sandbox:{ Sandbox.backend = Sandbox.None; workspace = Sys.getcwd () }
  in
  let args =
    `Assoc [ ("command", `String "tar --file=/tmp/out.tar -cf out.tar .") ]
  in
  let out = Lwt_main.run (tool.invoke args) in
  Alcotest.(check bool)
    "assigned path blocked" true
    (contains out "disallowed in workspace_only mode")

let test_shell_rejects_git_network_subcommand () =
  let tool =
    Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
      ~allowed_commands:[ "git" ] ~extra_allowed_paths:[]
      ~sandbox:{ Sandbox.backend = Sandbox.None; workspace = Sys.getcwd () }
  in
  let args = `Assoc [ ("command", `String "git clone repo") ] in
  let out = Lwt_main.run (tool.invoke args) in
  Alcotest.(check bool)
    "git clone blocked" true
    (contains out "disallowed in workspace_only mode")

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
      let tool =
        Tools_builtin.shell_exec ~workspace ~workspace_only:true
          ~allowed_commands:[ "ls" ] ~extra_allowed_paths:[ extra_dir ]
          ~sandbox:{ Sandbox.backend = Sandbox.None; workspace }
      in
      let out =
        Lwt_main.run
          (tool.invoke (`Assoc [ ("command", `String ("ls " ^ extra_dir)) ]))
      in
      Alcotest.(check bool)
        "extra allowed path usable in shell" true
        (contains out "exit_code: 0");
      let tool_no_extra =
        Tools_builtin.shell_exec ~workspace ~workspace_only:true
          ~allowed_commands:[ "ls" ] ~extra_allowed_paths:[]
          ~sandbox:{ Sandbox.backend = Sandbox.None; workspace = Sys.getcwd () }
      in
      let out2 =
        Lwt_main.run
          (tool_no_extra.invoke
             (`Assoc [ ("command", `String ("ls " ^ extra_dir)) ]))
      in
      Alcotest.(check bool)
        "without extra_allowed_paths blocked in shell" true
        (contains out2 "disallowed in workspace_only mode"))
    ~finally:(fun () ->
      (try Unix.rmdir extra_dir with _ -> ());
      try Unix.rmdir workspace with _ -> ())

let test_file_edit_replaces_first_match () =
  with_temp_workspace (fun workspace ->
      let path = "note.txt" in
      let oc = open_out path in
      output_string oc "abc abc";
      close_out oc;
      let tool =
        Tools_builtin.file_edit ~workspace ~workspace_only:true
          ~extra_allowed_paths:[]
      in
      let args =
        `Assoc
          [
            ("path", `String path);
            ("old_text", `String "abc");
            ("new_text", `String "xyz");
          ]
      in
      ignore (Lwt_main.run (tool.invoke args));
      let ic = open_in path in
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

let test_transcribe_rejects_outside_workspace () =
  with_temp_workspace (fun _workspace ->
      let cfg = Runtime_config.default in
      let tool = Tools_builtin.transcribe ~config:cfg in
      let args = `Assoc [ ("file_path", `String "/etc/passwd") ] in
      let out = Lwt_main.run (tool.invoke args) in
      Alcotest.(check bool)
        "outside workspace blocked" true
        (contains out "outside workspace"))

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
        (contains out2 "outside workspace"))
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
      let path = "big.txt" in
      let oc = open_out path in
      output_string oc (String.make 60000 'a');
      close_out oc;
      let tool =
        Tools_builtin.file_read ~workspace ~workspace_only:true
          ~extra_allowed_paths:[]
      in
      let out =
        Lwt_main.run (tool.invoke (`Assoc [ ("path", `String path) ]))
      in
      Alcotest.(check bool)
        "oversized read blocked with guidance" true
        (contains out "offset/limit"))

let test_file_read_paged_window_with_line_numbers () =
  with_temp_workspace (fun workspace ->
      let path = "paged.txt" in
      let oc = open_out path in
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
                  ("path", `String path); ("offset", `Int 2); ("limit", `Int 2);
                ]))
      in
      Alcotest.(check bool) "includes line 2" true (contains out "2: two");
      Alcotest.(check bool) "includes line 3" true (contains out "3: three");
      Alcotest.(check bool) "omits line 1" false (contains out "1: one"))

let test_file_read_paged_truncates_pathological_long_line () =
  with_temp_workspace (fun workspace ->
      let path = "huge_line.txt" in
      let oc = open_out path in
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
                  ("path", `String path); ("offset", `Int 1); ("limit", `Int 1);
                ]))
      in
      Alcotest.(check bool)
        "long line is truncated" true
        (contains out "(truncated");
      Alcotest.(check bool)
        "truncation note included" true
        (contains out "long lines are truncated");
      Alcotest.(check bool)
        "paged output stays bounded" true
        (String.length out < 10000))

let test_file_read_rejects_invalid_offset_limit () =
  with_temp_workspace (fun workspace ->
      let path = "window.txt" in
      let oc = open_out path in
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
                  ("path", `String path); ("offset", `Int 0); ("limit", `Int 1);
                ]))
      in
      Alcotest.(check bool)
        "offset validation error" true
        (contains offset_err "offset must be >= 1");
      let limit_low_err =
        Lwt_main.run
          (tool.invoke
             (`Assoc
                [
                  ("path", `String path); ("offset", `Int 1); ("limit", `Int 0);
                ]))
      in
      Alcotest.(check bool)
        "limit lower bound validation error" true
        (contains limit_low_err "limit must be >= 1");
      let limit_high_err =
        Lwt_main.run
          (tool.invoke
             (`Assoc
                [
                  ("path", `String path);
                  ("offset", `Int 1);
                  ("limit", `Int 2001);
                ]))
      in
      Alcotest.(check bool)
        "limit upper bound validation error" true
        (contains limit_high_err "limit must be <= 2000"))

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
        (contains out "outside workspace"))
    ~finally:(fun () ->
      (try Unix.unlink link_path with _ -> ());
      (try Unix.unlink outside_file with _ -> ());
      (try Unix.rmdir outside_dir with _ -> ());
      try Unix.rmdir workspace with _ -> ())

let test_file_append_creates_and_appends () =
  with_temp_workspace (fun workspace ->
      let path = "append.txt" in
      let tool =
        Tools_builtin.file_append ~workspace ~workspace_only:true
          ~extra_allowed_paths:[]
      in
      ignore
        (Lwt_main.run
           (tool.invoke
              (`Assoc [ ("path", `String path); ("content", `String "hello") ])));
      ignore
        (Lwt_main.run
           (tool.invoke
              (`Assoc [ ("path", `String path); ("content", `String " world") ])));
      let ic = open_in path in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      Alcotest.(check string) "append content" "hello world" content)

let test_file_edit_replace_all () =
  with_temp_workspace (fun workspace ->
      let path = "replace_all.txt" in
      let oc = open_out path in
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
                   ("path", `String path);
                   ("old_text", `String "a");
                   ("new_text", `String "z");
                   ("replace_all", `Bool true);
                 ])));
      let ic = open_in path in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      Alcotest.(check string) "all replaced" "z b z b" content)

let test_file_edit_lines_replaces_range () =
  with_temp_workspace (fun workspace ->
      let path = "lines.txt" in
      let oc = open_out path in
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
                   ("path", `String path);
                   ("start_line", `Int 2);
                   ("end_line", `Int 3);
                   ("content", `String "TWO\nTHREE");
                 ])));
      let ic = open_in path in
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
        ~sandbox:{ Sandbox.backend = Sandbox.None; workspace = "/tmp" }
        blocked_registry;
      let blocked_tool = find_tool_exn blocked_registry "file_read" in
      let blocked_out =
        Lwt_main.run
          (blocked_tool.invoke (`Assoc [ ("path", `String extra_file) ]))
      in
      Alcotest.(check bool)
        "workspace_only blocks outside file" true
        (contains blocked_out "outside workspace");

      let cfg_extra_allowed =
        mk_cfg ~dynamic_enabled:true ~workspace_only:true
          ~extra_allowed_paths:[ extra_dir ]
      in
      let extra_registry = Tool_registry.create () in
      Tools_builtin.register_all ~config:cfg_extra_allowed
        ~sandbox:{ Sandbox.backend = Sandbox.None; workspace = "/tmp" }
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
        ~sandbox:{ Sandbox.backend = Sandbox.None; workspace = "/tmp" }
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
        ~sandbox:{ Sandbox.backend = Sandbox.None; workspace = "/tmp" }
        blocked_registry;
      let blocked_tool = find_tool_exn blocked_registry "shell_exec" in
      let blocked_out =
        Lwt_main.run
          (blocked_tool.invoke
             (`Assoc [ ("command", `String ("ls " ^ extra_dir)) ]))
      in
      Alcotest.(check bool)
        "workspace_only blocks outside path arg" true
        (contains blocked_out "disallowed in workspace_only mode");

      let cfg_extra_allowed =
        mk_cfg ~dynamic_enabled:true ~workspace_only:true
          ~extra_allowed_paths:[ extra_dir ]
      in
      let extra_registry = Tool_registry.create () in
      Tools_builtin.register_all ~config:cfg_extra_allowed
        ~sandbox:{ Sandbox.backend = Sandbox.None; workspace = "/tmp" }
        extra_registry;
      let extra_tool = find_tool_exn extra_registry "shell_exec" in
      let extra_out =
        Lwt_main.run
          (extra_tool.invoke
             (`Assoc [ ("command", `String ("ls " ^ extra_dir)) ]))
      in
      Alcotest.(check bool)
        "extra_allowed_paths permits shell path arg" true
        (contains extra_out "exit_code: 0"))
    ~finally:(fun () ->
      (try Unix.rmdir extra_dir with _ -> ());
      try Unix.rmdir workspace with _ -> ())

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
    Alcotest.test_case "shell extra_allowed_paths grants access" `Quick
      test_shell_extra_allowed_paths_grants_access;
    Alcotest.test_case "file_edit first match" `Quick
      test_file_edit_replaces_first_match;
    Alcotest.test_case "file_read uses configured workspace" `Quick
      test_file_read_uses_configured_workspace_root;
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
  ]
