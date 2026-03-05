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
      ~allowed_commands:[ "ls" ]
  in
  let args = `Assoc [ ("command", `String "echo hi") ] in
  let out = Lwt_main.run (tool.invoke args) in
  Alcotest.(check bool) "blocked" true (contains out "not in the allowlist")

let test_shell_allowlist_allows_command () =
  let tool =
    Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
      ~allowed_commands:[ "ls" ]
  in
  let args = `Assoc [ ("command", `String "ls .") ] in
  let out = Lwt_main.run (tool.invoke args) in
  Alcotest.(check bool) "success" true (contains out "exit_code: 0")

let test_shell_rejects_command_chaining () =
  let tool =
    Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
      ~allowed_commands:[ "ls" ]
  in
  let args = `Assoc [ ("command", `String "ls && whoami") ] in
  let out = Lwt_main.run (tool.invoke args) in
  Alcotest.(check bool)
    "unsafe syntax blocked" true
    (contains out "unsafe shell syntax")

let test_shell_rejects_dollar_expansion () =
  let tool =
    Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
      ~allowed_commands:[ "ls" ]
  in
  let args = `Assoc [ ("command", `String "ls $HOME") ] in
  let out = Lwt_main.run (tool.invoke args) in
  Alcotest.(check bool)
    "dollar expansion blocked" true
    (contains out "unsafe shell syntax")

let test_shell_handles_quoted_args () =
  let tool =
    Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
      ~allowed_commands:[ "ls" ]
  in
  let args = `Assoc [ ("command", `String "ls \".\"") ] in
  let out = Lwt_main.run (tool.invoke args) in
  Alcotest.(check bool) "quoted arg success" true (contains out "exit_code: 0")

let test_shell_rejects_absolute_path_arg () =
  let tool =
    Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
      ~allowed_commands:[ "cat" ]
  in
  let args = `Assoc [ ("command", `String "cat /etc/passwd") ] in
  let out = Lwt_main.run (tool.invoke args) in
  Alcotest.(check bool)
    "absolute path blocked" true
    (contains out "disallowed in workspace_only mode")

let test_shell_rejects_url_arg () =
  let tool =
    Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
      ~allowed_commands:[ "git" ]
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
      ~allowed_commands:[ "ls" ]
  in
  let args = `Assoc [ ("command", `String "./ls") ] in
  let out = Lwt_main.run (tool.invoke args) in
  Alcotest.(check bool)
    "binary path blocked" true
    (contains out "binary path is disallowed")

let test_shell_rejects_option_assigned_absolute_path () =
  let tool =
    Tools_builtin.shell_exec ~workspace:(Sys.getcwd ()) ~workspace_only:true
      ~allowed_commands:[ "tar" ]
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
      ~allowed_commands:[ "git" ]
  in
  let args = `Assoc [ ("command", `String "git clone repo") ] in
  let out = Lwt_main.run (tool.invoke args) in
  Alcotest.(check bool)
    "git clone blocked" true
    (contains out "disallowed in workspace_only mode")

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
    Alcotest.test_case "shell absolute path arg blocked" `Quick
      test_shell_rejects_absolute_path_arg;
    Alcotest.test_case "shell url arg blocked" `Quick test_shell_rejects_url_arg;
    Alcotest.test_case "shell binary path bypass blocked" `Quick
      test_shell_rejects_binary_path_bypass;
    Alcotest.test_case "shell assigned absolute path blocked" `Quick
      test_shell_rejects_option_assigned_absolute_path;
    Alcotest.test_case "shell git network subcommand blocked" `Quick
      test_shell_rejects_git_network_subcommand;
    Alcotest.test_case "file_edit first match" `Quick
      test_file_edit_replaces_first_match;
    Alcotest.test_case "file_read uses configured workspace" `Quick
      test_file_read_uses_configured_workspace_root;
    Alcotest.test_case "extra_allowed_paths grants access" `Quick
      test_extra_allowed_paths_grants_access;
    Alcotest.test_case "transcribe path policy" `Quick
      test_transcribe_rejects_outside_workspace;
    Alcotest.test_case "localhost url accepts loopback" `Quick
      test_is_localhost_url_accepts_loopback_hosts;
    Alcotest.test_case "localhost url rejects spoofing" `Quick
      test_is_localhost_url_rejects_host_spoofing;
  ]
