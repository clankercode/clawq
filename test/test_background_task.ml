let init_git_repo path =
  let cmd =
    Printf.sprintf "git -C %s init -q >/dev/null 2>&1" (Filename.quote path)
  in
  match Sys.command cmd with
  | 0 -> ()
  | code -> Alcotest.failf "git init failed for %s (exit %d)" path code

let with_temp_git_repo f =
  let repo = Filename.temp_file "clawq-bg-repo" "" in
  Sys.remove repo;
  Unix.mkdir repo 0o755;
  init_git_repo repo;
  Fun.protect
    (fun () -> f repo)
    ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote repo))))

let test_enqueue_and_list_tasks () =
  with_temp_git_repo (fun repo_path ->
      let db = Memory.init ~db_path:":memory:" () in
      Background_task.init_schema db;
      let id =
        match
          Background_task.enqueue ~db ~runner:Background_task.Codex ~repo_path
            ~prompt:"implement feature" ()
        with
        | Ok id -> id
        | Error msg -> Alcotest.fail msg
      in
      let tasks = Background_task.list_tasks ~db in
      Alcotest.(check int) "one task" 1 (List.length tasks);
      let task = List.hd tasks in
      Alcotest.(check int) "task id" id task.Background_task.id;
      Alcotest.(check string)
        "runner" "codex"
        (Background_task.string_of_runner task.runner);
      Alcotest.(check string)
        "status" "queued"
        (Background_task.string_of_status task.status))

let test_cancel_queued_task () =
  with_temp_git_repo (fun repo_path ->
      let db = Memory.init ~db_path:":memory:" () in
      Background_task.init_schema db;
      let id =
        match
          Background_task.enqueue ~db ~runner:Background_task.Claude ~repo_path
            ~prompt:"fix bug" ()
        with
        | Ok id -> id
        | Error msg -> Alcotest.fail msg
      in
      (match Background_task.cancel ~db ~id with
      | Ok _ -> ()
      | Error msg -> Alcotest.fail msg);
      match Background_task.get_task ~db ~id with
      | None -> Alcotest.fail "expected task"
      | Some task ->
          Alcotest.(check string)
            "cancelled" "cancelled"
            (Background_task.string_of_status task.status))

let test_command_of_task_codex () =
  let task =
    {
      Background_task.id = 1;
      runner = Background_task.Codex;
      repo_path = "/tmp/repo";
      prompt = "ship it";
      branch = "clawq-bg-1";
      worktree_path = Some "/tmp/worktree";
      log_path = Some "/tmp/task.log";
      status = Background_task.Queued;
      session_key = None;
      channel = None;
      channel_id = None;
      pid = None;
      result_preview = None;
      created_at = "";
      started_at = None;
      finished_at = None;
    }
  in
  Alcotest.(check (array string))
    "codex argv"
    [|
      "codex"; "exec"; "--dangerously-bypass-approvals-and-sandbox"; "ship it";
    |]
    (Background_task.command_of_task task)

let test_command_of_task_claude () =
  let task =
    {
      Background_task.id = 2;
      runner = Background_task.Claude;
      repo_path = "/tmp/repo";
      prompt = "ship it";
      branch = "clawq-bg-2";
      worktree_path = Some "/tmp/worktree";
      log_path = Some "/tmp/task.log";
      status = Background_task.Queued;
      session_key = None;
      channel = None;
      channel_id = None;
      pid = None;
      result_preview = None;
      created_at = "";
      started_at = None;
      finished_at = None;
    }
  in
  Alcotest.(check (array string))
    "claude argv"
    [| "claude"; "-p"; "--dangerously-skip-permissions"; "ship it" |]
    (Background_task.command_of_task task)

let test_enqueue_tool_uses_context_session_key () =
  with_temp_git_repo (fun repo_path ->
      let db = Memory.init ~db_path:":memory:" () in
      Background_task.init_schema db;
      let tool = Background_task.enqueue_tool ~db in
      let args =
        `Assoc
          [
            ("runner", `String "codex");
            ("repo_path", `String repo_path);
            ("prompt", `String "implement feature");
          ]
      in
      let result =
        Lwt_main.run
          (tool.Tool.invoke
             ~context:
               {
                 Tool.session_key = Some "telegram:42:user";
                 send_progress = None;
               }
             args)
      in
      Alcotest.(check bool) "tool reports queued" true (String.length result > 0);
      match Background_task.get_task ~db ~id:1 with
      | None -> Alcotest.fail "expected queued task"
      | Some task ->
          Alcotest.(check (option string))
            "session key captured" (Some "telegram:42:user") task.session_key;
          Alcotest.(check (option string))
            "channel captured" (Some "telegram") task.channel;
          Alcotest.(check (option string))
            "channel id captured" (Some "42") task.channel_id)

let test_list_tool_returns_task_summary () =
  with_temp_git_repo (fun repo_path ->
      let db = Memory.init ~db_path:":memory:" () in
      Background_task.init_schema db;
      ignore
        (Background_task.enqueue ~db ~runner:Background_task.Codex ~repo_path
           ~prompt:"implement feature" ());
      let tool = Background_task.list_tool ~db in
      let result = Lwt_main.run (tool.Tool.invoke (`Assoc [])) in
      Alcotest.(check bool)
        "list mentions task" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "Background tasks:")
                result 0);
           true
         with Not_found -> false))

let test_wait_tool_returns_terminal_summary () =
  with_temp_git_repo (fun repo_path ->
      let db = Memory.init ~db_path:":memory:" () in
      Background_task.init_schema db;
      let id =
        match
          Background_task.enqueue ~db ~runner:Background_task.Codex ~repo_path
            ~prompt:"implement feature" ()
        with
        | Ok id -> id
        | Error msg -> Alcotest.fail msg
      in
      ignore
        (Background_task.mark_cancelled ~db ~id
           ~result_preview:"Cancelled before execution started");
      let tool = Background_task.wait_tool ~db in
      let result =
        Lwt_main.run
          (tool.Tool.invoke
             (`Assoc [ ("id", `Int id); ("timeout_seconds", `Float 0.1) ]))
      in
      Alcotest.(check bool)
        "wait mentions cancelled" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "status: cancelled")
                result 0);
           true
         with Not_found -> false))

let test_logs_tool_returns_excerpt () =
  with_temp_git_repo (fun repo_path ->
      let db = Memory.init ~db_path:":memory:" () in
      Background_task.init_schema db;
      let id =
        match
          Background_task.enqueue ~db ~runner:Background_task.Codex ~repo_path
            ~prompt:"implement feature" ()
        with
        | Ok id -> id
        | Error msg -> Alcotest.fail msg
      in
      let log_path = Filename.temp_file "clawq-bg" ".log" in
      let oc = open_out log_path in
      output_string oc "line one\nline two\nline three\n";
      close_out oc;
      ignore
        (Background_task.set_running ~db ~id ~branch:"clawq-bg-1"
           ~worktree_path:"/tmp/worktree" ~log_path ~pid:12345);
      Background_task.finish ~db ~id ~status:Background_task.Succeeded
        ~result_preview:"ok";
      let tool = Background_task.logs_tool ~db in
      let result =
        Lwt_main.run
          (tool.Tool.invoke (`Assoc [ ("id", `Int id); ("lines", `Int 2) ]))
      in
      Alcotest.(check bool)
        "logs include final lines" true
        (try
           ignore (Str.search_forward (Str.regexp_string "line two") result 0);
           ignore (Str.search_forward (Str.regexp_string "line three") result 0);
           true
         with Not_found -> false);
      Sys.remove log_path)

let test_delegate_tool_queues_task () =
  with_temp_git_repo (fun repo ->
      let db = Memory.init ~db_path:":memory:" () in
      Background_task.init_schema db;
      let tool = Background_task.delegate_tool ~db ~default_repo_path:repo () in
      let result =
        Lwt_main.run
          (tool.Tool.invoke
             (`Assoc
                [
                  ("goal", `String "implement the feature from TASK.md");
                  ("runner", `String "codex");
                ]))
      in
      Alcotest.(check bool)
        "delegate reports queued" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "Delegated task 1")
                result 0);
           true
         with Not_found -> false);
      match Background_task.get_task ~db ~id:1 with
      | None -> Alcotest.fail "expected delegated task"
      | Some task ->
          Alcotest.(check string)
            "delegated runner" "codex"
            (Background_task.string_of_runner task.runner);
          Alcotest.(check bool)
            "delegated prompt shaped" true
            (String.length task.prompt
            > String.length "implement the feature from TASK.md"))

let test_enqueue_rejects_non_git_repo () =
  let db = Memory.init ~db_path:":memory:" () in
  Background_task.init_schema db;
  let repo = Filename.temp_file "clawq-bg-repo" "" in
  Sys.remove repo;
  Unix.mkdir repo 0o755;
  let result =
    Background_task.enqueue ~db ~runner:Background_task.Codex ~repo_path:repo
      ~prompt:"implement feature" ()
  in
  ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote repo)));
  match result with
  | Ok _ -> Alcotest.fail "expected non-git repo rejection"
  | Error msg ->
      Alcotest.(check bool)
        "non-git repo mentioned" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "not a git repository")
                msg 0);
           true
         with Not_found -> false)

let suite =
  [
    Alcotest.test_case "enqueue and list tasks" `Quick
      test_enqueue_and_list_tasks;
    Alcotest.test_case "cancel queued task" `Quick test_cancel_queued_task;
    Alcotest.test_case "command_of_task codex" `Quick test_command_of_task_codex;
    Alcotest.test_case "command_of_task claude" `Quick
      test_command_of_task_claude;
    Alcotest.test_case "enqueue tool uses context session key" `Quick
      test_enqueue_tool_uses_context_session_key;
    Alcotest.test_case "list tool returns task summary" `Quick
      test_list_tool_returns_task_summary;
    Alcotest.test_case "wait tool returns terminal summary" `Quick
      test_wait_tool_returns_terminal_summary;
    Alcotest.test_case "logs tool returns excerpt" `Quick
      test_logs_tool_returns_excerpt;
    Alcotest.test_case "delegate tool queues task" `Quick
      test_delegate_tool_queues_task;
    Alcotest.test_case "enqueue rejects non-git repo" `Quick
      test_enqueue_rejects_non_git_repo;
  ]
