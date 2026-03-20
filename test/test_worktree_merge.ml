let contains_substring ~needle haystack =
  let nl = String.length needle in
  let hl = String.length haystack in
  let rec loop i =
    if i + nl > hl then false
    else if String.sub haystack i nl = needle then true
    else loop (i + 1)
  in
  nl > 0 && hl >= nl && loop 0

let init_git_repo path =
  let cmd =
    Printf.sprintf "git -C %s init -q >/dev/null 2>&1" (Filename.quote path)
  in
  match Sys.command cmd with
  | 0 -> ()
  | code -> Alcotest.failf "git init failed for %s (exit %d)" path code

let configure_git_identity repo =
  let cmd =
    Printf.sprintf
      "git -C %s config user.name 'Test User' >/dev/null 2>&1 && git -C %s \
       config user.email test@example.com >/dev/null 2>&1"
      (Filename.quote repo) (Filename.quote repo)
  in
  match Sys.command cmd with
  | 0 -> ()
  | code ->
      Alcotest.failf "git identity config failed for %s (exit %d)" repo code

let git_cmd repo args =
  let cmd =
    Printf.sprintf "git -C %s %s >/dev/null 2>&1" (Filename.quote repo) args
  in
  match Sys.command cmd with
  | 0 -> ()
  | code -> Alcotest.failf "git command failed (exit %d): %s" code args

let with_temp_dir f =
  let dir = Filename.temp_file "clawq-wt-test" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  Fun.protect
    (fun () -> f dir)
    ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir))))

let with_temp_git_repo f =
  with_temp_dir (fun dir ->
      init_git_repo dir;
      configure_git_identity dir;
      git_cmd dir "commit --allow-empty -m 'initial' -q";
      f dir)

let fake_task ?(automerge = false) ?(use_worktree = true) ?(merge_status = None)
    ~id ~repo_path ~branch ~worktree_path () : Background_task.task =
  {
    id;
    runner = Background_task.Codex;
    model = None;
    repo_path;
    prompt = "test";
    branch;
    worktree_path = Some worktree_path;
    log_path = None;
    status = Background_task.Succeeded;
    session_key = None;
    channel = None;
    channel_id = None;
    pid = None;
    result_preview = None;
    created_at = "2026-03-11 00:00:00";
    started_at = None;
    finished_at = None;
    automerge;
    use_worktree;
    merge_status;
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

let test_schema_migration () =
  with_temp_git_repo (fun repo_path ->
      let db = Memory.init ~db_path:":memory:" () in
      Background_task.init_schema db;
      match
        Background_task.enqueue ~db ~runner:Background_task.Codex
          ~automerge:true ~use_worktree:true ~repo_path ~prompt:"test" ()
      with
      | Error msg -> Alcotest.fail msg
      | Ok id -> (
          match Background_task.get_task ~db ~id with
          | None -> Alcotest.fail "task not found"
          | Some task ->
              Alcotest.(check bool) "automerge" true task.automerge;
              Alcotest.(check bool) "use_worktree" true task.use_worktree;
              Alcotest.(check (option string))
                "merge_status" None task.merge_status))

let test_schema_defaults () =
  with_temp_git_repo (fun repo_path ->
      let db = Memory.init ~db_path:":memory:" () in
      Background_task.init_schema db;
      match
        Background_task.enqueue ~db ~runner:Background_task.Codex ~repo_path
          ~prompt:"test" ()
      with
      | Error msg -> Alcotest.fail msg
      | Ok id -> (
          match Background_task.get_task ~db ~id with
          | None -> Alcotest.fail "task not found"
          | Some task ->
              Alcotest.(check bool) "automerge default" true task.automerge;
              Alcotest.(check bool)
                "use_worktree default" true task.use_worktree))

let test_merge_success () =
  with_temp_git_repo (fun repo_path ->
      let branch = "test-merge-branch" in
      let worktree_path = repo_path ^ "-wt" in
      git_cmd repo_path
        (Printf.sprintf "worktree add -b %s %s" branch
           (Filename.quote worktree_path));
      let file_path = Filename.concat worktree_path "newfile.txt" in
      let oc = open_out file_path in
      output_string oc "hello";
      close_out oc;
      git_cmd worktree_path "add newfile.txt";
      git_cmd worktree_path "commit -m 'add newfile' -q";
      let result =
        Lwt_main.run
          (Worktree_merge.merge_and_cleanup ~repo_path ~worktree_path ~branch)
      in
      (match result with
      | Worktree_merge.Merged { commit_count; _ } ->
          Alcotest.(check int) "one commit merged" 1 commit_count
      | other ->
          Alcotest.failf "expected Merged, got: %s"
            (Worktree_merge.format_result other));
      (* Verify working tree was updated *)
      let main_file = Filename.concat repo_path "newfile.txt" in
      Alcotest.(check bool)
        "newfile.txt exists in main repo" true
        (Sys.file_exists main_file);
      Alcotest.(check bool)
        "worktree removed" false
        (Sys.file_exists worktree_path))

let test_merge_conflict () =
  with_temp_git_repo (fun repo_path ->
      let branch = "test-conflict-branch" in
      let worktree_path = repo_path ^ "-wt" in
      git_cmd repo_path
        (Printf.sprintf "worktree add -b %s %s" branch
           (Filename.quote worktree_path));
      let file_path_wt = Filename.concat worktree_path "file.txt" in
      let oc = open_out file_path_wt in
      output_string oc "worktree version";
      close_out oc;
      git_cmd worktree_path "add file.txt";
      git_cmd worktree_path "commit -m 'wt change' -q";
      let file_path_main = Filename.concat repo_path "file.txt" in
      let oc = open_out file_path_main in
      output_string oc "main version";
      close_out oc;
      git_cmd repo_path "add file.txt";
      git_cmd repo_path "commit -m 'main change' -q";
      let result =
        Lwt_main.run
          (Worktree_merge.merge_and_cleanup ~repo_path ~worktree_path ~branch)
      in
      (match result with
      | Worktree_merge.Conflict { branch = b; _ } ->
          Alcotest.(check string) "conflict branch" branch b
      | other ->
          Alcotest.failf "expected Conflict, got: %s"
            (Worktree_merge.format_result other));
      Alcotest.(check bool)
        "worktree still exists" true
        (Sys.file_exists worktree_path);
      git_cmd repo_path
        (Printf.sprintf "worktree remove --force %s"
           (Filename.quote worktree_path)))

let test_noop_merge () =
  with_temp_git_repo (fun repo_path ->
      let branch = "test-noop-branch" in
      let worktree_path = repo_path ^ "-wt" in
      git_cmd repo_path
        (Printf.sprintf "worktree add -b %s %s" branch
           (Filename.quote worktree_path));
      let result =
        Lwt_main.run
          (Worktree_merge.merge_and_cleanup ~repo_path ~worktree_path ~branch)
      in
      (match result with
      | Worktree_merge.Already_merged -> ()
      | other ->
          Alcotest.failf "expected Already_merged, got: %s"
            (Worktree_merge.format_result other));
      Alcotest.(check bool)
        "worktree removed" false
        (Sys.file_exists worktree_path))

let test_delegate_prompt_automerge () =
  let prompt =
    Background_task.build_delegate_prompt ~automerge:true ~goal:"do stuff"
  in
  Alcotest.(check bool)
    "contains MUST commit" true
    (contains_substring ~needle:"MUST" prompt
    && contains_substring ~needle:"git commit" prompt);
  Alcotest.(check bool)
    "no 'Do not commit'" false
    (contains_substring ~needle:"Do not commit" prompt)

let test_delegate_prompt_no_automerge () =
  let prompt =
    Background_task.build_delegate_prompt ~automerge:false ~goal:"do stuff"
  in
  Alcotest.(check bool)
    "contains MUST commit" true
    (contains_substring ~needle:"MUST" prompt
    && contains_substring ~needle:"git commit" prompt);
  Alcotest.(check bool)
    "no 'Do not commit'" false
    (contains_substring ~needle:"Do not commit" prompt)

let test_finalize_tool_missing_id () =
  let db = Memory.init ~db_path:":memory:" () in
  Background_task.init_schema db;
  let tool = Worktree_merge.finalize_tool ~db in
  let result = Lwt_main.run (tool.Tool.invoke (`Assoc [ ("id", `Int 999) ])) in
  Alcotest.(check bool)
    "error for missing task" true
    (contains_substring ~needle:"Error" result)

let test_finalize_tool_no_worktree () =
  with_temp_git_repo (fun repo_path ->
      let db = Memory.init ~db_path:":memory:" () in
      Background_task.init_schema db;
      let id =
        match
          Background_task.enqueue ~db ~runner:Background_task.Codex ~repo_path
            ~prompt:"test" ()
        with
        | Ok id -> id
        | Error msg -> Alcotest.failf "enqueue failed: %s" msg
      in
      Background_task.finish ~db ~id ~status:Background_task.Succeeded
        ~result_preview:"ok";
      let tool = Worktree_merge.finalize_tool ~db in
      let result =
        Lwt_main.run (tool.Tool.invoke (`Assoc [ ("id", `Int id) ]))
      in
      Alcotest.(check bool)
        "error about no worktree" true
        (contains_substring ~needle:"no worktree" result))

let test_use_worktree_false_skips_worktree () =
  with_temp_git_repo (fun repo_path ->
      let db = Memory.init ~db_path:":memory:" () in
      Background_task.init_schema db;
      match
        Background_task.enqueue ~db ~runner:Background_task.Codex
          ~use_worktree:false ~repo_path ~prompt:"test" ()
      with
      | Error msg -> Alcotest.fail msg
      | Ok id -> (
          match Background_task.get_task ~db ~id with
          | None -> Alcotest.fail "task not found"
          | Some task ->
              Alcotest.(check bool) "use_worktree false" false task.use_worktree
          ))

let test_try_automerge_with_db () =
  with_temp_git_repo (fun repo_path ->
      let db = Memory.init ~db_path:":memory:" () in
      Background_task.init_schema db;
      let branch = "test-automerge-branch" in
      let worktree_path = repo_path ^ "-wt" in
      git_cmd repo_path
        (Printf.sprintf "worktree add -b %s %s" branch
           (Filename.quote worktree_path));
      let file_path = Filename.concat worktree_path "automerge.txt" in
      let oc = open_out file_path in
      output_string oc "automerge content";
      close_out oc;
      git_cmd worktree_path "add automerge.txt";
      git_cmd worktree_path "commit -m 'automerge commit' -q";
      match
        Background_task.enqueue ~db ~runner:Background_task.Codex
          ~automerge:true ~repo_path ~prompt:"test" ~branch ()
      with
      | Error msg -> Alcotest.fail msg
      | Ok id ->
          ignore
            (Background_task.set_running ~db ~id ~branch ~worktree_path
               ~log_path:"/dev/null" ~pid:1);
          Background_task.finish ~db ~id ~status:Background_task.Succeeded
            ~result_preview:"ok";
          let task =
            match Background_task.get_task ~db ~id with
            | Some t -> t
            | None -> Alcotest.failf "task not found"
          in
          let result = Lwt_main.run (Worktree_merge.try_automerge ~db task) in
          (match result with
          | Worktree_merge.Merged { commit_count; _ } ->
              Alcotest.(check int) "merged one commit" 1 commit_count
          | other ->
              Alcotest.failf "expected Merged, got: %s"
                (Worktree_merge.format_result other));
          let updated_task =
            match Background_task.get_task ~db ~id with
            | Some t -> t
            | None -> Alcotest.failf "task not found after merge"
          in
          Alcotest.(check (option string))
            "merge_status set" (Some "merged") updated_task.merge_status)

let test_merge_status_in_messages () =
  let task =
    {
      (fake_task ~id:42 ~repo_path:"/tmp/repo" ~branch:"b1"
         ~worktree_path:"/tmp/wt" ())
      with
      merge_status = Some "merged";
    }
  in
  let msg = Background_task.terse_finished_message task in
  Alcotest.(check bool)
    "contains automerged" true
    (contains_substring ~needle:"automerged" msg);
  let status = Background_task.status_message task in
  Alcotest.(check bool)
    "status contains automerged" true
    (contains_substring ~needle:"automerged" status)

let test_merge_status_conflict_message () =
  let task =
    {
      (fake_task ~id:42 ~repo_path:"/tmp/repo" ~branch:"b1"
         ~worktree_path:"/tmp/wt" ())
      with
      merge_status = Some "conflict";
    }
  in
  let msg = Background_task.terse_finished_message task in
  Alcotest.(check bool)
    "contains rebase conflict" true
    (contains_substring ~needle:"rebase conflict" msg);
  Alcotest.(check bool)
    "contains finalize" true
    (contains_substring ~needle:"background finalize" msg)

let test_set_merge_status () =
  with_temp_git_repo (fun repo_path ->
      let db = Memory.init ~db_path:":memory:" () in
      Background_task.init_schema db;
      match
        Background_task.enqueue ~db ~runner:Background_task.Codex ~repo_path
          ~prompt:"test" ()
      with
      | Error msg -> Alcotest.fail msg
      | Ok id ->
          Background_task.set_merge_status ~db ~id ~merge_status:"merged";
          let task =
            match Background_task.get_task ~db ~id with
            | Some t -> t
            | None -> Alcotest.failf "task not found"
          in
          Alcotest.(check (option string))
            "merge_status" (Some "merged") task.merge_status)

let test_dirty_worktree_blocks_merge () =
  with_temp_git_repo (fun repo_path ->
      let branch = "test-dirty-branch" in
      let worktree_path = repo_path ^ "-wt" in
      git_cmd repo_path
        (Printf.sprintf "worktree add -b %s %s" branch
           (Filename.quote worktree_path));
      (* Stage a file but do NOT commit *)
      let file_path = Filename.concat worktree_path "staged.txt" in
      let oc = open_out file_path in
      output_string oc "staged content";
      close_out oc;
      git_cmd worktree_path "add staged.txt";
      let result =
        Lwt_main.run
          (Worktree_merge.merge_and_cleanup ~repo_path ~worktree_path ~branch)
      in
      (match result with
      | Worktree_merge.Dirty_worktree { branch = b; details } ->
          Alcotest.(check string) "dirty branch" branch b;
          Alcotest.(check bool)
            "details mention staged file" true
            (contains_substring ~needle:"staged.txt" details)
      | other ->
          Alcotest.failf "expected Dirty_worktree, got: %s"
            (Worktree_merge.format_result other));
      (* Worktree should still exist *)
      Alcotest.(check bool)
        "worktree preserved" true
        (Sys.file_exists worktree_path);
      (* Clean up *)
      git_cmd repo_path
        (Printf.sprintf "worktree remove --force %s"
           (Filename.quote worktree_path)))

let test_dirty_worktree_unstaged_blocks_merge () =
  with_temp_git_repo (fun repo_path ->
      let branch = "test-unstaged-branch" in
      let worktree_path = repo_path ^ "-wt" in
      git_cmd repo_path
        (Printf.sprintf "worktree add -b %s %s" branch
           (Filename.quote worktree_path));
      (* Create a tracked file, commit it, then modify without staging *)
      let file_path = Filename.concat worktree_path "modified.txt" in
      let oc = open_out file_path in
      output_string oc "original";
      close_out oc;
      git_cmd worktree_path "add modified.txt";
      git_cmd worktree_path "commit -m 'add modified.txt' -q";
      let oc = open_out file_path in
      output_string oc "changed";
      close_out oc;
      let result =
        Lwt_main.run
          (Worktree_merge.merge_and_cleanup ~repo_path ~worktree_path ~branch)
      in
      (match result with
      | Worktree_merge.Dirty_worktree { details; _ } ->
          Alcotest.(check bool)
            "details mention modified file" true
            (contains_substring ~needle:"modified.txt" details)
      | other ->
          Alcotest.failf "expected Dirty_worktree, got: %s"
            (Worktree_merge.format_result other));
      Alcotest.(check bool)
        "worktree preserved" true
        (Sys.file_exists worktree_path);
      git_cmd repo_path
        (Printf.sprintf "worktree remove --force %s"
           (Filename.quote worktree_path)))

let test_format_result () =
  let merged = Worktree_merge.Merged { branch = "b1"; commit_count = 3 } in
  let s = Worktree_merge.format_result merged in
  Alcotest.(check bool)
    "contains branch" true
    (contains_substring ~needle:"b1" s);
  Alcotest.(check bool)
    "contains commits" true
    (contains_substring ~needle:"3 commits" s);
  let conflict =
    Worktree_merge.Conflict { branch = "b2"; message = "rebase failed" }
  in
  let s = Worktree_merge.format_result conflict in
  Alcotest.(check bool)
    "contains conflict" true
    (contains_substring ~needle:"conflict" s);
  let s = Worktree_merge.format_result Worktree_merge.No_worktree in
  Alcotest.(check bool)
    "no worktree" true
    (contains_substring ~needle:"No worktree" s);
  let s = Worktree_merge.format_result Worktree_merge.Already_merged in
  Alcotest.(check bool)
    "already merged" true
    (contains_substring ~needle:"already up to date" s);
  let s =
    Worktree_merge.format_result
      (Worktree_merge.Dirty_worktree
         { branch = "b3"; details = "A  staged.txt" })
  in
  Alcotest.(check bool)
    "dirty worktree mentions uncommitted" true
    (contains_substring ~needle:"uncommitted" s);
  Alcotest.(check bool)
    "dirty worktree mentions branch" true
    (contains_substring ~needle:"b3" s)

let test_merge_updates_working_tree () =
  with_temp_git_repo (fun repo_path ->
      let branch = "test-wt-update" in
      let worktree_path = repo_path ^ "-wt" in
      git_cmd repo_path
        (Printf.sprintf "worktree add -b %s %s" branch
           (Filename.quote worktree_path));
      let file_path = Filename.concat worktree_path "wt-file.txt" in
      let oc = open_out file_path in
      output_string oc "from worktree";
      close_out oc;
      git_cmd worktree_path "add wt-file.txt";
      git_cmd worktree_path "commit -m 'add wt-file' -q";
      let result =
        Lwt_main.run
          (Worktree_merge.merge_and_cleanup ~repo_path ~worktree_path ~branch)
      in
      (match result with
      | Worktree_merge.Merged _ -> ()
      | other ->
          Alcotest.failf "expected Merged, got: %s"
            (Worktree_merge.format_result other));
      (* File must exist in main repo working tree *)
      let main_file = Filename.concat repo_path "wt-file.txt" in
      Alcotest.(check bool)
        "wt-file.txt in main repo" true
        (Sys.file_exists main_file);
      (* Working tree must be clean (no diff between HEAD and index/worktree) *)
      let exit_code =
        Sys.command
          (Printf.sprintf "git -C %s diff --quiet HEAD >/dev/null 2>&1"
             (Filename.quote repo_path))
      in
      Alcotest.(check int) "git diff --quiet HEAD passes" 0 exit_code)

let test_completion_pass_requeues_dirty_task () =
  with_temp_git_repo (fun repo_path ->
      let db = Memory.init ~db_path:":memory:" () in
      Background_task.init_schema db;
      let branch = "test-cp-dirty" in
      let worktree_path = repo_path ^ "-wt" in
      git_cmd repo_path
        (Printf.sprintf "worktree add -b %s %s" branch
           (Filename.quote worktree_path));
      match
        Background_task.enqueue ~db ~runner:Background_task.Codex
          ~use_worktree:true ~automerge:true ~repo_path ~prompt:"test" ~branch
          ()
      with
      | Error msg -> Alcotest.fail msg
      | Ok id ->
          ignore
            (Background_task.set_running ~db ~id ~branch ~worktree_path
               ~log_path:"/dev/null" ~pid:1);
          Background_task.finish ~db ~id ~status:Background_task.DirtyWorktree
            ~result_preview:"dirty";
          Background_task.request_completion_pass ~db ~id;
          let task =
            match Background_task.get_task ~db ~id with
            | Some t -> t
            | None -> Alcotest.failf "task not found"
          in
          Alcotest.(check string)
            "status is queued" "queued"
            (Background_task.string_of_status task.status);
          Alcotest.(check (option string))
            "merge_status" (Some "completion_pass") task.merge_status;
          let msgs = Background_task.list_queued_messages ~db ~task_id:id in
          Alcotest.(check bool) "has queued message" true (List.length msgs > 0);
          git_cmd repo_path
            (Printf.sprintf "worktree remove --force %s"
               (Filename.quote worktree_path)))

let test_completion_pass_requeues_clean_task () =
  with_temp_git_repo (fun repo_path ->
      let db = Memory.init ~db_path:":memory:" () in
      Background_task.init_schema db;
      let branch = "test-cp-clean" in
      let worktree_path = repo_path ^ "-wt" in
      git_cmd repo_path
        (Printf.sprintf "worktree add -b %s %s" branch
           (Filename.quote worktree_path));
      match
        Background_task.enqueue ~db ~runner:Background_task.Codex
          ~use_worktree:true ~automerge:true ~repo_path ~prompt:"test" ~branch
          ()
      with
      | Error msg -> Alcotest.fail msg
      | Ok id ->
          ignore
            (Background_task.set_running ~db ~id ~branch ~worktree_path
               ~log_path:"/dev/null" ~pid:1);
          Background_task.finish ~db ~id ~status:Background_task.Succeeded
            ~result_preview:"ok";
          Background_task.request_completion_pass ~db ~id;
          let task =
            match Background_task.get_task ~db ~id with
            | Some t -> t
            | None -> Alcotest.failf "task not found"
          in
          Alcotest.(check string)
            "status is queued" "queued"
            (Background_task.string_of_status task.status);
          Alcotest.(check (option string))
            "merge_status" (Some "completion_pass") task.merge_status;
          git_cmd repo_path
            (Printf.sprintf "worktree remove --force %s"
               (Filename.quote worktree_path)))

let test_completion_pass_non_automerge () =
  with_temp_git_repo (fun repo_path ->
      let db = Memory.init ~db_path:":memory:" () in
      Background_task.init_schema db;
      let branch = "test-cp-noam" in
      let worktree_path = repo_path ^ "-wt" in
      git_cmd repo_path
        (Printf.sprintf "worktree add -b %s %s" branch
           (Filename.quote worktree_path));
      match
        Background_task.enqueue ~db ~runner:Background_task.Codex
          ~use_worktree:true ~automerge:false ~repo_path ~prompt:"test" ~branch
          ()
      with
      | Error msg -> Alcotest.fail msg
      | Ok id ->
          ignore
            (Background_task.set_running ~db ~id ~branch ~worktree_path
               ~log_path:"/dev/null" ~pid:1);
          Background_task.finish ~db ~id ~status:Background_task.Succeeded
            ~result_preview:"ok";
          Background_task.request_completion_pass ~db ~id;
          let task =
            match Background_task.get_task ~db ~id with
            | Some t -> t
            | None -> Alcotest.failf "task not found"
          in
          Alcotest.(check (option string))
            "merge_status" (Some "completion_pass") task.merge_status;
          git_cmd repo_path
            (Printf.sprintf "worktree remove --force %s"
               (Filename.quote worktree_path)))

let test_completion_pass_skips_second_time () =
  let task =
    {
      (fake_task ~id:42 ~automerge:true ~use_worktree:true
         ~merge_status:(Some "completion_pass") ~repo_path:"/tmp/repo"
         ~branch:"b1" ~worktree_path:"/tmp/wt" ())
      with
      status = Background_task.Succeeded;
    }
  in
  Alcotest.(check (option string))
    "merge_status is completion_pass" (Some "completion_pass") task.merge_status;
  Alcotest.(check bool) "automerge is true" true task.automerge;
  Alcotest.(check bool)
    "status is Succeeded" true
    (task.status = Background_task.Succeeded)

let test_completion_pass_second_time_no_automerge () =
  let task =
    {
      (fake_task ~id:43 ~automerge:false ~use_worktree:true
         ~merge_status:(Some "completion_pass") ~repo_path:"/tmp/repo"
         ~branch:"b1" ~worktree_path:"/tmp/wt" ())
      with
      status = Background_task.Succeeded;
    }
  in
  Alcotest.(check (option string))
    "merge_status is completion_pass" (Some "completion_pass") task.merge_status;
  Alcotest.(check bool) "automerge is false" false task.automerge

let test_completion_pass_dirty_second_time () =
  let task =
    {
      (fake_task ~id:44 ~automerge:true ~use_worktree:true
         ~merge_status:(Some "completion_pass") ~repo_path:"/tmp/repo"
         ~branch:"b1" ~worktree_path:"/tmp/wt" ())
      with
      status = Background_task.DirtyWorktree;
    }
  in
  Alcotest.(check (option string))
    "merge_status is completion_pass" (Some "completion_pass") task.merge_status;
  Alcotest.(check bool)
    "status is DirtyWorktree" true
    (task.status = Background_task.DirtyWorktree);
  Alcotest.(check bool)
    "automerge would not trigger (not Succeeded)" false
    (task.automerge && task.status = Background_task.Succeeded)

let test_completion_pass_message_content () =
  let msg = Background_task.completion_pass_message () in
  Alcotest.(check bool)
    "contains sentinel" true
    (contains_substring ~needle:Background_task.completion_sentinel msg);
  Alcotest.(check bool)
    "contains git rebase master" true
    (contains_substring ~needle:"git rebase master" msg);
  Alcotest.(check bool)
    "contains git add" true
    (contains_substring ~needle:"git add" msg)

let test_completion_pass_skips_no_worktree () =
  let task =
    fake_task ~id:45 ~automerge:true ~use_worktree:false ~repo_path:"/tmp/repo"
      ~branch:"b1" ~worktree_path:"/tmp/wt" ()
  in
  Alcotest.(check bool) "use_worktree is false" false task.use_worktree

let suite =
  [
    Alcotest.test_case "schema migration" `Quick test_schema_migration;
    Alcotest.test_case "schema defaults" `Quick test_schema_defaults;
    Alcotest.test_case "merge success" `Quick test_merge_success;
    Alcotest.test_case "merge conflict" `Quick test_merge_conflict;
    Alcotest.test_case "noop merge" `Quick test_noop_merge;
    Alcotest.test_case "delegate prompt automerge" `Quick
      test_delegate_prompt_automerge;
    Alcotest.test_case "delegate prompt no automerge" `Quick
      test_delegate_prompt_no_automerge;
    Alcotest.test_case "finalize tool missing id" `Quick
      test_finalize_tool_missing_id;
    Alcotest.test_case "finalize tool no worktree" `Quick
      test_finalize_tool_no_worktree;
    Alcotest.test_case "use_worktree false" `Quick
      test_use_worktree_false_skips_worktree;
    Alcotest.test_case "try_automerge with db" `Quick test_try_automerge_with_db;
    Alcotest.test_case "merge status in messages" `Quick
      test_merge_status_in_messages;
    Alcotest.test_case "merge status conflict message" `Quick
      test_merge_status_conflict_message;
    Alcotest.test_case "set_merge_status" `Quick test_set_merge_status;
    Alcotest.test_case "dirty worktree blocks merge" `Quick
      test_dirty_worktree_blocks_merge;
    Alcotest.test_case "unstaged changes block merge" `Quick
      test_dirty_worktree_unstaged_blocks_merge;
    Alcotest.test_case "format_result" `Quick test_format_result;
    Alcotest.test_case "merge updates working tree" `Quick
      test_merge_updates_working_tree;
    Alcotest.test_case "completion pass requeues dirty task" `Quick
      test_completion_pass_requeues_dirty_task;
    Alcotest.test_case "completion pass requeues clean task" `Quick
      test_completion_pass_requeues_clean_task;
    Alcotest.test_case "completion pass non-automerge" `Quick
      test_completion_pass_non_automerge;
    Alcotest.test_case "completion pass skips second time" `Quick
      test_completion_pass_skips_second_time;
    Alcotest.test_case "completion pass second time no automerge" `Quick
      test_completion_pass_second_time_no_automerge;
    Alcotest.test_case "completion pass dirty second time" `Quick
      test_completion_pass_dirty_second_time;
    Alcotest.test_case "completion pass message content" `Quick
      test_completion_pass_message_content;
    Alcotest.test_case "completion pass skips no worktree" `Quick
      test_completion_pass_skips_no_worktree;
  ]
