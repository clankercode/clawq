let with_temp_workspace f =
  let base = Filename.get_temp_dir_name () in
  let dir =
    Filename.concat base
      (Printf.sprintf "clawq_prompt_%d_%d" (Unix.getpid ()) (Random.bits ()))
  in
  (try Unix.rmdir dir with _ -> ());
  Unix.mkdir dir 0o755;
  Fun.protect
    (fun () -> f dir)
    ~finally:(fun () -> try Unix.rmdir dir with _ -> ())

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let test_dynamic_prompt_disabled_uses_base_prompt () =
  with_temp_workspace (fun workspace ->
      let prompt_cfg =
        { Runtime_config.default.prompt with dynamic_enabled = false }
      in
      let cfg =
        { Runtime_config.default with workspace; prompt = prompt_cfg }
      in
      let prompt = Prompt_builder.build ~config:cfg ~tool_registry:None () in
      Alcotest.(check string)
        "dynamic disabled returns base prompt" Prompt_builder.base_prompt prompt)

let test_default_prompt_enables_dynamic_workspace_context () =
  Alcotest.(check bool)
    "default dynamic prompt enabled" true
    Runtime_config.default.prompt.dynamic_enabled

let test_dynamic_prompt_includes_workspace_files () =
  with_temp_workspace (fun workspace ->
      write_file (Filename.concat workspace "EGO.md") "EGO SENTINEL";
      write_file (Filename.concat workspace "AGENTS.md") "AGENTS SENTINEL";
      let prompt_cfg =
        {
          Runtime_config.default.prompt with
          dynamic_enabled = true;
          include_tools_section = false;
          include_safety_section = false;
          include_runtime_section = false;
          include_datetime_section = false;
          include_autonomy_section = false;
          workspace_files = [ "EGO.md"; "AGENTS.md" ];
        }
      in
      let cfg =
        { Runtime_config.default with workspace; prompt = prompt_cfg }
      in
      let prompt = Prompt_builder.build ~config:cfg ~tool_registry:None () in
      Alcotest.(check bool)
        "has workspace section" true
        (Test_helpers.string_contains prompt "## Workspace Context");
      Alcotest.(check bool)
        "includes EGO contents" true
        (Test_helpers.string_contains prompt "EGO SENTINEL");
      Alcotest.(check bool)
        "includes AGENTS contents" true
        (Test_helpers.string_contains prompt "AGENTS SENTINEL"))

let test_dynamic_prompt_includes_self_reference () =
  with_temp_workspace (fun workspace ->
      let cfg = { Runtime_config.default with workspace } in
      let prompt = Prompt_builder.build ~config:cfg ~tool_registry:None () in
      Alcotest.(check bool)
        "has self-reference section" true
        (Test_helpers.string_contains prompt "## Self-Reference");
      Alcotest.(check bool)
        "has llms-full.txt URL" true
        (Test_helpers.string_contains prompt "https://clawq.org/llms-full.txt"))

let test_runtime_context_includes_git_details () =
  with_temp_workspace (fun workspace ->
      let git_dir = Filename.concat workspace ".git" in
      let refs_heads = Filename.concat git_dir "refs/heads" in
      Unix.mkdir git_dir 0o755;
      Unix.mkdir (Filename.concat git_dir "refs") 0o755;
      Unix.mkdir refs_heads 0o755;
      write_file (Filename.concat git_dir "HEAD") "ref: refs/heads/main\n";
      write_file (Filename.concat refs_heads "main") "0123456789abcdef\n";
      let prompt_cfg =
        {
          Runtime_config.default.prompt with
          dynamic_enabled = true;
          include_runtime_section = true;
          include_datetime_section = false;
        }
      in
      let cfg =
        { Runtime_config.default with workspace; prompt = prompt_cfg }
      in
      let cwd_before = Sys.getcwd () in
      Fun.protect
        (fun () ->
          Sys.chdir workspace;
          let runtime =
            Prompt_builder.build_runtime_context ~config:cfg ()
            |> Option.value ~default:""
          in
          Alcotest.(check bool)
            "includes os" true
            (Test_helpers.string_contains runtime "- OS: ");
          Alcotest.(check bool)
            "includes repo root" true
            (Test_helpers.string_contains runtime
               ("- Git repo root: " ^ workspace));
          Alcotest.(check bool)
            "includes git branch" true
            (Test_helpers.string_contains runtime "- Git branch: main"))
        ~finally:(fun () -> Sys.chdir cwd_before))

let test_runtime_context_includes_session_details () =
  with_temp_workspace (fun workspace ->
      let prompt_cfg =
        {
          Runtime_config.default.prompt with
          dynamic_enabled = true;
          include_runtime_section = true;
          include_datetime_section = false;
        }
      in
      let cfg =
        { Runtime_config.default with workspace; prompt = prompt_cfg }
      in
      let runtime =
        Prompt_builder.build_runtime_context ~config:cfg
          ~details:
            {
              Prompt_builder.session_id = "telegram:123:456";
              session_name = Some "main";
              is_main_session = true;
              heartbeat_routing_applies = true;
              effective_workspace = workspace;
              workspace_only = true;
              sandbox_backend_requested = "auto";
              sandbox_backend_effective = "bubblewrap";
              shell_is_sandboxed = true;
              shell_policy_summary =
                "shell allowlist + path checks; bubblewrap workspace isolation";
              shell_visible_roots_summary = workspace ^ ", /tmp/extra";
              daemon_uptime_line = Some "- Daemon uptime: 1h 23m";
              background_tasks = [];
              context_usage =
                Some
                  {
                    Prompt_builder.history_messages = 12;
                    estimated_history_tokens = 3456;
                    context_window_tokens = 128000;
                    compaction_threshold_tokens = 96000;
                    max_messages_per_session = 500;
                    compacted_before_turn = true;
                  };
              tunnel_status_line =
                Some "- Tunnel: https://my-tunnel.trycloudflare.com";
              task_tree_summary = None;
            }
          ()
        |> Option.value ~default:""
      in
      Alcotest.(check bool)
        "includes session id" true
        (Test_helpers.string_contains runtime "- Session id: telegram:123:456");
      Alcotest.(check bool)
        "includes heartbeat applicability" true
        (Test_helpers.string_contains runtime
           "- Heartbeat routing enabled for this session: yes");
      Alcotest.(check bool)
        "includes sandbox summary" true
        (Test_helpers.string_contains runtime
           "- Shell sandboxed: yes (requested=auto effective=bubblewrap)");
      Alcotest.(check bool)
        "includes daemon uptime" true
        (Test_helpers.string_contains runtime "- Daemon uptime: 1h 23m");
      Alcotest.(check bool)
        "includes context usage" true
        (Test_helpers.string_contains runtime
           "- Context usage: 12 messages, ~3456/128000 tokens");
      Alcotest.(check bool)
        "includes compaction trigger" true
        (Test_helpers.string_contains runtime
           "- Compaction: before a turn when history > 500 messages or est \
            tokens > 96000; compacted before this turn: yes");
      Alcotest.(check bool)
        "includes no background tasks line" true
        (Test_helpers.string_contains runtime "- Background tasks: none running");
      Alcotest.(check bool)
        "includes tunnel status" true
        (Test_helpers.string_contains runtime
           "- Tunnel: https://my-tunnel.trycloudflare.com"))

let remove_file path = try Sys.remove path with _ -> ()

let test_build_messages_picks_up_workspace_file_changes () =
  with_temp_workspace (fun workspace ->
      let agents_path = Filename.concat workspace "AGENTS.md" in
      write_file agents_path "ORIGINAL AGENTS CONTENT";
      let prompt_cfg =
        {
          Runtime_config.default.prompt with
          dynamic_enabled = true;
          include_tools_section = false;
          include_safety_section = false;
          include_runtime_section = false;
          include_datetime_section = false;
          include_autonomy_section = false;
          workspace_files = [ "AGENTS.md" ];
        }
      in
      let cfg =
        { Runtime_config.default with workspace; prompt = prompt_cfg }
      in
      let agent = Agent.create ~config:cfg () in
      (* First build_messages: should contain original content *)
      let msgs1 = Agent.build_messages agent in
      let sys1 =
        List.find (fun (m : Provider.message) -> m.role = "system") msgs1
      in
      Alcotest.(check bool)
        "first build has original content" true
        (Test_helpers.string_contains sys1.content "ORIGINAL AGENTS CONTENT");
      Alcotest.(check bool)
        "first build lacks updated content" false
        (Test_helpers.string_contains sys1.content "UPDATED AGENTS CONTENT");
      (* Mutate the workspace file on disk *)
      write_file agents_path "UPDATED AGENTS CONTENT";
      (* Second build_messages: should pick up the change *)
      let msgs2 = Agent.build_messages agent in
      let sys2 =
        List.find (fun (m : Provider.message) -> m.role = "system") msgs2
      in
      Alcotest.(check bool)
        "second build has updated content" true
        (Test_helpers.string_contains sys2.content "UPDATED AGENTS CONTENT");
      Alcotest.(check bool)
        "second build lacks original content" false
        (Test_helpers.string_contains sys2.content "ORIGINAL AGENTS CONTENT");
      remove_file agents_path)

let test_build_messages_picks_up_new_workspace_file () =
  with_temp_workspace (fun workspace ->
      let agents_path = Filename.concat workspace "AGENTS.md" in
      let prompt_cfg =
        {
          Runtime_config.default.prompt with
          dynamic_enabled = true;
          include_tools_section = false;
          include_safety_section = false;
          include_runtime_section = false;
          include_datetime_section = false;
          include_autonomy_section = false;
          workspace_files = [ "AGENTS.md" ];
        }
      in
      let cfg =
        { Runtime_config.default with workspace; prompt = prompt_cfg }
      in
      let agent = Agent.create ~config:cfg () in
      (* First build: no AGENTS.md exists yet *)
      let msgs1 = Agent.build_messages agent in
      let sys1 =
        List.find (fun (m : Provider.message) -> m.role = "system") msgs1
      in
      Alcotest.(check bool)
        "no agents content before creation" false
        (Test_helpers.string_contains sys1.content "NEW AGENTS FILE");
      (* Create the file *)
      write_file agents_path "NEW AGENTS FILE";
      (* Second build: should pick it up *)
      let msgs2 = Agent.build_messages agent in
      let sys2 =
        List.find (fun (m : Provider.message) -> m.role = "system") msgs2
      in
      Alcotest.(check bool)
        "picks up newly created file" true
        (Test_helpers.string_contains sys2.content "NEW AGENTS FILE");
      remove_file agents_path)

let test_build_messages_picks_up_deleted_workspace_file () =
  with_temp_workspace (fun workspace ->
      let agents_path = Filename.concat workspace "AGENTS.md" in
      write_file agents_path "DOOMED CONTENT";
      let prompt_cfg =
        {
          Runtime_config.default.prompt with
          dynamic_enabled = true;
          include_tools_section = false;
          include_safety_section = false;
          include_runtime_section = false;
          include_datetime_section = false;
          include_autonomy_section = false;
          workspace_files = [ "AGENTS.md" ];
        }
      in
      let cfg =
        { Runtime_config.default with workspace; prompt = prompt_cfg }
      in
      let agent = Agent.create ~config:cfg () in
      (* First build: file exists *)
      let msgs1 = Agent.build_messages agent in
      let sys1 =
        List.find (fun (m : Provider.message) -> m.role = "system") msgs1
      in
      Alcotest.(check bool)
        "has content before deletion" true
        (Test_helpers.string_contains sys1.content "DOOMED CONTENT");
      (* Delete the file *)
      Sys.remove agents_path;
      (* Second build: content should be gone *)
      let msgs2 = Agent.build_messages agent in
      let sys2 =
        List.find (fun (m : Provider.message) -> m.role = "system") msgs2
      in
      Alcotest.(check bool)
        "content gone after deletion" false
        (Test_helpers.string_contains sys2.content "DOOMED CONTENT"))

let test_dynamic_prompt_includes_autonomy_section () =
  with_temp_workspace (fun workspace ->
      let prompt_cfg =
        {
          Runtime_config.default.prompt with
          dynamic_enabled = true;
          include_tools_section = false;
          include_safety_section = false;
          include_runtime_section = false;
          include_datetime_section = false;
          include_autonomy_section = true;
          include_workspace_section = false;
        }
      in
      let cfg =
        { Runtime_config.default with workspace; prompt = prompt_cfg }
      in
      let prompt = Prompt_builder.build ~config:cfg ~tool_registry:None () in
      Alcotest.(check bool)
        "has autonomy header" true
        (Test_helpers.string_contains prompt "## Autonomous Operation");
      Alcotest.(check bool)
        "has steering information" true
        (Test_helpers.string_contains prompt "steering information");
      Alcotest.(check bool)
        "has continuous execution" true
        (Test_helpers.string_contains prompt "continuous execution");
      Alcotest.(check bool)
        "has explicitly ask you to stop" true
        (Test_helpers.string_contains prompt "explicitly ask you to stop"))

let test_dynamic_prompt_excludes_autonomy_section_when_disabled () =
  with_temp_workspace (fun workspace ->
      let prompt_cfg =
        {
          Runtime_config.default.prompt with
          dynamic_enabled = true;
          include_tools_section = false;
          include_safety_section = false;
          include_runtime_section = false;
          include_datetime_section = false;
          include_autonomy_section = false;
          include_workspace_section = false;
        }
      in
      let cfg =
        { Runtime_config.default with workspace; prompt = prompt_cfg }
      in
      let prompt = Prompt_builder.build ~config:cfg ~tool_registry:None () in
      Alcotest.(check bool)
        "no autonomy header" false
        (Test_helpers.string_contains prompt "## Autonomous Operation"))

let test_autonomy_section_appears_before_workspace () =
  with_temp_workspace (fun workspace ->
      write_file (Filename.concat workspace "EGO.md") "EGO CONTENT";
      let prompt_cfg =
        {
          Runtime_config.default.prompt with
          dynamic_enabled = true;
          include_tools_section = false;
          include_safety_section = false;
          include_runtime_section = false;
          include_datetime_section = false;
          include_autonomy_section = true;
          include_workspace_section = true;
          workspace_files = [ "EGO.md" ];
        }
      in
      let cfg =
        { Runtime_config.default with workspace; prompt = prompt_cfg }
      in
      let prompt = Prompt_builder.build ~config:cfg ~tool_registry:None () in
      let autonomy_pos =
        try
          let _ =
            Str.search_forward
              (Str.regexp_string "## Autonomous Operation")
              prompt 0
          in
          Str.match_beginning ()
        with Not_found -> max_int
      in
      let workspace_pos =
        try
          let _ =
            Str.search_forward
              (Str.regexp_string "## Workspace Context")
              prompt 0
          in
          Str.match_beginning ()
        with Not_found -> max_int
      in
      Alcotest.(check bool)
        "autonomy section found" true (autonomy_pos < max_int);
      Alcotest.(check bool)
        "workspace section found" true (workspace_pos < max_int);
      Alcotest.(check bool)
        "autonomy before workspace" true
        (autonomy_pos < workspace_pos))

let test_workspace_injection_note_present () =
  with_temp_workspace (fun workspace ->
      write_file (Filename.concat workspace "EGO.md") "EGO CONTENT";
      let prompt_cfg =
        {
          Runtime_config.default.prompt with
          dynamic_enabled = true;
          include_tools_section = false;
          include_safety_section = false;
          include_runtime_section = false;
          include_datetime_section = false;
          include_autonomy_section = false;
          workspace_files = [ "EGO.md" ];
        }
      in
      let cfg =
        { Runtime_config.default with workspace; prompt = prompt_cfg }
      in
      let prompt = Prompt_builder.build ~config:cfg ~tool_registry:None () in
      Alcotest.(check bool)
        "has no-reread note" true
        (Test_helpers.string_contains prompt "already injected into this prompt"))

let test_workspace_injection_note_absent_when_no_files () =
  with_temp_workspace (fun workspace ->
      let prompt_cfg =
        {
          Runtime_config.default.prompt with
          dynamic_enabled = true;
          include_tools_section = false;
          include_safety_section = false;
          include_runtime_section = false;
          include_datetime_section = false;
          include_autonomy_section = false;
          workspace_files = [ "EGO.md" ];
        }
      in
      let cfg =
        { Runtime_config.default with workspace; prompt = prompt_cfg }
      in
      let prompt = Prompt_builder.build ~config:cfg ~tool_registry:None () in
      Alcotest.(check bool)
        "no note when no files found" false
        (Test_helpers.string_contains prompt "already injected into this prompt"))

let test_tools_block_sorted_alphabetically () =
  let registry = Tool_registry.create () in
  let mk name =
    {
      Tool.name;
      description = name ^ " desc";
      parameters_schema = `Null;
      invoke = (fun ?context:_ _ -> Lwt.return "ok");
      invoke_stream = None;
      risk_level = Tool.Low;
      deferred = false;
    }
  in
  List.iter
    (fun n -> Tool_registry.register registry (mk n))
    [ "zebra"; "alpha"; "middle" ];
  let lines = Prompt_builder.tools_block (Some registry) in
  let names =
    List.map
      (fun line ->
        match
          String.split_on_char ' ' (String.sub line 2 (String.length line - 2))
        with
        | name :: _ -> name
        | [] -> "")
      lines
  in
  Alcotest.(check (list string))
    "tools sorted alphabetically"
    [ "alpha"; "middle"; "zebra" ]
    names

let index_of hay needle =
  let hlen = String.length hay in
  let nlen = String.length needle in
  let rec loop i =
    if i + nlen > hlen then None
    else if String.sub hay i nlen = needle then Some i
    else loop (i + 1)
  in
  if nlen = 0 then Some 0 else loop 0

let test_background_tasks_appear_after_context_usage () =
  with_temp_workspace (fun workspace ->
      let prompt_cfg =
        {
          Runtime_config.default.prompt with
          dynamic_enabled = true;
          include_runtime_section = true;
          include_datetime_section = false;
        }
      in
      let cfg =
        { Runtime_config.default with workspace; prompt = prompt_cfg }
      in
      let runtime =
        Prompt_builder.build_runtime_context ~config:cfg
          ~details:
            {
              Prompt_builder.session_id = "test:bg:order";
              session_name = None;
              is_main_session = false;
              heartbeat_routing_applies = false;
              effective_workspace = workspace;
              workspace_only = false;
              sandbox_backend_requested = "none";
              sandbox_backend_effective = "none";
              shell_is_sandboxed = false;
              shell_policy_summary = "none";
              shell_visible_roots_summary = workspace;
              daemon_uptime_line = Some "- Daemon uptime: 3m";
              background_tasks =
                [
                  {
                    Prompt_builder.id = 7;
                    runner = "codex";
                    repo_label = "myrepo";
                    branch = "feat-x";
                    status = "running";
                    health = "active";
                    elapsed = "3m";
                  };
                ];
              context_usage =
                Some
                  {
                    Prompt_builder.history_messages = 5;
                    estimated_history_tokens = 1000;
                    context_window_tokens = 128000;
                    compaction_threshold_tokens = 96000;
                    max_messages_per_session = 500;
                    compacted_before_turn = false;
                  };
              tunnel_status_line = None;
              task_tree_summary = Some "- [ ] do stuff";
            }
          ()
        |> Option.value ~default:""
      in
      Alcotest.(check bool)
        "includes background task" true
        (Test_helpers.string_contains runtime
           "#7 codex running 3m health=active repo=myrepo branch=feat-x");
      let ctx_pos =
        index_of runtime "- Context usage:" |> Option.value ~default:(-1)
      in
      let bg_pos =
        index_of runtime "- Background tasks:" |> Option.value ~default:(-1)
      in
      let tree_pos =
        index_of runtime "## Current Tasks" |> Option.value ~default:(-1)
      in
      Alcotest.(check bool) "context_usage position found" true (ctx_pos >= 0);
      Alcotest.(check bool) "background_tasks position found" true (bg_pos >= 0);
      Alcotest.(check bool) "task_tree position found" true (tree_pos >= 0);
      Alcotest.(check bool)
        "background tasks after context usage" true (bg_pos > ctx_pos);
      Alcotest.(check bool)
        "background tasks before task tree" true (bg_pos < tree_pos))

let test_tools_section_includes_shell_exec_example () =
  with_temp_workspace (fun workspace ->
      let registry = Tool_registry.create () in
      Tool_registry.register registry
        {
          Tool.name = "shell_exec";
          description = "Execute a shell command";
          parameters_schema = `Null;
          invoke = (fun ?context:_ _ -> Lwt.return "ok");
          invoke_stream = None;
          risk_level = Tool.Low;
          deferred = false;
        };
      let prompt_cfg =
        {
          Runtime_config.default.prompt with
          dynamic_enabled = true;
          include_tools_section = true;
          include_safety_section = false;
          include_runtime_section = false;
          include_datetime_section = false;
          include_autonomy_section = false;
          include_workspace_section = false;
        }
      in
      let cfg =
        { Runtime_config.default with workspace; prompt = prompt_cfg }
      in
      let prompt =
        Prompt_builder.build ~config:cfg ~tool_registry:(Some registry) ()
      in
      Alcotest.(check bool)
        "has example tool call header" true
        (Test_helpers.string_contains prompt "Example tool call:");
      Alcotest.(check bool)
        "has shell_exec example" true
        (Test_helpers.string_contains prompt "shell_exec(command="))

let test_runtime_context_includes_directory_contents () =
  with_temp_workspace (fun workspace ->
      write_file (Filename.concat workspace "README.md") "hello";
      Unix.mkdir (Filename.concat workspace "src") 0o755;
      let prompt_cfg =
        {
          Runtime_config.default.prompt with
          dynamic_enabled = true;
          include_runtime_section = true;
          include_datetime_section = false;
        }
      in
      let cfg =
        { Runtime_config.default with workspace; prompt = prompt_cfg }
      in
      let cwd_before = Sys.getcwd () in
      Fun.protect
        (fun () ->
          Sys.chdir workspace;
          let runtime =
            Prompt_builder.build_runtime_context ~config:cfg ()
            |> Option.value ~default:""
          in
          Alcotest.(check bool)
            "includes directory contents label" true
            (Test_helpers.string_contains runtime "- Directory contents:");
          Alcotest.(check bool)
            "includes README.md file" true
            (Test_helpers.string_contains runtime "README.md");
          Alcotest.(check bool)
            "includes src/ directory with trailing slash" true
            (Test_helpers.string_contains runtime "src/"))
        ~finally:(fun () -> Sys.chdir cwd_before))

let rec rm_rf path =
  if Sys.is_directory path then begin
    Array.iter
      (fun name -> rm_rf (Filename.concat path name))
      (Sys.readdir path);
    Unix.rmdir path
  end
  else Sys.remove path

let with_temp_git_repo f =
  let base = Filename.get_temp_dir_name () in
  let dir =
    Filename.concat base
      (Printf.sprintf "clawq_gitrepo_%d_%d" (Unix.getpid ()) (Random.bits ()))
  in
  (try rm_rf dir with _ -> ());
  Unix.mkdir dir 0o755;
  Unix.mkdir (Filename.concat dir ".git") 0o755;
  let cwd_before = Sys.getcwd () in
  Fun.protect
    (fun () ->
      Sys.chdir dir;
      f dir)
    ~finally:(fun () ->
      Sys.chdir cwd_before;
      try rm_rf dir with _ -> ())

let minimal_prompt_cfg =
  {
    Runtime_config.default.prompt with
    dynamic_enabled = true;
    include_tools_section = false;
    include_safety_section = false;
    include_runtime_section = false;
    include_datetime_section = false;
    include_autonomy_section = false;
    include_workspace_section = false;
    include_project_docs = true;
  }

let test_project_docs_loaded_from_git_root () =
  with_temp_git_repo (fun dir ->
      write_file (Filename.concat dir "CLAUDE.md") "PROJECT CLAUDE SENTINEL";
      write_file (Filename.concat dir "AGENTS.md") "PROJECT AGENTS SENTINEL";
      let cfg =
        {
          Runtime_config.default with
          workspace = dir;
          prompt = minimal_prompt_cfg;
        }
      in
      let pd =
        Prompt_builder.build_project_docs_message ~config:cfg ~ws_doc_digests:[]
          ()
      in
      (match pd.content with
      | None -> Alcotest.fail "expected project docs content"
      | Some content ->
          Alcotest.(check bool)
            "has CLAUDE.md" true
            (Test_helpers.string_contains content "PROJECT CLAUDE SENTINEL");
          Alcotest.(check bool)
            "has AGENTS.md" true
            (Test_helpers.string_contains content "PROJECT AGENTS SENTINEL"));
      Alcotest.(check int) "two digests" 2 (List.length pd.digests))

let test_project_docs_dedup_vs_workspace () =
  with_temp_git_repo (fun dir ->
      let content = "SHARED CONTENT BETWEEN WORKSPACE AND PROJECT" in
      write_file (Filename.concat dir "AGENTS.md") content;
      let ws_digest = Digest.to_hex (Digest.string content) in
      let cfg =
        {
          Runtime_config.default with
          workspace = dir;
          prompt = minimal_prompt_cfg;
        }
      in
      let pd =
        Prompt_builder.build_project_docs_message ~config:cfg
          ~ws_doc_digests:[ ws_digest ] ()
      in
      match pd.content with
      | None -> ()
      | Some c ->
          Alcotest.(check bool)
            "deduped AGENTS.md not in project docs" false
            (Test_helpers.string_contains c
               "SHARED CONTENT BETWEEN WORKSPACE AND PROJECT"))

let test_project_docs_dedup_self () =
  with_temp_git_repo (fun dir ->
      let same_content = "IDENTICAL CONTENT IN BOTH FILES" in
      write_file (Filename.concat dir "CLAUDE.md") same_content;
      write_file (Filename.concat dir "AGENTS.md") same_content;
      let cfg =
        {
          Runtime_config.default with
          workspace = dir;
          prompt = minimal_prompt_cfg;
        }
      in
      let pd =
        Prompt_builder.build_project_docs_message ~config:cfg ~ws_doc_digests:[]
          ()
      in
      Alcotest.(check int)
        "only one digest for identical content" 1 (List.length pd.digests);
      match pd.content with
      | None -> Alcotest.fail "expected content"
      | Some c ->
          let count =
            let needle = "IDENTICAL CONTENT IN BOTH FILES" in
            let nlen = String.length needle in
            let rec loop i acc =
              if i + nlen > String.length c then acc
              else if String.sub c i nlen = needle then loop (i + nlen) (acc + 1)
              else loop (i + 1) acc
            in
            loop 0 0
          in
          Alcotest.(check int) "content appears exactly once" 1 count)

let test_project_docs_disabled_via_config () =
  with_temp_git_repo (fun dir ->
      write_file (Filename.concat dir "CLAUDE.md") "SHOULD NOT APPEAR";
      let cfg =
        {
          Runtime_config.default with
          workspace = dir;
          prompt = { minimal_prompt_cfg with include_project_docs = false };
        }
      in
      let pd =
        Prompt_builder.build_project_docs_message ~config:cfg ~ws_doc_digests:[]
          ()
      in
      Alcotest.(check bool) "no content when disabled" true (pd.content = None))

let test_project_docs_budget_truncation () =
  with_temp_git_repo (fun dir ->
      let big = String.make 200 'X' in
      write_file (Filename.concat dir "CLAUDE.md") big;
      let cfg =
        {
          Runtime_config.default with
          workspace = dir;
          prompt = { minimal_prompt_cfg with max_project_doc_chars = 100 };
        }
      in
      let pd =
        Prompt_builder.build_project_docs_message ~config:cfg ~ws_doc_digests:[]
          ()
      in
      match pd.content with
      | None -> Alcotest.fail "expected content"
      | Some c ->
          Alcotest.(check bool)
            "content is truncated" true
            (Test_helpers.string_contains c "[...truncated...]"))

(* B706: room/thread sessions autoload project docs from their per-room
   effective_cwd (workspace subfolder), not the daemon process cwd. *)
let make_room_dir ?(git = false) () =
  let base = Filename.get_temp_dir_name () in
  let dir =
    Filename.concat base
      (Printf.sprintf "clawq_room_%d_%d" (Unix.getpid ()) (Random.bits ()))
  in
  (try rm_rf dir with _ -> ());
  Unix.mkdir dir 0o755;
  if git then Unix.mkdir (Filename.concat dir ".git") 0o755;
  dir

let test_project_docs_from_effective_cwd_git_repo () =
  (* Process cwd is one git repo (with its own docs); the room's effective_cwd
     is a different git repo. Docs must come from the room, not the process. *)
  with_temp_git_repo (fun process_dir ->
      write_file
        (Filename.concat process_dir "CLAUDE.md")
        "PROCESS CWD SHOULD NOT APPEAR";
      let room = make_room_dir ~git:true () in
      Fun.protect ~finally:(fun () -> try rm_rf room with _ -> ())
      @@ fun () ->
      write_file (Filename.concat room "CLAUDE.md") "ROOM CLAUDE SENTINEL";
      write_file (Filename.concat room "AGENTS.md") "ROOM AGENTS SENTINEL";
      let cfg = { Runtime_config.default with prompt = minimal_prompt_cfg } in
      let pd =
        Prompt_builder.build_project_docs_message ~config:cfg
          ~effective_cwd:room ~ws_doc_digests:[] ()
      in
      match pd.content with
      | None -> Alcotest.fail "expected project docs content from room cwd"
      | Some c ->
          Alcotest.(check bool)
            "room CLAUDE.md loaded" true
            (Test_helpers.string_contains c "ROOM CLAUDE SENTINEL");
          Alcotest.(check bool)
            "room AGENTS.md loaded" true
            (Test_helpers.string_contains c "ROOM AGENTS SENTINEL");
          Alcotest.(check bool)
            "process cwd docs not loaded" false
            (Test_helpers.string_contains c "PROCESS CWD SHOULD NOT APPEAR"))

let test_project_docs_from_non_git_effective_cwd () =
  (* A plain (non-git) room folder with an AGENTS.md should still autoload. *)
  with_temp_git_repo (fun _process_dir ->
      let room = make_room_dir ~git:false () in
      Fun.protect ~finally:(fun () -> try rm_rf room with _ -> ())
      @@ fun () ->
      (* Guard against a polluted temp tree (e.g. a stray /tmp/.git) that would
         make the room resolve to an ancestor git root instead of itself. *)
      (match Prompt_builder.find_git_root_and_dir room with
      | Some _ -> Alcotest.skip ()
      | None -> ());
      write_file (Filename.concat room "AGENTS.md") "PLAIN ROOM AGENTS SENTINEL";
      let cfg = { Runtime_config.default with prompt = minimal_prompt_cfg } in
      let pd =
        Prompt_builder.build_project_docs_message ~config:cfg
          ~effective_cwd:room ~ws_doc_digests:[] ()
      in
      match pd.content with
      | None -> Alcotest.fail "expected project docs from non-git room folder"
      | Some c ->
          Alcotest.(check bool)
            "plain room AGENTS.md loaded" true
            (Test_helpers.string_contains c "PLAIN ROOM AGENTS SENTINEL");
          Alcotest.(check (option string))
            "git_root reports the room dir" (Some room) pd.git_root)

let test_project_docs_refresh_on_effective_cwd_change () =
  (* Agent created with no effective_cwd in a docless git repo; later bound to a
     room with docs. refresh_project_docs_if_changed must load the room docs. *)
  with_temp_git_repo (fun _process_dir ->
      let cfg = { Runtime_config.default with prompt = minimal_prompt_cfg } in
      let agent = Agent.create ~config:cfg () in
      let room = make_room_dir ~git:true () in
      Fun.protect ~finally:(fun () -> try rm_rf room with _ -> ())
      @@ fun () ->
      write_file (Filename.concat room "AGENTS.md") "REBOUND ROOM SENTINEL";
      agent.Agent.effective_cwd <- Some room;
      let event = Agent.refresh_project_docs_if_changed agent in
      Alcotest.(check bool) "refresh emitted an event" true (event <> None);
      match agent.Agent.project_docs_content with
      | None -> Alcotest.fail "expected project docs after rebind"
      | Some c ->
          Alcotest.(check bool)
            "rebound room docs loaded" true
            (Test_helpers.string_contains c "REBOUND ROOM SENTINEL");
          Alcotest.(check (option string))
            "git_root updated to room" (Some room)
            agent.Agent.project_docs_git_root)

let test_project_docs_docless_rebind_no_event () =
  (* B706 regression: binding a room to a doc-less folder must update the
     resolved root (so later docs are picked up) but must NOT emit a spurious
     "project instructions refreshed" event/notification when no docs exist. *)
  with_temp_git_repo (fun _process_dir ->
      let cfg = { Runtime_config.default with prompt = minimal_prompt_cfg } in
      let agent = Agent.create ~config:cfg () in
      let room = make_room_dir ~git:true () in
      Fun.protect ~finally:(fun () -> try rm_rf room with _ -> ())
      @@ fun () ->
      (* Room has no CLAUDE.md/AGENTS.md. *)
      agent.Agent.effective_cwd <- Some room;
      let event = Agent.refresh_project_docs_if_changed agent in
      Alcotest.(check bool) "no event for doc-less rebind" true (event = None);
      Alcotest.(check (option string))
        "resolved root still updated to room" (Some room)
        agent.Agent.project_docs_git_root;
      Alcotest.(check bool)
        "content remains empty" true
        (agent.Agent.project_docs_content = None))

let test_project_docs_main_session_non_git_unchanged () =
  (* Scope guard (B706): the non-git fallback applies only to per-room
     effective_cwd. A main session (no effective_cwd) in a non-git cwd must NOT
     start autoloading docs from that cwd — prior behavior is preserved. *)
  let room = make_room_dir ~git:false () in
  Fun.protect ~finally:(fun () -> try rm_rf room with _ -> ()) @@ fun () ->
  (match Prompt_builder.find_git_root_and_dir room with
  | Some _ -> Alcotest.skip ()
  | None -> ());
  write_file (Filename.concat room "CLAUDE.md") "MAIN NON GIT SHOULD NOT LOAD";
  let cwd_before = Sys.getcwd () in
  Fun.protect ~finally:(fun () -> Sys.chdir cwd_before) @@ fun () ->
  Sys.chdir room;
  let cfg = { Runtime_config.default with prompt = minimal_prompt_cfg } in
  let pd =
    Prompt_builder.build_project_docs_message ~config:cfg ~ws_doc_digests:[] ()
  in
  Alcotest.(check bool)
    "main session in non-git cwd loads no project docs" true (pd.content = None)

let test_agent_tmpl =
  {
    Agent_template.name = "test_coder";
    description = "A test coder agent";
    role = Agent_template.Coder;
    system_prompt = "You are a coder.";
    goal = "";
    backstory = "";
    model = None;
    max_tool_iterations = None;
    allowed_tools = [];
    disallowed_tools = [];
    tool_search_enabled = None;
    reasoning_effort = None;
    cwd = None;
    source = Agent_template.Builtin;
    metadata = [];
  }

(* All default workspace identity files that should be suppressed for agents *)
let all_ws_files =
  [
    ("AGENTS.md", "AGENTS_SENTINEL_CONTENT");
    ("EGO.md", "EGO_SENTINEL_CONTENT");
    ("SOUL.md", "SOUL_SENTINEL_CONTENT");
    ("TOOLS.md", "TOOLS_SENTINEL_CONTENT");
    ("IDENTITY.md", "IDENTITY_SENTINEL_CONTENT");
    ("USER.md", "USER_SENTINEL_CONTENT");
    ("HEARTBEAT.md", "HEARTBEAT_SENTINEL_CONTENT");
    ("MEMORY.md", "MEMORY_SENTINEL_CONTENT");
    ("memory.md", "MEMORY_LC_SENTINEL_CONTENT");
  ]

let agent_ws_prompt_cfg ws_files =
  {
    Runtime_config.default.prompt with
    dynamic_enabled = true;
    include_workspace_section = true;
    include_tools_section = false;
    include_safety_section = false;
    include_runtime_section = false;
    include_datetime_section = false;
    include_autonomy_section = false;
    workspace_files = List.map fst ws_files;
  }

let test_workspace_docs_suppressed_for_named_agent () =
  with_temp_workspace (fun workspace ->
      List.iter
        (fun (name, content) ->
          write_file (Filename.concat workspace name) content)
        all_ws_files;
      write_file
        (Filename.concat workspace "BOOTSTRAP.md")
        "BOOTSTRAP_SENTINEL_CONTENT";
      let prompt_cfg = agent_ws_prompt_cfg all_ws_files in
      let cfg =
        { Runtime_config.default with workspace; prompt = prompt_cfg }
      in
      let prompt =
        Prompt_builder.build ~config:cfg ~tool_registry:None
          ~agent_template:test_agent_tmpl ()
      in
      (* Every workspace identity file must be absent *)
      List.iter
        (fun (name, sentinel) ->
          Alcotest.(check bool)
            (Printf.sprintf "%s content excluded for named agent" name)
            false
            (Test_helpers.string_contains prompt sentinel))
        all_ws_files;
      (* BOOTSTRAP.md also excluded *)
      Alcotest.(check bool)
        "BOOTSTRAP.md content excluded for named agent" false
        (Test_helpers.string_contains prompt "BOOTSTRAP_SENTINEL_CONTENT");
      (* Suppression note present *)
      Alcotest.(check bool)
        "has suppression note" true
        (Test_helpers.string_contains prompt "suppressed for named agents");
      (* Workspace root still shown *)
      Alcotest.(check bool)
        "workspace root still shown" true
        (Test_helpers.string_contains prompt workspace);
      (* Agent template system prompt IS present *)
      Alcotest.(check bool)
        "agent system prompt present" true
        (Test_helpers.string_contains prompt "You are a coder."))

let test_agent_goal_backstory_in_prompt () =
  with_temp_workspace (fun workspace ->
      let prompt_cfg = agent_ws_prompt_cfg [] in
      let cfg =
        { Runtime_config.default with workspace; prompt = prompt_cfg }
      in
      let tmpl =
        {
          test_agent_tmpl with
          goal = "GOAL_SENTINEL_XYZ";
          backstory = "BACKSTORY_SENTINEL_XYZ";
        }
      in
      let prompt =
        Prompt_builder.build ~config:cfg ~tool_registry:None
          ~agent_template:tmpl ()
      in
      Alcotest.(check bool)
        "goal present" true
        (Test_helpers.string_contains prompt "GOAL_SENTINEL_XYZ");
      Alcotest.(check bool)
        "backstory present" true
        (Test_helpers.string_contains prompt "BACKSTORY_SENTINEL_XYZ");
      Alcotest.(check bool)
        "goal section header" true
        (Test_helpers.string_contains prompt "## Agent Goal");
      Alcotest.(check bool)
        "backstory section header" true
        (Test_helpers.string_contains prompt "## Agent Backstory"))

let test_agent_template_dynamic_disabled () =
  with_temp_workspace (fun workspace ->
      let prompt_cfg =
        { Runtime_config.default.prompt with dynamic_enabled = false }
      in
      let cfg =
        { Runtime_config.default with workspace; prompt = prompt_cfg }
      in
      let prompt =
        Prompt_builder.build ~config:cfg ~tool_registry:None
          ~agent_template:test_agent_tmpl ()
      in
      (* With dynamic disabled, template system_prompt is returned directly *)
      Alcotest.(check string)
        "returns agent system prompt verbatim" "You are a coder." prompt)

let test_workspace_docs_present_for_default_agent () =
  with_temp_workspace (fun workspace ->
      List.iter
        (fun (name, content) ->
          write_file (Filename.concat workspace name) content)
        all_ws_files;
      let prompt_cfg = agent_ws_prompt_cfg all_ws_files in
      let cfg =
        { Runtime_config.default with workspace; prompt = prompt_cfg }
      in
      let prompt = Prompt_builder.build ~config:cfg ~tool_registry:None () in
      (* Without agent template, workspace files should be present.
         SOUL.md is excluded when EGO.md exists (legacy fallback). *)
      List.iter
        (fun (name, sentinel) ->
          let expect = name <> "SOUL.md" in
          Alcotest.(check bool)
            (Printf.sprintf "%s content %s for default agent" name
               (if expect then "present" else "excluded (EGO.md exists)"))
            expect
            (Test_helpers.string_contains prompt sentinel))
        all_ws_files;
      (* No suppression note *)
      Alcotest.(check bool)
        "no suppression note" false
        (Test_helpers.string_contains prompt "suppressed for named agents"))

let test_developer_message_in_build_messages () =
  with_temp_git_repo (fun dir ->
      write_file (Filename.concat dir "CLAUDE.md") "DEV MSG SENTINEL";
      let cfg =
        {
          Runtime_config.default with
          workspace = dir;
          prompt = minimal_prompt_cfg;
        }
      in
      let agent = Agent.create ~config:cfg () in
      let msgs = Agent.build_messages agent in
      let dev_msgs =
        List.filter (fun (m : Provider.message) -> m.role = "developer") msgs
      in
      Alcotest.(check bool) "has developer message" true (dev_msgs <> []);
      let dev_content = (List.hd dev_msgs).content in
      Alcotest.(check bool)
        "developer msg has project docs" true
        (Test_helpers.string_contains dev_content "DEV MSG SENTINEL"))

let test_no_developer_message_when_no_project_docs () =
  with_temp_git_repo (fun _dir ->
      let cfg =
        {
          Runtime_config.default with
          workspace = _dir;
          prompt = minimal_prompt_cfg;
        }
      in
      let agent = Agent.create ~config:cfg () in
      let msgs = Agent.build_messages agent in
      let dev_msgs =
        List.filter (fun (m : Provider.message) -> m.role = "developer") msgs
      in
      Alcotest.(check bool)
        "no developer message without project docs" true (dev_msgs = []))

let test_subdir_docs_injected_on_first_file_access () =
  with_temp_git_repo (fun dir ->
      let subdir = Filename.concat dir "src" in
      Unix.mkdir subdir 0o755;
      write_file (Filename.concat subdir "CLAUDE.md") "SUBDIR CLAUDE CONTENT";
      let cfg =
        {
          Runtime_config.default with
          workspace = dir;
          prompt = minimal_prompt_cfg;
        }
      in
      let agent = Agent.create ~config:cfg () in
      let tc : Provider.tool_call =
        {
          id = "tc1";
          function_name = "file_read";
          arguments =
            Printf.sprintf {|{"path": "%s"}|} (Filename.concat subdir "foo.ml");
        }
      in
      let events = Agent.observe_project_docs agent tc in
      Alcotest.(check bool) "injected event for subdir" true (events <> []);
      let msg = List.hd events in
      Alcotest.(check string) "subdir doc role is user" "user" msg.role;
      Alcotest.(check bool)
        "event has subdir content" true
        (Test_helpers.string_contains msg.content "SUBDIR CLAUDE CONTENT");
      Alcotest.(check bool)
        "event has metadata" true
        (Test_helpers.string_contains msg.content "project instructions loaded"))

let test_subdir_docs_not_injected_twice () =
  with_temp_git_repo (fun dir ->
      let subdir = Filename.concat dir "src" in
      Unix.mkdir subdir 0o755;
      write_file (Filename.concat subdir "CLAUDE.md") "SUBDIR CONTENT";
      let cfg =
        {
          Runtime_config.default with
          workspace = dir;
          prompt = minimal_prompt_cfg;
        }
      in
      let agent = Agent.create ~config:cfg () in
      let tc : Provider.tool_call =
        {
          id = "tc1";
          function_name = "file_read";
          arguments =
            Printf.sprintf {|{"path": "%s"}|} (Filename.concat subdir "foo.ml");
        }
      in
      let events1 = Agent.observe_project_docs agent tc in
      let events2 = Agent.observe_project_docs agent tc in
      Alcotest.(check bool) "first access injects" true (events1 <> []);
      Alcotest.(check bool) "second access does not inject" true (events2 = []))

let test_subdir_docs_dedup_vs_root () =
  with_temp_git_repo (fun dir ->
      let root_content = "ROOT AND SUBDIR SAME CONTENT" in
      write_file (Filename.concat dir "CLAUDE.md") root_content;
      let subdir = Filename.concat dir "src" in
      Unix.mkdir subdir 0o755;
      write_file (Filename.concat subdir "CLAUDE.md") root_content;
      let cfg =
        {
          Runtime_config.default with
          workspace = dir;
          prompt = minimal_prompt_cfg;
        }
      in
      let agent = Agent.create ~config:cfg () in
      let tc : Provider.tool_call =
        {
          id = "tc1";
          function_name = "file_read";
          arguments =
            Printf.sprintf {|{"path": "%s"}|} (Filename.concat subdir "foo.ml");
        }
      in
      let events = Agent.observe_project_docs agent tc in
      Alcotest.(check bool) "subdir content deduped vs root" true (events = []))

let test_root_docs_refresh_on_change () =
  with_temp_git_repo (fun dir ->
      write_file (Filename.concat dir "CLAUDE.md") "ORIGINAL";
      let cfg =
        {
          Runtime_config.default with
          workspace = dir;
          prompt = minimal_prompt_cfg;
        }
      in
      let agent = Agent.create ~config:cfg () in
      Alcotest.(check bool)
        "has initial content" true
        (agent.project_docs_content <> None);
      write_file (Filename.concat dir "CLAUDE.md") "MODIFIED";
      let event = Agent.refresh_project_docs_if_changed agent in
      Alcotest.(check bool) "refresh detected change" true (event <> None);
      match agent.project_docs_content with
      | None -> Alcotest.fail "expected content after refresh"
      | Some c ->
          Alcotest.(check bool)
            "content updated to MODIFIED" true
            (Test_helpers.string_contains c "MODIFIED"))

let test_root_docs_no_refresh_when_unchanged () =
  with_temp_git_repo (fun dir ->
      write_file (Filename.concat dir "CLAUDE.md") "STABLE";
      let cfg =
        {
          Runtime_config.default with
          workspace = dir;
          prompt = minimal_prompt_cfg;
        }
      in
      let agent = Agent.create ~config:cfg () in
      let event = Agent.refresh_project_docs_if_changed agent in
      Alcotest.(check bool) "no event when unchanged" true (event = None))

let test_no_spurious_refresh_after_subdir_load () =
  with_temp_git_repo (fun dir ->
      write_file (Filename.concat dir "CLAUDE.md") "ROOT CONTENT";
      let subdir = Filename.concat dir "src" in
      Unix.mkdir subdir 0o755;
      write_file (Filename.concat subdir "CLAUDE.md") "SUBDIR CONTENT";
      let cfg =
        {
          Runtime_config.default with
          workspace = dir;
          prompt = minimal_prompt_cfg;
        }
      in
      let agent = Agent.create ~config:cfg () in
      let tc : Provider.tool_call =
        {
          id = "tc1";
          function_name = "file_read";
          arguments =
            Printf.sprintf {|{"path": "%s"}|} (Filename.concat subdir "foo.ml");
        }
      in
      let _events = Agent.observe_project_docs agent tc in
      let event = Agent.refresh_project_docs_if_changed agent in
      Alcotest.(check bool)
        "no spurious refresh after subdir load" true (event = None))

let test_provider_extract_system_prompt_includes_developer () =
  let msgs =
    [
      Provider.make_message ~role:"system" ~content:"SYS CONTENT";
      Provider.make_message ~role:"developer" ~content:"DEV CONTENT";
      Provider.make_message ~role:"user" ~content:"USER CONTENT";
    ]
  in
  let extracted = Provider.extract_system_prompt msgs in
  Alcotest.(check bool)
    "system prompt includes system" true
    (Test_helpers.string_contains extracted "SYS CONTENT");
  Alcotest.(check bool)
    "system prompt includes developer" true
    (Test_helpers.string_contains extracted "DEV CONTENT");
  Alcotest.(check bool)
    "system prompt excludes user" false
    (Test_helpers.string_contains extracted "USER CONTENT")

let test_developer_role_mapped_to_system_in_json () =
  let msg = Provider.make_message ~role:"developer" ~content:"dev content" in
  let json = Provider.message_to_json msg in
  let open Yojson.Safe.Util in
  let role = json |> member "role" |> to_string in
  Alcotest.(check string) "developer mapped to system in JSON" "system" role;
  let content = json |> member "content" |> to_string in
  Alcotest.(check string) "content preserved" "dev content" content

let suite =
  [
    Alcotest.test_case "dynamic prompt disabled uses base prompt" `Quick
      test_dynamic_prompt_disabled_uses_base_prompt;
    Alcotest.test_case "default prompt enables dynamic workspace context" `Quick
      test_default_prompt_enables_dynamic_workspace_context;
    Alcotest.test_case "dynamic prompt includes workspace files" `Quick
      test_dynamic_prompt_includes_workspace_files;
    Alcotest.test_case "dynamic prompt includes self-reference" `Quick
      test_dynamic_prompt_includes_self_reference;
    Alcotest.test_case "runtime context includes git details" `Quick
      test_runtime_context_includes_git_details;
    Alcotest.test_case "runtime context includes session details" `Quick
      test_runtime_context_includes_session_details;
    Alcotest.test_case "build_messages picks up workspace file changes" `Quick
      test_build_messages_picks_up_workspace_file_changes;
    Alcotest.test_case "build_messages picks up new workspace file" `Quick
      test_build_messages_picks_up_new_workspace_file;
    Alcotest.test_case "build_messages picks up deleted workspace file" `Quick
      test_build_messages_picks_up_deleted_workspace_file;
    Alcotest.test_case "dynamic prompt includes autonomy section" `Quick
      test_dynamic_prompt_includes_autonomy_section;
    Alcotest.test_case "dynamic prompt excludes autonomy section when disabled"
      `Quick test_dynamic_prompt_excludes_autonomy_section_when_disabled;
    Alcotest.test_case "autonomy section appears before workspace" `Quick
      test_autonomy_section_appears_before_workspace;
    Alcotest.test_case "workspace injection note present when files injected"
      `Quick test_workspace_injection_note_present;
    Alcotest.test_case "workspace injection note absent when no files" `Quick
      test_workspace_injection_note_absent_when_no_files;
    Alcotest.test_case "tools block sorted alphabetically" `Quick
      test_tools_block_sorted_alphabetically;
    Alcotest.test_case "background tasks appear after context usage" `Quick
      test_background_tasks_appear_after_context_usage;
    Alcotest.test_case "tools section includes shell_exec example" `Quick
      test_tools_section_includes_shell_exec_example;
    Alcotest.test_case "runtime context includes directory contents" `Quick
      test_runtime_context_includes_directory_contents;
    Alcotest.test_case "project docs loaded from git root" `Quick
      test_project_docs_loaded_from_git_root;
    Alcotest.test_case "project docs dedup vs workspace" `Quick
      test_project_docs_dedup_vs_workspace;
    Alcotest.test_case "project docs dedup self (identical files)" `Quick
      test_project_docs_dedup_self;
    Alcotest.test_case "project docs disabled via config" `Quick
      test_project_docs_disabled_via_config;
    Alcotest.test_case "project docs budget truncation" `Quick
      test_project_docs_budget_truncation;
    Alcotest.test_case "project docs from effective_cwd git repo (B706)" `Quick
      test_project_docs_from_effective_cwd_git_repo;
    Alcotest.test_case "project docs from non-git effective_cwd (B706)" `Quick
      test_project_docs_from_non_git_effective_cwd;
    Alcotest.test_case "project docs refresh on effective_cwd change (B706)"
      `Quick test_project_docs_refresh_on_effective_cwd_change;
    Alcotest.test_case "project docs doc-less rebind emits no event (B706)"
      `Quick test_project_docs_docless_rebind_no_event;
    Alcotest.test_case "project docs main session non-git unchanged (B706)"
      `Quick test_project_docs_main_session_non_git_unchanged;
    Alcotest.test_case "workspace docs suppressed for named agent" `Quick
      test_workspace_docs_suppressed_for_named_agent;
    Alcotest.test_case "agent goal and backstory in prompt" `Quick
      test_agent_goal_backstory_in_prompt;
    Alcotest.test_case "agent template with dynamic disabled" `Quick
      test_agent_template_dynamic_disabled;
    Alcotest.test_case "workspace docs present for default agent" `Quick
      test_workspace_docs_present_for_default_agent;
    Alcotest.test_case "developer message in build_messages" `Quick
      test_developer_message_in_build_messages;
    Alcotest.test_case "no developer message without project docs" `Quick
      test_no_developer_message_when_no_project_docs;
    Alcotest.test_case "subdir docs injected on first file access" `Quick
      test_subdir_docs_injected_on_first_file_access;
    Alcotest.test_case "subdir docs not injected twice" `Quick
      test_subdir_docs_not_injected_twice;
    Alcotest.test_case "subdir docs dedup vs root" `Quick
      test_subdir_docs_dedup_vs_root;
    Alcotest.test_case "root docs refresh on change" `Quick
      test_root_docs_refresh_on_change;
    Alcotest.test_case "root docs no refresh when unchanged" `Quick
      test_root_docs_no_refresh_when_unchanged;
    Alcotest.test_case "no spurious refresh after subdir load" `Quick
      test_no_spurious_refresh_after_subdir_load;
    Alcotest.test_case "provider extract_system_prompt includes developer"
      `Quick test_provider_extract_system_prompt_includes_developer;
    Alcotest.test_case "developer role mapped to system in JSON" `Quick
      test_developer_role_mapped_to_system_in_json;
  ]
