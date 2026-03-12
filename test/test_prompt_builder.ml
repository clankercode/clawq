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
        (contains prompt "## Workspace Context");
      Alcotest.(check bool)
        "includes EGO contents" true
        (contains prompt "EGO SENTINEL");
      Alcotest.(check bool)
        "includes AGENTS contents" true
        (contains prompt "AGENTS SENTINEL"))

let test_dynamic_prompt_includes_self_reference () =
  with_temp_workspace (fun workspace ->
      let cfg = { Runtime_config.default with workspace } in
      let prompt = Prompt_builder.build ~config:cfg ~tool_registry:None () in
      Alcotest.(check bool)
        "has self-reference section" true
        (contains prompt "## Self-Reference");
      Alcotest.(check bool)
        "has llms-full.txt URL" true
        (contains prompt "https://clawq.org/llms-full.txt"))

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
          Alcotest.(check bool) "includes os" true (contains runtime "- OS: ");
          Alcotest.(check bool)
            "includes repo root" true
            (contains runtime ("- Git repo root: " ^ workspace));
          Alcotest.(check bool)
            "includes git branch" true
            (contains runtime "- Git branch: main"))
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
              task_tree_summary = None;
            }
          ()
        |> Option.value ~default:""
      in
      Alcotest.(check bool)
        "includes session id" true
        (contains runtime "- Session id: telegram:123:456");
      Alcotest.(check bool)
        "includes heartbeat applicability" true
        (contains runtime "- Heartbeat routing applies: yes");
      Alcotest.(check bool)
        "includes sandbox summary" true
        (contains runtime
           "- Shell sandboxed: yes (requested=auto effective=bubblewrap)");
      Alcotest.(check bool)
        "includes daemon uptime" true
        (contains runtime "- Daemon uptime: 1h 23m");
      Alcotest.(check bool)
        "includes context usage" true
        (contains runtime "- Context usage: 12 messages, ~3456/128000 tokens");
      Alcotest.(check bool)
        "includes compaction trigger" true
        (contains runtime
           "- Compaction: before a turn when history > 500 messages or est \
            tokens > 96000; compacted before this turn: yes");
      Alcotest.(check bool)
        "includes no background tasks line" true
        (contains runtime "- Background tasks: none running"))

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
        (contains sys1.content "ORIGINAL AGENTS CONTENT");
      Alcotest.(check bool)
        "first build lacks updated content" false
        (contains sys1.content "UPDATED AGENTS CONTENT");
      (* Mutate the workspace file on disk *)
      write_file agents_path "UPDATED AGENTS CONTENT";
      (* Second build_messages: should pick up the change *)
      let msgs2 = Agent.build_messages agent in
      let sys2 =
        List.find (fun (m : Provider.message) -> m.role = "system") msgs2
      in
      Alcotest.(check bool)
        "second build has updated content" true
        (contains sys2.content "UPDATED AGENTS CONTENT");
      Alcotest.(check bool)
        "second build lacks original content" false
        (contains sys2.content "ORIGINAL AGENTS CONTENT");
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
        (contains sys1.content "NEW AGENTS FILE");
      (* Create the file *)
      write_file agents_path "NEW AGENTS FILE";
      (* Second build: should pick it up *)
      let msgs2 = Agent.build_messages agent in
      let sys2 =
        List.find (fun (m : Provider.message) -> m.role = "system") msgs2
      in
      Alcotest.(check bool)
        "picks up newly created file" true
        (contains sys2.content "NEW AGENTS FILE");
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
        (contains sys1.content "DOOMED CONTENT");
      (* Delete the file *)
      Sys.remove agents_path;
      (* Second build: content should be gone *)
      let msgs2 = Agent.build_messages agent in
      let sys2 =
        List.find (fun (m : Provider.message) -> m.role = "system") msgs2
      in
      Alcotest.(check bool)
        "content gone after deletion" false
        (contains sys2.content "DOOMED CONTENT"))

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
        (contains prompt "## Autonomous Operation");
      Alcotest.(check bool)
        "has steering information" true
        (contains prompt "steering information");
      Alcotest.(check bool)
        "has continuous execution" true
        (contains prompt "continuous execution");
      Alcotest.(check bool)
        "has explicitly ask you to stop" true
        (contains prompt "explicitly ask you to stop"))

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
        (contains prompt "## Autonomous Operation"))

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
        (contains prompt "already injected into this prompt"))

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
        (contains prompt "already injected into this prompt"))

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
              task_tree_summary = Some "- [ ] do stuff";
            }
          ()
        |> Option.value ~default:""
      in
      Alcotest.(check bool)
        "includes background task" true
        (contains runtime
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
            (contains runtime "- Directory contents:");
          Alcotest.(check bool)
            "includes README.md file" true
            (contains runtime "README.md");
          Alcotest.(check bool)
            "includes src/ directory with trailing slash" true
            (contains runtime "src/"))
        ~finally:(fun () -> Sys.chdir cwd_before))

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
    Alcotest.test_case "runtime context includes directory contents" `Quick
      test_runtime_context_includes_directory_contents;
  ]
