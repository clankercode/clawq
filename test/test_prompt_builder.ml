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
  Fun.protect (fun () -> f dir) ~finally:(fun () -> try Unix.rmdir dir with _ -> ())

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let test_dynamic_prompt_disabled_uses_base_prompt () =
  with_temp_workspace (fun workspace ->
      let prompt_cfg = { Runtime_config.default.prompt with dynamic_enabled = false } in
      let cfg = { Runtime_config.default with workspace; prompt = prompt_cfg } in
      let prompt = Prompt_builder.build ~config:cfg ~tool_registry:None () in
      Alcotest.(check string)
        "dynamic disabled returns base prompt" Prompt_builder.base_prompt prompt)

let test_default_prompt_enables_dynamic_workspace_context () =
  Alcotest.(check bool)
    "default dynamic prompt enabled" true Runtime_config.default.prompt.dynamic_enabled

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
          workspace_files = [ "EGO.md"; "AGENTS.md" ];
        }
      in
      let cfg =
        { Runtime_config.default with workspace; prompt = prompt_cfg }
      in
      let prompt = Prompt_builder.build ~config:cfg ~tool_registry:None () in
      Alcotest.(check bool) "has workspace section" true
        (contains prompt "## Workspace Context");
      Alcotest.(check bool) "includes EGO contents" true
        (contains prompt "EGO SENTINEL");
      Alcotest.(check bool) "includes AGENTS contents" true
        (contains prompt "AGENTS SENTINEL"))

let suite =
  [
    Alcotest.test_case "dynamic prompt disabled uses base prompt" `Quick
      test_dynamic_prompt_disabled_uses_base_prompt;
    Alcotest.test_case "default prompt enables dynamic workspace context" `Quick
      test_default_prompt_enables_dynamic_workspace_context;
    Alcotest.test_case "dynamic prompt includes workspace files" `Quick
      test_dynamic_prompt_includes_workspace_files;
  ]
