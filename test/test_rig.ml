let with_temp_home f =
  let orig_home = Sys.getenv "HOME" in
  let orig_clawq = Sys.getenv_opt "CLAWQ_HOME" in
  let tmp = Filename.temp_dir "clawq_test_rig" "" in
  Unix.putenv "CLAWQ_HOME" tmp;
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv "HOME" orig_home;
      (match orig_clawq with
      | Some v -> Unix.putenv "CLAWQ_HOME" v
      | None -> ( try Unix.putenv "CLAWQ_HOME" "" with _ -> ()));
      ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote tmp))))
    (fun () -> f tmp)

let has needle haystack = String_util.contains haystack needle

let test_find_briefing () =
  match Rig.find_rig "briefing" with
  | None -> Alcotest.fail "Expected to find 'briefing' rig"
  | Some rig -> (
      Alcotest.(check string) "name" "briefing" rig.name;
      Alcotest.(check string) "version" "1.0" rig.version;
      match rig.source with
      | `Builtin -> ()
      | `User _ -> Alcotest.fail "Expected builtin source")

let test_find_nonexistent () =
  match Rig.find_rig "nonexistent" with
  | None -> ()
  | Some _ -> Alcotest.fail "Expected None for nonexistent rig"

let test_all_rigs_includes_briefing () =
  let rigs = Rig.all_rigs () in
  let names = List.map (fun (r : Rig.rig_def) -> r.name) rigs in
  Alcotest.(check bool) "includes briefing" true (List.mem "briefing" names)

let test_state_roundtrip () =
  with_temp_home (fun _tmp ->
      Alcotest.(check bool)
        "not installed initially" false
        (Rig.is_installed ~name:"briefing");
      Rig.mark_installed ~name:"briefing" ~version:"1.0";
      Alcotest.(check bool)
        "installed after mark" true
        (Rig.is_installed ~name:"briefing");
      Rig.mark_removed ~name:"briefing";
      Alcotest.(check bool)
        "removed after mark" false
        (Rig.is_installed ~name:"briefing"))

let test_prompt_install () =
  match Rig.prompt_for ~name:"briefing" ~action:`Install with
  | Error msg -> Alcotest.fail ("Expected Ok, got Error: " ^ msg)
  | Ok prompt ->
      Alcotest.(check bool) "contains sfeed" true (has "sfeed" prompt);
      Alcotest.(check bool)
        "contains feed URLs" true
        (has "news.ycombinator.com" prompt)

let test_prompt_install_fallbacks () =
  match Rig.prompt_for ~name:"briefing" ~action:`Install with
  | Error _ -> Alcotest.fail "Expected Ok"
  | Ok prompt ->
      Alcotest.(check bool)
        "contains newsboat fallback" true (has "newsboat" prompt);
      Alcotest.(check bool)
        "contains python fallback" true
        (has "Python" prompt || has "rss_fetch.py" prompt)

let test_prompt_remove () =
  match Rig.prompt_for ~name:"briefing" ~action:`Remove with
  | Error _ -> Alcotest.fail "Expected Ok"
  | Ok prompt ->
      Alcotest.(check bool)
        "references briefing-daily" true
        (has "briefing-daily" prompt);
      Alcotest.(check bool)
        "references briefing-hourly" true
        (has "briefing-hourly" prompt)

let test_prompt_adjust () =
  match Rig.prompt_for ~name:"briefing" ~action:`Adjust with
  | Error _ -> Alcotest.fail "Expected Ok"
  | Ok prompt ->
      Alcotest.(check bool)
        "references memory_recall" true
        (has "memory_recall" prompt)

let test_prompt_nonexistent () =
  match Rig.prompt_for ~name:"nonexistent" ~action:`Install with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "Expected Error for nonexistent rig"

let test_list_text () =
  let text = Rig.list_text () in
  Alcotest.(check bool) "mentions briefing" true (has "briefing" text);
  Alcotest.(check bool)
    "mentions description" true
    (has "briefing pipeline" text || has "Daily" text || has "daily" text)

let test_user_rig_parsing () =
  with_temp_home (fun tmp ->
      let rigs_dir = Filename.concat tmp "rigs" in
      Unix.mkdir rigs_dir 0o755;
      let rig_path = Filename.concat rigs_dir "myrig.md" in
      let content =
        {|---
name: myrig
description: My custom test rig
version: "2.0"
---
# Install Prompt
This is the install prompt for myrig.
# Adjust Prompt
This is the adjust prompt.
# Remove Prompt
This is the remove prompt.
|}
      in
      let oc = open_out rig_path in
      output_string oc content;
      close_out oc;
      let rigs = Rig.user_rigs () in
      match rigs with
      | [] -> Alcotest.fail "Expected to parse user rig"
      | rig :: _ -> (
          Alcotest.(check string) "name" "myrig" rig.name;
          Alcotest.(check string)
            "description" "My custom test rig" rig.description;
          Alcotest.(check string) "version" "2.0" rig.version;
          Alcotest.(check bool)
            "install prompt" true
            (has "install prompt for myrig" rig.prompts.install);
          match rig.source with
          | `User _ -> ()
          | `Builtin -> Alcotest.fail "Expected User source"))

let test_slash_rig_install () =
  let result = Slash_commands.handle "/rig install briefing" in
  match result with
  | Slash_commands.Rig (Slash_commands.RigInstall "briefing") -> ()
  | _ -> Alcotest.fail "Expected Rig (RigInstall \"briefing\")"

let test_slash_rig_list () =
  let result = Slash_commands.handle "/rig list" in
  match result with
  | Slash_commands.Rig Slash_commands.RigList -> ()
  | _ -> Alcotest.fail "Expected Rig RigList"

let test_slash_rig_bare () =
  let result = Slash_commands.handle "/rig" in
  match result with
  | Slash_commands.Rig Slash_commands.RigList -> ()
  | _ -> Alcotest.fail "Expected Rig RigList for bare /rig"

let test_slash_rig_adjust () =
  let result = Slash_commands.handle "/rig adjust briefing" in
  match result with
  | Slash_commands.Rig (Slash_commands.RigAdjust "briefing") -> ()
  | _ -> Alcotest.fail "Expected Rig (RigAdjust \"briefing\")"

let test_slash_rig_remove () =
  let result = Slash_commands.handle "/rig remove briefing" in
  match result with
  | Slash_commands.Rig (Slash_commands.RigRemove "briefing") -> ()
  | _ -> Alcotest.fail "Expected Rig (RigRemove \"briefing\")"

let test_slash_rigging_alias () =
  let result = Slash_commands.handle "/rigging" in
  match result with
  | Slash_commands.Rig Slash_commands.RigList -> ()
  | _ -> Alcotest.fail "Expected Rig RigList for /rigging alias"

let suite =
  [
    Alcotest.test_case "find_rig briefing" `Quick test_find_briefing;
    Alcotest.test_case "find_rig nonexistent" `Quick test_find_nonexistent;
    Alcotest.test_case "all_rigs includes briefing" `Quick
      test_all_rigs_includes_briefing;
    Alcotest.test_case "state roundtrip" `Quick test_state_roundtrip;
    Alcotest.test_case "install prompt content" `Quick test_prompt_install;
    Alcotest.test_case "install prompt fallbacks" `Quick
      test_prompt_install_fallbacks;
    Alcotest.test_case "remove prompt references cron jobs" `Quick
      test_prompt_remove;
    Alcotest.test_case "adjust prompt references memory" `Quick
      test_prompt_adjust;
    Alcotest.test_case "prompt for nonexistent" `Quick test_prompt_nonexistent;
    Alcotest.test_case "list_text includes briefing" `Quick test_list_text;
    Alcotest.test_case "user rig parsing" `Quick test_user_rig_parsing;
    Alcotest.test_case "slash /rig install" `Quick test_slash_rig_install;
    Alcotest.test_case "slash /rig list" `Quick test_slash_rig_list;
    Alcotest.test_case "slash /rig (bare)" `Quick test_slash_rig_bare;
    Alcotest.test_case "slash /rig adjust" `Quick test_slash_rig_adjust;
    Alcotest.test_case "slash /rig remove" `Quick test_slash_rig_remove;
    Alcotest.test_case "slash /rigging alias" `Quick test_slash_rigging_alias;
  ]
