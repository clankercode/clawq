let test_help_no_args () =
  let result = Completions.cmd_completions [] in
  Alcotest.(check bool)
    "help message contains 'completions'" true
    (let needle = "completions" in
     let haystack = result in
     let n = String.length needle and h = String.length haystack in
     let found = ref false in
     for i = 0 to h - n do
       if String.sub haystack i n = needle then found := true
     done;
     !found)

let test_print_bash () =
  let result = Completions.cmd_completions [ "print"; "--shell"; "bash" ] in
  Alcotest.(check bool)
    "bash script contains function name" true
    (let needle = "_clawq_completions" in
     let h = result and n = String.length needle in
     let found = ref false in
     for i = 0 to String.length h - n do
       if String.sub h i n = needle then found := true
     done;
     !found)

let test_print_zsh () =
  let result = Completions.cmd_completions [ "print"; "--shell"; "zsh" ] in
  Alcotest.(check bool)
    "zsh script starts with #compdef" true
    (String.length result > 9 && String.sub result 0 9 = "#compdef ")

let test_print_fish () =
  let result = Completions.cmd_completions [ "print"; "--shell"; "fish" ] in
  Alcotest.(check bool)
    "fish script contains complete -c clawq" true
    (let needle = "complete -c clawq" in
     let h = result and n = String.length needle in
     let found = ref false in
     for i = 0 to String.length h - n do
       if String.sub h i n = needle then found := true
     done;
     !found)

let test_unknown_shell () =
  let result =
    Completions.cmd_completions [ "print"; "--shell"; "powershell" ]
  in
  Alcotest.(check bool)
    "unknown shell returns error" true
    (let needle = "Error" in
     let h = result and n = String.length needle in
     let found = ref false in
     for i = 0 to String.length h - n do
       if String.sub h i n = needle then found := true
     done;
     !found)

let test_install_path_bash () =
  let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "~" in
  let path = Completions.install_path_for_shell Completions.Bash in
  Alcotest.(check bool)
    "bash install path under HOME" true
    (let prefix = home ^ "/.local" in
     String.length path >= String.length prefix
     && String.sub path 0 (String.length prefix) = prefix)

let test_install_path_fish () =
  let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "~" in
  let path = Completions.install_path_for_shell Completions.Fish in
  Alcotest.(check bool)
    "fish install path ends with .fish" true
    (let suffix = ".fish" in
     let n = String.length suffix and h = String.length path in
     h >= n && String.sub path (h - n) n = suffix);
  Alcotest.(check bool)
    "fish install path under HOME" true
    (let prefix = home ^ "/.config" in
     String.length path >= String.length prefix
     && String.sub path 0 (String.length prefix) = prefix)

let test_bad_usage () =
  let result = Completions.cmd_completions [ "unknown-subcmd" ] in
  Alcotest.(check bool)
    "bad usage returns Usage:" true
    (let needle = "Usage" in
     let h = result and n = String.length needle in
     let found = ref false in
     for i = 0 to String.length h - n do
       if String.sub h i n = needle then found := true
     done;
     !found)

let contains needle haystack =
  let n = String.length needle and h = String.length haystack in
  let found = ref false in
  for i = 0 to h - n do
    if String.sub haystack i n = needle then found := true
  done;
  !found

let test_scripts_include_subagents_and_background_aliases () =
  let bash = Completions.cmd_completions [ "print"; "--shell"; "bash" ] in
  Alcotest.(check bool)
    "bash top-level subagents" true
    (contains "status subagents transcribe" bash);
  Alcotest.(check bool)
    "bash background transcript alias" true
    (contains
       "list show add start wait logs transcript resume message send cancel \
        stop"
       bash);
  Alcotest.(check bool)
    "bash subagents subcommands" true
    (contains "list start stop send transcript" bash);
  Alcotest.(check bool)
    "bash session send" true
    (contains "show inject send events" bash);
  let zsh = Completions.cmd_completions [ "print"; "--shell"; "zsh" ] in
  Alcotest.(check bool)
    "zsh top-level subagents" true
    (contains "subagents:Manage native/local subagents" zsh);
  Alcotest.(check bool)
    "zsh background transcript alias" true
    (contains "'transcript' 'resume' 'message' 'send' 'cancel' 'stop'" zsh);
  let fish = Completions.cmd_completions [ "print"; "--shell"; "fish" ] in
  Alcotest.(check bool)
    "fish top-level subagents" true
    (contains "status subagents transcribe" fish);
  Alcotest.(check bool)
    "fish subagents subcommands" true
    (contains "__fish_seen_subcommand_from subagents" fish)

let suite =
  [
    Alcotest.test_case "help (no args)" `Quick test_help_no_args;
    Alcotest.test_case "print bash" `Quick test_print_bash;
    Alcotest.test_case "print zsh" `Quick test_print_zsh;
    Alcotest.test_case "print fish" `Quick test_print_fish;
    Alcotest.test_case "unknown shell" `Quick test_unknown_shell;
    Alcotest.test_case "install path bash" `Quick test_install_path_bash;
    Alcotest.test_case "install path fish" `Quick test_install_path_fish;
    Alcotest.test_case "bad usage" `Quick test_bad_usage;
    Alcotest.test_case "scripts include native subagents" `Quick
      test_scripts_include_subagents_and_background_aliases;
  ]
