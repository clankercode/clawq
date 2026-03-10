let test_run_default () =
  let output = Benchmark.run [ "-n"; "1" ] in
  Alcotest.(check bool)
    "contains header" true
    (String.length output > 0
    &&
    let needle = "clawq benchmark" in
    try
      ignore (Str.search_forward (Str.regexp_string needle) output 0);
      true
    with Not_found -> false);
  Alcotest.(check bool)
    "contains Summary" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Summary") output 0);
       true
     with Not_found -> false)

let test_run_iterations () =
  let output = Benchmark.run [ "--iterations"; "2" ] in
  Alcotest.(check bool)
    "contains iterations: 2" true
    (try
       ignore (Str.search_forward (Str.regexp_string "iterations: 2") output 0);
       true
     with Not_found -> false)

let test_tool_filter () =
  let output = Benchmark.run [ "--tool"; "baseline" ] in
  Alcotest.(check bool)
    "contains baseline" true
    (try
       ignore (Str.search_forward (Str.regexp_string "baseline") output 0);
       true
     with Not_found -> false);
  (* should NOT contain shell_exec_echo *)
  Alcotest.(check bool)
    "does not contain shell_exec_echo" true
    (try
       ignore
         (Str.search_forward (Str.regexp_string "shell_exec_echo") output 0);
       false
     with Not_found -> true)

let test_short_n_flag () =
  let output = Benchmark.run [ "-n"; "3" ] in
  Alcotest.(check bool)
    "contains iterations: 3" true
    (try
       ignore (Str.search_forward (Str.regexp_string "iterations: 3") output 0);
       true
     with Not_found -> false)

let test_unknown_tool () =
  let output = Benchmark.run [ "--tool"; "nonexistent" ] in
  Alcotest.(check bool)
    "contains Error" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Error") output 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "lists available tools" true
    (try
       ignore (Str.search_forward (Str.regexp_string "baseline") output 0);
       true
     with Not_found -> false)

let suite =
  [
    Alcotest.test_case "run default" `Quick test_run_default;
    Alcotest.test_case "run with iterations" `Quick test_run_iterations;
    Alcotest.test_case "short -n flag" `Quick test_short_n_flag;
    Alcotest.test_case "tool filter" `Quick test_tool_filter;
    Alcotest.test_case "unknown tool" `Quick test_unknown_tool;
  ]
