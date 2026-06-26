(* test_config_search.ml — Tests for config search *)

(* Helpers for inspecting search output *)

let search_lines query =
  let out = Config_search.search query in
  String.split_on_char '\n' out

let test_search_empty_query () =
  let out = Config_search.search "" in
  Alcotest.(check bool)
    "empty query returns usage" true
    (let h = String.length out and n = String.length "Usage:" in
     h >= n && String.sub out 0 n = "Usage:")

let test_search_no_match () =
  let out = Config_search.search "zzz_no_such_key_xyzzy" in
  Alcotest.(check bool)
    "no match message" true
    (let n = String.length "No config keys" in
     let h = String.length out in
     h >= n && String.sub out 0 n = "No config keys")

let test_search_workspace_exact () =
  (* "workspace" is an exact top-level leaf key — must appear in results *)
  let lines = search_lines "workspace" in
  Alcotest.(check bool)
    "workspace leaf in results" true
    (List.exists
       (fun line -> Test_helpers.string_contains line "workspace")
       lines)

let test_search_api_key_segment () =
  (* "api_key" is the last segment of several paths; exact segment score *)
  let lines = search_lines "api_key" in
  Alcotest.(check bool)
    "providers.<NAME>.api_key in results" true
    (List.exists
       (fun line ->
         Test_helpers.string_contains line "providers.<NAME>.api_key")
       lines);
  Alcotest.(check bool)
    "web_search.api_key in results" true
    (List.exists
       (fun line -> Test_helpers.string_contains line "web_search.api_key")
       lines)

let test_search_security_prefix () =
  (* "security" is a section; exact segment match returns all sub-keys *)
  let lines = search_lines "security" in
  Alcotest.(check bool)
    "security section in results" true
    (List.exists
       (fun line -> Test_helpers.string_contains line "security")
       lines);
  Alcotest.(check bool)
    "security.workspace_only in results" true
    (List.exists
       (fun line -> Test_helpers.string_contains line "security.workspace_only")
       lines);
  Alcotest.(check bool)
    "security.audit_enabled in results" true
    (List.exists
       (fun line -> Test_helpers.string_contains line "security.audit_enabled")
       lines)

let test_search_telegram_section () =
  (* "telegram" appears as a segment under channels *)
  let lines = search_lines "telegram" in
  Alcotest.(check bool)
    "channels.telegram sub-paths present" true
    (List.exists
       (fun line -> Test_helpers.string_contains line "channels.telegram")
       lines)

let test_search_token_substring () =
  (* "token" is a substring of multiple key names *)
  let lines = search_lines "token" in
  Alcotest.(check bool)
    "channels.discord.bot_token in results" true
    (List.exists
       (fun line ->
         Test_helpers.string_contains line "channels.discord.bot_token")
       lines);
  Alcotest.(check bool)
    "gateway.auth_token in results" true
    (List.exists
       (fun line -> Test_helpers.string_contains line "gateway.auth_token")
       lines)

let test_search_ordering_exact_first () =
  (* Exact full-path match must appear before substring matches.
     "workspace" has score 0 (exact); "workspace_only" etc. have higher scores. *)
  let out = Config_search.search "workspace" in
  let lines =
    List.filter (fun l -> String.length l > 0) (String.split_on_char '\n' out)
  in
  (* Find indices of relevant lines *)
  let idx_of substr =
    let rec go i = function
      | [] -> max_int
      | l :: rest ->
          let h = String.length l and n = String.length substr in
          let found =
            if n > h then false
            else
              let f = ref false in
              let j = ref 0 in
              while !j <= h - n && not !f do
                if String.sub l !j n = substr then f := true;
                incr j
              done;
              !f
          in
          if found then i else go (i + 1) rest
    in
    go 0 lines
  in
  let i_exact = idx_of "  workspace " in
  let i_sub = idx_of "security.workspace_only" in
  Alcotest.(check bool)
    "exact workspace before security.workspace_only" true (i_exact < i_sub)

let test_all_schema_paths_non_empty () =
  let paths = Config_set.all_schema_paths () in
  Alcotest.(check bool) "all_schema_paths non-empty" true (paths <> []);
  (* Must include both leaves and sections *)
  let has_leaf = List.exists (fun (_, k) -> k = `Leaf) paths in
  let has_section = List.exists (fun (_, k) -> k = `Section) paths in
  Alcotest.(check bool) "has leaves" true has_leaf;
  Alcotest.(check bool) "has sections" true has_section

let test_all_schema_paths_known_entries () =
  let paths = Config_set.all_schema_paths () in
  let has path kind = List.exists (fun (p, k) -> p = path && k = kind) paths in
  Alcotest.(check bool) "workspace leaf" true (has "workspace" `Leaf);
  Alcotest.(check bool)
    "agent_defaults.primary_model leaf" true
    (has "agent_defaults.primary_model" `Leaf);
  Alcotest.(check bool) "security section" true (has "security" `Section);
  Alcotest.(check bool) "gateway section" true (has "gateway" `Section);
  Alcotest.(check bool)
    "providers.<NAME> section" true
    (has "providers.<NAME>" `Section);
  Alcotest.(check bool)
    "providers.<NAME>.api_key leaf" true
    (has "providers.<NAME>.api_key" `Leaf)

let suite =
  [
    Alcotest.test_case "search empty query" `Quick test_search_empty_query;
    Alcotest.test_case "search no match" `Quick test_search_no_match;
    Alcotest.test_case "search workspace exact" `Quick
      test_search_workspace_exact;
    Alcotest.test_case "search api_key segment" `Quick
      test_search_api_key_segment;
    Alcotest.test_case "search security prefix" `Quick
      test_search_security_prefix;
    Alcotest.test_case "search telegram section" `Quick
      test_search_telegram_section;
    Alcotest.test_case "search token substring" `Quick
      test_search_token_substring;
    Alcotest.test_case "search ordering exact first" `Quick
      test_search_ordering_exact_first;
    Alcotest.test_case "all_schema_paths non-empty" `Quick
      test_all_schema_paths_non_empty;
    Alcotest.test_case "all_schema_paths known entries" `Quick
      test_all_schema_paths_known_entries;
  ]
