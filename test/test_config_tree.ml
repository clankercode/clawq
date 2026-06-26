(* test_config_tree.ml — tests for the tree(1)-style config renderer. *)

let fixture =
  `Assoc
    [
      ("workspace", `String "/home/u/clawq");
      ( "memory",
        `Assoc
          [
            ("backend", `String "sqlite");
            ("compaction_threshold_percent", `Int 80);
          ] );
      ( "providers",
        `Assoc [ ("openai", `Assoc [ ("api_key", `String "sk-secret-123") ]) ]
      );
    ]

let test_tree_has_glyphs () =
  let out = Config_tree.render_json fixture in
  Alcotest.(check bool)
    "has tee glyph" true
    (Test_helpers.string_contains out "├─");
  Alcotest.(check bool)
    "has elbow glyph" true
    (Test_helpers.string_contains out "└─");
  Alcotest.(check bool)
    "has vertical glyph" true
    (Test_helpers.string_contains out "│");
  Alcotest.(check bool)
    "nested leaf shown with value" true
    (Test_helpers.string_contains out "backend = sqlite")

let test_tree_redacts_secrets () =
  (* render_json itself does not redact; render_current does. Apply the same
     redaction the public entry point uses. *)
  let out = Config_tree.render_json (Config_show.redact_json fixture) in
  Alcotest.(check bool)
    "api_key redacted" true
    (Test_helpers.string_contains out "api_key = ***");
  Alcotest.(check bool)
    "secret value absent" false
    (Test_helpers.string_contains out "sk-secret-123")

let test_tree_keys_only_omits_values () =
  let out = Config_tree.render_json ~show_values:false fixture in
  Alcotest.(check bool)
    "key present" true
    (Test_helpers.string_contains out "backend");
  Alcotest.(check bool)
    "no value" false
    (Test_helpers.string_contains out "= sqlite")

let test_tree_root_label () =
  let out = Config_tree.render_json ~root_label:"memory" fixture in
  Alcotest.(check bool)
    "root label is first line" true
    (String.length out >= 6 && String.sub out 0 6 = "memory")

let test_tree_empty_object () =
  let out = Config_tree.render_json (`Assoc [ ("channels", `Assoc []) ]) in
  Alcotest.(check bool)
    "empty marker" true
    (Test_helpers.string_contains out "channels (empty)")

let suite =
  [
    Alcotest.test_case "tree has glyphs" `Quick test_tree_has_glyphs;
    Alcotest.test_case "tree redacts secrets" `Quick test_tree_redacts_secrets;
    Alcotest.test_case "tree keys-only omits values" `Quick
      test_tree_keys_only_omits_values;
    Alcotest.test_case "tree root label" `Quick test_tree_root_label;
    Alcotest.test_case "tree empty object" `Quick test_tree_empty_object;
  ]
