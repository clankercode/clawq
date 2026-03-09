(* test_config_show.ml — Tests for config show + redaction *)

let test_redact_api_key () =
  let json =
    `Assoc
      [
        ( "providers",
          `Assoc [ ("openai", `Assoc [ ("api_key", `String "sk-secret123") ]) ]
        );
      ]
  in
  let redacted = Config_show.redact_json json in
  let key =
    match redacted with
    | `Assoc fields -> (
        match List.assoc_opt "providers" fields with
        | Some (`Assoc providers) -> (
            match List.assoc_opt "openai" providers with
            | Some (`Assoc p) -> (
                match List.assoc_opt "api_key" p with
                | Some (`String s) -> s
                | _ -> "not found")
            | _ -> "not found")
        | _ -> "not found")
    | _ -> "not found"
  in
  Alcotest.(check string) "redacted" "***" key

let test_redact_bot_token () =
  let json = `Assoc [ ("bot_token", `String "MTk123.abc") ] in
  let redacted = Config_show.redact_json json in
  match redacted with
  | `Assoc [ ("bot_token", `String s) ] ->
      Alcotest.(check string) "redacted" "***" s
  | _ -> Alcotest.fail "unexpected structure"

let test_redact_preserves_non_secret () =
  let json = `Assoc [ ("host", `String "localhost"); ("port", `Int 13451) ] in
  let redacted = Config_show.redact_json json in
  Alcotest.(check string)
    "preserved"
    (Yojson.Safe.to_string json)
    (Yojson.Safe.to_string redacted)

let test_redact_empty_string () =
  let json = `Assoc [ ("api_key", `String "") ] in
  let redacted = Config_show.redact_json json in
  match redacted with
  | `Assoc [ ("api_key", `String s) ] ->
      Alcotest.(check string) "empty preserved" "" s
  | _ -> Alcotest.fail "unexpected structure"

let test_redact_nested () =
  let json =
    `Assoc
      [
        ( "channels",
          `Assoc
            [
              ( "discord",
                `Assoc
                  [
                    ("bot_token", `String "secret");
                    ("allow_guilds", `List [ `String "*" ]);
                  ] );
            ] );
      ]
  in
  let redacted = Config_show.redact_json json in
  match redacted with
  | `Assoc [ ("channels", `Assoc [ ("discord", `Assoc fields) ]) ] ->
      let token =
        match List.assoc_opt "bot_token" fields with
        | Some (`String s) -> s
        | _ -> "not found"
      in
      let guilds =
        match List.assoc_opt "allow_guilds" fields with
        | Some (`List [ `String s ]) -> s
        | _ -> "not found"
      in
      Alcotest.(check string) "token redacted" "***" token;
      Alcotest.(check string) "guilds preserved" "*" guilds
  | _ -> Alcotest.fail "unexpected structure"

(* --- smart_render tests --- *)

let make_large_section name n =
  let fields =
    List.init n (fun i -> (Printf.sprintf "field_%d" i, `String "value"))
  in
  (name, `Assoc fields)

let test_smart_render_small_passthrough () =
  let json = `Assoc [ ("host", `String "localhost"); ("port", `Int 8080) ] in
  let result = Config_show.smart_render json in
  let expected = Yojson.Safe.pretty_to_string ~std:true json in
  Alcotest.(check string) "small json passes through" expected result

let contains s sub =
  try
    ignore (Str.search_forward (Str.regexp_string sub) s 0);
    true
  with Not_found -> false

let test_smart_render_preserves_scalars () =
  let json =
    `Assoc
      ([
         ("workspace", `String "/home/user/project");
         ("default_temperature", `Float 0.7);
         ("enabled", `Bool true);
       ]
      @ [ make_large_section "channels" 80 ]
      @ [ make_large_section "security" 80 ])
  in
  let result = Config_show.smart_render json in
  Alcotest.(check bool) "contains workspace" true (contains result "workspace");
  Alcotest.(check bool)
    "contains workspace value" true
    (contains result "/home/user/project");
  Alcotest.(check bool) "contains temperature" true (contains result "0.7");
  Alcotest.(check bool)
    "channels summarized" true
    (contains result "channels: {80 fields}");
  Alcotest.(check bool)
    "security summarized" true
    (contains result "security: {80 fields}")

let test_smart_render_section_summary_format () =
  let json =
    `Assoc [ ("name", `String "test"); make_large_section "big_section" 100 ]
  in
  let result = Config_show.smart_render json in
  Alcotest.(check bool)
    "has section hint" true
    (contains result "Sections (use 'config show <name>' for details):");
  Alcotest.(check bool)
    "has section summary" true
    (contains result "big_section: {100 fields}")

let test_smart_render_with_prefix () =
  let json =
    `Assoc
      [
        ("cli", `Bool true);
        make_large_section "telegram" 50;
        make_large_section "discord" 40;
      ]
  in
  let result = Config_show.smart_render ~prefix:"channels" json in
  Alcotest.(check bool)
    "telegram with prefix" true
    (contains result "channels.telegram: {50 fields}");
  Alcotest.(check bool)
    "discord with prefix" true
    (contains result "channels.discord: {40 fields}");
  Alcotest.(check bool)
    "scalar preserved" true
    (contains result "\"cli\": true")

let test_smart_render_all_scalars () =
  (* Even if over threshold, if no sections, just render everything *)
  let fields =
    List.init 200 (fun i ->
        (Printf.sprintf "key_%d" i, `String (String.make 20 'x')))
  in
  let json = `Assoc fields in
  let result = Config_show.smart_render json in
  let expected = Yojson.Safe.pretty_to_string ~std:true json in
  Alcotest.(check string) "all-scalar renders fully" expected result

let test_smart_render_list_sections () =
  let items =
    List.init 20 (fun i ->
        `Assoc [ ("name", `String (Printf.sprintf "item_%d" i)) ])
  in
  let json =
    `Assoc
      [
        ("count", `Int 20); ("items", `List items); make_large_section "meta" 60;
      ]
  in
  let result = Config_show.smart_render json in
  Alcotest.(check bool)
    "list summarized" true
    (contains result "items: [20 items]")

(* --- resolve_dot_path tests --- *)

let test_resolve_dot_path_single () =
  let json = `Assoc [ ("gateway", `Assoc [ ("host", `String "0.0.0.0") ]) ] in
  match Config_show.resolve_dot_path json "gateway" with
  | Some (`Assoc [ ("host", `String "0.0.0.0") ]) -> ()
  | _ -> Alcotest.fail "single-level dot path failed"

let test_resolve_dot_path_nested () =
  let json =
    `Assoc
      [ ("channels", `Assoc [ ("discord", `Assoc [ ("intents", `Int 513) ]) ]) ]
  in
  match Config_show.resolve_dot_path json "channels.discord" with
  | Some (`Assoc [ ("intents", `Int 513) ]) -> ()
  | _ -> Alcotest.fail "nested dot path failed"

let test_resolve_dot_path_missing () =
  let json = `Assoc [ ("a", `Int 1) ] in
  match Config_show.resolve_dot_path json "a.b" with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for missing path"

let suite =
  [
    Alcotest.test_case "redact api_key" `Quick test_redact_api_key;
    Alcotest.test_case "redact bot_token" `Quick test_redact_bot_token;
    Alcotest.test_case "preserve non-secrets" `Quick
      test_redact_preserves_non_secret;
    Alcotest.test_case "preserve empty secrets" `Quick test_redact_empty_string;
    Alcotest.test_case "redact nested" `Quick test_redact_nested;
    Alcotest.test_case "smart_render small passthrough" `Quick
      test_smart_render_small_passthrough;
    Alcotest.test_case "smart_render preserves scalars" `Quick
      test_smart_render_preserves_scalars;
    Alcotest.test_case "smart_render section summary format" `Quick
      test_smart_render_section_summary_format;
    Alcotest.test_case "smart_render with prefix" `Quick
      test_smart_render_with_prefix;
    Alcotest.test_case "smart_render all scalars" `Quick
      test_smart_render_all_scalars;
    Alcotest.test_case "smart_render list sections" `Quick
      test_smart_render_list_sections;
    Alcotest.test_case "resolve_dot_path single" `Quick
      test_resolve_dot_path_single;
    Alcotest.test_case "resolve_dot_path nested" `Quick
      test_resolve_dot_path_nested;
    Alcotest.test_case "resolve_dot_path missing" `Quick
      test_resolve_dot_path_missing;
  ]
