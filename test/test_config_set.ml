(* test_config_set.ml — Tests for config set/get *)

let check_json msg expected actual =
  Alcotest.(check string)
    msg
    (Yojson.Safe.to_string expected)
    (Yojson.Safe.to_string actual)

let test_infer_value () =
  check_json "true" (`Bool true) (Config_set.infer_value "true");
  check_json "false" (`Bool false) (Config_set.infer_value "false");
  check_json "int" (`Int 42) (Config_set.infer_value "42");
  check_json "float" (`Float 0.7) (Config_set.infer_value "0.7");
  check_json "string" (`String "hello") (Config_set.infer_value "hello");
  check_json "null" `Null (Config_set.infer_value "null")

let test_json_set_simple () =
  let json = `Assoc [ ("a", `Int 1) ] in
  let result = Config_set.json_set [ "a" ] (`Int 2) json in
  Alcotest.(check string) "update a" "{\"a\":2}" (Yojson.Safe.to_string result)

let test_json_set_nested () =
  let json = `Assoc [ ("a", `Assoc [ ("b", `Int 1) ]) ] in
  let result = Config_set.json_set [ "a"; "b" ] (`Int 42) json in
  Alcotest.(check string)
    "nested" "{\"a\":{\"b\":42}}"
    (Yojson.Safe.to_string result)

let test_json_set_create () =
  let json = `Assoc [] in
  let result = Config_set.json_set [ "x"; "y" ] (`String "z") json in
  Alcotest.(check string)
    "create" "{\"x\":{\"y\":\"z\"}}"
    (Yojson.Safe.to_string result)

let test_json_get () =
  let json = `Assoc [ ("a", `Assoc [ ("b", `Int 42) ]) ] in
  Alcotest.(check (option string))
    "found"
    (Some (Yojson.Safe.to_string (`Int 42)))
    (Option.map Yojson.Safe.to_string (Config_set.json_get [ "a"; "b" ] json));
  Alcotest.(check (option string))
    "missing" None
    (Option.map Yojson.Safe.to_string (Config_set.json_get [ "a"; "c" ] json))

let test_roundtrip () =
  Test_helpers.with_temp_dir (fun dir ->
      let path = Filename.concat dir "config.json" in
      let json =
        `Assoc [ ("security", `Assoc [ ("tools_enabled", `Bool true) ]) ]
      in
      let s = Yojson.Safe.pretty_to_string ~std:true json in
      let oc = open_out path in
      output_string oc s;
      close_out oc;
      let result =
        Config_set.json_set [ "security"; "tools_enabled" ] (`Bool false) json
      in
      let v = Config_set.json_get [ "security"; "tools_enabled" ] result in
      Alcotest.(check string)
        "updated" "false"
        (match v with Some j -> Yojson.Safe.to_string j | None -> "none"))

let test_split_path () =
  Alcotest.(check (list string))
    "simple" [ "a"; "b"; "c" ]
    (Config_set.split_path "a.b.c");
  Alcotest.(check (list string)) "single" [ "a" ] (Config_set.split_path "a")

let suite =
  [
    Alcotest.test_case "infer value types" `Quick test_infer_value;
    Alcotest.test_case "json set simple" `Quick test_json_set_simple;
    Alcotest.test_case "json set nested" `Quick test_json_set_nested;
    Alcotest.test_case "json set create" `Quick test_json_set_create;
    Alcotest.test_case "json get" `Quick test_json_get;
    Alcotest.test_case "roundtrip" `Quick test_roundtrip;
    Alcotest.test_case "split path" `Quick test_split_path;
  ]
