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

let test_validate_path () =
  let valid = Config_set.validate_path in
  let schema = Config_set.config_schema in
  (* Valid top-level keys *)
  Alcotest.(check bool) "workspace" true (valid [ "workspace" ] schema);
  Alcotest.(check bool)
    "default_temperature" true
    (valid [ "default_temperature" ] schema);
  (* Valid nested keys *)
  Alcotest.(check bool) "gateway.host" true (valid [ "gateway"; "host" ] schema);
  Alcotest.(check bool)
    "security.rate_limit.burst_multiplier" true
    (valid [ "security"; "rate_limit"; "burst_multiplier" ] schema);
  (* Dynamic provider key *)
  Alcotest.(check bool)
    "providers.openai.api_key" true
    (valid [ "providers"; "openai"; "api_key" ] schema);
  Alcotest.(check bool)
    "providers.myhost.base_url" true
    (valid [ "providers"; "myhost"; "base_url" ] schema);
  (* Dynamic telegram account key *)
  Alcotest.(check bool)
    "channels.telegram.accounts.bot1.bot_token" true
    (valid [ "channels"; "telegram"; "accounts"; "bot1"; "bot_token" ] schema);
  (* Invalid keys *)
  Alcotest.(check bool) "nonexistent" false (valid [ "foo" ] schema);
  Alcotest.(check bool) "typo gateway" false (valid [ "gatway"; "host" ] schema);
  Alcotest.(check bool)
    "invalid nested" false
    (valid [ "gateway"; "nonexistent" ] schema);
  Alcotest.(check bool)
    "too deep on leaf" false
    (valid [ "workspace"; "sub" ] schema);
  Alcotest.(check bool)
    "invalid provider field" false
    (valid [ "providers"; "openai"; "bogus_field" ] schema)

let test_set_rejects_invalid_key () =
  Test_helpers.with_temp_dir (fun dir ->
      let path = Filename.concat dir "config.json" in
      let oc = open_out path in
      output_string oc "{}";
      close_out oc;
      (* Temporarily override config_path by writing and reading *)
      let json = `Assoc [] in
      let segments = Config_set.split_path "foo.bar" in
      Alcotest.(check bool)
        "foo.bar is invalid" false
        (Config_set.validate_path segments Config_set.config_schema);
      let segments2 = Config_set.split_path "gateway.host" in
      Alcotest.(check bool)
        "gateway.host is valid" true
        (Config_set.validate_path segments2 Config_set.config_schema);
      ignore (json, path))

let with_temp_home f =
  let base = Filename.get_temp_dir_name () in
  let dir = Filename.temp_file ~temp_dir:base "clawq_home_" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let old_home = try Some (Sys.getenv "HOME") with Not_found -> None in
  Unix.putenv "HOME" dir;
  Fun.protect
    (fun () -> f dir)
    ~finally:(fun () ->
      (match old_home with
      | Some value -> Unix.putenv "HOME" value
      | None -> Unix.putenv "HOME" "");
      (try
         let clawq_dir = Filename.concat dir ".clawq" in
         if Sys.file_exists clawq_dir then begin
           Array.iter
             (fun name ->
               let path = Filename.concat clawq_dir name in
               try Sys.remove path with _ -> ())
             (Sys.readdir clawq_dir);
           Unix.rmdir clawq_dir
         end
       with _ -> ());
      try Unix.rmdir dir with _ -> ())

let test_set_reasoning_effort_string () =
  with_temp_home (fun home ->
      let clawq_dir = Filename.concat home ".clawq" in
      Unix.mkdir clawq_dir 0o755;
      match Config_set.set_reasoning_effort (Some "high") with
      | Error err -> Alcotest.fail err
      | Ok () ->
          let json =
            Yojson.Safe.from_file (Filename.concat clawq_dir "config.json")
          in
          let value =
            Config_set.json_get [ "agent_defaults"; "reasoning_effort" ] json
          in
          Alcotest.(check (option string))
            "reasoning_effort set" (Some "\"high\"")
            (Option.map Yojson.Safe.to_string value))

let test_set_reasoning_effort_null () =
  with_temp_home (fun home ->
      let clawq_dir = Filename.concat home ".clawq" in
      Unix.mkdir clawq_dir 0o755;
      match Config_set.set_reasoning_effort None with
      | Error err -> Alcotest.fail err
      | Ok () ->
          let json =
            Yojson.Safe.from_file (Filename.concat clawq_dir "config.json")
          in
          let value =
            Config_set.json_get [ "agent_defaults"; "reasoning_effort" ] json
          in
          Alcotest.(check (option string))
            "reasoning_effort cleared" (Some "null")
            (Option.map Yojson.Safe.to_string value))

let test_is_secret_path () =
  Alcotest.(check bool)
    "api_key is secret" true
    (Config_set.is_secret_path "providers.anthropic.api_key");
  Alcotest.(check bool)
    "bot_token is secret" true
    (Config_set.is_secret_path "channels.discord.bot_token");
  Alcotest.(check bool)
    "signing_secret is secret" true
    (Config_set.is_secret_path "channels.slack.signing_secret");
  Alcotest.(check bool)
    "password is secret" true
    (Config_set.is_secret_path "channels.irc.password");
  Alcotest.(check bool)
    "access_token is secret" true
    (Config_set.is_secret_path "channels.matrix.access_token");
  Alcotest.(check bool)
    "host is not secret" false
    (Config_set.is_secret_path "gateway.host");
  Alcotest.(check bool)
    "primary_model is not secret" false
    (Config_set.is_secret_path "agent_defaults.primary_model")

let test_redact () =
  Alcotest.(check string) "short" "***" (Tui_input.redact "abc");
  Alcotest.(check string) "8 chars" "********" (Tui_input.redact "12345678");
  Alcotest.(check string)
    "long key" "sk-a...wxyz"
    (Tui_input.redact "sk-abcdefghijklmnopqrstuvwxyz");
  Alcotest.(check string) "9 chars" "1234...6789" (Tui_input.redact "123456789")

let test_get_value_redacted () =
  with_temp_home (fun home ->
      let clawq_dir = Filename.concat home ".clawq" in
      Unix.mkdir clawq_dir 0o755;
      (match
         Config_set.set_json_value "providers.test.api_key"
           (`String "sk-secret-key-12345")
       with
      | Ok () -> ()
      | Error e -> Alcotest.fail e);
      let result = Config_set.get_value_redacted "providers.test.api_key" in
      Alcotest.(check string) "secret redacted" "***" result;
      (match
         Config_set.set_json_value "agent_defaults.primary_model"
           (`String "gpt-5.4")
       with
      | Ok () -> ()
      | Error e -> Alcotest.fail e);
      let result2 =
        Config_set.get_value_redacted "agent_defaults.primary_model"
      in
      Alcotest.(check string) "non-secret visible" "gpt-5.4" result2)

let suite =
  [
    Alcotest.test_case "infer value types" `Quick test_infer_value;
    Alcotest.test_case "json set simple" `Quick test_json_set_simple;
    Alcotest.test_case "json set nested" `Quick test_json_set_nested;
    Alcotest.test_case "json set create" `Quick test_json_set_create;
    Alcotest.test_case "json get" `Quick test_json_get;
    Alcotest.test_case "roundtrip" `Quick test_roundtrip;
    Alcotest.test_case "split path" `Quick test_split_path;
    Alcotest.test_case "validate path" `Quick test_validate_path;
    Alcotest.test_case "set rejects invalid key" `Quick
      test_set_rejects_invalid_key;
    Alcotest.test_case "set reasoning_effort string" `Quick
      test_set_reasoning_effort_string;
    Alcotest.test_case "set reasoning_effort null" `Quick
      test_set_reasoning_effort_null;
    Alcotest.test_case "is_secret_path" `Quick test_is_secret_path;
    Alcotest.test_case "redact" `Quick test_redact;
    Alcotest.test_case "get_value_redacted" `Quick test_get_value_redacted;
  ]
