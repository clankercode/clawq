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
  let json = `Assoc [ ("host", `String "localhost"); ("port", `Int 3000) ] in
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

let suite =
  [
    Alcotest.test_case "redact api_key" `Quick test_redact_api_key;
    Alcotest.test_case "redact bot_token" `Quick test_redact_bot_token;
    Alcotest.test_case "preserve non-secrets" `Quick
      test_redact_preserves_non_secret;
    Alcotest.test_case "preserve empty secrets" `Quick test_redact_empty_string;
    Alcotest.test_case "redact nested" `Quick test_redact_nested;
  ]
