(* test_setup_line.ml — Unit tests for Setup_line pure functions *)

let validate_access_token_valid () =
  Alcotest.(check (result string string))
    "valid" (Ok "my_access_token")
    (Setup_line.validate_channel_access_token "my_access_token")

let validate_access_token_trimmed () =
  Alcotest.(check (result string string))
    "trimmed" (Ok "my_access_token")
    (Setup_line.validate_channel_access_token "  my_access_token  ")

let validate_access_token_empty () =
  match Setup_line.validate_channel_access_token "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty token"

let validate_access_token_whitespace () =
  match Setup_line.validate_channel_access_token "   " with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for whitespace-only"

let validate_secret_valid () =
  Alcotest.(check (result string string))
    "valid" (Ok "my_secret")
    (Setup_line.validate_channel_secret "my_secret")

let validate_secret_empty () =
  match Setup_line.validate_channel_secret "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty secret"

let build_json_roundtrip () =
  let json =
    Setup_line.build_line_json ~channel_access_token:"test_token"
      ~channel_secret:"test_secret" ~allow_from:[ "*" ]
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.line with
  | Some ln ->
      Alcotest.(check string)
        "channel_access_token" "test_token" ln.channel_access_token;
      Alcotest.(check string) "channel_secret" "test_secret" ln.channel_secret;
      Alcotest.(check (list string)) "allow_from" [ "*" ] ln.allow_from
  | None -> Alcotest.fail "expected line config"

let build_json_restricted_users () =
  let json =
    Setup_line.build_line_json ~channel_access_token:"tok" ~channel_secret:"sec"
      ~allow_from:[ "Ua1b2c3d4e5f6"; "Uf6e5d4c3b2a1" ]
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.line with
  | Some ln ->
      Alcotest.(check (list string))
        "allow_from"
        [ "Ua1b2c3d4e5f6"; "Uf6e5d4c3b2a1" ]
        ln.allow_from
  | None -> Alcotest.fail "expected line config"

let post_instructions_content () =
  let s = Setup_line.post_setup_instructions in
  Alcotest.(check bool)
    "has docs url" true
    (Test_helpers.string_contains s "https://clawq.org/channels/#line");
  Alcotest.(check bool)
    "has developer portal" true
    (Test_helpers.string_contains s "developers.line.biz")

let suite =
  [
    Alcotest.test_case "validate_access_token valid" `Quick
      validate_access_token_valid;
    Alcotest.test_case "validate_access_token trimmed" `Quick
      validate_access_token_trimmed;
    Alcotest.test_case "validate_access_token empty" `Quick
      validate_access_token_empty;
    Alcotest.test_case "validate_access_token whitespace" `Quick
      validate_access_token_whitespace;
    Alcotest.test_case "validate_secret valid" `Quick validate_secret_valid;
    Alcotest.test_case "validate_secret empty" `Quick validate_secret_empty;
    Alcotest.test_case "build_json roundtrip" `Quick build_json_roundtrip;
    Alcotest.test_case "build_json restricted users" `Quick
      build_json_restricted_users;
    Alcotest.test_case "post_instructions content" `Quick
      post_instructions_content;
  ]
