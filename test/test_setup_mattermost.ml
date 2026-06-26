(* test_setup_mattermost.ml — Unit tests for Setup_mattermost pure functions *)

let validate_url_valid_https () =
  Alcotest.(check (result string string))
    "https ok" (Ok "https://mattermost.example.com")
    (Setup_mattermost.validate_url "https://mattermost.example.com")

let validate_url_valid_http () =
  Alcotest.(check (result string string))
    "http ok" (Ok "http://localhost:8065")
    (Setup_mattermost.validate_url "http://localhost:8065")

let validate_url_empty () =
  match Setup_mattermost.validate_url "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty URL"

let validate_url_no_scheme () =
  match Setup_mattermost.validate_url "mattermost.example.com" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for URL without scheme"

let validate_url_ftp_scheme () =
  match Setup_mattermost.validate_url "ftp://mattermost.example.com" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for ftp scheme"

let validate_team_id_valid () =
  (* 26-char lowercase alphanumeric *)
  let id = "abcdefghij0123456789abcdef" in
  Alcotest.(check (result string string))
    "valid team id" (Ok id)
    (Setup_mattermost.validate_team_id id)

let validate_team_id_too_short () =
  match Setup_mattermost.validate_team_id "short" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for short team ID"

let validate_team_id_too_long () =
  match Setup_mattermost.validate_team_id (String.make 27 'a') with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for long team ID"

let validate_team_id_uppercase () =
  match Setup_mattermost.validate_team_id "ABCDEFGHIJ0123456789ABCDEF" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for uppercase team ID"

let validate_team_id_empty () =
  match Setup_mattermost.validate_team_id "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty team ID"

let build_json_roundtrip () =
  let team_id = "abcdefghij0123456789abcdef" in
  let json =
    Setup_mattermost.build_mattermost_json ~url:"https://mattermost.example.com"
      ~access_token:"test_token" ~team_id ~channel_ids:[ "ch1"; "ch2" ]
      ~allow_users:[ "*" ]
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.mattermost with
  | Some mm ->
      Alcotest.(check string) "url" "https://mattermost.example.com" mm.url;
      Alcotest.(check string) "access_token" "test_token" mm.access_token;
      Alcotest.(check string) "team_id" team_id mm.team_id;
      Alcotest.(check (list string))
        "channel_ids" [ "ch1"; "ch2" ] mm.channel_ids;
      Alcotest.(check (list string)) "allow_users" [ "*" ] mm.allow_users
  | None -> Alcotest.fail "expected mattermost config"

let build_json_restricted_users () =
  let team_id = "abcdefghij0123456789abcdef" in
  let json =
    Setup_mattermost.build_mattermost_json ~url:"https://mm.example.com"
      ~access_token:"tok" ~team_id ~channel_ids:[]
      ~allow_users:[ "uid1"; "uid2" ]
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.mattermost with
  | Some mm ->
      Alcotest.(check (list string))
        "allow_users" [ "uid1"; "uid2" ] mm.allow_users
  | None -> Alcotest.fail "expected mattermost config"

let post_instructions_content () =
  let s = Setup_mattermost.post_setup_instructions in
  Alcotest.(check bool)
    "has docs url" true
    (Test_helpers.string_contains s "https://clawq.org/channels/#mattermost");
  Alcotest.(check bool)
    "has personal access token mention" true
    (Test_helpers.string_contains s "Personal Access Tokens")

let suite =
  [
    Alcotest.test_case "validate_url https" `Quick validate_url_valid_https;
    Alcotest.test_case "validate_url http" `Quick validate_url_valid_http;
    Alcotest.test_case "validate_url empty" `Quick validate_url_empty;
    Alcotest.test_case "validate_url no scheme" `Quick validate_url_no_scheme;
    Alcotest.test_case "validate_url ftp scheme" `Quick validate_url_ftp_scheme;
    Alcotest.test_case "validate_team_id valid" `Quick validate_team_id_valid;
    Alcotest.test_case "validate_team_id too short" `Quick
      validate_team_id_too_short;
    Alcotest.test_case "validate_team_id too long" `Quick
      validate_team_id_too_long;
    Alcotest.test_case "validate_team_id uppercase" `Quick
      validate_team_id_uppercase;
    Alcotest.test_case "validate_team_id empty" `Quick validate_team_id_empty;
    Alcotest.test_case "build_json roundtrip" `Quick build_json_roundtrip;
    Alcotest.test_case "build_json restricted users" `Quick
      build_json_restricted_users;
    Alcotest.test_case "post_instructions content" `Quick
      post_instructions_content;
  ]
