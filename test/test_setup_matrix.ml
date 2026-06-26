(* test_setup_matrix.ml — Unit tests for Setup_matrix pure functions *)

let validate_homeserver_url_https () =
  Alcotest.(check (result string string))
    "https valid" (Ok "https://matrix.org")
    (Setup_matrix.validate_homeserver_url "https://matrix.org")

let validate_homeserver_url_http () =
  Alcotest.(check (result string string))
    "http valid" (Ok "http://localhost:8448")
    (Setup_matrix.validate_homeserver_url "http://localhost:8448")

let validate_homeserver_url_empty () =
  match Setup_matrix.validate_homeserver_url "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty URL"

let validate_homeserver_url_no_scheme () =
  match Setup_matrix.validate_homeserver_url "matrix.org" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for URL without scheme"

let validate_homeserver_url_trims () =
  Alcotest.(check (result string string))
    "trims whitespace" (Ok "https://matrix.org")
    (Setup_matrix.validate_homeserver_url "  https://matrix.org  ")

let validate_user_id_valid () =
  Alcotest.(check (result string string))
    "valid user ID" (Ok "@bot:matrix.org")
    (Setup_matrix.validate_user_id "@bot:matrix.org")

let validate_user_id_empty () =
  match Setup_matrix.validate_user_id "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty user ID"

let validate_user_id_no_at () =
  match Setup_matrix.validate_user_id "bot:matrix.org" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for user ID without @"

let validate_user_id_no_colon () =
  match Setup_matrix.validate_user_id "@botmatrix.org" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for user ID without :"

let validate_user_id_no_domain () =
  match Setup_matrix.validate_user_id "@bot:" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for user ID without domain"

let validate_user_id_no_local () =
  match Setup_matrix.validate_user_id "@:matrix.org" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for user ID without local part"

let validate_access_token_valid () =
  Alcotest.(check (result string string))
    "valid token" (Ok "syt_test_abc123")
    (Setup_matrix.validate_access_token "syt_test_abc123")

let validate_access_token_empty () =
  match Setup_matrix.validate_access_token "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty access token"

let build_json_roundtrip () =
  let json =
    Setup_matrix.build_matrix_json ~homeserver_url:"https://matrix.org"
      ~access_token:"syt_test_token" ~user_id:"@bot:matrix.org"
      ~allow_rooms:[ "*" ] ~allow_users:[ "*" ]
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.matrix with
  | Some m ->
      Alcotest.(check string)
        "homeserver_url" "https://matrix.org" m.homeserver_url;
      Alcotest.(check string) "access_token" "syt_test_token" m.access_token;
      Alcotest.(check string) "user_id" "@bot:matrix.org" m.user_id;
      Alcotest.(check (list string)) "allow_rooms" [ "*" ] m.allow_rooms;
      Alcotest.(check (list string)) "allow_users" [ "*" ] m.allow_users
  | None -> Alcotest.fail "expected matrix config"

let build_json_specific_rooms () =
  let json =
    Setup_matrix.build_matrix_json ~homeserver_url:"https://example.org"
      ~access_token:"tok" ~user_id:"@x:example.org"
      ~allow_rooms:[ "!roomA:example.org"; "!roomB:example.org" ]
      ~allow_users:[ "@alice:example.org" ]
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.matrix with
  | Some m ->
      Alcotest.(check (list string))
        "allow_rooms"
        [ "!roomA:example.org"; "!roomB:example.org" ]
        m.allow_rooms;
      Alcotest.(check (list string))
        "allow_users" [ "@alice:example.org" ] m.allow_users
  | None -> Alcotest.fail "expected matrix config"

let instructions_content () =
  let s = Setup_matrix.post_setup_instructions in
  Alcotest.(check bool)
    "has access token mention" true (Test_helpers.string_contains s "access token");
  Alcotest.(check bool)
    "has docs URL" true
    (Test_helpers.string_contains s "https://clawq.org/channels/#matrix")

let suite =
  [
    Alcotest.test_case "validate_homeserver_url https" `Quick
      validate_homeserver_url_https;
    Alcotest.test_case "validate_homeserver_url http" `Quick
      validate_homeserver_url_http;
    Alcotest.test_case "validate_homeserver_url empty" `Quick
      validate_homeserver_url_empty;
    Alcotest.test_case "validate_homeserver_url no_scheme" `Quick
      validate_homeserver_url_no_scheme;
    Alcotest.test_case "validate_homeserver_url trims" `Quick
      validate_homeserver_url_trims;
    Alcotest.test_case "validate_user_id valid" `Quick validate_user_id_valid;
    Alcotest.test_case "validate_user_id empty" `Quick validate_user_id_empty;
    Alcotest.test_case "validate_user_id no_at" `Quick validate_user_id_no_at;
    Alcotest.test_case "validate_user_id no_colon" `Quick
      validate_user_id_no_colon;
    Alcotest.test_case "validate_user_id no_domain" `Quick
      validate_user_id_no_domain;
    Alcotest.test_case "validate_user_id no_local" `Quick
      validate_user_id_no_local;
    Alcotest.test_case "validate_access_token valid" `Quick
      validate_access_token_valid;
    Alcotest.test_case "validate_access_token empty" `Quick
      validate_access_token_empty;
    Alcotest.test_case "build_json roundtrip" `Quick build_json_roundtrip;
    Alcotest.test_case "build_json specific rooms" `Quick
      build_json_specific_rooms;
    Alcotest.test_case "post_setup_instructions content" `Quick
      instructions_content;
  ]
