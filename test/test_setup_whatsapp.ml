(* test_setup_whatsapp.ml — Unit tests for Setup_whatsapp pure functions *)

let validate_phone_number_id_valid () =
  Alcotest.(check (result string string))
    "valid digits" (Ok "123456789012345")
    (Setup_whatsapp.validate_phone_number_id "123456789012345")

let validate_phone_number_id_trimmed () =
  Alcotest.(check (result string string))
    "trimmed" (Ok "12345")
    (Setup_whatsapp.validate_phone_number_id "  12345  ")

let validate_phone_number_id_empty () =
  match Setup_whatsapp.validate_phone_number_id "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty"

let validate_phone_number_id_non_digits () =
  match Setup_whatsapp.validate_phone_number_id "123abc" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-digits"

let validate_access_token_valid () =
  Alcotest.(check (result string string))
    "valid" (Ok "EAABc1234token")
    (Setup_whatsapp.validate_access_token "EAABc1234token")

let validate_access_token_empty () =
  match Setup_whatsapp.validate_access_token "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty"

let validate_verify_token_valid () =
  Alcotest.(check (result string string))
    "valid" (Ok "my_verify_secret")
    (Setup_whatsapp.validate_verify_token "my_verify_secret")

let validate_verify_token_empty () =
  match Setup_whatsapp.validate_verify_token "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty"

let build_json_roundtrip () =
  let json =
    Setup_whatsapp.build_whatsapp_json ~phone_number_id:"123456789"
      ~access_token:"test_token" ~verify_token:"test_verify" ~allow_from:[ "*" ]
      ~default_model:None
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.whatsapp with
  | Some wa ->
      Alcotest.(check string) "phone_number_id" "123456789" wa.phone_number_id;
      Alcotest.(check string) "access_token" "test_token" wa.access_token;
      Alcotest.(check string) "verify_token" "test_verify" wa.verify_token;
      Alcotest.(check (list string)) "allow_from" [ "*" ] wa.allow_from;
      Alcotest.(check (option string)) "default_model" None wa.default_model
  | None -> Alcotest.fail "expected whatsapp config"

let build_json_restricted_numbers () =
  let json =
    Setup_whatsapp.build_whatsapp_json ~phone_number_id:"111"
      ~access_token:"tok" ~verify_token:"ver"
      ~allow_from:[ "+15551234567"; "+15559876543" ]
      ~default_model:(Some "openai:gpt-4")
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.whatsapp with
  | Some wa ->
      Alcotest.(check (list string))
        "allow_from"
        [ "+15551234567"; "+15559876543" ]
        wa.allow_from;
      Alcotest.(check (option string))
        "default_model" (Some "openai:gpt-4") wa.default_model
  | None -> Alcotest.fail "expected whatsapp config"

let post_instructions_content () =
  let s = Setup_whatsapp.post_setup_instructions in
  let contains sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has docs url" true
    (contains "https://clawq.org/channels/#whatsapp");
  Alcotest.(check bool)
    "has meta developers" true
    (contains "developers.facebook.com")

let suite =
  [
    Alcotest.test_case "validate_phone_number_id valid" `Quick
      validate_phone_number_id_valid;
    Alcotest.test_case "validate_phone_number_id trimmed" `Quick
      validate_phone_number_id_trimmed;
    Alcotest.test_case "validate_phone_number_id empty" `Quick
      validate_phone_number_id_empty;
    Alcotest.test_case "validate_phone_number_id non-digits" `Quick
      validate_phone_number_id_non_digits;
    Alcotest.test_case "validate_access_token valid" `Quick
      validate_access_token_valid;
    Alcotest.test_case "validate_access_token empty" `Quick
      validate_access_token_empty;
    Alcotest.test_case "validate_verify_token valid" `Quick
      validate_verify_token_valid;
    Alcotest.test_case "validate_verify_token empty" `Quick
      validate_verify_token_empty;
    Alcotest.test_case "build_json roundtrip" `Quick build_json_roundtrip;
    Alcotest.test_case "build_json restricted numbers" `Quick
      build_json_restricted_numbers;
    Alcotest.test_case "post_instructions content" `Quick
      post_instructions_content;
  ]
