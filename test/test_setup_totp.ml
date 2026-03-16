(* test_setup_totp.ml — Unit tests for Setup_totp pure functions *)

let validate_ttl_valid () =
  Alcotest.(check (result string string))
    "valid TTL" (Ok "24")
    (Setup_totp.validate_ttl "24")

let validate_ttl_one () =
  Alcotest.(check (result string string))
    "TTL of 1 ok" (Ok "1")
    (Setup_totp.validate_ttl "1")

let validate_ttl_zero () =
  match Setup_totp.validate_ttl "0" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for zero TTL"

let validate_ttl_negative () =
  match Setup_totp.validate_ttl "-1" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for negative TTL"

let validate_ttl_non_number () =
  match Setup_totp.validate_ttl "abc" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-number TTL"

let validate_secret_valid () =
  Alcotest.(check (result string string))
    "valid secret" (Ok "JBSWY3DPEHPK3PXP")
    (Setup_totp.validate_secret "JBSWY3DPEHPK3PXP")

let validate_secret_empty () =
  match Setup_totp.validate_secret "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty secret"

let validate_secret_whitespace () =
  match Setup_totp.validate_secret "   " with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for whitespace secret"

let build_json_roundtrip () =
  (* TOTP lives in telegram account config, not a top-level section.
     We verify build_totp_json produces correct JSON structure by parsing
     just the totp sub-object directly using Yojson. *)
  let json =
    Setup_totp.build_totp_json ~totp_enabled:true
      ~totp_secret:"JBSWY3DPEHPK3PXP" ~session_ttl_hours:24
  in
  match json with
  | `Assoc fields -> (
      match List.assoc_opt "totp" fields with
      | Some (`Assoc totp_fields) -> (
          (match List.assoc_opt "enabled" totp_fields with
          | Some (`Bool true) -> ()
          | _ -> Alcotest.fail "expected enabled=true");
          (match List.assoc_opt "secret" totp_fields with
          | Some (`String s) ->
              Alcotest.(check string) "totp_secret" "JBSWY3DPEHPK3PXP" s
          | _ -> Alcotest.fail "expected secret string");
          match List.assoc_opt "session_ttl_hours" totp_fields with
          | Some (`Int 24) -> ()
          | _ -> Alcotest.fail "expected session_ttl_hours=24")
      | _ -> Alcotest.fail "expected totp object")
  | _ -> Alcotest.fail "expected top-level assoc"

let build_json_disabled () =
  let json =
    Setup_totp.build_totp_json ~totp_enabled:false ~totp_secret:""
      ~session_ttl_hours:24
  in
  match json with
  | `Assoc fields -> (
      match List.assoc_opt "totp" fields with
      | Some (`Assoc totp_fields) -> (
          match List.assoc_opt "enabled" totp_fields with
          | Some (`Bool false) -> ()
          | _ -> Alcotest.fail "expected enabled=false")
      | _ -> Alcotest.fail "expected totp object")
  | _ -> Alcotest.fail "expected top-level assoc"

let build_json_custom_ttl () =
  let json =
    Setup_totp.build_totp_json ~totp_enabled:true ~totp_secret:"MYSECRET"
      ~session_ttl_hours:48
  in
  match json with
  | `Assoc fields -> (
      match List.assoc_opt "totp" fields with
      | Some (`Assoc totp_fields) -> (
          match List.assoc_opt "session_ttl_hours" totp_fields with
          | Some (`Int 48) -> ()
          | _ -> Alcotest.fail "expected session_ttl_hours=48")
      | _ -> Alcotest.fail "expected totp object")
  | _ -> Alcotest.fail "expected top-level assoc"

let generate_totp_secret_non_empty () =
  let secret = Setup_totp.generate_totp_secret () in
  Alcotest.(check bool) "non-empty" true (String.length secret > 0)

let generate_totp_secret_base32_chars () =
  let secret = Setup_totp.generate_totp_secret () in
  let valid =
    String.for_all
      (fun c -> (c >= 'A' && c <= 'Z') || (c >= '2' && c <= '7'))
      secret
  in
  Alcotest.(check bool) "only base32 chars" true valid

let post_instructions_content () =
  let s = Setup_totp.post_setup_instructions in
  let contains sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has docs url" true
    (contains "https://clawq.org/security/#totp");
  Alcotest.(check bool) "mentions authenticator" true (contains "authenticator");
  Alcotest.(check bool) "mentions session_ttl" true (contains "session_ttl")

let suite =
  [
    Alcotest.test_case "validate_ttl valid" `Quick validate_ttl_valid;
    Alcotest.test_case "validate_ttl one" `Quick validate_ttl_one;
    Alcotest.test_case "validate_ttl zero" `Quick validate_ttl_zero;
    Alcotest.test_case "validate_ttl negative" `Quick validate_ttl_negative;
    Alcotest.test_case "validate_ttl non-number" `Quick validate_ttl_non_number;
    Alcotest.test_case "validate_secret valid" `Quick validate_secret_valid;
    Alcotest.test_case "validate_secret empty" `Quick validate_secret_empty;
    Alcotest.test_case "validate_secret whitespace" `Quick
      validate_secret_whitespace;
    Alcotest.test_case "build_json roundtrip" `Quick build_json_roundtrip;
    Alcotest.test_case "build_json disabled" `Quick build_json_disabled;
    Alcotest.test_case "build_json custom TTL" `Quick build_json_custom_ttl;
    Alcotest.test_case "generate_totp_secret non-empty" `Quick
      generate_totp_secret_non_empty;
    Alcotest.test_case "generate_totp_secret base32 chars" `Quick
      generate_totp_secret_base32_chars;
    Alcotest.test_case "post_instructions content" `Quick
      post_instructions_content;
  ]
