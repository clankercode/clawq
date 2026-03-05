let test_base32_roundtrip () =
  let input = "Hello, World!" in
  let encoded = Totp.base32_encode input in
  let decoded = Totp.base32_decode encoded in
  Alcotest.(check string) "base32 roundtrip" input decoded

let test_base32_known_vector () =
  (* RFC 4648 test vectors *)
  Alcotest.(check string) "empty" "" (Totp.base32_encode "");
  Alcotest.(check string) "f" "MY" (Totp.base32_encode "f");
  Alcotest.(check string) "fo" "MZXQ" (Totp.base32_encode "fo");
  Alcotest.(check string) "foo" "MZXW6" (Totp.base32_encode "foo");
  Alcotest.(check string) "foob" "MZXW6YQ" (Totp.base32_encode "foob");
  Alcotest.(check string) "fooba" "MZXW6YTB" (Totp.base32_encode "fooba");
  Alcotest.(check string) "foobar" "MZXW6YTBOI" (Totp.base32_encode "foobar")

let test_base32_decode_known () =
  Alcotest.(check string) "decode f" "f" (Totp.base32_decode "MY");
  Alcotest.(check string) "decode fo" "fo" (Totp.base32_decode "MZXQ");
  Alcotest.(check string) "decode foo" "foo" (Totp.base32_decode "MZXW6");
  Alcotest.(check string)
    "decode foobar" "foobar"
    (Totp.base32_decode "MZXW6YTBOI")

let test_generate_totp_format () =
  let secret = Totp.base32_encode "12345678901234567890" in
  let code = Totp.generate_totp ~secret ~time:0.0 in
  Alcotest.(check int) "TOTP is 6 digits" 6 (String.length code);
  String.iter
    (fun c -> Alcotest.(check bool) "digit char" true (c >= '0' && c <= '9'))
    code

let test_verify_totp_current () =
  let secret = Totp.base32_encode "12345678901234567890" in
  let time = 1000000000.0 in
  let code = Totp.generate_totp ~secret ~time in
  Alcotest.(check bool)
    "verify current" true
    (Totp.verify_totp ~secret ~code ~time)

let test_verify_totp_window () =
  let secret = Totp.base32_encode "12345678901234567890" in
  let time = 1000000000.0 in
  let code = Totp.generate_totp ~secret ~time in
  (* Should verify within +/- 30s window *)
  Alcotest.(check bool)
    "verify +29s" true
    (Totp.verify_totp ~secret ~code ~time:(time +. 29.0));
  Alcotest.(check bool)
    "verify -29s" true
    (Totp.verify_totp ~secret ~code ~time:(time -. 29.0))

let test_verify_totp_wrong_code () =
  let secret = Totp.base32_encode "12345678901234567890" in
  let time = 1000000000.0 in
  Alcotest.(check bool)
    "wrong code rejects" false
    (Totp.verify_totp ~secret ~code:"000000" ~time)

let test_verify_totp_expired () =
  let secret = Totp.base32_encode "12345678901234567890" in
  let time = 1000000000.0 in
  let code = Totp.generate_totp ~secret ~time in
  (* Should fail outside window (> 60s) *)
  Alcotest.(check bool)
    "verify +90s" false
    (Totp.verify_totp ~secret ~code ~time:(time +. 90.0))

let test_generate_secret () =
  let s1 = Totp.generate_secret () in
  let s2 = Totp.generate_secret () in
  Alcotest.(check bool) "secrets differ" true (s1 <> s2);
  Alcotest.(check bool) "secret non-empty" true (String.length s1 > 0);
  (* Should be base32-encoded (uppercase + 2-7) *)
  String.iter
    (fun c ->
      Alcotest.(check bool)
        "base32 char" true
        ((c >= 'A' && c <= 'Z') || (c >= '2' && c <= '7')))
    s1

let test_time_remaining () =
  let remaining = Totp.time_remaining ~time:0.0 in
  Alcotest.(check int) "full window" 30 remaining;
  let remaining2 = Totp.time_remaining ~time:15.0 in
  Alcotest.(check int) "half window" 15 remaining2;
  let remaining3 = Totp.time_remaining ~time:29.0 in
  Alcotest.(check int) "1 second left" 1 remaining3

let test_rfc6238_vector () =
  (* RFC 6238 test vector: SHA1, time=59, secret="12345678901234567890" *)
  let secret = Totp.base32_encode "12345678901234567890" in
  let code = Totp.generate_totp ~secret ~time:59.0 in
  (* At time=59, counter=1, expected TOTP: 287082 *)
  Alcotest.(check string) "RFC 6238 t=59" "287082" code

let suite =
  [
    Alcotest.test_case "base32 roundtrip" `Quick test_base32_roundtrip;
    Alcotest.test_case "base32 known vectors" `Quick test_base32_known_vector;
    Alcotest.test_case "base32 decode known" `Quick test_base32_decode_known;
    Alcotest.test_case "generate totp format" `Quick test_generate_totp_format;
    Alcotest.test_case "verify totp current" `Quick test_verify_totp_current;
    Alcotest.test_case "verify totp window" `Quick test_verify_totp_window;
    Alcotest.test_case "verify totp wrong code" `Quick
      test_verify_totp_wrong_code;
    Alcotest.test_case "verify totp expired" `Quick test_verify_totp_expired;
    Alcotest.test_case "generate secret" `Quick test_generate_secret;
    Alcotest.test_case "time remaining" `Quick test_time_remaining;
    Alcotest.test_case "RFC 6238 vector" `Quick test_rfc6238_vector;
  ]
