(* test_setup_irc.ml — Unit tests for Setup_irc pure functions *)

let validate_port_valid () =
  Alcotest.(check (result string string))
    "valid port" (Ok "6697")
    (Setup_common.validate_port "6697")

let validate_port_min () =
  Alcotest.(check (result string string))
    "min port" (Ok "1")
    (Setup_common.validate_port "1")

let validate_port_max () =
  Alcotest.(check (result string string))
    "max port" (Ok "65535")
    (Setup_common.validate_port "65535")

let validate_port_zero () =
  match Setup_common.validate_port "0" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for port 0"

let validate_port_too_large () =
  match Setup_common.validate_port "65536" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for port > 65535"

let validate_port_not_int () =
  match Setup_common.validate_port "abc" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-integer port"

let validate_nick_valid () =
  Alcotest.(check (result string string))
    "valid nick" (Ok "clawqbot")
    (Setup_irc.validate_nick "clawqbot")

let validate_nick_empty () =
  match Setup_irc.validate_nick "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty nick"

let validate_nick_with_space () =
  match Setup_irc.validate_nick "my bot" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for nick with space"

let validate_nick_with_tab () =
  match Setup_irc.validate_nick "my\tbot" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for nick with tab"

let build_json_roundtrip () =
  let json =
    Setup_irc.build_irc_json ~host:"irc.libera.chat" ~port:6697 ~tls:true
      ~nick:"clawqbot" ~password:"" ~sasl:false ~channels:[ "#clawq" ]
      ~allow_from:[ "*" ]
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.irc with
  | Some c ->
      Alcotest.(check string) "host" "irc.libera.chat" c.host;
      Alcotest.(check int) "port" 6697 c.port;
      Alcotest.(check bool) "tls" true c.tls;
      Alcotest.(check string) "nick" "clawqbot" c.nick;
      Alcotest.(check bool) "sasl" false c.sasl;
      Alcotest.(check (list string)) "channels" [ "#clawq" ] c.channels;
      Alcotest.(check (list string)) "allow_from" [ "*" ] c.allow_from
  | None -> Alcotest.fail "expected irc config"

let build_json_with_password () =
  let json =
    Setup_irc.build_irc_json ~host:"irc.example.com" ~port:6667 ~tls:false
      ~nick:"mybot" ~password:"s3cr3t" ~sasl:true ~channels:[ "#foo"; "#bar" ]
      ~allow_from:[ "alice"; "bob" ]
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.irc with
  | Some c ->
      Alcotest.(check (option string)) "password" (Some "s3cr3t") c.password;
      Alcotest.(check bool) "sasl" true c.sasl;
      Alcotest.(check (list string)) "channels" [ "#foo"; "#bar" ] c.channels;
      Alcotest.(check (list string))
        "allow_from" [ "alice"; "bob" ] c.allow_from
  | None -> Alcotest.fail "expected irc config"

let build_json_no_password () =
  let json =
    Setup_irc.build_irc_json ~host:"irc.example.com" ~port:6697 ~tls:true
      ~nick:"bot" ~password:"" ~sasl:false ~channels:[] ~allow_from:[ "*" ]
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.irc with
  | Some c ->
      Alcotest.(check (option string)) "password is None" None c.password
  | None -> Alcotest.fail "expected irc config"

let instructions_content () =
  let s = Setup_irc.post_setup_instructions in
  Alcotest.(check bool)
    "has docs URL" true
    (Test_helpers.string_contains s "https://clawq.org/channels/#irc");
  Alcotest.(check bool)
    "has daemon start" true
    (Test_helpers.string_contains s "clawq daemon start")

let suite =
  [
    Alcotest.test_case "validate_port valid" `Quick validate_port_valid;
    Alcotest.test_case "validate_port min" `Quick validate_port_min;
    Alcotest.test_case "validate_port max" `Quick validate_port_max;
    Alcotest.test_case "validate_port zero" `Quick validate_port_zero;
    Alcotest.test_case "validate_port too_large" `Quick validate_port_too_large;
    Alcotest.test_case "validate_port not_int" `Quick validate_port_not_int;
    Alcotest.test_case "validate_nick valid" `Quick validate_nick_valid;
    Alcotest.test_case "validate_nick empty" `Quick validate_nick_empty;
    Alcotest.test_case "validate_nick with_space" `Quick
      validate_nick_with_space;
    Alcotest.test_case "validate_nick with_tab" `Quick validate_nick_with_tab;
    Alcotest.test_case "build_json roundtrip" `Quick build_json_roundtrip;
    Alcotest.test_case "build_json with_password" `Quick
      build_json_with_password;
    Alcotest.test_case "build_json no_password" `Quick build_json_no_password;
    Alcotest.test_case "post_setup_instructions content" `Quick
      instructions_content;
  ]
