(* test_setup_nostr.ml — Unit tests for Setup_nostr pure functions *)

let validate_relay_wss () =
  Alcotest.(check (result string string))
    "wss relay valid" (Ok "wss://relay.damus.io")
    (Setup_nostr.validate_relay "wss://relay.damus.io")

let validate_relay_ws () =
  Alcotest.(check (result string string))
    "ws relay valid" (Ok "ws://localhost:7777")
    (Setup_nostr.validate_relay "ws://localhost:7777")

let validate_relay_empty () =
  match Setup_nostr.validate_relay "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty relay"

let validate_relay_https () =
  match Setup_nostr.validate_relay "https://relay.example.com" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for https relay (must be ws/wss)"

let validate_relay_no_scheme () =
  match Setup_nostr.validate_relay "relay.damus.io" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for relay without scheme"

let validate_non_empty_valid () =
  Alcotest.(check (result string string))
    "non-empty valid" (Ok "somevalue")
    (Setup_nostr.validate_non_empty "somevalue")

let validate_non_empty_empty () =
  match Setup_nostr.validate_non_empty "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty value"

let validate_non_empty_whitespace () =
  match Setup_nostr.validate_non_empty "   " with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for whitespace-only value"

let validate_private_key_valid () =
  Alcotest.(check (result string string))
    "nsec valid" (Ok "nsec1testkey")
    (Setup_nostr.validate_private_key "nsec1testkey")

let validate_private_key_empty () =
  match Setup_nostr.validate_private_key "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty private key"

let validate_pubkey_valid () =
  Alcotest.(check (result string string))
    "npub valid" (Ok "npub1testkey")
    (Setup_nostr.validate_pubkey "npub1testkey")

let validate_pubkey_empty () =
  match Setup_nostr.validate_pubkey "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty pubkey"

let validate_relays_list_valid () =
  Alcotest.(check (result string string))
    "valid relays list" (Ok "wss://relay.damus.io,wss://nos.lol")
    (Setup_nostr.validate_relays_list "wss://relay.damus.io,wss://nos.lol")

let validate_relays_list_empty () =
  match Setup_nostr.validate_relays_list "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty relays"

let validate_relays_list_invalid_scheme () =
  match Setup_nostr.validate_relays_list "https://relay.example.com" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for https scheme in relay"

let build_json_roundtrip () =
  let json =
    Setup_nostr.build_nostr_json
      ~relays:[ "wss://relay.damus.io"; "wss://nos.lol" ]
      ~private_key:"nsec1test_private_key" ~pubkey:"npub1test_public_key"
      ~nak_path:"nak" ~allow_from:[ "*" ]
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.nostr with
  | Some n ->
      Alcotest.(check (list string))
        "relays"
        [ "wss://relay.damus.io"; "wss://nos.lol" ]
        n.relays;
      Alcotest.(check string)
        "private_key" "nsec1test_private_key" n.private_key;
      Alcotest.(check string) "pubkey" "npub1test_public_key" n.pubkey;
      Alcotest.(check string) "nak_path" "nak" n.nak_path;
      Alcotest.(check (list string)) "allow_from" [ "*" ] n.allow_from
  | None -> Alcotest.fail "expected nostr config"

let build_json_custom_nak_path () =
  let json =
    Setup_nostr.build_nostr_json
      ~relays:[ "wss://relay.nostr.band" ]
      ~private_key:"hexkey123" ~pubkey:"hexpub456"
      ~nak_path:"/usr/local/bin/nak"
      ~allow_from:[ "npub1alice"; "npub1bob" ]
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.nostr with
  | Some n ->
      Alcotest.(check string) "nak_path" "/usr/local/bin/nak" n.nak_path;
      Alcotest.(check (list string))
        "allow_from"
        [ "npub1alice"; "npub1bob" ]
        n.allow_from
  | None -> Alcotest.fail "expected nostr config"

let instructions_content () =
  let s = Setup_nostr.post_setup_instructions in
  let contains sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has docs URL" true
    (contains "https://clawq.org/channels/#nostr");
  Alcotest.(check bool) "has nak mention" true (contains "nak");
  Alcotest.(check bool) "has daemon start" true (contains "clawq daemon start")

let suite =
  [
    Alcotest.test_case "validate_relay wss" `Quick validate_relay_wss;
    Alcotest.test_case "validate_relay ws" `Quick validate_relay_ws;
    Alcotest.test_case "validate_relay empty" `Quick validate_relay_empty;
    Alcotest.test_case "validate_relay https" `Quick validate_relay_https;
    Alcotest.test_case "validate_relay no_scheme" `Quick
      validate_relay_no_scheme;
    Alcotest.test_case "validate_non_empty valid" `Quick
      validate_non_empty_valid;
    Alcotest.test_case "validate_non_empty empty" `Quick
      validate_non_empty_empty;
    Alcotest.test_case "validate_non_empty whitespace" `Quick
      validate_non_empty_whitespace;
    Alcotest.test_case "validate_private_key valid" `Quick
      validate_private_key_valid;
    Alcotest.test_case "validate_private_key empty" `Quick
      validate_private_key_empty;
    Alcotest.test_case "validate_pubkey valid" `Quick validate_pubkey_valid;
    Alcotest.test_case "validate_pubkey empty" `Quick validate_pubkey_empty;
    Alcotest.test_case "validate_relays_list valid" `Quick
      validate_relays_list_valid;
    Alcotest.test_case "validate_relays_list empty" `Quick
      validate_relays_list_empty;
    Alcotest.test_case "validate_relays_list invalid_scheme" `Quick
      validate_relays_list_invalid_scheme;
    Alcotest.test_case "build_json roundtrip" `Quick build_json_roundtrip;
    Alcotest.test_case "build_json custom_nak_path" `Quick
      build_json_custom_nak_path;
    Alcotest.test_case "post_setup_instructions content" `Quick
      instructions_content;
  ]
