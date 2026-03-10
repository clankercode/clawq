(* test_setup_telegram.ml — Unit tests for Setup_telegram pure functions *)

let validate_token_valid () =
  Alcotest.(check (result string string))
    "valid" (Ok "123456:ABC-DEF")
    (Setup_telegram.validate_bot_token "123456:ABC-DEF")

let validate_token_spaces () =
  Alcotest.(check (result string string))
    "spaces trimmed" (Ok "123456:ABC-DEF")
    (Setup_telegram.validate_bot_token "  123456:ABC-DEF  ")

let validate_token_no_colon () =
  match Setup_telegram.validate_bot_token "nocolon" with
  | Error e ->
      Alcotest.(check bool)
        "mentions colon" true
        (try
           ignore (Str.search_forward (Str.regexp_string "colon") e 0);
           true
         with Not_found -> false)
  | Ok _ -> Alcotest.fail "expected error for no colon"

let validate_token_empty () =
  match Setup_telegram.validate_bot_token "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty token"

let validate_token_non_numeric_prefix () =
  match Setup_telegram.validate_bot_token "abc:DEF123" with
  | Error e ->
      Alcotest.(check bool)
        "mentions numeric" true
        (try
           ignore (Str.search_forward (Str.regexp_string "numeric") e 0);
           true
         with Not_found -> false)
  | Ok _ -> Alcotest.fail "expected error for non-numeric prefix"

let validate_token_empty_after_colon () =
  match Setup_telegram.validate_bot_token "123456:" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty suffix"

let build_json_basic () =
  let json =
    Setup_telegram.build_telegram_json ~name:"default"
      ~bot_token:"123456:ABC-DEF" ~allow_from:[ "*" ]
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.telegram with
  | Some tg ->
      Alcotest.(check int) "accounts count" 1 (List.length tg.accounts);
      let name, acct = List.hd tg.accounts in
      Alcotest.(check string) "name" "default" name;
      Alcotest.(check string) "token" "123456:ABC-DEF" acct.bot_token;
      Alcotest.(check (list string)) "allow_from" [ "*" ] acct.allow_from
  | None -> Alcotest.fail "expected telegram config"

let build_json_specific_users () =
  let json =
    Setup_telegram.build_telegram_json ~name:"mybot" ~bot_token:"999:XYZ"
      ~allow_from:[ "alice"; "bob" ]
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.telegram with
  | Some tg ->
      let name, acct = List.hd tg.accounts in
      Alcotest.(check string) "name" "mybot" name;
      Alcotest.(check (list string))
        "allow_from" [ "alice"; "bob" ] acct.allow_from
  | None -> Alcotest.fail "expected telegram config"

let build_full_multi_accounts () =
  let accounts : (string * Runtime_config.telegram_account) list =
    [
      ("default", { bot_token = "111:AAA"; allow_from = [ "*" ]; totp = None });
      ( "work",
        {
          bot_token = "222:BBB";
          allow_from = [ "boss"; "colleague" ];
          totp = None;
        } );
    ]
  in
  let json =
    Setup_telegram.build_full_telegram_json ~accounts ~text_coalesce_ms:500
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.telegram with
  | Some tg ->
      Alcotest.(check int) "accounts count" 2 (List.length tg.accounts);
      Alcotest.(check int) "coalesce" 500 tg.text_coalesce_ms;
      let n1, a1 = List.nth tg.accounts 0 in
      let n2, a2 = List.nth tg.accounts 1 in
      Alcotest.(check string) "a1 name" "default" n1;
      Alcotest.(check string) "a1 token" "111:AAA" a1.bot_token;
      Alcotest.(check string) "a2 name" "work" n2;
      Alcotest.(check (list string))
        "a2 allow" [ "boss"; "colleague" ] a2.allow_from
  | None -> Alcotest.fail "expected telegram config"

let build_full_with_totp () =
  let accounts : (string * Runtime_config.telegram_account) list =
    [
      ( "secure",
        {
          bot_token = "333:CCC";
          allow_from = [ "admin" ];
          totp =
            Some
              {
                totp_enabled = true;
                totp_secret = "JBSWY3DPEHPK3PXP";
                session_ttl_hours = 12;
              };
        } );
    ]
  in
  let json =
    Setup_telegram.build_full_telegram_json ~accounts ~text_coalesce_ms:150
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.telegram with
  | Some tg -> (
      let _, acct = List.hd tg.accounts in
      match acct.totp with
      | Some t ->
          Alcotest.(check bool) "totp enabled" true t.totp_enabled;
          Alcotest.(check string) "totp secret" "JBSWY3DPEHPK3PXP" t.totp_secret;
          Alcotest.(check int) "session_ttl" 12 t.session_ttl_hours
      | None -> Alcotest.fail "expected totp config")
  | None -> Alcotest.fail "expected telegram config"

let instructions_content () =
  let s = Setup_telegram.post_setup_instructions ~account_name:"default" in
  let contains sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool) "mentions BotFather" true (contains "BotFather");
  Alcotest.(check bool) "mentions /newbot" true (contains "/newbot");
  Alcotest.(check bool) "mentions daemon start" true (contains "daemon start");
  Alcotest.(check bool) "mentions account name" true (contains "default")

let deep_merge_preserves_existing () =
  let existing =
    Yojson.Safe.from_string
      {|{"channels":{"cli":true,"github":{"auth":{"type":"pat","token":"ghp_x"},"repos":[]}},"default_temperature":0.7}|}
  in
  let overlay =
    Setup_telegram.build_telegram_json ~name:"default" ~bot_token:"123:ABC"
      ~allow_from:[ "*" ]
  in
  let result = Setup_common.deep_merge_json existing overlay in
  let config = Config_loader.parse_config ~resolve_secrets:false result in
  (* Telegram should be present *)
  (match config.channels.telegram with
  | Some _ -> ()
  | None -> Alcotest.fail "expected telegram config after merge");
  (* GitHub should be preserved *)
  match config.channels.github with
  | Some _ -> ()
  | None -> Alcotest.fail "github should be preserved after merge"

let suite =
  [
    Alcotest.test_case "validate_bot_token valid" `Quick validate_token_valid;
    Alcotest.test_case "validate_bot_token spaces" `Quick validate_token_spaces;
    Alcotest.test_case "validate_bot_token no colon" `Quick
      validate_token_no_colon;
    Alcotest.test_case "validate_bot_token empty" `Quick validate_token_empty;
    Alcotest.test_case "validate_bot_token non-numeric prefix" `Quick
      validate_token_non_numeric_prefix;
    Alcotest.test_case "validate_bot_token empty after colon" `Quick
      validate_token_empty_after_colon;
    Alcotest.test_case "build_telegram_json basic roundtrip" `Quick
      build_json_basic;
    Alcotest.test_case "build_telegram_json specific users" `Quick
      build_json_specific_users;
    Alcotest.test_case "build_full multi accounts roundtrip" `Quick
      build_full_multi_accounts;
    Alcotest.test_case "build_full with totp" `Quick build_full_with_totp;
    Alcotest.test_case "post_setup_instructions content" `Quick
      instructions_content;
    Alcotest.test_case "deep merge preserves existing" `Quick
      deep_merge_preserves_existing;
  ]
