(* Tests for IRC channel module *)

(* --- chunk_text tests --- *)

let test_chunk_short () =
  let chunks = Irc.chunk_text "hello" in
  Alcotest.(check int) "1 chunk" 1 (List.length chunks);
  Alcotest.(check string) "content" "hello" (List.hd chunks)

let test_chunk_over_limit () =
  let text = String.make 1000 'x' in
  let chunks = Irc.chunk_text ~max_bytes:450 text in
  Alcotest.(check bool) "multiple chunks" true (List.length chunks >= 2)

let test_chunk_preserves_content () =
  let text = String.make 2000 'z' in
  let chunks = Irc.chunk_text text in
  let reconstructed = String.concat "" chunks in
  Alcotest.(check string) "content preserved" text reconstructed

(* --- parse_irc_line tests --- *)

let test_parse_ping () =
  match Irc.parse_irc_line "PING :server.example.com" with
  | Some msg ->
      Alcotest.(check string) "command" "PING" msg.command;
      Alcotest.(check (option string))
        "trailing" (Some "server.example.com") msg.trailing
  | None -> Alcotest.fail "expected Some"

let test_parse_privmsg () =
  match Irc.parse_irc_line ":nick!user@host PRIVMSG #channel :hello there" with
  | Some msg ->
      Alcotest.(check (option string))
        "prefix" (Some "nick!user@host") msg.prefix;
      Alcotest.(check string) "command" "PRIVMSG" msg.command;
      Alcotest.(check (option string))
        "trailing" (Some "hello there") msg.trailing
  | None -> Alcotest.fail "expected Some"

let test_parse_numeric () =
  match Irc.parse_irc_line ":server 001 bot :Welcome" with
  | Some msg ->
      Alcotest.(check string) "command" "001" msg.command;
      Alcotest.(check (option string)) "prefix" (Some "server") msg.prefix
  | None -> Alcotest.fail "expected Some"

let test_parse_empty () =
  match Irc.parse_irc_line "" with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for empty"

let test_parse_whitespace_only () =
  match Irc.parse_irc_line "   " with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for whitespace"

let test_parse_no_prefix () =
  match Irc.parse_irc_line "CAP * ACK :sasl" with
  | Some msg ->
      Alcotest.(check string) "command" "CAP" msg.command;
      Alcotest.(check (option string)) "no prefix" None msg.prefix
  | None -> Alcotest.fail "expected Some"

let test_parse_nick_in_use () =
  match Irc.parse_irc_line ":server 433 * bot :Nickname is already in use" with
  | Some msg -> Alcotest.(check string) "command" "433" msg.command
  | None -> Alcotest.fail "expected Some"

(* --- nick_from_prefix tests --- *)

let test_nick_from_prefix_with_user () =
  Alcotest.(check string)
    "nick" "alice"
    (Irc.nick_from_prefix "alice!user@host")

let test_nick_from_prefix_bare () =
  Alcotest.(check string) "bare nick" "bob" (Irc.nick_from_prefix "bob")

let test_nick_from_prefix_no_host () =
  Alcotest.(check string)
    "no host" "charlie"
    (Irc.nick_from_prefix "charlie!user")

(* --- is_allowed tests --- *)

let mk_irc_cfg ?(allow_from = []) () : Runtime_config.irc_config =
  {
    host = "irc.example.com";
    port = 6697;
    nick = "bot";
    password = None;
    channels = [ "#test" ];
    allow_from;
    sasl = false;
    tls = true;
    default_model = None;
  }

let test_is_allowed_empty_list () =
  let cfg = mk_irc_cfg () in
  Alcotest.(check bool)
    "empty allows all" true
    (Irc.is_allowed ~cfg ~nick:"anyone")

let test_is_allowed_match () =
  let cfg = mk_irc_cfg ~allow_from:[ "alice"; "bob" ] () in
  Alcotest.(check bool) "match" true (Irc.is_allowed ~cfg ~nick:"alice")

let test_is_allowed_no_match () =
  let cfg = mk_irc_cfg ~allow_from:[ "alice" ] () in
  Alcotest.(check bool) "no match" false (Irc.is_allowed ~cfg ~nick:"eve")

(* --- sasl_plain_payload tests --- *)

let test_sasl_plain_payload () =
  let payload = Irc.sasl_plain_payload ~nick:"bot" ~password:"secret" in
  (* SASL PLAIN = base64(\0nick\0password) *)
  let decoded = Base64.decode_exn payload in
  Alcotest.(check string) "sasl payload" "\x00bot\x00secret" decoded

let test_sasl_plain_payload_empty_password () =
  let payload = Irc.sasl_plain_payload ~nick:"bot" ~password:"" in
  let decoded = Base64.decode_exn payload in
  Alcotest.(check string) "empty password" "\x00bot\x00" decoded

let suite =
  [
    Alcotest.test_case "chunk short" `Quick test_chunk_short;
    Alcotest.test_case "chunk over limit" `Quick test_chunk_over_limit;
    Alcotest.test_case "chunk preserves content" `Quick
      test_chunk_preserves_content;
    Alcotest.test_case "parse ping" `Quick test_parse_ping;
    Alcotest.test_case "parse privmsg" `Quick test_parse_privmsg;
    Alcotest.test_case "parse numeric" `Quick test_parse_numeric;
    Alcotest.test_case "parse empty" `Quick test_parse_empty;
    Alcotest.test_case "parse whitespace" `Quick test_parse_whitespace_only;
    Alcotest.test_case "parse no prefix" `Quick test_parse_no_prefix;
    Alcotest.test_case "parse nick in use" `Quick test_parse_nick_in_use;
    Alcotest.test_case "nick from prefix with user" `Quick
      test_nick_from_prefix_with_user;
    Alcotest.test_case "nick from prefix bare" `Quick test_nick_from_prefix_bare;
    Alcotest.test_case "nick from prefix no host" `Quick
      test_nick_from_prefix_no_host;
    Alcotest.test_case "is_allowed empty" `Quick test_is_allowed_empty_list;
    Alcotest.test_case "is_allowed match" `Quick test_is_allowed_match;
    Alcotest.test_case "is_allowed no match" `Quick test_is_allowed_no_match;
    Alcotest.test_case "sasl plain payload" `Quick test_sasl_plain_payload;
    Alcotest.test_case "sasl empty password" `Quick
      test_sasl_plain_payload_empty_password;
  ]
