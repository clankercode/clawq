(* Tests for Nostr channel module *)

let mk_nostr_cfg ?(allow_from = [ "*" ]) () : Runtime_config.nostr_config =
  {
    private_key = "nsec1test";
    pubkey = "npub1test";
    relays = [ "wss://relay.example.com" ];
    allow_from;
    nak_path = "nak";
    default_model = None;
  }

(* --- is_allowed tests --- *)

let test_is_allowed_wildcard () =
  let config = mk_nostr_cfg () in
  Alcotest.(check bool)
    "wildcard allows all" true
    (Nostr.is_allowed ~config ~pubkey:"abc123")

let test_is_allowed_match () =
  let config = mk_nostr_cfg ~allow_from:[ "pub1"; "pub2" ] () in
  Alcotest.(check bool) "match" true (Nostr.is_allowed ~config ~pubkey:"pub1")

let test_is_allowed_no_match () =
  let config = mk_nostr_cfg ~allow_from:[ "pub1" ] () in
  Alcotest.(check bool)
    "no match" false
    (Nostr.is_allowed ~config ~pubkey:"pub9")

(* --- parse_event_line tests --- *)

let test_parse_event_valid () =
  let line = {|{"id":"evt1","pubkey":"pub1","kind":4,"content":"hello"}|} in
  match Nostr.parse_event_line line with
  | Some (id, _sender, text, _protocol) ->
      Alcotest.(check string) "id" "evt1" id;
      Alcotest.(check string) "text" "hello" text
  | None -> Alcotest.fail "expected Some"

let test_parse_event_empty_content () =
  let line = {|{"id":"evt2","pubkey":"pub1","kind":4,"content":""}|} in
  match Nostr.parse_event_line line with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for empty content"

let test_parse_event_invalid () =
  match Nostr.parse_event_line "not json" with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None"

let test_parse_event_empty () =
  match Nostr.parse_event_line "" with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for empty"

let test_parse_event_nested_rumor () =
  (* NIP-17 gift-wrap (kind=1059): inner content is a JSON rumor with pubkey and content *)
  let inner =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("pubkey", `String "real-sender");
           ("id", `String "inner-id");
           ("content", `String "inner msg");
         ])
  in
  let line =
    Printf.sprintf
      {|{"id":"evt3","kind":1059,"pubkey":"ephemeral","content":%s}|}
      (Yojson.Safe.to_string (`String inner))
  in
  match Nostr.parse_event_line line with
  | Some (id, sender, text, _protocol) ->
      Alcotest.(check string) "id" "inner-id" id;
      Alcotest.(check string) "sender" "real-sender" sender;
      Alcotest.(check string) "text" "inner msg" text
  | None -> Alcotest.fail "expected Some"

let test_parse_event_no_id () =
  (* "id" member missing -> member throws -> outer try catches -> None *)
  let line = {|{"pubkey":"pub1","content":"hello"}|} in
  match Nostr.parse_event_line line with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for missing id"

(* --- dedup_seen tests --- *)

let test_dedup_first () =
  let id = "nostr-dedup-" ^ string_of_float (Unix.gettimeofday ()) in
  Alcotest.(check bool) "first time" false (Nostr.dedup_seen id)

let test_dedup_second () =
  let id = "nostr-dedup2-" ^ string_of_float (Unix.gettimeofday ()) in
  ignore (Nostr.dedup_seen id);
  Alcotest.(check bool) "second time" true (Nostr.dedup_seen id)

let suite =
  [
    Alcotest.test_case "is_allowed wildcard" `Quick test_is_allowed_wildcard;
    Alcotest.test_case "is_allowed match" `Quick test_is_allowed_match;
    Alcotest.test_case "is_allowed no match" `Quick test_is_allowed_no_match;
    Alcotest.test_case "parse event valid" `Quick test_parse_event_valid;
    Alcotest.test_case "parse event empty content" `Quick
      test_parse_event_empty_content;
    Alcotest.test_case "parse event invalid" `Quick test_parse_event_invalid;
    Alcotest.test_case "parse event empty" `Quick test_parse_event_empty;
    Alcotest.test_case "parse event nested rumor" `Quick
      test_parse_event_nested_rumor;
    Alcotest.test_case "parse event no id" `Quick test_parse_event_no_id;
    Alcotest.test_case "dedup first" `Quick test_dedup_first;
    Alcotest.test_case "dedup second" `Quick test_dedup_second;
  ]
