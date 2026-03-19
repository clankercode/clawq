(* Tests for Matrix channel module *)

(* --- chunk_text tests --- *)

let test_chunk_short () =
  let chunks = Matrix.chunk_text "hello" in
  Alcotest.(check int) "1 chunk" 1 (List.length chunks);
  Alcotest.(check string) "content" "hello" (List.hd chunks)

let test_chunk_empty () =
  let chunks = Matrix.chunk_text "" in
  Alcotest.(check int) "1 chunk for empty" 1 (List.length chunks)

let test_chunk_over_limit () =
  let text = String.make 8001 'x' in
  let chunks = Matrix.chunk_text ~max_bytes:4000 text in
  Alcotest.(check bool) "multiple chunks" true (List.length chunks >= 2)

let test_chunk_preserves_content () =
  let text = String.make 12000 'z' in
  let chunks = Matrix.chunk_text text in
  let reconstructed = String.concat "" chunks in
  Alcotest.(check string) "content preserved" text reconstructed

(* --- is_room_allowed tests --- *)

let mk_matrix_cfg ?(allow_rooms = []) ?(allow_users = []) () :
    Runtime_config.matrix_config =
  {
    homeserver_url = "https://matrix.org";
    access_token = "tok";
    user_id = "@bot:matrix.org";
    allow_rooms;
    allow_users;
    default_model = None;
  }

let test_room_allowed_empty () =
  let cfg = mk_matrix_cfg () in
  Alcotest.(check bool)
    "empty list allows all" true
    (Matrix.is_room_allowed ~cfg ~room_id:"!any:room")

let test_room_allowed_match () =
  let cfg = mk_matrix_cfg ~allow_rooms:[ "!room1:m.org"; "!room2:m.org" ] () in
  Alcotest.(check bool)
    "match" true
    (Matrix.is_room_allowed ~cfg ~room_id:"!room1:m.org")

let test_room_allowed_no_match () =
  let cfg = mk_matrix_cfg ~allow_rooms:[ "!room1:m.org" ] () in
  Alcotest.(check bool)
    "no match" false
    (Matrix.is_room_allowed ~cfg ~room_id:"!other:m.org")

(* --- is_user_allowed tests --- *)

let test_user_allowed_empty () =
  let cfg = mk_matrix_cfg () in
  Alcotest.(check bool)
    "empty allows all" true
    (Matrix.is_user_allowed ~cfg ~user_id:"@any:m.org")

let test_user_allowed_match () =
  let cfg = mk_matrix_cfg ~allow_users:[ "@alice:m.org" ] () in
  Alcotest.(check bool)
    "match" true
    (Matrix.is_user_allowed ~cfg ~user_id:"@alice:m.org")

let test_user_allowed_no_match () =
  let cfg = mk_matrix_cfg ~allow_users:[ "@alice:m.org" ] () in
  Alcotest.(check bool)
    "no match" false
    (Matrix.is_user_allowed ~cfg ~user_id:"@bob:m.org")

(* --- parse_sync_events tests --- *)

let test_parse_sync_empty () =
  let cfg = mk_matrix_cfg () in
  let events = Matrix.parse_sync_events `Null ~cfg in
  Alcotest.(check int) "no events from null" 0 (List.length events)

let test_parse_sync_no_rooms () =
  let cfg = mk_matrix_cfg () in
  let json = Yojson.Safe.from_string {|{"rooms":{"join":{}}}|} in
  let events = Matrix.parse_sync_events json ~cfg in
  Alcotest.(check int) "no events" 0 (List.length events)

let test_parse_sync_with_message () =
  let cfg = mk_matrix_cfg () in
  let json =
    Yojson.Safe.from_string
      {|{"rooms":{"join":{"!room:m.org":{"timeline":{"events":[
        {"type":"m.room.message","sender":"@alice:m.org","content":{"msgtype":"m.text","body":"hello"}}
      ]}}}}}|}
  in
  let events = Matrix.parse_sync_events json ~cfg in
  Alcotest.(check int) "1 event" 1 (List.length events);
  let room_id, sender, body = List.hd events in
  Alcotest.(check string) "room_id" "!room:m.org" room_id;
  Alcotest.(check string) "sender" "@alice:m.org" sender;
  Alcotest.(check string) "body" "hello" body

let test_parse_sync_skips_own_messages () =
  let cfg = mk_matrix_cfg () in
  let json =
    Yojson.Safe.from_string
      {|{"rooms":{"join":{"!room:m.org":{"timeline":{"events":[
        {"type":"m.room.message","sender":"@bot:matrix.org","content":{"msgtype":"m.text","body":"from bot"}}
      ]}}}}}|}
  in
  let events = Matrix.parse_sync_events json ~cfg in
  Alcotest.(check int) "skip own messages" 0 (List.length events)

let test_parse_sync_skips_non_text () =
  let cfg = mk_matrix_cfg () in
  let json =
    Yojson.Safe.from_string
      {|{"rooms":{"join":{"!room:m.org":{"timeline":{"events":[
        {"type":"m.room.message","sender":"@alice:m.org","content":{"msgtype":"m.image","body":"pic.jpg"}}
      ]}}}}}|}
  in
  let events = Matrix.parse_sync_events json ~cfg in
  Alcotest.(check int) "skip non-text" 0 (List.length events)

let test_parse_sync_skips_disallowed_room () =
  let cfg = mk_matrix_cfg ~allow_rooms:[ "!allowed:m.org" ] () in
  let json =
    Yojson.Safe.from_string
      {|{"rooms":{"join":{"!other:m.org":{"timeline":{"events":[
        {"type":"m.room.message","sender":"@alice:m.org","content":{"msgtype":"m.text","body":"hello"}}
      ]}}}}}|}
  in
  let events = Matrix.parse_sync_events json ~cfg in
  Alcotest.(check int) "skip disallowed room" 0 (List.length events)

let test_parse_sync_skips_disallowed_user () =
  let cfg = mk_matrix_cfg ~allow_users:[ "@alice:m.org" ] () in
  let json =
    Yojson.Safe.from_string
      {|{"rooms":{"join":{"!room:m.org":{"timeline":{"events":[
        {"type":"m.room.message","sender":"@bob:m.org","content":{"msgtype":"m.text","body":"hello"}}
      ]}}}}}|}
  in
  let events = Matrix.parse_sync_events json ~cfg in
  Alcotest.(check int) "skip disallowed user" 0 (List.length events)

(* --- make_txn_id tests --- *)

let test_txn_id_non_empty () =
  let id = Matrix.make_txn_id ~room_id:"!room:m.org" in
  Alcotest.(check bool) "non-empty" true (String.length id > 0)

let test_txn_id_length () =
  let id = Matrix.make_txn_id ~room_id:"!room:m.org" in
  Alcotest.(check int) "16 chars" 16 (String.length id)

let test_txn_id_unique () =
  let id1 = Matrix.make_txn_id ~room_id:"!room1:m.org" in
  let id2 = Matrix.make_txn_id ~room_id:"!room1:m.org" in
  Alcotest.(check bool) "unique ids" true (id1 <> id2)

let suite =
  [
    Alcotest.test_case "chunk short" `Quick test_chunk_short;
    Alcotest.test_case "chunk empty" `Quick test_chunk_empty;
    Alcotest.test_case "chunk over limit" `Quick test_chunk_over_limit;
    Alcotest.test_case "chunk preserves content" `Quick
      test_chunk_preserves_content;
    Alcotest.test_case "room allowed empty" `Quick test_room_allowed_empty;
    Alcotest.test_case "room allowed match" `Quick test_room_allowed_match;
    Alcotest.test_case "room allowed no match" `Quick test_room_allowed_no_match;
    Alcotest.test_case "user allowed empty" `Quick test_user_allowed_empty;
    Alcotest.test_case "user allowed match" `Quick test_user_allowed_match;
    Alcotest.test_case "user allowed no match" `Quick test_user_allowed_no_match;
    Alcotest.test_case "parse sync empty" `Quick test_parse_sync_empty;
    Alcotest.test_case "parse sync no rooms" `Quick test_parse_sync_no_rooms;
    Alcotest.test_case "parse sync with message" `Quick
      test_parse_sync_with_message;
    Alcotest.test_case "parse sync skips own messages" `Quick
      test_parse_sync_skips_own_messages;
    Alcotest.test_case "parse sync skips non-text" `Quick
      test_parse_sync_skips_non_text;
    Alcotest.test_case "parse sync skips disallowed room" `Quick
      test_parse_sync_skips_disallowed_room;
    Alcotest.test_case "parse sync skips disallowed user" `Quick
      test_parse_sync_skips_disallowed_user;
    Alcotest.test_case "txn_id non-empty" `Quick test_txn_id_non_empty;
    Alcotest.test_case "txn_id length" `Quick test_txn_id_length;
    Alcotest.test_case "txn_id unique" `Quick test_txn_id_unique;
  ]
