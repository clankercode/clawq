(* Tests for Mattermost channel module *)

let mk_mm_cfg ?(allow_users = [ "*" ]) ?(channel_ids = []) () :
    Runtime_config.mattermost_config =
  {
    url = "https://mattermost.example.com";
    access_token = "tok";
    team_id = "team1";
    channel_ids;
    allow_users;
    default_model = None;
  }

(* --- is_allowed tests --- *)

let test_is_allowed_wildcard () =
  let config = mk_mm_cfg () in
  Alcotest.(check bool)
    "wildcard" true
    (Mattermost.is_allowed ~config ~user_id:"any")

let test_is_allowed_match () =
  let config = mk_mm_cfg ~allow_users:[ "user1"; "user2" ] () in
  Alcotest.(check bool)
    "match" true
    (Mattermost.is_allowed ~config ~user_id:"user1")

let test_is_allowed_no_match () =
  let config = mk_mm_cfg ~allow_users:[ "user1" ] () in
  Alcotest.(check bool)
    "no match" false
    (Mattermost.is_allowed ~config ~user_id:"user9")

(* --- is_allowed_channel tests --- *)

let test_channel_allowed_empty () =
  let config = mk_mm_cfg () in
  Alcotest.(check bool)
    "empty allows all" true
    (Mattermost.is_allowed_channel ~config ~channel_id:"any")

let test_channel_allowed_match () =
  let config = mk_mm_cfg ~channel_ids:[ "ch1"; "ch2" ] () in
  Alcotest.(check bool)
    "match" true
    (Mattermost.is_allowed_channel ~config ~channel_id:"ch1")

let test_channel_allowed_no_match () =
  let config = mk_mm_cfg ~channel_ids:[ "ch1" ] () in
  Alcotest.(check bool)
    "no match" false
    (Mattermost.is_allowed_channel ~config ~channel_id:"ch9")

(* --- parse_posted_event tests --- *)

let test_parse_posted_valid () =
  let post_json =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("channel_id", `String "ch1");
           ("user_id", `String "u1");
           ("message", `String "hello");
         ])
  in
  let data =
    `Assoc [ ("post", `String post_json); ("channel_type", `String "D") ]
  in
  match Mattermost.parse_posted_event data with
  | Some (ch, uid, msg, ctype) ->
      Alcotest.(check string) "channel" "ch1" ch;
      Alcotest.(check string) "user" "u1" uid;
      Alcotest.(check string) "message" "hello" msg;
      Alcotest.(check string) "channel_type" "D" ctype
  | None -> Alcotest.fail "expected Some"

let test_parse_posted_missing_channel_type () =
  let post_json =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("channel_id", `String "ch1");
           ("user_id", `String "u1");
           ("message", `String "hi");
         ])
  in
  let data = `Assoc [ ("post", `String post_json) ] in
  match Mattermost.parse_posted_event data with
  | Some (_, _, _, ctype) ->
      (* Absent channel_type defaults to "" -> group conduct *)
      Alcotest.(check string) "channel_type default" "" ctype;
      Alcotest.(check string)
        "maps to group" "group"
        (Mattermost.clawq_channel_type_of_mm ctype)
  | None -> Alcotest.fail "expected Some"

let test_clawq_channel_type_mapping () =
  Alcotest.(check string)
    "direct -> dm" "dm"
    (Mattermost.clawq_channel_type_of_mm "D");
  Alcotest.(check string)
    "group DM -> group" "group"
    (Mattermost.clawq_channel_type_of_mm "G");
  Alcotest.(check string)
    "open -> group" "group"
    (Mattermost.clawq_channel_type_of_mm "O");
  Alcotest.(check string)
    "private -> group" "group"
    (Mattermost.clawq_channel_type_of_mm "P")

let test_parse_posted_invalid () =
  let data = `Assoc [ ("post", `String "not json") ] in
  match Mattermost.parse_posted_event data with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None"

let test_parse_posted_missing_post () =
  match Mattermost.parse_posted_event (`Assoc []) with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None"

let test_parse_posted_null () =
  match Mattermost.parse_posted_event `Null with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None"

let suite =
  [
    Alcotest.test_case "is_allowed wildcard" `Quick test_is_allowed_wildcard;
    Alcotest.test_case "is_allowed match" `Quick test_is_allowed_match;
    Alcotest.test_case "is_allowed no match" `Quick test_is_allowed_no_match;
    Alcotest.test_case "channel allowed empty" `Quick test_channel_allowed_empty;
    Alcotest.test_case "channel allowed match" `Quick test_channel_allowed_match;
    Alcotest.test_case "channel allowed no match" `Quick
      test_channel_allowed_no_match;
    Alcotest.test_case "parse posted valid" `Quick test_parse_posted_valid;
    Alcotest.test_case "parse posted invalid" `Quick test_parse_posted_invalid;
    Alcotest.test_case "parse posted missing" `Quick
      test_parse_posted_missing_post;
    Alcotest.test_case "parse posted null" `Quick test_parse_posted_null;
    Alcotest.test_case "parse posted missing channel_type" `Quick
      test_parse_posted_missing_channel_type;
    Alcotest.test_case "clawq channel_type mapping" `Quick
      test_clawq_channel_type_mapping;
  ]
