let test_parse_envelope_valid () =
  let msg =
    {|{"envelope_id":"env123","type":"events_api","payload":{"event":{"type":"message","channel":"C1","user":"U1","text":"hi"}}}|}
  in
  match Slack_socket.parse_envelope msg with
  | None -> Alcotest.fail "expected Some envelope"
  | Some env ->
      Alcotest.(check string) "envelope_id" "env123" env.envelope_id;
      Alcotest.(check string) "payload_type" "events_api" env.payload_type;
      Alcotest.(check bool) "payload not null" true (env.payload <> `Null)

let test_parse_envelope_disconnect () =
  let msg = {|{"envelope_id":"env456","type":"disconnect","payload":{}}|} in
  match Slack_socket.parse_envelope msg with
  | None -> Alcotest.fail "expected Some envelope"
  | Some env ->
      Alcotest.(check string) "envelope_id" "env456" env.envelope_id;
      Alcotest.(check string) "payload_type" "disconnect" env.payload_type

let test_parse_envelope_no_id () =
  let msg = {|{"type":"events_api","payload":{}}|} in
  Alcotest.(check bool)
    "no envelope_id returns None" true
    (Slack_socket.parse_envelope msg = None)

let test_parse_envelope_invalid_json () =
  Alcotest.(check bool)
    "invalid json returns None" true
    (Slack_socket.parse_envelope "not json" = None)

let test_extract_event_body () =
  let payload =
    Yojson.Safe.from_string
      {|{"event":{"type":"message","channel":"C1","user":"U1","text":"hello"}}|}
  in
  let body = Slack_socket.extract_event_body payload in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "type" "event_callback"
    (json |> member "type" |> to_string);
  let event = json |> member "event" in
  Alcotest.(check string)
    "event type" "message"
    (event |> member "type" |> to_string);
  Alcotest.(check string) "channel" "C1" (event |> member "channel" |> to_string);
  Alcotest.(check string) "text" "hello" (event |> member "text" |> to_string)

let test_extract_event_body_reusable_by_slack () =
  (* Verify extracted body can be parsed by Slack.parse_event *)
  let payload =
    Yojson.Safe.from_string
      {|{"event":{"type":"message","channel":"C123","user":"U456","text":"world"}}|}
  in
  let body = Slack_socket.extract_event_body payload in
  match Slack.parse_event body with
  | Some (Slack.Message { channel_id; user_id; text; bot_id }) ->
      Alcotest.(check string) "channel_id" "C123" channel_id;
      Alcotest.(check string) "user_id" "U456" user_id;
      Alcotest.(check string) "text" "world" text;
      Alcotest.(check (option string)) "no bot_id" None bot_id
  | _ -> Alcotest.fail "expected Slack.Message"

let test_parse_envelope_empty_string_id () =
  let msg = {|{"envelope_id":"","type":"events_api","payload":{}}|} in
  Alcotest.(check bool)
    "empty envelope_id returns None" true
    (Slack_socket.parse_envelope msg = None)

let test_extract_event_body_non_object_payload () =
  (* When payload is not an Assoc, member raises Type_error and fallback fires *)
  let payload = `String "raw-text" in
  let body = Slack_socket.extract_event_body payload in
  Alcotest.(check string)
    "fallback returns raw payload string" {|"raw-text"|} body

let test_parse_envelope_empty_payload () =
  let msg = {|{"envelope_id":"env789","type":"hello"}|} in
  match Slack_socket.parse_envelope msg with
  | None -> Alcotest.fail "expected Some envelope"
  | Some env ->
      Alcotest.(check string) "envelope_id" "env789" env.envelope_id;
      Alcotest.(check string) "payload_type" "hello" env.payload_type

let suite =
  [
    Alcotest.test_case "parse envelope valid" `Quick test_parse_envelope_valid;
    Alcotest.test_case "parse envelope disconnect" `Quick
      test_parse_envelope_disconnect;
    Alcotest.test_case "parse envelope no id" `Quick test_parse_envelope_no_id;
    Alcotest.test_case "parse envelope invalid json" `Quick
      test_parse_envelope_invalid_json;
    Alcotest.test_case "extract event body" `Quick test_extract_event_body;
    Alcotest.test_case "extract event body reusable by slack" `Quick
      test_extract_event_body_reusable_by_slack;
    Alcotest.test_case "parse envelope empty string id" `Quick
      test_parse_envelope_empty_string_id;
    Alcotest.test_case "extract event body non-object payload" `Quick
      test_extract_event_body_non_object_payload;
    Alcotest.test_case "parse envelope empty payload" `Quick
      test_parse_envelope_empty_payload;
  ]
