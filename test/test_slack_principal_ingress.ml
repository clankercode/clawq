(** Tests for Slack Socket Mode ingress principal derivation (P21.M1.E1.T006).
    Fixture JSON covers hello app identity, envelopes, bots, display-only,
    namespace mismatch, envelope_id replay, and Events API signing-secret HMAC.
*)

module S = Slack_principal_ingress

(* ---- fixtures ---- *)

let app_id = "A012ABCDEF"
let team_id = "T061EG9RZ"
let enterprise_id = "E012ENTER"
let user_id = "U061F1EUR"
let channel_id = "C061EG9SL"

let fixture_hello =
  {|{
  "type": "hello",
  "connection_info": { "app_id": "A012ABCDEF" },
  "num_connections": 1,
  "debug_info": {
    "host": "applink-1",
    "approximate_connection_time": 3600
  }
}|}

let fixture_hello_wrong_app =
  {|{
  "type": "hello",
  "connection_info": { "app_id": "A0WRONG" },
  "num_connections": 1
}|}

let fixture_disconnect =
  {|{
  "type": "disconnect",
  "reason": "refresh_requested",
  "debug_info": { "host": "wss-111.slack.com" }
}|}

let fixture_events_api_human =
  {|{
  "envelope_id": "env-human-001",
  "type": "events_api",
  "accepts_response_payload": false,
  "payload": {
    "token": "legacy-verification-token-ignored",
    "team_id": "T061EG9RZ",
    "api_app_id": "A012ABCDEF",
    "type": "event_callback",
    "event_id": "Ev01ABC",
    "event_time": 1700000000,
    "event": {
      "type": "message",
      "channel": "C061EG9SL",
      "user": "U061F1EUR",
      "text": "hello clawq",
      "ts": "1700000000.000100",
      "event_ts": "1700000000.000100"
    }
  }
}|}

let fixture_events_api_enterprise =
  {|{
  "envelope_id": "env-ent-001",
  "type": "events_api",
  "accepts_response_payload": false,
  "payload": {
    "team_id": "T061EG9RZ",
    "enterprise_id": "E012ENTER",
    "api_app_id": "A012ABCDEF",
    "type": "event_callback",
    "event": {
      "type": "message",
      "channel": "C061EG9SL",
      "user": "U061F1EUR",
      "text": "enterprise hi",
      "ts": "1700000001.000100"
    }
  }
}|}

let fixture_events_api_bot =
  {|{
  "envelope_id": "env-bot-001",
  "type": "events_api",
  "accepts_response_payload": false,
  "payload": {
    "team_id": "T061EG9RZ",
    "api_app_id": "A012ABCDEF",
    "type": "event_callback",
    "event": {
      "type": "message",
      "subtype": "bot_message",
      "channel": "C061EG9SL",
      "bot_id": "B0BOTBOT",
      "text": "I am a bot",
      "ts": "1700000002.000100"
    }
  }
}|}

let fixture_events_api_display_only =
  {|{
  "envelope_id": "env-display-001",
  "type": "events_api",
  "accepts_response_payload": false,
  "payload": {
    "team_id": "T061EG9RZ",
    "api_app_id": "A012ABCDEF",
    "type": "event_callback",
    "event": {
      "type": "message",
      "channel": "C061EG9SL",
      "user": { "name": "display-only-user" },
      "text": "no immutable id",
      "ts": "1700000003.000100"
    }
  }
}|}

let fixture_events_api_wrong_team =
  {|{
  "envelope_id": "env-team-001",
  "type": "events_api",
  "accepts_response_payload": false,
  "payload": {
    "team_id": "T0OTHER",
    "api_app_id": "A012ABCDEF",
    "type": "event_callback",
    "event": {
      "type": "message",
      "channel": "C061EG9SL",
      "user": "U061F1EUR",
      "text": "wrong workspace",
      "ts": "1700000004.000100"
    }
  }
}|}

let fixture_events_api_missing_envelope_id =
  {|{
  "type": "events_api",
  "payload": {
    "team_id": "T061EG9RZ",
    "api_app_id": "A012ABCDEF",
    "type": "event_callback",
    "event": {
      "type": "message",
      "user": "U061F1EUR",
      "text": "no envelope id",
      "channel": "C061EG9SL"
    }
  }
}|}

let fixture_slash_command =
  {|{
  "payload": {
    "token": "bHKJ2n9AW6Ju3MjciOHfbA1b",
    "team_id": "T061EG9RZ",
    "team_domain": "clawq",
    "channel_id": "C061EG9SL",
    "channel_name": "general",
    "user_id": "U061F1EUR",
    "user_name": "ada",
    "command": "/clawq",
    "text": "status",
    "api_app_id": "A012ABCDEF"
  },
  "envelope_id": "env-slash-001",
  "type": "slash_commands",
  "accepts_response_payload": true
}|}

let fixture_interactive =
  {|{
  "payload": {
    "type": "block_actions",
    "team": { "id": "T061EG9RZ", "domain": "clawq" },
    "user": { "id": "U061F1EUR", "username": "ada", "team_id": "T061EG9RZ" },
    "api_app_id": "A012ABCDEF",
    "channel": { "id": "C061EG9SL", "name": "general" },
    "actions": []
  },
  "envelope_id": "env-interactive-001",
  "type": "interactive",
  "accepts_response_payload": true
}|}

(* ---- helpers ---- *)

let outcome_is_human = function S.Human _ -> true | _ -> false
let outcome_is_invalid = function S.Invalid _ -> true | _ -> false
let outcome_is_bot = function S.Bot_rejected _ -> true | _ -> false
let outcome_is_replay = function S.Replay _ -> true | _ -> false
let outcome_is_hello = function S.Hello _ -> true | _ -> false
let outcome_is_disconnect = function S.Disconnect _ -> true | _ -> false
let invalid_msg = function S.Invalid m -> m | _ -> ""
let bot_msg = function S.Bot_rejected m -> m | _ -> ""
let replay_msg = function S.Replay m -> m | _ -> ""

let contains ~needle s =
  try
    let _ = Str.search_forward (Str.regexp_string needle) s 0 in
    true
  with Not_found -> false

(* ---- tests ---- *)

let test_hello_app_identity_ok () =
  let outcome =
    S.validate_socket_message_string ~expected_app_id:app_id fixture_hello
  in
  match outcome with
  | S.Hello { app_id = got; num_connections } ->
      Alcotest.(check string) "app_id" app_id got;
      Alcotest.(check (option int)) "num_connections" (Some 1) num_connections
  | _ -> Alcotest.fail "expected Hello"

let test_hello_app_identity_mismatch () =
  let outcome =
    S.validate_socket_message_string ~expected_app_id:app_id
      fixture_hello_wrong_app
  in
  Alcotest.(check bool) "invalid" true (outcome_is_invalid outcome);
  Alcotest.(check bool)
    "mentions app_id" true
    (contains ~needle:"app_id" (invalid_msg outcome))

let test_disconnect () =
  let outcome = S.validate_socket_message_string fixture_disconnect in
  match outcome with
  | S.Disconnect { reason } ->
      Alcotest.(check (option string))
        "reason" (Some "refresh_requested") reason
  | _ -> Alcotest.fail "expected Disconnect"

let test_events_api_human_happy_path () =
  let seen = S.empty_seen_set () in
  let outcome =
    S.validate_socket_message_string ~expected_app_id:app_id
      ~expected_team_id:team_id ~seen fixture_events_api_human
  in
  match outcome with
  | S.Human { identity; display_name; event; ack } ->
      Alcotest.(check string) "team" team_id identity.team_id;
      Alcotest.(check string) "user" user_id identity.user_id;
      Alcotest.(check (option string)) "enterprise" None identity.enterprise_id;
      Alcotest.(check (option string)) "display absent" None display_name;
      Alcotest.(check string) "event type" "message" event.event_type;
      Alcotest.(check (option string))
        "channel" (Some channel_id) event.channel_id;
      Alcotest.(check string)
        "canonical key"
        (Printf.sprintf "team:%s:user:%s" team_id user_id)
        (S.human_identity_key identity);
      Alcotest.(check string)
        "workspace scope" team_id
        (S.workspace_scope identity);
      (match ack with
      | `Assoc [ ("envelope_id", `String eid) ] ->
          Alcotest.(check string) "ack envelope_id" "env-human-001" eid
      | _ -> Alcotest.fail "ack shape");
      (match S.to_connector_actor_key identity with
      | Ok key ->
          Alcotest.(check string)
            "actor key"
            (Printf.sprintf "connector:slack:tenant:%s:user:%s" team_id user_id)
            (Principal_identity.actor_identity_key key)
      | Error e -> Alcotest.failf "connector key: %s" e);
      (* Display name must not affect identity key *)
      Alcotest.(check string)
        "key ignores display"
        (S.human_identity_key identity)
        (S.human_identity_key { team_id; enterprise_id = None; user_id })
  | S.Invalid e -> Alcotest.failf "expected Human, got Invalid: %s" e
  | S.Bot_rejected e -> Alcotest.failf "expected Human, got Bot: %s" e
  | _ -> Alcotest.fail "expected Human"

let test_events_api_enterprise_namespace () =
  let seen = S.empty_seen_set () in
  let outcome =
    S.validate_socket_message_string ~expected_app_id:app_id
      ~expected_team_id:team_id ~expected_enterprise_id:enterprise_id ~seen
      fixture_events_api_enterprise
  in
  match outcome with
  | S.Human { identity; _ } ->
      Alcotest.(check (option string))
        "enterprise" (Some enterprise_id) identity.enterprise_id;
      Alcotest.(check string)
        "key with enterprise"
        (Printf.sprintf "enterprise:%s:team:%s:user:%s" enterprise_id team_id
           user_id)
        (S.human_identity_key identity);
      Alcotest.(check string)
        "scope"
        (enterprise_id ^ "/" ^ team_id)
        (S.workspace_scope identity)
  | S.Invalid e -> Alcotest.failf "expected Human: %s" e
  | _ -> Alcotest.fail "expected Human"

let test_bot_rejected () =
  let seen = S.empty_seen_set () in
  let outcome =
    S.validate_socket_message_string ~expected_app_id:app_id
      ~expected_team_id:team_id ~seen fixture_events_api_bot
  in
  Alcotest.(check bool) "bot rejected" true (outcome_is_bot outcome);
  Alcotest.(check bool)
    "mentions bot" true
    (contains ~needle:"bot" (bot_msg outcome))

let test_display_only_rejected () =
  let seen = S.empty_seen_set () in
  let outcome =
    S.validate_socket_message_string ~expected_app_id:app_id
      ~expected_team_id:team_id ~seen fixture_events_api_display_only
  in
  Alcotest.(check bool) "invalid" true (outcome_is_invalid outcome);
  Alcotest.(check bool)
    "mentions display or user_id" true
    (let m = invalid_msg outcome in
     contains ~needle:"display" m || contains ~needle:"user_id" m)

let test_team_mismatch () =
  let seen = S.empty_seen_set () in
  let outcome =
    S.validate_socket_message_string ~expected_app_id:app_id
      ~expected_team_id:team_id ~seen fixture_events_api_wrong_team
  in
  Alcotest.(check bool) "invalid" true (outcome_is_invalid outcome);
  Alcotest.(check bool)
    "mentions team" true
    (contains ~needle:"team_id" (invalid_msg outcome))

let test_missing_envelope_id () =
  let outcome =
    S.validate_socket_message_string ~expected_app_id:app_id
      fixture_events_api_missing_envelope_id
  in
  Alcotest.(check bool) "invalid" true (outcome_is_invalid outcome);
  Alcotest.(check bool)
    "mentions envelope_id" true
    (contains ~needle:"envelope_id" (invalid_msg outcome))

let test_envelope_id_dedupe_ack_once () =
  let seen = S.empty_seen_set () in
  let first =
    S.validate_socket_message_string ~expected_app_id:app_id
      ~expected_team_id:team_id ~seen fixture_events_api_human
  in
  Alcotest.(check bool) "first human" true (outcome_is_human first);
  let second =
    S.validate_socket_message_string ~expected_app_id:app_id
      ~expected_team_id:team_id ~seen fixture_events_api_human
  in
  Alcotest.(check bool) "second is replay" true (outcome_is_replay second);
  Alcotest.(check bool)
    "mentions envelope" true
    (contains ~needle:"envelope_id" (replay_msg second));
  (* Distinct envelope_id is accepted *)
  let third =
    S.validate_socket_message_string ~expected_app_id:app_id
      ~expected_team_id:team_id ~seen fixture_events_api_enterprise
  in
  Alcotest.(check bool) "different id ok" true (outcome_is_human third)

let test_slash_command_identity () =
  let seen = S.empty_seen_set () in
  let outcome =
    S.validate_socket_message_string ~expected_app_id:app_id
      ~expected_team_id:team_id ~seen fixture_slash_command
  in
  match outcome with
  | S.Human { identity; display_name; _ } ->
      Alcotest.(check string) "user" user_id identity.user_id;
      Alcotest.(check (option string)) "display" (Some "ada") display_name;
      Alcotest.(check string)
        "key ignores display"
        (Printf.sprintf "team:%s:user:%s" team_id user_id)
        (S.human_identity_key identity)
  | S.Invalid e -> Alcotest.failf "expected Human: %s" e
  | _ -> Alcotest.fail "expected Human"

let test_interactive_identity () =
  let seen = S.empty_seen_set () in
  let outcome =
    S.validate_socket_message_string ~expected_app_id:app_id
      ~expected_team_id:team_id ~seen fixture_interactive
  in
  match outcome with
  | S.Human { identity; display_name; event; _ } ->
      Alcotest.(check string) "user" user_id identity.user_id;
      Alcotest.(check (option string)) "display" (Some "ada") display_name;
      Alcotest.(check (option string))
        "channel" (Some channel_id) event.channel_id
  | S.Invalid e -> Alcotest.failf "expected Human: %s" e
  | _ -> Alcotest.fail "expected Human"

let test_malformed_json () =
  let outcome = S.validate_socket_message_string "{not-json" in
  Alcotest.(check bool) "invalid" true (outcome_is_invalid outcome);
  Alcotest.(check bool)
    "mentions parse" true
    (contains ~needle:"JSON" (invalid_msg outcome)
    || contains ~needle:"parse" (invalid_msg outcome))

let test_signing_secret_ok () =
  let secret = "my_signing_secret" in
  let timestamp = "1700000000" in
  let body = {|{"type":"event_callback","team_id":"T061EG9RZ"}|} in
  let basestring = "v0:" ^ timestamp ^ ":" ^ body in
  let signature =
    "v0=" ^ Digestif.SHA256.(hmac_string ~key:secret basestring |> to_hex)
  in
  match
    S.verify_events_api_signature ~now:1_700_000_000. ~signing_secret:secret
      ~timestamp ~body ~signature ()
  with
  | Ok () -> ()
  | Error e -> Alcotest.failf "expected Ok: %s" e

let test_signing_secret_bad_sig () =
  match
    S.verify_events_api_signature ~now:1_700_000_000.
      ~signing_secret:"my_signing_secret" ~timestamp:"1700000000" ~body:"{}"
      ~signature:"v0=deadbeef" ()
  with
  | Ok () -> Alcotest.fail "expected signature failure"
  | Error e ->
      Alcotest.(check bool)
        "mentions HMAC or failed" true
        (contains ~needle:"HMAC" e || contains ~needle:"failed" e)

let test_signing_secret_skew () =
  match
    S.verify_events_api_signature ~now:1_700_000_000.
      ~signing_secret:"my_signing_secret" ~timestamp:"1600000000" ~body:"{}"
      ~signature:
        (let basestring = "v0:1600000000:{}" in
         "v0="
         ^ Digestif.SHA256.(
             hmac_string ~key:"my_signing_secret" basestring |> to_hex))
      ()
  with
  | Ok () -> Alcotest.fail "expected skew failure"
  | Error e ->
      Alcotest.(check bool) "mentions skew" true (contains ~needle:"skew" e)

let test_signing_secret_empty () =
  match
    S.verify_events_api_signature ~signing_secret:"" ~timestamp:"1" ~body:"{}"
      ~signature:"v0=x" ()
  with
  | Ok () -> Alcotest.fail "expected empty secret failure"
  | Error e ->
      Alcotest.(check bool)
        "mentions secret" true
        (contains ~needle:"signing_secret" e)

let test_make_ack () =
  match S.make_ack ~envelope_id:"env-xyz" with
  | `Assoc [ ("envelope_id", `String "env-xyz") ] -> ()
  | _ -> Alcotest.fail "ack shape"

let suite =
  [
    ("hello app identity ok", `Quick, test_hello_app_identity_ok);
    ("hello app identity mismatch", `Quick, test_hello_app_identity_mismatch);
    ("disconnect", `Quick, test_disconnect);
    ("events_api human happy path", `Quick, test_events_api_human_happy_path);
    ( "events_api enterprise namespace",
      `Quick,
      test_events_api_enterprise_namespace );
    ("bot_id rejected", `Quick, test_bot_rejected);
    ("display-only rejected", `Quick, test_display_only_rejected);
    ("team_id mismatch", `Quick, test_team_mismatch);
    ("missing envelope_id", `Quick, test_missing_envelope_id);
    ("envelope_id dedupe ack once", `Quick, test_envelope_id_dedupe_ack_once);
    ("slash_commands identity", `Quick, test_slash_command_identity);
    ("interactive identity", `Quick, test_interactive_identity);
    ("malformed JSON", `Quick, test_malformed_json);
    ("signing secret ok", `Quick, test_signing_secret_ok);
    ("signing secret bad sig", `Quick, test_signing_secret_bad_sig);
    ("signing secret skew", `Quick, test_signing_secret_skew);
    ("signing secret empty", `Quick, test_signing_secret_empty);
    ("make_ack", `Quick, test_make_ack);
  ]
