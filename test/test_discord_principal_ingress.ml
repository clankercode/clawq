(** Tests for Discord Gateway/interaction principal derivation (P21.M1.E1.T007).
*)

module D = Discord_principal_ingress

let () = Mirage_crypto_rng_unix.use_default ()
let app_id = "123456789012345678"
let guild_id = "987654321098765432"
let user_id = "111122223333444455"
let session_id = "ready-session-abc"

let ready_session ?(last_seq = Some 10) ?(ready = true)
    ?(application_id = app_id) ?(session_id = session_id) () : D.gateway_session
    =
  { session_id; application_id; ready; last_seq }

let human_payload ?(guild = Some guild_id) ?(uid = user_id) ?(bot = false)
    ?(username = "Ada") ?webhook_id () =
  let author_fields =
    [ ("id", `String uid); ("username", `String username); ("bot", `Bool bot) ]
  in
  let fields =
    [
      ("id", `String "msg-1");
      ("channel_id", `String "chan-1");
      ("author", `Assoc author_fields);
    ]
  in
  let fields =
    match guild with
    | Some g -> ("guild_id", `String g) :: fields
    | None -> fields
  in
  let fields =
    match webhook_id with
    | Some w -> ("webhook_id", `String w) :: fields
    | None -> fields
  in
  `Assoc fields

let outcome_is_human = function D.Human _ -> true | _ -> false
let outcome_is_invalid = function D.Invalid _ -> true | _ -> false
let outcome_is_bot = function D.Bot_rejected _ -> true | _ -> false
let invalid_msg = function D.Invalid m -> m | _ -> ""
let bot_msg = function D.Bot_rejected m -> m | _ -> ""

let contains_ci hay needle =
  try
    let _ =
      Str.search_forward
        (Str.regexp_string (String.lowercase_ascii needle))
        (String.lowercase_ascii hay)
        0
    in
    true
  with Not_found -> false

let hex_encode s =
  let n = String.length s in
  let buf = Bytes.create (n * 2) in
  let hex = "0123456789abcdef" in
  for i = 0 to n - 1 do
    let c = Char.code s.[i] in
    Bytes.set buf (i * 2) hex.[c lsr 4];
    Bytes.set buf ((i * 2) + 1) hex.[c land 0xf]
  done;
  Bytes.unsafe_to_string buf

let test_happy_path_human () =
  let session = ready_session () in
  let outcome =
    D.derive_from_gateway ~session ~seq:11 ~expected_application_id:app_id
      ~event_name:"MESSAGE_CREATE" ~payload_json:(human_payload ()) ()
  in
  match outcome with
  | D.Human { identity; display_name; context } -> (
      Alcotest.(check string) "guild" guild_id identity.guild_id;
      Alcotest.(check string) "user" user_id identity.user_id;
      Alcotest.(check (option string)) "display" (Some "Ada") display_name;
      Alcotest.(check string)
        "canonical key"
        (Printf.sprintf "guild:%s:user:%s" guild_id user_id)
        (D.human_identity_key identity);
      Alcotest.(check bool)
        "gateway source" true
        (match context.source with `Gateway -> true | _ -> false);
      Alcotest.(check (option string))
        "app" (Some app_id) context.application_id;
      Alcotest.(check (option int)) "seq" (Some 11) context.seq;
      match D.connector_actor_key_of_identity identity with
      | Error e -> Alcotest.failf "actor key: %s" e
      | Ok key ->
          Alcotest.(check string)
            "actor key"
            (Printf.sprintf "connector:discord:tenant:%s:user:%s" guild_id
               user_id)
            (Principal_identity.actor_identity_key key))
  | D.Invalid e -> Alcotest.failf "expected Human, got Invalid: %s" e
  | D.Bot_rejected e -> Alcotest.failf "expected Human, got Bot: %s" e

let test_bot_rejected () =
  let session = ready_session () in
  let outcome =
    D.derive_from_gateway ~session
      ~payload_json:(human_payload ~bot:true ~username:"OtherBot" ())
      ()
  in
  Alcotest.(check bool) "bot rejected" true (outcome_is_bot outcome);
  Alcotest.(check bool)
    "mentions bot" true
    (contains_ci (bot_msg outcome) "bot")

let test_webhook_rejected () =
  let session = ready_session () in
  let outcome =
    D.derive_from_gateway ~session
      ~payload_json:(human_payload ~webhook_id:"555566667777888899" ())
      ()
  in
  Alcotest.(check bool) "webhook rejected" true (outcome_is_bot outcome);
  Alcotest.(check bool)
    "mentions webhook" true
    (contains_ci (bot_msg outcome) "webhook")

let test_missing_user_id () =
  let session = ready_session () in
  let payload =
    `Assoc
      [
        ("guild_id", `String guild_id);
        ("author", `Assoc [ ("username", `String "NoId"); ("bot", `Bool false) ]);
      ]
  in
  let outcome = D.derive_from_gateway ~session ~payload_json:payload () in
  Alcotest.(check bool) "invalid" true (outcome_is_invalid outcome);
  Alcotest.(check bool)
    "mentions user" true
    (contains_ci (invalid_msg outcome) "user_id")

let test_missing_guild_id_dm () =
  let session = ready_session () in
  let outcome =
    D.derive_from_gateway ~session
      ~payload_json:(human_payload ~guild:None ())
      ()
  in
  Alcotest.(check bool) "dm fail closed" true (outcome_is_invalid outcome);
  Alcotest.(check bool)
    "mentions guild" true
    (contains_ci (invalid_msg outcome) "guild")

let test_missing_author () =
  let session = ready_session () in
  let payload =
    `Assoc [ ("guild_id", `String guild_id); ("content", `String "hi") ]
  in
  let outcome = D.derive_from_gateway ~session ~payload_json:payload () in
  Alcotest.(check bool) "invalid" true (outcome_is_invalid outcome);
  Alcotest.(check bool)
    "mentions author" true
    (contains_ci (invalid_msg outcome) "author")

let test_pre_ready_fail_closed () =
  let session = ready_session ~ready:false () in
  let outcome =
    D.derive_from_gateway ~session ~payload_json:(human_payload ()) ()
  in
  Alcotest.(check bool) "invalid" true (outcome_is_invalid outcome);
  Alcotest.(check bool)
    "mentions Ready" true
    (contains_ci (invalid_msg outcome) "ready")

let test_sequence_regression () =
  let session = ready_session ~last_seq:(Some 42) () in
  let outcome =
    D.derive_from_gateway ~session ~seq:10 ~payload_json:(human_payload ()) ()
  in
  Alcotest.(check bool) "invalid" true (outcome_is_invalid outcome);
  Alcotest.(check bool)
    "mentions sequence" true
    (contains_ci (invalid_msg outcome) "sequence")

let test_application_mismatch () =
  let session = ready_session () in
  let outcome =
    D.derive_from_gateway ~session ~expected_application_id:"999999999999999999"
      ~payload_json:(human_payload ()) ()
  in
  Alcotest.(check bool) "invalid" true (outcome_is_invalid outcome);
  Alcotest.(check bool)
    "mentions application" true
    (contains_ci (invalid_msg outcome) "application")

let test_display_name_not_identity () =
  let session = ready_session () in
  let o1 =
    D.derive_from_gateway ~session
      ~payload_json:(human_payload ~username:"Name A" ())
      ()
  in
  let o2 =
    D.derive_from_gateway ~session
      ~payload_json:(human_payload ~username:"Name B" ())
      ()
  in
  match (o1, o2) with
  | D.Human h1, D.Human h2 ->
      Alcotest.(check string)
        "same key"
        (D.human_identity_key h1.identity)
        (D.human_identity_key h2.identity);
      Alcotest.(check (option string)) "name a" (Some "Name A") h1.display_name;
      Alcotest.(check (option string)) "name b" (Some "Name B") h2.display_name
  | _ -> Alcotest.fail "both should be Human"

let test_fields_missing_ids () =
  let session = ready_session () in
  let o1 =
    D.derive_from_fields ~session ~guild_id:None ~user_id:(Some user_id) ()
  in
  let o2 =
    D.derive_from_fields ~session ~guild_id:(Some guild_id) ~user_id:None ()
  in
  Alcotest.(check bool) "missing guild" true (outcome_is_invalid o1);
  Alcotest.(check bool) "missing user" true (outcome_is_invalid o2)

let test_non_snowflake_ids () =
  let session = ready_session () in
  let outcome =
    D.derive_from_fields ~session ~guild_id:(Some "not-a-snowflake")
      ~user_id:(Some user_id) ()
  in
  Alcotest.(check bool) "invalid snowflake" true (outcome_is_invalid outcome)

let test_is_snowflake () =
  Alcotest.(check bool) "ok" true (D.is_snowflake "1234567890");
  Alcotest.(check bool) "empty" false (D.is_snowflake "");
  Alcotest.(check bool) "alpha" false (D.is_snowflake "12ab")

let test_interaction_ed25519_happy () =
  let priv, pub = Mirage_crypto_ec.Ed25519.generate () in
  let pub_hex = hex_encode (Mirage_crypto_ec.Ed25519.pub_to_octets pub) in
  let body =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("type", `Int 2);
           ("application_id", `String app_id);
           ("guild_id", `String guild_id);
           ( "member",
             `Assoc
               [
                 ( "user",
                   `Assoc
                     [
                       ("id", `String user_id);
                       ("username", `String "Ada");
                       ("bot", `Bool false);
                     ] );
               ] );
         ])
  in
  let timestamp = "1700000000" in
  let msg = timestamp ^ body in
  let sig_hex = hex_encode (Mirage_crypto_ec.Ed25519.sign ~key:priv msg) in
  let interaction_json = Yojson.Safe.from_string body in
  let outcome =
    D.derive_from_interaction ~public_key_hex:pub_hex ~signature_hex:sig_hex
      ~timestamp ~body ~expected_application_id:app_id ~interaction_json ()
  in
  match outcome with
  | D.Human { identity; context; _ } ->
      Alcotest.(check string) "guild" guild_id identity.guild_id;
      Alcotest.(check string) "user" user_id identity.user_id;
      Alcotest.(check bool)
        "interaction source" true
        (match context.source with `Interaction -> true | _ -> false)
  | D.Invalid e -> Alcotest.failf "expected Human: %s" e
  | D.Bot_rejected e -> Alcotest.failf "expected Human: %s" e

let test_interaction_bad_signature () =
  let _priv, pub = Mirage_crypto_ec.Ed25519.generate () in
  let priv2, _ = Mirage_crypto_ec.Ed25519.generate () in
  let pub_hex = hex_encode (Mirage_crypto_ec.Ed25519.pub_to_octets pub) in
  let body =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("application_id", `String app_id);
           ("guild_id", `String guild_id);
           ("user", `Assoc [ ("id", `String user_id); ("bot", `Bool false) ]);
         ])
  in
  let timestamp = "1700000000" in
  let sig_hex =
    hex_encode (Mirage_crypto_ec.Ed25519.sign ~key:priv2 (timestamp ^ body))
  in
  let outcome =
    D.derive_from_interaction ~public_key_hex:pub_hex ~signature_hex:sig_hex
      ~timestamp ~body
      ~interaction_json:(Yojson.Safe.from_string body)
      ()
  in
  Alcotest.(check bool) "bad sig fail closed" true (outcome_is_invalid outcome);
  Alcotest.(check bool)
    "mentions signature" true
    (contains_ci (invalid_msg outcome) "signature")

let test_interaction_missing_signature_fail_closed () =
  let interaction_json =
    `Assoc
      [
        ("guild_id", `String guild_id);
        ("user", `Assoc [ ("id", `String user_id); ("bot", `Bool false) ]);
      ]
  in
  let outcome = D.derive_from_interaction ~interaction_json () in
  Alcotest.(check bool) "fail closed" true (outcome_is_invalid outcome);
  Alcotest.(check bool)
    "mentions signature" true
    (contains_ci (invalid_msg outcome) "signature")

let test_interaction_bot_rejected () =
  let interaction_json =
    `Assoc
      [
        ("guild_id", `String guild_id);
        ( "user",
          `Assoc
            [
              ("id", `String user_id);
              ("bot", `Bool true);
              ("username", `String "AppBot");
            ] );
      ]
  in
  let outcome =
    D.derive_from_interaction ~require_signature:false ~interaction_json ()
  in
  Alcotest.(check bool) "bot" true (outcome_is_bot outcome)

let suite =
  [
    ("happy path derives human identity", `Quick, test_happy_path_human);
    ("bot identity rejected", `Quick, test_bot_rejected);
    ("webhook identity rejected", `Quick, test_webhook_rejected);
    ("missing user_id fail closed", `Quick, test_missing_user_id);
    ("missing guild_id DM fail closed", `Quick, test_missing_guild_id_dm);
    ("missing author fail closed", `Quick, test_missing_author);
    ("pre-Ready session fail closed", `Quick, test_pre_ready_fail_closed);
    ("sequence regression fail closed", `Quick, test_sequence_regression);
    ("application mismatch fail closed", `Quick, test_application_mismatch);
    ("display name is not identity", `Quick, test_display_name_not_identity);
    ("fields missing ids fail closed", `Quick, test_fields_missing_ids);
    ("non-snowflake ids fail closed", `Quick, test_non_snowflake_ids);
    ("is_snowflake helper", `Quick, test_is_snowflake);
    ("interaction Ed25519 happy path", `Quick, test_interaction_ed25519_happy);
    ( "interaction bad signature fail closed",
      `Quick,
      test_interaction_bad_signature );
    ( "interaction missing signature fail closed",
      `Quick,
      test_interaction_missing_signature_fail_closed );
    ("interaction bot rejected", `Quick, test_interaction_bot_rejected);
  ]
