(** Tests for Telegram ingress principal derivation (P21.M1.E1.T008). *)

module T = Telegram_principal_ingress

let bot_ns = "123456789"
let user_id = "987654321"
let secret = "whsec_test_token_abc"

let human_from ?(id = user_id) ?(is_bot = false) ?(first = "Ada")
    ?(last = Some "Lovelace") ?(username = Some "ada") () =
  let fields =
    [
      ("id", `Int (int_of_string id));
      ("is_bot", `Bool is_bot);
      ("first_name", `String first);
    ]
  in
  let fields =
    match last with
    | Some l -> fields @ [ ("last_name", `String l) ]
    | None -> fields
  in
  let fields =
    match username with
    | Some u -> fields @ [ ("username", `String u) ]
    | None -> fields
  in
  `Assoc fields

let human_message_update ?(update_id = 42) ?(from = human_from ())
    ?(chat_id = 111) ?(chat_type = "private") ?(text = "hello") () =
  `Assoc
    [
      ("update_id", `Int update_id);
      ( "message",
        `Assoc
          [
            ("message_id", `Int 1);
            ("from", from);
            ( "chat",
              `Assoc
                [
                  ("id", `Int chat_id);
                  ("type", `String chat_type);
                  ("first_name", `String "Ada");
                ] );
            ("text", `String text);
          ] );
    ]

let bot_message_update ?(update_id = 43) () =
  human_message_update ~update_id
    ~from:
      (human_from ~id:"555" ~is_bot:true ~first:"OtherBot" ~last:None
         ~username:(Some "other_bot") ())
    ()

let display_only_update ?(update_id = 44) () =
  `Assoc
    [
      ("update_id", `Int update_id);
      ( "message",
        `Assoc
          [
            ("message_id", `Int 2);
            ( "from",
              `Assoc
                [
                  ("first_name", `String "Only Display");
                  ("username", `String "nodid");
                  ("is_bot", `Bool false);
                ] );
            ("chat", `Assoc [ ("id", `Int 1); ("type", `String "private") ]);
            ("text", `String "hi");
          ] );
    ]

let outcome_is_human = function T.Human _ -> true | _ -> false
let outcome_is_invalid = function T.Invalid _ -> true | _ -> false
let outcome_is_bot = function T.Bot_rejected _ -> true | _ -> false
let outcome_is_stale = function T.Stale_or_replay _ -> true | _ -> false
let invalid_msg = function T.Invalid m -> m | _ -> ""
let bot_msg = function T.Bot_rejected m -> m | _ -> ""

let test_happy_path_long_poll () =
  let outcome =
    T.verify_and_derive_long_poll ~bot_namespace:bot_ns
      ~update_json:(human_message_update ()) ()
  in
  match outcome with
  | T.Human { identity; display_name; username; chat; update_id } ->
      Alcotest.(check string) "bot ns" bot_ns identity.bot_namespace;
      Alcotest.(check string) "user id" user_id identity.user_id;
      Alcotest.(check (option string))
        "display" (Some "Ada Lovelace") display_name;
      Alcotest.(check (option string)) "username" (Some "ada") username;
      Alcotest.(check int) "update_id" 42 update_id;
      Alcotest.(check string)
        "canonical key"
        (Printf.sprintf "bot:%s:user:%s" bot_ns user_id)
        (T.human_identity_key identity);
      (match chat with
      | Some { chat_id; kind = T.Private } ->
          Alcotest.(check string) "chat id" "111" chat_id
      | _ -> Alcotest.fail "expected private chat context");
      (* Display name must not be part of identity key *)
      let key2 = T.human_identity_key { bot_namespace = bot_ns; user_id } in
      Alcotest.(check string)
        "key ignores display" key2
        (T.human_identity_key identity)
  | T.Invalid e -> Alcotest.failf "expected Human, got Invalid: %s" e
  | T.Bot_rejected e -> Alcotest.failf "expected Human, got Bot: %s" e
  | T.Stale_or_replay { message; _ } ->
      Alcotest.failf "expected Human, got Stale: %s" message

let test_bot_rejected () =
  let outcome =
    T.verify_and_derive_long_poll ~bot_namespace:bot_ns
      ~update_json:(bot_message_update ()) ()
  in
  Alcotest.(check bool) "bot rejected" true (outcome_is_bot outcome);
  Alcotest.(check bool)
    "mentions bot" true
    (try
       let _ =
         Str.search_forward (Str.regexp_string "bot") (bot_msg outcome) 0
       in
       true
     with Not_found -> false)

let test_missing_from_id () =
  let outcome =
    T.verify_and_derive_long_poll ~bot_namespace:bot_ns
      ~update_json:(display_only_update ()) ()
  in
  Alcotest.(check bool) "invalid" true (outcome_is_invalid outcome);
  Alcotest.(check bool)
    "mentions id" true
    (try
       let m = invalid_msg outcome in
       let _ = Str.search_forward (Str.regexp_string "id") m 0 in
       true
     with Not_found -> false)

let test_missing_update_id () =
  let update =
    `Assoc
      [
        ( "message",
          `Assoc
            [
              ("from", human_from ());
              ("chat", `Assoc [ ("id", `Int 1); ("type", `String "private") ]);
              ("text", `String "x");
            ] );
      ]
  in
  let outcome =
    T.verify_and_derive_long_poll ~bot_namespace:bot_ns ~update_json:update ()
  in
  Alcotest.(check bool) "invalid" true (outcome_is_invalid outcome)

let test_empty_bot_namespace () =
  let outcome =
    T.verify_and_derive_long_poll ~bot_namespace:"  "
      ~update_json:(human_message_update ()) ()
  in
  Alcotest.(check bool) "invalid" true (outcome_is_invalid outcome)

let test_display_name_not_identity () =
  let o1 =
    T.verify_and_derive_long_poll ~bot_namespace:bot_ns
      ~update_json:
        (human_message_update
           ~from:(human_from ~first:"NameA" ~last:None ~username:None ())
           ())
      ()
  in
  let o2 =
    T.verify_and_derive_long_poll ~bot_namespace:bot_ns
      ~update_json:
        (human_message_update ~update_id:99
           ~from:(human_from ~first:"NameB" ~last:None ~username:None ())
           ())
      ()
  in
  match (o1, o2) with
  | T.Human h1, T.Human h2 ->
      Alcotest.(check string)
        "same identity key"
        (T.human_identity_key h1.identity)
        (T.human_identity_key h2.identity);
      Alcotest.(check (option string)) "name a" (Some "NameA") h1.display_name;
      Alcotest.(check (option string)) "name b" (Some "NameB") h2.display_name
  | _ -> Alcotest.fail "both should be Human"

let test_cross_bot_namespaces_distinct () =
  let o1 =
    T.verify_and_derive_long_poll ~bot_namespace:"bot-a"
      ~update_json:(human_message_update ()) ()
  in
  let o2 =
    T.verify_and_derive_long_poll ~bot_namespace:"bot-b"
      ~update_json:(human_message_update ()) ()
  in
  match (o1, o2) with
  | T.Human h1, T.Human h2 ->
      Alcotest.(check bool)
        "distinct keys" true
        (T.human_identity_key h1.identity <> T.human_identity_key h2.identity);
      Alcotest.(check string)
        "same user id" h1.identity.user_id h2.identity.user_id
  | _ -> Alcotest.fail "both should be Human"

let test_monotonic_offset_and_replay () =
  let store = T.create_offset_store () in
  let o1 =
    T.verify_and_derive_long_poll ~offset_store:store ~bot_namespace:bot_ns
      ~update_json:(human_message_update ~update_id:10 ())
      ()
  in
  Alcotest.(check bool) "first ok" true (outcome_is_human o1);
  Alcotest.(check (option int))
    "advanced" (Some 10)
    (T.last_offset store ~bot_namespace:bot_ns);
  let o2 =
    T.verify_and_derive_long_poll ~offset_store:store ~bot_namespace:bot_ns
      ~update_json:(human_message_update ~update_id:10 ())
      ()
  in
  Alcotest.(check bool) "replay rejected" true (outcome_is_stale o2);
  let o3 =
    T.verify_and_derive_long_poll ~offset_store:store ~bot_namespace:bot_ns
      ~update_json:(human_message_update ~update_id:9 ())
      ()
  in
  Alcotest.(check bool) "stale rejected" true (outcome_is_stale o3);
  let o4 =
    T.verify_and_derive_long_poll ~offset_store:store ~bot_namespace:bot_ns
      ~update_json:(human_message_update ~update_id:11 ())
      ()
  in
  Alcotest.(check bool) "next ok" true (outcome_is_human o4);
  Alcotest.(check (option int))
    "advanced to 11" (Some 11)
    (T.last_offset store ~bot_namespace:bot_ns)

let test_offset_survives_restart_snapshot () =
  let store = T.create_offset_store ~initial:[ (bot_ns, 100) ] () in
  let o =
    T.verify_and_derive_long_poll ~offset_store:store ~bot_namespace:bot_ns
      ~update_json:(human_message_update ~update_id:100 ())
      ()
  in
  Alcotest.(check bool) "replay after restart" true (outcome_is_stale o);
  let o2 =
    T.verify_and_derive_long_poll ~offset_store:store ~bot_namespace:bot_ns
      ~update_json:(human_message_update ~update_id:101 ())
      ()
  in
  Alcotest.(check bool) "next after restart" true (outcome_is_human o2);
  let snap = T.offset_store_to_list store in
  Alcotest.(check (list (pair string int))) "snapshot" [ (bot_ns, 101) ] snap

let test_bot_rejected_still_advances_offset () =
  let store = T.create_offset_store () in
  let o =
    T.verify_and_derive_long_poll ~offset_store:store ~bot_namespace:bot_ns
      ~update_json:(bot_message_update ~update_id:7 ())
      ()
  in
  Alcotest.(check bool) "bot rejected" true (outcome_is_bot o);
  Alcotest.(check (option int))
    "offset advanced past bot" (Some 7)
    (T.last_offset store ~bot_namespace:bot_ns)

let test_group_chat_context_not_identity () =
  let o_priv =
    T.verify_and_derive_long_poll ~bot_namespace:bot_ns
      ~update_json:(human_message_update ~chat_id:1 ~chat_type:"private" ())
      ()
  in
  let o_group =
    T.verify_and_derive_long_poll ~bot_namespace:bot_ns
      ~update_json:
        (human_message_update ~update_id:50 ~chat_id:999 ~chat_type:"group" ())
      ()
  in
  match (o_priv, o_group) with
  | T.Human h1, T.Human h2 -> (
      Alcotest.(check string)
        "same principal key"
        (T.human_identity_key h1.identity)
        (T.human_identity_key h2.identity);
      match h2.chat with
      | Some { kind = T.Group; chat_id } ->
          Alcotest.(check string) "group chat" "999" chat_id
      | _ -> Alcotest.fail "expected group chat")
  | _ -> Alcotest.fail "both human"

let test_webhook_secret_ok () =
  let outcome =
    T.verify_and_derive_webhook ~bot_namespace:bot_ns
      ~expected_secret_token:secret ~provided_secret_token:(Some secret)
      ~update_json:(human_message_update ()) ()
  in
  Alcotest.(check bool) "webhook human" true (outcome_is_human outcome)

let test_webhook_secret_mismatch () =
  let outcome =
    T.verify_and_derive_webhook ~bot_namespace:bot_ns
      ~expected_secret_token:secret ~provided_secret_token:(Some "wrong")
      ~update_json:(human_message_update ()) ()
  in
  Alcotest.(check bool) "invalid" true (outcome_is_invalid outcome);
  Alcotest.(check bool)
    "mentions secret" true
    (try
       let m = invalid_msg outcome in
       let _ = Str.search_forward (Str.regexp_string "secret") m 0 in
       true
     with Not_found -> false)

let test_webhook_secret_missing () =
  let outcome =
    T.verify_and_derive_webhook ~bot_namespace:bot_ns
      ~expected_secret_token:secret ~provided_secret_token:None
      ~update_json:(human_message_update ()) ()
  in
  Alcotest.(check bool) "invalid" true (outcome_is_invalid outcome)

let test_webhook_empty_expected_fail_closed () =
  Alcotest.(check bool)
    "empty expected" false
    (T.verify_webhook_secret_token ~expected:"" ~provided:(Some "x"));
  let outcome =
    T.verify_and_derive_webhook ~bot_namespace:bot_ns ~expected_secret_token:""
      ~provided_secret_token:(Some "") ~update_json:(human_message_update ()) ()
  in
  Alcotest.(check bool) "invalid" true (outcome_is_invalid outcome)

let test_bot_namespace_of_token () =
  Alcotest.(check (option string))
    "parse" (Some "123456789")
    (T.bot_namespace_of_token "123456789:AAHdqTcvCH1vGWJxfSeofSAs0K5PALDsaw");
  Alcotest.(check (option string))
    "bad" None
    (T.bot_namespace_of_token "not-a-token");
  Alcotest.(check (option string)) "empty" None (T.bot_namespace_of_token "")

let test_callback_query_from () =
  let update =
    `Assoc
      [
        ("update_id", `Int 70);
        ( "callback_query",
          `Assoc
            [
              ("id", `String "cq1");
              ("from", human_from ());
              ( "message",
                `Assoc
                  [
                    ("message_id", `Int 3);
                    ( "chat",
                      `Assoc [ ("id", `Int 222); ("type", `String "private") ]
                    );
                  ] );
              ("data", `String "btn");
            ] );
      ]
  in
  match
    T.verify_and_derive_long_poll ~bot_namespace:bot_ns ~update_json:update ()
  with
  | T.Human { identity; _ } ->
      Alcotest.(check string) "user" user_id identity.user_id
  | other ->
      Alcotest.failf "expected Human from callback_query, got %s"
        (match other with
        | T.Invalid e -> e
        | T.Bot_rejected e -> e
        | T.Stale_or_replay { message; _ } -> message
        | T.Human _ -> "human")

let test_token_secret_not_in_identity () =
  match T.bot_namespace_of_token "42:SUPERSECRET" with
  | None -> Alcotest.fail "expected bot id"
  | Some ns ->
      let identity = { T.bot_namespace = ns; user_id = "1" } in
      let key = T.human_identity_key identity in
      Alcotest.(check bool)
        "secret not in key" false
        (try
           let _ = Str.search_forward (Str.regexp_string "SUPERSECRET") key 0 in
           true
         with Not_found -> false);
      Alcotest.(check string) "namespace is bot id" "42" ns

let suite =
  [
    ("happy path long-poll human identity", `Quick, test_happy_path_long_poll);
    ("bot is_bot rejected", `Quick, test_bot_rejected);
    ("missing from.id fail closed", `Quick, test_missing_from_id);
    ("missing update_id fail closed", `Quick, test_missing_update_id);
    ("empty bot_namespace fail closed", `Quick, test_empty_bot_namespace);
    ("display name is not identity", `Quick, test_display_name_not_identity);
    ("cross-bot namespaces distinct", `Quick, test_cross_bot_namespaces_distinct);
    ("monotonic offset and replay", `Quick, test_monotonic_offset_and_replay);
    ( "offset snapshot survives restart",
      `Quick,
      test_offset_survives_restart_snapshot );
    ( "bot rejected advances offset",
      `Quick,
      test_bot_rejected_still_advances_offset );
    ( "group chat context not identity",
      `Quick,
      test_group_chat_context_not_identity );
    ("webhook secret_token ok", `Quick, test_webhook_secret_ok);
    ("webhook secret mismatch fail closed", `Quick, test_webhook_secret_mismatch);
    ("webhook secret missing fail closed", `Quick, test_webhook_secret_missing);
    ( "webhook empty expected fail closed",
      `Quick,
      test_webhook_empty_expected_fail_closed );
    ("bot_namespace_of_token", `Quick, test_bot_namespace_of_token);
    ("callback_query from.id", `Quick, test_callback_query_from);
    ( "token secret not in identity key",
      `Quick,
      test_token_secret_not_in_identity );
  ]
